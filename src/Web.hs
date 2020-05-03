module Web where

import Lucid
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Multipart
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import Control.Monad
import Data.Foldable
import SVG (generateSVGDeck')
import Data.String
import Codec.Archive.Zip
import Data.Time
import Control.Concurrent
import Control.Monad
import System.Directory


type PurgeAPI = Get '[HTML] (Html ())
    :<|> "form" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] (Html ())
    :<|> "decks" :> Capture "deckId" String :> Get '[OctetStream] LBS.ByteString

decksDirectory = "decks"
flipFile = "flip/flip.svg"

runWebApp :: IO ()
runWebApp = run 8081 $ serveWithContext (Proxy :: Proxy PurgeAPI) ((defaultMultipartOptions (Proxy :: Proxy Mem)) {generalOptions = setMaxRequestNumFiles 32 defaultParseRequestBodyOptions} :. EmptyContext) $ mainHTML
    :<|> generateDeck 
    :<|> deck
    where
        mainHTML :: MonadIO m => m (Html ())
        mainHTML = liftIO $ do
            deckDirectories <- listDirectory decksDirectory
            return $ html_ $ do
                title_ "Dobble Generator"
                body_ $ do
                    h1_ "Dobble Generator"
                    h2_ "New deck"
                    form_ [action_ "form", method_ "post", enctype_ "multipart/form-data" ] $ do
                        span_ "Pick 31 symbols svg files "
                        input_ [type_ "file", id_ "symbolSvgFiles", name_ "symbolSvgFiles", multiple_ "true", accept_ ".svg"]
                        span_ " then a name of the deck "
                        input_ [type_ "input", id_ "deckName", name_ "deckName"]
                        span_ " and press "
                        input_ [type_ "submit", value_ "Create Deck!"]
                    h2_ "Generated decks"
                    p_ $ a_ [href_ "/"] "refresh"
                    forM_ deckDirectories $ \deckDirectory -> p_ $ a_ [href_ ("/decks/" <> fromString deckDirectory)] (fromString deckDirectory)

        deck :: MonadIO m => String -> m LBS.ByteString
        deck deckId = liftIO $ LBS.readFile (decksDirectory <> "/" <> deckId)

        generateDeck :: MonadIO m => MultipartData Mem -> m (Html ())
        generateDeck multipartData = liftIO $ do
            time <- getCurrentTime 
            let directoryName = decksDirectory <> "/" <> show time
            forkIO $ do
                let symbolSvgBSs = LBS.toStrict . fdPayload <$> filter ((== "symbolSvgFiles") . fdInputName) (files multipartData)
                flipSideSvgBS <- BS.readFile "flip/flip.svg"
                pages <- generateSVGDeck' symbolSvgBSs flipSideSvgBS
                LBS.writeFile (directoryName <> ".zip") $ fromArchive $ foldr (\(i, page) a -> addEntryToArchive (toEntry (show i <> ".png") 0 (LBS.fromStrict page)) a) emptyArchive (zip [1..] pages)
            return $ html_ $ do
                title_ "Dobble Generator"
                body_ $ do
                    h1_ "Deck is being generated..."
                    p_ (fromString directoryName)
                    a_ [href_ "/"] "return to the main page"

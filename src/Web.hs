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
import Data.Foldable
import SVG (generateSVGDeck')
import Data.String
import Codec.Archive.Zip
import Data.Time
import Control.Concurrent
import Control.Monad
import System.Directory
import Data.Text (unpack)
import Data.List
import Data.Ord
import System.Random
import Data.UUID (UUID)


type PurgeAPI = Get '[HTML] (Html ())
    :<|> "form" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] (Html ())
    :<|> "decks" :> Capture "deckId" String :> Get '[OctetStream] LBS.ByteString


runWebApp :: IO ()
runWebApp = run 8081 $ serveWithContext (Proxy :: Proxy PurgeAPI) ((defaultMultipartOptions (Proxy :: Proxy Mem)) {generalOptions = setMaxRequestNumFiles 32 defaultParseRequestBodyOptions} :. EmptyContext) $ mainHTML
    :<|> generateDeck 
    :<|> deck
    where
        decksDirectory :: FilePath
        decksDirectory = "decks"

        flipFile :: FilePath
        flipFile = "flip/flip.svg"

        mainHTML :: MonadIO m => m (Html ())
        mainHTML = liftIO $ do
            deckFiles <- listDirectory decksDirectory
            modTimes <- traverse getModificationTime $ ((decksDirectory <> "/") <>) <$> deckFiles
            timeZone <- getCurrentTimeZone
            let deckFileEntries = sortOn (Down . snd) (zip deckFiles modTimes)
            return $ html_ $ do
                title_ "Dobble Generator"
                body_ $ do
                    h1_ "Dobble Generator"
                    h2_ "New deck"
                    form_ [action_ "form", method_ "post", enctype_ "multipart/form-data" ] $ do
                        span_ "Pick 31 symbols as svg files "
                        input_ [type_ "file", id_ "symbolSvgFiles", name_ "symbolSvgFiles", multiple_ "true", accept_ ".svg"]
                        span_ " and a name for the deck "
                        input_ [type_ "input", id_ "deckName", name_ "deckName"]
                        input_ [type_ "submit", value_ "Generate"]
                    h2_ "Generated decks"
                    p_ $ a_ [href_ "/"] "refresh list"
                    forM_ deckFileEntries $ \(deckFile, utcModTime) -> p_ $ do
                        a_ [href_ ("/decks/" <> fromString deckFile)] (fromString (drop 37 deckFile))
                        span_ (fromString (" generated on " <> show (utcToLocalTime timeZone utcModTime)))

        deck :: MonadIO m => String -> m LBS.ByteString
        deck deckId = liftIO $ LBS.readFile (decksDirectory <> "/" <> deckId)

        generateDeck :: MonadIO m => MultipartData Mem -> m (Html ())
        generateDeck multipartData = liftIO $ do
            let (Just deckName) = unpack . iValue <$> find ((== "deckName") . iName) (inputs multipartData)
            forkIO $ do
                (uuid :: UUID) <- randomIO 
                let symbolSvgBSs = LBS.toStrict . fdPayload <$> filter ((== "symbolSvgFiles") . fdInputName) (files multipartData)
                flipSideSvgBS <- BS.readFile "flip/flip.svg"
                pages <- generateSVGDeck' symbolSvgBSs flipSideSvgBS
                let deckFile = decksDirectory <> "/" <> show uuid <> "_" <> deckName <> ".zip"
                LBS.writeFile deckFile $ fromArchive $ foldr (\(i, page) a -> addEntryToArchive (toEntry (show i <> ".png") 0 (LBS.fromStrict page)) a) emptyArchive (zip [1..] pages)
            return $ html_ $ do
                title_ "Dobble Generator"
                body_ $ do
                    h1_ $ "Generating deck"
                    p_ $ fromString $ "Deck '" <> deckName <> "' is being generated."
                    a_ [href_ "/"] "return to the main page"

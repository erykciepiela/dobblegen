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


type PurgeAPI = Get '[HTML] (Html ())
    :<|> "form" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Int

runWebApp :: IO ()
runWebApp = run 8081 $ serveWithContext (Proxy :: Proxy PurgeAPI) ((defaultMultipartOptions (Proxy :: Proxy Mem)) {generalOptions = setMaxRequestNumFiles 32 defaultParseRequestBodyOptions} :. EmptyContext) $ return mainHTML
    :<|> generateDeck
    where
        mainHTML :: Html ()
        mainHTML = html_ $ do
            title_ "Dobble Generator"
            body_ $ do
                h1_ "Dobble Generator"
                form_ [action_ "form", method_ "post", enctype_ "multipart/form-data" ] $ do
                    p_ "Pick 31 symbols svg files..."
                    input_ [type_ "file", id_ "symbolSvgFiles", name_ "symbolSvgFiles", multiple_ "true", accept_ ".svg"]
                    p_ "then logo svg file for the flip side of the cards..."
                    input_ [type_ "file", id_ "flipSideSvgFiles", name_ "flipSideSvgFile", multiple_ "false", accept_ ".svg"]
                    p_ "and press the button..."
                    input_ [type_ "submit", value_ "Create Deck!"]

        generateDeck :: MonadIO m => MultipartData Mem -> m Int
        generateDeck multipartData = liftIO $ do
            let symbolSvgBSs = LBS.toStrict . fdPayload <$> filter ((== "symbolSvgFiles") . fdInputName) (files multipartData)
            let (Just flipSideSvgBS) = LBS.toStrict . fdPayload <$> find ((== "flipSideSvgFile") . fdInputName) (files multipartData)
            pages <- generateSVGDeck' symbolSvgBSs flipSideSvgBS
            forM_ pages $ \page ->
                print $ BS.length page 
            return $ sum $ BS.length <$> pages

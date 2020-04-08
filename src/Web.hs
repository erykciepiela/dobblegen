module Web where

import Lucid
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Multipart
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
import Control.Monad

type PurgeAPI = Get '[HTML] (Html ())
    :<|> "form" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

webapp :: IO ()
webapp = run 8081 $ serveWithContext (Proxy :: Proxy PurgeAPI) ((defaultMultipartOptions (Proxy :: Proxy Mem)) {generalOptions = setMaxRequestNumFiles 31 defaultParseRequestBodyOptions} :. EmptyContext) $ return mainHTML
    :<|> createDeck
    where
        mainHTML :: Html ()
        mainHTML = html_ $ do
            title_ "Dobble Generator"
            body_ $ do
                h1_ "Dobble Generator"
                form_ [action_ "form", method_ "post", enctype_ "multipart/form-data" ] $ do
                    p_ "Select 31 symbols svg files..."
                    input_ [type_ "file", id_ "symbolSvgFiles", name_ "symbolSvgFiles", multiple_ "true", accept_ ".svg"]
                    p_ "And press the button..."
                    input_ [type_ "submit", value_ "Create Deck!"]

        createDeck :: MonadIO m => MultipartData Mem -> m Integer
        createDeck multipartData = liftIO $ do
            putStrLn "Inputs:"
            forM_ (inputs multipartData) $ \input ->
                putStrLn $ "  " ++ show (iName input)
                    ++ " -> " ++ show (iValue input)

            forM_ (files multipartData) $ \file -> do
                let content = fdPayload file
                putStrLn $ "Content of " ++ show (fdFileName file)
                LBS.putStr content
            return 0

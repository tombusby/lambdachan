{-# LANGUAGE OverloadedStrings #-}
import Servant
import Servant.Server.StaticFiles
import Network.Wai.Handler.Warp (run)
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types (StaticSettings(..), unsafeToPiece)

type API = Raw

server :: Server API
server = serveDirectoryWith settings
  where
    settings = (defaultWebAppSettings "frontend/") {
        ssIndices = [unsafeToPiece "index.html"]
      }

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  let portNum = 8000
  putStrLn $ "Server running on " ++ show portNum
  run portNum app

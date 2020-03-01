import Servant
import Network.Wai.Handler.Warp (run)

type API = Raw

server :: Server API
server = serveDirectoryWebApp "frontend/"

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
    let portNum = 8000
    run portNum app

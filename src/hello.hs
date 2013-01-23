import Network.CGI
import Text.XHtml

page :: Html
page = body << h1 << "Hello World!"

cgiMain :: CGI CGIResult
cgiMain = output $ renderHtml page

main :: IO ()
main = runCGI $ handleErrors cgiMain

import Network.CGI

cgiMain :: CGI CGIResult
cgiMain = redirect "http://www.regexplanet.com/advanced/haskell/index.html"

main :: IO ()
main = runCGI (handleErrors cgiMain)

{-# LANGUAGE DeriveGeneric
    , PackageImports
    , CPP #-}

import Network.CGI
import GHC.Generics
import Data.Aeson
import Data.List (intercalate)
import System.Info
import Data.Version
import "regex-compat-tdfa" Text.Regex
import Data.ByteString.Lazy (append)
import Data.ByteString.Lazy.Char8 (pack)
data RegexStatus =
    Status { success :: !Bool
           , version :: !String
           } deriving (Show,Generic)
instance ToJSON RegexStatus

versionString :: String
versionString = intercalate " "
                    [compilerName,showVersion compilerVersion,"regex-compat-tdfa",VERSION_regex_compat_tdfa]

cgiMain :: CGI CGIResult
cgiMain = do
    cal <- getInputFPS "callback"
    let responseStatus = encode $ Status True versionString
        encodedVersion = case cal of
           Nothing -> responseStatus
           Just callback -> callback `append`  (pack "(") `append` responseStatus `append` (pack ");")
    outputFPS encodedVersion

main :: IO ()
main = runCGI $ handleErrors cgiMain


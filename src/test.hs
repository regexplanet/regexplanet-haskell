{-# LANGUAGE DeriveGeneric
 ,PackageImports #-}
import Network.CGI
import Data.Aeson
import "regex-compat-tdfa" Text.Regex --PackageImport needed if other backends installed
import Text.Regex.Base (defaultCompOpt,defaultExecOpt,makeRegexOpts,AllTextSubmatches(..),MatchLength,MatchOffset,RegexContext(..))
import Text.Regex.TDFA.Common (CompOption(..)) --,ExecOption(..))
import Text.XHtml
import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.List (zip4,mapAccumL,intersperse,intercalate,dropWhileEnd)
import Control.Exception
import qualified Data.ByteString.Lazy as BL

data PlanetRegexOutput =
    RegexOutput { success :: !Bool
                , html    :: !String
                }
    | RegexError { success :: !Bool
                 , message :: !String
                 }
        deriving (Show,Generic)
instance ToJSON PlanetRegexOutput

totalProcess :: Regex -> String -> [String] -> [(String,(String,String,String,[String]),String,[(String,(MatchOffset,MatchLength))])]
totalProcess reg rep inps = zip4 inps processMatch processSub processGroups
    where processMatch= map (fromMaybe ("","","",[]))
                    $ map (matchRegexAll reg) inps
          processSub = map (\x -> subRegex reg x rep) inps
          processGroups = map (matchGroups reg) inps
matchGroups :: Regex -> String -> [(String,(MatchOffset,MatchLength))]
matchGroups a b = getAllTextSubmatches $ match a b
styleAttr,classAttr :: HtmlAttr
classAttr = strAttr "class" "table table-bordered table-striped bordered-table zebra-striped"
styleAttr = strAttr "style" "width:auto;"

testTable :: String -> [String] -> String -> [String] -> Html
testTable reg opt rep inp = simpleTable [classAttr,styleAttr] []
    [ [toHtml "Regular Expression", thecode $ toHtml reg]
    , [toHtml "Replacement"       , thecode $ toHtml rep]
    , [toHtml "Options"           , thecode $ toHtml $ intercalate " " opt]
    , [toHtml "# of inputs"       , thecode $ toHtml $ show $ length inp]
    ]

resultTable :: Regex -> String -> [String] -> Html
resultTable reg rep inp = simpleTable [classAttr,styleAttr] []
    $ map (bold . toHtml) ["Test","Input","BeforeMatch","Match","AfterMatch","Submatches","Substituted","n [start,end] group"] : rest
      where
        rowToHtml = (\i (thisinp,(before,matched,after,submatches),subst,groups) ->
                    (i+1,
                        [ thecode . toHtml $ show i
                        , thecode . toHtml $ thisinp
                        , thecode . toHtml $ before
                        , thecode . toHtml $ matched
                        , thecode . toHtml $ after
                        , thecode . toHtml $ intercalate " " submatches
                        , thecode . toHtml $ subst
                        , showGs groups]))
                            where showG i' (m,(mi,ml)) = (i'+1,(concat [show i',": [",show mi,",",show $ mi+ml,"] "]) +++ thecode (toHtml m))
                                  showGs g = concatHtml $ intersperse br $ snd $ mapAccumL showG (0::Int) g
        (_,rest) = mapAccumL rowToHtml (1::Int) $ totalProcess reg rep inp


outputHtml :: String -> [String] -> String -> [String] -> Maybe String -> String
outputHtml reg opt rep inp cal =
    let regex = makeMyRegex reg opt
        inputs = dropWhileEnd null inp
        tests = testTable reg opt rep inputs
        results = resultTable regex rep inputs
        out = renderHtmlFragment $ concatHtml [tests,results]
    in case cal of
           Nothing -> out
           Just callback -> callback ++ "(" ++ out ++ ");"

makeMyRegex :: String -> [String] -> Regex
makeMyRegex reg opts =
    let compOpts = defaultCompOpt
                     {caseSensitive = not $ elem "ignorecase" opts
                     ,multiline     = elem "multiline" opts
                     ,rightAssoc    = elem "rightAssoc" opts
                     ,newSyntax     = elem "newSyntax" opts
                     ,lastStarGreedy= elem "lastStarGreedy" opts
                     }
        execOpts = defaultExecOpt
    in makeRegexOpts compOpts execOpts reg

cgiMain :: CGI CGIResult
cgiMain = do
    reg <- getInput "regex" >>= return . fromMaybe ""
    opt <- getMultiInput "option"
    rep <- getInput "replacement" >>= return . fromMaybe ""
    inp <- getMultiInput "input"
    cal <- getInput "callback"

    -- Force the encoding to reveal errors. (CGI has odd error recovery)
    let result = RegexOutput True $ outputHtml reg opt rep inp cal
    BL.length (encode result) `seq` return ()

    standardHeaders
    outputFPS $ encode result

handleError :: SomeException -> IO ()
handleError e = runCGI $ do
    standardHeaders
    outputFPS . encode $ RegexError False $ show e

standardHeaders :: CGI ()
standardHeaders = do
    setHeader "Content-type" "text/plain;charset=utf-8"
    setHeader "Access-Control-Allow-Origin" "*"
    setHeader "Access-Control-Allow-Methods" "POST, GET"
    setHeader "Access-Control-Max-Age" $ show $ (60*60*24*7::Int)

main :: IO ()
main = catch (runCGI cgiMain) handleError


module Main where

import Control.Monad.IO.Class (liftIO)
import Web.Scotty hiding (request)
import Web.Scotty.Fay
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import Network.Wai.Test
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit.Base as H
import System.Directory

main :: IO ()
main = do
    dir <- getCurrentDirectory
    putStrLn dir
    tests >>= defaultMain

tests :: IO [Test]
tests = sequence
    [ waiTest "compiles Fay" test_compilesFay
    , waiTest "serveFay captures everything under base" test_capturesEverything
    , waiTest "imports" test_imports
    , waiTest "directory traversal" test_directoryTraversal
    ]

waiTest :: String -> Session () -> IO Test
waiTest name session = do
    app' <- scottyApp app
    return $ testCase name $ runSession session app'

app :: ScottyM ()
app = do
    serveFay (under "/fay" . from "test")

    get "/" $ do
        text "this is the root"

    get "/fay/shouldnt-get-here" $ do
        text "it shouldn't get here"

-- This is handy to have when debugging interactively.
runScottyApp :: IO ()
runScottyApp = scotty 3000 app

assertBool :: String -> Bool -> Session ()
assertBool str p = liftIO $ H.assertBool str p

assertNotStatus :: Int -> SResponse -> Session ()
assertNotStatus i SResponse{simpleStatus = s} = assertBool (concat
    [ "Expected a status other than "
    , show i
    ]) $ i /= sc
    where
        sc = HTTP.statusCode s

assertJavaScriptRenderedOk :: SResponse -> Session ()
assertJavaScriptRenderedOk response = do
    assertStatus 200 response
    assertHeader "Content-Type" "text/javascript" response

test_compilesFay :: Session ()
test_compilesFay = do
    let req = setPath defaultRequest "/fay/test/Fact.hs"
    resp <- request req

    assertJavaScriptRenderedOk resp

test_capturesEverything :: Session ()
test_capturesEverything = do
    let req = setPath defaultRequest "/fay/shouldnt-get-here"
    resp <- request req

    assertNotStatus 200 resp

test_imports :: Session ()
test_imports = do
    let req = setPath defaultRequest "/fay/test/Fib.hs"
    resp <- request req

    assertJavaScriptRenderedOk resp

test_directoryTraversal :: Session ()
test_directoryTraversal = do
    let req = setPath defaultRequest "/fay/test/Fib/../Fib.hs"
    resp <- request req

    assertNotStatus 200 resp

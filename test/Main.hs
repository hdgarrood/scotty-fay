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
    ]

waiTest :: String -> Session () -> IO Test
waiTest name session = do
    app' <- scottyApp app
    return $ testCase name $ runSession session app'

app :: ScottyM ()
app = do
    serveFay "/fay"

    get "/" $ do
        text "this is the root"

    get "/fay/shouldnt-get-here" $ do
        text "it shouldn't get here"

assertBool :: String -> Bool -> Session ()
assertBool str p = liftIO $ H.assertBool str p

assertNotStatus :: Int -> SResponse -> Session ()
assertNotStatus i SResponse{simpleStatus = s} = assertBool (concat
    [ "Expected a status other than "
    , show i
    ]) $ i /= sc
    where
        sc = HTTP.statusCode s

test_compilesFay :: Session ()
test_compilesFay = do
    let req = setPath defaultRequest "/fay/test/Fib.hs"
    resp <- request req

    assertStatus 200 resp
    assertHeader "Content-Type" "text/javascript" resp

test_capturesEverything :: Session ()
test_capturesEverything = do
    let req = setPath defaultRequest "/fay/shouldnt-get-here"
    resp <- request req

    assertNotStatus 200 resp

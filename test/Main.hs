module Main where

import Data.List
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

import Web.Scotty.Fay.Internal

main :: IO ()
main = do
    dir <- getCurrentDirectory
    putStrLn dir
    tests >>= defaultMain

tests :: IO [Test]
tests = sequence
    [ waiTest "compiles Fay" test_compilesFay
    , waiTest "can use fay packages" test_usingPackages
    , waiTest "serveFay captures everything under base" test_capturesEverything
    , waiTest "imports" test_imports
    , waiTest "directory traversal" test_directoryTraversal
    , return $ testGroup "configuration" $
        [ testCase "configuring include dirs" test_configuringIncludeDirs
        , testCase "configuring base path" test_configuringBasePath
        ]
    ]

waiTest :: String -> Session () -> IO Test
waiTest name session = do
    app' <- scottyApp app
    return $ testCase name $ runSession session app'

app :: ScottyM ()
app = do
    serveFay $
        ( under "/fay"
        . fromDirs ["test/fay-resources1", "test/fay-resources2"]
        . withPackages ["fay-text", "fay-jquery"]
        )

    get "/" $ do
        text "this is the root"

    get "/fay/shouldnt-get-here" $ do
        text "it shouldn't get here"

-- This is handy to have when debugging interactively.
runScottyApp :: IO ()
runScottyApp = scotty 3001 app

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
    let req = setPath defaultRequest "/fay/Fact.hs"
    resp <- request req

    assertJavaScriptRenderedOk resp

test_capturesEverything :: Session ()
test_capturesEverything = do
    let req = setPath defaultRequest "/fay/shouldnt-get-here"
    resp <- request req

    assertNotStatus 200 resp

test_imports :: Session ()
test_imports = do
    let req = setPath defaultRequest "/fay/Fib.hs"
    resp <- request req

    assertJavaScriptRenderedOk resp

test_directoryTraversal :: Session ()
test_directoryTraversal = do
    let req = setPath defaultRequest "/fay/Fib/../Fib.hs"
    resp <- request req

    assertNotStatus 200 resp

test_multipleIncludeDirs :: Session ()
test_multipleIncludeDirs = do
    let req = setPath defaultRequest "/fay/HelloWorld.hs"
    resp <- request req

    assertJavaScriptRenderedOk resp

test_fayUnderDirectories :: Session ()
test_fayUnderDirectories = do
    let req = setPath defaultRequest "/fay/under-a-dir/HelloWorldAgain.hs"
    resp <- request req

    assertJavaScriptRenderedOk resp

assertEq :: (Eq a, Show a) => a -> a -> H.Assertion
assertEq = H.assertEqual ""

assertSameElems :: (Ord a, Eq a, Show a) => [a] -> [a] -> H.Assertion
assertSameElems xs ys = assertEq (sort xs) (sort ys)

test_configuringIncludeDirs :: H.Assertion
test_configuringIncludeDirs =
    assertSameElems ["src", "src2", "src3"] $
        (configIncludeDirs . buildConfig $
            (under "/js" . from "src" . fromDirs ["src3", "src2"]))

test_configuringBasePath :: H.Assertion
test_configuringBasePath =
    assertEq "/js" $
        (configBasePath . buildConfig $ (under "/js" . from "src"))

test_getNonExistent :: H.Assertion
test_getNonExistent = do
    results <- getNonExistent ["non-existent2", "non-existent1", "src"]
    assertSameElems ["non-existent1", "non-existent2"] results

test_usingPackages :: Session ()
test_usingPackages = do
    let req = setPath defaultRequest "/fay/TestJQuery.hs"
    resp <- request req

    assertJavaScriptRenderedOk resp

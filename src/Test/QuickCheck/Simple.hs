-- |
-- Module      : Test.QuickCheck.Simple
-- Copyright   : 2015-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions of test properties and default-mains
-- using QuickCheck library.
module Test.QuickCheck.Simple
       ( Property (..)
       , boolTest', boolTest
       , eqTest', eqTest
       , qcTest
       , Test, TestError (..)
       , runTest
       , defaultMain', defaultMain, verboseMain,
       ) where

import Control.Applicative ((<$>))
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Test.QuickCheck
  (Testable, Result (..), quickCheckResult, label)
import qualified Test.QuickCheck as QC


-- | Property type. 'Bool' or 'Testable' of QuickCheck.
data Property
  = Bool (Maybe String) Bool
  | QuickCheck QC.Property

-- | Property with label string
type Test = (String, Property)

-- | Test error result.
data TestError
  = BFalse (Maybe String)
  | QCError Result
  deriving Show

mkBoolTest :: String -> Maybe String -> Bool -> Test
mkBoolTest n m = ((,) n) . Bool m

-- | 'Bool' specialized property with message for False case
boolTest' :: String
          -> String
          -> Bool
          -> Test
boolTest' n m = mkBoolTest n (Just m)

-- | 'Bool' specialized property
boolTest :: String
         -> Bool
         -> Test
boolTest n = mkBoolTest n Nothing

-- | 'Eq' specialized property with explicit passing
eqTest' :: (a -> a -> Bool) -> (a -> String) -> String -> a -> a -> Test
eqTest' eq show' n x y = boolTest' n msg $ x `eq` y where
  msg = unlines [show' x, "** NOT EQUALS **", show' y]

-- | 'Eq' specialized property
eqTest :: (Eq a, Show a) => String -> a -> a -> Test
eqTest = eqTest' (==) show

-- | QuickCheck 'Testable' property
qcTest :: Testable prop
       => String
       -> prop
       -> Test
qcTest n = ((,) n) . QuickCheck . label n

putErrorLn :: String -> IO ()
putErrorLn = putStrLn . ("*** " <>)

runBool :: String -> Maybe String -> Bool -> IO (Maybe TestError)
runBool n m = d  where
  d True  =  do
    putStrLn $ "+++ OK, success (" <> n <> ")"
    return   Nothing
  d False =  do
    putErrorLn $ "Failed! (" <> n <> ")"
    return . Just $ BFalse m

runQcProp :: String -> QC.Property -> IO (Maybe TestError)
runQcProp n p = err =<< quickCheckResult p  where
  err (Success {})  =
    return   Nothing
  err x             =  do
    putErrorLn $ "  label: " <> n
    return . Just $ QCError x

runProp :: String -> Property -> IO (Maybe TestError)
runProp n = d  where
  d (Bool m b)       =  runBool n m b
  d (QuickCheck p)   =  runQcProp n p

-- | Run a single test suite.
runTest :: Test
        -> IO (Maybe TestError)
runTest = uncurry runProp

runPropL :: String -> Property -> IO (Maybe (String, TestError))
runPropL n p = do
  me <- runProp n p
  return $ fmap ((,) n) me

showTestError :: TestError -> String
showTestError = d  where
  d (BFalse m)   =  fromMaybe "" m
  d (QCError r)  =  show r

-- | Default main to run test suites.
defaultMain' :: Bool -> [Test] -> IO ()
defaultMain' verbose xs = do
  es <- catMaybes <$> mapM (uncurry runPropL) xs
  let rlines m r = (m <> ":") : [ "  " <> x | x <- lines $ showTestError r ]
  when verbose $ mapM_ (\(m, r) -> mapM_ putStrLn $ rlines m r) es
  unless (null es) $ fail "Some failures are found."

-- | Not verbose version of 'defaultMain''.
defaultMain :: [Test] -> IO ()
defaultMain = defaultMain' False

-- | Verbose verison of defaultMain
verboseMain :: [Test] -> IO ()
verboseMain = defaultMain' True

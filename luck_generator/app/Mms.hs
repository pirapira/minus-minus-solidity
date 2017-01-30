{-# LANGUAGE TemplateHaskell, RecordWildCards, DeriveDataTypeable #-}
import Control.Monad
import Control.Applicative
import Control.Arrow hiding ((<+>))

import System.IO
import System.Directory
import System.Process
import Control.Concurrent
import Control.Exception
import System.Exit
import System.IO
import System.IO.Error

import System.Environment
import System.Random
import System.Console.CmdArgs
import System.Exit

import Luck.Template
import Test.QuickCheck

import Data.Maybe
import Data.List

import System.Directory
import System.Process

import Data.Data

data ContractElement =
  VariableDeclaration Int
  deriving (Data, Show)

stringGen :: Gen (Maybe [ContractElement])
stringGen = $(mkGenQ "minus-minus-solidity.luck") defFlags{_maxUnroll=2} TProxy1

main :: IO ()
main = do
  (mts : _ ) <- sample' stringGen
  case mts of
    Just c -> putStrLn $ show c
    Nothing -> error "Unsuccesful generation"

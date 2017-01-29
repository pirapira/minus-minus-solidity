-- copied from https://github.com/QuickChick/Luck/blob/master/luck/examples-template/C.hs
-- MIT License
-- 
-- Copyright (c) 2016 QuickChick
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
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
import System.Posix.Signals
import System.Process.Internals

import System.Environment
import System.Random
import System.Console.CmdArgs
import System.Exit

import Luck.Template
import Test.QuickCheck

import Data.Data
import Data.Maybe
import Data.List

import System.Directory
import System.Process

import Data.Data

import Text.PrettyPrint (Doc, (<+>), (<>), ($$))
import qualified Text.PrettyPrint as PP

class PP a where 
    pp :: a -> Doc

instance PP Int where
    pp = PP.int

data ContractElement =
  VariableDeclaration Int
  deriving (Data, Show)

data Contract = Contract Int [ContractElement] deriving (Data, Show)

instance PP ContractElement where
  pp (VariableDeclaration i) =PP.text "uint" <+>  PP.text "v" <> pp i <> PP.text ";"

instance PP a => PP [a] where
  pp lst = PP.vcat (map pp lst)

instance PP Contract where
    pp (Contract i elements) =
      PP.text "contract C" <> pp i <+> PP.text " {" <+> pp elements <+> PP.text "}"

stringGen :: Gen (Maybe Contract)
stringGen = $(mkGenQ "minus-minus-solidity.luck") defFlags{_maxUnroll=2} TProxy1

-- dump :: Fun -> IO ()
-- dump (Fun _ (t:ts)) = do
--  let tDoc = pp t
--      tsDoc = PP.vcat $ PP.text "#include <stdio.h>"
--                      : (PP.text "void loop() { while (1) { printf(\"1\"); } }")
--                      : (PP.text "void empty() { }")
--                      : map (\(i,t) ->
--                                 PP.vcat [ PP.text "void a" <> PP.int i <> PP.text "(int var0, int var1, int var2) {"
--                                         , PP.nest 2 $ pp t
--                                         , PP.text "}" ]
--                            ) (reverse $ zip [1..] $ ts)
--  putStrLn (PP.render tDoc)

main :: IO ()
main = do
  (mts : _ ) <- sample' stringGen
  case mts of
    Just c -> putStrLn $ PP.render $ pp c
    Nothing -> error "Unsuccesful generation"

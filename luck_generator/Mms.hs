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

data Exp = Var Int
         | Int Int
         | Add Exp Exp
         | Eq Exp Exp
           deriving (Show, Data)

data Stmt = Declare Int          Stmt
          | Asgn Int Exp         Stmt 
          | If Exp Stmt Stmt     Stmt
          | For Int Int Int Stmt Stmt
          | PrintVar Int         Stmt
          | FunCall Int [Exp]    Stmt 
          | Empty
            deriving (Show, Data)

class PP a where 
    pp :: a -> Doc

instance PP Int where
    pp = PP.int 

instance PP Exp where
    pp (Var x) = PP.text $ "var" ++ show x
    pp (Int n) = pp n
    pp (Add e1 e2) = PP.parens $ pp e1 <+> PP.char '+'  <+> pp e2
    pp (Eq e1 e2)  = PP.parens $ pp e1 <+> PP.text "==" <+> pp e2

ppForVar :: Int -> Doc 
ppForVar i = PP.char 'i' <> PP.int i

instance PP Stmt where
    pp (Declare x s) = PP.text "int" <+> pp (Var x) <+> PP.char ';' $$ pp s
    pp (Asgn x e s)  = pp (Var x) <+> PP.char '=' <+> pp e <+> PP.char ';' $$ pp s
    pp (If e s1 s2 s') = PP.text "if" <+> PP.parens (pp e) <+> PP.char '{' 
                                      $$ PP.nest 2 (pp s1)
                                      $$ PP.char '}'
                                      $$ PP.text "else {" 
                                      $$ PP.nest 2 (pp s2)
                                      $$ PP.char '}'
                                      $$ pp s'
    pp (PrintVar n s') = PP.text "printf(\"%d\\n\", " <+> pp (Var n) <+> PP.text ");" $$ pp s'
    pp (FunCall (-2) [] s') = PP.text "empty();" $$ pp s'
    pp (FunCall (-1) [] s') = PP.text "loop();" $$ pp s'
    pp (FunCall fid es s') = 
        PP.char 'a' <> PP.int fid <> PP.char '(' 
              <> PP.hcat (intersperse (PP.char ',') (map pp es))
              <> PP.text ");" $$ pp s'
    pp Empty = PP.empty
    pp (For i low high sfor s') = 
        PP.text "for (int" <+> ppForVar i <+> PP.char '=' <+> PP.int low  <> PP.char ';' 
                           <+> ppForVar i <+> PP.char '<' <+> PP.int high <> PP.char ';'
                           <+> ppForVar i <> PP.text "++) {" 
          $$ PP.nest 2 (pp sfor) 
          $$ PP.text "}"
          $$ pp s'
--    pp x = error $ show x

data Fun = Fun [Stmt] deriving (Data, Show)

stmtGen :: Gen (Maybe Fun)
stmtGen = $(mkGenQ "minus-minus-solidity.luck") defFlags{_maxUnroll=2} TProxy1

dump :: Fun -> IO ()
dump (Fun (t:ts)) = do 
  let indices = map fst $ zip [0..] ts
  let tDoc = PP.vcat [ PP.text "void a0(int var0, int var1, int var2) {"
                     , PP.nest 2 $ pp t 
                     , PP.text "}" ]
      tsDoc = PP.vcat $ PP.text "#include <stdio.h>" 
                      : (PP.text "void loop() { while (1) { printf(\"1\"); } }")
                      : (PP.text "void empty() { }")
                      : map (\(i,t) -> 
                                 PP.vcat [ PP.text "void a" <> PP.int i <> PP.text "(int var0, int var1, int var2) {"
                                         , PP.nest 2 $ pp t 
                                         , PP.text "}" ]
                            ) (reverse $ zip [1..] $ ts) 
--  let calls = map (\(i,_) -> PP.text "a" <> PP.int i <+> PP.text "();") (zip [0..] ts)
  let doc = PP.render $ PP.vcat ( PP.text "#include <stdio.h>" 
                                : (map (\(i,_) -> PP.text "extern void a" 
                                                      <> PP.int i <> PP.text "(int x, int y, int z);"
                                               ) (zip [1..] ts)
                                  )
                                ++ [ PP.text "extern void loop(); "
                                   , PP.text "extern void empty(); " ]
                                ++ [ tDoc 
                                   , PP.text "int main() {"
                                   , PP.text "  int undef;"
                                   , PP.text "  a0(undef, 0,1);"
                                   , PP.text "}" ])
  putStrLn (PP.render tsDoc)

main :: IO ()
main = do
  (mts : _ ) <- sample' stmtGen
  case mts of
    Just ts -> do
      dump ts
    Nothing -> error "Unsuccesful generation"

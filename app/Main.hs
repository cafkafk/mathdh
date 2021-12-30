-- Mathdh - MDH - Math Drills with Haskell
-- Copyright (C) 2021  Christina (cafkafk) - CESC

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.




-- THE REWRITE IN HASKELL
-- THE TASK THAT WAS LAZILY EVALUATED

-- WHATS NEXT:
-- TODO accumulate score
-- TODO automatic difficulty based on historic score
-- TODO performance stored for each combination, used
--      for frequency of appearance (hard questions more, easy less)

-- Usage: command n a b + - * /
-- Where n is amount of questions per subgame.
-- a and b are the range [a,b] where integers are taken from.
-- The tail is any operations you want to do, remember you can do "*" if your
--     shell does globs.
-- e.g.: arith 10 0 10 "*" + -
--       Runs 10x2 games (ordered and random) for "*", +, -,
--       with integers from [0,10]

module Main where

import Lib

import Control.Monad (forM_) -- I am so sorry
import Control.Exception

import Data.Maybe
import Data.List

import System.IO
import System.Random
import System.Environment
import System.Process

import Text.Read
import Text.Printf

-- Do IO n times
play n f = forM_ [1..n] $ \_ -> do
  f

-- Table generator
-- Generates kayleigh table inputs as 2-tuples forall a,b in [a,b]
table a b = [(x,y) | x <- [a..b], y <- [a..b]]

-- TODO This could well be a record.
-- Performs operation from string of operator
sapp :: [Char] -> Int -> Int -> Int
sapp "+" a b = a + b 
sapp "-" a b = a - b 
sapp "*" a b = a * b

-- Ensure number is entered
forceInput = do
  input <- getLine
  if not $ isNothing(readMaybe input :: Maybe Int) 
  then return input
  else forceInput

-- Runs the input
game op a b = do
  putStrLn $ "What is " ++ show a ++ op ++ show b ++ "?"
  input <- forceInput 
  let guess = (read input :: Int)
  let answer = sapp op a b
  system "clear"
  putStrLn $ if (answer == guess) then "Correct" else "Wrong, it was " ++ show answer

-- Deligate numbers to ops and runs game
drill n a b op = do
  g <- getStdGen
  let rTable = [(x,y) |
                x <- [a..b],
                y <- (take n (randomRs (a, b) g :: [Int]))]
  sequence $ map (\(x,y) -> game op x y) (table a b)
  sequence $ map (\(x,y) -> game op x y) rTable

-- "Gameloop"
main :: IO ()
main = do
  args <- getArgs
  
  let n = (read (args !! 0) :: Int)
  let a = (read (args !! 1) :: Int)
  let b = (read (args !! 2) :: Int)

  let op = drop 3 args

  mapM (drill n a b) op

  putStrLn "DONE"

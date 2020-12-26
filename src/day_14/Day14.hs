module Day14 where

import Data.Char
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

main = interact $ show.partOne.map parseLine.lines

partOne = id

type Reagent = (String, Int)
parseLine = fst . last . readP_to_S parser

parser = do
  reagents <- sepBy1 reagent (string ", ")
  _ <- string " => "
  product <- reagent
  return (product, reagents)

reagent :: ReadP Reagent
reagent = do
  n <- read <$> munch1 isDigit
  skipSpaces
  s <- munch1 isAlpha
  return (s,n)

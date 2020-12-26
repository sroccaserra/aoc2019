module Day14 where

import Data.Char
import qualified Data.Map as Map
import Data.Map ((!))
import Text.ParserCombinators.ReadP

partOne reactions = ingredients
  where ingredients = snd recipe
        recipe = grimoire ! "FUEL"
        grimoire = foldl storeByProductName Map.empty reactions

type Reagent = (String, Int)

storeByProductName m r = Map.insert name r m
  where name = fst $ fst r

---
-- Main

main = interact $ show . partOne . map parseLine . lines

---
-- Parsing

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

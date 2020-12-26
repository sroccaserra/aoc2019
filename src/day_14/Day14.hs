module Day14 where

import Data.Char
import qualified Data.Map as Map
import Data.Map (Map, (!), empty)
import Text.ParserCombinators.ReadP
import Debug.Trace

partOne reactions = oreFor grimoire empty ("FUEL", 1)
  where grimoire = foldl storeByProductName empty reactions

type Reagent = (String, Int)
type Reaction = (Reagent, [Reagent])

-- Not working, the stock' values are all independent, they should share insertions
oreFor :: Map String Reaction -> Map String Int -> Reagent -> Int
oreFor _ s ("ORE", n) = trace (show s) n
oreFor grimoire stock (name, needed) = nbReactions * (sum $ map (oreFor grimoire stock') ingredients)
  where ((_, provided), ingredients) = grimoire ! name :: Reaction
        available = Map.findWithDefault 0 name stock
        needed' = if available < needed then needed - available else 0
        nbReactions = ceiling $ (fromIntegral (needed') / fromIntegral (provided))
        stock' = Map.insertWith (+) name (nbReactions*provided - needed') stock

loadExample = foldl storeByProductName Map.empty . map parseLine

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

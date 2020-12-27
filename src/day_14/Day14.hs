module Day14 where

import Data.Char
import qualified Data.Map as Map
import Data.Map (Map, (!), empty)
import Text.ParserCombinators.ReadP
import Debug.Trace

partOne reactions = oreFor grimoire (0,empty) ("FUEL", 1)
  where grimoire = foldl storeByProductName empty reactions

type Reagent = (String, Int)
type Reaction = (Reagent, [Reagent])

type Grimoire = Map String Reaction
type Stock = Map String Int

type State = (Int, Stock)

-- stepReaction :: Grimoire -> State -> State
-- stepReaction g (((_, _), [("ORE", k)]), n, s) = ((("ORE", k), []), n+k, s)
-- stepReaction g (((i, p), i':rest), n, s) = stepReaction g (((i, p), rest), n', s')
--   where ((_, provided), )

-- Not working, the stock' values are all independent, they should share insertions
oreFor :: Grimoire -> State -> Reagent -> State
oreFor _ (s,stock) ("ORE", n) = (n+s, stock)
oreFor grimoire (s,stock) (name, needed) = foldl (\(n, s) (n', s') -> (n+n', Map.unionWith (+) s s')) (0,stock') results
  where results = map (oreFor grimoire (s,stock')) ingredients
        ((_, provided), ingredients) = grimoire ! name :: Reaction
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
  reagents <- reagent `sepBy` (string ", ")
  string " => "
  product <- reagent
  return (product, reagents)

reagent :: ReadP Reagent
reagent = do
  n <- read <$> munch1 isDigit
  skipSpaces
  s <- munch1 isAlpha
  return (s,n)

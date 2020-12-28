module Day14 where

import Data.Char
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Text.ParserCombinators.ReadP

partOne reactions =
  Map.lookup "ORE" $ until fullfiled (stepReaction grimoire) (Map.singleton "FUEL" 1)
  where grimoire = foldl storeByProductName Map.empty reactions

type Reagent = (String, Int)
type Reaction = (Reagent, [Reagent])

type Grimoire = Map String Reaction
type Required = Map String Int

storeByProductName m r@((name,_),_) = Map.insert name r m

fullfiled = Map.null . toProduce

toProduce = Map.delete "ORE" . Map.filter (> 0)

stepReaction :: Grimoire -> Required -> Required
stepReaction g reqs = foldl (require neededN) reqs' reagents
  where (needed, neededQ) = Map.findMin $ toProduce reqs
        ((_,providedQ), reagents) = g ! needed
        neededN = max 1 (div neededQ providedQ)
        reqs' = produce neededN reqs (needed, providedQ)

produce n reqs (name,q) = Map.insertWith (flip (-)) name (n*q) reqs

require n reqs (name,q) = Map.insertWith (+) name (n*q) reqs

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

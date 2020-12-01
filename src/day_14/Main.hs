import qualified Data.Map as M

type Material = String
type Quantity = Int
type Reagent = (Material, Quantity)
type Reaction = (Material, (Quantity, [Reagent]))

raw = [("SRPZP", (4, [("PQJGK", 29)])),
  ("ZVSMJ", (6, [("XKJP", 6), ("LDSMT", 4)])),
  ("DWZH", (7, [("CGCPW", 9), ("CFVS", 2)])),
  ("MGLKL", (1, [("VQWZC", 1), ("FRTJG", 1)])),
  ("RLZR", (4, [("GCTBC", 3), ("RGKB", 4)])),
  ("GCTBC", (9, [("LGLCP", 2)])),
  ("XVSRT", (2, [("DFPW", 3), ("FRTJG", 12)])),
  ("VQWZC", (3, [("VBXFG", 1)])),
  ("DGMZB", (8, [("GCTBC", 1), ("BVHM", 1)])),
  ("FRTJG", (7, [("DFPW", 7), ("JQNL", 3)])),
  ("HBLQT", (5, [("BHQN", 6), ("VXNP", 4)])),
  ("JZPMK", (7, [("XNBR", 1), ("FTBNQ", 27), ("RGKB", 2)])),
  ("STCX", (9, [("HKMV", 6), ("JHDHS", 4), ("NMSKF", 11)])),
  ("NVXTP", (5, [("ORE", 129)])),
  ("LKXML", (3, [("BVHM", 13), ("XNBR", 5)])),
  ("HXFCJ", (6, [("SBPM", 3), ("LDSMT", 4), ("GPBG", 13)])),
  ("FTBNQ", (4, [("XVSRT", 1), ("JHDHS", 1)])),
  ("PSJK", (5, [("LKXML", 6), ("HRLWP", 6)])),
  ("VRQD", (1, [("HRLWP", 5), ("PDHVG", 19)])),
  ("SBPM", (7, [("FTBNQ", 3), ("QLKTZ", 1)])),
  ("XKJP", (3, [("VXNP", 2)])),
  ("LMVF", (8, [("SRPZP", 4), ("XVSRT", 7)])),
  ("WZSTV", (2, [("GPBG", 2), ("DWZH", 8), ("JTCHR", 3), ("RLZR", 10), ("CFVS", 1), ("BFCZ", 20)])),
  ("JQNL", (9, [("ORE", 130)])),
  ("VBXFG", (4, [("ORE", 100)])),
  ("RDSHN", (8, [("XNBR", 4)])),
  ("CGCPW", (6, [("CDBTL", 2), ("XNBR", 2), ("QLKTZ", 8)])),
  ("BFCZ", (6, [("CGCPW", 7)])),
  ("BVHM", (1, [("FTBNQ", 7), ("VXNP", 7)])),
  ("DFXPB", (1, [("HXFCJ", 1), ("CSXD", 15)])),
  ("SWRV", (8, [("MCRW", 1), ("MGLKL", 6), ("HBLQT", 1)])),
  ("FUEL", (1, [("BZQGL", 19), ("NMSKF", 10), ("WZSTV", 20), ("LVGB", 15), ("FTBNQ", 26), ("DWZH", 45), ("FJWVP", 2), ("JZPMK", 56)])),
  ("CDBTL", (4, [("JTCHR", 12)])),
  ("PQJGK", (6, [("MGLKL", 1)])),
  ("LDSMT", (1, [("NVXTP", 1)])),
  ("GHVJ", (2, [("SWRV", 3), ("LGLCP", 1)])),
  ("SHTB", (4, [("DGMZB", 4), ("HXFCJ", 11), ("RLZR", 2)])),
  ("HKMV", (6, [("GHVJ", 5), ("RGKB", 1), ("GCTBC", 1)])),
  ("XNBR", (9, [("SRPZP", 1)])),
  ("JHDHS", (2, [("ZVSMJ", 1)])),
  ("NMSKF", (5, [("SWRV", 9)])),
  ("XKBS", (3, [("NVXTP", 1)])),
  ("GPBG", (2, [("BHQN", 7)])),
  ("CMXK", (3, [("NMSKF", 21), ("FTBNQ", 12), ("SBPM", 12)])),
  ("LGLCP", (4, [("GPBG", 2), ("ZVSMJ", 12), ("PDHVG", 2)])),
  ("DFPW", (8, [("ORE", 158)])),
  ("LVGB", (7, [("BVHM", 3), ("HXFCJ", 1), ("CGCPW", 5), ("BFCZ", 5), ("VRQD", 6), ("LDSMT", 3)])),
  ("PDHVG", (8, [("XVSRT", 1), ("XKJP", 1)])),
  ("BZQGL", (9, [("VRQD", 3), ("SHTB", 16), ("SBPM", 5)])),
  ("CFVS", (4, [("BVHM", 1), ("HKMV", 3)])),
  ("VXNP", (7, [("JQNL", 13)])),
  ("MCRW", (7, [("XKJP", 1), ("XVSRT", 6)])),
  ("BHQN", (4, [("NVXTP", 15), ("XKBS", 19)])),
  ("CSXD", (8, [("JHDHS", 8), ("CMXK", 5), ("GPBG", 2)])),
  ("FJWVP", (7, [("JZBR", 1), ("LKXML", 13), ("HKMV", 1), ("DFXPB", 9), ("XKBS", 3), ("PSJK", 2), ("LMVF", 2), ("HRLWP", 15)])),
  ("JZBR", (8, [("CGCPW", 1), ("RDSHN", 3)])),
  ("JTCHR", (5, [("PQJGK", 24)])),
  ("QLKTZ", (6, [("XVSRT", 1), ("LDSMT", 5)])),
  ("RGKB", (7, [("GPBG", 17)])),
  ("HRLWP", (1, [("STCX", 4)]))]

main :: IO ()
main = do
  print $ maybe (0, []) id $ findReaction "VXNP" g
    where g = foldr writeReaction emptyGrimoire raw

findReaction :: Material -> Grimoire -> Maybe (Quantity, [Reagent])
findReaction = M.lookup

writeReaction :: Reaction -> Grimoire -> Grimoire
writeReaction (m, p) = M.insert m p

type Grimoire = M.Map Material (Quantity, [Reagent])
emptyGrimoire = M.empty

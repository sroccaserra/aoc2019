solve = id

main = do
  input <- getContents
  mapM_ putStrLn $ solve $ lines input

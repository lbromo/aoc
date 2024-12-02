import Data.List (elemIndex, sort, transpose)

processInputFile :: FilePath -> IO [[Int]]
processInputFile path = do
  content <- readFile path
  let rows = map (map read . words) (lines content)
  return rows

diff :: [Int] -> [Int]
diff xs = map (\(v1, v2) -> v2 - v1) $ zip xs $ tail xs

safe :: [Int] -> Bool
safe xs =
  let dsize = all (\x -> abs x >= 1 && abs x <= 3) xs
      signs = all (\v -> signum v == signum (head xs)) $ tail xs
   in dsize && signs

-- Assumes `safe :: [Int] -> Bool` is already implemented
safeWithDampener :: [Int] -> Bool
safeWithDampener xs
  | (safe . diff) xs = True -- Already safe
  | otherwise = any (safe . diff . removeAtIndex xs) [0 .. length xs - 1]
  where
    -- Function to remove an element at a given index
    removeAtIndex :: [a] -> Int -> [a]
    removeAtIndex ys i = take i ys ++ drop (i + 1) ys

main :: IO ()
main = do
  let path = "input2.txt"
  arrays <- processInputFile path

  let nSafe = length $ filter id $ map (safe . diff) arrays

  putStrLn $ "Answer 1: " ++ show nSafe

  let nSafeWithDapener = length $ filter safeWithDampener arrays
  putStrLn $ "Answer 2: " ++ show nSafeWithDapener

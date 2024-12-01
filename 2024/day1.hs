import Data.List (sort, transpose)

processInputFile :: FilePath -> IO ([Int], [Int])
processInputFile path = do
  content <- readFile path
  let rows = map (map read . words) (lines content)
      [left, right] = transpose rows
  return (left, right)

main :: IO ()
main = do
  let path = "input.txt" -- Use `let` for local definitions
  (left, right) <- processInputFile path

  -- 1 distance between sorted lists
  let distance = foldl (+) 0 $ map (\(x, y) -> abs (x - y)) $ zip (sort left) (sort right)
  putStrLn ("Answer 1: " ++ show distance)

  -- 2 similarity score
  let counts = map (\x -> length (x)) $ map (\pred -> filter (\x -> x == pred) right) left
  let similarity_score = foldl (+) 0 $ map (\(c, v) -> c * v) $ zip counts left

  putStrLn ("Answer 2: " ++ show similarity_score)

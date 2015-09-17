import Data.List(delete)
import Data.Function(fix)
import Control.Monad
import System.Random

exactMatches :: Eq a => [a] -> [a] -> Int
exactMatches = ((length . filter id) .) . zipWith (==)

quasiMatches :: Eq a => [a] -> [a] -> Int
quasiMatches [] _ = 0
quasiMatches (x:xs) ys = (if x `elem` ys then 1 else 0) + quasiMatches xs (delete x ys)

validString :: Eq a => Int -> [a] -> [a] -> Bool
validString len dict str = length str == len && all (`elem` dict) str

doGame :: Int -> Int -> [Char] -> String -> IO ()
doGame 0 _ _ corr = putStrLn $ "Sorry, you lose. It was " ++ corr ++ "."
doGame gss len dict corr = do
  guess <- fix $ \again -> do
             putStr "Enter guess: "
             gg <- getLine
             if validString len dict gg then
                 return gg
             else
                 again
  if guess == corr then
      putStrLn "You got it!"
  else do
    let e = exactMatches guess corr
        q = quasiMatches guess corr
    putStrLn $ "Right Place: " ++ show e
    putStrLn $ "Right Digit: " ++ show (q - e)
    doGame (gss - 1) len dict corr

doGameDefault :: IO ()
doGameDefault = replicateM 4 (randomRIO ('0', '9')) >>= doGame 12 4 ['0'..'9']

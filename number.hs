import Data.Function(fix)
import Control.Monad
import Text.Read(readMaybe)
import System.Random

numberGame :: (Show a, Read a, Ord a) => (a, a) -> Int -> a -> IO ()
numberGame _ 0 corr = putStrLn $ "You lose. It was " ++ show corr
numberGame (mn, mx) n corr = do
  guess <- fix $ \g -> do
             putStr "> "
             gss <- readMaybe <$> getLine
             case gss of
               Nothing -> g
               Just x
                 | x >= mn && x <= mx -> return x
                 | otherwise -> g
  case compare guess corr of
    LT -> putStrLn "Too low..." >> numberGame (mn, mx) (n - 1) corr
    GT -> putStrLn "Too high..." >> numberGame (mn, mx) (n - 1) corr
    EQ -> putStrLn "You got it!"

numberGame' :: (Show a, Read a, Ord a, Random a) => (a, a) -> Int -> IO ()
numberGame' rng n = randomRIO rng >>= numberGame rng n

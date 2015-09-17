{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Control.Arrow
import Data.Char
import Data.List
import Data.Ix
import Data.Monoid
import Data.Maybe
import Data.STRef
import Data.Array.ST
import Data.Function
import Data.Ratio
import Data.Ord
import Data.Bits
import Data.Complex
import Data.Traversable(sequenceA)
import Numeric
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import qualified Factory.Math.Primality as Pr
import Data.Sequence(ViewL(..), ViewR(..), (|>), (<|), viewl, viewr)

divisible :: (Integral a) => a -> a -> Bool
divisible y x = (x `div` y) * y == x

factors :: (Integral a) => a -> [a]
factors x = lowDivs ++ sqrtDivs ++ highDivs
    where bound = ceiling . sqrt . fromRational . toRational $ x
          lowDivs = filter (flip divisible $ x) [1..bound-1]
          highDivs = reverse . map (x `div`) $ lowDivs
          sqrtDivs = if bound * bound == x then [bound] else []

isPrime :: (Integral a) => a -> Bool
isPrime = (liftA2 (&&) isPrimeList isNotOneList) . factors
    where isPrimeList = null . drop 2
          isNotOneList = not . null . drop 1

isPalindromic :: (Show a) => a -> Bool
isPalindromic x = show x == reverse (show x)

withSelf :: (a -> a -> b) -> a -> b
withSelf f x = f x x

eulerEightNumber :: String
eulerEightNumber = join
                   ["73167176531330624919225119674426574742355349194934",
                    "96983520312774506326239578318016984801869478851843",
                    "85861560789112949495459501737958331952853208805511",
                    "12540698747158523863050715693290963295227443043557",
                    "66896648950445244523161731856403098711121722383113",
                    "62229893423380308135336276614282806444486645238749",
                    "30358907296290491560440772390713810515859307960866",
                    "70172427121883998797908792274921901699720888093776",
                    "65727333001053367881220235421809751254540594752243",
                    "52584907711670556013604839586446706324415722155397",
                    "53697817977846174064955149290862569321978468622482",
                    "83972241375657056057490261407972968652414535100474",
                    "82166370484403199890008895243450658541227588666881",
                    "16427171479924442928230863465674813919123162824586",
                    "17866458359124566529476545682848912883142607690042",
                    "24219022671055626321111109370544217506941658960408",
                    "07198403850962455444362981230987879927244284909188",
                    "84580156166097919133875499200524063689912560717606",
                    "05886116467109405077541002256983155200055935729725",
                    "71636269561882670428252483600823257530420752963450"]

slice :: Int -> Int -> [a] -> [a]
slice begin end = take (end - begin) . drop begin

subdivideN :: Int -> [a] -> [[a]]
subdivideN n xs
    | length xs >= n = take n xs : (subdivideN n $ tail xs)
    | otherwise = []
--subdivideN n xs = (:) <$> (take n) <*> (subdivideN n . tail) $ xs

isPrimeQuick :: (Integral a) => a -> Bool
isPrimeQuick 1 = False
isPrimeQuick 2 = True
isPrimeQuick x = not . any (flip divisible $ x) $ [2..bound]
    where bound = ceiling . sqrt . fromRational . toRational $ x

eulerElevenNumber :: [[Int]]
eulerElevenNumber = map (map read) . map words $
      ["08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08",
       "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00",
       "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65",
       "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91",
       "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80",
       "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50",
       "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70",
       "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21",
       "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72",
       "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95",
       "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92",
       "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57",
       "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58",
       "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40",
       "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66",
       "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69",
       "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36",
       "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16",
       "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54",
       "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"]

addIncreasinglyToEnd :: Int -> a -> [[a]] -> [[a]]
addIncreasinglyToEnd _ _ [] = []
addIncreasinglyToEnd n x xs = (head xs ++ toAdd) : (recurse)
    where toAdd = replicate n x
          recurse = addIncreasinglyToEnd (n + 1) (x) (tail xs)

addDecreasinglyToEnd :: Int -> a -> [[a]] -> [[a]]
addDecreasinglyToEnd _ _ [] = []
addDecreasinglyToEnd n x xs = (head xs ++ toAdd) : (recurse)
    where toAdd = replicate n x
          recurse = addDecreasinglyToEnd (n - 1) (x) (tail xs)

fluffSize :: Int -> a -> [a] -> [a]
fluffSize n x xs
    | length xs >= n = xs
    | otherwise = fluffSize n x $ x:xs

triangularNumber :: (Integral a) => a -> a
triangularNumber n = sum [1..n]

eulerThirteenNumber :: [Integer]
eulerThirteenNumber = [37107287533902102798797998220837590246510135740250,
                       46376937677490009712648124896970078050417018260538,
                       74324986199524741059474233309513058123726617309629,
                       91942213363574161572522430563301811072406154908250,
                       23067588207539346171171980310421047513778063246676,
                       89261670696623633820136378418383684178734361726757,
                       28112879812849979408065481931592621691275889832738,
                       44274228917432520321923589422876796487670272189318,
                       47451445736001306439091167216856844588711603153276,
                       70386486105843025439939619828917593665686757934951,
                       62176457141856560629502157223196586755079324193331,
                       64906352462741904929101432445813822663347944758178,
                       92575867718337217661963751590579239728245598838407,
                       58203565325359399008402633568948830189458628227828,
                       80181199384826282014278194139940567587151170094390,
                       35398664372827112653829987240784473053190104293586,
                       86515506006295864861532075273371959191420517255829,
                       71693888707715466499115593487603532921714970056938,
                       54370070576826684624621495650076471787294438377604,
                       53282654108756828443191190634694037855217779295145,
                       36123272525000296071075082563815656710885258350721,
                       45876576172410976447339110607218265236877223636045,
                       17423706905851860660448207621209813287860733969412,
                       81142660418086830619328460811191061556940512689692,
                       51934325451728388641918047049293215058642563049483,
                       62467221648435076201727918039944693004732956340691,
                       15732444386908125794514089057706229429197107928209,
                       55037687525678773091862540744969844508330393682126,
                       18336384825330154686196124348767681297534375946515,
                       80386287592878490201521685554828717201219257766954,
                       78182833757993103614740356856449095527097864797581,
                       16726320100436897842553539920931837441497806860984,
                       48403098129077791799088218795327364475675590848030,
                       87086987551392711854517078544161852424320693150332,
                       59959406895756536782107074926966537676326235447210,
                       69793950679652694742597709739166693763042633987085,
                       41052684708299085211399427365734116182760315001271,
                       65378607361501080857009149939512557028198746004375,
                       35829035317434717326932123578154982629742552737307,
                       94953759765105305946966067683156574377167401875275,
                       88902802571733229619176668713819931811048770190271,
                       25267680276078003013678680992525463401061632866526,
                       36270218540497705585629946580636237993140746255962,
                       24074486908231174977792365466257246923322810917141,
                       91430288197103288597806669760892938638285025333403,
                       34413065578016127815921815005561868836468420090470,
                       23053081172816430487623791969842487255036638784583,
                       11487696932154902810424020138335124462181441773470,
                       63783299490636259666498587618221225225512486764533,
                       67720186971698544312419572409913959008952310058822,
                       95548255300263520781532296796249481641953868218774,
                       76085327132285723110424803456124867697064507995236,
                       37774242535411291684276865538926205024910326572967,
                       23701913275725675285653248258265463092207058596522,
                       29798860272258331913126375147341994889534765745501,
                       18495701454879288984856827726077713721403798879715,
                       38298203783031473527721580348144513491373226651381,
                       34829543829199918180278916522431027392251122869539,
                       40957953066405232632538044100059654939159879593635,
                       29746152185502371307642255121183693803580388584903,
                       41698116222072977186158236678424689157993532961922,
                       62467957194401269043877107275048102390895523597457,
                       23189706772547915061505504953922979530901129967519,
                       86188088225875314529584099251203829009407770775672,
                       11306739708304724483816533873502340845647058077308,
                       82959174767140363198008187129011875491310547126581,
                       97623331044818386269515456334926366572897563400500,
                       42846280183517070527831839425882145521227251250327,
                       55121603546981200581762165212827652751691296897789,
                       32238195734329339946437501907836945765883352399886,
                       75506164965184775180738168837861091527357929701337,
                       62177842752192623401942399639168044983993173312731,
                       32924185707147349566916674687634660915035914677504,
                       99518671430235219628894890102423325116913619626622,
                       73267460800591547471830798392868535206946944540724,
                       76841822524674417161514036427982273348055556214818,
                       97142617910342598647204516893989422179826088076852,
                       87783646182799346313767754307809363333018982642090,
                       10848802521674670883215120185883543223812876952786,
                       71329612474782464538636993009049310363619763878039,
                       62184073572399794223406235393808339651327408011116,
                       66627891981488087797941876876144230030984490851411,
                       60661826293682836764744779239180335110989069790714,
                       85786944089552990653640447425576083659976645795096,
                       66024396409905389607120198219976047599490197230297,
                       64913982680032973156037120041377903785566085089252,
                       16730939319872750275468906903707539413042652315011,
                       94809377245048795150954100921645863754710598436791,
                       78639167021187492431995700641917969777599028300699,
                       15368713711936614952811305876380278410754449733078,
                       40789923115535562561142322423255033685442488917353,
                       44889911501440648020369068063960672322193204149535,
                       41503128880339536053299340368006977710650566631954,
                       81234880673210146739058568557934581403627822703280,
                       82616570773948327592232845941706525094512325230608,
                       22918802058777319719839450180888072429661980811197,
                       77158542502016545090413245809786882778948721859617,
                       72107838435069186155435662884062257473692284509516,
                       20849603980134001723930671666823555245252804609722,
                       53503534226472524250874054075591789781264330331690]

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
    | even x = x:(collatz $ x `div` 2)
    | otherwise = x:(collatz $ 3 * x + 1)

cachedFunction :: (Ord a) => (a -> b) -> a -> State (Map.Map a b) b
cachedFunction f x = state $ \m -> case Map.lookup x m of
                                   Just y -> (y, m)
                                   Nothing ->
                                     let z = f x in (z, Map.insert x z m)

collatzLength :: (Integral a) => a -> State (Map.Map a Int) Int
collatzLength 1 = return 1
collatzLength x = state $ \m -> case Map.lookup x m of
                                Just y -> (y, m)
                                Nothing ->
                                    let (z, m') = runState (collatzLength n) m in (z + 1, Map.insert x (z + 1) m')
    where n = if even x then x `div` 2 else 3 * x + 1

writeNumber :: Int -> String
writeNumber 1000 = "one thousand"
writeNumber x | x < 20 = case x of 0 -> "zero"
                                   1 -> "one"
                                   2 -> "two"
                                   3 -> "three"
                                   4 -> "four"
                                   5 -> "five"
                                   6 -> "six"
                                   7 -> "seven"
                                   8 -> "eight"
                                   9 -> "nine"
                                   10 -> "ten"
                                   11 -> "eleven"
                                   12 -> "twelve"
                                   13 -> "thirteen"
                                   14 -> "fourteen"
                                   15 -> "fifteen"
                                   16 -> "sixteen"
                                   17 -> "seventeen"
                                   18 -> "eighteen"
                                   19 -> "nineteen"
writeNumber x | x `mod` 100 == 0 = writeNumber (x `div` 100) ++ " hundred"
writeNumber x | x > 100 = writeNumber (x `div` 100) ++ " hundred and " ++ writeNumber (x `mod` 100)
writeNumber x | x `mod` 10 == 0 = case x of 20 -> "twenty"
                                            30 -> "thirty"
                                            40 -> "forty"
                                            50 -> "fifty"
                                            60 -> "sixty"
                                            70 -> "seventy"
                                            80 -> "eighty"
                                            90 -> "ninety"
writeNumber x | x < 100 = writeNumber ((x `div` 10) * 10) ++ "-" ++ writeNumber (x `mod` 10)
writeNumber x = undefined

eulerEighteenNumber :: [[Int]]
eulerEighteenNumber = [[75],[95,64],[17,47,82],[18,35,87,10],[20,04,82,47,65]
 ,[19,01,23,75,03,34],[88,02,77,73,07,63,67],[99,65,04,28,06,16,70,92]
 ,[41,41,26,56,83,40,80,70,33],[41,48,72,33,47,32,37,16,94,29]
 ,[53,71,44,65,25,43,91,52,97,51,14],[70,11,33,28,77,73,17,78,39,68,17,57]
 ,[91,71,52,38,17,14,91,43,58,50,27,29,48]
 ,[63,66,04,68,89,53,67,30,73,16,69,87,40,31]
 ,[04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Eq, Ord, Enum)
type Day = Int
type Year = Int
data Date = Date Month Day Year deriving (Show, Eq)
data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq, Enum)
data FullDate = FullDate Date Weekday deriving (Show, Eq)

getMonth :: Date -> Month
getMonth (Date x _ _) = x

getDay :: Date -> Day
getDay (Date _ x _) = x

getYear :: Date -> Year
getYear (Date _ _ x) = x

getDate :: FullDate -> Date
getDate (FullDate x _) = x

getWeekday :: FullDate -> Weekday
getWeekday (FullDate _ x) = x

isLeapYear :: Year -> Bool
isLeapYear x = (x `mod` 400 == 0) || (x `mod` 4 == 0 && x `mod` 100 /= 0)

getDaysInMonth :: Date -> Int
getDaysInMonth (Date Sep _ _) = 30
getDaysInMonth (Date Apr _ _) = 30
getDaysInMonth (Date Jun _ _) = 30
getDaysInMonth (Date Nov _ _) = 30
getDaysInMonth (Date Feb _ n) = if isLeapYear n then 29 else 28
getDaysInMonth _ = 31

getNextDate :: Date -> Date
getNextDate dx@(Date m d y) = if succ d <= getDaysInMonth dx
                                  then Date m (succ d) y
                              else if m /= Dec
                                  then Date (succ m) 1 y
                              else
                                  Date Jan 1 (succ y)

getNextDay :: Weekday -> Weekday
getNextDay Sun = Mon
getNextDay x = succ x

getSuccDay :: FullDate -> FullDate
getSuccDay (FullDate d w) = FullDate (getNextDate d) (getNextDay w)

properDivisors :: Int -> [Int]
properDivisors n = runST $ do xs <- newSTRef []
                              let n' = floor . sqrt . fromIntegral $ n
                              if n > 1 then
                                modifySTRef' xs (1:)
                              else
                                return ()
                              forM_ [2..n'] $ \x ->
                                  if n `rem` x == 0 then
                                      if x == n `quot` x then
                                          modifySTRef' xs $ (x:)
                                      else
                                          modifySTRef' xs $ (x:) . (n `quot` x:)
                                  else
                                      return ()
                              readSTRef xs


sumOfProperDivisors :: Int -> Int
sumOfProperDivisors n = runST $ do y <- newSTRef 0
                                   let n' = floor . sqrt . fromIntegral $ n
                                       ns = filter ((== 0) . (n `rem`)) [2..n']
                                   if n > 1 then
                                     modifySTRef' y (+ 1)
                                   else
                                     return ()
                                   forM_ ns $ \x ->
                                       if x == n `quot` x then
                                           modifySTRef' y $ (x +)
                                       else
                                           modifySTRef' y $ (x + (n `quot` x) +)
                                   readSTRef y

newtype Euler44 = Euler44 {getEuler44 :: (Integer, Integer)}
    deriving (Show, Read)

instance Eq Euler44 where
    (==) = (==) `on` getEuler44

instance Ord Euler44 where
    compare (Euler44 a) (Euler44 b) =
        (compare `on` uncurry subtract . euler44Node) a b `mappend` compare a b

euler44Node :: Integral a => (a, a) -> (a, a)
euler44Node (x, y) = (pentagonal y, pentagonal (x + y))

pentagonal :: Integral a => a -> a
pentagonal n = n * (3 * n - 1) `quot` 2

triangular :: Integral a => a -> a
triangular n = n * (n + 1) `quot` 2

hexagonal :: Integral a => a -> a
hexagonal n = n * (2 * n - 1)

isPentagonal :: Integral a => a -> Bool
isPentagonal x = isInt value
    where epsilon = 0.1 ^ 5
          isInt n = (abs (n - fromInteger (round n))) <= epsilon
          value = (-1 - 2 * sqrt (1/4 + 6 * fromIntegral x)) / 6 :: Double

isHexagonal :: Integral a => a -> Bool
isHexagonal x = isInt value
    where epsilon = 0.1 ^ 5
          isInt n = (abs (n - fromInteger (round n))) <= epsilon
          value = (1 + sqrt (1 + 8 * fromIntegral x)) / 4 :: Double

primeFactors :: Integral a => a -> [a]
primeFactors n
 | n < 2 = undefined
 | isPrime n = [n]
 | otherwise = firstPrime : (primeFactors $ n `quot` firstPrime)
    where firstPrime = head . filter (flip divisible n) $ [2..n]

elem' :: Ord a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = case compare y x of
                     EQ -> True
                     LT -> False
                     GT -> elem' y xs

sieveOfEratosthenes :: forall a. (Integral a, Ix a) => a -> [a]
sieveOfEratosthenes n = runST $ do
                    arr <- newArray (2, n) False :: ST s (STUArray s a Bool) 
                    forM_ [2..n] $ \m -> do
                        x <- readArray arr m
                        unless x $
                            forM_ [2*m,3*m..n] $ \m' ->
                                writeArray arr m' True
                    res <- getAssocs arr
                    return (map fst . filter (not . snd) $ res)

traceShowCond :: (Show a) => Bool -> a -> b -> b
traceShowCond True = traceShow
traceShowCond False = const id

data Suit = Heart | Diamond | Club | Spade
    deriving (Show, Read, Eq, Enum)

data Card = Card Value Suit deriving Eq

type Value = Int

data PokerHand = HighCard Value | OnePair Value | TwoPair Value Value |
                 ThreeOfAKind Value | Straight | Flush |
                 FullHouse Value Value | FourOfAKind Value |
                 StraightFlush | RoyalFlush
    deriving (Show, Read, Eq, Ord)

type PokerValue = (PokerHand, Value) -- Hand and high card

getValue :: Card -> Value
getValue (Card x _) = x

getSuit :: Card -> Suit
getSuit (Card _ x) = x

instance Show Card where
    showsPrec _ (Card n x) = (value n :) . (suit x :)
        where suit y = head $ show y
              value 10 = 'T'
              value 11 = 'J'
              value 12 = 'Q'
              value 13 = 'K'
              value 14 = 'A'
              value n = head $ show n

instance Read Card where
    readsPrec _ (v:s:xs) = maybeToList $ do
                            v' <- value v
                            s' <- suit s
                            return (Card v' s', xs)
        where suit 'H' = Just Heart
              suit 'D' = Just Diamond
              suit 'S' = Just Spade
              suit 'C' = Just Club
              value 'T' = Just 10
              value 'J' = Just 11
              value 'Q' = Just 12
              value 'K' = Just 13
              value 'A' = Just 14
              value n = listToMaybe . map fst $ readsPrec 0 [n]

compareHands :: PokerValue -> PokerValue -> Ordering
compareHands (x, xh) (y, yh) = compare x y `mappend` compare xh yh

cardsToHand :: [Card] -> PokerValue
cardsToHand [] = error "Empty hand"
cardsToHand xs = let xs' = sortBy (compare `on` getValue) xs in
                    (maybe (error "No valid poker hand") id .
                     msum . map ($ xs') $ precedence,
                        getValue . last $ xs')
    where precedence = [royalFlush, straightFlush, fourOfAKind,
                        fullHouse, flush, straight, threeOfAKind,
                        twoPair, onePair, highCard]
          numbers = map getValue
          findPair (r, x) y = if x == y then (Just x, y) else (r, y)
          findPairs (rs, x) y = if x == y then (x:rs, y) else (rs, y)
          isStraight (b, x) y = (b && (x + 1 == y), y)
          allEqual [] = True
          allEqual xs = isJust . foldl (\x y -> guard (x == Just y) >> x)
                                (Just $ head xs) $ xs
          highCard ys = Just $ HighCard (getValue $ last ys)
          onePair ys = fmap OnePair . fst .
                        foldl findPair (Nothing, -1) $ numbers ys
          twoPair ys = case foldl findPairs ([], -1) $ numbers ys of
                            ([p1, p2], _) -> Just $ TwoPair p1 p2
                            _ -> Nothing
          threeOfAKind ys = case numbers ys of
                                [x,y,z,_,_] | x == y, y == z ->
                                    Just $ ThreeOfAKind x
                                [_,x,y,z,_] | x == y, y == z ->
                                    Just $ ThreeOfAKind x
                                [_,_,x,y,z] | x == y, y == z ->
                                    Just $ ThreeOfAKind x
                                _ -> Nothing
          straight ys = case foldl isStraight (True, head (numbers ys) - 1)
                                $ numbers ys of
                            (True, _) -> Just Straight
                            _ -> Nothing
          flush ys = if allEqual . map getSuit $ ys then Just Flush else Nothing
          fullHouse ys = case numbers ys of
                            [x,y,z,t,u]
                             | x == y && y == z && t == u ->
                                Just $ FullHouse x t
                            [x,y,z,t,u]
                             | x == y && z == t && t == u ->
                                Just $ FullHouse z x
                            _ -> Nothing
          fourOfAKind ys = case numbers ys of
                                [x,y,z,t,_] | x == y, y == z, z == t ->
                                    Just $ FourOfAKind x
                                [_,x,y,z,t] | x == y, y == z, z == t ->
                                    Just $ FourOfAKind x
                                _ -> Nothing
          straightFlush ys = case liftA2 (,) straight flush ys of
                              (Just Straight, Just Flush) -> Just StraightFlush
                              _ -> Nothing
          royalFlush ys = if isJust (straightFlush ys) &&
                             getValue (head ys) == 10 then
                                Just RoyalFlush
                          else
                                Nothing

newtype Euler60 = Euler60 {getEuler60 :: [[Int]]}
    deriving (Show, Read)

instance Eq Euler60 where
    (Euler60 x) == (Euler60 y) = map head x == map head y

instance Ord Euler60 where
    (Euler60 a) `compare` (Euler60 b) = let a' = map head a
                                            b' = map head b in
        compare (sum a') (sum b') `mappend` compare a' b'

inverseQuad :: RealFloat a => a -> a -> a -> (Complex a, Complex a)
inverseQuad a b c = let discr = b * b - 4 * a * c
                        sq = sqrt . cc $ discr in
                      ((- cc b + sq) / (2 * cc a),
                       (- cc b - sq) / (2 * cc a))
    where cc = fromRational . toRational

-- Begin Problems

euler1 :: Int
euler1 = sum $ filter threeOrFive [1..999]
    where threeOrFive = (||) <$> (divisible 3) <*> (divisible 5)

euler2 :: Int
euler2 = sum . filter even . fibo $ [2, 1]
    where fibo (x:y:xs) = if x+y > stopValue then x:y:xs else
                            fibo $ (x+y):x:y:xs
          stopValue = 4000000

euler3 :: Integer
euler3 = head $ highPrimes ++ lowPrimes
    where number = 600851475143
          bound = floor . sqrt . fromRational . toRational $ number
          divis = flip divisible $ number
          lowDivs = filter divis $ [bound,(bound-1)..1]
          lowPrimes = filter isPrime $ lowDivs
          highPrimes = filter isPrime . map (number `div`) $ lowDivs

euler4 :: Int
euler4 = maximum . filter isPalindromic $ allNums
    where range = [100..999]
          allNums = (*) <$> range <*> range

euler5 :: Int
euler5 = head . filter (\y -> all ($ y) conditions) $ [2520,2540..]
    where conditions = [divisible x | x <- [11..20]]

euler6 :: Int
euler6 = squareOfSums 100 - sumOfSquares 100
    where sumOfSquares x = sum . map square $ [1..x]
          squareOfSums x = square . sum $ [1..x]
          square = withSelf (*)

euler7 :: Int
euler7 = (!! 10000) . filter isPrime $ [1..]

euler8 :: Int
euler8 = maximum . map product . map (map digitToInt) . subdivideN 5 $ eulerEightNumber 

euler9 :: Int
euler9 = productOfThree . head . filter anyNegative . filter pythTriplet $ allValues
    where allValues = groupInto <$> [1..1000] <*> [1..1000]
          groupInto x y = (x, y, 1000 - x - y)
          pythTriplet (x, y, z) = x * x + y * y == z * z
          productOfThree (x, y, z) = x * y * z
          anyNegative (x, y, z) = not . any (< 0) $ [x, y, z]

euler10 :: Integer
euler10 = sum . filter isPrimeQuick $ [1..2000000]

euler11 :: Int
euler11 = maximum $ (concat horiz) ++ (concat vert) ++ (concat diag) ++ (concat diag')
    where sumHoriz = map (map product) . map (subdivideN 4)
          horiz = sumHoriz $ eulerElevenNumber
          vert  = sumHoriz . transpose $ eulerElevenNumber
          diag  = sumHoriz . transpose . map funcFluff . funcAdd  $ eulerElevenNumber
          diag' = sumHoriz . transpose . map funcFluff . funcAdd' $ eulerElevenNumber
          funcFluff = fluffSize (2 * (length . head $ eulerElevenNumber)) 0
          funcAdd  xs = addIncreasinglyToEnd 0 0 xs
          funcAdd' xs = addDecreasinglyToEnd (length xs) 0 xs

euler12 :: Int
euler12 = head . filter ((> 500) . (length . factors)) . map triangularNumber $ [1..]

euler13 :: Integer
euler13 = read . take 10 . show . sum $ eulerThirteenNumber

euler14 = fst . foldl1 foldFunction . zip [1..] . map (length . collatz) $ [1..999999]
    where foldFunction xt@(_, x) yt@(_, y) = if x > y then xt else yt

euler15 :: Integer
euler15 = factorial 40 `div` (factorial 20 ^ 2)
    where factorial x = product [1..x]

euler16 :: Int
euler16 = powerSum 1000
    where powerSum = sum . map (read . return) . show . (2 ^)

euler17 :: Int
euler17 = sum . map charLength . map writeNumber $ [1..1000]
    where charLength = length . filter isAlpha

euler18 :: Int
euler18 = maximum . foldl1 integrate $ eulerEighteenNumber
    where integrate xs ys = map (\(n, y) -> y + forRow xs n) . zip [0..] $ ys
          forRow xs n = max (xs `get` n) (xs `get` (n - 1))
          get xs x | x < 0 = 0
          get xs x | x >= length xs = 0
          get xs x = xs !! x

euler19 :: Int
euler19 = length . filter ((== 1) . getDay . getDate) .
          filter ((== Sun) . getWeekday) . takeWhile ((/= stop) . getDate) .
          dropWhile ((/= start) . getDate) . iterate getSuccDay $
          FullDate mon Mon
    where mon = Date Jan 1 1900
          start = Date Jan 1 1901
          stop = Date Jan 1 2001

euler20 :: Int
euler20 = sum . map (read . return) . show $ factorial 100
    where factorial x = product [1..x]

euler21 :: Int
euler21 = sum . filter (\x -> amicable x (sum . divisors $ x)) $ [1..9999]
    where divisors n = filter ((== 0) . (n `mod`)) $ [1..(n-1)]
          amicable x y
           | x == y = False
           | otherwise = (sum . divisors $ x) == y && (sum . divisors $ y) == x

euler22 :: IO Int
euler22 = readFile "names.txt" >>=
            (return . sum . zipWith (*) [1..] . map wordValue . sort . commaSplit)
    where commaSplit = map (filter (/= '"')) . words . map (\x -> if x == ',' then ' ' else x)
          alphaValue = (+ 1) . (subtract $ ord 'A') . ord . toUpper
          wordValue = sum . map alphaValue

euler23 :: Int
euler23 = sum . map fst . filter (null . snd) .
          map (\(x, y) -> (x, filter (validNumber x) y)) .
          map (second $ curry range 1 . (`quot` 2)) .
          map (join (,)) $ [1..28123]
    where isAbundant n = n < sumOfProperDivisors n
          validNumber y x = isAbundant x && isAbundant (y - x)

euler24 :: Integer
euler24 = read . (!! 999999) . sort . permutations $ "0123456789"

euler25 :: Integer
euler25 = fst $ evalState (stateList >>= (return . head . dropWhile condition)) initial
    where initial = (0, 0, 1)
          stateComp = state $ \(n, x, y) -> ((n, x), (n + 1, y, x + y))
          stateList = sequence . repeat $ stateComp
          condition = (< 1000) . length . show . snd

euler26 :: Int
euler26 = fst . maximumBy (compare `on` (maybe minBound id . snd)) .
            map (second $ evalState netState . (,) 1) .
            map (join (,)) $ [1..999]
    where divideOnce (n, m) = (n `quotRem` m, ((n - (n `quot` m) * m) * 10, m))
          divideState = state divideOnce
          stateList = sequence . repeat $ divideState
          netState = stateList >>= (return . msum . snd . mapAccumL recurrenceCycle [])
          recurrenceCycle acc x = (x:acc, fmap (+ 1) . findIndex (==x) $ acc)

euler27 :: Int
euler27 = uncurry (*) . fst . maximumBy (compare `on` snd) .
            map (second seqPrimes) $ functions
    where quadratic a b n = n * n + a * n + b
          parameters = [(x, y) | x <- [-999..999], y <- [-999..999]]
          functions = (id &&& uncurry quadratic) <$> parameters
          seqPrimes f = length . takeWhile isPrime . map f $ [0..]

euler28 :: Int
euler28 = case foldl accumulator (0, 0, 0, 0) $ [1..1001*1001] of
            (x, _, _, _) -> x
    where accumulator (n, a, 0 , 0) x = (n + x, a + 2, a + 1, 3)
          accumulator (n, a, 0 , b) x = (n + x, a, a - 1, b - 1)
          accumulator (n, a, a', b) x = (n, a, a' - 1, b)

euler29 :: Int
euler29 = length . sortedRemDups . sort $ [a^b | a <- [2..100], b <- [2..100]]
    where sortedRemDups [] = []
          sortedRemDups [x] = [x]
          sortedRemDups (x:y:xs) = if x == y then
                                     sortedRemDups (y:xs)
                                   else
                                     x: sortedRemDups (y:xs)

euler30 :: Int
euler30 = sum . filter (\x -> x == sumOfFifthPower x) $ [10..9999999]
    where sumOfFifthPower = sum . map (^ 5) . map digitToInt . show

euler31 :: Int
euler31 = enumerate 0 [200, 100, 50, 20, 10, 5, 2, 1]
    where enumerate n [] = 0
          enumerate 200 _ = 1
          enumerate n _ | n > 200 = 0
          enumerate n [x] = enumerate (n + x) [x]
          enumerate n (x:y:xs) = enumerate (n + x) (x:y:xs) +
                                 enumerate n (y:xs)

euler32 :: Int
euler32 = sum . map fst . filter snd .
          map (\(x, ys) -> (x, any (flip isValid x) ys)) .
          map (second $ properDivisors) . map (join (,)) $ [1..9999]
    where isValid x n = let y = n `div` x in (x * y == n) && isPan x y n
          isPan x y n = (== "123456789") . sort . mconcat . map show $ [x, y, n]

euler33 :: Int
euler33 = denominator . product . map (uncurry (%)) . filter isValid .
          filter (uncurry (<)) . join (liftM2 (,)) $ [10..99]
    where isValid = isValidStr . (show *** show)
          isValidStr (_, ['0', _]) = False
          isValidStr (_, [_, '0']) = False
          -- Trivial cases are handled in the safeguards above
          isValidStr p@([x0, x1], [y0, y1])
            | x1 == y0 = partialFrac x0 y1 == totalFrac p
          isValidStr p@([x0, x1], [y0, y1])
            | x0 == y1 = partialFrac x1 y0 == totalFrac p
          isValidStr _ = False
          totalFrac = uncurry (%) . (read *** read)
          partialFrac = (%) `on` digitToInt

euler34 :: Int
euler34 = sum . filter (\x -> x == digitFactorial x) $ [10..99999999]
    where digitFactorial = sum . map factorial . map digitToInt . show
          factorial = product . curry range 1

euler35 :: Int
euler35 = length . sortedRemDups . sort . concat .
          filter (all (isPrime . read)) . map rotations .
          filter (\x -> head x == head (sort x)) . filter isValid .
          map show $ [2..999999]
    where rotate [] = []
          rotate (x:xs) = xs ++ [x]
          rotations x = ([x] ++) . takeWhile (/= x) . tail . iterate rotate $ x
          isValid [_] = True
          isValid xs = null $ "245680" `intersect` xs
          sortedRemDups [] = []
          sortedRemDups [x] = [x]
          sortedRemDups (x:y:xs) = if x == y then
                                     sortedRemDups (y:xs)
                                   else
                                     x: sortedRemDups (y:xs)

euler36 :: Int
euler36 = sum . filter isValid $ [1..999999]
    where showBin = showIntAtBase 2 ("01" !!)
          isValid x = isPalindromic x && isPalindromic (showBin x "")

euler37 :: Int
euler37 = sum . take 11 . map fst . filter (all isPrime . map read . snd) .
          map (second $ truncs . show) . map (join (,)) $ [10..]
    where truncs = liftM2 (++) (tail . inits) (tail . init . tails)

euler38 :: Int
euler38 = maximum . map read . map concat . map (map show) .
          filter isPan . concat . map enumerate $ [1..99999]
    where enumerate x = drop 2 . inits . map (x *) $ [1..9]
          isPan = (== "123456789") . sort . mconcat . map show

euler39 :: Int
euler39 = fst . maximumBy (compare `on` length . snd) .
          map (second $ allRightTriangles) . map (join (,)) $ [1..1000]
    where allRightTriangles p = [[x, y, z] |
              x <- [1..p `quot` 3], y <- [x..2 * (p `quot` 3)],
              let z = p - x - y, isRight [x, y, z]]
          isRight [x, y, z] = x ^ 2 + y ^ 2 == z ^ 2
          isRight _ = False

euler40 :: Int
euler40 = product . map digitToInt . flip map [(!! (10 ^ x)) | x <- [0..6]] .
          flip ($) . concatMap show $ [0..]

euler41 :: Int
euler41 = head . filter isPrime . sortBy (compare `on` Down) . map read .
          concatMap permutations . tail . inits $ "123456789"

euler42 :: IO Int
euler42 = readFile "words.txt" >>= (return . length .
            filter (`elem'` triangles) . map wordValue . commaSplit)
    where wordValue = sum . map (subtract $ ord 'A' - 1) . map ord
          triangles = map (\n -> n * (n + 1) `div` 2) [1..]
          commaSplit = map (filter (/= '"')) . words .
              map (\x -> if x == ',' then ' ' else x)

euler43 :: Integer
euler43 = sum . map read . filter isValid . permutations $ "0123456789"
    where isValid x = isValidStep (tail x) primes
          primes = [2, 3, 5, 7, 11, 13, 17]
          isValidStep _ [] = True
          isValidStep xs (p:ps) = if (read . take 3 $ xs) `rem` p == 0 then
                                    isValidStep (tail xs) ps
                                  else False

euler44 :: Integer -- This one is REALLY slow
euler44 = d . euler44Node . fromJust $ evalState selectAll initial
    where isValid (x, y) = isPentagonal (y - x) &&
                           isPentagonal (y + x)
          d = uncurry subtract
          selectAll = selectNextForever >>= (return . msum)
          selectNextForever = sequence . repeat . state $ selectNext
          selectNext zs =
            let Euler44 (x, y) = Set.findMin zs
                newEl
                 | y == 1 = Set.fromList [Euler44 (x + 1, y), Euler44 (x, y + 1)]
                 | otherwise = Set.singleton $ Euler44 (x, y + 1)
                zs' = (Set.delete (Euler44 (x, y)) zs) `Set.union` newEl
                r | isValid $ euler44Node (x, y) = Just (x, y)
                  | otherwise = Nothing
              in (r, zs')
          initial = Set.singleton $ Euler44 (1, 1)
          traceLine x n = trace $ show (d (euler44Node x)) ++ " " ++ show x ++ " " ++ show n

euler45 :: Integer
euler45 = head . dropWhile (<= 40755) . filter triangleWorks $ triangulars
    where triangulars = map triangular [1..]
          triangleWorks = (&&) <$> isPentagonal <*> isHexagonal

euler46 :: Integer
euler46 = head . filter isValid $ oddComposites
    where oddComposites = filter odd . filter (not . isPrime) $ [2..]
          primes = filter isPrime [2..]
          doubleSquares = map (\x -> 2 * x * x) [1..]
          takeTo n = takeWhile (< n)
          isValid n = not $ n `elem` (liftA2 (+) (takeTo n primes) (takeTo n doubleSquares))

euler47 :: Integer
euler47 = firstOf . head . filter isValid $ sequential
    where lengthIsFour [_,_,_,_] = True
          lengthIsFour _         = False
          sequential = zip4 [2..] [3..] [4..] [5..]
          isValid (x, y, z, t) = let x' = nub $ primeFactors x
                                     y' = nub $ primeFactors y
                                     z' = nub $ primeFactors z
                                     t' = nub $ primeFactors t
                                   in lengthIsFour x' && lengthIsFour y' &&
                                      lengthIsFour z' && lengthIsFour t'
          firstOf (x, _, _, _) = x

euler48 :: Integer
euler48 = read . reverse . take 10 . reverse . show . sum .
            map (join (^)) $ [1..1000]

euler49 :: Integer
euler49 = read . concat . map show . head . filter ((/= 1487) . head) .
            filter isValid $ allPossible
    where isPermutable [] = True
          isPermutable (x:xs) = all (`elem` (permutations $ show x))
                                    $ map show xs
          isValid xs = all isPrime xs
          permutations' = map read . permutations . show
          allPossible = [[x, y, z] | x <- [1000..9999],
            y <- permutations' x, z <- permutations' x, x < y, y < z, z - y == y - x]

euler50 :: Integer
euler50 = fromJust $ evalState attemptSeqState initial
    where primes = sieveOfEratosthenes 1000000
          primeSet = Set.fromList primes
          primeSeq = Seq.fromList primes
          nextPrime n = Set.lookupGT n primeSet
          initial = (sum primes, primeSeq)
          isPrime' n = Set.member n primeSet
          attemptSeqState = sequence (repeat attemptSeq) >>= return . msum
          attemptSeq = state $ \(n, s) ->
                        case n of
                            _ | n > 1000000 ->
                                let s' = Seq.fromList $
                                            take (Seq.length s - 1) primes in
                                    (Nothing, (Fold.sum s', s'))
                              | isPrime' n ->
                                (Just n, (n, s))
                              | otherwise ->
                                let n' :< s' = viewl s
                                    _ :> lst = viewr s
                                    next = case nextPrime lst of
                                            Just x -> x
                                            Nothing -> error (show n) in
                                    (Nothing, (n - n' + next, s' |> next))

euler51 :: Int
euler51 = read . fst . head . dropWhile ((< 8) . snd) .
          map (second $ maximum . (0:)) .
          map (second $ map length) .
          map (second $ map (filter (isPrime . read))) .
          map (second $ map (filter ((/= '0') . head))) . -- Handle leading 0's
          map (second $ filter (not . allEqual)) .
          map (id &&& familiesUp) .
          map show . filter isPrime $ [2..]
    where replace x y z | z == x = y
          replace _ _ z = z
          replaceNth n x xs = take n xs ++ [x] ++ drop (n + 1) xs
          replaceAll x y = map $ replace x y
          enumeratePossible x = map (\y -> replaceAll '*' y x) $ ['0'..'9']
          tryReplace = mapM (\x -> [x, '*'])
          tryReplaceZO = mapM (\x -> if x `elem` "01" then [x, '*'] else [x])
          families = map enumeratePossible . tryReplace
          familiesUp = map enumeratePossible . tryReplaceZO
          allEqual [] = True
          allEqual xs = isJust . foldl (\x y -> guard (x == Just y) >> x)
                                (Just $ head xs) $ xs

euler52 :: Integer
euler52 = fst . head . filter (\(x, y) -> all (x `isPermute`) y) .
          map (id &&& (\x -> map (* x) [2..6])) $ [1..]
    where isPermute = (\x y -> x `elem` permutations y) `on` show

euler53 :: Int
euler53 = length [r1 | n <- [1..100], r <- [1..n],
                       let r1 = n `c` r, r1 > 1000000]
    where factorial n = product [1..n]
          n `c` r = factorial n `quot` (factorial r * factorial (n - r))

euler54 :: IO Int
euler54 = readFile "poker.txt" >>=
            (return . length . filter (== GT) . map (uncurry compareHands) .
                map (cardsToHand *** cardsToHand) .
                map (take 5 &&& drop 5) .
                map (map read) . map words . lines)

euler55 :: Int
euler55 = length . filter isLychrel $ [1..9999]
    where isLychrel x
           | x >= 10000 = error "Invalid algorithm for x >= 10000"
           | otherwise = null . filter isPalindromic . take 50 . tail .
                            iterate reverseSum $ x
          reverseSum x = x + read (reverse $ show x)

euler56 :: Int
euler56 = maximum . map digitSum $ [a ^ b | a <- [1..99], b <- [1..99]]
    where digitSum = sum . map digitToInt . show

euler57 :: Int
euler57 = length . filter isValid . map expansion $ [1..1000]
    where expansionI n 0 = 1 + 1 / (expansionI (n - 1) 1)
          expansionI 0 _ = 2
          expansionI n m = 2 + 1 / (expansionI (n - 1) (m + 1))
          expansion :: Integral a => Int -> Ratio a
          expansion n = expansionI n 0
          isValid n =
            length (show $ numerator n) > length (show $ denominator n)

euler58 :: Integer
euler58 = fst . head .
          filter ((< 1 % 10) . uncurry (%) . snd) . tail . zip [1,3..] .
          scanl scanF (0, 1) . parts . tail $ getDiagonals
    where primes = sieveOfEratosthenes 1000000
          primeSet = Set.fromList primes
          isPrime' x
           | x < 1000000 = Set.member x primeSet
           | otherwise = isPrime x
          getDiagonals' x m 0 = x : getDiagonals' (x + m + 2) (m + 2) 3
          getDiagonals' x m c = x : getDiagonals' (x + m) m (c - 1)
          getDiagonals = getDiagonals' 1 0 0
          parts (x:y:z:t:xs) = [x,y,z,t] : parts xs
          parts xs = [xs]
          scanF (p, t) y = (p + (length $ filter isPrime' y), t + length y)

euler59 :: IO Int
euler59 = readFile "cipher.txt" >>=
            (return . sum . map ord . head .
            filter (null . filter (not . isValidChar)) .
            map (map chr) . tryAll . map read . commaSplit)
    where decrypt = zipWith xor
          commaSplit = words . map (\x -> if x == ',' then ' ' else x)
          possibleKeys = [[x, y, z] | let ll = [97..122],
                            x <- ll, y <- ll, z <- ll]
          tryAll xs = map (decrypt xs . cycle) possibleKeys
          isValidChar x = any ($ x) [isAlphaNum, isSeparator, isPunctuation]

euler60 :: Int
euler60 = fromJust $ evalState stateFor starting
    where concatN x y = read $ show x ++ show y
          concatP x y = isPrimeQuick $ (concatN x y :: Int)
          concatPC x y = concatP x y && concatP y x
          concatAll [] = True
          concatAll [_] = True
          concatAll (x:xs) = all (concatPC x) xs && concatAll xs
          primesMagn n = sieveOfEratosthenes $ 2 ^ n
          primesFromTo n = dropWhile (< 2 ^ (n - 1)) $ primesMagn n
          primes = concatMap primesFromTo [2..]
          primes' = 3 : drop 3 primes
          starting = Set.singleton $ Euler60 [drop n primes' | n <- [0..3]]
          matching acc (x:_) = (x, acc /= x)
          isDistinct xss =
                    case mapAccumL matching (head $ head xss) (tail xss) of
                        (_, ys) -> all not ys
          possibleNext [] = []
          possibleNext [xs] = [[tail xs]]
          possibleNext (xs:ys:xss)
           | head (tail xs) == head ys = map (xs:) $ possibleNext (ys:xss)
           | otherwise = (tail xs : ys : xss) :
                            (map (xs:) $ possibleNext (ys:xss))
          insertInto xs s = foldl (flip Set.insert) s xs
          doNext sxss = let Euler60 xss = Set.findMin sxss
                            xs = map head xss
                            sxss' = Set.deleteMin sxss
                            new = map Euler60 $ possibleNext xss
                            done = concatAll xs in
                          (guard done >> (Just $ sum xs),
                            traceShow (sum xs) insertInto new sxss')
          stateFor :: State (Set.Set Euler60) (Maybe Int)
          stateFor = (sequence . repeat $ state doNext) >>= (return . msum)

--euler61 :: Int
euler61 = allValues
    where nFigure :: Int -> [Int]
          nFigure n = tail $ scanl (+) 0 [1, n + 1 ..]
          allValues = let l = [10..99] in
                        [[x * 100 + y, y * 100 + z, z * 100 + x] |
                            x <- l, y <- l, z <- l]
          conditions = map (flip elem' . nFigure) [1..3]
          validate = all id . liftA2 ($) conditions

euler62 :: Integer
euler62 = head . filter solution $ [1..]
    where epsilon = 0.1 ^ 5
          isInt n = (abs (n - fromInteger (round n))) <= epsilon
          isCube x = isInt $ fromInteger x ** (1/3)
          wanted = 3
          readInt x = read x :: Integer
          solution x = let perm = traceShow x $ permutations $ show x
                       in isCube x &&
                              ((== wanted) . length . filter isCube .
                                           map readInt $ perm)

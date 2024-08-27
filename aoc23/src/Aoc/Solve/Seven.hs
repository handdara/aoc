{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Solve.Seven
  ( solveDay7,
  )
where

import qualified Data.List as L
import qualified Data.Map as M

type Input = [String]

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Bounded, Enum)

data CardJ
  = JokerJ
  | TwoJ
  | ThreeJ
  | FourJ
  | FiveJ
  | SixJ
  | SevenJ
  | EightJ
  | NineJ
  | TenJ
  | QueenJ
  | KingJ
  | AceJ
  deriving (Show, Eq, Ord, Bounded, Enum)

data HandRank
  = HighCard
  | OnePair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind
  deriving (Show, Eq, Ord)

newtype Hand = Hand {getCards :: [Card]}
  deriving (Eq)

newtype HandJ = HandJ {getCardJs :: [CardJ]}
  deriving (Eq)

type Bid = Int

data HandBid = HandBid Hand Bid deriving (Show, Eq, Ord)

data HandBidJ = HandBidJ HandJ Bid deriving (Show, Eq, Ord)

type Counts c = M.Map c Int

insertCardCounts :: Hand -> Counts Card -> Counts Card
insertCardCounts (Hand []) cM = cM
insertCardCounts (Hand (c : cs)) cM = insertCardCounts (Hand cs) cM'new
  where
    cM'new = M.insertWith (+) c 1 cM

insertCardCountsJ :: HandJ -> Counts CardJ -> Counts CardJ
insertCardCountsJ (HandJ []) cM = cM
insertCardCountsJ (HandJ (c : cs)) cM = insertCardCountsJ (HandJ cs) cM'new
  where
    cM'new = M.insertWith (+) c 1 cM

countsToRank :: [Int] -> HandRank
countsToRank [5] = FiveKind
countsToRank [1, 4] = FourKind
countsToRank [2, 3] = FullHouse
countsToRank [1, 1, 3] = ThreeKind
countsToRank [1, 2, 2] = TwoPair
countsToRank [1, 1, 1, 2] = OnePair
countsToRank [1, 1, 1, 1, 1] = HighCard
countsToRank cs = error $ "countsToRank: unexpected count list: " <> show cs

handToRank :: Hand -> HandRank
handToRank h = countsToRank counts
  where
    counts = L.sort . M.elems . insertCardCounts h $ M.empty

handJToRank :: HandJ -> HandRank
handJToRank h = countsToRank counts
  where
    counts = L.sort . M.elems . insertCardCountsJ h $ M.empty

instance Ord Hand where
  compare l@(Hand cards'l) r@(Hand cards'r) =
    if rank'l == rank'r
      then compare cards'l cards'r
      else compare rank'l rank'r
    where
      rank'l = handToRank l
      rank'r = handToRank r

numJokers :: [CardJ] -> Int
numJokers = foldr (\c acc -> if c == JokerJ then acc + 1 else acc) 0

getMaxRank :: HandJ -> HandRank
getMaxRank h@(HandJ cs) =
  if all (== head cs) cs || numJokers cs == 4
    then FiveKind
    else maximum . map handJToRank . expandHand $ h

instance Ord HandJ where
  compare l@(HandJ cards'l) r@(HandJ cards'r) =
    if maxRank'l == maxRank'r
      then compare cards'l cards'r
      else compare maxRank'l maxRank'r
    where
      maxRank'l = getMaxRank l
      maxRank'r = getMaxRank r

instance Show Hand where
  show = show . getCards

instance Show HandJ where
  show = show . getCardJs

expandHand :: HandJ -> [HandJ]
expandHand = map HandJ . foldl expand [[]] . getCardJs
  where
    expand expandedAlready nextCard = do
      subhand <- expandedAlready
      wild <-
        if nextCard == JokerJ
          then [minBound .. maxBound]
          else [nextCard]
      return (wild : subhand)

-- * Parsing

charToCard :: Char -> Card
charToCard '2' = Two
charToCard '3' = Three
charToCard '4' = Four
charToCard '5' = Five
charToCard '6' = Six
charToCard '7' = Seven
charToCard '8' = Eight
charToCard '9' = Nine
charToCard 'T' = Ten
charToCard 'J' = Jack
charToCard 'Q' = Queen
charToCard 'K' = King
charToCard 'A' = Ace
charToCard c = error $ "charToCard: unexpected input char: " <> [c]

charToCardJ :: Char -> CardJ
charToCardJ 'J' = JokerJ
charToCardJ '2' = TwoJ
charToCardJ '3' = ThreeJ
charToCardJ '4' = FourJ
charToCardJ '5' = FiveJ
charToCardJ '6' = SixJ
charToCardJ '7' = SevenJ
charToCardJ '8' = EightJ
charToCardJ '9' = NineJ
charToCardJ 'T' = TenJ
charToCardJ 'Q' = QueenJ
charToCardJ 'K' = KingJ
charToCardJ 'A' = AceJ
charToCardJ c = error $ "charToCard: unexpected input char: " <> [c]

strToHand :: String -> Hand
strToHand h = Hand $ map charToCard h

strToHandJ :: String -> HandJ
strToHandJ h = HandJ $ map charToCardJ h

lineToHandBid :: String -> HandBid
lineToHandBid l = HandBid (strToHand h) (read b)
  where
    [h, b] = words l

lineToHandBidJ :: String -> HandBidJ
lineToHandBidJ l = HandBidJ (strToHandJ h) (read b)
  where
    [h, b] = words l

prepareInput'part1 :: Input -> [HandBid]
prepareInput'part1 = map lineToHandBid

prepareInput'part2 :: Input -> [HandBidJ]
prepareInput'part2 = map lineToHandBidJ

-- * Actual soln

getBid :: HandBid -> Bid
getBid (HandBid _ b) = b

getBidJ :: HandBidJ -> Bid
getBidJ (HandBidJ _ b) = b

bidListToWinnings :: [Bid] -> Int
bidListToWinnings = sum . zipWith (*) [1 ..]

solutionPart1 :: Input -> Int
solutionPart1 = bidListToWinnings . map getBid . L.sort . prepareInput'part1

solutionPart2 :: Input -> Int
solutionPart2 = bidListToWinnings . map getBidJ . L.sort . prepareInput'part2

solveDay7 :: FilePath -> IO ()
solveDay7 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)

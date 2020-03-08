module Dobble (
    Deck,
    deck,
    isDobbleSymbolNumber,
    projectivePlane,
    cards,
    Card,
    cardSymbols,
) where

import Data.Numbers.Primes
import Data.List


newtype Card s = Card {
    cardSymbols :: [s]
}

deriving instance (Eq s) => Eq (Card s)
deriving instance (Ord s) => Ord (Card s)
deriving instance (Show s) => Show (Card s)

newtype Deck s = Deck {
    cards :: [Card s]
}

deriving instance (Show s) => Show (Deck s)

-- | Dobble symbol number is each number equal to @p*p+p+1@ for sime prime @p@, examples 3, 7, 13, 31, 57, ...
isDobbleSymbolNumber :: Int -> Bool
isDobbleSymbolNumber n = let (Just p) = find (\p -> p * p + p + 1 >= n) (1:primes) in p * p + p + 1 == n

-- | For @symbols@ of size that is Bobble symbol number returns a proper Dobble deck, otherwise returns not proper Dobble deck
-- | for  3 symbols returns deck of 3 cards with 2 symbols on a card
-- | for  7 symbols returns deck of 7 cards with 3 symbols on a card
-- | for 13 symbols returns deck of 13 cards with 4 symbols on a card
-- | for 31 symbols returns deck of 31 cards with 6 symbols on a card
-- | for 57 symbols returns deck of 57 cards with 8 symbols on a card
deck :: [s] -> Deck s
deck symbols = let (Just prime) = find (\p -> p * p + p + 1 >= length symbols) (1:primes) in Deck $ Card . fmap (symbols !!) . filter (length symbols >) <$> projectivePlane prime

-- | For prime number @p@ returns @p2+p+1@ lines of @p2+p+1@ points, with @p+1@ points on each line
-- | E.g. 
-- | for p = 1 returns 3 lines with 3 points, with 2 points of each line
-- | for p = 2 returns 7 lines with 7 points, with 3 points of each line
-- | for p = 3 returns 13 lines with 13 points, with 4 points of each line
-- | for p = 5 returns 31 lines with 31 points, with 6 points of each line
-- | for p = 7 returns 57 lines with 57 points, with 8 points of each line
projectivePlane :: Int -> [[Int]]
projectivePlane p = [[((i * k + j) `mod` p) * p + k | k <- [0..(p - 1)]] <> [p * p + i] | i <- [0..(p - 1)], j <- [0..(p - 1)]]
    <> [[j * p + i | j <- [0..(p - 1)]] <> [p * p + p] | i <- [0..(p - 1)]] 
    <> [[p * p + i | i <- [0..p]]]

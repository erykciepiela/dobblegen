import Test.QuickCheck
import Dobble
import Data.List
import Data.Numbers.Primes
import Data.Maybe

main = do
  quickCheck projectivePlaneLinesIntersectInOnePoint
  quickCheck projectivePlaneLinesCoverProperEqualNumberOfPoints
  quickCheck projectivePlaneHasProperNumberOfLines
  quickCheck projectivePlaneLineIntersectionsCoverAllPoints

  quickCheck dobbleDeckCardsPairHaveOneCommonSymbol
  quickCheck dobbleDeckCardsPairCommonSymbolsCoverAllSymbols
  quickCheck dobbleDeckCardsNumberEqualToNumberOfSymbols

projectivePlaneLinesIntersectInOnePoint n = isPrime n && n <= 11 ==> let lines = projectivePlane n in all (\(line1, line2) -> length (line1 `intersect` line2) == 1) [(line1, line2) | line1 <- lines, line2 <- lines, line1 < line2]
projectivePlaneLineIntersectionsCoverAllPoints n = isPrime n && n <= 11 ==> let lines = projectivePlane n in (length $ nub $ (\(line1, line2) -> line1 `intersect` line2) <$> [(line1, line2) | line1 <- lines, line2 <- lines, line1 < line2]) == n * n + n + 1
projectivePlaneLinesCoverProperEqualNumberOfPoints n = isPrime n && n <= 11 ==> all ((== n + 1) . length) (projectivePlane n)
projectivePlaneHasProperNumberOfLines n = n >= 2 && n <= 11 ==> ((== n * n + n + 1) . length) (projectivePlane n)
dobbleDeckCardsPairHaveOneCommonSymbol numberOfSymbols = numberOfSymbols >=3 ==> isDobbleSymbolNumber numberOfSymbols == let cs = cards (deck [1..(numberOfSymbols :: Int)]) in all (\(card1, card2) -> length (cardSymbols card1 `intersect` cardSymbols card2) == 1) [(card1, card2) | card1 <- cs, card2 <- cs, card1 < card2]
dobbleDeckCardsPairCommonSymbolsCoverAllSymbols numberOfSymbols = numberOfSymbols >=3 ==> not (isDobbleSymbolNumber numberOfSymbols) || let cs = cards (deck [1..(numberOfSymbols :: Int)]) in length (nub $ mconcat $ (\(line1, line2) -> cardSymbols line1 `intersect` cardSymbols line2) <$> [(line1, line2) | line1 <- cs, line2 <- cs, line1 < line2]) == numberOfSymbols
dobbleDeckCardsNumberEqualToNumberOfSymbols numberOfSymbols = numberOfSymbols >=3 ==> isDobbleSymbolNumber numberOfSymbols == let cs = cards (deck [1..(numberOfSymbols :: Int)]) in length cs == numberOfSymbols

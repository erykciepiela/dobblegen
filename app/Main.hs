module Main where

import Dobble
import Graphics.Svg
import System.Directory

placeDocument :: Double -> Double -> Double -> Double -> Double -> Double -> Document -> Tree
placeDocument w h radius spin scale angle doc = GroupTree (Group defaultSvg{_transform = Just [Rotate angle Nothing, Translate radius 0, Scale scale Nothing, Rotate spin Nothing, Translate (-w/2) (-h/2)]} (_elements doc) Nothing defaultSvg)

placeDocuments :: Double -> Double -> Double -> [Document] -> Document
placeDocuments w h radius docs = let
    angle1 = 360 / fromIntegral (length docs)
    spin1 = 90 / fromIntegral (length docs)
    placedDocs = zipWith (\i doc -> placeDocument w h radius (i * spin1) 1 (i * angle1) doc) [1..] docs
    in (head docs) { _width = Just (Px 500), _height = Just (Px 500), _viewBox = Just (- (radius + w/2), - (radius + w/2), 2 * (radius + w/2), 2 * (radius + w/2)), _elements = placedDocs }

main :: IO ()
main = do
    symbolFiles <- fmap (\fp -> "symbols/" <> fp) <$> listDirectory "symbols"
    symbolDocs <- traverse (\fp -> do Just doc <- loadSvgFile fp; return doc) symbolFiles
    let d = deck (take 13 symbolDocs)
    let cardDocs = placeDocuments 3000 3000 3000 . cardSymbols <$> cards d
    traverse (\(i, cardDoc) -> saveXmlFile ("deck/"<> show i <> ".svg") cardDoc) (zip [1..] cardDocs)
    return ()
    -- docs <- traverse (\fp -> do Just doc <- loadSvgFile fp; return doc) ((\n -> "graphics/" <> n <> ".svg") <$> ["tongue", "eye", "cheek", "foot", "knee", "nail"])
    -- saveXmlFile "deck/output.svg" $ placeDocuments 3000 3000 3000 docs

module Main where

import Dobble
import Graphics.Svg
import System.Directory
import Data.Map.Strict

placeDocument :: Double -> Double -> Double -> Double -> Double -> Double -> String -> Tree
placeDocument w h radius spin scale angle name = UseTree (Use (Px 0, Px 0) name Nothing Nothing defaultSvg{_transform = Just [Rotate angle Nothing, Translate radius 0, Scale scale Nothing, Rotate spin Nothing, Translate (-w/2) (-h/2)]}) Nothing

placeDocuments :: Double -> Double -> Double -> [Document] -> Document
placeDocuments w h radius docs = let
    size = length docs
    angle1 = 360 / fromIntegral size
    spin1 = 90 / fromIntegral size
    symbols = (\(i, doc) -> (show i, ElementGeometry $ SymbolTree $ Symbol $ Group defaultSvg (_elements doc) Nothing defaultSvg)) <$> zip [0..] docs
    placedDocs = (\i -> placeDocument w h radius (fromIntegral i * spin1) 1 (fromIntegral i * angle1) (show i)) <$> [0.. (size - 1)]
    in (head docs) { _definitions = fromList symbols, _width = Just (Px 500), _height = Just (Px 500), _viewBox = Just (- (radius + w/2), - (radius + w/2), 2 * (radius + w/2), 2 * (radius + w/2)), _elements = placedDocs }

main :: IO ()
main = do
    symbolFiles <- fmap (\fp -> "symbols/" <> fp) <$> listDirectory "symbols"
    symbolDocs <- traverse (\fp -> do Just doc <- loadSvgFile fp; return doc) symbolFiles
    let d = deck (Prelude.take 13 symbolDocs)
    let cardDocs = placeDocuments 3000 3000 3000 . cardSymbols <$> cards d
    traverse (\(i, cardDoc) -> saveXmlFile ("deck/"<> show i <> ".svg") cardDoc) (zip [1..] cardDocs)
    return ()
    -- docs <- traverse (\fp -> do Just doc <- loadSvgFile fp; return doc) ((\n -> "graphics/" <> n <> ".svg") <$> ["tongue", "eye", "cheek", "foot", "knee", "nail"])
    -- saveXmlFile "deck/output.svg" $ placeDocuments 3000 3000 3000 docs

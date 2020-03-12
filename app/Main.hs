module Main where

import Dobble
import Graphics.Svg
import System.Directory
import Data.Map.Strict
import Data.Monoid
import Codec.Picture.Types

placeDocument :: Double -> Double -> Double -> Double -> Double -> Double -> String -> Tree
placeDocument w h radius spin scale angle name = UseTree (Use (Px 0, Px 0) name Nothing Nothing defaultSvg{_transform = Just [Rotate angle Nothing, Translate radius 0, Scale scale Nothing, Rotate spin Nothing, Translate (-w/2) (-h/2)]}) Nothing

placeDocuments :: Double -> Double -> Double -> Double -> Double -> [Int] -> Tree
placeDocuments w h radius x y ids = let
    size = length ids
    angle1 = 360 / fromIntegral size
    spin1 = 90 / fromIntegral size
    placedDocs = zipWith (\i id -> placeDocument w h radius (fromIntegral i * spin1 + 10) 1 (fromIntegral i * angle1 + 20) (show id)) [0..] ids
    rect = RectangleTree $ Rectangle defaultSvg{_fillColor = Last (Just FillNone), _strokeColor= Last (Just (ColorRef (PixelRGBA8 255 0 0 0))), _strokeWidth = Last (Just (Px 2))} (Px (- (radius + w/2)), Px (- (radius + w/2))) (Px (2 * radius + w)) (Px (2 * radius + w)) (Px 0, Px 0)
    in GroupTree (Group defaultSvg{_transform = Just [Translate (2 * (radius + w/2) * x) (2 * (radius + w/2) * y)]} (rect:placedDocs) Nothing defaultSvg)

foo :: Double -> Double -> Double -> [Document] -> Document
foo w h radius docs = let
    size = length docs
    symbols = (\(i, doc) -> (show i, ElementGeometry $ SymbolTree $ Symbol $ Group defaultSvg (_elements doc) Nothing defaultSvg)) <$> zip [0..] docs
    d = deck [0.. (size - 1)]
    columns = (3 :: Int)
    pxpercard = 300
    cardGroups = zipWith (\i card -> placeDocuments w h radius (fromIntegral (i `mod` columns)) (fromIntegral (i `div` columns)) (cardSymbols card)) [0..] (cards d)
    in (head docs) { _definitions = fromList symbols, _width = Just (Px (pxpercard * fromIntegral columns)), _height = Just (Px (fromIntegral (size `div'` columns) * pxpercard)), _viewBox = Just (- (radius + w/2), - (radius + w/2), fromIntegral columns * 2 * (radius + w/2), fromIntegral (size `div'` columns) * 2 * (radius + w/2)), _elements = cardGroups }

div' :: Int -> Int -> Int
div' a b = a `div` b + (if a `mod` b > 0 then 1 else 0)

main :: IO ()
main = do
    symbolFiles <- fmap (\fp -> "symbols/" <> fp) <$> listDirectory "symbols"
    symbolDocs <- traverse (\fp -> do Just doc <- loadSvgFile fp; return doc) symbolFiles
    saveXmlFile "deck/deck.svg" $ foo 3000 3000 3000 (Prelude.take 13 (cycle symbolDocs))

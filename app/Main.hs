module Main where

import Dobble
import Graphics.Svg
import System.Directory
import Data.Map.Strict
import Data.Monoid
import Codec.Picture.Types
import Data.List.Split
import Data.Foldable
import Data.Functor
import Codec.Picture( writePng )
import Graphics.Svg( loadSvgFile )
import Graphics.Rasterific.Svg( loadCreateFontCache
                              , renderSvgDocument
                              )

renderPng :: FilePath -> Document -> IO ()
renderPng pngfilename doc = do
    cache <- loadCreateFontCache "fonty-texture-cache"
    (finalImage, _) <- renderSvgDocument cache Nothing 300 doc
    writePng pngfilename finalImage

placeDocument :: Double -> Double -> Double -> Double -> Double -> Double -> String -> Tree
placeDocument w h radius spin scale angle name = UseTree (Use (Px 0, Px 0) name Nothing Nothing defaultSvg{_transform = Just [Rotate angle Nothing, Translate radius 0, Scale scale Nothing, Rotate spin Nothing, Translate (-w/2) (-h/2)]}) Nothing

placeDocuments :: Double -> Double -> Double -> Double -> Double -> [Int] -> Tree
placeDocuments w h radius x y ids = let
    size = length ids
    angle1 = 360 / fromIntegral size
    spin1 = 90 / fromIntegral size
    placedDocs = zipWith (\i id -> placeDocument w h radius (fromIntegral i * spin1 + 10) 1 (fromIntegral i * angle1 + 20) (show id)) [0..] ids
    borderStroke = Last (Just (ColorRef (PixelRGBA8 100 100 100 100)))
    rect = RectangleTree $ Rectangle defaultSvg{_fillColor = Last (Just FillNone), _strokeColor= borderStroke, _strokeWidth = Last (Just (Px 10))} (Px (- (radius + w/2)), Px (- (radius + w/2))) (Px (2 * radius + w)) (Px (2 * radius + w)) (Px 0, Px 0)
    in GroupTree (Group defaultSvg{_transform = Just [Translate (2 * (radius + w/2) * x + (radius + w/2)) (2 * (radius + w/2) * y + (radius + w/2))]} (rect:placedDocs) Nothing defaultSvg)

foo :: Int -> Int -> Double -> Double -> [Document] -> [Document]
foo columns rows w h docs = let
    radius = w
    symbols = (\(i, doc) -> (show i, ElementGeometry $ SymbolTree $ Symbol $ Group defaultSvg (_elements doc) Nothing defaultSvg)) <$> zip [0..] docs
    defs = fromList symbols
    pxpercard = 1000
    borderStroke = Last (Just (ColorRef (PixelRGBA8 0 0 0 0)))
    caption = TextTree Nothing (Text TextAdjustSpacing defaultSvg{_spanDrawAttributes=defaultSvg{_strokeColor=borderStroke}, _spanInfo=defaultSvg{_textInfoX=[Px 0], _textInfoY=[Px 0], _textInfoDX = [Px 200], _textInfoDY = [Px 100]}, _spanContent=[SpanText ("Sheet 1")]})
    in chunksOf (columns * rows) (cards (deck [0.. (length docs - 1)])) <&> (\cards -> let
        cardGroups = zipWith (\i card -> placeDocuments w h radius (fromIntegral (i `mod` columns)) (fromIntegral (i `div` columns)) (cardSymbols card)) [0..] cards
        in Document {_documentLocation = ".", _description="sheet", _styleRules = [], _definitions = defs, _width = Just (Px (pxpercard * fromIntegral columns)), _height = Just (Px (fromIntegral rows * pxpercard)), _viewBox = Just (0, 0, fromIntegral columns * 2 * (radius + w/2), fromIntegral rows * 2 * (radius + w/2)), _elements = caption:cardGroups })

main :: IO ()
main = do
    symbolFiles <- fmap (\fp -> "symbols/" <> fp) <$> listDirectory "symbols"
    symbolDocs <- traverse (\fp -> do Just doc <- loadSvgFile fp; return doc) symbolFiles
    traverse_ (\(i, doc) -> renderPng ("deck/sheet" <> show i <> ".png") doc) (zip [1..] (foo 3 4 3000 3000 (Prelude.take 31 (cycle symbolDocs))))

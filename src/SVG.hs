module SVG where

import Dobble
import Graphics.Svg
import System.Directory
import Data.Map.Strict
import Data.Monoid
import Codec.Picture.Types
import Data.List.Split
import Data.Foldable
import Data.Functor
import Codec.Picture(writePng, encodePng)
import Graphics.Rasterific.Svg(loadCreateFontCache, renderSvgDocument)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- Puts SVG documents of symbols onto SVG documents of pages of cards to print
data SVGDeckRenderer = SVGDeckRenderer {
    columnsPerPage :: Int,
    rowsPerPage :: Int,
    cardPixelWidth :: Double,
    symbolCardWidth :: Double,
    symbolCardHeight :: Double,
    flipDoc :: Document
}

instance DeckRenderer SVGDeckRenderer Document [Document] where
    renderDeck (SVGDeckRenderer columns rows pxpercard w h flipDoc) (Deck symbolDocs d) = let
        radius = sqrt (w ^ 2 + h ^ 2)
        face1Defs = fromList $ (\(i, doc) -> (show i, ElementGeometry $ SymbolTree $ Symbol $ Group defaultSvg (_elements doc) Nothing defaultSvg)) <$> zip [0..] symbolDocs
        face2Defs = singleton "0" $ ElementGeometry $ SymbolTree $ Symbol $ Group defaultSvg (_elements flipDoc) Nothing defaultSvg
        borderStroke = Last (Just (ColorRef (PixelRGBA8 0 0 0 0)))
        cardWidth = 3 * radius
        -- caption = TextTree Nothing (Text TextAdjustSpacing defaultSvg{_spanDrawAttributes=defaultSvg{_strokeColor=borderStroke}, _spanInfo=defaultSvg{_textInfoX=[Px 0], _textInfoY=[Px 0], _textInfoDX = [Px 200], _textInfoDY = [Px 100]}, _spanContent=[SpanText ("Sheet 1")]})
        in mconcat $ chunksOf (columns * rows) d <&> sheetTree cardWidth face1Defs face2Defs radius
            where
            sheetTree cardWidth face1Defs face2Defs radius cards = let
                cardGroups = zipWith (\i card -> let
                    x = fromIntegral (i `mod` columns)
                    y = fromIntegral (i `div` columns)
                    (f1, f2) = cardTree w h radius card
                    face1Tree = GroupTree (Group defaultSvg{_transform = Just [Translate (cardWidth * x + (cardWidth/2)) (cardWidth * y + (cardWidth/2))]} f1 Nothing defaultSvg)
                    face2Tree = GroupTree (Group defaultSvg{_transform = Just [Translate (cardWidth * (fromIntegral columns - 1 - x) + (cardWidth/2)) (cardWidth * y + (cardWidth/2))]} f2 Nothing defaultSvg)
                    in (face1Tree, face2Tree)) [0..] cards
                frontPage = Document {_documentLocation = ".", _description="", _styleRules = [], _definitions = face1Defs, _width = Just (Px (pxpercard * fromIntegral columns)), _height = Just (Px (fromIntegral rows * pxpercard)), _viewBox = Just (0, 0, fromIntegral columns * cardWidth, fromIntegral rows * cardWidth), _elements = fst <$> cardGroups }
                backPage = Document {_documentLocation = ".", _description="", _styleRules = [], _definitions = face2Defs, _width = Just (Px (pxpercard * fromIntegral columns)), _height = Just (Px (fromIntegral rows * pxpercard)), _viewBox = Just (0, 0, fromIntegral columns * cardWidth, fromIntegral rows * cardWidth), _elements = snd <$> cardGroups }
                in [frontPage, backPage]
                    where
                    cardTree :: Double -> Double -> Double -> [Int] -> ([Tree], [Tree])
                    cardTree w h radius ids = let
                        size = length ids
                        angle1 = 360 / fromIntegral size
                        spin1 = 90 / fromIntegral size
                        placedDocs = zipWith (\i id -> symbolTree w h radius (fromIntegral i * spin1 + 10) 1 (fromIntegral i * angle1 + 20) (show id)) [0..] ids
                        borderStroke = Last (Just (ColorRef (PixelRGBA8 100 100 100 255)))
                        cutStroke = Last (Just (ColorRef (PixelRGBA8 200 200 200 255)))
                        cardWidth = 3 * radius
                        cutRect = RectangleTree $ Rectangle defaultSvg{_fillColor = Last (Just FillNone), _strokeDashArray=Last (Just [Px 400]), _strokeColor=cutStroke, _strokeWidth = Last (Just (Px 10))} (Px (- (cardWidth/2)), Px (- (cardWidth/2))) (Px cardWidth) (Px cardWidth) (Px 0, Px 0)
                        border = RectangleTree $ Rectangle defaultSvg{_strokeLineCap=Last (Just CapRound), _fillColor = Last (Just FillNone), _strokeOpacity=Just 1, _strokeColor= borderStroke, _strokeWidth = Last (Just (Px 1000))} (Px (- (cardWidth/2)), Px (- (cardWidth/2))) (Px cardWidth) (Px cardWidth) (Px 1000, Px 1000)
                        logo = UseTree (Use (Px 0, Px 0) "0" Nothing Nothing defaultSvg{_transform = Just [Scale 6 Nothing, Rotate 30 Nothing, Translate (-w/2) (-h/2)]}) Nothing
                        in (placedDocs, [border, logo, cutRect])
                            where
                            symbolTree :: Double -> Double -> Double -> Double -> Double -> Double -> String -> Tree
                            symbolTree w h radius spin scale angle name = UseTree (Use (Px 0, Px 0) name Nothing Nothing defaultSvg{_transform = Just [Rotate angle Nothing, Translate radius 0, Scale scale Nothing, Rotate spin Nothing, Translate (-w/2) (-h/2)]}) Nothing

runCLI :: IO ()
runCLI = do
    symbolSVGs <- fmap ("symbols/" <>) <$> listDirectory "symbols" >>= traverse (\fp -> do Just doc <- loadSvgFile fp; return doc)
    Just flipSideSVG <- loadSvgFile "flip/flip.svg"
    traverse_ (\(i, pageSVG) -> renderPng ("deck/" <> show i <> ".png") pageSVG) (zip [1..] (generateSVGDeck symbolSVGs flipSideSVG))
        where
            renderPng :: FilePath -> Document -> IO ()
            renderPng pngfilename doc = do
                cache <- loadCreateFontCache "fonty-texture-cache"
                (finalImage, _) <- renderSvgDocument cache Nothing 300 doc
                writePng pngfilename finalImage

generateSVGDeck :: [Document] -> Document -> [Document]
generateSVGDeck symbolSVGs flipSideSVG = generateDeck (SVGDeckRenderer 2 2 1000 3000 3000 flipSideSVG) symbolSVGs

generateSVGDeck' :: [BS.ByteString] -> BS.ByteString -> IO [BS.ByteString]
generateSVGDeck' symbolBSs flipSideBS = do
    let symbolSVGs = sequenceA (parseSvgFile "" <$> symbolBSs)
    let flipSideSVG = parseSvgFile "" flipSideBS
    let (Just docs) = generateSVGDeck <$> symbolSVGs <*> flipSideSVG
    cache <- loadCreateFontCache "fonty-texture-cache"
    fmap (LBS.toStrict . encodePng . fst) <$> traverse (renderSvgDocument cache Nothing 300) docs



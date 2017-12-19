{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Osu where

import Data.Bits
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Maybe
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Obmapp.Beatmap as B
import Obmapp.Parser

versionInfo :: Parser B.FormatVersion
versionInfo = do
    _ <- string "osu file format v"
    v <- nat
    if v >= 1 && v <= 14
        then pure (B.FormatVersion v)
        else unexpected . Label . fromList $ "a valid format version number"

section :: T.Text -> Parser a -> Parser a
section title p = do
    actualTitle <- sectionTitle
    if title == actualTitle
        then (\_ x _ -> x) <$> untilNextLine <*> p <*> many (try untilNextLine)
        else label ("a matching section title (" ++ T.unpack title ++ ")") $ unexpected . Tokens . fromList . T.unpack $ actualTitle

sectionTitle :: Parser T.Text
sectionTitle = T.pack <$> between (symbol "[") (symbol "]") (many (notChar ']'))

kvPair :: T.Text -> Parser a -> Parser (Maybe a)
kvPair key p = Just <$> (const <$> keyValuePair key p <*> untilNextLine)

keyValuePair :: T.Text -> Parser a -> Parser a
keyValuePair key p = (\_ _ _ _ x -> x) <$> string key <*> optional linespace <*> char ':' <*> optional linespace <*> p

textValue :: Parser T.Text
textValue = textRemainingOnLine

hitObject :: Parser B.HitObject
hitObject = do
    x <- int
    _ <- char ','
    y <- int
    _ <- char ','
    time <- int
    _ <- char ','
    (type', newCombo) <- hitObjectTypeDetails
    _ <- char ','
    hitSnd <- hitSound
    _ <- case type' of
        HitCircle -> const Nothing <$> nothing
        _         -> Just <$> char ','
    (details, extras) <- hitObjectDetailsAndExtras type'
    pure B.HitObject
        { B.position = (x, y)
        , B.time = time
        , B.newCombo = newCombo
        , B.hitSound = hitSnd
        , B.details = details
        , B.extras = extras }

data HitObjectType = HitCircle | Slider | Spinner deriving (Eq, Show)

hitObjectTypeDetails :: Parser (HitObjectType, B.NewCombo)
hitObjectTypeDetails = do
    typeInfo <- int
    case getType typeInfo of
        Just type' -> pure (type', getNewCombo typeInfo)
        Nothing    -> unexpected . Label . fromList $ "a valid hit object type"
    where
        getType t
            | testBit t 0 = Just HitCircle
            | testBit t 1 = Just Slider
            | testBit t 3 = Just Spinner
            | otherwise   = Nothing
        getNewCombo t
            | not (testBit t 2) = Nothing
            | otherwise         = Just $ shiftR t 4 .&. bits [0..2]
        bits = foldr (\n xs -> bit n .|. xs) zeroBits

hitSound :: Parser B.HitSound
hitSound = hs <$> int where
    hs x = B.HitSound
        { B.normalHitSound  = testBit x 0
        , B.whistleHitSound = testBit x 1
        , B.finishHitSound  = testBit x 2
        , B.clapHitSound    = testBit x 3 }

hitObjectDetailsAndExtras :: HitObjectType -> Parser (B.HitObjectDetails, Maybe B.HitObjectExtras)
hitObjectDetailsAndExtras HitCircle = (\extras -> (B.HitCircle, extras)) <$> optionalHitObjectExtras
hitObjectDetailsAndExtras Slider = do
    shape <- sliderShape
    _ <- char ','
    repeats <- int
    _ <- char ','
    pixelLength <- float
    hitSoundsAndExtras <- optional $ do
        _ <- char ','
        hitSounds' <- hitSound `sepBy` char '|'
        _ <- char ','
        edgeExtras' <- edgeExtras `sepBy` char '|'
        extras' <- optionalHitObjectExtras
        pure (hitSounds', edgeExtras', extras')
    let hitSounds = fromMaybe [] (fmap (\(x, _, _) -> x) hitSoundsAndExtras)
    let edgeExtras' = fromMaybe [] (fmap (\(_, x, _) -> x) hitSoundsAndExtras)
    let extras = fromMaybe Nothing (fmap (\(_, _, x) -> x) hitSoundsAndExtras)
    if length hitSounds /= length edgeExtras'
        then unexpected . Label . fromList $ "matching numbers of slider edge hitsounds and extras"
        else pure (B.Slider
                { B.sliderShape = shape
                , B.edgeInfo = B.EdgeInfo
                    { B.repeats = repeats
                    , B.hitSoundsAndAdditions = zip hitSounds edgeExtras' }
                , B.pixelLength = pixelLength }, extras)
hitObjectDetailsAndExtras Spinner = (\endTime extras -> (B.Spinner { B.endTime = endTime }, extras)) <$> int <*> optionalHitObjectExtras

sliderShape :: Parser B.SliderShape
sliderShape = do
    type' <- sliderType
    case type' of
        Linear  -> B.Linear <$> some
            ((\_ x _ y -> (x, y))
            <$> char '|' <*> int <*> char ':' <*> int)
        Perfect -> (\_ x1 _ y1 _ x2 _ y2 -> B.Perfect (x1, y1) (x2, y2))
            <$> char '|'
            <*> int <*> char ':' <*> int
            <*> char '|'
            <*> int <*> char ':' <*> int
        Bezier  -> (\ps -> B.Bezier $ breakWhen (==) ps)
            <$> many
                ((\_ x _ y -> (x, y))
                <$> char '|' <*> int <*> char ':' <*> int)
        Catmull -> B.Catmull <$> many
            ((\_ x _ y -> (x, y))
            <$> char '|' <*> int <*> char ':' <*> int)

breakWhen :: (a -> a -> Bool) -> [a] -> [[a]]
breakWhen f = let
    g p ((q:qs), ps)
        | f p q     = ([p], (q:qs):ps)
        | otherwise = (p:q:qs, ps)
    g p ([], ps) = ([p], ps)

    combine (ps@(_:_), qs) = ps:qs
    combine ([]      , qs) =    qs

    in combine . foldr g ([], [])

data SliderType = Linear | Perfect | Bezier | Catmull deriving (Eq, Show)

sliderType :: Parser SliderType
sliderType = do
    c <- anyChar
    label "a valid slider type (L, P, B, or C)" $ case c of
        'L' -> pure Linear
        'P' -> pure Perfect
        'B' -> pure Bezier
        'C' -> pure Catmull
        _   -> unexpected (Tokens (c :| []))

edgeExtras :: Parser B.SliderExtras
edgeExtras = (\sampleSet _ additionSet -> B.SliderExtras
        { B.sliderSampleSet = sampleSet
        , B.sliderAdditionSet = additionSet })
    <$> int <*> char ':' <*> int

optionalHitObjectExtras :: Parser (Maybe B.HitObjectExtras)
optionalHitObjectExtras = fromMaybe Nothing <$> optional (flip const <$> char ',' <*> optional hitObjectExtras)

hitObjectExtras :: Parser B.HitObjectExtras
hitObjectExtras = e
    <$> int -- sample set
    <*> char ':'
    <*> int -- addition set
    <*> char ':'
    <*> int -- custom index
    <*> char ':'
    <*> int -- sample volume
    <*> char ':'
    <*> textRemainingOnLine
    where
        e s _ a _ i _ v _ f = B.HitObjectExtras
            { B.extrasSampleSet = s
            , B.extrasAdditionSet = a
            , B.extrasCustomIndex = i
            , B.extrasSampleVolume = v
            , B.extrasFileName = f }

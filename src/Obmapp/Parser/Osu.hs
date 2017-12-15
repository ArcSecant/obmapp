{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Osu where

import Data.Bits
import Data.Maybe
import qualified Data.Text as T
import qualified Obmapp.Beatmap as B
import Obmapp.Parser
import Obmapp.Parser.FormatError

versionInfo :: Parser B.FormatVersion
versionInfo = const B.FormatVersion  <$> text "osu file format v" <*> resultFulfills (> 0) naturalNumber `withErrMsg` "Could not parse version info."

section :: T.Text -> Parser a -> Parser a
section name p = flip withErrMsg ("Could not parse section: " ++ T.unpack name ++ ".") $ Parser $ \t -> do
    (name', t') <- runParser ((\_ name' _ -> name') <$> optional whitespace <*> sectionTitle <*> whitespace) t
    if name == name'
        then runParser (const <$> p <*> optional whitespace) t'
        else Left $ ParseError (MissingText $ T.unpack name) Nothing

sectionTitle :: Parser T.Text
sectionTitle = between "[" "]" (while (/= ']')) `withErrMsg` "Could not parse section title."

kvPair :: T.Text -> Parser a -> Parser (Maybe a)
kvPair t p = const <$> optional (loneKeyValuePair t p) <*> whitespace `withErrMsg` "Could not parse an optional key-value pair."

loneKeyValuePair :: T.Text -> Parser a -> Parser a
loneKeyValuePair t p = flip withErrMsg "Could not parse a key-value pair." $ (\_ _ _ _ x -> x)
    <$> text t
    <*> optional linespace
    <*> char ':'
    <*> optional linespace
    <*> p

textValue :: Parser T.Text
textValue = fmap T.strip $ untilT "\r\n"

hitObject :: Parser B.HitObject
hitObject = flip withErrMsg "Could not parse hit object." $ Parser $ \t -> do
    ((position, time, (type', newCombo), hitSnd), t1) <- runParser
        ((\x _ y _ time _ typeDetails _ hitSnd -> ((x, y), time, typeDetails, hitSnd))
        <$> int
        <*> char ','
        <*> int
        <*> char ','
        <*> int
        <*> char ','
        <*> hitObjectTypeDetails
        <*> char ','
        <*> hitSound)
        t
    (_, t2) <- flip runParser t1 $ case type' of
        HitCircle -> Parser $ \t' -> Right (Nothing, t')
        _         -> fmap Just (char ',')
    runParser ((\details extras -> B.HitObject
        { B.position = position
        , B.time = time
        , B.newCombo = newCombo
        , B.hitSound = hitSnd
        , B.details = details
        , B.extras = fromMaybe Nothing extras })
            <$> hitObjectDetails type'
            <*> optional (flip const <$> char ',' <*> optional hitObjectExtras))
        t2

data HitObjectType = HitCircle | Slider | Spinner deriving (Eq, Show)

hitObjectTypeDetails :: Parser (HitObjectType, B.NewCombo)
hitObjectTypeDetails = Parser $ \t -> do
    (typeInfo, t') <- runParser int t
    case getType typeInfo of
        Just type' -> Right ((type', getNewCombo typeInfo), t')
        Nothing    -> Left $ ParseError (FormatError MissingHitObjectType) Nothing
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

hitObjectDetails :: HitObjectType -> Parser B.HitObjectDetails
hitObjectDetails HitCircle = Parser $ \t -> Right (B.HitCircle, t)
hitObjectDetails Slider = Parser $ \t -> do
    ((shape, repeats, pixelLength, hitSounds, extras), t') <- runParser
        ((\shape _ repeats _ pixelLength _ hitSounds _ extras -> (shape, repeats, pixelLength, hitSounds, extras))
        <$> sliderShape
        <*> char ','
        <*> int
        <*> char ','
        <*> float
        <*> char ','
        <*> hitSound `sepBy` char '|'
        <*> char ','
        <*> edgeExtras `sepBy` char '|')
        t
    if any (/= repeats + 1) $ [length hitSounds, length extras]
        then Left $ ParseError (FormatError $ MismatchingSliderRepeats (repeats + 1) (length hitSounds) (length extras)) Nothing
        else Right (B.Slider
                { B.sliderShape = shape
                , B.edgeInfo = B.EdgeInfo
                    { B.repeats = repeats
                    , B.hitSoundsAndAdditions = zip hitSounds extras }
                , B.pixelLength = pixelLength }, t')
hitObjectDetails Spinner = (\endTime -> B.Spinner { B.endTime = endTime }) <$> int

sliderShape :: Parser B.SliderShape
sliderShape = Parser $ \t -> do
    (type', t1) <- runParser sliderType t
    flip runParser t1 $ case type' of
        Linear  -> B.Linear <$> atLeast 1
            ((\_ x _ y -> (x, y))
            <$> char '|' <*> int <*> char ':' <*> int)
        Perfect -> (\_ x1 _ y1 _ x2 _ y2 -> B.Perfect (x1, y1) (x2, y2))
            <$> char '|'
            <*> int <*> char ':' <*> int
            <*> char '|'
            <*> int <*> char ':' <*> int
        Bezier  -> (\ps -> B.Bezier $ breakWhen (==) ps)
            <$> atLeast 0
                ((\_ x _ y -> (x, y))
                <$> char '|' <*> int <*> char ':' <*> int)
        Catmull -> B.Catmull <$> atLeast 0
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
sliderType = Parser $ \t -> do
    (c, t') <- runParser aChar t
    case c of
        'L' -> Right (Linear, t')
        'P' -> Right (Perfect, t')
        'B' -> Right (Bezier, t')
        'C' -> Right (Catmull, t')
        _   -> Left $ ParseError (FormatError $ UnknownSliderType c) Nothing

edgeExtras :: Parser B.SliderExtras
edgeExtras = (\sampleSet _ additionSet -> B.SliderExtras
        { B.sliderSampleSet = sampleSet
        , B.sliderAdditionSet = additionSet })
    <$> int <*> char ':' <*> int

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
    <*> untilT "\r\n"
    where
        e s _ a _ i _ v _ f = B.HitObjectExtras
            { B.extrasSampleSet = s
            , B.extrasAdditionSet = a
            , B.extrasCustomIndex = i
            , B.extrasSampleVolume = v
            , B.extrasFileName = f }

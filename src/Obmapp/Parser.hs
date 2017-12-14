{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
import Obmapp.Parser.FormatError

data ParseError = ParseError
    { cause :: Cause
    , description :: Maybe String }
    deriving (Eq, Show)

data Cause
    = EndOfInput
    | ConditionNotFulfilled
    | MissingText String
    | NoParse
    | FormatError FormatError
    | CustomError String
    deriving (Eq, Show)

newtype Parser a = Parser { runParser :: T.Text -> Either ParseError (a, T.Text) }

withErrMsg :: Parser a -> String -> Parser a
withErrMsg (Parser p) msg = Parser $ \t -> case p t of
    Left (ParseError cause' _) -> Left $ ParseError cause' (Just msg)
    r -> r

instance Functor Parser where
    fmap f (Parser p) = Parser q where
        q t = do
            (x, t') <- p t
            pure (f x, t')

instance Applicative Parser where
    pure x = Parser . const . pure $ (x, T.empty)
    Parser p1 <*> Parser p2 = Parser p where
        p t = do
            (f, t') <- p1 t
            (x, t'') <- p2 t'
            pure (f x, t'')

instance Alternative Parser where
    empty = Parser . const . Left $ ParseError NoParse Nothing
    Parser p1 <|> Parser p2 = Parser $ \t -> case p1 t of
        Right r -> Right r
        Left _ -> case p2 t of
            Right r -> Right r
            Left _ -> Left $ ParseError NoParse Nothing

(<?>) :: Parser a -> Parser b -> Parser (a, b)
p <?> q = ((\x y -> (x, y)) <$> p <*> q) <|> ((\y x -> (x, y)) <$> q <*> p)

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p
    | n > 0     = Parser $ \t -> do
        (x, t') <- runParser p t
        (xs, t'') <- runParser (atLeast (n - 1) p) t'
        pure (x:xs, t'')
    | otherwise = Parser $ \t -> case runParser p t of
        Left _ -> Right ([], t)
        Right (x, t') -> do
            (xs, t'') <- runParser (atLeast n p) t'
            pure (x:xs, t'')

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = maybeListToList <$> optional ((:) <$> p <*> atLeast 0 (flip const <$> q <*> p))

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p q = (\x xs -> x:maybeListToList xs) <$> p <*> optional (flip const <$> q <*> p `sepBy` q)

maybeListToList :: Maybe [a] -> [a]
maybeListToList Nothing = []
maybeListToList (Just xs) = xs

optional :: Parser a -> Parser (Maybe a)
optional (Parser p) = Parser $ \t -> case p t of
    Left _ -> pure (Nothing, t)
    Right (x, t') -> pure (Just x, t')

fulfills :: (Char -> Bool) -> Parser Char
fulfills f = Parser $ \t -> case T.uncons t of
    Nothing -> Left $ ParseError EndOfInput Nothing
    Just (c, t') -> if f c
        then pure (c, t')
        else Left $ ParseError ConditionNotFulfilled Nothing

resultFulfills :: (a -> Bool) -> Parser a -> Parser a
resultFulfills f (Parser p) = Parser $ \t -> do
    r@(x, _) <- p t
    if f x
        then pure r
        else Left $ ParseError ConditionNotFulfilled Nothing

while :: (Char -> Bool) -> Parser T.Text
while f = Parser $ pure . T.span f

untilT :: T.Text -> Parser T.Text
untilT t = Parser $ pure . T.breakOn t

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight l Nothing = Left l
maybeToRight _ (Just r) = Right r

between :: T.Text -> T.Text -> Parser a -> Parser a
between a b p = Parser $ \t -> do
    (_, t1) <- runParser (text a) t
    (x, t2) <- runParser p t1
    (_, t3) <- runParser (text b) t2
    pure (x, t3)

aChar :: Parser Char
aChar = Parser $ \ t -> case T.uncons t of
    Just (c, t') -> Right (c, t')
    Nothing      -> Left $ ParseError EndOfInput Nothing

char :: Char -> Parser Char
char c = fulfills (== c)

whitespace :: Parser T.Text
whitespace =  fmap T.pack $ atLeast 1 oneWhitespace

oneWhitespace :: Parser Char
oneWhitespace = fulfills isSpace

linespace :: Parser T.Text
linespace = fmap T.pack $ atLeast 1 oneLinespace

oneLinespace :: Parser Char
oneLinespace = fulfills (`elem` [' ', '\t'])

naturalNumber :: Parser Int
naturalNumber = read <$> atLeast 1 (fulfills isDigit)

int :: Parser Int
int = f <$> optional (char '-') <*> naturalNumber where
    f Nothing n =  n
    f _       n = -n

float :: Parser Float
float = f <$> int <*> optional (flip const <$> char '.' <*> naturalNumber) where
    f i (Just d) = read $ show i ++ "." ++ show d
    f i Nothing  = fromIntegral i

text :: T.Text -> Parser T.Text
text t = Parser $ \t' -> do
    t'' <- maybeToRight (ParseError (MissingText $ T.unpack t) Nothing) (T.stripPrefix t t')
    pure (t, t'')

{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser where

import Data.Char (isDigit, isPrint, isSpace)
import qualified Data.Text as T

data ParseError
    = EndOfInput
    | ConditionNotFulfilled
    | MissingText String
    deriving (Eq, Show)

newtype Parser a = Parser { runParser :: T.Text -> Either [ParseError] (a, T.Text) }

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

optional :: Parser a -> Parser (Maybe a)
optional (Parser p) = Parser $ \t -> case p t of
    Left _ -> pure (Nothing, t)
    Right (x, t') -> pure (Just x, t')

fulfills :: (Char -> Bool) -> Parser Char
fulfills f = Parser $ \t -> case T.uncons t of
    Nothing -> Left [EndOfInput]
    Just (c, t') -> if f c
        then pure (c, t')
        else Left [ConditionNotFulfilled]

resultFulfills :: (a -> Bool) -> Parser a -> Parser a
resultFulfills f (Parser p) = Parser $ \t -> do
    r@(x, t') <- p t
    if f x
        then pure r
        else Left [ConditionNotFulfilled]

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

text :: T.Text -> Parser T.Text
text t = Parser $ \t' -> do
    t'' <- maybeToRight [MissingText $ T.unpack t] (T.stripPrefix t t')
    pure (t, t'')

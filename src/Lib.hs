module Lib where

import Data.Text as T

data ParseError = ParseError

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

newtype Version = Version Int deriving (Eq, Show)

parseVersionInfo :: T.Text -> Maybe Version
parseVersionInfo = undefined

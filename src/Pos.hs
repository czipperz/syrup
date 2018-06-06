module Pos where

data Pos = Pos String Int Int deriving (Eq)
data Tag a = Tag Pos a deriving (Eq)

advance :: Pos -> Char -> Pos
advance (Pos fileName line _) '\n' = Pos fileName (line + 1) 1
advance (Pos fileName line column) _ = Pos fileName line (column + 1)

unwrapTag :: Tag a -> a
unwrapTag (Tag _ a) = a

pos :: Tag a -> Pos
pos (Tag p _) = p

instance Functor Tag where
  fmap f (Tag p x) = Tag p (f x)
instance Show Pos where
  show (Pos fileName line column) = fileName ++ ':' : show line ++ ':' : show column
instance Show a => Show (Tag a) where
  show (Tag _ a) = show a

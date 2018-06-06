module Type where

data Type = TInt
          | TChar
          | TVoid
          | TPointer Type
          deriving Eq

instance Show Type where
  show TInt = "int"
  show TChar = "char"
  show TVoid = "void"
  show (TPointer t) = show t ++ "*"

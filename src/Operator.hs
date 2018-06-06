module Operator where

data Operator = Comma
              | Plus
              | Minus
              | Times
              | Divide
              | Assign
              deriving Eq

instance Show Operator where
  show Comma = ","
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show Assign = "="


module Print where

showListNoBrace :: Show a => [a] -> String
showListNoBrace [] = ""
showListNoBrace (x:xs) = s x xs
  where s a (x:xs) = show a ++ ", " ++ s x xs
        s a [] = show a

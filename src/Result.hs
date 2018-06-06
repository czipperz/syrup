module Result where

import Control.Monad (ap, liftM)
import Control.Monad.Fail

data Result a = Err String | Ok a

instance Functor Result where
  fmap = liftM
instance Applicative Result where
  pure = return
  (<*>) = ap
instance Monad Result where
  return = Ok
  Err e >>= _ = Err e
  Ok a >>= f = f a
  fail = Err
instance MonadFail Result where
  fail = Err

unwrapResult :: Result a -> a
unwrapResult (Ok a) = a
unwrapResult (Err e) = error $ show e

instance Show a => Show (Result a) where
  show (Err e) = e
  show (Ok a) = show a

module Value where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Type

data WrappedValue = WVInt Int
                  | WVChar Char
                  | WVString String
                  | WVVoid
                  deriving Eq

data Reference = Reference (Ptr ()) Type

data MaybeReference = Ref Reference | Val WrappedValue

instance Show WrappedValue where
  show (WVInt i) = show i
  show (WVString s) = show s
  -- show (VPointer
  show WVVoid = "(void)"

instance Show Reference where
  show (Reference r _) = show r

instance Show MaybeReference where
  show (Ref r) = show r
  show (Val v) = show v

undefinedWValue :: Type -> WrappedValue
undefinedWValue TInt = WVInt 0
undefinedWValue TVoid = WVVoid

newReference :: WrappedValue -> IO Reference
newReference (WVInt v) = do
  p <- new v
  return $ Reference (castPtr p) TInt
newReference WVVoid = return $ Reference nullPtr TVoid

freeReference :: Reference -> IO ()
freeReference (Reference p _) = if p == nullPtr then return () else free p

evalReference :: Reference -> IO WrappedValue
evalReference (Reference r TInt) = fmap WVInt . peek . castPtr $ r
evalReference (Reference _ TVoid) = return WVVoid

evalMaybeReference :: MaybeReference -> IO WrappedValue
evalMaybeReference (Ref r) = evalReference r
evalMaybeReference (Val v) = return v

setReference :: Reference -> WrappedValue -> IO ()
setReference (Reference p TInt) (WVInt val) = flip poke val $ castPtr p
setReference (Reference _ TVoid) _ = return ()

typeOfReference :: Reference -> Type
typeOfReference (Reference _ t) = t

module Report where

data Report a =
    Success a
  | Error [String]
  deriving Show

instance Functor Report where
  fmap f (Success a) = Success (f a)
  fmap _ (Error errors) = Error errors

instance Applicative Report where
  pure = Success
  Success f <*> Success a = Success (f a)
  Success _ <*> Error errors = Error errors
  Error errors <*> Success _ = Error errors
  Error errors <*> Error more = Error (errors ++ more)

instance Monad Report where
  Success a >>= f = f a
  Error errors >>= _ = Error errors

labelAndItemize :: String -> Report a -> Report a
labelAndItemize label (Error errors) = Error (label : map ("  - " ++ ) errors)
labelAndItemize _ report = report

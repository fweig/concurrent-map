module GuardedValue (
    GuardedValue(..)
) where

{- 
 - Datatype for simulating the values -oo and +oo
 -}
data GuardedValue a = LeftSentinel | Value a | RightSentinel
  deriving (Eq)

instance (Show a) => Show (GuardedValue a) where
    show LeftSentinel  = "-oo"
    show RightSentinel = "+oo"
    show (Value a)     = show a

instance (Ord a) => Ord (GuardedValue a) where
    compare (Value a)     (Value b)     = compare a b 
    compare RightSentinel (Value a)     = GT
    compare RightSentinel LeftSentinel  = GT
    compare RightSentinel RightSentinel = EQ
    compare LeftSentinel  (Value a)     = LT
    compare LeftSentinel  RightSentinel = LT
    compare LeftSentinel  LeftSentinel  = EQ
    compare (Value a)     RightSentinel = LT
    compare (Value a)     LeftSentinel  = GT

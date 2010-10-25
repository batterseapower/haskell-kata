{-# LANGUAGE TypeFamilies, EmptyDataDecls, ScopedTypeVariables, TypeOperators, FlexibleInstances, FlexibleContexts #-}

import Data.Monoid


-- Type family for evaluators on types
type family E a :: *

-- Tag for functor application: fundamental to our approach
infixr 9 :%
data f :% a

-- Tags for evalutor-style data declarations: such declarations contain "internal"
-- occurrences of E, so we can delay evaluation of their arguments
data P0T (f :: *)
type instance E (P0T f) = f
data P1T (f :: * -> *)
type instance E (P1T f :% a) = f a
data P2T (f :: * -> * -> *)
type instance E (P2T f :% a :% b) = f a b
data P3T (f :: * -> * -> * -> *)
type instance E (P3T f :% a :% b :% c) = f a b c

-- When applying legacy data types we have to manually force the arguments:
data FunT
type instance E (FunT :% a :% b) = E a -> E b
data Tup2T
type instance E (Tup2T :% a :% b) = (E a, E b)
data Tup3T
type instance E (Tup3T :% a :% b :% c) = (E a, E b, E c)


-- Evalutor-style versions of some type classes
class FunctorT f where
    fmapT :: (E a -> E b) -> E (f :% a) -> E (f :% b)

class MonoidT a where
    memptyT :: E a
    mappendT :: E a -> E a -> E a


data AdditiveIntT
type instance E AdditiveIntT = Int
instance MonoidT AdditiveIntT where
    memptyT = 0
    mappendT = (+)

data MultiplicativeIntT
type instance E MultiplicativeIntT = Int
instance MonoidT MultiplicativeIntT where
    memptyT = 1
    mappendT = (*)

-- Make the default instance of Monoid be additive:
instance MonoidT (P0T Int) where
    memptyT = memptyT :: E AdditiveIntT
    mappendT = mappendT :: E AdditiveIntT -> E AdditiveIntT -> E AdditiveIntT


main = do
    print (result :: E (P0T Int))
    print (result :: E MultiplicativeIntT)
  where
    result :: forall a. (E a ~ Int, MonoidT a) => E a
    result _ = memptyT `mappendT` 2 `mappendT` 3
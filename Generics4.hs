{-# LANGUAGE TypeFamilies, EmptyDataDecls, ScopedTypeVariables, TypeOperators, FlexibleInstances, FlexibleContexts #-}

import Data.Monoid


-- Type family for evaluators on types
newtype E a = E { unE :: E' a } -- Work around lack of type application by reifying a type witnesses as a newtype parameter
type family E' a :: *

-- Tag for functor application: fundamental to our approach
infixr 9 :%
data f :% a

-- Tags for evalutor-style data declarations: such declarations contain "internal"
-- occurrences of E, so we can delay evaluation of their arguments
data P0T (f :: *)
type instance E' (P0T f) = f
data P1T (f :: * -> *)
type instance E' (P1T f :% a) = f a
data P2T (f :: * -> * -> *)
type instance E' (P2T f :% a :% b) = f a b
data P3T (f :: * -> * -> * -> *)
type instance E' (P3T f :% a :% b :% c) = f a b c

-- When applying legacy data types we have to manually force the arguments:
data FunT
type instance E' (FunT :% a :% b) = E a -> E b
data Tup2T
type instance E' (Tup2T :% a :% b) = (E a, E b)
data Tup3T
type instance E' (Tup3T :% a :% b :% c) = (E a, E b, E c)


-- Type-level fixed points require UndecidableInstances, as you would expect!
--data FixT
--type instance E' (FixT :% f) = E' (f :% (FixT :% f))


-- Evalutor-style versions of some type classes
class FunctorT f where
    fmapT :: (E a -> E b) -> E (f :% a) -> E (f :% b)

class MonoidT a where
    memptyT :: E a
    mappendT :: E a -> E a -> E a


data AdditiveIntT
type instance E' AdditiveIntT = Int
instance MonoidT AdditiveIntT where
    memptyT = E 0
    mappendT (E x) (E y) = E (x + y)

data MultiplicativeIntT
type instance E' MultiplicativeIntT = Int
instance MonoidT MultiplicativeIntT where
    memptyT = E 1
    mappendT (E x) (E y) = E (x * y)


castE0 :: (a ~ a') => E a -> E a'
castE0 (E x) = E x

castE1 :: (a ~ a', b ~ b') => (E a -> E b) -> E a' -> E b'
castE1 f = castE0 . f . castE0

castE2 :: (a ~ a', b ~ b', c ~ c') => (E a -> E b -> E c) -> E a' -> E b' -> E c'
castE2 f = castE1 . f . castE0


-- Make the default instance of Monoid be additive:
instance MonoidT (P0T Int) where
    -- FIXME: these two don't work, but I don't fully understand why:
    --memptyT = castE0 (memptyT :: E AdditiveIntT)
    --mappendT = castE2 (mappendT :: E AdditiveIntT -> E AdditiveIntT -> E AdditiveIntT)
    
    -- These two do work:
    memptyT = case memptyT :: E AdditiveIntT of E x -> E x
    mappendT x y = case (mappendT :: E AdditiveIntT -> E AdditiveIntT -> E AdditiveIntT) (case x of E x -> E x) (case y of E y -> E y) of E z -> E z


main = do
    print $ unE (result :: E (P0T Int))
    print $ unE (result :: E AdditiveIntT)
    print $ unE (result :: E MultiplicativeIntT)
  where
    result :: forall a. (E' a ~ Int, MonoidT a) => E a
    result = memptyT `mappendT` E 2 `mappendT` E 3
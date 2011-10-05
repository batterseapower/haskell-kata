{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Process2 where

import Control.Applicative (Applicative(..), liftA2)
import Control.Monad (liftM, liftM2, ap, join)
import Data.Foldable (toList)
import Data.Traversable (Traversable(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))

import Debug.Trace


-- If you don't want DelayM to have Monad structure, you can nuke the nested use of DelayM,
-- and make some of the consumers simpler. I actually want this generalisation, though.
data DelayM q a r = Done r
                  | forall f. DelayStructure f => Delayed (f q) (f a -> DelayM q a r)

instance Functor (DelayM q a) where
    fmap f x = pure f <*> x

instance Applicative (DelayM q a) where
    pure = return
    Done f         <*> Done x         = Done (f x)
    Delayed qs k   <*> Done x         = Delayed qs (\as -> k as <*> Done x)
    Done f         <*> Delayed qs k   = Delayed qs (\as -> Done f <*> k as)
    Delayed qs1 k1 <*> Delayed qs2 k2 = Delayed (Pair qs1 qs2) (\(Pair as1 as2) -> k1 as1 <*> k2 as2)

instance Monad (DelayM q a) where
    return = Done
    Done x       >>= fxmy = fxmy x
    Delayed qs k >>= fxmy = Delayed qs (\as -> k as >>= fxmy)

delay :: Show q => q -> DelayM q a a
delay q = Delayed (Identity q) (\(Identity a) -> pure a)

justOne :: Show q => DelayM q a r -> DelayM q a (DelayM q a r)
justOne (Done x)       = Done (Done x)
justOne (Delayed qs k) = fmap k (delayMany qs)

class Traversable f => DelayStructure f where
    delayMany :: Show q => f q -> DelayM q a (f a)
    
    -- For debugging only:
    show1 :: Show a => f a -> String

instance DelayStructure Identity where
    delayMany (Identity q) = fmap Identity (delay q)
    show1 (Identity x) = "I (" ++ show x ++ ")"

instance (DelayStructure f, DelayStructure g) => DelayStructure (Product f g) where
    -- NB: the use of liftM2 is critical here! It ensures all but the leftmost child will be delayed.
    delayMany (Pair qs1 qs2) = liftM2 Pair (delayMany qs1) (delayMany qs2)
    show1 (Pair x y) = "P (" ++ show1 x ++ ") (" ++ show1 y ++ ")"


-- Simple example supercompiler-alike to show that it all works:


type HFunction = Int
--data State = State deriving (Eq)
type State = Int
data Term = Tieback HFunction | Base Int | Split Term Term deriving (Show)

type ScpM = DelayM State Term

-- Execution trace:
-- 10 => 9
--  4 => 3
--   1 => 0 BASE
--   2 => 1 BASE
--  5 => 4
--   2 => 1 BASE
--   3 => 2 BASE

split :: (State -> ScpM Term)
      -> State -> ScpM Term
--split = undefined
split f x | x <= 2    = pure (Base x)
          | otherwise = liftA2 Split (f (x `div` 2)) (f ((x `div` 2) + 1)) -- Change A2 to M2 to linearise the search :-)

reduce :: State -> State
--reduce = undefined
reduce x = x-1


supercompile :: State -> ([(State, HFunction)], Term)
supercompile state = unMemoM (my_choose (scPostMatch state)) []
  where
    -- Depth-first, left-biased exploration of the process tree.
    -- NB: by varying the implementation of justOne you change the strategy:
    --my_choose = choose justOne

    -- Breadth-first exploration of the process tree:
    my_choose = choose return

-- A simplified version: no sc-history
scPostMatch :: State -> ScpM Term
scPostMatch state = split delay (reduce state)


newtype MemoM a = MemoM { unMemoM :: [(State, HFunction)] -> ([(State, HFunction)], a) }

instance Functor MemoM where
    fmap = liftM

instance Applicative MemoM where
    pure = return
    (<*>) = ap

instance Monad MemoM where
    return x = MemoM $ \s -> (s, x)
    MemoM xf >>= fxmy = MemoM $ \s -> case xf s of (s', x) -> unMemoM (fxmy x) s'

modify :: ([(State, HFunction)] -> ([(State, HFunction)], a))
       -> MemoM a
modify = MemoM

choose :: (ScpM a -> ScpM (ScpM a))
       -> ScpM a -> MemoM a
choose choose_some = go
  where
    go = go' . join . choose_some
    
    go' (Done x)                    = pure x
    go' (Delayed (qs :: f State) k) = trace ("iteration: " ++ show1 qs) $ traverse sc qs >>= \fs -> go (sequenceA fs >>= k)

    sc :: State -> MemoM (ScpM Term)
    sc state = modify $ \memo -> case lookup state memo of
                                   Just h  -> (memo, pure (Tieback h))
                                   Nothing -> ((state, h'):memo, scPostMatch state)
                                     where h' = 1 + maximum (0:map snd memo)


main = supercompile 10
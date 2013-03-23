{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Process2 where

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Monad (liftM, liftM2, ap, join)
import Data.Foldable (Foldable(..), toList)
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

import Debug.Trace


data DelayStructure sh a where
    Leaf :: a -> DelayStructure () a
    Branch :: DelayStructure sh1 a -> DelayStructure sh2 a -> DelayStructure (sh1, sh2) a

instance Show a => Show (DelayStructure sh a) where
    show (Leaf x) = "Leaf (" ++ show x ++ ")"
    show (Branch t1 t2) = "Branch (" ++ show t1 ++ ") (" ++ show t2 ++ ")"

instance Functor (DelayStructure sh) where
    fmap = fmapDefault

instance Foldable (DelayStructure sh) where
    foldMap = foldMapDefault

instance Traversable (DelayStructure sh) where
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Branch t1 t2) = Branch <$> traverse f t1 <*> traverse f t2


-- If you don't want DelayM to have Monad structure, you can nuke the nested use of DelayM,
-- and make some of the consumers simpler. I actually want this generalisation, though.
data DelayM q a r = Done r
                  | forall sh. Delayed (DelayStructure sh q) (DelayStructure sh a -> DelayM q a r)

instance Functor (DelayM q a) where
    fmap f x = pure f <*> x

instance Applicative (DelayM q a) where
    pure = return
    Done f         <*> Done x         = Done (f x)
    Delayed qs k   <*> Done x         = Delayed qs (\as -> k as <*> Done x)
    Done f         <*> Delayed qs k   = Delayed qs (\as -> Done f <*> k as)
    Delayed qs1 k1 <*> Delayed qs2 k2 = Delayed (Branch qs1 qs2) (\(Branch as1 as2) -> k1 as1 <*> k2 as2)

instance Monad (DelayM q a) where
    return = Done
    Done x       >>= fxmy = fxmy x
    Delayed qs k >>= fxmy = Delayed qs (\as -> k as >>= fxmy)

delay :: q -> DelayM q a a
delay q = Delayed (Leaf q) (\(Leaf a) -> pure a)

runDelayM :: forall memom q a r.
             (Applicative memom, Monad memom,
              Show q) -- Debugging only
          => (DelayM q a r -> DelayM q a r) -- ^ Chooses the evaluation strategy
          -> (q -> memom (DelayM q a a))    -- ^ How to answer questions in the monad (possibly generating new requests in the process)
          -> DelayM q a r -> memom r
runDelayM choose_some sc = go
  where
    go = go' . choose_some
    
    go' (Done x)       = pure x
    go' (Delayed (qs :: DelayStructure sh q) (k :: DelayStructure sh a -> DelayM q a r))
        = -- trace ("iteration: " ++ show qs) $
          mungeDS sc qs >>= \mx -> go (mx >>= k)


mungeDS :: forall memom sh q a.
           (Applicative memom, Monad memom)
        => (q -> memom (DelayM q a a))
        -> DelayStructure sh q
        -> memom (DelayM q a (DelayStructure sh a))
mungeDS sc qs = (traverse sc qs :: memom (DelayStructure sh (DelayM q a a))) >>= \fs -> return (sequenceA fs :: DelayM q a (DelayStructure sh a))


depthFirst :: DelayM q a r -> DelayM q a r
depthFirst (Done x)       = Done x
depthFirst (Delayed qs k) = delayTail qs >>= k
  where
    delayTail :: DelayStructure sh q -> DelayM q a (DelayStructure sh a)
    delayTail (Leaf q)                                   = fmap Leaf (delay q)
    delayTail (Branch qs1 (qs2 :: DelayStructure sh2 q)) = liftM2 Branch (delayTail qs1) (traverse delay qs2 :: DelayM q a (DelayStructure sh2 a))

breadthFirst :: DelayM q a r -> DelayM q a r
breadthFirst = id


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

split :: Applicative t
      => (State -> t Term)
      -> State -> t Term
--split = undefined
split f x | x <= 2    = pure (Base x)
          | otherwise = liftA2 Split (f (x `div` 2)) (f ((x `div` 2) + 1)) -- Change A2 to M2 to linearise the search :-)

reduce :: State -> State
--reduce = undefined
reduce x = x-1


supercompile :: State -> ([(State, HFunction)], Term)
supercompile state = unMemoM (sc state >>= runDelayM eval_strat sc) []
  where
    --eval_strat = depthFirst
    eval_strat = breadthFirst

-- A simplified version of a supercompiler: no sc-history.
-- Generates requests for States
sc' :: State -> ScpM Term
sc' state = split delay (reduce state)

-- Allows us to answer requests for States in the memoisation monad,
-- possibly generating new requests in the process (hence the nested ScpM)
sc :: State -> MemoM (ScpM Term)
sc = memo sc'

memo :: (State -> ScpM Term)
     -> State -> MemoM (ScpM Term)
memo opt state = modify $ \memo -> case lookup state memo of
                                     Just h  -> (memo, pure (Tieback h))
                                     Nothing -> ((state, h'):memo, opt state)
                                       where h' = 1 + maximum (0:map snd memo)


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


main = print $ supercompile 10
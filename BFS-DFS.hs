{-# LANGUAGE GADTs, ExistentialQuantification, TypeSynonymInstances, ScopedTypeVariables #-}
import Control.Monad

import Data.List
import Data.Ord

import Debug.Trace


newtype ContT r m a = ContT { unContT :: (a -> m r) -> m r }

runContT :: Monad m => ContT r m r -> m r
runContT x = unContT x return

instance Functor (ContT r m) where
    fmap f m = ContT $ \c -> unContT m (c . f)

instance Monad (ContT r m) where
    return x = ContT ($ x)
    m >>= fm = ContT (\c -> unContT m (\x -> unContT (fm x) c))


newtype ScpM' a = ScpM' { unScpM' :: Int -> (Int, a) }

instance Functor ScpM' where
    fmap = liftM

instance Monad ScpM' where
    return x = ScpM' (\s -> (s, x))
    mx >>= fxmy = ScpM' (\s -> case unScpM' mx s of (s, x) -> unScpM' (fxmy x) s)


data ApplicativeTree f a where
    Lifted :: f a -> ApplicativeTree f a
    Pure :: a -> ApplicativeTree f a -- ??
    Deepen :: ApplicativeTree f a -> ApplicativeTree f a
    Star :: ApplicativeTree f (b -> a) -> ApplicativeTree f b -> ApplicativeTree f a

instance Functor f => Functor (ApplicativeTree f) where
    fmap f (Lifted x)   = Lifted (fmap f x)
    fmap f (Star mg mx) = Star (fmap (\g -> f . g) mg) mx


type ScpM = ContT (Res Res') ScpM'
data Res a = forall b. Res (ApplicativeTree ScpM b) (b -> ScpM' (Res a))
           | Done a
type Res' = Tree Int

runScpM :: ScpM Res' -> Res'
runScpM = runScpM' . go0
  where
    go0 :: ScpM Res' -> ScpM' Res'
    go0 mx = unContT mx (return . Done) >>= go1
    
    go1 :: Res Res' -> ScpM' Res'
    go1 (Done x) = return x
    --go1 (Res at cont) = dfs at cont
    go1 (Res at cont) = bfs at cont
    
    dfs :: forall a. ApplicativeTree ScpM a -> (a -> ScpM' (Res Res')) -> ScpM' Res'
    --dfs (Lifted mx)  cont = go0 mx >>= \a -> cont a >>= \r -> go1 r
    --dfs (Lifted mx)  cont = unContT mx (\(x :: a) -> undefined :: ScpM' (Res Res')) >>= \(r :: Res Res') -> undefined :: ScpM' Res'
    dfs (Lifted mx)  cont = unContT mx cont >>= go1
    dfs (Deepen t)   cont = dfs t cont
    dfs (Pure x)     cont = cont x >>= go1
    dfs (Star mf mx) cont = dfs mf (\f -> return (Res (fmap f mx) cont))
    
    {-
    bfs at cont = bfs'' 0 at cont
        
    bfs'' d at cont = trace (show (length cands)) $ snd $ minimumBy (comparing fst) cands
      where cands = bfs' d at cont
    
    bfs' :: Int -> ApplicativeTree ScpM a -> (a -> ScpM' (Res Res')) -> [(Int, ScpM' Res')]
    --bfs' d (Lifted mx)  cont = [(d, unContT mx cont >>= go1)]
    bfs' d (Lifted mx)  cont = do
        r <- unContT mx cont
        case r of Done x      -> return x
                  Res at cont -> bfs' (d + 1) at cont
    bfs' d (Star mf mx) cont = bfs' (d + 1) mf (\f -> return (Res (fmap f mx) cont)) ++ 
                               bfs' (d + 1) mx (\x -> return (Res (fmap ($ x) mf) cont))
    -}
    
    {-
    bfs at cont = bfs' [Candidate 0 at cont]
    
    bfs' :: [Candidate] -> ScpM' Res'
    --bfs' (Candidate (Lifted mx)  cont:cs) = unContT mx cont >>= go1
    bfs' (Candidate (Lifted mx)  cont:cs) = do
        r <- unContT mx cont
        case r of Done x -> return x
                  Res at cont -> 
    --bfs' (Candidate (Deepen t)   cont:cs) = bfs' (cs ++ [Candidate t cont])
    --bfs' (Candidate (Pure x)     cont:cs) = cont x >>= go1
    --bfs' (Candidate (Star mf mx) cont:cs) = bfs' (cs ++ [Candidate mf (\f -> return (Res (fmap f mx) cont)), Candidate mx (\x -> return (Res (fmap ($ x) mf) cont))])
    --bfs' (Candidate (Star mf mx) cont:cs) = bfs' (cs ++ [Candidate mf (\f -> return (Res (Pure f `Star` mx) cont)), Candidate mx (\x -> return (Res (mf `Star` Pure x) cont))])
    bfs' (Candidate (Star mf mx) cont:cs) = bfs' (cs ++ [Candidate mf (\f -> return (Res (Deepen (fmap f mx)) cont)), Candidate mx (\x -> return (Res (Deepen (fmap ($ x) mf)) cont))])
    -}
    
    {-
    bfs :: forall a r.
           ApplicativeTree ScpM a
        -> (ApplicativeTree ScpM a -> r)
        -> (a -> ScpM' (Res Res'))
        -> ScpM' Res'
        -}
    bfs at k = bfs' [Candidate at k (\at' -> bfs at' k)]
    
    bfs' :: [Candidate] -> ScpM' Res'
    bfs' (Candidate (Lifted (mx :: ScpM a)) k rb:cands) = do
        r <- unContT mx k
        case r of
            Done x     -> return x
            Res at' k' -> bfs' (cands ++ [Candidate at' k' (\at' -> bfs at' k')])
    bfs' (Candidate (Pure x) k rb:cands) = do
        r <- k x
        case r of
            Done x     -> return x
            Res at' k' -> bfs' (cands ++ [Candidate at' k' (\at' -> bfs at' k')])
    bfs' (Candidate (Star (Pure f) (Pure x)) k rb:cands) = do
        r <- k (f x)
        case r of
            Done x     -> return x
            Res at' k' -> bfs' (cands ++ [Candidate at' k' (\at' -> bfs at' k')])
    bfs' (Candidate (Star fat xat) k rb:cands) = do
        bfs' (cands ++ [Candidate xat (\x -> rb (Star fat (Pure x))) (\xat -> rb (Star fat xat)),
                        Candidate fat (\f -> rb (Star (Pure f) xat)) (\fat -> rb (Star fat xat))])
    
    {-
    bfs' :: [Candidate' b] -> (b -> ScpM' (Res Res')) -> ScpM' Res'
    bfs' (Candidate' (Lifted mx)  rb cont:cs) contt = do
        r <- unContT mx cont
        case r of Done x -> bfs (rb (Pure x)) contt
                  Res at cont -> undefined
    --bfs' (Candidate' (Pure x)     rb cont:cs) contt = s
    bfs' (Candidate' (Star mf mx) rb cont:cs) contt = bfs' (cs ++ [Candidate' mf (\f -> cont (f `Star` mx)) (\f -> return (Res (fmap f mx) cont)),
                                                                   Candidate' mx (\x -> cont (mf `Star`x))  (\x -> return (Res (fmap ($ x) mf) cont))]) contt
    -}

data Candidate = forall a. Candidate {
    focus :: ApplicativeTree ScpM a,
    finished :: a -> ScpM' (Res Res'),                     -- If focus reduced to pure value
    rebuild  :: ApplicativeTree ScpM a -> ScpM' (Res Res') -- If focus still a tree of some sort
  }

--data Candidate = forall a. Candidate Int (ApplicativeTree ScpM a) (a -> ScpM' (Res Res'))
--data Candidate' b = forall a. Candidate' (ApplicativeTree ScpM a) (ApplicativeTree ScpM a -> ApplicativeTree ScpM b) (a -> ScpM' (Res Res'))
--data Candidate2 = forall a. Candidate2 (ScpM a) (a -> ScpM' (Res Res'))

{-
runScpM :: ScpM Res' -> Res'
runScpM mx = runScpM' $ unContT mx (\b -> return (Res (Pure b) return)) >>= combine
  where
    combine :: Res -> ScpM' Res'
    combine (Res comps cont) = combineChoice comps cont

    combineChoice :: ApplicativeTree ScpM a -> (a -> ScpM' Res) -> ScpM' Res'
    combineChoice (Pure x)           cont = cont x >>= combine
    combineChoice (Star compf compx) cont = combineChoice compf $ \f -> do
        combineChoice compx $ \x -> do
            return (f x)
    
    -- combineChoice (Star compf compx) cont = do
    --   r <- unContT compf (return . flip Res return)
    --   case r of
    --       Res (Pure f) cont' -> combineChoice compx (\x -> cont (f x))
    --       
    --       
    --       -- Effects in breadth-first order:
    --       --Choice comps' cont' -> combine (Choice (comps ++ comps') (\bs -> case comps  `splitBy` bs of (bs, bs') -> cont' bs' >>= \r -> combine r >>= \b -> cont (b : bs)))
    --       -- Effects in depth-first order:
    --       Choice comps' cont' -> combineChoice (comps' ++ comps) (\bs -> case comps' `splitBy` bs of (bs', bs) -> cont' bs' >>= \r -> combine r >>= \b -> cont (b : bs))
-}

runScpM' :: ScpM' a -> a
runScpM' mx = snd (unScpM' mx 0)


class Monad m => MonadNext m where
    next :: m Int

instance MonadNext ScpM' where
    next = ScpM' (\s -> (s + 1, s))

instance MonadNext m => MonadNext (ContT r m) where
    next = ContT (\c -> next >>= c)


class Monad m => MonadChoice m where
    choice :: m (a -> b) -> m a -> m b

instance MonadChoice ScpM where
    choice mf mx = ContT $ \c -> return (Res (Lifted mf `Star` Lifted mx) c)


choicePair :: MonadChoice m => m a -> m b -> m (a, b)
choicePair ma mb = return (,) `choice` ma `choice` mb


bitsToNumber :: [Bool] -> Int
bitsToNumber = foldr (\b acc -> acc * 2 + if b then 1 else 0) 0

tHRESHOLD :: Int
tHRESHOLD = 3


data Tree a = Tree a [Tree a]
            deriving (Show)

tree :: [()] -> ScpM Res'
tree n | length n > tHRESHOLD = next >>= \i -> return (Tree i [])
       | otherwise            = next >>= \i -> choicePair (tree (() : n)) (tree (() : n)) >>= \(xs, ys) -> return (Tree i [xs, ys])


main :: IO ()
main = print (runScpM (tree [()]))


{-# LANGUAGE TypeSynonymInstances #-}
import Control.Arrow (first)
import Control.Monad


newtype Cont res a = Cont { unCont :: (a -> res) -> res }

instance Functor (Cont res) where
    fmap f m = Cont $ \c -> unCont m (c . f)

instance Monad (Cont res) where
    return x = Cont ($ x)
    m >>= fm = Cont $ \c -> unCont m $ \x -> unCont (fm x) c

callCC :: ((a -> Cont res b) -> Cont res a) -> Cont res a
callCC f = Cont $ \c -> unCont (f (\a -> Cont $ \_ -> c a)) c


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


type ScpM = ContT Res ScpM'
data Res = Choice { resComps :: [ScpM Res'], resCont :: [Res'] -> ScpM' Res }
         | Done Res'
type Res' = Tree Int

runScpM :: ScpM Res' -> Res'
runScpM mx = runScpM' $ unContT mx (return . Done) >>= combine
  where
    combine :: Res -> ScpM' Res'
    combine (Done b)            = return b
    combine (Choice comps cont) = combineChoice comps cont

    combineChoice :: [ScpM Res'] -> ([Res'] -> ScpM' Res) -> ScpM' Res'
    combineChoice []           cont = cont [] >>= combine
    combineChoice (comp:comps) cont = do
      r <- unContT comp (return . Done)
      case r of
          Done b              -> combineChoice comps (\bs -> cont (b:bs))
          -- Effects in breadth-first order:
          --Choice comps' cont' -> combine (Choice (comps ++ comps') (\bs -> case comps  `splitBy` bs of (bs, bs') -> cont' bs' >>= \r -> combine r >>= \b -> cont (b : bs)))
          -- Effects in depth-first order:
          Choice comps' cont' -> combineChoice (comps' ++ comps) (\bs -> case comps' `splitBy` bs of (bs', bs) -> cont' bs' >>= \r -> combine r >>= \b -> cont (b : bs))

runScpM' :: ScpM' a -> a
runScpM' mx = snd (unScpM' mx 0)


class Monad m => MonadNext m where
    next :: m Int

instance MonadNext ScpM' where
    next = ScpM' (\s -> (s + 1, s))

instance MonadNext m => MonadNext (ContT r m) where
    next = ContT (\c -> next >>= c)


class Monad m => MonadChoice m where
    choice :: [m Res'] -> m [Res']

instance MonadChoice ScpM where
    choice mxs = ContT $ \c -> return (Choice mxs c)

--done :: [Res'] -> ScpM a
--done bs = ContT $ \_ -> return (Done bs)


splitBy :: [b] -> [a] -> ([a], [a])
splitBy []     xs     = ([], xs)
splitBy (_:ys) (x:xs) = first (x:) $ splitBy ys xs



bitsToNumber :: [Bool] -> Int
bitsToNumber = foldr (\b acc -> acc * 2 + if b then 1 else 0) 0

tHRESHOLD :: Int
tHRESHOLD = 3


data Tree a = Tree a [Tree a]
            deriving (Show)

tree :: [()] -> ScpM Res'
tree n | length n > tHRESHOLD = next >>= \i -> return (Tree i [])
       | otherwise            = next >>= \i -> choice [tree (() : n), tree (() : n)] >>= \[xs, ys] -> return (Tree i [xs, ys])



main :: IO ()
main = print (runScpM (tree [()]))

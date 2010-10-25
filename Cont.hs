{-# LANGUAGE TypeSynonymInstances, Rank2Types #-}
import Control.Arrow (first)
import Control.Monad

import Data.Maybe


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


data CompTree a = Branch (CompTree a) (CompTree a)
                | Leaf a

mkCompTree :: [a] -> Maybe (CompTree a)
mkCompTree [] = Nothing
mkCompTree [x] = Just (Leaf x)
mkCompTree xs  = case mb_t1 of Nothing -> mb_t2; Just t1 -> case mb_t2 of Nothing -> Just t1; Just t2 -> Just (Branch t1 t2)
  where (xs1, xs2) = splitAt (length xs `div` 2) xs
        mb_t1 = mkCompTree xs1
        mb_t2 = mkCompTree xs2

flattenCompTree :: CompTree a -> [a]
flattenCompTree (Leaf x) = [x]
flattenCompTree (Branch t1 t2) = flattenCompTree t1 ++ flattenCompTree t2

instance Functor CompTree where
    fmap f (Leaf x)       = Leaf (f x)
    fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)

compTreeLeftmost :: CompTree a -> (a, a -> CompTree a)
compTreeLeftmost (Leaf x)       = (x, Leaf)
compTreeLeftmost (Branch t1 t2) = (x, \x' -> Branch (rb x') t2)
  where (x, rb) = compTreeLeftmost t1

compTreeLeftmost' :: CompTree a -> (a, Either (CompTree a, b -> CompTree b -> CompTree b) (b -> CompTree b))
compTreeLeftmost' (Leaf x)       = (x, Right Leaf)
compTreeLeftmost' (Branch t1 t2) = (x, Left $ case ei of Left (t', rb) -> (Branch t' t2, \y (Branch t' t2) -> Branch (rb y t') t2)
                                                         Right rb      -> (t2,           \y t2             -> Branch (rb y)    t2))
  where (x, ei) = compTreeLeftmost' t1


type ScpM = ContT Res ScpM'
data Res = Choice { resComps :: CompTree (ScpM Res'), resCont :: Maybe (CompTree Res') -> ScpM' Res }
         | Done Res'
type Res' = Tree Int

runScpM :: ScpM Res' -> Res'
runScpM mx = runScpM' $ unContT mx (return . Done) >>= combine
  where
    combine :: Res -> ScpM' Res'
    combine (Done b)            = return b
    combine (Choice comps cont) = combineChoice (Just comps) cont

    combineChoice :: Maybe (CompTree (ScpM Res')) -> (Maybe (CompTree Res') -> ScpM' Res) -> ScpM' Res'
    combineChoice Nothing  cont = cont Nothing >>= combine
    combineChoice (Just t) cont = do
      let (comp, ei) = compTreeLeftmost' t
      r <- unContT comp (return . Done)
      case r of
          Done b              -> combineChoice (case ei of Left (comps, _) -> Just comps; Right _ -> Nothing) (case ei of Left (_, rb) -> \(Just bs) -> cont (Just (rb b bs)); Right rb -> \Nothing -> cont (Just (rb b)))
          -- Effects in breadth-first order:
          --Choice comps' cont' -> combineChoice (case ei of Left (comps, _) -> Just (Branch comps comps'); Right _ -> Just comps') (case ei of Left (_, rb) -> \(Just (Branch bs bs')) -> cont' (Just bs') >>= \r -> combine r >>= \b -> cont (Just (rb b bs)); Right rb -> \(Just bs') -> cont' (Just bs') >>= \r -> combine r >>= \b -> cont (Just (rb b)))
          -- Effects in depth-first order:
          Choice comps' cont' -> combineChoice (case ei of Left (comps, _) -> Just (Branch comps' comps); Right _ -> Just comps') (case ei of Left (_, rb) -> \(Just (Branch bs' bs)) -> cont' (Just bs') >>= \r -> combine r >>= \b -> cont (Just (rb b bs)); Right rb -> \(Just bs') -> cont' (Just bs') >>= \r -> combine r >>= \b -> cont (Just (rb b)))

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
    choice mxs = ContT $ \c -> return (Choice (fromJust (mkCompTree mxs)) (\(Just ress') -> c (flattenCompTree ress')))

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

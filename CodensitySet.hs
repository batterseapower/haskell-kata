{-# LANGUAGE RankNTypes #-}
import Control.Applicative

import Data.Set (Set)
import qualified Data.Set as S


newtype CodensityOrd m a = CodensityOrd { runCodensityOrd :: forall b. Ord b => (a -> m b) -> m b }

-- liftCodensityOrd :: Monad m => m a -> CodensityOrd m a
-- liftCodensityOrd m = CodensityOrd ((>>=) m)
-- 
-- lowerCodensityOrd :: (Ord a, Monad m) => CodensityOrd m a -> m a
-- lowerCodensityOrd m = runCodensityOrd m return

instance Functor (CodensityOrd f) where
    fmap f m = CodensityOrd (\k -> runCodensityOrd m (k . f))

instance Applicative (CodensityOrd f) where
    pure x = CodensityOrd (\k -> k x)
    mf <*> mx = CodensityOrd (\k -> runCodensityOrd mf (\f -> runCodensityOrd mx (\x -> k (f x))))

instance Monad (CodensityOrd f) where
    return = pure
    m >>= k = CodensityOrd (\c -> runCodensityOrd m (\a -> runCodensityOrd (k a) c))


liftSet :: Ord a => Set a -> CodensityOrd Set a
liftSet m = CodensityOrd (bind m)
    where bind :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
          mx `bind` fxmy = S.fold (\x my -> fxmy x `S.union` my) S.empty mx

lowerSet :: Ord a => CodensityOrd Set a -> Set a
lowerSet m = runCodensityOrd m S.singleton


main = print $ lowerSet $ monadicPlus (liftSet $ S.fromList [1, 2, 3]) (liftSet $ S.fromList [1, 2, 3])

monadicPlus :: Monad m => m Int -> m Int -> m Int
monadicPlus mx my = do
    x <- mx
    y <- my
    return (x + y)

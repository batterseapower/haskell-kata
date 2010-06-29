{-# LANGUAGE Rank2Types #-}

import Control.Monad


newtype CodensityPlus p a = CodensityPlus { runCodensityPlus :: forall b. (a -> p b -> p b) -> p b -> p b }

liftCodensityPlus :: MonadPlus p => p a -> CodensityPlus p a
liftCodensityPlus m = CodensityPlus (\fmsuc mfai -> m >>= (\x -> fmsuc x mfai))

lowerCodensityPlus :: MonadPlus p => CodensityPlus p a -> p a
lowerCodensityPlus m = runCodensityPlus m (\x mx -> return x `mplus` mx) mzero

instance Functor (CodensityPlus p) where
    fmap f m = CodensityPlus (\fmsuc mfai -> runCodensityPlus m (fmsuc . f) mfai)

instance Monad (CodensityPlus p) where
    return x = CodensityPlus (\fmsuc mfai -> fmsuc x mfai)
    mx >>= fxmy = CodensityPlus (\fmsuc mfai -> runCodensityPlus mx (\x mfai -> runCodensityPlus (fxmy x) fmsuc mfai) mfai)

instance MonadPlus (CodensityPlus p) where
    mzero = CodensityPlus (\_fmsuc mfai -> mfai)
    m1 `mplus` m2 = CodensityPlus (\fmsuc mfai -> runCodensityPlus m1 fmsuc (runCodensityPlus m2 fmsuc mfai))


{-# NOINLINE interpret #-}
interpret :: [a] -> CodensityPlus [] a
interpret = liftCodensityPlus

{-# NOINLINE reify #-}
reify :: CodensityPlus [] a -> [a]
reify = lowerCodensityPlus


{-# RULES "reify/interpret" forall xs. interpret (reify xs) = xs #-}


{-# INLINE mapL #-}
mapL :: (a -> b) -> [a] -> [b]
mapL f xs = reify (mapD f (interpret xs))

{-# INLINE mapD #-}
mapD :: Monad m => (a -> b) -> m a -> m b
mapD f mx = do
    x <- mx
    return (f x)

{-# INLINE concatMapL #-}
concatMapL f xs = reify (concatMapD (interpret . f) (interpret xs))

{-# INLINE concatMapD #-}
concatMapD :: Monad m => (a -> m b) -> m a -> m b
concatMapD = flip (>>=)

{-# INLINE enumFromToL #-}
enumFromToL :: Int -> Int -> [Int]
enumFromToL x y = reify (enumFromToD x y)

{-# INLINE enumFromToD #-}
{-# SPECIALISE enumFromToD :: Int -> Int -> CodensityPlus p Int #-}
enumFromToD :: MonadPlus m => Int -> Int -> m Int
enumFromToD x y | x > y     = mzero
                | otherwise = return x `mplus` enumFromToD (x + 1) y


main :: IO ()
main = do
    print (reify (interpret ([1..10] :: [Int])))
    print (reify (interpret (enumFromToL 1 10 :: [Int])))
    print (mapL (+1) (mapL (+2) (enumFromToL 2 10)) :: [Int])
    print (concatMapL (\y -> return (y+4) `mplus` return (y+5)) (concatMapL (\x -> return (x+2) `mplus` return (x+3)) (enumFromToL 2 10)) :: [Int])
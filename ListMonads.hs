import Control.Monad
import Data.Maybe


newtype DfM a = DfM { unDfM :: [a] }

instance Functor DfM where
    fmap f = DfM . fmap f . unDfM

instance Monad DfM where
    return x = DfM [x]
    mx >>= fxmy = join $ fmap fxmy mx
      where
        join :: DfM (DfM a) -> DfM a
        join = DfM . dfs . map unDfM . unDfM
        
        dfs :: [[a]] -> [a]
        dfs = concat
    fail _ = mzero

instance MonadPlus DfM where
    mzero = DfM []
    mx `mplus` my = DfM $ unDfM mx ++ unDfM my


-- 1) Left-identity
--  return a >>= f == f a
-- <==>
--     concat $ fmap f [a]
--  == concat [f a]
--  == f a
--
-- 2) Right-identity
--  m >>= return == m
-- <==>
--     concat $ fmap (\x -> [x]) m
--  == m
--
-- 3) Associativity
--  (m >>= f) >>= g == m >>= (\x -> f x >>= g)
-- <==>
--     concat (fmap g (concat (fmap f m)))
--  == ???
--  == concat (fmap (\x -> concat (fmap g (f x))) m)

newtype BfM a = BfM { unBfM :: [a] }

instance Functor BfM where
    fmap f = BfM . fmap f . unBfM

instance Monad BfM where
    return x = BfM [x]
    mx >>= fxmy = join $ fmap fxmy mx
      where
        join :: BfM (BfM a) -> BfM a
        join = BfM . bfs . map unBfM . unBfM
        
        bfs :: [[a]] -> [a]
        bfs []  = []
        bfs xss = ys ++ bfs yss
          where (ys, yss) = unzip $ mapMaybe unconsMaybe xss
                unconsMaybe [] = Nothing
                unconsMaybe (x:xs) = Just (x, xs)
    fail _ = mzero

instance MonadPlus BfM where
    mzero = BfM []
    mx `mplus` my = BfM $ unBfM mx ++ unBfM my



newtype OmegaM a = OmegaM { unOmegaM :: [a] }

instance Functor OmegaM where
    fmap f = OmegaM . fmap f . unOmegaM

instance Monad OmegaM where
    return x = OmegaM [x]
    mx >>= fxmy = join $ fmap fxmy mx
      where
        join :: OmegaM (OmegaM a) -> OmegaM a
        join = OmegaM . diagonal . map unOmegaM . unOmegaM
        
        -- | This is the hinge algorithm of the Omega monad,
        -- exposed because it can be useful on its own.  Joins 
        -- a list of lists with the property that for every i j 
        -- there is an n such that @xs !! i !! j == diagonal xs !! n@.
        -- In particular, @n <= (i+j)*(i+j+1)/2 + j@.
        diagonal :: [[a]] -> [a]
        diagonal = concat . stripe
          where
            stripe [] = []
            stripe ([]:xss) = stripe xss
            stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)

            zipCons [] ys = ys
            zipCons xs [] = map (:[]) xs
            zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys
    fail _ = mzero

instance MonadPlus OmegaM where
    mzero = OmegaM []
    mx `mplus` my = OmegaM $ unOmegaM mx ++ unOmegaM my




mpluses :: MonadPlus m => [m a] -> m a
mpluses = foldr mplus mzero


main = do
    print $ unDfM    $ liftM2 (,) (mpluses [return x | x <- [1..5]]) (mpluses [return x | x <- [-1,-2..(-5)]])
    print $ unBfM    $ liftM2 (,) (mpluses [return x | x <- [1..5]]) (mpluses [return x | x <- [-1,-2..(-5)]])
    print $ unOmegaM $ liftM2 (,) (mpluses [return x | x <- [1..5]]) (mpluses [return x | x <- [-1,-2..(-5)]])

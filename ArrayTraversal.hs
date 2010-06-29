{-# LANGUAGE GADTs, Rank2Types, TupleSections #-}
import Control.Applicative
import Control.Arrow ((&&&), (***))

import Data.List
import Data.Traversable


class WriterMonad m where
    tell :: Int -> m ()


newtype DfM a = DfM { unDfM :: ([Int], a) }

instance Functor DfM where
    fmap f mx = pure f <*> mx

instance Applicative DfM where
    pure x = DfM ([], x)
    mf <*> mx = case unDfM mf of (told1, f) -> case unDfM mx of (told2, x) -> DfM (told1 ++ told2, f x)

instance Monad DfM where
    return x = DfM ([], x)
    mx >>= fxmy = case unDfM mx of (told1, x) -> case unDfM (fxmy x) of (told2, y) -> DfM (told1 ++ told2, y)

instance WriterMonad DfM where
    tell x = DfM ([x], ())


-- newtype BfM a = BfM { unBfM :: [([Int], a)] }
-- 
-- instance Functor BfM where
--     fmap f mx = pure f <*> mx
-- 
-- instance Applicative BfM where
--     pure x = BfM [([], x)]
--     mf <*> mx = BfM [(told1 ++ told2, f x) | (told1, f) <- unBfM mf, (told2, x) <- unBfM mx]
-- 
-- instance Monad BfM where
--     return x = BfM [([], x)]
--     mx >>= fxmy = join (fmap fxmy mx) -- BfM [(told1 ++ told2, y) | (told1, x) <- unBfM mx, (told2, y) <- unBfM (fxmy x)]
--       where
--         join :: BfM (BfM a) -> BfM a
--         join = BfM . (\(told, ys) -> map (told,) ys) . (concat *** concat) . map unzip . map unBfM . unBfM
-- 
-- instance WriterMonad BfM where
--     tell t = BfM [([t], ())]


bitsToNumber :: [Bool] -> Int
bitsToNumber = foldr (\b acc -> acc * 2 + if b then 1 else 0) 0

tHRESHOLD :: Int
tHRESHOLD = 4

tree :: (Applicative m, Monad m, WriterMonad m) => [Bool] -> m Int
tree n | length n > tHRESHOLD = return 1
       | otherwise            = tell (bitsToNumber n) >> traverse tree [False : n, True : n] >>= \[n1, n2] -> return (n1 + n2)



main :: IO ()
main = do
    print $ unDfM $ tree [True]
    --print $ ((concatMap fst &&& map snd) . unBfM) $ tree [True]

-- Depth-first traversal:   ([1,2,4,8,9,5,10,11,3,6,12,13,7,14,15],16)
-- Breadth-first traversal: ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],16)


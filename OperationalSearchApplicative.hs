{-# LANGUAGE GADTs, TypeSynonymInstances #-}
import Control.Applicative
import Control.Monad

import Data.Traversable

import Debug.Trace


class Monad m => MonadSuspend m where
    suspend :: String -> m Int



data Program instr a where
    Return :: a -> Program instr a
    Ap     :: Program instr (a -> b) -> Program instr a -> Program instr b
    Bind   :: Program instr b -> (b -> Program instr a) -> Program instr a
    Instr  :: instr a -> Program instr a


instance Applicative (Program instr) where
    pure   = return
    (<*>)  = Ap

instance Functor (Program instr) where
    fmap   = liftM

instance Monad (Program instr) where
    return = Return
    (>>=)  = Bind


data ProgramView instr a where
    ReturnView :: a -> ProgramView instr a
    BindView   :: instr b -> (b -> Program instr a ) -> ProgramView instr a

view :: Program instr a -> ProgramView instr a
view (Return x)              = ReturnView x
view ((Return f)   `Ap`   g) = view (g x)
view (())
view ((Return x)   `Bind` g) = view (g x)
view ((m `Bind` g) `Bind` h) = view (m `Bind` (\x -> g x `Bind` h))
view ((Instr i)    `Bind` g) = i `BindView` g
view (Instr i)               = i `BindView` Return


-- newtype JumpM a = 
-- 
-- instance Functor JumpM where
--     fmap = liftM
-- 
-- instance Applicative JumpM where
--     pure = return
--     mf <*> mx = ...
-- 
-- instance Monad JumpM where
--     return = ...
--     mx >>= fxmy = ...


data JumpI a where
    Suspend :: String -> JumpI Int

type JumpM = Program JumpI

instance MonadSuspend JumpM where
    suspend = Instr . Suspend



runJumpM :: JumpM a -> a
runJumpM = go id
  where
    go = undefined
    -- go :: [(Int -> )] -> (a -> b) -> JumpM a -> b
    -- go others k (Return x)          = k x
    -- go others k (mf `Ap` mx)        = go (\f -> go (\x -> k (f x)) mx) mf
    -- go others k (mx `Bind` fxmy)    = go (go k . fxmy) mx
    -- go others k (Instr (Suspend s)) = trace s $ k 1



bitsToNumber :: [Bool] -> Int
bitsToNumber = foldr (\b acc -> acc * 2 + if b then 1 else 0) 0

tHRESHOLD :: Int
tHRESHOLD = 4

tree :: (Applicative m, MonadSuspend m) => [Bool] -> m Int
tree n | length n > tHRESHOLD = return 1
       | otherwise            = suspend ("Suspension point: " ++ show (bitsToNumber n)) >>= \_ -> traverse tree [False : n, True : n] >>= \[n1, n2] -> return (n1 + n2)



main :: IO ()
main = print $ runJumpM $ tree [True]
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Delcont where

import Control.Monad


newtype Cont r a = Cont { unCont :: (a -> r) -> r }

runCont :: Cont r r -> r
runCont mx = unCont mx id

instance Monad (Cont r) where
    return x = Cont $ \k -> k x
    mx >>= fxmy = Cont $ \k -> unCont mx $ \x -> unCont (fxmy x) k

callCC :: ((forall b. a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> unCont (f (\x -> Cont $ \_k' -> k x)) k

shift :: ((forall r'. a -> Cont r' r) -> Cont r r) -> Cont r a
shift f = Cont $ \k -> unCont (f (\x -> Cont $ \k' -> k' (k x))) id

reset :: Cont a a -> Cont r a
reset mx = Cont $ \k -> k (unCont mx id)

contTest1 = runCont (callCC (\k -> k 1337) :: Cont Int Int)

--   ((1 + 1) + (2 + 1)) + 10
-- = 15
contTest2 = runCont (liftM2 (+) (reset (liftM (1+) (shift $ \k -> liftM2 (+) (k 1) (k 2))))
                                (reset (liftM (1+) (shift $ \k -> return 10))))


type State r s a = Cont (s -> r) a

-- We don't need the "reset"s in our translations of the standard definitions in Haskell,
-- since the Cont monad we implemented above has an implicit top-level reset which will do nicely

runState :: State a s a -> s -> a
runState mx s = runCont (liftM (\x _ -> x) mx) s

writeCell :: s -> State r s ()
writeCell s' = shift $ \f -> return $ \_ -> runCont (f ()) s'

readCell :: State r s s
readCell = shift $ \f -> return $ \s -> runCont (f s) s

stateTest1 = runState readCell 1337
stateTest2 = runState (do { writeCell 11; x <- readCell; writeCell (x + 1); readCell }) 10

-- I don't think you can implement this monad in terms of callCC, this just causes writeCell to abort:
brokenWriteCell :: s -> State r s ()
brokenWriteCell _s' = callCC $ \f -> f ()

{-# LANGUAGE RankNTypes #-}
import Text.PrettyPrint.HughesPJClass

import System.IO.Unsafe

import Control.Monad.State


type UniqM = State [Int]

uniqSupply :: [Int]
uniqSupply = [0..]

runUniq :: UniqM a -> a
runUniq = flip evalState uniqSupply

unique :: UniqM Int
unique = get >>= \(x:ss) -> put ss >> return x


-- -- Codensity is the "mother of all monads":
-- 
-- -- return :: forall b. b -> m b
-- -- (>>=)  :: forall a. m a -> (forall b. (a -> m b) -> m b)
-- --
-- -- return a >>= f = f a                      -- Left identity
-- -- m >>= return = m                          -- Right identity
-- -- (m >>= f) >>= g = m >>= (\x -> f x >>= g) -- Associativity
-- newtype Codensity m a = Codensity { runCodensity :: forall b. (a -> m b) -> m b }
-- 
-- liftCodensity :: Monad m => m a -> Codensity m a
-- liftCodensity m = Codensity ((>>=) m)
-- 
-- lowerCodensity :: Monad m => Codensity m a -> m a
-- lowerCodensity m = runCodensity m return
-- 
-- instance Functor (Codensity f) where
--     fmap f m = Codensity (\k -> runCodensity m (k . f))
-- 
-- instance Applicative (Codensity f) where
--     pure = return
--     mf <*> mx = Codensity (\k -> runCodensity mf (\f -> runCodensity mx (\x -> k (f x))))
-- 
-- instance Monad (Codensity f) where
--     return x = Codensity (\k -> k x)
--     m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))


instance Pretty MonadSyn where
    pPrint = runUniq . pPrintMonadSyn

pPrintMonadSyn (Return e)     = return $ text "return" <+> text e
pPrintMonadSyn (Bind mx fxmy) = do
    x <- fmap (\i -> "x" ++ show i) unique
    liftM2 (\dmx dmy -> text "let" <+> text x <+> text "=" <+> dmx $$ text "in" <+> dmy) (pPrintMonadSyn mx) (pPrintMonadSyn (fxmy x))
pPrintMonadSyn (Foreign e) = return $ text e


type Term = String

data MonadSyn = Return Term
              | Bind MonadSyn (String -> MonadSyn)
              | Foreign String

normalise :: MonadSyn -> MonadSyn
normalise m = go m Return
  where
    go :: MonadSyn -> (String -> MonadSyn) -> MonadSyn
    go (Return x)  k = k x
    go (Bind m k)  c = go m (\a -> go (k a) c)
    
    go (Foreign x) k = Bind (Foreign x) k


non_normalised = Bind (Return "10") $ \x ->
                 Bind (Bind (Bind (Foreign "get") (\y -> Return y)) (\z -> Bind (Foreign ("put " ++ x)) (\_ -> Return z))) $ \w ->
                 Return w

main = do
    putStrLn "== Before"
    print $ pPrint non_normalised
    
    putStrLn "== After"
    print $ pPrint $ normalise non_normalised

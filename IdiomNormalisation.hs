{-# LANGUAGE RankNTypes #-}
import Text.PrettyPrint.HughesPJClass

import Data.Supply

import System.IO.Unsafe

import Control.Monad
import Control.Monad.State


type UniqM = State (Supply Int)

uniqSupply :: Supply Int
uniqSupply = unsafePerformIO $ newSupply 0 (+1)

runUniq :: UniqM a -> a
runUniq = flip evalState uniqSupply

unique :: UniqM Int
unique = get >>= \s -> let (s1, s2) = split2 s in put s2 >> return (supplyValue s1)


-- -- Thingy is the "mother of all idioms":
-- 
-- -- pure   :: forall b. b -> i b
-- -- (<**>) :: forall a. i a -> (forall b. i (a -> b) -> i b)
-- --
-- -- pure id <*> v = v                            -- Identity
-- -- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
-- -- pure f <*> pure x = pure (f x)               -- Homomorphism
-- -- u <*> pure y = pure ($ y) <*> u              -- Interchange
-- --
-- -- v                 ==> pure id <*> v              -- Identity
-- -- u <*> (v <*> w)   ==> pure (.) <*> u <*> v <*> w -- Composition
-- -- pure f <*> pure x ==> pure (f x)                 -- Homomorphism
-- -- u <*> pure y      ==> pure ($ y) <*> u           -- Interchange
-- newtype Thingy i a = Thingy { runThingy :: forall b. Yoneda i (a -> b) -> Yoneda i b }
-- 
-- liftThingy :: Applicative i => i a -> Thingy i a
-- liftThingy i = Thingy (liftYoneda . (<**>) i . lowerYoneda)
-- 
-- lowerThingy :: Applicative i => Thingy i a -> i a
-- lowerThingy i = lowerYoneda $ runThingy i (liftYoneda (pure id))
-- 
-- instance Functor i => Functor (Thingy i) where
--     fmap f m = Thingy $ runThingy m . fmap (. f)
-- 
-- instance Applicative (Thingy i) where
--     pure x    = Thingy $ \m -> Yoneda (\k -> runYoneda m (k . ($ x)))
--     mf <*> mx = Thingy $ \m -> runThingy mx (runThingy mf (Yoneda (\k -> runYoneda m (k . (.)))))


instance Pretty IdiomSyn where
    pPrint (Pure e)    = text "pure" <+> parens (text e)
    pPrint (Ap mf mx)  = pPrint mf <+> text "<*>" <+> parens (pPrint mx)
    pPrint (Foreign e) = text e


type Term = String

data IdiomSyn = Pure Term
              | Ap IdiomSyn IdiomSyn
              | Foreign String

normalise :: IdiomSyn -> IdiomSyn
normalise m = go m (\tt -> Pure (tt "id")) id
  where
    --    i a      -> forall b. (forall c. ((a -> b) -> c) -> i c)      -> (forall d. (b -> d)       -> i d)
    go :: IdiomSyn -> (                    (Term -> Term)  -> IdiomSyn) -> (          (Term -> Term) -> IdiomSyn)
    --go (Pure x)    k = \tt -> k (tt . (\t -> "(" ++ t ++ ") " ++ x))
    --go (Ap mf mx)  k = \tt -> go mx (go mf (k $ tt . (\t -> "(.) (" ++ t ++ ")")))
    
    go (Pure x)    m = \k -> m (k . (\t -> t ++ " (" ++ x ++ ")"))
    go (Ap mf mx)  m = go mx (go mf (\k -> m (k . (\t -> "(.) (" ++ t ++ ")")))) 
    go (Foreign e) m = \k -> m (\t -> "(.) (\\x -> " ++ k "x" ++ ") (" ++ t ++ ")") `Ap` Foreign e
     -- e :: i a
     -- m :: (forall c. ((a -> b) -> c) -> i c)
     -- k :: (b -> d)
     --
     -- WANT:
     --  c =inst=> (a -> d)
     -- SO:
     --  m :: ((a -> b) -> (a -> d)) -> i (a -> d)
     --  (\t -> "(.) (\x -> " ++ k "x" ++ ")") :: ((a -> b) -> (a -> d))


non_normaliseds = [
    -- Composition
    Foreign "launchMissiles" `Ap` (Foreign "obtainLaunchCode" `Ap` Foreign "getAuthorization"),
    Pure "launchMissiles'" `Ap` (Pure "obtainLaunchCode'" `Ap` Pure "getAuthorization'"),
    -- Homomorphism
    Pure "f" `Ap` Pure "x",
    -- Interchange
    Foreign "launchMissiles" `Ap` Pure "1337"
  ]

main = forM_ non_normaliseds $ \non_normalised -> do
    putStrLn "== Before"
    print $ pPrint non_normalised
    
    putStrLn "== After"
    print $ pPrint $ normalise non_normalised

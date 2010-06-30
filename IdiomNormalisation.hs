{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
import Text.PrettyPrint.HughesPJClass

import System.IO.Unsafe

import Control.Monad
import Control.Monad.State


type UniqM = State [Int]

uniqSupply :: [Int]
uniqSupply = [0..]

runUniq :: UniqM a -> a
runUniq = flip evalState uniqSupply

unique :: UniqM Int
unique = get >>= \(x:ss) -> put ss >> return x


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


instance Pretty (IdiomSyn a) where
    pPrint (Pure e)    = text "pure" <+> parens (pPrint e)
    pPrint (Ap mf mx)  = pPrint mf <+> text "<*>" <+> parens (pPrint mx)
    pPrint (Foreign e) = text e

instance Pretty (Term a) where
    pPrint = runUniq . pPrintTerm

pPrintTerm :: Term a -> UniqM Doc
pPrintTerm (Lam f) = do
    x <- fmap (\i -> "x" ++ show i) unique
    d <- pPrintTerm (f (ForeignE x))
    return $ parens $ text "\\" <> text x <+> text "->" <+> d
pPrintTerm (App e1 e2) = liftM2 (\e1 e2 -> e1 <+> parens e2) (pPrintTerm e1) (pPrintTerm e2)
pPrintTerm (ForeignE e) = return $ text e


data Term a where
    Lam :: (Term a -> Term b) -> Term (a -> b)
    App :: Term (a -> b) -> Term a -> Term b
    ForeignE :: String -> Term a

data IdiomSyn a where
    Pure :: Term a -> IdiomSyn a
    Ap :: IdiomSyn (a -> b) -> IdiomSyn a -> IdiomSyn b
    Foreign :: String -> IdiomSyn a

normalise :: IdiomSyn a -> IdiomSyn a
normalise m = go m (\k -> Pure (k id)) id
  where
    --go :: forall a. IdiomSyn a
    --   -> (forall b.
    --      (forall c. ((Term a -> Term b) -> Term c) -> IdiomSyn c)
    --   -> (forall d.
    --      (Term b -> Term d)
    --   -> IdiomSyn d))
    go :: forall a b d.
          IdiomSyn a
       -> (forall c. ((Term a -> Term b) -> Term c) -> IdiomSyn c)
       -> (Term b -> Term d)
       -> IdiomSyn d
    go (Pure x)    m = \k -> m (\k' -> k (k' x))
    --go (Ap mf mx)  m = go mx (go mf (\k -> m (\k' -> k ((.) k'))))
    -- go (Ap mf mx)  m = go mx (go mf (\k -> m (k . (\t -> "(.) (" ++ t ++ ")"))))
    
    go (Ap mf mx) m = go mx (\k' -> go mf (\x -> m (\y -> x (\z -> Lam (\e -> y (z `App` e))))) (\w -> k' (w `App`)))
    -- HAVE
    --   mf :: IdiomSyn (e -> a)
    --   mx :: IdiomSyn e
    --   m :: forall f. ((Term a -> Term b) -> Term f) -> IdiomSyn f
    --   k :: Term b -> Term d
    --   go mf :: forall i.
    --            (forall g. ((Term (e -> a) -> Term i) -> Term g) -> IdiomSyn g)
    --         -> (forall h.
    --            (Term i -> Term h)
    --         -> IdiomSyn h)
    --   go mx :: forall j.
    --            (forall k. ((Term e -> Term j) -> Term k) -> IdiomSyn k)
    --         -> (forall l.
    --            (Term j -> Term l)
    --         -> IdiomSyn l)
    --
    -- GOAL
    --   undefined :: IdiomSyn d
    --   go mx (\(k' :: (Term e -> Term b) -> Term k) -> undefined :: IdiomSyn k) k :: IdiomSyn d
    --   go mx (\(k' :: (Term e -> Term b) -> Term k) -> go mf (\(x :: (Term (e -> a) -> Term (e -> b)) -> Term g) -> undefined :: IdiomSyn g) (undefined :: Term (e -> b) -> Term k) :: IdiomSyn k) k :: IdiomSyn d
    --   go mx (\(k' :: (Term e -> Term b) -> Term k) -> go mf (\(x :: (Term (e -> a) -> Term (e -> b)) -> Term g) -> m (\(y :: (Term a -> Term b)) -> undefined :: Term g) :: IdiomSyn g) (undefined :: Term (e -> b) -> Term k) :: IdiomSyn k) k :: IdiomSyn d
    --   go mx (\(k' :: (Term e -> Term b) -> Term k) -> go mf (\(x :: (Term (e -> a) -> Term (e -> b)) -> Term g) -> m (\(y :: (Term a -> Term b)) -> x (\(z :: Term (e -> a) -> Lam (\e -> y (z `App` e))))) :: IdiomSyn g) (\(w :: Term (e -> b)) -> k' (w `App`) :: Term k) :: IdiomSyn k) k :: IdiomSyn d
    --     x :: ((Term (e -> a) -> Term (e -> b)) -> Term g
    --     y :: Term a -> Term b
    --
    --     x (\(z :: Term (e -> a) -> Lam (\e -> y (z `App` e)))) :: Term g
    
    -- go (Foreign e) m = \k -> m (\t -> "(.) (\\x -> " ++ k "x" ++ ") (" ++ t ++ ")") `Ap` Foreign e
    go x@(Foreign _) m = \k -> m (\k' -> Lam (k . k')) `Ap` x
     -- HAVE
     --   Foreign e :: IdiomSyn a
     --   m :: forall f. ((Term a -> Term b) -> Term f) -> IdiomSyn f
     --   k :: Term b -> Term d
     -- 
     -- GOAL
     --   undefined :: IdiomSyn d
     --   m (\(k' :: Term a -> Term b) -> undefined :: Term (a -> d)) `Ap` Foreign e :: IdiomSyn d
     --   m (\(k' :: Term a -> Term b) -> Lam (k . k') :: Term (a -> d)) `Ap` Foreign e :: IdiomSyn d
    
     -- e :: i a
     -- m :: (forall c. ((a -> b) -> c) -> i c)
     -- k :: (b -> d)
     --
     -- WANT:
     --  c =inst=> (a -> d)
     -- SO:
     --  m :: ((a -> b) -> (a -> d)) -> i (a -> d)
     --  (\t -> "(.) (\x -> " ++ k "x" ++ ")") :: ((a -> b) -> (a -> d))


comp :: Term ((b -> c) -> (a -> b) -> (a -> c))
comp = Lam (\f -> Lam (\g -> Lam (\x -> f `App` (g `App` x))))

non_normaliseds = [
    -- Identity
    Foreign "effectful",
    Pure (Lam (\x -> x)) `Ap` Foreign "effectful",
    -- Composition
    Foreign "launchMissiles" `Ap` (Foreign "obtainLaunchCode" `Ap` Foreign "getAuthorization"),
    Pure comp `Ap` Foreign "launchMissiles" `Ap` Foreign "obtainLaunchCode" `Ap` Foreign "getAuthorization",
    Pure (ForeignE "launchMissiles'") `Ap` (Pure (ForeignE "obtainLaunchCode'") `Ap` Pure (ForeignE "getAuthorization'")),
    -- Homomorphism
    Pure (ForeignE "f") `Ap` Pure (ForeignE "x"),
    Pure (Lam (\x -> ForeignE "f" `App` x) `App` ForeignE "x"),
    -- Interchange
    Foreign "launchMissiles" `Ap` Pure (ForeignE "1337"), -- NB: demonstrates normaliser weakness. Beta-reduction introduced by normalisation!
    Pure (Lam (\x -> x `App` ForeignE "1337")) `Ap` Foreign "launchMissiles"
  ]

main = forM_ non_normaliseds $ \non_normalised -> do
    putStrLn "== Before"
    print $ pPrint non_normalised
    
    putStrLn "== After"
    print $ pPrint $ normalise non_normalised

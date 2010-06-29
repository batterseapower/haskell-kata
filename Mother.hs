{-# LANGUAGE RankNTypes, ScopedTypeVariables, GeneralizedNewtypeDeriving,
             FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad


-- Yoneda is the "mother of all functors":

-- flip fmap :: forall a. f a -> (forall b. (a -> b) -> f b)
--
-- fmap id = id                   -- Identity
-- fmap (f . g) = fmap f . fmap g -- Composition
newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda f = Yoneda (flip fmap f)

lowerYoneda :: Yoneda f a -> f a
lowerYoneda f = runYoneda f id

instance Functor (Yoneda f) where
    fmap f m = Yoneda (\k -> runYoneda m (k . f))
    -- fmap id m
    --   = Yoneda (\k -> runYoneda m (k . id))
    --   = Yoneda (\k -> runYoneda m k)
    --   = m
    --
    -- fmap (f . g) m
    --   = Yoneda (\k -> runYoneda m (k . (f . g)))
    --   = Yoneda (\k -> runYoneda m ((k . f) . g))
    --   = Yoneda (\k -> runYoneda (Yoneda (\k -> runYoneda m (k . g))) (k . f))
    --   = Yoneda (\k -> runYoneda (fmap g m) (k . f))
    --   = fmap f (fmap g m)

instance Applicative f => Applicative (Yoneda f) where
    pure = liftYoneda . pure
    mf <*> mx = liftYoneda (lowerYoneda mf <*> lowerYoneda mx)



-- Thingy is the "mother of all idioms":

-- pure   :: forall b. b -> i b
-- (<**>) :: forall a. i a -> (forall b. i (a -> b) -> i b)
--
-- pure id <*> v = v                            -- Identity
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
-- pure f <*> pure x = pure (f x)               -- Homomorphism
-- u <*> pure y = pure ($ y) <*> u              -- Interchange
--
-- v                 ==> pure id <*> v              -- Identity
-- u <*> (v <*> w)   ==> pure (.) <*> u <*> v <*> w -- Composition
-- pure f <*> pure x ==> pure (f x)                 -- Homomorphism
-- u <*> pure y      ==> pure ($ y) <*> u           -- Interchange
newtype Thingy i a = Thingy { runThingy :: forall b. Yoneda i (a -> b) -> Yoneda i b }

liftThingy :: Applicative i => i a -> Thingy i a
liftThingy i = Thingy (liftYoneda . (<**>) i . lowerYoneda)

lowerThingy :: Applicative i => Thingy i a -> i a
lowerThingy i = lowerYoneda $ runThingy i (liftYoneda (pure id))

instance Functor (Thingy i) where
    fmap f m = Thingy $ runThingy m . fmap (. f)
    -- fmap id m
    --   = Thingy (\x -> runThingy m (fmap (. id) x))
    --   = Thingy (\x -> runThingy m (fmap id x))
    --   = Thingy (\x -> runThingy m x)
    --   = m
    --
    -- fmap f (fmap g m)
    --   = Thingy (\x -> runThingy (fmap g m) (fmap (. f) x))
    --   = Thingy (\x -> runThingy (Thingy (\y -> runThingy m (fmap (. g) y))) (fmap (. f) x))
    --   = Thingy (\x -> runThingy m (fmap (. g) (fmap (. f) x)))
    --   = Thingy (\x -> runThingy m (fmap ((. g) . (. f)) x))
    --   = Thingy (\x -> runThingy m (fmap (\z -> (z . f) . g) x))
    --   = Thingy (\x -> runThingy m (fmap (\z -> z . (f . g)) x))
    --   = Thingy (\x -> runThingy m (fmap (. (f . g)) x))
    --   = fmap (f . g) m

instance Applicative (Thingy i) where
    pure x = Thingy $ \m -> Yoneda (\k -> runYoneda m (k . ($ x)))
    mf <*> mx = Thingy $ \m -> runThingy mx (runThingy mf (Yoneda (\k -> runYoneda m (k . (.)))))
    -- pure f <*> pure x
    --   = Thingy (\m -> runThingy (pure x) (runThingy (pure f) (Yoneda (\k -> runYoneda m (k . (.))))))
    --   = Thingy (\m -> runThingy (Thingy (\m -> Yoneda (\k -> runYoneda m (k . ($ x))))) (runThingy (Thingy (\m -> Yoneda (\k -> runYoneda m (k . ($ f))))) (Yoneda (\k -> runYoneda m (k . (.))))))
    --   = Thingy (\m -> (\m2 -> Yoneda (\k -> runYoneda m2 (k . ($ x)))) ((\m3 -> Yoneda (\k -> runYoneda m3 (k . ($ f)))) (Yoneda (\k -> runYoneda m (k . (.))))))
    --   = Thingy (\m -> (\m2 -> Yoneda (\k -> runYoneda m2 (k . ($ x)))) ((Yoneda (\k -> runYoneda ((Yoneda (\k -> runYoneda m (k . (.))))) (k . ($ f))))))
    --   = Thingy (\m -> (Yoneda (\k -> runYoneda ((Yoneda (\k -> runYoneda ((Yoneda (\k -> runYoneda m (k . (.))))) (k . ($ f))))) (k . ($ x)))))
    --   = Thingy (\m -> (Yoneda (\k4 -> (\k2 -> runYoneda ((Yoneda (\k3 -> runYoneda m (k3 . (.))))) (k2 . ($ f))) (k4 . ($ x)))))
    --   = Thingy (\m -> (Yoneda (\k4 -> ((runYoneda m (((k4 . ($ x)) . ($ f)) . (.)))))))
    --   = Thingy (\m -> Yoneda (\k -> runYoneda m (\i -> k (i (f x)))))
    --   = Thingy (\m -> Yoneda (\k -> runYoneda m (k . ($ (f x)))))
    --   = pure (f x)
    --
    -- pure id <*> v
    --    = Thingy (\m -> runThingy v (runThingy (pure id) (Yoneda (\k -> runYoneda m (k . (.))))))
    --    = Thingy (\m -> runThingy v ((\m -> Yoneda (\k1 -> runYoneda m (k1 . ($ id)))) (Yoneda (\k2 -> runYoneda m (k2 . (.))))))
    --    = Thingy (\m -> runThingy v ((\m -> Yoneda (\k1 -> runYoneda m ((k1 . ($ id)) . (.))))))
    --    = Thingy (\m -> runThingy v (\m -> Yoneda (\k1 -> runYoneda m ((k1 . ($ id)) . (.)))))
    --    = Thingy (\m -> runThingy v (\m -> Yoneda (\k1 -> runYoneda m (\h -> k1 (\e -> h e)))))
    --    = v
    --
    -- pure ($ y) <*> u
    --    = Thingy (\m -> runThingy u (runThingy (pure ($ y)) (Yoneda (\k -> runYoneda m (k . (.))))))
    --    = Thingy (\m -> runThingy u (runThingy (Thingy (\m -> Yoneda (\k -> runYoneda m (k . ($ ($ y)))))) (Yoneda (\k -> runYoneda m (k . (.))))))
    --    = Thingy (\m -> runThingy u ((Yoneda (\k -> runYoneda (Yoneda (\k -> runYoneda m (k . (.)))) (k . ($ ($ y)))))))
    --    = Thingy (\m -> runThingy u ((Yoneda (\k1 -> (\k2 -> runYoneda m (k2 . (.))) (k1 . ($ ($ y)))))))
    --    = Thingy (\m -> runThingy u (Yoneda (\k1 -> runYoneda m (\h -> k1 (\e -> h (e y))))))
    --
    --    = Thingy (\m -> Yoneda (\k5 -> runYoneda (runThingy u (Yoneda (\k4 -> runYoneda m (\f -> k4 (\b c -> f (b c)))))) (\d -> k5 (d y))))
    --    = Thingy (\m -> (Yoneda (\k5 -> runYoneda (runThingy u (Yoneda (\k4 -> runYoneda m (k4 . (.))))) (k5 . ($ y)))))
    --    = Thingy (\m -> runThingy (Thingy (\m -> Yoneda (\k -> runYoneda m (k . ($ y))))) (runThingy u (Yoneda (\k -> runYoneda m (k . (.))))))
    --    = Thingy (\m -> runThingy (pure y) (runThingy u (Yoneda (\k -> runYoneda m (k . (.))))))
    --    = u <*> pure y

-- Wotsit is the "mother of all categories":

-- id    :: forall c. t c c
-- (>>>) :: forall a b. t a b -> (forall c. t b c -> t a c)
--
-- id . t = t                -- Left-identity
-- t . id = t                -- Right-identity
-- t . (r . s) = (t . r) . s -- Associativity
newtype Wotsit t a b = Wotsit { runWotsit :: forall c. t b c -> t a c }

liftWotsit :: Category t => t a b -> Wotsit t a b
liftWotsit t = Wotsit ((>>>) t)

lowerWotsit :: Category t => Wotsit t a b -> t a b
lowerWotsit t = runWotsit t id

instance Category (Wotsit t) where
    -- *Strongly* reminiscent of NBE for monoids (reassociation realised by assocativity of function application)
    -- There is probably some connection between NBE and Yoneda (e.g. "Normalization and the Yoneda embedding", but
    -- can't get access to this paper electronically)
    id = Wotsit id
    t1 . t2 = Wotsit (runWotsit t2 . runWotsit t1)

-- Demonstrate that we can hoist stronger operations into Wotsit as well, given a type class context
instance Arrow t => Arrow (Wotsit t) where
    arr f = Wotsit (\k -> arr f >>> k)
    --first t1 = Wotsit (\k -> first (runWotsit t1 id) >>> k)
    first t1 = Wotsit (\k -> first (lowerWotsit t1) >>> k)


-- BiYoneda is the "mother of all BiFunctors"

-- Satisfies obvious laws
class Bifunctor u where
    bimap :: (a -> b) -> (c -> d) -> u b c -> u a d

instance Bifunctor (->) where
    bimap f g h = g . h . f

-- Every arrow is a Bifunctor:
--newtype ArrowBifunctor u a b = ArrowBifunctor { unArrowBifunctor :: u a b }
--                             deriving (Category, Arrow)
--
--instance Arrow u => Bifunctor (ArrowBifunctor u) where
--    bimap = arrowBimap

instance Arrow u => Bifunctor u where
   bimap = arrowBimap

arrowBimap :: Arrow r => (a -> b) -> (c -> d) -> r b c -> r a d
arrowBimap f g u = arr f >>> u >>> arr g


newtype BiYoneda u b c = BiYoneda { runBiYoneda :: forall a d. (a -> b) -> (c -> d) -> u a d }

liftBiYoneda :: Bifunctor u => u a b -> BiYoneda u a b
liftBiYoneda u = BiYoneda (\f' g' -> bimap f' g' u)

lowerBiYoneda :: BiYoneda u a b -> u a b
lowerBiYoneda u = runBiYoneda u id id

instance Bifunctor (BiYoneda u) where
    bimap f g u = BiYoneda (\f' g' -> runBiYoneda u (f . f') (g' . g))


-- ContraYoneda1 is the "mother of all ContraFunctor1"

-- Satisfies obvious laws
class ContraFunctor1 u where
    contrafmap1 :: (a -> b) -> u b c -> u a c

instance ContraFunctor1 (->) where
    contrafmap1 f h = h . f

-- Every arrow is a ContraFunctor1:
instance Arrow u => ContraFunctor1 u where
   contrafmap1 = arrowContrafmap1

arrowContrafmap1 :: Arrow r => (a -> b) -> r b c -> r a c
arrowContrafmap1 f u = arr f >>> u


newtype ContraYoneda1 u b c = ContraYoneda1 { runContraYoneda1 :: forall a. (a -> b) -> u a c }

liftContraYoneda1 :: ContraFunctor1 u => u a b -> ContraYoneda1 u a b
liftContraYoneda1 u = ContraYoneda1 (\f' -> contrafmap1 f' u)

lowerContraYoneda1 :: ContraYoneda1 u a b -> u a b
lowerContraYoneda1 u = runContraYoneda1 u id

instance ContraFunctor1 (ContraYoneda1 u) where
    contrafmap1 f u = ContraYoneda1 (\f' -> runContraYoneda1 u (f . f'))

-- FIXME: a bit funny. Why do I require such a strong superclass constraint?
-- I only need this for (lift/lower)ContraYoneda1Wotsit though, so not a big deal.
instance ContraFunctor1Category u => Category (ContraYoneda1 u) where
    id = ContraYoneda1 (\(k :: d -> a) -> contrafmap1 k id :: u d a) :: ContraYoneda1 u a a
    (u1 :: ContraYoneda1 u b c) . (u2 :: ContraYoneda1 u a b) = ContraYoneda1 (\(k :: d -> a) -> runContraYoneda1 u1 id . runContraYoneda1 u2 k :: u d c)

-- BiYonedaWotsit is the "mother of all BifunctorCategories"
-- NB: might be nicer to formulate this in terms of pureA rather than bimap

-- Satisfies interaction laws something like this:
--   bimap f g (u1 >>> u2) = bimap f id u1 >>> bimap id g u2
class (Bifunctor u, Category u) => BifunctorCategory u where

instance Arrow u => BifunctorCategory u where

pureA :: BifunctorCategory u => (a -> b) -> u a b
pureA f = bimap id f id

newtype BiYonedaWotsit u a b = BiYonedaWotsit { runBiYonedaWotsit :: Wotsit (BiYoneda u) a b }

instance Bifunctor (BiYonedaWotsit u) where
    --bimap f g u = BiYonedaWotsit (bimap f g (runBiYonedaWotsit u))
    bimap (f :: a -> b) (g :: c -> d) (u :: BiYonedaWotsit u b c) = BiYonedaWotsit (Wotsit (\(k :: BiYoneda u d e) -> bimap f id (runWotsit (runBiYonedaWotsit u) (bimap g id k))))

instance Category (BiYonedaWotsit u) where
    id = BiYonedaWotsit id
    u1 . u2 = BiYonedaWotsit (runBiYonedaWotsit u1 . runBiYonedaWotsit u2)

instance BifunctorCategory (BiYonedaWotsit u) where


-- ContraYoneda1Wotsit is the "mother of all ContraFunctor1Categories"

class (ContraFunctor1 u, Category u) => ContraFunctor1Category u where

instance Arrow u => ContraFunctor1Category u where

pureA1 :: ContraFunctor1Category u => (a -> b) -> u a b
pureA1 f = contrafmap1 f id

newtype ContraYoneda1Wotsit u a b = ContraYoneda1Wotsit { runContraYoneda1Wotsit :: Wotsit (ContraYoneda1 u) a b }

liftContraYoneda1Wotsit :: ContraFunctor1Category u => u a b -> ContraYoneda1Wotsit u a b
liftContraYoneda1Wotsit u = ContraYoneda1Wotsit (liftWotsit (liftContraYoneda1 u))

lowerContraYoneda1Wotsit :: ContraFunctor1Category u => ContraYoneda1Wotsit u a b -> u a b
lowerContraYoneda1Wotsit u = lowerContraYoneda1 (lowerWotsit (runContraYoneda1Wotsit u))

instance ContraFunctor1 (ContraYoneda1Wotsit u) where
    --bimap f g u = BiYonedaWotsit (bimap f g (runBiYonedaWotsit u))
    contrafmap1 f u = ContraYoneda1Wotsit (Wotsit (\k -> contrafmap1 f (runWotsit (runContraYoneda1Wotsit u) k)))

instance Category (ContraYoneda1Wotsit u) where
    id = ContraYoneda1Wotsit id
    u1 . u2 = ContraYoneda1Wotsit (runContraYoneda1Wotsit u1 . runContraYoneda1Wotsit u2)

instance ContraFunctor1Category (ContraYoneda1Wotsit u) where


-- Voldemort is the "mother of all arrows":

-- arr   :: forall c d. (c -> d) -> r c d
-- (>>>) :: forall a b. r a b -> (forall c. r b c -> r a c)
-- first :: forall a b. r a b -> (forall c. r (a, c) (b, c))
-- (***) :: forall a b. r a b -> (forall c d. r c d -> r (a, c) (b, d))
--
-- pure id                         = id                               -- Functor-identity
-- pure (g . f)                    = pure f >>> pure g                -- Functor-composition
-- first (pure f)                  = pure (f `cross` id)              -- Extension
-- first (f >>> g)                 = first f >>> first g              -- Functor
-- first f >>> pure (id `cross` g) = pure (id `cross` g) >>> first f  -- Exchange
-- first f >>> pure fst            = pure fst >>> f                   -- Unit
-- first (first f) >>> pure assoc  = pure assoc >>> first f           -- Association
--
--  where
--    f `cross` g = \(x, y) -> (f x, g y)
--    assoc (~(a, b), c) = (a, (b, c))
--newtype Voldemort r a b = Voldemort { runVoldemort :: forall c. BiYonedaWotsit r (a, c) (b, c) }
newtype Voldemort r a b = Voldemort { runVoldemort :: forall c. ContraYoneda1Wotsit r (a, c) (b, c) }

liftVoldemort :: Arrow r => r a b -> Voldemort r a b
liftVoldemort r = Voldemort (liftContraYoneda1Wotsit (first r))

lowerVoldemort :: Arrow r => Voldemort r a b -> r a b
lowerVoldemort (r :: Voldemort r a b) = arr (\x -> (x, ())) >>> lowerContraYoneda1Wotsit (runVoldemort r) >>> arr (\(x, ()) -> x)

instance Category (Voldemort r) where
    id = Voldemort id
    t1 . t2 = Voldemort (runVoldemort t1 . runVoldemort t2)

instance Arrow (Voldemort r) where
    arr f = Voldemort $ pureA1 (\(x, y) -> (f x, y))
    first (t1 :: Voldemort r a b) = Voldemort (pureA1 (\(~(a, c), d) -> (a, (c, d))) >>> runVoldemort t1 >>> pureA1 (\(b, ~(c, d)) -> ((b, c), d)))


-- Codensity is the "mother of all monads":

-- return :: forall b. b -> m b
-- (>>=)  :: forall a. m a -> (forall b. (a -> m b) -> m b)
--
-- return a >>= f = f a                      -- Left identity
-- m >>= return = m                          -- Right identity
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g) -- Associativity
newtype Codensity m a = Codensity { runCodensity :: forall b. (a -> m b) -> m b }

liftCodensity :: Monad m => m a -> Codensity m a
liftCodensity m = Codensity ((>>=) m)

lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity m = runCodensity m return

instance Functor (Codensity f) where
    fmap f m = Codensity (\k -> runCodensity m (k . f))

instance Applicative (Codensity f) where
    pure = return
    mf <*> mx = Codensity (\k -> runCodensity mf (\f -> runCodensity mx (\x -> k (f x))))

instance Monad (Codensity f) where
    return x = Codensity (\k -> k x)
    m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))


-- CodensityPlus is the "mother of all MonadPlus"

-- mzero :: forall a. m a
-- mplus :: forall a. m a -> m a -> m a
--
-- mzero >>= f         = mzero                -- Left-zero
-- v >>= (\_ -> mzero) = mzero                -- Right-zero
-- mplus mzero m       = m                    -- Left-identity
-- mplus m mzero       = m                    -- Right-identity
-- mplus m (mplus n o) = mplus (mplus m n) o  -- Associativity
newtype CodensityPlus p a = CodensityPlus { runCodensityPlus :: forall b. p b -> (a -> p b) -> p b }

liftCodensityPlus :: MonadPlus p => p a -> CodensityPlus p a
liftCodensityPlus m = CodensityPlus (\mfai fmsuc -> (m >>= fmsuc) `mplus` mfai)

lowerCodensityPlus :: MonadPlus p => CodensityPlus p a -> p a
lowerCodensityPlus m = runCodensityPlus m mzero return

instance Functor (CodensityPlus p) where
    fmap (f :: a -> b) (m :: CodensityPlus p a) = CodensityPlus (\mfai fmsuc -> runCodensityPlus m mfai (fmsuc . f))

instance Monad (CodensityPlus p) where
    return x = CodensityPlus (\_mfai fmsuc -> fmsuc x)
    mx >>= fxmy = CodensityPlus (\mfai fmsuc -> runCodensityPlus mx mfai (\x -> runCodensityPlus (fxmy x) mfai fmsuc))

instance MonadPlus (CodensityPlus p) where
    mzero = CodensityPlus (\mfai _fmsuc -> mfai)
    m1 `mplus` m2 = CodensityPlus (\mfai msuc -> runCodensityPlus m1 (runCodensityPlus m2 mfai msuc) msuc)


main :: IO ()
main = return ()

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
    --pure x = Thingy $ \m -> Yoneda (\k -> runYoneda m (k . ($ x)))
    --mf <*> mx = Thingy $ \m -> runThingy mx (runThingy mf (Yoneda (\k -> runYoneda m (k . (.)))))
    pure x = Thingy $ \m -> fmap ($ x) m
    mf <*> mx = Thingy $ \m -> runThingy mx (runThingy mf (fmap (.) m))
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
    --
    -- pure ($ y) <*> u
    --   = Thingy (\m -> runThingy u (runThingy (Thingy (\m -> fmap ($ y) m)) (fmap (.) m)))
    --   = Thingy (\m -> runThingy u (fmap ($ y) (fmap (.) m)))
    --   ??? free theorem
    --   = Thingy (\m -> fmap ($ y) (runThingy u (fmap (.) m)))
    --   = Thingy (\m -> runThingy (Thingy (\m -> fmap ($ y) m)) (runThingy u (fmap (.) m)))
    --   = u <*> pure y

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
    -- id . t
    --   = Wotsit (runWotsit t . runWotsit (Wotsit id))
    --   = Wotsit (runWotsit t . id)
    --   = Wotsit (runWotsit t)
    --   = t
    --
    -- t . id
    --   = Wotsit (runWotsit (Wotsit id) . runWotsit t)
    --   = Wotsit (id . runWotsit t)
    --   = Wotsit (runWotsit t)
    --   = t
    --
    -- t . (r . s)
    --   = Wotsit (runWotsit (Wotsit (runWotsit s . runWotsit r)) . runWotsit t)
    --   = Wotsit ((runWotsit s . runWotsit r) . runWotsit t)
    --   = Wotsit (runWotsit s . (runWotsit r . runWotsit t))
    --   = Wotsit (runWotsit s . runWotsit (Wotsit (runWotsit r . runWotsit t)))
    --   = (t . r) . s

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

-- contrafmap1 id x = x
-- contrafmap1 f (contrafmap1 g x) = contrafmap1 (g . f) x
class ContraFunctor1 u where
    contrafmap1 :: (a -> b) -> u b c -> u a c

instance ContraFunctor1 (->) where
    contrafmap1 f h = h . f
    -- contrafmap1 id x
    --   = x . id
    --   = x
    --
    -- contrafmap1 f (contrafmap1 g x)
    --   = (x . g) . f
    --   = x . (g . f)
    --   = contrafmap1 (g . f) x

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
    -- contrafmap1 id x
    --  = ContraYoneda1 (\f' -> runContraYoneda1 x (id . f'))
    --  = ContraYoneda1 (\f' -> runContraYoneda1 x f')
    --  = x
    --
    -- contrafmap1 f (contrafmap1 g x)
    --   = ContraYoneda1 (\f' -> runContraYoneda1 (ContraYoneda1 (\f' -> runContraYoneda1 x (g . f'))) (f . f'))
    --   = ContraYoneda1 (\f' -> runContraYoneda1 x (g . (f . f')))
    --   = ContraYoneda1 (\f' -> runContraYoneda1 x ((g . f) . f'))
    --   = contrafmap1 (g . f) x

-- FIXME: a bit funny. Why do I require such a strong superclass constraint?
-- I only need this for (lift/lower)ContraYoneda1Wotsit though, so not a big deal.
instance ContraFunctor1Category u => Category (ContraYoneda1 u) where
    id = ContraYoneda1 (\k -> contrafmap1 k id)
    u1 . u2 = ContraYoneda1 (\k -> runContraYoneda1 u1 id . runContraYoneda1 u2 k)
    -- id . u
    --   = ContraYoneda1 (\k -> runContraYoneda1 (ContraYoneda1 (\k -> contrafmap1 k id)) id . runContraYoneda1 u k)
    --   = ContraYoneda1 (\k -> contrafmap1 id id . runContraYoneda1 u k)
    --   = ContraYoneda1 (\k -> id . runContraYoneda1 u k)
    --   = ContraYoneda1 (\k -> runContraYoneda1 u k)
    --   = u
    --
    -- u . id
    --   = ContraYoneda1 (\k -> runContraYoneda1 u id . runContraYoneda1 (ContraYoneda1 (\k -> contrafmap1 k id)) k)
    --   = ContraYoneda1 (\k -> runContraYoneda1 u id . contrafmap1 k id)
    --
    --   = ContraYoneda1 (\k -> runContraYoneda1 u k)
    --   = u
    --
    -- u1 . (u2 . u3)
    --   = ContraYoneda1 (\k -> runContraYoneda1 u1 id . runContraYoneda1 (ContraYoneda1 (\k -> runContraYoneda1 u2 id . runContraYoneda1 u3 k)) k)
    --   = ContraYoneda1 (\k -> runContraYoneda1 u1 id . (runContraYoneda1 u2 id . runContraYoneda1 u3 k))
    --   = ContraYoneda1 (\k -> (runContraYoneda1 u1 id . runContraYoneda1 u2 id) . runContraYoneda1 u3 k)
    --   = ContraYoneda1 (\k -> runContraYoneda1 (ContraYoneda1 (\k -> runContraYoneda1 u1 id . runContraYoneda1 u2 k)) id . runContraYoneda1 u3 k)
    --   = (u1 . u2) . u3

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

-- contrafmap1 id id                   = id
-- contrafmap1 f (u1 . u2)             = u1 . contrafmap1 f u2   ???
-- contrafmap1 f id . contrafmap1 g id = contrafmap1 (g . f) id
--    == OR ==
-- pureA1 id           = id
-- pureA1 f . pureA1 g = pureA1 (f . g)
class (ContraFunctor1 u, Category u) => ContraFunctor1Category u where
    pureA1 :: (a -> b) -> u a b
    pureA1 f = contrafmap1 f id

instance Arrow u => ContraFunctor1Category u where


newtype ContraYoneda1Wotsit u a b = ContraYoneda1Wotsit { runContraYoneda1Wotsit :: Wotsit (ContraYoneda1 u) a b }

liftContraYoneda1Wotsit :: ContraFunctor1Category u => u a b -> ContraYoneda1Wotsit u a b
liftContraYoneda1Wotsit u = ContraYoneda1Wotsit (liftWotsit (liftContraYoneda1 u))

lowerContraYoneda1Wotsit :: ContraFunctor1Category u => ContraYoneda1Wotsit u a b -> u a b
lowerContraYoneda1Wotsit u = lowerContraYoneda1 (lowerWotsit (runContraYoneda1Wotsit u))

instance ContraFunctor1 (ContraYoneda1Wotsit u) where
    contrafmap1 f u = ContraYoneda1Wotsit (Wotsit (\k -> contrafmap1 f (runWotsit (runContraYoneda1Wotsit u) k)))

instance Category (ContraYoneda1Wotsit u) where
    id = ContraYoneda1Wotsit id
    u1 . u2 = ContraYoneda1Wotsit (runContraYoneda1Wotsit u1 . runContraYoneda1Wotsit u2)
    -- id . u
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit (ContraYoneda1Wotsit id) . runContraYoneda1Wotsit u)
    --   = ContraYoneda1Wotsit (id . runContraYoneda1Wotsit u)
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit u)
    --   = u
    --
    -- u . id
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit u . runContraYoneda1Wotsit (ContraYoneda1Wotsit id))
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit u . id)
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit u)
    --   = u
    --
    -- u1 . (u2 . u3)
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit u1 . runContraYoneda1Wotsit (ContraYoneda1Wotsit (runContraYoneda1Wotsit u2 . runContraYoneda1Wotsit u3)))
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit u1 . (runContraYoneda1Wotsit u2 . runContraYoneda1Wotsit u3))
    --   = ContraYoneda1Wotsit ((runContraYoneda1Wotsit u1 . runContraYoneda1Wotsit u2) . runContraYoneda1Wotsit u3)
    --   = ContraYoneda1Wotsit (runContraYoneda1Wotsit (ContraYoneda1Wotsit (runContraYoneda1Wotsit u1 . runContraYoneda1Wotsit u2)) . runContraYoneda1Wotsit u3)
    --   = (u1 . u2) . u3

instance ContraFunctor1Category (ContraYoneda1Wotsit u) where


-- pureC id          = id
-- pureC f . pureC g = pureC (f . g)
class Category p => PurishCategory p where
    pureC :: (a -> b) -> p a b

instance Arrow a => PurishCategory a where
    pureC = arr

newtype PurishWotsit p a b = PurishWotsit { runPurishWotsit :: forall c. (forall d. (d -> b) -> p d c) -> (forall e. (e -> a) -> p e c) }

liftPurishWotsit :: PurishCategory p => p a b -> PurishWotsit p a b
liftPurishWotsit p = PurishWotsit (\k k' -> pureC k' >>> p >>> k id)

lowerPurishWotsit :: PurishCategory p => PurishWotsit p a b -> p a b
lowerPurishWotsit p = runPurishWotsit p pureC id

instance Category (PurishWotsit p) where
    id = PurishWotsit (\k k' -> k k')
    p1 . p2 = PurishWotsit (runPurishWotsit p2 . runPurishWotsit p1)
    -- id . p
    --   = PurishWotsit (runPurishWotsit p . runPurishWotsit (PurishWotsit (\k k' -> k k')))
    --   = PurishWotsit (runPurishWotsit p . (\k k' -> k k'))
    --   = PurishWotsit (runPurishWotsit p)
    --   = p
    --
    -- p . id
    --   = PurishWotsit (runPurishWotsit (PurishWotsit (\k k' -> k k')) . runPurishWotsit p)
    --   = PurishWotsit ((\k k' -> k k') . runPurishWotsit p)
    --   = PurishWotsit (runPurishWotsit p)
    --   = p
    --
    -- p1 . (p2 . p3)
    --   = PurishWotsit (runPurishWotsit (PurishWotsit (runPurishWotsit p3 . runPurishWotsit p2)) . runPurishWotsit p1)
    --   = PurishWotsit ((runPurishWotsit p3 . runPurishWotsit p2) . runPurishWotsit p1)
    --   = PurishWotsit (runPurishWotsit p3 . (runPurishWotsit p2 . runPurishWotsit p1))
    --   = PurishWotsit (runPurishWotsit p3 . runPurishWotsit (PurishWotsit (runPurishWotsit p2 . runPurishWotsit p1)))
    --   = (p1 . p2) . p3

instance PurishCategory (PurishWotsit p) where
    pureC x = PurishWotsit (\k k' -> k (x . k'))
    -- pureC id
    --   = PurishWotsit (\k k' -> k (id . k'))
    --   = PurishWotsit (\k k' -> k k')
    --   = id
    --
    -- pureC f . pureC g
    --   = PurishWotsit (runPurishWotsit (PurishWotsit (\k k' -> k (g . k'))) . runPurishWotsit (PurishWotsit (\k k' -> k (f . k'))))
    --   = PurishWotsit ((\k k' -> k (g . k')) . (\k k' -> k (f . k')))
    --   = PurishWotsit (\k k' -> k (\l -> f (g (k' l))))
    --   = PurishWotsit (\k k' -> k ((f . g) . k'))
    --   = pureC (f . g)


-- Voldemort is the "mother of all arrows":

-- arr   :: forall c d. (c -> d) -> r c d
-- (>>>) :: forall a b. r a b -> (forall c. r b c -> r a c)
-- first :: forall a b. r a b -> (forall c. r (a, c) (b, c))
-- (***) :: forall a b. r a b -> (forall c d. r c d -> r (a, c) (b, d))
--
-- arr                            = pureC
-- first (arr f)                  = arr (f `cross` id)              -- Extension
-- first (f >>> g)                = first f >>> first g             -- Functor
-- first f >>> arr (id `cross` g) = arr (id `cross` g) >>> first f  -- Exchange
-- first f >>> arr fst            = arr fst >>> f                   -- Unit
-- first (first f) >>> arr assoc  = arr assoc >>> first f           -- Association
--
--  where
---   (>>>) = flip (.)
--    f `cross` g = \(x, y) -> (f x, g y)
--    assoc (~(a, b), c) = (a, (b, c))
--newtype Voldemort r a b = Voldemort { runVoldemort :: forall c. BiYonedaWotsit r (a, c) (b, c) }
newtype Voldemort r a b = Voldemort { runVoldemort :: forall c. PurishWotsit r (a, c) (b, c) }

liftVoldemort :: Arrow r => r a b -> Voldemort r a b
liftVoldemort r = Voldemort (liftPurishWotsit (first r))

lowerVoldemort :: Arrow r => Voldemort r a b -> r a b
lowerVoldemort (r :: Voldemort r a b) = arr (\x -> (x, ())) >>> lowerPurishWotsit (runVoldemort r) >>> arr (\(x, ()) -> x)

instance Category (Voldemort r) where
    id = Voldemort id
    t1 . t2 = Voldemort (runVoldemort t1 . runVoldemort t2)

instance PurishCategory (Voldemort r) where
    pureC f = Voldemort (pureC (\(x, y) -> (f x, y)))
    -- pureC id
    --   = Voldemort $ pureC (\(x, y) -> (id x, y))
    --   = Voldemort $ pureC (\(x, y) -> (x, y))
    --   = Voldemort $ pureC id
    --   = Voldemort id
    --   = id
    --
    -- pureC f . pureC g
    --   = Voldemort (runVoldemort (Voldemort (pureC (\(x, y) -> (f x, y)))) . runVoldemort (Voldemort (pureC (\(x, y) -> (g x, y)))))
    --   = Voldemort ((pureC (\(x, y) -> (f x, y))) . (pureC (\(x, y) -> (g x, y))))
    --   = Voldemort (pureC ((\(x, y) -> (f x, y)) . (\(x, y) -> (g x, y))))
    --   = Voldemort (pureC (\(x, y) -> (f (g x), y)))
    --   = Voldemort (pureC (\(x, y) -> ((f . g) x, y)))
    --   = pureC (f . g)

instance Arrow (Voldemort r) where
    arr = pureC
    first t1 = Voldemort (pureC assoc >>> runVoldemort t1 >>> pureC reassoc)
      where assoc   (~(a, c), d) = (a, (c, d))
            reassoc (b, ~(c, d)) = ((b, c), d)
    -- first (arr f)
    --   = Voldemort (pureC assoc >>> runVoldemort (Voldemort (pureC (\(x, y) -> (f x, y)))) >>> pureC reassoc)
    --   = Voldemort (pureC assoc >>> pureC (\(x, y) -> (f x, y)) >>> pureC reassoc)
    --   = Voldemort (pureC (reassoc . (\(x, y) -> (f x, y)) . assoc))
    --   = Voldemort (pureC (\(~(a, c), d) -> ((f a, c), d)))
    --   = Voldemort (pureC (\(~(a, c), d) -> ((f a, id c), d)))
    --   = Voldemort (pureC (\(~(a, c), d) -> ((f `cross` id) (a, c), d)))
    --   = Voldemort (pureC (\(x, d) -> ((f `cross` id) x, d)))
    --   = pureC (f `cross` id)
    --
    -- first (f >>> g)
    --   = Voldemort (pureC assoc >>> runVoldemort (Voldemort (runVoldemort g . runVoldemort f)) >>> pureC reassoc)
    --   = Voldemort (pureC assoc >>> (runVoldemort g . runVoldemort f) >>> pureC reassoc)
    --   = Voldemort (pureC reassoc . runVoldemort g . runVoldemort f . pureC assoc)
    --   = Voldemort (pureC reassoc . runVoldemort g . id . runVoldemort f . pureC assoc)
    --   = Voldemort (pureC reassoc . runVoldemort g . pureC id . runVoldemort f . pureC assoc)
    --   = Voldemort (pureC reassoc . runVoldemort g . pureC (assoc . reassoc) . runVoldemort f . pureC assoc)
    --   = Voldemort ((pureC reassoc . runVoldemort g . pureC assoc) . (pureC reassoc . runVoldemort f . pureC assoc))
    --   = Voldemort ((pureC assoc >>> runVoldemort g >>> pureC reassoc) . (pureC assoc >>> runVoldemort f >>> pureC reassoc))
    --   = Voldemort (runVoldemort (Voldemort (pureC assoc >>> runVoldemort g >>> pureC reassoc)) . runVoldemort (Voldemort (pureC assoc >>> runVoldemort f >>> pureC reassoc)))
    --   = first f >>> first g
    --
    -- first f >>> arr (id `cross` g)
    --   = Voldemort (runVoldemort (Voldemort (pureC (\(x, y) -> ((id `cross` g) x, y)))) . runVoldemort (Voldemort (pureC assoc >>> runVoldemort f >>> pureC reassoc)))
    --   = Voldemort ((pureC (\(x, y) -> ((id `cross` g) x, y))) . (pureC assoc >>> runVoldemort f >>> pureC reassoc))
    --   = Voldemort (pureC (\(x, y) -> ((id `cross` g) x, y)) . pureC reassoc . runVoldemort f . pureC assoc)
    --   = Voldemort (pureC ((\(x, y) -> ((id `cross` g) x, y)) . reassoc) . runVoldemort f . pureC assoc)
    --   = Voldemort (pureC (\(b, ~(c, d)) -> (((id `cross` g) (b, c), d))) . runVoldemort f . pureC assoc)
    --   = Voldemort (pureC (\(b, ~(c, d)) -> (((id b, g c), d))) . runVoldemort f . pureC assoc)
    --   = Voldemort (pureC (\(b, ~(c, d)) -> (((b, g c), d))) . runVoldemort f . pureC assoc)
    --
    --   = Voldemort (pureC reassoc . runVoldemort f . pureC (\(~(a, c), d) -> (a, (g c, d))))
    --   = Voldemort (pureC reassoc . runVoldemort f . pureC (assoc . (\(~(a, c), d) -> ((id a, g c), d))))
    --   = Voldemort (pureC reassoc . runVoldemort f . pureC (assoc . (\(x, y) -> ((id `cross` g) x, y))))
    --   = Voldemort (pureC reassoc . runVoldemort f . pureC assoc . pureC (\(x, y) -> ((id `cross` g) x, y)))
    --   = Voldemort ((pureC assoc >>> runVoldemort f >>> pureC reassoc) . (pureC (\(x, y) -> ((id `cross` g) x, y))))
    --   = Voldemort (runVoldemort (Voldemort (pureC assoc >>> runVoldemort f >>> pureC reassoc)) . runVoldemort (Voldemort (pureC (\(x, y) -> ((id `cross` g) x, y)))))
    --   = arr (id `cross` g) >>> first f


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
    m >>= f = Codensity (\c -> runCodensity m (\a -> runCodensity (f a) c))
    -- return a >>= f
    --   = Codensity (\c -> runCodensity (Codensity (\k -> k a)) (\a -> runCodensity (f a) c))
    --   = Codensity (\c -> runCodensity (f a) c)
    --   = f a
    --
    -- m >>= return
    --   = Codensity (\c -> runCodensity m (\a -> runCodensity (Codensity (\k -> k x)) c))
    --   = Codensity (\c -> runCodensity m (\a -> c a))
    --   = m
    --
    -- ((m >>= f) >>= g)
    --   = Codensity (\c -> runCodensity (Codensity (\c -> runCodensity m (\a -> runCodensity (f a) c))) (\a -> runCodensity (g a) c))
    --   = Codensity (\c -> runCodensity m (\a -> runCodensity (f a) (\a -> runCodensity (g a) c)))
    --   = Codensity (\c -> runCodensity m (\a -> runCodensity (Codensity (\c -> runCodensity (f a) (\a -> runCodensity (g a) c))) c))
    --   = Codensity (\c -> runCodensity m (\a -> runCodensity (f a >>= g) c))
    --   = m >>= (\x -> f x >>= g)


-- CodensityPlus is the "mother of all MonadPlus"

-- mzero :: forall a. m a
-- mplus :: forall a. m a -> m a -> m a
--
-- mzero >>= f         = mzero                 -- Left-zero
-- v >>= (\_ -> mzero) = mzero                 -- Right-zero
-- mplus mzero m       = m                     -- Left-identity
-- mplus m mzero       = m                     -- Right-identity
-- mplus m (mplus n o) = mplus (mplus m n) o   -- Associativity
-- mplus m n >>= o = mplus (m >>= o) (n >>= o) -- Distributivity
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
    -- mzero >>= f
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus (CodensityPlus (\_fmsuc mfai -> mfai)) (\x mfai -> runCodensityPlus (f x) fmsuc mfai) mfai)
    --   = CodensityPlus (\fmsuc mfai -> mfai)
    --   = mzero
    --
    -- v >>= (\_ -> mzero)
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus v (\x mfai -> runCodensityPlus ((\_ -> mzero) x) fmsuc mfai) mfai)
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus v (\x mfai -> runCodensityPlus (CodensityPlus (\_fmsuc mfai -> mfai)) fmsuc mfai) mfai)
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus v (\x mfai -> mfai) mfai)
    --   ???? parametricity
    --   = CodensityPlus (\_fmsuc mfai -> mfai)
    --   = mzero
    --
    -- mplus mzero m
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus (CodensityPlus (\_fmsuc mfai -> mfai)) fmsuc (runCodensityPlus m fmsuc mfai))
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus m fmsuc mfai)
    --   = m
    --
    -- mplus m mzero
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus m fmsuc (runCodensityPlus (CodensityPlus (\_fmsuc mfai -> mfai)) fmsuc mfai))
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus m fmsuc mfai)
    --   = m
    --
    -- mplus m (mplus n o)
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus m fmsuc (runCodensityPlus (CodensityPlus (\fmsuc mfai -> runCodensityPlus n fmsuc (runCodensityPlus o fmsuc mfai))) fmsuc mfai))
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus m fmsuc (runCodensityPlus n fmsuc (runCodensityPlus o fmsuc mfai)))
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus (CodensityPlus (\fmsuc mfai -> runCodensityPlus m fmsuc (runCodensityPlus n fmsuc mfai))) fmsuc (runCodensityPlus o fmsuc mfai))
    --   = mplus (mplus m n) o
    --
    -- mplus m n >>= o
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus (CodensityPlus (\fmsuc mfai -> runCodensityPlus m fmsuc (runCodensityPlus n fmsuc mfai))) (\x mfai -> runCodensityPlus (o x) fmsuc mfai) mfai)
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus m (\x mfai -> runCodensityPlus (o x) fmsuc mfai) (runCodensityPlus n (\x mfai -> runCodensityPlus (o x) fmsuc mfai) mfai))
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus (CodensityPlus (\fmsuc mfai -> runCodensityPlus m (\x mfai -> runCodensityPlus (o x) fmsuc mfai) mfai)) fmsuc (runCodensityPlus (CodensityPlus (\fmsuc mfai -> runCodensityPlus n (\x mfai -> runCodensityPlus (o x) fmsuc mfai) mfai)) fmsuc mfai))
    --   = CodensityPlus (\fmsuc mfai -> runCodensityPlus (m >>= o) fmsuc (runCodensityPlus (n >>= o) fmsuc mfai))
    --   = mplus (m >>= o) (n >>= o)

main :: IO ()
main = return ()

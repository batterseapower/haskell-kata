{-# LANGUAGE Rank2Types #-}
module GeneralisedApplicative where

-- What if I have two Records and I want to zip them together?
-- We want to zip each field in a distinctive way.
--
-- This module shows a possible solution using I concocted that
-- appears to be a generalisation of Applicative
data Record f = Record {
    foo :: f Int,
    bar :: f Bool,
    baz :: f Double
  }

naturality :: (forall a. f a -> g a)
           -> Record f -> Record g
naturality f r = Record {
    foo = f (foo r),
    bar = f (bar r),
    baz = f (baz r)
  }


-- NB: (/\f g -> forall a. Mapper f g a) seems to be a Category in a certain sense,
-- though the lack of kind polymorphism would prevent us from making it a Category instance
newtype Mapper f g a = Mapper { unMapper :: f a -> g a }

-- Analgous to pure :: a -> r a
pureRecord :: (forall a. f a) -> Record f
pureRecord x = Record {
    foo = x,
    bar = x,
    baz = x
  }

-- Analogous to <*> :: r (a -> b) -> r a -> r b
mapRecord :: Record (Mapper f g) -> Record f -> Record g
mapRecord r1 r2 = Record {
    foo = unMapper (foo r1) (foo r2),
    bar = unMapper (bar r1) (bar r2),
    baz = unMapper (baz r1) (baz r2)
  }


-- We can use that machinery to implement the zipping operation we were
-- originally after:

newtype Zipper f g h a = Zipper { unZipper :: f a -> g a -> h a }

zipRecord :: Record (Zipper f g h) -> Record f -> Record g -> Record h
zipRecord r1 r2 r3 = Record {
    foo = unZipper (foo r1) (foo r2) (foo r3),
    bar = unZipper (bar r1) (bar r2) (bar r3),
    baz = unZipper (baz r1) (baz r2) (baz r3)
  }

zipRecord' :: Record (Zipper f g h) -> Record f -> Record g -> Record h
zipRecord' r1 r2 r3 = mapRecord (doit1 r1 r2) r3
  where
    doit1 :: Record (Zipper f g h) -> Record f -> Record (Mapper g h)
    doit1 r1 r2 = mapRecord (naturality doit2 r1) r2
    
    doit2 :: Zipper f g h a -> Mapper f (Mapper g h) a
    doit2 z = Mapper (Mapper . unZipper z)


main :: IO ()
main = return ()

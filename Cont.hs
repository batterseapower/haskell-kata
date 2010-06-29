newtype Cont res a = Cont { unCont :: (a -> res) -> res }

instance Functor (Cont res) where
    fmap f m = Cont $ \c -> unCont m (c . f)

instance Monad (Cont res) where
    return a = Cont ($ a)
    m >>= k  = Cont $ \c -> unCont m $ \a -> unCont (k a) c

callCC :: ((a -> Cont res b) -> Cont res a) -> Cont res a
callCC f = Cont $ \c -> unCont (f (\a -> Cont $ \_ -> c a)) c


main :: IO ()
main = return ()

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Fish where

import           Prelude (id)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    {-| LAWS
        1. fmap id         ≡ id
        2. fmap f . fmap g ≡ fmap (f . g)
    -}

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {-| LAWS
        1. m >>= return    ≡ m
        2. return a >>= f  ≡ f a
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    -}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    {-| LAWS
        1. f >=> returnFish ≡ f
        2. returnFish >=> f ≡ f
        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    -}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    {-| LAWS
        1. join . returnJoin      ≡ id
        2. join . fmap returnJoin ≡ id
        3. join . fmap join       ≡ join . join
        4* join . fmap (fmap f)   ≡ fmap f . join
    -}


instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \x -> f x >>= g
    {-|
        3.  (f >=> g) >=> h                 ≡ f >=> (g >=> h)                   Definition of (>=>)
            \x -> (f >=> g) x >>= h         ≡ \x -> f x >>= (g >=> h)           Definition of (>=>)
            \x -> (\y -> f y >>= g) x >>= h ≡ \x -> f x >>= (\y -> g y >>= h)   Application of (\y -> f y >>= g) to x
            \x -> (f x >>= g) >>= h         ≡ \x -> f x >>= (\y -> g y >>= h)   Monad.3
            \x -> (f x >>= g) >>= h         ≡ \x -> (f x >>= g) >>= h           OK
    -}

instance Monad m => MonadJoin m where
    returnJoin = return
    join m = m >>= id                -- id ≡ (\x -> x)
    {-|
        1.  join . returnJoin         ≡ id    Definition of (.)
            \x -> join (returnJoin x) ≡ id    Definition of join
            \x -> returnJoin x >>= id ≡ id    Definition of returnJoin
            \x -> return x >>= id     ≡ id    Monad.2
            \x -> id x                ≡ id    Eta-reduction
            id                        ≡ id    OK
    -}

instance MonadFish m => Monad m where
    return = returnFish
    x >>= f = (id >=> f) x
    {-|
        1.  m >>= return          ≡ m   Definition of (>>=)
            (id >=> return) m     ≡ m   Definition of return
            (id >=> returnFish) m ≡ m   MonadFish.1
            id m                  ≡ m   Definition of id
            m                     ≡ m   OK
    -}

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id
    {-|
        1.  join . returnJoin ≡ id
            (id >=> id) . returnJoin ≡ id
            (id >=> id) . returnFish ≡ id
    -}

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin
    x >>= f =

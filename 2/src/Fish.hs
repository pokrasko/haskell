{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Fish where

import           Prelude (id)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    {-| LAWS
        1. fmap id                      ≡ id
        2. fmap f . fmap g              ≡ fmap (f . g)
    -}

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {-| LAWS
        1. m >>= return                 ≡ m
        or bind return                  ≡ id

        2. return a >>= f               ≡ f a
        or bind f . return              ≡ f

        3. (m >>= f) >>= g              ≡ m >>= (\x -> f x >>= g)
        or bind g . bind f              ≡ bind (bind g . f)
    -}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    {-| LAWS
        1. f >=> returnFish             ≡ f
        2. returnFish >=> f             ≡ f
        3. (f >=> g) >=> h              ≡ f >=> (g >=> h)
    -}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    {-| LAWS
        1.  join . returnJoin           ≡ id
        2.  join . fmap returnJoin      ≡ id
        3.  join . fmap join            ≡ join . join
        4*  join . fmap (fmap f)        ≡ fmap f . join
    -}


instance Monad m => Functor m where
    fmap f m = m >>= return . f         -- of fmap f = bind (return . f)
    {-|
        1.  fmap id                     ≡ id                      Definition of fmap
            bind (return . id)          ≡ id                      (.).1
            bind return                 ≡ id                      Monad.1
            id                          ≡ id                      OK

        2.  fmap f . fmap g                         ≡ fmap (f . g)            Definition of fmap
            bind (return . f) . bind (return . g)   ≡ bind (return . f . g)   Monad.3
            bind (bind (return . f) . (return . g)) ≡ bind (return . f . g)   (.).2
            bind (bind (return . f) . return . g)   ≡ bind (return . f . g)   Monad.2
            bind (return . f . g)                   ≡ bind (return . f . g)   OK
    -}

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \x -> f x >>= g           -- or f >=> g = bind g . f
    {-|
        1.  f >=> returnFish            ≡ f                       Definition of (>=>)
            bind returnFish . f         ≡ f                       Definition of returnFish
            bind return . f             ≡ f                       Monad.1
            id . f                      ≡ f                       (.).1
            f                           ≡ f                       OK

        2.  returnFish >=> f            ≡ f                       Definition of (>=>)
            bind f . returnFish         ≡ f                       Definition of returnFish
            bind f . return             ≡ f                       Monad.2
            f                           ≡ f                       OK

        3.  (f >=> g) >=> h             ≡ f >=> (g >=> h)         Definition of (>=>)
            bind h . bind g . f         ≡ bind (bind h . g) . f   Monad.3
            bind h . bind g . f         ≡ (bind h . bind g) . f   (.).2
            bind h . bind g . f         ≡ bind h . bind g . f     OK
    -}

instance Monad m => MonadJoin m where
    returnJoin = return
    join m = m >>= id                   -- or join = bind id
    {-|
        1.  join . returnJoin           ≡ id                      Definition of join
            bind id . returnJoin        ≡ id                      Definition of returnJoin
            bind id . return            ≡ id                      Monad.1
            id                          ≡ id                      OK
    -}

instance MonadFish m => Monad m where
    return = returnFish
    x >>= f = (id >=> f) x              -- or bind f = id >=> f
    {-|
        1.  bind return                 ≡ id                      Definition of bind
            id >=> return               ≡ id                      MonadFish.1
            id                          ≡ id                      OK
    -}

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id
    {-|
        1.  join . returnJoin           ≡ id
            (id >=> id) . returnJoin    ≡ id
            (id >=> id) . returnFish    ≡ id
    -}

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin
    x >>= f = join $ fmap f x           -- or bind f = join . fmap f
    {-|
        1.  bind return                 ≡ id                      Definition of (>>=)
            join . fmap return          ≡ id                      Definition of return
            join . fmap returnJoin      ≡ id                      MonadJoin.2
            id                          ≡ id                      OK
    -}

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    f >=> g = join . fmap g . f
    {-|
        1.  f >=> returnFish              ≡ f                     Definition of (>=>)
            join . fmap returnFish . f    ≡ f                     Definition of returnFish
            join . fmap returnJoin . f    ≡ f                     (.).2
            (join . fmap returnJoin) . f  ≡ f                     MonadJoin.2
            id . f                        ≡ f                     (.).1
            f                             ≡ f                     OK

        2.  returnFish >=> f                                   ≡ f              Definition of (>=>)
            join . fmap f . returnFish                         ≡ f              Definition of returnFish
            join . fmap f . returnJoin                         ≡ f
            fmap (join . fmap f . returnJoin)                  ≡ fmap f         Application of fmap to both sides
            fmap join . fmap (fmap f) . fmap returnJoin        ≡ fmap f         Composition of join to both sides
            join . fmap join . fmap (fmap f) . fmap returnJoin ≡ join . fmap f  MonadJoin.3
            join . join . fmap (fmap f) . fmap returnJoin      ≡ join . fmap f  MonadJoin.4
            join . fmap f . join . fmap returnJoin             ≡ join . fmap f  MonadJoin.2
            join . fmap f . id                                 ≡ join . fmap f  (.).1
            join . fmap f                                      ≡ join . fmap f  OK

        3.  (f >=> g) >=> h               ≡ f >=> (g >=> h)
            join . fmap h . join . fmap g . f ≡ join . fmap (join . fmap h . g) . f
            join . fmap h . join . fmap g . f ≡ join . fmap join . fmap (fmap h) . fmap g . f
            join . fmap h . join . fmap g . f ≡ join . join . fmap (fmap h) . fmap g . f
            join . fmap h . join . fmap g . f ≡ join . fmap h . join . fmap g . f
    -}

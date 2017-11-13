{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ChaoticGood where

import           Prelude     (Applicative(..), Foldable(..), Functor(..), Monoid(..), Ord, Traversable(..),
                              ($), (.), (<$>), mempty)
import           Data.Monoid ((<>))

import           Tree        (Tree(..), fromList, toList)


newtype Identity a = Identity
                   { getIdentity :: a
                   }

data Either e a = Left e
                | Right a

newtype Const a b = Const a

data Tuple a b = Tuple a b                                  -- (,) can't be redefined manually,
                                                            -- neither its instances can be


instance Functor Identity where
    fmap f = Identity . f . getIdentity
    {-|
        1.  fmap id                                                   ≡ id    Definition of fmap
            Identity . id . getIdentity                               ≡ id    Definition of (.)
            \xi -> Identity (id (getIdentity xi))                     ≡ id    Pattern-matching by xi
            \(Identity x) -> Identity (id (getIdentity (Identity x))) ≡ id    Definition of Identity
            \(Identity x) -> Identity (id x)                          ≡ id    Definition of id
            \(Identity x) -> Identity x                               ≡ id    Definition of id (reverse)
            id                                                        ≡ id    OK
    -}

instance Functor (Either e) where
    fmap f (Left e)  = Left e
    fmap f (Right x) = Right $ f x
    {-|
        2.  fmap (f . g)                   ≡ fmap f . fmap g                  Definition of (.)
            fmap (\x -> f (g x))           ≡ \xe -> fmap f (fmap g xe)        Eta-reduction (reverse)
            fmap (\y -> f (g y)) xe        ≡ fmap f (fmap g xe)               Pattern-matching by xe
        a.  fmap (\y -> f (g y)) (Left e)  ≡ fmap f (fmap g (Left e))         Definition of fmap
            Left e                         ≡ fmap f (Left e)                  Definition of fmap
            Left e                         ≡ Left e                           OK
        b.  fmap (\y -> f (g y)) (Right x) ≡ fmap f (fmap g (Right x))        Definition of fmap
            Right $ (\y -> f (g y)) x      ≡ fmap f (Right $ g x)             Definition of ($)
            Right ((\y -> f (g y)) x)      ≡ fmap f (Right (g x))             Application of (\y -> f (g y)) to x
            Right (f (g x))                ≡ fmap f (Right (g x)              Definition of fmap
            Right (f (g x))                ≡ Right $ f (g x)                  Definition of ($)
            Right (f (g x))                ≡ Right (f (g x))                  OK
    -}

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Functor (Const a) where
    fmap f (Const c) = Const c

instance Functor (Tuple a) where                          -- instance Functor ((,) a) where
    fmap f (Tuple x y) = Tuple x (f y)                    --     fmap f (x, y) = (x, f y)

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity x) = Identity $ f x
    {-|
        1.  u          <*> pure y     ≡ pure ($ y)     <*> u            Definition of pure
            u          <*> Identity y ≡ Identity ($ y) <*> u            Pattern-matching by u
            Identity f <*> Identity y ≡ Identity ($ y) <*> Identity f   Definition of (<*>)
            Identity (f y)            ≡ Identity (($ y) f)              Definition of ($)
            Identity (f y)            ≡ Identity (\f' -> f' y) f)       Application of (\f' -> f' y) f
            Identity (f y)            ≡ Identity (f y)                  OK
    -}

instance Applicative (Either e) where
    pure = Right

    (Left e)  <*> _         = Left e
    _         <*> (Left e)  = Left e
    (Right f) <*> (Right x) = Right $ f x

instance Applicative Tree where
    pure x = Node Leaf x Leaf

    Leaf            <*> _              = Leaf
    _               <*> Leaf           = Leaf
    fn@(Node _ f _) <*> (Node xl x xr) = Node (fn <*> xl) (f x) (fn <*> xr)
    {-|
        1.  pure id           <*> v                                           ≡ v             Pattern-matching by v
        a.  pure id           <*> Leaf                                        ≡ Leaf          Definition of (<*>)
            Leaf                                                              ≡ Leaf          OK
        b.  pure id           <*> Node xl x xr                                ≡ Node xl x xr  Definition of pure
            Node Leaf id Leaf <*> Node xl x xr                                ≡ Node xl x xr  Definition of (<*>)
            Node (Node Leaf id Leaf <*> xl) (id x) (Node Leaf id Leaf <*> xr) ≡ Node xl x xr  Definition of id
            Node (Node Leaf id Leaf <*> xl) x (Node Leaf id Leaf <*> xr)      ≡ Node xl x xr  Definition of pure (reverse)
            Node (pure id <*> xl) x (pure id <*> xr)                          ≡ Node xl x xr  Applicative.1 (reverse)
            Node xl x xr                                                      ≡ Node xl x xr  OK
    -}

instance Monoid a => Applicative (Const a) where
    pure _ = Const mempty

    (Const m1) <*> (Const m2) = Const $ m1 <> m2
    {-|
        2.  pure (.)     <*> u        <*> v        <*> w        ≡ u        <*> (v        <*> w)         Pattern-matching by u, v, w
            pure (.)     <*> Const cu <*> Const cv <*> Const cw ≡ Const cu <*> (Const cv <*> Const cw)  Definition of pure
            Const mempty <*> Const cu <*> Const cv <*> Const cw ≡ Const cu <*> (Const cv <*> Const cw)  Definition of <*>
            Const (mempty <> cu)      <*> Const cv <*> Const cw ≡ Const cu <*> (Const cv <*> Const cw)  Monoid.1
            Const cu                  <*> Const cv <*> Const cw ≡ Const cu <*> (Const cv <*> Const cw)  Definition of <*>
            Const (cu <> cv)                       <*> Const cw ≡ Const cu <*> Const (cv <> cw)         Definition of <*>
            Const ((cu <> cv) <> cw)                            ≡ Const (cu <> cv <> cw)                Monoid.3
            Const (cu <> cv <> cw)                              ≡ Const (cu <> cv <> cw)                OK
    -}

instance Monoid a => Applicative (Tuple a) where                -- instance Monoid a => Applicative ((,) a) where
    pure = Tuple mempty                                         --     pure y = (mempty, y)

    (Tuple m1 f) <*> (Tuple m2 x) = Tuple (m1 <> m2) $ f x      --     (m1, f) <*> (m2, x) = (x1 <> x2, f x)
    {-|
        3.  pure f         <*> pure x         ≡ pure (f x)          Definition of pure
            Tuple mempty f <*> Tuple mempty x ≡ Tuple mempty (f x)  Definition of (<*>)
            Tuple (mempty <> mempty) (f x)    ≡ Tuple mempty (f x)  Monoid.1 or Monoid.2
            Tuple mempty (f x)                ≡ Tuple mempty (f x)  OK
    -}

instance Foldable Identity where
    foldMap f = f . getIdentity

instance Foldable (Either e) where
    foldMap f Left{}    = mempty
    foldMap f (Right x) = f x
    {-|
        2.  foldMap f           ≡ fold . fmap f                   Eta-reduction (reverse)
            foldMap f x         ≡ (fold . fmap f) x               Definition of (.)
            foldMap f x         ≡ (\x' -> fold (fmap f x')) x     Application of (\x' -> fold (fmap f x')) to x
            foldMap f x         ≡ fold (fmap f x)                 Definition of fold
            foldMap f x         ≡ foldMap id (fmap f x)           Pattern-matching by x
        a.  foldMap f (Left e)  ≡ foldMap id (fmap f (Left e))    Definition of fmap
            foldMap f (Left e)  ≡ foldMap id (Left e)             Definition of foldMap
            foldMap mempty      ≡ foldMap mempty                  OK
        b.  foldMap f (Right x) ≡ foldMap id (fmap f (Right x))   Definition of fmap
            foldMap f (Right x) ≡ foldMap id (Right (f x))        Definition of foldMap
            f x                 ≡ id (f x)                        Definition of id
            f x                 ≡ f x                             OK
    -}

instance Foldable (Const a) where
    foldMap _ _ = mempty

instance Foldable (Tuple a) where                               -- instance Foldable ((,) a) where
    foldMap f (Tuple _ x) = f x                                 --     foldmap f (_, x) = f x

instance Traversable Identity where
    traverse fa = fmap Identity . fa . getIdentity

instance Traversable (Either e) where
    traverse fa (Left e)  = pure $ Left e
    traverse fa (Right x) = Right <$> fa x

instance Traversable Tree where
    traverse fa Leaf = pure Leaf
    traverse fa (Node l x r) = Node <$> traverse fa l <*> fa x <*> traverse fa r
    {-|
        1.  t . traverse f                                                          ≡ traverse (t . f)                                                        Eta-reduction (reverse)
            (t . traverse f) tree                                                   ≡ traverse (t . f) tree                                                   Definition of (.)
            t (traverse f tree)                                                     ≡ traverse (t . f) tree                                                   Pattern-matching by tree
        a.  t (traverse f Leaf)                                                     ≡ traverse (t . f) Leaf                                                   Definition of traverse
            t (pure Leaf)                                                           ≡ pure Leaf                                                               t.1
            pure Leaf                                                               ≡ pure Leaf                                                               OK
        b.  t (traverse f (Node l x r))                                             ≡ traverse (t . f) (Node l x r)                                           Definition of traverse
            t (Node       <$> traverse f l       <*> f x     <*> traverse f r)      ≡ Node      <$> traverse (t . f) l <*> (t . f) x <*> traverse (t . f) r   fmap f x ≡ pure f <*> x from Applicative
            t (pure Node  <*> traverse f l       <*> f x     <*> traverse f r)      ≡ pure Node <*> traverse (t . f) l <*> (t . f) x <*> traverse (t . f) r   t.2
            t (pure Node) <*> t (traverse f l)   <*> t (f x) <*> t (traverse f r)   ≡ pure Node <*> traverse (t . f) l <*> (t . f) x <*> traverse (t . f) r   t.1
            pure Node     <*> t (traverse f l)   <*> t (f x) <*> t (traverse f r)   ≡ pure Node <*> traverse (t . f) l <*> (t . f) x <*> traverse (t . f) r   Definition of (.) (reverse)
            pure Node     <*> (t . traverse f) l <*> t (f x) <*> (t . traverse f) r ≡ pure Node <*> traverse (t . f) l <*> (t . f) x <*> traverse (t . f) r   Traversable.1 (inductive)
            pure Node     <*> traverse (t . f) l <*> t (f x) <*> traverse (t . f) r ≡ pure Node <*> traverse (t . f) l <*> (t . f) x <*> traverse (t . f) r   Definition of (.)
            pure Node     <*> traverse (t . f) l <*> t (f x) <*> traverse (t . f) r ≡ pure Node <*> traverse (t . f) l <*> t (f x)   <*> traverse (t . f) r   OK
    -}

instance Traversable (Const a) where
    traverse fa (Const c) = pure $ Const c
    {-|
        2.  traverse Identity           ≡ Identity            Eta-reduction (reverse)
            traverse Identity cc        ≡ Identity cc         Pattern-matching by cc
            traverse Identity (Const c) ≡ Identity (Const c)  Definition of traverse
            pure (Const c)              ≡ Identity (Const c)  Definition of pure
            Identity (Const c)          ≡ Identity (Const c)  OK
    -}

instance Traversable (Tuple a) where                            -- instance Traversable ((,) a) where
    traverse fa (Tuple l x) = Tuple l <$> fa x                  --     traverse fa (l, x) = (,) l <$> fa x

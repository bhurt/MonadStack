{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
Module: Control.Monad.MonadStack
Description: A multi-level lift through a monad transformer stack.
Copyright: (C) 2015 by Brian Hurt
License: BSD 2-Clause
Maintainer: Brian Hurt <bhurt42@gmail.com>
Stability: experimental
Portability: safe

MonadStack provides a multi-level @lift@ function, called @liftFrom@.
It allows lifting from an arbitrary monad in a monad transformer stack
up to any higher monad in that stack.  And example use might be:

>    newtype MyMonad = MyMonad ... deriving (Monad)
>
>    doFoo :: MonadStack MyMonad m => something -> m whatever
>    doFoo arg = liftFrom go
>        where
>            go :: MyMonad whatever
>            go = do ...

This allows calling doFoo either in the MyMonad monad, or in any monad
transformer stack on top of the MyMonad.

MonadStack is similar to the @Control.Monad.Base@ module from the
<https://hackage.haskell.org/package/transformers-base transformers-base>
package, except that there is no functional dependency between the
monads.  A monad transformer stack can only have one base, but it can
have several MonadStack implementations.

MonadStack is very similar to the @Control.Monad.Lifter@ module from the
<https://hackage.haskell.org/package/MonadCompose MonadCompose> package,
with two exceptions.  One, MonadStack does not require the
OverlappingInstances and UndecidableInstances extensions- both of which
are prone to generating hard to diagnose bugs.  But this implies two,
that MonadStack is much less feature-rich than Lifter is.  Specifically,
MonadStack only works with "proper" monad transformers (those that
implment @MonadTrans@), while Lifter generalizes the notion of lifting
and works with more different types of monads, especially @MonadPlus@.

-}
module Control.Monad.MonadStack where

    import Control.Monad.Trans

    -- | A multi-level lifter class.
    --
    -- The existance of an implementation of @MonadStack m n@ implies
    -- that is is possible to convert an @m a@ into an @n a@ via
    -- zero or more applications of @lift@.
    class MonadStack m n where
        liftFrom :: m a -> n a

    -- | The base case implementation
    --
    -- You can always convert an @m a@ into an @m a@ by applying 0
    -- lifts.  This is like treating an @m a@ as a zero-level monad
    -- transformer stack (a monad transformer stack with no monad
    -- transformers), and @liftFrom@ is just the @id@ function.
    instance MonadStack m m where
        liftFrom = id

    -- | The inductive step implementation
    --
    -- If there exists an implementation of @MonadStack k m@ for
    -- monads @k@ and @m@ (that is, we can perform a @liftFrom@ to
    -- convert a @k a@ to a @m a@), and n is a monad transformer,
    -- then we can construct an implementation of @MonadStack k (n m)@,
    -- by just adding one more @lift@ to the @liftFrom@.

    -- Conceptually, if I can get from a k a to an m a via some
    -- sequence of lifts, and n is a monad transformer, then I can
    -- get from a k a to a n m a with one more lift.  Or, if I can
    -- lift through an @n@-level monad transformer stack, I can
    -- lift through a @n+1@ level monad transformer stack.
    instance (Monad k, Monad m, MonadTrans n, MonadStack k m)
                    => MonadStack k (n m) where
        liftFrom = lift . liftFrom

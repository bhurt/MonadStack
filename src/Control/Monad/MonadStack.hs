{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.MonadStack where

    import Control.Monad.Trans

    -- MonadStack m n implies there is some sequence of lifts
    -- by which I can convert an m a into an n a- i.e. that
    -- n a is a monad transformer stack sitting on top of an m a.
    class MonadStack m n where
        liftFrom :: m a -> n a

    -- I can get from any monad to itself with a no-op
    instance MonadStack m m where
        liftFrom = id

    -- If I can get from a k a to an m a via some sequence of 
    -- lifts, and n is a monad transformer, then I can get from
    -- a k a to a n m a with one more lift.
    instance (Monad k, Monad m, MonadTrans n, MonadStack k m)
                    => MonadStack k (n m) where
        liftFrom = lift . liftFrom

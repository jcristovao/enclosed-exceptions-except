{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @enclosed-exception-except@ is a complement to Michael Snoyman' excelent
--   "enclosed-exception" package, allowing you to run an IO computation that
--   may raise exceptions and convert it to an 'ExceptT e IO a' value, while
--   still remaining responsive to external asynchronous exceptions.
--
--   This module is fully strict, in the sense that it forces full evaluation
--   on its arguments.
--
--   The perfect use case for this is when you need to run a series of IO
--   instructions that may each raise an exception, but you want to provide
--   a custom error message for some of them.
--
module Control.Exception.Enclosed.Except
  ( eTry
  , eIoTry
  , eTextTry
  , eCustomTry
  ) where

import Data.Text as T
import Data.Maybe
import Data.Monoid
import Control.DeepSeq

import Control.Monad.Trans.Control
import Control.Exception.Enclosed
import Control.Exception.Lifted
import Control.Monad.Trans.Except


-- | Utility function. Available in bifunctors, but let us avoid that extra dependency.
mapLeft :: (e0 -> e1) -> Either e0 a -> Either e1 a
mapLeft f = either (Left . f) Right


fromIOException' :: SomeException -> IOException
fromIOException' e
  = fromMaybe (throw . AssertionFailed $ "Not an IOException:" <> show e)
  $ fromException e

-- | Runs provided @IO@ action, captures synchronous exceptions as @Left@ values,
-- re-throws asynchronous exceptions.
--
-- /Note:/ value @a@ if fully evaluated, and as such it should be a member of the
-- @NFData@ typeclass
eTry :: (MonadBaseControl IO (ExceptT e IO), NFData a)
     => IO a -> ExceptT SomeException IO a
eTry    = ExceptT . tryAnyDeep

-- | Runs provided @IO@ action, captures synchronous @IOException@ as @Left@
-- values, re-throws asynchronous exceptions (and synchronous non-IOExceptions).
--
-- /note:/ value @a@ if fully evaluated, and as such it should be a member of the
-- @nfdata@ typeclass
eIoTry :: (MonadBaseControl IO (ExceptT e IO), NFData a)
       => IO a -> ExceptT IOException IO a
eIoTry  = ExceptT . fmap (mapLeft fromIOException') . tryAnyDeep

-- | Runs provided @IO@ action, captures synchronous @IOException@ as left @Text@
-- values, re-throws asynchronous exceptions (and synchronous non-IOExceptions).
--
-- /note:/ value @a@ if fully evaluated, and as such it should be a member of the
-- @nfdata@ typeclass
eTextTry :: (MonadBaseControl IO (ExceptT e IO), NFData a)
         => IO a -> ExceptT Text IO a
eTextTry = mapExceptT (fmap (mapLeft (T.pack . show))) . eIoTry

-- | Runs provided @IO@ action, captures synchronous @IOException@ as left @Text@
-- values prepending the given error description, and re-throws asynchronous
-- exceptions (and synchronous non-IOExceptions).
--
-- /note:/ value @a@ if fully evaluated, and as such it should be a member of the
-- @nfdata@ typeclass
eCustomTry :: (MonadBaseControl IO (ExceptT e IO), NFData a)
  => Text -> IO a -> ExceptT Text IO a
eCustomTry err = mapExceptT (fmap (mapLeft ((err <>) . T.pack . show))) . eIoTry

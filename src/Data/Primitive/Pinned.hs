{-# language
        BangPatterns
      , DataKinds
      , DerivingStrategies
      , GeneralizedNewtypeDeriving
      , KindSignatures
      , MagicHash
      , RoleAnnotations
      , ScopedTypeVariables
      , StandaloneDeriving
  #-}

module Data.Primitive.Pinned
  ( Pinnedness(..)
  , ByteArray(..)
  , MutableByteArray(..)
  , new, newPinned
  ) where

import GHC.Exts
import Control.Monad.Primitive (PrimMonad(..))

import qualified Data.Primitive.ByteArray as P

data Pinnedness = Pinned | Unpinned

newtype ByteArray (p :: Pinnedness) = ByteArray P.ByteArray
  deriving newtype
    ( Eq, IsList, Monoid, Ord, Semigroup, Show
    )
type role ByteArray nominal

newtype MutableByteArray (p :: Pinnedness) s = MutableByteArray (P.MutableByteArray s)
type role MutableByteArray nominal nominal

new :: PrimMonad m
  => Int
  -> m (MutableByteArray 'Unpinned (PrimState m))
new !sz = do { marr <- P.newByteArray sz; pure (MutableByteArray marr) }

newPinned :: PrimMonad m
  => Int
  -> m (MutableByteArray 'Pinned (PrimState m))
newPinned !sz = do { marr <- P.newPinnedByteArray sz; pure (MutableByteArray marr) }

{-
byteArrayToByteString :: ByteArray 'Pinned -> ByteString
byteArrayToByteString (ByteArray (P.ByteArray b#)) =
  let contents# = byteArrayContents# b#
      fp = ForeignPtr contents# (PlainPtr (unsafeCoerce# b#))
      len = I# (sizeofByteArray# b#)
  in PS fp 0 len
-}

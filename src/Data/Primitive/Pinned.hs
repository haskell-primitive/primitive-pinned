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
      , TypeApplications
  #-}

module Data.Primitive.Pinned
  (
    -- * Types
    Pinnedness(..)
  , ByteArray(..)
  , MutableByteArray(..)

    -- * Creation
  , new
  , newPinned

    -- * Querying
  , size
  , sizeMutable

    -- * Conversion
  , unpinnedToPinned
  , pinnedToUnpinned
  , unsafeFreeze

    -- ** Conversion to/from 'Data.ByteString.ByteString'
  , pinnedToByteString
  , byteStringToPinned
  ) where

import GHC.Exts
import GHC.ST
import GHC.ForeignPtr
import Foreign.ForeignPtr
import GHC.Ptr (plusPtr)
import Data.Word (Word8)
import Data.Primitive.Foreign (peekArray)
import Control.Monad.Primitive (PrimMonad(..))
import Data.ByteString.Internal

import qualified Data.Primitive.ByteArray as P
import qualified Data.Primitive.PrimArray as P

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
new !sz = do
  marr <- P.newByteArray sz
  pure (MutableByteArray marr)
{-# inline new #-}

newPinned :: PrimMonad m
  => Int
  -> m (MutableByteArray 'Pinned (PrimState m))
newPinned !sz = do
  marr <- P.newPinnedByteArray sz
  pure (MutableByteArray marr)
{-# inline newPinned #-}

unsafeFreeze :: PrimMonad m
  => MutableByteArray p (PrimState m)
  -> m (ByteArray p)
unsafeFreeze (MutableByteArray marr) = ByteArray
  <$> P.unsafeFreezeByteArray marr
{-# inline unsafeFreeze #-}

size :: ByteArray p -> Int
size (ByteArray arr) = P.sizeofByteArray arr
{-# inline size #-}

sizeMutable :: PrimMonad m => MutableByteArray p (PrimState m) -> m Int
sizeMutable (MutableByteArray marr) = P.getSizeofMutableByteArray marr
{-# inline sizeMutable #-}

-- | Copy an 'Unpinned' 'Bytearray' into a 'Pinned' 'ByteArray'.
unpinnedToPinned :: ()
  => ByteArray 'Unpinned
  -> ByteArray 'Pinned
unpinnedToPinned b@(ByteArray unpinned) = ByteArray $ runST $ do
  let !sz = size b
  marr <- P.newPinnedByteArray sz
  P.copyByteArray marr 0 unpinned 0 sz
  P.unsafeFreezeByteArray marr
{-# inline unpinnedToPinned #-}

-- | Copy a 'Pinned' 'ByteArray' into an 'Unpinned' 'ByteArray'.
pinnedToUnpinned :: ()
  => ByteArray 'Pinned
  -> ByteArray 'Unpinned
pinnedToUnpinned b@(ByteArray pinned) = ByteArray $ runST $ do
  let !sz = size b
  marr <- P.newByteArray sz
  P.copyByteArray marr 0 pinned 0 sz
  P.unsafeFreezeByteArray marr
{-# inline pinnedToUnpinned #-}

pinnedToByteString :: ByteArray 'Pinned -> ByteString
pinnedToByteString (ByteArray (P.ByteArray b#)) =
  let contents# = byteArrayContents# b#
      fp = ForeignPtr contents# (PlainPtr (unsafeCoerce# b#))
      len = I# (sizeofByteArray# b#)
  in PS fp 0 len
{-# inline pinnedToByteString #-}

byteStringToPinned :: ByteString -> ByteArray 'Pinned
byteStringToPinned (PS fp off len) = accursedUnutterablePerformIO $
  withForeignPtr fp $ \ptr -> do
    (P.PrimArray bytearray#) <- peekArray @Word8 len (plusPtr ptr off)
    let b = ByteArray (P.ByteArray bytearray#)
    pure (unpinnedToPinned b)
{-# inline byteStringToPinned #-}


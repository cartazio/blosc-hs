{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Compression.Blosc where

import Prelude hiding (max)
import Data.Word
import Foreign.Ptr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative

import qualified Data.ByteString as B

import qualified Data.ByteString.Unsafe as BU
import GHC.Conc (getNumCapabilities)

import qualified Data.ByteString.Internal as BI (createAndTrim,ByteString(..))

import qualified  Data.Vector.Storable.Mutable as VSM
import qualified  Data.Vector.Storable as VS 

import  Foreign.ForeignPtr  (withForeignPtr)


{-int blosc_compress(int clevel, int doshuffle, size_t typesize, size_t nbytes,
                   const void *src, void *dest, size_t destsize);-}

{-  NOTE: is there a 4gb cap  on a compressed buffer??
    I think so... at least with the current docs on chunk size
 -}


-- | Compresses a string.
foreign import ccall unsafe "blosc.h blosc_compress"
  c_blosc_compress :: CInt -- ^ Compression Level
                 -> CInt -- ^ Shuffle  0 no , 1 yes
                 -> CSize      -- ^ element type size in bytes , shuffle only happens when >1 byte
                 -> CSize    -- ^ input buffer size in bytes
                 ---- lets try doing Word8 instead of CChar!
                 -> Ptr CChar  -- ^ Pointer to input buffer
                 -> Ptr Word8 -- ^ Pointer to result / destination buffer buffer
                 -- Word8 needed by the createAndTrim type
                 -> CSize --- ^  Destination buffer must be at least this size, 
                 -> IO CInt   -- ^ Size of compressed Buffer





--int blosc_decompress(const void *src, void *dest, size_t destsize)
foreign import ccall unsafe "blosc.h blosc_decompress"
    c_blosc_decompress :: Ptr CChar  --- ^ input (compressed) buffer
                    ->  Ptr Word8 -- ^ Destination decomp buffer                
                    -> CSize -- ^ Size of Destination buffer
                    -> IO CInt --- ^ bytes used in dest buffer or error

{-  
int blosc_set_nthreads(int nthreads);-}                    

foreign import  ccall unsafe "blosc.h blosc_set_nthreads"
    c_blosc_set_nthread :: CInt -- ^ New number of threads to use
                        -> IO CInt  -- ^ Previous number of Blosc Threads


foreign import ccall unsafe "blosc.hs blosc_free_resources"
    c_blosc_free_resources :: IO ()

foreign import ccall unsafe "blosc.hs blosc_cbuffer_sizes"
    c_blosc_cbuffer_sizes :: Ptr CChar --- ^ just needs to contain the min header number of bytes of the true header!
                -> Ptr CSize -- ^ will be updated to indicate DEcompressed size
                -> Ptr CSize -- ^ will indicate the Compressed (current) size
                -> Ptr CSize --- ^ Will indicate the block size --- which is what ??
                -> IO ()

{-  
/**
  Get `nitems` (of typesize size) in `src` buffer starting in `start`.
  The items are returned in `dest` buffer, which has to have enough
  space for storing all items.  Returns the number of bytes copied to
  `dest` or a negative value if some error happens.
 */

int blosc_getitem(const void *src, int start, int nitems, void *dest);


(assumes you have the full buffer, because I think this is reading the header
info)

NOTE: check assumption of this
-}                

foreign import ccall unsafe "blosc.hs blosc_getitem"
    c_blosc_getitem :: Ptr CChar 
            -> CInt --- ^ start element
            -> CInt -- ^ number of elements
            -> Ptr Word8 -- ^ result buffer 
            -> IO CInt --- ^ Number of bytes used in result buffer 






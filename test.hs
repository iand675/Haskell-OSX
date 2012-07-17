import Foreign.Ptr
import System.IO.Unsafe
import System.Posix.DynamicLinker

main = do
  print abortModalException

abortModalException :: Ptr a
{-# NOINLINE abortModalException #-}
abortModalException = castFunPtrToPtr $ unsafePerformIO $ dlsym Default "NSAbortModalException"


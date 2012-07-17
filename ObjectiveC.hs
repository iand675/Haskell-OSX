module Foreign.ObjectiveC where
import Foreign.Ptr

class IsObject a where {}
class IsClass a where {}
class IsMetaClass a where {}

newtype Object = Object { fromObject :: Ptr () }
newtype Selector = Selector { fromSelector :: Ptr () }


(#) :: Object -> Selector -> IO (Ptr ())
(#) obj sel = return nullPtr

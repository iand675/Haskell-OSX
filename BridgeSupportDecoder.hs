
module BridgeSupportDecoder where
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import qualified Text.XML.Cursor.Generic as G

import           TypeDecoder

data CFType = CFType { cfName :: T.Text
                     , cfType :: Maybe ObjCType
                     , cfGetTypeIdFunc :: Maybe T.Text
                     , cfTollfree :: Maybe T.Text }
                     deriving (Read, Show)

data BridgeStruct = Struct { stName :: T.Text
                           , stType :: Maybe ObjCType
                           , stOpaque :: Bool }
                           deriving (Read, Show)

data BridgeSupportFile = BridgeSupportFile { cfTypes :: [CFType]
                                           , structs :: [BridgeStruct]
                                           }
                                           deriving (Read, Show)

readBridgeSupportFile fp = do
  c <- fmap (child . fromDocument) $ Text.XML.readFile def fp
  return $! BridgeSupportFile (concatMap getCFTypes c) (concatMap getStructs c)

asCFType c = [ CFType { cfName = head $ laxAttribute "name" c
                      , cfType = decodeType $ head $ laxAttribute "type" c
                      , cfGetTypeIdFunc = Nothing
                      , cfTollfree = Nothing
                      }
             ]

asStruct c = [ Struct { stName = head $ laxAttribute "name" c
                      , stType = decodeType $ head $ laxAttribute "type" c
                      , stOpaque = True
                      }
             ]

getCFTypes = laxElement "cftype" >=> asCFType
getStructs = laxElement "struct" >=> asStruct




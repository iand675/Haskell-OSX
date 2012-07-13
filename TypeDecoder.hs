module TypeDecoder where
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List

type Name = Text

data ObjCType = OChar
              | OInt
              | OShort
              | OLong
              | OLongLong
              | OUChar
              | OUInt
              | OUShort
              | OULong
              | OULongLong
              | OFloat
              | ODouble
              | OBool
              | OVoid
              | OString
              | OObject
              | OClass
              | OSelector
              | OArray Int ObjCType
              | OStruct Name [(Maybe Name, ObjCType)]
              | OUnion Name [(Maybe Name, ObjCType)]
              | OBitField Int
              | OPtr ObjCType
              | OUnknown
              deriving (Read, Show, Eq)

decodeType :: Text -> Maybe ObjCType
decodeType = maybeResult . parse objcTypes

means c t = char c *> pure t

basics = foldl1' (<|>) $ map (uncurry means) basicTypes

basicTypes = [ ('c', OChar), ('i', OInt), ('s', OShort), ('l', OLong), 
               ('q', OLongLong), ('C', OUChar), ('I', OUInt), ('S', OUShort), 
               ('L', OULong), ('Q', OULongLong), ('f', OFloat), ('d', ODouble),
               ('B', OBool), ('v', OVoid), ('*', OString), ('@', OObject),
               ('#', OClass), (':', OSelector), ('?', OUnknown) ]

startNameChars = ['_'] ++ ['a'..'z'] ++ ['A'..'Z']

oname = takeWhile1 (`elem` (['0'..'9'] ++ startNameChars))

array = do
  char '['
  arrLength <- decimal
  internalType <- objcTypes
  char ']'
  return $! OArray arrLength internalType

struct = do
  char '{'
  n <- oname
  internalType <-internalStructure
  char '}'
  return $! OStruct n internalType

ounion = do
  char '('
  n <- oname
  internalType <- internalStructure
  char ')'
  return $! OUnion n internalType

internalStructure = (char '=' *> many namedType) <|> pure []

namedType = do
  mname <- (Just <$> named) <|> pure Nothing
  internalType <- objcTypes
  return $! (mname, internalType)
  where 
    named = do
      char '"'
      m <- oname
      char '"'
      return $! m

bitfield = do
  char 'b'
  numBits <- decimal
  return $! OBitField numBits

ptr = do
  char '^'
  internalType <- objcTypes
  return $! OPtr internalType

objcTypes = basics <|> array <|> struct <|> ounion <|> bitfield <|> ptr

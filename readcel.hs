import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Binary.Get
import Data.Word
import Data.Char
import Data.Int
import Data.List (intersperse)
import Control.Applicative

type CelByte     = Int8
type CelUByte    = Word8
type CelShort    = Int16
type CelUShort   = Word16
type CelInt      = Int32
type CelUInt     = Word32
type CelFloat    = Float
type CelDouble   = Double
type CelGUID     = CelString
type CelDateTime = CelWString
type CelLocale   = CelWString
type CelChar     = Word8
type CelWChar    = Word16

-- strings
type CelString  = BS.ByteString
celString = BSC.pack

data CelWString = CelWString { wstring :: BS.ByteString } deriving (Eq)

instance Show CelWString where
  show (CelWString ws) = show $ keepEvery 2 $ BSC.unpack ws

celWString :: [Char] -> CelWString
celWString s = CelWString $ BSC.pack $ '\0':(intersperse '\0' s)

dropEvery n xs = [ i | (i, c) <- zip xs [0, 1 ..], (mod c n) /= n-1]
keepEvery n xs = [ i | (i, c) <- zip xs [0, 1 ..], (mod c n) == n-1]



{-
 -
 - primitive parsers of basic Cel types
 -
 -}
parseCelByte :: Get CelByte
parseCelByte = fromIntegral <$> getWord8

parseCelShort :: Get CelShort
parseCelShort = fromIntegral <$> getWord16be

parseCelInt :: Get CelInt
parseCelInt = fromIntegral <$> getWord32be

parseCelUByte :: Get CelUByte
parseCelUByte = getWord8

parseCelUShort :: Get CelUShort
parseCelUShort = getWord16be

parseCelUInt :: Get CelUInt
parseCelUInt = getWord32be

parseCelString :: Get CelString
parseCelString = do
  len <- parseCelInt
  s   <- getLazyByteString $ fromIntegral len
  return s

parseCelWString :: Get CelWString
parseCelWString = do
  len <- parseCelInt
  s   <- getLazyByteString $ fromIntegral (2*len)
  return $ CelWString s
 
parseCelGUID :: Get CelGUID
parseCelGUID = parseCelString

parseCelDateTime :: Get CelDateTime
parseCelDateTime = parseCelWString

parseCelLocale :: Get CelLocale
parseCelLocale = parseCelWString


{-
 -
 - CelHeader
 -
 -}
data CelHeader = CelHeader { magic       :: CelUByte
                           , version     :: CelUByte
                           , nDataGroups :: CelInt
                           , fstGroupPos :: CelUInt
                           }

instance Show CelHeader where
  show (CelHeader m v n f) =
                                 "Cel file header:\n" ++
    "magic number:                " ++ show m ++ "\n" ++
    "version:                     " ++ show v ++ "\n" ++
    "no. of data groups           " ++ show n ++ "\n" ++
    "position of the first group: " ++ show f ++ "\n"

                                  

parseCelHeader :: Get CelHeader
parseCelHeader = do
  magic   <- parseCelUByte
  version <- parseCelUByte
  nGroups <- parseCelInt
  pos     <- parseCelUInt
  return $ CelHeader magic version nGroups pos


{-
 -
 - Cel MIME types
 -
 -}
data CelMIMEType = CelMIMEPlainText | CelMIMEFloat
                 | CelMIMEUInt32    | CelMIMEUInt16 | CelMIMEUInt8
                 | CelMIMEInt32     | CelMIMEInt16  | CelMIMEInt8
                 deriving (Show)

parseMIMEType :: Get CelMIMEType
parseMIMEType = do
  s <- parseCelWString
  case (lookup s
    [(celWString "text/x-calvin-integer-8"          , CelMIMEInt8)
    ,(celWString "text/x-calvin-integer-16"         , CelMIMEInt16)
    ,(celWString "text/x-calvin-integer-32"         , CelMIMEInt32)
    ,(celWString "text/x-calvin-unsigned-integer-8" , CelMIMEUInt8)
    ,(celWString "text/x-calvin-unsigned-integer-16", CelMIMEUInt16)
    ,(celWString "text/x-calvin-unsigned-integer-32", CelMIMEUInt32)
    ,(celWString "text/x-calvin-float"              , CelMIMEFloat)
    ,(celWString "text/plain"                       , CelMIMEPlainText)]) of
      Just t -> return t
      _      -> return $ error "undefined MIME type"


newtype CelMIMEString = CelMIMEString { mimeString :: CelString }

showMIMEString :: CelMIMEType -> CelMIMEString -> [Char]
showMIMEString CelMIMEPlainText (CelMIMEString s) = 
  show $ keepEvery 2 $ BSC.unpack s
showMIMEString _ (CelMIMEString s) = "unprintable MIME value"
{-
 -
 - Cel name/value/type triplet
 -
 -}
data CelNVTTriplet = CelNVTTriplet { nvtName  :: CelWString
                                   , nvtValue :: CelMIMEString
                                   , nvtType  :: CelMIMEType
                                   } 
instance Show CelNVTTriplet where
  show (CelNVTTriplet n v t) = "name:"  ++ show n ++ "," ++ 
                               "value:" ++ showMIMEString t v ++ "," ++
                               "type:"  ++ show t

parseCelNVTTriplet :: Get CelNVTTriplet
parseCelNVTTriplet = do
  name  <- parseCelWString
  value <- parseCelString
  typ   <- parseMIMEType
  return $ CelNVTTriplet name (CelMIMEString value) typ

parseCelNVTTriplets :: Int -> Get [CelNVTTriplet]
parseCelNVTTriplets n
  | n <= 0 = do
    return []
  | otherwise = do
    nvt  <- parseCelNVTTriplet
    nvts <- parseCelNVTTriplets $ fromIntegral n-1
    return $ nvt:nvts

{-
 -
 - Cel data header
 -
 -}

data DataHeader = DataHeader { dataId   :: CelString
                             , guId     :: CelGUID
                             , datetime :: CelDateTime
                             , locale   :: CelLocale
                             , nNVT     :: CelInt
                             , nvts     :: [CelNVTTriplet]
                             }
instance Show DataHeader where
  show (DataHeader id guid dt locale nNVT nvts) = 
                              "Cel data header:\n" ++
    "id:                 "  ++ show id     ++ "\n" ++
    "guid:               "  ++ show guid   ++ "\n" ++
    "date/time:          "  ++ show dt     ++ "\n" ++
    "guid:               "  ++ show locale ++ "\n" ++
    "no of nvt triplets: "  ++ show nNVT   ++ "\n" ++
    "nvt triplets:     \n"  ++ show nvts   ++ "\n"

parseDataHeader :: Get DataHeader
parseDataHeader = do
  dataId   <- parseCelString
  guId     <- parseCelGUID
  datetime <- parseCelDateTime
  locale   <- parseCelLocale
  nNVT     <- parseCelInt
  nvts     <- parseCelNVTTriplets $ fromIntegral nNVT
  return $ DataHeader dataId guId datetime locale nNVT nvts

{-
 - a small testing function
 -}

parseCelHeaderAndDataHeader :: Get (CelHeader, DataHeader)
parseCelHeaderAndDataHeader = do
  ch <- parseCelHeader
  dh <- parseDataHeader
  return (ch, dh)


processCel cel = res
  where res  = runGet parseCelHeaderAndDataHeader cel

main = do
  cel <- BS.readFile "array.cel"
  return $ processCel cel


test = BS.pack [0, 0, 0, 2, 0x30, 0x31, 0x32, 0x33]

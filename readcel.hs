import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Binary.Get
import Data.Binary.IEEE754
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

parseNThings :: Get a -> Int -> Get [a]
parseNThings parseThing n
  | n <= 0 = do
    return []
  | otherwise = do
    thing  <- parseThing
    things <- parseNThings parseThing $ fromIntegral n-1
    return $ thing:things


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
  show $ filter (/= '\0') $ keepEvery 2 $ BSC.unpack s
showMIMEString CelMIMEInt8  (CelMIMEString s) =
  show $ runGet parseCelByte s
showMIMEString CelMIMEUInt8  (CelMIMEString s) =
  show $ runGet parseCelUByte s
showMIMEString CelMIMEInt16  (CelMIMEString s) =
  show $ runGet parseCelShort s
showMIMEString CelMIMEUInt16 (CelMIMEString s) =
  show $ runGet parseCelUShort s
showMIMEString CelMIMEInt32  (CelMIMEString s) =
  show $ runGet parseCelInt s
showMIMEString CelMIMEUInt32 (CelMIMEString s) =
  show $ runGet parseCelUInt s
showMIMEString CelMIMEFloat (CelMIMEString s) =
  show $ runGet getFloat32be s
-- showMIMEString _ (CelMIMEString s) = "unprintable MIME value"

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
  show (CelNVTTriplet n v t) = "name: "  ++ show n ++ ", " ++ 
                               "value: " ++ showMIMEString t v ++ ", " ++
                               "type: "  ++ show t

parseCelNVTTriplet :: Get CelNVTTriplet
parseCelNVTTriplet = do
  name  <- parseCelWString
  value <- parseCelString
  typ   <- parseMIMEType
  return $ CelNVTTriplet name (CelMIMEString value) typ

parseCelNVTTriplets = parseNThings parseCelNVTTriplet 

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
    "id:                 "  ++ show id     ++ "\n" ++
    "guid:               "  ++ show guid   ++ "\n" ++
    "date/time:          "  ++ show dt     ++ "\n" ++
    "guid:               "  ++ show locale ++ "\n" ++
    "no of nvt triplets: "  ++ show nNVT   ++ "\n" ++
    "nvt triplets:     \n"  ++ showL nvts  ++ "\n"
    where
      showL []     = ""
      showL (x:xs) = show x ++ "\n" ++ showL xs

parseDataHeader :: Get DataHeader
parseDataHeader = do
  dataId   <- parseCelString
  guId     <- parseCelGUID
  datetime <- parseCelDateTime
  locale   <- parseCelLocale
  nNVT     <- parseCelInt
  nvts     <- parseCelNVTTriplets $ fromIntegral nNVT
  return $ DataHeader dataId guId datetime locale nNVT nvts


data CelFile = CelFile { header     :: CelHeader
                       , dataHeader :: DataHeader
                       }

instance Show CelFile where
  show (CelFile h dh) = "Cel file header:\n" ++ show h ++
                        "Cel data header:\n" ++ show dh

parseCelFile :: Get CelFile
parseCelFile = do
  h <- parseCelHeader
  dh <- parseDataHeader
  return $ CelFile h dh


processCel cel = res
  where res  = runGet parseCelFile cel

main = do
  cel <- BS.readFile "array.cel"
  return $ processCel cel


test = BS.pack [0, 0, 0, 2, 0x30, 0x31, 0x32, 0x33]

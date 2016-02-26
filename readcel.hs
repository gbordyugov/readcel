-- import qualified Data.ByteString.Lazy.UTF8 as BSU
import qualified Data.Text.Encoding         as DTE
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Char8      as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Char
import Data.Int
import Data.List (intersperse)

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
type CelString  = BSL.ByteString
celString = BSLC8.pack

lazyToStrict = BS.concat . BSL.toChunks
showWS       = show . DTE.decodeUtf16BE . lazyToStrict

data CelWString = CelWString BSL.ByteString deriving (Eq)

instance Show CelWString where
  show (CelWString ws) = showWS ws

celWString :: [Char] -> CelWString
celWString s = CelWString $ BSLC8.pack $ '\0':(intersperse '\0' s)


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
data CelMIMEType = CelMIMEPlainText | CelMIMEFloat  | CelMIMEAscii
                 | CelMIMEUInt32    | CelMIMEUInt16 | CelMIMEUInt8
                 | CelMIMEInt32     | CelMIMEInt16  | CelMIMEInt8
                 deriving (Show)

typeLUTable =
  [ (celWString "text/x-calvin-integer-8"           , CelMIMEInt8)
  , (celWString "text/x-calvin-integer-16"          , CelMIMEInt16)
  , (celWString "text/x-calvin-integer-32"          , CelMIMEInt32)
  , (celWString "text/x-calvin-unsigned-integer-8"  , CelMIMEUInt8)
  , (celWString "text/x-calvin-unsigned-integer-16" , CelMIMEUInt16)
  , (celWString "text/x-calvin-unsigned-integer-32" , CelMIMEUInt32)
  , (celWString "text/x-calvin-float"               , CelMIMEFloat)
  , (celWString "text/ascii"                        , CelMIMEAscii)
  , (celWString "text/plain"                        , CelMIMEPlainText)
  ]

parseMIMEType :: Get CelMIMEType
parseMIMEType = do
  s <- parseCelWString
  case (lookup s typeLUTable) of
      Just t -> return t
      _      -> error $ show "undefined MIME type: " ++ show s


newtype CelMIMEString = CelMIMEString { mimeString :: CelString }

showMIMEString :: CelMIMEType -> CelMIMEString -> [Char]
showMIMEString CelMIMEPlainText (CelMIMEString s) = 
  -- show $ filter (/= '\0') $ keepEvery 2 $ BSLC8.unpack s
  showWS s
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
showMIMEString CelMIMEFloat  (CelMIMEString s) =
  show $ runGet getFloat32be s
showMIMEString CelMIMEAscii  (CelMIMEString s) = show s

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
  show (CelNVTTriplet n v t) = "name: "  ++ show n             ++ ", " ++ 
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

data CelDataHeader = CelDataHeader { dataId   :: CelString
                                   , guId     :: CelGUID
                                   , datetime :: CelDateTime
                                   , locale   :: CelLocale
                                   , nNVT     :: CelInt
                                   , nvts     :: [CelNVTTriplet]
                                   , nparents :: CelInt
                                   , parents  :: [CelDataHeader]
                                   }

instance Show CelDataHeader where
  show (CelDataHeader id guid dt locale nNVT nvts np ps) = 
    "id:                 "  ++ show id     ++ "\n" ++
    "guid:               "  ++ show guid   ++ "\n" ++
    "date/time:          "  ++ show dt     ++ "\n" ++
    "locale:             "  ++ show locale ++ "\n" ++
    "no of nvt triplets: "  ++ show nNVT   ++ "\n" ++
    -- "nvt triplets:     \n"  ++ showL nvts  ++ "\n" ++
    "no of parents :     "  ++ show np     ++ "\n" ++
    "parents:          \n"  ++ showL ps ++ "\n" 
    -- ++ "end of " ++ show id ++ " data header"

showL []     = "eol"
showL (x:xs) = show x ++ "\n" ++ showL xs

parseCelDataHeader :: Get CelDataHeader
parseCelDataHeader = do
  dataId   <- parseCelString
  guId     <- parseCelGUID
  datetime <- parseCelDateTime
  locale   <- parseCelLocale
  nNVT     <- parseCelInt
  nvts     <- parseCelNVTTriplets $ fromIntegral nNVT
  np       <- parseCelInt
  parents  <- parseCelDataHeaders $ fromIntegral np
  return $ CelDataHeader dataId guId datetime locale nNVT nvts np parents

{-
 - still work in progress
 -}

parseCelDataHeaders= parseNThings parseCelDataHeader

data CelDataGroup = CelDataGroup { posNextDataGroup :: CelUInt
                                 , posFirstDataSet  :: CelUInt
                                 , noDataSets       :: CelInt
                                 , dgName           :: CelWString
                                 }
instance Show CelDataGroup where
  show (CelDataGroup np fp n name) = 
    "Data group:               " ++ show name ++ "\n" ++
    "no of data sets:          " ++ show n    ++ "\n" ++
    "next data group position: " ++ show np   ++ "\n" ++
    "first data set position:  " ++ show fp

parseDataGroup = do
  np   <- parseCelUInt
  fp   <- parseCelUInt
  no   <- parseCelInt
  name <- parseCelWString
  return $ CelDataGroup np fp no name


{-
 - end of work in progress
 -}

data CelFile = CelFile { celHeader  :: CelHeader
                       , dataHeader :: CelDataHeader
                       -- , dg         :: CelDataGroup
                       }

instance Show CelFile where
  show (CelFile h dh) = "Cel file header:\n" ++ show h ++
                        "Cel data header:\n" ++ show dh -- ++
                           -- "Data group     :\n" ++ show dg

parseCelFile :: Get CelFile
parseCelFile = do
  h  <- parseCelHeader
  dh <- parseCelDataHeader
  return $ CelFile h dh 


processCel cel = res
  where res = runGet parseCelFile cel

main = do
  cel <- BSL.readFile "array.cel"
  return $ processCel cel


-- test = BSL.pack [0, 0, 0, 2, 0x30, 0x31, 0x32, 0x33]
test =  BSL.pack [0x00, 0x30, 0x00, 0x31, 0x00, 0x32, 0x00, 0x33]

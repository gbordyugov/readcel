import qualified Data.Text                 as DT
import qualified Data.Text.Encoding        as DTE
import qualified Data.Text.Lazy.Encoding   as DTLE
import qualified Data.ByteString.Lazy      as BSL
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
type CelGUID     = CelText
type CelDateTime = CelText
type CelLocale   = CelText
type CelChar     = Word8
type CelWChar    = Word16

-- strings
type CelText = DT.Text
celText = DT.pack

-- type CelString  = DT.Text
-- celString = DT.pack

-- type CelWString = DT.Text
-- celWString = DT.pack


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

parseLazyByteString :: Int -> Get BSL.ByteString
parseLazyByteString f = do
  len <- parseCelInt
  s   <- getLazyByteString $ (fromIntegral f)*(fromIntegral len)
  return s

parseLazyByteStringFromString  = parseLazyByteString 1
parseLazyByteStringFromWString = parseLazyByteString 2

parseCelTextFromString :: Get CelText
parseCelTextFromString = do
  len <- parseCelInt
  s   <- getByteString $ fromIntegral len
  return $ DT.takeWhile (/='\0') $ DTE.decodeUtf8 s

parseCelTextFromWString :: Get CelText
parseCelTextFromWString = do
  len <- parseCelInt
  s   <- getByteString $ fromIntegral (2*len)
  return $ DT.takeWhile (/= '\0') $ DTE.decodeUtf16BE s
 
parseCelGUID :: Get CelGUID
parseCelGUID = parseCelTextFromString

parseCelDateTime :: Get CelDateTime
parseCelDateTime = parseCelTextFromWString

parseCelLocale :: Get CelLocale
parseCelLocale = parseCelTextFromWString

parseNThings :: Get a -> Int -> Get [a]
parseNThings parseThing n
  | n <= 0 = do
    return []
  | otherwise = do
    t  <- parseThing
    ts <- parseNThings parseThing $ fromIntegral n-1
    return $ t:ts


{-  
 -  
 -   CelHeader
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


data CelParameter = CelMIMEPlainText DT.Text
                  | CelMIMEFloat     Float
                  | CelMIMEAscii     DT.Text
                  | CelMIMEUInt8     Word8
                  | CelMIMEUInt32    Word32
                  | CelMIMEUInt16    Word16
                  | CelMIMEInt8      Int8
                  | CelMIMEInt16     Int16
                  | CelMIMEInt32     Int32
                    deriving (Show,Eq)

data CelNamedParameter = CelNamedParameter DT.Text CelParameter
                           deriving (Show, Eq)

parseCelNamedParameter :: Get CelNamedParameter
parseCelNamedParameter = do
  n <- parseCelTextFromWString
  v <- parseLazyByteStringFromString
  t <- parseCelTextFromWString
  case (DT.unpack t) of
    "text/x-calvin-integer-8" ->
      return $ CelNamedParameter n $ CelMIMEInt8 $ runGet getInt8 v
    "text/x-calvin-integer-16" ->
      return $ CelNamedParameter n $ CelMIMEInt16 $ runGet getInt16be v
    "text/x-calvin-integer-32" ->
      return $ CelNamedParameter n $ CelMIMEInt32 $ runGet getInt32be v
    "text/x-calvin-unsigned-integer-8" ->
      return $ CelNamedParameter n $ CelMIMEUInt8 $ runGet getWord8 v
    "text/x-calvin-unsigned-integer-16" ->
      return $ CelNamedParameter n $ CelMIMEUInt16 $ runGet getWord16be v
    "text/x-calvin-unsigned-integer-32" ->
      return $ CelNamedParameter n $ CelMIMEUInt32 $ runGet getWord32be v
    "text/x-calvin-float"                ->
      return $ CelNamedParameter n $ CelMIMEFloat $ runGet getFloat32be v
    "text/ascii" ->
      return $ CelNamedParameter n $ CelMIMEAscii $ DT.takeWhile (/='\0') $ DTE.decodeUtf8 $ BSL.toStrict v
    "text/plain" ->
      return $ CelNamedParameter n $ CelMIMEPlainText $ DT.takeWhile (/='\0') $ DTE.decodeUtf16BE $ BSL.toStrict v
    _ -> error $ show "undefined MIME type: " ++ show t

parseCelNamedParameters = parseNThings parseCelNamedParameter

{-
 -
 - Cel data header
 -
 -}

data CelDataHeader = CelDataHeader { dataId   :: CelText
                                   , guId     :: CelGUID
                                   , datetime :: CelDateTime
                                   , locale   :: CelLocale
                                   , nPars    :: CelInt
                                   , pars     :: [CelNamedParameter]
                                   , nparents :: CelInt
                                   , parents  :: [CelDataHeader]
                                   }

instance Show CelDataHeader where
  show (CelDataHeader id guid dt locale nPars pars np ps) = 
       "id:                 "  ++ show id     ++ "\n"
    ++ "guid:               "  ++ show guid   ++ "\n"
    ++ "date/time:          "  ++ show dt     ++ "\n"
    ++ "locale:             "  ++ show locale ++ "\n"
    ++ "no of nvt triplets: "  ++ show nPars  ++ "\n"
    ++ "nvt triplets:     \n"  ++ show pars   ++ "\n"
    ++ "no of parents :     "  ++ show np     ++ "\n"
    ++ "parents:          \n"  ++ show ps     ++ "\n" 

showL []     = "eol"
showL (x:xs) = show x ++ "\n" ++ showL xs

parseCelDataHeader :: Get CelDataHeader
parseCelDataHeader = do
  dataId   <- parseCelTextFromString
  guId     <- parseCelGUID
  datetime <- parseCelDateTime
  locale   <- parseCelLocale
  nPars    <- parseCelInt
  pars     <- parseCelNamedParameters $ fromIntegral nPars
  np       <- parseCelInt
  parents  <- parseCelDataHeaders $ fromIntegral np
  return $ CelDataHeader dataId guId datetime locale nPars pars np parents

{-
 - still work in progress
 -}

parseCelDataHeaders= parseNThings parseCelDataHeader

data CelDataGroup = CelDataGroup { posNextDataGroup :: CelUInt
                                 , posFirstDataSet  :: CelUInt
                                 , noDataSets       :: CelInt
                                 , dgName           :: CelText
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
  name <- parseCelTextFromWString
  return $ CelDataGroup np fp no name


{-
 - end of work in progress
 -}

data CelFile = CelFile { celHeader  :: CelHeader
                       , dataHeader :: CelDataHeader
                       -- , dg         :: CelDataGroup
                       }

instance Show CelFile where
  show (CelFile h dh) = "Cel file header:\n" ++ show h
                     ++ "Cel data header:\n" ++ show dh -- ++
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

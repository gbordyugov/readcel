import qualified Data.Text                 as DT
import qualified Data.Text.Encoding        as DTE
import qualified Data.ByteString.Lazy      as BSL
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Char
import Data.Int

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
type CelText     = DT.Text
celText = DT.pack

{-
 -
 - primitive parsers of basic Cel types
 -
 -}
parseCelByte   = fromIntegral <$> getWord8
parseCelShort  = fromIntegral <$> getWord16be
parseCelInt    = fromIntegral <$> getWord32be
parseCelUByte  = getWord8
parseCelUShort = getWord16be
parseCelUInt   = getWord32be

parseGenericByteString parser charWidth = do
  len <- parseCelInt
  s   <- parser $ (fromIntegral charWidth)*(fromIntegral len)
  return s

parseByteString     = parseGenericByteString getByteString
parseLazyByteString = parseGenericByteString getLazyByteString

parseLazyByteStringFromString  = parseLazyByteString 1
parseLazyByteStringFromWString = parseLazyByteString 2

parseCelTextFromString = do
  s <- parseByteString 1
  return $ DT.takeWhile (/='\0') $ DTE.decodeUtf8 s

parseCelTextFromWString = do
  s <- parseByteString 2
  return $ DT.takeWhile (/= '\0') $ DTE.decodeUtf16BE s
 
parseCelGUID     = parseCelTextFromString
parseCelDateTime = parseCelTextFromWString
parseCelLocale   = parseCelTextFromWString

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
data CelHeader = CelHeader CelUByte -- magic
                           CelUByte -- version
                           CelInt   -- nDataGroups
                           CelUInt  -- fstGroupPos

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


data CelParameter = CelParameterPlainText DT.Text
                  | CelParameterFloat     Float
                  | CelParameterAscii     DT.Text
                  | CelParameterUInt8     Word8
                  | CelParameterUInt32    Word32
                  | CelParameterUInt16    Word16
                  | CelParameterInt8      Int8
                  | CelParameterInt16     Int16
                  | CelParameterInt32     Int32
                    deriving (Show,Eq)

data CelNamedParameter = CelNamedParameter DT.Text CelParameter
                           deriving (Eq)

instance Show CelNamedParameter where
  show (CelNamedParameter t p) = show t ++ ": " ++ show p

parseCelNamedParameter :: Get CelNamedParameter
parseCelNamedParameter = do
  n <- parseCelTextFromWString
  v <- parseLazyByteStringFromString
  t <- parseCelTextFromWString
  case (DT.unpack t) of
    "text/x-calvin-integer-8" ->
      return $ ctor n $ CelParameterInt8      $ runGet getInt8 v
    "text/x-calvin-integer-16" ->
      return $ ctor n $ CelParameterInt16     $ runGet getInt16be v
    "text/x-calvin-integer-32" ->
      return $ ctor n $ CelParameterInt32     $ runGet getInt32be v
    "text/x-calvin-unsigned-integer-8" ->
      return $ ctor n $ CelParameterUInt8     $ runGet getWord8 v
    "text/x-calvin-unsigned-integer-16" ->
      return $ ctor n $ CelParameterUInt16    $ runGet getWord16be v
    "text/x-calvin-unsigned-integer-32" ->
      return $ ctor n $ CelParameterUInt32    $ runGet getWord32be v
    "text/x-calvin-float" ->
      return $ ctor n $ CelParameterFloat     $ runGet getFloat32be v
    "text/ascii" ->
      return $ ctor n $ CelParameterAscii     $ du8 v
    "text/plain" ->
      return $ ctor n $ CelParameterPlainText $ du16 v
    _ -> error $ show "undefined MIME type: " ++ show t
    where
      ctor = CelNamedParameter
      f    = DT.takeWhile (/='\0')
      du8  = f . DTE.decodeUtf8    . BSL.toStrict
      du16 = f . DTE.decodeUtf16BE . BSL.toStrict

parseCelNamedParameters = parseNThings parseCelNamedParameter

{-
 -
 - Cel data header
 -
 -}

data CelDataHeader = CelDataHeader CelText             -- dataId
                                   CelGUID             -- guId
                                   CelDateTime         -- datetime
                                   CelLocale           -- locale
                                   CelInt              -- nPars
                                   [CelNamedParameter] -- pars
                                   CelInt              -- nparents
                                   [CelDataHeader]     -- parents

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

parseCelDataHeaders = parseNThings parseCelDataHeader

data CelDataGroup = CelDataGroup CelUInt -- posNextDataGroup
                                 CelUInt -- posFirstDataSet
                                 CelInt  -- noDataSets
                                 CelText -- dgName
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


data CelValueType = CelValueTypeByte
                  | CelValueTypeUByte
                  | CelValueTypeShort
                  | CelValueTypeUShort
                  | CelValueTypeInt
                  | CelValueTypeUInt
                  | CelValueTypeFloat
                  | CelValueTypeString
                  | CelValueTypeWString
                  deriving (Eq, Show)
parseCelValueType = do
  i <- parseCelByte
  case i of
    0 -> return CelValueTypeByte
    1 -> return CelValueTypeUByte
    2 -> return CelValueTypeShort
    3 -> return CelValueTypeUShort
    4 -> return CelValueTypeUInt
    5 -> return CelValueTypeUInt
    6 -> return CelValueTypeFloat
    7 -> return CelValueTypeString
    8 -> return CelValueTypeWString
    _ -> error "strange type byte"

data CelColumnName = CelColumnName CelText      -- column name
                                   CelValueType -- volume type
                                   CelInt       -- type size
                                   deriving (Eq, Show)
parseCelColumnName = do
  n <- parseCelTextFromWString
  t <- parseCelValueType
  s <- parseCelInt
  return $ CelColumnName n t s

parseCelColumnNames = parseNThings parseCelColumnName

-- not working yet
data CelRow = CelRowByte    CelByte
            | CelRowUByte   CelUByte
            | CelRowShort   CelShort
            | CelRowUShort  CelUShort
            | CelRowInt     CelInt
            | CelRowUInt    CelUInt
            | CelRowFloat   CelFloat
            | CelRowString  CelText
            | CelRowWString CelText
            deriving (Eq, Show)

-- not working yet
data CelDataSet = CelDataSet CelUInt             -- fPosFirstEle
                             CelUInt             -- fPosNextDataSet
                             CelText             -- name
                             CelInt              -- nPars
                             [CelNamedParameter] -- pars
                             CelUInt             -- nCols
                             [CelColumnName]     -- colNames
                             CelUInt             -- nRows
                             [CelRow]            -- data rows
                             deriving (Eq, Show)

-- not working yet
parseCelDataSet = do
  fp1   <- parseCelUInt
  fpn   <- parseCelUInt
  fpn   <- parseCelUInt
  name  <- parseCelTextFromWString
  nPar  <- parseCelInt
  pars  <- parseCelNamedParameters $ fromIntegral nPar
  nCol  <- parseCelUInt
  cols  <- parseCelColumnNames $ fromIntegral nCol
  nRows <- parseCelUInt 
  return fp1 

{-
 - end of work in progress
 -}

data CelFile = CelFile { celHeader  :: CelHeader
                       , dataHeader :: CelDataHeader
                       , dg         :: CelDataGroup
                       }

instance Show CelFile where
  show (CelFile h dh dg) = "Cel file header:\n" ++ show h
                        ++ "Cel data header:\n" ++ show dh
                        ++ "Data group     :\n" ++ show dg

parseCelFile :: Get CelFile
parseCelFile = do
  h  <- parseCelHeader
  dh <- parseCelDataHeader
  dg <- parseDataGroup
  return $ CelFile h dh dg


processCel cel = res
  where res = runGet parseCelFile cel

main = do
  cel <- BSL.readFile "array.cel"
  return $ processCel cel


-- test = BSL.pack [0, 0, 0, 2, 0x30, 0x31, 0x32, 0x33]
test =  BSL.pack [0x00, 0x30, 0x00, 0x31, 0x00, 0x32, 0x00, 0x33]

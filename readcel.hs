{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Applicative ((<$>))
import           Data.Maybe          (fromJust)
import qualified Data.Text                 as DT
import qualified Data.Text.Encoding        as DTE
import qualified Data.ByteString.Lazy      as BSL
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Word
import           Data.Char
import           Data.Int

import           Text.PrettyPrint.HughesPJClass

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

parseCelByte   = fromIntegral <$> getWord8
parseCelShort  = fromIntegral <$> getWord16be
parseCelInt    = fromIntegral <$> getWord32be
parseCelUByte  = getWord8
parseCelUShort = getWord16be
parseCelUInt   = getWord32be
parseCelFloat  = getFloat32be

celIndentWidth = 2

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
  return $ DT.takeWhile (/='\0') $ DTE.decodeUtf16BE s
 
parseCelGUID     = parseCelTextFromString
parseCelDateTime = parseCelTextFromWString
parseCelLocale   = parseCelTextFromWString


parseNThings :: Get a -> Int -> Get [a]
parseNThings parseThing 0 = do return []
parseNThings parseThing n = do
  t  <- parseThing
  ts <- parseNThings parseThing $ fromIntegral n - 1
  return $ t:ts


skipTo p = do
  br <- bytesRead
  skip $ (fromIntegral p) - (fromIntegral br)

data CelHeader = CelHeader CelUByte -- magic
                           CelUByte -- version
                           CelInt   -- nDataGroups
                           CelUInt  -- fstGroupPos
                           deriving (Show)

instance Pretty CelHeader where
  pPrint (CelHeader m v ng pos) =
    vcat [ text "CelHeader:"
         , ni (text "magic :             " <> (integer $ fi m))
         , ni (text "version :           " <> (integer $ fi v))
         , ni (text "no of data groups : " <> (integer $ fi ng))
         , ni (text "first dg offset :   " <> (integer $ fi pos))
         ]
    where ni = nest celIndentWidth; fi = fromIntegral

parseCelHeader :: Get CelHeader
parseCelHeader = do
  magic   <- parseCelUByte
  version <- parseCelUByte
  nGroups <- parseCelInt
  pos     <- parseCelUInt
  return $ CelHeader magic version nGroups pos


data CelParameter = CelParameterPlainText CelText
                  | CelParameterFloat     Float
                  | CelParameterAscii     CelText
                  | CelParameterUInt8     Word8
                  | CelParameterUInt32    Word32
                  | CelParameterUInt16    Word16
                  | CelParameterInt8      Int8
                  | CelParameterInt16     Int16
                  | CelParameterInt32     Int32
                    deriving (Show, Eq)

instance Pretty CelParameter where
  pPrint (CelParameterPlainText t) = text $ DT.unpack t
  pPrint (CelParameterFloat     f) =        float f
  pPrint (CelParameterAscii     t) = text $ DT.unpack t
  pPrint (CelParameterUInt8     i) = text $ show i
  pPrint (CelParameterUInt16    i) = text $ show i
  pPrint (CelParameterUInt32    i) = text $ show i
  pPrint (CelParameterInt8      i) = text $ show i
  pPrint (CelParameterInt16     i) = text $ show i
  pPrint (CelParameterInt32     i) = text $ show i

data CelNamedParameter = CelNamedParameter CelText CelParameter
                           deriving (Show, Eq)

instance Pretty CelNamedParameter where
  pPrint (CelNamedParameter n p) =
    text (DT.unpack n) <> text ": " <> pPrint p


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
    _ -> error $ "undefined MIME type: " ++ show t
    where
      ctor   = CelNamedParameter
      filter = DT.takeWhile (/='\0')
      du8    = filter . DTE.decodeUtf8    . BSL.toStrict
      du16   = filter . DTE.decodeUtf16BE . BSL.toStrict
      -- emulate later versions of Data.Binary.Get
      getInt8        = fromIntegral <$> getWord8
      getInt16be     = fromIntegral <$> getWord16be
      getInt32be     = fromIntegral <$> getWord32be


parseCelNamedParameters = parseNThings parseCelNamedParameter

data CelDataHeader = CelDataHeader CelText             -- dataId
                                   CelGUID             -- guId
                                   CelDateTime         -- datetime
                                   CelLocale           -- locale
                                   CelInt              -- nPars
                                   [CelNamedParameter] -- pars
                                   CelInt              -- nparents
                                   [CelDataHeader]     -- parents
                                   deriving (Show)

instance Pretty CelDataHeader where
  pPrint (CelDataHeader id guid dt locale nPars pars np ps) = 
    vcat [ text "CelDataHeader:"
         , ni $ text "data id: "          <> (text $ up id)
         , ni $ text "guid: "             <> (text $ up guid)
         , ni $ text "data/time: "        <> (text $ up dt)
         , ni $ text "locale: "           <> (text $ up locale)
         , ni $ text "no of parameters: " <> (integer $ fi nPars)
         , ni $ text "parameters:"
         , ni $ vcat (map (ni . pPrint) pars)
         , ni $ text "no of parents: " <> (integer $ fi np)
         , ni $ vcat (map (ni . pPrint) ps)]
    where ni = nest celIndentWidth; fi = fromIntegral; up = DT.unpack

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

parseCelDataHeaders = parseNThings parseCelDataHeader

data CelDataGroup = CelDataGroup CelUInt      -- posNextDataGroup
                                 CelUInt      -- posFirstDataSet
                                 CelInt       -- no of data sets
                                 CelText      -- name
                                 [CelDataSet] -- data sets
                                 deriving (Show)

instance Pretty CelDataGroup where
  pPrint (CelDataGroup np pf nos name dsets) = 
    vcat [ text "CelDataGroup:"
         , ni $ text "name: "                        <> (text $ up name)
         , ni $ text "position of next data group: " <> (int $ fi np)
         , ni $ text "position of first data set: "  <> (int $ fi pf)
         , ni $ text "no of data sets: "             <> (int $ fi nos)
         , ni $ vcat (map (ni . pPrint) dsets)]
    where ni = nest celIndentWidth; fi = fromIntegral; up = DT.unpack

parseCelDataGroup = do
  np    <- parseCelUInt
  fp    <- parseCelUInt
  nSets <- parseCelInt
  name  <- parseCelTextFromWString
  ds    <- parseCelDataSets $ fromIntegral nSets
  return $ CelDataGroup np fp nSets name ds

parseCelDataGroups 0 = return $ []
parseCelDataGroups n = do
  g@(CelDataGroup pos _ _ _ _) <- parseCelDataGroup
  skipTo pos
  gs <- parseCelDataGroups $ n - 1
  return $ g:gs




data CelDataValueType = CelValueTypeByte
                      | CelValueTypeUByte
                      | CelValueTypeShort
                      | CelValueTypeUShort
                      | CelValueTypeInt
                      | CelValueTypeUInt
                      | CelValueTypeFloat
                      | CelValueTypeString
                      | CelValueTypeWString
                        deriving (Show, Enum)

instance Pretty CelDataValueType where
  pPrint CelValueTypeByte    = text "CelValueTypeByte"
  pPrint CelValueTypeUByte   = text "CelValueTypeUByte"
  pPrint CelValueTypeShort   = text "CelValueTypeShort"
  pPrint CelValueTypeUShort  = text "CelValueTypeUShort"
  pPrint CelValueTypeInt     = text "CelValueTypeInt"
  pPrint CelValueTypeUInt    = text "CelValueTypeUInt"
  pPrint CelValueTypeFloat   = text "CelValueTypeFloat"
  pPrint CelValueTypeString  = text "CelValueTypeString"
  pPrint CelValueTypeWString = text "CelValueTypeWString"

parseCelValueType = toEnum . fromIntegral <$> parseCelByte

data CelColumnDescription = CelColumnDescription CelText -- column name
                                   CelDataValueType      -- value type
                                   CelInt                -- type size
                                   deriving (Show)
instance Pretty CelColumnDescription where
  pPrint (CelColumnDescription name tipe size) = 
    (text "Column name") <+>
    (text $ show $ DT.unpack name) <+>
    (text "of type") <+> (pPrint tipe) <+>
    (text "of size") <+> (int $ fromIntegral size)

parseCelColumnDescription = do
  n <- parseCelTextFromWString
  t <- parseCelValueType
  s <- parseCelInt
  return $ CelColumnDescription n t s

parseCelColumnDescriptions = parseNThings parseCelColumnDescription

data CelDataSet = CelDataSet CelUInt                -- fPosFirstEle
                             CelUInt                -- fPosNextDataSet
                             CelText                -- name
                             CelInt                 -- nPars
                             [CelNamedParameter]    -- pars
                             CelUInt                -- nCols
                             [CelColumnDescription] -- colNames
                             CelUInt                -- nRows
                             CelDataRows            -- rows
                             deriving (Show)

instance Pretty CelDataSet where
  pPrint (CelDataSet fpf fpn name npars pars ncols cds nrows rows) = 
    vcat [ text "CelDataGroup:"
         , ni $ text "data set name: "             <> (text $ up name)
         , ni $ text "position of first element: " <> (int $ fi fpf)
         , ni $ text "position of next data set: " <> (int $ fi fpn)
         , ni $ text "no of parameters: "          <> (int $ fi npars)
         , ni $ vcat $ map (ni . pPrint) pars
         , ni $ text "number of columns:"          <> (int $ fi ncols)
         , ni $ vcat $ map (ni . pPrint) cds
         , ni $ text "number of rows: "            <> (int $ fi nrows)
         , ni $ pPrint rows
         ]
    where ni = nest celIndentWidth; fi = fromIntegral; up = DT.unpack

parseCelDataSet = do
  fp1   <- parseCelUInt
  fpn   <- parseCelUInt
  name  <- parseCelTextFromWString
  nPar  <- parseCelInt
  pars  <- parseCelNamedParameters $ fromIntegral nPar
  nCol  <- parseCelUInt
  cols  <- parseCelColumnDescriptions $ fromIntegral nCol
  nRows <- parseCelUInt 
  dta   <- parseCelDataRows cols $ fromIntegral nRows
  return $
    CelDataSet fp1 fpn name nPar pars nCol cols nRows $ CelDataRows dta

parseCelDataSets 0 = return $ []
parseCelDataSets n = do
  d@(CelDataSet _ pos _ _ _ _ _ _ _) <- parseCelDataSet
  skipTo pos
  ds <- parseCelDataSets $ n - 1
  return $ d:ds


data CelDataValue = CelDataByte   CelByte
                  | CelDataUByte  CelUByte
                  | CelDataShort  CelShort
                  | CelDataUShort CelUShort
                  | CelDataInt    CelInt
                  | CelDataUInt   CelUInt
                  | CelDataFloat  CelFloat
                  | CelDataText   CelText
                    deriving (Eq, Show)

instance Pretty CelDataValue where
  pPrint (CelDataByte   x) = int $ fromIntegral x
  pPrint (CelDataUByte  x) = int $ fromIntegral x
  pPrint (CelDataShort  x) = int $ fromIntegral x
  pPrint (CelDataUShort x) = int $ fromIntegral x
  pPrint (CelDataInt    x) = int $ fromIntegral x
  pPrint (CelDataUInt   x) = int $ fromIntegral x
  pPrint (CelDataFloat  x) = float x
  pPrint (CelDataText   x) = text $ DT.unpack x

type CelDataRow = [CelDataValue]

data CelDataRows = CelDataRows [CelDataRow] deriving (Show)

instance Pretty CelDataRows where
  -- pPrint (CelDataRows rows) = vcat $ map pPrint rows
  pPrint (CelDataRows rows) = text "data rows follow..."

parseCelDataValue t = case t of
  CelValueTypeByte    -> CelDataByte   <$> parseCelByte
  CelValueTypeUByte   -> CelDataUByte  <$> parseCelUByte
  CelValueTypeShort   -> CelDataShort  <$> parseCelShort
  CelValueTypeUShort  -> CelDataUShort <$> parseCelUShort
  CelValueTypeInt     -> CelDataInt    <$> parseCelInt
  CelValueTypeUInt    -> CelDataUInt   <$> parseCelUInt
  CelValueTypeFloat   -> CelDataFloat  <$> parseCelFloat
  CelValueTypeString  -> CelDataText   <$> parseCelTextFromString
  CelValueTypeWString -> CelDataText   <$> parseCelTextFromWString

parseCelDataValues []     = do return []
parseCelDataValues (d@(CelColumnDescription _ t _):ds) = do
  v  <- parseCelDataValue  t
  vs <- parseCelDataValues ds
  return $ v:vs

parseCelDataRow  = parseCelDataValues
parseCelDataRows ds = parseNThings (parseCelDataRow ds)


data CelFile = CelFile CelHeader
                       CelDataHeader
                       [CelDataGroup]
                       deriving (Show)

instance Pretty CelFile where
  pPrint (CelFile h dh gs) =
    vcat [ text "CelFile:"
         , ni $ pPrint h
         , ni $ pPrint dh
         , ni $ text "data groups:"
         , ni $ vcat (map (ni . pPrint) gs)
         ] where ni = nest celIndentWidth

parseCelFile :: Get CelFile
parseCelFile = do
  h@(CelHeader _ _ n pos)  <- parseCelHeader
  dh <- parseCelDataHeader
  gs <- parseCelDataGroups n
  skipTo pos
  return $ CelFile h dh gs


processCel cel = res
  where res = runGet parseCelFile cel

main = do
  f <- BSL.readFile "array.cel"
  let c@(CelFile h d g) = processCel f in
    do
    return $ pPrint c

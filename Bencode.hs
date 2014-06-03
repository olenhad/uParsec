module Bencode where
import qualified Data.Map.Lazy as Map
import Parser

data BencodeVal = BString String | BInt Int | BList [BencodeVal] | BDict [(BencodeVal,BencodeVal)]
                  deriving (Show)

parseString :: Parser BencodeVal
parseString = do n <- number
                 char ':'
                 str <- manyN item n
                 return $ BString str

parseInt :: Parser BencodeVal
parseInt = do char 'i'
              n <- number
              char 'e'
              return $ BInt n

parseList :: Parser BencodeVal
parseList = do char 'l'
               l <- many parseBencode
               char 'e'
               return $ BList l

parseDict :: Parser BencodeVal
parseDict = do char 'd'
               d <- many $ do { k <- parseString; v <- parseBencode; return (k,v)}
               char 'e'
               return $ BDict d


parseBencode :: Parser BencodeVal
parseBencode = parseString `option` parseInt `option` parseList

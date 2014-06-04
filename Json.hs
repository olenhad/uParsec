module Json where
import Parser

data JSONVal = JBool Bool |
               JString String |
               JNumber Int |
               JNull |
               JArray [JSONVal] |
               JPair (String, JSONVal) |
               JObject [(String, JSONVal)]
               deriving (Show)


parseBool :: Parser JSONVal
parseBool = do { symb "true"; return $ JBool True} `option`
            do { symb "false"; return $ JBool False }

parseString :: Parser JSONVal
parseString = do { s <- quotedString; return $ JString s}

parseNumber :: Parser JSONVal
parseNumber = do { n <- number; return $ JNumber n}

parseNull :: Parser JSONVal
parseNull = do { symb "null"; return JNull}

parseArray :: Parser JSONVal
parseArray = do { symb "["; l <- parseJson `sepBy` (symb ","); symb "]"; return $ JArray l}

parsePair :: Parser JSONVal
parsePair = do { k <- quotedString; symb ":"; v <- parseJson; return $ JPair (k,v)}

parseObject :: Parser JSONVal
parseObject = do { symb "{"; ps <- many parsePair ; symb "}";
                   return $ JObject $ map (\ (JPair p) -> p) ps}

parseJson :: Parser JSONVal
parseJson = parseBool `option` parseString `option` parseNumber `option`
            parseNull `option` parseArray `option` parseObject

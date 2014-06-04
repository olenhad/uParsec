module Parser where
import qualified Data.Char as C
import  Utils

data Parser a = Parser (String -> [(a, String)])

parse (Parser p) = p

instance Monad Parser where
   return a = Parser (\s -> [(a,s)])
   p >>= f = Parser (\s -> parse p s |>
                         map (\ (a, s') -> parse (f a) s') |>
                         concat)
-- Monad Laws
{-

return a >>= f = f a
p >>= return = p
p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g
-}

item :: Parser Char
item = Parser (\s -> case s of
                     "" -> []
                     (c:cs) -> [(c,cs)])


class Monad m => MonadPlus m where
      mzero :: m a
      mplus :: m a -> m a -> m a


-- mplus for the Parser is like an choice operator.
instance MonadPlus Parser where
         mzero = Parser (\cs -> [])
         mplus p q = Parser (\s -> (parse p) s ++ (parse q) s)

option :: Parser a -> Parser a -> Parser a
option  p q = Parser (\s -> case parse (mplus p q) s of
                                [] -> []
                                (x:xs) -> [x])

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = item >>= \c ->
              if p c then return c else mzero

char :: Char -> Parser Char
char c = satisfies (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do { char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p `option` return []

many1 :: Parser a -> Parser [a]
many1 p = do { a <- p; as <- many p; return (a:as)}

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) `option` return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) `option` return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where rest a = (do f <- op
                                    b <- p
                                    rest (f a b))
                                `option` return a

oneOf :: [Parser a] -> Parser a
oneOf l = foldl1 option l

manyN :: Parser a -> Int -> Parser [a]
manyN p 1 = do {c <- p; return [c]}
manyN p n = do {c <- p; rest <- manyN p (n-1); return (c:rest)}


manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = manyTill1 p end `option` return []

manyTill1 :: Parser a -> Parser b -> Parser [a]
manyTill1 p end = do a <- p
                     b <- lookAhead end
                     if b then return [a] else
                        do as <- manyTill p end
                           return (a:as)


lookAhead :: Parser a -> Parser Bool
lookAhead p = Parser (\s ->
                         case parse p s of
                         [] -> [(False, s)]
                         l -> [(True, s)])

quotedString :: Parser String
quotedString = do { char '"'; s <- manyTill item (char '"'); char '"'; return s}
-- Lexical combinators

space :: Parser String
space = many (satisfies isSpace)
       where isSpace ' ' = True
             isSpace '\n' = True
             isSpace '\t' = True
             isSpace _ = False

token :: Parser a -> Parser a
token p = do { a <- p; space ; return a}

symb :: String -> Parser String
symb s = token (string s)

digit :: Parser Char
digit = satisfies C.isDigit

number :: Parser Int
number = do cs <- many1 digit
            return $ read cs

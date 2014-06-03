module Epl(expr) where
import Parser

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = number `option` do { symb "("; n <- expr; symb ")"; return n}

addop = do {symb "+"; return (+)} `option` do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} `option` do {symb "/"; return (div)}

run :: String -> Int
run s = case parse expr s of
             [(num, _)] -> num

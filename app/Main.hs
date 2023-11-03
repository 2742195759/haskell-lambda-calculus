module Main (main) where

import Lib
import Text.Parsec
import Control.Monad (forever)

-- Calculus Part

data LTerm = LVar Char
           | LApp LTerm LTerm
           | LAbs String LTerm
           deriving (Show, Eq)

beta_reduce :: LTerm -> LTerm
beta_reduce (LApp (LAbs vars t) s) = if length vars == 1 then body else (LAbs (tail vars) body)
    where body = substitute (head vars) s t 
beta_reduce (LApp x y) = LApp (beta_reduce x) (beta_reduce y)
beta_reduce (LAbs x t) = LAbs x (beta_reduce t)
beta_reduce x = x

beta_reduce_many :: LTerm -> LTerm
beta_reduce_many x = if term_equal x (beta_reduce x) then x else beta_reduce_many (beta_reduce x)
        where term_equal left right = (toString right) == (toString left)

substitute :: Char -> LTerm -> LTerm -> LTerm
substitute x s (LVar m) = if x == m then s else (LVar m)
substitute x s (LApp t1 t2) = LApp (substitute x s t1) (substitute x s t2)
substitute x s (LAbs vars t) = if x `elem` vars then LAbs vars t else LAbs vars (substitute x s t)

toString :: LTerm -> String
toString (LVar m) = [m]
toString (LApp t1 t2) = "(" ++ (toString t1) ++ (toString t2) ++ ")"
toString (LAbs vars t) = "λ" ++ vars ++ "." ++ (toString t)

-- Parser Part

root_parser :: Parsec String () LTerm 
root_parser = do
    t <- term_parser
    eof
    return t

term_parser :: Parsec String () LTerm
term_parser = do
    (try application_parser) <|>
        try abstract_parser <|> 
        bracket_term <|>
        try variable_parser

reduction_term :: Parsec String () LTerm
reduction_term = do
    try abstract_parser <|> 
        try bracket_term <|>
        try variable_parser

bracket_term :: Parsec String () LTerm
bracket_term = do
    char '('
    x <- term_parser
    char ')'
    return x

abstract_parser :: Parsec String () LTerm
abstract_parser = do
    string "λ"
    xs <- many1 letter
    string "."
    t <- term_parser
    return (LAbs xs t)

variable_parser :: Parsec String () LTerm
variable_parser = do
    x <- letter
    return (LVar x)

application_parser :: Parsec String () LTerm
application_parser = do
    chainl1 reduction_term (notFollowedBy (char ')')  >> return LApp)

-- Main Entry

main :: IO ()
main = forever $ do 
    putStrLn "输入lambda表达式>>>"
    input <- getLine 
    let either_result = parse root_parser "" input
    let (Right result) = either_result
    putStrLn $ toString result
    putStrLn $ toString $ beta_reduce_many result

experiment :: IO ()
experiment = do 
    putStrLn $ toString $ beta_reduce_many $ LApp (LApp add one) one
    putStrLn $ toString $ beta_reduce_many $ LApp (LApp add two) two

getTermUnsafe :: String -> LTerm
getTermUnsafe input = case parse root_parser "" input of
    Right result -> result
    Left err -> error $ show err

zero = getTermUnsafe "(λf.λz.z)"
one  = getTermUnsafe "(λf.λz.fz)"
two  = getTermUnsafe "(λf.λz.f(fz))"
add  = getTermUnsafe "(λm.λn.λf.λz.mf(nfz))"

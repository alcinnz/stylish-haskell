{-# LANGUAGE OverloadedStrings #-}
module Data.CSS.Preprocessor.Conditions.Expr(
        Expr, Op(..), parse, eval, Datum(..)
    ) where

import Data.CSS.Syntax.Tokens(Token(..))
import Data.Text.Internal (Text(..))
import Data.Text (stripPrefix)

type Expr = [Op]
data Op = And | Or | Not | Var Text | Tok Token | MkRatio | Func Text [Token]
    | Less | LessEq | Equal | Greater | GreaterEq deriving Eq

parse :: Token -> [Token] -> (Expr, [Token])
parse end toks = let (toks', rest) = break (== end) toks in (parse' toks' [], rest)

--------
---- Shunting Yard parser
--------
parse' :: [Token] -> [(Op, Int)] -> Expr
parse' (Whitespace:toks) ops = parse' toks ops

parse' (Comma:toks) ops = pushOp toks Or 10 ops
parse' (Ident "not":toks) ops = pushOp toks Not 20 ops
parse' (Function "not":toks) ops = pushOp toks Not 0 ops
parse' (Ident "only":toks) ops = parse' toks ops
parse' (Ident "and":toks) ops = pushOp toks And 30 ops
parse' (Ident "or":toks) ops = pushOp toks Or 30 ops
parse' (Delim '<':Delim '=':toks) ops = pushOp toks LessEq 40 ops
parse' (Delim '<':toks) ops = pushOp toks LessEq 40 ops
parse' (Delim '>':Delim '=':toks) ops = pushOp toks GreaterEq 40 ops
parse' (Delim '>':toks) ops = pushOp toks Greater 40 ops
parse' (Colon:tok:toks) ops = Tok tok : pushOp toks Equal 40 ops
parse' (Delim '/':toks) ops = pushOp toks MkRatio 50 ops

parse' (LeftParen:toks) ops = pushOp toks (Var ")") 0 ops
parse' (RightParen:toks) ((Var ")", 0):ops) = parse' toks ops
parse' (RightParen:toks) ((Not, 0):ops) = Not : parse' toks ops -- Functional not syntax
parse' toks@(RightParen:_) ((op, _):ops) = op : parse' toks ops
parse' (RightParen:_) [] = [] -- Invalid!
parse' (Ident var:toks) ops@((peek, _):ops')
    -- First, fix up various range syntaxes.
    | peek `elem` [Less, LessEq, Greater, GreaterEq] = -- Chained conditions
        Var var : peek : Var var : parse' toks ops'
    | Just var' <- stripPrefix "max-" var = Var var' : pushOp toks LessEq 1000 ops
    | Just var' <- stripPrefix "min-" var = Var var' : pushOp toks GreaterEq 1000 ops
    | otherwise = Var var : parse' toks ops
parse' (tok:toks) ops = Tok tok : parse' toks ops
parse' [] ops = [op | (op, _) <- ops]

pushOp :: [Token] -> Op -> Int -> [(Op, Int)] -> Expr
pushOp toks op b ((peek, b'):ops') | b' >= b = peek : pushOp toks op b ops'
pushOp toks op b ops = parse' toks ((op, b):ops)

--------
---- Shunting Yard Evaluator
--------
data Datum = B Bool | N Float | Ratio Float Float deriving Eq

eval :: (Text -> Datum) -> (Token -> Datum) -> Expr -> Bool
eval = eval' []

eval' :: [Datum] -> (Text -> Datum) -> (Token -> Datum) -> Expr -> Bool
eval' (B y:B x:stack) v t (And:ops) = eval' (B (x && y):stack) v t ops
eval' (B y:B x:stack) v t (Or:ops) = eval' (B (x || y):stack) v t ops
eval' (B x:stack) v t (Not:ops) = eval' (B (not x):stack) v t ops
eval' stack v t (Var name:ops) = eval' (v name:stack) v t ops
eval' stack v t (Tok tok:ops) = eval' (t tok:stack) v t ops
-- TODO: How should I handle ratios?
eval' (N y:N x:stack) v t (MkRatio:ops) = eval' (Ratio x y:stack) v t ops
eval' (N y:N x:stack) v t (Less:ops) = eval' (B (x < y):stack) v t ops
eval' (N y:N x:stack) v t (LessEq:ops) = eval' (B (x <= y):stack) v t ops
eval' (y:x:stack) v t (Equal:ops) = eval' (B (x == y):stack) v t ops
eval' (N y:N x:stack) v t (Greater:ops) = eval' (B (x > y):stack) v t ops
eval' (N y:N x:stack) v t (GreaterEq:ops) = eval' (B (x >= y):stack) v t ops
eval' (B ret:_) _ _ [] = ret
eval' [] _ _ [] = True -- Special case
eval' _ _ _ _ = False -- Error handling fallback.

module Parser (parse) where

import Helpers
import Tree
import Data.List
import Data.Char
import Control.Applicative ((<|>), (<*>), (<$>), pure)
import Control.Monad (guard)

type ExprTree = Tree String

data TreeData = Num
              | Un
              | Bin

treeType :: ExprTree -> TreeData
treeType (Node _ Empty Empty) = Num
treeType (Node _ (Node _ _ _) (Node _ _ _)) = Bin
treeType _ = Un

tokens :: [Char]
tokens = " +-*/()"

applyToken :: Char -> [String] -> [String]
applyToken t = concat . map (breakAt t)

applyTokens :: [([String] -> [String])]
applyTokens = map applyToken tokens

breakUp :: String -> [String]
breakUp cs = filter (not . blank) $ foldr (id) [cs] applyTokens
    where
      blank x
           | null (dropSpaces x) = True
           | otherwise = False

op :: a -> Tree a -> Tree a -> Tree a
op = branch

unarOp :: a -> Tree a -> Tree a
unarOp x y = branch x y Empty

binSymbols :: [String]
binSymbols = [ "+"
             , "-"
             , "*"
             , "/"
             ]

unSymbols :: [String]
unSymbols = [ "sin"
            , "cos"
            , "tan"
            , "cot"
            ]

parseBinOp :: String -> [String] -> Maybe ExprTree
parseBinOp o e = do
  oper <- o `elemIndex` e
  let (x,y) = splitAt oper e
  xTree <- parse' x
  yTree <- parse' (drop 1 y) -- dropping the op
  return $ op o xTree yTree

binParsers :: [([String] -> Maybe ExprTree)]
binParsers = map parseBinOp binSymbols

parseUnOp :: String -> [String] -> Maybe ExprTree
parseUnOp o e = do
  oper <- o `elemIndex` e
  argStart <- nextElem oper e
  let (x, y) = splitAt argStart e
  arg <- parse' y
  return $ unarOp o arg

unarParsers :: [([String] -> Maybe ExprTree)]
unarParsers = map parseUnOp unSymbols

isParen :: [String] -> Bool
isParen e
    | ["("] `isPrefixOf` e = True
    | otherwise = False

parseParen :: [String] -> Maybe ExprTree
parseParen e = do
  guard $ isParen e
  closing <- ")" `elemIndex` e
  let (x,y) = splitAt closing e
  xTree <- parse' $ filter (not . null) $ drop 1 x
  return $ xTree

parseTerm :: [String] -> Maybe ExprTree
parseTerm e = case length e of
               1 -> return $ leaf $ head e
               _ -> Nothing

dispatch :: [([String] -> Maybe ExprTree)]
dispatch = concat
           [ binParsers
           , [parseParen]
           , unarParsers
           , [parseTerm]
           ]

parse' :: [String] -> Maybe ExprTree
parse' e = foldr (<|>) Nothing (dispatch <*> pure e)

isVarTerm :: String -> Bool
isVarTerm t = t `notElem` ([")", "("] ++ binSymbols ++ unSymbols) && (var t)
    where
      var t = let (nums, vars) = span isNumber t
              in case length vars of
                   0 -> False
                   _ -> True

-- supports only ONE variable
parseVariable :: String -> ExprTree
parseVariable s 
    | isVarTerm s = let (nums, vars) = span isNumber s
                    in if null nums 
                       then op "*" (leaf "1") (leaf vars)
                       else op "*" (leaf nums) (leaf vars)
    | otherwise = (leaf s)

parseVariables :: ExprTree -> ExprTree
parseVariables Empty = Empty
parseVariables t@(Node x t1 t2)  = case treeType t of
                                     Num -> parseVariable x
                                     _ -> op x 
                                          (parseVariables t1)
                                          (parseVariables t2)

parse :: String -> Maybe ExprTree
parse e = do
  tree <- parse' . breakUp $ e
  return $ parseVariables tree

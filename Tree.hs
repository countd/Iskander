module Tree (
             Tree(..)
            , leaf
            , branch
            , value
            )where

import Data.Functor
import Control.Applicative

data Tree a = Empty | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

branch :: a -> Tree a -> Tree a -> Tree a
branch = Node 

value :: Tree a -> a
value (Node x _ _) = x

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x t1 t2) = Node (f x) (fmap f t1) (fmap f t2)

-- Other ways to do this?
-- Check laws!
instance Applicative Tree where
    pure x = Node x Empty Empty
    Empty <*> _ = Empty
    (Node f t1 t2) <*> (Node x t3 t4) = (Node (f x) (t1 <*> t3) (t2 <*> t4))

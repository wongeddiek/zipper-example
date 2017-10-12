module Main where

import Data.Function ((&))
-- a & f = f a

import Data.Tree (Tree(..), Forest)
import Data.Tree.Pretty (drawVerticalTree)
{-
data Tree a
  = Node { rootLabel :: a
         , subForest :: Forest a
         }
\
data Forest a = [Tree a]
\
drawVerticalTree :: Tree String -> String
-}

poly :: Tree String
poly =
  Node "P"
  [ Node "O"
    [ Node "L"
      [ Node "N" []
      , Node "T" []
      ]
    , Node "Y"
      [ Node "S" []
      , Node "A" []
      ]
    ]
  , Node "L"
    [ Node "W"
      [ Node "C" []
      , Node "H" []
      ]
    , Node "A"
      [ Node "I" []
      , Node "P" []
      ]
    ]
  ]

draw :: Tree String -> IO ()
draw = putStrLn . drawVerticalTree

-- let's update the tree!

-- with pattern matching

changePatMatch :: Tree String -> Tree String
changePatMatch (Node p [o, Node l [Node _ ch, a]]) =
  Node p [o, Node l [Node "!!" ch, a]]
changePatMatch a = a

-- hard to re-use
-- fixed definition of change

-- what about a list of instructions
--   on how to traverse the tree?

data Direction = L | R
  deriving Show

directedUpdate :: [Direction] -> Tree String -> Tree String

directedUpdate (L:ds) (Node a (l:rs)) = Node a (directedUpdate ds l : rs)
directedUpdate (L:_ ) (Node a []    ) = Node a []

directedUpdate (R:ds) (Node a (l:r:_)) = Node a (l : [directedUpdate ds r])
directedUpdate (R:_ ) (Node a fs     ) = Node a fs

directedUpdate [] (Node _ fs) = Node "!!" fs

-- more flexible than before but...

-- inefficient for repeated updates
-- always have to start at the root
-- still neat :)

-- what if we recorded how we got to the target node?
-- we could use the info to work efficiently in/around the sub-tree

goLeft  :: (Tree String, [Direction]) -> (Tree String, [Direction])
goRight :: (Tree String, [Direction]) -> (Tree String, [Direction])

goLeft (Node _ (l:_), ds) = (l, L : ds)
goLeft (n           , ds) = (n, ds)

goRight (Node _ (_:r:_), ds) = (r, R : ds)
goRight (n             , ds) = (n, ds)

-- ok so we have a trail
-- nice to be able to navigate

-- can't go up though - only remembers down :'(

-- s/storing directions/Breadcrumbs|Trail/

-- remember: data Forest a = [Tree a]
data Crumb
  = WentLeft String (Forest String)
  | WentRight String (Forest String)
  deriving Show

type Trail = [Crumb]

left :: (Tree String, Trail) -> (Tree String, Trail)
left (Node v (l:r), bs) = (l, WentLeft v r : bs)
left (n, bs) = (n, bs)

right :: (Tree String, Trail) -> (Tree String, Trail)
right (Node v (l:r:rs), bs) = (r, WentRight v (l:rs) : bs)
right (n, bs) = (n, bs)

up :: (Tree String, Trail) -> (Tree String, Trail)
up (t, WentLeft  v r : bs) = (Node v (t:r), bs)
up (t, WentRight v l : bs) = (Node v (l++[t]), bs)
up (t, []) = (t, [])

-- we can now move freely in/around the tree
-- *

type Zipper = (Tree String, Trail)

modify :: (String -> String) -> Zipper -> Zipper
modify f (Node v ns, bs) = (Node (f v) ns, bs)

-- neat!
-- *

-- how about adding a sub-tree?

attach :: Tree String -> Zipper -> Zipper
attach t' (Node v ts, bs) = (Node v (t':ts), bs)

main :: IO ()
main = draw poly

{- resources
\
- Functional Pearls: The Zipper
  - http://gallium.inria.fr/%7ehuet/PUBLIC/zip.pdf
\
- http://learnyouahaskell.com/zippers
\
- http://okmij.org/ftp/continuations/zipper.html
  - zipper as a data structure derivative
  - zipper from any traversable
  - zipper based file-system/OS
- https://michaeldadams.org/papers/scrap_your_zippers/
  - generic zipper for heterogeneous types
\ 
-}

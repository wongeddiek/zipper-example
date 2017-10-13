module List where

import Data.Function ((&))
-- a & f = f a

-- given the following list
numbers :: [Int]
numbers =
  [0, 0, 0, 0, 0]

-- replace Nth value

-- replace 1st element
replace1 :: [a] -> a -> [a]
replace1 (_:xs) a = a:xs
replace1 []     _ = []

-- replace 2nd element
replace2 :: [a] -> a -> [a]
replace2 (x:_:xs) a = x:a:xs
replace2 xs       _ = xs

-- replace Nth element...

replaceN :: Int -> a -> [a] -> [a]
replaceN n x xs  =
  take n xs           -- values before N
  ++ [x]              -- value at N
  ++ drop (n + 1) xs  -- values after N

{-
insertN :: Int -> a -> [a] -> [a]
insertN n x xs =
  take n xs
  ++ [x]
  ++ drop n xs

deleteN :: Int -> [a] -> [a]
deleteN n xs =
  take n xs
  ++ drop (n + 1) xs
-}

-- update the list
numbers' :: [Int]
numbers' =
  numbers
  & replaceN 4 30
  & replaceN 3 50
  & replaceN 2 90
  & replaceN 3 60
  & replaceN 4 20

-- performance for index-sequential updates?

-- let's see if we can do better!

{-
   what if we encoded the list like so:
   ( [0,0,0,0,0]  ,  [] )
     ^---------^     ^
          |          |
         list       breadcrumbs
-}

type Listy a
  = ([a], [a])
-- fst is list
-- snd is breadcrumbs which we populate
--   as we traverse into the list

forward :: Listy a -> Listy a
forward (x:xs, bs) = (xs, x:bs)
forward (xs  , bs) = (xs, bs  )

backward :: Listy a -> Listy a
backward (xs, b:bs) = (b:xs, bs)
backward (xs, bs  ) = (xs  , bs)

-- modify replaces the head of the breadcrumbs
modify :: a -> Listy a -> Listy a
modify a (xs, _:bs) = (xs, a:bs)
modify _ (xs, bs  ) = (xs, bs)

-- recontruct list
toList :: Listy a -> [a]
toList (xs, b:bs) = toList (b:xs, bs)
toList (xs, []) = xs

flNumbers :: Listy Int
flNumbers =
  ([0, 0, 0, 0, 0], [])

flNumbers' :: [Int]
flNumbers' =
  flNumbers
  & forward -- ([0,0,0,0], [0]      )
  & forward -- ([0,0,0]  , [0,0]    )
  & forward -- ([0,0]    , [0,0,0]  )
  & forward -- ([0]      , [0,0,0,0])
  & forward  & modify 30  -- ([]     , [30,0,0,0,0]  )
  & backward & modify 50  -- ([30]   , [50,0,0,0]    )
  & backward & modify 90  -- ([50,30], [90,0,0]      )
  & forward  & modify 60  -- ([30]   , [60,90,0,0]   )
  & forward  & modify 20  -- ([]     , [20,60,90,0,0])
  & toList

-- performance?

-- E. Wong:
-- How can we use Listy without explicitly calling forward, backward, and modify?
-- we can write a wrapper function that takes an index n, a value a, and a listy (at its default state - empty breadcrumb), forward the listy to the index n, and modify the value at index n

-- first, let's make a small change to the modify function so that it modify the head of the list, instead of the head of the breadcrumb.

-- this makes it more consistent with 0-based indiex.  If we want to modify index 0 of a list, we move forward 0 times and call modify.  If we want to modify index n of a list, we move forward n times and call modify.

-- ([0,0,0,0,0], []) - listy is at index 0
-- ([0,0,0,0], [0]) - listy is at index 1

modify2 :: a -> Listy a -> Listy a
modify2 a (_:xs, bs) = (a:xs, bs)
modify2 _ (xs, bs  ) = (xs, bs)

-- next, let's create a function that resets the listy back to its default index 0 state
resetListy :: Listy a -> Listy a
resetListy (xs, b:bs) = resetListy (b:xs, bs)
resetListy (xs, bs)   = (xs, bs)

-- now, let's create a function that takes modify the listy at index n
-- modListy n a ()



-- rewriting listy to add an index number after 2nd list
-- listy index type
type ListyIndex a = ([a],[a],Int)

toListyIndex :: Listy a -> ListyIndex a
toListyIndex (xs, []) = (xs, [], 0)
toListyIndex (xs, bs) = (xs, bs, length bs - 1)

forward' :: ListyIndex a -> ListyIndex a
forward' (x:xs, bs, n) = (xs, x:bs, n+1)
forward' (xs  , bs, n) = (xs,   bs, n )

backward' :: ListyIndex a -> ListyIndex a
backward' (xs, b:bs, n) = (b:xs, bs, n-1)
backward' (xs, bs  , n) = (xs  , bs, n)

-- modify replaces the head of the breadcrumbs
modify' :: a -> ListyIndex a -> ListyIndex a
modify' a (_:xs, bs, n) = (a:xs, bs, n)
modify' _ (xs, bs  , n) = (xs, bs, n)

-- recontruct list
toList' :: ListyIndex a -> [a]
toList' (xs, b:bs, n) = toList' (b:xs, bs, n-1)
toList' (xs, [], n) = xs

-- warpper function to modify nth index of Listy
modifyN :: Int -> a -> ListyIndex a -> ListyIndex a
modifyN n a (xs, bs, index)
  | n == index = modify' a (xs, bs, index)
  | n > index  = modifyN n a (forward' (xs, bs, index))
  | n < index  = modifyN n a (backward' (xs, bs, index))


-- given the following listyIndex
listyX = (replicate 1000 0, [], 0)

-- update the listyIndex
listyX' =
  listyX
  & modifyN 0 30
  & modifyN 994 50
  & modifyN 998 90
  & modifyN 991 60
  & modifyN 993 20
  & modifyN 996 80


-- copy-on-write semantics

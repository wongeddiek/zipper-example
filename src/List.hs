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

-- copy-on-write semantics

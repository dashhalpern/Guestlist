module Guestlist where

type Name = String
type Fun = Int


data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show)

data Guestlister = Guesty [Employee] Int
  deriving (Show)

data GuestList = Guests ([Employee] , Int)
  deriving (Show)

instance Monoid GuestList where
    mempty = Guests ([], 0)
    mappend q  (Guests ((b: bl), c)) = glCons b q
    mappend q (Guests ([], c)) = q

theanswer = answer testCompany2

answer :: Tree Employee -> GuestList
answer trees = (takeleft (treeFold (Guests ([], 0), Guests ([], 0)) nextLevel trees))

takeleft :: (GuestList,GuestList) -> GuestList
takeleft (a,b) = a

getfun:: GuestList -> Int
getfun (Guests (b,c)) = c

glCons :: Employee -> GuestList -> GuestList
glCons (Emp a d) (Guests (bl, c)) =  (Guests (((Emp a d) : bl),(c + d)))

moreFun :: GuestList -> GuestList -> GuestList
moreFun (Guests (a, b)) (Guests (c, d)) = if (b > d)
									then Guests (a, b)
									else Guests (c, d)

data Tree a = Node {
rootLabel :: a, -- label value
subForest :: [Tree a] -- zero or more child trees
}


treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold b f (Node a []) = f a [b]
treeFold b f (Node a n) = f a (map (treeFold b f) n)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel a b =   replace (addleft a (switch(sumswitch b)))

replace :: (GuestList, GuestList) -> (GuestList, GuestList)
replace (b, c) = if (getfun c > getfun b)
				 then (c,c)
				 else (b,c)

addleft :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
addleft a (b,c) = (glCons a b, c)

sumswitch:: [(GuestList, GuestList)] -> (GuestList, GuestList)
sumswitch [] = ( Guests ([], 0), Guests ([], 0) )
sumswitch ((a,b) : c) =  (add (a, b) (sumswitch c))

add :: (GuestList, GuestList) -> (GuestList, GuestList) -> (GuestList, GuestList)
add (a, b) (c, d) = (mappend a c,mappend b d)

switch :: (GuestList, GuestList) -> (GuestList, GuestList)
switch (a,b) = (b,a) 

--(treeFold (f b (rootLabel s)) f) (subForest s) = 1
-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

stan = Emp "Stan" 9
bob = Emp "Bob" 2
joe = Emp "Joe" 5

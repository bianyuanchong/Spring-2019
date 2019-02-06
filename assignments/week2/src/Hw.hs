module Hw where
import Prelude(Show, Eq, undefined)

-- HOMEWORK ONE     Due 2/5 by 11:59pm by upload to your repo
--    (with a couple of hours "grace period" before it is considered late)
--    Note: There is also an analytical part to the homework
--    which will be posted on the class web page, and due to be
--    uploaded to Gradescope with the same due date and time, and
--    with the same grace period.

-- Fill in the bodies of the undefined functions and data.
-- DO NOT CHANGE THE TYPE SIGNATURES!

-- Think about whether you need to write each function
-- recursively on the structure of the data, or can
-- define more simply in terms of previously-defined functions,
-- or use a helper function.
-- You may always add your own helper functions and helper data!

-- Remember: Constructors must be capitalized; variable
-- and function names must be in lower case.
-- Constructor constants are like 0-ary functions (no arguments).


-- Part A: Basic Boolean data and functions

-- Note on data declarations: "deriving Show" will allow
-- data values to be printed by interpreter.


data Bool = True | False     deriving Show

-- Define the following familiar functions on Bools.
-- You may need multiple cases for each one.

not :: Bool -> Bool
not True = False
not False = True


and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False


or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True


xor :: Bool -> Bool -> Bool
xor True False  = True
xor False True = True
xor _ _ = False


-- Part B: Encoding of natural numbers using data expressions
-- and defining basic functions on these expressions.

data Nat =  Zero | Succ Nat deriving Show

-- the first 6 numbers
zero :: Nat
zero = Zero

one :: Nat
one = Succ zero

two :: Nat
two = Succ one

three  :: Nat
three = Succ two

four  :: Nat
four = Succ three

five  :: Nat
five = Succ four


-- Write the following functions
-- (Hint: try recursing on structure of first argument)

add ::  Nat -> Nat -> Nat
add x (Succ y)  = add (Succ x) y
add x Zero = x

dif :: Nat -> Nat -> Nat
dif (Succ x) (Succ y) = dif x y
dif Zero y = y
dif x Zero = x

mult ::  Nat -> Nat -> Nat
mult x Zero = Zero
mult Zero x = Zero
mult x (Succ y) = add x (mult x y)

exp ::  Nat -> Nat -> Nat
exp Zero x = Succ(Zero)
exp (Succ x) y = mult y (exp x y)

-- When are 2 Nats equal?
eq :: Nat -> Nat -> Bool
eq (Succ x) (Succ y)  = eq x y
eq Zero Zero = True
eq _ _  = False

-- When are 2 Nats not equal?
ne :: Nat -> Nat -> Bool
ne (Succ x) (Succ y)  = ne x y
ne Zero Zero = False
ne x Zero = True
ne Zero y = True

-- Less than on Nats
lt :: Nat -> Nat -> Bool
lt (Succ x) (Succ y) = lt x y
lt Zero Zero = False
lt z Zero = False
lt Zero y = True

-- Remaining Boolean tests

le :: Nat -> Nat -> Bool
le (Succ x) (Succ y) = le x y
le Zero Zero = True
le Zero y = True
le z Zero = False

gt :: Nat -> Nat -> Bool
gt x y = xor (le x y) True

ge :: Nat -> Nat -> Bool
ge x y = xor (lt x y) True

-- Example of useful test on Nats
-- return True on even Nats, False on odd.
isEven :: Nat -> Bool
isEven (Succ Zero) = False
isEven Zero = True
isEven (Succ(Succ x)) = isEven x


--Return the maximum of two Nats
max :: Nat -> Nat -> Nat
max (Succ x) (Succ y) = add (max x y) one
max x Zero = x
max Zero y = y

-- Part C:  Data Expressions: Now let's write our own data.

-- C.1: Dates

-- Write a data type for the 7 days of the week
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

-- what is your favorite Day? (Your choice!)
favoriteDay :: DayOfWeek
favoriteDay = Sunday

-- write a function that returns true if it is a weekend
isWeekend :: DayOfWeek -> Bool
isWeekend  Sunday = True
isWeekend Saturday = True
isWeekend _ = False

-- Write a function that gives the next day
nextDay :: DayOfWeek -> DayOfWeek
nextDay Monday  = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

-- write a data type for the Months of the year
data Month  = January | February | March | April | May | June | July | August | September | October | November | December deriving (Show, Eq)

-- In which month is your birthday?
partyMonth :: Month
partyMonth = March


-- Write a function that gives the next Month
nextMonth :: Month -> Month
nextMonth January = February
nextMonth February = March
nextMonth March = April
nextMonth April = May
nextMonth May = June
nextMonth June = July
nextMonth July = August
nextMonth August = September
nextMonth September = October
nextMonth October = November
nextMonth November = December
nextMonth December = January


-- C.2: Cartesian Coordinates

-- Write a data type for a 2D point where x and y are Nats
data Point = Point (Nat, Nat) deriving Show

-- Take 2 Nats and construct a Point
makePoint :: Nat -> Nat -> Point
makePoint x y = Point (x, y)

-- Select components from a Point
getX :: Point -> Nat
getX (Point (x, y)) = x

getY :: Point -> Nat
getY (Point (x, y)) = y


-- The Manhattan distance is the distance in the x direction plus the distance in the y direction
-- for instance the Manhattan distance of points (2,5) and (3,1) is 5
manhattanDistance :: Point -> Point -> Nat
manhattanDistance a b = add (dif (getX a) (getX b)) (dif (getY a) (getY b))


-- C.3: More Alterative data

-- Assume there is an boring math class where students only answer with a Bool OR with a Nat,
-- write a data type for that answer (hint: you may use two alternatives with | )
data ShortAnswer = BoolAnswer Bool | NatAnswer Nat deriving Show

-- Make a Nat answer
answerNat :: Nat -> ShortAnswer
answerNat x = NatAnswer x

-- Make a Bool answer
answerBool :: Bool -> ShortAnswer
answerBool True = BoolAnswer True
answerBool False = BoolAnswer False

-- What is 100 - 99?
ans1 :: ShortAnswer
ans1 = answerNat one

-- Is 100 - 99 an odd number?
ansTrue :: ShortAnswer
ansTrue = answerBool True

-- If the answers are equal return true otherwise return false
gradeAnswer :: ShortAnswer -> ShortAnswer -> Bool
gradeAnswer _ _  = undefined--or (eq x y) (and x y)

-- Part D: Important data structures: Lists

-- D.1: Lists of Nats

-- We can write lists for specific data, let's do Nats first
data ListNat = NilNat | ConsNat Nat ListNat deriving Show

-- Create a list of the first 4 nats
exampleListNat :: ListNat
exampleListNat = ConsNat Zero (ConsNat one (ConsNat two (ConsNat three NilNat)))

-- Find the length of a list (remember length is defined as the number of elements in the list)
lengthOfListNat :: ListNat -> Nat
lengthOfListNat NilNat = zero
lengthOfListNat (ConsNat x ls) = add (lengthOfListNat ls) one

-- Write a function that finds the sum of all the numbers in the list
sum :: ListNat -> Nat
sum NilNat = Zero
sum (ConsNat x ls) = add x (sum ls)

-- Write a function that tells when 2 Nat lists are equal
eqList :: ListNat -> ListNat -> Bool
eqList NilNat NilNat = True
eqList (ConsNat a la) (ConsNat b lb)  = and (eq a b) (eqList la lb)
eqList _ _ = False

-- Write a function that tests when a Nat is in a list
member :: Nat -> ListNat -> Bool
member a NilNat = False
member a (ConsNat x ls) = or (eq a x) (member a ls)


-- D.2:  Now let's do lists of Bools

data ListBool = NilBool | ConsBool Bool ListBool deriving Show

-- Give a list containing every bool
exampleListBool :: ListBool
exampleListBool = ConsBool True (ConsBool False NilBool)


lengthOfListBool :: ListBool -> Nat
lengthOfListBool NilBool  = Zero
lengthOfListBool (ConsBool a l) = add (lengthOfListBool l) one


-- D.3:  General lists: It gets very tiresome to write a list for every single datatype
-- so let's abstract out the type of elements using a polymorphic type

data List a = Nil | Cons a (List a)    deriving Show

-- Write a list of all the Bool values
listOfBool :: (List Bool)
listOfBool = Cons False (Cons True Nil)

-- Write a list of the first three Nats
listOfNat :: (List Nat)
listOfNat = Cons Zero (Cons one (Cons two Nil))

-- Write a list of all the weekdays
listOfWork :: (List DayOfWeek)
listOfWork = Cons Monday (Cons Tuesday (Cons Wednesday (Cons Thursday (Cons Friday (Cons Saturday (Cons Sunday Nil))))))

-- Useful function on lists
length :: (List a) -> Nat
length Nil  = Zero
length (Cons a b) = add (length b) one

-- Part E: Binary trees

-- A binary tree is either empty, or a node with a left subtree
-- a value at the root and a right subtree
data Tree a = Null | Node (Tree a) a (Tree a)     deriving Show

-- Give a balanced tree of three Bools corresponding to
--             True
--            /    \
--        False    False

exampleTree :: Tree Bool
exampleTree = Node (Node Null False Null) True (Node Null False Null)

-- return the number of elements in the tree
size :: (Tree a) -> Nat
size Null = Zero
size (Node a b c) = add (add (size a) (size c)) one 

-- Return the height (= number of nodes in longest path from root to leaf)

height :: (Tree a) -> Nat
height Null = Zero
height (Node a b c) = add (max (height a) (height c)) one

append :: (List a) -> (List a) -> (List a)
append Nil y = y
append (Cons a la) lb = Cons a (append la lb)

addNode :: (List a) -> a -> (List a)
addNode x y = append x (Cons y Nil)

-- Do an inorder traversal and store elements in a list
inorder :: (Tree a) -> (List a)
inorder Null = Nil
inorder (Node a b c) = append (addNode (inorder a) b) (inorder c)

-- Do a preorder traversal
preorder :: (Tree a) -> (List a)
preorder  Null  = Nil
preorder (Node a b c) = Cons b (append (preorder a) (preorder c))

-- extra ungraded questions below

-- What is the smallest datatype you can come up with?
data Smallests = Smallests Bool deriving Show
exampleSmallest :: Smallests
exampleSmallest = Smallests True

-- what is the craziest datatype you can come up with?
data Craziests = University Nat Bool Nat Nat Bool Nat
exampleCraziests :: Craziests
exampleCraziests = University two True one one True three

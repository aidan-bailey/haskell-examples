---------------------------------------
--------- BANNED KEYWORDS -------------
---------------------------------------
-- case class data default deriving  --
-- do  else  foreign  if  import  in --
-- infix infixl infixr instance  let --
-- module newtype of then type where --
---------------------------------------
-- NO TABS (could mess with compiler) -

main = do
  preludeExamples

----------------------
-- RANDOM FUNCTIONS --
----------------------

-- | Hello world function
helloWorld = putStrLn "Hello, World!" -- function definition

-- | add function adds two integers.
add :: (Integer, Integer) -> Integer -- function declaration
add (a, b) = a + b -- function definition

-- | calcSum function sums a list of numbers.
calcSum :: Num a => [a] -> a -- function declaration
calcSum a
  | null a = 0 -- if list is empty return 0
  | otherwise = head a + calcSum (tail a) -- recursively sum

-- | quicksort function sorts a list using quicksort
qsort [] = [] -- if list is empty, return empty list
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger -- for each x in xs pivot each side and combine result
  where
    smaller = [a | a <- xs, a <= x] -- pivot smaller
    larger = [b | b <- xs, b > x] -- pivot larger

-- | action sequencer function sequences actions and returns a list of outputs
seqn [] = return []
seqn (act : acts) = do
  -- for each action in the action list
  x <- act -- execute action and return result into list position
  xs <- seqn acts -- send the rest of the actions to be sequenced and executed
  return (x : xs) -- return the list of outputs

-- | && operator explicitly defined using pattern matching
(&&) :: Bool -> Bool -> Bool
{-
True && False = False
False && True = False
False && False = False
-}
_ && _ = False -- since anything other than True True will output False (wildcard underscores)

-- | testFront function checks if the begining element of a list is an 'a'
testFront :: [Char] -> Bool
testFront ('a' : _) = True -- wildcard underscore signifies we don't care about the other list values

-- | various different declarations for lists
listsRepresentations = do
  print ([1, 2, 3])
  print (1 : (2 : (3 : [])))
  print (1 : 2 : 3 : [])

-----------------------
-- PRELUDE FUNCTIONS --
-----------------------

-- | head function
chead :: [a] -> a
chead (x : _) = x

-- | function that acts as a description wrapper
printExample desc act = do
  putStr desc
  print act

-- | function that runs through various prelude library functions
preludeExamples = do
  let list = [1, 2, 3, 4, 5]
  print list
  printExample "Head: " (head list) -- selects first element of list : 1
  printExample "Tail: " (tail list) -- selects last elements of list : [2, 3, 4, 5]
  printExample "2nd Element: " (list !! 2) -- selects n'th element from list : 3
  printExample "First 3 Elements: " (take 3 list) -- selects first n elements from list : [1, 2, 3]
  printExample "Remove First 3 Elements: " (drop 3 list) -- removes first n elements from list : [4, 5]
  printExample "Length: " (length list) -- gets length of the list : 5
  printExample "Sum: " (sum list) -- calculates the sum of the list
  printExample "Product: " (product list) -- calculates product of list
  printExample "Append List to [6, 7, 8]: " (list ++ [6, 7, 8]) -- appends two lists
  printExample "Reverse: " (reverse list) -- reverses the list

------------------------
-- LAMBDA EXPRESSIONS --
------------------------

lambdaExamples = do
  print ((\x -> x ** 2) 2) -- Raises to power of 2
  print ((\x -> x `mod` 2 == 0) 2) -- Checks if number is even
  print ((\x y -> x + y) 2 3) -- Add two numbers together
  -- or
  print ((\x -> (\y -> x + y) 3) 2) -- Add two numbers together (alternative)

-- | function that returns first n odd integers
odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0 .. n -1] -- map applies a function to all elements of a list

---------------
-- OPERATORS --
---------------

-- TODO

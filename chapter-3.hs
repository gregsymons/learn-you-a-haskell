-- Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Incomplete pattern matches lead to runtime errors:

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- Pattern matching on tuples:

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Don't care values in tuple pattern matches:

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

-- Pattern matching in list comprehensions:

pairWiseSum :: (Num a) => [(a, a)] -> [a]
pairWiseSum xs = [ a + b | (a, b) <- xs]

-- Pattern matching lists:

head' :: [a] -> a
head' [] = error "Can't call head on an empty list!"
head' (h:_) = h

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:z:[]) = "The list has three elements: " ++ show x ++ " and " ++ show y ++ " and " ++ show z
tell (x:y:_) = "This list is long. It's first two elements are " ++ show x ++ " and " ++ show y

-- OR --

tell' :: (Show a) => [a] -> String
tell' [] = "The list is empty"
tell' [x] = "The list has one element: " ++ show x
tell' [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell' [x, y, z] = "The list has three elements: " ++ show x ++ " and " ++ show y ++ " and " ++ show z
tell' (x:y:_) = "This list is long. It's first two elements are " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- as patterns:

capital :: String -> String
capital "" = "Whoops, empty string"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
        | bmi       <= 18.5 = "You're underweight, you emo, you!"
        | bmi       <= 25.0 = "You're supposedly normal. Pfffft, I bet you're ugly!"
        | bmi       <= 30.0 = "You're fat! Lose some weight, fatty!"
        | otherwise = "You're a whale. Congratulations."

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height^2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height^2 <= 25.0 = "You're supposedly normal. Pfffft, I bet you're ugly!"
    | weight / height^2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale. Congratulations."

max' :: (Ord a) => a -> a -> a
max' a b
    | a < b     = b
    | otherwise = a

-- note the infix definition. Does not change how you call it though. YOu still
-- need the backticks.
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a < b = LT
    | a == b = EQ
    | otherwise = GT

-- Where bindings let you give names to things.

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale. Congratulations."
    where bmi = weight / height^2

-- You can have multiple where bindings:
bmiTell3 :: (RealFloat a) => a -> a -> String
bmiTell3 weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pfffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale. Congratulations."
    where bmi    = weight / height^2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

-- where bindings can pattern match:
bmiTell4 :: (RealFloat a) => a -> a -> String
bmiTell4 weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pfffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale. Congratulations."
    where bmi                   = weight / height^2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

-- where bindings can define new functions:

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height^2

-- Let bindings

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

-- let bindings are expressions:

theUltimateAnswer = 4 * (let a = 9 in a + 1) + 2

-- let bindings can introduce new local functions:

someSquares = [let square x = x * x in (square 5, square 3, square 2)]

-- multiple inlined lets should be separated by semicolons:

inlinedLets = (let a = 100; b = 200; c = 300 in a*b*c, let foo = "Hey "; bar = "there!" in foo ++ bar)

-- let bindings can pattern match:

letPatterns = (let (a, b, c) = (1, 2, 3) in a+b+c) * 100

-- let bindings can be used in list comprehensions:

calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [ bmi | (w, h) <- xs, let bmi = w / h^2 ]

-- let bindings in list comprehensions are not filters; they only bind names.
-- But you can still add predicate filters:

calcBmis3 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis3 xs = [ bmi | (w, h) <- xs, let bmi = w / h^2, bmi >= 25.0]

-- Case expressions
-- Pattern matching in function definitions is syntactic sugar for more general
-- case expressions:

head2 :: [a] -> a
head2 [] = error "Can't call head on an empty list!"
head2 (h:_) = h

headWithCase :: [a] -> a
headWithCase xs = case xs of 
                    []    -> error "Can't call head on an empty list!"
                    (x:_) -> x

-- As expressions, case expressions can appear anywhere any other expression
-- can:

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of 
                                        []  -> "empty"
                                        [_] -> "a singleton list"
                                        _   -> "a longer list"

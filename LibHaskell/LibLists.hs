
module LibHaskell.LibLists(
  strt
 ,pos
 ,posList
 ,delete
 ,delFirst
 ,insert
 ,occurences
 ,inFst
 ,look
 ,filterBreak
 ,after
 ,coltil
 ,churn
 ,sew
 ,append
 ,appendList
 ,spay
 ,dropAfter
 ,between
 ,removeBetween
 ,removeAllBetween
 ,bombard
 ,rm
 ,rmAll
 ,remBetwix
 ,remAllBetwix
 ,mostly
 ,byRight
 ,pop
 ,grab
 ,lst
 ,kill
 ,oneMore
 ,splitOn
 ,lastx
 ,flatten
 ,afterList
 ,refPos
 ,cond
 ,notCond
 ,contains
 ,removeLeading
 ,replaceAll
 ,replaceAllOf
 ,unit
 ,accuracy
 ,excedes
 ,ses
 ,before
 ,nav
 ,compress
 ,intersperse
) where

-- For general lists not biased to a type.
strt :: (Eq a) => [a] -> Int -> [a]
strt [] _ = []
strt x 0 = x
strt (x:xs) t
	| t == 1 = xs
	| otherwise = strt xs (t - 1)

-- The position of an element in a list.
pos :: (Eq a) => [a] -> a -> Int
pos [] _ = -1
pos (x:xs) t
	| notElem t (x:xs) = -1
	| t == x = 0
	| otherwise = 1 + pos xs t

--The position of a list in a larger list.
posList :: (Eq a) => [a] -> [a] -> Int
posList [] _ = -1
posList a@(x:xs) y
	| (take (length y) a) == y = 0
	| otherwise = 1 + posList xs y

--Delete a sub-list.
delete :: (Eq a) => [a] -> [a] -> [a]
delete [] _ = []
delete a@(x:xs) y
	| y == take (length y) a = delete (strt a (length y)) y
	| otherwise = x : delete xs y

--Remove a sub-list from a list.
delList :: (Eq a) => [a] -> [[a]] -> [a]
delList a [] = a
delList xs (y:ys) = delList ( delete xs y) ys

--Delete the first occurrance.
delFirst :: (Eq a) => [a] -> [a] -> [a]
delFirst a@(x:xs) ys
	| take (length ys) a == ys = strt xs $ (length ys) - 1
	| otherwise = x : delFirst xs ys

-- Indices start at 0
insert :: (Eq a) => [a] -> Int -> [a] -> [a]
insert xs wh ys = take (wh) xs ++ ys ++ strt xs wh

--How many times is a in the list?
occurences :: (Eq a) => [a] -> a -> Int
occurences xs y = sum $ map (\a -> if a == y then 1 else 0) xs

--Is an element in the first of a list of tupples?
inFst :: (Eq a) => a -> [(a,b)] -> Bool
inFst a xs = a `elem` (map fst xs)

--Associative Tupple Lookup
look :: (Eq a) => a -> [(a,b)] -> b
look t xs = snd $ grab (filter (\(a,b) -> a == t) xs) 

--Apply a predicate until its false. The parameter that trips it does not get added.
filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak fun (x:xs)
  | not (fun x) = []
  | otherwise = x : filterBreak fun xs

--Everything after (not including) an element
after :: (Eq a) => [a] -> a -> [a]
after x y = strt x $ 1 + (pos x y)

--Collect until you meet an element.
coltil :: (Eq a) => [a] -> a -> [a]
coltil x y = take (pos x y) x

--Continuously make elements of a list.
churn :: a -> Int -> [a]
churn x n
  | (n == 1) = x:[]
  | otherwise = x:churn x (n - 1)
  
--Zip re-implementation, because I have Not-Invented-Here syndrome
sew :: [a] -> [b] -> [(a,b)]
sew [] _ = []
sew _ [] = []
sew (x:xs) (y:ys) = (x,y) : sew xs ys

--Add one more element to a list.
append :: [a] -> a -> [a]
append [] b = b:[]
append (x:xs) b = x : append xs b

--Add something to the end of a list.
appendList :: [a] -> [a] -> [a]
appendList [] b = b++[]
appendList (x:xs) b = x : appendList xs b

-- Drop the last element from a list.
spay :: [a] -> [a]
spay [] = []
spay a@(x:xs)
  | (length a) == 2 = x:[]
  | otherwise = x : spay xs

--Collect up until a certain point.
dropAfter :: (Eq a) => [a] -> a -> [a]
dropAfter [] _ = []
dropAfter (x:xs) b
  | (x == b) = x:[]
  | otherwise = x : dropAfter xs b

--Get the information between two delimiters.
between :: (Eq a) =>  [a] -> (a,a) -> [a]
between [] _ = []
between xs (a,b)
  | a `elem` xs = spay $ dropAfter (after xs a) b
  | otherwise = xs

--Get rid of everything between two delimiters, not including the delimiters.
removeBetween ::(Eq a) =>  [a] -> (a,a) -> [a]
removeBetween xs (a,b) = delete xs $ between xs (a,b)

--RemoveBetween that works on lists.
removeAllBetween :: (Eq a) => [a] -> (a,a) -> [a]
removeAllBetween xs (a,b)
  | and [(a `elem` xs),(b `elem` xs)] = removeAllBetween (removeBetween xs (a,b)) (a,b)
  | otherwise = xs

--Repeat a function on the result of the function
bombard :: [([a] -> [a])] -> [a] -> [a]
bombard [] ys = ys
bombard (x:xs) ys = x (bombard xs ys)

--Delete for single elements
rm :: (Eq a) => [a] -> a -> [a]
rm [] _ = []
rm (x:xs) b
  | (x == b) = rm xs b
  | otherwise = x : rm xs b

--Remove every occurrence from a list.
rmAll :: (Eq a) => [a] -> [a] -> [a]
rmAll a [] = a
rmAll xs (b:bs) = rmAll (rm xs b) bs

--The following function is like removeBetween, but it also removes (a,b)
remBetwix :: (Eq a) => [a] -> (a,a) -> [a]
remBetwix xs (a,b) =  rmAll (removeBetween xs (a,b)) [a,b] 

--Deletes every occurance
remAllBetwix :: (Eq a) => [a] -> (a,a) -> [a]
remAllBetwix xs (a,b)
  | and [(a `elem` xs),(b `elem` xs)] = remAllBetwix (remBetwix xs (a,b)) (a,b)
  | otherwise = xs

--Is the list more than 50% an element?
mostly :: (Eq a) => [a] -> a -> Bool
mostly xs n = ncur > (length xs) - ncur
  where ncur = occurences xs n

--Sort a list of tupples with the Int in the the right.
byRight :: [(a,Int)] -> [(a,Int)]
byRight [] = []
byRight ((x,y):xs) = 
 let lesserSorted =  byRight [(a,b) | (a,b) <- xs, b <= y]
     greaterSorted = byRight [(a,b) | (a,b) <- xs, b > y]
 in lesserSorted ++ [(x,y)] ++ greaterSorted

--Pop the head off of a list.
pop :: [a] -> [a]
pop (_:xs) = xs

--Grab the first element of a list.
grab :: [a] -> a
grab [] = error "Empty List supplied to Grab!"
grab [x] = x
grab (x:_) = x

--Take the last element of a list.
lst :: [a] -> a
lst [x] = x
lst (x:xs) = lst xs

 
--How far is (1,_) from (_,2) in [a]? Will return (pos (after x a) b)
distance :: (Eq a) => [a] -> (a,a) -> Int
distance x (a,b) = abs ((pos x a) - (pos x b))

--Where in the list are the elements?
posin :: (Eq a) => [a] -> a -> [Int]
posin [] _   = []
posin (x:xs) b 
  | b `notElem` (x:xs) = []
  | x == b = ((length (x:xs))) : posin xs b
  | otherwise = posin xs b

--Uses the data that posin provides.
positions :: (Eq a) => [a] -> a -> [Int]
positions [] _ = []
positions x b = map (\c -> (length x) - c) $ posin x b

--Remove the rest of the list
kill :: [a] -> Int -> [a]
kill [] _ = []
kill (x:xs) c
  | c <= 0 = x:[]
  | otherwise = x : kill xs (c - 1)

--Returns the elment after a list within a list.
oneMore ::(Eq a) => [a] -> [a] -> [a]
oneMore x y = kill (strt x (posList x y)) ((length y))

--Break a list on every occurrence of an element.
splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn xs c
	| c `elem` xs = (filterBreak (/= c) xs) : splitOn (after xs c) c
	| otherwise = xs:[]

--Get the last x elements from a list.
lastx :: [a] -> Int -> [a]
lastx []  _ = []
lastx a@(x:xs) b
  | (length a) <= b = x : lastx xs b
  | otherwise = lastx xs b

--Flatten a list of two-part tupples into one list.
flatten :: [(a,a)] -> [a]
flatten [] = []
flatten ((a,b):xs) = a : b : flatten xs

--What comes after the list?
afterList :: (Eq a)  => [a] -> [a] -> [a]
afterList a@(x:xs) y
  | (take (length y) a) == y = (strt xs ((length y) - 1))
  | otherwise = afterList xs y

--Get the element out of the second list from where it occurs in the first.
refPos :: (Eq a) => [a] -> [b] -> a -> b
refPos a b c = b !! (pos a c)

-- Perform a function based on a predicate.
cond :: (a -> Bool) -> (a -> a) -> a -> a
cond f1 f2 c = if (f1 c) then (f2 c) else c

--Cond with not applied
notCond  :: (a -> Bool) -> (a -> a) -> a -> a
notCond f1 f2 c = if (not (f1 c)) then (f2 c) else c

--Is a list found within a larger list?
contains :: (Eq a) => [a] -> [a] -> Bool
contains [] _ = False
contains a@(x:xs) b
  | (take (length b) a) == b = True
  | otherwise = contains xs b

--Remove all elements from the beginning of a list.
removeLeading ::(Eq a) => [a] -> a -> [a]
removeLeading [] _ = []
removeLeading a@(x:xs) b
  | x == b = removeLeading xs b
  | otherwise = a 

--Replace all occurences of a match.
replaceAll :: (Eq a) => [a] -> (a,a) -> [a]
replaceAll [] _ = []
replaceAll (x:xs) (a,b)
  | (x == a) = b : replaceAll xs (a,b)
  | otherwise = x : replaceAll xs (a,b)

--Replace that uses lists of tupples.
replaceAllOf :: (Eq a) => [a] -> [(a,a)] -> [a]
replaceAllOf x [] = x
replaceAllOf x ((a,b):ys) = replaceAll (replaceAllOf x ys) (a,b)

-- Run "Unit Tests"
unit :: (Eq a) => [a] -> [([a] -> Bool)] -> Int -> Bool
unit a b x = excedes (map (\c -> (c a)) b) True x

accuracy :: [Bool] -> Int -> Bool
accuracy x y = excedes x True y

excedes :: (Eq a) => [a] -> a -> Int -> Bool
excedes a x c = (occurences a x) >= c

ses ::(Eq a) => [a] -> [a] -> Bool
ses a b = and [((grab a) == (grab b)),((pop a)==(pop b))]

before :: (Eq a) => [a] -> a -> [a]
before x y = filterBreak (/=y) x

nav :: (a -> Bool) -> [a] -> [Int]
nav f x = positions (map f x) True

goUntil :: (Eq a) => [a] -> (Int,a) -> [a]
goUntil x (a,b) = before (strt x a) b

compress :: [[a]] -> [a]
compress [] = []
compress (x:xs) = x ++ compress xs

intersperse :: [a] -> a -> [a]
intersperse [] _ = []
intersperse (x:xs) y = x:y: intersperse xs y
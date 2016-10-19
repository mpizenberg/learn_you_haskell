double x = x + x

doubleSmallNumber x =
    if x > 100
    then x
    else x * 2

length' xs = sum [1 | _ <- xs]

lucky :: (Integral a ) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN !"
lucky x = "Sorry, you're out of luck, pal !"

lucky' :: (Integral a) => a -> String
lucky' x =
    case x of
        7 -> "LUCKY NUMBER SEVEN !"
        _ -> "Sorry, you're out of luck, pal !"


bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2


initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let
        smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in
        smallerSorted ++ [x] ++ biggerSorted


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)


mult :: (Ord a, Integral a) => a -> a -> a -> a
mult limit a b
    | a < limit && b < limit = a * b
    | otherwise = 0


foldl' :: (b -> a -> b) -> b -> [a] -> b
-- seq is a primitive system function that when applied to x and y
-- will first reduce x then return y.
-- The idea is that y references x so that when y is reduced x
-- will not be a big unreduced chain anymore.
foldl' f z []     = z
foldl' f z (x:xs) =
    let z' = f z x
    in seq z' $ foldl' f z' xs


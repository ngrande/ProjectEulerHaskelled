-- Problem 1
isMatching :: Int -> Bool
isMatching x 
	| x `rem` 3 == 0 = True
	| x `rem` 5 == 0 = True
	| otherwise = False

multiples :: Int
multiples = sum $ takeWhile (<1000) $ filter isMatching [1..]

-- Problem 2
generateFibos :: [Int] -> [Int]
generateFibos xs
	| (head xs) > 4000000 = xs
	| otherwise = generateFibos (((head xs) + (head (tail xs))) : xs)

fibonaccied :: Int
fibonaccied = sum $ filter (even) $ generateFibos [2,1]

-- Problem 4
isPalindrome :: Int -> Bool
isPalindrome x 
	| (length $ show x) < 2 = False
	| x == (read $ reverse $ show x :: Int) = True
	| otherwise = False

palindromes :: Int -> Int
palindromes x = last $ filter (isPalindrome)  $ foldr (\y acc -> acc ++ [(x * y)]) [11] [100..999]

largestPalindrome :: Int
largestPalindrome = maximum $ map (\x -> palindromes x) [100..999]

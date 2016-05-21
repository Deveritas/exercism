module Palindromes where


largestPalindrome :: (Integral a) => a -> a -> (a, [(a, a)])
largestPalindrome = palindrome max

smallestPalindrome :: (Integral a) => a -> a -> (a, [(a, a)])
smallestPalindrome = palindrome min

palindrome :: (Integral a) => (a -> a -> a) -> a -> a -> (a, [(a, a)])
palindrome comp start end = (1, [(1, 1)])

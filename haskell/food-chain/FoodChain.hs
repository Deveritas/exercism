module FoodChain (song) where

song :: String
song = unlines $ map (unlines . verse) [1..8]

verse :: Int -> [String]
verse 8 = [start 8]
verse i = start i : reverse (take i sheSwallowed)

sheSwallowed :: [String]
sheSwallowed = [
    "I don't know why she swallowed the fly. Perhaps she'll die."
  , "She swallowed the spider to catch the fly."
  , "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
  , "She swallowed the cat to catch the bird."
  , "She swallowed the dog to catch the cat."
  , "She swallowed the goat to catch the dog."
  , "She swallowed the cow to catch the goat."
  ]

iKnow :: String
iKnow = "I know an old lady who swallowed a "

start :: Int -> String
start 1 = iKnow ++ "fly."
start 2 = iKnow ++ "spider.\nIt wriggled and jiggled and tickled inside her."
start 3 = iKnow ++ "bird.\nHow absurd to swallow a bird!"
start 4 = iKnow ++ "cat.\nImagine that, to swallow a cat!"
start 5 = iKnow ++ "dog.\nWhat a hog, to swallow a dog!"
start 6 = iKnow ++ "goat.\nJust opened her throat and swallowed a goat!"
start 7 = iKnow ++ "cow.\nI don't know how she swallowed a cow!"
start 8 = iKnow ++ "horse.\nShe's dead, of course!"
start _ = error "past end of song"
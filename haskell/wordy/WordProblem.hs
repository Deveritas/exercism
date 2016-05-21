module WordProblem (answer) where

import           Text.Parsec.Char       (digit, letter, spaces, string)
import           Text.Parsec.Combinator (many1, option, optional)
import           Text.Parsec.Prim       (parse)
import           Text.Parsec.String     (Parser)

import           Control.Monad          (void)


data Command a = Command String a deriving (Show)
data Question a = Question a [Command a] deriving (Show)

type Lexer = Parser String


-- Main

answer :: String -> Maybe Integer
answer = either (const Nothing) run . parse question "main"


-- Parsers

question :: (Integral a, Read a) => Parser (Question a)
question = Question <$> (whatis *> integer) <*> many1 command

command :: (Integral a, Read a) => Parser (Command a)
command = Command <$> commandL <*> integer

integer :: (Integral a, Read a) => Parser a
integer = read <$> integerL

whatis :: Parser ()
whatis = void whatisL


-- Lexers

whatisL :: Lexer
whatisL = string "What is "

commandL :: Lexer
commandL = many1 letter <* spaces <* optional (string "by ")

integerL :: Lexer
integerL = (++) <$> option "" (string "-") <*> many1 digit <* spaces


-- Interpreters

run :: (Integral a) => Question a -> Maybe a
run (Question start commands) = foldl step (Just start) commands

step :: (Integral a) => Maybe a -> Command a -> Maybe a
step acc (Command "plus"       val) = flip (+) val <$> acc
step acc (Command "minus"      val) = flip (-) val <$> acc
step acc (Command "multiplied" val) = flip (*) val <$> acc
step acc (Command "divided"    val) = flip div val <$> acc
step _   (Command _            _  ) = Nothing

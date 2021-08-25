module WordGuesser where

import Data.Char ()
import System.IO (hFlush, stdin, stdout)
import System.Random (getStdRandom, randomR)

-- Constants
numOfGuesses :: Int
numOfGuesses = 10

------------

main :: IO ()
main = do
  wordList <- readFile "enable1.txt"

  word <- wordToGuess $ lines wordList

  -- String of letters and dashes -> dashes represent currently unknown letters
  let known = replicate (length word) '_'
  putStrLn known

  guessWord known word numOfGuesses

-- Returns a word from a list of words using the random function
wordToGuess :: [String] -> IO String
wordToGuess words = do
  num <- getStdRandom $ randomR (0, length words)
  return (words !! num)

-- Main game loop
guessWord :: String -> String -> Int -> IO ()
guessWord known word guesses
  | known == word = do
    putStrLn ("Congratulations! You guessed the word " ++ word)
    promptRestart
  | guesses == 0 = do
    putStrLn ("You lose! The correct word was " ++ word)
    promptRestart
  | otherwise = do
    putStr "Enter a letter: "
    hFlush stdout
    line <- getLine

    let letter = head line
    (newKnown, newGuesses) <- handle letter known word guesses

    putStrLn newKnown
    guessWord newKnown word newGuesses

handle :: Char -> String -> String -> Int -> IO (String, Int)
handle letter known word guesses
  | letter `elem` word = return (fillLetter letter known word, guesses)
  | otherwise =
    putStrLn ("Try again! You have " ++ show (guesses - 1) ++ " guesses left.")
      >> return (known, guesses - 1)

-- Fills all the empty spaces of the known word with the guessed letter
fillLetter :: Char -> String -> String -> String
fillLetter letter known word = zipWith (\lk lw -> if lw == letter then lw else lk) known word

-- Prompts the use to play again and validates the input
promptRestart :: IO ()
promptRestart = do
  putStr "Do you want to play again? (yes or no) "
  hFlush stdout
  answer <- getLine

  case answer of
    "yes" -> main
    "no" -> putStrLn "Thank you for playing :)"
    _ -> putStrLn "Input either yes or no!" >> promptRestart
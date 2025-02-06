-- Version 1: Unlimited attempts
-- Pure function
strToInt :: String -> Maybe Int
strToInt s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

checkGuess :: Int -> Int -> String
checkGuess number guess
    | guess /= number = "Incorrect"
    | otherwise = "Correct"

validateNumberInput :: String -> Either String Int
validateNumberInput input = case strToInt input of
    Just num -> Right num
    Nothing  -> Left "Invalid input. Please enter a number."

-- Impure function
guessNumber :: Int -> IO ()
guessNumber number = do
    putStr "Player2 Enter your guess: "
    input <- getLine
    case validateNumberInput input of
        Right guess -> do
            let result = checkGuess number guess
            putStrLn result
            if result == "Correct"
                then return ()
                else guessNumber number
        Left err -> do
            putStrLn err
            guessNumber number

version1 :: IO ()
version1 = do
    putStr "Player1 Enter your number: "
    input <- getLine
    case validateNumberInput input of
        Right number -> guessNumber number
        Left err -> do
            putStrLn err
            version1

-- Version 2: Limited attempts
limitguessNumber :: Int -> Int -> IO ()
limitguessNumber number attemptsLeft
    | attemptsLeft <= 0 = putStrLn "You've run out of attempts."
    | otherwise = do
        putStr "Player2 Enter your guess: "
        input <- getLine
        case validateNumberInput input of
            Right guess -> do
                let result = checkGuess number guess
                putStrLn result
                if result == "Correct"
                    then return ()
                    else limitguessNumber number (attemptsLeft - 1)
            Left err -> do
                putStrLn err
                limitguessNumber number attemptsLeft

version2 :: IO ()
version2 = do
    putStr "Player1 Enter your number: "
    input1 <- getLine
    putStr "Enter the number of attempts allowed: "
    input2 <- getLine
    case (validateNumberInput input1, validateNumberInput input2) of
        (Right number, Right attempts) -> limitguessNumber number attempts
        _ -> do
            putStrLn "Invalid input. Please enter numbers only."
            version2

-- can you reuse code from the previous version?
--      Yes, validateNumberInput checkGuess and strToInt are reused.
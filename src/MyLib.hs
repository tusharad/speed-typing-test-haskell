{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MyLib where

import           Graphics.Vty
import           Graphics.Vty.Platform.Unix (mkVty)
import           Control.Concurrent
import           Control.Monad.Trans.State
import qualified Data.Vector as V
import           System.Random
import           Control.Monad.IO.Class
import           Data.String.Interpolate
import           Data.Maybe
import           WordStore

type UserInput = String
type GivenString = String
type CurrentGivenString = String
type CurrentUserInput = String

truncate_ :: Double -> Int -> Double
truncate_ num places = fromIntegral (floor (num*t)) / t
  where
    t = 10^places

titleText :: String
titleText = [i|--------------------\nSpeed Typing Test :)\n--------------------|]

titleLogo :: Image
titleLogo = mconcat $ map (string (defAttr `withForeColor` blue)) (lines titleText)

timer :: Vty -> Double -> Double -> MVar () -> MVar Double -> IO ()
timer vty totalSec remainingSec timerEndVar correctWordsCountVar
    | remainingSec <= 0.3 = update vty timeUpImage >> putMVar timerEndVar ()
    | otherwise = do
        correctWordsCount <- readMVar correctWordsCountVar
        update vty (fullImage correctWordsCount)
        threadDelay 300000
        timer vty totalSec (remainingSec-0.30000) timerEndVar correctWordsCountVar
    where
      timeUpImage = picForImage $ titleLogo <-> string (defAttr `withBackColor` red) "Time's up!"
      timerPic = string (defAttr `withForeColor` green) $ "Time Left: " <> show (truncate_ remainingSec 2) <> " seconds"
      wpmPic correctWordsCount = pad 10 0 0 0 $ string (defAttr `withForeColor` green) $ "WPM: " <> show (truncate_ ((correctWordsCount / (totalSec - remainingSec))*60) 2)
      fullImage correctWordsCount = picForImage $ titleLogo <-> (timerPic <|> wpmPic correctWordsCount)

getInputFromUser_ :: Vty -> String -> String -> MVar () -> IO (Maybe String)
getInputFromUser_ vty stringToType userInputString timerEndVar = do
    update vty prettyPrintStrings
    try <- tryTakeMVar timerEndVar
    case try of
      Just _ -> pure Nothing
      _ -> do
        e <- nextEvent vty
        tryAgain <- tryTakeMVar timerEndVar
        case tryAgain of
          Just _ -> pure Nothing
          _ -> do
            case e of
              EvKey KEnter _    -> pure (Just userInputString)
              EvKey (KChar c) _ -> getInputFromUser_ vty stringToType (userInputString ++ [c]) timerEndVar
              EvKey KBS _       -> getInputFromUser_ vty stringToType 
                (if null userInputString then "" else init userInputString) timerEndVar
              EvKey KEsc _      -> pure Nothing
              _                 -> getInputFromUser_ vty stringToType userInputString timerEndVar
    where
      stringToTypePic = string (defAttr `withForeColor` green) $ "String to Type: " <> stringToType
      userTypedStringPic = string (defAttr `withForeColor` green) $ "Type: " <> userInputString
      prettyPrintStrings = picForImage $ translate 0 5 $ stringToTypePic <-> userTypedStringPic

getInputFromUser :: V.Vector String -> Vty -> MVar () -> MVar Double -> StateT [(UserInput,GivenString)] IO ()
getInputFromUser listOfWords vty timerEndVar correctWordsCountVar = do
    n <- liftIO (randomRIO (0,9) :: IO Int)
    let suggestedWord = listOfWords V.! n
    mUserInputString <- liftIO $ getInputFromUser_ vty suggestedWord "" timerEndVar
    case mUserInputString of
        Nothing -> pure ()
        Just userInputString -> do
            modify ((userInputString,suggestedWord):)
            oldCount <- liftIO $ takeMVar correctWordsCountVar
            _ <- liftIO $ putMVar correctWordsCountVar $ oldCount + if userInputString == suggestedWord then 1 else 0
            getInputFromUser listOfWords vty timerEndVar correctWordsCountVar

showScore :: Vty -> Double -> Double -> [(UserInput,GivenString)] -> IO ()
showScore vty correctWordCount totalTimeElapsed _ = do
  update vty (picForImage $ gameOverPic <-> wPMFormulaPic)
  threadDelay 3000000
  where
    gameOverPic = string (defAttr `withForeColor` red) $ "Game Over!" 
    wPMFormulaPic = string (defAttr `withForeColor` green) $ "WPM: " 
      <> show (truncate_ ((correctWordCount / totalTimeElapsed)*60) 2)

type Seconds = Int

timers :: [Int]
timers = [10,20,30,40,50,60,70]

getTimerConfig :: Vty -> Int -> IO (Int)
getTimerConfig introWindow currentIndex = do
  let timers_ = zip [0..] timers
  let timerLength = length timers
  
  let x = fmap numToSecondsPic timers_
  update introWindow $ picForImage (titleLogo <-> getSecondsImage <-> foldr1 (<->) x)
  e <- nextEvent introWindow
  case e of
    EvKey KEnter _ -> pure (timers !! currentIndex) 
    EvKey KDown _ -> getTimerConfig introWindow ((currentIndex+1) `mod` timerLength)
    EvKey KUp _   -> getTimerConfig introWindow ((currentIndex-1) `mod` timerLength)
    _             -> getTimerConfig introWindow (0 `mod` timerLength)
  where
    getSecondsImage = string (defAttr `withForeColor` yellow) "Please select timer seconds"
    numToSecondsPic (ind,x) = 
      if ind == currentIndex then 
        string (defAttr `withForeColor` green `withBackColor` magenta) 
          $ (show x) <> " seconds"
      else 
        string (defAttr `withForeColor` green) $ (show x) <> " seconds"

getDifficultyConfig :: Vty -> Int -> IO (V.Vector String)
getDifficultyConfig  introWindow currentIndex = do
  let levels = ["Easy","Medium","Hard"]
  let levels_ = zip [0..] levels
  let levelsLength = length levels_
  
  let x = fmap numToSecondsPic levels_
  update introWindow $ picForImage (titleLogo <-> getSecondsImage <-> foldr1 (<->) x)
  e <- nextEvent introWindow
  case e of
    EvKey KEnter _ -> do
      case (levels !! currentIndex) of
        "Easy"   -> pure listOfEasyWords
        "Medium" -> pure listOfMediumWords
        "Hard"   -> pure listOfHardWords
        _       -> pure listOfEasyWords

    EvKey KDown _ -> getDifficultyConfig introWindow ((currentIndex+1) `mod` levelsLength)
    EvKey KUp _   -> getDifficultyConfig introWindow ((currentIndex-1) `mod` levelsLength)
    _             -> getDifficultyConfig introWindow (0 `mod` levelsLength)
  where
    getSecondsImage = string (defAttr `withForeColor` yellow) "Please difficulty level:"
    numToSecondsPic (ind,x) = 
      if ind == currentIndex then 
        string (defAttr `withForeColor` green `withBackColor` magenta) x 
      else 
        string (defAttr `withForeColor` green) x

main :: IO ()
main = do
    introWindow <- mkVty defaultConfig
    timerSeconds <- fromIntegral <$> getTimerConfig introWindow 0
    shutdown introWindow

    introWindow <- mkVty defaultConfig
    wordList <- getDifficultyConfig introWindow 0
    shutdown introWindow

    vty <- mkVty defaultConfig
    vty0 <- mkVty defaultConfig
    
    shutdownInput $ inputIface vty0
    
    timerEndVar <- newEmptyMVar
    correctWordsCountVar <- newMVar 0
    _ <- forkIO $ timer vty0 timerSeconds timerSeconds timerEndVar correctWordsCountVar
    
    evalStateT (getInputFromUser wordList vty timerEndVar correctWordsCountVar) []
    
    mCorrectWordCount <- tryTakeMVar correctWordsCountVar
    let correctWordCount = fromMaybe 0 mCorrectWordCount
    _ <- showScore vty correctWordCount timerSeconds []
    
    shutdown vty
    shutdown vty0

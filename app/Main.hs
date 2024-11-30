{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.Time.Clock 
import Data.Time.Calendar.OrdinalDate (fromOrdinalDateValid)
import Brick 
--fromOrdinalDate :: Year -> DayOfYear -> Day -- both Integer and Int
--------------------------------------
--       DATA TYPES 
data Light = On 
           | Off 
           deriving (Eq,Show)
data StopLight a = Arrow a
                 | Go a
                 | Wait a 
                 | Stop a 
                 deriving (Eq, Show)

instance Functor StopLight where 
    fmap f light = 
        case light of  
            (Arrow l) -> Arrow $ f l 
            (Go l)    -> Go $ f l 
            (Wait l)  -> Wait $ f l
            (Stop l)  -> Stop $ f l


data Side = Side 
          { go :: StopLight Light
          , wait :: StopLight Light
          , arrow :: StopLight Light
          , stop :: StopLight Light
          } deriving (Eq, Show)
data Square = Square 
            {north :: Side 
            ,south :: Side 
            ,east  :: Side 
            ,west  :: Side 
            } deriving (Eq, Show)
data Direction = North 
               | West 
               | South 
               | East 
               deriving (Eq, Show)

-----------------------------------------------
--          HELPER FUNCTIONS 

isOn :: StopLight Light -> Bool 
isOn (Go b)    = if b == On then True else False 
isOn (Wait b)  = if b == On then True else False 
isOn (Stop b)  = if b == On then True else False 
isOn (Arrow b) = if b == On then True else False 

getSideSquare :: Direction -> Square -> Side 
getSideSquare dir sq = 
    case dir of 
        North -> north sq 
        South -> south sq 
        West  -> west sq 
        East  -> east sq 

turnOnLight :: StopLight Light -> StopLight Light 
turnOnLight light = 
     case isOn light of 
      True  -> light 
      False -> On <$ light 

turnOnLightSide :: StopLight Light -> Side -> Side 
turnOnLightSide light (Side {go = g, stop = s, arrow = a, wait = w}) = 
    case light of 
        (Go _)     -> Side {go = turnOnLight light, arrow = a, stop = s, wait = w}
        (Stop _ )  -> Side {stop = turnOnLight light, go = g, arrow = a, wait = w}
        (Arrow _ ) -> Side {arrow = turnOnLight light, stop = s, go = g, wait = w}
        (Wait _)   -> Side {wait = turnOnLight light, stop = s, arrow = a, go = g}
    
turnOnLightSquare :: StopLight Light -> Side -> Direction -> Square -> Square 
turnOnLightSquare light side direction (Square {north = n , south = s, west = w, east = e}) = 
        let litUpSide = (turnOnLightSide light side)
        in 
        case direction of 
         North -> Square {north = litUpSide, south = s, west = w, east = e}
         South -> Square {south = litUpSide,north = n,  west = w, east = e}
         West  -> Square {west = litUpSide, north = n,  east = e, south = s}
         East  -> Square {east = litUpSide, west = w , north = n , south = s}
        
turnOn :: NominalDiffTime -> UTCTime -> StopLight Light -> Direction -> Square -> IO ()
turnOn seconds currTime light direction square = do 
    newCurrentTime <- getCurrentTime 
    let direction = North 
        cond      = (addUTCTime seconds currTime) <= newCurrentTime
    newSquare      <- return $ turnOnLightSquare light (getSideSquare direction square) direction square 
    

--------------------------------------------
-- Variables but you know haskell doesn't have variables right? 
allOff = Side 
       { go  = Go Off 
       , wait= Go Off 
       , arrow = Arrow Off 
       , stop = Stop Off 
       }

offSquare = Square 
         {north = allOff
         ,south = allOff
         ,east  = allOff 
         ,west  = allOff 
         }
mySquare = offSquare
---------------------------------------------
main :: IO ()
main = do 
    currentTime <- getCurrentTime :: IO UTCTime 
    putStrLn $ show mySquare 
    void (turnOn (fromIntegral 15 :: NominalDiffTime) currentTime (Go On) South mySquare)
    putStrLn $ show  mySquare 
-- acording to AI 
-- use addUTCTime for adding duration to a time 
-- use diffUTCTime for finding time differences 
-- us toRational to convert to a numeric type 
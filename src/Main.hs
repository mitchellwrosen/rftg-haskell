module Main where

import Control.Monad.State

import Rftg.Engine
import Rftg.GameState
import Rftg.Player

defaultGameSettings =
   GameSettings { gIs2PlayerAdvanced = False
                , gIsGoalsEnabled = False
                , gIsTakeoversEnabled = False
                , gUsesTGS = False
                , gUsesRVI = False
                , gUsesBOW = False
                }

main = do
   let players = [createPlayer "tom",
                  createPlayer "lewis",
                  createPlayer "jerry"]
   gameState <- initGameState defaultGameSettings players
   execStateT beginGame gameState
   return ()

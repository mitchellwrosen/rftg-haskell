{-# LANGUAGE TemplateHaskell #-}

module Rftg.GameState where

import Rftg.Card
import Rftg.CardJSON
import Rftg.Goal
import Rftg.Phase
import Rftg.Player

import Control.Lens.Getter ((^.))
import Control.Lens.TH (makeLenses)
import System.Random.Shuffle (shuffleM)


data GameSettings =
   GameSettings { gIs2PlayerAdvanced  :: Bool
                , gIsGoalsEnabled     :: Bool
                , gIsTakeoversEnabled :: Bool
                , gUsesTGS            :: Bool
                , gUsesRVI            :: Bool
                , gUsesBOW            :: Bool
                }

data GameState =
   GameState { _gPlayers            :: [Player]
             , _gDeck               :: [Card]
             , _gDiscard            :: [Card]
             , _gFirstGoals         :: [Goal]
             , _gMostGoals          :: [Goal]
             , _gVPPool             :: Int
             , _gPhases             :: [Phase]  -- Phases selected this round
             , _gCurPhase           :: Phase
             , _gRoundNum           :: Int
             , _gGameOver           :: Bool
             , _gSettings           :: GameSettings
             }

makeLenses ''GameState

initGameState :: GameSettings -> [Player] -> IO GameState
initGameState settings players = do
   deck <- getDeck settings
   (first_goals, most_goals) <- getGoals settings
   -- TODO: use lenses
   return GameState { _gPlayers        = players
                    , _gDeck           = deck
                    , _gDiscard        = []
                    , _gFirstGoals     = first_goals
                    , _gMostGoals      = most_goals
                    , _gVPPool         = (length players) * 12
                    , _gPhases         = []
                    , _gCurPhase       = InitPhase
                    , _gRoundNum       = 0
                    , _gGameOver       = False
                    , _gSettings       = settings
                    }

getDeck :: GameSettings -> IO [Card]
getDeck settings =
   getBaseCards >>=
      addTGSCards settings >>=
         addRVICards settings >>=
            addBOWCards settings
   where addTGSCards settings' deck =
            if gUsesTGS settings'
            then do
               tgs <- getTGSCards
               return $ removeGamblingWorld deck ++ tgs
            else return deck

         addRVICards settings' deck =
            if gUsesRVI settings'
            then do
               rvi <- getRVICards
               return $ removeGamblingWorld deck ++ rvi
            else return deck

         addBOWCards settings' deck =
            if gUsesBOW settings'
            then do
               rvi <- getBOWCards
               return $ removeGamblingWorld deck ++ rvi -- FIXME: does this `remove` belong?
            else return deck

         removeGamblingWorld = filter ((/= "Gambling World") . (^. cName))

-- Gets (first, most) goals from the available pool.
getGoals :: GameSettings -> IO ([Goal], [Goal])
getGoals settings =
   if gIsGoalsEnabled settings
   then do
      let (firstGoals, mostGoals) = getGoals' settings
      firstGoals' <- shuffleM firstGoals
      mostGoals' <- shuffleM mostGoals
      return (take 4 firstGoals', take 2 mostGoals')
   else return ([], [])
   where getGoals' settings' =
            getTGSGoals settings' `combineTuple`
            getRVIGoals settings' `combineTuple`
            getBOWGoals settings'

         getTGSGoals settings' =
            if gUsesTGS settings'
            then ([BudgetSurplus, GalacticStandardOfLiving, GalacticStatus, InnovationLeader, OverlordDiscoveries, SystemDiversity],
                  [GreatestInfrastructure, GreatestMilitary, LargestIndustry, ProductionLeader])
            else ([], [])

         getRVIGoals settings' =
            if gUsesRVI settings'
            then ([ExpansionLeader, GalacticRiches, UpliftKnowledge],
                  [PropagandaEdge, ResearchLeader])
            else ([], [])

         getBOWGoals settings' =
            if gUsesBOW settings'
            then ([GalacticStanding, MilitaryInfluence, PeaceWarLeader],
                  [GalacticPrestige, ProsperityLeader])
            else ([], [])

         combineTuple (as, bs) (cs, ds) = (as ++ cs, bs ++ ds)

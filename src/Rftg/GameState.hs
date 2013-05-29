module Rftg.GameState where

import System.Random.Shuffle (shuffleM)

import Rftg.Card
import Rftg.Phase
import Rftg.Player

data GameState =
   GameState { gPlayers            :: [Player]
             , gDeck               :: [Card]
             , gFirstGoals         :: [Goal]
             , gMostGoals          :: [Goal]
             , gVPPool             :: Int
             , gPhases             :: [Phase]  -- Phases selected this round
             , gCurPhase           :: Phase
             , gRoundNum           :: Int
             , gGameOver           :: Bool
             , gSettings           :: GameSettings
             }

data GameSettings =
   GameSettings { gIs2PlayerAdvanced  :: Bool
                , gIsGoalsEnabled     :: Bool
                , gIsTakeoversEnabled :: Bool
                , gUsesTGS            :: Bool
                , gUsesRVI            :: Bool
                , gUsesBOW            :: Bool
                }

initGameState :: GameSettings -> Int -> IO GameState
initGameState settings num_players = do
   deck <- getDeck settings
   (first_goals, most_goals) <- getGoals settings
   return GameState { gPlayers        = undefined
                    , gDeck           = deck
                    , gFirstGoals     = first_goals
                    , gMostGoals      = most_goals
                    , gVPPool         = num_players * 12
                    , gPhases         = []
                    , gCurPhase       = InitPhase
                    , gRoundNum       = 0
                    , gGameOver       = False
                    , gSettings       = settings
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

         removeGamblingWorld = filter ((/= "Gambling World") . cName)

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

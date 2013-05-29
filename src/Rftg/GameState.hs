module Rftg.GameState where

import System.Random.Shuffle (shuffleM)

import Rftg.Card

type Deck = [Card]

data Player = Player -- TODO

data Goal = -- The Gathering Storm
            BudgetSurplus            -- First to discard 1+ cards at round end
          | GalacticStandardOfLiving -- First to have 5+ VP chips
          | GalacticStatus           -- First to play a 6-cost development
          | InnovationLeader         -- First to have 1+ power in each phase, including Trade
          | OverlordDiscoveries      -- First to have 3+ "Alien" cards
          | SystemDiversity          -- First to have 1+ production or windfall world of each good kind

          | GreatestInfrastructure -- 4+ developments
          | GreatestMilitary       -- 6+ military
          | LargestIndustry        -- 3+ novelty/rare elements
          | ProductionLeader       -- 4+ production worlds (military or non-military)

            -- Rebels vs Imperium
          | ExpansionLeader -- First to have 8+ (developments or worlds) in tableau
          | GalacticRiches  -- First to have 4+ goods at the end of a phase
          | UpliftKnowledge -- First to have 3+ "Uplift" cards

          | PropagandaEdge -- 3+ Rebel military worlds
          | ResearchLeader -- 3+ Explore powers

            -- The Brink of War
          | GalacticStanding  -- First to have 2+ prestige chips and 3+ VP chips
          | MilitaryInfluence -- First to have either 3+ Imperium cards or 4+ Rebel cards in tableau
          | PeaceWarLeader    -- First to have either <0 military and 2+ worlds in tableau or 1+ takeover power and 2+ military worlds

          | GalacticPrestige -- 3+ prestige chips
          | ProsperityLeader -- 3+ cards with Consume powers (not including Trade)

data Phase = InitPhase
           | ExplorePhase
           | DevelopPhase
           | SettlePhase
           | ConsumePhase
           | ProducePhase

data GameState =
   GameState { gPlayers            :: [Player]
             , gDeck               :: Deck
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

getDeck :: GameSettings -> IO Deck
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

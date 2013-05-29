module Rftg.Goal where

data GoalKind = First | Most

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

goalKind :: Goal -> GoalKind
goalKind BudgetSurplus            = First
goalKind GalacticStandardOfLiving = First
goalKind GalacticStatus           = First
goalKind InnovationLeader         = First
goalKind OverlordDiscoveries      = First
goalKind SystemDiversity          = First
goalKind ExpansionLeader          = First
goalKind GalacticRiches           = First
goalKind UpliftKnowledge          = First
goalKind GalacticStanding         = First
goalKind MilitaryInfluence        = First
goalKind PeaceWarLeader           = First
goalKind GreatestInfrastructure   = Most
goalKind GreatestMilitary         = Most
goalKind LargestIndustry          = Most
goalKind ProductionLeader         = Most
goalKind PropagandaEdge           = Most
goalKind ResearchLeader           = Most
goalKind GalacticPrestige         = Most
goalKind ProsperityLeader         = Most

goalVP :: Goal -> Int
goalVP g = case goalKind g of
              First -> 3
              Most  -> 5

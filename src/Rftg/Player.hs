module Rftg.Player where

import Rftg.Card
import Rftg.Phase
import Rftg.Tableau

data Player = 
   Player { pName            :: String
          , pPhase           :: (Phase, Maybe Phase) -- The phase(s) the player has chosen this round
          , pHand            :: [Card]
          , pTableau         :: Tableau
          , pVPs             :: Int
          , pPrestige        :: Int
          , pHasUsedPrestige :: Bool 
          }

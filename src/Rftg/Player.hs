{-# LANGUAGE TemplateHaskell #-}

module Rftg.Player where

import Rftg.Card
import Rftg.Phase
import Rftg.Tableau

import Control.Lens.TH (makeLenses)

{-data Task = tDiscardFromHand Int -- Discard n cards from hand-}

data Player = 
   Player { _pName            :: String
          , _pPhaseChoice     :: (Phase, Maybe Phase) -- second Phase is Nothing when not 2p advanced
          , _pHand            :: [Card]
          , _pExploringHand   :: [Card]               -- Cards being explored
          , _pTableau         :: Tableau
          , _pVPs             :: Int
          , _pPrestige        :: Int
          , _pHasUsedPrestige :: Bool 
          }

makeLenses ''Player

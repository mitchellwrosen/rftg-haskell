module Rftg.Engine where

import Rftg.Card
import Rftg.GameState
import Rftg.Player

import Control.Lens ((.=), (%=), hasn't, ix, use)
import Control.Monad (when)
import Control.Monad.State (get, StateT)
import System.Random.Shuffle (shuffleM)

type Rftg = StateT GameState IO
type PlayerIndex = Int

-- Errors if the player index is invalid.
checkValidPlayerIndex :: PlayerIndex -> Rftg ()
checkValidPlayerIndex index = do
   state <- get
   when (hasn't (gPlayers . ix index) state) $
      error $ "Invalid player index " ++ show index

-- Draw n card to player's hand.
drawToHand :: PlayerIndex -> Int -> Rftg ()
drawToHand index n = do
   checkValidPlayerIndex n
   newCards <- draw n
   (gPlayers . ix index . pHand) %= (++ newCards)

-- Draw n cards from the top of the deck and return them, possibly reshuffling
-- the discard pile into the deck.
draw :: Int -> Rftg [Card]
draw n = do
   maybeShuffleDiscardIntoDeck n
   deck <- use gDeck
   let (top, rest) = splitAt n deck
   gDeck .= rest
   return top

-- If there are more than n cards in the deck, shuffles the discard pile and
-- places it under the deck, so that a draw of n cards is possible. Throws an
-- error if n > length deck + length discard
maybeShuffleDiscardIntoDeck :: Int -> Rftg ()
maybeShuffleDiscardIntoDeck n = do
   deck <- use gDeck
   discard <- use gDiscard
   let total_decksize = length deck + length discard
   if n > total_decksize
   then error $ show n ++ " cards attempted to be drawn from deck + discard size " ++ show total_decksize
   else when (n > length deck) $ do
      shuffled_discard <- use gDiscard >>= shuffleM
      gDeck .= deck ++ shuffled_discard

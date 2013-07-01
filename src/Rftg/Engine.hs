{-# LANGUAGE FlexibleContexts #-}

module Rftg.Engine where

import Rftg.Card
import Rftg.GameState
import Rftg.Player
import Rftg.Tableau

import Control.Lens
import Control.Lens.At (ix)
import Control.Lens.Fold (hasn't, preuse)
import Control.Lens.Getter ((^.), use, uses)
import Control.Lens.Setter ((.=), (%=), over)
import Control.Lens.Traversal (both)
import Control.Monad (when)
import Control.Monad.State (get, put, StateT)
import Data.List (partition)
import System.Random.Shuffle (shuffleM)

type Rftg = StateT GameState IO

type CardIndex   = Int
type PlayerIndex = Int

numInitialCards = 6

playerIndices :: Rftg [Int]
playerIndices = do
   state <- get
   return [0..length (state^.gPlayers) - 1]


beginGame :: Rftg ()
beginGame = do
   playerIndices >>= mapM_ (drawToHand numInitialCards)
   {- TODO:
    - Send messages to players
    -    game has begun
    -    here are your cards
    - Wait for messages from players
    -    which cards have been discarded
    -}

-- checkValidPlayerIndex index errors if there is no player at |index|.
checkValidPlayerIndex :: PlayerIndex -> Rftg ()
checkValidPlayerIndex index = do
   state <- get
   when (hasn't (gPlayers . ix index) state) $
      error $ "Invalid player index " ++ show index

-- checkValidHandIndex p_index c_index errors if there is no player at
-- |p_index| or no card |c_index| in |p_index|'s hand.
checkValidHandIndex :: PlayerIndex -> CardIndex -> Rftg ()
checkValidHandIndex p_index c_index = do
   checkValidPlayerIndex p_index
   state <- get
   when (hasn't (gPlayers . ix p_index . pHand . ix c_index) state) $
      error $ "Invalid hand index " ++ show c_index ++ " of player " ++ show p_index

-- checkValidTableauIndex p_index c_index errors if there is no player at
-- |p_index| or no card |c_index| in |p_index|'s tableau.
checkValidTableauIndex :: PlayerIndex -> CardIndex -> Rftg ()
checkValidTableauIndex p_index c_index = do
   checkValidPlayerIndex p_index
   state <- get
   when (hasn't (gPlayers . ix p_index . pTableau . ix c_index) state) $
      error $ "Invalid tableau index " ++ show c_index ++ " of player " ++ show p_index

-- draw n draws the top |n| cards from the deck. Possibly reshuffles the discard
-- pile into the deck.
--
-- Returns the drawn cards.
draw :: Int -> Rftg [Card]
draw n = do
   maybeShuffleDiscardIntoDeck n
   top <- uses gDeck (take n)
   gDeck %= drop n
   return top

-- drawToHand index n draws |n| cards to player |index|'s hand.
drawToHand :: Int -> PlayerIndex -> Rftg ()
drawToHand n index = do
   checkValidPlayerIndex index
   newCards <- draw n
   (gPlayers . ix index . pHand) %= (++ newCards)

-- maybeShuffleDiscardIntoDeck n shuffles the discard pile and appends it to the
-- deck if there are less than |n| cards remaining in the deck, so that a draw
-- of |n| cards will then be possible.
--
-- Errors if n > length deck + length discard.
maybeShuffleDiscardIntoDeck :: Int -> Rftg ()
maybeShuffleDiscardIntoDeck n = do
   deck    <- use gDeck
   discard <- use gDiscard
   let total_decksize = length deck + length discard
   if n > total_decksize
      then error $ show n ++ " cards attempted to be drawn from deck + discard size " ++ show total_decksize
      else when (n > length deck) $ do
         shuffled_discard <- use gDiscard >>= shuffleM
         gDeck .= deck ++ shuffled_discard

-- discardCard card discards |card| to the discard pile.
discardCard :: Card -> Rftg ()
discardCard card = gDiscard %= (++ [card])

-- discardFromHand p_index c_indices discards the cards at |c_indices| from the
-- player at |p_index|'s hand.
--
-- Returns the discarded cards.
discardFromHand :: PlayerIndex -> [CardIndex] -> Rftg [Card]
discardFromHand p_index c_indices = do
   Just hand <- preuse (gPlayers . ix p_index . pHand)
   let indexed_hand = zip [0..] hand
   let (discarded, remaining) =
         over both (map snd) $
            partition (\(i, _) -> i `elem` c_indices) indexed_hand

   (gPlayers . ix p_index . pHand) .= remaining
   return discarded

-- spendGood card moves the good on |card| to the discard pile. Errors if there
-- is no good to spend.
--
-- Returns the spent good.
spendGood :: PlayerIndex -> CardIndex -> Rftg GoodKind
spendGood p_index c_index = do
   Just tableau_card <- preuse (gPlayers . ix p_index . pTableau . ix c_index)
   case tableau_card ^. tGood of
      Just good_kind -> do
         discardCard $ tableau_card ^. tCard
         return good_kind
      Nothing -> error $ "No good at player " ++ show p_index ++ ", tableau " ++ show c_index

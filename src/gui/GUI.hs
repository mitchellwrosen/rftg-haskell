module GUI (
            initializeGUI
           ,startGUI
           ,beginDiscard
           ,beginExplorePhase
           ,beginDevelopPhase
           ,beginSettlePhase
           ,beginProducePhase
           ,beginChooseConsumePower
           ,beginChooseGood
           ) where

import Text.Printf
import Data.IORef
import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

import System.FilePath
import System.Directory (getDirectoryContents)
import Control.Monad
import Control.Monad.State
import GameGUI
import Messages

type Deck = M.Map String Pixbuf

overBoth :: (a -> b) -> (a, a) -> (b, b)
overBoth f (a, b) = (f a, f b)

drawDiscardPoolText :: Int -> Int -> Int -> String
drawDiscardPoolText = printf "Draw: %d Discard: %d Pool: %d"

cardImageDims = (372, 520)
cardImage :: IO Image
cardImage = do
   image <- liftIO $ imageNewFromFile "images/cards/card_back.jpg"
   return image

currentCardHeight :: Int -> Int
currentCardHeight width =
   let (cardWidth, cardHeight) = cardImageDims
       ratio = (fromIntegral cardHeight) / (fromIntegral cardWidth)
       height = fromIntegral . round $ (fromIntegral width / 6.0) * ratio
   in height

drawDiscardPoolLabel :: IO Label
drawDiscardPoolLabel = labelNew (Just $ drawDiscardPoolText 100 0 48)

playHistoryTextView :: IO TextView
playHistoryTextView = do
   view <- liftIO textViewNew
   textViewSetEditable view False
   textViewSetCursorVisible view False
   return view

infoBox :: IO (VBox, Image, Label, TextView)
infoBox = do
   box             <- vBoxNew False 0
   card            <- cardImage
   drawDiscardPool <- drawDiscardPoolLabel
   playHistory     <- playHistoryTextView
   boxPackStart box card            PackNatural 0
   boxPackStart box drawDiscardPool PackNatural 0
   boxPackStart box playHistory     PackGrow 0
   return (box, card, drawDiscardPool, playHistory)

opponentsBox :: IO HBox
opponentsBox = do
   box       <- hBoxNew False 0
   (opp1, _) <- tableauBox colorRed
   boxPackStart box opp1 PackGrow 0
   return box

phaseIconImage    = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/no_phase_icon.png" 32 32
scoreIconImage    = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/score_icon.png" 32 32
handIconImage     = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/hand_icon.png" 32 32
militaryIconImage = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/military_icon.png" 32 32

playerStatusBox :: IO HBox
playerStatusBox = do
   box          <- hBoxNew False 0
   empty1       <- hBoxNew False 0
   empty2       <- hBoxNew False 0
   phaseIcon    <- phaseIconImage
   scoreIcon    <- scoreIconImage
   handIcon     <- handIconImage
   militaryIcon <- militaryIconImage

   boxPackStart box empty1       PackGrow    0
   boxPackStart box phaseIcon    PackNatural 0
   boxPackStart box handIcon     PackNatural 0
   boxPackStart box scoreIcon    PackNatural 0
   boxPackStart box militaryIcon PackNatural 0
   boxPackStart box empty2       PackGrow    0
   return box

tableausBox :: IO (VBox, HBox, DrawingArea)
tableausBox = do
   box       <- vBoxNew False 0
   opponents <- opponentsBox
   sep       <- hSeparatorNew
   (player, tableau) <- tableauBox colorBlue
   liftIO $ boxPackStart box opponents PackGrow    0
   liftIO $ boxPackStart box sep       PackNatural 0
   liftIO $ boxPackStart box player    PackGrow    0
   return (box, opponents, tableau)

tableauBox :: Color -> IO (VBox, DrawingArea)
tableauBox color = do
   box          <- vBoxNew False 0
   tableau      <- tableauDrawingArea color
   playerStatus <- playerStatusBox
   boxPackStart box tableau        PackGrow    0
   boxPackStart box playerStatus   PackNatural 0
   return (box, tableau)

tableauDrawingArea :: Color -> IO DrawingArea
tableauDrawingArea color = do
   drawingArea <- drawingAreaNew
   widgetModifyBg drawingArea StateNormal color
   return drawingArea

strikeThroughLabel :: Label -> IO Label
strikeThroughLabel label = do
   text <- labelGetText label
   labelSetMarkup label ("<s>" ++ text ++ "</s>")
   return label

colorBoldLabel :: Label -> String -> IO Label
colorBoldLabel label color = do
   text <- labelGetText label
   labelSetMarkup label ("<b><big><span color=\"" ++ color ++ "\">" ++ text ++ "</span></big></b>")
   return label

exploreLabel = labelNew (Just "Explore") >>= strikeThroughLabel
developLabel = labelNew (Just "Develop") >>= strikeThroughLabel
settleLabel  = labelNew (Just "Settle")  >>= strikeThroughLabel
consumeLabel = labelNew (Just "Consume") >>= strikeThroughLabel
produceLabel = labelNew (Just "Produce") >>= strikeThroughLabel

-- TODO: Change this up to use phases Show to make a Map
phaseBox :: IO (HBox, [Label])
phaseBox = do
   box     <- hBoxNew False 0
   explore <- exploreLabel
   develop <- developLabel
   settle  <- settleLabel
   consume <- consumeLabel
   produce <- produceLabel
   boxPackStart box explore PackGrow 0
   boxPackStart box develop PackGrow 0
   boxPackStart box settle  PackGrow 0
   boxPackStart box consume PackGrow 0
   boxPackStart box produce PackGrow 0
   return (box, [explore, develop, settle, consume, produce])

doneButton = do
   button <- buttonNewWithLabel "Done"
   widgetSetSensitive button False
   return button

discardText num = "Choose " ++ show num ++ " cards to discard"
discardContextLabel num = labelNew (Just $ discardText num)

developContextLabel = labelNew (Just $ "Choose 1 card to develop")
settleContextLabel = labelNew (Just $ "Choose 1 card to settle")
chooseConsumePowerContextLabel = labelNew (Just $ "Choose a consume power")
chooseGoodContextLabel cardName = labelNew (Just $ "Choose a good for " ++ cardName)
produceContextLabel = labelNew (Just $ "Choose a windfall to produce on")

contextBox :: IO HBox
contextBox = do
   box <- hBoxNew False 0
   context <- discardContextLabel 2
   boxPackStart box context PackGrow 0
   return box

actionBox = do
   box  <- hBoxNew False 0
   done <- doneButton
   context <- contextBox
   boxPackStart box context PackGrow    0
   boxPackStart box done    PackNatural 0
   return (box, done, context)

cardPadding :: Int -> Int
cardPadding height = height `quot` 10

currentHandHeight :: Int -> Int
currentHandHeight width =
   let height = currentCardHeight width
       padding = cardPadding height
   in height + padding

lookupCardPixbuf :: Deck -> String -> Pixbuf
lookupCardPixbuf m card = fromMaybe (error $ "card lookup failed for card: " ++ card) (M.lookup card m)

cardBack :: Deck -> Pixbuf
cardBack deck = lookupCardPixbuf deck "card_back.jpg"

cardDisabled :: Deck -> Pixbuf
cardDisabled deck = lookupCardPixbuf deck "card_disabled.png"

setPixbuf :: Image -> Deck -> String -> IO ()
setPixbuf image deck card = imageSetFromPixbuf image (lookupCardPixbuf deck card)

getPixbufsForHand :: Deck -> Hand -> [Pixbuf]
getPixbufsForHand deck = map (\(HandCard name _) -> lookupCardPixbuf deck name)

getPixbufsForTableau :: Deck -> Tableau -> [Pixbuf]
getPixbufsForTableau deck = map (\(TableauCard name _ _) -> lookupCardPixbuf deck name)

getTableauCardsXY :: Tableau -> (Int, Int) -> [(TableauCard, (Int, Int))]
getTableauCardsXY tableau (width, height) =
   let xOffset = width `quot` 6
       pos card i = (card, (xOffset * (i `rem` 6), if i >= 6 then height `quot` 2 else 0))
   in mapI pos tableau

cardDims :: (Int, Int) -> (Int, Int)
cardDims (width, _) = (width `quot` 6, currentCardHeight width)

getTableauCardIndexFromXY :: Tableau -> (Double, Double) -> (Int, Int) -> Maybe Int
getTableauCardIndexFromXY tableau pos size =
   let (cardWidth, cardHeight) = cardDims size
       (x, y) = overBoth round pos
       findCardFunc ((_, (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       cards = getTableauCardsXY tableau size
       zipIndices l = zip l [0..length l]
   in foldr findCardFunc Nothing (zipIndices cards)

drawCurrentTableau :: GameGUI -> Deck -> EventM EExpose Bool
drawCurrentTableau gui deck = do
   let drawingArea = getPlayerTableau (getGamePlayGUI gui)
       stateRef = getGUIState gui
   state <- liftIO $ readIORef stateRef
   let tableau = currentTableau state
   drawWindow <- liftIO $ widgetGetDrawWindow drawingArea
   size@(width, height) <- liftIO $ widgetGetSize drawingArea
   gc <- liftIO $ gcNew drawWindow
   let cardHeight  = currentCardHeight width
       cards = getTableauCardsXY tableau size
   cardBackPixbuf <- liftIO $
         pixbufScaleSimple (cardBack deck) (width `quot` 8) (cardHeight * 3 `quot` 4) InterpBilinear
   selectPixbuf <- liftIO $ scaleCard deck "card_selection.png" width cardHeight
   disabledPixbuf <- liftIO $ scaleCard deck "card_disabled.png" width cardHeight
   liftIO $ forM_ cards (\(TableauCard name selected hasGood, (destX, destY)) -> do
      pixbuf <- scaleCard deck name width cardHeight
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when hasGood $
         let goodX = destX + width `quot` 24
             goodY = destY + cardHeight `quot` 4
         in drawPixbuf drawWindow gc cardBackPixbuf 0 0
                       goodX goodY
                       (-1) (-1) RgbDitherNone 0 0
      when (isSelected selected) $
         drawPixbuf drawWindow gc
                    selectPixbuf
                    0 0                                -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0        -- dithering
      when (isDisabled selected) $
         drawPixbuf drawWindow gc
                    disabledPixbuf
                    0 0                                -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0)       -- dithering
   liftIO $ widgetQueueDraw drawingArea
   return True

motionInTableau :: GameGUI -> Deck -> Tableau -> EventM EMotion Bool
motionInTableau gui deck tableau = do
   let gamePlayGUI = getGamePlayGUI gui
       drawingArea = getPlayerTableau $ getGamePlayGUI gui
   size <- liftIO $ widgetGetSize drawingArea
   pos <- eventCoordinates
   let ndx = getTableauCardIndexFromXY tableau pos size
   case ndx of
      Just i -> let TableauCard name _ _ = tableau !! i
                in liftIO $ setPixbuf (getCard $ getInfoGUI gui) deck name
   return True

verifyNumberToConsume :: GameGUI -> Tableau -> Int -> IO ()
verifyNumberToConsume gui tableau num =
   let numSelected = length (filter (\(TableauCard _ selected _) -> isSelected selected) tableau)
   in widgetSetSensitive (getDone $ getGamePlayGUI gui) (numSelected == num)

buttonPressedInTableauChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInTableauChooseGood gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getPlayerTableau $ getGamePlayGUI gui
       tableau = currentTableau state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   pos <- eventCoordinates
   let ndx = getTableauCardIndexFromXY tableau pos size
   case ndx of
      Just i -> liftIO $ when (clickType == SingleClick &&
                               button == LeftButton) $ do
                   let newTableau = toggleTableauCardAtIndex tableau i
                   writeIORef stateRef (state { currentTableau = newTableau })
                   verifyNumberToConsume gui newTableau (numDiscard state)
                   widgetQueueDraw drawingArea
   return True

buttonPressedInTableauProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInTableauProduce gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getPlayerTableau (getGamePlayGUI gui)
       tableau = currentTableau state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   pos <- eventCoordinates
   let ndx = getTableauCardIndexFromXY tableau pos size
   case ndx of
      Just i -> liftIO $ when (clickType == SingleClick &&
                               button == LeftButton) $ do
                   let newTableau = toggleTableauCardAtIndex tableau i
                   writeIORef stateRef (state { currentTableau = newTableau })
                   verifyNumberToConsume gui newTableau (numDiscard state)
                   widgetQueueDraw drawingArea
   return True

isSelected :: Selected -> Bool
isSelected Selected = True
isSelected _ = False

isDisabled :: Selected -> Bool
isDisabled Disabled = True
isDisabled _ = False

toggleSelection :: Selected -> Selected
toggleSelection Selected = UnSelected
toggleSelection UnSelected = Selected
toggleSelection a = a

toggleTableauCardAtIndex :: Tableau -> Int -> Tableau
toggleTableauCardAtIndex [] _        = []
toggleTableauCardAtIndex ((TableauCard name selected hasGood):xs) 0 =
   TableauCard name (toggleSelection selected) hasGood : xs
toggleTableauCardAtIndex (x:xs) ndx  =
   x : toggleTableauCardAtIndex xs (ndx - 1)

toggleHandCardAtIndex :: Hand -> Int -> Hand
toggleHandCardAtIndex [] _        = []
toggleHandCardAtIndex ((HandCard name selected):xs) 0 = HandCard name (toggleSelection selected) : xs
toggleHandCardAtIndex (x:xs) ndx  = x : toggleHandCardAtIndex xs (ndx - 1)

toggleExclusiveCardAtIndex :: Hand -> Int -> Hand
toggleExclusiveCardAtIndex [] _        = []
toggleExclusiveCardAtIndex ((HandCard name selected):xs) 0 =
   HandCard name (toggleSelection selected) : toggleExclusiveCardAtIndex xs (-1)
toggleExclusiveCardAtIndex ((HandCard name Selected):xs) ndx =
   HandCard name UnSelected : toggleExclusiveCardAtIndex xs (ndx - 1)
toggleExclusiveCardAtIndex ((HandCard name selected):xs) ndx =
   HandCard name selected : toggleExclusiveCardAtIndex xs (ndx - 1)

getHandCardsXYForDiscard :: Hand -> Int -> [(HandCard, (Int, Int))]
getHandCardsXYForDiscard hand width =
   let xOffset = xOffsetInHand width (length hand)
       pad = cardPadding (currentCardHeight width)
       pos c@(HandCard _ selected) i = (c, (xOffset * i, if isSelected selected then 0 else pad))
   in mapI pos hand

getHandCardIndexFromXYDiscard :: Hand -> (Double, Double) -> (Int, Int) -> Maybe Int
getHandCardIndexFromXYDiscard hand pos size@(width, _) =
   let (cardWidth, cardHeight) = cardDims size
       (x, y) = overBoth round pos
       findCardFunc ((_, (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       cards = getHandCardsXYForDiscard hand width
       zipIndices l = zip l [0..length l]
   in foldr findCardFunc Nothing (zipIndices cards)

drawCurrentHandDiscard :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandDiscard gui deck stateRef = do
   let drawingArea = getHand $ getGamePlayGUI gui
   state           <- liftIO $ readIORef stateRef
   (width, _)      <- liftIO $ widgetGetSize drawingArea
   drawWindow      <- liftIO $ widgetGetDrawWindow drawingArea
   gc              <- liftIO $ gcNew drawWindow
   let hand    = currentHand state
       height  = currentCardHeight width
       cards   = getPixbufsForHand deck hand
       xOffset = xOffsetInHand width (length hand)
   discardedPixbuf <- liftIO $ scaleCard deck "card_discarded.png" width height
   liftIO$ forM_ (getHandCardsXYForDiscard hand width) (\((HandCard name selected), (destX, destY)) -> do
      pixbuf <- scaleCard deck name width height
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when (isSelected selected) $ drawPixbuf drawWindow gc
                                 discardedPixbuf
                                 0 0
                                 destX destY
                                 (-1) (-1) RgbDitherNone 0 0)       -- dithering
   liftIO $ widgetQueueDraw drawingArea
   return True

motionInHandDiscard :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandDiscard gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize (getHand $ getGamePlayGUI gui)
   pos <- eventCoordinates
   let hand = currentHand state
       ndx = getHandCardIndexFromXYDiscard hand pos size
   case ndx of
      Just i -> let HandCard name _ = hand !! i
                in liftIO $ setPixbuf (getCard $ getInfoGUI gui) deck name
   return True

buttonPressedInHandDiscard :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandDiscard gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getHand $ getGamePlayGUI gui
       hand = currentHand state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   coords <- eventCoordinates
   let ndx = getHandCardIndexFromXYDiscard (currentHand state) coords size
   case ndx of
      Just i -> liftIO $ when (clickType == SingleClick &&
                               button == LeftButton) $ do
                   let newHand = toggleHandCardAtIndex hand i
                   writeIORef stateRef (state { currentHand = newHand })
                   verifyNumberToDiscard gui newHand (numDiscard state)
                   widgetQueueDraw drawingArea
   return True

getHandCardsXYForExplore :: Hand -> Hand -> Int -> ([(HandCard, (Int, Int))], [(HandCard, (Int, Int))])
getHandCardsXYForExplore hand explore width =
   let xOffset = xOffsetInHand width (1 + length (hand ++ explore))
       pad = cardPadding (currentCardHeight width)
       handPosition c i = (c, (xOffset * i, pad))
       explorePosition c@(HandCard _ selected) i =
         (c, (xOffset * (i + 1 + length hand), if isSelected selected then 0 else pad))
   in  (mapI handPosition hand, mapI explorePosition explore)

getHandCardIndexFromXYExplore :: Hand -> Hand -> (Double, Double) -> (Int, Int) -> (Maybe Int, Maybe Int)
getHandCardIndexFromXYExplore hand explore pos size@(width, _) =
   let (cardWidth, cardHeight) = cardDims size
       (x, y) = overBoth round pos
       findCardFunc ((HandCard _ selected, (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       (handCards, exploreCards) = getHandCardsXYForExplore hand explore width
       zipIndices l = zip l [0..length l]
   in (foldr findCardFunc Nothing (zipIndices handCards),
       foldr findCardFunc Nothing (zipIndices exploreCards))

drawCurrentHandExplore :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandExplore gui deck stateRef = do
   let drawingArea = getHand $ getGamePlayGUI gui
   state      <- liftIO $ readIORef stateRef
   (width, _) <- liftIO $ widgetGetSize drawingArea
   drawWindow <- liftIO $ widgetGetDrawWindow drawingArea
   gc         <- liftIO $ gcNew drawWindow
   let height  = currentCardHeight width
   let (hand, explore) = getHandCardsXYForExplore (currentHand state) (exploreCards state) width
   disabledPixbuf <- liftIO $ scaleCard deck "card_disabled.png" width height
   liftIO $ forM_ hand $ \(HandCard name _, (destX, destY)) -> do
        pixbuf <- scaleCard deck name width height
        drawPixbuf drawWindow gc
                   pixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0        -- dithering
        drawPixbuf drawWindow gc
                   disabledPixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0        -- dithering

   discardedPixbuf <- liftIO $ scaleCard deck "card_discarded.png" width height
   liftIO $ forM_ explore $ \(HandCard name selected, (destX, destY)) -> do
        pixbuf <- scaleCard deck name width height
        drawPixbuf drawWindow gc
                   pixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0        -- dithering
        when (isSelected selected) $
           drawPixbuf drawWindow gc
                      discardedPixbuf
                      0 0                                -- srcx srcy
                      destX destY
                      (-1) (-1) RgbDitherNone 0 0        -- dithering

   liftIO $ widgetQueueDraw drawingArea
   return True

motionInHandExplore :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandExplore gui deck stateRef = do
   let drawingArea = getHand $ getGamePlayGUI gui
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize drawingArea
   coords <- eventCoordinates
   let explore = exploreCards state
       hand = currentHand state
       (handNdx, expNdx) = getHandCardIndexFromXYExplore hand explore coords size
       card = fmap (explore !!) expNdx `mplus` fmap (hand !!) handNdx
   case card of
      Just (HandCard name _) -> liftIO $ setPixbuf (getCard $ getInfoGUI gui) deck name
   return True

buttonPressedInHandExplore :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandExplore gui deck stateRef = do
   let drawingArea = getHand $ getGamePlayGUI gui
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize drawingArea
   coords <- eventCoordinates
   clickType <- eventClick
   button <- eventButton
   let explore = exploreCards state
       (handNdx, expNdx) = getHandCardIndexFromXYExplore (currentHand state) explore coords size
   case expNdx of
      Just i -> liftIO $ when (clickType == SingleClick &&
                               button == LeftButton) $ do
                   let newExplore = toggleHandCardAtIndex explore i
                   writeIORef stateRef (state { exploreCards = newExplore })
                   verifyNumberToDiscard gui newExplore (numDiscard state)
                   widgetQueueDraw drawingArea
   return True

getHandCardsXYForDevelop :: Hand -> Int -> [(HandCard, (Int, Int))]
getHandCardsXYForDevelop hand width =
   let xOffset = xOffsetInHand width (length hand)
       pad = cardPadding (currentCardHeight width)
       pos c@(HandCard _ _) i = (c, (xOffset * i, pad))
   in mapI pos hand

getHandCardIndexFromXYDevelop :: Hand -> (Double, Double) -> (Int, Int) -> Maybe Int
getHandCardIndexFromXYDevelop hand pos size@(width, _) =
   let (cardWidth, cardHeight) = cardDims size
       (x, y) = overBoth round pos
       findCardFunc ((_, (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       cards = getHandCardsXYForDiscard hand width
       zipIndices l = zip l [0..length l]
   in foldr findCardFunc Nothing (zipIndices cards)

drawCurrentHandDevelop :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandDevelop gui deck stateRef = do
   let drawingArea = getHand $ getGamePlayGUI gui
   state           <- liftIO $ readIORef stateRef
   (width, _)      <- liftIO $ widgetGetSize drawingArea
   drawWindow      <- liftIO $ widgetGetDrawWindow drawingArea
   gc              <- liftIO $ gcNew drawWindow
   let hand    = currentHand state
       height  = currentCardHeight width
       cards   = getPixbufsForHand deck hand
       xOffset = xOffsetInHand width (length hand)
   selectPixbuf <- liftIO $ scaleCard deck "card_selection.png" width height
   disablePixbuf <- liftIO $ scaleCard deck "card_disabled.png" width height
   liftIO$ forM_ (getHandCardsXYForDevelop hand width) (\((HandCard name selected), (destX, destY)) -> do
      pixbuf <- scaleCard deck name width height
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when (isSelected selected) $
         drawPixbuf drawWindow gc
                    selectPixbuf
                    0 0                          -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0  -- dithering
      when (isDisabled selected) $
         drawPixbuf drawWindow gc
                    disablePixbuf
                    0 0                          -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0) -- dithering
   liftIO $ widgetQueueDraw drawingArea
   return True

motionInHandDevelop :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandDevelop gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize (getHand (getGamePlayGUI gui))
   pos <- eventCoordinates
   let hand = currentHand state
       ndx = getHandCardIndexFromXYDevelop hand pos size
   case ndx of
      Just i -> let HandCard name _ = hand !! i
                in liftIO $ setPixbuf (getCard $ getInfoGUI gui) deck name
   return True

buttonPressedInHandDevelop :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandDevelop gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getHand (getGamePlayGUI gui)
       hand = currentHand state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   coords <- eventCoordinates
   let ndx = getHandCardIndexFromXYDiscard (currentHand state) coords size
   case ndx of
      Just i -> liftIO $ when (clickType == SingleClick &&
                               button == LeftButton) $ do
                   let newHand = toggleExclusiveCardAtIndex hand i
                   writeIORef stateRef (state { currentHand = newHand })
                   verifyNumberToDevelop gui newHand
                   widgetQueueDraw drawingArea
   return True

drawCurrentHandSettle :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandSettle = drawCurrentHandDevelop

motionInHandSettle :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandSettle = motionInHandDevelop

buttonPressedInHandSettle :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandSettle = buttonPressedInHandDevelop

drawCurrentHandChooseConsume :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandChooseConsume = drawCurrentHandDevelop

motionInHandChooseConsume :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandChooseConsume = motionInHandDevelop

buttonPressedInHandChooseConsume :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandChooseConsume _ _ _ = return True

drawCurrentHandChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandChooseGood = drawCurrentHandDevelop

drawCurrentHandProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandProduce = drawCurrentHandDevelop

motionInHandChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandChooseGood = motionInHandDevelop

motionInHandProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandProduce = motionInHandDevelop

buttonPressedInHandChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandChooseGood _ _ _ = return True

buttonPressedInHandProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandProduce _ _ _ = return True

xOffsetInHand :: Int -> Int -> Int
xOffsetInHand width numCards = min (width `quot` 6) (width `quot` numCards)

mapI :: (a -> Int -> b) -> [a] -> [b]
mapI f l = zipWith f l [0..length l]

verifyNumberToDevelop :: GameGUI -> Hand -> IO ()
verifyNumberToDevelop gui hand = do
   let numSelected = length (filter (\(HandCard _ selected) -> isSelected selected) hand)
   widgetSetSensitive (getDone $ getGamePlayGUI gui) (numSelected `elem` [0, 1])
   return ()

verifyNumberToDiscard :: GameGUI -> Hand -> Int -> IO ()
verifyNumberToDiscard gui hand num = do
   let numSelected = length (filter (\(HandCard _ selected) -> isSelected selected) hand)
   widgetSetSensitive (getDone $ getGamePlayGUI gui) (numSelected == num)
   return ()


scaleCard :: Deck -> String -> Int -> Int -> IO Pixbuf
scaleCard deck name width height = pixbufScaleSimple (lookupCardPixbuf deck name) (width `quot` 6) height InterpBilinear


requestHandDrawingAreaSize drawingArea _ = do
   (width, height) <- widgetGetSize drawingArea
   widgetSetSizeRequest drawingArea (-1) (currentHandHeight width)

-- The Drawing Area for the player's current hand.
handDrawingArea ::  IO DrawingArea
handDrawingArea = do
   drawingArea <- drawingAreaNew
   onSizeAllocate drawingArea (requestHandDrawingAreaSize drawingArea)
   return drawingArea

colorBlue = Color 0x9000 0xa700 0xca00
colorRed  = Color 0xe700 0x6d00 0x6400

playerBox = do
   box     <- vBoxNew False 0
   (phase, phaseLabels) <- phaseBox
   (action, done, context)  <- actionBox
   hand    <- handDrawingArea
   sep1    <- hSeparatorNew
   sep2    <- hSeparatorNew
   boxPackStart box phase   PackNatural 0
   boxPackStart box sep1    PackNatural 0
   boxPackStart box action  PackNatural 0
   boxPackStart box sep2    PackNatural 0
   boxPackStart box hand    PackNatural 0
   return (box, phaseLabels, done, context, hand)

gamePlayBox = do
   box <- vBoxNew False 0
   (tableaus, opponents, playerTableau) <- tableausBox
   (player, phaseLabels, done, context, hand) <- playerBox
   sep <- hSeparatorNew
   boxPackStart box tableaus PackGrow    0
   boxPackStart box player   PackNatural 0
   return (box, opponents, playerTableau, phaseLabels, done, context, hand)

menuBar :: IO MenuBar
menuBar = do
   menu  <- menuBarNew
   game  <- menuItemNewWithLabel "Game"
   containerAdd menu game
   return menu

mainBox = do
   box <- hBoxNew False 0
   (info, card, drawDiscardPool, playHistory) <- infoBox
   (gamePlay, opponents, playerTableau, phaseLabels, done, context, hand) <- gamePlayBox
   sep <- liftIO vSeparatorNew
   boxPackStart box info     PackNatural 0
   boxPackStart box sep      PackNatural 0
   boxPackStart box gamePlay PackGrow    0
   return (box,
           InfoGUI card drawDiscardPool playHistory,
           GamePlayGUI opponents
                       playerTableau
                       (phaseLabels !! 0)
                       (phaseLabels !! 1)
                       (phaseLabels !! 2)
                       (phaseLabels !! 3)
                       (phaseLabels !! 4)
                       done
                       context
                       hand
          )

topLevelBox = do
   box  <- vBoxNew False 0
   (main, infoGUI, gamePlayGUI) <- mainBox
   menu <- menuBar
   boxPackStart box menu PackNatural 0
   boxPackStart box main PackGrow    0
   return (box, menu, infoGUI, gamePlayGUI)

gameWindow = do
   window <- liftIO windowNew
   (box, menu, infoGUI, gamePlayGUI) <- topLevelBox
   containerAdd window box
   consumePowers <- comboBoxNewText -- Doesn't get added to the window.
   stateRef <- newIORef (GUIState [] [] [] Discard 0 consumePowers)
   return (window, GameGUI menu infoGUI gamePlayGUI stateRef)

cardImageDirectory = "images" ++ pathSeparator : "cards"

loadCardPixbuf :: FilePath -> IO (String, Pixbuf)
loadCardPixbuf file = do
   pixbuf <- pixbufNewFromFile (cardImageDirectory ++ pathSeparator:file)
   return (file, pixbuf)

loadCardPixbufs :: IO (M.Map String Pixbuf)
loadCardPixbufs = do
   cardNames <- liftM (filter (\x -> x /= "." && x /= "..")) (getDirectoryContents cardImageDirectory)
   cardPairs <- mapM loadCardPixbuf cardNames
   return $ M.fromList cardPairs

consumePowerOptionMenu :: [String] -> IO ComboBox
consumePowerOptionMenu powers = do
   menu <- comboBoxNewText
   mapM_ (comboBoxAppendText menu) powers
   return menu

initializeGUI :: IO GameGUI
initializeGUI = do
   initGUI
   deck <- loadCardPixbufs
   (window, gui) <- gameWindow

   widgetAddEvents (getHand (getGamePlayGUI gui)) [PointerMotionMask, ButtonPressMask]
   on (getHand (getGamePlayGUI gui)) exposeEvent (drawCurrentHand gui deck)
   on (getHand (getGamePlayGUI gui)) motionNotifyEvent (motionInHand gui deck)
   on (getHand (getGamePlayGUI gui)) buttonPressEvent (buttonPressedInHand gui deck)

   widgetAddEvents (getPlayerTableau $ getGamePlayGUI gui) [PointerMotionMask]
   on (getPlayerTableau $ getGamePlayGUI gui) exposeEvent (drawCurrentTableau gui deck)
   on (getPlayerTableau $ getGamePlayGUI gui) motionNotifyEvent (motionInTableau gui deck [])
   on (getPlayerTableau $ getGamePlayGUI gui) buttonPressEvent (buttonPressedInTableau gui deck)

   on (getDone $ getGamePlayGUI gui) buttonActivated (doneButtonClicked gui)

   onDestroy window mainQuit
   widgetShowAll window
   return gui

startGUI :: IO ()
startGUI = mainGUI

decodeMessage :: GameGUI -> NetString -> IO ()
decodeMessage gui msgStr =
   let message :: InMessage
       message = read msgStr
   in case message of
         (DiscardMessage num)       -> beginDiscard gui num
         (ExploreMessage cards num) -> beginExplorePhase gui cards num
         (DevelopMessage cards)     -> beginDevelopPhase gui cards
         (SettleMessage cards)      -> beginSettlePhase gui cards
         (ChooseConsumePowerMessage powers) -> beginChooseConsumePower gui powers
         (ChooseGoodMessage cards num cardName) -> beginChooseGood gui cards num cardName

getSelectedHandCards :: GameGUI -> IO [HandCard]
getSelectedHandCards gui = do
   let stateRef = getGUIState gui
   state <- readIORef stateRef
   return $ filter (\(HandCard _ selected) -> isSelected selected) (currentHand state)

getSelectedExploreCards :: GameGUI -> IO [HandCard]
getSelectedExploreCards gui = do
   let stateRef = getGUIState gui
   state <- readIORef stateRef
   return $ filter (\(HandCard _ selected) -> isSelected selected) (exploreCards state)

getSelectedTableauCards :: GameGUI -> IO [TableauCard]
getSelectedTableauCards gui = do
   let stateRef = getGUIState gui
   state <- readIORef stateRef
   return $ filter (\(TableauCard _ selected _) -> isSelected selected) (currentTableau state)

getComboBoxActiveText :: ComboBox -> IO String
getComboBoxActiveText combo = do
   text <- comboBoxGetActiveText combo
   return $ fromMaybe (error "unexpected empty combo box") text

doneButtonClicked :: GameGUI -> IO ()
doneButtonClicked gui = do
   let stateRef = getGUIState gui
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> sendMessage =<< liftM FinishedDiscard (getSelectedHandCards gui)
      Explore -> sendMessage =<< liftM FinishedExplore (getSelectedExploreCards gui)
      Develop -> sendMessage =<< liftM FinishedDevelop (liftM listToMaybe $ getSelectedHandCards gui)
      Settle -> sendMessage =<< liftM FinishedSettle (liftM listToMaybe $ getSelectedHandCards gui)
      ChooseConsumePower -> sendMessage =<< liftM FinishedChooseConsumePower
                                                  (getComboBoxActiveText $ consumePowers state)
      ChooseGood -> sendMessage =<< liftM FinishedChooseGood (getSelectedTableauCards gui)
      Produce ->  sendMessage =<< liftM FinishedProduce (liftM listToMaybe $ getSelectedTableauCards gui)

buttonPressedInTableau :: GameGUI -> Deck -> EventM EButton Bool
buttonPressedInTableau gui deck = do
   let stateRef = getGUIState gui
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      ChooseGood -> buttonPressedInTableauChooseGood gui deck stateRef
      Produce -> buttonPressedInTableauProduce gui deck stateRef
      _ -> return True

drawCurrentHand :: GameGUI -> Deck -> EventM EExpose Bool
drawCurrentHand gui deck = do
   let stateRef = getGUIState gui
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> drawCurrentHandDiscard gui deck stateRef
      Explore -> drawCurrentHandExplore gui deck stateRef
      Develop -> drawCurrentHandDevelop gui deck stateRef
      Settle -> drawCurrentHandSettle gui deck stateRef
      ChooseConsumePower -> drawCurrentHandChooseConsume gui deck stateRef
      ChooseGood -> drawCurrentHandChooseGood gui deck stateRef
      Produce -> drawCurrentHandProduce gui deck stateRef

motionInHand :: GameGUI -> Deck -> EventM EMotion Bool
motionInHand gui deck = do
   let stateRef = getGUIState gui
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> motionInHandDiscard gui deck stateRef
      Explore -> motionInHandExplore gui deck stateRef
      Develop -> motionInHandDevelop gui deck stateRef
      Settle  -> motionInHandSettle gui deck stateRef
      ChooseConsumePower -> motionInHandChooseConsume gui deck stateRef
      ChooseGood -> motionInHandChooseGood gui deck stateRef
      Produce -> motionInHandProduce gui deck stateRef

buttonPressedInHand :: GameGUI -> Deck -> EventM EButton Bool
buttonPressedInHand gui deck = do
   let stateRef = getGUIState gui
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> buttonPressedInHandDiscard gui deck stateRef
      Explore -> buttonPressedInHandExplore gui deck stateRef
      Develop -> buttonPressedInHandDevelop gui deck stateRef
      Settle -> buttonPressedInHandSettle gui deck stateRef
      ChooseConsumePower -> buttonPressedInHandChooseConsume gui deck stateRef
      ChooseGood -> buttonPressedInHandChooseGood gui deck stateRef
      Produce -> buttonPressedInHandProduce gui deck stateRef

containerRemoveChildren container = do
   toRemove <- containerGetChildren container
   mapM_ (containerRemove container) toRemove

-- Controller

beginDiscard :: GameGUI -> Int -> IO ()
beginDiscard gui numDiscard = do
   let gamePlayGUI = getGamePlayGUI gui
       contextBox = getContext gamePlayGUI
       stateRef   = getGUIState gui
   strikeThroughLabel (getExplore gamePlayGUI)
   containerRemoveChildren contextBox
   label <- discardContextLabel numDiscard
   containerAdd contextBox label
   widgetShowAll contextBox
   modifyIORef stateRef (\state ->
      state {
          currentActivity = Discard
         ,numDiscard = numDiscard
      })

beginExplorePhase :: GameGUI -> Hand -> Int -> IO ()
beginExplorePhase gui cards keepCount = do
   let gamePlayGUI = getGamePlayGUI gui
       contextBox = getContext gamePlayGUI
       stateRef   = getGUIState gui
       numDiscard = length cards - keepCount
   colorBoldLabel (getExplore gamePlayGUI) "blue"
   containerRemoveChildren contextBox
   label <- discardContextLabel numDiscard
   containerAdd contextBox label
   widgetShowAll contextBox
   modifyIORef stateRef (\state ->
      state {
          exploreCards = cards
         ,currentActivity = Explore
         ,numDiscard = numDiscard
      })

enableAllCards :: Hand -> Hand
enableAllCards = map enableCard
   where enableCard (HandCard name _) = HandCard name UnSelected

enableAndDisableCards :: Hand -> Hand -> Hand
enableAndDisableCards hand enableCards = map enableOrDisableCard hand
   where enableOrDisableCard card@(HandCard name selected) =
            if card `elem` enableCards
            then HandCard name UnSelected
            else HandCard name Disabled

enableAndDisableTableauCards :: Tableau -> Tableau -> Tableau
enableAndDisableTableauCards table enableCards = map enableOrDisableCard table
   where enableOrDisableCard card@(TableauCard name selected hasGood) =
            if card `elem` enableCards
            then TableauCard name UnSelected hasGood
            else TableauCard name Disabled hasGood

beginDevelopPhase :: GameGUI -> Hand -> IO ()
beginDevelopPhase gui developCards = do
   let stateRef = getGUIState gui
       gamePlayGUI = getGamePlayGUI gui
       contextBox = getContext gamePlayGUI
   colorBoldLabel (getDevelop gamePlayGUI) "blue"
   containerRemoveChildren contextBox
   label <- developContextLabel
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gamePlayGUI) True
   modifyIORef stateRef (\state ->
      state {
          currentActivity = Develop
         ,currentHand = (enableAndDisableCards (currentHand state) developCards)
      })

beginSettlePhase :: GameGUI -> Hand -> IO ()
beginSettlePhase gui settleCards = do
   let stateRef = getGUIState gui
       gamePlayGUI = getGamePlayGUI gui
       contextBox = getContext gamePlayGUI
   colorBoldLabel (getSettle gamePlayGUI) "blue"
   containerRemoveChildren contextBox
   label <- settleContextLabel
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gamePlayGUI) True
   modifyIORef stateRef (\state ->
      state {
          currentActivity = Settle
         ,currentHand = (enableAndDisableCards (currentHand state) settleCards)
      })

beginProducePhase :: GameGUI -> Tableau -> Int -> IO ()
beginProducePhase gui eligibleCards numGoods = do
   let stateRef = getGUIState gui
       gamePlayGUI = getGamePlayGUI gui
       contextBox = getContext gamePlayGUI
   colorBoldLabel (getProduce gamePlayGUI) "blue"
   containerRemoveChildren contextBox
   label <- produceContextLabel
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gamePlayGUI) False
   modifyIORef stateRef (\state ->
      state {
          currentActivity = Produce
         ,currentTableau = (enableAndDisableTableauCards (currentTableau state) eligibleCards)
      })

beginChooseConsumePower :: GameGUI -> [String] -> IO ()
beginChooseConsumePower gui consumePowers = do
   let stateRef = getGUIState gui
       gamePlayGUI = getGamePlayGUI gui
       contextBox = getContext gamePlayGUI
   colorBoldLabel (getConsume gamePlayGUI) "blue"
   containerRemoveChildren contextBox
   label <- chooseConsumePowerContextLabel
   menu <- consumePowerOptionMenu consumePowers
   containerAdd contextBox label
   boxPackStart contextBox menu PackNatural 0
   widgetShowAll contextBox
   widgetSetSensitive (getDone gamePlayGUI) True
   modifyIORef stateRef (\state ->
      state {
          currentActivity = ChooseConsumePower
         ,consumePowers = menu
      })

beginChooseGood :: GameGUI -> Tableau -> Int -> String -> IO ()
beginChooseGood gui eligibleCards numGoods cardName = do
   let stateRef = getGUIState gui
       gamePlayGUI = getGamePlayGUI gui
       contextBox = getContext gamePlayGUI
   containerRemoveChildren contextBox
   label <- chooseGoodContextLabel cardName
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gamePlayGUI) False
   modifyIORef stateRef (\state ->
      state {
          currentActivity = ChooseGood
         ,numDiscard = numGoods
         ,currentTableau = (enableAndDisableTableauCards (currentTableau state) eligibleCards)
      })

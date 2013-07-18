module Main where

import Control.Monad.State (liftIO)

import Graphics.UI.Gtk
    ( VBox(..)
    , vBoxNew

    , mainGUI
    , mainQuit
    , initGUI

    , onDestroy

    , TextView(..)
    , textViewNew
    , textViewGetBuffer
    , textViewSetEditable
    , textViewSetCursorVisible

    , textBufferGetIterAtOffset
    , textBufferGetText

    , hSeparatorNew

    , Packing(..)
    , vBoxNew
    , boxPackStart

    , Window(..)
    , windowNew

    , containerAdd
    , widgetShowAll

    , EventM(..)
    , Modifier(..)
    , EKey(..)
    , on
    , eventKeyName
    , eventModifier
    , keyPressEvent
    )

data ChatGUI = ChatGUI
    { chatView    :: TextView
    , chatLogView :: TextView
    }

main :: IO ()
main = do
    initGUI
    (window, gui) <- chatWindow

    on (chatView gui) keyPressEvent (keyPressInChatTextView gui)

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

sendChatMessage :: ChatGUI -> IO ()
sendChatMessage gui = do
    buffer    <- textViewGetBuffer (chatView gui)
    startIter <- textBufferGetIterAtOffset buffer 0
    endIter   <- textBufferGetIterAtOffset buffer (-1)
    text      <- textBufferGetText buffer startIter endIter False
    putStrLn text

keyPressInChatTextView :: ChatGUI -> EventM EKey Bool
keyPressInChatTextView gui = do
    modifiers <- eventModifier
    key <- eventKeyName
    case key of
        "Return"
            | Shift `elem` modifiers -> do
                liftIO $ sendChatMessage gui
                return False
            | otherwise -> return True
        _ -> return False

chatTextView :: IO TextView
chatTextView = textViewNew

chatLogTextView :: IO TextView
chatLogTextView = do
    chatLogView <- textViewNew
    textViewSetEditable chatLogView False
    textViewSetCursorVisible chatLogView False
    return chatLogView

chatWindow :: IO (Window, ChatGUI)
chatWindow = do
    window      <- windowNew
    box         <- vBoxNew False 0

    chatView    <- chatTextView
    sep         <- hSeparatorNew
    chatLogView <- chatLogTextView

    boxPackStart box chatLogView PackGrow    0
    boxPackStart box sep         PackNatural 0
    boxPackStart box chatView    PackNatural 0
    containerAdd window box

    return (window, ChatGUI chatView chatLogView)

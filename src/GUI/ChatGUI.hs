module Main where

import Control.Monad.State (liftIO)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTChanIO, atomically, writeTChan, TChan(..))
import Control.Monad (forever)
import Control.Monad.State (StateT(..), liftIO)
import Data.ByteString.Lazy.Char8 (unpack)

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
import Data.Aeson (encode)

import Client.TcpClient (startTcpClient)
import Client.Message
    (
      Message(..)
    , ChatMessageData(..)
    )

data ChatGUI = ChatGUI
    { chatView    :: TextView
    , chatLogView :: TextView
    }

main :: IO ()
main = do
    initGUI
    (window, gui) <- chatWindow

    (hostname, portStr) <- getArguments
    outChan <- newTChanIO
    on (chatView gui) keyPressEvent (keyPressInChatTextView outChan gui)

    forkIO $ startTcpClient handleFunc ChatState outChan hostname (read portStr)

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

sendChatMessage :: TChan String -> ChatGUI -> IO ()
sendChatMessage outChan gui = do
    buffer    <- textViewGetBuffer (chatView gui)
    startIter <- textBufferGetIterAtOffset buffer 0
    endIter   <- textBufferGetIterAtOffset buffer (-1)
    text      <- textBufferGetText buffer startIter endIter False
    atomically $ writeTChan outChan (encodeToString . ChatMessage $ ChatMessageData text)
  where
    encodeToString = unpack . encode

keyPressInChatTextView :: TChan String -> ChatGUI -> EventM EKey Bool
keyPressInChatTextView outChan gui = do
    modifiers <- eventModifier
    key <- eventKeyName
    case key of
        "Return"
            | Shift `elem` modifiers -> return False
            | otherwise -> do
                liftIO $ sendChatMessage outChan gui
                return True
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

data ChatState = ChatState
    {
    }
type ChatIO = StateT ChatState IO

handleFunc :: String -> ChatIO ()
handleFunc = liftIO . putStrLn

getArguments :: IO (String, String)
getArguments = do
    args <- getArgs
    case length args of
        2 -> let [host, portStr] = args
             in return (host, portStr)
        _ -> error "Usage: ChatClient <hostname> <port #>"

{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment (getArgs)
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM (newTChanIO, atomically, writeTChan, TChan(..))
import Control.Applicative ((<$>))
import Control.Monad (forever, void)
import Control.Monad.State (StateT(..), liftIO)
import Data.ByteString.Lazy.Char8 (unpack, pack)

import Graphics.UI.Gtk
    ( VBox(..)
    , vBoxNew
    , hBoxNew

    , ListStore(..)
    , listStoreNew
    , listStoreAppend
    , treeViewNewWithModel

    , mainGUI
    , postGUIAsync
    , mainQuit
    , initGUI

    , onDestroy

    , TextView(..)
    , textViewNew
    , textViewGetBuffer
    , textViewSetEditable
    , textViewSetCursorVisible
    , textViewScrollToIter

    , textBufferGetStartIter
    , textBufferGetEndIter
    , textBufferGetText
    , textBufferInsert
    , textBufferGetInsert
    , textBufferDelete

    , hSeparatorNew

    , Packing(..)
    , vBoxNew
    , boxPackStart

    , Window(..)
    , windowNew

    , containerAdd
    , widgetShowAll
    , onSizeAllocate

    , EventM(..)
    , Modifier(..)
    , EKey(..)
    , on
    , eventKeyName
    , eventModifier
    , keyPressEvent

    , PolicyType(..)
    , scrolledWindowNew
    , scrolledWindowSetPolicy
    )
import Data.Aeson (decode, encode)
import Control.Lens (makeLenses, use)

import Client.TcpClient (startTcpClient)
import Client.Message
    (
      Message(..)
    , ChatMessageData(..)
    , UserMessageData(..)
    )

data ChatGUI = ChatGUI
    { chatView    :: TextView
    , chatLogView :: TextView
    , userList    :: ListStore String
    }

data ChatState = ChatState
    { _chatGUI :: ChatGUI
    }
makeLenses ''ChatState

type ChatIO = StateT ChatState IO

main :: IO ()
main = do
    initGUI
    (window, gui) <- chatWindow

    (hostname, portStr) <- getArguments
    outChan <- newTChanIO
    on (chatView gui) keyPressEvent (keyPressInChatTextView outChan gui)

    forkIO $ startTcpClient handleFunc (connectFunc outChan) (ChatState gui) outChan hostname (read portStr)

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

encodeToString :: Message -> String
encodeToString = unpack . encode

sendNewUserMessage :: TChan String -> String -> IO ()
sendNewUserMessage outChan userName =
    atomically $ writeTChan outChan (encodeToString . UserMessage . UserMessageData $ userName)

sendChatMessage :: TChan String -> ChatGUI -> IO ()
sendChatMessage outChan gui = do
    buffer    <- textViewGetBuffer (chatView gui)
    startIter <- textBufferGetStartIter buffer
    endIter   <- textBufferGetEndIter buffer
    text      <- textBufferGetText buffer startIter endIter False
    textBufferDelete buffer startIter endIter
    atomically $ writeTChan outChan (encodeToString . ChatMessage . ChatMessageData $ text)

keyPressInChatTextView :: TChan String -> ChatGUI -> EventM EKey Bool
keyPressInChatTextView outChan gui = do
    modifiers <- eventModifier
    key <- eventKeyName
    case key of
        "Return"
            | Shift `elem` modifiers -> return False
            | otherwise -> do
                liftIO . postGUIAsync $ sendChatMessage outChan gui
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
    window <- windowNew

    chatView    <- chatTextView
    sep         <- hSeparatorNew

    chatLogView <- chatLogTextView
    onSizeAllocate chatLogView (\_ -> scrollToEnd chatLogView)

    chatLogWindow <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy chatLogWindow PolicyNever PolicyAutomatic
    containerAdd chatLogWindow chatLogView

    userList     <- listStoreNew ["bob loblaw"]
    userListView <- treeViewNewWithModel userList

    hbox <- hBoxNew False 0
    boxPackStart hbox userListView  PackGrow 0
    boxPackStart hbox chatLogWindow PackGrow 0

    box    <- vBoxNew False 0
    boxPackStart box hbox     PackGrow    0
    boxPackStart box sep      PackNatural 0
    boxPackStart box chatView PackNatural 0
    containerAdd window box

    return (window, ChatGUI chatView chatLogView userList)

scrollToEnd :: TextView -> IO ()
scrollToEnd textView = do
    buffer <- textViewGetBuffer textView
    iter   <- textBufferGetEndIter buffer
    void $ textViewScrollToIter textView iter 0.0 Nothing

appendText :: TextView -> String -> IO ()
appendText textView text = do
    buffer <- textViewGetBuffer textView
    iter   <- textBufferGetEndIter buffer
    textBufferInsert buffer iter (text ++ "\n")

appendUser :: ListStore String -> String -> IO ()
appendUser list username = void $ listStoreAppend list username

connectFunc :: TChan String -> ChatIO ()
connectFunc outChan = liftIO $ sendNewUserMessage outChan "chebert"

handleFunc :: String -> ChatIO ()
handleFunc json = do
    chatLogView <- chatLogView <$> use chatGUI
    userList    <- userList    <$> use chatGUI
    case decode $ pack json :: Maybe Message of
        Just (ChatMessage info) -> liftIO . postGUIAsync $ appendText chatLogView (message info)
        Just (UserMessage info) -> liftIO . postGUIAsync $ appendUser userList (userName info)
        _ -> return ()

getArguments :: IO (String, String)
getArguments = do
    args <- getArgs
    case length args of
        2 -> let [host, portStr] = args
             in return (host, portStr)
        _ -> error "Usage: ChatClient <hostname> <port #>"

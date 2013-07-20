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
    , boxSetHomogeneous

    , Button(..)
    , buttonNewWithLabel
    , buttonActivated

    , ListStore(..)
    , listStoreNew
    , listStoreAppend

    , labelNew

    , Entry(..)
    , entryNew
    , entrySetText
    , entryGetText

    , treeViewColumnNew
    , treeViewColumnSetTitle
    , treeViewColumnPackStart

    , treeViewNewWithModel
    , treeViewAppendColumn
    , treeViewGetSelection

    , SelectionMode(..)
    , treeSelectionSetMode

    , cellRendererTextNew
    , cellLayoutPackStart
    , cellLayoutSetAttributes
    , cellText
    , AttrOp(..)

    , mainGUI
    , postGUIAsync
    , mainQuit
    , initGUI

    , objectDestroy

    , TextView(..)
    , textViewNew
    , textViewGetBuffer
    , textViewSetEditable
    , textViewSetCursorVisible
    , textViewScrollToIter

    , textBufferGetStartIter
    , textBufferGetEndIter
    , textBufferGetText
    , textBufferSetText
    , textBufferInsert
    , textBufferGetInsert
    , textBufferDelete

    , hSeparatorNew

    , Packing(..)
    , vBoxNew
    , boxPackStart

    , Window(..)
    , windowNew

    , ContainerClass(..)
    , containerAdd
    , containerRemove
    , containerForeach

    , widgetShowAll
    , widgetDestroy
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

data NetworkGUI = NetworkGUI
    { portEntry     :: Entry
    , usernameEntry :: Entry
    , serverEntry   :: Entry
    , connectButton :: Button
    }

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

networkGUI :: Window -> IO NetworkGUI
networkGUI window = do
    serverLabel <- labelNew $ Just "Server name:"
    serverEntry <- entryNew
    entrySetText serverEntry "localhost"

    portLabel <- labelNew $ Just "Port:"
    portEntry <- entryNew
    entrySetText portEntry "8000"

    serverBox <- hBoxNew False 0
    boxSetHomogeneous serverBox True
    boxPackStart serverBox serverLabel PackGrow 0
    boxPackStart serverBox serverEntry PackGrow 0
    boxPackStart serverBox portLabel PackGrow 0
    boxPackStart serverBox portEntry PackGrow 0

    usernameLabel <- labelNew $ Just "Username:"
    usernameEntry <- entryNew
    entrySetText usernameEntry "chebert"

    usernameBox <- hBoxNew False 0
    boxSetHomogeneous usernameBox True
    boxPackStart usernameBox usernameLabel PackGrow 0
    boxPackStart usernameBox usernameEntry PackGrow 0

    connectButton <- buttonNewWithLabel "Connect"

    mainBox <- vBoxNew False 0
    boxPackStart mainBox serverBox PackGrow 0
    boxPackStart mainBox usernameBox PackGrow 0
    boxPackStart mainBox connectButton PackNatural 0
    containerAdd window mainBox

    return $ NetworkGUI portEntry usernameEntry serverEntry connectButton

connectButtonClicked :: Window -> Window -> NetworkGUI -> TChan String -> IO ()
connectButtonClicked networkWindow window networkGUI outChan = do
    chatGUI <- chatWindow window outChan
    portStr  <- entryGetText (portEntry networkGUI)
    hostname <- entryGetText (serverEntry networkGUI)
    -- TODO: Right now this assumes successful connection to the network.
    forkIO $ startTcpClient handleFunc (connectFunc outChan) (ChatState chatGUI) outChan hostname (read portStr)
    widgetDestroy networkWindow

main :: IO ()
main = do
    initGUI
    window <- windowNew
    networkWindow <- windowNew
    networkGUI <- networkGUI networkWindow

    outChan <- newTChanIO
    on (connectButton networkGUI) buttonActivated (connectButtonClicked networkWindow window networkGUI outChan)

    on window objectDestroy mainQuit
    widgetShowAll window
    widgetShowAll networkWindow
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

containerRemoveAll :: (ContainerClass c) => c -> IO ()
containerRemoveAll c = containerForeach c (containerRemove c)

chatWindow :: Window -> TChan String -> IO ChatGUI
chatWindow window outChan = do
    containerRemoveAll window

    chatView    <- chatTextView
    sep         <- hSeparatorNew

    chatLogView <- chatLogTextView
    onSizeAllocate chatLogView (\_ -> scrollToEnd chatLogView)

    chatLogWindow <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy chatLogWindow PolicyNever PolicyAutomatic
    containerAdd chatLogWindow chatLogView

    userList     <- listStoreNew []
    userListView <- treeViewNewWithModel userList

    column <- treeViewColumnNew
    treeViewColumnSetTitle column "Users"
    userNameCell <- cellRendererTextNew
    cellLayoutPackStart column userNameCell False
    cellLayoutSetAttributes column userNameCell userList
        $ \ind -> [cellText := ind]
    treeViewAppendColumn userListView column
    treeSelection <- treeViewGetSelection userListView
    treeSelectionSetMode treeSelection SelectionNone

    hbox <- hBoxNew False 0
    boxPackStart hbox userListView  PackNatural 0
    boxPackStart hbox chatLogWindow PackGrow    0

    box    <- vBoxNew False 0
    boxPackStart box hbox     PackGrow    0
    boxPackStart box sep      PackNatural 0
    boxPackStart box chatView PackNatural 0
    containerAdd window box

    let chatGUI = ChatGUI chatView chatLogView userList
    on chatView keyPressEvent (keyPressInChatTextView outChan chatGUI)

    widgetShowAll window
    return chatGUI

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

{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment (getArgs)
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM (newTChanIO, atomically, writeTChan, TChan(..))
import Control.Applicative ((<$>))
import Control.Monad (forM_, forever, void)
import Control.Monad.State (StateT(..), liftIO)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.List (elemIndex)

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
    , listStoreClear
    , listStoreAppend
    , listStoreToList
    , listStoreRemove

    , Label(..)
    , labelNew
    , labelSetText

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
    , postGUISync
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
import Control.Lens (makeLenses, use, (.=))

import Client.TcpClient (startTcpClient)
import Client.Message
    (
      Message(..)
    , ChatMessageData(..)
    , UserMessageData(..)
    , UserListMessageData(..)
    , DisconnectMessageData(..)
    )

data NetworkGUI = NetworkGUI
    { portEntry     :: Entry
    , usernameEntry :: Entry
    , serverEntry   :: Entry
    , connectButton :: Button
    , statusLabel   :: Label
    }

data ChatGUI = ChatGUI
    { chatView    :: TextView
    , chatLogView :: TextView
    , userList    :: ListStore String
    }

data ChatState = ChatState
    { _chatGUI   :: Maybe ChatGUI
    , _cUserName :: String
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

    statusLabel <- labelNew Nothing

    connectButton <- buttonNewWithLabel "Connect"

    mainBox <- vBoxNew False 0
    boxPackStart mainBox serverBox PackGrow 0
    boxPackStart mainBox usernameBox PackGrow 0
    boxPackStart mainBox statusLabel PackNatural 0
    boxPackStart mainBox connectButton PackNatural 0
    containerAdd window mainBox

    return $ NetworkGUI portEntry usernameEntry serverEntry connectButton statusLabel

connectButtonClicked :: Window -> Window -> NetworkGUI -> TChan String -> IO ()
connectButtonClicked networkWindow window networkGUI outChan = do
    portStr  <- entryGetText (portEntry networkGUI)
    hostname <- entryGetText (serverEntry networkGUI)
    userName <- entryGetText (usernameEntry networkGUI)
    labelSetText (statusLabel networkGUI) "Connecting..."
    void . forkIO $ startTcpClient (connectFunc networkWindow window networkGUI userName outChan)
                                   (failedConnectionFunc networkGUI)
                                   outChan
                                   hostname
                                   (read portStr)

failedConnectionFunc :: NetworkGUI -> IO ()
failedConnectionFunc networkGUI = labelSetText (statusLabel networkGUI) "Failed to connect."

connectFunc :: Window -> Window -> NetworkGUI -> String -> TChan String -> IO (String -> ChatIO (), ChatState)
connectFunc networkWindow window networkGUI userName outChan = do
    sendNewUserMessage outChan userName
    postGUIAsync $ labelSetText (statusLabel networkGUI) "Sending user info..."
    return (handleFunc window networkWindow outChan, ChatState Nothing userName)

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

replaceUsers :: ListStore String -> [String] -> IO ()
replaceUsers list usernames =
    listStoreClear list >>
    forM_ usernames (appendUser list)
  where
    appendUser :: ListStore String -> String -> IO ()
    appendUser list username = void $ listStoreAppend list username

removeUser :: ListStore String -> String -> IO ()
removeUser list username = do
    usernames <- listStoreToList list
    let may_ndx = username `elemIndex` usernames
    case may_ndx of
        Just ndx -> listStoreRemove list ndx
        _ -> putStrLn $ "Could not find user: " ++ username

handleFunc :: Window -> Window -> TChan String -> String -> ChatIO ()
handleFunc window networkWindow outChan json = do
    may_chatGUI <- use chatGUI
    case may_chatGUI of
        Just chatGUI ->
            case decode $ pack json :: Maybe Message of
                Just (ChatMessage info) -> liftIO . postGUIAsync $ appendText (chatLogView chatGUI) (message info)
                Just (UserListMessage info) -> liftIO . postGUIAsync $ replaceUsers (userList chatGUI) (userNames info)
                Just (DisconnectMessage info) -> liftIO . postGUIAsync $ removeUser (userList chatGUI) (dUserName info)
                _ -> return ()
        _ ->
            case decode $ pack json :: Maybe Message of
                Just (UserListMessage info) -> do
                    gui <- liftIO $ chatWindow window outChan
                    chatGUI .= Just gui
                    liftIO $ do
                        widgetDestroy networkWindow
                        postGUIAsync $ replaceUsers (userList gui) (userNames info)
                _ -> return ()

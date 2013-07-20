{-# LANGUAGE TemplateHaskell #-}
module Server.TcpServer
    (
      UserID(..)
    , UserMap(..)

    , startTcpServer
    , sendMessageToClient
    ) where

import Control.Exception (try, SomeException(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
    (
      TChan(..)
    , readTChan
    , writeTChan
    , newTChanIO
    )
import Control.Monad (when, forever)
import Control.Monad.State (get, liftIO, execStateT, evalStateT, StateT(..))
import Debug.Trace (traceShow)
import qualified Data.Map as M
import System.IO
    (
      hGetLine
    , hPutStrLn
    , hSetBuffering
    , BufferMode(..)
    , Handle(..)
    )
import Network
    (
      accept
    , withSocketsDo
    , listenOn
    , Socket
    , PortNumber(..)
    , PortID(..)
    )

import Control.Lens (makeLenses, (^.), (%=), use)

data User = User
    { userID   :: !UserID
    , userHandle :: !Handle
    };
type UserMap = M.Map UserID User

type ChannelMessage = (UserID, Handle, Maybe String)
-- | The client forwards messages to the main thread via the IpcChannel.
type IpcChannel = TChan ChannelMessage

type UserID = (String, PortNumber)
-- | State for a Client thread.
data ClientState = ClientState
    { _cID      :: !UserID  -- ^ The user id of the given client.
    , _cHandle  :: !Handle  -- ^ The handle used to commmunicate with the client.
    , _cChannel :: !IpcChannel
    };
makeLenses ''ClientState

-- | State for the Main thread.
data MainState = MainState
    { _mChannel :: !IpcChannel
    , _mUsers   :: !UserMap
    };
makeLenses ''MainState

-- | Lives on the client thread.
type ClientIO = StateT ClientState IO
-- | Lives on the Main thread.
type MainIO   = StateT MainState IO
-- | Lives on the Socket thread.
type SocketIO = IO

-- | Entry point into the system.
startTcpServer ::  -- | Function to handle incoming messages from the client.
         ((UserID, String) -> UserMap -> StateT a IO ())
      -> (UserID -> StateT a IO ())  -- ^ Function to handle new user connection.
      -> (UserID -> UserMap -> StateT a IO ())  -- ^ Function to handle user disconnection.
      -> a  -- ^ Initial state of the handler function
      -> Int  -- ^ The port number to listen on.
      -> IO ()
startTcpServer handleFunc newUserFunc userDisconnectFunc initialState portNum = withSocketsDo $ do
    let port = PortNumber . fromIntegral $ portNum
    socket  <- listenOn port
    channel <- newTChanIO
    forkIO $ socketConnectionHandler socket channel
    evalStateT (mainLoop handleFunc newUserFunc userDisconnectFunc initialState)
               (MainState channel M.empty)

-- | Adds a user to the mUser map if not already in there.
addUserIfNew :: (UserID, Handle) -> MainIO Bool
addUserIfNew (userid, handle) = do
    users <- use mUsers
    if M.notMember userid users
    then do
        mUsers %= M.insert userid (User userid handle)
        return True
    else return False

-- | Waits for messages from the client channel, and executes the given
-- commands.
mainLoop ::  -- | Function to handle incoming messages from the client.
            ((UserID, String) -> UserMap -> StateT a IO ())
         -- | Function to handle new user connection.
         -> (UserID -> StateT a IO ())
         -- | Function to handle user disconnection.
         -> (UserID -> UserMap -> StateT a IO ())
         -> a  -- ^ State of the handler function
         -> MainIO ()
mainLoop handleFunc newUserFunc userDisconnectFunc handleFuncState = do
    channel <- use mChannel
    (userid, handle, may_message) <- liftIO . atomically $ readTChan channel
    isNewUser <- addUserIfNew (userid, handle)
    users <- use mUsers
    case may_message of
        --  We have received a message from the user.
        Just message -> do
            if isNewUser
            --  Make sure we call the new user handler.
            then do
                state <- liftIO $ execStateT (newUserFunc userid) handleFuncState
                loopFunc (userid, message) users state
            --  Otherwise just continue with the loop.
            else
                loopFunc (userid, message) users handleFuncState
        --  Here, we haven't received a message because the client has
        --  disconnected.
        _ -> do
            state <- liftIO $ execStateT (userDisconnectFunc userid users) handleFuncState
            mUsers %= M.delete userid
            mainLoop handleFunc newUserFunc userDisconnectFunc state
  where
    loopFunc userMessage users state = do
        state' <- liftIO $ execStateT (handleFunc userMessage users) state
        mainLoop handleFunc newUserFunc userDisconnectFunc state'

-- | Sends a message to the given client.
sendMessageToClient :: UserMap  -- ^ UserMap containing the User.
                    -> UserID  -- ^ ID of the user.
                    -> String  -- ^ Message.
                    -> IO ()
sendMessageToClient users userid message = do
    let may_user = M.lookup userid users
    case may_user of
        Just user -> hPutStrLn (userHandle user) message
        Nothing -> debugLog $ "Bad userID: " ++ show userid

-- | Runs on the Socket thread to handle all client connections.
-- Spawns a new thread for each client.
socketConnectionHandler ::  -- | The socket that the server is listening on.
                           Socket
                        -- | The IPC channel used to communicate with the main
                        -- thread.
                        -> IpcChannel
                        -> SocketIO ()
socketConnectionHandler socket channel = forever $ do
    -- Wait for a client to connect
    (handle, hostName, port) <- accept socket
    -- Set buffering to be line by line (default is block)
    hSetBuffering handle LineBuffering
    -- Create a thread to process incoming messages from the new client
    let userid = (hostName, port)
    forkIO $ evalStateT clientMessageHandler (ClientState userid handle channel)

-- | Forwards a message to the Main thread to handle.
forwardMessage :: String  -- ^ The message.
               -> ClientIO ()
forwardMessage message = do
    state <- get
    liftIO . atomically $ writeTChan (state^.cChannel) (state^.cID, state^.cHandle, Just message)

clientDisconnected :: ClientIO ()
clientDisconnected = do
    state <- get
    liftIO . atomically $ writeTChan (state^.cChannel) (state^.cID, state^.cHandle, Nothing)

-- | Listens for client messages, and then forwards them to the main thread.
clientMessageHandler :: ClientIO ()
clientMessageHandler = do
    handle <- use cHandle
    either_input <- liftIO . try $ hGetLine handle
    case either_input of
        Left (SomeException _) -> do
            clientDisconnected
        Right input -> do
            forwardMessage input
            clientMessageHandler

-- | Logs a message to stderr in a monad.
debugLog :: (Monad m, Show a) => a -> m ()
debugLog message = traceShow message $ return ()

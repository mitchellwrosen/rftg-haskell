module Client.TcpClient
    (
      startTcpClient
    ) where

import Control.Monad (forever, liftM)
import Control.Monad.State (StateT(..), execStateT)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
    (
      atomically
    , newTChanIO
    , TChan(..)
    , writeTChan
    , readTChan
    , orElse
    , STM(..)
    )
import Control.Exception (bracket)
import Network (connectTo, withSocketsDo, PortID(..))
import System.IO (Handle, hClose, hGetLine, hFlush, hPutStrLn)

-- | Starts the TCP Client connection.
startTcpClient ::  -- | The handler callback for incoming messages.
                  (String -> StateT a IO ())
               -> a  -- ^ The initial state of the handler function.
               -- | The out-bound channel that delivers messages from the user.
               -> TChan String
               -- | The host name.
               -> String
               -- | The port #.
               -> Int
               -> IO ()
startTcpClient handleFunc handleFuncState outChan hostname port =
    withSocketsDo $ do
        let portID = PortNumber . fromIntegral $ port
        bracket (connectTo hostname portID)
                hClose
                (startListenCycle handleFunc handleFuncState outChan)

-- | Begins the cycle to listen for inbound messages.
startListenCycle :: -- | The handler callback.
                    (String -> StateT a IO ())
                 -> a  -- ^ Initial handler function state.
                 -> TChan String  -- ^ Out-bound channel.
                 -> Handle  -- ^ Handle to the socket.
                 -> IO ()
startListenCycle handleFunc handleFuncState outChan socket = do
    inChan <- newTChanIO
    forkIO $ listenLoop (hGetLine socket) inChan
    mainLoop handleFunc handleFuncState inChan outChan socket

-- | The main loop. Either delivers an outbound message or recieves an inbound
-- message.
mainLoop ::  -- | Out-bound message handler callback.
           (String -> StateT a IO ())
         -> a  -- ^ Current state of the handle function.
         -> TChan String  -- ^ In-bound messages channel.
         -> TChan String  -- ^ Out-bound messages channel.
         -> Handle  -- ^ Handle to communicate with the TCP socket connection.
         -> IO ()
mainLoop handleFunc handleFuncState inChan outChan socket = do
    input <- atomically $ select inChan outChan
    case input of
        -- from inChan, to user
        Left str -> do
            state <- execStateT (handleFunc str) handleFuncState
            mainLoop handleFunc state inChan outChan socket
        -- from outChan, to net
        Right str -> do
            hPutStrLn socket str
            hFlush socket
            mainLoop handleFunc handleFuncState inChan outChan socket

-- | Chooses either chan1 on the Left or chan2 on the Right, depending on which
-- is ready.
select :: TChan a -> TChan b -> STM (Either a b)
select chan1 chan2 =
   liftM Left (readTChan chan1) `orElse` liftM Right (readTChan chan2)

-- | Performs an action forever and writes the result to the channel.
listenLoop :: IO a -> TChan a -> IO ()
listenLoop action channel = forever $
    action >>= atomically . writeTChan channel

{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (mapM_)
import Control.Monad.State (liftIO, StateT(..))
import System.Environment (getArgs)

import Control.Monad (void)
import Control.Lens (makeLenses, use, (%=), (+=))
import Data.Aeson (decode', encode)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.List (delete)
import qualified Data.Map as M

import Server.TcpServer (sendMessageToClient, startTcpServer, UserID(..), UserMap(..))
import Client.Message

data ChatState = ChatState
    { _userIDs :: [UserID]
    , _userNames :: M.Map UserID String
    };
makeLenses ''ChatState

type ChatIO = StateT ChatState IO

main :: IO ()
main = do
    portNum <- parseCommandLineArguments
    startTcpServer handleFunc newUserFunc userDisconnectFunc (ChatState [] M.empty) portNum

-- | Callback for when client disconnections occur.
userDisconnectFunc :: UserID -> UserMap -> ChatIO ()
userDisconnectFunc userid userMap = do
    may_username <- fmap (M.lookup userid) (use userNames)
    case may_username of
        Just username -> do
            userIDs %= delete userid
            userNames %= M.delete userid
            sendMessageToAllClients userMap (disconnectMessage username)
        _ -> liftIO . putStrLn $ "Bad userID: " ++ show userid

-- | Callback to handle incoming messages.
handleFunc :: (UserID, String) -> UserMap -> ChatIO ()
handleFunc (userid, strMessage) userMap = do
    let may_message :: Maybe Message
        may_message = decode' $ pack strMessage
    case may_message of
        Just message -> executeCommand userid userMap message
        Nothing -> liftIO . putStrLn $ "Bad message: " ++ strMessage

sendMessageToAllClients :: UserMap -> String -> ChatIO ()
sendMessageToAllClients userMap msg =
    use userIDs >>=
    liftIO . mapM_ (\uid -> sendMessageToClient userMap uid msg)

disconnectMessage :: String -> String
disconnectMessage = unpack . encode . DisconnectMessage . DisconnectMessageData

chatMessage :: String -> String
chatMessage = unpack . encode . ChatMessage . ChatMessageData

userMessage :: String -> String
userMessage = unpack . encode . UserMessage . UserMessageData

executeCommand :: UserID -> UserMap -> Message -> ChatIO ()
executeCommand userid userMap (ChatMessage info) = do
    may_name <- fmap (M.lookup userid) (use userNames)
    case may_name of
        Just name -> sendMessageToAllClients userMap (chatMessage $ name ++ ": " ++ message info)
        _ -> liftIO . putStrLn $ "bad id: " ++ show userid
executeCommand userid userMap (UserMessage info) = do
    userNames %= M.insert userid (userName info)
    sendMessageToAllClients userMap (userMessage $ userName info)

-- | Callback to handle new users.
newUserFunc :: UserID -> ChatIO ()
newUserFunc userid = userIDs %= (userid:)

-- | Parses a PortID from the command line arguments.
-- Only looks at the first argument.
parseCommandLineArguments :: IO Int
parseCommandLineArguments = fmap (read . head) getArgs

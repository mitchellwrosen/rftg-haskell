{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.State (liftIO, StateT(..))
import System.Environment (getArgs)

import Control.Lens (makeLenses, use, (%=), (+=))
import Data.Aeson (decode')
import Data.ByteString.Lazy.Char8 (pack)

import Server.TcpServer (sendMessageToClient, startTcpServer, UserID(..), UserMap(..))
import Client.Message

data ChatState = ChatState
    {
      _userIDs :: [UserID]
    };
makeLenses ''ChatState

type ChatIO = StateT ChatState IO

main :: IO ()
main = do
    portNum <- parseCommandLineArguments
    startTcpServer handleFunc newUserFunc (ChatState []) portNum

handleFunc (userid, message) userMap =
    liftIO $ sendMessageToClient userMap userid message
-- | Callback to handle incoming messages.
{-
handleFunc :: (UserID, String) -> UserMap -> ChatIO ()
handleFunc (userid, strMessage) userMap = do
    let may_message :: Maybe Message
        may_message = decode' $ pack strMessage
    case may_message of
        Just message -> executeCommand userid userMap message
        Nothing -> error $ "Bad message: " ++ strMessage
-}

executeCommand :: UserID -> UserMap -> Message -> ChatIO ()
executeCommand userid userMap message = undefined

-- | Callback to handle new users.
newUserFunc :: UserID -> ChatIO ()
newUserFunc userid = userIDs %= (userid:)

-- | Parses a PortID from the command line arguments.
-- Only looks at the first argument.
parseCommandLineArguments :: IO Int
parseCommandLineArguments = fmap (read . head) getArgs

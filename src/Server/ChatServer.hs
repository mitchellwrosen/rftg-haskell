{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (mapM_)
import Control.Monad.State (liftIO, StateT(..))
import System.Environment (getArgs)

import Control.Monad (void)
import Control.Lens (makeLenses, use, (%=), (+=))
import Data.Aeson (decode')
import Data.ByteString.Lazy.Char8 (pack)
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
    startTcpServer handleFunc newUserFunc (ChatState [] M.empty) portNum

-- | Callback to handle incoming messages.
handleFunc :: (UserID, String) -> UserMap -> ChatIO ()
handleFunc (userid, strMessage) userMap = do
    let may_message :: Maybe Message
        may_message = decode' $ pack strMessage
    case may_message of
        Just message -> executeCommand userid userMap message
        Nothing -> error $ "Bad message: " ++ strMessage

sendMessageToAllClients :: UserMap -> String -> ChatIO ()
sendMessageToAllClients userMap msg =
    use userIDs >>=
    liftIO . mapM_ (\uid -> sendMessageToClient userMap uid msg)

executeCommand :: UserID -> UserMap -> Message -> ChatIO ()
executeCommand userid userMap (ChatMessage info) = do
    may_name <- fmap (M.lookup userid) (use userNames)
    case may_name of
        Just name -> sendMessageToAllClients userMap (name ++ ": " ++ message info)
        _ -> return () -- Send unknown user message
executeCommand userid _ (UserMessage info) =
    void $ fmap (M.insert userid (userName info)) (use userNames)

-- | Callback to handle new users.
newUserFunc :: UserID -> ChatIO ()
newUserFunc userid = userIDs %= (userid:)

-- | Parses a PortID from the command line arguments.
-- Only looks at the first argument.
parseCommandLineArguments :: IO Int
parseCommandLineArguments = fmap (read . head) getArgs

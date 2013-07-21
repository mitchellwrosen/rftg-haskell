{-# LANGUAGE OverloadedStrings #-}
module Client.Message where

import Control.Applicative ((<$>), (<*>))

import Data.Aeson
    (
      FromJSON
    , ToJSON
    , toJSON
    , parseJSON
    , Value(..)
    , (.:)
    , (.=)
    , decode'
    , object
    )

data UserListMessageData =
    UserListMessageData
    { userNames :: [String]
    }
instance FromJSON UserListMessageData where
    parseJSON (Object v) = UserListMessageData <$>
                           v .: "userNames"
instance ToJSON UserListMessageData where
    toJSON (UserListMessageData usernames) =
        object [ "userNames" .= usernames ]

data UserMessageData =
    UserMessageData
    { userName :: String
    }
instance FromJSON UserMessageData where
    parseJSON (Object v) = UserMessageData <$>
                           v .: "userName"
instance ToJSON UserMessageData where
    toJSON (UserMessageData username) =
        object [ "userName" .= username ]

data ChatMessageData =
    ChatMessageData
    { message  :: String
    }
instance FromJSON ChatMessageData where
    parseJSON (Object v) = ChatMessageData <$>
                           v .: "message"
instance ToJSON ChatMessageData where
    toJSON (ChatMessageData message) =
        object [ "message" .= message ]

data DisconnectMessageData =
    DisconnectMessageData
    { dUserName :: String
    }
instance FromJSON DisconnectMessageData where
    parseJSON (Object v) = DisconnectMessageData <$>
                           v .: "userName"
instance ToJSON DisconnectMessageData where
    toJSON (DisconnectMessageData userName) =
        object [ "userName" .= userName ]

data Message = ChatMessage ChatMessageData
             | UserMessage UserMessageData
             | UserListMessage UserListMessageData
             | DisconnectMessage DisconnectMessageData

instance FromJSON Message where
    parseJSON (Object v) = do
        messageType <- v .: "type"
        case (messageType :: String) of
            "user" -> UserMessage <$> v .: "data"
            "userlist" -> UserListMessage <$> v .: "data"
            "chat" -> ChatMessage <$> v .: "data"
            "disconnect" -> DisconnectMessage <$> v .: "data"
            _ -> error "unknown message type"

instance ToJSON Message where
    toJSON (ChatMessage dta) =
        object [ "type" .= String "chat"
               , "data" .= toJSON dta
               ]
    toJSON (UserMessage dta) =
        object [ "type" .= String "user"
               , "data" .= toJSON dta
               ]
    toJSON (UserListMessage dta) =
        object [ "type" .= String "userlist"
               , "data" .= toJSON dta
               ]
    toJSON (DisconnectMessage dta) =
        object [ "type" .= String "disconnect"
               , "data" .= toJSON dta
               ]

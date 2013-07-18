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

data JoinMessageData =
    JoinMessageData
    { joinRoom :: String
    }
instance FromJSON JoinMessageData where
    parseJSON (Object v) = JoinMessageData <$>
                           v .: "room"
instance ToJSON JoinMessageData where
    toJSON (JoinMessageData room) =
        object [ "room" .= room ]

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

data Message = ChatMessage ChatMessageData
             | JoinMessage JoinMessageData
             | UserMessage UserMessageData

instance FromJSON Message where
    parseJSON (Object v) = do
        messageType <- v .: "type"
        case (messageType :: String) of
            "user" -> UserMessage <$> v .: "data"
            "join" -> JoinMessage <$> v .: "data"
            "chat" -> ChatMessage <$> v .: "data"
            _ -> error "unknown message type"

instance ToJSON Message where
    toJSON (ChatMessage dta) =
        object [ "type" .= String "chat"
               , "data" .= toJSON dta
               ]
    toJSON (JoinMessage dta) =
        object [ "type" .= String "join"
               , "data" .= toJSON dta
               ]
    toJSON (UserMessage dta) =
        object [ "type" .= String "user"
               , "data" .= toJSON dta
               ]

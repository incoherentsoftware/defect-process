{-# LANGUAGE NoStrictData #-}

module Msg.Payload
    ( AudioMsgPayload
    , CollisionMsgPayload
    , ConsoleMsgPayload
    , EnemyMsgPayload
    , HurtMsgPayload
    , InfoMsgPayload
    , MenuMsgPayload
    , RoomMsgPayload
    , ParticleMsgPayload
    , PlayerMsgPayload
    , ProjectileMsgPayload
    , NewThinkProjectileMsgPayload
    , NewUpdateProjectileMsgPayload
    , UiMsgPayload
    , WorldMsgPayload
    , MsgPayload
    , IsMsgPayload(..)
    ) where

data AudioMsgPayload
data CollisionMsgPayload
data ConsoleMsgPayload
data EnemyMsgPayload
data HurtMsgPayload
data InfoMsgPayload
data MenuMsgPayload
data RoomMsgPayload
data ParticleMsgPayload
data PlayerMsgPayload
data ProjectileMsgPayload
data NewThinkProjectileMsgPayload
data NewUpdateProjectileMsgPayload
data UiMsgPayload
data WorldMsgPayload

data MsgPayload
    = AudioMsgPayload' AudioMsgPayload
    | CollisionMsgPayload' CollisionMsgPayload
    | ConsoleMsgPayload' ConsoleMsgPayload
    | EnemyMsgPayload' EnemyMsgPayload
    | HurtMsgPayload' HurtMsgPayload
    | InfoMsgPayload' InfoMsgPayload
    | MenuMsgPayload' MenuMsgPayload
    | RoomMsgPayload' RoomMsgPayload
    | ParticleMsgPayload' ParticleMsgPayload
    | PlayerMsgPayload' PlayerMsgPayload
    | ProjectileMsgPayload' ProjectileMsgPayload
    | NewThinkProjectileMsgPayload' NewThinkProjectileMsgPayload
    | NewUpdateProjectileMsgPayload' NewUpdateProjectileMsgPayload
    | UiMsgPayload' UiMsgPayload
    | WorldMsgPayload' WorldMsgPayload

class IsMsgPayload a where
    toMsgPayload   :: a -> MsgPayload
    fromMsgPayload :: MsgPayload -> Maybe a

instance IsMsgPayload AudioMsgPayload where
instance IsMsgPayload CollisionMsgPayload where
instance IsMsgPayload ConsoleMsgPayload where
instance IsMsgPayload EnemyMsgPayload where
instance IsMsgPayload HurtMsgPayload where
instance IsMsgPayload InfoMsgPayload where
instance IsMsgPayload MenuMsgPayload where
instance IsMsgPayload RoomMsgPayload where
instance IsMsgPayload ParticleMsgPayload where
instance IsMsgPayload PlayerMsgPayload where
instance IsMsgPayload ProjectileMsgPayload where
instance IsMsgPayload NewThinkProjectileMsgPayload where
instance IsMsgPayload NewUpdateProjectileMsgPayload where
instance IsMsgPayload UiMsgPayload where
instance IsMsgPayload WorldMsgPayload where

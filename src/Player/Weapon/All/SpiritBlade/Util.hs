module Player.Weapon.All.SpiritBlade.Util
    ( SpiritFormData(..)
    , attackToSpiritFormData
    , isSpiritFormAllInactive
    , readPlayerGroundBeneathOffsetY
    ) where

import qualified Data.List.NonEmpty as NE

import Attack
import Collision.Hitbox
import Configs.All.PlayerWeapon.SpiritBlade
import Id
import InfoMsg.Util
import Msg
import Player.Weapon.All.SpiritBlade.Data
import Util

playerGroundBeneathMaxOffsetY = 1000.0 :: OffsetY

data SpiritFormData = SpiritFormData
    { _appearAtkDesc          :: SpiritBladeAttackDescriptions -> AttackDescription
    , _atkDescs               :: [SpiritBladeAttackDescriptions -> AttackDescription]
    , _existingMsgId          :: MsgId
    , _updateSpiritFormMsgIds :: MsgId -> SpiritFormMsgIds -> SpiritFormMsgIds
    , _offset                 :: Direction -> OffsetX -> OffsetY -> Pos2
    }

attackToSpiritFormData :: SpiritBladeData -> Attack -> Maybe SpiritFormData
attackToSpiritFormData spiritBladeData atk
    -- double slash -> spirit large slash
    | atkDesc == _doubleSlash1 atkDescs = Just $ SpiritFormData
        { _appearAtkDesc          = _spiritLargeSlashAppear
        , _atkDescs               = [_spiritLargeSlash]
        , _existingMsgId          = _largeSlashId $ _spiritFormMsgIds spiritBladeData
        , _updateSpiritFormMsgIds = \msgId spiritFormMsgIds -> spiritFormMsgIds {_largeSlashId = msgId}
        , _offset                 = \dir _ _ -> vecFlip (_spiritLargeSlashOffset cfg) dir
        }

    -- spin slash -> spirit launch slash
    | atkDesc == _spinSlash atkDescs = Just $ SpiritFormData
        { _appearAtkDesc          = _spiritLaunchSlashAppear
        , _atkDescs               = [_spiritLaunchSlash]
        , _existingMsgId          = _launchSlashId $ _spiritFormMsgIds spiritBladeData
        , _updateSpiritFormMsgIds = \msgId spiritFormMsgIds -> spiritFormMsgIds {_launchSlashId = msgId}
        , _offset                 = \dir wallOffsetX _ ->
            let
                Pos2 offsetX offsetY = _spiritLaunchSlashOffset cfg
                offsetX'             = case dir of
                    LeftDir  -> minimum $ offsetX NE.:| [-wallOffsetX]
                    RightDir -> minimum $ offsetX NE.:| [wallOffsetX]
            in vecFlip (Pos2 offsetX' offsetY) dir
        }

    -- knockback slash -> spirit backwards slash
    | atkDesc == _knockbackSlash atkDescs = Just $ SpiritFormData
        { _appearAtkDesc          = _spiritBackwardsSlashAppear
        , _atkDescs               = [_spiritBackwardsSlash]
        , _existingMsgId          = _backwardsSlashId $ _spiritFormMsgIds spiritBladeData
        , _updateSpiritFormMsgIds = \msgId spiritFormMsgIds -> spiritFormMsgIds {_backwardsSlashId = msgId}
        , _offset                 = \dir wallOffsetX _ ->
            let
                Pos2 offsetX offsetY = _spiritBackwardsSlashOffset cfg
                offsetX'             = case dir of
                    LeftDir  -> minimum $ offsetX NE.:| [-wallOffsetX]
                    RightDir -> minimum $ offsetX NE.:| [wallOffsetX]
            in vecFlip (Pos2 offsetX' offsetY) dir
        }

    -- advancing slash -> spirit pillar blast
    | atkDesc == _advancingSlash atkDescs = Just $ SpiritFormData
        { _appearAtkDesc          = _spiritPillarBlastAppear
        , _atkDescs               = [_spiritPillarBlast]
        , _existingMsgId          = _pillarBlastId $ _spiritFormMsgIds spiritBladeData
        , _updateSpiritFormMsgIds = \msgId spiritFormMsgIds -> spiritFormMsgIds {_pillarBlastId = msgId}
        , _offset                 = \dir wallOffsetX _ ->
            let
                Pos2 offsetX offsetY = _spiritPillarBlastOffset cfg
                offsetX'             = case dir of
                    LeftDir  -> minimum $ offsetX NE.:| [-wallOffsetX]
                    RightDir -> minimum $ offsetX NE.:| [wallOffsetX]
            in vecFlip (Pos2 offsetX' offsetY) dir
        }

    -- air down thrust -> spirit air circular slash
    | atkDesc == _airDownThrust atkDescs = Just $ SpiritFormData
        { _appearAtkDesc          = _spiritAirCircularSlashAppear
        , _atkDescs               = [_spiritAirCircularSlash]
        , _existingMsgId          = _airCircularSlashId $ _spiritFormMsgIds spiritBladeData
        , _updateSpiritFormMsgIds = \msgId spiritFormMsgIds -> spiritFormMsgIds {_airCircularSlashId = msgId}
        , _offset                 = \dir _ groundBeneathOffsetY ->
            let
                Pos2 offsetX offsetY = _spiritAirCircularSlashOffset cfg
                offsetY'             = minimum $ offsetY NE.:| [groundBeneathOffsetY]
            in vecFlip (Pos2 offsetX offsetY') dir
        }

    -- air double slash -> spirit air blow
    | atkDesc == _airDoubleSlash1 atkDescs = Just $ SpiritFormData
        { _appearAtkDesc          = _spiritAirBlowAppear
        , _atkDescs               = [_spiritAirBlow]
        , _existingMsgId          = _airBlowId $ _spiritFormMsgIds spiritBladeData
        , _updateSpiritFormMsgIds = \msgId spiritFormMsgIds -> spiritFormMsgIds {_airBlowId = msgId}
        , _offset                 = \dir _ _ -> vecFlip (_spiritAirBlowOffset cfg) dir
        }

    | otherwise = Nothing

    where
        cfg      = _config (spiritBladeData :: SpiritBladeData)
        atkDesc  = _description atk
        atkDescs = _attackDescriptions spiritBladeData

isSpiritFormAllInactive :: SpiritBladeData -> Bool
isSpiritFormAllInactive spiritBladeData = and [idF spiritFormMsgIds == NullId | idF <- idFs]
    where
        spiritFormMsgIds = _spiritFormMsgIds spiritBladeData
        idFs             =
            [ _largeSlashId
            , _launchSlashId
            , _backwardsSlashId
            , _pillarBlastId
            , _airBlowId
            , _airCircularSlashId
            ]

readPlayerGroundBeneathOffsetY :: MsgsRead ThinkPlayerMsgsPhase m => m OffsetY
readPlayerGroundBeneathOffsetY = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> OffsetY
        processMsgs []     = playerGroundBeneathMaxOffsetY
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo ->
                let 
                    playerY        = hitboxBot $ _hitbox (playerInfo :: PlayerInfo)
                    groundBeneathY = vecY $ _groundBeneathPos playerInfo
                in abs $ groundBeneathY - playerY
            _                        -> processMsgs ps

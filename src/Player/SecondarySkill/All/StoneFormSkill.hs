module Player.SecondarySkill.All.StoneFormSkill
    ( mkStoneFormSkill
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (isJust)
import qualified Data.List as L
import qualified Data.Set as S

import Attack
import Attack.Hit
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.StoneForm
import Constants
import FileCache
import Id
import Msg
import Player
import Player.SecondarySkill as SS
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

phasedFrameTagName = FrameTagName "phased" :: FrameTagName

data DeflectType
    = MeleeDeflect
    | RangedDeflect

data StoneFormSkillAttackDescriptions = StoneFormSkillAttackDescriptions
    { _ground             :: AttackDescription
    , _groundBreak        :: AttackDescription
    , _groundShockwave    :: AttackDescription
    , _air                :: AttackDescription
    , _airBreak           :: AttackDescription
    , _airBreakCancelable :: AttackDescription
    , _airShockwave       :: AttackDescription
    }

mkStoneFormSkillAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m StoneFormSkillAttackDescriptions
mkStoneFormSkillAttackDescs =
    StoneFormSkillAttackDescriptions <$>
    loadAtkDesc "stone-form.atk" <*>
    loadAtkDesc "stone-break.atk" <*>
    loadAtkDesc "stone-shockwave.atk" <*>
    loadAtkDesc "stone-air-form.atk" <*>
    loadAtkDesc "stone-air-break.atk" <*>
    loadAtkDesc "stone-air-break-cancelable.atk" <*>
    loadAtkDesc "stone-air-shockwave.atk"
    where loadAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/player/player-skills.pack" f

data StoneFormSkillData = StoneFormSkillData
    { _storedAirVel       :: Maybe Vel2
    , _cooldown           :: Secs
    , _newDeflect         :: Maybe DeflectType
    , _deflectedHashedIds :: S.Set HashedId
    , _attackDescs        :: StoneFormSkillAttackDescriptions
    , _config             :: StoneFormConfig
    }

mkStoneFormSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m StoneFormSkillData
mkStoneFormSkillData = do
    stoneFormSkillAtkDescs <- mkStoneFormSkillAttackDescs
    cfg                    <- readConfig _playerSkill _stoneForm

    return $ StoneFormSkillData
        { _storedAirVel       = Nothing
        , _cooldown           = 0.0
        , _newDeflect         = Nothing
        , _deflectedHashedIds = S.empty
        , _attackDescs        = stoneFormSkillAtkDescs
        , _config             = cfg
        }

mkStoneFormSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some SecondarySkill)
mkStoneFormSkill = do
    stoneFormSkillData <- mkStoneFormSkillData
    return . Some $ (mkSecondarySkill stoneFormSkillData StoneFormSkill)
        { _think      = thinkStoneFormSkill
        , _update     = updateStoneFormSkill
        , _draw       = drawStoneFormSkill
        , _onCooldown = stoneFormSkillOnCooldown
        }

thinkStoneFormSkill :: InputRead m => SecondarySkillThink StoneFormSkillData m
thinkStoneFormSkill canUseSkill player slot stoneFormSkill =
    (meterMsgs ++) . (phasedMsgs ++) . think <$> readInputState
        where
            playerAtk = _attack player

            mkPlayerMsgSetAttackDesc = \atkDesc -> mkMsg $ PlayerMsgSetAttackDesc atkDesc
            attackIs'                = \atkDesc -> maybe False (`attackIs` atkDesc) playerAtk
            attackIn'                = \atkDescs -> maybe False (`attackIn` atkDescs) playerAtk

            flags                     = _flags player
            onGround                  = _touchingGround flags
            stoneFormSkillData        = _data stoneFormSkill
            cooldown                  = _cooldown stoneFormSkillData
            atks                      = _attackDescs stoneFormSkillData
            groundAtkDesc             = _ground atks
            groundShockwaveAtkDesc    = _groundShockwave atks
            airAtkDesc                = _air atks
            airBreakAtkDesc           = _airBreak atks
            airBreakCancelableAtkDesc = _airBreakCancelable atks
            airShockwaveAtkDesc       = _airShockwave atks
            pos                       = _pos (player :: Player)
            dir                       = _dir (player :: Player)
            atkWillBeDone             = maybe False (_done . updateAttack pos dir) playerAtk
            newDeflect                = _newDeflect stoneFormSkillData

            cfg               = _config (stoneFormSkillData :: StoneFormSkillData)
            stoneFormCooldown = _stoneFormCooldown cfg

            phasedMsgs = case playerAtk of
                Just atk
                    | phasedFrameTagName `isAttackFrameTag` atk -> [mkMsg PlayerMsgSetPhased]
                _                                               -> []

            meterMsgs = case newDeflect of
                Just MeleeDeflect  -> [mkMsg $ PlayerMsgGainMeter (_meleeDeflectMeterGain cfg)]
                Just RangedDeflect -> [mkMsg $ PlayerMsgGainMeter (_rangedDeflectMeterGain cfg)]
                Nothing            -> []

            think :: InputState -> [Msg ThinkPlayerMsgsPhase]
            think inputState
                | isJust newDeflect = if
                    | onGround  -> [mkPlayerMsgSetAttackDesc groundShockwaveAtkDesc]
                    | otherwise -> [mkPlayerMsgSetAttackDesc airShockwaveAtkDesc]

                | attackIs' groundAtkDesc && not onGround = [mkPlayerMsgSetAttackDesc airAtkDesc]
                | attackIs' airAtkDesc && onGround        = [mkPlayerMsgSetAttackDesc groundAtkDesc]

                | attackIn' [airBreakAtkDesc, airBreakCancelableAtkDesc] && atkWillBeDone =
                    case _storedAirVel stoneFormSkillData of
                        Nothing  -> []
                        Just vel -> [mkMsgEx (PlayerMsgSetVelocity vel) MsgEndOrder]

                | isSkillPressed && canUseSkill && cooldown <= 0.0 = if
                    | onGround ->
                        let
                            updateActive = \ss -> ss
                                { _data = (_data ss)
                                    { _storedAirVel = Nothing
                                    , _cooldown     = stoneFormCooldown
                                    }
                                }
                        in
                            [ mkPlayerMsgSetAttackDesc groundAtkDesc
                            , mkMsg $ PlayerMsgUpdateSecondarySkill slot updateActive
                            ]

                    | otherwise ->
                        let
                            vel = _vel (player :: Player)

                            updateActive = \ss -> ss
                                { _data = (_data ss)
                                    { _storedAirVel = Just vel
                                    , _cooldown     = stoneFormCooldown
                                    }
                                }
                        in
                            [ mkPlayerMsgSetAttackDesc airAtkDesc
                            , mkMsg $ PlayerMsgUpdateSecondarySkill slot updateActive
                            ]

                | otherwise = []

                where
                    isSkillPressed =
                        isSecondarySkillPressed inputState slot || isSecondarySkillPressedBuffer player slot

isAttackStoneForm :: Attack -> SecondarySkill StoneFormSkillData -> Bool
isAttackStoneForm atk stoneFormSkill = atk `attackIn` stoneFormAtkDescs
    where
        atkDescs          = _attackDescs $ _data stoneFormSkill
        stoneFormAtkDescs =
            [ _ground atkDescs
            , _groundBreak atkDescs
            , _air atkDescs
            , _airBreak atkDescs
            ]

updateStoneFormSkill :: MsgsRead UpdatePlayerMsgsPhase m => SecondarySkillUpdate StoneFormSkillData m
updateStoneFormSkill player stoneFormSkill = update <$> readMsgsTo (_msgId player)
    where
        processHurtMsg :: StoneFormSkillData -> HurtMsgPayload -> StoneFormSkillData
        processHurtMsg !stoneFormSkillData p = case p of
            HurtMsgAttackHit atkHit ->
                let
                    phased              = _phased $ _flags player
                    atkHashedId         = _hashedId atkHit
                    deflectedHashedIds  = _deflectedHashedIds stoneFormSkillData
                    atkNotPrevDeflected = atkHashedId `S.notMember` deflectedHashedIds
                in if
                    | phased && atkNotPrevDeflected -> stoneFormSkillData
                        { _newDeflect         = Just $ if
                            | _isRanged (atkHit :: AttackHit) -> RangedDeflect
                            | otherwise                       -> MeleeDeflect
                        , _deflectedHashedIds = atkHashedId `S.insert` deflectedHashedIds
                        }
                    | otherwise                     -> stoneFormSkillData

        update :: [HurtMsgPayload] -> SecondarySkill StoneFormSkillData
        update hurtMsgPayloads = case _attack player of
            Just atk
                | atk `isAttackStoneForm` stoneFormSkill ->
                    let stoneFormSkillData' = L.foldl' processHurtMsg stoneFormSkillData hurtMsgPayloads
                    in stoneFormSkill {SS._data = stoneFormSkillData'}

            _ ->
                let
                    cooldown            = max 0.0 (_cooldown stoneFormSkillData - timeStep)
                    stoneFormSkillData' = stoneFormSkillData
                        { _cooldown   = cooldown
                        , _newDeflect = Nothing
                        }
                in stoneFormSkill {_data = stoneFormSkillData'}

            where stoneFormSkillData = _data stoneFormSkill

drawStoneFormSkill :: (GraphicsReadWrite m, MonadIO m) => SecondarySkillDraw StoneFormSkillData m
drawStoneFormSkill player stoneFormSkill = case _attack player of
    Just atk
        | atk `isAttackStoneForm` stoneFormSkill ->
            let
                pos     = _pos (atk :: Attack)
                dir     = _dir (atk :: Attack)
                opacity = playerOpacity player
            in do
                pos' <- vecAdd pos <$> playerLerpOffset player
                drawSpriteEx pos' dir playerBodyZIndex 0.0 opacity NonScaled (attackSprite atk)

    _ -> return ()

stoneFormSkillOnCooldown :: SecondarySkillOnCooldown StoneFormSkillData
stoneFormSkillOnCooldown stoneFormSkill = _cooldown (_data stoneFormSkill) > 0.0

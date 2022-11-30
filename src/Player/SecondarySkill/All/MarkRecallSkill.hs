module Player.SecondarySkill.All.MarkRecallSkill
    ( mkMarkRecallSkill
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (isNothing)

import Attack
import Attack.Projectile
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.MarkRecall
import Constants
import FileCache
import Id
import Msg
import Particle.All.Simple
import Player
import Player.BufferedInputState
import Player.SecondarySkill as SS
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

packPath           = \f -> PackResourceFilePath "data/player/player-skills.pack" f
particleSpritePath = packPath "mark-recall-particle.spr"                  :: PackResourceFilePath
recallSoundPath    = "event:/SFX Events/Player/Skills/mark-recall-recall" :: FilePath

markerFrameTagName    = FrameTagName "marker" :: FrameTagName
recallPlayerAirOffset = Pos2 0.0 (-5.0)       :: Pos2

data MarkRecallSkillAttackDescriptions = MarkRecallSkillAttackDescriptions
    { _set    :: AttackDescription
    , _airSet :: AttackDescription
    , _marker :: AttackDescription
    }

mkMarkRecallSkillAttackDescriptions :: (FileCache m, GraphicsRead m, MonadIO m) => m MarkRecallSkillAttackDescriptions
mkMarkRecallSkillAttackDescriptions =
    MarkRecallSkillAttackDescriptions <$>
    loadAtkDesc "mark-recall-set.atk" <*>
    loadAtkDesc "mark-recall-air-set.atk" <*>
    loadAtkDesc "mark-recall-marker.atk"
    where loadAtkDesc = \f -> loadPackAttackDescription $ packPath f

data MarkRecallSkillData = MarkRecallSkillData
    { _cooldown     :: Secs
    , _markerProjId :: MsgId
    , _attackDescs  :: MarkRecallSkillAttackDescriptions
    , _config       :: MarkRecallConfig
    }

mkMarkRecallSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m MarkRecallSkillData
mkMarkRecallSkillData = do
    markerProjId <- newId
    atkDescs     <- mkMarkRecallSkillAttackDescriptions
    cfg          <- readConfig _playerSkill _markRecall

    return $ MarkRecallSkillData
        { _cooldown     = 0.0
        , _markerProjId = markerProjId
        , _attackDescs  = atkDescs
        , _config       = cfg
        }

mkMarkRecallSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some SecondarySkill)
mkMarkRecallSkill = do
    markRecallSkillData <- mkMarkRecallSkillData
    return . Some $ (mkSecondarySkill markRecallSkillData MarkRecallSkill)
        { _think      = thinkMarkRecallSkill
        , _update     = updateMarkRecallSkill
        , _onCooldown = markRecallSkillOnCooldown
        }

readMarkerPos :: (AllowMsgRead p InfoMsgPayload, MsgsRead p m) => SecondarySkill MarkRecallSkillData -> m (Maybe Pos2)
readMarkerPos markRecallSkill = processMsg <$> readMsgs
    where
        processMsg :: [InfoMsgPayload] -> Maybe Pos2
        processMsg []     = Nothing
        processMsg (d:ds) = case d of
            InfoMsgProjectilePos projPos _ projId
                | projId == _markerProjId (_data markRecallSkill) -> Just projPos
            _                                                     -> processMsg ds

setMarkRecallSkillCooldown :: SecondarySkill MarkRecallSkillData -> SecondarySkill MarkRecallSkillData
setMarkRecallSkillCooldown markRecallSkill = markRecallSkill {_data = markRecallSkillData {_cooldown = markerCooldown}}
    where
        markRecallSkillData = _data markRecallSkill
        markerCooldown      = _markerCooldown $ _config (markRecallSkillData :: MarkRecallSkillData)

thinkMarkRecallSkill :: (InputRead m, MsgsRead ThinkPlayerMsgsPhase m) => SecondarySkillThink MarkRecallSkillData m
thinkMarkRecallSkill canUseSkill player slot markRecallSkill = do
    inputState <- readInputState
    markerPos  <- readMarkerPos markRecallSkill

    let
        markRecallSkillData = _data markRecallSkill
        cooldown            = _cooldown markRecallSkillData
        isSkillPressed      =
            isSecondarySkillPressed inputState slot || isSecondarySkillPressedBuffer player slot
        atkDescs            = _attackDescs markRecallSkillData
        touchingGround      = _touchingGround (_flags (player :: Player) :: PlayerFlags)
        markerProjId        = _markerProjId markRecallSkillData
        playerPos           = _pos (player :: Player)
        cfg                 = _config (markRecallSkillData :: MarkRecallSkillData)

    return $ if
        | isNothing markerPos && isSkillPressed && canUseSkill && cooldown <= 0.0 ->
            let
                atkDescF
                    | touchingGround = _set
                    | otherwise      = _airSet
            in
                [ mkMsg $ PlayerMsgSetAttackDesc (atkDescF atkDescs)
                , mkMsg $ PlayerMsgUpdateSecondarySkill slot setMarkRecallSkillCooldown
                , mkMsg $ PlayerMsgClearInputBuffer allSecondarySkillBufferedInputs
                ]

        | Just markerPos' <- markerPos, isSkillPressed && cooldown <= 0.0 ->
            let
                playerOffset
                    | touchingGround = zeroPos2
                    | otherwise      = recallPlayerAirOffset
                recallPos            = markerPos' `vecAdd` _recallOffset cfg `vecAdd` playerOffset
                mkParticle           = loadSimpleParticle markerPos' RightDir worldEffectZIndex particleSpritePath
            in
                [ mkMsgTo (ProjectileMsgSetTtl 0.0) markerProjId
                , mkMsg $ PlayerMsgUpdateSecondarySkill slot setMarkRecallSkillCooldown
                , mkMsgEx (PlayerMsgSetPosition recallPos) MsgEndOrder
                , mkMsg PlayerMsgResetPrevHitbox
                , mkMsg PlayerMsgForceNewAttackId
                , mkMsg $ PlayerMsgClearInputBuffer allSecondarySkillBufferedInputs
                , mkMsg $ ParticleMsgAddM mkParticle
                , mkMsg $ AudioMsgPlaySound recallSoundPath recallPos
                , mkMsg $ WorldMsgHitlag (_recallHitlag cfg)
                ]

        | Just atk <- _attack player, atk `attackIs` _airSet atkDescs && touchingGround ->
            [mkMsgEx PlayerMsgClearAttack MsgFrontOrder]

        | Just atk <- _attack player, markerFrameTagName `isAttackFrameTag` atk, attackFrameChanged atk ->
            let
                pos           = playerPos `vecAdd` _markerOffset cfg
                markerAtkDesc = _marker atkDescs
                mkMarkerProj  = mkAttackProjectileWithMsgId pos RightDir markerAtkDesc [] markerProjId
            in [mkMsg $ NewThinkProjectileMsgAddM mkMarkerProj]

        | otherwise -> []

updateMarkRecallSkill :: MsgsRead UpdatePlayerMsgsPhase m => SecondarySkillUpdate MarkRecallSkillData m
updateMarkRecallSkill _ markRecallSkill = return $ markRecallSkill
    { _data = markRecallSkillData {_cooldown = cooldown}
    }
    where
        markRecallSkillData = _data markRecallSkill
        cooldown            = max 0.0 (_cooldown markRecallSkillData - timeStep)

markRecallSkillOnCooldown :: SecondarySkillOnCooldown MarkRecallSkillData
markRecallSkillOnCooldown = (> 0.0) . _cooldown . _data

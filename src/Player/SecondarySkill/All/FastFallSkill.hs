module Player.SecondarySkill.All.FastFallSkill
    ( mkFastFallSkill
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Attack.Projectile
import Collision
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.FastFall
import FileCache
import InfoMsg.Util
import Msg
import Particle.All.Simple
import Player
import Player.SecondarySkill as SS
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

fallFrameTagName = FrameTagName "fall" :: FrameTagName

fastFallParticlesPath =
    PackResourceFilePath "data/particles/particles.pack" "player-fast-fall.spr" :: PackResourceFilePath

data FastFallSkillAttackDescriptions = FastFallSkillAttackDescriptions
    { _fastFall   :: AttackDescription
    , _playerLand :: AttackDescription
    , _landSoft   :: AttackDescription
    , _landMedium :: AttackDescription
    , _landStrong :: AttackDescription
    }

mkFastFallSkillAttackDescriptions :: (FileCache m, GraphicsRead m, MonadIO m) => m FastFallSkillAttackDescriptions
mkFastFallSkillAttackDescriptions =
    FastFallSkillAttackDescriptions <$>
    loadPackAtkDesc "fast-fall.atk" <*>
    loadPackAtkDesc "fast-fall-player-land.atk" <*>
    loadPackAtkDesc "fast-fall-soft-effect.atk" <*>
    loadPackAtkDesc "fast-fall-medium-effect.atk" <*>
    loadPackAtkDesc "fast-fall-strong-effect.atk"
    where loadPackAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/player/player-skills.pack" f

data FastFallSkillData = FastFallSkillData
    { _used                 :: Bool
    , _fastFallVelY         :: VelY
    , _landEffectAttackDesc :: AttackDescription
    , _attackDescriptions   :: FastFallSkillAttackDescriptions
    , _configs              :: FastFallConfig
    }

mkFastFallSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m FastFallSkillData
mkFastFallSkillData = do
    atkDescs <- mkFastFallSkillAttackDescriptions
    cfgs     <- readConfig _playerSkill (_fastFall :: PlayerSkillConfig -> FastFallConfig)

    return $ FastFallSkillData
        { _used                 = False
        , _fastFallVelY         = _fastFallStrongVelY cfgs
        , _landEffectAttackDesc = _landSoft atkDescs
        , _attackDescriptions   = atkDescs
        , _configs              = cfgs
        }

mkFastFallSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some SecondarySkill)
mkFastFallSkill = do
    fastFallSkillData <- mkFastFallSkillData
    return . Some $ (mkSecondarySkill fastFallSkillData FastFallSkill)
        { _think      = thinkFastFallSkill
        , _update     = updateFastFallSkill
        , _onCooldown = fastFallSkillOnCooldown
        }

readPlayerGroundDistance :: MsgsRead ThinkPlayerMsgsPhase m => SecondarySkill FastFallSkillData -> m Distance
readPlayerGroundDistance fastFallSkill = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Distance
        processMsgs []     = _fastFallStrongGroundDistance $ _configs (_data fastFallSkill)
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo ->
                let
                    groundY = vecY $ _groundBeneathPos playerInfo
                    playerY = hitboxBot $ _hitbox playerInfo
                in groundY - playerY
            _                        -> processMsgs ps

activateFastFallSkill :: Distance -> SecondarySkill FastFallSkillData -> SecondarySkill FastFallSkillData
activateFastFallSkill groundDistance fastFallSkill = fastFallSkill
    { _data = fastFallSkillData
        { _used                 = True
        , _fastFallVelY         = fastFallVelYF cfgs
        , _landEffectAttackDesc = landEffectAtkDescF $ _attackDescriptions fastFallSkillData
        }
    }
    where
        fastFallSkillData = _data fastFallSkill
        cfgs              = _configs fastFallSkillData

        (landEffectAtkDescF, fastFallVelYF)
            | groundDistance < _fastFallSoftGroundDistance cfgs   = (_landSoft, _fastFallSoftVelY)
            | groundDistance < _fastFallMediumGroundDistance cfgs = (_landMedium, _fastFallMediumVelY)
            | otherwise                                           = (_landStrong, _fastFallStrongVelY)

thinkFastFallSkill :: (InputRead m, MsgsRead ThinkPlayerMsgsPhase m) => SecondarySkillThink FastFallSkillData m
thinkFastFallSkill canUseSkill player slot fastFallSkill = think =<< readInputState
    where
        think :: MsgsRead ThinkPlayerMsgsPhase m1 => InputState -> m1 [Msg ThinkPlayerMsgsPhase]
        think inputState
            | not used && not touchingGround && canUseSkill && isSkillPressed = do
                groundDistance <- readPlayerGroundDistance fastFallSkill
                return
                    [ mkMsg $ PlayerMsgSetAttackDesc fastFallAtkDesc
                    , mkMsg $ PlayerMsgUpdateSecondarySkill slot (activateFastFallSkill groundDistance)
                    , mkMsg PlayerMsgResetRisingJump
                    ]

            | otherwise =
                let
                    isAtkFrameTagChanged = \atk -> fallFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk
                    updateVel            = \(Vel2 velX _) -> Vel2 velX (_fastFallVelY fastFallSkillData)
                    pos                  = _pos (player :: Player)
                    dir                  = _dir (player :: Player)
                in return $ case _attack player of
                    Just atk
                        | atk `attackIs` fastFallAtkDesc && isAtkFrameTagChanged atk ->
                            let mkParticles = loadSimpleParticle pos dir playerOverBodyZIndex fastFallParticlesPath
                            in
                                [ mkMsgEx (PlayerMsgUpdateVelocity updateVel) MsgEndOrder
                                , mkMsg $ ParticleMsgAddM mkParticles
                                ]

                        | atk `attackIs` fastFallAtkDesc && touchingGround ->
                            let
                                mkLandEffectAttackProj =
                                    mkPlayerAttackProjectile pos dir (_landEffectAttackDesc fastFallSkillData)
                            in
                                [ mkMsg $ PlayerMsgSetAttackDesc (_playerLand atkDescs)
                                , mkMsg $ NewThinkProjectileMsgAddM mkLandEffectAttackProj
                                ]

                    _ -> []

            where
                fastFallSkillData = _data fastFallSkill
                used              = _used fastFallSkillData
                atkDescs          = _attackDescriptions fastFallSkillData
                fastFallAtkDesc   = _fastFall (atkDescs :: FastFallSkillAttackDescriptions)
                touchingGround    = _touchingGround (_flags player :: PlayerFlags)
                isSkillPressed    =
                    isSecondarySkillPressed inputState slot || isSecondarySkillPressedBuffer player slot

updateFastFallSkill :: ConfigsRead m => SecondarySkillUpdate FastFallSkillData m
updateFastFallSkill player fastFallSkill =
    let
        touchingGround    = _touchingGround (_flags player :: PlayerFlags)
        velY              = vecY $ _vel (player :: Player)
        fastFallSkillData = _data fastFallSkill
        used              = _used fastFallSkillData
        used'             = if
            | touchingGround && velY >= 0.0 && used -> False
            | otherwise                             -> used
    in do
        cfgs <- readConfig _playerSkill (_fastFall :: PlayerSkillConfig -> FastFallConfig)

        return $ fastFallSkill
            { _data = fastFallSkillData
                { _used    = used'
                , _configs = cfgs
                }
            }

fastFallSkillOnCooldown :: SecondarySkillOnCooldown FastFallSkillData
fastFallSkillOnCooldown = _used . _data

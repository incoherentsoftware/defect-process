module Enemy.All.Hammer.AI
    ( thinkAI
    ) where

import Control.Monad.State (execState, modify)

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Hammer
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.All.Hammer.AI.Run
import Enemy.All.Hammer.AttackDescriptions
import Enemy.All.Hammer.Behavior
import Enemy.All.Hammer.Data
import Msg
import Util
import Window.Graphics

reFormSoundPath = "event:/SFX Events/Enemy/Hammer/re-form" :: FilePath

moveFrameTagName  = FrameTagName "move"  :: FrameTagName
soundFrameTagName = FrameTagName "sound" :: FrameTagName

thinkAI :: ConfigsRead m => EnemyThinkAI HammerEnemyData m
thinkAI enemy = do
    aiEnabled <- not <$> readSettingsConfig _debug _disableAI

    return . flip execState [] $ do
        modify (++ mkGravityMsg enemy)

        let
            runBehaviorInstr' = \cmd -> runBehaviorInstr aiEnabled cmd enemy
            behaviorInstrs    = thinkBehaviorInstrs enemy
        modify (++ concatMap runBehaviorInstr' behaviorInstrs)

        modify (++ mkEnemyUpdatePrevBehaviorMsg enemy)
        modify (++ mkAdditionalAudioMsgs enemy)

mkGravityMsg :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkGravityMsg enemy = case _behavior (_data enemy) of
    HurtBehavior _ hurtType
        | hurtType `elem` [LaunchUpHurt, FallenHurt, KnockDownHurt] -> gravityMsg
    LaunchedBehavior _ NotInHangtime                                -> gravityMsg
    FallenBehavior _                                                -> gravityMsg
    _                                                               -> []
    where
        gravity    = _gravity $ _config (_data enemy)
        gravityVel = Vel2 0.0 (gravity * timeStep)
        enemyId    = _msgId enemy
        gravityMsg = [mkMsgToEx (EnemyMsgUpdateVelocity $ vecAdd gravityVel) enemyId MsgEndOrder]

mkEnemyUpdatePrevBehaviorMsg :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdatePrevBehaviorMsg enemy = mkEnemyUpdateMsg enemy $ \e -> e
    { _data = (_data e) {_prevBehavior = behavior}
    }
    where behavior = _behavior $ _data enemy

mkAdditionalAudioMsgs :: Enemy HammerEnemyData -> [Msg ThinkEnemyMsgsPhase]
mkAdditionalAudioMsgs enemy = case E._sprite enemy of
    Just spr
        | soundFrameTagName `isSpriteFrameTag` spr && _frameChanged spr ->
            [mkMsg $ AudioMsgPlaySound reFormSoundPath (E._pos enemy)]
    _                                                                   -> []

thinkTeleportBehaviorInstrs :: Enemy HammerEnemyData -> [HammerEnemyBehaviorInstr]
thinkTeleportBehaviorInstrs enemy = case E._sprite enemy of
    Just spr
        | moveFrameTagName `isSpriteFrameTag` spr && _frameChanged spr -> [UpdateTeleportInstr]
        | spriteFinished spr                                           -> [StartPatrolInstr]
        | otherwise                                                    -> []

    Nothing -> [StartTeleportInstr]

thinkAttackBehaviorInstrs :: Enemy HammerEnemyData -> [HammerEnemyBehaviorInstr]
thinkAttackBehaviorInstrs enemy
    | isMeteorAtk = if
        | onGround  -> [StartAttackLandInstr]
        | otherwise -> []
    | atkDone     = [StartIdleInstr]
    | otherwise   = []
    where
        atk           = _attack enemy
        atkMeteorDesc = _meteor $ _attackDescs (_data enemy)
        isMeteorAtk   = maybe False ((== atkMeteorDesc) . _description) atk
        onGround      = enemyTouchingGround enemy
        atkDone       = maybe True _done atk

thinkHurtBehaviorInstrs :: Secs -> HurtType -> Enemy HammerEnemyData -> [HammerEnemyBehaviorInstr]
thinkHurtBehaviorInstrs hurtTtl hurtType enemy = case hurtType of
    AirHurt
        | sprFinished -> [StartIdleInstr]
    FallenHurt
        | sprFinished -> [StartFallenInstr hurtTtl']

    WallHurt
        | sprFinished && hurtTtl <= 0.0 -> [StartLaunchedInstr 0.0]
        | otherwise                     -> [UpdateHurtInstr hurtTtl hurtType]

    -- let enemy hit ground
    KnockDownHurt -> []

    _
        | inAir && touchingWall && abs velX >= minWallSplatImpactSpeed -> [StartWallSplatInstr]
        | inAir && inHangtimeVel && hurtType == LaunchUpHurt           -> [StartLaunchedInstr minHangtimeSecs]
        | sprFinished && hurtTtl <= 0.0                                -> case hurtType of
            AirHurt      -> [StartIdleInstr]
            LaunchedHurt -> [StartLaunchedInstr 0.0]
            _            -> [StartLaunchedInstr minHangtimeSecs]
        | otherwise                                                    -> [UpdateHurtInstr hurtTtl hurtType]

    where
        inAir                   = not $ enemyTouchingGround enemy
        touchingWall            = enemyTouchingWall enemy
        velX                    = vecX $ E._vel enemy
        sprFinished             = enemySpriteFinished enemy
        hurtTtl'                = hurtTtl - timeStep
        cfg                     = _config $ E._data enemy
        minWallSplatImpactSpeed = _minWallSplatImpactSpeed cfg
        minHangtimeSecs         = _minHangtimeSecs cfg
        inHangtimeVel           = enemyInHangtimeVel enemy cfg

thinkBehaviorInstrs :: Enemy HammerEnemyData -> [HammerEnemyBehaviorInstr]
thinkBehaviorInstrs enemy = case _behavior enemyData of
    behavior
        | canAttackSwingPlayer enemy                       -> [StartAttackInstr atkSwingDesc]
        | canAttackMeteorPlayer enemy                      -> [StartAttackInstr atkMeteorDesc]
        | isHealthZero health && behavior /= DeathBehavior -> [StartDeathInstr]

    IdleBehavior idleTtl
        | idleTtl > 0.0 -> [UpdateIdleInstr idleTtl]
        | otherwise     -> [StartPatrolInstr]

    PatrolBehavior
        | willUseTeleport -> [StartTeleportInstr]
        | otherwise       -> [UpdatePatrolInstr]

    ReFormBehavior
        | sprFinished -> [StartIdleInstr]

    AttackLandBehavior
        | atkSprFinished -> [CreateAttackLandParticlesInstr, StartReFormInstr]

    TeleportBehavior              -> thinkTeleportBehaviorInstrs enemy
    AttackBehavior                -> thinkAttackBehaviorInstrs enemy
    HurtBehavior hurtTtl hurtType -> thinkHurtBehaviorInstrs hurtTtl hurtType enemy

    LaunchedBehavior hangtimeTtl _
        | hangtimeTtl > 0.0 && inHangtimeVel -> [LaunchedInHangtimeInstr hangtimeTtl]
        | otherwise                          -> [LaunchedNotInHangtimeInstr hangtimeTtl]

    FallenBehavior fallenTtl
        | fallenTtl > 0.0 -> [UpdateFallenInstr fallenTtl]
        | otherwise       -> [StartSitUpInstr]

    SitUpBehavior
        | sprFinished -> [StartReFormInstr]

    WallSplatBehavior wallSplatTtl
        | wallSplatTtl > 0.0 -> [UpdateWallSplatInstr wallSplatTtl]
        | otherwise          -> [StartLaunchedInstr 0.0]

    SpawnBehavior
        | sprFinished -> [StartIdleInstr]
        | otherwise   -> [UpdateSpawnInstr]

    DeathBehavior
        | sprFinished -> [SetDeadInstr]

    _ -> []

    where
        health          = E._health enemy
        enemyData       = E._data enemy
        sprFinished     = enemySpriteFinished enemy
        cfg             = _config enemyData
        inHangtimeVel   = enemyInHangtimeVel enemy cfg
        willUseTeleport = _willUseTeleport enemyData
        atkSprFinished  = maybe False spriteFinished (enemyAttackSprite enemy)

        atkDescs      = _attackDescs enemyData
        atkSwingDesc  = _swing atkDescs
        atkMeteorDesc = _meteor atkDescs

canAttackPlayerAtDistanceX :: Distance -> Enemy HammerEnemyData -> Bool
canAttackPlayerAtDistanceX atkDistanceX enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 playerX playerY)
        | canAttackFromBehavior ->
            let
                Pos2 x y    = E._pos enemy
                distPlayerX = abs $ playerX - x
            in distPlayerX <= atkDistanceX && playerY >= y
    _                           -> False
    where
        canAttackFromBehavior = case _behavior (_data enemy) of
            PatrolBehavior -> True
            _              -> False

canAttackMeteorPlayer :: Enemy HammerEnemyData -> Bool
canAttackMeteorPlayer enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 _ playerY)
        | playerY >= y -> canAttackPlayerAtDistanceX attackMeteorRangeX enemy
    _                  -> False
    where
        y                  = vecY $ E._pos enemy
        attackMeteorRangeX = _attackMeteorRangeX . _hammer . _config $ _data enemy

canAttackSwingPlayer :: Enemy HammerEnemyData -> Bool
canAttackSwingPlayer enemy = case enemyKnownPlayerPos enemy of
    Just (Pos2 _ playerY)
        | abs (playerY - y) <= attackSwingRangeY -> canAttackPlayerAtDistanceX attackSwingRangeX enemy
    _                                            -> False
    where
        y                 = vecY $ E._pos enemy
        hammerCfg         = _hammer $ _config (_data enemy)
        attackSwingRangeX = _attackSwingRangeX hammerCfg
        attackSwingRangeY = _attackSwingRangeY hammerCfg

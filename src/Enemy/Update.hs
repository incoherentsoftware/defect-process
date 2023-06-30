module Enemy.Update
    ( updateEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (get, gets, execStateT, lift, modify, put)
import Data.Dynamic           (toDyn)
import Data.Foldable          (foldlM, for_)
import qualified Data.List as L
import qualified Data.Set as S

import AppEnv
import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import Enemy.DebugText
import FileCache
import Id
import Msg
import Particle.All.Simple
import Util
import World.Surface
import World.ZIndex

roofOffsetY            = 1.0  :: Float
maxOnPlatformDistanceY = 25.0 :: PosY

stasisHitEffectPath =
    PackResourceFilePath "data/player/player-skills.pack" "stasis-blast-hit-effect.spr" :: PackResourceFilePath
tauntedSoundPath    = "event:/SFX Events/Enemy/taunted"                                 :: FilePath

updateEnemyMessages :: Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
updateEnemyMessages enemy =
    processAiMessages enemy >>=
    processCollisionMessages >>=
    processEnemyMessages >>=
    processHurtMessages >>=
    processTauntMessages

updateEnemyByWall :: PosX -> WallSurfaceType -> Enemy d -> Enemy d
updateEnemyByWall wallX wallType enemy = enemy
    { _pos   = Pos2 (x + xOffset) y
    , _flags = (_flags enemy)
        { _touchingLeftWall  = wallType == LeftWallSurface
        , _touchingRightWall = wallType == RightWallSurface
        }
    }
    where
        Pos2 x y = E._pos enemy
        hbx      = enemyHitbox enemy
        hbxLeft  = hitboxLeft hbx
        hbxRight = hitboxRight hbx
        xOffset  = case wallType of
            LeftWallSurface
                | hbxLeft < wallX  -> wallX - hbxLeft
                | otherwise        -> 0.0
            RightWallSurface
                | hbxRight > wallX -> wallX - hbxRight
                | otherwise        -> 0.0

updateEnemyByRoof :: PosY -> Enemy d -> Enemy d
updateEnemyByRoof roofY enemy = enemy
    { _pos = Pos2 x (roofY + hitboxHeight hitbox + roofOffsetY)
    , _vel = Vel2 velX (if velY < 0.0 then 0.0 else velY)
    }
    where
        x              = vecX $ E._pos enemy
        Vel2 velX velY = E._vel enemy
        hitbox         = enemyHitbox enemy

updateEnemyWillFallOffGround :: Enemy d -> Enemy d
updateEnemyWillFallOffGround enemy = enemy
    { _flags = (_flags enemy) {_willFallOffGround = True}
    }

updateEnemyPosVel :: Enemy d -> Enemy d
updateEnemyPosVel enemy = enemy
    { _pos           = pos
    , _vel           = vel'
    , _launchTargetY = launchTargetY'
    }
    where
        vel            = E._vel enemy
        pos@(Pos2 _ y) = E._pos enemy `vecAdd` toPos2 (vel `vecMul` timeStep)
        launchTargetY  = E._launchTargetY enemy
        vel'           = case launchTargetY of
            Just minY
                | y <= minY -> zeroVel2
            _               -> vel

        launchTargetY'
            | vecY vel' >= 0.0 = Nothing
            | otherwise        = launchTargetY

updateEnemyAttack :: Enemy d -> Enemy d
updateEnemyAttack enemy = enemy
    { _attack = attack
    , _vel    = vel'
    }
    where
        pos            = E._pos enemy
        vel            = E._vel enemy
        dir            = E._dir enemy
        (attack, vel') = case _attack enemy of
            Nothing  -> (Nothing, vel)
            Just atk ->
                let
                    atk'   = updateAttack pos dir atk
                    atkVel = attackVel atk'
                in (Just atk', attackVelToVel2 atkVel vel)

updateEnemyDebug :: ConfigsRead m => Enemy d -> m (Enemy d)
updateEnemyDebug enemy = do
    debugCfg <- readConfig _settings _debug
    return $ enemy
        { _debugText   = updateEnemyDebugText (_health enemy) <$> _debugText enemy
        , _debugConfig = debugCfg
        }

updateEnemyInStasis :: Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
updateEnemyInStasis enemy = flip execStateT enemy $ do
    modify $ \e -> e
        { _flags      = clearEnemyFlagsInStasis $ _flags e
        , _stasisData = updateEnemyStasisData $ _stasisData e
        }
    get >>= lift . updateEnemyMessages >>= put
    get >>= lift . updateEnemyDebug >>= put

updateEnemy :: Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
updateEnemy enemy
    | isEnemyInStasis enemy = updateEnemyInStasis enemy
    | otherwise             = flip execStateT enemy $ do
        modify $ \e -> e
            { _flags       = clearEnemyFlags $ _flags e
            , _tauntedData = updateEnemyTauntedData <$> _tauntedData e
            , _stasisData  = updateEnemyStasisData $ _stasisData e
            }
        get >>= lift . updateEnemyMessages >>= put
        modify updateEnemyAttack
        modify updateEnemyPosVel
        modify $ \e -> (_updateSprite e) e
        get >>= lift . updateEnemyDebug >>= put

processAiMessages :: MsgsRead UpdateEnemyMsgsPhase m => Enemy d -> m (Enemy d)
processAiMessages enemy = L.foldl' processMsg enemy <$> readMsgsTo (_msgId enemy)
    where
        processMsg :: Enemy d -> InfoMsgPayload -> Enemy d
        processMsg e d = case d of
            InfoMsgSeenPlayer playerInfo -> e {_knownPlayerInfo = playerInfo}
            _                            -> e

processCollisionMessages :: Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
processCollisionMessages enemy = foldlM processMsg enemy =<< readMsgsTo (_msgId enemy)
    where
        updateEnemyGroundResponse :: PosY -> SurfaceType -> Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
        updateEnemyGroundResponse groundY surfaceType e = case surfaceType of
            GeneralSurface     -> groundResponse
            PlatformSurface
                | onPlatform   -> groundResponse
                | otherwise    -> return e
            SpeedRailSurface _ -> groundResponse
            where
                y              = vecY $ E._pos e
                onPlatform     = abs (groundY - y) <= maxOnPlatformDistanceY
                groundResponse = (_updateGroundResponse e) groundY e

        processMsg :: Enemy d -> CollisionMsgPayload -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
        processMsg e d = case d of
            CollisionMsgTouchingGround groundY surfaceType -> updateEnemyGroundResponse groundY surfaceType e
            CollisionMsgTouchingWall groundX _ wallType    -> return $ updateEnemyByWall groundX wallType e
            CollisionMsgTouchingRoof roofY                 -> return $ updateEnemyByRoof roofY e
            CollisionMsgWillFallOffGround                  -> return $ updateEnemyWillFallOffGround e
            CollisionMsgMovingPlatform _ _                 -> return e
            CollisionMsgWallProximity _ _                  -> return e

setEnemyAttack :: (ConfigsRead m, MonadIO m) => AttackDescription -> Enemy d -> m (Enemy d)
setEnemyAttack atkDesc enemy =
    let
        pos = E._pos enemy
        dir = E._dir enemy
    in do
        atk <- mkEnemyAttack pos dir atkDesc (enemyTauntedStatus enemy)
        return $ enemy {_attack = Just atk}

setEnemyStasis :: MsgsWrite UpdateEnemyMsgsPhase m => Secs -> Attack -> Enemy d -> m (Enemy d)
setEnemyStasis stasisSecs stasisAtk enemy
    | atkHashedId `S.member` hitByHashedIds = return enemy
    | otherwise                             =
        let
            pos              = hitboxCenter $ enemyHitbox enemy
            dir              = _dir (stasisAtk :: Attack)
            mkEffectParticle = loadSimpleParticle pos dir enemyHurtParticleZIndex stasisHitEffectPath
            mkEffectMsg      = mkMsg $ ParticleMsgAddM mkEffectParticle
        in do
            writeMsgs $ mkEffectMsg:attackHitlagMessages stasisAtk ++ attackHitSoundMessages pos stasisAtk

            return $ enemy
                { _hitByHashedIds = atkHashedId `S.insert` hitByHashedIds
                , _stasisData     = (_stasisData enemy) {_stasisTtl = stasisSecs}
                }
    where
        atkHashedId    = hashId $ _id stasisAtk
        hitByHashedIds = _hitByHashedIds enemy

processEnemyMessages :: Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
processEnemyMessages enemy = foldlM processMsg enemy =<< readMsgsTo (_msgId enemy)
    where
        processMsg :: Enemy d -> EnemyMsgPayload -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
        processMsg !e d = case d of
            EnemyMsgUpdate update                  -> updateDynamic $ toDyn update
            EnemyMsgUpdateM update                 -> updateDynamic $ toDyn update
            EnemyMsgSetVelocity vel                -> return $ e {E._vel = vel}
            EnemyMsgUpdateVelocity update          -> return $ e {E._vel = update $ E._vel e}
            EnemyMsgSetDirection dir'              -> return $ e {E._dir = dir'}
            EnemyMsgClearAttack                    -> return $ e {_attack = Nothing}
            EnemyMsgFinishAttack                   -> return $ e {_attack = finishAttack <$> _attack e}
            EnemyMsgSetAttackDesc atkDesc          -> setEnemyAttack atkDesc e
            EnemyMsgSetAttackDescM atkDesc         -> flip setEnemyAttack e =<< atkDesc
            EnemyMsgSetDead                        -> return $ e {_flags = (_flags e) {_dead = True}}
            EnemyMsgSetHangtime hangtimeSecs       -> return $ updateHangtimeResponse hangtimeSecs e
            EnemyMsgSetStasis stasisSecs stasisAtk -> setEnemyStasis stasisSecs stasisAtk e
            EnemyMsgAddM _                         -> return e
            EnemyMsgAdds _                         -> return e
            EnemyMsgAddsM _                        -> return e
            where
                updateDynamic          = \dyn -> (_updateDynamic e) dyn e
                updateHangtimeResponse = _updateHangtimeResponse e

hurtEnemy :: AttackHit -> Bool -> Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
hurtEnemy atkHit allowDuplicateHit enemy = do
    damageMultiplier <- readSettingsConfig _debug _enemiesDamageMultiplier

    let
        Damage damageVal = _damage (atkHit :: AttackHit)
        damageVal'       = ceiling $ fromIntegral damageVal * damageMultiplier
        atkHit'          = atkHit {_damage = Damage damageVal'} :: AttackHit
        atkHashedId      = _hashedId atkHit'
        hitByHashedIds   = _hitByHashedIds enemy

    if
        -- ignore id checking for deferred attack hits since they've technically already hit the enemy
        | not allowDuplicateHit && atkHashedId `S.member` hitByHashedIds -> return enemy

        | isEnemyInStasis enemy -> return $ enemy
            { _hitByHashedIds     = atkHashedId `S.insert` hitByHashedIds
            , _stasisData         = (_stasisData enemy)
                { _stasisTtl          = 0.0
                , _deferredAttackHits = atkHit:_deferredAttackHits (_stasisData enemy)
                }
            }

        | otherwise -> do
            enemy' <- (_updateHurtResponse enemy) atkHit' enemy
            return $ enemy'
                { _hitByHashedIds = atkHashedId `S.insert` hitByHashedIds
                , _stasisData     = (_stasisData enemy') {_stasisTtl = 0.0}
                }

processHurtMessages :: Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
processHurtMessages enemy =
    let
        processMsg :: Enemy d -> HurtMsgPayload -> AppEnv UpdateEnemyMsgsPhase (Enemy d)
        processMsg e p = case p of
            HurtMsgAttackHit atkHit -> hurtEnemy atkHit False e
    in flip execStateT enemy $ do
        msgs <- lift $ readMsgsTo (_msgId enemy)
        get >>= \e -> lift (foldlM processMsg e msgs) >>= put

        unless (isEnemyInStasis enemy) $ do
            deferredAtkHits <- gets $ _deferredAttackHits . _stasisData
            for_ deferredAtkHits $ \atkHit ->
                get >>= lift . hurtEnemy atkHit True >>= put
            modify $ \e -> e
                { _stasisData = (_stasisData e) {_deferredAttackHits = []}
                }

processTauntMessages :: forall m d. MsgsReadWrite UpdateEnemyMsgsPhase m => Enemy d -> m (Enemy d)
processTauntMessages enemy = processMsgs =<< readMsgs
    where
        processMsgs :: [PlayerMsgPayload] -> m (Enemy d)
        processMsgs []     = return enemy
        processMsgs (p:ps) = case p of
            PlayerMsgActivateTaunt -> case _tauntedData enemy of
                Nothing          -> return enemy
                Just tauntedData -> do
                    when (_status tauntedData == EnemyTauntedInactive) $
                        writeMsgs [mkMsg $ AudioMsgPlaySound tauntedSoundPath (E._pos enemy)]
                    return $ enemy {_tauntedData = Just (activateEnemyTauntedData tauntedData)}

            _ -> processMsgs ps

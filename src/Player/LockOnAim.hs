module Player.LockOnAim
    ( module Player.LockOnAim.Types
    , mkPlayerLockOnAim
    , thinkPlayerLockOnAim
    , updatePlayerLockOnAim
    , drawPlayerLockOnAim
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, execStateT, lift, put, modify)
import Data.Functor           ((<&>))
import Data.Traversable       (for)
import Data.Maybe             (fromMaybe, listToMaybe)
import qualified Data.List as L
import qualified Data.Set as S
import qualified SDL.Raw

import Attack.Util
import Collision.Hitbox
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy.Util
import FileCache
import Msg
import Player.BufferedInputState
import Player.LockOnAim.Types
import Player.Types
import Player.Util
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex
import {-# SOURCE #-} Player

lockOnAxisThresholdSq = 0.5 ** 2.0     :: Float
lockOnAngleThreshold  = toRadians 10.0 :: Radians
lockOnReticleOpacity  = Opacity 0.85   :: Opacity

lockOnReticleSprPathSuffixes = map pure ['a'..'p'] :: [FilePath]

mkPlayerLockOnAim :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerLockOnAim
mkPlayerLockOnAim = do
    reticleSprs <- for (reverse lockOnReticleSprPathSuffixes) $ \suffix ->
        loadPackSprite $ PackResourceFilePath "data/ui/ui.pack" ("lock-on-reticle-" ++ suffix ++ ".spr")

    return $ PlayerLockOnAim
        { _enemyLockOn    = Nothing
        , _reticleSprites = reticleSprs
        , _manualOverride = False
        }

isSwitchTargetInput :: InputRead m => Player -> m Bool
isSwitchTargetInput player = readInputState <&> \inputState ->
    LockOnSwitchTargetAlias `aliasPressed` inputState || LockOnSwitchTargetInput `inPlayerInputBuffer` player

isLockOnCursorInput :: InputRead m => Player -> m Bool
isLockOnCursorInput player = readInputState <&> \inputState ->
    LockOnCursorAlias `aliasPressed` inputState || LockOnCursorInput `inPlayerInputBuffer` player

isLockOnClearInput :: InputRead m => Player -> m Bool
isLockOnClearInput player = readInputState <&> \inputState ->
    LockOnClearAlias `aliasPressed` inputState || LockOnClearInput `inPlayerInputBuffer` player

thinkPlayerLockOnAim :: InputRead m => Player -> PlayerLockOnAim -> m [Msg ThinkPlayerMsgsPhase]
thinkPlayerLockOnAim player _ =
    let mkMsgClearInputBuffer = \input -> mkMsg $ PlayerMsgClearInputBuffer (S.singleton input)
    in flip execStateT [] $ do
        whenM (lift (isSwitchTargetInput player)) $
            modify ((mkMsgClearInputBuffer LockOnSwitchTargetInput):)
        whenM (lift (isLockOnCursorInput player)) $
            modify ((mkMsgClearInputBuffer LockOnCursorInput):)
        whenM (lift (isLockOnClearInput player)) $
            modify ((mkMsgClearInputBuffer LockOnClearInput):)

clearPlayerLockOnAim :: PlayerLockOnAim -> PlayerLockOnAim
clearPlayerLockOnAim lockOnAim = lockOnAim {_enemyLockOn = Nothing}

axisVec :: InputState -> Vec2
axisVec inputState = Vec2 xAxis yAxis
    where
        xAxis = gamepadAxis SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX inputState
        yAxis = gamepadAxis SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY inputState

axisThresholdSq :: InputState -> Float
axisThresholdSq inputState = xAxis ** 2 + yAxis ** 2
    where Vec2 xAxis yAxis = axisVec inputState

readMsgsEnemyLockOnData :: MsgsRead UpdatePlayerMsgsPhase m => m [EnemyLockOnData]
readMsgsEnemyLockOnData = L.foldl' processMsg [] <$> readMsgs
    where
        processMsg :: [EnemyLockOnData] -> InfoMsgPayload -> [EnemyLockOnData]
        processMsg lockOnDatas d = case d of
            InfoMsgEnemyLockOnReticle lockOnData -> lockOnData:lockOnDatas
            _                                    -> lockOnDatas

sortEnemyLockOnDataByDist :: Pos2 -> [EnemyLockOnData] -> [EnemyLockOnData]
sortEnemyLockOnDataByDist playerPos lockOnDatas = L.sortBy cmp lockOnDatas
    where
        cmp :: EnemyLockOnData -> EnemyLockOnData -> Ordering
        cmp lockOnData1 lockOnData2 = compare (playerDistSq enemyPos1) (playerDistSq enemyPos2)
            where
                playerDistSq = \p -> vecDistSq playerPos p
                enemyPos1    = hitboxBotCenter $ _enemyHitbox lockOnData1
                enemyPos2    = hitboxBotCenter $ _enemyHitbox lockOnData2

-- lock-on should cycle through valid targets, ignoring previously locked on
-- locked-on history gets cleared when:
-- * all valid targets have already been locked on
-- * current locked-on target isn't in the set of valid targets (new set of targets)
updatePrevSwitchTargetEnemyIds :: [EnemyLockOnData] -> Maybe PlayerEnemyLockOn -> Maybe PlayerEnemyLockOn
updatePrevSwitchTargetEnemyIds _ Nothing                   = Nothing
updatePrevSwitchTargetEnemyIds lockOnDatas (Just enLockOn) = Just $ if
    | allTargetsPrevSeen || isNewTargets -> enLockOn {_prevSwitchTargetEnemyIds = S.empty}
    | otherwise                          -> enLockOn
    where
        lockOnDatasEnemyIds      = S.fromList [_enemyId (d :: EnemyLockOnData) | d <- lockOnDatas]
        prevSwitchTargetEnemyIds = _prevSwitchTargetEnemyIds enLockOn
        allTargetsPrevSeen       = lockOnDatasEnemyIds `S.isSubsetOf` prevSwitchTargetEnemyIds
        lockOnEnemyId            = _enemyId (enLockOn :: PlayerEnemyLockOn)
        isNewTargets             = lockOnEnemyId `S.notMember` lockOnDatasEnemyIds

isCurrentEnemyLockOn :: EnemyLockOnData -> PlayerLockOnAim -> Bool
isCurrentEnemyLockOn enemyLockOnData lockOnAim = case _enemyLockOn (lockOnAim :: PlayerLockOnAim) of
    Nothing          -> False
    Just enemyLockOn -> _enemyId (enemyLockOn :: PlayerEnemyLockOn) == _enemyId (enemyLockOnData :: EnemyLockOnData)

switchLockOnTarget :: (InputRead m, MsgsRead UpdatePlayerMsgsPhase m) => Player -> PlayerLockOnAim -> m PlayerLockOnAim
switchLockOnTarget player lockOnAim = do
    inputState <- readInputState

    let
        filterTargets :: [EnemyLockOnData] -> [EnemyLockOnData]
        filterTargets lockOnDatas = filter keepByPos lockOnDatas
            where
                keepByPos :: EnemyLockOnData -> Bool
                keepByPos lockOnData
                    | upHeld && leftHeld    = enemyX <= playerX && enemyY <= playerY
                    | upHeld && rightHeld   = enemyX >= playerX && enemyY <= playerY
                    | downHeld && leftHeld  = enemyX <= playerX && enemyY >= playerY
                    | downHeld && rightHeld = enemyX >= playerX && enemyY >= playerY
                    | upHeld                = enemyY <= playerY
                    | downHeld              = enemyY >= playerY
                    | leftHeld              = enemyX <= playerX
                    | rightHeld             = enemyX >= playerX
                    | otherwise             = case _dir player of
                        LeftDir  -> enemyX <= playerX
                        RightDir -> enemyX >= playerX
                    where
                        aliasHold' = \alias -> alias `aliasHold` inputState

                        downHeld             = aliasHold' DownAlias
                        upHeld               = aliasHold' UpAlias
                        leftHeld             = aliasHold' LeftAlias
                        rightHeld            = aliasHold' RightAlias
                        Pos2 playerX playerY = hitboxCenter $ playerHitbox player
                        Pos2 enemyX enemyY   = hitboxCenter $ _enemyHitbox lockOnData

        selectTarget :: [EnemyLockOnData] -> Maybe PlayerEnemyLockOn -> Maybe PlayerEnemyLockOn
        selectTarget [] enLockOn = enLockOn
        selectTarget (lockOnData:lockOnDatas) enLockOn
            | ignoreTarget       = selectTarget lockOnDatas enLockOn
            | otherwise          =
                let enemyHbx = _enemyHitbox lockOnData
                in Just $ PlayerEnemyLockOn
                    { _enemyId                  = enemyId
                    , _enemyHealth              = _enemyHealth (lockOnData :: EnemyLockOnData)
                    , _enemyVel                 = _enemyVel (lockOnData :: EnemyLockOnData)
                    , _lockOnPos                = hitboxBotCenter enemyHbx `vecAdd` _reticleOffset lockOnData
                    , _reticleScale             = _reticleScale (lockOnData :: EnemyLockOnData)
                    , _prevSwitchTargetEnemyIds = enemyId `S.insert` prevSwitchTargetEnemyIds
                    }
            where
                enemyId                  = _enemyId (lockOnData :: EnemyLockOnData)
                prevSwitchTargetEnemyIds = maybe S.empty _prevSwitchTargetEnemyIds enLockOn
                ignoreTarget             = fromMaybe False $ do
                    lockOnEnemyId <- (_enemyId :: PlayerEnemyLockOn -> MsgId) <$> enLockOn
                    Just $ enemyId == lockOnEnemyId || enemyId `S.member` prevSwitchTargetEnemyIds

        playerPos   = _pos player
        enemyLockOn = _enemyLockOn (lockOnAim :: PlayerLockOnAim)

    enemyLockOnDatas <- readMsgsEnemyLockOnData
    let
        enemyLockOnDatas' = flip execState enemyLockOnDatas $ do
            -- open up target selection to any enemy if restricting by held dir or player dir yields either of
            -- * no valid targets
            -- * only the already selected target
            case filterTargets enemyLockOnDatas of
                []                                        -> return ()
                [elod]
                    | isCurrentEnemyLockOn elod lockOnAim -> return ()
                elods                                     -> put elods
            modify $ sortEnemyLockOnDataByDist playerPos

    let
        enemyLockOn' = flip execState enemyLockOn $ do
            modify $ updatePrevSwitchTargetEnemyIds enemyLockOnDatas'
            modify $ selectTarget enemyLockOnDatas'
    return $ (lockOnAim :: PlayerLockOnAim) {_enemyLockOn = enemyLockOn'}

updateLockOnCursorTarget :: (InputRead m, MsgsRead UpdatePlayerMsgsPhase m) => PlayerLockOnAim -> m PlayerLockOnAim
updateLockOnCursorTarget lockOnAim = do
    mousePos <- _mouseWorldPos <$> readInputState

    let
        selectTarget :: Maybe PlayerEnemyLockOn -> [EnemyLockOnData] -> Maybe PlayerEnemyLockOn
        selectTarget enLockOn []                      = enLockOn
        selectTarget enLockOn (lockOnData:lockOnDatas)
            | mousePos `containsPointHitbox` enemyHbx =
                let enemyId = _enemyId (lockOnData :: EnemyLockOnData)
                in Just $ PlayerEnemyLockOn
                    { _enemyId                  = enemyId
                    , _enemyHealth              = _enemyHealth (lockOnData :: EnemyLockOnData)
                    , _enemyVel                 = _enemyVel (lockOnData :: EnemyLockOnData)
                    , _lockOnPos                = hitboxBotCenter enemyHbx `vecAdd` _reticleOffset lockOnData
                    , _reticleScale             = _reticleScale (lockOnData :: EnemyLockOnData)
                    , _prevSwitchTargetEnemyIds = S.fromList [enemyId]
                    }
            | otherwise                               = selectTarget enLockOn lockOnDatas
            where enemyHbx = _enemyHitbox lockOnData

    enemyLockOn <- selectTarget (_enemyLockOn (lockOnAim :: PlayerLockOnAim)) <$> readMsgsEnemyLockOnData
    return $ (lockOnAim :: PlayerLockOnAim) {_enemyLockOn = enemyLockOn}

updateLockOnGamepadAxisTarget
    :: (InputRead m, MsgsRead UpdatePlayerMsgsPhase m)
    => Player
    -> PlayerLockOnAim
    -> m PlayerLockOnAim
updateLockOnGamepadAxisTarget player lockOnAim = do
    inputState <- readInputState

    let
        selectTarget :: Maybe PlayerEnemyLockOn -> EnemyLockOnData -> Maybe PlayerEnemyLockOn
        selectTarget enLockOn lockOnData
            | aimAngleDiff > lockOnAngleThreshold = enLockOn
            | otherwise                           = Just enLockOn'
            where
                playerAimAngleWithPos' = \enPos -> playerAimAngleWithPos player enPos

                Vec2 axisX axisY = axisVec inputState
                axisAngle        = atan2 axisY axisX
                enemyHbx         = _enemyHitbox lockOnData
                enemyCenterPos   = hitboxCenter enemyHbx
                enemyAngle       = playerAimAngleWithPos' enemyCenterPos
                aimAngleDiff     = abs $ enemyAngle - axisAngle

                enemyId   = _enemyId (lockOnData :: EnemyLockOnData)
                enLockOn' = PlayerEnemyLockOn
                    { _enemyId                  = enemyId
                    , _enemyHealth              = _enemyHealth (lockOnData :: EnemyLockOnData)
                    , _enemyVel                 = _enemyVel (lockOnData :: EnemyLockOnData)
                    , _lockOnPos                = hitboxBotCenter enemyHbx `vecAdd` _reticleOffset lockOnData
                    , _reticleScale             = _reticleScale (lockOnData :: EnemyLockOnData)
                    , _prevSwitchTargetEnemyIds = S.fromList [enemyId]
                    }

        enemyLockOn = _enemyLockOn (lockOnAim :: PlayerLockOnAim)
        playerPos   = _pos player

    enemyLockOn' <- L.foldl' selectTarget enemyLockOn . reverse . sortEnemyLockOnDataByDist playerPos <$>
        readMsgsEnemyLockOnData
    return $ (lockOnAim :: PlayerLockOnAim) {_enemyLockOn = enemyLockOn'}

updateLockOnTarget :: (InputRead m, MsgsRead UpdatePlayerMsgsPhase m) => Player -> PlayerLockOnAim -> m PlayerLockOnAim
updateLockOnTarget player lockOnAim = do
    axisThresholdSq'     <- axisThresholdSq <$> readInputState
    isSwitchTargetInput' <- isSwitchTargetInput player
    isLockOnCursorInput' <- isLockOnCursorInput player
    isLockOnClearInput'  <- isLockOnClearInput player

    if
        | isSwitchTargetInput'                      -> switchLockOnTarget player lockOnAim
        | isLockOnCursorInput'                      -> updateLockOnCursorTarget lockOnAim
        | isLockOnClearInput'                       -> return $ clearPlayerLockOnAim lockOnAim
        | axisThresholdSq' >= lockOnAxisThresholdSq -> updateLockOnGamepadAxisTarget player lockOnAim
        | otherwise                                 -> return lockOnAim

updateLockOnPosHealth :: MsgsRead UpdatePlayerMsgsPhase m => PlayerLockOnAim -> m PlayerLockOnAim
updateLockOnPosHealth lockOnAim = case _enemyLockOn (lockOnAim :: PlayerLockOnAim) of
    Nothing       -> return lockOnAim
    Just enLockOn ->
        let
            processLockOnDatas :: [EnemyLockOnData] -> PlayerLockOnAim
            processLockOnDatas []              = clearPlayerLockOnAim lockOnAim
            processLockOnDatas (lockOnData:lockOnDatas)
                | _enemyId (lockOnData :: EnemyLockOnData) == currentEnemyId =
                    let enemyPos = hitboxBotCenter $ _enemyHitbox lockOnData
                    in lockOnAim
                        { _enemyLockOn = Just $ enLockOn
                            { _enemyHealth = _enemyHealth (lockOnData :: EnemyLockOnData)
                            , _enemyVel    = _enemyVel (lockOnData :: EnemyLockOnData)
                            , _lockOnPos   = enemyPos `vecAdd` _reticleOffset lockOnData
                            }
                        }
                | otherwise                                                  = processLockOnDatas lockOnDatas
                where currentEnemyId = _enemyId (enLockOn :: PlayerEnemyLockOn)
        in processLockOnDatas <$> readMsgsEnemyLockOnData

updateLockOnManualOverride :: InputRead m => PlayerLockOnAim -> m PlayerLockOnAim
updateLockOnManualOverride lockOnAim = do
    axisThresholdSq'  <- axisThresholdSq <$> readInputState
    let manualOverride = axisThresholdSq' >= lockOnAxisThresholdSq
    return $ lockOnAim {_manualOverride = manualOverride}

updateLockOnReticleSprites :: PlayerLockOnAim -> PlayerLockOnAim
updateLockOnReticleSprites lockOnAim = lockOnAim {_reticleSprites = reticleSprs}
    where reticleSprs = map updateSprite (_reticleSprites lockOnAim)

updatePlayerLockOnAim
    :: (InputRead m, MsgsRead UpdatePlayerMsgsPhase m)
    => Player
    -> PlayerLockOnAim
    -> m PlayerLockOnAim
updatePlayerLockOnAim player lockOnAim =
    updateLockOnTarget player lockOnAim >>=
    updateLockOnPosHealth >>=
    updateLockOnManualOverride >>=
    return . updateLockOnReticleSprites

drawPlayerLockOnAim :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => PlayerLockOnAim -> m ()
drawPlayerLockOnAim lockOnAim = unlessM (readSettingsConfig _debug _hideTargeting) $
    case _enemyLockOn (lockOnAim :: PlayerLockOnAim) of
        Nothing       -> return ()
        Just enLockOn ->
            let
                sprs           = _reticleSprites lockOnAim
                enHealth       = _enemyHealth (enLockOn :: PlayerEnemyLockOn)
                enHealthVal    = realToFrac $ _value enHealth
                enHealthMaxVal = realToFrac $ _maxValue enHealth
                enVel          = _enemyVel (enLockOn :: PlayerEnemyLockOn)
                numSprs        = realToFrac $ length lockOnReticleSprPathSuffixes
                sprIndex       = floor (enHealthVal / (enHealthMaxVal / numSprs)) - 1
                scale          = Scaled $ _reticleScale (enLockOn :: PlayerEnemyLockOn)
                lockOnPos      = _lockOnPos enLockOn
            in case listToMaybe (drop sprIndex sprs) of
                Just spr -> do
                    lockOnPos' <- graphicsLerpPos lockOnPos enVel
                    drawSpriteEx lockOnPos' RightDir playerLockOnZIndex 0.0 lockOnReticleOpacity scale spr
                Nothing  -> return ()

module Player.Weapon.All.Scythe
    ( mkScytheWeapon
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, get, lift, put, when)
import Data.Functor           ((<&>))
import qualified Data.Set as S

import Attack
import Configs
import Configs.All.PlayerWeapon.Scythe
import Constants
import FileCache
import InfoMsg.Util
import Msg
import Particle.All.Simple
import Player
import Player.BufferedInputState
import Player.Weapon as W
import Player.Weapon.All.Scythe.Data
import Player.Weapon.All.Scythe.FloatingAttack
import Player.Weapon.All.Scythe.RecallSlashProjectile
import Projectile as P
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

summonFlagFrameTagName          = FrameTagName "summon"           :: FrameTagName
blinkSlash2SoundFrameTagName    = FrameTagName "blinkSlash2Sound" :: FrameTagName
blinkSlashPlayerOffsetY         = 40.0                            :: PosY
blinkParticlePlayerOffsetY      = -50.0                           :: PosY
floatingAttackActiveTimeoutSecs = 0.02                            :: Secs
blinkSlashArenaWallFixOffsetX   = 100.0                           :: PosX

blinkSlash2SoundPath = "event:/SFX Events/Player/Weapons/Scythe/blink-slash2" :: FilePath

scythePack                = \f -> PackResourceFilePath "data/player/weapons/scythe.pack" f
playerParticleSprFilePath = scythePack "particle2-.spr" :: PackResourceFilePath

mkScytheWeapon :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Weapon)
mkScytheWeapon = do
    scytheData <- mkScytheData
    return . Some $ (mkWeapon scytheData ScytheWeapon)
        { W._think  = thinkScythe
        , W._update = updateScythe
        }

scytheEnoughChargeHeld :: ScytheData -> Bool
scytheEnoughChargeHeld scytheData = _chargeHeld scytheData >= chargeHeldThresholdSecs
    where chargeHeldThresholdSecs = _chargeHeldThresholdSecs $ _config (scytheData :: ScytheData)

mkFloatingAttackChargeHeldMsgs :: WeaponThinkStatus -> ScytheData -> [Msg ThinkPlayerMsgsPhase]
mkFloatingAttackChargeHeldMsgs weaponThinkStatus scytheData = case _floatingAttackStatus scytheData of
    FloatingAttackInactive                           -> []
    FloatingAttackActive _ _                         -> []
    FloatingAttackActiveReady floatingAtkMsgId _ _ _ ->
        let
            enoughChargeHeld = case weaponThinkStatus of
                WeaponThinkForeground _ -> scytheEnoughChargeHeld scytheData
                _                       -> False
            update           = \p -> p
                { P._data = (P._data p) {_enoughChargeHeld = enoughChargeHeld}
                }
        in [mkMsgTo (ProjectileMsgUpdate update) floatingAtkMsgId]

-- move player back within arena walls bounds, if outside due to blink strike
fixBlinkPosMsgs
    :: forall m. MsgsRead ThinkPlayerMsgsPhase m
    => ScytheData
    -> Maybe Attack
    -> Player
    -> m [Msg ThinkPlayerMsgsPhase]
fixBlinkPosMsgs scytheData currentAtk player = case currentAtk of
    Just atk
        | atk `attackIs` _blinkSlash1 (_playerScytheAttackDescs scytheData) -> readRoomArenaWallsInfo <&> \case
            Nothing             -> []
            Just arenaWallsInfo ->
                let
                    Pos2 x y   = _pos (player :: Player)
                    leftWallX  = vecX $ _leftWallPos arenaWallsInfo
                    rightWallX = vecX $ _rightWallPos arenaWallsInfo
                in if
                    | x <= leftWallX  ->
                        let fixedPos = Pos2 (leftWallX + blinkSlashArenaWallFixOffsetX) y
                        in [mkMsgEx (PlayerMsgSetPosition fixedPos) MsgEndOrder]
                    | x >= rightWallX ->
                        let fixedPos = Pos2 (rightWallX - blinkSlashArenaWallFixOffsetX) y
                        in [mkMsgEx (PlayerMsgSetPosition fixedPos) MsgEndOrder]
                    | otherwise       -> []

    _ -> return []

    where
        readRoomArenaWallsInfo :: m (Maybe RoomArenaWallsInfo)
        readRoomArenaWallsInfo = processMsgs <$> readMsgs
            where
                processMsgs :: [InfoMsgPayload] -> Maybe RoomArenaWallsInfo
                processMsgs []     = Nothing
                processMsgs (p:ps) = case p of
                    InfoMsgRoomArenaWalls arenaWallsInfo -> Just arenaWallsInfo
                    _                                    -> processMsgs ps

thinkScythe :: (ConfigsRead m, InputRead m, MonadIO m, MsgsRead ThinkPlayerMsgsPhase m) => WeaponThink ScytheData m
thinkScythe weaponThinkStatus player currentAtk scythe =
    let
        scytheData = W._data scythe

        landAtkUpdateMsgs = case currentAtk of
            Nothing  -> []
            Just atk ->
                let
                    playerScytheAtkDescs = _playerScytheAttackDescs scytheData
                    playerSummonMoves    = _playerSummonMoves scytheData
                    onGround             = _touchingGround (_flags player :: PlayerFlags)
                    inAir                = not onGround
                    clearAtkMsg          = mkMsg PlayerMsgClearAttack

                    attackIn' = \atkDescFs -> atk `attackIn` [atkDescF playerScytheAtkDescs | atkDescF <- atkDescFs]
                    summonIn' = \atkDescFs -> atk `attackIn` [atkDescF playerSummonMoves | atkDescF <- atkDescFs]
                in if
                    -- air blade slash1/2 land
                    | onGround && attackIn' [_airBladeSlash1, _airBladeSlash2]                      -> [clearAtkMsg]
                    -- cancel grounded attacks if now in air
                    | inAir && attackIn' [_bladeSlash1, _bladeSlash2]                               -> [clearAtkMsg]
                    -- cancel grounded summon moves if now in air
                    | inAir && summonIn' [_forwards, _longForwards, _upwards, _towards, _closeFist] -> [clearAtkMsg]
                    | otherwise                                                                     -> []

        audioMsgs = case currentAtk of
            Nothing  -> []
            Just atk ->
                let
                    atkFrameChanged = attackFrameChanged atk
                    pos             = _pos (player :: Player)
                    isAtkFrameTag   = blinkSlash2SoundFrameTagName `isAttackFrameTag` atk
                    mkPlaySoundMsg  = \filePath -> mkMsg $ AudioMsgPlaySound filePath pos
                in if
                    | atkFrameChanged && isAtkFrameTag -> [mkPlaySoundMsg blinkSlash2SoundPath]
                    | otherwise                        -> []

        chargeHeldMsgs = mkFloatingAttackChargeHeldMsgs weaponThinkStatus scytheData
    in do
        newAtkMsgs <- case weaponThinkStatus of
            WeaponThinkForeground weaponAtkStatus -> thinkScytheAttack scytheData weaponAtkStatus currentAtk player
            WeaponThinkBackground                 -> return []

        fixBlinkStrikeMsgs <- fixBlinkPosMsgs scytheData currentAtk player

        return $ landAtkUpdateMsgs ++ audioMsgs ++ newAtkMsgs ++ chargeHeldMsgs ++ fixBlinkStrikeMsgs

newSummonAttackMsgs :: InputRead m => ScytheData -> Player -> m [Msg ThinkPlayerMsgsPhase]
newSummonAttackMsgs scytheData player = attackMsgsFromInput <$> readInputState
    where
        setPlayerAttackDescMsgs :: Direction -> AttackDescription -> [Msg ThinkPlayerMsgsPhase]
        setPlayerAttackDescMsgs dir playerSummonMoveDesc =
            [ mkMsg $ PlayerMsgSetAttackDescEx pos dir playerSummonMoveDesc
            -- clear input buffer to prevent possibly triggering floating attack immediately afterwards
            , mkMsg $ PlayerMsgClearInputBuffer (S.singleton WeaponInput)
            ]
            where pos = _pos (player :: Player)

        attackMsgsFromInput :: InputState -> [Msg ThinkPlayerMsgsPhase]
        attackMsgsFromInput inputState
            | upHeld && weaponPressed && onGround = setPlayerAttackDescMsgs playerDir (_upwards playerSummonMoves)

            | downHeld && weaponPressed && onGround = setPlayerAttackDescMsgs playerDir (_away playerSummonMoves)

            | downHeld && weaponPressed && inAir = setPlayerAttackDescMsgs playerDir (_airDiagonal playerSummonMoves)

            | (inPlayerTapInputBuffer [LeftInput, LeftInput] player || isPlayerInputBufferQCF LeftDir player) &&
                not rightHeld && weaponPressed && onGround = setPlayerAttackDescMsgs LeftDir longForwards

            | (inPlayerTapInputBuffer [RightInput, RightInput] player || isPlayerInputBufferQCF RightDir player) &&
                not leftHeld && weaponPressed && onGround = setPlayerAttackDescMsgs RightDir longForwards

            | weaponPressed =
                let
                    playerSummonMove
                        | inAir     = _airForwards playerSummonMoves
                        | otherwise = _forwards playerSummonMoves
                in setPlayerAttackDescMsgs playerDir playerSummonMove

            | otherwise = []

            where
                weaponPressed = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
                downHeld      = DownAlias `aliasHold` inputState
                upHeld        = UpAlias `aliasHold` inputState
                leftHeld      = LeftAlias `aliasHold` inputState
                rightHeld     = RightAlias `aliasHold` inputState
                onGround      = _touchingGround (_flags player :: PlayerFlags)
                inAir         = not onGround

                playerSummonMoves = _playerSummonMoves scytheData
                longForwards      = _longForwards playerSummonMoves

                playerDir
                    | leftHeld  = LeftDir
                    | rightHeld = RightDir
                    | otherwise = _dir (player :: Player)

playerBlinkSlashAtkMsgs :: Pos2 -> Direction -> MsgId -> Player -> ScytheData -> [Msg ThinkPlayerMsgsPhase]
playerBlinkSlashAtkMsgs floatingAtkPos dir floatingAtkMsgId player scytheData =
    [ mkMsg $ PlayerMsgSetAttackDescEx blinkAtkPos dir blinkSlash1
    , mkMsgEx (PlayerMsgSetPosition blinkAtkPos) MsgAfterNormalOrder
    , particleMsg
    , updateFloatingAtkStatusMsg FloatingAttackInactive
    , removeFloatingAtkMsg floatingAtkMsgId
    ]
    where
        blinkAtkPos = floatingAtkPos `vecAdd` Pos2 0.0 blinkSlashPlayerOffsetY
        blinkSlash1 = _blinkSlash1 $ _playerScytheAttackDescs scytheData

        Pos2 playerX playerY = _pos (player :: Player)
        playerPos            = Pos2 playerX (playerY + blinkParticlePlayerOffsetY)
        playerDir            = _dir (player :: Player)
        loadSimpleParticle'  = loadSimpleParticle playerPos playerDir worldEffectZIndex playerParticleSprFilePath
        particleMsg          = mkMsg $ ParticleMsgAddM loadSimpleParticle'

playerRecallSlashAtkMsgs :: MonadIO m => Pos2 -> MsgId -> Player -> ScytheData -> m [Msg ThinkPlayerMsgsPhase]
playerRecallSlashAtkMsgs floatingAtkPos floatingAtkMsgId player scytheData =
    let
        scytheAtkDescs    = _scytheAttackDescs (scytheData :: ScytheData)
        playerPos         = _pos (player :: Player)
        playerSummonMoves = _playerSummonMoves scytheData
        onGround          = _touchingGround (_flags player :: PlayerFlags)
        towardsSummon
            | onGround    = _towards playerSummonMoves
            | otherwise   = _airTowards playerSummonMoves
    in do
        Some recallSlashProj <- mkRecallSlashProjectile floatingAtkPos playerPos scytheAtkDescs
        return $
            [ mkMsg $ NewThinkProjectileMsgAdd (Some recallSlashProj)
            , updateFloatingAtkStatusMsg $ FloatingAttackActive (P._msgId recallSlashProj) 0.0
            , removeFloatingAtkMsg floatingAtkMsgId
            , mkMsg $ PlayerMsgSetAttackDesc towardsSummon
            ]

playerBladeSlashMsgs :: InputRead m => ScytheData -> Maybe Attack -> Player -> m [Msg ThinkPlayerMsgsPhase]
playerBladeSlashMsgs scytheData currentAtk player = do
    inputState <- readInputState

    let
        attackIn'   = \atkDescs -> maybe False (`attackIn` atkDescs) currentAtk
        attackNotIn = \atkDescs -> not $ attackIn' atkDescs

        weaponPressed   = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
        onGround        = _touchingGround (_flags player :: PlayerFlags)
        inAir           = not onGround
        leftHeld        = LeftAlias `aliasHold` inputState
        rightHeld       = RightAlias `aliasHold` inputState
        pos             = _pos (player :: Player)
        dir
            | leftHeld  = LeftDir
            | rightHeld = RightDir
            | otherwise = _dir (player :: Player)

        playerScytheAtkDescs = _playerScytheAttackDescs scytheData
        bladeSlash1          = _bladeSlash1 playerScytheAtkDescs
        bladeSlash2          = _bladeSlash2 playerScytheAtkDescs
        airBladeSlash1       = _airBladeSlash1 playerScytheAtkDescs
        airBladeSlash2       = _airBladeSlash2 playerScytheAtkDescs

    if
        -- blade slash1
        | weaponPressed && onGround && attackNotIn [bladeSlash1, bladeSlash2] ->
            return [mkMsg $ PlayerMsgSetAttackDescEx pos dir bladeSlash1]

        -- air blade slash1
        | weaponPressed && inAir && attackNotIn [airBladeSlash1, airBladeSlash2] ->
            return [mkMsg $ PlayerMsgSetAttackDescEx pos dir airBladeSlash1]

        | otherwise -> return []

newFloatingAttackMsgs
    :: (ConfigsRead m, InputRead m, MonadIO m)
    => ScytheData
    -> Maybe Attack
    -> Player
    -> MsgId
    -> Pos2
    -> Direction
    -> m [Msg ThinkPlayerMsgsPhase]
newFloatingAttackMsgs scytheData currentAtk player floatingAtkMsgId floatingAtkPos floatingAtkDir = do
    inputState <- readInputState

    let
        weaponPressed = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
        weaponNotHeld = not $ WeaponAlias `aliasHold` inputState
        upHeld        = UpAlias `aliasHold` inputState
        downHeld      = DownAlias `aliasHold` inputState
        leftHeld      = LeftAlias `aliasHold` inputState
        rightHeld     = RightAlias `aliasHold` inputState

        onGround          = _touchingGround (_flags player :: PlayerFlags)
        playerSummonMoves = _playerSummonMoves scytheData
        playerCloseFistSummon
            | onGround    = _closeFist playerSummonMoves
            | otherwise   = _airCloseFist playerSummonMoves
        playerDownSummon
            | onGround    = _downwards playerSummonMoves
            | otherwise   = _airDownwards playerSummonMoves
        scytheAtkDescs    = _scytheAttackDescs (scytheData :: ScytheData)

        playerBlinkSlashAtkMsgs' = \dir ->
            playerBlinkSlashAtkMsgs floatingAtkPos dir floatingAtkMsgId player scytheData
        addFloatingAtkMsgs'      = \playerSummonDesc scytheAtkDesc ->
            let pos = floatingAtkPos
            in addFloatingAtkMsgs playerSummonDesc pos floatingAtkDir scytheAtkDesc floatingAtkMsgId scytheData

    if
        -- vertical slash
        | weaponPressed && downHeld -> addFloatingAtkMsgs' playerDownSummon (_vertSlash scytheAtkDescs)

        -- recall slash
        | weaponPressed && upHeld -> playerRecallSlashAtkMsgs floatingAtkPos floatingAtkMsgId player scytheData

        -- shatter launch
        | weaponNotHeld && scytheEnoughChargeHeld scytheData ->
            addFloatingAtkMsgs' playerCloseFistSummon (_shatterLaunch scytheAtkDescs)

        -- blink slash (left)
        | (inPlayerTapInputBuffer [LeftInput, LeftInput] player || isPlayerInputBufferQCF LeftDir player) &&
          not rightHeld && weaponPressed -> return $ playerBlinkSlashAtkMsgs' LeftDir

        -- blink slash (right)
        | (inPlayerTapInputBuffer [RightInput, RightInput] player || isPlayerInputBufferQCF RightDir player) &&
          not leftHeld && weaponPressed -> return $ playerBlinkSlashAtkMsgs' RightDir

        -- ground/air blade slash
        | otherwise -> playerBladeSlashMsgs scytheData currentAtk player

summonAttackMsgs :: (ConfigsRead m, MonadIO m) => ScytheData -> Maybe Attack -> m (Maybe [Msg ThinkPlayerMsgsPhase])
summonAttackMsgs _ Nothing = return Nothing
summonAttackMsgs scytheData (Just currentAtk)
    | currentAtkDesc == _forwards playerSummonMoves = if
        | shouldSummon -> mkFloatingAtkMsgs $ _multiSlash1 scytheAtkDescs
        | otherwise    -> return $ Just []

    | currentAtkDesc == _upwards playerSummonMoves = if
        | shouldSummon -> mkFloatingAtkMsgs $ _riseSlash scytheAtkDescs
        | otherwise    -> return $ Just []

    | currentAtkDesc == _away playerSummonMoves = if
        | shouldSummon -> mkFloatingAtkMsgs $ _pullSlash scytheAtkDescs
        | otherwise    -> return $ Just []

    | currentAtkDesc == _airForwards playerSummonMoves = if
        | shouldSummon -> mkFloatingAtkMsgs $ _multiSlash1 scytheAtkDescs
        | otherwise    -> return $ Just []

    | currentAtkDesc == _airDiagonal playerSummonMoves = if
        | shouldSummon -> mkFloatingAtkMsgs $ _diagSpinSlash scytheAtkDescs
        | otherwise    -> return $ Just []

    | currentAtkDesc == _longForwards playerSummonMoves = if
        | shouldSummon -> mkFloatingAtkMsgs $ _vertSpinSlash scytheAtkDescs
        | otherwise    -> return $ Just []

    | otherwise = return Nothing

    where
        currentAtkPos     = _pos (currentAtk :: Attack)
        currentAtkDir     = _dir (currentAtk :: Attack)
        currentAtkDesc    = _description currentAtk
        playerSummonMoves = _playerSummonMoves scytheData
        scytheAtkDescs    = _scytheAttackDescs (scytheData :: ScytheData)

        mkFloatingAtkMsgs :: (ConfigsRead m, MonadIO m) => AttackDescription -> m (Maybe [Msg ThinkPlayerMsgsPhase])
        mkFloatingAtkMsgs floatingAtkDesc = do
            let currentAtkPos' = playerSummonMovesOffsetPos scytheData currentAtkDesc currentAtkPos currentAtkDir
            newFloatingAtk    <- mkFloatingAttack currentAtkPos' currentAtkDir floatingAtkDesc scytheAtkDescs

            let
                updateScytheSt = \scythe -> scythe
                    { W._data = (W._data scythe)
                        { _floatingAttackStatus = FloatingAttackActive (P._msgId newFloatingAtk) 0.0
                        }
                    }

            return . Just $
                [ mkMsg $ NewThinkProjectileMsgAdd (Some newFloatingAtk)
                , mkMsg $ PlayerMsgUpdateWeapon updateScytheSt
                ]

        shouldSummon = summonFlagFrameTagName `isAttackFrameTag` currentAtk && attackFrameChanged currentAtk

thinkScytheAttack
    :: (ConfigsRead m, InputRead m, MonadIO m)
    => ScytheData
    -> WeaponAttackStatus
    -> Maybe Attack
    -> Player
    -> m [Msg ThinkPlayerMsgsPhase]
thinkScytheAttack scytheData wpnAtkStatus currentAtk player = case _floatingAttackStatus scytheData of
    FloatingAttackInactive -> summonAttackMsgs scytheData currentAtk >>= \case
        Nothing
            | wpnAtkStatus == WeaponAttackReady -> newSummonAttackMsgs scytheData player
            | otherwise                         -> return []
        Just msgs                               -> return msgs

    FloatingAttackActive _ _
        | wpnAtkStatus == WeaponAttackReady -> playerBladeSlashMsgs scytheData currentAtk player
        | otherwise                         -> return []

    FloatingAttackActiveReady floatingAtkMsgId pos dir _
        | wpnAtkStatus == WeaponAttackReady ->
            newFloatingAttackMsgs scytheData currentAtk player floatingAtkMsgId pos dir
        | otherwise                         -> return []

updateScythe :: (InputRead m, MsgsRead UpdatePlayerMsgsPhase m) => WeaponUpdate ScytheData m
updateScythe weaponUpdateStatus _ _ scythe = flip execStateT scythe $ do
    when (weaponUpdateStatus == WeaponUpdateForeground) $
        get >>= \s -> do
            let sData   = W._data s
            chargeHeld <- lift readInputState <&> \inputState -> if
                | WeaponAlias `aliasHold` inputState -> _chargeHeld sData + timeStep
                | otherwise                          -> 0.0
            put $ s
                { W._data = sData {_chargeHeld = chargeHeld}
                }

    -- reset to FloatingActiveInactive when floating attack disappears
    get >>= \s ->
        let
            updateFloatingAttackStatus
                :: MsgsRead UpdatePlayerMsgsPhase m1
                => (Secs -> FloatingAttackStatus)
                -> MsgId
                -> Secs
                -> m1 (Weapon ScytheData)
            updateFloatingAttackStatus floatingAtkStatusF floatingAtkMsgId missingSecs = processMsgs <$> readMsgs
                where
                    processMsgs :: [InfoMsgPayload] -> Weapon ScytheData
                    processMsgs = \case
                        [] ->
                            let
                                missingSecs'                                          = missingSecs + timeStep
                                status
                                    | missingSecs' >= floatingAttackActiveTimeoutSecs = FloatingAttackInactive
                                    | otherwise                                       = floatingAtkStatusF missingSecs'
                            in s
                                { W._data = (W._data s) {_floatingAttackStatus = status}
                                }

                        (d:ds) -> case d of
                            InfoMsgProjectilePos _ _ msgId
                                | msgId == floatingAtkMsgId -> s
                            _                               -> processMsgs ds
        in case _floatingAttackStatus (W._data s) of
            FloatingAttackInactive -> return ()

            FloatingAttackActive floatingAtkMsgId missingSecs ->
                let floatingAtkStatusF = \secs -> FloatingAttackActive floatingAtkMsgId secs
                in put =<< lift (updateFloatingAttackStatus floatingAtkStatusF floatingAtkMsgId missingSecs)

            FloatingAttackActiveReady floatingAtkMsgId pos dir missingSecs ->
                let floatingAtkStatusF = \secs -> FloatingAttackActiveReady floatingAtkMsgId pos dir secs
                in put =<< lift (updateFloatingAttackStatus floatingAtkStatusF floatingAtkMsgId missingSecs)

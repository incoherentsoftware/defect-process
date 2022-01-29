module Console.Commands
    ( consoleCommands
    ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe)
import Data.Yaml              (decodeEither')
import System.Directory       (doesDirectoryExist)
import System.FilePath        ((</>))
import Text.Read              (readMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Internal.Numbered as SDL.Internal

import AppEnv
import Attack.Util
import Audio.Fmod
import Audio.Volume
import Collision
import Configs
import Configs.All.Level
import Configs.All.Settings
import Configs.All.Settings.Audio
import Configs.All.Settings.Debug
import Console.Types
import Console.Util
import Enemy
import Enemy.All
import Enemy.Manager
import FileCache
import Id
import Level
import Level.Room
import Level.Room.ArenaWalls
import Level.Room.ArenaWalls.EnemySpawn
import Level.Room.ArenaWalls.JSON
import Level.Room.ArenaWalls.Util
import Level.Room.Event.Types
import Level.Room.Item.Pickup.All
import Level.Room.Item.Pickup.All.GunItemPickups
import Level.Room.Item.Pickup.All.HealthItemPickup
import Level.Room.Item.Pickup.All.MovementSkillItemPickups
import Level.Room.Item.Pickup.All.SecondarySkillItemPickups
import Level.Room.Item.Pickup.All.UpgradeItemPickups
import Level.Room.Item.Pickup.All.WeaponItemPickups
import Level.Room.Item.Types
import Level.Room.JSON
import Level.Room.Parse
import Msg.Phase
import Particle.All.Simple
import Particle.Manager
import Player
import Player.Gun as G
import Player.Gun.Manager
import Player.Info
import Player.Meter
import Player.MovementSkill as MS
import Player.SecondarySkill as SS
import Player.SecondarySkill.All
import Player.SecondarySkill.Manager
import Player.Upgrade
import Player.Upgrade.Manager
import Player.Weapon as W
import Player.Weapon.All
import Player.Weapon.Manager
import SaveFiles
import Stats.Manager
import Util
import Window
import World
import World.Audio
import World.Audio.LayeredMusic.Manager
import World.Camera
import World.Util
import World.ZIndex

consoleCommands =
    [ (["spawn", "s"], spawnCmd)
    , (["item", "i"], itemCmd)
    , (["rmEnemy", "rm"], removeEnemyCmd)
    , (["show"], showCmd)
    , (["hide"], hideCmd)
    , (["ai"], aiCmd)
    , (["hurt"], hurtCmd)
    , (["muteSound", "ms"], muteSoundCmd)
    , (["muteMusic", "mm"], muteMusicCmd)
    , (["soundVolume", "soundVol"], soundVolumeCmd)
    , (["musicVolume", "musicVol"], musicVolumeCmd)
    , (["teleport", "t"], teleportCmd)
    , (["room"], roomCmd)
    , (["god"], godCmd)
    , (["damageMultiplier", "damageMult"], playerDamageMultiplierCmd)
    , (["enemiesDamageMultiplier", "enDamageMult"], enemiesDamageMultiplierCmd)
    , (["give"], giveCmd)
    , (["giveWeapon", "giveW"], giveWeaponCmd)
    , (["giveGun", "giveG"], giveGunCmd)
    , (["giveMovement", "giveM"], giveMovementSkillCmd)
    , (["giveSecondary", "giveS"], giveSecondarySkillCmd)
    , (["giveUpgrade", "giveU"], giveUpgradeCmd)
    , (["clear"], clearCmd)
    , (["gold"], goldCmd)
    , (["infiniteMeter", "infMeter"], infiniteMeterCmd)
    , (["meter", "m"], meterCmd)
    , (["quit", "q", "exit"], quitCmd)
    , (["progressLayeredMusic", "progressL"], progressLayeredMusicCmd)
    , (["reloadConfigs", "reloadCfgs", "rc"], reloadConfigsCmd)
    , (["cameraOffset", "co"], cameraOffsetCmd)
    , (["cameraPosition", "cp"], cameraPositionCmd)
    , (["save"], saveCmd)
    , (["load"], loadCmd)
    , (["warpOut"], warpOutCmd)
    , (["exploreMusic", "exploremusic"], exploreMusicCmd)
    , (["battleMusic", "battlemusic"], battleMusicCmd)
    , (["reloadSounds", "rs", "r"], reloadSoundsCmd)
    , (["dangerValue", "dv"], dangerValueCmd)
    , (["arena"], arenaCmd)
    , (["shop", "store"], shopCmd)
    , (["event"], eventCmd)
    , (["run"], runCmd)
    , (["direction", "dir"], directionCmd)
    , (["bind"], bindCmd)
    , (["devConsole"], devConsoleCommand)
    , (["arenaTrigger"], arenaTriggerCmd)
    , (["bossTrigger"], bossTriggerCmd)
    , (["defaultGraphicsFallback"], defaultGraphicsFallbackCmd)
    ] :: [([T.Text], ConsoleCommand (AppEnv ConsoleMsgsPhase))]

argsN :: Int -> [T.Text] -> T.Text
argsN n args = case drop n args of
    (arg:_) -> arg
    _       -> ""

args0 :: [T.Text] -> T.Text
args0 = argsN 0

args1 :: [T.Text] -> T.Text
args1 = argsN 1

args2 :: [T.Text] -> T.Text
args2 = argsN 2

args3 :: [T.Text] -> T.Text
args3 = argsN 3

readMaybe' :: Read a => T.Text -> Maybe a
readMaybe' = readMaybe . T.unpack

formatPos2Int :: Pos2 -> T.Text
formatPos2Int (Pos2 x y) = "(" <> prettyShowRound x <> ", " <> prettyShowRound y <> ")"
    where prettyShowRound = prettyShow @Int . round

spawnCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
spawnCmd args world =
    let
        name      = args0 args
        enemyType = enemyTypeFromName name

        mkEnemyFromName :: Pos2 -> AppEnv p (Maybe (Some Enemy))
        mkEnemyFromName pos@(Pos2 x _) = maybe (return Nothing) (\f -> Just <$> f pos dir) mkF
            where
                playerX           = vecX $ _pos (_player (world :: World) :: Player)
                dir
                    | x < playerX = RightDir
                    | otherwise   = LeftDir

                mkF = mkEnemyFromType <$> enemyType

        spawnEnemy :: Pos2 -> AppEnv ConsoleMsgsPhase (Maybe ConsoleCommandResult)
        spawnEnemy pos = mkEnemyFromName pos <&> \case
            Just (Some enemy) ->
                let
                    debugArg
                        | args3 args /= "" = args3 args
                        | args2 args /= "" = ""
                        | otherwise        = args1 args

                    debugFlags         = map T.toLower (T.splitOn "," debugArg)
                    (debugTxt, enemy') = (_setDebugBehavior enemy) debugFlags enemy
                    output             = "spawned " <> name <> " enemy at " <> formatPos2Int pos <> debugTxt
                    enemyManager       = _enemyManager world
                    enemies            = _enemies (enemyManager :: EnemyManager)
                    enemyManager'      = enemyManager {_enemies = Some enemy':enemies} :: EnemyManager
                    world'             = world {_enemyManager = enemyManager'}
                in Just $ UpdateWorldResult output world'
            Nothing    -> Nothing
    in do
        mousePos <- _mouseWorldPos <$> readInputState

        let
            targetPos = case (args1 args, args2 args) of
                ("", _) -> Just mousePos
                (_, "") -> Just mousePos
                (x, y)  -> Pos2 <$> (readMaybe' x :: Maybe Float) <*> (readMaybe' y :: Maybe Float)

            arenaWalls          = _arenaWalls (_room (_level (world :: World)) :: Room)
            enemySpawnPositions = (_enemySpawnPositions :: RoomArenaWalls -> [Pos2]) <$> arenaWalls
            targetPos'          = case (targetPos, enemySpawnPositions) of
                (Just (Pos2 targetX _), Just (Pos2 _ spawnY:_)) ->
                    roomArenaWallsEnemySpawnOffset (Pos2 targetX spawnY) <$> enemyType
                _                                               -> targetPos

        case targetPos' of
            Nothing  -> return $ NoUpdateResult "invalid coordinates"
            Just pos -> fromMaybe (NoUpdateResult "unknown spawn") <$> spawnEnemy pos

itemCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
itemCmd args world = do
    let
        name     = args0 args
        roomType = (_type :: Room -> RoomType) . _room $ _level (world :: World)

    pos <- _mouseWorldPos <$> readInputState

    item <- case T.toLower name of
        -- NOTE: this is modified from the full source since only sword is included in this repo
        "sword"       -> Just <$> mkSwordItemPickup roomType pos
        "gauntlets"   -> Just <$> mkSwordItemPickup roomType pos
        "scythe"      -> Just <$> mkSwordItemPickup roomType pos
        "staff"       -> Just <$> mkSwordItemPickup roomType pos
        "spiritblade" -> Just <$> mkSwordItemPickup roomType pos

        -- NOTE: this is modified from the full source since only revolver is included in this repo
        "revolver"        -> Just <$> mkRevolverItemPickup roomType pos
        "shotgun"         -> Just <$> mkRevolverItemPickup roomType pos
        "shardgun"        -> Just <$> mkRevolverItemPickup roomType pos
        "grenadelauncher" -> Just <$> mkRevolverItemPickup roomType pos
        "spikegun"        -> Just <$> mkRevolverItemPickup roomType pos
        "ricochetgun"     -> Just <$> mkRevolverItemPickup roomType pos


        -- NOTE: this is modified from the full source since only dash is included in this repo
        "dash"     -> Just <$> mkDashItemPickup roomType pos
        "teleport" -> Just <$> mkDashItemPickup roomType pos
        "grapple"  -> Just <$> mkDashItemPickup roomType pos

        -- NOTE: this is modified from the full source since only stoneForm is included in this repo
        "stoneform" -> Just <$> mkStoneFormItemPickup roomType pos
        "flight"    -> Just <$> mkStoneFormItemPickup roomType pos
        "fastfall"  -> Just <$> mkStoneFormItemPickup roomType pos

        "doublejump"    -> Just <$> mkDoubleJumpUpgradeItemPickup roomType pos
        "movementskill" -> Just <$> mkMovementSkillUpgradeItemPickup roomType pos
        "meter"         -> Just <$> mkMeterUpgradeItemPickup roomType pos

        "health" -> Just <$> mkHealthItemPickup roomType pos (_statsManager world)

        _ -> return Nothing

    return $ case item of
        Nothing    -> NoUpdateResult $ "unknown item " <> name
        Just item' ->
            let
                output = "spawned " <> name <> " item at " <> prettyShow pos
                level  = _level (world :: World)
                room   = _room level
                items  = _items (room :: Room)
                world' = world
                    { _level = level
                        { _room = room {_items = item':items}
                        }
                    } :: World
            in UpdateWorldResult output world'

removeEnemyCmd :: InputRead m => ConsoleCommand m
removeEnemyCmd args world = case args0 args of
    "all" ->
        let
            enemyManager' = enemyManager {_enemies = []} :: EnemyManager
            world'        = world {_enemyManager = enemyManager'}
        in return $ UpdateWorldResult "removed all enemies" world'

    _ -> do
        mousePos <- _mouseWorldPos <$> readInputState

        let
            isNotSelected = \(Some enemy) -> not $ containsPointHitbox mousePos (enemyHitbox enemy)
            enemies       = filter isNotSelected (_enemies (enemyManager :: EnemyManager))
            enemyManager' = enemyManager {_enemies = enemies} :: EnemyManager
            world'        = world {_enemyManager = enemyManager'}
            output        = "removed enemies at " <> formatPos2Int mousePos

        return $ UpdateWorldResult output world'

    where enemyManager = _enemyManager world

showCmd :: ConfigsRead m => ConsoleCommand m
showCmd args _ = do
    cfgs <- readConfigs

    return . fromMaybe (NoUpdateResult "invalid show") $ do
        let args0Elem   = \xs -> args0 args `elem` xs
        updateDebugCfg <- if
            | args0Elem ["hitboxes", "hbx"] -> Just $ \f d -> d
                { _drawEntityHitboxes   = f $ _drawEntityHitboxes d
                , _drawSurfaceHitboxes  = f $ _drawSurfaceHitboxes d
                , _drawPlatformHitboxes = f $ _drawPlatformHitboxes d
                , _drawArenaHitboxes    = f $ _drawArenaHitboxes d
                , _drawPortalHitboxes   = f $ _drawPortalHitboxes d
                , _drawItemHitboxes     = f $ _drawItemHitboxes d
                }

            | args0Elem ["entityHitboxes", "entityHbx"]     ->
                Just $ \f d -> d {_drawEntityHitboxes = f (_drawEntityHitboxes d)}
            | args0Elem ["surfaceHitboxes", "surfaceHbx"]   ->
                Just $ \f d -> d {_drawSurfaceHitboxes = f (_drawSurfaceHitboxes d)}
            | args0Elem ["platformHitboxes", "platformHbx"] ->
                Just $ \f d -> d {_drawPlatformHitboxes = f (_drawPlatformHitboxes d)}
            | args0Elem ["arenaHitboxes", "arenaHbx"]       ->
                Just $ \f d -> d {_drawArenaHitboxes = f (_drawArenaHitboxes d)}
            | args0Elem ["portalHitboxes", "portalHbx"]     ->
                Just $ \f d -> d {_drawPortalHitboxes = f (_drawPortalHitboxes d)}
            | args0Elem ["itemHitboxes", "itemHbx"]         ->
                Just $ \f d -> d {_drawItemHitboxes = f (_drawItemHitboxes d)}
            | args0Elem ["enemyDebugText", "enemyTxt"]      ->
                Just $ \f d -> d {_drawEnemyDebugText = f (_drawEnemyDebugText d)}
            | otherwise                                     -> Nothing

        let
            (onOff, infoText) = case args1 args of
                "0" -> (const False, "hiding ")
                "1" -> (const True, "showing ")
                _   -> (not, "toggling show ")

            cfgs' = cfgs
                { _settings =
                    let settingsCfg = _settings cfgs
                    in settingsCfg {_debug = updateDebugCfg onOff (_debug settingsCfg)}
                }
        Just $ UpdateConfigsResult (infoText <> args0 args) cfgs'

hideCmd :: ConfigsRead m => ConsoleCommand m
hideCmd args _ = do
    cfgs <- readConfigs

    return . fromMaybe (NoUpdateResult "invalid hide") $ do
        updateDebugCfg <- case args0 args of
            "hud"       -> Just $ \f d -> d {_hideHud = f (_hideHud d)}
            "targeting" -> Just $ \f d -> d {_hideTargeting = f (_hideTargeting d)}
            "cursor"    -> Just $ \f d -> d {_hideCursor = f (_hideCursor d)}
            "player"    -> Just $ \f d -> d {_hidePlayer = f (_hidePlayer d)}
            _           -> Nothing

        let
            (onOff, infoText) = case args1 args of
                "0" -> (const False, "showing ")
                "1" -> (const True, "hiding ")
                _   -> (not, "toggling hide ")

            cfgs' = cfgs
                { _settings =
                    let settingsCfg = _settings cfgs
                    in settingsCfg {_debug = updateDebugCfg onOff (_debug settingsCfg)}
                }
        Just $ UpdateConfigsResult (infoText <> args0 args) cfgs'

aiCmd :: ConfigsRead m => ConsoleCommand m
aiCmd args _ = do
    enable <- case args0 args of
        "0" -> return False
        "1" -> return True
        _   -> readSettingsConfig _debug _disableAI

    configs <- readConfigs <&> \cfgs -> cfgs
        { _settings =
            let settingsCfg = _settings cfgs
            in settingsCfg {_debug = (_debug settingsCfg) {_disableAI = not enable}}
        }

    let output = (if enable then "enabled" else "disabled") <> " enemy AI"
    return $ UpdateConfigsResult output configs

hurtCmd :: Monad m => ConsoleCommand m
hurtCmd args world = return $ case readMaybe' (args0 args) :: Maybe Int of
    Nothing   -> NoUpdateResult "invalid hurt amount"
    Just hurt ->
        let
            player  = _player (world :: World)
            hp      = _health (player :: Player)
            hp'     = decreaseHealth (Damage hurt) hp
            player' = player {_health = hp'} :: Player
            world'  = world {_player = player'} :: World
            output  = "hurt player, hp now: " <> prettyShow hp'
        in UpdateWorldResult output world'

muteSoundCmd :: (ConfigsRead m, MonadIO m) => ConsoleCommand m
muteSoundCmd args _ = do
    on <- case args0 args of
        "0" -> return True
        "1" -> return False
        _   -> not <$> readSettingsConfig _audio _soundsEnabled

    volume <- if
        | on        -> readSettingsConfig _audio _soundVolume
        | otherwise -> return $ mkVolume 0
    setFmodSoundVolume volume

    configs <- readConfigs <&> \cfgs -> cfgs
        { _settings =
            let
                settingsCfg = _settings cfgs
                audioCfg    = _audio (settingsCfg :: SettingsConfig)
            in settingsCfg {_audio = audioCfg {_soundsEnabled = on}}
        }

    let output = "sounds " <> (if on then "on" else "off")
    return $ UpdateConfigsResult output configs

muteMusicCmd :: (ConfigsRead m, MonadIO m) => ConsoleCommand m
muteMusicCmd args _ = do
    mute <- case args0 args of
        "0" -> return False
        "1" -> return True
        _   -> readSettingsConfig _audio _musicEnabled

    muteFmodMusic mute

    configs <- readConfigs <&> \cfgs -> cfgs
        { _settings =
            let
                settingsCfg = _settings cfgs
                audioCfg    = _audio (settingsCfg :: SettingsConfig)
            in settingsCfg {_audio = audioCfg {_musicEnabled = not mute}}
        }

    let output = "music " <> (if mute then "muted" else "unmuted")
    return $ UpdateConfigsResult output configs

soundVolumeCmd :: (ConfigsRead m, MonadIO m) => ConsoleCommand m
soundVolumeCmd args _ = do
    volume <- case args0 args of
        ""  -> Just <$> readSettingsConfig _audio _soundVolume
        vol -> return $ mkVolume <$> (readMaybe' vol :: Maybe Int)

    case volume of
        Nothing  -> return $ NoUpdateResult "invalid sound volume"
        Just vol -> do
            configs <- setSoundVolumeConsole vol =<< readConfigs
            let output = "set sound volume to " <> prettyShow vol
            return $ UpdateConfigsResult output configs

musicVolumeCmd :: (ConfigsRead m, MonadIO m) => ConsoleCommand m
musicVolumeCmd args _ = do
    volume <- case args0 args of
        ""  -> Just <$> readSettingsConfig _audio _musicVolume
        vol -> return $ mkVolume <$> (readMaybe' vol :: Maybe Int)

    case volume of
        Nothing  -> return $ NoUpdateResult "invalid music volume"
        Just vol -> do
            configs <- setMusicVolumeConsole vol =<< readConfigs
            let output = "set music volume to " <> prettyShow vol
            return $ UpdateConfigsResult output configs

teleportCmd :: InputRead m => ConsoleCommand m
teleportCmd args world = do
    mousePos <- _mouseWorldPos <$> readInputState
    let
        pos = case (args0 args, args1 args) of
            ("", _) -> Just mousePos
            (_, "") -> Just mousePos
            (x, y)  -> Pos2 <$> (readMaybe' x :: Maybe Float) <*> (readMaybe' y :: Maybe Float)

    return $ case pos of
        Nothing   -> NoUpdateResult "invalid coordinates"
        Just pos' ->
            let
                player = _player (world :: World)
                world' = world
                    { _player = player
                        { _pos = pos'
                        , _vel = Vel2 0.0 0.1
                        }
                    } :: World
                output = "teleported player to " <> formatPos2Int pos'
            in UpdateWorldResult output world'

roomCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
roomCmd args world = do
    let
        roomName     = args0 args
        isPrefixed   = \s -> s `T.isPrefixOf` roomName
        stripPrefix' = \s -> fromMaybe roomName (T.stripPrefix s roomName)

    roomType <- case roomName of
        ""                       -> return . (_type :: Room -> RoomType) . _room $ _level (world :: World)  -- reload
        "next"                   -> return NextRoomType
        _
            | isPrefixed "to-"   -> return $ ToTransitionRoomType (stripPrefix' "to-")
            | isPrefixed "from-" -> return $ FromTransitionRoomType (stripPrefix' "from-")
            | otherwise          -> liftIO $ do
                arenasPath       <- translateResourcePath $ "data/levels/arenas" </> T.unpack roomName
                challengesPath   <- translateResourcePath $ "data/levels/challenges" </> T.unpack roomName
                isArenasPath     <- doesDirectoryExist arenasPath
                isChallengesPath <- doesDirectoryExist challengesPath
                return $ if
                    | isArenasPath     -> ArenaRoomType roomName
                    | isChallengesPath -> ChallengeRoomType roomName
                    | otherwise        -> SpecialRoomType roomName

    let
        changeWorldRoom' = case roomType of
            NextRoomType -> changeWorldRoom
            _            -> changeWorldRoomNoSkip

    catchAppEnv (Right <$> changeWorldRoom' roomType 0.0 world) (return . Left) >>= \case
        Left e       -> return . NoUpdateResult $ "invalid room [" <> roomName <> "]: " <> prettyShow e
        Right world' ->
            let
                newRoom     = _room $ _level (world' :: World)
                newRoomType = _type (newRoom :: Room)
                output      = "room changed to: " <> prettyShow newRoomType
            in return $ UpdateWorldResult output world'

godCmd :: ConfigsRead m => ConsoleCommand m
godCmd args _ = case args0 args of
    "enemies" -> do
        on <- case args1 args of
            "0" -> return False
            "1" -> return True
            _   -> not <$> readSettingsConfig _debug _enemiesInvincible

        configs <- readConfigs <&> \cfgs -> cfgs
            { _settings =
                let settingsCfg = _settings cfgs
                in settingsCfg {_debug = (_debug settingsCfg) {_enemiesInvincible = on}}
            }

        let output = "godmode enemies: " <> (if on then "on" else "off")
        return $ UpdateConfigsResult output configs

    "player" -> do
        on <- case args1 args of
            "0" -> return False
            "1" -> return True
            _   -> not <$> readSettingsConfig _debug _playerInvincible

        configs <- readConfigs <&> \cfgs -> cfgs
            { _settings =
                let settingsCfg = _settings cfgs
                in settingsCfg {_debug = (_debug settingsCfg) {_playerInvincible = on}}
            }

        let output = "godmode player: " <> (if on then "on" else "off")
        return $ UpdateConfigsResult output configs

    _ -> return $ NoUpdateResult ""

playerDamageMultiplierCmd :: ConfigsRead m => ConsoleCommand m
playerDamageMultiplierCmd args _ = case readMaybe' (args0 args) :: Maybe Float of
    Nothing         -> return $ NoUpdateResult "invalid percent value"
    Just multiplier -> do
        configs <- readConfigs <&> \cfgs -> cfgs
            { _settings =
                let settingsCfg = _settings cfgs
                in settingsCfg {_debug = (_debug settingsCfg) {_playerDamageMultiplier = multiplier}}
            }
        let output = "player damage multiplier set to: " <> args0 args
        return $ UpdateConfigsResult output configs

enemiesDamageMultiplierCmd :: ConfigsRead m => ConsoleCommand m
enemiesDamageMultiplierCmd args _ = case readMaybe' (args0 args) :: Maybe Float of
    Nothing
        | args0 args == "" -> do
            enDamageMult <- readSettingsConfig _debug _enemiesDamageMultiplier
            return $ NoUpdateResult ("enemiesDamageMultiplier: " <> T.pack (show enDamageMult))
        | otherwise        -> return $ NoUpdateResult "invalid percent value"

    Just multiplier -> do
        configs <- readConfigs <&> \cfgs -> cfgs
            { _settings =
                let settingsCfg = _settings cfgs
                in settingsCfg {_debug = (_debug settingsCfg) {_enemiesDamageMultiplier = multiplier}}
            }
        let output = "enemies damage multiplier set to: " <> args0 args
        return $ UpdateConfigsResult output configs

giveCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
giveCmd args world = tryGive [giveWeaponCmd, giveGunCmd, giveMovementSkillCmd, giveSecondarySkillCmd, giveUpgradeCmd]
    where
        tryGive :: [ConsoleCommand (AppEnv ConsoleMsgsPhase)] -> AppEnv ConsoleMsgsPhase ConsoleCommandResult
        tryGive = \case
            []         -> return $ NoUpdateResult "unknown"
            (cmd:cmds) -> cmd args world >>= \case
                NoUpdateResult _ -> tryGive cmds
                result           -> return result

giveWeaponCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
giveWeaponCmd args world = do
    let player = _player (world :: World)

    wpn <- case T.toLower (args0 args) of
        -- NOTE: this is modified from the full source since only sword is included in this repo
        "sword"       -> Just <$> mkSwordWeapon
        "gauntlets"   -> Just <$> mkSwordWeapon
        "staff"       -> Just <$> mkSwordWeapon
        "scythe"      -> Just <$> mkSwordWeapon
        "spiritblade" -> Just <$> mkSwordWeapon
        "random"      -> case filter (`notElem` playerWeaponTypes player) allWeaponTypes of
            []     -> return Nothing
            (t:ts) ->
                let wpnTypes = t NE.:| ts
                in Just <$> (mkWeaponFromType =<< randomChoice wpnTypes)

        name
            | Just t <- readMaybe' name -> Just <$> mkWeaponFromType t
            | otherwise                 -> return Nothing

    return $ case wpn of
        Nothing       -> NoUpdateResult "unknown weapon"
        Just (Some w) ->
            let
                player' = givePlayerWeapon (Some w) player
                world'  = world {_player = player'} :: World
                output  = "gave " <> prettyShow (W._type w)
            in UpdateWorldResult output world'

giveGunCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
giveGunCmd args world = do
    let player = _player (world :: World)

    gun <- case T.toLower (args0 args) of
        -- NOTE: this is modified from the full source since only revolver is included in this repo
        "revolver"        -> Just <$> mkRevolverGun
        "shotgun"         -> Just <$> mkRevolverGun
        "grenadelauncher" -> Just <$> mkRevolverGun
        "shardgun"        -> Just <$> mkRevolverGun
        "spikegun"        -> Just <$> mkRevolverGun
        "ricochetgun"     -> Just <$> mkRevolverGun
        "random"          -> case filter (`notElem` playerGunTypes player) allGunTypes of
            []     -> return Nothing
            (t:ts) ->
                let gunTypes = t NE.:| ts
                in Just <$> (mkGunFromType =<< randomChoice gunTypes)

        name
            | Just t <- readMaybe' name -> Just <$> mkGunFromType t
            | otherwise                 -> return Nothing

    return $ case gun of
        Nothing       -> NoUpdateResult "unknown gun"
        Just (Some g) ->
            let
                player' = givePlayerGun (Some g) player
                world'  = world {_player = player'} :: World
                output = "gave " <> prettyShow (G._type g)
            in UpdateWorldResult output world'

giveMovementSkillCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
giveMovementSkillCmd args world = do
    let player = _player (world :: World)

    movementSkill <- case args0 args of
        -- NOTE: this is modified from the full source since only stoneForm is included in this repo
        "dash"     -> Just <$> mkDashSkill
        "teleport" -> Just <$> mkDashSkill
        "grapple"  -> Just <$> mkDashSkill
        "random"   ->
            let
                moveSkillTypes = case playerMovementSkillType player of
                    Just moveSkillType -> filter (/= moveSkillType) allMovementSkillTypes
                    Nothing            -> allMovementSkillTypes
            in case moveSkillTypes of
                []     -> return Nothing
                (t:ts) ->
                    let msTypes = t NE.:| ts
                    in Just <$> (mkMovementSkillFromType =<< randomChoice msTypes)

        name
            | Just t <- readMaybe' name -> Just <$> mkMovementSkillFromType t
            | otherwise                 -> return Nothing

    return $ case movementSkill of
        Nothing        -> NoUpdateResult "unknown movement skill"
        Just (Some ms) ->
            let
                player' = player {_movementSkill = Just (Some ms)}
                world'  = world {_player = player'} :: World
                output  = "gave " <> prettyShow (MS._type ms)
            in UpdateWorldResult output world'

giveSecondarySkillCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
giveSecondarySkillCmd args world = do
    secondarySkill <- case args0 args of
        -- NOTE: this is modified from the full source since only stoneForm is included in this repo
        "stoneForm"  -> Just <$> mkStoneFormSkill
        "flight"     -> Just <$> mkStoneFormSkill
        "fastFall"   -> Just <$> mkStoneFormSkill

        name
            | Just t <- readMaybe' name -> Just <$> mkSecondarySkillFromType t
            | otherwise                 -> return Nothing

    return $ case secondarySkill of
        Nothing           -> NoUpdateResult "unknown secondary skill"
        Just (Some skill) ->
            let
                slot   = case args1 args of
                    "neutral" -> Just SecondarySkillNeutralSlot
                    "up"      -> Just SecondarySkillUpSlot
                    "down"    -> Just SecondarySkillDownSlot
                    _         -> Nothing

                player            = _player (world :: World)
                secondarySkillMgr = giveSecondarySkillManagerSkill slot (Some skill) (_secondarySkillManager player)
                player'           = player {_secondarySkillManager = secondarySkillMgr}
                world'            = world {_player = player'} :: World
                output            = "gave " <> prettyShow (SS._type skill)
            in UpdateWorldResult output world'

giveUpgradeCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
giveUpgradeCmd args world = return $ case upgradeType of
    Nothing -> NoUpdateResult "unknown upgrade"
    Just t  ->
        let
            player  = _player (world :: World)
            world'  = world {_player = givePlayerUpgrade t player} :: World
            output  = "gave " <> prettyShow t
        in UpdateWorldResult output world'
    where
        upgradeType = case T.toLower (args0 args) of
            "doublejump"    -> Just DoubleJumpUpgradeType
            "movementskill" -> Just MovementSkillUpgradeType
            "meter"         -> Just MeterUpgradeType
            _               -> Nothing

clearCmd :: Monad m => ConsoleCommand m
clearCmd args world = return $ UpdateWorldResult output world'
    where
        clearPlayerWeapons         = \p -> p {_weaponManager = (_weaponManager p) {_weapons = []}}
        clearPlayerGuns            = \p -> p {_gunManager = (_gunManager p) {_guns = []}}
        clearPlayerMovementSkill   = \p -> p {_movementSkill = Nothing}
        clearPlayerSecondarySkills = \p -> p
            { _secondarySkillManager = (_secondarySkillManager p)
                { _neutralSlot = Nothing
                , _upSlot      = Nothing
                , _downSlot    = Nothing
                }
            }
        clearPlayerUpgrades        = \p -> p {_upgradeManager = (_upgradeManager p) {_counts = M.empty}}

        player            = _player (world :: World)
        (player', output) = case args0 args of
            "weapons"         -> (clearPlayerWeapons player, "cleared player weapons")
            "guns"            -> (clearPlayerGuns player, "cleared player guns")
            "movementSkill"   -> (clearPlayerMovementSkill player, "cleared player movement skill")
            "secondarySkills" -> (clearPlayerSecondarySkills player, "cleared player secondary skills")
            "upgrades"        -> (clearPlayerUpgrades player, "cleared player upgrades")
            "all"             ->
                ( clearPlayerWeapons .
                  clearPlayerGuns .
                  clearPlayerMovementSkill .
                  clearPlayerSecondarySkills .
                  clearPlayerUpgrades $ player
                , "cleared player weapons/guns/movement skill/secondary skills/upgrades"
                )
            _                 -> (player, "unknown clear")

        world' = world {_player = player'} :: World

goldCmd :: Monad m => ConsoleCommand m
goldCmd args world = return $ case readMaybe' (args0 args) :: Maybe Int of
    Nothing         -> NoUpdateResult "invalid gold amount"
    Just goldAmount ->
        let
            goldValue     = GoldValue goldAmount
            player        = _player (world :: World)
            player'       = player {_gold = goldValue}
            statsManager  = _statsManager world
            statsManager' = statsManager {_acquiredGold = _acquiredGold statsManager + goldValue}
            world'        = world
                { _player       = player'
                , _statsManager = statsManager'
                } :: World
            output        = "gold set to " <> prettyShow goldValue
        in UpdateWorldResult output world'

infiniteMeterCmd :: ConfigsRead m => ConsoleCommand m
infiniteMeterCmd args _ = do
    enable <- case args0 args of
        "0" -> return False
        "1" -> return True
        _   -> not <$> readSettingsConfig _debug _infiniteMeter

    configs <-
        readConfigs <&> \cfgs -> cfgs
            { _settings =
                let settingsCfg = _settings cfgs
                in settingsCfg {_debug = (_debug settingsCfg) {_infiniteMeter = enable}}
            }

    let output = (if enable then "enabled" else "disabled") <> " infinite meter"
    return $ UpdateConfigsResult output configs

meterCmd :: ConfigsRead m => ConsoleCommand m
meterCmd args world = case args0 args of
    "inf"      -> infiniteMeterCmd [] world
    "infinite" -> infiniteMeterCmd [] world

    arg ->
        let
            player      = _player (world :: World)
            playerMeter = _meter player
        in return $ if
            | Just n <- readMaybe' arg ->
                let
                    maxMeterValInt = _int (_maxValue (playerMeter :: PlayerMeter) :: MeterValue)
                    n'             = max 0 (min maxMeterValInt n)
                    meterVal       = MeterValue n'

                    output = "set player meter to " <> prettyShow n'
                    world' = (world :: World)
                        { _player = player {_meter = playerMeter {_value = meterVal}}
                        }
                in UpdateWorldResult output world'

            | otherwise ->
                let output = "invalid meter value: " <> args0 args
                in NoUpdateResult output

quitCmd :: (GraphicsReadWrite m, MonadIO m) => ConsoleCommand m
quitCmd _ _ = freeGraphicsAndExit

progressLayeredMusicCmd :: MonadIO m => ConsoleCommand m
progressLayeredMusicCmd _ world = do
    let audio            = _audio (world :: World)
    layeredMusicManager <- progressLayeredMusicManagerMusic $ _layeredMusicManager audio
    let world'           = world {_audio = audio {_layeredMusicManager = layeredMusicManager}} :: World

    let output = prettyShow $ _nowPlaying layeredMusicManager
    return $ UpdateWorldResult output world'

reloadConfigsCmd :: MonadIO m => ConsoleCommand m
reloadConfigsCmd _ _ = UpdateConfigsResult <$> pure "reloaded configs" <*> mkConfigs

cameraOffsetCmd :: ConfigsRead m => ConsoleCommand m
cameraOffsetCmd args _ =
    let
        offsetX = args0 args
        offsetY = args1 args
        offset  = Pos2 <$> (readMaybe' offsetX :: Maybe Float) <*> (readMaybe' offsetY :: Maybe Float)
    in case offset of
        Nothing      -> return $ NoUpdateResult "invalid offset"
        Just offset' -> do
            configs <- readConfigs <&> \cfgs -> cfgs
                { _settings =
                    let settingsCfg = _settings cfgs
                    in settingsCfg {_debug = (_debug settingsCfg) {_cameraDebugOffset = offset'}}
                }

            let output = "updated camera debug offset to " <> prettyShow offset'
            return $ UpdateConfigsResult output configs

cameraPositionCmd :: Monad m => ConsoleCommand m
cameraPositionCmd _ world =
    let
        player         = _player (world :: World)
        level          = _level (world :: World)
        room           = _room level
        worldCameraPos = calculateWorldCameraPos player room zeroPos2 zeroPos2 (_camera world)
    in do
        let output = "camera position: " <> formatPos2Int worldCameraPos
        return $ NoUpdateResult output

defaultGraphicsFallbackCmd :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ConsoleCommand m
defaultGraphicsFallbackCmd _ _ = doesSaveFilesSettingsExist >>= \case
    True  -> return $ NoUpdateResult ""
    False -> (applyFallbackRenderConfigConsole =<< readConfigs) >>= \case
        Nothing    -> return $ NoUpdateResult ""
        Just cfgs' ->
            let output = "default graphics fallback to native fullscreen borderless"
            in return $ UpdateConfigsResult output cfgs'

saveCmd :: (ConfigsRead m, MonadIO m) => ConsoleCommand m
saveCmd args _ = case args0 args of
    "settings" -> (writeSaveFilesSettings =<< readConfigs) <&> \case
        Left errText -> NoUpdateResult errText
        Right ()     -> NoUpdateResult "wrote settings save file"
    ""         -> return $ NoUpdateResult "no save specified"
    _          -> return $ NoUpdateResult "invalid save specified"

loadCmd :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ConsoleCommand m
loadCmd args _ = case args0 args of
    "settings" -> (readSaveFilesSettings =<< readConfigs) >>= \case
        Left errText -> return $ NoUpdateResult errText
        Right cfgs   -> do
            cfgs' <-
                applyRenderConfigConsole (_render (_settings cfgs :: SettingsConfig)) cfgs >>=
                applyAudioConfigConsole (_audio (_settings cfgs :: SettingsConfig))
            return $ UpdateConfigsResult "loaded settings save file" cfgs'

    "progress" -> (readSaveFilesProgress =<< readConfigs) <&> \case
        Left errText -> NoUpdateResult errText
        Right cfgs   ->
            let cfgs' = applyProgressConfigConsole (_progress cfgs) cfgs
            in UpdateConfigsResult "loaded progress save file" cfgs'

    "" -> return $ NoUpdateResult "no load specified"
    _  -> return $ NoUpdateResult "invalid load specified"

warpOutCmd :: Monad m => ConsoleCommand m
warpOutCmd _ world = return $ UpdateWorldResult "player warping out" world'
    where
        player  = _player (world :: World)
        flags   = _flags (player :: Player)
        player' = player {_flags = flags {_warpingOut = True}} :: Player
        world'  = world {_player = player'} :: World

exploreMusicCmd :: ConfigsRead m => ConsoleCommand m
exploreMusicCmd args _ = case exploreMusicType of
    Nothing        -> return $ NoUpdateResult "invalid music track"
    Just musicType -> do
        cfgs <- setExplorationMusicConsole musicType <$> readConfigs
        return $ UpdateConfigsResult ("set explore music track to " <> prettyShow musicType) cfgs
    where
        exploreMusicType = case T.toLower (args0 args) of
            "a" -> Just ExploreAMusic
            "b" -> Just ExploreBMusic
            "c" -> Just ExploreCMusic
            _   -> Nothing

battleMusicCmd :: ConfigsRead m => ConsoleCommand m
battleMusicCmd args _ = case battleMusicType of
    Nothing        -> return $ NoUpdateResult "invalid music track"
    Just musicType -> do
        cfgs <- setBattleMusicConsole musicType <$> readConfigs
        return $ UpdateConfigsResult ("set battle music track to " <> prettyShow musicType) cfgs
    where
        battleMusicType = case T.toLower (args0 args) of
            "a" -> Just BattleAMusic
            "b" -> Just BattleBMusic
            "c" -> Just BattleCMusic
            _   -> Nothing

reloadSoundsCmd :: (ConfigsRead m, MonadIO m) => ConsoleCommand m
reloadSoundsCmd _ world =
    let
        worldAudio = _audio (world :: World)
        world'     = world
            { _audio = worldAudio {_soundContinuousHashedIds = S.empty}
            } :: World
    in do
        reloadFmodSounds
        setFmodSoundVolume =<< readSettingsConfig _audio _soundVolume

        return $ UpdateWorldResult "reloaded sounds" world'

dangerValueCmd :: MonadIO m => ConsoleCommand m
dangerValueCmd args world = return $ case args0 args of
    dangerValue
        | Just dv <- DangerValue <$> readMaybe' dangerValue ->
            let
                world' = (world :: World)
                    { _level = level {_currentDangerValue = dv}
                    }
            in UpdateWorldResult ("set level to " <> prettyShow dv) world'

    "" -> NoUpdateResult $ "currently: " <> prettyShow (_currentDangerValue level)
    _  -> NoUpdateResult $ "invalid danger value"

    where level = _level (world :: World)

arenaCmd :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => ConsoleCommand m
arenaCmd args world =
    let
        playWorldAudioSound' :: (ConfigsRead m1, MonadIO m1) => FilePath -> Pos2 -> m1 ()
        playWorldAudioSound' soundPath pos = do
            hashedId <- hashId <$> newId
            let audio = _audio (world :: World)
            void $ playWorldAudioSound soundPath hashedId (Just pos) audio

        level = _level (world :: World)
        room  = _room level
    in case _arenaWalls (room :: Room) of
        Nothing         -> return $ NoUpdateResult "no arena walls in current room, must be arena room"
        Just arenaWalls -> case T.toLower (args0 args) of
            "down" -> do
                let
                    leftWallPos  = roomArenaWallsLeftWallPos arenaWalls
                    rightWallPos = roomArenaWallsRightWallPos arenaWalls

                playWorldAudioSound' wallDisappearSoundPath leftWallPos
                playWorldAudioSound' wallDisappearSoundPath rightWallPos
                wallDisappearParticles <- sequenceA
                    [ loadSimpleParticle leftWallPos RightDir levelArenaWallsZIndex wallDisappearSprPath
                    , loadSimpleParticle rightWallPos RightDir levelArenaWallsZIndex wallDisappearSprPath
                    ]

                let
                    particleMgr  = _particleManager world
                    particleMgr' = particleMgr {_particles = wallDisappearParticles ++ _particles particleMgr}
                    arenaWalls'  = arenaWalls {_status = WallsDoneStatus} :: RoomArenaWalls

                    world' = (world :: World)
                        { _level           = level {_room = (room :: Room) {_arenaWalls = Just arenaWalls'}}
                        , _particleManager = particleMgr'
                        }
                return $ UpdateWorldResult "lowering arena walls down" world'

            "up" -> do
                cfg <- (_level :: Configs -> LevelConfig) <$> readConfigs
                let
                    dangerValue  = case readMaybe' (args1 args) of
                        Nothing  -> Just $ _currentDangerValue level
                        Just idx -> maybe Nothing (Just . _maxDangerValue) (_arenaWallsMaxWidths cfg !!? idx)

                roomJSON     <- decodeEither' <$> readFileCache (roomTypeToFilePath $ _type (room :: Room))
                let wallsJSON = (_arenaWalls :: RoomJSON -> Maybe RoomArenaWallsJSON) <$> roomJSON

                case (dangerValue, wallsJSON) of
                    (Nothing, _)                        ->
                        let rangeTxt = T.pack $ show [0..length (_arenaWallsMaxWidths cfg) - 1]
                        in return $ NoUpdateResult ("invalid arena size specified, must be one of: " <> rangeTxt)
                    (_, Left e)                         ->
                        return $ NoUpdateResult ("failed to load current room info:" <> T.pack (show e))
                    (_, Right Nothing)                  -> return $ NoUpdateResult ("missing current room arena info")
                    (Just dangerVal, Right (Just json)) -> do
                        let
                            (leftWall, rightWall) = mkRoomArenaWallsLeftRightWalls dangerVal cfg json
                            arenaWalls'           = arenaWalls
                                { _status     = WallsActiveStatus (-99999.0)  -- hack to prevent arena enemy spawns
                                , _leftWall   = leftWall
                                , _rightWall  = rightWall
                                , _wallSprite = _wallAppear $ _sprites (arenaWalls :: RoomArenaWalls)
                                }

                        rampFmodMusicWorldToNormalVolume
                        playWorldAudioSound' wallAppearSoundPath (roomArenaWallsLeftWallPos arenaWalls')
                        playWorldAudioSound' wallAppearSoundPath (roomArenaWallsRightWallPos arenaWalls')

                        let
                            world' = (world :: World)
                                { _level = level
                                    {_room = (room :: Room)
                                        { _arenaWalls = Just arenaWalls'
                                        , _bounds     = (_bounds room) {_arenaWallsBounds = Nothing}
                                        }
                                    }
                                }
                        return $ UpdateWorldResult "raising arena walls up" world'

            _ -> return $ NoUpdateResult "invalid arena option, must specify up/down"

shopCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
shopCmd args world =
    let
        level    = _level (world :: World)
        room     = _room level
        roomType = _type (room :: Room)

        setRoomItems :: [Some RoomItem] -> World
        setRoomItems items = (world :: World)
            { _level = level
                { _room = room {_items = items}
                }
            }
    in case args0 args of
        _
            | not (isTransitionRoomType roomType) -> return $
                NoUpdateResult "invalid current room type, must be a (to-/from-) transition room"

        "up" -> decodeEither' <$> readFileCache (roomTypeToFilePath roomType) >>= \case
            Left e         -> return $ NoUpdateResult ("failed to load current room info:" <> T.pack (show e))
            Right roomJSON -> do
                let playerInfo = mkPlayerInfo $ _player (world :: World)
                itemPickups   <- loadRoomItemsShop roomJSON roomType playerInfo (_statsManager world)
                return $ UpdateWorldResult "shop up" (setRoomItems itemPickups)

        "down" -> return $ UpdateWorldResult "shop down" (setRoomItems [])
        _      -> return $ NoUpdateResult "invalid shop option, must specify up/down"

eventCmd :: ConsoleCommand (AppEnv ConsoleMsgsPhase)
eventCmd args world =
    let
        level    = _level (world :: World)
        room     = _room level
        roomType = _type (room :: Room)
    in if
        | not (isTransitionRoomType roomType) -> return $
            NoUpdateResult "invalid current room type, must be a (to-/from-) transition room"

        | otherwise ->
            let
                eventType
                    | args0 args `elem` ["lightningStrike", "lightning"] = Just LightningStrikeEvent
                    | args0 args `elem` ["bouncingBall", "ball"]         = Just BouncingBallEvent
                    | args0 args `elem` ["slotMachine", "slots", "slot"] = Just SlotMachineEvent
                    | otherwise                                          = Nothing
            in case eventType of
                Nothing         -> return $ NoUpdateResult "invalid event type"
                Just eventType' -> do
                    decodeEither' <$> readFileCache (roomTypeToFilePath roomType) >>= \case
                        Left e         ->
                            return $ NoUpdateResult ("failed to load current room info:" <> T.pack (show e))
                        Right roomJSON -> do
                            eventActivators <- loadRoomItemEvent roomJSON eventType'
                            let
                                world' = (world :: World)
                                    { _level = level {_room = room {_items = eventActivators}}
                                    }
                            return $ UpdateWorldResult ("event " <> prettyShow eventType') world'

runCmd :: Monad m => ConsoleCommand m
runCmd args _ = return $ case args0 args of
    ""       -> NoUpdateResult "invalid, must specify file name"
    fileName -> RunFileResult $ T.unpack ("data/" <> fileName)

directionCmd :: Monad m => ConsoleCommand m
directionCmd args world =
    let
        dirTxt = T.toLower $ args0 args
        dir    = case dirTxt of
            "left"  -> Just LeftDir
            "right" -> Just RightDir
            _       -> Nothing
    in return $ case dir of
        Nothing   -> NoUpdateResult "invalid direction"
        Just dir' ->
            let
                output = "set player direction: " <> dirTxt
                player = _player (world :: World)
                world' = world {_player = player {_dir = dir'}} :: World
            in UpdateWorldResult output world'

bindCmd :: Monad m => ConsoleCommand m
bindCmd args _ = return $ if
    | null args        -> PrintCmdBindsResult
    | length args /= 2 -> NoUpdateResult "invalid, must be in the form: bind <key> \"<cmd>\""
    | otherwise        ->
        let
            cmdLine         = args1 args
            bindExecResult' = \(SDL.Scancode code) -> BindCmdResult (KeyRawData code) cmdLine
        in case T.toLower (args0 args) of
            ""      -> NoUpdateResult "invalid, must specify key to bind"
            "f1"    -> bindExecResult' SDL.ScancodeF1
            "f2"    -> bindExecResult' SDL.ScancodeF2
            "f3"    -> bindExecResult' SDL.ScancodeF3
            "f4"    -> bindExecResult' SDL.ScancodeF4
            "f5"    -> bindExecResult' SDL.ScancodeF5
            "f6"    -> bindExecResult' SDL.ScancodeF6
            "f7"    -> bindExecResult' SDL.ScancodeF7
            "f8"    -> bindExecResult' SDL.ScancodeF8
            "f9"    -> bindExecResult' SDL.ScancodeF9
            "f10"   -> bindExecResult' SDL.ScancodeF10
            "f11"   -> bindExecResult' SDL.ScancodeF11
            "f12"   -> bindExecResult' SDL.ScancodeF12
            "guide" ->
                let inputRawData = GamepadButtonRawData (SDL.Internal.toNumber SDL.ControllerButtonGuide)
                in BindCmdResult inputRawData cmdLine
            _       -> NoUpdateResult "invalid key to bind"

devConsoleCommand :: ConfigsRead m => ConsoleCommand m
devConsoleCommand _ _ = do
    cfgs <- readConfigs
    let
        cfgs' = cfgs
            { _settings =
                let settingsCfg = _settings cfgs
                in settingsCfg {_debug = (_debug settingsCfg) {_devConsoleEnabled = True}}
            }

    return $ UpdateConfigsResult "enabled developer console" cfgs'

arenaTriggerCmd :: ConfigsRead m => ConsoleCommand m
arenaTriggerCmd args _ = do
    cfgs <- readConfigs
    let
        settingsCfg = _settings cfgs
        debugCfg    = _debug settingsCfg

        disable = case args0 args of
            "0" -> True
            "1" -> False
            _   -> not $ _disableRoomArenaWallsTrigger debugCfg

        cfgs' = cfgs
            { _settings = settingsCfg
                { _debug = debugCfg {_disableRoomArenaWallsTrigger = disable}
                }
            }

        output = (if disable then "disabled" else "enabled") <> " arena trigger"

    return $ UpdateConfigsResult output cfgs'

bossTriggerCmd :: forall m. ConfigsRead m => ConsoleCommand m
bossTriggerCmd args _ = do
    cfgs <- readConfigs
    let
        settingsCfg = _settings cfgs
        debugCfg    = _debug settingsCfg

        disable = case args0 args of
            "0" -> True
            "1" -> False
            _   -> not $ _disableRoomBossTrigger debugCfg

        cfgs' = cfgs
            { _settings = settingsCfg
                { _debug = debugCfg {_disableRoomBossTrigger = disable}
                }
            }

        output = (if disable then "disabled" else "enabled") <> " room boss trigger"

    return $ UpdateConfigsResult output cfgs'

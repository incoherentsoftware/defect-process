module Player.Weapon.All.Scythe.Data
    ( ScytheAttackDescriptions(..)
    , PlayerSummonMoves(..)
    , PlayerScytheAttackDescriptions(..)
    , FloatingAttackStatus(..)
    , ScytheData(..)
    , mkScytheData
    , playerSummonMovesOffsetPos
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Configs
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.Scythe
import FileCache
import Msg
import Util
import Window.Graphics

packFilePath = "data/player/weapons/scythe.pack" :: FilePath

loadAtkDesc :: (FileCache m, GraphicsRead m, MonadIO m) => FilePath -> m AttackDescription
loadAtkDesc f = loadPackAttackDescription $ PackResourceFilePath packFilePath f

data ScytheAttackDescriptions = ScytheAttackDescriptions
    { _multiSlash1              :: AttackDescription
    , _multiSlash2              :: AttackDescription
    , _multiSlash3              :: AttackDescription
    , _multiSlash4              :: AttackDescription
    , _multiSlash5              :: AttackDescription
    , _multiSlashGlow           :: AttackDescription
    , _multiSlashGlowOverlay    :: AttackDescription
    , _shatterLaunch            :: AttackDescription
    , _vertSlash                :: AttackDescription
    , _vertSlashLand            :: AttackDescription
    , _pullSlash                :: AttackDescription
    , _pullSlashGlow            :: AttackDescription
    , _pullSlashGlowOverlay     :: AttackDescription
    , _riseSlash                :: AttackDescription
    , _riseSlashGlow            :: AttackDescription
    , _riseSlashGlowOverlay     :: AttackDescription
    , _vertSpinSlash            :: AttackDescription
    , _vertSpinSlashGlow        :: AttackDescription
    , _vertSpinSlashGlowOverlay :: AttackDescription
    , _diagSpinSlash            :: AttackDescription
    , _diagSpinSlashGlow        :: AttackDescription
    , _diagSpinSlashGlowOverlay :: AttackDescription
    , _recallSlash1             :: AttackDescription
    , _recallSlash2             :: AttackDescription
    , _recallSlash3             :: AttackDescription
    }

mkScytheAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m ScytheAttackDescriptions
mkScytheAttackDescs =
    ScytheAttackDescriptions <$>
    loadAtkDesc "multi-slash1-.atk" <*>
    loadAtkDesc "multi-slash2-.atk" <*>
    loadAtkDesc "multi-slash3-.atk" <*>
    loadAtkDesc "multi-slash4-.atk" <*>
    loadAtkDesc "multi-slash5-.atk" <*>
    loadAtkDesc "multi-slash-glow.atk" <*>
    loadAtkDesc "multi-slash-glow-overlay.atk" <*>
    loadAtkDesc "shatter-launch.atk" <*>
    loadAtkDesc "vert-slash.atk" <*>
    loadAtkDesc "vert-slash-land.atk" <*>
    loadAtkDesc "pull-slash.atk" <*>
    loadAtkDesc "pull-slash-glow.atk" <*>
    loadAtkDesc "pull-slash-glow-overlay.atk" <*>
    loadAtkDesc "rise-slash.atk" <*>
    loadAtkDesc "rise-slash-glow.atk" <*>
    loadAtkDesc "rise-slash-glow-overlay.atk" <*>
    loadAtkDesc "vert-spin-slash.atk" <*>
    loadAtkDesc "vert-spin-slash-glow.atk" <*>
    loadAtkDesc "vert-spin-slash-glow-overlay.atk" <*>
    loadAtkDesc "diag-spin-slash.atk" <*>
    loadAtkDesc "diag-spin-slash-glow.atk" <*>
    loadAtkDesc "diag-spin-slash-glow-overlay.atk" <*>
    loadAtkDesc "recall-slash1-.atk" <*>
    loadAtkDesc "recall-slash2-.atk" <*>
    loadAtkDesc "recall-slash3-.atk"

data PlayerSummonMoves = PlayerSummonMoves
    { _forwards     :: AttackDescription
    , _longForwards :: AttackDescription
    , _upwards      :: AttackDescription
    , _away         :: AttackDescription
    , _downwards    :: AttackDescription
    , _towards      :: AttackDescription
    , _closeFist    :: AttackDescription
    , _airForwards  :: AttackDescription
    , _airDownwards :: AttackDescription
    , _airDiagonal  :: AttackDescription
    , _airCloseFist :: AttackDescription
    , _airTowards   :: AttackDescription
    }

mkPlayerSummonMoves :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerSummonMoves
mkPlayerSummonMoves =
    PlayerSummonMoves <$>
    loadAtkDesc "player-summon-forwards.atk" <*>
    loadAtkDesc "player-long-summon-forwards.atk" <*>
    loadAtkDesc "player-summon-upwards.atk" <*>
    loadAtkDesc "player-summon-away.atk" <*>
    loadAtkDesc "player-summon-downwards.atk" <*>
    loadAtkDesc "player-summon-towards.atk" <*>
    loadAtkDesc "player-summon-close-fist.atk" <*>
    loadAtkDesc "player-air-summon-forwards.atk" <*>
    loadAtkDesc "player-air-summon-downwards.atk" <*>
    loadAtkDesc "player-air-summon-diag.atk" <*>
    loadAtkDesc "player-air-summon-close-fist.atk" <*>
    loadAtkDesc "player-air-summon-towards.atk"

data PlayerScytheAttackDescriptions = PlayerScytheAttackDescriptions
    { _bladeSlash1    :: AttackDescription
    , _bladeSlash2    :: AttackDescription
    , _airBladeSlash1 :: AttackDescription
    , _airBladeSlash2 :: AttackDescription
    , _blinkSlash1    :: AttackDescription
    , _blinkSlash2    :: AttackDescription
    }

mkPlayerScytheAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerScytheAttackDescriptions
mkPlayerScytheAttackDescs =
    PlayerScytheAttackDescriptions <$>
    loadAtkDesc "blade-slash1-.atk" <*>
    loadAtkDesc "blade-slash2-.atk" <*>
    loadAtkDesc "air-blade-slash1-.atk" <*>
    loadAtkDesc "air-blade-slash2-.atk" <*>
    loadAtkDesc "blink-slash1-.atk" <*>
    loadAtkDesc "blink-slash2-.atk"

data FloatingAttackStatus
    = FloatingAttackInactive
    | FloatingAttackActive MsgId Secs
    | FloatingAttackActiveReady MsgId Pos2 Direction Secs
    deriving Show

data ScytheData = ScytheData
    { _floatingAttackStatus    :: FloatingAttackStatus
    , _chargeHeld              :: Secs
    , _scytheAttackDescs       :: ScytheAttackDescriptions
    , _playerSummonMoves       :: PlayerSummonMoves
    , _playerScytheAttackDescs :: PlayerScytheAttackDescriptions
    , _config                  :: ScytheConfig
    }

mkScytheData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m ScytheData
mkScytheData = do
    scytheAtkDescs       <- mkScytheAttackDescs
    playerSummonMoves    <- mkPlayerSummonMoves
    playerScytheAtkDescs <- mkPlayerScytheAttackDescs
    cfg                  <- readConfig _playerWeapon _scythe

    return $ ScytheData
        { _floatingAttackStatus    = FloatingAttackInactive
        , _chargeHeld              = 0.0
        , _scytheAttackDescs       = scytheAtkDescs
        , _playerSummonMoves       = playerSummonMoves
        , _playerScytheAttackDescs = playerScytheAtkDescs
        , _config                  = cfg
        }

playerSummonMovesOffsetPos :: ScytheData -> AttackDescription -> Pos2 -> Direction -> Pos2
playerSummonMovesOffsetPos scytheData summonMoveDesc pos dir = pos `vecAdd` offset
    where
        summonMoves = _playerSummonMoves scytheData
        cfg         = _config (scytheData :: ScytheData)

        Pos2 offsetX offsetY
            | summonMoveDesc == _forwards summonMoves     = _summonForwardsOffset cfg
            | summonMoveDesc == _airForwards summonMoves  = _summonForwardsOffset cfg
            | summonMoveDesc == _airDiagonal summonMoves  = _summonAirDiagonalOffset cfg
            | summonMoveDesc == _longForwards summonMoves = _summonLongForwardsOffset cfg
            | summonMoveDesc == _upwards summonMoves      = _summonUpwardsOffset cfg
            | summonMoveDesc == _away summonMoves         = _summonAwayOffset cfg
            | otherwise                                   = zeroPos2

        offset = Pos2 (offsetX * directionNeg dir) offsetY

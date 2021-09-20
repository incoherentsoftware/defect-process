module World.Types
    ( WorldStatus(..)
    , World(..)
    ) where

import AppEnv.Types
import Enemy.Manager.Types
import Level.Types
import Msg.Phase
import Particle.Manager.Types
import Player.Types
import Projectile.Manager.Types
import Stats.Manager
import Util
import World.Audio.Types
import World.Camera.Types
import World.RunProgressScreen
import World.ScreenWipe
import World.Screenshake.Types
import World.UI.Types

data WorldStatus
    = WorldInitialStatus
    | WorldAliveStatus
    | WorldDeadStatus
    deriving Eq

data World = World
    { _player            :: Player
    , _level             :: Level
    , _enemyManager      :: EnemyManager
    , _projectileManager :: ProjectileManager
    , _particleManager   :: ParticleManager
    , _audio             :: WorldAudio
    , _statsManager      :: StatsManager
    , _ui                :: WorldUI
    , _runProgressScreen :: RunProgressScreen
    , _hitlagTtl         :: Secs
    , _screenshake       :: Screenshake
    , _camera            :: WorldCamera
    , _status            :: WorldStatus
    , _levelLoadSecs     :: Secs
    , _screenWipe        :: Maybe WorldScreenWipe
    , _pendingChange     :: Maybe (World -> AppEnv UpdateWorldMsgsPhase World)
    }

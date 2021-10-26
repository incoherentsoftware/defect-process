module Enemy.Types
    ( EnemyType(..)
    , enemyTypeFromName
    , EnemyDummyType(..)
    , EnemyUpdateAI
    , EnemyHitbox
    , EnemyPullable
    , EnemyUpdateHurtResponse
    , EnemyUpdateGroundResponse
    , EnemyUpdateHangtimeResponse
    , EnemyUpdateSprite
    , EnemyUpdateDynamic
    , EnemyDraw
    , EnemyDrawOverlay
    , EnemySetDebugBehavior
    , Enemy(..)
    ) where

import Data.Aeson.Types (FromJSON, Parser, Value(String), parseJSON, typeMismatch)
import Data.Dynamic     (Dynamic)
import qualified Data.Set as S
import qualified Data.Text as T

import AppEnv.Types
import Attack.Hit.Types
import Attack.Types
import Attack.Util
import Collision
import Configs.All.Settings.Debug
import Enemy.DebugText
import Enemy.Flags
import Id
import InfoMsg.Util
import Msg.Types
import Util
import Window.Graphics
import {-# SOURCE #-} Enemy.Util

data EnemyType
    = AxeEnemy
    | FlyingEnemy
    | GiantEnemy
    | HammerEnemy
    | DogEnemy
    | BlobEnemy
    | SpearEnemy
    | TurretEnemy
    | BubbleTurretEnemy
    | LankyEnemy
    | BatEnemy
    | BombEnemy
    | ClawsEnemy
    | FlailEnemy
    | ZombieEnemy
    | WallEnemy
    | HopEnemy
    | BossEnemy
    deriving (Eq, Ord, Show)

instance FromJSON EnemyType where
    parseJSON :: Value -> Parser EnemyType
    parseJSON value@(String v) = case enemyTypeFromName v of
        Nothing     -> typeMismatch "EnemyType" value
        Just enType -> return enType
    parseJSON value            = typeMismatch "EnemyType" value

enemyTypeFromName :: T.Text -> Maybe EnemyType
enemyTypeFromName = \case
    "axe"          -> Just AxeEnemy
    "flying"       -> Just FlyingEnemy
    "giant"        -> Just GiantEnemy
    "hammer"       -> Just HammerEnemy
    "dog"          -> Just DogEnemy
    "blob"         -> Just BlobEnemy
    "spear"        -> Just SpearEnemy
    "turret"       -> Just TurretEnemy
    "bubbleTurret" -> Just BubbleTurretEnemy
    "lanky"        -> Just LankyEnemy
    "bat"          -> Just BatEnemy
    "bomb"         -> Just BombEnemy
    "claws"        -> Just ClawsEnemy
    "flail"        -> Just FlailEnemy
    "zombie"       -> Just ZombieEnemy
    "wall"         -> Just WallEnemy
    "hop"          -> Just HopEnemy
    "boss"         -> Just BossEnemy
    _              -> Nothing

data EnemyDummyType
    = EnemyDummyType  -- dummy enemies only interact w/ player attacks
    | EnemyRealType
    deriving Eq

type EnemyUpdateAI d m             = Enemy d -> m [Msg ThinkEnemyMsgsPhase]
type EnemyHitbox d                 = Enemy d -> Hitbox
type EnemyPullable d               = Enemy d -> Bool
type EnemyUpdateHurtResponse d m   = AttackHit -> Enemy d -> m (Enemy d)
type EnemyUpdateGroundResponse d m = PosY -> Enemy d -> m (Enemy d)
type EnemyUpdateHangtimeResponse d = Secs -> Enemy d -> Enemy d
type EnemyUpdateSprite d           = Enemy d -> Enemy d
type EnemyUpdateDynamic d m        = Dynamic -> Enemy d -> m (Enemy d)
type EnemyDraw d m                 = Enemy d -> m ()
type EnemyDrawOverlay d m          = Enemy d -> m ()
type EnemySetDebugBehavior d       = [T.Text] -> Enemy d -> (T.Text, Enemy d)

data Enemy d = Enemy
    { _data                   :: d
    , _dummyType              :: EnemyDummyType
    , _msgId                  :: MsgId
    , _pos                    :: Pos2
    , _vel                    :: Vel2
    , _dir                    :: Direction
    , _health                 :: Health
    , _launchTargetY          :: Maybe Float
    , _hitByHashedIds         :: S.Set HashedId
    , _sprite                 :: Maybe Sprite
    , _attack                 :: Maybe Attack
    , _hitbox                 :: EnemyHitbox d
    , _pullable               :: EnemyPullable d
    , _lockOnReticleData      :: EnemyLockOnReticleData
    , _deathEffectData        :: EnemyDeathEffectData
    , _spawnEffectData        :: EnemySpawnEffectData
    , _knownPlayerInfo        :: Maybe PlayerInfo
    , _flags                  :: EnemyFlags
    , _thinkAI                :: EnemyUpdateAI d (AppEnv ThinkEnemyMsgsPhase)
    , _updateHurtResponse     :: EnemyUpdateHurtResponse d (AppEnv UpdateEnemyMsgsPhase)
    , _updateGroundResponse   :: EnemyUpdateGroundResponse d (AppEnv UpdateEnemyMsgsPhase)
    , _updateHangtimeResponse :: EnemyUpdateHangtimeResponse d
    , _updateSprite           :: EnemyUpdateSprite d
    , _updateDynamic          :: EnemyUpdateDynamic d (AppEnv UpdateEnemyMsgsPhase)
    , _draw                   :: Maybe (EnemyDraw d (AppEnv DrawMsgsPhase))
    , _drawOverlay            :: EnemyDrawOverlay d (AppEnv DrawMsgsPhase)
    , _setDebugBehavior       :: EnemySetDebugBehavior d
    , _bodyZIndex             :: ZIndex
    , _debugText              :: Maybe EnemyDebugText
    , _debugConfig            :: DebugConfig
    }

instance CollisionEntity (Enemy d) where
    collisionEntityMsgId :: Enemy d -> MsgId
    collisionEntityMsgId = _msgId

    collisionEntityHitbox :: Enemy d -> Hitbox
    collisionEntityHitbox e = (_hitbox (e :: Enemy d)) e

    collisionEntityPrevHitbox :: Enemy d -> Hitbox
    collisionEntityPrevHitbox = collisionEntityHitbox  -- TODO

    collisionEntityVel :: Enemy d -> Vel2
    collisionEntityVel = _vel

    collisionEntityDir :: Enemy d -> Direction
    collisionEntityDir = _dir

    collisionEntityHitByHashedIds :: Enemy d -> S.Set HashedId
    collisionEntityHitByHashedIds = _hitByHashedIds

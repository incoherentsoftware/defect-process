module Enemy.All.Boss.Data
    ( SummonAttackDesc(..)
    , HpThresholdAttackData(..)
    , BossEnemyData(..)
    , mkBossEnemyData
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random.Shuffle  (shuffleM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Attack
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Boss
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Behavior
import Enemy.All.Boss.Images
import Enemy.All.Boss.Sprites
import FileCache
import Util
import Window.Graphics

hpThresholdMultipliers = [0.66, 0.33] :: [Float]

data SummonAttackDesc = SummonAttackDesc
    { _summonIn  :: AttackDescription
    , _summonOut :: AttackDescription
    }

data HpThresholdAttackData = HpThresholdAttackData
    { _phaseOutPos             :: Pos2
    , _thresholds              :: M.Map Damage (Maybe SummonAttackDesc)
    , _summonAttackDesc        :: SummonAttackDesc
    , _summonAttackDescChoices :: NE.NonEmpty SummonAttackDesc
    }

mkHpThresholdAttackData :: MonadIO m => Pos2 -> EnemyAttackDescriptions -> EnemyConfig -> m HpThresholdAttackData
mkHpThresholdAttackData pos atkDescs cfgs =
    let
        maxHealth  = _maxValue . _health . _boss $ cfgs
        thresholds = M.fromList
            [ (damage, Nothing)
            | m <- hpThresholdMultipliers
            , let damage = Damage $ round (fromIntegral maxHealth * m)
            ]

        summonAtkDescChoices =
            [ SummonAttackDesc (_summonFlyingIn atkDescs) (_summonFlyingOut atkDescs)
            , SummonAttackDesc (_summonSpearsIn atkDescs) (_summonSpearsOut atkDescs)
            , SummonAttackDesc (_summonWallsIn atkDescs) (_summonWallsOut atkDescs)
            ]
    in do
        summonAtkDescChoices' <- NE.fromList <$> liftIO (shuffleM summonAtkDescChoices)

        return $ HpThresholdAttackData
            { _phaseOutPos             = pos
            , _thresholds              = thresholds
            , _summonAttackDesc        = NE.head summonAtkDescChoices'
            , _summonAttackDescChoices = summonAtkDescChoices'
            }

data BossEnemyData = BossEnemyData
    { _incapacitatedSecs      :: Secs
    , _knownInnerLeftWallX    :: PosX
    , _knownInnerRightWallX   :: PosX
    , _attackShortChoices     :: NE.NonEmpty AttackDescription
    , _attackMediumAirChoices :: NE.NonEmpty AttackDescription
    , _attackLongChoices      :: NE.NonEmpty AttackDescription
    , _sprites                :: EnemySprites
    , _images                 :: EnemyImages
    , _attackDescs            :: EnemyAttackDescriptions
    , _behavior               :: BossEnemyBehavior
    , _prevBehavior           :: BossEnemyBehavior
    , _hpThresholdAttackData  :: HpThresholdAttackData
    , _config                 :: EnemyConfig
    }

mkBossEnemyData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m BossEnemyData
mkBossEnemyData pos@(Pos2 x _) = do
    sprs                  <- mkEnemySprites
    imgs                  <- mkEnemyImages
    atkDescs              <- mkEnemyAttackDescs
    cfg                   <- _enemy <$> readConfigs
    hpThresholdAttackData <- mkHpThresholdAttackData pos atkDescs cfg

    let
        initialBehavior
            | _skipSpawnAnim (_boss cfg) = IdleBehavior 0.0
            | otherwise                  = SpawnBehavior

        attackShortChoices = NE.fromList
            [ _giant (atkDescs :: EnemyAttackDescriptions)
            , _hammer (atkDescs :: EnemyAttackDescriptions)
            , _dog (atkDescs :: EnemyAttackDescriptions)
            , _hop (atkDescs :: EnemyAttackDescriptions)
            , _flying (atkDescs :: EnemyAttackDescriptions)
            , _lanky (atkDescs :: EnemyAttackDescriptions)
            ]

        attackMediumAirChoices = NE.fromList
            [ _flail1 (atkDescs :: EnemyAttackDescriptions)
            , _hammer (atkDescs :: EnemyAttackDescriptions)
            , _hop (atkDescs :: EnemyAttackDescriptions)
            , _lanky (atkDescs :: EnemyAttackDescriptions)
            ]

        attackLongChoices = NE.fromList
            [ _dog (atkDescs :: EnemyAttackDescriptions)
            , _blob (atkDescs :: EnemyAttackDescriptions)
            , _hammer (atkDescs :: EnemyAttackDescriptions)
            , _turret1 (atkDescs :: EnemyAttackDescriptions)
            , _hop (atkDescs :: EnemyAttackDescriptions)
            , _lanky (atkDescs :: EnemyAttackDescriptions)
            ]

    return $ BossEnemyData
        { _incapacitatedSecs      = 0.0
        , _knownInnerLeftWallX    = x
        , _knownInnerRightWallX   = x
        , _attackShortChoices     = attackShortChoices
        , _attackMediumAirChoices = attackMediumAirChoices
        , _attackLongChoices      = attackLongChoices
        , _sprites                = sprs
        , _images                 = imgs
        , _attackDescs            = atkDescs
        , _behavior               = initialBehavior
        , _prevBehavior           = initialBehavior
        , _hpThresholdAttackData  = hpThresholdAttackData
        , _config                 = cfg
        }

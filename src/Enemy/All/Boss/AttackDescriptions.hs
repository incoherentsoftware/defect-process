module Enemy.All.Boss.AttackDescriptions
    ( EnemyAttackDescriptions(..)
    , mkEnemyAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import FileCache
import Window.Graphics

data EnemyAttackDescriptions = EnemyAttackDescriptions
    { _blob                     :: AttackDescription
    , _blobProjectile           :: AttackDescription
    , _dog                      :: AttackDescription
    , _dogImpact                :: AttackDescription
    , _flail1                   :: AttackDescription
    , _giant                    :: AttackDescription
    , _hammer                   :: AttackDescription
    , _hammerImpact             :: AttackDescription
    , _turret1                  :: AttackDescription
    , _turret2                  :: AttackDescription
    , _hop                      :: AttackDescription
    , _hopProjectile            :: AttackDescription
    , _flying                   :: AttackDescription
    , _lanky                    :: AttackDescription
    , _lankyReappear            :: AttackDescription
    , _lankyProjectileIndicator :: AttackDescription
    , _lankyProjectile          :: AttackDescription
    , _summonFlyingIn           :: AttackDescription
    , _summonFlyingOut          :: AttackDescription
    , _flyingProjectile         :: AttackDescription
    , _summonSpearsIn           :: AttackDescription
    , _summonSpearsOut          :: AttackDescription
    , _spearProjectile          :: AttackDescription
    , _summonWallsIn            :: AttackDescription
    , _summonWallsOut           :: AttackDescription
    , _wallProjectile           :: AttackDescription
    , _phaseIn                  :: AttackDescription
    , _phaseOut                 :: AttackDescription
    }

mkEnemyAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyAttackDescriptions
mkEnemyAttackDescs =
    EnemyAttackDescriptions <$>
    loadPack1AtkDesc "attack-blob.atk" <*>
    loadPack1AtkDesc "attack-blob-projectile.atk" <*>
    loadPack1AtkDesc "attack-dog.atk" <*>
    loadPack1AtkDesc "attack-dog-impact.atk" <*>
    loadPack1AtkDesc "attack-flail1-.atk" <*>
    loadPack1AtkDesc "attack-giant.atk" <*>
    loadPack2AtkDesc "attack-hammer.atk" <*>
    loadPack2AtkDesc "attack-hammer-impact.atk" <*>
    loadPack1AtkDesc "attack-turret1-.atk" <*>
    loadPack1AtkDesc "attack-turret2-.atk" <*>
    loadPack2AtkDesc "attack-hop.atk" <*>
    loadPack2AtkDesc "attack-hop-projectile.atk" <*>
    loadPack2AtkDesc "attack-flying.atk" <*>
    loadPack3AtkDesc "attack-lanky.atk" <*>
    loadPack3AtkDesc "attack-lanky-reappear.atk" <*>
    loadPack3AtkDesc "attack-lanky-projectile-indicator.atk" <*>
    loadPack3AtkDesc "attack-lanky-projectile.atk" <*>
    loadPack3AtkDesc "attack-summon-flying-in.atk" <*>
    loadPack3AtkDesc "attack-summon-flying-out.atk" <*>
    loadPack3AtkDesc "attack-flying-projectile.atk" <*>
    loadPack3AtkDesc "attack-summon-spears-in.atk" <*>
    loadPack3AtkDesc "attack-summon-spears-out.atk" <*>
    loadPack3AtkDesc "attack-spear-projectile.atk" <*>
    loadPack3AtkDesc "attack-summon-walls-in.atk" <*>
    loadPack3AtkDesc "attack-summon-walls-out.atk" <*>
    loadPack3AtkDesc "attack-wall-projectile.atk" <*>
    loadPack3AtkDesc "attack-phase-in.atk" <*>
    loadPack3AtkDesc "attack-phase-out.atk"
    where
        loadPack1AtkDesc = \f ->
            loadPackAttackDescription $ PackResourceFilePath "data/enemies/boss-enemy-attack1.pack" f
        loadPack2AtkDesc = \f ->
            loadPackAttackDescription $ PackResourceFilePath "data/enemies/boss-enemy-attack2.pack" f
        loadPack3AtkDesc = \f ->
            loadPackAttackDescription $ PackResourceFilePath "data/enemies/boss-enemy-attack3.pack" f

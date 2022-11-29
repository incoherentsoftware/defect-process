module Particle.All.AttackSpecks.Util
    ( attackSpecksParticlePaths
    , toAttackSpecksDirection
    ) where

import qualified Data.List.NonEmpty as NE

import FileCache
import Particle.All.AttackSpecks.Types
import Util

packPath = \f -> PackResourceFilePath "data/particles/particles.pack" f

swordParticlePaths = NE.fromList $ map packPath
    [ "sword-a.spr"
    , "sword-b.spr"
    , "sword-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

staffParticlePaths = NE.fromList $ map packPath
    [ "staff-a.spr"
    , "staff-b.spr"
    , "staff-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

gauntletsParticlePaths = NE.fromList $ map packPath
    [ "gauntlets-a.spr"
    , "gauntlets-b.spr"
    , "gauntlets-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

scytheParticlePaths = NE.fromList $ map packPath
    [ "scythe-a.spr"
    , "scythe-b.spr"
    , "scythe-c.spr"
    , "scythe-d.spr"
    , "scythe-e.spr"
    ] :: NE.NonEmpty PackResourceFilePath

spiritBladeParticlePaths = NE.fromList $ map packPath
    [ "spirit-blade-a.spr"
    , "spirit-blade-b.spr"
    , "spirit-blade-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

bulletParticlePaths = NE.fromList $ map packPath
    [ "bullet-a.spr"
    , "bullet-b.spr"
    , "bullet-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

shardParticlePaths = NE.fromList $ map packPath
    [ "shard-a.spr"
    , "shard-b.spr"
    , "shard-c.spr"
    , "shard-d.spr"
    ] :: NE.NonEmpty PackResourceFilePath

spikeParticlePaths = NE.fromList $ map packPath
    [ "spike-a.spr"
    , "spike-b.spr"
    , "spike-c.spr"
    , "spike-d.spr"
    ] :: NE.NonEmpty PackResourceFilePath

ricochetParticlePaths = NE.fromList $ map packPath
    [ "ricochet-a.spr"
    , "ricochet-b.spr"
    ] :: NE.NonEmpty PackResourceFilePath

grenadeParticlePaths = NE.fromList $ map packPath
    [ "grenade-a.spr"
    , "grenade-b.spr"
    ] :: NE.NonEmpty PackResourceFilePath

mineParticlePaths = NE.fromList $ map packPath
    [ "mine-a.spr"
    , "mine-b.spr"
    , "mine-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

goldParticlePaths = NE.fromList $ map packPath
    [ "gold-a.spr"
    , "gold-b.spr"
    , "gold-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

grappleParticlePaths = NE.fromList $ map packPath
    [ "grapple-a.spr"
    , "grapple-b.spr"
    , "grapple-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

attackSpecksParticlePaths :: AttackSpecksType -> NE.NonEmpty PackResourceFilePath
attackSpecksParticlePaths = \case
    SwordSpecksType        -> swordParticlePaths
    StaffSpecksType        -> staffParticlePaths
    GauntletsSpecksType    -> gauntletsParticlePaths
    ScytheSpecksType       -> scytheParticlePaths
    SpiritBladeSpecksType  -> spiritBladeParticlePaths
    BulletSpecksType       -> bulletParticlePaths
    ShardSpecksType        -> shardParticlePaths
    ShardExplodeSpecksType -> shardParticlePaths  -- reuse shard sprites
    SpikeSpecksType        -> spikeParticlePaths
    RicochetSpecksType     -> ricochetParticlePaths
    GrenadeSpecksType      -> grenadeParticlePaths
    MineSpecksType         -> mineParticlePaths
    GoldSpecksType         -> goldParticlePaths
    GrappleSpecksType      -> grappleParticlePaths

toAttackSpecksDirection :: Maybe Direction -> AttackSpecksDirection
toAttackSpecksDirection = \case
    Just LeftDir  -> SpecksLeftDir
    Just RightDir -> SpecksRightDir
    Nothing       -> SpecksAnyDir

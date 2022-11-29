module Level.Room.Item.GoldChunk
    ( module Level.Room.Item.GoldChunk.Types
    , mkGoldChunkSet
    , mkGoldChunk
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (foldlM)
import Data.Maybe             (fromMaybe)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set  as S

import Attack.Hit
import Attack.Util
import Collision
import Constants
import FileCache
import Id
import Level.Room.Item as RI
import Level.Room.Item.GoldChunk.JSON
import Level.Room.Item.GoldChunk.Types
import Msg
import Particle.All.AttackSpecks
import Particle.All.Simple
import Util
import Window.Graphics
import World.GoldDrop
import World.Util
import World.ZIndex

defaultImageFileName = "gold-chunk-a.image" :: FilePath
imageFileNames       =
    [ "gold-chunk-a.image"
    , "gold-chunk-b.image"
    , "gold-chunk-c.image"
    , "gold-chunk-d.image"
    , "gold-chunk-e.image"
    ] :: [FilePath]

packResourceFilePath :: FilePath -> PackResourceFilePath
packResourceFilePath f = PackResourceFilePath "data/levels/level-items.pack" f

hitSoundFilePath = "event:/SFX Events/Level/crate-hit"              :: FilePath
hitEffectPath    = packResourceFilePath "gold-chunk-hit-effect.spr" :: PackResourceFilePath

perCountGoldValue = GoldValue 25 :: GoldValue
goldChunkWidth    = 102.0        :: Float
goldChunkHeight   = 112.0        :: Float
goldChunkGravity  = 2460.0       :: Float

data GoldChunkData = GoldChunkData
    { _image          :: Image
    , _damagedImages  :: [Image]
    , _dropGoldValues :: [GoldValue]
    }

mkGoldChunkData :: (FileCache m, GraphicsRead m, MonadIO m) => GoldChunkCount -> m GoldChunkData
mkGoldChunkData (GoldChunkCount countNum) =
    GoldChunkData <$>
    loadPackImg imgFileName <*>
    traverse loadPackImg damagedImgFileNames <*>
    splitGoldDropGoldValue (GoldValue countNum * perCountGoldValue)
    where
        loadPackImg = \f -> loadPackImage $ packResourceFilePath f

        dropNum                            = max 0 (length imageFileNames - countNum)
        (imgFileName, damagedImgFileNames) = case drop dropNum imageFileNames of
            (f:fs) -> (f, fs)
            _      -> (defaultImageFileName, safeTail imageFileNames)

mkGoldChunkSet :: (FileCache m, GraphicsRead m, MonadIO m) => [GoldChunkSetJSON] -> m [Some RoomItem]
mkGoldChunkSet jsons =
    let
        choices = concat
            [ replicate n json
            | json <- jsons
            , let n = fromMaybe 1 (_chanceMultiplier json)
            ]
    in case choices of
        (c:cs) -> do
            json <- randomChoice $ c NE.:| cs
            traverse mkGoldChunk (_chunks json)
        _      -> return []

mkGoldChunk :: (FileCache m, GraphicsRead m, MonadIO m) => GoldChunkJSON -> m (Some RoomItem)
mkGoldChunk json =
    let
        Pos2 x y = _pos (json :: GoldChunkJSON)
        count    = _count json
    in do
        msgId         <- newId
        goldChunkData <- mkGoldChunkData count
        let pos        = Pos2 (x - goldChunkWidth / 2.0) (y - goldChunkHeight)
        let hbx        = rectHitbox pos goldChunkWidth goldChunkHeight

        return . Some $ (mkRoomItem GoldChunkItemType goldChunkData msgId hbx)
            { _isAttackable = True
            , _update       = updateGoldChunk
            , _draw         = drawGoldChunk
            }

drawGoldChunk :: (GraphicsReadWrite m, MonadIO m) => RoomItemDraw GoldChunkData m
drawGoldChunk goldChunk = drawImage pos RightDir levelItemZIndex img
    where
        pos = hitboxBotCenter $ RI._hitbox goldChunk
        img = _image (_data goldChunk :: GoldChunkData)

updateGoldChunk :: MsgsReadWrite UpdateLevelMsgsPhase m => RoomItemUpdate GoldChunkData m
updateGoldChunk goldChunk =
    pure (updateGoldChunkGravity goldChunk) >>=
    processGoldChunkCollisionMsgs >>=
    processGoldChunkHurtMsgs

updateGoldChunkGravity :: RoomItem GoldChunkData -> RoomItem GoldChunkData
updateGoldChunkGravity goldChunk = goldChunk
    { _hitbox = setHitboxTopLeft (Pos2 x y') hbx
    , _vel    = Vel2 velX velY'
    }
    where
        hbx            = RI._hitbox goldChunk
        Pos2 x y       = hitboxTopLeft hbx
        Vel2 velX velY = RI._vel goldChunk
        velY'          = velY + goldChunkGravity * timeStep
        y'             = y + velY' * timeStep

processGoldChunkCollisionMsgs
    :: MsgsRead UpdateLevelMsgsPhase m
    => RoomItem GoldChunkData
    -> m (RoomItem GoldChunkData)
processGoldChunkCollisionMsgs goldChunk = L.foldl' processMsg goldChunk <$> readMsgsTo (RI._msgId goldChunk)
    where
        processMsg :: RoomItem GoldChunkData -> CollisionMsgPayload -> RoomItem GoldChunkData
        processMsg gc d = case d of
            CollisionMsgTouchingGround groundY _
                | velY >= 0.0 ->
                    let
                        hbx = RI._hitbox gc
                        x   = vecX $ hitboxTopLeft hbx
                        y   = groundY - hitboxHeight hbx
                    in gc
                        { _hitbox = setHitboxTopLeft (Pos2 x y) hbx
                        , _vel    = Vel2 velX 0.0
                        }

            _ -> gc

            where Vel2 velX velY = RI._vel gc

damageGoldChunk
    :: HashedId
    -> RoomItem GoldChunkData
    -> ([Msg UpdateLevelMsgsPhase], RoomItem GoldChunkData -> RoomItem GoldChunkData)
damageGoldChunk atkHashedId goldChunk
    | atkHashedId `S.member` RI._hitByHashedIds goldChunk = ([], id)
    | otherwise                                           =
        let
            pos            = hitboxCenter $ RI._hitbox goldChunk
            goldChunkData  = _data goldChunk
            dropGoldValues = _dropGoldValues goldChunkData

            goldFakeAtkHit = (mkAttackHitEmpty NullId pos)
                { _hitEffectType   = NormalHitEffect
                , _specksType      = Just GoldSpecksType
                , _specksPos       = Just SpecksAtkIntersectPos
                , _specksDirection = Just SpecksAnyDir
                }

            hitMsgs =
                [ mkMsg $ AudioMsgPlaySound hitSoundFilePath pos
                , mkMsg $ ParticleMsgAddM (loadSimpleParticle pos RightDir worldEffectZIndex hitEffectPath)
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle goldFakeAtkHit)
                ]
        in case _damagedImages goldChunkData of
            [] ->
                let
                    removeItemMsg = mkMsg $ RoomMsgRemoveItem (RI._msgId goldChunk)
                    goldDrops     = traverse (mkGoldChunkGoldDrop pos) dropGoldValues
                    dropGoldMsg   = mkMsg $ NewUpdateProjectileMsgAddsM goldDrops
                in ([removeItemMsg, dropGoldMsg] ++ hitMsgs, id)

            (damagedImg:damagedImgs) ->
                let
                    n           = length dropGoldValues - length damagedImgs - 1
                    goldDrops   = traverse (mkGoldChunkGoldDrop pos) (take n dropGoldValues)
                    dropGoldMsg = mkMsg $ NewUpdateProjectileMsgAddsM goldDrops

                    update = \gc -> gc
                        { _data           = (_data gc)
                            { _image          = damagedImg
                            , _damagedImages  = damagedImgs
                            , _dropGoldValues = drop n dropGoldValues
                            }
                        , _hitByHashedIds = atkHashedId `S.insert` RI._hitByHashedIds gc
                        }
                in (dropGoldMsg:hitMsgs, update)

processGoldChunkHurtMsgs
    :: forall m. MsgsReadWrite UpdateLevelMsgsPhase m
    => RoomItem GoldChunkData
    -> m (RoomItem GoldChunkData)
processGoldChunkHurtMsgs goldChunk = foldlM processMsg goldChunk =<< readMsgsTo (RI._msgId goldChunk)
    where
        processMsg :: RoomItem GoldChunkData -> HurtMsgPayload -> m (RoomItem GoldChunkData)
        processMsg !gc d = case d of
            HurtMsgAttackHit atkHit -> do
                let (msgs, update) = damageGoldChunk (_hashedId atkHit) gc
                writeMsgs msgs
                return $ update gc

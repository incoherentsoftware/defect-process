module World.GoldDrop
    ( splitGoldDropGoldValue
    , mkGoldChunkGoldDrop
    , mkArenaGoldDrops
    , mkEndBossGoldDrops
    ) where

import Control.Monad          (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State    (execState, gets, modify)
import System.Random          (randomRIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Collision
import Configs
import Configs.All.Level
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Id
import Msg
import Projectile
import Util
import Window.Graphics
import World.GoldDrop.Types
import World.Util
import World.ZIndex

goldPickupSoundPath = "event:/SFX Events/Level/gold-pickup" :: FilePath

discreteGoldValueA = GoldValue 5  :: GoldValue
discreteGoldValueB = GoldValue 10 :: GoldValue
discreteGoldValueC = GoldValue 20 :: GoldValue
discreteGoldValueD = GoldValue 30 :: GoldValue
discreteGoldValueE = GoldValue 50 :: GoldValue

allDiscreteGoldValues = NE.fromList
    [ discreteGoldValueA
    , discreteGoldValueB
    , discreteGoldValueC
    , discreteGoldValueD
    , discreteGoldValueE
    ] :: NE.NonEmpty GoldValue

debugHitboxColor = Color 255 255 255 10 :: Color

dropWidth          = 17.0            :: Float
dropHeight         = 35.0            :: Float
dropAliveSecs      = 120.0           :: Float
dropSpeed          = 350.0           :: Float
dropGravity        = 3400.0          :: Float
dropMinRandomAngle = toRadians 60.0  :: Radians
dropMaxRandomAngle = toRadians 120.0 :: Radians

arenaDropHoverOffsetXMultiplier   = 50.0   :: PosX
endBossDropHoverOffsetXMultiplier = 20.0   :: PosX
hoverOffsetY                      = -100.0 :: PosY

mkGoldDropSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m GoldDropSprites
mkGoldDropSprites =
    GoldDropSprites <$>
    loadPackSpr "gold-idle-a.spr" <*>
    loadPackSpr "gold-idle-b.spr" <*>
    loadPackSpr "gold-idle-c.spr" <*>
    loadPackSpr "gold-idle-d.spr" <*>
    loadPackSpr "gold-idle-e.spr" <*>
    loadPackSpr "gold-appear-a.spr" <*>
    loadPackSpr "gold-appear-b.spr" <*>
    loadPackSpr "gold-appear-c.spr" <*>
    loadPackSpr "gold-appear-d.spr" <*>
    loadPackSpr "gold-appear-e.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/levels/level-items.pack" f

initialGoldDropSprite :: GoldDropType -> GoldValue -> GoldDropSprites -> Sprite
initialGoldDropSprite goldDropType goldValue sprs = case goldDropType of
    GoldChunkGoldDrop
        | goldValue <= discreteGoldValueA -> _idleA sprs
        | goldValue <= discreteGoldValueB -> _idleB sprs
        | goldValue <= discreteGoldValueC -> _idleC sprs
        | goldValue <= discreteGoldValueD -> _idleD sprs
        | otherwise                       -> _idleE sprs
    ArenaGoldDrop
        | goldValue <= discreteGoldValueA -> _appearA sprs
        | goldValue <= discreteGoldValueB -> _appearB sprs
        | goldValue <= discreteGoldValueC -> _appearC sprs
        | goldValue <= discreteGoldValueD -> _appearD sprs
        | otherwise                       -> _appearE sprs

isAppearSprite :: GoldDropData -> Bool
isAppearSprite goldDropData = _sprite goldDropData `elem` appearSprs
    where appearSprs = map ($ _sprites goldDropData) [_appearA, _appearB, _appearC, _appearD, _appearE]

isIdleSprite :: GoldDropData -> Bool
isIdleSprite goldDropData = _sprite goldDropData `elem` idleSprs
    where idleSprs = map ($ _sprites goldDropData) [_idleA, _idleB, _idleC, _idleD, _idleE]

toIdleSprite :: GoldDropData -> Sprite
toIdleSprite goldDropData
    | spr == _appearA sprs = _idleA sprs
    | spr == _appearB sprs = _idleB sprs
    | spr == _appearC sprs = _idleC sprs
    | spr == _appearD sprs = _idleD sprs
    | spr == _appearE sprs = _idleE sprs
    | otherwise            = spr
    where
        spr  = _sprite goldDropData
        sprs = _sprites goldDropData

mkGoldDropData :: (FileCache m, GraphicsRead m, MonadIO m) => GoldDropType -> GoldValue -> m GoldDropData
mkGoldDropData goldDropType goldValue = do
    sprs <- mkGoldDropSprites

    return $ GoldDropData
        { _type    = goldDropType
        , _value   = goldValue
        , _sprite  = initialGoldDropSprite goldDropType goldValue sprs
        , _sprites = sprs
        }

updateGoldDropData :: GoldDropData -> GoldDropData
updateGoldDropData goldDropData = goldDropData {_sprite = spr'}
    where
        spr                                                      = _sprite goldDropData
        spr'
            | isAppearSprite goldDropData  && spriteFinished spr = toIdleSprite goldDropData
            | otherwise                                          = updateSprite spr

splitGoldDropGoldValueEx :: MonadIO m => GoldValue -> [GoldValue] -> m [GoldValue]
splitGoldDropGoldValueEx goldValue discreteGoldValues
    | goldValue <= GoldValue 0 = return []
    | otherwise                = do
        discreteGoldVal <- case NE.nonEmpty (filter (<= goldValue) discreteGoldValues) of
            Nothing   -> return goldValue
            Just vals -> randomChoice vals
        (discreteGoldVal:) <$> splitGoldDropGoldValueEx (goldValue - discreteGoldVal) discreteGoldValues

splitGoldDropGoldValue :: MonadIO m => GoldValue -> m [GoldValue]
splitGoldDropGoldValue goldValue = splitGoldDropGoldValueEx goldValue (NE.toList allDiscreteGoldValues)

mkGoldDrop
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Vel2
    -> GoldValue
    -> GoldDropType
    -> m (Some Projectile)
mkGoldDrop (Pos2 x y) vel goldValue goldDropType =
    let
        x'     = x - dropWidth / 2.0
        y'     = y - dropHeight
        hitbox = rectHitbox (Pos2 x' y') dropWidth dropHeight
    in do
        goldDropData <- mkGoldDropData goldDropType goldValue
        msgId        <- newId

        return . Some $ (mkProjectile goldDropData msgId hitbox dropAliveSecs)
            { _vel                  = vel
            , _registeredCollisions = S.fromList [ProjRegisteredSurfaceCollision]
            , _update               = updateGoldDrop
            , _draw                 = drawGoldDrop
            , _processCollisions    = processGoldDropCollisions
            }

mkGoldChunkGoldDrop :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> GoldValue -> m (Some Projectile)
mkGoldChunkGoldDrop pos goldValue = do
    angle  <- liftIO $ randomRIO (dropMinRandomAngle, dropMaxRandomAngle)
    let vel = Vel2 (dropSpeed * cos angle) (-dropSpeed * sin angle)
    mkGoldDrop pos vel goldValue GoldChunkGoldDrop

mkArenaGoldDropsEx :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> GoldValue -> PosX -> m [Some Projectile]
mkArenaGoldDropsEx (Pos2 x y) goldValue hoverOffsetXMultiplier = do
    let
        discreteGoldValues =
            [ discreteVal
            | discreteVal <- NE.toList allDiscreteGoldValues
            , discreteVal < goldValue || discreteVal == NE.head allDiscreteGoldValues
            ]
    goldValues <- splitGoldDropGoldValueEx goldValue discreteGoldValues

    let
        startI    = -(length goldValues `div` 2)
        positions =
            [ Pos2 (x + fromIntegral i * hoverOffsetXMultiplier) (y + hoverOffsetY)
            | (i, _) <- zip [startI..] goldValues
            ]

    sequenceA
        [ mkGoldDrop p zeroVel2 goldVal ArenaGoldDrop
        | (p, goldVal) <- zip positions goldValues
        ]

mkArenaGoldDrops :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> GoldValue -> m [Some Projectile]
mkArenaGoldDrops pos goldValue = mkArenaGoldDropsEx pos goldValue arenaDropHoverOffsetXMultiplier

mkEndBossGoldDrops :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m [Some Projectile]
mkEndBossGoldDrops pos = do
    endBossGoldValue <- readConfig _level _endBossGoldValue
    mkArenaGoldDropsEx pos endBossGoldValue endBossDropHoverOffsetXMultiplier

updateGoldDropPos :: Projectile GoldDropData -> Projectile GoldDropData
updateGoldDropPos goldDrop = goldDrop {_hitbox = const hitbox}
    where
        vel    = _vel goldDrop
        offset = toPos2 $ vel `vecMul` timeStep
        hitbox = moveHitbox offset (projectileHitbox goldDrop)

updateGoldDropVel :: Projectile GoldDropData -> Projectile GoldDropData
updateGoldDropVel goldDrop = goldDrop {_vel = vel}
    where vel = _vel goldDrop `vecAdd` Vel2 0.0 (dropGravity * timeStep)

addGoldDropPlayerCollision :: Projectile GoldDropData -> Projectile GoldDropData
addGoldDropPlayerCollision goldDrop = goldDrop
    { _registeredCollisions = ProjRegisteredPlayerCollision `S.insert` _registeredCollisions goldDrop
    }

updateGoldDrop :: Monad m => ProjectileUpdate GoldDropData m
updateGoldDrop goldDrop = return . flip execState goldDrop $ do
    let goldDropData = _data goldDrop
    case _type goldDropData of
        GoldChunkGoldDrop -> modify $ updateGoldDropPos . updateGoldDropVel
        ArenaGoldDrop     -> unless (isAppearSprite goldDropData) $
            modify $ updateGoldDropPos . updateGoldDropVel

    goldDropData' <- updateGoldDropData <$> gets _data
    -- allow player to pickup gold mid-air immediately after appear anim for arena drops
    when (isAppearSprite goldDropData && isIdleSprite goldDropData') $
        modify addGoldDropPlayerCollision

    modify $ \gd -> gd {_data = goldDropData'}

processGoldDropCollisions :: ProjectileProcessCollisions GoldDropData
processGoldDropCollisions projCollisions goldDrop = processCollisions projCollisions
    where
        processCollisions :: [ProjectileCollision] -> [Msg ThinkCollisionMsgsPhase]
        processCollisions []                     = []
        processCollisions (collision:collisions) = case collision of
            ProjSurfaceCollision hbx _ -> goldDropSurfaceCollision hbx goldDrop ++ processCollisions'
            ProjPlayerCollision _      -> goldDropPlayerCollision goldDrop
            _                          -> processCollisions'
            where processCollisions' = processCollisions collisions

goldDropSurfaceCollision :: Hitbox -> Projectile GoldDropData -> [Msg ThinkCollisionMsgsPhase]
goldDropSurfaceCollision surfaceHbx goldDrop =
    [ mkMsgTo (ProjectileMsgSetHitbox hbx') goldDropId
    , mkMsgTo (ProjectileMsgSetVelocity zeroVel2) goldDropId
    , mkMsgTo (ProjectileMsgUpdate addGoldDropPlayerCollision) goldDropId
    ]
    where
        hbx        = projectileHitbox goldDrop
        x          = vecX $ hitboxTopLeft hbx
        y          = hitboxTop surfaceHbx - hitboxHeight hbx
        hbx'       = setHitboxTopLeft (Pos2 x y) hbx
        goldDropId = _msgId goldDrop

goldDropPlayerCollision :: Projectile GoldDropData -> [Msg ThinkCollisionMsgsPhase]
goldDropPlayerCollision goldDrop =
    [ mkMsg $ PlayerMsgUpdateGold (+ gold)
    , mkMsgTo (ProjectileMsgSetTtl 0.0) (_msgId goldDrop)
    , mkMsg $ AudioMsgPlaySound goldPickupSoundPath pos
    ]
    where
        gold = _value $ _data goldDrop
        pos  = hitboxTopLeft $ projectileHitbox goldDrop

drawGoldDrop :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw GoldDropData m
drawGoldDrop goldDrop =
    let
        hbx = projectileHitbox goldDrop
        pos = hitboxBotCenter hbx
        spr = _sprite $ _data goldDrop
    in do
        whenM (readSettingsConfig _debug _drawItemHitboxes) $
            drawHitbox debugHitboxColor debugHitboxZIndex hbx

        drawSprite pos RightDir goldDropZIndex spr

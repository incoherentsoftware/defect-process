module Attack.Description.Parse
    ( loadPackAttackDescription
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types       (FromJSON, Parser, Value(Array, Null, Number))
import Data.Aeson.Types       (genericParseJSON, parseJSON, typeMismatch)
import Data.Maybe             (isNothing, fromMaybe)
import Data.Scientific        (toRealFloat)
import Data.Yaml              (decodeEither', encode)
import GHC.Generics           (Generic)
import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Data.Vector as V

import Attack.Description.Types
import Attack.Sound
import Attack.Util
import Collision
import FileCache
import Player.Meter
import Util
import Window.Graphics
import World.Screenshake.Types

data AttackVelsJSON
    = AttackVelJSON AttackVel
    | AttackVelsJSON [AttackVel]

instance FromJSON AttackVelsJSON where
    parseJSON :: Value -> Parser AttackVelsJSON
    parseJSON a@(Array v)
        | [Number x, Number y] <- V.toList v =
            let vel = Vel2 (toRealFloat x) (toRealFloat y)
            in return $ AttackVelJSON (AttackVel2 vel)
        | otherwise                          = AttackVelsJSON <$> parseJSON a
    parseJSON Null                           = return $ AttackVelJSON NoAttackVel
    parseJSON value                          = typeMismatch "AttackVelsJSON" value

data MaybeVel2sJSON
    = MaybeVel2JSON (Maybe Vel2)
    | MaybeVel2sJSON [Maybe Vel2]

instance FromJSON MaybeVel2sJSON where
    parseJSON :: Value -> Parser MaybeVel2sJSON
    parseJSON a@(Array v)
        | [Number x, Number y] <- V.toList v = return $ MaybeVel2JSON (Just (Vel2 (toRealFloat x) (toRealFloat y)))
        | otherwise                          = MaybeVel2sJSON <$> parseJSON a
    parseJSON Null                           = return $ MaybeVel2JSON Nothing
    parseJSON value                          = typeMismatch "MaybeVel2sJSON" value

data AttackSoundFrameIndexJSON
    = AttackSoundFrameIndexJSON FrameIndex
    | AttackSoundFrameIndicesJSON [FrameIndex]

instance FromJSON AttackSoundFrameIndexJSON where
    parseJSON :: Value -> Parser AttackSoundFrameIndexJSON
    parseJSON (Number v)  =
        let frameIndex = FrameIndex $ round (toRealFloat v)
        in return $ AttackSoundFrameIndexJSON frameIndex
    parseJSON a@(Array _) = AttackSoundFrameIndicesJSON <$> parseJSON a
    parseJSON value       = typeMismatch "AttackSoundFrameIndexJSON" value

data AttackDescriptionJSON = AttackDescriptionJSON
    { _sprite                       :: Value
    , _hitboxes                     :: [Maybe Hitbox]
    , _vels                         :: AttackVelsJSON
    , _hitVels                      :: MaybeVel2sJSON
    , _cancelFrameIndex             :: Maybe FrameIndex
    , _walkCancelFrameIndex         :: Maybe FrameIndex
    , _launchTargetOffsetY          :: Maybe Float
    , _noLaunchTargetOvershoot      :: Maybe Bool
    , _isWeakHitVels                :: Maybe Bool
    , _hitlag                       :: Maybe Secs
    , _damage                       :: Maybe Damage
    , _stagger                      :: Maybe Stagger
    , _hitstunMultiplier            :: Maybe Float
    , _soundFilePath                :: Maybe FilePath
    , _soundFrameIndex              :: Maybe AttackSoundFrameIndexJSON
    , _soundContinuous              :: Maybe Bool
    , _hitSoundFilePath             :: Maybe FilePath
    , _screenshakeMagnitude         :: Maybe ScreenshakeMagnitude
    , _screenshakeFrameIndex        :: Maybe FrameIndex
    , _screenshakeOnHit             :: Maybe Bool
    , _refreshHitboxesOnLoop        :: Maybe Bool
    , _refreshHitboxesPerFrameCount :: Maybe Int
    , _hitEffectSpriteFileName      :: Maybe FilePath
    , _hitEffectType                :: Maybe AttackHitEffectType
    , _meterGain                    :: Maybe MeterValue
    , _nextAttackOnDonePath         :: Maybe FilePath
    , _isRanged                     :: Maybe Bool
    }
    deriving Generic

instance FromJSON AttackDescriptionJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

parseLoadAttackDescription
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => FileName
    -> BS.ByteString
    -> FilePath
    -> m AttackDescription
parseLoadAttackDescription filePath atkDescByteStr packFilePath =
    let
        error' :: String -> a
        error' s = error $ filePath ++ ": " ++ s

        adjustHitboxPos :: Pos2 -> Pos2 -> Hitbox -> Hitbox
        adjustHitboxPos originPos topLeftOffset hitbox = case hitbox of
            RectHitbox _ width height offset -> rectHitboxEx zeroPos2 width height (adjustPos offset)
            PolyHitbox _ offsets             -> polyHitbox zeroPos2 (map adjustPos offsets)
            DummyHitbox _                    -> dummyHitbox zeroPos2
            where adjustPos = \pos -> pos `vecSub` originPos `vecSub` topLeftOffset

        atkDescJSON = case decodeEither' atkDescByteStr of
            Left e     -> error' $ show e
            Right json -> json

        hitboxes = _hitboxes (atkDescJSON :: AttackDescriptionJSON)
        vels     = _vels (atkDescJSON :: AttackDescriptionJSON)
        hitVels  = _hitVels (atkDescJSON :: AttackDescriptionJSON)

        cancelFrameIndex = case _cancelFrameIndex (atkDescJSON :: AttackDescriptionJSON) of
            Just frameIndex -> frameIndex
            Nothing         -> 0

        walkCancelFrameIndex = case _walkCancelFrameIndex (atkDescJSON :: AttackDescriptionJSON) of
            Just frameIndex -> frameIndex
            Nothing         -> maxBound

        hitlag = case _hitlag (atkDescJSON :: AttackDescriptionJSON) of
            Just hitlag'
                | hitlag' >= 0.0 -> hitlag'
                | otherwise      -> error' "Invalid hitlag"
            Nothing              -> 0.0

        damage = case _damage (atkDescJSON :: AttackDescriptionJSON) of
            Just (Damage val)
                | val >= 0  -> Damage val
                | otherwise -> error' "Invalid damage"
            Nothing         -> Damage 0

        stagger = case _stagger (atkDescJSON :: AttackDescriptionJSON) of
            Just (Stagger val)
                | val >= 0  -> Stagger val
                | otherwise -> error' "Invalid stagger"
            Nothing         -> damageToStagger damage

        soundFrameIndices  = case _soundFrameIndex (atkDescJSON :: AttackDescriptionJSON) of
            Just (AttackSoundFrameIndexJSON index)
                | index < 0                 -> error' "Invalid soundFrameIndex"
                | otherwise                 -> [index]
            Just (AttackSoundFrameIndicesJSON indices)
                | or [i < 0 | i <- indices] -> error' "Invalid soundFrameIndex"
                | otherwise                 -> indices
            Nothing                         -> []
        soundContinuous    = fromMaybe False (_soundContinuous (atkDescJSON :: AttackDescriptionJSON))
        hitSoundFilePath   = _hitSoundFilePath (atkDescJSON :: AttackDescriptionJSON)

        soundType = case _soundFilePath (atkDescJSON :: AttackDescriptionJSON) of
            Nothing               -> AttackNoSound
            Just soundFilePath
                | soundContinuous ->
                    let
                        mkAttackSoundContinuousData :: FrameIndex -> Maybe FrameIndex -> AttackSoundContinuousData
                        mkAttackSoundContinuousData startFrameIndex endFrameIndex = AttackSoundContinuousData
                            { _startFrameIndex = startFrameIndex
                            , _endFrameIndex   = endFrameIndex
                            }
                    in case soundFrameIndices of
                        [] -> error' "soundContinuous set without soundFrameIndex"

                        [index] ->
                            let continuousData = mkAttackSoundContinuousData index Nothing
                            in AttackPlaySoundContinuous soundFilePath continuousData

                        indices@(startIndex:_)
                            | [_, endIndex] <- indices ->
                                let continuousData = mkAttackSoundContinuousData startIndex (Just endIndex)
                                in AttackPlaySoundContinuous soundFilePath continuousData
                            | otherwise                -> error' "soundContinuous set with unexpected soundFrameIndex"

                | otherwise -> case soundFrameIndices of
                    [frameIndex] -> AttackPlaySound soundFilePath frameIndex
                    frameIndices -> AttackPlaySounds soundFilePath (S.fromList frameIndices)

        sound = AttackSound
            { _type             = soundType
            , _hitSoundFilePath = hitSoundFilePath
            }

        screenshakeMagnitude  = _screenshakeMagnitude (atkDescJSON :: AttackDescriptionJSON)
        screenshakeFrameIndex = _screenshakeFrameIndex (atkDescJSON :: AttackDescriptionJSON)
        screenshakeOnHit      = _screenshakeOnHit (atkDescJSON :: AttackDescriptionJSON)
        screenshakeType       = case (screenshakeMagnitude, screenshakeFrameIndex, screenshakeOnHit) of
            (Nothing, Nothing, Nothing)                -> NoScreenshake
            (_, Just _, Just _)                        ->
                error' "screenshakeFrameIndex/screenshakeOnHit both specified"
            (Just magnitude, _, _)
                | magnitude < 0.0                      ->
                    error' "invalid screenshakeMagnitude specified"
            (Just _, Nothing, Nothing)                 ->
                error' "screenshakeMagnitude set without screenshakeFrameIndex/screenshakeOnHit"
            (Nothing, Just _, Nothing)                 ->
                error' "screenshakeFrameIndex set without screenshakeMagnitude"
            (Nothing, Nothing, Just _)                 ->
                error' "screenshakeOnHit set without screenshakeMagnitude"
            (Just magnitude, Just frameIndex, Nothing) -> ScreenshakeOnFrame frameIndex magnitude
            (Just magnitude, Nothing, Just onHit)
                | onHit                                -> ScreenshakeOnHit magnitude
                | otherwise                            ->
                    error' "screenshakeOnHit must be true if specified"

        refreshHitboxesOnLoop        = _refreshHitboxesOnLoop (atkDescJSON :: AttackDescriptionJSON)
        refreshHitboxesPerFrameCount = _refreshHitboxesPerFrameCount (atkDescJSON :: AttackDescriptionJSON)

        hitEffectSpriteFileName = _hitEffectSpriteFileName (atkDescJSON :: AttackDescriptionJSON)
        hitEffectPath           = PackResourceFilePath packFilePath <$> hitEffectSpriteFileName

        hitEffectType  = _hitEffectType (atkDescJSON :: AttackDescriptionJSON)
        hitEffectType' = fromMaybe NormalHitEffect hitEffectType
    in do
        spr <- parseLoadSprite filePath (encode $ _sprite (atkDescJSON :: AttackDescriptionJSON))

        let
            refreshHitboxesType = case (refreshHitboxesOnLoop, refreshHitboxesPerFrameCount) of
                (Nothing, Nothing)         -> NoRefreshHitboxes
                (Just False, Nothing)      -> RefreshHitboxesOnLoop
                (Just True, Nothing)       -> RefreshHitboxesOnLoop
                (Nothing, Just frameCount) -> RefreshHitboxesPerFrameCount frameCount
                (Just _, Just _)           ->
                    error' "both refreshHitboxesOnLoop/refreshHitboxesPerFrameCount specified"

            maxLoops   = _maxLoops =<< _loopData spr
            noMaxLoops = isNothing maxLoops

        case refreshHitboxesType of
            RefreshHitboxesOnLoop
                | noMaxLoops  -> error' "refreshHitboxesOnLoop set but no maxLoops specified"
            _                 -> return ()

        let
            hitboxesLength = length hitboxes
            velsLength     = case vels of
                AttackVelJSON _   -> Nothing
                AttackVelsJSON vs -> Just $ length vs
            hitVelsLength  = case hitVels of
                MaybeVel2JSON _   -> Nothing
                MaybeVel2sJSON vs -> Just $ length vs

        when (maybe False (hitboxesLength /=) velsLength) $
            error' "hitboxes/vels lengths don't match"
        when (fromMaybe False ((/=) <$> velsLength <*> hitVelsLength)) $
            error' "vels/hitVels lengths don't match"
        when (maybe False (hitboxesLength /=) hitVelsLength) $
            error' "hitboxes/hitVels lengths don't match"

        let
            loopDataCycle' :: [a] -> [a]
            loopDataCycle' vs = loopDataCycle (_loopData spr) vs

            sprImages         = _images spr
            sprOrigins        = map _originPos sprImages
            sprTopLeftOffsets = map _topLeftOffset sprImages
            hitboxes'         =
                [ adjustHitboxPos origin topLeftOffset <$> hbx
                | (origin, topLeftOffset, hbx) <- zip3 sprOrigins sprTopLeftOffsets (loopDataCycle' hitboxes)
                ]

            vels' = case vels of
                AttackVelJSON v   -> loopDataCycle' $ repeat v
                AttackVelsJSON vs -> loopDataCycle' vs

            hitVels' = case hitVels of
                MaybeVel2JSON v   -> loopDataCycle' $ repeat v
                MaybeVel2sJSON vs -> loopDataCycle' vs

        nextAttackDescOnDone <- case _nextAttackOnDonePath (atkDescJSON :: AttackDescriptionJSON) of
            Nothing   -> return Nothing
            Just path -> Just <$> loadPackAttackDescription (PackResourceFilePath packFilePath path)

        return $ AttackDescription
            { _filePath                = filePath
            , _sprite                  = spr
            , _hitboxes                = hitboxes'
            , _vels                    = vels'
            , _hitVels                 = hitVels'
            , _cancelFrameIndex        = cancelFrameIndex
            , _walkCancelFrameIndex    = walkCancelFrameIndex
            , _launchTargetOffsetY     = _launchTargetOffsetY (atkDescJSON :: AttackDescriptionJSON)
            , _noLaunchTargetOvershoot = _noLaunchTargetOvershoot (atkDescJSON :: AttackDescriptionJSON)
            , _isWeakHitVels           = _isWeakHitVels (atkDescJSON :: AttackDescriptionJSON)
            , _hitlag                  = hitlag
            , _damage                  = damage
            , _hitstunMultiplier       = _hitstunMultiplier (atkDescJSON :: AttackDescriptionJSON)
            , _stagger                 = stagger
            , _sound                   = sound
            , _screenshakeType         = screenshakeType
            , _refreshHitboxesType     = refreshHitboxesType
            , _hitEffectPath           = hitEffectPath
            , _hitEffectType           = hitEffectType'
            , _onHitType               = NormalOnHitType
            , _onSurfaceHitType        = NoSurfaceHitType
            , _meterGain               = _meterGain (atkDescJSON :: AttackDescriptionJSON)
            , _nextAttackDescOnDone    = nextAttackDescOnDone
            , _isRanged                = fromMaybe False (_isRanged (atkDescJSON :: AttackDescriptionJSON))
            }

loadPackAttackDescription :: (FileCache m, GraphicsRead m, MonadIO m) => PackResourceFilePath -> m AttackDescription
loadPackAttackDescription packResourceFilePath =
    let
        atkDescFileName = _fileName packResourceFilePath
        atkDescFilePath = _filePath (packResourceFilePath :: PackResourceFilePath)
    in do
        atkDescByteStr <- readFileCachePackResource packResourceFilePath
        parseLoadAttackDescription atkDescFileName atkDescByteStr atkDescFilePath

module Player.MovementSkill.All.GrappleSkill.Data
    ( Overshoot(..)
    , ShotReleased(..)
    , ShotType(..)
    , GrappleStatus(..)
    , isShotGrapple
    , isPullGrapple
    , GrappleImages(..)
    , GrappleData(..)
    , mkGrappleData
    , updateGrappleData
    , setGrappleThrowFireDrawStateMsg
    , setGrapplePullFireDrawStateMsg
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.Grapple
import FileCache
import Id
import Msg
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState
import Player.MovementSkill.All.GrappleSkill.GunFireDrawDatas
import Player.MovementSkill.Types
import Player.Types
import Util
import Window.Graphics

packFilePath = "data/player/player-skills.pack" :: FilePath

data Overshoot
    = PreventOvershoot
    | AllowOvershoot
    deriving (Eq, Show)

data ShotReleased
    = ShotReleased
    | NoShotReleased
    deriving Show

data ShotType
    = TowardsShot
    | PullShot
    deriving Show

data GrappleStatus
    = NotGrappling
    | ShotGrapple ShotType ShotReleased Secs
    | TowardsGrappling Pos2 VelY Secs Overshoot (Maybe Direction)
    | PullGrappling Secs
    | HangtimeAfterGrappling Secs
    deriving Show

isShotGrapple :: GrappleStatus -> Bool
isShotGrapple = \case
    ShotGrapple _ _ _ -> True
    _                 -> False

isPullGrapple :: GrappleStatus -> Bool
isPullGrapple = \case
    PullGrappling _ -> True
    _               -> False

data GrappleImages = GrappleImages
    { _prongs        :: Image
    , _cable         :: Image
    , _playerTowards :: Image
    }

mkGrappleImages :: (FileCache m, GraphicsRead m, MonadIO m) => m GrappleImages
mkGrappleImages =
    GrappleImages <$>
    loadPackImg "grapple-prongs.image" <*>
    loadPackImg "grapple-cable.image" <*>
    loadPackImg "grapple-towards.image"
    where loadPackImg = \f -> loadPackImage $ PackResourceFilePath packFilePath f

data GrappleData = GrappleData
    { _status             :: GrappleStatus
    , _aimAngle           :: Radians
    , _projectileMsgId    :: MsgId
    , _queuedFireDrawData :: Maybe GunFireDrawData
    , _fireDrawState      :: GunFireDrawState
    , _fireDrawDatas      :: GrappleGunFireDrawDatas
    , _images             :: GrappleImages
    , _config             :: GrappleConfig
    }

mkGrappleData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GrappleData
mkGrappleData = do
    fireDrawState <- mkGunFireDrawState zeroPos2
    fireDrawDatas <- mkGrappleGunFireDrawDatas
    imgs          <- mkGrappleImages
    cfg           <- readConfig _playerSkill _grapple

    return $ GrappleData
        { _status             = NotGrappling
        , _aimAngle           = 0.0
        , _projectileMsgId    = NullId
        , _queuedFireDrawData = Nothing
        , _fireDrawState      = fireDrawState
        , _fireDrawDatas      = fireDrawDatas
        , _images             = imgs
        , _config             = cfg
        }

updateGrappleData :: (ConfigsRead m, MonadIO m) => Player -> GrappleData -> m GrappleData
updateGrappleData player grappleData = do
    cfg <- readConfig _playerSkill _grapple

    let
        aimAngle      = _aimAngle (grappleData :: GrappleData)
        fireDrawState = _fireDrawState grappleData

        (fireDrawData, fakeFiredShot) = case _queuedFireDrawData grappleData of
            Nothing  -> (_gunFireDrawData fireDrawState, False)
            Just fdd -> (Just fdd, True)
        playerDir                     = _dir (player :: Player)

    fireDrawState' <- updateGunFireDrawStateEx False player fireDrawData aimAngle fakeFiredShot playerDir fireDrawState
    fireDrawDatas  <- updateGrappleGunFireDrawDatas $ _fireDrawDatas grappleData

    return $ grappleData
        { _queuedFireDrawData = Nothing
        , _fireDrawState      = fireDrawState'
        , _fireDrawDatas      = fireDrawDatas
        , _config             = cfg
        }

setGrappleThrowFireDrawStateMsg :: Msg ThinkPlayerMsgsPhase
setGrappleThrowFireDrawStateMsg = mkMsg . PlayerMsgUpdateMovementSkill $ \ms ->
    let msData = _data ms
    in ms
        { _data = msData {_queuedFireDrawData = Just $ _throw (_fireDrawDatas msData)}
        }

setGrapplePullFireDrawStateMsg :: Msg ThinkCollisionMsgsPhase
setGrapplePullFireDrawStateMsg = mkMsg . PlayerMsgUpdateMovementSkill $ \ms ->
    let msData = _data ms
    in ms
        { _data = msData {_queuedFireDrawData = Just $ _pull (_fireDrawDatas msData)}
        }

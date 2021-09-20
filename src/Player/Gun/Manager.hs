module Player.Gun.Manager
    ( module Player.Gun.Manager.Types
    , module Player.Gun.FireDrawState
    , mkGunManager
    , updateGunManager
    , gunManagerActive
    , gunManagerCancelable
    , gunManagerGunType
    , gunManagerDir
    , giveGunManagerGun
    , clearGunManagerGuns
    , drawGunManager
    , drawGunManagerOverlay
    , thinkGunManager
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, modify, when)
import Data.Functor           ((<&>))
import Data.Maybe             (listToMaybe)
import qualified Data.Set as S

import AppEnv
import Configs
import FileCache
import Msg
import Player.BufferedInputState
import Player.Gun as G
import Player.Gun.FireDrawState
import Player.Gun.Manager.Types
import Player.Types
import Player.Util
import Util
import Window.Graphics
import Window.InputState
import {-# SOURCE #-} Player

gunSwitchSoundPath = "event:/SFX Events/UI/gun-switch" :: FilePath

mkGunManager :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GunManager
mkGunManager = do
    fireDrawState <- mkGunFireDrawState zeroPos2
    return $ GunManager
        { _guns          = []
        , _fireDrawState = fireDrawState
        }

gunManagerActive :: GunManager -> Bool
gunManagerActive = gunFireDrawStateActive . _fireDrawState

gunManagerCancelable :: GunManager -> Bool
gunManagerCancelable = gunFireDrawStateCancelable . _fireDrawState

gunManagerActiveGun :: GunManager -> Maybe (Some Gun)
gunManagerActiveGun = listToMaybe . _guns

gunManagerGunType :: GunManager -> Maybe GunType
gunManagerGunType gunManager = (\(Some g) -> G._type g) <$> gunManagerActiveGun gunManager

gunManagerDir :: GunManager -> Direction
gunManagerDir = (_dir :: GunFireDrawState -> Direction) . _fireDrawState

switchGunManagerGuns :: GunManager -> GunManager
switchGunManagerGuns gunManager = gunManager {_guns = guns'}
    where
        guns  = _guns gunManager
        guns' = safeTail guns ++ take 1 guns

thinkGunManager :: Shootable -> Player -> GunManager -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
thinkGunManager shootable player gunManager =
    let
        thinkGun :: (Some Gun, GunStatus) -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
        thinkGun ((Some g), s) = (_think g) s player g
    in do
        whenM (isSwitchGun player gunManager) $
            writeMsgs
                [ mkMsg $ AudioMsgPlaySoundCentered gunSwitchSoundPath
                , mkMsg $ PlayerMsgClearInputBuffer (S.singleton SwitchGunInput)
                ]

        let gunStatuses = ActiveStatus shootable:repeat InactiveStatus
        concat <$> traverse thinkGun (zip (_guns gunManager) gunStatuses)

isSwitchGun :: InputRead m => Player -> GunManager -> m Bool
isSwitchGun player gunManager
    | length (_guns gunManager) < 2 = return False
    | otherwise                     = readInputState <&> \inputState ->
        SwitchGunAlias `aliasPressed` inputState || SwitchGunInput `inPlayerInputBuffer` player

updateGunManager :: Player -> GunManager -> AppEnv UpdatePlayerMsgsPhase GunManager
updateGunManager player gunManager =
    let
        moveSkillActive = playerMovementSkillActive player
        active          = gunManagerActive gunManager
        isAttacking     = playerAttackActive player
        cancelable      = gunManagerCancelable gunManager
        attackCancels   = active && isAttacking && cancelable
        cancelShoot     = moveSkillActive || attackCancels
        guns            = _guns gunManager
    in do
        guns' <- traverse (\(Some g) -> Some <$> (_update g) g) guns
        let
            fireDrawData = case listToMaybe guns' of
                Nothing         -> Nothing
                Just (Some gun) -> _fireDrawData gun

        fireDrawState <- updateGunFireDrawState cancelShoot player fireDrawData (_fireDrawState gunManager)
        isSwitchGun'  <- isSwitchGun player gunManager

        return . flip execState gunManager $ do
            modify $ \gs -> gs
                { _guns          = guns'
                , _fireDrawState = fireDrawState
                }

            when isSwitchGun' $
                modify switchGunManagerGuns

giveGunManagerGun :: Some Gun -> GunManager -> GunManager
giveGunManagerGun someGun@(Some gun) gunManager = case _guns gunManager of
    []                            -> gunManager {_guns = [someGun]}
    (Some gun1:_)
        | G._type gun1 == gunType -> gunManager
    [someGun1, someGun2@(Some gun2)]
        | G._type gun2 == gunType -> gunManager {_guns = [someGun2, someGun1]}
    [someGun1]                    -> gunManager {_guns = [someGun, someGun1]}
    [_, someGun2]                 -> gunManager {_guns = [someGun, someGun2]}
    _                             -> gunManager
    where gunType = G._type gun

clearGunManagerGuns :: GunManager -> GunManager
clearGunManagerGuns gunManager = gunManager {_guns = []}

drawGunManager :: GunManager -> AppEnv DrawMsgsPhase ()
drawGunManager gunManager = drawGunFireDrawState $ _fireDrawState gunManager

drawGunManagerOverlay :: Pos2 -> Direction -> GunManager -> AppEnv DrawMsgsPhase ()
drawGunManagerOverlay pos dir gunManager = case _guns gunManager of
    (Some gun:_) -> (_drawOverlay gun) pos dir gun
    _            -> return ()

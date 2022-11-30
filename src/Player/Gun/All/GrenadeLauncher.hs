module Player.Gun.All.GrenadeLauncher
    ( mkGrenadeLauncherGun
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.PlayerGun.GrenadeLauncher
import Constants
import FileCache
import Msg
import Player
import Player.BufferedInputState
import Player.Gun
import Player.Gun.All.GrenadeLauncher.Data
import Player.Gun.All.GrenadeLauncher.Grenade
import Player.Gun.All.GrenadeLauncher.Mine
import Player.Gun.FireDrawState
import Util
import Window.Graphics
import Window.InputState

grenadeLauncherSoundPath      = "event:/SFX Events/Player/Guns/grenade-launcher"       :: FilePath
grenadeLauncherUnderSoundPath = "event:/SFX Events/Player/Guns/grenade-launcher-under" :: FilePath
throwMineSoundPath            = "event:/SFX Events/Player/Guns/mine-toss"              :: FilePath

throwMineFrameTagName = FrameTagName "throwMine" :: FrameTagName

mkGrenadeLauncherGun :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Gun)
mkGrenadeLauncherGun = do
    grenadeLauncherData <- mkGrenadeLauncherData

    return . Some $ (mkGun grenadeLauncherData GrenadeLauncherGun)
        { _fireDrawData = Just $ _shootFireDrawData grenadeLauncherData
        , _think        = thinkGrenadeLauncher
        , _update       = updateGrenadeLauncher
        }

thinkGrenadeLauncher :: InputRead m => GunThink GrenadeLauncherData m
thinkGrenadeLauncher gunStatus player grenadeLauncher = think <$> readInputState
    where
        think :: InputState -> [Msg ThinkPlayerMsgsPhase]
        think inputState
            | fireDelayTtl > 0.0 = []

            | shootDownPressed && canShoot = if
                | not (canSpendMeter mineMeterCost) -> [mkMsg $ UiMsgInsufficientMeter mineMeterCost shootJustPressed]
                | otherwise                         ->
                    let updateThrowMine = \g -> g {_fireDrawData = Just $ _throwMineFireDrawData (_data g)}
                    in
                        [ mkMsg $ PlayerMsgUpdateGun updateThrowMine
                        , mkMsg PlayerMsgFiredGun
                        ]

            | shootPressed && canShoot = if
                | not (canSpendMeter shotMeterCost) -> [mkMsg $ UiMsgInsufficientMeter shotMeterCost shootJustPressed]
                | otherwise                         ->
                    let
                        updateShoot = \g ->
                            let
                                gData         = _data g
                                fireDelaySecs = _fireDelaySecs $ _config (gData :: GrenadeLauncherData)
                            in g
                                { _data         = gData {_fireDelayTtl = fireDelaySecs}
                                , _fireDrawData = Just $ _shootFireDrawData gData
                                }
                    in
                        [ mkMsg PlayerMsgFiredGun
                        , mkMsg $ NewThinkProjectileMsgAddM (mkGrenade player)
                        , mkMsg $ AudioMsgPlaySound grenadeLauncherSoundPath pos
                        , mkMsg $ AudioMsgPlaySound grenadeLauncherUnderSoundPath pos
                        , mkMsg $ PlayerMsgUpdateGun updateShoot
                        , mkMsg $ PlayerMsgSpendMeter shotMeterCost
                        ]

            | otherwise = case _torso <$> playerGunFireDrawSprites player of
                Just torsoSpr
                    | throwMineFrameTagName `isSpriteFrameTag` torsoSpr && _frameChanged torsoSpr ->
                        [ mkMsg $ NewThinkProjectileMsgAddM (mkMine player)
                        , mkMsg $ AudioMsgPlaySound throwMineSoundPath pos
                        , mkMsg $ PlayerMsgSpendMeter mineMeterCost
                        ]

                _ -> []

            where
                canSpendMeter = \meterCost -> canSpendPlayerMeter meterCost player

                grenadeLauncherData = _data grenadeLauncher
                fireDelayTtl        = _fireDelayTtl grenadeLauncherData

                cfg           = _config (grenadeLauncherData :: GrenadeLauncherData)
                shotMeterCost = _shotMeterCost cfg
                mineMeterCost = _mineMeterCost cfg
                canShoot      = gunStatus == ActiveStatus Shootable
                pos           = _pos (player :: Player)

                shootJustPressed = ShootAlias `aliasPressed` inputState
                shootPressed     = shootJustPressed || ShootInput `inPlayerInputBuffer` player
                shootDownPressed =
                    (ShootAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
                    ShootDownInput `inPlayerInputBuffer` player

updateGrenadeLauncher :: Monad m => GunUpdate GrenadeLauncherData m
updateGrenadeLauncher grenadeLauncher = return $ grenadeLauncher {_data = grenadeLauncherData'}
    where
        grenadeLauncherData  = _data grenadeLauncher
        fireDelayTtl         = max 0.0 (_fireDelayTtl grenadeLauncherData - timeStep)
        grenadeLauncherData' = grenadeLauncherData {_fireDelayTtl = fireDelayTtl}

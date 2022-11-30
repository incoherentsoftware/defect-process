module Player.SecondarySkill.All.SummonPlatformSkill
    ( mkSummonPlatformSkill
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.SummonPlatform
import Constants
import FileCache
import Id
import Msg
import Player
import Player.SecondarySkill as SS
import Player.SecondarySkill.All.SummonPlatformSkill.Platform
import Util
import Window.Graphics
import Window.InputState

data SummonPlatformSkillAttackDescriptions = SummonPlatformSkillAttackDescriptions
    { _summon     :: AttackDescription
    , _summonSoft :: AttackDescription
    }

mkSummonPlatformSkillAttackDescriptions
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => m SummonPlatformSkillAttackDescriptions
mkSummonPlatformSkillAttackDescriptions =
    SummonPlatformSkillAttackDescriptions <$>
    loadAtkDesc "summon-platform-player.atk" <*>
    loadAtkDesc "summon-platform-player-soft.atk"
    where loadAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath "data/player/player-skills.pack" f

data SummonPlatformSkillData = SummonPlatformSkillData
    { _cooldown       :: Secs
    , _platformProjId :: MsgId
    , _attackDescs    :: SummonPlatformSkillAttackDescriptions
    , _config         :: SummonPlatformConfig
    }

mkSummonPlatformSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SummonPlatformSkillData
mkSummonPlatformSkillData = do
    platformProjId <- newId
    atkDescs       <- mkSummonPlatformSkillAttackDescriptions
    cfg            <- readConfig _playerSkill _summonPlatform

    return $ SummonPlatformSkillData
        { _cooldown       = 0.0
        , _platformProjId = platformProjId
        , _attackDescs    = atkDescs
        , _config         = cfg
        }

mkSummonPlatformSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some SecondarySkill)
mkSummonPlatformSkill = do
    summonPlatformSkillData <- mkSummonPlatformSkillData
    return . Some $ (mkSecondarySkill summonPlatformSkillData SummonPlatformSkill)
        { _think      = thinkSummonPlatformSkill
        , _update     = updateSummonPlatformSkill
        , _onCooldown = summonPlatformSkillOnCooldown
        }

setSummonPlatformSkillCooldown :: SecondarySkill SummonPlatformSkillData -> SecondarySkill SummonPlatformSkillData
setSummonPlatformSkillCooldown summonPlatformSkill = summonPlatformSkill
    { _data = summonPlatformSkillData {_cooldown = summonCooldown}
    }
    where
        summonPlatformSkillData = _data summonPlatformSkill
        summonCooldown          = _summonCooldown $ _config (summonPlatformSkillData :: SummonPlatformSkillData)

thinkSummonPlatformSkill :: InputRead m => SecondarySkillThink SummonPlatformSkillData m
thinkSummonPlatformSkill canUseSkill player slot summonPlatformSkill = think <$> readInputState
    where
        think :: InputState -> [Msg ThinkPlayerMsgsPhase]
        think inputState
            | inAir && isSkillPressed && canUseSkill && cooldown <= 0.0 = if
                | not canSpendMeter -> [mkMsg $ UiMsgInsufficientMeter meterCost isSkillJustPressed]
                | otherwise         ->
                    [ mkMsg $ PlayerMsgSetAttackDesc summonAtkDesc
                    , mkMsg $ PlayerMsgUpdateSecondarySkill slot setSummonPlatformSkillCooldown
                    , mkMsg $ PlayerMsgSpendMeter meterCost
                    , mkMsg $ NewThinkProjectileMsgAddM (mkSummonedPlatform playerPos)
                    ]

            | otherwise = []

            where
                summonPlatformSkillData = _data summonPlatformSkill
                cooldown                = _cooldown summonPlatformSkillData
                cfg                     = _config (summonPlatformSkillData :: SummonPlatformSkillData)
                meterCost               = _summonMeterCost cfg
                canSpendMeter           = canSpendPlayerMeter meterCost player
                isSkillJustPressed      = isSecondarySkillPressed inputState slot
                isSkillPressed          = isSkillJustPressed || isSecondarySkillPressedBuffer player slot
                inAir                   = not $ _touchingGround (_flags (player :: Player) :: PlayerFlags)
                playerPos               = _pos (player :: Player)

                playerVelY                              = vecY $ _vel player
                atkDescs                                = _attackDescs summonPlatformSkillData
                summonAtkDesc
                    | playerVelY <= _summonSoftVelY cfg = _summonSoft atkDescs
                    | otherwise                         = _summon atkDescs


updateSummonPlatformSkill :: MsgsRead UpdatePlayerMsgsPhase m => SecondarySkillUpdate SummonPlatformSkillData m
updateSummonPlatformSkill _ summonPlatformSkill = return $ summonPlatformSkill
    { _data = summonPlatformSkillData {_cooldown = cooldown}
    }
    where
        summonPlatformSkillData = _data summonPlatformSkill
        cooldown                = max 0.0 (_cooldown summonPlatformSkillData - timeStep)

summonPlatformSkillOnCooldown :: SecondarySkillOnCooldown SummonPlatformSkillData
summonPlatformSkillOnCooldown = (> 0.0) . _cooldown . _data

module Player.SecondarySkill.All.StasisBlastSkill
    ( mkStasisBlastSkill
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.StasisBlast
import Constants
import FileCache
import Msg
import Player
import Player.SecondarySkill as SS
import Util
import Window.Graphics
import Window.InputState

data StasisBlastSkillAttackDescriptions = StasisBlastSkillAttackDescriptions
    { _ground :: AttackDescription
    , _air    :: AttackDescription
    }

mkStasisBlastSkillAttackDescs
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => StasisBlastConfig
    -> m StasisBlastSkillAttackDescriptions
mkStasisBlastSkillAttackDescs cfg =
    StasisBlastSkillAttackDescriptions <$>
    loadAtkDescWithOnHit "stasis-blast.atk" <*>
    loadAtkDescWithOnHit "stasis-blast-air.atk"
    where
        onHitType = ReplacedOnHitType $ \_ enemyId atk ->
            let stasisSecs = _blastStasisSecs cfg
            in [mkMsgTo (EnemyMsgSetStasis stasisSecs atk) enemyId]

        loadAtkDescWithOnHit = \f -> do
            atkDesc <- loadPackAttackDescription $ PackResourceFilePath "data/player/player-skills.pack" f
            return $ atkDesc {_onHitType = onHitType}

data StasisBlastSkillData = StasisBlastSkillData
    { _cooldown    :: Secs
    , _attackDescs :: StasisBlastSkillAttackDescriptions
    , _config      :: StasisBlastConfig
    }

mkStasisBlastSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m StasisBlastSkillData
mkStasisBlastSkillData = do
    cfg      <- readConfig _playerSkill _stasisBlast
    atkDescs <- mkStasisBlastSkillAttackDescs cfg

    return $ StasisBlastSkillData
        { _cooldown    = 0.0
        , _attackDescs = atkDescs
        , _config      = cfg
        }

mkStasisBlastSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some SecondarySkill)
mkStasisBlastSkill = do
    fastFallSkillData <- mkStasisBlastSkillData

    return . Some $ (mkSecondarySkill fastFallSkillData StasisBlastSkill)
        { _think      = thinkStasisBlastSkill
        , _update     = updateStasisBlastSkill
        , _onCooldown = stasisBlastSkillOnCooldown
        }

thinkStasisBlastSkill :: InputRead m => SecondarySkillThink StasisBlastSkillData m
thinkStasisBlastSkill canUseSkill player slot stasisBlastSkill = think <$> readInputState
    where
        think :: InputState -> [Msg ThinkPlayerMsgsPhase]
        think inputState
            | canUseSkill && isSkillPressed && cooldown <= 0.0 = if
                | not canSpendMeter -> [mkMsg $ UiMsgInsufficientMeter meterCost isSkillJustPressed]
                | otherwise         ->
                    let
                        atkDesc
                            | touchingGround = _ground atkDescs
                            | otherwise      = _air atkDescs

                        setCooldown = \ss -> ss
                            { SS._data = (SS._data ss) {_cooldown = _blastCooldown cfg}
                            }
                    in
                        [ mkMsg $ PlayerMsgSetAttackDesc atkDesc
                        , mkMsg $ PlayerMsgUpdateSecondarySkill slot setCooldown
                        , mkMsg $ PlayerMsgSpendMeter meterCost
                        ]

            | Just atk <- _attack player, atk `attackIs` _air atkDescs && touchingGround =
                [mkMsgEx PlayerMsgClearAttack MsgFrontOrder]

            | otherwise = []

            where
                stasisBlastSkillData = SS._data stasisBlastSkill
                cfg                  = _config (stasisBlastSkillData :: StasisBlastSkillData)
                meterCost            = _blastMeterCost cfg
                canSpendMeter        = canSpendPlayerMeter meterCost player
                isSkillJustPressed   = isSecondarySkillPressed inputState slot
                isSkillPressed       = isSkillJustPressed || isSecondarySkillPressedBuffer player slot
                cooldown             = _cooldown stasisBlastSkillData
                touchingGround       = _touchingGround (_flags (player :: Player) :: PlayerFlags)
                atkDescs             = _attackDescs stasisBlastSkillData

updateStasisBlastSkill :: MsgsRead UpdatePlayerMsgsPhase m => SecondarySkillUpdate StasisBlastSkillData m
updateStasisBlastSkill _ stasisBlastSkill = return $ stasisBlastSkill
    { SS._data = stasisBlastSkillData {_cooldown = cooldown}
    }
    where
        stasisBlastSkillData = SS._data stasisBlastSkill
        cooldown             = max 0.0 (_cooldown stasisBlastSkillData - timeStep)

stasisBlastSkillOnCooldown :: SecondarySkillOnCooldown StasisBlastSkillData
stasisBlastSkillOnCooldown stasisBlastSkill = _cooldown (SS._data stasisBlastSkill) > 0.0

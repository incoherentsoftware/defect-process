module Player.TauntState
    ( PlayerTauntState(..)
    , mkPlayerTauntState
    , playerTauntStateClearQueuedEnemyIdsMsg
    , playerTauntStateUpdateEnemyIdsMsg
    , playerTauntStateStartTauntMsgs
    , playerTauntStateActivateTauntMsgs
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Tuple             (swap)
import qualified Data.List as L
import qualified Data.Set as S

import Attack.Description
import Configs
import Configs.All.Enemy
import Configs.All.Player
import FileCache
import Id
import Msg
import Player.Meter
import Player.TauntState.Types
import Window.Graphics

packPath = \f -> PackResourceFilePath "data/player/player-movement.pack" f

mkPlayerTauntState :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerTauntState
mkPlayerTauntState = do
    let loadPackAtkDesc = \f -> loadPackAttackDescription $ packPath f
    tauntAtkA          <- loadPackAtkDesc "taunt-a.atk"
    tauntAtkB          <- loadPackAtkDesc "taunt-b.atk"
    tauntAtkC          <- loadPackAtkDesc "taunt-c.atk"
    tauntAtkD          <- loadPackAtkDesc "taunt-d.atk"

    return $ PlayerTauntState
        { _upTauntAttacks        = (tauntAtkA, tauntAtkC)
        , _downTauntAttacks      = (tauntAtkB, tauntAtkD)
        , _tauntedEnemyIds       = S.empty
        , _queuedTauntedEnemyIds = S.empty
        }

playerTauntStateClearQueuedEnemyIdsMsg :: PlayerTauntState -> Msg ThinkPlayerMsgsPhase
playerTauntStateClearQueuedEnemyIdsMsg _ = mkMsg $ PlayerMsgUpdateTauntState update
    where update = \ts -> ts {_queuedTauntedEnemyIds = S.empty}

readHitstunEnemyIds :: MsgsRead ThinkPlayerMsgsPhase m => m (S.Set MsgId)
readHitstunEnemyIds = S.fromList . L.foldl' processMsg [] <$> readMsgs
    where
        processMsg :: [MsgId] -> InfoMsgPayload -> [MsgId]
        processMsg enemyIds p = case p of
            InfoMsgEnemyInHitstun enemyId -> enemyId:enemyIds
            _                             -> enemyIds

readAllEnemyIds :: MsgsRead ThinkPlayerMsgsPhase m => m (S.Set MsgId)
readAllEnemyIds = S.fromList . L.foldl' processMsg [] <$> readMsgs
    where
        processMsg :: [MsgId] -> InfoMsgPayload -> [MsgId]
        processMsg enemyIds p = case p of
            InfoMsgEnemyPos _ enemyId -> enemyId:enemyIds
            _                         -> enemyIds

playerTauntStateUpdateEnemyIdsMsg
    :: (ConfigsRead m, MsgsRead ThinkPlayerMsgsPhase m)
    => PlayerTauntState
    -> m (Msg ThinkPlayerMsgsPhase)
playerTauntStateUpdateEnemyIdsMsg _ = do
    enemyIds <- readConfig _enemy _tauntedMeterRewardRequiresHitstun >>= \case
        True  -> readHitstunEnemyIds
        False -> readAllEnemyIds

    let update = \ts -> ts {_queuedTauntedEnemyIds = _queuedTauntedEnemyIds ts `S.union` enemyIds}
    return $ mkMsg (PlayerMsgUpdateTauntState update)

playerTauntStateStartTauntMsgs :: Bool -> PlayerTauntState -> [Msg ThinkPlayerMsgsPhase]
playerTauntStateStartTauntMsgs isUpInput tauntState =
    [ mkMsg $ PlayerMsgSetAttackDesc tauntAtk
    , mkMsg $ PlayerMsgUpdateTauntState update
    ]
    where
        upTauntAtks   = _upTauntAttacks tauntState
        downTauntAtks = _downTauntAttacks tauntState

        update = \ts -> ts
            { _upTauntAttacks   = if isUpInput then swap upTauntAtks else upTauntAtks
            , _downTauntAttacks = if isUpInput then downTauntAtks else swap downTauntAtks
            }

        tauntAtk
            | isUpInput = fst $ upTauntAtks
            | otherwise = fst $ downTauntAtks

playerTauntStateActivateTauntMsgs
    :: (ConfigsRead m, MsgsRead ThinkPlayerMsgsPhase m)
    => PlayerTauntState
    -> m [Msg ThinkPlayerMsgsPhase]
playerTauntStateActivateTauntMsgs tauntState =
    let
        queuedEnemyIds       = _queuedTauntedEnemyIds tauntState
        enemyIds             = _tauntedEnemyIds tauntState
        enemyIds'            = enemyIds `S.union` queuedEnemyIds
        queuedEnemyDiffCount = S.size enemyIds' - S.size enemyIds
    in do
        allEnemyIds <- readAllEnemyIds
        let
            update = \ts -> ts
                { _tauntedEnemyIds       = enemyIds' `S.union` allEnemyIds
                , _queuedTauntedEnemyIds = S.empty
                }

        gainMeterVal <- MeterValue . (queuedEnemyDiffCount *) <$> readConfig _player _tauntGainMeterMultiplier

        return
            [ mkMsg PlayerMsgActivateTaunt
            , mkMsg $ PlayerMsgUpdateTauntState update
            , mkMsg $ PlayerMsgGainMeter NullId gainMeterVal
            ]

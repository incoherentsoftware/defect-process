module Player.TauntState
    ( PlayerTauntState(..)
    , mkPlayerTauntState
    , playerTauntStateClearHitstunEnemyIdsMsg
    , playerTauntStateUpdateHitstunEnemyIdsMsg
    , playerTauntStateActivateMsgs
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as L
import qualified Data.Set as S

import Attack.Description
import Configs
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
    tauntAtk           <- loadPackAtkDesc "taunt.atk"

    return $ PlayerTauntState
        { _tauntAttack           = tauntAtk
        , _tauntedEnemyIds       = S.empty
        , _queuedHitstunEnemyIds = S.empty
        }

playerTauntStateClearHitstunEnemyIdsMsg :: PlayerTauntState -> Msg ThinkPlayerMsgsPhase
playerTauntStateClearHitstunEnemyIdsMsg _ = mkMsg $ PlayerMsgUpdateTauntState update
    where update = \ts -> ts {_queuedHitstunEnemyIds = S.empty}

playerTauntStateUpdateHitstunEnemyIdsMsg
    :: forall m. MsgsRead ThinkPlayerMsgsPhase m
    => PlayerTauntState
    -> m (Msg ThinkPlayerMsgsPhase)
playerTauntStateUpdateHitstunEnemyIdsMsg _ =
    let
        readHitstunEnemyIds :: m (S.Set MsgId)
        readHitstunEnemyIds = S.fromList . L.foldl' processMsg [] <$> readMsgs
            where
                processMsg :: [MsgId] -> InfoMsgPayload -> [MsgId]
                processMsg enemyIds p = case p of
                    InfoMsgEnemyInHitstun enemyId -> enemyId:enemyIds
                    _                             -> enemyIds
    in do
        enemyIds  <- readHitstunEnemyIds
        let update = \ts -> ts {_queuedHitstunEnemyIds = _queuedHitstunEnemyIds ts `S.union` enemyIds}
        return $ mkMsg (PlayerMsgUpdateTauntState update)

playerTauntStateActivateMsgs
    :: (ConfigsRead m, MsgsRead ThinkPlayerMsgsPhase m)
    => PlayerTauntState
    -> m [Msg ThinkPlayerMsgsPhase]
playerTauntStateActivateMsgs tauntState =
    let
        readAllEnemyIds :: MsgsRead ThinkPlayerMsgsPhase m1 => m1 (S.Set MsgId)
        readAllEnemyIds = S.fromList . L.foldl' processMsg [] <$> readMsgs
            where
                processMsg :: [MsgId] -> InfoMsgPayload -> [MsgId]
                processMsg enemyIds p = case p of
                    InfoMsgEnemyPos _ enemyId -> enemyId:enemyIds
                    _                         -> enemyIds

        hitstunEnemyIds       = _queuedHitstunEnemyIds tauntState
        tauntedEnemyIds       = _tauntedEnemyIds tauntState
        tauntedEnemyIds'      = tauntedEnemyIds `S.union` hitstunEnemyIds
        hitstunEnemyDiffCount = S.size tauntedEnemyIds' - S.size tauntedEnemyIds
    in do
        allEnemyIds <- readAllEnemyIds
        let
            update = \ts -> ts
                { _tauntedEnemyIds       = tauntedEnemyIds' `S.union` allEnemyIds
                , _queuedHitstunEnemyIds = S.empty
                }

        gainMeterVal <- MeterValue . (hitstunEnemyDiffCount *) <$> readConfig _player _tauntGainMeterMultiplier

        return
            [ mkMsg PlayerMsgActivateTaunt
            , mkMsg $ PlayerMsgUpdateTauntState update
            , mkMsg $ PlayerMsgGainMeter NullId gainMeterVal
            ]

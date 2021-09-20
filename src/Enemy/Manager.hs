module Enemy.Manager
    ( module Enemy.Manager.Types
    , mkEnemyManager
    , thinkEnemyManager
    , updateEnemyManager
    , drawEnemyManager
    , enemyManagerRealEnemies
    ) where

import Control.Monad.State (get, execStateT, lift, modify, put)
import Data.Foldable       (foldlM, sequenceA_)

import AppEnv
import Enemy as E
import Enemy.Manager.Types
import Enemy.Update
import Msg
import Util

mkEnemyManager :: EnemyManager
mkEnemyManager = EnemyManager {_enemies = []}

thinkEnemyManager :: EnemyManager -> AppEnv ThinkEnemyMsgsPhase ()
thinkEnemyManager enemyManager = sequenceA_ [thinkEnemy e | Some e <- enemies]
    where enemies = _enemies (enemyManager :: EnemyManager)

updateEnemyManagerMsgs :: EnemyManager -> AppEnv UpdateEnemyMsgsPhase EnemyManager
updateEnemyManagerMsgs enemyManager = foldlM processMsg enemyManager =<< readMsgs
    where
        processMsg :: EnemyManager -> EnemyMsgPayload -> AppEnv UpdateEnemyMsgsPhase EnemyManager
        processMsg !em d = case d of
            EnemyMsgAddM mkE   -> do
                enemies' <- (:enemies) <$> mkE
                return $ (em :: EnemyManager) {_enemies = enemies'}
            EnemyMsgAdds es      -> return $ (em :: EnemyManager) {_enemies = es ++ enemies}
            EnemyMsgAddsM mkEs -> do
                enemies' <- (++ enemies) <$> mkEs
                return $ (em :: EnemyManager) {_enemies = enemies'}
            _                  -> return em
            where enemies = _enemies (em :: EnemyManager)

updateEnemyManager :: EnemyManager -> AppEnv UpdateEnemyMsgsPhase EnemyManager
updateEnemyManager enemyManager = do
    enemyManager' <- updateEnemyManagerMsgs enemyManager

    let enemies = _enemies (enemyManager' :: EnemyManager)
    enemies'   <- flip execStateT enemies $ do
        get >>= lift . traverse (\(Some e) -> Some <$> updateEnemy e) >>= put
        modify $ filter (\(Some e) -> not (enemyDead e))

    return $ (enemyManager' :: EnemyManager) {_enemies = enemies'}

drawEnemyManager :: EnemyManager -> AppEnv DrawMsgsPhase ()
drawEnemyManager enemyManager = sequenceA_ [drawEnemy e | Some e <- enemies]
    where enemies = _enemies (enemyManager :: EnemyManager)

enemyManagerRealEnemies :: EnemyManager -> [Some Enemy]
enemyManagerRealEnemies enemyManager = filter (\(Some e) -> _dummyType e == EnemyRealType) enemies
    where enemies = _enemies enemyManager

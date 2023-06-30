module Player.BufferedInputState
    ( PlayerInput(..)
    , PlayerBufferedInputState()
    , allWeaponBufferedInputs
    , allShootBufferedInputs
    , allSecondarySkillBufferedInputs
    , mkPlayerBufferedInputState
    , inPlayerBufferedInputState
    , inPlayerBufferedInputStateTapInputs
    , isPlayerBufferedInputStateQCF
    , playerBufferedInputStateLastDir
    , updatePlayerBufferedInputState
    , updatePlayerBufferedInputStateInHitlag
    ) where

import Control.Monad.State (State, get, execState, execStateT, lift, modify, put, when)
import Data.Maybe          (fromMaybe)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Configs
import Configs.All.Settings
import Configs.All.Settings.Input
import Constants
import Msg
import Player.BufferedInputState.Types
import Player.Flags
import Player.Types
import Util
import Window.InputState

allTapInputs =
    [ LeftInput
    , RightInput
    , DownInput
    , DownLeftInput
    , DownRightInput
    , WeaponInput
    , WeaponReleaseInput
    ] :: [PlayerInput]

allJumpBufferedInputs = S.fromList
    [ JumpInput
    , JumpDownInput
    ]

allWeaponBufferedInputs = S.fromList
    [ WeaponInput
    , WeaponUpInput
    , WeaponDownInput
    , WeaponReleaseInput
    , WeaponReleaseUpInput
    , WeaponReleaseDownInput
    ] :: S.Set PlayerInput

allShootBufferedInputs = S.fromList
    [ ShootInput
    , ShootUpInput
    , ShootDownInput
    , ShootHoldInput
    , ShootReleaseInput
    , ShootReleaseUpInput
    , ShootReleaseDownInput
    ] :: S.Set PlayerInput

allSecondarySkillBufferedInputs = S.fromList
    [ SecondarySkillNeutralInput
    , SecondarySkillUpInput
    , SecondarySkillDownInput
    ] :: S.Set PlayerInput

mkPlayerBufferedInputState :: ConfigsRead m => m PlayerBufferedInputState
mkPlayerBufferedInputState = do
    cfg <- readConfig _settings _input
    return $ PlayerBufferedInputState
        { _inputMap                        = M.empty
        , _tapInputs                       = []
        , _lastDir                         = LeftDir
        , _lastHeldStraightDownElapsedSecs = maxSecs
        , _config                          = cfg
        }

inPlayerBufferedInputState :: PlayerInput -> PlayerBufferedInputState -> Bool
inPlayerBufferedInputState input bufferedInputState = input `M.member` _inputMap bufferedInputState

inPlayerBufferedInputStateTapInputs :: [PlayerInput] -> PlayerBufferedInputState -> Bool
inPlayerBufferedInputStateTapInputs inputs bufferedInputState = inputs `L.isInfixOf` tapInputs'
    where
        tapInputs  = map fst (_tapInputs bufferedInputState)
        tapInputs' = filter (`elem` inputs) tapInputs

playerBufferedInputStateLastDir :: PlayerBufferedInputState -> Direction
playerBufferedInputStateLastDir = _lastDir

isPlayerBufferedInputStateQCF :: Direction -> PlayerBufferedInputState -> Bool
isPlayerBufferedInputStateQCF dir bufferedInputState =
    let
        mostRecentInput :: PlayerInput -> PlayerInput -> Secs -> Maybe Secs -> Maybe Secs
        mostRecentInput playerInput bufferPlayerInput bufferSecs maxBufferSecs
            | bufferPlayerInput == playerInput = Just $ maybe bufferSecs (max bufferSecs) maxBufferSecs
            | otherwise                        = maxBufferSecs

        inTapInputs = \inputs -> inputs `inPlayerBufferedInputStateTapInputs` bufferedInputState
    in case dir of
        LeftDir
            | inTapInputs [DownInput, DownLeftInput, LeftInput]   -> True
        RightDir
            | inTapInputs [DownInput, DownRightInput, RightInput] -> True
        _                                                         -> fromMaybe False $ do
            let inputMap        = _inputMap bufferedInputState
            downSideBufferSecs <- case dir of
                LeftDir  -> M.foldrWithKey (mostRecentInput DownLeftInput) Nothing inputMap
                RightDir -> M.foldrWithKey (mostRecentInput DownRightInput) Nothing inputMap

            let
                downElapsedSecs     = _lastHeldStraightDownElapsedSecs bufferedInputState
                cfg                 = _config (bufferedInputState :: PlayerBufferedInputState)
                downSideElapsedSecs = _bufferSecs cfg - downSideBufferSecs

            Just $ case dir of
                LeftDir  -> inTapInputs [DownLeftInput, LeftInput] && downElapsedSecs > downSideElapsedSecs
                RightDir -> inTapInputs [DownRightInput, RightInput] && downElapsedSecs > downSideElapsedSecs

readInputs :: InputRead m => [PlayerInput] -> m [PlayerInput]
readInputs prevTapInputs = do
    inputState <- readInputState

    let
        addInput :: (InputState -> Bool) -> PlayerInput -> State [PlayerInput] ()
        addInput inInputState input = when (inInputState inputState) $
            modify (input:)

        downAliasHold     = DownAlias `aliasHold` inputState
        upAliasHold       = UpAlias `aliasHold` inputState
        lastTapInput      = maybeLast prevTapInputs
        wasDownLeftInput  = maybe False (DownLeftInput ==) lastTapInput
        wasDownRightInput = maybe False (DownRightInput ==) lastTapInput

    return . flip execState [] $ do
        if
            | downAliasHold && LeftAlias `aliasPressed` inputState                                          ->
                modify (DownLeftInput:)
            | downAliasHold && RightAlias `aliasPressed` inputState                                         ->
                modify (DownRightInput:)
            | wasDownLeftInput && DownAlias `aliasNotHold` inputState && LeftAlias `aliasHold` inputState   ->
                modify (LeftInput:)
            | wasDownRightInput && DownAlias `aliasNotHold` inputState && RightAlias `aliasHold` inputState ->
                modify (RightInput:)
            | otherwise                                                                                     -> do
                addInput (DownAlias `aliasPressed`) DownInput
                addInput (LeftAlias `aliasPressed`) LeftInput
                addInput (RightAlias `aliasPressed`) RightInput

        addInput (JumpAlias `aliasPressed`) JumpInput
        addInput (\i -> JumpAlias `aliasPressed` i && downAliasHold) JumpDownInput

        addInput (WeaponAlias `aliasPressed`) WeaponInput
        addInput (\i -> WeaponAlias `aliasPressed` i && upAliasHold) WeaponUpInput
        addInput (\i -> WeaponAlias `aliasPressed` i && downAliasHold) WeaponDownInput
        addInput (WeaponAlias `aliasHold`) WeaponHoldInput
        addInput (WeaponAlias `aliasReleased`) WeaponReleaseInput
        addInput (\i -> WeaponAlias `aliasReleased` i && upAliasHold) WeaponReleaseUpInput
        addInput (\i -> WeaponAlias `aliasReleased` i && downAliasHold) WeaponReleaseDownInput

        addInput (ShootAlias `aliasPressed`) ShootInput
        addInput (\i -> ShootAlias `aliasPressed` i && upAliasHold) ShootUpInput
        addInput (\i -> ShootAlias `aliasPressed` i && downAliasHold) ShootDownInput
        addInput (ShootAlias `aliasHold`) ShootHoldInput
        addInput (ShootAlias `aliasReleased`) ShootReleaseInput
        addInput (\i -> ShootAlias `aliasReleased` i && upAliasHold) ShootReleaseUpInput
        addInput (\i -> ShootAlias `aliasReleased` i && downAliasHold) ShootReleaseDownInput

        addInput (MovementSkillAlias `aliasPressed`) MovementSkillInput

        let secondarySkillAliasPressed = SecondarySkillAlias `aliasPressed` inputState
        addInput (\_ -> secondarySkillAliasPressed && not upAliasHold && not downAliasHold) SecondarySkillNeutralInput
        addInput (\_ -> secondarySkillAliasPressed && upAliasHold) SecondarySkillUpInput
        addInput (\_ -> secondarySkillAliasPressed && downAliasHold) SecondarySkillDownInput

        addInput (SwitchWeaponAlias `aliasPressed`) SwitchWeaponInput
        addInput (SwitchGunAlias `aliasPressed`) SwitchGunInput
        addInput (LockOnCursorAlias `aliasPressed`) LockOnCursorInput
        addInput (LockOnClearAlias `aliasPressed`) LockOnClearInput
        addInput (LockOnSwitchTargetAlias `aliasPressed`) LockOnSwitchTargetInput
        addInput (\i -> InteractAlias `aliasPressed` i && (upAliasHold || downAliasHold)) TauntInput

playerBufferedInputStateTapInputs :: PlayerBufferedInputState -> [PlayerInput]
playerBufferedInputStateTapInputs bufferedInputState = map fst (_tapInputs bufferedInputState)

concatTapInputs :: Secs -> [PlayerInput] -> [PlayerTapInput] -> [PlayerTapInput]
concatTapInputs tapBufferSecs inputs tapInputs = tapInputs' ++ newTapInputs
    where
        inputs'      = filter (`elem` allTapInputs) inputs
        newTapInputs = map (, tapBufferSecs) inputs'
        tapInputs'   = [(i, ttl') | (i, ttl) <- tapInputs, let ttl' = ttl - timeStep, ttl' > 0.0]

processClearInputsMsg :: MsgsRead UpdatePlayerMsgsPhase m => PlayerBufferedInputState -> m PlayerBufferedInputState
processClearInputsMsg bufferedInputState = L.foldl' processMsg bufferedInputState <$> readMsgs
    where
        processMsg :: PlayerBufferedInputState -> PlayerMsgPayload -> PlayerBufferedInputState
        processMsg b d = case d of
            PlayerMsgClearInputBuffer inputs -> clearInputs inputs b
            _                                -> b

updateLastDir :: PlayerBufferedInputState -> PlayerBufferedInputState
updateLastDir bufferedInputState = bufferedInputState {_lastDir = lastDir}
    where
        revTapInputs = reverse $ map fst (_tapInputs bufferedInputState)
        leftIndex    = L.elemIndex LeftInput revTapInputs
        rightIndex   = L.elemIndex RightInput revTapInputs

        lastDir = case (leftIndex, rightIndex) of
            (Just leftI, Just rightI)
                | leftI < rightI -> LeftDir
                | otherwise      -> RightDir
            (Just _, Nothing)    -> LeftDir
            (Nothing, Just _)    -> RightDir
            (Nothing, Nothing)   -> _lastDir bufferedInputState

updateLastHeldStraightDownElapsedSecs :: InputRead m => PlayerBufferedInputState -> m PlayerBufferedInputState
updateLastHeldStraightDownElapsedSecs bufferedInputState = do
    inputState <- readInputState
    let
        leftRightHeld                   = LeftAlias `aliasHold` inputState || RightAlias `aliasHold` inputState
        lastHeldStraightDownElapsedSecs = if
            | DownAlias `aliasHold` inputState && not leftRightHeld -> 0.0
            | otherwise                                             ->
                _lastHeldStraightDownElapsedSecs bufferedInputState + timeStep
    return $ bufferedInputState {_lastHeldStraightDownElapsedSecs = lastHeldStraightDownElapsedSecs}

updatePlayerBufferedInputState
    :: (ConfigsRead m, InputRead m, MsgsRead UpdatePlayerMsgsPhase m)
    => Player
    -> PlayerBufferedInputState
    -> m PlayerBufferedInputState
updatePlayerBufferedInputState player bufferedInputState =
    let
        cfg           = _config (bufferedInputState :: PlayerBufferedInputState)
        bufferSecs    = _bufferSecs cfg
        tapSecs       = _tapBufferSecs cfg
        prevTapInputs = playerBufferedInputStateTapInputs bufferedInputState
    in do
        newInputs <- readInputs prevTapInputs
        let
            newInputsMap = M.fromList $ map (, bufferSecs) newInputs
            inputMap     = flip execState (_inputMap bufferedInputState) $ do
                modify $ M.map (subtract timeStep)
                modify $ M.filter (> 0.0)
                modify $ M.union newInputsMap
            tapInputs    = concatTapInputs tapSecs newInputs (_tapInputs bufferedInputState)
            flags        = _flags player

        flip execStateT bufferedInputState $ do
            modify $ \b -> b
                { _inputMap  = inputMap
                , _tapInputs = tapInputs
                }
            modify updateLastDir
            get >>= lift . updateLastHeldStraightDownElapsedSecs >>= put

            get >>= lift . processClearInputsMsg >>= put

            when (_attacked flags) $
                modify (clearInputs allWeaponBufferedInputs . clearWeaponSpecialTapInputs)
            when (_movementSkilled flags) $
                modify $ clearInput MovementSkillInput
            when (_firedGun flags) $
                modify $ clearInputs allShootBufferedInputs
            when (_jumped flags || _platformDropped flags) $
                modify $ clearInputs allJumpBufferedInputs

updatePlayerBufferedInputStateInHitlag :: InputRead m => PlayerBufferedInputState -> m PlayerBufferedInputState
updatePlayerBufferedInputStateInHitlag bufferedInputState = updateInHitlag <$> readInputs prevTapInputs
    where
        prevTapInputs = playerBufferedInputStateTapInputs bufferedInputState

        updateInHitlag :: [PlayerInput] -> PlayerBufferedInputState
        updateInHitlag newInputs = updateLastDir bufferedInputState'
            where
                cfg          = _config (bufferedInputState :: PlayerBufferedInputState)
                bufferSecs   = _bufferSecs cfg
                tapSecs      = _tapBufferSecs cfg
                newInputsMap = M.fromList $ map (, bufferSecs) newInputs
                inputMap     = M.union newInputsMap (_inputMap bufferedInputState)
                tapInputs    = concatTapInputs tapSecs newInputs (_tapInputs bufferedInputState)

                bufferedInputState' = bufferedInputState
                    { _inputMap  = inputMap
                    , _tapInputs = tapInputs
                    }

clearInput :: PlayerInput -> PlayerBufferedInputState -> PlayerBufferedInputState
clearInput input inputMap = clearInputs (S.singleton input) inputMap

clearInputs :: S.Set PlayerInput -> PlayerBufferedInputState -> PlayerBufferedInputState
clearInputs inputs bufferedInputState = bufferedInputState {_inputMap = inputMap'}
    where
        inputMap  = _inputMap bufferedInputState
        inputMap' = case S.toList inputs of
            [input] -> M.delete input inputMap
            _       -> M.filterWithKey (\k _ -> k `S.notMember` inputs) inputMap

clearWeaponSpecialTapInputs :: PlayerBufferedInputState -> PlayerBufferedInputState
clearWeaponSpecialTapInputs bufferedInputState = bufferedInputState
    { _tapInputs = process $ _tapInputs bufferedInputState
    }
    where
        process :: [PlayerTapInput] -> [PlayerTapInput]
        process = \case
            ((LeftInput, _):lastInput@(LeftInput, _):tapInputs)                      -> process $ lastInput:tapInputs
            ((RightInput, _):lastInput@(RightInput, _):tapInputs)                    -> process $ lastInput:tapInputs
            ((DownInput, _):(DownLeftInput, _):lastInput@(LeftInput, _):tapInputs)   -> process $ lastInput:tapInputs
            ((DownInput, _):(DownRightInput, _):lastInput@(RightInput, _):tapInputs) -> process $ lastInput:tapInputs
            (tapInput:tapInputs)                                                     -> tapInput:process tapInputs
            []                                                                       -> []

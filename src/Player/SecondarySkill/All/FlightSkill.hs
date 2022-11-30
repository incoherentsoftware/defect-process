module Player.SecondarySkill.All.FlightSkill
    ( mkFlightSkill
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, get, gets, lift, modify, put)

import Attack
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.Flight
import Constants
import FileCache
import Msg
import Player
import Player.SecondarySkill as SS
import Util
import Window.Graphics
import Window.InputState

formWingsSoundFrameTagName = FrameTagName "formWingsSound"                       :: FrameTagName
formWingsSoundPath         = "event:/SFX Events/Player/Skills/flight-form-wings" :: FilePath

data FlightSkillState
    = InactiveState
    | HoverState Secs
    | FlightMovementState SpeedX SpeedY

data FlightSkillData = FlightSkillData
    { _state         :: FlightSkillState
    , _flightAtkDesc :: AttackDescription
    , _used          :: Bool
    , _config        :: FlightConfig
    }

mkFlightSkillData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m FlightSkillData
mkFlightSkillData = do
    flightAtkDesc <- loadPackAttackDescription (PackResourceFilePath "data/player/player-skills.pack" "flight.atk")
    cfg           <- readConfig _playerSkill _flight

    return $ FlightSkillData
        { _state         = InactiveState
        , _flightAtkDesc = flightAtkDesc
        , _used          = False
        , _config        = cfg
        }

mkFlightSkill :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some SecondarySkill)
mkFlightSkill = do
    flightSkillData <- mkFlightSkillData
    return . Some $ (mkSecondarySkill flightSkillData FlightSkill)
        { _think      = thinkFlightSkill
        , _update     = updateFlightSkill
        , _onCooldown = flightSkillOnCooldown
        }

setFlightSkillStateEx :: FlightSkillState -> Bool -> SecondarySkill FlightSkillData -> SecondarySkill FlightSkillData
setFlightSkillStateEx state used flightSkill = flightSkill
    { _data = (_data flightSkill)
        { _state = state
        , _used  = used
        }
    }

setFlightSkillState :: FlightSkillState -> SecondarySkill FlightSkillData -> SecondarySkill FlightSkillData
setFlightSkillState state flightSkill = setFlightSkillStateEx state used flightSkill
    where used = _used $ _data flightSkill

isPlayerAttackFlight :: Player -> SecondarySkill FlightSkillData -> Bool
isPlayerAttackFlight player flightSkill = case _attack player of
    Just atk -> atk `attackIs` _flightAtkDesc (_data flightSkill)
    Nothing  -> False

flightSkillVel :: FlightSkillData -> InputState -> Vel2
flightSkillVel flightSkillData inputState = Vel2 velX velY
    where
        cfg                         = _config (flightSkillData :: FlightSkillData)
        hoverVel@(Vel2 _ hoverVelY) = _hoverVel cfg
        Vel2 flightVelX flightVelY  = case _state flightSkillData of
            InactiveState                     -> zeroVel2
            HoverState _                      -> hoverVel
            FlightMovementState speedX speedY -> Vel2 speedX speedY

        upHeld    = UpAlias `aliasHold` inputState
        downHeld  = DownAlias `aliasHold` inputState
        leftHeld  = LeftAlias `aliasHold` inputState
        rightHeld = RightAlias `aliasHold` inputState

        velXMultiplier
            | upHeld || downHeld    = _diagSpeedXMultiplier cfg
            | otherwise             = 1.0
        velYMultiplier
            | leftHeld || rightHeld = _diagSpeedYMultiplier cfg
            | otherwise             = 1.0

        velX
            | leftHeld  = -flightVelX * velXMultiplier
            | rightHeld = flightVelX * velXMultiplier
            | otherwise = 0.0
        velY
            | upHeld    = -flightVelY * velYMultiplier
            | downHeld  = flightVelY * velYMultiplier
            | otherwise = hoverVelY

thinkFlightSkill :: InputRead m => SecondarySkillThink FlightSkillData m
thinkFlightSkill canUseSkill player slot flightSkill = think <$> readInputState
    where
        think :: InputState -> [Msg ThinkPlayerMsgsPhase]
        think inputState = case _state flightSkillData of
            InactiveState
                | not used && not touchingGround && canUseSkill && isSkillPressed ->
                    let hoverState = HoverState $ _hoverSecs cfg
                    in
                        [ mkMsg $ PlayerMsgSetAttackDesc (_flightAtkDesc flightSkillData)
                        , mkMsgEx (PlayerMsgSetVelocity flightVel) MsgEndOrder
                        , mkMsg PlayerMsgResetAirStallAttacksCounter
                        , mkMsg $ PlayerMsgUpdateSecondarySkill slot (setFlightSkillStateEx hoverState True)
                        ]
                | otherwise                                                       -> []

            HoverState ttl
                | ttl <= 0.0 ->
                    let flightMoveState = FlightMovementState (_dashSpeedX cfg) (_dashSpeedY cfg)
                    in [mkMsg $ PlayerMsgUpdateSecondarySkill slot (setFlightSkillState flightMoveState)]
                | otherwise  -> [mkMsgEx (PlayerMsgSetVelocity flightVel) MsgEndOrder]

            FlightMovementState _ _
                | touchingGround || not isPlayerAtkFlight ->
                    let clearAtkMsgs = if isPlayerAtkFlight then [mkMsg PlayerMsgClearAttack] else []
                    in clearAtkMsgs ++ [mkMsg $ PlayerMsgUpdateSecondarySkill slot (setFlightSkillState InactiveState)]
                | otherwise                               -> [mkMsgEx (PlayerMsgSetVelocity flightVel) MsgEndOrder]

            where
                touchingGround    = _touchingGround $ _flags player
                flightSkillData   = _data flightSkill
                used              = _used flightSkillData
                isPlayerAtkFlight = isPlayerAttackFlight player flightSkill
                cfg               = _config (flightSkillData :: FlightSkillData)
                flightVel         = flightSkillVel flightSkillData inputState
                isSkillPressed    =
                    isSecondarySkillPressed inputState slot || isSecondarySkillPressedBuffer player slot

processMessages
    :: MsgsRead UpdatePlayerMsgsPhase m
    => SecondarySkill FlightSkillData
    -> m (SecondarySkill FlightSkillData)
processMessages flightSkill = processMsgs <$> readMsgs
    where
        processMsgs :: [PlayerMsgPayload] -> SecondarySkill FlightSkillData
        processMsgs = \case
            []                           -> flightSkill
            (PlayerMsgResetDoubleJump:_) -> setFlightSkillState InactiveState flightSkill
            (_:ps)                       -> processMsgs ps

updateFlightSkill :: (ConfigsRead m, MsgsReadWrite UpdatePlayerMsgsPhase m) => SecondarySkillUpdate FlightSkillData m
updateFlightSkill player flightSkill = flip execStateT flightSkill $ do
    get >>= lift . processMessages >>= put

    let
        touchingGround = _touchingGround $ _flags player
        velY           = vecY $ _vel player
        used           = _used $ _data flightSkill
        used'          = if
            | touchingGround && velY >= 0.0 && used -> False
            | otherwise                             -> used

    cfg <- lift $ readConfig _playerSkill _flight
    modify $ \fs -> fs
        { _data = (_data fs)
            { _used   = used'
            , _config = cfg
            }
        }

    gets (_state . _data) >>= \case
        InactiveState -> return ()

        HoverState ttl ->
            let ttl' = max 0.0 (ttl - timeStep)
            in modify $ setFlightSkillState (HoverState ttl')

        FlightMovementState speedX speedY ->
            let
                baseSpeed = _baseSpeed cfg
                speedX'   = max baseSpeed (speedX - _dashDecelerateSpeedX cfg * timeStep)
                speedY'   = max baseSpeed (speedY - _dashDecelerateSpeedY cfg * timeStep)
            in modify $ setFlightSkillState (FlightMovementState speedX' speedY')

    case _attack player of
        Just atk
            | formWingsSoundFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk -> lift $
                let pos = _pos (player :: Player)
                in writeMsgs [mkMsg $ AudioMsgPlaySound formWingsSoundPath pos]
        _                                                                                 -> return ()

flightSkillOnCooldown :: SecondarySkillOnCooldown FlightSkillData
flightSkillOnCooldown = _used . _data

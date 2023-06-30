module Level.Room.Item.Jukebox
    ( module Level.Room.Item.Jukebox.Types
    , mkJukebox
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe)
import qualified Data.Text as T

import Collision.Hitbox
import FileCache
import Id
import Level.Room.Item as RI
import Level.Room.Item.Jukebox.JSON
import Level.Room.Item.Jukebox.Types
import Msg
import Util
import Window.Graphics
import Window.InputState
import World.Audio.LayeredMusic
import World.ZIndex

jukeboxWidth  = 119.0 :: Float
jukeboxHeight = 218.0 :: Float

interactSwitchTrackText           = "Switch track: {InteractAlias}"       :: T.Text
interactCurrentTrackTextColor     = Color 192 192 192 255                 :: Color
interactOverlayBackdropColor      = Color 0 0 0 200                       :: Color
interactOverlayBackdropBorderSize = 20.0                                  :: Float
interactOverlayBackdropOffsetY    = -245.0                                :: OffsetY
interactOverlayBackdropHeight     = 85.0                                  :: Float
interactOverlayText0OffsetY       = interactOverlayBackdropOffsetY + 18.0 :: OffsetY
interactOverlayText1OffsetY       = interactOverlayText0OffsetY + 44.0    :: OffsetY

jukeboxCycleSoundPath = "event:/SFX Events/Level/jukebox-cycle" :: FilePath

mkJukeboxSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m JukeboxSprites
mkJukeboxSprites =
    JukeboxSprites <$>
    loadPackSpr "jukebox-idle.spr" <*>
    loadPackSpr "jukebox-active.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/levels/level-items.pack" f

mkJukeboxData :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => JukeboxType -> m JukeboxData
mkJukeboxData jukeboxType = do
    currentTrackDisplayTxt     <- mkDisplayText "" Font29 interactCurrentTrackTextColor
    switchTrackInputDisplayTxt <- mkInputDisplayText interactSwitchTrackText Font32 whiteColor
    sprs                       <- mkJukeboxSprites

    return $ JukeboxData
        { _type                        = jukeboxType
        , _touchingPlayer              = False
        , _sprite                      = _idle sprs
        , _switchTrackInputDisplayText = switchTrackInputDisplayTxt
        , _currentTrackDisplayText     = currentTrackDisplayTxt
        , _sprites                     = sprs
        }

mkJukebox :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => JukeboxJSON -> m (Some RoomItem)
mkJukebox json =
    let
        Pos2 x y = _pos json
        pos      = Pos2 (x - jukeboxWidth / 2.0) (y - jukeboxHeight)
        hbx      = rectHitbox pos jukeboxWidth jukeboxHeight
    in do
        jukeboxData <- mkJukeboxData $ _type (json :: JukeboxJSON)
        msgId       <- newId

        return . Some $ (mkRoomItem JukeboxItemType jukeboxData msgId hbx)
            { _draw            = drawJukebox
            , _think           = thinkJukebox
            , _update          = updateJukebox
            , _playerCollision = jukeboxPlayerCollision
            , _inInteractRange = _touchingPlayer . _data
            }

thinkJukebox :: MsgsRead ThinkLevelMsgsPhase m => RoomItemThink JukeboxData m
thinkJukebox jukebox =
    let
        interactPressed :: [PlayerMsgPayload] -> Bool
        interactPressed []     = False
        interactPressed (d:ds) = case d of
            PlayerMsgInteract _ -> True
            _                   -> interactPressed ds
    in (interactPressed <$> readMsgs) <&> \case
        True
            | _touchingPlayer (_data jukebox) ->
                let
                    pos         = hitboxBotCenter $ _hitbox jukebox
                    jukeboxType = _type (_data jukebox :: JukeboxData)

                    setJukeboxSpr = \j ->
                        let jData = _data j
                        in j {_data = jData {_sprite = _active $ _sprites jData}}
                in
                    [ mkMsg $ AudioMsgPlaySound jukeboxCycleSoundPath pos
                    , mkMsg $ AudioMsgCycleJukeboxMusic jukeboxType
                    , mkMsgTo (RoomMsgUpdateItem setJukeboxSpr) (_msgId jukebox)
                    ]

        _ -> []

readLayeredMusicType :: MsgsRead UpdateLevelMsgsPhase m => JukeboxType -> m (Maybe LayeredMusicType)
readLayeredMusicType jukeboxType = processInfoMsgs <$> readMsgs
    where
        processInfoMsgs :: [InfoMsgPayload] -> Maybe LayeredMusicType
        processInfoMsgs []     = Nothing
        processInfoMsgs (p:ps) = case p of
            InfoMsgBattleMusic musicType
                | jukeboxType == BattleJukeboxType      -> Just musicType
            InfoMsgExplorationMusic musicType
                | jukeboxType == ExplorationJukeboxType -> Just musicType
            _                                           -> processInfoMsgs ps

updateJukebox
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateLevelMsgsPhase m)
    => RoomItemUpdate JukeboxData m
updateJukebox jukebox =
    let
        jukeboxPressed :: [AudioMsgPayload] -> Bool
        jukeboxPressed []     = False
        jukeboxPressed (d:ds) = case d of
            AudioMsgCycleJukeboxMusic _ -> True
            _                           -> jukeboxPressed ds

        jukeboxData = _data jukebox
        jukeboxType = _type (jukeboxData :: JukeboxData)
    in do
        spr <- (jukeboxPressed <$> readMsgs) <&> \case
            True
                -- different jukebox was pressed, make this one inactive
                | not (_touchingPlayer jukeboxData) -> _idle $ _sprites jukeboxData
            _                                       -> updateSprite $ _sprite jukeboxData

        switchTrackInputDisplayTxt <- updateInputDisplayText $ _switchTrackInputDisplayText jukeboxData

        let currentTrackDisplayTxt = _currentTrackDisplayText jukeboxData
        currentTrackDisplayTxt'   <- readLayeredMusicType jukeboxType <&> \case
            Nothing        -> currentTrackDisplayTxt
            Just musicType ->
                let txt = "(" <> prettyShow musicType <> ")"
                in updateDisplayText txt currentTrackDisplayTxt

        return $ jukebox
            { _data = jukeboxData
                { _touchingPlayer              = False
                , _sprite                      = spr
                , _switchTrackInputDisplayText = switchTrackInputDisplayTxt
                , _currentTrackDisplayText     = currentTrackDisplayTxt'
                }
            }

drawInteractOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItem JukeboxData -> m ()
drawInteractOverlay jukebox = do
    let
        jukeboxData                = _data jukebox
        switchTrackInputDisplayTxt = _switchTrackInputDisplayText jukeboxData
        currentTrackDisplayTxt     = _currentTrackDisplayText jukeboxData

    widths <- sequenceA
        [ inputDisplayTextWidth switchTrackInputDisplayTxt
        , displayTextWidth currentTrackDisplayTxt
        ]

    let
        Pos2 x y  = hitboxCenter $ RI._hitbox jukebox
        rectWidth = fromMaybe 0.0 (maybeMaximum widths) + interactOverlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y + interactOverlayBackdropOffsetY)
    drawRect rectPos rectWidth interactOverlayBackdropHeight interactOverlayBackdropColor uiInfoTextZIndex

    let
        textPos0 = Pos2 x (y + interactOverlayText0OffsetY)
        textPos1 = Pos2 x (y + interactOverlayText1OffsetY)
    drawDisplayTextCentered textPos0 uiInfoTextZIndex currentTrackDisplayTxt
    drawInputDisplayTextCentered textPos1 uiInfoTextZIndex switchTrackInputDisplayTxt

drawJukebox :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItemDraw JukeboxData m
drawJukebox jukebox =
    let
        pos         = hitboxBotCenter $ _hitbox jukebox
        jukeboxData = _data jukebox
        spr         = _sprite (jukeboxData :: JukeboxData)
    in do
        drawSprite pos RightDir levelItemZIndex spr
        when (_touchingPlayer jukeboxData) $
            drawInteractOverlay jukebox

jukeboxPlayerCollision :: RoomItemPlayerCollision JukeboxData
jukeboxPlayerCollision _ jukebox = [mkMsgTo (RoomMsgUpdateItem updateTouching) (_msgId jukebox)]
    where updateTouching = \j -> j {_data = (_data j) {_touchingPlayer = True}}

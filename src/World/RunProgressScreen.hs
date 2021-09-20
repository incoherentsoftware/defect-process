module World.RunProgressScreen
    ( RunProgressScreen(..)
    , mkRunProgressScreen
    , drawRunProgressScreen
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (for_)
import qualified Data.Set as S
import qualified SDL.Raw

import Constants
import FileCache
import Level.Room.Chooser.Types
import Level.Room.Util
import Util
import Window.Graphics
import World.ScreenWipe
import World.ZIndex

markerStartOffsetX = 15.0 :: PosX
markerMidOffsetX   = 10.0 :: PosX
markerEndOffsetX   = 30.0 :: PosX

data RunProgressScreen = RunProgressScreen
    { _markerImage             :: Image
    , _segmentStartImage       :: Image
    , _segmentStartFilledImage :: Image
    , _segmentImage            :: Image
    , _segmentFilledImage      :: Image
    , _segmentEndImage         :: Image
    , _segmentEndFilledImage   :: Image
    }

mkRunProgressScreen :: (FileCache m, GraphicsReadWrite m, MonadIO m) => m RunProgressScreen
mkRunProgressScreen =
    RunProgressScreen <$>
    loadPackImg "progression-marker.image" <*>
    loadPackImg "progression-segment-start.image" <*>
    loadPackImg "progression-segment-start-filled.image" <*>
    loadPackImg "progression-segment.image" <*>
    loadPackImg "progression-segment-filled.image" <*>
    loadPackImg "progression-segment-end.image" <*>
    loadPackImg "progression-segment-end-filled.image"
    where loadPackImg = \f -> loadPackImage $ PackResourceFilePath "data/ui/ui.pack" f

drawRunProgressScreen :: (GraphicsReadWrite m, MonadIO m) => WorldScreenWipe -> RoomChooser -> RunProgressScreen -> m ()
drawRunProgressScreen screenWipe roomChooser runProgressScreen
    | isArenaWipeOutIn || isEndWipeOutIn = drawRunProgressScreenInternal screenWipe roomChooser runProgressScreen
    | otherwise                          = return ()
    where
        screenWipeType   = _type screenWipe
        currentRoomType  = _currentRoomType roomChooser
        isArenaWipeOutIn =
            (isToTransitionRoomType currentRoomType && screenWipeType == WorldScreenWipeOut) ||
            (isArenaRoomType currentRoomType && screenWipeType == WorldScreenWipeIn)
        isEndWipeOutIn   =
            (currentRoomType == endHallwayRoomType && screenWipeType == WorldScreenWipeOut) ||
            (currentRoomType == endRoomType && screenWipeType == WorldScreenWipeIn)

drawRunProgressScreenInternal
    :: (GraphicsReadWrite m, MonadIO m)
    => WorldScreenWipe
    -> RoomChooser
    -> RunProgressScreen
    -> m ()
drawRunProgressScreenInternal screenWipe roomChooser runProgressScreen =
    let
        numArenas                             = _numArenas roomChooser
        numVisitedArenas                      = S.size $ _visitedArenaRoomNames roomChooser
        currentRoomType                       = _currentRoomType roomChooser
        numVisitedArenas'
            | isArenaRoomType currentRoomType = max (numVisitedArenas - 1) 0
            | otherwise                       = numVisitedArenas
        numMidSegments                        = max (numArenas - 1) 0

        segmentStartImg = _segmentStartImage runProgressScreen
        segmentEndImg   = _segmentEndImage runProgressScreen
        segmentImg      = _segmentImage runProgressScreen

        segmentStartImgWidth = imageWidth segmentStartImg
        segmentEndImgWidth   = imageWidth segmentEndImg
        segmentImgWidth      = imageWidth segmentImg
        totalWidth           = segmentStartImgWidth + segmentEndImgWidth + segmentImgWidth * fromIntegral numMidSegments
        y                    = virtualRenderHeight / 2.0

        screenWipeX = _posX screenWipe
        clipRect    = case _type screenWipe of
            WorldScreenWipeIn  -> SDL.Raw.Rect 0 0 (round screenWipeX) (round virtualRenderHeight)
            WorldScreenWipeOut ->
                let width = round $ virtualRenderWidth - screenWipeX
                in SDL.Raw.Rect (round screenWipeX) 0 width (round virtualRenderHeight)
    in do
        setCameraSpace CameraScreenSpace
        setGraphicsClipRect $ Just clipRect

        let
            startX                      = (virtualRenderWidth - totalWidth) / 2.0
            segmentStartImg'
                | numVisitedArenas' > 0 = _segmentStartFilledImage runProgressScreen
                | otherwise             = segmentStartImg
        drawImage (Pos2 startX y) RightDir runProgressZIndex segmentStartImg'

        let
            numMidSegmentsFilled  = max (numVisitedArenas' - 1) 0
            numMidSegmentsFilled' = min numMidSegmentsFilled numMidSegments
        for_ [0..numMidSegmentsFilled' - 1] $ \i ->
            let pos = Pos2 (startX + segmentStartImgWidth + segmentImgWidth * fromIntegral i) y
            in drawImage pos RightDir runProgressZIndex (_segmentFilledImage runProgressScreen)

        for_ [numMidSegmentsFilled'..numArenas - 2] $ \i ->
            let pos = Pos2 (startX + segmentStartImgWidth + segmentImgWidth * fromIntegral i) y
            in drawImage pos RightDir runProgressZIndex segmentImg

        let
            endX            = startX + totalWidth - segmentEndImgWidth
            isEnd           = currentRoomType == endHallwayRoomType || currentRoomType == endRoomType
            segmentEndImg'
                | isEnd     = _segmentEndFilledImage runProgressScreen
                | otherwise = segmentEndImg
        drawImage (Pos2 endX y) RightDir runProgressZIndex segmentEndImg'

        let
            markerX
                | numVisitedArenas' == 0         = startX + markerStartOffsetX
                | numVisitedArenas' == numArenas = endX + markerEndOffsetX
                | otherwise                      =
                    let midImgsWidth = segmentImgWidth * fromIntegral (numVisitedArenas' - 1)
                    in startX + segmentStartImgWidth + midImgsWidth + markerMidOffsetX
        drawImage (Pos2 markerX y) RightDir runProgressZIndex (_markerImage runProgressScreen)

        setGraphicsClipRect Nothing

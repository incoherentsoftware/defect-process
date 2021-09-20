module World.UI.Voiceover
    ( module World.UI.Voiceover.Types
    , mkVoiceoverUI
    , updateVoiceoverUI
    , drawVoiceoverUI
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (foldlM)
import qualified Data.Text as T

import Constants
import Menu.ZIndex
import Msg
import Util
import Window.Graphics
import World.UI.Voiceover.Types

voiceoverDeathText = "process lost - restarting" :: T.Text

voiceoverSoundPath = "event:/SFX Events/UI/voiceover" :: FilePath

voiceoverFontColor            = Color 0 255 33 255 :: Color
voiceoverTextPosY             = 200.0              :: PosY
opacityLingerSecs             = 1.0                :: Secs
maxOpacityVal                 = 1.0                :: Float
minOpacityVal                 = 0.0                :: Float
opacityFadeInOutMultiplier    = 0.6                :: Float
textBackdropBorderSize        = 5.0                :: Float
textBackdropOpacityMultiplier = 0.5                :: Float

textBackdropColor :: Opacity -> Color
textBackdropColor opacity = Color 30 30 30 alpha
    where alpha = fromIntegral $ round (fromIntegral (opacityToAlpha opacity) * textBackdropOpacityMultiplier)

mkVoiceoverUI :: (GraphicsRead m, MonadIO m) => m VoiceoverUI
mkVoiceoverUI = do
    deathDisplayText <- mkDisplayText voiceoverDeathText AltFont36 voiceoverFontColor
    return $ VoiceoverUI
        { _deathDisplayText = deathDisplayText
        , _displayText      = Nothing
        , _opacity          = VoiceoverAppearOpacity minOpacityVal
        }

updateVoiceoverUI :: MsgsReadWrite UpdateWorldUiMsgsPhase m => VoiceoverUI -> m VoiceoverUI
updateVoiceoverUI voiceoverUI = foldlM processMsg voiceoverUI' =<< readMsgs
    where
        processMsg :: MsgsWrite UpdateWorldUiMsgsPhase m1 => VoiceoverUI -> UiMsgPayload -> m1 VoiceoverUI
        processMsg ui d = case d of
            UiMsgShowVoiceoverText f -> do
                writeMsgs [mkMsg $ AudioMsgPlaySoundCentered voiceoverSoundPath]
                return $ ui
                    { _displayText = Just $ f ui
                    , _opacity     = VoiceoverAppearOpacity minOpacityVal
                    }
            _                        -> return ui

        opacity = case _opacity voiceoverUI of
            VoiceoverAppearOpacity opacityVal    ->
                let opacityVal' = opacityVal + opacityFadeInOutMultiplier * timeStep
                in if
                    | opacityVal' >= maxOpacityVal -> VoiceoverLingerOpacity opacityLingerSecs
                    | otherwise                    -> VoiceoverAppearOpacity opacityVal'
            VoiceoverLingerOpacity ttl
                | ttl <= 0.0                     -> VoiceoverDisappearOpacity maxOpacityVal
                | otherwise                      -> VoiceoverLingerOpacity $ ttl - timeStep
            VoiceoverDisappearOpacity opacityVal ->
                let opacityVal' = max minOpacityVal (opacityVal - opacityFadeInOutMultiplier * timeStep)
                in VoiceoverDisappearOpacity $ opacityVal'

        displayText = case opacity of
            VoiceoverDisappearOpacity opacityVal
                | opacityVal <= minOpacityVal -> Nothing
            _                                 -> _displayText voiceoverUI

        voiceoverUI' = voiceoverUI
            { _opacity     = opacity
            , _displayText = displayText
            }

drawVoiceoverUI :: (GraphicsReadWrite m, MonadIO m) => VoiceoverUI -> m ()
drawVoiceoverUI voiceoverUI = case _displayText voiceoverUI of
    Nothing          -> return ()
    Just displayText ->
        let
            textPos     = Pos2 (virtualRenderWidth / 2.0) voiceoverTextPosY
            textOpacity = Opacity $ case _opacity voiceoverUI of
                VoiceoverAppearOpacity val    -> val
                VoiceoverLingerOpacity _      -> maxOpacityVal
                VoiceoverDisappearOpacity val -> val
        in do
            rectWidth  <- (+ textBackdropBorderSize * 2.0) <$> displayTextWidth displayText
            rectHeight <- (+ textBackdropBorderSize * 2.0) <$> displayTextHeight displayText
            let rectPos = textPos `vecSub` Pos2 (rectWidth / 2.0) (rectHeight / 2.0)
            drawRect rectPos rectWidth rectHeight (textBackdropColor textOpacity) menuOverZIndex

            drawDisplayTextCenteredEx textPos menuOverZIndex NonScaled textOpacity displayText

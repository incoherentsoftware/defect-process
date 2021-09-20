module Menu.UnlocksMenu.Types
    ( module Menu.UnlocksMenu.JSON
    , UnlocksEntryStatus(..)
    , UnlocksEntry(..)
    , UnlocksOverlay(..)
    , UnlocksMenuSelection(..)
    , UnlocksMenuData(..)
    ) where

import Audio.Fmod.Types
import Menu.HelpPopup.Types
import Menu.SoundIndices.Types
import Menu.UnlocksMenu.JSON
import Msg.Payload
import Player.Gun.Types
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Weapon.Types
import Util
import Window.Graphics.DisplayText.Types
import Window.Graphics.Image.Types
import Window.Graphics.InputDisplayText.Types
import Window.Graphics.Opacity
import Window.Graphics.Sprite.Types
import Window.Graphics.SymbolDisplayText.Types
import Window.Graphics.UiControls.Types
import World.Audio.LayeredMusic.Types
import World.Util

data UnlocksEntryStatus
    = UnlockedStatus DisplayText
    | LockedStatus SymbolDisplayText InputDisplayText
    | UnavailableStatus

data UnlocksEntry a = UnlocksEntry
    { _type               :: Maybe a
    , _status             :: UnlocksEntryStatus
    , _cost               :: GoldValue
    , _index              :: Int
    , _upIndex            :: Int
    , _downIndex          :: Int
    , _leftIndex          :: Int
    , _rightIndex         :: Int
    , _button             :: Button
    , _lockedOverlayImage :: Image
    , _msgPayload         :: Maybe ConsoleMsgPayload
    , _helpPopup          :: Maybe HelpPopup
    , _helpPopupActive    :: Bool
    }

data UnlocksOverlay = UnlocksOverlay
    { _pos    :: Pos2
    , _sprite :: Sprite
    }

data UnlocksMenuSelection
    = UnlocksMenuSubSelection Int
    | UnlocksMenuMainMenuSelection
    deriving Eq

data UnlocksMenuData = UnlocksMenuData
    { _backgroundImage          :: Image
    , _totalGoldDisplayText     :: SymbolDisplayText
    , _musicEntries             :: [UnlocksEntry LayeredMusicType]
    , _weaponEntries            :: [UnlocksEntry WeaponType]
    , _gunEntries               :: [UnlocksEntry GunType]
    , _movementSkillEntries     :: [UnlocksEntry MovementSkillType]
    , _secondarySkillEntries    :: [UnlocksEntry SecondarySkillType]
    , _selectedOverlayImage     :: Image
    , _unlocksDisplayText       :: DisplayText
    , _unavailableDisplayText   :: DisplayText
    , _viewInfoInputDisplayText :: InputDisplayText
    , _mainMenuButton           :: Button
    , _insufficientGoldImage    :: Image
    , _insufficientGoldOpacity  :: Opacity
    , _unlocksOverlays          :: [UnlocksOverlay]
    , _selection                :: UnlocksMenuSelection
    , _musicIndex               :: FmodMusicIndex
    , _soundIndices             :: MenuSoundIndices
    }

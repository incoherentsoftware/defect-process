module Menu.HelpPopup.Util
    ( HelpPopupScreenDescription(..)
    , HelpPopupTabDescription(..)
    , HelpPopupDescription(..)
    , generalInfoHelpPopupDescription
    , targetingInfoHelpPopupDescription
    , weaponTypeToHelpPopupDescription
    , gunTypeToHelpPopupDescription
    , movementSkillTypeToHelpPopupDescription
    , secondarySkillTypeToHelpPopupDescription
    , upgradeTypeToHelpPopupDescription
    , musicHelpPopupDescription
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T

import Configs.All.Settings.Menu
import FileCache
import Menu.HelpPopup.Types
import Menu.ZIndex
import Player.Gun.Types
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Upgrade
import Player.Weapon.Types
import Util
import Window.Graphics
import Window.InputState

symbolInputAliasTextPos        = Pos2 1488.0 914.0 :: Pos2
stoneFormSkillSlotTextPos      = Pos2 960.0 587.0  :: Pos2
flightSkillSlotTextPos         = Pos2 815.0 573.0  :: Pos2
fastFallSkillSlotTextPos       = Pos2 811.0 605.0  :: Pos2
stasisBlastSkillSlotTextPos    = Pos2 959.0 570.0  :: Pos2
markRecallSkillSlotText0Pos    = Pos2 670.0 605.0  :: Pos2
markRecallSkillSlotText1Pos    = Pos2 1250.0 605.0 :: Pos2
summonPlatformSkillSlotTextPos = Pos2 959.0 513.0  :: Pos2

variousHelpPackPath = \f -> PackResourceFilePath "data/menu/help/various-help.pack" f
buttonImagePath     = \f -> PackResourceFilePath "data/menu/pause-menu.pack" f
weaponImagePath     = \p f -> PackResourceFilePath ("data/menu/help/weapons/" ++ p) f

data HelpPopupScreenDescription = HelpPopupScreenDescription
    { _imagePath               :: PackResourceFilePath
    , _textOverlayDescriptions :: [HelpPopupTextOverlayDescription]
    }

data HelpPopupTabDescription = HelpPopupTabDescription
    { _imagePath               :: PackResourceFilePath
    , _imageAltOverlayPath     :: Maybe PackResourceFilePath
    , _tabButtonImagePath      :: PackResourceFilePath
    , _tabButtonPos            :: Pos2
    , _textOverlayDescriptions :: [HelpPopupTextOverlayDescription]
    }

data HelpPopupDescription = HelpPopupDescription
    { _iconButtonImagePath :: PackResourceFilePath
    , _popupDescription    :: Either HelpPopupScreenDescription [HelpPopupTabDescription]
    }

inputAliasToSymbolText :: InputAlias -> T.Text
inputAliasToSymbolText = \case
    JumpAlias           -> "{JumpSymbol}"
    WeaponAlias         -> "{WeaponSymbol}"
    ShootAlias          -> "{ShootSymbol}"
    MovementSkillAlias  -> "{MovementSkillSymbol}"
    SecondarySkillAlias -> "{SecondarySkillSymbol}"
    _                   -> "???"

mkSymbolInputAliasPosTextDesc :: InputAlias -> HelpPopupTextOverlayDescription
mkSymbolInputAliasPosTextDesc inputAlias = HelpPopupTextOverlayDescription
    { _text = inputAliasToSymbolText inputAlias <> " : {" <> prettyShow inputAlias <> "}"
    , _draw = drawCentered
    }
    where
        drawCentered :: (GraphicsReadWrite m, InputRead m, MonadIO m) => HelpPopupTextOverlayDraw m
        drawCentered popupTextOverlay =
            drawInputDisplayTextCentered symbolInputAliasTextPos menuOverZIndex (_inputDisplayText popupTextOverlay)

generalInfoHelpPopupDescription :: HelpPopupDescription
generalInfoHelpPopupDescription = HelpPopupDescription
    { _iconButtonImagePath = buttonImagePath "general-info-icon-button.image"
    , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "general-info-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc JumpAlias]
            }
    }

mkLockOnTargetingHelpPosTexts :: MenuConfig -> [HelpPopupTextOverlayDescription]
mkLockOnTargetingHelpPosTexts menuCfg =
    [ mkDescMouseKb pos0 "Aim with mouse cursor"
    , mkDescMouseKb pos1 "Set lock-on target: {LockOnCursorAlias}"
    , mkDescMouseKb pos2 "Cycle lock-on target: {LockOnSwitchTargetAlias}"
    , mkDescGamepad pos3 "Cycle lock-on target: {LockOnSwitchTargetAlias}"
    , mkDescGamepad pos4 "Aim with right analog stick to set lock-on target"
    ]
    where
        drawMouseKb :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> HelpPopupTextOverlayDraw m
        drawMouseKb pos popupTextOverlay = do
            drawInputDisplayTextMouseKb pos menuOverZIndex (_inputDisplayText popupTextOverlay)

        drawGamepad :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> HelpPopupTextOverlayDraw m
        drawGamepad pos popupTextOverlay =
            drawInputDisplayTextGamepad pos menuOverZIndex (_inputDisplayText popupTextOverlay)

        mkDescMouseKb = \pos txt -> HelpPopupTextOverlayDescription txt (drawMouseKb pos)
        mkDescGamepad = \pos txt -> HelpPopupTextOverlayDescription txt (drawGamepad pos)

        (pos0, pos1, pos2, pos3, pos4) = _helpPopupTargetingTextPositions menuCfg

targetingInfoHelpPopupDescription :: MenuConfig -> HelpPopupDescription
targetingInfoHelpPopupDescription menuCfg = HelpPopupDescription
    { _iconButtonImagePath = buttonImagePath "targeting-info-icon-button.image"
    , _popupDescription    = Left $ HelpPopupScreenDescription
        { _imagePath               = variousHelpPackPath "targeting-info-help.image"
        , _textOverlayDescriptions = mkLockOnTargetingHelpPosTexts menuCfg
        }
    }

weaponTypeToHelpPopupDescription :: MenuConfig -> WeaponType -> HelpPopupDescription
weaponTypeToHelpPopupDescription menuCfg wpnType = case wpnType of
    SwordWeapon -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "sword-icon-button.image"
        , _popupDescription    = Right
            [ HelpPopupTabDescription
                { _imagePath               = swordHelpImgPath "sword-basic-help.image"
                , _imageAltOverlayPath     = Nothing
                , _tabButtonImagePath      = swordHelpImgPath "basic-attacks-button.image"
                , _tabButtonPos            = _helpPopupSwordBasicTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            , HelpPopupTabDescription
                { _imagePath               = swordHelpImgPath "sword-special-help.image"
                , _imageAltOverlayPath     = Just $ swordHelpImgPath "sword-special-help-alt-overlay.image"
                , _tabButtonImagePath      = swordHelpImgPath "special-attacks-button.image"
                , _tabButtonPos            = _helpPopupSwordSpecialTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            ]
        }

    GauntletsWeapon -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "gauntlets-icon-button.image"
        , _popupDescription    = Right
            [ HelpPopupTabDescription
                { _imagePath               = gauntletsHelpImgPath "gauntlets-basic-help.image"
                , _imageAltOverlayPath     = Nothing
                , _tabButtonImagePath      = gauntletsHelpImgPath "basic-attacks-button.image"
                , _tabButtonPos            = _helpPopupGauntletsBasicTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            , HelpPopupTabDescription
                { _imagePath               = gauntletsHelpImgPath "gauntlets-special-help.image"
                , _imageAltOverlayPath     = Just $ gauntletsHelpImgPath "gauntlets-special-help-alt-overlay.image"
                , _tabButtonImagePath      = gauntletsHelpImgPath "special-attacks-button.image"
                , _tabButtonPos            = _helpPopupGauntletsSpecialTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            , HelpPopupTabDescription
                { _imagePath               = gauntletsHelpImgPath "gauntlets-charged-help.image"
                , _imageAltOverlayPath     = Nothing
                , _tabButtonImagePath      = gauntletsHelpImgPath "charged-attacks-button.image"
                , _tabButtonPos            = _helpPopupGauntletsChargedTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            ]
        }

    ScytheWeapon -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "scythe-icon-button.image"
        , _popupDescription    = Right
            [ HelpPopupTabDescription
                { _imagePath               = scytheHelpImgPath "scythe-basic-help.image"
                , _imageAltOverlayPath     = Nothing
                , _tabButtonImagePath      = scytheHelpImgPath "basic-attacks-button.image"
                , _tabButtonPos            = _helpPopupScytheBasicTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            , HelpPopupTabDescription
                { _imagePath               = scytheHelpImgPath "scythe-special-help.image"
                , _imageAltOverlayPath     = Just $ scytheHelpImgPath "scythe-special-help-alt-overlay.image"
                , _tabButtonImagePath      = scytheHelpImgPath "special-attacks-button.image"
                , _tabButtonPos            = _helpPopupScytheSpecialTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            , HelpPopupTabDescription
                { _imagePath               = scytheHelpImgPath "scythe-floating-help.image"
                , _imageAltOverlayPath     = Just $ scytheHelpImgPath "scythe-floating-help-alt-overlay.image"
                , _tabButtonImagePath      = scytheHelpImgPath "floating-scythe-attacks-button.image"
                , _tabButtonPos            = _helpPopupScytheFloatingTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            ]
        }

    StaffWeapon -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "staff-icon-button.image"
        , _popupDescription    = Right
            [ HelpPopupTabDescription
                { _imagePath               = staffHelpImgPath "staff-basic-help.image"
                , _imageAltOverlayPath     = Nothing
                , _tabButtonImagePath      = staffHelpImgPath "basic-attacks-button.image"
                , _tabButtonPos            = _helpPopupStaffBasicTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            , HelpPopupTabDescription
                { _imagePath               = staffHelpImgPath "staff-special-help.image"
                , _imageAltOverlayPath     = Just $ staffHelpImgPath "staff-special-help-alt-overlay.image"
                , _tabButtonImagePath      = staffHelpImgPath "special-attacks-button.image"
                , _tabButtonPos            = _helpPopupStaffSpecialTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            ]
        }

    SpiritBladeWeapon -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "spirit-blade-icon-button.image"
        , _popupDescription    = Right
            [ HelpPopupTabDescription
                { _imagePath               = spiritBladeHelpImgPath "spirit-blade-basic-help.image"
                , _imageAltOverlayPath     = Nothing
                , _tabButtonImagePath      = spiritBladeHelpImgPath "basic-attacks-button.image"
                , _tabButtonPos            = _helpPopupSpiritBladeBasicTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            , HelpPopupTabDescription
                { _imagePath               = spiritBladeHelpImgPath "spirit-blade-special-help.image"
                , _imageAltOverlayPath     =
                    Just $ spiritBladeHelpImgPath "spirit-blade-special-help-alt-overlay.image"
                , _tabButtonImagePath      = spiritBladeHelpImgPath "special-attacks-button.image"
                , _tabButtonPos            = _helpPopupSpiritBladeSpecialTabButtonPos menuCfg
                , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc WeaponAlias]
                } :: HelpPopupTabDescription
            ]
        }

    where
        swordHelpImgPath       = \fileName -> weaponImagePath "sword-help.pack" fileName
        gauntletsHelpImgPath   = \fileName -> weaponImagePath "gauntlets-help.pack" fileName
        scytheHelpImgPath      = \fileName -> weaponImagePath "scythe-help.pack" fileName
        staffHelpImgPath       = \fileName -> weaponImagePath "staff-help.pack" fileName
        spiritBladeHelpImgPath = \fileName -> weaponImagePath "spirit-blade-help.pack" fileName

gunTypeToHelpPopupDescription :: GunType -> HelpPopupDescription
gunTypeToHelpPopupDescription = \case
    RevolverGun -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "revolver-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "revolver-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc ShootAlias]
            }
        }

    ShotgunGun -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "shotgun-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "shotgun-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc ShootAlias]
            }
        }

    GrenadeLauncherGun -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "grenade-launcher-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "grenade-launcher-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc ShootAlias]
            }
        }

    ShardGun -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "shard-gun-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "shard-gun-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc ShootAlias]
            }
        }

    SpikeGun -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "spike-gun-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "spike-gun-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc ShootAlias]
            }
        }

    RicochetGun -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "ricochet-gun-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "ricochet-gun-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc ShootAlias]
            }
        }

movementSkillTypeToHelpPopupDescription :: MovementSkillType -> HelpPopupDescription
movementSkillTypeToHelpPopupDescription = \case
    DashSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "dash-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "dash-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc MovementSkillAlias]
            }
        }

    TeleportSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "teleport-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "teleport-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc MovementSkillAlias]
            }
        }

    GrappleSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "grapple-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "grapple-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc MovementSkillAlias]
            }
        }

mkSecondarySkillSlotInputTextDesc :: Pos2 -> SecondarySkillSlot -> HelpPopupTextOverlayDescription
mkSecondarySkillSlotInputTextDesc pos slot = HelpPopupTextOverlayDescription
    { _text = case slot of
        SecondarySkillNeutralSlot -> "{SecondarySkillNeutralInputSymbol}"
        SecondarySkillUpSlot      -> "{SecondarySkillUpInputSymbol}"
        SecondarySkillDownSlot    -> "{SecondarySkillDownInputSymbol}"
    , _draw = drawCentered
    }
    where
        drawCentered :: (GraphicsReadWrite m, InputRead m, MonadIO m) => HelpPopupTextOverlayDraw m
        drawCentered popupTextOverlay =
            drawInputDisplayTextCentered pos menuOverZIndex (_inputDisplayText popupTextOverlay)

secondarySkillTypeToHelpPopupDescription :: SecondarySkillType -> SecondarySkillSlot -> HelpPopupDescription
secondarySkillTypeToHelpPopupDescription secondarySkillType slot = case secondarySkillType of
    StoneFormSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "stone-form-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "stone-form-help.image"
            , _textOverlayDescriptions =
                [ mkSymbolInputAliasPosTextDesc SecondarySkillAlias
                , mkSecondarySkillSlotInputTextDesc stoneFormSkillSlotTextPos slot
                ]
            }
        }

    FlightSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "flight-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "flight-help.image"
            , _textOverlayDescriptions =
                [ mkSymbolInputAliasPosTextDesc SecondarySkillAlias
                , mkSecondarySkillSlotInputTextDesc flightSkillSlotTextPos slot
                ]
            }
        }

    FastFallSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "fast-fall-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "fast-fall-help.image"
            , _textOverlayDescriptions =
                [ mkSymbolInputAliasPosTextDesc SecondarySkillAlias
                , mkSecondarySkillSlotInputTextDesc fastFallSkillSlotTextPos slot
                ]
            }
        }

    StasisBlastSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "stasis-blast-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "stasis-blast-help.image"
            , _textOverlayDescriptions =
                [ mkSymbolInputAliasPosTextDesc SecondarySkillAlias
                , mkSecondarySkillSlotInputTextDesc stasisBlastSkillSlotTextPos slot
                ]
            }
        }

    MarkRecallSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "mark-recall-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "mark-recall-help.image"
            , _textOverlayDescriptions =
                [ mkSymbolInputAliasPosTextDesc SecondarySkillAlias
                , mkSecondarySkillSlotInputTextDesc markRecallSkillSlotText0Pos slot
                , mkSecondarySkillSlotInputTextDesc markRecallSkillSlotText1Pos slot
                ]
            }
        }

    SummonPlatformSkill -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "summon-platform-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "summon-platform-help.image"
            , _textOverlayDescriptions =
                [ mkSymbolInputAliasPosTextDesc SecondarySkillAlias
                , mkSecondarySkillSlotInputTextDesc summonPlatformSkillSlotTextPos slot
                ]
            }
        }

upgradeTypeToHelpPopupDescription :: PlayerUpgradeType -> HelpPopupDescription
upgradeTypeToHelpPopupDescription = \case
    DoubleJumpUpgradeType -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "double-jump-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "double-jump-upgrade-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc JumpAlias]
            }
        }

    MovementSkillUpgradeType -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "movement-skill-upgrade-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "movement-skill-upgrade-help.image"
            , _textOverlayDescriptions = [mkSymbolInputAliasPosTextDesc MovementSkillAlias]
            }
        }

    MeterUpgradeType -> HelpPopupDescription
        { _iconButtonImagePath = buttonImagePath "meter-upgrade-icon-button.image"
        , _popupDescription    = Left $ HelpPopupScreenDescription
            { _imagePath               = variousHelpPackPath "meter-upgrade-help.image"
            , _textOverlayDescriptions = []
            }
        }

musicHelpPopupDescription :: HelpPopupDescription
musicHelpPopupDescription = HelpPopupDescription
    { _iconButtonImagePath = buttonImagePath "general-info-icon-button.image"  -- unused
    , _popupDescription    = Left $ HelpPopupScreenDescription
        { _imagePath               = variousHelpPackPath "music-help.image"
        , _textOverlayDescriptions = []
        }
    }

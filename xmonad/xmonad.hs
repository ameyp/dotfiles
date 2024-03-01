import XMonad

import XMonad.Actions.CycleWS

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.ToggleLayouts

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Ungrab

import qualified XMonad.StackSet as W

main :: IO ()
main = do
  -- For the status bar
  xmprocPolybar <- spawnPipe ("killall polybar; polybar --reload -c=~/.config/polybar/config.ini example")
  -- For the wallpaper
  xmprocFeh <- spawnPipe ("killall feh; feh --bg-scale ~/Pictures/wallpapers/6.jpeg")
  xmonad $ docks . ewmh $ myConfig

myLayoutHook = tiled ||| Grid ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook = composeAll
  [ className =? "Safeeyes" --> doFloat
  , manageDocks
  ]

myConfig = def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , layoutHook = avoidStruts $ spacingWithEdge 3 $ myLayoutHook
    , manageHook = myManageHook <+> manageHook def
    }
  `additionalKeysP`
    [ ("M-f", spawn "firefox")
    , ("M-c", spawn "kitty")
    , ("M-e", spawn "emacsclient -c")
    , ("M-p", spawn "rofi -show run")
    , ("M-<Down>", nextWS)
    , ("M-<Up>",   prevWS)
    , ("M-<F1>", windows $ W.greedyView "1")
    , ("M-<F2>", windows $ W.greedyView "2")
    , ("M-<F3>", windows $ W.greedyView "3")
    , ("M-<F4>", windows $ W.greedyView "4")
    , ("M-<F5>", windows $ W.greedyView "5")
    , ("M-<F6>", windows $ W.greedyView "6")
    , ("M-<F7>", windows $ W.greedyView "7")
    , ("M-<F8>", windows $ W.greedyView "8")
    , ("M-<F9>", windows $ W.greedyView "9")
    , ("M-w", kill)
    ]

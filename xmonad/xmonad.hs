import XMonad

import XMonad.Actions.CycleWS

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Ungrab

main :: IO ()
main = do
  -- For the status bar
  xmprocPolybar <- spawnPipe ("killall polybar; polybar --reload -c=~/.config/polybar/config.ini example")
  -- For the wallpaper
  xmprocFeh <- spawnPipe ("killall feh; feh --bg-scale ~/Pictures/wallpapers/6.jpeg")
  xmonad $ docks . ewmhFullscreen . ewmh $ myConfig

myConfig = def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , layoutHook = avoidStruts $ spacingWithEdge 5 $ layoutHook def
    , manageHook = manageHook def <+> manageDocks
    }
  `additionalKeysP`
    [ ("M-f", spawn "firefox")
    , ("M-c", spawn "kitty")
    , ("M-e", spawn "emacsclient -c")
    , ("M-<Down>", nextWS)
    , ("M-<Up>",   prevWS)
    ]

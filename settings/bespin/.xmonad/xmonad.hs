import XMonad

import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid

import qualified XMonad.StackSet as W
import Data.Map (union, fromList)




-- withUrgencyHook veroorzaakt de pidgin-crash
main = xmonad $ withUrgencyHook NoUrgencyHook configuur
-- main = xmonad configuur



kBorderNormaal = "black"
kBorderSelect  = "white"
kBalk          = "black"
datFont        = "-*-terminus-medium-*-*-*-13-*-*-*-*-*-*-*" 


wextra totaal xs = xs ++ map show [l+1..l+1+n]
  where l = length xs
        n = totaal - l


configuur = defaultConfig {
        terminal           = "gnome-terminal",
        borderWidth        = 1,
        workspaces         = ["com","tekst","web","mail","terminals","6","7","8","muziek"],
        normalBorderColor  = kBorderNormaal,
        focusedBorderColor = kBorderSelect,
        defaultGaps        = [(0,15,0,0)],
        
        keys = (\c -> extraKeys c `union` keys defaultConfig c),
        
        logHook = dynamicLogWithPP logPP,
        layoutHook = layouts,
        manageHook = manageer
    }

logPP = defaultPP {
    ppCurrent = \i -> "<fc=#ffff00>[" ++ i ++ "]</fc>",
    ppVisible = \i -> i,
    ppHidden  = \i -> i,
    ppHiddenNoWindows = \i -> "",
    ppUrgent = \i -> "<fc=#ff00ff>" ++ i ++ "</fc>",
    ppSep = " | ",
    ppWsSep = " "
  }



layouts = windowNavigation $
              onWorkspace "com" Grid $
              onWorkspace "tekst" (mirrorTiled ||| tiled) $
              onWorkspaces ["web","mail"] (tabs ||| Grid) $
              (tiled ||| mirrorTiled ||| tabs ||| Grid ||| full)
        where full = noBorders Full
              tiled       =          Tall nmaster delta (3/5)
              mirrorTiled = Mirror $ Tall nmaster delta (4/5)
              nmaster = 1
              delta = 3/100
              tabs  = tabbed shrinkText tabconf



manageer  = composeAll [
    -- className   =? "Firefox" --> doF (W.focusDown . W.shift "web")
    className   =? "Firefox" --> doF (W.shift "web")
    , className =? "Pidgin"  --> doF (W.shift "com")
    , className =? "Thunderbird-bin" --> doF (W.shift "mail")
    , className =? "Rhythmbox" --> doF (W.shift "muziek")
  ]




tabconf = Theme {
    activeColor = "white",
    inactiveColor = "black",
    urgentColor = "#a0ffd0",
    
    activeBorderColor   = "#444444",
    inactiveBorderColor = "#444444",
    urgentBorderColor   = "#444444",
    
    activeTextColor     = "black",
    inactiveTextColor   = "white",
    urgentTextColor     = "black",
    
    fontName = datFont,
    
    decoWidth = 100,
    decoHeight = 15
  }



extraKeys conf@(XConfig {XMonad.modMask = modMask}) = fromList [

    -- Kleinere master
    ((modMask, xK_minus), sendMessage Shrink)

    -- Grotere master
    , ((modMask, xK_equal), sendMessage Expand)

    -- Naar volgend venster
    , ((modMask, xK_Tab), windows W.focusDown)
    
    -- Naar vorig venster
    , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp)

    -- Verwissel met volgend venster
    , ((modMask .|. controlMask, xK_Tab), windows W.swapDown)

    -- Verwissel met vorig venster
    , ((modMask .|. shiftMask .|. controlMask, xK_Tab), windows W.swapUp)

    -- Navigatie
    , ((modMask,               xK_Right), sendMessage $ Go R)
    , ((modMask,               xK_Left ), sendMessage $ Go L)
    , ((modMask,               xK_Up   ), sendMessage $ Go U)
    , ((modMask,               xK_Down ), sendMessage $ Go D)
    , ((modMask .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
    
    -- launch dmenu
    , ((modMask,               xK_p     ), spawn ("exe=`dmenu_path | dmenu -b -nb black -nf white -sb white -sf black -fn " ++ datFont ++ "` && eval \"exec $exe\""))

    , ((modMask,               xK_F12   ), spawn ("gnome-screensaver-command --lock"))

    , ((modMask              , xK_BackSpace), focusUrgent)

  ]


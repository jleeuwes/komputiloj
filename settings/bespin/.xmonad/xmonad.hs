import XMonad

import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid

import qualified XMonad.StackSet as W
import Data.Map (union, fromList)




-- withUrgencyHook veroorzaakt de pidgin-crash
-- main = xmonad $ withUrgencyHook NoUrgencyHook configuur
main = xmonad configuur



kBorderNormaal = "white"
kBorderSelect  = "black"
kBalk          = "#500070"
datFont        = "-*-terminus-medium-*-*-*-13-*-*-*-*-*-*-*" 


wextra totaal xs = xs ++ map show [l+1..l+1+n]
  where l = length xs
        n = totaal - l


configuur = defaultConfig {
        terminal           = "gnome-terminal",
        borderWidth        = 2,
        workspaces         = ["com","tekst","web","mail","terminals","6","7","8","muziek"],
        normalBorderColor  = kBorderNormaal,
        focusedBorderColor = kBorderSelect,
        defaultGaps        = [(15,0,0,0)],
        
        keys = (\c -> extraKeys c `union` keys defaultConfig c),
        
        logHook = dynamicLog,
        layoutHook = layouts,
        manageHook = manageer
    }




layouts = windowNavigation (tiled ||| Mirror tiled ||| tabbed shrinkText tabconf ||| Grid ||| noBorders Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 3/5

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100




manageer  = composeAll [
    className   =? "Firefox-bin" --> doF (W.shift "web")
    , className =? "Pidgin"  --> doF (W.shift "com")
    , className =? "Thunderbird-bin" --> doF (W.shift "mail")
    , className =? "Rhythmbox" --> doF (W.shift "muziek")
  ]




tabconf = TConf {
    activeColor = "white",
    inactiveColor = kBalk,
    urgentColor = "#a040c0",
    
    activeBorderColor   = "white",
    inactiveBorderColor = "#200040",
    urgentBorderColor   = "#200040",
    
    activeTextColor     = "black",
    inactiveTextColor   = "white",
    urgentTextColor     = "white",
    
    fontName = datFont,
    tabSize  = 15
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
    , ((modMask,               xK_p     ), spawn ("exe=`dmenu_path | dmenu -nb \\#500070 -nf white -sb white -sf black -fn " ++ datFont ++ "` && eval \"exec $exe\""))

    , ((modMask,               xK_F12   ), spawn ("gnome-screensaver-command --lock"))

    , ((modMask              , xK_BackSpace), focusUrgent)

  ]


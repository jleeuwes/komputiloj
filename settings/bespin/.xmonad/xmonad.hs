import XMonad

import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.DynamicLog

import qualified XMonad.StackSet as W
import Data.Map (union, fromList)




main = xmonad configuur



kBorderNormaal = "#c0c0c0"
kBorderSelect  = "black"
kBalk          = "#500070"
datFont        = "-*-fixed-medium-r-*-*-13-*-*-*-*-*-*-*" 


wextra totaal xs = xs ++ map show [l+1..l+1+n]
  where l = length xs
        n = totaal - l


configuur = defaultConfig {
        terminal           = "gnome-terminal",
        borderWidth        = 2,
        workspaces         = wextra 9["com","tekst","web","mail","terminals"],
        normalBorderColor  = kBorderNormaal,
        focusedBorderColor = kBorderSelect,
        defaultGaps        = [(15,0,0,0)],
        
        keys = (\c -> extraKeys c `union` keys defaultConfig c),
        
        logHook = dynamicLog,
        layoutHook = layouts,
        manageHook = composeAll []
    }




layouts = windowNavigation (tiled ||| Mirror tiled ||| tabbed shrinkText tabconf ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 3/5

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100




tabconf = TConf {
    activeColor = "white",
    inactiveColor = kBalk,
    urgentColor = "white",
    
    activeBorderColor   = "white",
    inactiveBorderColor = kBalk,
    urgentBorderColor   = "white",
    
    activeTextColor     = "black",
    inactiveTextColor   = "white",
    urgentTextColor     = kBalk,
    
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
    , ((modMask,               xK_p     ), spawn ("exe=`dmenu_path | dmenu -nb \\#500070 -nf white -sb white -sf black` && eval \"exec $exe\""))

  ]


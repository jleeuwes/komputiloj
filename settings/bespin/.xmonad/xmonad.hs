import XMonad

import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.DynamicLog

import qualified XMonad.StackSet as W
import Data.Map (union, fromList)




main = xmonad configuur




configuur = defaultConfig {
        terminal           = "gnome-terminal",
        borderWidth        = 2,
        workspaces         = ["com","werk","web","mail"],
        normalBorderColor  = "#c0c0c0",
        focusedBorderColor = "#000000",
        defaultGaps        = [(15,0,0,0)],
        
        keys = (\c -> extraKeys c `union` keys defaultConfig c),
        
        logHook = dynamicLog,
        layoutHook = layouts,
        manageHook = composeAll []
    }




layouts = windowNavigation (tiled ||| Mirror tiled ||| tabbed shrinkText defaultTConf ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 3/5

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100




extraKeys conf@(XConfig {XMonad.modMask = modMask}) = fromList [

    -- Kleinere master
    ((modMask, xK_minus), sendMessage Shrink)

    -- Grotere master
    , ((modMask, xK_equal), sendMessage Expand)

    -- Naar volgend venster
    , ((modMask, xK_Tab), windows W.focusUp)
    
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
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu -nb \\#500070 -nf white -sb white -sf black` && eval \"exec $exe\"")

  ]


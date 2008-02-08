import XMonad

import qualified XMonad.StackSet as W
import Data.Map (union, fromList)




main = xmonad defaults




defaults = defaultConfig {
        terminal           = "gnome-terminal",
        borderWidth        = 2,
        workspaces         = ["com","werk","web","mail"],
        normalBorderColor  = "#c0c0c0",
        focusedBorderColor = "#000000",
        defaultGaps        = [(0,0,0,0)],
        
        keys = (\c -> extraKeys c `union` keys defaultConfig c),
        
        layoutHook = layouts,
        manageHook = composeAll []
    }




layouts = tiled ||| Mirror tiled ||| Full
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

    -- Naar volgend venster
    ((modMask, xK_Down), windows W.focusDown)

    -- Naar vorig venster
    , ((modMask, xK_Up), windows W.focusUp)

    -- Verwissel met volgend venster
    , ((modMask .|. shiftMask, xK_Down), windows W.swapDown)

    -- Verwissel met vorig venster
    , ((modMask .|. shiftMask, xK_Up), windows W.swapUp)

    -- Kleinere master
    , ((modMask, xK_Left), sendMessage Shrink)

    -- Grotere master
    , ((modMask, xK_Right), sendMessage Expand)

  ]


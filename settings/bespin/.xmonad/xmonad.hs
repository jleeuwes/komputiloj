import XMonad

import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Util.Run (spawnPipe)

import qualified XMonad.StackSet as W
import Data.Map (union, fromList)
import System.IO (hPutStrLn)



-- withUrgencyHook veroorzaakt de pidgin-crash
main = do
  pijp <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ configuur pijp



kBorderNormaal = "black"
kBorderSelect  = "white"
kBalk          = "black"
datFont        = "-*-terminus-medium-*-*-*-13-*-*-*-*-*-*-*" 


wextra totaal xs = xs ++ map show [l+1..l+1+n]
  where l = length xs
        n = totaal - l


configuur pijp = defaultConfig {
        modMask            = mod4Mask,
        terminal           = "gnome-terminal",
        borderWidth        = 2,
        workspaces         = ["com","tekst","web","mail","terminals","6","7","8","muziek"],
        normalBorderColor  = kBorderNormaal,
        focusedBorderColor = kBorderSelect,
        defaultGaps        = [(0,15,0,0)],
        
        keys = (\c -> extraKeys c `union` keys defaultConfig c),
        
        logHook = dynamicLogWithPP $ logPP pijp,
        layoutHook = layouts,
        manageHook = manageer
    }

logPP pijp = defaultPP {
    ppCurrent = \i -> "<fc=#ffff00>[" ++ i ++ "]</fc>",
    ppVisible = \i -> "<fc=#ff7700>(" ++ i ++ ")</fc>",
    ppHidden  = \i -> "<fc=#707070>" ++ i ++ "</fc>",
    ppHiddenNoWindows = \i -> "",
    ppUrgent = \i -> "<fc=#ff00ff>" ++ schoon i ++ "*</fc>",
    ppSep = " ",
    ppWsSep = " ",

    ppLayout = layoutCode,
    ppOrder = \[ws, l, t] -> [l, ws, t],

    ppOutput = hPutStrLn pijp
  }

schoon = schoon' False
schoon' _       [] = []
schoon' _   ('<':xs) = schoon' True xs
schoon' _   ('>':xs) = schoon' False xs
schoon' True  (_:xs) = schoon' True xs
schoon' False (x:xs) = x : schoon' False xs

layoutCode "Grid" = "#"
layoutCode "Tall" = "|"
layoutCode "Mirror Tall" = "-"
layoutCode "Tabbed Simplest" = "T"
layoutCode "Full" = "F"
layoutCode i      = '?' : i


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



extraKeys conf@(XConfig {XMonad.modMask = modMask}) = fromList ([

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

    -- scherm locken
    , ((modMask,               xK_F12   ), spawn ("gnome-screensaver-command --lock"))

    -- naar aandachtvragend venster
    , ((modMask              , xK_BackSpace), focusUrgent)

    -- hack de WM-naam om Java misschien te laten werken
    , ((modMask .|. controlMask .|. shiftMask, xK_z), setWMName "LG3D")

  ] ++
    -- overgenomen van http://paste.lisp.org/display/58874, snap er zo snel geen hol van:
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Page_Down, xK_Page_Up] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  )


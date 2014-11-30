--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit

import XMonad.Actions.CycleWS (toggleWS)

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect
import XMonad.Layout.IM

import XMonad.Util.Run (safeSpawn)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return),   safeSpawn (XMonad.terminal conf) [])
    , ((modm .|. shiftMask, xK_KP_Enter), safeSpawn (XMonad.terminal conf) [])

    -- super+p heeft ruzie met dat rare touchpadmediaknopje op mijn HP
    -- launch dmenu
    , ((modm, xK_p),                      dmenu_run)
    , ((modm, xK_w),                      safeSpawn "wachtwoord-balk" [])
    
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    
    -- Vang alt-tab af (want ga ik vaak nog proberen), maar doe niks, om het af
    -- te leren
    , ((mod1Mask,           xK_Tab   ), return ())

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm,               xK_KP_Enter), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    
    -- Dingen omdraaien
    , ((modm              , xK_x), sendMessage $ Toggle REFLECTX)
    , ((modm              , xK_y), sendMessage $ Toggle REFLECTY)

    -- Jump to urgent window
    , ((modm              , xK_BackSpace), focusUrgent)
    
    -- Switchen tussen twee workspaces
    , ((modm              , xK_grave), toggleWS)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    
    -- Multimedia keys (zie /usr/include/X11/XF86keysym.h voor codes)
    -- , ((0, 0x1008FF14), spawn "media playpause")
    -- , ((0, 0x1008FF16), spawn "media prev")
    -- , ((0, 0x1008FF17), spawn "media next")
    -- , ((0, 0x1008FF11), spawn "media vol-")
    -- , ((0, 0x1008FF12), spawn "media mute")
    -- , ((0, 0x1008FF13), spawn "media vol+")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modm, button4), const $ sendMessage Expand)
    , ((modm, button5), const $ sendMessage Shrink)

    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = layoutHintsToCenter $ smartBorders $ avoidStruts $
           mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $
           -- gimpMod $ imMod
           (wide ||| tall ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tall    = Tall 1 delta (3/5)
     wide    = Mirror $ Tall 1 delta (4/5+delta)

     -- The default number of windows in the master pane
     nmaster = 1

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Omring standaard layout met ruimte voor gimp-balkjes, indien van toepassing
-- (http://nathanhowell.net/2009/03/08/xmonad-and-the-gimp/)
-- gimpMod
--   = withIM (barSz) (Role "gimp-toolbox")
--   . reflectHoriz
--   . withIM (barSz / (1-barSz)) (Role "gimp-dock")
--   . reflectHoriz
--   where barSz = 0.15
-- 
-- imMod
--   = reflectHoriz . withIM (0.1335) (Role "contact_list" `Or` Role "buddy_list") . reflectHoriz

layoutNm = layoutNm' False False False . words
--variant met ascii-reflects
-- layoutNm' rx ry ln = case ln of
--   ("IM":xs)         -> layoutNm' rx ry xs
--   ["Tall"]          | rx        -> "=|"
--                     | otherwise -> "|="
--   ["Mirror","Tall"] | ry        -> "--"
--                     | otherwise -> "__"
--   ["Full"]          -> "[]"
--   ("ReflectX":xs)   -> layoutNm' (not rx) ry xs
--   ("ReflectY":xs)   -> layoutNm' rx (not ry) xs
--   (x:xs)            -> x ++ " " ++ layoutNm' rx ry xs
--   []                -> ""

layoutNm' rx ry m ln = case ln of
  ("IM":xs)         -> layoutNm' rx ry m xs
  ("Mirror":xs)     -> layoutNm' rx ry (not m) xs
  ("ReflectX":xs)   -> layoutNm' (not rx) ry m xs
  ("ReflectY":xs)   -> layoutNm' rx (not ry) m xs
  [x]               -> ref (atom x)
  (x:xs)            -> x ++ " " ++ layoutNm' rx ry m xs
  []                -> ""
  where ref = refC rx 'ˣ' . refC ry 'ʸ' . refC m '↻'
        refC True  c  = (c:)
        refC False _  = (' ':)
        atom "Tall"   = "T"
        atom "Full"   = "F"

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook =
    -- (fmap not isDialog --> doF avoidMaster) <+>
    composeAll -- [ className =? "MPlayer"        --> doFloat
    [ resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    -- , className =? "Pidgin" <||> className =? "pidgin"  --> doShift "com"
    , manageDocks
    ]

-- Avoid changing master on new window creation (https://bbs.archlinux.org/viewtopic.php?id=94969)
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    otherwise           -> c

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = statusBar "xmobar" myPP toggleStrutsKey myConfig
      >>= return . withUrgencyHook NoUrgencyHook
      >>= xmonad
  where toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myPP :: PP
myPP = defaultPP
  { ppCurrent         = xmobarColor "#ffffff" "#" -- . wrap "[" "]"
  , ppVisible         = wrap "(" ")"
  , ppHidden          = xmobarColor "#707070" ""
  , ppHiddenNoWindows = xmobarColor "#404040" ""
  , ppUrgent          = xmobarColor "#ff66ff" "" . xmobarStrip
  , ppSep             = " " -- xmobarColor "#404040" "" " / "
  , ppWsSep           = " "
  , ppTitle           = xmobarColor "#6666ff" "" . shorten 80
  , ppLayout          = xmobarColor "#f0e040" "" . layoutNm
  , ppOrder           = id
  -- , ppOutput          = putStrLn
  -- , ppSort            = getSortByIndex
  , ppExtras          = []
  }

dmenu_run = safeSpawn "dmenu_run"
  ["-nb", "#000", "-nf", "#fff", "-sb", "#fff", "-sf", "#000", "-fn",
  "xft:inconsolata:size=14"]

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
myConfig = defaultConfig {
      -- simple stuff
        terminal           = "urxvt",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        -- numlockMask        = myNumlockMask,
        workspaces         = ["com","text","web","mail","sys","6","7","8","music"],
        normalBorderColor  = "#000000",
        focusedBorderColor = "#a0a0a0",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }


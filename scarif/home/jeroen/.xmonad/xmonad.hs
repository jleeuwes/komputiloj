{-# LANGUAGE FlexibleContexts #-}
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

import Numeric

import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect
import XMonad.Layout.IM

import XMonad.Util.Run (safeSpawn)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Graphics.X11.ExtraTypes.XF86

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal (TODO updatePointer doesn't work here; add it to the hook for new windows?)
    [ ((modm .|. shiftMask, xK_Return),   safeSpawn (XMonad.terminal conf) [])
    , ((modm .|. shiftMask, xK_KP_Enter), safeSpawn (XMonad.terminal conf) [])

    -- launch dmenu
    , ((modm, xK_p),                      runDmenu "dmenu_run")
    , ((modm, xK_w),                      runDmenu "wachtwoord-balk")
    , ((modm, xK_s),                      runDmenu "beeld-balk")
    
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown >> doUpdatePointer)
    
    -- Vang alt-tab af (want ga ik vaak nog proberen), maar doe niks, om het af
    -- te leren
    , ((mod1Mask,           xK_Tab   ), return ())

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown >> doUpdatePointer)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp >> doUpdatePointer)

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster >> doUpdatePointer)

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster >> doUpdatePointer)
    , ((modm,               xK_KP_Enter), windows W.swapMaster >> doUpdatePointer)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown >> doUpdatePointer)

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp >> doUpdatePointer)

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink >> doUpdatePointer)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand >> doUpdatePointer)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused (windows . W.sink) >> doUpdatePointer)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1) >> doUpdatePointer)

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)) >> doUpdatePointer)
    
    -- Dingen omdraaien (gebruik ik niet echt)
    -- , ((modm              , xK_x), sendMessage $ Toggle REFLECTX)
    -- , ((modm              , xK_y), sendMessage $ Toggle REFLECTY)

    -- Jump to urgent window
    , ((modm              , xK_BackSpace), focusUrgent >> doUpdatePointer)
    
    -- Switchen tussen twee workspaces
    , ((modm              , xK_grave), toggleWS >> doUpdatePointer)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Switch to a not-yet-logged-in user (otherwise use Ctrl-Alt-F8 and co)
    , ((0, xF86XK_LaunchA), spawn "dm-tool switch-to-greeter")

    -- Brightness control
    -- Step size 23 is chosen so we can go from 852 (100%) to 1 (absolute minimum brightness).
    -- When going up from 0, the min-value options make sure we get back to a proper multiple-of-23-plus-1.
    -- Without shift we do 10 steps, which approximately cuts the space in four brightness levels,
    -- and also goes down to 1 (and then 0 if you press again).
    , ((0         , xF86XK_MonBrightnessUp   ), spawn "brightnessctl set +230 --min-value=231")
    , ((0         , xF86XK_MonBrightnessDown ), spawn "brightnessctl set 230-")
    , ((shiftMask , xF86XK_MonBrightnessUp   ), spawn "brightnessctl set +23 --min-value=24")
    , ((shiftMask , xF86XK_MonBrightnessDown ), spawn "brightnessctl set 23-")
    
    -- Multimedia keys
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows (f i) >> doUpdatePointer)
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
myLayout = layoutHintsToCenter $ smartSpacing 2 $ smartBorders $ avoidStruts $
           -- mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $
           -- gimpMod $ imMod
           (wide ||| tall ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tall    = Tall 1 (3/100) (3/5) -- TODO tweak as good as wide
     wide    = Mirror $ Tall 1
                  (22.54 / 1080) -- increments - perfect for our border+strut size + terminal line height at 1080p
                  (761/1000) -- starting ratio - perfect for a terminal under or above a browser,
                             -- or two terminals, on 1080p

     -- The default number of windows in the master pane
     nmaster = 1

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
    , isDialog                      --> doFloat
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
updateBackgroundHook = do
    workspace <- gets (W.tag . W.workspace . W.current . windowset)
    setBgX "#000000" -- prevent more burn-in -- $ withIndex workspaceColor workspace

  where
    -- Set background using hsetroot because that is compatible with
    -- xfce-terminal (launching a process on each change is not ideal but hey)
    setBgH hexcolor = liftIO $ safeSpawn "/run/current-system/sw/bin/hsetroot" ["-solid", hexcolor]

    -- Set background by painting on the root window (not compatible with
    -- xfce-temrinal transparency)
    setBgX hexcolor = do
        disp <- asks display
        root <- asks theRoot
        liftIO $ do
            setWindowBackground disp root $ fromHex hexcolor
            clearWindow disp root
    fromHex = head0 . map fst . filter (null . snd) . readHex . tail
    head0 [] = 0
    head0 (x:_) = x

doUpdatePointer = updatePointer (0.5, 0.5) (0.0, 0.0)

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

-- Leuke vormpjes:
-- https://en.wikipedia.org/wiki/Geometric_Shapes
-- ...
-- ▪ ↓ ⋅ ▼ ◆ ● ◉ ○ ◌ ◎
myPP :: PP
myPP = def
  { ppCurrent         = withIndex $ \i -> xmobarColor "#000000" (workspaceColor i) " ● " -- (workspaceText Hi i)
  , ppVisible         = withIndex $ \i -> xmobarColor "#000000" (workspaceColor i) " ◉ " -- whatever, we don't use xinerama
  , ppHidden          = withIndex $ \i -> xmobarColor "#000000" (workspaceColor i) " ○ "
  , ppHiddenNoWindows = withIndex $ \i -> xmobarColor "#000000" (workspaceColor i) " ◠ "
  , ppUrgent          = withIndex $ \i -> xmobarColor "#000000" (workspaceColor i) " ◎ " -- . xmobarStrip
  , ppSep             = " " -- xmobarColor "#404040" "" " / "
  , ppWsSep           = ""
  , ppTitle           = xmobarColor "#6666ff" "" . shorten 60
  , ppTitleSanitize   = xmobarStrip
  , ppLayout          = const "" -- xmobarColor "#f0e040" "" . layoutNm
  , ppOrder           = id
  -- , ppOutput          = putStrLn
  -- , ppSort            = getSortByIndex
  , ppExtras          = []
  }

-- xmobarc fg nm = xmobarColor fg $ workspaceColor nm

workspaceColor = (workspaceColors !!)
  where
    workspaceColors
      = "#000000"
      : cycle
        [ "#f04040"
        , "#ffb040"
        , "#ffff40"
        , "#40f040"
        , "#20e0f0"
        , "#5050ff"
        , "#f050ff"
        , "#ffffff"
        , "#aaaaaa"
        ]

data HiLo = Hi | Lo

workspaceText Hi = pad . ((:[]) . (digits !!))
  where
    digits = "⁰¹²³⁴⁵⁶⁷⁸⁹" ++ repeat '⁺'
workspaceText Lo = pad . ((:[]) . (digits !!))
  where
    digits = "₀₁₂₃₄₅₆₇₈₉" ++ repeat '₊'

withIndex :: (Int -> String) -> String -> String
withIndex f = f . index
  where
    index
      = read . ('0' :) . filter (`elem` "0123456789")

runDmenu cmd = safeSpawn cmd dmenu_opts
  where
    dmenu_opts
      = ["-b", "-nb", "#000", "-nf", "#fff", "-sb", "#fff", "-sf", "#000", "-fn", "inconsolata:size=14"]

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
myConfig = def {
      -- simple stuff
        terminal           = "xfce4-terminal",
        focusFollowsMouse  = True,
        borderWidth        = 2,
        modMask            = mod4Mask,
        -- numlockMask        = myNumlockMask,
        workspaces         = map show [1..9], -- ¹²³⁴⁵⁶⁷⁸⁹₁₂₃₄₅₆₇₈₉
        normalBorderColor  = "#a0a0a0",
        focusedBorderColor = "#000000",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = updateBackgroundHook,
                           -- Don't use updatePointer here, because it messes with drag-and-drop
                           -- and makes the pointer jump around when going over the root window;
                           -- instead we added doUpdatePointer to most window-related keyboard shortcuts
        startupHook        = myStartupHook
    }


import XMonad

import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import Data.Map (union, fromList)
import Data.List hiding (union)
import Data.Char
import System.IO (hPutStrLn)
import System.IO.Unsafe
import System.Directory (getDirectoryContents)

import Control.Concurrent
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Maybe
import Text.Regex
import Numeric (showHex)


dzenCmd = "dzen2 -fg white -h 15 -y 785 -fn " ++ datFont
datFont        = "-misc-fixed-bold-*-*-*-13-*-*-*-*-*-*-*"

-- withUrgencyHook veroorzaakt de pidgin-crash -- NIET MEER \o/
main = do
  pijp <- spawnPipe $ dzenCmd ++ " -e onstart=lower -bg black -x 0 -ta l"
  extraDzen
  xmonad $ withUrgencyHook NoUrgencyHook $ configuur pijp


-- voor leesbare foutmeldingen bij compileerfouten, voeg regel toe in /etc/X11/app-defaults/Xmessage-color:
-- *Text*foreground: white

iconPath = "/home/jeroen/.xmonad/icons/"
wsIconPath = iconPath ++ "workspaces/"
lIconPath  = iconPath ++ "layouts/"
extraIconPath = iconPath ++ "extra/"

iconsFor idir = unsafePerformIO $ do
  dir <- getDirectoryContents idir
  return [take (length f - 4) f | f <- dir, ".xbm" `isSuffixOf` f] -- filter (isSuffixOf ".xbm") dir

wsIconsFor = iconsFor wsIconPath
lIconsFor  = iconsFor lIconPath

kBorderNormaal = "black"
kBorderSelect  = "white"
kBalk          = "black"


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
    ppCurrent = \i -> "^fg(black)^bg(white)" ++ blokje i ++ "^fg()^bg()",
    ppVisible = \i -> "^fg(black)^bg(#ff7700)" ++ blokje i ++ "^fg()^bg()",
    ppHidden  = \i -> "^fg(#707070)^bg(#202020)" ++ blokje i ++ "^fg()^bg()",
    ppHiddenNoWindows = \i -> "^bg(#202020)^p(15)^bg()",
    ppUrgent = \i -> "^fg(#00ff00)^bg(#202020)" ++ schoon i ++ "^fg()^bg()",
    ppSep = "^p(10)",
    ppWsSep = "",

    ppLayout = layoutIcon,
    ppOrder = order,

    ppExtras = [io nowPlaying >>= return . Just],

    ppOutput = hPutStrLn pijp
  }
  where order (ws:l:t:rest) = l : ws : t : rest
        order gek           = gek


blokje = wsIcon

icon iPath iSet i = concat ["^i(", iPath, ic, ".xbm", ")"]
  where ic = if i `elem` iSet then i else "unknown"
wsIcon = icon wsIconPath wsIconsFor
layoutIcon i = concat ["^fg(#707070)", ic, "^fg()"]
  where ic = icon lIconPath lIconsFor $ filter (not . isSpace) i

marge m txt = concat ["^p(", ms, ")", txt, "^p(", ms, ")"]
  where ms = show m

safe [] = []
safe ('^':xs) = '^' : '^' : safe xs
safe (x:xs)   = x : safe xs

schoon = schoon' False
schoon' _       [] = []
schoon' False ('^':'^':xs) = '^' : schoon' False xs
schoon' False ('^':'i':xs) = '^' : 'i' : schoon' False xs
schoon' _     ('^':xs) = schoon' True xs
schoon' True  (')':xs) = schoon' False xs
schoon' True  (_:xs) = schoon' True xs
schoon' False (x:xs) = x : schoon' False xs

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




-- extra dzen-balk

join j = concat . intersperse j

extraDzen = do
  stat <- spawnPipe $ dzenCmd ++ " -e onstart=raise -x 1080 -w 200 -ta r -bg #202020"
  forkIO $ extraLoop stat

toSeconds = floor . (*1000000)

extraLoop stat = do
  extraStat >>= hPutStrLn stat
  threadDelay $ toSeconds 0.5
  extraLoop stat

extraStat = do
  stats <- sequence [battery, datetime] --nowPlaying zit in 'normale' balk
  return $ join " " stats

extraIcon i = concat ["^i(", extraIconPath, i, ".xbm)"]

nowPlaying = eetError "" $ do
  np <- runProcessWithInput "/home/jeroen/bin/np" [] ""
  let lns = lines np
  return $ if null lns then "" else concat ["^fg(#7070ff)", extraIcon "nowPlaying", head lns, "^fg()"]

batteryColor b | b < 10    = "^fg(black)^bg(red)"
               | otherwise = concat ["^fg(#", byteI, byteH, "00)"]
                           where byte  = 255 * b `div` 100
                                 byteH = showHex byte ""
                                 byteI = showHex (255-byte) ""

eetError :: a -> IO a -> IO a
eetError def io = catch io (\e -> do putStrLn (show e) ; return def)

battery = eetError "" $ do
  info <- readFile "/proc/acpi/battery/BAT1/info"
  state <- readFile "/proc/acpi/battery/BAT1/state"
  power <- readFile "/proc/acpi/ac_adapter/ACAD/state"
  let bat = do
      remain <- lookup "remaining capacity" $ splitInfo state
      total <- lookup "last full capacity" $ splitInfo info
      return $ amp remain * 100 `div` amp total
  let acStr = case lookup "state" $ splitInfo power of
          Nothing         -> ""
          Just "on-line"  -> extraIcon "adapter"
          Just "off-line" -> extraIcon "batterij"
          _               -> "?"
  let batStr = maybe "" (\b -> concat [batteryColor b, acStr, show b, "%", "^fg()^bg()"]) bat
  return batStr

amp = read . takeWhile isDigit

splitInfo = catMaybes . map (ltup . splitRegex (mkRegex ":\\s*")) . lines
  where ltup [a,b] = Just (a,b)
        ltup _     = Nothing

datetime = do
  zt <- getZonedTime
  let lt = zonedTimeToLocalTime zt
  let (yy,mm,dd) = toGregorian $ localDay lt
  let time = localTimeOfDay lt
  return $ concat [show dd, " ", maand mm, " ", show yy, " ", nul $ show $ todHour time, ":", nul $ show $ todMin time]

maand = (maanden !!)
maanden = ["jan", "feb", "maart", "april", "mei",
  "juni", "juli", "augustus", "september", "oktober", "november", "december"]

nul []  = []
nul [x] = ['0', x]
nul xs  = xs

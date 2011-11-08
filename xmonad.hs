import           Control.Applicative ((<$>))
import           Data.List           (isInfixOf)
import           Data.Ratio          ((%))
import qualified Data.Map as M
import           System.Exit         (exitWith, ExitCode(..))

import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Util.Run

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook

import           XMonad.Actions.CycleWS
import           XMonad.Layout.Combo
import           XMonad.Layout.ComboP
import           XMonad.Layout.Grid
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane


main = do
    barproc <- spawnPipe myBar
    xmonad $ withUrgencyHook NoUrgencyHook
           $ myConfig barproc


-- Settings {{{
myTerminal           = "urxvtc"
myBar                = "xmobar ~/.xmonad/mobarrc"
myModMask            = mod4Mask
myNumlockMask        = mod2Mask
myFocusFollowsMouse  = True
myBorderWidth        = 1
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"
myWorkspaces         = map show [1..9] ++ ["0", "-", "="]
ctrlMask             = controlMask

myConfig bar = defaultConfig {
    terminal           = myTerminal
  , modMask            = myModMask
  , numlockMask        = myNumlockMask
  , workspaces         = myWorkspaces
  , manageHook         = myManageHook
  , layoutHook         = myLayout
  , logHook            = myXmobarLogHook bar

  , keys               = myKeys
  , mouseBindings      = myMouseBindings
  , focusFollowsMouse  = myFocusFollowsMouse

  , borderWidth        = myBorderWidth
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
}
-- }}}


-- Window rules {{{
myManageHook = composeAll
  [
    isFullscreen --> doFullFloat

  , className =? "stalonetray"    --> doIgnore

  , className =? "Nightly"        --> doShift "2"
  , className =? "Nightly" <&&> isInfixOf "Downloads" <$> title
                                  --> doCenterFloat
  , className =? "Nightly" <&&> isInfixOf "Firebug" <$> title
                                  --> doShift "-"
                                  <+> doFullFloat

  , title     =? "mutt"           --> doShift "4"

  , className =? "Skype"          --> doShift "="
  , title     =? "irssi"          --> doShift "="
  , title     =? "mcabber"        --> doShift "="
  , title     =? "finch"          --> doShift "="
  , title     =? "ncmpcpp"        --> doShift "="

  , className =? "Gimp"           --> doShift "5"
  , className =? "Gimp"           --> doFloat
  , className =? "Keepassx"       --> doFloat
  , className =? "feh"            --> doCenterFloat

  , className =? "Vim" <&&> stringProperty "WM_WINDOW_ROLE" =? "diff"
                                  --> doFullFloat
  , className =? "Vim" <&&> stringProperty "WM_WINDOW_ROLE" =? "merge"
                                  --> doFullFloat
  ]
-- }}}


-- Status bars and logging {{{
myXmobarLogHook xmproc = dynamicLogWithPP $ xmobarPP {
    ppCurrent         = xmobarColor "#00FF00" "" . wrap "<" ">"
  , ppVisible         = xmobarColor "#00AA00" "" . wrap "<" ">"
  , ppHidden          = xmobarColor "#AAAAAA" ""
  , ppHiddenNoWindows = const ""
  , ppUrgent          = xmobarColor "#FFFFFF" "#FF0000" . wrap "[" "]" . xmobarStrip
  , ppWsSep           = " "
  , ppSep             = xmobarColor "#FFFFFF" "" " | "
  , ppLayout          = xmobarColor "#AAAAAA" ""  . shorten 80
  , ppTitle           = xmobarColor "#CCCCCC" ""
  , ppOutput          = hPutStrLn xmproc
}
-- }}}


-- Layouts {{{
myLayout = avoidStruts $
           onWorkspace "1" (myFull ||| myTiled) $
           onWorkspace "=" (myTabbed ||| myTiled) $
           minimize $ smartBorders tiled
           ||| myFull
           ||| myTabbed
           ||| myGrid
           ||| termCombo
  where
    myTiled = smartBorders tiled
    myFull = noBorders Full
    myGrid = smartBorders Grid

    tiled = maximize $ Tall nmaster delta ratio
      where
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

    myTabbed = tabbed shrinkText myTabConfig
      where
        myTabConfig = defaultTheme { inactiveBorderColor = "#BFBFBF"
                                   , activeTextColor = "#FFFFFF"}

    termCombo = maximize $ combineTwoP comboLayout comboPane1 comboPane2 comboCondition
      where
        comboLayout = TwoPane 0.03 0.5
        comboPane1  = myTabbed
        comboPane2  = myTabbed
        comboCondition = ClassName "URxvt"
-- }}}


-- Mouse bindings and options {{{
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    --, ((modMask, button4), ())
    --, ((modMask, button5), ())
    ]
-- }}}


-- Key bindings {{{
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_p     ), spawn "dmenu_run -p 'Run:' -nb '#000000' -nf '#d8d8d8' -sb '#d8d8d8' -sf '#000000'")

    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_n     ), refresh)

    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,            xK_BackSpace), withFocused (sendMessage . maximizeRestore))
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), withFocused (\f -> sendMessage $ MinimizeWin f))
    , ((modMask .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
    , ((modMask,               xK_u     ), focusUrgent)
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask .|. ctrlMask,  xK_Return), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask,               xK_q     ), restart "xmonad" True)

    , ((modMask,               xK_b     ), sendMessage ToggleStruts)


    -- Jump to previous workspace
    , ((modMask,               xK_Tab   ), toggleWS)
    -- Swap window between two panes
    , ((modMask              , xK_s     ), sendMessage $ SwapWindow)
    ]

    ++

    [
    -- XF86AudioMute
      ((0, 0x1008ff12), spawn "amixer -q set PCM toggle")
    -- XF86AudioLowerVolume
    , ((0, 0x1008ff11), spawn "amixer -q set PCM 5- unmute")
    -- XF86AudioRaiseVolume
    , ((0, 0x1008ff13), spawn "amixer -q set PCM 5+ unmute")
    -- keycode 172 (keysym 0x1008ff14, XF86AudioPlay)
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((modMask, 0x1008ff14), spawn $ termCmdWithName conf "ncmpcpp")

    -- keycode 192 (keysym 0x1008ff45, XF86Launch5)
    --, ((0, 0x1008FF45), windows $ W.greedyView "1")
    -- keycode 193 (keysym 0x1008ff46, XF86Launch6)
    --, ((0, 0x1008FF46), windows $ W.greedyView "2")
    -- keycode 194 (keysym 0x1008ff47, XF86Launch7)
    --, ((0, 0x1008FF47), windows $ W.greedyView "3")
    -- keycode 195 (keysym 0x1008ff48, XF86Launch8)
    --, ((0, 0x1008FF48), windows $ W.greedyView "4")
    -- keycode 196 (keysym 0x1008ff49, XF86Launch9)
    --, ((0, 0x1008FF49), windows $ W.greedyView "5")
    -- keycode 164 (keysym 0x1008ff30, XF86Favorites)
    , ((0, 0x1008FF30), spawn "/home/dmedvinsky/bin/toggle_tray.sh")

    -- keycode 180 (keysym 0x1008ff18, XF86HomePage)
    , ((0, 0x1008ff18), spawn "uzbl-browser")
    -- keycode 225 (keysym 0x1008ff1b, XF86Search)
    {-, ((0, 0x1008ff1b), spawn "")-}
    -- keycode 163 (keysym 0x1008ff19, XF86Mail)
    {-, ((0, 0x1008ff19), spawn "")-}
    -- keycode 148 (keysym 0x1008ff1d, XF86Calculator)
    , ((0,                       0x1008ff1d), spawn $ termCmdWithName conf "mcabber")
    , ((modMask,                 0x1008ff1d), spawn $ termCmdWithName conf "finch")
    , ((modMask .|. controlMask, 0x1008ff1d), spawn $ termCmdWithName conf "irssi")

    , ((modMask,               xK_F1    ), spawn "gvim")
    , ((modMask,               xK_F2    ), spawn "firefox")
    {-, ((modMask,               xK_F3    ), spawn )-}
    , ((modMask,               xK_F4    ), spawn $ termCmdWithName conf "mutt")
    {-, ((modMask,               xK_F5    ), spawn )-}
    , ((modMask,               xK_F12   ), spawn "/home/dmedvinsky/bin/lock")
    , ((modMask .|. controlMask, xK_F12 ), spawn "/home/dmedvinsky/bin/lock 1")
    ]

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal])
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
-- }}}


-- Utility functions {{{
--data Host = Work | Home
  --deriving (Eq, Read, Show)

--getHost :: IO Host
--getHost = do
  --hostName <- nodeName `fmap` getSystemID
  --return $ case hostName of
    --"zeus" -> Work
    --_      -> Home

termCmdWithName conf a =
    (XMonad.terminal conf) ++ " -name " ++ a ++ " -e " ++ a
-- }}}


-- vim: set foldmethod=marker

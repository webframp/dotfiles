import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S
import qualified Data.Map as M
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Loggers
import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Exit
import System.IO
import Data.Monoid
import Control.Monad
import System.Directory (getHomeDirectory)

main = do
  homedir <- getHomeDirectory
  spawn "taffybar"
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ pagerHints
    $ smeConfig homedir

--                [ className =? "HipChat" <&&> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR" --> doIgnore
smeManageHook = composeAll
                [ manageDocks
                , isFullscreen --> doFullFloat
--                , className =? "mplayer2" --> doFloat
                ]

smeConfig homedir = defaultConfig {
  manageHook = smeManageHook
  , handleEventHook = fullscreenEventHook
  , layoutHook = smeLayout homedir
  , modMask = mod4Mask
  , workspaces = smeWorkspaces
  , borderWidth = 2
  , terminal = "urxvt"
  , normalBorderColor  = "#2a2b2f"
  , focusedBorderColor = "DarkOrange"
  , keys = \c -> mkKeymap c $ smeKeymap homedir
  , startupHook = do
      return ()
      checkKeymap (smeConfig homedir) (smeKeymap homedir)
  }

smeLayout homedir =
    avoidStrutsOn [U] $             -- don't map windows over docks, etc.
    workspaceDir homedir $          -- start all workspaces in ~
    smartBorders $                  -- no borders on full-screen
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $
    --    onWorkspace "chat" myThree $  -- use 3-column layout on chat desktop

    myGrid
    ||| myTall
    ||| myFullTabbed             -- tall and fullscreen tabbed layouts
    where
        myThree = ThreeCol 1 0.03 0.50
        myGrid = renamed [Replace "Grid"] $ GridRatio (19 / 21)
        myTall = Tall 1 0.05 0.5
        myFullTabbed = simpleTabbed

--Search engines to be selected :  [google (g), wikipedia (w), duckduckgo (d), aur (r), wiki (a)]
--keybinding: hit mod + s + <searchengine>
searchEngineMap method = M.fromList $
                         [ ((0, xK_g), method S.google )
                         , ((0, xK_w), method S.wikipedia )
                         , ((0, xK_d), method $ S.searchEngine "duckduckgo" "https://duckduckgo.com/?q=")
                         , ((0n, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
                         , ((0, xK_r), method $ S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
                         , ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
                         ]

pConfig = defaultXPConfig
       { font = "xft:Bitstream Vera Sans Mono:pixelsize=20:autohint=true"
       , bgColor           = "#0c1021"
       , fgColor           = "#f8f8f8"
       --, fgHLight          = "#f8f8f8"
       --, bgHLight          = "steelblue3"
       --, borderColor       = "DarkOrange"
       , promptBorderWidth = 0
       , position          = Bottom
       , height            = 22
       , defaultText       = []
       }

smePromptConfig = pConfig {
  font = "xft:Bitstream Vera Sans Mono:pixelsize=20:autohint=true"
  , autoComplete = Just 50000
  }

smeWorkspaces = ["term","web","3","4","5","6","7","8","9","0"]

promptedGoto = workspacePrompt smePromptConfig $ windows . W.greedyView
promptedShift = workspacePrompt smePromptConfig $ windows . W.shift

smeKeymap homedir =
  -- Macbook Air first row
  [  ("<XF86MonBrightnessDown>", spawn "sudo /home/sme/bin/screen-backlight down")
  , ("<XF86MonBrightnessUp>", spawn "sudo /home/sme/bin/screen-backlight up")
  , ("<XF86LaunchA>", spawn "scrot '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/screenshots/'")
  , ("<XF86LaunchB>", spawn "")
  , ("<XF86KbdBrightnessDown>", spawn "sudo /home/sme/bin/keyboard-backlight down")
  , ("<XF86KbdBrightnessUp>", spawn "sudo /home/sme/bin/keyboard-backlight up")
  , ("<XF86AudioPrev>", spawn "")
  , ("<XF86AudioPlay>", spawn "")
  , ("<XF86AudioNext>", spawn "")
  , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 2dB-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 2dB+")
  , ("M-<F11>", spawn "amixer -q set Master 3%- unmute")
  , ("M-<F12>", spawn "amixer -q set Master 3%+ unmute")
    -- Run dmenu to launch programs
  , ("M-r", spawn "dmenu_run")
  , ("M-s", SM.submap $ searchEngineMap $ S.promptSearchBrowser pConfig "/usr/bin/surf")
-- launch browser
  , ("M-b", spawn "tabbed -c surf -e")
  , ("M-e", spawn "thunar")
    -- Close the focused window
  , ("M-S-c", kill)
    -- Switch to the next layout
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", setLayout $ Layout $ smeLayout homedir)
    -- Focus the next window
  , ("M-j", windows W.focusDown)
    -- Focus the previous window
  , ("M-k", windows W.focusUp)

    -- Swap the focused window with the next window
  , ("M-S-j", windows W.swapDown)
    -- Swap the focused window with the previous window
  , ("M-S-k", windows W.swapUp)

    -- Shrink the master window
  --, ("M-c", sendMessage Shrink)
    -- Expand the master window
  --, ("M-r", sendMessage Expand)
    -- Increment the number of windows in the master area
  , ("M-l", sendMessage (IncMasterN 1))
    -- Decrement the number of windows in the master area
  , ("M-/", sendMessage (IncMasterN (-1)))

    -- Focus urgent window
  , ("M-a", focusUrgent)

    -- Swap the focused window and the master window
  , ("M-m", windows W.swapMaster)

  , ("M-g", promptedGoto)
  , ("M-S-g", promptedShift)

    -- Quit
  , ("M-S-q", io (exitWith ExitSuccess))
    -- Restart xmonad
  , ("M-q", restart "xmonad" True)

    -- Start a terminal
  , ("M-<Return>", safeSpawn "urxvt" [])

    -- Push the focused window back into tiling
  , ("M-w t", withFocused $ windows . W.sink)
    -- Change the working dir of the current workspace
  , ("M-w c", changeDir smePromptConfig)
    -- Turn off avoiding the toolbar
  , ("M-w m", sendMessage $ ToggleStrut U)
  , ("M-w r", sendMessage $ ToggleStrut R)

    -- Toggle left/right top/bottom reflection of layouts
  , ("M-w x", sendMessage $ Toggle REFLECTX)
  , ("M-w y", sendMessage $ Toggle REFLECTY)

  , ("M-w s", spawn "import -window root ~/screenshots/shot.png")

  , ("M-<Pause>", spawn "xscreensaver-command -lock")

    -- Workspace cycling
  , ("M-n", nextWS)
  , ("M-p", prevWS)
  , ("M-S-n", shiftToNext)
  , ("M-S-p", shiftToPrev)
  , ("M-z", toggleWS)
  ]
      -- Move between workspaces, move windows between workspaces
      ++
      [("M-" ++ m ++ [k], windows $ f i)
        | (i, k) <- zip smeWorkspaces "1234567890",
          (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]
      -- Move between screens
      ++
      [("M-" ++ m ++ [key], f sc)
        | (key, sc) <- zip "',." [0..]
        , (f, m) <- [(viewScreen, ""), (sendToScreen, "S-")]]

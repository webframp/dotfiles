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
import XMonad.Layout.Decoration
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Loggers

import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Directory (getHomeDirectory)
import System.Exit
import System.IO
import Data.Monoid
import Data.Maybe (fromJust)

main :: IO ()
main = do
  homedir <- getHomeDirectory
  spawn "taffybar"
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ pagerHints
    $ smeConfig homedir


smeManageHook :: ManageHook
smeManageHook = composeAll
                [ manageDocks
                , isFullscreen --> doFullFloat
                , className =? "Firefox" --> doShift "web"
                , className =? "Pidgin" --> doShift "im"
                , className =? "Spotify" --> doShift "music"
--                , className =? "mplayer2" --> doFloat
--                , className =? "HipChat" <&&> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR" --> doIgnore
                ]

-- Solarized Colors
solarizedCommon :: [([Char], [Char])]
solarizedCommon = [ ("yellow","#b58900")
                  , ("orange","#cb4b16")
                  , ("red","#dc322f")
                  , ("magenta","#d33682")
                  , ("violet","#6c71c4")
                  , ("blue","#268bd2")
                  , ("cyan","#2aa198")
                  , ("green","#859900")
                  ]

solarizedDark :: [([Char], [Char])]
solarizedDark = [ ("S_base03","#002b36")
                , ("S_base02","#073642")
                , ("S_base01","#586e75")
                , ("S_base00","#657b83")
                , ("S_base0","#839496")
                , ("S_base1","#93a1a1")
                , ("S_base2","#eee8d5")
                , ("S_base3","#fdf6e3")
                ]

solarizedLight :: [([Char], [Char])]
solarizedLight = [ ("S_base03","#fdf6e3")
                 , ("S_base02","#eee8d5")
                 , ("S_base01","#93a1a1")
                 , ("S_base00","#839496")
                 , ("S_base0", "#657b83")
                 , ("S_base1", "#586e75")
                 , ("S_base2", "#073642")
                 , ("S_base3", "#002b36")
                 ]

-- TODO: join assoc lists?
-- join: currentTheme = solarizedCommon + solarizedDark
-- then: getColor "red" currentTheme in decoration theme blocks

-- Lookup color in assoc list or return safe default
getColor :: String -> [([Char], [Char])] -> String
getColor name list =
  case lookup name list of
  Nothing -> "#cccccc"
  x -> fromJust x

smeConfig homedir = defaultConfig {
  manageHook = smeManageHook
  , handleEventHook = docksEventHook <+> fullscreenEventHook
  , layoutHook = smeLayout homedir
  , modMask = mod4Mask
  , workspaces = smeWorkspaces
  , borderWidth = 2
  , terminal = "urxvt"
  , normalBorderColor  = getColor "S_base01" solarizedDark
  , focusedBorderColor = getColor "yellow" solarizedCommon
  , keys = \c -> mkKeymap c $ smeKeymap homedir
  , startupHook = do
      return ()
      checkKeymap (smeConfig homedir) (smeKeymap homedir)
  }

solarizedTabs = defaultTheme { activeColor         = getColor "S_base02" solarizedDark
                             , activeTextColor     = getColor "green" solarizedCommon
                             , activeBorderColor   = getColor "S_base01" solarizedDark
                             , inactiveColor       = getColor "S_base03" solarizedDark
                             , inactiveTextColor   = getColor "S_base01" solarizedDark
                             , inactiveBorderColor = getColor "S_base01" solarizedDark
                             , urgentColor         = getColor "cyan" solarizedCommon
                             , urgentTextColor     = getColor "S_base3" solarizedDark
                             , urgentBorderColor   = getColor "S_base3" solarizedDark
                             , fontName            = "xft:Andika:size=8"
                             -- , decoWidth           = 200
                             -- , decoHeight          = 20
                             -- , windowTitleAddons   = []
                             -- , windowTitleIcons    = []
                             }

smeLayout homedir =
    avoidStrutsOn [U] $             -- don't map windows over docks, etc.
    workspaceDir homedir $          -- start all workspaces in ~
    smartBorders $                  -- no borders on full-screen
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $
    onWorkspace "web" myFullTabbed $
    --    onWorkspace "chat" myThree $  -- use 3-column layout on chat desktop
    myGrid
    ||| myTall
    ||| myFullTabbed             -- tall and fullscreen tabbed layouts
    where
        myThree = ThreeCol 1 0.03 0.50
        myGrid = renamed [Replace "Grid"] $ GridRatio (19 / 21)
        myTall = Tall 1 0.05 0.5
        myFullTabbed  = tabbed shrinkText solarizedTabs

--Search engines to be selected :  [google (g), wikipedia (w), duckduckgo (d), aur (r), wiki (a)]
--keybinding: hit mod + s + <searchengine>
-- searchEngineMap :: forall a t.
--                    (Num t, Ord t) =>
--                    (S.SearchEngine -> a) -> M.Map (t, KeySym) a
searchEngineMap method = M.fromList $
                         [ ((0, xK_g), method S.google )
                         , ((0, xK_w), method S.wikipedia )
                         , ((0, xK_d), method $ S.searchEngine "duckduckgo" "https://duckduckgo.com/?q=")
                         , ((0, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
                         , ((0, xK_r), method $ S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
                         , ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
                         , ((0, xK_h), method $ S.searchEngine "hoogle" "https://www.fpcomplete.com/hoogle?q=")
                         ]

pConfig :: XPConfig
pConfig = defaultXPConfig
       { font = "xft:Andika:pixelsize=16"
       , bgColor           = getColor "S_base03" solarizedDark
       , fgColor           = getColor "S_base01" solarizedDark
       , fgHLight          = getColor "S_base2" solarizedCommon
       , bgHLight          = getColor "cyan" solarizedCommon
       --, borderColor       = "DarkOrange"
       , promptBorderWidth = 0
       , position          = Bottom
       , height            = 22
       , defaultText       = []
       }

smePromptConfig :: XPConfig
smePromptConfig = pConfig { autoComplete = Just 50000 }

smeWorkspaces :: [[Char]]
smeWorkspaces = ["term","web","im","music","5","6","7","8","9"]
-- or greek: alpha, beta, gamma, delta, epsilon, zeta, eta, theta, iota
-- smeWorkspaces = ["α","β","γ","δ","ε","ζ","η","θ","ι"]

promptedGoto :: X ()
promptedGoto = workspacePrompt smePromptConfig $ windows . W.greedyView

promptedShift :: X ()
promptedShift = workspacePrompt smePromptConfig $ windows . W.shift

smeKeymap :: String -> [([Char], X ())]
smeKeymap homedir =
  -- Macbook Air first row
  [ ("<XF86MonBrightnessDown>", spawn "xbacklight -10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight +10")
  -- , ("<XF86LaunchA>", spawn "scrot '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/screenshots/'")
  -- , ("<XF86LaunchB>", spawn "")
  , ("<XF86KbdBrightnessDown>", spawn "sudo /home/sme/bin/keyboard-backlight down")
  , ("<XF86KbdBrightnessUp>", spawn "sudo /home/sme/bin/keyboard-backlight up")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioPlay>", spawn "playerctl play")
  , ("<XF86AudioNext>", spawn "playerctl next")
  , ("<XF86AudioMute>", spawn "pamixer -t;volnoti-show -m")
  , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 5;volnoti-show $(pamixer --get-volume)")
  , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 5;volnoti-show $(pamixer --get-volume)")
  , ("M-<F9>", spawn "playerctl play-pause")
  , ("M-<F11>", spawn "pamixer --decrease 5;volnoti-show $(pamixer --get-volume)")
  , ("M-<F12>", spawn "pamixer --increase 5;volnoti-show $(pamixer --get-volume)")
    -- Run dmenu wrapper to launch programs
  , ("M-r", spawn "yegonesh")
  , ("M-s", SM.submap $ searchEngineMap $ S.promptSearchBrowser pConfig "/usr/bin/firefox")
-- launch browser
  , ("M-b b", spawn "/usr/bin/firefox")
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
  -- , ("M-l", sendMessage (IncMasterN 1))
    -- Decrement the number of windows in the master area
  -- , ("M-/", sendMessage (IncMasterN (-1)))

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

  , ("M-<Pause>", spawn "slock")

    -- Workspace cycling
  , ("M-n", nextWS)
  , ("M-p", prevWS)
  , ("M-S-n", shiftToNext)
  , ("M-S-p", shiftToPrev)
  , ("M-z", toggleWS)
  , ("M-<Tab>", toggleWS)
  ]
      -- Move between workspaces, move windows between workspaces
      ++
      [("M-" ++ m ++ [k], windows $ f i)
        | (i, k) <- zip smeWorkspaces "123456789",
          (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]
      -- Move between screens
      ++
      [("M-" ++ m ++ [key], f sc)
        | (key, sc) <- zip "',." [0..]
        , (f, m) <- [(viewScreen, ""), (sendToScreen, "S-")]]

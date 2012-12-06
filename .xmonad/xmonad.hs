-- xmonad.hs
-- XMonad config file - sean.escriva@gmail.com
--
import XMonad
-- Hooks
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..), focusUrgent)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
-- Actions
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, toggleWS, Direction1D(..), WSType(..), findWorkspace)
import XMonad.Actions.WindowGo (title, raiseMaybe, runOrRaise) --, (=?))
import XMonad.Actions.UpdatePointer
-- Utils
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.EZConfig hiding (additionalMouseBindings, removeMouseBindings)
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
-- Layouts
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.Magnifier (magnifiercz)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.IM
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Named

-- Prompt
-- import XMonad.Prompt

import System.IO (hPutStrLn)
import Data.Char (isSpace)
import qualified XMonad.StackSet as W

-- import Graphics.X11.ExtraTypes.XF86

-- basic colors
cyan = "#00a0df"

-- Color names are easier to remember:
colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#efad3f"
colorWhite           = "#cfbfad"
colorRed             = "#b61819"
colorPurple          = "#9C3378"

-- zenburn
zbBackground         = "#3f3f3f"
zbForeground         = "#dcdccc"
zbBlack              = "#1E2320"
zbRed                = "#705050"
zbGreen              = "#60b48a"
zbYellow             = "#dfaf8f"
zbBlue               = "#506070"
zbPurple             = "#dc8cc3"
zbCyan               = "#8cd0d3"
zbWhite              = "#dcdccc"
zbBrightBack         = "#709080"
zbBrightRed          = "#dca3a3"
zbBrightGreen        = "#c3bf9f"
zbBrightYellow       = "#f0dfaf"
zbBrightBlue         = "#94bff3"
zbBrightPurple       = "#ec93d3"
zbBrightCyan         = "#93e0e3"
zbBrightWhite        = "#ffffff"

colorNormalBorder    = zbGreen
colorFocusedBorder   = zbBrightGreen

colorBG              = tnBackground

tnBackground         = "#1d1f21"
tnBrightBlue         = "#81a2be"
tnBrightRed          = "#cc6666"
tnYellow             = "#ae7b00"
tnWhite              = "#929593"

-- fonts
barFont  = "terminus"
barXFont = "inconsolata:size=14"
xftFont  = "xft: inconsolata-14"

statusBarCmd = "dzen2" ++
               " -bg '" ++ colorBG ++ "'" ++
               " -fg '" ++ tnBrightBlue ++ "'" ++
               " -sa c" ++
               " -fn '" ++ barXFont ++ "'" ++
               " -w 1300 -x 0 -y 0 -ta l -expand r -e ''"

mTerm    = "urxvtc"

smePP = defaultPP
        { ppCurrent = dzenColor tnBrightRed colorBG . wrap "" ""
        , ppVisible = dzenColor tnBrightBlue colorBG . wrap "" ""
        , ppSep     = dzenColor tnWhite colorBG " ^r(1x8) "
        , ppUrgent  = dzenColor colorBG tnYellow . wrap "[" "]"
        , ppTitle   = dzenColor tnWhite "" . trim
        }

standardLayouts = Mirror tiled |||
                  defaultTall  |||
                  Full
                where
                  tiled         = Tall nmaster delta ratio
                  defaultTall   = ResizableTall 1 (3/100) (1/2) []
                  nmaster       = 1
                  ratio         = toRational (2/(1 + sqrt 5 :: Double)) -- golden ratio
                  delta         = 0.03

-- grids = magnifiercz 1.2 (GridRatio (4/3)) |||
--         GridRatio (4/3)

gimp     = reflectHoriz $ 
           named "Gimp" $ 
           withIM (11/64) (Role "gimp-toolbox") $ 
           ResizableTall 2 (1/118) (11/20) [5/4,5/4,5/4]

smeKeys :: [([Char], X ())]
smeKeys =
    [ ("M-p"        , safeSpawn "yeganesh_run" [] )
    , ("M-g"        , runOrRaise "conkeror" (className =? "Conkeror"))
    , ("M-s"        , runOrRaise "skype"    (className =? "Skype"))
    , ("M-m"        , safeSpawn "mumble" [] )
    -- , ("M-S-g"      , safePromptSelection "firefox-nightly")
    -- , ("M-w"        , goToSelected defaultGSConfig)
    -- , ("M-C-n"      , appendFilePrompt largeXPConfig { bgColor = colorOrange, fgColor = colorDarkGray } notesFile)
    , ("M-`"    , focusUrgent)
    , ("M-i"    , raiseMaybe (runInTerm "-title irssi" "sh -c 'tmux -D -R -S irc irssi'") (title =? "irssi"))
    -- , ("M-S-i"  , raiseMaybe (runInTerm "-title irssi" "sh -c 'ssh -t webframp@astrotrain screen -D -R -S irc irssi'") (title =? "irssi"))
    -- , ("M-m"    , raiseMaybe (runInTerm "-title mutt" "sh -c 'screen -D -R -S mail mutt'") (title =? "mutt"))
    , ("M-b"    , sendMessage ToggleStruts)
    -- move window to and focus NonEmpty wss except scratchpad
    , ("M-C-s"    , shiftAndView Next)
    , ("M-C-d"    , shiftAndView Prev)
    , ("M-f"      , nextScreen)
    , ("M-a"      , prevScreen)
    , ("M-S-f"    , shiftNextScreen)
    , ("M-S-a"    , shiftPrevScreen)
    , ("M-<Tab>"  , toggleWS)
    , ("M--"      , toggleWS)
    -- Media keys
    , ("<XF86AudioLowerVolume>" , unsafeSpawn "amixer -q set Master 2dB-" 	)
    , ("<XF86AudioMute>"        , unsafeSpawn "amixer -q set Master toggle")
    , ("<XF86AudioRaiseVolume>" , unsafeSpawn "amixer -q set Master 2dB+" 	)
    -- Screenshot
    , ("<XF86LaunchA>" , unsafeSpawn "scrot '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/screenshots/'")
    , ("<XF86LaunchB>" , unsafeSpawn "scrot '%Y-%m-%d-%H%M_$wx$h.png' -e '/usr/bin/imgurbash $f'")
    , ("<XF86MonBrightnessUp>", unsafeSpawn "xbacklight -inc 5")
    , ("<XF86MonBrightnessDown>", unsafeSpawn "xbacklight -dec 5")
    , ("<XF86KbdBrightnessUp>", unsafeSpawn "xbacklight -dec 5")
    , ("<XF86KbdBrightnessDown>", unsafeSpawn "xbacklight -dec 5")
    ]
    where -- | non-empty workspaces less scratchpad
        shiftAndView dir = findWorkspace getSortByIndexNoSP dir NonEmptyWS 1
                >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
        getSortByIndexNoSP =
                fmap (.scratchpadFilterOutWorkspace) getSortByIndex


-- smeMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
--     -- mod-button1, Set the window to floating mode and move by dragging
--     [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
--     -- mod-button2, Raise the window to the top of the stack
--     , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
--     -- mod-button3, Set the window to floating mode and resize by dragging
--     , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
--     -- cycle through workspaces
--     , ((controlMask .|. modMask, button4), nextScreen)
--     , ((controlMask .|. modMask, button5), prevScreen)
--     ]

smeManageHook :: ManageHook
smeManageHook = composeAll
              [ resource  =? "desktop_window" --> doIgnore
              , className =? "MPlayer" --> doFloat
              , isFullscreen           --> doFullFloat
              , isDialog               --> doCenterFloat
              ] <+> manageDocks

smeLogHook h = do
           dynamicLogWithPP $ smePP { ppOutput = hPutStrLn h }

smeConfig = defaultConfig
       { terminal = mTerm
       , focusFollowsMouse      = False
       , modMask                = mod4Mask -- command key
       , focusedBorderColor     = cyan
       , layoutHook             = avoidStruts $
                                  smartBorders (
                                               onWorkspace "5" gimp standardLayouts
                                               )
       , manageHook             = smeManageHook
       -- , mouseBindings          = smeMouseBindings
       }
       `additionalKeysP` smeKeys

strutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

-- Main
main = do
     h <- spawnPipe statusBarCmd
     xmonad $ withUrgencyHook NoUrgencyHook $ ewmh smeConfig { logHook = smeLogHook h }


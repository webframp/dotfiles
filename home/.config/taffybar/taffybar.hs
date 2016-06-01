import System.Taffybar
import System.Taffybar.Pager
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS2
import System.Taffybar.Battery
import System.Taffybar.NetMonitor
import System.Taffybar.CommandRunner

import System.Taffybar.Widgets.PollingGraph

-- Notes for future work, clickable AP selector:
-- import System.Taffybar.Widgets.Util
-- import qualified Graphics.UI.Gtk as Gtk

import System.Information.Memory
import System.Information.CPU

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO()
main = do
  -- The color components are floating point numbers in the range 0 to 1. If the
  -- values passed in are outside that range, they will be clamped.
  let memCfg = defaultGraphConfig { graphDataColors = [(0.7098039215686275,0.5372549019607843,0,1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [(0.5215686274509804,0.6,0,1)
                                                      ,(0.16470588235294117,0.6313725490196078,0.596078431372549,0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      pagCfg = defaultPagerConfig { activeWindow     = escape . shorten 40
                                  , activeLayout     = escape
                                  , activeWorkspace  = colorize "#b58900" "" . wrap "[" "]" . escape
                                  , hiddenWorkspace  = escape
                                  , emptyWorkspace   = escape
                                  , visibleWorkspace = wrap "(" ")" . escape
                                  , urgentWorkspace  = colorize "#dc322f" "#b58900" . escape
                                  , widgetSep        = " : "
                                  }

  let clock = textClockNew Nothing "<span fgcolor='#cb4b16'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew pagCfg
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew (defaultWeatherConfig "KJFK") 10 -- KPDX
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      bat = batteryBarNew defaultBatteryConfig 30
      net = netMonitorNew 1 "wlp2s0b1"
      ap = commandRunnerNew 60 "getap.sh" [""] "offline" "#859900"
      tray = systrayNew
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager, note ]
                                        , endWidgets = [ ap, tray, bat, wea, clock, net, mem, cpu, mpris ]
                                        }

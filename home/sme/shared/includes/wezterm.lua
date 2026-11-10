local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.default_domain = 'WSL:NixOS'
config.default_cwd = '/home/sme'

config.font = wezterm.font('Hack Nerd Font Mono')
config.font_size = 16.0

-- Swamp Club palette from github.com/sntxrr/ghostty-swamp-club (MIT), shared
-- with the herdr theme in modules/home-manager/herdr.nix so panes match the chrome.
config.color_schemes = {
  ['Swamp Club'] = {
    ansi = {
      '#1a1a1a', '#ff003c', '#05df72', '#fde047',
      '#60a5fa', '#e879f9', '#00d3f2', '#d1d5db',
    },
    brights = {
      '#6b7280', '#f87171', '#39ff14', '#fef08a',
      '#93c5fd', '#f0abfc', '#67e8f9', '#ffffff',
    },
    background = '#080808',
    foreground = '#d1d5db',
    cursor_bg = '#39ff14',
    cursor_fg = '#080808',
    cursor_border = '#39ff14',
    selection_bg = '#062e16',
    selection_fg = '#d1d5db',
    split = '#06411e',
  },
}
config.color_scheme = 'Swamp Club'

config.term = 'xterm-256color'
config.enable_tab_bar = false

-- Fix for C-Space binding (send NUL)
config.keys = {
  { key = 'Space', mods = 'CTRL', action = wezterm.action.SendString('\x00') },
}

config.audible_bell = "Disabled"
config.notification_handling = "SuppressFromFocusedWindow"

-- Native Windows toast notification on bell (only when unfocused)
wezterm.on('bell', function(window, pane)
  if window:is_focused() then return end
  window:toast_notification('Task Complete', 'A task has finished', nil, 4000)
end)

return config

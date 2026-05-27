local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.default_domain = 'WSL:NixOS'
config.default_cwd = '/home/sme'

config.font = wezterm.font('Hack Nerd Font Mono')
config.font_size = 16.0

config.color_scheme = 'Dracula'

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

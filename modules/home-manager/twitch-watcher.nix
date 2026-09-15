# ABOUTME: systemd user service for the twitch-watcher daemon, run from a local checkout
# ABOUTME: Restart=always turns the daemon's heap-watchdog self-recycle into a clean restart
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.webframp.twitchWatcher;
in {
  options.webframp.twitchWatcher = {
    enable = lib.mkEnableOption "twitch-watcher systemd user service";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.nodejs;
      defaultText = lib.literalExpression "pkgs.nodejs";
      description = "Node.js package used to run the daemon.";
    };

    workingDirectory = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/src/webframp/twitch-watcher";
      defaultText = lib.literalExpression ''"''${config.home.homeDirectory}/src/webframp/twitch-watcher"'';
      description = ''
        Path to the twitch-watcher checkout. The service runs
        `node dist/index.js` from here, so `npm install` and `npm run build`
        must have been run in this directory. config.json and .tokens.json are
        resolved relative to it.
      '';
    };

    winUser = lib.mkOption {
      type = lib.types.str;
      default = config.home.username;
      defaultText = lib.literalExpression "config.home.username";
      description = ''
        Windows username used to derive the Chromium/Firefox/profile paths
        (C:\Users\<winUser>\...). Passed to the daemon as WIN_USER.
      '';
    };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = lib.literalExpression ''"''${config.home.homeDirectory}/.config/twitch-watcher/env"'';
      description = ''
        Optional path to a systemd EnvironmentFile (KEY=value lines) read at
        service start. Use it to supply HONEYCOMB_API_KEY or to override the
        derived browser paths (CHROMIUM_PATH, FIREFOX_PATH, WATCHER_PROFILE).
        Values here take precedence over WIN_USER-derived defaults. The file is
        read from disk at runtime, not copied into the Nix store, and is
        treated as optional — a missing file does not fail the service.
      '';
    };

    autoStart = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Start the daemon on login (WantedBy default.target).";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.hostPlatform.isLinux;
        message = "webframp.twitchWatcher is WSL/Linux-only (drives Windows Chrome over CDP).";
      }
    ];

    systemd.user.services.twitch-watcher = {
      Unit = {
        Description = "twitch-watcher: EventSub + headless Chromium view daemon";
        # Restart backoff and a start limit so a misconfigured daemon can't
        # hammer the Twitch API in a tight crash loop. 5 starts / 5 min, then
        # systemd holds off until reset. A leak-driven exit (heap watchdog,
        # exit 1) is well under this budget and restarts promptly.
        StartLimitIntervalSec = 300;
        StartLimitBurst = 5;
      };

      Service = {
        Type = "simple";
        WorkingDirectory = cfg.workingDirectory;
        ExecStart = "${cfg.package}/bin/node ${cfg.workingDirectory}/dist/index.js";
        Environment = [
          "WIN_USER=${cfg.winUser}"
          # dist/index.js is a plain node entrypoint; ensure the daemon's own
          # child_process spawns (Windows exes via /mnt/c paths) still work by
          # inheriting the login PATH rather than a stripped systemd PATH.
          "PATH=${lib.makeBinPath [cfg.package]}:/usr/bin:/bin"
        ];
        EnvironmentFile = lib.mkIf (cfg.environmentFile != null) "-${cfg.environmentFile}";

        Restart = "always";
        RestartSec = 10;

        # The daemon spawns Windows browser processes and holds a headless
        # Chromium over CDP; kill the whole cgroup on stop so nothing is
        # stranded on the WSL side.
        KillMode = "mixed";
        TimeoutStopSec = 30;
      };

      Install = lib.mkIf cfg.autoStart {
        WantedBy = ["default.target"];
      };
    };
  };
}

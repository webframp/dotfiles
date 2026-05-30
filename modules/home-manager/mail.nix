# ABOUTME: Email configuration module using protonmail-bridge + mbsync + mu + msmtp
# ABOUTME: Provides local IMAP/SMTP via Proton Mail Bridge with home-manager accounts.email
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.mail;

  accountModule = types.submodule {
    options = {
      address = mkOption {
        type = types.strMatching ".*@.*";
        description = "Email address for this account";
        example = "user@protonmail.com";
      };

      realName = mkOption {
        type = types.str;
        default = "Sean Escriva";
        description = "Display name for sent mail";
      };

      primary = mkOption {
        type = types.bool;
        default = false;
        description = "Whether this is the primary email account";
      };

      passEntry = mkOption {
        type = types.str;
        description = "Pass entry containing the bridge-generated password";
        example = "email/protonmail.com/bridge";
      };

      gpgKey = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "GPG key ID for email signing (optional)";
        example = "BE06ADB38C7F719D";
      };

      aliases = mkOption {
        type = types.listOf (types.strMatching ".*@.*");
        default = [];
        description = "Alternative email addresses for this account";
        example = ["alias@example.com"];
      };

      maildirName = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Subdirectory name under ~/Mail (defaults to account attr name)";
      };
    };
  };
in {
  options.webframp.mail = {
    enable = mkEnableOption "email (protonmail-bridge + mbsync + mu + msmtp)";

    bridgeCert = mkOption {
      type = types.path;
      default = "${config.home.homeDirectory}/.config/protonmail/bridge-v3/cert.pem";
      description = "Path to Proton Mail Bridge's self-signed TLS certificate";
    };

    imapPort = mkOption {
      type = types.port;
      default = 1143;
      description = "Proton Mail Bridge IMAP port";
    };

    smtpPort = mkOption {
      type = types.port;
      default = 1025;
      description = "Proton Mail Bridge SMTP port";
    };

    accounts = mkOption {
      type = types.attrsOf accountModule;
      default = {};
      description = "Email accounts to configure";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.protonmail-bridge
      pkgs.docker-credential-helpers
    ];

    accounts.email.maildirBasePath = "Mail";

    accounts.email.accounts = mapAttrs (name: acct: {
      inherit (acct) address realName primary aliases;

      userName = acct.address;
      passwordCommand = "pass ${acct.passEntry}";

      imap = {
        host = "127.0.0.1";
        port = cfg.imapPort;
        tls = {
          enable = true;
          useStartTls = true;
          certificatesFile = cfg.bridgeCert;
        };
      };

      smtp = {
        host = "127.0.0.1";
        port = cfg.smtpPort;
        tls = {
          enable = true;
          useStartTls = true;
          certificatesFile = cfg.bridgeCert;
        };
      };

      gpg = mkIf (acct.gpgKey != null) {
        key = acct.gpgKey;
        signByDefault = true;
      };

      maildir.path =
        if acct.maildirName != null
        then acct.maildirName
        else name;

      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        extraConfig.account = {
          AuthMechs = "LOGIN";
        };
      };

      msmtp = {
        enable = true;
        extraConfig = {
          auth = "login";
        };
      };

      mu.enable = true;
    }) cfg.accounts;

    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.mu.enable = true;

    home.activation.muIndex = let
      allAddresses = lib.concatMap (acct: [acct.address] ++ acct.aliases) (attrValues cfg.accounts);
      myAddressFlags = lib.concatMapStringsSep " " (a: "--my-address ${a}") allAddresses;
    in
      lib.hm.dag.entryAfter ["writeBoundary"] ''
        run ${pkgs.mu}/bin/mu init --maildir ~/Mail ${myAddressFlags} 2>/dev/null || true
        run ${pkgs.mu}/bin/mu index 2>/dev/null || true
      '';

    systemd.user.services.protonmail-bridge = mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "Proton Mail Bridge";
        After = ["network-online.target" "gpg-agent.service"];
      };
      Service = {
        ExecStart = "${pkgs.protonmail-bridge}/bin/protonmail-bridge --noninteractive";
        Restart = "always";
        RestartSec = 5;
        Environment = [
          "PATH=${lib.makeBinPath [pkgs.docker-credential-helpers pkgs.gnupg pkgs.pass]}"
        ];
      };
      Install = {
        WantedBy = ["default.target"];
      };
    };

    launchd.agents.protonmail-bridge = mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        ProgramArguments = ["${pkgs.protonmail-bridge}/bin/protonmail-bridge" "--noninteractive"];
        KeepAlive = true;
        RunAtLoad = true;
      };
    };
  };
}

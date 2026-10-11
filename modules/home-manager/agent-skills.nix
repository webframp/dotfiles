# ABOUTME: Personal agent skills linked from config/skills into every harness skill directory
# ABOUTME: One git-tracked copy per skill, symlinked into ~/.agents, ~/.claude, and ~/.kiro skills
#
# Uses direct activation-script symlinks, not home.file + mkOutOfStoreSymlink,
# for the same reason as claude-code.nix: the store-profile indirection adds a
# second hop, and agents that edit a skill with an atomic writer (write .tmp,
# rename) resolve only one hop and fail with EROFS.
#
# Flake evaluation only sees git-tracked files. A new skill under
# config/skills must be `git add`ed before `make switch`, or the assertion
# below reports it missing.
#
# The activation never touches entries it did not create: a real directory or
# file at a link path is left alone with a warning (swamp-installed skills,
# ~/.claude/skills/synced, and so on). Links created are recorded in a state
# file, and a link dropped from `skills` is removed only if it is still a
# symlink pointing into config/skills.
{
  config,
  lib,
  ...
}: let
  cfg = config.webframp.agentSkills;
  srcDir = "${config.home.homeDirectory}/src/webframp/dotfiles/config/skills";
  targetDirs = {
    agents = ".agents/skills";
    claude = ".claude/skills";
    kiro = ".kiro/skills";
  };
  missing = builtins.filter (name: !builtins.pathExists (../../config/skills + "/${name}/SKILL.md")) cfg.skills;
  q = lib.escapeShellArg;
in {
  options.webframp.agentSkills = {
    enable = lib.mkEnableOption "personal agent skills linked into every harness";

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Skill directory names under config/skills to link.";
    };

    targets = lib.mkOption {
      type = lib.types.listOf (lib.types.enum (builtins.attrNames targetDirs));
      default = builtins.attrNames targetDirs;
      description = "Harness skill directories to link into.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = missing == [];
        message = "webframp.agentSkills: no config/skills/<name>/SKILL.md in the flake source for: ${lib.concatStringsSep ", " missing} (typo, or not git-added?)";
      }
    ];

    home.activation.linkAgentSkills = lib.hm.dag.entryAfter ["writeBoundary"] ''
      skillSrc=${q srcDir}
      skillState="''${XDG_STATE_HOME:-$HOME/.local/state}/webframp-agent-skills"
      declare -A skillWanted=()

      for skillTarget in ${lib.concatMapStringsSep " " (t: "\"$HOME\"/${q targetDirs.${t}}") cfg.targets}; do
        run mkdir -p "$skillTarget"
        for skillName in ${lib.concatMapStringsSep " " q cfg.skills}; do
          skillLink="$skillTarget/$skillName"
          # ln -sfn onto a real directory would create a link inside it, so
          # only replace existing symlinks or fill empty slots.
          if [[ -L $skillLink || ! -e $skillLink ]]; then
            run ln -sfn "$skillSrc/$skillName" "$skillLink"
            skillWanted["$skillLink"]=1
          else
            warnEcho "agentSkills: $skillLink is not a symlink; leaving it alone"
          fi
        done
      done

      if [[ -f $skillState/links ]]; then
        while IFS= read -r skillOld; do
          [[ -n $skillOld && ! -v skillWanted[$skillOld] && -L $skillOld ]] || continue
          case "$(readlink "$skillOld")" in
            "$skillSrc"/*) run rm "$skillOld" ;;
          esac
        done < "$skillState/links"
      fi

      if [[ ! -v DRY_RUN ]]; then
        mkdir -p "$skillState"
        printf '%s\n' "''${!skillWanted[@]}" > "$skillState/links"
      fi
    '';
  };
}

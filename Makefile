# ABOUTME: Common targets for managing this nix flake home-manager configuration
# ABOUTME: Auto-detects hostname for switch/build targets

# Valid hostnames with configurations
VALID_HOSTS := bluestreak ubuntu generic

HOSTNAME := $(shell hostname -s)
USER := sme
# Allow override: make switch HOST=generic
HOST ?= $(HOSTNAME)
FLAKE_TARGET := $(USER)@$(HOST)

# Check if host has a configuration
define check_host
	@if ! echo "$(VALID_HOSTS)" | grep -qw "$(HOST)"; then \
		echo "Error: No configuration for host '$(HOST)'"; \
		echo "Valid hosts: $(VALID_HOSTS)"; \
		echo ""; \
		echo "To use a specific config: make $(1) HOST=<hostname>"; \
		exit 1; \
	fi
endef

# Age for generation cleanup (override: make clean-generations AGE=14d)
AGE ?= 30d

# Current system for package builds
SYSTEM := $(shell nix eval --impure --raw --expr 'builtins.currentSystem')

# Dynamic package discovery from pkgs/
# Note: Adds ~0.5s startup overhead due to nix eval at parse time.
# This is a fixed cost (evaluates attribute names only, not package contents).
PACKAGES := $(shell nix eval .#packages.$(SYSTEM) --apply 'builtins.attrNames' --json 2>/dev/null | jq -r '.[]')

.PHONY: switch build check fmt update clean clean-generations news diff zsh-bench help
.PHONY: pkg-list pkg-build-all pkg-bump $(PACKAGES)

## Primary targets

switch: ## Apply home-manager configuration
	$(call check_host,$@)
	home-manager switch --flake .#$(FLAKE_TARGET)

build: ## Dry-run build to verify changes
	$(call check_host,$@)
	nix build .#homeConfigurations.$(FLAKE_TARGET).activationPackage --dry-run

diff: ## Show what will change before switching
	$(call check_host,$@)
	home-manager build --flake .#$(FLAKE_TARGET)
	nix store diff-closures ~/.local/state/nix/profiles/home-manager ./result

## Maintenance

update: ## Update flake inputs
	nix flake update

fmt: ## Format nix files with alejandra
	alejandra .

check: ## Validate flake
	nix flake check

clean: ## Garbage collect old generations
	nix-collect-garbage -d

clean-generations: ## Remove old home-manager generations (default: 30d, override: AGE=14d)
	nix profile wipe-history --profile ~/.local/state/nix/profiles/home-manager --older-than $(AGE)

news: ## Show home-manager news
	$(call check_host,$@)
	home-manager news --flake .#$(FLAKE_TARGET)

## Diagnostics

zsh-bench: ## Measure zsh startup time (run 5 times)
	@echo "Measuring zsh startup time..."
	@for i in 1 2 3 4 5; do /usr/bin/time zsh -i -c exit 2>&1; done

## Custom packages (pkgs/)
## Individual package targets are generated dynamically (e.g., make aws-doctor)

pkg-list: ## List available custom packages
	@echo "Available packages (build with 'make <name>'):"
	@for pkg in $(PACKAGES); do echo "  $$pkg"; done

pkg-build-all: ## Build all custom packages
	@for pkg in $(PACKAGES); do \
		echo "Building $$pkg..."; \
		nix build .#$$pkg || exit 1; \
	done
	@echo "All packages built successfully"

pkg-bump: ## Bump package to latest GitHub release (PKG=name)
ifndef PKG
	@echo "Usage: make pkg-bump PKG=<package-name>"
	@echo "Run 'make pkg-list' to see available packages"
	@exit 1
endif
	@./scripts/bump-pkg.sh $(PKG)

# Generate a target for each package (e.g., make aws-doctor, make iamlive)
define pkg_target
$(1): ## Build $(1) package
	nix build .#$(1)
endef
$(foreach pkg,$(PACKAGES),$(eval $(call pkg_target,$(pkg))))

## Help

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help

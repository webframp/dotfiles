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

.PHONY: switch build check fmt update clean clean-generations news diff zsh-bench help

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

## Help

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help

.ONESHELL:
.SHELL := /usr/bin/bash

help: ## display task help output
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

deploy: ## Build and switch to new nix darwin environment
	nix build .#darwinConfigurations.bluestreak.system \
	   --extra-experimental-features 'nix-command flakes'

	./result/sw/bin/darwin-rebuild switch --flake .#bluestreak

# Makefile for legacy Stow compatibility and shared dotfiles verification.
#
# `make` alone is safe: it previews legacy Stow/seed actions and writes nothing.
# `make switch` applies seed-only configs and legacy .stow-packages if nonempty.
# `make home-generation` builds and inspects Home Manager without activation.
# `make check` compares both deployment manifests against the live $HOME.
# See docs/nix-migration.md for Home Manager activation.

SHELL := bash

# '#' opens a comment in a Makefile, so route the character through a
# variable. Make expands the variable after it strips comments.
HASH := \#

STOW_DIR    := .
STOW_TARGET := $(HOME)
PACKAGES    := $(shell grep -v '^$(HASH)' .stow-packages | grep -v '^[[:space:]]*$$')

# Default to the action that changes nothing.
.DEFAULT_GOAL := dry-run

PYRIGHT_VERSION := 1.1.403

.PHONY: help dry-run switch unstow check home-generation brew brew-diff doctor tools plugins lint typecheck test verify smoke test-tools capabilities migrate-claude

help: ## Show this help
	@echo "Stow packages: $(PACKAGES)"
	@echo
	@echo "Targets:"
	@grep -hE '^[a-zA-Z_-]+:.*?$(HASH)$(HASH) .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?$(HASH)$(HASH) "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'

dry-run: ## Simulate Stow deployment and seeding, write nothing (default)
ifneq ($(strip $(PACKAGES)),)
	stow --simulate --verbose --dir $(STOW_DIR) --target $(STOW_TARGET) $(PACKAGES)
else
	@echo "No Stow packages declared; Home Manager activation is separate." >&2
endif
	@python3 scripts/seed-configs

switch: ## Deploy .stow-packages and seed configs, not Home Manager
ifneq ($(strip $(PACKAGES)),)
	stow --restow --dir $(STOW_DIR) --target $(STOW_TARGET) $(PACKAGES)
else
	@echo "No Stow packages declared; Home Manager activation is separate." >&2
endif
	@python3 scripts/seed-configs --apply

unstow: ## Remove links for .stow-packages; leave Home Manager and seeds
ifneq ($(strip $(PACKAGES)),)
	stow --delete --dir $(STOW_DIR) --target $(STOW_TARGET) $(PACKAGES)
else
	@echo "No Stow packages declared; nothing to unstow." >&2
endif

check: ## Compare the live $HOME against this checkout
	@scripts/check

home-generation: ## Build and inspect the Home Manager generation, write nothing
	@scripts/check-home-generation

brew: ## Install the packages that the Brewfile declares
	brew bundle install --file=Brewfile

brew-diff: ## Show installed packages that the Brewfile does not declare
	@echo "$(HASH) requested formulae that the Brewfile omits:"
	@comm -23 \
		<(brew leaves --installed-on-request | sort -u) \
		<(grep '^brew "' Brewfile | sed -e 's/^brew "//' -e 's/".*//' -e 's|.*/||' | sort -u)
	@echo "$(HASH) casks that the Brewfile omits:"
	@comm -23 \
		<(brew list --cask -1 | sort -u) \
		<(grep '^cask "' Brewfile | sed -e 's/^cask "//' -e 's/".*//' -e 's|.*/||' | sort -u)

lint: ## Check shell, Python, JSON, Lisp and Lua syntax
	@python3 scripts/verify lint

typecheck: ## Type-check the Python configuration tools and tests
	@python3 scripts/verify typecheck

test: ## Run isolated configuration regression tests
	@python3 scripts/verify test

verify: ## Run lint, typecheck and tests without installing packages
	@python3 scripts/verify all

smoke: ## Load Emacs offline with installed packages and temporary state
	@python3 scripts/verify smoke

test-tools: ## Install the declared verification dependencies explicitly
	brew install shellcheck
	uv tool install pyright==$(PYRIGHT_VERSION)

doctor: ## Read-only environment probe (JSON on stdout, summary on stderr)
	@scripts/doctor

capabilities: ## Probe declared dev capabilities on temp workspaces (no installs)
	@python3 scripts/check-capabilities

tools: ## Bootstrap runtime toolchain from scripts/runtime-versions.env
	@scripts/setup-tools --install

migrate-claude: ## Preview the sonnet-defaults-v1 Claude settings migration (read-only)
	@python3 scripts/migrate-claude-settings

plugins: ## Init git submodules and install TPM plugins (explicit only)
	@test -f "$(HOME)/.tmux.conf" || { echo 'Activate Home Manager before make plugins' >&2; exit 1; }
	git submodule update --init --recursive
	tmux start-server \; set-environment -g TMUX_PLUGIN_MANAGER_PATH "$(HOME)/.tmux/plugins/"
	@if [ -x tmux/.tmux/plugins/tpm/bin/install_plugins ]; then \
		tmux/.tmux/plugins/tpm/bin/install_plugins; \
	else \
		echo "tpm/bin/install_plugins missing; run 'git submodule update --init --recursive'" >&2; \
		exit 1; \
	fi

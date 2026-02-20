# Doom Emacs Config Testing
# Run `make test` before committing config changes

DOOM := ~/.config/emacs/bin/doom
MODULES_DIR := modules

.PHONY: test lint doctor sync check clean help

help:
	@echo "Doom config test targets:"
	@echo "  make test   - Run all checks (lint + doctor)"
	@echo "  make lint   - Check elisp syntax"
	@echo "  make doctor - Run doom doctor"
	@echo "  make sync   - Run doom sync"
	@echo "  make check  - Dry-run sync"
	@echo "  make clean  - Remove compiled .elc files"

test: lint doctor
	@echo "All checks passed!"

lint:
	@echo "==> Checking elisp syntax..."
	@failed=0; \
	for f in $$(find . -name "*.el" -not -path "./.local/*"); do \
		emacs --batch -Q --eval " \
			(condition-case err \
				(with-temp-buffer \
					(insert-file-contents \"$$f\") \
					(lisp-mode) \
					(check-parens)) \
				(error (message \"FAIL: $$f - %s\" (error-message-string err)) \
					(kill-emacs 1)))" 2>&1 || failed=1; \
	done; \
	if [ $$failed -eq 1 ]; then exit 1; fi
	@echo "==> Syntax check complete"

doctor:
	@echo "==> Running doom doctor..."
	@$(DOOM) doctor

sync:
	@echo "==> Running doom sync..."
	@$(DOOM) sync

check:
	@echo "==> Dry-run sync..."
	@$(DOOM) sync -n

clean:
	@echo "==> Removing compiled files..."
	@find . -name "*.elc" -delete
	@echo "==> Clean complete"

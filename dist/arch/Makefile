# Ghostty Custom (OSC 50) — Arch Linux Package
# =============================================
#
# Build commands for the split-package PKGBUILD.
# All commands should be run from the repo root.
#
# Prerequisites: base-devel, zig, pandoc-cli, blueprint-compiler
#
# Usage:
#   make pkg        Build the package (runs makepkg from repo root)
#   make install    Build and install with pacman -U
#   make srcinfo    Regenerate .SRCINFO
#   make clean      Remove build artifacts

PKG_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
REPO_ROOT := $(abspath $(PKG_DIR)/../..)

.PHONY: pkg install srcinfo clean

pkg:
	@echo "==> Building ghostty-custom packages..."
	cd $(REPO_ROOT) && makepkg -f -p $(PKG_DIR)/PKGBUILD

install: pkg
	@echo "==> Installing ghostty-custom packages..."
	sudo pacman -U $(REPO_ROOT)/ghostty-custom-*.pkg.tar.zst

srcinfo:
	@echo "==> Generating .SRCINFO..."
	cd $(REPO_ROOT) && makepkg --printsrcinfo -p $(PKG_DIR)/PKGBUILD > $(PKG_DIR)/.SRCINFO

clean:
	@echo "==> Cleaning build artifacts..."
	rm -rf $(REPO_ROOT)/ghostty-custom-*.pkg.tar.zst
	rm -rf $(REPO_ROOT)/src
	rm -rf $(REPO_ROOT)/pkg

# Ghostty Custom — Arch Linux Package

Split-package PKGBUILD for Ghostty with OSC 50 dynamic font support.

## What is this?

This is a custom build of [Ghostty](https://github.com/ghostty-org/ghostty)
that adds support for OSC 50 escape sequences, allowing applications to
dynamically change the terminal font family and size.

Packages produced:
- `ghostty-custom` — Main terminal binary, desktop file, icons, docs
- `ghostty-custom-shell-integration` — Shell integration scripts
- `ghostty-custom-terminfo` — Terminfo definitions (only entries not already provided by ncurses >= 6.5)

## Quick Start

```bash
# From the repo root:
make -f dist/arch/Makefile pkg      # Build package
make -f dist/arch/Makefile install  # Build and install
```

Or directly with makepkg:

```bash
makepkg -f -p dist/arch/PKGBUILD
sudo pacman -U ghostty-custom-*.pkg.tar.zst
```

## Updating .SRCINFO

```bash
make -f dist/arch/Makefile srcinfo
```

## Prerequisites

- `base-devel` (Arch Linux)
- `zig` >= 0.15
- `pandoc-cli` (for docs)
- `blueprint-compiler` (for GTK UI)

## Forks

To adapt this PKGBUILD for your own fork:

1. Update `url=` to point to your repository
2. Update `pkgver=` to match your build
3. Regenerate `.SRCINFO` with `make srcinfo`

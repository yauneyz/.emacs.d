#!/usr/bin/env bash
set -euo pipefail

# Ensure required tooling for language servers and formatters is installed.
# Each block checks for the tool before installing so the script is idempotent.

echo "→ Installing Node-based language servers"
if command -v npm >/dev/null 2>&1; then
  npm install -g typescript-language-server typescript eslint_d prettier >/dev/null
  npm install -g pyright >/dev/null
else
  echo "⚠️  npm not found; skip TypeScript/Pyright installs" >&2
fi

echo "→ Ensuring Go toolchain extras"
if command -v go >/dev/null 2>&1; then
  go install golang.org/x/tools/gopls@latest
  go install mvdan.cc/gofumpt@latest
  go install github.com/fatih/gomodifytags@latest
  go install github.com/josharian/impl@latest
  go install gotest.tools/gotestsum@latest
else
  echo "⚠️  go not found; skip Go language helper installs" >&2
fi

echo "→ Ensuring Python language servers"
if command -v pipx >/dev/null 2>&1; then
  pipx install --include-deps ruff-lsp >/dev/null || pipx upgrade ruff-lsp >/dev/null
  pipx install --include-deps debugpy >/dev/null || pipx upgrade debugpy >/dev/null
elif command -v pip >/dev/null 2>&1; then
  python3.12 -m pip install --user ruff-lsp debugpy >/dev/null
else
  echo "⚠️  pip/pipx not found; skip Python helpers" >&2
fi

echo "→ Ensuring Rust components"
if command -v rustup >/dev/null 2>&1; then
  rustup component add rustfmt clippy
  rustup component add rust-analyzer
else
  echo "⚠️  rustup not found; skip rust-analyzer" >&2
fi

echo "→ Ensuring Haskell Language Server"
if command -v ghcup >/dev/null 2>&1; then
  ghcup install hls >/dev/null
  ghcup set hls >/dev/null
else
  echo "⚠️  ghcup not found; skipping HLS install" >&2
fi

echo "→ Done"

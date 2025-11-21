# Repository Guidelines

## Project Structure & Module Organization
- `init.el` loads every `lisp/NN-topic.el` in numeric order; keep new files numerically slotted and expose features with `provide 'lang-foo` so `51-lang-bundle.el` can `require` them.
- UI, completion, and language plumbing live in focused modules (`20-ui.el`, `50-lsp.el`, `55-clojure.el`, etc.); only drop scripts in `install-scripts/` and keep project-specific glue out of `straight/` and `var/`.
- Tree-sitter grammars install themselves via `treesit-auto`; extra grammars and language servers belong beside `install-scripts/lang-servers.sh` to stay reproducible.

## Build, Test, and Development Commands
- `emacs --init-directory ~/.emacs.d --batch -l init.el` smoke-tests the full config; add `--eval '(message "OK")'` to gate CI scripts.
- `emacs --init-directory ~/.emacs.d` launches the interactive build; use `SPC d d` (DAP) and `SPC t t` (test hydra) for per-language workflows.
- `bash install-scripts/lang-servers.sh` provisions gopls, pyright, rust-analyzer, HLS, ruff-lsp, and Node toolchains; run `install-scripts/go.sh` when you only need Go extras.

## Coding Style & Naming Conventions
- Emacs Lisp stays 2-space indented with `lexical-binding` headers; wrap package setup in `use-package` and stash helper fns underneath comment rulers (`;; --- Section`).
- Stick to explicit requires (`(require 'lang-python)`) rather than implicit load order, and prefer buffer-local `setq-local` for mode tweaks.
- Keep key prefixes consistent with existing leader conventions (`<leader>l` for LSP, `<leader>d` for DAP, `<leader>t` for tests).

## Testing Guidelines
- Drop new regression checks under `lisp/tests/` using ERT; seed buffers with `with-temp-buffer` and `should` for deterministic assertions.
- Language hydras delegate to compilers: use `+test/run-file` (Go/Rust/Jest) and `+test/haskell-run-project-tests`; document manual verification steps when relying on REPL-only flows (CIDER, Org exports).
- Batch-run suites with `emacs --init-directory ~/.emacs.d --batch -l init.el -l lisp/tests/<file>-test.el -f ert-run-tests-batch-and-exit` before opening a PR.

## Commit & Pull Request Guidelines
- Follow the log’s present-tense style (`"Add HLS stack wiring"`) and include bodies when behavior shifts or new dependencies appear.
- PR descriptions should list motivation, testing (`batch init`, `+test/run-project`, screenshots for UI), and linked issues or TODOs so other agents can pick up quickly.
- Trim noisy diffs from caches (`recentf`, `var/`), and re-run `git status` to confirm only intentional files move forward.

## Security & Configuration Tips
- Keep API keys and machine-local preferences in `custom.el` or `authinfo`; never commit secrets to tracked modules.
- Before merge, eyeball generated directories (`straight/`, `elpa/`, `tree-sitter/`) and revert incidental changes so reviews stay focused on code.

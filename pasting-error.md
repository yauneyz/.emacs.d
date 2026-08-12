# Clipboard paste broken in Emacs on cold boot (Hyprland)

**Date investigated:** 2026-08-12
**Status:** Fix applied, pending verification after rebuild + reboot.

## Symptom

Pasting from the system clipboard into Emacs does not work in the Emacs
instances launched at Hyprland startup. Killing and restarting Emacs fixes it
for the rest of the session. Happens on every cold boot.

## Setup at the time

- Emacs 30.2, pgtk build (native Wayland), wrapped in
  `~/nixos-config/modules/home/emacs.nix` with wl-clipboard on PATH.
- Hyprland startup in `~/nixos-config/modules/home/hyprland/exec-once.nix`
  fires all commands at once on the `hyprland.start` event:
  `dbus-update-activation-environment`, `systemctl --user import-environment`,
  `wl-clip-persist --clipboard both`, `wl-paste --watch cliphist store`,
  waybar, etc., and `hypr-startup`.
- `~/nixos-config/modules/home/scripts/scripts/hypr-startup.sh` launches three
  standalone pgtk Emacs frames (workspaces 3, 9, 15) within the first couple of
  seconds of compositor life.
- Emacs-side clipboard config (`~/.emacs.d/lisp/30-editor.el`,
  `my/configure-clipboard`) already includes the Wayland
  `text/plain;charset=utf-8` target in `x-select-request-type`, so MIME-type
  mismatch was ruled out.

## Diagnosis

Prime suspect: **`wl-clip-persist --clipboard both`**.

- wl-clip-persist's own README warns that primary-selection mode (which `both`
  enables) "breaks the selection system" in some applications, and recommends
  `--clipboard regular`.
- It has a documented history of leaving paste-receiving apps wedged:
  - basecamp/omarchy#1885 — Chromium/Brave/Electron apps hang indefinitely on
    paste; wl-clip-persist never completes serving the data.
  - basecamp/omarchy#1062 — wl-clip-persist randomly breaks the clipboard;
    fix was to remove it.
- Mechanism matching the symptom: during startup churn wl-clip-persist grabs
  the selection and a clipboard read inside Emacs's GTK layer never completes.
  GTK's clipboard state machine stays wedged for the life of that process, so
  paste is dead in that Emacs instance until restart — a fresh process gets
  fresh GTK state, which is why restarting "fixes" it.

Secondary factor: a startup race — `hl.exec_cmd` doesn't sequence anything, so
the first Emacs can start before the clipboard daemons / data-control protocol
are actually serving.

## Fix applied (2026-08-12)

1. `exec-once.nix`: `wl-clip-persist --clipboard both` →
   `wl-clip-persist --clipboard regular`. Loses nothing meaningful — primary
   selection persistence is the part that misbehaves.
2. `hypr-startup.sh`: readiness poll at the top (up to ~6 s) that waits until
   `wl-paste --list-types` answers — either success or a "No selection" error
   means the data-control protocol is live — before launching any apps.

## Follow-up (2026-08-12, later): copy direction broke too

Copying *from* Emacs to the system clipboard stopped working in the running
Emacs instance (started earlier the same day). Live diagnosis:

- The session was still running the **old** config — `wl-clip-persist
  --clipboard both` had been up since Aug 10; the nix fix only takes effect
  after rebuild + re-login.
- `wl-copy` → `wl-paste` roundtrip in a terminal: worked.
- A fresh `emacs -Q` (pgtk) calling `gui-select-text`: copy reached the system
  clipboard fine, even with `--clipboard both` still running.
- Conclusion: the failure is **per-process**, not global — the same GTK
  clipboard-state wedge as the paste bug, hitting the copy direction. A wedged
  Emacs instance stays broken until that Emacs is restarted.

Live remediation applied: killed wl-clip-persist and relaunched it as
`wl-clip-persist --clipboard regular` inside the running session, then
verified the roundtrip. The wedged Emacs itself still needs a restart to
recover; the nix rebuild makes the change permanent.

## If it ever comes back

- Fallback: drop wl-clip-persist entirely. `wl-paste --watch cliphist store`
  already persists text history; wl-clip-persist only adds "most recent item
  survives the source app closing" (mainly relevant for images).
- To identify which side is at fault while broken:
  - `wl-paste` in a terminal works, but
    `M-: (gui-get-selection 'CLIPBOARD 'UTF8_STRING)` in the broken Emacs
    returns nil → wl-clip-persist / GTK interaction.
  - `wl-paste` is also empty/failing → compositor-side startup race.
- References:
  - https://github.com/basecamp/omarchy/issues/1885
  - https://github.com/basecamp/omarchy/issues/1062
  - https://github.com/Linus789/wl-clip-persist
  - https://wiki.hypr.land/0.47.0/Useful-Utilities/Clipboard-Managers/

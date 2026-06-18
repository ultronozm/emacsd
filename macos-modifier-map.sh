#!/usr/bin/env bash
set -euo pipefail

# Modifier-key remap for macOS (hidutil):
#   Caps Lock    -> Left Control     (Caps becomes a normal Control)
#   Left Control -> Right Control    (Emacs init.el sets ns-right-control-modifier
#                                      to 'super, so physical Left Control = Super)
#
# `install-agent` makes it stick across logins via a LaunchAgent that travels with
# this repo (path is derived, not hardcoded — works for any user/$HOME).
#
# NOTE: do NOT also remap Caps Lock in System Settings > Keyboard > Modifier Keys;
# that path sends a keycode Emacs reads as Right Control (-> Super). Let this script
# own Caps Lock.

LABEL="com.emacsd.emacs-modifier-map"
PLIST="$HOME/Library/LaunchAgents/${LABEL}.plist"
SELF="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"

usage() {
  cat <<'USAGE'
Usage: macos-modifier-map.sh [set|clear|show|install-agent|uninstall-agent]

  set              Apply the mappings now (Caps->LCtrl, LCtrl->RCtrl).
  clear            Remove all hidutil user key mappings.
  show             Show current hidutil user key mappings.
  install-agent    Apply now + install a per-login LaunchAgent (path-portable).
  uninstall-agent  Unload + remove the LaunchAgent.
USAGE
}

cmd_set() {
  hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x7000000E0},{"HIDKeyboardModifierMappingSrc":0x7000000E0,"HIDKeyboardModifierMappingDst":0x7000000E4}]}'
}

cmd_clear() { hidutil property --set '{"UserKeyMapping":[]}'; }
cmd_show()  { hidutil property --get "UserKeyMapping"; }

cmd_install_agent() {
  mkdir -p "$HOME/Library/LaunchAgents"
  cat > "$PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>${LABEL}</string>
  <key>ProgramArguments</key>
  <array>
    <string>${SELF}</string>
    <string>set</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
</dict>
</plist>
PLIST
  launchctl unload "$PLIST" 2>/dev/null || true
  launchctl load "$PLIST"
  cmd_set
  echo "installed + loaded: $PLIST"
  echo "  runs at each login: $SELF set"
}

cmd_uninstall_agent() {
  launchctl unload "$PLIST" 2>/dev/null || true
  rm -f "$PLIST"
  echo "removed: $PLIST"
}

mode="${1:-set}"
case "$mode" in
  set)              cmd_set ;;
  clear)            cmd_clear ;;
  show)             cmd_show ;;
  install-agent)    cmd_install_agent ;;
  uninstall-agent)  cmd_uninstall_agent ;;
  -h|--help|help)   usage ;;
  *) usage >&2; exit 2 ;;
esac

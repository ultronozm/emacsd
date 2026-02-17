#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: macos-modifier-map.sh [set|clear|show]

  set   Apply key mappings:
        Caps Lock -> Left Control
        Left Control -> Right Control
  clear Remove all hidutil user key mappings.
  show  Show current hidutil user key mappings.
USAGE
}

cmd_set() {
  hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x7000000E0},{"HIDKeyboardModifierMappingSrc":0x7000000E0,"HIDKeyboardModifierMappingDst":0x7000000E4}]}'
}

cmd_clear() {
  hidutil property --set '{"UserKeyMapping":[]}'
}

cmd_show() {
  hidutil property --get "UserKeyMapping"
}

mode="${1:-set}"
case "$mode" in
  set)   cmd_set ;;
  clear) cmd_clear ;;
  show)  cmd_show ;;
  -h|--help|help) usage ;;
  *)
    usage >&2
    exit 2
    ;;
esac

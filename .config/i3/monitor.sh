#!/usr/bin/env bash
set -euo pipefail

HDMI="HDMI-2"
VGA="VGA-2"
STATE_FILE="${XDG_RUNTIME_DIR:-/tmp}/i3-monitor-state"

is_connected() {
  xrandr --query | grep -q "^$1 connected"
}

new_state="none"
if is_connected "$HDMI" && is_connected "$VGA"; then
  new_state="hdmi+vga"
elif is_connected "$HDMI"; then
  new_state="hdmi"
elif is_connected "$VGA"; then
  new_state="vga"
fi

old_state="$(cat "$STATE_FILE" 2>/dev/null || true)"
[ "$new_state" = "$old_state" ] && exit 0

case "$new_state" in
  hdmi+vga)
    xrandr \
      --output "$HDMI" --primary --mode 1920x1080 --rate 60 --pos 0x0 \
      --output "$VGA" --mode 1280x720 --right-of "$HDMI"
    ;;
  hdmi)
    xrandr \
      --output "$HDMI" --primary --mode 1920x1080 --rate 60 --pos 0x0 \
      --output "$VGA" --off
    ;;
  vga)
    xrandr \
      --output "$VGA" --primary --mode 1280x720 --pos 0x0 \
      --output "$HDMI" --off
    ;;
  none)
    ;;
esac

echo "$new_state" > "$STATE_FILE"

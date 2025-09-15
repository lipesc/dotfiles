#!/bin/bash

# Script Paths
log_file="$HOME/.config/waybar/cpugovernor.log"
prev_gov_file="$HOME/.cache/prev_governor.txt"

# Ensure cache file exists
touch "$prev_gov_file"

# Read current and previous governors
current_governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
previous_governor=$(cat "$prev_gov_file")

# Notify and log on change
if [ "$current_governor" != "$previous_governor" ]; then
  notify-send "CPU Governor" "Changed to $current_governor" --icon=system --app-name=cpugovernor
  echo "$current_governor" >"$prev_gov_file"
fi

# Output for Waybar
case "$current_governor" in
performance)
  echo '{"text": "󱐋 Performance", "class": "performance", "tooltip": "<b>Governor</b> Performance"}'
  ;;
powersave)
  echo '{"text": "󰒲 Powersave", "class": "powersave", "tooltip": "<b>Governor</b> Powersave"}'
  ;;
*)
  echo '{"text": "unknown", "class": "unknown", "tooltip": "<b>Governor</b> Unknown"}'
  ;;
esac 

#!/bin/bash
pkill polybar
polybar main &
sleep 1  # Aumentado para garantir que systray esteja pronto
copyq --start-server &
blueman-applet &
nm-applet &
pkill dunst && dunst &

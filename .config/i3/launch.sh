#!/bin/bash

sleep 2
copyq --start-server &
blueman-applet &
killall volctl
volctl &

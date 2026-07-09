killall xidlehook

# Run xidlehook
xidlehook \
  --not-when-fullscreen \
  --not-when-audio \
  --timer 3600 'i3lock -c 04090e' '' \
  --timer 7200 'systemctl suspend' ''
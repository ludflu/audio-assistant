#! /usr/bin/env bash

MSG=$1
TMPFILE=$(mktemp /tmp/talkout.XXXXXXX.wav)
pico2wave -w $TMPFILE "$MSG"
mpv $TMPFILE
#aplay $TMPFILE
rm $TMPFILE

#say "$MSG"

#! /usr/bin/env bash

TMPFILE=$(mktemp /tmp/talkout.XXXXXXX.wav)
MSG=$1
pico2wave -w $TMPFILE "$MSG"
aplay $TMPFILE


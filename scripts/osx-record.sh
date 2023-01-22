#! /usr/bin/env bash
FNAME=$1
DUR=$2
rec -c 2 -b 16 -L -r 16000 $FNAME trim 0 00:$DUR

#! /usr/bin/env bash
FNAME=$1
DUR=$2

#ssh jsnavely@rpivad "arecord -f cd -D plughw:3,0 --duration=$DUR -r 16000" >$FNAME
ssh jsnavely@rpivad "arecord -f cd --duration=$DUR -r 16000" >$FNAME

#echo "arecord -D sysdefault:CARD=PCH -f dat -r 16000 -d $DUR >$FNAME"
#arecord -D sysdefault:CARD=PCH -f dat -r 16000 -d "$DUR" >"$FNAME"

# 16bit little endian

# 2 channels (stereo) 16bit little endian 16khz 30 seconds
#rec -c 2 -b 16 -L -r 16000 in.wav trim 0 00:30


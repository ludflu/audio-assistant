# Voice Assistant
Voice Assistant provides basic functionality to enable the creation of a voice activated software agent.
An incoming stream of audio is chunked according to boundaries defined by a VAD (voice activity detector).
When a complete chunk is found, it is sent to a voice recognition API, and the resulting text is used to determine
what action, if any, should be taken

## Voice Activity Detector
Google's venerable [open source package WebRTC VAD](https://hackage.haskell.org/package/webrtc-vad) is used, and 30ms chunks of audio are sent at 16000hz
At some point it would be nice to upgrade to a more modern system.

## Voice Recognition API

OpenAI's Whisper model is used to recognize incoming audio and transform it to text.
A [**simple wrapper API**](https://gitlab.com/ludflu/whisper-asr) is provided, but other API's could be used instead.

## Audio Processing

In order to deal with audio in a streaming fashion, without the need to read all the samples into memory,
we use the [conduit-audio](https://hackage.haskell.org/package/conduit-audio) module to splice, resample and re-chunk.

## Desired actions

- [x] report the time
- [x] report the date
- [x] say hello
- [x] play the guessing game
- [ ] report the weather
- [ ] manage notes (CRUD + search)
- [ ] email someone
- [ ] manage contacts (CRUD + search)
- [ ] set reminders
- [ ] manage calendar items
- [ ] generic question answering based on sentence vector embeddings

## Building

### Linux

The followin packages are required for building and running:

 - zlib
 - pulseaudioFull
 - libsndfile
 - libsamplerate
 - lame
 - libao
 - pcre

If you have NixOS you can use the provided default.nix

### OS X

```
brew install zlib pulseaudio libsndfile libsamplerate libao pcre lame
export CPATH=/opt/homebrew/include
export LIBRARY_PATH=/opt/homebrew/lib
cabal build
```

### GHC

Tested with GHC 9.2.5, Cabal 3.6.2.0

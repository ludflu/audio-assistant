
app/Main.hs vad-audio:
	source ~/.zshrc
	cabal build

run:
	cabal clean
	cabal run vad-audio -- --recordingLength 1200 --wavpath tmp --ollamHost 192.168.1.200

dep:
	sudo apt install pulseaudio
	sudo apt install libpcre3 libpcre3-dev
	sudo apt install zlib1g-dev libsndfile1-dev sndfile-tools libsamplerate-dev libmp3lame-dev

build:
	cabal clean
	cabal build

test:
	cabal test

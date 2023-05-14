
app/Main.hs vad-audio:
	source ~/.zshrc
	cabal build

run:
	cabal run vad-audio -- --debug

dep:
	sudo apt install pulseaudio
	sudo apt install libpcre3 libpcre3-dev
	sudo apt install zlib1g-dev libsndfile1-dev sndfile-tools libsamplerate-dev libmp3lame-dev



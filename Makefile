app/Main.hs vad-audio:
	source ~/.zshrc
	cabal build

run:
	cabal run vad-audio -- --debug

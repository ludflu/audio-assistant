with (import <nixpkgs> { });
haskell.lib.buildStackProject {
  name = "HaskellR";
  buildInputs = [zlib pulseaudioFull libsndfile libsamplerate lame libao];
}


let pkgs = import <nixpkgs> { };
in 
  pkgs.mkShell {
  packages = with pkgs; [ haskell.packages.ghc924.ghc cabal-install xz zlib pulseaudioFull libsndfile libsamplerate lame libao pcre];
}


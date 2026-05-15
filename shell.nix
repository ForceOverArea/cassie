with import <nixpkgs> {};
stdenv.mkDerivation (with pkgs; {
  name = "lambda-fm";
  nativeBuildInputs = [ 
    cabal-install
    ghc
    haskell-language-server
  ];
  buildInputs = [
    zlib
  ];
})
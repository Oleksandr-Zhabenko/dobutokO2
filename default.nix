{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "dobutokO2";
  version = "0.44.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    aftovolio base bytestring directory end-of-exe mmsyn2-array mmsyn7l
    mmsyn7ukr-array mmsyn7ukr-common process vector
  ];
  executableHaskellDepends = with pkgs.haskellPackages; [
    aftovolio base bytestring directory end-of-exe mmsyn2-array mmsyn7l
    mmsyn7ukr-array mmsyn7ukr-common process vector
  ];
  homepage = "https://hackage.haskell.org/package/dobutokO2";
  description = "Helps to create experimental music from a file (or its part) and a Ukrainian text";
  license = pkgs.lib.licenses.mit;
  mainProgram = "dobutokO2";
}

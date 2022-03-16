{ pkgs ? import <nixpkgs> {} }:
  
with pkgs; stdenv.mkDerivation {
  name = "env";
  buildInputs = [ elmPackages.elm ];
}
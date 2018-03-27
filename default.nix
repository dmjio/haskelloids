{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./haskelloids.nix { GLEW = pkgs.glew; }

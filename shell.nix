{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, containers, gloss, hpack
      , protolude, stdenv, text, time
      }:
      mkDerivation {
        pname = "othaskell";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base containers gloss protolude text time
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          array base containers gloss protolude text time
        ];
        testHaskellDepends = [
          array base containers gloss protolude text time
        ];
        preConfigure = "hpack";
        homepage = "https://github.com/jmorag/othaskell#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

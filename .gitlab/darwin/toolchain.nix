{ system }:

let
  sources = import ./nix/sources.nix;
  nixpkgsSrc = sources.nixpkgs;
  pkgs = import nixpkgsSrc { inherit system; };
in

let
  hsPkgs = pkgs.haskellPackages;
  alex = hsPkgs.alex;
  happy = hsPkgs.happy;
  targetTriple = pkgs.stdenv.targetPlatform.config;
  ghc = pkgs.stdenv.mkDerivation rec {
    version = "8.10.7";
    name = "ghc";
    src = fetchTarball {
      url = "https://downloads.haskell.org/ghc/${version}/ghc-${version}-aarch64-apple-darwin.tar.xz";
      sha256 = "sha256:0dp5k4w60xbs3k8bpnwyrl520r623i7ra0d0iybgi91lzimbq1l8";
    };
    configureFlags = [
      "LLC=${llvm}/bin/llc"
      "OPT=${llvm}/bin/opt"
      "CLANG=/usr/bin/clang"
      "CONF_CC_OPTS_STAGE2=--target=${targetTriple}"
      "CONF_CXX_OPTS_STAGE2=--target=${targetTriple}"
      "CONF_GCC_LINKER_OPTS_STAGE2=--target=${targetTriple}"
    ];
    buildPhase = "true";

    # N.B. Work around #20253.
    nativeBuildInputs = [ pkgs.gnused ];
    postInstallPhase = ''
      settings="$out/lib/ghc-${version}/settings"
      sed -i -e "s%\"llc\"%\"${llvm}/bin/llc\"%" $settings
      sed -i -e "s%\"opt\"%\"${llvm}/bin/opt\"%" $settings
      sed -i -e "s%\"clang\"%\"/usr/bin/clang\"%" $settings
      sed -i -e 's%("C compiler flags", "")%("C compiler flags", "--target=aarch64-apple-darwin")%' $settings
      sed -i -e 's%("C++ compiler flags", "")%("C++ compiler flags", "--target=aarch64-apple-darwin")%' $settings
      sed -i -e 's%("C compiler link flags", "")%("C compiler link flags", "--target=aarch64-apple-darwin")%' $settings
    '';
  };

  ourtexlive = with pkgs;
    texlive.combine {
      inherit (texlive)
        scheme-medium collection-xetex fncychap titlesec tabulary varwidth
        framed capt-of wrapfig needspace dejavu-otf helvetic upquote;
    };
  fonts = with pkgs; makeFontsConf { fontDirectories = [ dejavu_fonts ]; };

  llvm = pkgs.llvm_11;
in

pkgs.writeTextFile {
  name = "toolchain";
  text = ''
    export PATH
    PATH="${pkgs.autoconf}/bin:$PATH"
    PATH="${pkgs.automake}/bin:$PATH"
    PATH="${pkgs.coreutils}/bin:$PATH"
    export FONTCONFIG_FILE=${fonts}
    export XELATEX="${ourtexlive}/bin/xelatex"
    export MAKEINDEX="${ourtexlive}/bin/makeindex"
    export HAPPY="${happy}/bin/happy"
    export ALEX="${alex}/bin/alex"
    export GHC="${ghc}/bin/ghc"
    export LLC="${llvm}/bin/llc"
    export OPT="${llvm}/bin/opt"
    export SPHINXBUILD="${pkgs.python3Packages.sphinx}/bin/sphinx-build"
    export CABAL_INSTALL="${pkgs.cabal-install}/bin/cabal"
    export CABAL="$CABAL_INSTALL"
    #export CURSES_LIB_DIRS="${pkgs.ncurses}/lib"
  '';
}


{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clash-cores = {
      url = "github:clash-lang/clash-compiler?dir=clash-cores";
      flake = false;
    };
  };

  outputs = { clash-cores, nixpkgs, flake-utils, self }:
    flake-utils.lib.eachDefaultSystem (system: 
  let
    name = "Blinker";
    family = "Cyclone V";
    device = "5CGXFC5C6F27C7";
    hdl = "vhdl";
    src = ./.;
 
    pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
    hpkgs = pkgs.haskellPackages.override {
      overrides = self: super: {
        clash-prelude = super.callHackageDirect {
        pkg = "clash-prelude";
        ver = "1.8.1";
        sha256 = "sha256-HUt8Aw5vMFWThp26e/FdVkcjGQK8rvUV/ZMlv/KvHgg=";
      } {};
        clash-ghc = super.callHackageDirect {
        pkg = "clash-ghc";
        ver = "1.8.1";
        sha256 = "sha256-oMK756+7WA5rGSRLkJ6Rpdkb2IkJ2VA6S82HGGyiK7Y=";
      } {};
        clash-lib = super.callHackageDirect {
        pkg = "clash-lib";
        ver = "1.8.1";
        sha256 = "sha256-/dFgCj9e+gkyyUDAB1n1ukaEnkugCR7cRkP+SFJmjjY=";
      } {};
        clash-cores = super.callCabal2nix "clash-cores" "${clash-cores}/clash-cores/" { };
      };
    };
    hpkg = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix name src {}) (drv: {
        enableLibraryProfiling = false;

        postBuild = ''
          ${hpkgs.clash-ghc}/bin/clash -package-db dist/package.conf.inplace ${name} --${hdl}
        '';

        postInstall = ''
          mkdir -p "$out/share"
          cp -r "${hdl}/" "$out/share/${hdl}"
        '';
    });

    f = "${hpkg}/share/${hdl}/${name}.topEntity";
    hdls = builtins.filter (x: pkgs.lib.hasSuffix hdl x) (builtins.attrNames (builtins.readDir f));
    # qsys = pkgs.lib.findFirst (x: pkgs.lib.hasSuffix "qsys" x) (throw "no qsys found") (builtins.attrNames (builtins.readDir f));
    qsf = pkgs.lib.concatStringsSep "\n" ([
      "set_global_assignment -name DEVICE ${device}"
      "set_global_assignment -name FAMILY \"${family}\""
      "set_global_assignment -name TOP_LEVEL_ENTITY ${name}"
      "set_global_assignment -name SDC_FILE ${name}.sdc"
      # "set_global_assignment -name QSYS_FILE ${qsys}"
    ] ++ (map (x: "set_global_assignment -name VHDL_FILE ${x}") hdls));
  in
    {
      packages.default = hpkg;
      packages.sof = pkgs.runCommand "${name}.sof" {
        buildInputs = [ pkgs.quartus-prime-lite ];
        src = pkgs.symlinkJoin {
          name = "${name}.topEntity";
          paths = [
            "${hpkg}/share/${hdl}/${name}.topEntity"
            (pkgs.writeTextDir "${name}.qsf" qsf)
            (pkgs.writeTextDir "${name}.qpf" ''
              PROJECT_REVISION = "${name}"
            '')
          ];
        };
      } ''
        mkdir $out
        ln -s $src/* .
        # quartus_map --read_settings_files=on --write_settings_files=off Blinker -c Blinker
        quartus_sh --flow compile ${name}
        cp -r * $out
      '';

      devShells.default = hpkgs.shellFor {
        packages = p: [ hpkg ];
        nativeBuildInputs = [
          pkgs.cabal-install
          hpkgs.haskell-language-server
          hpkgs.clash-ghc

          (pkgs.writeShellScriptBin "flash" ''
            quartus_pgm -m jtag -o "p;$1"
          '')
        ];
        buildInputs = [ pkgs.quartus-prime-lite ];
      };

      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
    }
  );
}

{
  description = "hsig: оффлайн-синтезатор и DSL для электронной музыки на Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      # Только Linux и macOS, см. docs/DESIGN.md, разд. 1.
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));

      # В стор попадает только то, от чего действительно зависит сборка.
      # docs/, out/ и dist-newstyle/ не должны инвалидировать кэш.
      hsigSource =
        pkgs:
        pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./hsig.cabal
            ./LICENSE
            ./patches
            ./src
            ./test
            ./tracks
          ];
        };

      # Берём набор по умолчанию (сейчас GHC 9.10.3): у него полное покрытие
      # бинарным кэшем. Прибивать конкретный ghcXYZ имеет смысл только когда
      # появится причина отойти от дефолта.
      haskellPackagesFor =
        pkgs:
        pkgs.haskellPackages.extend (
          hfinal: _hprev: {
            hsig = hfinal.callCabal2nix "hsig" (hsigSource pkgs) { };
          }
        );
    in
    {
      packages = forAllSystems (
        pkgs:
        let
          hp = haskellPackagesFor pkgs;
        in
        {
          default = hp.hsig;
          hsig = hp.hsig;
        }
      );

      # nix run .#demo: рендер демо-трека без входа в дев-шелл.
      apps = forAllSystems (
        pkgs:
        let
          hp = haskellPackagesFor pkgs;
        in
        {
          default = self.apps.${pkgs.stdenv.hostPlatform.system}.demo;
          demo = {
            type = "app";
            program = "${pkgs.lib.getExe' hp.hsig "demo"}";
            meta.description = "Рендер демо-трека hsig";
          };
        }
      );

      devShells = forAllSystems (
        pkgs:
        let
          hp = haskellPackagesFor pkgs;
        in
        {
          default = hp.shellFor {
            packages = ps: [ ps.hsig ];
            withHoogle = true;
            nativeBuildInputs = [
              hp.cabal-install
              hp.haskell-language-server
              hp.hlint
              hp.fourmolu
              hp.cabal-fmt
              hp.ghcid
              pkgs.pkg-config
              # Примеры книги коммитятся сжатыми: 23 МБ wav в репозитории
              # это перебор, а mp3 весит около двух.
              pkgs.lame
            ];
            shellHook = ''
              echo "hsig: cabal build | cabal test | cabal run demo"
            '';
          };

          # Отдельный шелл на случай, когда нужен -fllvm (см. флаг llvm в
          # hsig.cabal): даёт opt/llc той версии LLVM, с которой собран GHC.
          # Тянет полный LLVM, поэтому не в дефолтном шелле.
          llvm = self.devShells.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [ hp.ghc.llvmPackages.llvm ];
          });
        }
      );

      # nix flake check: собирает библиотеку и прогоняет tasty.
      checks = forAllSystems (pkgs: {
        hsig = (haskellPackagesFor pkgs).hsig;
      });

      # Для потребления hsig из других флейков.
      overlays.default = _final: prev: {
        haskellPackages = haskellPackagesFor prev;
      };

      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}

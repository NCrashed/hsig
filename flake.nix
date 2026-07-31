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
      # out/ и dist-newstyle/ не должны инвалидировать кэш.
      #
      # book/, docs/book и README.md тут не ради документации: книга это
      # исполняемый book, а тесты сверяют показанный в ней код с исходниками
      # и ссылки со звуковыми файлами. Без них тесты в песочнице падают.
      hsigSource =
        pkgs:
        pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./hsig.cabal
            ./LICENSE
            ./README.md
            ./book
            ./docs/book
            ./patches
            ./src
            ./test
            ./tracks
          ];
        };

      # Измеренные характеристики головы (MIT KEMAR, компактный набор, 200 КБ).
      # В репозиторий не кладём: фиксированный хэш даёт ту же
      # воспроизводимость без бинарников в истории. Лицензия разрешает любое
      # использование при ссылке на авторов: Bill Gardner и Keith Martin,
      # MIT Media Lab, 1994.
      kemar =
        pkgs:
        pkgs.runCommand "kemar-compact"
          {
            src = pkgs.fetchurl {
              url = "https://sound.media.mit.edu/resources/KEMAR/compact.tar.Z";
              hash = "sha256-JUjZ542150xKOaruIGHCR7r+0vnQ2gcNUfenSjX+R5c=";
            };
            nativeBuildInputs = [ pkgs.gzip ];
          }
          ''
            mkdir -p $out
            zcat $src | tar -xf - -C $out --strip-components=1
          '';

      # Берём набор по умолчанию (сейчас GHC 9.10.3): у него полное покрытие
      # бинарным кэшем. Прибивать конкретный ghcXYZ имеет смысл только когда
      # появится причина отойти от дефолта.
      haskellPackagesFor =
        pkgs:
        pkgs.haskellPackages.extend (
          hfinal: _hprev: {
            # Тестам нужен набор HRTF, и путь к нему приходит переменной
            # окружения, как и в дев-шелле: иначе проверки этапа M10 нечем
            # прогнать в песочнице.
            hsig = pkgs.haskell.lib.overrideCabal (hfinal.callCabal2nix "hsig" (hsigSource pkgs) { }) (
              old: {
                preCheck = (old.preCheck or "") + ''
                  export HSIG_HRTF=${kemar pkgs}
                '';
              }
            );
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

            # Путь к набору HRTF: библиотека и тесты читают его отсюда.
            HSIG_HRTF = "${kemar pkgs}";

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

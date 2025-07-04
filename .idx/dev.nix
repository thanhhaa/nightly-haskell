# To learn more about how to use Nix to configure your environment
# see: https://developers.google.com/idx/guides/customize-idx-env
{ pkgs, ... }: {
  # Which nixpkgs channel to use.
  channel = "stable-24.05"; # or "unstable" or "stable-23.11"
  # Use https://search.nixos.org/packages to find packages
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.hlint
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.implicit-hie
    pkgs.haskellPackages.ghcide
    pkgs.haskellPackages.hiedb
    pkgs.haskellPackages.fourmolu
    # An experiment of formatting .cabal files
    pkgs.haskellPackages.cabal-fmt
  ];
  # Sets environment variables in the workspace
  env = { };
  idx = {
    # Search for the extensions you want on https://open-vsx.org/ and use "publisher.id"
    extensions = [
      # "vscodevim.vim"
      "haskell.haskell"
      "justusadam.language-haskell"
    ];
    # Enable previews
    previews = {
      enable = true;
      previews = {
        # web = {
        #   # Example: run "npm run dev" with PORT set to IDX's defined port for previews,
        #   # and show it in IDX's web preview panel
        #   command = ["npm" "run" "dev"];
        #   manager = "web";
        #   env = {
        #     # Environment variables to set for your server
        #     PORT = "$PORT";
        #   };
        # };
      };
    };
    # Workspace lifecycle hooks
    workspace = {
      # Runs when a workspace is first created
      onCreate = {
        # Example: install JS dependencies from NPM
        # npm-install = "npm install";
        # Open editors for the following files by default, if they exist:
        default.openFiles = [ "src/Main.hs" ".idx/dev.nix" "README.md" ];
      };
      # Runs when the workspace is (re)started
      onStart = {
        # Example: start a background task to watch and re-build backend code
        # watch-backend = "npm run watch-backend";
      };
    };
  };
}

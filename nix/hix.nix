{ pkgs, ... }: {
  # name = "project-name";
  compiler-nix-name = "ghc8107"; # Version of GHC to use

  # Keep the dev shell native-only. The hix template's cross targets pull large
  # GHC toolchains from cache.iog.io, which makes direnv fragile on poor links.
  crossPlatforms = _: [ ];

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  # shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "1.8.0.0";
  # shell.tools.ormolu = "latest";
  shell.withHoogle = true;
}

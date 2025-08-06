### Format manually

#### Cabal

```shell
find . -name "*.cabal" -exec cabal-fmt --inplace {} \;
```

#### Nix

```shell
find . -name "*.nix" -exec nixpkgs-fmt {} \;
```

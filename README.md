# Hacking


## On backend

```bash
nix-shell -A shells.ghc --run 'cabal new-repl lib:rhyolite-backend'
```


## On frontend

```bash
nix-shell -A shells.ghc --arg frontend-only true --run 'cabal new-repl lib:rhyolite-frontend[-run] --project-file frontend.project'
```

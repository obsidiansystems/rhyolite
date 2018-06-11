# Hacking


## On backend

```bash
nix-shell -A shells.ghc --run 'cabal new-repl lib:rhyolite-backend'
```


## On frontend

```bash
cd frontend
nix-shell .. -A shells.ghc --arg frontend true --run 'cabal new-repl'
```

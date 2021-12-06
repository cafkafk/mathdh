#!/usr/bin/env sh
stack build --nix --compiler ghc-8.10.7
stack install --nix --compiler ghc-8.10.7

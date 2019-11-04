#!/usr/bin/env bash
rm -f .ghc.env*
if [ -z "$IN_NIX_SHELL" ]
then
  nix-shell --run "runhaskell ./shake.hs $@"
else
  runhaskell ./shake.hs $@
fi

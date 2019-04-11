#!/usr/bin/env bash
rm -f .ghc.env*
runhaskell shake.hs $@

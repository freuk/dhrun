#!/usr/bin/env bash
rm .ghc.env*
runhaskell shake.hs $@

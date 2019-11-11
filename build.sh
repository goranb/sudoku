#!/usr/bin/env bash

# build & invoke
ghc -threaded -eventlog -rtsopts --make sudoku.hs -o dist/sudoku -outputdir dist # && ./dist/sudoku

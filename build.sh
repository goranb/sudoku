#!/usr/bin/env bash

# build
ghc --make sudoku.hs -o dist/sudoku -outputdir dist
# invoke
./dist/sudoku

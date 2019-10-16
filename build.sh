#!/usr/bin/env bash

# build & invoke
ghc --make sudoku.hs -o dist/sudoku -outputdir dist && ./dist/sudoku

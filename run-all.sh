#!/bin/bash

. ./.hsenv/bin/activate

mkdir -p _build
rm -rf _build/*

for PROBLEM_SRC in src/Problem-*.hs
do
    PROBLEM=${PROBLEM_SRC#src/}
    PROBLEM=${PROBLEM/.hs/}
    echo Compiling $PROBLEM ...
    ghc -O2 $PROBLEM_SRC -o _build/${PROBLEM}.bin

    timeout 5s /usr/bin/time -f "%e" ./_build/${PROBLEM}.bin
done

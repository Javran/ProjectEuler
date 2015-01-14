#!/bin/bash

. ./.hsenv/bin/activate

mkdir -p _build
rm -rf _build/*

for PROBLEM_SRC in `ls src/Problem-*.hs | sort -V`
do
    PROBLEM=${PROBLEM_SRC#src/}
    PROBLEM=${PROBLEM/.hs/}
    echo Compiling $PROBLEM ...
    ghc -O2 $PROBLEM_SRC -o _build/${PROBLEM}.bin -outputdir=_build -fforce-recomp
    cd _build

    timeout 5s /usr/bin/time -f "%e" ./${PROBLEM}.bin
    cd ..
done

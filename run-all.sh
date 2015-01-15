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
    /usr/bin/time -f "%e" -o time-report.txt -a -- ./${PROBLEM}.bin
    echo exitcode = $?
    cd ..
done

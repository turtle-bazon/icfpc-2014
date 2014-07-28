#!/bin/sh

SOLUTION_DIR=/tmp/icfpc-2014-solution
SOLUTION_FILE=skobochka-icfpc-2014-solution.tar.gz

rm -rf $SOLUTION_DIR
rm -f $SOLUTION_FILE
rm -f $SOLUTION_FILE.sha1
mkdir $SOLUTION_DIR

mkdir $SOLUTION_DIR/solution
mkdir $SOLUTION_DIR/code

cp ilisp/examples/power-pill-hunter.gcc $SOLUTION_DIR/solution/lambdaman.gcc
cp ghc/ghost-build/deadend-predict.ghc $SOLUTION_DIR/solution/ghost0.ghc
cp ghc/ghost-build/deadend-pursue.ghc $SOLUTION_DIR/solution/ghost1.ghc
cp README.md $SOLUTION_DIR/code/
cp -R ai $SOLUTION_DIR/code/
cp -R gcc $SOLUTION_DIR/code/
cp -R ghc $SOLUTION_DIR/code/
cp -R ilisp $SOLUTION_DIR/code/

tar -C $SOLUTION_DIR -czf $SOLUTION_FILE .
rm -rf $SOLUTION_DIR
openssl dgst -sha1 $SOLUTION_FILE > $SOLUTION_FILE.sha1


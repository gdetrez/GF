#! /bin/bash

### This script builds a binary distribution of GF from the source package
### that this script is a part of. It also assumes that you have installed
### the Haskell Platform, version 2010.1.0.0 or 2010.2.0.0

destdir=/tmp/gf-build-binary-dist  # assemble binary dist here
prefix=/usr/local                  # where to install
targz=gf-bin.tar.gz                # the final tar file, should be renamed 
langs=""                           # which languages?
#langs="langs=-Pol" # temporary problem with Polish, omit it

set -e                             # Stop if an error occurs
set -x                             # print commands before exuting them

runhaskell Setup.hs configure --user --prefix $prefix -fserver
runhaskell Setup.hs build $langs
runhaskell Setup.hs copy --destdir=$destdir $langs

tar -C $destdir/$prefix -zcf $targz .
echo "Created $targz, rename it to something more informative"
rm -r $destdir

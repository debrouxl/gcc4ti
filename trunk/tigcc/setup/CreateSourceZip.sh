#!/bin/bash

ZIPDIR=../../tigcc-win32
A68KSRCDIR=/e/TI-89/A68k/source

mkdir a68k
cp -f "$A68KSRCDIR"/* a68k/

rm -f "$ZIPDIR/tigccsrc.zip"
cd ../doc
zip -9 -r "doc.zip" Programs System -x CVS
cd ../setup
zip -9 -r "$ZIPDIR/tigccsrc.zip" ../Readme.txt ../License.txt a68k ../archive ../components ../doc/doc.zip ../gcc ../general ../hsf2rc ../ide ../ld-tigcc ../pstarter ../setup/*.nsi ../setup/*.ini ../setup/*.bat ../setup/*.sh ../setup/makeall.pif ../tigcc ../tprbuilder ../ttpack -x CVS
rm -f ../doc/doc.zip

rm -rf a68k

echo Finished.

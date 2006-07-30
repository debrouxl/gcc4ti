#!/bin/bash

ZIPDIR=`pwd`/../../tigcc-win32
A68KSRCDIR=/e/TI-89/A68k/source

rm -f "$ZIPDIR/tigccsrc.zip"
mkdir srczip
mkdir srczip/doc
cp -rfp ../doc .
cd doc
find . -name CVS -exec rm -rf "{}" \;
zip -9 -r "../srczip/doc/doc.zip" Programs System -x CVS/\*
cd ..
rm -rf doc
mkdir srczip/a68k
cp -p -f "$A68KSRCDIR"/* srczip/a68k/
cp -rfp ../Readme.txt ../License.txt ../archive ../components ../gcc ../general ../hsf2rc ../ide ../ld-tigcc ../pstarter ../setup ../tigcc ../tprbuilder ../ttpack srczip/
cd srczip
find . -name CVS -exec rm -rf "{}" \;
zip -9 -r "$ZIPDIR/tigccsrc.zip" Readme.txt License.txt a68k archive components doc/doc.zip gcc general hsf2rc ide ld-tigcc pstarter setup/*.nsi setup/*.ini setup/*.bat setup/*.sh setup/MAKEALL.PIF tigcc tprbuilder ttpack -x CVS/\*
cd ..
rm -rf srczip

echo Finished.

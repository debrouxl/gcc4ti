#!/bin/bash

ZIPDIR=`pwd`/../../../gcc4ti-source
A68KSRCDIR=~/a68k/source

rm -f "$ZIPDIR/gcc4tisrc.zip"
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
cp -rfp ../Readme.txt ../License.txt ../archive ../components ../gcc ../general ../hsf2rc ../ide ../ld-tigcc ../pstarter ../setup ../tigcc ../tools ../tprbuilder srczip/
cd srczip
find . -name CVS -exec rm -rf "{}" \;
zip -9 -r "$ZIPDIR/gcc4tisrc.zip" Readme.txt License.txt a68k archive components doc/doc.zip gcc general hsf2rc ide ld-tigcc pstarter setup/*.nsi setup/*.ini setup/*.bat setup/*.sh setup/MAKEALL.PIF tigcc tools tprbuilder -x CVS/\*
cd ..
rm -rf srczip

echo Finished.

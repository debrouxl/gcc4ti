#!/bin/bash

TIGCCDIR=../../tigcc-win32
TIGCCCVSDIR=..

echo Scanning executables for viruses...
if clamscan -ir "$TIGCCDIR/tigcc.exe" "$TIGCCDIR/tprbuilder.exe" "$TIGCCDIR/Bin" "$TIGCCCVSDIR/doc/Programs" Setup.exe
then echo OK
else echo "ERROR: Viruses found!" ; exit 1
fi

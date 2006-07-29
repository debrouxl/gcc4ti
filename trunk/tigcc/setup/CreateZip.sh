#!/bin/bash

ZIPDIR=../../tigcc-win32
TIGCCCVSDIR=..

rm -f "$ZIPDIR/tigcc.zip"
zip -j -9 "$ZIPDIR/tigcc.zip" "$TIGCCCVSDIR/readme/Readme.txt" "$TIGCCCVSDIR/License.txt" "Setup.exe"
rm -f Setup.exe

echo Finished.

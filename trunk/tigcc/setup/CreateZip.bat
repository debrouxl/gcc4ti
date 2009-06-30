@ECHO OFF

SET INFOZIPDIR=E:\zip\zip
SET ZIPDIR=E:\TI-89\tigcc-096
SET TIGCCCVSDIR=E:\TI-89\tigcccvs

DEL "%ZIPDIR%\tigcc.zip"
"%INFOZIPDIR%\zip.exe" -j -9 "%ZIPDIR%\tigcc.zip" "%TIGCCCVSDIR%\readme\Readme.txt" "%TIGCCCVSDIR%\License.txt" "Setup.exe"
DEL Setup.exe

ECHO Finished.

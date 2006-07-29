@ECHO OFF

SET UPXDIR=E:\UPX
SET IDEDIR=E:\TI-89\Compilers\TIGCC\Bin

"%UPXDIR%\UPX.exe" -9 "%IDEDIR%\ide.exe"

ECHO Finished.

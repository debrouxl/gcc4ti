@ECHO OFF

SET UPXDIR=E:\UPX
SET IDEDIR=E:\TI-89\Compilers\TIGCC\Bin

"%UPXDIR%\UPX.exe" -8 "%IDEDIR%\ide.exe"

ECHO Finished.

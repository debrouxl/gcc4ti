@ECHO OFF

SET TIGCCDIR=E:\TI-89\Compilers\TIGCC
SET TIGCCCVSDIR=E:\TI-89\tigcccvs
SET CLAMWINDIR=E:\clamav\ClamWin

ECHO Scanning executables for viruses...
"%CLAMWINDIR%\bin\clamscan" -ir -d "%CLAMWINDIR%\db" "%TIGCCDIR%\tigcc.exe" "%TIGCCDIR%\tprbuilder.exe" "%TIGCCDIR%\Bin" "%TIGCCCVSDIR%\doc\Programs" Setup.exe
IF NOT ERRORLEVEL 1 GOTO OK
ECHO.
ECHO ERROR: Viruses found!
PAUSE
:OK
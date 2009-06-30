@ECHO OFF

SET INFOZIPDIR=E:\zip\zip
SET ZIPDIR=E:\TI-89\tigcc-096
SET A68KSRCDIR=E:\TI-89\A68k\source

MD a68k
COPY "%A68KSRCDIR%\*.*" a68k

DEL "%ZIPDIR%\tigccsrc.zip"
CD ..\doc
"%INFOZIPDIR%\zip.exe" -9 -r "doc.zip" Programs System -x .svn
CD ..\setup
"%INFOZIPDIR%\zip.exe" -9 -r "%ZIPDIR%\tigccsrc.zip" ..\Readme.txt ..\License.txt a68k ..\archive ..\components ..\doc\doc.zip ..\gcc ..\general ..\hsf2rc ..\ide ..\ld-tigcc ..\pstarter ..\setup\*.nsi ..\setup\*.ini ..\setup\*.bat ..\setup\*.sh ..\setup\makeall.pif ..\tigcc ..\tools ..\tprbuilder -x .svn
DEL ..\doc\doc.zip

DELTREE /Y a68k

ECHO Finished.

@echo off
REM --------------------------------------------------------------------
REM This nifty batch script will compile one tool which is specified
REM on the commandline
REM
REM for example if we want to build ttpack we will call this tool:
REM buildone.bat ttpack
REM
REM if upx is found in the path it will pack the executables otherwise
REM they stay in their original form
REM
REM at the end of the batch script the final executable is moved into
REM the calctools bin directory
REM --------------------------------------------------------------------
echo compiling %1 ...
gcc %1.c -o %1.exe
@IF ERRORLEVEL 1 goto failed

start /min /wait upx -9 %1.exe > nul
@IF ERRORLEVEL 1 goto skipped
goto notskipped

:skipped
echo compression skipped

:notskipped
echo moving executable ...
mkdir bin
move %1.exe bin\
goto end

:failed
pause
goto end

:end

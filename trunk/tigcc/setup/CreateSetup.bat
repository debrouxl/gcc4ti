@ECHO OFF

SET NSISDIR=E:\NSIS\

del Setup.exe
%NSISDIR%\makensis.exe tigcc.nsi

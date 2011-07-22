@echo off
REM --------------------------------------------------------------------
REM
REM This batch script will loop through a list of all pctools and
REM try to execute buildone.bat for them.
REM
REM Due to memory restriction we build them in more than one loop
REM I know this can be fixed by increasing the shell memory but
REM I don't want to "confuse" future users.
REM --------------------------------------------------------------------

set ttsct=ttbin2hex ttbin2oth ttbin2str
for %%a in (%ttsct%) do call buildone.bat %%a

set ttsct=ttextract ttstrip ttarchive
for %%a in (%ttsct%) do call buildone.bat %%a

set ttsct=ttpack ttunpack tthelp ttinfo 
for %%a in (%ttsct%) do call buildone.bat %%a

set ttsct=ttchecksum ttbin2bin ttsplit ttppggen
for %%a in (%ttsct%) do call buildone.bat %%a

set ttsct=tttiler ttunarchive tthex2bin ttsetname
for %%a in (%ttsct%) do call buildone.bat %%a

set ttsct=ttbin2txt ttdos2ebk ttebkgen ttunebk
for %%a in (%ttsct%) do call buildone.bat %%a

@echo on


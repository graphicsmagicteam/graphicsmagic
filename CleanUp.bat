@ECHO OFF
REM Cleanup all temp files, maps and remote debug files from the Repositories

del /S /Q *.~*
del /S /Q *.map
del /S /Q *.rsm
pause
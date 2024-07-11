@echo off
SET THEFILE=C:\fpcupdeluxe\Projetos\horse\console\lib\win32\console.exe
echo Linking %THEFILE%
C:\fpcupdeluxe\fpc\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  --gc-sections  -s  --entry=_mainCRTStartup    -o C:\fpcupdeluxe\Projetos\horse\console\lib\win32\console.exe C:\fpcupdeluxe\Projetos\horse\console\lib\win32\link7208.res
if errorlevel 1 goto linkend
C:\fpcupdeluxe\fpc\bin\i386-win32\postw32.exe --subsystem console --input C:\fpcupdeluxe\Projetos\horse\console\lib\win32\console.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end

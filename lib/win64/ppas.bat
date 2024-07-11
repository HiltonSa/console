@echo off
SET THEFILE=C:\fpcupdeluxe\Projetos\horse\console\lib\win64\console.exe
echo Linking %THEFILE%
C:\fpcupdeluxe\fpc\bin\i386-win32\x86_64-win64-ld.exe -b pei-x86-64  --gc-sections  -s  --entry=_mainCRTStartup    -o C:\fpcupdeluxe\Projetos\horse\console\lib\win64\console.exe C:\fpcupdeluxe\Projetos\horse\console\lib\win64\link5644.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end

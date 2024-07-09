@echo off
cd poly_ch4cl
if errorlevel 1 goto batcherror
move Debug\poly_ch4cl.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_ch4o
if errorlevel 1 goto batcherror
move Debug\poly_ch4o.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_ch5
if errorlevel 1 goto batcherror
move Debug\poly_ch5.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_clhbr
if errorlevel 1 goto batcherror
move Debug\poly_clhbr.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_cmc
if errorlevel 1 goto batcherror
move Debug\poly_cmc.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_cwmc
if errorlevel 1 goto batcherror
move Debug\poly_cwmc.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_h2ni
if errorlevel 1 goto batcherror
move Debug\poly_h2ni.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_h3
if errorlevel 1 goto batcherror
move Debug\poly_h3.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_hni
if errorlevel 1 goto batcherror
move Debug\poly_hni.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_ho2
if errorlevel 1 goto batcherror
move Debug\poly_ho2.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_nh3
if errorlevel 1 goto batcherror
move Debug\poly_nh3.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_oh3
if errorlevel 1 goto batcherror
move Debug\poly_oh3.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror
cd ..\poly_ohcl
if errorlevel 1 goto batcherror
move Debug\poly_ohcl.exe .
if errorlevel 1 goto batcherror
rd /S /Q Debug
if errorlevel 1 goto batcherror

:end




:batcherror
echo ERROR Cleaning up Debug directories and moving executables
echo Finish compiling all executables and/or clean up objects
echo manually
pause
:end



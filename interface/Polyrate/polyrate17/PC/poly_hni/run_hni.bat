@echo off
set CHEM=hni
set NUM=1

echo COPYING %CHEM% FILES
copy ..\..\testrun\%CHEM%\*.dat .
copy ..\..\testrun\%CHEM%\*.f* .
copy ..\..\poten\pothni.dat .

copy %CHEM%tr%NUM%.dat   poly.fu5
copy %CHEM%tr%NUM%.fu29  poly.fu29
echo RUNNING %CHEM%tr%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%tr%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%tr%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%tr%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%tr%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%tr%NUM%.fu61

echo DONE RUNNING %CHEM% TEST RUNS




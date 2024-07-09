@echo off
set CHEM=cmc
set NUM=1

echo COPYING %CHEM% FILES
copy ..\..\testrun\%CHEM%\*.dat .
copy ..\..\testrun\%CHEM%\*.f* .
copy ..\..\poten\potcmc1.dat .
copy ..\..\poten\potcmc2.dat .

copy %CHEM%tr%NUM%.dat   poly.fu5
echo RUNNING %CHEM%tr%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%tr%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%tr%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%tr%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%tr%NUM%.fu15
if exist poly.fu50 move poly.fu50 %CHEM%tr%NUM%.fu50
if exist poly.fu51 move poly.fu51 %CHEM%tr%NUM%.fu51
if exist esp.fu61  move esp.fu61  %CHEM%tr%NUM%.fu61

set NUM=2
copy %CHEM%tr%NUM%.dat   poly.fu5
copy cmcic.dat poly.fu50
echo RUNNING %CHEM%tr%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%tr%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%tr%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%tr%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%tr%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%tr%NUM%.fu29
if exist poly.fu50 move poly.fu50 %CHEM%tr%NUM%.fu50
if exist poly.fu51 move poly.fu51 %CHEM%tr%NUM%.fu51
if exist esp.fu61  move esp.fu61  %CHEM%tr%NUM%.fu61


set NUM=3
copy %CHEM%tr%NUM%.dat   poly.fu5
copy %CHEM%tr%NUM%.fu51  poly.fu51

echo RUNNING %CHEM%tr%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%tr%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%tr%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%tr%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%tr%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%tr%NUM%.fu29
if exist poly.fu51 move poly.fu51 %CHEM%tr%NUM%.fu51
if exist esp.fu61  move esp.fu61  %CHEM%tr%NUM%.fu61

echo DONE RUNNING %CHEM% TEST RUNS




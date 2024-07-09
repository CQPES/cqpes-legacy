@echo off
set CHEM=ch5
copy ..\..\testrun\%CHEM%\*.dat .
copy ..\..\testrun\%CHEM%\*.f* .




set NUM=j1tr8
copy ..\..\poten\potch5j1.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu22 move poly.fu22 %CHEM%%NUM%.fu22
if exist poly.fu49 move poly.fu49 %CHEM%%NUM%.fu49
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat


set NUM=j1tr9
copy ..\..\poten\potch5j1.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat


set NUM=j1tr10
copy ..\..\poten\potch5j1.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat



set NUM=j1tr11
copy ..\..\poten\potch5j1.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat



set NUM=j1tr12
copy ..\..\poten\potch5j1.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat



set NUM=j2itr1
copy ..\..\poten\potch5j2.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat


set NUM=j2itr2
copy ..\..\poten\potch5j2.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat

set NUM=j2tr1
copy ..\..\poten\potch5j2.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat

set NUM=j2tr2
copy ..\..\poten\potch5j2.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat

set NUM=j2tr3
copy ..\..\poten\potch5j2.dat potch5.dat
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61
del potch5.dat



echo DONE RUNNING %CHEM% TEST RUNS




@echo off
set CHEM=oh3


echo COPYING %CHEM% FILES
copy ..\..\testrun\%CHEM%\*.dat .
copy ..\..\testrun\%CHEM%\*.f* .


set NUM=fu30tr1
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu30  poly.fu30
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61


set NUM=fu30tr2
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu30  poly.fu30
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu30tr3
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu30  poly.fu30
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu30tr4
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu30  poly.fu30
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu30tr5
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu30  poly.fu30
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu30tr6
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu30  poly.fu30
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu31tr1
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu31  poly.fu31
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu31tr2
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu31  poly.fu31
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu40tr1
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu40  poly.fu40
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu40tr2
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu40  poly.fu40
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61


set NUM=fu40tr3
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu40  poly.fu40
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu40tr4
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu40  poly.fu40
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu40tr5
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu40  poly.fu40
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=fu40tr6
copy %CHEM%%NUM%.dat   poly.fu5
copy %CHEM%%NUM%.fu40  poly.fu40
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr1
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr2
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr3
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr4
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr5
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr6
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr7
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr8
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr9
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

set NUM=tr10
copy %CHEM%%NUM%.dat   poly.fu5
echo RUNNING %CHEM%%NUM% TEST RUN
poly_%CHEM%
echo CLEANING UP FILES
if exist poly.fu5  move poly.fu5  %CHEM%%NUM%.fu5
if exist poly.fu6  move poly.fu6  %CHEM%%NUM%.fu6
if exist poly.fu14 move poly.fu14 %CHEM%%NUM%.fu14
if exist poly.fu15 move poly.fu15 %CHEM%%NUM%.fu15
if exist poly.fu29 move poly.fu29 %CHEM%%NUM%.fu29
if exist esp.fu61  move esp.fu61  %CHEM%%NUM%.fu61

echo DONE RUNNING %CHEM% TEST RUNS




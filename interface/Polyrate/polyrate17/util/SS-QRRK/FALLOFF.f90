program falloff
!Dec. 22, 2018, Junwei Lucas Bao, and Donald G. Truhlar
!University of Minnesota, Twin Cities
implicit none
integer :: number_Trxn, number_Pressure
character(4) ::  betaclogic 
real*8 :: avgEtrans,navgEtrans, FE
real*8 :: sigma_Pstar, epsilo_Pstar, MW_Pstar, sigma_bath, epsilo_bath, MW_bath
real*8, dimension(:), allocatable :: Trxn(:), Pressure(:)
character(15) :: unit_Pressure, method_collision, unit_avgEtrans, method_falloff
character(15) :: string
character(3) :: switch_rxn2  
integer :: q1, q2
integer :: modified_opt
integer :: num_rxn2 
integer :: num_rxnPHLQRRK 
character(5) :: option_rxn1  
integer :: switch_HLQRRK  
real*8, dimension(:), allocatable :: A_rxn1(:), Ea_rxn1(:), kT_rxn1(:)
real*8, dimension(:,:), allocatable :: kT_rxn1PH(:,:), A_rxn1PH(:,:), Ea_rxn1PH(:,:)
integer, dimension(:), allocatable :: i_lowest(:) 
real*8 :: s_Pstar, s_Astar, geomean_freq, MaxE_QRRK
real*8, dimension(:), allocatable :: A_rxnneg1(:), Ea_rxnneg1(:), A_rxn2(:),Ea_rxn2(:)
real*8, dimension(:,:), allocatable :: A_rxn2p(:,:),Ea_rxn2p(:,:)
real*8, dimension(:), allocatable :: Ecrit_rxn2p(:), summationp(:)
real*8, dimension(:), allocatable :: RxnDE(:) 
real*8 :: ZPE_TS1, ZPE_TS2, ZPE_stab, qbarrier_rxnneg1, qbarrier_rxn2
real*8 :: MaxE_print, ZeroE_print1, ZeroE_print2, StepE_print
integer :: GammaE_type1, GammaE_type2  
real*8 :: para1, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11, para12,&
& para01, para02, para03, para04, para05, para06, para07, para08, para09, para010, para011, para012 
common para1, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11, para12, &
& para01, para02, para03, para04, para05, para06, para07, para08, para09, para010, para011, para012
real*8 :: MaxE_stab, StepE_stab
real*8, dimension(:), allocatable :: E_stab(:), DOS_stab(:)
real*8 :: MaxE_TS1, StepE_TS1
real*8, dimension(:), allocatable :: E_TS1(:), N_TS1(:)
real*8 :: MaxE_TS2, StepE_TS2
real*8, dimension(:), allocatable :: E_TS2(:), N_TS2(:)
real*8, parameter :: PI=3.14159265359d0
real*8, parameter :: R=0.0019872d0  
real*8, parameter :: kBstd=1.38064852d-23 
real*8, parameter :: kB=1.98709369d-3 
real*8, parameter :: h=1.51982986082d-16 
real*8, parameter :: autokcal=627.509d0
character(20) :: title, keyword, tempstring
real*8, parameter ::  min_stepE=1.d-2, max_stepE=1.0
real*8 :: safevalue_maxE, T_max 
real*8, parameter :: safevalue_maxEQRRK=3500.
integer :: step, step1, step2, step3, step4, step5, step6, step7, step8, step9, step10, special_opt
real*8 :: E_hv 
integer :: number_Estab, number_ETS1
real*8, dimension(:,:), allocatable :: c_bath(:,:) 
real*8, dimension(:,:), allocatable :: k_coll(:,:), k_coll0(:,:)
real*8 :: betac
real*8, dimension(:), allocatable :: betac_value(:)  
real*8 :: k_HS
real*8 :: alphaa
real*8 :: FE_computed, FE_computed2, FE_computed3
real*8 ::  maxEFE3
integer :: FE_logic
real*8 :: FE_ini
real*8 :: Ecrit_rxnneg1, Ecrit_rxn2
real*8:: ZPE_WR 
real*8, dimension(:), allocatable:: vibfreq_WR(:) 
integer :: num_tot_line 
integer :: status, system

real*8 :: ini_qrrk, final_qrrk
real*8 :: tempnum, summation, summation1, summation2, summation3, summation4, summation5, summation6
real*8, dimension(:,:), allocatable ::  k_stab(:,:), k_diss(:,:), ratio_falloff(:,:), ratio_diss(:,:), summation7(:)
real*8, dimension(:,:,:), allocatable :: k_stabPH(:,:,:), ratio_falloffPH(:,:,:)
real*8, dimension(:), allocatable :: k_neg1(:), k_rxn2(:)
real*8, dimension(:,:), allocatable :: k_rxn2p(:,:)
real*8, dimension(:,:,:), allocatable :: k_dissp(:,:,:), ratio_dissp(:,:,:)
real*8 :: E_ind, E_ind2  
real*8 :: k_i 
real*8 :: Dis_E   

real*8, dimension(:), allocatable :: k_rxn1(:), k_rxn1_mod(:),summation8(:) 
real*8, dimension(:,:), allocatable :: k_rxn1PH(:,:), k_rxn1PH_mod(:,:)
real*8, dimension(:,:,:), allocatable :: k_energ(:,:,:)
real*8 :: K_E
real*8, dimension(:), allocatable :: GammaErxnneg1(:), GammaErxn2(:)
real*8 :: assign_GammaE1, assign_GammaE2
real*8 :: GammaE_value
real*8, dimension(:), allocatable :: beta(:)  !=1/RT in kcal/mol
real*8, dimension(:), allocatable ::  Q_stab(:), summation0(:) 
real*8, dimension(:,:), allocatable :: P_stab(:,:)
integer :: stepcount0, stepcount1, stepcount2, stepcount3, stepcount4
real*8, dimension(:,:), allocatable :: g_Dis(:,:)
real*8, dimension(:), allocatable :: summation01(:) 
real*8, dimension(:), allocatable :: k_muVTrxnneg1(:), k_muVTrxn2(:)

integer :: betacorrtype

character(30) :: picword  
character(10) :: date, time, date2, time2
!----------Initialzation----------------------
special_opt=0
switch_HLQRRK=0
betacorrtype=0
num_rxnPHLQRRK=0
modified_opt=0
!---------------------------------------------

write(*,*) 'FALLOFF PROGRAM START'
call date_and_time(date,time)
date2=date(5:6)//"/"//date(7:8)//"/"//date(1:4)
time2=time(1:2)//":"//time(3:4)//":"//time(5:6)
write(*,*)'Calculation run on ',date2,' at ', time2
write(*,*) '     '
!------------Start Read input file: falloff.inp-------------------------------------------
write(*,*) 'START reading falloff.inp ...'
open (unit=20, file='falloff.inp', status='old', action='read')
read(20,*) title
if (title(1:10)=='#BasicInfo') then
write(*,*) '  Reading #BasicInfo'
else if (title(1:11)=='#intNexp-bE') then
  write(*,*) 'Using special utililty: int(Nexp(-betaE)dE)'
  special_opt=1
  read(20,*) keyword, number_Trxn
  read(20,*) keyword
  if (keyword(1:8)=='Trxnlist') then
  write(*,*) '  Reading Trxn list'
  else
  write(*,*) 'Illegal keyword: ', keyword
  stop
  endif
  allocate (Trxn(number_Trxn))
  do step=1, number_Trxn
  read(20,*) Trxn(step)
  enddo
  close(20)
  goto 1001
!
else
write(*,*) 'Illegal section: ', title
stop
endif 
read(20,*) keyword, number_Trxn
read(20,*) keyword
if (keyword(1:8)=='Trxnlist') then
write(*,*) '  Reading Trxn list'
else
write(*,*) 'Illegal keyword: ', keyword
stop
endif
allocate (Trxn(number_Trxn))
do step=1, number_Trxn
read(20,*) Trxn(step)
enddo
read(20,*) keyword, number_Pressure
if (keyword(1:5)=='numbP') then
continue
else
write(*,*) 'Illegal keyword: ', keyword
stop
endif
read(20,*) keyword, unit_Pressure
if (keyword(1:5)=='unitP') then
continue
else
write(*,*) 'Illegal keyword: ', keyword
stop
endif
read(20,*) keyword
if (keyword(1:5)=='Plist') then
write(*,*) '  Reading P list'
else
write(*,*) 'Illegal keyword: ', keyword
stop
endif
allocate (Pressure(number_Pressure))
do step=1, number_Pressure
read(20,*) Pressure(step)
enddo
!Convert input pressure to Pa
!torr, bar, Pa, atm, kPa, lg10atm, lg10bar, lg10torr  are allowed unit for input pressures
if (unit_Pressure(1:4)=='torr') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)*133.322368d0
enddo
else if (unit_Pressure(1:3)=='bar') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)*1.d5
enddo
else if (unit_Pressure(1:3)=='atm') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)*1.01325d5
enddo
else if (unit_Pressure(1:2)=='Pa') then
continue
else if (unit_Pressure(1:3)=='kPa') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)*1.d3
enddo
else if (unit_Pressure(1:7)=='lg10atm') then
do step=1, number_Pressure
 Pressure(step)=10.**(Pressure(step))*1.01325d5
enddo
else if (unit_Pressure(1:7)=='lg10bar') then
do step=1, number_Pressure
 Pressure(step)=10.**(Pressure(step))*1.0d5
enddo
else if (unit_Pressure(1:7)=='lg10torr') then
do step=1, number_Pressure
 Pressure(step)=10.**(Pressure(step))*133.322368d0
enddo
else
write(*,*) 'Illegal unit of input pressures: ', unit_Pressure
stop
endif 
!
read(20,*) title
if (title(1:10)=='#Collision') then
write(*,*) '  Reading #Collision'
else
write(*,*) 'Illegal section: ', title
stop
endif
read(20,*) keyword, method_collision
if ((method_collision(1:4)=='Troe').or.(method_collision(1:4)=='Reid').or.&
& (method_collision(1:5)=='Forst')) then
write(*,*) '  Reading Collision method: ', method_collision
else
write(*,*) 'Unrecongnized method of collision: ', method_collision
write(*,*) 'Use Troe as default for method of collision!'
method_collision='Troe'
endif

read(20,*) keyword, avgEtrans, unit_avgEtrans, navgEtrans, betaclogic, string
if (string(1:2)=='FE') then
  backspace(20)
else
  if ((string(1:13)=='GLTcorrection').or.(string(1:13)=='gltcorrection')) then
    betacorrtype = 1 
  else
    write(*,*) 'Unrecognized keyword:', string
    stop
  endif 
endif
if ((betaclogic(1:3)=='all').or.(betaclogic(1:3)=='All')) then
write(*,*) '  Reading |<deltaE>|'
else if ((betaclogic(1:4)=='down').or.(betaclogic(1:4)=='Down')) then
write(*,*) '  Reading <deltaE>down'
else
write(*,*) 'You need to speficy <deltaE> with "all" or "down"!'
stop
endif
!Convert avgEtrans (cm**-1, cal*mol-1, kcal*mol-1)  to kcal/mol
if (unit_avgEtrans(1:10)=='kcal*mol-1') then
continue
else if (unit_avgEtrans(1:9)=='cal*mol-1') then
avgEtrans=avgEtrans*1.d-3
else if ((unit_avgEtrans(1:6)=='cm**-1').or.(unit_avgEtrans(1:4)=='cm-1')) then
avgEtrans=avgEtrans*2.85911d-3
else
write(*,*) 'Illegal Unit of avgEtrans: ', unit_avgEtrans
stop
end if

read(20,*) keyword, FE
if ((FE==0).or.(FE==0.0).or.(FE==0.)) then
write(*,*) '  FE will be computed by code from DOS'
write(*,*) '  (integrate DOS using Troe original FE definition)'
FE_logic=1
  status=system("ls DOS_stab.inp")
  if (status .ne. 0 ) then
  write(*,*) 'In order to compute FE via DOS, DOS_stab.inp is needed!'
  stop
  endif
else if ((FE==-1).or.(FE==-1.).or.(FE==-2).or.(FE==-2.)) then
write(*,*) '  FE will be computed by code from W-R approx.'
  if ((FE==-1).or.(FE==-1.)) &
  &write(*,*) '  (Using Troe analytical FE with W-R DOS)'
  if ((FE==-2).or.(FE==-2.)) &
  &write(*,*) '  (Doing numerical integration of FE with W-R DOS)'
FE_ini=FE
FE_logic=2
  status=system("ls vibinfo.inp")
  if (status .ne. 0 ) then
  write(*,*) 'In order to compute FE via W-R approximation, vibinfo.inp is needed!'
  stop
  endif
else
FE_logic=0  !User has provided a specific value for FE
write(*,100) FE
100 format ('   User input FE:', F5.2)
endif

if (betacorrtype==1) then
  if (FE_logic==0) write(*,*) '  G-L-T correction is ignored when user specifies a specific FE'
  if (FE_logic==1) write(*,*) '  Doing G-L-T correction for beta_c with user provided DOS.'
  if (FE_logic==2) write(*,*) '  Doing G-L-T correction for beta_c with W-R DOS'
else
endif


read(20,*) keyword, sigma_Pstar, epsilo_Pstar, MW_Pstar
if ((keyword(1:10)=='paramPstar').or.(keyword(1:7)=='paramAB')) then
write(*,*) '  Reading collision parameters for P*'
else if (keyword(1:6)=='paramA') then
write(*,*) '  Reading collision parameters for A' !for HLQRRK calc.
!The variables' names in the code are still sigma_Pstar, epsilo_Pstar, MW_Pstar
else
write(*,*) 'Illegal keyword: ', keyword
stop
endif
read(20,*) keyword, sigma_bath, epsilo_bath, MW_bath
if (keyword(1:9)=='paramBath') then
write(*,*) '  Reading collision parameters for bath gas'
else
write(*,*) 'Illegal keyword: ', keyword
stop
endif

read(20,*) title
if (title(1:14)=='#FalloffMethod') then
write(*,*) '  Reading #FalloffMethod'
else
write(*,*) 'Illegal section: ', title
stop
endif
read(20,*) keyword, method_falloff
if ((method_falloff(1:4)=='QRRK').or.(method_falloff(1:4)=='RRKM')) then
write(*,*) '  Reading falloff method: ', method_falloff
else if ((method_falloff(1:6)=='HLQRRK').or.(method_falloff(1:7)=='PHLQRRK')&
&.or.(method_falloff(1:9)=='PBOHLQRRK')) then
  if (method_falloff(1:6)=='HLQRRK') then
  write(*,*) '  Reading falloff method (Hinshelwood-Lindemann): ', method_falloff
  read(20,*) tempstring
  if (tempstring(1:8).eq.'modified') then
     backspace(20)
     read(20,*) tempstring, modified_opt
     if (modified_opt.eq.1) then
        write(*,*) '   Use modified HLQRRK: Approximately considering reverse rxn:' 
        write(*,*) '   (Require input ZPE-included reaction energy (kcal/mol) )'
        write(*,*) '   M. C. Lin and K. J. Laidler, Trans. Faraday Soc. 64, 94 (1968)'
        write(*,*) '   This is ONLY suitable for single isomerization reaction! NOT for'
        write(*,*) '   dissociating to two or more products!' 
     else
        write(*,*) 'Illegal value of modified:' , modified_opt
        stop
     endif
  else
     backspace(20)
  endif
  else if ((method_falloff(1:7)=='PHLQRRK').or.&
&(method_falloff(1:9)=='PBOHLQRRK'))  then
  write(*,*) '  Reading falloff method (Parallel Hinshelwood-Lindemann): ', method_falloff
  backspace(20)
  read(20,*) tempstring, tempstring, tempstring
    if ((tempstring(1:4)=='rxn1').or.(tempstring(1:8)=='modified')) then
       write(*,*) "You need to specify number of parallel rxns after ",&
       &method_falloff, "!"
       stop
    else
      backspace(20)
      read(20,*) tempstring, tempstring, num_rxnPHLQRRK
      write(*,*) '  Number of parallel reactions: ', num_rxnPHLQRRK
    endif  
  read(20,*) tempstring
  if (tempstring(1:8).eq.'modified') then
     if (method_falloff(1:9)=='PBOHLQRRK') then
        write(*,*) 'PBOHLQRRK method cannot use "modified" keyword!'
        stop
     endif
     backspace(20)
     read(20,*) tempstring, modified_opt
     if (modified_opt.eq.1) then
        allocate (RxnDE(num_rxnPHLQRRK))
        write(*,*) '   Use modified PHLQRRK: Approximately considering reverse rxn:'
        write(*,*) '   (Require input ZPE-included reaction energy (kcal/mol) for each rxn)'
        write(*,*) '   M. C. Lin and K. J. Laidler, Trans. Faraday Soc. 64, 94 (1968)'
        write(*,*) '   This is ONLY suitable for single isomerization reaction! NOT for'
        write(*,*) '   dissociating to two or more products!'
        if (num_rxnPHLQRRK>1) then
           write(*,*) '   '
           write(*,*) 'Warning! You have multiple parallel reactions, do you still insist using'
           write(*,*) 'the modified version which is ONLY applicable for SINGLE isomerization reaction?'
           write(*,*) '(yes/no)'
           read(*,*) string
           if ((string(1:3)=='yes').or.(string(1:3)=='YES').or.(string(1:3)=='Yes')) then
             write(*,*) 'Fine...'
           else
               write(*,*) 'Terminate... '
               stop
           endif 
        endif
     else
        write(*,*) 'Illegal value of modified:' , modified_opt
        stop
     endif
  else
     backspace(20)
  endif
  endif
switch_HLQRRK=1
else
write(*,*) 'Illegal method for falloff: ', method_falloff
stop
endif


!--Temp implementation limitation--
!#parallel reactions allowed for PBOHLQRRK is 2.
if (method_falloff(1:9)=='PBOHLQRRK') then
  if (num_rxnPHLQRRK.ne.2)  then
     write(*,*) 'The current implementation only allows'
     write(*,*) 'two parallel reactions for PBOHLQRRK!'
     stop
  endif
endif
!----------------------------------------


if (switch_HLQRRK==1) then
switch_rxn2(1:2)='no'
num_rxn2 = 0
!for HLQRRK or PHLQRRK calculation, rxn2 is not allowed
goto 123
endif

num_rxn2 = 0 !initial value
read(20,*) keyword, switch_rxn2
!if ((switch_rxn2(1:3)=='YES').or.(switch_rxn2(1:2)=='NO') &
!& .or.(switch_rxn2(1:3)=='yes').or.(switch_rxn2(1:2)=='no')) then
!continue
if ((switch_rxn2(1:3)=='YES').or.(switch_rxn2(1:3)=='yes')) then
  num_rxn2 = 1
else if ((switch_rxn2(1:3)=='NO').or.(switch_rxn2(1:3)=='no')) then
  num_rxn2 = 0
else
backspace(20)
read(20,*) keyword, num_rxn2 !read in number of parallel rxns in 2nd step
  if (num_rxn2 == 0) then
    switch_rxn2 = 'NO'
  !"Rxn2 NO" is equivalent to "Rxn2 0"
  else if (num_rxn2 >= 1) then
    if (method_falloff(1:4)=='QRRK') then
      switch_rxn2 = 'YES'
      !"Rxn2 YES" is equivalent to "Rxn2 1"
    else if (method_falloff(1:4)=='RRKM') then
      switch_rxn2 = 'YES'
      if (num_rxn2 > 1) then
        write(*,*) 'Currently, RRKM can only have 1 reaction for the 2nd step!'
        stop
      endif
    endif
  endif
!write(*,*) 'Illegal rxn 2 swith keyword: ', switch_rxn2
!stop
endif

123 if ((method_falloff(1:7).ne.'PHLQRRK').and.&
&(method_falloff(1:9).ne.'PBOHLQRRK')) then
  read(20,*) keyword, option_rxn1
  if (switch_HLQRRK==1) then
    if ((option_rxn1(1:4)=='rate').or.(option_rxn1(1:4)=='Rate').or.(option_rxn1(1:4)=='RATE')) then
      write(*,*) 'For HLQRRK, rxn1 with RATE option is NOT allowed!'
      write(*,*) 'Only rxn1 with AEa option is allowed!!'
      stop
    endif
  endif
  if ((option_rxn1(1:4)=='rate').or.(option_rxn1(1:4)=='Rate').or.(option_rxn1(1:4)=='RATE')) then
    allocate (kT_rxn1(number_Trxn))
    write(*,*) '  Reading rxn 1 k(T) '
    do step=1, number_Trxn
      read(20,*) kT_rxn1(step)
    enddo
  else if ((option_rxn1(1:3)=='AEa').or.(option_rxn1(1:3)=='AEA').or.(option_rxn1(1:3)=='aea')) then
    allocate (kT_rxn1(number_Trxn), A_rxn1(number_Trxn), Ea_rxn1(number_Trxn))
    write(*,*) '  Reading rxn 1 A and Ea'
    do step=1, number_Trxn
      read(20,*) A_rxn1(step), Ea_rxn1(step)
      kT_rxn1(step)=A_rxn1(step)*exp(-Ea_rxn1(step)/(R*Trxn(step)))
    enddo
  else
    write(*,*) 'Illegal specification of rxn 1 rate const.: ', option_rxn1
    stop
  endif
  if ((modified_opt.eq.1).and.(method_falloff(1:6)=='HLQRRK')) then
     allocate (RxnDE(1))
     read(20,*)  tempstring, RxnDE(1)
     if ((tempstring(1:5).ne.'RxnDE').and.(tempstring(1:5).ne.'rxndE')) then
         write(*,*) 'At the end of the rate parameter section (AEa or rate),'
         write(*,*) 'Please input ZPE-included reaction energy (kcal/mol),'
         write(*,*) 'starting with the keyword: "RxnDE"!'
         stop
     endif
  endif
elseif ((method_falloff(1:7).eq.'PHLQRRK').or.&
&(method_falloff(1:9).eq.'PBOHLQRRK')) then
!HLQRRK with 1st being parallel rxns: A->P1, A->P2, ..., A->Pn
  allocate (kT_rxn1PH(num_rxnPHLQRRK,number_Trxn),&
  &A_rxn1PH(num_rxnPHLQRRK,number_Trxn),Ea_rxn1PH(num_rxnPHLQRRK,number_Trxn))
  allocate (i_lowest(number_Trxn))
  tempnum=1.d20
  do step1=1, num_rxnPHLQRRK
    read(20,*) keyword, option_rxn1
    if ((option_rxn1(1:4)=='rate')&
&.or.(option_rxn1(1:4)=='Rate').or.(option_rxn1(1:4)=='RATE')) then
      write(*,*) 'For PHLQRRK or PBOHLQRRK, rxn1 with RATE option is NOT allowed!'
      write(*,*) 'Only rxn1 with AEa option is allowed!!'
      stop
    endif
    if ((option_rxn1(1:3)=='AEa').or.(option_rxn1(1:3)=='AEA').or.(option_rxn1(1:3)=='aea')) then
      write(*,*) '  Reading rxn 1 A and Ea for parallel rxn', step1
      do step2=1, number_Trxn
        read(20,*) A_rxn1PH(step1,step2), Ea_rxn1PH(step1,step2)
        kT_rxn1PH(step1,step2)=A_rxn1PH(step1,step2)*exp(-Ea_rxn1PH(step1,step2)/(R*Trxn(step2)))
      enddo
    else
      write(*,*) 'Illegal specification of rxn 1 rate const.: ', option_rxn1
      stop
    endif
     if (modified_opt.eq.1) then
       read(20,*)  tempstring, RxnDE(step1)
       if ((tempstring(1:5).ne.'RxnDE').and.(tempstring(1:5).ne.'rxndE')) then
         write(*,*) 'At the end of the rate parameter section (AEa or rate) for each rxn,'
         write(*,*) 'Please input ZPE-included reaction energy (kcal/mol) for this rxn,'
         write(*,*) 'starting with the keyword: "RxnDE"!'
         stop
       endif
     endif
  enddo
  i_lowest(:)=1 !initial value
  do step2=1, number_Trxn
    do step1=1, num_rxnPHLQRRK
       if (Ea_rxn1PH(step1,step2)<tempnum) then
         tempnum=Ea_rxn1PH(step1,step2)
         i_lowest(step2)=step1
       endif
    enddo
  enddo
endif


if (switch_HLQRRK==1) then
  read(20,*) keyword, s_Astar
  if (keyword(1:6)=='sAstar') then
    continue
  else
    write(*,*) 'sAstar is needed!'
    stop
  endif
  read(20,*) keyword, geomean_freq
  if (keyword(1:9)=='GeoMeFreq') then
    E_hv=geomean_freq*0.00285911
    write(*,101) E_hv
  101 format ('   Stepsize of QRRK summation: unit quantum E(hv)= ', F6.2, 'kcal/mol')
  else
    write(*,*) 'GeoMeFreq is needed!'
    stop
  endif
  read(20,*) keyword, MaxE_QRRK
  if (keyword(1:4)=='MaxE') then
    write(*,102) MaxE_QRRK
  102 format ('   Energy upper limit of QRRK summation: E(+infinity)= ', F7.2,'kcal/mol')
  else
    write(*,*) 'MaxE in #QRRKData is needed!'
    stop
  endif
  if (MaxE_QRRK>=safevalue_maxEQRRK) then
    continue
  else
    write(*,*) 'Please set MaxE for QRRK bigger than ', safevalue_maxEQRRK, &
  & 'kcal/mol, in order to converge the summation of QRRK!'
    stop
  endif
  close(20)
  goto  124
endif




if (method_falloff(1:4)=='QRRK')  then
  read(20,*) title
  if (title(1:9)=='#QRRKData') then
  write(*,*) '  Reading #QRRKData'
  else
  write(*,*) 'Illegal section: ', title, ' Section #QRRKData is needed!'
  stop
  endif
  read(20,*) keyword, s_Pstar
  if ((keyword(1:6)=='sPstar').or.(keyword(1:7)=='sABstar')) then
  continue
  else
  write(*,*) 'sPstar is needed!'
  stop
  endif
  read(20,*) keyword, geomean_freq
  if (keyword(1:9)=='GeoMeFreq') then
  E_hv=geomean_freq*0.00285911
  write(*,101) E_hv
  else
  write(*,*) 'GeoMeFreq is needed!'
  stop
  endif
  read(20,*) keyword, MaxE_QRRK
  if (keyword(1:4)=='MaxE') then
  write(*,102) MaxE_QRRK
  else
  write(*,*) 'MaxE in #QRRKData is needed!'
  stop
  endif
  if (MaxE_QRRK>=safevalue_maxEQRRK) then
  continue
  else
  write(*,*) 'Please set MaxE for QRRK bigger than ', safevalue_maxEQRRK, &
& 'kcal/mol, in order to converge the summation of QRRK!'
  stop
  endif
  read(20,*) keyword
  if (keyword(1:10)=='rxnneg1AEa') then
  write(*,*) '  Reading A and Ea of rxn -1'
  else
  write(*,*) 'rxnneg1AEa is needed!'
  stop
  endif
  allocate (A_rxnneg1(number_Trxn), Ea_rxnneg1(number_Trxn))
  do step=1, number_Trxn
  read(20,*) A_rxnneg1(step), Ea_rxnneg1(step)
  enddo
  if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then
    close(20)
  else if ((switch_rxn2(1:3)=='YES').or.(switch_rxn2(1:3)=='yes')) then
    if (num_rxn2 == 1) then 
      read(20,*) keyword
      if (keyword(1:7)=='rxn2AEa') then
        write(*,*) '  Reading A and Ea of rxn 2'
        allocate (A_rxn2(number_Trxn),Ea_rxn2(number_Trxn))
        do step=1, number_Trxn
        read(20,*) A_rxn2(step),Ea_rxn2(step)
        enddo
        close(20)
      else
        write(*,*) 'rxn2AEa is needed!'
        stop
      endif
    else if  (num_rxn2 > 1) then
      allocate (A_rxn2p(num_rxn2,number_Trxn),Ea_rxn2p(num_rxn2,number_Trxn))
      do step=1, num_rxn2
        read(20,*) keyword
        if (keyword(1:7)=='rxn2AEa') then
          write(*,*) '  Reading A and Ea of rxn 2: parallel rxn', step
          do step2=1, number_Trxn
            read(20,*) A_rxn2p(step,step2),Ea_rxn2p(step,step2)
          enddo
        else
          write(*,*) 'rxn2AEa for parallel rxn',step,' is needed!'
          stop
        endif  
      enddo   
      close(20)
    endif
  else
    write(*,*) 'Wrong switch of rxn 2 (YES/NO)!'
    stop
  endif  
else if (method_falloff(1:4)=='RRKM')  then  
  step1=0 
  do      
    read(20,*) tempstring
    step1=step1+1
    if (step1>=80000) then 
    write(*,*) 'You need #RRKMData section! (or too many loops)'
    stop
    else 
    continue
    endif
    if (tempstring(1:9)=='#RRKMData') then
    exit
    else
    endif
  enddo
  write(*,*) '  Skip #QRRKData section'
  write(*,*) '  Reading #RRKMData'
  read(20,*) keyword, ZPE_TS1
  if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then
   read(20,*) keyword
   if (keyword(1:8)=='ZPEofTS2') then
   continue
   else
   write(*,*) 'You still need to put ZPEofTS2 keyword even though information of rxn 2 is not needed!'
   stop
   endif
  else
   read(20,*) keyword, ZPE_TS2 
  endif
  read(20,*) keyword, ZPE_stab 
  read(20,*) keyword, qbarrier_rxnneg1
  if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then
   read(20,*) keyword
   if (keyword(1:9)=='qbarrier2') then
   continue
   else
   write(*,*) 'You still need to put qbarrier2 keyword even though information of rxn 2 is not needed!'
   stop
   endif
  else
    read(20,*) keyword, qbarrier_rxn2
  endif
  read(20,*) keyword, MaxE_print
  read(20,*) keyword, ZeroE_print1
  if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then
   read(20,*) keyword
   if (keyword(1:11)=='ZeroPrintE2') then
   continue
   else
   write(*,*) 'You still need to put ZeroPrintE2 keyword even though information of rxn 2 is not needed!'
   stop
   endif
  else
    read(20,*) keyword, ZeroE_print2
  endif
  read(20,*) keyword, StepE_print
  read(20,*) keyword, GammaE_type1
  write(*,*)  '  Reading Gamma(E) parameters of rxn 1'
  if (GammaE_type1==0) then
  write(*,*) 'VTST is NOT considered for rxn -1.'
  else if (GammaE_type1==1) then
  read(20,*)  para1, para2, para3, para4, para5
  else if (GammaE_type1==2) then
  read(20,*)  para1, para2, para3, para4
  else if (GammaE_type1==3) then
  read(20,*) para1, para2, para3, para4, para5, para6, para7, para8, para9, para10
  else if (GammaE_type1==4) then
  read(20,*) para1, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11
  else if (GammaE_type1==5) then
  read(20,*) para1, para2, para3, para4, para5, para6
  else
  write(*,*) 'Cannot recongnize functional form for Gamma(E) of rxn -1!'
  stop
  endif 
  if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then
   close(20)
  else
    read(20,*) keyword, GammaE_type2
    if (keyword(1:15)=='GammaEfuncType2') then
      continue
    else
      write(*,*) 'GammaEfuncType2 is needed!'
      stop
    endif
    write(*,*)  '  Reading Gamma(E) parameters of rxn 2'
    if (GammaE_type2==0) then
    write(*,*) 'VTST is NOT considered for rxn 2.'
    else if (GammaE_type2==1) then
    read(20,*)  para01, para02, para03, para04, para05
    else if (GammaE_type2==2) then
    read(20,*)  para01, para02, para03, para04
    else if (GammaE_type2==3) then
    read(20,*) para01, para02, para03, para04, para05, para06, para07, para08, para09, para010
    else if (GammaE_type2==4) then
    read(20,*) para01, para02, para03, para04, para05, para06, para07, para08, para09, para010, para011
    else if (GammaE_type2==5) then
    read(20,*) para01, para02, para03, para04, para05, para06
    else
    write(*,*) 'Cannot recongnize functional form for Gamma(E) of rxn 2!'
    stop
    endif
  close(20)
  endif 
else
  write(*,*) 'Illegal method of falloff!'
  close(20)
  stop
endif
!------------END Read input file: falloff.inp-----------------------------------------
write(*,*) 'END reading falloff.inp'


!determine safevalue_maxE for RRKM:
T_max=maxval(Trxn(:))
if (T_max <= 400.) then
safevalue_maxE=200.
else if (T_max <= 1000. ) then
safevalue_maxE=400.
else 
safevalue_maxE=500.
endif

124 if ((method_falloff(1:4)=='RRKM').or.(FE==0).or.(FE==0.0).or.(FE==0.)) then 
  write(*,*) 'START reading DOS_stab.inp ...'
  open (unit=21, file='DOS_stab.inp', status='old', action='read')
  read(21,*)  MaxE_stab, StepE_stab
  if (MaxE_stab<safevalue_maxE) then
  write(*,103) safevalue_maxE 
103 format ('MaxE of stab in DOS_stab.inp should be at least larger than ', F7.2, ' kcal/mol')
  stop
  else
  endif
  if ((StepE_stab .ne. min_stepE)) then
  write(*,104) StepE_stab, min_stepE
104 format ('Inappropriate stepsize ', F5.2, ' in DOS_stab.inp! Stepsize should be', F5.2, ' kcal/mol')
  stop
  else
  endif
  number_Estab=MaxE_stab/StepE_stab  !number of data points needs to be read
  allocate(E_stab(number_Estab), DOS_stab(number_Estab))
  do step=1, number_Estab
  read(21,*) E_stab(step), DOS_stab(step)
  enddo
  write(*,*) 'END reading DOS_stab.inp'
  close(21) 
else
endif
if (method_falloff(1:4)=='RRKM') then
  write(*,*) 'START reading N_TS1.inp ...'
  open (unit=22, file='N_TS1.inp', status='old', action='read')
  read(22,*)  MaxE_TS1, StepE_TS1
  if ((MaxE_TS1.ne.MaxE_stab).or.(StepE_TS1.ne.StepE_stab)) then
  write(*,*) 'Max E or stepsize in DOS_stb and N_TS1.inp should be the same!'
  stop
  else
  endif  
  allocate(E_TS1(number_Estab), N_TS1(number_Estab))  
  do step=1, number_Estab
  read(22,*) E_TS1(step), N_TS1(step)
  enddo 
  write(*,*) 'END reading N_TS1.inp'
  close(22)
else
endif
if ((method_falloff(1:4)=='RRKM').and.((switch_rxn2(1:3)=='YES').or.(switch_rxn2(1:3)=='yes'))) then
  write(*,*) 'START reading N_TS2.inp ...'
  open (unit=23, file='N_TS2.inp', status='old', action='read')
  read(23,*)  MaxE_TS2, StepE_TS2
  if ((MaxE_TS2.ne.MaxE_stab).or.(StepE_TS2.ne.StepE_stab)) then
  write(*,*) 'Max E or stepsize in DOS_stb and N_TS2.inp should be the same!'
  stop
  else
  endif
  allocate(E_TS2(number_Estab), N_TS2(number_Estab))
  do step=1, number_Estab
  read(23,*) E_TS2(step), N_TS2(step)
  enddo
  write(*,*) 'END reading N_TS2.inp'
  close(23)
else
endif
1001 if (special_opt==1) then
  write(*,*) 'START reading N_TS1.inp ...'
  open (unit=22, file='N_TS1.inp', status='old', action='read')
  read(22,*)  MaxE_TS1, StepE_TS1
  if (MaxE_TS1<safevalue_maxE) then
  write(*,103) safevalue_maxE
  stop
  else
  endif
  number_ETS1=MaxE_TS1/StepE_TS1
  allocate(E_TS1(number_ETS1), N_TS1(number_ETS1))
  do step=1, number_ETS1
  read(22,*) E_TS1(step), N_TS1(step)
  enddo
  write(*,*) 'END reading N_TS1.inp'
  close(22)
  goto 1002
else
endif
1002 if (special_opt==1) then
allocate(summation7(number_Trxn))
summation7(:)=0.
do step9=1, number_Trxn
  do step10=1, number_ETS1
  summation7(step9)=summation7(step9)+N_TS1(step10)*&
  &exp(-E_TS1(step10)/(Trxn(step9)*8.314d0*1.d-3/4.184d0))*StepE_TS1
  enddo
enddo
open (unit=31, file='intNexp_bE.txt', status='unknown', action='write')
write(31,*) 'T/K      int(Nexp(-betaE)dE)/Hartree'
do step9=1, number_Trxn
write(31,300)  Trxn(step9), summation7(step9)
enddo
close(31)
goto 1003
else
endif

write(*,*) '                             '
write(*,*) 'START collision calculation...'
if ((FE==-1).or.(FE==-1.)&
&.or.(FE==-2).or.(FE==-2.)) then
    write(*,*) '  Reading vibinfo.inp for Whitten-Rabinovitch FE'
    call readfile(num_tot_line)
    allocate (vibfreq_WR(num_tot_line-1)) !number of vibrational modes=num_tot_line-1
    open (unit=24, file='vibinfo.inp', status='old', action='read')
    read(24,*) ZPE_WR
    do step=1, num_tot_line-1
    read(24,*) vibfreq_WR(step)
    enddo
    close(24)
else
endif

 open (unit=32, file='betac.txt', status='unknown', action='write')
 allocate (c_bath(number_Trxn, number_Pressure), k_coll(number_Trxn,&
&number_Pressure), betac_value(number_Trxn),k_coll0(number_Trxn,number_Pressure))
do step1=1, number_Trxn
  if ((FE==0).or.(FE==0.00).or.(FE==0.).or.(FE==0.0)) then 
    if (method_falloff(1:4)=='QRRK') then
    Ecrit_rxnneg1=Ea_rxnneg1(step1)
    else if (method_falloff(1:6)=='HLQRRK') then
    Ecrit_rxnneg1=Ea_rxn1(step1) 
    else if ((method_falloff(1:7)=='PHLQRRK').or.&
    &(method_falloff(1:9)=='PBOHLQRRK')) then
    Ecrit_rxnneg1=Ea_rxn1PH(i_lowest(step1),step1)
    else  
    Ecrit_rxnneg1=qbarrier_rxnneg1
    endif
    FE=FE_computed(Trxn(step1),Ecrit_rxnneg1) 
  else if ((FE==-1).or.(FE==-1.)) then
    if (method_falloff(1:4)=='QRRK') then
    Ecrit_rxnneg1=Ea_rxnneg1(step1)
    else if (method_falloff(1:6)=='HLQRRK') then
    Ecrit_rxnneg1=Ea_rxn1(step1) 
    else if ((method_falloff(1:7)=='PHLQRRK').or.&
    &(method_falloff(1:9)=='PBOHLQRRK')) then
    Ecrit_rxnneg1=(Ea_rxn1PH(i_lowest(step1),step1))
    else  
    Ecrit_rxnneg1=qbarrier_rxnneg1
    endif
    maxEFE3=MaxE_QRRK
    FE=FE_computed2(Trxn(step1),Ecrit_rxnneg1, ZPE_WR, vibfreq_WR, num_tot_line)
  else if ((FE==-2).or.(FE==-2.)) then
    if (method_falloff(1:4)=='QRRK') then
    Ecrit_rxnneg1=Ea_rxnneg1(step1)
    maxEFE3=MaxE_QRRK
    else if (method_falloff(1:6)=='HLQRRK') then
    Ecrit_rxnneg1=Ea_rxn1(step1) !Variable name Ecrit_rxnneg1 is still used
    maxEFE3=MaxE_QRRK
    else if ((method_falloff(1:7)=='PHLQRRK').or.&
    &(method_falloff(1:9)=='PBOHLQRRK')) then
    Ecrit_rxnneg1=(Ea_rxn1PH(i_lowest(step1),step1))
    maxEFE3=MaxE_QRRK
    else  
    Ecrit_rxnneg1=qbarrier_rxnneg1
    maxEFE3=MaxE_stab
    endif
    FE=FE_computed3(Trxn(step1),Ecrit_rxnneg1, ZPE_WR, vibfreq_WR,num_tot_line,maxEFE3)
  else
  endif
  betac_value(step1)=betac(Trxn(step1), FE,&
  &avgEtrans*(Trxn(step1)/300.)**navgEtrans, betaclogic, Ecrit_rxnneg1,&
  &betacorrtype, FE_logic, ZPE_WR, vibfreq_WR,num_tot_line,maxEFE3)
  write(32,*) Trxn(step1), betac_value(step1)
  do step2=1, number_Pressure
  c_bath(step1,step2)=(Pressure(step2)/(kBstd*Trxn(step1)))*1.d-6  !molecule/cm**3
  k_coll0(step1,step2)=betac_value(step1)*k_HS(Trxn(step1),sigma_Pstar, &
& epsilo_Pstar, MW_Pstar, sigma_bath, epsilo_bath, MW_bath,&
&method_collision)   
  k_coll(step1,step2)=k_coll0(step1,step2)*c_bath(step1,step2)
  enddo
  if (FE_logic==1) then
  FE=0
  else if (FE_logic==2) then
  FE=FE_ini
  else
  endif
enddo
write(*,*) 'END collision calculation'
close(32, status='delete')
if (method_falloff(1:6)=='HLQRRK') then
write(*,*) 'START HLQRRK calculations...'
open (unit=29, file='QRRK_kE.txt', status='unknown', action='write')
write(29,*) 'HL-QRRK Calculation run on ',date2,' at ', time2
allocate (k_rxn1(number_Trxn),k_rxn1_mod(number_Trxn), summation8(number_Trxn))
write(29,*) 'Hinshelwood-Lindemann Theory:'
write(29,*) 'A+M->A*+M  energization (k_energ)'
write(29,*) 'A*+M->A+M  de-energization (k_deenerg)'
if (modified_opt.eq.1) then
   write(29,*) 'A*<->P*->P  rxn 1' !modified HLQRRK 
else
   write(29,*) 'A*->P  rxn 1'  !Original HLQRRK
endif
write(29,*) '              '
write(29,*) '         '
write(29,*) 'High-p limit krxn1(T)/s**-1:'
write(29,*) 'High-p limit krxn1(T)_fromQRRK=sum[K(E)*krxn1(E)]'
write(29,*) ' T/K      krxn1(T)_fromInput  krxn1(T)_fromQRRK'
     do step1=1, number_Trxn
       summation2=0. !initialize
        ini_qrrk=Ea_rxn1(step1)
        final_qrrk=MaxE_QRRK
        do E_ind=ini_qrrk, final_qrrk, E_hv
        summation2=summation2+k_i(Trxn(step1),geomean_freq,s_Astar,A_rxn1(step1),Ea_rxn1(step1),E_ind)*K_E(Trxn(step1),geomean_freq,s_Astar,E_ind)
!        write(29,300) Trxn(step1), E_ind, k_rxn1(step1)
        end do
       write(29,303) Trxn(step1), kT_rxn1(step1), summation2
303  format (F7.2,ES15.4, ES20.4)
     enddo
     write(29,*) '                '
     write(29,*) 'Microcanonical rate const. for rxn 1:'
     write(29,*) ' T/K  E+/(kcal/mol,E+=E-Ea(T))   log10krxn1(E)/s**-1'
     do step1=1, number_Trxn
        ini_qrrk=Ea_rxn1(step1)
        final_qrrk=Ea_rxn1(step1)+100. 
        do E_ind=ini_qrrk, final_qrrk, E_hv
        write(29,303) Trxn(step1), E_ind-ini_qrrk, log10(k_i(Trxn(step1),geomean_freq,s_Astar,A_rxn1(step1),Ea_rxn1(step1),E_ind))
        end do
     enddo
allocate (k_stab(number_Trxn,number_Pressure),ratio_falloff(number_Trxn,number_Pressure))
     do step1=1, number_Trxn
       summation2=0. 
       do step2=1, number_Pressure
        ini_qrrk=Ea_rxn1(step1)
        final_qrrk=MaxE_QRRK
        do E_ind=ini_qrrk, final_qrrk, E_hv
        k_rxn1(step1)=k_i(Trxn(step1),geomean_freq,s_Astar,A_rxn1(step1),Ea_rxn1(step1),E_ind)
        if (modified_opt.eq.1) then
           k_rxn1_mod(step1)=k_i(Trxn(step1),geomean_freq,s_Astar,A_rxn1(step1),Ea_rxn1(step1)-RxnDE(1),E_ind) 
        else
           k_rxn1_mod(step1)=0.d0
        endif
        summation2=summation2+k_rxn1(step1)*K_E(Trxn(step1),geomean_freq,s_Astar,E_ind)&
        &/(1.+(k_rxn1(step1)+k_rxn1_mod(step1))/k_coll(step1,step2))
        !write(99,*) K_E(Trxn(step1),geomean_freq,s_Astar,E_ind),k_coll(step1,step2)!debug
        end do
        k_stab(step1,step2)=summation2
        ratio_falloff(step1,step2)=summation2/kT_rxn1(step1)
        summation2=0. !initialize summation2
       enddo
     enddo
close(29)
goto  127
endif
if ((method_falloff(1:7)=='PHLQRRK').or.&
&(method_falloff(1:9)=='PBOHLQRRK')) then
  if (method_falloff(1:7)=='PHLQRRK') &
  &  write(*,*) 'START PHLQRRK calculations...'
  if (method_falloff(1:9)=='PBOHLQRRK') &
  & write(*,*) 'START PBOHLQRRK calculations...'
open (unit=29, file='QRRK_kE.txt', status='unknown', action='write')
  if (method_falloff(1:7)=='PHLQRRK')&
  & write(29,*) 'P-HL-QRRK Calculation run on ',date2,' at ', time2
  if (method_falloff(1:9)=='PBOHLQRRK')&
  & write(29,*) 'P-BOHL-QRRK Calculation run on ',date2,' at ', time2
  allocate (k_rxn1(number_Trxn),summation8(number_Trxn))
  if (method_falloff(1:7)=='PHLQRRK') then
    write(29,*) 'Hinshelwood-Lindemann Theory with parallel reactions:'
    write(29,*) 'A+M->A*+M  energization (k_energ)'
    write(29,*) 'A*+M->A+M  de-energization (k_deenerg)'
    if (modified_opt.eq.1) then
      write(29,*) 'A*<->P1*->P1  rxn 1'
      write(29,*) 'A*<->P2*->P2  rxn 2'
      write(29,*) '......'
      write(29,*) 'A*<->Pi*->Pi  rxn i'
    else  !regular (approximated) treatment
      write(29,*) 'A*->P1  rxn 1'
      write(29,*) 'A*->P2  rxn 2'
      write(29,*) '......'
      write(29,*) 'A*->Pi  rxn i'
    endif
    write(29,*) '              '
  elseif  ((method_falloff(1:9)=='PBOHLQRRK').and.(num_rxnPHLQRRK.eq.2)) then
    write(29,*) 'B-O Hinshelwood-Lindemann Theory with 2 parallel reactions:'
    write(29,*) 'A+M->A*+M  energization (k_energ)'
    write(29,*) 'A*+M->A+M  de-energization (k_deenerg)'
    write(29,*) 'A*+M->A**+M  energization (k_energ2)'
    write(29,*) 'A**+M->A*+M  de-energization (k_deenerg2)'
    write(29,*) 'A*->P1  rxn 1 (with lowest activation energy)'
    write(29,*) 'A**->P1  rxn 1 at higher energy'
    write(29,*) 'A**->P2  rxn 2 (with higher activation energy)'
    write(29,*) '              '
  endif
do step=1, num_rxnPHLQRRK
write(29,*) '         '
write(29,*) 'High-p limit krxni(T)/s**-1 for rxn', step
write(29,*) 'High-p limit krxni(T)_fromQRRK=sum[K(E)*krxni(E)]'
write(29,*) ' T/K      krxn1(T)_fromInput  krxn1(T)_fromQRRK'
     do step1=1, number_Trxn
        summation2=0. !initialize 
        ini_qrrk=Ea_rxn1PH(step,step1)
        final_qrrk=MaxE_QRRK
        do E_ind=ini_qrrk, final_qrrk, E_hv
        summation2=summation2+k_i(Trxn(step1),geomean_freq,s_Astar,A_rxn1PH(step,step1),&
        &Ea_rxn1PH(step,step1),E_ind)*K_E(Trxn(step1),geomean_freq,s_Astar,E_ind)
        end do
        write(29,303) Trxn(step1), kT_rxn1PH(step,step1), summation2
     enddo
     write(29,*) '                '
     write(29,*) 'Microcanonical rate const. for rxn:', step
     write(29,*) ' T/K  E+/(kcal/mol,E+=E-Ea(T))   log10krxni(E)/s**-1'
     do step1=1, number_Trxn
        ini_qrrk=Ea_rxn1PH(step,step1)
        final_qrrk=Ea_rxn1PH(step,step1)+100.
        do E_ind=ini_qrrk, final_qrrk, E_hv
        write(29,303) Trxn(step1), E_ind-ini_qrrk,log10(k_i(Trxn(step1),&
        &geomean_freq,s_Astar,A_rxn1PH(step,step1),Ea_rxn1PH(step,step1),E_ind))
        end do
     enddo
enddo
allocate(k_stabPH(num_rxnPHLQRRK,number_Trxn,number_Pressure),&
&ratio_falloffPH(num_rxnPHLQRRK,number_Trxn,number_Pressure))
allocate(k_rxn1PH(num_rxnPHLQRRK,number_Trxn),k_rxn1PH_mod(num_rxnPHLQRRK,number_Trxn))

if (method_falloff(1:7)=='PHLQRRK') then
  do step=1, num_rxnPHLQRRK
       do step1=1, number_Trxn
         summation2=0. !initialize summation2
         summation3=0.
         do step2=1, number_Pressure
           ini_qrrk=Ea_rxn1PH(step,step1)
           final_qrrk=MaxE_QRRK
           do E_ind=ini_qrrk, final_qrrk, E_hv
             do step3=1, num_rxnPHLQRRK
               k_rxn1PH(step3,step1)=k_i(Trxn(step1),geomean_freq,s_Astar,A_rxn1PH(step3,step1),Ea_rxn1PH(step3,step1),E_ind)
               if (modified_opt.eq.1) then 
               !this modification is not suitable for multiple parallel reactions; it is only approximately applicable
               !at high-pressure range.
                  k_rxn1PH_mod(step3,step1)&
                  &=k_i(Trxn(step1),geomean_freq,s_Astar,A_rxn1PH(step3,step1),Ea_rxn1PH(step3,step1)-RxnDE(step3),E_ind) 
               else
                   k_rxn1PH_mod(step3,step1)=0.d0
               endif
               summation3=summation3+k_rxn1PH(step3,step1)+k_rxn1PH_mod(step3,step1)
               !directly including k_rxn1PH_mod in the denominator is only a very rough treatment 
               !for the case with multiple parallel isomerization reactions
             enddo
             summation2=summation2+k_rxn1PH(step,step1)&
             &*K_E(Trxn(step1),geomean_freq,s_Astar,E_ind)/(1.+summation3/k_coll(step1,step2)) 
             !write(99,*) K_E(Trxn(step1),geomean_freq,s_Astar,E_ind),k_coll(step1,step2)!debug
             summation3=0.
           enddo
           k_stabPH(step,step1,step2)=summation2
           ratio_falloffPH(step,step1,step2)=summation2/kT_rxn1PH(step,step1)
           summation2=0. !initialize summation2
           summation3=0.
         enddo
       enddo
  enddo
elseif  ((method_falloff(1:9)=='PBOHLQRRK').and.(num_rxnPHLQRRK.eq.2)) then
write(*,*) 'PBOHLQRRK is under development... It is not ready to use yet'
stop
endif
close(29)
goto  127
endif
if (method_falloff(1:4)=='QRRK') then
  write(*,*) 'START QRRK calculations...'
  open (unit=29, file='QRRK_kE.txt', status='unknown', action='write')
  write(29,*) 'Calculation run on ',date2,' at ', time2
  allocate (k_neg1(number_Trxn),k_stab(number_Trxn,number_Pressure), &
  & ratio_falloff(number_Trxn,number_Pressure), k_rxn2(number_Trxn),&
  & k_diss(number_Trxn,number_Pressure), ratio_diss(number_Trxn,number_Pressure),summation8(number_Trxn))
  if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then 
     write(29,*) 'Trxn/K       E+/(kcal/mol,E+=E-Ea(T))   log10krxn-1(E)/s**-1'
     do step1=1, number_Trxn
       summation2=0. !initialize summation2
       Ecrit_rxnneg1=Ea_rxnneg1(step1)
       do step2=1, number_Pressure
        ini_qrrk=Ecrit_rxnneg1
        final_qrrk=MaxE_QRRK
        do E_ind=ini_qrrk, final_qrrk, E_hv
        k_neg1(step1)=k_i(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind)
        summation2=summation2+k_coll(step1,step2)&
        &*Dis_E(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind, MaxE_QRRK)/(k_coll(step1,step2)+k_neg1(step1))
        write(29,300) Trxn(step1), E_ind-ini_qrrk, log10(k_neg1(step1))
300     format (F7.2,ES15.4, F16.5)
        end do
        k_stab(step1,step2)=kT_rxn1(step1)*summation2
        ratio_falloff(step1,step2)=summation2
        summation2=0. !initialize summation2
       enddo
     enddo
    
     write(29,*) '      '
     write(29,*) 'Summation(Dis_E(E)) over E (Normalization check):'
     do step1=1, number_Trxn
     summation2=0. 
     do E_ind=ini_qrrk, final_qrrk, E_hv
     summation2=summation2+Dis_E(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind,MaxE_QRRK)
     enddo
     write( 29, 300) Trxn(step1), summation2
     enddo

  else  !rnx 2 is involved; zero of energy = ZPE of stab_P
    if (num_rxn2 == 1) then
       write(29,*)  '                    '
       write(29,*) 'Trxn/K E+/(kcal/mol,E+=E-Ea(rxn-1)(T))   log10krxn-1(E)    log10krxn2(E)/s**-1'    
       do step1=1, number_Trxn
         summation3=0. !initialize summation3
         summation4=0.
         Ecrit_rxnneg1=Ea_rxnneg1(step1)
         Ecrit_rxn2=Ea_rxn2(step1)
         do step2=1, number_Pressure
            ini_qrrk=Ecrit_rxnneg1
            final_qrrk=MaxE_QRRK
            do E_ind=ini_qrrk, final_qrrk, E_hv
            k_neg1(step1)=k_i(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind)
            k_rxn2(step1)=k_i(Trxn(step1),geomean_freq,s_Pstar,A_rxn2(step1),Ecrit_rxn2,E_ind)
            if (isnan(k_rxn2(step1))==.true.) then
            k_rxn2(step1)=k_i(Trxn(step1),geomean_freq,s_Pstar,A_rxn2(step1),Ecrit_rxn2,E_ind+0.5*E_hv)
            else
            endif
            summation3=summation3+k_coll(step1,step2)&
            &*Dis_E(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind,MaxE_QRRK)&
            &/(k_coll(step1,step2)+k_neg1(step1)+k_rxn2(step1))
            summation4=summation4+k_rxn2(step1)&
            &*Dis_E(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind,MaxE_QRRK)&
            &/(k_coll(step1,step2)+k_neg1(step1)+k_rxn2(step1))
            write(29,301) Trxn(step1), E_ind-ini_qrrk, log10(k_neg1(step1)), log10(k_rxn2(step1))
301         format (F7.2,ES17.4, F25.5, F31.5)
            end do
            k_stab(step1,step2)=kT_rxn1(step1)*summation3
            k_diss(step1,step2)=kT_rxn1(step1)*summation4
            ratio_falloff(step1,step2)=summation3
            ratio_diss(step1,step2)=summation4
            summation3=0. !initialize summation3
            summation4=0.
         enddo
       enddo
    else if (num_rxn2 > 1) then
      allocate(summationp(num_rxn2),Ecrit_rxn2p(num_rxn2),k_rxn2p(num_rxn2,number_Trxn),k_dissp(num_rxn2,number_Trxn,number_Pressure)) 
      allocate(ratio_dissp(num_rxn2,number_Trxn,number_Pressure))
      write(29,*)  '                    '
      do step1=1, number_Trxn
         summation3=0.
         summation4=0.
         summationp=0. !initialization
         Ecrit_rxnneg1=Ea_rxnneg1(step1)
         do step3=1, number_Pressure
            ini_qrrk=Ecrit_rxnneg1
            final_qrrk=MaxE_QRRK
            do E_ind=ini_qrrk, final_qrrk, E_hv
              k_neg1(step1)=k_i(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind)
              do step2=1, num_rxn2
                Ecrit_rxn2p(step2)=Ea_rxn2p(step2,step1)
                k_rxn2p(step2,step1)=k_i(Trxn(step1),geomean_freq,s_Pstar,A_rxn2p(step2,step1),Ecrit_rxn2p(step2),E_ind)
                if (isnan(k_rxn2p(step2,step1))==.true.) then
                  k_rxn2p(step2,step1)=k_i(Trxn(step1),geomean_freq,s_Pstar,A_rxn2p(step2,step1),Ecrit_rxn2p(step2),E_ind+0.5*E_hv)
                else
                endif
                summation4=summation4+k_rxn2p(step2,step1)
              enddo 
              summation3=summation3+k_coll(step1,step3)&
              &*Dis_E(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind,MaxE_QRRK)&
              &/(k_coll(step1,step3)+k_neg1(step1)+summation4)
              do step2=1, num_rxn2
                summationp(step2)=summationp(step2)+k_rxn2p(step2,step1)&
                &*Dis_E(Trxn(step1),geomean_freq,s_Pstar,A_rxnneg1(step1),Ecrit_rxnneg1,E_ind,MaxE_QRRK)&
                &/(k_coll(step1,step3)+k_neg1(step1)+summation4)
              enddo
            summation4=0.
            enddo            
            k_stab(step1,step3)=kT_rxn1(step1)*summation3
            ratio_falloff(step1,step3)=summation3  
            do step2=1, num_rxn2
              k_dissp(step2,step1,step3)=kT_rxn1(step1)*summationp(step2)
              ratio_dissp(step2,step1,step3)=summationp(step2)
            enddo
            summation3=0.
            summation4=0.
            summationp=0.
         enddo 
      enddo
    endif
  endif    
  close(29)
  write(*,*) 'END QRRK calculations.'
else
endif
if (method_falloff(1:4)=='RRKM') then
  write(*,*) 'START RRKM calculations...'
  allocate(beta(number_Trxn), P_stab(number_Trxn, number_Estab), GammaErxnneg1(number_Estab), &
& Q_stab(number_Trxn), summation0(number_Trxn),k_neg1(number_Estab))
  allocate (k_stab(number_Trxn,number_Pressure), &
  & ratio_falloff(number_Trxn,number_Pressure), &
  & k_diss(number_Trxn,number_Pressure),ratio_diss(number_Trxn,number_Pressure))

  open (unit=28, file='RRKM_kE.txt', status='unknown', action='write')
  write(28,*) 'Calculation run on ',date2,' at ', time2
  write(28,*) 'Ro-vibrational partition function of stab computed from DOS:'
  write(28,*) 'T/K   Q_stab(T)(w/re ZPE_stab)     Q_stab(T)(w/re min of stab)'
    do step=1, number_Estab
    GammaErxnneg1(step)=assign_GammaE1(GammaE_type1, E_stab(step))
    enddo
    do step2=1, number_Trxn
      Q_stab(step2)=0. !Initialize Q_stab
      beta(step2)=1.d0/(Trxn(step2)*8.314d0*1.d-3/4.184d0) !=1/RT in kcal/mol 
      do step=1, number_Estab
        Q_stab(step2)=DOS_stab(step)*(1./autokcal)*exp(-beta(step2)*E_stab(step))*StepE_stab+Q_stab(step2)
      enddo
    write(28,194) Trxn(step2), Q_stab(step2), Q_stab(step2)*exp(-beta(step2)*ZPE_stab)
194 format  (F7.2, 2ES16.4) 
    enddo
   !Compute P_stab
   write(28,*) '                 '
   write(28,*) 'T/K   E/(kcal/mol,w/re ZPE of stab)    P_stab(E)/Hartree**-1  int(P_stab(E),E)'
   do step2=1, number_Trxn
     summation0(step2)=0. !for nomarlization check of P_stab(E)
     do step=1, number_Estab
     P_stab(step2,step)=DOS_stab(step)*(1./autokcal)*exp(-beta(step2)*E_stab(step))/Q_stab(step2) 
     summation0(step2)=summation0(step2)+P_stab(step2,step)*StepE_stab
     enddo
   enddo
   do step2=1, number_Trxn
   stepcount2=0
   stepcount2=stepcount2+1
   stepcount0=0
   do step=1, number_Estab
     stepcount0=stepcount0+1
    if (stepcount2==1) then
     if ((stepcount0==int(StepE_print/StepE_stab)+1).or.(stepcount0==1)) then
       if (E_stab(step)-qbarrier_rxnneg1<=MaxE_print) then
          write(28,193) Trxn(step2), E_stab(step), P_stab(step2,step), summation0(step2)
193       format (F7.2, F12.2, ES30.4, F25.4)
          stepcount0=1
       else
       endif
     else
     endif
    else
    endif
   enddo
   enddo
    write(28,*) '              '
    write(28,191) ZeroE_print1
191 format ('E/(kcal/mol,w/re ZeroPrintE1:',F7.2,' )  Gamma(E)  log10krxn-1(E)/s**-1' )   
    stepcount0=0
    do step=1, number_Estab
      stepcount0=stepcount0+1
      if (E_stab(step)<qbarrier_rxnneg1) then
        k_neg1(step)=0.
        GammaE_value=0
        stepcount0=0
      else
        k_neg1(step)=GammaErxnneg1(step-int(qbarrier_rxnneg1/StepE_stab)+1)*N_TS1(int((E_stab(step)-qbarrier_rxnneg1)/StepE_stab)+1)/(h*DOS_stab(step))
        GammaE_value=GammaErxnneg1(step-int(qbarrier_rxnneg1/StepE_stab)+1)
        if ((stepcount0==int(StepE_print/StepE_stab)+1).or.(stepcount0==1)) then
          if (E_stab(step)-qbarrier_rxnneg1<=MaxE_print) then
            write(28,192) E_stab(step)-ZeroE_print1, GammaE_value, log10(k_neg1(step))
192         format (F10.2, F36.4, F15.5)
            stepcount0=1
          else
          endif
        else
        endif
      endif
    enddo

    write( 28, *) '           '
    write( 28, *) 'Canonical rate const. kmuVT(T) of rxn -1 (s**-1):'
    write( 28, *) ' T/K                   kmuVT(T) of rxn-1'
    allocate(k_muVTrxnneg1(number_Trxn))
    do step=1, number_Trxn
      k_muVTrxnneg1(step)=0. !initialization
      do step2=1, number_Estab
        k_muVTrxnneg1(step)=k_muVTrxnneg1(step)+P_stab(step,step2)*k_neg1(step2)*StepE_stab 
      enddo
      write( 28, 183) Trxn(step),  k_muVTrxnneg1(step)
183  format (F8.2, ES30.4)
    enddo


    write( 28, *) '   '
    write( 28, *) 'g(E) values:'
    write( 28, *) ' T/K     E/(kcal/mol)    g(E)/hartree**-1'
    allocate(g_Dis(number_Trxn, number_Estab),summation01(number_Trxn))
    do step2=1, number_Trxn
      summation01(step2)=0. !initialize
      do step=1, number_Estab !start from E=0.01 kcal/mol, which is consistent with the start of k-1(E); of course g(E)=0. for E<qbarrier
         summation01(step2)=summation01(step2)+k_neg1(step)*DOS_stab(step)*exp(-beta(step2)*E_stab(step))*StepE_stab
      enddo   
    enddo
    do step2=1, number_Trxn
       stepcount0=0
      do step=1, number_Estab   
       stepcount0=stepcount0+1 
        g_Dis(step2,step)=k_neg1(step)*DOS_stab(step)*exp(-beta(step2)*E_stab(step))/summation01(step2)
        if ((stepcount0==int(StepE_print/StepE_stab)+1).or.(stepcount0==1)) then
          if (E_stab(step)-qbarrier_rxnneg1<=MaxE_print) then
          write( 28, 184) Trxn(step2), E_stab(step), g_Dis(step2,step)
184       format (F8.2, F8.2, ES30.4)
          stepcount0=1
          else 
          endif
        else
        endif
      enddo
    enddo

    write( 28, *) '           '
    write( 28, *) 'int[g(E)dE] Normalization check:'
    do step2=1, number_Trxn
      summation01(step2)=0.
      do step=1, number_Estab
        summation01(step2)=summation01(step2)+g_Dis(step2,step)*StepE_stab
      enddo
    write( 28, 183) Trxn(step2),  summation01(step2)
    enddo 

  if ((switch_rxn2(1:3)=='YES').or.(switch_rxn2(1:3)=='yes')) then ! rxn 2 is involved
     allocate(k_rxn2(number_Estab), GammaErxn2(number_Estab))
     write(28,*) '              '
     write(28,189)  ZeroE_print2
189 format ('E/(kcal/mol,w/re ZeroPrintE2:',F7.2,' )  Gamma(E)  log10krxn2(E)/s**-1' )
     do step=1, number_Estab
     GammaErxn2(step)=assign_GammaE2(GammaE_type2, E_stab(step))
     enddo
     do step=1, number_Estab
       stepcount0=stepcount0+1
       if (E_stab(step)<qbarrier_rxn2) then
        k_rxn2(step)=0.
        GammaE_value=0
        stepcount0=0
       else
        k_rxn2(step)=GammaErxn2(step-int(qbarrier_rxn2/StepE_stab)+1)*N_TS2(int((E_stab(step)-qbarrier_rxn2)/StepE_stab)+1)/(h*DOS_stab(step))
        GammaE_value=GammaErxn2(step-int(qbarrier_rxn2/StepE_stab)+1)
        if ((stepcount0==int(StepE_print/StepE_stab)+1).or.(stepcount0==1)) then
          if (E_stab(step)-qbarrier_rxn2<=MaxE_print) then
            write(28,192) E_stab(step)-ZeroE_print2, GammaE_value, log10(k_rxn2(step))
            stepcount0=1
          else
          endif
        else
        endif
       endif
     enddo
     do step1=1, number_Trxn
       do step2=1, number_Pressure
         summation5=0. !initialize
         summation6=0.
         do step3=1, number_Estab !integral from 0.01, but for E<qbarrier, drop all these points ('cuz 0/0)
           if (g_Dis(step1,step3)==0.) then
           continue
           else
           summation5=summation5+k_coll(step1,step2)*g_Dis(step1,step3)*StepE_stab&
           &/(k_coll(step1,step2)+k_neg1(step3)+k_rxn2(step3))
           summation6=summation6+k_rxn2(step3)*g_Dis(step1,step3)*StepE_stab&
           &/(k_coll(step1,step2)+k_neg1(step3)+k_rxn2(step3))
           endif
         enddo
         k_stab(step1,step2)=kT_rxn1(step1)*summation5
         ratio_falloff(step1,step2)=summation5
         k_diss(step1,step2)=kT_rxn1(step1)*summation6
         ratio_diss(step1,step2)=summation6
       enddo
    enddo  

    write( 28, *) '           '
    write( 28, *) 'Canonical rate const. kmuVT(T) of rxn 2 (s**-1):'
    write( 28, *) ' T/K                   kmuVT(T) of rxn2'
    allocate(k_muVTrxn2(number_Trxn))
    do step=1, number_Trxn
      k_muVTrxn2(step)=0. !initialization
      do step2=1, number_Estab
        k_muVTrxn2(step)=k_muVTrxn2(step)+P_stab(step,step2)*k_rxn2(step2)*StepE_stab
      enddo
      write( 28, 183) Trxn(step),  k_muVTrxn2(step)
    enddo      

  else !rxn 2 is not involved
     !compute k_stab, k_diss, ratio_falloff, ratio_diss     
     do step1=1, number_Trxn
       do step2=1, number_Pressure
         summation5=0. !initialize
         do step3=1, number_Estab !integral from 0.01, but for E<qbarrier, drop all these points ('cuz 0/0)
           if (g_Dis(step1,step3)==0.) then
           continue
           else
           summation5=summation5+k_coll(step1,step2)*g_Dis(step1,step3)*StepE_stab&
           &/(k_coll(step1,step2)+k_neg1(step3))
           endif
         enddo
         k_stab(step1,step2)=kT_rxn1(step1)*summation5
         ratio_falloff(step1,step2)=summation5
       enddo
    enddo
  endif
  
  close(28)
  write(*,*) 'END RRKM calculations.'
else
endif

127   write(*,*) 'WRITE results to falloff.out'
open (unit=30, file='falloff.out', status='unknown', action='write')
write(30,*) '######FALLOFF PROGRAM#######'
write(30,*) 'Author: J. L. Bao, and D. G. Truhlar'
write(30,*) 'Calculation run on ',date2,' at ', time2
write(30,*) 'Result 1: collision k(T)(=betac*omiga*kHS*c_bath) '
write(30,*) 'T/K       Pressure/Pa       k_coll/s**-1'
do step3=1, number_Trxn
  do step4=1, number_Pressure
  write(30,198) Trxn(step3), Pressure(step4), k_coll(step3,step4)
198 format (F7.2, ES15.2, ES15.4)
  enddo
enddo

write(30,*) '                       '
write(30,*) 'Result 1.2: betac '
write(30,*) 'T/K           betac'
do step3=1, number_Trxn
  write(30,198) Trxn(step3), betac_value(step3) 
enddo



if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then 
  write(30,*) '                       '
  if (method_falloff(1:6)=='HLQRRK') then
    write(30,*) 'Result 2: kuni(T)'
    write(30,*) 'T/K       Pressure/Pa       kuni(T)/(s**-1)'
  elseif ((method_falloff(1:7)=='PHLQRRK').or.&
  &(method_falloff(1:9)=='PBOHLQRRK')) then
    write(30,*) 'Result 2: kuni(T)'
  else
    write(30,*) 'Result 2: kstab(T)'
    write(30,*) 'T/K       Pressure/Pa       kstab(T)/(cm**3/(molecule*s))'
  endif
  if ((method_falloff(1:7).ne.'PHLQRRK').and.&
  &(method_falloff(1:9).ne.'PBOHLQRRK')) then
  do step3=1, number_Trxn
    do step4=1, number_Pressure
      write(30,199) Trxn(step3), Pressure(step4), k_stab(step3,step4)
199   format (F7.2, ES15.2, 2ES19.4) 
    enddo
  enddo
  elseif ((method_falloff(1:7).eq.'PHLQRRK').or.&
  &(method_falloff(1:9)=='PBOHLQRRK')) then
    do step=1, num_rxnPHLQRRK
       write(30,*) 'k_uni for parallel rxn',step,'/(s**-1)'
       write(30,*) 'T/K       Pressure/Pa       kuni_i(T)/(s**-1)'
       do step3=1, number_Trxn
         do step4=1, number_Pressure
            write(30,199) Trxn(step3), Pressure(step4), k_stabPH(step,step3,step4)
         enddo
       enddo
    enddo
  endif
else
  write(30,*) '                       '
  if (num_rxn2 == 1) then
    if (method_falloff(1:6)=='HLQRRK') then
      write(30,*) 'Result 2: kstab(T) and kdiss(T) /(s**-1)'
    else
      write(30,*) 'Result 2: kstab(T) and krxn(T) /(cm**3/(molecule*s))'
    endif
    write(30,*) 'T/K       Pressure/Pa         kstab(T)           kdiss(T)'
    do step3=1, number_Trxn
      do step4=1, number_Pressure
        write(30,195) Trxn(step3), Pressure(step4), k_stab(step3,step4), k_diss(step3,step4)
195     format (F7.2, ES15.2, 2ES19.4)
      enddo
    enddo
  else if (num_rxn2 > 1) then
    write(30,*) 'T/K       Pressure/Pa         kstab(T)/(s**-1)'
    do step1=1, number_Trxn
      do step2=1, number_Pressure
        write(30,195) Trxn(step1), Pressure(step2), k_stab(step1,step2)
      enddo
    enddo
   do step1=1, num_rxn2
     write(30,*) '                      '   
     write(30,*) 'Step 2: k_diss for parallel rxn', step1,'/(cm**3/(molecule*s))'
     write(30,*) 'T/K       Pressure/Pa         krxn_i(T)'
     do step2=1, number_Trxn
       do step3=1, number_Pressure
         write(30,195) Trxn(step2), Pressure(step3), k_dissp(step1,step2,step3)
       enddo
     enddo
   enddo
  endif
endif

write(30,*) '                       '
write(30,*) 'Result 3: falloff'
!convert Pa in the code to the original unit in input
if (unit_Pressure(1:4)=='torr') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)/133.322368
enddo
else if (unit_Pressure(1:3)=='bar') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)/1.d5
enddo
else if (unit_Pressure(1:3)=='atm') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)/1.01325d5
enddo
else if (unit_Pressure(1:2)=='Pa') then
continue
else if (unit_Pressure(1:3)=='kPa') then
do step=1, number_Pressure
 Pressure(step)=Pressure(step)/1.d3
enddo
else if (unit_Pressure(1:7)=='lg10atm') then
do step=1, number_Pressure
 Pressure(step)=log10((Pressure(step))/1.01325d5)
enddo
else if (unit_Pressure(1:7)=='lg10bar') then
do step=1, number_Pressure
 Pressure(step)=log10(Pressure(step)/1.0d5)
enddo
else if (unit_Pressure(1:7)=='lg10torr') then
do step=1, number_Pressure
 Pressure(step)=log10(Pressure(step)/133.322368)
enddo
else
endif

if ((switch_rxn2(1:2)=='NO').or.(switch_rxn2(1:2)=='no')) then
  if (method_falloff(1:6)=='HLQRRK') then
    write(30,*) 'T/K       Pressure/', unit_Pressure,' log10(kuni/krxn1(T,HPLimit)'
  elseif ((method_falloff(1:7)=='PHLQRRK').or.&
  &(method_falloff(1:9)=='PBOHLQRRK')) then
    write(30,*) 'T/K       Pressure/', unit_Pressure,'log10(kuni_i/krxn_i(T,HPLimit)'
  else !chemical activation A+B->AB*->AB (without AB further dissociation)
    write(30,*) 'T/K       Pressure/', unit_Pressure,' log10(kstab/krxn1(T,HPLimit)'
  endif
  if ((method_falloff(1:7).ne.'PHLQRRK').and.&
  &(method_falloff(1:9).ne.'PBOHLQRRK')) then
  do step3=1, number_Trxn
    do step4=1, number_Pressure
      write(30,199) Trxn(step3), Pressure(step4), log10(ratio_falloff(step3,step4))
    enddo
  enddo
  elseif ((method_falloff(1:7)=='PHLQRRK').or.&
  &(method_falloff(1:9)=='PBOHLQRRK')) then
  do step=1, num_rxnPHLQRRK
    write(30,*) 'For parallel rxn',step,'/(s**-1)'
    write(30,*) 'T/K       Pressure/',unit_Pressure,'log10(kuni_i/krxn_i(T,HPLimit)'
    do step3=1, number_Trxn
      do step4=1, number_Pressure
         write(30,199) Trxn(step3), Pressure(step4),log10(ratio_falloffPH(step,step3,step4))
      enddo
    enddo
  enddo
  endif
else !this is for chemical activation A+B->AB*->AB, AB->P1, AB->P2, AB->P3, ...
  if (num_rxn2 == 1) then ! A+B->AB*->AB, AB->P1
    write(30,*) 'Col 1:T/K       Col 2: Pressure/', unit_Pressure
    write(30,*) 'Col 3: log10(kstab/krxn1(T,HPLimit)'
    write(30,*) 'Col 4: log10(krxn/krxn1(T,HPLimit))'
    write(30,*) '     '
    do step3=1, number_Trxn
      do step4=1, number_Pressure
        write(30,199) Trxn(step3), Pressure(step4), &
        & log10(ratio_falloff(step3,step4)), log10(ratio_diss(step3,step4))
      enddo
    enddo
  else if (num_rxn2 > 1) then  !A+B->AB*->AB, AB->P1, AB->P2, AB->P3, ...
    write(30,*) 'T/K       Pressure/',unit_Pressure,'log10(kstab/krxn1(T,HPLimit)'
    do step3=1, number_Trxn
      do step4=1, number_Pressure
        write(30,199) Trxn(step3), Pressure(step4), &
        & log10(ratio_falloff(step3,step4))
      enddo
    enddo
    do step1=1, num_rxn2
      write(30,*) '    '
      write(30,*) '2nd step: parallel rxn',step1,'log10(krxn/krxn1(T,HPLimit)'
      write(30,*) 'T/K       Pressure/',unit_Pressure,'log10(krxn_i/krxn1(T,HPLimit)'
      do step2=1, number_Trxn
        do step3=1, number_Pressure
          write(30,199) Trxn(step2), Pressure(step3), &
          & log10(ratio_dissp(step1,step2,step3))
        enddo
      enddo
    enddo
  endif
endif

write(*,*) '     '
write(30,*) '###END###'
close(30)
1003 write(*,*) '                   '
write(*,*) 'FALLOFF PROGRAM ENDING'
write(*,*) '---------------------'
write(*,*) '---END---'
end program falloff
!-------------------------------------------------------------------------
real*8 function betac(Trxn, FE, avgEtrans, &
&betaclogic,Ecrit_rxnneg1,betacorrtype,FE_logic,&
&ZPE_WR, vibfreq_WR,num_tot_line,maxEFE3) 
implicit none
real*8 :: Trxn, FE, avgEtrans !avgEtrans has been converted to kcal/mol
real*8 :: Ecrit_rxnneg1
real*8 :: righthand 
real*8 :: beta1, beta2
real*8, parameter :: kB=1.98709369d-3 !kcal/(mol*K)
character(4) :: betaclogic
integer :: betacorrtype, FE_logic
integer :: num_tot_line
real*8 :: ZPE_WR, maxEFE3
real*8 :: vibfreq_WR(num_tot_line-1)
real*8 :: delta
INTEGER, PARAMETER :: maxrecs = 1000000
INTEGER :: N, J, NR32, ios, num_tot_line32
CHARACTER(LEN=30) :: junk
real*8, allocatable :: X(:), Y(:)
real*8 :: XX, YY, XERR

if ((betaclogic(1:3)=='all').or.(betaclogic(1:3)=='All')) then
!here, avgEtrans is |<deltaE>all|
  righthand=abs(avgEtrans)/(FE*kB*Trxn)
  beta1=((-righthand+sqrt(righthand**2+4.d0*righthand))/2.d0)**2
  beta2=((-righthand-sqrt(righthand**2+4.d0*righthand))/2.d0)**2
  if ((beta1>=1.d0).and.(beta2<=1.d0)) then
  betac=beta2
  else if ((beta2>=1.d0).and.(beta1<=1.d0)) then
  betac=beta1
  else if ((beta2<=1.d0).and.(beta1<=1.d0)) then
  betac=min(beta1, beta2)
  else if ((beta2>=1.d0).and.(beta1>=1.d0)) then
  betac=1.d0
  endif
else if ((betaclogic(1:4)=='down').or.(betaclogic(1:4)=='Down')) then
!here avgEtrans is <deltaE>down
betac=(avgEtrans/(avgEtrans+FE*kB*Trxn))**2
endif

if (betacorrtype.eq.1) then
  betac = betac/delta(Trxn,FE, avgEtrans, betaclogic,Ecrit_rxnneg1,FE_logic,&
&ZPE_WR, vibfreq_WR,num_tot_line,maxEFE3)
else 
endif

if (betac.gt.1.) then
 NR32=0
 open (unit=33, file='betac.txt', status='old', action='read')
 DO J=1,maxrecs
   READ(33,*,IOSTAT=ios) junk
   IF (ios /= 0) EXIT
   IF (J == maxrecs) THEN
     write(*,*) "Error: Maximum number of records exceeded..."
     write(*,*) "Exiting program now..."
   STOP
   ENDIF
   NR32 = NR32 + 1
 ENDDO
 num_tot_line32=NR32
 allocate(X(num_tot_line32), Y(num_tot_line32))
 rewind(33)
 DO J=1,num_tot_line32
   read(33,*) X(J), Y(J) 
 enddo 
 close(33) 
 N = num_tot_line32
 XX = Trxn 
 CALL POLINT(X,Y,N,XX,YY,XERR) 
 betac = YY
endif
if (betac.gt.1.)  betac = 1.
end function betac
!---------------------------------------------------------------------------
real*8 function delta(Trxn, FE, avgEtrans, betaclogic,&
&Ecrit_rxnneg1,FE_logic,ZPE_WR, vibfreq_WR,num_tot_line,maxEFE3)
implicit none
real*8 :: Trxn, FE, avgEtrans !avgEtrans has been converted to kcal/mol
real*8 :: Ecrit_rxnneg1
integer :: betacorrtype, FE_logic
real*8, parameter :: kB=1.98709369d-3 !kcal/(mol*K)
real*8 :: a, q, DOSWR
character(4) ::  betaclogic
real*8 :: delta1, delta2, deltaN
real*8 :: node(20), weight(20), EG(20)
integer :: num_tot_line, i, j, k, block
real*8 :: ZPE_WR, maxEFE3
real*8 :: vibfreq_WR(num_tot_line-1)
real*8 :: summation, summation0
real*8 :: RT, Eblocku, Eblockl

RT = Trxn*8.314d0*1.d-3/4.184d0
summation = 0.
summation0 = 0.

node=(/-0.993128599185094924786,&
-0.963971927277913791268,&
-0.912234428251325905868,&
-0.8391169718222188233945,&
-0.7463319064601507926143,&
-0.6360536807265150254528,&
-0.5108670019508270980044,&
-0.3737060887154195606725,&
-0.2277858511416450780805,&
-0.0765265211334973337546,&
0.0765265211334973337546,&
0.22778585114164507808,&
0.373706088715419560673,&
0.510867001950827098004,&
0.6360536807265150254528,&
0.746331906460150792614,&
0.8391169718222188233945,&
0.9122344282513259058678,&
0.9639719272779137912677,&
0.9931285991850949247861/)
weight=(/0.017614007139152118312,&
0.040601429800386941331,&
0.0626720483341090635695,&
0.083276741576704748725,&
0.1019301198172404350368,&
0.1181945319615184173124,&
0.1316886384491766268985,&
0.1420961093183820513293,&
0.149172986472603746788,&
0.1527533871307258506981,&
0.152753387130725850698,&
0.1491729864726037467878,&
0.142096109318382051329,&
0.131688638449176626899,&
0.11819453196151841731,&
0.101930119817240435037,&
0.08327674157670474872,&
0.062672048334109063569,&
0.040601429800386941331,&
0.017614007139152118312/)

!To obtaine <deltaE>down
if ((betaclogic(1:3)=='all').or.(betaclogic(1:3)=='All')) then
  q = -abs(avgEtrans) 
  !the input abs(avgEtrans) is |<deltaE>all|, q is <deltaE>all
  a = 0.5d0*(-q + sqrt(q**2 - 4*q*FE*kB*Trxn)) ! a = <deltaE>down 
  if (a.lt.0.) stop  !debug
else
  a = avgEtrans !input is already <deltaE>down  
endif

if (FE_logic==2) then
  summation=0.
  summation0=0.
  block=int(abs(maxEFE3)/abs(Ecrit_rxnneg1))
  do k=1, block
     Eblocku=k*(maxEFE3)/block !upper limit of one interval
     Eblockl=(k-1)*(maxEFE3)/block !lower limit of one interval
     EG=(Eblockl+Eblocku)*0.5+(Eblocku-Eblockl)*node*0.5
   do j=1,20 
     summation=summation+weight(j)&
     &*DOSWR(EG(j), Ecrit_rxnneg1,ZPE_WR,&
     &vibfreq_WR,num_tot_line)*exp(-(EG(j))/RT)
   enddo
   summation=summation*(Eblocku-Eblockl)*0.5
   summation0=summation0+summation
   summation=0. !re-initialize summation
  enddo
  deltaN = summation0
  !delta1
  summation=0.
  summation0=0.
  block = 1
  Eblocku=Ecrit_rxnneg1 !upper limit of one interval
  Eblockl=0. !lower limit of one interval
  EG=(Eblockl+Eblocku)*0.5+(Eblocku-Eblockl)*node*0.5
   do j=1,20 
     summation=summation+weight(j)&
     &*DOSWR(EG(j), Ecrit_rxnneg1,ZPE_WR,&
     &vibfreq_WR,num_tot_line)*exp(-(EG(j))/RT)
   enddo
   summation=summation*(Eblocku-Eblockl)*0.5
   summation0=summation0+summation
   summation=0. !re-initialize summation
   delta1 = summation0 / deltaN
  !delta2
  summation=0.
  summation0=0.
  block = 1
  Eblocku=Ecrit_rxnneg1 !upper limit of one interval
  Eblockl=0. !lower limit of one interval
  EG=(Eblockl+Eblocku)*0.5+(Eblocku-Eblockl)*node*0.5
   do j=1,20 
     summation=summation+weight(j)&
     &*DOSWR(EG(j), Ecrit_rxnneg1,ZPE_WR,&
     &vibfreq_WR,num_tot_line)*exp(-(EG(j))/RT)*&
     &exp(-(Ecrit_rxnneg1-EG(j))/(FE*RT))
   enddo
   summation=summation*(Eblocku-Eblockl)*0.5
   summation0=summation0+summation
   summation=0. !re-initialize summation
   delta2 = summation0 / deltaN
  !delta
  delta = delta1 - delta2 * (FE*RT)/(FE*RT + a)   

else if (FE_logic==1) then
write(*,*) 'GLT correction with user specified DOS has not yet been implemented!'
stop


else if (FE_logic==0) then
  delta=1.  !no beta_c correction
else 
  delta=1.  !no beta_c correction
endif
end function delta
!----------------------------------------------------------------------------
real*8 function k_HS(Trxn,sigma_Pstar,epsilo_Pstar, MW_Pstar,&
& sigma_bath, epsilo_bath, MW_bath, method_collision)
implicit none
real*8 :: Trxn
real*8 :: sigma_Pstar,epsilo_Pstar, MW_Pstar, sigma_bath, epsilo_bath, MW_bath
! epsilo in the unit of epsilo/kB, i.e. Kelven
real*8 :: k_HS0  ! Oringinal Hard–sphere collision rate const. (cm-3 molecule-1 s-1)
real*8, parameter :: PI=3.14159265359d0
real*8, parameter :: kB=1.98709369d-3 !kcal/(mol*K)
real*8 :: T_red ! reduced T:=T_rxn/epsilo (epsilo is in Kelven)
character(15) :: method_collision
k_HS0=1.d6*(PI*(5.61231d-1*(sigma_Pstar+sigma_bath)*1.d-10)**2)*sqrt(8.d0*1.38064852d-23*Trxn/&
&(PI*1.d-3*MW_Pstar*MW_bath/((MW_Pstar+MW_bath)*6.02214086d23)))
T_red=Trxn/sqrt(epsilo_Pstar*epsilo_bath)
if (method_collision(1:4)=='Reid') then
k_HS=k_HS0*(1.16145*T_red**(-0.14874)+0.52487/(exp(0.7732*T_red))+2.16178/(exp(2.437887*T_red)))
elseif (method_collision(1:4)=='Troe') then
  if ((T_red>=3.) .and. (T_red<=300.)) then
  k_HS=k_HS0*1./(6.97d-1+5.185d-1*log10(T_red))
  else
  k_HS=k_HS0*1./(6.36d-1+5.67d-1*log10(T_red))
  endif
elseif (method_collision(1:5)=='Forst') then
k_HS=2.708d0*k_HS0*(1./T_red)**(1./3.)
end if
end function k_HS
!--------------------------------------------------------------------------
real*8 function FE_computed(Trxn, Ecrit_rxnneg1)
implicit none
real*8 :: Trxn ! in K
real*8 :: max_energy, Ecrit_rxnneg1, stepsize  !in kcal/mol
integer :: num_step ! number of steps=max_energy/stepsize
real*8, dimension(:), allocatable :: E_stab_local(:), DOS_stab_local(:)
real*8 :: step_index
real*8 :: summation
real*8:: beta   !=1/(RT), in (kcal/mol)**-1
real*8, parameter :: autokcal=627.509d0
summation=0d0

open (unit=21, file='DOS_stab.inp', status='old', action='read')
read(21,*) max_energy, stepsize
num_step=int(max_energy/stepsize)
allocate(E_stab_local(num_step),DOS_stab_local(num_step))
do step_index=1, num_step
read(21,*) E_stab_local(step_index), DOS_stab_local(step_index)
enddo
close(21)
beta=1.d0/(Trxn*8.314d0*1.d-3/4.184d0)

do step_index=int(Ecrit_rxnneg1/stepsize), num_step
summation=summation+DOS_stab_local(step_index)*exp(-beta*E_stab_local(step_index))*stepsize/(autokcal)
enddo
FE_computed=beta*summation/((1./autokcal)*DOS_stab_local(Ecrit_rxnneg1/stepsize)*exp(-beta*Ecrit_rxnneg1))
do step_index=1, num_step
summation=summation+DOS_stab_local(step_index)*1.d-2/(autokcal)
enddo
write(*,200) Trxn, FE_computed 
200 format ('At T=', F7.2, ' K, FE=', ES15.3)
end function FE_computed
!--------------------------------------------------------------------------
real*8 function FE_computed2(Trxn, Ecrit_rxnneg1, ZPE_WR, vibfreq_WR, num_tot_line)
implicit none
real*8 :: Trxn ! in K
real*8 :: max_energy, Ecrit_rxnneg1, ZPE_WR  !in kcal/mol
integer :: num_tot_line, i, j , k
real*8 :: vibfreq_WR(num_tot_line-1) 
real*8 :: aE0, lnfact
real*8 :: summation1, summation2, summation3
real*8:: invbeta  !=RT, in kcal/mol
real*8 :: w_WR, beta_WR
summation1=0.d0
summation2=0.d0
summation3=1.
invbeta=Trxn*8.314d0*1.d-3/4.184d0
if (Ecrit_rxnneg1>=ZPE_WR) then
w_WR=10.**(-1.0506d0*(Ecrit_rxnneg1/ZPE_WR)**0.25)
else
w_WR=1./(5.*(Ecrit_rxnneg1/ZPE_WR)+2.73d0*(Ecrit_rxnneg1/ZPE_WR)**0.5+3.51d0)
endif

do i=1, num_tot_line-1
summation1=summation1+vibfreq_WR(i)*vibfreq_WR(i)
summation2=summation2+vibfreq_WR(i)
enddo
beta_WR=(num_tot_line-2)*(num_tot_line-2)*summation1/((num_tot_line-1)*summation2**2)
aE0=1.-beta_WR*w_WR


do j=1, num_tot_line-2
summation3=summation3+(exp(lnfact(dble(num_tot_line-2))-lnfact((dble(num_tot_line-2-j)))))&
&*(invbeta/(Ecrit_rxnneg1+aE0*ZPE_WR))**j
enddo
FE_computed2=summation3
write(*,200) Trxn, FE_computed2
200 format ('At T=', F7.2, ' K, FE=', ES15.3)
end function FE_computed2
!---------------------------------------------------------------
real*8 function FE_computed3(Trxn,Ecrit_rxnneg1, ZPE_WR,&
&vibfreq_WR,num_tot_line,maxEFE3)
implicit none
real*8 :: Trxn ! in K
real*8 :: maxEFE3, Ecrit_rxnneg1, ZPE_WR  !in kcal/mol
real*8 :: DOSWR !W-R DOS
integer :: num_tot_line, i, j , k, m, block
real*8 :: vibfreq_WR(num_tot_line-1)
real*8:: invbeta  !=RT, in kcal/mol
real*8 :: h
real*8 :: node(20),weight(20),EG(20)
real*8 :: summation, summation0, Eblocku, Eblockl

node=(/-0.993128599185094924786,&
-0.963971927277913791268,&
-0.912234428251325905868,&
-0.8391169718222188233945,&
-0.7463319064601507926143,&
-0.6360536807265150254528,&
-0.5108670019508270980044,&
-0.3737060887154195606725,&
-0.2277858511416450780805,&
-0.0765265211334973337546,&
0.0765265211334973337546,&
0.22778585114164507808,&
0.373706088715419560673,&
0.510867001950827098004,&
0.6360536807265150254528,&
0.746331906460150792614,&
0.8391169718222188233945,&
0.9122344282513259058678,&
0.9639719272779137912677,&
0.9931285991850949247861/)
weight=(/0.017614007139152118312,&
0.040601429800386941331,&
0.0626720483341090635695,&
0.083276741576704748725,&
0.1019301198172404350368,&
0.1181945319615184173124,&
0.1316886384491766268985,&
0.1420961093183820513293,&
0.149172986472603746788,&
0.1527533871307258506981,&
0.152753387130725850698,&
0.1491729864726037467878,&
0.142096109318382051329,&
0.131688638449176626899,&
0.11819453196151841731,&
0.101930119817240435037,&
0.08327674157670474872,&
0.062672048334109063569,&
0.040601429800386941331,&
0.017614007139152118312/)

invbeta=Trxn*8.314d0*1.d-3/4.184d0
summation=0.
summation0=0.
block=int(abs(maxEFE3-Ecrit_rxnneg1)/abs(Ecrit_rxnneg1))+1
do k=1, block
   Eblocku=Ecrit_rxnneg1+k*(maxEFE3-abs(Ecrit_rxnneg1))/block !upper limit of one interval
   Eblockl=Ecrit_rxnneg1+(k-1)*(maxEFE3-abs(Ecrit_rxnneg1))/block !lower limit of one interval
   EG=(Eblockl+Eblocku)*0.5+(Eblocku-Eblockl)*node*0.5
   do j=1,20 
     summation=summation+weight(j)&
     &*DOSWR(EG(j), Ecrit_rxnneg1,ZPE_WR,&
&vibfreq_WR,num_tot_line)*exp(-(EG(j))/invbeta)
   enddo
   summation=summation*(Eblocku-Eblockl)*0.5
   summation0=summation0+summation
   summation=0. !re-initialize summation
enddo

FE_computed3=summation0/(invbeta*&
&DOSWR(Ecrit_rxnneg1, Ecrit_rxnneg1,ZPE_WR,&
&vibfreq_WR,num_tot_line)*exp(-(Ecrit_rxnneg1)/invbeta))

write(*,200) Trxn, FE_computed3
200 format ('At T=', F7.2, ' K, FE=', ES15.3)
end function FE_computed3

real*8 function DOSWR(E_DOSWR, E0, ZPE_WR, vibfreq_WR,num_tot_line)
implicit none
integer :: num_tot_line, i
real*8 :: E_DOSWR, E0, ZPE_WR, lnfact, lnDOSWR, productvib
real*8 :: aE, w_WR, beta_WR
real*8 :: vibfreq_WR(num_tot_line-1)
real*8 :: summation1, summation2
real*8 :: E_rel
summation1 = 0.
summation2 = 0.

E_rel= E_DOSWR

if (E_rel>=ZPE_WR) then
w_WR=10.**(-1.0506*(E_rel/ZPE_WR)**0.25)
else
w_WR=1./(5.*(E_rel/ZPE_WR)+2.73*(E_rel/ZPE_WR)**0.5+3.51)
endif

do i=1, num_tot_line-1
summation1=summation1+vibfreq_WR(i)*vibfreq_WR(i)
summation2=summation2+vibfreq_WR(i)
enddo

beta_WR=dble(num_tot_line-2)*dble(num_tot_line-2)*summation1/(dble(num_tot_line-1)*summation2**2)
aE=1.-beta_WR*w_WR

productvib=1.
do i = 1, num_tot_line-1
  productvib=productvib*vibfreq_WR(i)*2.85914d-3  !kcal/mol for each hv term
enddo
lnDOSWR=(log(E_rel+aE*ZPE_WR))*dble(num_tot_line-2)-lnfact(dble(num_tot_line-2))&
&-log(productvib)
DOSWR=exp(lnDOSWR)


end function DOSWR

!-----------------------------------------------------------
real*8 function  lnfact(x)
implicit none
real*8 :: x
if (x<=168) then
lnfact=log(gamma(x+1.d0))
else
lnfact=0.5*log(2*3.14159265359d0*x)+x*log(x)-x+1.d0/(12.*x)
endif
end function lnfact
!--------------------------------------------------------------
real*8 function K_E(Trxn,geomean_freq,s_Pstar, E_sys)
implicit none
real*8 :: Trxn, geomean_freq ! units: T-K, freq_Astar-cm**-1
real*8 :: s_Pstar
real*8 ::  E_sys !E of rxn sys in kcal/mol
real*8 :: n_quanta, alpha, lnfact
real*8, parameter :: kB=1.98709369d-3 !kcal/(mol*K)
K_E=0.d0 !initialization
n_quanta=E_sys/(geomean_freq*2.85911d-3)  !Conversion factor 1cm**-1=2.85911d-3 kcal/mol
alpha=exp(-geomean_freq*2.85911d-3/(kB*Trxn))
K_E=exp(n_quanta*log(alpha)+s_Pstar*log(1.d0-alpha)+lnfact(n_quanta+s_Pstar-1.d0)-lnfact(n_quanta)-lnfact(s_Pstar-1.d0))
end function K_E
!------------------------------------------------
real*8 function Dis_E(Trxn,geomean_freq,s_Pstar,A_rxnneg1,Ecrit_rxnneg1,E_sysdis, Emaxlocal)
implicit none
real*8 :: Trxn,geomean_freq,s_Pstar,A_rxnneg1,Ecrit_rxnneg1,E_sysdis, Emaxlocal
real*8 :: E_index, ini, finali, stepsize
real*8 :: summation_local ! Denominator of eqn. 12
real*8 :: k_i, K_E
Dis_E=0.d0 !initialization
summation_local=0.d0
ini=Ecrit_rxnneg1
finali=Emaxlocal
stepsize=geomean_freq*2.85911d-3  !Conversion factor 1cm**-1=2.85911d-3 kcal/mol
do E_index=ini, finali, stepsize
summation_local=k_i(Trxn,geomean_freq,s_Pstar,A_rxnneg1,Ecrit_rxnneg1, E_index)&
&*K_E(Trxn,geomean_freq,s_Pstar, E_index)+summation_local
end do
Dis_E=k_i(Trxn,geomean_freq,s_Pstar,A_rxnneg1,Ecrit_rxnneg1, E_sysdis)&
&*K_E(Trxn,geomean_freq,s_Pstar, E_sysdis)/summation_local

end function Dis_E
!---------------------------------------------------
real*8 function k_i(Trxn,geomean_freq,s_Pstar,A_i,Ecrit_i, E_syski)
implicit none
real*8 :: Trxn,geomean_freq,s_Pstar,A_i,Ecrit_i, E_syski
real*8 :: n_quanta, m_quanta_i ! number of quanta; E critical number of quanta of rxn i
real*8 :: lnfact
k_i=0.d0 !initilization
n_quanta=E_syski/(geomean_freq*2.85911d-3)  !Conversion factor 1cm**-1=2.85911d-3 kcal/mol
m_quanta_i=Ecrit_i/(geomean_freq*2.85911d-3)
if (n_quanta>=m_quanta_i) then
k_i=exp(log(A_i)+lnfact(n_quanta)+lnfact(n_quanta-m_quanta_i+s_Pstar-1.d0)-lnfact(n_quanta-m_quanta_i)-lnfact(n_quanta+s_Pstar-1.d0))
else
k_i=0.
endif
end function k_i
!------------------------------------------------
real*8 function assign_GammaE1(function_type, E_sysG)
implicit none
integer :: function_type
real*8 :: E_sysG
real*8 :: para1, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11, para12,&
&para01, para02, para03, para04, para05, para06, para07, para08, para09,para010, para011, para012 
common para1, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11, para12, &
para01, para02, para03, para04, para05, para06, para07, para08, para09, para010,para011, para012
if (function_type==0) then
 assign_GammaE1=1.
!--
else if (function_type==1) then
 assign_GammaE1=para1+para2*exp(-E_sysG/para3)+para4*exp(-E_sysG/para5)
 if (assign_GammaE1>1.) then
 assign_GammaE1=1.
 else
 endif
!--
else if (function_type==2) then
 assign_GammaE1=para1+(para2-para1)*(E_sysG)**para4/(para3**para4+(E_sysG)**para4)
 if (assign_GammaE1>1.) then
 assign_GammaE1=1.
 else
 endif
!--
else if (function_type==3) then
 if (E_sysG<=para10) then
 assign_GammaE1=para1+(para2-para1)*(E_sysG)**para4/(para3**para4+(E_sysG)**para4)
 else
 assign_GammaE1=para5+para6*exp(-E_sysG/para7)+para8*exp(-E_sysG/para9)
 endif
 if (assign_GammaE1>1.) then
 assign_GammaE1=1.
 else
 endif
!--
else if (function_type==4) then
 if (E_sysG<=para11) then
 assign_GammaE1=para1+para2*exp(-E_sysG/para3)+para4*exp(-E_sysG/para5)
 else
 assign_GammaE1=para6+para7*exp(-E_sysG/para8)+para9*exp(-E_sysG/para10)
 endif
 if (assign_GammaE1>1.) then
 assign_GammaE1=1.
 else
 endif
!--
else if (function_type==5) then
!input turn must be smaller than maxE
 if (E_sysG<=para6) then
 assign_GammaE1=1.
 else
 assign_GammaE1=para1+para2*exp(-E_sysG/para3)+para4*exp(-E_sysG/para5)
 endif
 if (assign_GammaE1>1.) then
 assign_GammaE1=1.
 else
 endif
else
endif
end function assign_GammaE1
!-------------------------------------------------------------------------------
real*8 function assign_GammaE2(function_type, E_sysG)
implicit none
integer :: function_type
real*8 :: E_sysG
real*8 :: para1, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11, para12, &
& para01, para02, para03, para04, para05, para06, para07, para08,para09, para010, para011, para012
common para1, para2, para3, para4, para5, para6, para7, para8, para9, para10, para11, para12, &
& para01, para02, para03, para04, para05, para06, para07, para08, para09,para010, para011, para012
if (function_type==0) then
 assign_GammaE2=1.
!--
else if (function_type==1) then
 assign_GammaE2=para01+para02*exp(-E_sysG/para03)+para04*exp(-E_sysG/para05)
 if (assign_GammaE2>1.) then
 assign_GammaE2=1.
 else
 endif
!--
else if (function_type==2) then
 assign_GammaE2=para01+(para02-para01)*(E_sysG)**para04/(para03**para04+(E_sysG)**para04)
 if (assign_GammaE2>1.) then
 assign_GammaE2=1.
 else
 endif
!--
else if (function_type==3) then
 if (E_sysG<=para010) then
 assign_GammaE2=para01+(para02-para01)*(E_sysG)**para04/(para03**para04+(E_sysG)**para04)
 else
 assign_GammaE2=para05+para06*exp(-E_sysG/para07)+para08*exp(-E_sysG/para09)
 endif
 if (assign_GammaE2>1.) then
 assign_GammaE2=1.
 else
 endif
!--
else if (function_type==4) then
 if (E_sysG<=para011) then
 assign_GammaE2=para01+para02*exp(-E_sysG/para03)+para04*exp(-E_sysG/para05)
 else
 assign_GammaE2=para06+para07*exp(-E_sysG/para08)+para09*exp(-E_sysG/para010)
 endif
 if (assign_GammaE2>1.) then
 assign_GammaE2=1.
 else
 endif
!--
else if (function_type==5) then
!input turn must be smaller than maxE
 if (E_sysG<=para06) then
 assign_GammaE2=1.
 else
 assign_GammaE2=para01+para02*exp(-E_sysG/para03)+para04*exp(-E_sysG/para05)
 endif
 if (assign_GammaE2>1.) then
 assign_GammaE2=1.
 else
 endif
else
endif
end function assign_GammaE2
!-------------------------------------------------------------------------------
subroutine readfile(num_tot_line)
IMPLICIT NONE
INTEGER, PARAMETER :: maxrecs = 1000000
INTEGER :: J, NR, ios, num_tot_line
CHARACTER(LEN=30) :: junk
NR = 0
open (unit=24, file='vibinfo.inp', status='old', action='read')
DO J=1,maxrecs
 READ(24,*,IOSTAT=ios) junk
 IF (ios /= 0) EXIT
 IF (J == maxrecs) THEN
 write(*,*) "Error: Maximum number of records exceeded..."
 write(*,*) "Exiting program now..."
 STOP
 ENDIF
 NR = NR + 1
ENDDO
num_tot_line=NR
close(24)
end subroutine
!--------------------------------------------------------------------------------
SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
PARAMETER(NMAX=1000,TINY=1.E-25)
REAL*8 XA(N),YA(N), X,Y,DY
REAL*8 C(NMAX),D(NMAX)
REAL*8 DD,H,HH,T,W
NS=1
HH=DABS(X-XA(1))
DO I=1,N
  H=DABS(X-XA(I))
  IF(H.EQ.0.D0) THEN
    Y=YA(I)
    DY=0.D0                 
    RETURN
  ELSE IF (H.LT.HH) THEN
    NS=I
    HH=H
  ENDIF
  C(I)=YA(I)
  D(I)=YA(I)+TINY        !The TINY part is needed to prevent a rare 
END DO                   !zero-over-zero condition.
Y=YA(NS)
NS=NS-1
DO M=1,N-1
  DO I=1,N-M
    W=C(I+1)-D(I)
    H=XA(I+M)-X          !H<>0 (tested before)
    T=(XA(I)-X)*D(I)/H
    DD=T-C(I+1)
    IF(DD.EQ.0.) Pause ' Pole at requested value of X.'
    DD=W/DD
    D(I)=C(I+1)*DD
    C(I)=T*DD
  END DO
  IF(2*NS.LT.N-M) THEN
    DY=C(NS+1)
  ELSE 
    DY=D(NS) 
    NS=NS-1
  ENDIF
  Y=Y+DY
END DO 
END SUBROUTINE                     



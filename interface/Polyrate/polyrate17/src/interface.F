C***************************************************************************
c        function case (string)
c****************************************************************************
c
         function case (string)
c
c   Function which takes a string of 80 characters and converts the 
c   upper case letters in the string to lower case letters
c
      implicit double precision (a-h,o-z)
         character*80 string, case
         character*1 xlett
c
         do 10 i = 1, 80
               xlett = string(i:i)
               itry = ichar (xlett)
                if (xlett .ge. 'A' .and. xlett .le. 'Z') then 
                    itry = itry + 32
                    string (i:i) = char (itry)
                endif
   10     continue
c
                case  = string 
c
          return
          end
C
C**********************************************************************
C CFLOAT
C**********************************************************************
C
      FUNCTION cfloat(STRING)
C
      use perconparam; use common_inc
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER(*) :: STRING
      CHARACTER*80 NUMBER
      CHARACTER CH
      LOGICAL LEXP,LDEC
C
      LEXP = .FALSE.
      LDEC = .FALSE.
      LENGTH = LEN(STRING)
      IF (LENGTH .EQ. 0) THEN
         CFLOAT = 0.0D0
         RETURN
      ENDIF
C     WRITE(FU6,*) LENGTH,STRING
C
C     Find the first nonblank character
C
      I = 1
10    IF (STRING(I:I) .EQ. ' ' .AND. I .LE. LENGTH) THEN
         I = I + 1
         GOTO 10
      ENDIF
C
C     If it is a blank string set function to zero
C
      IF (I .GT. LENGTH) THEN
         CFLOAT = 0.0D0
         RETURN
      ENDIF
      IBEG = I
C
C     Find the first blank character after the number
C
      I = IBEG+1
20    IF (STRING(I:I) .NE. ' ' .AND. I .LE. LENGTH) THEN
         I = I + 1
         GOTO 20
      ENDIF
      IEND = I-1
C
C     Stripe the blanks before and after the number
C
      NUMBER = STRING(IBEG:IEND)
      LENGTH = IEND - IBEG + 1
C   
C     Make sure there is no blank left
C
      IF (INDEX(NUMBER,' ') .LE. LENGTH) THEN
         WRITE(FU6,1000) STRING
         STOP 'CFLOAT 1'
      ENDIF
C
C     Find the decimal point
C
      IDEC = INDEX(NUMBER,'.')
      IF (IDEC .NE. 0) LDEC = .TRUE.
C
C     Find the exponential symbol
C
      IUE = INDEX(NUMBER,'E')
      ILE = INDEX(NUMBER,'e')
      IUD = INDEX(NUMBER,'D')
      ILD = INDEX(NUMBER,'d')
      ISUM = IUE + ILE + IUD + ILD
      IEXP = MAX0(IUE,ILE,IUD,ILD)
      IF (ISUM .GT. IEXP) THEN
         WRITE(FU6,1000) STRING
         STOP 'CFLOAT 2'
      ENDIF
      IF (IEXP .NE. 0) THEN
         LEXP = .TRUE.
      ELSE
         IEXP = LENGTH + 1
      ENDIF
C
      IF (.NOT. LDEC) IDEC = IEXP
C
C     Get the number before decimal
C
      IBEG = 2
      IF (NUMBER(1:1) .EQ. '+') THEN
         SIGN = 1.0D0
      ELSEIF(NUMBER(1:1) .EQ. '-') THEN
         SIGN = -1.0D0
      ELSE
         SIGN = 1.0D0
         IBEG = 1
      ENDIF
      IF (IBEG .EQ. IEXP) THEN
         F1 = 1.0D0
      ELSE
         F1 = 0.0D0
      ENDIF
      DO 50 I = IBEG,IDEC-1
         CH = NUMBER(I:I)
         IF (CH .GE. '0' .AND. CH .LE. '9') THEN
            N = ICHAR(CH) - ICHAR('0')
            F1 = F1 * 10.0D0 + DBLE(N)
         ELSE
            WRITE(FU6,1000) STRING
            STOP 'CFLOAT 3'
         ENDIF
50    CONTINUE
C
C     Get the number after decimal 
C
      F2 = 0.0D0
      IF (LDEC) THEN
         J = 0
         DO 60 I = IDEC+1,IEXP-1
            CH = NUMBER(I:I)
            IF (CH .GE. '0' .AND. CH .LE. '9') THEN
               N = ICHAR(CH) - ICHAR('0')
               F2 = F2 * 10.0D0 + DBLE(N)
               J = J + 1
            ELSE
               WRITE(FU6,1000) STRING
               STOP 'CFLOAT 4'
            ENDIF
60       CONTINUE
         F2 = F2 / 10.0D0 ** DBLE(J)
      ENDIF
C
C    Get the exponent
C
      ESIGN = 1.0D0
      F3 = 0.0D0
      IF (LEXP) THEN 
         IBEG = IEXP + 2
         IF (NUMBER(IEXP+1:IEXP+1) .EQ. '+') THEN
            ESIGN = 1.0D0
         ELSEIF(NUMBER(IEXP+1:IEXP+1) .EQ. '-') THEN
            ESIGN = -1.0D0
         ELSE
            ESIGN = 1.0D0
            IBEG = IEXP + 1
         ENDIF
         DO 70 I = IBEG,LENGTH
            CH = NUMBER(I:I)
            IF (CH .GE. '0' .AND. CH .LE. '9') THEN
               N = ICHAR(CH) - ICHAR('0')
               F3 = F3 * 10.0D0 + DBLE(N)
            ELSE
               WRITE(FU6,1000) STRING
               STOP 'CFLOAT 5'
            ENDIF
70       CONTINUE
      ENDIF 
C
      CFLOAT = (SIGN * (F1 + F2)) * 10.0D0 ** (ESIGN*F3)
C
      RETURN
C
1000  FORMAT(/1X,'Illegal number: ',A80)
C
      END
c
c ***************************************************************************
c     default
c ***************************************************************************
c
      subroutine default(PARALLEL)
c
c     This is the main subroutine for setting the default for all the
c     parameters and variables used in Polyrate.  The defaults for each
c     different section of input is set in a separate subroutine for 
c     clarity.
c
      implicit double precision (a-h,o-z)
      LOGICAL PARALLEL
      call default_mem
c
c GENERAL
      call defgen(PARALLEL)
c ENERGETICS
      call defeng
c SECOND
      call defsec
c OPTIMIZATION
      call defopt
c STATIONARY
      call defsta
c PATH
      call defpat
c TUNNEL
      call deftun
c RATE
      call defrat
c
      return
      end
c
c ***************************************************************************
c     defeng
c ***************************************************************************
c
      subroutine defeng
c
c     subroutine to set the defaults for keywords used in the ENERGETICS
c     section 
c
      use keyword_interface, only : nlist,cezero,basis,potnam
c     use pot, only : 
      use potmod
      use perconparam
      use common_inc
      use kintcm, only : nlistl,ipot,iezeru,ibasis
!      implicit double precision (a-h,o-z)
       implicit none
c
c ___________________________________________________________________________
c
c  set defaults for switches
c ___________________________________________________________________________
c
c
c  Switch               default
c -------            --------------------------
c     
c ___________________________________________________________________________
c
c    set defaults for variables
c ___________________________________________________________________________
c
c  Variable          polyrate interface variable
c  --------          ---------------------------
c   POTENTIAL          potnam, ipot
c   EZERO              cezero
c   EZUNIT             iezeru                                           0619PJ01
c
      potnam      =   'hooks'                                           0312YC97
      ipot        =   0
      cezero      =   'calculate'
      iezeru      =   0                                                 0619PJ01 
c
c     There are NO defaults for ACESOPTE and ACESOPTF but we will
c     initialize the arrays to blank and zero.
c
      nlist(:,:)(:)=' ' ; nlistl=0
c
c     There are NO defaults for SPECBASIS but the arrays will be        0522RS95
c     initialized here.
c
      if(allocated(basis))deallocate(basis)
      allocate(basis(natom)); basis='   ';   ezer0=0.d00
      ibasis=0                                                          0522RS95

      return
      end
c
c ***************************************************************************
c     defgen
c ***************************************************************************
c
      subroutine defgen(PARALLEL)
      use common_inc;  use kintcm; use keyword_interface
      use cm
      use rate_const, only : sincr
c
c     subroutine to set the defaults for keywords used in the GENERAL
c     section
c
      implicit double precision (a-h,o-z)
      LOGICAL PARALLEL
c
c TITLE - default is blank
      do 1 i=1,5
        do 1 j=1,80
1        ftitle(i)(j:j) = ' '
c
      parallel=.FALSE.                                                  1219BL04

c RESTART -  default is calculation is not a restart and path is not saved
      iunxt = 0
      sincr = 0.0d0                                                     09/95KAN
c
c SUPERMOL -                                                            0327YC97
      isup = 0
c
c CLASSVIB - default is quantized vibrations                            0528JC97
      iclasv = 0
c
c UNIT - default is angstrom for input and output                       0402JZ07
c     igeou5 = 0                                                        1104JC97
      iunit5 = 0                                                        0405JZ07
      gufac5 = 1.88972652D0                                                --   
      iunit6 = 0                                                           --   
      gufac6 = 1.88972652D0                                             0405JZ07
c
c CHECK - default is to not stop after reading and checking input
      icheck = 0
c
c MDMOVIE - default is not to write the file
      imdmov = 0
c     
c WRITEFU30 - default is not to write the file
      iwrt30 = 0
c
c WRITEFU31 - default is not to write the file                          0810JC97
      iwrt31 = 0                                                        0810JC97
c
c WRITE62 - default is not to write the file                            0522TA02
      iwrt62 = 0                                                        0522TA02
c
      ivtst = -1                                                        07/95KAN
c
c     write(fu6,*) 'defgen ivtst',ivtst
c VRC-TST - default is not to use VRC-TST
      ivrc  =  0                                                        1026JZ07
c Monte Carlo sampling points
      nmc   =  1000
c Number of Gauss-Leguerre quadrature points used in VRC-TST
      nniter = 4
c Generator type in SPRNG
      igtype = 4
c step for angular momentum quantum number J
      jstep = 5
c maximum value of angular momentum quantum number J
      jmax = 300 
      return
      end
c
c ***************************************************************************
c     defopt
c ***************************************************************************
c
      subroutine defopt
c
c     subroutine to set the defaults for keywords used in the OPTIMIZATION
c     section
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use rate_const; use potmod
      implicit double precision (a-h,o-z)
c
c ___________________________________________________________________________
c
c  set defaults for switches
c ___________________________________________________________________________
c
c
c  Switch               default
c -------            --------------------------
c   POLYRATE         on
c   ACES             off - use POLYRATE routines
c   MOPAC            off - use POLYRATE routines
c   PRINT            do not print
c   RETRY            off
c   BFGS             on                                                   IR0495
c   TSBFGS           off                                                  IR0495
c   SDSTART          on                                                   IR0495
c
      igpot  = 0
      potgeo = 'polyrate'                                                 0308YC97
      iprxnt =  0
      ibfgs  = 1                                                          IR0495
      ibfgst = 0                                                          IR0495
      ieft   = 0                                                          0317YC99
      ief    = 0                                                          0317YC99
      iretry  = 0                                                         IR0495
      ihunit = 1                                                          IR0495
c
c ___________________________________________________________________________
c
c    set defaults for variables
c ___________________________________________________________________________
c
c  Variable          polyrate interface variable
c  --------          ---------------------------
c   GCOMP              convg
c   GCOMPTS            convgt
c   DLX2               dlx1
c   SCALE              scale
c   STPTOL             stptol
c   NITER              jniter
c   HREC               ihrec                                             IR0495
c   HRECTS             ihrect                                            IR0495
c

c --- Commented by: Jingzhi Pu                                          0619PJ01
c     Reason:                                                           0619PJ01
c      INITGEO default is moved to 'defsta' subroutine in POLYRATE8.7,  0619PJ01
c      because this keyword is not defined in OPTIMIZATION section.     0619PJ01
c
c   INITGEO            initg                             0327YC97       0619PJ01
c
c      do 10 i = 1, 5                                    0514PF97       0619PJ01
c      initg(i)    =   0                                 0514PF97       0619PJ01
c 10   continue                                          0514PF97       0619PJ01
   

      convg       =   1.d-5                                              IR0495 
      convgt      =   1.d-5                                              IR0495
      dlx1        =   1.d-5
      scale       =   0.2d0                                              IR0495
      stptol      =   1.d-5                                              IR0495
      jniter      =   50
      ihrec       =   10                                                 IR0495
      ihrect      =   5                                                  IR0495
c
      rmax  = 4.0d0                                                      0317YC99
      rmin  = 0.0d0                                                      0317YC99
      omin  = 0.8d0                                                      0317YC99
      ddmax = 0.3d0                                                      0317YC99
      ddmaxts = 0.5d0                                                    0317YC99
c
      return
      end
c
c ***************************************************************************
c     defpat
c ***************************************************************************
c
      subroutine defpat
c
c     subroutine to set the defaults for keywords used in the PATH section 
c
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use rate_const; use potmod
      implicit double precision (a-h,o-z)
c
c ___________________________________________________________________________
c
c  set defaults for switches
c ___________________________________________________________________________
c
c
c  Switch               default
c -------            --------------------------
c   NEXTPT           do not just calculate the next point
c   IC               do not perform zero-order interpolated corrections
c   ICOPT            base on corrected data
c     restart        VTST-IC was done in the original run
c     modify         use parameters in original file #50
c   HESS             compute the Hessian along the MEP
c   SFIRST           do not use a special step size for first n points
c   ESD              use euler intergrator - all other intergrators     0601YC98
c                    are turned off (set to zero)
c   SSPECIAL         assume no special s values for normal mode analysis 
c   SSPECPR          do no special printing at the special s values 
c   EXREACT          do not extrapolate into reactant region
c   EXPROD           do not extrapolate into product region
c   PRINTSTEP        do not print at every step
c   PRSAVERP         print at every save step
c   PRSAVEMODE       print N.M. at every save step
c   PRDISTMX         print interatomic distace matrices every 20 save grids 
c   SYMMETRY         assume no symmetry along the reaction path
c   CALCMEFF         do not automatically calculate effective reduced mass
c                    along path
c   SADDLE           the reaction has a saddle point
c   SPECSTOP         do not use special stop for reaction path          0423TA02
c   RODS             dividing surface is not re-oriented                0929PF97
c   VRP              do not use vrp rxn path following                  0219PF98
c   lsst             do not use SS-T torsional correction               0517JZ12
c
      ipath      =  0                                                   1219BL04
      inxtpt     =  0
      ivic       =  0
      ivicm      =  2                                                   1203YC96
      ivice      =  0
      ivico      =  1
      icrst      =  0                                                   0524YC96
      icmod      =  0                                                   0911RS96
      ihess      =  1
      isfrst     =  0
      ieuler     =  1
      ies1       =  0
      ipagem     =  0
      ispec      =  0
      ispcpr     =  0
      iexrct     =  0
      iexprd     =  0
      iprstp     =  0
      iprsmd     =  0
      iprsve     =  0
      isym       =  0
      iscalerp   =  0                                                   0109BE07
      imeff      =  0                                                   0911RS96
      ireord     =  0                                                   1210YC96
      inosad     =  0                                                   0210JC97
      iprdis     =  20                                                  0507YC97
      nprsmd     =  9999                                                0626YC97
      isstop     =  0                                                   0423TA02
      irods      =  0                                                   0929PF97
      ivrp       =  0                                                   0929PF97
c
      ifrfac      =   0                                                 0808JC00
      ifcfac      =   0                                                 1227PJ00
      isclpt      =   0                                                 0211PJ01
      irtpjac     =   0                                                 0411PJ01
      icartrp     =   1                                                 0625TA02
      ifqfac    =   0                                                   0815PJ01
      incrf     =   0                                                   1008JZ09
      lsst      =   0
c
c
c ___________________________________________________________________________
c
c    set defaults for variables
c ___________________________________________________________________________
c
c  Variable          polyrate interface variable
c  --------          ---------------------------
c   SCALEMASS          redm
c   SLP                slp
c   SLM                slm
c   SSTEP              del
c   NSTEPS             nst
c   FIRSTSTEP          fstep
c   COORD              coord                                            07/95KAN
c   INTDEF             intdef                                           07/95KAN
c   SDEBG1             sdebg1                                           07/95KAN
c   SDEBG2             sdebg2                                           07/95KAN
c   IDIRECT            isen
c   NFSTEP             nst0
c   FSIZE              del1
c   DELTA2             delta2
c   DIFFD              diffd
c   DLX3               d3lx
c   INH                inh
c   INI                ini                                              0202YC98
c   CURV               curv
c   VSCALE             vfac
c   EXNSTEP            nste
c   EXSTEP             dlex
c   EXALPHA            alph
c   SPRNT              sprnt
c   POTINF             nptinf
c   EZERO              cezero
c   SIGN               psign
c   INTMU              intmu                                            0327YC97
c   SVL                svl                                              1029JZ07
c   SVU                svu                                              1029JZ07
c   SDEL               sdel                                             1029JZ07

c
      intmu       =   1                                                 0327YC97
      redm        =   1.0d0                                             09/95KAN
      slp         =  +1.0d0                                             09/95KAN
      slm         =  -1.0d0                                             09/95KAN
      del         =   5.d-4                                             09/95KAN
      nst         =   99999
      fstep       =   'nmode'
      coord       =   'cart'                                            09/95KAN
      intdef      =   'intdef'                                          09/95KAN
      sdebg1      =  0.0d0                                              07/95KAN
      sdebg2      =  0.0d0                                              07/95KAN
      isen        =   1
      nst0        =   0
      del1        =   1.d-5                                             09/95KAN
      delta2      =   5.d-5                                             09/95KAN
      diffd       =   1.d-8
      d3lx        =   1.d-4
      curv        =   'dgrad'                                           0816YC96
      vfac        =   1.0d0                                             09/95KAN
      freqfac     =   1.0d0                                             0808JC00
      sprnt       =   0.0d0                                             09/95KAN
      nptinf      =   0
      cezero      =   'calculate'
      inh         =   9                                                 1118PF97
      ini         =   1                                                 0601YC98
      psign       =   'reactant'
      sincw = 0.01d0                                                    0601YC98
      inm31 = 1                                                         0601YC98
      lbexp = 3                                                         0601YC98
      ifxmf = 0                                                         0317YC99
      tension = -1.0d0                                                  0911JZ08
c unit for variables svl, svu, and sdel is angstrom now, they need to
c converted to au. 
      svl   = 2.0d0*1.88972652D0                                        1029JZ07
      svu   = 2.0d0*1.88972652D0                                        1029JZ07
      svs   = 1.0d0*1.88972652D0                                        1029JZ07
       
c
c     set extrapolation defaults
c
      do 1 i=1,2
        nste(i)     =   0
        dlex(i)     =   0.03d0                                          09/95KAN
        alph(i)     =   1.0d0                                           09/95KAN
1     continue
      iwha(1) = -1
      iwha(2) =  1
c
      return
      end
c
c ***************************************************************************
c     defrat
c ***************************************************************************
c
      subroutine defrat
c
c     subroutine to set the defaults for keywords used in the RATE section 
c
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use rate_const
      implicit double precision (a-h,o-z)
c
c ___________________________________________________________________________
c
c  set defaults for switches
c ___________________________________________________________________________
c
c
c  Switch               default
c -------            --------------------------
c   FORWARDK         do not just compute forward rate (off) 
c   BOTHK            compute both forward and reverse rates 
c   EDGEOK           stop calculation if near end of grid (on)
c   GSPEC            assume no special grid for free energy (off)
c   GTEMP            assume no special grid for free energy (off)
c   EACT             do not compute activation energies
c   ANALYSIS         do no detailed analysis
c   TST              compute conventional transition state theory
c   CVT              compute canonical transition state theory but
c                    not any of the other variational methods
c   PRDELG           do not print free energy curve
c   PRPART           do not print partition functions
c   PRGIGT           do not print improved generalized free energy curve
c   PRVIB            do not print extra vibrational mode information
c   PRPATH           print reaction path info. to unit fu25,26
c   VPFEXP           do not scale either partition functions
c   SPT              do not use perturbation method
c   SPTOPT  
c      CORIOLIS      do not use coriolis term
c      PRINT         do not print out extra data
c   GTLOG            do not calculate generalized free energy of activation
c                    using the logarithm of the vibration partition functions
c     
      irate  = 0                                                        1219BL04
      ifrate = 0
      ibrate = 1
      ngflag = 1
      igspec = 0
      igtemp = 0
      ieact  = 0
      ianaly = 0
      itst = 1
      nfcvt = 1
      icvt = 0
      muvt = 0
      iejmuvt = 0
      nfus = 0
      nfcus = 0                                                         1001PF97
      iprg = 0
      iprt = 0
      iprigt = 0
      iprvib = 0
      iscale = 0
      ipvib = 0
      ipvibc = 0
      ipvibp = 0
c
      iprmep = 0                                                        0706WH94
      ixmol = 0                                                         0706WH94
      iclf = 0
      ibathm = 0                                                        0317YC99
      igtlog = 0                                                        0423TA02
c
c ___________________________________________________________________________
c
c    set defaults for variables
c ___________________________________________________________________________
c
c  Variable          polyrate interface variable
c  --------          ---------------------------
c   REVKEXP            nrev  (scale factor for reverse rates)
c   SIGMAF             sigmaf
c   SIGMAR             sigmar
c   STATE              state
c     DIAB             switc 
c   TEMP               ntemp, temp(40)
c   SMAX               slpg
c   SMIN               slmg
c   PRVIB              sx,sy ==> sob,soe
c   MUVTOPT
c      NITER           mniter
c      FIT1            ifit1 ==> iftmv1
c      FIT2            ifit2 ==> iftmv2
c      PRENERGY        mnprmv ==> nprmvt
c      SLOWER          xsmmvt ==> smmvt
c      SUPPER          xspmvt ==> spmvt
C      EGRID           egrid 
c   STATEOPT           L9, LN3 ==> set to 0 (thermal)
c
      nrev      =   0
      sigmaf    =   1
      sigmar    =   1
      state     =   'therm'
      switc     =   1.D+10
      ntemp     =   7
      temp(1)   =   200.0d0                                             09/95KAN
      temp(2)   =   300.0d0                                             09/95KAN
      temp(3)   =   400.0d0                                             09/95KAN
      temp(4)   =   600.0d0                                             09/95KAN
      temp(5)   =   1000.0d0                                            09/95KAN
      temp(6)   =   1500.0d0                                            09/95KAN
      temp(7)   =   2400.0d0                                            09/95KAN
      slpg      =   1.0d0                                               09/95KAN
      slmg      =  -1.0d0                                               09/95KAN
      sob       =   0.0d0                                               09/95KAN
      soe       =   0.0d0                                               09/95KAN
      mniter    =   30
      ifit1     =   5
      ifit2     =   3
      mnprmv    =   0
      xsmmvt    =   0.0d0                                               09/95KAN
      xspmvt    =   0.0d0                                               09/95KAN
      egrid     =   5.d0*cmtoau
c
      do 1 i=1,4
        do 1 j=1,n3tm
1       l9(i,j) = 0
c
      return
      end
c
c ***************************************************************************
c     defsec
c ***************************************************************************
c
      subroutine defsec
c
c     subroutine to set the defaults for keywords used in the SECOND
c     section
c
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use rate_const
      implicit double precision (a-h,o-z)

c
c ___________________________________________________________________________
c
c  set defaults for switches
c ___________________________________________________________________________
c
c
c  Switch               default
c -------            --------------------------
c   POLYRATE         on
c   ACES             off - use POLYRATE routines
c   MOPAC            off - use POLYRATE routines
c   FPRINT           do not print second derivative info
c
      ispot  = 0
      potsec  = 'ghook'                                                 0301YC97
      ifprnt =  0
c
c ___________________________________________________________________________
c
c    set defaults for variables
c ___________________________________________________________________________
c
c  Variable          polyrate interface variable
c  --------          ---------------------------
c   POTENTIAL          potnam, ipot
c   EZERO              cezero
c   NUMSTEP            xnmstp
c   NUMTYP             numtyp
c
      potnam      =   'hooks'                                           0312YC97
      ipot        =   0
      cezero      =   'calculate'
      xnmstp      =   1.d-4                                             09/95KAN
      numtyp      =   'central'
c
      return
      end
c
c ***************************************************************************
c     defsta
c ***************************************************************************
c
      subroutine defsta
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use rate_const
c
c     subroutine to set the defaults for keywords used in the all the
c     stationary point sections:  REACT1, REACT2, PROD1, PROD2,  WELLR, WELLP,
c     and START
c
      implicit double precision (a-h,o-z)
c
c ___________________________________________________________________________
c
c  set defaults for switches
c ___________________________________________________________________________
c
c
c  Switch               default
c -------            --------------------------
c   HARMONIC         assume harmonic approximation - all anharmonicity flags
c                    are turned off (set to zero)
c   KBPRINT          do not print turning points in WKB calculation
c   DIATOM           assume a non-diatom
c   PROJECT          project out overall translations and rotations
c   IFIRST           do not use a special step size for 1st derivatives
c   STATUS           information of the stationary points               0725YC97
c   FREQUNIT         unit of the frequencies                            0807YC97
c     
         ifirst     =  0
      do 1 i=1,8
         jxfreq(i)  =  1
         iharm(i)   =  1
         imor(i)    =  0
         imtyp(i)   =  0
         imorqq(i)  =  0
         iqqwkb(i)  =  0
         iqqsem(i)  =  0
         iwkb(i)    =  0
         ivary(i)   =  0
         ikbprt(i)  =  0
         ndiat(i)   =  0
         iproj(i)   =  1
         iolin(i)   =  3                                                0507YC97
         ilina(i)   =  0                                                0527PF97
         nf(i)      =  0
         istatu(i)  =  0                                                0725YC97
         steng(i)   =  0.0d0   
         ifreu(i)   =  0                                                0807YC97
         iprmd(i)   =  0                                                0317YC99
1     continue
c
c ___________________________________________________________________________
c
c    set defaults for variables
c ___________________________________________________________________________
c
c  Variable          polyrate interface variable
c  --------          ---------------------------
c   CONSTANT           ncnst
c   DEMIN              xdemin
c   ANTLR              xantlr
c   DQQP               xdqqp
c   WKBTOL             xwkbtl
c   KBQUAD             ikbqua
c   MORMODEL           mormodel
c   SPECIES            idmn                                             0603PF97
c   INITGEO            initg                                            0619PJ01
c
      do 2 i=1,8
         ncnst(i)    =   0
         xdemin(i)   =   0.159d0                                        09/95KAN
         xantlr(i)   =   1.d-8                                          09/95KAN
         xdqqp(i,1)  =   1.d-3                                          09/95KAN
         xdqqp(i,2)  =   1.d-4                                          09/95KAN
         xwkbtl(i)   =   1.d-8                                          09/95KAN
         ikbqua(i)   =   40
         mortyp(i)   =   'morsei'
         idmn(i)     =   'nonlinrp'                                     0603PF97
2     continue
         idmn(5)     =   'nonlints'                                     0911JC97
         idmn(7)     =   'nonlinwe'                                     0911JC97
         idmn(8)     =   'nonlinwe'                                     0911JC97
c
c     Assume anharmonicity will be treated the same over the entire range
c     of s values.  This is used with the VRANGE keyword.
      nregon = 0

c
c     INITGEO - initial geometry is set to be read from fu5.            0619PJ01
c     Moved from 'defopt' subroutine in POLYRATE-version 8.7            0619PJ01  
c
      do i = 1, 8                                                       0222PJ02
          initg(i) = 0                                                  0619PJ01
      end do                                                            0619PJ01


c
c ___________________________________________________________________________
c
c    set defaults for lists
c ___________________________________________________________________________
c
c GEOM  -   Z-matrix inputs have no variables by default                0425RS95

      do 60 i=1,8                                                       0425RS95
60      nvarj(i) = 0                                                    0425RS95
c
c
c ELEC  -      initialize electronic degenercies and energies
      do 3 i=1,24
        nedeg(i) = 0
        elec(i) = 0.0d0                                                 09/95KAN
3     continue
      do 20 i=1,24,3
20      nedeg(i) = 1
c
c     initialize reactant and product flag
      do 4 i=1,8
4       irepr(i) = 0
c
c VANHAR -     initialize all modes to harmonic

      do 5 i=1,n3tm
        do 6 j=1,4
          vharmr(i,j) = 'harmonic '
          moder(j,i) = 0                                                0425YC96
6       continue
        vharmr(i,5) = 'harmonic '
        mode(i) = 0
        vharmr(i,7) = 'harmonic '                                       0726PF97
        vharmr(i,8) = 'harmonic '                                       0726PF97
        moder(7,i) = 0                                                  0726PF97
        moder(8,i) = 0                                                  0726PF97
5     continue
c
c VRANGE -        initialize all regions to harmonic

c
c TOR -       initialize storage arrays
      do 9 i=1,8
        nmnumb(i) = 0
9       nmodes(i) = 0
      do i = 1,8                                                        0317YC99
        do j = 1, n3tm                                                  0317YC99
          ntrnb(i,j) = 0                                                0521YC99
c using CWO scheme as default                                            0408JZ10
          ntrsch(i,j) = 7                                               0521YC99
c using Full for MN method as default                                   0408JZ10
          ntrlev(i,j) = 1                                               0521YC99
c using MN method for torsion as default                                0408JZ10
          ntrmtd(i,j) = 1                                               0408JZ10
          ntrm(i,j) = 1
          ntrnum(i,j) = 0                                               0521YC99
          ntrbnd(i,j,1) = 4                                             0521YC99
          ntrbnd(i,j,2) = 5                                             0521YC99
          readi(i,j) = 0.0d0                                            0326JZ10
          do k = 1, NMAXP                                               0521YC99
            ntrsig(i,j,k) = 1                                           0521YC99
            torome(i,j,k) = 0.0d0                                       0521YC99
            torw(i,j,k) = 1000.0d0/AUTOCM                               0521YC99
            toru(i,j,k) = 0.0d0                                         0521YC99
            tormi(i,j,k) = 0.0d0
            torwl(i,j,k) = 0.0d0                                        1206BE05
            torwr(i,j,k) = 0.0d0                                        1206BE05
            ratiol(i,j,k) = 0.0d0                                       1206BE05
            ratior(i,j,k) = 0.0d0                                       1206BE05
          enddo                                                         0521YC99
          do k=1,natom
            ntrisb(i,j,k) = 0.0d0                                       0521YC99
          enddo                                                         0521YC99
C         do k=1,4                                                      0521YC99
C           ntrisb(i,j,k) = k                                           0521YC99
C         enddo                                                         0521YC99
        enddo                                                           0317YC99
      enddo                                                             0317YC99
c
c Initialize variables internal to POLYRATE that are revelent to
c the stationary points
c
      do 10 i = 1, n6tm
         anhrm(i) = 0.0d0
         y00(i) = 0.0d0
         y00r(i) = 0.0d0
         xer(i) = 0.0d0
10    continue
      return
      end subroutine defsta
c
c ***************************************************************************
c     deftun
c ***************************************************************************
c
      subroutine deftun
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use rate_const
c
c     subroutine to set the defaults for keywords used in the TUNNEL section 
c
      implicit double precision (a-h,o-z)
c
c ___________________________________________________________________________
c
c  set defaults for switches
c ___________________________________________________________________________
c
c
c  Switch               default
c -------            --------------------------
c   WIGNER           compute wigner tunneling
c   ZCT              on
c   SCT              on
c   LCT              off
c   PRPROB           LCT print option - off
c   PRFREQ           LCT print option - off
c   PRDETAIL         LCT print option - off
c   SPLINE           assume spline fit for interpolating effective mass
c   LAGRANGE         off - use spline fit
c   EMIN             assume an energy is not given
c   QRST             off
c   ILCT             ilcgit                                             0507AR02
c     
      itunnl   = 0                                                      1219BL04
      iwign    = 1
      izct     = 1
      isct     = 1
      ilct     = 0
      ipprob   = 0
      ipfreq   = 0
      ipdat    = 0
      ipgrid   = 0
      isplne   = 1
      ilagrg   = 0
      iemin    = 0
      iqrst    = 0
      ilcgit   = 0                                                      0507AR02
c
c ___________________________________________________________________________
c
c    set defaults for variables
c ___________________________________________________________________________
c
c  Variable          polyrate interface variable
c  --------          ---------------------------
c   LAGRANGE           nlang
c   EMIN               pemin
c   NQE                nq12
c   NQTH               nq22
c   NEXCIT             nexcit
c   NSEGBOLTZ          nseg
c   NSEGTHETA          nseg2
c   VEFSRCH            nvef                                             0708JC00
c   NGTHETA            ng
c   NGAMP              ngs0
c   INTERPOLATE        intlct
c   VADAVG             ivavg                                            0708JC00
c   VEFSRCH            ivsrch                                           0708JC00
C   LCTRST             ilcrst                                           0708JC00
C   LCTSTORE           ilcstr                                           0708JC00
c   STATES(in QRST)    iqrnst (all states considered)                   0423TA02
c   STQVIB(in QRST)    iqrnsq (all states considered)                   0522TA02
c   nsplic number of grid points with ILCT1D
c   nsplix and nspliy are the grid points with ILCT2D
c
      nlang       =   4
      pemin       =   0.0d0                                             09/95KAN
      nq12        =   40
      nqq2        =   40
      nq22        =   40
      nqq2        =   40
      nqq1        =   nq12
      nqq2        =   nq22
      nexcit      =   -1
      nseg        =   1
      nseg2       =   1
c     nvef        =   0                                                 0708JC00
      nvef        =   999                                               0731JZ08
      ng          =   120
      ngs0        =   120
      intlct      =   4
      ivavg       =   0                                                 0708JC00
      ilcrst      =   0                                                 0708JC00
      ilcstr      =   1                                                 0708JC00
      allocate(nsplic(maxps),nsplix(maxps),nspliy(maxps))
      nsplic = 9; nsplix = 9; nspliy = 11
      iqrnst      =   -1                                                0423TA02
      iqrnsq      =   -1                                                0522TA02
c
c
      return
      end
c
c ***************************************************************************
c     icint
c ***************************************************************************
c
      function icint(svalue)
c
c   Function which takes a character string which is really a integer and
c   converts it to an integer.  Error checking not possible using ichar is
c   done.  Program will stop if an illegal character is passed via the 
c   string 'svalue'.
c
c      implicit double precision (a-h,o-z)
c
      use perconparam, only : fu6
      character(*) :: svalue
c
c     check for negative or positive sign (- or +)
c
      icint = 0
      istart = 1
      isign = +1
      if (svalue(1:1).eq.'+') then
         isign = +1
         istart = 2
      else if(svalue(1:1).eq.'-') then
         isign = -1
         istart = 2
      end if
c
c     now convert each character to an integer - and store in icint
c
      k = istart
      do while (svalue(k:k).ne.' ')
         j = ichar(svalue(k:k)) - ichar('0')
         if (j.lt.0.or.j.gt.9) then
            write(fu6,1000)svalue
            stop
         end if
1000     format(3x,'ERROR: illegal digit in an integer: ',A80)
c
         icint = 10.*icint + float(j)
         k = k+1
      end do
      icint = icint * isign
c
      return
      end
c
c ************************************************************************
c     INTAB 
c ************************************************************************
c
      subroutine intab 
c
c     Subroutine to write out a table of all the input parameters
c
      use common_inc; use kintcm; use cm
      use perconparam; use rate_const
      use keyword_interface
      use efmain_mod, only : ddmaxts
      implicit double precision (a-h,o-z)
c
      character * 3 swit(2)
      character * 3 yswit(2)
      character * 4 swlct(2)                                            0708JC00
      character * 2 asymb(103)
c
      data swit /'off','on'/
      data yswit /'no','yes'/
      data swlct /'LCG3','LCG4'/                                        0708JC00
c
c     array of atomic symbols
c
      data (asymb(i),i=1,103)  /
     *   'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
     *   'Na','Mg','Al','Si','P ','S ','Cl','Ar',
     *   'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu',
     *        'Zn','Ga','Ge','As','Se','Br','Kr',
     *   'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     *        'Cd','In','Sn','Sb','Te','I ','Xe',
     *   'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb',
     *        'Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ','Re',
     *        'Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     *   'Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk',
     *        'Cf','Es','Fm','Md','No','Lr'/
c
c
c    for a restart run - only write out the parameters in the 
c    Tunnel and Rate sections
c     if (lgs(8).gt.0) go to 100
c
c     *********************************************************************
c           write out current date and time and then the title
c     *********************************************************************
c
      write(fu6,1000)
      call dattim(fu6)
      do 1 i=1,5
          if (ftitle(i).ne.' ') write(fu6,1001) ftitle(i)
1     continue
      write(fu6,1002)
1000  format(//,80('-'),/80('-')//)
1001  format(a80)
1002  format(//80('-'))
c
c     *********************************************************************
c           write out data read in *GENERAL section
c     *********************************************************************
c
      write(fu6,1100)
1100  format(//,80('-'),/80('-'),//2x,' General Section Parameters: ',
     *   //80('-')/)
c     
c     classvib                                                          0528JC97
      if (iclasv.eq.1) write (fu6,1125)                                 0528JC97
1125  format(2x,'CLASSVIB: classical harmonic vibrational ',            0528JC97
     *   'partition functions.')                                        0528JC97
c
      if (isup.eq.1) then                                               0327YC97
         write (fu6,1150) 'nosupermol'                                  0327YC97
      else                                                              0327YC97
         write(fu6,1150) 'supermol'                                     0327YC97
      endif                                                             0327YC97
1150  format (2x,'SUPERMOLECULE mode: ',47x,a10)                        0327YC97
c
c     if (igeou5.eq.0) then                                             0402JZ07
      if (iunit5.eq.0) then                                             0405JZ07
         write (fu6,1175) 'angstrom'                                    1104JC97
      else                                                              1104JC97
         write(fu6,1175) 'a.u.'                                         1104JC97
      endif                                                             1104JC97
      if (iunit6.eq.0) then                                             0405JZ07
         write (fu6,1176) 'angstrom'                                    0405JZ07
      else
         write(fu6,1176) 'a.u.'                                         0405JZ07
      endif
c1175  format (2x,'GEOMUNIT: Units for fu5 input geometries: ',25x,a10)  1104JC97
1175  format (2x,'INPUNIT: Units for fu5 input geometries and distances:
     *',13x,a10)                                                        0405JZ07
1176  format (2x,'OUTUNIT: Units for fu6 output geometries and distances
     *:',12x,a10)                                                       0405JZ07
c
      if (ivtst.eq.1) then
        write(fu6,1200) 
      end if
1200  format(2x,'IVTST: Interpolated VTST calculation will be done')
c
      if (ivic.eq.1) then                                            
        if (ivico.eq.1) write(fu6,1204) 'corrected'                     0514WH94
        if (ivico.eq.2) write(fu6,1204) 'both'                          0514WH94
        if (ivice.eq.0) write(fu6,1296) 'SECKART'                       0726YC98
        if (ivice.eq.1) write(fu6,1296) 'DECKART'                       0726YC98
        if (ivicm.eq.0) write(fu6,1205) 'ICA'                           03/96/YC
        if (ivicm.eq.1) write(fu6,1205) 'ICR'                           03/96/YC
        if (ivicm.eq.2) write(fu6,1205) 'ICL'                           03/96/YC
        if (ivicm.eq.3) write(fu6,1205) 'none'                          0625YC97
        if (rangea.ne.0) write (fu6,1297) rangea                        1203YC96
1296  format(2x,'VTST-IOC: correction procedure for VMEP',31x,a7)       0808YC97
1297  format(2x,'VTST-IOC: width of the Eckart at HL',33x,f9.4)         1203YC96
      end if
1204  format(2x,'VTST-IOC: zero-order interpolated corrections',23x, 
     *   a9)
1205  format(2x,'VTST-IOC: correction procedure for frequencies',27x,   0808YC97
     *   a4) 
      if (ivic.eq.2) write (fu6,1295)                                   0605YC98
1295  format(2x,'VTST-ISPE: correction procedure for VMEP')             0605YC98
      if (icrst.eq.1) write(fu6,1210)                                   1/4RS96
1210  format(2x,'This is a restart calculation where the ',             1/4RS96
     *   'the VTST-IOC correction was not applied in the original ',    1/4RS96
     *   'but it will be used here.')                                   1/4RS96
c
      if (icmod.eq.1) write(fu6,1220)                                   5/29RS96
1220  format(2x,'This is a restart calculation where a ',               5/29RS96
     *   'a new file 50 will be used for computing the VTST-IOC ',      5/29RS96
     *   'correction .')                                                5/29RS96
c
      if (imdmov.eq.1) write(fu6,1230)
1230  format(2x,'MDMOVIE file is written to unit fu65') 
c
      if (iwrt30.eq.1) write(fu6,1240)                                  0524YC96
1240  format(2x,'FILE 30 is written to unit fu30')                      0524YC96
c
      if (iwrt31.eq.1) write(fu6,1231)                                  0810JC97
1231  format(2x,'FILE 31 is written to unit fu31')                      0810JC97
c
      if (iwrt62.ne.0) write(fu6,1262)                                  0522TA02
1262  format(2x,'Information about stationary points is ',              0522TA02
     *          'written to unit fu62')                                 0522TA02
c
c    VRC-TST 
c
      if (ivrc.eq.1) write(fu6,1270)
1270  format(2x,'VRC-TST: Variable-reaction-coordinate TST ',           1026JZ07
     *        'will be done')
c
c
c     restart calculation
c
      if (iunxt.ne.0) then
        if (iunxt.eq.-1)  write(fu6,1300)
        if (iunxt.eq.1)   write(fu6,1400)
        if (abs(sincr).lt.1e-8) then
           if (iunxt.eq.2)  write(fu6,1500)
           if (iunxt.eq.3) write(fu6,1600)
        else
           if (iunxt.eq.2)  write(fu6,1700) sincr
           if (iunxt.eq.3) write(fu6,1800) sincr
        end if
      end if
c
1300  format(2x,'Restart Information Saved:  MEP is written to unit',
     *  ' fu1')
1400  format(2x,'Restart Calculation:  MEP is read from unit fu1')
1500  format(2x,'Restart Calculation:  MEP is read from units fu1 and',
     *  ' fu2')
1600  format(2x,'Restart Calculation:  MEP is read from units fu1 and',
     *  ' fu2 and written to unit fu3')
1700  format(2x,'Restart Calculation:  MEP is read from units fu1 and',
     *  ' fu2 with s values from fu2 incremented by ',f10.4)
1800  format(2x,'Restart Calculation:  MEP is read from units fu1 and',
     *  ' fu2 and written to unit fu3  with s values from fu2 ',
     *  ' incremented by',f10.4)
c
c     atomic masses
c
      if (lgs(8) .le. 0) write(fu6,1900)
1900  format(/2x,'Atomic information:',/12x,'Atom ',4x,
     *  'Atomic number',4x,'Atomic Symbol',5x,'Mass (amu)',/)           06/96ELC
      do 2 i=1,natom
2       write(fu6,1950)i,label(i),asymb(label(i)),xmass(i)
1950  format(13x,i2,10x,i3,15x,a2,10x,f10.6)                            06/96ELC
c
c
c     *********************************************************************
c           write out data read in *ENERGETICS section
c     *********************************************************************
c
      write(fu6,2000)
2000  format(//,80('-'),/80('-'),//2x,' ENERGETICS Section Parameters: ',
     *   //80('-')/)
c
c     potential type
c
      write(fu6,2100) potnam
2100  format(2x,'Potential and first derivative type: ',a8)
c
c     zero of energy
      if (cezero .eq. 'read') then                                      0512WH94
         if (iezeru.eq.1) then                                          0205YC97
            write(fu6,2110) cezero,ezer0*ckcal                          0512WH94
         else                                                           0513WH94
            write(fu6,2120) cezero,ezer0                                0512WH94
         endif                                                          0513WH94
      else                                                              0512WH94
         write(fu6,2130) cezero                                         0512WH94
      endif                                                             0512WH94
c
2110  format(2x,'Zero of energy: ',a9,4x,f12.4,' kcal/mol')             0512WH94
2120  format(2x,'Zero of energy: ',a9,4x,f12.4,' a.u.')                 0512WH94
2130  format(2x,'Zero of energy: ',a9)                                  0512WH94
c
c     Special basis set - if used with ACES II                          0522RS95
c                                                                       0522RS95
      if (ibasis.ne.0) then                                             0522RS95
        write(fu6,2140)                                                 0522RS95
        do  210 i=1,ibasis                                              0522RS95
210         write(fu6,2150) basis(i)                                    0522RS95
      end if                                                            0522RS95
2140  format(/2x,'** ACES II Special Basis Set: '/)                     0522RS95
2150  format(5x,a30)                                                    0522RS95
c
c
c     *********************************************************************
c           write out data read in *SECOND section
c     *********************************************************************
c
      write(fu6,3000)
3000  format(//,80('-'),/80('-'),//2x,' SECOND Section Parameters: ',
     *   //80('-')/)
c
c     method type
c
      write(fu6,3100)potsec
3100  format(2x,'Second derivatives are computed using the methods in: '
     *        ,a8)

c
c     Print out the normal mode information by FPRINT                   0619PJ01
c
      write(fu6,3300) swit(ifprnt+1)                                    0619PJ01

c
c     print out switches:  used with POLYRATE internal routines
c
c
c     print out switches: used with POLYRATE internal routines
c
       if (potsec.eq.'ghook') then                                      0301YC97
         write(fu6,3200)
c
c --- Commented by: Jingzhi Pu                                          0619PJ01
c     Reason:                                                           0619PJ01 
c        This FPRINT msg should be printed no matter                    0619PJ01
c                 hhook or ghook is used.                               0619PJ01
c
c        write(fu6,3300) swit(ifprnt+1)                                 0619PJ01
c

         write(fu6,3400)
         if (iunit6.eq.0) then                                          0405JZ07
          write(fu6,3510) xnmstp/gufac6
         else
          write(fu6,3512) xnmstp/gufac6
         endif                                                          0405JZ07
         write(fu6,3520) numtyp
      end if                                                            0425RS95
c
3200  format(//2x,'** Switches:'/)
3300  format(4x,'FPRINT: print normal mode information',23x,a3)
3400  format(//2x,'** Second Derivative Parameters:'/)
3510  format(4x,'NUMSTEP:  second derivative step size for frequencies',
     *   2x,e12.6,2x,'Angstroms')
3512  format(4x,'NUMSTEP:  second derivative step size for frequencies',
     *   2x,e12.6,2x,'Bohrs')                                            0405JZ07
3520  format(4x,'NUMTYP:  method for computing second derivative',
     *   7x,a9)
c
c
c     *********************************************************************
c           write out data read in *OPTIMIZATION section
c     *********************************************************************
c
      write(fu6,4000)
4000  format(//,80('-'),/80('-'),//2x,' OPTIMIZATION Section ',         1116WH94
     *   'Parameters: ',//80('-')/)
c
c     method type
c
      write(fu6,4100) potgeo
4100  format(2x,'Geometry optimizations are computed using the ',
     *     'methods in: ',a8)
c
c     print out switches: used with POLYRATE internal routines
c
      if (potgeo.eq.'polyrate') then                                    0301YC97
        write(fu6,4200)
        write(fu6,4310) swit(iprxnt+1)
        write(fu6,4320) swit(ibfgs+1)                                   0911RS96
        write(fu6,4325) swit(ibfgst+1)                                  0911RS96
        write(fu6,4330) swit(ihunit+1)                                  0911RS96
        write(fu6,4340) swit(iretry+1)                                  0911RS96
c
        write(fu6,4400) 
        if(iunit6.eq.0) then                                            0405JZ07
          write(fu6,4510) dlx1/gufac6
          write(fu6,4520) convg
          write(fu6,4525) convgt                                        
          write(fu6,4530) jniter
          write(fu6,4540) scale                                         
          write(fu6,4545) stptol/gufac6                                 0405JZ07
          write(fu6,4550) ihrec                                         IR0495
          write(fu6,4560) ihrect 
        else
          write(fu6,4512) dlx1/gufac6
          write(fu6,4520) convg                                         
          write(fu6,4525) convgt                                        
          write(fu6,4530) jniter
          write(fu6,4540) scale                                         
          write(fu6,4546) stptol/gufac6                                 0405JZ07
          write(fu6,4550) ihrec                                         IR0495
          write(fu6,4560) ihrect  
        endif                                                           0405JZ07
c       write(fu6,4520) convg
c       write(fu6,4525) convgt
c       write(fu6,4530) jniter
c       write(fu6,4540) scale
c       write(fu6,4545) stptol                                          IR0495
c       write(fu6,4550) ihrec                                           IR0495
c       write(fu6,4560) ihrect                                          IR0495
        if ((ieft.eq.1).or.(ief.eq.1)) then                             0317YC99
         write (fu6,4570) rmax                                          0317YC99
         write (fu6,4571) rmin                                          0317YC99
         write (fu6,4572) omin                                          0317YC99
         write (fu6,4573) ddmax                                         0317YC99
         write (fu6,4574) ddmaxts                                       0317YC99
        endif                                                           0317YC99 
      end if                                                            0425RS95
c
4200  format(//2x,'** Switches:'/)
4310  format(4x,'PRINT:  print derivatives after each newton step',
     *     12x,a3)
4320  format(4x,'BFGS:   update the Hessian using the BFGS method',12x,
     *       a3)
4325  format(4x,'TSBFGS: update the Hessian using the BFGS method (TS)',
     *       7x,a3)
4330  format(4x,'SDSTART: start with unit Hessian (steepest descent)',  IR0495
     *       9x,a3)
4340  format(4x,'RETRY:  try Newton step if quasi-Newton stalls',14x,a3)IR0495
c
4400  format(//2x,'** Geometry Optimization Parameters:'/)
4510  format(4x,'DLX2:  second derivative step size for optimization',
     *   4x,f8.6,1x,'Angstroms')                                        0405JZ07
4512  format(4x,'DLX2:  second derivative step size for optimization',  0405JZ07
     *   4x,f8.6,1x,'bohrs')
4520  format(4x,'GCOMP:  Max. gradient component threshold',7x,e15.6,   06/96ELC
     *       ' hartree/bohr')                                           06/96ELC
4525  format(4x,'TSGCOMP: Max. gradient component threshold (TS)',2x,
     *       e14.6,' hartree/bohr')                                     06/96ELC
4530  format(4x,'NITER:  number of iterations in geom opt',19x,i4)
4540  format(4x,'SCALE:  maximum component of Newton step',15x,f8.6)
4545  format(4x,'STPTOL: fail if max. component of Newton step less',   IR0495
     *' than',f8.6, ' Angstroms')                                       0405JZ07
4546  format(4x,'STPTOL: fail if max. component of Newton step less',   0405JZ07
     *' than',f8.6, ' bohrs')  
4550  format(4x,'HREC:   iterations between Hessian calculations',12x,
     *       i4)
4560  format(4x,'TSHREC: iterations between Hessian calculations (TS)',
     *       7x, i4)
4570  format(4x,'RMAX: maximum value of the energy to be bracketed',    0317YC99
     *       6x, f8.6)                                                  0317YC99
4571  format(4x,'RMIN: minimum value of the energy to be brackedtd',    0317YC99
     *       6x, f8.6)                                                  0317YC99
4572  format(4x,'OMIN: minumum value for overlaping of the direction',  0317YC99
     *       4x, f8.6)                                                  0317YC99
4573  format(4x,'DDMAX: maximum value of the trust radius (in ang)',    0317YC99
     *       6x, f8.6)                                                  0317YC99
4574  format(4x,'DDMAXTS: minumum value of the trust radius (in ang)',  0317YC99
     *       4x, f8.6)                                                  0317YC99
c
c     *********************************************************************
c           write out data read in *REACT1 section
c     *********************************************************************
c
c
c     write out reactant #1 information if section was present in input deck
c
      if (irepr(1).eq.1) then
        write(fu6,5500)
        call intabs(1)
      end if
5500  format(//,80('-'),/80('-'),//2x,' Reactant #1 Parameters: ',
     *   //80('-')/)
c
c
c     *********************************************************************
c           write out data read in *REACT2 section
c     *********************************************************************
c
c
c     write out reactant #2 information if section was present in input deck
c
      if (irepr(2).eq.1) then
        write(fu6,5600)
        call intabs(2)
      end if
5600  format(//,80('-'),/80('-'),//2x,' Reactant #2 Parameters: ',
     *   //80('-')/)
c
c
c     *********************************************************************
c           write out data read in *PROD1 section
c     *********************************************************************
c
c
c     write out product #1 information if section was present in input deck
c
      if (irepr(3).eq.1) then
        write(fu6,5700)
        call intabs(3)
      end if
5700  format(//,80('-'),/80('-'),//2x,' Product #1 Parameters: ',
     *   //80('-')/)
c
c
c     *********************************************************************
c           write out data read in *PROD2 section
c     *********************************************************************
c
c
c     write out product #2 information if section was present in input deck
c
      if (irepr(4).eq.1) then
        write(fu6,5800)
        call intabs(4)
      end if
5800  format(//,80('-'),/80('-'),//2x,' Product #2 Parameters: ',
     *   //80('-')/)
c
c
c     *********************************************************************
c           write out data read in *START section
c     *********************************************************************
c
c
c     write out starting point information if section was present in input deck
c
      if (irepr(5).eq.1) then
        write(fu6,5900)
        call intabs(5)
      end if
5900  format(//,80('-'),/80('-'),//2x,' Starting point Parameters: ',
     *   //80('-')/)
c
c     *********************************************************************
c           write out data read in *WELLR section                       0727PF97
c     *********************************************************************
c
c
c     write out wellr information if section was present in input deck
c
      if (irepr(7).eq.1) then
        write(fu6,5925)
        call intabs(7)
      end if
5925  format(//,80('-'),/80('-'),//2x,' WELLR Parameters: ',
     *   //80('-')/)
c
c     *********************************************************************
c           write out data read in *WELLP section                       0727PF97
c     *********************************************************************
c
c
c     write out wellp information if section was present in input deck
c
      if (irepr(8).eq.1) then
        write(fu6,5950)
        call intabs(8)
      end if
5950  format(//,80('-'),/80('-'),//2x,' WELLP Parameters: ',
     *   //80('-')/)
c
c     *********************************************************************
c           write out data read in *PATH section
c     *********************************************************************
c
      if (ipath.ne.0) then
      write(fu6,6000)
6000  format(//,80('-'),/80('-'),//2x,' PATH Section Parameters: ',
     *   //80('-')/)
c
c     print out switches
c
      write(fu6,6160)
6160  format(2x,'** Switches:'/) 
c
      if(ivrc.eq.1) then                                                0708JZ08
      write(fu6,6165) yswit(1)
      else
      write(fu6,6165) yswit(2-inosad)                                   0210JC97
      endif
      write(fu6,6170) swit(inxtpt+1)
      write(fu6,6180) swit(ieuler+1)
      write(fu6,6190) swit(iexrct+1)
      write(fu6,6200) swit(iexprd+1)
      write(fu6,6210) swit(iprstp+1)
c      write(fu6,6220) swit(iprsve+1)
c      write(fu6,6230) swit(iprsmd+1)
      write(fu6,6240) swit(ihess+1)
      write(fu6,6245) swit(imeff+1)
      write(fu6,6246) swit(ireord+1)
      write(fu6,6231) swit(irods+1)                                     1001PF97
      write(fu6,6247) swit(iclf+1)                                      0317YC99
      write(fu6,6248) swit(ibathm+1)                                    0317YC99
      write(fu6,6249) swit(icartrp+1)                                   0625TA02
c
c --- if FCSCALE is used and CARTRP is not declared                     0815PJ01
c     CARTRP will be turned on as default later                         0815PJ01 
c
c     if (ifcfac.eq.1 .and. icartrp .eq. -1) then                       0815PJ01
c        write(fu6,6249) swit(icartrp+2)                                0815PJ01
c     else                                                              0815PJ01
c        write(fu6,6249) swit(icartrp+1)                                0815PJ01  
c     end if                                                            0815PJ01 
c     The lines above were commented by TVA.                            0625TA02
c

6165  format(4x,'SADDLE:  the reaction has a saddle point',21x,a3)      0210JC97
6170  format(4x,'NEXTPT:  compute only next point on path',21x,a3)
6180  format(4x,'ESD:  use Euler integrator',35X,a3)                    0601YC98
6190  format(4x,'EXFIRST:  extrapolate into reactant region',19x,a3) 
6200  format(4x,'EXSECOND:  extrapolate into product region',19x,a3) 
6210  format(4x,'PRINTSTEP:  print geom, v, and grad at each step',
     *    13x,a3) 
6231  format(4x,'RODS:  re-orient the dividing surface',24x,a3)         0929PF97
6240  format(4x,'HESS:  compute the Hessians along the MEP',20x,a3)
6245  format(4x,'CALCMEFF:  calculate effective reduced mass along',
     *    ' MEP',8x,a3)
6246  format(4x,'REORDER:  reordering frequencies along the MEP',
     *    15x,a3)
6247  format(4x,'IVTST0FREQ:  low frequency mode by IVTST-0',19x,a3)    0317YC99
6248  format(4x,'BATH:  solvent approximaton',34x,a3)                   0317YC99
6249  format(4x,'CARTRP:  vib. analysis in cartesian for RP',19x,a3)    0619PJ01
c
c     print out parameters
c
      write(fu6,6250)
6250  format(//2x,'** Reaction Path Parameters:'/)
c
      if(ivrc.ne.1) then                                                0708JZ08
      write(fu6,6260) redm
      if(iunit6.eq.1) write(fu6,6270) del/gufac6                        0405JZ07
      if(iunit6.eq.0) write(fu6,6272) del/gufac6                        0405JZ07
      write(fu6,6290) nst
      write(fu6,6300) fstep
      write(fu6,6310) isen 
      write(fu6,6325) psign 
      write(fu6,6330) curv 
      write(fu6,6335) coord                                             07/95KAN
      if(sdebg1.ne.0.0.and.sdebg2.ne.0.0)
     *   write(fu6,6336)sdebg1/gufac6,sdebg2/gufac6                     0405JZ07
      write(fu6,6340) vfac 
      if (ifrfac.ne.0) write(fu6,6345) freqfac                          0808JC00
      write(fu6,6350) sprnt/gufac6                                      0405JZ07
      write(fu6,6351) nptinf 
      if(iunit6.eq.0) write(fu6,6352) d3lx/gufac6                       0405JZ07
      if(iunit6.eq.1) write(fu6,63521) d3lx/gufac6                      0405JZ07
      write(fu6,6353) intmu                                             0327YC97
      write(fu6,6168) ini                                               0202YC98
      write(fu6,6164) iprdis
      write (fu6,6167) nprsmd                                           0626YC97
      write (fu6,6166) nprsmd                                           0626YC97
      else                                                              0708JZ08
      if(iunit6.eq.1) write(fu6,6150) svs/gufac6                        0708JZ08
      if(iunit6.eq.0) write(fu6,6151) svs/gufac6                        0708JZ08
      write(fu6,6152) maxs                                              0708JZ08
      endif                                                             0708JZ08
6150  format(4X,'Step size of s for VRC-MDS:',37X,F4.2,2X,'Bohr')       0708JZ08
6151  format(4X,'Step size of s for VRC-MDS:',37X,F4.2,2X,'Agnstrom')   0708JZ08
6152  format(4X,'Maximum number of multifaceted dividing surface',12X,  0708JZ08
     *          I4)                                                     0708JZ08
c
6168  format(4x,'INI: interval of gradient step for interpolation',11x  0202YC98
     *         ,I5)                                                     0202YC98
6166  format(4x,'PRSAVEMODE:  print save point information + n.m. ',    0626YC97 
     *          ' every ',I5, ' save-grids.')                           0626YC97
6167  format(4x,'PRSAVERP:  print save point information in every ',    0626YC97 
     *          '       ',I5, ' save-grids.')                           0626YC97
6164  format(4x,'PRDISTMX:  print interatomic distance matrices in',    0507YC97
     *          ' every ',I5, ' save-grids.')                           0507YC97
6260  format(4x,'SCALEMASS: mass to which coordinates are scaled',      06/96ELC
     *       15x,f9.6,1x,'amu')                                         06/96ELC
6270  format(4x,'SSTEP:  step size in mass scaled distance',20x,e14.6,
     *       1x,'bohr')                                                 06/96ELC
6272  format(4x,'SSTEP:  step size in mass scaled distance',20x,e14.6,
     *       1x,'angstrom')                                             0405JZ07
6290  format(4x,'NSTEPS:  maximum number of steps',30x,i8) 
6300  format(4x,'FIRSTSTEP: method for determining first step',19x,a5)
6310  format(4x,'IDIRECT: initial direction off of saddle pt.',18x,i2) 
6325  format(4x,'SIGN: unbound eigenvector points toward the',20x,a7)
6330  format(4x,'CURV:  method for computing curvature',26x,a7) 
6335  format(4x,'COORD: method for computing bound vibrations',19x,a5)  07/95KAN
6336  format(4x,'Internal coordinate debug print from s equal',F6.2,    07/95KAN
     * ' to',F6.2)
6340  format(4x,'VSCALE:  scale factor for MEP',34x,f8.6) 
6345  format(4x,'FREQSCALE:  scale factor for vibrational frequencies', 0808JC00
     *       11x,f8.6)                                                  0808JC00
6350  format(4x,'SPRNT:  RPH information is printed if abs(s) >',
     *   17x,f8.6) 
6351  format(4x,'POTINF:  number of pieces of info. printed from PES',
     *   11x,i2) 
6352  format(4x,'DLX3:  step size for numerical 3rd derivatives ',14x,  0528WH94
     *   E14.6,1x,'angstroms')                                          0405JZ07
63521 format(4x,'DLX3:  step size for numerical 3rd derivatives ',14x, 
     *   E14.6,1x,'bohrs')                                              0405JZ07
6353  format(4x,'INTMU: interpolation of mueffective at saddle point',  0327YC97
     *   11x,I2)
c
c     print out special regions of s where anharmonicity is altered
c
      if (nregon.gt.0) then
        write(fu6,6355)
        do 3 i=1,nregon
          if(iunit6.eq.0) then                                          0405JZ07
           write(fu6,6357)sanhrm(i,1)/gufac6,sanhrm(i,2)/gufac6,nmode(i)
     *                   ,cmodet(i,nmode(i))
          else
           write(fu6,6358)sanhrm(i,1),sanhrm(i,2),nmode(i),
     *                    cmodet(i,nmode(i))
          endif                                                         0405JZ07
3       continue
      end if
6355  format(/2x,'** Method of computing anharmonicity is varied for',
     *   ' the following modes in the following regions:',
     *   /12x,'s region',10x,'mode',5x,'anharmonicity')
6357  format(5x,f8.4,1x,'angstroms',5x,f8.4,1x,'angstroms',5x,i2,9x,a9)
6358  format(5x,f8.4,1x,'bohrs',5x,f8.4,1x,'bohrs',5x,i2,9x,a9)
c
c     print out range of s computed
c
      if(iunit6.eq.1) then                                              0405JZ07
      write(fu6,6360)slm,slp  
      else
      write(fu6,6361)slm/gufac6,slp/gufac6                              0405JZ07
      endif
6360  format(//2x,'** Reaction path is computed for the',               06/96ELC
     *  ' interval',/5x,f9.6,1x,'bohrs',                                06/96ELC
     *  ' < = s < = ',f9.6,1x,'bohrs')                                  06/96ELC
6361  format(//2x,'** Reaction path is computed for the',               0405JZ07
     *  ' interval',/5x,f9.6,1x,'angstroms',                            0405JZ07
     *  ' < = s < = ',f9.6,1x,'angstroms')                              0405JZ07
c
      if (isstop.gt.0) write (fu6,6362) (1.d0-fracdw),fracdw            0423TA02
      if (isstop.lt.0) write (fu6,6364) (1.d0-fracdw),fracdw            0423TA02
6362  format(5x,'or until V becomes smaller than',/5x,'[',              0423TA02
     *    f5.3,' * V(saddle point) + ',f5.3,' * V(rorp)]',/5x,          0423TA02
     *    'where V(rorp) is the higher of reactants or products.')      0423TA02
6364  format(5x,'or until Va^G becomes smaller than',/5x,'[',           0423TA02
     *    f5.3,' * V+ZPE(saddle point) + ',f5.3,' * V+ZPE(rorp)]',/5x,  0423TA02
     *    'where V+ZPE(rorp) is the higher of reactants or products.')  0423TA02
c
c     print out special step size if used
c
      if (isfrst.eq.1) write(fu6,6370) del1/gufac6,nst0
6370  format(/2x,'** Special step size of ',f8.6,' is used for the',
     *  ' first ',i3,' steps',/5x,'off the saddle point')
c
c     print out special s values where normal mode analysis is computed 
c
      write(fu6,6380)
      if (ispec.eq.1) then
         do 4 i=1,nspec
           if(iunit6.eq.0) then                                         0405JZ07
             write(fu6,6390) sspec(i)/gufac6
           else
             write(fu6,6392) sspec(i)/gufac6
           endif
4        continue
      else
         write(fu6,*) '    None'                                        06/96ELC
      end if
6380  format(/2x,'** Generalized normal mode analysis will be carried', 06/96ELC
     * ' out at the following',/4x, ' s values in addition to the s',   06/96ELC
     * ' values in the save grid:',/)                                   06/96ELC
6390  format(5x,f9.6,2x,'angstroms')                                    0405JZ07
6392  format(5x,f9.6,2x,'bohrs')                                        0405JZ07
c
c     print out integrator
c
      if (ivrp.eq.1) then                                               0929PF97
         if (ieuler.eq.1) then                                          0929PF97
            write(fu6,6425)                                             0929PF97
         else if (ies1.eq.1) then                                       0929PF97
            write(fu6,6426)                                             0929PF97
            stop                                                        0930PF97
         else if (ipagem.eq.1) then                                     0929PF97
            write(fu6,6427)                                             0929PF97
            stop                                                        0930PF97
         end if                                                         0929PF97
      else if (ieuler.eq.1) then
         write(fu6,6400)
      else if (ies1.eq.1) then
         write(fu6,6410) delta2/gufac6,diffd
      else if (ipagem.eq.1) then
         write(fu6,6420) inh                                            1118PF97
      end if
      if (ivrp.eq.1) then                                               0930PF97
         write(fu6,6428)                                                0930PF97
      endif                                                             0930PF97
6400  format(/2x,'** Euler method is used to compute the reaction path') 
6410  format(/2x,'** Euler with stabilization is used to compute the',
     * ' reaction path',/
     * 5x,'step size for finite difference derivative: ',f8.6, 
     * 5x,'magnitude of difference vector:             ',f8.6) 
6420  format(/2x,'** Page-McIver method is used to compute the',
     * ' reaction path',/
     * 5x,'step size scale factor : ',4x,i4)                            1118PF97
6425  format(/2x,'** VRP/E method is used to compute the',
     *      ' reaction path')                                           0219PF98
6426  format(/2x,'** VRP/ES1 method is not supported in this version')  0219PF98
6427  format(/2x,'** VRP/PM method is not supported in this version')   0219PF98
6428  format(11x,'CALCS:  option for determining s',24x,'MEP')          0219PF98
c
c     print out extrapolation options 
c
      if (iexrct.eq.1) then
         write(fu6,6430)
         if (iwha(1).eq.1) write(fu6,6432)
         if (iwha(1).eq.-1) write(fu6,6434)
         write(fu6,6440)nste(1),dlex(1)/gufac6,alph(1)                  0405JZ07
      else if (iexprd.eq.1) then
         write(fu6,6450)
         if (iwha(2).eq.1) write(fu6,6432)
         if (iwha(2).eq.-1) write(fu6,6434)
         write(fu6,6440)nste(2),dlex(2)/gufac6,alph(2)
      end if
6430  format(/2x,'** Extrapolation in first direction: ')
6432  format(7x,'Extrapolation towards products')
6434  format(7x,'Extrapolation towards reactants')
6440  format(7x,'Number of steps to be taken: ',14x,i4,
     *  /7x,'Step size: ',32x,f8.6,
     *  /7x,'Extrapolation range parameter: ',12x,f8.6)
6450  format(/2x,'** Extrapolation in second direction: ')
c
c     print out the scaling force constants information                 0621PJ01
c
      if (ifcfac .eq. 1) then                                              .. 
          j = 1                                                            ..
          write(fu6,6460)                                                  ..
          write(fu6,6461)                                                  ..
          do i=1,NBL                                                       .. 
              if (fcfac(j) .ne. 1.0d0) then                                ..
                 write(fu6, 6470) IBL(1,i), IBL(2,i), fcfac(j)             ..
              end if                                                       ..
              j = j + 1                                                    ..
          end do                                                           ..  
          do i=1,NBA                                                       ..
              if (fcfac(j) .ne. 1.0d0) then                                .. 
                 write(fu6, 6471) IBA(1,i),IBA(2,i),IBA(3,i),              ..
     *                            fcfac(j)                                 ..
              end if                                                       ..
              j = j + 1                                                    ..
          end do                                                           .. 
          do i = 1, NTO                                                    ..
              if (fcfac(j) .ne. 1.0d0) then                                ..  
                  write(fu6, 6472) ITO(1,i),ITO(2,i),ITO(3,i),             ..
     *                             ITO(4,i),fcfac(j)                       ..
              end if                                                       ..
              j = j + 1                                                    ..
          end do                                                           ..
          do i = 1, NLBE                                                   .. 
              if (fcfac(j) .ne. 1.0d0) then                                .. 
                  write(fu6, 6473) ILBE(1,i),ILBE(2,i),                    ..
     *                             ILBE(3,i),fcfac(j)                      ..
              end if                                                       ..
              j = j + 1                                                    ..
          end do                                                           ..
      end if                                                               .. 
c
6460  format(/2x,'** Scaling principal force constants: ')                 ..
6461  format(7x, 'internal coordinate', 5x, 'scaling factor')              ..
6470  format(9x,i3,' -',i2,17x,f7.4)                                       ..   
6471  format(9x,i3,' -',i2,' -',i2,13x,f7.4)                               ..
6472  format(9x,i3,' -',i2,' -',i2,' -',i2,9x,f7.4)                        .. 
6473  format(9x,i3,' =',i2,' =',i2,13x,f7.4)                            0621PJ01

c
      end if
c 
c
c     *********************************************************************
c           write out data read in *TUNNEL section
c     *********************************************************************
c
      if (itunnl.ne.0) then
      
c
      write(fu6,7000)
7000  format(//,80('-'),/80('-'),//2x,' TUNNEL Section Parameters: ',
     *   //80('-')/)

c
c     print out switches
c
      write(fu6,7470)
7470  format(2x,'** Tunneling method:'/) 
c
      write(fu6,7480) swit(iwign+1)
      write(fu6,7490) swit(izct+1)
      write(fu6,7500) swit(isct+1)
      if(ilct.gt.0) then                                                0708JC00
          iflct1=0
          iflct2=0
          if(ilcgit.eq.1) iflct1=ilcgit
          if(ilcgit.eq.2) iflct2=ilcgit-1
         write(fu6,7520) swit(2)                                        0708JC00
         write(fu6,7525) swlct(ilct)                                    0708JC00
         write(fu6,7545) swit(ipdat+1)
         write(fu6,7530) swit(ipprob+1)
         write(fu6,7540) swit(ipfreq+1)
         write(fu6,7547) swit(ipgrid+1)
         write(fu6,7527) swit(iflct1+1)
         write(fu6,7528) swit(iflct2+1)
      end if
c
7480  format(4x,'WIGNER:  compute Wigner tunneling',21x,a3)
7490  format(4x,'ZCT:  compute zero curvature tunneling',16x,a3)
7500  format(4x,'SCT:  compute small curvature tunneling',15x,a3)
7520  format(4x,'LCT:  compute large curvature tunneling',15x,a3)
7525  format(4x,'      Large curvature tunneling version',15x,a4)       0708JC00
7527  format(4x,'LCT1D: 1D fit to a spline under tension',15x,a4)
7528  format(4x,'LCT2D: 2D fit to a spline under tension',15x,a4)
7530  format(4x,'PRPROB:  print uniformized probabilities',14x,a3)
7540  format(4x,'PRFREQ:  print probabilities, freq, and energies',
     *    6x,a3)
7545  format(4x,'LCTDETAIL:  print details of LCT calculations',        0517WH94
     *   9x,a3)
7547  format(4x,'LCTGRID: specify a grid for the LCT calculations ',
     *   5x,a3)
c
c     print out parameters
c
      write(fu6,7550)
7550  format(//2x,'** Tunneling Parameters:'/)
c
      write(fu6,7560) nq12,nq22 
      write(fu6,7570) nseg
      write(fu6,7580) nseg2
      if (pemin .ne. 0.0d0) then                                        0528WH94
         write(fu6,7590) pemin
      else                                                              0528WH94
         write(fu6,7591) 'middle'                                       0528WH94
      endif                                                             0528WH94
      if (isplne.eq.1) then
         write(fu6,7610)
      else
         write(fu6,7620) nlang
      end if
      if (ilct.gt.0) then                                               0708JC00
         write(fu6,7630)                                                0708JC00
         write(fu6,7635) swit(ilcstr+1)                                 0708JC00
         write(fu6,7640) swit(ilcrst+1)                                 0708JC00
         if (ilcgit.gt.0) then                                          1118BE05
           write(fu6,7642) swit(ilcgit)                                 1118BE05
         endif                                                          1118BE05
c        if(ilcgit.eq.1) write(fu6,7643) nsplic                         0507AR02
         write(fu6,7645) swit(ivavg+1)                                  0708JC00
         write(fu6,7650) nvef                                           0708JC00
         write(fu6,7655) ng                                             0708JC00
         write(fu6,7660) ngs0                                           0708JC00
         write(fu6,7665) intlct                                         0708JC00
         if (nexcit.ne.-1) then
            write(fu6,7670) nexcit                                      0708JC00
         else
            write(fu6,7675)                                             0708JC00
         end if
      end if
C 
C     print the LCT detail information options                         
C   
      if (ipdat .eq. 1) then                                            0502WH94
         write(fu6,7700)                                                0502WH94
         istate = 0                                                     0502WH94
         icount = 0                                                     0502WH94
110      if (inumi(istate+1) .ne. 0) then                               0502WH94
            write(fu6,7710) istate                                      0502WH94
            do 120 i = 1, inumi(istate+1)                               0502WH94
               write(fu6,7720) ilb(i,istate+1), iub(i,istate+1)         0502WH94
120         continue                                                    0502WH94
            icount = icount + 1                                         0502WH94
         endif                                                          0502WH94
         if (icount .lt. inums) then                                    0502WH94
            istate = istate + 1                                         0502WH94
            goto 110                                                    0502WH94
         endif                                                          0502WH94
      endif                                                             0502WH94
c
c     Quantized-state tunneling
c
      if (iqrst .ne. 0) then                                            0719WH94
         allocate(enlvrc(0:maxwkb))      
         write(fu6,7800)                                                0719WH94
         if (iqrst .eq. 1) then                                         0719WH94
             write(fu6,7810)                                            0719WH94
         else if (iqrst .eq. 2) then                                    0719WH94
             write(fu6,7820)                                            0719WH94
         else if (iqrst .eq. 3) then                                    0719WH94
             write(fu6,7830)                                            0719WH94
         endif                                                          0719WH94
         write(fu6,7890) iwr                                            0719WH94
      endif                                                             0719WH94
c 
7560  format(4x,'NQE,NQTH: quadrature parameters for tunneling',        06/96ELC
     * ' calcs.     ',i5,2x,i5)                                         06/96ELC
7570  format(4x,'NSEGBOLTZ:  number of segments for Boltzmann integral', 
     *   3x,i5) 
7580  format(4x,'NSEGTHETA:  number of segments for theta integral', 
     *   7x,i5) 
7590  format(4x,'EMIN:  max. energy in % tunneling',25x,f9.5) 
7591  format(4x,'EMIN:  max. energy in % tunneling',28x,a6)             0528WH94
7610  format(4x,'SPLINE:  spline fit for interpolating effective mass') 
7620  format(4x,'LAGRANGE:  order of interpolation for effective mass',
     *   4x,i5) 
c
7630  format(//2x,'** LCT Options:'/)                                   0708JC00
7635  format(4x,'LCTSTR:   store information in unit fu49',20x,a3)      0708JC00
7640  format(4x,'LCTRST:   restart LCT calculation from unit fu48',     0708JC00
     * 12x,a3)                                                          0708JC00
7642  format(4x,'ILCT:     interpolated LCT calculation',22x,a3)        0507AR02
7643  format(4x,'NILGPT:   interpolation points in a ILCT ',            0507AR02
     * 'calculation',8x,i3)                                             0507AR02
7645  format(4x,'VADAVG:   average of the overlaping adiabatic ',       0708JC00
     *  'potentials',4x,a3)                                             0708JC00
7650  format(4x,'VEFSRCH:  search for LCG4 nonadiabatic regions ',      0708JC00
     * 11x,i5)                                                          0708JC00
7655  format(4x,'NGTHETA:  number of quad. points for theta integral',  0708JC00
     *  7x,i5)
7660  format(4x,'NGAMP:    number of quad. points for total amplitude', 0708JC00
     *  6x,i5)                                                          0708JC00
7665  format(4x,'INTERPOLATE:  order of interpolation for LCT',         0708JC00
     * 14x,i5)
7670  format(4x,'NEXCIT:   tunneling into nth accessible state',        0708JC00
     * 13x,i5)                                                          0708JC00
7675  format(4x,'ALLEXCIT: tunneling into all available excited',       0708JC00
     * ' states')
C
7700  format(/4x,'Detail on the calculation of LCT tunneling into ')
7710  format( 4x,'state ',I3,' will be printed out for the following',
     *       /4x,'intervals of the energy grid:')
7720  format(30x,'from IE = ',I5,' to IE = ',I5)
7800  format(/4x,'Quantized reactant state tunneling with:')            0719WH94
7810  format( 8x,'harmonic potential')                                  0719WH94
7820  format( 8x,'harmonic-Va^G potential with WKB approximation')      06/96ELC
7830  format( 8x,'symmetric Va^G potential with WKB approximation')     06/96ELC
7890  format( 8x,'for the reaction-path mode ( mode ',I3,')')           0719WH94
c
      end if
c
c
c     *********************************************************************
c           write out data read in *RATE section
c     *********************************************************************
c
      if (irate.ne.0) then
      write(fu6,8500)
8500  format(//,80('-'),/80('-'),//2x,' RATE Section Parameters: ',
     *   //80('-')/)
c
c     print out switches
c
      write(fu6,8600)
8600  format(2x,'** Switches:'/)
c
      write(fu6,8610) swit(ifrate+1)
      write(fu6,8620) swit(ibrate+1)
      write(fu6,8630) swit(ngflag+1)
      write(fu6,8640) swit(iprg+1)
      write(fu6,8650) swit(iprigt+1)
      write(fu6,8651) swit(igtlog+1)                                    0423TA02
c
8610  format(4x,'FORWARDK:  compute forward rates',37x,a3)
8620  format(4x,'BOTHK:  compute forward and reverse rates',28x,a3)
8630  format(4x,'EDGEOK:  cont. calc. even if max VAG or DELG is',
     *  ' near edge of grid',4x,a3)
8640  format(4x,'PRDELG:  print free energy curve',37x,a3)
8650  format(4x,'PRGIGT:  print improved generalized free energy',
     *  ' curve',16x,a3)
8651  format(4x,'GTLOG:  use logarithms for calculating free ',
     *  'energy curve',13x,a3)
c
c     print out methods used 
      write(fu6,8660)
8660  format(//2x,'** Transition State Theory Methods:'/)
c
      write(fu6,8670) yswit(itst+1)
      write(fu6,8680) yswit(nfcvt+1)
      write(fu6,8685) yswit(nfcus+1)                                    1001PF97
      write(fu6,8690) yswit(icvt+1)
      write(fu6,8700) yswit(muvt+1)
      write(fu6,8704) yswit(iejmuvt+1)                                  0708JZ08
      write(fu6,8710) yswit(nfus+1)
c
8670  format(4x,'TST:  conventional transition state theory',10x,a3) 
8680  format(4x,'CVT:  canonical VTST',32x,a3) 
8685  format(4x,'CUS:  canonical unified statistical calculations',     1001PF97
     *       4x,a3)                                                     1001PF97
8690  format(4x,'ICVT:  improved canonical VTST',22x,a3)
8700  format(4x,'muVT:  microcanonical VTST',26x,a3)
8704  format(4X,'EJmuVT: E,J-resolved microcanonical VTST',12X,a3)      0708JZ08
8710  format(4x,'US:  unifed statistical calculations',16x,a3)
c
c     print out vibration perturbation treatment
      if (ipvib.eq.1) then
         write(fu6,8711)
          write(fu6,8713) swit(ipvibc+1)
          write(fu6,8715) swit(ipvibp+1)
      end if
8711  format(//2x,'** Perturbation treatment for Vib. Part. Func.:'/)
8713  format(4x,'CORIOLIS:  include coriolis term',20x,a3)
8715  format(4x,'PRINT:  extra data printed out',22x,a3)
c
c     print out parameters
c
      write(fu6,8720)
8720  format(//2x,'** Rate Calculation Parameters:'/)
c
      write(fu6,8730) nrev
      write(fu6,8740) sigmaf
      write(fu6,8750) sigmar
      write(fu6,8760) state
      if (iprt.ne.0) write(fu6,8770) prpart
c
8730  format(4x,'REVKEXP: scale reverse rate by this power of',         06/96ELC
     * ' ten',9x,i3)                                                    06/96ELC
8740  format(4x,'SIGMAF:  number of ways of forming forward GTS',
     *   11x,f4.0)
8750  format(4x,'SIGMAR:  number of ways of forming reverse GTS',
     *   11x,f4.0)
8760  format(4x,'STATE:  thermal or state-selected rate',20x,a5)
8770  format(4x,'PRPART:  print partition function for',21x,a3)
c
c     optionally print out muVT options
c
      if (muvt.eq.1) then
        write(fu6,8772) mniter
        write(fu6,8774) ifit1
        write(fu6,8776) ifit2
        write(fu6,8778) mnprmv
        if(xsmmvt.ne.0.0d0.or.xspmvt.ne.0.0d0)
     *        write(fu6,8779) xsmmvt/gufac6,xspmvt/gufac6               0405JZ07
      end if
8772  format(4x,'NITER:  number of Gauss-Leguerre quad pts',17x,i3)
8774  format(4x,'FIT1:  number of points in primary fit',20x,i3)
8776  format(4x,'FIT2:  number of points in secondary fit',18x,i3)
8778  format(4x,'PRENERGY:  number of energies to be printed',15x,i3)
8779  format(4x,'SLOWER & SUPPER: range on s for GT search',17x,f7.3,
     *        2x,f7.3)
c
c     print out temperatures
c
      write(fu6,8780)
8780  format(/2x,'** Temperatures (K):',/)                              06/96ELC
c
      do 5 i=1,ntemp
5         write(fu6,8790)temp(i)
8790  format(5x,f9.4)
c
c     print out analysis temperatures
c
      if (ianaly.ne.0) write(fu6,8800)
8800  format(//2x,'** Temperatures for the detailed analysis:'/)
c
      do 6 i=1,ianaly
6         write(fu6,8810)analt(i)
8810  format(5x,f9.4)
c
c     print out temperatures for activation energies
c
      if (ieact.ne.0) write(fu6,8820)
8820  format(//2x,'** Temperatures pairs for activation energies:'/)
c
      do 7 i=1,ieact
7         write(fu6,8830) etpair(i,1),etpair(i,2)
8830  format(5x,f9.4,4x,f9.4)
c
c     print out special free energy grid if option was chosen
c
      if (igspec.eq.1) then
         if(iunit6.eq.1) write(fu6,8840) slmg/gufac6,slpg/gufac6        0405JZ07
         if(iunit6.eq.0) write(fu6,8842) slmg/gufac6,slpg/gufac6        0405JZ07
      else
         write(fu6,8850)
      end if
8840  format(//2x,'** Free energy is computed for ',f9.6,               06/96ELC
     *  ' bohrs < = s < =',f9.6,' bohrs')                               06/96ELC
8842  format(//2x,'** Free energy is computed for ',f9.6,               0405JZ07
     *  ' angstroms < = s < =',f9.6,' angstroms')
8850  format(//2x,'** Free energy profile is computed for the',         06/96ELC
     * ' full reaction path')                                           06/96ELC
c
c     print out special temperature dependent free energy grid if option 
c     was chosen
c
      if (igtemp.ne.0) then
         do 8 i=1,igtemp
8           write(fu6,8860)gtemp(i),slma(i)/gufac6,slpa(i)/gufac6       0405JZ07
      end if
8860  format(//2x,'** At T = ',f9.4,' the Free Energy is computed for: ',
     *  //5x,f9.6,' < = s < = ',f9.6)
c
c     print out scale factors for partition functions 
c
      write(fu6,8870)iscale
8870  format(//2x,'** Reactant and Product partition functions are',
     *  ' scaled by: 10**',i3)                                          06/96ELC
c
c     print out region of s where extra vibrational mode and Qvib is
c     printed if option was chosen
c
      if (iprvib.ne.0) write(fu6,8880) sob/gufac6,soe/gufac6            0405JZ07
8880  format(//2x, '** Extra vibrational mode and Qvib is printed for:',
     *     //5x,f9.6,' < = s < = ',f9.6)
c
c
c     extra reaction-path output (fu25-fu27)
c
      if (iprmep .ne. 0) then                                           0719WH94
         write(fu6,9000)                                                0719WH94
         write(fu6,9010)                                                0719WH94
         if (iprmep .eq. 2) write(fu6,9020) nfrind                      0719WH94
         if (iprcd .eq. 1) write(fu6,9070) nprca                        0203YC98
         write(fu6,9060) itvmep                                         0719WH94
      endif                                                             0719WH94
c
9000  format(//2x,'** Extra output of reaction-path information **')    0719WH94
9010  format(/2x,'VMEP and Va^G will be written to fu25,')              06/96ELC
9020  format( 2x,'and ',I2,' GTS frequencies will be written to fu26,') 0719WH94
9050  format( 2x,'and GTS geometries will written to fu27 in XMOL xyz ',0719WH94
     *           'format,')                                             0719WH94
9060  format( 2x,'with an interval of ',I3,' save grid points')         0719WH94
      write(fu6,9990)
9070  format( 2x,'and ',I2,' internal coordinates will be',             0203YC98
     *                ' written to fu28,')                              0203YC98
9990  format(///,80('-'),/80('-'))
      end if
      return
      end subroutine intab
c
c ************************************************************************
c     intaba
c ************************************************************************
c
      subroutine intaba(jtype)
c
c     Subroutine to write the portion of the table detailing each of
c     the stationary point initial geometries if ACES geometry
c     optimization routines are used  (potgeo=aces).  This
c     code was written by R. Steckler on 4/25/95.
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c
      use common_inc; use kintcm, only : nvarj
      use perconparam, only : fu6
      use keyword_interface
      
      implicit double precision (a-h,o-z)

c
      write(fu6,1000)
1000  format(//2x,'** Geometry (Z-matrix):',/)
c
      do 10 i=1,nratmd(jtype)
10      write(fu6,1100) zmat(jtype,i)
1100  format(7x,a30)
c
      if (nvarj(jtype).gt.0) write(fu6,1200)
1200  format(/6x,'Z-matrix variables:')
c
      do 30 i=1,nvarj(jtype)
30      write(fu6,1300) avar(jtype,i)
1300  format(7x,a15)
c
      return
      end subroutine intaba
c
c ************************************************************************
c     intabg
c ************************************************************************
c
      subroutine intabg(jtype)
c
c     Subroutine to write the portion of the table detailing each of
c     the stationary point initial geometries if POLRATE geometry
c     optimization routines are used  (potgeo=polyrate).  This
c     code was moved from intabs by R. Steckler on 4/25/95.
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactant well                                          0728PF97
c     Jtype = 8  product well                                           0728PF97
c
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm, only : icnst,label
      implicit double precision (a-h,o-z)

      character * 3 swit(2)
      character * 3 optswt(2)
      character * 2 asymb(103)
c
      data swit /'off','on'/
      data optswt /'cst','   '/
      data (asymb(i),i=1,103)  /
     *   'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
     *   'Na','Mg','Al','Si','P ','S ','Cl','Ar',
     *   'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu',
     *        'Zn','Ga','Ge','As','Se','Br','Kr',
     *   'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     *        'Cd','In','Sn','Sb','Te','I ','Xe',
     *   'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb',
     *        'Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ','Re',
     *        'Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     *   'Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk',
     *        'Cf','Es','Fm','Md','No','Lr'/
c
c     write(fu6,2400)
      if(iunit6.eq.1) write(fu6,2400)                                   0405JZ07
      if(iunit6.eq.0) write(fu6,2410)                                   0405JZ07
2400  format(//2x,'** Geometry (a.u.):',/
     *   /3x,'Atom Number',4x,'Atomic Symbol',9x,'X',15x,'Y',15x,'Z')
2410  format(//2x,'** Geometry (angstrom):',/
     *   /3x,'Atom Number',4x,'Atomic Symbol',9x,'X',15x,'Y',15x,'Z')
c
      if (jtype.le.2) then
         k = 1
         ntot = nratom(jtype)
      else if (jtype.le.4) then
         k = 3
         ntot = nratom(jtype)
      else
         if (jtype.eq.5) k = 5                                          0729PF97
         if (jtype.eq.7) k = 7                                          0729PF97
         if (jtype.eq.8) k = 8                                          0729PF97
         ntot = natom
      end if
c
      do 1 i=1,ntot
         if (jtype.le.4) then
            inum = iatsv(i,jtype)
         else
            inum = i
         end if
         ilab = label(inum)
         write(fu6,2500)inum,asymb(ilab),
     *                  (xr((inum*3-j),k)/gufac6,j=2,0,-1)              0405JZ07
1     continue
c
2500  format(6x,i2,16x,a2,5x,f15.8,2x,f15.8,2x,f15.8)
c
c     if geometry is to be optimized and some coordinates will be
c     held constant - print out those coordinates
c
       if (istatu(jtype).eq.0) then                                     0725YC97
         if (ncnst(jtype).eq.0) then
            write(fu6,2600)
2600        format(//2x,'** All coordinates are optimized')
         else
            if (jtype.le.4) natm = nratom(jtype)
            if (jtype.ge.5) natm = natom
            write(fu6,2700)ncnst(jtype)
2700        format(//2x,'** ',I2,' Coordinates will be held',
     *       ' constant during the geometry optimization',
     *       //3x,
     *      'Atom Number',4x,'Atomic Symbol',7x,'X',14x,'Y',14x,'Z')
            do 2 i=1,natm
              if (jtype.le.4) then
                 inum = iatsv(i,jtype)
              else
                 inum = i
              end if
              ilab = label(inum)
              write(fu6,2800)inum,asymb(ilab),
     *            (optswt(icnst((inum*3-j),jtype)+1),j=2,0,-1)
2800          format(6x,i2,16x,a2,11x,a3,12x,a3,12x,a3)
2           continue
         end if
      end if
c
c     write out diatom information if species is a diatom
c
      if (ndiat(jtype).eq.1) then
         if(iunit6.eq.0) then                                           0405JZ07
           write(fu6,2900)re(jtype)/gufac6,be(jtype),de(jtype)
         else 
           write(fu6,2910)re(jtype)/gufac6,be(jtype),de(jtype)
         endif                                                          0405JZ07
      end if
2900  format(//2x,'** DIATOM:  '/,5x,'Diatomic morse constants:',/      0405JZ07
     * 6x, 'Re =   ',f10.6,3x,'Angstroms'/6x,'Beta = ',f10.6,/
     * 6x, 'De =   ',E14.6)
2910  format(//2x,'** DIATOM:  '/,5x,'Diatomic morse constants:',/
     * 6x, 'Re =   ',f10.6,3x,'Bohrs'/6x,'Beta = ',f10.6,/
     * 6x, 'De =   ',E14.6)                                             0405JZ07
      return
      end subroutine intabg
c
c ************************************************************************
c     intabs
c ************************************************************************
c
      subroutine intabs(jtype)
c
c     Subroutine to write the portion of the table detailing each of
c     the stationary point calculations with jtype specifying which
c     calculation is being done:
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactant well                                          0727PF97
c     Jtype = 8  product well                                           0727PF97
c
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm
      implicit double precision (a-h,o-z)
c
      character * 3 swit(2)
      character * 3 optswt(2)
      character * 2 asymb(103)
      character * 6 dir
c
      data swit /'off','on'/
      data optswt /'cst','   '/
      data (asymb(i),i=1,103)  /
     *   'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
     *   'Na','Mg','Al','Si','P ','S ','Cl','Ar',
     *   'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu',
     *        'Zn','Ga','Ge','As','Se','Br','Kr',
     *   'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     *        'Cd','In','Sn','Sb','Te','I ','Xe',
     *   'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb',
     *        'Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ','Re',
     *        'Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     *   'Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk',
     *        'Cf','Es','Fm','Md','No','Lr'/
c
c     print out switches
c
      write(fu6,1000)
1000  format(2x,'** Switches:'/) 
c
      write(fu6,1099) istatu(jtype)                                     0725YC97
      write(fu6,1300) swit(jxfreq(jtype)+1)
      if ((istatu(jtype).ne.0).and.(istatu(jtype).ne.4)) then           0807YC97 
         if (ifreu(jtype).eq.0) then                                    0807YC97
            write (fu6,1350) 'au   '                                    0807YC97
         else                                                           0807YC97
            write (fu6,1350) 'waven'                                    0807YC97
         endif                                                          0807YC97
      endif                                                             0807YC97
      write(fu6,1500) swit(iharm(jtype)+1)
      write(fu6,1510) swit(iproj(jtype)+1)
      write(fu6,1525) idmn(jtype)                                       0605PF97
      if (idmn(jtype).eq.'linrp'.or.idmn(jtype).eq.'lints'
     *               .or.idmn(jtype).eq.'linwell') then                 0729PF97
         if (iolin(jtype).eq.1) dir = 'x-axis'                          0507YC97
         if (iolin(jtype).eq.2) dir = 'y-axis'                          0507YC97
         if (iolin(jtype).eq.3) dir = 'z-axis'                          0507YC97
         write (fu6,1530) dir                                           0507YC97
      endif                                                             0507YC97
      if (initg(jtype).eq.0) then                                       0514PF97
           write (fu6,1540) 'geom '                                     0514PF97
      else                                                              0514PF97
           write (fu6,1540) 'hooks'                                     0514PF97
      endif                                                             0514PF97
      write (fu6,1550) swit(iprmd(jtype)+1)                             0317YC99
c
1099  format(4x,'STATUS: status of input info',32x,I1)                  0725YC97
1300  format(4x,'FREQ:  normal mode analysis',33x,a3)
1350  format(4x,'FREQUNIT: unit of the frequencies',27x,a5)             0807YC97
1500  format(4x,'HARMONIC:  harmonic approximation',27x,a3)
1510  format(4x,'PROJECT:  project overall trans. and rot.',19x,a3)
1525  format(4x,'SPECIES:  species type',37x,a8)                        0605PF97
1530  format(4x,'LINAXIS:  orientation of linear molecule',20X,A6)      0527PF97
1540  format(4x,'INITGEO:  initial geometry taken from',23x,a5)         0514PF97
1550  format(4X,'PRMODE:  print eigenvectors in Xmol form',20X,a3)      0317YC99
c
c     print out geometry and coordinates that will be optimized
c
      if (potgeo.eq.'polyrate') call intabg(jtype)                      0301YC97
      if (potgeo.eq.'aces') call intaba(jtype)                          0425RS95
c
      write(fu6,3000)
3000  format(/2x,'** Electronic energies:',/,2x,'Degeneracy',2x,        06/96ELC
     * 'Energy',1x,'(hartrees)')                                        06/96ELC
c
      jend = 3*jtype
      jbeg = jend-2
      do 3 k=jbeg,jend
3       write(fu6,3100) nedeg(k), elec(k)
3100  format(4x,i3,8x,f10.4)                                            06/96ELC
c
c     write out anharmonicity information
c
      if (iharm(jtype).eq.0) then
        write(fu6,3200)
3200    format(//2x,'** Anharmonicity:'/)
        
         if (imor(jtype).eq.1) then
            write(fu6,3300)mortyp(jtype),xdemin(jtype)
         end if
c
         if (imorqq(jtype).eq.1) then
            write(fu6,3400)mortyp(jtype),xantlr(jtype)
         else if (iqqwkb(jtype).eq.1.and.iunit6.eq.0) then              0405JZ07
            write(fu6,3500)xdqqp(jtype,1)/gufac6,xdqqp(jtype,2)/gufac6
         else if (iqqwkb(jtype).eq.1.and.iunit6.eq.1) then
            write(fu6,3510)xdqqp(jtype,1),xdqqp(jtype,2)
         else if (iqqsem(jtype).eq.1.and.iunit6.eq.0) then
            write(fu6,3600)xdqqp(jtype,1)/gufac6,xdqqp(jtype,2)/gufac6
         else if (iqqsem(jtype).eq.1.and.iunit6.eq.1) then
            write(fu6,3610)xdqqp(jtype,1),xdqqp(jtype,2)                0405JZ07
         else if (iwkb(jtype).eq.1) then
            write(fu6,4000)xwkbtl(jtype),ikbqua(jtype),
     *               swit(ikbprt(jtype)+1)
         endif
c
         n = nmodes(jtype)                                              0521YC99
         write(fu6,3700) n                                              0521YC99
         do i=1,nxmod(jtype)                                            0521YC99
           if (vharmr(i,jtype) .eq. 'tor      ') then                   0521YC99
             nx = ixmode(i,jtype)                                       0521YC99
c
c  scheme = 3, 5 for curvilinear method                                 0521YC99
c
            if (ntrsch(jtype,nx).eq.3.or.ntrsch(jtype,nx).eq.5.or.      1206BE05
     *            ntrsch(jtype,nx).eq.7) then                           1206BE05
                  write (fu6,3720)                                      0521YC99
                  write(fu6,3801) nx,                                   0521YC99
     *               ntrsig(jtype,nx,1),                                0521YC99
     *               ntrbnd(jtype,nx,1),ntrbnd(jtype,nx,2),             0521YC99
     *               (ntrisb(jtype,nx,k),                               0521YC99
     *               k=1,ntrnum(jtype,nx))                              0521YC99
c
c  scheme = 2, 4 for rectilinear method                                 0521YC99
c
             elseif (ntrsch(jtype,nx).eq.2.or.                          0521YC99
     *               ntrsch(jtype,nx).eq.4.or.                          1206BE05
     *               ntrsch(jtype,nx).eq.6) then                        1206BE05
                  write (fu6,3710)                                      0521YC99
                  write(fu6,3800) nx,                                   0521YC99
     *               ntrsig(jtype,nx,1),(ntrisb(jtype,nx,k),            0521YC99
     *               k=1,ntrnum(jtype,nx))                              0521YC99
             endif                                                      0521YC99
           write (fu6,3701) nx,ntrnb(jtype,nx),ntrsch(jtype,nx),        0521YC99
     *                      ntrlev(jtype,nx)                            0521YC99
c
c  This was printed out before anything was calculated, so half         0511BE05
c  of the numbers were meaningless.  Everything meaningful in this      0511BE05
c  table is already printed out, so it is now commented out.            0511BE05
c           write (fu6,3702)                                             0521YC99
c           write (fu6,3703)                                             0521YC99
c           write (fu6,3704)                                             0521YC99
c           do k = 1, ntrnb(jtype,nx)                                    0521YC99
c             write (fu6,3705)   k,ntrsig(jtype,nx,k),                   0521YC99
c     *               torome(jtype,nx,k)*AUTOCM,                         0521YC99
c     *               torw(jtype,nx,k)*AUTOCM,                           0521YC99
c     *               toru(jtype,nx,k)*AUTOCM,                           0521YC99
c     *               tormi(jtype,nx,k)
c           enddo                                                        0521YC99
c           write (fu6,3702)                                             0521YC99
c  End of changes from Ben Ellingson                                    0511BE05
           endif                                                        0521YC99
         enddo                                                          0521YC99
c
         if (ivary(jtype).eq.1) then                                    0513WH94
            write(fu6,4100)
            do 5 j=1,nxmod(jtype)
5              write(fu6,4200)ixmode(j,jtype),vharmr(j,jtype)           0513WH94
            if (imtyp(jtype).ne.0) write(fu6,4300) mortyp(jtype)
         endif
c
      end if
c
3300     format(3x,'MORSE:  ',a9,3x,' DEMIN = ',f10.4)
3400     format(3x,'MORSEQQ:  ',a9,3x,' ANTLR = ',e11.4)
3500     format(4x,' WKB using a quadratic-quartic fit to the potential'
     *   ,' at steps (angstroms):  ',/7x,2f10.6)
3510     format(4x,' WKB using a quadratic-quartic fit to the potential'
     *   ,' at steps (bohrs):  ',/7x,2f10.6)
3600     format(4x,' Semi-classical using a quadratic-quartic fit to',
     *   ' the potential at steps (angstroms):  ',/7x,2f10.6)
3610     format(4x,' Semi-classical using a quadratic-quartic fit to',
     *   ' the potential at steps (bohrs):  ',/7x,2f10.6)
3700     format(/2x,'The following ',i1,' modes will be treated using', 0513WH94
     *   ' the hindered rotor approximation:')
3701     format(/2x,'Mode ',i5,' has ',i5,' distinct minima along the', 0521YC99
     *   ' torsional coordinate.',/3x,'Scheme = ',i5,5x,                0521YC99
     *   'Level = ',i5)                                                 0521YC99
3702   FORMAT ( 1X,75(1H=))                                             0521YC99
3703   FORMAT ( 4x,'Minima',4x,'SIGMA',5x,'w',7x,'Barrier Height',7x,   0521YC99
     *         'U',10x,'I')                                             0521YC99 
3704   FORMAT ( 1X,8x,13x,'(cm-1)',8x,'(cm-1)',9x,'(cm-1)',6x,'(au^3)', 0521YC99
     *         /1X,75(1H-))                                             0521YC99
3705   FORMAT ( 3X,I5,3x,I5,3x,F9.3,5x,F9.3,7x,F9.3,2x,1PE15.6)         0521YC99
3710     format(/7x,'Mode Number',
     *     7x,'SIGMA(1)',7x,'Atoms in one subgroup')
3720     format(/7x,'Mode Number',      
     *     3x,'SIGMA(1)',4x,'with bond',7x,'Atoms in gyrator')          0317YC99
3801     format(8x,i3,11x,i3,8x,2i3,8x,10(i3))                          0317YC99
3800     format(8x,i3,14x,i3,10x,10(i3))
4000     format(4x,' WKB with tolerance of ',e10.3,' phase quadrature',
     *   ' points:  ',/7x,i4,' and the print flag is ',a3)
4100     format(/4x,' VANHAR - each mode will be treated as follows:',
     *          /4x,'            (modes not mentioned are harmonic)')
4200     format(6x,i5,5x,a10)                                           0317YC99
4300     format(6x,'Morse model used: ',a9)
c
      return
      end subroutine intabs
C
C**********************************************************************
C  OPT50
C**********************************************************************
C
      SUBROUTINE opt50
C
C     This subroutine checks the VTST-IC options which is in unit 50.
C   
C     This subroutine is rewritten for version 6.0 by Wei-Ping Hu 04/21/94
C
      use common_inc; use perconparam, only : fu6,ckcal
      use kintcm, only : idvmep
      use cm, only : spics,spicv,nspic
      use rate_const ; use keyword_interface
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C

C
      LOGICAL LEXIT
C
      LEXIT = .FALSE.
C
      I1 = LGSIC(1) / 10
      I2 = LGSIC(1) - I1 * 10
      IF (I1 .EQ. 2) THEN
         NFRP1 = NF(5)
      ELSE
         NFRP1 = NF(5) + 1
      ENDIF
      IF (I2 .EQ. 2) THEN
         NFRP2 = NF(5)
      ELSE
         NFRP2 = NF(5) + 1
      ENDIF
C
      IF (IVICE.NE.2) THEN
        WRITE(FU6,500)
      ELSE
        WRITE(FU6,501)
      ENDIF
C
500   FORMAT(//2X,'** VTST-IOC options:')
501   FORMAT(//2X,'** VTST-ISPE options:')
C
C     Check LGSIC(1)
C
      I1 = LGSIC(1) / 10                
      I2 = MOD(LGSIC(1),10)  
      IF (I1 .LE. 2) THEN 
         WRITE(FU6,1000) I1
      ELSE
         WRITE(FU6,1010) 
      ENDIF
      IF (I2 .LE. 2) THEN 
         WRITE(FU6,1100) I2
      ELSE
         WRITE(FU6,1110) 
      ENDIF
C
1000  FORMAT(/1X,'The correction on the reactant side will be based on',
     *           I2,' reactant species.')
1010  FORMAT(/1X,'The correction on the reactant side will be based on',
     *           ' a well.')
1100  FORMAT(/1X,'The correction on the product side will be based on',
     *           I2,' product species.')
1110  FORMAT(/1X,'The correction on the product side will be based on',
     *           ' a well.')
C
C     Check LGSIC(2)
C
      IF (LGSIC(2) .EQ. 0) THEN
         WRITE(FU6,1200)
      ELSE
         WRITE(FU6,1210)
         DO 50 I=NF(5),1,-1
            WRITE(FU6,1220) NF(5)+1-I,NFRP1+1-IFRR(I),NFRP2+1-IFRP(I)
50       CONTINUE 
      ENDIF
C
1200  FORMAT(/1X,'The frequencies will be matched in decreasing order')
1210  FORMAT(/1X,'The frequency matching is read from unit fu50:',
     *      /1X,'SP mode','    Reac-side mode','    Prod-side mode')
1220  FORMAT(1X,I4,3X,I11,7X,I11)
C
C
C     Check LGSIC(3)
C 
      IF (LGSIC(3).EQ.0) THEN          
         WRITE(FU6,1300)
      ELSE
         WRITE(FU6,1310)
      ENDIF
C
1300  FORMAT(/1X,'No correction is requsted for product frequencies')
1310  FORMAT(/1X,'The product frequencies will be corrected.')
C
C     Check LGSIC(4)
C
      IF (LGSIC(4) .EQ. 0) THEN 
         WRITE(FU6,1400) 
      ELSE
         WRITE(FU6,1410)
      ENDIF
C
1400  FORMAT(/1X,'No correction is requested for the moment of inertia', 
     *          ' tensor.')
1410  FORMAT(/1X,'The moment of inertia tensor will be corrected.')
C 
C     Check LGSIC(5)
C
      IF (LGSIC(4) .EQ. 1) THEN
         IF (LGSIC(5) .EQ. 0) THEN
            WRITE(FU6,1500) 'Atomic unit.     '
         ELSE IF (LGSIC(5) .EQ. 1) THEN
            WRITE(FU6,1500) 'SI unit.'
         ELSE IF (LGSIC(5) .EQ. 2) THEN
            WRITE(FU6,1500) 'C.G.S. unit.     '
         ELSE IF (LGSIC(5) .EQ. 3) THEN
            WRITE(FU6,1500) 'Scaled unit.     '
         ELSE IF (LGSIC(5) .EQ. 4) THEN
            WRITE(FU6,1500) 'Atomic mass unit.'
         ENDIF
      ENDIF
C
1500  FORMAT(/1X,'The unit used for the input of the determinant',
     *       /1X,'of moments of inertia in fu50 is ',A17)
C
C     Check LGSIC(6)
C
      IF (ABS(LGS(9)) .GE. 2) THEN
         IF (LGSIC(6) .EQ. 0) THEN
            WRITE(FU6,1600)
         ELSEIF (LGSIC(6) .EQ. 1) THEN
            WRITE(FU6,1610)
         ELSE
            WRITE(FU6,1620)
         ENDIF
      ENDIF
C
1600  FORMAT(/1X,'No correction will be made to the energy in the ',
     *       /1X,'nonadiabatic region in LCT calculations.')
1610  FORMAT(/1X,'Linear correction will be used to correct the energy',
     *       /1X,'in the nonadiabatic region in LCT calculations.')
1620  FORMAT(/1X,'Linear or quadratic correction will be used to ',
     *           'correct the energy ',
     *       /1X,'in the nonadiabatic region in LCT calculations.')
C
C     Check LGSIC(7)
C 
      IF (LGSIC(7) .NE. 0) WRITE(FU6,1700)(NF(5)+1-ICFR(I),I=1,LGSIC(7))
C
1700  FORMAT(/1X,'The following GTS mode frequencies will be ',
     *           'interpolated directly:',
     *      (/1X,10I4))
C
C     Check LGSIC(8)
C
      IF (LGSIC(8) .EQ. 1) WRITE(FU6,1800)
C
1800  FORMAT(/1X,'User-specified range parameters will be used in ',
     *           'the VTST-IOC calculations.') 
C
C     Check LGSIC(9)
C
      IF (LGSIC(9) .LE. 0) THEN
         WRITE (FU6,1900)
         IF (LGSIC(9) .LT. 0) WRITE(FU6,1910)  -LGSIC(9)
      ELSE
         WRITE(FU6,1920)
         IF (LGSIC(9) .LT. 99) WRITE(FU6,1910) LGSIC(9)
      ENDIF
C
 1900 FORMAT(/1X,'The reduced moments of inertia of the hindered rotor',
     *       /1X,'modes (if any) will be corrected.')
 1910 FORMAT(/1X,'The reduced moments of inertia of the last ',I2,
     *           ' hindered rotor modes',
     *     /1X,'along the MEP will be set to the saddle point values.')
 1920 FORMAT(/1X,'The reduced moments of inertia of the hindered rotor',
     *       /1X,'modes (if any) will not be corrected.')
C
C     Check LGSIC(10)
C
      IF (LGSIC(10) .EQ. 1) WRITE(FU6,2000)
C
2000  FORMAT(/1X,'Zero order IVTST calculations will be performed.')
C
      IF (LGSIC(11) .EQ. 1) WRITE(FU6,2010)                             0406WH95
      IF (LGSIC(12) .EQ. 1) WRITE(FU6,2020)                             0406WH95
C
2010  FORMAT(/1X,'No corrections on frequencies at the reactant well.') 0406WH95
2020  FORMAT(/1X,'No corrections on frequencies at the product well.')  0406WH95
      IF (IVICE.EQ.2.AND.NSPIC.NE.0) THEN                               0203YC98
         WRITE (FU6,1998)
         if (idvmep.eq.1) then                                          0317YC99
C          write (fu6,1999)                                             0317YC99
           if(iunit6.eq.1) write (fu6,1999)                             0405JZ07
           if(iunit6.eq.0) write (fu6,2100)                             0405JZ07
         else                                                           0317YC99
C          WRITE (FU6,2001)                                             0203YC98
           if(iunit6.eq.1) write (fu6,2001)                             0405JZ07
           if(iunit6.eq.0) write (fu6,2101)                             0405JZ07
         endif                                                          0317YC99
         WRITE (FU6,2002)                                               0203YC98
         DO I = 1,NSPIC                                                 0203YC98
C          WRITE (FU6,2003) SPICS(I),SPICV(I)*CKCAL                     0203YC98
           write (fu6,2003) SPICS(I)/GUFAC6,SPICV(I)*CKCAL              0405JZ07
         ENDDO                                                          0203YC98
      ENDIF                                                             0203YC98
1998  FORMAT(/1X,'ISPE - Interpolate with extra points:',/)             0601YC98
1999  FORMAT(11X,'     S(bohr)     DV(kcal/mol)')                       0317YC99
2001  FORMAT(11X,'     S(bohr)      V(kcal/mol)')                       0203YC98
2002  FORMAT(11X,'    --------     ------------')                       0203YC98
2003  FORMAT(11X,F12.3,5X,F12.4)                                        0203YC98
2100  FORMAT(11X,'   S(angstrom)    DV(kcal/mol)')                      0405JZ07
2101  FORMAT(11X,'   S(angstrom)     V(kcal/mol)')                      0405JZ07
C
C     Finished printing option list, now check compatibilities
C
      IF (IVICE.NE.2) THEN
        WRITE(FU6,4000)
      ELSE
        WRITE(FU6,4001)
      ENDIF
C
4000  FORMAT(/2X,'** VTST-IOC COMPATIBILITY CHECK')
4001  FORMAT(/2X,'** VTST-ISPE COMPATIBILITY CHECK')
C
C     CHECK LGSIC(1)
C
      IF ((LGS(3) .EQ. 0) .AND. ((I1 .EQ. 3) .OR. (I2 .EQ. 3))) THEN
         WRITE(FU6,4100)
         LEXIT = .TRUE.
      ENDIF
C
4100  FORMAT(/1X,'ERROR: MEPTYPER or MEPTYPEP should not be set to ',
     *           '"WELL" if there is',
     *       /1X,'       no *PATH section in unit fu5 input.')
C        
C     CHECK LGSIC(2)
C
      IF (LGS(3) .EQ. 0 .AND. LGSIC(2) .EQ. 1) THEN
         WRITE(FU6,4200)
         LEXIT = .TRUE.
      ENDIF
C      IF (LGSIC(2) .NE. 1 .AND. LGSIC(1) .NE. 22) THEN
C         WRITE(FU6,4210)
C         LEXIT = .TRUE.
C      ENDIF
4200  FORMAT(/1X,'ERROR: FREQMAT can not be used if MEP is not ', 
     *           'calculated.')
4210  FORMAT(/1X,'ERROR: FREQMAT must be used in this calculation.')
C
C     CHECK LGSIC(3)
C
      IF (LGSIC(3) .EQ. 0) THEN
         IF ((LGS(3) .NE. 0) .AND. (I2 .NE. 3) ) THEN 
            WRITE(FU6,4300)
            LEXIT = .TRUE.
         ENDIF     
      ENDIF
C
4300  FORMAT(/1X,'ERROR: NOPRODFREQ can not be used in this ',
     *           'calculation.')
C
C     No check for LGSIC(4)-LGSIC(9)
C
C     Check LGSIC(10)
C
      IF (LGSIC(10) .EQ. 1) THEN
         IF (LGS(9) .NE. -1) THEN
            WRITE(FU6,5000)
            LEXIT = .TRUE.
         ENDIF
         IF (LGSIC(1) .NE. 22) THEN
            WRITE(FU6,5010)
            LEXIT = .TRUE.
         ENDIF
         IF (LGS(6) .NE. 1) THEN
            WRITE(FU6,5020)
            LEXIT = .TRUE.
         ENDIF
      ENDIF
C
C     Check LGSIC(11)
C
      IF (LGSIC(11) .EQ. 1 .AND. I1 .NE. 3) THEN                        0406WH95
         WRITE(FU6,5100)                                                0406WH95
         LEXIT = .TRUE.                                                 0406WH95
      ENDIF                                                             0406WH95
C
C     Check LGSIC(12)
C
      IF (LGSIC(12) .EQ. 1 .AND. I2 .NE. 3) THEN                        0406WH95
         WRITE(FU6,5110)                                                0406WH95
         LEXIT = .TRUE.                                                 0406WH95
      ENDIF                                                             0406WH95
C
5000  FORMAT(/1X,'ERROR: 0IVTST can only be used if ZCT is the only ',
     *           'tunneling method',
     *       /1X,'       that is turned on in unit fu5 input.')
5010  FORMAT(/1X,'ERROR: 0IVTST can only be used if both MEPTYPER ',
     *           'and MEPTYPEP are set to "TWO".')         
5020  FORMAT(/1X,'ERROR: 0IVTST can only be used if there are two ',
     *           'reactant and',
     *       /1X,'       two product species in the reaction.')
C
5100  FORMAT(/1X,'ERROR: NOCORRECTION can only be used in the RCINFO ', 0406WH95
     *           'keyword only if MEPTYPER is set to "WELL".')
5110  FORMAT(/1X,'ERROR: NOCORRECTION can only be used in the PCINFO ', 0406WH95
     *           'keyword only if MEPTYPEP is set to "WELL".')
C
      IF (LEXIT) THEN
         WRITE(FU6,7000)
         STOP 'OPT50 1'
      ELSE
         WRITE(FU6,7010)
      ENDIF
C
7000  FORMAT(/1X,' COMPATIBILITY CHECK FAILED, PROGRAM STOPPED.')
7010  FORMAT(/1X,' COMPATIBILITY CHECK PASSED')
C 
      RETURN
C
      END SUBROUTINE opt50
c
c***********************************************************************
c  option
c***********************************************************************
c
c
c   include file added 15/08/91
c   All include files added 25/09/91, and argument list removed
c
      subroutine option
      use perconparam , only : fu6,natoms,npotpt,nsdm,natom
      use fu_40
      use common_inc
      use kintcm, only : nfcvt,iunxt,icvt,ilct,ipot,nptinf,iclasv,
     1mnprmv,ifrfac,ivic,isstop,itunnl,icheck,ivtst,muvt,mniter,
     2inxtpt,iexprd,iexrct,ipath,ispec,nregon,nfus,izct,isct,iharm,
     3irepr,jxfreq,iqqwkb,iqqsem,iwkb,imor,imorqq,istatu
      use keyword_interface
      use cm, only : ivrc,sdebg1,sdebg2
      use rate_const, only : inh,iqrst,del,ini
c
      implicit double precision (a-h,o-z)
c
      logical lexit,lnors                                               09/95KAN
      save                                                              0601YC98
c
c Check for consistency and write out information about options
c    selected.  Written Oct. '85.  BCG
c Rewritten for the 2.0 version May '88 TNT
c Rewritten for the 6.0 version Feb '94 RS
c
c     called by:
c                main
c
      lexit = .false.
c
      if (INI.gt.INH) then                                              0202YC98
         write (fu6,*) 'The value of INI is greater than INH,',         0202YC98
     *          ' It has been reset to the value of INH.'               0202YC98
         ini = inh                                                      0601YC98
      endif                                                             0202YC98
c
      if (lgs(12).ne.0.and.lgs(30).gt.0) then                           0205YC98
         write (fu6,*) 'The extrapolation option is not supported',     0205YC98
     *          ' with electronic structure input files'                0205YC98
         write (fu6,*) 'Reset the values of SLP and SLM.'               0205YC98
         lexit = .true.                                                 0205YC98
      endif                                                             0205YC98
c
c do not check if tst only
c
      if (nfcvt.eq.1) then                                              0317YC99
      ISAVEP =  DINT((SLP-SLM)/(DEL*DBLE(INH)))                         1105PF97
      if (ISAVEP.gt.NSDM) then                                          0729YC97
         write (fu6,*) 'ERROR !!! number of save grids is greater than' 0729YC97
     *               ,' allowed value;'                                 0729YC97
         write (fu6,*) 'you asked ',isavep,' save grids'                0729YC97
     *               ,' but only ',nsdm,' are allowed'                  0729YC97
         write (fu6,*) 're-compile POLYRATE with different param file'  0729YC97
         lexit = .true.                                                 0729YC97
      endif                                                             0729YC97
      endif                                                             0317YC99
c
c if an external field is present --- check that no anharmonicity is 
c being requested
c
      do 10 jtype=1,8                                                   0729PF97
        if (irepr(jtype).eq.1) then
           if (icode(jtype).lt.0) then                                  0606PF97
               if (iharm(jtype).ne.1) then
                   write (fu6,999)
                   write (fu6,1000)
                   lexit = .true. 
               endif
           endif
        endif
10    continue
c
c Restart -- the rest of the checks will not be done if this is a restart
c            calculation.
c
      lnors = iunxt.le.0
      if (lnors) then
c
c     calculating zero of energy ( i.e. sum of reactants' energies)
c     this is not allowed if there are no reactant and products
c
         if (cezero.eq.'calculate') then
            if (irepr(1).eq.0.and.irepr(2).eq.0.and.irepr(3).eq.0
     *             .and.irepr(4).eq.0) then
                if (.not.lexit) write (fu6,999)
                write (fu6, 1020) 
                lexit = .true.   
             endif               
         else if (cezero.ne.'read') then
            write (fu6,*) 'ERROR! ',cezero,' IS NOT AN OPTION FOR',
     *           ' EZERO KEYWORD'
            lexit = .true.
         endif
c
c MEP -- make sure rate or tunneling calculations are not requested
c        if the path is not computed
c
         if (ipath.eq.0) then
            if (nfcvt.gt.0.or.icvt.gt.0.or.muvt.gt.0.or.nfus.gt.0) then
               if (.not.lexit) write (fu6,999)
               write(fu6,1040)
               lexit = .true.       
            endif                  
            if (izct.gt.0.or.isct.gt.0.or.ilct.gt.0) then
               if (.not.lexit) write (fu6,999)
               write(fu6,1060)
               lexit = .true.       
            endif                  
          endif
c
c Integrator - check that the cubic starting algorithm is not
c              used with ab-inito data or if the saddle point
c              frequencies are not computed.
c
            if (fstep.eq.'cubic'.and.ipot.eq.1) then                    0327YC97
                  if (.not.lexit) write (fu6,999)
                  write(fu6,1080)
                  lexit = .true.
            end if
            if (fstep.eq.'cubic'.and.jxfreq(5).eq.0) then
                  if (.not.lexit) write (fu6,999)
                  write(fu6,1100)
                  lexit = .true.
             endif
c
c Special s values - check that they are not used with ab-initio data
c
            if (ispec.eq.1.and.ipot.eq.1) then                          0327YC97
                    if (.not.lexit) write (fu6,999)
                     write (fu6,1120)
                     lexit = .true.
            endif
c
c Anharmonicity - quardratic-quartic fit options cannot be used
c                 with ab-initio data and only the harmonic or
c                 hindered rotor can by used with the LCT tunneling
c
       do 20 jtype = 1,8                                                0729PF97
        if (iqqwkb(jtype).eq.1.or.iqqsem(jtype).eq.1) then
c
            if (ipot.eq.1) then                                         0327YC97
               if (.not.lexit) write (fu6,999)
               write (fu6,1140)
               lexit = .true.
            end if
            if (ilct.gt.0) then                                         0708JC00
               if (.not.lexit) write (fu6,999)
               write (fu6,1160)
               lexit = .true.
            end if
         end if
c
         if (iwkb(jtype).eq.1.) then
c
            if (ipot.eq.1) then                                         0327YC97
               if (.not.lexit) write (fu6,999)
               write (fu6,1180)
               lexit = .true.
            end if
            if (ilct.gt.0) then                                         0708JC00
               if (.not.lexit) write (fu6,999)
               write (fu6,1160)
               lexit = .true.
            end if
         endif
c
         if (imor(jtype).eq.1.or.imorqq(jtype).eq.1.) then
            if (ilct.gt.0) then                                         0708JC00
               if (.not.lexit) write (fu6,999)
               write (fu6,1160)
               lexit = .true.
            end if
         end if
c
c  Torsion paramters check                                              0326JZ10
C
c        do i = 1, nxmod(jtype)
c           mtol = 0
c           im = ixmode(i,jtype)
c           if(jtype.ne.5) then
c             itor = moder(jtype,im)
c           else 
c             itor = modets(1,im)
c           endif
c           if(itor.eq.9) then
c             nmin= ntrnb(jtype,6)
c             do imin = 1, nmin
c               mtol = mtol + ntrsig(jtype,im,imin)
c             enddo
c             if (mod(mtol,nmin).ne.0) then
c               write(fu6,*) 'Total system number is non-integer'
c    *          ,' for mode ',im   
c               stop
c             endif
c           endif
c        enddo

C SSTor option check
c
      if(coord .ne. 'curv2'.and.lsst.eq.1) then
         write(fu6,*)'Error: non-redundant internal coordinates should',
     *               ' be used for SS-T method'
         lexit = .true.
      endif

     

20     continue
c
        if (nregon.gt.0.and.ipot.eq.1) then                             0327YC97
          if (.not.lexit) write (fu6,999)
          write (fu6,1200)
          lexit = .true.
        end if
c
      end if
c
c The rest of the checking is done for all calculations - restart
c or otherwise
c
c ICVT - check that the cvt rate is computed if icvt is desired
c
      if (icvt.eq.1.and.nfcvt.eq.0) then
        if (.not.lexit) write (fu6,999)
        write (fu6,1220)
        lexit = .true.
      end if           
c
c Extrapolation - can not be used with LCT tunneling
c
      if ((iexrct.eq.1.or.iexprd.eq.1).and.ilct.gt.0) then              0708JC00
        if (.not.lexit) write (fu6,999)
        write (fu6,1240)                
        lexit = .true.                 
      end if
c
c
c State-selected - can only be used with 2 reactants and 2 products
c
      if (state.ne.'therm') then
            if (irepr(1).ne.1.or.irepr(2).ne.1.or.irepr(3).ne.1
     *            .or.irepr(4).ne.1) then
                if (.not.lexit) write (fu6,999)
                write (fu6,1260)
                lexit = .true.
            endif
      endif
c
c RPH - check that the nextpt option is only used with ab-initio data
c
c
      if (ipot.ne.1.and.inxtpt.eq.1) then                               0515PF97
         if (.not.lexit) write (fu6,999)
         write (fu6,1280)
         lexit = .true.
      endif
c
c Check size of data in parameter files for consistency
c
      if (nptinf.gt.npotpt) then
          if (.not.lexit) write (fu6,999)
          write (fu6, 1300) nptinf,npotpt
          lexit = .true.
      endif
c
c Check that the number of atoms is not larger than dimension statement
c
      if (natom.gt.natoms) then 
          lexit = .true.         
          if (.not.lexit) write (fu6,999)
          write (fu6, 1320)
      endif               
c
c If vibrational partition functions are classical, no anharmonicity    0528JC97
c is allowed                                                            0528JC97
c
       do jt=1,8                                                        0729PF97
         if (iharm(jt).ne.1.and.iclasv.eq.1) then                       0528JC97
             lexit = .true.                                             0528JC97
             if (.not.lexit) write (fu6,999)                            0528JC97
             write (fu6,1323)                                           0528JC97
         endif                                                          0528JC97
       enddo
c
c If it is a reaction with no saddle point, optimization of the         0210JC97
c starting geometry is not allowed                                      0210JC97
c
       if (lgs(1).eq.0.and.istatu(5).eq.0) then                         0725YC97
             lexit = .true.                                             0210JC97
             if (.not.lexit) write (fu6,999)                            0210JC97
             write (fu6,1325)                                           0210JC97
       endif                                                            0210JC97
c
c If it is a reaction with no saddle point, only firststep=gradient     0210JC97
c is allowed
c
       if (lgs(1).eq.0.and.fstep.ne.'gradi') then                       0210JC97
             lexit=.true.                                               0210JC97
             if (.not.lexit) write (fu6,999)                            0201JC97
             write (fu6,1327)                                           0201JC97
       endif                                                            0210JC97
c
c If the reaction has a saddle point, firststep=gradient,               0210JC97
c is not allowed
c
       if (lgs(1).ne.0.and.fstep.eq.'gradi') then                       0210JC97
             lexit=.true.                                               0210JC97
             if (.not.lexit) write (fu6,999)                            0210JC97
             write (fu6,1329)                                           0210JC97
       endif                                                            0210JC97

c
c Check muVT options for validity
c
      if (mnprmv.gt.6.or.mnprmv.gt.mniter) then
         if (.not.lexit) write (fu6,999)
         write(fu6,1330) mnprmv,mniter
         lexit = .true.
      end if
c
c Check if it is a unimolecular reaction if QRST is specified
c
      if (lgs(6) .lt. 3 .and. iqrst .ne. 0) then                        0812WH94
         write(fu6,1340)                                                0812WH94
         lexit = .true.                                                 0812WH94
      endif                                                             0812WH94
c     Internal Coordinates for bound state vibrations                   07/95KAN
      if (lgs2(39).eq.0)then                                            07/95KAN
          write(fu6,2130)                                               07/95KAN
      elseif (lgs2(39).eq.1.or.lgs2(39).eq.3.or.lgs2(39).eq.5)then      0626YC97
          write(fu6,2131)                                               07/95KAN
      elseif (lgs2(39).eq.2.or.lgs2(39).eq.4.or.lgs2(39).eq.6)then      0626YC97
          write(fu6,2132) sdebg1/gufac6,sdebg2/gufac6                   0405JZ07
      else if(lgs2(39).le.2.and.(nratom(1)+nratom(2)).gt.3) then        07/95KAN
         write(fu6,*) ' Option curv1 is only available for triatomics!' 07/95KAN
         stop                                                           07/95KAN
      else                                                              07/95KAN
          write(fu6,2133) lgs2(39)                                      07/95KAN
          lexit = .true.                                                07/95KAN
      endif                                                             07/95KAN
c
c Freqscale can only be used with TST and CVT rates                     0808JC00
c
      if (((icvt.eq.1).or.(muvt.eq.1)).and.(ifrfac.ne.0)) then          0808JC00
         write (fu6,1350)                                               0808JC00
         lexit = .true.                                                 0808JC00
      endif                                                             0808JC00
c
c Freqscale cannot   be used with IVTST or VTST-IC                      0808JC00
c
      if ((ivic.ne.0).and.(ifrfac.ne.0)) then                           0808JC00
         write (fu6,1355)                                               0808JC00
         lexit = .true.                                                 0808JC00
      endif                                                             0808JC00
c
c Freqscale can only be used with harmonic vibrations                   0808JC00
c
      if (ifrfac.ne.0) then                                             0808JC00
         do jtype=1,8                                                   0808JC00
           if (irepr(jtype).eq.1) then                                  0808JC00
               if (iharm(jtype).ne.1) then                              0808JC00
                   write (fu6,1360)                                     0808JC00
                   lexit = .true.                                       0808JC00
               endif                                                    0808JC00
           endif                                                        0808JC00
         enddo                                                          0808JC00
      endif                                                             0808JC00
c
c The SPECSTOP keyword cannot be used if the calculation uses a
c restart file, is an IVTST calculation (using fu29), or is a 
c calculation where information is read from or write to an
c electronic structure file.
c
      if ((isstop.ne.0).and.((iunxt.gt.0).or.(ivtst.eq.1).or.           0423TA02
     *    (iwrt30.eq.1).or.(ipot.gt.1))) then                           0423TA02
         write (fu6,1400)                                               0423TA02
         lexit = .true.                                                 0423TA02
      endif                                                             0423TA02
c
c VRC TST keywords related check
c
      if (ivrc.eq.1.and.itunnl.ne.0) then
         write(fu6,1500)
         lexit = .true.
      endif
      
c     if (iejmuvt.eq.1.and.ivrc.eq.0) then
c        write(fu6,1510)
c        lexit = .true.
c     elseif (itst.eq.1.and.ivrc.eq.1) then
c        write(fu6,1520)
c        lexit = .true.
c     endif
c
c Stop calculation if CHECK is switched on or if an error was encountered
c
      write (fu6,2000)
      if (icheck.eq.1) then
         write(fu6,2100)
         stop 'finished checking input'
      end if
c
      if (lexit) then
         stop 'option 1'
      endif
c
      return
c
 999  format (' ERRORS or invalid choices in fu5 input:')
 1000 format (' ***** Invalid -- Only the harmonic approximation is',
     *   ' valid for the case of the presence of an external field.')
 1020 format (//5X,10(1H*),'EZERO cannot be calculate if the reactant',
     *    ' and product properties are not input')
 1040 format (' ***** Only the TST rate constant can be computed if',
     *    ' the path is not computed')
 1060 format (' ***** ZCT, SCT, or LCT tunneling can not be computed',
     *    ' if the path is not computed')
 1080 format (' ***** Invalid options: Evaluation of the reaction',
     *   ' path at the saddle point within',/,' ***** the local cubic',
     *   ' approximation to the energy was selected, CUBIC,',
     *   /,' ***** but RPH information is being used;',
     *   ' therefore no potential is available.') 
 1100 format (' ***** Invalid options: Evaluation of the reaction',
     *   ' path at the saddle point within',/,' ***** the local cubic',
     *   ' approximation to the energy was selected,' 
     *   ,/,' ***** but the saddle point',
     *   ' imaginary frequency motion is not available, NOFREQ')
 1120 format (' ***** Invalid options: Special s values were selected,',
     *   /,' ***** but RPH information is being used;',
     *   ' therefore no potential is available.')
 1140 format (' ***** Invalid options: The anharmonicity option',
     *   ' selects a quadratic-quartic fit to the potential at',
     *   ' two points but RPH information is being used',
     *   ' ; therefore no potential is available.')
 1160 format (' ***** The LCT calculation is',
     * ' not available with any anharmonic option, ',  
     * 'except for the hindered-rotor anharmonic option')           
 1180 format (' ***** Invalid options: The WKB method with the',
     *   ' true potential was selected,', /,
     *   ' ***** but RPH information is being used;',
     *   ' therefore no potential is available.')
 1200 format (5x,'VRANGE cannot be used if MEP is not calculated.')
 1220 format (' ***** Invalid option: ICVT rate can not be computed',
     *    ' unless the CVT rate is computed.')
 1240 format (' ***** The LCT calculation is not available with',
     *    ' any extrapolation option at large abs(s)')
 1260 format (' ***** For the state-selected option, only',
     *   ' the case of 2 products and 2 reactants can be treated')
 1280 format (' ***** The NEXTPT keyword is available only with ',
     *   'electronic structure input data POTNAM=unit30).')
 1300 format(//,' ***** Magnitude of POTINF is larger than NPOTPT',
     *     'in this executable : ',/,15X,'POTINF = ',I5,/,15X,
     *     'NPOTPT = ', I5,/,7X,'NPOTPT can be increased by ',
     *     'changing the value in the param.inc file and recompiling',
     *     //)                                                       
 1320 format (/,3X, 2(1h*),'NATOM is larger than the number of',
     *        ' atoms (NATOMS) allowed by this executable',/,5x,
     *        ' NATOMS can be modified in the include file',
     *        ' param.inc',24(1H*)//)
 1323 format (/,1x,'CLASSVIB and ANHAMONICITY is not',                  0528JC97
     * ' supported in this',/,1x,'version of POLYRATE'//)               0528JC97
 1325 format (/,1x,'NOSADDLE and STATUS 0  in the START section is not',0729YC97
     * ' supported in this',/,1x,'version of POLYRATE'//)               0210JC97
 1327 format (/,1x,'With NOSADDLE only the GRADIENT option is',         0210JC97
     * ' allowed for the FIRSTSTEP keyword'//)                          0210JC97
 1329 format (/,1x,'The GRADIENT option for the FIRSTSTEP keyword',     0210JC97
     * ' is only allowed for reactions',/,1x,                           0210JC97
     * 'with no saddle point'//)                                        0210JC97
 1330 format (' ***** NPRENERGY is ',i3,' but it must be less than',
     *        ' or equal to the smaller of 6 and NITER',
     *        /' NITER was given as ',i3)
 1340 format (/1x,'QRST CAN ONLY BE USED IN A UNIMOLECULAR REACTION!')  0812WH94
 1350 format (/1x,'FREQSCALE CAN ONLY BE USED WITH TST AND CVT      ')  0808JC00
 1355 format (/1x,'FREQSCALE CANNOT BE USED WITH IVTST ot IVTST-IC  ')  0808JC00
 1360 format(/1x,'FREQSCALE CAN ONLY BE USED WITH HARMONIC VIBRATIONS') 0808JC00
 1400 format (/1x,'***** The SPECSTOP keyword should not be used ',     0423TA02
     *            'for a calculation involving a',                      0423TA02
     *        /7x,'restart file, an IVTST calculation, or a ',          0423TA02
     *            'calculation that uses ',                             0423TA02
     *        /7x,'electronic structure files.')                        0423TA02
 1500 format (/1x,'VRC CAN ONLY BE USED WITH NOTUNNEL       ')          0708JZ08
 1510 format (/1X,'EJmuVT CAN ONLY BE USED WITH VRC         ')          0708JZ08
 1520 format (/1X,'VRC CAN ONLY BE USED WITH NOTST          ')          0708JZ08
 2000 format (1x,79(1h*))                                               0519WH94
 2100 format ('POLYRATE stopped in option because CHECK was',
     * ' switched on')
 2130 FORMAT(5X,'Cartesian coordinates will be used for bound mode '    07/95KAN
     *   ,'vibrations.')                                                07/95KAN
 2131 FORMAT(5X,'Internal coordinates will be used for bound mode '     07/95KAN
     *   ,'vibrations.')                                                07/95KAN
 2132 FORMAT(5X,'Internal coordinates will be used for bound mode '     07/95KAN
     *   ,'vibrations, with debug output from s equal',F6.2,' to',F6.2) 07/95KAN
 2133 FORMAT(1X,'***** Invalid option: LGS2(39)= ',I5)                  07/95KAN
c
       end subroutine option
c
c ************************************************************************
c     RACES
c ************************************************************************
c
      subroutine races(string,istrt,jtype)
c
c     Subroutine to read in the ACES namelist options.  They
c     are stored in upper case as required by ACES.
c
c     jtype = 1  first reactant characterization
c     jtype = 2  second reactant characterization
c     jtype = 3  first product characterization
c     jtype = 4  second product characterization
c     jtype = 5  saddle point characterization
c     jtype = 6  generalized transition states characterizations
c     jtype = 7  energy calculations
c     jtype = 8  first derivative calculations
c     jtype = 9  second derivative calculations
c     jtype = 10 geometry optimizations (minimums)
c     jtype = 11 transition state geometry optimizations
c
      use common_inc; use perconparam, only : fu5,fu6
      use keyword_interface; use kintcm
      implicit double precision (a-h,o-z)
      character * 80 string,upcse
c
      call rline(fu5,string,istrt,isect,iend)
      string = upcse(string)
      iname = 1
      kline = 1
      if (jtype.le.6) then
         nlist(jtype,1) = '*ACES2('
         npos = 7
      else
         npos = 1
      end if
c
      do while(string(istrt:istrt+2).ne.'END')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  ACESOPT section must end with an END')
            stop
         end if
c
c        find length of parameter for concantenation
c
         k = istrt
         do while (string(k:k).ne.' ')
            k=k+1
         end do
c
c        add to namelist
c
         if (iname.eq.1) then
            if (npos.eq.1) then
               nlist(jtype,kline)= string(istrt:k-1)
               npos = (k-istrt)+npos-1
            else
                nlist(jtype,kline)=nlist(jtype,kline)(1:npos)//
     *                       string(istrt:k-1)
                npos = (k-istrt)+npos
            end if
         else 
            nlist(jtype,kline)=nlist(jtype,kline)(1:npos)//
     *                       ','//string(istrt:k-1)
            npos = (k-istrt)+npos+1
         end if
c
c        start a new line if necessary
c
         if (iname.eq.4) then
            kline = kline+1
            iname = 0
            npos = 1
         end if
         iname = iname + 1
c
c        check bounds on the length of namelist
c
         if (kline.gt.5) then
           write(6,1100)
1100       format(3x,'ERROR:  only 20 items are allowed in ACESOPT',
     *               ' if more are needed change the dimension of',
     *               ' nlist')
           stop
         end if
c
         call rline(fu5,string,istrt,isect,iend)
         string = upcse(string)
      end do
c
c     save number of lines in parameter list
      if (iname.eq.1) then                                              0522RS95
        nlistl(jtype) = kline-1                                         0522RS95
      else                                                              0522RS95
        nlistl(jtype) = kline                                           0522RS95
      end if                                                            0522RS95
c
      return
      end subroutine races
c
c ************************************************************************
c     ranaly
c ************************************************************************
c
      subroutine ranaly(string,istrt)
c
c     Subroutine to read in the temperatures at which a detailed analysis
c     is desired.
c
      use common_inc; use perconparam, only : fu5,fu6
      use kintcm, only : ianaly
      use cm, only : analt
      implicit double precision (a-h,o-z)
      character * 80 string
c
      ianaly = 0
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  analysis section must end with an END')
            stop
         end if
c
         ianaly = ianaly + 1
         if (ianaly.gt.40) then
           write(fu6,1010)
1010       format(3x,'ERROR:  max number of temperatures allowed is 40')
           stop
         end if
c
         analt(ianaly) = cfloat(string(istrt:80))
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine ranaly
c
c ************************************************************************
c     ratoms
c ************************************************************************
c
      subroutine ratoms(string,istrt)
      use common_inc
      use perconparam
      use rate_const
      use cm, only : label,xmass,xvdw
c
c     subroutine to read in atom labels and possible the masses.  If the
c     masses are not present then they will be looked up in a table of
c     the most common isoptopes (ams).
c
      implicit double precision (a-h,o-z)
c
      character * 80 string
      character * 2 asymb(103)
      dimension ams(103),avdw(103)
c
c     array of atomic masses
c
      data (ams(i),i=1,54)  /
     *   1.007825d+00,4.0026d+00,7.01600d+00,9.01218d+00,11.00931d+00,
     *   12.0d+00,14.00307d+00,15.99491d+00,18.99840d+00,19.99244d+00,
     *   22.9898d+00,23.98504d+00,26.98153d+00,27.97693d+00,
     *   30.97376d+00,31.97207d+00,34.96885d+00,39.948d+00,
     *   38.96371d+00,39.96259d+00,44.95592d+00,47.90d+00,50.9440d+00,
     *   51.9405d+00,54.9381d+00,55.9349d+00,58.9332d+00,57.9353d+00,
     *   62.9298d+00,63.9291d+00,68.9257d+00,73.9219d+00,74.9216d+00,
     *   79.9165d+00,78.9183d+00,83.9115d+00,
     *   84.9117d+00,87.9056d+00,89.9054d+00,89.9043d+00,92.9060d+00,
     *   97.9055d+00,97.0d+00,101.9037d+00,102.9048d+00,105.9032d+00,
     *   106.9041d+00,113.9036d+00,114.9041d+00,119.9022d+00,
     *   120.9038d+00,129.9067d+00,126.9044d+00,131.9042d+00/
      data (ams(i),i=55,103)  /
     *   132.9054d+00,137.9052d+00,138.9063d+00,139.9054d+00,
     *   140.9076d+00,141.9077d+00,144.9127d+00,151.9197d+00,
     *   152.9212d+00,157.9241d+00,158.9253d+00,163.9292d+00,
     *   164.9303d+00,165.9303d+00,168.9342d+00,173.9389d+00,
     *   174.9408d+00,179.9465d+00,180.9480d+00,183.9509d+00,
     *   186.9557d+00,191.9615d+00,192.9629d+00,194.9648d+00,
     *   196.9665d+00,201.9706d+00,
     *   204.9744d+00,207.9766d+00,208.9804d+00,208.9824d+00,
     *   209.9871d+00,222.0176d+00,
     *   223.0197d+00,226.0254d+00,
     *   227.0278d+00,232.0381d+00,231.0359d+00,238.0508d+00,
     *   237.0482d+00,244.0642d+00,243.0614d+00,247.0703d+00,
     *   247.0703d+00,251.0796d+00,252.0829d+00,257.0751d+00,
     *   258.0986d+00,259.1009d+00,260.1053d+00/
c
c     array of atomic symbols
c
      data (asymb(i),i=1,103)  /
     *   'h ','he','li','be','b ','c ','n ','o ','f ','ne', 
     *   'na','mg','al','si','p ','s ','cl','ar',
     *   'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',
     *        'zn','ga','ge','as','se','br','kr',
     *   'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',
     *        'cd','in','sn','sb','te','i ','xe',
     *   'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb',
     *        'dy','ho','er','tm','yb','lu','hf','ta','w ','re',
     *        'os','ir','pt','au','hg','tl','pb','bi','po','at','rn',
     *   'fr','ra','ac','th','pa','u','np','pu','am','cm','bk',
     *        'cf','es','fm','md','no','lr'/
c
c     array of van der Waals Radii (in Angstrom)
c
      data (avdw(i), i=1,54) /
     *  1.10d+00,1.40d+00,1.81d+00,1.53d+00,1.92d+00,1.70d+00,
     *  1.55d+00,1.52d+00,1.47d+00,1.54d+00,2.27d+00,1.73d+00,
     *  1.84d+00,2.10d+00,1.80d+00,1.80d+00,1.75d+00,1.88d+00,
     *  2.75d+00,2.31d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  1.87d+00,2.11d+00,1.85d+00,1.90d+00,1.83d+00,2.02d+00,
     *  3.03d+00,2.49d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  1.93d+00,2.17d+00,2.06d+00,2.06d+00,1.98d+00,2.16d+00/
c
      data (avdw(i), i=55,103) /
     *  3.43d+00,2.68d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,1.96d+00,2.02d+00,2.07d+00,1.97d+00,
     *  2.02d+00,2.20d+00,3.48d+00,2.83d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,2.00d+00,
     *  2.00d+00/
c 
c    convert avdw from Angstrom to Bohr
      avdw = avdw *1.88972652D0
c
c     read in each line in this set until an 'end' is found.  For
c     each line check if the atomic name or number is given.  If
c     an associated mass is not give assign the atom the mass of
c     the most common isotope.
c
      itot = 1
      call rline(fu5,string,istrt,isect,iend)
      do while (string(istrt:istrt+2).ne.'end')
c
c         first read in atom number
c
        ib = istrt
        call rword(string,istrt,ierr)
        ie = istrt - 1
        iatm = icint(string(ib:ie))
        call allocate1(iatm)
c
c              See if the first non-blank character is numeric; if so
c              assume atomic number was given.
         j = ichar(string(istrt:istrt)) - ichar('0')
         if (j.ge.0.and.j.le.9) then
             label(iatm) = icint(string(istrt:80))
c
c              check that atomic number is less than or equal to 103
             if (label(iatm).le.0.or.label(iatm).gt.103) then
                write(fu6,1100)label(iatm),string
1100            format(3x,'Incorrect atomic number:  ',i2,
     *             ' determined from the following line in file 5:'
     *             /1x,a80)
             end if
         else
c
c           for non-numeric assume it is the atomic symbol
c
             nfind = 0
             i = 1
             do while(nfind.eq.0.and.i.le.103)
               if (string(istrt:istrt+1).eq.asymb(i)) then
                   label(iatm) = i
                   nfind = 1
               end if
               i = i+1
             end do
c
             if (nfind.eq.0) then 
                write(fu6,1200)string(istrt:istrt+1)
                stop
             end if
1200         format(3x,'ERROR: illegal atomic symbol: ',A2)
         end if
c
c        find atomic masses -  search for mass after end of label
c
         call rword(string,istrt,ierr)
         if (ierr.eq.1) then
             xmass(iatm) = ams(label(iatm))
         else
             xmass(iatm) = cfloat(string(istrt:80))
         end if
c
         xvdw(iatm) = avdw(label(iatm))
c
c        read in next atom
c
         itot = itot+1
         call rline(fu5,string,istrt,isect,iend)
      end do
c
c     set number of atoms variable
c
      natom = itot - 1
      n3tm= 3 * natom; nvib = n3tm-1; n3tm1 = n3tm +1; n6tm = 6 * natom
      nvibm = n3tm - 1
      ncr2 = (natom*(natom+1))/2; n3s31 = 2 * n3tm + 5
c
      return
      end subroutine ratoms
c ***************************************************************************
c     rbath
c ***************************************************************************
c
      subroutine rbath(string,istrt)
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm
c
c     Subroutine to read in the bath parameters
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      call bath_mem
      do i = 1,n3tm
        diffu(i) = 0.0d0
      enddo
      frict = 0.0d0
      ircoup = 0
      batemp = 298.0d0
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
            stop 'rbath 1'
         end if
1000     format(3x,'ERROR:  BATH section must end with an END')
c
         if (string(istrt:istrt+7).eq.'friction') then
            call rword(string,istrt,ierr)
            if (ierr.ne.1) then
               frict = cfloat(string(istrt:80))
c
c frict time in fs
c
               frict = frict * 41.29411765d0
c 
            else
               write(fu6,*)' ERROR:  variable FRICTION must ',
     *                    'have an argument'
               stop 'rbath 2'
            endif
         elseif (string(istrt:istrt+8).eq.'diffusion') then
            ircoup = 0
            call rdiffu
         elseif (string(istrt:istrt+7).eq.'cuplcnst') then
            ircoup = 1
            call rdiffu
         elseif (string(istrt:istrt+7).eq.'bathtemp') then
            call rword(string,istrt,ierr)
            if (ierr.ne.1) then
               batemp = cfloat(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable BATHTEMP must ',
     *                     'have an argument'
               stop 'rbath 3'
            endif
         else
            write (fu6,1100) string(istrt:80)
            stop 'rbath 4'
         endif
c
         call rline(fu5,string,istrt,isect,iend)
      enddo
1100  format(/1X,'Unrecognized subkeyword in the BATH section',/A80)
      return
      end subroutine rbath
c
c ************************************************************************
c     rcnst
c ************************************************************************
c
      subroutine rcnst(string,istrt,jtype)
c
c     Subroutine to read in the coordinates that will be held constant
c     in any optimization of a stationary point geometry.
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactant well                                          0729PF97
c     Jtype = 8  product well                                           0729PF97
c
      use perconparam, only : fu5,fu6,natom,n3tm
      use kintcm, only : ncnst
      use cm, only : icnst
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     Initialize flag array - each element will be set to 1 if that
c     parameter is to be optimized
c
      if(.not.allocated(icnst))then
       allocate(icnst(natom*3,8)); icnst=0
      end if
      icnst(:,jtype)=1
      ncst = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end'.and.ncst.le.n3tm)
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  constant section must end with an END')
            stop
         end if
c
c        parse line to get atom number
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that an x y or z was also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  must specify x, y or z with atom number')
c
         iend = istrt - 1
         jatm = icint(string(ibegin:iend))
c
c        check that jatm is a legal atom number
c
         if (jatm.le.0.or.jatm.gt.natom) then
             write(fu6,1200) string
1200         format(3x,'ERROR:  invalid atom number in constant',
     *       ' Invalid line is: ',/1x,a80)
             stop
         end if
c
c        set constant flags to 0 for those coordinates to be held constant
c
         if (string(istrt:istrt).eq.'x') then
            icnst(jatm*3-2,jtype) = 0
         else if  (string(istrt:istrt).eq.'y') then
            icnst(jatm*3-1,jtype) = 0
         else if  (string(istrt:istrt).eq.'z') then
            icnst(jatm*3,jtype) = 0
         end if
         ncst = ncst+1
c
c        check if a second and possibly a third coordinate for this
c        atom is also to be held constant
c
         call rword(string,istrt,ierr)
         if (ierr.eq.0) then
           ncst = ncst+1
           if (string(istrt:istrt).eq.'x') then
              icnst(jatm*3-2,jtype) = 0
           else if  (string(istrt:istrt).eq.'y') then
              icnst(jatm*3-1,jtype) = 0
           else if  (string(istrt:istrt).eq.'z') then
              icnst(jatm*3,jtype) = 0
           end if
           call rword(string,istrt,ierr)
           if (ierr.eq.0) then
              ncst = ncst+1
              if (string(istrt:istrt).eq.'x') then
                 icnst(jatm*3-2,jtype) = 0
              else if  (string(istrt:istrt).eq.'y') then
                 icnst(jatm*3-1,jtype) = 0
              else if  (string(istrt:istrt).eq.'z') then
                 icnst(jatm*3,jtype) = 0
              end if
           end if
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      ncnst(jtype) = ncst - 1 
      return
      end subroutine rcnst
C
C**********************************************************************
C RDETIL
C**********************************************************************
C
      SUBROUTINE rdetil
C
C     Read the LCG3 detail output information
C     
C     The input was used to be from unit fu40, in POLYRATE 6.0
C     it is moved to unit fu5
C

C
      use common_inc; use perconparam, only : fu5,fu6
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER*80 WORD(40)
      LOGICAL LSEC,LEOF
C
      INUMS = 0
C
10    CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)
      IF (WORD(1) .NE. 'END') THEN
         IF (LSEC .OR. LEOF) THEN
            WRITE(FU6,3000) 
            STOP 'RDETIL 1'
         ENDIF
C
3000  FORMAT(/1X,'The LCTDETAIL section must end with an END')
C
         IF (WORD(1) .NE. 'STATE' .OR. NWORD .NE. 2) THEN
            WRITE(FU6,3200)
            STOP 'RDETIL 2'
         ELSE
            ISTATE = ICINT(WORD(2))
            INUMS = INUMS + 1
         ENDIF 
C
3200  FORMAT(/1X,'The following line is expected in LCTDETAIL section:',
     *       /1X,'STATE n, where n is 0 or a positive integer.')
3300  FORMAT(/1X,'The first line after STATE must be:',
     *       /1X,'INTERVAL n, where n is a positive integer.')
3400  FORMAT(/1X,'A line with two numbers are expected after INTERVAL.')
C
         CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)
         IF (WORD(1) .NE. 'INTERVAL' .OR. NWORD .NE. 2) THEN
            WRITE(FU6,3300)
            STOP 'RDETIL 3'
         ELSE
            INTVAL = ICINT(WORD(2))
            INUMI(ISTATE+1) = INTVAL
         ENDIF 
         DO 30 J = 1, INTVAL
            CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)
            IF (NWORD .NE. 2) THEN
               WRITE(FU6,3400)
               STOP 'RDETIL 4'
            ELSE
               ILB(J,ISTATE+1) = ICINT(WORD(1))
               IUB(J,ISTATE+1) = ICINT(WORD(2))
            ENDIF
30       CONTINUE
C
         GOTO 10
C
      ENDIF
C
      RETURN
C
      END subroutine rdetil
C
c
c ************************************************************************
c     rdiatm
c ************************************************************************
c
      subroutine rdiatm(string,istrt,jtype)
c
c     subroutine to read in the diatomic options for stationary points.
c     the stationary section.  Since it is a list variable the data must
c     concluded with an 'end'. 
c
      use common_inc; use perconparam, only : fu5,fu6
      use kintcm, only : ndiat
      use keyword_interface, only : gufac5
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     diatomic option is not valid for the saddle point - check that
c     jtype is not 5
c
      if (jtype.eq.5) then
        write(fu6,1000)
1000    format(3x,'ERROR:  diatom is not valid for the saddle point')
        stop
      end if
c
c     set diatomic flag
c
      ndiat(jtype) = 1
c
c     read diatomic options until an end is found
c
      call rline(fu5,string,istrt,isect,iend)
      do while (string(istrt:istrt+2).ne.'end')
         if (string(istrt:istrt+1).eq.'re') then
            call rword(string,istrt,ierr)
            re(jtype) = cfloat(string(istrt:80))*gufac5                 0405JZ07
         else if (string(istrt:istrt+3).eq.'beta') then
            call rword(string,istrt,ierr)
            be(jtype) = cfloat(string(istrt:80)) 
         else if (string(istrt:istrt+1).eq.'de') then
            call rword(string,istrt,ierr)
            de(jtype) = cfloat(string(istrt:80))
c                              check if list was none of the above - error
         else
             write(fu6,1100)string(istrt:80)
1100         format(' ERROR: illegal argument to diatom -',/1x,a80) 
             stop
         end if
         call rline(fu5,string,istrt,isect,iend)
c
c                               check if end of file was found - an error
         if (iend.eq.1) then
            write(fu6,*)'ERROR: diatom list must end with an END'
            stop
         end if
      end do
c
      return
      end subroutine rdiatm
c***********************************************************************
c     rdiffu
c***********************************************************************
c
      subroutine rdiffu
c
c     called by:
c           rbath
c
      use common_inc; use perconparam, only : fu5,fu6
      use kintcm, only : ndiat,ircoup
      use cm
      use keyword_interface, only : gufac5
      implicit double precision (a-h,o-z)
      character*80 word(40)
      logical lsec,leof
c
      itot = 0
10    call readln(fu5,word,nword,lsec,leof)
      if (word(1) .ne. 'END') then
         if (lsec .or. leof) then
           if (ircoup.eq.0)  write(fu6,1000)
           if (ircoup.eq.1)  write(fu6,999)
            stop 'RDIFFU 1'
         endif
c
1000  format(/1X,'The DIFFUSION must end with an END')
 999  format(/1X,'The CUPLCNST must end with an END')
c
         if (nword.eq.2) then
            index = icint(word(1))
            IF (IRCOUP.EQ.1) THEN
              DO I = 1,3
                DIFFU(3*(INDEX-1)+I) = cfloat(word(2))
              ENDDO
            ELSE
              DO I = 1,3
c
c read in 10-5 cm^2 s^-1 unit
c
                DIFFU(3*(INDEX-1)+I) = cfloat(word(2))*0.8647D-5
              ENDDO
            ENDIF
            itot = itot + 1
         else
           if (ircoup.eq.0)  write(fu6,1100)
           if (ircoup.eq.1)  write(fu6,1101)
            stop 'RDIFFU 2'
         endif
         goto 10
      endif
c
1001  format(/1X,'wrong format')
1100  format(/1X,'Unrecognized subkeyword in DIFFUSION')
1101  format(/1X,'Unrecognized subkeyword in CUPLCNST')
      return
      end subroutine rdiffu
C
C**********************************************************************
C RDLN
C**********************************************************************
C
C     This subroutine is an exact copy of readln, except that it now
C     accepts STRING as a parameter and returns it.                     1020BE05
C
      SUBROUTINE rdln(IUNIT,WORD,NWORD,LSEC,LEOF,STRING)
C
      IMPLICIT NONE   
C
      CHARACTER*80  LINE,SENAME,STRING,UPCSE
      CHARACTER*80  WORD(40)
      INTEGER       ICOM, IP, IWORDSTART, NWORD, IWS, IUNIT
      LOGICAL       LEOF,LSEC,LBLANK
      LOGICAL       LENDWORD, LNEXT
 
C     ICOM is the location of the comment character if it exists
      
C     IP is the index for the position currently being processed in
C     the line.
 
C     IWORDSTART is the position where the first word starts
 
C     LBLANK is true as soon as it is confirmed that there are
C     characters on the current working line.
      
      LSEC   = .FALSE.
      LEOF   = .FALSE.
      LBLANK = .TRUE. 
      NWORD  = 0
      
      DO WHILE (LBLANK)
        READ(IUNIT,'(A80)',END=500) STRING
        IP = 1
        DO WHILE (IP .LE. 80 .AND. STRING(IP:IP) .EQ. ' ')
          IP = IP + 1
        END DO
        IF (IP.LT.80.AND.STRING(IP:IP) .NE. '#')  LBLANK = .FALSE.
      END DO
      IWORDSTART = IP
      
C     Determine if comment exists later in the line, and record its
C     position in ICOM if there is.
      
      ICOM = IP
      DO WHILE (IP .LE. 80 .AND. STRING(IP:IP) .NE. '#')
        IP = IP + 1
        ICOM = IP
        if(ip>80)exit
      END DO

C     ICOM will be 81 if no comment card exists

      LINE = UPCSE(STRING)

      IWS = IWORDSTART
      LENDWORD = .FALSE.
      LNEXT=.TRUE.
      DO 10, IP = IWORDSTART, ICOM-1                                    1019BE05
      
        IF (LINE(IP:IP) .EQ. ' ' .AND. LNEXT) THEN
          NWORD = NWORD + 1
          WORD(NWORD)= LINE(IWS:IP-1)
          LENDWORD = .TRUE.
          LNEXT =    .FALSE.
        ELSE IF (LINE(IP:IP) .NE. ' ') THEN
          LNEXT=.TRUE.
          IF (LENDWORD) THEN
            IWS = IP
            LENDWORD = .FALSE.
          ENDIF
        END IF
      
10    END DO
      
      IF (WORD(1)(1:1) .EQ. '*') THEN
         SENAME = WORD(1)(2:)
         WORD(1) = SENAME
         LSEC = .TRUE.
      ENDIF

      RETURN

500   LEOF = .TRUE.

      RETURN
      
      END SUBROUTINE rdln
c
c ***************************************************************************
c     read5
c ***************************************************************************

      subroutine read5(PARALLEL)
      use common_inc
      use perconparam
      use kintcm
c
c     Subroutine to read in the input for polyrate version 6.0 and later.
c     This subroutine was completely re-written to handle keyword input.
c     The input is read in using the subroutine rline.  Parsing of information
c     is done in a separate subroutine for each section.
c
c      implicit double precision (a-h,o-z)
      character * 80 string
      LOGICAL PARALLEL
c
c     initialize variables and set defaults
c
c
      call default(PARALLEL)
c
c
c     start off by finding the first section given in the input deck
c
      call rline(fu5,string,istrt,isect,iend)
c
c     error check - check that the file wasn't empty and that the 
c     first line found was the start of a section
c
      if (iend.eq.1) then
         write(fu6,1000)
1000     format(1x,'error:  input file was empty')
         stop
      end if
c
      if (isect.ne.1) then
         write(fu6,1100)
1100     format(1x,'error:  first non-comment, non-blank line must be'
     *             ,' a section header that begins with a *')
         stop
      end if
c
c
c     Call the correct routine to read in the data in this section.  
c     control will come back here once another section header has
c     been found.  This will continue until an end-of-file mark
c     has been encoutered.
c
      do while (iend.eq.0)
         j = istrt + 1
         if (string(j:j+5).eq.'genera') then
               call rgener(string,iend,istrt)
         else if (string(j:j+9).eq.'energetics') then
               call renerg(string,iend,istrt)
         else if (string(j:j+5).eq.'second') then
               call rsecnd(string,iend,istrt)
         else if (string(j:j+11).eq.'optimization') then
               call ropt(string,iend,istrt)
         else if (string(j:j+5).eq.'react1') then
               irepr(1) = 1
               call rstat(string,iend,istrt,1)
         else if (string(j:j+5).eq.'react2') then
               irepr(2) = 1
               call rstat(string,iend,istrt,2)
         else if (string(j:j+4).eq.'prod1') then
               irepr(3) = 1
               call rstat(string,iend,istrt,3)
         else if (string(j:j+4).eq.'prod2') then
               irepr(4) = 1
               call rstat(string,iend,istrt,4)
         else if (string(j:j+4).eq.'start') then
               irepr(5) = 1
               call rstat(string,iend,istrt,5)
         else if (string(j:j+4).eq.'wellr') then
               irepr(7) = 1
               call rstat(string,iend,istrt,7)
         else if (string(j:j+4).eq.'wellp') then
               irepr(8) = 1
               call rstat(string,iend,istrt,8)
         else if (string(j:j+3).eq.'path') then
               call rpath(string,iend,istrt,PARALLEL)
         else if (string(j:j+5).eq.'tunnel') then
               call rtunnl(string,iend,istrt)
         else if (string(j:j+3).eq.'rate') then
               call rrate(string,iend,istrt)
         else
               write(fu6,1200) string(j-1:80)
               stop
         end if
      end do
c
1200  format(3x,'Error:  the following is not a valid section name',
     *         /A80)
c
c     print out a table of all input parameters
c
      call intab
      return
      end subroutine read5
C
C**********************************************************************
C READIC
C**********************************************************************
C
      SUBROUTINE readic
C
C     This subroutine reads in the VTST-IC information in keyword format.
C     It is used as a replacement of the old read50 subroutine in version
C     5.0-5.1.
C
C     This subroutine uses the new subroutine READLN
C     to read in the keywords instead of the combination of
C     RLINE and RWORD.
C

C 
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use rate_const; use potmod
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER*80  WORD(40)
      DIMENSION TEMFQ(N3TM),FACTOR(5)
      DIMENSION ITEM(N3TM)
      LOGICAL LSEC,LEOF,LWELLR,LWELLP,LFINTR,LFINTP
C
C     Specify the default
C
      call readic_mem
      call zucupd_mem
      IMEPR = 2
      IMEPP = 2
      LWELLR = .FALSE.
      LFINTR = .FALSE.
      LWELLP = .FALSE.
      LFINTP = .FALSE.
      IMATCH = 0
      IPROD = 1
      IDETMI = 0
      IDUNIT = 0
      IRANGE = 0
      IHRRMI = 99
      IVTST = 0
      INADIB = 2                                                        0824YC98
      NLFREQ = 0
      NOCR = 0                                                          0406WH95
      NOCP = 0                                                          0406WH95
      IDVMEP = 0                                                        0317YC99 
      NFRS = NF(5) 
      NFRR = NF(1) + NF(2)
      NFRP = NF(3) + NF(4)
      if (ibathm.eq.1) then                                             0317YC99
        NFRS = NFRS + 1                                                 0317YC99 
      endif                                                             0317YC99
      NZFRR = MAX0(NFRS - NFRR,0)
      NZFRP = MAX0(NFRS - NFRP,0)
      NFRP1 = NFRS
      NFRP2 = NFRS
C
      DO 10 I = 1, NFRS
         IFRR(I) = I
         IFRP(I) = I
10    CONTINUE
C
C     Set default for L, BR, and BP
C
      RANGE = 1.0D0
      BV1 = 5.0D0
      BV2 = 5.0D0
C
C     Read in the section name
C
      IF (IVIC.EQ.1) INPF = FU50                                        0605YC98
      IF (IVIC.EQ.2) INPF = FU51                                        0605YC98
C
      CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
      IF (IVIC.EQ.1.AND.(.NOT.LSEC.OR.WORD(1).NE.'VTSTIC')) THEN
         WRITE(FU6,6000)
         STOP 'READIC 0'
      ENDIF
      IF (IVIC.EQ.2.AND.(.NOT.LSEC.OR.WORD(1).NE.'ISPEGEN')) THEN
         WRITE(FU6,6001)
         STOP 'READIC 0'
      ENDIF
C
C     Read in the first keyword
C 
      CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
C
100   IF (.NOT. LSEC .AND. .NOT. LEOF) THEN
C        
C MEPTYPER, type of MEP on the reactant side
C     
         IF (WORD(1) .EQ. 'MEPTYPER') THEN
            IF (NWORD .GE. 2) THEN
               IF (WORD(2) .EQ. 'ONE') THEN
                  IMEPR = 1
               ELSEIF (WORD(2) .EQ. 'TWO') THEN
                  IMEPR = 2
               ELSEIF (WORD(2) .EQ. 'WELL') THEN
                  IMEPR = 3
               ELSE
                  WRITE(FU6,5500) 'MEPTYPER'
                  STOP 'READIC 1'
               ENDIF
               LWELLR = IMEPR .EQ. 3
               LFINTR = IMEPR .NE. 2
               IF (LFINTR) NFRP1 = NFRP1 + 1
            ELSE
               WRITE(FU6,5200) 'MEPTYPER',1
               STOP 'READIC 2'
            ENDIF 
C
C MEPTYPEP, type of MEP on the product side
C
         ELSEIF (WORD(1) .EQ. 'MEPTYPEP') THEN
            IF (NWORD .GE. 2) THEN
               IF (WORD(2) .EQ. 'ONE') THEN
                  IMEPP = 1
               ELSEIF (WORD(2) .EQ. 'TWO') THEN
                  IMEPP = 2
               ELSEIF (WORD(2) .EQ. 'WELL') THEN
                  IMEPP = 3
               ELSE
                  WRITE(FU6,5500) 'MEPTYPEP' 
                  STOP 'READIC 3'
               ENDIF
               LWELLP = IMEPP .EQ. 3
               LFINTP = IMEPP .NE. 2
               IF (LFINTP) NFRP2 = NFRP2 + 1
            ELSE
               WRITE(FU6,5200) 'MEPTYPEP',1
               STOP 'READIC 4'
            ENDIF 
C
         
C
C FREQMAT, manually match the saddle point frequencies to the two stationary
C          point frequencies
C
         ELSEIF (WORD(1) .EQ. 'FREQMAT') THEN
            IMATCH = 1
200         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'FREQMAT'
                  STOP 'READIC 5'
               ENDIF
               IF (NWORD .LT. 3) THEN
                  WRITE(FU6,3100) 'FREQMAT',3
                  STOP 'READIC 6'
               ELSE
                  IMODES = ICINT(WORD(1))
                  IMODER = ICINT(WORD(2))
                  IMODEP = ICINT(WORD(3))
                  IFRR(NFRS+1-IMODES) = NFRP1 + 1 - IMODER
                  IFRP(NFRS+1-IMODES) = NFRP2 + 1 - IMODEP
               ENDIF
               GOTO 200
            ENDIF
C
C NOFREQMAT
C
         ELSEIF (WORD(1) .EQ. 'NOFREQMAT') THEN 
            IMATCH = 0
C
C REACFREQ
C
         ELSEIF (WORD(1) .EQ. 'REACFREQ') THEN
            NFREQ = 0
300         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'REACFREQ'
                  STOP 'READIC 7'
               ENDIF
               DO 310 IM = 1,NWORD
                  TEMFQ(IM+NFREQ) = CFLOAT(WORD(IM))
310            CONTINUE
               NFREQ = NFREQ + NWORD
               GOTO 300
            ELSEIF (NFREQ .NE. NFRR) THEN
               WRITE(FU6,3200) NFRR,'REACFREQ',NFREQ
               STOP  'READIC 8'
            ENDIF
            DO 320 IM = 1,NFRR
               WERA(NFRR+1-IM) = TEMFQ(IM)
320         CONTINUE
C
C PRODFREQ
C
         ELSEIF (WORD(1) .EQ. 'PRODFREQ') THEN
            IPROD = 1
            NFREQ = 0
400         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'PRODFREQ'
                  STOP 'READIC 9'
               ENDIF
               DO 410 IM = 1,NWORD
                  TEMFQ(IM+NFREQ) = CFLOAT(WORD(IM))
410            CONTINUE
               NFREQ = NFREQ + NWORD
               GOTO 400
            ELSEIF (NFREQ .NE. NFRP) THEN
               WRITE(FU6,3200) NFRP,'PRODFREQ',NFREQ
               STOP  'READIC 10'
            ENDIF
            DO 420 IM = 1,NFRP
               WERA(NFRR+NFRP+1-IM) = TEMFQ(IM)
420         CONTINUE
C
C NOPRODFREQ
C
         ELSEIF (WORD(1) .EQ. 'NOPRODFREQ') THEN
            IPROD = 0
C
C DETMI
C
         ELSEIF (WORD(1) .EQ. 'DETMI') THEN
            IDETMI = 1
            NDET = 0
500         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'DETMI'
                  STOP 'READIC 11'
               ENDIF
               DO 510 ID = 1,NWORD
                  TEMFQ(NDET+ID) = CFLOAT(WORD(ID))
510            CONTINUE
               NDET = NDET + NWORD
               GOTO 500 
            ELSEIF (NDET .NE. 5) THEN 
               WRITE(FU6,3200) 5,'DETMI',NDET
               STOP  'READIC 12'
            ENDIF
            FMIR1A = TEMFQ(1)
            FMIR2A = TEMFQ(2)
            FMISPA = TEMFQ(3)
            FMIP1A = TEMFQ(4)
            FMIP2A = TEMFQ(5)
C
C NODETMI
C
         ELSEIF (WORD(1) .EQ. 'NODETMI') THEN
            IDETMI = 0
C
C UNITMI  
C
         ELSEIF (WORD(1) .EQ. 'UNITMI') THEN
            IF (NWORD .EQ. 2) THEN
               IF (WORD(2) .EQ. 'AU') THEN
                  IDUNIT = 0
               ELSEIF (WORD(2) .EQ. 'SI') THEN
                  IDUNIT = 1
               ELSEIF (WORD(2) .EQ. 'CGS') THEN
                  IDUNIT = 2
               ELSEIF (WORD(2) .EQ. 'SCALE') THEN
                  IDUNIT = 3
               ELSEIF (WORD(2) .EQ. 'AMU') THEN
                  IDUNIT = 4
               ELSE
                  WRITE(FU6,5500) 'UNITMI'
                  STOP 'READIC 13'
               ENDIF
            ELSE
               WRITE(FU6,5200) 'UNITMI',1
               STOP 'READIC 14'
            ENDIF 
C
C LCTCOR 
C
         ELSEIF (WORD(1) .EQ. 'LCTCOR') THEN
            IF (NWORD .EQ. 2) THEN
               IF (WORD(2) .EQ. 'NOCOR') THEN
                  INADIB = 0
               ELSEIF (WORD(2) .EQ. 'LINEAR') THEN
                  INADIB = 1
               ELSEIF (WORD(2) .EQ. 'QUADLIN') THEN
                  INADIB = 2
               ELSE
                  WRITE(FU6,5500) 'LCTCOR'
                  STOP 'READIC 15'
               ENDIF
            ELSE
               WRITE(FU6,5200) 'LCTCOR',1
               STOP 'READIC 16'
            ENDIF 
C
C LOWFREQ
C
         ELSEIF (WORD(1) .EQ. 'LOWFREQ') THEN
            NLFREQ = 0
600         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'LOWFREQ'
                  STOP 'READIC 17'
               ENDIF
               DO 610 IM = 1,NWORD
                  ITEM(IM+NLFREQ) = ICINT(WORD(IM))
610            CONTINUE
               NLFREQ = NLFREQ + NWORD
               GOTO 600
            ENDIF
            DO 620 IM = 1,NLFREQ
               ICFR(IM) = NFRS + 1 - ITEM(IM)
620         CONTINUE
C
C RANGEIC
C
         ELSEIF (WORD(1) .EQ. 'RANGEIC') THEN
            IRANGE = 1
700         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'RANGEIC'
                  STOP 'READIC 18'
               ENDIF
               IF (NWORD .NE. 2) THEN
                  WRITE(FU6,3150) 'RANGEIC',2
                  STOP 'READIC 19'
               ENDIF
               IF (WORD(1) .EQ. 'RPL') THEN
                  RANGE = CFLOAT(WORD(2))
               ELSEIF(WORD(1) .EQ. 'BR') THEN
                  BV1 = CFLOAT(WORD(2))
               ELSEIF(WORD(1) .EQ. 'BP') THEN
                  BV2 = CFLOAT(WORD(2))
               ELSE
                  WRITE(FU6,4100) WORD(1),'RANGEIC'
                  STOP 'READIC 20'
               ENDIF
               GOTO 700
            ENDIF
C
C HRRMI
C
         ELSEIF (WORD(1) .EQ. 'HRRMI') THEN
            IF (NWORD .EQ. 2) THEN
               IF (WORD(2) .EQ. 'ALL') THEN
                  IHRRMI = 0
               ELSE
                  IHRRMI = ICINT(WORD(2))
               ENDIF
            ELSE
               WRITE(FU6,5200) 'HRRMI',1
               STOP 'READIC 21'
            ENDIF 
C
C NOHRRMI
C
         ELSEIF (WORD(1) .EQ. 'NOHRRMI') THEN
            IHRRMI = 99
C
C MIRP          
C
         ELSEIF (WORD(1) .EQ. 'MIRP') THEN
            NHRMOD = 0
            MHRMOD = 0
            DO 800 I = 1,4
               DO 800 J = 1, NF(I)
               IF(MODER(I,J) .EQ. 9) NHRMOD = NHRMOD + 1
800         CONTINUE
820         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'MIRP'
                  STOP 'READIC 22'
               ENDIF
               DO 830 IM = 1,NWORD
                  TEMFQ(IM+MHRMOD) = CFLOAT(WORD(IM))
830            CONTINUE
               MHRMOD = MHRMOD + NWORD
               GOTO 820
            ELSEIF (NHRMOD .NE. MHRMOD) THEN
               WRITE(FU6,3200) NHRMOD,'MIRP',MHRMOD
               STOP 'READIC 23'
            ENDIF
            ISUM = 0
            MHRMOD = 0
            DO 860 IRE = 1,4
               NFREQ = NF(IRE)
               DO 850 IMM = NFREQ,1,-1
                  IC = ISUM + IMM
                  IF (MODER(IRE,IMM) .EQ. 9) THEN
                     MHRMOD = MHRMOD + 1
                     HRMIR(IC) = TEMFQ(MHRMOD)
                  ENDIF
850            CONTINUE
               ISUM = ISUM + NFREQ
860         CONTINUE
C
C MITS
C
         ELSEIF (WORD(1) .EQ. 'MITS') THEN
            NHRMOD = 0
            MHRMOD = 0
C
C     Find how many HR modes for the saddle point 
C
            DO 900 J = 1, NF(5)
               IF(MODETS(1,J) .EQ. 9) NHRMOD = NHRMOD + 1
900         CONTINUE
C
920         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'MITS'
                  STOP 'READIC 22'
               ENDIF
               DO 930 IM = 1,NWORD
                  TEMFQ(IM+MHRMOD) = CFLOAT(WORD(IM))
930            CONTINUE
               MHRMOD = MHRMOD + NWORD
               GOTO 920
            ELSEIF (NHRMOD .NE. MHRMOD) THEN
               WRITE(FU6,3200) NHRMOD,'MITS',MHRMOD
               STOP 'READIC 23'
            ENDIF
            MHRMOD = 0
            DO 950 IMM = NF(5),1,-1
               IF (MODETS(1,IMM) .EQ. 9) THEN
                  MHRMOD = MHRMOD + 1
                  HRMITS(IMM) = TEMFQ(MHRMOD)
               ENDIF
950         CONTINUE
C
C 0IVTST
C     
         ELSEIF (WORD(1) .EQ. '0IVTST') THEN
            IVTST = 1
C
         ELSEIF (WORD(1) .EQ. 'NO0IVTST') THEN
            IVTST = 0
C
C SADFREQ
C
         ELSEIF (WORD(1) .EQ. 'SADFREQ') THEN
            NFREQ = 0
1000        CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'SADFREQ'
                  STOP 'READIC 24'
               ENDIF
               DO 1010 IM = 1,NWORD
                  TEMFQ(IM+NFREQ) = CFLOAT(WORD(IM))
1010           CONTINUE
               NFREQ = NFREQ + NWORD
               GOTO 1000
            ELSEIF (NFREQ .NE. NFRS) THEN
               WRITE(FU6,3200) NFRS,'SADFREQ',NFREQ
               STOP  'READIC 25'
            ENDIF
            DO 1020 IM = 1,NFRS
               WESADA(NFRS+1-IM) = TEMFQ(IM)
1020        CONTINUE
C
C RCFREQA 
C
         ELSEIF (WORD(1) .EQ. 'RCFREQA') THEN
            NFREQ = 0
1100        CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'RCFREQA'
                  STOP 'READIC 26'
               ENDIF
               DO 1110 IM = 1,NWORD
                  TEMFQ(IM+NFREQ) = CFLOAT(WORD(IM))
1110           CONTINUE
               NFREQ = NFREQ + NWORD
               GOTO 1100
            ELSEIF (NFREQ .NE. NFRP1) THEN
               WRITE(FU6,3200) NFRP1,'RCFREQA',NFREQ
               STOP  'READIC 27'
            ENDIF
            DO 1120 IM = 1,NFRP1
               FRP1A(NFRP1+1-IM) = TEMFQ(IM)
1120        CONTINUE
C
C RCFREQS
C
         ELSEIF (WORD(1) .EQ. 'RCFREQS') THEN
            NFREQ = 0
1200        CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'RCFREQS'
                  STOP 'READIC 28'
               ENDIF
               DO 1210 IM = 1,NWORD
                  TEMFQ(IM+NFREQ) = CFLOAT(WORD(IM))
1210           CONTINUE
               NFREQ = NFREQ + NWORD
               GOTO 1200
            ELSEIF (NFREQ .NE. NFRP1) THEN
               WRITE(FU6,3200) NFRP1,'RCFREQS',NFREQ
               STOP  'READIC 29'
            ENDIF
            DO 1220 IM = 1,NFRP1
               FRP1S(NFRP1+1-IM) = TEMFQ(IM)
1220        CONTINUE
C
C PCFREQA
C
         ELSEIF (WORD(1) .EQ. 'PCFREQA') THEN
            NFREQ = 0
1300        CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'PCFREQA'
                  STOP 'READIC 30'
               ENDIF
               DO 1310 IM = 1,NWORD
                  TEMFQ(IM+NFREQ) = CFLOAT(WORD(IM))
1310           CONTINUE
               NFREQ = NFREQ + NWORD
               GOTO 1300
            ELSEIF (NFREQ .NE. NFRP2) THEN
               WRITE(FU6,3200) NFRP2,'PCFREQA',NFREQ
               STOP  'READIC 31'
            ENDIF
            DO 1320 IM = 1,NFRP2
               FRP2A(NFRP2+1-IM) = TEMFQ(IM)
1320        CONTINUE
C
C PCFREQS
C
         ELSEIF (WORD(1) .EQ. 'PCFREQS') THEN
            NFREQ = 0
1400        CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'PCFREQS'
                  STOP 'READIC 32'
               ENDIF
               DO 1410 IM = 1,NWORD
                  TEMFQ(IM+NFREQ) = CFLOAT(WORD(IM))
1410           CONTINUE
               NFREQ = NFREQ + NWORD
               GOTO 1400
            ELSEIF (NFREQ .NE. NFRP2) THEN
               WRITE(FU6,3200) NFRP2,'PCFREQS',NFREQ
               STOP  'READIC 33'
            ENDIF
            DO 1420 IM = 1,NFRP2
               FRP2S(NFRP2+1-IM) = TEMFQ(IM)
1420        CONTINUE
C
C FREQIMAG
C
         ELSEIF (WORD(1) .EQ. 'FREQIMAG') THEN
            IF (NWORD .EQ. 2) THEN
               TSWIM = CFLOAT(WORD(2))
            ELSE
               WRITE(FU6,5200) 'FREQIMAG',1
               STOP 'READIC 34'
            ENDIF 
C
C ENERXN
C
         ELSEIF (WORD(1) .EQ. 'ENERXN') THEN
            IF (NWORD .EQ. 2) THEN
               ERXN = CFLOAT(WORD(2))
            ELSE
               WRITE(FU6,5200) 'ENERXN',1
               STOP 'READIC 35'
            ENDIF 
C
C ENESAD
C
         ELSEIF (WORD(1) .EQ. 'ENESAD') THEN
            IF (NWORD .EQ. 2) THEN
               BARRA = CFLOAT(WORD(2))
            ELSE
               WRITE(FU6,5200) 'ENESAD',1
               STOP 'READIC 36'
            ENDIF 
C
C RCINFO
C
         ELSEIF (WORD(1) .EQ. 'RCINFO') THEN
1500        CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'RCINFO' 
                  STOP 'READIC 37'
               ENDIF
               IF (WORD(1) .NE. 'NOFREQCORR' .AND. NWORD .NE. 2) THEN   0406WH95
                  WRITE(FU6,3150) 'RCINFO',2
                  STOP 'READIC 38'
               ENDIF
               IF (WORD(1) .EQ. 'SRC') THEN
                  SP1 = CFLOAT(WORD(2))
               ELSEIF(WORD(1) .EQ. 'NOFREQCORR') THEN                   0406WH95
                  NOCR = 1                                              0406WH95
               ELSEIF(WORD(1) .EQ. 'ENERCS') THEN
                  VP1S = CFLOAT(WORD(2))
               ELSEIF(WORD(1) .EQ. 'ENERCA') THEN
                  VP1A = CFLOAT(WORD(2))
               ELSE
                  WRITE(FU6,4100) WORD(1),'RCINFO'
                  STOP 'READIC 39'
               ENDIF
               GOTO 1500
            ENDIF
C
C PCINFO
C
         ELSEIF (WORD(1) .EQ. 'PCINFO') THEN
1600        CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
            IF (WORD(1) .NE. 'END') THEN
               IF (LSEC .OR. LEOF) THEN
                  WRITE (FU6,3000) 'PCINFO' 
                  STOP 'READIC 40'
               ENDIF
               IF (WORD(1) .NE. 'NOFREQCORR' .AND. NWORD .NE. 2) THEN   0406WH95
                  WRITE(FU6,3150) 'PCINFO',2
                  STOP 'READIC 41'
               ENDIF
               IF (WORD(1) .EQ. 'SPC') THEN
                  SP2 = CFLOAT(WORD(2))
               ELSEIF(WORD(1) .EQ. 'NOFREQCORR') THEN                   0406WH95
                  NOCP = 1                                              0406WH95
               ELSEIF(WORD(1) .EQ. 'ENEPCS') THEN
                  VP2S = CFLOAT(WORD(2))
               ELSEIF(WORD(1) .EQ. 'ENEPCA') THEN
                  VP2A = CFLOAT(WORD(2))
               ELSE
                  WRITE(FU6,4100) WORD(1),'PCINFO'
                  STOP 'READIC 42'
               ENDIF
               GOTO 1600
            ENDIF
C
C RDVMEP   // using different s value in fu51 file                       0317YC99
C
         ELSEIF (WORD(1) .EQ. 'RDVMEP') THEN                             0317YC99
            idvmep = 1                                                   0317YC99
C
C above are all the valid keyword in this version of VTST-IC
C if an invalid keyword is encountered, stop the program
C
         ELSE
            WRITE(FU6,7000) WORD(1)
            STOP 'READIC 43'
         ENDIF
C
C finished processing a keyword, read the next nonblank, noncomment line
C
         CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)
C
         GOTO 100
C
C end of unit fu50 input
C
      ENDIF
      NSPIC = 0                                                          0203YC98
1996  IF (.NOT.LEOF.AND.WORD(1).EQ.'POINT'.AND.IVICE.EQ.2) THEN          0203YC98
        NSPIC = NSPIC + 1                                                0203YC98
1997    CALL READLN(INPF,WORD,NWORD,LSEC,LEOF)                           0203YC98
        IF (.NOT.LSEC.AND..NOT.LEOF) THEN                                0203YC98
          IF (WORD(1).EQ.'SMEP') THEN                                    0203YC98
             IF (NWORD.EQ.2) THEN                                        0203YC98
               SPICS(NSPIC) = CFLOAT(WORD(2))                            0203YC98
             ELSE                                                        0203YC98
               WRITE (FU6,*) 'ERROR, SMEP has an argument'               0203YC98
               STOP 'SMEP in readic'                                     0203YC98
             ENDIF                                                       0203YC98
          ELSE IF (WORD(1).EQ.'VMEP') THEN                               0203YC98
             IF (NWORD.EQ.2) THEN                                        0203YC98
               SPICV(NSPIC) = CFLOAT(WORD(2))                            0203YC98
               SPICV(NSPIC) = SPICV(NSPIC) / CKCAL                       0203YC98
             ELSE                                                        0203YC98
               WRITE (FU6,*) 'ERROR, VMEP has an argument'               0203YC98
               STOP 'VMEP in readic'                                     0203YC98
             ENDIF    
          ELSE IF (WORD(1).EQ.'DVMEP') THEN                              0317YC99
             IF (NWORD.EQ.2) THEN                                        0317YC99
               SPICV(NSPIC) = CFLOAT(WORD(2))                            0317YC99
               SPICV(NSPIC) = SPICV(NSPIC) / CKCAL                       0317YC99
             ELSE                                                        0317YC99
               WRITE (FU6,*) 'ERROR, VMEP has an argument'               0317YC99
               STOP 'DVMEP in readic'                                    0317YC99
             ENDIF                                                       0317YC99
          ELSE                                                           0203YC98
             WRITE (FU6,*) WORD(1),' is not a keyword in fu51'           0203YC98
             STOP 'readic error'                                         0203YC98
          ENDIF                                                          0203YC98
          GOTO  1997                                                     0203YC98
        ELSE                                                             0203YC98
          GOTO 1996                                                      0203YC98
        ENDIF                                                            0203YC98
      ENDIF                                                              0203YC98
C
C     Set LGSIC options
C      
      LGSIC(1) = IMEPR * 10 + IMEPP
      LGSIC(2) = IMATCH
      LGSIC(3) = IPROD
      LGSIC(4) = IDETMI
      LGSIC(5) = IDUNIT
      LGSIC(6) = INADIB
      LGSIC(7) = NLFREQ
      LGSIC(8) = IRANGE
      LGSIC(9) = IHRRMI
      LGSIC(10) = IVTST
      LGSIC(11) = NOCR                                                  0406WH95
      LGSIC(12) = NOCP                                                  0406WH95
C
C     Check the LGSIC options
C
      CALL OPT50
C
C  Do necessary data transport and conversion
C  
C     Copy the reactant/product-side complex frequencies to the
C     reactant/product frequencies if there is no well on the
C     reactant/product side
C   
      IF (.NOT. LWELLR) THEN
         DO 2100 I=1,NFRR
            WERA(I) = FRP1A(I+NZFRR)
2100     CONTINUE
      ENDIF
C
      IF (.NOT. LWELLP) THEN
         DO 2110 I=1,NFRP
            WERA(I+NFRR) = FRP2A(I+NZFRP)
2110     CONTINUE
      ENDIF
C
C  Convert data to atomic units
C
      BARRA  = BARRA  / CKCAL
      ERXN   = ERXN   / CKCAL
      VP1S   = VP1S   / CKCAL
      VP1A   = VP1A   / CKCAL
      VP2S   = VP2S   / CKCAL
      VP2A   = VP2A   / CKCAL
C
      DO 2200 I = 1, NFRP1
         FRP1S(I)   =   FRP1S(I) * CMTOAU                               0606YC96
         FRP1A(I)   =   FRP1A(I) * CMTOAU                               0606YC96
2200  CONTINUE 
      DO 2210 I = 1, NFRP2
         FRP2S(I)   =   FRP2S(I) * CMTOAU                               0606YC96
         FRP2A(I)   =   FRP2A(I) * CMTOAU                               0606YC96
2210  CONTINUE
      DO 2220 I = 1, NFRS
         WESADA(I)  =  WESADA(I) * CMTOAU                               0606YC96
2220  CONTINUE
C
      NTOT = NFRR + NFRP
      DO 2230 I = 1, NTOT 
         WERA(I) = WERA  (I) * CMTOAU                                   0606YC96
2230  CONTINUE
C
      TSWIM = TSWIM * CMTOAU                                            0606YC96
C
C     Convert det I to a.u.
C
      IF (LGSIC(5) .NE. 0 .AND. LGSIC(4) .NE. 0) THEN
         COKGM2  = CKGM2  * 1.0D-47
         COGCM2  = COKGM2 * 1.0D7
         IF (LGSIC(5) .EQ. 1) THEN
            FACTR1 = COKGM2
            FACTR2 = COKGM2 ** 3.0D0
         ELSE IF (LGSIC(5) .EQ. 2) THEN
            FACTR1 = COGCM2
            FACTR2 = COGCM2 ** 3.0D0
         ELSE IF (LGSIC(5) .EQ. 3) THEN
            FACTR1 = CKGM2
            FACTR2 = CKGM2 ** 3.0D0
         ELSE IF (LGSIC(5) .EQ. 4) THEN                                 
            FACTR1 = CAU                                                
            FACTR2 = CAU ** 3.0D0                                       
         ENDIF
         DO 2300 I = 1,5
            IF (ICODE(I) .EQ. 2 .OR. ICODE(I) .EQ. 3 ) THEN
               FACTOR(I) = FACTR1
            ELSE IF (ICODE(I) .EQ. 4) THEN
               FACTOR(I) = FACTR2
            ELSE
               FACTOR(I) = 1.0D0
            ENDIF
2300     CONTINUE
         FMIR1A = FMIR1A / FACTOR(1)
         FMIR2A = FMIR2A / FACTOR(2)
         FMIP1A = FMIP1A / FACTOR(3)
         FMIP2A = FMIP2A / FACTOR(4)
         FMISPA = FMISPA / FACTOR(5)
      ENDIF
C    
      RETURN
C
3000  FORMAT(/1X,'The ',A12,' section must end with an END.')
3100  FORMAT(/1X,'Each line in ',A12,' input section must contain',I2,
     *           ' numbers.')
3150  FORMAT(/1X,'Each line in ',A12,' input section must contain',I2,
     *           ' data items.')
3200  FORMAT(/1X,'There must be exactly ',I3,' numbers in the ',A12,
     *           ' input but there are ',I3,'.')
4100  FORMAT(/1X,'Unrecoginized subkeyword ',A12,' in ',A12,' section.')
5200  FORMAT(/1X,'The keyword ',A12,' must have ',I1,' argument(s).')
5500  FORMAT(/1X,'Invalid argument for ',A12)
6000  FORMAT(/1X,'The first line of unit fu50 input must be *VTSTIC')
6001  FORMAT(/1X,'The first line of unit fu51 input must be *ISPEGEN')
7000  FORMAT(/1X,'Unrecoginized keyword: ',A12)
C
      END subroutine readic
C
C**********************************************************************
C READLN
C**********************************************************************
C
C   This is a new READLN written by Ben Lynch                           1019BE05                
C
C   This subroutine reads from a file IUNIT and returns a list of
C   words in the array WORD(40).  The number of words found is
C   returned at NWORD.  If a new section is found (starting with '*'
C   LSEC is set to .TRUE. and WORD(1) will hold the section name.
C   LEOF is returned as .TRUE. if the end of the file was reached.
C
      SUBROUTINE readln(IUNIT,WORD,NWORD,LSEC,LEOF)
C
      IMPLICIT NONE
C
      CHARACTER*80  LINE,SENAME,STRING,UPCSE
      CHARACTER*80  WORD(40)
      INTEGER       ICOM, IP, IWORDSTART, NWORD, IWS, IUNIT
      LOGICAL       LEOF,LSEC,LBLANK
      LOGICAL       LENDWORD, LNEXT

C     ICOM is the location of the comment character if it exists

C     IP is the index for the position currently being processed in
C     the line.

C     IWORDSTART is the position where the first word starts

C     LBLANK is true as soon as it is confirmed that there are
C     characters on the current working line.

      LSEC   = .FALSE.
      LEOF   = .FALSE.
      LBLANK = .TRUE.
      NWORD  = 0

      DO WHILE (LBLANK)
        READ(IUNIT,'(A80)',END=500) STRING
        IP = 1
        DO WHILE (IP .LE. 80 .AND. STRING(IP:IP) .EQ. ' ')
          IP = IP + 1
          if(ip>80)exit
        END DO
        if(ip<=80)then
          IF (IP.LT.80.AND.STRING(IP:IP) .NE. '#')  LBLANK = .FALSE.
        end if
      END DO
      IWORDSTART = IP

C     Determine if comment exists later in the line, and record its
C     position in ICOM if there is.

      ICOM = IP
      DO WHILE (IP .LE. 80 .AND. STRING(IP:IP) .NE. '#')
        IP = IP + 1
        ICOM = IP
        if(ip>80)exit
      END DO

C     ICOM will be 81 if no comment card exists

      LINE = UPCSE(STRING)

      IWS = IWORDSTART
      LENDWORD = .FALSE.
      LNEXT=.TRUE.
      DO 10, IP = IWORDSTART, ICOM-1                                    1019BE05
 
        IF (LINE(IP:IP) .EQ. ' ' .AND. LNEXT) THEN  
          NWORD = NWORD + 1
          WORD(NWORD)= LINE(IWS:IP-1)
          LENDWORD = .TRUE.
          LNEXT =    .FALSE.
        ELSE IF (IP . EQ. 80 .AND. LNEXT) THEN                          1111BE05
          NWORD = NWORD + 1                                             1111BE05
          WORD(NWORD)= LINE(IWS:IP)                                     1111BE05
          LENDWORD = .TRUE.                                             1111BE05
          LNEXT =    .FALSE.                                            1111BE05
        ELSE IF (LINE(IP:IP) .NE. ' ') THEN
          LNEXT=.TRUE.
          IF (LENDWORD) THEN
            IWS = IP
            LENDWORD = .FALSE.
          ENDIF
        END IF

10    END DO

      IF (WORD(1)(1:1) .EQ. '*') THEN
         SENAME = WORD(1)(2:)
         WORD(1) = SENAME
         LSEC = .TRUE.
      ENDIF

      RETURN

500   LEOF = .TRUE.

      RETURN

      END
c
c***********************************************************************
c     rect
c***********************************************************************
c
      subroutine rect(string,istrt)
c
c     Subroutine to read in pairs of temperatures for which the
c     activation energy will be computed.
c
      use perconparam, only : fu5,fu6; use cm
      use kintcm; use common_inc
      implicit double precision (a-h,o-z)
      character * 80 string
c
      ieact = 0
      do 1 i=1,10
        do 1 j=1,2
1       etpair(i,j) = 0.0d0                                             09/95KAN
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
         ieact = ieact + 1
         if (ieact.gt.10) then
           write(fu6,1010)
1010       format(3x,'ERROR:  max number of temps pairs is 10')
           stop
         end if
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  EACT section must end with an END')
            stop
         end if
c
c        parse line to get temperatures
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a second temperature is also on the line
         if (ierr.eq.1) then
             write(fu6,1100)
             stop
         end if
1100     format(3x,'ERROR:  must specify pairs of temperatures')
c
         iend = istrt - 1
         etpair(ieact,1) = cfloat(string(ibegin:iend))
         etpair(ieact,2) = cfloat(string(istrt:80))
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      npair = ieact
      return
      end subroutine rect
c
c***********************************************************************
c  redgeo
c***********************************************************************
c
      subroutine redgeo
c
c     Sets variables for the jtype species with: 
C     jtype = 1 or 2 for reactants
c     jtype = 3 or 4 for products
c     jtype = 5 for the saddle point
c     jtype = 7 reactant well                                           0728PF97
c     jtype = 8 product well                                            0728PF97
c
c     called by:
c              rstat 
c     moved call to subroutine rstat on 5/30/97
c        moved to allow for linearity checking before react, prod, or start
c         switches are printed out.
c
      use perconparam ; use cm
      use kintcm; use common_inc
      use keyword_interface, only : idmn
      implicit double precision (a-h,o-z)
c
c      dimension xl(6)
c     The above line was commented because the variable xl is not used. 0423TA02
c
c     set the variables for all reactants, products, wells, and the saddle 
c     point that exist in the system
c
c commented out the next two lines of code.  they were not needed after moving
c the routine call to rstat.  checks are done in rstat before enetering.
       do 100 jtype = 1,8 
c
          if (irepr(jtype).eq.0)  go to 100 
c
         if (jtype.ne.5) then                                           0729PF97
            nrx = nratom(jtype)                                         0729PF97
         else                                                           0729PF97
            nrx = natom                                                 0729PF97
         endif                                                          0729PF97
c
         if (idmn(jtype).eq.'nonlinrp'.or.
     *       idmn(jtype).eq.'nonlints'.or.
     *       idmn(jtype).eq.'nonlinwe') then                            0728PF97
           icode(jtype) = 4
         elseif (idmn(jtype).eq.'linrp'.or.idmn(jtype).eq.'lints'
     *        .or.idmn(jtype).eq.'linwell') then                        0728PF97
           if (nrx.eq.1) then
             icode(jtype) = 1
           elseif (nrx.eq.2) then
              if (ndiat(jtype).eq.1) then
                icode(jtype) = 2
              else
                icode(jtype) = 3
              endif
           else
              icode(jtype) = 3
           endif
         else if (idmn(jtype).eq.'atomic') then
           icode(jtype) = 1
         else if (idmn(jtype).eq.'ssrp'.or.idmn(jtype).eq.'ssts') then
           icode(jtype) = -4
         else 
           write(fu6,*) 'ERROR:  unknown value ', idmn(jtype)
           stop
         endif
c
c special case for VRC-TST
c
         if(ivrc.eq.1) icode(5)=4                                       0401JZ08
c
c     compute number of frequencies
c 
         if (icode(jtype) .lt. 0) then
            nfreq = 3*nrx
         else if (icode(jtype).eq.1) then
            nfreq = 0
         else if (icode(jtype).eq.3 .or. icode(jtype).eq.2) then
            nfreq = 3*nrx - 5
         else
            nfreq = 3*nrx - 6
         endif
         if (jtype.eq.5) nfreq = nfreq-1
         nf(jtype) = nfreq
c 
         if (iharm(jtype).eq.1) then
            lgs5 = 0
         else if (imor(jtype).eq.1) then
            lgs5 = 1
         else if (imorqq(jtype).eq.1) then
            lgs5 = 2
         else if (iqqwkb(jtype).eq.1) then
            lgs5 = 7
         else if (iqqsem(jtype).eq.1) then
            lgs5 = 8
         else if (ivary(jtype).eq.1) then
            lgs5 = 21
         else
            lgs5 = nregon+20
         end if
c
         if (icode(jtype).ne.1) then
            do 20 i = 1,nfreq
               if(jtype.le.8.and.jtype.ne.5) moder(jtype,i) = lgs5      0729PF97
               if(jtype.eq.5) modets(1,i) = lgs5
 20         continue
         endif   
c
c     if variable anharmonicity is used - set flags here
c
         if (ivary(jtype).eq.1) call setanh(jtype)
c
c     set flags for indicies over which the geometry search will be
c     carried out
c
         icodj = abs(icode(jtype))
         if (icodj .gt. 2 .or. icode(jtype).eq. -2) then
            nfix = ncnst(jtype)
            ndim(jtype) = 3*nrx - nfix
c
            if (jtype.eq.5) then
               j = 1
               j0 = 1
               do 40 k=1,3*natom
                 if (icnst(k,5).eq.1) then
                    indx(j,5) = k
                    j= j+1
                 end if
                   indx0(j0,5) = k
                   j0 = j0 + 1
40             continue
            else
               j=1
               j0=1
               do 50 k=1,nrx
                  istart = 3*iatsv(k,jtype) - 2
                  do 60 i = istart,istart+2
                     if (icnst(i,jtype).eq.1) then
                        indx(j,jtype) = i
                        j=j+1
                     end if
                     indx0(j0,jtype) = i
                     j0 = j0 + 1
60                continue
50             continue
            end if
         end if
C 
100   continue 
      return
      end subroutine redgeo
c***********************************************************************
c     refopt
c***********************************************************************
c
      subroutine refopt
      use common_inc; use perconparam; use kintcm; use keyword_interface
      use cm; use efmain_mod
c
c     subroutine to read in the options in optmin and optts keywords
c
c     called by:
c           ropt
c
      implicit double precision (a-h,o-z)
      character*80 word(40)
      logical lsec,leof
c
10    call readln(fu5,word,nword,lsec,leof)
      if (word(1) .ne. 'END') then
         if (lsec .or. leof) then
            write(fu6,1000)
            stop 'EFOPT 1'
         endif
c
1000  format(/1X,'The EFOPT section must end with an END')
c
         if (word(1) .eq. 'RMAX') then
            if (nword.eq.2) then
               rmax = cfloat(word(2))
            else
               write (fu6,1001) 'RMAX'
            endif
         elseif(word(1).eq.'RMIN') then
            if (nword.eq.2) then
               rmin = cfloat(word(2))
            else
               write (fu6,1001) 'RMIN'
            endif
         elseif(word(1).eq.'OMIN') then
            if (nword.eq.2) then
               omin = cfloat(word(2))
            else
               write (fu6,1001) 'OMIN'
            endif
         elseif(word(1).eq.'DDMAX') then
            if (nword.eq.2) then
               ddmax = cfloat(word(2))
            else
               write (fu6,1001) 'DDMAX'
            endif
         elseif(word(1).eq.'DDMAXTS') then
            if (nword.eq.2) then
                ddmaxts = cfloat(word(2))
            else
               write (fu6,1001) 'DDMAXTS'
            endif
         else
            write(fu6,1100)
            stop 'EFOPT 2'
         endif
         goto 10
      endif
c
1001  format(/1X,A10,' must have an argument')
1100  format(/1X,'Unrecognized subkeyword in the EFOPT section')
      return
      end subroutine refopt
c
c***********************************************************************
c     relec
c***********************************************************************
c
      subroutine relec(string,istrt,jtype)
c
c     subroutine to read in the electronic degenercies for stationary points
c     the stationary section.  Since it is a list variable the data must
c     concluded with an 'end'. 
c
      use common_inc; use perconparam, only : fu5,fu6
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     read denergency and energy in atomic units until an end is found 
c
      k=2
      call rline(fu5,string,istrt,isect,iend)
      do while (string(istrt:istrt+2).ne.'end')
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that the energy was also on the line
         if (ierr.eq.1) then
             write(fu6,1100)
1100         format(3x,'ERROR:  must specify energy with electronic',
     *             ' degenercy')
             stop
         end if
c
         iend = istrt - 1
         jend = 3*jtype
         jbeg = jend-k
         nedeg(jbeg) = icint(string(ibegin:iend))
         elec(jbeg) = cfloat(string(istrt:80))
c
         call rline(fu5,string,istrt,isect,iend)
c
c                               check if end of file was found - an error
         if (iend.eq.1) then
            write(fu6,*)'ERROR: elec list must end with an END'
            stop
         end if
         k = k-1
      end do
c
      return
      end subroutine relec
c
c***********************************************************************
c     renerg
c***********************************************************************
c
      subroutine renerg(string,iend,istrt)
      use common_inc; use perconparam, only : fu5,fu6
      use keyword_interface, only : potnam
      use kintcm, only : ipot
c
c     subroutine to read in the input in the energetics section.  
c
c     called by:
c           read5
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     read in first keyword in this section
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c POTENTIAL
         if (string(istrt:istrt+5).eq.'potent') then
            call rpotet(string,istrt)
c EZUNIT
         else if (string(istrt:istrt+5).eq.'ezunit')then                0205YC97
            call rezeru(string,istrt)                                   0205YC97 
c EZERO
         else if (string(istrt:istrt+4).eq.'ezero') then
            call rezero(string,istrt)
c SPECBASIS                                                             0522RS95
         else if (string(istrt:istrt+8).eq.'specbasis') then            0522RS95
            call rbasis                                                 1116BL03
c ACESOPTE
         else if (string(istrt:istrt+7).eq.'acesopte') then
            potnam = 'aces'
            ipot=2
            call races(string,istrt,7)
c ACESOPTF
         else if (string(istrt:istrt+7).eq.'acesoptf') then
            potnam = 'aces'
            ipot=2
            call races(string,istrt,8)
         else
            write(fu6,1200) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
1200  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' ENERGETICS section',/A80)
c
      return
      end subroutine renerg
c
c***********************************************************************
c     res1
c***********************************************************************
c
      subroutine res1(string,istrt)
      use common_inc; use perconparam, only : fu5,fu6
      use keyword_interface, only : potnam,gufac5
      use kintcm, only : ipot
      implicit double precision(a-h,o-z)
c
c     Subroutine to read in the variables for Euler with stabilization
c     integrator. 
c
      character * 80 string
      character * 80 vname
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end'.and.narg.le.2)
c
         narg = narg+1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  es1 section must end with an END')
            stop
         end if
c
c        parse line to get variable name
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a value is also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  must specify distance with delta2 or diffd') 
c
         iend = istrt - 1
         vname = string(ibegin:iend)
c
c        check that vname is a legal variable name and what it is 
c
         if (vname(1:6).eq.'delta2') then 
             delta2 = cfloat(string(istrt:80))*gufac5
         else  if (vname(1:5).eq.'diffd') then
             diffd = cfloat(string(istrt:80))
         else
             write(fu6,1200)vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in ES1: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      narg = narg-1
      if (narg.gt.2) then
         write(fu6,1300) narg
         stop
      end if
1300  format(3x,'ERROR:  ES1 can have at most 2 arguments,',
     *  ' it had ',i3)
c
      return
      end subroutine res1
c
c***********************************************************************
c     rextrp
c***********************************************************************
c
      subroutine rextrp(string,iend,istrt,idir)
c
c     Subroutine to read in the input for the extrapolation region with
c      idir = 1          first direction
c      idir = 2          second direction
c
      use common_inc; use perconparam, only : fu5,fu6
      use keyword_interface, only : potnam,gufac5
      use rate_const
      implicit double precision(a-h,o-z)
      character * 80 string
c
c     read in first keyword in this section
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
c EXREACT
         if (string(istrt:istrt+6).eq.'exreact') then
            iwha(idir) = -1
c
c EXPROD
         else if (string(istrt:istrt+5).eq.'exprod') then
            iwha(idir) = 1
c
c EXNSTEP
         else if (string(istrt:istrt+6).eq.'exnstep') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               nste(idir) = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable EXNSTEP must ',
     *                    'have an integer argument'
               stop
            end if
c EXSTEP
         else if (string(istrt:istrt+5).eq.'exstep') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               dlex(idir) = cfloat(string(istrt:80))*gufac5             0405JZ07
            else
               write(fu6,*)' ERROR:  variable EXSTEP must ',
     *                    'have an argument'
               stop
            end if
c EXALPHA
         else if (string(istrt:istrt+6).eq.'exalpha') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               alph(idir) = cfloat(string(istrt:80))/gufac5             0405JZ07
            else
               write(fu6,*)' ERROR:  variable EXALPHA must ',
     *                    'have an argument'
               stop
            end if
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
1000  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' extrapolation section',/A80)
c
      return
      end subroutine rextrp
c
c***********************************************************************
c     rezero
c***********************************************************************
c
      subroutine rezero(string,istrt)
      use common_inc; use perconparam
      use keyword_interface
      use kintcm, only : ipot,iezeru ; use rate_const
c
c     Subroutine to read in the input for the zero of energy
c
      implicit double precision (a-h,o-z)

      character * 80 string
c
c     parse argument off the ezero keyword line
c
      call rword(string,istrt,ierr)
      if(ierr.ne.1) then
          if(string(istrt:istrt+3).eq.'read') then
             cezero = 'read'
             call rword(string,istrt,ierr)
             if (ierr.ne.1) then
                ezer0 = cfloat(string(istrt:80))
                if (iezeru.eq.1) ezer0=ezer0/ckcal                      0205YC97
             else
                write(fu6,*)'ERROR:  The EZERO read option must ',
     *              'have a numerical argument'
                      stop
             end if
          else
             cezero = string(istrt:istrt+8)
             ezer0 = 0.0d0                                              09/95KAN
          end if
      else
          write(fu6,*)' ERROR:  variable EZERO must ',
     *               'have a character argument'
          stop
      end if
      return
      end subroutine rezero
c
c***********************************************************************
c     rezeru
c***********************************************************************
c
      subroutine rezeru(string,istrt)
      use common_inc; use perconparam, only : fu6
      use kintcm, only : iezeru
c
c     Subroutine to read in the unit for the zero of energy             0205YC97
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     parse argument off the ezerounit keyword line
c
      call rword(string,istrt,ierr)
      if(ierr.ne.1) then
          if(string(istrt:istrt+3).eq.'kcal') then
             iezeru = 1 
          elseif (string(istrt:istrt+1).eq.'au') then
             iezeru = 0
          else
             write (fu6,*) 'ERROR: this option is not support'
             stop
          endif
      else
          write(fu6,*)' ERROR:  variable EZERO must ',
     *               'have a character argument'
          stop
      endif
      return
      end


c
c***********************************************************************
c     rgener
c***********************************************************************
c
      subroutine rgener(string,iend,istrt)
c
c     subroutine to read in the input in the general section.  This includes
c     the title, potential, restart options, and atoms.
c
c     called by:
c           read5
c
c     calls:
c           rline,rtitle,rpotet,rrstrt
c
      use common_inc; use rate_const
      use perconparam, only : fu5,fu6; use cm
      use keyword_interface; use kintcm
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     read in first keyword in this section
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c TITLE
         if (string(istrt:istrt+4).eq.'title') then
            call rtitle(string,istrt)
c
c SUPERMOL
         else if (string(istrt:istrt+7).eq.'supermol') then             0327YC97
            isup = 0                                                    0327YC97
c NOSUPERMOL
         else if (string(istrt:istrt+9).eq.'nosupermol') then           0327YC97
            isup = 1                                                    0327YC97
c CLASSVIB  
         else if (string(istrt:istrt+7).eq.'classvib') then             0528JC97
            iclasv = 1                                                  0528JC97
c NOCLASSVIB  
         else if (string(istrt:istrt+9).eq.'noclassvib') then           0528JC97
            iclasv = 0                                                  0528JC97
c CHECK
         else if (string(istrt:istrt+4).eq.'check') then
            icheck = 1
c ICOPT
         else if (string(istrt:istrt+4).eq.'icopt') then
            ivic = 1
            call ricopt(string,istrt)
c DL                                                                    0605YC98
         else  if (string(istrt:istrt+1).eq.'dl') then
            call rword(string,istrt,ierr)
            if (ierr.eq.1) then
                write(fu6,*) 'ERROR: DL must have an argument'
                stop 'rgener'
            else if (string(istrt:istrt+3).eq.'none') then
                ivic = 0
            else if (string(istrt:istrt+2).eq.'ioc') then
                ivic = 1 
            else if (string(istrt:istrt+3).eq.'ispe') then
                ivic = 2
                ivice = 2
                ivicm = 3
                ivico = 2
            else                                                        0317YC99
                write (fu6,1200) ' DL '//string(istrt:80)               0317YC99
                stop                                                    0317YC99
            endif
c MDMOVIE
         else  if (string(istrt:istrt+6).eq.'mdmovie') then
            imdmov = 1
c WRITEFU30                                                           
         else  if (string(istrt:istrt+8).eq.'writefu30') then
            iwrt30 = 1                                      
c WRITEFU31                                                             0810JC97
         else  if (string(istrt:istrt+8).eq.'writefu31') then           0810JC97
            iwrt31 = 1                                                  0810JC97
c WRITE62
         else  if (string(istrt:istrt+6).eq.'write62') then             0522TA02
            call rword(string,istrt,ierr)                               0522TA02
            if (ierr.eq.1) then                                         0522TA02
                write(fu6,*) 'ERROR: WRITE62 must have an argument'     0522TA02 
                stop 'rgener'                                           0522TA02
            else if (string(istrt:istrt+3).eq.'none') then              0522TA02
                iwrt62 = 0                                              0522TA02
            else if (string(istrt:istrt+5).eq.'viball') then            0522TA02
                iwrt62 = 1                                              0522TA02
            else if (string(istrt:istrt+5).eq.'vibsp') then             0522TA02
                iwrt62 = -1                                             0522TA02
            else                                                        0522TA02
                write (fu6,1200) ' WRITE62 '//string(istrt:80)          0522TA02
                stop 'rgener'                                           0522TA02
            endif                                                       0522TA02
c IVTST0                                                                07/95KAN
         else  if (string(istrt:istrt+5).eq.'ivtst0') then              07/95KAN
            ivtst = 0                                                   07/95KAN
c IVTST1                                                                07/95KAN
         else  if (string(istrt:istrt+5).eq.'ivtst1') then              07/95KAN
            ivtst = 1                                                   07/95KAN
c VRC-TST Option
         else if (string(istrt:istrt+5).eq.'vrcopt') then
            ivrc = 1
            ieuler = 0
            ihess = 0
            icartrp = 0
            muvt = 1
            iejmuvt = 1
            call rvrcopt(string,istrt)
c VRC-TST
         else  if (string(istrt:istrt+2).eq.'vrc') then                 1026JZ07
            ivrc  = 1
            ieuler = 0
            ihess = 0
            icartrp = 0
            muvt = 1
            iejmuvt = 1
c GEOMUNIT                                                              1104JC97
c        else  if (string(istrt:istrt+7).eq.'geomunit') then            1104JC97
c INPUNIT
         else  if (string(istrt:istrt+6).eq.'inpunit') then             0405JZ07
            call rrgu5(string,istrt)                                    1104JC97
c OUTUNIT
         else  if (string(istrt:istrt+6).eq.'outunit') then             0405JZ07
            call rrgu6(string,istrt)
c RESTART
         else if (string(istrt:istrt+5).eq.'restar') then
            call rrstrt(string,istrt)
            if (iunxt.ge.1) then
              lgs(8) = iunxt
c             lgsave = lgs(8) 
            end if
c ATOMS
         else if (string(istrt:istrt+4).eq.'atoms') then
            call ratoms(string,istrt)
         else
            write(fu6,1200) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
1200  format(3x,'Error: the following is not a valid keyword in the',
     *       ' general section',/A80)
c
      return
      end
c
c***********************************************************************
c     rgeom
c***********************************************************************
c
      subroutine rgeom(string,istrt,jtype)
c
c     Generic subroutine to read in the initial geometry for reactants and
c     products and wells.  
c
c     jtype = 1  first reactant 
c     jtype = 2  second reactant 
c     jtype = 3  first product 
c     jtype = 4  second product
c     jtype = 7  reactant well
c     jtype = 8  product well
c
      use common_inc, only : xr,iatsv,nratom
      use perconparam, only : fu5,fu6,natom
      use kintcm, only : initg
      use keyword_interface, only : gufac5
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     initialize the conversion factor for the geometries
c
c            if (igeou5.eq.0) then                                      0402JZ07
c                gufac=1.88972652D0                                     1104JC97
c            else                                                       1104JC97
c                gufac=1.D0                                             1104JC97
c            endif                                                      1104JC97
c
c     read in each line of the geometry until an 'end' is found
c
      call rline(fu5,string,istrt,isect,iend)
      nratm = 1
      if(.not.allocated(iatsv))then
        allocate(iatsv(natom,8),xr(3*natom,8)); iatsv=0; xr=0.d00
      end if
c
      do while(string(istrt:istrt+2).ne.'end'.and.nratm.le.natom)
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  stationary point geometry specification',
     *               ' must end with and END')
            stop
         end if
c
c        parse line to get atom number
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         jatm = icint(string(ibegin:iend))
c
c        check that jatm is a legal atom number
c
         if (jatm.le.0.or.jatm.gt.natom) then
             write(fu6,1100) string
1100         format(3x,'ERROR:  invalid atom number in stationary',
     *       ' point geometry specification.  Invalid line is: ',
     *       /1x,a80)
             stop
         end if
c
c        save atom number
c
         iatsv(nratm,jtype) = jatm
c
c        parse the x, y and z coordinates off the rest of the line
c
c        in MORATE, only read the atom numbers, not the coordinates
c        the user chooses not to optimize the geometry.  This allows the
c        user to put in an optimized geometry and not have to redo an
c        expensive calculation.
c
c         if (potnam .eq. 'mopac') goto 9000    
         if (initg(jtype).eq.1) goto 9000                               0514PF97
c
         do 2 j=2,0,-1
             ibegin = istrt
             call rword(string,istrt,ierr)
             iend = istrt - 1
c
c             check that the coordinate was found on the line
c
             if (ierr.eq.1.and.j.ne.0) then
                write(fu6,1200) string
1200            format(3x,'ERROR:  invalid cartesian coordinate in',
     *          ' stationary point.  Invalid line is: ',/a80)
                stop
             end if
c
c            save coordinate
c
             if (jtype.eq.1.or.jtype.eq.2) 
     *           xr(jatm*3-j,1) = cfloat(string(ibegin:iend))*gufac5    0405JZ07
             if (jtype.eq.3.or.jtype.eq.4) 
     *           xr(jatm*3-j,3) = cfloat(string(ibegin:iend))*gufac5    0405JZ07
             if (jtype.eq.7.or.jtype.eq.8)
     *           xr(jatm*3-j,jtype)=cfloat(string(ibegin:iend))*gufac5  0405JZ07
c
2        continue
c
9000     nratm = nratm+1
         call rline(fu5,string,istrt,isect,iend)
      end do
      nratom(jtype) = nratm - 1
c
      return
      end
c
c***********************************************************************
c     rgsad
c***********************************************************************
c
      subroutine rgsad(string,istrt,jtype)
c
c     Generic subroutine to read in the initial geometry for the saddle or 
c     starting stationary point. 
c
c     Jtype = 5  saddle point or starting stationary point
c
      use common_inc
      use perconparam
      use kintcm
      use keyword_interface, only : gufac5
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     initialize the conversion factor for the geometries
c
c             if (igeou5.eq.0) then                                     0402JZ07
c                gufac=1.88972652D0                                     1104JC97
c            else                                                       1104JC97
c                gufac=1.D0                                             1104JC97
c            endif                                                      1104JC97
c
c     read in one line of the geometry until natom have been read in
c
c
      do 1 i=1,natom
c
         call rline(fu5,string,istrt,isect,iend)
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  saddle point geometry specification',
     *               ' must end with and END')
            stop
         end if
c
c        parse line to get atom number
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         jatm = icint(string(ibegin:iend))
c
c        check that jatm is a legal atom number
c
         if (jatm.le.0.or.jatm.gt.natom) then
             write(fu6,1100) string
1100         format(3x,'ERROR:  invalid atom number in saddle',
     *       ' point geometry specification.  Invalid line is: ',
     *       /1x,a80)
             stop
         end if
c 
c        in MORATE and ACES, only the atom numbers are read, 
c        not the coordinates
c        in ACES, the coordinates are read but they are ignored unless
c        the user chooses not to optimize the geometry.  This allows the
c        user to put in an optimized geometry and not have to redo an
c        expensive calculation.
c
c         if (potnam .eq. 'mopac') goto 1    
          if (initg(5).eq.1) goto 1                                     0514PF97
c
c        parse the x, y and z coordinates off the rest of the line
c
         do 2 j=2,0,-1
             ibegin = istrt
             call rword(string,istrt,ierr)
             iend = istrt - 1
c
c             check that the coordinate was found on the line
c
             if (ierr.eq.1.and.j.ne.0) then
                write(fu6,1200) string
1200            format(3x,'ERROR:  invalid cartesian coordinate in',
     *          ' saddle point.  Invalid line is: ',/a80)
                stop
             end if
c
c            save coordinate
c
             xr(jatm*3-j,5) = cfloat(string(ibegin:iend))*gufac5        1104JC97
c
2        continue
c
1     continue
      call rline(fu5,string,istrt,isect,iend)
      if(string(istrt:istrt+2).ne.'end') then
        write(fu6,1300)
        stop
      end if
c
1300  format(2x,'ERROR: all atoms in the system must be given in the',
     * ' starting point geometry specification')   
c
      return
      end
c
c***********************************************************************
c     rgspec
c***********************************************************************
c
      subroutine rgspec(string,istrt)
      use common_inc
      use perconparam, only : fu5,fu6
      use kintcm
      use keyword_interface, only : gufac5
      use rate_const
c
c     Subroutine to read in the range for which the free energy curve
c     is to be computed.
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  GSPEC section must end with an END')
            stop
         end if
c
c        parse line to get variable name
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a value is also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  must specify value with smax or smin') 
c
         iend = istrt - 1
         vname = string(ibegin:iend)
c
c        check that vname is a legal variable name and what it is 
c
         if (vname(1:4).eq.'smax') then 
             slpg = cfloat(string(istrt:80))*gufac5                     0405JZ07
         else  if (vname(1:4).eq.'smin') then
             slmg = cfloat(string(istrt:80))*gufac5                     0405JZ07
         else
             write(fu6,1200) vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in GSPEC: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rgspec
c
c***********************************************************************
c     rgtemp
c***********************************************************************
c
      subroutine rgtemp(string,istrt)
      use common_inc
      use perconparam, only : fu5,fu6
      use kintcm
      use keyword_interface, only : gufac5
      use rate_const; use cm
c
c     Subroutine to read in the temperatures.
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      do 1 i=1,40
1       gtemp(i) = 0.0d0                                                09/95KAN
c
      igtemp = 0
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
         igtemp = igtemp + 1
         if (igtemp.gt.40) then
           write(fu6,1010)
1010       format(3x,'ERROR:  max number of temps allowed is 40')
           stop
         end if
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  GTEMP section must end with an END')
            stop
         end if
c
c        parse line to get temperature
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that the s values are also on the line
         if (ierr.eq.1) then
             write(fu6,1100)
             stop
         end if
1100     format(3x,'ERROR:  must specify smin and smax with ',
     *     ' temperature')
c
         iend = istrt - 1
         gtemp(igtemp) = cfloat(string(ibegin:iend))
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt-1
c
c        store smin and smax for this temperature
c
         slma(igtemp) = cfloat(string(ibegin:iend))*gufac5              0405JZ07
         slpa(igtemp) = cfloat(string(istrt:80))*gufac5                 0405JZ07
c
c        check if the lowest value was put in first (smin) - if not
c        just switch them.
c
         if (slma(igtemp).gt.slpa(igtemp)) then
            tmp = slma(igtemp)
            slma(igtemp) = slpa(igtemp)
            slpa(igtemp) = tmp
         end if
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rgtemp
c
c***********************************************************************
c     rgzmat
c***********************************************************************
c
      subroutine rgzmat(string,istrt,jtype)
      use common_inc
      use perconparam, only : fu5,fu6,natom
      use kintcm
      use keyword_interface, only : nratmd,zmat
      use rate_const; use cm
c
c     Generic subroutine to read in the initial geometry for reactants, 
c     products, wells, and saddle point in z-matrix form.  This is used only 
c     when the geometry optimization is done using ACES.
c     Note that the z-matrix is left in upper case to be consistent
c     with ACES.
c
c     jtype = 1  first reactant 
c     jtype = 2  second reactant 
c     jtype = 3  first product 
c     jtype = 4  second product
c     jtype = 5  saddle point
c     jtype = 7  reactant well
c     jtype = 8  product well
c
      implicit double precision (a-h,o-z)
      character * 80 string,upcse
c
c     read in each line of the geometry until an 'end' is found
c
      call rline(fu5,string,istrt,isect,iend)
c
c     This subroutine was modified on 10/23/96 to take into account
c     dummy atoms in the z-matrix.  nratm is the number of atoms
c     in the reactant and nratd is the number including dummy atoms.
c
      nratm = 1
      nratd = 1
c
      do while(string(istrt:istrt+2).ne.'end'.and.nratm.le.natom)
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  stationary point z matrix geometry',
     *               ' must end with and END')
            stop
         end if
c
c        modified to take into account dummy atoms
         zmat(jtype,nratd) = upcse(string(istrt:istrt+29)) 
         if (string(istrt:istrt).ne.'x') nratm = nratm+1
         nratd = nratd+1
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      if (jtype.le.4) nratom(jtype) = nratm - 1
      nratmd(jtype) = nratd - 1
      ncnst(jtype) = 0
c
      return
      end subroutine rgzmat

c
c***********************************************************************
c     rhessc
c***********************************************************************
c
      subroutine rhessc(string,iend,istrt)
      use common_inc
      use perconparam, only : fu5,fu6,natom
      use kintcm, only : ispot
      use keyword_interface, only : potsec,zmat
      use rate_const
c
c     subroutine to read in the option for hesscal keyword
c
c     called by:
c           rsecnd
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      call rword(string,istrt,ierr)                                     0301YC97
      if(ierr.ne.1) then                                                0301YC97
c GHOOK
        if (string(istrt:istrt+4).eq.'ghook') then                      0301YC97
              potsec = 'ghook'
              ispot = 0
c HHOOK
        elseif (string(istrt:istrt+4).eq.'hhook') then                  0301YC97
               potsec = 'hhook'
               ispot = 1
        endif
      else
        write(fu6,*)' ERROR:  variable HESSCAL must',                   0301YC97
     *           ' have an argument'                                    0219YC97
        stop                                                            0219YC97
      endif
      return
      end subroutine rhessc
c
c***********************************************************************
c     ricopt
c***********************************************************************
c
      subroutine ricopt(string,istrt)
      use perconparam, only : fu5,fu6,natom
      use kintcm, only : ivico,ivicm
      use keyword_interface, only : ivice,icmod
      use rate_const
c
c     Subroutine to read in the options for interpolated corrected VTST
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
         narg = narg+1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  icopt section must end with an END')
            stop
         end if
c
c different interpolated  correction methods
c icr - interpolated correction based on ratio                          03/96/YC
c icl - interpolated correction based on log of ratio                   03/96/YC
c the default is ic - interpolated correction based on difference       03/96/YC
c new variable ivicm is introduced                                      03/96/YC
c
         if (string(istrt:istrt+3).eq.'freq') then                      0626YC97
             call rword(string,istrt,ierr)                              1203YC96
             if(ierr.ne.1) then                                         1203YC96
               if (string(istrt:istrt+2).eq.'icr') then                 03/96/YC
                      ivicm = 1                                         03/96/YC
               else if (string(istrt:istrt+2).eq.'ica') then            1203YC96
                      ivicm = 0
               else if (string(istrt:istrt+2).eq.'icl') then            03/96/YC
                      ivicm = 2                                         03/96/YC
               else if (string(istrt:istrt+3).eq.'none') then           0626YC97
                      ivicm = 3                                         0626YC97
               else                                                     0626YC97
                 write(fu6,*)' ERROR:  variable FREQ must',             0626YC97
     *             ' have an argument'                                  0626YC97 
                 stop                                                   0626YC97
               end if                                                   0626YC97 
            end if 
         else if (string(istrt:istrt+6).eq.'correct') then 
             ivico = 1
         else if (string(istrt:istrt+3).eq.'both') then 
             ivico = 2
         else if (string(istrt:istrt+7).eq.'zero') then 
c            this is a place holder in case there is another order
c            possible.  Right now only zero order exist.
         else if (string(istrt:istrt+6).eq.'restart') then
             icrst = 1 
         else if (string(istrt:istrt+5).eq.'modify') then
             icmod =1
         else if (string(istrt:istrt+5).eq.'energy') then               0203YC98
             call rword(string,istrt,ierr)                              0203YC98
             if(ierr.ne.1) then                                         0203YC98
                if (string(istrt:istrt+6).eq.'eckartl') then            1203YC96
                   ivice = 1                                            1203YC96
                   call rword(string,istrt,ierr)                        1203YC96
                      if(ierr.ne.1) then                                1203YC96
                         rangea  = cfloat(string(istrt:80))             1203YC96
                      else                                              1203YC96
                         write(fu6,*)' ERROR:  variable eckartl must',  1203YC96
     *                   ' have an argument'                            1203YC96
                      stop                                              1203YC96
                      end if                                            1203YC96
                else if (string(istrt:istrt+6).eq.'deckart') then       0726YC98
                         ivice = 1                                      1203YC96
                else if (string(istrt:istrt+6).eq.'seckart') then       0726Yc98
                         ivice = 0           
                else                                                    1203YC96
                      write (6,*) 'not valid keyword option for ',
     *                             'ENERGY.'
                      stop
                endif                                                   0203YC98
             else                                                       0626YC97
                 write(fu6,*)' ERROR:  variable ENERGY must',           0626YC97
     *             ' have an argument'                                  0626YC97
                 stop                                                   0626YC97
             end if                                                     0626YC97
         else
            write(fu6,1300) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
1300  format(3x,'ERROR:  invalid variable in ICOPT: ', a80)
      return
      end subroutine ricopt
c
c***********************************************************************
c     rlct
c***********************************************************************
c
      subroutine rlct(string,istrt)
      use perconparam, only : fu5,fu6,natom
      use kintcm
      use keyword_interface
      use rate_const; use potmod, only : irtpjac
c
c     Subroutine to read in the variables for large-curvature tunneling
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
         narg = narg+1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  lctopt section must end with an END')
            stop
         end if
c
c
         if (string(istrt:istrt+7).eq.'nolctrst') then                  0708JC00
             ilcrst=0                                                   0708JC00
         else if (string(istrt:istrt+5).eq.'lctrst') then               0708JC00
             ilcrst=1                                                   0708JC00
         else if (string(istrt:istrt+7).eq.'nolctstr') then             0708JC00
             ilcstr=0                                                   0708JC00
         else if (string(istrt:istrt+5).eq.'lctstr') then               0708JC00
             ilcstr=1                                                   0708JC00
         else if (string(istrt:istrt+5).eq.'ilct1d') then               0507AR02
             ilcgit=1                                                   0507AR02
         else if (string(istrt:istrt+5).eq.'ilct2d') then               AFRnov04
             ilcgit=2                                                   AFRnov04
         else if (string(istrt:istrt+5).eq.'prprob') then               0708JC00
             ipprob = 1
         else if (string(istrt:istrt+5).eq.'prfreq') then 
             ipfreq = 1
         else if (string(istrt:istrt+7).eq.'novadavg') then             0708JC00
             ivavg=0                                                    0708JC00
         else if (string(istrt:istrt+5).eq.'vadavg') then               0708JC00
             ivavg=1                                                    0708JC00
c
         else if (string(istrt:istrt+8).eq.'lcgmethod') then            0808JC00
            call rword(string,istrt,ierr)                               0808JC00

c           check that a value is also on the line                      0808JC00
            if (ierr.eq.1) then                                         0808JC00
                write(fu6,1025)                                         0808JC00
                stop                                                    0808JC00
            end if                                                      0808JC00
1025        format(3x,'ERROR: must specify a number with LCGMETHOD')    0808JC00
            ilct = icint(string(istrt:80)) - 2                          0808JC00
c
c     check value of ilct
c
            if ((ilct.lt.1).or.(ilct.gt.2)) then                        0808JC00
                write(fu6,1030)                                         0808JC00
                stop                                                    0808JC00
            end if                                                      0808JC00
1030        format(3x,'ERROR: valid options for LCGMETHOD are 3 or 4')  0808JC00

C
C     Detail of LCT (unit fu40) input has been moved to unit fu5 
C     and the code is now in rtunnl and rdetil
C
C        else if (string(istrt:istrt+7).eq.'prdetail') then             0502WH94
C            ipdat = 1                                                  0502WH94

         else if (string(istrt:istrt+6).eq.'vefsrch') then              0708JC00
            call rword(string,istrt,ierr)                               0708JC00

c           check that a value is also on the line                      0708JC00
            if (ierr.eq.1) then                                         0708JC00
                write(fu6,1050)                                         0708JC00
                stop                                                    0708JC00
            end if                                                      0708JC00
1050        format(3x,'ERROR: must specify a number with VEFSRCH')      0708JC00
            nvef = icint(string(istrt:80))                              0708JC00
c
         else if (string(istrt:istrt+6).eq.'ngtheta') then 
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1100) 
                stop
            end if
1100        format(3x,'ERROR:  must specify quadrature with NGTHETA') 
            ng = icint(string(istrt:80))
c
         else  if (string(istrt:istrt+4).eq.'ngamp') then
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1200) 
                stop
            end if
1200        format(3x,'ERROR:  must specify quadrature with NGAMP') 
            ngs0 = icint(string(istrt:80))
c
         else  if (string(istrt:istrt+10).eq.'interpolate') then
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1250) 
                stop
            end if
1250        format(3x,'ERROR:  must specify order with INTERPOLATE') 
            intlct = icint(string(istrt:80))
c 
         else if (string(istrt:istrt+5) .eq. 'rtpjac' )then             0411PJ01 
            irtpjac = 1                                                 0411PJ01
c
         else
            write(fu6,1300) string(istrt:iend) 
            stop
         end if
1300     format(3x,'ERROR:  invalid variable in LCTOPT: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rlct
C
C**********************************************************************
C RLCTGRD
C**********************************************************************
C
      SUBROUTINE rlctgrd
      use perconparam, only : fu5,fu6,natom
      use kintcm
      use keyword_interface
      use rate_const; use potmod, only : irtpjac
C
C     Read the ILCT1D or ILCT2D grid information
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER*80 WORD(40)
      LOGICAL LSEC,LEOF
C
10    CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)
      IF (WORD(1) .NE. 'END') THEN
         IF (LSEC .OR. LEOF) THEN
            WRITE(FU6,3000) 
            STOP 'LCTGRID 1'
         ENDIF
C
3000  FORMAT(/1X,'The LCTGRID section must end with an END')
C
         IF (WORD(1) .NE. 'STATE' .OR. NWORD .NE. 2) THEN
            WRITE(FU6,3200)
            STOP 'LCTGRID 2'
         ELSE
            ISTATE = ICINT(WORD(2))
         ENDIF 
C
3200  FORMAT(/1X,'The following line is expected in LCTGRID section:',
     *       /1X,'STATE n, where n is 0 or a positive integer.')
3300  FORMAT(/1X,'A line with one number is expected after STATE.')
3400  FORMAT(/1X,'A line with two numbers are expected after STATE.')
3500  FORMAT(/1X,'Number in STATE between 3 and 21.')
C
         CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)
         IF(ILCGIT.EQ.1) THEN
           IF (NWORD .NE. 1) THEN
             WRITE(FU6,3300)
             STOP 'LCTGRID 3'
           ELSE
             DUMP=ICINT(WORD(1))
             IF(DUMP.LT.3.OR.DUMP.GT.21) THEN
              WRITE(FU6,3500)
              STOP 'LCTGRID 4'
             ELSE
              NSPLIC(ISTATE+1) = DUMP
             ENDIF
           ENDIF
         ELSE IF(ILCGIT.EQ.2) THEN
           IF(NWORD.NE.2) THEN
             WRITE(FU6,3400) 
             STOP 'LCTGRID 5'
           ELSE
             DUMP1=ICINT(WORD(1))
             DUMP2=ICINT(WORD(2))
             IF(DUMP1.LT.3.OR.DUMP1.GT.21.OR.
     &DUMP2.LT.3.OR.DUMP2.GT.21) THEN
              WRITE(FU6,3500)
              STOP 'LCTGRID 6'
             ELSE
              NSPLIX(ISTATE+1) = DUMP1
              NSPLIY(ISTATE+1) = DUMP2
             ENDIF
           ENDIF
         ENDIF
C
         GOTO 10
C
      ENDIF
C
      RETURN
C
      END subroutine rlctgrd
c
c ***************************************************************************
c     rline
c ***************************************************************************
c
      subroutine rline(iunitr,string,istrt,isect,iend)
      use perconparam, only : fu5,fu6
c
c      This subroutine finds the first non-comment and non-blank line 
c      and the location of the first character on that line.  The contents
c      of the line are placed in the variable string.  It will 
c      then change it to all lower case using the routine 'case'.
c      A flag is set if the line is the start of a new section.
c
c      Modified to read from unit iunitr       0810JC97
c
c      istrt:   location in 80 character string that is the first non-blank
c      isect:   a 1 is returned if the first non-blank character is a * 
c               signalling the start of a section
c      iend:    a 1 is returned if the end of file 5 is encountered
c      string:  80 character string that is the contents of the current line
c
      implicit double precision (a-h,o-z)
c
      character * 80 string,case
      external case
c
1000  format(A80)
c
c       initialize variables
c
      isect = 0
      iend = 0
      iblnk = 1
c
c     read in next line in file 5 - find first non-blank character and
c     determine if it is a comment (# sign signals a comment).  Once a
c     comment is found the rest of the line is skipped (by setting the
c     counter to the last character [i=80])
c
      do while (iblnk.eq.1)
         read(iunitr,1000,end=9999) string
         i = 1
         do while (iblnk.eq.1.and.i.le.80)
            if (string(i:i).ne.' ') then
               if (string(i:i).ne.'#') then
                   iblnk = 0
                else
                   i = 80
                end if
            end if
            i = i+1
         end do
       end do
c
c      set location of first non-blank character
       istrt = i-1
c
c      check if it is a section header (first character a *)
c
       if (string(istrt:istrt).eq.'*') isect=1
c
c      strip of any trailing comments on the line
c
       do 1 j=istrt,80
1        if (string(j:j).eq.'#') string(j:80) = ' '
c
c      change to lower case
c
       string = case(string)
       return
9999   iend=1
       return
       end subroutine rline
c
c***********************************************************************
c     rlowfr
c***********************************************************************
c
      subroutine rlowfr(string,istrt)
      use perconparam
      use kintcm
      use cm
      use keyword_interface, only : gufac5
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      dimension valfl(3)
c
      if(.not.allocated(ifqlow))then
         allocate(ifqlow(n3tm)); ifqlow=0
      end if
      if(.not.allocated(frelow)) then
         allocate(frelow(3,N3TM));frelow=0d0
      end if
      do i = 1,n3tm
       ifqlow(i) = 0
      enddo
c
      call rline(fu5,string,istrt,isect,iend)
      iclf = 1
      nxm = 1
      flsr = -1.0d0
      flsp =  1.0d0
c
      do while(string(istrt:istrt+2).ne.'end'.and.nxm.le.3*natom)
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
           write(fu6,1000)
1000       format(3x,'ERROR: variable IVTST0FREQ option must end with'
     >               ,' END')
           stop
         endif
         if (string(istrt:istrt+3).eq.'mode') then
           do i = 1,3
             valfl(i) = 0.0d0
           enddo
           call rline(fu5,string,istrt,isect,iend)
           do while(string(istrt:istrt+2).ne.'end')
             if (isect.eq.1.or.iend.eq.1) then
               write(fu6,1100)
1100           format(3x,'Variable MODE option must end with END')
               stop
             endif
             if (string(istrt:istrt+4).eq.'index') then
               call rword(string,istrt,ierr)
               if(ierr.ne.1) then
                 iflmode = icint(string(istrt:80))
               else
                 write(fu6,*)'ERROR:  variable index must have an',
     *                        ' argument'
                 stop
               endif
             elseif (string(istrt:istrt+7).eq.'reactant') then
               call rword(string,istrt,ierr)
               if(ierr.ne.1) then
                 valfl(1) = cfloat(string(istrt:80))
               else
                 write(fu6,*)'ERROR:  variable reactant must have an',
     *                        ' argument'
                 stop
                endif
             elseif (string(istrt:istrt+6).eq.'product') then
              call rword(string,istrt,ierr)
              if(ierr.ne.1) then
                valfl(3) = cfloat(string(istrt:80))
              else
                write(fu6,*)'ERROR:  variable product must have an',
     *                        ' argument'
                stop
              endif
             elseif (string(istrt:istrt+4).eq.'start') then
              call rword(string,istrt,ierr)
              if(ierr.ne.1) then
                valfl(2) = cfloat(string(istrt:80))
              else
                write(fu6,*)'ERROR:  variable start must have an',
     *                        ' argument'
                stop
              endif
             elseif (string(istrt:istrt+1).eq.'sr') then
              call rword(string,istrt,ierr)
              if(ierr.ne.1) then
                 flsr = cfloat(string(istrt:80))*gufac5                 0405JZ07
              else
                 write(fu6,*)'ERROR:  variable sr must have an',
     *                        ' argument'
                 stop
              endif
             elseif (string(istrt:istrt+1).eq.'sp') then
              call rword(string,istrt,ierr)
              if(ierr.ne.1) then
                flsp = cfloat(string(istrt:80))*gufac5                  0405JZ07
              else
                write(fu6,*)'ERROR:  variable flsp must have an',
     *                        ' argument'
                 stop
              endif
             else
               write (fu6,*) 'ERROR: not a IVTST0FREQ option'
               stop
             endif
             call rline(fu5,string,istrt,isect,iend)
           enddo
           do i = 1, 3
             ifqlow(iflmode)=1
             frelow(i,iflmode)=valfl(i)/AUTOCM
           enddo
         endif
         nxm = nxm+1
         call rline(fu5,string,istrt,isect,iend)
      enddo
      return
      end subroutine rlowfr
c
c***********************************************************************
c     rmuvt
c***********************************************************************
c
      subroutine rmuvt(string,istrt)
      use common_inc
      use perconparam
      use kintcm
      use cm
      use keyword_interface, only : gufac5
c
c     Subroutine to read in the variables for muVT rate calculation
c     This handles the muVTOPT list keyword.
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 8 fittyp
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
         narg = narg+1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  muvtopt section must end with an END')
            stop
         end if
c
c
c NITER
         if (string(istrt:istrt+4).eq.'niter') then 
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1100) 
                stop
            end if
1100        format(3x,'ERROR:  must specify a number with NITER') 
            mniter = icint(string(istrt:80))
c                     check that the value is valid
            nemvt = 90
            if (niter.gt.nemvt) then 
                write (fu6,*) ' Maximum number of Gauss-Laguerre ',
     *         'points for Boltzmann average of muVT results is', 
     *          nemvt,' niter reset to', nemvt
                niter = nemvt
            end if
c FIT
         else if (string(istrt:istrt+3).eq.'fit') then 
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1200)
                stop
            end if
1200        format(3x,'ERROR:  must specify a string with FIT')
            fittyp = string(istrt:80)
            if (fittyp.eq.'gas53'.or.fittyp.eq.'gsolid53') then
              ifit1 = 5
              ifit2 = 3
            else if (fittyp.eq.'gas35') then
              ifit1 = 3
              ifit2 = 5
            else if (fittyp.eq.'gas15'.or.fittyp.eq.'gsolid15') then
              ifit1 = 1
              ifit2 = 5
            else if (fittyp.eq.'gsolid13'.or.fittyp.eq.'gas13') then    0228JZ07
              ifit1 = 1
              ifit2 = 3
            else if (fittyp.eq.'gsolid31'.or.fittyp.eq.'gas31') then    0228JZ07
              ifit1 = 3
              ifit2 = 1
            else
               write(fu6,1300) fittyp
               stop
            end if
1300        format(3x,'ERROR:  invalid fit specified by FIT in ',
     *        'muVTOPT: ', a80)
c PRENERGY
         else if (string(istrt:istrt+7).eq.'prenergy') then 
             call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1400)
                stop
            end if
1400        format(3x,'ERROR:  must specify a number with PRENERGY')
            mnprmv = icint(string(istrt:80))
c SLOWER
         else if (string(istrt:istrt+5).eq.'slower') then 
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1500) 
                stop
            end if
1500        format(3x,'ERROR:  must specify s value with SLOWER') 
            xsmmvt = cfloat(string(istrt:80))*gufac5                    0405JZ07
c SUPPER
         else if (string(istrt:istrt+5).eq.'supper') then 
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1600) 
                stop
            end if
1600        format(3x,'ERROR:  must specify s value with SUPPER') 
            xspmvt = cfloat(string(istrt:80))*gufac5                    0405JZ07
c
         else if(string(istrt:istrt+4).eq.'egrid') then
            call rword(string,istrt,ierr)
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1650)
                stop
            end if
1650        format(3x,'ERROR:  must specify a number with EGRID')
            egrid = cfloat(string(istrt:80))*cmtoau
c
         else
            write(fu6,1700) string(istrt:iend) 
            stop
         end if
1700     format(3x,'ERROR:  invalid variable in muVTOPT: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rmuvt
c
c***********************************************************************
c     ropt
c***********************************************************************
c
      subroutine ropt(string,iend,istrt)
      use perconparam
      use kintcm
      use cm
      use keyword_interface, only : gufac5,potgeo
c
c     subroutine to read in the input in the optimization section.  
c
c     called by:
c           read5
c
      implicit double precision (a-h,o-z)
      character * 80 string
      logical TSOPT
c
c     read in first keyword in this section
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
c OPTMIN
         if (string(istrt:istrt+5).eq.'optmin') then
            TSOPT = .FALSE. 
            call roptop(string,iend,istrt,TSOPT)
c OPTTS
         else if(string(istrt:istrt+4).eq.'optts') then
            TSOPT = .TRUE. 
            call roptop(string,iend,istrt,TSOPT)
c GAMESS
         else if (string(istrt:istrt+3).eq.'gamess') then
          potgeo='gamess'
          igpot=3
c ACESOPTTS                                                             0425RS95
         else if (string(istrt:istrt+8).eq.'acesoptts') then
            potgeo='aces'
            igpot=2
            call races(string,istrt,11)
c ACESOPT                                                               0425RS95
         else if (string(istrt:istrt+6).eq.'acesopt') then
            potgeo='aces'
            igpot=2
            call races(string,istrt,10)
c ACES                                                                  0425RS95
         else if (string(istrt:istrt+3).eq.'aces') then
          potgeo='aces'
          igpot=2
c PRINT
         else if (string(istrt:istrt+4).eq.'print') then
            iprxnt = 1 
c NOPRINT
         else if (string(istrt:istrt+6).eq.'noprint') then
            iprxnt = 0 
c BFGS                                                                   IR0495
c         else if (string(istrt:istrt+3).eq.'bfgs') then
c            ibfgs = 1
c TSBFGS                                                                 IR0495
c         else if (string(istrt:istrt+5).eq.'tsbfgs') then
c            ibfgst = 1
c NOBFGS                                                                 IR0495
c         else if (string(istrt:istrt+5).eq.'nobfgs') then
c            ibfgs = 0
c NOTSBFGS                                                               IR0495
c         else if (string(istrt:istrt+7).eq.'notsbfgs') then
c            ibfgst = 0
c RETRY                                                                  IR0495
         else if (string(istrt:istrt+4).eq.'retry') then
            iretry =1
c NORETRY                                                                IR0495
         else if (string(istrt:istrt+6).eq.'noretry') then
            iretry =0
c SDSTART                                                                IR0495
         else if (string(istrt:istrt+6).eq.'sdstart') then
            ihunit=1
c NOSDSTART                                                              IR0495
         else if (string(istrt:istrt+8).eq.'nosdstart') then
            ihunit=0
c SCALE
         else if (string(istrt:istrt+4).eq.'scale') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               scale = cfloat(string(istrt:80))
            else
               write(fu6,*)'ERROR:  variable scale must have an',
     *                        ' argument'
               stop
            end if
c DLX2
         else if (string(istrt:istrt+3).eq.'dlx2') then                 0528WH94
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               dlx1 = cfloat(string(istrt:80))
               dlx1 = dlx1*gufac5                                       0405JZ07
               ifirst = 1
            else
               write(fu6,*)'ERROR:  variable dlx2 must have an argument'
               stop
            end if
c CONVERGE                                                               IR0495
         else if (string(istrt:istrt+7).eq.'converge') then
            write(fu6,'("CONVERGE keyword obsolete.",/,
     *                  "Use GCOMP and TSGCOMP instead.")')
            stop
c STPTOL                                                                IR0495
         else if (string(istrt:istrt+5).eq.'stptol') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               stptol = cfloat(string(istrt:80))*gufac5                 0405JZ07
            else
             write(fu6,*)'ERROR:  variable stptol must have an argument'    
             stop
            end if
c GCOMP                                                                  IR0495
         else if (string(istrt:istrt+4).eq.'gcomp') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               convg = cfloat(string(istrt:80))
            else
             write(fu6,*)'ERROR:  variable gcomp must have an argument'    
             stop
            end if
c TSGCOMP                                                                IR0495
         else if (string(istrt:istrt+6).eq.'tsgcomp') then
          call rword(string,istrt,ierr)
          if(ierr.ne.1) then
             convgt = cfloat(string(istrt:80))
          else
           write(fu6,*)'ERROR:  variable tsgcomp must have an argument'
           stop
          end if
c NITER
         else if (string(istrt:istrt+4).eq.'niter') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               jniter = icint(string(istrt:80))
            else
               write(fu6,*)'ERROR:  variable niter must have an',
     *                        ' argument'
               stop
            end if
c HREC                                                                   IR0495
         else if (string(istrt:istrt+3).eq.'hrec') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               ihrec = icint(string(istrt:80))
               if( ihrec .eq. 1 ) ibfgs=0
            else
               write(fu6,*)'ERROR:  variable hrec must have an',
     *                        ' argument'
               stop
            end if
c TSHREC                                                                 IR0495
         else if (string(istrt:istrt+5).eq.'tshrec') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               ihrect = icint(string(istrt:80))
               if( ihrect .eq. 1 ) ibfgst=0
            else
               write(fu6,*)'ERROR:  variable tshrec must have an',
     *                        ' argument'
               stop
            end if
c
c EFOPT
         else if (string(istrt:istrt+4).eq.'efopt') then                     0317YC99
            call refopt                                                      0317YC99
         else
            write(fu6,1200) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
1200  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' OPTIMIZATION section',/A80)
c
      return
      end subroutine ropt

c
c***********************************************************************
c     roptop
c***********************************************************************
c
      subroutine roptop(string,iend,istrt,TSOPT)
      use perconparam, only : fu6
      use kintcm
      use cm
      use keyword_interface, only : potgeo
c
c     subroutine to read in the options in optmin and optts keywords
c
c     called by:
c           ropt
c
      implicit double precision (a-h,o-z)
      character * 80 string
      logical  TSOPT

      call rword(string,istrt,ierr)                                     0214YC97
      if(ierr.ne.1) then                                                0214YC97
          if (string(istrt:istrt+4).eq. 'ohook') then                   0214YC97
c OHOOK
              igpot = 1                                                 0214YC97
              potgeo = 'ohook'                                          0312YC97
          else if (string(istrt:istrt+3).eq.'bfgs') then                0214YC97
c BFGS
              if (TSOPT) then 
                 ibfgst = 1 
              else
                 ibfgs = 1                                              0214YC97
              endif
          else if (string(istrt:istrt+1).eq.'nr') then                  0214YC97
c NR
              if (TSOPT) then 
                 ibfgst = 0
              else
                 ibfgs = 0                                              0214YC97
              endif
c EF
          else if (string(istrt:istrt+1).eq.'ef') then                  0317YC99
              if (TSOPT) then                                           0317YC99
                ieft = 1                                                0317YC99
              else                                                      0317YC99
                ief = 1                                                 0317YC99
              endif                                                     0317YC99
          endif                                                         0317YC99
      else
          if (TSOPT) then 
                 write(fu6,*)'ERROR:  variable OPTTS must have an',     0214YC97
     *                         ' argument'                              0214YC97
                 stop                                                   0214YC97
          else
                 write(fu6,*)'ERROR:  variable OPTMIN must have an',    0214YC97
     *                        ' argument'                               0214YC97
                 stop                                                   0214YC97
          endif
      endif
      return
      end subroutine roptop
c
c ***************************************************************************
c     rpath
c ***************************************************************************
c
      subroutine rpath(string,iend,istrt,PARALLEL)
      use common_inc
      use perconparam
      use kintcm
      use keyword_interface; use cm, only : sdebg1,sdebg2
      use rate_const; use potmod, only : ifqfac
c
c     Subroutine to read in the input for following the reaction path
c
      implicit double precision (a-h,o-z)

      character * 80 string
c
      LOGICAL PARALLEL
      logical icdef
      call rpath_mem
      icdef = .false.                                                   0110PJ01
c
c     set flag that path is present
c
      ipath = 1
c 
c     read in first keyword in this section
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
C PARALLEL                                                              1219BL04
         if (string(istrt:istrt+7).eq.'parallel') then                  1219BL04             
            parallel=.TRUE.                                             1219BL04

c NEXTPT
         else  if (string(istrt:istrt+5).eq.'nextpt') then              1219BL04
            inxtpt = 1
c NONEXTPT
         else  if (string(istrt:istrt+7).eq.'nonextpt') then   
            inxtpt = 0
c CALCMEFF                                                               912RS96
         else if (string(istrt:istrt+7).eq.'calcmeff') then              912RS96
            imeff = 1                                                    912RS96
c HESS
         else if (string(istrt:istrt+3).eq.'hess') then
            ihess = 1
c NOHESS
         else  if (string(istrt:istrt+5).eq.'nohess') then   
            ihess = 0
c INTMU
         else if (string(istrt:istrt+4).eq.'intmu') then                0327YC97
            call rword (string,istrt,ierr)                              0327YC97
            if (ierr.ne.1) then                                         0327YC97
              intmu = icint(string(istrt:80))                           0327YC97
              if (intmu.ne.1.and.intmu.ne.3) then                       0327YC97
                write (fu6,*) 'ERROR: INTMU can be either 1 ',          0327YC97
     *               'or 3'                                             0327YC97
                stop                                                    0327YC97
              endif                                                     0327YC97
            else                                                        0327YC97
              write(fu6,*) 'ERROR: variable INTMU must ',               0327YC97
     *                'have an argument'                                0327YC97
              stop                                                      0327YC97
            end if                                                      0327YC97
c SCALEMASS
         else if (string(istrt:istrt+8).eq.'scalemass') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               redm = cfloat(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable SCALEMASS must ',
     *                    'have an argument'
               stop
            end if
c SRANGE
         else if (string(istrt:istrt+5).eq.'srange') then
            call rrange(string,istrt)
c SVRC                                                                 
         else if (string(istrt:istrt+3).eq.'svrc') then                 1029JZ07
            call rsvrc(string,istrt)
c SSTEP
         else if (string(istrt:istrt+4).eq.'sstep') then
            call rword(string,istrt,ierr)
            if (ierr.ne.1) then
               del = cfloat(string(istrt:80))*gufac5                    0405JZ07
            else
               write(fu6,*)' ERROR:  variable SSTEP must ',
     *                    'have an argument'
               stop
            end if
c NSTEPS
         else if (string(istrt:istrt+5).eq.'nsteps') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               nst = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable NSTEPS must ',
     *                    'have an integer argument'
               stop
            end if
c FIRSTSTEP
         else if (string(istrt:istrt+8).eq.'firststep') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               fstep = string(istrt:istrt+4) 
            else
               write(fu6,*)' ERROR:  variable FIRSTSTEP must ',
     *                    'have a character argument'
               stop
            end if
c COORD                                                                 07/95KAN
         else if (string(istrt:istrt+4).eq.'coord') then                07/95KAN
            call rword(string,istrt,ierr)                               07/95KAN
            if(ierr.ne.1) then                                          07/95KAN
               coord = string(istrt:istrt+4)                            07/95KAN
            else                                                        07/95KAN
               write(fu6,*)' ERROR:  variable COORD must ',             07/95KAN
     *                    'have a character argument'                   07/95KAN
               stop                                                     07/95KAN
            end if                                                      07/95KAN
c INTDEF                                                                07/95KAN
         else if (string(istrt:istrt+5).eq.'intdef') then               07/95KAN
            call readint(ierr)                                          07/95KAN
            if(ierr.ne.1) then                                          07/95KAN
               icdef = .true.                                           0110PJ00
               call intcor(ifcfac)                                      0217PJ00
            else                                                        07/95KAN
               write(fu6,*)' ERROR IN READING INTDEF'                   07/95KAN
               stop                                                     07/95KAN
            end if                                                      07/95KAN
c SST
         else if (string(istrt:istrt+4).eq.'sstor') then
             call rsst

c SDEBG1                                                                07/95KAN
         else if (string(istrt:istrt+5).eq.'sdebg1') then               07/95KAN
            call rword(string,istrt,ierr)                               07/95KAN
            if(ierr.ne.1) then                                          07/95KAN
               sdebg1 = cfloat(string(istrt:80))*gufac5                 07/95KAN
            else                                                        07/95KAN
               write(fu6,*)' ERROR:  variable SDEBG1 must ',            07/95KAN
     *                    'have an argument'                            07/95KAN
               stop                                                     07/95KAN
            end if                                                      07/95KAN
c SDEBG2                                                                07/95KAN
         else if (string(istrt:istrt+5).eq.'sdebg2') then               07/95KAN
            call rword(string,istrt,ierr)                               07/95KAN
            if(ierr.ne.1) then                                          07/95KAN
               sdebg2 = cfloat(string(istrt:80))*gufac5                 07/95KAN
            else                                                        07/95KAN
               write(fu6,*)' ERROR:  variable SDEBG1 must ',            07/95KAN
     *                    'have an argument'                            07/95KAN
               stop                                                     07/95KAN
            end if                                                      07/95KAN
c PRPATH                                                                07/95KAN
         else if (string(istrt:istrt+5).eq.'prpath') then               07/95KAN
            call rprmep                                                 07/95KAN
c
c IDIRECT
         else if (string(istrt:istrt+6).eq.'idirect') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               isen = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable IDIRECT must ',
     *                    'have an +1 or -1 argument'
               stop
            end if
c SADDLE                                                                0210JC97
         else if (string(istrt:istrt+5).eq.'saddle') then               0210JC97
            inosad = 0                                                  0210JC97
c NOSADDLE                                                              0210JC97
         else if (string(istrt:istrt+7).eq.'nosaddle') then             0210JC97
            inosad = 1                                                  0210JC97
c SFIRST
         else if (string(istrt:istrt+5).eq.'sfirst') then
            isfrst = 1
            call rsfrst(string,istrt)
c SIGN
         else if (string(istrt:istrt+3).eq.'sign') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               psign = string(istrt:istrt+6) 
            else
               write(fu6,*)' ERROR:  variable SIGN must ',
     *                    'have a character argument'
               stop
             end if
c SPECSTOP
         else if (string(istrt:istrt+7).eq.'specstop') then             0423TA02
            call rsstop                                                 0423TA02
c DLX3 
         else if (string(istrt:istrt+3).eq.'dlx3') then                 0528WH94
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               d3lx = cfloat(string(istrt:80))*gufac5                   0405JZ07
            else
               write(fu6,*)' ERROR:  variable DLX3 must ',
     *                    'have an argument'
               stop
            end if
c RODS
         else if (string(istrt:istrt+3).eq.'rods') then                 0930PF97
            irods = 1                                                   0930PF97
c NORODS
         else if (string(istrt:istrt+5).eq.'norods') then               0930PF97
            irods = 0                                                   0930PF97
c ES1OPT                                                                1118PF97
         else if (string(istrt:istrt+5).eq.'es1opt') then
            ieuler = 0
            ies1 = 1
            ipagem = 0
            call res1(string,istrt)
c RPM                                                                   1011PF97
         else if (string(istrt:istrt+2).eq.'rpm') then                  1011PF97
            call rrpm(string,istrt)                                     1011PF97
c INI
         else if (string(istrt:istrt+2).eq.'ini') then                  0202YC98
            call rword(string,istrt,ierr)                               0202YC98
            if(ierr.ne.1) then                                          0202YC98
               ini = icint(string(istrt:80))                            0202YC98
            else                                                        0202YC98
               write(fu6,*)' ERROR:  variable INI must ',               0202YC98
     *                    'have an integer argument'                    0202YC98
               stop                                                     0202YC98
            end if                                                      0202YC98
c INH
         else if (string(istrt:istrt+2).eq.'inh') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               inh = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable INH must ',
     *                    'have an integer argument'
               stop
            end if
c CURV
         else if (string(istrt:istrt+3).eq.'curv') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               curv = string(istrt:istrt+6) 
            else
               write(fu6,*)' ERROR:  variable CURV must ',
     *                    'have a character argument'
               stop
            end if
c SSPECIAL
         else if (string(istrt:istrt+7).eq.'sspecial') then
            ispec = 1
            call rspecl(string,istrt)
c SSPECPR
         else if (string(istrt:istrt+6).eq.'sspecpr') then              0517WH94
            ispcpr = 1
c SYMMETRY
         else if (string(istrt:istrt+7).eq.'symmetry') then
            isym = 1
c NOSYMMETRY
         else if (string(istrt:istrt+9).eq.'nosymmetry') then
            isym = 0
c SCALERP
         else if (string(istrt:istrt+6).eq.'scalerp') then              0109BE07
            iscalerp = 1                                                0109BE07
c VSCALE
         else if (string(istrt:istrt+5).eq.'vscale') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               vfac = cfloat(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable VSCALE must ',
     *                    'have an argument'
               stop
            end if
c FREQSCALE                                                             0808JC00
         else if (string(istrt:istrt+8).eq.'freqscale') then            0808JC00
            call rword(string,istrt,ierr)                               0808JC00
            if(ierr.ne.1) then                                          0808JC00
               freqfac = cfloat(string(istrt:80))                       0808JC00

c --- commented by pu                                                   0817PJ01
c              ifrfac = 1                                               0808JC00

               ifqfac = 1                                               0815PJ01 
            else                                                        0808JC00
               write(fu6,*)' ERROR:  variable FREQSCALE must ',         0808JC00
     *                    'have an argument'                            0808JC00
               stop                                                     0808JC00
            end if                                                      0808JC00
C FREQINCR
         else if(string(istrt:istrt+8).eq.'freqincr') then              1008JZ09
           call rword(string,istrt,ierr)
           if(ierr.ne.1) then
             freqbottom=cfloat(string(istrt:80))
             incrf = 1
           else
             write(fu6,*)' ERROR:  variable freqincr must ',         
     *                    'have an argument'                            
               stop                                                     
            end if                                                      
c FCSCALE                                                               1227PJ00
         else if (string(istrt:istrt+6).eq.'fcscale') then              1227PJ00
             call rfcfac(ierr)                                          1227PJ00
      
             if (ierr .eq. 1) then 
                 write(fu6,*)'ERROR: reading the force constant ',      0217PJ01
     *                       'scaling factors in FCSCALE'               0217PJ01
                 stop                                                   0217PJ01
             else                                                       0217PJ01
                 ifcfac = 1                                             0217PJ01 
             end if                                                     0217PJ01
             if (icdef) then                                            0110PJ00 
                 call intcor(ifcfac)                                    0110PJ00
             end if                                                     0217PJ01
c SCLPT                                                                 0211PJ01
         else if (string(istrt:istrt+4) .eq. 'sclpt') then              0211PJ01
                isclpt = 1                                              0211PJ01
c NOSCLPT                                                               0211PJ01
         else if (string(istrt:istrt+6) .eq. 'nosclpt') then            0211PJ01
                isclpt = 0                                              0211PJ01
c CARTRP                                                                0214PJ01
         else if (string(istrt:istrt+5) .eq. 'cartrp') then             0215PJ01
                icartrp = 1                                             0815PJ01
c NOCARTRP
         else if (string(istrt:istrt+7) .eq. 'nocartrp') then           0619PJ01
                icartrp = 0                                             0619PJ01
     
c ISWR  
         else if (string(istrt:istrt+3).eq.'iswr') then                 0527WH94
            call rword(string,istrt,ierr)                               0527WH94
            if(ierr.ne.1) then                                          0527WH94
               iswr = icint(string(istrt:80))                           0527WH94
            else                                                        0527WH94
               write(fu6,*)' ERROR:  variable ISWR must ',              0527WH94
     *                    'have an argument'                            0527WH94
               stop                                                     0527WH94
            end if                                                      0527WH94
c ISWP  
         else if (string(istrt:istrt+3).eq.'iswp') then                 0527WH94
            call rword(string,istrt,ierr)                               0527WH94
            if(ierr.ne.1) then                                          0527WH94
               iswp = icint(string(istrt:80))                           0527WH94
            else                                                        0527WH94
               write(fu6,*)' ERROR:  variable ISWP must ',              0527WH94
     *                    'have an argument'                            0527WH94
               stop                                                     0527WH94
            end if                                                      0527WH94
c IVTSTMOPT                                                             0601YC98
         elseif (string(istrt:istrt+8).eq.'ivtstmopt') then             0601YC98
            call rivtm(string,istrt)                                    0601YC98
c BATH
         elseif (string(istrt:istrt+3).eq.'bath') then                  0317YC99
            ibathm=1                                                    0317YC99
            call rbath(string,istrt)                                    0317YC99
c IVTST0FREQ 
         elseif (string(istrt:istrt+9).eq.'ivtst0freq') then
            call rlowfr(string,istrt)
c EXFIRST
         else if (string(istrt:istrt+6).eq.'exfirst') then
            iexrct = 1
            idir = 1
            call rextrp(string,iend,istrt,idir)
c EXSECOND
         else if (string(istrt:istrt+7).eq.'exsecond') then
            iexprd = 1
            idir = 2
            call rextrp(string,iend,istrt,idir)
c SPRNT
         else if (string(istrt:istrt+4).eq.'sprnt') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               sprnt = cfloat(string(istrt:80))*gufac5                  0405JZ06
            else
               write(fu6,*)' ERROR:  variable SPRNT must ',
     *                    'have an argument'
               stop
            end if
c POTINF
         else if (string(istrt:istrt+5).eq.'potinf') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               nptinf = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable POTINF must ',
     *                    'have an argument'
               stop
            end if
c REORDER
         else if (string(istrt:istrt+6).eq.'reorder') then              1210YC96
            ireord = 1                                                  1210YC96
c NOREORDER
         else if (string(istrt:istrt+8).eq.'noreorder') then            1210YC96
            ireord = 0
c PRINTSTEP
         else if (string(istrt:istrt+8).eq.'printstep') then
            iprstp = 1
c NOPRINTSTEP
         else  if (string(istrt:istrt+10).eq.'noprintstep') then   
            iprstp = 0
c PRSAVERP
         else if (string(istrt:istrt+7).eq.'prsaverp') then
            iprsve = 1
            call rword(string,istrt,ierr)                               0626YC97
            if(ierr.ne.1) then
               nprsmd = icint(string(istrt:80))                         0626YC97
            end if
c NOPRSAVERP
c         else  if (string(istrt:istrt+9).eq.'noprsaverp') then
c            iprsve = 0
c PRSAVEMODE
         else if (string(istrt:istrt+9).eq.'prsavemode') then
            iprsmd = 1
            call rword(string,istrt,ierr)                               0626YC97
            if(ierr.ne.1) then         
               nprsmd = icint(string(istrt:80))                         0626YC97
            end if
c PRDISTMX                                                              0507YC97
         else if (string(istrt:istrt+7).eq.'prdistmx') then             0507YC97
            call rword(string,istrt,ierr)                               0507YC97
            if(ierr.ne.1) then                                          0507YC97
               iprdis = icint(string(istrt:80))                         0507YC97
            else                                                        0507YC97
               write(fu6,*)' ERROR:  variable PRDISTMX must ',          0507YC97
     *                    'have an argument'                            0507YC97
               stop                                                     0507YC97
            end if                                                      0507YC97
c NOPRSAVEMODE
c         else  if (string(istrt:istrt+10).eq.'noprsavemode') then
c            iprsmd = 0
         else
            write(fu6,1000) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      delsv = del * dble(inh)                                           1118PF97
c
1000  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' path section',/A80)
c
      return
      end subroutine rpath
c****************************************************************
c     rpivot 
c****************************************************************
c 
      subroutine rpivot(string,istrt,jtype)
      use common_inc, only : npt
      use perconparam
      use cm, only : npvt,xpvt,ippsv,ivrc
      use keyword_interface, only : gufac5
c
c subroutine to read the coordinates of pivot points
c
c jtype = 1 first reactant
c jtype = 2 second reactant
c Note that jtype must be 1 or 2 here since VRC-TST is only for
c association reactions. 
c
      implicit double precision (a-h,o-z)
      character * 80 string
      if(.not.allocated(ippsv)) then
        allocate(ippsv(npivots,2),xpvt(n3pt,2))  
      endif
c
c Jtype must be 1 or 2 and others are not allowed 
c
      if (jtype.gt.2) then
        write(fu6,1000)
1000    format(3X,'Pivot point is only for reatant 1 or 2')
        stop
      endif      
c
c piovt point is only used for VRC-TST
c
      if (ivrc.eq.0) then
       write(fu6,1100) 
1100   format(3X,'Piovt point is only used for VRC-TST ',
     *           'VRC keyword must be specified in General section')
       stop
      endif
c 
c read the coordinates in the line until an 'end' is found 
c
      call rline(fu5,string,istrt,isect,iend)
      npt=1
c
      do while(string(istrt:istrt+2).ne.'end')
c
c
c        parse line to get atom number
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         jpvt = icint(string(ibegin:iend))
c
c check that jpvt is a legal pivot point number
c
         if(jpvt.gt.npivots.or.npt.gt.npivots) then
           write(fu6,1110) string
1110      format(3x,'ERROR: pivot point number exceed maximum allowed.',
     *            ' Invalid line is: ', 1x, a80)
           stop
         endif 
c
c save pivot point number
c
         ippsv(npt,jtype) = jpvt

           do 50 j=2, 0, -1
             ibegin = istrt
             call rword(string,istrt,ierr)
             iend = istrt - 1
c check that the coordinate was found on the line
           if (ierr.eq.1.and.j.ne.0) then
             write(fu6,1200) string
1200         format(3X,'ERROR: invalid cartesian coordinate in ',
     *              'pivot point. Invalid line is: ',/a80)
             stop
           endif
c save coordinate
c            pvtpt(jtype,j) = cfloat(string(ibegin:iend))*gufac5
             xpvt(jpvt*3-j,jtype) = cfloat(string(ibegin:iend))*gufac5
50         continue
c
c          ibegin = istrt
c          call rword(string,istrt,ierr)
c          iend = istrt - 1
c          if (ierr.eq.1) then
c            write(fu6,1300) string
c1300        format(3X,'ERROR: invalid reactive atom indentification ',
c    *              'number in pivot point. Invalid line is: ',/a80)
c            stop
c          endif
c          ip2r(jpvt,jtype) = icint(string(ibegin:iend))
C
           npt = npt+1
           call rline(fu5,string,istrt,isect,iend)
      enddo
      npvt(jtype)=npt - 1
c
      return 
      end subroutine rpivot
c
c ***************************************************************************
c     rpotet
c ***************************************************************************
c
      subroutine rpotet(string,istrt)
      use kintcm, only : ipot
      use keyword_interface, only : potnam
c
c     Subroutine to read in the potential type.  Currently polyrate will 
c     only support file 30 and analytical potentials.  This keyword must
c     have an associated argument on the same line or on the first 
c     succeeding non-blank line. 
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     search for the potential type on the same line as the potential keyword 
c
      call rword(string,istrt,ierr)
c
c     if ierr=1 then use the default of analytic otherwise use the 
c     value given
c
      if (ierr.eq.1) then
          potnam = 'hooks'                                              0312YC97
          ipot = 0
      else
          potnam = string(istrt:istrt+7)
          if (potnam.eq.'hooks') ipot=0                                 0327YC97
          if (potnam.eq.'unit30') ipot = 1                              0327YC97
          if (potnam.eq.'unit31') ipot = 1                              0810JC97
          if (potnam.eq.'unit40') ipot=1                                0327YC97
          if (potnam.eq.'aces') ipot=2                                  0327YC97
          if (potnam.eq.'unit29') ipot=3                                0327YC97
      end if
c
      return
      end subroutine rpotet
c
c ***************************************************************************
c     rpotge
c ***************************************************************************
c
      subroutine rpotge(string,istrt)
      use kintcm, only : igpot
      use keyword_interface, only : potgeo
c
c     Subroutine to read in the potential type to be used for optimizing
c     the geometry.  Currently polyrate will only support file 30,
c     mopac, and analytical potentials.  This keyword must
c     have an associated argument on the same line or on the first
c     succeeding non-blank line.
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     search for the potential type on the same line as the potential keyword
c
      call rword(string,istrt,ierr)
c
c     if ierr=1 then use the default of analytic otherwise use the
c     value given
c
      if (ierr.eq.1) then
          potgeo = 'polyrate'                                           0301YC97
          igpot = 0
      else
      end if
c
      return
      end subroutine rpotge
c
c**********************************************************************
c RPRMEP
c**********************************************************************
c
      subroutine rprmep
      use perconparam, only : fu5,fu6
      use rate_const, only : ifrind,iprca,ixmol,itvmep,nfrind,iprmep
      use kintcm, only : iprcd
c
c     This subroutine reads the options to print the mep information to fu25-27
c
      implicit double precision(a-h,o-z)
c
      character*80 word(40)
      logical lsec,leof
c
      iprmep = 1
      itvmep = 1
      iprcd = 0
10    call readln(fu5,word,nword,lsec,leof)
      if (word(1) .ne. 'END') then
         if (lsec .or. leof) then
            write(fu6,1000)
            stop 'RPRMEP 1'
         endif
c
1000  format(/1X,'The PRPATH section must end with an END')
c
         if (word(1) .eq. 'FREQ') then
            nfrind = nword - 1
            if (nfrind .ge. 1) iprmep = 2
            do i = 1, nfrind
               ifrind(i) = icint(word(1+i))
            enddo
         elseif(word(1) .eq. 'COORD') then                              0203YC98
            nprca = nword - 1                                           0203YC98
            if (nprca .ge. 2) iprcd = 1                                 0203YC98
            do i = 1, nprca                                             0203YC98
               iprca(i) = icint(word(1+i))                              0203YC98
            enddo
         elseif(word(1) .eq. 'XMOL') then
            ixmol = 1
         elseif(word(1) .eq. 'INTERVAL') then
            itvmep = icint(word(2))
         else
            write(fu6,1100)
            stop 'RPRMEP 2'
         endif
         goto 10
      endif
c
1100  format(/1X,'Unrecognized subkeyword in the PRPATH section')
c
      return
c
      end subroutine rprmep
c
c ***************************************************************************
c     rprvib
c ***************************************************************************
c
      subroutine rprvib(string,istrt)
      use common_inc
      use keyword_interface, only : gufac5
      use perconparam, only : fu5,fu6
c
c     Subroutine to read in the range of s between which extra vibrational
c     mode and Qvib is printed.
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  PRVIB section must end with an END')
            stop
         end if
c
c        parse line to get variable name
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a value is also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  PRVIB -- must specify value with sx or sy') 
c
         iend = istrt - 1
         vname = string(ibegin:iend)
c
c        check that vname is a legal variable name and what it is 
c
         if (vname(1:2).eq.'sx') then 
             sob = cfloat(string(istrt:80))*gufac5                      0405JZ07
         else  if (vname(1:2).eq.'sy') then
             soe = cfloat(string(istrt:80))*gufac5                      0405JZ07
         else
             write(fu6,1200) vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in PRVIB: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rprvib
C
C**********************************************************************
C RQRST
C**********************************************************************
C
      SUBROUTINE rqrst
      use common_inc
      use keyword_interface, only : gufac5
      use perconparam, only : fu5,fu6
      use kintcm
      use rate_const, only : srw,iwr,iqrst
C
C     This subroutine reads the options to perform the quantized-state tunneling
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER*80 WORD(40)
      LOGICAL LSEC,LEOF
C
      IQRST = 1
      JSRW = 0
      JMODE = 0
C
10    CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)
      IF (WORD(1) .NE. 'END') THEN
         IF (LSEC .OR. LEOF) THEN
            WRITE(FU6,1000)
            STOP 'RQRST 1'
         ENDIF
C
1000  FORMAT(/1X,'The RQRST section must end with an END')
C
         IF (WORD(1) .EQ. 'HARMONIC') THEN
            IQRST = 1
         ELSEIF(WORD(1) .EQ. 'HARMWKB') THEN
            IQRST = 2
         ELSEIF(WORD(1) .EQ. 'SYMWKB') THEN
            IQRST = 3
         ELSEIF(WORD(1) .EQ. 'MODE') THEN
            IWR = ICINT(WORD(2))
            JMODE = 1
         ELSEIF(WORD(1) .EQ. 'SRW') THEN
            IF (NWORD .LT. 2) THEN
               WRITE(FU6,1050)
               STOP 'RQRST 2'
            ENDIF
            SRW = CFLOAT(WORD(2))*GUFAC5                                0405JZ07
            SRW = - ABS(SRW)
            JSRW = 1
         ELSEIF(WORD(1) .EQ. 'STATES') THEN                             0423TA02
            IF (NWORD .LT. 2) THEN                                      0423TA02
               WRITE(FU6,1400)                                          0423TA02
               STOP 'RQRST 6'                                           0423TA02
            ENDIF                                                       0423TA02
            IF (WORD(2) .EQ. 'ALL') THEN                                0423TA02
               IQRNST = -1                                              0423TA02
            ELSE                                                        0423TA02
               IQRNST = ICINT(WORD(2))                                  0423TA02
               IF (IQRNST .LT. 0) THEN                                  0423TA02
                  WRITE(FU6,1450)                                       0423TA02
                  STOP 'RQRST 7'                                        0423TA02
               ENDIF                                                    0423TA02
            ENDIF                                                       0423TA02
         ELSEIF(WORD(1) .EQ. 'STQVIB') THEN                             0522TA02
            IF (NWORD .LT. 2) THEN                                      0522TA02
               WRITE(FU6,1500)                                          0522TA02
               STOP 'RQRST 8'                                           0522TA02
            ENDIF                                                       0522TA02
            IF (WORD(2) .EQ. 'ALL') THEN                                0522TA02
               IQRNSQ = -1                                              0522TA02
            ELSEIF (WORD(2) .EQ. 'SAMET') THEN                          0522TA02
               IQRNSQ = -2                                              0522TA02
            ELSE                                                        0522TA02
               WRITE(FU6,1550)                                          0522TA02
               STOP 'RQRST 9'                                           0522TA02
            ENDIF                                                       0522TA02
         ELSE
            WRITE(FU6,1100)
            STOP 'RQRST 3'
         ENDIF
         GOTO 10
      ENDIF
C
      IF (IQRST .GE. 2 .AND. JSRW .EQ. 0) THEN
         WRITE(FU6,1200)
         STOP 'RQRST 4'
      ENDIF
C
      IF (JMODE .EQ. 0) THEN
         WRITE(FU6,1300)
         STOP 'RQRST 5'
      ENDIF
C
1050  FORMAT(/1X,'Subkeyword SRW must have an argument.')
1100  FORMAT(/1X,'Unrecognized subkeyword in the QRST section')
1200  FORMAT(/1X,'SRW must be set if HARMWKB or SYMWKB is used.')
1300  FORMAT(/1X,'MODE must be set in the QRST section.')
1400  FORMAT(/1X,'Subkeyword STATES must have an argument.')            0423TA02
1450  FORMAT(/1X,'Subkeyword STATES must have a positive argument.')    0423TA02
1500  FORMAT(/1X,'Subkeyword STQVIB must have an argument.')            0522TA02
1550  FORMAT(/1X,'Subkeyword STQVIB has a wrong argument.')             0522TA02
C
      RETURN
C
      END subroutine rqrst
c
c ***************************************************************************
c     rquad
c ***************************************************************************
c
      subroutine rquad(string,istrt)
      use common_inc
      use perconparam, only : fu5,fu6
      use keyword_interface, only : gufac5
c
c     Subroutine to read in the quadrature for tunneling corrections
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
         narg = narg+1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  sct section must end with an END')
            stop
         end if
c
c        parse line to get variable name
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a value is also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  must specify order with NQE or NQTH') 
c
         iend = istrt - 1
         vname = string(ibegin:iend)
c
c        check that vname is a legal variable name and what it is 
c
         if (vname(1:3).eq.'nqe') then 
             nq12 = icint(string(istrt:80))
             nqq1 = nq12
         else  if (vname(1:4).eq.'nqth') then
             nq22 = icint(string(istrt:80))
             nqq2 = nq22
         else if (vname(1:9).eq.'nsegboltz') then
               nseg = icint(string(istrt:80))
         else if (vname(1:9).eq.'nsegtheta') then
               nseg2 = icint(string(istrt:80))
         else
             write(fu6,1200) vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in QUAD: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rquad
c ***************************************************************************
c     rrange
c ***************************************************************************
c
      subroutine rrange(string,istrt)
      use common_inc, only : slm,slp
      use perconparam, only : fu5,fu6
      use keyword_interface, only : gufac5
c
c     Subroutine to read in the range of s values over which the MEP
c     is to be calculated.
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end'.and.narg.le.2)
c
         narg = narg + 1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  svalue section must end with an END')
            stop
         end if
c
c        parse line to get variable name
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a value is also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  must specify distance with SLP or SLM')
c
         iend = istrt - 1
         vname = string(ibegin:iend)
c
c        check that vname is a legal variable name and what it is 
c
         if (vname(1:3).eq.'slp') then 
             slp = cfloat(string(istrt:80))*gufac5                      0405JZ07
         else  if (vname(1:3).eq.'slm') then
             slm = cfloat(string(istrt:80))*gufac5                      0405JZ07
         else
             write(fu6,1200)vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in SVALUE: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      narg = narg - 1
      if (narg.gt.2) then
         write(fu6,1300) narg
         stop
      end if
1300  format(3x,'ERROR:  SRANGE can have at most 2 arguments',
     *  ', it had ',i3)
c
      return
      end subroutine rrange
c ***************************************************************************
c     rrate
c ***************************************************************************
c
      subroutine rrate(string,iend,istrt)
c
c     Subroutine to read in the input for rate calculation
c
      use perconparam; use common_inc; use kintcm; use cm
      use keyword_interface, only : prpart,state
      use rate_const, only : switc
      implicit double precision (a-h,o-z)
      character * 80 string
c
c
c     set flag that rate is present
c
      irate = 1
c
c     read in first keyword in this section
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
c FORWARDK 
         if (string(istrt:istrt+7).eq.'forwardk') then
            ifrate = 1
            ibrate = 0
c NOFORWARDK
         else  if (string(istrt:istrt+9).eq.'noforwardk') then   
            ifrate = 0
c BOTHK
         else if (string(istrt:istrt+4).eq.'bothk') then
            ibrate = 1
            ifrate = 1
c GTLOG
         else if (string(istrt:istrt+4).eq.'gtlog') then                0423TA02
            igtlog = 1                                                  0423TA02
c NOGTLOG
         else if (string(istrt:istrt+6).eq.'nogtlog') then              0423TA02
            igtlog = 0                                                  0423TA02
c REVKEXP
         else if (string(istrt:istrt+6).eq.'revkexp') then              0528WH94
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               nrev = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable REVKEXP',
     *                    'must have an integer argument'
               stop
            end if
c SIGMAF
         else if (string(istrt:istrt+5).eq.'sigmaf') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
c              sigmaf = icint(string(istrt:80))
               sigmaf = cfloat(string(istrt:80))                        1205JZ07
            else
               write(fu6,*)' ERROR:  variable SIGMAF must ',
     *                    'have an integer argument'
               stop
            end if
c SIGMAR
         else if (string(istrt:istrt+5).eq.'sigmar') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
c              sigmar = icint(string(istrt:80))
               sigmar = cfloat(string(istrt:80))                        1205JZ07
            else
               write(fu6,*)' ERROR:  variable SIGMAR must ',
     *                    'have an integer argument'
               stop
            end if
c STATEOPT
         else if (string(istrt:istrt+7).eq.'stateopt') then
            call rstate(string,istrt)
c STATE
         else if (string(istrt:istrt+4).eq.'state') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               state = string(istrt:istrt+4)
            else
               write(fu6,*)' ERROR:  variable STATE must ',
     *                    'have a character argument'
               stop
            end if
            if (state.eq."diab") then
              call rword(string,istrt,ierr)
              if(ierr.ne.1) then
                 switc = cfloat(string(istrt:80))
              else
                 write(fu6,*)' ERROR:  variable DIAB must ',
     *                    'have an integer argument'
                 stop
              end if
            end if
c TEMP
         else if (string(istrt:istrt+3).eq.'temp') then
            call rtemp(string,istrt) 
c EDGEOK
         else if (string(istrt:istrt+5).eq.'edgeok') then
            ngflag = 1
c NOEDGEOK
         else if (string(istrt:istrt+7).eq.'noedgeok') then
            ngflag = 0
c GSPEC
         else if (string(istrt:istrt+4).eq.'gspec') then
            igspec = 1
            call rgspec(string,istrt)
c GTEMP
         else if (string(istrt:istrt+4).eq.'gtemp') then
            call rgtemp(string,istrt) 
c VPFEXP
         else if (string(istrt:istrt+5).eq.'vpfexp') then               0528WH94
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               iscale = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable VPFEXP ',
     *                    'must have an integer argument'
               stop
            end if
c SPTOPT
         else if (string(istrt:istrt+5).eq.'sptopt') then               0528WH94
            ipvib = 1
            call rvpert(string,istrt)
c SPT
         else if (string(istrt:istrt+2).eq.'spt') then                  0528WH94
            ipvib = 1
c PTTWO
         else if (string(istrt:istrt+4).eq.'pttwo') then
            idcpt = 20
            write (6,*) 'checks pttwo '
c DPTTWO
         else if (string(istrt:istrt+5).eq.'dpttwo') then
            idcpt = 30
            write (6,*) 'checks dpttwo '
c NDCPT
         else if (string(istrt:istrt+4).eq.'ndcpt') then
            idcpt = 10
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               isel = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable NDCPT ',
     *                    'must have an integer argument'
               stop
            end if
            write (6,*) 'checks ndcpt ',idcpt,' with set ',isel
c DCPT
         else if (string(istrt:istrt+3).eq.'dcpt') then
            idcpt = 1
            write (6,*) 'checks dcpt ',idcpt
c NOSPT
         else if (string(istrt:istrt+4).eq.'nospt') then                0528WH94
            ipvib = 0
c EACT
         else if (string(istrt:istrt+3).eq.'eact') then
            call rect(string,istrt)
c NOEACT
         else if (string(istrt:istrt+5).eq.'noeact') then
            ieact = 0
c ANALYSIS
         else if (string(istrt:istrt+7).eq.'analysis') then
            call ranaly(string,istrt)
c NOANALYSIS
         else if (string(istrt:istrt+9).eq.'noanalysis') then
            ianaly = 0
c TST
         else if (string(istrt:istrt+2).eq.'tst') then
            itst = 1
c NOTST
         else if (string(istrt:istrt+4).eq.'notst') then
            itst = 0
c CVT
         else if (string(istrt:istrt+2).eq.'cvt') then
            nfcvt = 1
c NOCVT
         else if (string(istrt:istrt+4).eq.'nocvt') then
            nfcvt = 0
c ICVT
         else if (string(istrt:istrt+3).eq.'icvt') then
c           icvt = 1
            write(fu6,*) 'ICVT option is no longer supported'
            stop           
c NOICVT
         else if (string(istrt:istrt+5).eq.'noicvt') then
            icvt = 0
c muVTOPT
         else if (string(istrt:istrt+6).eq.'muvtopt') then
            muvt = 1
            call rmuvt(string,istrt)
c muVT
         else if (string(istrt:istrt+3).eq.'muvt') then
            muvt = 1
c NOmuVT
         else if (string(istrt:istrt+5).eq.'nomuvt') then
            muvt = 0
c ejmuVT
         else if (string(istrt:istrt+6).eq.'ejmuvt') then
            iejmuvt = 1
         else if (string(istrt:istrt+8).eq.'noejmuvt') then
            iejmuvt = 0
c US
         else if (string(istrt:istrt+1).eq.'us') then
            nfus = 1
            muvt = 1
c NOUS
         else if (string(istrt:istrt+3).eq.'nous') then
            nfus = 0
c CUS                                                                   0929YC97
         else if (string(istrt:istrt+2).eq.'cus') then                  0929YC97
            nfcus = 1                                                   0929YC97
            nfcvt = 1                                                   0929YC97
            call rword(string,istrt,ierr)                               0929YC97
            if(ierr.ne.1) then                                          0929YC97
               ncusmx = icint(string(istrt:80))                         0929YC97
            else                                                        0929YC97
               write(fu6,*)' ERROR:  variable CUS must',                0929YC97
     *           ' have an argument'                                    0929YC97
               stop                                                     0929YC97
            end if                                                      0929YC97
c NOCUS                                                                 0929YC97
         else if (string(istrt:istrt+4).eq.'nocus') then                0929YC97
            nfcus = 0                                                   0929YC97
c PRDELG
         else if (string(istrt:istrt+5).eq.'prdelg') then
            iprg = 1
c NOPRDELG
         else if (string(istrt:istrt+7).eq.'noprdelg') then
            iprg = 0
c PRGIGT
         else if (string(istrt:istrt+5).eq.'prgigt') then
c           iprigt = 1
            write(6,*) 'PRGIGT is an obsolete keyword'
            write(6,*) 'ICVT option is no longer supported'
            stop
c NOPRGIGT
         else if (string(istrt:istrt+7).eq.'noprgigt') then
c           iprigt = 0
            write(6,*) 'NOPRGIGT is an obsolete keyword'
            write(6,*) 'ICVT option is no longer supported'
            stop
c PRPATH
         else if (string(istrt:istrt+5).eq.'prpath') then               0705WH94
            call rprmep                                                 0705WH94
c PRPART
         else if (string(istrt:istrt+5).eq.'prpart') then
            iprt = 1
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               prpart = string(istrt:istrt+2)
            else
               write(fu6,*)' ERROR:  variable PRPART must ',
     *                    'have a character argument'
               stop
            end if
c NOPRPART
         else if (string(istrt:istrt+7).eq.'noprpart') then
            iprt = 0
c PRVIB
         else if (string(istrt:istrt+4).eq.'prvib') then
            iprvib = 1
            call rprvib(string,istrt)
c NOPRVIB
         else if (string(istrt:istrt+6).eq.'noprvib') then
            iprvib = 0
         else
            write(fu6,1000) string(istrt:80)
            stop
         end if
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      if (prpart .eq. 'rps' .or. prpart .eq. 'rpt') iprg = 1            0620WH94
c
1000  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' rate section',/A80)
c
      return
      end subroutine rrate
c
c***********************************************************************
c     rrgu5   
c***********************************************************************
c
      subroutine rrgu5(string,istrt)
c
c     Subroutine to read in the unit for the geometries in fu5          1104JC97
c     It is now to be used to read in the unit for all the distance 
c     input                                                             0405JZ07
c

      use keyword_interface, only : gufac5,iunit5
      use perconparam, only : fu6
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     parse argument off the inpunit keyword line
c
      call rword(string,istrt,ierr)
      if(ierr.ne.1) then
          if(string(istrt:istrt+2).eq.'ang') then
c            igeou5 = 0                                                 0402JZ07
             iunit5 = 0                                                 0405JZ07
             gufac5 = 1.88972652D0                                      0405JZ07
          elseif (string(istrt:istrt+1).eq.'au') then
c            igeou5 = 1                                                 0402JZ07
             iunit5 = 1                                                 0405JZ07
             gufac5 = 1.0D0                                             0405JZ07
          else
             write (fu6,*) 'ERROR: this option is not support'
             stop
          endif
      else
          write(fu6,*)' ERROR:  variable INPUNIT must ',
     *               'have a character argument'
          stop
      endif
      return
      end subroutine rrgu5
c***********************************************************************
c     rrgu6   
c***********************************************************************
c
      subroutine rrgu6(string,istrt)
      use keyword_interface, only : gufac6,iunit6
      use perconparam, only : fu6
c
c     subroutine to read in the unit for all the distance output        0405JZ07
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     parse argument off the outunit keyword line
c
      call rword(string,istrt,ierr)
      if(ierr.ne.1) then
          if(string(istrt:istrt+2).eq.'ang') then
             iunit6 = 0                                                 0405JZ07
             gufac6 = 1.88972652D0
          elseif (string(istrt:istrt+1).eq.'au') then
             iunit6 = 1                                                 0405JZ07
             gufac6 = 1.0D0
          else
             write (fu6,*) 'ERROR: this option is not support'
             stop
          endif
      else
          write(fu6,*)' ERROR:  variable OUTUNIT must ',
     *               'have a character argument'
          stop
      endif
      return
      end subroutine rrgu6
c
c ***************************************************************************
c     rrpm
c ***************************************************************************
c
      subroutine rrpm(string,istrt)
c
c     subroutine to read the rpm options.  the rpm keyword is in the path
c     section.  since it is a list keyword, the data must conclude with an
c     'end.'
c
      use perconparam, only : fu6
      use kintcm
      implicit double precision (a-h,o-z)
      character * 80 string
c
      call rword(string,istrt,ierr)
c
      if (ierr.eq.1) then
         write(fu6,*) 'ERROR: RPM must have an argument'
         stop 'RRPM 1'
c
c ESD
      else if (string(istrt:istrt+2).eq.'esd') then
         ieuler = 1
         ies1 = 0
         ipagem = 0
         ivrp = 0
c ES1
      else if (string(istrt:istrt+2).eq.'es1') then
         ieuler = 0
         ies1 = 1
         ipagem = 0
         ivrp = 0
c PAGEM
      else if (string(istrt:istrt+4).eq.'pagem') then
         ieuler = 0
         ies1 = 0
         ipagem = 1
         ivrp = 0
c VRPE
      else if (string(istrt:istrt+3).eq.'vrpe') then
         ieuler = 1
         ies1 = 0
         ipagem = 0
         ivrp = 1
      else
         write(fu6,*) 'ERROR: not a valid argument for RPM'
         stop 'RRPM 2'
      endif
c
      return
      end subroutine rrpm
c
c ***************************************************************************
c     rrstrt
c ***************************************************************************
c
      subroutine rrstrt(string,istrt)
      use perconparam, only : fu5,fu6
      use kintcm
      use keyword_interface, only : gufac5
c
c     subroutine to read in the restart options.  This keyword is under
c     the general section.  Since it is a list variable the data must
c     concluded with an 'end'. 
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     read restart options until an end is found
c
      call rline(fu5,string,istrt,isect,iend)
      do while (string(istrt:istrt+2).ne.'end')
         if (string(istrt:istrt+7).eq.'writefu3') then
             iunxt = 3
         else if (string(istrt:istrt+8).eq.'readfu1&2') then
             if (iunxt.ne.3) iunxt = 2
         else if (string(istrt:istrt+6).eq.'readfu1') then
             iunxt = 1
         else if (string(istrt:istrt+7).eq.'writefu1') then
             iunxt = -1
         else if (string(istrt:istrt+4).eq.'merge') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: merge variable must have an argument'
               stop
             else
               sincr = cfloat(string(istrt:80))*gufac5                  0405JZ07
             end if
c                              check if list was none of the above - error
         else
             write(fu6,1000)string(istrt:80)
1000         format(' ERROR: illegal argument to restart -',/1x,a80) 
             stop
         end if
         call rline(fu5,string,istrt,isect,iend)
c
c                               check if end of file was found - an error
         if (iend.eq.1) then
            write(fu6,*)'ERROR: restart list must end with an END'
            stop
         end if
      end do
c
      return
      end subroutine rrstrt
c
c ***************************************************************************
c     rsct
c ***************************************************************************
c
      subroutine rsct(string,istrt)
      use perconparam, only : fu5,fu6
      use kintcm
      use keyword_interface, only : gufac5
      use rate_const, only : sincr
c
c     Subroutine to read in the options for small-curvature tunneling
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
         narg = narg+1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  sctopt section must end with an END')
            stop
         end if
c
c
         if (string(istrt:istrt+5).eq.'spline') then 
             isplne = 1
         else  if (string(istrt:istrt+8).eq.'lagrange') then
            isplne = 0
c
c           parse line to get variable name
c
            call rword(string,istrt,ierr)
c
c           check that a value is also on the line
            if (ierr.eq.1) then
                write(fu6,1200) 
                stop
            end if
1200        format(3x,'ERROR:  must specify order with LAGRANGE') 
c
            nlang = icint(string(istrt:80))
         else
            write(fu6,1300) string(istrt:iend) 
            stop
         end if
1300     format(3x,'ERROR:  invalid variable in SCT: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rsct
c
c***********************************************************************
c     rsecnd
c***********************************************************************
c
      subroutine rsecnd(string,iend,istrt)
      use perconparam, only : fu5,fu6
      use kintcm , only : ifprnt,ispot
      use keyword_interface, only : numtyp,gufac5,potsec
      use rate_const, only : sincr
      use cm, only : xnmstp
c
c     subroutine to read in the input in the second section.  
c
c     called by:
c           read5
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     read in first keyword in this section
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
c HESSCAL
         if (string(istrt:istrt+6).eq.'hesscal') then                   0301YC97
            call rhessc(string,iend,istrt)
c POLYRATE
c         if (string(istrt:istrt+7).eq.'polyrate') then
c          potsec='analytic'
c          ispot=0
c MOPAC
c         else if (string(istrt:istrt+4).eq.'mopac') then
c          potsec='mopac'
c          ispot=1
c ACESOPT                                                               0425RS95
         else if (string(istrt:istrt+6).eq.'acesopt') then
            potsec='aces'
            ispot=2
            call races(string,istrt,9)
c ACES
         else if (string(istrt:istrt+3).eq.'aces') then
            potsec='aces'
            ispot=2
c GAMESS
         else if (string(istrt:istrt+3).eq.'gamess') then
            potsec='gamess'
            ispot=3
c FPRINT
         else if (string(istrt:istrt+5).eq.'fprint') then
            ifprnt = 1 
c NOFPRINT
         else if (string(istrt:istrt+7).eq.'nofprint') then
            ifprnt = 0 
c NUMSTEP
         else if (string(istrt:istrt+6).eq.'numstep') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               xnmstp = cfloat(string(istrt:80))
               xnmstp = xnmstp*gufac5                                   0405JZ07
            else
               write(fu6,*)' ERROR:  variable numstep must',
     *           ' have an argument'
               stop
            end if
c NUMTYPE
         else if (string(istrt:istrt+6).eq.'numtype') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               numtyp = string(istrt:istrt+8)
            else
               write(fu6,*)' ERROR:  variable numtyp must',
     *           ' have an argument'
               stop
            end if
c
         else
            write(fu6,1200) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
1200  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' SECOND section',/A80)
c
      return
      end subroutine rsecnd
c
c ***************************************************************************
c     rsfrst
c ***************************************************************************
c
      subroutine rsfrst(string,istrt)
      use common_inc, only : del1,nst0
      use perconparam, only : fu5,fu6
      use keyword_interface, only : gufac5
c
c     Subroutine to read in the variables for special step sizes for the
c     first n steps. 
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end'.and.narg.le.2)
c
         narg = narg+1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  svalue sfirst must end with an END')
            stop
         end if
c
c        parse line to get variable name
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a value is also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  must specify distance with nfstep or fsize')
c
         iend = istrt - 1
         vname = string(ibegin:iend)
c
c        check that vname is a legal variable name and what it is 
c
         if (vname(1:6).eq.'nfstep') then 
             nst0 = icint(string(istrt:80))
         else  if (vname(1:5).eq.'fsize') then
             del1 = cfloat(string(istrt:80))*gufac5                     0405JZ07
         else
             write(fu6,1200)vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in SFIRST: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      narg = narg - 1
      if (narg.gt.2) then
         write(fu6,1300) narg
         stop
      end if
1300  format(3x,'ERROR:  SFIRST can have at most 2 arguments,',
     *  ' it had ',i3)
c
      return
      end subroutine rsfrst
c
c ***************************************************************************
c     rspecl
c ***************************************************************************
c
      subroutine rspecl(string,istrt)
      use common_inc, only : sspec,nspec
      use perconparam, only : fu5,fu6,nmspec
c
c     Subroutine to read in a list of s values at which to do the normal
c     mode analysis.
c
      implicit double precision (a-h,o-z)
      character * 80 string
      if(.not.allocated(sspec)) then
         allocate(sspec(nmspec));sspec=0d0
      endif
c
      nspec = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end') 
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  SSPECIAL section must end with an END')
            stop
         end if
c
c        save s value 
c
         sspec(nspec) = cfloat(string(istrt:80)) 
c
         call rline(fu5,string,istrt,isect,iend)
         nspec = nspec+1
      end do
c
      nspec = nspec-1
c
      return
      end subroutine rspecl
c**********************************************************************
c     RSSTOP
c**********************************************************************
c
      subroutine rsstop
      use perconparam, only : fu5,fu6
      use rate_const, only  : fracdw
      use kintcm, only : isstop
c
c     This subroutine reads the options for special stop of the path.   0423TA02
c
      implicit double precision(a-h,o-z)
c
      character*80 word(40)
      logical lsec,leof
c
      icurve = -1
      ipnt = 2
      fracdw = 0.5d0
10    call readln(fu5,word,nword,lsec,leof)
      if (word(1) .ne. 'END') then
         if (lsec .or. leof) then
            write(fu6,1000)
            stop 'rsstop 1'
         endif
c
         if (word(1) .eq. 'CURVE') then
            if (word(2) .eq. 'VMEP') then
               icurve = 1
            elseif (word(2) .eq. 'VAG') then
               icurve = -1
            else
               write(fu6,1200)
               stop 'rsstop 3'
            endif
         elseif(word(1) .eq. 'POINT') then
            if (word(2) .eq. 'FINEGRID') then
               ipnt = 1
            elseif (word(2) .eq. 'SAVEGRID') then
               ipnt = 2
            else
               write(fu6,1300)
               stop 'rsstop 4'
            endif
         elseif(word(1) .eq. 'PERCENTDOWN') then
            fracdw = cfloat(word(2))/100.d0
         else
            write(fu6,1100)
            stop 'rsstop 2'
         endif
c
         goto 10
      endif
      isstop = icurve * ipnt
c
1000  format(/1X,'The SPECSTOP keyword must end with an END.')
1100  format(/1X,'Unrecognized subkeyword in the SPECSTOP keyword.')
1200  format(/1X,'Unrecognized argument for CURVE subkeyword.')
1300  format(/1X,'Unrecognized argument for POINT subkeyword.')
c
      return
c
      end subroutine rsstop
c
c ***************************************************************************
c     rstat
c ***************************************************************************
c
      subroutine rstat(string,iend,istrt,jtype)
c
c     Generic subroutine to read in the input for all the stationary points.
c     Jtype = 1  first reactant 
c     Jtype = 2  second reactant 
c     Jtype = 3  first product 
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactant well
c     Jtype = 8  product well
c
      use perconparam, only : fu5,fu6,natom
      use kintcm; use cm
      use keyword_interface
      implicit double precision (a-h,o-z)
      character * 80 string,upcse
      character * 6  dir 
      CHARACTER*80  WORD(40)
      logical  lsec,leof
      call rstat_mem
c
c     read in first keyword in this section
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
c GEOMZMAT
         if (string(istrt:istrt+7).eq.'geomzmat') then
            call rgzmat(string,istrt,jtype)
c ZMATVAR
         else if (string(istrt:istrt+6).eq.'zmatvar') then
            call rzvar(string,istrt,jtype)
c INITGEO
         else if (string(istrt:istrt+6).eq.'initgeo') then              0514PF97
            call rword(string,istrt,ierr)                               0514PF97
            if(ierr.ne.1) then                                          0514PF97
               if (string(istrt:istrt+3).eq. 'geom') then               0514PF97
                 initg(jtype) = 0                                       0514PF97
               else if (string(istrt:istrt+4).eq.'hooks') then          0514PF97
                 initg(jtype) = 1                                       0514PF97
               endif                                                    0514PF97
            else                                                        0514PF97
               write(fu6,*)'ERROR:  variable INITGEO must have an',     0514PF97
     *                        ' argument'                               0514PF97
               stop                                                     0514PF97
            endif                                                       0514PF97
C
c for  REACT1,REACT2, PROD1, PROD2, START
c STATUS  =  0  calculate geom, energy, frequencies
c            2  read geom, calc energy, hessian, freq, eigenvectors
c            6  read geom, energy, freq                              (FOR RP) 
c STATUS
         else if (string(istrt:istrt+5).eq.'status') then               0701YC97
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then                                          0701YC97
                 istatu(jtype) = icint(string(istrt:80))                0701YC97
                 if (istatu(jtype).eq.4.and.jtype.lt.5) then            0725YC97
                   write (fu6,*) 'ERROR: STATUS = 4 is not available',  0725YC97
     *              ' for reactants and products'                       0725YC97
                   stop 'rstat 2'                                       0725YC97
                 endif                                                  0725YC97
            else                                                        0701YC97
               write(fu6,*)'ERROR:  variable STATUS must have an',      0701YC97
     *                        ' argument'                               0701YC97
               stop                                                     0701YC97
            endif                                                       0701YC97
c ENERGY
         else if (string(istrt:istrt+5).eq.'energy') then
             if (istatu(jtype).ne.0) then
               call rword(string,istrt,ierr)
               if (ierr.eq.1) then
                 write(fu6,*)'ERROR: ENERGY variable must have an ',
     *                'argument'
                 stop
               else
                 steng(jtype) = cfloat(string(istrt:80))
               end if
             else
               write (fu6,*)'WARNING: STATUS of ',jtype,
     *              ' is 0, energy will be re-evaluated'
             endif
c GEOM
         else if (string(istrt:istrt+3).eq.'geom') then
            if (jtype.le.8.and.jtype.ne.5)
     *          call rgeom(string,istrt,jtype)                          0730PF97
            if (jtype.eq.5)
     *             call rgsad(string,istrt,jtype)                       0730PF97
c PIVOT 
         else if (string(istrt:istrt+4).eq.'pivot') then                1026JZ07
            call rpivot(string,istrt,jtype)
c ACESOPT                                                               0510RS95
         else if (string(istrt:istrt+6).eq.'acesopt') then
            call races(string,istrt,jtype)
c NOOPT
c         else if (string(istrt:istrt+4).eq.'noopt') then
c            iopt(jtype) = 0 
c OPT
c         else if (string(istrt:istrt+2).eq.'opt') then
c            iopt(jtype) = 1 
c SPECIES
         else if (string(istrt:istrt+6).eq.'species') then              0603PF97
            call rword(string,istrt,ierr)
            if (ierr.ne.1) then 
               idmn(jtype) = string(istrt:istrt+7)
            else
               write(fu6,*) 'ERROR: variable SPECIES must have an',
     *                       ' argument!'
               stop
            end if
c LINAXIS                                                               0527PF97 
         else if (string(istrt:istrt+6).eq.'linaxis') then              0527PF97
            if (idmn(jtype).eq.'linrp'.or.idmn(jtype).eq.'lints'
     *        .or.idmn(jtype).eq.'linwell') then                        0728PF97
               call rword(string,istrt,ierr)                            0507YC97
               if(ierr.ne.1) then                                       0507YC97
                 dir = upcse(string(istrt:istrt+5))                     0507YC97
                 if (dir.eq.'X-AXIS') iolin(jtype) = 1                  0507YC97
                 if (dir.eq.'Y-AXIS') iolin(jtype) = 2                  0507YC97
                 if (dir.eq.'Z-AXIS') iolin(jtype) = 3                  0507YC97
               else                                                     0507YC97
c if no argument is specified with the linear keyword then the          0522PF97
c  default axis is taken as the z-axis.                                 0522PF97
                 iolin(jtype) = 3                                       0522PF97
               end if                                                   0507YC97
            elseif (idmn(jtype).eq.'nonlinrp'.or.
     *              idmn(jtype).eq.'nonlints'.or.
     *              idmn(jtype).eq.'nonlinwe'.or.                       0727PF97
     *              idmn(jtype).eq.'ssrp'.or.
     *              idmn(jtype).eq.'ssts') then                         0605PF97
               write(fu6,*) 'Error:  cannot use the ',idmn(jtype),      0527PF97
     *               ' option with the SPECIES keyword and the ',
     *                 'LINAXIS keyword.'                               0527PF97
               stop                                                     0527PF97
            endif                                                       0527PF97
c CONSTANT
         else if (string(istrt:istrt+7).eq.'constant') then
            call rcnst(string,istrt,jtype)
c FREQUNIT
         else if (string(istrt:istrt+7).eq.'frequnit') then             0807YC97
             call rword(string,istrt,ierr)                              0807YC97 
             if(ierr.ne.1) then                                         0807YC97 
               if (string(istrt:istrt+1).eq.'au') then                  0807YC97 
                      ifreu(jtype) = 0                                  0807YC97 
               else if (string(istrt:istrt+4).eq.'waven') then          0807YC97 
                      ifreu(jtype) = 1                                  0807YC97
               else                                                     0807YC97
                    write (fu6,*) 'ERROR in FREQUNIT, a.u. is used'     0807YC97
               endif                                                    0807YC97
             endif                                                      0807YC97
c VIB
         else if (string(istrt:istrt+2).eq.'vib') then                  0317YC99
c
c The default has been changed because when read in the frequencies 
c no normal mode analysis is required
c
            if (istatu(jtype).ne.6) then                                0725YC97
               write (fu6,*)'WARNING: STATUS of ',jtype,                0725YC97
     *              ' is not 6, frequencies will be re-evaluated'       0725YC97
            endif                                                       0725YC97
            jxfreq(jtype) = 0                                           0725YC97
            NFREQ = 0                                                   0725YC97
300         CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)                       0725YC97
            IF (WORD(1) .NE. 'END') THEN                                0725YC97
               IF (LSEC .OR. LEOF) THEN                                 0725YC97
                  STOP 'READ FREQ'                                      0725YC97
               ENDIF                                                    0725YC97
               DO 310 IM = 1,NWORD                                      0725YC97
                  TEMWER(JTYPE,IM+NFREQ) = CFLOAT(WORD(IM))             0725YC97
310            CONTINUE                                                 0725YC97
               NFREQ = NFREQ + NWORD                                    0725YC97
               GOTO 300                                                 0725YC97
            ENDIF                                                       0725YC97
c FREQ
         else if (string(istrt:istrt+3).eq.'freq') then                 0317YC99
            jxfreq(jtype) = 1                                           0317YC99
c NOFREQ                                                                0317YC99
         else if (string(istrt:istrt+5).eq.'nofreq') then               0317YC99
            jxfreq(jtype) = 0                                           0317YC99
c
c HESSIAN : hessian in packed form                                      0725YC97
c
         else if (string(istrt:istrt+6).eq.'hessian') then              0725YC97
            if (istatu(jtype).lt.4) then                                0725YC97
               write (fu6,*)'WARNING: STATUS of ',jtype,                0725YC97
     *              ' is less than 4, hessian will be re-evaluated'     0725YC97
            endif                                                       0725YC97
            j = 1                                                       0725YC97
            k = 0                                                       0725YC97
510         call readln(fu5,word,nword,lsec,leof)                       0725YC97
            if(word(1) .ne. 'END') then                                 0725YC97
              IF (LSEC .OR. LEOF) THEN                                  0725YC97
                STOP 'READ HESS'                                        0725YC97
              ENDIF                                                     0725YC97
              do 520 i = 1, nword                                       0725YC97
                 k = k + 1                                              0725YC97
                 if(k .gt. j) then                                      0725YC97
                    j = j + 1                                           0725YC97
                    k = 1                                               0725YC97
                 endif                                                  0725YC97
                 TEMHES(j,k) = cfloat(word(i))                          0725YC97
                 TEMHES(k,j) = TEMHES(j,k)                              0725YC97
520           continue                                                  0725YC97
            goto 510                                                    0725YC97
            endif                                                       0725YC97
c
c EIGENVECTOR : in full form                                            0725YC97
c
         else if (string(istrt:istrt+10).eq.'eigenvector') then         0725YC97
            if (istatu(jtype).lt.4) then                                0725YC97
               write (fu6,*)'WARNING: STATUS of ',jtype,                0725YC97
     *            ' is less than 4, eigenvectors will be re-evaluated'  0725YC97
            endif                                                       0725YC97
            j = 1                                                       0725YC97
            k = 0                                                       0725YC97
            N = 3*NATOM                                                 0725YC97
400         CALL READLN(FU5,WORD,NWORD,LSEC,LEOF)                       0725YC97
            IF (WORD(1) .NE. 'END') THEN                                0725YC97
               IF (LSEC .OR. LEOF) THEN                                 0725YC97
                  STOP 'READ EIGENVECTOR'                               0725YC97
               ENDIF                                                    0725YC97
               DO 410 I = 1,NWORD                                       0725YC97
                  K = K + 1                                             0725YC97
                  if(k .gt. N) then                                     0725YC97
                     j = j + 1                                          0725YC97
                     k = 1                                              0725YC97
                  endif                                                 0725YC97
                  TEMGE(J,K) = CFLOAT(WORD(I))                          0725YC97
410            CONTINUE                                                 0725YC97
               GOTO 400                                                 0725YC97
            ENDIF                                                       0725YC97
c
c PROJECT
         else if (string(istrt:istrt+6).eq.'project') then
            iproj(jtype) = 1 
c NOPROJECT
         else if (string(istrt:istrt+8).eq.'noproject') then
            iproj(jtype) = 0 
c DIATOM
         else if (string(istrt:istrt+5).eq.'diatom') then
            ndiat(jtype) = 1
            call rdiatm(string,istrt,jtype)
c ELEC
         else if (string(istrt:istrt+3).eq.'elec') then
            call relec(string,istrt,jtype)
c HARMONIC
         else if (string(istrt:istrt+7).eq.'harmonic') then
            iharm(jtype) = 1 
c MORSE
         else if (string(istrt:istrt+5).eq.'morse') then
            iharm(jtype) = 0 
            imor(jtype) = 1
c MORMODEL
         else if (string(istrt:istrt+7).eq.'mormodel') then
            iharm(jtype) = 0 
            imtyp(jtype) = 1
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               mortyp(jtype) = string(istrt:istrt+8)
            else
               write(fu6,*)'ERROR:  variable MORMODEL must have an',
     *                        ' argument'
               stop
            end if
c MORSEQQ
         else if (string(istrt:istrt+6).eq.'morseqq') then
            iharm(jtype) = 0 
            imorqq(jtype) = 1
c DEMIN
         else if (string(istrt:istrt+4).eq.'demin') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: demin variable must have an argument'
               stop
             else
               xdemin(jtype) = cfloat(string(istrt:80))
             end if
c ANTLR
         else if (string(istrt:istrt+4).eq.'antlr') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: antlr variable must have an argument'
               stop
             else
               xantlr(jtype) = cfloat(string(istrt:80))
             end if
c QQWKB
         else if (string(istrt:istrt+4).eq.'qqwkb') then
            iharm(jtype) = 0 
            iqqwkb(jtype) = 1
c QQSEMI
         else if (string(istrt:istrt+5).eq.'qqsemi') then
            iharm(jtype) = 0 
            iqqsem(jtype) = 1
c DQQP
         else if (string(istrt:istrt+3).eq.'dqqp') then
            call rword(string,istrt,ierr)
            if (ierr.eq.1) then
              write(fu6,*) 'ERROR: dqqp variable must have an argument'
              stop
            else
              ibegin = istrt
              call rword(string,istrt,ierr)
              istop = istrt-1
              xdqqp(jtype,1) = cfloat(string(ibegin:istop))*gufac5
              xdqqp(jtype,2) = cfloat(string(istrt:80))*gufac5
            end if
c TOROPT
         else if (string(istrt:istrt+5).eq.'toropt') then               0521YC99
            call rtoropt(jtype)                                         0521YC99
c TOR
         else if (string(istrt:istrt+2).eq.'tor') then                  0521YC99
            iharm(jtype) = 0                                            0521YC99
            call rtor(jtype)                                            0521YC99  
c PRMODE
         else if (string(istrt:istrt+5).eq.'prmode') then               0317YC99
            iprmd(jtype) = 1                                            0317YC99
c WKB
         else if (string(istrt:istrt+2).eq.'wkb') then
            iwkb(jtype) = 1
            call rwkb(string,istrt,jtype)
c VANHAR
         else if (string(istrt:istrt+5).eq.'vanhar') then
            iharm(jtype) = 0 
            ivary(jtype) = 1
            call rvary(string,istrt,jtype)
c VRANGE
         else if (string(istrt:istrt+5).eq.'vrange') then
            iharm(jtype) = 0 
            call rvrang(string,istrt,jtype)
         else
            write(fu6,1000) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c
1000  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' stationary points section',/A80)
c
      return
      end subroutine rstat
c
c ***************************************************************************
c     rstate
c ***************************************************************************
c
      subroutine rstate(string,istrt)
      use perconparam, only : fu5,fu6
      use rate_const, only : ln3,l9
c
c     Subroutine to read in the options used with a state-selected 
c     rate constant calculation.  These options are only used when
c     the STATE keyword is set to adiab or diab.
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 6 reatyp
      character * 7 modtyp
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
            stop
         end if
1000     format(3x,'ERROR:  stateopt section must end with an END')
c
c        parse line to species type
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         reatyp = string(ibegin:iend)
c
c        parse line to get mode number
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         nmod = icint(string(ibegin:iend))
c
c        get treatment of this mode of the remaining part of the line
c
         modtyp = string(istrt:istrt+6)
c
c        store this information
c
         write(fu6,*)' input: ',reatyp,nmod,modtyp
         if (modtyp.eq."ground") then
            k = -1
         else if (modtyp.eq."first") then
            k = 1
         else if (modtyp.eq."thermal") then
            k = 0
         else
            write(fu6,1100)modtyp
            stop
         end if
1100     format(3x,'Error:  invalid mode treatment in STATEOPT: ',a7)
c
         write(fu6,*)' input: ',reatyp,nmod,modtyp,k
         if (reatyp.eq."react1") then
           l9(1,nmod) = k
         else if (reatyp.eq."react2") then
           l9(2,nmod) = k
         else if (reatyp.eq."prod1") then
           l9(3,nmod) = k
         else if (reatyp.eq."prod2") then
           l9(4,nmod) = k
         else if (reatyp.eq."ts") then
           ln3(1,nmod) = k
         else if (reatyp.eq."switts") then
           ln3(2,nmod) = k
         else
           write(fu6,1200) reatyp
           stop
         end if
1200     format(3x,'Error:  invalid species in STATOPT: ',a6)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rstate
c
c********************************************************
c rsvrc
c********************************************************
c
      subroutine rsvrc(string,istrt)
      use common_inc
      use perconparam; use cm
      use kintcm
      use keyword_interface
c subroutine to read the range and step size of variable 
C reaction coordinate
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      nspair = 1
      ntotp = npvt(1)*npvt(2)
c
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  SVRC section must end with an END')
            stop
         end if
c
         if(npair.gt.ntotp) then
            write(fu6,1100) ntotp
1100        format(2x,'ERROR: Number of pairs exceeds the total pairs ',
     *      I4)
            stop
         endif
c
c read pivot point identification number
c
c        ibegin = istrt
c        call rword(string, istrt, ierr)
c        iend = istrt - 1
c        jp1 = icint(string(ibegin:iend))
c
c        ibegin = istrt
c        call rword(string, istrt, ierr)
c        iend = istrt - 1
c        jp2 = icint(string(ibegin:iend))
c
c read upper and lower limit of s values
c
         ibegin = istrt
         call rword(string, istrt, ierr)
         iend = istrt - 1
         svl =dble(cfloat(string(ibegin:iend))*gufac5)
c
         ibegin = istrt
         call rword(string, istrt, ierr)
         iend = istrt - 1
         svu = dble(cfloat(string(ibegin:iend))*gufac5)
c
         if(svu.lt.svl) then
          tmps = svu
          svu  = svl
          svl  = tmps
         endif
          
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
c
c        check that a value is also on the line
c        if (ierr.eq.1) then
c            write(fu6,1200)
c            stop
c        end if
c1200     format(3x,'ERROR:  must specify a value for the pair s ')
         svs = dble(cfloat(string(ibegin:iend))*gufac5)
c
c        nspair = nspair + 1
      call rline(fu5,string,istrt,isect,iend) 
      enddo
c        nspair = nspair - 1
c        if(nspair.ne.ntotp) then
c         write(fu6,1300) ntotp
c1300      format(3x,'ERROR: number of pairs is not equal to the total',
c    *          ' pairs', I4)    
c         stop
c        endif
c
      return
      end subroutine rsvrc
c
c ***************************************************************************
c     rtemp
c ***************************************************************************
c
      subroutine rtemp(string,istrt)
      use keyword_interface; use rate_const
      use perconparam, only : fu5,fu6
c
c     Subroutine to read in the temperatures.
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      ntemp = 0
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
            stop
         end if
1000     format(3x,'ERROR:  temperature section must end with an END')
c
         ntemp = ntemp + 1
         if (ntemp.gt.40) then
           write(fu6,1010)
1010       format(3x,'ERROR:  max number of temps allowed is 40')
           stop
         end if
c
         temp(ntemp) = cfloat(string(istrt:80))
         if (temp(ntemp).le.0) then
           write(fu6,1020)ntemp,temp(ntemp)
           stop 
         end if
1020     format(3x,'ERROR:  temperature number ',i2,' is invalid',f15.4)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rtemp
c
c ***************************************************************************
c     rtitle
c ***************************************************************************
c
      subroutine rtitle(string,istrt)
      use keyword_interface, only : ftitle
      use perconparam, only : fu5,fu6
c
c     subroutine to read in the title.  This is in the general section of the
c     input under the keyword title.  Since title is a list variable the 
c     input under title must end with an 'end'.  Due to limitations in the
c     dimension the title is restricted to 5 lines.
c
      implicit double precision (a-h,o-z)
c
      character * 80 string
      character*80  word(40)
      logical  lsec,leof
c
c     stub for testing:
c
      i = 1
100   call rdln(fu5,word,nword,lsec,leof,string)
      if (i .le. 5 .and. word(1) .ne. 'END') then
         ftitle(i) = string
         i = i + 1
         goto 100
      endif
c
      if (i .gt. 5 .and. word(1) .ne. 'END') then
         write(fu6,1000)
         stop 'RTITLE 1'
      endif
c
1000  format('The TITLE section can only have 5 or less lines, ',
     *       'and should end with an END.')
c  
c     call rline(fu5,string,istrt,isect,iend)
c     do while (i.le.5.and.string(istrt:istrt+2).ne.'end')
c        ftitle(i) = string
c        i = i+1
c        call rline(fu5,string,istrt,isect,iend)
c     end do
c
c     if the 'end' wasn't found then keep reading title lines
c     but do not store them until an end is found
c
c     if (i.gt.5) then
c         do while (string(istrt:istrt+2).ne.'end')
c            call rline(fu5,string,istrt,isect,iend)
c            if(iend.eq.1) then
c              write(fu6,1000)
c            end if
c         end do
c     end if
c
      return
      end subroutine rtitle
c
c
c***********************************************************************
c     rtor
c***********************************************************************
c
      subroutine rtor(jtype)
      use common_inc
      use perconparam
      use kintcm
c
c     Subroutine to read in the the tor anharmonicity options.
c     Reorganized by J. Zheng April 2010.
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactant well
c     Jtype = 8  product well
c
      implicit double precision (a-h,o-z)
      character*80 word(40)
      logical lsec,leof
c
c     read in each line of the geometry until an 'end' is found
c
10    call readln(fu5,word,nword,lsec,leof)
      if (word(1) .ne. 'END') then
         if (lsec .or. leof) then
            write(fu6,1000)
            stop 'rtor 1'
         endif
1000  format(/1X,'The tor section must end with an END')
c
         nmodes(jtype) = 0
c
c  parse line with MODE P M  SCHEME  N
c
         if (nword.eq.5) then
c
c        parse line to get mode number
c
           nx = icint(word(1))
           nmnumb(jtype) = nx
c
c        parse line to get number of distinct minima 
c
           ntrnb(jtype,nx) = icint(word(2))
c
C        parse line to get number of total minima
c
           ntrm(jtype,nx) = icint(word(3))
c
c        set the symmetry number if P = 1
c           
c          if (ntrnb(jtype,nx).eq.1) then
c           ntrsig(jytpe,nx,1)= ntrm(jtype,nx)
c          endif
          do k = 1, ntrm(jtype,nx)
             ntrsig(jtype,nx,k) = ntrm(jtype,nx)/ntrnb(jtype,nx)
          enddo
c
c        parse method code
c
           IF (WORD(4).EQ.'CT') THEN
             ntrmtd(jtype,nx) = 1
           ELSEIF (WORD(4).EQ.'RPG') THEN
             ntrmtd(jtype,nx) = 2 
           ELSEIF (WORD(4).EQ.'SRPG') THEN
             ntrmtd(jtype,nx) = 3  
           ELSEIF (WORD(4).EQ.'AS') THEN
             ntrmtd(jtype,nx) = 4 
           ELSEIF (WORD(4).EQ.'SAS') THEN
             ntrmtd(jtype,nx) = 5
           ELSEIF (WORD(4).EQ.'FR') THEN
             ntrmtd(jtype,nx) = 6
           ELSE
             WRITE (FU6,*) 'TOR/METHOD OPTION IS NOT ACCEPTED'
             STOP 'rtor 2'
           ENDIF
c
c        parse level code
c
c          IF (WORD(5).EQ.'FULL') THEN
c            ntrlev(jtype,nx) = 1
c          ELSEIF (WORD(5).EQ.'SF') THEN
c            ntrlev(jtype,nx) = 2 
c          ELSEIF (WORD(5).EQ.'SC') THEN
c            ntrlev(jtype,nx) = 3 
c          ELSE
c            WRITE (FU6,*) 'TOR/LEVEL OPTION IS NOT ACCEPTED'
c            STOP 'rtor 3'
c          ENDIF
c
c        parse line to get number of atoms in subgroup
c
           ntrnum(jtype,nx) = icint(word(5))
c
c        Default interpolation scheme                                   1027BE05
c          torintrp(jtype,nx) = 1                                       1027BE05
c        Default HRDS zero-point correction to off                      1028BE05
c          torzpc(jtype,nx) = 1                                         1028BE05
c
         else
            write(fu6,1100)
            stop 'rtor 2'
         endif
         nmodes(jtype) = nmodes(jtype)+1
c
c check whether the total symetry number (M/P) is integer
c
         if(mod(ntrm(jtype,nx),ntrnb(jtype,nx)).ne.0) then
            write(fu6,*) 'Torsional mode: ',nx
            write(fu6,*) 'Total symmetry number (M/P) is not integer '
            write(fu6,*) 'Check the input values of P and M' 
            stop 'rtor 3'
         endif
         goto 10
      endif
c
1100  format(/1X,'Unrecognized subkeyword in the tor section')
      return
      end subroutine rtor
c
c***********************************************************************
c     rtoropt
c***********************************************************************
c
      subroutine rtoropt(jtype)
      use common_inc
      use perconparam
      use kintcm; use keyword_interface; use cm
c
c     Subroutine to read in the the toropt anharmonicity options.
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactant well
c     Jtype = 8  product well
c
      implicit double precision (a-h,o-z)
      character*80 word(40)
      logical lsec,leof
c
c     read in each line of the geometry until an 'end' is found
c
10    call readln(fu5,word,nword,lsec,leof)
      if (word(1) .ne. 'END') then
         if (lsec .or. leof) then
            write(fu6,1000)
            stop 'rtoropt 1'
         endif
c
1000  format(/1X,'The toropt section must end with an END')
c
c     read in OMEGA option
c
         if (word(2).eq.'OMEGA') then
            if (nword.lt.NMAXP+1) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
                 torome(jtype,nx,k+1)=cfloat(word(K+2))/AUTOCM
               ENDDO
            else
               write (fu6,1001) 'OMEGA'
               stop 'rtoropt 1'
            endif
c
c     read in CTLEVEL option
c
         elseif (word(2).eq.'CTLEVEL') then
            nx = icint(word(1))
            if (word(3).eq.'FULL') then
               ntrlev(jtype,nx) = 1
            elseif (word(3).eq.'SF') then
               ntrlev(jtype,nx) = 2
            elseif (word(3).eq.'SFE') then
               ntrlev(jtype,nx) = 3
            else
               write (fu6,1001) 'CTLEVEL '
               stop 'rtoropt 2'
            endif
c 
c       read in CTSCHEME option
c
          elseif (word(2).eq.'CTSCHEME') then
            nx = icint(word(1))
            IF (WORD(3).EQ.'OW') THEN
              ntrsch(jtype,nx) = 1
            ELSEIF (WORD(3).EQ.'RO') THEN
              ntrsch(jtype,nx) = 2 
            ELSEIF (WORD(3).EQ.'CO') THEN
              ntrsch(jtype,nx) = 3  
            ELSEIF (WORD(3).EQ.'RW') THEN
              ntrsch(jtype,nx) = 4 
            ELSEIF (WORD(3).EQ.'CW') THEN
              ntrsch(jtype,nx) = 5
            ELSEIF (WORD(3).EQ.'RWO') THEN
              ntrsch(jtype,nx) = 6
            ELSEIF (WORD(3).EQ.'CWO') THEN
              ntrsch(jtype,nx) = 7 
            ELSE
              WRITE (FU6,*) 'TOR/SCHEME OPTION IS NOT ACCEPTED'
              STOP 'rtoropt 3'
            ENDIF
c
c     read in W option
c
         elseif (word(2).eq.'W') then
            if (nword.lt.NMAXP+2) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
                 torw(jtype,nx,k)=cfloat(word(K+2))/AUTOCM
               ENDDO
            else
               write (fu6,1001) ' W '
               stop
            endif
c  
c     read in WL option
c           
         elseif (word(2).eq.'WL') then
            if (nword.lt.NMAXP+2) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
                 torwl(jtype,nx,k)=cfloat(word(K+2))/AUTOCM
               ENDDO
            else
               write (fu6,1001) ' WL '
               stop
            endif
c
c     read in WR option
c     
         elseif (word(2).eq.'WR') then
            if (nword.lt.NMAXP+2) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
                 torwr(jtype,nx,k)=cfloat(word(K+2))/AUTOCM
               ENDDO
            else
               write (fu6,1001) ' WR '
               stop
            endif
c     
c     read in RATIOL option
c     
         elseif (word(2).eq.'RATIOL') then
            if (nword.lt.NMAXP+2) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
c                ratiol(jtype,nx,k)=cfloat(word(K+2))       
                 ratiol(jtype,nx,k)=cfloat(word(K+2))/360.0d0           0324JZ10
               ENDDO
            else
               write (fu6,1001) ' RATIOL '
               stop
            endif
c
c     read in RATIOR option
c  
         elseif (word(2).eq.'RATIOR') then
            if (nword.lt.NMAXP+2) then
               nx = icint(word(1))   
               DO K = 1, NWORD-2
c                ratior(jtype,nx,k)=cfloat(word(K+2))
                 ratior(jtype,nx,k)=cfloat(word(K+2))/360.0d0           0324JZ10
               ENDDO
            else 
               write (fu6,1001) ' RATIOR '
               stop
            endif
c     
c     READI option added 1021BE06
c
         elseif (word(2).eq.'READI') then 
            if (nword.lt.NMAXP+1) then
               nx = icint(word(1))
               tormi(jtype,nx,1)=cfloat(word(3))
               readi(jtype,nx)=tormi(jtype,nx,1)                        0326JZ10
            else
               write (fu6,1001) ' READI '
               stop
            endif
c
c     read in I option
c
         elseif (word(2).eq.'I') then
            if (nword.lt.NMAXP+1) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
                 tormi(jtype,nx,k+1)=cfloat(word(K+2))
               ENDDO
            else
               write (fu6,1001) ' I '
               stop
            endif
c
c     read in U option
c
         elseif (word(2).eq.'U') then
            if (nword.lt.NMAXP+1) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
                 toru(jtype,nx,k+1)=cfloat(word(K+2))/AUTOCM
               ENDDO
            else
               write (fu6,1001) ' U '
               stop
            endif
c
c     read in SIGMA option
c
c        elseif (word(2).eq.'SIGMA') then
c           if (nword.lt.NMAXP+1) then
c              nx = icint(word(1))
c              DO K = 1, NWORD-2
c                ntrsig(jtype,nx,K+1)=icint(word(K+2))
c              ENDDO
c           else
c              write (fu6,1001) 'SIGMA'
c              stop
c           endif
c
c     read in ISB option
c
         elseif (word(2).eq.'ISB') then
            if (nword.lt.NATOMS+2) then
               nx = icint(word(1))
               DO K = 1, NWORD-2
                 ntrisb(jtype,nx,K)=icint(word(K+2))
               ENDDO
            else
               write (fu6,1001) 'ISB'
               stop
            endif
c
c     read in INTRP option                                              1027BE05
c
c        elseif (word(2).eq.'INTRP') then
c            nx = icint(word(1))
c            if (word(3).eq.'MN') then
c              torintrp(jtype,nx)=1
c            elseif (word(3).eq.'AS') then
c              torintrp(jtype,nx)=2
c            elseif (word(3).eq.'RPG') then
c              torintrp(jtype,nx)=3
c            elseif (word(3).eq.'SRPG') then                            1206BE05
c              torintrp(jtype,nx)=4                                     1206BE05
c            elseif (word(3).eq.'SAS') then                             1206BE05
c              torintrp(jtype,nx)=5                                     1206BE05
c            elseif (word(3).eq.'FR') then                              0131BE07
c              torintrp(jtype,nx)=6                                     0131BE07
c            else
c              write (fu6,1100) 'INTRP'
c              stop
c            endif
c
c     read in ZPC option                                                1028BE05
c
c        elseif (word(2).eq.'ZPC') then
c            nx = icint(word(1))
c            if (word(3).eq.'OFF') then
c              torzpc(jtype,nx)=1
c            elseif (word(3).eq.'ON') then
c              torzpc(jtype,nx)=2
c            else   
c              write (fu6,1100) 'ZPC'
c              stop
c            endif 
c
c
c     read in NBOND option
c
         elseif (word(2).eq.'NBOND') then
            if (nword.eq.4) then
               nx = icint(word(1))
               ntrbnd(jtype,nx,1) =icint(word(3))
               ntrbnd(jtype,nx,2) =icint(word(4))
            else
               write (fu6,1001) 'NBOND'
               stop
            endif
         else
            write(fu6,1100)
            stop 'rtoropt 2'
         endif
         goto 10
      endif
c
1001  format(/1X,'Error in ',A10,' too many arguments')
1100  format(/1X,'Unrecognized subkeyword in the toropt section')
      return
      end subroutine rtoropt
c
c ***************************************************************************
c     rtunnl
c ***************************************************************************
c
      subroutine rtunnl(string,iend,istrt)
      use common_inc, only : pemin
      use perconparam, only : fu5,fu6,natom
      use kintcm; use keyword_interface; use cm
c
c     Subroutine to read in the input for tunneling calculations 
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c
c     set flag that tunnel is present
c
      ilctopt=0                                                         0708JC00
      itunnl = 1
c
c     read in first keyword in this section
      call rline(fu5,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
c WIGNER 
         if (string(istrt:istrt+5).eq.'wigner') then
            iwign = 1
c NOWIGNER
         else  if (string(istrt:istrt+7).eq.'nowigner') then   
            iwign = 0
            write(6,1000)
1000        format('Invalid option: Wigner tunneling can not be',
     *              ' turned off in this version of POLYRATE')
            stop
c ZCT
         else if (string(istrt:istrt+2).eq.'zct') then
            izct = 1 
c NOZCT
         else if (string(istrt:istrt+4).eq.'nozct') then
            izct = 0 
c SCTOPT
         else if (string(istrt:istrt+5).eq.'sctopt') then
            isct = 1
            call rsct(string,istrt)
c SCT
         else if (string(istrt:istrt+2).eq.'sct') then
            isct = 1
c NOSCT
         else if (string(istrt:istrt+4).eq.'nosct') then
            isct = 0
c LCTOPT
         else if (string(istrt:istrt+5).eq.'lctopt') then
            ilctopt = 1                                                 0708JC00
            call rlct(string,istrt)
c LCTDETAIL
         else if (string(istrt:istrt+8).eq.'lctdetail') then
            ipdat = 1
            call rdetil                                                 0502WH94
c LCTGRD
         else if (string(istrt:istrt+6).eq.'lctgrid') then
            ipgrid = 1
            call rlctgrd
c LCT                                                                   0808JC00
         else if (string(istrt:istrt+2).eq.'lct') then                  0808JC00
            if (ilctopt.ne.1) ilct = 2                                  1408JC00
c NOLCT
         else if (string(istrt:istrt+4).eq.'nolct') then
            ilct = 0
c EMIN
         else if (string(istrt:istrt+3).eq.'emin') then
            iemin = 1
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               pemin = cfloat(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable EMIN must ',
     *                    'have an argument'
               stop
            end if
c QUAD
         else if (string(istrt:istrt+3).eq.'quad') then
            call rquad(string,istrt) 
c QRST
         else if (string(istrt:istrt+3).eq.'qrst') then                 0707WH94
            call rqrst                                                  0707WH94
c EXCITED
         else if (string(istrt:istrt+6).EQ.'excited') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
               nexcit = icint(string(istrt:80))
            else
               write(fu6,*)' ERROR:  variable EXCITED must ',
     *                    'have an integer argument'
               stop
            end if
c ALLEXCIT
         else if (string(istrt:istrt+7).eq.'allexcit') then
            nexcit = -1 
c NOALLEXCIT
         else if (string(istrt:istrt+9).eq.'noallexcit') then
            nexcit = 0 
         else
            write(fu6,1100) string(istrt:80)
            stop
         end if
         call rline(fu5,string,istrt,isect,iend)
      end do
c By default the large curvature method is LCG4                         0708JC00
      if(ilctopt.eq.1.and.ilct.ne.1) ilct=2                             1408JC00
      if(ilctopt.eq.1.and.ilct.eq.1) ilct=1                             1408JC00
c
1100  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' tunnel section**',/A80)
c
      return
      end subroutine rtunnl
c
C***********************************************************************
C  rvar6
C***********************************************************************
C
      subroutine rvar6 (IOUT)
      use perconparam, only : fu5,fu6,natom
      use kintcm; use keyword_interface; use cm
      use common_inc
C     use rate_const     
C     use gf; use potmod
C     use path_mod; use intbsv; use energetics_mod

c
c     read necessary info from unit iout -- this is only for the new 
c     variables introduced by the new interface in version 6.0
c
c    called by:
c               restor
c
c
      implicit double precision (a-h,o-z)

      call rstat_mem
c
c     read logical variable for tunneling
      read(iout,*) llcg
c
c     character variables:
c
      do 1 i=1,5
1        read(iout,1000) otitle(i)
      read(iout,1101) potnam,potsec,potgeo,numtyp
      read(iout,1100) (mortyp(i),idmn(i),i=1,8)                         0727OF97
      read(iout,1200) fstep,curv,state,prpart,cezero,psign
1000  format(a80)
1101  format(a8)
1100  format(5(a9,a7,a9))
1200  format(a5,a7,a5,a3,a9,a7)
c
      read(iout,1201) vharmr
      read(iout,1201) cmodet
1201  format(a9)
c
c     integer variables
c
       read(iout,*) ipot,iunxt,ipath,isym,ispot,igpot,ifprnt,iprxnt
       read(iout,*) jniter,ifirst
       read(iout,*) (label(i),i=1,natom)
       do 10 i=1,8
         read(iout,*) jxfreq(i),iproj(i)
         read(iout,*) irepr(i),ncnst(i),ndiat(i),istatu(i),ifreu(i)     0725YC97
         read(iout,*) ncnst(i),ndiat(i),imor(i),imtyp(i)
         read(iout,*) imorqq(i),iqqwkb(i),iwkb(i),ivary(i)
         read(iout,*) iolin(i),ikbprt(i),ikbqua(i),iharm(i),iqqsem(i)   0507YC97
         read(iout,*) nmodes(i),nmnumb(i),nxmod(i)
10     continue
       read(iout,*) icnst
       read(iout,*) mniter,ifit1,ifit2,mnprmv
       read(iout,*) ixmode
       read(iout,*) inxtpt,isfrst,ieuler,ies1,ipagem,iexrct,iexprd
       read(iout,*) iprstp,iprsve,iprsmd,iwign,izct,ispec,ispcpr
       read(iout,*) nstep,isct,ilctdum,isplne,nlang,ifrate,ibrate       0708JC00
       read(iout,*) nrev,nsigf,nsigr,ngflag,ieact,ianaly,itst,nfcvt
       read(iout,*) icvt,muvt,nfus,iprg,iprigt,iprt,iprvib,igspec
       read(iout,*) igtemp,imerg,itunnl,irate,iemin,nexcit,nregon
       read(iout,*) nmodes,imdmov
! JZ 2014
!      read(iout,*) nmode,imdmov
       read(iout,*) nptinf,ipprob,ipfreq,ihess,imeff,ireord
c modified -AFR ilct was changed by the dummy variable ilctdum          0708JC00
c modified - YC added new variable ivicm
       read(iout,*) ipdat,intlct,ivicm,ivico,ipvib,ipvibp,ipvibc,
     *              ivic,ivice
c modified - YC end
       read(iout,*) lgs,lgs2
       read(iout,*) irods,ivrp                                          0930PF97
c
c      floating point variables
c
       read(iout,*) (xmass(i),i=1,natom),xnmstp,convg,scale,dlx1
       do 20 i=1,8
         read(iout,*) xdemin(i),xantlr(i)
         read(iout,*) xwkbtl(i)
20     continue
       read(iout,*) delex,alpha
       read(iout,*) xdqqp
       read(iout,*) sanhrm
       read(iout,*) xsmmvt,xspmvt
c
       return
       end subroutine rvar6
c
c ***************************************************************************
c     rvary
c ***************************************************************************
c
      subroutine rvary(string,istrt,jtype)
      use perconparam, only : fu5,fu6,natom
      use kintcm; use keyword_interface; use cm
c
c     Subroutine to read in the the anharmonicity desired for reactants,
c     products, wells, and or the saddle point.
c
c     Jtype = 1  first reactant 
c     Jtype = 2  second reactant 
c     Jtype = 3  first product 
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactants wells
c     Jtype = 8  product wells
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     read in each line of the geometry until an 'end' is found
c
      call rline(fu5,string,istrt,isect,iend)
      nxm = 1
c
      do while(string(istrt:istrt+2).ne.'end'.and.nxm.le.3*natom)
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  variable anharmonicity option must end',
     *               ' an END')
            stop
         end if
c
c        parse line to get mode number
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         ixmode(nxm,jtype) = icint(string(ibegin:iend))
c
c        parse the anharmonicity method off the rest of the line
c
         vharmr(nxm,jtype) = string(istrt:istrt+8)
c
         nxm = nxm+1
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      nxmod(jtype) = nxm - 1
      return
      end
c
c ***************************************************************************
c     rvpert
c ***************************************************************************
c
      subroutine rvpert(string,istrt)
      use perconparam, only : fu5,fu6
      use kintcm
c
c     Subroutine to read in the options for using the
c     simple perturbation theory for the vibrational
c     partition functions.
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      call rline(fu5,string,istrt,isect,iend)
      ibegin = istrt                                                    09/95KAN
c
      do while(string(istrt:istrt+2).ne.'end') 
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  VIBPTOPT section must end with an END')
            stop
         end if
c
         iend = istrt - 1                                               09/95KAN
         vname = string(ibegin:iend)                                    09/95KAN
c
c        check that string is a legal variable name and what it is 
c
         if (string(istrt:istrt+7).eq.'coriolis') then 
             ipvibc = 1
         else  if (string(istrt:istrt+4).eq.'print') then
             ipvibp = 1
         else
             write(fu6,1200) vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in VIBPTOPT: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rvpert
c
c ***************************************************************************
c     rvrang
c ***************************************************************************
c
      subroutine rvrang(string,istrt,jtype)
      use perconparam, only : fu5,fu6
      use kintcm; use cm; use keyword_interface
c
c     Subroutine to read in the the anharmonicity desired for the reaction
c     path for specific regions of the reaction coordinate s.
c
c     Jtype = 1  first reactant 
c     Jtype = 2  second reactant 
c     Jtype = 3  first product 
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactants wells
c     Jtype = 8  product wells
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     check that this was used for the reaction path only
c
      if (jtype.ne.5) then
        write(fu6,1000)
        stop
      end if
c
1000  format(3x,'ERROR:  variable anharmonicity along s, VRANGE, can',
     *     ' only be used with the saddle point or starting point')
c
c     read in each region until an 'end' is found
c
      call rline(fu5,string,istrt,isect,iend)
      nregon = 0
c
      do while(string(istrt:istrt+2).ne.'end')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1100)
1100        format(3x,'ERROR:  vrange anharmonicity option must end',
     *               ' with an END')
            stop
         end if
c
c        parse line to get limits on s for this region
c
         nregon = nregon+1
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         sanhrm(nregon,1) = cfloat(string(ibegin:iend))*gufac5          0405JZ07
c
c        parse line to get outer limit on s for this region 
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         sanhrm(nregon,2) = cfloat(string(ibegin:iend))*gufac5          0405JZ07
c
c        parse line to get mode number 
c
         ibegin = istrt
         call rword(string,istrt,ierr)
         iend = istrt - 1
         nmode(nregon) = icint(string(ibegin:iend))
c
c        parse the anharmonicity method off the rest of the line
c
         cmodet(nregon,nmode(nregon)) = string(istrt:istrt+8)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      return
      end subroutine rvrang
c********************************************************************
c RVRCOPT
c********************************************************************
c
      subroutine rvrcopt(string,istrt)
      use perconparam, only : fu5,fu6
      use kintcm; use cm
c
c subroutine to read in the options for VRC-TST
c
      implicit double precision (a-h,o-z)
      character * 80 string
      character * 80 vname
c
      narg = 1
      call rline(fu5,string,istrt,isect,iend)
c
      do while(string(istrt:istrt+2).ne.'end'.and.narg.le.5)
c
         narg = narg + 1
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  VRCOPT section must end with an END')
            stop
         end if
c
c        parse line to get variable name
c
         ibegin = istrt
         call rword(string,istrt,ierr)
c
c        check that a value is also on the line
         if (ierr.eq.1) then
             write(fu6,1100) 
             stop
         end if
1100     format(3x,'ERROR:  must specify a value with NMC ')
c
         iend = istrt - 1
         vname = string(ibegin:iend)
c
c        check that vname is a legal variable name and what it is 
c
         if(vname(1:3).eq.'nmc') then
            nmc = icint(string(istrt:80))
         elseif(vname(1:5).eq. 'niter') then
            nniter = icint(string(istrt:80))
         elseif(vname(1:5).eq.'gtype') then
            igtype = icint(string(istrt:80))
         elseif(vname(1:5).eq.'jstep') then
            jstep = icint(string(istrt:80))
         elseif(vname(1:4).eq.'jmax') then
            jmax = icint(string(istrt:80))
         else
             write(fu6,1200) vname
             stop
         end if
1200     format(3x,'ERROR:  invalid variable in VRCOPT: ', a80)
c
         call rline(fu5,string,istrt,isect,iend)
      end do
c
      narg = narg - 1
      if (narg.gt.5) then
         write(fu6,1300) narg
         stop
      end if
1300  format(3x,'ERROR:  VRCOPT can have at most 5 arguments',
     *  ', it had ',i3)
c
      return
      end subroutine rvrcopt
c
c ***************************************************************************
c     rwkb
c ***************************************************************************
c
      subroutine rwkb(string,istrt,jtype)
      use perconparam, only : fu5,fu6
      use kintcm; use cm, only : xwkbtl
c
c     subroutine to read in the wkb anharmonicity options.  This keyword
c     is under the stationary point section.  Since it is a list variable
c     the data must be concluded with an 'end'. 
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     read wkb options until an end is found
c
      call rline(fu5,string,istrt,isect,iend)
      do while (string(istrt:istrt+2).ne.'end')
         if (string(istrt:istrt+5).eq.'wkbtol') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: wkbtol variable must have an',
     *           ' argument'
               stop
             else
               xwkbtl(jtype) = cfloat(string(istrt:80))
             end if
         else if (string(istrt:istrt+5).eq.'kbquad') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: kbquad variable must have an',
     *           ' argument'
               stop
             else
               ikbqua(jtype) = icint(string(istrt:80))
             end if
         else if (string(istrt:istrt+6).eq.'kbprint') then
             ikbprt(jtype)=1
c                              check if list was none of the above - error
         else
             write(fu6,1000)string(istrt:80)
1000         format(' ERROR: illegal argument to wkb -',/1x,a80) 
             stop
         end if
         call rline(fu5,string,istrt,isect,iend)
c
c                               check if end of file was found - an error
         if (iend.eq.1) then
            write(fu6,*)'ERROR: wkb list must end with an END'
            stop
         end if
      end do
c
      return
      end subroutine rwkb
c
c ***************************************************************************
c     rword
c ***************************************************************************
c
      subroutine rword(string,istrt,ierr)
c
c     This subroutine is a utility routine that takes a character string
c     and searches from the current character (istrt) for the first
c     character that occurs after at least one blank (ie for the next 
c     word on a line).  It returns the location of that character in
c     istrt.  ierr is set to 1 if no other characters were found in 
c     the string.
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     clear flags
      jblank = 0
      ierr = 0
c
c     search for the first blank - signaling the end of the current word
c
      do while (istrt.le.79.and.jblank.eq.0)
         istrt = istrt+1
         if (string(istrt:istrt).eq.' ') then
c                                               find first letter of next word
            jblank = 1
c            do while (string(istrt:istrt).eq.' '.and.ierr.eq.0)
            do while (ierr.eq.0.and.string(istrt:istrt).eq.' ')         1019BE05    
               istrt = istrt + 1
               if (istrt.gt.80) ierr=1
               if(istrt>80) exit
            end do
         end if
         if (istrt.eq.80) ierr=1
      end do
      return
      end subroutine rword
c
c***********************************************************************
c     rzvar
c***********************************************************************
c
      subroutine rzvar(string,istrt,jtype)
      use kintcm, only : nvarj
      use keyword_interface, only : avar
      use perconparam, only : fu5,fu6
c
c     Generic subroutine to read in the z matrix variables for the 
c     geometry optimizations of the reactants, products, wells, and the 
c     saddle point.  The variables are left in uppercase as 
c     required by ACES.
c
c     jtype = 1  first reactant 
c     jtype = 2  second reactant 
c     jtype = 3  first product 
c     jtype = 4  second product
c     jtype = 5  saddle point
c     Jtype = 7  reactants wells
c     Jtype = 8  product wells
c
      implicit double precision (a-h,o-z)
      character * 80 string,upcse
c
c     read in each variable until an 'end' is found
c
      call rline(fu5,string,istrt,isect,iend)
      nvar = 1
c
      do while(string(istrt:istrt+2).ne.'end')
c
c        check for input error
c
         if (isect.eq.1.or.iend.eq.1) then
            write(fu6,1000)
1000        format(3x,'ERROR:  Z matrix variables list',
     *               ' must end with and END')
            stop
         end if
c
         avar(jtype,nvar) = upcse(string(istrt:istrt+14))               0510RS95
         call rline(fu5,string,istrt,isect,iend)
         nvar = nvar + 1
      end do
c
      nvarj(jtype) = nvar-1
      return
      end subroutine rzvar
c
c ***************************************************************************
c     setanh
c ***************************************************************************
c
      subroutine setanh(jtype)
      use common_inc
      use perconparam
      use keyword_interface; use cm; use kintcm, only : nxmod
      use memanh
c
c     This subroutine will set the variables associated with the
c     variable anharmonicity option.  In the 'read' routines the
c     anharmonicity methods for each mode was stored as a character
c     variable.  Here this is translated into the corresponding 
c     integer constant.
c
      implicit double precision (a-h,o-z)
      call setanh_mem
c
c     first initalize everything to harmonic
c
      do 10 i=1,nf(jtype)
           if (jtype.le.8.and.jtype.ne.5) moder(jtype,i) = 0            0728PF97
           if (jtype.eq.5) modets(1,i) = 0
10    continue
c          
      do 1 i=1,nxmod(jtype)
        im = ixmode(i,jtype) 
        if (vharmr(i,jtype).eq.'harmonic ') then
           if (jtype.le.8.and.jtype.ne.5) moder(jtype,im) = 0           0728PF97
           if (jtype.eq.5) modets(1,im) = 0
        else if (vharmr(i,jtype).eq.'morse    ') then
           if (jtype.le.8.and.jtype.ne.5) moder(jtype,im) = 1           0728PF97
           if (jtype.eq.5) modets(1,im) = 1
        else if (vharmr(i,jtype).eq.'morseqq  ') then
           if (jtype.le.8.and.jtype.ne.5) moder(jtype,im) = 2           0728PF97
           if (jtype.eq.5) modets(1,im) = 2
        else if (vharmr(i,jtype).eq.'qqwkb    ') then
           if (jtype.le.8.and.jtype.ne.5) moder(jtype,im) = 7           0728PF97
           if (jtype.eq.5) modets(1,im) = 7
        else if (vharmr(i,jtype).eq.'qqsemi   ') then
           if (jtype.le.8.and.jtype.ne.5) moder(jtype,im) = 8           0728PF97
           if (jtype.eq.5) modets(1,im) = 8
        else if (vharmr(i,jtype).eq.'tor      ') then                   0521YC99
           if (jtype.le.8.and.jtype.ne.5) moder(jtype,im) = 9           0317YC99
           if (jtype.eq.5) modets(1,im) = 9                             0317YC99
        end if
1     continue
c
c     reorder so that the frequencies are numbered from smallest to largest
c
      do 2 i=1,nf(jtype)                                                0512WH94
         if (jtype.le.4.or.jtype.ge.7) mode(i) = moder(jtype,i)         0601YC98
         if (jtype.eq.5) mode(i) = modets(1,i)                          0601YC98
2     continue                                                          0601YC98
c 
      j = 1                                                             0601YC98
      do 3 i=nf(jtype),1,-1                                             0512WH94
         if (jtype.le.4.or.jtype.ge.7) moder(jtype,i) = mode(j)         0601YC98
         if (jtype.eq.5) modets(1,i) = mode(j)                          0601YC98
         j = j+1                                                        0601YC98
3     continue                                                          0601YC98
c
c     if (jtype.le.8.and.jtype.ne.5) then                               0728PF97
c
      if (jtype.le.8) then                                              0521YC99
        do i=1,nf(jtype)                                                0521YC99
          ntemp1(i) = ntrnb(jtype,i)                                    0521YC99
          ntemp2(i) = ntrsch(jtype,i)                                   0521YC99
          ntemp3(i) = ntrlev(jtype,i)                                   0521YC99
          ntemp4(i) = ntrnum(jtype,i)                                   0521YC99
          ntemp5(i,1) = ntrbnd(jtype,i,1)                               0521YC99
          ntemp5(i,2) = ntrbnd(jtype,i,2)                               0521YC99
          ntemp7(i) = torintrp(jtype,i)                                 1027BE05
          ntemp8(i) = torzpc(jtype,i)                                   1028BE05
          ntemp9(i) = ntrm(jtype,i)                                     0409JZ10
          ntemp10(i)= ntrmtd(jtype,i)                                   0409JZ10
          temp11(i) = readi(jtype,i)                                    0409JZ10
          do j = 1, NMAXP                                               0521YC99
            ntemp6(i,j) = ntrsig(jtype,i,j)                             0521YC99
            tempr1(i,j) = torome(jtype,i,j)                             0521YC99
            tempr2(i,j) = torw(jtype,i,j)                               0521YC99
            tempr3(i,j) = toru(jtype,i,j)                               0521YC99
            tempr4(i,j) = tormi(jtype,i,j)
            tempr5(i,j) = torwl(jtype,i,j)                              1206BE05
            tempr6(i,j) = torwr(jtype,i,j)                              1206BE05
            tempr7(i,j) = ratiol(jtype,i,j)                             1206BE05
            tempr8(i,j) = ratior(jtype,i,j)                             1206BE05
          enddo                                                         0521YC99
          do j=1,ntemp4(i)                                              0521YC99
            nsub(i,j) = ntrisb(jtype,i,j)                               0521YC99
          enddo                                                         0521YC99
        enddo                                                           0521YC99
c
        do i=1,nf(jtype)                                                0521YC99
          ntrnb(jtype,i) = ntemp1(nf(jtype)+1-i)                        0521YC99
          ntrsch(jtype,i) = ntemp2(nf(jtype)+1-i)                       0521YC99
          ntrlev(jtype,i) = ntemp3(nf(jtype)+1-i)                       0521YC99
          ntrnum(jtype,i) = ntemp4(nf(jtype)+1-i)                       0521YC99
          ntrbnd(jtype,i,1) = ntemp5(nf(jtype)+1-i,1)                   0521YC99
          ntrbnd(jtype,i,2) = ntemp5(nf(jtype)+1-i,2)                   0521YC99
          torintrp(jtype,i) = ntemp7(nf(jtype)+1-i)                     1027BE05
          torzpc(jtype,i) = ntemp8(nf(jtype)+1-i)                       1028BE05
          ntrm(jtype,i)  = ntemp9(nf(jtype)+1-i)                        0409JZ10
          ntrmtd(jtype,i)= ntemp10(nf(jtype)+1-i)                       0409JZ10
          readi(jtype,i) = temp11(nf(jtype)+1-i)                        0409JZ10
          do j = 1, NMAXP                                               0521YC99
            ntrsig(jtype,i,j) = ntemp6(nf(jtype)+1-i,j)                 0521YC99
            torome(jtype,i,j) = tempr1(nf(jtype)+1-i,j)                 0521YC99
            torw(jtype,i,j) = tempr2(nf(jtype)+1-i,j)                   0521YC99
            toru(jtype,i,j) = tempr3(nf(jtype)+1-i,j)                   0521YC99
            tormi(jtype,i,j) = tempr4(nf(jtype)+1-i,j)
            torwl(jtype,i,j) = tempr5(nf(jtype)+1-i,j)                  1206BE05
            torwr(jtype,i,j) = tempr6(nf(jtype)+1-i,j)                  1206BE05
            ratiol(jtype,i,j) = tempr7(nf(jtype)+1-i,j)                 1206BE05
            ratior(jtype,i,j) = tempr8(nf(jtype)+1-i,j)                 1206BE05
          enddo                                                         0521YC99
          do j=1,ntrnum(jtype,i)                                        0521YC99
            ntrisb(jtype,i,j) = nsub(nf(jtype)+1-i,j)                   0521YC99
          enddo                                                         0521YC99
        enddo                                                           0521YC99
      endif                                                             0521YC99
c
      return
      end subroutine setanh
c
c
c ***************************************************************************
c     setlgs
c ***************************************************************************
c
      subroutine setlgs
c
c     This subroutine takes the variables and flags set by evaluating the
c     keywords and discerns the correct LGS flags.  This will only be 
c     done for those which are independent of reactant, product and the
c     saddle point. 
c
      use common_inc; use perconparam; use keyword_interface
      use rate_const; use potmod
      use kintcm
      use cm, only : iprxnt,sdebg1,sdebg2
      implicit double precision (a-h,o-z)
c
      lgs=0
c
c LGS(1):  calculate saddle point geometry
c
      if (inosad.eq.1) then                                             021097JC
        lgs(1) = 0
      else if (iprxnt.eq.0) then
        lgs(1) = 1
      else
        lgs(1) = 2
      end if
c
c LGS(2): Calculate normal modes at initial generalized TS
c
      if (jxfreq(5).eq.0) then
         lgs(2) = 0
      else if (ifprnt.eq.0) then
         lgs(2) = 1
      else
         lgs(2) = 2
      end if
c
c LGS(3):  Calculate minimum energy path
c
      if (ipath.eq.1) lgs(3) = 1 
      if (iprstp.eq.1) lgs(3) = 2
      if (iprstp.eq.0.and.ipath.eq.1.and.ihess.eq.0) lgs(3) = 3
      if (isym.eq.1) lgs(3) = -lgs(3)
c
c LGS(4):  Print reaction path information at each save point
c
      if (iprsve.eq.1) lgs(4) = 1
      if (iprsmd.eq.1) lgs(4) = 2
c
c LGS(5): dependent on jtype (see stlgsj) 
c
c LGS(6):  Calculate geometries and properties of reactants and products
c
c                                         ** no reactants or products
      if ((irepr(1).eq.0).and.(irepr(3).eq.0)) then                     0808YC97
           lgs(6) = 0                                                   0814RS95
c                                         ** two reactants
      else if (irepr(1).eq.1.and.irepr(2).eq.1) then                    0814RS95
         if (irepr(3).eq.1.and.irepr(4).eq.1) then
            lgs(6) = 1
         else
            lgs(6) = 2
         end if
c                                         ** one reactant
      else if (irepr(3).eq.1.and.irepr(4).eq.1) then
           lgs(6) = 3
      else
           lgs(6) = 4
      end if
c
c LGS(7):  Calculate conv. or generalized T.S. theory rate constants
c
      if (irate.eq.0) then
         nfcvt = 0
         itst  = 0
      else
        if (itst.eq.1.and.nfcvt.eq.1) then
           if (iprg.eq.0) then
              lgs(7) = 1
           else 
              if (iprt.eq.0) then
                 lgs(7) = 2
              else 
                 if (prpart.eq.'rp') then
                    lgs(7) = 3
                 else 
                    lgs(7) = 4
                 end if
              end if
           end if
        else if (itst.eq.1.and.nfcvt.eq.0) then
           if (iprg.eq.0) then
              lgs(7) = -1
           else 
              if (iprt.eq.0) then
                lgs(7) = -2
              else 
                lgs(7) = -4
              end if
           end if
        end if
      end if

c
c LGS(8):  Restart option
c
      lgs(8) = iunxt
c
c LGS(9):  Calculate tunneling corrections
c
      if (itunnl.eq.0) then
        isct = 0
        izct = 0
      else
        if (izct.eq.1) then
           if (isct.eq.0.and.ilct.eq.0) then
              lgs(9) = -1
           else if (isct.eq.1.and.ilct.eq.0) then
              lgs(9) = 1
           else if (isct.eq.1.and.ilct.gt.0.and.nexcit.eq.0) then       0708JC00
              lgs(9) = 2
           else if (isct.eq.1.and.ilct.gt.0.and.nexcit.lt.0) then       0708JC00
              lgs(9) = 3
           else if (isct.eq.1.and.ilct.gt.0.and.nexcit.gt.0) then       0708JC00
              lgs(9) = -(nexcit+1)
           end if
        end if
      end if
c
c LGS(11):  Read special list of s values for mode analysis
c
      if (ispec.eq.1.and.ispcpr.eq.0) lgs(11) = 1
      if (ispec.eq.1.and.ispcpr.eq.1) lgs(11) = 2
c
c LGS(12):  Use extrapolation methods at large abs(s)
c
      if(iexrct.eq.1) then
        if (iexprd.eq.0) then
           lgs(12) = 1
        else
           lgs(12) = 3
        end if
      else if (iexprd.eq.1) then
        lgs(12) = 2
      end if
c      
c LGS(13) and LGS(14):  not used
c
c LGS(15):   dependent on jtype (see stlgsj)
c
c LGS(16):  Calculate reverse rates
c
      if (ibrate.eq.1) then
        lgs(16) = 0
      else if (ifrate.eq.1) then
        lgs(16) = 1
      end if
c
c LGS(17):  Use scale factor for reverse rates
c
      lgs(17) = nrev
c
c LGS(18) and LGS(19):  not used
c
c LGS(20):  Calculate ICVT rate constants
c
      if (icvt.eq.1) then
        if (iprigt.eq.0) then
           lgs(20) = 1
        else
           lgs(20) = 2
        end if
      end if
c
c LGS(21):  Calculate muVT and US rates
c
      if (muvt.eq.1) then
        if (nfus.eq.0) then
           lgs(21) = 1
        else
           lgs(21) = 2
        end if
      end if
c
c LGS(22):  Continue ICVT calculation even if VAD or DELG is near an endpoint
c
      lgs(22) = ngflag
c
c LGS(23):  Calculate state-selected rate constants
c
      if (state.eq.'therm') lgs(23) = 0
      if (state.eq.'adiab') lgs(23) = 1
      if (state.eq.'diab ') lgs(23) = 2
c
c LGS(24):  not used
c
c LGS(25):   dependent on jtype (see stlgsj)
c
c LGS(26):   not used
c
c LGS(27):  Direction of unbound vector
      if (psign.eq.'product') then
          lgs(27) = 0
      else
          lgs(27) = -1
      end if
c
c LGS(28):  Minimum E for percentage of tunneling
c
      if (iemin.ne.0) lgs(28) = 1
c
c LGS(29):  Method for computing reaction path curvature components
c
      if (curv.eq.'oneside') lgs(29) = 0
      if (curv.eq.'dgrad  ') lgs(29) = 1                                0816YC96
      if (curv.eq.'dhess  ') lgs(29) = 2                                0816YC96
c
c LGS(30):  Use electronic structure input file information
c
      if (iwrt30.eq.1) lgs(30) = -1
      if (iwrt31.eq.1) lgs(30) = -3                                     0810JC97
      if (potnam(1:6).eq.'unit30') lgs(30) = 1                          0919RS96
      if (potnam(1:6).eq.'unit40') lgs(30) = 2                          073096PF
      if (potnam(1:6).eq.'unit31') lgs(30) = 3                          0810JC97
c
c LGS(31):  Integrator for MEP calculation
c
      if (fstep.eq.'cubic') lgs(31) = 30
      if (ies1.eq.1.and.fstep.eq.'nmode') lgs(31) = 2
      if (ies1.eq.1.and.fstep.eq.'cubic') lgs(31) = 32
      if (ipagem.eq.1.and.fstep.eq.'nmode') lgs(31) = 5
      if (ipagem.eq.1.and.fstep.eq.'cubic') lgs(31) = 35
      if (fstep.eq.'gradi') lgs(31) = 0                                 021097JC
      if (ies1.eq.1.and.fstep.eq.'gradi') lgs(31) = 2                   021097JC
      if (ipagem.eq.1.and.fstep.eq.'gradi') lgs(31) = 5                 021097JC

c
c LGS(32):  Small-curvature effective mass interpoation
c
      if (isplne.eq.1) then
        lgs(32) = 0
      else
        lgs(32) = nlang
      end if
c
c LGS(33):   dependent on jtype (see stlgsj)
c
c LGS(34):   dependent on jtype (see stlgsj)
c
c
c LGS(35):  Direct Dynamics option  -- removed in v7.3
c
      lgs(35) = 0
c      if (potnam.ne.'hooks'.and.lgs(30).lt.1)                           0312YC97
c     *           lgs(35) = 1                                            0621RS95
c
c LGS(36):  Compute only the geometry of the next point
c
      lgs(36) = inxtpt
c
c LGS(37):  Total vibrational partition functions are scaled by
c
      lgs(37) = iscale
c
c
c LGS2(1):   stop calculation after reading and checking input
c
      lgs2(1) = icheck
c
c LGS2(2):   dependent on jtype (see stlgsj)
c
c LGS2(3):   removed in this version of polyrate
c
      lgs2(3) = 0
c
c LGS2(4):  energy of reactants at their classical equil. geom.
c
      if (cezero.eq.'calculate') lgs2(4) = isup
      if (cezero.eq.'read') lgs2(4) = 2 
c
c  force reactants == supermol; calculate == nosupermol                 0401YC97
c  leave the read option
c  
      lgs2(4) = isup
      if (cezero.eq.'read') lgs2(4) = 2
c
c LGS2(5):  print n pieces of POTINF from the PES along the MEP
c
      lgs2(5) = nptinf
c
c LGS2(6):  not used
c
c LGS2(7):  Not needed - input incorporated into file fu5
c
c LGS2(8):  Print out extra details of LCT calculations
c
      lgs2(8) = ipdat
c
c LGS2(9):  Order of interpolation used to calculate the LCG 
c           theta integrals, excited state potential, the
c           vibrational period, and the sine of the angle
c           between the MEP and the tunneling path.
c
      lgs2(9) = intlct
c
c LGS2(10):  Do the zero-order interpolated corrections with            03/96/YC
c            different methods. ICA,ICL,ICR and default is ICL          1203YC96
      lgs2(10) = ivicm                                                  03/96/YC
c
c LGS2(11):  Do the zero-order interpolated corrections
c
      if (ivic.eq.0) then
         lgs2(11) =  0
      else
         lgs2(11) = ivico
      end if
c
c
c LGS2(39): Coordinates for bound state vibrations                      07/95KAN
c        lgs2(39) =  0  cartesian coordinates                           07/95KAN
c        lgs2(39) =  1  internal coordinates (type curv1)               07/95KAN
c        lgs2(39) =  2  internal coordinates curv1 with additional      07/95KAN
c                        output at each save point (debug)              07/95KAN
c        lgs2(39) =  3  internal coordinates (type curv2)               07/95KAN
c        lgs2(39) =  4  internal coordinates curv2 with additional      07/95KAN
c                        output at each save point (debug)              07/95KAN
c        lgs2(39) =  5  internal coordinates (type curv3)               0626YC97
c        lgs2(39) =  6  internal coordinates curv3 with additional      0626YC97
c                        output at each save point (debug)              0626YC97
c     if s values between sdebg1 and sdebg2 print debug output          07/95KAN
c        in the internal coordinate routines                            07/95KAN
c                                                                       07/95KAN
      if (coord.eq.'curv1') then                                        07/95KAN
         lgs2(39) = 1                                                   07/95KAN
      else if(coord .eq.'curv1'.and.(abs(sdebg1)-abs(sdebg2)).ne.0.0)   07/95KAN
     > then                                                             07/95KAN
         lgs2(39) =  2                                                  07/95KAN
      else if (coord.eq.'curv2') then                                   07/95KAN
         lgs2(39) = 3                                                   07/95KAN
      else if(coord .eq.'curv2'.and.(abs(sdebg1)-abs(sdebg2)).ne.0.0)   07/95KAN
     > then                                                             07/95KAN
         lgs2(39) =  4                                                  07/95KAN
      else if (coord.eq.'curv3') then                                   0626YC97
         lgs2(39) =  5                                                  0626YC97
      else if(coord .eq.'curv3'.and.(abs(sdebg1)-abs(sdebg2)).ne.0.0)   0626YC97
     > then                                                             0626YC97
         lgs2(39) =  6                                                  0626YC97
      else                                                              07/95KAN
         lgs2(39) = 0                                                   07/95KAN
      end if                                                            07/95KAN
c 
c LGS2(38):  Do the zero-order interpolated corrections                 07/95KAN
c
      if (ivtst.eq.0) then                                              07/95KAN
         lgs2(38) =  0                                                  07/95KAN
      elseif(ivtst.eq.1) then                                           07/95KAN
         lgs2(38) =  1                                                  07/95KAN
      else                                                              07/95KAN
         lgs2(38) = -1                                                  07/95KAN
      end if                                                            07/95KAN
c
c
c LGS2(12):  Quantized reactant state tunneling
c
      if (lgs(6).ge.3) lgs2(12) = iqrst                                 0707WH94
c
c LGS2(13):  print mep information to fu25,26
c
      lgs2(13) = iprmep                                                 0705WH94
c
c LGS2(14):  xmol output
c
      lgs2(14) = ixmol                                                  0705WH94
c
c LGS2(15):  calculate vibrational partition functions
c            using the simple perturbation treatment?
c
      if (ipvib .eq.0) then
         lgs2(15) = 0
         if (idcpt.ne.0) lgs2(15)=13                                    0317Yc99
      else
         if (ipvibc.eq.0.and.ipvibp.eq.0) then
            lgs2(15) = 1
         else if (ipvibc.eq.1.and.ipvibp.eq.0) then
            lgs2(15) = 2
         else if (ipvibc.eq.0.and.ipvibp.eq.1) then
            lgs2(15) = 11
         else if (ipvibc.eq.1.and.ipvibp.eq.1) then
            lgs2(15) = 12
         end if
      end if
c
c LGS2(16): calculate the generalized free energy of activation
c           using the logarithm of the partition function instead
c           of the product of the partition functions
c
      lgs2(16) = igtlog                                                 0423TA02
c
c LGS2(17): number of states to be included in the quantized reactant 
c           state tunneling. A value of -1 means that all states should 
c           be included. A value of 0 means that only the groud state
c           should be included
c
      lgs2(17) = iqrnst                                                 0423TA02
C
C     If other electronic structure package is used to obtain the PES, the 
C     following lgs options are set to fixed values
C
c  comment out - 0327YC97
c
c      if (lgs(35) .ne. 0) then                                         0502WH94
c      if (isup.eq.1) then
c         lgs(24) = 0                                                   0502WH94
c         lgs(30) = 0                                                   0502WH94
c         lgs(33) = 0                                                   0502WH94
c         lgs(34) = 0                                                   0502WH94
c         lgs(36) = 0                                                   0502WH94
c      endif
c
c
c LGS3 is added for Force constant scaling                              0211PJ01
c
       do i = 1, 5                                                      0211PJ01
           lgs3(i) = 0                                                  0211PJ01
       end do                                                           0211PJ01
c
c LGS3(1): The force constants scaling is used for scaling potential    0211PJ01
c          or used for scaling the frequencies                          0211PJ01
c          LGS3(1) = 0 --- no scaling for force constant                0211PJ01
c             ..   = 1 --- just scale the frequencies                   0211PJ01
c             ..   = 2 --- scale the effective potential                0211PJ01
c
      if (ifcfac .eq. 0) then                                           0211PJ01
          lgs3(1) = 0                                                   0211PJ01
      else if (isclpt .eq. 0) then                                      0211PJ01
          lgs3(1) = 1                                                   0211PJ01
      else                                                              0211PJ01
          lgs3(1) = 2                                                   0211PJ01
      end if                                                            0211PJ01
c
c CHECK for imcompatablity of fcscale, cartrp, and coordinate system    0223PJ01
c
      if( icartrp.eq.1 .and. ifcfac .eq. 1) then                        0223PJ01
           write(fu6,*) 'ERROR: can not use CARTRP with FCSCALE'        0223PJ01
           stop
      end if
c
      if( lgs2(39).lt.3 .and. ifcfac.eq.1) then                         0223PJ01
           write(fu6,*) 'ERROR: can not use Cartesian coordinate and ', 0223PJ01
     *                  'curv1, when FCSCALE is used.'                  0223PJ01
           stop                                                         0223PJ01
      end if                                                            0223PJ01
c
c --- convert the default to nocartrp if no 'NOCARTRP' is specified     0815PJ01
c     when 'FCSCALE' is used                                            0815PJ01
c
c     if (icartrp.eq.-1 .and. ifcfac.eq.1) then                         0815PJ01
c          icartrp = 0                                                  0815PJ01
c     end if                                                            0815PJ01
c     The line above were commented by TVA.                             0625TA02
c  
c
c LGS3(2): Use internal coordinate normal mode analysis for             0214PJ01
c          Reactant and products                                        0214PJ01
c                                              
c          LGS3(2) = 0 --- use cartesian for R.P.                       0214PJ01
c          LGS3(2) = 1 --- use internal coordinate for R.P.             0214PJ01
c                          (default when curvlinear is used in COORD)   0214PJ01
c                                  
      if ((lgs2(39) .ge. 3) .and. (icartrp .eq. 0)) then                0215PJ01
           lgs3(2) = 1                                                  0215PJ01
      end if
c
c LGS3(3) for MEP and representative tunneling path turnning point      0411PJ01
c pesdo-Jacobian mass-scaled Jacobian coordinates                       0411PJ01
c
      if (irtpjac .eq. 1) then                                          0411PJ01
         lgs3(3) = 1                                                    0411PJ01
      end if                                                            0411PJ01
c       
      return
      end subroutine setlgs
c
c ***************************************************************************
c     setvar
c ***************************************************************************
c
      subroutine setvar
      use common_inc
      use perconparam
      use kintcm; use cm; use rate_const
      use keyword_interface, only : gufac5
c
c     This is an interface routine between the old input format and
c     hence the variables expected by POLYRATE and the new input.  This
c     routine will set all global variables.  SETVARJ will set those variables
c     that depend on reactants, products, saddlepoint or the reaction path.
c
c     This routine contains much of the code present in main in Polyrate
c     version 4.0.
c
c
      implicit double precision (a-h,o-z)
      
c
c     set general variables
c
c    Check for restart run -- only do these calculations for non-restart
      if (lgs(8) .le. 0) then
c
c         Write out restart information if WRITEFU1 or WRITE3 
c         keywords were chosen
c
c          if (iunxt.eq.-1.or.iunxt.eq.3) then 
c             call dattim (fu1)
c             call wtitle (fu1)
c          endif
c
           if (ibathm.eq.1) then                                        0317YC99
             n3 = 3 * natom + 1                                         0317Yc99
             nf(5) = nf(5) + 1                                          0317Yc99
           else                                                         0317Yc99
             n3 = 3 * natom                                             0317YC99
           endif                                                        
c
c         Scale atomic masses to reduced mass of reactants              
c         Redefine xmass array to hold coordinate mass factors          
c         Repeat for each coordinate                                    
c                                                                       
           do 10 i = 1, natom                                           
              svmas(i) = xmass(i)/cau                                  
              xmass(i) = sqrt(xmass(i)/redm)                          
   10      continue
           l = 0
           do 30 i = 1, natom
              do 20 j = 1, 3
                 l = l+1
                 amass(l) = xmass(i)
   20         continue
   30      continue
c
c mass of the effective solvent coordinate 
C
          if (ibathm.eq.1) then                                         0317YC99
             amass(n3) = sqrt(redm/redm)                                0317YC99
          endif
c
          do 40 i = 1,nf(5)                                             0325WH94
             modets(2,i) = lgs(5)
40        continue
          narr = 1
      endif
c
c  Convert PEMIN to KCAL
c
      pemin = pemin/ckcal
c
c  Set flag for special free energy curve region
c
      ngspec = -igspec
      if (igtemp.gt.0) ngspec = 1
c
c  Set indicies for temperatures used in activation energies
c  Currently the temperatures are stored in etpair(40,2) and
c  are not correlated with the temperatures for which the
c  rates are computed.
c
      do 50 i=1,npair
         ind1 = 0
         ind2 = 0
         temp1 = etpair(i,1)
         temp2 = etpair(i,2)
         do 60 j=1,ntemp
            if (temp(j).eq.temp1) then
              ind1 = 1
              it1(i) = j
            end if
            if (temp(j).eq.temp2) then
              ind2 = 1
              it2(i) = j
            end if
60       continue
         if (ind1.eq.0) then
            write(fu6,1000) npair, temp1
            stop 'setvar 1'
         end if 
         if (ind2.eq.0) then
            write(fu6,1000) npair, temp2
            stop 'setvar 2'
         end if 
50    continue
1000  format(5x,'The temperature in pair ',i2,' for the activation',
     *      ' energies is invalid:  ',f10.6
     *      / 5x,' All temperatures must be values at which the ',
     *        'reaction rate was computed')
c
c     Set variable (ntrat) for number of temperatures at which a detailed
c     analysis is desired
c
      ntrat = ianaly
      itr(1) = 0
      ntrat1 = ntrat + 1
      do 70 i=2,ntrat1
         ind1 = 0
         temp1 = analt(i-1)
         do 80 j=1,ntemp
            if (temp(j).eq.temp1) then
              ind1 = 1
              itr(i) = j
            end if
80       continue
         if (ind1.eq.0) then
            write(fu6,1100) temp1
            stop 'setvar 3'
         end if
70    continue
1100  format(5x,'The temperature for the detailed analysis',
     *      ' is invalid:  ',f10.6
     *      / 5x,' All temperatures must be values at which the ',
     *        'reaction rate was computed')

c
c     Convert reduced masses to a.u.
c
      if (lgs(8).le.0) redm = redm/cau
c
      if (lgs(8).le.0) call redgeo
c
c     Set tunneling logical variables
c
      if (itunnl.eq.1) then
        ltun = .true.
        lmep = .true.
        if (lgs(9) .lt. -1 .or. lgs(9) .gt. 0) lcdsc = .true.
        if (abs(lgs(9)) .ge. 2) llcg = .true.
        if (lgs(9) .eq. 2) llcgg = .true.
      end if
c
c     Set IOT
c
      if (ipprob .eq. 1) iot = 1                                        0518WH94
      if (ipfreq .eq. 1) iot = 2                                        0518WH94
c
c     Set up array needed for state-selected rate constant calculation
c
      i = 1
      do 90 k=1,4
        if(irepr(k).eq.1.and.nf(k).gt.0) then
          do 100 j=1,nf(k)
            lrp(i) = l9(k,j)
            i = i+1
100       continue
        end if
90    continue
c 
c     Convert ISWR and ISWP to the internal convention of the mode number
c
      iswr = nf(1) + 1 - iswr                                           0527WH94
      iswp = nf(1) + 1 - iswp                                           0527WH94c
c
      return
      end subroutine setvar
c
c ***************************************************************************
c     stlgsj
c ***************************************************************************
c
      subroutine stlgsj(jtype)
      use common_inc
      use perconparam
      use kintcm
      use keyword_interface
c
c     This subroutine sets the LGS flags for the variables that are 
c     dependent on jtype. 
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 7  reactants wells
c     Jtype = 8  product wells
c
c
      implicit double precision (a-h,o-z)
c
c LGS(5):  Anharmonicity
c
      if (iharm(jtype).eq.1) then
         lgs(5) = 0
      else if (imor(jtype).eq.1) then 
         lgs(5) = 1
      else if (imorqq(jtype).eq.1) then
         lgs(5) = 2
      else if (iqqwkb(jtype).eq.1) then
         lgs(5) = 7
      else if (iqqsem(jtype).eq.1) then
         lgs(5) = 8
      else if (ivary(jtype).eq.1) then
         lgs(5) = 21
      else
         lgs(5) = nregon+20
      end if
c
c LGS(15):  Choice of morse model
c
      lgs(15) = 0
      if (imtyp(jtype).eq.1) then
         if (mortyp(jtype).eq.'morsei')     lgs(15) = 1
         if (mortyp(jtype).eq.'morseiii')   lgs(15) = 2
        if (mortyp(jtype).eq.'morseii')    lgs(15) = 3
         if (mortyp(jtype).eq.'morseia')    lgs(15) = 4
         if (mortyp(jtype).eq.'morseiiia')  lgs(15) = 5
      end if
c
c LGS(25):  Second derivative calculation
c
      if (numtyp.eq.'central  ') lgs(25) = 0
      if (numtyp.eq.'quadratic') lgs(25) = 1
      if (numtyp.eq.'fourth   ') lgs(25) = 2
c
c LGS(33):  Use WKB off true potential
c
      if (iwkb(jtype).eq.1) lgs(33) = 1
c
c LGS(34):  Solid-state or gas
c
      if (icode(jtype).lt.0) lgs(34) = 1
c
c LGS2(2): Projection of overall trans. and rot. at stat. pts.
c
      if (iproj(jtype).eq.1) lgs2(2) = 1
c
c
c LGS(10):  Derivative calculation option
c
      if (lgs(33).eq.1) then
         lgs(10) = 4
      else if (lgs(5).eq.0) then
         lgs(10) = 2
      else if (lgs(5).le.1.and.(lgs(15).eq.1.or.lgs(15).eq.4)) then
         if (lgs(9).eq.0) then
            lgs(10) = 2       
         else
            lgs(10) = 3
         end if
      else if (lgs(5).le.1) then
         lgs(10) = 3       
      else 
         lgs(10) = 4       
      end if
c
      return
      end subroutine stlgsj
c
c ***************************************************************************
c     stvarj
c ***************************************************************************
c
      subroutine stvarj(jtype)
      use cm; use dxiz; use common_inc
      use kintcm
      use rate_const
c
c     This is an interface routine between the old input format and
c     hence the variables expected by POLYRATE and the new input.  
c     It will set those variables needed for reactants, products, 
c     saddlepoint or the reaction path.
c
c     This routine contains much of the code present in main in Polyrate
c     version 5.0.
c
c
      implicit double precision (a-h,o-z)
c
c     set general variables
c
      demin = xdemin(jtype)
      antlr = xantlr(jtype) 
c                                  scale factor: newton search for stat. pt.
      step = scale
c                                  second deriv. step size
      dlx = xnmstp
c                                  first deriv. step size
      if (ifirst.eq.1) then
        dlx2 = dlx1
      else
        dlx2 = dlx
      end if
c
c                                  2 distances for fit
      dqqp(1) = xdqqp(jtype,1)
      dqqp(2) = xdqqp(jtype,2)
c
      cnvg = convg
      derstp = xnmstp
      niter = jniter
      if (istatu(jtype).ne.0) niter = -1                                0725YC97
c                                  wkb control information
      wkbtol = xwkbtl(jtype)
      kbquad = ikbqua(jtype)
      kbprnt = ikbprt(jtype)
c
      return
      end subroutine stvarj
C
C***************************************************************************
C UPCSE 
C***************************************************************************
C
      CHARACTER*80 FUNCTION upcse(STRING)
C
C   Function which takes a string of 80 characters and converts the 
C   lower case letters in the string to upper case letters
C   This functions is a modified version of CASE which was written
C   by Rozeanne Steckler
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER(*) :: STRING
      CHARACTER(80) :: LINE
      CHARACTER * 1 XLETT
C
      LINE = STRING
      DO 10 I = 1, 80
         XLETT = LINE(I:I)
         ITRY = ICHAR (XLETT)
         IF (XLETT .GE. 'a' .AND. XLETT .LE. 'z') THEN 
            ITRY = ITRY - 32
            LINE(I:I) = CHAR (ITRY)
         ENDIF
10    CONTINUE
C
      UPCSE  = LINE 
C
      RETURN
C
      END
c
c ***************************************************************************
c     wtitle
c ***************************************************************************
c
      subroutine wtitle (io)
      use keyword_interface, only : ftitle
c
c     read/write title from/to device IO
c
c     This subroutine was rewritten in March 1992 for version 5.0.
c     It now handles up to a 5 line title.  It no longer reads the title
c     just writes it out.
c
c     IRW <=  0, formatted read,
c     IRW >=  1, formatted write
c
C     called by:
C                MAIN,FINOUT,RESTOR,RPHWRT,RPHSET,TABLE
C
      implicit double precision (a-h,o-z)
c
c
      if (io.le.0) return
      write(io,1000)
      do 1 i=1,5
          if (ftitle(i).ne.' ') write(io,1001) ftitle(i)
1     continue
      write(io,1002)
1000  format(//,80('-'),/80('-')//)
1001  format(a80)
1002  format(//80('-'))
c
      return
      end subroutine wtitle
C***********************************************************************
C  wvar6
C***********************************************************************
C
      subroutine wvar6 (iout)
      use keyword_interface; use common_inc; use perconparam
      use rate_const; use cm; use kintcm
c
c     write necessary info to unit iout -- this is only for the new 
c     variables introduced by the new interface in version 6.0
c
c    called by:
c               restrt
c
c
      implicit double precision (a-h,o-z)
c
c     write logical variable for tunneling
      write(iout,*) llcg
c
c     character variables:
c
1000  format(a80)
      do 1 i=1,5
1     write(iout,1000) ftitle(i)
      write(iout,1001) potnam,potsec,potgeo,numtyp
      write(iout,1100) (mortyp(i),idmn(i),i=1,8)                        0727PF97
      write(iout,1200) fstep,curv,state,prpart,cezero,psign
1001  format(a8)
1100  format(5(a9,a7,a9))
1200  format(a5,a7,a5,a3,a9,a7)
c
      write(iout,1201) vharmr
      write(iout,1201) cmodet
1201  format(a9)
c
c     integer variables
c
       write(iout,*) ipot,iunxt,ipath,isym,ispot,igpot,ifprnt,iprxnt
       write(iout,*) jniter,ifirst
       write(iout,*) (label(i),i=1,natoms)
       do 10 i=1,8
         write(iout,*) jxfreq(i),iproj(i)
         write(iout,*) irepr(i),ncnst(i),ndiat(i),istatu(i),ifreu(i)    0725YC97
         write(iout,*) ncnst(i),ndiat(i),imor(i),imtyp(i)
         write(iout,*) imorqq(i),iqqwkb(i),iwkb(i),ivary(i)
         write(iout,*) iolin(i),ikbprt(i),ikbqua(i),iharm(i),iqqsem(i)  0507YC97
         write(iout,*) nmodes(i),nmnumb(i),nxmod(i)
10     continue
       write(iout,*) icnst
       write(iout,*) mniter,ifit1,ifit2,mnprmv
       write(iout,*) ixmode
       write(iout,*) inxtpt,isfrst,ieuler,ies1,ipagem,iexrct,iexprd
       write(iout,*) iprstp,iprsve,iprsmd,iwign,izct,ispec,ispcpr
       write(iout,*) nstep,isct,ilct,isplne,nlang,ifrate,ibrate
       write(iout,*) nrev,nsigf,nsigr,ngflag,ieact,ianaly,itst,nfcvt
       write(iout,*) icvt,muvt,nfus,iprg,iprigt,iprt,iprvib,igspec
       write(iout,*) igtemp,imerg,itunnl,irate,iemin,nexcit,nregon
!      write(iout,*) nmode,imdmov
! J. Zheng 2014
       write(iout,*) nmodes,imdmov
       write(iout,*) nptinf,ipprob,ipfreq,ihess,imeff,ireord
       write(iout,*) ipdat,intlct,ivicm,ivico,ipvib,ipvibp,ipvibc,
     *               ivic,ivice
       write(iout,*) lgs,lgs2
       write(iout,*) irods,ivrp                                         0930PF97
c
c      floating point variables
c
       write(iout,*) (xmass(i),i=1,natoms),xnmstp,convg,scale,dlx1
       do 20 i=1,8
         write(iout,*) xdemin(i),xantlr(i)
         write(iout,*) xwkbtl(i)
20     continue
       write(iout,*) delex,alpha
       write(iout,*) xdqqp
       write(iout,*) sanhrm
       write(iout,*) xsmmvt,xspmvt
c
       return
       end subroutine wvar6
c**********************************************************************
c RSST
c**********************************************************************
c
      subroutine rsst
      use perconparam, only : n3tm,fu5,fu6
      use sst
c
c     This subroutine reads the options for SS-T method applying to R.P.
c
      implicit double precision(a-h,o-z)
c
      character*80 word(40)
      logical lsec,leof
c
      lsst = 1
      nmtor = -1
10    call readln(fu5,word,nword,lsec,leof)
      if (word(1) .ne. 'END') then
         if (lsec .or. leof) then
            write(fu6,1000)
            stop 'RSST 1'
         endif
c
1000  format(/1X,'The SSTOR section must end with an END')
c
         if (word(1) .eq. 'MTOR') then
            nmtor = nword - 1
            ntor = nmtor
            call prepsst
            do i = 1, nmtor
               dmtor(i) = cfloat(word(1+i))
            enddo
c        elseif(word(1) .eq. 'NTOR') then  
c           if (nword .lt.2) then
c              write(fu6, *)' NTOR need an argument '
c              stop
c           endif
c             nmtor = icint(word(2))
c             if(ntor.ne.nmtor.and.nmtor.ne.-1) then
c               write(fu6,*)' NTOR is not consistent with number of M ' 
c    *                      ,'values'
c               write(fu6,*)'NTOR =',ntor
c               write(fu6,*)'Number of M values =',nmtor
c               stop
c             endif
         else
            write(fu6,1100)
            write(fu6,*) word(1) 
            stop 'RSST 2'
         endif
         goto 10
      endif
c
1100  format(/1X,'Unrecognized subkeyword in the SSTOR section')
c
      return
      end subroutine rsst
C
      subroutine prepsst
      use perconparam, only : n3tm,natom,fu6
      use keyword_interface, only : coord
      use cm, only : xmass
      use sst
      implicit double precision(a-h,o-z)
      call sst_mem
c
C     if(coord .ne. 'curv2' ) then
C        write(fu6,*)'Error: non-redundant internal coordinates should',
C    *               ' be used for SS-T method'
C        stop
C     endif
C
C  calculate total mass
C
      totmass = 0d0
      do i = 1, natom
         totmass = totmass + xmass(i)
      enddo      
c
      return
      end subroutine prepsst


C***********************************************************************
C  RATE
C***********************************************************************
C
      SUBROUTINE rate
      use common_inc
      use perconparam
      use rate_const
      use kintcm
      use keyword_interface, only : gufac6,iunit6
      use cm; use sst
C
C     THIS ROUTINE SETS UP TEMPERATURE LOOP
C     THEN, FOR EACH T, DELTA G(S) IS CALCULATED AND STORED
C
C      USES L0 TO TELL VPART TO INCLUDE ONLY GROUND OR FIRST EXCITED
C      STATE WHEN APPROPRIATE IN VIB. ADIAB. OR DIAB. CALC.
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C     MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
C     CALLED BY:
C                MAIN
C     CALLS:
C            RQCOM,GAUSSQ,FITMAX,KAPVA,EPART,RPART,PTQVIB,VPART,STAUV
C            SPL1B2,VTMUSN,FINOUT,HRPART
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
C
      DIMENSION TAB(3),QVSV(N6TM)
C     PARAMETER (NEMVT=90)                                              01/13B92
C     PARAMETER (NGR=220242)
C     DIMENSION ENDPTS(2),BMUVT(NEMVT),TMUVT(NEMVT),WMUVT(NEMVT)        01/13B92
      DIMENSION ENDPTS(2),BMUVT(MNITER),TMUVT(MNITER),WMUVT(MNITER)
      DIMENSION IPRMVT(6)                                               01/13B92
      DIMENSION XNMVT(6,NSDM),EMVT(6)
      DIMENSION XDG(NSDM),YDG(NSDM)                                     0929YC97
      DIMENSION NV(NVIBM),EZEROX(NVIBM),EONE(NVIBM),ENVIB(NVIBM)
 
      real(8), allocatable :: GTNS(:,:),GTN(:),DEN(:)
      save                                                              0601YC98
      call rate_mem
C
C
      IF (IFRFAC.EQ.1) GOTO 87                                          0814JC00
C
C
C     WRITE OUT SECTION HEADER
C
      WRITE (FU6,1000)
C
C     Input for RATE routine has been moved to READ5                     3/18T90
C
C     Initialize the array ENDPTS which is used in the gaussq 
C     subroutine.  
C
      ENDPTS(1) = 0.D0                                                  0113GL92
      ENDPTS(2) = 0.D0                                                  0113GL92
C
      NSEG = MIN(3,MAX(1,NSEG))
      NSEG2 = MIN(3,MAX(1,NSEG2))
C
C   SCALING THE REACTANT AND PRODUCT FREQUENCIES WHICH ARE THEN 
C   BECOME REACTION COORDINATE.
C
      IF (VFAC .NE. 1.0D0) THEN                                         4/19T89
         IF(ISWR.NE.0 .AND. LGS(6).GE.3) THEN                           4/19T89 
           VAR = VAR - 0.5D0*WER(ISWR)*(1.0D0 - SQRT(VFAC))             4/19T89
           WER(ISWR) = WER(ISWR)*SQRT(VFAC)                             4/19T89
           XER(ISWR) = XER(ISWR)/SQRT(VFAC)                             4/19T89
         ENDIF
         IF(ISWP.NE.0.AND.(LGS(6).EQ.2.OR.LGS(6).EQ.4)) THEN            4/19T89
           ISWP = NF(1) + NF(2) + ISWP                                  4/19T89
           VAP = VAP - 0.5D0*WER(ISWP)*(1.0D0-SQRT(VFAC))               4/19T89
           WER(ISWP) = WER(ISWP)*SQRT(VFAC)                             4/19T89
           XER(ISWP) = XER(ISWP)/SQRT(VFAC)                             4/19T89
         ENDIF                                                          4/19T89
      ENDIF                                                             4/19T89
C
C     WRITE OUT LIMITS ON S
C
      IF (LGS(7).LE.-1) GO TO 15                                        180491VM
      IF (NGSPEC.EQ.0) WRITE (FU6,1050)
      IF (NGSPEC.EQ.-1) WRITE (FU6,1100) SLMG/GUFAC6,SLPG/GUFAC6        0405JZ07
      IF (NGSPEC.EQ.1) THEN
         WRITE (FU6,1150)
         DO 20 I = 1, NTEMP
            WRITE (FU6,1200) TEMP(I),SLMA(I)/GUFAC6,SLPA(I)/GUFAC6      0405JZ07
   20    CONTINUE
      ENDIF
 15   WRITE (FU6,2600) NTEMP
      IF (LGS(9).NE.0) THEN                                             180491VM
         WRITE (FU6,2650) NQ12,NQ22
      ENDIF                                                             180491VM
C
C     SCALE VMEP BY VFAC
C
       WRITE (FU6,2750) VFAC
       IF (VFAC.NE.1.0D0) THEN
          DO 60 I = 1, LSAVE
             ZPE = VADIB(I)-VCLAS(I)
             VCLAS(I) = VCLAS(I)*VFAC
             VADIB(I) = VCLAS(I)+ZPE
   60     CONTINUE
       ENDIF
C                                                                        6/13T89
C SCALING QV                                                             6/13T89
C                                                                        6/13T89
      IF (LGS(37) .NE. 0) WRITE(FU6,1570) LGS(37)                        6/13T89
C
C     FIND MAX OF ADIABATIC POTENTIAL
C
      CALL FITMAX (1,LSAVE,S3,F3,S5,F5,V3X,V5,ISTART,ISTOP)             7/14YL92
C
C     WRITE OUT SUMMARY OF ADIABATIC MAXIMUM
C
      WRITE (FU6,1250)
      WRITE (FU6,1300) S3/GUFAC6                                        0405JZ07
      WRITE (FU6,1350) F3,F3*CEV,F3*AUTOCM,F3*CKCAL
      F3P = F3-EPRD
      WRITE (FU6,1400) F3P,F3P*CEV,F3P*AUTOCM,F3P*CKCAL
      FZR = F3-VAR
      WRITE (FU6,1450) FZR,FZR*CEV,FZR*AUTOCM,FZR*CKCAL
      FZP = F3-VAP
      WRITE (FU6,1500) FZP,FZP*CEV,FZP*AUTOCM,FZP*CKCAL
      WRITE (FU6,1550) S5/GUFAC6                                        0405JZ07
      WRITE (FU6,1350) F5,F5*CEV,F5*AUTOCM,F5*CKCAL
      F5P = F5-EPRD
      WRITE (FU6,1400) F5P,F5P*CEV,F5P*AUTOCM,F5P*CKCAL
      FZR = F5-VAR
      WRITE (FU6,1450) FZR,FZR*CEV,FZR*AUTOCM,FZR*CKCAL
      FZP = F5-VAP
      WRITE (FU6,1500) FZP,FZP*CEV,FZP*AUTOCM,FZP*CKCAL
      WRITE (FU6,1600)
C
C     STORE MAX OF ADIABATIC FOR ICVT CALCULATION.
C
      VAGMAX = F5
      SMAXX  = S5
      IF (LGS(21).NE.0) THEN
C
C     FIND LARGEST VALUE BETWEEN VAGMAX,VAR,VAP FOR VAGMU
C
         VAGMU = MAX(VAGMAX,VAR,VAP)
         IF (VAGMU.GT.VAGMAX) WRITE (FU6,2950)
         WRITE (FU6,3000) VAGMU,VAGMU*CKCAL
      ENDIF
      IF (LGS(34) .NE. 0) THEN
         ISHFT = 1                                                      11/20T87
      ELSE IF (ICODE(5).EQ.3) THEN                                         ..
         ISHFT = 6                                                      11/20T87
      ELSE
         ISHFT = 7
      ENDIF
      N3M7 = N3-ISHFT
      DO 80 I = 1, NTEMP
         BETA(I) = 1.0D0/(BK*TEMP(I))
   80 CONTINUE
C
C     CALCULATE TUNNELING CORRECTIONS
C
      IF (LGS(9).NE.0) THEN
C
C  SETUP INFORMATION FOR LCG3 CALCULATION WHEN THE SYSTEM IS TREATED AS
C  A HINDERED ROTOR (BUT NO OTHER ANHARMONICITY IS SET)
C
C  Certain combinations of options never NSHLF, so this initializes     1103BE05
c  the variable if it has not been initialized yet                      1103BE05
        IF (NSHLF.EQ.0) THEN                                             1103BE05
          NSHLF = 1                                                     1103BE05
        ENDIF                                                           1103BE05
         CALL KAPVA 
         VAG = VAD
      ELSE
         VAG = VAGMAX
      ENDIF
      IF (LGS(7).LE.0) RETURN                                           180491VM
      IF (LGS(21).NE.0) THEN
         niter = mniter
         CALL GAUSSQ (6,NITER,0.0D0,0.0D0,0,ENDPTS,BMUVT,TMUVT,WMUVT)   01/13B92
C
c Determine the highest energy that will be used in muVT calculations
C Highest E = TMUVT(NITER)*BKT + VAGMU ;T is the highest temp.
C HT -- Hightest temperature
C EMMAX -- Highest energy in muVT 
C Added by Jingjing Zheng Feb. 2012
C
         HT = TEMP(1)
         DO I = 1, NTEMP
           IF(TEMP(I).GT.HT) HT = TEMP(I)
         ENDDO
         EMMAX = TMUVT(NITER)*BK*HT + VAGMU
C        write(6,*)"Maximum E in muVT", EMMAX*CKCAL
C 
c     Set values for muVT options:  printing and limits on s grid for
c     NGT(s) calculations 
c
         iftmv1 = ifit1
         iftmv2 = ifit2
         nprmvt = mnprmv
         smmvt = xsmmvt
         spmvt = xspmvt
C
C  SET UP FOR PRINTING NGT(S) GRID                                      01/13B92
         IPRMVT(1) = 0                                                  01/13B92
         IF (NPRMVT.GT.0) THEN                                          01/13B92
C   GET INDICES OF ENERGY GRID FOR PRINTING                             01/13B92
            DO 9595 IPR = 1,NPRMVT                                      01/13B92

               II = (NITER*IPR)/NPRMVT                                  01/13B92
               IPRMVT(IPR) = MAX(IPR,II)                                01/13B92
9595        CONTINUE                                                    01/13B92
         END IF                                                         01/13B92
C  SET UP BOUNDS ON S FOR NGT(S) CALCULATION
         ISMMVT = 1                                                     01/13B92
         ISPMVT = LSAVE                                                 01/13B92
         IF (SMMVT.NE.0.0D0 .OR. SPMVT.NE.0.0D0) THEN                   01/13B92
            DO 9596 IS = 1,LSAVE                                        01/13B92
               IF (SSUBI(IS).LT.SMMVT) ISMMVT = IS                      01/13B92
9596        CONTINUE                                                    01/13B92
            IS = LSAVE + 1                                              01/13B92
            DO 9597 ISS = 1,LSAVE                                       01/13B92
               IS = IS - 1                                              01/13B92
               IF (SSUBI(IS).GT.SPMVT) ISPMVT = IS                      01/13B92
9597        CONTINUE                                                    01/13B92
         END IF                                                         01/13B92
c
         IF (ICODE(5) .LT. 0) THEN 
           N3M7 = N3 - 1 
         ELSE IF (ICODE(5).EQ.3) THEN
           N3M7 = N3 - 6 
         ELSE  
           N3M7 = N3 - 7
         ENDIF 
C DeltaE is set to 1 cm-1
         E0_MAX = 0d0
         ESUM = 0.D0
         DO 9598 IS = 1,LSAVE    
C
C  Calculate zero-point energy to determine the NGR
C
             ESUM = 0.D0
             DO  I = 1, N3M7
                  JSWITC = 1
                  IF (SSUBI(IS).GE.SWITC) JSWITC = 2
                  E = EVIB(WETS(I,IS),XETS(I,IS),0,Y0TS(I,IS),IS)
                  EZEROX(I) = E
                  E = EVIB(WETS(I,IS),XETS(I,IS),1,Y0TS(I,IS),IS)
                  EONE(I) = E
                  IF (LN3(JSWITC,I).EQ.1) THEN
                    ENVIB(I) = EONE(I)
                    NV(I) = 1
                  ELSE
                    ENVIB(I) = EZEROX(I)
                    NV(I) = 0
                  ENDIF
                  ESUM = ESUM+ENVIB(I)
             ENDDO
             E0 = EMMAX - VCLAS(IS) - ESUM
             IF(E0_MAX.LT.E0) E0_MAX=E0
9598      CONTINUE  
c Calculate sum of states and density of states along reaction path
c for the energies below EMMAX
c
C     
          NGR = INT(E0_MAX/EGRID)+1
          IF(.NOT.ALLOCATED(GTNS)) THEN
            ALLOCATE(GTNS(NGR,NSDM),GTN(NGR),DEN(NGR))
            GTNS = 0d0; GTN = 0d0; DEN = 0d0
          ENDIF
          IF (LSST. EQ. 0) THEN
            CALL SUMSTATES(EMMAX,VAGMU,EGRID,ISMMVT,ISPMVT,GTNS,NGR)
          ENDIF
      ENDIF                                                             01/13B92
C*
C COMPUTE THE RELATIVE TRANSLATIONAL REDUCED MASS OF THE FORWARD AND    6/13T89
C REVERSE REACTIONS AND REPLACED THE SCALING REDUCED MASSES REDM AND    6/13T89
C REDMR BY THE NONSCALING VALUES BEFORE CALCULATE THE QTR PF'S.         6/13T89
C                                                                       6/13T89
      R1MAS = 0.0D0                                                     6/13T89
      DO 82 IMS = 1,NRATOM(1)                                           6/13T89
82       R1MAS = R1MAS + SVMAS(IATSV(IMS,1))                            6/13T89
      R2MAS = 0.0D0                                                     6/13T89
      DO 84 IMS = 1,NRATOM(2)                                           6/13T89
84       R2MAS = R2MAS + SVMAS(IATSV(IMS,2))                            6/13T89
      REDMF = (R1MAS*R2MAS)/(R1MAS+R2MAS)                               10/9WH92
                                                                        6/13T89
C IF GAS MOLECULE + SOLID SURFAC THEN REDM IS THE MOLECULAR MASS        6/13T89
C OF THE GAS MOLECULE                                                   6/13T89
C                                                                       6/13T89
      IF(LGS(34).NE.0 .AND. ICODE(1).GT.0) REDMF = R1MAS                6/13T89
C                                                                       6/13T89
      P1MAS = 0.0D0                                                     6/13T89
      DO 86 IMS = 1,NRATOM(3)                                           6/13T89
86       P1MAS = P1MAS + SVMAS(IATSV(IMS,3))                            6/13T89
      P2MAS = 0.0D0                                                     6/13T89
      DO 88 IMS = 1,NRATOM(4)                                           6/13T89
88       P2MAS = P2MAS + SVMAS(IATSV(IMS,4))                            6/13T89
      REDMR = (P1MAS*P2MAS)/(P1MAS+P2MAS)                               6/13T89
      IF(LGS(34).NE.0 .AND. ICODE(3).GT.0) REDMR = P1MAS                6/13T89
C                                                                       6/13T89
C
87    CONTINUE                                                          0814JC00
C
C EVALUATE THE EXPONENT FACTOR WHICH WILL BE FACTORED OUT OF THE        6/13T89
C VIBRATIONAL PF TO AVOID UNDERFLOW.                                    6/13T89
C                                                                       6/13T89
      FAC37 = DBLE(LGS(37))*LOG(10.0D0)                                 6/13T89
      EXFAC = EXP(FAC37)                                                6/13T89
C                                                                       6/13T89
C     LOOP OVER TEMPERATURES
C
      NFRR = NF(1) + NF(2)
      NFRP = NF(3) + NF(4) 
      NTOT = NFRR + NFRP
      DO 200 ITEMP = 1, NTEMP
         WRITE (FU6,1650) TEMP(ITEMP)
C
C        FIND DESIRED LIMITS ON S
C
         IF (NGSPEC.EQ.0) THEN
            ISTART = 1
            ISTOP = LSAVE
         ELSEIF (ABS(NGSPEC).EQ.1) THEN
            IF (NGSPEC.EQ.1) THEN
               SLMG = SLMA(ITEMP)
               SLPG = SLPA(ITEMP)
            ENDIF
            J = 1
C
C           DO WHILE (SLMG.GT.SSUBI(J))
C
   90       CONTINUE
            IF (SLMG.LE.SSUBI(J)) GO TO 100
            J = J+1
C
C           END DO
C
            GO TO 90
  100       CONTINUE
            ISTART = J
            J = 1
C
C           DO WHILE (SLPG.GT.SSUBI(J))
C
  110       CONTINUE
            IF (SLPG.LE.SSUBI(J)) GO TO 120
            J = J+1
C
C           END DO
C
            GO TO 110
  120       CONTINUE
            ISTOP = J
         ENDIF
         ISTRTO = ISTART
         ISTOPO = ISTOP
C
         T = TEMP(ITEMP)
         BKT = BK*T
         RT = RCONST*T
C
C    REACTANT AND PRODUCT TRANSLATIONAL, ROTATIONAL, AND ELECTRONIC
C        PARTITION FUNCTIONS                              
C       REACTANTS
C
         IF (LGS(6).LT.3 ) THEN
C
C    2 REACTANTS
C
C  .....................    TRANSLATION
C
            IF (ICODE(1) .GT. 0 .OR. ICODE(2) .GT. 0) THEN
               QTR = ((REDMF*BKT)/(2.0D0*PI))**1.5D0
               QTRCC = QTR*CONK0
            ELSE                                                        11/20T87
               QTR = 1.0D0                                                 ..
               QTRCC = 1.0D0                                               ..
            ENDIF                                                       11/20T87
C
C .......................    ROTATION
C
            IF (ICODE(1) .GT. 0 .AND. ICODE(2) .GT. 0) THEN
               QRR = RPART(FMOM(1),BKT,ICODE(1))*RPART(FMOM(2),BKT,
     *                     ICODE(2))
            ELSE IF (ICODE(1) .GT. 0 .AND. LGS(34) .NE. 0) THEN         11/20T87
               QRR = RPART(FMOM(1),BKT,ICODE(1))                           ..
            ELSE                                                           ..
               QRR = 1.0D0                                                 ..
            ENDIF                                                       11/20T87
C
C ........................   ELECTRONIC
C
            QER = EPART(1,BKT)*EPART(2,BKT)                             1501/92VM
C
         ELSE
C
C    1 REACTANT    2 CASES: 1/ GAS REACTANT
C                           2/ GAS REACTANT +  SURFACE
C
C .......................   TRANSLATION
C
            IF (ICODE(1) .GT. 0 .AND. LGS(34) .NE. 0) THEN               5/25T89
               QTR = ((REDMF*BKT)/(2.0D0*PI))**1.5D0                    11/20T87
               QTRCC = QTR*CONK0                                           ..
            ELSE                                                           ..
               QTR = 1.0D0                                                 ..
               QTRCC = 1.0D0                                               ..
            ENDIF                                                       11/20T87
C
C .......................    ROTATION
C
            IF ( ICODE(1) .GT. 0) THEN
               QRR = RPART(FMOM(1),BKT,ICODE(1))
            ELSE                                                        11/20T87
               QRR = 1.0D0                                                 ..
            ENDIF                                                       11/20T87
C
C   .....................   ELECTRONIC
C
               QER = EPART(1,BKT)                                       150192VM
         ENDIF
C
C       PRODUCTS
C
         IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
C
C       2  PRODUCTS
C
C   .................    TRANSLATION
C
            IF (ICODE(3) .GT. 0 .OR. ICODE(4) .GT. 0) THEN              11/20T87
               QTP = ((REDMR*BKT)/(2.0D0*PI))**1.5D0                       ..
               QTPCC = QTP*CONK0                                           ..
            ELSE                                                           ..
               QTP = 1.0D0                                                 ..
               QTPCC = 1.0D0                                               ..
            ENDIF                                                       11/20T87
C
C  ....................   ROTATION
C
            IF ( ICODE(3) .GT. 0 .AND. ICODE(4) .GT. 0) THEN
               QRP = RPART(FMOM(3),BKT,ICODE(3))*RPART(FMOM(4),BKT,
     *                    ICODE(4))
            ELSE IF ( LGS(34) .NE. 0 .AND. ICODE(3) .GT. 0) THEN        11/20T87
               QRP = RPART(FMOM(3),BKT,ICODE(3))                           ..
            ELSE                                                           ..
               QRP = 1.0D0                                                 ..
            ENDIF                                                       11/20T87
C
C  ....................   ELECTRONIC
C
            QEP = EPART(3,BKT)*EPART(4,BKT)                             150192VM
         ELSE
C
C        1 PRODUCT
C   .....................  TRANSLATION
C
            IF (ICODE(3) .GT. 0 .AND. LGS(34) .NE. 0) THEN               5/25T89
               QTP = ((REDMR*BKT)/(2.0D0*PI))**1.5D0                    11/20T87
               QTPCC = QTP*CONK0                                           ..
            ELSE                                                           ..
               QTP = 1.0D0                                                 ..
               QTPCC = 1.0D0                                               ..
            ENDIF                                                       11/20T87
C
C   ...................... ROTATION
C
            IF (ICODE(3) .GT. 0) THEN
               QRP = RPART(FMOM(3),BKT,ICODE(3))
            ELSE                                                        11/20T87
               QRP = 1.0D0                                                 ..
            ENDIF                                                       11/20T87
C
C   .....................  ELECTRONIC
C
            QEP = EPART(3,BKT)                                          150192VM
         ENDIF
C
C    REACTANT AND PRODUCT VIBRATIONAL PFS
C
         IOP = 1
         IBEG = 1
C*         PROD = 1.0D0                                                 6/13T89
         SUM = 0.0D0                                                    6/13T89
         AFLAG = '   '
         IF (LGS(5).GE.21) AFLAG = 'SET'                                6/30YL91
         DO 140 IY = 1, 2 
            IF (NF(IY).NE.0.AND.LGS2(15).EQ.0) THEN                     1106YL92
               IENDR = NF(IY)+IBEG-1
               J = 1
               DO 130 IX = IBEG, IENDR
c                  IF (AFLAG.EQ.'SET') LGS(5) = MODER(IY,J)             0317Yc99
                  LGS(5) = MODER(IY,J)                                  0317Yc99
                  L0 = LRP(IX)
                  IKBM = IX
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IF (IY .EQ. 1) THEN
                        IMHR = NF(1)+1-IX
                     ELSE
                        IMHR = NF(1)+NF(2)+1-IX
                     ENDIF
                     IF (IY.EQ.2) THEN
                       IXI = IX - NF(1)
                     ELSE
                       IXI = IX
                     ENDIF
                     QVSV(IX)=HRPART(0.d0,WER(IX),TORMI(IY,IXI,1),      1020BE06
     *                               BKT,IY,IXI,IMHR)                   1020BE06
                  ELSE                                                     ..
                     QVSV(IX)=VPART(WER(IX),XER(IX),BKT,DEMIN,IOP,         ..
     *                              Y00R(IX))                              ..
                  ENDIF                                                 6/30YL91
                  J = J+1
C*                  PROD = PROD*QVSV(IX)                                6/13T89
                    SUM = SUM + LOG(QVSV(IX))                           6/13T89
  130          CONTINUE
C 
C               IF (LGS2(12) .NE. 0) THEN                                0719WH94
C Commented by Lucas; it should be changed to the following line
C so that under HO approximation, vib P.F. of R and P shold not be
C changed
               IF ((LGS2(12).NE.0).AND.(LGS2(12).NE.1)) THEN            2017Lucas
                  IM = NF(1)+1-IWR
                  SUM = SUM - LOG(QVSV(1)*QVSV(IM))                     0719WH94
                  CALL QTQVIB(BKT,QVIBF,QVIBR)                          0719WH94
                  QVSV(1)  = QVIBF                                      0719WH94
                  QVSV(IM) = QVIBR                                      0719WH94
                  IF (IM .EQ. 1) QVSV(IM) = QVIBF                       0719WH94
                  SUM = SUM + LOG(QVIBF*QVIBR)                          0719WH94
               ENDIF
C
            ELSEIF (NF(IY).NE.0.AND.LGS2(15).NE.0) THEN                 1106YL92
               NMOD = NF(IY)                                                ..
               EGRNDT = EGRNDR(IY)                                          ..
               DUMMY = PTQVIB(NMOD,N3TM,EGRNDT,EFNDTR(IBEG),                ..
     *                        WER(IBEG),BKT)                                ..
               PROD = PROD * DUMMY                                          ..
               SUM = SUM + LOG(DUMMY)                                       ..
            ENDIF                                                       1106YL92
            IBEG = IBEG+NF(IY)
            IOP = IOP+1
  140    CONTINUE
c
c       solvent coordinate contribution only harmonic
c
         if (ibathm.eq.1) then                                          0317Yc99
           FRSOL = (PI/(4*FRICT))                                       0317Yc99
           VPSOL = VPART(FRSOL,0.d0,BKT,DEMIN,IOP,0.d0)                 0824JC00
           SUM = SUM + LOG(VPSOL)                                       0317Yc99
         endif                                                          0317YC99
         SUMRE = SUM                                                    0423TA02
         QVR = EXP(SUM+FAC37)                                           6/13T89
         IBEGP = IBEG
         SUM = 0.0D0                                                    6/13T89
         EMAX = DEMIN-EPRD
         IVP = 4
         IF (LGS(6).EQ.2) IVP = 3
         DO 160 IY = 3, IVP
            IF (NF(IY).NE.0.AND.LGS2(15).EQ.0) THEN                     1106YL92
               IENDP = NF(IY)+IBEG-1
               J = 1
               DO 150 IX = IBEG, IENDP
                  LGS(5) = MODER(IY,J)                                  0317Yc99
                  L0 = LRP(IX)
                  IKBM = IX
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IF (IY .EQ. 3) THEN                                0615WH94
                        IMHR = NF(1)+NF(2)+NF(3)+1-IX                   0615WH94
                     ELSE                                               0615WH94
                        IMHR = NF(1)+NF(2)+NF(3)+NF(4)+1-IX             0615WH94
                     ENDIF     
                     IF (IY.EQ.4) THEN
                       IXI = IX - NF(1) - NF(2) - NF(3)
                     ELSE
                       IXI = IX - NF(1) - NF(2)
                     ENDIF
                     QVSV(IX)=HRPART(0.d0,WER(IX),TORMI(IY,IXI,1),      1020BE06
     *                               BKT,IY,IXI,IMHR)                   1020BE06
                  ELSE                                                     ..
                     QVSV(IX)=VPART(WER(IX),XER(IX),BKT,EMAX,IOP,          ..
     *                              Y00R(IX))                              ..
                  ENDIF                                                 6/30YL91
                  J = J+1
C*                  PROD = PROD*QVSV(IX)                                 6/13T89
                  SUM = SUM + LOG(QVSV(IX))                              6/13T89
  150          CONTINUE
            ELSEIF (NF(IY).NE.0.AND.LGS2(15).NE.0) THEN                 1106YL92
               EGRNDT = EGRNDR(IY)                                          ..
               NMOD = NF(IY)                                                ..
               DUMMY = PTQVIB(NMOD,N3TM,EGRNDT,EFNDTR(IBEG),                ..
     *                        WER(IBEG),BKT)                                ..
               PROD = PROD * DUMMY                                          ..
               SUM = SUM + LOG(DUMMY)                                       ..
            ENDIF                                                       1106YL92
            IBEG = IBEG+NF(IY)
            IOP = IOP+1
  160    CONTINUE
c
c       solvent coordinate contribution only harmonic
c
         if (ibathm.eq.1) then                                          0317Yc99
           FRSOL = (PI/(4*FRICT))                                       0317Yc99
           VPSOL = VPART(FRSOL,0.d0,BKT,DEMIN,IOP,0.d0)                 0824JC00
           SUM = SUM + LOG(VPSOL)                                       0317Yc99
         endif                                                          0317Yc99
         SUMPR = SUM                                                    0423TA02
         QVP = EXP(SUM+FAC37)                                            6/13T89
         QINTR = QVR*QRR*QER
C
C   PHIF,PHIR IN A.U. OR UNITLESS.  PHIFCC,PHIRCC IN CGS OR UNITLESS.
C
         PHIF = QTR*QINTR
         PHIFCC = QTRCC*QINTR
         QINTR = QVP*QRP*QEP
         PHIR = QTP*QINTR
         PHIRCC = QTPCC*QINTR
C        PREFAC = CNVRT*SIGMAF*BKT/(2.0D0*PI)
C   The SIGMAF should be put in the Gibbs free energy although either way 
C   is ok numerically
         PREFAC = CNVRT*BKT/(2.0D0*PI)

         ARG1 = EPRD/BKT
         ARGNEW = ARG1+2.302585093D0*DBLE(LGS(17))
         ARGABS = ABS(ARGNEW)
         IF (ARGABS.GT.75.2D0) THEN                                      17/8T88
            WRITE (FU6,3050) T,ARG1,ARGNEW,PHIF,PHIR
            LGS(16) = 1
         ELSE
            IF (LGS2(16).EQ.0) THEN                                     0423TA02
               REVFAC = SIGMAR*PHIFCC*EXP(ARGNEW)/(SIGMAF*PHIRCC)
            ELSEIF (LGS2(16).EQ.1) THEN                                 0423TA02
               REVFAC = (SIGMAR*EXP(ARGNEW)*QTRCC*QRR*QER/              0423TA02
     *                  (SIGMAF*QTPCC*QRP*QEP))*EXP(SUMRE-SUMPR)        0423TA02
            ENDIF                                                       0423TA02
         ENDIF
         IF (LGS(7).GE.3) THEN
C
C     EXTRA OUTPUT OF INDIVIDUAL PARTITION FUNCTIONS
C
            ISTOP = IENDP-IBEGP+1
            IF (IENDR.GT.ISTOP) ISTOP = IENDR
            WRITE (FU6,1700)
            WRITE (FU6,1750) QER,QTR,QRR,QVR,PHIF,QTRCC,PHIFCC
            WRITE (FU6,1800) QEP,QTP,QRP,QVP,PHIR,QTPCC,PHIRCC
            WRITE (FU6,1810)
            WRITE (FU6,1815)
C
C     Write out the Internal Free Energy of Reactant if Unimolecular
C
            IF ((LGS(6).EQ.3.OR.LGS(6).EQ.4) .AND. ICODE(1).GT.0) THEN  1212PF99
               IF (LGS2(16).EQ.0) THEN                                  0423TA02
                  FRENR = -1*RCONST*T*dlog(QER*QRR*QVR)                 1212PF99
               ELSEIF (LGS2(16).EQ.1) THEN                              0423TA02
                  FRENR = -1*RCONST*T*(dlog(QER*QRR)+SUMRE)             0423TA02
               ENDIF                                                    0423TA02
               WRITE (FU6,2051) FRENR                                   1212PF99
               WRITE (FU6,2052)                                         1213PF99
            ENDIF                                                       1212PF99
C
            WRITE (FU6,1850)
C
            IF (NF(1).GE.1) WRITE (FU6,1910) (QVSV(I),I=NF(1),1,-1)     0615WH94
            IF (NF(2).GE.1) WRITE (FU6,1920) (QVSV(I),I=NFRR,NF(1)+1,-1)..
            IF (NF(3).GE.1) WRITE (FU6,1930) (QVSV(I),I=NFRR+NF(3),     ..
     *                                        NFRR+1,-1)                ..
            IF (NF(4).GE.1) WRITE (FU6,1940) (QVSV(I),I=NTOT,           ..
     *                                        NFRR+NF(3)+1,-1)          0615WH94
            WRITE (FU6,2050)
C
C
            IF (LGS(20).EQ.0) WRITE (FU6,3100)
        ENDIF
C
C     COMPUTE GT ELECTRONIC PARTITION FUNCTION AT THIS TEMP
C
         QE = EPART(5,BKT)                                              0115VM92
C
C     LOOP OVER S
C
         ISTART = ISTRTO
         ISTOP = ISTOPO
         MARRMT = 1                                                     6/30YL91
         DO 180 IS = ISTART, ISTOP
C
C           IF (LGS(7) .EQ. 4) WRITE (FU6,3300) SSUBI(IS),QE            0615WH94
            IF (LGS(7) .EQ. 4) THEN                                     0405JZ07
              IF(IUNIT6.EQ.1) WRITE (FU6,3300) SSUBI(IS)/GUFAC6,QE        ..    
              IF(IUNIT6.EQ.0) WRITE (FU6,3302) SSUBI(IS)/GUFAC6,QE        ..    
            ENDIF                                                       0405JZ07
C
C     CALCULATE GT PARTITION FUNCTION
C
            IF (LGS(34) .NE. 0) THEN                                    11/20T87
               QR = 1.0D0                                                  ..
            ELSE                                                        11/20T87
               IF (FMITS(IS).EQ.0.0D0) THEN
                  QR = 0.0D0
               ELSE
                  QR = RPART(FMITS(IS),BKT,ICODE(5))
               ENDIF
            ENDIF
            IF ( QR .NE. 0.0D0) THEN
               VSX = VCLAS(IS)
               IOP = 0
               IF (IS.EQ.NSHLF) IOP = 5
               IF (SOB.GT.SOE) THEN
                  IF (SSUBI(IS).GE.SOE.AND.SSUBI(IS).LE.SOB) IOP = 5
               ELSE
                  IF (SSUBI(IS).GE.SOB.AND.SSUBI(IS).LE.SOE) IOP = 5
               ENDIF
C*               PROD = 1.0D0                                           6/13T89
               SUM = 0.0D0                                              6/13T89
               EMAX = DEMIN-VSX
C no more LGS(19)
               IF (LGS2(15).EQ.0) THEN                                  0610YC96
C              IF (LGS(19).EQ.0.AND.LGS2(15).EQ.0) THEN                 1106YL92
                  JSWITC = 1
                  IF (SSUBI(IS).GE.SWITC) JSWITC = 2
                  IF (AFLAG.EQ.'SET'.AND.NARR.GT.1) THEN                6/30YL91
                     M = MARRMT                                            ..
                     N = NARR -1                                           ..
                     DO  165 J = M, N                                      ..
                         IF (SSUBI(IS).GE.SRARR(J)) MARRMT=MARRMT+1        ..
165                  CONTINUE                                              ..
                  ENDIF                                                    ..
                  DO 168 I = 1, N3M7                                       ..
                     MODE(I) = MODETS(MARRMT,I)                            ..
168               CONTINUE                                              6/30YL91
                  DO 170 I = 1, N3M7
                     IF (AFLAG.EQ.'SET') LGS(5) = MODE(I)
                     L0 = LN3(JSWITC,I)
                     IKBS = IS
                     IKBM = I
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IMHR = NF(5) + 1 - I                               0615WH94
                     QVSV(I)=HRPART(SSUBI(I),WETS(I,IS),FMIHTS(I,IS),
     *                    BKT,5,I,IMHR)
                  ELSE                                                     ..
                     QVSV(I)=VPART(WETS(I,IS),XETS(I,IS),BKT,EMAX,IOP,     ..
     *                              Y0TS(I,IS))                            ..
                  ENDIF                                                 6/30YL91
C*                     PROD = PROD*QVSV(I)                               6/13T89
                       SUM = SUM + LOG(QVSV(I))                          6/13T89
  170             CONTINUE
C*                  QV = PROD
                  QV = EXP(SUM+FAC37)
C  no more LGS(19)
               ELSEIF (LGS2(15).NE.0) THEN
C              ELSEIF (LGS(19).EQ.0.AND.LGS2(15).NE.0) THEN             1106YL92
                  EGRNDT = EGRND(IS)                                        ..
                  DO 175 I = 1, N3M7                                        ..
                     EFNDTP(I) = EFNDT(I,IS)                                ..
175               CONTINUE                                                  .. 
                  DUMMY = PTQVIB(N3M7,N3TM,EGRNDT,EFNDTP,                   ..
     *                           WETS(1,IS),BKT)                            ..
                  PROD = PROD * DUMMY                                       ..
                  SUM = SUM + LOG(DUMMY)                                    ..
                  QV = EXP(SUM+FAC37)                                       ..
               END IF
C
               SUMGT = SUM                                              0423TA02

C  Add SS-T torsional correction
C
               IF(LSST.EQ.1) THEN                                       0517JZ12
C
                 FTOR = 1D0
                 DO I = 1, NTOR
                    EXPARG = TORBH(I,IS)/BKT/2D0
                    FTOR = FTOR*DSQRT(TPI/BKT)/DMTOR(I)
                    FTOR = FTOR*exp(-EXPARG)*BES0(EXPARG)
                 ENDDO
C                write(6,*) 'DBW'
                 DO K = 1, N3M7
                    IF(WETS(K,IS).GT.0.1/AUTOCM) THEN
                      FTOR=FTOR*WETS(K,IS)
                    ENDIF
                    IF(DBW(K,IS).GT.0.1/AUTOCM) THEN
                      FTOR = FTOR/DBW(K,IS)
C            IF(ITEMP.eq.1)write(6,*)DBW(K,IS)*AUTOCM,SSUBI(IS)/GUFAC6
                    ENDIF
                 ENDDO
                 FTOR = FTOR*DSQRT(DETDS(IS))
C                write(6,*) 'FTOR ', FTOR
                 QV = QV*FTOR
C              IF(ITEMP.eq.19) write(6,*) 'FTOR', FTOR
               ENDIF
C
C  END of SS-T correction
C
               QGT = QR*QV*QE
C
C     CALCULATE QIGT AND DELIG
C
c              IF (LGS(20).NE.0) THEN
c                 IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20                6/30YL91
c                 CALL STAUV (IS,BKT,VAGMAX,SUMTV)
c                 VAGMU = MAX(VAGMAX,VAR,VAP)                           0223JZ12
c                 WRITE(6, *)"ICVT Threshold E: ", VAGMU*CKCAL
c                 CALL STAUV (IS,BKT,VAGMU,SUMTV)                       0223JZ12
c                 QIGT = QE*(QR*QV-SUMTV*EXFAC)                          6/13T89
c                 IF (QIGT/QGT .lt. 1.e-4) THEN                         9/25BCG00
c                   WRITE(FU6,9915) SSUBI(IS)/GUFAC6,QIGT,QE,QR,QV,
c    &                              SUMTV,EXFAC                         0405JZ07
c9915              FORMAT(' S=',F8.3,' QIGT=',1pE14.4,' QE=',E14.4,     9/25BCG00
c    &                ' QR=',E14.4,' QV=',E14.4,' SUMTV=',E14.4,        9/25BCG00
c    &                ' EXFAC=',E14.4)                                  9/25BCG00
c                   DELIG(IS) = -9999.9                                 9/25BCG00
c                 ELSE                                                  9/25BCG00
c                    DELIG(IS) = RT*((VSX/BKT)-LOG(QIGT)+LOG(PHIFCC))   9/25BCG00
c                 ENDIF                                                 07/95KAN
c              ELSE
c                 QIGT = QGT
c              ENDIF
C
C     CALCULATE DELTA G AND SAVE
C      DELG IS FOR CVT.
C The conversion factor C0 should be 1 for A -> TS
C (free energy of activation is independent of the change of
C standard-state pressure/concentration)
               C0 = 1.d0
C for A + B -> TS, re-assign C0 
C      7.24270614D+21 = P/k in unit of cc^-1 K  (P is 1 bar)
C      C0 = P/kT gas-phase concentration under standard-state conditions 
               IF (LGS(6).LT.3 ) C0 = 7.242971565D+21/T
               IF (LGS2(16).EQ.0) THEN                                  0423TA02
C                 DELG(IS) = RT*((VSX/BKT)-LOG(QGT) + LOG(PHIFCC))      11/20T87
                  DELG(IS) = RT*((VSX/BKT)-LOG(QGT) + LOG(PHIFCC)
     *                       -LOG(C0) - LOG(SIGMAF))   
               ELSEIF (LGS2(16).EQ.1) THEN                              0423TA02
C                 DELG(IS) = RT*((VSX/BKT) - LOG(QR*QE) - SUMGT         0423TA02
C    *                       + LOG(QTRCC*QRR*QER) + SUMRE)              0423TA02
                  DELG(IS) = RT*((VSX/BKT) - LOG(QR*QE) - SUMGT        
     *            + LOG(QTRCC*QRR*QER)-LOG(C0)-LOG(SIGMAF)+SUMRE) 
               ENDIF                                                    0423TA02
C
               IF (LGS(7).GE.4) THEN                                    12/30S87
C
C     EXTRA OUTPUT OF INDIVIDUAL PARTITION FUNCTIONS
C
                  WRITE (FU6,3310)
c                 WRITE (FU6,3350) QR,QV,QGT,QIGT
                  WRITE (FU6,3350) QR,QV,QGT
                  WRITE (FU6,3360) (QVSV(I),I=N3M7,1,-1)                0615WH94
C
               ENDIF
            ENDIF
  180    CONTINUE
C
C     OPTIONALLY PRINT OUT DELTA G(S)
C
         IF (LGS(7).GE.2) THEN
            IF(IUNIT6.EQ.1) WRITE (FU6,3450)                            0405JZ07
            IF(IUNIT6.EQ.0) WRITE (FU6,3460)                            0405JZ07
            WRITE (FU6,3500) (SSUBI(I)/GUFAC6,DELG(I),I=ISTART,ISTOP)   0405JZ07
         ENDIF
c        IF (LGS(20).EQ.2) THEN
c           IF(IUNIT6.EQ.1) WRITE (FU6,3550) T                          0405JZ07
c           IF(IUNIT6.EQ.0) WRITE (FU6,3560) T                          0405JZ07
c           WRITE (FU6,3500) (SSUBI(I)/GUFAC6,DELIG(I),I=ISTART,ISTOP)  0405JZ07
c        ENDIF
C
C     COMPUTE CONVENTIONAL TST RATES AND STORE FOR SUMMARY
C
C        DELG(:) = DELG(:)+LOG(C0)*RT
         IF (LGS(1).NE.0) THEN
            CONF(ITEMP) = PREFAC*EXP(-DELG(NSHLF)/RT)/C0
            CONR(ITEMP) = CONF(ITEMP)*REVFAC
         ELSE
            CONF(ITEMP) = 1.0D0
            CONR(ITEMP) = 1.0D0
         ENDIF
          IF (ivic.eq.2.and.LZOC) THEN                                  0824YC98
           DO I = 1, LSAVE                                              0203YC98
             IF (SSUBI(I).LE.SSPMAX) THEN                               0203YC98
                IDEG = I                                                0203YC98
             ELSE
                GOTO 2059
             ENDIF                                                      0203YC98
           ENDDO                                                        0203YC98
2059       CONTINUE
           DELGSP = DELG(IDEG)+ (SSPMAX-SSUBI(IDEG))*
     *                     (DELG(IDEG+1)-DELG(IDEG))/                   0203YC98
     *                     (SSUBI(IDEG+1)-SSUBI(IDEG))                  0203YC98
C          TSTCOR = PREFAC*EXP(-DELGSP/RT)                              0203YC98
           TSTCOR = PREFAC*EXP(-DELGSP/RT)/C0
C          WRITE (FU6,2060) SSPMAX,DELGSP                               0203YC98
           IF(IUNIT6.EQ.1) WRITE (FU6,2060) SSPMAX/GUFAC6,DELGSP        0405JZ07
           IF(IUNIT6.EQ.0) WRITE (FU6,2062) SSPMAX/GUFAC6,DELGSP        0405JZ07
           WRITE (FU6,2070) TSTCOR                                      0203YC98
C
C   assign TST-COR to TST
C
         CONF(ITEMP) = TSTCOR                                           0606YC98
         CONR(ITEMP) = CONF(ITEMP)*REVFAC                               0606YC98
C
2060     FORMAT (/,1X,'Max of VMEP occurs at s = s*VMEP =',F8.4,        0203YC98
     *           ' (bohr)', ' with DELG =',F8.4)                        0203YC98
2062     FORMAT (/,1X,'Max of VMEP occurs at s = s*VMEP =',F8.4,        0405JZ07
     *           ' (angstrom)', ' with DELG =',F8.4)
2070     FORMAT (1X,'Generalized TST rate for s = s*VMEP is ',          0203YC98
     *                 1PE10.3,' (cm**3/molecule-sec)')                 0203YC98
         ENDIF                                                          0203YC98
C
C    Compute CUS result
C
         IF (NFCUS.EQ.1) THEN                                           0929YC97
           NDG = ISTOP - ISTART + 1                                     0929YC97
           DO I = 1, NDG                                                0929YC97
             XDG(I) = SSUBI(I+ISTART-1)                                 0929YC97
             YDG(I) = DELG(I+ISTART-1)                                  0929YC97
           ENDDO                                                        0929YC97
C          CALL CUSSPL (NCUSMX,NDG,XDG,YDG,RT,PREFAC,CUSK)              0929YC97
           CALL CUSSPL (NCUSMX,NDG,XDG,YDG,RT,PREFAC/C0,CUSK)     
           CUSTF(ITEMP) = CUSK                                          0929YC97
           CUSTR(ITEMP) = REVFAC*CUSTF(ITEMP)                           0929YC97
         ENDIF                                                          0929YC97
C
C     FIND MAX DELG(S) WITH THREE AND FIVE POINT FITS AND COMPUTE
C     CORRESPONDING CVT RATES
C
         CALL FITMAX (0,LSAVE,S3,G3,S5,G5,V3X,V5,ISTART,ISTOP)          7/14YL92
         SCVT(ITEMP) = S5
         SFIT5 = S5
         VFIT5 = V5
         IF (LGS(9).NE.0) THEN
C
C     FIND VA AT S*CVT5 FROM SPLINE FIT
C
            CALL SPL1B2 (NSPL,SSUBI,ASPL,BSPL,CSPL,DSPL,S5,TAB,0)
            V5 = TAB(1)
         ENDIF
C
C
C     COMPUTE CVT/CAG CORRECTION AND CVT RATES
C
         IF (IFRFAC.EQ.0) THEN                                          0814JC00
          KAPCVT(ITEMP) = EXP((V5-VAG)/BKT)
         ENDIF                                                          0814JC00
         CVT3F = PREFAC*EXP(-G3/RT)/C0
         CVTF(ITEMP) = PREFAC*EXP(-G5/RT)/C0
         CVT3R = REVFAC*CVT3F                                           09/95KAN
         CVTR(ITEMP) = REVFAC*CVTF(ITEMP)
C
C     OUTPUT CVT RATES FROM 3 AND 5 POINT FITS
C
         IF (LGS(6).LE.2 .OR. (LGS(34).NE.0 .AND. ICODE(1).GT.0)) THEN
C            WRITE (FU6,2100)
             IF(IUNIT6.EQ.1) WRITE (FU6,2100)                           0405JZ07
             IF(IUNIT6.EQ.0) WRITE (FU6,2110)                           0405JZ07
         ELSE IF(LGS(6).GT.2 .OR. (ICODE(1).LT.0.AND.ICODE(2).LT.0))THEN
C            WRITE (FU6,2125)
             IF(IUNIT6.EQ.1) WRITE (FU6,2125)                           0405JZ07
             IF(IUNIT6.EQ.0) WRITE (FU6,2135)                           0405JZ07
         ENDIF
         WRITE (FU6,2150) S3/GUFAC6,V3X*CKCAL,G3,CVT3F                  0405JZ07
         WRITE (FU6,2200) SFIT5/GUFAC6,VFIT5*CKCAL,G5,CVTF(ITEMP)       0405JZ07
         IF (LGS(20).NE.0) THEN
C
C     FIND MAX DELIG(S) WITH THREE AND FIVE POINT FITS AND COMPUTE
C     CORRESPONDING ICVT RATES
C
            CALL FITMAX(-1,LSAVE,S3,G3,S5,G5,V3X,V5,                    1/3RS96
     *                   ISTART,ISTOP)
            SICVT(ITEMP) = S5
            SFIT5 = S5
            VFIT5 = V5
C
C     FIND VA AT S*CVT5 FROM SPLINE FIT
C
            CALL SPL1B2 (NSPL,SSUBI,ASPL,BSPL,CSPL,DSPL,S5,TAB,0)
            V5 = TAB(1)
C
C     COMPUTE ICVT RATES
C
c           CIVT3F = PREFAC*EXP(-G3/RT)
c           CIVTF(ITEMP) = PREFAC*EXP(-G5/RT)
c           CIVT3R = REVFAC*CIVT3F                                      09/95KAN
c           CIVTR(ITEMP) = REVFAC*CIVTF(ITEMP)
C
C     OUTPUT ICVT RATES FROM 3 AND 5 POINT FITS
C
c        IF (LGS(6).LE.2 .OR. (LGS(34).NE.0 .AND. ICODE(1).GT.0)) THEN
c            IF(IUNIT6.EQ.1) WRITE (FU6,2250)                           0405JZ07
c            IF(IUNIT6.EQ.0) WRITE (FU6,2252)                           0405JZ07
c        ELSE IF(LGS(6).GT.2 .OR. (ICODE(1).LT.0.AND.ICODE(2).LT.0))THEN
c            IF(IUNIT6.EQ.1) WRITE (FU6,2260)                           0405JZ07
c            IF(IUNIT6.EQ.0) WRITE (FU6,2262)                           0405JZ07
c        ENDIF
c           WRITE (FU6,2150) S3/GUFAC6,V3X*CKCAL,G3,CIVT3F              0405JZ07
c           WRITE (FU6,2200) SFIT5/GUFAC6,VFIT5*CKCAL,G5,CIVTF(ITEMP)   0405JZ07
c        ELSE
c           CIVTF(ITEMP) = 1.0D0
c           CIVTR(ITEMP) = 1.0D0
         ENDIF
C
         IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20                         6/30YL91
         IF (LGS(21).EQ.0) THEN
            CMUVTF(ITEMP) = 1.0D0
            CMUVTR(ITEMP) = 1.0D0
            CUSVTF(ITEMP) = 1.0D0
            CUSVTR(ITEMP) = 1.0D0
         ELSE
C
C     COMPUTE MUVT RATES
C
            IF (LGS(21).EQ.1) THEN
               LUSC = 0
               WRITE (FU6,3600) T,IFTMV1,IFTMV2                         01/13B92
            ELSE
               LUSC = 1
               WRITE (FU6,3700) T,IFTMV1                                01/13B92
            ENDIF
            TESTX = (DEMIN-VAGMU)/BKT
            SUMMU = 0.0D0
            SUMUS = 0.0D0
            IPRIND = 1                                                  01/13B92
            IPRINT = IPRMVT(IPRIND)                                     01/13B92
            CVTS = SCVT(ITEMP)
            DO 190 IMU = 1, NITER  
               IF (ITEMP.EQ.1 .AND. IMU.EQ.IPRINT) THEN                 01/13B92
                  DO 19098 IS=ISMMVT,ISPMVT                             01/13B92
                     XNMVT(IPRIND,IS) = 1.D+30                          01/13B92
19098             CONTINUE                                              01/13B92
                  EMVT(IPRIND) = (TMUVT(IMU)*BKT+VAGMU)*CKCAL           01/13B92
               END IF                                                   01/13B92
               IF (TMUVT(IMU).LE.TESTX) THEN
                  EMU = TMUVT(IMU)*BKT+VAGMU
                  CALL VTMUSN (EMU,SVTMU3,VTMUN3,SVTMU5,VTMUN5,CVTS,
     *               GTNCVT,SMIN15,SMAX15,SMIN25,VMIN15,VMAX15,VMIN25,
     *               VTUSN,LUSC,GTNS,NGR,EGRID,ISMMVT,ISPMVT,IFTMV1,
     *               IFTMV2,VAGMU)
                  IF (LUSC.EQ.0) THEN
                     WRITE (FU6,3610) EMU,EMU*CKCAL
                     WRITE (FU6,3620) SVTMU3/GUFAC6,SVTMU5/GUFAC6,
     *                         SCVT(ITEMP)/GUFAC6,VTMUN3,VTMUN5,GTNCVT 
                  ELSE
                     WRITE (FU6,3610) EMU,EMU*CKCAL
                     WRITE (FU6,3720) VTMUN5,VTUSN,
     *               SMIN15/GUFAC6,SMIN25/GUFAC6,SMAX15/GUFAC6,
     *               VMIN15,VMIN25,VMAX15       
C                    WRITE (FU6,3750) EMU,EMU*CKCAL,VTMUN5,VTUSN,SMIN15,
C    *                  SMAX15,SMIN25,VMIN15,VMIN25,VMAX15
                     SUMUS = SUMUS+WMUVT(IMU)*VTUSN
                  ENDIF
                  SUMMU = SUMMU+WMUVT(IMU)*VTMUN5
                  IF (ITEMP.EQ.1 .AND. IMU.EQ.IPRINT) THEN              01/13B92
                     DO 19099 IS=ISMMVT,ISPMVT                          01/13B92
                        XNMVT(IPRIND,IS) = GTN(IS)                      01/13B92
19099                CONTINUE                                           01/13B92
                     IPRIND = IPRIND + 1                                01/13B92
                     IPRINT = 0                                         01/13B92
                     IF (IPRIND.LE.NPRMVT) IPRINT = IPRMVT(IPRIND)      01/13B92
                  END IF                                                01/13B92
               ENDIF
190         CONTINUE
C
            IF (ITEMP.EQ.1 .AND. NPRMVT .NE. 0) THEN                    01/13B92
               DO 191 IS = ISMMVT,ISPMVT                                01/13B92
                  WRITE (FU6,4000) SSUBI(IS)                            0616WH94
                  WRITE (FU6,4010) (EMVT(IPR),XNMVT(IPR,IS),            0616WH94
     *                             IPR=1,NPRMVT)                        0616WH94
191            CONTINUE                                                 01/13B92
            END IF                                                      01/13B92
C
            VTMUI = CNVRT*SIGMAF*BKT*SUMMU*EXP(-VAGMU/BKT)
            CMUVTF(ITEMP) = QE*VTMUI/(2.0D0*PI*PHIFCC)
            CMUVTR(ITEMP) = REVFAC*CMUVTF(ITEMP)
            IF (LUSC.EQ.0) THEN
               CUSVTF(ITEMP) = 1.0D0
               CUSVTR(ITEMP) = 1.0D0
            ELSE
               VTUSI = CNVRT*SIGMAF*BKT*SUMUS*EXP(-VAGMU/BKT)
               CUSVTF(ITEMP) = QE*VTUSI/(2.0D0*PI*PHIFCC)
               CUSVTR(ITEMP) = REVFAC*CUSVTF(ITEMP)
            ENDIF
         ENDIF
  200 CONTINUE
C
C     COMPUTE FINAL RATES AND OTHER INFO AND OUTPUT
C
      CALL FINOUT(SMAXX)
      RETURN
C
 1000 FORMAT(/1X,25(1H*),' Reaction rate calculations ',25(1H*))        0614WH94
 1050 FORMAT(/3X,'NGSPEC = 0  no special limits on free energy calc')
 1100 FORMAT(/3X,'NGSPEC = -1  free energy computed for',F10.5,
     *' < s < ',F10.5)
 1150 FORMAT(/3X,'NGSPEC = 1  the free energy is computed as follows:'
     *,//7X,'TEMP(K)',20X,'SLMG',20X,'SLPG'/)
 1200 FORMAT(6X,F8.2,11X,F10.5,13X,F10.5)
 1250 FORMAT(/1X,78(1H-),/20X,'Energetics at adiabatic maximum (VAD)'   0614WH94
     */1X,78(1H-),/34X,'hartrees',5X,'  eV',8X,'cm**-1',6X,' kcal')
 1300 FORMAT(2X,'3 point fit s = ',F8.4)
 1350 FORMAT(5X,'VAD w/re reactant V    ',4X,                           0614WH94
     *2(F9.4,3X),F9.2,3X,F9.4)
 1400 FORMAT(5X,'VAD w/re product V     ',4X,                           0614WH94
     *2(F9.4,3X),F9.2,3X,F9.4)
 1450 FORMAT(5X,'VAD w/re reactant V+ZPE',4X,                           0614WH94
     *2(F9.4,3X),F9.2,3X,F9.4)
 1500 FORMAT(5X,'VAD w/re product V+ZPE ',4X,                           0614WH94
     *2(F9.4,3X),F9.2,3X,F9.4)
 1550 FORMAT(/2X,'5 point fit s = ',F8.4)
 1570 FORMAT(/1X,'** The vibrational partition functions are ',
     *'multiplied',/4X,'by a factor of 10**',I2,' to avoid underflow',/)0614WH94
 1600 FORMAT(/1X,78(1H-))                                               0614WH94
 1650 FORMAT(/1X,26(1H*),' Temperature = ',F8.2,' K ',27(1H*))          1019WH92
 1700 FORMAT(/1X,78(1H-)/,2X,                                           0615WH94
     *'Reactant and product partition functions w/re classical energy ',
     *'of reactant',/1X,78(1H-)/,
     *23X,'Qelec',5X,'Phi_rel',7X,'Qrot',8X,                            06/96ELC
     *'Qvib',8X,'Phi')
 1750 FORMAT(2X,'Reactant:',/5X,'atomic units',3X,F9.5,1P,4E12.4,       0615WH94
     *                      /5X,'CGS units   ',12X,1PE12.4,24X,E12.4)
 1800 FORMAT(2X,'Product: ',/5X,'atomic units',3X,F9.5,1P,4E12.4,       0615WH94
     *                      /5X,'CGS units   ',12X,1PE12.4,24X,E12.4,
     * /1X,78(1H-))
 1810 FORMAT(2X,'Note: Phi_rel is the relative translational',          06/96ELC
     * ' partition function.')                                          06/96ELC
 1815 FORMAT(2X,'      Phi is the product of all the partition',        ..
     * ' functions to its left.')                                       1203WH92
 1850 FORMAT(/1X,78(1H-)/,18X,'Individual vibrational partition'        0615WH94
     *,' functions',/1X,78(1H-))
 1910 FORMAT(/2X,'Reactant 1 : ',5(1PE12.3),/,(15X,5(1PE12.3)))         06/96ELC
 1920 FORMAT(/2X,'Reactant 2 : ',5(1PE12.3),/,(15X,5(1PE12.3)))         06/96ELC
 1930 FORMAT(/2X,'Product  1 : ',5(1PE12.3),/,(15X,5(1PE12.3)))         06/96ELC
 1940 FORMAT(/2X,'Product  2 : ',5(1PE12.3),/,(15X,5(1PE12.3)))         06/96ELC
 2050 FORMAT(1X,78(1H-))
 2051 FORMAT(/2X,'Internal Free Energy of Reactant = -RT LN (Qelec',    1212PF99
     *      '*Qrot*Qvib)',/34X, ' = ',F9.5,2X,'kcal/mol')               1212PF99
 2052 FORMAT(/2x,'NOTE: The rotational partition function (Qrot) and',  3/1/00BL
     *      ' the internal free energy',/8x,'printed above do not',     3/1/00BL
     *      ' include the rotational symmetry number.')                 1213PF99
 2100 FORMAT(/16X,'Canonical Variational Transition ',                  1201WH92
     *'State Properties',/1X,78(1H-),/,
     *15X,'    CVT       G  CVT',/,
     *15X,'   s   (T)   V [s   (T)]     Gmax (T)          k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'   (bohr) ',2(4X,'(kcal/mol)'),'  (cm**3/molecule-sec)')
 2110 FORMAT(/16X,'Canonical Variational Transition ',                  0405JZ07
     *'State Properties',/1X,78(1H-),/,
     *15X,'    CVT       G  CVT',/,
     *15X,'   s   (T)   V [s   (T)]     Gmax (T)          k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'(angstrom)',2(4X,'(kcal/mol)'),'  (cm**3/molecule-sec)')
 2125 FORMAT(/16X,'Canonical Variational Transition ',                  1201WH92
     *'State Properties',/1X,78(1H-),/,
     *15X,'    CVT       G  CVT',/,
     *15X,'   s  (T)    V [s   (T)]     Gmax (T)          k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'  (bohr)     (kcal/mol)    (kcal/mol)         (1/sec)')
 2135 FORMAT(/16X,'Canonical Variational Transition ',                  0405JZ07
     *'State Properties',/1X,78(1H-),/,
     *15X,'    CVT       G  CVT',/,
     *15X,'   s  (T)    V [s   (T)]     Gmax (T)          k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'(angstrom)   (kcal/mol)    (kcal/mol)         (1/sec)')
 2150 FORMAT(1X,'3-Point fit',4X,F8.4,6X,F8.3,6X,F8.3,6X
     *     ,1PE10.3)
 2200 FORMAT(1X,'5-Point fit',4X,F8.4,6X,F8.3,6X,F8.3,6X
     *     ,1PE10.3)
 2250 FORMAT(/16X,'Improved Canonical Variational Transition ',         1203WH92
     *'State Properties',/1X,78(1H-),/,
     *15X,'    ICVT      G  ICVT',/,
     *15X,'   s    (T)  V [s    (T)]    Gmax (T)          k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'   (bohr)  ',2(4X,'(kcal/mol)'),'  (cm**3/molecule-sec)')
 2252 FORMAT(/16X,'Improved Canonical Variational Transition ',         0405JZ07
     *'State Properties',/1X,78(1H-),/,
     *15X,'    ICVT      G  ICVT',/,
     *15X,'   s    (T)  V [s    (T)]    Gmax (T)          k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'(angstrom) ',2(4X,'(kcal/mol)'),'  (cm**3/molecule-sec)')
 2260 FORMAT(/,16X,'Improved Canonical Variational Transition ',        1203WH92
     *'State Properties',/1X,78(1H-),/,
     *15X,'    ICVT      G  ICVT',/,
     *15X,'   s    (T)  V [s    (T)]     Gmax (T)         k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'   (bohr)      (kcal/mol)    (kcal/mol)       (1/sec)')
 2262 FORMAT(/,16X,'Improved Canonical Variational Transition ',        0405JZ07
     *'State Properties',/1X,78(1H-),/,
     *15X,'    ICVT      G  ICVT',/,
     *15X,'   s    (T)  V [s    (T)]     Gmax (T)         k(T)',/,
     *15X,'    *         a  *',/,
     *15X,'(angstrom)     (kcal/mol)    (kcal/mol)       (1/sec)')
 2300 FORMAT(1X,78(1H-))
 2350 FORMAT(8F10.6)
 2400 FORMAT(4I5,F10.5,I5,2F10.5,2I5)
 2450 FORMAT (' NTEMP=', I5, ', but cannot be greater than 40.')
 2500 FORMAT(8F10.0)
 2550 FORMAT(6F10.0)
 2600 FORMAT(3X,27HTemperature loop :  NTEMP =,I5)                      1019WH92
 2650 FORMAT(3X,26HFor tunneling, NQ12,NQ22 =,2I4)
 2750 FORMAT(3X,41HFor this run, VMEP has been multiplied by,F10.5)     1027WH92
 2950 FORMAT(1X,30HVa^Gmu is greater than Va^Gmax)                      06/96ELC
 3000 FORMAT(1X,8HVa^Gmu =,1PE20.10,3X,1H(,1X,F9.4,6H kcal))            06/96ELC
 3050 FORMAT(/,7H Temp =,F10.2/30H large argument of exponential ,/1X,
     *1P,2E14.3,5X,3Hphi,1P,2E14.3,
     * /,' ***** Only forward rate constants are calculated.')
 3100 FORMAT(/1X,41HNo ICVT calculations and hence QIGT = QGT)
 3300 FORMAT(/5X,'s(bohr) = ',F9.5,4X,'Qelec = ',F9.5,/1X,65('-'))      0615WH94
 3302 FORMAT(/5X,'s(angstrom) = ',F9.5,4X,'Qelec = ',F9.5,/1X,65('-'))  0405JZ07
 3310 FORMAT(5X,'Qrot',8X,'Qvib',8X,'Q^GT',8X,'Q^IGT')
 3350 FORMAT(1X,1P,4E12.4)
 3360 FORMAT(/5X,'Individual GTS qvib:',//,(1X,5E12.4))                 0615WH94
 3400 FORMAT(1X,F8.4,1P,3E9.2,E10.2,1X,E9.2,1X,E9.2)
 3450 FORMAT(/1X,'Generalized standard-state free energy of activation',
     *        1X,'in kcal/mol vs. s in bohrs',
     *       /1X,'     s   ','     delta G')
 3460 FORMAT(/1X,'Generalized standard-state free energy of activation',
     *        1X,'in kcal/mol vs. s in angstroms',
     *       /1X,'     s   ','     delta G')
 3500 FORMAT(1X,F9.5,F12.4)
C3550 FORMAT(/1X,'Improved generalized free energy of activation in',   0115WH93
C    *       /1X,'kcal/mol vs. s in bohrs for T =',F8.2,' K',
C    *       /1X,'     s   ','    delta IG')
C3560 FORMAT(/1X,'Improved generalized free energy of activation in',   0405JZ07
C    *       /1X,'kcal/mol vs. s in angstroms for T =',F8.2,' K',
C    *       /1X,'     s   ','    delta IG')
 3600 FORMAT(/1X,'FOR T =',F8.2,4X,'some muVT results',/,
     *30X,I1,'-point',19X,I1,'-point')                                  01/13B92
 3610 FORMAT(/4X,'E = ',F12.6,' hartrees  (',F10.4,' kcal)')
 3620 FORMAT(/4X,'s*muVT = ',F9.5,6X, 's*muVT = ',F9.5,
     *        6X,'s*CVT        = ',F9.5,1P,
     *       /4X,'NmuVT  = ',E12.4,3X,'NmuVT  = ',E12.4,
     *       /3X,'NGT(s*CVT,E) = ',E12.4,/)                             0616WH94
 3650 FORMAT(1X,2F11.6,3(0PF10.3,1PE15.3))
 3700 FORMAT(/1X,'For T =',F8.2,4X,'some muVT and US results',//,
     * 1X,'all extrema obtained from a ',I1,'-point fit.',/)            0616WH94
 3720 FORMAT(/4X,1P,'NmuVT  = ',E12.4, 4X,'NUS    = ',E12.4,0P,
     *       /4X,   'smin1  = ',F9.5, 7X,'smin2  = ',E12.4,
     *        4X,'smax  = ',F9.5,1P,
     *       /4X,   'NmuVT1 = ',E12.4,4X,'NmuVT2 = ',E12.4,
     *        4X,'NuMAX = ',E12.4,/)                                    0616WH94
 3750 FORMAT(1X,2F12.6,1P,2E15.3,0P,3F10.3,1P,3E15.3)
 4000 FORMAT(/1X,'Summary of NGT(s,E) at s = ',F9.5,
     *        /4X,'     E(a.u.)',2X,'    NGT(s,E)')                     0616WH94
 4010 FORMAT(4X,1P,E12.4,2X,E12.4)                                      01/13B92
C
      END subroutine rate
C
C**********************************************************************
C  REARG1
C**********************************************************************
C
      SUBROUTINE rearg1(N1,N2,A,ITAB)
      use perconparam
C
C     Written by Wei-Ping Hu  10/28/92
C     Rearrange elements in an one-dimensional array
C     according to the order in ITAB 
C     An array of size N1 will be rearanged according to array ITAB(N2)
C     N1 = N2 or N1 = N2 + 1, 
C     A is the array to be rearranged
C     ITABLE is the look-up table array such that
C     if j = ITAB(i) then the new rearranged A(i) is
C     from the original A(j)
C     The missing element (when N1=N2+1) will be put into A(N2+1).
C     usually, ITAB is taken from the subroutine fsort1
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C  
      DIMENSION A(N3TM),ITAB(N3TM),TEM(N3TM)
      LOGICAL LRC(N3TM)
C
C
      DO 50 I = 1, N2
         TEM(I) = A( ITAB(I) )
50    CONTINUE
C
C     Find the mode corresponding to the reaction coordinate
C
      IF (N1 .GT. N2) THEN
         DO 60 I = 1, N1
            LRC(I) = .TRUE.
60       CONTINUE
         DO 65 I = 1, N2 
            LRC(ITAB(I)) = .FALSE.
65       CONTINUE
         DO 70 I = 1, N1
            IF (LRC(I)) IRC = I
70       CONTINUE
         A(N2+1) = A(IRC)
      ENDIF
C
      DO 100 I = 1, N2
         A(I) = TEM(I)
100   CONTINUE
C
      RETURN
C 
      END SUBROUTINE rearg1
c
c***********************************************************************
c     rprfl
c***********************************************************************
c
      SUBROUTINE rprfl(WER,WEW,IREPR,NF,IFQLOW,FRELOW)
C
      use perconparam
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension WER(N6TM),WEW(N6TM)
      dimension IREPR(8),NF(8),IFQLOW(N3TM),FRELOW(3,N3TM)  
c
      dimension iwerr(n3tm),fsrr(n3tm)
c      logical   lwell,lprh                                             0423TA02
      logical   lwell
c
      nfr = 0
c
      do i = 1,n3tm
        if (ifqlow(i).ne.0) write (fu6,1996) i
      enddo
c
      do ip = 1,2  
        lwell = .false.
        ishft = nfr
        if (ip.eq.1) then
          if (irepr(1).eq.1) nfr=nf(1)
          if ((irepr(1).eq.1).and.(irepr(2).eq.1)) nfr=nf(1)+nf(2)
          if (irepr(7).eq.1) then 
             nfr=nf(7)
             lwell = .true.
          endif
        else
          if (irepr(3).eq.1) nfr=nf(3)
          if ((irepr(3).eq.1).and.(irepr(4).eq.1)) nfr=nf(3)+nf(4)
          if (irepr(8).eq.1) then
             nfr=nf(8)
             lwell = .true.
          endif
        endif
c
c        if (nfr.ge.nf(5)) then
          do i = 1,n3tm
            iwerr(i) = 0
            fsrr(i) = 0.0d0
          enddo
c
          do i = 1, nfr
            iwerr(i) = i
            fsrr(i) = wer(i+ishft)
          enddo
c
          do i = 1, nfr
            do j = i, nfr
              if (fsrr(i).lt.fsrr(j)) then
                 call dpswap(fsrr(i),fsrr(j))
                 call iswap(iwerr(i),iwerr(j))
              endif
            enddo
          enddo
c
          do i = 1,n3tm
            if ((iwerr(i).ne.0).and.(ifqlow(i).ne.0)) then
              if (ip.eq.1) then
                ix = 1
              else
                ix = 3
              endif
              if (.not.lwell) then
                 if (ix.eq.1) then
                   if (iwerr(i).gt.nf(1)) then
                     write (fu6,1997) nf(2)-(iwerr(i)-nf(1))+1,
     >                           ' react2 ',frelow(ix,i)*AUTOCM
                     wer(iwerr(i)+ishft) = frelow(ix,i)
                   else
                     write (fu6,1997) nf(1)-iwerr(i)+1,
     >                           ' react1 ',frelow(ix,i)*AUTOCM
                     wer(iwerr(i)+ishft) = frelow(ix,i)
                   endif 
                 else
                   if (iwerr(i).gt.nf(3)) then
                     write (fu6,1997) nf(4)-(iwerr(i)-nf(3))+1,
     >                           ' prod2  ',frelow(ix,i)*AUTOCM
                     wer(iwerr(i)+ishft) = frelow(ix,i)
                   else
                     write (fu6,1997) nf(3)-iwerr(i)+1,
     >                           ' prod1 ',frelow(ix,i)*AUTOCM
                     wer(iwerr(i)+ishft) = frelow(ix,i)
                   endif
                 endif
              else
                 wew(nfr+ishft-i+1) = frelow(ix,i)
                 if (ix.eq.1) then
                   write (fu6,1997) i,' reactw ',
     >                              frelow(ix,i)*AUTOCM
                 else
                   write (fu6,1997) i,' prodw ',
     >                              frelow(ix,i)*AUTOCM 
                 endif
              endif
            endif
          enddo
c        endif
      enddo
c
1996  format (' Mode ',I5,' along the reaction path is modified using',
     >        ' IVTST0FREQ method ')
1997  format (' Mode ',I5,' of ',A8,'has been modified to ',
     >        F10.4,' cm-1')
c
      RETURN
      END SUBROUTINE rprfl
c
c***********************************************************************
c     repfl
c***********************************************************************
c
      subroutine repfl(iop,nend,S,BARRS,WSTAR,REDM,EPRD,EWP,EWR,FLSR,
     >                 FLSP,FRELOW,FREQ,IFQLOW,IREPR,LGS)
      use perconparam
c
c     replace the lowfreq mode along the path (include saddle point but 
c     exclude the reactants, products, and wells )
c
      implicit double precision (a-h,o-z)
c
      dimension frelow(3,n3tm),freq(n3tm)
      dimension ifqlow(n3tm),irepr(8),lgs(39)

      double precision L
      logical llin,lup,ldwn
c
      llin = .false.
      lup  = .false.
      ldwn = .false.
c
      if (iop.gt.0) then
        do i = 1, nend
          if (ifqlow(i).ne.0) then
            fr  = frelow(1,i)
            fts = frelow(2,i)
            fp  = frelow(3,i)
c            write (6,*) 'for mode ',i
c            write (6,*) 'fr,fts,fp ',fr*AUTOCM,
c     >              fts*AUTOCM, fp*AUTOCM
            A=EPRD
            B= (2*BARRS-A)+SQRT(BARRS*(BARRS-A))
c If WSTAR=0, replace with a very small number                          1118BE05
            if (WSTAR.eq.0) WSTAR=0.001                                 1118BE05
            L=SQRT(2*BARRS*(BARRS-A)/(REDM*(WSTAR**2)*B))
c
c if s<>0 and the frequencies aren't the same
c
            if ((s.ne.0).and.(((s.lt.0).and.(fts.ne.fr)).or.
     >                        ((s.gt.0).and.(fts.ne.fp))))  then
c
c 2->2
c
              if ((lgs(6).eq.1).and.(irepr(7).ne.1).and.
     >            (irepr(8).ne.1)) then
c
c set up symmetry eckart form for case of two out of three
c are the same
c
                if ((s.gt.0).and.(fts.eq.fr)) fr = fp
                if ((s.lt.0).and.(fts.eq.fp)) fp = fr
                if (((fr.gt.fts).and.(fts.gt.fp)).or.
     >            ((fr.lt.fts).and.(fts.lt.fp))) then
                    llin = .true.
                elseif ((fts.gt.fp).and.(fts.gt.fr)) then
                    lup  = .true.
                elseif ((fts.lt.fp).and.(fts.lt.fr)) then
                    ldwn = .true.
                endif
c
                if (llin) then
                  AM = fp - fr
                  EM = 0.5d0*(fp + fr)
                  XM = 2*((fts-EM)/AM)
                  T0 = -0.5d0*L*LOG((1+XM)/(1-XM))
                  AM =0.5d0* AM
c                  write (6,*) 'Using hyperbolic tangent'
                  fltemp =  hbt(AM,EM,T0,L,S) 
                else
                  AM = fp-fr
                  CM = fr 
                  if (lup) 
     >              BM=(2*fts-AM-2*CM)+2*SQRT((fts-CM)*(fts-AM-CM))
                  if (ldwn)
     >              BM=(2*fts-AM-2*CM)-2*SQRT((fts-CM)*(fts-AM-CM))
                  S0 = -L*LOG((AM+BM)/(BM-AM))
c                  write (6,*) 'Using eckart'
                  fltemp = ECKART(AM,BM,CM,S0,L,S)
                endif 
c
c 2->1
c
              elseif ((lgs(6).eq.2).or.
     >                (irepr(7).ne.1.and.irepr(8).eq.1)) then
c
c set up for cases of two out of three are the same
c
                if ((fts.eq.fr).and.(fp.lt.fr)) fr = fr+2.0d0
                if ((fts.eq.fr).and.(fp.gt.fr)) fr = fr-2.0d0
                if ((fts.eq.fp).and.(fr.lt.fp)) fp = fp+2.0d0
                if ((fts.eq.fp).and.(fr.gt.fp)) fp = fp-2.0d0
                if (s.lt.0) then
                  AM = fp - fr
                  EM = 0.5d0*(fp + fr)
                  XM = 2*((fts-EM)/AM)
                  T0 = -0.5d0*L*LOG((1+XM)/(1-XM))
                  AM =0.5d0* AM
c                  write (6,*) 'Using hyperbolic tangent'
                  fltemp =  hbt(AM,EM,T0,L,S)
                else
                  AM = fp - fr
                  EM = 0.5d0*(fp + fr)
                  XM = 2*((fts-EM)/AM)
                  T0 = -0.5d0*L*LOG((1+XM)/(1-XM))
                  AM =0.5d0* AM
c                  write (6,*) 'Using cut-off hyperbolic tangent 1'
                  fltemp =  cohbt1(AM,EM,T0,L,FLSP,S)
                endif
c
c 1->2
c
              elseif ((lgs(6).eq.3).or.
     >                (irepr(7).eq.1.and.irepr(8).ne.1)) then
c
c set up for the cases of two out of three are the same
c
                if ((fts.eq.fr).and.(fp.lt.fr)) fr = fr+2.0d0
                if ((fts.eq.fr).and.(fp.gt.fr)) fr = fr-2.0d0
                if ((fts.eq.fp).and.(fr.lt.fp)) fp = fp+2.0d0
                if ((fts.eq.fp).and.(fr.gt.fp)) fp = fp-2.0d0
                if (s.gt.0) then
                  AM = fp - fr
                  EM = 0.5d0*(fp + fr)
                  XM = 2*((fts-EM)/AM)
                  T0 = -0.5d0*L*LOG((1+XM)/(1-XM))
                  AM =0.5d0* AM
c                  write (6,*) 'Using hyperbolic tangent'
                  fltemp =  hbt(AM,EM,T0,L,S)
                else
                  AM = fp - fr
                  EM = 0.5d0*(fp + fr)
                  XM = 2*((fts-EM)/AM)
                  T0 = -0.5d0*L*LOG((1+XM)/(1-XM))
                  AM =0.5d0* AM
c                  write (6,*) 'Using cut-off hyperbolic tangent 1'
                  fltemp =  cohbt1(AM,EM,T0,L,FLSR,S)
                endif
c
c 1->1
c
              elseif ((lgs(6).eq.4).or.
     >                (irepr(7).eq.1.and.irepr(8).eq.1)) then
                if (((fr.gt.fts).and.(fts.gt.fp)).or.
     >              ((fr.lt.fts).and.(fts.lt.fp))) then
                  if (irepr(7).eq.1) then
                    VVP = EWP
                  else
                    VVP = EPRD
                  endif
                  if (irepr(8).eq.1) then
                    VVR = EWR
                  else
                    VVR = 0.0d0
                  endif    
c
                  A= VVP-VVR
                  B= (2*BARRS-A)+SQRT(BARRS*(BARRS-A))
c If WSTAR=0, replace with a very small number                          1118BE05
                  if (WSTAR.eq.0) WSTAR=0.001                           1118BE05
                  L=SQRT(2*BARRS*(BARRS-A)/(REDM*(WSTAR**2)*B))
c
                  AM = fp - fr
                  EM = 0.5d0*(fp + fr)
                  XM = 2*((fts-EM)/AM)
                  T0 = -0.5d0*L*LOG((1+XM)/(1-XM))
                  AM =0.5d0* AM
c                  write (6,*) 'Using cut-off hyperbolic tangent 2'
                  fltemp =  cohbt2(AM,EM,T0,L,FLSR,FLSP,S)
                else
                  if ((s.gt.0).and.(fts.eq.fr)) fr = fp
                  if ((s.lt.0).and.(fts.eq.fp)) fp = fr
                  if (s.lt.0) then
                    SW = FLSR
                    FW = fr
                    VW = EWR 
                  else 
                    SW = FLSP
                    FW = fp
                    VW = EWP
                  endif
c                  write (6,*) 'SW ',SW,' FW ',FW,' VW ',VW
                  BM = (SW**2*REDM*WSTAR**2)/(2*(BARRS-VW))
                  AM = (fts-FW) * exp (BM)
                  CM = FW
c                  write (6,*) 'Using cut-off gaussians'
c                  write (6,*) 'A,B,C ',AM,BM,CM
                  fltemp =  cog(AM,BM,CM,SW,S)
                endif
              endif
            else
              fltemp = fts
            endif
            irep = nend-i+1
            freq(irep) = fltemp
          endif
        enddo
      endif
      return
      end subroutine repfl
C
C***********************************************************************
C  RESTOR
C***********************************************************************
C
      SUBROUTINE restor
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : ftitle,otitle
C
C     READS INFO FROM UNIT FU1 TO RESTART RATE CALCULATION
C     THIS ROUTINE WAS EXTENSIVELY MODIFIED ON 8/07/84.  TO SAVE SOME
C     TIME, ALL DATES HAVE BEEN LEFT OUT OF COLUMNS 72-80.
C
C     CALLED BY:
C                DOREST
C     CALLS:
C            DATTIM,TITLE,MEPSRT,RESTRT
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 11/7/91
C    MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C   FORMAT statements were modified 10/21/GL92; all dates and times
C   have been omitted from columns 73-80.
C
C   The include file esp.inc has been removed in version 5.0         1021GL92
C   The storage of the bond orders and charges computed if           1021GL92
C   LGS(35)=1 has been removed in version 5.0.                       1021GL92
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      DIMENSION NRATM2(NATOMS),NF2(4),IATSV2(NATOMS,4)  
      DIMENSION MODER2(4,N3TM),WER2(N6TM),XER2(N6TM),Y00R2(N6TM)  
      DIMENSION FMIHR2(N3TM),SRARR2(NARRS),MODET2(NARRS,N3TM)           6/30YL91
      DIMENSION FMIHT2(N3TM,NSDM)                                       6/30YL91
      DIMENSION EGRND2(4),EFNDT2(N6TM)                                  1106YL92
      character*80 string
c      dimension tem(5),iorder(5)
C
c     Read in variables from the new interface
C     If this is a merge MEP calculation, read the new interface variables
C     out of fu2 restart file first, and then read the fu1 restart file
C     new interface variables, keeping only the latter ones.
C     Anytime a call to rvar6 is made, the restart option must be saved
C     as the one for the current run, not replaced by the one from the original
C     run.
c
      if(.not.allocated(wer)) then
         allocate(wer(n6tm)); wer=0d0
      endif
      if(.not.allocated(ssubi)) then
        allocate(ssubi(nsdm)); ssubi=0d0
      endif
      if(.not.allocated(iatsv))then
        allocate(iatsv(natom,8),xr(3*natom,8)); iatsv=0; xr=0.d00
      end if
!     if(.not.allocated(geom)) then
!       allocate(geom(n3tm,nsdm)); geom=0d0
!     endif
      call dopnm_mem
      call readic_mem
      call zucupd_mem
     
      IF (LGS(8) .GE. 2) THEN                                           0118SW95
         LGS8SV = LGS(8)                                                0118SW95
         call rvar6(fu2)                                                0118SW95
         LGS(8) = LGS8SV                                                0118SW95
      ENDIF                                                             0118SW95
C
      LGS8SV = LGS(8)                                                   0118SW95
      call rvar6(fu1)
      LGS(8) = LGS8SV                                                   0118SW95
c
      WRITE (FU6,1400)
C
C     WRITE OLD TITLE
C
      do 1 i=1,5
          if (otitle(i).ne.' ') write(fu6,*) otitle(i)
1     continue
C
C     WRITE RUN TITLE
C
      do 2 i=1,5
          if (ftitle(i).ne.' ') write(fu6,*) ftitle(i)
2     continue
C
C     READ AND WRITE RUN PARAMETERS TO INSURE CORRECT
C
      READ (FU1,*) NST,DEL,SLM,SLP,NST2                                 1105PF97
      READ (FU1,*) INH                                                  1105PF97
      READ (FU1,*) SOB,SOE                                              7/22YL91
      READ (FU1,*) EZER0                                                7/23YL91
C
      READ (FU1,*) (ICODE(I),I=1,5),(NF(I),I=1,5),N3,LSAVE1,NSHLF
      NATOM = N3/3
      READ (FU1,*) (NEDEG(I),I=1,15)
      READ (FU1,*) (ELEC(I),I=1,15)
      READ (FU1,*) (AMASS(I),I=1,N3)
      READ (FU1,*) REDM,REDMR,DEMIN,EPRD,VAR,VAP,WSTAR                  0313YC97
      READ (FU1,*) (FMOM(I),I=1,4)
C  
      READ  (FU1,*) (SVMAS(I),I=1,NATOM)                                6/12T89
      READ  (FU1,*) (NRATOM(I),I=1,4)                                   6/12T89
      DO 10 I = 1,4                                                     6/12T89
        READ (FU1,*) (IATSV(J,I),J=1,NRATOM(I))                         6/12T89
10    CONTINUE                                                          6/12T89
C                                                                       6/12T89
      NTOT = NF(1)+NF(2)+NF(3)+NF(4)
      READ (FU1,*) (WER(I),XER(I),I=1,NTOT),ANTLR
      IF (LGS(5).GE.21) THEN                                            6/30YL91
         READ (FU1,*) (Y00(I),I=1,NTOT)                                    ..
         READ (FU1,*) (FMIHR(I), I = 1,NTOT)                               ..
         READ (FU1,*) ((MODER(I,J),J=1,NF(I)),I=1,4)                       ..
      ENDIF                                                             6/30YL91
      N3M7 = NF(5)                                                      10/8WH92
      IF (LGS(5).GE.21) THEN                                            6/30YL91
         READ (FU1,*) NARR                                                 ..
         NARL = NARR - 1                                                   ..
         IF (NARL.GE.1) READ (FU1,*) (SRARR(I),I=1,NARL)                   ..
         READ (FU1,*) ((MODETS(J,I),I=1,N3M7),J=1,NARR)                    ..
      ENDIF                                                             6/30YL91
      READ (FU1,*) (SSUBI(I),I=1,LSAVE1)
      READ (FU1,*) ((GEOM(J,I),J=1,N3),I=1,LSAVE1)
C                                                    
      READ (FU1,*) (EGRNDR(I),I=1,4)                                    1106YL92
      READ (FU1,*) (EFNDTR(I),I=1,NTOT)                                     ..
      READ (FU1,*) (EGRND(I),I=1,LSAVE1)                                    ..
      READ (FU1,*) ((EFNDT(J,I),J=1,N3M7),I=1,LSAVE1)                   1106YL92
      READ (FU1,*) (VCLAS(I),I=1,LSAVE1)                                  
      READ (FU1,*) (FMITS(I),I=1,LSAVE1)
      READ (FU1,*) ((WETS(J,I),J=1,N3M7),I=1,LSAVE1)
      READ (FU1,*) ((XETS(J,I),J=1,N3M7),I=1,LSAVE1)
      READ (FU1,*) ((Y0TS(J,I),J=1,N3M7),I=1,LSAVE1)
      READ (FU1,*) ((FMIHTS(J,I),J=1,N3M7),I=1,LSAVE1)                  6/30YL91
      READ (FU1,*) (VADIB(I),I=1,LSAVE1)
      READ (FU1,*) (CDSCMU(I), I=1, LSAVE1)                             2/10GL91
c
c    temp fix to ordering problem
c
c      do 997 i=1,lsave1
c        do 998 j=1,5
c998     tem(j)=wets(j,i)
c        call frsort(5,tem,.true.,iorder)
c        do 999  j=1,5
c         wets(j,i) = tem(j)
c         xets(j,i) = xets(iorder(j),i)
c         y0ts(j,i) = y0ts(iorder(j),i)
c999      fmihts(j,i) = fmihts(iorder(j),i)
c997   continue
C
C     Read unit fu50 data and mu eff. for ZOC from unit fu1
C
      READ (FU1,*) (LGSIC(I),I=1,40)                                    0124WH93
      READ (FU1,*) (ZOCMCD(I),I=1,LSAVE1)                               9/18YL92
      READ (FU1,*) LZCRST                                               1012WH92
      IF (LGS2(11) .NE. 0 .AND. .NOT. LZCRST) THEN
         WRITE(FU6,3300)
         STOP 'RESTOR 1'
      ENDIF 
      READ (FU1,*) BARRA, ERXN                                          1105WH92
      READ (FU1,*) FMIR1A,FMIR2A,FMISPA,FMIP1A,FMIP2A                   0107WH93
      READ (FU1,*) TSWIM                                                1105WH92
      READ (FU1,*) RANGE, BV1, BV2                                      0804WH93
      READ (FU1,*) SP1, VP1S, VP1A                                      1108WH93
      READ (FU1,*) SP2, VP2S, VP2A                                      1108WH93
      READ (FU1,*) (FRP1S(I), I = 1,N3M7)                               1105WH92
      READ (FU1,*) (FRP2S(I), I = 1,N3M7)                               ..
      READ (FU1,*) (FRP1A(I), I = 1,N3M7)                               ..
      READ (FU1,*) (FRP2A(I), I = 1,N3M7)                               1105WH92
      READ (FU1,*) (WESADA(I), I = 1,N3M7)                              1027WH92
      READ (FU1,*) (WERA(I), I = 1, NTOT)                               ..
      READ (FU1,*) (IFRR(I), I = 1,N3M7)                                1027WH92
      READ (FU1,*) (IFRP(I), I = 1,N3M7)                                1027WH92
      READ (FU1,*) (ICFR(I), I = 1,N3M7)                                0202WH93
      READ (FU1,*) (HRMIR(I), I = 1,N6TM)                               0728WH93
      READ (FU1,*) (HRMITS(I), I = 1,N3TM)                              0728WH93
C
      IF (LGS(33).EQ.1) THEN
         READ (FU1,*) ((EWKB0(J,I),J=1,N3M7),I=1,LSAVE1)
         READ (FU1,*) (WGSEX(I),I=1,NTOT)
      ENDIF
      IF (LLCG) THEN                                                    2/10GL91
        READ (FU1,*) ((BCUR(J,I),J=1,N3M7), I=1,LSAVE1)                 5/10DL90
        READ (FU1,*) ((DXSV(J,I),J=1,N3), I=1,LSAVE1)                   5/10DL90
        READ (FU1,*) (((COFSV(K,J,I),K=1,N3), J=1,N3), I=1,LSAVE1)      5/10DL90
      END IF                                                            5/10DL90
C
      WRITE (FU6,1600) NST,LSAVE1,DEL,SLM,SLP,NST2,INH                  1105PF97
      WRITE (FU6,1700) REDM,REDMR,SIGMAF,SIGMAR,DEMIN,EPRD
      WRITE (FU6,1710) EZER0, EZER0*CKCAL                               1006GL92
      IF (LGS(8).GE.2) GO TO 20
      LSAVE = LSAVE1
      GO TO 60
   20 CONTINUE                                                          0118SW95
      READ (FU2,*) IDUM,DUM,DUM,DUM,DUM,IDUM
      READ (FU2,*) IDUM,IDUM
      READ (FU2,*) DUM,DUM
      READ (FU2,*) DUM                                                  7/23YL91
      READ (FU2,*) IDM,IDM,ICODE(3),ICODE(4),IDM,(NF2(I),I=1,4),IDM,    1019BE05
     *           LSAVE2,IDM 
      READ (FU2,*) (IDUM,I=1,6),(NEDEG(I),I=7,15) 
      READ (FU2,*) (DUM,I=1,6),(ELEC(I),I=7,15) 
      READ (FU2,*) (DUM,I=1,N3)
      READ (FU2,*) DUM,DUM,DUM,SIGMAR,DUM,EPRD,DUM,DUM,DUM
      READ (FU2,*) (DUM,I=1,2),FMOM(3),FMOM(4) 
C  
      READ (FU2,*) (DUM,I=1,NATOM)                                      6/12T89
      READ (FU2,*) (NRATM2(I),I=1,4)                                    6/12T89
      DO 25 I = 1,4                                                     6/12T89
           READ (FU2,*) (IATSV2(J,I),J=1,NRATM2(I))                     6/12T89
25    CONTINUE                                                          6/12T89
      DO 28 I = 3,4
        DO 27 J = 1,NRATM2(I)
          IATSV(J,I) = IATSV2(J,I)
27      CONTINUE
28    CONTINUE
C
      NTOT2 = NF2(1) + NF2(2) + NF2(3) + NF2(4) 
      NF(3) = NF2(3)
      NF(4) = NF2(4)
      NTDM = NF2(1) + NF2(2) 
      IBEGP = NF(1) + NF(2) + 1
      IENDP = NTOT2 
      READ (FU2,*) (WER2(I),XER2(I),I=1,NTOT2),DUM                      01/13/GL92
      IF (LGS(5).GE.21) THEN                                            6/30YL91
         READ (FU2,*) (Y00R2(I),I=1,NTOT2)                                  ..
         READ (FU2,*) (FMIHR2(I),I=1,NTOT2)                                 ..
         READ (FU2,*) ((MODER2(I,J),J=1,NF2(I)),I=1,4)                      ..
      ENDIF                                                             6/30YL91
C
      LBEG = LSAVE1+1
      LSAVE = LSAVE1+LSAVE2
      IF (LSAVE.GT.NSDM) STOP 'RESTOR 2'                                1021GL92
      IF (LGS(5).GE.21) THEN                                            6/30YL91
         READ (FU2,*) NARR2                                                 ..
         NARL2 = NARR2 - 1                                                  ..
         IF (NARR2.GT.0) READ (FU2,*) (SRARR2(I),I=1,NARL2)                 ..
         READ (FU2,*) ((MODET2(J,I),I=1,N3M7),J=1,NARR2)                    ..
      ENDIF                                                             6/30YL91
      READ (FU2,*) (SSUBI(I),I=LBEG,LSAVE)
      READ (FU2,*) ((GEOM(J,I),J=1,N3),I=LBEG,LSAVE)
C                                                  
      READ (FU2,*) (EGRND2(I),I=1,4)                                    1106YL92
      READ (FU2,*) (EFNDT2(I),I=1,NTOT2)                                ..
      READ (FU2,*) (EGRND(I),I=LBEG,LSAVE)                              ..
      READ (FU2,*) ((EFNDT(J,I),J=1,N3M7),I=LBEG,LSAVE)                 1106YL92
C
      EGRNDR(3) = EGRND2(3)                                             1106YL92
      EGRNDR(4) = EGRND2(4)                                             1106YL92
      L = IBEGP 
      DO 29 I = NTDM+1,IENDP
        WER(L) = WER2(I)
        XER(L) = XER2(I)
        Y00R(L) = Y00R2(I) 
        FMIHR(L) = FMIHR2(I)                                            1106YL92
        EFNDTR(L) = EFNDT2(I)                                           1106YL92
        L = L + 1 
29    CONTINUE 
      DO 30 I = 3,4
         DO 30 J = 1,NF2(I)
30          MODER(I,J) = MODER2(I,J)
      READ (FU2,*) (VCLAS(I),I=LBEG,LSAVE)
      READ (FU2,*) (FMITS(I),I=LBEG,LSAVE)
      READ (FU2,*) ((WETS(J,I),J=1,N3M7),I=LBEG,LSAVE)
      READ (FU2,*) ((XETS(J,I),J=1,N3M7),I=LBEG,LSAVE)
      READ (FU2,*) ((Y0TS(J,I),J=1,N3M7),I=LBEG,LSAVE)
      READ (FU2,*) ((FMIHT2(J,I),J=1,N3M7),I=LBEG,LSAVE)                6/30YL91
      READ (FU2,*) (VADIB(I),I=LBEG,LSAVE)
      READ (FU2,*) (CDSCMU(I), I= LBEG, LSAVE)                          2/10GL91
C
C     Read unit fu50 data and mu eff. for ZOC from unit fu2
C
      READ (FU2,*) (LGSIC(I),I=1,40)                                    0124WH93
      READ (FU2,*) (ZOCMCD(I),I=LBEG,LSAVE)                             9/18YL92
      READ (FU2,*) LZCRST                                               1012WH92
      IF (LGS2(11) .NE. 0 .AND. .NOT. LZCRST) THEN                      ..
         WRITE(FU6,3300)                                                ..
         STOP 'RESTOR 3'                                                ..
      ENDIF                                                             .. 
      READ (FU2,*) BARRA, ERXN                                          1105WH92
      READ (FU2,*) FMIR1A,FMIR2A,FMISPA,FMIP1A,FMIP2A                   0107WH93
      READ (FU2,*) TSWIM                                                1105WH92
      READ (FU2,*) RANGE, BV1, BV2                                      1108WH93
      READ (FU2,*) SP1, VP1S, VP1A                                      1108WH93
      READ (FU2,*) SP2, VP2S, VP2A                                      1105WH92
      READ (FU2,*) (FRP1S(I), I = 1,N3M7)                               .. 
      READ (FU2,*) (FRP2S(I), I = 1,N3M7)                               ..
      READ (FU2,*) (FRP1A(I), I = 1,N3M7)                               ..
      READ (FU2,*) (FRP2A(I), I = 1,N3M7)                               1105WH92
      READ (FU2,*) (WESADA(I), I = 1,N3M7)                              1027WH92
      READ (FU2,*) (WERA(I), I = 1, NTOT)                               ..
      READ (FU2,*) (IFRR(I), I = 1,N3M7)                                1027WH92
      READ (FU2,*) (IFRP(I), I = 1,N3M7)                                1027WH92
      READ (FU2,*) (ICFR(I), I = 1,N3M7)                                0202WH93
      READ (FU2,*) (HRMIR(I), I = 1,N6TM)                               0728WH93
      READ (FU2,*) (HRMITS(I), I = 1,N3TM)                              0728WH93
C
      IF (LGS(33).EQ.1) THEN
         READ (FU2,*) ((EWKB0(J,I),J=1,N3M7),I=1,LBEG,LSAVE)
         READ (FU2,*) (WGSEX(I),I=1,NTOT2)
      ENDIF   
      IF (LLCG) THEN                                                    2/10GL91
        READ (FU2, *) ((BCUR(J,I),J=1,N3M7), I=LBEG,LSAVE)              5/10DL90
        READ (FU2, *) ((DXSV(J,I),J=1,N3), I=LBEG,LSAVE)                5/10DL90
        READ (FU2, *) (((COFSV(K,J,I),K=1,N3), J=1,N3), I=LBEG,LSAVE)   5/10DL90
      END IF                                                            5/10DL90
C      READ (FU5,*) SINCR                                               3/18T90
      DO 40 I = LBEG, LSAVE
         SSUBI(I) = SSUBI(I)+SINCR
   40 CONTINUE
      CALL MEPSRT
      IF (LGS(5).GT.21) THEN                                            6/30YL91
         MSWITH = 1                                                        ..
         NARL2 = NARR2 - 1                                                 ..
         DO 45 I = 1, NARL2                                                ..
            IF (SSUBI(LBEG).GT.(SRARR2(I)+SINCR)) MSWITH = MSWITH + 1      ..
45       CONTINUE                                                          ..
         IBEG = NARR + 1                                                   ..
         NARR = NARR + NARR2 - MSWITH + 1                                  ..
C                                                                          ..
C     check consistancy between LGS(5) and the total number of             ..
C     arrangemnets                                                         ..
C                                                                          ..
         IF ((LGS(5)-20).NE.NARR.OR.NARR.GT.79) STOP 'RESTOR 4'         1021GL92
            DO 46 I = 1, IBEG, NARR                                        ..
               SRARR(I) = SRARR2(MSWITH)                                   ..
               DO 47 J = 1, N3M7                                           ..
                  MODETS(J,I) = MODET2(J,MSWITH)                           ..
47             CONTINUE                                                    ..
            MSWITH = MSWITH + 1                                            ..
46       CONTINUE                                                          ..
      ENDIF                                                             6/30YL91
C
C   Write the merged MEP information from units fu1 and fu2 to unit fu3
C   if LGS(8) = 3.
C
      IF (LGS(8) .EQ. 3) CALL RESTRT(FU3)                               1106YL92
C
C     WRITE OUT SUMMARY OF REACTION PATH PROPERTIES
C                                                          
   60 WRITE (FU6,1900)
      WRITE (FU6,2000)
      DO 70 I = 1, LSAVE
         WRITE(FU6,2100)SSUBI(I),                                       0623WH94
     *        (K,(GEOM(3*K-3+J,I)/AMASS(3*K-3+J),J=1,3),K=1,NATOM)
   70 CONTINUE
C
      CALL MEPOUT 
C
      RETURN
C
 1400 FORMAT(/2X,'Restart calculation',//2X,'Title of run:')
 1600 FORMAT(/2X,'Parameters used in the original run:',
     *       /2X,'NST    = ',I6,
     *       /2X,'LSAVE  = ',I5,
     *       /2X,'DEL    = ',F10.6,
     *       /2X,'SLM    = ',F10.6,
     *       /2X,'SLP    = ',F10.6,
     *       /2X,'NST2   = ',I6,
     *       /2X,'INH    = ',I5)                                        1105PF97
 1700 FORMAT(/2X,'REDM   = ',F13.6,
     *       /2X,'REDMR  = ',F13.6,
     *       /2X,'SIGMAF = ',F13.6,
     *       /2X,'SIGMAR = ',F13.6,
     *       /2X,'DEMIN  = ',F13.6,
     *       /2X,'EPRD   = ',F13.6)
 1710 FORMAT(2X,'EZER0 = ',1PE15.6,' hartrees (',0P,F12.4,' kcal/mol)') 0623WH94
 1900 FORMAT(/2X,'Reaction path properties')
 2000 FORMAT(/1X,6(1H*),' Space-fixed cartesian coordinates vs'         0613WH94
     *,' reaction coordinate (a.u.) ',6(1H*)/)
 2100 FORMAT(/1X,'s = ',F10.5,
     *       /1X,'Atom',12X,'X',14X,'Y',14X,'Z',/,(I5,4X,1P,3E15.6))    0613WH94
 3300 FORMAT(/5X,'THE RESTART FILE DOES NOT CONTAIN INFORMATION',/,
     *   'ABOUT VTST-IC, SO IC OPTION CAN NOT BE USED')                 0623WH94
C
      END subroutine restor
C
C***********************************************************************
C  RESTRT
C***********************************************************************
C
      SUBROUTINE restrt (IOUT)
      use perconparam, only : n3tm,n6tm,natom
      use common_inc
      use rate_const
C
C     WRITE NECESSARY INFO TO UNIT IOUT
C
C    CALLED BY:
C               DOPNM,RESTOR
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
c     Write out the new variables etc from the interface in version #6
c     and later
c
      call readic_mem
      call wvar6(iout)                                                  1/12S94
      call zucupd_mem
c
      WRITE (IOUT,*) NST,DEL,SLM,SLP,NST2                               1105PF97
      WRITE (IOUT,*) INH                                                1105PF97
      WRITE (IOUT,*) SOB,SOE                                            7/22YL91
      WRITE (IOUT,*) EZER0                                              7/23YL91
      WRITE (IOUT,*) (ICODE(I),I=1,5),(NF(I),I=1,5),N3,LSAVE,NSHLF
      WRITE (IOUT,*) (NEDEG(I),I=1,15)
      WRITE (IOUT,*) (ELEC(I),I=1,15)
      WRITE (IOUT,*) (AMASS(I),I=1,N3)
      WRITE (IOUT,*) REDM,REDMR,DEMIN,EPRD,VAR,VAP,WSTAR                0313YC97 
      WRITE (IOUT,*) (FMOM(I),I=1,4)
C  
      WRITE (IOUT,*) (SVMAS(I),I=1,NATOM)                                6/12T89
      WRITE (IOUT,*) (NRATOM(I),I=1,4)                                   6/12T89
      DO 10 I = 1,4                                                      6/12T89
        WRITE(IOUT,*) (IATSV(J,I),J=1,NRATOM(I))                         6/12T89
10    CONTINUE                                                           6/12T89
C                                                                        6/12T89
      NTOT = NF(1)+NF(2)+NF(3)+NF(4)
C      WRITE (IOUT,*) (WER(I),XER(I),I=1,NTOT),ANTLR
      WRITE (IOUT,1500) (WER(I),XER(I),I=1,NTOT),ANTLR
      IF (LGS(5).GE.21) THEN                                            6/30YL91
         WRITE (IOUT,1500)(Y00R(I),I=1,NTOT)                                ..
         WRITE (IOUT,1500)(FMIHR(I),I=1,NTOT)                               ..
         WRITE (IOUT,*) ((MODER(I,J),J=1,NF(I)),I=1,4)                      ..
      ENDIF                                                             6/30YL91
      N3M7 = NF(5)                                                      10/8WH92
      IF (LGS(5).GE.21) THEN                                            6/30YL91
         WRITE (IOUT,*) NARR                                              ..
         NARL = NARR - 1                                                  ..
         IF (NARR.GT.1) WRITE (IOUT,*) (SRARR(I),I=1,NARL)                ..
         WRITE (IOUT,*) ((MODETS(J,I),I=1,N3M7),J=1,NARR)                 ..
      ENDIF                                                             6/30YL91
      WRITE (IOUT,1500) (SSUBI(I),I=1,LSAVE)
      WRITE (IOUT,1500) ((GEOM(J,I),J=1,N3),I=1,LSAVE)
C
      WRITE (IOUT,*) (EGRNDR(I),I=1,4)                                  1106YL92
      WRITE (IOUT,1500) (EFNDTR(I),I=1,NTOT)                             ..
      WRITE (IOUT,1500) (EGRND(I),I=1,LSAVE)                             ..
      WRITE (IOUT,1500) ((EFNDT(J,I),J=1,N3M7),I=1,LSAVE)               1106YL92
C
      WRITE (IOUT,1500) (VCLAS(I),I=1,LSAVE)
      WRITE (IOUT,1500) (FMITS(I),I=1,LSAVE)
      WRITE (IOUT,1500) ((WETS(J,I),J=1,N3M7),I=1,LSAVE)
      WRITE (IOUT,1500) ((XETS(J,I),J=1,N3M7),I=1,LSAVE)
      WRITE (IOUT,1500) ((Y0TS(J,I),J=1,N3M7),I=1,LSAVE)
      WRITE (IOUT,1500) ((FMIHTS(J,I),J=1,N3M7),I=1,LSAVE)              6/30YL91
      WRITE (IOUT,1500) (VADIB(I),I=1,LSAVE)  
      WRITE (IOUT, 1500) (CDSCMU(I), I = 1, LSAVE)                      2/10GL91
C
C     Save unit fu50 data and mu eff. for ZOC into unit fu1
C
      WRITE (IOUT,*) (LGSIC(I), I=1,40)                                 0124WH93
      WRITE (IOUT,1500) (ZOCMCD(I),I=1,LSAVE)                           1012WH92
      IF (LGS2(11) .NE. 0) THEN                                         ..
         LZCRST = .TRUE.                                                ..
      ELSE                                                              .. 
         LZCRST = .FALSE.                                               ..
      ENDIF                                                             1012WH92
C
      WRITE (IOUT,*) LZCRST                                             1012WH92
      WRITE (IOUT,*) BARRA,ERXN                                         1105WH92
      WRITE (IOUT,*) FMIR1A,FMIR2A,FMISPA,FMIP1A,FMIP2A                 0107WH92
      WRITE (IOUT,*) TSWIM                                               ..
      WRITE (IOUT,*) RANGE, BV1, BV2                                    1108WH93
      WRITE (IOUT,*) SP1, VP1S, VP1A                                    1108WH93
      WRITE (IOUT,*) SP2, VP2S, VP2A                                    1108WH93
      WRITE (IOUT,*) (FRP1S(I), I = 1,N3M7)                             0107WH93
      WRITE (IOUT,*) (FRP2S(I), I = 1,N3M7)                              ..
      WRITE (IOUT,*) (FRP1A(I), I = 1,N3M7)                              ..
      WRITE (IOUT,*) (FRP2A(I), I = 1,N3M7)                             1105WH92
      WRITE (IOUT,*) (WESADA(I), I = 1,N3M7)                            1027WH92
      WRITE (IOUT,*) (WERA(I), I = 1, NTOT)                              ..
      WRITE (IOUT,*) (IFRR(I), I = 1,N3M7)                              1027WH92
      WRITE (IOUT,*) (IFRP(I), I = 1,N3M7)                              1027WH92
      WRITE (IOUT,*) (ICFR(I), I = 1,N3M7)                              0202WH93
      WRITE (IOUT,*) (HRMIR(I), I = 1,N6TM)                             0728WH93
      WRITE (IOUT,*) (HRMITS(I), I = 1,N3TM)                            0728WH93
C
      IF (LGS(33).EQ.1) THEN
         WRITE (IOUT,1500) ((EWKB0(I,J),I=1,N3M7),J=1,LSAVE)
         WRITE (IOUT,1500) (WGSEX(I),I=1,NTOT)
      ENDIF
      IF (LLCG) THEN                                                    2/10GL91
        WRITE (IOUT, 1500) ((BCUR(J,I),J=1,N3M7), I=1,LSAVE)            5/10DL90
        WRITE (IOUT, 1500) ((DXSV(J,I),J=1,N3), I=1,LSAVE)              5/10DL90
        WRITE (IOUT, 1500) (((COFSV(K,J,I),K=1,N3), J=1,N3), I=1,LSAVE) 5/10DL90
      END IF                                                            5/10DL90
      RETURN
C
 1500 FORMAT(1X,8(1X,1P,1E15.8),(/,1X,8(1X,1P,1E15.8)))                 1223YL91
C
      END SUBROUTINE restrt 
C
C
C
C***********************************************************************
C  RPART
C***********************************************************************
C
C   PARAMETERS MODIFIED 6/19/91
C
      FUNCTION rpart (FI,BKT,IOP)
      use perconparam, only : pi
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     COMPUTES CLASSICAL ROTATIONAL PARTITION FUNCTION IN A.U.
C
C     CALLED BY:
C                RATE
C
      RPART = 1.0D0
      IF (IOP.LT.2) RETURN
      IF (IOP.LE.3) THEN
C
C         LINEAR MOLECULE
C
         RPART = 2.0D00*FI*BKT
      ELSE
C
C         NON-LINEAR MOLECULE
C
         RPART = SQRT(8.0D0*PI*FI*BKT**3)
      ENDIF
      RETURN
      END function rpart
C
C***********************************************************************
C  QUADLW  IS REMOVED FROM THIS VERSION OF POLYRATE.
C***********************************************************************
C
C***********************************************************************
C  RPHAIT
C***********************************************************************
C
      SUBROUTINE rphait (FS,SS,S,F,NINT1)
C
C NINT1-point Lagrange interpolation of the function FS to the
C    point S.  Written Oct. '85. BCG
C
C     CALLED BY:
C                RHPINT
C     CALLS:
C            AITKNF
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION FS(NINT1),SS(NINT1),FF(10)
C
      DO 10 I = 1, NINT1
         FF(I) = FS(I)
   10 CONTINUE
      NAIT = NINT1-1
      F = AITKNF(S,FF,SS,NAIT)
      RETURN
      END SUBROUTINE rphait
C
C***********************************************************************
C  RPHB
C***********************************************************************
C
C    PARAMETERS MODIFIED 6/19/91
C
      SUBROUTINE rphb (NBS,S,N,NFREQ,SS,DX1,DX2,DX3,COF,B)
      use perconparam, only : n3tm,nvibm
C
C Calculate Miller-Handy-Adams curvature factor BF,k
C    Written Oct. '85. BCG
C Dimensions were modified by TJ and TNT Jun.'88
C
C     CALLED BY:
C                RPHRD2,RPHRD1
C     CALLS:
C            QUADFT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION SS(3),DX1(N3TM),DX2(N3TM),DX3(N3TM),COF(N3TM,N3TM)
      DIMENSION Y(3),Z(3),DERGRD(N3TM),B(NVIBM)
C
C Compute deriv of [sign(s) * -grad(v)] w.r.t. s
C
      DO 10 I = 1, N
         Y(2) = -DX2(I)
         IF (SS(2).LE.0.0D0) Y(2) = -Y(2)                                       
         Y(3) = -DX3(I)
         IF (SS(3).LE.0.0D0) Y(3) = -Y(3)                                       
         IF (NBS.LE.2) THEN
C
C One-sided differences
C
            DERGRD(I) = (Y(3)-Y(2))/(SS(3)-SS(2))                               
         ELSE
C
C Three-point fit to quadratic
C
            Y(1) = -DX1(I)
            IF (SS(1).LE.0.0D0) Y(1) = -Y(1)
            CALL QUADFT (SS,Y,Z)
            DERGRD(I) = Z(2)+2.0D0*Z(3)*S
         ENDIF
   10 CONTINUE
      ISHFT = N-NFREQ
      DO 30 IB = 1, NFREQ
         SUM = 0.0D0
         DO 20 J = 1, N
            SUM = SUM+DERGRD(J)*COF(J,IB+ISHFT)
   20    CONTINUE
C
C     correction of RPH according to BCALC
C 
         B(IB) = -SUM
   30 CONTINUE
      RETURN
      END SUBROUTINE rphb
C
C***********************************************************************
C  RPHDXN
C***********************************************************************
C
      SUBROUTINE rphdxn (LOPT,DX,DXNORM,AMASS,N)
C
C    Optionally convert from unweighted to mass-weighted gradient and
C    normalize gradient vector.  Written Oct. '85.  BCG
C
C     CALLED BY:
C                RHPRD2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DX(N),AMASS(N)
C
C     FLAG FOR READING FORCE OR GRADIENT
C
      IF (LOPT.EQ.99.OR.LOPT.EQ.100) THEN
         DO I  = 1, N                                                   0812YC97
           DX(I) = -1.0d0 * DX(I)                                       0812YC97
         ENDDO                                                          0812YC97
      ENDIF                                                             0812YC97
C
      IF (LOPT.EQ.0.OR.LOPT.LE.-2.OR.LOPT.EQ.100) THEN                  0830YC96
C
C Convert from unscaled to mass-scaled
C
         DO 10 I = 1, N
            DX(I) = DX(I)/AMASS(I)
   10    CONTINUE
      ENDIF
C
C Normalize DX
C
        SUM = 0.0D0
        DO 20 I = 1, N
           T = DX(I)
           SUM = SUM+T*T
   20   CONTINUE
        DXNORM = SQRT(SUM)                                              0619KN95
        DO 30 I = 1, N
           DX(I) = DX(I)/DXNORM
   30   CONTINUE
      RETURN
      END SUBROUTINE rphdxn
C
C***********************************************************************
C  RPHEXP
C***********************************************************************
C
      SUBROUTINE rphexp (IOP,A,FASY,S,F)
      use perconparam, only : fu6
C
C Evaluate asymptotic exponential functional forms.  This subroutine
C    must be changed if new functional forms are added.  The changes are
C    indicated below.  Written Oct. '85.  BCG
C
C     CALLED BY:
C                RPHINT
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(3)
C
      IF (IOP.LE.0) THEN
C
         F = 0.0D0
      ELSEIF (IOP.EQ.1) THEN
C
C F = A*(1-B*EXP(-C*S))*EXP(-C*S)
C
         T = EXP(-A(3)*S)
         F = A(1)*(1.0D0-A(2)*T)*T
C
      ELSEIF (IOP.LE.4) THEN
C
C F = A*(S-B)*EXP(-C*S)
C
         NEXP = IOP-1
         SN = S**NEXP
         T = EXP(-A(3)*SN)
         F = A(1)*(S-A(2))*T
C
      ELSEIF (IOP.LE.7) THEN
C
C F = A*((S-B)**2)*EXP(-C*S)
C
         NEXP = IOP-4
         SN = S**NEXP
         T = EXP(-A(3)*SN)
         SMB = S-A(2)
         F = A(1)*SMB*SMB*T
C
      ELSEIF (IOP.LE.10) THEN
C
C F = A*(S**B)*EXP(-C*S)
C
         NEXP = IOP-7
         SN = S**NEXP
         T = EXP(-A(3)*SN)
         SABS = ABS(S)
         F = A(1)*(SABS**A(2))*T
C
C    To add a new functional form, the proper ELSE IF statement for the
C    new option number must be placed here.  This is followed by the
C    necessary code to evaluate the functional form in terms of the
C    three parameters A(1), A(2), and A(3), and the s value S.
C
      ELSE
C
C Bad option number
C
         WRITE (FU6,1000) IOP,S
         STOP 'RPHEXP 1'
      ENDIF
C
C Add on asymptotic value.
C
      F = FASY+F
      RETURN
C
 1000 FORMAT (1X,I3,' is not a valid option number for the',
     *   ' asymptotic functional forms.',
     *  /1X,'RPHEXP called with s = ',F10.5)
C
      END subroutine rphexp
C
C**********************************************************************
C  RPHFIT
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C

      SUBROUTINE rphfit (LNS,NFREQ,IQUIT)
      use common_inc, only : mode,lgs
      use perconparam, only : fu6
      use rate_const
      use efmain_mod, only : iprnt
C
C Evaluate parameters of the asymptotic exponential fits.
C    Written Oct. '85. BCG
C
C     CALLED BY:
C                RPHSET
C     CALLS:
C            RPHFT3,RPHFT2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      IQUIT = 0
      IPRNT = LOPT(1)-2
      IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN
         IOPFIT = IFITVR
C
C Long range exponential tails in reactants
C    Fit V(S)
C
         CALL RPHFT3 (IOPFIT,SS(4),SS(3),SS(2),VS(4),VS(3),VS(2),VS(1),
     *      AVR(1),AVR(2),AVR(3),IERR,IPRNT)
         IF (IERR.GT.0) THEN
            WRITE (FU6,1000)
            STOP 'RPHFIT 1'
         ENDIF
         IF (IERR.LT.0) WRITE (FU6,2200)
C
C    Fit FMOM**-1
C       Two-pt fit, range parameter set to potential range parameter
C
         AFIR(3) = AVR(3)
         CALL RPHFT2 (IOPFIT,SS(4),SS(3),FMOMS(4),FMOMS(3),FMOMS(1),AFIR        
     *      (1),AFIR(2),AFIR(3),IERR)
         IF (IERR.GT.0) THEN
            WRITE (FU6,1500)
            IQUIT = 1
         ENDIF
         IF (IERR.LT.0) WRITE (FU6,2700)
         DO 10 I = 1, NFREQ
C
C    Fit w's
C
            IF (LOPT(7).GT.0) THEN
               IOPFIT = IFITWR(I)
C
C       Three-pt fits
C
               CALL RPHFT3 (IOPFIT,SS(4),SS(3),SS(2),WS(4,I),WS(3,I),WS(
     *            2,I),WS(1,I),AWR(1,I),AWR(2,I),AWR(3,I),IERR,IPRNT)           
            ELSE
C
C       Two-pt fits, range parameter set to potential range parameter
C
               IFITWR(I) = IOPFIT
               AWR(3,I) = AVR(3)
               CALL RPHFT2 (IOPFIT,SS(4),SS(3),WS(4,I),WS(3,I),WS(1,I),
     *            AWR(1,I),AWR(2,I),AWR(3,I),IERR)
            ENDIF
            IF (IERR.GT.0) THEN
               WRITE (FU6,1100) I
               IF (LOPT(7).GT.0) STOP 'RPHFIT 2'
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,2300) I
C
C    Fit k3's
C       Three-pt fits
C
            IF (LOPT(7).GT.1) THEN
               CALL RPHFT3 (IFTK3R(I),SS(4),SS(3),SS(2),XK3S(4,I),XK3S(3
     *            ,I),XK3S(2,I),XK3S(1,I),AK3R(1,I),AK3R(2,I),AK3R(3,I),        
     *            IERR,IPRNT)
            ELSE
C
C       Two-pt fits, range parameter set to omega range parameter
C
               IFTK3R(I) = IOPFIT
               AK3R(3,I) = AWR(3,I)
               CALL RPHFT2 (IOPFIT,SS(4),SS(3),XK3S(4,I),XK3S(3,I),XK3S(
     *            1,I),AK3R(1,I),AK3R(2,I),AK3R(3,I),IERR)
            ENDIF
            IF (IERR.GT.0) THEN
               WRITE (FU6,1200) I
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,2400) I
C
C    Fit k4's
C
            IF (LOPT(7).GT.2) THEN
C
C       Three-pt fits
C
               CALL RPHFT3 (IFTK4R(I),SS(4),SS(3),SS(2),XK4S(4,I),XK4S(3
     *            ,I),XK4S(2,I),XK4S(1,I),AK4R(1,I),AK4R(2,I),AK4R(3,I),        
     *            IERR,IPRNT)
            ELSE
C
C       Two-pt fits, range parameter set to omega range parameter
C
               IFTK4R(I) = IOPFIT
               AK4R(3,I) = AWR(3,I)
               CALL RPHFT2 (IOPFIT,SS(4),SS(3),XK4S(4,I),XK4S(3,I),XK4S(
     *            1,I),AK4R(1,I),AK4R(2,I),AK4R(3,I),IERR)
            ENDIF
            IF (IERR.GT.0) THEN
               WRITE (FU6,1300) I
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,2500) I
C
C    Fit B's
C       Two-pt fits, range parameter set to omega range parameter
C
            ABFR(3,I) = AWR(3,I)
            CALL RPHFT2 (IOPFIT,SS(4),SS(3),BFS(4,I),BFS(3,I),BFS(1,I),
     *         ABFR(1,I),ABFR(2,I),ABFR(3,I),IERR)
            IF (IERR.GT.0) THEN
               WRITE (FU6,1400) I
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,2600) I
C
C    Fit Reduced moment of inertia                                      0925JC97
C       Two-pt fit, range parameter set to potential range parameter    0925JC97
C
            IF (LGS(5).GE.21.AND.MODE(I).EQ.9) THEN                     0925JC97
                  AMIR(3,I) = AVR(3)                                    0925JC97
                  CALL RPHFT2 (IOPFIT,SS(4),SS(3),1.D0/FMIRS(4,I),      0925JC97
     *                         1.D0/FMIRS(3,I),0.D0,                    0925JC97
     *                         AMIR(1,I),AMIR(2,I),AMIR(3,I),IERR)      0925JC97
                  IF (IERR.GT.0) THEN                                   0925JC97
                     WRITE (FU6,1505)                                   0925JC97
                     IQUIT = 1                                          0925JC97
                  ENDIF                                                 0925JC97
                  IF (IERR.LT.0) WRITE (FU6,2705)                       0925JC97
            ENDIF                                                       0925JC97
   10    CONTINUE
      ENDIF
C
      IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
         IOPFIT = IFITVP
C
C Long range exponential tails in products
C    Fit V(S)
C
         CALL RPHFT3 (IOPFIT,SS(LNS-3),SS(LNS-2),
     * SS(LNS-1),VS(LNS-3),VS(LNS-2
     *      ),VS(LNS-1),VS(LNS),AVP(1),AVP(2),AVP(3),IERR,IPRNT)
         IF (IERR.GT.0) THEN
            WRITE (FU6,1600)
            STOP 'RPHFIT 3'
         ENDIF
         IF (IERR.LT.0) WRITE (FU6,2800)
C
C    Fit FMOM**-1
C       Two-pt fits, range parameter set to potential range parameter
C
         AFIP(3) = AVP(3)
         CALL RPHFT2 (IOPFIT,SS(LNS-3),SS(LNS-2),
     *      FMOMS(LNS-3),FMOMS(LNS-2),
     *      FMOMS(LNS),AFIP(1),AFIP(2),AFIP(3),IERR)
         IF (IERR.GT.0) THEN
            WRITE (FU6,2100)
            IQUIT = 1
         ENDIF
         IF (IERR.LT.0) WRITE (FU6,3300)
         DO 20 I = 1, NFREQ
C
C    Fit w's
C
            IF (LOPT(8).GT.0) THEN
C
C       Three-pt fits
C
               IOPFIT = IFITWP(I)
               CALL RPHFT3 (IOPFIT,SS(LNS-3),SS(LNS-2),
     *  SS(LNS-1),WS(LNS-3,I)
     *            ,WS(LNS-2,I),WS(LNS-1,I),WS(LNS,I),
     *  AWP(1,I),AWP(2,I),AWP(
     *            3,I),IERR,IPRNT)
            ELSE
C
C       Two-pt fits, range parameter set to potential range parameter
C
               IFITWP(I) = IOPFIT
               AWP(3,I) = AVP(3)
               CALL RPHFT2 (IOPFIT,SS(LNS-3),SS(LNS-2),
     *            WS(LNS-3,I),WS(LNS-2,
     *            I),WS(LNS,I),AWP(1,I),AWP(2,I),AWP(3,I),IERR)
            ENDIF
            IF (IERR.GT.0) THEN
               WRITE (FU6,1700) I
               IF (LOPT(8).GT.0) STOP 'RPHFIT 4'
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,2900) I
C
C    Fit k3's
C
            IF (LOPT(8).GT.1) THEN
C
C       Three-pt fits
C
               CALL RPHFT3 (IFTK3P(I),SS(LNS-3),SS(LNS-2),
     *            SS(LNS-1),XK3S(LNS
     *            -3,I),XK3S(LNS-2,I),XK3S(LNS-1,I),
     *            XK3S(LNS,I),AK3P(1,I),
     *            AK3P(2,I),AK3P(3,I),IERR,IPRNT)
            ELSE
C
C       Two-pt fits, range parameter set to potential range parameter
C
               IFTK3P(I) = IOPFIT
               AK3P(3,I) = AWP(3,I)
               CALL RPHFT2 (IOPFIT,SS(LNS-3),SS(LNS-2),XK3S(LNS-3,I),
     *            XK3S(LNS-2,I),XK3S(LNS,I),AK3P(1,I),
     *            AK3P(2,I),AK3P(3,I),
     *            IERR)
            ENDIF
            IF (IERR.GT.0) THEN
               WRITE (FU6,1800) I
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,3000) I
C
C    Fit k4's
C
            IF (LOPT(8).GT.2) THEN
C
C       Three-pt fits
C
               CALL RPHFT3 (IFTK4P(I),SS(LNS-3),SS(LNS-2),
     *            SS(LNS-1),XK4S(LNS
     *            -3,I),XK4S(LNS-2,I),XK4S(LNS-1,I),
     *            XK4S(LNS,I),AK4P(1,I),
     *            AK4P(2,I),AK4P(3,I),IERR,IPRNT)
            ELSE
C
C       Two-pt fits, range parameter set to omega range parameter
C
               IFTK4P(I) = IOPFIT
               AK4P(3,I) = AWP(3,I)
               CALL RPHFT2 (IOPFIT,SS(LNS-3),SS(LNS-2),XK4S(LNS-3,I),
     *            XK4S(LNS-2,I),XK4S(LNS,I),AK4P(1,I),
     *            AK4P(2,I),AK4P(3,I),
     *            IERR)
            ENDIF
            IF (IERR.GT.0) THEN
               WRITE (FU6,1900) I
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,3100) I
C
C    Fit B's
C       Two-pt fits, range parameter set to omega range parameter
C
            ABFP(3,I) = AWP(3,I)
            CALL RPHFT2 (IOPFIT,SS(LNS-3),SS(LNS-2),
     *         BFS(LNS-3,I),BFS(LNS-2,I
     *         ),BFS(LNS,I),ABFP(1,I),ABFP(2,I),ABFP(3,I),IERR)
            IF (IERR.GT.0) THEN
               WRITE (FU6,2000) I
               IQUIT = 1
            ENDIF
            IF (IERR.LT.0) WRITE (FU6,3200) I
C
C    Fit Reduced moment of inertia                                      0925JC97
C       Two-pt fit, range parameter set to potential range parameter    0925JC97
C
            IF (LGS(5).GE.21.AND.MODE(I).EQ.9) THEN                     0925JC97
                  AMIP(3,I) = AVP(3)                                    0925JC97
                  CALL RPHFT2 (IOPFIT,SS(LNS-3),SS(LNS-2),
     *                         1.D0/FMIRS(LNS-3,I),1.D0/FMIRS(3,I),0.D0,0925JC97
     *                         AMIP(1,I),AMIP(2,I),AMIP(3,I),IERR)      0925JC97
                  IF (IERR.GT.0) THEN                                   0925JC97
                     WRITE (FU6,1507)                                   0925JC97
                     IQUIT = 1                                          0925JC97
                  ENDIF                                                 0925JC97
                  IF (IERR.LT.0) WRITE (FU6,2707)                       0925JC97
            ENDIF                                                       0925JC97
   20    CONTINUE
      ENDIF
      RETURN
C
 1000 FORMAT (' EXPONENTIAL FIT FAILED FOR REACTANT POTENTIAL')
 1100 FORMAT (' EXPONENTIAL FIT FAILED FOR REACTANT FREQUENCY', I5)
 1200 FORMAT (' EXPONENTIAL FIT FAILED FOR REACTANT K3', I5)
 1300 FORMAT (' EXPONENTIAL FIT FAILED FOR REACTANT K4', I5)
 1400 FORMAT (' EXPONENTIAL FIT FAILED FOR REACTANT BFS', I5)
 1500 FORMAT (' EXPONENTIAL FIT FAILED FOR REACTANT FMOM')
 1505 FORMAT (' EXPONENTIAL FIT FAILED FOR REACTANT REDMOM')
 1507 FORMAT (' EXPONENTIAL FIT FAILED FOR PRODUCT REDMOM')
 1600 FORMAT (' EXPONENTIAL FIT FAILED FOR PRODUCT POTENTIAL')
 1700 FORMAT (' EXPONENTIAL FIT FAILED FOR PRODUCT FREQUENCY', I5)
 1800 FORMAT (' EXPONENTIAL FIT FAILED FOR PRODUCT K3', I5)
 1900 FORMAT (' EXPONENTIAL FIT FAILED FOR PRODUCT K4', I5)
 2000 FORMAT (' EXPONENTIAL FIT FAILED FOR PRODUCT BFS', I5)
 2100 FORMAT (' EXPONENTIAL FIT FAILED FOR PRODUCT FMOM')
 2200 FORMAT (' WARNING FOR EXPONENTIAL FIT OF REACTANT POTENTIAL')
 2300 FORMAT (' WARNING FOR EXPONENTIAL FIT OF REACTANT FREQUENCY', I5)
 2400 FORMAT (' WARNING FOR EXPONENTIAL FIT OF REACTANT K3', I5)
 2500 FORMAT (' WARNING FOR EXPONENTIAL FIT OF REACTANT K4', I5)
 2600 FORMAT (' WARNING FOR EXPONENTIAL FIT OF REACTANT BFS', I5)
 2700 FORMAT (' WARNING FOR EXPONENTIAL FIT OF REACTANT FMOM')
 2705 FORMAT (' WARNING FOR EXPONENTIAL FIT OF REACTANT REDFMOM')
 2707 FORMAT (' WARNING FOR EXPONENTIAL FIT OF PRODUCT REDFMOM')
 2800 FORMAT (' WARNING FOR EXPONENTIAL FIT OF PRODUCT POTENTIAL')
 2900 FORMAT (' WARNING FOR EXPONENTIAL FIT OF PRODUCT FREQUENCY', I5)
 3000 FORMAT (' WARNING FOR EXPONENTIAL FIT OF PRODUCT K3', I5)
 3100 FORMAT (' WARNING FOR EXPONENTIAL FIT OF PRODUCT K4', I5)
 3200 FORMAT (' WARNING FOR EXPONENTIAL FIT OF PRODUCT BFS', I5)
 3300 FORMAT (' WARNING FOR EXPONENTIAL FIT OF PRODUCT FMOM')
C
      END SUBROUTINE rphfit
C
C***********************************************************************
C  RPHFT2
C***********************************************************************
C
      SUBROUTINE rphft2 (IOP,SS1,SS2,FS1,FS2,FASY,A,B,C,IERR)
      use perconparam, only : fu6
C
C 2-point fits to 3-parameter asymptotic exponential functions with
C    the range parameter C fixed.  Written Oct. '85. BCG
C    IOP - option selecting the functional form
C    SSi - s values at three points
C    FSi - function values at the SSi
C    FASY- asymptotic function value
C    A,B,C- parameters
C    IERR - error flag
C This subroutine must be changed if new functional forms are added.
C    The changes are indicated below.  Currently the subroutine
C    supports 10 functional forms:
C    Option code       Functional form
C    ___________       _____________________________________________
C        1             F(s) = A [1 - B exp(-Cs)] exp(-Cs)
C        2             F(s) = A (s-B) exp(-Cs)
C        3             F(s) = A (s-B) exp(-Cs**2)
C        4             F(s) = A (s-B) exp(-Cs**3)
C        5             F(s) = A (s-B)**2 exp(-Cs)
C        6             F(s) = A (s-B)**2 exp(-Cs**2)
C        7             F(s) = A (s-B)**2 exp(-Cs**3)
C        8             F(s) = A abs(s)**B exp(-Cs)
C        9             F(s) = A abs(s)**B exp(-Cs**2)
C       10             F(s) = A abs(s)**B exp(-Cs**3)
C
C
C     CALLED BY:
C                RPHFIT
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IERR = 0
      A = 0.0D0
      B = 0.0D0
      IF (IOP.LE.0) RETURN
C
C It is assumed that ABS(SS2)>ABS(SS1) and ABS(FS1-FASY)>ABS(FS2-FASY)
C    Get sign of s from SS2 and set S1,S2 such that S2>0
C
      IF (SS2.LT.0.0D0) THEN
         SGNS = -1.0D0
      ELSE
         SGNS = 1.0D0
      ENDIF
      S1 = SGNS*SS1
      S2 = SGNS*SS2
C
C    Get Fi = FSi - FASY
C
      F1 = FS1-FASY
      F2 = FS2-FASY
C
C    Check if F1 and F2 are zero
C
      IF (F1.EQ.0.0D0.AND.F2.EQ.0.0D0) RETURN
C
C    Get sign of F from F2 and set F1,F2 such that F2>0
C
      IF (F2.LT.0.0D0) THEN
         SGNF = -1.0D0
         F1 = -F1
         F2 = -F2
      ELSE
         SGNF = 1.0D0
      ENDIF
      IERR = 0
      IF (S2.LE.S1.OR.S2.LT.0.0D0.OR.F2.LE.0.0D0) THEN
         WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
         WRITE (FU6,1100)
         IERR = 1
         A = 0.0D0
         B = 0.0D0
         RETURN
      ENDIF
C
C ======================================================================
C
      IF (IOP.EQ.1) THEN
C
C fit F = A*(1-B*EXP(-C*S))*EXP(-C*S)
C
         CC = SGNS*C
         IF (CC.LT.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1200) SGNS
            IERR = 1
            A = 0.0D0
            B = 0.0D0
            RETURN
         ENDIF
         EX1 = EXP(CC*S1)
         EX2 = EXP(CC*S2)
         AB = (F1*EX1-F2*EX2)/(1.0D0/EX2-1.0D0/EX1)
         A = F2*EX2+AB/EX2
         B = AB/A
C
C    check that A is greater than zero
C
         IF (A.LE.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1300) A,B
            IERR = -1
         ELSEIF (F1.LE.0.0D0) THEN
            S0 = LOG(B)/C
            IF (S0.LT.S1.OR.S0.GE.S2) THEN
               WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
               WRITE (FU6,1400) A,B
               IERR = -1
            ENDIF
         ENDIF
C
C    change sign of A to have sign of SGNF
C
         A = SGNF*A
C
C ======================================================================
C
      ELSEIF (IOP.LE.4) THEN
C
C fit F = A*(S-B)*EXP(-CC*S)
C
         NEXP = IOP-1
         CC = C*(SGNS**NEXP)
         IF (CC.LT.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1500) SGNS,NEXP
            IERR = 1
            A = 0.0D0
            B = 0.0D0
            RETURN
         ENDIF
         S1N = S1**NEXP
         S2N = S2**NEXP
         EX1 = F2*EXP(-CC*S1N)
         T = EXP(-CC*S2N)
         EX2 = F1*T
         B = (S2*EX2-S1*EX1)/(EX2-EX1)
         A = F2/((S2-B)*T)
C
C    check that A > 0
C    check that and B is in the correct range
C
         IF (A.LE.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1300) A,B
            IERR = -1
C
C    check that and B is in the correct range
C
         ELSEIF ((F1.GT.0.0D0.AND.B.GT.S1).OR.(F1.LE.0.0D0.AND.
     *      (B.LT.S1.OR.B.GT.S2))) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1400) A,B
            IERR = -1
         ENDIF
C
C    change sign of A to have sign of SGNS*SGNF and sign of B to have
C    sign of SGNS
C
         A = SGNS*SGNF*A
         B = SGNS*B
C
C ======================================================================
C
      ELSEIF (IOP.LE.7) THEN
C
C fit F = A*((S-B)**2)*EXP(-C*S)
C    for this fit F1 must also be positive
C
         IF (F1.LT.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1600)
            IERR = 1
            RETURN
         ENDIF
         NEXP = IOP-4
         CC = C*(SGNS**NEXP)
         IF (CC.LT.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1500) SGNS,NEXP
            IERR = 1
            A = 0.0D0
            B = 0.0D0
            RETURN
         ENDIF
         S1N = S1**NEXP
         S2N = S2**NEXP
         TF2 = SQRT(F2)
         EX1 = TF2*EXP(-0.5D0*CC*S1N)
         T = EXP(-CC*S2N)
         EX2 = SQRT(F1*T)
         B = (S2*EX2-S1*EX1)/(EX2-EX1)
         T = (S2-B)*(S2-B)*T
         A = F2/T
C
C    check that A > 0
C
         IF (A.LE.0.0D0) THEN
            IERR = -1
C
C    check that B is in the correct range
C
         ELSEIF (B.GT.S1) THEN
            IERR = -2
         ENDIF
         IF (IERR.NE.0) THEN
            IERRO = IERR
            AO = A
            BO = B
            SIGNO = -1.0D0
            B = (S2*EX2+S1*EX1)/(EX2+EX1)
            T = (S2-B)*(S2-B)*T
            A = F2/T
C
C    check that A > 0  and B is in the correct range
C
            IF (A.LE.0.0D0) THEN
               IERR = -1
            ELSEIF (B.GT.S1) THEN
               IERR = -2
            ENDIF
            SIGNF = 1.0D0                                               09/95KAN
            IF (IERR.NE.0.AND.IERRO.LT.0) THEN
               A = AO
               B = BO
               SIGNF = SIGNO                                            09/95KAN
            ENDIF
         ENDIF
         IF (IERR.NE.0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            IF (IERR.EQ.-1) WRITE (FU6,1300) A,B
            IF (IERR.EQ.-2) WRITE (FU6,1400) A,B
         ENDIF
C
C    change sign of A to have sign of SGNF and sign of B to have sign of
C    SGNS
C
         A = SGNF*A
         B = SGNS*B
C
C ======================================================================
C
      ELSEIF (IOP.LE.10) THEN
C
C fit F = A*(S**B)*EXP(-C*S)
C
C    for this fit S1 and F1 must also be positive
C
         IF (F1.LE.0.0D0.OR.S1.LE.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1600)
            IERR = 1
            RETURN
         ENDIF
         NEXP = IOP-7
         CC = C*(SGNS**NEXP)
         IF (CC.LT.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1500) SGNS,NEXP
            IERR = 1
            A = 0.0D0
            B = 0.0D0
            RETURN
         ENDIF
         S1N = S1**NEXP
         S2N = S2**NEXP
         XLS2 = LOG(S2/S1)
         XLF2 = LOG(F2/F1)
         B = (XLF2+CC*(S2N-S1N))/XLS2
         A = F2*EXP(CC*S2N)/(S2**B)
C
C    check that A > 0
C
         IF (A.LE.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,SGNF,FASY,F1,F2,C
            WRITE (FU6,1300) A,B
            IERR = -1
         ENDIF
C
C    change sign of A to have sign of SGNF
C
         A = SGNF*A
C
C ======================================================================
C
C    To add a new functional form, the proper ELSE IF statement for the
C    new option number must be placed here.  This is followed by the
C    necessary code to evaluate the parameters of the functional
C    form in terms of the function values at 2 s values.
C
C ======================================================================
C
      ELSE
C
C Bad option number
C
         WRITE (FU6,1700) IOP
         STOP 'RPHFT2 1'
      ENDIF
      RETURN
C
 1000 FORMAT (1X, 78(1H*)/ ' RPHFT2, IOP=', I5/ 1X, F5.0, '*S=', 16X,
     *   1P,2E13.5/ 1X, 0PF5.0, '*(F-', 1PE13.5, ')=', 1P,2E13.5/ 1X,
     *   'C=', 1PE13.5)
 1100 FORMAT (' EITHER S OR F IS NOT ORDERED CORRECTLY')
 1200 FORMAT (1X, F3.0, '*C < 0.')
 1300 FORMAT (' A <= 0.'/ ' A,B=', 1P,2E13.5)
 1400 FORMAT (' B NOT IN CORRECT RANGE.'/ ' A,B=', 1P,2E13.5)
 1500 FORMAT (' (', F3.0, '**-', I1, ')*C < 0.')
 1600 FORMAT (' F1<0')
 1700 FORMAT (1X, I3, ' is not allowed as an option number for the',
     *   ' asymptotic exponential fits.')
C
      END SUBROUTINE rphft2
C
C***********************************************************************
C  RPHFT3
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/20/91
C
      SUBROUTINE rphft3 (IOP,SS1,SS2,SS3,FS1,FS2,FS3,FASY,A,B,C,IERR,
     *   IOPPRN)
      use perconparam, only : fu6
C
C 3-point fits of asymptotic exponential functions.
C    Written Oct. '85.  BCG
C    IOP - option selecting the functional form
C    SSi - s values at three points
C    FSi - function values at the SSi
C    FASY- asymptotic function value
C    A,B,C- parameters
C    IERR - error flag
C    IOPPRN- print flag
C This subroutine must be changed if new functional forms are added.
C    The changes are indicated below. Currently the routine suppports
C    10 functional forms:
C    Option code       Functional form
C    ___________       _____________________________________________
C        1             F(s) = A [1 - B exp(-Cs)] exp(-Cs)
C        2             F(s) = A (s-B) exp(-Cs)
C        3             F(s) = A (s-B) exp(-Cs**2)
C        4             F(s) = A (s-B) exp(-Cs**3)
C        5             F(s) = A (s-B)**2 exp(-Cs)
C        6             F(s) = A (s-B)**2 exp(-Cs**2)
C        7             F(s) = A (s-B)**2 exp(-Cs**3)
C        8             F(s) = A abs(s)**B exp(-Cs)
C        9             F(s) = A abs(s)**B exp(-Cs**2)
C       10             F(s) = A abs(s)**B exp(-Cs**3)
C
C     CALLED BY:
C                RPHFIT
C     CALLS:
C            RPHG1,RPHNWT,RPHG2,RPHG5
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

c     SAVE DG
C
      EXTERNAL RPHG1,RPHG2,RPHG5
C
      IERR = 0
      A = 0.0D0
      B = 0.0D0
      C = 0.0D0
      IF (IOP.LE.0) RETURN
C
C It is assumed that ABS(SS3)>ABS(SS2) and ABS(FS2-FASY)>ABS(FS3-FASY)
C    Get sign of s from SS3 and set S1,S2,S3 such that S3>S2>0
C
      IF (SS3.LT.0.0D0) THEN
         SGNS = -1.0D0
      ELSE
         SGNS = 1.0D0
      ENDIF
      S1 = SGNS*SS1
      S2 = SGNS*SS2
      S3 = SGNS*SS3
C
C    Get Fi = FSi - FASY
C
      F1 = FS1-FASY
      F2 = FS2-FASY
      F3 = FS3-FASY
C
C    First check if F = 0 for all s
C
      IF (F1.EQ.0.0D0.AND.F2.EQ.0.0D0.AND.F3.EQ.0.0D0) RETURN
C
C    Get sign of F from F3 and set F1,F2,F3 such that F2>F3>0
C
      IF (F3.LT.0.0D0) THEN
         SGNF = -1.0D0
         F1 = -F1
         F2 = -F2
         F3 = -F3
      ELSE
         SGNF = 1.0D0
      ENDIF
      IERR = 0
C
C Check that the S and F values are consistent with the functional
C    forms
C
      IF (S3.LE.S2.OR.S2.LE.S1.OR.S2.LT.0.0D0.OR.F2.LE.F3.OR.F3.
     *   LE.0.0D0) THEN
         WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
         WRITE (FU6,1100)
         IERR = 1
         RETURN
      ENDIF
      IF (IOPPRN.GT.0) WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,
     *                                  FASY,F1,F2,F3
C
C ======================================================================
C
      IF (IOP.EQ.1) THEN
C
C fit F = A*(1-B*EXP(-C*S))*EXP(-C*S)
C
C    Root search for G(C) = 0.  First find limits on C.  Start at
C       initial guess at C.
C
         C = 2.0D0*LOG((F2-F3)/F3)/(S3-S1)
         IF (C.LE.0.0D0) C = 1.0D0
         G = RPHG1(C,S1,S2,S3,F1,F2,F3,DG)
C
C       Check sign of G
C
         IF (G.GT.0.0D0) THEN
C
C       G is positive, increase C until G becomes negative
C
            IC = 0
            IF (IOPPRN.GT.1) WRITE (FU6,2500)
   10       CONTINUE
            CMIN = C
            C = C+1.0D0
            G = RPHG1(C,S1,S2,S3,F1,F2,F3,DG)
            IC = IC+1
            IF (IOPPRN.GT.1) WRITE (FU6,2600) IC,C,G
            IF (IC.LT.1000.AND.G.GT.0.0D0) GO TO 10
            IF (G.GT.0.0D0) THEN
C
C          Couldn't find a negative G, set error flag
C
               WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
               WRITE (FU6,1200) C
               IERR = 1
            ELSE
C
C          G is negative, CMAX is last C and CMIN is the previous C
C
               CMAX = C
            ENDIF
         ELSE
C
C       G is negative decrease C until G becomes positive
C
            DELC = C/20.0D0
            IF (IOPPRN.GT.1) WRITE (FU6,2700)
   20       CONTINUE
            CMAX = C
            C = C-DELC
            G = RPHG1(C,S1,S2,S3,F1,F2,F3,DG)
            IF (IOPPRN.GT.1) WRITE (FU6,2800) C,G
            IF (C.GT.0.0D0.AND.G.LT.0.0D0) GO TO 20
            CMIN = C
            IF (C.GT.0.0D0) THEN
C
C       G < 0 at all values of G checked, G at C=0 is zero and if sign
C          of the derivative of G (DG = dG/dC) is positive then a root
C          does exist
C
               C = 0.0D0
               G = RPHG1(C,S1,S2,S3,F1,F2,F3,DG)
               IF (DG.LE.0.0D0) THEN
C
C          Derivative is negative, no root exists
C
                  WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
                  WRITE (FU6,1300)
                  IERR = 1
               ELSE
C
C          Derivative positive, now find limits on C so that root search
C             does not find C=0 as root.  Take step in C and check if
C             G(C) is positive, if not decrease step size.  Start off
C             with step size of DELC/10
C
   30             CONTINUE
                  DELC = 0.1D0*DELC
                  C = DELC
                  G = RPHG1(C,S1,S2,S3,F1,F2,F3,DG)
                  IF (G.LT.0.0D0.AND.C.GT.1.0D-6) GO TO 30
                  IF (G.LT.0.0D0) THEN
                     WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,
     *                                FASY,F1,F2,F3
                     WRITE (FU6,1400)
                     IERR = 1
                  ELSE
C
C          Increase C until the sign of DG becomes negative
C
                     C = 0.0D0
                     IF (IOPPRN.GT.1) WRITE (FU6,2900)
   40                CONTINUE
                     C = C+DELC
                     G = RPHG1(C,S1,S2,S3,F1,F2,F3,DG)
                     IF (IOPPRN.GT.1) WRITE (FU6,2800) C,G,DG
                     IF (DG.GT.0.0D0) GO TO 40
C
C          CMIN is last C, CMAX taken from loop to find positive G above
C
                     CMIN = C
                  ENDIF
               ENDIF
            ELSE
C
C          Found positive G, CMIN is the last C and CMAX is the previous
C             C
C
               CMIN = C
            ENDIF
         ENDIF
         IF (IERR.EQ.0) THEN
C
C       Limits on C found, now ready for root search
C
            C = 0.5D0*(CMIN+CMAX)
            G = RPHG1(C,S1,S2,S3,F1,F2,F3,DG)
            SGNDG = SIGN(1.0D0,DG)
C
C       Root search
C
            IF (IOPPRN.GT.1) WRITE (FU6,3000)
            CALL RPHNWT (RPHG1,S1,S2,S3,F1,F2,F3,C,CMIN,CMAX,SGNDG,IERR,
     *         IOPPRN)
            IF (IERR.EQ.0) THEN
C
C    Check sign of C (must be positive)
C
               IF (C.LE.0.0D0) THEN
                  WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
                  WRITE (FU6,1500)
                  IERR = 1
               ELSE
C
C    Find A and B
C
                  EX2 = EXP(C*S2)
                  EX3 = EXP(C*S3)
                  ABX = (F2*EX2-F3*EX3)/(1.0D0/EX3-1.0D0/EX2)
                  A = F3*EX3+ABX/EX3
                  B = ABX/A
C
C    Check that A is greater than zero
C
                  IF (A.LE.0.0D0) THEN
                     WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,
     *                                FASY,F1,F2,F3
                     WRITE (FU6,1600) A,B,C
                     IERR = -1
                  ELSEIF (F1.LE.0.0D0) THEN
                     S0 = LOG(B)/C
                     IF (S0.LT.S1.OR.S0.GE.S2) THEN
                        WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,
     *                                   FASY,F1,F2,F3
                        WRITE (FU6,1700) A,B,C
                        IERR = -1
                     ENDIF
                  ENDIF
C
C    change sign of A to have sign of SGNF and sign of C to sign of SGNS
C
                  A = SGNF*A
                  C = SGNS*C
               ENDIF
            ENDIF
         ENDIF
C
C ======================================================================
C
      ELSEIF (IOP.LE.4) THEN
C
C fit F = A*(S-B)*EXP(-C*S**NEXP)
C
         NEXP = IOP-1
         S1N = S1**NEXP
         S2N = S2**NEXP
         S3N = S3**NEXP
C
C    Root search for G(C)=0; first check if root exists.
C
         CX = LOG(F2*(S3-S1)*(S2N-S1N)/(F3*(S2-S1)*(S3N-S1N)))/(S3N-S2N)
         CX = MAX(CX,0.0D0)
         G = RPHG2(CX,S1,S2,S3,F1,F2,F3,DG)
         IF (IOPPRN.GT.1) WRITE (FU6,3100) CX,G,DG
         IF (G.GT.0.0D0) THEN
C
C       minimum occurs for a  positive G, therefore no root exists
C
            WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
            WRITE (FU6,1800)
            IERR = 1
         ELSE
C
C       get value of G(C=0).
C
            C = 0.0D0
            G0 = RPHG2(C,S1,S2,S3,F1,F2,F3,DG)
            IF (IOPPRN.GT.1) WRITE (FU6,3200) C,G0,DG
C
C       If G0 > 0, two roots exist, try to find the one for C>Cx first,
C          and if it doesn't work then try to find one for C<Cx.
C
            IF (G0.GT.0.0D0) THEN
               NROOT = 2
            ELSE
               NROOT = 1
            ENDIF
C
C       initial guess at C
C
            C = LOG(F2*(S3-S2)/F3*(S2-S1))/(S3N-S2N)
            IF (C.LT.CX) C = CX+0.5D0
            G = RPHG2(C,S1,S2,S3,F1,F2,F3,DG)
            SGNDG = SIGN(1.0D0,DG)
            IF (IOPPRN.GT.1) WRITE (FU6,3300) C,G,DG
C
C       if G > 0, C is an upper limit, otherwise a lower limit
C
            IF (G.GT.0.0D0) THEN
               CMIN = CX
               CMAX = C
            ELSE
               CMIN = C
               CMAX = 1.0D+30
            ENDIF
C
C       loop over roots starts here
C
            IROOT = 0
            IERRO = 0
   50       CONTINUE
            IROOT = IROOT+1
            IF (IOPPRN.GT.1) WRITE (FU6,3400) IROOT
C
C       Root search
C
            CALL RPHNWT (RPHG2,S1,S2,S3,F1,F2,F3,C,CMIN,CMAX,SGNDG,IERR,
     *         IOPPRN)
            IF (IERR.EQ.0) THEN
C
C    check that C is greater than zero
C
               IF (C.LE.0.0D0) THEN
                  IERR = 2
               ELSE
C
C    Compute A and B
C
                  EX2 = F3*EXP(-C*S2N)
                  T = EXP(-C*S3N)
                  EX3 = F2*T
                  B = (S3*EX3-S2*EX2)/(EX3-EX2)
                  A = F3/((S3-B)*T)
C
C    check that A > 0  and B < S2
C
                  IF (A.LE.0.0D0.OR.(F1.GT.0.0D0.AND.B.GT.S1).OR.(F1.LE.
     *               0.0D0.AND.(B.LT.S1.OR.B.GT.S2))) IERR = -1
               ENDIF
            ENDIF
            IF (IERRO.EQ.0.OR.(IERR.LE.0.AND.IERRO.GT.0)) THEN
               IERRO = IERR
               AO = A
               BO = B
               CO = C
            ENDIF
            IF (IERR.NE.0.AND.IROOT.LT.NROOT) THEN
C
C    A second root should be tried, set up limits, and reset
C
               CMIN = 0.0D0
               CMAX = CX
               C = 0.5D0*(CMIN+CMAX)
               G = RPHG2(C,S1,S2,S3,F1,F2,F3,DG)
               IERR = 0
            ENDIF
C
C    check if looping back for second root
C
            IF (IERRO.NE.0.AND.IROOT.LT.NROOT) GO TO 50
            IF (IERR.NE.0.AND.IERRO.LT.0) THEN
               IERR = IERRO
               A = AO
               B = BO
               C = CO
               WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
               IF (IERR.EQ.1) WRITE (FU6,2400)
               IF (IERR.EQ.2) WRITE (FU6,1500)
               IF (IERR.EQ.-1) WRITE (FU6,1900) A,B,C
            ENDIF
C
C    change sign of A to have sign of SGNS*SGNF, sign of B to have sign
C    of SGNS, and sign of C to sign of SGNS
C
            A = SGNS*SGNF*A
            B = SGNS*B
            IF (NEXP.NE.2) C = SGNS*C
         ENDIF
C
C ======================================================================
C
      ELSEIF (IOP.LE.7) THEN
C
C fit F = A*((S-B)**2)*EXP(-C*S)
C
C    for this fit F1 must be positive
C
         IF (F1.LT.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
            WRITE (FU6,2000)
            IERR = 1
         ELSE
            NEXP = IOP-4
            S1N = S1**NEXP
            S2N = S2**NEXP
            S3N = S3**NEXP
            TF1 = SQRT(F1)
            TF2 = SQRT(F2)
            TF3 = SQRT(F3)
C
C    Loop over three different combinations of signs
C
            IERRO = 0
            DO 70 KSIGN = 1, 3
               IF (KSIGN.EQ.1) THEN
                  SIGN1 = -1.0D0
                  SIGN2 = -1.0D0
               ELSEIF (KSIGN.EQ.2) THEN
                  SIGN1 = 1.0D0
                  SIGN2 = -1.0D0
               ELSE
                  SIGN1 = -1.0D0
                  SIGN2 = 1.0D0
               ENDIF
C
C    Root search for G(C)=0
C       Find minimum and evaluate G at the minimum
C
               IF (KSIGN.LT.3) THEN
                  CX = 2.0D0*LOG(TF2*(S3-S1)*(S2N-S1N)/(TF3*(S2-S1)*(S3N
     *               -S1N)))/(S3N-S2N)
                  CX = MAX(CX,0.0D0)
               ELSE
                  CX = 0.0D0
               ENDIF
               G = RPHG5(CX,S1,S2,S3,TF1,TF2,TF3,DG)
               IF (IOPPRN.GT.1) WRITE (FU6,3500) KSIGN,SIGN1,SIGN2,CX,
     *                                           G,DG
               IF (G.GT.0.0D0) THEN
C
C    minimum occurs for a  positive G, therefore no root exists
C
                  IERR = 2
               ELSE
                  IF (KSIGN.LT.3) THEN
C
C       get value of G(C=0).
C
                     C = 0.0D0
                     G0 = RPHG5(C,S1,S2,S3,TF1,TF2,TF3,DG)
                     IF (IOPPRN.GT.1) WRITE (FU6,3200) C,G0,DG
C
C       If G0 > 0, two roots exist, try to find the one for C>Cx first,
C          and if it doesn't work then try to find one for C<Cx.
C
                     IF (G0.GT.0.0D0) THEN
                        NROOT = 2
                     ELSE
                        NROOT = 1
                     ENDIF
                  ELSE
                     NROOT = 1
                  ENDIF
C
C       initial guess at C
C
                  IF (KSIGN.LT.3) THEN
                     C = 2.0D0*LOG(TF2*(S3-S2)/(TF3*(S2-S1)))/(S3N-S2N)
                     IF (C.LT.CX) C = CX+0.5D0
                  ELSE
                     C = 0.0D0
                  ENDIF
                  G = RPHG5(C,S1,S2,S3,TF1,TF2,TF3,DG)
                  IF (IOPPRN.GT.1) WRITE (FU6,3300) C,G,DG
C
C       if G > 0, C is an upper limit, otherwise a lower limit
C
                  IF (G.GT.0.0D0) THEN
                     CMIN = CX
                     CMAX = C
                  ELSE
                     CMIN = C
                     CMAX = 1.0D+30
                  ENDIF
                  SGNDG = SIGN(1.0D0,DG)
C
C       loop over roots starts here
C
                  IROOT = 0
   60             CONTINUE
                  IROOT = IROOT+1
C
C       Root search
C
                  IF (IOPPRN.GT.1) WRITE (FU6,3600) IROOT
                  CALL RPHNWT (RPHG5,S1,S2,S3,TF1,TF2,TF3,C,CMIN,CMAX,
     *               SGNDG,IERR,IOPPRN)
                  IF (IERR.EQ.0) THEN
C
C    check that C is greater than zero
C
                     IF (C.LE.0.0D0) THEN
                        IERR = 3
                     ELSE
C
C    Compute A and B
C
                        EX2 = TF3*EXP(-0.5D0*C*S2N)
                        T = EXP(-C*S3N)
                        EX3 = SIGN2*SQRT(F2*T)
                        B = (S2*EX2+S3*EX3)/(EX2+EX3)
                        T = (S3-B)*(S3-B)*T
                        A = F3/T
C
C    check that A>0
C
                        IF (A.LE.0.0D0.OR.B.GT.S1) IERR = -1
                     ENDIF
                  ENDIF
                  IF (IERRO.EQ.0.OR.(IERR.LE.0.AND.IERRO.GT.0)) THEN
                     IERRO = IERR
                     AO = A
                     BO = B
                     CO = C
                     KSIGNO = KSIGN
                  ENDIF
                  IERRX = IERR
                  IF (IERR.NE.0.AND.IROOT.LT.NROOT) THEN
C
C    A second root should be tried, set up limits, and reset IERR
C
                     CMIN = 0.0D0
                     CMAX = CX
                     C = 0.5D0*(CMIN+CMAX)
                     G = RPHG5(C,S1,S2,S3,TF1,TF2,TF3,DG)
                     SGNDG = SIGN(1.0D0,DG)
                     IERR = 0
                  ENDIF
C
C    check if looping back for second root
C
                  IF (IERRX.NE.0.AND.IROOT.LT.NROOT) GO TO 60
               ENDIF
               IF (IERR.EQ.0) GO TO 80
   70       CONTINUE
   80       CONTINUE
            IF (IERR.NE.0.AND.IERRO.LT.0) THEN
               IERR = IERRO
               A = AO
               B = BO
               C = CO
               KSIGN = KSIGNO
               WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
               IF (IERR.EQ.2) WRITE (FU6,1800) KSIGN
               IF (IERR.EQ.3) WRITE (FU6,1500) KSIGN
               IF (IERR.EQ.-1) WRITE (FU6,2100) A,B,C,KSIGN
            ENDIF
C
C    change sign of A to have sign of SGNS*SGNF, sign of B to have sign
C    of SGNS, and sign of C to sign of SGNS
C
            A = SGNF*A
            B = SGNS*B
            IF (NEXP.NE.2) C = SGNS*C
         ENDIF
C
C ======================================================================
C
      ELSEIF (IOP.LE.10) THEN
C
C fit F = A*(S**B)*EXP(-C*S**N)
C
C    for this fit S1 and F1 must be positive
C
         IF (F1.LE.0.0D0.OR.S1.LE.0.0D0) THEN
            WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
            WRITE (FU6,2200)
            IERR = 1
         ELSE
            NEXP = IOP-7
            S1N = S1**NEXP
            S2N = S2**NEXP
            S3N = S3**NEXP
            XLS3 = LOG(S3/S2)
            XLS2 = LOG(S2/S1)
            XLF3 = LOG(F3/F2)
            XLF2 = LOG(F2/F1)
            T = (XLS3*(S2N-S1N)-XLS2*(S3N-S2N))
            C = (XLS2*XLF3-XLS3*XLF2)/T
            B = ((S2N-S1N)*XLF3-(S3N-S2N)*XLF2)/T
            A = F3*EXP(C*S3N)/(S3**B)
C
C    check that C > 0
C
            IF (C.LE.0.0D0) THEN
               WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
               WRITE (FU6,1500)
               IERR = 1
            ELSEIF (A.LE.0.0D0) THEN
               WRITE (FU6,1000) IOP,SGNS,S1,S2,S3,SGNF,FASY,F1,F2,F3
               WRITE (FU6,2300) A,B,C
               IERR = -1
            ENDIF
C
C    change sign of A to have sign of SGNF and sign of C to sign of SGNS
C
            A = SGNF*A
            IF (NEXP.NE.2) C = SGNS*C
         ENDIF
C
C ======================================================================
C
C    To add a new functional form, the proper ELSE IF statement for the
C    new option number must be placed here.  This is followed by the
C    necessary code to evaluate the parameters of the functional
C    form in terms of the function values at 3 s values.
C
C ======================================================================
C
      ELSE
C
C Bad option number
C
         WRITE (FU6,3800) IOP
         STOP 'RPHFT3 1'
      ENDIF
      IF (IOPPRN.GT.0) WRITE (FU6,3700) A,B,C,IERR
      RETURN
C
 1000 FORMAT (1X, 78(1H*)/ ' RPHFT3, IOP=', I5/ 1X, F5.0, '*S=', 16X,
     *   1P,3E13.5/ 1X, 0PF5.0, '*(F-', 1PE13.5, ')=', 1P,3E13.5)
 1100 FORMAT (' S AND/OR F IS NOT ORDERED CORRECTLY')
 1200 FORMAT (' FOR IOP=1, C=', F20.10, ', BUT G>0.')
 1300 FORMAT (' FOR IOP=1, DG<0 FOR C=0.')
 1400 FORMAT (' FOR IOP=1, DG>0 FOR C=0, BUT CANNOT FIND C WHERE G>0.')
 1500 FORMAT (' C < 0.' : ' FOR KSIGN=', I5)
 1600 FORMAT (' A <= 0.'/ ' A,B,C=', 1P,3E13.5)
 1700 FORMAT (' B IS NOT IN THE CORRECT RANGE FOR F1<= 0.')
 1800 FORMAT (' G IS POSITIVE AT LOCAL MINIMUM.' : ' FOR KSIGN=', I5)
 1900 FORMAT (' A <= 0 AND/OR B IS NOT IN CORRECT RANGE.'/
     *   ' A,B,C=', 1P,3E13.5)
 2000 FORMAT (' F1<0')
 2100 FORMAT (' A <= 0 AND/OR B > S1.'/ ' A,B,C=', 1P,3E13.5 :
     *   ' FOR KSIGN=', I5)
 2200 FORMAT (' F1<=0 AND/OR S1<=0')
 2300 FORMAT (' A <= 0 AND/OR B <= 0.'/ ' A,B,C=', 1P,3E13.5)
 2400 FORMAT (' ROOT SEARCH NOT CONVERGED')
 2500 FORMAT (' Increase C until G(C) becomes negative'/ 4X, 'IC', T10,
     *   'C', T25, 'G(C)')
 2600 FORMAT (1X, I5, 1P,2E15.7)
 2700 FORMAT (' Decrease C until G(C) becomes positive'/ T10,
     *   'C', T25, 'G(C)')
 2800 FORMAT (5X, 1P,3E15.7)
 2900 FORMAT (' Increase C until DG becomes negative'/ T10,
     *   'C', T25, 'G(C)')
 3000 FORMAT (' Root search using RPHG1')
 3100 FORMAT (' CX, G, DG=', 1P,3E15.7)
 3200 FORMAT (' C, G0, DG=', 1P,3E15.7)
 3300 FORMAT (' C0, G, DG=', 1P,3E15.7)
 3400 FORMAT (' Root search using RPHG2, IROOT=', I5)
 3500 FORMAT (' KSIGN,SIGN1,SIGN2=', I5, 2F10.5/ ' CX,G,DG=',1P,3E15.7)
 3600 FORMAT (' Root search using RPHG5, IROOT=', I5)
 3700 FORMAT (' A, B, C, IERR=', 1P,3E15.7, I10)
 3800 FORMAT (1X, I3, ' is not allowed as an option number for the',
     *   ' asymptotic exponential fits.')
C
      END SUBROUTINE rphft3
C
C***********************************************************************
C  RPHG1
C***********************************************************************
C
      FUNCTION rphg1 (C,S1,S2,S3,F1,F2,F3,DG)
C
C Function used in root search for the range parameter in the
C    asymptotic functional form for option number 1.
C    Written Oct. '85.
C
C    CALLED BY:
C               RPHFT3
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DS32 = S3-S2
      DS21 = S2-S1
      DS31 = S3-S1
      EX32 = EXP(C*DS32)
      EX21 = EXP(C*DS21)
      EX31 = EXP(C*DS31)
      G = F3*(EX32-EX31)+F2*(EX21-1.0D0/EX32)+F1*(1.0D0/EX31-1.0D0/EX21)
      RPHG1 = G
      DG = F3*(DS32*EX32-DS31*EX31)+F2*(DS21*EX21+DS32/EX32)+F1*(DS21/
     *   EX21-DS31/EX31)
      RETURN
      END FUNCTION rphg1
C
C***********************************************************************
C  RPHG2
C***********************************************************************
C
C   COMMON BLOCKS MODIFIED 6/19/91
C
      FUNCTION rphg2 (C,S1,S2,S3,F1,F2,F3,DG)
      use perconparam
      use rate_const, only : nexp
C
C Function used in root search for the range parameter in the
C    asymptotic functional form for option numbers 2, 3, and 4.
C    Written Oct. '85.
C
C    CALLED BY:
C               RPHNWT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      S1N = S1**NEXP
      S2N = S2**NEXP
      S3N = S3**NEXP
      DS21 = S2N-S1N
      DS31 = S3N-S1N
      EX21 = EXP(C*DS21)
      EX31 = EXP(C*DS31)
      G = F1*(S3-S2)
      T = F2*(S3-S1)*EX21
      G = G-T
      DG = -T*DS21
      T = F3*(S2-S1)*EX31
      G = G+T
      DG = DG+T*DS31
      RPHG2 = G
      RETURN
      END function rphg2
C
C***********************************************************************
C  RPHG5
C***********************************************************************
C
C    COMMON BLOCKS MODIFIED 6/19/91
C
      FUNCTION rphg5 (C,S1,S2,S3,F1,F2,F3,DG)
      use perconparam, only : fu6
      use rate_const, only : sign1,sign2,nexp
C
C Function used in root search for the range parameter in the
C    asymptotic functional form for option numbers 5, 6, and 7.
C    Written Oct. '85.
C
C    CALLED BY:
C               RPHNWT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      S1N = S1**NEXP
      S2N = S2**NEXP
      S3N = S3**NEXP
      DS21 = S2N-S1N
      DS31 = S3N-S1N
      EX21 = EXP(0.5D0*C*DS21)
      EX31 = EXP(0.5D0*C*DS31)
      G = SIGN1*SIGN2*F1*(S3-S2)
      T = SIGN2*F2*(S3-S1)*EX21
      G = G+T
      DG = T*DS21
      T = F3*(S2-S1)*EX31
      G = G+T
      DG = DG+T*DS31
      DG = 0.5D0*DG
      RPHG5 = G
      RETURN
      END FUNCTION rphg5
C
C***********************************************************************
C  RPHINT
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE rphint (IOP)
      use perconparam
      use rate_const
      use common_inc
      use kintcm
      use keyword_interface, only : gufac6,iunit6
C
C  Interpolate RPH information. Written Oct. '85. BCG
C     IOP = 1, for reactants
C         = 2, for products
C         > 2, along MEP
C
C     CALLED BY:
C                RPHSET,REACT,NORMOD,PATH
C     CALLS:
C           RPHEXP,RPHAIT,ANHARM,ZEROPT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  For the interpolation, e will ise the absolute value of the curvature 
C  components instead of the signed values. So, we will store the signed BFS 
C  array in the BFSTMP array and we will make BFS = abs (BFS)
C  This is done for avoiding problems in the interpolation related to 
C  the change in sign of BFS.
C
      SAVE ICONTH                                                       0804JC97
      SAVE NPSM                                                         0810JC97
      SAVE ISPSM                                                        0810JC97
      SAVE SSM                                                          0810JC97
      SAVE NPSV                                                         0810JC97
      SAVE ISPSV                                                        0810JC97
      SAVE SSV                                                          0810JC97
      DIMENSION XROT(N3TM),XREF(N3TM)                                   0804JC97
!      DIMENSION SSM(2,NPT31), SSV(2,NPT31)                              0810JC97
      DIMENSION BFSTMP(NSDIM,NVIBM),TMPRMI(NVIBM),idum(natoms)
      real(8), allocatable :: ssm(:,:),ssv(:,:)
C
      if(.not.allocated(ssm))then
        NPT31 = INMM*NSDM
        allocate(ssm(2,npt31),ssv(2,npt31))
        ssm=0.d00; ssv=0.d00
      end if
      if(.not.allocated(xxext))then
        allocate(xxext(n3tm+1,2));xxext=0.d00
      end if
C
      DO I=1,NSDIM                                                      0321JC97
       DO J=1,NVIBM                                                     0321JC97
        BFSTMP(I,J)=BFS(I,J)                                            0321JC97
        BFS(I,J)=ABS(BFS(I,J))                                          0321JC97
       ENDDO                                                            0321JC97
      ENDDO                                                             0321JC97
C 
      DO J=1,NVIBM                                                      0925JC97
        TMPRMI(J)=0.D0                                                  0925JC97
      ENDDO                                                             0925JC97
C
      IF (ICODE(5).LT.0) THEN                                           11/20T87
         ISHFT =  1                                                        ..
      ELSE IF (ICODE(5) .EQ. 3) THEN                                       ..
         ISHFT =  6                                                        ..
      ELSE                                                                 ..
         ISHFT =  7                                                        ..
      ENDIF                                                             11/20T87
      NFREQ = NF(5)                                                     9/18YL92
      ISHFT = N3-NF(5)                                                  9/18YL92
C
      IF (IOP.EQ.1) THEN
C
C Reactants
C
         V = 0.0D0
         DO 10 I = 1, NFREQ
            FREQ(I+ISHFT) = WS(1,I)
            XK3(I) = XK3S(1,I)
            XK4(I) = XK4S(1,I)
            BCURV(I) = 0.0D0
            FMOMHR(I+ISHFT) = FMIRS(1,I)                                0925JC97
   10    CONTINUE
         IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                           6/5S89
            FMOM(5) = 0.0D0
         ELSE
            FMOM(5) = 1.0D0/FMOMS(1)
         ENDIF
C
      ELSEIF (IOP.EQ.2) THEN
C
C Products
C
         V = VS(NSDIM)
         DO 20 I = 1, NFREQ
            FREQ(I+ISHFT) = WS(NSDIM,I)
            XK3(I) = XK3S(NSDIM,I)
            XK4(I) = XK4S(NSDIM,I)
            BCURV(I) = 0.0D0
            FMOMHR(I+ISHFT) = FMIRS(NSDIM,I)                            0925JC97
   20    CONTINUE
         IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN                           6/5S89
            FMOM(5) = 0.0D0
         ELSE
            FMOM(5) = 1.0D0/FMOMS(NSDIM)
         ENDIF
C
      ELSE
       IF (ABS(LOPT(2)).NE.500) THEN                                    0804JC97
C
C Along MEP
C
         IF ((LGS(6).EQ.1.OR.LGS(6).EQ.2) .AND. S.LT.SS(3). AND
     *       .IREPR(7).EQ.0) THEN                                       1126JC97
C
C    Exponential interpolation in reactant region
C
           IF(IFITVR.GT.0) THEN                                         07/95KAN
            CALL RPHEXP (IFITVR,AVR,VS(1),S,T)
            V = T
           ENDIF                                                        07/95KAN
            DO 30 I = 1, NFREQ
              IF(IFITWR(I).GT.0) THEN                                   07/95KAN
               CALL RPHEXP (IFITWR(I),AWR(1,I),WS(1,I),S,FREQ(I+ISHFT))
              ENDIF                                                     07/95KAN
               IF (LOPT(5).GT.0) THEN
                  CALL RPHEXP (IFTK3R(I),AK3R(1,I),XK3S(1,I),S,XK3(I))
               ELSE
                  XK3(I) = 0.0D0
               ENDIF
               IF (LOPT(5).GT.1) THEN
                  CALL RPHEXP (IFTK4R(I),AK4R(1,I),XK4S(1,I),S,XK4(I))
               ELSE
                  XK4(I) = 0.0D0
               ENDIF
               IF (LOPT(6).GT.0) THEN
                  CALL RPHEXP (IFITWR(I),ABFR(1,I),BFS(1,I),S,BCURV(I))
               ELSE
                  BCURV(I) = 0.0D0
               ENDIF
               IF (LGS(5).GE.21.AND.MODE(I).EQ.9) THEN                  0925JC97
                  CALL RPHEXP (IFITVR,AMIR(1,I),0.D0,S,T)               0925JC97
                  TMPRMI(I) = 1.D0+30                                   0925JC97
                  IF (T.GT.0.D0) TMPRMI(I) = 1.0D0/T                    0925JC97
               ENDIF                                                    0925JC97
   30       CONTINUE
            CALL RPHEXP (IFITVR,AFIR(1),FMOMS(1),S,T)
            IF (T.GT.0.0D0) THEN
               FMOM(5) = 1.0D0/T
            ELSE
               FMOM(5) = 1.0D+30
            ENDIF
C
           ELSEIF ((LGS(6).EQ.1.OR.LGS(6).EQ.3).AND.S.GT.SS(NSS-2).
     *              AND.IREPR(8).EQ.0) THEN                             1126JC97
C
C    Exponential interpolation in product region
C
            CALL RPHEXP (IFITVP,AVP,VS(NSS),S,T)
            V = T
            DO 40 I = 1, NFREQ
               CALL RPHEXP (IFITWP(I),AWP(1,I),WS(NSS,I),
     *                      S,FREQ(I+ISHFT))
               IF (LOPT(5).GT.0) THEN
                  CALL RPHEXP (IFTK3P(I),AK3P(1,I),XK3S(NSS,I),S,XK3(I))
               ELSE
                  XK3(I) = 0.0D0
               ENDIF
               IF (LOPT(5).GT.1) THEN
                  CALL RPHEXP (IFTK4P(I),AK4P(1,I),XK4S(NSS,I),S,XK4(I))
               ELSE
                  XK4(I) = 0.0D0
               ENDIF
               IF (LOPT(6).GT.0) THEN
                  CALL RPHEXP (IFITWP(I),ABFP(1,I),BFS(NSS,I),
     *                         S,BCURV(I))
               ELSE
                  BCURV(I) = 0.0D0
               ENDIF
               IF (LGS(5).GE.21.AND.MODE(I).EQ.9) THEN                  0925JC97
                  CALL RPHEXP (IFITVP,AMIP(1,I),0.D0,S,T)               0925JC97
                  TMPRMI(I) = 1.D0+30                                   0925JC97
                  IF (T.GT.0.D0) TMPRMI(I) = 1.0D0/T                    0925JC97
               ENDIF                                                    0925JC97
   40       CONTINUE
            CALL RPHEXP (IFITVP,AFIP(1),FMOMS(NSS),S,T)
            FMOM(5) = 1.0D+30
            IF (T.GT.0.0D0) FMOM(5) = 1.0D0/T
         ELSE
C
C Locate s in grid
C
            I0 = 2                                                      1126JC97
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                        1126JC97
                IF (IREPR(7).EQ.0) I0 = 3                               1126JC97
            ELSE                                                        1126JC97
                IF (IREPR(7).EQ.0) THEN                                 1126JC97
                    SS(2)=SS(1)                                         1126JC97
                    VS(2)=VS(1)                                         1126JC97
                    FMOMS(2)=FMOMS(1)                                   1126JC97
                    DO 42 I=1,NFREQ                                     1126JC97
                       WS(2,I)=WS(1,I)                                  1126JC97
                       XK3S(2,I)=XK3S(1,I)                              1126JC97
                       XK4S(2,I)=XK4S(1,I)                              1126JC97
                       BFS(2,I)=BFS(1,I)                                1126JC97
                       FMIRS(2,I)=FMIRS(1,I)                            1126JC97
42                  CONTINUE                                            1126JC97
                ENDIF                                                   1126JC97
            ENDIF                                                       1126JC97
            N = NSS-1                                                   1126JC97
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN                        1126JC97
                IF (IREPR(8).EQ.0) N = NSS-2                            1126JC97
            ELSE                                                        1126JC97
                IF (IREPR(8).EQ.0) THEN                                 1126JC97
                    SS(NSS-1)=SS(NSS)                                   1126JC97
                    VS(NSS-1)=VS(NSS)                                   1126JC97
                    FMOMS(NSS-1)=FMOMS(NSS)                             1126JC97
                    DO 45 I=1,NFREQ                                     1126JC97
                       WS(NSS-1,I)=WS(NSS,I)                            1126JC97
                       XK3S(NSS-1,I)=XK3S(NSS,I)                        1126JC97
                       XK4S(NSS-1,I)=XK4S(NSS,I)                        1126JC97
                       BFS(NSS-1,I)=BFS(NSS,I)                          1126JC97
                       FMIRS(NSS-1,I)=FMIRS(NSS,I)                      1126JC97
45                  CONTINUE                                            1126JC97
                ENDIF                                                   1126JC97
            ENDIF                                                       1126JC97
            DO 50 I = 3, N                                              0730PF97
               IS = I
               IF (S.LT.SS(IS)) GO TO 60
   50       CONTINUE
   60       CONTINUE
C
C NINT1-pt. Lagrange interpolation
C
            ISI = IS-NINTH
            ISI = MAX(I0,ISI)
            ISI = MIN(ISI,N-NINT1+1)
            CALL RPHAIT (VS(ISI),SS(ISI),S,T,NINT1)
            V = T
            DO 70 I = 1, NFREQ
               CALL RPHAIT (WS(ISI,I),SS(ISI),S,FREQ(I+ISHFT),NINT1)
               IF (LOPT(5).GT.0) THEN
                  CALL RPHAIT (XK3S(ISI,I),SS(ISI),S,XK3(I),NINT1)
               ELSE
                  XK3(I) = 0.0D0
               ENDIF
               IF (LOPT(5).GT.1) THEN
                  CALL RPHAIT (XK4S(ISI,I),SS(ISI),S,XK4(I),NINT1)
               ELSE
                  XK4(I) = 0.0D0
               ENDIF
               IF (LOPT(6).GT.0) THEN
                  CALL RPHAIT (BFS(ISI,I),SS(ISI),S,BCURV(I),NINT1)
               ELSE
                  BCURV(I) = 0.0D0
               ENDIF
               IF (LGS(5).GE.21.AND.MODE(I).EQ.9) THEN                  0925JC97
                  CALL RPHAIT (FMIRS(ISI,I),SS(ISI),S,TMPRMI(I),NINT1)  0925JC97
               ENDIF                                                    0925JC97
   70       CONTINUE
            CALL RPHAIT (FMOMS(ISI),SS(ISI),S,T,NINT1)
            FMOM(5) = 1.0D+30
            IF (T.GT.0.0D0) FMOM(5) = 1.0D0/T
         ENDIF
        ELSE                                                            0804JC97
C
C    Spline MIVTST
C
           if (iconth.ne.1) then
             write (fu6,*) 'IVTST-M calculation running'
             iconth=1
             if (lgs(30).eq.3) then
              call arrsm(npsm,ispsm,ssm)
              call arrsv(npsv,ispsv,ssv)
             endif
c1->2
             if (lgs(6).eq.3) then
                   do i=1,n3tm
                      xrot(i)=xr(i,1)
                      if (lgs(30).ne.3) then
                        xref(i)=xxs(i,3)
                      else
                        xref(i)=xxext(i+1,1)
                      endif
                   enddo
                   if (lgs(30).ne.3) then
                        sref=ss(3)
                   else
                        sref=xxext(1,1)
                   endif
                   sval=0.D0
                   call calcs(xref,xrot,sval,idum)
                   ss(1)=-sval+sref
                   write (fu6,2200)  ss(1)
c2->1
             else if (lgs(6).eq.2.and.irepr(3).eq.1) then               0317Yc99
                   do i=1,n3tm
                     xrot(i)=xr(i,3)
                     if (lgs(30).ne.3) then
                        xref(i)=xxs(i,nss-2)
                     else
                        xref(i)=xxext(i+1,2)
                     endif
                   enddo
                   if (lgs(30).ne.3) then
                        sref=ss(nss-2)
                   else
                        sref=xxext(1,2)
                   endif
                   sval=0.D0
                   call calcs(xref,xrot,sval,idum)
                   ss(nss)=sval+sref
                   ss(nsdim)=sval+sref
                   write (fu6,2100)  ss(nss)
c1->1
             else if (lgs(6).eq.4.and.irepr(3).eq.1) then               0317Yc99
                  do i=1,n3tm
                    xrot(i)=xr(i,1)
                    if (lgs(30).ne.3) then
                        xref(i)=xxs(i,3)
                    else
                        xref(i)=xxext(i+1,1)
                    endif
                    if (lgs(30).ne.3) then
                        sref=ss(3)
                    else
                        sref=xxext(1,1)
                    endif
                   enddo
                   sval=0.D0
                   call calcs(xref,xrot,sval,idum)
                   ss(1)=-sval+sref
                   write (fu6,2200)  ss(1)
                  do i=1,n3tm
                    xrot(i)=xr(i,3)
                    if (lgs(30).ne.3) then
                        xref(i)=xxs(i,nss-2)
                    else
                        xref(i)=xxext(i+1,2)
                    endif
                    if (lgs(30).ne.3) then
                        sref=ss(nss-2)
                    else
                        sref=xxext(1,2)
                    endif
                  enddo
                   sval=0.D0
                   call calcs(xref,xrot,sval,idum)
                   ss(nss)=sval+sref
                   ss(nsdim)=sval+sref
                   write (fu6,2100)  ss(nss)
             endif
             if (irepr(7).eq.1) then
                   do i=1,n3tm
                      xrot(i)=xr(i,7)
                      if (lgs(30).ne.3) then
                        xref(i)=xxs(i,3)
                      else
                        xref(i)=xxext(i+1,1)
                      endif
                   enddo
                   if (lgs(30).ne.3) then
                        sref=ss(3)
                   else
                        sref=xxext(1,1)
                   endif
                   sval=0.D0
                   call calcs(xref,xrot,sval,idum)
                   ss(2)=-sval+sref
                   write (fu6,2300)  ss(2)
             endif
             if (irepr(8).eq.1) then
                   do i=1,n3tm
                     xrot(i)=xr(i,8)
                     if (lgs(30).ne.3) then
                        xref(i)=xxs(i,nss-2)
                     else
                        xref(i)=xxext(i+1,2)
                     endif
                   enddo
                   if (lgs(30).ne.3) then
                        sref=ss(nss-2)
                   else
                        sref=xxext(1,2)
                   endif
                   sval=0.D0
                   call calcs(xref,xrot,sval,idum)
                   ss(nss-1)=sval+sref
                   ss(nsdim-1)=sval+sref
                   write (fu6,2400)  ss(nss-1)
             endif
             if (LCDSC.and.LOPT(2).eq.-500) call armuef
c                                                                       0317Yc99
c up date data if there is no product or product well                    ..
c take the last point on the path as product                             ..
c
        if (irepr(3).ne.1.and.irepr(8).ne.1) then                        ..
          write (fu6,*) '  No product found, use the last point',        ..
     >       ' in the reaction path instead'                             ..
          ss(nsdim)=ss(nss-2)                                            ..
          ss(nss)=ss(nsdim)                                              ..
          vs(nsdim)=vs(nss-2)                                            ..
          vs(nss)=vs(nsdim)                                              ..
          do j = 1, nfreq                                                ..
            ws(nsdim,j)=ws(nss-2,j)                                      ..
            ws(nss,j)=ws(nsdim,j)                                        ..
            IF (LGS(5).GT.0.OR.LGS(33).EQ.1) THEN                        ..
              xk3s(nsdim,j) = xk3s(nss-2,j)                              ..
              xk3s(nss,j) = xk3s(nsdim,j)                                ..
              xk4s(nsdim,j) = xk4s(nss-2,j)                              ..
              xk4s(nss,j) = xk4s(nsdim,j)                                ..
            ENDIF                                                        ..
            fmirs(nsdim,j)= fmirs(nss-2,j)                               ..
            fmirs(nss,j) = fmirs(nsdim,j)                                ..
            bfs(nsdim,j) = bfs(nss-2,j)                                  ..
            bfs(nss,j) = bfs(nsdim,j)                                    ..
          enddo                                                          ..
          fmoms(nsdim) = fmoms(nss-2)                                    ..
          fmoms(nss) = fmoms(nsdim)                                      ..
        endif                                                            ..
        write (fu6,*)
        write (fu6,*) '     REACTANT  ',' REACTANTW ',' SADDLE PT ',     ..
     >                '   PRODUCTW  ','  PRODUCT  '                      ..
        write (fu6,*) '     --------  ',' --------- ',' --------- ',     ..
     >                '   --------  ','  -------  '                      ..
  6     format (5F12.3)                                                  ..
C       write (fu6,*) '  S (bohr)'                                       ..
C       write (fu6,6) ss(1),ss(2),ss(nsdim-2),ss(nsdim-1),ss(nsdim)      ..
        if(iunit6.eq.0) write (fu6,*) '  S (angstrom)'                  0405JZ07
        if(iunit6.eq.1) write (fu6,*) '  S (bohr)'                      0405JZ07
        write (fu6,6) ss(1)/gufac6,ss(2)/gufac6,ss(nsdim-2)/gufac6,
     >                ss(nsdim-1)/gufac6,ss(nsdim)/gufac6               0405JZ07
        write (fu6,*) '  V (kcal/mol)'                                   ..
        write (fu6,6) vs(1)*CKCAL,vs(2)*CKCAL,vs(nsdim-2)*CKCAL,         ..
     >                vs(nsdim-1)*CKCAL,vs(nsdim)*CKCAL                  ..
        write (fu6,*) '  Vibrational frequencies (cm-1)'                 ..
        do i = 1,nfreq                                                   ..
         write (fu6,6) ws(1,i)*AUTOCM,ws(2,i)*AUTOCM,                    ..
     >                 ws(nsdim-2,i)*AUTOCM,                             ..
     >                 ws(nsdim-1,i)*AUTOCM,ws(nsdim,i)*AUTOCM           ..
        enddo                                                            ..
        write (fu6,*) '  Moment of Inertia in a.u.'                      ..
  8     format (5E12.5)                                                  ..
        write (fu6,8) fmoms(1),fmoms(2),fmoms(nsdim-2),fmoms(nsdim-1),   ..
     >                fmoms(nsdim)                                      0317Yc99 
             endif
             xmfr=1.D10
             xmfp=1.D10
             do i=1,nfreq
               call splnf(nss,wstar,redm,ss,vs,ws,s,t,i,lgs(6),
     *                        irepr,tension)                            0911JZ08
               freq(i+ishft)=t
               if (lopt(5).gt.0) then
                 call splnf(nss,wstar,redm,ss,vs,xk3s,s,t,i,lgs(6),
     *                        irepr,tension)                            0911JZ08
                 xk3(i)=t
               else
                 xk3(i) = 0.0d0
               endif
               if (lopt(5).gt.1) then
                 call splnf(nss,wstar,redm,ss,vs,xk4s,s,t,i,lgs(6),
     *                        irepr,tension)                            0911JZ08
                 xk4(i)=t
               else
                 xk4(i) = 0.0d0
               endif
               if (isct.ne.0.and.lopt(6).gt.0) then
                  call splnb(nss,wstar,redm,ss,vs,bfs,s,t,i,lgs(6),
     *                        irepr,lbexp,tension)                      0911JZ08
                  bcurv(i)=t
               else
                  bcurv(i) = 0.0d0
               endif
               IF (LGS(5).GE.21.AND.MODE(I).EQ.9) THEN                  0926JC97
                  CALL splrmi(nss,wstar,redm,ss,vs,fmirs,s,t,i,lgs(6),  0926JC97
     *                                                         irepr,   0926JC97
     *                                                       tension)   0911JZ08
                  tmprmi(i)=t                                           0926JC97
               ENDIF                                                    0926JC97
c
c We are looking for the lowest frequency at reactants and products
c Nevertheless, some zero frequencies are not exactly zero, but a small
c number (or a negative number indicating an imaginary negligible
c erroneus frequency). Therefore, we will select as lowest frequency
c the lowest value bigger than alow, which is the value of a frequency
c that should be zero strictly speaking, but is non-zero in the calculation.
c
               alow=50.D0*CMTOAU
c
               if (irepr(7).eq.0) then
                 if (ws(1,i).lt.xmfr.and.ws(1,i).gt.alow) xmfr=ws(1,i)
               else
                 if (ws(2,i).lt.xmfr.and.ws(2,i).gt.alow) xmfr=ws(1,i)
               endif
               if (irepr(8).eq.0) then
                 if (ws(nss,i).lt.xmfp.and.ws(nss,i).gt.alow)
     *             xmfp=ws(nss,i)
               else
                 if (ws(nss-1,i).lt.xmfp.and.ws(nss-1,i).gt.alow)
     *             xmfp=ws(nss-1,i)
               endif
            enddo
c
c Make an array with all the s and v,
c and an array with all the s and 1/(moment of inertia)
c (only if using fu31) and interpolate using splines for
c fu31
c
            if (lgs(30).eq.3) then
              call splv31(npsv,nss,ispsv,issp,wstar,redm,ssv,ss,
     *                    vs,s,t,lgs(6),xmfr,xmfp,inm31,sincw,irepr)

              v = t
              call splm31(npsm,nss,wstar,redm,ss,vs,fmoms,
     *                    ssm,s,t,LGS(6),irepr)
              fmom(5)=1.D0/t
            else
              call splnv(nss,issp,wstar,redm,ss,vs,s,t,
     *                     lgs(6),xmfr,xmfp,irepr,tension)              0911JZ08
              v = t
              call splnm(nss,wstar,redm,ss,vs,fmoms,s,t,LGS(6),irepr,
     *                   tension)                                       0911JZ08
              fmom(5)=1.D0/t
            endif
C
C END OF SPLINE MIVTST
C                                                                       0804JC97
        ENDIF
         IF (LOPT(1).GT.4) THEN
            WRITE (FU6,1250) S,FMOM(5)                                  0623WH94
            WRITE (FU6,1300) (FREQ(I+ISHFT)*AUTOCM,BCURV(I),            0623WH94
     *                        XK3(I),XK4(I),I=NFREQ,1,-1)
         ENDIF
C 0925JC97         IF (LGS(5).GT.0) CALL ANHARM (IOP)
C 0925JC97         CALL ZEROPT (IOP)                 
         IF (LGS(5).GT.0) THEN                                          0925JC97
            CALL ANHARM (IOP)                                           0925JC97
C
C Return the stored values of the reduced moment of intertia            0925JC97
C (they were changed in a non consisten way in the call to              0925JC97
C anharm)                                                               0925JC97
C
            DO 80 I = 1,NFREQ                                           0925JC97
               IF (LGS(5).GE.21.AND.MODE(I).EQ.9) THEN                  0925JC97
                  IF (TMPRMI(I).GT.1.D0) THEN                           0925JC97
                     FMOMHR(I+ISHFT)=TMPRMI(I)                          0925JC97
                  ELSE                                                  0925JC97
                     FMOMHR(I+ISHFT)=1.0D+30                            0925JC97
                  ENDIF                                                 0925JC97
               ENDIF                                                    0925JC97
   80       CONTINUE                                                    0925JC97
         ENDIF                                                          0925JC97
         if (iop.ne.5) CALL ZEROPT (IOP)                                0317Yc99
      ENDIF
C
C  Now, we return the original signed values to BFS
C                                                                       0321JC97
      DO I=1,NSDIM                                                      0321JC97
       DO J=1,NVIBM                                                     0321JC97
        BFS(I,J)=BFSTMP(I,J)                                            0321JC97
       ENDDO                                                            0321JC97
      ENDDO                                                             0321JC97
C
      RETURN
C
 1250 FORMAT (/1X,'at s = ',F10.5,4X,'DET(I) = ',1P,E12.4,/1X,45('-'))  0623WH94
 1300 FORMAT (/1X,'Freq.(cm**-1)','     Bk      ','     XK3    ',
     *                            '     XK4     ',
     * //,(2X,0P,F10.2,2X,1P,3E12.4))
 2100 FORMAT (/1x,'Estimated value of the reaction coordinate at',
     *        ' products: s =', f12.5)
 2200 FORMAT (/1x,'Estimated value of the reaction coordinate at',
     *        ' reactants: s =', f12.5)
 2300 FORMAT (/1x,'Estimated value of the reaction coordinate at',
     *        ' reactant well: s =', f12.5)
 2400 FORMAT (/1x,'Estimated value of the reaction coordinate at',
     *        ' product well: s =', f12.5)
C
      END SUBROUTINE rphint
C***********************************************************************
C  RPHNWT
C***********************************************************************
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      SUBROUTINE rphnwt (RPHG,S1,S2,S3,F1,F2,F3,C,CMIN,CMAX,SGNDG,IERR,
     *   IOPPRN)
      use perconparam, only : fu6
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      EXTERNAL RPHG
C
C Newton search for zero of G(C); C passes initial guess in and root
C    out; CMIN and CMAX at lower and upper bounds on C; SGNDG is initial
C    sign of the derivative of G and cannot change during iterations.
C    Written Oct. '85.  BCG
C
C    CALLED BY:
C                RPHFT3
C
      IERR = 0
      IC = 0
      COLD = C
      IF (IOPPRN.GT.1) WRITE (FU6,1100)
C
C Loop over Newton iterations starts here.
C
   10 CONTINUE
      IC = IC+1
      G = RPHG(C,S1,S2,S3,F1,F2,F3,DG)
      IF (IOPPRN.GT.1) WRITE (FU6,1200) IC,C,G,DG,CMIN,CMAX
      IF (DG*SGNDG.LT.0.0D0) THEN
C
C    Sign of DG changed, reset CMIN or CMAX and use mean as next guess.
C
         IF (C.LT.COLD) THEN
            CMIN = MAX(COLD,CMIN)
         ELSE
            CMAX = MIN(COLD,CMAX)
         ENDIF
         C = 0.5D0*(CMIN+CMAX)
         G = 1.0D0
      ELSE
C
C    Reset CMIN or CMAX
C
         IF (G*DG.GT.0.0D0) THEN
            CMAX = MIN(CMAX,C)
         ELSE
            CMIN = MAX(CMIN,C)
         ENDIF
         COLD = C
C
C    Next guess
C
         C = C-G/DG
         IF (C.LT.CMIN.OR.C.GT.CMAX) C = 0.5D0*(CMIN+CMAX)
      ENDIF
C
C    Check for convergence.
C
      IF (ABS(G).GT.1.0D-6.AND.IC.LT.100) GO TO 10
      IF (IC.GE.100) THEN
         IF (IOPPRN.GT.0) WRITE (FU6,1000)
         IERR = 1
      ENDIF
      RETURN
C
 1000 FORMAT (' TOO MANY ITERATIONS IN NEWTON SEARCH')
 1100 FORMAT (3X, 'IC', T10, 'C', T25, 'G', T40, 'DG', T54, 'CMIN', T69,
     *   'CMAX')
 1200 FORMAT (I5,1P,5E14.5)
C
      END SUBROUTINE rphnwt
C
C***********************************************************************
C  RPHRD1
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE rphrd1 (IOP)
      use perconparam, only : nsdim,fu6,fu30
      use common_inc
      use rate_const
      use kintcm, only : irepr
C
C Read in RPH information and set up tables used for interpolation.
C    Written Oct. '85.  BCG
C
C    IOP = 1,2, read in reactant information
C    IOP = 3,4, read in product information
C    IOP = 5, read in saddle point information
C    IOP = 6, read in RPH information for points along the MEP
C    IOP = 7, reactant well                                             0730PF97
C    IOP = 8, product well                                              0730PF97
C
C    CALLED BY:
C               RPHSET
C    CALLS:
C          RPHRD2,RPHB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
      SAVE DELV                                                         09/95KAN
C
      CHARACTER*80 CARD
C
C Processing the reactants and products and wells
C
      IF (IOP.LT.5.OR.IOP.EQ.7.OR.IOP.EQ.8) THEN                        0730PF97
C
C Called for reactants or products or wells
C
         IF (IOP.LE.2) THEN
C
C    For reactants store quantitites in first location of grids
C
            IS = 1
         ELSEIF (IOP.LE.4) THEN
C
C    For products store quantitites in last location of grids
C
            IS = NSDIM
C    For reactant well store quantities in second location              0730PF97
C
         ELSEIF (IOP.EQ.7) THEN                                         0730PF97
            IS = 2
C    For product well store quantities in the first to the last location
C
         ELSEIF (IOP.EQ.8) THEN                                         0730PF97
            IS = NSDIM - 1                                              0730PF97
C
         ENDIF
         N = NRATOM(IOP)
         IF (ICODE(IOP).EQ.1) THEN
C
C    ICODE = 1 ==> atom, just read in S and V
C
            CALL RPHRD2 (IOP,IS,0,0,0,0,0,0,0,0,0)
         ELSEIF (ICODE(IOP).EQ.2) THEN
C
C    ICODE = 2 ==> diatom, everything that is needed is read in
C       subroutine DIATOM, except for the mode number and the sign for
C       the 3rd derivative
C
            CALL RPHRD2 (IOP,IS,1,1,0,0,0,0,0,0,0)
            READ (FU30,*) SGN
            IF (LOPT(1).GT.1) WRITE (FU6,*) 'SNG', SGN
            IFRQ = IFREQ(1)
            WS(IS,IFRQ) = FREQ(1)
            IF (LOPT(5).GT.0) XK3S(IS,IFRQ) = SGN*XK3(1)
            IF (LOPT(5).GT.1) XK4S(IS,IFRQ) = XK4(1)
         ELSEIF (ICODE(IOP).GE.3) THEN
C
C    ICODE > 2 ==> Polyatomic
C
            NEND = 3*N
            IF (ICODE(IOP) .LT. 0) THEN                                 11/20T87
               NFREQ = NEND                                               ..
            ELSE IF (ICODE(IOP).EQ.3) THEN                                ..
               NFREQ = NEND-5                                             ..
            ELSE                                                          ..
               NFREQ = NEND-6                                             ..
            ENDIF                                                       11/20T87
C
C       Get frequencies and anharmonicities for polyatomic
C
            CALL RPHRD2 (IOP,IS,NEND,NFREQ,0,0,0,1,1,0,0)
         ENDIF
C
C    Calculate potential shift and store asymptotic values of V
C    Use the point 1 and NSDIM (determined by IS) to store the 
C    values.  FMOMS is the inverse of the moment of inertia.
C    With 2 reactants or 2 products, the molecules are seperated
C    100 bohr and the moment of innertia is infinity. Notice: IS
C    value is same for both IOP=1,2 (IS=1) and IOP=3,4 (IS=NSDIM)
C
         IF (IOP.EQ.1) THEN
            DELV = VS(IS)
            VS(IS) = 0.0D0
            FMOMS(IS) = 1.0D0/FMOM(1)
            SS(IS) = S
         ELSEIF (IOP.EQ.2) THEN
            DELV = DELV+VS(IS)
            VS(IS) = 0.0D0
            FMOMS(IS) = 0.0D0
            SS(IS) = -100.0D0
         ELSEIF (IOP.EQ.3) THEN
            VS(IS) = VS(IS)-DELV
            VSAVE = VS(IS)
            FMOMS(IS) = 1.0D0/FMOM(3)
            SS(IS) = S
         ELSEIF (IOP.EQ.4) THEN                                         0730PF97
            VS(IS) = VS(IS)+VSAVE
            FMOMS(IS) = 0.0D0
            SS(IS) = 100.0D0
         ELSEIF (IOP.EQ.7) THEN                                         0730PF97
            VS(IS) = VS(IS) - DELV                                      0730PF97
            SS(IS) = S                                                  0730PF97
            FMOMS(IS) = 1.0D0 / FMOM(7)                                 0730PF97
         ELSEIF (IOP.EQ.8) THEN                                         0730PF97
            VS(IS) = VS(IS) - DELV                                      0730PF97
            SS(IS) = S                                                  0730PF97
            FMOMS(IS) = 1.0D0 / FMOM(8)                                 0730PF97
         ENDIF
C
C processing the saddle point
C
      ELSEIF (IOP.EQ.5) THEN
C
C Called for saddle point.  Saddle point parameters stored in location
C    NSDIM-2 for now
C
         VSHIFT = DELV
         IS = NSDIM-2                                                   0801PF97
         IF (LGS(34) .NE. 0) THEN                                       11/20T87
            NFREQ = N3 - 1                                                 ..
         ELSE IF (ICODE(5).EQ.3) THEN                                      ..
            NFREQ = N3-6                                                   ..
         ELSE                                                              ..
            NFREQ = N3-7                                                   ..
         ENDIF                                                          11/20T87
C
C    Get RPH info
C
         CALL RPHRD2 (IOP,IS,N3,NFREQ,0,1,0,1,1,0,1)
C
         IF (LOPT(4).NE.0) THEN
            WSTAR = FREQ(1)
         ELSE
C
C    If F matrix not read in, read in imaginary freq. at saddle point
C
            READ (FU30,1300) CARD
            READ (FU30,*) WSTAR
            FREQ(1) = - ABS(WSTAR)                                      0427WH93
            CALL NOROUT(2,DXP)                                          0427WH93
C           IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,WSTAR
         ENDIF
         IF (LOPT(6).EQ.2) THEN
C
C    Save eigenvector for unbound motion along reaction path for
C       computing B at saddle point
C
            IF (LGS(27).EQ.-1) THEN
               SGN = 1.0D0
            ELSE
               SGN = -1.0D0
            ENDIF
            DO 10 I = 1, N3
               DXSAD(I) = SGN*COF(I,1)
               DO 10 J = 1, N3
                  COFSAD(I,J) = COF(I,J)
   10       CONTINUE
         ENDIF
         DO 20 I = 1, NFREQ
            IFRSAD(I) = IFREQ(I)
   20    CONTINUE
C
      ELSE
C
C Read in number of addition points along reaction coordinate
C
         VSHIFT = DELV
         READ (FU30,1300) CARD
         READ (FU30,*) NPTS
         IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,NPTS
         IF (NPTS.GT.NSDIM-5) THEN                                      0730PF97
            WRITE (FU6,1400) NSDIM
            STOP 'RPHRD1 1'
         ENDIF
         IF ( LGS(34) .NE. 0) THEN                                      11/20T87
            NFREQ = N3 -1                                                  ..
         ELSE IF (ICODE(5).EQ.3) THEN                                      ..
            NFREQ = N3-6                                                   ..
         ELSE                                                              ..
            NFREQ = N3-7                                                   ..
         ENDIF                                                          11/20T87
C8       NSS = NPTS+2                                                           
C8       IF (LGS(1).GT.0) NSS = NSS+1
C8       IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                           6/5S89
C8          NSS = NSS+1
C8          IS = 2
C8       ELSE
C8          IS = 1
C8       ENDIF
C8       IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN                           6/5S89
C8          NSS = NSS+1
C8          NSLAST = NSS-2
C8       ELSE
C8          NSLAST = NSS-1
C8       ENDIF
C 
C  The initial storage numbering scheme is fixed as follows:            0801PF97
C     IS = 1          Reactants                                         0801PF97
C     IS = 2          Reactant Well                                     0801PF97
C     IS = 3          First Path Point                                  0801PF97
C     IS = NSDIM - 2  Saddle Point                                      0801PF97
C     IS = NSDIM - 1  Product Well                                      0801PF97
C     IS = NSDIM      Products                                          0801PF97
C  Therefore, the first path point is placed in IS = 3 and the          0801PF97
C  last point is placed in NSLAST = NSS - 2                             0801PF97
C
         NSS = NPTS + 5                                                 0730PF97
         IS = 3                                                         0730PF97
         NSLAST = NSS - 2                                               0730PF97
C
C    Loop over grid points in s
C
   30    CONTINUE
         IF (LOPT(6).EQ.2) THEN
C
C       Shift gradient vectors used to compute BF
C
            DO 40 I = 1, N3
               DX1(I) = DX2(I)
               DX2(I) = DX(I)
   40       CONTINUE
            SB(1) = SB(2)
            SB(2) = SB(3)
         ENDIF
C
C    Read in RPH info
C
         CALL RPHRD2 (IOP,IS,N3,NFREQ,1,1,1,1,1,1,1)
C
         IF (IS.LT.NSLAST) THEN                                         0801PF97
            IS = IS + 1                                                 0801PF97
            GOTO 30                                                     0801PF97
         ENDIF                                                          0801PF97
C
C    End of loop over grid points
C
C    Put product values at NSS
C
         SS(NSS) = SS(NSDIM)
         FMOMS(NSS) = FMOMS(NSDIM)
         VS(NSS) = VS(NSDIM)
         DO 50 I = 1, NFREQ
            WS(NSS,I) = WS(NSDIM,I)
            IF (LOPT(5).GT.0) XK3S(NSS,I) = XK3S(NSDIM,I)
            IF (LOPT(5).GT.1) XK4S(NSS,I) = XK4S(NSDIM,I)
   50    CONTINUE
C
C   Put wellp values at nss - 1, if wellp exists                        0730PF97
C
        IF (irepr(8).eq.1) THEN                                         0801PF97
         SS(NSS-1) = SS(NSDIM-1)                                        0730PF97
         FMOMS(NSS-1) = FMOMS(NSDIM-1)                                  0730PF97
         DO 55 I = 1, NF(8)                                             0730PF97
            WS(NSS-1,I) = WS(NSDIM-1,I)                                 0730PF97
            IF (LOPT(5).GT.0) XK3S(NSS-1,I) = XK3S(NSDIM-1,I)           0730PF97
            IF (LOPT(5).GT.1) XK4S(NSS-1,I) = XK4S(NSDIM-1,I)           0730PF97
 55      CONTINUE                                                       0730PF97
        ENDIF                                                           0801PF97
C
         IF (LOPT(6).EQ.2) THEN
C
C    Calcuate BF at next-to-last grid point
C
            ST = SS(NSLAST)
            NBS = 2
            CALL RPHB (NBS,ST,N3,NFREQ,SB,DX1,DX2,DX,COF,BCURV)
            DO 60 I = 1, NFREQ
               IF (ABS(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
               BFS(NSLAST,IFREQ(I)) = BCURV(I)
   60       CONTINUE
         ELSEIF (LOPT(6).GT.2) THEN
C
C    Interpolate BF at saddle point from points on either side
C
            DO 70 I = 1, NFREQ
               BFS(ISSP,I) = 0.5D0*(BFS(ISSP-1,I)+BFS(ISSP+1,I))
   70       CONTINUE
         ENDIF
C
C Read in information to fit functional forms for interpolations in the
C    asymptotic regions.  Functional forms for the exponential fits
C    selected by the option codes.  The functional forms are:
C    Option code       Functional form
C    ___________       _____________________________________________
C        1             F(s) = A [1 - B exp(-Cs)] exp(-Cs)
C        2             F(s) = A (s-B) exp(-Cs)
C        3             F(s) = A (s-B) exp(-Cs**2)
C        4             F(s) = A (s-B) exp(-Cs**3)
C        5             F(s) = A (s-B)**2 exp(-Cs)
C        6             F(s) = A (s-B)**2 exp(-Cs**2)
C        7             F(s) = A (s-B)**2 exp(-Cs**3)
C        8             F(s) = A abs(s)**B exp(-Cs)
C        9             F(s) = A abs(s)**B exp(-Cs**2)
C       10             F(s) = A abs(s)**B exp(-Cs**3)
C
C        IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                           6/5S89
C
C    Read in point in reactant valley at finite s for exponential fits
C
C8   The extrapolation to an extra point has been removed in version 8.0
C8   and will be deleted in future versions
C8          READ (FU30,1300) CARD
C8          IS = 3                                                      0730PF97
C
C    Read option code for fitting potential, s, and V(s)
C
C8          READ (FU30,*) IFITVR,SS(IS),VS(IS)
C8          IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,IFITVR
C8          IF (LOPT(1).GT.1) WRITE (FU6,1100) SS(IS),VS(IS)
C8          IF (LOPT(7).GT.0) THEN
C
C    Read in option codes for fitting frequencies and the frequencies
C
C8             READ (FU30,1300) CARD
C8             READ (FU30,*) (IFITWR(I),I=1,NFREQ)
C8             READ (FU30,*) (WS(IS,I),I=1,NFREQ)
C8             IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,(IFITWR(I),I=1,
C8   *            NFREQ)
C8             IF (LOPT(1).GT.1) WRITE (FU6,1100) (WS(IS,I),I=1,NFREQ)
C8             IF (LOPT(7).GT.1) THEN
C
C    Read in option codes for fitting cubic anharmonicities and the
C       cubic anharmonicities
C
C8                READ (FU30,1300) CARD
C8                READ (FU30,*) (IFTK3R(I),I=1,NFREQ)
C8                READ (FU30,*) (XK3S(IS,I),I=1,NFREQ)
C8                IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,
C8   *                                     (IFTK3R(I),I=1,NFREQ)
C8                IF (LOPT(1).GT.1) WRITE (FU6,1100) (XK3S(IS,I),I=1,
C8   *                                                NFREQ)
C8                IF (LOPT(7).GT.2) THEN
C
C    Read in option codes for fitting quartic anharmonicities and the
C       quartic anharmonicities
C
C8                   READ (FU30,1300) CARD
C8                   READ (FU30,*) (IFTK4R(I),I=1,NFREQ)
C8                   READ (FU30,*) (XK4S(IS,I),I=1,NFREQ)
C8                   IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,(IFTK3R(I),
C8   *                                                  I=1,NFREQ)
C8                   IF (LOPT(1).GT.1) WRITE (FU6,1100) (XK4S(IS,I),I=1,
C8   *                  NFREQ)
C8                ENDIF
C8             ENDIF
C8          ENDIF
C        ENDIF
C        IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
C
C    Read in point in product valley at finite s for expontential fits
C
C8          READ (FU30,1300) CARD
C8          IS = NSS-2                                                  0730PF97
C
C    Read option code for fitting potential, s, V(s)
C
C8          READ (FU30,*) IFITVP,SS(IS),VS(IS)
C8          IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,IFITVR
C8          IF (LOPT(1).GT.1) WRITE (FU6,1100) SS(IS),VS(IS)
C8          IF (LOPT(8).GT.0) THEN
C
C    Read in option codes for fitting frequencies and the frequencies
C
C8             READ (FU30,1300) CARD
C8             READ (FU30,*) (IFITWP(I),I=1,NFREQ)
C8             READ (FU30,*) (WS(IS,I),I=1,NFREQ)
C8             IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,(IFITWP(I),I=1,
C8   *            NFREQ)
C8             IF (LOPT(1).GT.1) WRITE (FU6,1100) (WS(IS,I),I=1,NFREQ)
C8             IF (LOPT(8).GT.1) THEN
C
C    Read in option codes for fitting cubic anharmonicities and the
C       cubic anharmonicities
C
C8                READ (FU30,1300) CARD
C8                READ (FU30,*) (IFTK3P(I),I=1,NFREQ)
C8                READ (FU30,*) (XK3S(IS,I),I=1,NFREQ)
C8                IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,(IFTK3P(I),
C8   *                                                     I=1,NFREQ)
C8                IF (LOPT(1).GT.1) WRITE (FU6,1100) (XK3S(IS,I),I=1,
C8   *                                                NFREQ)
C8                IF (LOPT(8).GT.2) THEN
C
C    Read in option codes for fitting quartic anharmonicities and the
C       quartic anharmonicities
C
C8                   READ (FU30,1300) CARD
C8                   READ (FU30,*) (IFTK4P(I),I=1,NFREQ)
C8                   READ (FU30,*) (XK4S(IS,I),I=1,NFREQ)
C8                   IF (LOPT(1).GT.1) WRITE (FU6,1200) CARD,(IFTK4P(I),
C8   *                                                  I=1,NFREQ)
C8                   IF (LOPT(1).GT.1) WRITE (FU6,1100) (XK4S(IS,I),I=1,
C8   *                  NFREQ)
C8                ENDIF
C8             ENDIF
C8          ENDIF
C        ENDIF
      ENDIF
      RETURN
C
 1000 FORMAT (1X,A79,/1X,'Imaginary freq. = ',1P,E12.4,' a.u.')
 1100 FORMAT (2X,1P,5E15.5)
 1200 FORMAT (1X,A79,/,(2X,10I4) )
 1300 FORMAT (1X,A79)
 1400 FORMAT (/1X,'Maximum number of input points is ',I5)
C
      END SUBROUTINE rphrd1
C
C***********************************************************************
C  RPHRD2 
C*********************************************************************** 
C 
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91 
C 
      SUBROUTINE rphrd2 (IOP,IS,N,NFREQ,IRS,IRX,IRDX,IRF,IRK3,IRB,IRMOM)
      use perconparam, only : nvibm,n3tm,fu30,nsdim,fu6,fu28
      use rate_const
      use common_inc
      use kintcm
C 
C Read in RPH information; called by RPHRD1.  Written Oct. '85.  BCG
C   IOP < 5 ==> reactants or products
C       = 5 ==> saddle point
C       > 5 ==> point along MEP
C       = 7,8 > wellr and wellp                                         0730PF97
C   IS - index for saving info in RPHCM arrays
C   N  - number of coordinates
C   NFREQ - number of frequencies
C   IRS  .NE. 0 indicates that a point along the MEP is being read
C   IRX  .NE. 0 indicates that X is read
C   IRDX .NE. 0 indicates that DX is read
C   IRF  .NE. 0 indicates that frequencies are read in or computed from
C               the force constant matrix which is read in.
C   IRK3 .NE. 0 indicates that cubic anharmonicities are read in
C   IRK4 .NE. 0 indicates that quartic anharmonicities are read in
C   IRB  .NE. 0 indicates that BF are read in or computed
C   IRMOM.NE. 0 indicates that the moment of inertia is computed
C
C
C     CALLED BY:
C                RPHRD1
C     CALLS:
C           RPHB,RPHTRX,CENTER,RPHDXN,RPHTRF,FDIAG,ANHARM,ZEROPT,NOROUT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
C a temporary array to store the hessian elements 
C
C     DIMENSION TPF(N3TM*N3TM),XGUARD(N3TM)                             0219PF98
C     The above line was commented because XGUARD variable is not used. 0423TA02
      DIMENSION TPF(N3TM*N3TM)                                          0423TA02
      DIMENSION JFREQ(NVIBM)
c     DIMENSION SAVCOF(N3TM,N3TM)
      real(8), allocatable :: savcof(:,:)
      logical :: issad
      CHARACTER*80 CARD
      SAVE  SAVCOF                                                      0225JC98
      if(.not.allocated(savcof))then
        allocate(savcof(n3tm,n3tm)); savcof=0.d00
      end if

C
C read in s and V
C
      READ (FU30,1300) CARD
      READ (FU30,*) S,V
      IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,S,V
C
C check if the saddle point value can be shifted
C
      IF (LGS(1).GT.0.AND.IRS.GT.0.AND.ISSP.LT.0.AND.S.GT.0.0D0) THEN
C
C put saddle point values in location IS
C the location of the initial saddle point information has been changed to
C   IS = NSDIM - 2                                                      0730PF97
C
         ISSP = IS
         SS(IS) = 0.0D0
         VS(IS) = VS(NSDIM-2)                                           0730PF97
         DO 10 I = 1, NFREQ
            WS(IS,I) = WS(NSDIM-2,I)                                    0730PF97
            XK3S(IS,I) = XK3S(NSDIM-2,I)                                0730PF97
            XK4S(IS,I) = XK4S(NSDIM-2,I)                                0730PF97
            FMIRS(IS,I) = FMIRS(NSDIM-2,I)                              0126JC98
   10    CONTINUE
         DO 11 I=1,N3TM                                                 0405JC97
            XXS(I,IS)=XR(I,5)                                           0405JC97
   11    CONTINUE                                                       0405JC97
         FMOMS(IS) = FMOMS(NSDIM-2)                                     0730PF97
         DO 20 I = 1, N
            DX(I) = DXSAD(I)
   20    CONTINUE
         IF (IRB.GT.0.AND.LOPT(6).EQ.2)THEN
C
C Calcuate the curvature vector BF at the previous grid point
C
            SB(3) = 0.0D0
            IF (IS.GT.3) THEN                                           0225JC98
               IF (IS.EQ.4) THEN                                        0225JC98
                  NBS = 2
               ELSE
                  NBS = 3
               ENDIF
               ST = SS(IS-1)
               CALL RPHB (NBS,ST,N,NFREQ,SB,DX1,DX2,DX,COF,BCURV)
               DO 30 I = 1, NFREQ
                  IF (ABS(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
                  BFS(IS-1,IFREQ(I)) = BCURV(I)
   30          CONTINUE
            ENDIF
C
C Reset mode numbers, gradient vectors, coefficient matrix, and sb
C
            DO 40 I = 1, NFREQ
               IFREQ(I) = IFRSAD(I)
   40       CONTINUE
            DO 50 I = 1, N
C               DX1(I) = DX(I)                                          0510YC96
C               DX2(I) = DX1(I)                                         0510YC96
C I think it should be left shift                                       0510YC96
               DX1(I) = DX2(I)                                          0510YC96
               DX2(I) = DX(I)                                           0510YC96
               DO 50 J = 1, N
                  COF(I,J) = COFSAD(I,J)
   50       CONTINUE
            SB(1) = SB(2)
            SB(2) = SB(3)
         ENDIF
         IS = IS+1
      ENDIF
C
C Put s and V in grid
C
      SS(IS) = S
      VS(IS) = V-VSHIFT
C
      IF (IRX.GT.0) THEN
C
C Read in coordinates
C
C         READ (FU30,1300) CARD
C         READ (FU30,*) (X(I),I=1,N)
C
C The saddle point geometry is read from fu5 instead of fu30 
C
         IF (IOP.EQ.6) THEN                                             0730PF97
            READ (FU30,1300) CARD                                       0816YC96
            READ (FU30,*) (X(I),I=1,N)                                  0816YC96
         ELSE                                                           0816YC96
            CARD = 'X= '                                                0816YC96
            DO I = 1,N                                                  0816YC96
              X(I) = XR (I,5)                                           0816YC96
            ENDDO                                                       0816YC96
C
C Since in fu5 the coordinates are unscaled, we could need to
C mass-scale them for consistency
C
            IF (LOPT(3).LE.-2.OR.LOPT(3).EQ.1) CALL RPHTRX (N,AMASS,X,1)1126JC97
C
         ENDIF                                                          0816YC96
         IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,(X(I),I=1,N)
         IF (IRMOM.GT.0) THEN
C
C If in mass-weighted change to cartesian
C
            IF (LOPT(3).LE.-2.OR.LOPT(3).EQ.1) CALL RPHTRX (N,AMASS,X,2)0830YC96
C
C Copy the Cartesian coordinates before they are being shifted.
C
            DO 55 I = 1, N                                              06/95KAN
C              XXC(I) = X(I)                                            06/95KAN
               XXS(I,IS)=X(I)                                           0405JC97
   55       CONTINUE                                                    06/95KAN
C
C Shift origin to center of mass and compute moment of inertia
C
            KOP = ABS(IOP)                                               6/3T90
C
            IF(LGS(34).NE.0 .AND. (IOP.GT.4 .OR. ICODE(KOP).LT.0))THEN  11/20T87
               FMOMS(IS) = 1.0D+30                                      11/20T87
            ELSE
               CALL CENTER (5,0)                                        1125JC97
               FMOMS(IS) = 1.0D0/FMOM(5)
            ENDIF
            IF (IOP.GE.5.AND.IPRCD.EQ.1)                                0203YC98
     *          CALL PRCORD(FU28,S,V,X,NATOM,NPRCA,IPRCA,AMASS,1)       0203YC98
            CALL RPHTRX (N,AMASS,X,1)
         ENDIF
      ENDIF
C
      IF (IRDX.GT.0) THEN
C
C Read in gradient vector(s)
C
         READ (FU30,1300) CARD
         READ (FU30,*) (DX(I),I=1,N)
      
         IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,(DX(I),I=1,N)
C
C Normalize (and mass-weIght if necessary) DX
C
         CALL RPHDXN (LOPT(3),DX,DXN,AMASS,N)                           0812YC97
         DXMAG  = DXN
         DXNORM = DXN
         IF (IRB.GT.0.AND.LOPT(6).EQ.2.AND.IRODS.EQ.0.AND.IVRP.EQ.0)THEN0225JC98
            SB(3) = S
C
C Calcuate the curvature vector BF at the previous grid point
C
            IF (IS.GT.3) THEN                                           0225JC98
               IF (IS.EQ.4) THEN                                        0225JC98
                  NBS = 2
               ELSE
                  NBS = 3
               ENDIF
               ST = SS(IS-1)
               CALL RPHB (NBS,ST,N,NFREQ,SB,DX1,DX2,DX,COF,BCURV)
               DO 60 I = 1, NFREQ
                  IF (ABS(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
                  BFS(IS-1,IFREQ(I)) = BCURV(I)
   60          CONTINUE
            ENDIF
         ENDIF
C
C  Move to the end of hessian elements
C
C         IF (LOPT(6).GT.2) THEN
CC
CC Read in additional vector(s) near s to compute curvature vector
CC
C            SB(2) = S
C            READ (FU30,1300) CARD
C            READ (FU30,*) SB(3)
C            IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,SB(3)
C            READ (FU30,*) (DX2(I),I=1,N)
C            IF (LOPT(1).GT.1) WRITE (FU6,1200) (DX2(I),I=1,N)
CC
CC Normalize (and mass-weIght if necessary) DX
CC
C            CALL RPHDXN (LOPT(3),DX2,DX2N,DXMAX,AMASS,N)                1026YC96
C            IF (LOPT(6).GT.3) THEN
C               READ (FU30,1300) CARD
C               READ (FU30,*) SB(1)
C               IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,SB(1)
C               READ (FU30,*) (DX1(I),I=1,N)
C               IF (LOPT(1).GT.1) WRITE (FU6,1200) (DX1(I),I=1,N)
CC
CC Normalize (and mass-weIght if necessary) DX1
CC
C               CALL RPHDXN (LOPT(3),DX1,DXN1,DXMAX,AMASS,N)             1026YC96
C            ENDIF
C         ENDIF
C
C  End if shifting
C
      ENDIF
C
C Read in mode numbers
C
      READ (FU30,*) (IFREQ(I),I=1,NFREQ)
      IF (LOPT(1).GT.1) WRITE (FU6,*)'IFREQ', (IFREQ(I),I=1,NFREQ)
C
C Reorder indexes when the reaction is unimolecular or there are wells  1126JC97
C
          if (                                                          1126JC97
     *        iop.eq.7.or.                                              1126JC97
     *        iop.eq.8.or.                                              1126JC97
     *       (iop.eq.1.and.(lgs(6).eq.3.or.lgs(6).eq.4)).or.            1126JC97
     *       (iop.eq.3.and.(lgs(6).eq.2.or.lgs(6).eq.4))                1126JC97
     *       ) then                                                     1126JC97
               savifr=ifreq(nfreq)                                      1126JC97
               do i=nfreq-1,1,-1                                        1126JC97
                   ifreq(i+1)=ifreq(i)                                  1126JC97
               enddo                                                    1126JC97
               ifreq(1)=savifr                                          1126JC97
           endif                                                        1126JC97
C                                                                       1126JC97
C Reorder indexes for 2 reactants or 2 products                         1126JC97
C                                                                       1126JC97
C                                                                       1126JC97
          if (                                                          1126JC97
     *       ( (iop.eq.1.or.iop.eq.2).and.                              1126JC97
     *       (lgs(6).eq.1.or.lgs(6).eq.2) ).                            1126JC97
     *       or.                                                        1126JC97
     *       ( (iop.eq.3.or.iop.eq.4).and.                              1126JC97
     *       (lgs(6).eq.1.or.lgs(6).eq.3) )                             1126JC97
     *       ) then                                                     1126JC97
               do i=1,nfreq                                             1126JC97
                   jfreq(i)=ifreq(nfreq-i+1)                            1126JC97
               enddo                                                    1126JC97
               do i=1,nfreq                                             1126JC97
                   ifreq(i)=nf(5)+1-jfreq(i)                            1126JC97
               enddo                                                    1126JC97
           endif                                                        1126JC97
C                                                                       1126JC97
      IF (IRF.GT.0) THEN
         ISHFT = N-NFREQ
C
C for stationary points, just diagonalize (-IOP)
C for saddle point, put the imaginary freq (=2) and sort
C for others, projection  (=3)  
C
         IF (IOP.LT.5.OR.IOP.EQ.7.OR.IOP.EQ.8) THEN                     0730PF97
            IOPF = -IOP
         ELSEIF (IOP.EQ.5) THEN
            IOPF = 2
         ELSE
            IOPF = 3
         ENDIF
         IF (LOPT(4).NE.0) THEN
C
C Read in force constant matrix
C
            READ (FU30,1300) CARD
            IF (LOPT(1).GT.1) WRITE (FU6,1300) CARD
            JF = N
C
C read in packed line and change into packed triangular
C
            IF (LOPT(4).EQ.99) THEN                                     0830YC96
               JF = NINT(N*(N+1)/2.0)                                   0830YC96
               READ (FU30,*) (TPF(I),I=1,JF)                            0830YC96
               K = 1                                                    0830YC96
               DO I=1,N                                                 0830YC96
                 DO J= 1,I                                              0830YC96
                   F(I,J) = TPF(K)                                      0830YC96
                   K= K +1                                              0830YC96
                 END DO                                                 0830YC96
                 IF (LOPT(1).GT.1) WRITE (FU6,1200) (F(I,J),J=1,I)      0830YC96
               END DO                                                   0830YC96
            ELSE 
              DO 70 I = 1, N
                 IF (LOPT(4).LT.0) JF = I
                 READ (FU30,*) (F(I,J),J=1,JF)
                 IF (LOPT(1).GT.1) WRITE (FU6,1200) (F(I,J),J=1,JF)
   70         CONTINUE
            END IF
            IF ((LOPT(4).LT.0).OR.(LOPT(4).EQ.99)) THEN                 0830YC96
               NM1 = N-1
               DO 80 J = 1, NM1
                  JP1 = J+1
                  DO 80 I = JP1, N
                     F(J,I) = F(I,J)
   80          CONTINUE
            ENDIF
C
            IF (LGS(36).EQ.0) THEN                                      1110DL89
C
C Transform to mass-weighted coordinates
C
C               IF (ABS(LOPT(4)).EQ.1) CALL RPHTRF (N,AMASS,F,1)
                IF ((ABS(LOPT(4)).EQ.1).OR.(LOPT(4).EQ.99)) THEN        0830YC96
                      CALL RPHTRF (N,AMASS,F,1)                         0830YC96
                ENDIF                                                   0830YC96
C
C Diagonalize F matrix
C
               IF ((IRODS.EQ.1.OR.IVRP.EQ.1).AND.IOP.EQ.6) THEN         0225JC98
                   DO 82 I=1,N3TM                                       0225JC98
                      DO 83 J=1,N3TM                                    0225JC98
                        SAVCOF(I,J)=COF(I,J)                            0225JC98
   83                 CONTINUE                                          0225JC98
   82              CONTINUE                                             0225JC98
                   CALL DORODS (IOPF,IS)                                0219PF98
               ENDIF                                                    1014PF97
C
               IF(LGS2(39).NE.0.AND.IOP.GE.6.AND.IOP.NE.7.
     *                           AND.IOP.NE.8)THEN
                issad=.false.
                CALL ICFDIAG (IOP,ISSAD)                                0317Yc99
               ELSE                                                     07/95KAN
                 CALL FDIAG (IOPF)                                      07/95KAN
               ENDIF                                                    07/95KAN
C
               IF ((IRODS.EQ.1.OR.IVRP.EQ.1).AND.IOP.EQ.6) THEN         0225JC98
                   CALL ENDRODS (IOPF)                                  0219PF98
               END IF                                                   1014PF97
C
            ENDIF                                                       1110DL89
         ELSE
C
C Read in frequencies directly
C
            READ (FU30,1300) CARD
            DO 85 I = 1, ISHFT                                          0319WH93
               FREQ(I) = 0.0D0                                          0319WH93
85          CONTINUE     
            READ (FU30,*) (FREQ(I+ISHFT),I=1,NFREQ)
            IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,(FREQ(I+ISHFT),I=1,
     *         NFREQ)
         ENDIF
         IF (IRK3.GT.0.AND.LOPT(5).GT.0) THEN
C
C Read in anharmonicities
C
            READ (FU30,1300) CARD
            READ (FU30,*) (XK3(I),I=1,NFREQ)
            IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,(XK3(I),I=1,NFREQ)
            IF (LOPT(5).GT.1) THEN
               READ (FU30,1300) CARD
               READ (FU30,*) (XK4(I),I=1,NFREQ)
               IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,(XK4(I),I=1,
     *                                            NFREQ)
            ENDIF
         ENDIF
C
C Reorder and store frequencies
C
         DO 90 I = 1, NFREQ
            IFRQ = IFREQ(I)
            WS(IS,IFRQ) = FREQ(I+ISHFT)
            XK3S(IS,IFRQ) = XK3(I)
            XK4S(IS,IFRQ) = XK4(I)
   90    CONTINUE
C
C sort freq in descending order                                         0211YC97
C
         DO I = 1,NFREQ                                                 0211YC97
           DO J = I+1, NFREQ                                            0211YC97
            IF (FREQ(I+ISHFT).GT.FREQ(J+ISHFT)) THEN                    0211YC97
                 TEMPF=FREQ(I+ISHFT)                                    0211YC97
                 FREQ(I+ISHFT) = FREQ(J+ISHFT)                          0211YC97
                 FREQ(J+ISHFT) = TEMPF                                  0211YC97
            ENDIF                                                       0211YC97
           ENDDO                                                        0211YC97
         ENDDO                                                          0211YC97
C
         IF (LGS(5).GT.0)  THEN                                         0925JC97
            CALL ANHARM (IOPF)                                          0925JC97
            DO I = 1, NFREQ                                             0925JC97
                 FMIRS(IS,I) = FMOMHR(I+ISHFT)                          0925JC97
            ENDDO                                                       0925JC97
         ENDIF                                                          0925JC97
         CALL ZEROPT (IOPF)                                             9/18YL92
         DO 100 I = 1, N
            DXP(I) = DX(I)
  100    CONTINUE
         IF (IOPF.NE.2 .OR. LOPT(4).NE.0) CALL NOROUT (IOPF,DXP)        0427WH93
C
C Move from previous section
C
         IF (LOPT(6).GT.2.AND.IRDX.GT.0) THEN
C
C Read in additional vector(s) near s to compute curvature vector
C
            SB(2) = S
            READ (FU30,1300) CARD
            READ (FU30,*) SB(3)
            IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,SB(3)
            READ (FU30,*) (DX2(I),I=1,N)
            IF (LOPT(1).GT.1) WRITE (FU6,1200) (DX2(I),I=1,N)
C
C Normalize (and mass-weIght if necessary) DX
C
            CALL RPHDXN (LOPT(3),DX2,DX2N,AMASS,N)
            IF (LOPT(6).GT.3) THEN
               READ (FU30,1300) CARD
               READ (FU30,*) SB(1)
               IF (LOPT(1).GT.1) WRITE (FU6,1000) CARD,SB(1)
               READ (FU30,*) (DX1(I),I=1,N)
               IF (LOPT(1).GT.1) WRITE (FU6,1200) (DX1(I),I=1,N)
C
C Normalize (and mass-weIght if necessary) DX1
C
               CALL RPHDXN (LOPT(3),DX1,DXN1,AMASS,N)
            ENDIF
         ENDIF
C
C Finish shifting
C
         IF (IRB.GT.0) THEN
C
C Read in or compute curvature vector B (if LOPT(6)=2, then B already
C    computed, unless RODS is on)
C
            IF (LOPT(6).EQ.1) THEN
               READ (FU30,1300) CARD
               READ (FU30,*) (BFS(IS,IFREQ(I)),I=1,NFREQ)
               IF (LOPT(1).GT.2) WRITE (FU6,1000) CARD,(BFS(IS,IFREQ(I))
     *                                            ,I=1,NFREQ)
            ELSEIF (LOPT(6).GT.2) THEN
               NBS = LOPT(6)-1
               CALL RPHB (NBS,S,N,NFREQ,SB,DX1,DX,DX2,COF,BCURV)
               DO 110 I = 1, NFREQ
                  IF (ABS(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
                  BFS(IS,IFREQ(I)) = BCURV(I)
  110          CONTINUE
            ELSEIF (LOPT(6).EQ.2.AND.(IRODS.EQ.1.OR.IVRP.EQ.1)) THEN    0225JC98
               SB(3) = S                                                0225JC98
               IF (IS.GT.3) THEN                                        0225JC98
                  IF (IS.EQ.4) THEN                                     0225JC98
                     NBS = 2                                            0225JC98
                  ELSE                                                  0225JC98
                     NBS = 3                                            0225JC98
                  ENDIF                                                 0225JC98
                  ST = SS(IS-1)                                         0225JC98
                  CALL RPHB (NBS,ST,N,NFREQ,SB,DX1,DX2,DX,SAVCOF,BCURV) 0225JC98
                  DO 120 I = 1, NFREQ                                   0225JC98
                     IF (ABS(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0      0225JC98
                     BFS(IS-1,IFREQ(I)) = BCURV(I)                      0225JC98
  120             CONTINUE                                              0225JC98
               ENDIF                                                    0225JC98
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
C
 1000 FORMAT (1X,A79,/,(2X,1P,5E15.5))
 1200 FORMAT (2X,1P,5E15.5)
 1300 FORMAT (1X,A79)
C
      END SUBROUTINE rphrd2
C***********************************************************************
C  RPHSET
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE rphset (IOP)
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : gufac6,iunit6
C
C Set up RPH interpolation information.  This is the one of 3 entries
C    into the RPH routines from the VTST program; the others are RPHINT
C    and RPHWRT.  Written Oct. '85.  BCG
C
C    IOP = 0, first call to RPHSET
C    IOP = 1,2, read in reactant information
C    IOP = 3,4, read in product information
C    IOP = 5, read in saddle point information
C    IOP = 6, read in RPH information for points along the MEP
C    IOP = 7, reactant well                                             0730PF97
C    IOP = 8, product well                                              0730PF97
C
C     CALLED BY:
C                MAIN,REACT,NORMOD
C     CALLS:
C           DATTIM,TITLE,RPHRD1,RPHFIT,RPHINT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION APTS(NVIBM,NSDM)
C
C  APTS(NVIBM,NSDM) is used for temporary storage
C
      IF (IOP.EQ.0) THEN
C
C First call to RPHSET, read title and options.
C    Read and write in one-line title into title 3.
C
         WRITE (FU6,1000)
         CALL TITLE (0,FU30,3)
         CALL DATTIM (FU6)
         CALL TITLE (1,FU6,3)
C
C    Read options:
C       LOPT(1)  - print option
C               <= 0, print out only titles and options
C                = 1, also print out summary
C                = 2, also echo input as it is read
C               >= 3, also print out interpolated results each time
C                    RPHINT is called
C       LOPT(2)  = maximum number of points used in Lagrange
C                  interpolation
C       LOPT(3) <=-2, read coordinates in mass-scaled cartesians,
C                     gradient in unscaled cartesians
C                =-1, read coordinates in unscaled cartesians, gradient
C                     in mass-scaled cartesians
C                = 0, read coordinates and gradients in unscaled
C                     cartesians
C                = 1, read coordinates and gradients in mass-scaled
C                     cartesians
C                =99, read coordinates in unscaled cartesians,
C                     - gradient (FORCE) in mass-scaled cartesians
C                =100, read coordinates in unscaled cartesians,
C                     - gradient (FORCE) in unscaled cartesians
C       LOPT(4) <=-2, packed F matrix input in mass-weighted coord.
C                =-1, packed F matrix input in cartesian coordinates
C                = 0, omegas read in
C                = 1, full F matrix input in cartesian coordinates
C               >= 2, full F matrix input in mass-weighted coord.
C                =99, packed line F matrix input in unscaled coordinates
C       LOPT(5)  - option for selecting input for anharmonicities
C               <= 0, no anharmonic data input
C                = 1, read in k3 at each grid point
C               >= 2, also read in k4 at each grid point
C       LOPT(6)  - option for computing B from gradient
C               <= 0, set to zero everywhere
C                = 1, read in directly
C                = 2, use gradient on save grid and fit to quadratic
C                = 3, read in gradient at one extra point near the
C                     save-grid point and compute using one-sided
C                     derivative
C               >= 4, read in gradient at two extra points near the
C                     save-grid point and fit to quadratic
C       LOPT(7)  - option for point in reactant valley at finite s.
C               <= 0, read in s and V only; use range parameter from V
C                     to fit asymptotic forms for w, k3, k4, B, and
C                     FMOM**-1
C                = 1, read in s,V, and w's; use range parameter from
C                     w's to fit asymptotic forms for k3, k4, B, and use
C                     range parameter from V to fit asymptotic form for
C                     FMOM**-1
C                = 2, read in s,V, w's, and k3's; use range parameter
C                     from w's to fit asymptotic forms for k4, B, and
C                     use range parameter from V to fit asymptotic form
C                     for FMOM**-1
C               >= 3, read in s,V, w's, k3's, and k4's; use range
C                     parameter from w's to fit asymptotic forms for B
C                     and use range parameter from V to fit asymptotic
C                     form for FMOM**-1
C       LOPT(8)  - option for point in product valley at finite s.
C                  option values are same as for LOPT(7).
C
         call rphset_mem
         READ (FU30,*) LOPT
         WRITE (FU6,1100) (I,I=1,20),(LOPT(I),I=1,20)                   0621WH94
         IF (LOPT(2).EQ.-1) LOPT(2)=500                                 1202JC97
         IF (LOPT(6).GT.1.AND.LOPT(4).EQ.0) THEN
            WRITE (FU6,2600)
            STOP 'RPHSET 1'
         ENDIF
         IF (LGS(34) .NE. 0) THEN                                       11/20T87
            N3M7 = N3 -1                                                   ..
         ELSE                                                              ..
            N3M7 = N3-7                                                    ..
         ENDIF                                                          11/20T87
         WRITE (FU6,1200)
         DO 20 IS = 1, NSDIM
            DO 10 I = 1, N3M7
               WS(IS,I) = 0.0D0
               XK3S(IS,I) = 0.0D0
               XK4S(IS,I) = 0.0D0
               BFS(IS,I) = 0.0D0
               FMIRS(IS,I) = 0.0D0                                      0925JC97
   10       CONTINUE
            VS(IS) = 0.0D0
            FMOMS(IS) = 0.0D0
   20    CONTINUE
         ISSP = -1
      ELSE
C
C Read in reaction path interpolation information for IOP >0 and <8.    0730PF97
C
         CALL RPHRD1 (IOP)
      ENDIF
C
C For NEXTPT option, only option code 0 is allowed                      0821YC96
C
      IF (((IFITVR.NE.0).OR.(IFITVP.NE.0)).AND.(LGS(36).NE.0)) THEN     0821YC96
           WRITE (FU6,3000)                                             0821YC96
           STOP ' RPHSET/NEXTPT '                                       0821YC96
      ENDIF                                                             0821YC96
3000  FORMAT (//' WARNING ! The NEXTPT option requires option code = 0')
C
      IF (IOP.GT.5.AND.IOP.NE.7.AND.IOP.NE.8) THEN                      0730PF97
         IF (LGS(34) .NE. 0) THEN                                       11/20T87
            ISHFT = 1                                                      ..
         ELSE IF (ICODE(5).EQ.3) THEN                                   11/20T87
            ISHFT = 6
         ELSE
            ISHFT = 7
         ENDIF
         NFREQ = N3-ISHFT
         IF (LOPT(6).EQ.1.AND.LGS(1).GT.0) THEN
C
C Set curvature components for saddle point by linear interpolation
C
            T = -SS(ISSP-1)/(SS(ISSP+1)-SS(ISSP-1))
            DO 30 I = 1, NFREQ
               BFS(ISSP,I) = (1.0D0-T)*BFS(ISSP-1,I)+T*BFS(ISSP+1,I)
   30       CONTINUE
         ENDIF
C
C Fit long range exponential tails
C
         CALL RPHFIT (NSS,NFREQ,IERR)
C
         NINT1 = LOPT(2)
         NINTMX = NSS
CPF      IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) NINTMX = NINTMX-2              6/5S89
CPF      IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) NINTMX = NINTMX-2
         NINTMX = NINTMX - 5                                            0801PF97
         NINT1 = MIN(NINT1,NINTMX)
         NINTH = (NINT1+1)/2
C         IF (LOPT(1).GT.0) THEN
          IF ((LOPT(1).GT.0).AND.(LGS(36).NE.1)) THEN                   0821YC96
C
C Evaluate exponential fits and store info in temporary arrays for
C    printing later
C
            IS0 = 3                                                     0801PF97
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                        6/5S89
               IS0 = 3                                                  0801PF97
               DELS = INT(10.0D0*(SS(3)-SS(2)))                                 
               DELS = MAX(0.1D0*DELS,0.1D0)
               S = SS(2)-5.0D0*DELS                                             
               DO 50 I = 1, 5
                  S = S+DELS
                  CALL RPHINT (3)
                  SSUBI(I) = S
                  VCLAS(I) = V
                  IF (FMOM(5).GT.0.0D0) THEN
                     FMITS(I) = 1.0D0/FMOM(5)
                  ELSE
                     FMITS(I) = 1.0D+30
                  ENDIF
                  DO 40 J = 1, NFREQ
                     WETS(J,I) = FREQ(J+ISHFT)
                     XETS(J,I) = XK3(J)
                     Y0TS(J,I) = XK4(J)
                     APTS(J,I) = BCURV(J)
   40             CONTINUE
   50          CONTINUE
            ENDIF
            NS0 = NSS - 2                                               0801PF97
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
               NS0 = NSS-2                                              0801PF97
               DELS = INT(10.0D0*(SS(NSS-2)-SS(NSS-3)))                 0801PF97
               DELS = MAX(0.1D0*DELS,0.1D0)
               S = SS(NSS-1)-DELS                                               
               DO 70 I = 6, 10
                  S = S+DELS
                  CALL RPHINT (3)
                  SSUBI(I) = S
                  VCLAS(I) = V
                  IF (FMOM(5).GT.0.0D0) THEN
                     FMITS(I) = 1.0D0/FMOM(5)
                  ELSE
                     FMITS(I) = 1.0D+30
                  ENDIF
                  DO 60 J = 1, NFREQ
                     WETS(J,I) = FREQ(J+ISHFT)
                     XETS(J,I) = XK3(J)
                     Y0TS(J,I) = XK4(J)
                     APTS(J,I) = BCURV(J)
   60             CONTINUE
   70          CONTINUE
            ENDIF
C
C Write out summary
C
            WRITE (FU6,1300) NINT1,NS0-IS0+1
C
C Potential and inverse of the moment of interia
C
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                        6/5S89
               WRITE (FU6,1380)
             IF(IUNIT6.EQ.1)                                            0405JZ07
     *         WRITE (FU6,1400) IFITVR,IFITVR,(AVR(I),AFIR(I),I=1,3),
     *                          VS(1),0.0D0
             IF(IUNIT6.EQ.0)                                            0405JZ07
     *         WRITE (FU6,1410) IFITVR,IFITVR,(AVR(I),AFIR(I),I=1,3),
     *                          VS(1),0.0D0
               DO 80 IS = 1, 5
                  WRITE (FU6,1500) SSUBI(IS)/GUFAC6,VCLAS(IS),FMITS(IS) 0405JZ07
   80          CONTINUE
            ENDIF
            WRITE (FU6,2500)
            WRITE (FU6,1450)
            DO 90 IS = IS0, NS0
               WRITE (FU6,1600) IS-IS0+1,SS(IS)/GUFAC6,VS(IS),FMOMS(IS) 0405JZ07
   90       CONTINUE
            WRITE (FU6,2500)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
               WRITE(FU6,1390)
             IF(IUNIT6.EQ.1)                                            0405JZ07
     *         WRITE (FU6,1400) IFITVP,IFITVP,(AVP(I),AFIP(I),I=1,3),
     *                          VS(NSS),0.0D0
             IF(IUNIT6.EQ.0)                                            0405JZ07
     *         WRITE (FU6,1410) IFITVR,IFITVR,(AVR(I),AFIR(I),I=1,3),
     *                          VS(1),0.0D0
               DO 100 IS = 6, 10
                  WRITE (FU6,1500) SSUBI(IS)/GUFAC6,VCLAS(IS),FMITS(IS) 0405JZ07
  100          CONTINUE
            ENDIF
C
C Frequencies
C
            WRITE (FU6,1700)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                        6/5S89
               WRITE (FU6,1380)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFITWR(J),AWR(1,J),AWR(2,J),AWR(3,J),0623WH94
     *                           WS(1,J),J=1,NFREQ)
               DO 130 IS = 1, 5
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,WETS(J,IS),J=1,NFREQ)
  130          CONTINUE
            ENDIF
            WRITE (FU6,2500)
            WRITE (FU6,1450)
            DO 140 IS = IS0, NS0
               WRITE (FU6,1855) SS(IS)/GUFAC6,IS-IS0+1                  0405JZ07
               WRITE (FU6,1860) (J,WS(IS,J),J=1,NFREQ)
  140       CONTINUE
            WRITE (FU6,2500)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
               WRITE (FU6,1390)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFITWP(J),AWP(1,J),AWP(2,J),AWP(3,J),0623WH94
     *                           WS(NSS,J),J=1,NFREQ)
               DO 160 IS = 6, 10
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,WETS(J,IS),J=1,NFREQ)
  160          CONTINUE
            ENDIF
C
C Cubic anharmonicities
C
            WRITE (FU6,2200)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                        6/5S89
               WRITE (FU6,1380)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFTK3R(J),AK3R(1,J),
     *                      AK3R(2,J),AK3R(3,J),XK3S(1,J),J=1,NFREQ)    0623WH94
               DO 190 IS = 1, 5
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,XETS(J,IS),J=1,NFREQ)
  190          CONTINUE
            ENDIF
            WRITE (FU6,2500)
            WRITE (FU6,1450)
            DO 200 IS = IS0, NS0
               WRITE (FU6,1855) SS(IS)/GUFAC6,IS-IS0+1                  0405JZ07
               WRITE (FU6,1860) (J,XK3S(IS,J),J=1,NFREQ)
  200       CONTINUE
            WRITE (FU6,2500)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
               WRITE (FU6,1390)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFTK3P(J),AK3P(1,J),
     *                      AK3P(2,J),AK3P(3,J),XK3S(NSS,J),J=1,NFREQ)
               DO 210 IS = 6, 10
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,XETS(J,IS),J=1,NFREQ)
  210          CONTINUE
            ENDIF
C
C Quartic anharmonicities
C
            WRITE (FU6,2300)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                        6/5S89
               WRITE (FU6,1380)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFTK4R(J),AK4R(1,J),
     *                      AK4R(2,J),AK4R(3,J),XK4S(1,J),J=1,NFREQ)    0623WH94
               DO 300 IS = 1, 5
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,Y0TS(J,IS),J=1,NFREQ)
  300          CONTINUE
            ENDIF
            WRITE (FU6,2500)
            WRITE (FU6,1450)
            DO 310 IS = IS0, NS0
               WRITE (FU6,1855) SS(IS)/GUFAC6,IS-IS0+1                  0405JZ07
               WRITE (FU6,1860) (J,XK4S(IS,J),J=1,NFREQ)
  310       CONTINUE
            WRITE (FU6,2500)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
               WRITE (FU6,1390)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFTK4P(J),AK4P(1,J),
     *                      AK4P(2,J),AK4P(3,J),XK4S(NSS,J),J=1,NFREQ)
               DO 320 IS = 6, 10
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,Y0TS(J,IS),J=1,NFREQ)
  320          CONTINUE
            ENDIF
C
C Reaction-path curvature components
C
            WRITE (FU6,2400)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                        6/5S89
               WRITE (FU6,1380)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFITWR(J),ABFR(1,J),
     *                      ABFR(2,J),ABFR(3,J),BFS(1,J),J=1,NFREQ)     0623WH94
               DO 400 IS = 1, 5
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,APTS(J,IS),J=1,NFREQ)
  400          CONTINUE
            ENDIF
            WRITE (FU6,2500)
            WRITE (FU6,1450)
            DO 410 IS = IS0, NS0
               WRITE (FU6,1855) SS(IS)/GUFAC6,IS-IS0+1                  0405JZ07
               WRITE (FU6,1860) (J,BFS(IS,J),J=1,NFREQ)
  410       CONTINUE
            WRITE (FU6,2500)
            IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
               WRITE (FU6,1390)                                         0623WH94
               WRITE (FU6,1750)                                         0623WH94
               WRITE (FU6,1800) (J,IFITWP(J),ABFP(1,J),
     *                      ABFP(2,J),ABFP(3,J),BFS(NSS,J),J=1,NFREQ)
               DO 420 IS = 6, 10
                  WRITE (FU6,1850) SSUBI(IS)/GUFAC6                     0405JZ07
                  WRITE (FU6,1860) (J,APTS(J,IS),J=1,NFREQ)
  420          CONTINUE
            ENDIF
         ENDIF
C
         WRITE (FU6,1200)
         IF (IERR.NE.0) STOP 'RPHSET 2'
      ENDIF
      RETURN
C
 1000 FORMAT (//1X,78('*'),/3X,'Set up parameters for interpolating',
     *   ' reaction-path Hamiltonian',/1X,78('*'))                      0621WH94
 1100 FORMAT (/20X,'===== General (LOPT) options =====',                0621WH94
     *         /1X,'Option: ',20I3,
     *         /1X,'Choice: ',20I3)
 1200 FORMAT (/1X,78(1H*))                                              0621WH94
 1300 FORMAT (/15X,'Summary of interpolation data',/1X,60('-'),
     *//2X,'For interpolations to asymptotic values',
     * /2X,'the following exponential forms are used:',
     *//2X,'Fit option    Functional form',
     * /2X,'    1         F(s) = Fasy + A [1-Bexp(-Cs)] exp(-Cs)',
     * /2X,'    2         F(s) = Fasy + A (s-B) exp(-Cs)',
     * /2X,'    3         F(s) = Fasy + A (s-B) exp(-Cs**2)',
     * /2X,'    4         F(s) = Fasy + A (s-B) exp(-Cs**3)',
     * /2X,'    5         F(s) = Fasy + A (s-B)**2 exp(-Cs)',
     * /2X,'    6         F(s) = Fasy + A (s-B)**2 exp(-Cs**2)',
     * /2X,'    7         F(s) = Fasy + A (s-B)**2 exp(-Cs**3)',
     * /2X,'    8         F(s) = Fasy + A [abs(s)]**B exp(-Cs)',
     * /2X,'    9         F(s) = Fasy + A [abs(s)]**B exp(-Cs**2)',
     * /2X,'   10         F(s) = Fasy + A [abs(s)]**B exp(-Cs**3)',
     *//2X,'In the interaction region, ',I3,'-point Lagrange',
     * /2X,'interpolation is used on a grid of ',I4,' points.')
 1380 FORMAT(/2X,'****** Reactant Side ******')
 1390 FORMAT(/2X,'****** Product  Side ******')
 1400 FORMAT(/18X,'   for V    ','for 1/Det(I)',/,
     */5X,'Fit option',3X,I6,I12,
     */5X,'    A     ',3X,1P,2E12.4,
     */5X,'    B     ',3X,2E12.4,
     */5X,'    C     ',3X,2E12.4,
     */5X,'   Fasy   ',3X,2E12.4,/,
     */5X,'   s(bohr)',3X,'  V(hartree)','  1/Det(I) (a.u.)',/)
 1410 FORMAT(/18X,'   for V    ','for 1/Det(I)',/,
     */5X,'Fit option',3X,I6,I12,
     */5X,'    A     ',3X,1P,2E12.4,
     */5X,'    B     ',3X,2E12.4,
     */5X,'    C     ',3X,2E12.4,
     */5X,'   Fasy   ',3X,2E12.4,/,
     */5X,'   s(angstrom)',3X,'  V(hartree)','  1/Det(I) (a.u.)',/)
 1450 FORMAT (1X,'grid points:')
 1500 FORMAT (5X,F10.5,3X,1P,5E12.4)
 1600 FORMAT (1X,I4,F10.5,3X,1P,5E12.4)
 1700 FORMAT (/2X,'Frequency fitting information:')
 1750 FORMAT (/1X,'Mode','  Fit option','     A      ','      B      ',
     *                                  '     C      ','     Fasy    ')
 1800 FORMAT (1X,I4,5X,I2,5X,1P,4E12.4)
 1850 FORMAT (/2X,'at s = ',F10.5,/1X,20('-'),
     *        /1X,'mode','  frequency (a.u.)',/)
 1855 FORMAT (/2X,'at s = ',F10.5,'  grid = ',I3,/1X,30('-'),           0623WH94
     *        /1X,'mode','  frequency (a.u.)',/)
 1860 FORMAT (1X,I4,1X,1P,E15.5)                                        0623WH94
 2200 FORMAT (/2X,'Third derivative fitting information:')
 2300 FORMAT (/2X,'Fourth derivative fitting information:')
 2400 FORMAT (/2X,'Curvature component (BF,K) fitting information:')
 2500 FORMAT ( )
 2600 FORMAT (/1X,'You have selected the omegas to be read in directly',0622WH94
     */1X,'and the BFs to be computed.  However, to compute the BFs',
     */1X,'it is necessary to read in the F matrix.')
C
      END SUBROUTINE rphset
C
C***********************************************************************
C  RPHTRF
C***********************************************************************
C
C    PARAMETERS COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE rphtrf (N,AMASSX,FX,ISW)
      use common_inc, only : ind
      use perconparam, only : n3tm
C
C Transform matrix of 2nd derivatives wrt unweighted coordinates to 2nd
C    derivates wrt mass-weighted coordinates.  Written Oct. '85.  BCG
C
C     CALLED BY:
C               RPHRD2,RPHWRT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION AMASSX(N3TM),FX(N3TM,N3TM)
C
      DO 10 I = 1, N
         IF (ISW.LT.0) THEN
            TI = AMASSX(IND(I))
         ELSE
            TI = 1.0D0/AMASSX(IND(I))
         ENDIF
         DO 10 J = 1, N
            IF (ISW.LT.0) THEN
               TJ = AMASSX(IND(J))
            ELSE
               TJ = 1.0D0/AMASSX(IND(J))
            ENDIF
            FX(I,J) = FX(I,J)*TI*TJ
   10 CONTINUE
      RETURN
      END SUBROUTINE rphtrf
C
C***********************************************************************
C  RPHTRX
C***********************************************************************
C
      SUBROUTINE rphtrx (N,AMASS,X,IOP)
C
C Transform unscaled coordinates to mass-weighted and vice-versa.
C    Subroutine added 01/30/87. bcg
C     IOP = 1,   unscaled to mass-scaled
C     IOP = 2,   mass-scaled to unscaled
C
C     CALLED BY:
C               RPHRD2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION AMASS(N),X(N)
C
      DO 10 I = 1, N
         IF (IOP.EQ.2) THEN
            X(I) = X(I)/AMASS(I)
         ELSE
            X(I) = X(I)*AMASS(I)
         ENDIF
   10 CONTINUE
      RETURN
      END SUBROUTINE rphtrx
C
C***********************************************************************
C  RPHWRT
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C   MODIFIED FOR WRITTING FU31 BY JC                                    0810JC97
C
      SUBROUTINE rphwrt (IOP)
C
C Write out RPH interpolation information.  Written Oct. '85.  BCG
c  only used when lgs(30) = -1 or lgs(30) = -3
C
C    IOP = 0, first call to RPHSET
C    IOP = 1,2, write out reactant information
C    IOP = 3,4, write out product information
C    IOP = 5, write out saddle point information
C    IOP = 6, set up to write out RPH information for points along
C             the MEP
C    IOP = 7, reactant well information                                 0730PF97
C    IOP = 8, product well information                                  0730PF97
C    IOP = 9, write out RPH information for points along the MEP        0917JC97
C
C     CALLED BY:
C                MAIN,REACT,NORMOD,PATH
C     CALLS:
C           TITLE,RPHTRF
C
      use common_inc
      use perconparam
      use rate_const
      use kintcm, only : isup
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      save VPARTP
      save VPARTR
C
      INTEGER ISAVEP
      DIMENSION FTMP(N3TM,N3TM)                                         0115JC98
      LIFREQ = 0
      DO I=1,N3TM                                                       0115JC98
        DO J=1,N3TM                                                     0115JC98
          FTMP(I,J)=FSV(I,J)                                            0115JC98
        ENDDO                                                           0115JC98
      ENDDO                                                             0115JC98
C
C Commented out by jc 0810JC97  DIMENSION LLOPT(40)
C
C
      IF (IOP.EQ.0) THEN
C
C    First call; read title and options from lfn 30.
C
         IF (LGS(30).EQ.-1) CALL TITLE (0,FU30,3)
C
C    Options:
C       LLOPT(1-2)- not used
C note:       all gradients will be un normalized
C       LLOPT(3) <=-2, write coordinates in mass-scaled cartesians,
C                     gradient in unscaled cartesians
C                =-1, write coordinates in unscaled cartesians, gradien
C                     in mass-scaled cartesians
C                = 0, write coordinates and gradients in unscaled
C                     cartesians
C                = 1, write coordinates and gradients in mass-scaled
C                = 99, write coordinates in unscaled cartesians,
C                      -gradients (FORCE) in mass-scaled cartesians 
C                =100, write coordinates in unscaled cartesians,
C                       -gradients in unscaled, un-normalized cartesians
C       LLOPT(4)  =-2, packed F matrix written in mass-weighted coord.
C                =-1, packed F matrix written in cartesian coordinates
C                = 0, omegas written
C                = 1, full F matrix written in cartesian coordinates
C                = 2, full F matrix written in mass-weighted coord.
C       LLOPT(5)  - option for writing anharmonicity data
C               <= 0, no anharmonic data written
C                = 1, write k3 at each grid point
C               >= 2, write k3 and k4 at each grid point
C       LLOPT(6)  - option for writing information for B
C               <= 0, not written
C                = 1, write out B
C                = 2, write out gradient on save grid
C                = 3, write out gradient at one extra point near the
C                     save-grid point  (unit according to LLOPT(3))
C               >= 4, write out gradient at two extra points near the
C                     save-grid point  (unit according to LLOPT(3))
C       LLOPT(7-40), not used
C
        IF (LGS(30).EQ.-1) READ (FU30,*) LLOPT
        IF (LGS(30).EQ.-3) THEN
         CALL ROPT31
         IF (LLOPT(5).GT.0) THEN
             WRITE (FU6,1098)
             STOP 'RPHWRT 1'
         ELSE IF (LLOPT(4).EQ.0) THEN
             WRITE (FU6,1099)
             STOP 'RPHWRT 2'
         ENDIF
        ENDIF
        LGRAD = LLOPT(6)
        LPGRD = LLOPT(3)
C        IF (LGRAD.GT.2.AND.LLOPT(3).EQ.0.OR.LLOPT(3).LE.-2) THEN
C           WRITE (FU6,2300)
C           STOP 'RPHSET 1'
C        ENDIF
C
C    Reactants or products or wells
C
      ELSEIF (IOP.LE.8.AND.IOP.NE.5.AND.IOP.NE.6) THEN                  0730PF97
         IF (LGS(30).EQ.-1) WRITE (FU30,1100) IOP
         IF (LGS(30).EQ.-3) THEN
              IF (IOP.EQ.1) THEN
                    BACKSPACE FU31
                    WRITE (FU31,1101)
C                   VPARTP=V
                    VPARTR=V                                            0315JZ10
              ENDIF
              IF (IOP.EQ.2) THEN
                    WRITE (FU31,1102)
                    IF (ISUP.NE.0) THEN
c                         VPARTP=VPARTP+V                          
                          VPARTR=VPARTR+V                               0315JZ10
                    ELSE
c                         VPARTP=V
                          VPARTR=V                                      0315JZ10
                    ENDIF
              ENDIF
              IF (IOP.EQ.3) THEN
                    WRITE (FU31,1103)
                    IF (ISUP.EQ.0) THEN
c                     IF (LGS(6).EQ.2.OR.LGS(6).EQ.4) VPARTP=V-VPARTP
                      IF (LGS(6).EQ.2.OR.LGS(6).EQ.4) VPARTP=V-VPARTR   0315JZ10
                    ELSE
c                     VPARTP=V-VPARTP
                      VPARTP=V-VPARTR                                   0315JZ10
                    ENDIF
              ENDIF
              IF (IOP.EQ.4) THEN
                    WRITE (FU31,1104)
                    IF (ISUP.NE.0) THEN
                          VPARTP=VPARTP+V
                    ELSE
                          VPARTP=V-VPARTP
                    ENDIF
              ENDIF
              IF (IOP.EQ.7) THEN
                    WRITE (FU31,1107)
c                   WRITE (FU31,1207) V
                    WRITE (FU31,1207) V-VPARTR                          0315JZ10
              ENDIF
              IF (IOP.EQ.8) THEN
                    WRITE (FU31,1108)
c                   WRITE (FU31,1208) V
                    WRITE (FU31,1208) V-VPARTR                          0315JZ10
              ENDIF
         ENDIF
         SWRT = 0.0D0
         IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN
              ILIMOP = 4
         ELSE
              ILIMOP = 3
         ENDIF
         IF (IOP.LT.ILIMOP) THEN
            VWRT = 0.0D0
            IF (LGS(30).EQ.-1) WRITE (FU30,1200) SWRT,VWRT
         ELSE
            IF (LGS(30).EQ.-1) WRITE (FU30,1200) SWRT,V
            IF (LGS(30).EQ.-3.AND.IOP.EQ.ILIMOP) 
     *                 WRITE (FU31,1105) VPARTP
         ENDIF
         N = NRATOM(IOP)
         IF (IOP.EQ.1.OR.IOP.EQ.3.OR.IOP.EQ.7.OR.IOP.EQ.8) LIFREQ = 0   1126JC97
         IF (ICODE(IOP).EQ.1) THEN
            IF (LGS(30).EQ.-1) WRITE (FU30,1000) LIFREQ+1               1126JC97
         ELSEIF (ICODE(IOP).EQ.2) THEN
            IF (LGS(30).EQ.-1) WRITE (FU30,1000) LIFREQ+1               1126JC97
            LIFREQ = LIFREQ+1                                           1126JC97
            SGN = 1.0D0
            IF (LGS(30).EQ.-1) WRITE (FU30,1200) SGN
         ELSE
            NEND = NDIM(IOP)
            IF (ICODE(IOP) .LT. 0) THEN                                 11/20T87
               ISHFT = 0                                                   ..
            ELSE IF (ICODE(IOP).EQ.3) THEN                              11/20T87
               ISHFT = 5
            ELSE
               ISHFT = 6
            ENDIF
            NFREQ = NEND - ISHFT
            IF (LGS(30).EQ.-1) WRITE (FU30,1000) (LIFREQ+I,I=1,NFREQ)
            LIFREQ = LIFREQ+NFREQ                                       1226JC97
            IF (LLOPT(4).NE.0) THEN
              IF (ABS(LLOPT(4)).EQ.1) CALL RPHTRF (NEND,AMASS,FTMP,-1)  0115JC98
              IF (LGS(30).EQ.-1) WRITE (FU30,1400)
              IF (LGS(30).EQ.-3) WRITE (FU31,1401)
              JF = NEND
              DO 10 I = 1, NEND
               IF (LLOPT(4).LT.0) JF = I
               IF (LGS(30).EQ.-1) WRITE (FU30,1200) (FTMP(I,J),J=1,JF)  0115JC98
               IF (LGS(30).EQ.-3) WRITE (FU31,1200) (FTMP(I,J),J=1,JF)  0115JC98
   10         CONTINUE
              IF (LGS(30).EQ.-3) WRITE (FU31,1402)
            ELSE
               WRITE (FU30,1500) (FREQ(I+ISHFT),I=1,NFREQ)
            ENDIF
            IF (LLOPT(5).GT.0) WRITE (FU30,1600) (XK3(I),I=1,NFREQ)
            IF (LLOPT(5).GT.1) WRITE (FU30,1700) (XK4(I),I=1,NFREQ)
         ENDIF
C
C    Saddle point
C
      ELSEIF (IOP.EQ.5) THEN
         SWRT = 0.0D0
         IF (LGS(30).EQ.-1) WRITE (FU30,1800)
         IF (LGS(30).EQ.-3) WRITE (FU31,1801)
         IF (ICODE(5) .LT. 0) THEN                                      11/20T87
            ISHFT = 1                                                      ..
         ELSE IF (ICODE(5).EQ.3) THEN                                   11/20T87
            ISHFT = 6
         ELSE
            ISHFT = 7
         ENDIF
         NFREQ = N3 - ISHFT
         IF (LGS(30).EQ.-1) WRITE (FU30,1200) SWRT,V
         IF (LGS(30).EQ.-3) WRITE (FU31,1106) V
C
C  The saddle point geometry is no longer written in fu30               1104JC97
C
C        IF (LLOPT(3).EQ.0.OR.LLOPT(3).EQ.-1.or.LLOPT(3)                1104JC97
C    *        .eq.100.or.llopt(3).eq.99) THEN                           1104JC97
C
C  Write out unscaled X
C
C          IF (LGS(30).EQ.-1) WRITE (FU30,1300) (X(I)/AMASS(I),I=1,N3)  1104JC97
C        ELSE                                                           1104JC97
C
C  Write out mass-scaled X
C
C          IF (LGS(30).EQ.-1) WRITE (FU30,1300) (X(I),I=1,N3)           1104JC97
C        ENDIF                                                          1104JC97
C
         IF (LGS(30).EQ.-1) WRITE (FU30,1000) (I,I=1,NFREQ)
         IF (LLOPT(4).NE.0) THEN
           IF (ABS(LLOPT(4)).EQ.1) CALL RPHTRF (N3,AMASS,FTMP,-1)       0115JC98
           IF (LGS(30).EQ.-1) WRITE (FU30,1400)
           IF (LGS(30).EQ.-3) WRITE (FU31,1401)
           JF = N3
           DO 20 I = 1, N3
              IF (LLOPT(4).LT.0) JF = I
              IF (LGS(30).EQ.-1) WRITE (FU30,1200) (FTMP(I,J),J=1,JF)   0115JC98
              IF (LGS(30).EQ.-3) WRITE (FU31,1200) (FTMP(I,J),J=1,JF)   0115JC98
   20      CONTINUE
           IF (LGS(30).EQ.-3) WRITE (FU31,1402)
         ELSE
           WRITE (FU30,1500) (FREQ(I+ISHFT),I=1,NFREQ)
         ENDIF
         IF (LLOPT(5).GT.0) WRITE (FU30,1600) (XK3(I),I=1,NFREQ)
         IF (LLOPT(5).GT.1) WRITE (FU30,1700) (XK4(I),I=1,NFREQ)
         IF (LLOPT(4).EQ.0) WRITE (FU30,1900) -FREQ(1)
C
C
      ELSEIF (IOP.EQ.6) THEN
         IF (LGS(30).EQ.-1) WRITE (FU30,2000)
         ISAVEP =  DINT((SLP-SLM)/(DEL*DBLE(INH)))                      1105PF97
         IF (LGS(30).EQ.-1) WRITE (FU30,*) ISAVEP                       1024YC96
      ELSEIF (IOP.EQ.9) THEN                                            0917JC97
C
C    Points along MEP
C
         IF (LGS(34) .NE. 0) THEN                                       11/20T87
            ISHFT = 1                                                      ..
         ELSE IF (ICODE(5).EQ.3) THEN                                   11/20T87
            ISHFT = 6
         ELSE
            ISHFT = 7
         ENDIF
         NFREQ = N3 - ISHFT
         IF (LGS(30).EQ.-1) THEN
         WRITE (FU30,2100) S,V
         IF (LLOPT(3).EQ.0.OR.LLOPT(3).EQ.-1.or.LLOPT(3)
     *        .eq.100.or.llopt(3).eq.99) THEN
C
C   Write out unscaled X     1026YC96
C
            WRITE (FU30,1300) (X(I)/AMASS(I),I=1,N3)
         ELSE
C
C   Write out mass-scaled X  1026YC96
C
            WRITE (FU30,1300) (X(I),I=1,N3)
         ENDIF
C
         IF (LLOPT(3).EQ.0.OR.LLOPT(3).LE.-2) THEN
C
C   Write out unscaled DX, un-normalized  1026YC96
C
            WRITE (FU30,2200) (DX(I)*AMASS(I)*DXMAG,I=1,N3)
         ELSE IF (LLOPT(3).EQ.99) THEN
C
C   Write out mass-scaled forces, un-normalized   1026YC96
C
            WRITE (FU30,2200) (-DX(I)*DXMAG,I=1,N3)
         ELSE IF (LLOPT(3).EQ.100) THEN
C
C   Write out unscaled, un-normalized forces  1026YC96
C
            WRITE (FU30,2200) (-DX(I)*AMASS(I)*DXMAG,I=1,N3)
C
C   Write out mass-scaled DX, un-normalized   1026YC96
C
         ELSE 
            WRITE (FU30,2200) (DX(I)*DXMAG,I=1,N3)
         ENDIF
         WRITE (FU30,1000) (I,I=1,NFREQ)
         ENDIF
         IF (LLOPT(4).NE.0) THEN
           IF (ABS(LLOPT(4)).EQ.1) CALL RPHTRF (N3,AMASS,FTMP,-1)       0115JC98
           IF (LGS(30).EQ.-1) WRITE (FU30,1400)
           IF (LGS(30).EQ.-3) WRITE (FU31,1401)
           JF = N3
           DO 30 I = 1, N3
              IF (LLOPT(4).LT.0) JF = I
              IF (LGS(30).EQ.-1) WRITE (FU30,1200) (FTMP(I,J),J=1,JF)   0115JC98
              IF (LGS(30).EQ.-3) WRITE (FU31,1200) (FTMP(I,J),J=1,JF)   0115JC98
   30      CONTINUE
           IF (LGS(30).EQ.-3) WRITE (FU31,1402)
         ELSE
           WRITE (FU30,1500) (FREQ(I+ISHFT),I=1,NFREQ)
         ENDIF
         IF (LLOPT(5).GT.0) WRITE (FU30,1600) (XK3(I),I=1,NFREQ)
         IF (LLOPT(5).GT.1) WRITE (FU30,1700) (XK4(I),I=1,NFREQ)
      ENDIF
      RETURN
C
 1000 FORMAT (1X, 26I3)                                                 1125JC97
 1098 FORMAT (1X, 'Error: Anharmonicity not compatible with writefu31')
 1099 FORMAT (1X, 'Error: Hessian matrix has to be written in unit 31')
 1100 FORMAT (' IOP =', I5, ', S, V=')
 1101 FORMAT (//, 1X, '*REACT1')
 1102 FORMAT (//, 1X, '*REACT2')
 1103 FORMAT (//, 1X, '*PROD1')
 1104 FORMAT (//, 1X, '*PROD2')
 1105 FORMAT (/, 1X, '  ENERXN',5X, 3E20.10)
 1106 FORMAT (/, 1X, '  ENESAD',5X, 3E20.10)
 1107 FORMAT (//, 1X, '*WELLR')
 1108 FORMAT (//, 1X, '*WELLP')
 1200 FORMAT (1X, 1PE19.10, 2E20.10)
 1207 FORMAT (/, 1X, '  ENEWELLR',5X, 3E20.10)
 1208 FORMAT (/, 1X, '  ENEWELLP',5X, 3E20.10)
 1300 FORMAT (' X='/ (1X, 1PE19.10, 2E20.10))
 1400 FORMAT (' F MATRIX (PACKED)=')
 1401 FORMAT (/, 1X, '  HESSIAN')
 1402 FORMAT (1X, '  END')
 1500 FORMAT (' FREQUENCIES='/ (1X, 1PE19.10, 3E20.10))
 1600 FORMAT (' XK3='/ (1X, 1PE19.10, 3E20.10))
 1700 FORMAT (' XK4='/ (1X, 1PE19.10, 3E20.10))
 1800 FORMAT (' SADDLE POINT,  V=')
 1801 FORMAT (//, 1X, '*SADDLE')
 1900 FORMAT (' WSTAR='/ 1PE19.10)
 2000 FORMAT (' BEGIN POINTS ALONG REACTION COORDINATE')
 2100 FORMAT (' S,V='/ (1X, 1PE19.10, 3E20.10))
 2200 FORMAT (' DX='/ (1X, 1PE19.10, 2E20.10))
 2300 FORMAT (' You have selected the gradient to NOT be mass weighted',
     */1X,'and to be written at two points on either side of the',
     *   ' save grid.',
     */1X,'However, the two extra gradient vectors are written in ',
     */1X,'routine PATH and are forced to be mass weighted.')
C
      END SUBROUTINE rphwrt
C
C***********************************************************************
C  RQCOM
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE rqcom (NATMR1,NATMR2,INDEX,IS,CMRQ)
      use common_inc
      use perconparam
      use rate_const, only : geom
C
C     THIS SUBROUTINE CALCULATES THE DISTANCE BETWEEN THE CENTERS
C     OF MASSES OF TWO REACTING SPECIES. NATMR1 AND NATMR2 ARE THE
C     NUMBERS OF ATOMS IN THE FIRST AND SECOND SPECIES. INDEX CONTAINS
C     THE INDICES OF THE ATOMS IN THE SPECIES STARTING FROM THE
C     FIRST REACTANT.
C
C     CALLED BY:
C                RATE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION INDEX(10),XQ(N3TM),XCMR1(3),XCMR2(3),ATMASS(10)
C
C     READ THE MASS-SCALED COORDINATES FROM GEOM(N3TM,NSDM) AND TRANSFER
C     THEM TO THE CARTESIAN COORDINATES.
C
      DO 10 I = 1, N3
         XQ(I) = GEOM(I,IS)
         XQ(I) = XQ(I)/AMASS(I)
   10 CONTINUE
C
C     TMASR1 AND TMASR2 ARE THE TOTAL MASSES OF THE TWO REACTANTS.
C     XCMR1 AND XCMR2 CONTAINS THE COORDINATES OF THE CENTERS OF MASSES.
C
C     CALCULATE THE ATOMIC MASSES FROM AMASS(N3TM) AND REDM
C
      DO 20 I = 1, NATMR1
         IX = INDEX(I)
         KX = IX*3
         ATMASS(IX) = REDM*(AMASS(KX)**2)
   20 CONTINUE
      IN = NATMR1+1
      IEND = NATMR1+NATMR2
      DO 30 I = IN, IEND
         IX = INDEX(I)
         KX = IX*3
         ATMASS(IX) = REDM*(AMASS(KX)**2)
   30 CONTINUE
C
C
      TMASR1 = 0.0D0
      DO 40 I = 1, NATMR1
         IX = INDEX(I)
         TMASR1 = TMASR1+ATMASS(IX)
   40 CONTINUE
      DO 60 I = 1, 3
         XCMR1(I) = 0.0D0
         J = 3-I
         DO 50 K = 1, NATMR1
            IX = INDEX(K)
            KX = IX*3-J
            XCMR1(I) = XCMR1(I)+ATMASS(IX)*XQ(KX)
   50    CONTINUE
         XCMR1(I) = XCMR1(I)/TMASR1
   60 CONTINUE
      TMASR2 = 0.0D0
      DO 70 I = IN, IEND
         IX = INDEX(I)
         TMASR2 = TMASR2+ATMASS(IX)
   70 CONTINUE
      DO 90 I = 1, 3
         XCMR2(I) = 0.0D0
         J = 3-I
         DO 80 K = IN, IEND
            IX = INDEX(K)
            KX = IX*3-J
            XCMR2(I) = XCMR2(I)+ATMASS(IX)*XQ(KX)
   80    CONTINUE
         XCMR2(I) = XCMR2(I)/TMASR2
   90 CONTINUE
C
C     NOW CALCULATE THE DISTANCE BETWEEN THE CENTERS OF MASSES
C
      CMRQ2 = 0.0D0
      DO 100 I = 1, 3
         CMRQ2 = CMRQ2+(XCMR1(I)-XCMR2(I))**2
  100 CONTINUE
      CMRQ = SQRT(CMRQ2)
      RETURN
      END SUBROUTINE rqcom 
C
C***********************************************************************
C  RSPDRV
C***********************************************************************
C
      SUBROUTINE rspdrv (NM,N,A,W,MATZ,Z,FV1,FV2,IERR)
      use perconparam
C
C     PATCH ROUTINE USING RSP TO DO DIAGONALIZATION
C     Subroutine name has been changed from 'RST' since Ver 5.0
C
C     CALLED BY:
C                FDIAG, INTPM
C     CALLS:
C            RSPP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)      
C
      DIMENSION A(NM,N),W(N),Z(NM,N),FV1(N),FV2(N)
      DIMENSION B(N3TM*(N3TM+1)/2)
      NV = N*(N+1)/2
      L = 1
      DO 10 J = 1, N
         DO 10 I = 1, J
            B(L) = A(I,J)
            L = L+1
   10 CONTINUE
      CALL RSPP (NM,N,NV,B,W,MATZ,Z,FV1,FV2,IERR)
      RETURN
      END SUBROUTINE rspdrv

      SUBROUTINE rspp (NM,N,NV,A,W,MATZ,Z,FV1,FV2,IERR)
C
C     CALLED BY:
C                 RSPDRV
C     CALLS:
C                 TQL2,TQLRAT,TRBAK3,TRED3
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(NV),W(N),Z(NM,N),FV1(N),FV2(N)
C
C     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF
C     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK)
C     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED)
C     OF A REAL SYMMETRIC PACKED MATRIX.
C
C     ON INPUT-
C
C        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C        DIMENSION STATEMENT,
C
C        N  IS THE ORDER OF THE MATRIX  A,
C
C        NV  IS AN INTEGER VARIABLE SET EQUAL TO THE
C        DIMENSION OF THE ARRAY  A  AS SPECIFIED FOR
C        A  IN THE CALLING PROGRAM.  NV  MUST NOT BE
C        LESS THAN  N*(N+1)/2,
C
C        A  CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
C        PACKED MATRIX STORED ROW-WISE,
C
C        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF
C        ONLY EIGENVALUES ARE DESIRED,  OTHERWISE IT IS SET TO
C        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS.
C
C     ON OUTPUT-
C
C        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER,
C
C        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO,
C
C        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN
C        ERROR COMPLETION CODE DESCRIBED IN SECTION 2B OF THE
C        DOCUMENTATION.  THE NORMAL COMPLETION CODE IS ZERO,
C
C        FV1  AND  FV2  ARE TEMPORARY STORAGE ARRAYS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DATA ZERO,ONE / 0.0D0,1.0D0 /
      IF (N.LE.NM) GO TO 10
      IERR = 10*N
      GO TO 60
   10 IF (NV.GE.(N*(N+1))/2) GO TO 20
      IERR = 20*N
      GO TO 60
C

   20 CALL TRED3 (N,NV,A,W,FV1,FV2)
      IF (MATZ.NE.0) GO TO 30
C
C     ********** FIND EIGENVALUES ONLY **********
C
      CALL TQLRAT (N,W,FV2,IERR)
      
      GO TO 60
C
C     ********** FIND BOTH EIGENVALUES AND EIGENVECTORS **********
C
   30 DO 50 I = 1, N
C
         DO 40 J = 1, N
            Z(J,I) = ZERO
   40    CONTINUE
C
         Z(I,I) = ONE
   50 CONTINUE
C
      CALL TQL2 (NM,N,W,FV1,Z,IERR)
      IF (IERR.NE.0) GO TO 60
      CALL TRBAK3 (NM,N,NV,A,N,Z,IERR)
   60 RETURN
C
C     ********** LAST CARD OF RSP **********
C
      END subroutine rspp

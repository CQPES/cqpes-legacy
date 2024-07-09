C***********************************************************************
C  MEPINV
C***********************************************************************
C
      SUBROUTINE mepinv
      use common_inc
      use perconparam
      use keyword_interface, only : gufac6,iunit6
      use rate_const
      use cm; use sst
C                     
C     Invert the arrays which store MEP information if ISEN = 1
C     This subroutine is added in version 5.0 to invert the
C     MEP storage arrays.
C     MEPSRT is no longer used in PATH
C
C     CALLED BY:
C                PATH
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
c       save                                                            0601YC98
C
      CALL INVRT(LSAVE,SSUBI)
      IF (LGS(3) .LT. 0) RETURN
C
      NSHLF = LSAVE - NSHLF + 1
      N3M7 = NF(5) 
      AFLAG = '   '
C
      DO 10  J = 1, N3M7
         DO 10 JJ = 1, NARR                       
            IF (MODETS(JJ,J).NE.0) AFLAG = 'SET'  
10    CONTINUE
C                                             
      CALL INVRT(LSAVE,VCLAS)
      CALL INVRT(LSAVE,VADIB)
      CALL INVRT(LSAVE,FMITS)
      CALL INVRT(LSAVE,CDSCMU)
      CALL INVRT(LSAVE,ZOCMCD) 
      CALL INVRT(LSAVE,EGRND)                                           0513WH93
      CALL INVRT(LSAVE,SBKAP)                                           0202YC98
C      CALL INVRT(LSAVE,XLCDSC)
C      CALL INVRT(LSAVE,XHCDSC)
C
      CALL INVRT2(N3,LSAVE,N3TM,GEOM)
      CALL INVRT2(N3M7,LSAVE,NVIBM,BCUR)                                0601YC98
      IF (LLCG) THEN
         CALL INVRT2(N3,LSAVE,N3TM,DXSV)
c         CALL INVRT2(N3M7,LSAVE,NVIBM,BCUR)                            0601YC98
         CALL INVRT3(N3,N3,LSAVE,N3TM,N3TM,COFSV) 
      ENDIF
C 
      CALL INVRT2(N3M7,LSAVE,NVIBM,WETS)
      CALL INVRT2(N3M7,LSAVE,N3TM,EFNDT)                                0513WH93
      IF (AFLAG .EQ. 'SET') THEN
         CALL INVRT2(N3M7,LSAVE,NVIBM,XETS)
         CALL INVRT2(N3M7,LSAVE,NVIBM,Y0TS)         
         CALL INVRT2(N3M7,LSAVE,N3TM,FMIHTS)
      ENDIF
      IF (LGS(33) .EQ. 1) CALL INVRT2(N3M7,LSAVE,NVIBM,EWKB0)
C
      IF (LSST .EQ. 1) THEN                                             0101JZ13
        CALL INVRT2(N3TM,LSAVE,N3TM,DBW)
        CALL INVRT2(NTOR,LSAVE,NTOR,TORBH)
        CALL INVRT(LSAVE,DETDS)
      ENDIF
      RETURN
      END SUBROUTINE mepinv           
C
C**********************************************************************
C  MEPOUT
C**********************************************************************
C
      SUBROUTINE mepout
      use perconparam; use common_inc
      use keyword_interface, only : gufac6,iunit6
      use rate_const
C
C     Called by : PATH,ELRPH,ZOCUPD,RESTOR
C
C     This subroutine prints the classical energy, adiabatic
C     ground state energy, mu effective, and normal mode frequencies
C     along the MEP to FORTRAN unit fu6.

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER     IWE(NVIBM)
      CHARACTER*1 IMA(NVIBM)
      save                                                              0601YC98
C
      N3M7 = NF(5)
C     WRITE (FU6,1000)
      IF(IUNIT6.EQ.1) WRITE(FU6,1000)                                   0405JZ07
      IF(IUNIT6.EQ.0) WRITE(FU6,1010)                                   0405JZ07
      DO 200 I = 1, LSAVE
         DO 100 J = 1, N3M7
            IWE(J) = NINT(WETS(J,I)*AUTOCM)
            IF (IWE(J) .GE. 0) THEN
               IMA(J) = ' '
            ELSE
               IWE(J) = -IWE(J)
               IMA(J) = 'i'
            ENDIF
  100    CONTINUE
C
C        WRITE (FU6,1100) SSUBI(I),VCLAS(I)*CKCAL,VADIB(I)*CKCAL,
C    *                    CDSCMU(I),(IWE(J),IMA(J),J=N3M7,1,-1)
         WRITE (FU6,1100) SSUBI(I)/GUFAC6,VCLAS(I)*CKCAL,VADIB(I)*CKCAL,
     *                    CDSCMU(I),(IWE(J),IMA(J),J=N3M7,1,-1)         0405JZ07
  200 CONTINUE
C
      RETURN
C  
1000  FORMAT(/,2X,T5,'Classical and adiabatic energies (kcal/mol), ',   1223WH92
     *               'effective CD-SC reduced ',
     *       /,2X,T5,'mass (mu) (a.u.), and frequencies (cm**-1) vs. s', 
     *       /,1X,T2,'s(bohr)',T14,'VMEP',T23,'Va^G',T31,'mu^CD-SC',    06/96ELC
     *       T39,'   frequencies (cm**-1)', /)
1010  FORMAT(/,2X,T5,'Classical and adiabatic energies (kcal/mol), ',   0405JZ07
     *               'effective CD-SC reduced ',
     *       /,2X,T5,'mass (mu) (a.u.), and frequencies (cm**-1) vs. s',
     *       /,1X,T2,'s(angstrom)',T14,'VMEP',T23,'Va^G',T31,'mu^CD-SC',
     *       T39,'   frequencies (cm**-1)', /)
1100  FORMAT(1X, F7.3, 1X, 2(F9.4, 1X), F8.2, 1X, 7(1X,I4,A1),
     *       /,(38X,7(1X,I4,A1)))
C     
      END SUBROUTINE mepout
C***********************************************************************
C  MEPSRT
C***********************************************************************
C
      SUBROUTINE mepsrt
      use perconparam; use common_inc
      use keyword_interface, only : gufac6,iunit6
      use rate_const
      use cm
C                     
C     DO BUBBLE SORT OF MEP IN ORDER OF INCREASING S.
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C     MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
C   The include file esp.inc has been removed in version 5.0            1021GL92
C   The information for the bond orders and charges computed            1021GL92
C   if LGS(35)=1 has been removed in version 5.0.                       1021GL92
C
C     CALLED BY:
C                PATH
C
C       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
C*
      NSWIT = 1   
      N3M7 = NF(5)                                                      1026WH92
C
   10 CONTINUE
      IF (NSWIT.EQ.0) GO TO 50
      NSWIT = 0
      DO 40 I = 1, LSAVE-1
         IP1 = I+1
         IF (SSUBI(I).LT.SSUBI(IP1)) GO TO 40
         NSWIT = NSWIT+1
         IF (NSHLF.EQ.I) THEN
            NSHLF = IP1
         ELSEIF (NSHLF.EQ.IP1) THEN
            NSHLF = I
         ENDIF
         TEMPX = SSUBI(IP1)
         SSUBI(IP1) = SSUBI(I)
         SSUBI(I) = TEMPX
         TEMPX = VCLAS(IP1)
         VCLAS(IP1) = VCLAS(I)
         VCLAS(I) = TEMPX
         TEMPX = FMITS(IP1)
         FMITS(IP1) = FMITS(I)
         FMITS(I) = TEMPX
         TEMPX = VADIB(IP1)
         VADIB(IP1) = VADIB(I)
         VADIB(I) = TEMPX
         TEMPX = EGRND(IP1)                                             1106YL92
         EGRND(IP1) = EGRND(I)                                          1106YL92
         EGRND(I) = TEMPX                                               1106YL92
C
         TEMPX = CDSCMU(IP1)                                            8/26YL91
         CDSCMU(IP1) = CDSCMU(I)                                        8/26YL91
         CDSCMU(I) = TEMPX                                              8/26YL91
C
         TEMPX = ZOCMCD(IP1)                                            1016WH92
         ZOCMCD(IP1)  = ZOCMCD(I)                                       1016WH92
         ZOCMCD(I) = TEMPX                                              1016WH92  
C
         TEMPX = SBKAP(IP1)                                             0812YC97
         SBKAP(IP1) = SBKAP(I)                                          0812YC97
         SBKAP(I) = TEMPX                                               0812YC97
c         TEMPX = XLCDSC(IP1)                                           0601YC98
c         XLCDSC(IP1) = XLCDSC(I)                                       0601YC98
c         XLCDSC(I) = TEMPX                                             0601YC98
c         TEMPX = XHCDSC(IP1)                                           0601YC98
c         XHCDSC(IP1) = XHCDSC(I)                                       0601YC98
c         XHCDSC(I) = TEMPX                                             0601YC98
         DO 20 J = 1, N3
            TEMPX = GEOM(J,IP1)
            GEOM(J,IP1) = GEOM(J,I)
            GEOM(J,I) = TEMPX
              IF (LLCG) THEN                                            11/18/GL91
               TEMPX = DXSV(J,IP1)       
               DXSV(J,IP1) = DXSV(J,I)    
               DXSV(J,I) = TEMPX           
            DO 18 K = 1, N3             
               TEMPX = COFSV(K,J,IP1)       
               COFSV(K,J,IP1) = COFSV(K,J,I) 
               COFSV(K,J,I) = TEMPX 
   18       CONTINUE 
           ENDIF                                                        11/18/GL91                                            
   20    CONTINUE
C*
        AFLAG = '   '
         DO 30 J = 1, N3M7
            TEMPX = WETS(J,IP1)
            WETS(J,IP1) = WETS(J,I)
            WETS(J,I) = TEMPX
            TEMPX = EFNDT(J,IP1)                                        1106YL92
            EFNDT(J,IP1) = EFNDT(J,I)                                   1106YL92
            EFNDT(J,I) = TEMPX                                          1106YL92
            DO 37 JJ = 1, NARR                                          6/30YL91
               IF (MODETS(JJ,J).NE.0) AFLAG = 'SET'                         ..
37          CONTINUE                                                        ..
            IF (AFLAG.EQ.'   ') GO TO 36                                6/30YL91
               TEMPX = XETS(J,IP1)
               XETS(J,IP1) = XETS(J,I)
               XETS(J,I) = TEMPX
               TEMPX = Y0TS(J,IP1)
               Y0TS(J,IP1) = Y0TS(J,I)
               Y0TS(J,I) = TEMPX 
               TEMPX = FMIHTS(J,IP1)
               FMIHTS(J,IP1) = FMIHTS(J,I)
               FMIHTS(J,I) = TEMPX 
36          CONTINUE
            AFLAG = '  '                                                6/30YL91
            IF (LGS(33).EQ.1) THEN
               TEMPX = EWKB0(J,IP1)
               EWKB0(J,IP1) = EWKB0(J,I)
               EWKB0(J,I) = TEMPX
            ENDIF
c           IF (LLCG) THEN                                              1118GL91
            TEMPX = BCUR(J,IP1)
            BCUR(J,IP1) = BCUR(J,I)
            BCUR(J,I) = TEMPX
c           ENDIF                                                       1118GL91
   30    CONTINUE
   40 CONTINUE                                                           
C    
      GO TO 10
   50 CONTINUE                  
      RETURN
      END SUBROUTINE mepsrt   
C***********************************************************************
C MNBRAK
C***********************************************************************
C Given a function FUNC and given distinct initial points AX,BX. This
C routine searches in the downhill direction and returns new points
C AX,BX,CX which bracket a minimum of the function.
C
C CALLED BY: LINMN
C
      SUBROUTINE mnbrak (AX,BX,CX,FA,FB,FC,FUNC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     EXTERNAL FUNC                                                     2/10/90VM
      EXTERNAL F1DIM 
C
      DATA GOLD /1.618034D0/,GLIMIT /100.0D0/,TINY /1.0D-20/
C
      FA = FUNC(AX)
      FB = FUNC(BX)
      IF(FB .GT. FA) THEN
        DUM = AX
        AX = BX
        BX = DUM
        DUM = FB
        FB = FA
        FA = DUM
      ENDIF
      CX = BX + GOLD*(BX-AX)
      FC = FUNC(CX)
 10   IF (FB .GE. FC) THEN
         R = (BX-AX)*(FB-FC)
         Q = (BX-CX)*(FB-FA)
         U = BX-((BX-CX)*Q-(BX-AX)*R)/
     *       (2.0D0*SIGN(MAX(ABS(Q-R),TINY),Q-R))
         ULIM = BX + GLIMIT*(CX-BX)
         IF((BX-U)*(U-CX) .GT. 0.0D0) THEN
            FU = FUNC(U)
            IF(FU .LT. FC) THEN
               AX = BX
               FA = FB
               BX = U
               FB = FU
               GOTO 10
            ELSEIF(FU .GT. FB) THEN
               CX = U
               FC = FU
               GOTO 10
            ENDIF
            U = CX + GOLD*(CX-BX)
            FU = FUNC(U)
         ELSEIF ((CX-U)*(U-ULIM) .GT. 0.0D0) THEN
            FU = FUNC(U)
            IF(FU .LT. FC) THEN
               BX = CX
               CX = U
               U = CX + GOLD*(CX-BX)
               FB = FC
               FC = FU
               FU = FUNC(U)
            ENDIF
         ELSEIF ((U-ULIM)*(ULIM-CX) .GE. 0.0D0) THEN
            U = ULIM
            FU = FUNC(U)
         ELSE
            U = CX + GOLD*(CX-BX)
            FU = FUNC(U)
         ENDIF
         AX = BX
         BX = CX
         CX = U
         FA = FB
         FB = FC
         FC = FU
         GOTO 10
      ENDIF
      RETURN
      END SUBROUTINE mnbrak
C
C***********************************************************************
C  MUCDSC
C***********************************************************************
C
      SUBROUTINE mucdsc (N3M7,LDEL,IFLAG,LCOUNT,BKAP)
      use perconparam, only : n3tm,fu6
      use common_inc
      use keyword_interface, only : gufac6,iunit6
      use rate_const
C
C Computes mueff for cdscsag
C    Because of derivs of tps, we now get results at previous save
C        point unless IFLAG .gt. 0, signifying end of path
C
C
C     CALLED BY:
C                BCALC
C     CALLS:
C           TPCDSC,QUADFT
C
C    MODIFICATION FOR INCLUDE FILES DONE 26/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LWRITE
C
      DIMENSION Z(3),ATP(3),STPC(3),TPC(3,2),BKAPM(2),FREDMY(N3TM)
      DIMENSION TAB(3)                                                  0810JC97
C
      SAVE STPC,TPC,BKAPM
C
C
C
C If at end of the grid reuse turning point and zero-point energy
C    derivative info from previous points
C
C MI-VTST spline interpolation of the mueff                             0804JC97
C                                                                       0804JC97
      IF (LGS(30).GT.0.AND.LOPT(2).EQ.-500) THEN                        0810JC97
        CALL SPLNMF (S,TAB)                                             0804JC97
        CDSCMU(LSAVE) = TAB(1)                                          0804JC97
        RETURN                                                          0804JC97
      ENDIF                                                             0804JC97
C                                                                       0804JC97
C
      LWRITE = .FALSE.
      IF (LGS(4).NE.0) THEN
         IF (SOB.GT.SOE) THEN
           IF (S.GE.SOE.AND.S.LE.SOB) LWRITE = .TRUE.
         ELSE
           IF (S.GE.SOB.AND.S.LE.SOE) LWRITE = .TRUE.
         END IF
      END IF
      ISHFT = N3 - N3M7
      KK = 0                                                            9/18YL92
117   KK = KK + 1                                                       9/18YL92
      DO 45 I = 1, N3                                                   9/18YL92
         IF (KK .EQ. 1) THEN                                            9/18YL92
            FREDMY(I) = FREQ(I)                                         9/18YL92
         ELSE                                                               ..
            SDUMMY = SSUBI(LSAVE)                                           ..
            IDUMMY = I - ISHFT                                              ..
            IF (I.GT.ISHFT) THEN
               if (LGS2(10).EQ.0) THEN
                           FREDMY(I)=FREQ(I)+ZOCFRE(IDUMMY,SDUMMY)      10/1WH92
               else IF (LGS2(10).EQ.1) THEN
                           FREDMY(I)=FREQ(I)*ZOCFRE(IDUMMY,SDUMMY)      03/96/YC
               else IF (LGS2(10).EQ.2) THEN
                           FREDMY(I)=FREQ(I)*EXP(ZOCFRE(IDUMMY,SDUMMY)) 03/96/YC
               else IF (LGS2(10).EQ.3) THEN
                           FREDMY(I)=FREQ(I)
               ENDIF
            ENDIF
         ENDIF                                                              ..
45    CONTINUE                                                          9/18YL92
      IF (IFLAG.EQ.0) THEN
         CALL TPCDSC(BCURV,BKAP,FREDMY,N3,REDM,TP)
         TPC(1,KK) = TPC(2,KK)                                              ..
         TPC(2,KK) = TPC(3,KK)                                              ..
         TPC(3,KK) = TP                                                     ..
         IF (KK .EQ. 1) THEN  
            STPC(1) = STPC(2)
            STPC(2) = STPC(3)
            STPC(3) = SSUBI(LSAVE)
         ENDIF 
         IF (LWRITE) WRITE (FU6,1000) TPC(3,KK)
      ENDIF
      IF (KK.LT.2.AND.LGS2(11).NE.0) GO TO 117                          9/18YL92
C
C If first time through skip to end; effective mass factors are
C    computed for the first grid point in special section of code
C    when LCOUNT=3.
C
      IKK = 1                                                           9/18YL92
      IF (LGS2(11).NE.0) IKK = 2                                        9/18YL92
      DO 119 KK = 1, IKK                                                9/18YL92
C
C If second time through coumpute overlaps then skip end; effective
C    mass factors are computed for the second grid point on the next
C    pass (i.e. for LCOUNT=3) but in the normal section of code.
C
        IF (LCOUNT.GT.2) THEN                                           9/18YL92
C
C LCOUNT > 2, therefore compute effective mass factors for the current
C    grid point if IFLAG = 0, otherwise for the previous grid point.
C
            PROD = 1.0D0
C
            IF (IFLAG.EQ.0) THEN
C
C  Compute quantities at previous grid point
C
                ARG = TPC(2,KK)*BKAPM(1)                                9/18YL92
                ARG2 = ARG*ARG
C
C  Derivative of the turning pt. wrt s
C
                DO 20 J = 1, 3
                   ATP(J) = ABS(TPC(J,KK))                              9/18YL92
   20           CONTINUE
                CALL QUADFT (STPC,ATP,Z)
                W = Z(2)+2.0D0*Z(3)*STPC(2)
            ELSE
C
C  At end of grid, compute quantities at current grid point
C
                ARG = TPC(3,KK)*BKAP                                    9/18YL92
                ARG2 = ARG*ARG
C
C  Derivative of the turning pt. wrt s
C
                DO 30 J = 1, 3
                   ATP(J) = ABS(TPC(J,KK))                              9/18YL92
   30           CONTINUE
                CALL QUADFT (STPC,ATP,Z)
                W = Z(2)+2.0D0*Z(3)*STPC(3)
            ENDIF
C
C  SC factor
C
            W2 = W*W
            XARG = -2.0D0*ARG-ARG2+W2
            IF (XARG.LT.0.0D0) PROD = PROD*EXP(XARG)
C
C  SC effective mass
C
            XMUEXP = REDM*PROD
            IF (IFLAG.EQ.0) THEN
C
C  Save effective masses at previous grid point
C
               LRES = LSAVE-LDEL
            ELSE
C
C  Save effective masses at current grid point
C
               LRES = LSAVE
            ENDIF
            IF (KK.EQ.1) THEN                                           9/18YL92
               CDSCMU(LRES) = XMUEXP                                    9/18YL92
c               XLCDSC(LRES) = XARG                                     0601YC98
            ELSE                                                        9/18YL92
               ZOCMCD(LRES) = XMUEXP                                    9/18YL92
c               XHCDSC(LRES) = XARG                                     0601YC98
C              WRITE(FU6,*)LRES,ZOCMCD(LRES)                            0601YC98
            ENDIF                                                       9/18YL92
            IF (IFLAG.EQ.0.AND.LCOUNT.EQ.3) THEN
C
C  Extra section to generate results at first save point
C
               PROD = 1.0D0
               ARG = TPC(1,KK)*BKAPM(2)                                 9/18YL92
               ARG2 = ARG*ARG 
C
C  Derivative of the turning pt. wrt s
C
               DO 50 J = 1, 3
                  ATP(J) = ABS(TPC(J,KK))                               9/18YL92
   50          CONTINUE
               CALL QUADFT (STPC,ATP,Z)
               W = Z(2)+2.0D0*Z(3)*STPC(1)
               W2 = W*W
C
C  SC factor
C
               XARG = -2.0D0*ARG-ARG2+W2
               IF (XARG.LT.0.0D0) PROD = PROD*EXP(XARG)
               XMUEXP = REDM*PROD                                       9/18YL92
               LRES = LSAVE-2*LDEL
C
C  SC factor
C
               IF (KK.EQ.1) THEN                                        9/18YL92
                  CDSCMU(LRES) = XMUEXP                                     ..
C                 WRITE(FU6,*)LRES,CDSCMU(LRES)
               ELSE                                                         ..
                  ZOCMCD(LRES) = XMUEXP                                     ..
C                 WRITE(FU6,*)LRES,ZOCMCD(LRES)
               ENDIF                                                    9/18YL92
            ENDIF
        ENDIF
119   CONTINUE                                                          9/18YL92
C
C Shift storeage of BFm's and save current coefficient matrix
C
      BKAPM(2) = BKAPM(1)
      BKAPM(1) = BKAP
      RETURN
C
 1000 FORMAT (/,'The centrifugal-dominant turning point =', 8X,1P,
     * 7E15.7, : / (25X,1P,7E15.7))
C
      END subroutine mucdsc
C
C***********************************************************************
C  MXLNEQ
C***********************************************************************
C
      SUBROUTINE mxlneq (A,NN,IDA,DETT,JRANK,EPS,IN,MM,NABSM)
      use perconparam, only : fu6
C
C PROGRAMMED BY R. HOTCHKISS, U. COMP. CTR., U. OF MINN., REVISED OCT.73
C      modified for the VAX by Bruce Garrett, Nov. 1980
C*
C*    Dimensions changed by Tom Joseph Jan. 1988.
C*
C*    Solves for the matrix X in C*X = B. C must be a sq. matrix
C*    A(NN,NN+abs(MM)) contains matrices C and B, A(i,j) = C(i,j)
C*    A(i,j+NN) = B(i,j)
C*    IDA - first dim. of A, NN - order of matrix C (ie. # of eqns.)
C*    IN(NN) - work array
C*    M = 0 is used to calculate inverse of C   
C*    Notice that NABSM = NN + ABS(MM)                                  9/20DL90
C     CALLED BY:
C                FIVPT,NEWT,PROJCT,TREPT
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION MACHEP
C     DIMENSION A(IDA,NABSM),IN(IDA)                                    9/20DL90
      DIMENSION A(IDA,NABSM),INX(IDA)                                   9/20DL90
C
C     DATA ZERO,ONE/0.0D0,1.0D0/,MACHEP/1.0D35/
C     REVISED BY N.ABUSALBI TO ALLOW A DET OF THE ORDER OF 10**37
C
      DATA ZERO,ONE / 0.0D0,1.0D0 /,MACHEP / 1.0D+38 /
      DATA INTMX / 400000 /
      save                                                              0601YC98
C
C INITIATE SOME LOCAL AND OUTPUT VARIABLES
C
      JRANK = NN
      N = NN
      ID = IDA
      OPS = EPS
C
C error checking
C
      IF (N.GT.0.AND.N.LE.ID) GO TO 10
      WRITE (FU6,1700)
      WRITE (FU6,1000) N,ID
      STOP 'MXLNEQ 1'
   10 IF (ID.GE.N.AND.ID.LT.INTMX) GO TO 20
      WRITE (FU6,1700)
      WRITE (FU6,1100) ID,N,INTMX
      STOP 'MXLNEQ 2'
   20 IF ((ONE+OPS).NE.ONE.AND.OPS.GE.ZERO) GO TO 30
      WRITE (FU6,1700)
      WRITE (FU6,1200) OPS
      STOP 'MXLNEQ 3'
   30 MOD = MM
      NM = ABS(MM)
      IF (NM.LT.INTMX) GO TO 40
      WRITE (FU6,1700)
      WRITE (FU6,1300) MM,INTMX
      STOP 'MXLNEQ 4'
   40 NM = NN+NM
      NMIDA = NM*ID*2
      IF (NMIDA.LT.INTMX) GO TO 50
      WRITE (FU6,1700)
      WRITE (FU6,1400) NMIDA,INTMX
      STOP 'MXLNEQ 5'
   50 CONTINUE
C
C  end of error checking
C CONTINUE TO INITIALIZE
C
      K1 = 1
      NFLAG = 0
      DET = ONE
C
C MOD IS -1 DET ONLY (this version does not have this option)
C        = or > 0 FOR INV, DET AND 0 OR MORE SETS OF LIN EQNS
C        <-1 FOR LIN EQNS ONLY AND DET
C MAIN GAUSS JORDAN LOOP BEGINS
C
      DO 150 K = 1, N
C
C SEARCH FOR LARGEST PIVOT CANDIDATE IN REMAINING LOWER RIGHT SQUARE
C  MATRIX
C
         PIV = ZERO
         L = K
         DO 60 I = K, N
            P = ABS(A(I,K))
            IF (PIV.GE.P) GO TO 60
            PIV = P
            L = I
   60    CONTINUE
C
C PIVOT WITH ABS VALUE PIV AND SUBSCRIPTS L AND M HAS BEEN FOUND
C
         PIVOT = A(L,K)
C
C CONTINUE IF PIV LARGER THAN USER EPS
C
         IF (PIV.GT.OPS) GO TO 80
         IF (EPS.EQ.OPS) JRANK = K-1
C
C EPS TEST FAILED, CHECK FOR ZERO PIVOT
C
         IF (PIV.GT.ZERO) GO TO 70
C
C PIVOT IS ZERO, TERMINATE PROGRAM UNLESS MOD=-1,IE, DET ONLY CASE
C      this version does not have the det only mode
C ZERO PIVOT MEANS ZERO DET AND EXIT IF DET ONLY MODE
C
         WRITE (FU6,1700)
         WRITE (FU6,1500) K
         STOP 'MXLNEQ 6'
C
C ISSUE NON-FATAL MESSAGE, PIV .LE. EPS
C
   70    WRITE (FU6,1700)
         WRITE (FU6,1600) K,EPS
C
C SET OPS TO 0 SO SOLUTION MAY CONTINUE AFTER ERROR MESSAGE
C
         OPS = ZERO
         PIV = PIVOT
C
C CALCULATE DETERMINANT AND CHECK FOR OVERFLOW
C
   80    DET = PIVOT*DET
         IF (ABS(DET).LT.MACHEP) GO TO 90
         WRITE (FU6,1700)
         WRITE (FU6,1800) K,DET
         STOP 'MXLNEQ 7'
C
C RESET LEADING ROW DO INDEX FOR DET ONLY AND LIN EQN ONLY CASE
C
   90    IF (MOD.LT.0) K1 = K
C
C SAVE PIVOT INDEX
C
         INX(K) = L
C
C CHECK FOR ROW INTERCHANGE
C
         IF (L.EQ.K) GO TO 110
         DET = -DET
C
C INTERCHANGE ROW CONTAINING PIVOT AND CURRENT ROW
C ONLY PARTIAL ROWS NEED BE EXCHANGED FOR DET ONLY OR LIN EQN ONLY
C  SINCE LOWER LEFT PARTIALLY FORMED TRIANGLE IS NOT NEEDED
C
         DO 100 J = K1, NM
            Z = A(L,J)
            A(L,J) = A(K,J)
            A(K,J) = Z
  100    CONTINUE
C
C PIVOT ELEMENT IS NOW ON DIAGONAL
C SAVE DIVISION TIME BY USING RECIPROCAL OF PIVOT
C
  110    PIVOT = ONE/PIVOT
C
C PRE-DIVIDE NECESSARY PORTION OF PIVOT ROW
C
         DO 120 J = K1, NM
            A(K,J) = A(K,J)*PIVOT
  120    CONTINUE
C
C SET PIVOT ELEMENT TO ZERO SO MAIN REDUCTION STEP DOESNT OPERATE ON
C  PIVOT ROW
C
         A(K,K) = ZERO
C
C SWEEP THROUGH ALL OR PART OF MATRIX USING KTH ROW, PIVOT ROW, TO
C  REDUCE THE MATRIX
C
         DO 140 I = K1, N
            Z = A(I,K)
            IF (Z.EQ.ZERO) GO TO 140
C
C THIS CHECK NOT ONLY PREVENTS OPERATING ON PIVOT ROW BUT CATCHES
C  OTHER ZEROES IN PIVOT COLUMNS. THESE OTHER ZEROES WOULD LEAVE JTH
C  ROW UNCHANGED IN FOLLOWING LOOP SO CONSIDERABLE TIME MAY BE SAVED BY
C  SKIPPING OPERATION
C
            DO 130 J = K1, NM
               A(I,J) = A(I,J)-Z*A(K,J)
  130       CONTINUE
C
C THE INVERSE IS CREATED IN PLACE BY SUBSTITUTING AN IDENTITY MATRIX
C  COL BY COL, SINCE WE ARE SUBT. THE PIVOT ROW FROM OFF DIAGONAL 0
C  ELEMENTS AT THIS POINT, WE NOW PLACE -A(I,K)/A(K,K) AT THIS POINT IN
C  THE PIVOT COL
C
            A(I,K) = -Z*PIVOT
  140    CONTINUE
C
C SIMILARLY DIVIDING PIVOT ROW BY THE PIVOT IS EQUIVALENT TO PLACING
C  ONE/A(K,K) AT THE PIVOT POINT FOR THE INVERSE
C
         A(K,K) = PIVOT
  150 CONTINUE
      IF (N.EQ.1) GO TO 210
      IF (MOD.GE.0) GO TO 180
C
C BACK SUBSTITUTION FOR LIN EQN ONLY CASE
C
      K1 = K1+1
      DO 170 K = K1, NM
         I = N
         DO 170 L = 2, N
            I1 = I
            I = I-1
            Z = ZERO
            DO 160 J = I1, N
               Z = Z+A(I,J)*A(J,K)
  160       CONTINUE
            A(I,K) = A(I,K)-Z
  170 CONTINUE
      GO TO 210
C
C FINAL REORDERING OF MATRIX
C
  180 K = N
C
C SKIP LAST STEP SINCE NO INTERCHANGE COULD OCCUR THERE
C
      DO 200 J = 2, N
C
C PERFORM INTERCHANGES IN EXACT REVERSE ORDER OF PREVIOUS EXECUTION
C
         K = K-1
C
C ROW INTERCHANGE DURING INVERSION IMPLIES COL INTERCHANGE HERE
C
         M = INX(K)
         IF (M.EQ.K) GO TO 200
C
C COL INTERCHANGE
C
         DO 190 I = 1, N
            Z = A(I,K)
            A(I,K) = A(I,M)
            A(I,M) = Z
  190    CONTINUE
  200 CONTINUE
  210 DETT = DET
      RETURN
C
 1000 FORMAT(11H arg 2, n = ,I10/7X,4H id=,I10/
     *       29H n must be .ge. 1 and .le. id)
 1100 FORMAT(12H arg 3, id = ,I10 /9X,3H n=,I10/
     *       27H id must be .ge. N and .le.,I10)
 1200 FORMAT(13H arg 6, eps = ,1PE13.5/
     *       33H eps must be .ge. zero and finite)
 1300 FORMAT(11H arg 8, m = ,I10/
     *   20H abs(m) must be .le. ,I10 )
 1400 FORMAT(7H size = ,I10/
     *       36H size = id*(n+abs(m))*2 must be .le. ,I10)
 1500 FORMAT(4H k =,I10/
     *       46H at step k a gauss-jordan pivot value was zero)
 1600 FORMAT(4H k = ,I10/6H eps = ,1PE13.5/
     *   50H at step k a gauss-jordan pivot value was .le. eps )
 1700 FORMAT(15H *** mxlneq *** )
 1800 FORMAT(4H k = ,I10/6H det = ,1PE13.5/
     *        27H at step k det is too large )
C
      END SUBROUTINE mxlneq
C
C***********************************************************************
C  NEXTPT
C***********************************************************************
C
      SUBROUTINE nextpt
      use common_inc
      use perconparam
      use rate_const, only : fsv,del,inh
      use keyword_interface, only : gufac6,iunit6
C
C     ROUTINE THAT COMPUTES THE NEXT POINT ON THE MEP FOR RPH INPUT DATA--
C     ALLOWS ONE TO USE THE VARIOUS GRADIENT FOLLOWING TECHNIQUES CODED IN
C     POLYRATE TO DETERMINE THE NEXT GEOMETRY FOR AN ELECTRONIC STRUCTURE
C     CALCULATION.
C
C     ADDED TO POLYRATE V 1.6 ON 6/5/89
C
C     CALLED BY:
C          MAIN
C     CALLS:
C          INTEGR
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 02/07/91
C   FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     THE LAST GEOMETRY READ IN FROM FILE #30 IS CURRENTLY STORED IN X() AND
C     THE DERIVATIVE IS IN DX().  THESE VALUES ARE ALREADY MASS SCALED.
C     THEREFORE HERE WE NEED ONLY CALL THE INTEGRATION ROUTINES WITH THE
C     CORRECT VALUES
C
      WRITE (FU6,1050)
C
C     WRITE (FU6,1250) (NST+NST2),ISEN,INH,SLP,SLM                      1105PF97
      IF(IUNIT6.EQ.1) WRITE (FU6,1250) (NST+NST2),ISEN,INH,SLP/GUFAC6,
     *                                 SLM/GUFAC6                       0405JZ07
      IF(IUNIT6.EQ.0) WRITE (FU6,1260) (NST+NST2),ISEN,INH,SLP/GUFAC6,
     *                                 SLM/GUFAC6                       0405JZ07
      IF (DEL.LE.0.0D0) THEN
         WRITE (FU6,2900)
         STOP 'NEXTPT 1'
      ENDIF
      FISEN = DBLE(ISEN)
C
         IF (LGS(31).EQ.0) WRITE (FU6,1450)
         IF (LGS(31).EQ.2) WRITE (FU6,1550)
         IF (LGS(31).EQ.5) WRITE (FU6,1660)
C
C     IF PAGE-MCIVER METHOD IS USED -- THE NORMALIZATION CONSTANT (DXMAG)
C      NEEDS TO BE CALCULATED HERE AND PASSED TO THE SUBROUTINE USING THE
C      'IRCCM' COMMON BLOCK.  THE F MATRIX ALSO NEEDS TO BE PLACED IN FSV.
C
      IF (LGS(31).EQ.5) THEN
C        Find maximum derivative component
         DXMAX = 0.0D0
         DO 120 I = 1,N3
            T = ABS(DX(I))
            IF (T .GT. DXMAX) THEN
               DXMAX = T
            END IF
  120    CONTINUE
C
C
C        Determine the normalization factor
         DXNORM = 0.0D0
         DO 130 I = 1,N3
            DX(I) = DX(I)/DXMAX
            DXNORM = DXNORM + DX(I)*DX(I)
  130    CONTINUE
         DXNORM = SQRT(DXNORM)
C
C        Normalize the gradient vector
         DO 140 I = 1,N3
            DX(I) = DX(I)/DXNORM
  140    CONTINUE
         DXMAG = DXNORM*DXMAX
C
C        Store F:
         DO 150 I=1,N3TM
            DO 150 J=1,N3TM
150            FSV(I,J) = F(I,J)
C
      END IF
C
C     TAKE STEP ALONE GRADIENT AND PRINT OUT RESULTS
C
      STEPX = DEL
      KLNXTP = 1
      CALL INTEGR(STEPX,1,NFUNC,KLNXTP,FISEN,LGS(31))
C
C     REMOVE MASS SCALING                                               1110DL89
C
      CALL RPHTRX(N3,AMASS,X,2)
C
      WRITE(FU6,3000)S/GUFAC6                                           0405JZ07
      WRITE(FU6,3010) (X(I)/GUFAC6,I=1,N3)                              0405JZ07
3000  FORMAT(///'At s = ',F9.6,'   the new geometry is:  '/)
3010  FORMAT(3(1PE14.7,1X))
C
 1050 FORMAT(///1X,32(1H*),' Reaction path ',31(1H*)/)                  1014WH92
 1100 FORMAT(2I5,4F10.6,I10,2G10.3)
 1200 FORMAT(2I5)                                                       5/6/90VM
 1250 FORMAT(/1X,'All s values and step sizes are in mass-scaled',      1105PF97
     *' bohr.',/1X,'For path of steepest descent, max no. steps in'
     *,' each direction =',I7,/1X,'Direction of the initial step ',
     *'is in',I3,' times normal mode of the imaginary',
     */1x,'frequency',
     *//1X,' hessian grid multiple (INH) =',I5,
     *27HPath also stopped if s .GT.,F10.6,12H  or s .LT. ,F10.6)
 1260 FORMAT(/1X,'All s values and step sizes are in mass-scaled',      0405JZ07
     *' angstrom.',/1X,'For path of steepest descent, max no. steps in'
     *,' each direction =',I7,/1X,'Direction of the initial step ',
     *'is in',I3,' times normal mode of the imaginary',
     */1x,'frequency',
     *//1X,' hessian grid multiple (INH) =',I5,
     *27HPath also stopped if s .GT.,F10.6,12H  or s .LT. ,F10.6)
 1450 FORMAT(1X,'Use fixed-step Euler integrator with no',
     * ' stabilization')
 1550 FORMAT(1X,'Use fixed-step Euler integrator with stabilization',
     * ' by the Dupuis-Gordon method (ES1)')
 1660 FORMAT(1X,'Use fixed-step local quadratic (Page-McIver)',
     * ' integrator')
 2900 FORMAT('  ***** DEL <= 0.0 IS NOT ALLOWED !!! *****')
 3700 FORMAT(/,1X,'PERMITTED INH VALUES ARE THOSE GREATER THAN OR ',    5/6/90VM
     *'EQUAL TO 1.  INH = ',I5)                                         5/6/90VM
 3800 FORMAT(/,1X,'PERMITTED INM VALUES ARE THOSE GREATER THAN OR ',    5/6/90VM
     *'EQUAL TO INH FOR THE PAGE-MCIVER ALGORITHM.  INH = ',I5,         5/6/90VM
     *' INM = ',I5)                                                     5/6/90VM
C
       RETURN
C
       END SUBROUTINE nextpt
C***********************************************************************
C  digistr                                                              0317Yc99
C***********************************************************************
       function digistr (num)
       implicit double precision (a-h,o-z)
       character*3 digistr
       character*1 digit(0:9)
       data digit /'0','1','2','3','4','5','6','7','8','9'/

       i = num / 100
       j = (num - i * 100) / 10
       k = num - i* 100 - j * 10

       digistr = digit(i)//digit(j)//digit(k)

       return
       end function digistr
C***********************************************************************
C  NOROUT
C***********************************************************************
C
      SUBROUTINE norout (IOP,DXXP)
      use common_inc
      use perconparam
      use rate_const
      use cm, only : label,lbath
      use kintcm, only : iprmd,iprdis,ifrfac
      use keyword_interface, only : gufac6,iunit6
C
C The section of code that optional prints out normal mode info has been
C from NORMOD to this subroutine.  BCG 09/10/85
C Modified output by Thanh Truong  6/24/88         
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C     FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C
C     CALLED BY:          
C                RPHRD2,NORMOD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*
      DIMENSION TEMCOF(N3TM,N3TM)                                       0210YC97
      DIMENSION ISPORD(N3TM),TWOUT(N3TM)                                0210YC97
      DIMENSION DXXP(N3TM),INDEX(N3TM),WOUT(N3TM)
      DIMENSION RXYZ(MAXD)                                              0507YC97
      CHARACTER*1 XYZ(3,N3TM),IMA(N3TM),ITMA(N3TM)                      0210YC97
      LOGICAL PMODE
C
      character*1 digit(0:9)                                            0317Yc99 
      character*8 fdname                                                0317YC99
      character*4 anam                                                  0317Yc99
      character*3 digistr                                               0317YC99
      data digit / '0','1','2','3','4','5','6','7','8','9' /            0317Yc99
C
      PMODE = .FALSE.
      KOP = ABS(IOP)
      IF (IOP.LT.0) THEN                                               
         NEND = NDIM(KOP)  
         IF (ICODE(KOP) .LT. 0) THEN                                    11/20T87
            ISHFT = 0                                                      ..
         ELSE IF (ICODE(KOP).EQ.3 .OR. ICODE(KOP).EQ.2) THEN             6/13T89
            ISHFT = 5  
         ELSE
            ISHFT = 6   
         ENDIF
      ELSE   
         NEND = N3                                                      11/20T87
         ISHFT = N3 - NF(5)                                             1017WH92
      ENDIF
      NEND1 = NEND-ISHFT
C
C     PRINT INTERATOMIC DISTANCES                                       0507YC97
C                
      IF (MOD(LSAVE,IPRDIS).EQ.0.AND.LSAVE.NE.0) THEN                   0507YC97
        L=0                                                             0507YC97
        DO I=1,NATOM                                                    0507YC97
           DO J=1,I                                                     0507YC97
             L=L+1                                                      0507YC97
             II = (I-1)*3                                               0507YC97
             JJ = (J-1)*3                                               0507YC97
           RXYZ(L)=SQRT((X(II+1)/AMASS(II+1)-X(JJ+1)/AMASS(JJ+1))**2+   0507YC97
     1                (X(II+2)/AMASS(II+2)-X(JJ+2)/AMASS(JJ+2))**2+     0507YC97
     2                (X(II+3)/AMASS(II+3)-X(JJ+3)/AMASS(JJ+3))**2)     0507YC97
           RXYZ(L)=RXYZ(L)/GUFAC6                                       0405JZ07
           ENDDO                                                        0507YC97
         ENDDO                                                          0507YC97
C        WRITE (FU6,666) S                                              0507YC97
         IF(IUNIT6.EQ.1) WRITE(FU6,666) S/GUFAC6                        0405JZ07
         IF(IUNIT6.EQ.0) WRITE(FU6,668) S/GUFAC6                        0405JZ07
 666     FORMAT (/1X,'  INTERATOMIC DISTANCES IN BOHR @ S = ',          0507YC97
     1               F9.5,1X,'bohr')                                    0507YC97
 668     FORMAT (/1X,'  INTERATOMIC DISTANCES IN ANGSTROM @ S = ',      0405JZ07
     1               F9.5,1X,'angstrom')
         CALL PRNTFC (RXYZ,NATOM,L)                                     0507YC97
      ENDIF                                                             0507YC97
C
C     OPTIONAL OUTPUT
C
      IF (IOP.LT.0.OR.LGS(4).EQ.0) GOTO 10 
C
C     WRITE(FU6,2100) S                                                 0618YC96
      IF(IUNIT6.EQ.1) WRITE(FU6,2100) S/GUFAC6                          0405JZ07
      IF(IUNIT6.EQ.0) WRITE(FU6,2110) S/GUFAC6                          0405JZ07
      IF (S.EQ. 0.0d0) GOTO 10                                          0618YC96
      IF (LGS(5).GT.0) WRITE (FU6,1400) s,(ANHRM(I),I=NEND1,1,-1)       1029WH92
      IF (LGS(5).EQ.1.OR.LGS(5).EQ.2)         
     *      WRITE (FU6,1500) s,(Y00(I),I=NEND1,1,-1)                    1019BE05
      IF (LGS(5).EQ.7.OR.LGS(5).EQ.8.OR.LGS(5).EQ.21) THEN              1025YL91
         WRITE (FU6,1900) (ANHRM(I),I=NEND1,1,-1)                       1029WH92
         WRITE (FU6,2000) (AB(I),I=NEND1,1,-1)                          1029WH92
         WRITE (FU6,4400) (FMOMHR(I+ISHFT),I=NEND1,1,-1)                6/30YL91
      ENDIF
   10 IF (LGS(4).EQ.2) GO TO 50
      IF (SOB.LE.SOE) GO TO 20
      IF (S.GE.SOE.AND.S.LE.SOB) THEN
        PMODE = .TRUE.
        GO TO 50      
      ENDIF
      GO TO 30
   20 IF (S.GE.SOB.AND.S.LE.SOE)THEN
         PMODE = .TRUE.
         GO TO 50 
      ENDIF
   30 IF (IOP.EQ.2.AND.LGS(2).EQ.2) GO TO 50
      IF (IOP.LT.0.AND.LGS(2).EQ.2) GO TO 50
   40 RETURN
C
C     OUTPUT ALL NORMAL MODES
C
   50 CONTINUE 
      IF (IOP.LT. 0) GOTO 60
      IF (LGS(4) .EQ.0 .AND. PMODE) THEN
C         SPRNT = S
C         WRITE(FU6,2100) SPRNT       
C        WRITE(FU6,2100) S                                              0619YC96
         IF(IUNIT6.EQ.1) WRITE(FU6,2100) S/GUFAC6                       0405JZ07
         IF(IUNIT6.EQ.0) WRITE(FU6,2110) S/GUFAC6                       0405JZ07
      ENDIF
      if (s.ne.0) GOTO 70                                               0824YC98
   60 WRITE (FU6,2300)
      WRITE (FU6,2400) (NEND1+1-I,GSE(I),GSE(I)*CEV,GSE(I)*AUTOCM,      1029WH92
     *                GSE(I)*CKCAL,I=NEND1,1,-1)
      ZEROAU = 0.0D0
      ZEROCM = 0.0D0
      ZEROEV = 0.0D0
      ZEROKC = 0.0D0
      DO 80 I = 1, NEND1
         ZEROAU = GSE(I)+ZEROAU
         ZEROCM = GSE(I)*AUTOCM+ZEROCM
         ZEROEV = GSE(I)*CEV+ZEROEV
         ZEROKC = GSE(I)*CKCAL+ZEROKC
   80 CONTINUE
      WRITE (FU6,2500) ZEROAU,ZEROEV,ZEROCM,ZEROKC
C
C   PRINT WKB TURNING POINTS
C
   70 IF (.NOT.(KBPRNT.EQ.0.OR.IFWKB.NE.111999)) THEN
         WRITE (FU6,2600)
         DO 90 I = NEND1,1,-1
            IF (.NOT.(LGS(5).EQ.21.AND.MODE(I).NE.9)) THEN
               WRITE (FU6,2700) NEND1+1-I,TP1(I),TP2(I)                 1029WH92
            ENDIF
   90    CONTINUE
      ENDIF
C
C                                  
      DO 85 I = 1,NEND
        IF (FREQ(I) .GE. 0.0D0) THEN
          IMA(I) = ' '                          
          WOUT(I) = FREQ(I)
        ELSE
          IMA(I) = 'i'     
          WOUT(I) = -FREQ(I)
        ENDIF
  85  CONTINUE
      IF (IOP.EQ.2) THEN
         WRITE (FU6,3000)
C
C    write Frequencies at Saddle Point in Canonical Order               0210YC97
C
         ISHFT2 = ISHFT + 1                                             0210YC97
         DO I = NEND,ISHFT2,-1                                          0210YC97
            TWOUT(I) = WOUT(I)                                          0210YC97
            ITMA(I) = IMA(I)                                            0210YC97
            ISPORD(I) = I                                               0210YC97
         ENDDO                                                          0210YC97
            TWOUT(ISHFT) = WOUT(1)                                      0210YC97
            ITMA(ISHFT) = IMA(1)                                        0210YC97
            ISPORD(ISHFT)= 1                                            0210YC97
         DO II = ISHFT,2,-1                                             0210YC97
            TWOUT(II-1)  = WOUT(II)                                     0210YC97
            ITMA(II-1) = IMA(II)                                        0210YC97
            ISPORD(II-1) = II                                           0210YC97
         ENDDO                                                          0210YC97
         DO I = NEND, 1, -1                                             0210YC97
            WOUT(I) = TWOUT(I)                                          0210YC97
            IMA(I) = ITMA(I)                                            0210YC97
         ENDDO                                                          0210YC97
         WRITE(FU6,3100) (NEND+1-I,WOUT(I),IMA(I),WOUT(I)*CEV,          1029WH92
     *   IMA(I),WOUT(I)*AUTOCM,IMA(I),WOUT(I)*CKCAL,IMA(I),I=NEND,1,-1)
         WRITE (FU6,3200)
      ENDIF
C
C  Print out scaled frequencies
C
      IF (IOP.EQ.2) THEN                                                0808JC00
        IF (IFRFAC.NE.0) THEN                                           0808JC00
          WRITE (FU6, 3010)                                             0808JC00
          WRITE(FU6,3100) (NEND+1-I,WOUT(I)*FREQFAC,IMA(I),             0808JC00
     *    WOUT(I)*CEV*FREQFAC,                                          0808JC00
     *    IMA(I),WOUT(I)*AUTOCM*FREQFAC,IMA(I),WOUT(I)*CKCAL*FREQFAC,   0808JC00
     *    IMA(I),I=NEND,1,-1)                                           0808JC00
          WRITE (FU6,3200)                                              0808JC00
        ENDIF                                                           0808JC00
      ENDIF                                                             0808JC00
C 
C  Print out for state selected rate calculations
C                        
      IF(LGS(23).NE.0 .AND. S.EQ.0.0D0) THEN      
         WRITE(FU6,3500) 
         NDIA = 1            
         IF( IOP.LE. -1) THEN                          
            K = ABS(IOP) 
         ELSE
            K = 5
         ENDIF
 91      CONTINUE
         DO 94 I = -1,1  
            NCOUNT = 0                     
            DO 92 J = 1,NF(K) 
               IF(IOP .LE. -1) THEN
                  IMO = L9(K,J)
               ELSE
                  IMO = LN3(NDIA,J)
               ENDIF
               IF(IMO .EQ. I) THEN
                  NCOUNT = NCOUNT + 1
                  INDEX(NCOUNT) = J
               ENDIF 
 92         CONTINUE
            IF (NCOUNT .GT. 0) THEN
               IF(I .EQ. -1) THEN
                  WRITE(FU6,3600)(NEND+1-(INDEX(L)+ISHFT),L=NCOUNT,1,-1)1029WH92
               ELSEIF(I .EQ. 0) THEN
                  WRITE(FU6,3700)(NEND+1-(INDEX(L)+ISHFT),L=NCOUNT,1,-1)1029WH92
               ELSE
                  WRITE(FU6,3800)(NEND+1-(INDEX(L)+ISHFT),L=NCOUNT,1,-1)1029WH92
               ENDIF
            ENDIF        
 94      CONTINUE
         IF(LGS(23).EQ.2 .AND. NDIA.EQ.1 .AND. K.EQ.5) THEN
            WRITE(FU6,3900) SWITC
            NDIA = 2
            GOTO 91
         ENDIF
      ENDIF
C                           
C Write out frequencies and normal modes.
C   
      IF (LGS(4).EQ.2 .OR. PMODE) GO TO 100
      IF (IOP.GT.0.AND.IOP.NE.2) GO TO 40
  100 WRITE (FU6,3300)
C
C write eigenvectors in Canonical Order
C
      IF (IOP.EQ.2) THEN                                                0210YC97
         DO 950 I = 1, NEND                                             0210YC97
            NEW = ISPORD(I)                                             0210YC97
            DO 940 J = 1, NEND                                          0210YC97
               TEMCOF(J,I) = COF(J,NEW)                                 0210YC97
940         CONTINUE                                                    0210YC97
950      CONTINUE                                                       0210YC97
      ENDIF                                                             0210YC97
C
      IF (LBATH) THEN                                                   0317Yc99
         NSLUX = NEND - 1                                               0317Yc99
      ELSE                                                              0317Yc99
         NSLUX = NEND                                                   0317Yc99
      ENDIF                                                             0317YC99
       DO 110 I = 1,NEND
         INDEX(I) = I
         XYZ(1,I) = 'X'
         XYZ(2,I) = 'Y'
         XYZ(3,I) = 'Z'
 110   CONTINUE
       DO 140 M = NEND,1,-2                                             1017WH92
         LSTR = M
         LEND = M - 1                                                   0121GL92
         IF(LEND .LE. 1) LEND = 1                                       1017WH92
         WRITE(FU6,4000) (NEND+1-INDEX(II),II=LSTR,LEND,-1)             1029WH92
         WRITE(FU6,4100) (WOUT(II)*AUTOCM,IMA(II),II=LSTR,LEND,-1)      1017WH92
         WRITE(FU6,4200) ((XYZ(MM,LL),MM=1,3),LL=LSTR,LEND,-1)          1017WH92
C        WRITE(FU6,3400)
         NA = 1
         DO 120 I = 1,NSLUX,3                                           0317Yc99  
           KSTR = I 
           KEND = I + 2 
           IF (IOP.EQ.2) THEN                                           0210YC97
             WRITE(FU6,4300)IATOM(NA),                                  0210YC97
     *                ((TEMCOF(IND(K),L),K=KSTR,KEND),L=LSTR,LEND,-1)   0210YC97
           ELSE                                                         0210YC97
             WRITE(FU6,4300)IATOM(NA),
     *                 ((COF(IND(K),L),K=KSTR,KEND),L=LSTR,LEND,-1)     1116WH92
           ENDIF
           NA = NA + 1
 120     CONTINUE     
         if (lbath) then                                                0317Yc99
           IF (LSTR.NE.LEND) THEN                                       0317Yc99
              IF (IOP.EQ.2) THEN                                        0317Yc99
               write (fu6,4301) TEMCOF(N3,LSTR),TEMCOF(N3,LEND)         0317Yc99
             ELSE                                                       0317Yc99
               write (fu6,4301) COF(N3,LSTR),COF(N3,LEND)               0317Yc99
             ENDIF                                                      0317Yc99
           ELSE                                                         0317Yc99
              IF (IOP.EQ.2) THEN                                        0317Yc99
               write (fu6,4302) TEMCOF(N3,LSTR)                         0317Yc99
             ELSE                                                       0317Yc99
               write (fu6,4302) COF(N3,LSTR)                            0317Yc99
             ENDIF                                                      0317Yc99
           ENDIF                                                        0317Yc99
         endif                                                          0317Yc99
 140    CONTINUE
c
c        //  write out the eigenvector in xmol                          0317Yc99
c       
         IF (IOP.GT.0) THEN                                               ..
               ID = 5                                                     ..
         ELSE
               ID = ABS(IOP)                                              ..
         ENDIF                                                            ..
c
         IF (S.EQ.0.and.IPRMD(ID).EQ.1) THEN                              ..
           IF (IOP.EQ.-1) ANAM = "R1MD"                                   ..
           IF (IOP.EQ.-2) ANAM = "R2MD"                                   ..
           IF (IOP.EQ.-3) ANAM = "P1MD"                                   ..
           IF (IOP.EQ.-4) ANAM = "P2MD"
           IF (IOP.EQ.-7) ANAM = "RWMD"
           IF (IOP.EQ.-8) ANAM = "PWMD" 
           IF (IOP.GT.0) ANAM = "TSMD"
           L = 1
           NATM = NEND/3
           DO I = NEND1,1,-1
             fdname = anam(1:4)//'.'//digistr(L)
             OPEN(unit=94,File=FDNAME,Status="UNKNOWN")
             INDS = I+ISHFT
             WRITE (94,*) NATM
             WRITE (94,1902) L,FREQ(INDS)*AUTOCM
1902         FORMAT ('* MODE',I5,5X,F8.3,' cm-1')
             DO J = 1,NATM
               INDT = 3*(IATOM(J)-1)
               IF (IOP.LT.0) THEN
                 WRITE (94,1901) LABEL(IATOM(J)),
     *                    X(INDT+1)*.5292d0,
     *                    X(INDT+2)*.5292d0,
     *                    X(INDT+3)*.5292d0,
     *                    COF(INDT+1,INDS),
     *                    COF(INDT+2,INDS),
     *                    COF(INDT+3,INDS)
               ELSE
                 WRITE (94,1901) LABEL(IATOM(J)),
     *                    X(INDT+1)*.5292d0/AMASS(INDT+1),
     *                    X(INDT+2)*.5292d0/AMASS(INDT+1),
     *                    X(INDT+3)*.5292d0/AMASS(INDT+1),
     *                    COF(INDT+1,INDS),
     *                    COF(INDT+2,INDS),
     *                    COF(INDT+3,INDS)
               ENDIF
             ENDDO
c
             FSCALE = 0.7d0
             IFRAME = 19
             OFFSET = 0
c
             DO M = 1,IFRAME
               WRITE (94,*) NATM
               WRITE (94,1903) L,FREQ(INDS)*AUTOCM,M
1903           FORMAT ('* MODE',I5,5X,F8.3,' cm-1 Frame ',I5)
               DO J = 1,NATM
               INDT = 3*(IATOM(J)-1)
                 IF (IOP.LT.0) THEN
                   WRITE (94,1901) LABEL(IATOM(J)),
     *                    X(INDT+1)*.5292d0+
     *                             (FSCALE*SIN(TPI*M/IFRAME+OFFSET)*
     *                             COF(INDT+1,INDS)/.5292d0),
     *                    X(INDT+2)*.5292d0+
     *                             (FSCALE*SIN(TPI*M/IFRAME+OFFSET)*
     *                             COF(INDT+2,INDS)/.5292d0),
     *                    X(INDT+3)*.5292d0+
     *                             (FSCALE*SIN(TPI*M/IFRAME+OFFSET)*
     *                             COF(INDT+3,INDS)/.5292d0),
     *                    COF(INDT+1,INDS),
     *                    COF(INDT+2,INDS),
     *                    COF(INDT+3,INDS)
                 ELSE
                   WRITE (94,1901) LABEL(IATOM(J)),
     *                    X(INDT+1)*.5292d0/AMASS(INDT+1)+
     *                             (FSCALE*SIN(TPI*M/IFRAME+OFFSET)*
     *                             COF(INDT+1,INDS)/.5292d0),
     *                    X(INDT+2)*.5292d0/AMASS(INDT+2)+
     *                             (FSCALE*SIN(TPI*M/IFRAME+OFFSET)*
     *                             COF(INDT+2,INDS)/.5292d0),
     *                    X(INDT+3)*.5292d0/AMASS(INDT+3)+
     *                             (FSCALE*SIN(TPI*M/IFRAME+OFFSET)*
     *                             COF(INDT+3,INDS)/.5292d0),
     *                    COF(INDT+1,INDS),
     *                    COF(INDT+2,INDS),
     *                    COF(INDT+3,INDS)
                 ENDIF
               ENDDO
             ENDDO
             CLOSE(94) 
             L = L + 1
           ENDDO
1901     FORMAT (I3,6F12.6)
         ENDIF                                                          0317Yc99
C
      GO TO 40
C
 1100 FORMAT(2H V, 8X, 1PE13.6 /
     *   10H VA        , 1P,5E13.6)
 1200 FORMAT(6H    X ,1P,3E13.6)
 1300 FORMAT(6H   DX ,1P,3E13.6)
 1400 FORMAT(7H  ANHRM,3x,F8.4,3X,5E12.5, : / (3X,1P,5E12.5))           0317Yc99
 1500 FORMAT(7H    Y00,3x,F8.4,3X,5E12.5, : / (3X,1P,5E12.5))           0317Yc99
 1900 FORMAT(2X,'QUADRATIC-QUARTIC force constants '/ 7H      F,        0317Yc99
     *   3X,5E12.5,: / (3X,5E12.5))                                     0317Yc99
 2000 FORMAT (7H      A,3X,1P,5E12.5, : / (3X,5E12.5))                  0317YC99
 2100 FORMAT(/,1X,'At s = ',F9.5,1X,'bohr')
 2110 FORMAT(/,1X,'At s = ',F9.5,1X,'angstrom')                         0405JZ07
 2300 FORMAT(//1X,75(1H-)/18X,'Vibrational ground-state energies ',
     * /1X,75(1H-),/2X,'mode',12X,'hartrees',8X,'eV',11X,'cm**-1',
     *  9X,'kcal'/)
 2400 FORMAT(2X,I3,10X,1P,E12.4,4X,0P,F8.4,4X,F10.2,4X,F10.4)
 2500 FORMAT(/2X,'Total ZPE',4X,1P,E12.4,4X,0P,F8.4,4X,F10.2,
     *        4X,F10.4,/1X,75(1H-))
 2600 FORMAT(/' Turning points for modes:')
 2700 FORMAT(' Mode ',I2,10X,'TP1= ',E9.3,5X,'TP2= ',E9.3)
 2800 FORMAT(/1X,'Harmonic Frequencies (cm**-1)')                       0824YC98
 2900 FORMAT(5X,5(F10.3,1X,A1))
 3000 FORMAT(//1X,78(1H-)/,24X,'Harmonic Frequencies',/1X,78(1H-),      0824YC98
     */16X,'a.u.',12X,'eV',10X,'cm**-1',9X,'kcal',/)
 3010 FORMAT(//1X,78(1H-)/,24X,' Scaled Frequencies ',/1X,78(1H-),      0808JC00
     */16X,'a.u.',12X,'eV',10X,'cm**-1',9X,'kcal',/)
 3100 FORMAT(2X,'Mode ',I3,4X,F10.7,1X,A1,4X,F6.4,1X,A1,4X,F9.2,        0610WH94
     *1X,A1,4X,F8.4,1X,A1)
 3200 FORMAT(1X,78(1H-))
 3300 FORMAT(/1X,'Frequencies and normalized eigenvector components ',
     *       /1X,'in mass-scaled space-fixed cartesian coordinates')
 3400 FORMAT(1H )
 3500 FORMAT(/,'       For state-selected rate calculation',/)
 3600 FORMAT(1X,'  Restricted ground state modes:',20I3/(32X,20I3))
 3700 FORMAT(1X,'  Thermalized modes: ',20I3/(22X,30I3))
 3800 FORMAT(1X,'  Restricted to first excited state modes:',20I3,
     * /(42X,20I3))
 3900 FORMAT(/,'  Beyond the diabatic intersection at s = ',F10.5,/)
 4000  FORMAT(/,1X,4HMode,16X,I3,27X,I3)
 4100  FORMAT(1X,'omega (cm**-1)',2X,F9.2,1X,A1,(19X,F9.2,1X,A1))
 4200  FORMAT(/,1X,4HAtom,2(3(8X,A1),3X))
 4300  FORMAT(1X,I3,5X,2(3(F7.3,2X),3X))
 4301  FORMAT(1X,'eff solv ',8X,F7.3,23X,F7.3)                          0317YC99
 4302  FORMAT(1X,'eff solv ',8X,F7.3)                                   0317Yc99
 4400  FORMAT(1X,'Reduced moment of inertia of internal rotation',
     * /4X,5E14.6,:/(4X,5E14.6))                                        0611WH94
C
      END subroutine norout
C**********************************************************************
C  NSTATE
C**********************************************************************

       SUBROUTINE nstate (ESV,TRNPT1,TRNPT2,NEMAX,NPX)
       use common_inc
       use perconparam
       use rate_const
C
C    On input:
C       ESV    : the array that stores the energies for tunneling
C       TRNPT1 : the classical turning point on the reactant side (i.e. s < 0)
C       TRNPT2 : the classical turning point on the product side (i.e. s > 0)
C                TRNPT1 and TRNPT2 were determined in PSAG previously.
C       NEMAX  : the number of energy grid
C 
C    On output:
C       NPACC  : the number of accessible of states at each energy grid point
C       MPACC(J)  : the maximum of NPACC(IE) for IE less than or equal to J
C
C Called by:
C      LCG3
C
C Calls: LCTP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ESV(NSV),SMLQ(NVIBM),RXX(N3TM)
      DIMENSION TRNPT1(NSV),TRNPT2(NSV),VADSCR(1,1)
      DIMENSION COFCU(N3TM,NVIBM),GEOMCU(N3TM),WETSCU(N3TM)
      EQUIVALENCE (VADBCU,VADSCR(1,1))
C
C Initialize arrays, etc.
C
      ISHFT = 7
      IOP = 3
      IF(ICODE(5).EQ.3) ISHFT = 6
      IF(LGS(34).NE.0) ISHFT = 1                                        
      N3M7 = N3 - ISHFT
      LSAVE = LSAVE
      SL = SSUBI(1)
      SR = SSUBI(LSAVE)
      NCUTF = 0
      IF (IEXOG.EQ.0.AND.(VADIB(LSAVE).LT.VAP)) NCUTF = 1
      IF (IEXOG.EQ.-1.AND.(VADIB(1).LT.VAR)) NCUTF = 2
      IF (NCUTF.NE.0) THEN
         WRITE (6,9110)
         IF (IEXOG.EQ.0) THEN
             ENERGY = VAP
             IXX = LSAVE
         ELSE
             ENERGY = VAR
             IXX = 1
         ENDIF
         VADBCU = ENERGY
         CALL LCTP (1,LSAVE,IXX,LSAVE,ENERGY,STXX,VADIB,ASPL,BSPL,
     *                CSPL,DSPL)
         CALL LOCATE (SSUBI,LSAVE,STXX,IDUMY)
         CALL INTRPL(GEOM,SSUBI,GEOMCU,STXX,IDUMY,NSDM,LSAVE,1,
     *                N3TM,1,N3)
            CALL INTRPL(COFSV,SSUBI,COFCU,STXX,IDUMY,NSDM,LSAVE,N3TM,
     *                  NVIBM,1,N3M7)
            CALL INTRPL(WETS,SSUBI,WETSCU,STXX,IDUMY,NSDM,LSAVE,1,NVIBM,
     *                  1,N3M7)
         IF (IEXOG.EQ.0) THEN 
            SR = STXX
            ICUTF = IDUMY + 1
         ELSE 
            SL = STXX
            ICUTF = IDUMY
         ENDIF
      ELSE
         IF (IEXOG.EQ.0) THEN
            IXX = LSAVE
         ELSE
            IXX = 1
         ENDIF
         VADBCU = VADIB(IXX)
         DO 20 ICOORD = 1,N3TM
               GEOMCU (ICOORD) = GEOM(ICOORD,IXX)
               DO 30 IMODE = 1, NVIBM
                     COFCU(ICOORD,IMODE) = COFSV(ICOORD,IMODE,IXX)
30             CONTINUE
20       CONTINUE
         DO 40 IMODE = 1, NVIBM
               WETSCU(IMODE) = WETS(IMODE,IXX)
40       CONTINUE
      ENDIF
      SLCUTF = SL 
      SRCUTF = SR
C 
C Determin the number of accessible states, using equation (10) in the 
C POLYRATE 4 paper.
C
      NELAST = NEMAX + 1
      NDUMOL = 0
      DO 80 IE = 1,NELAST
         ENER = ESV(IE)
         STP0 = TRNPT1(IE)
         STP1 = TRNPT2(IE)
         IF (IEXOG.EQ.0) THEN
            STPX = STP0
         ELSE
            STPX = STP1
         ENDIF
         IF (STP0.GT.SL.AND.STP1.LT.SR) THEN
            CALL LOCATE (SSUBI,LSAVE,STPX,JJX)
            CALL INTRPL (GEOM,SSUBI,RXX,STPX,JJX,NSDM,LSAVE,1,N3TM,1,N3)
            CALL LCPROJ(COFCU,RXX,GEOMCU,SMLQ,N3TM,N3,NVIBM,N3M7)
C Calculate the frequency of the p mode
            TERM = 0.0D0
            DO 50 I = 1, N3M7
               IF (WETSCU(I).GT.0) TERM = TERM + (SMLQ(I)*WETSCU(I))**2
50          CONTINUE
            TERM = SQRT(TERM)
            JDUM = 0
60          JDUM = JDUM + 1
            VXDUM = VADBCU + DBLE(JDUM)*TERM
            IF (VXDUM.GT.ENER.OR.TERM.EQ.0.0) GO TO 70
            GO TO 60
70          NPX = JDUM - 1
            NPACC(IE) = NPX
            IF(NPX .GT. NDUMOL) NDUMOL = NPX
            MPACC(IE) = NDUMOL
         ENDIF
80    CONTINUE
      NPX = NDUMOL
      RETURN
 9110 FORMAT(/,1X,'In the calculation of LCG3 tunneling probabilities ',
     *'into excited states,',/,1X,'the contributions from the final ',   
     *'excited states will not be included,',/,1X,'if the adiabatic ',
     *' potential at this point is smaller than the adiabatic energy ',
     */,1X,'of both the reactant and the product.')
      END SUBROUTINE nstate
C
C***********************************************************************
C   OPENFI
C***********************************************************************
C
      SUBROUTINE openfi(NUNIT, FISTAT, FINAME, LEXIT)
      use perconparam
C
C   Subroutine to open a file given the unit number, the status, 
C   and the filename. 
C   If an error occurrs upon opening a file the logical variable LEXIT
C   is set TRUE.
C
C   Called by: MAIN,FIOPEN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*20 FINAME
      CHARACTER*7  FISTAT
      LOGICAL LEXIT
C
      IF (INDEX(FISTAT,'NEW') .NE. 0) THEN
        OPEN(UNIT=NUNIT,FILE=FINAME,STATUS='NEW',FORM='FORMATTED',
     *        ERR=100)
      ELSEIF (INDEX(FISTAT,'OLD') .NE. 0) THEN
        OPEN(UNIT=NUNIT,FILE=FINAME,STATUS='OLD',FORM='FORMATTED',
     *        ERR=100)
      ELSE
        OPEN(UNIT=NUNIT,FILE=FINAME,STATUS='UNKNOWN',FORM='FORMATTED',
     *        ERR=100)
      ENDIF
C
      RETURN
C
100   LEXIT = .TRUE.
      WRITE (FU6, 1000) FINAME, NUNIT
C
      RETURN
C     
1000  FORMAT (/,2X,'Error opening the file ', A10,
     1        ' which is linked to FORTRAN unit ',I3)
C
      END subroutine openfi
C***********************************************************************
C  OVRLP
C***********************************************************************
C
      SUBROUTINE ovrlp (LWRITE,LLN3,N3M7,C,COLDLC,FREQ,SX)              9/18YL92
      use perconparam, only : n3tm,fu6
      use keyword_interface, only : gufac6,iunit6
c
C     COMPUTES OVERLAPS BETWEEN OLD AND NEW MODE EIGENVECTORS AND WARNS
C     IF A CROSSING HAS OCCURRED.  IN THIS CASE, PREVIOUSLY SAVED
C     TURNING POINTS, ZERO POINT ENERGIES, AND B COEFFICIENTS ARE
C     REORDERRED.  IND1 AND IND2 ARE THE INDICES OF THE MODES WHICH
C     HAVE CROSSED.
C
C
C     CALLED BY:
C                FDIAG
C     Restructured on Sept. 18, 1992 by Y.-P. Liu
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION C(N3TM,N3TM),INDEX(N3TM),OVER(N3TM)
      DIMENSION COLDLC(N3TM,N3TM),FREQ(N3TM)                            9/18YL92
      LOGICAL LWRITE                                                    5/10DL90
C
      ISHFT = LLN3-N3M7
       DO 11 I = 1, N3TM                                                1215YL91
          INDEX(I) = I                                                  1215YL91
          OVER(I) = 0.D0                                                1215YL91
11     CONTINUE                                                         1215YL91
       J0 = ISHFT + 1                                                   5/10DL90
C     LOOP OVER ALL N3M7 EIGENVECTORS FOR THE BEST OVERLAP BETWEEN THE  5/10DL90
C     OLD AND NEW ONES, I IS THE COLUMN INDEX OF THE OLD EIGENVECTORS   5/10DL90
       DO 210 I = LLN3, J0, -1                                          5/04YL92
        SUMMX = 0.0D0                                                   5/10DL90
        DO 220 J = LLN3, J0, -1                                         5/04YL92
         IDUMY = 0                                                      4/10YL92
         IF (I.LT.LLN3) THEN                                            5/04YL92
            DO 225 II = LLN3, I+1, -1                                   5/04YL92
               IF (J.EQ.INDEX(II)) IDUMY = 1                            4/10YL92
  225       CONTINUE                                                    4/10YL92
         ENDIF                                                          4/10YL92
C  if J has not been used yet, calculate the overlp between mode J and mode I
         IF (IDUMY.EQ.0) THEN                                           4/10YL92
         SUM = 0.0D0                                                    5/10DL90
         DO 230 K = 1, LLN3                                             5/10DL90
          SUM = SUM + C(K,J)*COLDLC(K,I)                                5/10DL90
  230    CONTINUE                                                       5/10DL90
         IF(ABS(SUM).GT.ABS(SUMMX)) THEN                                5/10DL90
          ISW = J                                                       5/10DL90
          SUMMX = SUM                                                   5/10DL90
         END IF                                                         5/10DL90
         ENDIF                                                          4/10YL92
  220   CONTINUE                                                        5/10DL90
  240    CONTINUE                                                       5/10DL90
        INDEX(I) = ISW                                                  5/10DL90
        OVER(I) = SUMMX                                                 5/10DL90
  210  CONTINUE                                                         5/10DL90
       IF (LWRITE) THEN                                                 5/10DL90
C       WRITE (FU6,1450) SX, (I,I=J0,LLN3)                              5/10DL90
        IF(IUNIT6.EQ.1) WRITE (FU6,1450) SX/GUFAC6, (I,I=J0,LLN3)       0405JZ07
        IF(IUNIT6.EQ.0) WRITE (FU6,1460) SX/GUFAC6, (I,I=J0,LLN3)       0405JZ07
        WRITE (FU6,1550) (INDEX(I),I=J0,LLN3)                           5/10DL90
        WRITE (FU6,1600) (OVER(I),I=J0,LLN3)                            5/10DL90
       END IF                                                           5/10DL90
C
C      INTERCHANGE FREQUENCIES AND COLUMNS OF EIGENVECTORS              5/10DL90
C
       DO 250 I = J0, LLN3                                              5/04YL92
        OVER(I) = FREQ(INDEX(I))                                        5/10DL90
  250  CONTINUE                                                         5/10DL90
       DO 260 I = J0, LLN3                                              5/04YL92
        FREQ(I) = OVER(I)                                               5/10DL90
  260  CONTINUE                                                         5/10DL90
       DO 270 K = 1, LLN3                                               5/10DL90
        DO 280 I = J0, LLN3                                             5/04YL92
         OVER(I) = C(K,INDEX(I))                                        5/10DL90
  280   CONTINUE                                                        5/10DL90
        DO 290 I = J0, LLN3                                             5/04YL92
         C(K,I) = OVER(I)                                               5/10DL90
  290   CONTINUE                                                        5/10DL90
  270  CONTINUE                                                         5/10DL90
C
 200  CONTINUE
C*
      RETURN
C
 1350 FORMAT(' At s =',F10.5,' bohr, maximum overlap found between ',
     *       'two new vectors and old vectors', I5, 
     *       2(/, ' New vector', I5, ' has overlap =', 1PE15.7), /,
     *       ' TRY DECREASING SAVE SIZE IN THIS REGION.')
 1450 FORMAT(/,' s = ', F10.5,' bohr',/, ' New modes', T15, 
     *       I5, (T20, 10I10))
 1460 FORMAT(/,' s = ', F10.5,' angstrom',/, ' New modes', T15,         0405JZ07
     *       I5, (T20, 10I10))
 1550 FORMAT(' switched to', T15, I5, (T20, 10I10))
 1600 FORMAT(' overlap =', T15, 1PE10.2, (T25, 1P,10E10.2))
C
      END SUBROUTINE ovrlp

C***********************************************************************
C  PBTSRT
C***********************************************************************
      SUBROUTINE pbtsrt (NA,NB,PB1,PB2,PB3)
C
C   Compares each element of PB1 and PB2, and stores the bigger one in PB3
C   
C
C     Called by KAPVA
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION PB1(NA,NB),PB2(NA,NB),PB3(NA,NB)
C
C
      DO 10 I = 1, NB
         DO 10 J = 1, NA
            PB3(J,I) = PB1(J,I)
            IF (PB2(J,I).GT.PB1(J,I)) PB3(J,I) = PB2(J,I)
  10  CONTINUE
      RETURN
      END SUBROUTINE pbtsrt
***********************************************************************
C  PHID
C***********************************************************************
C
      SUBROUTINE phid (EPS,PH,DPH)
      use perconparam, only : fu6
C
C     PHID   - compute phi function needed for uniform semiclassical
C              quantization of double well potential
C
C     CALLED BY:
C                WKBPOT
C     CALLS:
C           BRNULI
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION C(10)
      LOGICAL LFIRST,LCONV
      DATA LFIRST / .TRUE. /,NTERM / 10 /,SWTCH / 2.0D0 /
      DATA PSIHLF / -1.963510026021423D0 /
      save                                                              0601YC98
      AEPS = ABS(EPS)
      PH = 0.0D0
      DPH = 1.0D+35
      IF (EPS.EQ.0.0D0) RETURN
      IF (AEPS.GE.SWTCH) THEN
         IF (LFIRST) THEN
            CALL BRNULI (NTERM,C)
            SGN = -1.0D0
            DO 10 I = 1, NTERM
               FJ = DBLE(2*I)
               SGN = -SGN
               C(I) = SGN * C(I) * (1.0D0-2.0D0/4**I) /                 1012WH92                 
     *                (FJ * (FJ-1.0D0))
C              C(I) = SGN*C(I)*(1.0D0-2.0D0**(1-2*I))/
C    *                (FJ*(FJ-1.0D0))
   10       CONTINUE
            LFIRST = .FALSE.
         ENDIF
         T = (1.0D0/AEPS)/AEPS
         SUM1 = C(1)
         SUM2 = -C(1)
         T2N = 1.0D0
         I = 1
C
C        DO WHILE (I .LT. NTERM .OR. .NOT.LCONV)
C
   20    CONTINUE
         I = I+1
         T2N = T2N*T
         TERM1 = C(I)*T2N
         SUM1 = SUM1+TERM1
         TERM2 = -TERM1*(2.0D0*I-1.0D0)
         SUM2 = SUM2+TERM2
         LCONV = ABS(TERM1/SUM1).LT.1.0D-4.AND.ABS(TERM2/SUM2).LT.1.0D-3
C
C        END DO
C
         IF (I.GE.NTERM.OR.LCONV) GO TO 30
         GO TO 20
   30    CONTINUE
         IF (.NOT.LCONV) WRITE (FU6,1000) SWTCH,NTERM,SUM1,TERM1,SUM2,
     *      TERM2
         PH = SUM1/EPS
         DPH = SUM2*T
      ELSE
         SUM1 = 0.0D0
         SUM2 = 0.0D0
         TE = 2.0D0*AEPS
         I = 0
C
C        DO WHILE (I .LT. 800 .OR. .NOT.LCONV)
C
   40    CONTINUE
         I = I+1
         TIM = 2.0D0*I-1.0D0
         T = TE/TIM
         TERM1 = T-ATAN(T)
         SUM1 = SUM1+TERM1
         T = T*T
         TERM2 = 2.0D0*T/((1.0D0+T)*TIM)
         SUM2 = SUM2+TERM2
         LCONV = ABS(TERM1/SUM1).LT.1.0D-6.AND.ABS(TERM2/SUM2).LT.1.0D-6
C
C        END DO
C
         IF (I.GE.800.OR.LCONV) GO TO 50
         GO TO 40
   50    CONTINUE
         IF (.NOT.LCONV) WRITE (FU6,1100) SWTCH,I,SUM1,TERM1,SUM2,TERM2
         SUM1 = SUM1*SIGN(1.0D0,EPS)
         T = -LOG(AEPS)+PSIHLF
         PH = EPS*(1.0D0+T)+SUM1
         DPH = T+SUM2
      ENDIF
      RETURN
C
 1000 FORMAT(' PHI NOT CONVERGED, EPS.GT.', F3.1, ' NTERM=',
     *   I3, 1P,4E13.4)
 1100 FORMAT(' PHI NOT CONVERGED, EPS.LT.', F3.1, ' NTERM=',
     *   I3, 1P,4E13.4)
C
      END SUBROUTINE phid
C***********************************************************************
C  PHSINT
C***********************************************************************
C
      SUBROUTINE phsint (E,X1X,X2,VI,LIFREQ,JFREQ,
     *                   TX,THETA,DNDE,IDER,IERR)
      use common_inc
      use perconparam
      use rate_const, only : kbquad
C
C   Rewritten February 1987 - GCH
C
C     THIS ROUTINE PERFORMS THE CALCULATION OF THE PHASE INTEGRAL,THETA,
C     AT VIB. ENERGY E ON THE POTENTIAL OF MODE JFREQ, AND IF IDER.NE.0,
C     CALCULATES THE DERIVATIVE, DNDE, OF THETA WITH THE ENERGY. TX HOLD
C     THE MASS-SCALED GEOMETRY OF THE MOLECULE AT THE BOTTOM OF THE WELL;
C     VI IS ITS ENERGY. CHEBYSHEV QUADRATURE IS USED.
C     THE QUADRATURE ROUTINES ALSO USED TO TEST IF THE CORRECT TURNING
C     POINTS HAVE BEEN LOCATED. IF NOT, TP IS CALLED TO RECALCULATE THE
C     TURNING POINT VALUES, AND THE QUADRATURE IS REPEATED.
C     ON RETURN, IERR=0 IF NO TURNING POINTS WERE RECALCULATED,IERR=1 IF
C     X1X WAS RECALCULATED, IERR=2 IF X2 WAS RECALCULATED, AND IERR=3 IF
C     BOTH X1X AND X2 WERE RECALCULATED.
C
C     CALLED BY:
C                 WKBVIB
C
C     CALLS:
C                 TP,TRANS,ENERG
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 7/01/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION TX(N3TM)
C
C
      XMU = REDM
      K = KBQUAD
      IERR = 0
      EZEROX = VI
      E2 = E
   10 RP = X2
      RM = X1X
C
C  PHASE INTEGRAL - CHEBYSHEV QUADRATURE FROM 0 - X1X (NEGATIVE SEGMENT)
C                    AND FROM 0 - X2 (POSITIVE SEGMENT)
C   FIRST LOCATE THE QUADRATURE POINT NEAREST (BUT LESS THAN) 0.
C
      Z = ACOS((RP+RM)/(RP-RM))*DBLE(K+1)/PI
      IZERO = Z
      IF (IZERO.EQ.0) IZERO = 1
      IF (IZERO.GT.K) IZERO = K
C
C   SET UP FOR INTEGRATION FROM ZERO TO THE NEGATIVE TURNING POINT
C
      SUM = 0.0D0
      ITT = 1
      ISTEP = -1
      IMIN = IZERO
      IMAXX = 1
C
C   BEGIN LOOP OVER THE TWO SEGMENTS
C
   20 SUM = 0.0D0
      YOLD = 0.0D0
C
C  LOOP OVER QUADRATURE POINTS
C
      DO 40 I = IMIN, IMAXX, ISTEP
C
C  CALCULATE QUADRATURE POINT AND WEIGHT
C
         W = (PI/(DBLE(K)+1.0D0))*(SIN(DBLE(I)*PI/(DBLE(K)+1.0D0)))**2
         YI = COS(DBLE(I)*PI/(DBLE(K)+1.0D0))
         Y = 0.5D0*(YI*(RM-RP)+RP+RM)
C
C  CALCULATE THE POTENTIAL AT THIS POINT
C
         DO 30 JI = 1, N3
            X(JI) = TX(JI)+Y*COF(JI,JFREQ)
   30    CONTINUE
c         call energ(1)                                                   6/2RS94
         call ehook(1,iproc)                                                   0301YC97
         VX = V-EZEROX
C
C  TEST FOR AN ENERGY > E
C
         IF (VX.GT.E2) THEN
            IF (IERR.EQ.0) THEN
               IERR = ITT
            ELSEIF (IERR.NE.ITT) THEN
               IERR = 3
            ENDIF
            T = 0.5D0*(Y+YOLD)
            CALL TP (E2,T,YOLD,Y,EZEROX,LIFREQ,JFREQ,
     *               TX,0.0D0,1,DUM,ICON)
            IF (ITT.EQ.1) THEN
               X1X = T
            ELSE
               X2 = T
            ENDIF
            GO TO 10
         ENDIF
C
C  ADD CONTRIBUTION TO INTEGRAL
C
         TJAC = 0.5D0*ABS((RP-RM))
         FI = TJAC*SQRT((E2-VX))/SQRT(1.0D0-YI**2)
         SUM = SUM+W*FI
C
C END LOOP OVER QUADRATURE POINTS
C
   40 CONTINUE
C
C   NOW EVALUATE THE INTEGRAL FROM ZERO TO THE POSITIVE TURNING POINT
C
      IF (ITT.EQ.1) THEN
         SUMNEG = SUM
         ITT = 2
         ISTEP = 1
         IMIN = IZERO+1
         IMAXX = K
         GO TO 20
      ELSE
         SUMPOS = SUM
      ENDIF
C
C   EVALUATION OF PHASE INTEGRAL COMPLETED
C
      TOTAL = SUMNEG+SUMPOS
      THETA = (SQRT(2.0D0*XMU)/PI)*TOTAL
      IF (IDER.EQ.0) RETURN
C
C   EVALUATE THE DERIVATIVE, DNDE, AGAIN LOOPING OVER NEGATIVE AND
C   POSITIVE SEGMENTS.
C   FIND THE QUADRATURE POINT NEAREST (BUT LESS THAN) 0.
C
      Z = (ACOS((RP+RM)/(RP-RM))*DBLE(2*K)/PI)+1.0D0
      Z = Z*0.5D0
      IZERO = Z
      IF (IZERO.EQ.0) IZERO = 1
      IF (IZERO.GT.K) IZERO = K
C
C   FIRST PERFORM THE QUADRATURE FROM ZERO TO THE NEGATIVE TURNING POINT
C
      ITT = 1
      IMIN = IZERO
      IMAXX = 1
      ISTEP = -1
C
C LOOP OVER SEGMENTS
C
   50 SUM = 0.0D0
      YOLD = 0.0D0
C
C  LOOP OVER QUADRATURE POINTS
C
      DO 70 I = IMIN, IMAXX, ISTEP
C
C CALCULATE QUADRATURE POINTS AND WEIGHTS
C
         W = PI/DBLE(K)
         YI = COS((2.0D0*DBLE(I)-1.0D0)*PI/(2.0D0*DBLE(K)))
         Y = 0.5D0*(YI*(RM-RP)+RP+RM)
C
C  EVALUATE POTENTIAL AT POINT
C
         DO 60 JI = 1, N3
            X(JI) = TX(JI)+Y*COF(JI,JFREQ)
   60    CONTINUE
c         call energ(1)                                                  6/2RS94
         call ehook(1,iproc)                                                  0301YC97
         VX = V-EZEROX
C
C  TEST FOR POTENTIAL > E
C
         IF (VX.GT.E2) THEN
            IF (IERR.EQ.0) THEN
               IERR = ITT
            ELSEIF (IERR.NE.ITT) THEN
               IERR = 3
            ENDIF
            T = 0.5D0*(Y+YOLD)
            CALL TP (E2,T,YOLD,Y,EZEROX,LIFREQ,JFREQ,
     *               TX,0.0D0,1,DUM,ICON)
            IF (ITT.EQ.1) THEN
               X1X = T
            ELSE
               X2 = T
            ENDIF
            GO TO 10
         ENDIF
C
C  ADD CONTRIBUTION TO INTEGRAL
C
         TJAC = 0.5D0*ABS((RP-RM))
         FI = TJAC*SQRT(1.0D0-YI**2)/SQRT((E2-VX))
         SUM = SUM+W*FI
C
C  END LOOP OVER QUADRATURE POINTS
C
   70 CONTINUE
      IF (ITT.EQ.1) THEN
         DUMNEG = SUM
      ELSE
         DUMPOS = SUM
      ENDIF
C
C  SET UP FOR INTEGRATION OVER POSITIVE SEGMENT
C
      IF (ITT.EQ.1) THEN
         ITT = 2
         IMIN = IZERO+1
         IMAXX = K
         ISTEP = 1
         GO TO 50
      ENDIF
C
C   QUADRATURE COMPLETED
C
C
      TOTAL = DUMNEG+DUMPOS
      DNDE = TOTAL*(0.5D0*SQRT(2.0D0*XMU)/PI)
      RETURN
      END  SUBROUTINE phsint
C**********************************************************************
C  POLINT
C**********************************************************************
      SUBROUTINE polint(XA,YA,N,X,Y,DY)
      use perconparam, only : fu6
C
C     Called by:
C               INTERP, LCG3
C
C*    USES NEVILLE'S ALGORITHM TO INTERPOLATE FOR Y,
C     FOR A GIVEN X. XA AND YA ARE OF LENGTH N. WHERE N-1
C     IS THE DEGREE OF POLYNOMIAL USED AND DY IS THE ERROR
C     ESTIMATE. 
C*
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (NMAX = 10)
      DOUBLE PRECISION XA(N),YA(N),C(NMAX),D(NMAX)
      NS = 1
      DIF = ABS(X-XA(1))
C     FIND THE INDEX NS OF THE CLOSEST TABLE ENTRY
      DO 10 I=1,N
      DIFT = ABS(X-XA(I))
      IF(DIFT.LT.DIF) THEN
        NS = I
        DIF = DIFT
      ENDIF
C     INITIALIZE THE C AND D ARRAYS
      C(I) = YA(I)
      D(I) = YA(I)
 10   CONTINUE
C     FIRST APPROXIMATION TO Y
      Y = YA(NS)
      NS = NS -1
      DO 20 M=1,N-1
      DO 15 I=1,N-M
      HO = XA(I) - X
      HP = XA(I+M) - X
      W = C(I+1) - D(I)
      DEN = HO - HP
      IF(DEN.EQ.0.0D0)THEN
        WRITE(FU6,*)' IDENTICAL X VALUES IN POLINT'
        RETURN
      ENDIF
      DEN = W/DEN
      D(I) = HP*DEN
      C(I) = HO*DEN
 15   CONTINUE
      IF(2*NS.LT.N-M) THEN
        DY = C(NS+1)
      ELSE
        DY = D(NS)
        NS =  NS - 1
      ENDIF
      Y = Y + DY
  20  CONTINUE
      RETURN
      END SUBROUTINE polint
C
C***********************************************************************
C PRMEP
C***********************************************************************
C
      SUBROUTINE prmep
      use common_inc; use rate_const; use cm; use kintcm
      use perconparam
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     This subroutine writes reaction-path energetics to fu25 if LGS2(13) > 0
C
C                                                       Wei-Ping Hu
C                                                       06/30/94
      character * 2 asymb(103)
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
C     Write table heading
C
      WRITE(FU25,500)
C
      ISBEG =  NSHLF - NSHLF  / ITVMEP * ITVMEP
      IF (ISBEG .EQ. 0) ISBEG = ITVMEP
      NSTEP = (LSAVE - ISBEG) / ITVMEP + 1
      IS = ISBEG
      DO 100 I = 1, NSTEP
        IF (IVIC.EQ.1) THEN
         WRITE(FU25,600) SSUBI(IS),VCLAS(IS)*CKCAL,VADIB(IS)*CKCAL,     0812YC97
     *                     ZOCMCD(IS),SBKAP(IS),FMITS(IS)               0812YC97
        ELSE
         WRITE(FU25,600) SSUBI(IS),VCLAS(IS)*CKCAL,VADIB(IS)*CKCAL,
     *                     CDSCMU(IS),SBKAP(IS),FMITS(IS)
        ENDIF
         IS = IS + ITVMEP
100   CONTINUE
C
      IF (LGS2(13) .GE. 2) THEN
         WRITE(FU26,700) (IFRIND(J),J=1,NFRIND)
         WRITE(FU26,*)
         IS = ISBEG
         DO 200 I = 1, NSTEP
            WRITE(FU26,800) SSUBI(IS),
     *                  (WETS(NF(5)+1-IFRIND(J),IS)*AUTOCM,J=1,NFRIND)
            IS = IS + ITVMEP
200      CONTINUE    
      ENDIF
C
C     Print xmol xyz input to unit fu27
C
      IF (LGS2(14) .EQ. 1) THEN
         AUTOA = 0.529177D0
         IS = ISBEG
         DO 300 I = 1, NSTEP
            WRITE(FU27,900) NATOM, SSUBI(IS), VCLAS(IS)*CKCAL, 
     *      (ASYMB(LABEL(J)),(GEOM(3*J-K,IS)/AMASS(3*J-K)*AUTOA,
     *       K=2,0,-1),J=1,NATOM)
            IS = IS + ITVMEP
300      CONTINUE  
      ENDIF
C
C
      RETURN    
C
500   FORMAT(1X,' s (bohr)',2X,' Vmep (kcal)',2X,'  Va^G (kcal)',       0812YC97
     *       2X,'  mu^CD-SC  ',4X,'  kappa  ',2X,'  det(I)  ',/)        0812YC97
600   FORMAT(1X,F9.4,2X,F12.4,2X,F12.4,2X,F12.4,2X,F12.4,2X,E12.4) 
700   FORMAT(12X,'mode',10X,'frequency in cm**-1',               
     *       /1X,' s (bohr)',1X,10(2X,I3,2X))                     
800   FORMAT(1X,F9.4,10F7.0)
900   FORMAT(1X,I2,/,'*  s = ',F9.4,' bohr',4X,'V = ',F10.4,' kcal',
     *       /,(A2,6X,F15.6,1X,F15.6,1X,F15.6))
C
      END SUBROUTINE prmep
C***********************************************************************
C  PROJCT
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/20/91
C
      SUBROUTINE projct
      use common_inc
      use perconparam, only: fu6,natom,eps
      use potmod; use cm
C
C     CALCULATES PROJECTED FORCE CONSTANT MATRIX
C     ALSO PROVIDES NORMALIZED GRAD(V)
C
C     CALLED BY:
C                FDIAG
C     CALLS:
C            MXLNEQ
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      real(8) :: rot(3,3)
      DIMENSION TENS(3,3,3),ISCR(3),SCR1(3),SCR2(3)                     1106YL92
      LOGICAL LDEBUG                                                    1106YL92
C
      DATA TENS / 5*0.0D0,-1.0D0,0.0D0,1.0D0,3*0.0D0,1.0D0,3*0.0D0,
     *            -1.0D0,3*0.0D0,-1.0D0,0.0D0,1.0D0,5*0.0D0 /
C
C  PROJCT is only called for generalized T.S., not for reactants or
C  products.
C
C  Skip over computing inertia tensor if external field is present.
C
      LDEBUG = .FALSE.                                                  1106YL92
      LPTBCR = LGS2(15)
      IF (LGS(34) .EQ. 0) THEN
         IF (ICODE(5).EQ.4) THEN
C
C     COMPUTE INERTIA TENSOR AND INVERT (NON-LINEAR CASE)
C
            DO 10 I = 1, 3
               DO 10 J = 1, 3
                  ROT(I,J) = 0.0D0
   10       CONTINUE
            DO 20 I = 1, NATOM
               L = 3*(I-1)+1
               ROT(1,1) = ROT(1,1)+X(L+1)**2+X(L+2)**2
               ROT(1,2) = ROT(1,2)-X(L)*X(L+1)
               ROT(1,3) = ROT(1,3)-X(L)*X(L+2)
               ROT(2,2) = ROT(2,2)+X(L)**2+X(L+2)**2
               ROT(2,3) = ROT(2,3)-X(L+1)*X(L+2)
               ROT(3,3) = ROT(3,3)+X(L)**2+X(L+1)**2
   20       CONTINUE
            ROT(2,1) = ROT(1,2)
            ROT(3,1) = ROT(1,3)
            ROT(3,2) = ROT(2,3)
            IF (LPTBCR.EQ.2.OR.LPTBCR.EQ.12) THEN                       1106YL92
               DO 210 I = 1, 3                                          1106YL92
                  BEROT(I) = 0.D0                                       1106YL92
                  DO 210 J = 1, 3                                       1106YL92
                     PVEC(I,J) = 0.D0                                   1106YL92
210            CONTINUE                                                 1106YL92
               CALL RSPP(3,3,6,ROT,BEROT,1,PVEC,SCR1,SCR2,IERR)         1106YL92
               CALL MXLNEQ (ROT,3,3,DET,JRNK,EPS,ISCR,0,3)              0601YC98
               FMOM(5) = DET*REDM**3                                    0601YC98
C               FMOM(5) = BEROT(1)*BEROT(2)*BEROT(3)*REDM**3             1106YL92
               DO 220 I = 1, 3                                          1106YL92
c                  BEROT(I) = 1/BEROT(I)                                 1106YL92
                   BEROT(I) = 1/(2.0D0*REDM*BEROT(I))                   0601YC98
220            CONTINUE                                                 1106YL92
               IF (LDEBUG) WRITE(6,219) ((PVEC(I,J),I=1,3),J=1,3)       1106YL92
            ELSE                                                        1106YL92
               CALL MXLNEQ (ROT,3,3,DET,JRNK,EPS,ISCR,0,3)              9/20DL90
               IF (JRNK.LT.3) THEN
                  WRITE (FU6,*) 
     *            ' PROBLEM WITH MXLNEQ IN SUBROUTINE PROJCT'
                  STOP 'PROJCT 1'
               ENDIF
               FMOM(5) = DET*REDM**3
            ENDIF
         ELSEIF (ICODE(5).EQ.3) THEN
C
C    LINEAR CASE : Assumed that the molecule lies along the z-axis.
C
            SUM = 0.0D0
            DO 30 I = 1, NATOM
               L = 3*I
               SUM = SUM+X(L)**2
   30       CONTINUE
            ZMOM = SUM
            FMOM(5) = REDM*SUM
         ENDIF
C
C     COMPUTE TOTAL MASS
C
         TOTM = 0.0D0
         DO 40 I = 1, NATOM
            L = 3*(I-1)+1
            TOTM = TOTM+AMASS(L)**2
   40    CONTINUE
      ENDIF
C
C     COMPUTE P MATRIX
C
      DO 100 IP = 1, NATOM
         INX = 3*(IP-1)
         DO 100 JP = 1, IP
            JNDX = 3*(JP-1)
            DO 90 IC = 1, 3
               JEND = 3
               IF (JP.EQ.IP) JEND = IC
               DO 90 JC = 1, JEND
                  SUM = 0.0D0
C
C Don't include rotation and translation projection operator if
C external field is present.
C
                IF(LGS(34).NE.0) THEN
                   II = INX + IC
                   JJ = JNDX + JC
                   PROJ(II,JJ) = DX(II)*DX(JJ)
                ELSE
C
C  FOR GAS PHASE
C
C  NON-LINEAR CASE
C
                  IF (ICODE(5).EQ.4) THEN
                     DO 80 IA = 1, 3
                        DO 80 IB = 1, 3
                           IF (TENS(IA,IB,IC)) 50, 80, 50
   50                      DO 70 JA = 1, 3
                              DO 70 JB = 1, 3
                                 IF (TENS(JA,JB,JC)) 60, 70, 60
   60                            SUM = SUM+TENS(IA,IB,IC)*TENS(JA,JB,JC)
     *                              *ROT(IA,JA)*X(INX+IB)*X(JNDX+JB)
   70                      CONTINUE
   80                CONTINUE
                  ENDIF
                  II = INX+IC
                  JJ = JNDX+JC
                  IF (ICODE(5).EQ.4) THEN
                     PROJ(II,JJ) = SUM+DX(II)*DX(JJ)
C
C  LINEAR CASE
C
                  ELSEIF (ICODE(5).EQ.3) THEN
                     IF (IC.EQ.3.AND.JC.EQ.3) THEN
                        PROJ(II,JJ) = DX(II)*DX(JJ)
                     ELSEIF (IC.NE.3.AND.IC.EQ.JC) THEN
                        PROJ(II,JJ) = X(INX+3)*X(JNDX+3)/ZMOM
                     ELSEIF (IC.NE.JC) THEN
                        PROJ(II,JJ) = 0.0D0
                     ENDIF
                  ENDIF
C
C  FOR TRANSLATION MOTION
C
                  IF (IC.EQ.JC) PROJ(II,JJ) = PROJ(II,JJ)+
     *                                        (AMASS(II)*AMASS(JJ)
     *               )/TOTM
                ENDIF
   90       CONTINUE
  100 CONTINUE
c     // extend projection matrix to include solvent coordinate
c
      IF (LBATH) THEN                                                   0317Yc99
         DO I = 1,N3                                                        ..
c                                                                           ..
c          just reaction coordinate contribution                            ..
c                                      
           PROJ(N3,I) = DX(N3)*DX(I)                                        ..
           PROJ(I,N3) = PROJ(N3,I)                                          ..
         ENDDO                                                              ..
      ENDIF                                                             0317Yc99
      DO 110 I = 1, N3
         DO 110 J = 1, I
            PROJ(I,J) = -PROJ(I,J)
            IF (I.EQ.J) PROJ(I,J) = 1.0D0+PROJ(I,J)
  110 CONTINUE
      DO 120 I = 1, N3
         DO 120 J = 1, I
            PROJ(J,I) = PROJ(I,J)
  120 CONTINUE
C
C     POST AND PREMULTIPLY F BY P.  USE COF FOR SCRATCH
C
      DO 140 I = 1, N3
         DO 140 J = 1, N3
            SUM = 0.0D0
            DO 130 K = 1, N3
               SUM = SUM+F(I,K)*PROJ(K,J)
  130       CONTINUE
            COF(I,J) = SUM
  140 CONTINUE
      DO 160 I = 1, N3
         DO 160 J = 1, N3
            SUM = 0.0D0
            DO 150 K = 1, N3
               SUM = SUM+PROJ(I,K)*COF(K,J)
  150       CONTINUE
            F(I,J) = SUM
  160 CONTINUE
c
      RETURN
  219 FORMAT(' corresponding eigenvectors:', 3(/,5X,3F15.10))           1106YL92
      END SUBROUTINE projct
C***********************************************************************
C     PRCORD
C***********************************************************************
C
      SUBROUTINE prcord(IFOUT,S,V,X,NATOM,NPRCA,IPRCA,AMASS,MW)
      use perconparam, only : natoms,n3tm,ckcal
C     CALLED BY:
C                NOROUT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION X(N3TM)
      DIMENSION XC(NATOMS),YC(NATOMS),ZC(NATOMS),AMASS(N3TM)
      DIMENSION CORD(6)
      DIMENSION IPRCA(4)
C
      PARAMETER (ONE=1.D0, NINETY=90.D0)
C
      CONV=NINETY/DASIN(ONE)
C
C     MASS-WEIGHTED TO CARTESIAN COORDINATES (IOP=2)
C     ALSO SIMULTANEOUSLY CONVERTS DERIVATIVES
C
c     calculate bond distances in Cartesians
c     standard option
c
      IF (MW.EQ.2) THEN
        DO  I = 1, NATOM
           XC(I) = X(3*I-2)/AMASS(3*I-2)
           YC(I) = X(3*I-1)/AMASS(3*I-2)
           ZC(I) = X(3*I)/AMASS(3*I-2)
        ENDDO
      ELSE
        DO  I = 1, NATOM
           XC(I) = X(3*I-2)
           YC(I) = X(3*I-1)
           ZC(I) = X(3*I)
        ENDDO
      ENDIF
c
c     calculate bond distances in Mass-Scaled Cartesians
c     alt. option
c
c      IF (MW.EQ.1) THEN
c        DO  I = 1, NATOM
c           XC(I) = X(3*I-2)/AMASS(3*I-2)
c           YC(I) = X(3*I-1)/AMASS(3*I-2)
c           ZC(I) = X(3*I)/AMASS(3*I-2)
c        ENDDO
c      ELSE
c        DO  I = 1, NATOM
c           XC(I) = X(3*I-2)
c           YC(I) = X(3*I-1)
c           ZC(I) = X(3*I)
c        ENDDO
c      ENDIF
c
      ICOUNT = 0
      DO I = 1, NPRCA-1
        ICOUNT = ICOUNT + 1
        CORD(ICOUNT)= DIST(IPRCA(I+1),IPRCA(I),XC,YC,ZC,NATOM)
      ENDDO
C
      ILB = ICOUNT - 1
      DO I = 1, ILB
        ICOUNT = ICOUNT + 1
        CORD(ICOUNT) = CONV*ANGL(IPRCA(I+2),IPRCA(I+1),IPRCA(I),
     >                    XC,YC,ZC,NATOM)
        IF(CORD(ICOUNT).LE.0.d0) CORD(ICOUNT) = CORD(ICOUNT)+180.0d0
      ENDDO
C
      IF (NPRCA.EQ.4) THEN
        ICOUNT = ICOUNT + 1
        CORD(ICOUNT)= CONV*PTORS(IPRCA(1),IPRCA(2),IPRCA(3),
     >            IPRCA(4),XC,YC,ZC,NATOM)
      ENDIF
      WRITE (IFOUT,100) S,V*CKCAL,(CORD(I),I=1,ICOUNT)
100   FORMAT (1X,F8.4,7F10.4)
      RETURN
      END SUBROUTINE prcord
C
C***********************************************************************
C  PSAG
C***********************************************************************
C
      SUBROUTINE psag (E,PE,PED,TPLX,TPR,NTP,STP,SSX,                   1016WH92
     *                 ICHAR,LIMAX,VMAXX,SMAXX,LNEGCD,IMXINT)
      use common_inc
      use perconparam, only : nsdm
      use rate_const
C
C  MCP and SO methods removed 30OCT85. BCG
C  SCSAG removed 1016WH92
C 
C     LOCATES POINTS WHERE E=VA AND CALCULATES THETA= INTEGRAL OF
C     IM(PS) BETWEEN THEM
C     PE(I) IS THE PROBAB(E) FOR THE I-TH QUADRATURE (N OR 2N+1)
C
C     CALLED BY:
C                KAPVA
C     CALLS:
C            AITKEN, PSATP, SPL1B1, SPL1B2, SPL1D1  
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C    MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION PE(2),SUM(2),THETA(2),TAB(3)
      DIMENSION STP(6),SSX(NSDM)                                        1215YL91
      DIMENSION PED(2), SUMD(2), THETAD(2), IOP(2), SCR(NSDM+1),        8/12YL91
     *          ADH(NSDM+1), BDH(NSDM+1), CDH(NSDM+1), DDH(NSDM+1)
C
      CHARACTER*2 ICHAR(6)
      CHARACTER*2 ICHRL,ICHRR
      LOGICAL LNEGCD
      save                                                              0601YC98
C
C
      NTP = 0
C
C     FOLLOWING VALUES SET IN CASE NO TURNING POINTS FOUND
C
      DO 10 I = 1, 2
         PE(I) = 0.5D0
         THETA(I) = 0.0D0
         PED(I) = 0.5D0                                                 8/26YL91
         THETAD(I) = 0.0D0                                              8/26YL91
   10 CONTINUE
C
C################################################################################
C
C   This source code was moved from the subroutine VSPLIN to this 
C   subroutine PSAG 
C
      IOP(1)     = 3                                                    8/26YL91
      IOP(2)     = 3                                                    8/26YL91
      SCR(1)     = 0.D0                                                 8/26YL91
      SCR(LSAVE) = 0.D0                                                 8/26YL91
C
C
      SCR(1)    = 0.D0                                                  8/26YL91
      SCR(LSAVE) = 0.D0                                                 8/26YL91
      CALL SPL1D1 (LSAVE, SSUBI, CDSCMU, SCR, IOP, 1, ADH, BDH, CDH)    8/26YL91
      CALL SPL1B1 (LSAVE, SSUBI, CDSCMU, SCR, 1, ADH, BDH, CDH, DDH)    8/26YL91
C
C################################################################################
C
      IFLG = 0
   30 CONTINUE
C
C   Initialize the sum terms
C
      DO 40 I = 1, 2                                                    8/26YL91
            SUM(I)  = 0.D0                                              8/26YL91
            SUMD(I) = 0.D0                                              8/26YL91
   40 CONTINUE                                                          8/26YL91
      CALL PSATP (IFLG,E,SL,SR,SN,XT,ICHRL,ICHRR,LIMAX,
     *             VMAXX,SMAXX,IMXINT)                                  8/16B91
      IF (IFLG.EQ.0) GO TO 100
C
C     PAIR OF T.P.'S FOUND. -- CALCULATE THETA INTEGRAL.
C     I=1,2 FOR DIFFERENT QUADRATURES (N AND 2N+1)
C     FOR MCPSAG, INCLUDE CURVATURE CORRECTION, WHICH IS REALLY AN
C     EFFECTIVE MU
C
      RTXMU = SQRT(REDM)
C
      DO 70 ISEG = 1, NSEG2
         DO 70 N = 1, NQ22
            S = SL+(2.0D0*DBLE(ISEG)-1.0D0+PT2(N))*XT/DBLE(NSEG2)
            CALL SPL1B2 (NSPL,SSX,ASPL,BSPL,CSPL,DSPL,S,TAB,0)
            T = TAB(1)-E
            IF (T.LT.0.0D0) GO TO 70
            T = SQRT(T)
            TEMPX = T                                                   8/26YL91
            DO 50 I = 1, 2
               SUM(I) = SUM(I)+TEMPX*WT2(N,I)*RTXMU
   50       CONTINUE
C-----------------------------------------------------------------------------
C  SCSAG has been removed from version 5.0
C-----------------------------------------------------------------------------
C CDSCSAG
C
            IF (LCDSC) THEN                                             8/26YL91
             if (lgs(30).gt.0.and.lopt(2).eq.-500) then                 081097JC
               call splnmf (s,tab)                                      080497JC
             else                                                       080497JC
               IF (LGS(32).EQ.0) THEN                                       ..
                  CALL SPL1B2 (NSPL,SSX,ADH,BDH,CDH,DDH,S,TAB,0)            ..
               ELSE                                                         ..
                  CALL AITKEN (S,SSX,CDSCMU,TAB,LGS(32),NSPL)               ..
               ENDIF                                                        ..
             endif                                                      080497JC
C                                                                           ..
               IF (TAB(1).LE.0.0D0) THEN                                    ..
                  LNEGCD = .TRUE.                                           ..
               ELSE                                                         ..
                  T = TEMPX*SQRT(TAB(1))                                    ..
                  DO 65 I = 1, 2                                            ..
                     SUMD(I) = SUMD(I)+T*WT2(N,I)                          ..
   65             CONTINUE                                                  ..
               ENDIF                                                        ..
            ENDIF                                                       8/26YL91
C------------------------------------------------------------------------------
   70 CONTINUE
      T = SQRT(2.0D0)*XT/DBLE(NSEG2)
C
C     ADD CONTRIBUTION OF T.P. PAIR TO OVERALL THETA AND P(E)
C     NOTE -- NO CONNECTION FORMULAS USED
C
      DO 80 I = 1, 2
         THETA(I) = THETA(I)+T*SUM(I)
         PE(I) = 0.0D0
C
C Add the shift constant SHIFTC=60.0 to solve the overflow and underflow 
c problem in the case that theta is too large.
c 
         SHIFTC=60.0D0
         IF (THETA(I) .LE.50.0D0) THEN                                  0302JZ09
           PE(I) = 1.0D0/(1.0D0+EXP(2.0D0*THETA(I)))                    0325JZ08
         ELSEIF (THETA(I).GT.50.0D0.AND.LGS2(12).GE.1) then             0302JZ09
           PE(I) = 1.0D0/EXP(2.0D0*THETA(I)-SHIFTC)                     0302JZ09
           PE(I) = PE(I)*EXP(-SHIFTC)
         ENDIF                                                          0302JZ09
         IF (LCDSC) THEN                                                8/26YL91
             THETAD(I)= THETAD(I)+T*SUMD(I)                               ..
             PED(I) = 0.0D0                                               ..
            IF (THETAD(I) .LE.50.0D0) THEN                              0302JZ08
               PED(I) = 1.0D0/(1.0D0+EXP(2.0D0*THETAD(I))) 
            ELSEIF (THETAD(I).GT.50.0D0.AND.LGS2(12).GE.1) then 
               PED(I) = 1.0D0/EXP(2.0D0*THETAD(I)-SHIFTC)
               PED(I) = PED(I) * EXP(-SHIFTC)
            ENDIF   
         ENDIF                                                          8/26YL91
   80    CONTINUE
C
      NTP = NTP+2
C
C     SAVE UP TO THREE T.P. PAIRS
C
      TPR = SR                                                          5/10DL90
      IF (NTP.GT.6) GO TO 30
      STP(NTP-1) = SL
      STP(NTP) = SR
      ICHAR(NTP-1) = ICHRL
      ICHAR(NTP) = ICHRR
      IF (IFLG .EQ. 2) GO TO 100                                        5/10DL90
C
C     LOOP BACK TO SEARCH FOR MORE T.P. PAIRS
C
      GO TO 30
  100 CONTINUE
      TPLX = STP(1)                                                     5/10DL90
      RETURN
      END SUBROUTINE psag 
C
C***********************************************************************
C  PSATP
C***********************************************************************
C
C   PAARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C

      SUBROUTINE psatp (IFLG,E,SL,SR,SN,XT,ICHRL,ICHRR,
     *                  IMAXX,VMAXX,SMAXX,IMXINT)                       8/16B91
      use perconparam
      use rate_const
C
C     PSATP     - find turning points in adiabatic barrier
C                 added 10/13/1986  by bcg.
C
C  Called by:
C     PSAG
C
C  Calls:
C     PSATP2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LRT
C
      CHARACTER*2 ICHRL,ICHRR
      save                                                              0601YC98
C
      ICHRL = '  '
      ICHRR = '  '
      SGN = 1.0D0
      ITP = 1
      IF (IFLG.EQ.0) THEN
C
C  IFLG = 0, start from left most point
C  IFLG .NE. 0, start from last point
C
         IS0 = 1
         LRT = .FALSE.
C
C  check if the first point is higher than the energy, if so use the
C  first point as the turning point
C
         IF (E.LT.VADIB(1)) THEN
            SGN = -1.0D0
            ICHRL = '**'
            SL = SSUBI(1)
            ITP = 2
            IFLG = 1
         ENDIF
      ENDIF
      IF (ITP.EQ.1) THEN
         CALL PSATP2 (IFLG,E,S,IS0,LRT,SGN,ITP,
     *                ICHRR,IMAXX,VMAXX,SMAXX,IMXINT)                   8/16B91 
         SL = S
         SGN = -1.0D0
      ENDIF
      IF (IFLG.NE.0) THEN
         CALL PSATP2 (IFLG,E,S,IS0,LRT,SGN,ITP,
     *                ICHRR,IMAXX,VMAXX,SMAXX,IMXINT)                   8/16B91   
         SR = S
         SN = (SL+SR)*0.5D0
         XT = (SR-SL)*0.5D0
      ENDIF
      IF (ITP .EQ. 4) IFLG = 2                                          5/10DL90
      RETURN
      END SUBROUTINE psatp
C
C***********************************************************************
C  PSATP2
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C

      SUBROUTINE psatp2 (IFLG,E,SX,IS0,LRT,SGN,ITP,
     *                   ICHRR,IMAXX,VMAXX,SMAXX,IMXINT)
      use perconparam
      use rate_const 
C
C     PSATP2    - find turning points in adiabatic barrier
C                 added 10/13/1986  by bcg.
C
C  Called by:
C     PSATP
C
C  Calls:
C     CUBIC2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LRT,LSET
C
      DIMENSION RRT(3),AIRT(3),SRT(4)
      CHARACTER*2 ICHRR
      save                                                              0601YC98
C
C  IFLG = 1,  successfully found tps
C  IFLG = 0,  no (more) tps found
C
      NDUM = 0                                                          09/95KAN
      IFLG = 1
      LSET = .FALSE.                                                    8/16B91
      IF (E.GE.VMAXX) THEN                                              8/16B91
C  For energies >= maximum set turning points to SMAXX                  8/16B91
         SX = SMAXX                                                     8/16B91
         IF (SGN.GT.0.0D0) THEN                                         8/16B91
C  For first time through set IS0 to NSPL, otherwise return IFLG=0        8/16B91
C    to signal no more tps                                              8/16B91
            IF (IS0.EQ.1) THEN                                          8/16B91
               IS0 = NSPL                                               8/16B91
            ELSE                                                        8/16B91
               IFLG = 0                                                 8/16B91
            END IF                                                      8/16B91
         ENDIF                                                          8/16B91
      ELSE IF (LRT) THEN                                                8/16B91
C  a root from the cubic polynomial left over, use it before searching
C     through the grid again
         NRC = NRC+1
         SX = SRT(NRC)
         IF (NRC.EQ.NRT) LRT = .FALSE.
      ELSE 
         IF (E.GT.VADIB(IMAXX)) THEN                                    8/16B91
C  Energy is between the maximum on the grid and the maximum from the   8/16B91
C    spline fit.  If the second time called with SGN>0, IFLG=0 will be  8/16B91
C    returned.                                                          8/16B91
            LSET = IS0.EQ.1.OR.SGN.LT.0                                 8/16B91
C  Roots of cubic for the interval IMXINT where the max in VS occurs    8/16B91
C     are found below.  The index is incremented by 1 because it is     8/16B91
C     decremented by 1 below.                                           8/16B91
            IS = IMXINT + 1                                             8/16B91
C  Set IS0 so that in the next call with SGN>0, IFLG=0 will be returned 8/16B91
            IS0 = NSPL                                                  8/16B91
         ELSE                                                           8/16B91
C  search grid until E-V changes sign or end of grid is hit
            IS = IS0
10          CONTINUE
               IF (LSET.OR.IS.GE.NSPL) GO TO 20
               IS = IS+1
               LSET = (E-VADIB(IS))*SGN.LE.0.0D0
            GO TO 10
20          CONTINUE
         END IF
         IS0 = IS                                                       8/16B91
C
         IF (LSET) THEN
C  E-V changed sign between grid points IS-1 and IS
            IS = IS-1
C  S1 and S2 are left and right bounds on the turning point
            S1 = SSUBI(IS)
            S2 = SSUBI(IS+1)
            DD = DSPL(IS)-E
C  V is expressed as a cubic spline, solve for zeros of cubic polynomial
            ASPDMY = ASPL(IS)                                           7/14YL92
            BSPDMY = BSPL(IS)                                           7/14YL92
            CSPDMY = CSPL(IS)                                           7/14YL92
            CALL CUBIC2 (ASPDMY,BSPDMY,CSPDMY,DD,NREAL,RRT,AIRT)        7/14YL92
            NRT = 0
            IF (NREAL.GE.1) THEN
C  NREAL roots found, check if they are between S1 and S2
               NRT = 0
               DO 30 I = 1, NREAL
                  IF (RRT(I).GE.S1.AND.RRT(I).LE.S2) THEN
                     NRT = NRT+1
                     SRT(NRT) = RRT(I)
                  END IF
   30          CONTINUE
            ENDIF
            IF (NRT.GT.0) THEN
C  NRT valid real roots found, use lowest one
               NRC = 1
               SX = SRT(1)
               IF (NRT.GT.1) LRT = .TRUE.
            ELSE
C  no valid real root found, use the bound
               IF (ITP.EQ.1) SX = S1
               IF (ITP.EQ.2) SX = S2
            ENDIF
         ELSE
C
C  either ran into end of grid or second pass with E>VADIB(IMAXX) and SGN>0 8/16B91
            IF (SGN.LT.0.0D0) THEN
C  looking for right turning point but couldn't find it
               ICHRR = '**'
               SX = SSUBI(NSPL)
            ELSE
C  looking for left turning point but couldn't find it, return with
C     IFLG=0 (signals end of tps)
               IFLG = 0
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE psatp2
C
C***********************************************************************
C  PTQVIB
C***********************************************************************
C
C
      FUNCTION ptqvib (NMOD,N3TM,EGRND,EFNDT,FREQ,BKT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     Computes vibrational partition function using the vibrational
C     ground-state energy and fundementals from perturbation theory
C     for the nondegenerate case.
C
C     The formula for the partition function are given in 
C     J. Chem. Phys. 94, 357, 1991.  
C
C      CALLED BY:
C                 RATE
C
      DIMENSION EFNDT(N3TM),FREQ(NMOD)
C
      IFDMY = 0
      CALL CHKFRE(NMOD,FREQ,IFDMY)
      IF (IFDMY.EQ.0) THEN
         PROD = EGRND/BKT
c         PROD = 1 - EXP(-PROD)                                         0601YC98
c         PROD = 1/PROD                                                 0601YC98
         PROD = EXP(-PROD)                                              0601YC98
         DO 100 I = 1, NMOD
            FACTOR = EFNDT(I)/BKT
            FACTOR = 1 - EXP(-FACTOR)
            FACTOR = 1/FACTOR
            PROD = PROD * FACTOR
100      CONTINUE
         PTQVIB = PROD
      ELSE
         PTQVIB = 10.0D0**(10.0D0*DBLE(IFDMY))
      ENDIF
      RETURN
C
      END FUNCTION ptqvib
C
C***********************************************************************
C  QQPOT
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/20/91
C
      SUBROUTINE qqpot (LIFREQ,JFREQ)
      use perconparam
      use common_inc
C
C     THIS SUBROUTINE FITS THE QUADRATIC-QUARTIC POTENTIAL TO TWO POINTS
C     ALONG THE NORMAL COORDINATE USING STEP SIZES DQQP(1) AND DQQP(2)
C
C     CALLED BY:
C                ANHARM
C     CALLS:
C            TRANS,ENERG,LIN2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION TX(N3TM),TDX(N3TM),VMVE(2)
C
C
C     PUT CURRENT GEOMETRY IN TEMP. STORAGE
C
      IF (S.LT.0.0D0) LWDMY = 1
      DO 10 I = 1, N3
         TX(I) = X(I)
         TDX(I) = DX(I)
   10 CONTINUE
c      call energ(1)                                                     6/2RS94
      call ehook(1,iproc)                                                     0301YC97
      VI = V
C
C     MOVE ALONG NORMAL COORDINATE
C
      DO 30 J = 1, 2
         DO 20 I = 1, N3
            X(I) = TX(I)+DQQP(J)*COF(I,JFREQ)
   20    CONTINUE
c         call energ(1)                                                  6/2RS94
         call ehook(1,iproc)                                                  0301YC97 
         VMVE(J) = V-VI
   30 CONTINUE
      A11 = 0.5D0*DQQP(1)*DQQP(1)
      A12 = A11*A11/6.0D0
      A21 = 0.5D0*DQQP(2)*DQQP(2)
      A22 = A21*A21/6.0D0
      B1DMY = VMVE(1)                                                   7/14YL92
      B2DMY = VMVE(2)                                                   7/14YL92
      CALL LIN2 (A11,A12,A21,A22,B1DMY,B2DMY,ANHDMY,ABDMY)              7/14YL92
      ANHRM(LIFREQ) = ANHDMY                                            7/14YL92
      AB(LIFREQ) = ABDMY                                                7/14YL92
      DO 40 I = 1, N3
         X(I) = TX(I)
         DX(I) = TDX(I)
   40 CONTINUE
      V = VI                                                            9/18YL92
      RETURN
      END SUBROUTINE qqpot
C
C**********************************************************************
C QRSENE
C**********************************************************************
C
      subroutine qrsene(VM,NB,ENE0,ENRC,DEDNRC)
      use common_inc
      use perconparam, only : fu6,maxwkb
      use rate_const
C
C     This subroutine calculates the quantized energy states
C     for the quantized reactant state tunneling calculations
C
C                                                    Wei-Ping Hu, June 1994
C     Called by:
C                KAPVA
C
C     Calls:
C                WKBENE
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      save                                                              0601YC98
C
      DIMENSION DEDNRC(0:MAXWKB), ENRC(0:MAXWKB)
C
      WR = WER(NF(1) + 1 - IWR)                                      
C
C     If use wkb for the reaction coordinate, call WKBENE
C     to calculate En and dEn/dn, otherwise set them to harmonic values
C
      IF (LGS2(12) .GT. 1) THEN         
         LGS212 = LGS2(12)             
         LGS217 = LGS2(17)                                              0423TA02
         CALL WKBENE(DEDNRC,ENRC,LGS212,LGS217,NB,NBOUND,IQRNSQ,        0523TA02
     *               REDM,SMAX,SRW,SSUBI,VADIB,WR)                      0523TA02
      ELSE                                                         
C         NB = INT(VM / WR)
         NSTAOP = INT(VM / WR)
         IF (LGS2(17) .GE. 0 .AND. LGS2(17) .LT. NSTAOP) THEN           0423TA02
            NB = LGS2(17)                                               0522TA02
         ELSE                                                           0423TA02
            NB = NSTAOP                                                 0423TA02
         ENDIF                                                          0423TA02
         IF (IQRNSQ .EQ. -1) THEN                                       0522TA02
            NBOUND = NSTAOP                                             0522TA02
         ELSEIF (IQRNSQ .EQ. -2) THEN                                   0522TA02
            NBOUND = NB                                                 0522TA02
         ENDIF                                                          0522TA02
         NBND = MAX0(NB,NBOUND)                                         0522TA02
         IF (NBND .GT. MAXWKB) THEN                                     0522TA02
            WRITE(FU6,1060) NB,MAXWKB                                   0423TA02
            STOP 'QRSENE 1'                                             0423TA02
         ENDIF                                                          0423TA02
         DO 60 N = 0, NBND                                              0522TA02
            ENRC(N) = (DBLE(N) + 0.5D0) * WR                       
            DEDNRC(N) = WR                                        
60       CONTINUE                                                 
      ENDIF
 1060 FORMAT(/1X,'ERROR: NUMBER OF VIBRATIONAL STATES',i3,              0423TA02
     *           ' EXCEED MAXWKB',i3,'.',                               0423TA02
     *       /1X,'INCREASE MAXWKB IN PARAM.INC AND RECOMPILE')          0423TA02
C 
       IF (LGS2(12) .EQ. 1) THEN                                     
          ENE0 = VAR - 0.5D0 * WR                                   
       ELSE                                                        
C
C     The reaction path mode at the reactant is alway the lowest freq. mode
C
          ENE0 = VAR - 0.5D0 * WER(1)                             
       ENDIF        
C
      CONTINUE
C
      RETURN                                                            0522TA02
      END subroutine qrsene
C***********************************************************************
C  QSLVE
C***********************************************************************
C
      SUBROUTINE qslve (X1,X2,F1,F2,FD,Y,X)
      use perconparam
C
C    FINDS A QUADRATIC EQUATION WITH VALUES F1 AT X1 AND F2 AT X2, AND
C    WITH DERIVATIVE FD AT X1. THEN FINDS THE POINT X WHERE THE FUNCTION
C    EQUALS Y AND X1 < X < X2. IF FIT FAILS OR X CANNOT BE FOUND, FIND X
C    BY LINEAR INTERPOLATION USING F1 AND F2.
C
C    CALLED BY:  TP
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C  TEST FOR PROPER CONDITIONS TO PERFORM INTERPOLATION
C
      IF ((Y.GT.F1.AND.Y.GT.F2).OR.(Y.LT.F1.AND.Y.LT.F2)) THEN
         WRITE (FU6,1000) F1,F2,Y
         RETURN
      ENDIF
C
      DELX = X2-X1
      C = F1
      B = FD
      A = (F2-C-(B*DELX))/(DELX*DELX)
C
C THE FUNCTION IS G(X)= A*(X-X1)**2 + B*(X-X1) + C
C NOW SOLVE FOR G(X)=Y
C
      C = C-Y
      RAD = B*B-4.0D0*A*C
      IF (RAD.GE.0.0D0.AND.A.NE.0.0D0) THEN
         RAD = SQRT(RAD)
         XP = X1+(-B+RAD)/(2.0D0*A)
         XM = X1+(-B-RAD)/(2.0D0*A)
C
C TEST VALUES OF X AND RETURN IF OK
C
         IF ((XP.GT.X1.AND.XP.LT.X2).OR.(XP.LT.X1.AND.XP.GT.X2)) THEN
            X = XP
         ELSEIF ((XM.GT.X1.AND.XM.LT.X2).OR.(XM.LT.X1.AND.XM.GT.X2))
     *       THEN
            X = XM
         ELSEIF (XM.EQ.X1.OR.XM.EQ.X2) THEN
            X = XM
         ELSEIF (XP.EQ.X1.OR.XP.EQ.X2) THEN
            X = XP
         ENDIF
      ELSE
C
C  DO LINEAR INTERPOLATION
C
         X = X1+(X2-X1)*(Y-F1)/(F2-F1)
      ENDIF
C
      RETURN
C
 1000 FORMAT(/,' ***** ERROR IN INTERPOLATION FROM QSLVE:',/,
     * ' F(X1) = ',E15.8,';  F(X2) = ',E15.8,';  Y = ',F15.8)
C
      END SUBROUTINE qslve
C
C***********************************************************************
C QTQVIB
C***********************************************************************
C
      SUBROUTINE qtqvib(BKT,QVIBF,QVIBR)
      use perconparam
      use rate_const, only : enlvrc,nbound,iwr
      use common_inc, only : var,nf,wer
C
C     This subroutine calculates QR(F,3) and QR(3,F) vibrational
C     partition function for the quantized reactant state tunneling calc.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      WR = WER(NF(1)+1-IWR)
C
C     Calculate QR(3,F)
C
      QVIBR = EXP(-WR/2/BKT) / (1-EXP(-WER(1)/BKT))
C
C     Calculate QR(F,3)
C
      QVIBF = 0.0D0
      DO 10 IM = 0, NBOUND 
         QVIBF = QVIBF + EXP(-( WER(1)/2+ENLVRC(IM)-VAR )/BKT)
10    CONTINUE
C
      RETURN
C
      END SUBROUTINE qtqvib
C
C***********************************************************************
C  QUADFT
C***********************************************************************
C
      SUBROUTINE quadft (X,Y,Z)
C
C     QUADFT - quadratic fit of three points
C
C      CALLED BY:
C                  RPHB,BCALC,MUBAR
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(3),Y(3),Z(3)
      T = X(3)-X(1)
      T1 = (Y(3)-Y(1))/T
      CP = (T1-(Y(2)-Y(1))/(X(2)-X(1)))/(X(3)-X(2))
      BP = T1-CP*T
      Z(3) = CP
      Z(2) = BP-2.0D0*CP*X(1)
      Z(1) = Y(1)-X(1)*(BP-CP*X(1))
      RETURN
      END SUBROUTINE quadft
C
C***********************************************************************
C  QUADTW
C***********************************************************************
C
      SUBROUTINE quadtw (X,Y,Z)
C
C                                                   2
C     For a function of the form Y(X) = A + B(X - C) , where A = Y(2),
C     find B and C given the information of (X(1),Y(1)) and (X(3),Y(3)),
C     X is stored in the order of X(1) < X(2) < X(3).
C 
C
C      CALLED BY:
C                  LCG3
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(3),Y(3)
      DX = X(3)-X(1)
      DY = (Y(2)-Y(3))
      DYSQ = SQRT(ABS(DY))
      DX = DX * DYSQ
      DY2 = DYSQ + SQRT(ABS(Y(2)-Y(1)))
      X(2) = X(3) - DX/DY2
      Z = -DY/(X(3) - X(2))**2
      RETURN
      END SUBROUTINE quadtw
c

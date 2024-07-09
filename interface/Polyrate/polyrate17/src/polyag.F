C***********************************************************************
C  AITKEN
C***********************************************************************
C
      SUBROUTINE aitken (SX,SSX,XMUXX,TAB,LNINT1,LNSMAX)
      use perconparam, only : NSDM
C
C     AITKEN - do Aitken interpolation of XMUXX from r.p. grid
C     ADDED TO POLYRATE ON 3/14/86 BY R. STECKLER
C     PARAMETERS AND COMMON BLOCKS MOVED ON 6/18/91
C
C     MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
C  Called by:
C             PSAG
C  Calls:
C         AITKF2,LOCS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C*
      DIMENSION FX(40),TAB(3),SSX(NSDM),XMUXX(NSDM)                     1215YL91

c     write(6,*) 'SX, SSX, XMUXX,LNINT1,LNSMAX',SX,
c    *           LNINT1,LNSMAX
C
C  If SX is off the grid set values to last point on grid
C
      IF (SX.LE.SSX(1)) THEN
         TAB(1) = XMUXX(1)
      ELSEIF (SX.GE.SSX(LNSMAX)) THEN
         TAB(1) = XMUXX(LNSMAX)
      ELSE
C
C  Interpolate
C
         NP = LNINT1+1
         NN = NP/2-1
C
C  Locate grid point for SX on the grid
C
         IS = 2
c        write(6,*) 'LNSMAX',LNSMAX
c        write(6,*) 'SX ', SX 
c        write(6,*) 'SSX ', SSX
         CALL LOCS (IS,SX,SSX,LNSMAX)
         IS = MAX(1,IS-NN)
         IS = MIN(IS,LNSMAX-NP)
c       write(6,*) 'IS,SX,XMUXX(IS),FX,SSX(IS),LNINT1', IS
c    *              SX,XMUXX(IS),FX,SSX(IS),LNINT1
         TAB(1) = AITKF2(SX,XMUXX(IS),FX,SSX(IS),LNINT1)
c       write(6,*) 'TAB(1) ' , TAB(1)
      ENDIF
      RETURN
      END SUBROUTINE aitken
C
C***********************************************************************
C  AITKF2
C***********************************************************************
C
      FUNCTION aitkf2 (Y,F,FX,X,NINT1)
      use perconparam, only : fu6
C
C   SUBROUTINE ADDED TO POLYRATE 3/21/86 BY R. STECKLER
C   TAKEN FROM THE ABC CODE THAT WAS WRITTEN ON 7/11/83
C     CHECKS AITKEN INTERPOLATION FOR "BAD" VALUES
C
C     CALLED BY:
C                AITKEN
C     CALLS:
C           AITKNF
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION FX(40),F(40),X(40)
      N = NINT1
      I0 = 0
      TOLD = 0.0D0
   10 NP1 = N+1
      DO 20 I = 1, NP1
         IF (I.EQ.1) GO TO 20
         IF (X(I+I0).EQ.X(I+I0-1)) GO TO 50
   20    FX(I+I0) = F(I+I0)
      T = AITKNF(Y,FX(I0+1),X(I0+1),N)
      IF (N.LE.1) GO TO 40
      FMIN = F(I0+1)
      FMAX = F(I0+1)
      DO 30 I = 2, NP1
         FMIN = MIN(FMIN,F(I+I0))
         FMAX = MAX(FMAX,F(I+I0))
   30 CONTINUE
      IF (T.GE.FMIN.AND.T.LE.FMAX) GO TO 40
      TT = TOLD-T
      IF (T.NE.0.0D0) TT = TT/T
      IF (ABS(TT).LT.0.1D0) GO TO 40
      TOLD = T
      N = N-1
      NH = NP1/2
      IF (X(NH+I0+1).LT.Y) I0 = I0+1
      GO TO 10
   40 AITKF2 = T
      RETURN
   50 CONTINUE
      WRITE (FU6,1000) I0,N,NP1,Y,(X(I0+I),F(I0+I),I=1,NP1)
      STOP 'AITKF2 1'
C
 1000 FORMAT(' AITKF2, I0,N,NP1,Y=',3I5,1PE15.7/' X,F='/(1X,2E15.7))
C
      END FUNCTION aitkf2
C***********************************************************************
C  AITKNF
C***********************************************************************
C
      FUNCTION aitknf (Y,FX,X,N)
C
C      This function subprogram is compatible with the UCC function.
C     CALLED BY:
C                AITKF2,RPHINT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FX(*),X(*)                                              01/13/GL92
      NP1 = N+1
      DO 20 I = 1, N
         IP = I+1
         DO 10 J = IP, NP1
            FX(J) = (FX(I)*(X(J)-Y)-FX(J)*(X(I)-Y))/(X(J)-X(I))
   10    CONTINUE
   20 CONTINUE
      AITKNF = FX(NP1)
      RETURN
      END FUNCTION aitknf
C***********************************************************************
C  ALFCT
C***********************************************************************
C
      SUBROUTINE alfct (N,A)
C
C Calculates A=log((2N)!) for large N
C     CALLED BY:
C                BRNULI
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N)
      A(1) = LOG(2.0D0)
      IF (N.LT.2) RETURN
      DO 10 I = 2, N
         J = 2*I
         FJ = DBLE(J)
         A(I) = LOG(FJ)+LOG(FJ-1.0D0)+A(I-1)
   10 CONTINUE
      RETURN
      END SUBROUTINE alfct
C
C***************************************************************************
C ANCOEF
C***************************************************************************
      SUBROUTINE ancoef(NMOD,NEND,LPTBCR,IDBG,FREQ,
     *                  BE,ZETA,C,Q,EGRND,EFNTP,ENUT,ANCO)
      use perconparam
C
C This subroutine evaulates the constant term (E ) and the mnharmonicity
C                                               0
C coefficient x   , and then calculates the ground state energy and the
C              ij
C fundementals.
C
C On input:
C          NMOD     :  number of vibrational normal modes
C          N3TM     :  3 * (maximum number of atoms [i.e., NATOMS])
C          NEND     :  3 * (number of atoms in the species)
C          LPTBCR   :  option for including the coriolis terms
C          IDBG     :  extra output to unit IDBG
C          AUTOCM   :  the factor for converting a.u. to cm**-1
C          FREQ     :  normal mode freqnencies
C          BE       :  rotational constants
C          ZETA     :  coriolis constants
C          C(I,J,K) :  f
C                       ijk
C          Q(I,J)   :  f
C                       iijj
C          
C On output:
C          EGRND    :  the ground state energy
C          EFNTP    :  the fundementals
C
C Calls:
C        CHKUP
C
C Called by:
C        NORMOD
C          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDEBUG                                                    09/95KAN
C
      DIMENSION C(N3TM,N3TM,N3TM),Q(N3TM,N3TM),EFNTP(N3TM)
      DIMENSION BE(3),ZETA(N3TM,N3TM,3),ANCO(N3TM,N3TM)
      DIMENSION W(N3TM),FREQ(N3TM),DUMMY(N3TM,N3TM)
      DATA ZERO/0.0D0/
c     LOGICAL LDEBUG                                                    09/95KAN
C
C     STATEMENT FUNCTIONS
      F(I,J)=(8.D0*W(I)**2-3.D0*W(J)**2)/(4.D0*W(J)*(4.D0*W(I)**2-
     $ W(J)**2))
      A(I,J)=0.5D0/W(J)+0.125D0/(2.D0*W(I)+W(J))
      G(I,J)=2.D0*W(I)/(4.D0*W(I)**2-W(J)**2)
      B(I,J)=0.5D0/(2.D0*W(I)+W(J))
      D(I,J,K)=(W(I)+W(J)+W(K))*(W(I)-W(J)-W(K))*(W(I)-W(J)+W(K))
     $        *(W(I)+W(J)-W(K))
      H(I,J,K)=W(K)*(W(K)**2-W(I)**2-W(J)**2)/(2.D0*D(I,J,K))
      Z(I,J,K)=1/(W(I)+W(J)+W(K))
      Z1(I,J,K)=1/(W(I)+W(J)-W(K))
      Z2(I,J,K)=1/(W(I)-W(J)+W(K))
      Z3(I,J,K)=1/(W(I)-W(J)-W(K))
      E1(I,J,K)=ABS(1/Z1(I,J,K))
      E2(I,J,K)=ABS(1/Z2(I,J,K))
      E3(I,J,K)=ABS(1/Z3(I,J,K))
C     END OF STATEMENT FUNCTIONS
      IDBG2 = 70
      LDEBUG = (LPTBCR.EQ.11).OR.(LPTBCR.EQ.12)
c
c P is the factor used to check if the terms are required to be "deperturb"
c In this case, it is set to be a large number, which means PT2 is always used
c
      P = 100000.0D0
c
c set P = 0 so deperturb all the time
C      P = 0.20d0
C
      ENUT=0.D0
      EGRND = 0.D0
      EDUMMY = 0.D0
      DO 5 I = 1, N3TM
         EFNTP(I) = 0.0D0
         DO 5 J = 1, N3TM
            DUMMY(I,J) = 0.D0
5     CONTINUE
C     DIAGONAL XII IN A.U.
c      WRITE (6,*) ' NEND AND NMOD', NEND,NMOD
      ISHFT = NEND - NMOD
      DO 6 I = 1, NMOD
         W(I) = FREQ(I+ISHFT)
6     CONTINUE
      DO 50 I=1,NMOD
      ENUT=ENUT+0.375D0*Q(I,I)-0.4375D0*C(I,I,I)**2/W(I)
      ANCO(I,I)=1.5D0*Q(I,I)-(3.75D0/W(I))*C(I,I,I)**2
      IF(I.EQ.1)  WRITE(IDBG2,108) ANCO(I,I),Q(I,I),W(I),C(I,I,I)
108   FORMAT(1X,'ANCO(1,1)=',E20.10,1X,'Q(1,1)=',E20.10,1X,
     $'W(1)=',E20.10,1X,'C(1,1,1)=',E20.10)
      IF(I.EQ.3)  WRITE(IDBG2,128) ANCO(I,I),Q(I,I),W(I),C(I,I,I)
128   FORMAT(1X,'ANCO(3,3)=',E20.10,1X,'Q(3,3)=',E20.10,1X,
     $'W(3)=',E20.10,1X,'C(3,3,3)=',E20.10)
      SUM=0.D0
      DO 25 J=1,NMOD
      IF(J.EQ.I) GO TO 25
      U1=ABS(2.D0*W(I)-W(J))*P
C      U2=ABS(CHKUP(I,I,J,N3TM,C))                                      0601YC98
      U2=ABS(CHKUP(I,I,J,N3TM,C)) / 2.0d0                               0601YC98
      IF(U2.GE.U1) GOTO 10
      SUM=SUM+F(I,J)*CHKUP(I,I,J,N3TM,C)**2
      ENUT=ENUT+0.1875D0*CHKUP(I,I,J,N3TM,C)**2*W(J)/(4.D0*W(I)**2
     $        -W(J)**2)
      GOTO 11
   10 SUM=SUM+A(I,J)*CHKUP(I,I,J,N3TM,C)**2
      ENUT=ENUT-0.09375D0*CHKUP(I,I,J,N3TM,C)**2/(2.D0*W(I)+W(J))
   11 IF(I.EQ.1) WRITE(IDBG2,118) SUM,W(J),F(I,J),CHKUP(I,I,J,N3TM,C)
118   FORMAT(1X,'SUM=',E20.10,1X,'W(J)=',E20.10,1X,'F(1,J)=',
     $E20.10,1X,'CHKUP(1,1,J)=',E20.10)
      IF(I.EQ.3) WRITE(IDBG2,138) SUM,W(J),F(I,J),CHKUP(I,I,J,N3TM,C)
138   FORMAT(1X,'SUM=',E20.10,1X,'W(J)=',E20.10,1X,'F(3,J)=',
     $E20.10,1X,'CHKUP(3,3,J)=',E20.10)
   25 CONTINUE
      ANCO(I,I)=(ANCO(I,I)-SUM)
   50 CONTINUE
      IF(NMOD.EQ.1) GO TO 200
      NMODM1=NMOD-1
C     OFF-DIAGONAL XIJ IN A.U.
      DO 150 I=1,NMODM1
      JSTART=I+1
      DO 100 J=JSTART,NMOD
      ANCO(I,J)=Q(I,J)-3.D0*C(I,I,I)*C(I,J,J)/W(I)-3.D0*C(J,J,J)*
     $C(I,I,J)/W(J)
      U1=ABS(2.D0*W(I)-W(J))*P
c      U2=ABS(C(I,I,J))                                                 0601YC98
      U2=ABS(C(I,I,J))/2.0d0                                            0601YC98
      IF(U2.GE.U1) GOTO 103
      ANCO(I,J)=ANCO(I,J)-G(I,J)*C(I,I,J)**2
      GOTO 104
  103 ANCO(I,J)=ANCO(I,J)-B(I,J)*C(I,I,J)**2
  104 U1=ABS(2.D0*W(J)-W(I))*P
c      U2=ABS(C(I,J,J))                                                 0601YC98
      U2=ABS(C(I,J,J))/2.0d0                                            0601YC98
      IF(U2.GE.U1) GOTO 105
      ANCO(I,J)=ANCO(I,J)-G(J,I)*C(I,J,J)**2
      GOTO 106
  105 ANCO(I,J)=ANCO(I,J)-B(J,I)*C(I,J,J)**2
  106 SUM=0.D0
      DO 75 K=1,NMOD
      IF(K.EQ.I.OR.K.EQ.J) GO TO 75
      
      U1=ABS(W(I)+W(J)-W(K))*P
      U2=ABS(CHKUP(I,J,K,N3TM,C))
C     WRITE(IDBG2,301) U1,U2,I,J,K
C 301 FORMAT(1X,'U11=',E20.10,1X,'U12=',E20.10,5X,I4,I4,I4)
C  CHECK E1,E2,E3 WHICH ONEIS THE SMALLEST ONE, THEN THE RESONANCE 
C  TERM IS DETERMINED AND IS REPLACED.
      IF(E2(I,J,K).LE.E1(I,J,K)) GOTO 110
      IF(E3(I,J,K).LE.E1(I,J,K)) GOTO 120
      IF(U2.GE.U1) GOTO 300
 110  U1=ABS(W(I)+W(K)-W(J))*P
      U2=ABS(CHKUP(I,J,K,N3TM,C))
C     WRITE(IDBG2,302) U1,U2,I,J,K
C 302 FORMAT(1X,'U21=',E20.10,1X,'U22=',E20.10,5X,I4,I4,I4)
      IF(E3(I,J,K).LE.E2(I,J,K)) GOTO 120
      IF(U2.GE.U1) GOTO 350
 120  U1=ABS(W(J)+W(K)-W(I))*P
      U2=ABS(CHKUP(I,J,K,N3TM,C))
C     WRITE(IDBG2,303) U1,U2,I,J,K
C 303 FORMAT(1X,'U31=',E20.10,1X,'U32=',E20.10,5X,I4,I4,I4)
      IF(U2.GE.U1) GOTO 400
      SUM=SUM+H(I,J,K)*CHKUP(I,J,K,N3TM,C)**2
      IF(K.LT.J) GO TO 75
      ENUT=ENUT-0.25D0*C(I,J,K)**2*W(I)*W(J)*W(K)/D(I,J,K)
      GOTO 75
  300 SUM=SUM+0.125D0*(Z(I,J,K)+Z2(I,J,K)-Z3(I,J,K))                
     $ *CHKUP(I,J,K,N3TM,C)**2
      IF(K.LT.J) GO TO 75
      ENUT=ENUT-0.03125D0*(Z(I,J,K)-Z2(I,J,K)+Z3(I,J,K))
      GOTO 75
  350 SUM=SUM+0.125D0*(Z(I,J,K)-Z1(I,J,K)-Z3(I,J,K))                         
     $ *CHKUP(I,J,K,N3TM,C)**2
      IF(K.LT.J) GO TO 75
      ENUT=ENUT-0.03125D0*(Z(I,J,K)-Z1(I,J,K)+Z3(I,J,K))
      GOTO 75
  400 SUM=SUM+0.125D0*(Z(I,J,K)-Z1(I,J,K)+Z2(I,J,K))
     $ *CHKUP(I,J,K,N3TM,C)**2
      IF(K.LT.J) GO TO 75
      ENUT=ENUT-0.03125D0*(Z(I,J,K)-Z1(I,J,K)-Z2(I,J,K))
   75 CONTINUE
      ANCO(I,J)=ANCO(I,J)-SUM
      IF(LPTBCR.NE.2.AND.LPTBCR.NE.12) GO TO 550
      WFAC=(W(I)/W(J))+(W(J)/W(I))
            DO 500 K=1,3
               DUMMY(I,J)=DUMMY(I,J)+BE(K)*(ZETA(I,J,K)**2)*WFAC
               ANCO(I,J)=ANCO(I,J)+DUMMY(I,J)
               EDUMMY = EDUMMY -0.5D0*BE(K)*ZETA(I,J,K)**2              09/95KAN
  500       CONTINUE
  550       ANCO(J,I) = ANCO(I,J)
            DUMMY(J,I) = DUMMY(I,J)
  100    CONTINUE
  150 CONTINUE
         DO 501 K=1,3
            EDUMMY = EDUMMY -0.25D0*BE(K)                               09/95KAN
            ENUT = ENUT+ EDUMMY
  501    CONTINUE
      WRITE(IDBG2,9) ((ANCO(I,J)*AUTOCM,I=1,NMOD),J=1,NMOD)
    9 FORMAT(1X,'ANCO(I,J)=',(5E20.10))
      WRITE(IDBG2,19) ENUT*AUTOCM
   19 FORMAT(1X,'ENUT =',E20.10)
      EGRND = ENUT
      DO 210 I = 1, NMOD
         SUM = 0.0D0
         EGRND = EGRND + 0.125D0*ANCO(I,I)
         SUM = SUM + 1.5D0*ANCO(I,I)
         DO 220 J = 1, NMOD
            SUM = SUM + 0.5D0*ANCO(I,J)
            EGRND = EGRND + 0.125D0*ANCO(I,J)
220      CONTINUE
         EFNTP(I) = SUM
210   CONTINUE
200   DO 230 I = 1, NMOD
         EFNTP(I) = EFNTP(I) + W(I)
         EGRND = EGRND + 0.5D0*W(I)
230   CONTINUE
c     JUST CHECK FOR FUNDALMENTALS ALONG THE PATH                       0601YC98
c      DO  I = 1, NMOD                                                  0601YC98
c         FREQ(I+ISHFT) =  EFNTP(I)                                     0601YC98
c      ENDDO                                                            0601YC98
      IF (LDEBUG) THEN
         WRITE (IDBG,1600)
         L = 0
         DO 80 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1800) L,W(I)*AUTOCM,ZERO,ZERO,W(I)*AUTOCM
   80    CONTINUE
         L = 0
         DO 81 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1700) L,L,ZERO,ANCO(I,I)*AUTOCM,ZERO,
     *                        ANCO(I,I)*AUTOCM
   81    CONTINUE
         IF (NMOD.EQ.1) GO TO 84
         DO 82 I = NMOD, 2, -1
            L = NMOD - I + 1
            JSTART = I - 1
            DO 83 J = JSTART, 1, -1
               M = NMOD - J + 1
               WRITE (IDBG,1700) L,M,ZERO,(ANCO(I,J)-DUMMY(I,J))*AUTOCM,
     *                           DUMMY(I,J)*AUTOCM,ANCO(I,J)*AUTOCM
   83       CONTINUE
   82    CONTINUE
   84    WRITE (IDBG,1900) ZERO,(ENUT-EDUMMY)*AUTOCM,EDUMMY*AUTOCM,
     *                     ENUT*AUTOCM
         WRITE(IDBG,2000) EGRND*AUTOCM
         WRITE(IDBG,2100) (EFNTP(I)*AUTOCM,I=1,NMOD)
         WRITE (IDBG,2600)
         L = 0
         DO 280 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2700) L,L,L,6.0D0*C(I,I,I)*AUTOCM
  280    CONTINUE
         IF (NMOD.EQ.1) GO TO 286
         DO 281 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 282 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2700) L,M,M,2.0D0*C(I,J,J)*AUTOCM
  282       CONTINUE
  281    CONTINUE
         DO 283 I = 1, NMOD-2
            L = NMOD - I + 1
            JSTART = I + 1
            DO 284 J = JSTART, NMOD-1
               M = NMOD - J + 1
               KSTART = J + 1
               DO 285 K = KSTART, NMOD
                  N = NMOD - K + 1
                  WRITE (IDBG,2700) L,M,N,C(I,J,K)*AUTOCM
  285          CONTINUE
  284       CONTINUE
  283    CONTINUE
  286    CONTINUE
         WRITE (IDBG,2800)
         L = 0
         DO 380 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2900) L,L,L,L,24.0D0*Q(I,I)*AUTOCM
  380    CONTINUE
         IF (NMOD.EQ.1) GO TO 383
         DO 381 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 382 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2900) L,L,M,M,4.0D0*Q(I,J)*AUTOCM
  382       CONTINUE
  381    CONTINUE
  383    CONTINUE
      ENDIF
      RETURN
1600  FORMAT(/,' Perturbation theory anharmonicity and coriolis effect
     *analysis',/,/,1X,'Spectroscopic',2X,'Harmonic',4X,'Anharmonic',4X,
     *'Coriolis',5X,'Total',/,' constant (cm**-1)',2X,'part',10X,
     *'part',9X,'part',/)
1700  FORMAT(1X,'X(',I2,',',I2,')',2X,4(1X,F12.3))
1800  FORMAT(1X,'W(',I2,')',5X,4(1X,F12.3))
1900  FORMAT(/,1X,'E-CONST',3X,4(1X,F12.3))
2000  FORMAT(/,1X,'The zero point energy is ',F9.3)
2100  FORMAT(1X,'The fundementals are ',/,(11X,4(1X,F12.3)))
2600  FORMAT(/,' Cubic potential constants in reduced normal',
     *         ' coordinates')
2700  FORMAT(1X,'F(',I2,',',I2,',',I2,')',3X,1P,E16.8)
2800  FORMAT(/,' Quartic potential constants in reduced normal',
     *         ' coordinates')
2900  FORMAT(1X,'F(',I2,',',I2,',',I2,',',I2,')',3X,1P,E16.8)
      END SUBROUTINE ancoef
***********************************************************************
C  ANGLV 
C***********************************************************************
C
      SUBROUTINE anglv (ASC,BSC,ALP,NEND)
      use perconparam
C
C THIS SUBPROGRAM CALCULATES THE ANGLE BETWEEN TWO VECTORS ASC AND BSC
C
C     CALLED BY:
C                DORODS,VALVAG
C 
C   PARAMETERS AND COMMON BLOCKS MODIFIED 11/11/96
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ASC(NEND),BSC(NEND)


C      WRITE (FU6,2100) (ASC(i),i=1,NEND)
C      WRITE (FU6,2100) (BSC(i),i=1,NEND)
      PRODUCTE=0.0
      XNORME=0.0
      XNORMG=0.0
      DO 10 I=1,NEND
         PRODUCTE=PRODUCTE+ASC(i)*BSC(i)
         XNORMG=XNORMG+ASC(i)*ASC(i)
         XNORME=XNORME+BSC(i)*BSC(i)
 10   CONTINUE
      IF (XNORMG.LT.1.D-16.OR.XNORME.LT.1.D-16) THEN
         WRITE (6,*) 'NORM ZERO'
         ALP=0.
         GOTO 100
      END IF
C      WRITE (FU6,3000) PRODUCTE, DSQRT(XNORMG), DSQRT(XNORME)
C      WRITE(FU6,*) 'ACOS( ',PRODUCTE/(DSQRT(XNORMG)*DSQRT(XNORME)),' )'
      DOTPROD = PRODUCTE/(DSQRT(XNORMG)*DSQRT(XNORME))                  0708PF97
      IF (DABS(DOTPROD).GT.1.0D0.AND.
     *        DABS(DOTPROD-1.0D0).LT.1.0D-10) THEN                      0708PF97
         WRITE(FU6,*) 'WARNING:  SET DOTPROD = 1.0'                     0708PF97
         DOTPROD = 1.0D0                                                0708PF97
      ENDIF                                                             0708PF97
      ALP=DACOS(DOTPROD)                                                0708PF97
C     WRITE (FU6,1000) ALP, DSQRT(XNORMG), DSQRT(XNORME)                0708PF97

 100  RETURN                                                             
C
1000  FORMAT ('ANGLE, NORMA, NORMB:',f8.4,2(1x,f8.4))                   0824JC00
2100  FORMAT (12f10.6)
3000  FORMAT ('PRODUCTE, NORMA, NORMB:',3(F10.7))
      END SUBROUTINE anglv
C
C***********************************************************************
C  ANHARM
C***********************************************************************
C
      SUBROUTINE anharm (IOP)
      use common_inc
      use perconparam
      use rate_const
C
C This subroutine was restrutured 30OCT85.  The sections of code to
C    compute the 3rd and 4th derivatives were moved to subroutine
C    ANHDER.   BCG
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C     FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 
C
C Computes anharmonicities for bound normal modes.
C    Anharmonicity selected by the value of LGS(5).
C       LGS(5) = 0, harmonic
C          all anharmonic terms set to zero.
C       LGS(5) = 1, Morse
C          Morse option selected by the value of LGS(15).
C          LGS(15) = 1, Morse I, xe computed from dissoc. energy DEI
C                       and the 2nd derivative. Sign of the 3rd
C                       derivative obtained from the actual derivative.
C          LGS(15) = 2, Morse III, xe = omega/4DE, DE = max[DEII,DEI].
C                       Sign of the 3rd derivative obtained from the
C                       actual derivative.
C          LGS(15) = 3, Morse II xe computed from the 2nd and 3rd
C                       derivatives. DEII = 4xe/omega. Sign of the 3rd
C                       derivative obtained from the actual derivative.
C          LGS(15) = 4, Morse Ia, xe from MI but the sign of the 3rd
C                       derivative obtained from the the hyperspherical
C                       radius.  This is the only case that the third
C                       derivative is not needed.
C          LGS(15) = 5, Morse IIIa, xe from MIII and sign of the 3rd
C                       derivative obtained from the actual derivative
C                       if MII used but from the hyperspherical radius
C                       if MI used.
C          ANRHM(I) = Morse anharmonicity (unitless)
C       LGS(5) = 2, Morse-quadratic-quartic
C          Morse option selected by the value of LGS(15) (see above).
C          Third derivative is always calculate to test if quartic
C             anharmonicity should be used. If xe is smaller than ANTLR,
C             quartic anharmonicity calculated and ANHRM(I)=-D4V.
C       LGS(5) = 3, Dunham-y  *****this option is removed
C          Dunham coefficients computed from third and fourth
C             derivatives. ANHRM(I) is reset.
C       LGS(5) = 4, Dunham-x  ***** this option is removed
C          Dunham coefficients computed from third and fourth
C             derivatives. ANHRM(I) is reset.
C       LGS(5) = 5, Pade      ***** this option is removed
C          Pade coefficients computed from third and fourth
C             derivatives. ANHRM(I) is reset.
C       LGS(5) = 6, Pitzer-Gwinn, uses Pade for lowest energy level.
C          Pade coefficients computed from third and fourth
C          derivatives. ANHRM(I) is reset.***** this option is removed
C       LGS(5) = 7, Quadratic-quartic fit to potential at two points,
C                   energy levels by primitive WKB.
C          Harmonic coefficient stored in ANHRM(I), and quartic
C             coefficient stored in AB(I).
C       LGS(5) = 8, Quadratic-quartic fit to potential at two points,
C                   energy levels by uniform semiclassical.
C          Harmonic coefficient stored in ANHRM(I), and quartic
C             coefficient stored in AB(I).
C       LGS(5) = 9, Hindered-rotator approximation for torsions         0408JZ10
C       LGS(33) = 1, WKB solution to zeropoint eigenvalue
C
C       LGS(5)=,>21,options for mode I selected by the value of MODE(I) 6/30YL91
C             MODE(I) = 9, hindered internal rotation approximation        ..
C             NARR=LGS(5)-20, No of ranges along the MEP which have        ..
C                             special assignments of anharmonicity      6/30YL91
C
C     IOP=-1 to -4 for reactants or products
C     IOP > 0 for transition state
C     IOP = 7, 8 for wells                                              0801PF97
C
C     CALLED BY:
C                RPHRD2,NORMOD
C     CALLS:
C            TRANS,SGND3V,ANHDER,QQPOT,WKBVIB,HINDRT,FIRST
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
      DIMENSION CFF(N3TM),DXOLD(N3TM),XOLD(N3TM)                        9/29WH92
      SAVE
      call dopnm_mem
      call anh_mem
C
C  Initialize arrays
      DO 10 I = 1, N6TM
         ANHRM(I) = 0.0D0
         Y00(I) = 0.0D0
         INTOUT(I) = 0
   10 CONTINUE
      DO 15 I = 1, N3TM                                                 6/30YL91
         FMOMHR(I) = 0.D0                                                  ..
   15 CONTINUE                                                          6/30YL91
C
C  Insure that 3rd derivative allowed
C
      IF (IOP.LE.0) THEN
C
C  Set up for reactants or products or wells                            0801PF97
C
         KOP = ABS(IOP)
         NEND = NDIM(KOP)
         IF (ICODE(KOP) .LT. 0) THEN                                    11/20T87
            ISHFT = 0                                                      ..
         ELSE IF (ICODE(KOP) .LT. 4) THEN                                  ..
            ISHFT = 5                                                      ..
         ELSE                                                              ..
            ISHFT = 6                                                      ..
         ENDIF                                                          11/20T87
         DO 17 I = 1, N3                                                9/6YL91
            DXOLD(I) = DX(I)                                               ..
            XOLD(I) = X(I)                                                 ..
   17    CONTINUE                                                       9/6YL91
C
C  Convert reactant or product or well geom to mass-scaled
C
         CALL TRANS (1,N3,AMASS,X,DX)                                   9/18YL92
      ELSE
C
C  Set up for generalized transition state
C
         NEND = N3
         IF (ICODE(5) .LT. 0) THEN                                      11/20T87
            ISHFT = 1                                                      ..
         ELSE IF (ICODE(5) .LT. 4) THEN                                    ..
            ISHFT = 6                                                      ..
         ELSE                                                              ..
            ISHFT = 7                                                      ..
         ENDIF                                                          11/20T87
      ENDIF
      NFREQ = NEND-ISHFT
C
C  Save V, X, R, and normalized DX
C
      VI = V
C
C  Dissociation energy for MI model
C
      DEI = DEMIN-VI
      IF (LGS(30).LE.0) THEN
         DO 20 I = 1, N3
            DXI(I) = DX(I)
            XI(I) = X(I)
   20    CONTINUE
C*
C  IN THIS SUBROUTINE IT IS ASSUMED THAT THE FIRST NCR2 VALUES OF
C  THE ARRAY POTINF ARE THE INTERNAL COORDINATES OF THE SYSTEM AT
C  THIS POSITION
C
        IF (LGS2(5) .GT. 0 .AND. NPOTPT .GE. NCR2) THEN                 0910GL91
           DO 30 I = 1, NCR2
              RI(I) = POTINF(I)                                         0910GL91
   30      CONTINUE
        ENDIF                                                           0910GL91
C
C  Get un-normed DX
C
c         call first(1)                                                  6/2RS94
         call ghook(1,iproc)                                                  0301YC97 
c
         DO 40 I = 1, N3
            VEC3(I) = DX(I)
   40    CONTINUE
      ENDIF
C
      AFLAG = '   '
      IF (LGS(5).GE.21) AFLAG = 'SET'                                   6/30YL91
C =====================================================================
C  Loop over normal modes
C
      DO 50 IFRQ = 1, NFREQ
         JFREQ = IFRQ+ISHFT
C        IF (AFLAG.EQ.'SET') LGS(5) = MODE(IFRQ)                        0924JC97
         IF (AFLAG.EQ.'SET'.AND.IOP.LE.0) LGS(5) =                      0924JC97
     *                              MODE(NF(ABS(IOP))+1-IFRQ)           0924JC97
         IF (AFLAG.EQ.'SET'.AND.IOP.GE.0) LGS(5) = MODE(IFRQ)           0924JC97
C
C ----------------------------------------------------------------------
C
         IF (LGS(5).EQ.0) THEN
C
C  Harmonic
C
            INTOUT(IFRQ) = 0
C
C ----------------------------------------------------------------------
C
         ELSEIF (LGS(5).EQ.1) THEN
C
C  Morse
C     Compute 3rd derivative for all cases except MIa.
C
            IF (LGS(15).NE.4) CALL ANHDER (IFRQ,JFREQ,NEND,D3V,D4V,
     *         RATIO,3)
            IF (LGS(15).NE.3) THEN
C
C     Morse I approximation; calculate XE
C
               ANI = FREQ(JFREQ)/(4.0D0*DEI)
               ANHRM(IFRQ) = ANI
               INTOUT(IFRQ) = 1
            ENDIF
            IF (LGS(15).EQ.2.OR.LGS(15).EQ.3.OR.LGS(15).EQ.5) THEN
C
C     Morse II
C        Compute 2nd deriv along bound mode from FREQ
C
               D2V = FREQ(JFREQ)**2*REDM
               DEII = 1.0D+30
               IF (ABS(D3V).GT.1.0D-10) DEII=9.0D0*D2V**3/
     *                                       (2.0D0*D3V**2)
               ANII = 0.0D0
               IF (DEII.GT.0.0D0) ANII = 0.25D0*FREQ(JFREQ)/DEII
               ANHRM(IFRQ) = ANII
               INTOUT(IFRQ) = 2
            ENDIF
            IF (LGS(15).EQ.2.OR.LGS(15).EQ.5) THEN
C
C     Morse III
C
               IF (DEII.GT.DEI) THEN
C
C        For DEII > DEI use MII
C
                  ANHRM(IFRQ) = ANII
                  INTOUT(IFRQ) = 2
               ELSE
C
C        For DEII <= DEI use MI
C
                  ANHRM(IFRQ) = ANI
                  INTOUT(IFRQ) = 1
               ENDIF
            ENDIF
            IF (IOP.LE.0.OR.IOP.EQ.2) THEN
C
C     Print out for reactants, products, wells, and saddle point
C        MI
C
               IF (LGS(15).EQ.1.OR.LGS(15).EQ.4) WRITE (FU6,1000) IFRQ,
     *            DEI
C
C        MII or MIII-->MII
C
               IF (((LGS(15).EQ.2.OR.LGS(15).EQ.5).AND.INTOUT(IFRQ)
     *            .EQ.2).OR.LGS(15).EQ.3) WRITE (FU6,1100) IFRQ,DEII
C
C        MIII --> MI
C
               IF ((LGS(15).EQ.2.OR.LGS(15).EQ.5).AND.INTOUT(IFRQ).EQ.1
     *            ) WRITE (FU6,1200) IFRQ,DEI,DEII
            ENDIF
            IDUMMY = INTOUT(IFRQ)                                       7/14YL92
            IF (LTUN) CALL SGND3V (D3V,IFRQ,JFREQ,IDUMMY)               7/14YL92
C
C ----------------------------------------------------------------------
C
         ELSEIF (LGS(5).EQ.2) THEN
C
C  Morse-quadratic-quartic
C     Calculate 3rd derivative
C
            CALL ANHDER (IFRQ,JFREQ,NEND,D3V,D4V,RATIO,3)
C
C     Morse II so xe can be tested
C        Compute 2nd deriv along bound mode from FREQ
C
            D2V = FREQ(JFREQ)**2*REDM
            DEII = 1.0D+30
            IF (ABS(D3V).GT.1.0D-10) DEII = 9.0D0*D2V**3/
     *                                      (2.0D0*D3V**2)
            ANII = 0.0D0
            IF (DEII.GT.0.0D0) ANII = 0.25D0*FREQ(JFREQ)/DEII
            INTTMP = 0
C
C     Check if Morse can be used or if quartic correction must be
C        computed
C
            IF (ANII.GE.ANTLR) THEN
               IF (LGS(15).NE.3) THEN
C
C     Morse I
C
                  ANI = 0.25D0*FREQ(JFREQ)/DEI
                  ANHRM(IFRQ) = ANI
                  INTTMP = 1
               ELSE
C
C     Morse II
C
                  ANHRM(IFRQ) = ANII
                  INTTMP = 2
               ENDIF
               IF (LGS(15).EQ.2.OR.LGS(15).EQ.5) THEN
C
C     Morse III
C
                  IF (DEII.GT.DEI) THEN
C
C        For DEII > DEI use MII
C
                     ANHRM(IFRQ) = ANII
                     INTTMP = 2
                  ELSE
C
C        For DEII <= DEI use MI
C
                     ANHRM(IFRQ) = ANI
                     INTTMP = 1
                  ENDIF
               ENDIF
            ENDIF
            IF (INTTMP.NE.0.AND.ANHRM(IFRQ).GE.ANTLR) THEN
               INTOUT(IFRQ) = INTTMP
               IF (IOP.LE.0.OR.IOP.EQ.2) THEN
C
C     Print out for reactants, products, wells and saddle point
C        MI
C
                  IF (LGS(15).EQ.1.OR.LGS(15).EQ.4) 
     *                      WRITE (FU6,1000) IFRQ,DEI
C
C        MII or MIII-->MII
C
                  IF (((LGS(15).EQ.2.OR.LGS(15).EQ.5).AND.INTOUT(IFRQ)
     *               .EQ.2).OR.LGS(15).EQ.3) WRITE (FU6,1100) IFRQ,DEII
C
C        MIII --> MI
C
                  IF ((LGS(15).EQ.2.OR.LGS(15).EQ.5).AND.INTOUT(IFRQ)
     *               .EQ.1) WRITE (FU6,1200) IFRQ,DEI,DEII
               ENDIF
               IDUMMY = INTOUT(IFRQ)                                    7/14YL92
               IF (LTUN) CALL SGND3V (D3V,IFRQ,JFREQ,IDUMMY)            7/14YL92
            ELSE
C
C  Third derivative is too small, quartic anharmonicity needed.
C     Calculate 4th derivative
C
               CALL ANHDER (IFRQ,JFREQ,NEND,D3V,D4V,RATIO,4)
               IF (D4V.LT.0.0D0) THEN
                  WRITE (FU6,1500) S,IFRQ,FREQ(JFREQ),D3V,D4V
                  STOP 'ANHARM 1'
               ENDIF
               ANHRM(IFRQ) = -D4V
               INTOUT(IFRQ) = 3
               IF (IOP.LT.0.OR.IOP.EQ.2) WRITE (FU6,1300) IFRQ,RATIO
            ENDIF
C
C ----------------------------------------------------------------------
C
         ELSEIF (LGS(5).EQ.7.OR.LGS(5).EQ.8) THEN
C
C  Compute quadratic-quartic parameters by two-point fit
C
            CALL QQPOT (IFRQ,JFREQ)
            INTOUT(IFRQ) = 20
C
         ELSEIF (LGS(5).EQ.9) THEN                                      6/30YL91
C                                                                          ..
C  Compute reduced moment of inertia with hindered rotor approximation     ..
C                                                                          ..
          IF (IOP.LT.0) THEN                                            0311YC98
            DO I = 1, N3TM                                              0311YC98
               CFF(I) = COF(I,ISHFT+NF(ABS(IOP))+1-IFRQ)                0311YC98
            ENDDO                                                       0311YC98
          ELSE                                                          0311YC98
            DO 45 I = 1, N3TM                                              ..
               CFF(I) = COF(I,JFREQ)                                    9/29WH92
45          CONTINUE                                                    0311YC98
          ENDIF                                                         0311YC98
          IF (IOP.LE.0) THEN                                            0317YC99
              IDTT = KOP                                                0317YC99
              IFRQQ = NF(KOP)+1-IFRQ                                    0521YC99
              JFREQQ = ISHFT+NF(ABS(IOP))+1-IFRQ                        0521YC99
          ELSE                                                          ..
              IDTT = 5                                                  ..
              IFRQQ = IFRQ                                              ..
              JFREQQ = JFREQ
          ENDIF                                                           
c
c 1:OW|2:RO|3:CO|4:RW|5:CW
c
          IF ((NTRSCH(IDTT,IFRQQ).EQ.2).or.                             0712YC99
     >        (NTRSCH(IDTT,IFRQQ).EQ.4).or.                             0712YC99
     >        (NTRSCH(IDTT,IFRQQ).EQ.6)) THEN                           1206BE05
                 CALL HINDRT(IOP,IFRQ,NEND,1,CFF,FMHR)                  0712YC99
          ELSEIF ((NTRSCH(IDTT,IFRQQ).EQ.3).or.                         0712YC99
     >            (NTRSCH(IDTT,IFRQQ).EQ.5).or.                         0712YC99
     >            (NTRSCH(IDTT,IFRQQ).EQ.7)) THEN                       1206BE05
                 CALL HINDRT1(IOP,IFRQ,NEND,1,FMHR)                     0712YC99
          ELSE                                                          0712YC99
                 FMHR = TORMI(IDTT,IFRQQ,1)                             0712YC99
          ENDIF                                                         0712YC99
          IF (IOP.LT.0.OR.IOP.GE.7) THEN                                0311YC98
            FMOMHR(IFRQ) = FMHR                                         0311YC98
            TORMI(IDTT,IFRQQ,1) = FMHR                                  1020BE06 
          ELSE                                                          0311YC98
            FMOMHR(JFREQ) = FMHR                                         ..
            TORMI(IDTT,IFRQQ,1) = FMHR                                  0109BE07
          ENDIF
          INTOUT(IFRQ) = 20                                             6/30YL91
         ENDIF
C
C ----------------------------------------------------------------------
C
C         IF (LGS(33).EQ.1) CALL WKBVIB (IFRQ,JFREQ,0)                  9/25BCG00
         IF (LGS(33).EQ.1) THEN                                         9/25BCG00
            JSWITC = 1                                                  9/25BCG00
            IF (S.GE.SWITC) JSWITC = 2                                  9/25BCG00
            ISTATE = 0                                                  9/25BCG00
            IF (LN3(JSWITC,IFRQ).GT.0) ISTATE = 1                       9/25BCG00
            CALL WKBVIB (IFRQ,JFREQ,ISTATE)                             9/25BCG00
         ENDIF                                                          9/25BCG00
   50 CONTINUE
C
C ======================================================================
C
      IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20                            6/30YL91
C
C  Restore V, X, R, and NORMED DX
C
      IF (IOP.LT.0) THEN                                                0225WH94
         DO 55 I = 1, N3                                                0225WH94
            DX(I) = DXOLD(I)                                            0225WH94
            X(I) = XOLD(I)                                              0225WH94
   55    CONTINUE                                                       0225WH94
      ELSE                                                              0325WH94
         IF (LGS(30).LE.0) THEN
            V = VI
            DO 60 I = 1, N3
               X(I) = XI(I)
               DX(I) = DXI(I)
   60       CONTINUE
         ENDIF
C 
         IF (LGS2(5) .GT. 0 .AND. NPOTPT .GE. NCR2) THEN                9/10GL91
            DO 70 I = 1, NCR2
            POTINF(I) = RI(I)                                           9/10GL91
   70      CONTINUE
         ENDIF                                                          9/10GL91
      ENDIF
C
      RETURN
C
 1000 FORMAT (/1X,'For bound mode ', I3, ' cubic anharmonicity',
     *   ' determined',
     */1X,'by 2nd derivative and DE =', 1PE15.7)
 1100 FORMAT (/1X, 'For bound mode ', I3, ' cubic anharmonicity',
     *   ' determined',
     */1X,'by 2nd and 3rd derivatives, computed DE =',1PE15.7)
 1200 FORMAT (/1X, 'For bound mode ', I3, ' cubic anharmonicity',
     *   ' modified',
     */1X,'to dissociate at',1PE15.7,' a.u. instead of', E15.7,' a.u.') 0618WH94
 1300 FORMAT (/1X,'For bound mode ', I3, ' quartic anharm has ratio =',
     *   1PE15.7)
 1400 FORMAT(/1X,'THIRD DERIVATIVES ARE NOT COMPUTED, SO ANHARMONICITY'
     */1X,'IS SET TO ZERO --- CHECK LGS(10)')
 1500 FORMAT (/1X,'For s=', F10.5, ', mode=',I5,',',
     *        /1X,'Freq.=',1PE15.7,'  D3V=', E15.7, ', D4V=',E15.7)
C
      END SUBROUTINE anharm
C
C***********************************************************************
C  ANHDER
C***********************************************************************
C
      SUBROUTINE anhder (IFRQ,JFREQ,NEND,D3VV,D4VV,RATIO,IFLAG)
      use common_inc
      use perconparam
      use rate_const, only : xk3,xk4
C
C  Compute the 3rd and 4th derivatives along the normal modes
C     IFLAG = 3 for 3rd derivative
C           = 4 for 4th derivative
C
C  This section of code was moved out of subroutine ANHARM on 30OCT85
C     to make code more modular.  BCG
C
c  Include statements were added 6/18/91
C
C  CALLED BY:
C             ANHARM
C  CALLS:
C        TRANS,FIRST
C

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*
C
      IF (IFLAG.EQ.3) THEN
         IF (LGS(30).LE.0) THEN
C
C  Compute 3rd deriv of V along bound mode from potential
C
            DO 10 I = 1, N3                                             1025YL91
               X(I) = XI(I)+DLX*COF(I,JFREQ)                            1025YL91
   10       CONTINUE
c    compute unnormalized first derivative
c            call first(1)                                               6/2RS94
            call ghook(1,iproc)                                               0301YC97
c
            DO 20 I = 1, N3
               VEC4(I) = DX(I)
   20       CONTINUE
            DO 30 I = 1, N3                                             1025YL91
               X(I) = XI(I)-DLX*COF(I,JFREQ)                            1025YL91
   30       CONTINUE
c
c    compute unnormalized first derivative
c            call first(1)                                               6/2RS94
            call ghook(1,iproc)                                               0301YC97
c
            DO 40 I = 1, N3
               VEC2(I) = DX(I)
   40       CONTINUE
            SUM = 0.0D0
            SUM2 = 0.0D0
            DO 50 I = 1, N3                                             1025YL91
               TERM = (VEC4(I)+VEC2(I)-2.0D0*VEC3(I))*COF(I,JFREQ)      1025YL91
               SUM = SUM+TERM
               SUM2 = SUM2+ABS(TERM)
   50       CONTINUE
            D3VV = SUM/DLX**2
            XK3(IFRQ) = D3VV
         ELSE
C
C  3rd deriv of V along bound mode obtained from RPH interpolation
C
            D3VV = XK3(IFRQ)
         ENDIF
      ELSEIF (IFLAG.EQ.4) THEN
         IF (LGS(30).LE.0) THEN
            D4VV = 0.0D0
            IF (LGS(10).GT.3) THEN
C
C  Compute 4th deriv of V along bound mode from potential
C
               DO 60 I = 1, N3                                          1025YL91
                  X(I) = XI(I)+2.0D0*DLX*COF(I,JFREQ)                   1025YL91
   60          CONTINUE
c
c    compute unnormalized first derivative
c               call first(1)                                            6/2RS94
               call ghook(1,iproc)                                            0301YC97
               DO 70 I = 1, N3
                  VEC5(I) = DX(I)
   70          CONTINUE
               DO 80 I = 1, N3                                          1025YL91
                  X(I) = XI(I)-2.0D0*DLX*COF(I,JFREQ)                   1025YL91
   80          CONTINUE
c
c    compute unnormalized first derivative
c               call first(1)                                            6/2RS94
               call ghook(1,iproc)                                            0301YC97
c
               DO 90 I = 1, N3
                  VEC1(I) = DX(I)
   90          CONTINUE
               SUM = 0.0D0
               SUM2 = 0.0D0
               DO 100 I = 1, N3                                         1025YL91
                  TERM = (VEC5(I)-2.0D0*VEC4(I)+2.0D0*VEC2(I)-
     *                    VEC1(I))*COF(I,JFREQ)                         1025YL91
                  SUM = SUM+TERM
                  SUM2 = SUM2+ABS(TERM)
  100          CONTINUE
               RATIO = SUM/SUM2
               D4VV = SUM/(2.0D0*DLX**3)
               XK4(IFRQ) = D4VV
            ENDIF
         ELSE
C
C  4th deriv of V along bound mode obtained from RPH interpolation
C
            D4VV = XK4(IFRQ)
            RATIO = 0.0D0
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE anhder
C
C***********************************************************************
C  BCALC
C***********************************************************************
C
      SUBROUTINE bcalc (SBX,DX0,DX1X,STEPX,LDEL,IFLAG,LCOUNT,BKAP)
      use common_inc
      use perconparam
      use rate_const, only : bcur
      use energetics_mod, only : fsp
C
C     CALCULATES MILLER-HANDY-ADAMS CURVATURE FACTORS BK,F AND KAPPA
C        For IFLAG = -1, evaluate B at the end of the grid using
C           one-sided differences, then compute effective masses
C           at the previous grid point
C        For IFLAG = 0, evaluate B, points one either side of the
C           save point are available.  One-sided differences or
C           quadratic fit are selected by LGS(29).
C        For IFLAG = 1, evaluate effective masses at the last grid
C           point, B that is needed was computed in last call.
C
C        For LGS(29) = 0, use one-sided differences for derivative of
C           gradient wrt s.
C        For LGS(29) = 1, use quadratic fit to obtain the derivative
C           of gradient wrt s.
C        For LGS(29) = 2, use hessian and gradient itself to calculate
C           B.
C
C         CALLED BY:
C                   PATH
C         CALLS:
C               QUADFT,MUBAR,MUCDSC
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C   MODIFICATIONS FOR CDSCSAG MADE 26/08/91 
C   FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LWRITE
      DIMENSION DX0(N3TM),DX1X(N3TM),DERGRD(N3TM),SBX(3),Y(3),Z(3)
      save                                                              0601YC98
C
C
      IF (ABS(LGS(9)).GE.2) THEN                                        6/14DL91
        LWRITE = .FALSE.                                                5/10DL90
        IF (LGS(4).NE.0) THEN                                           5/10DL90
          LWRITE = .TRUE.                                               5/10DL90
        ELSE IF (SOB.GT.SOE) THEN                                       5/10DL90
          IF (S.GE.SOE.AND.S.LE.SOB) LWRITE = .TRUE.                    5/10DL90
        ELSE                                                            5/10DL90
          IF (S.GE.SOB.AND.S.LE.SOE) LWRITE = .TRUE.                    5/10DL90
        END IF                                                          5/10DL90
      END IF                                                            5/10DL90
C
      ISHFT = N3-N3M7
      IEND = N3M7
C
C If using RPH interpolation B computed already and can skip over
C    this section of code
C
      IF (IFLAG .LE. 0) THEN
         IF (LGS(30) .LE. 0) THEN
C
C Compute deriv of -grad(v) w.r.t. s
C
            IF (IFLAG .EQ. 0 .AND. LGS(29) .EQ. 1) THEN
C
C    Use three-point fit of grad(v) vs. s
C
               DO 1 I = 1, N3
                  Y(1) = -DX(I)
                  Y(2) = -DX0(I)
                  Y(3) = -DX1X(I)
                  CALL QUADFT (SBX,Y,Z)
                  DERGRD(I) = Z(2)+2.0D0*Z(3)*SBX(2)
    1          CONTINUE
C
            ELSEIF (IFLAG .LE. 0 .AND. LGS(29) .EQ. 2) THEN             1/3/91VM
C
C    Use the eigenvector, hessian, and gradient to calculate B
C
               IF (IFLAG .EQ. -1) THEN                                  1/3/91VM
                  DO 3 I = 1, N3                                        1/3/91VM
                     DERGRD(I)  = 0.0D0                                 1/3/91VM
                  DO 3 J = 1, N3                                        1/3/91VM
                      DERGRD(I) = DERGRD(I) - FSP(I,J)*DX(J)/DXMAG      1/3/91VM
    3             CONTINUE                                              1/3/91VM
               ELSEIF (IFLAG .EQ. 0) THEN                               1/3/91VM
                  DO 4 I = 1, N3                                        1/3/91VM
                     DERGRD(I)  = 0.0D0                                 1/3/91VM
                  DO 4 J = 1, N3                                        1/3/91VM
                      DERGRD(I) = DERGRD(I) - FSP(I,J)*DX0(J)/DXMAGO    1/3/91VM
    4             CONTINUE                                              1/3/91VM
               ENDIF                                                    1/3/91VM
            ELSE
C
C    Use one-sided difference to calculate the derivative of gradient wrt s
C
               DO 10 I = 1, N3
                  DERGRD(I) = -(DX(I)-DX0(I))/(SBX(1)-SBX(2))
   10          CONTINUE
            ENDIF
C
C  Compute BK,F for bound modes
C
            DO 30 IB = 1, IEND
               SUM = 0.0D0
               DO 20 J = 1, N3
                  SUM = SUM+DERGRD(J)*COF(J,IB+ISHFT)
   20          CONTINUE
C
C Following statement corrects error in MILLER-HANDY-ADAMS
C
               BCURV(IB) = -SUM*SIGN(1.0D0,S)
   30       CONTINUE
         ENDIF
C
         IF ((LGS(9) .EQ. 1 .AND. LGS(4) .NE. 0) .OR. LCDSC) THEN       8/26YL91
C
C
C  Calculate curvature kappa for printing
C
            SUM = 0.0D0
            DO 40 I = 1, IEND
               SUM = SUM+BCURV(I)*BCURV(I)
   40       CONTINUE
            BKAP = SQRT(SUM)
            IF (LGS(4).NE.0 .OR.LWRITE)                                 8/26YL91
     *          WRITE (FU6,1000) BKAP,(IEND-I+1,BCURV(I),I=IEND,1,-1)   0613WH94
         ENDIF
      ENDIF
C
C  Compute the effective mass factors:
C              SCSAG has been removed form version 5.0 
C              For CDSCSAG - call MUCDSC
C
      IFLAG = MAX(IFLAG,0)
C     CALL MUBAR (N3M7,LDEL,IFLAG,LCOUNT)
      IF (LCDSC) CALL MUCDSC(N3M7,LDEL,IFLAG,LCOUNT,BKAP)               0826GL91
C
C                                                                       5/10DL90
C  STORE B AND TURNING POINTS FOR USE IN LCG CALCULATIONS, THE          5/10DL90
C  VALUE OF B AND TPS(3,I) ARE FOR THE CURRENT SAVE POINT (LSAVE)       5/10DL90
C                                                                       5/10DL90
c      IF (LLCG) THEN                                                    1118GL91
         DO 50 I = 1, IEND                                              5/10DL90
            BCUR(I,LSAVE) = BCURV(I)                                    5/10DL90
   50    CONTINUE                                                       5/10DL90
c      ENDIF                                                             1118GL91
C
      RETURN
C
 1000 FORMAT(/1X,'Curvature of the reaction path (kappa) and',
     1       /1X,'components of the curvature vector (BmF) :',/,
     2       /1X,'kappa = ',1P,E14.6,/,
     3       /1X,'mode        BmF',/,(I4,1X,1P,E16.6))
C
      END SUBROUTINE bcalc
C
C***********************************************************************
C  BCALC0
C***********************************************************************
C
      SUBROUTINE bcalc0(LGS,NSHLF,S,EFFMU,IMU)
      use perconparam
C
C     CALCULATES EFFMU, MUSC, A, B, AND C FOR S.P. BY INTERPOLATION
C     Include statement added 6/18/91
C
C     CALLED BY:
C               PATH
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION EFFMU(NSDM),S(NSDM),LGS(39)
C
C
      if (imu.eq.1) then                                                0327YC97
         IF (LGS(1).GT.0) THEN
            EFFMU(NSHLF) = 0.5D0*(EFFMU(NSHLF-1)+EFFMU(NSHLF+1))
         ENDIF
      ELSE if (imu.eq.3) then
C                                                                        6/13T89
C LINEAR INTERPOLATION FOR EFFMU AT THE VICINITY OF THE SADDLE POINT     6/13T89
C                                                                        6/13T89
         NS = NSHLF                                                      6/13T89
         SLOPE = (EFFMU(NS+2)-EFFMU(NS-2))/(S(NS+2)-S(NS-2))             6/13T89
         EFFMU(NS-1)= EFFMU(NS-2) + SLOPE*(S(NS-1)-S(NS-2))              6/13T89
         EFFMU(NS)  = EFFMU(NS-2) + SLOPE*(S(NS)-S(NS-2))                6/13T89
         EFFMU(NS+1)= EFFMU(NS-2) + SLOPE*(S(NS+1)-S(NS-2))              6/13T89
      else
         write (fu6,*) 'Fatal error with BCALC0'
         write (fu6,*) 'INTMU is either 1 or 3'
         stop 'bcalc0 1'      
      ENDIF                                                              6/13T89
      RETURN
      END subroutine bcalc0
C
C***********************************************************************
C  BOLTZ
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C
      SUBROUTINE boltz (BET,VM,PE,SUM,SUMNOT,ESV,EMAX,VMAXX,IT,JTUN,    0625TA02
     *                  NB,ENE0,ENRC,DEDNRC)
      use common_inc
      use perconparam, xxx=>nb
      use rate_const
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     PERFORMS DE INTEGRATION OF P(E)
C     THIS SUBROUTINE WAS ALTERED ON 2/7/85 TO COMPUTE THE PERCENTAGE
C     OF THE SUM FROM ENERGIES LOWER THAN E(MIN).
C
C     CALLED BY:
C               KAPVA
C     CALLS:
C               FIVPT
C
      DIMENSION PE(2,NSV),SUM(4),ESV(NSV)                               7/14YL92
      DIMENSION EX(5),FEX(5)
      DIMENSION PQ1(NSV),PQ2(NSV),PN1(0:NSV),PN2(0:NSV)                 0331WH94
      DIMENSION DEDNRC(0:MAXWKB),ENRC(0:MAXWKB)                         0331WH94
      CHARACTER*5 TNAME(5)                                              0708JC00
C
      save                                                              0601YC98
C     DATA TNAME /'MEPSAG','CD-SCSAG','LCG3','LCG4','muOMT'/            0708JC00
      DATA TNAME /'ZCT','SCT','LCG3','LCG4','muOMT'/                    0423TA02
C
      XN = 0.5D0*BET*VM
      DO 10 J = 1, 4
         SUM(J) = 0.0D0
   10 CONTINUE
      SUMNOT = 0.0D0                                                    0625TA02
      TSUM = 0.0D0
      FMAX = 0.0D0
      XINSEG = 1.0D0/DBLE(NSEG)
      IE = 0
      DO 30 ISEG = 1, NSEG
         CL = (2.0D0*DBLE(ISEG)-1.0D0)*XINSEG
         DO 30 N = 1, NQ12
            IE = IE+1
            T = EXP(XN*(2.0D0-CL-PT(N)*XINSEG))
C
C Find the energy on the grid at which P(E)*EXP(-beta*E) is maximum
C
            E = ESV(IE)
            FX = T*PE(2,IE)
            IF (FX.GT.FMAX) THEN
               FMAX = FX
               EMAX = E
               IEMX = IE
            ENDIF
C
C Check if this energy is below PEMIN
C
            IF (E.LT.PEMIN) TSUM = TSUM+T*WT(N,2)*PE(2,IE)
C
            IF (LGS2(12) .GE. 1) THEN                                   0126WH94
               T = -1.0D0/T                                             0126WH94
            ELSE                                                        0126WH94
               TNOT = -1.0D0/T                                          0625TA02
               T = T-1.0D0/T
            ENDIF                                                       0126WH94
            DO 20 J = 1, 2
               J0 = 2*(J-1)
               W = WT(N,J)*T
               IF (LGS2(12).LE.0) WNOT = WT(N,J)*TNOT                   0625TA02
               DO 20 K = 1, 2
                  II = K+J0
                  SUM(II) = SUM(II)+W*PE(K,IE)
                  IF (LGS2(12).LE.0.AND.II.EQ.4) THEN                   0625TA02
                     SUMNOT = SUMNOT + WNOT*PE(K,IE)                    0625TA02
                  ENDIF                                                 0625TA02
   20       CONTINUE
   30 CONTINUE
C
C     Note that the T value changed here
C
      T = XN*XINSEG
      TNOT = XN*XINSEG                                                  0625TA02
C
      DO 40 II = 1, 4
         SUM(II) = 1.0D0+T*SUM(II)
   40 CONTINUE
      TSUM = T * TSUM                                                   0127WH94
C
      IF (LGS2(12).LE.0) THEN                                           0625TA02
         SUMNOT = 1.0D0+TNOT*SUMNOT                                     0625TA02
      ELSEIF (LGS2(12).GE.1) THEN                                       0625TA02
         SUMNOT = SUM(4)                                                0625TA02
      ENDIF                                                             0625TA02
C
      NETOT = NSEG*NQ12
C
C     If quantized tunneling energy method is used (LGS2(12) > 0)
C     the above only calculated the tunneling contribution at
C     energy above VAD. The following adds the contribution from
C     the quantized states below VAD to the value calculated above.
C     TSUM is reset to zero since no contribution below VAD has been
C     calculated yet and PEMIN must be below VAD.
C                                                    Wei-Ping
C
      IF (LGS2(12) .GE. 1) THEN                                         0127WH94
         DO 45 I = 1, NETOT                                             ..
            PQ1(I) = PE(1,I)                                            ..
            PQ2(I) = PE(2,I)                                            ..
45       CONTINUE                                                       0127WH94
C                                                                       ..
         WR = WER(NF(1) + 1 - IWR)                                      0127WH94
C
C        TERM1 = BET * EXP(BET*VAD)                                     0127WH94
C  The line above was commented by Titus Albu.  0423TA02
C        IE = 1                                                         0522TA02
C  The initialization of IE should be done into the do loop after
C  the number of states.  (Titus Albu)
C
         IF (IT .EQ. 1) WRITE(FU6,500)                                  0522TA02
     *                  TNAME(JTUN),WR*AUTOCM,1.0D0/(BK*BET)            0522TA02
C
         TSUM = 0.0D0
         PEXMAX = 0.0D0                                                 0707WH94
         QEMAX  = 0.0D0                                                 0707WH94
         DO 100 I = 0, NB                                               0331WH94
            IE = 1                                                      0522TA02
            ENERGY = ENE0 + ENRC(I)                                     0609WH94
            IF (ENERGY .GT. ESV(NETOT)) THEN                            0215WH94
               IE = NETOT                                               ..
            ELSE                                                        ..
80             IE = IE + 1                                              ..
               IF (ESV(IE) .LE. ENERGY) GOTO 80                         ..
            ENDIF                                                       ..
C
C     Calculate the P(E) at En by linear interpolation
C
            ALF = (PQ1(IE)-PQ1(IE-1))/(ESV(IE)-ESV(IE-1))               ..
            BLF = PQ1(IE) - ALF * ESV(IE)                               ..
            PN1(I) = ALF * ENERGY + BLF                                 ..
            IF (PN1(I) .LT. 0.0D0) THEN                                 ..
               PN1(I) = PQ1(1)                                          ..
               WRITE(FU6,*) 'WARNING: P(En) < 0, ',                     ..
     *                      'INCREASE NQE OR NSEG'                      ..
            ENDIF                                                       ..
            IF (PN1(I) .GT. 0.51D0) THEN                                ..
               PN1(I) = PQ1(NETOT)                                      ..
               WRITE(FU6,*) 'WARNING:  P(En) > 0.5, ',                  ..
     *                      'INCREASE NQE OR NSEG'                      ..
            ENDIF                                                       0215WH94
C
C           CONBT = TERM1 * DEDNRC(I) * PN1(I) * EXP(-BET*ENERGY)       0331WH94
C  The line above was commented by Titus Albu.  0423TA02
            CONBT =DEDNRC(I)*PN1(I)*BET*EXP(BET*(VAD-ENERGY))           0423TA02
            SUM(1) = SUM(1) + CONBT                                     0215WH94
            SUM(3) = SUM(3) + CONBT                                     ..
C
            ALF = (PQ2(IE)-PQ2(IE-1))/(ESV(IE)-ESV(IE-1))               ..
            BLF = PQ2(IE) - ALF * ESV(IE)                               ..
            PN2(I) = ALF * ENERGY + BLF                                 ..
            IF (PN2(I) .LT. 0.0D0) THEN                                 ..
               PN2(I) = PQ2(1)                                          ..
               WRITE(FU6,*) 'WARNING: P(En) < 0, ',                     ..
     *                      'INCREASE NQE OR NSEG'                      ..
            ENDIF                                                       ..
            IF (PN2(I) .GT. 0.51D0) THEN                                ..
               PN2(I) = PQ2(NETOT)                                      ..
               WRITE(FU6,*) 'WARNING:  P(En) > 0.5, ',                  ..
     *                      'INCREASE NQE OR NSEG'                      ..
            ENDIF                                                       0215WH94
C
C           CONBT = TERM1 * DEDNRC(I) * PN2(I) * EXP(-BET*ENERGY)       0331WH94
C  The line above was commented by Titus Albu.  0423TA02
            CONBT =DEDNRC(I)*PN2(I)*BET*EXP(BET*(VAD-ENERGY))           0423TA02
            SUM(2) = SUM(2) + CONBT                                     0215WH94
            SUM(4) = SUM(4) + CONBT                                     ..
C
            TEMCON = PN2(I)*EXP(-ENERGY*BET)                            0707WH94
            IF (TEMCON .GT. PEXMAX) THEN                                0707WH94
               PEXMAX = TEMCON                                          0707WH94
               QEMAX = ENERGY                                           0707WH94
            ENDIF                                                       0707WH94
C
            IF (ENERGY .LT. PEMIN) TSUM = TSUM + CONBT                  0215WH94
            IF (IT .EQ. 1) WRITE(FU6,1000) I,ENERGY*CKCAL,              0401WH94
     *                           PN2(I),DEDNRC(I)/WR,CONBT              0522TA02
100      CONTINUE                                                       0127WH94
         IF (IT .EQ. 1) WRITE(FU6,1100) VAD*CKCAL,SUMNOT                0625TA02
      ENDIF
C
C Compute percent contribution from below PEMIN
C
      PER = 100.0D0*TSUM/SUM(4)                                         0127WH94
C
C Five point fit to find Emax
C
      IF (IEMX.GT.2.AND.IEMX.LT.NETOT-1) THEN
         J = IEMX-3
         DO 200 K = 1, 5
            J = J+1
            E = ESV(J)-VMAXX
            FEX(K) = EXP(-BET*E)*PE(2,J)
            EX(K) = E+VMAXX
  200    CONTINUE
         EMAX = EX(3)
         CALL FIVPT (1,1,EX,FEX,EMAX,FMAX)                              5/10DL90
      ENDIF
C
      IF (LGS2(12) .GT. 0) EMAX = QEMAX                                 0707WH94
      EMAX = EMAX*CKCAL
C
      RETURN
C
  500 FORMAT(2X,'Quantized Reactant State Tunneling; Method = ',        0522TA02
     *           A5,'; WR = ',F7.2,' cm**-1',/2X,71('-'),               0522TA02
     *      /3X,'n',4X,'En(kcal/mol)',5X,'P(En)',7X,'dEn/dn',           0522TA02
     *       3X,'Contribution to kappa at ',F6.1,' K',/)                0522TA02
 1000 FORMAT(1X,I3,3X,F10.4,3X,1P,E12.4,0P,2X,F8.4,' WR',12X,1PE10.4)   0522TA02
 1100 FORMAT(1X,'Above',1X,F10.4,40X,1PE10.4)                           0522TA02
C
      END subroutine boltz
C
C***********************************************************************
C BRENT
C***********************************************************************
C Given a function F, and given a bracketing triplet of abscissas AX,BX,
C and CX ( such that BX is between AX and CX, and F(BX) is less than both
C F(AX) and F(CX) . This routine isolates the minimum to a fractional
C precision of about TOL using BRENT's method.
C
C CALLED BY: LINMN
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      FUNCTION brent (AX,BX,CX,F,TOL,XMIN)
      use perconparam
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      SAVE D
C
      PARAMETER (ITMAX = 100,CGOLD = 0.381966D0,ZEPS = 1.0D-10)
      EXTERNAL F
C
      A = MIN(AX,CX)
      B = MAX(AX,CX)
      V = BX
      W = V
      X = V
      E = 0.0D0
      FX = F(X)
      FV = FX
      FW = FX
      DO 50 ITER = 1,ITMAX
         XM = 0.5D0*(A+B)
         TOL1 = TOL*ABS(X) + ZEPS
         TOL2 = 2.0D0*TOL1
         IF(ABS(X-XM).LE.(TOL2-0.5D0*(B-A))) GOTO  100
         IF(ABS(E).GT.TOL1) THEN
            R = (X-W)*(FX-FV)
            Q = (X-V)*(FX-FW)
            P = (X-V)*Q - (X-W)*R
            Q = 2.0D0*(Q-R)
            IF(Q.GT.0.0D0) P = -P
            Q = ABS(Q)
            ETEMP = E
            E = D
            IF(ABS(P).GE.ABS(0.5D0*Q*ETEMP).OR.P.LE.Q*(A-X).OR.
     *         P.GE.Q*(B-X)) GOTO 10
            D = P/Q
            U = X + D
            IF((U-A).LT.TOL2 .OR. (B-U).LT.TOL2) D = SIGN(TOL1,XM-X)
            GOTO 20
         ENDIF
 10      IF(X.GE.XM) THEN
            E = A-X
         ELSE
            E = B-X
         ENDIF
         D = CGOLD*E
 20      IF(ABS(D).GE.TOL1) THEN
            U = X+D
         ELSE
            U = X+SIGN(TOL1,D)
         ENDIF
         FU = F(U)
         IF(FU.LE.FX) THEN
            IF(U.GE.X) THEN
               A = X
            ELSE
               B = X
            ENDIF
            V = W
            FV = FW
            W = X
            FW = FX
            X = U
            FX = FU
         ELSE
            IF(U.LT.X) THEN
               A = U
            ELSE
               B = U
            ENDIF
            IF(FU.LE.FW .OR. W.EQ.X) THEN
               V = W
               FV = FW
               W = U
               FW = FU
            ELSEIF(FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
               V = U
               FV = FU
            ENDIF
         ENDIF
 50   CONTINUE
      WRITE(FU6,*)'BRENT EXCEED MAXIMUM OF 100 ITERATIONS'
 100  XMIN = X
      BRENT = FX
      RETURN
      END FUNCTION brent
C
C***********************************************************************
C  BRNULI
C***********************************************************************
C
      SUBROUTINE brnuli (N,B)
      use perconparam
C
C Calculates the Bernouli numbers
C
C     Include statement was added 6/18/91
C     CALLED BY:
C                 PHID
C     CALLS:
C                 ALFCT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION B(N)
      save                                                              0601YC98
C
C     B(I) CONTAINS THE 2I BERNOULI NUMBER,
C     I.E., B(1) IS B SUB 2 AS DEFINED IN ABRAM. AND STEGUN
C
      IF (N.GT.31) GO TO 40
      ALTPI = LOG(TPI)
      CALL ALFCT (N,B)
      B(1) = 1.0D0/6.0D0
      B(2) = -1.0D0/30.0D0
      SGN = -1.0D0
      DO 30 I = 3, N
         SGN = -SGN
         J = 2*I
         FJ = DBLE(J)
         T = 2.0D0*SGN*EXP(B(I)-FJ*ALTPI)
         SUM = 1.0D0
         DO 10 K = 2, 400
            FK = DBLE(K)
            TERM = FK**(-J)
            SUM = SUM+TERM
            IF (TERM/SUM.LT.1.0D-13) GO TO 20
   10    CONTINUE
         WRITE (FU6,1100) K
   20    B(I) = T*SUM
   30 CONTINUE
      RETURN
   40 WRITE (FU6,1000)
      STOP 'BRNULI 1'
C
 1000 FORMAT(' 31 IS THE LARGEST NUMBER OF BERNOULI NUMBERS THAT'
     *       ,' CAN BE COMPUTED ON THE VAX')
 1100 FORMAT(39H ***BRNULI, SERIES NOT CONVERGED AFTER ,I10,6H TERMS)
C
      END SUBROUTINE brnuli
C
C***********************************************************************
C  CENTER
C***********************************************************************
C
      SUBROUTINE center (IOP,IMPR)
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : gufac6,iunit6
      use kintcm
C
C     TRANSLATES CENTER OF MASS OF SYSTEM TO SADDLE POINT OR TO
C     C. O. M. OF A REACTANT OR PRODUCT SPECIES
C     IOP=1 OR 2 FOR REACTANTS, IOP=3 OR 4 FOR PRODUCTS
C     IOP=5 FOR SADDLE POINT
C     ALSO CALCULATES MOMEMT OF INERTIA OR PRODUCT OF ALL 3
C     FOR S. P. OR REACTANT OR PRODUCT
C
C     Include statements were added 6/18/91
C     FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C
C     CALLED BY:
C                RPHRD2,POLYAT,MAIN
C     CALLS:
C            MXLNEQ
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LDEBUG                                                    1106YL92
C*
      DIMENSION XCM(3),TENS(3,3,3),ISCR(3),SCR1(3),SCR2(3)              1106YL92
      real(8) :: rot(3,3)
C
      DATA TENS / 5*0.0D0,-1.0D0,0.0D0,1.0D0,3*0.0D0,1.0D0,3*0.0D0,
     *            -1.0D0,3*0.0D0,-1.0D0,0.0D0,1.0D0,5*0.0D0 /
C
C
      LDEBUG = .FALSE.                                                  1106YL92
      LPTBCR = LGS2(15)
      IBEG = 1
      IEND = NATOM
      IF (IOP.LT.5) IEND = NRATOM(IOP)
      TOTM = 0.D00
      DO 10 I = IBEG, IEND
         TOTM = TOTM+AMASS(3*IATOM(I))**2
   10 CONTINUE
C
      DO 40 K = 1, 3
         SUM = 0.D00
         DO 20 I = IBEG, IEND
            DSUM = X(3*(IATOM(I)-1)+K)*(AMASS(3*(IATOM(I)-1)+K)**2)
            sum = sum + dsum
   20    CONTINUE
         XCM(K) = SUM/TOTM
C
C         MOVE C. O. M. OF FULL SYSTEM TO C. O. M. OF SPECIES IOP
C
         DO 30 I = K, N3, 3
            X(I) = X(I)-XCM(K)
   30    CONTINUE
   40 CONTINUE
c
C     IF (IMPR.EQ.1) WRITE (FU6,1000) (XCM(K),K=1,3)                    1125JC97
      IF (IMPR.EQ.1.AND.IUNIT6.EQ.1)WRITE(FU6,1000)(XCM(K)/GUFAC6,K=1,3)0405JZ07
      IF (IMPR.EQ.1.AND.IUNIT6.EQ.0)WRITE(FU6,1002)(XCM(K)/GUFAC6,K=1,3)0405JZ07
C
C     COMPUTE  MOMENT OF INERTIA IN A.U.
C
      IF (ICODE(IOP).LE.3) THEN
C
C         LINEAR MOLECULE
C         ALIGN MOLECULE WITH X-AXIS FOR MORATE
C         BUT ALONG Z-AXIS FOR POLYRATE TESTRUNS
C         IOLIN =1,2,3 MEANS X,Y,Z AXIS.
C
         SUM = 0.0D0
         JV = 1
         T = ABS(X(1))+ABS(X(2))+ABS(X(3))
         IF (T.LT.1.0D-08) JV = 4
         XX1 = X(JV)
         X2 = X(JV+1)
         X3 = X(JV+2)
         DO 50 I = IBEG, IEND
            L = 3*IATOM(I)-2
            TX = (X(L)**2+X(L+1)**2+X(L+2)**2)
            IF (L.NE.JV) THEN
               DPXX = X(L)*XX1+X(L+1)*X2+X(L+2)*X3
               SV = SIGN(1.0D0,DPXX)
            ELSE
               SV = 1.0D0
            ENDIF                      
            IF (IOLIN(IOP).EQ.3) THEN                                   0507YC97
               X(L) = 0.0D0                                             0507YC97
               X(L+1) = 0.0D0                                           0507YC97
               X(L+2) = SQRT(TX)*SV                                     0507YC97
            ELSE IF (IOLIN(IOP).EQ.2) THEN                              0507YC97
               X(L) = 0.0D0                                             0507YC97
               X(L+1) = SQRT(TX)*SV                                     0507YC97
               X(L+2) = 0.0D0                                           0507YC97
            ELSE                                                        0507YC97
               X(L) = SQRT(TX)*SV                                       0507YC97
               X(L+1) = 0.0D0                                           0507YC97
               X(L+2) = 0.0D0                                           0507YC97
            ENDIF                                                       0507YC97
            SUM = SUM+TX*AMASS(L)**2
   50    CONTINUE
         XMOM = SUM
         FMOM(IOP) = REDM*SUM
      ELSE
C
C         NON-LINEAR MOLECULE -- FIND DET OF MOMENT OF INERTIA TENSOR
C         THIS = IA*IB*IC
C
         DO 60 I = 1, 3
            DO 60 J = 1, 3
               ROT(I,J) = 0.0D0
   60    CONTINUE
         DO 70 I = IBEG, IEND
            L = 3*IATOM(I)-2
            ROT(1,1) = ROT(1,1)+(X(L+1)**2+
     *                 X(L+2)**2)*AMASS(L)**2
            ROT(1,2) = ROT(1,2)-X(L)*X(L+1)*AMASS(L)**2
            ROT(1,3) = ROT(1,3)-X(L)*X(L+2)*AMASS(L)**2
            ROT(2,2) = ROT(2,2)+(X(L)**2+
     *                 X(L+2)**2)*AMASS(L)**2
            ROT(2,3) = ROT(2,3)-X(L+1)*X(L+2)*AMASS(L)**2
            ROT(3,3) = ROT(3,3)+(X(L)**2+
     *                 X(L+1)**2)*AMASS(L)**2
   70    CONTINUE
         ROT(2,1) = ROT(1,2)
         ROT(3,1) = ROT(1,3)
         ROT(3,2) = ROT(2,3)
         IF (LPTBCR.EQ.2.OR.LPTBCR.EQ.12) THEN                          1106YL92
            DO 210 I = 1, 3                                             1106YL92
               BEROT(I) = 0.D0                                          1106YL92
               DO 210 J = 1, 3                                          1106YL92
                  PVEC(I,J) = 0.D0                                      1106YL92
210         CONTINUE                                                    1106YL92
            CALL RSPDRV(3,3,ROT,BEROT,1,PVEC,SCR1,SCR2,IERR)            1106YL92
            FMOM(IOP) = BEROT(1)*BEROT(2)*BEROT(3)*REDM**3              0601YC98
C            FMOM(IOP) = FMOM(IOP)*BEROT(1)*BEROT(2)*BEROT(3)*REDM**3    1106YL92
            DO 220 I = 1, 3                                             1106YL92
               BEROT(I) = 1/(2.0D0*REDM*BEROT(I))                       1106YL92
220         CONTINUE                                                    1106YL92
            IF (LDEBUG) WRITE(6,219) ((PVEC(I,J),I=1,3),J=1,3)          1106YL92
         ELSE
            CALL MXLNEQ(ROT,3,3,DET,JRNK,EPS,ISCR,0,3)                  9/20DL90
            IF (JRNK .LT. 3) THEN
               WRITE(FU6,*) ' PROBLEM WITH MXLNEQ IN SUBROUTINE CENTER'
               STOP 'CENTER 1'
            ENDIF
            FMOM(IOP) = DET*REDM**3
         ENDIF                                                          1106YL92
      ENDIF
      IF (ICODE(IOP).LE.3.AND.IMPR.EQ.1) WRITE (FU6,1100) FMOM(IOP)     1125JC97
      IF (ICODE(IOP).GT.3.AND.IMPR.EQ.1) WRITE (FU6,1200) FMOM(IOP)     1125JC97
C                                                                       6/13T89
C  COMPUTE THE PROJECTION OPERATOR P FOR MORATE                         6/13T89
C                                                                       6/13T89
C      IF (LGS(35) .NE. 0 .OR. LGS2(2) .NE. 0) THEN               
c
c choice to project out Rotational and Translational modes
c for reactants and products - only for the nosupuermol case has the choice
c for wells - all cases has choice
c 
      
      IF(ISTATU(IOP).EQ.6) GOTO 200                                     0101JZ13
      IF ((isup.eq.1.and.abs(iop).lt.5.and.iproj(abs(iop)).ne.0)
     >.or.(abs(iop).gt.6.and.iproj(abs(iop)).ne.0)
     >.or.(abs(iop).eq.5.and.iproj(5).ne.0)) then                       0721YC99
         do ii = 1,3*IEND 
           ind(ii) = indx0(ii,iop)
         enddo
         DO 100 IP = IBEG,IEND                                          6/13T89
            INX = 3*(IP-1)                                              6/13T89
            DO 100 JP = 1,IP                                            6/13T89
               JNDX = 3*(JP-1)                                          6/13T89
               DO 90 IC = 1,3                                           6/13T89
                  JEND = 3                                              6/13T89
                  IF (JP.EQ.IP) JEND = IC                               6/13T89
                  DO 90 JC = 1,JEND                                     6/13T89
                     SUM = 0.0D0                                        6/13T89
C                                                                       6/13T89
C FOR NON-LINEAR CASE                                                   6/13T89
C                                                                       6/13T89
                     IF(ICODE(IOP).EQ.4) THEN                           6/13T89
                        DO 80 IA = 1,3                                  6/13T89
                           DO 80 IB = 1,3                               6/13T89
                              IF (TENS(IA,IB,IC)) 72,80,72              6/13T89
72                            DO 76 JA = 1,3                            6/13T89
                                 DO 76 JB = 1,3                         6/13T89
                                    IF (TENS(JA,JB,JC)) 74,76,74        6/13T89
74                                  SUM = SUM+TENS(IA,IB,IC)*TENS(JA,JB,6/13T89
     *                                   JC)*ROT(IA,JA)*X(IND(INX+IB))* 6/13T89
     *                                   X(IND(JNDX+JB))*AMASS(IND(INX+ 6/13T89
     *                                   IB))*AMASS(IND(JNDX+JB))       6/13T89
76                               CONTINUE                               6/13T89
80                      CONTINUE                                        6/13T89
                     ENDIF                                              6/13T89
                     II = INX + IC                                      6/13T89
                     JJ = JNDX + JC                                     6/13T89
                     IF (ICODE(IOP) .EQ. 4) THEN                        6/13T89
                        PROJ(II,JJ) = SUM                               6/13T89
C                                                                       6/13T89
C FOR LINEAR CASE                                                       6/13T89
C                                                                       6/13T89
                     ELSEIF (ICODE(IOP).EQ.3 .OR. ICODE(IOP).EQ.2) THEN 6/13T89
                        IF (IC.EQ.1 .AND. JC.EQ.1 .OR. IC.NE.JC) THEN   6/13T89
                           PROJ(II,JJ) = 0.0D0                          6/13T89
                        ELSEIF(IC.NE.1 .AND. IC.EQ.JC) THEN             6/13T89
                           PROJ(II,JJ)=X(IND(INX+1))*
     *                             X(IND(JNDX+1))*AMASS 
     *                             (IND(INX+1))*AMASS(IND(JNDX+1))/     6/13T89
     *                             XMOM                                 6/13T89
                        ENDIF                                           6/13T89
                     ENDIF                                              6/13T89
C                                                                       6/13T89
C TRANSLATIONAL MOTIONS                                                 6/13T89
C                                                                       6/13T89
                     IF(IC.EQ.JC) THEN                                  6/13T89
                        PROJ(II,JJ)=PROJ(II,JJ)+AMASS(IND(II))*
     *                            AMASS(IND(JJ))/TOTM
                     ENDIF                                              6/13T89
90             CONTINUE                                                 6/13T89
100      CONTINUE                                                       6/13T89
         NEND = NDIM(IOP) 
         DO 110 I = 1,NEND                                              6/13T89
            DO 110 J = 1,I                                              6/13T89
               PROJ(I,J) = -PROJ(I,J)                                   6/13T89
               IF (I.EQ.J) PROJ(I,J) = 1.0D0 + PROJ(I,J)                6/13T89
110      CONTINUE                                                       6/13T89
         DO 120 I = 1,NEND                                              6/13T89
            DO 120 J = 1,I                                              6/13T89
               PROJ(J,I) = PROJ(I,J)                                    6/13T89
120      CONTINUE                                                       6/13T89
      ENDIF                                                             6/13T89
200   CONTINUE                                     
      RETURN
C
  219 FORMAT(' corresponding eigenvectors:', 3(/,5X,3F15.10))           1106YL92
 1000 FORMAT(/1X,'New center of mass is at ',3(1PE15.6),                0610WH94
     1       /1X,'bohrs with respect to previous origin.')
 1002 FORMAT(/1X,'New center of mass is at ',3(1PE15.6),                0405JZ07
     1       /1X,'angstroms with respect to previous origin.')
 1100 FORMAT(1X,'Moment of inertia (a.u.) = ',1PE14.6)
 1200 FORMAT(1X,'Products of three principal moments of inertia ',      1110DL89
     1'(a.u.) = ',1PE14.6)                                              0610WH94
C
      END SUBROUTINE center
C
C*********************************************
C  CHKFRE
C*********************************************
C
      SUBROUTINE chkfre (NMOD,FREQ,IFDMY)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   Called by:
C          NORMOD
C
      DIMENSION FREQ(NMOD)
C
      CMTOAU = 4.556335D-6
      IFDMY = 0
      DO 100 I = 1, NMOD
c        IF (FREQ(I).LE.0) IFDMY = IFDMY + 1                            0601YC98
        IF (FREQ(I).LE.150.0d0*CMTOAU) IFDMY = IFDMY + 1                0601YC98
100   CONTINUE
      RETURN
      END SUBROUTINE chkfre
C
C*********************************************
C  CHKUP
C*********************************************
C
      FUNCTION chkup (I,J,K,N3TM,C)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   Called by:
C          ANCOEF
C
      DIMENSION C(N3TM,N3TM,N3TM)
C
      II=I
      JJ=J
      KK=K
      IF(II.LE.JJ) GO TO 25
      III=II
      II=JJ
      JJ=III
   25 IF(JJ.LE.KK) GO TO 35
      JJJ=JJ
      JJ=KK
      KK=JJJ
      IF(II.LE.JJ) GO TO 35
      III=II
      II=JJ
      JJ=III
   35 CHKUP=C(II,JJ,KK)
      RETURN
      END FUNCTION chkup
C
C***********************************************************************
C  CLASS
C***********************************************************************
C
      SUBROUTINE class (KIND,N,ALPHA,BETA,B,A,MUZERO)
      use perconparam
C
C           THIS PROCEDURE SUPPLIES THE COEFFICIENTS A(J), B(J) OF THE
C        RECURRENCE RELATION
C
C             B P (X) = (X - A ) P   (X) - B   P   (X)
C              J J            J   J-1       J-1 J-2
C
C        FOR THE VARIOUS CLASSICAL (NORMALIZED) ORTHOGONAL POLYNOMIALS,
C        AND THE ZERO-TH MOMENT
C
C             MUZERO = INTEGRAL W(X) DX
C
C        OF THE GIVEN POLYNOMIAL   WEIGHT FUNCTION W(X).  SINCE THE
C        POLYNOMIALS ARE ORTHONORMALIZED, THE TRIDIAGONAL MATRIX IS
C        GUARANTEED TO BE SYMMETRIC.
C
C           THE INPUT PARAMETER ALPHA IS USED ONLY FOR LAGUERRE AND
C        JACOBI POLYNOMIALS, AND THE PARAMETER BETA IS USED ONLY FOR
C        JACOBI POLYNOMIALS.  THE LAGUERRE AND JACOBI POLYNOMIALS
C        REQUIRE THE GAMMA FUNCTION.
C
C     INCLUDE statement was added 6/18/91
C
C     CALLED BY:
C                 GAUSSQ
C     CALLS:
C                 DGAMMA
C     ..................................................................
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION MUZERO
      DIMENSION A(N),B(N)
      EXTERNAL DGAMMA                                                   23/10/90VM
C
      NM1 = N-1
      GO TO (10,30,50,70,90,110), KIND
C
C              KIND = 1=  LEGENDRE POLYNOMIALS P(X)
C              ON (-1, +1), W(X) = 1.0D0
C
   10 MUZERO = 2.0D0
      DO 20 I = 1, NM1
         A(I) = 0.0D0
         ABI = DBLE(I)
         B(I) = ABI/SQRT(4.0D0*ABI*ABI-1.0D0)
   20 CONTINUE
      A(N) = 0.0D0
      RETURN
C
C              KIND = 2=  CHEBYSHEV POLYNOMIALS OF THE FIRST KIND T(X)
C              ON (-1, +1), W(X) = 1 / SQRT(1 - X*X)
C
   30 MUZERO = PI
      DO 40 I = 1, NM1
         A(I) = 0.0D0
         B(I) = 0.5D0
   40 CONTINUE
      B(1) = SQRT(0.5D0)
      A(N) = 0.0D0
      RETURN
C
C              KIND = 3=  CHEBYSHEV POLYNOMIALS OF THE SECOND KIND U(X)
C              ON (-1, +1), W(X) = SQRT(1 - X*X)
C
   50 MUZERO = PI/2.0D0
      DO 60 I = 1, NM1
         A(I) = 0.0D0
         B(I) = 0.5D0
   60 CONTINUE
      A(N) = 0.0D0
      RETURN
C
C              KIND = 4=  HERMITE POLYNOMIALS H(X) ON (-INFINITY,
C              +INFINITY), W(X) = EXP(-X**2)
C
   70 MUZERO = SQRT(PI)
      DO 80 I = 1, NM1
         A(I) = 0.0D0
         B(I) = SQRT(DBLE(I)/2.0D0)
   80 CONTINUE
      A(N) = 0.0D0
      RETURN
C
C              KIND = 5=  JACOBI POLYNOMIALS P(ALPHA, BETA)(X) ON
C              (-1, +1), W(X) = (1-X)**ALPHA + (1+X)**BETA, ALPHA AND
C              BETA GREATER THAN -1
C
   90 AB = ALPHA+BETA
      ABI = 2.0D0+AB
      MUZERO = 2.0D0**(AB+1.0D0)*DGAMMA(ALPHA+1.0D0)*DGAMMA(BETA+1.0D0)/
     *   DGAMMA(ABI)
      A(1) = (BETA-ALPHA)/ABI
      B(1) = SQRT(4.0D0*(1.0D0+ALPHA)*(1.0D0+BETA)/((ABI+1.0D0)*ABI*ABI)
     *   )
      A2B2 = BETA*BETA-ALPHA*ALPHA
      DO 100 I = 2, NM1
         ABI = 2.0D0*DBLE(I)+AB
         A(I) = A2B2/((ABI-2.0D0)*ABI)
         B(I) = SQRT(4.0D0*DBLE(I)*(DBLE(I)+ALPHA)*(DBLE(I)+BETA)*
     *          (DBLE(I)+AB)/((ABI*ABI-1.0D0)*ABI*ABI))
  100 CONTINUE
      ABI = 2.0D0*DBLE(N)+AB
      A(N) = A2B2/((ABI-2.0D0)*ABI)
      RETURN
C
C              KIND = 6=  LAGUERRE POLYNOMIALS L(ALPHA)(X) ON
C              (0, +INFINITY), W(X) = EXP(-X) * X**ALPHA, ALPHA GREATER
C              THAN -1.
C
  110 MUZERO = DGAMMA(ALPHA+1.0D0)
      DO 120 I = 1, NM1
         A(I) = 2.0D0*DBLE(I)-1.0D0+ALPHA
         B(I) = SQRT(DBLE(I)*(DBLE(I)+ALPHA))
  120 CONTINUE
      A(N) = 2.0D0*DBLE(N)-1.0D0+ALPHA
      RETURN
      END SUBROUTINE class
C
C**********************************************************************
C  COG
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION cog(A,B,C,S0,SMEP)
C
C     CALLED BY:
C               ZOCVCL, ZOCFRE
C
C     THIS FUNCTION RETURNS THE VALUE OF A CUT-OFF GAUSSIAN AT SMEP
C     B MUST BE POSITIVE!
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER( EXPLIM = 600.0D0 )                                     09/95KAN
C
      IF ( ABS(SMEP) .GE. ABS(S0) ) THEN
         COG = C
      ELSE
         X  = -B / (1.0D0 - (SMEP/S0) ** 2.0D0)
         IF (X .LT. -EXPLIM) THEN
            COG = C
         ELSE
            COG = A * EXP(X) + C
         ENDIF
      ENDIF     
C
      RETURN
C
      END function cog
C
C**********************************************************************
C  COHBT1
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION cohbt1(A,C,S0,RANGE,S1,SMEP)
C
C     CALLED BY:
C               ZOCFRE
C
C     THIS FUNCTION RETURNS THE VALUE OF AN CUT-OFF HYPERBOLIC TANGENT AT SMEP
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER( EXPLIM = 600.0D0 )                                     09/95KAN
C
      IF (S1 .GT. 0.0D0) THEN
C
C     For the product-side correction function, S1=SP
C
         IF ( SMEP .GE. S1) THEN 
            COHBT1 = A + C
         ELSE
            FACTOR = (SMEP-S0)*S1/RANGE/(S1-SMEP)
            IF (ABS(FACTOR) .GE. EXPLIM) THEN
               COHBT1 = SIGN(A,FACTOR) + C
            ELSE
               COHBT1 = A * TANH(FACTOR) + C
            ENDIF
         ENDIF
C
      ELSEIF (S1 .LT. 0.0D0) THEN
C
C     For the reactant-side correction function, S1=SR
C
         IF ( SMEP .LE. S1) THEN 
            COHBT1 = -A + C
         ELSE
            FACTOR = -(SMEP-S0)*S1/RANGE/(SMEP-S1)
            IF ( ABS(FACTOR) .GE. EXPLIM) THEN
               COHBT1 = SIGN(A,FACTOR) + C
            ELSE
               COHBT1 = A * TANH(FACTOR) + C
            ENDIF
         ENDIF
      ENDIF 
C
      RETURN
C
      END function cohbt1
C
C**********************************************************************
C  COHBT2
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION cohbt2(A,C,S0,RANGE,S1,S2,SMEP)
C
C     CALLED BY:
C               ZOCFRE
C
C     THIS FUNCTION RETURNS THE VALUE OF AN CUT-OFF HYPERBOLIC TANGENT AT SMEP
C     NOTE: S1 < S0 < S2, RANGE > 0, S1 < 0, S2 > 0, S1 < SMEP < S2
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER( EXPLIM = 600.0D0 )                                     09/95KAN
C
      IF (SMEP .LE. S1) THEN
         COHBT2 = -A + C
      ELSE IF (SMEP .GE. S2) THEN
         COHBT2 =  A + C
      ELSE
         FACTOR = (SMEP-S0)*ABS(S1)*S2/RANGE/(S2-SMEP)/(SMEP-S1)
         IF (ABS(FACTOR) .GE. EXPLIM) THEN
            COHBT2 = SIGN(A,FACTOR) + C
         ELSE
            COHBT2 = A * TANH(FACTOR) + C
         ENDIF
      ENDIF
C
      RETURN
C
      END function cohbt2
C***********************************************************************
C  COLSHF   
C***********************************************************************
C
      SUBROUTINE colshf (NCOLX)
      use common_inc
      use perconparam, only : nsdm,n3m7
      use rate_const
      use cm ; use sst
C
C     SHIFTS RESULTS IN STORAGE ARRAYS FOR PATH
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C     MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
C   The include file esp.inc has been removed in version 5.0            1021GL92
C   The information for the bond orders and charges computed            1021GL92
C   if LGS(35)=1 has been removed in version 5.0.                       1021GL92
C
C     CALLED BY:
C               PATH
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      save
C
      LSHFT = NSDM-NCOLX
      DO 30 IC = 1, NCOLX
         ICL = IC + LSHFT
C*
         SSUBI(IC)  = SSUBI(ICL)
         VCLAS(IC)  = VCLAS(ICL)
         FMITS(IC)  = FMITS(ICL)
         VADIB(IC)  = VADIB(ICL)
         EGRND(IC)  = EGRND(ICL)                                        1106YL92
         CDSCMU(IC) = CDSCMU(ICL)                                       8/26YL91
         ZOCMCD(IC) = ZOCMCD(ICL)                                       10/7WH92
         SBKAP(IC) = SBKAP(ICL)                                         0812YC97
C         XLCDSC(IC) = XLCDSC(ICL)                                      0601YC98
C         XHCDSC(IC) = XHCDSC(ICL)                                      0601YC98 
         DO 10 JR = 1, N3M7
c           IF (LLCG) THEN                                              1118GL91
            BCUR(JR,IC) = BCUR(JR,ICL)                                  5/10DL90
c           ENDIF                                                       1118GL91
            WETS(JR,IC) = WETS(JR,ICL)
            Y0TS(JR,IC) = Y0TS(JR,ICL)
            EWKB0(JR,IC) = EWKB0(JR,ICL)
            XETS(JR,IC) = XETS(JR,ICL)
            FMIHTS(JR,IC) = FMIHTS(JR,ICL)                              6/30YL91
            EFNDT(JR,IC) = EFNDT(JR,ICL)                                1106YL92
   10    CONTINUE
         DO 20 JR = 1, N3
            GEOM(JR,IC) = GEOM(JR,ICL)
   20    CONTINUE                                                        
            IF (LLCG) THEN                                              10/28/GL91
                DO 21 JR = 1, N3                                        10/28/GL91
                   DXSV(JR,IC) = DXSV(JR,ICL)                           10/28/GL91
                DO 21 KR = 1,N3                                         10/28/GL91
                   COFSV(JR,KR,IC) = COFSV(JR,KR,ICL)                   5/10DL90
   21           CONTINUE                                                10/28/GL91
            ENDIF                                                       10/28/GL91
       IF(LSST.EQ.1) THEN                                               01/01/JZ13
         DO 23 JR = 1, NTOR
            TORBH(JR,IC) = TORBH(JR,ICL)
   23    CONTINUE
C        DO 24 JR =2, N3M7-NTOR+1
         DO 24 JR =1, N3M7
            DBW(JR,IC) = DBW(JR,ICL)
   24    CONTINUE
         DETDS(IC)= DETDS(ICL)
       ENDIF
C
   30 CONTINUE                                                           6/13T89
C
C       Symmetrization of reaction path data
C
      IF (LGS(3) .LT. 0) THEN
         LTIMES = NCOLX - 1
         DO 50 IC = 1, LTIMES
            ICP = NCOLX + IC
            ICM = NCOLX - IC
            SSUBI(ICP)  = -SSUBI(ICM)
            VCLAS(ICP)  = VCLAS(ICM)
            FMITS(ICP)  = FMITS(ICM)
            VADIB(ICP)  = VADIB(ICM)
            EGRND(ICP)  = EGRND(ICM)                                    1106YL92
            CDSCMU(ICP) = CDSCMU(ICM)                                   8/26YL91
            ZOCMCD(ICP) = ZOCMCD(ICM)                                   10/7WH92
            SBKAP(ICP) = SBKAP(ICM)                                     0812YC97
c            XLCDSC(ICP) = XLCDSC(ICM)
c            XHCDSC(ICP) = XHCDSC(ICM)
            DO 40 JR = 1, N3M7
               WETS(JR,ICP) = WETS (JR,ICM)
               Y0TS(JR,ICP) = Y0TS (JR,ICM)
               EWKB0(JR,ICP) = EWKB0(JR,ICM)
               XETS(JR,ICP) = XETS(JR,ICM)
               FMIHTS(JR,ICP) = FMIHTS(JR,ICM)                          6/30YL91
               EFNDT(JR,ICP) = EFNDT(JR,ICM)                            1106YL92
   40       CONTINUE
   50    CONTINUE
      ENDIF 
C
      RETURN 
      END SUBROUTINE colshf
C
C*************************************************************************
C CORTRM
C*************************************************************************
C
      SUBROUTINE cortrm (NMODE,NEND,N3,COF,PVEC,ZETA)
      use perconparam
C
C  This subroutine evaluates the coriolis constants
C  
C  Called by:
C       NORMOD
C
C  On input:
C     NMODE: total number of vibrational modes
C     NEND:  3 * (total number of atoms in the species)
C     N3:    total number of atoms in the reaction
C     N3TM:  3 * ( maximum number of atoms [i.e., NATOMS])
C     COF:   normal mode eigenvectors
C     PVEC:  eigenvectors of the inertia tensor
C
C  On output:
C     ZETA:  the coriolis constants
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION COF(N3TM,N3TM),PVEC(3,3),ZETA(N3TM,N3TM,3)
      DIMENSION PCOF(N3TM,N3TM)
      LOGICAL LDEBUG
C
C     First transform eigenvector coef's to principal axes
C
      LDEBUG = .FALSE.
      IDBG = 6
      ISHFT=NEND-NMODE
      DO 508 J=1,NEND
         KK=1
         NATOMX=N3/3
         DO 505 K=1,NATOMX
            PCOF(KK,J)=PVEC(1,1)*COF(KK,J)+PVEC(2,1)*COF(KK+1,J)
     *                 +PVEC(3,1)*COF(KK+2,J)
            PCOF(KK+1,J)=PVEC(1,2)*COF(KK,J)+PVEC(2,2)*COF(KK+1,J)
     *                 +PVEC(3,2)*COF(KK+2,J)
            PCOF(KK+2,J)=PVEC(1,3)*COF(KK,J)+PVEC(2,3)*COF(KK+1,J)
     *                 +PVEC(3,3)*COF(KK+2,J)
            KK=KK+3
  505    CONTINUE
  508 CONTINUE
      DO 510 I=1,NMODE
         DO 510 J=1,NMODE
            DO 510 K=1,3
               ZETA(I,J,K)=0.D0
  510 CONTINUE
C
      DO 540 I=2,NMODE
         IM1=I-1
         DO 530 J=1,IM1
            KK=1
            DO 520 IA=1,NATOMX
               ZETA(I,J,1)=ZETA(I,J,1)
     *                    +PCOF(KK+1,I+ISHFT)*PCOF(KK+2,J+ISHFT)
     *                    -PCOF(KK+2,I+ISHFT)*PCOF(KK+1,J+ISHFT)
               ZETA(I,J,2)=ZETA(I,J,2)
     *                    +PCOF(KK,I+ISHFT)*PCOF(KK+2,J+ISHFT)
     *                    -PCOF(KK+2,I+ISHFT)*PCOF(KK,J+ISHFT)
               ZETA(I,J,3)=ZETA(I,J,3)
     *                    +PCOF(KK,I+ISHFT)*PCOF(KK+1,J+ISHFT)
     *                    -PCOF(KK+1,I+ISHFT)*PCOF(KK,J+ISHFT)
               KK=KK+3
  520       CONTINUE
            DO 525 K=1,3
               ZETA(J,I,K)=-ZETA(I,J,K)
  525       CONTINUE
  530    CONTINUE
  540 CONTINUE
C
      IF (LDEBUG) THEN
         WRITE(IDBG,549)
         DO 585 K=1,3
            WRITE(IDBG,559)
            DO 575 I=1,NMODE
               WRITE(IDBG,569) (ZETA(I,J,K),J=1,NMODE)
  575       CONTINUE
  585    CONTINUE
      ENDIF
C
      RETURN
C
  549 FORMAT(' ZETA COUPLING CONSTANTS:')
  559 FORMAT(1X)
  569 FORMAT(1X,12F10.4,/3X,12F10.4)
      END SUBROUTINE cortrm
C
C***********************************************************************
C  CUBIC
C***********************************************************************
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      SUBROUTINE cubic (AA,BB,CC,DD,X0,ANS)
      use perconparam
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      save
C
C
C     FIND REAL ROOT OF AX3+BX2+CX+D=0 CLOSEST TO XO
C
C     CALLED BY:
C               FIVPT
C     CALLS:
C               CUBIC2
C
      DIMENSION ROOT(3),AIRT(3)
C
      CALL CUBIC2 (AA,BB,CC,DD,NREAL,ROOT,AIRT)
C
      IF (NREAL.EQ.0) THEN
         WRITE (FU6,1000)
         ANS = X0
      ELSE
         ANS = ROOT(1)
         IF (X0.NE.-999.D0.AND.NREAL.GT.1) THEN
C
C     FIND ROOT CLOSEST TO X0
C
            D = ABS(X0-ROOT(1))
            DO 10 I = 2, NREAL
               DTEST = ABS(X0-ROOT(I))
               IF (DTEST.LT.D) THEN
                  ANS = ROOT(I)
                  D = DTEST
               ENDIF
   10       CONTINUE
         ELSEIF (NREAL.GT.1) THEN
C
C     ANS DETERMINED BY ORDERING ROOTS.
C        ORDERING ALREADY DONE IN CUBIC2
C
            ANS = ROOT(2)
         ENDIF
      ENDIF
      RETURN
C
 1000 FORMAT (' CUBIC CALLED WITH A=B=C=0, NO ROOTS FOUND')
C
      END SUBROUTINE cubic
C
C***********************************************************************
C  CUBIC2
C***********************************************************************
C
      SUBROUTINE cubic2 (A,B,C,D,NREAL,RRT,AIRT)
C
C       SOLVE CUBIC EQUATION
C       A*X**3 + B*X**2 + C*X + D = 0
C
C       NREAL - NUMBER OF REAL ROOTS
C       RRT(1-3) - REAL PARTS OF THE 3 ROOTS
C       AIRT(1-3) - IMAG. PARTS OF THE 3 ROOTS
C          ROOTS ORDERED PURELY REAL ROOTS FOLLOWED BY COMPLEX ONES.
C          REAL ROOTS ARE IN INCREASING ORDER
C          COMPLEX ROOTS ARE IN INCREASING ORDER OF THE IMAG. PARTS
C
C       CALLED BY: CUBIC, PSATP2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RRT(3),AIRT(3)
      LOGICAL LFIRST
      DATA LFIRST / .TRUE. /
      save                                                              0601YC98
C
      IF (LFIRST) THEN
         C3 = 2.0D0*((1.0D0/6.0D0)-1.0D-11)
         RT3 = SQRT(3.0D0)
         LFIRST = .FALSE.
      ENDIF
      IF (A.NE.0.0D0) THEN
         P = B/A
         Q = C/A
         R = D/A
         X0 = -C3*P
         P2 = P*P
         AA = Q-C3*P2
         BB = (2.0D0*P2-9.0D0*Q)*P/27.0D0+R
         IF (AA.NE.0.0D0) THEN
            IF (BB.NE.0.0D0) THEN
               BBH = 0.5D0*BB
               BBHS = BBH*BBH
               T2 = BBHS+(AA**3)/27.0D0
               IF (T2.GE.0.0D0) THEN
C
C  CASE 1, T2 > 0.,  1 REAL ROOT
C
                  NREAL = 1
                  T = SQRT(T2)
                  A3 = -BBH+T
                  AP = SIGN(ABS(A3)**C3,A3)
                  A3 = -BBH-T
                  AM = SIGN(ABS(A3)**C3,A3)
                  X1 = AM+AP
                  X2 = -0.5D0*X1
                  Y = 0.5D0*RT3*(AP-AM)
                  RRT(1) = X1+X0
                  RRT(2) = X2+X0
                  RRT(3) = X2+X0
                  AIRT(1) = 0.0D0
                  AIRT(2) = -ABS(Y)
                  AIRT(3) = -AIRT(2)
               ELSE
C
C  CASE 2, T2 < 0.,  3 REAL ROOTS
C
                  NREAL = 3
                  T = SQRT(-T2)
                  TT = -BBH
                  THETA = C3*ATAN2(T,TT)
                  R = SQRT(BBHS-T2)
                  R3 = R**C3
                  CS = COS(THETA)
                  SN = SIN(THETA)
                  X1 = R3*CS
                  X2 = RT3*R3*SN
                  RRT(1) = 2.0D0*X1+X0
                  RRT(2) = -X1+X2+X0
                  RRT(3) = -X1-X2+X0
                  DO 10 I = 1, 3
                     AIRT(I) = 0.0D0
   10             CONTINUE
               ENDIF
            ELSE
C
C  CASE 3, BB=0
C
               RRT(1) = X0
               AIRT(1) = 0.0D0
               IF (AA.GT.0.0D0) THEN
C
C     FOR BB=0, AA>0,  1 REAL ROOT
C
                  NREAL = 1
                  RRT(2) = X0
                  RRT(3) = X0
                  X1 = SQRT(AA)
                  AIRT(2) = -X1
                  AIRT(3) = X1
               ELSE
C
C     FOR BB=0, AA<0,  3 REAL ROOTS
C
                  NREAL = 3
                  X1 = SQRT(-AA)
                  RRT(2) = X1+X0
                  RRT(3) = -X1+X0
                  AIRT(2) = 0.0D0
                  AIRT(3) = 0.0D0
               ENDIF
            ENDIF
         ELSE
C
C  CASE 4, AA = 0, 1 REAL ROOT
C
            T = -BB
            X1 = SIGN(ABS(T)**C3,T)
            NREAL = 1
            RRT(1) = X1+X0
            AIRT(1) = 0.0D0
            T = 0.5D0*X1
            RRT(2) = T+X0
            RRT(3) = T+X0
            AIRT(2) = -ABS(RT3*X1)
            AIRT(3) = -AIRT(2)
         ENDIF
      ELSEIF (B.NE.0.0D0) THEN
C
C  A = 0,  REDUCES TO QUADRATIC EQUATION
C
         AA = -0.5D0*C/B
         BB = D/B
         T2 = AA*AA-BB
         IF (T2.GE.0.0D0) THEN
C
C     T2 >= 0., 2 REAL, 0 COMPLEX
C
            NREAL = 2
            T = SQRT(T2)
            RRT(1) = AA+T
            RRT(2) = AA-T
            RRT(3) = 0.0D0
            DO 20 I = 1, 3
               AIRT(I) = 0.0D0
   20       CONTINUE
         ELSE
C
C     T2 <  0., 0 REAL ROOTS, 2 COMPLEX
C
            NREAL = 0
            T = SQRT(-T2)
            RRT(1) = AA
            RRT(2) = AA
            RRT(3) = 0.0D0
            AIRT(1) = -T
            AIRT(2) = T
            AIRT(3) = 0.0D0
         ENDIF
      ELSEIF (C.NE.0.0D0) THEN
C
C  A,B = 0,   REDUCES TO LINEAR EQUATION
C
         RRT(1) = -D/C
         AIRT(1) = 0.0D0
         DO 30 I = 2, 3
            RRT(I) = 0.0D0
            AIRT(I) = 0.0D0
   30    CONTINUE
         NREAL = 1
C
C  A,B,C = 0.,    NO ROOTS
C
      ELSE
         NREAL = 0
         DO 40 I = 1, 3
            RRT(I) = 0.0D0
            AIRT(I) = 0.0D0
   40    CONTINUE
      ENDIF
      IF (NREAL.GT.1) THEN
C
C  PUT REAL ROOTS IN INCREASING ORDER
C
         NRM = NREAL-1
         DO 60 I = 1, NRM
            IP = I+1
            X1 = RRT(I)
            I1 = I
            DO 50 J = IP, NREAL
               IF (RRT(J).LE.X1) THEN
                  X1 = RRT(J)
                  I1 = J
               ENDIF
   50       CONTINUE
            IF (I1.NE.I) THEN
               RRT(I1) = RRT(I)
               RRT(I) = X1
            ENDIF
   60    CONTINUE
      ENDIF
      RETURN
      END SUBROUTINE cubic2
C****************************************************************************
C  CUSSPL
C****************************************************************************
      SUBROUTINE cusspl(NMAXI,N,X,Y,RT,PREFAC,CUSK)
      use perconparam
C
C     This subroutine spline fits the deltag curve and find the maxima and
C     minima along the curve for CUS calculation
C
C     CALLED BY:
C               RATE
C     CALLS:
C               TSPSI, TREPT, FIVPT, ISWAP, DPSWAP
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(NSDM),Y(NSDM),W(NSDM),WK(11*NSDM),
     * SIGMA(NSDM),YS(NSDM),YP(NSDM)
C
C     assume there are at most 10 maxima and 10 minima in each delta G curve
C
      DIMENSION IDMAX(10),XMAX(10),YMAX(10)
      DIMENSION IDMIN(10),XMIN(10),YMIN(10)
      DIMENSION FIVX(5),FIVY(5),TREX(3),TREY(3)
C
      LOGICAL UNIF,PER,LIM1,LI,LIP1
C
C     Initialize values for spline call
C
      WRITE (6,*) 'For CUS calculation:'
      UNIF = .FALSE.
      PER =  .FALSE.
      CUSK = 0.0d0
      ERR = 1.0D-15
      DO I = 1, N
         W(I) = 1.0d0/(ERR**2)
         SIGMA(I) = 0.0d0
         YS(I) = 0.0d0
         YP(I) = 0.0d0
      ENDDO
      SM = N
      SMTOL = SQRT(2.0d0/SM)
      LWK = 11*N
C
C     Spline fit the curve
C
C      CALL TSPSS(N,X,Y,PER,UNIF,W,SM,SMTOL,LWK,
C     *              WK,SIGMA,YS,YP,
C     *              NIT,IER)
      NCD = 2
      IENDC = 3
         CALL TSPSI(N,X,Y,NCD,IENDC,PER,UNIF,LWK,WK,
     *              YP,SIGMA,IER)
C
C     Write out values
C
C      DO I = 1, N
C        WRITE (6,100) I, X(I), Y(I), YP(I),
C     *               hppval(X(I),n,x,y,yp,sigma,ier)
C      ENDDO
C 100  FORMAT (1X,I5,2F15.5,2F17.9)
C
C     March along the 1st derivatives and find the maxima
C
      IMAX =  0
      IMIN =  0
C
      DO I = 2, N
         LIM1 = .FALSE.
         LI  = .FALSE.
         LIP1 = .FALSE.
         IF (YP(I-1).GT.0.0d0) LIM1 = .TRUE.
         IF (YP(I).GT.0.0d0)   LI = .TRUE.
         IF (YP(I+1).GT.0.0d0) LIP1 = .TRUE.
         IF ((LIM1.NEQV.LI).AND.(LIM1.NEQV.LIP1)) THEN
C
C     Use the second derivative to find the maxima
C
C              IF (hppval(X(I),N,X,Y,YP,sigma,ier).LT.0.0d0) THEN
C
C     Or the sign of first derivatives changed from positive to negative
C
               IF (LIM1.AND..NOT.LI) THEN
                 IMAX = IMAX + 1
                 IDMAX(IMAX) = I
                 DO J = 1,3
                   TREX(J) = X(I-2+J)
                   TREY(J) = Y(I-2+J)
                 ENDDO
                 CALL TREPT (1,TREX,TREY,XMAX(IMAX),YMAX(IMAX))
              ENDIF
         ENDIF
      ENDDO
C
C     Choose the highest NMAX maxima for calculation
C     1. Bsort the array by decreasing in Y
C
      IF (IMAX.GT.NMAXI) THEN
      DO I = 1, IMAX-1
        DO J = I+1, IMAX
          IF (YMAX(J).GT.YMAX(I)) THEN
             CALL ISWAP (IDMAX(I),IDMAX(J))
             CALL DPSWAP (XMAX(I),XMAX(J))
             CALL DPSWAP (YMAX(I),YMAX(J))
          ENDIF
        ENDDO
      ENDDO
C
C     2. Order again according to increasing in X value
C
      IMAX = NMAXI
      DO I = 1, IMAX-1
        DO J = I+1, IMAX
          IF (IDMAX(J).LT.IDMAX(I)) THEN
             CALL ISWAP(IDMAX(I),IDMAX(J))
             CALL DPSWAP(XMAX(I),XMAX(J))
             CALL DPSWAP(YMAX(I),YMAX(J))
          ENDIF
        ENDDO
      ENDDO
      ELSE
         WRITE (6,111) IMAX, NMAXI
 111  FORMAT (1X,'Found ',I3,' maxima, but ask for ',I3,
     *          '.  Therefore, use all maxima for calculation.')
      ENDIF
C
C     IF no maximum found, get the global maximum, in this case,
C     the CUS will be the same as CVT
C
      IF(IMAX.EQ.0) THEN
         IMAX = 1
         YONE = Y(1)
         DO I = 2, N
           IF (Y(I).GT.YONE) THEN
              YONE = Y(I)
              IDONE = I
           ENDIF
         ENDDO
         IDMAX(1) = IDONE
         XMAX(1) = X(IDONE)
         YMAX(1) = Y(IDONE)
      ENDIF
C
C     5 pt fits the global maximum
C
      IF (IMAX.EQ.1) THEN
        I = IDMAX(1)
        DO J = 1,5
          FIVX(J) = X(I-3+J)
          FIVY(J) = Y(I-3+J)
        ENDDO
      CALL FIVPT (1,0,FIVX,FIVY,XMAX(1),YMAX(1))
      ENDIF
C
C     Print the Maxima and calculate the RCUS
C
      WRITE (6,*) 'Selected ',IMAX,' maxima:'
      DO I = 1 , IMAX
        WRITE (6,110) XMAX(I), YMAX(I)
        CUSK = CUSK + 1.0d0/(PREFAC*EXP(-YMAX(I)/RT))
      ENDDO
C
C    Looking for minima, assuming 1 minima between two maxima
C
      IF (IMAX.GT.1) THEN
        WRITE (6,*) 'Minima in between:'
        DO I = 1, IMAX - 1
          GMIN = YMAX(1)
          DO J = IDMAX(I),IDMAX(I+1)
             IF (Y(J).LT.GMIN) THEN
                GMIN = Y(J)
                JMIN = J
             ENDIF
          ENDDO
          IMIN = IMIN + 1
          IDMIN(IMIN) = JMIN
          XMIN(IMIN) = X(JMIN)
          YMIN(IMIN) = Y(JMIN)
C
C     Refine minima with 3 pt fit
C
        DO J = 1,3
          TREX(J) = X(IDMIN(I)-2+J)
          TREY(J) = Y(IDMIN(I)-2+J)
        ENDDO
        CALL TREPT (1,TREX,TREY,XMIN(I),YMIN(I))
        WRITE (6,110) XMIN(I), YMIN(I)
        CUSK = CUSK - 1.0d0/(PREFAC*EXP(-YMIN(I)/RT))
        ENDDO
      ENDIF
110   FORMAT (1X,'at s = ',F10.5,' with DELG = ',F10.5)
      CUSK = 1.0d0/CUSK
      RETURN
      END SUBROUTINE cusspl
C
C***********************************************************************
C DERVAG
C***********************************************************************
C
C
      SUBROUTINE dervag (p,xi,n)
      use perconparam, only : n3tm
C
C CALLED BY: FRPRMN
C
C CALLs 
c     -VALVAG:  EVALUATES THE FUNCTION
c 
C
C GIVEN AN N-DIMENSIONAL POINT P(1:N) AND USING A FUNCTION VALVAG
C THIS ROUTINE EVALUATES THE FIRST DERIVATIVE BY A TWO-POINT CENTRAL
C DIFFERENCE METHOD, RETURNING IT IN THE XI ARRAY
C

C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION XPP(N3TM),XPN(N3TM),P(N3TM),XI(N3TM)

      EXTERNAL VALVAG

      DATA EPSI /1.D-5/

      DO 10 J=1,N
         DO 20 I=1,N
            IF (J.EQ.I) THEN
               XPP(I)=P(I)+EPSI
               XPN(I)=P(I)-EPSI
            ELSE
               XPP(I)=P(I)
               XPN(I)=P(I)
            END IF
 20      CONTINUE
C 
C THE VECTOR IS NORMALIZED IN VALVAG
C
         VP=VALVAG(XPP)
         VN=VALVAG(XPN)
         XI(J)=1.D0/(2.D0*EPSI)*(VP-VN)

 10   CONTINUE
      RETURN
      END SUBROUTINE dervag
C
C***********************************************************************
C  DGAMMA
C***********************************************************************
C
      FUNCTION  dgamma(ZZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     MODIFIED 4/4/88 BY DWS TO BE ACCURATE FOR ZZ GT 3.
C  THIS IS A PROCEDURE THAT EVALUATES GAMMA(Z) FOR
C     0 LT Z LE 3 TO 16 SIGNIFICANT FIGURES
C    IT IS BASED ON A CHEBYSHEV-TYPE POLYNOMIAL
C   APPROXIMATION GIVEN IN H. WERNER AND R. COLLINGE, MATH. COMPUT.
C    15 (1961), PP. 195-97.
C   APPROXIMATIONS TO THE GAMMA FUNCTION, ACCURATE UP TO 18 SIGNIFICANT
C   DIGITS, MAY BE FOUND IN THE PAPER QUOTED ABOVE
C
C
C
      DIMENSION  A(18)
      PREFAC=1.0D0
      Z=ZZ
   44 CONTINUE
      IF(Z.LT.3.0D0)GO TO 45
      Z=Z-1.0D0
      PREFAC=PREFAC*Z
      GO TO 44
   45 CONTINUE
C
       A(1)=1.0D0
       A(2)=.4227843350984678D0
       A(3)=.4118403304263672D0
      A(4)=.0815769192502609D0
      A(5)=.0742490106800904D0
      A(6)=-.0002669810333484D0
      A(7)=.0111540360240344D0
      A(8)=-.0028525821446197D0
      A(9)=.0021036287024598D0
      A(10)=-.0009184843690991D0
      A(11)=.0004874227944768D0
      A(12)=-.0002347204018919D0
      A(13)=.0001115339519666D0
      A(14)=-.0000478747983834D0
      A(15)=.0000175102727179D0
      A(16)=-.0000049203750904D0
      A(17)=.0000009199156407D0
      A(18)=-.0000000839940496D0
C
C
C
      IF(Z.LE.1.0D0  ) GO TO 10
      IF(Z.LE.2.0D0  ) GO TO 20
      T=Z-2.0D0
      GO TO 30
10    T=Z
      GO TO 30
20    T=Z-1.0D0
30    P=A(18)
      DO 40 K1=1,17
      K=18-K1
      P=T*P+A(K)
40    CONTINUE
C
      P=P*PREFAC
      IF(Z.GT.2.0D0  ) GO TO 50
      IF(Z.GT.1.0D0  ) GO TO 60
      DGAMMA=P/(Z*(Z+1.0D0  ))
      RETURN
60    DGAMMA=P/Z
      RETURN
50    DGAMMA=P
      RETURN
      END FUNCTION  dgamma
C
C***********************************************************************
C  DIATOM
C***********************************************************************
C
      SUBROUTINE diatom (VADX,XGSE,IOP)
      use common_inc
      use perconparam
      use rate_const
C
C     SETS UP PARAMETERS FOR DIATOMIC REACTANT FOR ROUTINE REACT
C
C      USES L9(IOP,1) TO DETERMINE WHETHER TO SUBSTITUTE FIRST EXCITED
C      STATE ENERGY FOR GROUND STATE ENERGY WHEN APPROPRIATE FOR
C      VIB. ADIAB. OR DIAB. CALCS.
C   
C      Include statements were added 6/18/91
C
C     CALLED BY:
C                REACT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
      if(.not.allocated(intout))allocate(intout(n6tm))
C*
C
C     READ IN AND WRITE OUT DIATOMIC CONSTANTS
C
C      READ (FU5,1000) REX,DEX,BEX
      REX = RE(IOP)                                                      3/19T90
      DEX = DE(IOP)                                                      3/19T90
      BEX = BE(IOP)                                                      3/19T90
      WRITE (FU6,1100) IOP,REX,DEX,BEX
      XK3(1) = -6.0D0*DEX*BEX**3
      XK4(1) = 14.0D0*DEX*BEX**4
C
C     IF THE MULTIPLE OPTION WAS CHOSEN FOR ANHARM (LGS(5)=21) THEN
C     SET A FLAG AND TEMPORARILY CHANGE LGS(5).
C
      AFLAG = '   '
      IF (LGS(5).GE.21) THEN                                            6/30YL91
         AFLAG = 'SET'
         LGS(5) = MODE(1)
      ENDIF
C
C     COMPUTE ENDOERGICITY
C
C     IF (IOP.GE.3.AND.LGS(24).NE.0) EPRD = EPRD-DEX
C
C     COMPUTE MOMENT OF INERTIA
C
      XM1 = AMASS(3*IATOM(1))**2
      XM2 = AMASS(3*IATOM(2))**2
      Q = (XM1*XM2)/(XM1+XM2)
      XMU = REDM*Q
      FMOM(IOP) = XMU*REX*REX
      WRITE (FU6,1200) FMOM(IOP)
C
C     SCALE BE TO REDM AND COMPUTE HARMONIC FREQUENCY
C
      BEX = BEX*SQRT(REDM/XMU)
      FREQ(6) = BEX*SQRT(2.D00*DEX/REDM)                                0427WH94
      INTOUT(1) = 0                                                     0427WH94
C
C----------------------------------------------------------------------
C     THE REST OF THIS SUBROUTINE COMPUTES THE ANHARMONICITY.  THE
C     METHOD USED IS DETERMINED BY LGS(5).
C----------------------------------------------------------------------
C
      IF (LGS(5).LE.2) THEN
C
C                                                            MORSE
C
         ANHRM(1) = 0.0D0                                               0427WH94
         XGSE = 0.5D0*FREQ(6)                                           0427WH94
         IF (L9(IOP,1).EQ.1) XGSE = 3.0D0*XGSE
         VADX = VADX+XGSE
         IF (LGS(5).GT.0.AND.LGS(10).GE.2) THEN
            INTOUT(1) = 1                                               0427WH94
            IF (LGS(15).EQ.1) THEN
               INTOUT(1) = 2                                            0427WH94
               BEX = BEX*SQRT(DEX/(DEMIN-V))
               DEX = DEMIN-V
            ENDIF
            ANHRM(1) = BEX/SQRT(8.0D0*DEX*REDM)
            Y00(1) = -6.0D0*DEX*BEX**3
            XGSE = XGSE-0.25D0*FREQ(6)*ANHRM(1)                         0427WH94
            VADX = VADX-0.25D0*FREQ(6)*ANHRM(1)                         0427WH94
            IF (L9(IOP,1).EQ.1) THEN
               EXCIT = 2.0D0*FREQ(6)*ANHRM(1)                           0427WH94
               XGSE = XGSE-EXCIT
               VADX = VADX-EXCIT
            ENDIF
         ENDIF
      ENDIF
      IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20                            6/30YL91
C
      if (lgs(30).le.0) call ehook(0,iproc)                             0301YC97
C
      RETURN
C
 1000 FORMAT(8F10.6)
 1100 FORMAT(6X,7HSPECIES,I2,' IS A DIATOM WITH MORSE CONSTANTS RE,DE,
     *BE (all in a.u.) =',3F10.6)
 1200 FORMAT(6X,'MOMENT OF INERTIA = ',D20.10,' a.u.')
C
      END SUBROUTINE diatom
C
C**********************************************************************
C  DOPNM
C**********************************************************************
C
C     CALLED BY: MAIN
C     CALLS    : RPHSET,RPHWRT,PATH,ELRPH,NEXTPT,RESTRT
C
      SUBROUTINE dopnm(PARALLEL,NEXTTAG,LBUSY,JRUN,NJRUN)
      use common_inc
      use perconparam
      use rate_const
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PARALLEL, LBUSY(512)                                      0101BL05
      DIMENSION JRUN(512)                                               0202BL05
      call dopnm_mem
C
C
C        THIS SUBROUTINE IS MOVED FROM MAIN.F
C        COMPUTE STEEPEST DESCENTS PATH AND PROJECTED NORMAL MODES
C        ALONG THE PATH.
C                                          
C
c modified for keyword input, unit30.                                   073096PF
c
      IF (LGS(3).NE.0) THEN
         IF (LGS(30).EQ.1) THEN
            CALL RPHSET (6)
         elseif (lgs(30).eq.2) then                                     073096PF
            call rph40(6)                                               073096PF
         elseif (lgs(30).eq.3) then                                     0810JC97
            call rph31(6)                                               0810JC97
         ENDIF
c
c write fu30 format
c
        IF (LGS(30).LT.0) CALL RPHWRT (6)
        IF (LGS(36).EQ.0) THEN          
         IF (LGS(30).LE.0) CALL PATH(PARALLEL,NEXTTAG,LBUSY,JRUN,NJRUN) 0101BL05
         IF (LGS(30).GT.0) CALL ELRPH                                   073096PF
         ELSEIF (LGS(36).NE.0) THEN        
c
c only calculate next point
c 
            CALL NEXTPT                   
            WRITE (FU6,1000)                                            0821YC96
            STOP 'DOPNM -  THE NEXT POINT FOUND'                        0821YC96
         ENDIF                         
         IF (LGS(8).LT.0) CALL RESTRT(FU1)                              1106YL92
      ENDIF
1000  FORMAT (//' *** NEXT POINT FOUND, STOP POLYRATE ***')             0821YC96
      RETURN
      END SUBROUTINE dopnm
C
C
C**********************************************************************
C  DOREPR
C**********************************************************************
C
      SUBROUTINE dorepr
      use common_inc
      use perconparam; use cm
      use kintcm, only : irepr
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      call dorepr_mem
c
c    Optionally compute reactant and product properties
c
         LEZER0 = .FALSE.                                               1228WH94
         if (lgs(6).ne.0) then
            if (irepr(1).eq.1) then                                     0808YC97
               call prepj(1)                                            0523RS95
               call react(1)
               call wrthok(1)                                           0701YC97
            endif                                                       0808YC97
            if (lgs(6).le.2) then
               call prepj(2)                                            0523RS95
               call react(2)
               call wrthok(2)                                           0701YC97
            end if
            if (irepr(3).eq.1) then                                     0808YC97
               call prepj(3)                                            0523RS95
               call react (3)
               call wrthok(3)                                           0701YC97
            endif                                                       0808YC97
            if (lgs(6).eq.1.or.lgs(6).eq.3) then
               call prepj(4)                                            0523RS95
               call react (4)
               call wrthok(4)                                           0701YC97
            end if
            if (irepr(7).eq.1) then                                     0910JC97
               call prepj(7)                                            0910JC95
               call react(7)                                            0910JC95
               call wrthok(7)                                           0910JC97
            endif                                                       0910JC97
            if (irepr(8).eq.1) then                                     0910JC97
               call prepj(8)                                            0910JC95
               call react(8)                                            0910JC95
               call wrthok(8)                                           0910JC97
            endif                                                       0910JC97
         endif
c
c     Write out summary information of reactants and products
c
C     At this point the EZER0 has been properly decided.
C     From now on, the potential routine should give back energy
C     subtracted by EZER0
C
         LEZER0 = .TRUE.                                                1228WH94
C
         write (fu6,1550)
         write (fu6,1560) ezer0,ezer0*cev,ezer0*autocm,ezer0*ckcal
         write (fu6,1580)
         ifwkb = 0
         call rprfl(WER,WEW,IREPR,NF,IFQLOW,FRELOW)                     0707YC98
c
      RETURN

 1550 FORMAT(//80('-'),/32X,' Zero of energy ',/,80('-'))               1019WH92
 1560 FORMAT(/,10X,'EZER0 = ',3X,1PE15.5,' hartree atomic units',       1019WH92
     1       ' (a.u.)',/,21X,1PE15.5,' eV',/,21X,1PE15.5,' cm**-1',/,
     2       21X,1PE15.5,' kcal/mol')          
 1580 FORMAT(/80(1H-))                                                  1019WH92

      END subroutine dorepr
C
C**********************************************************************
C  DOREST
C**********************************************************************
C
      SUBROUTINE dorest
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface
      use kintcm, only : initg
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C  
C     PREPARE FOR A RESTART RUN        
C
      LOGICAL  LLCG1

      call zucupd_mem
      call sad_mem
C
      IPROC=0

      LGS7 = LGS(7)
      LGS9 = LGS(9)
      LGS211 = LGS2(11)                                                 1018WH94
      LGS212 = LGS2(12)
      LGS213 = LGS2(13)
      LGS214 = LGS2(14)
      LLCG1 = LLCG                                                      0909WH94
C
      CALL RESTOR
C
      LGS(7) = LGS7
      LGS(9) = LGS9
      LGS2(11) = LGS211                                                 1018WH94
      LGS2(12) = LGS212
      LGS2(13) = LGS213
      LGS2(14) = LGS214
      LLCG = LLCG1                                                      0909WH94
C
      LEZER0 = .TRUE.                                                   1228WH94
C
C
C   If ESP, LGS(35) = 1, and |LGS(9)| .GE. 2 then in addition to calling 
C   the subprogram SETUP the subprogram ESPINT must also be called as this is
C   the subprogram that initializes the electronic structure package. 
C
         IF (ABS(LGS(9)) .GE. 2) THEN                                   1107GL92
C            CALL SETUP(N3TM)                                           1107GL92
             CALL PREP                                                  1220WH94
C             IF (LGS(35) .EQ. 1) THEN                                   1107GL92
              IF (INITG(5).EQ.1) THEN                                   0514PF97
                 DO 10 I = 1, N3TM                                      1107GL92
                       IND(I) = I                                       1107GL92
10               CONTINUE                                               1107GL92
                 CALL OHOOK(5,iproc)                                    0227BL05
             ENDIF                                                      1107GL92
         ENDIF                                                          1107GL92
C
      BARRS = VCLAS(NSHLF) * VFAC                                       1105WH92
      VADSAD = VADIB(NSHLF)                                             0327WH94
      FMISPS = FMITS(NSHLF)                                             0124WH93
      DO 100 I = 1, NF(5)                                               1105WH92
         WESADS(I) = WETS(I,NSHLF)                                      1105WH92
100   CONTINUE  
C
C     Print the energetics for a restart run
C
      CALL ENROUT(VAR,VAP,EPRD,IFRFAC,FREQFAC)                          0808JC00
      CALL SENOUT(EPRD,BARRS,VAR,VAP,VADSAD,IFRFAC,FREQFAC)             0808JC00
C
C     CALCULATE THE PARAMETERS IN THE ZERO ORDER IVTST
C     CORRECTION FOR THE CLASSICAL ENERGIES AND VIBRATIONAL
C     FREQUENCIES ALONG THE MEP
C
      IF (LGS2(11).NE.0) THEN
         if (icrst.ne.0) then
            call readic
            call zoc3p
         end if
         if (icmod.ne.0) then
            call readic
            call zoc3p
         end if
         WRITE(FU6,1000)                                                0606WH94
         CALL OPT50                                                     0606WH94
         IF (LGS(3) .NE. 0) CALL ZOCPAR
         CALL ZOCPRN
      ENDIF
C    
C    
      RETURN
C
1000  FORMAT(/1X,'*********** VTST-IC information **********',/)        0606WH94
C
      END SUBROUTINE dorest
C**********************************************************************
C  DORPH
C**********************************************************************
C
      SUBROUTINE dorph
      use perconparam
      use common_inc
      use keyword_interface, only : potnam
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      save     
      call dorph_mem
c
c     Called by:
c               MAIN
c
c     Calls:
c            RPHWRT, RPHSET
c
c     Prepare potential routine
c
c      Determine if rph information is to be used:
c          lgs(30) =  -1   use pes but write out rph to for030
c          lgs(30) =   0   use pes
c          lgs(30) =   1   read in rph from file for030
c          lgs(30) =   2   read in rph from file for040                 073096PF
c          lgs(30) =   3   read in rph from file for031                 0810JC97
c
c

c
       if (lgs(30).le.0) then
          if (potnam.eq.'hooks') then                                   0312YC97
             write (fu6,1400)
             write (fu6,1500)
             call prep
             write (fu6,1400)
          endif
          if (lgs(30).lt.0) call rphwrt (0)
       else
          if(lgs(30).eq.1) then                                         073096PF
            call rphset (0)
          elseif (lgs(30).eq.2) then                                    073096PF
            call rph40(0)                                               073096PF
          elseif (lgs(30).eq.3) then                                    0810JC97
            call rph31(0)                                               0810JC97
          endif                                                         073096PF
       endif
c
 1400 FORMAT(/1X,78(1H*))
 1500 FORMAT(/1X,'Potential data from file 4:')   
 
      RETURN
   
      END SUBROUTINE dorph
C
C**********************************************************************
C  DOSAFR
C**********************************************************************
C
      SUBROUTINE dosafr
      use common_inc
      use perconparam
      use rate_const
      use kintcm, only : istatu,ifreu
      use cm
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DXXP(N3TM)

C     THIS SUBROUTINE IS MOVED FROM MAIN.F
C     COMPUTE FREQUENCIES AND NORMAL MODES AT SADDLE POINT
C 
      NFREQ = NF(5)
      MARR = 1                                                          6/30YL91
      IF (LGS(5).GT.21) THEN                                            ..
         IEND = NARR - 1                                                ..
         DO 103 I = 1, IEND                                             ..
            IF (0.GT.SRARR(I)) MARR = MARR + 1                          ..
 103     CONTINUE                                                       ..
      ENDIF  
                                                                        ..
      DO 105 I = 1, NFREQ                                               ..
         MODE(I) = MODETS(MARR,I)                                       ..
105   CONTINUE 
   
      IF (LGS(5).EQ.21.AND.LGS(1).GT.0.AND.LGS(2).GT.0) THEN
         WRITE (FU6,1900) (MODE(I),I=NFREQ,1,-1)
      ENDIF
      IF (ISTATU(5).EQ.6) THEN                                          0725YC97
        DO I = 1,N3TM                                                   0725YC97
            FREQ(I) = 0.0d0                                             0725YC97
        ENDDO                                                           0725YC97
        DO I = 1,NF(5)                                                  0725YC97
          IF (IFREU(5).EQ.0) THEN                                       0807YC97
                 FREQ(N3-I+1) = TEMWER(5,I)                             0725YC97
          ELSE                                                          0807YC97
                 FREQ(N3-I+1) = TEMWER(5,I)*CMTOAU                      0807YC97
          ENDIF                                                         0807YC97
        ENDDO                                                           0725YC97
        IF (IFREU(5).EQ.0) THEN                                         0807YC97
              FREQ(1) = TEMWER(5,NF(5)+1)                               0725YC97
        ELSE                                                            0807YC97
              FREQ(1) = TEMWER(5,NF(5)+1)*CMTOAU                        0807YC97
        ENDIF                                                           0807YC97
        CALL ZEROPT(5)                                                  0725YC97
        DO I = 1,N3                                                     0725YC97
          DO J = 1,N3                                                   0725YC97
            COF(I,J) = TEMGE(I,J)/(amass(ind(i))*amass(ind(j)))         0725YC97
          ENDDO                                                         0725YC97
        ENDDO                                                           0725YC97
        CALL FDIAG (7)                                                  0725YC97
        CALL NOROUT (2,DXXP)                                            0725YC97
      ELSE                                                              0725YC97
      IF (LGS(30).LE.0) THEN
         IF (LGS(2).GT.0) THEN                                          8/21YL91
C
C for the Saddle point, the rot. trans. are not projected via projct
C
            IF (LGS(1).GT.0) CALL NORMOD (2+5,STEPX,FISEN)              1118PF97
            IF (LGS(1).EQ.0) CALL NORMOD (3+5,STEPX,FISEN)              1118PF97
         ENDIF                                                          8/21YL91
         IF (LGS(30).LT.0) CALL RPHWRT (5)
      ELSE
         IF (LGS(1).GT.0) THEN
           if (lgs(30).eq.1) then                                       073096PF
              call rphset(5)
           elseif (lgs(30).eq.2) then                                   073096PF
              call rph40(5)                                             073096PF
           elseif (lgs(30).eq.3) then                                   0810JC97
              call rph31(5)                                             0810JC97
           endif                                                        073096PF
         ENDIF
         V = V - VSHIFT
         VAD = VAD - VSHIFT
      ENDIF
      ENDIF                                                             0725YC97
C 
C     HERE WE STORE SADDLE POINT FREQUENCIES INTO WESADS ARRAY          10/2WH92
C
      ISHFT = 3 * NATOM - NF(5)                                            ..
      IF (LBATH) ISHFT = 3 * NATOM + 1 - NF(5)                          0317Yc99
      DO 200 I = 1, NF(5)                                                  ..
         WESADS(I) = FREQ(I + ISHFT)                                       ..
200   CONTINUE                                                          1002WH92
      WSTAR = ABS(FREQ(1))                                              0112WH93
C
C     HERE WE STORE CLASSICAL BARRIER HEIGHT AT SADDLE POINT                 
C     TO VARIABLE BARRS                                                    
C
      BARRS = V * VFAC                                                  1012WH92
C

 1900 FORMAT(6X,'Anharmonicity chosen for mode i: ',10I3/(31X,10I3))    0610WH94

      RETURN

      END SUBROUTINE dosafr

C
C**********************************************************************
C  DOSAGE
C**********************************************************************
C
      SUBROUTINE dosage
      use common_inc
      use perconparam
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     THIS SUBROUTINE IS MOVED FROM MAIN.F
C     CALCULATE SADDLE POINT OR INPUT STARTING GEOMETRY (LGS(1)=0)
C
C   Called by: 
C             MAIN
C
C   Calls:
C         ESPINT, SADDLE, ENERG
C
      IF (LGS(1).EQ.0) THEN
         NDIM(5) = N3
         DO 90 I = 1, NATOM
            IATOM(I) = I
90       CONTINUE
C
         DO 100 I = 1, N3
            IND(I) = I
            X(I) = XR(I,5)
100      CONTINUE
C
C****************************************************************************
C   Computation of the bond orders and charges if LGS(35)=1 has      1021GL92
C   been removed in version 5.0.                                     1021GL92
C****************************************************************************
C
C   If ESP, LGS(35) = 1, then a call to ESPINT is needed, this is the 
C   subprogram that initializes the electronic structure package.
C
C        IF (LGS(35) .EQ. 1) CALL ESPINT(5)                             1107GL92
         IF (LGS(30) .LE. 0) THEN                                       1107GL92
c             call energ(0)                                              6/2RS94
             call ehook(0,iproc)                                              0301YC97
         ENDIF                                                          1107GL92
         WRITE (FU6,1600)
         WRITE (FU6,1610)
         WRITE (FU6,1620) (J+1,(X(J*3+I),I=1,3),J=0,NATOM-1)            0610WH94
         WRITE (FU6,1700) (NEDEG(I),ELEC(I),I=13,15)
         WRITE (FU6,1800) ICODE(5)
      ELSE
         CALL SADDLE
      ENDIF   
C
 1600 FORMAT(1X,'Starting point geometry in unscaled cartesians ',
     *          '(bohrs)')
 1610 FORMAT(//17X,1HX,15X,1HY,15X,1HZ)                                 0610WH94
 1620 FORMAT(1X,I3,4X,1P,3E16.6)                                        0610WH94
 1700 FORMAT(/1X,'Electronic degeneracies and energies (a.u.) =',       0610WH94
     * I4,2X,F12.8,/,(46X,I4,2X,F12.8))
 1800 FORMAT (/,6X,'For GTS geometries, ICODE=', I2)
C
      RETURN
C
      END SUBROUTINE dosage 
C
C***********************************************************************
C  DPSWAPD
C***********************************************************************
      SUBROUTINE dpswap(X,Y)
C
C     This subroutine swaps two double precision numbers
C
C     CALLED BY:
C               CUSSPL
C
      IMPLICIT NONE
      DOUBLE PRECISION X,Y
      DOUBLE PRECISION TEMP

      TEMP = X
      X = Y
      Y = TEMP
      RETURN
      END SUBROUTINE dpswap
C***********************************************************************
C  EBND
C***********************************************************************
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      FUNCTION ebnd (WE,XE,V,REDM)
      use perconparam
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     SOLVES CUBIC EQN TO DETERMINE THE ENERGY OF THE V-TH LEVEL OF
C     THE BOUND MODE WITH FREQ WE AND ANHARM A=-XE
C
C     CALLED BY:
C               ZEROPT,VPART,EVIB
C
C     CALLING ARGU. CHANGED BY THANH TRUONG 12/1/87
C -------------------------------------------------------------------
C
C     FOR IMAGINARY FREQUENCIES THE ZERO POINT ENERGY IS SET TO ZERO.
C
      IF (WE.LT.1.0D-12) THEN
         EBND = 0.0D0
         WRITE (FU6,1000) WE
         RETURN
      ENDIF
      EPS0 = WE*(V+0.5D0)
      EPS1 = 0.75D0/(REDM*WE)**2
      EPS1 = EPS1*(2.0D0*V*V+2.0D0*V+1.0D0)
      C1 = ABS(XE)*EPS1/(12.0D0*EPS0)
      C1SQ = C1*C1
      TEST = 1.0D0/27.0D0
C
      IF (C1SQ.LT.TEST) THEN
         THETA = ACOS(SQRT(27.0D0)*C1)
         X2 = 2.0D0*COS(THETA/3.0D0)/SQRT(3.0D0)
      ELSE
         C2 = SQRT(C1SQ-TEST)
         THRD = 1.0D0/3.0D0
         X2 = (C1+C2)**THRD+(C1-C2)**THRD
      ENDIF
      EBND = 0.5D0*(X2+1.0D0/X2)*EPS0+ABS(XE)*EPS1/(24.0D0*X2*X2)
      RETURN
C
 1000   FORMAT(3X,'THE FREQ IS IMAG: ',E12.4,' THE MORSE ENERGY FOR',
     *      ' THIS MODE IS SET TO ZERO')
C
      END FUNCTION ebnd
C
C**********************************************************************
C  ECKART
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION eckart(A,B,C,S0,L,SMEP)
C
C     CALLED BY:
C               ZOCVCL, ZOCFRE
C
C     THIS FUNCTION RETURN THE VALUE OF AN ECKART FUNCTION AT SMEP
C 
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION L
C
      PARAMETER( EXPLIM = 600.0D0 )                                     09/95KAN
C
      X  = (SMEP - S0) / L
      IF (X .GT. EXPLIM) THEN
         ECKART = A + C
      ELSE IF (X .LT. -EXPLIM) THEN
         ECKART = C
      ELSE
         Y = EXP(X)
         ECKART = A*Y / (1.0D0+Y) + B*Y / (1.0D0+Y) ** 2.0D0 + C
      ENDIF
C
      RETURN
C
      END function eckart
C***********************************************************************
C  ELLIP
C***********************************************************************
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      SUBROUTINE ellip (X,EL1,EL2)
      use perconparam
C
C     CALLED BY:
C               HQSC,THETA2,THETA3
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(20),SUM(4)
      DATA A / 1.451196212D-2,4.41787012D-3,1.736506451D-2,5.26449639D-3
     *   ,3.742563713D-2,3.328355346D-2,4.757383546D-2,4.069697526D-2,
     *   3.590092383D-2,6.880248576D-2,6.260601220D-2,9.200180037D-2,
     *   9.666344259D-2,1.2498593597D-1,4.4325141463D-1,2.4998368310D-1,
     *   1.38629436112D0,0.5D0,1.0D0,0.0D0 /
C
C     ELLIPTIC INTEGRALS OF THE FIRST AND SECOND KIND
C
      IF (X.GE.0.0D0.OR.X.LT.1.0D0) THEN
         XP = 1.0D0-X
         DO 10 I = 1, 4
            SUM(I) = A(I)
   10    CONTINUE
         K = 4
         DO 30 J = 1, 4
            DO 20 I = 1, 4
               K = K+1
               SUM(I) = A(K)+SUM(I)*XP
   20       CONTINUE
   30    CONTINUE
         T = -LOG(XP)
         EL1 = SUM(1)+T*SUM(2)
         EL2 = SUM(3)+T*SUM(4)
         RETURN
      ELSE
         WRITE (FU6,1000) X
         WRITE (FU6,*) 'RESET X, E1, and E2 to zeros'
         X = 0.0d0
         E1 = 0.0d0
         E2 = 0.0d0
      ENDIF
c         STOP 'ELLIP 1'
C
 1000 FORMAT(' ELLIP - X MUST BE .GE. 0 .AND. .LT. 1',' X=',F10.6)
C
      END SUBROUTINE ellip
C
C***********************************************************************
C  ELRPH
C***********************************************************************
C
      SUBROUTINE elrph
      use common_inc
      use perconparam, only : fu6,n3tm,nsdm,natom
      use rate_const
      use kintcm
      use keyword_interface, only : gufac6,iunit6
C
C     Calculate reaction path for polyatomic VTST by interpolating the
C     electronic structure information.
C
C     Compute reaction path starting from saddle point, in both
C        directions, using one of several optional integration
C        routines.
C
C     CALLED BY:
C                DOPNM 
C     CALLS:
C           RPHINT,SSAVE,BCALC,EXTRAP,COLSHF,BCALC0,MEPINV,MEPOUT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LEXIT,LBCALC,LMORE                                        6/07YL92
C                                                                       6/13T89
C
      DIMENSION DXB(N3TM),DXOLD(N3TM),SBX(3)                            9/30WH92
C
C
C     EPSX: Small additive number for equivalence testing.
C
      DATA EPSX /5.0D-8/                                                7/1/91VM
C
C  Initialization of variables
C
      dxb=0.d00; dxold=0.d00
      IFWKB = 0
      FISEN = DBLE(ISEN)
      DEL = DEL * DBLE(INH)                                             1106PF97
      STEPX = DEL
      IOP = 3
      N3M7 = NF(5)                                                      1016WH92 
C
C     WRITE OUT SECTION HEADER
C
      WRITE (FU6,1050)
C             
      IF(IUNIT6.EQ.1) then                                              0405JZ07
       WRITE (FU6,1250) (NST+NST2),INH,SLP/GUFAC6,SLM/GUFAC6            1106PF97
      ELSE
       WRITE (FU6,1260) (NST+NST2),INH,SLP/GUFAC6,SLM/GUFAC6            0405JZ07
      ENDIF
C
      if (isstop.gt.0) write (fu6,1254) (1.d0-fracdw),fracdw            0423TA02
      if (isstop.lt.0) write (fu6,1255) (1.d0-fracdw),fracdw            0423TA02
C
      IF (ISEN .EQ. 1) THEN                                             6/28T90
         WRITE(FU6,1251)                                                6/28T90
      ELSE                                                              6/28T90
         WRITE(FU6,1252)                                                6/28T90
      ENDIF                                                             6/28T90
C
      NST = NST+NST2
C
C        Read reaction-path hamiltonian data
C
      S = 0.0D0
      CALL RPHINT (IOP)
C
      IF (LGS(5).EQ.0) WRITE (FU6,1005)
      IF (LGS(1).NE.0) THEN
         LSAVE = NSDM
         LDEL = -1
      ELSE
         LSAVE = 0
         LDEL = 1
      ENDIF
C
      IF (LGS(1).NE.0) THEN
C
C        Put s.p. info into RESCOM arrays
C
         CALL SSAVE (LSAVE,BKAP,0)
C
C     Determine the values for V or VaG when special stop is desired
C     and save the value of ZPE for the saddle point.
C
         if (isstop.ne.0) then                                          0423TA02
            vreact = 0.d0                                               0423TA02
            vrorp = dmax1(vreact,eprd)                                  0423TA02
            varorp = dmax1(var,vap)                                     0423TA02
            vmepdw = (1-fracdw)*v + fracdw*vrorp                        0423TA02
            vzpedw = (1-fracdw)*vad + fracdw*varorp                     0423TA02
            szpesp = vad - v                                            0423TA02
         endif                                                          0423TA02
C
      ENDIF
C
      LEXIT = .FALSE.
      IDIREC = 1
      LCOUNT = 0
      MARRSP = 1                                                        6/30YL91
      IF (LGS(5).GT.21) THEN                                               ..
         IEND = NARR - 1                                                   ..
         DO 25 IARR = 1, IEND                                              ..
            IF (0.GE.SRARR(IARR)) MARRSP = MARRSP + 1                      ..
25       CONTINUE                                                          ..
      ENDIF                                                                ..
      MARR = MARRSP                                                     6/30YL91
C
C====================================================================
C     Loop over directions of s
C
   30 CONTINUE
C
C     Initialization
C         step counter
C
      IST = 0
C
C        Switch for setting the eigenvector of Hessian                  5/10DL90
C
      INDPH = 1                                                         9/18YL92
C
C         Flag for writing DX to RPH file
C
      IWRDX = 0
C
C         Flag for calculating B's
C
      LBCALC = .FALSE.
      S = 0.0D0
      SLAST = 0.0D0
C
      LGS4 = LGS(4)
C
C         Flag for special stop encountered already
C
      istops = 0                                                        0423TA02
      szpe = szpesp                                                     0423TA02
C
C--------------------------------------------------------------------
C         Integration loop over NST steps
C
   50 CONTINUE
C
C     Increment step counter
C
      IST = IST+1
C
C     Interpolate rph information
C
      S = S+STEPX*FISEN
      CALL RPHINT (IOP)
C
C     Save point so put GTS info into RESCOM arrays
C
      LSAVE = LSAVE +LDEL
      CALL SSAVE (LSAVE,BKAP,0)
C
C     For calculations with electronic structure files all the points 
C     are saved points, however the special stop is treated as there
C     is a distinction between a saved point (have the Hessian determined)
C     and a point where the Hessian is not determined.
C
C     This part is for a saved point:
C     Determine if the special stop conditions are met to stop calculating
C     the reaction path at a saved point and then change the limits.
C
         if (isstop.eq.2.and.istops.eq.0) then                          0423TA02
            if (v.lt.vmepdw.and.s.lt.0.d0) then                         0423TA02
               slm = s                                                  0423TA02
               istops = 1                                               0423TA02
            elseif (v.lt.vmepdw.and.s.gt.0.d0) then                     0423TA02
               slp = s                                                  0423TA02
               istops = 1                                               0423TA02
            endif                                                       0423TA02
         elseif (isstop.eq.-2.and.istops.eq.0) then                     0423TA02
            if (vad.lt.vzpedw.and.s.lt.0.d0) then                       0423TA02
               slm = s                                                  0423TA02
               istops = 1                                               0423TA02
            elseif (vad.lt.vzpedw.and.s.gt.0.d0) then                   0423TA02
               slp = s                                                  0423TA02
               istops = 1                                               0423TA02
            endif                                                       0423TA02
         endif                                                          0423TA02
C
C     Save the ZPE value of the saved point for special stop at current point
C     based on the VaG value.
C
         if (isstop.eq.-1) szpe = vad - v                               0423TA02
C
C     This part is for a non-saved point:
C     Determine if the special stop conditions are met, and then change
C     the limits of calculating the reaction path.
C
      if (isstop.eq.1.and.istops.eq.0) then                             0423TA02
         if (v.lt.vmepdw.and.s.lt.0.d0) then                            0423TA02
            slm = s                                                     0423TA02
            istops = 1                                                  0423TA02
         elseif (v.lt.vmepdw.and.s.gt.0.d0) then                        0423TA02
            slp = s                                                     0423TA02
            istops = 1                                                  0423TA02
         endif                                                          0423TA02
      elseif (isstop.eq.-1.and.istops.eq.0) then                        0423TA02
         if ((v+szpe).lt.vzpedw.and.s.lt.0.d0) then                     0423TA02
            slm = s                                                     0423TA02
            istops = 1                                                  0423TA02
         elseif ((v+szpe).lt.vzpedw.and.s.gt.0.d0) then                 0423TA02
            slp = s                                                     0423TA02
            istops = 1                                                  0423TA02
         endif                                                          0423TA02
      endif                                                             0423TA02
C
C     Check if B should be computed
C
      LCOUNT = LCOUNT+1
      IFLAG = 0
C
C     Compute effective masses at previous grid point
C
      CALL BCALC (SBX,DXOLD,DXB,STEPX,LDEL,IFLAG,LCOUNT,BKAP)
C
C     Check for step loop exit condition
C
      IF (.NOT.((S.LE.SLM).OR.(S.GE.SLP).OR.(ABS(S-SLM).LE.             9/18YL92
     *   0.1D0*DEL).OR.(ABS(S-SLP).LE.0.1D0*DEL))) THEN                 1106PF97
         LMORE = .FALSE.                                                6/07YL92
         IF (IDIREC.EQ.1.AND.LGS(3).GT.0.AND.LGS(1).NE.0) THEN              ..
            IF (LSAVE.GT.1) THEN                                            ..
               LMORE = .TRUE.                                               ..
            ELSE                                                            ..
               WRITE (FU6,4100)                                             ..
            ENDIF                                                           ..
          ELSEIF ((IDIREC.EQ.2.AND.LGS(3).GT.0).OR.(LGS(1).EQ.0)) THEN      ..
           ! IF (LSAVE.LT.NSDM) THEN
             IF (LSAVE<=NSDM) THEN 
               LMORE = .TRUE.                                               ..
            ELSE                                                            ..
               WRITE (FU6,4200)                                             ..
            ENDIF                                                           ..
          ELSEIF (IDIREC.EQ.1.AND.LGS(3).LT.0) THEN                         ..
            IF ((2*(NSDM-LSAVE)+1).LT.NSDM) THEN                            ..
               LMORE = .TRUE.                                               ..
            ELSE                                                            ..
               WRITE (FU6,4300)                                             ..
            ENDIF                                                           ..
          ENDIF                                                             ..
         IF ((IST.LT.NST).AND.LMORE) GO TO 50                           6/07YL92
      ENDIF
C
C         End integration loop over NST steps
C--------------------------------------------------------------------
C
      LGS(4) = LGS4
C     
      IF (LGS(3) .NE. 3) THEN
        IF (LCDSC) THEN                                                 2/10GL91
           IF (LBCALC) THEN
C
C               Compute B for the last step and effective masses at
C               previous grid point
C
              IFLAG = -1
              LCOUNT = LCOUNT+1
              CALL BCALC (SBX,DXOLD,DXB,STEPX,LDEL,
     *                    IFLAG,LCOUNT,BKAP)

           ENDIF
C
C            Calculate effective masses at last grid point
C
           IFLAG = 1
           CALL BCALC (SBX,DXOLD,DXB,STEPX,LDEL,IFLAG,LCOUNT,BKAP)
        ENDIF
C
        IF(IDIREC.EQ.LGS(12).OR.LGS(12).EQ.3) THEN
           CALL EXTRAP (LDEL,FISEN,IDIREC)
        ENDIF        
      ENDIF 
C
      IF (IDIREC.NE.2) THEN
         IF (LGS(1).EQ.0) THEN
            LEXIT = .TRUE.
         ENDIF
C
         IF (.NOT.LEXIT) THEN
C                
C               Shift results in RESCOM arrays
C
            LNCOL = NSDM-LSAVE+1                     
            CALL COLSHF (LNCOL)
C
C               Set up for other s direction
C
            FISEN = -FISEN
            LCOUNT = 0
            IFWKB = 0
            LDEL = 1
            LSAVE = LNCOL
            IF (LGS(3) .LT. 0) LSAVE = 2*LNCOL - 1
            NSHLF = LNCOL
C
C                   Reset X to saddle point geometry
C
            DO 80 I = 1, N3
               X(I) = GEOM(I,NSHLF)
   80       CONTINUE
            IF (LGS(9).NE.0) THEN                                       9/18YL92
               DO 81 I = 1, N3                                              ..
                  DO 82 II = 1, N3                                          ..
                     COFX(II,I) = CSV(II,I)                                 ..
   82             CONTINUE                                                  ..
                  IF (ABS(LGS(9)).GT.2) THEN                                ..
                     SGN1(I) = SGN2(I)                                      ..
                     IN1(I) = IN2(I)                                        ..
                  ENDIF                                                     ..
   81           CONTINUE                                                    ..
            ENDIF                                                       9/18YL92
            MARR = MARRSP                                               6/30YL91
C
C               Return to top of loop over direction os S
C
            IDIREC = 2
            IF (LGS(3) .GE. 0) THEN
               IF (LSAVE.LT.NSDM) THEN                                  6/07YL92
                  LMORE = .TRUE.                                            ..
               ELSE                                                         ..
                  WRITE (FU6,4200)                                          ..
               ENDIF                                                        ..
               IF (LMORE) GO TO 30                                          ..
            ENDIF                                                       6/07YL92
         ENDIF
      ENDIF
C
C     End of loop over directions of s
C====================================================================
C
      IF (.NOT.LEXIT) THEN
C        IF (LCDSC) CALL BCALC0(LGS,NSHLF,SSUBI,CDSCMU)                  8/26YL91
C        IF (LGS2(11) .NE. 0) CALL BCALC0(LGS,NSHLF,SSUBI,ZOCMCD)        1016WH92
        IF(LCDSC) CALL BCALC0(LGS,NSHLF,SSUBI,CDSCMU,INTMU)             0327YC97
        IF (LGS2(11).NE.0) CALL BCALC0(LGS,NSHLF,SSUBI,ZOCMCD,INTMU)    0327YC97
C
C        Invert array storeage if SSUBI array in descending order
C
         IF (ISEN.GE.0) THEN
            CALL MEPINV                        
         ENDIF
      ENDIF
c
c IVTSTM through hooks, the fast way is to rearrange the arrays         0202YC98
c into the format we want as in fu30/40 then try the routine calls      0202YC98
c
c      if (lgs(12).ne.0.AND.INOSAD.NE.1) CALL IVTMH(1,LSAVE)             0202YC98
C
C        Write out summary of reaction path properties
C                                                   
      IF (LGS(3).EQ.2 .AND. LGS(4) .EQ. 0) THEN
         WRITE (FU6,2410)
         DO 85 I = 1,LSAVE
           WRITE(FU6,2550) SSUBI(I),                                    0623WH94
     *       (K,(GEOM(3*K-3+J,I)/AMASS(3*K-3+J),J=1,3),K=1,NATOM)
   85    CONTINUE
      ELSE
C
C-----------------------------------------------------------------------
C     SCSAG has been removed from Version 5.0
C
C     Print the MEP information to FU6
C
      CALL MEPOUT
C 
      ENDIF
C
      RETURN
C
 1005 FORMAT(/2X,'Note: ZPE of imaginary frequencies will be set to',
     *       /2X,'      zero when using the harmonic approximation.')   0612WH94
 1050 FORMAT(//1X,32(1H*),' Reaction path ',32(1H*))                    0612WH94
 1250 FORMAT(/2X,'All s values and step sizes are in ',
     *           'mass-scaled bohrs.',/,
     */2X,'For the path of steepest descent:',
     */6X,'Max no. of steps in each direction = ',I7,
     */6X,'Hessian grid multiple (INH) = ',I5,
     */6X,'Path stopped if s > ',F10.6,' or s < ' ,F10.6)
 1260 FORMAT(/2X,'All s values and step sizes are in ',                 0405JZ07
     *           'mass-scaled angstroms.',/,
     */2X,'For the path of steepest descent:',
     */6X,'Max no. of steps in each direction = ',I7,
     */6X,'Hessian grid multiple (INH) = ',I5,
     */6X,'Path stopped if s > ',F10.6,' or s < ' ,F10.6)
 1251 FORMAT(/2X,'The initial step is in the direction of positive s.') 1203WH92
 1252 FORMAT(/2X,'The initial step is in the direction of negative s.') 1203WH92
 1254 format(6x,'or V becomes smaller than',/6x,'[',                    0423TA02
     *    f5.3,' * V(saddle point) + ',f5.3,' * V(rorp)]',/6x,          0423TA02
     *    'where V(rorp) is the higher of reactants or products.')      0423TA02
 1255 format(5x,'or Va^G becomes smaller than',/6x,'[',                 0423TA02
     *    f5.3,' * V+ZPE(saddle point) + ',f5.3,' * V+ZPE(rorp)]',/6x,  0423TA02
     *    'where V+ZPE(rorp) is the higher of reactants or products.')  0423TA02
 1350 FORMAT(/2X,'Extra mode information is printed between s =',       1201WH92
     * F9.4,' and',F9.4)
 1900 FORMAT(/2X,'Detailed reaction path information printed if |s|',
     * ' > ',F10.6)
 2130 FORMAT(/1X,21('*'),' Detailed reaction path information ',
     *           21('*'),
     *      //23X,'(s in mass-scaled bohrs)',
     *       /23X,'(V in hartrees)',
     *       /23X,'(X,Y,Z in unscaled bohrs)',
     *       /23X,'(DX,DY,DZ in unscaled hartree/bohr)')
 2140 FORMAT(/1X,'s = ',F9.5,'    V = ',F14.10)
 2200 FORMAT(/2X,'Atom',11X,'X',14X,'Y',14X,'Z',/,(I5,4X,1P,3E15.6))    0613WH94
 2250 FORMAT(/2X,'Atom',10X,'DX',13X,'DY',13X,'DZ',/,(I5,4X,1P,3E15.6)) 0613WH94
 2410 FORMAT(/1X,6(1H*),' Space-fixed cartesian coordinates vs'         0613WH94
     *,' reaction coordinate (a.u.) ',6(1H*)/)
 2550 FORMAT(/1X,'s = ',F10.5,
     *       /1X,'Atom',12X,'X',14X,'Y',14X,'Z',/,(I5,4X,1P,3E15.6))    0613WH94
 2800 FORMAT(' BF=', /, (1X, 1PE19.10, 3E20.10))
4100  FORMAT(/,2X,50('*'),/,2X,'Warning!!!'                             6/07YL92
     *,/,2X,'Number of save points in the first',                       6/07YL92
     *' direction has',/,2X,'reached the limit, NSDM.',/,2X,50('*'))    6/07YL92
4200  FORMAT(/,2X,50('*'),/,2X,'Warning!!!'                             6/07YL92
     *,/,2X,'Number of save points has reached the',                    6/07YL92
     *' limit, NSDM.',/,2X,50('*'))                                     6/07YL92
4300  FORMAT(/,2X,50('*'),/,2X,'Warning!!!'                             6/07YL92
     *,/,2X,'Total number of save points after',                        6/07YL92
     *' reflection has',/,2X,'reached the limit, NSDM.',/,2X,50('*'))   6/07YL92
C
      END SUBROUTINE elrph
C
C***********************************************************************
C  ENROUT
C***********************************************************************
C
C
      SUBROUTINE enrout (VAR,VAP,EPRD,IFRFAC,FREQFAC)
      use perconparam
C
C     Writes the reaction energetics to the FU6 file.
C
C     CALLED BY:
C                REACT,ZOCUPD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
         VARC1 = 0.D0 - VAR                                             0430GL92
         VARC2 = 0.D0 - EPRD                                            0430GL92
         VARC3 = 0.D0 - VAP
         VAR1  = VAR - EPRD                                             0430GL92
         VAR2  = VAR - VAP
         VAPC1 = EPRD - VAR                                             0430GL92
         VAPC2 = EPRD - VAP                                             0430GL92
         VAP1  = VAP - VAR                                              0430GL92
         VAP2  = VAP - EPRD                                             0430GL92
C
         WRITE (FU6,2300)
         WRITE (FU6,2400)                                               0430GL92
         WRITE (FU6,2310) 0.D0, VAR*CKCAL, EPRD*CKCAL, VAP*CKCAL        0430GL92
         WRITE (FU6,2320) VARC1*CKCAL, 0.D0, VAPC1*CKCAL, VAP1*CKCAL    0430GL92
         WRITE (FU6, 2330) VARC2*CKCAL, VAR1*CKCAL, 0.D0, VAP2*CKCAL    0430GL92
         WRITE (FU6, 2340) VARC3*CKCAL, VAR2*CKCAL, VAPC2*CKCAL, 0.D0   0430GL92
         WRITE (FU6,2410)                                               0430GL92
         WRITE (FU6,2310) 0.D0, VAR*CEV, EPRD*CEV, VAP*CEV              0430GL92
         WRITE (FU6,2320) VARC1*CEV, 0.D0, VAPC1*CEV, VAP1*CEV          0430GL92
         WRITE (FU6, 2330) VARC2*CEV, VAR1*CEV, 0.D0, VAP2*CEV          0430GL92
         WRITE (FU6, 2340) VARC3*CEV, VAR2*CEV, VAPC2*CEV, 0.D0         0430GL92
         WRITE (FU6,2420)                                               0430GL92
         WRITE (FU6,2421) 0.D0, VAR, EPRD, VAP                          06/96ELC
         WRITE (FU6,2422) VARC1, 0.D0, VAPC1, VAP1                      06/96ELC
         WRITE (FU6, 2423) VARC2, VAR1, 0.D0, VAP2                      06/96ELC
         WRITE (FU6, 2424) VARC3, VAR2, VAPC2, 0.D0                     06/96ELC
         WRITE (FU6,2430)                                               0430GL92
         WRITE (FU6,2431) 0.D0, VAR*AUTOCM, EPRD*AUTOCM, VAP*AUTOCM     06/96ELC
         WRITE (FU6,2432) VARC1*AUTOCM, 0.D0, VAPC1*AUTOCM, VAP1*AUTOCM 06/96ELC
         WRITE (FU6, 2433) VARC2*AUTOCM, VAR1*AUTOCM, 0.D0, VAP2*AUTOCM 06/96ELC
         WRITE (FU6, 2434) VARC3*AUTOCM, VAR2*AUTOCM, VAPC2*AUTOCM,0.D0 06/96ELC
         WRITE (FU6, 2435)
C
C   Print out the results when using scaled frequencies
C
         IF (IFRFAC.NE.0) THEN                                          0808JC00
         DZPR = (VAR * FREQFAC) - VAR                                   0808JC00
         DZPP = (VAP - EPRD) * FREQFAC - (VAP - EPRD)                   0808JC00
         WRITE (FU6,2500)                                               0808JC00
         WRITE (FU6,2400)                                               0808JC00
         WRITE (FU6,2310) 0.D0, (VAR+DZPR)*CKCAL, EPRD*CKCAL,           0808JC00
     *                    (VAP+DZPP)*CKCAL                              0808JC00
         WRITE (FU6,2320) (VARC1-DZPR)*CKCAL, 0.D0, (VAPC1-DZPR)*CKCAL, 0808JC00
     *                    (VAP1-DZPR+DZPP)*CKCAL                        0808JC00
         WRITE (FU6,2330) VARC2*CKCAL, (VAR1+DZPR)*CKCAL, 0.D0,         0808JC00
     *                    (VAP2+DZPP)*CKCAL                             0808JC00
         WRITE (FU6,2340) (VARC3-DZPP)*CKCAL, (VAR2-DZPP+DZPR)*CKCAL,   0808JC00
     *                    (VAPC2-DZPP)*CKCAL, 0.D0                      0808JC00
         WRITE (FU6,2410)                                               0808JC00
         WRITE (FU6,2310) 0.D0,(VAR+DZPR)*CEV,EPRD*CEV,(VAP+DZPP)*CEV   0808JC00
         WRITE (FU6,2320) (VARC1-DZPR)*CEV, 0.D0, (VAPC1-DZPR)*CEV,     0808JC00
     *                    (VAP1-DZPR+DZPP)*CEV                          0808JC00
         WRITE (FU6, 2330) VARC2*CEV,(VAR1+DZPR)*CEV,0.D0,              0808JC00
     *                     (VAP2+DZPP)*CEV                              0808JC00
         WRITE (FU6, 2340) (VARC3-DZPP)*CEV, (VAR2-DZPP+DZPR)*CEV,      0808JC00
     *                     (VAPC2-DZPP)*CEV, 0.D0                       0808JC00
         WRITE (FU6,2420)                                               0808JC00
         WRITE (FU6,2421) 0.D0, (VAR+DZPR), EPRD, (VAP+DZPP)            0808JC00
         WRITE (FU6,2422) (VARC1-DZPR), 0.D0, (VAPC1-DZPR),             0808JC00
     *                    (VAP1-DZPR+DZPP)                              0808JC00
         WRITE (FU6, 2423) VARC2, (VAR1+DZPR), 0.D0, (VAP2+DZPP)        0808JC00
         WRITE (FU6, 2424) (VARC3-DZPP), (VAR2-DZPP+DZPR),              0808JC00
     *                     (VAPC2-DZPP), 0.D0                           0808JC00
         WRITE (FU6,2430)                                               0808JC00
         WRITE (FU6,2431) 0.D0, (VAR+DZPR)*AUTOCM, EPRD*AUTOCM,         0808JC00
     *                    (VAP+DZPP)*AUTOCM                             0808JC00
         WRITE (FU6,2432) (VARC1-DZPR)*AUTOCM, 0.D0,                    0808JC00
     *                    (VAPC1-DZPR)*AUTOCM, (VAP1-DZPR+DZPP)*AUTOCM  0808JC00
         WRITE (FU6, 2433) VARC2*AUTOCM, (VAR1+DZPR)*AUTOCM, 0.D0,      0808JC00
     *                     (VAP2+DZPP)*AUTOCM                           0808JC00
         WRITE (FU6, 2434) (VARC3-DZPP)*AUTOCM,(VAR2-DZPP+DZPR)*AUTOCM, 0808JC00
     *                     (VAPC2-DZPP)*AUTOCM,0.D0                     0808JC00
         ENDIF                                                          0808JC00
C
      RETURN
C                      
 2300 FORMAT(//1X,'In this table Ve denotes the potential energy at',   1014WH92
     1       ' classical equilibrium,',/1X,'i.e., the energy of ',
     2       'an optimized reactant or product. ',/1X,
     3       'Further, E^G denotes the ground-state energy, i.e., ',    06/96ELC
     4       'E^G = Ve + ZPE.',                                         06/96ELC
     5       //,80('-'),/,T40,'Reaction energetics',/,80('-')/,
     6       T20,' Reactant Ve',3X,' Reactant E^G',5X,'  Product Ve',   06/96ELC
     7       3X,'  Product E^G',/,80('-'))                              06/96ELC
 2310 FORMAT(1X,'w/re reactants Ve',T20,F12.4,3X,F12.4,5X,F12.4,3X,     1014WH92
     *      F12.4)
 2320 FORMAT(1X,'w/re reactants E^G',T20,F12.4,3X,F12.4,5X,F12.4,3X,    06/96ELC
     *      F12.4)
 2330 FORMAT(1X,'w/re products Ve',T20,F12.4,3X,F12.4,5X,F12.4,3X,F12.4)
 2340 FORMAT(1X,'w/re products E^G',T20,F12.4,3X,F12.4,5X,F12.4,3X,     06/96ELC
     *       F12.4)                                                     06/96ELC
 2400 FORMAT(T47,'kcal/mol')
 2410 FORMAT(T50,'eV')
 2420 FORMAT(T38,'hartree atomic units (a.u.)')
 2421 FORMAT(1X,'w/re reactants Ve',T20,F12.7,3X,F12.7,5X,F12.7,3X,     06/96ELC
     *      F12.7)                                                      06/96ELC
 2422 FORMAT(1X,'w/re reactants E^G',T20,F12.7,3X,F12.7,5X,F12.7,3X,    06/96ELC
     *      F12.7)                                                      06/96ELC
 2423 FORMAT(1X,'w/re products Ve',T20,F12.7,3X,F12.7,5X,F12.7,3X,      06/96ELC
     *      F12.7)                                                      06/96ELC
 2424 FORMAT(1X,'w/re products E^G',T20,F12.7,3X,F12.7,5X,F12.7,3X,     06/96ELC
     *      F12.7)                                                      06/96ELC
 2430 FORMAT(T48,'cm**-1')                                              1223WH92
 2431 FORMAT(1X,'w/re reactants Ve',T20,F12.1,3X,F12.1,5X,F12.1,3X,     06/96ELC
     *      F12.1)                                                      06/96ELC
 2432 FORMAT(1X,'w/re reactants E^G',T20,F12.1,3X,F12.1,5X,F12.1,3X,    06/96ELC
     *      F12.1)                                                      06/96ELC
 2433 FORMAT(1X,'w/re products Ve',T20,F12.1,3X,F12.1,5X,F12.1,3X,      06/96ELC
     *      F12.1)                                                      06/96ELC
 2434 FORMAT(1X,'w/re products E^G',T20,F12.1,3X,F12.1,5X,F12.1,3X,     06/96ELC
     *      F12.1)                                                      06/96ELC
 2435 FORMAT(80('-'))
 2500 FORMAT(//1X,'The following values are obtained using scaled',     0808JC00
     *       ' frequencies ',                                           0808JC00
     *       //,80('-'),/,T40,'Reaction energetics',/,80('-')/,         0808JC00
     *       T20,' Reactant Ve',3X,' Reactant E^G',5X,'  Product Ve',   0808JC00
     *       3X,'  Product E^G',/,80('-'))                              0808JC00
C
      END SUBROUTINE enrout
C***********************************************************************
C  EPART
C***********************************************************************
C
      FUNCTION epart (IOP,BKT)
      use common_inc
      use perconparam
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     COMPUTES ELECTRONIC PARTITION FUNCTION FOR IOP-TH SPECIES
C
C     CALLED BY:
C               RATE, TSRATE
C
      IEND = 3*IOP
      IBEG = IEND-2
      SUM = 0.0D0
      DO 10 I = IBEG, IEND
         IF (NEDEG(I).NE.0) THEN                                        15/1/92VM
            SUM = SUM+DBLE(NEDEG(I))*EXP(-ELEC(I)/BKT)                  15/1/92VM
         ENDIF
   10 CONTINUE
      EPART = SUM
      RETURN
      END FUNCTION epart
C
C
C***********************************************************************
C  ERF
C***********************************************************************
C
      FUNCTION erf (EZ)
C
C     CALCULATES ERROR FUNCTION USING THE RELATION 7.1.26 FROM
C     ABRAMOWITZ AND STEGUN.
C
C     CALLED BY:
C                ZIGAMA
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      P = 0.3275911D0
      A1 = 0.254829592D0
      A2 = -0.284496736D0
      A3 = 1.421413741D0
      A4 = -1.453152027D0
      A5 = 1.061405429D0
      T = 1.0D0/(1.0D0+P*EZ)
      ERF = A1*T
      ERF = ERF+A2*(T**2)
      ERF = ERF+A3*(T**3)
      ERF = ERF+A4*(T**4)
      ERF = ERF+A5*(T**5)
      ERF = ERF*EXP(-EZ**2)
      ERF = 1.0D0-ERF
      RETURN
      END FUNCTION erf
C
C***********************************************************************
C  EVIB
C***********************************************************************
C
      FUNCTION evib (W,XX,NV,Y0,IS)
      use common_inc
      use perconparam
      use rate_const
C
C     CALCULATES VIBRATIONAL ENERGY LEVEL FOR MODE WITH
C     FREQ W, ANHARM XX, AND IN VIBRATIONAL STATE NV.
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C
C     CALLED BY:
C                STAUV,VTMUSN
C     CALLS:
C           EBND,DUNLEV,PADLEV,EWKB,WKBPOT,EWKB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*
C
      DATA TOLER / 1.0D-06 /
      EVIB = 0d0
C
C
C     STATEMENT FUNCTION
C
C     ESTR(WE,XE,V) = WE*(V+0.5D0)*(1.0D0-XE*(V+0.5D0))
C
      V = DBLE(NV)
C
C      IF (LGS(33).EQ.1.AND.NV.EQ.0) THEN                               9/25BCG00
      IF (LGS(33).EQ.1.and.((l0.eq.0.and.nv.eq.0).or.                   9/25BCG00
     *   (l0.gt.0.and.nv.eq.1))) THEN                                   9/25BCG00
         EVIB = EWKB0(IKBM,IS)
         RETURN
      ENDIF
       IF (LGS(5).LE.2.OR.LGS(5).EQ.9) THEN                             6/30YL91
         IF (XX.GE.-TOLER.OR.LGS(5).EQ.9) THEN
C           EVIB = ESTR(W,XX,V)
            EVIB = W*(V+0.5D0)*(1.0D0-XX*(V+0.5D0))
         ELSE
            EVIB = EBND(W,XX,V,REDM)
         ENDIF
      ELSEIF (LGS(5).EQ.7) THEN
         EVIB = EWKB(XX,Y0,NV,W,ELAST,REDM)
      ELSEIF (LGS(5).EQ.8) THEN
         EVIB = WKBPOT(XX,Y0,NV,W)
      ENDIF
C
C     THE ZPE IS SET TO ZERO IF THE FREQ. IS IMAG. AND HARMONIC
C     OPTION IS SET.
C
      IF ((LGS(5).EQ.0.OR.LGS(5).EQ.9).AND.W.LT.0.0D0) EVIB = 0.0D0     6/30YL91
      RETURN
      END FUNCTION evib
C
C***********************************************************************
C  EWKB
C***********************************************************************
C
      FUNCTION ewkb (FB,AB,N,WE,ELAST,REDM)
      use perconparam
C
C     THIS FUNCTION USES A SEMICLASSICAL METHOD TO EVALUATE THE
C     Nth ENERGY LEVEL OF A QUADRATIC-QUARTIC POTENTIAL.  A
C     NEWTON-RAPHSON SEARCH IS USED TO FIND THE ENERGY.  THE
C     INITIAL GUESS FOR THE ENERGY IS THE HARMONIC ENERGY OR
C     FOR THE CASE OF IMAG. HARMONIC FREQ. THE ENERGY OF THE
C     PREVIOUS STEP IS USED.
C     ADDED TO ICVTST ON 1/24/85
C
C     Include statement was added 6/18/91
C
C     CALLED BY:
C                ZEROPT,VPART,EVIB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IF (WE.GT.0.0D0) THEN
         E = (DBLE(N)+0.5D0)*WE
      ELSE
         E = (DBLE(N)+0.5D0)*2.0D0*ELAST
      ENDIF
      FF = 10.0D0
C
C     DO WHILE(ABS(FF).GT.1.0D-8)
C
   10 CONTINUE
      IF (ABS(FF).LE.1.0D-8) GO TO 40
      SRT = SQRT(FB**2+2.0D0*E*AB/3.0D0)
      TP2 = 6.0D0*(-FB+SRT)/AB
      TP2 = SQRT(ABS(TP2))
      TP1 = -TP2                                                        09/95KAN
C
C       EVALUATE INTEGRAL--CHEBYSHEV QUADRATURE
C
      K = 15
      SUM = 0.0D0
      DO 20 I = 1, K
         W = (PI/(DBLE(K)+1.0D0))*(SIN(DBLE(I)*PI/(DBLE(K)+1.0D0)))**2
         Y = COS(DBLE(I)*PI/(DBLE(K)+1.0D0))
         Y2 = 1.0D0-Y**2
         Y4 = 1.0D0-Y**4
         TEM1 = E*Y4/Y2
         TEM2 = FB*(TP2**2)*(Y**2)/2.0D0
         XINT = TP2*SQRT(2.0D0*REDM*(TEM1-TEM2))
         SUM = SUM+W*XINT/PI
   20 CONTINUE
C
C       EVAULUATE DERIVATIVE TO APPLY NEWTON-RAPHSON
C
      DSUM = 0.0D0
      DO 30 I = 1, K
         W = PI/DBLE(K)
         Y = COS((2.0D0*DBLE(I)-1.0D0)*PI/(2.0D0*DBLE(K)))
         TEM1 = E*Y4/Y2
         TEM2 = FB*(TP2**2)*(Y**2)/2.0D0
         XINT = TP2*SQRT(2.0D0*REDM*(TEM1-TEM2))
         XINT = REDM/XINT
         DSUM = DSUM+W*XINT/PI
   30 CONTINUE
C
C       COMPUTE NEW APPROXIMATION TO E
C
      FF = SUM-(N+0.5D0)
      DFF = DSUM
      E = E-FF/DFF
      GO TO 10
C
C     END DO
C
   40 CONTINUE
      EWKB = E
      IF (N.EQ.0) ELAST = E
      RETURN
      END FUNCTION ewkb 
C
C***********************************************************************
C  EXTRAP
C***********************************************************************
C
      SUBROUTINE extrap (LDEL,FISEN,IDREC)
      use perconparam
      use common_inc
      use rate_const
      use cm, only : sbkap
      use keyword_interface, only : iunit6,gufac6
      use kintcm
C
C     PROVIDES SIMPLE EXPONENTIAL EXTRAPOLATION OF QUANTITIES USED IN
C     TUNNELING TO THEIR ASYMPTOTIC VALUES
C
C     CALLED BY:
C                PATH
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 02/07/91
C   MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      save
C             
C     IWHAT TELLS IF WE WANT REACTANTS (-1) OR PRODUCTS (+1) FOR ASYMPT
C     NSTEP EXTRA WILL BE TAKEN, WITH STEP SIZE DELEX
C     ALPHA IS THE EXPONENTIAL RANGE PARAMETER
C     SET ASYMPTOTIC VALUES AND GET DIFFERENCE FROM CURRENT S
C 
      NSTEP = NSTE(IDREC)                                                3/18T90
      IWHAT = IWHA(IDREC)                                                3/18T90
      DELEX = DLEX(IDREC)                                                3/18T90
      ALPHA = ALPH(IDREC)                                                3/18T90
C   
C      VADAS = VAR
      IF (IWHAT.EQ.-1) VADAS = VAR                                      0418VM9M
      IF (IWHAT.EQ.1) VADAS = VAP
      DELVA = VAD-VADAS
C      VZERO = 0.0D0
      IF (IWHAT.EQ.-1) VZERO = 0.0D0                                    0418VM91
      IF (IWHAT.EQ.1) VZERO = EPRD
      DELVC = V - VZERO                                                 0217WH94
      DMCDSC = CDSCMU(LSAVE)-REDM                                       8/26YL91
C
C     LOOP OVER STEPS ALONG S
C
c      S0 = S                                                           0601YC98
      ALF = -FISEN*ALPHA
C
      ISHFT = 7
      IF (ICODE(5).EQ.3) ISHFT = 6
      N3M7 = N3-ISHFT
      IF (s.gt.0.0d0) then                                              0601YC98
         s = slp                                                        0601YC98
        s0 = s                                                          0601YC98
      else                                                              0601YC98
         s = slm                                                        0601YC98
        s0 = s                                                          0601YC98
      endif                                                             0601YC98
      DO 10 I = 1, NSTEP
         S = S+DELEX*FISEN
         LSAVE = LSAVE+LDEL
         SSUBI(LSAVE) = S
         EXFAC = EXP(ALF*(S-S0))
c
c  The IVTSTM-H only works with saddle point option, if nosad then extrapolation
c  with exp form is used
c
         IF (INOSAD.EQ.1) THEN                                          0202YC98
            VCLAS(LSAVE) = VZERO+DELVC*EXFAC                            0202YC98
            VADIB(LSAVE) = VADAS+DELVA*EXFAC                            0202YC98
            CDSCMU(LSAVE) = REDM+DMCDSC*EXFAC                           8/26YL91
         ELSE                                                           0202YC98
            VCLAS(LSAVE) = 0.0d0                                        0202YC98
            SBKAP(LSAVE) = 0.0d0                                        0812YC97
c           XLCDSC(LSAVE) = 0.0d0                                       0202YC98
c           XHCDSC(LSAVE) = 0.0d0                                       0202YC98
            VADIB(LSAVE) = 0.0d0                                        0202YC98
            CDSCMU(LSAVE) = 0.0d0                                       0202YC98
         ENDIF                                                          0202YC98
      DO 10 J = 1, N3M7
            WETS(J,LSAVE) = 0.0D0
   10 CONTINUE
      IF (IWHAT.EQ.-1) THEN
         WRITE (FU6,1100)
      ELSEIF (IWHAT.EQ.1) THEN                                          18/491VM
         WRITE (FU6,1200)
      ENDIF
C     WRITE (FU6,1300) S0,S,DELEX,ALPHA
      WRITE (FU6,1300) S0/GUFAC6,S/GUFAC6,DELEX/GUFAC6,ALPHA            0405JZ07
      RETURN
C
 1000 FORMAT(2I5,2F10.6)
 1100 FORMAT(/,' EXTRAPOLATION TOWARDS REACTANTS PERFORMED.')
 1200 FORMAT(/,' EXTRAPOLATION TOWARDS PRODUCTS PERFORMED.')
 1300 FORMAT(' FROM S = ',F7.3,' TO ',F7.3,' WITH STEPSIZE = ',F7.4,/
     *,' ALPHA = ',F10.6,/)
C
      END SUBROUTINE extrap
C
C**********************************************************************
C  EXTRP
C**********************************************************************
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      FUNCTION extrp(X1,X2,Y1,Y2,XX)
      use perconparam
C
C     Called by LCG                                                     0708JC00
C
C     FIT THE TWO POINTS TO A STRAIGHT LINE AND DO THE INTERPOLATION
C     OR EXTRAPOLATION.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      XDIF = X1 - X2
      IF(XDIF.EQ.0.0D0)THEN
       WRITE(FU6,*)' IDENTICAL X VALUES IN EXTRP'
       WRITE(FU6,10)X1,Y1
 10    FORMAT(2X,'X1, Y1',2E14.5)
       STOP 'EXTRP 1'
      ENDIF
      CA = (Y1 - Y2)/XDIF
      CB = Y1 - CA*X1
      EXTRP = CA*XX + CB
      RETURN
      END FUNCTION extrp   
C
C***********************************************************************
C F1DI
C***********************************************************************
C
C
      FUNCTION f1di(X)
      use perconparam
      use common_inc, only : xcom,xicom,ncom
C
C CALLED BY: LINMNR
C
C CALL VALVAG
C
C EVALUATES THE FUNCTION FOR LINMNR
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION XT(N3TM)

      DO 10 J=1,NCOM
         XT(J)=XCOM(J)+X*XICOM(J)
 10   CONTINUE
      F1DI=VALVAG(XT)
      RETURN
      END FUNCTION f1di
C
C***********************************************************************
C F1DIM
C***********************************************************************
C
      FUNCTION f1dim (RFAC)
      use perconparam
      use common_inc
C
C     Include statements were added 6/18/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DO 10 I = 1,NCOM
        X(IND(I)) = XCOM(I) + RFAC*XICOM(I)
 10   CONTINUE
c      call energ(0)                                                     6/2RS94
      call ehook(0,iproc)
C
      F1DIM = V
      RETURN
      END
C
C***********************************************************************
C   FCINPT
C***********************************************************************
C
      SUBROUTINE fcinpt
      use perconparam
C
C   This subprogram closes all the input data files used by the end of READ5.
C
C   Calls:
C         FICLSE
C   Called by:
C         MAIN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*20 FSTAT
C
C   Initialize the variable FSTAT; this variable is used to set the status
C   for a file when it is closed, i.e., KEEP or DELETE.
C
      FSTAT = 'KEEP'
C
C   Close the input data file linked to FORTRAN unit fu5, all the data in 
C   this file is read in the subprograms INITZE and READ5.
C
      CALL FICLSE (FU5, FSTAT)
C
      RETURN
      END SUBROUTINE fcinpt
C
C***********************************************************************
C   FCMEP
C***********************************************************************
C
      SUBROUTINE fcmep
      use common_inc
      use kintcm
      use perconparam
C
C   This subprogram closes all the files used by the end of the MEP.
C
C   Calls:
C         FICLSE
C   Called by:
C         MAIN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*20 FSTAT
C
C   Initialize the variable FSTAT; this variable is used to set the status
C   for a file when it is closed, i.e., KEEP or DELETE.
C
      FSTAT = 'KEEP'
C
C   Close the files used for storing the MEP information, .i.e., files linked
C   to FORTRAN units fu1, fu2, and/or fu3.
C
      IF (LGS(8) .EQ. -1 .OR. LGS(8) .EQ. 1) 
     *    CALL FICLSE (FU1, FSTAT)
      IF (LGS(8) .EQ. 2) CALL FICLSE (FU2, FSTAT)
      IF (LGS(8) .EQ. 3) CALL FICLSE (FU3, FSTAT)
C
C   Close the file linked to FORTRAN unit fu30 if LGS(30) = 1, or LGS(30) = -1.
C
      IF (LGS(30) .EQ. -1 .OR. LGS(30) .EQ. 1)                          073196PF
     *    CALL FICLSE (FU30, FSTAT)                                     073196PF
C
C
C   Close the file linked to FORTRAN unit fu31 if LGS(31) = 3, or LGS(30) = -3.
C
      IF (LGS(30) .EQ. -3 .OR. LGS(30) .EQ. 3)                          0810JC97
     *    CALL FICLSE (FU31, FSTAT)                                     0810JC97
C
C   Close the file linked to FORTRAN unit fu40 if LGS(30) = 2           073196PF
C
      IF (LGS(30) .EQ. 2) CALL FICLSE (FU40, FSTAT)                     073196PF
C
C   If LGS(35) = 1, close the input data files linked to FORTRAN units
C   fu71 through f75.                                                   0606PF97
C
c      if (potnam.eq.'mopac') CALL FICLSE (FU75, FSTAT)                  0530RS95
c      if (potnam.eq.'mopac' .AND. LGS(8) .LE. 0) THEN                   0530RS95
      IF (INITG(5).EQ.1) CALL FICLSE (FU75, FSTAT)                      0514PF97
      IF (LGS(8) .LE. 0) THEN                                           0514PF97
          IF (LGS(6) .NE. 0) THEN
              IF (INITG(1).EQ.1) CALL FICLSE (FU71, FSTAT)              0606PF97
              IF (INITG(3).EQ.1) CALL FICLSE (FU73, FSTAT)              0606PF97
              IF (LGS(6).LE.2 .AND. INITG(2).EQ.1)                      0514PF97
     *               CALL FICLSE (FU72, FSTAT)                          0606PF97
              IF ((LGS(6).EQ.3 .OR. LGS(6).EQ.1) .AND. INITG(4).EQ.1)   0514PF97
     *               CALL FICLSE (FU74, FSTAT)
          ENDIF
      ENDIF
C
C   Close the input data file linked to FORTRAN unit fu50 if LGS2(11) =/= 0.
C
         IF (LGS2(11).NE.0.AND.IVIC.EQ.1) CALL FICLSE (FU50, FSTAT)
         IF (LGS2(11).NE.0.AND.IVIC.EQ.2) CALL FICLSE (FU51, FSTAT)
C
      RETURN
      END SUBROUTINE fcmep
C
C***********************************************************************
C   FCRATE
C***********************************************************************
C
      SUBROUTINE fcrate
      use perconparam
      use common_inc
      use cm
      use rate_const
      use kintcm
C
C   This subprogram closes all the data files that are open at the end of
C   the calculation.
C
C   Calls:
C         FICLSE
C   Called by:
C         MAIN
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
         CHARACTER*20 FSTAT
C
C   Initialize the variable FSTAT; this variable is used to set the status
C   for a file when it is closed, i.e., KEEP or DELETE.
C
         FSTAT = 'KEEP'
C
C   Close the long output file and the short output files linked to 
C   FORTRAN units fu6, fu14, and fu15, respectively.
C
      CALL FICLSE (FU6, FSTAT)
      CALL FICLSE (FU14, FSTAT)
      CALL FICLSE (FU15, FSTAT)
      CALL FICLSE (FU61,FSTAT)                                          0725YC97
      IF(IWRT62 .NE. 0) CALL FICLSE (FU62,FSTAT)                        0522TA02
C
C   Close the output files from the LCG tunneling calculation,          0708JC00
C   |LGS(9)| is greater than or equal to 2.
C
      IF (IOT .NE. 0) THEN
          CALL FICLSE (FU22, FSTAT)
      ELSE 
          FSTAT = 'DELETE'
          CALL FICLSE (FU22, FSTAT)
          FSTAT = 'KEEP'
      ENDIF
      IF (LLCG .AND. LGS2(8) .NE. 0) THEN
          CALL FICLSE (FU41, FSTAT)
          CALL FICLSE (FU42, FSTAT)
          CALL FICLSE (FU43, FSTAT)
          CALL FICLSE (FU44, FSTAT)
          CALL FICLSE (FU45, FSTAT)
          CALL FICLSE (FU46, FSTAT)
          CALL FICLSE (FU47, FSTAT)
      ENDIF
      IF(LLCG) THEN                                                     0708JC00
       IF(ILCRST.EQ.1) THEN                                             0708JC00
        CALL FICLSE (FU48, FSTAT)                                       0708JC00
       ENDIF                                                            0708JC00
       IF(ILCSTR.EQ.1) THEN                                             0708JC00
         CALL FICLSE (FU49, FSTAT)                                      0708JC00
       ENDIF                                                            0708JC00
      ENDIF                                                             0708JC00
C
      IF (LGS2(13) .NE. 0) THEN                                         0705WH94
         CALL FICLSE (FU25, FSTAT)                                      0705WH94
         IF (LGS2(13) .EQ. 2) CALL FICLSE (FU26, FSTAT)                 0705WH94
         IF (IPRCD .EQ. 1) CALL FICLSE (FU28, FSTAT)                    0203YC98
      ENDIF                                                             0705WH94
C
      IF (LGS2(14) .EQ. 1) CALL FICLSE( FU27, FSTAT)                    0705WH94
C
      RETURN
      END SUBROUTINE fcrate
C
C***********************************************************************
C  FDIAG
C***********************************************************************
C
      SUBROUTINE fdiag (IOP)
      use perconparam, only : fu6,n3tm
      use common_inc
      use potmod
      use rate_const
      use kintcm
C
C
C This subprogram does
C    1) diagonalization of the force matrix
C    2) sort the normal modes in the order of the magnitude of the eigenvalues
C    3) sort the normal modes so according to the its overlap with the ones at
C       the previous save points, when tunneling is requested,
C    4) change the phase of an eigenvector so that its biggest component is
C       positive, when at the saddle point or when SCT is requested, 
C    5) match the phase of eigenvectors with the ones at the previous save point
C       when LCT is requested,
C
C     CALLED BY:
C                RPHRD2,NORMOD
C     CALLS:
C            PROJCT,RSPDRV,OVRLP 
C 
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION SCR(N3TM),SCR2(N3TM),TCOF(N3TM),TEMCOF(N3TM,N3TM)       0206WH93
      DIMENSION IORDER(N3TM)                                            0206WH93
      LOGICAL LWRITE,LREORD                                             1028WH92
C
      DATA EPSX  /1.0D-10/
      IF(.NOT.ALLOCATED(COF)) ALLOCATE(COF(N3TM,N3TM))
      scr=0.d00; scr2=0.d00; tcof=0.d00; temcof=0.d00; cof=0.d00
C
      IF (IOP.LT.7) THEN                                                0725YC97
      LGS4 = LGS(4)                                                     9/18YL92
      LGS9 = LGS(9)                                                         ..
      IF(ABS(LGS9).GE.2) LWRITE = LGS4.GT.0 .OR.                            ..
     *    (SOB.LT.SOE .AND. S.GE.SOB .AND. S.LE.SOE) .OR.                   ..
     *    (SOE.LT.SOB .AND. S.GE.SOE .AND. S.LE.SOB)                    9/18YL92
      KOP = ABS(IOP)

      IF (IOP.LT.0) THEN
         N3M7 = NF(KOP)                                                 9/18YL92
         NEND = 3*NRATOM(KOP)                                           9/18YL92
      ELSE
         N3M7 = NF(5)                                                   9/18YL92
         NEND = N3                                                      9/18YL92
      ENDIF
           
C     call fdiag_mem(nend)
      call fdiag_mem(N3TM)
C
C Diagonalize F to obtain force constants and normal mode directions.
C    If not at saddle point, use normalized grad(v) to project out of F
C    the zero modes
C
C
      IF (IOP.EQ.3) CALL PROJCT
C
      CALL RSPDRV (N3TM,NEND,F,FREQ,1,COF,SCR,SCR2,IERR)                0206WH93
C
      IF (IERR.NE.0) THEN
         WRITE (FU6,1000) IERR
         STOP 'FDIAG 1'
      ENDIF
C
C     Calculate frequencies from force constants
c     Add the freqfac to obtain the scaled frequencies                  1126NG04
c     when the FREQSCALE appears in the fu5 file                        1126NG04
c     Fixed by Nuria
C
      DO 10 I = 1, NEND
         IF (FREQ(I) .NE. 0.0D0) THEN
            if  (ifqfac.eq.1.and.lgs(30).gt.0) then                     1024BE06
               FREQ(I) = freqfac*SQRT(ABS(FREQ(I)/REDM))*               1126NG04
     &                SIGN(1.0D0,FREQ(I))                               1126NG04
             else                                                       1126NG04
               FREQ(I) = SQRT(ABS(FREQ(I)/REDM))*SIGN(1.0D0,FREQ(I))
             end if                                                     1126NG04
         ENDIF
         INTOUT(I) = 0
   10 CONTINUE
      else                                                              0725YC97
        N3M7 = NF(5)                                                    0725YC97
        NEND = N3                                                       0725YC97
        IOP = IOP - 5                                                   0725YC97
      endif                                                             0725YC97

C     Sort the absolute value of frequencies in accending order
C     but keep the big imaginary frequency of saddle point the first one
C
      IF (IOP .EQ. 2) THEN
         IBEG = 2
      ELSE
         IBEG = 1
      ENDIF
      ISHFT = NEND - N3M7
C
      LREORD = .FALSE.; iorder=0
      DO 80 I = 1,NEND
         IORDER(I) = I
80    CONTINUE
c
C   add a sort to make all frequencies in the canonical order
C      1. ordering the tranlational and rotational modes (+1 for GTS)
C         in the order of decreasing absolute magnitude
C      2. ordering the vibrational modes in decreasing order
C
      DO 100 I = IBEG, NEND
         DO 90 J = I+1,NEND
C            IF ( ABS(FREQ(J)).LT.ABS(FREQ(I)).AND.J.GT.ISHFT ) THEN
             IF ( ABS(FREQ(J)).LT.ABS(FREQ(I))) THEN                    1210YC96
               ITEMP = IORDER(I)
               IORDER(I) = IORDER(J)
               IORDER(J) = ITEMP
               TEMPX = FREQ(I)
               FREQ (I) = FREQ(J)
               FREQ (J) = TEMPX
               LREORD = .TRUE.
            ENDIF
90       CONTINUE 
100   CONTINUE 
C
C   add a sort to make all frequencies in the right order               1210YC96
C   canonical order :
C      1. ordering the tranlational and rotational modes (+1 for GTS)
C         in the order of decreasing absolute magnitude
C      2. ordering the vibrational modes in decreasing order
C
      if (lreord.and.(ireord.eq.0)) then
        do i = ISHFT+1,NEND
          do j = i+1, NEND
            if (freq(i).gt.freq(j)) then
               tempf = freq(i)
               freq(i) = freq(j)
               freq(j) = tempf
               itemo = iorder(i)
               iorder(i) = iorder(j)
               iorder(j) = itemo
            endif
          enddo
        enddo
      endif
C
C    end of sort
C
C     According to the sorting above, reorder the eigenvector array
C
      IF (LREORD) THEN
         DO 150 I = IBEG, NEND
            NEW = IORDER(I)
            DO 140 J = 1, NEND
               TEMCOF(J,I) = COF(J,NEW)
140         CONTINUE
150      CONTINUE
         DO 160 I = IBEG, NEND
            DO 160 J = 1, NEND
               COF(J,I) = TEMCOF(J,I)
160      CONTINUE
      ENDIF
C
C     Calculate the overlap integral and check the frequency matching
C
      IF (IOP.GT.0.AND.LGS(9).NE.0.AND.INDPH.NE.0.AND.IREORD.NE.0) CALL 1210YC96
     *                         OVRLP(LWRITE,N3,N3M7,COF,COFX,FREQ,S)    9/18YL92
C
C Change phase of each eigenvector so that the largest component is
C    always positive
C
      DO 230 J = 1, NEND                                                9/18YL92
         COFMAX = 0.0D0                                                     ..
         IMAXX = IN1(J)  
         IF (ABS(LGS(9)).GE.2.AND.INDPH.NE.0)                               ..
     *      A = SGN1(J) *SIGN(1.0D0,COF(IMAXX,J))                           ..
         DO 210 I = 1, NEND                                                 ..
            IF (ABS(COF(I,J)).GT.(COFMAX+EPSX)) THEN                        ..
               COFMAX = ABS(COF(I,J))                                       ..
               IMAXX = I                                                    ..
            ENDIF  
  210    CONTINUE 
         IF (ABS(LGS(9)) .LT. 2 .OR. INDPH .EQ. 0)                          ..
     *      A = SIGN(1.0D0,COF(IMAXX,J)) 
         DO 225 I = 1,NEND                                                  ..
            COF(I,J) = A*COF(I,J)                                           ..
            COFX(I,J) = COF(I,J)                                            ..
  225    CONTINUE                                                           ..
         IF (ABS(LGS(9)).GE.2) THEN                                         ..
            IN1(J) = IMAXX                                                  ..
            SGN1(J) = SIGN(1.0D0,COF(IMAXX,J))                              ..
         ENDIF                                                              ..
  230 CONTINUE                                                              ..
C
      IF (IOP .GT. 0 .AND. INDPH.EQ.0)  THEN                            9/18YL92
          DO 260 I = 1, NEND                                            9/18YL92
             DO 250 K = 1,NEND                                          9/18YL92
                CSV(K,I) = COF(K,I)                                     9/18YL92
  250        CONTINUE                                                   9/18YL92
             IF (ABS(LGS(9)).GE.2) THEN                                 9/18YL92
                SGN2(I) = SGN1(I)                                       9/18YL92
                IN2(I) = IN1(I)                                         9/18YL92
             ENDIF                                                      9/18YL92
  260     CONTINUE                                                      9/18YL92
       ELSEIF (IOP .LT.0) THEN                                          9/18YL92
          DO 350 K = 1, NEND                                            1025YL91
             DO 310 I = 1, N3                                           1025YL91
                    TCOF(I) = 0.D0                                      1025YL91
  310        CONTINUE                                                   1025YL91
             DO 320 I = 1, NEND                                         1025YL91
                    TCOF(IND(I)) =  COF(I,K)                            1028WH92
  320        CONTINUE                                                   1025YL91
             DO 330 I = 1, N3                                           1025YL91
                    COF(I,K) = TCOF(I)                                  1025YL91
  330        CONTINUE                                                   1025YL91
  350     CONTINUE                                                      1025YL91
      ENDIF                                                             1025YL91
C
      RETURN                                                             
C
 1000 FORMAT (/ 1X, 30(1H*),7H IERR =,I5)
 1100 FORMAT (5X,'********* CAUTION: FOR ALL IMAG. FREQ. THE ZPE WILL    
     *BE SET TO ZER0 WHEN USING THE HARMONIC APPROXIMATION*********')    
C
      END SUBROUTINE fdiag
C
C***********************************************************************
C   FICLSE
C***********************************************************************
C
      SUBROUTINE ficlse(NUNIT, FSTAT)
      use perconparam
C
C   Subroutine to close a file given the unit number and the status. 
C   If an error occurs when a file is being closed this subprogram
C   will print an error message to FORTRAN unit fu6 and stop.
C
C   Called by: 
C             FCINPT, FCMEP, FCRATE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*20 FSTAT
C
C   Check the value of FSTAT, FSTAT can only be KEEP or DELETE.  If 
C   FSTAT does not have one of these two values, reset to KEEP.
C
         IF (FSTAT .NE. 'KEEP') THEN
             IF (FSTAT .NE. 'DELETE') FSTAT = 'KEEP'
         ENDIF
C
         CLOSE (UNIT=NUNIT, STATUS=FSTAT, ERR=100) 
C
         RETURN
C
100      WRITE (FU6, 1000) NUNIT
1000     FORMAT(/,2X,T5,'Error occurred while closing the file',
     *                  'linked to FORTRAN unit ',I2)
         STOP 'FICLSE 1'
         END SUBROUTINE ficlse
C  
C**********************************************************************
C  FINDL 
C**********************************************************************
C
      SUBROUTINE findl
      use perconparam
      use common_inc
      use keyword_interface; use rate_const
C
C     CALLED BY:
C               MAIN
C     CALLS:
C               ECKART
C
C     THIS FUNCTION RETURN THE VALUE OF AN ECKART FUNCTION AT SMEP
C
C  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  GET the range paramter L from Low Level
C
      IF (BARRS.LT.0) THEN
          WRITE (FU6,*) 'The Classical barrier is less than zero!'
          WRITE (fu6,*) 'Not able to fit the L parameter. '             0601YC98
          RETURN
      ELSE IF (EPRD.GT.0) THEN
          WRITE (FU6,*) 'The reaction is endothermic!'
          RETURN
      ELSE
      V0S = BARRS
      AVS = EPRD
      CVS = 0.0d0
      DET = SQRT( ( V0S - CVS ) * ( V0S - CVS - AVS ) )
      BVS = 2.0D0*V0S-AVS-2.0D0*CVS+2.0D0*DET
C
C Calculate L parameter from the imaginary frequency of LL
C
      RANGE = SQRT((2.0d0*V0S*(V0S-AVS))/(REDM*(WSTAR*WSTAR)*BVS))
C
C    ISS only goes down to 2 now, to avoid out-of bounds                1020BE05
C
C 1/2 down, reactant side
C
      DES = BARRS/2.0d0
      IF (EPRD.LE.0) THEN
        DO ISS = 2,LSAVE
           IF ((SSUBI(ISS).LE.0.0d0).AND.(VCLAS(ISS).GT.DES)) THEN
                IL = ISS - 1
                IR = ISS
                GO TO 999
           ENDIF
        ENDDO
c
c 1/2 down, product side
c
      ELSE
        DO ISS = LSAVE,2, -1
           IF ((SSUBI(ISS).GT.0.0d0).AND.(VCLAS(ISS).GT.DES)) THEN
                IL = ISS - 1
                IR = ISS
                GO TO 999
           ENDIF
        ENDDO
      ENDIF
C
C  linear interpolation to find the extra point
C
 999  RSL = (DES-VCLAS(IL))/(VCLAS(IR)-VCLAS(IL))
      STX = SSUBI(IL)+RSL*(SSUBI(IR)-SSUBI(IL))
      VCX = DES
      ERROLD = 1000.0d0
      DO IG = 1,1000
        RANGES = IG * 0.01D0
        S0X = -RANGES * LOG((AVS+BVS)/(BVS-AVS))
        ERRX = VCX - ECKART (AVS,BVS,CVS,S0X,RANGES,STX)
        IF (ABS(ERRX).LT.ERROLD) THEN
          ERROLD = ABS(ERRX)
          TEMPL = RANGES
          TEMPS0 = S0X
        ENDIF
      ENDDO
      RANGES = TEMPL
      S0VS = TEMPS0
      TEMPV  = ECKART(AVS,BVS,CVS,S0VS,RANGES,STX)
      WRITE (FU6,1000)
C     WRITE (FU6,1010) IL,IR,STX,VCX*CKCAL
      IF(IUNIT6.EQ.1) WRITE (FU6,1010) IL,IR,STX/GUFAC6,VCX*CKCAL       0405JZ07
      IF(IUNIT6.EQ.0) WRITE (FU6,1015) IL,IR,STX/GUFAC6,VCX*CKCAL       0405JZ07
      WRITE (FU6,610)  AVS*CKCAL,BVS*CKCAL,BARRS*CKCAL
      WRITE (FU6,1020) TEMPV*CKCAL
C     WRITE (FU6,1025) RANGES
C     WRITE (FU6,1030) RANGE
      IF(IUNIT6.EQ.1) then                                              0405JZ07
        WRITE (FU6,1025) RANGES
        WRITE (FU6,1030) RANGE
      ENDIF
      IF(IUNIT6.EQ.0) then
        WRITE (FU6,1045) RANGES/GUFAC6
        WRITE (FU6,1050) RANGE/GUFAC6
      ENDIF                                                             0405JZ07
610   FORMAT(2X,'A = ',F12.6,' kcal/mol',/2X,'B = ',F12.6,' kcal/mol',
     *      /2X,'V = ',F12.6,' kcal/mol')
1000  FORMAT(/1X,78('*'),/2X,'Low level information used for',
     *       ' the range parameter L for VTST-IC (RPL)',/1X,78('*'))
1010  FORMAT(2X,'Extra Point between = ',I6,' and ',I6
     *      /2X,'S (extra point) = ',F12.6,' bohr',/2X,
     *      'V (extra point) = ',F12.6,' kcal/mol',/)
1015  FORMAT(2X,'Extra Point between = ',I6,' and ',I6
     *      /2X,'S (extra point) = ',F12.6,' angstrom',/2X,
     *      'V (extra point) = ',F12.6,' kcal/mol',/)                   0405JZ07
1020  FORMAT(/2X,'Calculated value of V at extra point =  ',F12.6,
     *      ' kcal/mol')
1025  FORMAT(2X,'L calculated from 4-point fit =         ',F12.6,
     *      ' bohr') 
1030  FORMAT(2X,'L calculated from imaginary frequency = ',F12.6, 
     *      ' bohr')
1045  FORMAT(2X,'L calculated from 4-point fit =         ',F12.6,
     *      ' angstrom')
1050  FORMAT(2X,'L calculated from imaginary frequency = ',F12.6,
     *      ' angstrom')
      ENDIF
      RETURN
      END SUBROUTINE findl
C
C***********************************************************************
C  FINOUT
C***********************************************************************
C
      SUBROUTINE finout(SVAGM)
      use common_inc
      use perconparam
      use rate_const
      use kintcm
      use keyword_interface, only : iunit6,gufac6
C
C     COMPUTES AND OUTPUTS FINAL SET OF RATE CONSTANTS
C     ALSO OBTAINS ACTIVATION ENERGIES AND ANALYZES K+/KCVT
C
C      CALLED BY:
C                 RATE
C      CALLS:
C            TABLE,FIVPT,DATTIM,TITLE,WTITLE
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C   MODIFICATIONS FOR CD-SCSAG WERE MADE 08/26/91
C   SUBROUTINE RESTRUCTED 09/20/91 BY G. LYNCH
C   FORMAT STATEMENTS MODIFIED BY GCL 01/17/92
C   80-column output was made by Wei-Ping Hu, June 1994
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      CHARACTER*1 IMA(NVIBM)
      CHARACTER*4 SWLCT(2)                                              0708JC00
      DIMENSION VCX(40),VAX(40),FMX(40)
      DIMENSION CAKBT(40,6),EACTVE(6)                                   0929YC97
      DIMENSION SPT(5),FPT(5),TAVE(10),WEX(NVIBM),IWETS(NVIBM),
     * IWEX(NVIBM)
      DIMENSION RTOT(50), RVR(50)
      DIMENSION RZCTNT(40,6),RSCTNT(40,6),RLCTNT(40,6),RMOTNT(40,6)     0625TA02
      CHARACTER*4 ATYPE(6)                                              0929YC97
      save                                                              0601YC98
      DATA SWLCT /'LCG3','LCG4'/                                        0708JC00
C
C     STATEMENT FUNCTION
C
      EACT(B1,B2,XK1,XK2) = ((LOG(XK2/XK1))/(B1-B2))*CKCAL
C
C     INPUT INFO FOR ACTIVATION ENERGY ANALYSIS HAS BEEN MOVED TO READ5
C
C*******************************************************************************
C
C     OUTPUT KAPPA FACTORS VS. TEMPERATURE
C
C*******************************************************************************
C
C
C   Note:  If LTUN = TRUE then Wigner and Cag are always calculated
C          If LTUN = FALSE then no Kappa values were calculated
C
C
      MTYPE = 2                                                         1106YL92
      ATYPE(1) = ' TST'                                                     ..
      ATYPE(2) = ' CVT'                                                     ..
      IF (NFCUS.NE.0) THEN                                              0929YC97
          MTYPE = MTYPE + 1                                                 ..
          ATYPE(MTYPE) = ' CUS'                                             ..
      ENDIF                                                                 ..
c     IF (LGS(20).NE.0) THEN                                                ..
c         MTYPE = MTYPE + 1                                                 ..
c         ATYPE(MTYPE) = 'ICVT'                                             ..
c     ENDIF                                                                 ..
      IF (LGS(21).GE.1) THEN                                                ..
          MTYPE = MTYPE + 1                                                 ..
          ATYPE(MTYPE) = 'mCVT'                                             ..
      ENDIF                                                                 ..
      IF (LGS(21).EQ.2) THEN                                                ..
          MTYPE = MTYPE + 1                                                 ..
          ATYPE(MTYPE) = '  US'                                             ..
      ENDIF                                                             0929YC97
      IF (.NOT. LTUN) GO TO 100
C
C   If LCDSC = FALSE then ZCT (MEPSAG) only
C
      IF (.NOT. LCDSC) THEN
          WRITE (FU6, 1000)
C         WRITE (FU6, 1015) swlct(ilct)                                 0708JC00
          IF (LLCG) THEN                                                0625TA02
             WRITE (FU6, 1014) swlct(ilct)                              0625TA02
          ELSE                                                          0625TA02
             WRITE (FU6, 1012)                                          0625TA02
          ENDIF                                                         0625TA02
          WRITE (FU6, 1018)                                             0625TA02
          WRITE (FU6, 1001)
          DO 10 I = 1, NTEMP
                WRITE (FU6, 1400) TEMP(I), KAPW(I), KAPCAG(I), 
     *                            KAPCVT(I), KAPSAG(I)
   10     CONTINUE
      ELSE
          WRITE (FU6, 1010)
C         WRITE (FU6, 1015) swlct(ilct)                                 0708JC00
          IF (LLCG) THEN                                                0625TA02
             WRITE (FU6, 1014) swlct(ilct)                              0625TA02
          ELSE                                                          0625TA02
             WRITE (FU6, 1013)                                          0625TA02
          ENDIF                                                         0625TA02
          WRITE (FU6, 1018)                                             0625TA02
          WRITE (FU6, 1011)
          DO 11 I = 1, NTEMP
                WRITE (FU6, 1050) TEMP(I), KAPW(I), KAPCAG(I), KAPCVT(I)
   11     CONTINUE
      ENDIF
C
C   SCSAG is removed from version 5.0
C
      IF (LCDSC) THEN
C
C   If LLCG = FALSE then only ZCT (MEPSAG) and SCT (CD-SCSAG)
C
        IF (.NOT. LLCG) THEN
            WRITE (FU6, 1020)
            DO 12 I = 1, NTEMP
                  WRITE (FU6, 1050) TEMP(I), KAPSAG(I), KACDSC(I) 
   12       CONTINUE
C
C   If LLCG = TRUE then ZCT (MEPSAG), SCT (CD-SCSAG), and LCG3
C
        ELSEIF (LLCG) THEN
                WRITE (FU6, 1030)
                DO 13 I = 1, NTEMP
                      WRITE (FU6, 1050) TEMP(I), KAPSAG(I), 
     *                                  KACDSC(I), KAPLCG(I)
   13           CONTINUE
                WRITE (FU6, 1031)                                       1106YL92
                DO 133 I = 1, NTEMP                                     1106YL92
                      WRITE (FU6, 1050) TEMP(I), KACOMT(I),             1106YL92
     *                                  KAMOMT(I)                       1106YL92
  133           CONTINUE
        ENDIF
      ENDIF
C
      IF (LCDSC) THEN                                                   0625TA02
         IF (.NOT. LLCG) THEN                                           0625TA02
            WRITE (FU6, 1027)                                           0625TA02
            DO 14 I = 1, NTEMP                                          0625TA02
                  WRITE (FU6,1050) TEMP(I),KNTZCT(I),KNTSCT(I)          0625TA02
   14       CONTINUE                                                    0625TA02
         ELSEIF (LLCG) THEN                                             0625TA02
            WRITE (FU6, 1028)                                           0625TA02
            DO 15 I = 1, NTEMP                                          0625TA02
                  WRITE (FU6,1050) TEMP(I),KNTZCT(I),KNTSCT(I),         0625TA02
     *                                     KNTLCT(I),KNTMOT(I)          0625TA02
   15       CONTINUE                                                    0625TA02
         ENDIF                                                          0625TA02
      ENDIF                                                             0625TA02
C 
C
  100 CONTINUE
C
C*******************************************************************************
C
C   If there is no saddle point (LGS(1) = 0) then the TST rates are zero
C
C*******************************************************************************
C
      IF (LGS(1).EQ.0) WRITE (FU6,1100)
C
C*******************************************************************************
C     COMPUTE AND OUTPUT FINAL RATES VS. TEMPERATURE
C
C In the case of external field present.
C       CASE                           FW RATE        REV  RATE
C  1 adsorbed reac, 1 ad prod.          s-1             s-1
C  1 ad. react.   , 1 gas prod.         s-1         cm**3/mol-s-1
C  1 gas react.   , 1 ad. prod.      cm**3/mol-s-1      s-1
C  2 ad. react.   , 1 gas prod.         s-1         cm**3/mol-s-1
C  1 gas+1 ad. re., 2 ad. prod.      cm**3/mol-s-1      s-1
C  1 gas+1 ad. re., 1 ad.+1gas p     cm**3/mol-s-1  cm**3/mol-s-1
C
C*******************************************************************************C
      LGS16 = 2-LGS(16)
      DO 90 IPASS = 1, LGS16
         IF (IPASS.EQ.1) THEN
            IF(LGS(6).GT.2 ) THEN
               IF(ICODE(1).GT.0 .AND. LGS(34).NE.0) THEN                7/27T88
                  WRITE (FU6,1150)
               ELSE
                  WRITE (FU6,1200)
               ENDIF                                                       ..
            ELSE
               IF(ICODE(1).LT.0 .AND. ICODE(2).LT.0) THEN
                  WRITE (FU6,1200)
               ELSE                                                        ..
                  WRITE (FU6,1150)
               ENDIF                                                       ..
            ENDIF
         ELSE
            IF(LGS(6).EQ.2.OR.LGS(6).EQ.4) THEN                         7/27T88
               IF(ICODE(3).GT.0 .AND. LGS(34).NE.0) THEN
                  WRITE (FU6,1500) LGS(17)
               ELSE
                  WRITE (FU6,1250) LGS(17)
               ENDIF
            ELSE
               IF(ICODE(3).LT.0 .AND. ICODE(4).LT.0) THEN
                  WRITE (FU6,1250) LGS(17)
               ELSE
                  WRITE (FU6,1500) LGS(17)
               ENDIF
            ENDIF
         ENDIF
C
C   First, set up the TST, TST/W, TST/CAG, CVT, ICVT, MUVT, and US rates
C   but only print the TST, TST/W, TST/CAG, and CVT rates first.
C
         if (ivic.eq.2.and.LZOC)                                        0606YC98
     *             write (fu6,*) ' TST stands for Generalized TST rate'
         IF (NFCUS.EQ.0) THEN                                           0929YC97
               WRITE (FU6,1300)                                         0929YC97
         ELSE                                                           0929YC97
               WRITE (FU6,1301)                                         0929YC97
         ENDIF    
         DO 20 IT = 1, NTEMP
            CONX = CONF(IT)
            IF (IPASS.EQ.2) CONX = CONR(IT)
            CVTX = CVTF(IT)
            IF (IPASS.EQ.2) CVTX = CVTR(IT)
C           CIVTX = CIVTF(IT)
C           IF (IPASS.EQ.2) CIVTX = CIVTR(IT)
            CONW(IT) = CONX*KAPW(IT)
            CONCAG(IT) = CONX*KAPCAG(IT)
            CVTCAG(IT) = CVTX*KAPCVT(IT)
C           CIVT(IT) = CIVTX
            CMUVTX = CMUVTF(IT)
            IF (IPASS.EQ.2) CMUVTX = CMUVTR(IT)
            CMVTCA(IT) = CMUVTX
            CUSVTX = CUSVTF(IT)
            IF (IPASS.EQ.2) CUSVTX = CUSVTR(IT)
            CUVTCA(IT) = CUSVTX
            CUSTX = CUSTF(IT)                                           0929YC97
            IF (IPASS.EQ.2) CUSTX = CUSTR(IT)                           0929YC97
            CUST(IT) = CUSTX*KAPCVT(IT)                                 0929YC97
            IF (NFCUS.EQ.0) THEN                                        0929YC97
            WRITE (FU6,1400) TEMP(IT), CONX, CONW(IT), CONCAG(IT), CVTX 0929YC97
            ELSE                                                        0929YC97
            WRITE (FU6,1400) TEMP(IT), CONX, CONW(IT), CONCAG(IT), CVTX,0929YC97
     *            CUSTX                                                 0929YC97
            ENDIF                                                       0929YC97
            CAKBT(IT,1) = CONCAG(IT)                                    1106YL92
            CAKBT(IT,2) = CVTCAG(IT)                                    1106YL92
            ILEVEL = 2                                                  0929YC97
            IF (NFCUS.NE.0) THEN                                        0929YC97
               ILEVEL = ILEVEL + 1                                      0929YC97
               CAKBT(IT,ILEVEL) =  CUST(IT)                             0929YC97
            ENDIF                                                       0929YC97
C           IF (LGS(20).NE.0) THEN                                      0929YC97
C              ILEVEL = ILEVEL + 1                                      0929YC97
C              CAKBT(IT,ILEVEL) = CIVT(IT)                              0929YC97
C           ENDIF                                                       0929YC97
            IF (LGS(21).GE.1) THEN                                      0929YC97
               ILEVEL = ILEVEL + 1                                      0929YC97
               CAKBT(IT,ILEVEL) = CMVTCA(IT)                            0929YC97
            ENDIF                                                       0929YC97
            IF (LGS(21).EQ.2) THEN                                      0929YC97
               ILEVEL = ILEVEL + 1                                      0929YC97
               CAKBT(IT,ILEVEL) = CUVTCA(IT)                            0929YC97
            ENDIF                                                       0929YC97
   20    CONTINUE
C
C   Write out the CVT/CAG, ICVT, MUVT, and the US rates
C
         IF (NFCUS.EQ.0) THEN                                           0929YC97
         WRITE (FU6,1310) ATYPE(2)//'/CAG',(ATYPE(ITYPE),ITYPE=3,MTYPE) 1106YL92
         ELSE                                                           0929YC97
              WRITE (FU6,1315) ATYPE(2)//'/CAG',ATYPE(3)//'/CAG',       0929YC97
     *             (ATYPE(ITYPE),ITYPE=4,MTYPE)                         0929YC97
         ENDIF                                                          0929YC97
         IF (MTYPE .LT.6) WRITE(FU6,*)
         DO 21 IT = 1, NTEMP                                            1106YL92
            WRITE (FU6,1400) TEMP(IT),(CAKBT(IT,ITYPE),ITYPE=2,MTYPE)   1106YL92
  21     CONTINUE                                                       1106YL92
C
C*************     ZCT (MEPSAG)
C
C   If LTUN = TRUE then ZCT rates are calculated; print k(ZCT)
C
         IF (LTUN) THEN
         WRITE (FU6,1320) (ATYPE(ITYPE)//'/ZCT',ITYPE=1,MTYPE)          0618WH94
         IF (MTYPE .LT.6) WRITE(FU6,*)                                  0929YC97
         DO 30 IT = 1, NTEMP
            DO 31 ITYPE = 1, MTYPE                                      1106YL92
               ZCTKT(IT,ITYPE) = CAKBT(IT,ITYPE)*KAPSAG(IT)             1106YL92
   31       CONTINUE                                                    1106YL92
            WRITE (FU6,1400) TEMP(IT),(ZCTKT(IT,ITYPE),ITYPE=1,MTYPE)   1106YL92
   30    CONTINUE
         WRITE (FU6,1350) (ATYPE(ITYPE)//'/ZCTnt',ITYPE=1,MTYPE)        0625TA02
         IF (MTYPE .LT.6) WRITE(FU6,*)                                  0625TA02
         DO 32 IT = 1, NTEMP                                            0625TA02
            DO 33 ITYPE = 1, MTYPE                                      0625TA02
               RZCTNT(IT,ITYPE) = CAKBT(IT,ITYPE)*KNTZCT(IT)            0625TA02
   33       CONTINUE                                                    0625TA02
            WRITE (FU6,1400) TEMP(IT),(RZCTNT(IT,ITYPE),ITYPE=1,MTYPE)  0625TA02
   32    CONTINUE                                                       0625TA02
         END IF                                                         5/10DL90
C
C*************     SCT (CD-SCSAG)
C
C   If LCDSC = TRUE then SCT rates are calculated; print k(SCT)
C
         IF (LCDSC) THEN                                                8/26YL91
            WRITE (FU6,1330) (ATYPE(ITYPE)//'/SCT',ITYPE=1,MTYPE)       0618WH94
            IF (MTYPE .LT.6) WRITE(FU6,*)                               0929YC97
            DO 41 IT = 1, NTEMP                                         0826YL91
               DO 411 ITYPE = 1, MTYPE                                  1106YL92
                  CDKT(IT,ITYPE) = CAKBT(IT,ITYPE)*KACDSC(IT)           1106YL92
  411          CONTINUE                                                 1106YL92
               WRITE (FU6,1400) TEMP(IT),(CDKT(IT,ITYPE),ITYPE=1,MTYPE) 1106YL92
   41       CONTINUE                                                         ..
         WRITE (FU6,1350) (ATYPE(ITYPE)//'/SCTnt',ITYPE=1,MTYPE)        0625TA02
         IF (MTYPE .LT.6) WRITE(FU6,*)                                  0625TA02
         DO 34 IT = 1, NTEMP                                            0625TA02
            DO 35 ITYPE = 1, MTYPE                                      0625TA02
               RSCTNT(IT,ITYPE) = CAKBT(IT,ITYPE)*KNTSCT(IT)            0625TA02
   35       CONTINUE                                                    0625TA02
            WRITE (FU6,1400) TEMP(IT),(RSCTNT(IT,ITYPE),ITYPE=1,MTYPE)  0625TA02
   34    CONTINUE                                                       0625TA02
         ENDIF                                                          8/26YL91
C
C*************     LCG3
C
C   If LLCG = TRUE then LCG3 rates are calculated; print k(LCG3)
C
         IF (LLCG) THEN           
             WRITE (FU6,1340) (ATYPE(ITYPE)//'/LCT',ITYPE=1,MTYPE)      0618WH94
             IF (MTYPE .LT.6) WRITE(FU6,*)                              0929YC97
             DO 42 IT = 1,NTEMP   
               DO 421 ITYPE = 1, MTYPE                                  1106YL92
                  CLKT(IT,ITYPE) = CAKBT(IT,ITYPE)*KAPLCG(IT)           1106YL92
  421          CONTINUE                                                 1106YL92
               WRITE (FU6,1400) TEMP(IT),(CLKT(IT,ITYPE),ITYPE=1,MTYPE) 1106YL92
   42        CONTINUE                                                   5/10DL90
         WRITE (FU6,1350) (ATYPE(ITYPE)//'/LCTnt',ITYPE=1,MTYPE)        0625TA02
         IF (MTYPE .LT.6) WRITE(FU6,*)                                  0625TA02
         DO 36 IT = 1, NTEMP                                            0625TA02
            DO 37 ITYPE = 1, MTYPE                                      0625TA02
               RLCTNT(IT,ITYPE) = CAKBT(IT,ITYPE)*KNTLCT(IT)            0625TA02
   37       CONTINUE                                                    0625TA02
            WRITE (FU6,1400) TEMP(IT),(RLCTNT(IT,ITYPE),ITYPE=1,MTYPE)  0625TA02
   36    CONTINUE                                                       0625TA02
C
             WRITE (FU6,1340) (ATYPE(ITYPE)//'/COMT',ITYPE=1,MTYPE)     1106YL92
             IF (MTYPE .LT.6) WRITE(FU6,*)                              0929YC97
             DO 422 IT = 1,NTEMP   
               DO 423 ITYPE = 1, MTYPE                                  1106YL92
                  COMTKT(IT,ITYPE) = CAKBT(IT,ITYPE)*KACOMT(IT)         1106YL92
  423          CONTINUE                                                 1106YL92
               WRITE (FU6,1400) TEMP(IT),(COMTKT(IT,ITYPE),             1106YL92
     *                          ITYPE=1,MTYPE)                          1106YL92
  422      CONTINUE                                                     5/10DL90
C
             WRITE (FU6,1340) (ATYPE(ITYPE)//'/muOMT',ITYPE=1,MTYPE)    1106YL92
             IF (MTYPE .LT.6) WRITE(FU6,*)                              0929YC97
             DO 424 IT = 1,NTEMP   
               DO 425 ITYPE = 1, MTYPE                                  1106YL92
                  UOMTKT(IT,ITYPE) = CAKBT(IT,ITYPE)*KAMOMT(IT)         1106YL92
  425          CONTINUE                                                 1106YL92
               WRITE (FU6,1400) TEMP(IT),(UOMTKT(IT,ITYPE),             1106YL92
     *                          ITYPE=1,MTYPE)                          1106YL92
  424      CONTINUE                                                     5/10DL90
         WRITE (FU6,1350) (ATYPE(ITYPE)//'/mOTnt',ITYPE=1,MTYPE)        0625TA02
         IF (MTYPE .LT.6) WRITE(FU6,*)                                  0625TA02
         DO 38 IT = 1, NTEMP                                            0625TA02
            DO 39 ITYPE = 1, MTYPE                                      0625TA02
               RMOTNT(IT,ITYPE) = CAKBT(IT,ITYPE)*KNTMOT(IT)            0625TA02
   39       CONTINUE                                                    0625TA02
            WRITE (FU6,1400) TEMP(IT),(RMOTNT(IT,ITYPE),ITYPE=1,MTYPE)  0625TA02
   38    CONTINUE                                                       0625TA02
         END IF                                                         5/10DL90
C
C*************     SCSAG has been removed from version 5.0
C
C*******************************************************************************
C
C     ACTIVATION ENERGIES BY TWO POINT FITS TO K=A*EXP(-EA/RT)
C
C*******************************************************************************
C
         IF (NPAIR.EQ.0) GO TO 80
         WRITE (FU6,1650)
         IF (NFCUS.EQ.0) THEN                                           0929YC97
           WRITE (FU6,1700)    
         ELSE                                                           0929YC97
           WRITE (FU6,1701)                                             0929YC97
         ENDIF                                                          0929YC97
C
C   TST, TST/W, CVT, and CUS  ACTIVATION ENERGIES
C
         DO 50 IPR = 1, NPAIR
            I1 = IT1(IPR)
            I2 = IT2(IPR)
C
C   Calculate the average of the upper and lower temperatures 
C
            TAVE(IPR) = 2.0D0/(1.0D0/TEMP(I1)+1.0D0/TEMP(I2))
C
            B1 = BETA(I1)
            B2 = BETA(I2)
            E1 = EACT(B1,B2,CONF(I1),CONF(I2))
            IF (IPASS.EQ.2) E1 = EACT(B1,B2,CONR(I1),CONR(I2))
            IF (LTUN) THEN
               E2 = EACT(B1,B2,CONW(I1),CONW(I2))
            END IF
            E3 = EACT(B1,B2,CVTF(I1),CVTF(I2))
            IF (IPASS.EQ.2) E3 = EACT(B1,B2,CVTR(I1),CVTR(I2))
            IF (NFCUS.EQ.1) THEN
               E4 = EACT(B1,B2,CUSTF(I1),CUSTF(I2))
               IF (IPASS.EQ.2) E4 = EACT(B1,B2,CUSTR(I1),CUSTR(I2))
            ENDIF
            IF (NFCUS.EQ.0) THEN
              WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),
     *                       E1, E2, E3
            ELSE
              WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),
     *                       E1, E2, E3, E4
            ENDIF
   50    CONTINUE
C
C   TST/CAG, CVT/CAG, CUS/CAG, ICVT, and MUVT activation energies
C
         IF (LTUN) THEN
         IPRBG = 1
         IF (MTYPE.GT.4) THEN
           IPRED = 4
         ELSE
           IPRED = MTYPE
         ENDIF
   49    IF (IPRBG.EQ.1) THEN
           IF (NFCUS.EQ.1) THEN
              WRITE (FU6,1800) ATYPE(1)//'/CAG',ATYPE(2)//'/CAG',
     *                   ATYPE(3)//'/CAG',
     *                   (ATYPE(ITYPE),ITYPE=4,IPRED)
           ELSE
              WRITE (FU6,1800) ATYPE(1)//'/CAG',ATYPE(2)//'/CAG',
     *                   (ATYPE(ITYPE),ITYPE=3,IPRED)
           ENDIF
         ELSE
              WRITE (FU6,1800) (ATYPE(ITYPE),ITYPE=IPRBG,IPRED)
         ENDIF
         IF ((IPRED-IPRBG).LT.3) WRITE(FU6,*)
         DO 51 IPR = 1, NPAIR
            I1 = IT1(IPR)
            I2 = IT2(IPR)
            B1 = BETA(I1)
            B2 = BETA(I2)
            DO 48 ITYPE = IPRBG,IPRED                                   0924YC97
               C1 = CAKBT(I1,ITYPE)                                     1106YL92
               C2 = CAKBT(I2,ITYPE)                                     1106YL92
               EACTVE(ITYPE)=EACT(B1,B2,C1,C2)                          1106YL92
   48       CONTINUE                                                    1106YL92
            WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),             1106YL92
     *                       (EACTVE(ITYPE),ITYPE=IPRBG,IPRED)          0927YC97
   51    CONTINUE
         IF (IPRED.NE.MTYPE) THEN
             IPRBG = 5
             IPRED = MTYPE
             GOTO 49
         ENDIF
         ENDIF
C
C*************     ZCT (MEPSAG) Activation Energies
C
         IF (LMEP) THEN 
         IPRBG = 1
         IF (MTYPE.GT.4) THEN
           IPRED = 4
         ELSE
           IPRED = MTYPE
         ENDIF
   59    WRITE (FU6,1800) (ATYPE(ITYPE)//'/ZCT',ITYPE=IPRBG,IPRED)
         IF ((IPRED-IPRBG).LT.3) WRITE(FU6,*)
         DO 60 IPR = 1, NPAIR
            I1 = IT1(IPR)
            I2 = IT2(IPR)
            B1 = BETA(I1)
            B2 = BETA(I2)
            DO 61 ITYPE = IPRBG,IPRED                                   0924YC97
               C1 = ZCTKT(I1,ITYPE)                                     1106YL92
               C2 = ZCTKT(I2,ITYPE)                                     1106YL92
               EACTVE(ITYPE)=EACT(B1,B2,C1,C2)                          1106YL92
   61       CONTINUE                                                    1106YL92
            WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),             1106YL92
     *                       (EACTVE(ITYPE),ITYPE=IPRBG,IPRED)          0927YC97
   60    CONTINUE
         IF (IPRED.NE.MTYPE) THEN
             IPRBG = 5
             IPRED = MTYPE
             GOTO 59
         ENDIF
         ENDIF                          
C
C*************     SCT (CD-SCSAG) Activation Energies
C
         IF (LCDSC) THEN                                                8/26YL91
         IPRBG = 1                                                      0927YC97
         IF (MTYPE.GT.4) THEN                                           0927YC97
           IPRED = 4                                                    0927YC97
         ELSE                                                           0927YC97
           IPRED = MTYPE                                                0927YC97
         ENDIF                                                          0927YC97
   69    WRITE (FU6,1810) (ATYPE(ITYPE)//'/SCT',ITYPE=IPRBG,IPRED)      0927YC97
         IF ((IPRED-IPRBG).LT.3) WRITE(FU6,*)                           0927YC97
         DO 75 IPR = 1, NPAIR                                           0826YL91
               I1 = IT1(IPR)                                              ..
               I2 = IT2(IPR)                                              ..
               B1 = BETA(I1)                                              ..
               B2 = BETA(I2)                                              ..
               DO 76 ITYPE = IPRBG, IPRED
                  C1 = CDKT(I1,ITYPE)                                   1106YL92
                  C2 = CDKT(I2,ITYPE)                                   1106YL92
                  EACTVE(ITYPE)=EACT(B1,B2,C1,C2)                       1106YL92
   76          CONTINUE                                                 1106YL92
            WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),             1106YL92
     *                       (EACTVE(ITYPE),ITYPE=IPRBG,IPRED)
   75       CONTINUE
         IF (IPRED.NE.MTYPE) THEN                                       0927YC97
             IPRBG = 5                                                  0927YC97
             IPRED = MTYPE                                              0927YC97
             GOTO 69                                                    0927YC97
         ENDIF                                                          0927YC97
         ENDIF                                                          8/26YL91
C
C*************     LCG3 Activation Energies
C
         IF (LLCG) THEN 
         IPRBG = 1                                                      0927YC97
         IF (MTYPE.GT.4) THEN                                           0927YC97
           IPRED = 4                                                    0927YC97
         ELSE                                                           0927YC97
           IPRED = MTYPE                                                0927YC97
         ENDIF                                                          0927YC97
   79    WRITE (FU6,1820) (ATYPE(ITYPE)//'/LCT',ITYPE=IPRBG,IPRED)      0927YC97
         IF ((IPRED-IPRBG).LT.3) WRITE(FU6,*)                           0927YC97
         DO 72 IPR = 1,NPAIR                                            5/10DL90
           I1 = IT1(IPR)                                                5/10DL90
           I2 = IT2(IPR)                                                5/10DL90
           B1 = BETA(I1)                                                5/10DL90
           B2 = BETA(I2)                                                5/10DL90
               DO 78 ITYPE = IPRBG, IPRED                               0927YC97
                  C1 = CLKT(I1,ITYPE)                                   1106YL92
                  C2 = CLKT(I2,ITYPE)                                   1106YL92
                  EACTVE(ITYPE)=EACT(B1,B2,C1,C2)                       1106YL92
   78          CONTINUE                                                 1106YL92
            WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),             1106YL92
     *                       (EACTVE(ITYPE),ITYPE=IPRBG,IPRED)          0927YC97
   72    CONTINUE                                                       5/10DL90
         IF (IPRED.NE.MTYPE) THEN                                       0927YC97
             IPRBG = 5                                                  0927YC97
             IPRED = MTYPE                                              0927YC97
             GOTO 79                                                    0927YC97
         ENDIF                                                          0927YC97
C
         IPRBG = 1                                                      0927YC97
         IF (MTYPE.GT.4) THEN                                           0927YC97
           IPRED = 4                                                    0927YC97
         ELSE                                                           0927YC97
           IPRED = MTYPE                                                0927YC97
         ENDIF                                                          0927YC97
  791    WRITE (FU6,1820) (ATYPE(ITYPE)//'/COMT',ITYPE=IPRBG,IPRED)     0927YC97
         IF ((IPRED-IPRBG).LT.3) WRITE(FU6,*)                           0927YC97
         DO 721 IPR = 1,NPAIR                                           1106YL92
           I1 = IT1(IPR)                                                1106YL92
           I2 = IT2(IPR)                                                1106YL92
           B1 = BETA(I1)                                                1106YL92
           B2 = BETA(I2)                                                1106YL92
               DO 781 ITYPE = IPRBG,IPRED                               0927YC97
                  C1 = COMTKT(I1,ITYPE)                                 1106YL92
                  C2 = COMTKT(I2,ITYPE)                                 1106YL92
                  EACTVE(ITYPE)=EACT(B1,B2,C1,C2)                       1106YL92
  781          CONTINUE                                                 1106YL92
            WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),             1106YL92
     *                       (EACTVE(ITYPE),ITYPE=IPRBG,IPRED)          0927YC97
  721    CONTINUE                                                       1106YL92
         IF (IPRED.NE.MTYPE) THEN                                       0927YC97
             IPRBG = 5                                                  0927YC97
             IPRED = MTYPE                                              0927YC97
             GOTO 791                                                   0927YC97
         ENDIF                                                          0927YC97
C
         IPRBG = 1                                                      0927YC97
         IF (MTYPE.GT.4) THEN                                           0927YC97
           IPRED = 4                                                    0927YC97
         ELSE                                                           0927YC97
           IPRED = MTYPE                                                0927YC97
         ENDIF                                                          0927YC97
  792    WRITE (FU6,1820) (ATYPE(ITYPE)//'/muOMT',ITYPE=IPRBG,IPRED)    0927YC97
         IF ((IPRED-IPRBG).LT.3) WRITE(FU6,*)                           0927YC97
         DO 722 IPR = 1,NPAIR                                           1106YL92
           I1 = IT1(IPR)                                                1106YL92
           I2 = IT2(IPR)                                                1106YL92
           B1 = BETA(I1)                                                1106YL92
           B2 = BETA(I2)                                                1106YL92
               DO 782 ITYPE = IPRBG, IPRED                              0927YC97
                  C1 = UOMTKT(I1,ITYPE)                                 1106YL92
                  C2 = UOMTKT(I2,ITYPE)                                 1106YL92
                  EACTVE(ITYPE)=EACT(B1,B2,C1,C2)                       1106YL92
  782          CONTINUE                                                 1106YL92
            WRITE (FU6,1850) TEMP(I1), TEMP(I2), TAVE(IPR),             1106YL92
     *                       (EACTVE(ITYPE),ITYPE=IPRBG,IPRED)          0927YC97
  722    CONTINUE                                                       1106YL92
         IF (IPRED.NE.MTYPE) THEN                                       0927YC97
             IPRBG = 5                                                  0927YC97
             IPRED = MTYPE                                              0927YC97
             GOTO 792                                                   0927YC97
         ENDIF                                                          0927YC97
         ENDIF                                                          5/10DL90
C
C     WRITE OUT SUMMARY OF FORWARD RATES TO FOR015
C
   80    IF (IPASS .EQ. 1) CALL TABLE
   90 CONTINUE
C
C     PROPERTIES OF VARIOUS BOTTLENECKS AND
C     DETAILED ANALYSIS OF RATIO OF CONVEVTIONAL TO CVT RATES
C
      IF (NTRAT.EQ.0) GOTO 270
      NTRAT1 = NTRAT+1
      IF (NTRAT.GT.0) GO TO 110
C
C     DO ANALYSIS AT ALL TEMPERATURES
C
      NTRAT = NTEMP
      NTRAT1 = NTEMP+1
      DO 101 I = 1, NTEMP
         ITR(I+1) = I
  101 CONTINUE
C
  110 CONTINUE
C
      N3M7 = NF(5)                                                      1016WH92
C
      IF (LGS(8).LE.0) THEN
         CALL DATTIM (FU14)
C        CALL TITLE (1,FU14,1)
         CALL WTITLE(FU14)
      ELSE
C        CALL TITLE (1,FU14,1)
C        CALL TITLE (1,FU14,2)
         CALL DATTIM (FU14)
         CALL WTITLE(FU14)
      ENDIF
      WRITE (FU14,1950)
      WRITE (FU6,1950)
C
C     FIRST FIND AND OUTPUT V,VA,I, AND FREQUENCIES AT S.P. AND S*CVT
C
      IF(LGS(34) .EQ. 0) THEN
        IF(IUNIT6.EQ.1) THEN                                            0405JZ07
         WRITE (FU6,2100)
         WRITE (FU14,2100)
        ELSE
         WRITE (FU6,2102)
         WRITE (FU14,2102)
        ENDIF
      ELSE
        IF(IUNIT6.EQ.1) THEN                                            0405JZ07
         WRITE (FU6,2110)
         WRITE (FU14,2110)
        ELSE
         WRITE (FU6,2112)
         WRITE (FU14,2112)
        ENDIF
      ENDIF
      IF (LGS(1).EQ.0) GO TO 140
C             
C     OUTPUT S.P. RESULTS -- I.E., THOSE FOR CONVEVTIONAL TST
C
      DO 130 I = 1, N3M7
         IWETS(I) = NINT(WETS(I,NSHLF)*AUTOCM)                          6/22T90
         IF (IWETS(I) .GE. 0) THEN
            IMA(I) = ' '
         ELSE
            IWETS(I) = -IWETS(I)
            IMA(I) = 'i'
         ENDIF
  130 CONTINUE
      IF (LGS(34) .EQ. 0) THEN
         WRITE (FU6,2250) SSUBI(NSHLF)/GUFAC6,VCLAS(NSHLF)*CKCAL,       0405JZ07
     *                    VADIB(NSHLF)*CKCAL, FMITS(NSHLF),
     *                    (IWETS(I),IMA(I),I=N3M7,1,-1)
      ELSE
         WRITE (FU6,2260) SSUBI(NSHLF)/GUFAC6,VCLAS(NSHLF)*CKCAL,       0405JZ07
     *                    VADIB(NSHLF)*CKCAL,
     *                  (IWETS(I),IMA(I),I=N3M7,1,-1)
      ENDIF
      IF(LGS(34) .EQ. 0) THEN
C
         WRITE (FU14,2200) SSUBI(NSHLF)/GUFAC6,VCLAS(NSHLF)*CKCAL,      0405JZ07
     *                      VADIB(NSHLF)*CKCAL,
     *                      FMITS(NSHLF),(IWETS(I),IMA(I),I=N3M7,1,-1)
      ELSE
         WRITE (FU14,2260) SSUBI(NSHLF)/GUFAC6,VCLAS(NSHLF)*CKCAL,      0405JZ07
     *                   VADIB(NSHLF)*CKCAL,
     *                   (IWETS(I),IMA(I),I=N3M7,1,-1)
      ENDIF
  140 CONTINUE
C
C     NOW DETERMINE THESE QUANTITIES AT S*CVT(T) BY 5 POINT FITS
C
      DO 250 IT = 1, NTRAT1
C
C     ON FIRST PASS, GET PROPERTIES AT VAG (TEMP=0.0K)
C
         I1 = ITR(IT)
C
C     FIND 5 NEAREST S POINTS TO S*CVT(T)
C
         IF (I1.EQ.0) THEN
           SVAL = SVAGM
         ELSE
           SVAL = SCVT(I1)
         ENDIF
         DO 160 IS = 1, LSAVE
            IF (SVAL.LT.SSUBI(IS)) GO TO 170
  160    CONTINUE
         STOP 'FINOUT 1'
  170    IF ((SSUBI(IS)-SVAL).GT.(SVAL-SSUBI(IS-1))) IS = IS-1
         J = IS-3
         DO 180 I = 1, 5
            SPT(I) = SSUBI(J+I)
            FPT(I) = VCLAS(J+I)
  180    CONTINUE
         CALL FIVPT (0,0,SPT,FPT,SVAL,FVAL)
         VCX(IT) = FVAL
         DO 190 I = 1, 5
            FPT(I) = VADIB(J+I)
  190    CONTINUE
         CALL FIVPT (0,0,SPT,FPT,SVAL,FVAL)
         VAX(IT) = FVAL
         IF (LGS(34) .EQ. 0) THEN                                       11/20T87
            DO 200 I = 1, 5
               FPT(I) = FMITS(J+I)
  200       CONTINUE
            CALL FIVPT (0,0,SPT,FPT,SVAL,FVAL)
            FMX(IT) = FVAL
         ENDIF
         DO 220 K = 1, N3M7
            DO 210 I = 1, 5
               FPT(I) = WETS(K,J+I)
  210       CONTINUE
            CALL FIVPT (0,0,SPT,FPT,SVAL,FVAL)
            WEX(K) = FVAL
  220    CONTINUE
         TOUT = 0.0D0
         DO 230 I = 1, N3M7
            IWEX(I) = NINT(WEX(I)*AUTOCM)                               23/10/90VM
            IF (IWEX(I) .GE. 0) THEN
               IMA(I) = ' '
            ELSE
               IWEX(I) = -IWEX(I)
               IMA(I) = 'i'
            ENDIF
  230    CONTINUE
         IF (I1.EQ.0) GO TO 240
         TOUT = TEMP(I1)
  240    IF (LGS(34) .EQ. 0) THEN
           WRITE(FU6,2300) TOUT,SVAL/GUFAC6,VCX(IT)*CKCAL,VAX(IT)*CKCAL,0405JZ07
     *                      FMX(IT),(IWEX(I),IMA(I),I=N3M7,1,-1)
         ELSE
           WRITE(FU6,2310) TOUT,SVAL/GUFAC6,VCX(IT)*CKCAL,VAX(IT)*CKCAL,0405JZ07
     *                      (IWEX(I),IMA(I),I=N3M7,1,-1)
         ENDIF
         IF (LGS(34) .EQ. 0) THEN
           WRITE(FU14,2400)TOUT,SVAL/GUFAC6,VCX(IT)*CKCAL,VAX(IT)*CKCAL,0405JZ07
     *                         FMX(IT),(IWEX(I),IMA(I),I=N3M7,1,-1)
         ELSE
           WRITE(FU14,2360)TOUT,SVAL/GUFAC6,VCX(IT)*CKCAL,VAX(IT)*CKCAL,0405JZ07
     *                      (IWEX(I),IMA(I),I=N3M7,1,-1)
         ENDIF
  250 CONTINUE
      IF (LGS(1).NE. 0) THEN
         WRITE (FU6,2000)
         WRITE (FU6,2450)
         DO 260 IT = 2, NTRAT1
            I1 = ITR(IT)
            RTOT(IT) = CONF(I1)/CVTF(I1)
            RBVC = EXP(-(VCLAS(NSHLF)-VCX(IT))*BETA(I1))
            IF (LGS(34).NE. 0) THEN
               RROT = 1.0D0
            ELSE
               RROT = FMITS(NSHLF)/FMX(IT)
            ENDIF
            IF (ICODE(5).NE.3) RROT = SQRT(RROT)
            RVIB = RTOT(IT)/(RBVC*RROT)
            RVR(IT) = RVIB*RROT
            WRITE (FU6,2500) TEMP(I1), RBVC, RVIB, RROT
  260    CONTINUE
         WRITE (FU6, 2460)
         DO 261 IT = 2, NTRAT1
                I1 = ITR(IT)
                WRITE (FU6, 2510) TEMP(I1), RVR(IT), RTOT(IT)
  261    CONTINUE
         WRITE (FU6, 2470)
         DO 262 IT = 2, NTRAT1
                I1 = ITR(IT)
                RBVA = EXP(-(VADIB(NSHLF)-VAX(IT))*BETA(I1))
                RENT = RTOT(IT)/RBVA
                WRITE (FU6, 2500) TEMP(I1), RBVA, RENT
  262    CONTINUE
C
      ENDIF
C
  270 CONTINUE
      IF (LGS(16) .EQ. 1) RETURN
      IF (LGS(6) .EQ. 1 .OR. LGS(6) .EQ. 4) THEN                        1216WH92
         WRITE(FU6,2550)                                                1216WH92
      ELSEIF (LGS(6) .EQ. 2) THEN                                       1216WH92
         WRITE(FU6,2551)                                                1216WH92
      ELSEIF (LGS(6) .EQ. 3) THEN                                       1216WH92
         WRITE(FU6,2552)                                                1216WH92
      ENDIF                                                             1216WH92
      WRITE(FU6,2555)                                                   1216WH92
      DO 280 IT = 1, NTEMP
         EQCONF = CVTF(IT)/CVTR(IT)
         EQCONR = 1.0D0/EQCONF
         WRITE (FU6,2560) TEMP(IT),EQCONF,EQCONR
  280 CONTINUE
C
      RETURN
C
 1000 FORMAT(/,3X,28(1H*),17H  Final results  ,28(1H*))
 1001 FORMAT(//31X,17H  kappa factors  ,/33X,13(1H-),//5X,4HT(K),7X,
     *       5HTST/W,6X,7HTST/CAG,6X,7HCVT/CAG,8X,3HZCT/)
 1010 FORMAT(/,3X,28(1H*),17H  Final results  ,28(1H*))
 1011 FORMAT(//31X,17H  Kappa factors  ,/33X,13(1H-),//5X,4HT(K),7X,
     *       5HTST/W,6X,7HTST/CAG,6X,7HCVT/CAG/)
 1012 FORMAT(/3X,'In the following output:'/,                           0625TA02
     *       /3X,'   ZCT stands for the MEPSAG method and')             0625TA02
 1013 FORMAT(/3X,'In the following output:'/,                           0625TA02
     *       /3X,'   ZCT stands for the MEPSAG method,',                0625TA02
     *       /3X,'   SCT stands for the CD-SCSAG method, and')          0625TA02
 1014 FORMAT(/3X,'In the following output:'/,                           0625TA02
     *       /3X,'   ZCT stands for the MEPSAG method,',                0625TA02
     *       /3X,'   SCT stands for the CD-SCSAG method,',              0625TA02
     *       /3X,'   LCT stands for the ',a4,' method, and')            0625TA02
C1015 FORMAT(/3X,'In the following output:'/,
C    *       /3X,'   ZCT stands for the MEPSAG method,',
C    *       /3X,'   SCT stands for the CD-SCSAG method,',
C    *       /3X,'   LCT stands for the ',a4,' method, and',            0625TA02
C    *       /3X,'   nt or notunn stands for the non-tunneling ',       0625TA02
C    *           'contributions.')                                      0625TA02
 1018 FORMAT(3X,'   nt or notunn stands for the non-tunneling ',        0625TA02
     *           'contributions.')                                      0625TA02
 1020 FORMAT(//5X,4HT(K),9X,3HZCT,9X,3HSCT,/)                           0618WH94
 1030 FORMAT(//5X,4HT(K),9X,3HZCT,9X,3HSCT,9X,3HLCT,/)                  0618WH94
 1031 FORMAT(//5X,4HT(K),7X,4HCOMT,8X,5HmuOMT,/)                        1216WH92
 1027 FORMAT(//5X,4HT(K),5X,'ZCT-notunn',2X,'SCT-notunn',/)             0625TA02
 1028 FORMAT(//5X,4HT(K),5X,'ZCT-notunn',2X,'SCT-notunn',               0625TA02
     *                   2X,'LCT-notunn',2X,'muOMT-notunn',/)           0625TA02
 1050 FORMAT(1X,F9.2,2X,1P,4E12.4)
 1100 FORMAT(/1X,'Conventional TST rates have been set equal to 1.0',
     */1X,'because there is no saddle point.')                          0618WH94
 1150 FORMAT(//23X,34HForward rates (cm**3/molecule-sec),
     * /,23X,34(1H-))
 1200 FORMAT(//29X,23HForward rates (sec**-1),/29X,23(1H-))
 1250 FORMAT(//25X,20HReverse rates (10**-,I2,8Hsec**-1),/,25X,         1027WH92
     *       30(1H-))
 1300 FORMAT(/3X,4HT(K),6X,3HTST,9X,5HTST/W,6X,7HTST/CAG,7X,3HCVT/)
 1301 FORMAT(/3X,4HT(K),6X,3HTST,9X,5HTST/W,6X,7HTST/CAG,7X,3HCVT,7X,
     *       3HCUS/)
 1315 FORMAT(//3X,4HT(K),4X,A8,4X,A8,2(6X,A4,2X),6X,A4,/)
 1310 FORMAT(//3X,4HT(K),4X,A8,4(6X,A4,2X),/)
 1320 FORMAT(//3X,4HT(K),5(2X,A10),1X,A10,/)
 1330 FORMAT(//3X,4HT(K),5(2X,A10),1X,A10,/)
 1340 FORMAT(//3X,4HT(K),5(2X,A10),1X,A10,/)
 1350 FORMAT(//3X,4HT(K),1X,6(2X,A10),/)                                0625TA02
 1400 FORMAT(1X,F7.2,1P,6E12.4)
 1500 FORMAT(//16X,22H  Reverse rates (10**-,I2,14Hcm**3/molecule,      1027WH92
     *       5H-sec),/,16X,43(1H-))
 1650 FORMAT(//24X,'Activation energies (kcal/mol)',/24X,30(1H-))       1027WH92
 1700 FORMAT(/23X,                    '      -1 -1',
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),8X,3HTST,
     *        6X,5HTST/W,8X,3HCVT,/)
 1701 FORMAT(/23X,                    '      -1 -1',
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),8X,3HTST,
     *        6X,5HTST/W,8X,3HCVT,8X,3HCUS,/)
 1710 FORMAT(/23X,                    '      -1 -1',
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),8X,3HCVT,
     *       4X,7HCVT/CAG,/)
 1711 FORMAT(/23X,                    '      -1 -1',
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),8X,3HCVT,
     *       4X,7HCVT/CAG,7X,4HICVT,/)
 1712 FORMAT(/23X,                    '      -1 -1',
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),8X,3HCVT,
     *       4X,7HCVT/CAG,7X,4HICVT,7X,4HmuVT,/)
 1800 FORMAT(/23X,                    '      -1 -1',                
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),2X,A9,            
     *       2X,A9,2X,A9,2X,A9,/)                                    
 1810 FORMAT(/23X,                    '      -1 -1',                 
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),2X,A9,             
     *       2X,A9,2X,A9,2X,A9,/)                                    
 1820 FORMAT(/23X,                    '      -1 -1',                    0707WH94
     * /3X,7HLower T,2X,7HUpper T,4X,9H(Ave T  ),1X,A10,
     *       1X,A10,1X,A10,1X,A10,/)                                       
 1850 FORMAT(1X,2F9.2,F10.2,3X,4F11.4)                                  1204WH92
 1950 FORMAT(//24X,35HBottleneck properties (TST and CVT),/,24X,35(1H-))1204WH92
 2000 FORMAT(//25X,30HConventional TST to CVT ratios,/,25X,30(1H-))     1223WH92
 2100 FORMAT(/6X,1HT,8X,1Hs,8X,4HVMEP,8X,4HVa^G,11X,1HI,10X,            06/96ELC
     *       11Hfrequencies,/,5X,3H(K),5X,6H(bohr),4X,6H(kcal),         0301GL92
     *       6X,6H(kcal),7X,6H(a.u.),8X,8H(cm**-1),/)
 2102 FORMAT(/6X,1HT,8X,1Hs,8X,4HVMEP,8X,4HVa^G,11X,1HI,10X,            0405JZ07
     *       11Hfrequencies,/,5X,3H(K),3X,10H(angstrom),2X,6H(kcal),  
     *       6X,6H(kcal),7X,6H(a.u.),8X,8H(cm**-1),/)
 2110 FORMAT(/6X,1HT,8X,1Hs,8X,4HVMEP,8X,4HVa^G,7X,                     06/96ELC
     *       11Hfrequencies,/,5X,3H(K),5X,6H(bohr),4X,6H(kcal),         0301GL92
     *       6X,6H(kcal),5X,8H(cm**-1),/)                               0301GL92
 2112 FORMAT(/6X,1HT,8X,1Hs,8X,4HVMEP,8X,4HVa^G,7X,                     0405JZ07
     *       11Hfrequencies,/,5X,3H(K),3X,10H(angstrom),2X,6H(kcal), 
     *       6X,6H(kcal),5X,8H(cm**-1),/) 
 2200 FORMAT(5X,4HS.P.,1X,F9.4,2F12.6,1PD13.4,3X,3(I4,A1,1X),
     *20(/59X,3(I4,A1,1X)))
 2250 FORMAT(5X,4HS.P.,1X,F9.4,2F12.6,1PD13.4,3X,3(I4,A1,1X),
     *20(/59X,3(I4,A1,1X)))
 2260 FORMAT(5X,4HS.P.,1X,F9.4,2F12.6,3X,5(I4,A1,1X),
     *20(/44X,5(I4,A1,1X)))
 2300 FORMAT(2X,F8.2,F9.4,F12.6,F12.6,1PE13.4,3X,3(I4,A1,1X),
     *20(/59X,3(I4,A1,1X)))
 2310 FORMAT(2X,F8.2,F9.4,F12.6,F12.6,3X,5(I4,A1,1X),
     *20(/44X,5(I4,A1,1X)))
 2360 FORMAT(2X,F8.2,F9.4,2F12.6,3X,5(I4,A1,1X),
     *20(/44X,5(I4,A1,1X)))
 2400 FORMAT(2X,F8.2,F9.4,2F12.6,1PD13.3,3X,3(I4,A1,1X),
     *20(/59X,3(I4,A1,1X)))
 2450 FORMAT(/5X,4HT(K),7X,14HBoltzmann VMEP,8X,3Hvib,14X,3Hrot/)
 2460 FORMAT(//5X,4HT(K),28X,7Hvib*rot,10X,5Htotal/)
 2470 FORMAT(//5X,4HT(K),7X,14HBoltzmann Va^G,7X,8HEntropic,/)          06/96ELC
 2500 FORMAT(2X,F8.2,1X,F16.6,1X,F16.6,1X,F16.6)
 2510 FORMAT(2X,F8.2,1X,18X,2(F16.6,1X))
 2550 FORMAT(//14X,'Equilibrium constants (unitless)')                  1216WH92
 2551 FORMAT(//11X,'Equilibrium constants (cm**3/molecule)')            1216WH92
 2552 FORMAT(//11X,'Equilibrium constants (molecule/cm**3)')            1216WH92
 2555 FORMAT(1X,60('-'),/5X,4HT(K),17X,7HForward,17X,7HReverse,/)       1216WH92
 2560 FORMAT(2X,F8.2,10X,1P,E15.6,9X,1P,E15.6)
C
      END SUBROUTINE finout
C
C***********************************************************************
C  FIOPEN 
C***********************************************************************
C
      SUBROUTINE fiopen
C
C     This subroutine sorts through the LGS and LGS2 options and calls
C     the subprogram OPENFI to open all the input and output files needed
C     for a particular calculation.  This subprogram does not open the
C     files linked to FORTRAN units fu5 and fu6; these files are opened 
C     in MAIN.
C
C     This subroutine has been rewritten by Gillian Lynch
C     and Wei-Ping Hu  September 30, 1992
C
C     Called by: MAIN
C
C     Calls:     OPENFI
C
      use common_inc; use perconparam
      use kintcm
      use keyword_interface, only : imdmov
      use cm, only : iwrt62
      use rate_const, only : lgs3
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*20 FINAME
      CHARACTER*7  FISTAT
C
      LOGICAL LEXIT 
      LEXIT = .FALSE.  
      FISTAT = 'OLD'
C
C   Open all the input files needed for this calculation.
C
C   LGS(8) > 0: requires input files on units fu1 and/or fu2 for a 
C               restart calculation.
C
         IF (LGS(8) .GT. 0) THEN
            FINAME = 'poly.fu1'
            CALL OPENFI(FU1, FISTAT, FINAME, LEXIT)
            IF (LGS(8) .EQ. 2) THEN
               FINAME = 'poly.fu2'
               CALL OPENFI(FU2, FISTAT, FINAME, LEXIT)
            ENDIF
         ENDIF
C
C   LGS(18) & LGS(19): requires input files for Quack-Troe calculations
C                      on units fu18, fu19, and/or fu20.  
C
C             From version 6.0, LGS(18) and LGS(19) are removed
C             So fu18-20 are no longer used
C
C   LGS(30): Electronic structure input file requires information 
C            from unit fu30
C
         IF (LGS(30) .EQ. 1) THEN
             FINAME = 'poly.fu30'
             CALL OPENFI(FU30, FISTAT, FINAME, LEXIT)
         elseif (lgs(30).eq.2) then                                     073096PF
             finame = 'poly.fu40'                                       073096PF
             call openfi(fu40,fistat,finame,lexit)                      073096PF
         else if (lgs(30).eq.3) then                                    0810JC97
             finame = 'poly.fu31'                                       0810JC97
             call openfi(fu31,fistat,finame,lexit)                      0810JC97
         ENDIF

C
C   LGS2(8): Extra output from the LCG3 calculation requires information 
C            from unit fu40
C
C            This has been absorbed into unit fu5 since POLYRATE 6.0
C
C
C   LGS2(11): The zero order VTST-IC calculation requires information
C             from unit fu50
C
         IF (LGS2(11).NE.0.AND.IVIC.EQ.1) THEN
            FINAME = 'poly.fu50'
            FISTAT = 'OLD'
            CALL OPENFI(FU50,FISTAT,FINAME,LEXIT)
         ENDIF
         IF (LGS2(11).NE.0.AND.IVIC.EQ.2) THEN
            FINAME = 'poly.fu51'
            FISTAT = 'OLD'
            CALL OPENFI(FU51,FISTAT,FINAME,LEXIT)
         ENDIF
C
C   LGS2(38): IVTST calculation requires information
C             from unit fu7l                                            0606PF97
C
         IF (LGS2(38) .GE. 0 .AND. LGS(8) .LE. 0) THEN                  07/95KAN
            FINAME = 'poly.fu29'                                        07/95KAN
            FISTAT = 'OLD'                                              07/95KAN
            CALL OPENFI(29,FISTAT,FINAME,LEXIT)                         07/95KAN
         ENDIF                                                          07/95KAN
c                   
c   If the MDMOVIE option was chosen then file 65 needs to be           
c   opened.                                                             
         if (imdmov.eq.1) then                                          
           finame='poly.fu65'                                           
           fistat = 'new'                                               
           call openfi(fu65,fistat,finame,lexit)                        
         end if                                                         
c
C   If potnam = mopac, then the special files required by MOPAC
C   need to be opened.
C
c         IF (potnam.eq.'mopac') THEN                                    0913RS96
          IF (INITG(1).EQ.1.OR.INITG(2).EQ.1.OR.INITG(3).EQ.1.OR.
     &        INITG(4).EQ.1.OR.INITG(5).EQ.1.OR.INITG(7).EQ.1.OR.        0601YC98
     &        INITG(8).EQ.1) THEN                                        0601YC98
c
             IF (LGS(8) .LE. 0) THEN
C
C   This is not a restart calculation so the input data files for the 
C   reactant, product, and saddle point (or generalized transition state)
C   species are needed.
C
                 IF (LGS(6).NE.0 .AND. INITG(1).EQ.1) THEN              0514PF97
                     FINAME = 'esp.fu71'                                0606PF97
                     CALL OPENFI(FU71, FISTAT, FINAME, LEXIT)           0606PF97
                 ENDIF
                 IF ((LGS(6).EQ.1 .OR. LGS(6).EQ.2)
     &                              .AND. INITG(2).EQ.1) THEN           0514PF97
                     FINAME = 'esp.fu72'                                0606PF97
                     CALL OPENFI(FU72, FISTAT, FINAME, LEXIT)
                 ENDIF
                 IF (LGS(6).NE.0 .AND. INITG(3).EQ.1) THEN              0514PF97
                     FINAME = 'esp.fu73'                                0606PF97
                     CALL OPENFI(FU73, FISTAT, FINAME, LEXIT)
                 ENDIF
                 IF ((LGS(6).EQ.1 .OR. LGS(6).EQ.3)
     &                              .AND. INITG(4).EQ.1) THEN           0514PF97
                     FINAME = 'esp.fu74'                                0606PF97
                     CALL OPENFI(FU74, FISTAT, FINAME, LEXIT)
                 ENDIF
                 IF (INITG(5) .EQ. 1) THEN                              0514PF97
                     FINAME = 'esp.fu75'
                     CALL OPENFI(FU75, FISTAT, FINAME, LEXIT)
                 ENDIF
                 IF (INITG(7).EQ.1) THEN                                0601YC98
                     FINAME = 'esp.fu77'                                0601YC98
                     CALL OPENFI(FU77, FISTAT, FINAME, LEXIT)           0601YC98
                 ENDIF                                                  0601YC98
                 IF (INITG(8).EQ.1) THEN                                0601YC98
                     FINAME = 'esp.fu78'                                0601YC98
                     CALL OPENFI(FU78, FISTAT, FINAME, LEXIT)           0601YC98
                 ENDIF                                                  0601YC98
             ELSE 
C
C   If a restart calculation (LGS(8) > 0) and LCG3 tunneling (|LGS(9)| .GE. 2)
C   then the unit fu75 file is needed.
C
                 IF (LLCG) THEN
                     FINAME = 'esp.fu75'
                     CALL OPENFI(FU75, FISTAT, FINAME, LEXIT)
                 ENDIF
             ENDIF
         ENDIF
C
C   LGS(30) = -1: Use a potential energy surface to generate an electronic
C                 structure type input file data to unit fu30.
C
         IF (LGS(30) .EQ. -1) THEN
             FISTAT = 'OLD'
             FINAME = 'poly.fu30'
             CALL OPENFI(FU30, FISTAT, FINAME, LEXIT)
         ENDIF
C
C   LGS(30) = -3: Use a potential energy surface to generate an electronic
C                 structure type input file data to unit fu31.
C
         IF (LGS(30) .EQ. -3) THEN                                      0810JC97
             FISTAT = 'UNKNOWN'                                         0810JC97
             FINAME = 'poly.fu31'                                       0810JC97
             CALL OPENFI(FU31, FISTAT, FINAME, LEXIT)                   0810JC97
             fistat='old'                                               0810JC97
         ENDIF                                                          0810JC97
C
C   Open all the output file needed for this calculation.
C
         IF(IWRT62 .NE. 0) THEN                                         0522TA02
             FISTAT = 'NEW'                                             0522TA02
             FINAME = 'esp.fu62'                                        0522TA02 
             CALL OPENFI(FU62, FISTAT, FINAME, LEXIT)                   0522TA02
         ENDIF                                                          0522TA02
C
         FISTAT = 'NEW'
         FINAME = 'esp.fu61'                                            0725YC97
         CALL OPENFI(FU61, FISTAT, FINAME, LEXIT)                       0725YC97
C
C   Open the short fu14 and fu15 output files.
C
         IF (LGS(7) .NE. 0) THEN
             FINAME = 'poly.fu15'
             CALL OPENFI(FU15, FISTAT, FINAME, LEXIT)                   1001WH92
             IF (LGS(7) .GT. 0) THEN
                FINAME = 'poly.fu14'
                CALL OPENFI(FU14, FISTAT, FINAME, LEXIT)
             ENDIF                                                      ..
         ENDIF 
C
C   LGS(8) = 3: Use unit fu3 to store merged MEP information from units
C               fu1 and fu2.
C
         IF (LGS(8) .EQ. 3) THEN
             FINAME = 'poly.fu3'
             CALL OPENFI(FU3, FISTAT, FINAME, LEXIT)
         ENDIF
C
         IF (LGS(8) .EQ. -1) THEN
             FINAME = 'poly.fu1'
             CALL OPENFI(FU1, FISTAT, FINAME, LEXIT)
         ENDIF 

C   |LGS(9)| .GE. 2 and IOT > 0: Use unit fu22 to print extra information
C                                from the LCG3 calculation. 
C
         IF (LLCG) THEN
             FINAME = 'poly.fu22'
             CALL OPENFI(FU22, FISTAT, FINAME, LEXIT)
         ENDIF
C
C   LGS2(8) = 1: Print extra information from LCT calculation to units
C                fu41, fu42, fu43, fu44, fu45, fu46, and fu47.
C
         IF (LGS2(8) .EQ. 1) THEN 
             FINAME = 'poly.fu41'
             CALL OPENFI(FU41, FISTAT, FINAME, LEXIT)
             FINAME = 'poly.fu42'
             CALL OPENFI(FU42, FISTAT, FINAME, LEXIT)
             FINAME = 'poly.fu43'
             CALL OPENFI(FU43, FISTAT, FINAME, LEXIT)
             FINAME = 'poly.fu44'
             CALL OPENFI(FU44, FISTAT, FINAME, LEXIT)
             FINAME = 'poly.fu45'
             CALL OPENFI(FU45, FISTAT, FINAME, LEXIT)
             FINAME = 'poly.fu46'
             CALL OPENFI(FU46, FISTAT, FINAME, LEXIT)
             FINAME = 'poly.fu47'
             CALL OPENFI(FU47, FISTAT, FINAME, LEXIT)
         ENDIF                                                          0708JC00
         IF(LLCG) THEN                                                  0708JC00
             IF(ILCSTR.EQ.1) THEN                                       0708JC00
               FINAME = 'poly.fu49'                                     0708JC00
               CALL OPENFI(FU49, FISTAT, FINAME, LEXIT)                 0708JC00
             ENDIF                                                      0708JC00
             IF(ILCRST.EQ.1) THEN                                       0708JC00
               FINAME = 'poly.fu48'                                     0708JC00
               FISTAT = 'OLD'                                           0708JC00
               CALL OPENFI(FU48, FISTAT, FINAME, LEXIT)                 0708JC00
             ENDIF                                                      0708JC00
         ENDIF
C
         IF (LGS2(13) .NE. 0) THEN                                      0705WH94
             FINAME = 'poly.fu25'                                       0705WH94
             CALL OPENFI(FU25, FISTAT, FINAME, LEXIT)                   0705WH94
             IF (LGS2(13) .EQ. 2) THEN                                  0705WH94
                FINAME = 'poly.fu26'                                    0705WH94
                CALL OPENFI(FU26, FISTAT, FINAME, LEXIT)                0705WH94
             ENDIF                                                      0705WH94
             IF (IPRCD .EQ. 1) THEN                                     0203YC98
                 FINAME = 'poly.fu28'                                   0203YC98
                 CALL OPENFI(FU28, FISTAT, FINAME, LEXIT)               0203YC98
                 WRITE(FU28,*) 'S IN BOHRS, V IN KCAL/MOL'              0708Yc98
                 WRITE(FU28,*) 'BOND LENGTH IN BOHRS, BOND ANGLE',      0708Yc98
     >              ' AND TORSION IN DEGREES'                           0708YC98
             ENDIF                                                      0203YC98
         ENDIF                                                          0705WH94
C
         IF (LGS2(14) .EQ. 1) THEN                                      0705WH94
             FINAME = 'poly.fu27'                                       0705WH94
             CALL OPENFI(FU27, FISTAT, FINAME, LEXIT)                   0705WH94
         ENDIF                                                          0705WH94
c

         if(lgs3(3) .eq. 1) then                                        0411PJ01
             finame = 'poly.fu24'                                       0411PJ01
             call openfi(fu24, fistat, finame, lexit)                   0411PJ01
         end if                                                         0411PJ01

c
c
c    OPEN FILE FOR INTERNAL COORDINATE SPECIAL OUTPUT                   07/95KAN
c         FINAME = 'poly.fu60'                                          07/95KAN
c         CALL OPENFI(60, FISTAT, FINAME, LEXIT)                        07/95KAN
c
         IF (LEXIT) STOP 'FIOPEN 1'
C
         RETURN
         END SUBROUTINE fiopen
C
C***********************************************************************
C  FITMAX
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C
      SUBROUTINE fitmax (IOP,LLSAVE,S3,G3,S5,G5,V3X,V5,
     *                   ISTART,ISTOP)
      use perconparam
      use common_inc
      use rate_const
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      save                                                              0601YC98
C
C
C     FOR IOP.LT.0 FIND MAX DELIG(S) WITH 3 AND 5 POINT FITS
C     FOR IOP=0, FIND MAX DELG(S) WITH 3 AND 5 POINT FITS
C     FOR IOP .GT. 0, FIND MAX VAD(S) WITH 3 AND 5 POINT FITS
C
C     CALLED BY:
C                RATE
C     CALLS:
C            TREPT
C            FIVPT
C
      DIMENSION XPT(5),GPT(5)
C
C
C     FIND LARGEST DELG , DELIG OR VAD VALUE
C
      GMAX = 0.0D0
      IRRCON = 0
      IF (IOP.GT.0) THEN
         ISTART = 1
         ISTOP = LLSAVE
      ENDIF
      DO 10 I = ISTART, ISTOP
         IF (IOP.EQ.0) THEN
            IF (DELG(I).GT.GMAX) THEN
               GMAX = DELG(I)
               IMAXX = I
            ENDIF
         ELSEIF (IOP.GT.0) THEN
            IF (VADIB(I).GT.GMAX) THEN
               GMAX = VADIB(I)
               IMAXX = I
            ENDIF
         ELSEIF (IOP.LT.0) THEN
            IF (DELIG(I).GT.GMAX) THEN
               GMAX = DELIG(I)
               IMAXX = I
            ENDIF
         ENDIF
   10 CONTINUE
      IF (IMAXX.LE.(ISTART+1).OR.IMAXX.GE.(ISTOP-1)) IRRCON = 1         10/11BCG00
      IF (IRRCON.EQ.0) GO TO 40
      IF (LGS(22).EQ.0) GO TO 20
      S3 = SSUBI(IMAXX)
      G3 = DELG(IMAXX)
      IF (IOP.GT.0) G3 = VADIB(IMAXX)
      IF (IOP.LT.0) G3 = DELIG(IMAXX)
      IF (IOP.LE.0) V3X = VADIB(IMAXX)
      S5 = S3
      G5 = G3
      V5 = V3X
      IF (IOP.EQ.0) WRITE (FU6,1000)
      IF (IOP.GT.0) WRITE (FU6,1100)
      IF (IOP.LT.0) WRITE (FU6,1200)
      GO TO 100
   20 CONTINUE
C
C     ERROR CONDITION - MAKE THIS A NON-FATAL ERROR
C
      IF (IOP.NE.0) GO TO 30
      WRITE (FU6,1000)
      STOP 'FITMAX 1'
   30 IF (IOP.GT.0) WRITE (FU6,1100)
      IF (IOP.LT.0) WRITE (FU6,1200)
      STOP 'FITMAX 2'
C
C     THREE POINT FIT
C
   40 J = IMAXX-2
      DO 50 I = 1, 3
         XPT(I) = SSUBI(J+I)
         GPT(I) = DELG(J+I)
         IF (IOP.GT.0) GPT(I) = VADIB(J+I)
         IF (IOP.LT.0) GPT(I) = DELIG(J+I)
   50 CONTINUE
      CALL TREPT (1,XPT,GPT,XMAX,GMAX)
      S3 = XMAX
      G3 = GMAX
C
C     FOR IOP=0, FIT ADIBATIC POTENTIAL AND EVALUATE AT S3 AND S5
C
      IF (IOP.GT.0) GO TO 70
      DO 60 I = 1, 3
         GPT(I) = VADIB(J+I)
   60 CONTINUE
      CALL TREPT (0,XPT,GPT,XMAX,FVAL)
      V3X = FVAL
C
C     FIVE POINT FIT.  USES THREE POINT FIT XMAX AS GUESS FOR MAX
C
   70 CONTINUE
C      IF (IMAXX.EQ.2) IMAXX = 3                                        10/11BCG00
C      IF (IMAXX.EQ.(LLSAVE-1)) IMAXX = LLSAVE-2                        10/11BCG00
      J = IMAXX-3
      DO 80 I = 1, 5
         XPT(I) = SSUBI(J+I)
         GPT(I) = DELG(J+I)
         IF (IOP.GT.0) GPT(I) = VADIB(J+I)
         IF (IOP.LT.0) GPT(I) = DELIG(J+I)
   80 CONTINUE
      CALL FIVPT (1,0,XPT,GPT,XMAX,GMAX)
      S5 = XMAX
      G5 = GMAX
      IF (IOP.GT.0) GO TO 100
      DO 90 I = 1, 5
         GPT(I) = VADIB(J+I)
   90 CONTINUE
      CALL FIVPT (0,0,XPT,GPT,XMAX,FVAL)
      V5 = FVAL
  100 RETURN
C
 1000 FORMAT(1X,20(1H$),31H  LARGEST DELG IS NEAR ENDPOINT)
 1100 FORMAT(1X,20(1H$),30H  LARGEST VAD IS NEAR ENDPOINT)
 1200 FORMAT(1X,20(1H$),32H  LARGEST DELIG IS NEAR ENDPOINT)
C
      END SUBROUTINE fitmax
C
C***********************************************************************
C  FIVPT
C***********************************************************************
C
      SUBROUTINE fivpt (IOP,IB,X,F,XMAX,FMAX)
      use perconparam                         
C
C     COMPUTES QUARTIC FIT F=AX4+BX3+CX2+DX+E THRU FIVE POINTS --
C     (X(I),F(I),I=1,5)
C     THREE POINT RESULT STORED IN XMAX AT START IS USED AS INITIAL
C     GUESS FOR SOLUTION OF XMAX
C     FOR IOP=0,FMAX=F(XMAX) ONLY
C     FOR IOP .GT. 0, XMAX RECOMPUTED
C
C     Include statements were added 6/18/91
C
C     CALLED BY:
C                FITMAX,BOLTZ,VTMUSN,FINOUT
C     CALLS:
C            MXLNEQ,CUBIC
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save                                                              0601YC98
C
      DIMENSION X(5),F(5),ISCR(5),AB(5,6)
C
      DO 10 I = 1, 5
         XX = X(I)
         XX2 = XX*XX
         AB(I,1) = XX2*XX2
         AB(I,2) = XX2*XX
         AB(I,3) = XX2
         AB(I,4) = XX
         AB(I,5) = 1.0D0
         AB(I,6) = F(I)
   10 CONTINUE
      CALL MXLNEQ (AB,5,5,DET,JRANK,EPS,ISCR,-1,6)                      9/20DL90
      IF (JRANK.LT.5) THEN
        IF (IB .EQ. 1) THEN
         XMAX = X(3)
         GO TO 20
        ELSE
         STOP 'FIVPT 1'
        ENDIF
      ENDIF
      A = AB(1,6)
      B = AB(2,6)
      C = AB(3,6)
      D = AB(4,6)
      E = AB(5,6)
      IF (IOP.NE.0) THEN
C
C         TO FIND MAX, NEED TO SOLVE CUBIC EQUATION F'=0
C
         AA = 4.0D0*A
         BB = 3.0D0*B
         CC = C*2.0D0
         XGUES = XMAX
         CALL CUBIC (AA,BB,CC,D,XGUES,ANS)
         XMAX = ANS
      ENDIF
      FMAX = E+XMAX*(D+XMAX*(C+XMAX*(B+XMAX*A)))
   20 RETURN                                                             
      END SUBROUTINE fivpt
C
C***********************************************************************
C FRPRMN 
C***********************************************************************
C
C
      SUBROUTINE frprmn (P,N,FTOL,ITER,FRET)
      use perconparam
C
C CALLED BY: DORODS
C
C CALLS:
C       -VALVAG: FUNCTION TO MINIMIZE
C       -DERVAG: COMPUTES NUMERICAL FIRST DERIVATIVE FOR VALVAG
C       -LINMNR: SUBROUTINE FOR LINE MINIMIZATION 
C
C CONJUGATED GRADIENTS MINIMIZATION SUBROUTINE
C GIVEN A STARTING POINT P, A VECTOR OF LENGTH N, 
C FLETCHER-REEVES-POLLAK-RIBIERE MINIMIZATION IS PERFORMED ON A FUNCTION 
C VALVAG, USING ITS GRADIENT AS CALCULATED BY A ROUTINE DERVAG.
C ON INPUT:   
C    P:     INITIAL POINT
C    N:     DIMENSION OF P
C    FTOL:  CONVERGENCE TOLERANCE
C    
C ON OUTPUT:
C    P:     LOCATION OF THE MINIMUM
C    ITER:  NUMBER OF ITERATIONS PERFORMED
C    FRET:  MINIMUM VALUE OF THE FUNCTION
C
C PARAMETERS:
C    ITMAX: MAXIMUM ALLOWED NUMBER OF ITERATIONS
C    EPSI:   SMALL NUMBER TO RECTIFY SPECIAL CASE OF CONVERGING TO EXACTLY
C           ZERO FUNCTION VALUE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION G(N3TM),H(N3TM),XI(N3TM),P(N3TM)

      EXTERNAL VALVAG

      DATA ITMAX /600/
      DATA EPSI /1.0D-10/ 
C
C     INITIALIZATIONS
C
      FP=VALVAG(P)
      CALL DERVAG (P,XI,N)
      DO 10 J=1,N
         G(J)=-XI(J)
         H(J)=G(J)
         XI(J)=H(J)
 10   CONTINUE


C
C LOOP OVER ITERATIONS
C
      DO 40 ITS=1,ITMAX
         ITER=ITS
         CALL LINMNR (P,XI,N,FRET)
C
C NOW WE HAVE THE NORMAL RETURN
C
         IF (2.*ABS(FRET-FP).LE.FTOL*(ABS(FRET)+ABS(FP)+EPSI)) RETURN 
         FP=VALVAG(P)
         CALL DERVAG (P,XI,N)
         GG=0.D0
         DGG=0.D0
         DO 20 J=1,N
            GG=GG+G(J)**2
C
C STATEMENT FOR fLETCHER-REEVES
C
C           DGG=DGG+XI(J)**2
C
C STATEMENT FOR POLLAK-RIBIERE
C
            DGG=DGG+(XI(J)+G(J))*XI(J)
 20      CONTINUE
C
C IF GRADIENT IS EQUAL TO ZERO THEN WE ARE DONE
C (THIS IS VERY UNLIKELY)
C
         IF (GG.EQ.0.D0) RETURN
         GAM=DGG/GG
         DO 30 J=1,N
            G(J)=-XI(J)
            H(J)=G(J)+GAM*H(J)
            XI(J)=H(J)
 30      CONTINUE
 40      CONTINUE
      STOP 'FRPRMN MAXIMUM ITERATIONS EXCEEDED'
      END
C
C***********************************************************************
C FRSORT
C***********************************************************************
C
      SUBROUTINE frsort(NSIZE,FR,LORDER,IORDER)
C
C     FR is the input array and output array
C     NSIZE is the size of the array
C     LORDER determine the method of sorting:
C            True  means assending
C            False means desending
C
C     IORDER is an array which keeps record on where the
C            new array elements come from, i.e.,
C            IORDER(I) is the index that the new array element
C            FR(I) was in the unsorted array.
C
C
C     CALLED BY:
C               ZOCPAR 
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION FR(NSIZE),IORDER(NSIZE)
      LOGICAL LORDER
C
C     Initialize the IORDER array
C
      DO 10 I = 1, NSIZE
         IORDER(I) = I
10    CONTINUE
C
C     First sort the array in accending order
C
      DO 100 I = 1, NSIZE -1
         DO 100 J = I+1, NSIZE
            IF (FR(J) .LT. FR(I)) THEN
               TEMP  = FR(I)
               FR(I) = FR(J)
               FR(J) = TEMP
               ITEMP = IORDER(I)
               IORDER(I) = IORDER(J)
               IORDER(J) = ITEMP
            ENDIF
100   CONTINUE  
C
C     Then if we want decending order we switch the array
C
      IF (.NOT. LORDER) THEN
         NHALF = NSIZE / 2
         DO 200 I = 1, NHALF  
            TEMP = FR(I)
            FR(I) = FR(NSIZE+1-I)
            FR(NSIZE+1-I) = TEMP
            ITEMP = IORDER(I)
            IORDER(I) = IORDER(NSIZE+1-I)
            IORDER(NSIZE+1-I) = ITEMP 
200      CONTINUE 
      ENDIF
C 
      RETURN
C
      END SUBROUTINE frsort
   
C
C**********************************************************************
C  GAUSQD
C**********************************************************************
      SUBROUTINE gausqd(X1,X2,X,W,N)
      use perconparam
C
C     Called by: 
C               LCG3, SINTGL
C
C     USES GAUSS-LEGENDRE N POINT QUADRATURE FORMULA TO CALCULATE
C     THE WEIGHT W AND THE X VALUES.
C     X1 AND X2 ARE THE LIMITS OF INTEGRATION
C*
C     Include statements were added 6/18/91
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C     Save NN                                                           09/95KAN
C
      DOUBLE PRECISION X(N),W(N)                                        12/31B88
C
C     THE ROOTS ARE SYMMETRIC IN THE INTERVAL, SO FIND ONLY HALF
      M = (N+1)/2
      XM = 0.5D0*(X2 + X1)
      XL = 0.5D0*(X2 - X1)
C     LOOP OVER THE ROOTS
      DO 30 I=1,M
      Z = COS(PI*(DBLE(I) - 0.25D0)/(DBLE(N) + 0.5D0))
C      REFINEMENT LOOP USES NEWTONS METHOD
 10   CONTINUE
      NN = NN + 1
      P1 = 1.0D0
      P2 = 0.0D0
C     EVALUATE THE LEGENDRE POLYNOMIAL AT Z
      DO 20 J=1,N
      P3 = P2
      P2 = P1
      P1 = ((2.0D0*DBLE(J)-1.0D0)*Z*P2-(DBLE(J)-1.0D0)*P3)/DBLE(J)
 20   CONTINUE
C     COMPUTE THE DERIVATIVES OF P1
      IF(Z.NE.1.0D0) THEN
       PP = DBLE(N)*(Z*P1 - P2)/(Z*Z - 1.0D0)
      ELSE
       WRITE(FU6,*)' ERROR IN GAUSQD'
       STOP 'GAUSQD 1'
      ENDIF
      Z1 = Z
      Z = Z1 - P1/PP
C     NEWTONS METHOD
      IF(ABS(Z - Z1).GT.EPS) GO TO 10
      X(I) = XM - XL*Z
      X(N+1-I) = XM + XL*Z
      W(I) = 2.0D0*XL/((1.0D0 - Z*Z)*PP*PP)
      W(N+1-I) = W(I)
 30   CONTINUE

      RETURN
      END SUBROUTINE gausqd
C
C***********************************************************************
C  GAUSSQ
C***********************************************************************
C
      SUBROUTINE gaussq (KIND,N,ALPHA,BETA,KPTS,ENDPTS,B,T,W)
C
C     CALLED BY:
C                RATE
C     CALLS:
C                CLASS,GBSLVE,GBTQL2
C
C           THIS SET OF ROUTINES COMPUTES THE NODES X(I) AND WEIGHTS
C        C(I) FOR GAUSSIAN-TYPE QUADRATURE RULES WITH PRE-ASSIGNED
C        NODES.  THESE ARE USED WHEN ONE WISHES TO APPROXIMATE
C
C                 INTEGRAL (FROM A TO B)  F(X) W(X) DX
C
C                              N
C        BY                   SUM C  F(X )
C                             I=1  I    I
C
C        HERE W(X) IS ONE OF SIX POSSIBLE NON-NEGATIVE WEIGHT
C        FUNCTIONS (LISTED BELOW), AND F(X) IS THE
C        FUNCTION TO BE INTEGRATED.  GAUSSIAN QUADRATURE IS PARTICULARLY
C        USEFUL ON INFINITE INTERVALS (WITH APPROPRIATE WEIGHT
C        FUNCTIONS), SINCE THEN OTHER TECHNIQUES OFTEN FAIL.
C
C           ASSOCIATED WITH EACH WEIGHT FUNCTION W(X) IS A SET OF
C        ORTHOGONAL POLYNOMIALS.  THE NODES X(I) ARE JUST THE ZEROES
C        OF THE PROPER N-TH DEGREE POLYNOMIAL.
C
C     INPUT PARAMETERS
C
C        KIND     AN INTEGER BETWEEN 1 AND 6 GIVING THE TYPE OF
C                 QUADRATURE RULE
C
C        KIND = 1=  LEGENDRE QUADRATURE, W(X) = 1 ON (-1, 1)
C        KIND = 2=  CHEBYSHEV QUADRATURE OF THE FIRST KIND
C                   W(X) = 1/SQRT(1 - X*X) ON (-1, +1)
C        KIND = 3=  CHEBYSHEV QUADRATURE OF THE SECOND KIND
C                   W(X) = SQRT(1 - X*X) ON (-1, 1)
C        KIND = 4=  HERMITE QUADRATURE, W(X) = EXP(-X*X) ON
C                   (-INFINITY, +INFINITY)
C        KIND = 5=  JACOBI QUADRATURE, W(X) = (1-X)**ALPHA * (1+X)**
C                   BETA ON (-1, 1), ALPHA, BETA .GT. -1.
C                   NOTE= KIND=2 AND 3 ARE A SPECIAL CASE OF THIS.
C        KIND = 6=  GENERALIZED LAGUERRE QUADRATURE, W(X) = EXP(-X)*
C                   X**ALPHA ON (0, +INFINITY), ALPHA .GT. -1
C
C        N        THE NUMBER OF POINTS USED FOR THE QUADRATURE RULE
C        ALPHA    REAL PARAMETER USED ONLY FOR GAUSS-JACOBI AND GAUSS-
C                 LAGUERRE QUADRATURE (OTHERWISE USE 0.).
C        BETA     REAL PARAMETER USED ONLY FOR GAUSS-JACOBI QUADRATURE--
C                 (OTHERWISE USE 0.).
C        KPTS     (INTEGER) NORMALLY 0, UNLESS THE LEFT OR RIGHT END-
C                 POINT (OR BOTH) OF THE INTERVAL IS REQUIRED TO BE A
C                 NODE (THIS IS CALLED GAUSS-RADAU OR GAUSS-LOBATTO
C                 QUADRATURE).  THEN KPTS IS THE NUMBER OF FIXED
C                 ENDPOINTS (1 OR 2).
C        ENDPTS   REAL ARRAY OF LENGTH 2.  CONTAINS THE VALUES OF
C                 ANY FIXED ENDPOINTS, IF KPTS = 1 OR 2.
C        B        REAL SCRATCH ARRAY OF LENGTH N
C
C     OUTPUT PARAMETERS (BOTH ARRAYS OF LENGTH N)
C
C        T        WILL CONTAIN THE DESIRED NODES X(1),,,X(N)
C        W        WILL CONTAIN THE DESIRED WEIGHTS C(1),,,C(N)
C
C     SUBROUTINES REQUIRED
C
C        GBSLVE, CLASS, AND GBTQL2 ARE PROVIDED. UNDERFLOW MAY SOMETIMES
C        OCCUR, BUT IT IS HARMLESS IF THE UNDERFLOW INTERRUPTS ARE
C        TURNED OFF AS THEY ARE ON THIS MACHINE.
C
C     ACCURACY
C
C        THE ROUTINE WAS TESTED UP TO N = 512 FOR LEGENDRE QUADRATURE,
C        UP TO N = 136 FOR HERMITE, UP TO N = 68 FOR LAGUERRE, AND UP
C        TO N = 10 OR 20 IN OTHER CASES.  IN ALL BUT TWO INSTANCES,
C        COMPARISON WITH TABLES IN REF. 3 SHOWED 12 OR MORE SIGNIFICANT
C        DIGITS OF ACCURACY.  THE TWO EXCEPTIONS WERE THE WEIGHTS FOR
C        HERMITE AND LAGUERRE QUADRATURE, WHERE UNDERFLOW CAUSED SOME
C        VERY SMALL WEIGHTS TO BE SET TO ZERO.  THIS IS, OF COURSE,
C        COMPLETELY HARMLESS.
C
C     METHOD
C
C           THE COEFFICIENTS OF THE THREE-TERM RECURRENCE RELATION
C        FOR THE CORRESPONDING SET OF ORTHOGONAL POLYNOMIALS ARE
C        USED TO FORM A SYMMETRIC TRIDIAGONAL MATRIX, WHOSE
C        EIGENVALUES (DETERMINED BY THE IMPLICIT QL-METHOD WITH
C        SHIFTS) ARE JUST THE DESIRED NODES.  THE FIRST COMPONENTS OF
C        THE ORTHONORMALIZED EIGENVECTORS, WHEN PROPERLY SCALED,
C        YIELD THE WEIGHTS.  THIS TECHNIQUE IS MUCH FASTER THAN USING A
C        ROOT-FINDER TO LOCATE THE ZEROES OF THE ORTHOGONAL POLYNOMIAL.
C        FOR FURTHER DETAILS, SEE REF. 1.  REF. 2 CONTAINS DETAILS OF
C        GAUSS-RADAU AND GAUSS-LOBATTO QUADRATURE ONLY.
C
C     REFERENCES
C
C        1.  GOLUB, G. H., AND WELSCH, J. H.,  CALCULATION OF GAUSSIAN
C            QUADRATURE RULES,  MATHEMATICS OF COMPUTATION 23 (APRIL,
C            1969), PP. 221-230.
C        2.  GOLUB, G. H.,  SOME MODIFIED MATRIX EIGENVALUE PROBLEMS,
C            SIAM REVIEW 15 (APRIL, 1973), PP. 318-334 (SECTION 7).
C        3.  STROUD AND SECREST, GAUSSIAN QUADRATURE FORMULAS, PRENTICE-
C            HALL, ENGLEWOOD CLIFFS, N.J., 1966.
C
C
C     ..................................................................
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION MUZERO
      DIMENSION B(N),T(N),W(N),ENDPTS(2)
C
      CALL CLASS (KIND,N,ALPHA,BETA,B,T,MUZERO)
C
C           THE MATRIX OF COEFFICIENTS IS ASSUMED TO BE SYMMETRIC.
C           THE ARRAY T CONTAINS THE DIAGONAL ELEMENTS, THE ARRAY
C           B THE OFF-DIAGONAL ELEMENTS.
C           MAKE APPROPRIATE CHANGES IN THE LOWER RIGHT 2 BY 2
C           SUBMATRIX.
C
      IF (KPTS.EQ.0) GO TO 20
      IF (KPTS.EQ.2) GO TO 10
C
C           IF KPTS=1, ONLY T(N) MUST BE CHANGED
C
      T(N) = GBSLVE(ENDPTS(1),N,T,B)*B(N-1)**2+ENDPTS(1)
      GO TO 20
C
C           IF KPTS=2, T(N) AND B(N-1) MUST BE RECOMPUTED
C
   10 GAM = GBSLVE(ENDPTS(1),N,T,B)
      T1 = ((ENDPTS(1)-ENDPTS(2))/(GBSLVE(ENDPTS(2),N,T,B)-GAM))
      B(N-1) = SQRT(T1)
      T(N) = ENDPTS(1)+GAM*T1
C
C           NOTE THAT THE INDICES OF THE ELEMENTS OF B RUN FROM 1 TO N-1
C           AND THUS THE VALUE OF B(N) IS ARBITRARY.
C           NOW COMPUTE THE EIGENVALUES OF THE SYMMETRIC TRIDIAGONAL
C           MATRIX, WHICH HAS BEEN MODIFIED AS NECESSARY.
C           THE METHOD USED IS A QL-TYPE METHOD WITH ORIGIN SHIFTING
C
   20 W(1) = 1.0D0
      DO 30 I = 2, N
         W(I) = 0.0D0
   30 CONTINUE
C
      CALL GBTQL2 (N,T,B,W,IERR)
      DO 40 I = 1, N
         W(I) = MUZERO*W(I)*W(I)
   40 CONTINUE
C
      RETURN
      END SUBROUTINE gaussq 
C
C***********************************************************************
C  GBSLVE
C***********************************************************************
C
      FUNCTION gbslve (SHIFT,N,A,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C       THIS PROCEDURE PERFORMS ELIMINATION TO SOLVE FOR THE
C       N-TH COMPONENT OF THE SOLUTION DELTA TO THE EQUATION
C
C             (JN - SHIFT*IDENTITY) * DELTA  = EN,
C
C       WHERE EN IS THE VECTOR OF ALL ZEROES EXCEPT FOR 1 IN
C       THE N-TH POSITION.
C
C       THE MATRIX JN IS SYMMETRIC TRIDIAGONAL, WITH DIAGONAL
C       ELEMENTS A(I), OFF-DIAGONAL ELEMENTS B(I).  THIS EQUATION
C       MUST BE SOLVED TO OBTAIN THE APPROPRIATE CHANGES IN THE LOWER
C       2 BY 2 SUBMATRIX OF COEFFICIENTS FOR ORTHOGONAL POLYNOMIALS.
C
C       CALLED BY:
C                  GAUSSQ
C                                
      DIMENSION A(N),B(N)
C
      ALPHA = A(1)-SHIFT
      NM1 = N-1
      DO 10 I = 2, NM1
         ALPHA = A(I)-SHIFT-B(I-1)**2/ALPHA
   10 CONTINUE
      GBSLVE = 1.0D0/ALPHA
      RETURN
      END FUNCTION gbslve
C
C***********************************************************************
C  GBTQL2
C***********************************************************************
C
      SUBROUTINE gbtql2 (N,D,E,Z,IERR)
C
C     CALLED BY:
C                 GAUSSQ
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,
C     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
C     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND FIRST COMPONENTS OF THE
C     EIGENVECTORS OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL
C     METHOD, AND IS ADAPTED FROM THE EISPAK ROUTINE IMTQL2
C
C     ON INPUT=
C
C        N IS THE ORDER OF THE MATRIX;
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX;
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS FIRST N-1 POSITIONS.  E(N) IS ARBITRARY;
C
C        Z CONTAINS THE FIRST ROW OF THE IDENTITY MATRIX.
C
C      ON OUTPUT=
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1, 2, ..., IERR-1;
C
C        E HAS BEEN DESTROYED;
C
C        Z CONTAINS THE FIRST COMPONENTS OF THE ORTHONORMAL EIGENVECTORS
C          OF THE SYMMETRIC TRIDIAGONAL MATRIX.  IF AN ERROR EXIT IS
C          MADE, Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES;
C
C        IERR IS SET TO
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     ------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION MACHEP
      DIMENSION D(N),E(N),Z(N)
C
C     ========== MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C                MACHEP = 16.0D0**(-13) FOR LONG FORM ARITHMETIC
C                ON S360 ==========
C
      MACHEP = 1.0D-16
C
      IERR = 0
      IF (N.EQ.1) GO TO 110
C
      E(N) = 0.0D0
      DO 70 L = 1, N
         J = 0
C
C     ========== LOOK FOR SMALL SUB-DIAGONAL ELEMENT ==========
C
   10    DO 20 M = L, N
            IF (M.EQ.N) GO TO 30
            IF (ABS(E(M)).LE.MACHEP*(ABS(D(M))+ABS(D(M+1)))) GO TO 30
   20    CONTINUE
C
   30    P = D(L)
         IF (M.EQ.L) GO TO 70
         IF (J.EQ.30) GO TO 100
         J = J+1
C
C     ========== FORM SHIFT ==========
C
         G = (D(L+1)-P)/(2.0D0*E(L))
         R = SQRT(G*G+1.0D0)
         G = D(M)-P+E(L)/(G+SIGN(R,G))
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
         MML = M-L
C
C     ========== FOR I=M-1 STEP -1 UNTIL L DO -- ==========
C
         DO 60 II = 1, MML
            I = M-II
            F = S*E(I)
            B = C*E(I)
            IF (ABS(F).LT.ABS(G)) GO TO 40
            C = G/F
            R = SQRT(C*C+1.0D0)
            E(I+1) = F*R
            S = 1.0D0/R
            C = C*S
            GO TO 50
   40       S = F/G
            R = SQRT(S*S+1.0D0)
            E(I+1) = G*R
            C = 1.0D0/R
            S = S*C
   50       G = D(I+1)-P
            R = (D(I)-G)*S+2.0D0*C*B
            P = S*R
            D(I+1) = G+P
            G = C*R-B
C
C     ========== FORM FIRST COMPONENT OF VECTOR ==========
C
            F = Z(I+1)
            Z(I+1) = S*Z(I)+C*F
            Z(I) = C*Z(I)-S*F
C
   60    CONTINUE
C
         D(L) = D(L)-P
         E(L) = G
         E(M) = 0.0D0
         GO TO 10
   70 CONTINUE
C
C     ========== ORDER EIGENVALUES AND EIGENVECTORS ==========
C
      DO 90 II = 2, N
         I = II-1
         K = I
         P = D(I)
C
         DO 80 J = II, N
            IF (D(J).GE.P) GO TO 80
            K = J
            P = D(J)
   80    CONTINUE
C
         IF (K.EQ.I) GO TO 90
         D(K) = D(I)
         D(I) = P
C
         P = Z(I)
         Z(I) = Z(K)
         Z(K) = P
C
   90 CONTINUE
C
      GO TO 110
C
C     ========== SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ==========
C
  100 IERR = L
  110 RETURN
C
C     ========== LAST CARD OF GBTQL2 ==========
C
      END SUBROUTINE gbtql2
C*************************************************************************
C  GRAD
C*************************************************************************
      SUBROUTINE grad (IH,NMOD,N3,NEND,DLX,X0,AMASS,DXX,COF)
      use perconparam
      use common_inc, only : v,x,xxc,dx
C
C     CONVERTS A POSTION SPECIFIED BY NORMAL MODE DIRECTION
C     VECTORS FROM NORMAL COORDS TO STRAIGHT CARTESIAN COORDS.
C     ALSO GETS GRAD(V) IN MASS-SCALED CARTESIANS
C
C     Called by:
C            GRADDR
C     Calls:
C            FIRST
C
C     On input:
C        IH   : order of the derivative
C        NMOD : number of normal modes
C        N3   : total number of degrees of freedom of the reaction
C        NEND : total number of degrees of freedom of the species
C        N3TM : the dimension of IH, X0, AMASS, and COF
C        DLX  : the stepsize for evaluating the derivative
C        X0   : the position vector in cartesians
C        AMASS: the array stores the mass factors for scaling the 
C               coordinates, (mi/mu)**(1/2)  
C        COF  : the normal mode eigenvectors
C     On output:
C        DX   : the gradient
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C
c      COMMON /PEFCOM/ V,X(N3TM),XXC(N3TM),DX(N3TM)
      DIMENSION IH(N3TM)
      DIMENSION SMQ(N3TM),XPOS(N3TM),X0(N3TM),COF(N3TM,N3TM)
      DIMENSION DXX(N3TM),AMASS(N3TM),XSAV(N3TM)
C      write (70,*) 'in grad ih =',(ih(i),i=1,nmod)                     0601YC98
      do i = 1,n3tm                                                     0601YC98
        dx(i) = 0.0d0                                                   0601YC98
        xpos(i) = 0.0d0                                                 0601YC98
c
c to get around the problem of reassign the X0 array, use XSAV to store 0601YC98 
c not clean, but works                                                  0601YC98
c 
        xsav(i) = x0(i)                                                 0601YC98
      enddo                                                             0601YC98
      vsav = v                                                          0601YC98
c
c      write (6,*) 'in grad v, vsav',v,vsav
c      write (6,*) 'in grad x0'
c      write (6,1010) (x0(i),i=1,n3tm)
c1010   format (3F15.8)
C
C     GENERATE POSITION IN NORMAL MODE SPACE
C     CONVERT DISTANCES TO MU IN A.U. AND MULTIPLY STEP BY 10.
      ISHFT = NEND - NMOD
      DO 50 JNM=1,NMOD
         SMQ(JNM)=IH(JNM)*DLX
   50  CONTINUE
c   50 SMQ(JNM)=IH(JNM)*DLX*42.6948D0*10.D0
C     NORMAL COORDS TO CART COORDS (IN BOHR):
c
 995  format (20I3)
 996  format (5E15.6)
      DO JDIR=1,N3
         XPOS(JDIR)=X0(JDIR)
         DO JNM=1,NMOD
            XPOS(JDIR)=XPOS(JDIR)+SMQ(JNM)*COF(JDIR,JNM+ISHFT)
c     *      /AMASS(JDIR)
         ENDDO
      ENDDO
  626 format (3E15.8)
      DO I = 1,N3TM                                                     0601YC98
        X(I) = XPOS(I)                                                  0601YC98
      ENDDO                                                             0601YC98
C     GET POTENTIAL AND GRAD IN STRAIGHT CARTESIANS                     0601YC98
c      call ghook(0,iproc)                                                     0301YC97
       call ghook(1,iproc)
C
C     CONVERT GRAD TO MASS-SCALED CARTESIANS
C
      DO IX=1,N3
c         DXX(IX)=DX(IX)/AMASS(IX)
        DXX(IX) =DX(IX)
      ENDDO
c
c     make sure that x0 contains the original position
c
      DO I = 1,N3TM
        X0(I) = XSAV(I)
      ENDDO
      v = vsav
      RETURN
      END SUBROUTINE grad 
C
C*************************************************************
C  GRADDR
C*************************************************************
      SUBROUTINE graddr (NMOD,N3,NEND,LPTBCR,REDM,DLX,X0,AMASS,FREQ,COF
     >                                      ,C,Q,S)
      use perconparam
C
C    This subroutine evaluates the derivatives of the true potential
C    to obtain dimensionless normal coordinate force constants
C
C    Called by:
C          NORMOD
C    Calls:
C          GRAD
C
C    On input:
C        NMOD : number of normal modes
C        N3   : total number of degrees of freedom of the reaction
C        NEND : total number of degrees of freedom of the species
C        N3TM : the dimension of IH, X0, AMASS, and COF
C        AUTOCM:converts the energy from atomic unit to kcal/mol
C        REDM : the reduced mass
C        DLX  : the stepsize for evaluating the derivative
C        X    : the position vector in cartesians
C        AMASS: the array stores the mass factors for scaling the
C               coordinates, (mi/mu)**(1/2)
C        FREQ : the normal mode frequencies
C        COF  : the normal mode eigenvectors
C
C    On output:
C        C(I,J,K) : f
C                    ijk
C        Q(I,J)   : f
C                    iijj
C
C    Note that f   = 6 C(I,I,I), f   = 2 C(I,I,J), f   = C(I,J,K)
C               iii               iij               ijk
C
C    Note that f    = 24 Q(I,I), f    = 4 Q(I,J)
C               iiii              iijj
C
C    V = V  + 1/2 SUM k q q  + 1/6 SUM f   q q q + 1/24 SUM  f    q q q q
C         e        i   i i i       ijk  ijk i j k       ijkl  ijkl i j k l
C
C    where, q is the dimensionaless normal coordinates.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION C(N3TM,N3TM,N3TM),Q(N3TM,N3TM)
      DIMENSION IH(N3TM),WM(N3TM),DX(N3TM),AMASS(N3TM)
      DIMENSION FREQ(N3TM),COF(N3TM,N3TM)
      DIMENSION GRAD0(N3TM),GRAD1(N3TM),GRAD2(N3TM),GRAD3(N3TM),
     *          GRAD4(N3TM),GRAD5(N3TM),GRAD6(N3TM),VEC(N3TM),
     *          X0(N3TM)
      LOGICAL LDEBUG
      LDEBUG =  .FALSE.
      IDBG   =  70
C
C     SET UP (REDM*W)**0.5 FACTORS IN A.U.
C
      ISHFT =  NEND - NMOD
      DO 117 I = 1, N3TM
         WM(I) = 0.D0
         DO 127 J = 1, N3TM
            Q(I,J) = 0.D0
            DO 137 K = 1, N3TM
               C(I,J,K) = 0.D0
137         CONTINUE
127      CONTINUE
117   CONTINUE
      DO 1 IW=1,NMOD
         WM(IW)=SQRT(REDM*FREQ(IW+ISHFT))
    1 CONTINUE
C     CALCULATE DIAGONAL THIRD AND FOURTH DERIVATIVES
C     ALONG WITH THE IIJ-TYPE THIRD DERIVATIVES
      H2=DLX*DLX
      H3=H2*DLX
C   SCALE H FOR MASS IN ATOMIC UNITS
      IF (LDEBUG) WRITE(IDBG,*) 'S = ', S
      IF (LDEBUG) WRITE(IDBG,*) 'REDM = ',REDM
      IF (LDEBUG) WRITE(IDBG,*) 'DLX = ', DLX
      IF (LDEBUG) WRITE(IDBG,*) 'STEP SIZES H2, H3'
      IF (LDEBUG) WRITE(IDBG,39) H2,H3
      IF (LDEBUG) WRITE(IDBG,9) (WM(I),I=1,NMOD)
C     IH IS THE NUMBER OF STEPS H TAKEN IN EACH NORMAL MODE DIRECTION.
C     WHEN IT IS ZERO, WE ARE AT THE STARTING GEOMETRY.
      IF (LDEBUG) WRITE (IDBG,*) 'INIT GEOMETRY'
      IF (LDEBUG) WRITE(IDBG,17) (X0(I),I=1,N3)
   17 FORMAT(1X,'X(I)=',(5D20.10))
      DO 5 II=1,NMOD
         IH(II)=0
    5 CONTINUE
C     OBTAIN AND STORE GRAD(V) AT STARTING POINT
      CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)                    0206WH93
      DO 10 I=1,N3
         GRAD0(I)=DX(I)
   10 CONTINUE
      IF(LDEBUG) WRITE(IDBG,29) (GRAD0(I),I=1,N3)
C     THIRD DERIVATIVES
      DO 100 IMOD=1,NMOD
          IH(IMOD)=1
          CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)                0206WH93
          DO 20 I=1,N3
             GRAD1(I)=DX(I)
   20     CONTINUE
          IF (LDEBUG) WRITE(IDBG,49) (GRAD1(I),I=1,N3)
          IH(IMOD)=-1
          CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)                0206WH93
          DO 30 I=1,N3
             GRAD2(I)=DX(I)
   30     CONTINUE
          IF (LDEBUG) WRITE(IDBG,59) (GRAD2(I),I=1,N3)
          DO 35 I=1,N3
             VEC(I)= GRAD1(I)+GRAD2(I)-2.D0*GRAD0(I)
   35     CONTINUE
          IF (LDEBUG) WRITE(IDBG,69) (VEC(I),I=1,N3)
           DO 50 KMOD=IMOD,NMOD
             SUM=0.D0
             DO 40 I=1,N3
                SUM=SUM+VEC(I)*COF(I,KMOD+ISHFT)
                IF (LDEBUG) WRITE(IDBG,79) COF(I,KMOD+ISHFT),KMOD
                IF (LDEBUG) WRITE(IDBG,89) SUM
   40        CONTINUE
             IF (LDEBUG) WRITE(IDBG,99) SUM
             IF (LDEBUG) WRITE (IDBG,*) 'WMh2',WM(IMOD),WM(KMOD),H2
             DERIV= SUM/(WM(IMOD)*WM(IMOD)*WM(KMOD)*H2)
             IF (LDEBUG) WRITE(IDBG,119) DERIV*AUTOCM
             DERIV=DERIV/2.D0
             IF (IMOD.EQ.KMOD) DERIV=DERIV/3.D0
             C(IMOD,IMOD,KMOD)=DERIV
             IF (LDEBUG) WRITE(IDBG,119) DERIV*AUTOCM,IMOD,IMOD,KMOD
             IF (LDEBUG) WRITE(IDBG,109) WM(IMOD)*SQRT(AUTOCM),
     *                   WM(KMOD)*SQRT(AUTOCM)
             IF (LDEBUG) WRITE(IDBG,129) C(IMOD,IMOD,KMOD)*AUTOCM,
     *                   IMOD,IMOD,KMOD
   50     CONTINUE
C     FOURTH DERIVATIVES
          IH(IMOD)=2
          CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)                0206WH93
          DO 60 I=1,N3
             GRAD3(I)=DX(I)
   60     CONTINUE
          IF (LDEBUG) WRITE(IDBG,139) (GRAD3(I),I=1,N3)
          IH(IMOD)=-2
          CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)                0206WH93
          DO 70 I=1,N3
             GRAD4(I)=DX(I)
   70     CONTINUE
          IF (LDEBUG) WRITE(IDBG,149) (GRAD4(I),I=1,N3)
          DO 75 I=1,N3
             VEC(I)= GRAD3(I)-2.D0*GRAD1(I)+
     *                    2.D0*GRAD2(I)-GRAD4(I)
   75     CONTINUE
          IF (LDEBUG) WRITE(IDBG,159) (VEC(I),I=1,N3)
          SUM=0.D0
          DO 80 I=1,N3
             SUM=SUM+VEC(I)*COF(I,IMOD+ISHFT)
             IF (LDEBUG) WRITE(IDBG,169) VEC(I)*COF(I,IMOD+ISHFT),
     *                                   COF(I,IMOD+ISHFT),IMOD
   80     CONTINUE
          IF (LDEBUG) WRITE(IDBG,99) SUM
          IF (LDEBUG) WRITE (IDBG,*) 'WMh3',WM(IMOD),H3
          DERIV=SUM/(WM(IMOD)**4*2.D0*H3)
          IF (LDEBUG) WRITE(IDBG,189) DERIV,WM(IMOD)
          DERIV=DERIV/24.D0
          Q(IMOD,IMOD)=DERIV
          IF (LDEBUG) WRITE(IDBG,179) SUM
          IF (LDEBUG) WRITE(IDBG,119) DERIV*AUTOCM,IMOD,IMOD
          IF (LDEBUG) WRITE(IDBG,209) Q(IMOD,IMOD)*AUTOCM,IMOD,IMOD
  18      IH(IMOD)=0
  100 CONTINUE
C     NOW CALCULATE IJK-TYPE THIRD AND IIJJ-TYPE FOURTH
C     DERIVATIVES
      IF(NMOD.EQ.1) RETURN
      NMODM1=NMOD-1
      DO 300 IMOD=1,NMODM1
        JSTART=IMOD+1
C THIRD DERIVATIVES
         DO 200 JMOD=JSTART,NMOD
            IH(IMOD)=1
            IH(JMOD)=1
            CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)              0206WH93
            DO 110 I=1,N3
               GRAD1(I)=DX(I)
  110       CONTINUE
            IF (LDEBUG) WRITE(IDBG,239) (GRAD1(I),I=1,N3)
   38       IH(JMOD)=-1
            CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)              0206WH93
            DO 115 I=1,N3
               GRAD2(I)=DX(I)
  115       CONTINUE
            IF (LDEBUG) WRITE(IDBG,259) (GRAD2(I),I=1,N3)
   48       IH(IMOD)=-1
            IH(JMOD)=1
            CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)              0206WH93
            DO 120 I=1,N3
               GRAD3(I)=DX(I)
  120       CONTINUE
            IF (LDEBUG) WRITE(IDBG,269) (GRAD3(I),I=1,N3)
   58       IH(JMOD)=-1
            CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)              0206WH93
            DO 125 I=1,N3
                GRAD4(I)=DX(I)
  125       CONTINUE
            IF(LDEBUG) WRITE(IDBG,279) (GRAD4(I),I=1,N3)
   68       DO 130 I=1,N3
              VEC(I)=GRAD1(I)-GRAD2(I)-GRAD3(I)+GRAD4(I)
  130       CONTINUE
            IF (LDEBUG) WRITE(IDBG,289) (VEC(I),I=1,N3)
   78       DO 150 KMOD=JMOD,NMOD
              SUM=0.D0
              DO 140 I=1,N3
                 IF (LDEBUG) WRITE(IDBG,299) SUM,COF(I,KMOD+ISHFT),KMOD
                 SUM=SUM+VEC(I)*COF(I,KMOD+ISHFT)
  140         CONTINUE
   88         DERIV=SUM/(WM(IMOD)*WM(JMOD)*WM(KMOD)*4.D0*H2)
  188         IF (KMOD.EQ.JMOD) DERIV=DERIV/2.D0
              C(IMOD,JMOD,KMOD)=DERIV
              IF (LDEBUG) WRITE(IDBG,319) DERIV*AUTOCM,
     *                    WM(IMOD)*SQRT(AUTOCM),WM(JMOD),WM(KMOD)
c              IF (LDEBUG) WRITE(IDBG,309) SUM
              IF (LDEBUG) WRITE(IDBG,419) C(IMOD,JMOD,KMOD)*AUTOCM,
     *                    IMOD,JMOD,KMOD
  150       CONTINUE
C     FOURTH DERIVATIVES
            IH(IMOD)=0
            IH(JMOD)=1
            CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)
            DO 155 I=1,N3
               GRAD5(I)=DX(I)
  155       CONTINUE
            IF (LDEBUG) WRITE(IDBG,429) (GRAD5(I),I=1,N3)
   98       IH(JMOD)=-1
            CALL GRAD(IH,NMOD,N3,NEND,DLX,X0,AMASS,DX,COF)
            DO 160 I=1,N3
               GRAD6(I)=DX(I)
  160       CONTINUE
            IF (LDEBUG) WRITE(IDBG,439) (GRAD6(I),I=1,N3)
  108       DO 165 I=1,N3
               VEC(I)=GRAD1(I)-2.D0*GRAD5(I)+GRAD3(I)-GRAD2(I)+
     *                     2.D0*GRAD6(I)-GRAD4(I)
  165       CONTINUE
            IF (LDEBUG) WRITE(IDBG,449) (VEC(I),I=1,N3)
  118       SUM=0.D0
            DO 170 I=1,N3
               IF (LDEBUG) WRITE(IDBG,459) SUM,COF(I,JMOD+ISHFT)
               SUM=SUM+VEC(I)*COF(I,JMOD+ISHFT)
  170       CONTINUE
            DERIV=SUM/(WM(IMOD)**2*WM(JMOD)**2*2.D0*H3)
  128       DERIV=DERIV/4.D0
            Q(IMOD,JMOD)=DERIV
            IF (LDEBUG) WRITE(IDBG,469) DERIV*AUTOCM,
     *                WM(IMOD)*SQRT(AUTOCM),WM(JMOD)*SQRT(AUTOCM)
            IF (LDEBUG) WRITE(IDBG,479) Q(IMOD,JMOD),IMOD,JMOD
  28        IH(JMOD)=0
  200    CONTINUE
  300 CONTINUE
c     copy the IJJ terms to JJI terms
c
      DO I = 1,NMOD
       DO J = I+1, NMOD
        C(J,J,I) = C(I,J,J)
       ENDDO
      ENDDO
      RETURN
    9 FORMAT(1X,'WM(I)=',(5E15.7))
   19 FORMAT(1X,'DX1(I)=',(5E15.7))
   29 FORMAT(1X,'GRAD0(I)=',(5E15.7))
   39 FORMAT(1X,'H2=',E15.7,5X,'H3=',E15.7)
   49 FORMAT(1X,'GRAD1(I)=',(5E15.7))
   59 FORMAT(1X,'GRAD2(I)=',(5E15.7))
   69 FORMAT(1X,'VEC(I)=',(5E15.7))
   79 FORMAT(1X,'COF(I,J)=',E15.7,5X,'KMOD=',I4)
   89 FORMAT(1X,'SUM=',E15.7)
   99 FORMAT(1X,'SUM(TOTAL)=',E15.7)
  109 FORMAT(1X,'WM(IMOD)=',E15.7,5X,'WM(KMOD)=',E15.7)
  129 FORMAT(1X,'C(I,I,K)=',E15.7,5X,I4,I4,I4)
  139 FORMAT(1X,'GRAD3(I)=',(5E15.7))
  149 FORMAT(1X,'GRAD4(I)=',(5E15.7))
  159 FORMAT(1X,'VEC(I)=',(5E15.7))
  169 FORMAT(1X,'SUM=',E15.7,5X,
     *'COF(I,J)=',E15.7,5X,'IMOD=',I4)
  179 FORMAT(1X,'SUM(TOTAL)=',E15.7)
  189 FORMAT(1X,'DERIV=',E15.7,5X,'WM(IMOD)=',E15.7)
  209 FORMAT(1X,'Q(I,I)=',E15.7,5X,I4,I4)
  239 FORMAT(1X,'GRAD1(I)=',(5E15.7))
  259 FORMAT(1X,'GRAD2(I)=',(5E15.7))
  269 FORMAT(1X,'GRAD3(I)=',(5E15.7))
  279 FORMAT(1X,'GRAD4(I)=',(5E15.7))
  289 FORMAT(1X,'VEC(I)=',(5E15.7))
  299 FORMAT(1X,'SUM=',E15.7,5X,'COF(I,K)=',E15.7,
     *5X,'KMOD=',I4)
  309 FORMAT(1X,'SUM(TOTAL)=',E15.7)
  319 FORMAT(1X,'DERIV=',E20.10,'WM(I)=',E15.7,
     *5X,'WM(J)=',E15.7,5X,'WM(K)=',E15.7)
  419 FORMAT(1X,'C(I,J,K)=',E15.7,5X,I4,I4,I4)
  429 FORMAT(1X,'GRAD5(I)',(5E15.7))
  439 FORMAT(1X,'GRAD6(I)=',(5E15.7))
  449 FORMAT(1X,'VEC(I)=',(5E15.7))
  459 FORMAT(1X,'SUM-',E15.7,5X,'COF(I,J)=',E15.7)
  119 FORMAT(1X,'DERIV=',E15.7,10X,3I5)
  469 FORMAT(1X,'DERIV=',E15.7,5X,'WM(I)=',E15.7,5X,
     *'WM(J)=',E15.7)
  479 FORMAT(1X,'Q(I,J)=',E15.7,5X,I4,I4)
 9209 FORMAT(1H ,25(1H*),' OUTPUT FOR POLYMODE ',25(1H*))
 9219 FORMAT(1X,2(I2,1H,),E15.7)
 9229 FORMAT(1X,3(I2,1H,),E15.7)
 9239 FORMAT(1X,4(I2,1H,),E15.7)
      END SUBROUTINE graddr
C
C **********************************************************************
C   DORODS
C **********************************************************************
C
      SUBROUTINE dorods (IOP,IS)
      use perconparam
      use common_inc
      use rate_const
      use keyword_interface, only : gufac6
C
C     THIS IS A SUBROUTINE TO PERFORM THE RE-ORIENTAION OF THE DIVIDING
C     SURFACE (RODS) ALGORITHM.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION SCR(N3TM),XGUARD(N3TM)
      if(.not.allocated(gguard)) allocate(gguard(n3tm))

C
      LRODS = .TRUE.                                                    0219PF98
C
      KOP = ABS(IOP)
      IF (IOP.LT.0) THEN
         N3M7 = NF(KOP)
         NEND = 3*NRATOM(KOP)
      ELSE
         N3M7 = NF(5)
         NEND = N3
      ENDIF
C
C     IRODS = 1, RODS DOCTOR CALCULATION (GEOMETRY & GRADIENTS NOT UPDATED)
C     IVRP  = 1, VRP PATH FOLLOWING (GEOMETRY & GRADIENTS UPDATED)
C
C     AT THIS POINT DX CONTAINS THE NORMALIZED GRADIENT VECTOR, BUT THE
C      ALGORITHM NEEDS THE UNNORMALIZED GRADIENT.  DX WILL BE REPLACED
C      WITH THE UNNORMALIZED GRADIENT, THE NORMALIZED GRADIENT WILL BE
C      KEPT IN GGUARD.
C
C     SCR IS THE TRIAL GRADIENT DIRECTION, SET EQUAL TO DX AT THE START
C

      DO 10 I = 1, NEND
         XGUARD(I) = X(I)                                               0219PF98
         GGUARD(I) = DX(I)
         IF (LGS(30).GT.0) DX(I) = DX(I) * DXNORM
         IF (LGS(30).LE.0) DX(I) = DX(I) * DXMAG
         SCR(I)=DX(I)
 10   CONTINUE

C
C     V WILL BE THE VALUE OF VMEP BEFORE THE OPTIMIZATION
C
     
      IF (LGS(30).GT.0) THEN
         V = VS(IS) - VS(1)
      END IF
  
C
C     BEGIN DIVIDING SURFACE OPTIMIZATION
C
C
      CALL FRPRMN (SCR,NEND,1.0D-4,ITER,VAG)

C
C     VTT IS THE NEW VALUE FOR VMEP (LESS THAN OR EQUAL TO THE PREVIOUS VALUE)
C
      IF (LGS(30).GT.0) THEN
         VS(IS) = VTT
      ELSE
         V = VTT
      END IF
C
C     REPLACE DX WITH THE OPTIMIZED DIRECTION OBTAINED FROM FRPRMN (NORMALIZED)
C      TO PERFORM A LAST PROJECTION
C
      CALL ANGLV(DX,SCR,XXX2,NEND)

C
      DO 20 I = 1, NEND
         DX(I) = SCR(I)
 20   CONTINUE

C
C      IF ((IVRP.EQ.1.OR.IRODS.EQ.1).AND.ICALCS.EQ.1) THEN               0219PF98
C         DIST = 0.0D0
C         DO 30 I = 1, NEND
C           DIST = DIST + (XNOU(I)-SAVEX(I))**2
C 30      CONTINUE
C         DIST = DSQRT(DIST)
C         S = S - FISEN * STEPX + DIST * FISEN
C      END IF
C
      write(fu6,*) 'DESPMAG = ',despmag
C
C     IF (LGS(30).GT.0) WRITE (6,2100) SS(IS), XXX2*180.0D0/PI
C     IF (LGS(30).LE.0) WRITE (6,2100) S, XXX2*180.0D0/PI
      IF (LGS(30).GT.0) WRITE (6,2100) SS(IS)/GUFAC6,XXX2*180.0D0/PI    0405JZ07
      IF (LGS(30).LE.0) WRITE (6,2100) S/GUFAC6,XXX2*180.0D0/Pi         0405JZ07
      WRITE (6,2200) ITER,VAG*CKCAL
C
 2100 FORMAT ('S=',f10.5,' ANGLE BETWEEN DX AND THE OPTIMUM DIRECTION',
     *         2X,f10.5)
 2200 FORMAT ('ITERATIONS USED IN VAG OPTIMIZATION:',I4,
     *        ' FINAL VALUE FOR VAG:',f10.5)
C
      RETURN
      END SUBROUTINE dorods 
C
C **********************************************************************
C     ENDRODS
C **********************************************************************
C
      SUBROUTINE  endrods (IOPPLF)
      use perconparam
      use common_inc
      use kintcm, only :ivrp
C
C     THIS SUBROUTINE UPDATES THE GEOMETRY OF THE SYSTEM IF A VRP
C      CALCULATION HAS BEEN PERFORMED.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
C
      IF (IVRP.EQ.1.AND.IOPPLF.EQ.3.AND.LGS(30).LE.0) THEN
         DO 20 I = 1, N3
           X(I) = XNOU(I)
 20      CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE  endrods
C***************************************************************************
C PTWO 
C***************************************************************************
      SUBROUTINE ptwo(NMOD,NEND,LPTBCR,IDBG,FREQ,C,Q,EGRND,EFNTP,
     >                E0,ANCO,SUM)
      use perconparam
C
C This subroutine evaulates the constant term (E ) and the anharmonicity
C                                               0
C coefficient x   , and then calculates the ground state energy and the
C              ij
C fundementalsi with the Perturbation Theory
C
C On input:
C          NMOD     :  number of vibrational normal modes
C          N3TM     :  3 * (maximum number of atoms [i.e., NATOMS])
C          NEND     :  3 * (number of atoms in the species)
C          FREQ     :  normal mode freqnencies
C          C(I,J,K) :  f
C                       ijk
C          Q(I,J)   :  f
C                       iijj
C On output:
C          EGRND    :  the ground state energy
C          EFNTP    :  the fundementals
C
C Called by:
C        NORMOD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDEBUG  
C
C      DOUBLE PRECISION K2,K2B,K2C,K2D
C      DOUBLE PRECISION NUM2
C     The above lines were commented because variables are not used.    0423TA02
      DIMENSION C(N3TM,N3TM,N3TM),Q(N3TM,N3TM),EFNTP(N3TM)
      DIMENSION ANCO(N3TM,N3TM),SUM(9)
      DIMENSION W(N3TM),FREQ(N3TM)
      DATA ZERO/0.0D0/
C
C     FUNCTION STATEMENTS
      DIJK(I,J,K)=(W(I)+W(J)+W(K))*(W(I)-W(J)-W(K))*
     >            (-W(I)+W(J)-W(K))*(-W(I)-W(J)+W(K)) 
C     END OF FUNCTIUON
C
      DO I=1,9
        SUM(I) = 0.0d0
      ENDDO
C
      LDEBUG = (LPTBCR.EQ.13)
C
      EGRND = 0.D0
      DO I = 1, N3TM
         EFNTP(I) = 0.0D0
         DO J = 1, N3TM
           ANCO(I,J) = 0.0D0
         ENDDO
      ENDDO
C
C     Set up frequencies
C
      ISHFT = NEND - NMOD
      DO I = 1, NMOD
         W(I) = FREQ(I+ISHFT)
      ENDDO
C
C     SUM(1) is harmonic ground state
C
      DO I=1,NMOD
         SUM(1)=SUM(1) + W(I)* 0.5D0
      ENDDO
C
C      WRITE (6,*) 'SUM1 = ', SUM(1)*AUTOCM
C
C     Calculate E0
C
      DO I=1,NMOD
         SUM(3)= SUM(3) + 0.375d0*Q(I,I)
         SUM(4)=SUM(4) - 0.4375d0*C(I,I,I)**2/W(I)
      ENDDO
C
C      WRITE (6,*) 'SUM3 = ', SUM(3)*AUTOCM
C      WRITE (6,*) 'SUM4 = ', SUM(4)*AUTOCM
C
      TERM1=0.0D0
      DO 130 I=1,NMOD
        DO 120 J=1,NMOD
          IF (I.NE.J) THEN
            TERM1=0.1875d0*W(I)*C(J,J,I)**2/(4*W(J)**2-W(I)**2)
            SUM(9)= SUM(9) + TERM1
          ENDIF
 120    CONTINUE
 130  CONTINUE
C
C      WRITE (6,*) 'SUM9 = ',SUM(9)*AUTOCM
C
      DO K=3,NMOD
        DO J=2,K-1
           DO I=1,J-1
              VALJA=-0.25d0*C(I,J,K)**2*W(I)*W(J)*W(K)/DIJK(I,J,K)
              SUM(6)= SUM(6) + VALJA
           ENDDO
        ENDDO
      ENDDO
C
C      WRITE (6,*) 'SUM6 = ', SUM(6)*AUTOCM
C
      E0=  SUM(3) + SUM(4) + SUM(6) + SUM(9)
C
C  THESE TERMS FROM XII
C
      DO 250 I = 1,NMOD
        TERM1= 1.50d0*Q(I,I)
        TERM2=-3.75d0*C(I,I,I)**2/W(I)
        DO M=1,NMOD
          IF(M.NE.I)
     >    TERM2=TERM2 - 0.25d0*C(I,I,M)**2*((8*W(I)**2-3*W(M)**2)/
     >         (W(M)*(4*W(I)**2-W(M)**2)))
        ENDDO
        SUM(7)= SUM(7) + TERM2*0.25d0
        SUM(2)= SUM(2) + TERM1*0.25d0
        ANCO(I,I) = TERM1 + TERM2
 250  CONTINUE
C
C      WRITE (6,*) 'SUM7 = ', SUM(7)*AUTOCM
C      WRITE (6,*) 'SUM2 = ', SUM(2)*AUTOCM
C
C  THESE TERMS FROM XIJ
C
      DO 380 J=2,NMOD
        DO 360 I=1,J-1
          VAL0= Q(I,J)
          VAL1=0.0d0
          VAL2=0.0d0
          DO 340 M=1,NMOD
              IF (M.NE.I.AND.M.NE.J) THEN
                  VAL1= VAL1 - C(I,I,M)*C(J,J,M)/W(M)
              ELSEIF (M.EQ.I.OR.M.EQ.J) THEN
                  VAL1= VAL1 - 3.0d0*C(I,I,M)*C(J,J,M)/W(M)
              ENDIF
              ANF=C(I,J,M)
              IF(M.LT.I) ANF = C(M,I,J)
              IF(M.GT.I.AND.M.LT.J) ANF = C(I,M,J)
              IF (M.EQ.I) THEN
                   VAL2A = -2.0d0*C(M,M,J)**2*W(I)/
     >                (4.0d0*W(I)**2-W(J)**2)
              ELSE IF (M.EQ.J) THEN
                   VAL2A = -2.0d0*C(M,M,I)**2*W(J)/
     >                (4.0d0*W(J)**2-W(I)**2)
              ELSE
               VAL2A= -ANF**2*W(M)*(-W(I)**2-W(J)**2+W(M)**2)/
     >                   (2.0d0*DIJK(I,J,M))
              ENDIF
              VAL2= VAL2 + VAL2A
 340      CONTINUE
          XIJ= VAL0 + VAL1 + VAL2
          ANCO(I,J) = XIJ
          SUM(8)=SUM(8) + 0.25D0*XIJ
 360    CONTINUE
 380  CONTINUE
C
C      WRITE (6,*) 'SUM8 = ', SUM(8)*AUTOCM
C
      DO I = 1, NMOD
        DO J = 1, I
           ANCO(I,J) = ANCO(J,I)
        ENDDO
      ENDDO
C
C      EGRND = E0 + SUM(1) + SUM(2) + SUM(7) + SUM(8)
C
      DO I = 1, NMOD
        DO J = I, NMOD
           EGRND = EGRND + 0.25d0*ANCO(I,J)
        ENDDO
      ENDDO
      DO I = 1, NMOD
         EGRND = EGRND + 0.5D0*W(I)
      ENDDO
      EGRND = EGRND + E0
C
      DO I = 1, NMOD
        EFNTP(I) = W(I)
        DO J = 1, NMOD
          IF (I.EQ.J) THEN
            EFNTP(I) = EFNTP(I) + 2.0d0*ANCO(I,I)
          ELSE
            EFNTP(I) = EFNTP(I) + 0.5D0*ANCO(I,J)
          ENDIF
        ENDDO
      ENDDO
c
c     JUST CHECK FOR FUNDALMENTALS ALONG THE PATH
c      DO  I = 1, NMOD
c         FREQ(I+ISHFT) =  EFNTP(I)
c      ENDDO
c
      IF (LDEBUG) THEN
         WRITE (IDBG,*) 'NMOD = ', NMOD
         WRITE (IDBG,1600)
         L = 0
         DO 80 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1800) L,W(I)*AUTOCM,ZERO,ZERO,W(I)*AUTOCM
   80    CONTINUE
         L = 0
         DO 81 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1700) L,L,ZERO,ANCO(I,I)*AUTOCM,ZERO,
     *                        ANCO(I,I)*AUTOCM
   81    CONTINUE
         IF (NMOD.EQ.1) GO TO 84
         DO 82 I = NMOD, 2, -1
            L = NMOD - I + 1
            JSTART = I - 1
            DO 83 J = JSTART, 1, -1
               M = NMOD - J + 1
               WRITE (IDBG,1700) L,M,ZERO,ANCO(I,J)*AUTOCM,
     *                           ZERO,ANCO(I,J)*AUTOCM
   83       CONTINUE
   82    CONTINUE
   84    WRITE (IDBG,1900) ZERO, E0*AUTOCM,ZERO,
     *                     E0*AUTOCM
         WRITE(IDBG,2000) EGRND*AUTOCM
         WRITE(IDBG,2100) (EFNTP(I)*AUTOCM,I=1,NMOD)
         WRITE (IDBG,2600)
         L = 0
         DO 280 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2700) L,L,L,6.0D0*C(I,I,I)*AUTOCM
  280    CONTINUE
         IF (NMOD.EQ.1) GO TO 286
         DO 281 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 282 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2700) L,M,M,2.0D0*C(I,J,J)*AUTOCM
  282       CONTINUE
  281    CONTINUE
         DO 283 I = 1, NMOD-2
            L = NMOD - I + 1
            JSTART = I + 1
            DO 284 J = JSTART, NMOD-1
               M = NMOD - J + 1
               KSTART = J + 1
               DO 285 K = KSTART, NMOD
                  N = NMOD - K + 1
                  WRITE (IDBG,2700) L,M,N,C(I,J,K)*AUTOCM
  285          CONTINUE
  284       CONTINUE
  283    CONTINUE
  286    CONTINUE
         WRITE (IDBG,2800)
         L = 0
         DO 680 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2900) L,L,L,L,24.0D0*Q(I,I)*AUTOCM
  680    CONTINUE
         IF (NMOD.EQ.1) GO TO 383
         DO 381 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 382 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2900) L,L,M,M,4.0D0*Q(I,J)*AUTOCM
  382       CONTINUE
  381    CONTINUE
  383    CONTINUE
      ENDIF
      RETURN
1600  FORMAT(/,'Perturbation Theory anharmonicity analysis',
     */,/,1X,'Spectroscopic',2X,'Harmonic',4X,'Anharmonic',4X,
     *'Coriolis',5X,'Total',/,' constant (cm**-1)',2X,'part',10X,
     *'part',9X,'part',/)
1700  FORMAT(1X,'X(',I2,',',I2,')',2X,4(1X,F12.3))
1800  FORMAT(1X,'W(',I2,')',5X,4(1X,F12.3))
1900  FORMAT(/,1X,'E-CONST',3X,4(1X,F12.3))
2000  FORMAT(/,1X,'The zero point energy is ',F9.3)
2100  FORMAT(1X,'The fundementals are ',/,(11X,4(1X,F12.3)))
2600  FORMAT(/,' Cubic potential constants in reduced normal',
     *         ' coordinates')
2700  FORMAT(1X,'F(',I2,',',I2,',',I2,')',3X,F16.8)
2800  FORMAT(/,' Quartic potential constants in reduced normal',
     *         ' coordinates')
2900  FORMAT(1X,'F(',I2,',',I2,',',I2,',',I2,')',3X,F16.8)
      END SUBROUTINE ptwo

C***************************************************************************
C DPTWO 
C***************************************************************************
      SUBROUTINE dptwo(NMOD,NEND,LPTBCR,IDBG,FREQ,C,Q,EGRND,EFNTP,
     >                E0,ANCO,SUM)
      use perconparam
C
C This subroutine evaulates the constant term (E ) and the anharmonicity
C                                               0
C coefficient x   , and then calculates the ground state energy and the
C              ij
C fundementalsi with the Degenerate Perturbation Theory
C
C On input:
C          NMOD     :  number of vibrational normal modes
C          N3TM     :  3 * (maximum number of atoms [i.e., NATOMS])
C          NEND     :  3 * (number of atoms in the species)
C          FREQ     :  normal mode freqnencies
C          C(I,J,K) :  f
C                       ijk
C          Q(I,J)   :  f
C                       iijj
C On output:
C          EGRND    :  the ground state energy
C          EFNTP    :  the fundementals
C
C Called by:
C        NORMOD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDEBUG  
C
C      DOUBLE PRECISION K2,K2B,K2C,K2D
C      DOUBLE PRECISION NUM2
C     The above lines were commented because variables are not used.    0423TA02
      DIMENSION C(N3TM,N3TM,N3TM),Q(N3TM,N3TM),EFNTP(N3TM)
      DIMENSION ANCO(N3TM,N3TM),SUM(9)
      DIMENSION W(N3TM),FREQ(N3TM)
      DIMENSION IPAIR(N3TM,4) 
      DATA ZERO/0.0D0/
C
C     FUNCTION STATEMENTS
      DIJK(I,J,K)=(W(I)+W(J)+W(K))*(W(I)-W(J)-W(K))*
     >            (-W(I)+W(J)-W(K))*(-W(I)-W(J)+W(K)) 
C     END OF FUNCTIUON
C
      DO I=1,9
        SUM(I) = 0.0d0
      ENDDO
C
      LDEBUG = (LPTBCR.EQ.13)
C
      EGRND = 0.D0
      DO I = 1, N3TM
         EFNTP(I) = 0.0D0
         DO J = 1, N3TM
           ANCO(I,J) = 0.0D0
         ENDDO
      ENDDO
      WRITE (6,*) 'NMOD = ', NMOD
C
C     Set up frequencies
C
      ISHFT = NEND - NMOD
      DO I = 1, NMOD
         W(I) = FREQ(I+ISHFT)
      ENDDO
C
C     check for degeneracies
C
      DO I = 1, N3TM
         DO J = 1,4 
           IPAIR(I,J) = 0
         ENDDO
      ENDDO
C
      DO I = 1, NMOD
        DO J = 1, NMOD
          IF ((I.NE.J).AND.(IDINT(W(I)*AUTOCM)
     >          .EQ.IDINT(W(J)*AUTOCM))) THEN
             DO K = 1, 4
               IF (IPAIR(I,K).EQ.0) THEN
                    IPAIR(I,K)=J
                    GOTO 998
               ENDIF
             ENDDO
 998         CONTINUE 
          ENDIF
        ENDDO
      ENDDO
c      WRITE (6,*)  'test- degeneracy check'
      DO I = 1, NMOD
        DO K = 1,4
c
c     note: the W(I) is in acesending order and print in descending
c
          IF(IPAIR(I,K).NE.0) THEN 
            WRITE (6,*) 'FREQ ',NMOD-I+1,' is degenerate with FREQ ',
     >          NMOD-IPAIR(I,K)+1
          ENDIF
        ENDDO
      ENDDO
C
C     SUM(1) is harmonic ground state
C
      DO I=1,NMOD
         SUM(1)=SUM(1) + W(I)* 0.5D0
      ENDDO
C
C      WRITE (6,*) 'SUM1 = ', SUM(1)*AUTOCM
C
C     Calculate E0
C
      DO I=1,NMOD
         SUM(3)= SUM(3) + 0.375d0*Q(I,I)
         SUM(4)=SUM(4) - 0.4375d0*C(I,I,I)**2/W(I)
      ENDDO
C
C      WRITE (6,*) 'SUM3 = ', SUM(3)*AUTOCM
C      WRITE (6,*) 'SUM4 = ', SUM(4)*AUTOCM
C
      TERM1=0.0D0
      DO 130 I=1,NMOD
        DO 120 J=1,NMOD
          IF (I.NE.J) THEN
            TERM1=0.1875d0*W(I)*C(J,J,I)**2/(4*W(J)**2-W(I)**2)
            SUM(9)= SUM(9) + TERM1
          ENDIF
 120    CONTINUE
 130  CONTINUE
C
C      WRITE (6,*) 'SUM9 = ',SUM(9)*AUTOCM
C
      DO K=3,NMOD
        DO J=2,K-1
           DO I=1,J-1
              VALJA=-0.25d0*C(I,J,K)**2*W(I)*W(J)*W(K)/DIJK(I,J,K)
              SUM(6)= SUM(6) + VALJA
           ENDDO
        ENDDO
      ENDDO
C
C      WRITE (6,*) 'SUM6 = ', SUM(6)*AUTOCM
C
      E0=  SUM(3) + SUM(4) + SUM(6) + SUM(9)
C
C  THESE TERMS FROM XII
C
      DO 250 I = 1,NMOD
        TERM1= 1.50d0*Q(I,I)
        TERM2=-3.75d0*C(I,I,I)**2/W(I)
        DO M=1,NMOD
          IF(M.NE.I)
     >    TERM2=TERM2 - 0.25d0*C(I,I,M)**2*((8*W(I)**2-3*W(M)**2)/
     >         (W(M)*(4*W(I)**2-W(M)**2)))
        ENDDO
        SUM(7)= SUM(7) + TERM2*0.25d0
        SUM(2)= SUM(2) + TERM1*0.25d0
        ANCO(I,I) = TERM1 + TERM2
 250  CONTINUE
C
C      WRITE (6,*) 'SUM7 = ', SUM(7)*AUTOCM
C      WRITE (6,*) 'SUM2 = ', SUM(2)*AUTOCM
C
C  THESE TERMS FROM XIJ
C
      DO 380 J=2,NMOD
        DO 360 I=1,J-1
          VAL0= Q(I,J)
          VAL1=0.0d0
          VAL2=0.0d0
          DO 340 M=1,NMOD
              IF (M.NE.I.AND.M.NE.J) THEN
                  VAL1= VAL1 - C(I,I,M)*C(J,J,M)/W(M)
              ELSEIF (M.EQ.I.OR.M.EQ.J) THEN
                  VAL1= VAL1 - 3.0d0*C(I,I,M)*C(J,J,M)/W(M)
              ENDIF
              ANF=C(I,J,M)
              IF(M.LT.I) ANF = C(M,I,J)
              IF(M.GT.I.AND.M.LT.J) ANF = C(I,M,J)
              IF (M.EQ.I) THEN
                   VAL2A = -2.0d0*C(M,M,J)**2*W(I)/
     >                (4.0d0*W(I)**2-W(J)**2)
              ELSE IF (M.EQ.J) THEN
                   VAL2A = -2.0d0*C(M,M,I)**2*W(J)/
     >                (4.0d0*W(J)**2-W(I)**2)
              ELSE
               VAL2A= -ANF**2*W(M)*(-W(I)**2-W(J)**2+W(M)**2)/
     >                   (2.0d0*DIJK(I,J,M))
              ENDIF
              VAL2= VAL2 + VAL2A
 340      CONTINUE
          XIJ= VAL0 + VAL1 + VAL2
          ANCO(I,J) = XIJ
          SUM(8)=SUM(8) + 0.25D0*XIJ
 360    CONTINUE
 380  CONTINUE
C
C      WRITE (6,*) 'SUM8 = ', SUM(8)*AUTOCM
C
      DO I = 1, NMOD
        DO J = 1, I
           ANCO(I,J) = ANCO(J,I)
        ENDDO
      ENDDO
C
C      EGRND = E0 + SUM(1) + SUM(2) + SUM(7) + SUM(8)
C
      DO I = 1, NMOD
        DO J = I, NMOD
           EGRND = EGRND + 0.25d0*ANCO(I,J)
        ENDDO
      ENDDO
      DO I = 1, NMOD
         EGRND = EGRND + 0.5D0*W(I)
      ENDDO
      EGRND = EGRND + E0
C
      DO I = 1, NMOD
        EFNTP(I) = W(I)
        DO J = 1, NMOD
          IF (I.EQ.J) THEN
            EFNTP(I) = EFNTP(I) + 2.0d0*ANCO(I,I)
          ELSE
            EFNTP(I) = EFNTP(I) + 0.5D0*ANCO(I,J)
          ENDIF
        ENDDO
      ENDDO
c
c     The following section is used to printing the fundalmentals along the path
c
c      DO  I = 1, NMOD
c         FREQ(I+ISHFT) =  EFNTP(I)
c      ENDDO
c
      IF (LDEBUG) THEN
         WRITE (IDBG,1600)
         L = 0
         DO 80 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1800) L,W(I)*AUTOCM,ZERO,ZERO,W(I)*AUTOCM
   80    CONTINUE
         L = 0
         DO 81 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1700) L,L,ZERO,ANCO(I,I)*AUTOCM,ZERO,
     *                        ANCO(I,I)*AUTOCM
   81    CONTINUE
         IF (NMOD.EQ.1) GO TO 84
         DO 82 I = NMOD, 2, -1
            L = NMOD - I + 1
            JSTART = I - 1
            DO 83 J = JSTART, 1, -1
               M = NMOD - J + 1
               WRITE (IDBG,1700) L,M,ZERO,ANCO(I,J)*AUTOCM,
     *                           ZERO,ANCO(I,J)*AUTOCM
   83       CONTINUE
   82    CONTINUE
   84    WRITE (IDBG,1900) ZERO, E0*AUTOCM,ZERO,
     *                     E0*AUTOCM
         WRITE(IDBG,2000) EGRND*AUTOCM
         WRITE(IDBG,2100) (EFNTP(I)*AUTOCM,I=1,NMOD)
         WRITE (IDBG,2600)
         L = 0
         DO 280 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2700) L,L,L,6.0D0*C(I,I,I)*AUTOCM
  280    CONTINUE
         IF (NMOD.EQ.1) GO TO 286
         DO 281 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 282 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2700) L,M,M,2.0D0*C(I,J,J)*AUTOCM
  282       CONTINUE
  281    CONTINUE
c
         DO 283 I = 1, NMOD-2
            L = NMOD - I + 1
            JSTART = I + 1
            DO 284 J = JSTART, NMOD-1
               M = NMOD - J + 1
               KSTART = J + 1
               DO 285 K = KSTART, NMOD
                  N = NMOD - K + 1
                  WRITE (IDBG,2700) L,M,N,C(I,J,K)*AUTOCM
  285          CONTINUE
  284       CONTINUE
  283    CONTINUE
  286    CONTINUE
         WRITE (IDBG,2800)
         L = 0
         DO 680 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2900) L,L,L,L,24.0D0*Q(I,I)*AUTOCM
  680    CONTINUE
         IF (NMOD.EQ.1) GO TO 383
         DO 381 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 382 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2900) L,L,M,M,4.0D0*Q(I,J)*AUTOCM
  382       CONTINUE
  381    CONTINUE
  383    CONTINUE
      ENDIF
      RETURN
1600  FORMAT(/,'Perturbation Theory anharmonicity analysis',
     */,/,1X,'Spectroscopic',2X,'Harmonic',4X,'Anharmonic',4X,
     *'Coriolis',5X,'Total',/,' constant (cm**-1)',2X,'part',10X,
     *'part',9X,'part',/)
1700  FORMAT(1X,'X(',I2,',',I2,')',2X,4(1X,F12.3))
1800  FORMAT(1X,'W(',I2,')',5X,4(1X,F12.3))
1900  FORMAT(/,1X,'E-CONST',3X,4(1X,F12.3))
2000  FORMAT(/,1X,'The zero point energy is ',F9.3)
2100  FORMAT(1X,'The fundementals are ',/,(11X,4(1X,F12.3)))
2600  FORMAT(/,' Cubic potential constants in reduced normal',
     *         ' coordinates')
2700  FORMAT(1X,'F(',I2,',',I2,',',I2,')',3X,F16.8)
2800  FORMAT(/,' Quartic potential constants in reduced normal',
     *         ' coordinates')
2900  FORMAT(1X,'F(',I2,',',I2,',',I2,',',I2,')',3X,F16.8)
      END SUBROUTINE dptwo

C***************************************************************************
C DCPT
C***************************************************************************
      SUBROUTINE dcpt(NMOD,NEND,LPTBCR,IDBG,FREQ,C,Q,EGRND,EFNTP,
     >                              E0,RXNC,ANCO,SUM)
      use perconparam
C
C This subroutine evaulates the constant term (E ) and the anharmonicity
C                                               0
C coefficient x   , and then calculates the ground state energy and the
C              ij
C fundementalsi with the DCPT2 method.
C
C On input:
C          NMOD     :  number of vibrational normal modes
C          N3TM     :  3 * (maximum number of atoms [i.e., NATOMS])
C          NEND     :  3 * (number of atoms in the species)
C          FREQ     :  normal mode freqnencies
C          C(I,J,K) :  f
C                       ijk
C          Q(I,J)   :  f
C                       iijj
C On output:
C          EGRND    :  the ground state energy
C          EFNTP    :  the fundementals
C
C Called by:
C        NORMOD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDEBUG  
C
      DOUBLE PRECISION K2,K2B,K2C,K2D
      DOUBLE PRECISION NUM2
      DIMENSION C(N3TM,N3TM,N3TM),Q(N3TM,N3TM),EFNTP(N3TM)
      DIMENSION ANCO(N3TM,N3TM),SUM(9)
      DIMENSION W(N3TM),FREQ(N3TM)
      DATA ZERO/0.0D0/
C
C     FUNCTION STATEMENTS
      SUP(SIG,EP,DK) = SIG*(DSQRT(EP**2+DK)-EP)
C     Eq (13)
C      SUP(SIG,EP,DK)= SIG*(DSQRT(EP**2+(DK*EP**2/(DK+EP**2)))-EP)
C     Eq (2)
C      SUP(SIG,EP,DK)= SIG*(DSQRT(EP**2+(DK*EP**4/(DK**2+EP**4)))-EP)
C     NEW TYPE
C      SUP(SIG,EP,DK)= SIG*2.d0*DK*EP/(4.0d0*EP**2+DK)
C     END OF FUNCTIUON
C
      DO I=1,9
        SUM(I) = 0.0d0
      ENDDO
C
      LDEBUG = (LPTBCR.EQ.13)
C
      EGRND = 0.D0
      DO I = 1, N3TM
         EFNTP(I) = 0.0D0
         DO J = 1, N3TM
           ANCO(I,J) = 0.0D0
         ENDDO
      ENDDO
C
C     Set up frequencies
C
      ISHFT = NEND - NMOD
      DO I = 1, NMOD
         W(I) = FREQ(I+ISHFT)
      ENDDO
C

C     SUM(1) is harmonic ground state
C
      DO I=1,NMOD
         SUM(1)=SUM(1) + W(I)* 0.5D0
      ENDDO
C
C     Calculate E0
C
      DO I=1,NMOD
         SUM(3)= SUM(3) + 0.375d0*Q(I,I)
         SUM(4)=SUM(4) - 0.4375d0*C(I,I,I)**2/W(I)
      ENDDO
C
      TERM1=0.0D0
      DO 130 I=1,NMOD
        DO 120 J=1,NMOD
          IF (I.NE.J) THEN
          RE= 0.5d0 * ( 2*W(J)-W(I) )
          RHKSQ=  0.1875d0*W(I)*C(J,J,I)**2/(2*W(J)+W(I))
          E = DABS(RE)
          S = DSIGN(1.0d0,RE)
          TERM1= SUP(S,E,RHKSQ)
          SUM(9)= SUM(9) + TERM1
c just check the term
c        AAA = SUP(S,E,RHKSQ)*AUTOCM
c        BBB = RHKSQ/(2.0d0*RE)*AUTOCM
c        EER = AAA/BBB
c        WRITE (52,989) RXNC,I,J,RE*AUTOCM,RHKSQ*AUTOCM,AAA,BBB,EER
c
          ENDIF
 120    CONTINUE
 130  CONTINUE
C
      DO K=3,NMOD
        DO J=2,K-1
           DO I=1,J-1
              VALJA= - C(I,J,K)**2/(32* (W(I)+W(J)+W(K)))
              K2=  C(I,J,K)**2/32
              BE= 0.5d0*(W(I)-W(J)-W(K))
              E=  DABS(BE)
              ZZ= -BE
              S= DSIGN(1.0d0,ZZ)
              VALJB= SUP(S,E,K2)
              CE= 0.5d0*(-W(I)+W(J)-W(K))
              E= DABS(CE)
              ZZ= -CE
              S = DSIGN(1.0d0,ZZ)
              VALJC= SUP(S,E,K2)
              DE= 0.5d0*(-W(I)-W(J)+W(K))
              E= DABS(DE)
              ZZ= -DE
              S = DSIGN(1.0d0,ZZ)
              VALJD= SUP(S,E,K2)
              SUM(6)= SUM(6) + VALJA + VALJB + VALJC + VALJD
           ENDDO
        ENDDO
      ENDDO
C
      E0=  SUM(3) + SUM(4) + SUM(6) + SUM(9)
C
C  THESE TERMS FROM XII
C
      DO 250 I = 1,NMOD
        TERM1= 1.50d0*Q(I,I)
        TERM2=-3.75d0*C(I,I,I)**2/W(I)
        DO 240 M=1,NMOD
          IF(M.EQ.I) GO TO 240
          RE= 0.5d0*(2*W(I)-W(M))
          E= DABS(RE)
          NUM2= 4.0d0*W(M)*(2*W(I)+W(M))
          RHKSQ= -1.0d0*C(I,I,M)**2 *(8*W(I)**2-3*W(M)**2)/NUM2
          K2= DABS(RHKSQ)
          ZZ= RHKSQ * RE
          S = DSIGN(1.0d0,ZZ)
          TERM2=TERM2 + SUP(S,E,K2)
c  just check the term
c        AAA = SUP(S,E,K2)*AUTOCM
c        BBB = RHKSQ/(2.0d0*RE)*AUTOCM
c        EER = AAA/BBB
c        WRITE (54,989) RXNC,I,M,RE*AUTOCM,RHKSQ*AUTOCM,AAA,BBB,EER
c989     FORMAT (1X,F7.4,2I3,4F15.9,F6.3)
c
 240    CONTINUE
        SUM(7)= SUM(7) + TERM2*0.25d0
        SUM(2)= SUM(2) + TERM1*0.25d0
        ANCO(I,I) = TERM1 + TERM2
 250  CONTINUE
C
C  THESE TERMS FROM XIJ
C
      DO 380 J=2,NMOD
        DO 360 I=1,J-1
          VAL0= Q(I,J)
          VAL1=0.0D0
          RE=0.5D0*(2.0D0*W(I)-W(J))
          RHKSQ=2.0d0*(W(I)*C(I,I,J)**2)/(2.D0*W(I)+W(J))
          E=DABS(RE)
          S=DSIGN(1.0d0,-RE)
          VAL2=SUP(S,E,RHKSQ)
          RE=0.5D0*(2.D0*W(J)-W(I))
          RHKSQ=2.0d0*(W(J)*C(J,J,I)**2)/(2.D0*W(J)+W(I))
          E=DABS(RE)
          S=DSIGN(1.D0,-RE)
          VAL2 = VAL2 + SUP(S,E,RHKSQ)
            DO 340 M=1,NMOD
              IF (M.NE.I.AND.M.NE.J) THEN
                  VAL1= VAL1 - C(I,I,M)*C(J,J,M)/W(M)
              ELSEIF (M.EQ.I.OR.M.EQ.J) THEN
                  VAL1= VAL1 - 3.0d0*C(I,I,M)*C(J,J,M)/W(M)
              ENDIF
              IF(M.EQ.I.OR.M.EQ.J) GO TO 340
              ANF=C(I,J,M)
              IF(M.LT.I) ANF = C(M,I,J)
              IF(M.GT.I.AND.M.LT.J) ANF = C(I,M,J)
              VAL2A= - ANF**2/(8*(W(I)+W(J)+W(M)))
              K2B= ANF**2/8.0d0
              E2B= 0.5d0*(W(I)-W(J)-W(M))
              S = DSIGN(1.0d0,E2B)
              K2= K2B
              E = DABS(E2B)
              VAL2B = SUP(S,E,K2)
              K2C= ANF**2/8.0d0
              E2C= 0.5d0 * (-W(I)+W(J)-W(M))
              K2= K2C
              E= DABS(E2C)
              S = DSIGN(1.0d0,E2C)
              VAL2C = SUP(S,E,K2)
              K2D= -ANF**2/8.0d0
              E2D= 0.5d0 * (-W(I)-W(J)+W(M))
              ZZ= K2D *E2D
              K2= ABS(K2D)
              E= DABS(E2D)
              S =DSIGN(1.0d0,ZZ)
              VAL2D= SUP(S,E,K2)
              VAL2= VAL2 + VAL2A + VAL2B + VAL2C + VAL2D
 340        CONTINUE
          XIJ= VAL0 + VAL1 + VAL2
          ANCO(I,J) = XIJ
          SUM(8)=SUM(8) + 0.25D0*XIJ
 360    CONTINUE
 380  CONTINUE
C
      DO I = 1, NMOD
        DO J = 1, I
           ANCO(I,J) = ANCO(J,I)
        ENDDO   
      ENDDO   
C
      DO I = 1, NMOD
        DO J = I, NMOD
           EGRND = EGRND + 0.25d0*ANCO(I,J)
        ENDDO
      ENDDO
      DO I = 1, NMOD
         EGRND = EGRND + 0.5D0*W(I)
      ENDDO
      EGRND = EGRND + E0
c
      DO I = 1, NMOD
        EFNTP(I) = W(I)
        DO J = 1, NMOD
          IF (I.EQ.J) THEN
            EFNTP(I) = EFNTP(I) + 2.0d0*ANCO(I,I)
          ELSE
            EFNTP(I) = EFNTP(I) + 0.5D0*ANCO(I,J)
          ENDIF
        ENDDO
      ENDDO
c
c     JUST CHECK FOR FUNDALMENTALS ALONG THE PATH
c      DO  I = 1, NMOD
c         FREQ(I+ISHFT) =  EFNTP(I)
c      ENDDO
c
      IF (LDEBUG) THEN
         WRITE (IDBG,*) 'NMOD = ', NMOD
         WRITE (IDBG,1600)
         L = 0
         DO 80 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1800) L,W(I)*AUTOCM,ZERO,ZERO,W(I)*AUTOCM
   80    CONTINUE
         L = 0
         DO 81 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1700) L,L,ZERO,ANCO(I,I)*AUTOCM,ZERO,
     *                        ANCO(I,I)*AUTOCM
   81    CONTINUE
         IF (NMOD.EQ.1) GO TO 84
         DO 82 I = NMOD, 2, -1
            L = NMOD - I + 1
            JSTART = I - 1
            DO 83 J = JSTART, 1, -1
               M = NMOD - J + 1
               WRITE (IDBG,1700) L,M,ZERO,ANCO(I,J)*AUTOCM,
     *                           ZERO,ANCO(I,J)*AUTOCM
   83       CONTINUE
   82    CONTINUE
   84    WRITE (IDBG,1900) ZERO, E0*AUTOCM,ZERO,
     *                     E0*AUTOCM
         WRITE(IDBG,2000) EGRND*AUTOCM
         WRITE(IDBG,2100) (EFNTP(I)*AUTOCM,I=1,NMOD)
         WRITE (IDBG,2600)
         L = 0
         DO 280 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2700) L,L,L,6.0D0*C(I,I,I)*AUTOCM
  280    CONTINUE
         IF (NMOD.EQ.1) GO TO 286
         DO 281 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 282 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2700) L,M,M,2.0D0*C(I,J,J)*AUTOCM
  282       CONTINUE
  281    CONTINUE
         DO 283 I = 1, NMOD-2
            L = NMOD - I + 1
            JSTART = I + 1
            DO 284 J = JSTART, NMOD-1
               M = NMOD - J + 1
               KSTART = J + 1
               DO 285 K = KSTART, NMOD
                  N = NMOD - K + 1
                  WRITE (IDBG,2700) L,M,N,C(I,J,K)*AUTOCM
  285          CONTINUE
  284       CONTINUE
  283    CONTINUE
  286    CONTINUE
         WRITE (IDBG,2800)
         L = 0
         DO 680 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2900) L,L,L,L,24.0D0*Q(I,I)*AUTOCM
  680    CONTINUE
         IF (NMOD.EQ.1) GO TO 383
         DO 381 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 382 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2900) L,L,M,M,4.0D0*Q(I,J)*AUTOCM
  382       CONTINUE
  381    CONTINUE
  383    CONTINUE
      ENDIF
      RETURN
1600  FORMAT(/,'Degeneracy-Corrected Perturbation Theory anharmonicity 
     *analysis',/,/,1X,'Spectroscopic',2X,'Harmonic',4X,'Anharmonic',4X,
     *'Coriolis',5X,'Total',/,' constant (cm**-1)',2X,'part',10X,
     *'part',9X,'part',/)
1700  FORMAT(1X,'X(',I2,',',I2,')',2X,4(1X,F12.3))
1800  FORMAT(1X,'W(',I2,')',5X,4(1X,F12.3))
1900  FORMAT(/,1X,'E-CONST',3X,4(1X,F12.3))
2000  FORMAT(/,1X,'The zero point energy is ',F9.3)
2100  FORMAT(1X,'The fundementals are ',/,(11X,4(1X,F12.3)))
2600  FORMAT(/,' Cubic potential constants in reduced normal',
     *         ' coordinates')
2700  FORMAT(1X,'F(',I2,',',I2,',',I2,')',3X,F16.8)
2800  FORMAT(/,' Quartic potential constants in reduced normal',
     *         ' coordinates')
2900  FORMAT(1X,'F(',I2,',',I2,',',I2,',',I2,')',3X,F16.8)
      END SUBROUTINE dcpt

C***************************************************************************
C NDCPT
C***************************************************************************
      SUBROUTINE ndcpt(NMOD,NEND,LPTBCR,IDBG,FREQ,C,Q,EGRND,EFNTP,
     >                              E0,ANCO,SUM,RXNC,ISEL)
      use perconparam
C
C This subroutine evaulates the constant term (E ) and the anharmonicity
C                                               0
C coefficient x   , and then calculates the ground state energy and the
C              ij
C fundementalsi with the DCPT2 method.
C
C On input:
C          NMOD     :  number of vibrational normal modes
C          N3TM     :  3 * (maximum number of atoms [i.e., NATOMS])
C          NEND     :  3 * (number of atoms in the species)
C          FREQ     :  normal mode freqnencies
C          C(I,J,K) :  f
C                       ijk
C          Q(I,J)   :  f
C                       iijj
C On output:
C          EGRND    :  the ground state energy
C          EFNTP    :  the fundementals
C
C Called by:
C        NORMOD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDEBUG  
C
      DOUBLE PRECISION K2,K2B,K2C,K2D
      DOUBLE PRECISION NUM2
      DIMENSION C(N3TM,N3TM,N3TM),Q(N3TM,N3TM),EFNTP(N3TM)
      DIMENSION ANCO(N3TM,N3TM),SUM(9)
      DIMENSION W(N3TM),FREQ(N3TM)
      DATA ZERO/0.0D0/
C
C     FUNCTION STATEMENTS
        SUP(EP,DK)= 2.d0*DK*EP/(4.0d0*EP**2+DK)
C     END OF FUNCTIUON
C
      DO I=1,9
        SUM(I) = 0.0d0
      ENDDO
C
      LDEBUG = (LPTBCR.EQ.13)
C
      EGRND = 0.D0
      DO I = 1, N3TM
         EFNTP(I) = 0.0D0
         DO J = 1, N3TM
           ANCO(I,J) = 0.0D0
         ENDDO
      ENDDO
C
C     Set up frequencies
C
      ISHFT = NEND - NMOD
      DO I = 1, NMOD
         W(I) = FREQ(I+ISHFT)
      ENDDO
C
C     SUM(1) is harmonic ground state
C
      DO I=1,NMOD
         SUM(1)=SUM(1) + W(I)* 0.5D0
      ENDDO
C      WRITE (6,*) 'SUM1 = ', SUM(1)*AUTOCM
C
C     Calculate E0
C
      DO I=1,NMOD
         SUM(3)= SUM(3) + 0.375d0*Q(I,I)
         SUM(4)=SUM(4) - 0.4375d0*C(I,I,I)**2/W(I)
      ENDDO
C      WRITE (6,*) 'SUM3 = ', SUM(3)*AUTOCM
C      WRITE (6,*) 'SUM4 = ', SUM(4)*AUTOCM
      TERM1=0.0D0
      DO 130 I=1,NMOD
        DO 120 J=1,NMOD
          IF (I.NE.J) THEN
          RE= 0.5d0 * ( 2*W(J)-W(I) )
          RHKSQ=  0.1875d0*W(I)*C(J,J,I)**2/(2*W(J)+W(I))
          TERM1= SUP(RE,RHKSQ)
          SUM(9)= SUM(9) + TERM1
c just check the term
c          AAA = SUP(RE,RHKSQ)*AUTOCM
c          BBB = RHKSQ/(2.0d0*RE)*AUTOCM
c          EER = AAA/BBB
c          WRITE (52,989) RXNC,I,J,RE*AUTOCM,RHKSQ*AUTOCM,AAA,BBB,EER
c
          ENDIF
 120    CONTINUE
 130  CONTINUE
C
C      WRITE (6,*) 'SUM9 = ',SUM(9)*AUTOCM
C
      DO K=3,NMOD
        DO J=2,K-1
           DO I=1,J-1
              VALJA= - C(I,J,K)**2/(32* (W(I)+W(J)+W(K)))
              K2=  C(I,J,K)**2/32
              BE= -0.5d0*(W(I)-W(J)-W(K))
              VALJB= SUP(BE,K2)
              CE= -0.5d0*(-W(I)+W(J)-W(K))
              VALJC= SUP(CE,K2)
              DE= -0.5d0*(-W(I)-W(J)+W(K))
              VALJD= SUP(DE,K2)
              SUM(6)= SUM(6) + VALJA + VALJB + VALJC + VALJD
           ENDDO
        ENDDO
      ENDDO
C
C      WRITE (6,*) 'SUM6 = ', SUM(6)*AUTOCM      
C
      E0=  SUM(3) + SUM(4) + SUM(6) + SUM(9)
C
C  THESE TERMS FROM XII
C
      DO 250 I = 1,NMOD
        TERM1= 1.50d0*Q(I,I)
        TERM2=-3.75d0*C(I,I,I)**2/W(I)
        DO 240 M=1,NMOD
          IF(M.EQ.I) GO TO 240
c
c new form 
c
        RE= 0.5d0*(2*W(I)-W(M))
        NUM2= 4.0d0*W(M)*(2*W(I)+W(M))
        RHKSQ= C(I,I,M)**2*ABS((8*W(I)**2-3*W(M)**2))/NUM2
        SIG = SIGN(1.0d0,(3*W(M)**2-8*W(I)**2))
        TERM2=TERM2 + SIG*SUP(RE,RHKSQ)
c just check the term
c        AAA = SIG*SUP(RE,RHKSQ)*AUTOCM
c        BBB = SIG*RHKSQ/(2.0d0*RE)*AUTOCM
c        EER = AAA/BBB
c        WRITE (54,989) RXNC,I,M,RE*AUTOCM,RHKSQ*AUTOCM,AAA,BBB,EER
c989     FORMAT (1X,F7.4,2I3,4F15.7,F6.3)
c
 240    CONTINUE
        SUM(7)= SUM(7) + TERM2*0.25d0
        SUM(2)= SUM(2) + TERM1*0.25d0
        ANCO(I,I) = TERM1 + TERM2
 250  CONTINUE
C
C      WRITE (6,*) 'SUM7 = ', SUM(7)*AUTOCM
C      WRITE (6,*) 'SUM2 = ', SUM(2)*AUTOCM 
C
C  THESE TERMS FROM XIJ
C
      DO 380 J=2,NMOD
        DO 360 I=1,J-1
          VAL0= Q(I,J)
          VAL1=0.0D0
          RE=-0.5D0*(2.0D0*W(I)-W(J))
          RHKSQ=2.0d0*(W(I)*C(I,I,J)**2)/(2.D0*W(I)+W(J))
          VAL2=SUP(RE,RHKSQ)
          RE=-0.5D0*(2.D0*W(J)-W(I))
          RHKSQ=2.0d0*(W(J)*C(J,J,I)**2)/(2.D0*W(J)+W(I))
          VAL2 = VAL2 + SUP(RE,RHKSQ)
            DO 340 M=1,NMOD
              IF (M.NE.I.AND.M.NE.J) THEN
                  VAL1= VAL1 - C(I,I,M)*C(J,J,M)/W(M)
              ELSEIF (M.EQ.I.OR.M.EQ.J) THEN
                  VAL1= VAL1 - 3.0d0*C(I,I,M)*C(J,J,M)/W(M)
              ENDIF
              IF(M.EQ.I.OR.M.EQ.J) GO TO 340
              ANF=C(I,J,M)
              IF(M.LT.I) ANF = C(M,I,J)
              IF(M.GT.I.AND.M.LT.J) ANF = C(I,M,J)
              VAL2A= -ANF**2/(8*(W(I)+W(J)+W(M)))
              K2B= ANF**2/8.0d0
              E2B= 0.5d0*(W(I)-W(J)-W(M))
              VAL2B = SUP(E2B,K2B)
              K2C= ANF**2/8.0d0
              E2C= 0.5d0*(-W(I)+W(J)-W(M))
              VAL2C = SUP(E2C,K2C)
              K2D= ANF**2/8.0d0
              E2D= 0.5d0*(W(I)+W(J)-W(M))
              VAL2D= SUP(E2D,K2D)
              VAL2= VAL2 + VAL2A + VAL2B + VAL2C + VAL2D
 340        CONTINUE
          XIJ= VAL0 + VAL1 + VAL2
          ANCO(I,J) = XIJ
          SUM(8)=SUM(8) + 0.25D0*XIJ
 360    CONTINUE
 380  CONTINUE
C
C      WRITE (6,*) 'SUM8 = ', SUM(8)*AUTOCM
C
      DO I = 1, NMOD
        DO J = 1, I
           ANCO(I,J) = ANCO(J,I)
        ENDDO   
      ENDDO   
C
C      EGRND = E0 + SUM(1) + SUM(2) + SUM(7) + SUM(8)
C
      DO I = 1, NMOD
        DO J = I, NMOD
           EGRND = EGRND + 0.25d0*ANCO(I,J)
        ENDDO
      ENDDO
      DO I = 1, NMOD
         EGRND = EGRND + 0.5D0*W(I)
      ENDDO
      EGRND = EGRND + E0
C
      DO I = 1, NMOD
        EFNTP(I) = W(I)
        DO J = 1, NMOD
          IF (I.EQ.J) THEN
            EFNTP(I) = EFNTP(I) + 2.0d0*ANCO(I,I)
          ELSE
            EFNTP(I) = EFNTP(I) + 0.5D0*ANCO(I,J)
          ENDIF
        ENDDO
      ENDDO
c
c     JUST CHECK FOR FUNDALMENTALS ALONG THE PATH
c      DO  I = 1, NMOD
c         FREQ(I+ISHFT) =  EFNTP(I)
c      ENDDO
c
      IF (LDEBUG) THEN
         WRITE (IDBG,*) 'NMOD = ', NMOD
         WRITE (IDBG,1600)
         L = 0
         DO 80 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1800) L,W(I)*AUTOCM,ZERO,ZERO,W(I)*AUTOCM
   80    CONTINUE
         L = 0
         DO 81 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,1700) L,L,ZERO,ANCO(I,I)*AUTOCM,ZERO,
     *                        ANCO(I,I)*AUTOCM
   81    CONTINUE
         IF (NMOD.EQ.1) GO TO 84
         DO 82 I = NMOD, 2, -1
            L = NMOD - I + 1
            JSTART = I - 1
            DO 83 J = JSTART, 1, -1
               M = NMOD - J + 1
               WRITE (IDBG,1700) L,M,ZERO,ANCO(I,J)*AUTOCM,
     *                           ZERO,ANCO(I,J)*AUTOCM
   83       CONTINUE
   82    CONTINUE
   84    WRITE (IDBG,1900) ZERO, E0*AUTOCM,ZERO,
     *                     E0*AUTOCM
         WRITE(IDBG,2000) EGRND*AUTOCM
         WRITE(IDBG,2100) (EFNTP(I)*AUTOCM,I=1,NMOD)
         WRITE (IDBG,2600)
         L = 0
         DO 280 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2700) L,L,L,6.0D0*C(I,I,I)*AUTOCM
  280    CONTINUE
         IF (NMOD.EQ.1) GO TO 286
         DO 281 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 282 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2700) L,M,M,2.0D0*C(I,J,J)*AUTOCM
  282       CONTINUE
  281    CONTINUE
         DO 283 I = 1, NMOD-2
            L = NMOD - I + 1
            JSTART = I + 1
            DO 284 J = JSTART, NMOD-1
               M = NMOD - J + 1
               KSTART = J + 1
               DO 285 K = KSTART, NMOD
                  N = NMOD - K + 1
                  WRITE (IDBG,2700) L,M,N,C(I,J,K)*AUTOCM
  285          CONTINUE
  284       CONTINUE
  283    CONTINUE
  286    CONTINUE
         WRITE (IDBG,2800)
         L = 0
         DO 680 I = NMOD, 1, -1
            L = L + 1
            WRITE (IDBG,2900) L,L,L,L,24.0D0*Q(I,I)*AUTOCM
  680    CONTINUE
         IF (NMOD.EQ.1) GO TO 383
         DO 381 I = 1, NMOD-1
            L = NMOD - I + 1
            JSTART = I + 1
            DO 382 J = JSTART, NMOD
               M = NMOD - J + 1
               WRITE (IDBG,2900) L,L,M,M,4.0D0*Q(I,J)*AUTOCM
  382       CONTINUE
  381    CONTINUE
  383    CONTINUE
      ENDIF
      RETURN
1600  FORMAT(/,'New Degeneracy-Corrected Perturbation Theory'
     *,' anharmonicity analysis',/,/,1X,'Spectroscopic',2X,'Harmonic'
     *,4X,'Anharmonic',4X,
     *'Coriolis',5X,'Total',/,' constant (cm**-1)',2X,'part',10X,
     *'part',9X,'part',/)
1700  FORMAT(1X,'X(',I2,',',I2,')',2X,4(1X,F12.3))
1800  FORMAT(1X,'W(',I2,')',5X,4(1X,F12.3))
1900  FORMAT(/,1X,'E-CONST',3X,4(1X,F12.3))
2000  FORMAT(/,1X,'The zero point energy is ',F9.3)
2100  FORMAT(1X,'The fundementals are ',/,(11X,4(1X,F12.3)))
2600  FORMAT(/,' Cubic potential constants in reduced normal',
     *         ' coordinates')
2700  FORMAT(1X,'F(',I2,',',I2,',',I2,')',3X,F16.8)
2800  FORMAT(/,' Quartic potential constants in reduced normal',
     *         ' coordinates')
2900  FORMAT(1X,'F(',I2,',',I2,',',I2,',',I2,')',3X,F16.8)
      END SUBROUTINE ndcpt

c
C
C***********************************************************************
C     ASYMALP 
C***********************************************************************
C
      SUBROUTINE asymalp (XM,YM,ZM,PVEC,ALPHA)
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      DIMENSION ALPHA(3,3),PVEC(3,3)
      DIMENSION XM(3),YM(3),ZM(3),PT(3)
c
      DO I = 1,3
        DO J = 1,3
          PT(J) = PVEC(J,I)
        ENDDO
c        write (6,*) 'PT = ',(PT(K),K=1,3)
        CALL dotprd(XM,PT,ALPHA(1,I),3)
        CALL dotprd(YM,PT,ALPHA(2,I),3)
        CALL dotprd(ZM,PT,ALPHA(3,I),3)
      ENDDO
c check determinent
      DET1 = ALPHA(1,1)*(ALPHA(2,2)*ALPHA(3,3)-Alpha(2,3)*ALPHA(3,2))
      det2 = ALPHA(1,2)*(alpha(2,1)*alpha(3,3)-alpha(2,3)*aLPHA(3,1))
      DET3 = ALPHA(1,3)*(ALPHA(2,1)*ALPHA(3,2)-ALPHA(2,2)*ALPHA(3,1))
      det = det1 - det2 + det3
c      write (6,*) 'Determinent = ',det       
      return
      end SUBROUTINE asymalp
c
C***********************************************************************
C     ASYMCAL
C***********************************************************************
c
      SUBROUTINE asymcal (natm,iatom,x,amass,ngm,MG,XM,YM,ZM,AM,BM,CM,UM
     >                    ,PRGM)
      use perconparam
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      DIMENSION X(3*NATM),AMASS(3*NATM)
      DIMENSION MG(NGM)
      DIMENSION XNEW(3),XTEMP(NATOMS,3)
C      DIMENSION XTEMPX(NATOMS,3)
C     The above line was commented because variable is not used.        0423TA02
      DIMENSION PRGM(3),IATOM(NATM)
      DIMENSION XM(3),YM(3),ZM(3)
c
1992  format (3F15.7)
c
c  rotate all for printing, the origin of XM,YM,ZM coordinates is the center
c  of mass of the gyrator
c
      DO I = 1,NGM
        DO J = 1,3
          XTEMP(I,J) = X(3*(MG(I)-1)+J)-PRGM(J)
        ENDDO
      ENDDO
      DO I = 1,NGM
         DO J = 1,3
           XNEW(J) = XTEMP(I,J)
         ENDDO 
         CALL dotprd(XM,XNEW,XTN,3) 
         CALL dotprd(YM,XNEW,YTN,3)
         CALL dotprd(ZM,XNEW,ZTN,3)
         XTEMP(I,1) = XTN
         XTEMP(I,2) = YTN
         XTEMP(I,3) = ZTN 
      ENDDO
      AM = 0.0d0
      BM = 0.0d0
      CM = 0.0d0
      UM = 0.0d0
      DO I = 1,NGM
c        write (6,*) 'for group ',i,AMASS(3*MG(I))**2
        AM = AM + AMASS(3*MG(I))**2*(XTEMP(I,1)**2+XTEMP(I,2)**2)
        BM = BM + AMASS(3*MG(I))**2*(XTEMP(I,1)*XTEMP(I,3))
        CM = CM + AMASS(3*MG(I))**2*(XTEMP(I,2)*XTEMP(I,3))
        UM = UM + AMASS(3*MG(I))**2*XTEMP(I,1)
      ENDDO
      return
      end SUBROUTINE asymcal
c
C***********************************************************************
c     ASYMENT
C***********************************************************************
c
      subroutine asyment (left,natm,iatom,totm,x,amass,berot,pvec)
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      DIMENSION X(3*NATM),AMASS(3*NATM)
      DIMENSION XCM(3),IATOM(NATM)
      DIMENSION ROT(3,3),BEROT(3),PVEC(3,3)
      DIMENSION SCR1(3),SCR2(3),SCR3(3)
      LOGICAL   LEFT,LCMP(3)
c
      DIMENSION IMAX(3),VMAX(3),TPVEC(3,3),TBEROT(3)
c
c     calc COM and move to COM
c
      IBEG = 1
      IEND = NATM
c
c      TOTM = 0.D00
c      DO I = IBEG, IEND
c         TOTM = TOTM+AMASS(3*IATOM(I))**2
c      ENDDO
c
c      write (6,*) 'TOTM = ',TOTM
c
      DO K = 1, 3
         SUM = 0.D00
         DO I = IBEG, IEND
            SUM = SUM+X(3*(IATOM(I)-1)+K)*AMASS(3*(IATOM(I)-1)+K)**2
         ENDDO
         XCM(K) = SUM/TOTM
         DO I = K, 3*NATM, 3
            X(I) = X(I)-XCM(K)
         ENDDO
      ENDDO
c      write (6,*) 'COM = ',(XCM(I),I=1,3)
1992  format (3F15.7)
c      write (6,*) 'Center of mass coord in ang '
c      write (6,1992) (X(I)*.5292d0,I=1,3*NATM)
C
C     calc MOMENT OF INERTIA TENSOR
C
      DO I = 1, 3
        DO J = 1, 3
            ROT(I,J) = 0.0D0
        ENDDO
      ENDDO
      DO I = IBEG, IEND
            L = 3*IATOM(I)-2
            ROT(1,1) = ROT(1,1)+(X(L+1)**2+
     *                 X(L+2)**2)*AMASS(L)**2
            ROT(1,2) = ROT(1,2)-X(L)*X(L+1)*AMASS(L)**2
            ROT(1,3) = ROT(1,3)-X(L)*X(L+2)*AMASS(L)**2
            ROT(2,2) = ROT(2,2)+(X(L)**2+
     *                 X(L+2)**2)*AMASS(L)**2
            ROT(2,3) = ROT(2,3)-X(L+1)*X(L+2)*AMASS(L)**2
            ROT(3,3) = ROT(3,3)+(X(L)**2+
     *                 X(L+1)**2)*AMASS(L)**2
      ENDDO
      ROT(2,1) = ROT(1,2)
      ROT(3,1) = ROT(1,3)
      ROT(3,2) = ROT(2,3)
c 
c      write (6,*) 'ROT'
c      DO I = 1,3
c         write (6,1999) (ROT(I,J),J=1,3)
c      ENDDO
c
c  to obtain the principal axes, need to diagonalize the inertia tensor
c
      DO I = 1, 3
        BEROT(I) = 0.D0
        DO J = 1, 3
          PVEC(I,J) = 0.D0
        ENDDO
      ENDDO
c
      CALL RSPDRV(3,3,ROT,BEROT,1,PVEC,SCR1,SCR2,IERR)
c
c      write (6,*) 'berot'
c      write(6,1998) BEROT(1),BEROT(2),BEROT(3)
c      write (6,*) 'PVEC old'
c      do i = 1,3
c        WRITE(6,1999) (PVEC(I,J),J=1,3)
c      enddo
c 
C Below commented by J. Zheng May 19 2010
C
c  swap pvec 
c     assume X: 1, Y: 2, Z: 3
c  so we always end up with either left handed or right handed coordinate
c
c
c     DO I = 1,3 
c       IMAX(I) = 0
c       VMAX(I) = 0.0d0
c     ENDDO
c
c  do search
c
c     DO I = 1,3
c       DO J = 1,3
c         IF (ABS(PVEC(J,I)).GE.VMAX(I)) THEN
c            VMAX(I) = ABS(PVEC(J,I))
c            IMAX(I) = J
c         ENDIF
c       ENDDO
c     ENDDO
c      write (6,*) 'Order of pvec ',(IMAX(I),I=1,3)
c     DO I = 1,3
c       TBEROT(IMAX(I)) = BEROT(I)
c       DO J = 1,3
c         TPVEC(J,IMAX(I)) =  PVEC(J,I)
c       ENDDO
c     ENDDO
c     DO I = 1,3
c       BEROT(I) = TBEROT(I)
c       BEROT(I) = TBEROT(IMAX(I))                                      0519JZ10
c       DO J = 1,3
c         PVEC(J,I) =  TPVEC(J,I)
c       ENDDO
c     ENDDO
C --end the comment
c
c  check if is right handed
c
      DO I = 1,3
        SCR2(I) = PVEC(I,2)
        SCR3(I) = PVEC(I,3)
      ENDDO
      CALL XPROD(SCR2,SCR3,SCR1)
c      write (6,*) 'SCR1 = ',(SCR1(J),J=1,3)
c      write (6,*) 'PVEC1 =',(PVEC(J,1),J=1,3)
c   MAKE IT ALWAYS RIGHT HANDED                                            0509BE05
c        
      DO I = 1,3
        PVEC(I,1) = SCR1(I)
      ENDDO 
      LEFT = .false.
c    No need to check whether right or left handed                         0509BE05
c      DO I = 1,3
c        IF (((SCR1(I).LT.0.0d0).and.(PVEC(I,1).LT.0.0d0)).or.
c     >      ((SCR1(I).GT.0.0d0).and.(PVEC(I,1).GT.0.0d0)).or.
c     >      ((SCR1(I).EQ.0.0d0).and.(PVEC(I,1).EQ.0.0d0))) THEN
c          LCMP(I) = .true.
c        ELSE
c          LCMP(I) = .false.
c        ENDIF
c      ENDDO
c      IF (LCMP(1).AND.LCMP(2).AND.LCMP(3)) THEN
c          LEFT = .false.
c      ELSE
c          LEFT = .true.
c     Make it always right handed BE
c      DO I = 1,3
c        TBEROT(I) = BEROT(I)
c        DO J = 1,3
c          TPVEC(J,I) =  PVEC(J,I)
c        ENDDO
c      ENDDO 
c     Switch 1 and 2
c      BEROT(1) = TBEROT(2)
c      BEROT(2) = TBEROT(1)
c      DO I = 1,3
c        PVEC(I,1) = TPVEC(I,2)
c        PVEC(I,2) = TPVEC(I,1)
c      ENDDO
c          LEFT = .false.
c      ENDIF
c      write (6,*) 'LEFT handed ? ',LEFT
c      write (6,*) 'Principal moments ',BEROT(1),BEROT(2),BEROT(3)
c      write (6,*) 'Reduced Mass ',REDM
c     
c      write(6,1997)
c 1997 format('Principal moments of inertia in amu bohr**2 :')
c      write(6,1998) BEROT(1)*REDM,BEROT(2)*REDM,BEROT(3)*REDM
c 1998 format('IA= ',1PE15.6,' IB= ',1PE15.6,' IC= ',1PE15.6)
c      write (6,*) 'PVEC - column is the eigenvector'
c 1999 format(3F15.8) 
c      do i = 1,3
c        WRITE(6,1999) (PVEC(I,J),J=1,3)
c      enddo
c      FMOM = BEROT(1)*BEROT(2)*BEROT(3)*REDM**3
      return
      end subroutine asyment       
c
C***********************************************************************
c     ASYMNZD
C***********************************************************************
c
      SUBROUTINE asymnzd (left,lsym,natm,iatom,x,amass,NGM,IAG,MG,XM,YM,
     >                    ZM,prgM) 
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      DIMENSION X(3*NATM),AMASS(3*NATM)
      DIMENSION IAG(2),MG(NGM),IATOM(NATM)
      DIMENSION XM(3),YM(3),ZM(3)
      DIMENSION XGCM(3),RGM(3),PRGM(3)
c
c      DIMENSION VDUM(3)
c     The above line was commented because variable is not used.        0423TA02
      LOGICAL   LSYM,LEFT
c
c     ZM - along the rotation axis
c
      IND1 = 3*(IAG(1)-1) 
      IND2 = 3*(IAG(2)-1)
c      write (6,*) 'C1 = ',(X(IND1+I),I=1,3)
c      write (6,*) 'C2 = ',(X(IND2+I),I=1,3)
      DO I = 1,3
        ZM(I) =  X(IND2+I)-X(IND1+I)
      ENDDO
c      
c      write (6,*) 'ZM = ',(ZM(I),I=1,3)
c      
c No need to calculate symmetric molecules differently                     0509BE05
c       IF (LSYM) THEN
c        write (6,*) 'symmetric molecule'
c         DO I = 1,3
c           XGCM(I) = X(3*(MG(1)-1)+I)
c         ENDDO
c       ELSE
c
c     calculate the center of mass of the gyroscope
c
      TOTM = 0.0d0 
      DO I = 1, NGM
        TOTM = TOTM + AMASS(3*(MG(I)))**2
      ENDDO
      DO K = 1, 3
         SUM = 0.D00
         DO I = 1, NGM 
            SUM = SUM+X(3*(MG(I)-1)+K)*AMASS(3*(MG(I)-1)+K)**2
         ENDDO
         XGCM(K) = SUM/TOTM
      ENDDO
c      ENDIF
c      write (6,*) 'XGCM = ',(XGCM(I),I=1,3)
c
c     find XM that goes through the XGCM
c
c                    (XM)
c                  GM
c                 / |
c                /  |
c               /   |_
c              /    | |
c             C1---------C2  (ZM)
c
      DO I = 1,3
        RGM(I) = XGCM(I) - X(IND1+I)
      ENDDO
c RGM = C1GM
c      write (6,*) 'RGM = ',(RGM(I),I=1,3)
      DOT = 0.0d0
      XRGM = 0.0d0
      XZM = 0.0d0
      DO I = 1,3
        DOT = DOT + ZM(I)*RGM(I)
        XRGM = XRGM + RGM(I)**2
        XZM = XZM + ZM(I)**2
      ENDDO
      XRGM = SQRT(XRGM)
      XZM = SQRT(XZM)
      CTHETA = DOT/(XRGM*XZM)
c      write (6,*) 'ctheta = ',ctheta*180/3.14156d0,
c     >            'cosine = ',ACOS(ctheta)
c
c     once project RGM to ZM, find the intersection point
c
      DO I = 1,3
        PRGM(I) = X(IND1+I) + CTHETA*XRGM*ZM(I)/XZM
      ENDDO
c      write (6,*) 'PRGM = ',(PRGM(I),I=1,3)
c
c   XM = PRGM,GM
c
      DO I = 1,3
        XM(I) = XGCM(I) - PRGM(I)
      ENDDO
c
c   ZM = PRGM,X  in 1.0 unit
c
c      DO I = 1,3
c        ZM(I) = PRGM(I) + 1.0d0*(ZM(I)/XZM)
c      ENDDO
c
c    origin of XM,YM,ZM at PRGM
c
c      write (6,*) 'XM = ',(XM(I),I=1,3)
c
c
      IF (LEFT) THEN
c
c     find YM perpendicular to ZM and XM in left hand coordinate
c     because the principal axis in left hand
c
        CALL XPROD(XM,ZM,YM)
      ELSE
        CALL XPROD(ZM,XM,YM)
      ENDIF
c
c     return unit vector XM, YM, ZM
c
      XXM = 0.0d0
      XYM = 0.0d0
      XZM = 0.0d0
      DO I = 1,3
        XXM = XXM + XM(I)**2
        XYM = XYM + YM(I)**2
        XZM = XZM + ZM(I)**2
      ENDDO
      XXM = SQRT(XXM)
      XYM = SQRT(XYM)
      XZM = SQRT(XZM)
      DO I = 1,3
        XM(I) = XM(I)/XXM
        YM(I) = YM(I)/XYM
        ZM(I) = ZM(I)/XZM
      ENDDO
c
1992  format (A5,3F15.8)
c      write (6,1992) 'XM = ',(XM(I),I=1,3)
c      write (6,1992) 'YM = ',(YM(I),I=1,3)
c      write (6,1992) 'ZM = ',(ZM(I),I=1,3)
c
c      CALL dotprd(XM,YM,DUM,3)
c      write (6,*) 'XM . YM = ', DUM
c      CALL XPROD(XM,YM,VDUM)
c      write (6,*) 'XM x YM = ', (VDUM(I),I=1,3)
c      CALL dotprd(YM,ZM,DUM,3)
c      write (6,*) 'YM . ZM = ', DUM
c      CALL XPROD(YM,ZM,VDUM)
c      write (6,*) 'YM x ZM = ', (VDUM(I),I=1,3)
c      CALL dotprd(ZM,XM,DUM,3)
c      write (6,*) 'ZM . XM = ', DUM
c      CALL XPROD(ZM,XM,VDUM)
c      write (6,*) 'ZM x XM = ', (VDUM(I),I=1,3)
c 
      return
      end SUBROUTINE asymnzd
C
C***********************************************************************
C     ASYMRMI 
C***********************************************************************
C
      subroutine asymrmi(lsym,natm,iatom,x,amass,totm,ngm,iag,mg,rmi,am,
     >                   um,betam,alpha,berot)
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      DIMENSION X(3*NATM),AMASS(3*NATM)
      DIMENSION IAG(2),MG(NGM),IATOM(NATM)
      DIMENSION ALPHA(3,3),BEROT(3),PVEC(3,3)
      DIMENSION PRGM(3),BETAM(3),RM(3),XTM(3)
c
      DIMENSION XM(3),YM(3),ZM(3)
      LOGICAL   LSYM,LEFT
c
1992  format (3F15.8)
c      write (6,*) '=== Move to center of mass and find moment of I'
      call ASYMENT (left,natm,iatom,totm,x,amass,berot,pvec)
c      write (6,*) '=== Find ZM, XM, YM'
      call ASYMNZD (left,lsym,natm,iatom,x,amass,ngm,IAG,MG,XM,YM
     >             ,ZM,PRGM) 
c      write (6,*) '=== Form rm1, rm2, rm3'
c      write (6,*) 'prgm = ',(prgm(I),I=1,3)
      DO I = 1,3
        DO J = 1,3
          XTM(J) = PVEC(J,I)
        ENDDO
        CALL dotprd(XTM,prgm,RM(I),3)
      ENDDO
c      write (6,*) 'rm = '
c      write (6,1992) (RM(I),I=1,3)
c      write (6,*) '=== Check direction cosine'
      call ASYMALP (XM,YM,ZM,PVEC,ALPHA)
c
c      write (6,*) 'alpha '
c      do i = 1,3
c        write (6,1992) (alpha(i,j),j=1,3)
c      enddo
c
c      write (6,*) '== Rotate to XM, YM, ZM, and calculate AM,BM,CM,UM'
      call ASYMCAL (natm,iatom,x,amass,ngm,MG,XM,YM,ZM,AM,BM,CM,UM
     >              ,PRGM)
c      write (6,*) 'AM = ',AM,'BM = ',BM
c      write (6,*) 'CM = ',CM,'UM = ',UM
      RMI = 0.0d0
      DO I = 1,3
        IF (I.EQ.1) THEN 
           IMIN1 = 3
        ELSE
           IMIN1 = I - 1
        ENDIF
        IF (I.EQ.3) THEN
           IPLU1 = 1
        ELSE
           IPLU1 = I + 1
        ENDIF
c  Rows and Columns of Alpha Matrix were switched                          0509BE05
c        BETAM(I) = ALPHA(I,3)*AM-ALPHA(I,1)*BM-ALPHA(I,2)*CM+
c     *    UM*(ALPHA(IMIN1,2)*RM(IPLU1)-ALPHA(IPLU1,2)*RM(IMIN1))
c        TERM1 = (ALPHA(I,2)*UM)**2/TOTM
c       Switch Rows and Columns
        BETAM(I) = ALPHA(3,I)*AM-ALPHA(1,I)*BM-ALPHA(2,I)*CM+
     *    UM*(ALPHA(2,IMIN1)*RM(IPLU1)-ALPHA(2,IPLU1)*RM(IMIN1))
        TERM1 = (ALPHA(2,I)*UM)**2/TOTM
c       End of changes                                                     0509BE05
        TERM2 = BETAM(I)**2/BEROT(I)
        RMI = RMI + TERM1 + TERM2
c        write (6,*) 'RMI during calc ',RMI
c       write (6,*) 'PM during calc ',BEROT(1),BEROT(2),BEROT(3)
c        write (6,*) 'TERM2 ', TERM2
c        write (6,*) 'BETA = ',BETAM(I),' TERM1 = ',TERM1,' TERM2 = ',TERM2
      ENDDO
      RMI = AM - RMI  
c
      return  
      end subroutine asymrmi
c
c
C***********************************************************************
C  EFFBATH
C***********************************************************************
C
      subroutine effbath(iswth)
      use common_inc
      use perconparam
      use cm
      implicit double precision (a-h,o-z)
C
      VOLD = V
      Y = X(N3)
C
      F1 = PI*PI*REDM/(16.0d0*FRICT*FRICT)
      SUM = 0.0d0
      IF (LFOPT) THEN
        DO I = 1,N3-1
          SUM = SUM + F1 * DIFFU(I) * (X(I)-XSSAV(I))
        ENDDO
      ELSE
        Y = 0.0d0
      ENDIF
C
      V = V+(0.5d0*F1*Y*Y)-(Y*SUM)+0.5d0*(SUM*SUM)/F1
C
      IF (ISWTH.GE.1) THEN
        DO I = 1,N3-1
          DX(I) = DX(I)- DIFFU(I)*(F1*Y-SUM)
        ENDDO
        DX(N3) = F1*Y-SUM
      ENDIF
C
c      IF (ISWTH.GE.2) THEN
c 1992 format (3F15.8)
c      write (99,*) 'at S = ',S,' V =',V,' Y = ',Y
c      write (99,*) ' SUM = ',SUM,' F1 = ',F1
c       write (99,*) ' XSSAV ='
c      write (99,1992) (XSSAV(I),I=1,N3)
c      write (99,*) ' X ='
c      write (99,1992) (X(I),I=1,N3)
c      write (99,*) ' DX ='
c      write (99,1992) (DX(I),I=1,N3)
c      write (99,*) ' X-XSAV ='
c      write (99,1992) (X(I)-XSSAV(I),I=1,N3)
c      write (99,*) ' DIFFU ='
c      write (99,1992) (DIFFU(I),I=1,N3)
c 1997 format (8E15.6)
c      write (98,1997) S,Y,SUM,(Y-SUM),F1,VOLD*CKCAL,V*CKCAL,
c     >       0.5*F1*(Y-SUM)*(Y-SUM)*CKCAL
c      ENDIF
c
      return
      end subroutine effbath
c

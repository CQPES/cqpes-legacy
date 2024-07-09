C***********************************************************************
C  SENOUT
C***********************************************************************
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
      SUBROUTINE senout(EPRD,V,VAR,VAP,VAD,IFRFAC,FREQFAC)
      use perconparam
C
C     WRITE OUT SADDLE POINT ENERGETICS
C
C     CALLED BY:
C          MAIN, ZOCUPD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
         WRITE (FU6,2000)
         WRITE (FU6,2100)
         WRITE (FU6,2200) V,V*CEV,V*AUTOCM,V*CKCAL
         VCP = V-EPRD
         WRITE (FU6,2300) VCP,VCP*CEV,VCP*AUTOCM,VCP*CKCAL
         WRITE (FU6,2400) VAD,VAD*CEV,VAD*AUTOCM,VAD*CKCAL
         SAP = VAD-EPRD
         WRITE (FU6,2500) SAP,SAP*CEV,SAP*AUTOCM,SAP*CKCAL
         VZR = VAD-VAR
         WRITE (FU6,2600) VZR,VZR*CEV,VZR*AUTOCM,VZR*CKCAL
         VZP = VAD-VAP
         WRITE (FU6,2700) VZP,VZP*CEV,VZP*AUTOCM,VZP*CKCAL
         ZPES = VAD-V
         WRITE (FU6,2800) ZPES,ZPES*CEV,ZPES*AUTOCM,ZPES*CKCAL
C
C   Print out the results when using scaled frequencies
C
         IF (IFRFAC.NE.0) THEN                                          0808JC00
            ZPER = VAD - VZR                                            0808JC00
            ZPEP = SAP - VZP                                            0808JC00
            WRITE (FU6,1900)                                            0808JC00
            WRITE (FU6,2000)                                            0808JC00
            WRITE (FU6,2100)                                            0808JC00
            WRITE (FU6,2200) V,V*CEV,V*AUTOCM,V*CKCAL                   0808JC00
            WRITE (FU6,2300) VCP,VCP*CEV,VCP*AUTOCM,VCP*CKCAL           0808JC00
            WRITE (FU6,2400) (V+ZPES*FREQFAC),                          0808JC00
     *                       (V+ZPES*FREQFAC)*CEV,                      0808JC00
     *                       (V+ZPES*FREQFAC)*AUTOCM,                   0808JC00
     *                       (V+ZPES*FREQFAC)*CKCAL                     0808JC00
            WRITE (FU6,2500) (VCP+ZPES*FREQFAC),                        0808JC00
     *                       (VCP+ZPES*FREQFAC)*CEV,                    0808JC00
     *                       (VCP+ZPES*FREQFAC)*AUTOCM,                 0808JC00
     *                       (VCP+ZPES*FREQFAC)*CKCAL                   0808JC00
            WRITE (FU6,2600) (V+(ZPES - ZPER)*FREQFAC),                 0808JC00
     *                       (V+(ZPES - ZPER)*FREQFAC)*CEV,             0808JC00
     *                       (V+(ZPES - ZPER)*FREQFAC)*AUTOCM,          0808JC00
     *                       (V+(ZPES - ZPER)*FREQFAC)*CKCAL            0808JC00
            WRITE (FU6,2700) (VCP+(ZPES - ZPEP)*FREQFAC),               0808JC00
     *                       (VCP+(ZPES - ZPEP)*FREQFAC)*CEV,           0808JC00
     *                       (VCP+(ZPES - ZPEP)*FREQFAC)*AUTOCM,        0808JC00
     *                       (VCP+(ZPES - ZPEP)*FREQFAC)*CKCAL          0808JC00
            WRITE (FU6,2800) (ZPES*FREQFAC),                            0808JC00
     *                       (ZPES*FREQFAC)*CEV,                        0808JC00
     *                       (ZPES*FREQFAC)*AUTOCM,                     0808JC00
     *                       (ZPES*FREQFAC)*CKCAL                       0808JC00
         ENDIF                                                          0808JC00
C
      RETURN
C
 1900 FORMAT(//1X,'The following values are obtained using scaled',     0808JC00
     *       ' frequencies ',/)                                         0808JC00
 2000 FORMAT(//,78(1H-),/2X,'Saddle point energetics ',
     * '(V = classical energy, ZPE = zero point energy)',
     *      /,78(1H-))
 2100 FORMAT(32X,'hartrees',8X,'eV',8X,'cm**-1',7X,'kcal')
 2200 FORMAT(2X,'V w/re reactants V',10X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2300 FORMAT(2X,'V w/re product V',12X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2400 FORMAT(2X,'V+ZPE w/re reactant V',7X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2500 FORMAT(2X,'V+ZPE w/re product V',8X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2600 FORMAT(2X,'V+ZPE w/re reactant V+ZPE',3X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2700 FORMAT(2X,'V+ZPE w/re product V+ZPE',4X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2800 FORMAT(2X,'V+ZPE w/re saddle point V',3X,
     * 2(F10.5,2X),F10.2,2X,F10.4,/,78(1H-),/)
C
      END                                           
C
C***********************************************************************
C  SGND3V
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE sgnd3v (D3V,LIFREQ,JFREQ,INT)                          7/14YL92
      use common_inc; use perconparam
      use rate_const, only:  lopt
C
C  Store the sign of the 3rd derivative in Y00 for use in computing
C     the Morse turning point.
C
C     CALLED BY:
C                ANHARM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      IF (INT.EQ.1.AND.LGS(15).GT.3) THEN
C
C  Morse Ia
C
         IF (LGS(30).LE.0.OR.LOPT(4).LT.2) THEN
C
C     For MIa the sign of the 3rd derivative is obtained from the
C        direction of dissociation of the Morse oscillator.  This is
C        computed to be the negative of the sign of the change in the
C        hyperspherical radius for motion along the normal coordinate.
C
            RHO1 = 0.0D0
            RHO2 = 0.0D0
            DO 10 I = 1, N3                                             1025YL91
               XT = X(I)
               RHO1 = RHO1+XT*XT
               XT = XT+DLX*COF(I,JFREQ)
               RHO2 = RHO2+XT*XT
   10       CONTINUE
            Y00(LIFREQ) = -(RHO2-RHO1)
         ELSE
C
C     If LGS(30) > 0, RPH interpolation is used, and for LOPT(4) = 2,
C        the eigevectors are not available.  In this case it is assumed
C        that the Morse oscillator dissociates on the concave side of
C        the path; therefore we assume the 3rd derivative has the same
C        sign as Bf.  Since Bf is not available now, this is done in
C        subroutine TURNPT but for now we set Y00 to zero as a signal.
C
            Y00(LIFREQ) = 0.0D0
         ENDIF
      ELSE
C
C  Morse II
C
         Y00(LIFREQ) = D3V
      ENDIF
      RETURN
      END SUBROUTINE sgnd3v 
C
C***********************************************************************
C  SIMPSN
C***********************************************************************
C
      SUBROUTINE simpsn(N,FN,H,RESULT)
      use common_inc; use perconparam
      !use rate_const, only:  lopt
C
C     Calculate a definite integral using Simpson's formula
C     N must be even
C     FN is the function values
C     H is the length of the interval
C     Calculated result is stored in RESULT
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION FN(0:N)
C
      IF (MOD(N,2) .NE. 0) THEN
         WRITE(*,*)'N must be even in subroutine SIMPSN.'
         STOP 'SIMPSN 1'
      ENDIF
C
      RESULT = 0.0D0
C
      DO 10 I = 1,N-1,2
         RESULT = RESULT + 4.0D0 * FN(I)
10    CONTINUE
C
      DO 20 I = 2,N-2,2
         RESULT = RESULT + 2.0D0 * FN(I)
20    CONTINUE
C
      RESULT = RESULT + FN(0) + FN(N)
      RESULT = RESULT * H / 3.0D0
C
      RETURN
C
      END SUBROUTINE simpsn
C
C***********************************************************************
C  SPL1B1
C***********************************************************************
C
      SUBROUTINE spl1b1 (N,X,F,W,IJ,A,B,C,D)
C
C     CALLED BY:
C                VSPLIN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     WHERE N = NUMBER OF POINTS IN THE INTERPOLATION
C           X = ORIGIN OF TABLE OF THE INDEPENDENT VARIABLE
C           F = ORIGIN OF TABLE OF THE DEPENDENT VARIABLE
C           W = ORIGIN OF TABLE OF SECOND DERIVATIVES AS CALCULATED
C               BY SPL1D1
C          IJ = SPACING IN THE TABLES F AND W
C           A = ORIGIN OF TABLE OF THE CUBIC COEFFICIENT
C           B = ORIGIN OF TABLE OF THE QUADRATIC COEFFICIENT
C           C = ORIGIN OF TABLE OF THE LINEAR COEFFICIENT
C           D = ORIGIN OF TABLE OF THE CONSTANT COEFFICIENT
C
C
C     SPL1B1 CONVERTS THE SPLINE FIT DATA SUPPLIED BY SPL1D1
C     FROM THE FUNCTION AND ITS SECOND DERIVATIVE AT THE KNOTS,
C     TO THE FOUR COEFFICINTS OF THE CUBIC - A,B,C,D, WHERE
C      F IS NOW APPROXIMATED BY A*X**3 + B*X**2 + C*X + D
C
      DIMENSION X(N),F(N),W(N),A(N),B(N),C(N),D(N)                      1215YL91
      DATA EPS / 1.0D-11 /
      C6 = (1.0D0/6.0D0)-EPS
      C3 = 2.0D0*C6
C
C      GIVEN X,F,W COMPUTE A,B,C,D
C
      NM1 = N-1
      DO 10 I = 1, NM1
         M = (I-1)*IJ+1
         MPIJ = M+IJ
         IP1 = I+1
         XIP1 = X(IP1)
         XI = X(I)
         H = XIP1-XI
         FI = F(I)
         FIP1 = F(MPIJ)
         WIP1 = W(MPIJ)
         WI = W(M)
         AI = (WIP1-WI)*C6
         T1 = XIP1*WI
         T2 = XI*WIP1
         BI = 0.5D0*(T1-T2)
         T1 = XIP1*T1
         T2 = XI*T2
         C(I) = (0.5D0*(T2-T1)+FIP1-FI)/H-AI*H
         A(I) = AI/H
         T1 = XIP1*T1
         T2 = XI*T2
         D(I) = (C6*(T1-T2)+FI*XIP1-FIP1*XI)/H-C3*H*BI
         B(I) = BI/H
   10 CONTINUE
      RETURN
      END
C
C***********************************************************************
C  SPL1B2
C***********************************************************************
C
      SUBROUTINE spl1b2 (N,X,A,B,C,D,Y,TAB,IOP)
C
C     CALLED BY:
C                  PSAG,RATE,VSPLIN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     WHERE N = NUMBER OF POINTS IN THE INTERPOLATION
C           X = ORIGIN OF TABLE OF THE INDEPENDENT VARIABLE
C           A = ORIGIN OF TABLE OF CUBIC COEFFICIENTS
C           B = ORIGIN OF TABLE OF QUADRATIC COEFFICIENTS
C           C = ORIGIN OF TABLE OF LINEAR COEFFICIENTS
C           D = ORIGIN OF TABLE OF CONSTANT COEFFICIENTS
C           Y = THE POINT AT WHICH INTERPOLATION IS DESIRED
C         TAB = AN ARRAY OF DIMENSION 3 WHICH CONTAINS THE FUNCTION
C               VALUE, FIRST DERIVATIVE, AND SECOND DERIVATIVE AT Y
C         IOP = INTEGER SPECIFYING WHETHER DERIVATIVES ARE TO BE COMPUTE
C            .LE. 0 , F ONLY COMPUTED
C             = 1 , F AND FIRST DERIVATIVE ARE COMPUTED
C            .GE. 2 , F AND FIRST AND SECOND DERIVATIVES COMPUTED
C
C       THE FUNCTION F IS NOW APPROXIMATED BY THE CUBIC EQUATION
C          A*X**3 + B*X**2 + C*X + D
C
      DIMENSION X(N),A(N),B(N),C(N),D(N),TAB(3)                         1215YL91
C
C     LOCATE Y IN THE X TABLE
C
      NM1 = N-1
      IF (Y-X(1)) 10, 10, 20
   10 I = 1
      GO TO 60
   20 IF (Y-X(N)) 40, 30, 30
   30 I = NM1
      GO TO 60
   40 I = 1
      DO 50 K = 2, NM1
         IF (X(K).GT.Y) GO TO 60
         I = I+1
   50 CONTINUE
   60 CONTINUE
C
C      CALCULATE F(Y)
C
      T = A(I)*Y
      TAB(1) = ((T+B(I))*Y+C(I))*Y+D(I)
C
C      CALCULATE THE FIRST DERIVATIVE OF F(Y)
C
      IF (IOP.LE.0) RETURN
      T = 3.0D0*T
      TAB(2) = (T+2.0D0*B(I))*Y+C(I)
C
C      CALCULATE THE SECOND DERIVATIVE OF F(Y)
C
      IF (IOP.EQ.1) RETURN
      TAB(3) = 2.0D0*(T+B(I))
      RETURN
      END
C
C***********************************************************************
C  SPL1D1
C***********************************************************************
C
      SUBROUTINE spl1d1 (N,X,F,W,IOP,IJ,A,B,C)
      use perconparam, only : fu6
C
C     CALLED BY:
C                VSPLIN
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
C     WHERE N = NUMBER OF POINTS IN THE INTERPOLATION
C           X = ORIGIN OF TABLE OF INDEPENDENT VARIALE
C           F = ORIGIN OF TABLE OF DEPENDENT VARIABLE
C           W = AN ARRAY OF DIMENSION N WHICH CONTAINS THE CALCULATED
C               SECOND DERIVATIVES UPON RETURN
C         IOP = AN ARRAY OF DIMENSION 2 WHICH CONTAINS COMBINATIONS OF
C               THE INTEGERS 1 THRU 5 USED TO SPECIFY THE BOUNDARY
C               CONDITIONS
C     IF IOP(1)=1 AND IOP(2)=1, W(1) AND W(K) ARE THE VALUES OF THE
C     SECOND DERIVATIVE AT X(1) AND X(K), RESPECTIVELY.
C     IF IOP(1)=2 AND IOP(2)=2, W(1) DETERMINES F""(X(1)) BY THE
C     RELATION F""(X(1))=W(1)*F""(X(2)), AND W(K) DOES F""(X(K)) BY THE
C     RELATION F""(X(N))=W(K)*F""(X(N-1)).
C     IF IOP(1)=3 AND IOP(2)=3, W(1) AND W(K) ARE THE VALUES OF THE
C     FIRST DERIVATIVE AT X(1) AND X(K), RESPECTIVELY.
C     IF IOP(1)=4 AND IOP(2)=4, THE RERIODIC BOUNDARY CONDITIONS ARE
C     ALLOWED, F""(1)=F""(N), F(1)=F(N).
C     IF IOP(1)=5 AND IOP(2)=5, THE FIRST DERIVATIVES AT X(1) AND X(N)
C     ARE CALCULATED BY USING A DIFFERENTIATED FOUR-POINT LAGRANGIAN
C     INTERPOLATION FORMULA EVALUATED AT X(1) AND X(N), RESPECTIVELYR.
C          IJ = SPACING IN THE F AND W TABLES
C       A,B,C = ARRAYS OF DIMENSION N USED FOR TEMPORARY STORAGE
C
      DIMENSION IOP(2),X(N),F(N),W(N),A(N),B(N),C(N)
      DATA BOB / 0.0D0 /, BILL / 0.0D0 /, EPS/ 1.0D-11 /
      K = N-1
      SXIV = (1.0D0/6.0D0)-EPS
      THIV = 2.0D0*SXIV
      A(2) = -(X(2)-X(1))*SXIV
      B(2) = (X(3)-X(1))*THIV
      W(IJ+1) = (F(2*IJ+1)-F(IJ+1))/(X(3)-X(2))-(F(IJ+1)-F(1))/(X(2)-X(1
     *   ))
      IF (N-3) 10, 30, 10
   10 DO 20 I = 3, K
         M = (I-1)*IJ+1
         J1 = M+IJ
         J2 = M-IJ
         CON = (X(I+1)-X(I-1))*THIV
         DIFX = X(I)-X(I-1)
         DON = DIFX*SXIV
         BIMI = 1.0D0/B(I-1)
         B(I) = CON-(DON**2)*BIMI
         E = (F(J1)-F(M))/(X(I+1)-X(I))-(F(M)-F(J2))/DIFX
         W(M) = E-(DON*W(J2))*BIMI
         A(I) = -(DON*A(I-1))*BIMI
   20 CONTINUE
   30 K1 = (N-2)*IJ+1
      BNMI = 1.0D0/B(N-1)
      C(N-1) = -(X(N)-X(N-1))*SXIV*BNMI
      W(K1) = W(K1)*BNMI
      A(N-1) = A(N-1)*BNMI
      K2 = K-1
      IF (N-3) 40, 60, 40
   40 DO 50 I = 2, K2
         J = N-I
         CON = (X(J+1)-X(J))*SXIV
         BJI = 1.0D0/B(J)
         A(J) = (A(J)-CON*A(J+1))*BJI
         C(J) = -(CON*C(J+1))*BJI
         K3 = (J-1)*IJ+1
         M = K3+IJ
         W(K3) = (W(K3)-CON*W(M))*BJI
   50 CONTINUE
   60 K4 = (N-1)*IJ+1
      IF (IOP(1)-5) 70, 90, 70
   70 C1 = W(1)
      IF (IOP(2)-5) 80, 110, 80
   80 C2 = W(K4)
      GO TO 130
   90 IF (N-4) 570, 100, 100
  100 A1 = X(1)-X(2)
      A2 = X(1)-X(3)
      A3 = X(1)-X(4)
      A4 = X(2)-X(3)
      A5 = X(2)-X(4)
      A6 = X(3)-X(4)
      W(1) = F(1)*(1.0D0/A1+1.0D0/A2+1.0D0/A3)-A2*A3*F(IJ+1)/(A1*A4*A5)+
     *   A1*A3*F(2*IJ+1)/(A2*A4*A6)-A1*A2*F(3*IJ+1)/(A3*A5*A6)
      GO TO 70
  110 IF (N-4) 570, 120, 120
  120 B1 = X(N)-X(N-3)
      B2 = X(N)-X(N-2)
      B3 = X(N)-X(N-1)
      B4 = X(N-1)-X(N-3)
      B5 = X(N-1)-X(N-2)
      B6 = X(N-2)-X(N-3)
      L1 = K4-IJ
      L2 = L1-IJ
      L3 = L2-IJ
      W(K4) = -B2*B3*F(L3)/(B6*B4*B1)+B1*B3*F(L2)/(B6*B5*B2)-B1*B2*F(L1)
     *   /(B4*B5*B3)+F(K4)*(1.0D0/B1+1.0D0/B2+1.0D0/B3)
      GO TO 80
  130 DO 160 I = 1, K
         M = (I-1)*IJ+1
  170 MK = IOP(1)
      GO TO (180,210,260,310,260), MK
  180 IF (I-1) 200, 190, 200
  190 A(1) = -1.0D0
      C(1) = 0.0D0
      GO TO 340
  200 BOB = 0.0D0
      GO TO 340
  210 IF (I-1) 230, 220, 230
  220 A(1) = -1.0D0
      C(1) = 0.0D0
      W(1) = 0.0D0
      GO TO 340
  230 IF (I-2) 240, 240, 250
  240 BOB = -C1
      GO TO 340
  250 BOB = 0.0D0
      GO TO 340
  260 IF (I-1) 280, 270, 280
  270 XDTO = X(2)-X(1)
      A(1) = -XDTO*THIV
      C(1) = 0.0D0
      W(1) = -C1+(F(IJ+1)-F(1))/XDTO
      GO TO 340
  280 IF (I-2) 290, 290, 300
  290 BOB = (X(2)-X(1))*SXIV
      GO TO 340
  300 BOB = 0.0D0
      GO TO 340
  310 IF (I-1) 330, 320, 330
  320 A(1) = -1.0D0
      C(1) = 1.0D0
      W(1) = 0.0D0
      GO TO 340
  330 BOB = 0.0D0
  340 ML = IOP(2)
      GO TO (350,380,430,480,430), ML
  350 IF (I-1) 370, 360, 370
  360 A(N) = 0.0D0
      C(N) = -1.0D0
      GO TO 140
  370 BILL = 0.0D0
      GO TO 140
  380 IF (I-1) 400, 390, 400
  390 A(N) = 0.0D0
      C(N) = -1.0D0
      W(K4) = 0.0D0
      GO TO 140
  400 IF (I-K) 420, 410, 420
  410 BILL = -C2
      GO TO 140
  420 BILL = 0.0D0
      GO TO 140
  430 IF (I-1) 450, 440, 450
  440 A(N) = 0.0D0
      C(N) = (X(N-1)-X(N))*THIV
      W(K4) = C2-(F(K4)-F(K1))/(X(N)-X(N-1))
      GO TO 140
  450 IF (I-K) 470, 460, 470
  460 BILL = (X(N)-X(N-1))*SXIV
      GO TO 140
  470 BILL = 0.0D0
      GO TO 140
  480 IF (I-1) 500, 490, 500
  490 A(N) = 0.0D0
      C(N) = (X(N-1)+X(1)-X(N)-X(2))*THIV
      W(K4) = (F(IJ+1)-F(1))/(X(2)-X(1))-(F(K4)-F(K1))/(X(N)-X(N-1))
      GO TO 140
  500 IF (I-2) 520, 510, 520
  510 BILL = (X(2)-X(1))*SXIV
      GO TO 140
  520 IF (I-K) 540, 530, 540
  530 BILL = (X(N)-X(N-1))*SXIV
      GO TO 140
  540 BILL = 0.0D0
      GO TO 140
  140    IF (I-1) 150, 160, 150
  150    W(1) = W(1)-BOB*W(M)
         W(K4) = W(K4)-BILL*W(M)
         A(1) = A(1)-BOB*A(I)
         A(N) = A(N)-BILL*A(I)
         C(1) = C(1)-BOB*C(I)
         C(N) = C(N)-BILL*C(I)
  160 CONTINUE
  550 CON = A(1)*C(N)-C(1)*A(N)
      D1 = -W(1)
      D2 = -W(K4)
      W(1) = (D1*C(N)-C(1)*D2)/CON
      W(K4) = (A(1)*D2-D1*A(N))/CON
      DO 560 I = 2, K
         M = (I-1)*IJ+1
         W(M) = W(M)+A(I)*W(1)+C(I)*W(K4)
  560 CONTINUE
      GO TO 580
  570 WRITE (FU6,1000)
  580 RETURN
C
 1000 FORMAT(1H ,39HSPL1D1 N LESS THAN 4, RESULTS INCORRECT  )
C
      END SUBROUTINE spl1d1
C
C***********************************************************************
C  SSAVE
C***********************************************************************

      SUBROUTINE ssave (L,BKAP,NPOPT)
C
C     SAVES REACTION PATH RESULTS FOR ROUTINE PATH
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C
C   The include file esp.inc has been removed in version 5.0         1021GL92
C   The information for the bond orders and charges has been         1021GL92
C   removed in version 5.0.                                          1021GL92
C
C     CALLED BY:
C                PATH
C
C
   
      use common_inc
      use perconparam
      use rate_const
      use cm, only : sbkap
      use sst
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
      save                                                              0601YC98
C
C
      IF (LGS(1).EQ.0.AND.L.EQ.NSDM) RETURN            
      IL = L
      ISHFT = N3-N3M7
C
C
      IF (NPOPT .LT. 2)  SSUBI(IL) = S                                  0224BL05
C 
      DO 10 I = 1, N3
         GEOM(I,IL) = X(I)
   10 CONTINUE
C
      VCLAS(IL) = V
      FMITS(IL) = FMOM(5)

      IF (NPOPT .NE. 1) THEN                                            0224BL05
      EGRND(IL) = EGRNDT                                                1106YL92
c      SBKAP(IL-1) = BKAP                                                0621YC99
c Array value changed to avoid out-of-bound for 'IL=1'.  Older          1103BE05
c versions of Polyrate have IL, not sure why YC changed it to 'IL-1'    1103BE05
      SBKAP(IL) = BKAP                                                  1103BE05
      DO 20 I = 1, N3M7                                                 0224BL05
         WETS(I,IL) = FREQ(I+ISHFT)                                     0224BL05
         EFNDT(I,IL) = EFNDTP(I)                                        1106YL92
   20 CONTINUE                                                          0224BL05
      VADIB(IL) = VAD
      ENDIF                                                             0224BL05
C
      IF (LGS(5).EQ.0.AND.LGS(33).EQ.0) GO TO 40
      AFLAG = '   '
      IF (LGS(5).GE.21) AFLAG = 'SET'                                   6/30YL91
      DO 30 I = 1, N3M7
         IF (LGS(33).EQ.1) EWKB0(I,IL) = ETP(I)
         IF (AFLAG.EQ.'SET') LGS(5) = MODE(I)
         IF (LGS(5).EQ.0) GO TO 30                  
         IF (LGS(5).EQ.7.OR.LGS(5).EQ.8) THEN
            XETS(I,IL) = ANHRM(I)
            Y0TS(I,IL) = AB(I)
         ELSEIF (LGS(5).EQ.9) THEN                                      6/30YL91
            FMIHTS(I,IL) = FMOMHR(I+ISHFT)                              6/30YL91
         ELSE
            XETS(I,IL) = ANHRM(I)
            Y0TS(I,IL) = Y00(I)
         ENDIF
   30 CONTINUE
   40 CONTINUE

      IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20                            6/20YL91
C
C *    STORE DATA FOR LCG3 CALCULATIONS
C
      IF (LLCG) THEN                                                    1028GL91
      DO 50 I = 1, N3M7                                                 5/10DL90
         DO 55 J = 1, N3                                                5/10DL90
            COFSV(J,I,IL) = COF(J,I+ISHFT)                              5/10DL90
   55    CONTINUE                                                       5/10DL90
   50 CONTINUE                                                          5/10DL90
      DO 56 I = 1, N3                                                   5/10DL90
         DXSV(I,IL) = DX(I)                                             5/10DL90
   56 CONTINUE                                                          5/10DL90
      ENDIF                                                             10/28/GL91
C
C     STORE DATA FOR SST CALCULATIONS
C
      IF(LSST.EQ.1) THEN                                                0101JZ13
        DO 60 I = 1, NTOR 
          TORBH(I,IL) = TBH(I)
   60   CONTINUE
        DO 70 I = 1, N3TM
          DBW(I,IL) = DBWTMP(I)
   70   CONTINUE
        DETDS(IL)= DETD
      ENDIF
      RETURN
      END subroutine ssave
C
C***********************************************************************
C  TABLE
C***********************************************************************
C
      SUBROUTINE table
      use common_inc
      use perconparam,only : nvibm,fu15
      use rate_const
      use kintcm
      use keyword_interface, only : ivice
C
C     WRITES OUT A SUMMARY OF SELECTED FORWARD RATE CONSTANTS TO
C     FOR015
C     THIS SUBROUTINE WAS ADDED IN MARCH 1986 BY R. STECKLER
C
C     CALLED BY:
C                FINOUT
C
C     CALLS:
C            DATTIM,TITLE
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C   MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C   FORMAT STATEMENTS MODIFIED BY GCL 01/17/92
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C*******************************************************************************
C
C     Summarize the forward rates vs. temperature
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
C
      CHARACTER*4 SWLCT(2)                                              0708JC00
      DATA SWLCT /'LCG3','LCG4'/                                        0708JC00
C
      IF (IFRFAC.EQ.1) WRITE (FU15,4400)                                0815JC00
C
      IF (LGS(8).LE.0) THEN
         CALL DATTIM (FU15)
         CALL wTITLE (FU15)
      ELSE
         CALL DATTIM (FU15)
         CALL wTITLE (FU15)
      ENDIF
C
C  This section was copied from the subroutine finout.f and inserted here
C  by GCL 01/17/92
C
      IF (LZOC) THEN                                                    0824YC98
        IF (IVICE.NE.2) THEN
            WRITE(FU15,500)                                             10/5WH92
        ELSE
            WRITE(FU15,501)
        ENDIF
      ENDIF
      IF(LGS(6).GT.2 ) THEN
         IF(ICODE(1).GT.0 .AND. LGS(34).NE.0) THEN                      7/27T88
            WRITE (FU15,1000)
         ELSE
            WRITE (FU15,1050)
         ENDIF                                                          ..
      ELSE
         IF(ICODE(1).LT.0 .AND. ICODE(2).LT.0) THEN
            WRITE (FU15,1050)
         ELSE                                                           ..
            WRITE (FU15,1000)
         ENDIF                                                          ..
      ENDIF
C
C
C     FIRST CHECK IF SCSAG TUNNELING CALCULATIONS WERE COMPUTED
C     IF THEY WERE THEN INCLUDE THESE IN THE SUMMARY TABLE.
C
      IF (.NOT. LCDSC) THEN
C
C                                              NO SCT (CD-SCSAG) TUNNELING
C
         IF (LGS(21).EQ.0) THEN
C
C                                              NO MUVT CALCULATIONS
C
C           IF (LGS(20).EQ.0) THEN
C
C                                              NO ICVT CALCULATIONS
C
                IF (IZCT.EQ.0) THEN                                     0915PF97
C
C                                              NO ZCT CALCULATIONS      0915PF97
C
                   WRITE (FU15,1100)                                    0915PF97
                   DO 10 I = 1, NTEMP
                      WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I)
   10              CONTINUE
                ELSE                                                    0915PF97
C
C                                              INCLUDE ZCT              0915PF97
C
                   WRITE (FU15,1150)                                    0915PF97
                   DO 100 I = 1, NTEMP                                  0915PF97
                      WRITE (FU15,1700) TEMP(I), CONF(I), CVTF(I),      0916PF97
     *                                  ZCTKT(I,2)                      0916PF97
  100              CONTINUE                                             0916PF97
                ENDIF                                                   0916PF97
C           ELSE
C
C                                              INCLUDE ICVT
C
C             IF (IZCT.EQ.0) THEN                                       0916PF97
C
C                                              NO ZCT                   0916PF97
C              WRITE (FU15,1200)
C              DO 20 I = 1, NTEMP
C                 WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CIVT(I)
C  20          CONTINUE
C             ELSE                                                      0916PF97
C                                              INCLUDE ZCT              0916PF97
C              WRITE (FU15,1250)                                        0916PF97
C              DO 25 I= 1, NTEMP                                        0916PF97
C                 WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2), 0916PF97
C    *                              CIVT(I)                             0916PF97
C  25          CONTINUE                                                 0916PF97
C             ENDIF                                                     0916PF97
C           ENDIF
         ELSE
C
C                                              INCLUDE MUVT
C
          IF (IZCT.EQ.0) THEN                                           0916PF97
C
C                                              NO ZCT                   0916PF97
            WRITE (FU15,1300)
            DO 30 I = 1, NTEMP
c              WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CIVT(I),
c    *                         CMVTCA(I)
               WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CMVTCA(I)
   30       CONTINUE
          ELSE                                                          0916PF97
C                                              INCLUDE ZCT              0916PF97
            WRITE(FU15,1350)                                            0916PF97
            DO 35 I = 1, NTEMP                                          0916PF97
c              WRITE(FU15,1700) TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2),     0916PF97
c    *                          CIVT(I),CMVTCA(I)                       0916PF97
               WRITE(FU15,1700) TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2),  
     *                          CMVTCA(I)
   35       CONTINUE                                                    0916PF97
          ENDIF                                                         0916PF97
         ENDIF
      ELSE IF (LCDSC .AND. .NOT. LLCG) THEN
C
C                                             INCLUDE SCT (CD-SCSAG) TUNNELING
C
         IF (LGS(21).EQ.0) THEN
C
C                                              NO MUVT CALCULATIONS
C
C           IF (LGS(20).EQ.0) THEN
C
C                                              NO ICVT CALCULATIONS
C
               IF (IZCT.EQ.0) THEN                                      0916PF97
C
C                                              NO ZCT CALCULATIONS      0916PF97
C
                  WRITE (FU15,1400)
                  DO 40 I = 1, NTEMP
                     WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CDKT(I,2)1106YL92
   40             CONTINUE
               ELSE                                                     0916PF97
C
C                                              INCLUDE ZCT              0916PF97
                  WRITE (FU15,1425)                                     0916PF97
                  DO 110 I = 1, NTEMP                                   0916PF97
                     WRITE(FU15,1700) TEMP(I),CONF(I),CVTF(I),          0916PF97
     *                               ZCTKT(I,2),CDKT(I,2)               0916PF97
  110             CONTINUE                                              0916PF97
               ENDIF                                                    0916PF97
C           ELSE
C
C                                              INCLUDE ICVT
C
C             IF (IZCT.EQ.0) THEN                                       0916PF97
C              WRITE (FU15,1500)
C              DO 50 I = 1, NTEMP
C                 WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CDKT(I,2),  1106YL92
C    *                              CIVT(I),CDKT(I,3)                   1106YL92
C  50          CONTINUE
C             ELSE                                                      0916PF97
C              WRITE (FU15,1525)                                        0916PF97
C              DO 55 I = 1, NTEMP                                       0916PF97
C                 WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2), 0916PF97
C    *                              CDKT(I,2),CIVT(I),CDKT(I,3)         0916PF97
C  55          CONTINUE                                                 0916PF97
C             ENDIF                                                     0916PF97
C           ENDIF
         ELSE
C
C                                              INCLUDE MUVT
C
          IF (IZCT.EQ.0) THEN                                           0916PF97
C                                              NO ZCT CALCULATIONS      0916PF97
            WRITE (FU15,1600)
            DO 60 I = 1, NTEMP
               WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CDKT(I,2),     1106YL92
     *                           CMVTCA(I),CDKT(I,3)  
   60       CONTINUE
          ELSE                                                          0916PF97
C                                              INCLUDE ZCT              0916PF97
            WRITE (FU15,1625)                                           0916PF97
            DO 65 I = 1, NTEMP                                          0916PF97
               WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2),    0916PF97
     *                  CDKT(I,2),CMVTCA(I),CDKT(I,3) 
   65       CONTINUE                                                    0916PF97
          ENDIF                                                         0916PF97
         ENDIF
      ELSE IF (LLCG) THEN
C
C        INCLUDE SCT (CD-SCSAG) AND LCG3 TUNNELING
C
         IF (LGS(21) .EQ. 0) THEN
          IF (LGS(20) .EQ. 0) THEN
           IF (IZCT.EQ.0) THEN                                          0916PF97
C                                              NO ZCT                   0916PF97
            IF(ILCT.LE.1) WRITE(FU15,1450)                              0708JC00
            IF(ILCT.EQ.2) WRITE(FU15,1453)                              0708JC00
            DO 70 I = 1, NTEMP                                          5/10DL90
             WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CDKT(I,2),
     *                         CLKT(I,2)                                1106YL92
   70       CONTINUE                                                    1106YL92
           ELSE                                                         0916PF97
C                                              INCLUDE ZCT              0916PF97
            IF(ILCT.LE.1) WRITE(FU15,1452)                              0708JC00
            IF(ILCT.EQ.2) WRITE(FU15,1454)                              0708JC00
            DO 120 I = 1, NTEMP                                         0916PF97
             WRITE(FU15,1700) TEMP(I),CONF(I),CVTF(I),                  0916PF97
     *                       ZCTKT(I,2),CDKT(I,2),CLKT(I,2)             0916PF97
  120       CONTINUE                                                    0916PF97
           ENDIF
            WRITE(FU15,1451)                                            1106YL92
            DO 71 I = 1, NTEMP                                          1106YL92
             WRITE (FU15,1700) TEMP(I),COMTKT(I,2),UOMTKT(I,2)          1106YL92
   71       CONTINUE                                                    1106YL92
C
C        INCLUDE ICVT
C
C         ELSE
C          IF (IZCT.EQ.0) THEN                                          0916PF97
C                                              NO ZCT                   0916PF97
C           IF(ILCT.LE.1) WRITE(FU15,1550)                              0708JC00
C           IF(ILCT.EQ.2) WRITE(FU15,1553)                              0708JC00
C           DO 80 I = 1, NTEMP
C            WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),CDKT(I,2),       1106YL92
C    *                         CLKT(I,2),CIVT(I),CDKT(I,3),CLKT(I,3)    1106YL92

C  80       CONTINUE
C          ELSE                                                         0916PF97
C                                              INCLUDE ZCT              0916PF97
C           IF(ILCT.LE.1) WRITE(FU15,1552)                              0708JC00
C           IF(ILCT.EQ.2) WRITE(FU15,1554)                              0708JC00
C           DO 85 I = 1, NTEMP                                          0916PF97
C            WRITE (FU15,1700) TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2),      0916PF97
C    *                  CDKT(I,2),CLKT(I,2),CIVT(I),CDKT(I,3),CLKT(I,3) 0916PF97
C  85       CONTINUE                                                    0916PF97
C          ENDIF                                                        0916PF97
C           WRITE(FU15,1551)                                            1106YL92
C           DO 81 I = 1, NTEMP                                          1106YL92
C            WRITE (FU15,1700) TEMP(I),COMTKT(I,2),UOMTKT(I,2),         1106YL92
C    *                          COMTKT(I,3),UOMTKT(I,3)                 1106YL92
C  81       CONTINUE                                                    1106YL92
         END IF
C
C     INCLUDE MUVT
C
        ELSE
        IF (IZCT.EQ.0) THEN                                             0916PF97
C                                              NO ZCT                   0916PF97
         IF(ILCT.LE.1) WRITE(FU15,1650)                                 0708JC00
         IF(ILCT.EQ.2) WRITE(FU15,1653)                                 0708JC00
         DO 90 I = 1, NTEMP
C         WRITE (FU15,1750) TEMP(I),CONF(I),CVTF(I),CDKT(I,2),CLKT(I,2),1106YL92
C    *     CIVT(I),CDKT(I,3),CLKT(I,3),CMVTCA(I),CDKT(I,4),CLKT(I,4)    1106YL92
          WRITE (FU15,1750) TEMP(I),CONF(I),CVTF(I),CDKT(I,2),CLKT(I,2),
     *                      CMVTCA(I),CDKT(I,3),CLKT(I,3)
   90    CONTINUE
        ELSE                                                            0916PF97
C                                              INCLUDE ZCT              0916PF97
         IF(ILCT.LE.1) WRITE(FU15,1652)                                 0708JC00
         IF(ILCT.EQ.2) WRITE(FU15,1654)                                 0708JC00
         DO 95 I = 1, NTEMP                                             0916PF97
c         WRITE (FU15,1775)TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2),CDKT(I,2),0916PF97
c    *                     CLKT(I,2),CIVT(I),CDKT(I,3),CLKT(I,3),       0916PF97
c    *                     CMVTCA(I),CDKT(I,4),CLKT(I,4)                0916PF97
          WRITE (FU15,1775)TEMP(I),CONF(I),CVTF(I),ZCTKT(I,2),CDKT(I,2),
     *                     CLKT(I,2),CMVTCA(I),CDKT(I,3),CLKT(I,3)
   95    CONTINUE                                                       0916PF97
         ENDIF                                                          0916PF97
         WRITE(FU15,1651)                                               1106YL92
         DO 91 I = 1, NTEMP                                             1106YL92
            WRITE (FU15,1700) TEMP(I),COMTKT(I,2),UOMTKT(I,2),          1106YL92
     *             COMTKT(I,3),UOMTKT(I,3),COMTKT(I,4),UOMTKT(I,4)      1106YL92
   91    CONTINUE                                                       1106YL92
        END IF
      ENDIF
C
      WRITE(FU15,2000)                                                  10/5WH92
C
      RETURN
C
  500 FORMAT(1X,'The following are VTST-IOC calculation results.')      1218WH92
  501 FORMAT(1X,'The following are VTST-ISPE calculation results.')     0708YC98
 1000 FORMAT(/1X,'Summary of forward rate constants ',                  1027WH92
     *       '(cm**3/molecule-1 s-1) :')
 1050 FORMAT(/1X,'Summary of forward rate constants ',                  ..
     *       '(s-1) :')
 1100 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',/)
 1150 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',/)               0915PF97
c1200 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',7X,'ICVT',/)
c1250 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',7X,'ICVT',/)     0916PF97
C1300 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',7X,'ICVT',6X,'MUVT',/)
 1300 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',7X,'MUVT',/)
 1350 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',7X,'ICVT',       0916PF97
     *        6X,'MUVT',/)                                              0916PF97
 1400 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',/)               0423TA02
 1425 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',/)  0423TA02
 1450 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG3',/) 0423TA02
 1451 FORMAT(/4X,'T(K)',3X,'CVT/COMT',2X,'CVT/mOMT',/)                  1106YL92
 1452 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
     *        2X,'CVT/LCG3',/)                                          0423TA02
 1453 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG4',/) 0423TA02
 1454 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
     *        2X,'CVT/LCG4',/)                                          0423TA02
c1500 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',4X,'ICVT',       0423TA02
c    *        4X,'ICVT/SCT',/)                                          0423TA02
c1525 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
c    *        4X,'ICVT',4X,'ICVT/SCT',/)                                0423TA02
c1550 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG3',   0423TA02
c    *        4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG3',/)                 0423TA02
c1551 FORMAT(/4X,'T(K)',3X,'CVT/COMT',2X,'CVT/mOMT',1X,'ICVT/COMT',     1106YL92
c    *        1X,'ICVT/mOMT',/)                                         1106YL92
c1552 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
c    *        2X,'CVT/LCG3',4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG3',/)   0423TA02
c1553 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG4',   0423TA02
c    *        4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG4',/)                 0423TA02
c1554 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
c    *        2X,'CVT/LCG4',4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG4',/)   0423TA02
c1600 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',4X,'ICVT',       0423TA02
c    *        4X,'ICVT/SCT',4X,'MUVT',4X,'MUVT/SCT',/)                  0423TA02
 1600 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',4X,
     *        'MUVT',4X,'MUVT/SCT',/) 
C1625 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
C    *        4X,'ICVT',4X,'ICVT/SCT',4X,'MUVT',4X,'MUVT/SCT',/)        0423TA02
 1625 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT', 
     *        4X,'MUVT',4X,'MUVT/SCT',/)
c1650 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG3',   0423TA02
c    *        4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG3',4X,'MUVT',         0423TA02
c    *        4X,'MUVT/SCT',1X,'MUVT/LCG3',/)                           0423TA02
 1650 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG3',   
     *        4X,'MUVT',4X,'MUVT/SCT',1X,'MUVT/LCG3',/)       
 1651 FORMAT(/4X,'T(K)',3X,'CVT/COMT',2X,'CVT/mOMT',1X,'ICVT/COMT',     1106YL92
     *        1X,'ICVT/mOMT',1X,'MUVT/COMT',1X,'MUVT/mOMT',/)           1106YL92
c1652 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
c    *        2X,'CVT/LCG3',4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG3',     0423TA02
c    *        4X,'MUVT',4X,'MUVT/SCT',1X,'MUVT/LCG3',/)                 0423TA02
 1652 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT', 
     *        2X,'CVT/LCG3',4X,'MUVT',4X,'MUVT/SCT',1X,'MUVT/LCG3',/)
c1653 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG4',   0423TA02
c    *        4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG4',4X,'MUVT',         0423TA02
c    *        4X,'MUVT/SCT',1X,'MUVT/LCG4',/)                           0423TA02
 1653 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/SCT',2X,'CVT/LCG4',   
     *        4X,'MUVT',4X,'MUVT/SCT',1X,'MUVT/LCG4',/)  
c1654 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',    0423TA02
c    *        2X,'CVT/LCG4',4X,'ICVT',4X,'ICVT/SCT',1X,'ICVT/LCG4',     0423TA02
c    *        2X,'MUVT',4X,'MUVT/SCT',1X,'MUVT/LCG4',/)                 0423TA02
 1654 FORMAT(/4X,'T(K)',5X,'TST',7X,'CVT',6X,'CVT/ZCT',3X,'CVT/SCT',  
     *        2X,'CVT/LCG4',4X,'MUVT',4X,'MUVT/SCT',1X,'MUVT/LCG4',/) 
 1700 FORMAT(1X,F8.2,1P,8E10.2)                                         10/5WH92
 1750 FORMAT(1X,F8.2,1P,10E10.2)                                        10/5WH92
 1775 FORMAT(1X,F8.2,1P,11E10.2)                                        0916PF97
 2000 FORMAT(/1X)
 4400 FORMAT(//1X,21('*'),' CALCULATION WITH SCALED FREQUENCIES ',      0815JC00
     *       21('*'))
C
      END subroutine table
C
C***********************************************************************
C  TAUV
C***********************************************************************
C
      FUNCTION tauv (ENVIB,ELIMIT,BKT,DETI,ESUB,TERM1,TERM2,Z1,Z2,ICD)
C
C     CALCULATES TAUV FOR A GIVEN SET OF NV'S IN DIFFERENT MODES.
C     CONSTT=2**(2.5).  ZIGAMA(Z) IS INCOMPLETE GAMMA FUNCTION(3/2,Z)
C
C     CALLED BY:
C                STAUV
C     CALLS:
C            ZIGAMA
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CONSTT = 5.6568542494923802D0
      ESUB = ELIMIT-ENVIB
      ZIG = ESUB/BKT
      Z1 = ZIG
      IF (ICD.EQ.4) THEN
         TAUV = ZIGAMA(ZIG)
         Z2 = TAUV
         TAUV = TAUV*BKT*SQRT(BKT)
         TAUV = TAUV*EXP(-ENVIB/BKT)
         TERM1 = TAUV
         TAUV = TAUV-(2.0D0*ESUB*SQRT(ESUB)/3.0D0)*EXP(-ELIMIT/BKT)
         TERM2 = TERM1-TAUV
         TAUV = TAUV*SQRT(DETI)*CONSTT
      ELSEIF (ICD.EQ.3) THEN
         TAUV = 1.0D0-EXP(-ZIG)
         Z2 = TAUV
         TAUV = TAUV*BKT*EXP(-ENVIB/BKT)
         TERM1 = TAUV
         TERM2 = ESUB*EXP(-ELIMIT/BKT)
         TAUV = TAUV-TERM2
         TAUV = TAUV*2.0D0*DETI
      ENDIF
      RETURN
      END function tauv
C
C***********************************************************************
C  THETA2
C***********************************************************************
C
      SUBROUTINE theta2 (XMU,A,B,E,TH1,DTH1,TH2,DTH2,IERR)
      use perconparam
C
C     CALLED BY:
C                 WKBPOT
C     CALLS:
C            ELLIP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IERR = 0
      AH = 0.5D0*A
      TS = AH*AH+E*B
      IF (TS.LT.0.0D0) THEN
         WRITE (FU6,1000) E,A,B
         IERR = 1
         RETURN
      ENDIF
      T = SQRT(TS)
      SS = 0.5D0*(1.0D0-AH/T)
      B3PI = B*3.0D0*PI
      T1 = 2.0D0*SQRT(XMU*T)/B3PI
      IF (SS.LT.0.0D0.OR.SS.GE.1.0D0) THEN
         WRITE (FU6,1100) SS
         IERR = 1
         RETURN
      ENDIF
      CALL ELLIP (SS,EL1,EL2)
      TS = 0.5D0*(1.0D0+AH/T)
      IF (TS.LT.0.0D0.OR.TS.GE.1.0D0) THEN
         WRITE (FU6,1100) TS
         IERR = 1
         RETURN
      ENDIF
      CALL ELLIP (TS,EL3,EL4)
      TH1 = T1*((AH+T)*EL1-A*EL2)
      TH2 = 2.0D0*T1*((T-AH)*EL3+A*EL4)
      T1 = SQRT(XMU/T)/PI
      DTH1 = 0.5D0*T1*EL1
      DTH2 = T1*EL3
      RETURN
C
 1000 FORMAT(3X,'THETA2 PROBLEM: GONE NEGATIVE',3E16.6)
 1100 FORMAT(3X,'THETA2 PROBLEM: X OUT OF RANGE',E16.6)
C
      END SUBROUTINE theta2
C
C***********************************************************************
C  THETA3
C***********************************************************************
C
C PARAMETERS MODIFIED 6/18/91
C
      SUBROUTINE theta3 (XMU,A,B,E,TH1,DTH1,TH2,DTH2,IERR)
      use perconparam
C
C     CALLED BY:
C                WKBPOT
C     CALLS:
C           ELLIP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IERR = 0
      RTEB = SQRT(E*B)
      RTEB2 = 2.0D0*RTEB
      B3PI = B*3.0D0*PI
      T1 = A+RTEB2
      IF (RTEB2.GT.A) GO TO 10
C
C      E BELOW BARRIER
C
      QS = 2.0D0*RTEB2/T1
      IF (QS.LT.0.0D0.OR.QS.GE.1.0D0) THEN
         WRITE (FU6,1000) QS
         IERR = 1
         RETURN
      ENDIF
      CALL ELLIP (QS,EL1,EL2)
      TH1 = SQRT(XMU*T1)*(A*(EL2-EL1)+RTEB2*EL1)/B3PI
      DTH1 = SQRT(XMU/T1)*EL1/PI
      T2 = A-RTEB2
      RS = T2/T1
      IF (RS.LT.0.0D0.OR.RS.GE.1.0D0) THEN
         WRITE (FU6,1000) RS
         IERR = 1
         RETURN
      ENDIF
      CALL ELLIP (RS,EL1,EL2)
      TH2 = -2.0D0*SQRT(XMU*T1)*(A*EL2-RTEB2*EL1)/B3PI
      DTH2 = SQRT(XMU/T1)*2.0D0*EL1/PI
      RETURN
C
C    E ABOVE BARRIER
C
   10 T2 = RTEB2-A
      SS = 0.5D0*T1/RTEB2
      IF (SS.LT.0.0D0.OR.SS.GE.1.0D0) THEN
         WRITE (FU6,1000) SS
         IERR = 1
         RETURN
      ENDIF
      CALL ELLIP (SS,EL1,EL2)
      TH1 = SQRT(XMU*RTEB)*(T2*EL1+2.0D0*A*EL2)/B3PI
      DTH1 = SQRT(XMU/RTEB)*EL1/TPI
      TS = 0.5D0*T2/RTEB2
      IF (TS.LT.0.0D0.OR.TS.GE.1.0D0) THEN
         WRITE (FU6,1000) TS
         IERR = 1
         RETURN
      ENDIF
      CALL ELLIP (TS,EL1,EL2)
      TH2 = 2.0D0*SQRT(XMU*RTEB)*(T1*EL1-2.0D0*A*EL2)/B3PI
      DTH2 = SQRT(XMU/RTEB)*EL1/PI
      RETURN
C
 1000 FORMAT(3X,'THETA3 PROBLEM: X OUT OF RANGE',E16.6)
C
      END SUBROUTINE theta3
C
C***********************************************************************
C  TITLE
C***********************************************************************
C
      SUBROUTINE title (IRW,IO,ITLE)
C
C read/write title from/to device IO
C    One-line title limited to 80 characters.
C IRW <=  0, formatted read,
C IRW >=  1, formatted write
C     CALLED BY:
C                RESTOR,RPHWRT,RPHSET,TABLE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1 CTITLE(80,3)
      save ctitle
      IT = MAX(1,ITLE)
      IT = MIN(3,IT)
      IF (IRW.LE.0) THEN
         READ (IO,1000) (CTITLE(I,IT),I=1,80)
      ELSEIF (IRW.GE.1) THEN
         IF (IO.NE.6) WRITE (IO,1000) (CTITLE(I,IT),I=1,80)
         IF (IO.EQ.6) THEN
            WRITE(IO,*)
            WRITE (IO,1100) (CTITLE(I,IT),I=1,80)
         ENDIF
      ENDIF
      RETURN
C
 1000 FORMAT(80A1)
 1100 FORMAT(1X,80A1)
C
      END SUBROUTINE title 
C
C***********************************************************************
C  TP
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/18/91
C
      SUBROUTINE tp (E,T,TMIN,TMAX,VI,LIFREQ,
     *               JFREQ,TX,THMAX,IDO,DEDT,ICON)
      use perconparam
      use common_inc
      use rate_const, only : wkbtol,kbquad
C
C   ROUTINE TO LOCATE A TURNING POINT ON NORMAL MODE COORDINATE OR
C   EVALUATE THE POTENTIAL AT A GIVEN POINT ON THE COORDINATE.
C
C    Rewritten February, 1987 by GCH
C
C  INPUT/OUTPUT :
C       E    = On input, energy for which turning point is to be found.
C              If IDO=0,on output E is the energy at the given point T.
C       T    = On input, if IDO=0, coordinate along normal mode at which
C              potential is to be calculated.  Otherwise, T is the guess
C              to the turning point at energy E. On output, if IDO > 0,
C              T is the calculated turning point.
C       TMIN = On input, the lower limit used in search for T.
C       TMAX = On input, the limit used in search for T (used for IDO=1)
C       VI   = On input, the potential evaluated at T=0.
C      LIFREQ= On input, the index of the current mode, counting all 3N
C              frequencies.
C       JFREQ= On input, the index of the current mode,not including the
C              rotational,translational (or, for GTS, the gradient) mode
C       TX   = On input, mass scaled coordinates of system at T=0.
C       THMAX= Maximum for approximation to phase integral calculated
C              during search for turning point with no upper limit to
C              value of T (used only when IDO=2)
C       IDO  = On input, if:
C              IDO =0, find the energy at coordinate T.
C                  =1, find turning point of energy E between TMIN,TMAX.
C                  =2, look for a turning point, beginning at TMIN.  End
C                      search if an approximation to the phase integral
C                      is larger than THMAX.
C       DEDT = On output, the derivative of potential wrt normal mode.
C       ICON = On output, ICON=0 if search was completed successfully,=1
C              if no turning point could be located. If IDO=0, ICON=1
C              means that T is not a turning point for any E > 0.
C
C
C     CALLED BY:
C                PHSINT,WKBVIB
C
C     CALLS:
C                TRANS,FIRST
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*
      DIMENSION TX(N3TM)
C
      TEPS = DERSTP
      ICON = 0
      ISTART = 0
      OPROD=0.0D0
C
C  SET ZMAX,ZMIN EQUAL TO THE ABSOLUTE VALUES OF TMAX AND TMIN
C
      IF (IDO.NE.0) THEN
         Z1 = ABS(TMAX)
         Z2 = ABS(TMIN)
         IF(IDO.EQ.2) THEN
           ZMIN=Z2
         ELSE
           ZMAX = MAX(Z1,Z2)
           ZMIN = MIN(Z1,Z2)
         END IF
         SGN = SIGN(1.0D0,T)
      ENDIF
      IF (IDO.EQ.2) GO TO 90
C
C   BEGIN LOOP OVER INTERPOLATIONS (IF IDO=1)
C
C   CALCULATE E AND DEDT AT T
C
   10 DO 20 I = 1, N3
         X(I) = TX(I)+T*COF(I,JFREQ)
   20 CONTINUE
c      call first(1)                                                     6/2RS94
      call ghook(1,iproc)
      TE = V-VI
      DEDT = 0.0D0
      DO 30 I = 1, N3
         DEDT = DEDT+DX(I)*COF(I,JFREQ)
   30 CONTINUE
      PROD = T*DEDT
C
C  IDO=0 - TEST IF T IS A PROPER TURNING POINT AND RETURN
C
      IF (IDO.EQ.0) THEN
         IF (PROD.LT.0.0D0.OR.TE.LT.0.0D0) ICON = 1
         E = TE
         RETURN
      ENDIF
C
C  TEST FOR CONVERGENCE
C
      EDIF = E-TE
      IF (DEDT.NE.0.0D0) THEN
         DELT = EDIF/DEDT
      ELSE
         DELT = 0.0D0
      ENDIF
      IF (PROD.GE.0.0D0.AND.ABS(T).LE.ZMAX.AND.ABS(T).GE.ZMIN) THEN
         IF (ABS(EDIF).LT.WKBTOL.AND.ABS(DELT).LT.TEPS) RETURN
      ENDIF
C
C  SET NEW VALUES FOR ZMIN OR ZMAX
C
      IF (EDIF.LT.0.0D0) THEN
C
C   SET ZMAX=T AND DEFINE EP AND DEDTP
C
         ZMAX = ABS(T)
         DEDTP = DEDT
         EP = TE
         IF (ISTART.EQ.0) THEN
            DO 40 I = 1, N3
               X(I) = TX(I)+SGN*ZMIN*COF(I,JFREQ)
   40       CONTINUE
c            call first(1)                                               6/2RS94
            call ghook(1,iproc)                                               0301YC97
            EM = V-VI
            DEDTM = 0.0D0
            DO 50 I = 1, N3
               DEDTM = DEDTM+DX(I)*COF(I,JFREQ)
   50       CONTINUE
            ISTART = 1
         ENDIF
      ELSE
C
C   SET ZMIN=T AND DEFINE EM AND DEDTM
C
         ZMIN = ABS(T)
         DEDTM = DEDT
         EM = TE
         IF (ISTART.EQ.0) THEN
            DO 60 I = 1, N3
               X(I) = TX(I)+SGN*ZMAX*COF(I,JFREQ)
   60       CONTINUE
c            call first(1)                                               6/2RS94
            call ghook(1,iproc)                                               0301YC97
            EP = V-VI
            DEDTP = 0.0D0
            DO 70 I = 1, N3
               DEDTP = DEDTP+DX(I)*COF(I,JFREQ)
   70       CONTINUE
            ISTART = 1
         ENDIF
      ENDIF
C
C   ESTIMATE THE POSITION OF THE TURNING POINT BY FITTING A QUADRATIC TO
C   VALUES OF THE POTENTIAL AT ZMAX AND ZMIN, AND THE SLOPE AT ZMAX OR Z
C
   80 DIFP = E-EP
      DIFM = E-EM
      IF (ABS(DIFP).LT.ABS(DIFM)) THEN
         X1X = SGN*ZMAX
         X2 = SGN*ZMIN
         F1 = EP
         F2 = EM
         FD = DEDTP
      ELSE
         X1X = SGN*ZMIN
         X2 = SGN*ZMAX
         F1 = EM
         F2 = EP
         FD = DEDTM
      ENDIF
      CALL QSLVE (X1X,X2,F1,F2,FD,E,T)
C
C  END OF LOOP OVER INTERPOLATION
C
      GO TO 10
C
C  IDO=2 : LOOK FOR A TURNING POINT > ZMIN (UPPER BOUND ONLY ON VALUE OF
C          THMAX) NOTE: V(ZMIN)-VI MUST BE LESS THAN OR EQUAL TO E
C
   90 CONTINUE
      CONST = SQRT(2.0D0*REDM)/PI
      JSTRT = 0
      SUM = 0.0D0
      X2 = SGN*ZMIN
      PART = THMAX/DBLE(KBQUAD)
      PART = PART/CONST
C
C   EVALUATE THE POTENTIAL AT X2
C
  100 DO 110 I = 1, N3
         X(I) = TX(I)+X2*COF(I,JFREQ)
  110 CONTINUE
c      call first(1)                                                     6/2RS94
      call ghook(1,iproc)                                                     0301YC97
      E2 = V-VI
      DEDX2 = 0.0D0
      DO 120 I = 1, N3
         DEDX2 = DEDX2+DX(I)*COF(I,JFREQ)
  120 CONTINUE
      PROD = DEDX2*X2
C
C  TEST IF A LOCAL MAXIMUM HAS BEEN PASSED
C
      PPROD = PROD*OPROD
      IF (PPROD.LT.0.0D0.AND.PROD.LT.0.0D0) THEN
C
C  FIND THE VALUE OF THE POTENTIAL AT THE LOCAL MAXIMUM BY BISECTION
C
         Z1 = X1X
         DEDZ1 = DEDX1
         Z2 = X2
  130    Z = 0.5D0*(Z1+Z2)
         DO 140 I = 1, N3
            X(I) = TX(I)+Z*COF(I,JFREQ)
  140    CONTINUE
c         call first(1)                                                  6/2RS94
         call ghook(1,iproc)                                                  0301YC97
         EZ = V-VI
         DEDZ = 0.0D0
         DO 150 I = 1, N3
            DEDZ = DEDZ+DX(I)*COF(I,JFREQ)
  150    CONTINUE
         PRODZ = DEDZ*Z
C
C  TEST FOR Z AT THE POSITION OF THE LOCAL MAXIMUM
C
         DELE = DEDZ*(Z2-Z1)
         IF (ABS(DELE).LT.WKBTOL) GO TO 160
C
C  MAKE A NEW GUESS TO THE POSITION OF THE MAXIMUM
C
         IF (PRODZ.LT.0.0D0) THEN
            Z2 = Z
         ELSE
            Z1 = Z
            DEDZ1 = DEDZ
         ENDIF
         GO TO 130
C
C  IF THE LOCAL MAXIMUM >= E FIND THE TURNING POINT BY INTERPOLATION
C
  160    IF (EZ.GT.E) THEN
            ZMIN = ABS(X1X)
            ZMAX = ABS(Z)
            EP = EZ
            EM = E1
            DEDTP = DEDZ
            DEDTM = DEDX1
            GO TO 80
         ELSEIF ((E-EZ).LT.WKBTOL) THEN
            IF (PRODZ.GE.0.0D0) THEN
               T = Z
               DEDT = DEDZ
            ELSE
               T = Z1
               DEDT = DEDZ1
            ENDIF
            RETURN
         ENDIF
      ENDIF
C
C   TEST IF E2 > E WITH PROPER SIGN OF THE DERIVATIVE.
C
      IF (JSTRT.EQ.0.AND.E2.NE.E) THEN
         JSTRT = 1
      ELSE
         EDIF = E-E2
         IF (EDIF.LT.0.0D0.AND.PROD.GT.0.0D0) THEN
C
C  THE TURNING POINT IS BETWEEN X1X AND X2. DO INTERPOLATION.
C
            ZMIN = ABS(X1X)
            ZMAX = ABS(X2)
            EP = E2
            EM = E1
            DEDTP = DEDX2
            DEDTM = DEDX1
            GO TO 80
         ENDIF
C
C   ADD CONTRIBUTION TO APPROXIMATION TO INTEGRAL AND COMPARE TO THMAX
C
         EZ = MAX(E1,E2)
         IF (E.GT.EZ) THEN
            SUM = SUM+ABS(X2-X1X)*SQRT(E-EZ)*CONST
            IF (SUM.GT.THMAX) THEN
C
C   NO TURNING POINT FOUND
C
               ICON = 1
               RETURN
            ENDIF
C
         ENDIF
      ENDIF
C
C   STEP TO NEW POINT
C
      IF (E.NE.E2) THEN
         DELX = SGN*PART/SQRT(ABS(E-E2))
      ELSE
         DELX = SGN*ABS(ZMAX-ZMIN)/DBLE(KBQUAD)
      ENDIF
      X1X = X2
      E1 = E2
      DEDX1 = DEDX2
      OPROD = PROD
      X2 = X1X+DELX
      GO TO 100
C
      END SUBROUTINE tp
C
C***********************************************************************
C  TPCDSC
C***********************************************************************
C
      SUBROUTINE tpcdsc (B,BKAP,FREQ,N3,REDM,TP)
      use perconparam,only : N3TM,NVIBM,N3M7
C
C  Computes effective turning point (TP) along the curvature dirrection
C
C     CALLED BY:
C                MUCDSC
C
C    CONVERSION TO INCLUDE FILES DONE 26/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION FREQ(N3TM),B(NVIBM)
C
      ISHFT = N3-N3M7
C
C If at end of the grid reuse turning point and zero-point energy
C    derivative info from previous points
C
      TP = 0.D0
      DO 10 I = 1, N3M7
         IF (FREQ(I+ISHFT).GT.0) TP = TP + (B(I)*FREQ(I+ISHFT))**2
   10 CONTINUE
      IF (BKAP.EQ.0) THEN                                               0426YC96
           TP = 0.D0                                                    0426YC96
      ELSE                                                              0426YC96
           TP = SQRT(BKAP/(REDM*SQRT(TP)))                              0426YC96
      ENDIF                                                             0426YC96
      RETURN
      END SUBROUTINE tpcdsc                                    
C***********************************************************************
C  TRANS
C***********************************************************************
      SUBROUTINE trans (IOP,N3,AMASS,X,DX)
      use perconparam
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION X(N3TM),DX(N3TM),AMASS(N3TM)
C
C     CONVERTS CARTESIAN TO MASS-WEIGHTED COORDINATES (IOP=1)
C     OR MASS-WEIGHTED TO CARTESIAN COORDINATES (IOP=2)
C     ALSO SIMULTANEOUSLY CONVERTS DERIVATIVES
C
      DO 10 I = 1, N3
         IF (IOP.EQ.2) THEN
            DX(I) = DX(I)*AMASS(I)
            X(I) = X(I)/AMASS(I)
         ELSE
            DX(I) = DX(I)/AMASS(I)
            X(I) = X(I)*AMASS(I)
         ENDIF
   10 CONTINUE
      RETURN
      END SUBROUTINE trans
C
C***********************************************************************
C  TREPT
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/20/91
C
      SUBROUTINE trept (IOP,X,F,XMAX,FMAX)
      use perconparam, only : eps
C
C     COMPUTES PARABOLIC FIT F=AX2+BX+C THRU THREE  POINTS --
C     (X(I),F(I),I=1,3)
C     FOR IOP=0, FMAX=F(XMAX) ONLY
C     FOR IOP .GT. 0,XMAX RECOMPUTED
C
C     CALLED BY:
C                FITMAX,VTMUSN
C     CALLS:
C            MXLNEQ
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      dimension ab(3,4)
C
      DIMENSION X(3),F(3),ISCR(3)
C
      DO 10 I = 1, 3
         AB(I,1) = X(I)*X(I)
         AB(I,2) = X(I)
         AB(I,3) = 1.0D0
         AB(I,4) = F(I)
   10 CONTINUE
      CALL MXLNEQ (AB,3,3,DET,JRANK,EPS,ISCR,-1,4)                      9/20DL90
      IF (JRANK.LT.3) STOP 'TREPT 1'
      A = AB(1,4)
      B = AB(2,4)
      C = AB(3,4)
      IF (IOP.NE.0) THEN
         XMAX = -B/(2.0D0*A)
      ENDIF
      FMAX = XMAX*(B+XMAX*A)+C
      RETURN
      END SUBROUTINE trept
C
C***********************************************************************
C  TSOUT
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/18/91
C    MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
      SUBROUTINE tsout
      use rate_const
      use perconparam
      use common_inc
      use kintcm, only : ifrfac
      use keyword_interface, only : ivice
C
C     COMPUTES AND OUTPUTS FINAL TST RATE CONSTANTS
C     ALSO OBTAINS ACTIVATION ENERGIES
C
C      CALLED BY:
C                 TSRATE
C      CALLS:
C             TTABLE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TAVE(10)
C
C
C     STATEMENT FUNCTION
C
      EACT(B1,B2,XK1,XK2) = ((LOG(XK2/XK1))/(B1-B2))*CKCAL
C
C     INPUT INFO FOR ACTIVATION ENERGY ANALYSIS HAS BEEN MOVED TO READ5
C
      WRITE (FU6,1000)
C
      IF (LGS(1).EQ.0) WRITE (FU6,1100)
C
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
      LGS16 = 2-LGS(16)
      DO 90 IPASS = 1, LGS16
         IF (IPASS.EQ.1) THEN
            IF(LGS(6).GT.2 ) THEN
               IF(ICODE(1).GT.0 .AND. LGS(34).NE.0) THEN
                  WRITE (FU6,1150)
               ELSE
                  WRITE (FU6,1200)
               ENDIF
            ELSE
               IF(ICODE(1).LT.0 .AND. ICODE(2).LT.0) THEN
                  WRITE (FU6,1200)
               ELSE
                  WRITE (FU6,1150)
               ENDIF
            ENDIF
         ELSE
            IF(LGS(6).EQ.2.OR.LGS(6).EQ.4) THEN
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
         WRITE (FU6,1300)
         DO 20 IT = 1, NTEMP
            CONX = CONF(IT)
            IF (IPASS.EQ.2) CONX = CONR(IT)
            WRITE (FU6,1050) TEMP(IT),CONX
   20    CONTINUE
C
C     ACTIVATION ENERGIES BY TWO POINT FITS TO K=A*EXP(-EA/RT)
C
         IF (NPAIR.EQ.0) GO TO 80
         WRITE (FU6,1650)
         WRITE (FU6,1700)
         DO 50 IPR = 1, NPAIR
            I1 = IT1(IPR)
            I2 = IT2(IPR)
            TAVE(IPR) = 2.0D0/(1.0D0/TEMP(I1)+1.0D0/TEMP(I2))
            B1 = BETA(I1)
            B2 = BETA(I2)
            E1 = EACT(B1,B2,CONF(I1),CONF(I2))
            IF (IPASS.EQ.2) E1 = EACT(B1,B2,CONR(I1),CONR(I2))
            WRITE (FU6,1750) TEMP(I1),TEMP(I2),TAVE(IPR),E1
   50    CONTINUE
C
C     WRITE OUT SUMMARY OF FORWARD RATES TO FOR015
C
   80    IF (IPASS.EQ.1) THEN
C
            IF (IFRFAC.EQ.1) WRITE (FU15,4400)                          0815JC00
C
            CALL DATTIM (FU15)
c           CALL TITLE (1,FU15,1)
            CALL wTITLE (FU15)
            IF (LZOC) THEN                                              0824YC98
               IF (IVICE.NE.2) THEN
                  WRITE(FU15,1900)                                      1027WH92
               ELSE
                  WRITE(FU15,1901)
               ENDIF
            ENDIF                                                       0824YC98
            WRITE (FU15,2000)
            WRITE (FU15,2100)
            DO 60 I = 1, NTEMP
               WRITE (FU15,2200) TEMP(I),CONF(I)
   60       CONTINUE
            WRITE(FU15,2600)                                            1027WH92
         ENDIF
C             
   90 CONTINUE
C
      IF (LGS(16) .EQ. 1) RETURN
      WRITE (FU6,2550)
      DO 100 IT = 1, NTEMP
         EQCONF = CONF(IT)/CONR(IT)                                     18/4/91VM
         EQCONR = 1.0D0/EQCONF
         WRITE (FU6,1400) TEMP(IT),EQCONF,EQCONR
  100 CONTINUE
C
      RETURN
C
 1000 FORMAT(/1X,25(1H*),17H  Final results  ,26(1H*))                  1027WH92
 1050 FORMAT(1X,F11.2,1P,E22.6)                                         1027WH92
 1100 FORMAT(/,50H Conventional TST rates have been set equal to one,/)
 1150 FORMAT(/1X,'  Forward rates (cm**3/molecule-sec)',/1X,36('-'))    1027WH92
 1200 FORMAT(/1X,'      Forward rates (sec**-1)       ',/1X,36('-'))    1027WH92
 1250 FORMAT(/1X,'  Reverse rates (10**-',I2,' sec**-1)  ',/1X,36('-')) 1027WH92
 1300 FORMAT(7X,'T(K)  ',12X,'TST',/)                                   ..
 1400 FORMAT(1X,F11.2,1P,E22.6,1P,E22.6)                                ..
 1500 FORMAT(/1X,'  Reverse rates(10**-',I2,'cm**3/molecule-sec)',      ..
     *       /1X,45('-'))   
 1650 FORMAT(/1X,7(1H*),' Activation energies (kcal/mol) ',7(1H*))      1027WH92         
 1700 FORMAT(/4X,7HLower T,3X,7HUpper T,4X,5HAve T,7X,3HTST,/)          1027WH92
 1750 FORMAT(1X,3F10.2,F13.6)                                           1027WH92
 1900 FORMAT(/1X,'The following are VTST-IOC calculation results.')     0115WH92
 1901 FORMAT(/1X,'The following are VTST-ISPE calculation results.')    0708YC98
 2000 FORMAT(/1X,'Forward TST rate constants:')
 2100 FORMAT(/7X,'T',13X,'TST',/)                                       1027WH92             
 2200 FORMAT(1X,F10.2,4X,1P,E12.2)                         
 2550 FORMAT(/8X,'Equilibrium constants (unitless or cgs units)',7X,    1027WH92
     *        /1X,60('-'),/7X,'T(K) ',12X,'FORWARD',15X,'REVERSE',/)
 2600 FORMAT(/1X)
 4400 FORMAT(//1X,21('*'),' CALCULATION WITH SCALED FREQUENCIES ',
     *       21('*'))
C
      END SUBROUTINE tsout
C***********************************************************************
C  TSRATE
C***********************************************************************
C
C
      SUBROUTINE tsrate
      use perconparam
      use common_inc
      use rate_const
      use kintcm
      use keyword_interface
      use cm, only : frict
C
C     THIS ROUTINE SETS UP TEMPERATURE LOOP
C     THEN, FOR EACH T, DELTA G(S) IS CALCULATED AND STORED
C
C      USES L0 TO TELL VPART TO INCLUDE ONLY GROUND OR FIRST EXCITED
C      STATE WHEN APPROPRIATE IN VIB. ADIAB. OR DIAB. CALC.
C
C      PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C      MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C
C     CALLED BY:
C                MAIN
C     CALLS:
C            TSOUT, RQCOM, SSAVE, HRPART, PTQVIB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
C
      DIMENSION QVSV(N6TM)
C
      IF (LGS(34) .NE. 0) THEN
         ISHFT = 1
      ELSEIF (ICODE(5).EQ.3) THEN
         ISHFT = 6
      ELSE
         ISHFT = 7
      ENDIF
      N3M7 = N3-ISHFT
C
      IF (LGS(3).EQ.0) THEN
         S = 0.0D0
C
         IF (LGS(1).NE.0) THEN
            LSAVE = 1
            NSHLF = 1
            CALL SSAVE (LSAVE,BKAP,0)
         ENDIF
      ELSE
         IF (LGS(1).NE.0) LSAVE = NSHLF
      ENDIF
C
C     WRITE OUT SECTION HEADER
C
      IF (LGS(9).NE.0) GO TO 210
C
      WRITE (FU6,1000)
C
C   SCALING THE REACTANT AND PRODUCT FREQUENCIES WHICH ARE THEN 
C   BECOME REACTION COORDINATE.
C
      IF (IFRFAC.NE.0) goto 87                                           0814JC00
      IF (VFAC .NE. 1.0D0) THEN
         IF(ISWR.NE.0 .AND. LGS(6).GE.3) THEN
           VAR = VAR - 0.5D0*WER(ISWR)*(1.0D0 - SQRT(VFAC))
           WER(ISWR) = WER(ISWR)*SQRT(VFAC)
           XER(ISWR) = XER(ISWR)/SQRT(VFAC)
         ENDIF
         IF(ISWP.NE.0.AND.LGS(6).EQ.2.OR.LGS(6).EQ.4) THEN
           ISWP = NF(1) + NF(2) + ISWP
           VAP = VAP - 0.5D0*WER(ISWP)*(1.0D0-SQRT(VFAC))
           WER(ISWP) = WER(ISWP)*SQRT(VFAC)
           XER(ISWP) = XER(ISWP)/SQRT(VFAC)
         ENDIF
      ENDIF
C
      WRITE (FU6,2600) NTEMP
C
C   CHECK IF QUACK-TROE VMEP OR Q'S TO BE USED
C
      IF (LGS(1).NE.0) THEN
C      IF (LGS(18).EQ.0 .AND. LGS(1).NE.0) THEN
C
C     SCALE VMEP BY VFAC
C
         WRITE (FU6,2750) VFAC
         IF (VFAC.NE.1.0D0) THEN
            DO 60 I = NSHLF, LSAVE
               ZPE = VADIB(I)-VCLAS(I)
               VCLAS(I) = VCLAS(I)*VFAC
               VADIB(I) = VCLAS(I)+ZPE
   60       CONTINUE
         ENDIF
      ENDIF              
C
C SCALING QV
C
      IF (LGS(37) .NE. 0) WRITE(FU6,1570) LGS(37)
C
      DO 80 I = 1, NTEMP
         BETA(I) = 1.0D0/(BK*TEMP(I))
   80 CONTINUE
C
C COMPUTE THE RELATIVE TRANSLATIONAL REDUCED MASS OF THE FORWARD AND
C REVERSE REACTIONS AND REPLACED THE SCALING REDUCED MASSES REDM AND
C REDMR BY THE NONSCALING VALUES BEFORE CALCULATE THE QTR PF'S.
C
 210  R1MAS = 0.0D0
      DO 82 IMS = 1,NRATOM(1)
82       R1MAS = R1MAS + SVMAS(IATSV(IMS,1))
      R2MAS = 0.0D0
      DO 84 IMS = 1,NRATOM(2)
84       R2MAS = R2MAS + SVMAS(IATSV(IMS,2))
      REDMF = (R1MAS*R2MAS)/(R1MAS+R2MAS)
C
C IF GAS MOLECULE + SOLID SURFAC THEN REDMF IS THE MOLECULAR MASS
C OF THE GAS MOLECULE
C
      IF(LGS(34).NE.0 .AND. ICODE(1).GT.0) REDMF = R1MAS
C
      P1MAS = 0.0D0
      DO 86 IMS = 1,NRATOM(3)
86       P1MAS = P1MAS + SVMAS(IATSV(IMS,3))
      P2MAS = 0.0D0
      DO 88 IMS = 1,NRATOM(4)
88       P2MAS = P2MAS + SVMAS(IATSV(IMS,4))
      REDMR = (P1MAS*P2MAS)/(P1MAS+P2MAS)
      IF(LGS(34).NE.0 .AND. ICODE(3).GT.0) REDMR = P1MAS
C
87    CONTINUE                                                           0814JC00
C
C EVALUATE THE EXPONENT FACTOR WHICH WILL BE FACTORED OUT OF THE
C VIBRATIONAL PF TO AVOID UNDERFLOW.
C
      FAC37 = DBLE(LGS(37))*LOG(10.0D0)
C
C     LOOP OVER TEMPERATURES
C
      DO 200 ITEMP = 1, NTEMP
         IF ((LGS(7).LE.-2.AND.LGS(1).NE.0).OR.(LGS(7).EQ.-4.AND.LGS(1)
     *      .EQ.0)) WRITE (FU6,1650) TEMP(ITEMP)
C
         ISTART = NSHLF
         ISTOP = LSAVE
C
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
            ELSE
               QTR = 1.0D0
               QTRCC = 1.0D0
            ENDIF
C
C .......................    ROTATION
C
            IF (ICODE(1) .GT. 0 .AND. ICODE(2) .GT. 0) THEN
               QRR = RPART(FMOM(1),BKT,ICODE(1))*RPART(FMOM(2),BKT,
     *                     ICODE(2))
            ELSE IF (ICODE(1) .GT. 0 .AND. LGS(34) .NE. 0) THEN
               QRR = RPART(FMOM(1),BKT,ICODE(1))
            ELSE
               QRR = 1.0D0
            ENDIF
C
C ........................   ELECTRONIC
C
            QER = EPART(1,BKT)*EPART(2,BKT)                             15/1/92VM
C
         ELSE
C
C    1 REACTANT    2 CASES: 1/ GAS REACTANT
C                           2/ GAS REACTANT +  SURFACE
C
C .......................   TRANSLATION
C
            IF (ICODE(1) .GT. 0 .AND. LGS(34) .NE. 0) THEN
               QTR = ((REDMF*BKT)/(2.0D0*PI))**1.5D0
               QTRCC = QTR*CONK0
            ELSE
               QTR = 1.0D0
               QTRCC = 1.0D0
            ENDIF
C
C .......................    ROTATION
C
            IF ( ICODE(1) .GT. 0) THEN
               QRR = RPART(FMOM(1),BKT,ICODE(1))
            ELSE
               QRR = 1.0D0
            ENDIF
C
C   .....................   ELECTRONIC
C
               QER = EPART(1,BKT)                                       15/1/92VM
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
            IF (ICODE(3) .GT. 0 .OR. ICODE(4) .GT. 0) THEN
               QTP = ((REDMR*BKT)/(2.0D0*PI))**1.5D0
               QTPCC = QTP*CONK0
            ELSE
               QTP = 1.0D0
               QTPCC = 1.0D0
            ENDIF
C
C  ....................   ROTATION
C
            IF ( ICODE(3) .GT. 0 .AND. ICODE(4) .GT. 0) THEN
               QRP = RPART(FMOM(3),BKT,ICODE(3))*RPART(FMOM(4),BKT,
     *                    ICODE(4))
            ELSE IF ( LGS(34) .NE. 0 .AND. ICODE(3) .GT. 0) THEN
               QRP = RPART(FMOM(3),BKT,ICODE(3))
            ELSE
               QRP = 1.0D0
            ENDIF
C
C  ....................   ELECTRONIC
C
            QEP = EPART(3,BKT)*EPART(4,BKT)                             15/1/92VM
         ELSE
C
C        1 PRODUCT
C   .....................  TRANSLATION
C
            IF (ICODE(3) .GT. 0 .AND. LGS(34) .NE. 0) THEN
               QTP = ((REDMR*BKT)/(2.0D0*PI))**1.5D0
               QTPCC = QTP*CONK0
            ELSE
               QTP = 1.0D0
               QTPCC = 1.0D0
            ENDIF
C
C   ...................... ROTATION
C
            IF (ICODE(3) .GT. 0) THEN
               QRP = RPART(FMOM(3),BKT,ICODE(3))
            ELSE
               QRP = 1.0D0
            ENDIF
C
C   .....................  ELECTRONIC
C
            QEP = EPART(3,BKT)                                          15/1/92VM
         ENDIF
C
C    REACTANT AND PRODUCT VIBRATIONAL PFS
C
         IOP = 1
         IBEG = 1
         SUM = 0.0D0
         AFLAG = '   '
         IF (LGS(5).GE.21) AFLAG = 'SET'                                 6/30YL91
         DO 140 IY = 1, 2 
            IF (NF(IY).NE.0.AND.LGS2(15).EQ.0) THEN                      1106YL92
               IENDR = NF(IY)+IBEG-1
               J = 1
               DO 130 IX = IBEG, IENDR
C                 IF (AFLAG.EQ.'SET') LGS(5) = MODER(IY,J)              0324JZ10
                  LGS(5) = MODER(IY,J)                                  0324JZ10
                  L0 = LRP(IX)
                  IKBM = IX
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IF (IY .EQ. 1) THEN                                0618WH94
                        IMHR = NF(1)+1-IX                                ..
                     ELSE                                                ..
                        IMHR = NF(1)+NF(2)+1-IX                          ..
                     ENDIF                                               ..
                     IF (IY.EQ.2) THEN                                  0521YC99
                       IXI = IX - NF(1)                                 0521YC99
                     ELSE                                               0521YC99
                       IXI = IX                                         0521YC99
                     ENDIF                                              0521YC99
                     QVSV(IX)=HRPART(0.d0,WER(IX),TORMI(IY,IXI,1),      1020BE06
     *                               BKT,IY,IXI,IMHR)                   1020BE06
                  ELSE                                                  0630YL91
                     QVSV(IX)=VPART(WER(IX),XER(IX),BKT,DEMIN,IOP,         ..
     *                              Y00R(IX))                              ..
                  ENDIF                                                 6/30YL91
                  J = J+1
                    SUM = SUM + LOG(QVSV(IX))
  130          CONTINUE
            ELSEIF (NF(IY).NE.0.AND.LGS2(15).NE.0) THEN                  1106YL92
               EGRNDT = EGRNDR(IY)                                          ..
               NMOD = NF(IY)                                                ..
               DUMMY = PTQVIB(NMOD,N3TM,EGRNDT,EFNDTR(IBEG),                ..
     *                        WER(IBEG),BKT)                                ..
               PROD = PROD * DUMMY                                          ..
               SUM = SUM + LOG(DUMMY)                                       ..
            ENDIF                                                       1106YL92
            IBEG = IBEG+NF(IY)
            IOP = IOP+1
  140    CONTINUE
c
c       solvent coordinate contribution only harmonic                   0317Yc99
c
         if (ibathm.eq.1) then                                          0317Yc99
           FRSOL = (PI/(4*FRICT))                                       0317Yc99
           VPSOL = VPART(FRSOL,0.d0,BKT,DEMIN,IOP,0.d0)                 0824JC00
           SUM = SUM + LOG(VPSOL)                                       0317Yc99
         endif                                                          0317Yc99
         SUMRE = SUM                                                    0423TA02
         QVR = EXP(SUM+FAC37)
         IBEGP = IBEG
         SUM = 0.0D0
         EMAX = DEMIN-EPRD
         IVP = 4
         IF (LGS(6).EQ.2) IVP = 3
         DO 160 IY = 3, IVP
            IF (NF(IY).NE.0.AND.LGS2(15).EQ.0) THEN                     1106YL92
               IENDP = NF(IY)+IBEG-1
               J = 1
               DO 150 IX = IBEG, IENDP
                  IF (AFLAG.EQ.'SET') LGS(5) = MODER(IY,J)
                  L0 = LRP(IX)
                  IKBM = IX
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IF (IY .EQ. 3) THEN                                0618WH94
                        IMHR = NF(1)+NF(2)+NF(3)+1-IX                   0618WH94
                     ELSE                                               0618WH94
                        IMHR = NF(1)+NF(2)+NF(3)+NF(4)+1-IX             0618WH94
                     ENDIF                                              0618WH94
                     IF (IY.EQ.4) THEN                                  0521YC99
                       IXI = IX - NF(1) - NF(2) - NF(3)                 0521YC99
                     ELSE                                               0521YC99
                       IXI = IX - NF(1) - NF(2)                         0521YC99
                     ENDIF                                              0521YC99
                     QVSV(IX)=HRPART(0.d0,WER(IX),TORMI(IY,IXI,1),      1020BE06
     *                               BKT,IY,IXI,IMHR)                   1020BE06
                  ELSE                                                  0630YL91
                      QVSV(IX)=VPART(WER(IX),XER(IX),BKT,EMAX,IOP,         ..
     *                              Y00R(IX))                              ..
                  ENDIF                                                 6/30YL91
                  J = J+1
                  SUM = SUM + LOG(QVSV(IX))
  150          CONTINUE
            ELSEIF (NF(IY).NE.0.AND.LGS2(15).NE.0) THEN                  1106YL92
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
         if (ibathm.eq.1) then
           FRSOL = (PI/(4*FRICT))
           VPSOL = VPART(FRSOL,0.d0,BKT,DEMIN,IOP,0.d0)                 0824JC00
           SUM = SUM + LOG(VPSOL)
         endif
         SUMPR = SUM                                                    0423TA02
         QVP = EXP(SUM+FAC37)
         QINTR = QVR*QRR*QER
C
C   PHIF,PHIR IN A.U. OR UNITLESS.  PHIFCC,PHIRCC IN CGS OR UNITLESS.
C
         PHIF = QTR*QINTR
         PHIFCC = QTRCC*QINTR
         QINTR = QVP*QRP*QEP
         PHIR = QTP*QINTR
         PHIRCC = QTPCC*QINTR
         PREFAC = CNVRT*SIGMAF*BKT/(2.0D0*PI)
         ARG1 = EPRD/BKT
         ARGNEW = ARG1+2.302585093D0*DBLE(LGS(17))
         ARGABS = ABS(ARGNEW)
         IF (ARGABS.GT.75.2D0) THEN
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
         IF (LGS(7).EQ.-4) THEN
C
C     EXTRA OUTPUT OF INDIVIDUAL PARTITION FUNCTIONS
C
            ISTOP = IENDP-IBEGP+1
            IF (IENDR.GT.ISTOP) ISTOP = IENDR
            WRITE (FU6,1700)
            WRITE (FU6,1750) QER,QTR,QRR,QVR,PHIF,QTRCC,PHIFCC
            WRITE (FU6,1800) QEP,QTP,QRP,QVP,PHIR,QTPCC,PHIRCC
            WRITE (FU6,1810)                                            1204WH92
            WRITE (FU6,1815)                                            1204WH92
C
C     Write out the Internal Free Energy of Reactant if Unimolecular
C
            IF ((LGS(6).EQ.3.OR.LGS(6).EQ.4) .AND. ICODE(1).GT.0) THEN  1212PF99
               FRENR = -1*RCONST*T*dlog(QER*QRR*QVR)                    1212PF99
               WRITE (FU6,2051) FRENR                                   1212PF99
               WRITE (FU6,2052)                                         1212PF99
            ENDIF                                                       1212PF99
C
            WRITE (FU6,1850)
C 
            NFRR = NF(1) + NF(2)
            NFRP = NF(3) + NF(4)
            NTOT = NFRR + NFRP
C
            IF (NF(1) .GE.1) WRITE(FU6,1910) (QVSV(I),I=NF(1),1,-1)     0618WH94
            IF (NF(2) .GE.1) WRITE(FU6,1920) (QVSV(I),I=NFRR,NF(1)+1,-1)0618WH94
            IF (NF(3) .GE.1) WRITE(FU6,1930) (QVSV(I),I=NFRR+NF(3),     0618WH94
     *                                        NFRR+1,-1)                 ..
            IF (NF(4) .GE.1) WRITE(FU6,1940) (QVSV(I),I=NTOT,            ..
     *                                        NFRR+NF(3)+1,-1)          0618WH94
            WRITE (FU6,2050)
C
            IF (LGS(1) .EQ. 0) GO TO 190
C
         ENDIF
C
         IF (LGS(1) .EQ. 0) GO TO 190
C
C     COMPUTE GT ELECTRONIC PARTITION FUNCTION AT THIS TEMP
C
         QE = EPART(5,BKT)                                              15/092VM
C
C     LOOP OVER S
C
         ISTART = ISTRTO
         ISTOP = ISTOPO
         DO 180 IS = ISTART, ISTOP
C           IF (LGS(7) .EQ. -4) WRITE (FU6,3300) SSUBI(IS), QE
            IF (LGS(7) .EQ. -4) THEN                                    0405JZ07
              IF(IUNIT6.EQ.1) WRITE (FU6,3300) SSUBI(IS)/GUFAC6, QE    
              IF(IUNIT6.EQ.0) WRITE (FU6,3302) SSUBI(IS)/GUFAC6, QE
            ENDIF                                                       0405JZ07
C
C     CALCULATE GT PARTITION FUNCTION
C
            IF (LGS(34) .NE. 0) THEN
               QR = 1.0D0
            ELSE
               IF (FMITS(IS).EQ.0.0D0) THEN
                  QR = 0.0D0
               ELSE
                  QR = RPART(FMITS(IS),BKT,ICODE(5))
               ENDIF
            ENDIF
            MARR = 1                                                    6/30YL91
C            IF (NARR.GT.1) THEN                                            ..
            IF (AFLAG.EQ.'SET'.AND.NARR.GT.1) THEN
               NARL = NARR - 1                                             ..
C               DO 165 IARR = 1, NARL
               M = MARR                                      
               DO 165 IARR = M, NARL
                  IF (SSUBI(IS).GE.SRARR(IARR)) MARR = MARR + 1            ..
165            CONTINUE                                                    ..
            ENDIF                                                          ..
            DO 166 I = 1, N3M7                                             ..
               MODE(I) = MODETS(MARR,I)                                    ..
166         CONTINUE                                                    6/30YL91
            IF ( QR .NE. 0.0D0) THEN
               VSX = VCLAS(IS)
               IOP = 5
               SUM = 0.0D0
               EMAX = DEMIN-VSX
C no more LGS(19)
               IF (LGS2(15).EQ.0) THEN                                  0610YC96
C              IF (LGS(19).EQ.0.AND.LGS2(15).EQ.0) THEN                 1106YL92
                  JSWITC = 1
                  IF (SSUBI(IS).GE.SWITC) JSWITC = 2
                  DO 170 I = 1, N3M7
                     IF (AFLAG.EQ.'SET') LGS(5) = MODE(I)
                     L0 = LN3(JSWITC,I)
                     IKBS = IS
                     IKBM = I
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IMHR = NF(5) + 1 - I                               0618WH94
                     QVSV(I)=HRPART(SSUBI(I),WETS(I,IS),FMIHTS(I,IS),
     *                       BKT,5,I,IMHR)
                  ELSE                                                  0630YL91
                     QVSV(I)=VPART(WETS(I,IS),XETS(I,IS),BKT,EMAX,IOP,     ..
     *                              Y0TS(I,IS))                            ..
                  ENDIF                                                 6/30YL91
                     SUM = SUM + LOG(QVSV(I))
  170             CONTINUE
                  QV = EXP(SUM+FAC37)
C no more LGS(19)
               ELSEIF (LGS2(15).NE.0) THEN                              0610YC96
C             ELSEIF (LGS(19).EQ.0.AND.LGS2(15).NE.0) THEN              1106YL92
                  EGRNDT = EGRND(IS)                                        ..
                  DO 175 I = 1, N3M7                                        ..
                     EFNDTP(I) = EFNDT(I,IS)                                ..
175               CONTINUE                                                  .. 
                  DUMMY = PTQVIB(N3M7,N3TM,EGRNDT,EFNDTP,WETS(1,IS),        ..
     *                           BKT)                                       ..
                  PROD = PROD * DUMMY                                       ..
                  SUM = SUM + LOG(DUMMY)                                    ..
                  QV = EXP(SUM+FAC37)                                       ..
               ENDIF
C 
               SUMGT = SUM                                              0423TA02
               QGT = QR*QV*QE                                           0610YC96
C               IF (LGS(19).NE.2) QGT = QR*QV*QE
C
C     CALCULATE DELTA G AND SAVE
C     DELG IS FOR TST.
C
               IF (LGS2(16).EQ.0) THEN                                  0423TA02
                  DELG(IS) = RT*((VSX/BKT)-LOG(QGT) + LOG(PHIFCC))
               ELSEIF (LGS2(16).EQ.1) THEN                              0423TA02
                  DELG(IS) = RT*((VSX/BKT) - LOG(QR*QE) - SUMGT         0423TA02
     *                       + LOG(QTRCC*QRR*QER) + SUMRE)              0423TA02
               ENDIF                                                    0423TA02
C
               IF (LGS(7).EQ.-4) THEN
C
C     EXTRA OUTPUT OF INDIVIDUAL PARTITION FUNCTIONS
C
               WRITE (FU6,3310)                                         0618WH94
               WRITE (FU6,3350) QR,QV,QGT                               0618WH94
               WRITE (FU6,3360) (QVSV(I),I=N3M7,1,-1)                   0618WH94
               ENDIF
            ENDIF
  180    CONTINUE
C
C     PRINT OUT DELTA G(S)
C
         IF (LGS(7).LE.-2) THEN
C           WRITE (FU6,3450) T
C           WRITE (FU6,3500) (SSUBI(I),DELG(I),I=ISTART,ISTOP)
            IF(IUNIT6.EQ.1) WRITE (FU6,3450) T                          0405JZ07
            IF(IUNIT6.EQ.0) WRITE (FU6,3460) T                          0405JZ07
            WRITE (FU6,3500) (SSUBI(I)/GUFAC6,DELG(I),I=ISTART,ISTOP)   0405JZ07
         ENDIF
C
C     COMPUTE CONVENTIONAL TST RATES AND STORE FOR SUMMARY
C
  190    IF (LGS(1).NE.0) THEN
            CONF(ITEMP) = PREFAC*EXP(-DELG(NSHLF)/RT)
            CONR(ITEMP) = CONF(ITEMP)*REVFAC
         ELSE
            CONF(ITEMP) = 1.0D0
            CONR(ITEMP) = 1.0D0
         ENDIF
         IF (AFLAG .EQ. 'SET') LGS(5) = NARR + 20                       0330YL92
C
  200 CONTINUE
C
C     COMPUTE FINAL RATES AND OTHER INFO AND OUTPUT
C
      CALL TSOUT
      RETURN
C
 1000 FORMAT(/1X,25(1H*),' Reaction rate calculations ',25(1H*),/)      0618WH94
 1570 FORMAT(/1X,'** The vibrational partition functions are ',
     *'multiplied',/4X,'by a factor of 10**',I2,' to avoid underflow',/)0614WH94
 1650 FORMAT(/1X,26(1H*),' Temperature = ',F8.2,' K ',27(1H*))          1019WH92
 1700 FORMAT(/1X,78(1H-)/,2X,                                           0615WH94
     *'Reactant and product partition functions w/re classical energy ',
     * 'of reactant',/1X,78(1H-)/,
     * 23X,'Qelec',5X,'Phi_rel',7X,'Qrot',8X,                           06/96ELC
     * 'Qvib',8X,'Phi')
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
     *       ' include the rotational symmetry number.')                1213PF99
 2600 FORMAT(3X,27HTemperature loop :  NTEMP =,I5)                      1019WH92
 2750 FORMAT(3X,41HFor this run, VMEP has been multiplied by,F10.5)     1027WH92
 3050 FORMAT(/,7H Temp =,F10.2/30H large argument of exponential ,/1X,
     *1P,2E14.3,5X,3Hphi,1P,2E14.3,
     * /,' ***** Only forward rate constants are calculated.')
 3300 FORMAT(/5X,'s(bohr) = ',F9.5,4X,'Qelec = ',F9.5,/1X,65('-'))      0615WH94
 3302 FORMAT(/5X,'s(angstrom) = ',F9.5,4X,'Qelec = ',F9.5,/1X,65('-'))
 3310 FORMAT(5X,'Qrot',8X,'Qvib',8X,'Q^GT')
 3350 FORMAT(1X,1P,3E12.4)
 3360 FORMAT(/5X,'Individual GTS qvib:',//,(1X,5E12.4))                 0615WH94
 3450 FORMAT(/1X,'Generalized free energy of activation in kcal/mol',   0115WH93
     *       /1X,'vs. s in bohrs for T =',F8.2,' K',
     *       /1X,'     s   ','     delta G')
 3460 FORMAT(/1X,'Generalized free energy of activation in kcal/mol',   0115WH93
     *       /1X,'vs. s in angstroms for T =',F8.2,' K',                0405JZ07
     *       /1X,'     s   ','     delta G')
 3500 FORMAT(1X,F9.5,F12.4)
C
      END SUBROUTINE tsrate
C
C***********************************************************************
C  TURNPT
C***********************************************************************
C
      SUBROUTINE turnpt (I,E,WE,XE,B1,BKF,TP)
      use common_inc, only : lgs,intout,anhrm,ab,redm
      use perconparam
      use rate_const
c
C     Computes concave-side turning point (TP) for E=zero-point energy
C     for curvature component BKF
C
C     CALLED BY:
C                ZEROPT,MUBAR
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      save
C
C  Compute the quadratic coefficient
C
      B0 = 0.5D0*REDM*WE*WE
C
      IF (LGS(33).EQ.1) THEN
C
C   WKB case
C
         TPL(I) = MIN(TP1(I),TP2(I))
         TPG(I) = MAX(TP1(I),TP2(I))
         IF (BKF.LE.0.0D0) TP = TP2(I)
         IF (BKF.GT.0.0D0) TP = TP1(I)
         RETURN
      ENDIF
C
      IF (LGS(5).EQ.0.OR.LGS(5).EQ.9) THEN                              6/30YL91
C
C  Harmonic case
C
         IF (WE.LE.0.0D0) THEN
            TP = 0.0D0
         ELSE
            TP = -SQRT(E/B0)*SIGN(1.0D0,BKF)
         ENDIF
         TPL(I) = -ABS(TP)
         TPG(I) = -TPL(I)
C
      ELSEIF (INTOUT(I).LE.2) THEN
C
C  Morse case
C     First make sure that we have the sign of the 3rd derivative
C
         D3V = B1
         IF (D3V.EQ.0.0D0) D3V = BKF
         DEX = WE/(4.0D0*XE)
         BETAX = WE*SQRT(REDM/(2.0D0*DEX))
         ARG = SQRT(E/DEX)*SIGN(1.0D0,BKF)*(-SIGN(1.0D0,D3V))
         TP = (LOG(1.0D0+ARG)/BETAX)*SIGN(1.0D0,D3V)
         TPG(I) = (LOG(1.0D0-ARG)/BETAX)*SIGN(1.0D0,D3V)
         IF (TP .LT. 0.0D0) THEN
           TPL(I) = TP
         ELSE
           TPL(I) = TPG(I)
           TPG(I) = TP
         ENDIF
C
         ELSEIF (LGS(5).EQ.7.OR.LGS(5).EQ.8) THEN
C
C  Quadratic-quartic case
C
         FB = ANHRM(I)
         A = AB(I)
         ARG = SQRT(FB**2+2.0D0*A*E/3.0D0)
         TP2X = 6.0D0*(-FB+ARG)/A
         TP = -SQRT(ABS(TP2X))*SIGN(1.0D0,BKF)
         TPL(I) = -ABS(TP)
         TPG(I) = -TPL(I)
      ENDIF
C
      RETURN
      END subroutine turnpt                                 
C
C**************************************************************************
C UPDHR
C**************************************************************************
C
      SUBROUTINE updhr
      use common_inc
      use perconparam, only : fu6
      use rate_const
C
C     Called by ZOCUPD
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      IF (LGSIC(9) .GT. 0) THEN
	 IF (LGSIC(9) .NE. 99) THEN
            IC = 0
            DO 20 IM = 1, NF(5)
               IF (MODETS(1,IM) .EQ. 9) THEN
                  IC = IC + 1
                  IF (IC .LE. LGSIC(9)) THEN
                     TEMPMI = FMIHTS(IM,NSHLF)
                     DO 10 IS = 1, LSAVE
                        FMIHTS(IM,IS) = TEMPMI
10                   CONTINUE
                  ENDIF
               ENDIF
20          CONTINUE
	 ENDIF
      ELSE IF (LGSIC(9) .LE. 0) THEN
         IC = 0
         DO 50 IM = 1, NF(5)
            IF (MODETS(1,IM) .EQ. 9) THEN
               IC = IC + 1
               IF (FMIHTS(IM,NSHLF) .NE. 0) THEN
                  ALPHA = HRMITS(IM) / FMIHTS(IM,NSHLF)
               ELSE
                  ALPHA = 1.0D0
               END IF
               WRITE(FU6,1000) NF(5)-IM+1,FMIHTS(IM,NSHLF),HRMITS(IM),
     *                         ALPHA
               IF (IC .LE. ABS(LGSIC(9))) THEN
                  TEMPMI = HRMITS(IM)
                  DO 30 IS = 1, LSAVE
                     FMIHTS(IM,IS) = TEMPMI
30                CONTINUE
               ELSE
                  DO 40 IS = 1, LSAVE
                     FMIHTS(IM,IS) = ALPHA * FMIHTS(IM,IS)
40                CONTINUE
               ENDIF
               ISHFT = N3 - NF(5)
               IF (LGS(3) .EQ. 0) FMOMHR(ISHFT+IM) = HRMITS(IM)
            ENDIF
50       CONTINUE
         IC = 0
         DO 60 IRE = 1, 4
            NFREQ = NF(IRE)
            DO 60 IMM = 1, NFREQ
               IC = IC + 1
               IF (MODER(IRE,IMM) .EQ. 9) THEN
                  FMIHR(IC) = HRMIR(IC)
                  IF (IRE .LE. 2) THEN
                    WRITE(FU6,1100)IRE,NFREQ-IMM+1,FMIHR(IC),HRMIR(IC)
                  ELSE
                    WRITE(FU6,1200)IRE-2,NFREQ-IMM+1,FMIHR(IC),HRMIR(IC)
                  ENDIF
               ENDIF
60       CONTINUE
      ENDIF          
C
      RETURN
C 
1000  FORMAT (//6X,'For Saddle point mode ',I2,
     */6X,'The uncorrected reduced moment of inertia (a.u.)= ',1P,E12.4,
     */6X,'The   corrected reduced moment of inertia (a.u.)= ',1P,E12.4,
     */6X,'The alpha factor = ',0P,F12.4)
1100  FORMAT (//6X,'For reactant ',I1,' mode ',I3, 
     */6X,'The uncorrected reduced moment of inertia (a.u.)= ',1P,E12.4,
     */6X,'The   corrected reduced moment of inertia (a.u.)= ',1P,E12.4)
1200  FORMAT (//6X,'For product ',I1,' mode ',I3, 
     */6X,'The uncorrected reduced moment of inertia (a.u.)= ',1P,E12.4,
     */6X,'The   corrected reduced moment of inertia (a.u.)= ',1P,E12.4)
C
      end subroutine updhr
C
C**********************************************************************
C  UPDMI
C**********************************************************************
C
      SUBROUTINE updmi
      use common_inc
      use perconparam, only : fu6
      use rate_const
C
C     Called by: ZOCUPD
C
C     This subroutine corrects det I
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      FMOM(1) = FMIR1A
      FMOM(2) = FMIR2A
      FMOM(3) = FMIP1A
      FMOM(4) = FMIP2A
      IF (LGS(3) .EQ. 0) THEN
         FMOM(5) = FMISPA
         FMITS(1) = FMISPA
         RETURN
      ENDIF
C
      FACTOR = FMISPA / FMISPS
C
      DO 20 I = 1, LSAVE                                                0726WH93
            FMITS(I) = FMITS(I) * FACTOR
20    CONTINUE
C
      RETURN
C
      END subroutine updmi
C
C**********************************************************************
C  VICLCG
C**********************************************************************
      SUBROUTINE viclcg (LGSIC6,ZETA1,STP0,STP1,VZOCLC,QDX,VICCRT,
     *                   NDIM,IZ0,IVIC)
      use perconparam
C
C     Computes zero-order correction of the classical potential along
C     a tunneling path by interpolating information at the end points of
C     the tunneling path, and the information at the saddle point when
C     the quadratic fit is requested.
C
C     Called by:
C           LCG3TH
C
C     Calls:
C           ZOCVCL, QUADTW
C
C     On input:
C           LGSIC6   = 1, linear fit only
C                      2, linear when the correction to the VMEP at the saddle 
C                       point is between the corrections to the VMEP at the
C                       end points of the tunneling path
C           ZETA1    : the length of the tunneling path
C           STP0,STP1: the s values of the end points of the tunneling path
C           VZOCLC   : the correction to the VMEP at the saddle point
C           QDX      : the array that stores the quadrature points on the
C                      tunneling path
C           NDIM     : the number of quadrature points which are in the
C                      nonadiabatic region
C
C     On output:
C           VICCRT   : the array that stores the correction to the classical
C                      potential along the tunneling path
C           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DVCL(3),ZCOORD(3),VICCRT(NQD),QDX(NQD)
C
c      IF (IVIC.EQ.2) THEN                                               0824YC98
c          DO I = IZ0, NDIM+IZ0-1    
c               FCONST = (QDX(I)-ZCOORD(1))/(ZCOORD(3)-ZCOORD(1))
c               VICCRT(I) = ZOCVCL(STP0+FCONST*(STP1-STP0))
c          ENDDO
c      ELSE
        ZCOORD(1) = 0.0D0
        ZCOORD(2) = 0.0D0
        ZCOORD(3) = ZETA1
        FCONST = 0.0D0
        DVCL(1) = ZOCVCL(STP0)
        DVCL(2) = VZOCLC
        DVCL(3) = ZOCVCL(STP1)
        DVCDMY = (DVCL(1)-DVCL(2))*(DVCL(2)-DVCL(3))
        IF (DVCDMY.LT.0.D0.AND.LGSIC6.EQ.2) THEN
          CALL QUADTW(ZCOORD,DVCL,FCONST)
          DO 100 I = IZ0, NDIM+IZ0-1                                    0806WH93
            VICCRT(I) = DVCL(2) + FCONST*(QDX(I)-ZCOORD(2))**2
100       CONTINUE
        ELSE
          FCONST = (DVCL(3)-DVCL(1))/(ZCOORD(3)-ZCOORD(1))
          DO 200 I = IZ0, NDIM+IZ0-1                                    0806WH93
            VICCRT(I) = DVCL(1) + FCONST*(QDX(I)-ZCOORD(1))
200       CONTINUE
        ENDIF
c      ENDIF
      RETURN
      END subroutine viclcg
C
C***********************************************************************
C  VPART
C***********************************************************************
C
C   PARAMETERS AND COMMON INCLUDE FILES ADDED 23/7/91
C
      FUNCTION vpart (W,XX,BKT,EMAX,IOP,Y0)
      use common_inc; use perconparam; use kintcm
      use rate_const; use keyword_interface
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
  
C
C
C     COMPUTES VIBRATIONAL PARTITION FUNCTION FOR MODE WITH
C     FREQ W AND ANHARM XX.
C     SUMMATION TERMINATED IF E .GT. EMAX OR HAVE REACHED
C     RELATIVE CONVERGENCE WITHIN TOLER
C     OR IF LEVEL ENERGIES REACH A MAXIMUM
C
C      USES L0  TO INCLUDE ONLY GROUND OR FIRST EXCITED
C      STATE WHEN APPROPRIATE IN VIB. ADIAB. OR DIAB. CALC.
C      IF L0 = -1 : MODE IS RESTRICTED TO GROUND STATE
C      IF L0 =  0 : THERMAL TREATMENT OF MODE
C      IF L0 =  1 : MODE IS RESTRICTED TO FIRST EXCITED STATE
C
C      CALLED BY:
C                 RATE
C      CALLS:
C             EBND,DUNLEV,PADLEV,EWKB,WKBPOT,PITZ
C
      DATA TOLER / 1.0D-9 /,NMAX / 3500 /
C
C     STATEMENT FUNCTION
C
      ESTR(WE,XE,V) = WE*(V+0.5D0)*(1.0D0-XE*(V+0.5D0))
C
C     IF SELECTED, THE VIBRATIONAL PARTITION FUNCTION IS GOING TO BE    0528JC97
C     CALCULATED CLASSICALLY (ALWAYS HARMONIC, NO CHECK OF ANHARMONICITY0528JC97
C     IS MADE).                                                         0528JC97
C                                                                       0528JC97
      IF (ICLASV.EQ.1) THEN                                             0528JC97
C                                                                       0528JC97
C     CHECK THE FREQUENCY IS NOT IMAGINARY                              0528JC97
C                                                                       0528JC97
            IF (W.LE.0.0D0) THEN                                        0528JC97
                  VPART=1.0D10                                          0528JC97
            ELSE                                                        0528JC97
                  VPART=BKT/W                                           0528JC97
            ENDIF                                                       0528JC97
            RETURN                                                      0528JC97
      ENDIF                                                             0528JC97
C
C     START PARTITION FUNCTION OFF WITH V=0. TERM
C
C23/7/GL91 VMAX = 1.D30                                                 7/19B91
      ISTATE = 0                                                        9/25BCG00
      IF (L0.GT.0) ISTATE = 1                                           9/25BCG00
      XSTATE = DBLE(ISTATE)                                             9/25BCG00
      VMXWKB = 1.D30                                                    7/19B91
      IF (LGS(33).EQ.1) THEN
         IF (IOP.EQ.0.OR.IOP.EQ.5) THEN
            E = EWKB0(IKBM,IKBS)
         ELSE
            E = WGSEX(IKBM)
         ENDIF
         GO TO 10
      ENDIF
      IF (LGS(5).LT.3) THEN
        IF (LGS(5).EQ.0) THEN                                           0819YC99
Cbcg          IF (IFRFAC.EQ.0) THEN                                         0808JC00
Cbcg             E = W*0.5d0                                                0819YC99
Cbcg          ELSE                                                          0808JC00
Cbcg             E = W*0.5d0*FREQFAC                                        0808JC00
Cbcg          ENDIF                                                         0808JC00
C
C   IF w < freqbottom cm-1, increase it to freqbottom cm-1
          IF (INCRF.EQ.1.AND.W.LT.FREQBOTTOM*CMTOAU.AND.W.GT.0.d0) 
     *       W=FREQBOTTOM*CMTOAU                                        1008JZ09
          E = W*(XSTATE+0.5d0)                                          9/25BCG00
          IF (IFRFAC.NE.0) E=E*FREQFAC                                  9/25BCG00
        ELSE                                                            0819YC99
          IF (XX.GE.-TOLER) THEN
C         E = ESTR(W,XX,0.0D0)                                          9/25BCG00
            E = ESTR(W,XX,XSTATE)                                       9/25BCG00
          ELSE
C         E = EBND(W,XX,0.0D0,REDM)                                     9/25BCG00
            E = EBND(W,XX,XSTATE,REDM)                                  9/25BCG00
          ENDIF
        ENDIF                                                           0819YC99
      ELSEIF (LGS(5).EQ.7) THEN
C      E = EWKB(XX,Y0,0,W,ELAST,REDM)                                   9/25BCG00
         E = EWKB(XX,Y0,ISTATE,W,ELAST,REDM)                            9/25BCG00
      ELSEIF (LGS(5).EQ.8) THEN
C      E = WKBPOT(XX,Y0,0,W)                                            9/25BCG00
         E = WKBPOT(XX,Y0,ISTATE,W)                                     9/25BCG00
      ENDIF
C
C     ZPE=0 IF HARMONIC OPTION IS CHOSEN AND THE FREQ. IS IMAGINARY
C
      IF (LGS(5).EQ.0.AND.W.LT.0.0D0) E = 0.0D0
   10 SUM = EXP(-E/BKT)
C
C     MODE IS RESTRICTED TO ground or excited STATE                     9/25BCG00
C
C      IF (L0.LT.0) THEN                                                9/25BCG00
      IF (L0.ne.0) THEN                                                 9/25BCG00
         VPART = SUM
         RETURN
      ENDIF
C23/7/GL91 IF (E.GE.VMAX) THEN                                          7/19B91
      IF (E.GE.VMXWKB) THEN                                             7/19B91
C  Energy is greater than maximum in potential (in WKBPOT) and E has     7/19B91
C23/7/GL9  been set to VMAX. No other energy levels possible            7/19B91
C  been set to VMXWKB. No other energy levels possible                  7/19B91
         VPART = SUM                                                     7/19B91
         RETURN                                                          7/19B91
C24/7/GL END                                                            7/19B91
      ENDIF                                                              7/19B91
C
C     SOLVE THE HARMONIC CASE (FOR THERMAL TREATMENT ONLY!)
C
C      IF (L0.EQ.0) THEN                                                9/25BCG00
      IF (LGS(5).EQ.0) THEN                                             9/25BCG00
C         IF (LGS(5).EQ.0.AND.LGS(33).NE.1) THEN                        9/25BCG00
         IF (LGS(33).NE.1) THEN                                         9/25BCG00
            IF (SUM.EQ.1.0D0) THEN
               VPART = 1.0D0
               IF (W.LT.0.0D0) VPART = 1.0D+10                          3/20DL91
            ELSE
               VPART = SUM/(1.D0-SUM*SUM)
            ENDIF
C            RETURN                                                     9/25BCG00
         ELSE                                                           9/25BCG00
C     Thermal treatment with WKB anharmonicity
C
C         ELSEIF (LGS(5).EQ.0.AND.LGS(33).EQ.1) THEN                    9/25BCG00
C
            IF (W.LE.0.0D0) THEN
c               VPART = SUM
               VPART = 10D+10                                           0825YC98
            ELSE
               OSUM = SUM
               E = 0.5D0*W
               SUM = EXP(-E/BKT)
               VPART = SUM*SUM*SUM/(1.0D0-SUM*SUM)
               VPART = VPART+OSUM
            ENDIF
C            RETURN                                                     9/25BCG00
         ENDIF
         RETURN                                                         9/25BCG00
      ENDIF
C
C     NOW COMPUTE THE PARTITION FUNCTION FOR THE ANHARMONIC OPTIONS
C
      ELST = E
      DO 20 I = 1, NMAX
         V = DBLE(I)
C
C         CALCULATE ENERGY LEVEL
C
         IF (LGS(5).LE.2) THEN
            IF (XX.GE.-TOLER) THEN
               E = ESTR(W,XX,V)
            ELSE
               E = EBND(W,XX,V,REDM)
            ENDIF
         ELSEIF (LGS(5).EQ.7) THEN
            E = EWKB(XX,Y0,I,W,ELAST,REDM)
         ELSEIF (LGS(5).EQ.8) THEN
            E = WKBPOT(XX,Y0,I,W)
         ENDIF
C
C         IF (E.GT.EMAX.OR.E.LE.ELST) THEN                               7/19B91
C23/7/GL91 IF (E.GT.EMAX.OR.E.GT.VMAX.OR.E.LE.ELST) THEN                7/19B91
         IF (E.GT.EMAX.OR.E.GT.VMXWKB.OR.E.LE.ELST) THEN                7/19B91
            IF (IOP.NE.0) THEN
               RELCON = TERM/SUM
               WRITE (FU6,1100) IOP,W*AUTOCM,RELCON                     0317Yc99
            ENDIF
            VPART = SUM
            RETURN
         ENDIF
         ELST = E
         TERM = EXP(-E/BKT)
         IF (L0.GT.0) THEN
C
C   MODE RESTRICTED TO FIRST EXCITED STATE
C
            SUM = TERM
            VPART = SUM
            RETURN
         ENDIF
         SUM = SUM+TERM
         IF (I.EQ.1) SUM1 = SUM
         IF ((TERM/SUM).LT.TOLER) THEN
            VPART = SUM
            RETURN
         ENDIF
   20 CONTINUE
      WRITE (FU6,1000) BKT,W,XX
      IF (IOP.NE.0) THEN
         RELCON = TERM/SUM
         WRITE (FU6,1100) IOP,W*AUTOCM,RELCON                           0317Yc99
      ENDIF
      VPART = SUM
      RETURN
C
 1000 FORMAT(1X,20(1H$),15H  FOR BKT,W,X =,3D20.10,5X,
     * 26HNMAX IN VPART IS TOO SMALL)
 1100 FORMAT(1X,'IOP = ',I2,' FOR FREQ WE = ',F10.3,' cm-1,',           0317Yc99
     * ' CONVERGENCE OF VPART = ',E12.6)                                0317Yc99
C
      END function vpart
C***********************************************************************
C  VSPLIN
C***********************************************************************
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C   MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C   FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C
      SUBROUTINE vsplin (LLGS,NSMAX,SX,SMAXX,VX,VMAXX,IMAXX,IMXINT)
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : iunit6,gufac6
C
C     CALLED BY:
C                 KAPVA
C     CALLS:
C           SPL1D1,SPL1B1,SPL1B2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION SCR(NSDM+1),IOP(2),SX(NSMAX),VX(NSMAX),ROOT(2),TAB(3)
      DIMENSION LLGS(39)
      save                                                              0601YC98
      IERRCN = 0
      NSPL = NSMAX
C
C     GENERATE SPLINE FITS TO VA AND CH
C
      IOP(1) = 5
      IOP(2) = 5
      SCR(NSPL) = 0.0D0
      IJ = 1                                                            1215YL91
      CALL SPL1D1 (NSPL,SX,VX,SCR,IOP,IJ,ASPL,BSPL,CSPL)                1215YL91
      CALL SPL1B1 (NSPL,SX,VX,SCR,IJ,ASPL,BSPL,CSPL,DSPL)               1215YL91
C
C     FIND SPLINE FIT MAX OF VAD
C     FIRST FIND LARGEST V VALUE
C
      VMAXX = 0.0D0
      DO 10 I = 1, NSPL
         IF (VX(I).LT.VMAXX) GO TO 10
         VMAXX = VX(I)
         IMAXX = I
   10 CONTINUE
      IF (IMAXX.EQ.1.OR.IMAXX.EQ.NSPL) IERRCN = 1
      IF (IERRCN.NE.0) THEN
         IF (LLGS(22).EQ.0) THEN
           WRITE (FU6,*) ' MAXIMUM OF ADIABATIC BARRIER AT EDGE OF GRID'
           STOP 'VSPLIN 1'
         ENDIF
         VMAXX = VX(IMAXX)
         SMAXX = SX(IMAXX)
      ELSE
         IM = IMAXX-1
         SL = SX(IM)                                                    1112WH92
         SR = SX(IM+2)                                                  1112WH92
C
C     FIND SMAXX FROM SPLINE FIT COEFFICIENTS
C     LOOK ON BOTH SIDES OF LARGEST V AND CHOOSE LARGEST VMAXX
C
         IMXINT = 0                                                     8/16B91
         VMAXX = 0.0D0
         DO 30 ISIDE = 1, 2
            ARG = BSPL(IM)*BSPL(IM)-3.0D0*ASPL(IM)*CSPL(IM)
            IF (ARG.GE.0.0D0) THEN
               DET = SQRT(ARG)
               ROOT(1) = (-BSPL(IM)+DET)/(3.0D0*ASPL(IM))
               ROOT(2) = (-BSPL(IM)-DET)/(3.0D0*ASPL(IM))
               DO 20 IROOT = 1, 2
                  IF (ROOT(IROOT).LE.SR.AND.ROOT(IROOT).GE.SL) THEN     1112WH92
                    CALL SPL1B2 (NSPL,SX,ASPL,BSPL,
     *                           CSPL,DSPL,ROOT(IROOT),TAB,0)
                     IF (TAB(1).GT.VMAXX) THEN
                        VMAXX = TAB(1)
                        SMAXX = ROOT(IROOT)
                        IMXINT = IM                                     8/16B91
                     ENDIF
                  ENDIF
   20          CONTINUE
            ENDIF
            IM = IM+1
   30    CONTINUE
         IF (IMXINT.EQ.0) THEN                                          8/16B91
            WRITE (FU6,1000)                                            1112WH92
            VMAXX = VX(IMAXX)                                           ..
            SMAXX = SX(IMAXX)                                           ..
            IMXINT= IMAXX                                               1112WH92
         ENDIF
              IF (LGS(23) .GT. 0) THEN                                  0430GL92
C                 WRITE (FU6,1200) VMAXX,VMAXX*CKCAL,SMAXX              0430GL92
               IF(IUNIT6.EQ.1) 
     *            WRITE (FU6,1200) VMAXX,VMAXX*CKCAL,SMAXX/GUFAC6       0405JZ07
               IF(IUNIT6.EQ.0)  
     *            WRITE (FU6,1210) VMAXX,VMAXX*CKCAL,SMAXX/GUFAC6       0405JZ07
              ELSE                                                      0430GL92
C                 WRITE (FU6,1100) VMAXX,VMAXX*CKCAL,SMAXX              0430GL92
               IF(IUNIT6.EQ.1)  
     *            WRITE (FU6,1100) VMAXX,VMAXX*CKCAL,SMAXX/GUFAC6       0405JZ07
               IF(IUNIT6.EQ.0)
     *            WRITE (FU6,1110) VMAXX,VMAXX*CKCAL,SMAXX/GUFAC6       0405JZ07
              ENDIF                                                     0430GL92
      ENDIF
C
      RETURN
C
 1000 FORMAT(1X,'NOTE: The adiabatic maximum has been set to ',         1112WH92
     *      /1X,'      the largest save point value.')
 1100 FORMAT(/1X,'From spline fit the maximum of Va^G =',1P,E13.5,      06/96ELC
     1       ' hartrees (',0P,F9.4,' kcal/mol)',
     2       /1X,'where the reaction coordinate s =',F12.6,' bohr.')    0616WH94
 1110 FORMAT(/1X,'From spline fit the maximum of Va^G =',1P,E13.5,      0405JZ07
     1       ' hartrees (',0P,F9.4,' kcal/mol)',                        
     2       /1X,'where the reaction coordinate s =',F12.6,' angstrom.')
 1200 FORMAT(/1X,'From spline fit the maximum of Va = ',1P,E13.5,        ..
     1       ' hartrees (',0P,F9.4,' kcal/mol)',                         ..
     2       /1X,'where the reaction coordinate s =',F12.6,' bohr.')     ..
 1210 FORMAT(/1X,'From spline fit the maximum of Va = ',1P,E13.5,       0405JZ07
     1       ' hartrees (',0P,F9.4,' kcal/mol)',                        
     2       /1X,'where the reaction coordinate s =',F12.6,' angstrom.')
C
      END SUBROUTINE vsplin
C
C***********************************************************************
C  VTMUSN
C***********************************************************************
C
      SUBROUTINE vtmusn (EMU,SVTMU3,VTMUN3,SVTMU5,VTMUN5,CVTS,GTNCVT,
     * SMIN15,SMAX15,SMIN25,VMIN15,VMAX15,VMIN25,VTUSN,LUSC,GTNS,
     * NGR,DELE,ISMMVT,ISPMVT,IFTMV1,IFTMV2,VAGMU)  

      use common_inc
      use perconparam, only : nvibm,nsdm,fu6,ckcal
      use rate_const
      use sst
C    
C     THIS SUBROUTINE CALCULATES NMUVT(E) USED IN THE
C     CALCULATION OF KMUVT.
C
C     Modified by BCG August 1985 to enable calculations for systems
C     with more than two modes.  The algorithm is identical to that
C     written by Rozeanne for the ICVT calculation (see subroutine
C     STAUV.)
C
C     CALLED BY:
C                RATE
C     CALLS:
C            EVIB,TREPT,FIVPT
C
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
C
      DIMENSION NVIB(NVIBM),ENVIB(NVIBM),EZEROX(NVIBM),EONE(NVIBM)
      DIMENSION GTNS(NGR,NSDM),GTNMU(NSDM)
      DIMENSION XPT(5),GPT(5)
C
C     ENZERO IS THE TOTAL VIBRATIONAL ENERGY WHEN ALL MODES
C     ARE IN NV=0 STATE
C
      IF (ICODE(5) .LT. 0) THEN                                         11/20T87
         N3M7 = N3 - 1                                                    ..
      ELSE IF (ICODE(5).EQ.3) THEN                                         ..
         N3M7 = N3 - 6                                                    ..
      ELSE                                                                 ..
         N3M7 = N3 - 7                                                    ..
      ENDIF                                                             11/20T87
      AFLAG = '   '
      IF (LGS(5).EQ.21) AFLAG = 'SET'
      IMIN = ISMMVT                                                     01/13B92
      INDEX = 0
      DO 60 IS = ISMMVT,ISPMVT                                          01/13B92
C
C Set up list of vibrational quantum numbers and zero-point energies
C    for the different modes
C
         ESUM = 0.D0
         DO 10 I = 1, N3M7
            IF (AFLAG.EQ.'SET') LGS(5) = MODE(I)
            IKBM = I
            L0=LN3(JSWITC,I)                                            10/11BG00
            E = EVIB(WETS(I,IS),XETS(I,IS),0,Y0TS(I,IS),IS)
            EZEROX(I) = E
            IKBM = I
            E = EVIB(WETS(I,IS),XETS(I,IS),1,Y0TS(I,IS),IS)
            EONE(I) = E
            IF (LN3(JSWITC,I).EQ.1) THEN
               ENVIB(I) = EONE(I)
               NVIB(I) = 1
            ELSE
               ENVIB(I) = EZEROX(I)
               NVIB(I) = 0
               IF (LN3(JSWITC,I).EQ.0.AND.IBEG.EQ.0) IBEG = I
            ENDIF
            ESUM = ESUM+ENVIB(I)
C
   10    CONTINUE
         EMAX = EMU - ESUM - VCLAS(IS)
       
         IF(LSST .EQ. 0 ) THEN
           IF(EMAX. GT. 0.d0) THEN
             N = INT(EMAX/DELE) 
             GTNMU(IS) = GTNS(N,IS)
           ELSE
             GTNMU(IS) = 1.d0
           ENDIF
         ELSE
           IF(EMAX. GT. 0.d0) THEN
           CALL invlaplace(EMAX,WETS(:,IS),NVIBM,FMITS(IS),IS,
     *                     GTNMU(IS),DEN) 
           ELSE
             GTNMU(IS) = 1.d0
           ENDIF
         ENDIF
C     
C Redefine left and right edges of grid to be first and last points,
C    respectively, at which N**GT(E,s) < 10**30
C
         IF (INDEX.EQ.0) THEN
            IF (GTNMU(IS).LT.1.0D+30) THEN
               INDEX = 1
               ISF = IS
            ENDIF
         ELSEIF (INDEX.EQ.1) THEN
            IF (GTNMU(IS).LT.1.0D+30) ISL = IS
         ENDIF
C check for absolute minimum in NGT(s) curve                            01/13B92
         IF (GTNMU(IS).LT.GTNMU(IMIN)) IMIN = IS                        01/13B92
C        IF(GTNMU(IMIN).LT.1D0) IMIN = IS      
C        IF (GTNMU(IS).LT.GTNMU(IMIN).AND.GTNMU(IS).GT.1D0) IMIN = IS   0101JZ13
60    CONTINUE

C Debug purpose
C     write(99,'(a,2f10.3)') 'ENERGY = ',EMU*CKCAL,(EMU-VAGMU)*CKCAL
C     write(99,'(2X,a,8X,a,10X,a)') 's (A)','N','log(N)'
C     DO IS = ISMMVT,ISPMVT
C        IF(SSUBI(IS).EQ.0d0) THEN
C          ITST = IS
C          goto 64 
C        ENDIF
C     ENDDO
C64   CONTINUE
C     DO 65 IS = ISMMVT,ISPMVT 
C      write(99,'(f8.3,es14.6,2f10.3)') SSUBI(IS)*0.529177249,GTNMU(IS),
C    *                  log10(GTNMU(IS)),GTNMU(IS)/GTNMU(ITST)
C65   CONTINUE
C end of debug printing

C
      IF (LUSC.EQ.0) THEN
C
C  Doing muvt claculation only (no US), fit absolute minimum only
C
c        IMIN = MAX(IMIN,ISF)
C        IF (LGS(22).EQ.0.AND.(IMIN.LE.ISF.OR.IMIN.GE.ISL)) THEN
         IF (IMIN.LE.ISMMVT+2.OR.IMIN.GE.ISPMVT-2) THEN
            SVTMU3 = SSUBI(IMIN)
            SVTMU5 = SVTMU3
            VTMUN3 = GTNMU(IMIN)
            VTMUN5 = VTMUN3
            WRITE (FU6,1000)
         ELSE
            IF (IFTMV1.EQ.1) THEN                                       01/13B92
               SVTMU5 = SSUBI(IMIN)                                     01/13B92
               VTMUN5 = GTNMU(IMIN)                                     01/13B92
            ELSE IF (IFTMV1.EQ.3) THEN                                  01/13B92
C
C  three-point fit
C
               IMIN = MAX(IMIN,ISF+1)
               IMIN = MIN(IMIN,ISL-1)
               J = IMIN-2
               DO 70 I = 1, 3                                           01/13B92
                  XPT(I) = SSUBI(J+I)
                  GPT(I) = GTNMU(J+I)
70                CONTINUE                                              01/13B92
               CALL TREPT (1,XPT,GPT,SMIN,VMIN)
               SVTMU5 = SMIN                                            01/13B92
               VTMUN5 = VMIN                                            01/13B92
            ELSE IF (IFTMV1.EQ.5) THEN                                  01/13B92
C
C  five-point fit.
C
               IMIN = MAX(IMIN,ISF+2)
               IMIN = MIN(IMIN,ISL-2)
               J = IMIN-3
               DO 75 I = 1, 5                                           01/13B92
                  XPT(I) = SSUBI(J+I)
                  GPT(I) = GTNMU(J+I)
75             CONTINUE                                                 01/13B92
               SMIN = SSUBI(IMIN)                                       01/13B92
               CALL FIVPT (1,0,XPT,GPT,SMIN,VMIN)
               SVTMU5 = SMIN                                            01/13B92
               VTMUN5 = VMIN                                            01/13B92
            END IF
C  now compute second fits for checks                                   01/13B92
            IF (IFTMV2.EQ.1) THEN                                       01/13B92
               SVTMU3 = SSUBI(IMIN)                                     01/13B92
               VTMUN3 = GTNMU(IMIN)                                       01/13B92
            ELSE IF (IFTMV2.EQ.3) THEN                                  01/13B92
C  three-point fit
               IMIN = MAX(IMIN,ISF+1)                                   01/13B92
               IMIN = MIN(IMIN,ISL-1)                                   01/13B92
               J = IMIN-2                                               01/13B92
               DO 95 I = 1, 3                                           01/13B92
                  XPT(I) = SSUBI(J+I)                                   01/13B92
                  GPT(I) = GTNMU(J+I)                                     01/13B92
95             CONTINUE                                                 01/13B92
               CALL TREPT (1,XPT,GPT,SMIN,VMIN)                         01/13B92
               SVTMU3 = SMIN                                            01/13B92
               VTMUN3 = VMIN                                            01/13B92
            ELSE IF (IFTMV2.EQ.5) THEN                                  01/13B92
C  five-point fit. 
               IMIN = MAX(IMIN,ISF+2)                                   01/13B92
               IMIN = MIN(IMIN,ISL-2)                                   01/13B92
               J = IMIN-3                                               01/13B92
               DO 96 I = 1, 5                                           01/13B92
                  XPT(I) = SSUBI(J+I)                                   01/13B92
                  GPT(I) = GTNMU(J+I)                                     01/13B92
96             CONTINUE                                                 01/13B92
               SMIN = SSUBI(IMIN)                                       01/13B92
               CALL FIVPT (1,0,XPT,GPT,SMIN,VMIN)                       01/13B92
               SVTMU3 = SMIN                                            01/13B92
               VTMUN3 = VMIN                                            01/13B92
            END IF                                                      01/13B92
         ENDIF                                                          01/13B92
      ELSE                                                              01/13B92
C 
C Doing US calculation, need two lowest minima and highest maximum      01/13B92
C    between them.                                                      01/13B92
         INDEX = 0
         IMIN1 = 0
         IMIN2 = 0
         DO 100 I = ISF,ISL                                             01/13B92
            IF (INDEX.EQ.0) THEN                                        01/13B92
C
C  looking for a minimum
               IF (I.EQ.ISL.OR.GTNMU(I).LT.GTNMU(I+1)) THEN
                  IF (IMIN1.EQ.0) THEN
C
C  first minimum found
                     IMIN1 = I
                  ELSEIF (IMIN2.EQ.0) THEN
C
C  second minimum found
                     IMIN2 = I
                  ELSEIF (GTNMU(I).LT.GTNMU(IMINMX)) THEN
C
C  nth minimum found (n>2) and it is smaller than at least one of those
C     previously found
                     IMIN1 = IMINMN
                     IMIN2 = I
                  ENDIF
C
C  reset index to now look for a maximum
                  INDEX = 1
                  IF (IMIN2.NE.0) THEN
C
C  if at least two minimum were found figure out which is the smallest
C     and largest of the two smallest
                     IF (GTNMU(IMIN1).LE.GTNMU(IMIN2)) THEN
                        IMINMN = IMIN1
                        IMINMX = IMIN2
                     ELSE
                        IMINMN = IMIN2
                        IMINMX = IMIN1
                     ENDIF
                  ENDIF
               ENDIF
C
C  looking for a maximum
            ELSEIF (I.LT.ISL.AND.GTNMU(I).GT.GTNMU(I+1)) THEN
C
C  maximum found
C  reset index to now look for a minimum
               INDEX = 0
            ENDIF
100      CONTINUE                                                       01/13B92
C
C  Now find maximum between the two minima                              01/13B92
         IMAX = 0                                                       01/13B92
         IF (IMIN2.NE.0) THEN                                           01/13B92
            IMAX = IMIN1                                                01/13B92
            DO 110 I = IMIN1+1,IMIN2                                    01/13B92
               IF(GTNMU(I).GT.GTNMU(IMAX)) IMAX = I                         01/13B92
110         CONTINUE                                                    01/13B92
         END IF                                                         01/13B92
C
C  Fit lowest two minima
C
         IMIN1 = MAX(IMIN1,ISF)
         IF (LGS(22).EQ.0.AND.(IMIN1.LE.ISF.OR.IMIN1.GE.ISL)) THEN
            SMIN15 = SSUBI(IMIN1)                                       01/13B92
            VMIN15 = GTNMU(IMIN1)                                         01/13B92
            WRITE (FU6,1100)
         ELSE IF (IFTMV1.EQ.1) THEN                                     01/13B92
            SMIN15 = SSUBI(IMIN1)                                       01/13B92
            VMIN15 = GTNMU(IMIN1)                                         01/13B92
         ELSE IF (IFTMV1.EQ.3) THEN                                     01/13B92
C
C  three-point fit
C
            IMIN1 = MAX(IMIN1,ISF+1)
            IMIN1 = MIN(IMIN1,ISL-1)
            J = IMIN1-2
            DO 120 I = 1, 3                                             01/13B92
               XPT(I) = SSUBI(J+I)
               GPT(I) = GTNMU(J+I)
  120       CONTINUE                                                    01/13B92
            CALL TREPT (1,XPT,GPT,SMIN,VMIN)
            SMIN15 = SMIN
            VMIN15 = VMIN
         ELSE IF (IFTMV1.EQ.5) THEN                                     01/13B92
C
C  five-point fit.
C
            IMIN1 = MAX(IMIN1,ISF+2)
            IMIN1 = MIN(IMIN1,ISL-2)
            J = IMIN1-3
            DO 125 I = 1, 5                                             01/13B92
               XPT(I) = SSUBI(J+I)
               GPT(I) = GTNMU(J+I)
125         CONTINUE                                                    01/13B92
            SMIN = SSUBI(IMIN1)                                         01/13B92
            CALL FIVPT (1,0,XPT,GPT,SMIN,VMIN)
            SMIN15 = SMIN
            VMIN15 = VMIN
         ENDIF
C
C  Second minimum
C
         IF (IMIN2.EQ.0) THEN
            VMIN25 = VMIN15
            SMIN25 = SMIN15
            VMAX15 = VMIN15
            SMAX15 = SMIN15
         ELSE
            IF (LGS(22).EQ.0.AND.(IMIN2.LE.ISF.OR.IMIN2.GE.ISL)) THEN
               SMIN25 = SSUBI(IMIN2)                                    01/13B92
               VMIN25 = GTNMU(IMIN2)                                      01/13B92
               WRITE (FU6,1200)
            ELSE IF (IFTMV1.EQ.1) THEN                                  01/13B92
               SMIN25 = SSUBI(IMIN2)                                    01/13B92
               VMIN25 = GTNMU(IMIN2)                                      01/13B92
            ELSE IF (IFTMV1.EQ.3) THEN                                  01/13B92
C
C  three-point fit
C
               IMIN2 = MAX(IMIN2,ISF+1)
               IMIN2 = MIN(IMIN2,ISL-1)
               J = IMIN2-2
               DO 130 I = 1, 3                                          01/13B92
                  XPT(I) = SSUBI(J+I)
                  GPT(I) = GTNMU(J+I)
  130          CONTINUE                                                 01/13B92
               CALL TREPT (1,XPT,GPT,SMIN,VMIN)
               SMIN25 = SMIN
               VMIN25 = VMIN
            ELSE IF (IFTMV1.EQ.5) THEN                                  01/13B92
C
C  five-point fit.
C
               IMIN2 = MAX(IMIN2,ISF+2)
               IMIN2 = MIN(IMIN2,ISL-2)
               J = IMIN2-3
               DO 135 I = 1, 5                                          01/13B92
                  XPT(I) = SSUBI(J+I)
                  GPT(I) = GTNMU(J+I)
135            CONTINUE                                                 01/13B92
               SMIN = SSUBI(IMIN2)                                      01/13B92
               CALL FIVPT (1,0,XPT,GPT,SMIN,VMIN)
               SMIN25 = SMIN
               VMIN25 = VMIN
            ENDIF
C
C  Maximum
C
            IF (LGS(22).EQ.0.AND.(IMAX.LE.ISF.OR.IMAX.GE.ISL)) THEN     01/13B92
               SMAX15 = SSUBI(IMAX)                                     01/13B92
               VMAX15 = GTNMU(IMAX)                                       01/13B92
               WRITE (FU6,1300)
            ELSE IF (IFTMV1.EQ.1) THEN                                  01/13B92
               SMAX15 = SSUBI(IMAX)                                     01/13B92
               VMAX15 = GTNMU(IMAX)                                       01/13B92
            ELSE IF (IFTMV1.EQ.3) THEN                                  01/13B92
C
C  three-point fit
C
               IMAX = MAX(IMAX,ISF+1)                                   01/13B92
               IMAX = MIN(IMAX,ISL-1)                                   01/13B92
               J = IMAX-2                                               01/13B92
               DO 140 I = 1, 3                                          01/13B92
                  XPT(I) = SSUBI(J+I)
                  GPT(I) = GTNMU(J+I)
  140          CONTINUE
               CALL TREPT (1,XPT,GPT,SMAXX,VMAXX)
               SMAX15 = SMAXX
               VMAX15 = VMAXX
            ELSE IF (IFTMV1.EQ.5) THEN                                  01/13B92
C
C  five-point fit.
C
               IMAX = MAX(IMAX,ISF+2)                                   01/13B92
               IMAX = MIN(IMAX,ISL-2)                                   01/13B92
               J = IMAX-3                                               01/13B92
               DO 150 I = 1, 5                                          01/13B92
                  XPT(I) = SSUBI(J+I)
                  GPT(I) = GTNMU(J+I)
  150          CONTINUE                                                 01/13B92
               SMAXX = SSUBI(IMAX)                                      01/13B92
               CALL FIVPT (1,0,XPT,GPT,SMAXX,VMAXX)
               SMAX15 = SMAXX
               VMAX15 = VMAXX
            ENDIF
         ENDIF
C
         IF (VMIN15.LT.VMIN25) THEN
            VTMUN5 = VMIN15
            VTMIN5 = VMIN25
            SVTMU5 = SMIN15
         ELSE
            VTMUN5 = VMIN25
            VTMIN5 = VMIN15
            SVTMU5 = SMIN25
         ENDIF
         VTUSN = VTMUN5*VTMIN5*VMAX15
         VTUSN = VTUSN/((VTMUN5+VTMIN5)*VMAX15-(VTMUN5*VTMIN5))
      ENDIF
C
C  find NGT(E,S*CVT) using IFTMV1-point fit
C
      ICVTS = 0
      DO 160 I = 1, LSAVE
         IF (SSUBI(I).GT.CVTS) GO TO 160
         ICVTS = I
  160 CONTINUE
      IF (LGS(22).EQ.0.AND.(ICVTS.LE.ISF.OR.ICVTS.GE.ISL)) THEN
C     IF (ICVTS.LE.ISF.OR.ICVTS.GE.ISL) THEN
C     IF (ICVTS.LE.3.OR.ICVTS.GE.LSAVE-3) THEN
         GTNCVT = GTNMU(ICVTS)
      ELSE IF (IFTMV1.EQ.1) THEN                                        01/13B92
         GTNCVT = GTNMU(ICVTS)                                            01/13B92
      ELSE IF (IFTMV1.EQ.3) THEN                                        01/13B92
C  three-point fit.                                                     01/13B92
C                                                                       01/13B92
         ICVTS = MAX(ICVTS,ISF+1)                                       01/13B92
         ICVTS = MIN(ICVTS,ISL-1)                                       01/13B92
         J = ICVTS-2                                                    01/13B92
         DO 170 I = 1, 3                                                01/13B92
            XPT(I) = SSUBI(J+I)                                         01/13B92
            GPT(I) = GTNMU(J+I)                                         01/13B92
  170    CONTINUE                                                       01/13B92
         CALL TREPT (0,XPT,GPT,CVTS,GTNCVT)                             5/01YL91
      ELSE IF (IFTMV1.EQ.5) THEN                                        01/13B92
C
C  five-point fit. Uses three-point fit xmin as guess for min
C
         ICVTS = MAX(ICVTS,ISF+2)
         ICVTS = MIN(ICVTS,ISL-2)
         J = ICVTS-3
         DO 175 I = 1, 5                                                01/13B92
            XPT(I) = SSUBI(J+I)
            GPT(I) = GTNMU(J+I)
175      CONTINUE                                                       01/13B92
         CVTS = SSUBI(ICVTS)                                            01/13B92
         CALL FIVPT (0,0,XPT,GPT,CVTS,GTNCVT)
      ENDIF
C
200   CONTINUE
      IF (AFLAG.EQ.'SET') LGS(5) = 21
      RETURN
C
 1000 FORMAT (39H *** MINIMUM GTNMU IS NEAR ENDPOINT ***)
 1100 FORMAT(1X,41H*** FIRST MIN IN NGT IS NEAR ENDPOINT ***)
 1200 FORMAT (43H *** SECOND MIN IN NGT IS NEAR ENDPOINT ***)
 1300 FORMAT (36H *** MAX IN NGT IS NEAR ENDPOINT ***)
C
      END SUBROUTINE vtmusn
C
C***********************************************************************
C  WKBENE
C***********************************************************************
C
      SUBROUTINE wkbene(DEDNRC,ENRC,LGS212,LGS217,NSTATE,NBOUND,IQRNSQ, 0522TA02
     *                  REDM,SMAX,SRW,SSUBI,VADIB,WR)
      use perconparam
C
C     Finding the WKB eigenstates of the mode corresponding to
C     the reaction coordinate in a unimolecular reaction
C
C     Written by Wei-Ping Hu  03/30/94
C
C     Calls: MXLNEQ
C
C     Called by: BOLTZ
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION ENRC(0:MAXWKB),DEDNRC(0:MAXWKB),SSUBI(NSDM),VADIB(NSDM)
      DIMENSION FUNCT(0:NSDM),PHAINT(NSDM),ENECUR(0:NSDM)
      DIMENSION TF(3),TG(3),TX(3),SINTVL(NSDM)
      DIMENSION AB(3,4),iscr(3)
C
C     Find the index corresponding to the reactant well
C
      IF (SRW .LT. SSUBI(1)) THEN
         WRITE(FU6,1000)
         STOP 'WKBENE 1'
      ENDIF
C
      ISRW = 0
10    ISRW = ISRW + 1
      IF (SSUBI(ISRW) .LE. SRW) GOTO 10
      ABS1 = ABS(SSUBI(ISRW-1) - SRW)
      ABS2 = ABS(SSUBI(ISRW) -SRW)
      IF (ABS2 .GT. ABS1) ISRW = ISRW - 1
      VAGZRO = VADIB(ISRW)
C
C     Find the index that are closest to the maximum of VaG on the reactant side
C
      ISMAX = 0
20    ISMAX = ISMAX + 1
      IF (SSUBI(ISMAX) .LE. SMAX) GOTO 20
      ISMAX = ISMAX - 1
C
C     Determine number of intervals from VaG max to reactant well
C
      N1 = ISMAX - ISRW
C
C     Set up the energy curve from the reactant to VaG max
C
      DO 50 I = ISRW,ISMAX
         ENECUR(I-ISRW) = VADIB(I) - VAGZRO
         SINTVL(I-ISRW) = SSUBI(I+1) - SSUBI(I)                         1118PF97
50    CONTINUE
C
C     Perform phase integral at every grip point
C
C     Calculate the right half by Simpson's rule
C     Calculate the left half by analytic integration
C
      DO 100 I = 1, N1
         N = 2 * I
C
C     Calculate the right half
C
         EN = ENECUR(I)
         DINTVL = SINTVL(I)                                             1118PF97
         DO 80 J = 0, N
            JJ = ABS(J-I)
            FUNCT(J) = MAX(0.0D0,(EN - ENECUR(JJ))) ** 0.5D0
80       CONTINUE
         CALL SIMPSN(N,FUNCT,DINTVL,RESULT)                             1118PF97
         PHAINT(I) = RESULT / 2.0D0 * (2.0D0*REDM) ** 0.5D0             09/95KAN
C
C     Calculate the left half harmonically and add them together
C
         IF (EN .GT. 0.0D0) THEN
            RESULT = EN / WR * PI / 2.0D0
         ELSE
            RESULT = 0.0D0
         ENDIF
C
C     If VaG is supposed to be symmetric with respect to reactant,
C     just double the right half result
C
         IF (LGS212 .EQ. 3) THEN
            PHAINT(I) = 2.0D0 * PHAINT(I)
         ELSE
            PHAINT(I) = PHAINT(I) + RESULT
         ENDIF
100   CONTINUE
C
C     Now the phase integral are calculated. Find the eigenstates using
C     three point fit to solve theta(En) = (n+1/2)pi
C
C     Find how many states are open below the VaG max. n=0 is the ground state
C
C      NSTATE = INT(PHAINT(N1) / PI - 0.5D0)
C
C      IF (NSTATE .GT. MAXWKB) WRITE(FU6,1050)                          0423TA02
C
C     Find how many states are open below the VaG max. n=0 is the ground state
C
      NSTAOP = INT(PHAINT(N1) / PI - 0.5D0)                             0423TA02
C
C     Determine the number of states to be included in the calculation
C
      IF (LGS217 .GE. 0 .AND. LGS217 .LT. NSTAOP) THEN                  0423TA02
         NSTATE = LGS217                                                0423TA02
      ELSE                                                              0423TA02
         NSTATE = NSTAOP                                                0423TA02
      ENDIF                                                             0423TA02
C
C     Determine the number of states used for vibrational partition
C     function at the reactant for this quantized coordinate.
C
      IF (IQRNSQ .EQ. -1) THEN                                          0522TA02
         NBOUND = NSTAOP                                                0522TA02
      ELSEIF (IQRNSQ .EQ. -2) THEN                                      0522TA02
         NBOUND = NSTATE                                                0522TA02
      ENDIF                                                             0522TA02
C
C     Determine the number of states that should be considered in the
C     calculation. This number should be equal to NSTAOP if all the 
C     open states are included in the calculation. However, if only
C     LGS217 states are included in the calculation, one still should
C     considered the energy of LGS217+1 states as DEDNRC(LGS217) will
C     be obtained through a three point fit that depend on 
C     ENRC(LGS217+1). More over, DEDNRC(0) depends on ENRC(0), ENRC(1),
C     and ENRC(2), if NSTAOP is greater than or equal to 2.
C
      IF (LGS217 .EQ. 0 .AND. NSTAOP .GE. 2) THEN                       0423TA02
         NSTACO =  2                                                    0423TA02
      ELSEIF (LGS217 .GT. 0 .AND. LGS217 .LT. NSTAOP) THEN              0423TA02
         NSTACO = LGS217 + 1                                            0423TA02
      ELSE                                                              0423TA02
         NSTACO = NSTAOP                                                0423TA02
      ENDIF                                                             0423TA02
C
C     Check if the number of states considered is smaller than the 
C     maximum allowed
C
      IF (NSTACO .GT. MAXWKB) THEN                                      0423TA02
         WRITE(FU6,1060) NSTACO,MAXWKB                                  0423TA02
         STOP 'WKBENE 1.5'                                              0423TA02
      ENDIF                                                             0423TA02
 1060 FORMAT(/1X,'ERROR: NUMBER OF WKB STATES CONSIDERED ',i3,          0423TA02
     *           ' EXCEED MAXWKB',i3,'.',                               0423TA02
     *       /1X,'INCREASE MAXWKB IN PARAM.INC AND RECOMPILE.')         0423TA02
C
C     Loop around the open eigenstates
C
      I = 0
C
      DO 300 N = 0, NSTACO
         VAL = (DBLE(N) + 0.5D0) * PI
C
C     Find the grid point just above the eigenstate
C
150      I = I + 1
         IF (I .GT. N1) THEN
            STOP 'WKBENE 2'
         ELSE
            IF (PHAINT(I) .LT. VAL) GOTO 150
         ENDIF
C
C     Set up the three point fit information
C
         IF (I .EQ. 1) THEN
            DO 160 J = 1,3
               TF(J) = PHAINT(J)
               TX(J) = ENECUR(J)
160         CONTINUE
         ELSEIF (I .EQ. N1) THEN
            DO 170 J = 1,3
               TF(J) = PHAINT(I-3+J)
               TX(J) = ENECUR(I-3+J)
170         CONTINUE
         ELSE
            DO 180 J = 1,3
               TF(J) = PHAINT(I-2+J)
               TX(J) = ENECUR(I-2+J)
180         CONTINUE
         ENDIF
C
         DO 200 J = 1,3
            AB(J,1) = TX(J)*TX(J)
            AB(J,2) = TX(J)
            AB(J,3) = 1.0D0
            AB(J,4) = TF(J)
200      CONTINUE
C
C     Calculate the quadratic coefficients
C
         CALL MXLNEQ(AB,3,3,DET,JRANK,EPS,ISCR,-1,4)
         IF (JRANK .LT. 3) STOP 'WKBENE 3'
         A = AB(1,4)
         B = AB(2,4)
         C = AB(3,4)
C
C     Solve the quadratic equation to get the eigenenergy
C
         CNEW = C - VAL
         DQ = B * B - 4.0D0 * A * CNEW
         IF (DQ .LT. 0) THEN
            WRITE(FU6,1200)
            STOP 'WKBENE 4'
         ENDIF
         ROOT1 = (-B + DQ ** 0.5D0) / 2.0D0 / A
         ROOT2 = (-B - DQ ** 0.5D0) / 2.0D0 / A
C
C     Find the root closest to the middle point
C
         ABS1 = ABS(ROOT1 - TX(2))
         ABS2 = ABS(ROOT2 - TX(2))
         IF (ABS1 .LE. ABS2) THEN
            ENRC(N) = ROOT1
         ELSE
            ENRC(N) = ROOT2
         ENDIF
300   CONTINUE
C
C     Use three point fit to get dE/dn at the eigenenergies
C
C      IF (NSTATE .EQ. 0) THEN
      IF (NSTAOP .EQ. 0) THEN                                           0423TA02
         DEDNRC(0) = WR
C      ELSEIF (NSTATE .EQ. 1) THEN
      ELSEIF (NSTAOP .EQ. 1) THEN                                       0423TA02
         DEDNRC(0) = WR
         DEDNRC(1) = ENRC(1) - ENRC(0)
      ELSE
         DO 500 N = 0, NSTATE
            IF (N .EQ. 0) THEN
               DO 400 J = 1,3
                  TX(J) = DBLE(J-1)
                  TG(J) = ENRC(J-1)
400            CONTINUE
C            ELSEIF (N .EQ. NSTATE)  THEN
            ELSEIF (N .EQ. NSTAOP)  THEN                                0423TA02
               DO 410 J = 1,3
                  TX(J) = DBLE(N-3+J)
                  TG(J) = ENRC(N-3+J)
410            CONTINUE
            ELSE
               DO 420 J = 1,3
                  TX(J) = DBLE(N-2+J)
                  TG(J) = ENRC(N-2+J)
420            CONTINUE
            ENDIF
            DO 450 J = 1,3
               AB(J,1) = TX(J)*TX(J)
               AB(J,2) = TX(J)
               AB(J,3) = 1.0D0
               AB(J,4) = TG(J)
450         CONTINUE
            CALL MXLNEQ(AB,3,3,DET,JRANK,EPS,ISCR,-1,4)
            IF (JRANK .LT. 3) STOP 'WKBENE 5'
            A = AB(1,4)
            B = AB(2,4)
            C = AB(3,4)
            DEDNRC(N) = 2.0D0 * A * DBLE(N) + B
500      CONTINUE
C
      ENDIF
C
      RETURN
C
1000  FORMAT(/1X,'THE MEP WAS NOT FOLLOWED LONG ENOUGH TO REACH ',
     *           'THE REACTANT WELL.')
C1050  FORMAT(/1X,'WARNING: NUMBER OF OPEN WKB STATES EXCEED MAXWKB')
1100  FORMAT(/1X,'ERROR IN FINDING THE WKB EIGENSTATES')
1200  FORMAT(/1X,'ERROR IN SOLVING THE QUADRATIC EQUATION IN WKBENE')
C
      END SUBROUTINE wkbene
C
C***********************************************************************
C  WKBPOT
C***********************************************************************
C
      FUNCTION wkbpot (A,B,N,WE)
      use perconparam, only : fu6,pi,tpi
      use rate_const, only : wets
      use common_inc, only : redm
C
C     This function uses a semiclassical method with parabolic
C     connection formulas (J.N.L. Connor, MOL PHYS, 15(1968) 37 and
C     CPL, 4(1969) 419) to compute the nth energy level of the jth
C     bending mode.  The potential is V = V0 + sgnA*X**2 + B*X**4.
C     Phase integrals are computed by analytic expressions in terms      7/19B91
C     of elliptic integrals, and the energy is found by a quadratic      7/19B91
C     fitting procedure.                                                 7/19B91
C     Added 4/8/85. 
C
C     CALLED BY:
C                ZEROPT,VPART,EVIB
C     CALLS:
C            HQSC,THETA2,THETA3,PHID
C
C*     THE USE OF CONSTANTS LIKE 24.0, 16.0 ETC ARE NOT CLEAR
C*      S/7/22/87
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 23/7/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C      SAVE  E1, FN1                                                     09/95KAN
C
      LOGICAL LFIRST
C
      DATA EPSX / 1.0D-8 /
c
      save                                                              0601YC98
C
C     First determine if at this point it is a single well (or an
C     effective single well since the energy is above central barrier)
C     (ITYP=2) or a double well problem (ITYP=3).
C
      IF (B.LE.0.0D0) THEN
         ITYP = 1
         V0 = 0.0D0
      ELSEIF (A.GE.0.0D0) THEN
         ITYP = 2
         V0 = 0.0D0
      ELSE
         ITYP = 3
         V0 = 0.25D0*A*A/B
      ENDIF
      AC = 0.5D0*ABS(A)
      BC = B/24.0D0
C
C     Initialize variables and set necessary constants
C
      IF (WE.EQ.WETS(1,1)) ELAST = 0.5D0*ABS(WE)
      IF (ELAST.EQ.0.0D0) ELAST = 16.0D0/627.5095D0
      EMIN = 0.0D0
      EMAX = 1.0D+30
      IC = 0
      DELC = 0.1D0*V0
      IF (DELC.LE.0.0D0) DELC = 0.01D0
      SGN = -1.0D0
      XN = (DBLE(N)+1.0D0)*0.5D0
      LFIRST = .TRUE.
C
C     Compute initial guess to energy (E2).  If ITYP=2 use the harmonic
C     energy and for a double well use the energy for the previous step.
C
      IF (ITYP.EQ.2.AND.WE.GE.0.0D0) THEN
         E2 = (N+0.5D0)*WE
      ELSE
         E2 = (N+0.5D0)*2.0D0*ELAST
      ENDIF
C
      IF (ITYP.EQ.1) THEN
         CALL HQSC (REDM,AC,BC,N,E2,1.0D+36,IERR)
         IF (IERR.EQ.0) THEN
            WKBPOT = E2
            ELAST = E2
         ELSE
            WKBPOT = E2                                                  7/19B91
C23/7/GL91  VMAX = 0.999999*E2                                          7/19B91
            VMXWKB = 0.999999d0*E2                                      09/95KAN
         ENDIF
         RETURN
      ENDIF
C
C     Loop to search for E2 s.t. RE(FN2)=0.  The first time through it
C     uses a Newton-Raphson type of search and the succeeding times
C     the new energy guess is computed by fitting FN2 to a quadratic.
C
C
C     Compute alpha and epsilon (TH1 and TH2) and their derivatives.
C     NOTE:  Due to the symmetry in this problem alpha=beta.
C
   10 IF (ITYP.EQ.2) CALL THETA2 (REDM,AC,BC,E2,TH1,DTH1,TH2,DTH2,IERR)
      IF (ITYP.EQ.3) CALL THETA3 (REDM,AC,BC,E2,TH1,DTH1,TH2,DTH2,IERR)
C
C     Compute phase factor phi and its derivative
C
      IF (IERR.NE.0) GO TO 120
      CALL PHID (TH2,PH,DPH)
      IF (TH2.LT.27.0D0) GO TO 20
      ATNEX = 0.5D0*PI
      EX = 0.0D0
      GO TO 30
   20 EX = EXP(PI*TH2)
      ATNEX = ATAN(EX)
   30 FN2 = TH1-XN-(PH+SGN*ATNEX)/TPI
C
C     WRITE(FU6,900)TH1,PH,ATNEX,FN2,E2
C
      IF (ABS(FN2).LT.EPSX) GO TO 90
      IF (EX.LT.1.0D+18) GO TO 40
      DATNEX = 0.5D0/EX
      GO TO 50
   40 DATNEX = 0.5D0*EX/(1.0D0+EX*EX)
   50 DFN = DTH1-(DPH/TPI+SGN*DATNEX)*DTH2
      IF (LFIRST) GO TO 60
C
C   FIT F(E) TO QUADRATIC IN E USING F1,F2,DFN2
C
      T = E1-E2
      IF (ABS(T).LT.1.0D-12) GO TO 60
      C = ((FN1-FN2)/T-DFN)/T
      IF (ABS(C).LT.1.0D-8) GO TO 60
      T1 = 0.5D0*DFN/C
      T = 1.0D0-FN2/(T1*T1*C)
      IF (T.LT.0.0D0) GO TO 60
      E1 = E2
      FN1 = FN2
      E2 = E2-T1*(1.0D0-SQRT(T))
      GO TO 80
   60 E1 = E2
      E2 = E2-FN2/DFN
      IF (E2.GT.0.0D0) GO TO 70
      DELC = 0.1D0*DELC
      IF (DELC.LT.1.0D-12) GO TO 120
      E2 = DELC
   70 FN1 = FN2
      LFIRST = .FALSE.
   80 IC = IC+1
      IF (IC.GT.50) GO TO 110
      IF (FN1.GT.0.0D0) EMAX = E1
      IF (FN1.LT.0.0D0) EMIN = E1
      IF (E2.GT.EMIN.AND.E2.LT.EMAX) GO TO 10
      E2 = 0.5D0*(EMIN+EMAX)
      GO TO 10
   90 IF (SGN.GT.0.0D0) GO TO 100
      EM = E2
      EMIN = E2
      EMAX = 1.0D+30
      SGN = 1.0D0
      LFIRST = .TRUE.
      IC = 0
      GO TO 30
  100 EP = E2
      IF (N.EQ.0.AND.E2.NE.0.0D0) ELAST = E2
      IF ((-1)**N.GE.0) THEN
         WKBPOT = EM
      ELSE
         WKBPOT = EP
      ENDIF
      RETURN
  110 WRITE (FU6,1100) IC,E1,FN1
      E2 = E1
      GO TO 90
  120 WRITE (FU6,1000)
      E2 = 0.0D0
      GO TO 90
C
 1000 FORMAT(' WKBPOT - PROBLEM WITH ENERGY LEVEL.  SET TO ZERO.')
 1100 FORMAT(' WKBPOT -',I3,' ITERATIONS. E=',1PE13.5,' F(E)=',E13.5)
C
      END function wkbpot
C
C***********************************************************************
C  WKBVIB
C***********************************************************************
C
      SUBROUTINE wkbvib (LIFREQ,JFREQ,NV)
      use common_inc
      use perconparam
      use rate_const, only : etp,tp1,tp2,wkbtol
C
C   THIS ROUTINE FINDS THE WKB APPROXIMATION TO THE NV EIGENVALUE OF THE
C   LIFREQth NORMAL MODE VIBRATIONS.
C
C   Rewritten February,1987 by GCH
C
C     CALLED BY:
C                 ANHARM
C
C     CALLS:
C                 TRANS,ENERG,TP,PHSINT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      dimension tx(n3tm), tdx(n3tm)
C
      DATA TPSX / 0.00001D0 /
C
C   IF THIS IS THE FIRST TIME THIS SUBROUTINE HAS BEEN ENTERED,
C   INITIALIZE THE STORAGE OF TURNING POINTS AND ENERGIES
C
      call wkb_mem

      IF (IFWKB.NE.111999) THEN
         DO 10 I = 1, N3TM
C* 30 CHANGED TO N3TM BY TOM JOSEPH 7/15/87
            TP1(I) = 0.0D0
            TP2(I) = 0.0D0
            ETP(I) = 0.0D0
   10    CONTINUE
         WKBTOL = ABS(WKBTOL)
         IFWKB = 111999
      ENDIF
C
C   PUT CURRENT GEOMETRY INTO TEMPORARY STORAGE.  VI IS THE CLASSICAL
C   ENERGY.
C
      DO 20 I = 1, N3
         TX(I) = X(I)
         TDX(I) = DX(I)
   20 CONTINUE
c      call energ(1)                                                     6/2RS94
      call ehook(1,iproc)                                                     0301YC97
      VI = V
      FV = DBLE(NV)
      XN = FV+0.5D0
      XMU = REDM
      XEPS = DERSTP
      EMIN = 0.0D0
      EMAX = 0.0D0
      X1MAX = 0.0D0
      X2MAX = 0.0D0
      X1MIN = 0.0D0
      X2MIN = 0.0D0
      THETA = 0.0D0
      E = 0.0D0
      JFLAG = 0
C
C   CHOOSE THE H.O. TURNING POINTS AS INITIAL GUESSES TO THE TURNING
C   POINTS OR USE TURNING POINTS FROM PREVIOUS SAVE POINT ON THE MEP
C
      IF (TP1(JFREQ).GE.0.0D0.OR.TP2(JFREQ).LE.0.0D0) THEN
         IF (FREQ(JFREQ).GT.0.0D0) THEN
            X2 = 1.0D0/SQRT(XMU*FREQ(JFREQ))
            X2 = X2*SQRT(2.0D0*FV+1.0D0)
            X1X = -X2
         ENDIF
C
C  FREQUENCY IS ZERO OR NEGATIVE - SEARCH FOR TURNING POINTS FOR E=0
C
         IF (FREQ(JFREQ).LE.0.0D0) THEN
            X1X = -TPSX
            CALL TP (E1,X1X,0.0D0,0.0D0,VI,LIFREQ,
     *               JFREQ,TX,0.0D0,0,DEDX1,ICON)
            X2 = TPSX
            CALL TP (E2,X2,0.0D0,0.0D0,VI,LIFREQ,
     *               JFREQ,TX,0.0D0,0,DEDX2,ICON)
            IF (E1.LT.0.0D0) THEN
               E = 0.0D0
               XX1=X1X
               CALL TP (E,XX1,X1X,0.0D0,VI,LIFREQ,
     *                  JFREQ,TX,XN,2,DEDXX1,ICON)
               IF (ICON.NE.0) THEN
                  X1X = 0.0D0
                  X2 = 0.0D0
                  JFLAG = 1
                  GO TO 80
               ELSE
                  E1 = 0.0D0
                  X1X = XX1
                  DEDX1 = DEDXX1
               ENDIF
            ELSE
               E1 = 0.0D0
               X1X = 0.0D0
               DEDX1 = 0.0D0
            ENDIF
            IF (E2.LT.0.0D0) THEN
               E = 0.0D0
               CALL TP (E,XX2,X2,0.0D0,VI,LIFREQ,
     *                  JFREQ,TX,XN,2,DEDXX2,ICON)
               IF (ICON.NE.0) THEN
                  X1X = 0.0D0
                  X2 = 0.0D0
                  JFLAG = 1
                  GO TO 80
               ELSE
                  E2 = 0.0D0
                  X2 = XX2
                  DEDX2 = DEDXX2
               ENDIF
            ELSE
               E2 = 0.0D0
               X2 = 0.0D0
               DEDX2 = 0.0D0
            ENDIF
            E = 0.0D0
            CALL PHSINT (E,X1X,X2,VI,LIFREQ,JFREQ,TX,THETA,DNDE,1,IERR)
            IF (THETA.GT.XN) THEN
               X1X = 0.0D0
               X2 = 0.0D0
               E = 0.0D0
               GO TO 80
            ELSE
               X1MIN = ABS(X1X)
               X2MIN = X2
               X1X = X1X-XEPS
               X2 = X2+XEPS
            ENDIF
         ENDIF
C
C  OTHERWISE SET TURNING POINTS TO VALUES AT PREVIOUS SAVE POINT
C
      ELSE
         X1X = TP1(JFREQ)
         X2 = TP2(JFREQ)
      ENDIF
C
C   CHOOSE THE INITIAL GUESS TO THE WKB ENERGY AS THE MINIMUM ENERGY
C   EVALUATED AT THE TURNING POINTS
C
   30 CALL TP (E1,X1X,0.0D0,0.0D0,VI,LIFREQ,
     *         JFREQ,TX,0.0D0,0,DEDX1,ICON)
      IF (ICON.NE.0) THEN
         IF (ABS(X1X).LT.XEPS) THEN
            JFLAG = 1
            GO TO 80
         ENDIF
         X1X = X1X/2.0D0
         GO TO 30
      ENDIF
      A1E = E1
      A1X = X1X
      CALL TP (E2,X2,0.0D0,0.0D0,VI,LIFREQ,
     *         JFREQ,TX,0.0D0,0,DEDX2,ICON)
      IF (ICON.NE.0) THEN
         IF (X2.LT.XEPS) THEN
            JFLAG = 1
            GO TO 80
         ENDIF
         X2 = X2/2.0D0
         GO TO 30
      ENDIF
      A2E = E2
      A2X = X2
C
      IF (E1.LE.E2) THEN
         IDIR = 1
         E = E1
         XMAX = X2
         XX = X2
      ELSE
         IDIR = -1
         E = E2
         XMAX = X1X
         XX = X1X
      ENDIF
C
C     LOCATE THE OTHER TURNING POINT AT ENERGY E
C
      CALL TP (E,XX,0.0D0,XMAX,VI,LIFREQ,JFREQ,TX,0.0D0,1,DEDX,ICON)
      IF (IDIR.GT.0) THEN
         X2 = XX
         E2 = E
         DEDX2 = DEDX
      ELSE
         X1X = XX
         E1 = E
         DEDX1 = DEDX
      ENDIF
C
C  LOOP OVER EVALUATION OF THETA AND CALCULATION OF NEW STEP
C
C     EVALUATE THE PHASE INTEGRAL AND COMPARE TO XN
C
   40 CALL PHSINT (E,X1X,X2,VI,LIFREQ,JFREQ,TX,THETA,DNDE,1,IERR)
      IF (IERR.EQ.1.OR.IERR.EQ.3) THEN
         CALL TP (E,X1X,0.0D0,0.0D0,VI,LIFREQ,JFREQ,
     *            TX,0.0D0,0,DEDX1,ICON)
         IF (E.EQ.A1E) A1X = X1X
      ENDIF
      IF (IERR.EQ.2.OR.IERR.EQ.3) THEN
         CALL TP (E,X2,0.0D0,0.0D0,VI,LIFREQ,
     *            JFREQ,TX,0.0D0,0,DEDX2,ICON)
         IF (E.EQ.A2E) A2X = X2
      ENDIF
C
      OLDE = E
      DIF = XN-THETA
C
      IF (DIF.GT.0.0D0) THEN
         X1MIN = MAX(ABS(X1X),X1MIN)
         X2MIN = MAX(X2,X2MIN)
         EMIN = MAX(E,EMIN)
      ELSE
         IF ((EMAX.GT.0.0D0.AND.E.LE.EMAX).OR.(EMAX.EQ.0.0D0)) THEN
            X1MAX = ABS(X1X)
            X2MAX = X2
            EMAX = E
         ENDIF
      ENDIF
C
C  TEST FOR CONVERGENCE
C
      IF (EMAX.GT.0.0D0) THEN
         EDIF = EMAX-EMIN
         IF (ABS(EDIF).LT.WKBTOL) GO TO 80
      ENDIF
C
C  CALCULATE A NEW STEP ALONG THE MODE COORDINATE, USING THE DERIVATIVES
C
      IF (DNDE.GT.0.0D0) THEN
         DELE = DIF/DNDE
      ELSE
         DELE = WKBTOL*SIGN(1.0D0,DIF)
      ENDIF
      IF (ABS(DEDX1).GT.DEDX2) THEN
C
C   FIND A NEW NEGATIVE TURNING POINT
C
         IF (DEDX1.NE.0.0D0) THEN
            DELX1 = DELE/DEDX1
         ELSE
            DELX1 = XEPS*SIGN(-1.0D0,DIF)
         ENDIF
         KFLAG = 0
   50    XX1 = X1X+DELX1
         CALL TP (E,XX1,0.0D0,0.0D0,VI,LIFREQ,
     *            JFREQ,TX,0.0D0,0,DEDXX1,ICON)
         TEST = (E-OLDE)*DELE
         IF ((ICON.NE.0).OR.(TEST.LT.0.0D0).OR.(E.GE.EMAX.AND.EMAX.NE.
     *      0.0D0).OR.(E.LE.EMIN)) THEN
C
C  ERROR AT THIS TURNING POINT
C
            KFLAG = 1
            IF (ABS(E-OLDE).LT.WKBTOL) THEN
               IF (E.GE.EMAX.AND.EMAX.NE.0.0D0) THEN
C
C THE ROUTINE HAS FOUND AN ASYMPTOTIC LIMIT TO THE BOUND ENERGY
C
                  GO TO 80
               ELSE
C
C  THE ROUTINE HAS FOUND A LOCAL MAXIMUM WITH ENERGY LESS THAN THE BOUND
C  THE VALUE OF THE PHASE INTEGRAL AT THIS ENERGY IS THETA.  SEARCH FOR
C  TURNING POINT WITH THE SAME ENERGY, UNTIL THE APPROXIMATE VALUE OF TH
C  BECOMES LARGER THAN XN
C
                  XXX = XX1
                  CALL TP (E,XX1,XXX,0.0D0,VI,LIFREQ,JFREQ,TX,DIF,2,
     *               DEDXX1,ICON)
                  IF (ICON.NE.0) THEN
C
C  SEARCH FAILED
C
                     GO TO 80
                  ELSE
                     IF (E.EQ.A1E) A1X = XX1
                     GO TO 70
                  ENDIF
               ENDIF
            ELSE
C
C  OTHERWISE, CHOOSE A SMALLER STEPSIZE
C
               DELX1 = DELX1*0.5D0
               GO TO 50
            ENDIF
         ELSEIF (ABS(E-OLDE).LT.WKBTOL*0.5D0.AND.KFLAG.EQ.0) THEN
C
C  THE CHANGE IN ENERGY IS TOO SMALL - TAKE A BIGGER STEP
C
            DELX1 = DELX1*3.0D0
            GO TO 50
         ENDIF
C
C  TURNING POINT OK
C
         IF (E.GT.A1E) THEN
            A1E = E
            A1X = XX1
         ENDIF
C
C  LOCATE THE POSITIVE TURNING POINT
C
         IF (A2E.GE.E) THEN
            IDO = 1
            XMIN = X2MIN
            IF (E2.GE.E) THEN
               XMAX = X2
            ELSE
               XMAX = A2X
            ENDIF
         ELSE
            IDO = 2
            XMIN = A2X
         ENDIF
         XX2 = XMIN
         CALL TP (E,XX2,XMIN,XMAX,VI,LIFREQ,JFREQ,TX,XN,IDO,DEDXX2,ICON)
         IF (ICON.NE.0.AND.IDO.EQ.2) THEN
C
C  NO POSITIVE TURNING POINT COULD BE FOUND FOR ENERGY E
C
            KFLAG = 2
            IF (EMAX.NE.0.0D0) THEN
               EMAX = MIN(E,EMAX)
            ELSE
               EMAX = E
            ENDIF
            X1MAX = ABS(XX1)
            DELX1 = 0.5D0*DELX1
            GO TO 50
         ENDIF
C
C  TURNING POINT LOCATED
C
         IF (E.GT.A2E) THEN
            A2E = E
            A2X = XX2
         ENDIF
      ELSE
C
C   CHOOSE A NEW POSITIVE TURNING POINT
C
         IF (DEDX2.NE.0.0D0) THEN
            DELX2 = DELE/DEDX2
         ELSE
            DELX2 = XEPS*SIGN(1.0D0,DIF)
         ENDIF
         KFLAG = 0
   60    XX2 = X2+DELX2
         CALL TP (E,XX2,0.0D0,0.0D0,VI,LIFREQ,JFREQ,TX,0.0D0,0,DEDXX2,
     *      ICON)
         TEST = (E-OLDE)*DELE
         IF ((ICON.NE.0).OR.(TEST.LT.0.0D0).OR.(E.GE.EMAX.AND.EMAX.NE.
     *      0.0D0).OR.(E.LE.EMIN)) THEN
C
C ERROR AT THIS TURNING POINT
C
            KFLAG = 1
            IF (ABS(E-OLDE).LT.WKBTOL) THEN
               IF (E.GE.EMAX.AND.EMAX.NE.0.0D0) THEN
C
C THE ROUTINE HAS FOUND AN ASYMPTOTIC LIMIT TO THE BOUND ENERGY
C
                  GO TO 80
               ELSE
C
C  THE ROUTINE HAS FOUND A LOCAL MAXIMUM WITH ENERGY LESS THAN THE BOUND
C  THE VALUE OF THE PHASE INTEGRAL AT THIS ENERGY IS THETA.  SEARCH FOR
C  TURNING POINT WITH THE SAME ENERGY, UNTIL THE APPROXIMATE VALUE OF TH
C  BECOMES LARGER THAN XN
C
                  XXX = XX2
                  CALL TP (E,XX2,XXX,0.0D0,VI,LIFREQ,JFREQ,TX,DIF,2,
     *               DEDXX2,ICON)
                  IF (ICON.NE.0) THEN
C
C  SEARCH FAILED
C
                     GO TO 80
                  ELSE
                     IF (E.EQ.A2E) AX2 = XX2
                     GO TO 70
                  ENDIF
               ENDIF
            ELSE
C
C OTHERWISE CHOOSE A SMALLER STEP SIZE
C
               DELX2 = DELX2*0.5D0
               GO TO 60
            ENDIF
         ELSEIF (ABS(E-OLDE).LT.WKBTOL*0.5D0.AND.KFLAG.EQ.0) THEN
C
C  THE CHANGE IN ENERGY IS TOO SMALL - TAKE A BIGGER STEP
C
            DELX2 = DELX2*3.0D0
            GO TO 60
         ENDIF
C
C TURNING POINT OK
C
         IF (E.GT.A2E) THEN
            A2E = E
            A2X = XX2
         ENDIF
C
C  LOCATE THE NEGATIVE TURNING POINT
C
         IF (A1E.GE.E) THEN
            IDO = 1
            XMIN = -X1MIN
            IF (E1.GE.E) THEN
               XMAX = X1X
            ELSE
               XMAX = A1X
            ENDIF
         ELSE
            IDO = 2
            XMIN = A1X
         ENDIF
         XX1 = XMIN
         IF (IDO.EQ.1.AND.XX1.GE.0.0D0) XX1 = -XEPS
         CALL TP (E,XX1,XMIN,XMAX,VI,LIFREQ,JFREQ,TX,XN,IDO,DEDXX1,ICON)
         IF (ICON.NE.0.AND.IDO.EQ.2) THEN
C
C  NO NEGATIVE TURNING POINT COULD BE FOUND FOR ENERGY E
C
            KFLAG = 2
            IF (EMAX.NE.0.0D0) THEN
               EMAX = MIN(E,EMAX)
            ELSE
               EMAX = E
            ENDIF
            X2MAX = XX2
            DELX2 = 0.5D0*DELX2
            GO TO 60
         ENDIF
C
C  TURNING POINT LOCATED
C
         IF (E.GT.A1E) THEN
            A1E = E
            A1X = XX1
         ENDIF
      ENDIF
C
C  BOTH NEGATIVE AND POSITIVE TURNING POINTS HAVE BEEN LOCATED FOR E
C
   70 X1X = XX1
      E1 = E
      DEDX1 = DEDXX1
      X2 = XX2
      E2 = E
      DEDX2 = DEDXX2
      GO TO 40
C
   80 CONTINUE
C
C   SEARCH COMPLETED
C   STORE THE EIGENVALUE IN ETP(LIFREQ) AND THE TURNING POINTS IN TP1,TP2
C
      ETP(LIFREQ) = E
      TP1(LIFREQ) = X1X
      TP2(LIFREQ) = X2
C
C   MAKE SURE THE CORRECT VALUE OF THETA WAS FOUND
C
      IF (JFLAG.EQ.0) THEN
         TERR = ABS(DNDE)*WKBTOL
         IF (ABS(DIF).GT.TERR) JFLAG = 1
      ENDIF
      IF (JFLAG.NE.0) WRITE (FU6,1000) S,LIFREQ,NV,E,THETA
C
C   RESTORE THE ORIGINAL GEOMETRY
C
      DO 90 I = 1, N3
         X(I) = TX(I)
         DX(I) = TDX(I)
   90 CONTINUE
      RETURN
C
C
 1000 FORMAT(/,' ***** WARNING: AT S =',F10.6,3X,' MODE ',I2,
     *' (v=',I2,') IS UNBOUND OR NEAR ASYMPTOTIC LIMIT'
     *,/,' E = ',E15.8,7X,' THETA = ',E15.8)
C
      END SUBROUTINE wkbvib
C***********************************************************************
C  WRTHOK
C***********************************************************************
C
      subroutine wrthok(jtype)
      use common_inc
      use perconparam
      use kintcm, only : istatu,isup
      use cm, only : lbath,iwrt62
c
      implicit double precision (a-h,o-z)
c
      LOGICAL LTS
c
      IF (JTYPE.EQ.5) THEN 
          LTS = .TRUE.
      ELSE
          LTS = .FALSE.
      ENDIf
c
      if (LTS) then
         write (fu61,*) '*START'
         ntot = natom
      else 
         if (jtype.le.4) then                                           0910JC97
            ntot = nratom(jtype)                                        0910JC97
         else                                                           0910JC97
            ntot = natom                                                0910JC97
         endif                                                          0910JC97
         if (jtype.eq.1) write (fu61,*) '*REACT1'
         if (jtype.eq.2) write (fu61,*) '*REACT2'
         if (jtype.eq.3) write (fu61,*) '*PROD1'
         if (jtype.eq.4) write (fu61,*) '*PROD2'
         if (jtype.eq.7) write (fu61,*) '*WELLR'                        0910JC97
         if (jtype.eq.8) write (fu61,*) '*WELLP'                        0910JC97
      endif
c
c     write GEOM section
c
      WRITE (fu61,*) '# Geometry in a.u.'
      WRITE (fu61,*) ' GEOM'
      IF (LTS) THEN
         DO J = 1, NTOT
            LSTR = 3 * J - 2
            LEND = LSTR + 2
            WRITE(fu61,1800) J,(X(L)/AMASS(L),L=LSTR,LEND)
         ENDDO
      ELSE IF (JTYPE.GT.4) THEN                                         0910JC97
         DO J = 1, NTOT
            LSTR = 3 * J - 2
            LEND = LSTR + 2
            WRITE(fu61,1800) J,(X(L),L=LSTR,LEND)
         ENDDO
      ELSE
         DO J = 1, NTOT
            LSTR = 3 * IATOM(J) - 2
            LEND = LSTR + 2
            WRITE(fu61,1800) IATOM(J),(X(L),L=LSTR,LEND)
         ENDDO
      ENDIF
c
1800  FORMAT(3x,i2,5x,f15.8,2x,f15.8,2x,f15.8)
      WRITE (fu61,*) ' END'
c
c     write ENERGY section
c 
      WRITE (fu61,*) '# Energy in a.u.'
c
c     prints the absolute energies instead of relative ones
c
      IF (ISUP.EQ.1) THEN                                               0621YC99
        IF ((JTYPE.EQ.1).OR.(JTYPE.EQ.2).OR.(JTYPE.EQ.4)) THEN          0621YC99
          WRITE (fu61,*) ' ENERGY ', V                                  0621YC99
        ELSE                                                            0621YC99
          WRITE (fu61,*) ' ENERGY ', V + EZER0                          0621YC99
        ENDIF                                                           0621YC99 
      ELSE                                                              0621YC99
        IF (JTYPE.EQ.2.OR.JTYPE.EQ.4) THEN                              0621YC99
          WRITE (fu61,*) ' ENERGY ', 0.0d0                              0621YC99
        ELSEIF (JTYPE.EQ.1) THEN                                        0621YC99
          WRITE (fu61,*) ' ENERGY ', V                                  0621YC99
        ELSE                                                            0621YC99
          WRITE (fu61,*) ' ENERGY ', V + EZER0                          0621YC99
        ENDIF                                                           0621YC99
      ENDIF                                                             0621YC99
c
c     write VIB section
c
      IF (ICODE(JTYPE).EQ.3) THEN 
        ISHFT = 5
      ELSEIF (ICODE(JTYPE).EQ.-4) THEN                                  0317Yc99
        ISHFT = 0                                                       0317Yc99
      ELSE
        ISHFT = 6
      ENDIF
      IF (LTS) ISHFT = ISHFT + 1
      NSTOP = 3*NTOT
      IF (LBATH) NSTOP = NSTOP+1                                        0317Yc99
      IF ((NSTOP-ISHFT).GT.0) THEN
        WRITE (fu61,*) '# Frequencies in a.u.'
        WRITE (fu61,*) ' VIB'
        WRITE (fu61,1500) (FREQ(K),K=NSTOP,ISHFT+1,-1)
        IF (LTS) WRITE (fu61,1600) FREQ(1)
        WRITE (fu61,*) ' END'
        if (lts.and.iabs(iwrt62).eq.1) then                             0522TA02
           write (fu62,*) '*START  #Frequencies in cm-1'                0522TA02
           write (fu62,1562) (freq(k)*autocm,k=nstop,ishft+1,-1),       0522TA02
     *                           freq(1)*autocm                         0522TA02
        elseif (.not.lts.and.iwrt62.eq.1) then                          0522TA02
           if (jtype.eq.1) write(fu62,*) '*REACT1 #Frequencies in cm-1' 0522TA02
           if (jtype.eq.2) write(fu62,*) '*REACT2 #Frequencies in cm-1' 0522TA02
           if (jtype.eq.3) write(fu62,*) '*PROD1  #Frequencies in cm-1' 0522TA02
           if (jtype.eq.4) write(fu62,*) '*PROD2  #Frequencies in cm-1' 0522TA02
           if (jtype.eq.7) write(fu62,*) '*WELLR  #Frequencies in cm-1' 0522TA02
           if (jtype.eq.8) write(fu62,*) '*WELLP  #Frequencies in cm-1' 0522TA02
           write (fu62,1562) (freq(k)*autocm,k=nstop,ishft+1,-1)        0522TA02
        endif                                                           0522TA02
      ENDIF
c
c     write HESSIAN section
c
      IF (LTS) THEN
        IF (ISTATU(5).LE.4) THEN
          WRITE (fu61,*) ' HESSIAN'
          DO I = 1, NSTOP
            J = I 
            WRITE(fu61,1500) 
     *       (F(I,K)*(AMASS(ind(i))*AMASS(ind(k))),K=1,J)
          ENDDO
          WRITE (fu61,*) ' END'
        ENDIF
c
c     write EIGENVECTOR section
c
        WRITE (fu61,*) ' EIGENVECTOR'
        DO I = 1, NSTOP
          WRITE(fu61,1500)
     *         (COF(I,J)*(AMASS(ind(i))*AMASS(ind(j))),J=1,NSTOP)
        ENDDO 
        WRITE (fu61,*) ' END'
      ENDIF
C
 1500 FORMAT (1X, 1E19.10, 3E20.10)
 1600 FORMAT (1x, 1E19.10)
 1562 FORMAT (1x, 10F8.1)
      RETURN 
      END subroutine wrthok
C
C***********************************************************************
C  XDOTP
C***********************************************************************
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      FUNCTION xdotp(XA,XB,NN)
C
C     CALLED BY:
C               LCNWTN, LCXINT
C*
C     THIS FUNCTION CALCULATES THE DOT PRODUCT OF THE VECTORS
C     XA AND XB.
C*
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION XA(NN),XB(NN)
      SUM = 0.0D0
      DO 10 I=1,NN
      SUM = SUM + XA(I)*XB(I)
 10   CONTINUE
      XDOTP = SUM
      RETURN
      END FUNCTION xdotp
C***********************************************************************
C  WRTWEL
C***********************************************************************
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
      SUBROUTINE wrtwel(V,EPRD,VAD,VAR,VAP,NW,IFRFAC,FREQFAC)
      use perconparam
C
C     WRITE OUT WELLS ENERGETICS
C
C     JC 09/11/97
C
C     CALLED BY:
C          REACT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IF (NW.EQ.1) THEN
         WRITE (FU6,2000)
      ELSE
         WRITE (FU6,2005)
      ENDIF
         WRITE (FU6,2100)
         WRITE (FU6,2200) V,V*CEV,V*AUTOCM,V*CKCAL
         VCP = V-EPRD
         WRITE (FU6,2300) VCP,VCP*CEV,VCP*AUTOCM,VCP*CKCAL
         WRITE (FU6,2400) VAD,VAD*CEV,VAD*AUTOCM,VAD*CKCAL
         SAP = VAD-EPRD
         WRITE (FU6,2500) SAP,SAP*CEV,SAP*AUTOCM,SAP*CKCAL
         VZR = VAD-VAR
         WRITE (FU6,2600) VZR,VZR*CEV,VZR*AUTOCM,VZR*CKCAL
         VZP = VAD-VAP
         WRITE (FU6,2700) VZP,VZP*CEV,VZP*AUTOCM,VZP*CKCAL
         ZPES = VAD-V
      IF (NW.EQ.1) THEN
         WRITE (FU6,2800) ZPES,ZPES*CEV,ZPES*AUTOCM,ZPES*CKCAL
      ELSE
         WRITE (FU6,2805) ZPES,ZPES*CEV,ZPES*AUTOCM,ZPES*CKCAL
      ENDIF
C
C   Print out the results when using scaled frequencies
C
         IF (IFRFAC.NE.0) THEN                                          0808JC00
            ZPER = VAD - VZR                                            0808JC00
            ZPEP = SAP - VZP                                            0808JC00
            WRITE (FU6,1900)                                            0808JC00
            IF (NW.EQ.1) THEN                                           0808JC00
               WRITE (FU6,2000)                                         0808JC00
            ELSE                                                        0808JC00
               WRITE (FU6,2005)                                         0808JC00
            ENDIF                                                       0808JC00
            WRITE (FU6,2100)                                            0808JC00
            WRITE (FU6,2200) V,V*CEV,V*AUTOCM,V*CKCAL                   0808JC00
            WRITE (FU6,2300) VCP,VCP*CEV,VCP*AUTOCM,VCP*CKCAL           0808JC00
            WRITE (FU6,2400) (V+ZPES*FREQFAC),                          0808JC00
     *                       (V+ZPES*FREQFAC)*CEV,                      0808JC00
     *                       (V+ZPES*FREQFAC)*AUTOCM,                   0808JC00
     *                       (V+ZPES*FREQFAC)*CKCAL                     0808JC00
            WRITE (FU6,2500) (VCP+ZPES*FREQFAC),                        0808JC00
     *                       (VCP+ZPES*FREQFAC)*CEV,                    0808JC00
     *                       (VCP+ZPES*FREQFAC)*AUTOCM,                 0808JC00
     *                       (VCP+ZPES*FREQFAC)*CKCAL                   0808JC00
            WRITE (FU6,2600) (V+(ZPES - ZPER)*FREQFAC),                 0808JC00
     *                       (V+(ZPES - ZPER)*FREQFAC)*CEV,             0808JC00
     *                       (V+(ZPES - ZPER)*FREQFAC)*AUTOCM,          0808JC00
     *                       (V+(ZPES - ZPER)*FREQFAC)*CKCAL            0808JC00
            WRITE (FU6,2700) (VCP+(ZPES - ZPEP)*FREQFAC),               0808JC00
     *                       (VCP+(ZPES - ZPEP)*FREQFAC)*CEV,           0808JC00
     *                       (VCP+(ZPES - ZPEP)*FREQFAC)*AUTOCM,        0808JC00
     *                       (VCP+(ZPES - ZPEP)*FREQFAC)*CKCAL          0808JC00
            WRITE (FU6,2800) (ZPES*FREQFAC),                            0808JC00
     *                       (ZPES*FREQFAC)*CEV,                        0808JC00
     *                       (ZPES*FREQFAC)*AUTOCM,                     0808JC00
     *                       (ZPES*FREQFAC)*CKCAL                       0808JC00
         ENDIF                                                          0808JC00
C
      RETURN
C
 1900 FORMAT(//1X,'The following values are obtained using scaled',     0808JC00
     *       ' frequencies ',/)                                         0808JC00
 2000 FORMAT(//,78(1H-),/2X,'Reactants well energetics ',
     * '(V = classical energy, ZPE = zero point energy)',
     *      /,78(1H-))
 2005 FORMAT(//,78(1H-),/2X,'Products well energetics ',
     * '(V = classical energy, ZPE = zero point energy)',
     *      /,78(1H-))
 2100 FORMAT(32X,'hartrees',8X,'eV',8X,'cm**-1',7X,'kcal')
 2200 FORMAT(2X,'V w/re reactants V',10X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2300 FORMAT(2X,'V w/re product V',12X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2400 FORMAT(2X,'V+ZPE w/re reactant V',7X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2500 FORMAT(2X,'V+ZPE w/re product V',8X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2600 FORMAT(2X,'V+ZPE w/re reactant V+ZPE',3X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2700 FORMAT(2X,'V+ZPE w/re product V+ZPE',4X,
     * 2(F10.5,2X),F10.2,2X,F10.4)
 2800 FORMAT(2X,'V+ZPE w/re reactants well V',1X,
     * 2(F10.5,2X),F10.2,2X,F10.4,/,78(1H-),/)
 2805 FORMAT(2X,'V+ZPE w/re products well V',2X,
     * 2(F10.5,2X),F10.2,2X,F10.4,/,78(1H-),/)
C
      END SUBROUTINE wrtwel
C
C***********************************************************************
C  YGEN      is removed in this version of POLYRATE
C***********************************************************************
C
C***********************************************************************
C  ZEROPT
C***********************************************************************
C
      SUBROUTINE zeropt (IOP)
      use common_inc
      use perconparam, only : fu6,n3tm
      use rate_const
C
C     COMPUTES ZERO POINT ENERGY FOR NORMOD
C
C      USES TWO NEW LOGICAL VARIABLES (RETLEXC AND LEXC7) TO SUBSTITUTE
C      FIRST EXCITED STATE ENERGY WHEN APPROPRIATE FOR VIB. AD. OR
C      DIAB. CALC.
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C
C     CALLED BY:
C                RPHRD2,RPHINT,NORMOD
C     CALLS:
C            EBND,DUNLEV,PADLEV,EWKB,WKBPOT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
C*
      SAVE 
C     LOGICAL LEXC(N3TM),LEXC7
      LOGICAL LEXC7
      LOGICAL, allocatable :: LEXC(:)
      if(.not.allocated(lexc)) allocate(lexc(n3tm))
C     DIMENSION GSEZ(N3TM)                                              09/95KAN
C
C     DETERMINE NUMBER OF TRANSLATIONS AND ROTATIONS PRESENT
C
      KOP = ABS(IOP)    
      IF (IOP.LT.0) THEN 
C         i = ICVTS-3
         NEND1 = NF(KOP)
         ISHFT = 3*NRATOM(KOP) - NEND1
      ELSE    
         NEND1 = NF(5)
         ISHFT = N3 - NEND1
      ENDIF
      VAD = 0.0D0
      IF (IOP.GE.0) VAD = V
      IF (LGS(25).NE.0) VSAV = VAD                                      0601YC98
      AFLAG = '   '
      IF (LGS(5).GE.21) AFLAG = 'SET'                                   6/30YL91
      DO 10 I = 1, NEND1
         SUM = 0.0D0
         ANHDM = ANHRM(I)                                               9/18YL92
         ABDMY = AB(I)                                                      ..
         FRQDMY = FREQ(I+ISHFT)                                         9/18YL92
         IF (AFLAG.EQ.'SET') LGS(5) = MODE(I)
         IF (LGS(5).LT.3 .OR. LGS(5) .EQ. 9) THEN                       1120GL91
            IF (ICODE(5).LT.0) THEN                                      6/9T88
               LEXC7 = ISHFT.EQ.1.AND.IOP.GE.0                           6/9T88
            ELSEIF (ICODE(5).EQ.4) THEN                                  6/9T88
               LEXC7 = ISHFT.EQ.7                                        6/9T88
            ELSEIF (ICODE(5).EQ.3) THEN                                  6/9T88
               LEXC7 = ISHFT.EQ.6.AND.IOP.GE.0                           6/9T99
            ENDIF                                                        6/9T88
            JSWITC = 1
C
C      IF NEEDED IN NEXT STEP, S SHOULD HAVE BEEN STORED IN PATH.
C
            IF (LEXC7.AND.S.GE.SWITC) JSWITC = 2
            LEXC(I) = LEXC7.AND.LN3(JSWITC,I).EQ.1
            IF (IOP.LT.0) LEXC(I) = L9(KOP,I).EQ.1
            IF ((LGS(5) .EQ. 0 .OR. LGS(5) .EQ. 9) .AND.                11/20/GL91
     *           FREQ(I+ISHFT) .LT. 0) THEN                             11/20/GL91
               GSE(I) = 0.0D0
            ELSE
               GSE(I) = 0.5D0*FREQ(I+ISHFT)
            ENDIF
            IF (LEXC(I)) GSE(I) = GSE(I)+FREQ(I+ISHFT)
            SUM = SUM+GSE(I)
         ENDIF
C
C****************** WKB **********************
C
         IF (LGS(33).EQ.1.AND..NOT.LEXC(I)) THEN
            ENER = ETP(I)
            GSE(I) = ENER
            VAD = VAD+ENER
            GO TO 10
         ENDIF
C
C****************** HARMONIC ***********************
C
         IF (LGS(5).EQ.0.OR.LGS(5).EQ.9) VAD = VAD+SUM                  6/30YL91
C
C      ADD CUBIC ANHARMONICITY TO MORSE MODES
C
         IF (LGS(5).GT.0.AND.LGS(5).LE.2.AND.ANHRM(I).GE.0.0D0) THEN
C
C****************** MORSE ********************
C
            GSE(I) = GSE(I)-0.25D0*FREQ(I+ISHFT)*ANHRM(I)
            IF (LEXC(I)) THEN
               EXTRA = 2.0D0*FREQ(I+ISHFT)*ANHRM(I)
               GSE(I) = GSE(I)-EXTRA
               SUM = SUM+4.0D0*EXTRA
            ENDIF
            SUM = SUM+FREQ(I+ISHFT)*ANHRM(I)
            VAD = VAD+GSE(I)
         ELSEIF (LGS(5).EQ.2.AND.ANHRM(I).LT.0.0D0) THEN
C
C******************** MORSE QUADRATIC-QUARTIC *******************
C
            VIBQUN = 0.0D0
            IF (LEXC(I)) VIBQUN = 1.0D0
            ENER = EBND(FRQDMY,ANHDM,VIBQUN,REDM)                       9/18YL92
            GSE(I) = ENER
            VAD = VAD+ENER
            IF (LEXC(I)) VAD = VAD-FREQ(I+ISHFT)
         ELSEIF (LGS(5).EQ.7) THEN
C
C****************** PRIMITIVE WKB QUADRATIC-QUARTIC ***************
C
            N = 0
            IF (LEXC(I)) N = 1    
            ENER = EWKB(ANHDM,ABDMY,N,FRQDMY,ELAST,REDM)                9/18YL92
            GSE(I) = ENER
            VAD = VAD+ENER
         ELSEIF (LGS(5).EQ.8) THEN
C
C***************** UNIFORM SEMICLASSICAL QUAD-QUARTIC ***************
C
            N = 0
            IF (LEXC(I)) N = 1                                          9/18YL92
C           write(6,*)'LEXC(I) ',LEXC(I)
            ENER = WKBPOT(ANHDM,ABDMY,N,FRQDMY)                         9/18YL92
c           write(6,*) 'ENER ', ENER
            GSE(I) = ENER
            VAD = VAD+ENER                                                     
         ENDIF
   10 CONTINUE
C
C    Perturbation treatment, the zero point energy is already
C    calculated --
C
C      IF (LGS2(15).NE.0) VAD = V + EGRNDT                               1106YL92
      IF (LGS2(15).NE.0) THEN
        IF (IOP.LT.0) THEN
            VAD = VSAV + EGRNDT
        ELSE
            VAD = V + EGRNDT
        ENDIF
      ENDIF
      IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20                            6/30YL91
c      IF (IOP.GE.0) RETURN                                             0601YC98
      RETURN
      END SUBROUTINE zeropt
C
C***********************************************************************
C  ZIGAMA
C***********************************************************************
C
      FUNCTION zigama (Z)
C
C     CALCULATES INCOMPLETE GAMMA FUNCTION(3/2,Z) USING RELATIONS
C     6.5.2,6.5.22 AND 6.5.16 FROM ABRAMOWITZ AND STEGUN.
C     CSQRPI=SQRT(PI)/2.  ERF(Z) IS THE ERROR FUNCTION.
C
C     Include statement was added 6/18/91
C
C     CALLED BY:
C                TAUV
C     CALLS:
C            ERF
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      EXTERNAL ERF                                                      23/10/90VM
      CSQRPI = 0.8862269254527580D0
      EZ = SQRT(Z)
      ZIGAMA = ERF(EZ)
      ZIGAMA = ZIGAMA*CSQRPI
      ZIGAMA = ZIGAMA-EZ*EXP(-Z)
      RETURN
      END FUNCTION zigama                                    
C
C**********************************************************************
C  ZOC3P
C**********************************************************************
C
      SUBROUTINE zoc3p
C
      use common_inc
      use perconparam
      use rate_const
      use cm, only : frict,lbath
      use keyword_interface, only : icrst,icmod
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     This subroutine determine the data of those 3 points 
C     needed to perform the Zero-Order-Correction
C
C     We need the semiempirical and ab initio energies and frequencies
C     they are:
C                 VP1S,VP1A,VP2S,VP2A,BARRS,BARRA,SP1,SP2
C                 FRP1S,FRP1A,FRP2S,FRP2A,WESADS,WESADA
C                 FMIS1A
C
C     Some of them have been read in from unit 50, and some of them
C     has been calculated in polyrate before this subroutine is called
C     here we set the ones whose value haven't been assigned
C
C     Called by:   
C                 MAIN  
C
C     Calls:
C                 FRSORT, REARG1 
C
C 
      LOGICAL LWELLR,LWELLP,LFINTR,LFINTP
      DIMENSION TEMPFR(N3TM), IORDER(N3TM)
C
      PARAMETER( SLIM = 10.0d0 )                                        09/95KAN
C
      I1 = LGSIC(1)/10
      I2 = MOD(LGSIC(1),10)
      LWELLR = I1 .EQ. 3
      LWELLP = I2 .EQ. 3
      LFINTR = I1 .NE. 2
      LFINTP = I2 .NE. 2
      NFRR  = NF(1) + NF(2)
      NFRP  = NF(3) + NF(4)
      NFRS  = NF(5)
      NZFRR = MAX0(NFRS - NFRR,0)
      NZFRP = MAX0(NFRS - NFRP,0) 
C
      IF (LFINTR) THEN
         NFRP1 = NFRS + 1
      ELSE
         NFRP1 = NFRR
      ENDIF
      IF (LFINTP) THEN
         NFRP2 = NFRS + 1
      ELSE
         NFRP2 = NFRP
      ENDIF
C
      if (LBATH) FRSOL = (PI/(4*FRICT))                                 0317Yc99
C
      IF (.NOT. LWELLR) THEN
         VP1S = 0.0d0                                                   09/95KAN
         VP1A = 0.0d0                                                   09/95KAN
         DO 50 I = 1, NFRR
            FRP1S(I+NZFRR) = WER(I)
50       CONTINUE
c
c        // reverse order
c
         IF (LBATH) THEN                                                0317Yc99
           IF (LFINTR) THEN                                             0317Yc99
             FRP1S(NFRP1) = FRSOL                                       0317Yc99
           ELSE                                                         0317Yc99
             FRP1S(NZFRR) = FRSOL                                       0317Yc99
           ENDIF                                                        0317Yc99
         ENDIF                                                          0317Yc99
         IF (.NOT. LFINTR) SP1 = -SLIM
      ENDIF
C
      IF (.NOT. LWELLP) THEN
         VP2S = EPRD
         VP2A = ERXN
         DO 60 I = 1, NFRP
            FRP2S(I+NZFRP) = WER(I+NFRR)
60       CONTINUE  
c
c        //reverse order
c
         IF (LBATH) THEN                                                0317Yc99
           IF (LFINTP) THEN                                             0317Yc99
             FRP2S(NFRP2) = FRSOL                                       0317Yc99
           ELSE                                                         0317Yc99
             FRP2S(NZFRP) = FRSOL                                       0317Yc99
           ENDIF                                                        0317Yc99
         ENDIF                                                          0317Yc99
         IF (.NOT. LFINTP) SP2 = SLIM       
      ENDIF
C
C     Check if the energies of the two stationary points are lower than
C     the energy of saddle point
C 
c      IF (LGS(3) .NE. 0 .AND.                                           1116WH93
c     *   .NOT. (BARRA .GT. VP1A .AND. BARRA .GT. VP2A)) THEN            0601YC98
c         WRITE(FU6,1000) BARRA * CKCAL, VP1A * CKCAL, VP2A * CKCAL      0601YC98
c         CLOSE(FU6)                                                     0601YC98
c         STOP 'ZOC3P 1'                                                 0601YC98
c      ENDIF                                                             0601YC98
c
c     for no frequencies corrections, copy the frequencies from the low-level
c     to high-level                                                     0626YC97
c
      IF (lgs2(10).EQ.3) THEN
          DO I = 1,NFRS
             WESADA(I) = WESADS(I)
          ENDDO
          if (LBATH) WESADA(NFRS) = FRSOL                               0317Yc99
          DO I = 1,NFRR+NFRP
             WERA(I) = WER(I)
          ENDDO
          DO I = 1,NFRR
             FRP1A(I) = WERA(I)
          ENDDO
          if (LBATH) FRP1A(NFRR+1) = FRSOL                              0317Yc99
          DO I = 1,NFRP
             FRP2A(I) = WERA(I+NFRR)
          ENDDO
          if (LBATH) FRP2A(NFRP+1) = FRSOL                              0317Yc99
      ENDIF
c
c     if freqmat is not used with either well or one, the list of the frequencies
c     will be increased by one this means to delete the lowest mode     0626YC97
c
      IF ((LGSIC(2).EQ.0).AND.LFINTR) THEN
           DO I = 1,NFRS
               IFRR(I) = IFRR(I)+1
           ENDDO
      ENDIF
      IF ((LGSIC(2).EQ.0).AND.LFINTP) THEN
           DO I = 1,NFRS
               IFRP(I) = IFRP(I)+1
           ENDDO
      ENDIF
C     
C     Sort the reactant and product side frequencies in accending order
C
      IF (LGS(3) .NE. 0) THEN
C        IF (.NOT. LWELLR) CALL FRSORT(NFRP1,FRP1S,.TRUE.,IORDER)       0317Yc99
C        IF (.NOT. LWELLP) CALL FRSORT(NFRP2,FRP2S,.TRUE.,IORDER)       0317Yc99
C     The above two lines has a bug that the size of FRP1S or FRP2S is
C     defined consistently if use NFRP1 or NFRP2 in FRSORT subroutine.
C     The NFRP1 and NFRP2 should be replace by NFRS
         IF (.NOT. LWELLR) CALL FRSORT(NFRS,FRP1S,.TRUE.,IORDER)        1023JZ07
         IF (.NOT. LWELLP) CALL FRSORT(NFRS,FRP2S,.TRUE.,IORDER)        1023JZ07
C 
C     Match the saddle point frequencies to the reactant and product
C     side frequencies according to 
C     the arrays IFRR and IFRP which are read in from unit 50
C
         CALL REARG1(NFRP1,NFRS,FRP1S,IFRR)
         CALL REARG1(NFRP2,NFRS,FRP2S,IFRP)
         CALL REARG1(NFRP1,NFRS,FRP1A,IFRR)
         CALL REARG1(NFRP2,NFRS,FRP2A,IFRP)
      ENDIF
C
C     Reorder the WERA array such that it matches the WER array element by
C     element so as to ensure that we get the corrected zero-point energy and
C     partition function right.
C
C     Optionally force the product ab initio frequencies to be the same
C     as polyrate product frequencies (LGSIC(3)=0)
C  
      DO 100 I = 1,NFRR
         TEMPFR(I) = WER(I)
100   CONTINUE
      CALL FRSORT(NFRR,TEMPFR,.TRUE.,IORDER)
      DO 150 I = 1,NFRR
         TEMPFR(IORDER(I)) = WERA(I)
150   CONTINUE
      DO 160 I = 1,NFRR
         WERA(I) = TEMPFR(I)
160   CONTINUE
      IF (LGSIC(3).EQ.0) THEN
         DO 170 I = 1, NFRP
            WERA(I+NFRR) = WER(I+NFRR)
            IF (LGS(3) .EQ. 0) FRP2A(I) = WER(I+NFRR)
170      CONTINUE
      ELSE
         DO 200 I = 1,NFRP
            TEMPFR(I) = WER(I+NFRR)
200      CONTINUE
         CALL FRSORT(NFRP,TEMPFR,.TRUE.,IORDER)
         DO 250 I = 1,NFRP
            TEMPFR(IORDER(I)) = WERA(I+NFRR)
250      CONTINUE
         DO 260 I = 1,NFRP
            WERA(I+NFRR) = TEMPFR(I)
260      CONTINUE
      ENDIF 
C
C     Prepare det I interpolation information
C
      if (icrst.eq.0.and.icmod.eq.0) FMISPS = FMOM(5)                   0913RS96
C
      RETURN
C
1000  FORMAT(3F12.4)
C
      END SUBROUTINE zoc3p
C
C**********************************************************************
C  ZOCAST
C**********************************************************************

      SUBROUTINE zocast
      use common_inc
      use perconparam
      use rate_const
      use cm, only : frict,lbath
      use keyword_interface, only : icrst,icmod
C
C     CALLED BY:
C
C               ZOCUPD
C
C     CALLS : ZOCVCL, ZOCFRE
C
C     This subroutine calculate the asy values of the ZOC 
C     functions to make sure we did everything right
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION FRP1AT(N3TM),FRP2AT(N3TM)                               09/95KAN
C
      ZERO = 0.0D0
      NFRR = NF(1) + NF(2)
      NFRP = NF(3) + NF(4)
      NTOT = NFRR + NFRP
      NFRS = NF(5)
C
      IF (LGS(3) .NE. 0) THEN
         VP1AT = VP1S + ZOCVCL(SP1)
         VP2AT = VP2S + ZOCVCL(SP2)
C
C YC  - IC methods  =0:IC/=1:ICR/=2:ICL                                 03/96/YC
C
         IF (LGS2(10).EQ.0) THEN                                        03/96/YC
           DO I = 1,NFRS
               FRP1AT(I) = FRP1S(I)  + ZOCFRE(I,SP1)
               FRP2AT(I) = FRP2S(I)  + ZOCFRE(I,SP2)
           ENDDO
         ENDIF
         IF (LGS2(10).EQ.1) THEN                                        03/96/YC
           DO I = 1,NFRS                                                03/96/YC
              FRP1AT(I) = FRP1S(I)  * ZOCFRE(I,SP1)                     03/96/YC
              FRP2AT(I) = FRP2S(I)  * ZOCFRE(I,SP2)                     03/96/YC
           ENDDO                                                        03/96/YC
         ENDIF                                                          03/96/YC
         IF (LGS2(10).EQ.2) THEN                                        03/96/YC
           DO I = 1,NFRS                                                03/96/YC
              FRP1AT(I) = FRP1S(I)  * EXP(ZOCFRE(I,SP1))                03/96/YC
              FRP2AT(I) = FRP2S(I)  * EXP(ZOCFRE(I,SP2))                03/96/YC
           ENDDO                                                        03/96/YC
         ENDIF                                                          03/96/YC
         IF (LGS2(10).EQ.3) THEN                                        0626YC97
           DO I = 1,NFRS                                                0626YC97
               FRP1AT(I) = FRP1S(I)                                     0626YC97
               FRP2AT(I) = FRP2S(I)                                     0626YC97
           ENDDO                                                        0626YC97
         ENDIF                                                          0626YC97
100      CONTINUE
         DO 90 I = 1, LGSIC(7)
            J = ICFR(I)
            FRP1AT(J) = ZOCFRE(J,SP1)
            FRP2AT(J) = ZOCFRE(J,SP2)
90       CONTINUE 
      ENDIF
C
      WRITE(FU6,200)
      WRITE(FU6,210) (BARRA - VCLAS(NSHLF)) * CKCAL
      WRITE(FU6,220) (EPRD - ERXN)  * CKCAL
      IF (LGS(3) .NE. 0) THEN
         WRITE(FU6,230) (VP1A - VP1AT) * CKCAL
         WRITE(FU6,240) (VP2A - VP2AT) * CKCAL 
      ENDIF
C     
C     Write the det I differences information
C
      IF (LGSIC(4) .NE. 0) THEN     
         WRITE(FU6,245)
         IF (LGS(6) .EQ. 1 .OR. LGS(6) .EQ. 2) THEN
            WRITE(FU6,250) FMIR1A - FMOM(1), FMIR2A - FMOM(2)
         ELSE 
            WRITE(FU6,250) FMIR1A - FMOM(1)
         ENDIF
         WRITE(FU6,252) FMISPA - FMITS(NSHLF)
         IF (LGS(6) .EQ. 1 .OR. LGS(6) .EQ. 3) THEN
            WRITE(FU6,254) FMIP1A - FMOM(3), FMIP2A - FMOM(4)
         ELSE
            WRITE(FU6,254) FMIP1A - FMOM(3)
         ENDIF
      ENDIF
C
C        Write the frequency differences information
C
      WRITE(FU6,340)
      WRITE(FU6,400)(NFRR+1-I,(WERA(I)-WER(I))*AUTOCM,I=NFRR,1,-1)
C
      IF (LGS(3) .NE. 0) THEN
         WRITE(FU6,310)
         WRITE(FU6,400)(NFRS+1-I,(FRP1A(I)-FRP1AT(I))*AUTOCM,
     *                  I=NFRS,1,-1)
      ENDIF
C
      WRITE(FU6,330)
      WRITE(FU6,400)(NFRS+1-I,(WESADA(I)-WETS(I,NSHLF))*AUTOCM,
     *                  I=NFRS,1,-1)
C
      IF (LGS(3) .NE. 0) THEN
         WRITE(FU6,320)
         WRITE(FU6,400)(NFRS+1-I,(FRP2A(I)-FRP2AT(I))*AUTOCM,
     *                  I=NFRS,1,-1)
      ENDIF
C
      WRITE(FU6,350)
      WRITE(FU6,400)(NTOT+1-I,(WERA(I)-WER(I))*AUTOCM,I=NTOT,NFRR+1,-1)
C
      RETURN  
C  
 200  FORMAT(/2X,'Differences in the corrected data between input and ',
     *           'interpolated values',
     *       /1X,73('*'),
     *      //1X,'                        Diff.(kcal/mol)')
 210  FORMAT( 1X,'Energy of saddle point: ',F15.4)
 220  FORMAT( 1X,'Energy of reaction    : ',F15.4)
 230  FORMAT( 1X,'Energy of point 1     : ',F15.4)
 240  FORMAT( 1X,'Energy of point 2     : ',F15.4)
 245  FORMAT(/1X,'                       Diff.(a.u.)')
 250  FORMAT( 1X,'Det I of reactant    : ',1P,2E15.4)
 252  FORMAT( 1X,'Det I of saddle point: ',1P,E15.4)
 254  FORMAT( 1X,'Det I of product     : ',1P,2E15.4)
 310  FORMAT(/1X,'Point 1  frequencies:',/1X,'Mode    Diff.(cm**-1)')
 320  FORMAT(/1X,'Point 2  frequencies:',/1X,'Mode    Diff.(cm**-1)')
 330  FORMAT(/1X,'Saddle point frequencies:',
     *       /1X,'Mode    Diff.(cm**-1)')
 340  FORMAT(/1X,'Reactant frequencies:',/1X,'Mode    Diff.(cm**-1)')
 350  FORMAT(/1X,'Product  frequencies:',/1X,'Mode    Diff.(cm**-1)')
 400  FORMAT(1X,I4,F15.4)     
C
      END SUBROUTINE zocast
C
C**********************************************************************
C  ZOCFRE
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION zocfre(I,SMEP)
      use rate_const

C     CALLED BY:
C                ZOCUPD
C
C     THIS FUNCTION CALCULATES THE CORRECTION FOR FREQUENCIES ** 2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LINF,LFINTR,LFINTP
C
      I1 = LGSIC(1)/10
      I2 = MOD(LGSIC(1),10)
      LFINTR = I1 .NE. 2
      LFINTP = I2 .NE. 2
      LINF = (.NOT. LFINTR .AND. SMEP.LE.0 .OR.
     *        .NOT. LFINTP .AND. SMEP.GE.0)
C
      IF (IFFUN(I) .EQ. 0) THEN
        ZOCFRE = 0
      ELSE IF (LINF) THEN
         IF (IFFUN(I).EQ.1) THEN
            ZOCFRE = ECKART(AF(I),BF(I),CF(I),S0F(I),RANGE,SMEP)
         ELSE IF (IFFUN(I).EQ.2) THEN
            ZOCFRE = HBT(AF(I),CF(I),S0F(I),RANGE,SMEP)
         ENDIF
      ELSE IF (LFINTR .AND. SMEP.LT.0) THEN
         IF (IFFUN(I).EQ.1) THEN
            ZOCFRE = COG(AF1(I),BF1(I),CF1(I),SP1,SMEP)
         ELSE IF (IFFUN(I).EQ.2) THEN
            IF (LFINTP) THEN
               ZOCFRE = COHBT2(AF(I),CF(I),S0F(I),RANGE,SP1,SP2,SMEP)
            ELSE
               ZOCFRE = COHBT1(AF(I),CF(I),S0F(I),RANGE,SP1,SMEP)
            ENDIF
         ENDIF
      ELSE IF (LFINTP .AND. SMEP.GE.0) THEN
         IF (IFFUN(I).EQ.1) THEN
            ZOCFRE = COG(AF2(I),BF2(I),CF2(I),SP2,SMEP)
         ELSE IF (IFFUN(I).EQ.2) THEN
            IF (LFINTR) THEN
               ZOCFRE = COHBT2(AF(I),CF(I),S0F(I),RANGE,SP1,SP2,SMEP)
            ELSE
               ZOCFRE = COHBT1(AF(I),CF(I),S0F(I),RANGE,SP2,SMEP)
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
C
      END function zocfre
C
C**********************************************************************
C  ZOCPAR
C**********************************************************************
C
      SUBROUTINE zocpar
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : gufac6,ivice,iunit6

C     CALLED BY:
C               MAIN, DOREST
C
C     THIS SUBROUTINE CALCULATES THE PARAMETERS
C     FOR THE ECKART FUNTION OR HYPERBOLIC TANGENT
C     IN THE ZERO ORDER INTERPOLATION APPROXIMATION.
C
C     IT WILL PASS THE APPROPRIATE PARAMETERS VIA COMMON
C     BLOCKS TO THE FUNCTIONS ZOCVCL AND ZOCFRE TO CALCULATE
C     THE CORRECTIONS FOR VMEP AND FREQUENCIES.  
C
C     Modified for version 6.0 keyword input  05/21/94 WH
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C--------------------------------------------------------------------
C     IN THE FOLLOWING ? = R --- REACTANT
C                          P --- PRODUCT
C                          S --- SADDLE POINT
C
C     V0A,V0S  : AB INITIO AND SEMIEMPIRICAL BARRIER HEIGHT
C     VPA,VPS  : AB INITIO AND SEMIEMPIRICAL ENERGY OF P
C     REDM     : REDUCED MASS
C     RANGE    : RANGE PARAMETER OBTAINED FROM AB INITIO DATA
C     AV,BV,S0V: PARAMETERS IN ECKART FUNCTION FOR VMEP
C     DFR?     : ARRAYS OF THE DIFF. IN AB INITIO & SEMI FREQUENCIES
C                FOR R,P AND S
C     DV0,DVP,DVR  : DIFFERENCES IN ENERGY 
C     AF,BF,CF,
C     S0F      : PARAMETER ARRAY FOR THE FREQ. CORRECTION FUNCTION
C     NFR?     : NUMBER OF VIBRATIONNAL MODE FOR R,P ANS S
C     IVFUN,   : FLAG THAT DETERMINES WHICH CORRECTION FUNCTION
C     IFFUN(I)   TO USE FOR ENERGY AND FREQUENCY MODE I
C-------------------------------------------------------------------
C
      DIMENSION  DFRR(N3TM), DFRP(N3TM), DFRS(N3TM)
c
      call zocpar_mem
C
      EPIS  = 1.0D-6 
      VRS   = VP1S
      VPS   = VP2S
      V0S   = BARRS
      VRA   = VP1A
      VPA   = VP2A
      V0A   = BARRA
      NFRS  = NF(5)
      IF (IVICE.NE.2) THEN                                              0203YC98

C     RANGEA is the L from high level (calc from imaginary freq)
C
        AV = VPA - VRA
        CV = VRA
        DET = SQRT( ( V0A - CV ) * ( V0A - CV - AV ) )
        BV = 2.0D0*V0A - AV - 2.0D0*CV + 2.0D0*SIGN(DET,V0A-VRA)
C
C Calculate L parameter from the imaginary frequency of HL              1203YC96
C
        IF (RANGEA.EQ.0)                                                1203YC96
     *     RANGEA = SQRT((2.0d0*V0A*(V0A-AV))/(REDM*(TSWIM*TSWIM)*BV))  1203YC96
        S0VA = -RANGEA * LOG ((AV+BV)/(BV-AV))                          1203YC96
c
C approximate the RPL with L from imaginary freq if not read in         1203YC96
c
        AVS = VPS - VRS                                                 1203YC96
        CVS = VRS                                                       1203YC96
        DETS = SQRT( ( V0S - CVS ) * ( V0S - CVS - AVS ) )              1203YC96
        BVS = 2.0D0*V0S - AVS - 2.0D0*CVS + 2.0D0*SIGN(DETS,V0S-VRS)    1203YC96
        RANGES = SQRT((2.0d0*V0S*(V0S-AVS))/(REDM*(WSTAR*WSTAR)*BVS))   1203YC96
        IF (LGSIC(8).EQ.0) RANGE = RANGES                               1203YC96
        S0VS = -RANGE * LOG ((AVS+BVS)/(BVS-AVS))                       1203YC96
C
C     RANGE(L) now has a default of the 3 points fit at LL
C     It can also be input from unit fu50
C
        WRITE(FU6,500)
        WRITE(FU6,600)
C       WRITE(FU6,610) AV*CKCAL, BV*CKCAL, V0A*CKCAL, RANGE
        IF(IUNIT6.EQ.1) THEN                                            0405JZ07
          WRITE(FU6,610) AV*CKCAL, BV*CKCAL, V0A*CKCAL, RANGE/GUFAC6    
          WRITE(FU6,1030) RANGEA/GUFAC6
          WRITE(FU6,1040) RANGES/GUFAC6
        ELSE 
          WRITE(FU6,620) AV*CKCAL, BV*CKCAL, V0A*CKCAL, RANGE/GUFAC6    
          WRITE(FU6,1032) RANGEA/GUFAC6
          WRITE(FU6,1042) RANGES/GUFAC6
        ENDIF                                                           0405JZ07
C       WRITE(FU6,1030) RANGEA                                          1203YC96
C       WRITE(FU6,1040) RANGES                                          0420YC97
1030    FORMAT (1X,'L (calc from 3-point Eckart fit @ HL) = '
     *                                     ,F12.6,' bohr')              1203YC96
1040    FORMAT (1X,'L (calc from 3-point Eckart fit @ LL) = '
     *                                     ,F12.6,' bohr')              0420YC97
1032    FORMAT (1X,'L (calc from 3-point Eckart fit @ HL) = '           0405JZ07
     *                                  ,F12.6,' angstrom') 
1042    FORMAT (1X,'L (calc from 3-point Eckart fit @ LL) = '
     *                                  ,F12.6,' angstrom')             0405JZ07
      ENDIF                                                             0203YC98
      IF (IVICE.EQ.0) THEN                                              1203YC96
        DVR = VRA - VRS
        DVP = VPA - VPS
        DV0 = V0A - V0S
C
C     Determine which function form to use and the parameters  
C     of ZOCVCL
C
        IF ( ABS( (DV0 - DVR) / DV0 ) .LE. EPIS ) THEN
           IF ( ABS( (DVP - DV0) / DV0 ) .LE. EPIS ) THEN
              IVFUN = 0
           ELSE
              DV0 = DV0 + SIGN(2*EPIS*DV0,DVR - DVP) 
              IVFUN = 1
           ENDIF
        ELSE IF ( ABS( (DV0 - DVP) / DV0 ) .LE. EPIS ) THEN
           DV0 = DV0 + SIGN(2*EPIS*DV0,DVP - DVR)
           IVFUN = 1  
C
        ELSE IF ( ( DV0 .GT. DVR .AND. DV0 .GT. DVP ) .OR.
     *        ( DV0 .LT. DVR .AND. DV0 .LT. DVP ) ) THEN
           IVFUN = 1
        ELSE
           IVFUN = 2
        ENDIF
C
        IF ( IVFUN .EQ. 1 ) THEN    
           AV = DVP - DVR
           CV = DVR
           DET = SQRT( ( DV0 - CV ) * ( DV0 - AV - CV ) )
           BV = 2.0D0*DV0 - AV - 2.0D0*CV + 2.0D0*SIGN(DET,DV0 - DVR)
           S0V= -RANGE * LOG( (AV + BV) / (BV - AV) ) 
C
           CV2 = DVP
           AV2 = ( DV0 - DVP ) * EXP(BV2)
C
C     BV1 and BV2 now both have a default value 5.0
C     They can also be inpput from unit fu50
C
           CV1 = DVR 
           AV1 = ( DV0 - DVR ) * EXP(BV1)
C
        ELSE IF ( IVFUN .EQ. 2 ) THEN                    
           DTEM = DV0 - DVP                                             0726WH93
           AV = DVP + 2.0D0 * DTEM - DVR                                0726WH93
           CV = DVR                                                     0726WH93
           DET = SQRT( ( DV0 - CV ) * ( DV0 - AV - CV ) )               0726WH93
           BV = 2.0D0*DV0 - AV - 2.0D0*CV + 2.0D0*SIGN(DET,DV0 - DVR)   0726WH93
           S0V= -RANGE * LOG( (AV + BV) / (BV - AV) )                   0726WH93
           CV2 = DVP                                                    0726WH93
           AV2 = ( DV0 - DVP ) * EXP(BV2)                               0726WH93
           CV1 = DVR                                                    0726WH93
           AV1 = ( DV0 - DVR ) * EXP(BV1)                               0726WH93
        ENDIF      
      ENDIF                                                             1203YC96
C 
C     Determine which function form to use and the parameters
C     of ZOCFRE
        if (lgs2(10).eq.3) then                                         0626YC97 
          DO I = 1,NFRS                                                 0626YC97
              DFRR(I) = 0.0d0                                           0626YC97
              DFRP(I) = 0.0d0                                           0626YC97
              DFRS(I) = 0.0d0                                           0626YC97
          ENDDO                                                         0626YC97
          RETURN                                                        0626YC97
        endif                                                           0626YC97
        if (lgs2(10).eq.0) then                                         03/96/YC
          DO I = 1,NFRS
              DFRR(I) = FRP1A(I)  - FRP1S(I)
              DFRP(I) = FRP2A(I)  - FRP2S(I)
              DFRS(I) = WESADA(I) - WESADS(I)
          ENDDO
        endif                                                           03/96/YC
        if (lgs2(10).eq.1) then                                         03/96/YC
           DO I = 1,NFRS                                                03/96/YC
              if ((FRP1A(I).eq.0).and.(FRP1S(I).eq.0)) then             03/96/YC
                 DFRR(I) = 1                                            03/96/YC
              else                                                      03/96/YC
                 DFRR(I) = (FRP1A(I)/FRP1S(I))                          03/96/YC
              endif                                                     03/96/YC
              if ((FRP2A(I).eq.0).and.(FRP2S(I).eq.0)) then             03/96/YC
                 DFRP(I) = 1                                            03/96/YC
              else                                                      03/96/YC
                 DFRP(I) = (FRP2A(I)/FRP2S(I))                          03/96/YC
              endif                                                     03/96/YC
              if ((WESADA(I).eq.0).and.(WESADS(I).eq.0)) then           03/96/YC
                 DFRS(I) = 1                                            03/96/YC
              else                                                      03/96/YC
                 DFRS(I) = (WESADA(I)/WESADS(I))                        03/96/YC
              endif                                                     03/96/YC
           ENDDO                                                        03/96/YC
        endif                                                           03/96/YC
        if (lgs2(10).eq.2) then                                         03/96/YC
           DO I = 1,NFRS                                                03/96/YC
              if ((FRP1A(I).eq.0).and.(FRP1S(I).eq.0)) then             03/96/YC
                 DFRR(I) = 0.0d0                                        0203YC98
              else                                                      03/96/YC
                 DFRR(I) = log(FRP1A(I)/FRP1S(I))                       03/96/YC
              endif                                                     03/96/YC
              if ((FRP2A(I).eq.0).and.(FRP2S(I).eq.0)) then             03/96/YC
                 DFRP(I) = 0.0d0                                        0203YC98
              else                                                      03/96/YC
                 DFRP(I) = log(FRP2A(I)/FRP2S(I))                       03/96/YC
              endif                                                     03/96/YC
              if ((WESADA(I).eq.0).and.(WESADS(I).eq.0)) then           03/96/YC
                 DFRS(I) = 0.0d0                                        0203YC98
              else                                                      03/96/YC
                 DFRS(I) = log(WESADA(I)/WESADS(I))                     03/96/YC
              endif                                                     03/96/YC
           ENDDO                                                        03/96/YC
        endif                                                           03/96/YC
C
C     Optionally replace the low frequency mode along the
C     MEP with a funtional form
C
      IF (LGSIC(7) .NE. 0 ) THEN
         DO 85 I = 1,LGSIC(7)
            J = ICFR(I)
            DFRR(J) = FRP1A(J) 
            DFRP(J) = FRP2A(J)
            DFRS(J) = WESADA(J)
85       CONTINUE   
      ENDIF   
C 
C     Optionally set the frequency corrections at wells to be zero
C     (Not recommended to be used with the 0-IVTST low frequency method)
C
      IF (LGSIC(11) .EQ. 1) THEN                                        0406WH95
         DO 90 I = 1, NFRS                                              0406WH95
            DFRR(I) = 0.0D0                                             0406WH95
90       CONTINUE                                                       0406WH95
      ENDIF                                                             0406WH95
C
      IF (LGSIC(12) .EQ. 1) THEN                                        0406WH95
         DO 95 I = 1, NFRS                                              0406WH95
            DFRP(I) = 0.0D0                                             0406WH95
95       CONTINUE                                                       0406WH95
      ENDIF                                                             0406WH95
C
      DO 100 I=1,NFRS
         IF ( ABS( (DFRS(I)-DFRR(I)) / DFRS(I) ) .LE. EPIS ) THEN
            IF ( ABS( (DFRP(I)-DFRS(I)) / DFRS(I) ) .LE. EPIS ) THEN
               IFFUN(I) = 0
            ELSE
               DFRS(I) = DFRS(I) + SIGN(2*EPIS*DFRS(I),DFRR(I)-DFRP(I)) 
               IFFUN(I) = 1
            ENDIF
C
         ELSE IF ( ABS( (DFRS(I)-DFRP(I)) / DFRS(I) ) .LE. EPIS ) THEN
            DFRS(I) = DFRS(I) + SIGN(2*EPIS*DFRS(I),DFRP(I)-DFRR(I))
            IFFUN(I) = 1    
C
         ELSE IF ( ( DFRS(I) .GT. DFRR(I) .AND. DFRS(I) .GT. DFRP(I) )
     * .OR. ( DFRS(I) .LT. DFRR(I) .AND. DFRS(I) .LT. DFRP(I) ) ) THEN
            IFFUN(I) = 1
         ELSE
            IFFUN(I) = 2
         ENDIF
C
        IF ( IFFUN(I) .EQ. 1 ) THEN
           AF(I) = DFRP(I) - DFRR(I)
           CF(I) = DFRR(I)
           DET = SQRT( (DFRS(I) - CF(I)) * ( DFRS(I) - CF(I) - AF(I)))
           BF(I) = 2.0D0*DFRS(I) - AF(I) - 2.0D0*CF(I) +
     *             2.0D0*SIGN( DET,DFRS(I)-DFRR(I) )
           S0F(I) = -RANGE * LOG( ( AF(I)+BF(I) ) / ( BF(I)-AF(I) ) )
C
           CF2(I) = DFRP(I)
           BF2(I) = BV2
           AF2(I) = ( DFRS(I) - DFRP(I) ) * EXP(BF2(I))
C
           CF1(I) = DFRR(I)
           BF1(I) = BV1
           AF1(I) = ( DFRS(I) - DFRR(I) ) * EXP(BF1(I))
C
        ELSE IF ( IFFUN(I) .EQ. 2 ) THEN
           AF(I) = ( DFRP(I) - DFRR(I) ) / 2.0D0
           CF(I) = ( DFRP(I) + DFRR(I) ) / 2.0D0
           XX    = ( DFRS(I) - CF(I)   ) / AF(I)
           S0F(I)= -RANGE * LOG( (1.0D0+XX) / (1.0D0-XX) ) / 2.0D0
C
           AF2(I) = AF(I)                                               0726WH93
           CF2(I) = CF(I)                                               0726WH93
           S0F2(I)= S0F(I)                                              0726WH93
C
           AF1(I)  = AF2(I)
           CF1(I)  = CF2(I)
           S0F1(I) = S0F2(I) 
        ENDIF
C
100   CONTINUE
C
      RETURN
C
500   FORMAT(/,1x,66('*'),
     *       /10X,'Zero-order interpolated correction information',
     *       /1X,66('*'))
600   FORMAT(/1X,'In calculating the range parameter:')
610   FORMAT(1X,'A = ',F12.6,' kcal/mol',/1X,'B = ',F12.6,' kcal/mol',
     *      /1X,'V = ',F12.6,' kcal/mol',/1X,'L = ',F12.6,' bohr')
620   FORMAT(1X,'A = ',F12.6,' kcal/mol',/1X,'B = ',F12.6,' kcal/mol',
     *      /1X,'V = ',F12.6,' kcal/mol',/1X,'L = ',F12.6,' angstrom')  0405JZ07
C
      END SUBROUTINE zocpar
C**********************************************************************
C  ZOCPRN
C**********************************************************************
C
      SUBROUTINE zocprn
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : iunit6,gufac6
      use kintcm, only : ivicm
C
C     CALLED BY:
C                 MAIN
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LFINTR,LFINTP,WELLR,WELLP
      DIMENSION TEMFRR(N3TM),TEMFRS(N3TM),TEMFRP(N3TM),
     *          TEMFR1(N3TM),TEMFR2(N3TM)
C
      NFRS = NF(5) 
      NFRR = NF(1) + NF(2)
      NFRP = NF(3) + NF(4)
      I1 = LGSIC(1)/10
      I2 = MOD(LGSIC(1),10)
      LFINTR = I1 .NE. 2
      LFINTP = I2 .NE. 2 
      WELLR  = I1 .EQ. 3                                                0615YC96
      WELLP  = I2 .EQ. 3                                                0615YC96
      IF (LGS(3) .EQ. 0)  WRITE(FU6,400)
C
C     Write energy information
C
      WRITE(FU6,500) 
      WRITE(FU6,510) BARRA*CKCAL, BARRS*CKCAL
      WRITE(FU6,520) ERXN *CKCAL, EPRD *CKCAL
      WRITE(FU6,525) VP1A *CKCAL, VP1S *CKCAL
      WRITE(FU6,526) VP2A *CKCAL, VP2S *CKCAL
C   
C     Write det I information
C
      IF (LGSIC(4) .NE. 0) THEN
         WRITE(FU6,527)
         WRITE(FU6,529)
         WRITE(FU6,530) FMIR1A,FMISPA,FMIP1A
         IF (LGS(6) .EQ. 1) THEN
            WRITE(FU6,531) FMIR2A,FMIP2A
         ELSEIF (LGS(6) .EQ. 2) THEN
            WRITE(FU6,531) FMIR2A
         ELSEIF (LGS(6) .EQ. 3) THEN
            WRITE(FU6,532) FMIP2A
         ENDIF
      ENDIF
C
      WRITE(FU6,528)
      WRITE(FU6,529)
      WRITE(FU6,530) FMOM(1),FMISPS,FMOM(3)
      IF (LGS(6) .EQ. 1) THEN
         WRITE(FU6,531) FMOM(2),FMOM(4)
      ELSEIF (LGS(6) .EQ. 2) THEN
         WRITE(FU6,531) FMOM(2)
      ELSEIF (LGS(6) .EQ. 3) THEN
         WRITE(FU6,532) FMOM(4)
      ENDIF 
      IF (IVICM.NE.3) THEN                                              0203YC98
C
C     Write corrected frequency information, HL
C
C  lgsic(1) is 22 if both reactant side and product side are two
C
        IF (LGSIC(1) .EQ. 22) THEN
           NTOT = NFRS
        ELSE
           NTOT = NFRS + 1
        ENDIF
        NZFRR = NTOT - NFRR
        NZFRP = NTOT - NFRP
        DO I = NTOT - NZFRR + 1, NTOT
           TEMFRR(I) = 0.0d0
        ENDDO
        DO I = NTOT - NZFRP + 1, NTOT
           TEMFRP(I) = 0.0d0
        ENDDO
c
        DO I = 1,NFRR
           TEMFRR(NFRR+1-I) = WERA(I)
        ENDDO
        DO I = 1,NFRP
           TEMFRP(NFRP+1-I) = WERA(I+NFRR)
        ENDDO
        DO I = 1,NFRS
           TEMFRS(NFRS+1-I) = WESADA(I)
           TEMFR1(NFRS+1-I) = FRP1A(I)
           TEMFR2(NFRS+1-I) = FRP2A(I)
        ENDDO
        TEMFRS(NFRS+1) = -ABS(TSWIM)
        TEMFR1(NFRS+1) = FRP1A(NFRS+1)
        TEMFR2(NFRS+1) = FRP2A(NFRS+1)
        WRITE(FU6,535)
        IF (WELLR.and.WELLP) WRITE (FU6,540)                            0615YC96
        IF (.not.WELLR.and.WELLP) THEN                                  0615YC96
             WRITE (FU6,541)                                            0615YC96
             WRITE (FU6,542)                                            0615YC96
        ENDIF                                                           0615YC96
        IF (WELLR.and..not.WELLP) THEN                                  0615YC96
             WRITE (FU6,543)                                            0615YC96
             WRITE (Fu6,544)                                            0615YC96
        ENDIF                                                           0615YC96
        IF (.not.WELLR.and..not.WELLP) THEN                             0615YC96
             WRITE (FU6,545)                                            0615YC96
             WRITE (FU6,546)                                            0615YC96
        ENDIF                                                           0615YC96
        DO  I = 1, NTOT
          IF (LGS(3) .NE. 0) THEN
             WRITE(6,200)I,TEMFRR(I)*AUTOCM,TEMFR1(I)*AUTOCM,
     *       TEMFRS(I)*AUTOCM,TEMFR2(I)*AUTOCM,TEMFRP(I)*AUTOCM
          ELSE
             WRITE(6,200)I,TEMFRR(I)*AUTOCM,TEMFRR(I)*AUTOCM,
     *       TEMFRS(I)*AUTOCM,TEMFRP(I)*AUTOCM,TEMFRP(I)*AUTOCM
          ENDIF
        ENDDO
C
C     Print potential function frequency information, LL
C
        DO I = 1,NFRR
          TEMFRR(NFRR+1-I) = WER(I)
        ENDDO
        DO I = 1,NFRP
          TEMFRP(NFRP+1-I) = WER(I+NFRR)
        ENDDO
        DO I = 1,NFRS
          TEMFRS(NFRS+1-I) = WESADS(I)
          TEMFR1(NFRS+1-I) = FRP1S(I)
          TEMFR2(NFRS+1-I) = FRP2S(I)
        ENDDO
        TEMFRS(NFRS+1) = - WSTAR
        TEMFR1(NFRS+1) = FRP1S(NFRS+1)
        TEMFR2(NFRS+1) = FRP2S(NFRS+1) 
        WRITE(FU6,550)
        IF (WELLR.and.WELLP) WRITE (FU6,540)                            0615YC96
        IF (.not.WELLR.and.WELLP) THEN                                  0615YC96
           WRITE (FU6,541)                                              0615YC96
           WRITE (FU6,542)                                              0615YC96
        ENDIF                                                           0615YC96
        IF (WELLR.and..not.WELLP) THEN                                  0615YC96
           WRITE (FU6,543)                                              0615YC96
           WRITE (FU6,544)                                              0615YC96
        ENDIF                                                           0615YC96
        IF (.not.WELLR.and..not.WELLP) THEN                             0615YC96
           WRITE (FU6,545)                                              0615YC96
           WRITE (FU6,546)                                              0615YC96
        ENDIF                                                           0615YC96
        DO I = 1, NTOT
           IF (LGS(3) .NE. 0) THEN
             WRITE(6,200)I,TEMFRR(I)*AUTOCM,TEMFR1(I)*AUTOCM,
     *       TEMFRS(I)*AUTOCM,TEMFR2(I)*AUTOCM,TEMFRP(I)*AUTOCM
           ELSE
             WRITE(6,200)I,TEMFRR(I)*AUTOCM,TEMFRR(I)*AUTOCM,
     *       TEMFRS(I)*AUTOCM,TEMFRP(I)*AUTOCM,TEMFRP(I)*AUTOCM
           ENDIF
        ENDDO
        IF (LGSIC(7).NE.0) WRITE(FU6,600) (NFRS+1-ICFR(I),I=1,LGSIC(7))
C
        IF (LGS(3) .EQ. 0) RETURN  
C
C     Write the parameters of the energy correction functions
C
        WRITE(FU6,650)
C       IF (LFINTR) WRITE(FU6,651) SP1
C       IF (LFINTP) WRITE(FU6,652) SP2                                  0427WH94
        IF (LFINTR.AND.IUNIT6.EQ.1) WRITE(FU6,651) SP1/GUFAC6           0405JZ07
        IF (LFINTR.AND.IUNIT6.EQ.0) WRITE(FU6,653) SP1/GUFAC6           0405JZ07
        IF (LFINTP.AND.IUNIT6.EQ.1) WRITE(FU6,652) SP2/GUFAC6           0405JZ07
        IF (LFINTP.AND.IUNIT6.EQ.0) WRITE(FU6,654) SP2/GUFAC6           0405JZ07
        IF (IUNIT6.EQ.1) THEN                                           0405JZ07
           WRITE(FU6,655)                           
        ELSE
           WRITE(FU6,656)
        ENDIF                                                           0405JZ07
           WRITE(FU6,660)

        WRITE(FU6,690)IVFUN, AV*CKCAL, BV*CKCAL, CV1*CKCAL, S0V/GUFAC6  0405JZ07
        IF (LFINTR) THEN
          IF (IVFUN .EQ. 1) THEN
C           WRITE(FU6,665)
            IF(IUNIT6.EQ.1) WRITE(FU6,665)                              0405JZ07
            IF(IUNIT6.EQ.0) WRITE(FU6,667)                              0405JZ07
          ELSE
C           WRITE(FU6,666)
            IF(IUNIT6.EQ.1) WRITE(FU6,666)                              0405JZ07
            IF(IUNIT6.EQ.0) WRITE(FU6,668)                              0405JZ07
          ENDIF
          WRITE(FU6,670)
          WRITE(FU6,690)IVFUN, AV1*CKCAL, BV1, CV1*CKCAL, S0V1/GUFAC6   0405JZ07
        ENDIF
        IF (LFINTP) THEN
          IF (IVFUN .EQ. 1) THEN
C           WRITE(FU6,675)
            IF(IUNIT6.EQ.1) WRITE(FU6,675)                              0405JZ07
            IF(IUNIT6.EQ.0) WRITE(FU6,677)                              0405JZ07
          ELSE
C           WRITE(FU6,676)
            IF(IUNIT6.EQ.1) WRITE(FU6,676)                              0405JZ07
            IF(IUNIT6.EQ.0) WRITE(FU6,678)                              0405JZ07
          ENDIF
          WRITE(FU6,680)
          WRITE(FU6,690)IVFUN, AV2*CKCAL, BV2, CV2*CKCAL, S0V2/GUFAC6   0405JZ07
        ENDIF
C
C
        IF (LGSIC(4) .NE. 0) WRITE(FU6,691) FMISPA / FMISPS
C
C     Write the parameters of the frequency correction functions
C
        WRITE(FU6,700)
C       IF (LGS2(10).EQ.0) WRITE (FU6,710)                              0203YC98
C       IF (LGS2(10).EQ.1) WRITE (FU6,711)                              0203YC98
C       IF (LGS2(10).EQ.2) WRITE (FU6,712)                              0203YC98
        IF (LGS2(10).EQ.0.AND.IUNIT6.EQ.1) WRITE (FU6,710)              0405JZ07
        IF (LGS2(10).EQ.0.AND.IUNIT6.EQ.0) WRITE (FU6,810)              0405JZ07
        IF (LGS2(10).EQ.1.AND.IUNIT6.EQ.1) WRITE (FU6,711)              0405JZ07
        IF (LGS2(10).EQ.1.AND.IUNIT6.EQ.0) WRITE (FU6,811)              0405JZ07
        IF (LGS2(10).EQ.2.AND.IUNIT6.EQ.1) WRITE (FU6,712)              0405JZ07
        IF (LGS2(10).EQ.2.AND.IUNIT6.EQ.0) WRITE (FU6,812)              0405JZ07
        WRITE(FU6,715)
        DO I = NFRS,1,-1
          WRITE(FU6,740)NFRS+1-I,IFFUN(I),AF(I)*AUTOCM,BF(I)*AUTOCM,
     *                               CF(I)*AUTOCM,S0F(I)/GUFAC6         0405JZ07
        ENDDO
        IF (LFINTR) THEN
C         WRITE(FU6,720)
          IF(IUNIT6.EQ.1)  WRITE(FU6,720)                               0405JZ07
          IF(IUNIT6.EQ.0)  WRITE(FU6,820)                               0405JZ07
          WRITE(FU6,725)
          DO I = NFRS,1,-1
            IF(IFFUN(I).EQ.1) THEN                                      0405JZ07
              WRITE(FU6,740)NFRS+1-I,IFFUN(I),AF1(I)*AUTOCM,BF1(I),
     *                               CF1(I)*AUTOCM,S0F1(I)/GUFAC6       0405JZ07
            ELSEIF (IFFUN(I).EQ.2) THEN
              WRITE(FU6,740)NFRS+1-I,IFFUN(I),AF1(I)*AUTOCM,
     *                 BF1(I)/GUFAC6,CF1(I)*AUTOCM,S0F1(I)/GUFAC6
            ENDIF                                                       0405JZ07
          ENDDO
        ENDIF
        IF (LFINTP) THEN
C         WRITE(FU6,730)
          IF(IUNIT6.EQ.1) WRITE(FU6,730)                                0405JZ07
          IF(IUNIT6.EQ.0) WRITE(FU6,830)                                0405JZ07
          WRITE(FU6,735)
          DO I = NFRS,1,-1
            IF(IFFUN(I).EQ.1) THEN                                      0405JZ07
              WRITE(FU6,740)NFRS+1-I,IFFUN(I),AF2(I)*AUTOCM,BF2(I),
     *                               CF2(I)*AUTOCM,S0F2(I)/GUFAC6       0405JZ07
            ELSEIF (IFFUN(I).EQ.2) THEN
              WRITE(FU6,740)NFRS+1-I,IFFUN(I),AF2(I)*AUTOCM,
     *                 BF2(I)/GUFAC6,CF2(I)*AUTOCM,S0F2(I)/GUFAC6
            ENDIF
          ENDDO
        ENDIF
C
      ENDIF                                                             0203YC98
      RETURN
C
 200  FORMAT(1X,I4,5F12.2) 
 400  FORMAT(/10X,'Zero-order interpolated correction information',
     *       /1X,66('*'))
 500  FORMAT(/1X,38(' '),'      Corrected','    Pot. function')  
 510  FORMAT(1X,'Classical barrier height (kcal/mol) : ',2F15.4)
 520  FORMAT(1X,'Energy of reaction       (kcal/mol) : ',2F15.4)   
 525  FORMAT(1X,'Energy of point 1        (kcal/mol) : ',2F15.4)
 526  FORMAT(1X,'Energy of point 2        (kcal/mol) : ',2F15.4)
 527  FORMAT(/1X,'Product of principal moments of inertia in a.u. ',
     *           '(corrected)')
 528  FORMAT(/1X,'Product of principal moments of inertia in a.u. ',
     *           '(potential function)')
 529  FORMAT(1X ,'      Reactant        Saddle       Product')          1108WH93
 530  FORMAT(1X,1P,3E14.4)
 531  FORMAT(1X,1P,E14.4,14X,E14.4)                                     0519WH94
 532  FORMAT(1X,1P,28X,E14.4)                                           0519WH94
 535  FORMAT(/1X,'Frequencies (corrected) in cm**-1, High Level')       0615YC96
 540  FORMAT(1X,'Mode    Reactant     Point 1      Saddle',             0615YC96
     *              '     Point 2     Product')                         0615YC96
 541  FORMAT(1X,'                      Sorted            ',             0615YC96
     *              '                        ')                         0615YC96
 542  FORMAT(1X,'Mode    Reactant     Reactant     Saddle',             0615YC96
     *              '     Point 2     Product')                         0615YC96
 543  FORMAT(1X,'                                        ',             0615YC96
     *              '     Sorted             ')                         0615YC96
 544  FORMAT(1X,'Mode    Reactant     Point 1      Saddle',             0615YC96
     *              '     Product     Product')                         0615YC96
 545  FORMAT(1X,'                      Sorted            ',             0615YC96
     *              '     Sorted             ')                         0615YC96
 546  FORMAT(1X,'Mode    Reactant     Reactant     Saddle',             0615YC96
     *              '     Product     Product')                         0615YC96
 550  FORMAT(/1X,'Frequencies (potential function) in cm**-1, Low',     0615YC96
     *              ' Level ')
 600  FORMAT(/1X,'GTS frequencies that are interpolated directly are',
     *       /1X,'mode: ',10I4,(/7X,10I4))
 650  FORMAT(/1X,'In the correction function for VMEP')
 651  FORMAT(/1X,'SP1 = ',F10.4,' bohr')
 652  FORMAT(/1X,'SP2 = ',F10.4,' bohr')
 653  FORMAT(/1X,'SP1 = ',F10.4,' angstrom')                            0405JZ07
 654  FORMAT(/1X,'SP2 = ',F10.4,' angstrom')                            0405JZ07
 655  FORMAT(/1X,'A, B, and C are in kcal/mol; S0 is in bohr.')
 656  FORMAT(/1X,'A, B, and C are in kcal/mol; S0 is in angstrom.')     0405JZ07
 660  FORMAT(1X, 'Function              A              B              C'
     *      ,       '             S0')
 665  FORMAT(/1X,'AR and CR are in kcal/mol; BR is unitless;',
     *       ' S0R is in bohr.')
 666  FORMAT(/1X,'AR and CR are in kcal/mol; BR and S0R are in bohr.')
 667  FORMAT(/1X,'AR and CR are in kcal/mol; BR is unitless;',
     *       ' S0R is in angstrom.')                                    0405JZ07
 668  FORMAT(/1X,'AR and CR are in kcal/mol; BR and S0R are in ', 
     *'angstrom.')                                                      0405JZ07
 670  FORMAT(1X, 'Function             AR             BR             CR'
     *      ,       '            S0R')
 675  FORMAT(/1X,'AP and CP are in kcal/mol; BP is unitless;',
     *       ' S0P is in bohr.')
 676  FORMAT(/1X,'AP and CP are in kcal/mol; BP and S0P are in bohr.')
 677  FORMAT(/1X,'AP and CP are in kcal/mol; BP is unitless;',
     *       ' S0P is in angstrom.')                                    0405JZ07
 678  FORMAT(/1X,'AP and CP are in kcal/mol; BP and S0P are in ',
     * 'angstrom.')                                                     0405JZ07
 680  FORMAT(1X, 'Function             AP             BP             CP'
     *      ,       '            S0P')
 690  FORMAT(1X,I4,4X,4E15.4)
 691  FORMAT(/1X,'Alpha factor for the correction of Det I = ',F10.6)
 696  FORMAT(1X,'Function             AP             BP             CP')
 697  FORMAT(/1X,'A and C are in atomic unit for det I; ',
     *       ' B and S0 are in bohr.')
 698  FORMAT(1X,'Function              A              B              C',
     *             '             S0')
 699  FORMAT(1X,I4,4X,1P,4E15.4)
 700  FORMAT(/1X,'In the correction functions for frequencies:') 
 710  FORMAT(1X,'A, B, and C are in cm**-1; S0 is in bohr.')
 810  FORMAT(1X,'A, B, and C are in cm**-1; S0 is in angstrom.')        0405JZ07
 711  FORMAT(1X,'A, B, and C are unitless (ratio of freq.);',           0203YC98
     *          ' S0 is in bohr.')                                      0203YC98
 811  FORMAT(1X,'A, B, and C are unitless (ratio of freq.);',
     *          ' S0 is in angstrom.')                                  0405JZ07
 712  FORMAT(1X,'A, B, and C are unitless (log of ratio of freq.);',    0203YC98
     *          ' S0 is in bohr.')                                      0203YC98
 812  FORMAT(1X,'A, B, and C are unitless (log of ratio of freq.);',
     *          ' S0 is in angstrom.')
 715  FORMAT(1X,'Mode Function              A              B',
     * '              C             S0')
 720  FORMAT(/1X,'AR and CR are in cm**-1; S0R is in bohr;'
     * /1X,'BR is unitless (Function = 1) or in bohr (Function = 2).')
 820  FORMAT(/1X,'AR and CR are in cm**-1; S0R is in angstrom;'         0405JZ07
     */1X,'BR is unitless (Function = 1) or in angstrom (Function = 2).'
     *)
 725  FORMAT(1X,'Mode Function             AR             BR',
     * '             CR            S0R')
 730  FORMAT(/1X,'AP and CP are in cm**-1; S0P is in bohr;'
     *  /1X,'BP is unitless (Function = 1) or in bohr (Function = 2).')
 830  FORMAT(/1X,'AP and CP are in cm**-1; S0P is in angstrom;'         0405JZ07
     */1X,'BP is unitless (Function = 1) or in angstrom (Function = 2).'
     *)
 735  FORMAT(1X,'Mode Function             AP             BP',
     * '             CP            S0P')
 740  FORMAT(1X,I3,2X,I4,4X,1P,4E15.4)
C
      END SUBROUTINE zocprn
C
C***********************************************************************
C  ZOCUPD
C***********************************************************************
C
      SUBROUTINE zocupd
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : iunit6,gufac6,ivice
      use kintcm, only : ibathm
c     use cm, only : frict
c     use keyword_interface
c     use kintcm
      use cm
C
C     This routine update the information of each equilibrium configurations,
C     and of each GTS to include the zero order correction.
C
C     The following data are updated:
C
C          VAR,VAP,FMOM,FMITS,WSTAR,
C          WER,WETS,VCLAS,VADIB
C
C     CALLED BY:
C                MAIN
C     CALLS:
C            ZOCVCL,ZOCFRE,UPDMI
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION FR(N3TM),IORDER(N3TM)
      DIMENSION SSPMX(3),VSPMX(3)                                       0203YC98
      IF (LGS(3) .EQ. 0) THEN                                           0801WH94
         LSAVE = 1                                                      0801WH94
         NSHLF = 1                                                      0801WH94
      ENDIF        
      call zucupd_mem
C
      IF (IVICE.NE.2) THEN                                              0824YC98
         WRITE(FU6,1000)                                                1108WH93
      ELSE
         WRITE(FU6,1001)
      ENDIF
C
C-----------------------------------------------------------------------
C updates EPRD, WER, VAR, VAP
C 
      EPRD = ERXN
      IDUMMY = 0
      VAR = 0.D0
      VAP = EPRD
      IF (IVICE.NE.2) THEN                                              0203YC98
          WSTAR = ABS(TSWIM)                                            1104WH92
      ELSE                                                              0203YC98
        CALL ZSPMEP (VP1A,BARRA,VP2A,VP1S,BARRS,VP2S)                   0203YC98
      ENDIF                                                             0203YC98
      DO 50 IRE = 1, 4
         IF (I .LE. 2) THEN                                             0503WH95
            VI = 0.0D0                                                  0503WH95
         ELSE                                                           0503WH95
            VI = EPRD                                                   0503WH95
         ENDIF                                                          0503WH95
         DEI = DEMIN - VI                                               0503WH95
         NFREQ = NF(IRE)
         ISHFT = 3*NRATOM(IRE) - NFREQ
         VAD = 0.D0
         IF (NFREQ.NE.0) THEN
            DO 60 IFRE = 1, NFREQ
               IDUMMY = IDUMMY + 1
               WER(IDUMMY) = WERA(IDUMMY)
               FREQ(IFRE+ISHFT) = WERA(IDUMMY)
               MODE(IFRE) = MODER(IRE,IFRE)
               ANHRM(IFRE) = XER(IDUMMY)
               IF (MODE(IFRE) .EQ. 1)
     *            ANHRM(IFRE) = FREQ(IFRE+ISHFT)/(4.0D0*DEI)            0503WH95
               AB(IFRE) = Y00R(IDUMMY)
60          CONTINUE
            CALL ZEROPT(-IRE)
         ENDIF
c
c  add in the contribution of the bath mode in zero point energy
c
         IF (ibathm.eq.1) then                                          0317Yc99
            VSOL = 0.5d0*(PI/(4.0d0*FRICT))                             0317Yc99
         ENDIF                                                          0317Yc99
         IF (IRE.LE.2) THEN
            IF (IRE.EQ.1.AND.IBATHM.EQ.1) THEN
              write (6,*) 'Contribution of solvent coordinate ',
     >              ' to the reactant ZPE ',VSOL*CKCAL,' kcal/mol.'
              VAR = VAR + VAD + VSOL
            ELSE
              VAR = VAR + VAD
            ENDIF
         ELSE
            IF (IRE.EQ.3.AND.IBATHM.EQ.1) THEN
              write (6,*) 'Contribution of solvent coordinate ',
     >              ' to the product ZPE ',VSOL*CKCAL,' kcal/mol.'
              VAP = VAP + VAD + VSOL
            ELSE
              VAP = VAP + VAD
            ENDIF
         ENDIF
c
50    CONTINUE
C
      IF (LGSIC(10) .EQ. 1) THEN                                        0722WH93
         WRITE(FU6,1100)                                                0722WH93
         CALL IVTST0                                                    0722WH93
         CALL ENROUT(VAR,VAP,EPRD,IFRFAC,FREQFAC)                       0808JC00
         CALL SENOUT(EPRD,V,VAR,VAP,VAD,IFRFAC,FREQFAC)                 0808JC00
         CALL MEPOUT                                                    0722WH93
         RETURN                                                         0722WH93
      ENDIF                                                             0722WH93
C
C-----------------------------------------------------------------------
C updates correction term to the classical potential in the non-adiabatic
C region for the LCG3 calculations
C
      VZOCLC = BARRA - VCLAS(NSHLF)
C
C-----------------------------------------------------------------------
C updates VCLAS, VADIA, MUeff, Det I
C
      N3M7 = NF(5)
      ISHFT = N3 - N3M7
      NARR = LGS(5) - 20
      IRR = 1
      DO 70 IMM = 1, N3
         MODE(IMM) = MODETS(1,IMM)
70    CONTINUE
      DO 100 ISS = 1,LSAVE
         CDSCMU(ISS) = ZOCMCD(ISS)
100   CONTINUE 
C
      IF (LGSIC(4) .NE. 0) CALL UPDMI
      IF (LGSIC(9) .NE. 99) CALL UPDHR
C
      LTOTLE = LSAVE
      IF (LGS(3).LT.0) LTOTLE = (LSAVE + 1)/2
      DO 110 ISS = 1,LTOTLE
         SDUMMY = SSUBI(ISS)
         IF (LGS(3) .EQ. 0) THEN
            VCLAS(ISS) = BARRA
         ELSE
            VCLAS(ISS) = VCLAS(ISS) + ZOCVCL(SDUMMY)
         ENDIF
c        IF (NARR.GT.1.AND.IRR.LT.NARR.AND.SSUBI(ISS).GE.
c    1   nint(SRARR(IRR))) 
c    *   THEN
         IF (NARR.GT.1.AND.IRR.LT.NARR.AND.SSUBI(ISS).GE.
     1   SRARR(IRR)) 
     *   THEN
            IRR = IRR + 1
            DO 120 IMM = 1, N3
               MODE(IMM) = MODETS(IRR,IMM)
120         CONTINUE            
         ENDIF
         V = VCLAS(ISS)
         DEI = DEMIN - V                                                0503WH95
C  
C        Determine the order of GTS frequencies at this save point
C
         DO 125 I = 1, N3M7                                             1020WH93
            FR(I) = WETS(I,ISS)                                         1020WH93
125      CONTINUE                                                       1020WH93
         CALL FRSORT(N3M7,FR,.TRUE.,IORDER)                             1020WH93
C
         IF (LGS(3).NE.3) THEN
            DO 150 IMM = 1, N3M7
               AB(IMM) = Y0TS(IMM,ISS)
               ANHRM(IMM) = XETS(IMM,ISS)
               IF (LGS(3) .EQ. 0) THEN
                  WETS(IMM,ISS) = WESADA(IMM)
               ELSE 
                  IF (LGSIC(2) .EQ. 0) THEN                             1020WH93
C
C     Add corrections to the frequencies according to accending order
C
                    if (LGS2(10).EQ.0) then
                       WETS(IORDER(IMM),ISS) = WETS(IORDER(IMM),ISS) +  1020WH93
     *                                       ZOCFRE(IMM,SDUMMY)         1020WH93
                    endif
                    if ((WETS(IORDER(IMM),ISS).gt.0).and.               03/96/YC
     *                  (LGS2(10).EQ.1)) then                           03/96/YC
                         WETS(IORDER(IMM),ISS)=WETS(IORDER(IMM),ISS) *  03/96/YC
     *                                      (ZOCFRE(IMM,SDUMMY))        03/96/YC
                    endif                                               03/96/YC
                    if ((WETS(IORDER(IMM),ISS).gt.0).and.               03/96/YC
     *                  (LGS2(10).EQ.2)) then                           03/96/YC
                         WETS(IORDER(IMM),ISS)=WETS(IORDER(IMM),ISS) *  03/96/YC
     *                                        EXP(ZOCFRE(IMM,SDUMMY))   03/96/YC
                   endif
                  ELSE                                                  1020WH93
C     But in rare occasion, we use the input order to correct the frequency
C
                    if (LGS2(10).EQ.0) then                             031596YC
                      WETS(IMM,ISS) = WETS(IMM,ISS) +                   031596YC
     *                                             ZOCFRE(IMM,SDUMMY)   031596YC
                    endif                                               031596YC
                    if (LGS2(10).EQ.1) then                             031596YC
                      WETS(IMM,ISS) = WETS(IMM,ISS) *                   031596YC
     *                                             ZOCFRE(IMM,SDUMMY)   031596YC
                    endif                                               031596YC
                    if (LGS2(10).EQ.2) then                             031596YC
                      WETS(IMM,ISS) = WETS(IMM,ISS) *                   031596YC
     *                                        EXP(ZOCFRE(IMM,SDUMMY))   031596YC
                    endif                                               031596YC
                  ENDIF
               ENDIF
150         CONTINUE
C
C       Interpolate the low frequency modes directly
C
            IF (LGS(3) .NE. 0) THEN 
               DO 152 I = 1,LGSIC(7) 
                  IMM = ICFR(I)
                  IF (LGSIC(2) .EQ. 0) THEN                             1020WH93
                     WETS(IORDER(IMM),ISS) = ZOCFRE(IMM,SDUMMY)         1020WH93
                  ELSE                                                  1020WH93
                     WETS(IMM,ISS) = ZOCFRE(IMM,SDUMMY)                 1020WH93
                  ENDIF  
152            CONTINUE     
            ENDIF
C      
            DO 155 IMM = 1,N3M7
               FREQ(IMM+ISHFT) = WETS(IMM,ISS)
               IF (MODE(IMM) .EQ. 1)
     *            ANHRM(IMM) = FREQ(IMM+ISHFT)/(4.0D0*DEI)              0503WH95
155         CONTINUE
C
            CALL ZEROPT(2)
            VADIB(ISS) = VAD
         ENDIF
110   CONTINUE
      IF (LGS(3).LT.0) THEN
         DO 160 ISS = 1,LTOTLE-1
            ISI = 2*LTOTLE-ISS
            VCLAS(ISI) = VCLAS(ISS)
            VADIB(ISI) = VADIB(ISS)
            DO 170 IMM = 1, N3M7
               WETS(IMM,ISI) = WETS(IMM,ISS)
 170        CONTINUE
 160     CONTINUE
      ENDIF
      V   = VCLAS(NSHLF)
      VAD = VADIB(NSHLF)
C-----------------------------------------------------------------------
C writes the corrected energetics
C     
c      CALL ZOCAST
c
C     FIND THE MAXIMUM VMEP AT HIGH LEVEL AND POINT BEFORE AND AFTER    0203YC98
C
       IF (IVICE.EQ.2) THEN                                             0203YC98
C Corrected by Nuria Gonzalez-Garcia to fix ISPE bug with fu31 files    1126NG04
         VHLMAX = -100.0d0                                              1126NG04
           DO  I = 1, LSAVE                                             0203YC98
              IF (VCLAS(I).GT.VHLMAX) THEN                              0203YC98
                 VHLMAX = VCLAS(I)                                      0203YC98
                 IMAXX = I                                              0203YC98
              ENDIF                                                     0203YC98
           ENDDO                                                        0203YC98
           DO I = 1,3                                                   0203YC98
              SSPMX(I) = SSUBI(IMAXX-2+I)                               0203YC98
              VSPMX(I) = VCLAS(IMAXX-2+I)                               0203YC98
           ENDDO                                                        0203YC98
           CALL TREPT (1,SSPMX,VSPMX,SSPMAX,VSPMAX)                     0203YC98
       ENDIF                                                            0203YC98
C
C     SCALE THE BARRIER HIGHT                                           0203YC98
C
c       IF (SPFAC.NE.0.0d0) THEN                                         0203YC98
c         SPFAC = SPFAC / VSPMAX                                         0203YC98
c         WRITE (FU6,*) 'SPICS - SCALE WITH FACTOR ', SPFAC              0203YC98
c         DO I = 1, LSAVE                                                0203YC98
c           VCLAS(I) = SPFAC * VCLAS(I)                                  0203YC98
c         ENDDO                                                          0203YC98
c         WRITE (FU6,2060) SSPMAX, SPFAC*VSPMAX*CKCAL                    0203YC98
c       ELSE                                                             0203YC98
C        WRITE (FU6,2060) SSPMAX,VSPMAX*CKCAL                           0203YC98
         IF(IUNIT6.EQ.1) WRITE(FU6,2060) SSPMAX/GUFAC6,VSPMAX*CKCAL     0405JZ07
         IF(IUNIT6.EQ.0) WRITE(FU6,2070) SSPMAX/GUFAC6,VSPMAX*CKCAL     0405JZ07
c       ENDIF                                                            0203YC98
2060   FORMAT (/,1X,'DL-Max of VMEP occurs at s = s*VMEP =',F8.4,       0203YC98
     *           ' (bohr)', ' with VMEP =',F8.4)                        0203YC98
2070   FORMAT (/,1X,'DL-Max of VMEP occurs at s = s*VMEP =',F8.4,       0405JZ07
     *           ' (angstrom)', ' with VMEP =',F8.4)
c
c put IVTSTM through hooks, the fast way is to rearrange the arrays     0202YC98
c into the format we want as in fu30/40 then try the routine calls      0202YC98
c
      if (lgs(12).ne.0) CALL IVTMH(2,0)                                 0202YC98
c
      CALL ENROUT(VAR,VAP,EPRD,IFRFAC,FREQFAC)                          0808JC00
      CALL SENOUT(EPRD,V,VAR,VAP,VAD,IFRFAC,FREQFAC)                    0808JC00
C
C     Print new MEP information to FU6
C
      IF (LGS(3) .NE. 0) CALL MEPOUT
C
      RETURN
C
1000  FORMAT(//1X,78('*'),/1X,'The following output refers to the',
     * ' VTST-IOC calculation.',/1X,79('*'))
1001  FORMAT(//1X,78('*'),/1X,'The following output refers to the',
     * ' VTST-ISPE calculation.',/1X,79('*'))
1100  FORMAT(//1X,'******* VTST-IC CALCULATION *******')
C
      END SUBROUTINE zocupd
C
C**********************************************************************
C  ZOCVCL
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION zocvcl(SMEP)
      use perconparam; use cm
c     use keyword_interface, only : ivice
      use keyword_interface
c     use rate_const, only : ivfun,barra,lgsic,av,bv,cv,s0v
      use rate_const
      use efmain_mod, only : wstar 
C
C     CALLED BY:
C               ZOCUPD
C
C     THIS FUNCTION CALCULATES THE CORRETION FOR VMEP
C 
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      LOGICAL LINF,LFINTR,LFINTP
C
      I1 = LGSIC(1)/10
      I2 = MOD(LGSIC(1),10)
      LFINTR = I1 .NE. 2
      LFINTP = I2 .NE. 2
      LINF = ( .NOT. LFINTR .AND. SMEP.LE.0.0D0 .OR.
     *         .NOT. LFINTP .AND. SMEP.GE.0.0D0)
C
C  Vmep(DL) = Vmep(LL) + (Veckart(HL)-Veckart(LL))
C
      IF (IVICE.EQ.1) THEN                                              1203YC96
         ZOCVCL= ECKART(AV,BV,CV,S0VA,RANGEA,SMEP)                      1203YC96
     *          - ECKART(AVS,BVS,CVS,S0VS,RANGE,SMEP)                   1203YC96
      ELSE IF (IVICE.EQ.2) THEN                                         0203YC98
            CALL SPLSPV(IREX,NSPIC,SPICS,SPICV,                         0203YC98
     *           SMEP,ZOCVCL,BARRS,VP1S,VP2S,WSTAR,REDM,                0203YC98
     *           TENSION)                                               0911JZ08
       write(6,*) 'ZOCVCL =',ZOCVCL
      ELSE                                                              1203YC96
        IF (IVFUN .EQ. 0) THEN
          ZOCVCL = 0.0D0
        ELSE IF (LINF) THEN
           IF (IVFUN.EQ.1) THEN
              ZOCVCL = ECKART(AV,BV,CV,S0V,RANGE,SMEP)
           ELSE IF (IVFUN.EQ.2) THEN
              IF (SMEP .LE. 0.0D0) THEN
                 ZOCVCL = ECKART(AV,BV,CV,S0V,RANGE,SMEP)
              ELSE
                 DV0 = BARRA - BARRS
                 ZOCVCL = 2.0D0 * DV0 - ECKART(AV,BV,CV,S0V,RANGE,SMEP)
              ENDIF
           ENDIF
        ELSE IF ( LFINTR .AND. SMEP .LT. 0.0D0 ) THEN
              ZOCVCL = COG(AV1,BV1,CV1,SP1,SMEP)
        ELSE IF ( LFINTP .AND. SMEP .GE. 0.0D0 ) THEN
              ZOCVCL = COG(AV2,BV2,CV2,SP2,SMEP)
        ENDIF
      ENDIF
C
      RETURN
C
      END function zocvcl
C
************************************************************************
C  VALVAG
C***********************************************************************
C
      FUNCTION valvag(ADIR)                                             01/97JVF
C
C THIS SUBPROGRAM CALCULATES THE VALUE OF VAG FOR A GIVEN PROJECTION
C DIRECTION ADIR GIVEN X,DX,V AND F
C
C THE FUNCTION CALCULATES THE PROJECTED HESSIAN AND THE PROJECTED GRADIENT 
C USING ADIR. ONCE WE HAVE THE PROJECTED QUANTITIES THE FUNCTION PERFORMS 
C A MINIMIZATION IN THE CONSTRAINED PROJECTED HYPERSURFACE ASSUMING 
C THAT WE ARE CLOSE TO THE ACTUAL MEP AND THE POTENTIAL IN THIS 
C REGION IS QUADRATIC
C
C     CALLED BY:
C                FDIAG3
C 
      use common_inc
      use perconparam, only : n3tm

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      COMMON /posic/ is
C     the line above was commented because posic or is is not used.
C
      DIMENSION ADIR(N3TM),SCDX(N3TM),SCF(N3TM,N3TM)
      DIMENSION GPRJ(N3TM),DESP(N3TM)
      DIMENSION FGUARD(N3TM,N3TM)
      DIMENSION SCR2(N3TM),SCR3(N3TM),FRQEN(N3TM)
      DIMENSION A(N3TM,N3TM),B(N3TM,N3TM)
      DIMENSION GGRAD(N3TM),IORDER(N3TM)
      LOGICAL LREORD
C
      if(.not.allocated(xnou)) allocate(xnou(n3tm))
     
C
C     INITIALIZE VECTORS
C
      SCV=V
      NEND=N3
      DO 10 I=1,N3TM
         XNOU(I)=0.0D0
         GPRJ(I)=0.0D0
         GGRAD(I)=0.0D0
         SCR3(i)=0.0D0
         DO 20 J=1,N3TM
            FGUARD(I,J)=F(I,J)
            A(I,J)=0.0D0
            B(I,J)=0.0D0
 20      CONTINUE
 10   CONTINUE
C
C     ANGLE BETWEEN DX AND ADIR
C
      CALL ANGLV (DX,ADIR,XXX10,NEND)

C
C     ADIR MUST BE NORMALIZED
C
      SUMA=0.0D0
      DO 6 I=1,NEND
         SUMA=SUMA+ADIR(I)*ADIR(I)
6     CONTINUE
      DO 7 I=1,NEND
         ADIR(I)=ADIR(I)/DSQRT(SUMA)
7     CONTINUE
     
C
C PROJECT ADIR, TRANSLATIONS AND ROTATIONS OUT OF THE HESSIAN MATRIX
C
C SCDX WILL KEEP THE ACTUAL GRADIENT
C
      DO 8 I=1,NEND
         SCDX(I)=DX(I)
         DX(I)=ADIR(I)
 8    CONTINUE
      CALL PROJCT
      DO 9 I=1,NEND
         DO 9 J=1,NEND
            SCF(I,J)=F(I,J) 
 9    CONTINUe
C
C DIAGONALIZE THE PROJECTED MATRIX
C
      CALL RSPDRV (N3TM,NEND,SCF,FREQ,1,a,SCR3,SCR2,IERR)
C
C RESTORE ACTUAL GRADIENT
C
      DO 11 I=1,NEND
         DX(I)=SCDX(I)
 11   CONTINUE
C
C     FORM THE PROJECTED GRADIENT WHICH WILL BE ORTHOGONAL TO THE ADIR
C     DIRECTION AND ALSO TO ROTATIONS AND TRANSLATIONS
C     GPRJ=PROJ*DX  WHERE PROJ IS THE MATRIX OBTAINED IN PROJCT
C
      DO 12 I=1,NEND
      SUM=0.0D0
         DO 13 J=1,NEND
            SUM=SUM+PROJ(I,J)*DX(J)
 13      CONTINUE
            GPRJ(I)=SUM
 12   CONTINUE
C
C     IF WE CONSIDER PERFECTLY QUADRATIC THE INTERVAL WHICH CONTAINS
C     X (THE GEOMETRY THAT COMES FROM FDIAG)
C     XNOU (THE GEOMETRY THAT GIVES THE MINIMUM ENERGY IN THE PROJECTED PLANE)
C     THEN, APLYING THE TAYLOR SERIE
C     V(XNOU)=V(X)+(XNOU-X)GPRJ+(1/2)*(XNOU-X)FPRJ(XNOU-X)
C     IT IS CLEAR THAT THE MINIMUM CAN BE COMPUTED BY:
C          XNOU=X-FPRJ^(-1)GPRJ 
C     BUT FPRJ IS SINGULAR. ACTUALLY, WHAT WE DO IS:
C          LT*DESP=-LAMB^(-1)*LT*GPRJ
C     HAVING DELETED THE ROWS AND COLUMNS EQUAL TO ZERO IN ALL THE VECTORS
C     AND MATRICES
C
C     USE THE MATRIX OBTAINED FROM THE DIAGONALIZATION TO DO AN ORTHOGONAL
C     TRANSFORMATION OF THE GRADIENT AND POSITION VECTORS 
C     THE F MATRIX IS GOING TO BE TRANSFORMED BY REPLACING IT WITH
C     THE EIGENVALUES MATRIX
C
C     CALCULATE FREQUENCIES FROM FORCE CONSTANTS
C
      DO 16 I = 1, NEND
         IF (FREQ(I) .NE. 0.0D0) THEN
            FRQEN(I)=FREQ(I)
            FREQ(I) = SQRT(ABS(FREQ(I)/REDM))*SIGN(1.0D0,FREQ(I))
         ENDIF
         INTOUT(I) = 0
 16   CONTINUE

C
C     SORT THE ABSOLUTE VALUE OF FREQUENCIES IN ACCENDING ORDER
C
      ISHFT = NEND - 7
C
      LREORD = .FALSE.
      DO 80 I = 1,NEND
         IORDER(I) = I
80    CONTINUE
      DO 100 I = 1, NEND
         DO 90 J = I+1,NEND
            IF ( ABS(FREQ(J)).GT.ABS(FREQ(I)).AND.J.GT.ISHFT ) THEN
               ITEMP = IORDER(I)
               IORDER(I) = IORDER(J)
               IORDER(J) = ITEMP
               TEMPX = FREQ(I)
               FREQ (I) = FREQ(J)
               FREQ (J) = TEMPX
               TEMPX=FRQEN(I)
               FRQEN(I)=FRQEN(J)
               FRQEN(J)=TEMPX
               LREORD = .TRUE.
            ENDIF
90       CONTINUE 
100   CONTINUE 
      DO 101 I=1,ISHFT
         DO 91 J=I+1,ISHFT
            IF (FREQ(J).gt.FREQ(I)) THEN
               ITEMP=IORDER(I)
               IORDER(I)=IORDER(J)
               IORDER(J)=ITEMP
               TEMPX=FREQ(I)
               FREQ(I)=FREQ(J)
               FREQ(J)=TEMPX
               TEMPX=FRQEN(I)
               FRQEN(I)=FRQEN(J)
               FRQEN(J)=TEMPX
               LREORD=.TRUE.
             END IF
 91      CONTINUE
 101   CONTINUE
C
C     ACCORDING TO THE SORTING ABOVE, REORDER THE EIGENVECTOR ARRAY,
C     AND THE PROJECTED GRADIENT VECTOR
C
      IF (LREORD) THEN
         DO 150 I = 1, NEND
            NEW = IORDER(I)
            DO 140 J = 1, NEND
               B(J,I) = A(J,NEW)
140         CONTINUE
c           SCR2(I)=GPRJ(NEW)
150      CONTINUE
         DO 160 I = 1, NEND
c           GPRJ(I)=SCR2(I) 
            DO 160 J = 1, NEND
               A(J,I) = B(J,I)
160         CONTINUE
      ENDIF
C
C GGRAD WILL CONTAIN THE NON-TRANSFORMED GRADIENT
C
      DO 14 I = 1, NEND
            GGRAD(I)=GPRJ(I)
            SCR2(I) = 0.0D0
            DO 14 K = 1, NEND
               SCR2(I) = SCR2(I)+A(K,I)*GPRJ(K)
  14  CONTINUE
      DO 15 I=1,NEND
         GPRJ(I)=SCR2(I)
  15  CONTINUE
C
C     COMPUTE THE PRODUCT -LT*FP*L*LT*GPRJ=rEM
C
      DO 17 I=1,NEND
         IF (I.GT.ISHFT) THEN
            DESP(I)=0.0D0
         ELSE
            DESP(I)=-(1./DABS(FRQEN(I)))*GPRJ(I)
         END IF
 17   CONTINUE
C
C     COMPUTE THE DISPLACEMENT IN THE FORMER COORDINATES
C
      DO 142 I = 1, NEND
            SCR2(I) = 0.0D0
            DO 142 K = 1, NEND
               SCR2(I) = SCR2(I)+A(I,K)*DESP(K)
  142 CONTINUE
      DO 18 I=1,NEND
         DESP(I)=SCR2(I)
 18   CONTINUE
      DESPMAG = 0.0D0
      DO 50 I = 1, NEND
         DESPMAG = DESPMAG + DESP(I) * DESP(I)
 50   CONTINUE
      DESPMAG = DSQRT(DESPMAG)
C
      DO 19 I=1,NEND
         XNOU(I) = DESP(I)+X(I)
 19   CONTINUE
C
C     CALCULATE THE NEW VALUE FOR THE ENERGY
C
C     QUADRATIC TERM

      DO 500 I=1,NEND
         DO 500 J=1,NEND
            SUM = 0.0D0
            DO 510 K=1,NEND
               SUM=SUM+F(I,K)*DESP(K)
 510        CONTINUE
            SCR2(I)=SUM
 500  CONTINUE
      SUM2=0.0D0
      DO 600 I=1,NEND
         SUM2=SUM2+DESP(I)*SCR2(I)
 600  CONTINUE
      SUM2=0.5d0*SUM2
C
C     LINEAR TERM
C
      SUM3=0.0D0
      DO 700 I=1,NEND
         SUM3=SUM3+DESP(I)*GGRAD(I)
 700  CONTINUE
C
C     TOTAL ENERGY
C
      VTT=SCV+SUM3+SUM2
C
C     COMPUTE THE ZERO POINT ENERGY
C
      V=VTT
      IF (VTT.GT.SCV) THEN
         VALVAG=0.0
         WRITE (6,*) 'HIGH VALUE FOR V IN VALVAG ROUTINE'
      ELSE
C        CALL ZEROPT (3)
C        GOTO 1111
         VAD=V
         DO I=1,ISHFT
            IF (FREQ(I).GE.0.0) VAD=VAD+0.5D0*FREQ(I)
         END DO
C1111    CONTINUE
c        WRITE (66,2800) IEXP,KKK,(VTT-SCV)*CKCAL,VAD*ckcal,S,XXX10
         VALVAG=-VAD
      END IF

C
C     RESTORE F AND V VALUES
C
 777  DO I=1,NEND
         DO J=1,NEND
            F(I,J)=FGUARD(I,J)
         END DO
      END DO

      V=SCV
           
      RETURN

2000  FORMAT (12F9.1)
2100  FORMAT (12E9.2)
2700  FORMAT (12E10.3)
2800  FORMAT (2I5,' VMEP: ',F10.7,' VAG: ',F10.7,' S: ',F10.6,
     * ' ANGLE: ',F10.6)
      END function valvag

C****************************************************************************
      SUBROUTINE splspv(lgs6,n,si,vi,t,rv,V0,V1,V2,WIM,REDM,TENSION)
C****************************************************************************
      use perconparam, only : nsdim,pi,fu6
c     use dxiz
C     use cm, only : nmc
C

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      SAVE GV
      SAVE GAS
      DIMENSION SI(NSDIM), VI(NSDIM),GS(NSDIM)
      real(8),allocatable ::  GV(:), GAS(:)
      if(.not.allocated(gv))then
        allocate(gv(nsdim),gas(nsdim)); gv=0.d00; gas=0.d00
      end if 

C
C     Now we interpolate DV using splines based on both the
C     original and the additional data, in this case, the L parameter
C     of the low level is known, we use this L value to fit all.
C
      IF (NMC.NE.1) THEN
        NMC = 1
C
C     We calculate S0 and TL and some parameters of an Eckart curve
C     based on the low-level values.
C
        A = V2 - V1
        C = V1
        A2 = ABS(V0-V1)
        B2 = ABS(V0-V2)
        WK=(WIM)**2.D0*REDM
        AL1=SQRT(A2/WK)
        AL2=SQRT(B2/WK)
C
C     In order to avoid problems is A2 and B2 are too different (very
C     asymmetric reaction) we limit the value of AL1 and AL2
C
        AL1=DMIN1(AL1,2.D0*AL2)
        AL2=DMIN1(AL2,2.D0*AL1)
        TL=(AL1 + AL2)/2.D0
        S0=(-AL1+AL2)/2.D0
C
        WRITE (FU6,*) 'PARAMETERS USED FOR MAPPING'
        WRITE (FU6,*) 'S0 = ',S0,' TL = ',TL
C
        DO I = 1,N
           GS(I) = SI(I)
           GV(I) = VI(I)
        ENDDO
        IF (LGS6.EQ.1.OR.LGS6.EQ.2) THEN
                GAS(1)=-1.D0
        ELSE
                GAS(1)=2./PI*ATAN((GS(1)-S0)/TL)
        ENDIF
        IF (LGS6.EQ.1.OR.LGS6.EQ.3) THEN
                GAS(N)=1.D0
        ELSE
                GAS(N)=2./PI*ATAN((GS(N)-S0)/TL)
        ENDIF
        DO I=2,N-1
                GAS(I)=2./PI*ATAN((GS(I)-S0)/TL)
        ENDDO
      ENDIF
      TA=2./PI*ATAN((T-S0)/TL)
      CALL SPL(GAS,GV,N,TA,RV,1,TENSION)
      RETURN
      END SUBROUTINE splspv
C***********************************************************************
C     ZSPMEP
C***********************************************************************
      SUBROUTINE zspmep (VRA,V0A,VPA,VRS,V0S,VPS)
      use common_inc
      use perconparam
      use cm
      use kintcm, only : idvmep
      use rate_const, only : sp1,sp2,ssubi,vclas,lgsic,tension
C
C     This subroutine creates the spline fit information for the
C     SPIC option
C     CALLED BY:
C               ZOCPAR
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     V0A, V0A  : HL/LL BARRIER HEIGHT
C     VPA, VPA  : HL/LL PRODUCT ENERGY or WELL
C     VRA, VRA  : HL/LL REACTANT ENERGY or WELL
C
c
c Check the s value to 0.0001 bohr along the path, then calculate the
c energy differences
c
      IF (IDVMEP.EQ.0) THEN                                             0317Yc99
        DO I = 1, NSPIC
           ISPS = NINT(10000*SPICS(I))
           DO J = 1 , LSAVE
             ISSUBI = NINT(10000*SSUBI(J))
             IF (ISSUBI.EQ.ISPS) THEN
                 SPICV(I) = SPICV(I) - VCLAS(J)
             ENDIF
           ENDDO
        ENDDO
      ENDIF                                                             0317Yc99
C
C Add saddle point, reactant (or well), and product (or well)
C
      SPICS(NSPIC+1) = 0.0d0
      SPICV(NSPIC+1) = V0A - V0S
      NSPIC = NSPIC + 1
C
      SPICS(NSPIC+1) = SP1
      SPICV(NSPIC+1) = VRA - VRS
      NSPIC = NSPIC + 1
C
      SPICS(NSPIC+1) = SP2
      SPICV(NSPIC+1) = VPA - VPS
      NSPIC = NSPIC +  1
C
      IF (LGSIC(1).EQ.22) IREX = 1
      IF (LGSIC(1).EQ.21) IREX = 2
      IF (LGSIC(1).EQ.12) IREX = 3
      IF (LGSIC(1).EQ.11) IREX = 4
C
C Sort the input information in assending order
C
      DO I = 1, NSPIC -1
         DO J = I+1, NSPIC
            IF (SPICS(J) .LT. SPICS(I)) THEN
               TEMPSP  = SPICS(I)
               SPICS(I) = SPICS(J)
               SPICS(J) = TEMPSP
               TEMPSP = SPICV(I)
               SPICV(I) = SPICV(J)
               SPICV(J) = TEMPSP
            ENDIF
         ENDDO
      ENDDO
C
      WRITE (FU6,*) 'ISPE - ARRAY USED FOR SPLINE FIT'
      DO I = 1, NSPIC
        WRITE (FU6,199) I,SPICS(I),SPICV(I)*CKCAL
      ENDDO
199   FORMAT (5X,'POINT(',I3,')= ',2F12.4)
C
      CALL SPLSPV(IREX,NSPIC,SPICS,SPICV,SPS,SPV,V0S,VRS,VPS,
     *            WSTAR,REDM,TENSION)
      RETURN
      END SUBROUTINE zspmep
C

      SUBROUTINE BSSRSUM(EMAX,DE,M,W,XX,IS,NVIBM,N3,FMITS,ICODE,GTN,DEN,
     *           NGR)
C
C  This subroutine is to use the extened Beyer-Schwinehart method 
C  by Stein & Rabinovitch to calculate sum of states
C  Ref: J.C.P 58, 2438 (1973); J.C.P. 70, 5107 (1979)
C
      IMPLICIT NONE
      INTEGER   NVIBM,ICODE,N3,N3M7,IS,M,I,J,K,RJMAX,NGR
      INTEGER   RJ(M)
      REAL(8)  EMAX,DE,FMITS,EI
      REAL(8)  T(M),AT(M),GTN(NGR),DEN(NGR)
      REAL(8)  W(NVIBM),XX(NVIBM)
      real(8)  autocm
    
      autocm = 2.19474627D+05
      GTN = 0d0
      AT  = 0d0

      IF(NGR .LT. M) THEN
        WRITE(6, *) 'Increasing NGR parameter in rate subroutine'
        write(6,*)  'NGR = ',NGR,'M =', M
      ENDIF
      IF (ICODE .LT. 0) THEN                                            
         N3M7 = N3 - 1                                                  
      ELSE IF (ICODE.EQ.3) THEN                                         
         N3M7 = N3 - 6                                                  
      ELSE                                                              
         N3M7 = N3 - 7                                                  
      ENDIF                                                             
C
C  calculate classical sum of states for external rotation
C  and put the N(E_i+DE)-N(E_i) to array T(I)
C  see ref.  J.C.P. 70, 5107 (1979)
      DO 100 I = 2, M
         EI = DE*DBLE(I-1)
         IF (EI .GT. EMAX) THEN
            T(I) = 0
            GOTO 100
         ENDIF
         IF(ICODE .LT. 0) THEN
           T(I) = 0
         ELSEIF(ICODE.EQ.3) THEN
           T(I) = 2.D0*FMITS*DE
         ELSE
           T(I) = 8.D0*DSQRT(2D0*FMITS)/3D0*((EI+DE)*DSQRT(EI+DE)
     *            -EI*DSQRT(EI))
         ENDIF
         AT(I) = 0.d0 
 100  CONTINUE
      T(1) = 1.D0
      IF(ICODE .LT. 0) T(1) = 0.d0
C
c  Using Extended Beyer-Schwinehart method by Stein & Rabinovitch to 
C  calculate vibrations
C
      DO 200 J = 1, N3M7
         IF(W(J)*AUTOCM.lt.-1.d0) then
         WRITE(6,*) 'You have imaginary frequncies along reaction path!'
         WRITE(6,*) 'TERMINATE Beyer-Schwinehart algrithm!'
         write(6,*) 'Please firstly try to eliminate imaginary frequncy'
         write(6,*) 'along the reaction path, by using / choosing'
         write(6,*) 'appropriate internal coordinates'
         write(6,*) '(use CURV2 or CURV3 options)!'
         stop
         endif
         CALL ENUMVIB(W(J),XX(J),DE,RJMAX,RJ,M)
         DO 210 I = 1, RJMAX
           DO 220 K = RJ(I)+1, M
             AT(K) = AT(K) + T(K-RJ(I))
 220       CONTINUE
 210     CONTINUE
         DO I = 1, M
           T(I)  = AT(I) + T(I)
           AT(I) = 0.d0
         ENDDO
 200  CONTINUE

      GTN(1) = T(1)
      DEN(1) = GTN(1)/DE
      DO I = 2, M
        DEN(I) = T(I)/DE
        GTN(I) = T(I) + GTN(I-1)
C       write(6,*) 'GTN(I)', I, GTN(I)
      ENDDO
C     write(6,*) 'GTN(M), T(M) ',GTN(M), T(M)
      RETURN
      END

      subroutine enumvib(W,XX,DE,RJMAX,RJ,M)
      implicit none
      integer  i, M, RJMAX
      integer  RJ(M)
      double precision w,xx,de,vi,e,emax

C     write(6,*) 'XX = ', XX
      if(abs(xx) .lt. 1.d-14) then
         RJMAX = int(dble(m-1)*de/w)
C        write(6,'(a,2i8,f10.2)')'RJMAX,M,W,DE ', RJMAX,M,
C    *                           W*2.19474627D+05
         IF(RJMAX.GT.M) then
           write(6,*) 'Reduce the egrid value '
           stop
         endif
      else
         RJMAX = int(abs(0.5d0*(1.d0-xx)/xx))
C        write(6,*) 'RJMAX,M,W,DE ', RJMAX,M,W,DE
      endif

      do i = 1, RJMAX
         vi = dble(i)
         RJ(i) = IDNINT(w*(1.d0-xx-xx*vi)*vi/de)
      enddo

c     emax = dble(m-1)*de
c     do i = 1, M
c        vi = dble(i)
c        e = w*(1d0-xx-xx*vi)*vi
c        if(e .lt. 0d0 .or. e. gt. emax) goto 100
c        RJ(i) = IDNINT(e/de)
c     enddo
c100  RJMAX = i - 1
      return
      end

      subroutine SUMSTATES(EMAX,VAGMU,DELE,ISMMVT,ISPMVT,GTNS,NGR)
      use perconparam
      use common_inc
      use rate_const
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
      real(8)  gtns(ngr,nsdm)
      real(8)  gtn(ngr),den(ngr)
      real(8)  NV(NVIBM),EZEROX(NVIBM),EONE(NVIBM),ENVIB(NVIBM)
 
      GTNS = 0d0

      DO 100 IS = ISMMVT,ISPMVT
C
C  Calculate zero-point energy 
C
         ESUM = 0.D0
         DO 10 I = 1, N3M7
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
   10    CONTINUE
         E0 = EMAX - VCLAS(IS) - ESUM
         M = INT(E0/DELE)+1
C  Using the extended Beyer-Schwinehart method by Stein & Rabinovitch
            CALL BSSRSUM(E0,DELE,M,WETS(:,IS),XETS(:,IS),IS,
     *        NVIBM,N3,FMITS(IS),ICODE(5),GTN,DEN,NGR)

C        ETHRESH = VAGMU - ESUM - VCLAS(IS)
c        write(6,*) 'ETHRESH =', ETHRESH*CKCAL
c        IF (ETHRESH .GT. 0d0) THEN
c          IE = ETHRESH/DELE + 1
c          write(6,*) 'IE M', IE, M
c          DO I = IE+1, M
c             GTNS(I,IS) = GTN(I) - GTN(IE)
c             write(6,'(a,3es14.6)') 'GTNS GTN(I) GTN(IE)',
c    *                              GTNS(I,IS),GTN(I),GTN(IE)
c          ENDDO
c        ELSE
c          DO I = 1, M
c             GTNS(I,IS) = GTN(I)
c          ENDDO
c        ENDIF
        GTNS(:,IS) = GTN(:)
 100  CONTINUE
C
C debug purpose
C
C     M = INT((EMAX-VAGMU)/DELE)+1
C     IF(M .GT. NGR) STOP 'M > NGR'
C     open(99,FILE='poly.fu99',status='unknown')
C     DO I = 1, M, 50
C         write(99,'(a,f10.2,a)') 'Energy = ',DELE*DBLE(I+1)*CKCAL,
C    *                             ' kcal/mol'
C        DO IS = ISMMVT, ISPMVT
C          write(99,'(f8.2,ES15.6)')SSUBI(IS)*0.529177249,GTNS(I,IS)
C        ENDDO
C     ENDDO  
C     close(99)
      return
      end

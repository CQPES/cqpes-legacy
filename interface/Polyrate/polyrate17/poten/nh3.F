      SUBROUTINE setup(N3TM)
C
C   Sys.: NH3
C   Ref.: B. Maessen, P. Bopp, D. R. McLaughlin, and M. Wolfsberg
C         Z. Naturforsch. Teil A 39, 1005-1006 (1984).
C
C        SETUP must be called once before any calls to SURF.
C        The potential parameters are initialized in the subprogram SETUP.
C        The cartesian coordinates, potential energy, and the derivatives of 
C        the potential energy with respect to the cartesian coordinates are 
C        passed in the call statement through the argument list:
C                  CALL SURF (V, X, DX, N3TM)
C        where X and DX are dimensioned N3TM.
C        All information passed through the parameter list is in 
C        hartree atomic units.  
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / POTCON / RE, ALPHAE, A, B, C, SDR, SDR2, SDA, SDA2, 
     *                  SRIJ, SAIJ, PRDR, PRDA, DAIJ(3), DRIJ(3),
     *                  C2A, C2B, C2C, C2D, C2E, C2F,
     *                  C3A, C3B, C3C, C3D, C3E, C3F, C3G, C3H,
     *                  C4A
      COMMON /RTCON/ RT1D2, RT1D3, RT1D6, PI, PID2
      COMMON /INDICE/ IJ(3), IK(3)
C
      DATA N3TMMN / 12/
C
      WRITE (6, 1000)
C
C   Check the value of N3TM passed by the calling program.
C
         IF (N3TM .LT. N3TMMN) THEN
             WRITE (6, 6000) 
             STOP 'SETUP 1'
         ENDIF
C
         IJ(1) = 2 
         IJ(2) = 3 
         IJ(3) = 1 
         IK(1) = 3 
         IK(2) = 1 
         IK(3) = 2 
C
         PI = 3.1415926536D0
C
C INITIALIZE NH3 POTENTIAL CONSTANTS (SEE WOLFSBERG AND
C MORINO ET. AL.
C
      RE = 1.024D0
      ALPHAE = (PI / 180.00D0) * 107.3D0
C
      A = 0.53190D-11
      B = 1.1406D-11
      C = 0.6140D0
C
C INITIALIZE POTENTIAL CONSTANTS
C
C QUADRATIC TERMS: C2
C
      C2A = 3.52595D0
      C2B = 0.22163D0
      C2C = -0.22163D0
      C2D = 0.01480D0
      C2E = -0.11620D0
      C2F = 0.05810D0
C
C CUBIC TERMS: C3
C
      C3A = -0.55000D0
      C3B = -0.54000D0
      C3C = -7.01000D0
      C3D = -0.03778D0
      C3E =  0.05667D0
      C3F = -0.22667D0
      C3G = -0.07000D0
      C3H =  0.07000D0
C
C QUARTIC TERM: C4A
C
      C4A = 11.07417D0
C
C DETERMINE OTHER USEFUL CONSTANTS
C
      RT1D2 = 1.0D0/SQRT(2.0D0)
      RT1D3 = 1.0D0/SQRT(3.0D0)
      RT1D6 = 1.0D0/SQRT(6.0D0)
      PID2 = PI / 2.0D0
C
1000  FORMAT(/,2X,T5,'Setup has been called for NH3, ',
     *                'surface no. 2 of Wolfsberg')
6000  FORMAT(/,2X,T5,'Error: The value of N3TM passed by the ',
     *               'calling program is smaller ',
     *       /,2X,T12,'than the minimum value allowed.')
C
      RETURN
      END
      SUBROUTINE SURF(V, XB, DXB, N3TM)
C
C THE DRIVER ROUTINE SHOULD BE CALLED SURF
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /XANG/ X(30)
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
      COMMON /POTVAL/ VM, VS, VNH3, VQUAD, VCUBIC, VQUAR
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON / POTCON / RE, ALPHAE, A, B, C, SDR, SDR2, SDA, SDA2, 
     *                  SRIJ, SAIJ, PRDR, PRDA, DAIJ(3), DRIJ(3),
     *                  C2A, C2B, C2C, C2D, C2E, C2F,
     *                  C3A, C3B, C3C, C3D, C3E, C3F, C3G, C3H,
     *                  C4A
         DIMENSION XB(N3TM), DXB(N3TM)
C
C EEQ0 - EQUILIBRIUM ENERGY OF NH3 IN C3V GEOMETRY
C
       DATA EEQ0 /0.2533613651D0/
C
C DRIVER ROUTINE THAT COMPUTES THE WOLFSBERG POTENTIAL AND
C DERIVATIVES FOR NH3 - FINAL DERIVATIVES IN CARTESIANS
C
C
      CALL CORDTR(XB, N3TM)
C
      CALL POTM
      CALL POTS
      CALL DVDINC
      CALL DRDXSB
      CALL DALPDR
      CALL DVMDXS
      CALL DNDXSB
      CALL DFDXSB
      CALL DGDXSB
      CALL DVSDXS
C
C  SUBTRACT OFF EEQ0 SO THAT EQUILIBRIUM GEOM OF NH3 HAS V=0.0
C
      V = VNH3 - EEQ0
C
      DO 300 I=1,12
      DXB(I) = DVDX(I) * 0.52917706D0
 300  CONTINUE
C
      RETURN
      END
C
      SUBROUTINE CORDTR(XB, N3TM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C RNH(I) = N-HI BOND LENGTH IN ANGSTROM
C
C RHH(I) = H(J)-H(K) DISTANCE IN ANGSTROM
C
C DELRNH(I) = DELTA IN N-HI BOND LENGTH FROM
C     EQUILIBRIUM IN ANGSTROM
C
C ALPHA(I) = HJ-N-HK BOND ANGLE IN RADIANS
C
C DELALP(I) = DELTA IN HJ-N-HK BOND ANGLE FROM
C     EQUILIBRIUM IN RADIANS
C
C RKAPPA(I) = ANGLE BETWEEN H-H-H PLANE AND ITH
C     BOND IN RADIANS
C
C X(I) = CARTESIAN CORDINATES
C     I = 1 - 3 :   X, Y, Z, OF N
C     I = 4 - 6 :   X, Y, Z, OF H1
C     I = 7 - 9 :   X, Y, Z, OF H2
C     I = 10 - 12 : X, Y, Z, OF H3
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
      COMMON / POTCON / RE, ALPHAE, A, B, C, SDR, SDR2, SDA, SDA2, 
     *                  SRIJ, SAIJ, PRDR, PRDA, DAIJ(3), DRIJ(3),
     *                  C2A, C2B, C2C, C2D, C2E, C2F,
     *                  C3A, C3B, C3C, C3D, C3E, C3F, C3G, C3H,
     *                  C4A
C
      COMMON / RTCON / RT1D2, RT1D3, RT1D6, PI, PID2
C
      COMMON / INDICE / IJ(3), IK(3)
C
      COMMON /XANG/ X(30)
         DIMENSION XB(N3TM)
C
C CHANGE ALL COORDINATES FROM BOHR TO ANGSTROM
C
       DO 10 I=1,12
  10   X(I) = XB(I) * 0.52917706D0
      N3M6=6
C
C CALCULATE RNH
C
      IOFF=3
      DO 20 IH=1,3
      RNH(IH) = SQRT ((X(IOFF+1) - X(1))**2
     A            +   (X(IOFF+2) - X(2))**2
     B            +   (X(IOFF+3) - X(3))**2)
      IOFF=IOFF+3
 20   CONTINUE
C
C CALCULATE RHH
C
      DO 30 I=1,3
      J=IJ(I)
      K=IK(I)
      RHH(I) = SQRT ((X(3*J+1) - X(3*K+1))**2
     A           +   (X(3*J+2) - X(3*K+2))**2
     B           +   (X(3*J+3) - X(3*K+3))**2)
 30   CONTINUE
C
C CALCULATE ALPHA
C
      DO 40 I=1,3
      J=IJ(I)
      K=IK(I)
      ALPHA(I) = ACOS((RNH(J)**2+RNH(K)**2-RHH(I)**2)/
     A           (2.0D0*RNH(J)*RNH(K)))
 40   CONTINUE
C
C CALCULATE DELRNH AND DELALP
C
      DO 50 I=1,3
      DELRNH(I) = RNH(I) - RE
      DELALP(I) = ALPHA(I) - ALPHAE
 50   CONTINUE
C
C CALCULATE RKAPPA
C
C DETERMINE COMPONENTS OF VECTOR NORMAL TO HHH PLANE
C
      RNORMI = (X(8)-X(5))*(X(12)-X(6)) - (X(9)-X(6))*(X(11)-X(5))
      RNORMJ = -((X(7)-X(4))*(X(12)-X(6)) - (X(9)-X(6))*(X(10)-X(4)))
      RNORMK = (X(7)-X(4))*(X(11)-X(5)) - (X(8)-X(5))*(X(10)-X(4))
C
      DO 60 I=1,3
      RKAPPA(I) = PID2 - ACOS ((RNORMI*(X(1)-X(3*I+1))
     A                    +     RNORMJ*(X(2)-X(3*I+2))
     B                    +     RNORMK*(X(3)-X(3*I+3))) /
     C (RNH(I)*SQRT(RNORMI**2+RNORMJ**2+RNORMK**2)))
 60   CONTINUE
C
C DETERMINE USEFUL SUMS:
C
C...SDR = SUM DELRNH(I)
C...SDR2 = SUM DELRNH(I)**2
C...SDA = SUM DELALP(I)
C...SDA2 = SUM DELALP(I)**2
C...SRIJ = SUM DELRNH(I)*DELRNH(J)
C...SAIJ = SUM DELALP(I)*DELALP(J)
C...PRDR = PRODUCT DELRNH(I)
C...PRDA = PRODUCT DELALP(I)
C...DAIJ(K) = PRODUCT DELALP(I)*DELALP(J)
C...DRIJ(K) = PRODUCT DELRNH(I)*DELRNH(J)
C
      SDR = DELRNH(1)+DELRNH(2)+DELRNH(3)
      SDR2 = DELRNH(1)**2 + DELRNH(2)**2 + DELRNH(3)**2
      SDA = DELALP(1)+DELALP(2)+DELALP(3)
      SDA2 = DELALP(1)**2+DELALP(2)**2+DELALP(3)**2
      SRIJ = DELRNH(1)*DELRNH(2)+DELRNH(1)*DELRNH(3)+DELRNH(2)*DELRNH(3)
      SAIJ = DELALP(1)*DELALP(2)+DELALP(1)*DELALP(3)+DELALP(2)*DELALP(3)
      PRDR = DELRNH(1)*DELRNH(2)*DELRNH(3)
      PRDA = DELALP(1)*DELALP(2)*DELALP(3)
C
      DO 300 I=1,3
      J=IJ(I)
      K=IK(I)
      DAIJ(I) = DELALP(J)*DELALP(K)
      DRIJ(I) = DELRNH(J)*DELRNH(K)
 300  CONTINUE
C
      RETURN
      END
C
      SUBROUTINE POTM
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE CALCULATES THE MORINO POTENTIAL FOR
C NH3 WITH WOLFSBERG'S MODIFICATIONS
C
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
      COMMON / POTCON / RE, ALPHAE, A, B, C, SDR, SDR2, SDA, SDA2, 
     *                  SRIJ, SAIJ, PRDR, PRDA, DAIJ(3), DRIJ(3),
     *                  C2A, C2B, C2C, C2D, C2E, C2F,
     *                  C3A, C3B, C3C, C3D, C3E, C3F, C3G, C3H,
     *                  C4A
C
      COMMON /POTVAL/ VM, VS, VNH3, VQUAD, VCUBIC, VQUAR
C
C CALCULATE QUADRATIC TERM
C
      VQUAD = C2A * SDR2 + C2B * SDA2 + C2C * SAIJ + C2D * SRIJ
     *      + C2E * (DELRNH(1)*DELALP(1) + DELRNH(2)*DELALP(2)
     *            +  DELRNH(3)*DELALP(3))
     *      + C2F * (DELRNH(1)*(DELALP(2) + DELALP(3))
     *            +  DELRNH(2)*(DELALP(1) + DELALP(3))
     *            +  DELRNH(3)*(DELALP(1) + DELALP(2)))
C
C CUBIC TERM
C
      VCUBIC = C3A * (DELRNH(1)*(DELRNH(2)**2 + DELRNH(3)**2)
     *             +  DELRNH(2)*(DELRNH(1)**2 + DELRNH(3)**2)
     *             +  DELRNH(3)*(DELRNH(1)**2 + DELRNH(2)**2))
     *       + C3B * PRDR
     *       + C3C * (DELRNH(1)**3 + DELRNH(2)**3 + DELRNH(3)**3)
     *       + C3D * (DELALP(1)**3 + DELALP(2)**3 + DELALP(3)**3)
     *       + C3E * (DELALP(1)*(DELALP(2)**2 + DELALP(3)**2)
     *             +  DELALP(2)*(DELALP(1)**2 + DELALP(3)**2)
     *             +  DELALP(3)*(DELALP(1)**2 + DELALP(2)**2))
     *       + C3F * PRDA + C3G * SDR * SDA2 + C3H * SDR * SAIJ
C
C QUARTIC TERM
C
      VQUAR = C4A * (DELRNH(1)**4 + DELRNH(2)**4 + DELRNH(3)**4)
C
C
      VM = VQUAD + VCUBIC + VQUAR
C
C CONVERT TO HARTREE
C
      VM = VM * .22937D0
C
      RETURN
      END
C
      SUBROUTINE POTS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE CALCULATES THE WOLFSBERG TERM
C IN THE WOLFSBERG NH3 POTENTIAL
C
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
      COMMON / POTCON / RE, ALPHAE, A, B, C, SDR, SDR2, SDA, SDA2, 
     *                  SRIJ, SAIJ, PRDR, PRDA, DAIJ(3), DRIJ(3),
     *                  C2A, C2B, C2C, C2D, C2E, C2F,
     *                  C3A, C3B, C3C, C3D, C3E, C3F, C3G, C3H,
     *                  C4A
C
      COMMON / RTCON / RT1D2, RT1D3, RT1D6, PI, PID2
C
      COMMON /POTVAL/ VM, VS, VNH3, VQUAD, VCUBIC, VQUAR
C
      ROE = RT1D3 * (RKAPPA(1)+RKAPPA(2)+RKAPPA(3))
C
      VS = A*(ROE**2) + (B*EXP(-C*(ROE**2)))
C
C CONVERT VS FROM ERGS TO HARTREE
C
      VS = VS * 2.2937D+10
C
      VNH3 = VM + VS
C
      RETURN
      END
C
      SUBROUTINE MULT(A,NA,MA,B,NB,MB,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE PERFORMS THE MATRIX MULTIPLICATION :
C
C         C = A X B
C
      DIMENSION A(NA,MA), B(NB,MB), C(NA,MB)
C
      DO 100 I=1,NA
C
      DO 50 J=1,MB
C
      SUM=0.0D0
      DO 25 K = 1,MA
C
      SUM = SUM + A(I,K) * B(K,J)
 25   CONTINUE
C
      C(I,J) = SUM
 50   CONTINUE
C
 100  CONTINUE
      RETURN
      END
C
      SUBROUTINE DALPDR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE CALCULATES THE NON-ZERO
C PARTIAL DERIVATIVES OF ALPHA(I) W.R.T.
C RNH(I) AND RHH(I)
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON /INDICE/ IJ(3), IK(3)
C
      DO 100 I=1,3
      J = IJ(I)
      K = IK(I)
C
      DADR(I,J) = -(1.0D0/SIN(ALPHA(I))) *
     A             (RNH(J)**2 - RNH(K)**2 + RHH(I)**2) /
     B             (2.0D0 * RNH(K) * RNH(J)**2)
C
      DADR(I,K) = -(1.0D0/SIN(ALPHA(I))) *
     A             (RNH(K)**2 - RNH(J)**2 + RHH(I)**2) /
     B             (2.0D0 * RNH(J) * RNH(K)**2)
C
      DADR(I,3+I) = (1.0D0/SIN(ALPHA(I))) * RHH(I) /
     A              (RNH(J) * RNH(K))
C
 100  CONTINUE
C
      RETURN
      END
C
      SUBROUTINE DFDXSB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE EVALUATES THE PARTIAL DERIVATIVES OF FNORM(I)
C W.R.T. THE CARTESIANS
C
      COMMON /XANG/ X(30)
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON /NORMAL/ RNORM(3), FNORM(3), GNORM(3)
C
C EVALUATE DFDX(I,J), I=1,3; J=1,3
C
      DO 100 I = 1,3
C
      DO 50 IX = 1,3
C
      DFDX(I,IX) = RNORM(IX)
C
 50   CONTINUE
C
 100  CONTINUE
C
C EVALUATE DFDX(I,J), I=1,3; J=4,12
C
      DO 500 I=1,3
C
      DO 400 IX = 1,3
C
      DO 300 M = 1,3
C
      DFDX(I, 3*IX + M) = (X(1) - X(3*I+1))*DNDX(1,3*IX+M)
     A                  + (X(2) - X(3*I+2))*DNDX(2,3*IX+M)
     B                  + (X(3) - X(3*I+3))*DNDX(3,3*IX+M)
C
      IF(I.EQ.IX) DFDX(I,3*IX+M) = DFDX(I,3*IX+M) - RNORM(M)
C
 300  CONTINUE
C
 400  CONTINUE
C
 500  CONTINUE
C
      RETURN
      END
C
      SUBROUTINE DGDXSB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE EVALUATES THE PARTIAL DERIVATIVES OF
C GNORM W.R.T. THE CARTESIANS
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON /NORMAL/ RNORM(3), FNORM(3), GNORM(3)
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
C EVALUATE THE NORM OF RNORM
C
      XNORM = SQRT(RNORM(1)**2 + RNORM(2)**2 + RNORM(3)**2)
C
C EVALUATE DGDX(I,J), I=1,3; J=1,3
C
      DO 200 I=1,3
C
      DO 100 IX=1,3
C
      DGDX(I,IX) = XNORM * (DRDX(I,IX))
C
 100  CONTINUE
C
 200  CONTINUE
C
C EVALUATE DGDX(I,J), I=1,3; J=4,12
C
      DO 500 I=1,3
C
      DO 400 IX = 4,12
C
      DGDX(I,IX) = RNH(I) * (RNORM(1) * DNDX(1,IX)
     A                    +  RNORM(2) * DNDX(2,IX)
     B                    +  RNORM(3) * DNDX(3,IX))
     C                    /  XNORM
     D                    +  XNORM * (DRDX(I,IX))
C
 400  CONTINUE
C
 500  CONTINUE
C
      RETURN
      END
      SUBROUTINE DNDXSB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE EVALUATES THE COMPONENTS OF THE
C VECTOR NORMAL TO THE H1-H-H PLANE AND THEIR
C DERIVATIVES W.R.T. THE CARTESIANS
C
      COMMON /XANG/ X(30)
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON /NORMAL/ RNORM(3), FNORM(3), GNORM(3)
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
C INITIALIZE RNORM
C
      RNORM(1) = RNORMI
      RNORM(2) = RNORMJ
      RNORM(3) = RNORMK
C
C EVALUATE THE NON-ZERO DERIVATIVES OF RNORM
C
      DNDX(1,5) = (X(9) - X(12))
      DNDX(2,4) = -DNDX(1,5)
C
      DNDX(1,6) = (X(11) - X(8))
      DNDX(3,4) = -DNDX(1,6)
C
      DNDX(1,8) = (X(12) - X(6))
      DNDX(2,7) = -DNDX(1,8)
C
      DNDX(1,9) = - (X(11) - X(5))
      DNDX(3,7) = - DNDX(1,9)
C
      DNDX(1,11) = - (X(9) - X(6))
      DNDX(2,10) = -DNDX(1,11)
C
      DNDX(1,12) = (X(8) - X(5))
      DNDX(3,10) = -DNDX(1,12)
C
      DNDX(2,6) = (X(7) - X(10))
      DNDX(3,5) = -DNDX(2,6)
C
      DNDX(2,9) = (X(10) - X(4))
      DNDX(3,8) = -DNDX(2,9)
C
      DNDX(2,12) = -(X(7) - X(4))
      DNDX(3,11) = -DNDX(2,12)
C
C EVALUATE FNORM(I)
C
      DO 100 I=1,3
C
      FNORM(I) = RNORM(1) * (X(1) - X(3*I + 1))
     A         + RNORM(2) * (X(2) - X(3*I + 2))
     B         + RNORM(3) * (X(3) - X(3*I + 3))
C
 100  CONTINUE
C
C EVALUATE GNORM(I)
C
      DO 200 I=1,3
C
      GNORM(I) = RNH(I) * SQRT(RNORM(1)**2 + RNORM(2)**2 + RNORM(3)**2)
C
 200  CONTINUE
C
      RETURN
      END
C
      SUBROUTINE DRDXSB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE CALCULATES THE PARTIAL DERIVATIVES
C OF RNH(I) AND RHH(I) W.R.T. THE CARTESIAN COORDINATES
C X(J)
C
      COMMON /XANG/ X(30)
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON /INDICE/ IJ(3), IK(3)
C
C CALCULATE DRDX - R IS RNH
C
      DO 100 J=1,3
      DO 100 I=1,3
      DRDX(I,J) = -(X(3*I+J) - X(J)) / RNH(I)
      DRDX(I, 3*I + J) = -DRDX(I,J)
 100  CONTINUE
C
C CALCULATE DRDX - R IS RHH
C
      DO 200 I=1,3
      J = IJ(I)
      K = IK(I)
      DO 200 M=1,3
      DRDX(3+I,3*K+M) = (X(3*K+M) - X(3*J+M))/RHH(I)
      DRDX(3+I,3*J+M) = -DRDX(3+I,3*K+M)
 200  CONTINUE
C
      RETURN
      END
C
      SUBROUTINE DVDINC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE CALCULATES THE NON-ZERO
C DERIVATIVES OF V(NH3) FROM WOLFSBERG
C W.R.T. THE INTERNAL COORDINATES:
C RNH(I), ALPHA(I), AND KAPPA(I)
C
C DVDIC(J), J=1,3 ARE DV/DRNH(I)
C DVDIC(J), J=4,6 ARE DV/DALPHA(I)
C DVDIC(J), J=7,9 ARE DV/DKAPPA(I)
C
C
      COMMON / POTCON / RE, ALPHAE, A, B, C, SDR, SDR2, SDA, SDA2, 
     *                  SRIJ, SAIJ, PRDR, PRDA, DAIJ(3), DRIJ(3),
     *                  C2A, C2B, C2C, C2D, C2E, C2F,
     *                  C3A, C3B, C3C, C3D, C3E, C3F, C3G, C3H,
     *                  C4A
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
      COMMON /INDICE/ IJ(3), IK(3)
C
      DIMENSION DV2(6), DV3(6), DV4(6)
C
C
C  CALCULATE DVQUAD/DRNH AND DVQUAD/DALP
C
      DO 10 I=1,3
      J = IJ(I)
      K = IK(I)
C
      DV2(I) = 2.0D0 * C2A * DELRNH(I)
     A       + C2D * (DELRNH(J) + DELRNH(K))
     B       + C2E * DELALP(I)
     C       + C2F * (DELALP(J) + DELALP(K))
C
      DV2(I+3) = 2.0D0 * C2B * DELALP(I)
     A         + C2C * (DELALP(J) + DELALP(K))
     B         + C2E * DELRNH(I)
     C         + C2F * (DELRNH(J) + DELRNH(K))
C
 10   CONTINUE
C
C CALCULATE DVCUBIC/DRNH AND DVCUBIC/DALP
C
      DO 20 I=1,3
      J=IJ(I)
      K = IK(I)
C
      DV3(I) = C3A * (DELRNH(J)**2 + DELRNH(K)**2
     A             + 2.0D0*(DRIJ(J) + DRIJ(K)))
     B       + C3B * DRIJ(I)
     C       + 3.0D0 * C3C * DELRNH(I)**2
     D       + C3G * SDA2  +  C3H * SAIJ
C
      DV3(I+3) = 3.0D0 * C3D * DELALP(I)**2
     A         + C3E * (DELALP(J)**2 + DELALP(K)**2
     B               + 2.0D0*(DAIJ(J) + DAIJ(K)))
     C         + C3F * DAIJ(I)
     D         + 2.0D0 * C3G * DELALP(I) * SDR
     E         + C3H * (DELALP(J) + DELALP(K)) * SDR
C
 20   CONTINUE
C
C CALCULATE DQUARTIC/DRNH
C
      DO 30 I=1,3
      DV4(I) = 4.0D0 * C4A * DELRNH(I)**3
 30   CONTINUE
C
      DO 50 I=1,6
      DVDIC(I) = DV2(I) + DV3(I) + DV4(I)
 50   CONTINUE
C
      RKSUM = RKAPPA(1) + RKAPPA(2) + RKAPPA(3)
      DKAPPA = (2.0D0*A*RKSUM - (2.0D0*C*B*RKSUM)
     A       * EXP((-C*RKSUM**2)/3.0D0))/3.0D0
C
      DVDIC(7) = DKAPPA
      DVDIC(8) = DKAPPA
      DVDIC(9) = DKAPPA
C
      RETURN
      END
C
      SUBROUTINE DVMDXS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE CALCULATES THE PARTIAL DERIVATIVES
C OF VM W.R.T. THE CARTESIAN COORDINATES
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      DIMENSION DICDX(6,12)
C
C CALCULATE DADX
C
      CALL MULT(DADR, 3, 6, DRDX, 6, 12, DADX)
C
C MOVE DRDX AND DADX INTO ONE TEMP ARRAY DICDX
C
      DO 20 I=1,3
      DO 10 J=1,12
      DICDX(I,J) = DRDX(I,J)
      DICDX(I+3,J) = DADX(I,J)
 10   CONTINUE
 20   CONTINUE
C
C CALCULATE DVMDX
C
      CALL MULT(DVDIC, 1, 6, DICDX, 6, 12, DVMDX)
C
C CONVERT TO HARTREE
C
      DO 30 I=1,12
      DVMDX(I) = DVMDX(I) * 0.22937D0
 30   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE DVSDXS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE EVALUATES THE PARTIAL DERIVATIVES OF
C U(FNORM,GNORM) W.R.T. THE CARTESIANS
C          AND
C THE PARTIAL DERIVATIVES OF KAPPA(I) W.R.T. THE CARTESIANS
C          AND
C THE PARTIAL DERIVATIVES OF VS W.R.T. THE CARTESIANS
C
C          AND ULTIMATELY
C
C THE PARTIAL DERIVATIVES OF VNH3 W.R.T. THE CARTESIANS
C
      COMMON /DERIV/ DVDIC(9), DRDX(6,12), DADR(3,6), DUDX(3,12),
     *               DADX(3,12), DVMDX(12), DNDX(3,12), DFDX(3,12),
     *               DGDX(3,12), DKDX(3,12), DVSDX(12), DVDX(12)
C
      COMMON /NORMAL/ RNORM(3), FNORM(3), GNORM(3)
C
      COMMON /RTCON/ RT1D2, RT1D3, RT1D6, PI, PID2
C
      COMMON /COORD/ RNH(3), RHH(3), ALPHA(3), RKAPPA(3),
     *               DELRNH(3), DELALP(3),
     *               RNORMI, RNORMJ, RNORMK
C
C EVALUATE DUDX
C
      DO 200 I=1,3
C
      DO 100 IX = 1,12
C
      DUDX(I,IX) = (GNORM(I) * DFDX(I,IX) - FNORM(I) * DGDX(I,IX)) /
     A              (GNORM(I))**2
C
 100  CONTINUE
C
 200  CONTINUE
C
C
C EVALUATE DKDX
C
      DO 101 I=1,3
C
      DO 51 IX = 1,12
C
      DKDX(I,IX) = (1.0D0/SIN(PID2 - RKAPPA(I))) * DUDX(I,IX)
C
 51   CONTINUE
C
 101  CONTINUE
C
C
C EVALUATE DVSDX
C
      DKAPPA = DVDIC(7)
C
      DO 102 IX = 1,12
C
      DVSDX(IX) = DKAPPA * (DKDX(1,IX) + DKDX(2,IX) + DKDX(3,IX))
C
 102  CONTINUE
C
C CONVERT TO HARTREE
C
      DO 202 IX = 1,12
C
      DVSDX(IX) = DVSDX(IX) * 2.2937D+10
C
 202  CONTINUE
C
C EVALUATE DVDX
C
      DO 300 IX=1,12
C
      DVDX(IX) = DVMDX(IX) + DVSDX(IX)
C
 300  CONTINUE
C
      RETURN
      END

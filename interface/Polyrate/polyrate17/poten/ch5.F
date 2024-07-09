      SUBROUTINE SURF(V, COORD, DX, N3TM)
C
C   System:    CH5 
C   Reference: T. Joseph, R. Steckler, and D. G. Truhlar
C              J. Chem. Phys. 87, 7036-7049 (1987).
C
C   This is a driver routine for the CH5 potential energy surfaces J1 and J2.
C   This driver reads in the potential parameters from FORTRAN unit 4; the
C   potential parameters for the surface J1 are different from those for J2.
C
C   SETUP must be called once before any calls to SURF.
C   The cartesian coordinates, potential energy, and derivatives of the energy
C   with respect to the cartesian coordinates are passed by the calling  
C   program in the argument list as follows:
C        CALL SURF (V, X, DX, N3TM)
C   where X and DX are arrays dimensioned N3TM, and N3TM must be greater
C   than or equal to 18 (3*number of atoms).  
C   All the information passed to and from the potential energy surface 
C   routine is in hartree atomic units.  
C
C        This potential is written such that:
C                       X(1)  - X(3)  : X, Y, Z for H1
C                       X(4)  - X(6)  : X, Y, Z for H2
C                       X(7)  - X(9)  : X, Y, Z for H3
C                       X(10) - X(12) : X, Y, Z for H4
C                       X(13) - X(15) : X, Y, Z for C 
C                       X(16) - X(18) : X, Y, Z for H5
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION COORD(N3TM),DX(N3TM)
C
      COMMON /ICORCM/ RR(15),DERR(15)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      COMMON /POTXCM/ X(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
C
C     PUT COORDINATES IN PROPER ARRAYS
C
      IPT = 1
      DO 10 I = 1, 6
         X(I) = COORD(IPT)
         Y(I) = COORD(IPT+1)
         Z(I) = COORD(IPT+2)
         IPT = IPT+3
   10 CONTINUE
C
      CALL POT (0, COORD, DX, N3TM)
      V = ENERGY
      RETURN
      END
C
C
      SUBROUTINE SETUP(N3TM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   N3TMMN = 3 * NATOMS
C   NATOMS = the number of atoms represented by this potential function
C
C   The variable N3TMMN is the minimum value of N3TM allowed to be 
C   passed by the calling routine for the number of cartesian 
C   coordinates needed to represent the full system represented by this 
C   potential energy surface routine.
C   N3TM must be greater than or equal to N3TMMN.
C
      PARAMETER (N3TMMN = 18)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      COMMON /PDT2CM/ PI,APHI,BPHI,CPHI,PCH4,FCH3,HCH3,RNOT,A3,B3,DELT, 
     *   DIJ,A,B,C0,D,REX
      COMMON /PDT3CM/ FK0,FA,CA,CB
      CHARACTER*5 SURFNM
C
C  CHECK THE NUMBER OF CARTESIAN COORDINATES SET BY THE CALLING PROGRAM
C
      IF (N3TM .LT. N3TMMN) THEN
          WRITE (6, 6000) N3TM, N3TMMN
          STOP 'SETUP 1'
      ENDIF
C
C  OPEN THE POTENTIAL DATA FILE 
C
       OPEN (UNIT=4, FILE='potch5.dat', STATUS='OLD', 
     *       FORM='FORMATTED', ERR=6100)
C
C
      REWIND (4)
      READ (4, 1000) SURFNM
      READ (4,1100) (D1(I),I=1,3)
      READ (4,1100) (D3(I),I=1,3)
      READ (4,1200) B1,C1
      WRITE (6,1300) SURFNM
      WRITE (6,1400) (D1(I),I=1,3)
      WRITE (6,1500) (D3(I),I=1,3),(ALPH(I),I=1,3),(RE(I),I=1,3)
      WRITE (6,1600) B1,C1
C
C  CLOSE THE POTENTIAL DATA FILE
C
      CLOSE (UNIT=4)
C
      RETURN
C
1000  FORMAT(A)
 1100 FORMAT(3F8.5)
 1200 FORMAT(2F6.2)
1300  FORMAT(/,2X,T5,'SETUP has been called for the CH5 ',A3,
     *               'potential energy surface',
     *       //,2X,T5,'Triplet potential parameters:')
1400  FORMAT(2X,T5,'D1:',T12,F8.5,T22,F8.5,T32,F8.5)
1500  FORMAT(2X,T5,'D3:',T12,F8.5,T22,F8.5,T32,F8.5,
     *       /,2X,T5,'Alpha:',T12,F8.5,T22,F8.5,T32,F8.5,
     *       /,2X,T5,'Re:',T12,F8.5,T22,F8.5,T32,F8.5)
1600  FORMAT(2X,T5,'B1:',T12,F5.2,T25,'C1:',T32,F8.5)
6000  FORMAT(/,2X,T5,'Warning: N3TM is set equal to ',I3,
     *                  ' but this potential routine',
     *          /,2X,T14,'requires N3TM be greater than or ',
     *                   'equal to ',I3,/)
C
6100  WRITE(6,*)'Error opening potential data file'
      STOP 'SETUP 2'
C
      END
      BLOCK DATA PARMS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      COMMON /PDT2CM/ PI,APHI,BPHI,CPHI,PCH4,FCH3,HCH3,RNOT,A3,B3,DELT, 
     *   DIJ,A,B,C0,D,REX
      COMMON /PDT3CM/ FK0,FA,CA,CB
C
C     PARAMETERS USED IN THE RAFF SURFACE - FROM RAFF,
C     JCP  60, (1974), P. 2227. THE VALUES OF SOME PARAMETERS
C     ARE CHANGED  FROM THAT OF  RAFF SURFACE WHILE DESIGNING
C     SURFACE J1 AND J2 AND FEW NEW PARAMETERS ARE INTRODUCED
C
C*    APARM(1) IS CALCULATED IN SUBROUTINE FORCE
C*
C
      DATA ALPH / 0.939D0,1.029955D0,0.939D0 /
      DATA RE / 2.0673D0,1.402D0,2.0673D0 /
      DATA BETA / 0.939D0,1.029955D0,0.939D0 /
      DATA AA / 0.0D0,0.0D0,-2.3603D0 /
      DATA REFV,TAU,CP / 0.71098329D0,1.9105588D0,0.080768D0 /
      DATA APARM / 4.0D0,0.8999975D0,0.448035D0,0.606528D0,3.239921D0 /
C
      DATA APHI,BPHI,CPHI / 2.7982366D-1,5.937217D-2,3.6301534D0 /
      DATA PCH4,FCH3,HCH3,RNOT / 1.9106D0,2.19D-2,4.38D-02,2.052D0 /
      DATA A3,B3,DELT,DIJ / 2.102956D-2,5.798533D-1,0.1D0,3.1190D0 /
      DATA A,B,C0,D / -1.56944D+3,-1.12008D+3,1.5786151D+3,2.67146D0 /
      DATA PI,CA,CB / 3.141592654D0,0.903D0,0.072D0 /
      DATA REX,FK0,FA / 1.18584D0,2.55D0,0.8D0 /
      END
      SUBROUTINE OOPB (VOOP,DVOOP,V,X,DX,N3TM)
C
C     THIS SUBROUTINE CALCULTES THE OUT OF PLANE BEND POTENTIAL THAT
C     RESULTS FROM THE METHYL.  THE FORM USED IS THE SAME AS THAT
C     GIVEN BY DUCHOVIC ET. AL. (JPC 89, 1339, (1984)).
C     THE DERIVATIVES WITH RESPECT TO THE CARTESIAN COORDINATES (X)
C     ARE ALSO COMPUTED AND STORED IN DVOOP.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DVOOP(18),XDIF(2),YDIF(2),ZDIF(2),RV(3,3),DRV(3,3,18)
      DIMENSION XMAGR(3),DXMAGR(3,18),DELTA(3),DDELTA(3,18)
      DIMENSION GAMMA(3),DGAMMA(3,18),TOP(3),DTOP(3,18),DCROSS(3,18)
      DIMENSION DGCROS(18),DPHI(18),DFDX(18),DHDX(18),DRDX(18)
C
C
      DIMENSION X(N3TM),DX(N3TM)
C
      COMMON /TESTCM/ GAMMA,DGAMMA
      COMMON /PDT2CM/ PI,APHI,BPHI,CPHI,PCH4,FCH3,HCH3,RNOT,A3,B3,DELT, 
     *   DIJ,A0,B0,C0,D,REX
C
C     START BY COMPUTING THE NECESSARY DIFFERENCES IN THE R1,R2 AND
C     R3 VECTORS.
C
      XDIF(1) = X(7)-X(4)
      YDIF(1) = X(8)-X(5)
      ZDIF(1) = X(9)-X(6)
      XDIF(2) = X(10)-X(4)
      YDIF(2) = X(11)-X(5)
      ZDIF(2) = X(12)-X(6)
C
C     NOW COMPUTE THE INDIVIDUAL COMPONENTS OF THE R1,R2 AND R3 VECTORS
C
      J = 1
      DO 10 I = 1, 3
         RV(I,1) = X(J+3)-X(13)
         RV(I,2) = X(J+4)-X(14)
         RV(I,3) = X(J+5)-X(15)
         J = J+3
   10 CONTINUE
C
C     RSTAR IS THE C-H4 BOND LENGTH
C
      RSTAR = SQRT((X(1)-X(13))**2+(X(2)-X(14))**2+(X(3)-X(15))**2)
C
C     COMPUTE THE CROSS PRODUCT OF THESE DIFFERENCES
C
      CROSSX = YDIF(1)*ZDIF(2)-YDIF(2)*ZDIF(1)
      CROSSY = ZDIF(1)*XDIF(2)-XDIF(1)*ZDIF(2)
      CROSSZ = XDIF(1)*YDIF(2)-XDIF(2)*YDIF(1)
      GCROSS = SQRT(CROSSX**2+CROSSY**2+CROSSZ**2)
C
C     COMPUTE THE THREE TRIPLE PRODUCTS--WITH R1,R2 AND R3 (TOP)
C     AND THEN COMPUTE THE ANGLE GAMMA USED IN DELTA.
C
      DO 20 I = 1, 3
         XMAGR(I) = SQRT(RV(I,1)**2+RV(I,2)**2+RV(I,3)**2)
         TOP(I) = CROSSX*RV(I,1)+CROSSY*RV(I,2)+CROSSZ*RV(I,3)
         GAMMA(I) = +TOP(I)/(GCROSS*XMAGR(I))
   20 CONTINUE
C
C     NOW THE SECOND TERM IN DELTA--PHINOT
C     FIRST THE SWITCHING FUNCTION SPHI THEN PHINOT
C
      SPHI = 0.0D0
      REST = BPHI*(RSTAR-CPHI)**3
      IF (REST.LT.70.0D0) THEN
         EREST = EXP(REST)
         SPHI = 1.0D0-TANH(APHI*(RSTAR-RNOT)*EREST)
      ENDIF
      PHINOT = PCH4+(PCH4-PI/2.0D0)*(SPHI-1.0D0)
C
C     NOW PUT THESE TWO TERMS TOGETHER TO GET EACH DELTA
C
      DO 30 I = 1, 3
         DELTA(I) = ACOS(GAMMA(I))-PHINOT
   30 CONTINUE
C
C     NEXT COMPUTE THE PARTIALS OF DELTA W/R TO EACH X(I)
C      IN DOING THIS COMPUTATION FIRST FOR EACH TERM CALCULATED
C      ABOVE THE PARTIAL WILL BE COMPUTED AND A STORED USING THE
C      SAME VARIABLE WITH A 'D' IN FRONT.
C
C
C     START WITH THE PARTIAL OF CROSS WITH RESPECT TO EACH X(I)
C
      DO 60 I = 1, 3
         DO 40 J = 1, 3
            DCROSS(I,J) = 0.0D0
   40    CONTINUE
         DO 50 J = 13, 18
            DCROSS(I,J) = 0.0D0
   50    CONTINUE
         IF (I.EQ.1) THEN
            DCROSS(I,4) = 0.0D0
            DCROSS(I,5) = ZDIF(1)-ZDIF(2)
            DCROSS(I,6) = YDIF(2)-YDIF(1)
            DCROSS(I,7) = 0.0D0
            DCROSS(I,8) = ZDIF(2)
            DCROSS(I,9) = -YDIF(2)
            DCROSS(I,10) = 0.0D0
            DCROSS(I,11) = -ZDIF(1)
            DCROSS(I,12) = YDIF(1)
         ELSEIF (I.EQ.2) THEN
            DCROSS(I,4) = ZDIF(2)-ZDIF(1)
            DCROSS(I,5) = 0.0D0
            DCROSS(I,6) = XDIF(1)-XDIF(2)
            DCROSS(I,7) = -ZDIF(2)
            DCROSS(I,8) = 0.0D0
            DCROSS(I,9) = XDIF(2)
            DCROSS(I,10) = ZDIF(1)
            DCROSS(I,11) = 0.0D0
            DCROSS(I,12) = -XDIF(1)
         ELSEIF (I.EQ.3) THEN
            DCROSS(I,4) = YDIF(1)-YDIF(2)
            DCROSS(I,5) = XDIF(2)-XDIF(1)
            DCROSS(I,6) = 0.0D0
            DCROSS(I,7) = YDIF(2)
            DCROSS(I,8) = -XDIF(2)
            DCROSS(I,9) = 0.0D0
            DCROSS(I,10) = -YDIF(1)
            DCROSS(I,11) = XDIF(1)
            DCROSS(I,12) = 0.0D0
         ENDIF
   60 CONTINUE
C
C     NOW COMPUTE THE PARTIALS FOR THE THREE R VECTORS
C
      DO 70 I = 1, 3
         DO 70 J = 1, 3
            DO 70 K = 1, 18                                             0101BL05
               DRV(I,J,K) = 0.0D0
   70 CONTINUE
      DO 80 K = 4, 6
         DRV(1,K-3,K) = 1.0D0
   80 CONTINUE
      DO 90 K = 7, 9
         DRV(2,K-6,K) = 1.0D0
   90 CONTINUE
      DO 100 K = 10, 12
         DRV(3,K-9,K) = 1.0D0
  100 CONTINUE
      DO 110 I = 1, 3
         DO 110 J = 1, 3
            DRV(I,J,J+12) = -1.0D0
  110 CONTINUE
C
C     NOW COMPUTE THE PARTIALS FOR THE TRIPLE PRODUCT  (TOP)
C
      DO 120 I = 1, 18
         DO 120 J = 1, 3
            DTOP(J,I) = DCROSS(1,I)*RV(J,1)+CROSSX*DRV(J,1,I)+DCROSS(2,I
     *         )*RV(J,2)+CROSSY*DRV(J,2,I)+DCROSS(3,I)*RV(J,3)+CROSSZ*  
     *         DRV(J,3,I)
  120 CONTINUE
C
C     PARTIALS FOR THE MAGNITUDE OF THE CROSS PRODUCT  (GCROSS)
C
      DO 130 I = 1, 18
         DGCROS(I) = (1.0D0/GCROSS)*(DCROSS(1,I)*CROSSX+DCROSS(2,I)*    
     *      CROSSY+DCROSS(3,I)*CROSSZ)
  130 CONTINUE
C
C     PARTIALS FOR THE MAGNITUDE OF R1, R2 AND R3
C
      DO 150 I = 1, 3
         DO 150 J = 1, 18
            DXMAGR(I,J) = 0.0D0
            DO 140 K = 1, 3
               DXMAGR(I,J) = DXMAGR(I,J)+(1.0D0/XMAGR(I))*(RV(I,K)*DRV(I
     *            ,K,J))
  140       CONTINUE
  150 CONTINUE
C
C     NOW WE'RE READY FOR THE PARTIALS OF GAMMA
C
      DO 160 I = 1, 3
         DO 160 J = 1, 18
            DGAMMA(I,J) = +GCROSS*XMAGR(I)*DTOP(I,J)-TOP(I)*(XMAGR(I)*  
     *         DGCROS(J)+GCROSS*DXMAGR(I,J))
            DGAMMA(I,J) = DGAMMA(I,J)/(GCROSS*XMAGR(I))**2
            DGAMMA(I,J) = DGAMMA(I,J)*(-1.0D0)/(SQRT(1.0D0-GAMMA(I)**2)
     *         )
  160 CONTINUE
C
C     NOW THE PARTIAL FOR THE SECOND TERM IN DELTA -- PHINOT
C       FIRST COMPUTE THE PARTIAL OF S(SWITCHING FUNCTION) WITH
C       RESPECT TO RSTAR, THE THE PARTIAL OF RSTAR W/R X(I) AND
C       FINALLY THE PARTIAL OF PHINOT W/R TO S.
C
      DSDR = 0.0D0
      REST = BPHI*(RSTAR-CPHI)**3
      IF (REST.LT.70.0D0) THEN
         TEXP = EXP(REST)
         DSDR = -((SECH(APHI*(RSTAR-RNOT)*TEXP))**2)
         DSDR = DSDR*APHI*TEXP*(1.0D0+(RSTAR-RNOT)*BPHI*3.0D0*(RSTAR-   
     *      CPHI)**2)
      ENDIF
      DO 170 I = 1, 18
         DRDX(I) = 0.0D0
         DPHI(I) = 0.0D0
  170 CONTINUE
C
      CST = 1.0D0/(2.0D0*RSTAR)
      DO 180 I = 1, 3
         DRDX(I) = CST*2.0D0*(X(I)-X(I+12))
         DRDX(I+12) = -DRDX(I)
         DPHI(I) = DRDX(I)*DSDR*(PCH4-PI/2.0D0)
         DPHI(I+12) = -DPHI(I)
  180 CONTINUE
C
C     NOW PUT BOTH TERMS TOGETHER TO GET THE PARTIALS OF DELTA
C
      DO 190 I = 1, 3
         DO 190 J = 1, 18
            DDELTA(I,J) = DGAMMA(I,J)-DPHI(J)
  190 CONTINUE
C
C     WE HAVE NOW COMPUTED ALL THE DELTA TERMS.  NEXT COMPUTE THE
C       FORCE CONSTANTS (F AND H) AND THEIR DERIVATIVES.  THE
C       OUT-OF-PLANE BEND ENERGY WILL ALSO BE COMPUTED HERE (VOOP)
C
C     FIRST THE SWITCHING FUNCTION
C
      S3 = 1.0D0-TANH(A3*(RSTAR-RNOT)*(RSTAR-B3)**2)
      F = (1.0D0-S3)*FCH3
      H = (1.0D0-S3)*HCH3
      VOOP = 0.0D0
      DO 200 I = 1, 3
         VOOP = VOOP+F*(DELTA(I)**2)+H*(DELTA(I)**4)
  200 CONTINUE
C
C     PARTIAL OF THE SWITCHING FUNCTION W/R TO RSTAR
C
      DSDR = -(SECH(A3*(RSTAR-RNOT)*(RSTAR-B3)**2)**2)*A3*((RSTAR-B3)**2
     *   +(RSTAR-RNOT)*2*(RSTAR-B3))
      DFDS = -FCH3
      DHDS = -HCH3
C
C     PARTIAL OF EACH FORCE CONSTANT
C
      DO 210 I = 1, 18
         DFDX(I) = 0.0D0
         DHDX(I) = 0.0D0
  210 CONTINUE
      DO 220 I = 1, 3
         DFDX(I) = DFDS*DSDR*DRDX(I)
         DHDX(I) = DHDS*DSDR*DRDX(I)
         DFDX(I+12) = -DFDX(I)
         DHDX(I+12) = -DHDX(I)
  220 CONTINUE
C
C    NOW WE ARE READY FOR THE FULL PARTIAL DERIVATIVES OF VOOP
C
                                                                        
      DO 250 I = 1, 18
         TERM1 = 0.0D0
         DO 230 J = 1, 3
            TERM1 = TERM1+DFDX(I)*DELTA(J)**2
  230    CONTINUE
         TERM2 = 2.0D0*DELTA(1)*DDELTA(1,I)+2.0D0*DELTA(2)*DDELTA(2,I)
     *           +2.0D0*DELTA(3)*DDELTA(3,I)
         TERM2 = TERM2*F
         TERM3 = 0.0D0
         DO 240 J = 1, 3
            TERM3 = TERM3+DHDX(I)*DELTA(J)**4
  240    CONTINUE
         TERM4 = 4.0D0*DELTA(1)**3*DDELTA(1,I)+4.0D0*DELTA(2)**3*
     *           DDELTA(2,I)+4.0D0*DELTA(3)**3*DDELTA(3,I)
         TERM4 = TERM4*H
         DVOOP(I) = TERM1+TERM2+TERM3+TERM4
  250 CONTINUE
      RETURN
      END
C
      FUNCTION SECH (X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (X.GT.70.0D0) THEN
         SECH = 0.0D0
      ELSE
         SECH = 1.0D0/COSH(X)
      ENDIF
      RETURN
      END
                                                                        
      SUBROUTINE THETA (ASF,DASF,ASB,DASB)
C
C     THIS SUBROUTINE COMPUTES THE EQUILIBRIUM THETA AS A FUNCTION OF
C     R (THE AVERAGE C-H BOND LENGTH IN METHANE).
C     THE DERIVATIVE OF THETA WITH RESPECT TO R(I) IS ALSO COMPUTED.
C     NOTE HERE THAT BECAUSE OF SYMMETRY EACH OF THESE PARTIALS IS
C     THE SAME.       ADDED 11/22/85
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      COMMON /PDT2CM/ PI,APHI,BPHI,CPHI,PCH4,FCH3,HCH3,RNOT,A3,B3,DELTA,
     *   DIJ,A0,B0,C0,D0,REX
C
      RAVG = (R(1)+R(2)+R(3)+R(4))/4.0D0
C
C     COMPUTE THETA
C
      IF (RAVG.GT.DIJ) THEN
         ASF = PI/2.0D0
         DASF = 0.0D0
      ELSEIF (RAVG.LE.RE(1)) THEN
         ASF = TAU
         DASF = 0.0D0
      ELSEIF (RAVG.GT.RE(1).AND.RAVG.LE.RE(1)+DELTA) THEN
         DELTA2 = DELTA*DELTA
         FK1 = -4.0D0*CP/(DELTA2)
         A = 3.0D0*FK1/(DELTA2)
         B = -(5.0D0*A*DELTA2+FK1)/(2.0D0*DELTA)
         C = -(20.0D0*A*DELTA2+12.0D0*B*DELTA)/6.0D0
         F = TAU
         RDIF = RAVG-RE(1)
         RDIF2 = RDIF*RDIF
         RDIF3 = RDIF2*RDIF
         RDIF4 = RDIF3*RDIF
         RDIF5 = RDIF4*RDIF
         ASF = A*RDIF5+B*RDIF4+C*RDIF3+F
         DASF = 5.0D0*A*RDIF4+4.0D0*B*RDIF3+3.0D0*C*RDIF2
      ELSEIF (RAVG.LE.DIJ-DELTA) THEN
         ASF = TAU-4.0D0*CP*(RAVG-RE(1))
         DASF = -4.0D0*CP
      ELSEIF (RAVG.LE.DIJ.AND.RAVG.GT.DIJ-DELTA) THEN
         FK1 = (TAU-4.0D0*CP*(DIJ-DELTA-RE(1))-F)/(DELTA**3)
         FK2 = (-4.0D0*CP)/(DELTA**2)
         A = -3.0D0*(2.0D0*FK1+FK2)/(DELTA**2)
         B = (FK2+5.0D0*A*DELTA**2)/(2.0D0*DELTA)
         C = (12.0D0*B*DELTA-20.0D0*A*DELTA**2)/6.0D0
         F = PI/2.0D0
         RDIF = RAVG-DIJ
         RDIF3 = RDIF*RDIF*RDIF
         RDIF4 = RDIF3*RDIF
         RDIF5 = RDIF4*RDIF
         ASF = A*RDIF5+B*RDIF4+C*RDIF3+F
         DASF = 5.0D0*A*RDIF4+4.0D0*B*RDIF3+3.0D0*C*RDIF*RDIF
      ENDIF
C
C COMPUTE BACKSIDE EQUILIBRIUM ANGLE
C
      XYZ = COS(ASF)
      CTAU = COS(TAU)
      CTAU2 = CTAU*CTAU
      A = (CTAU+0.5D0)/CTAU2
      ASB = ACOS(A*XYZ*XYZ-0.5D0)
      DASB = 2.0D0*A*XYZ*SIN(ASF)*DASF/SIN(ASB)
C
C NOW COMPUTE THE DER W/R TO RI INSTEAD OF RAVG
C
      DASF = DASF/4.0D0
      DASB = DASB/4.0D0
      RETURN
      END
      SUBROUTINE POT (ISWTCH, COORD, DXX, N3TM)
C
C     SUBROUTINE TO CALCULATE THE CH4-H SURFACE
C     REQUIRES SUBROUTINES DIST,ANGLE, TRI, AND SWITCH
C     ALSO COMPUTES DE/DR FOR ALL R AND STORES RESULTS IN ARRAY DER
C     DEFINITION OF CARTESIAN COORDINATES X(I), Y(I), AND Z(I)
C      I = 1, 2, 3, 4   REFERS TO H1, H2, H3, H4
C      I = 5            REFERS TO C
C      I = 6            REFERS TO H'
C
C     DEFINITION OF THE R-ARRAY  R(I)
C     H1 IS THE ATOM THAT WILL FORM H-H WITH H5 -- IN ALTERNATE RAFF
C      I = 1, 2, 3, 4   REFERS TO C - H(I)
C      I = 5            REFERS TO C - H'
C      I = 6, 7, 8, 9   REFERS TO H' - H(I)
C      I = 10, 11, 12   REFERS TO H1 - H(2,3,4)
C      I = 13, 14       REFERS TO H2 - H(3,4)
C      I = 15           REFERS TO H3 - H4
C***********************************************************************
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DFR(4),DVOOP(18),Y1(6),Y2(6),F(4),DF(4),DK(6)
C
      DIMENSION COORD(N3TM),DXX(N3TM)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /POTXCM/ X(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
      COMMON /POTRCM/ AG(6),R2(15),ACS(6),ASS(6)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      COMMON /DERVCP/ DECCR1,DECCR5,DECCR6
      COMMON /TEMCOM/ RTEM(9),DTEMP(9),ETEM
      COMMON /ALPDCM/ DALP
      DATA EVCON / 2.721161D+1 /
C
C COMPUTE THE INTERPARTICLE DISTANCES R
C
      CALL DISTA
C
C***********************************************************************
C CHECK FOR THE FIFTH ATOM--I.E.--CH4---(H)
C  BY DEFINITION THE FIFTH H ATOM IS THE ONE WHOSE C-H DISTANCE IS MAXIM
C********************************
C
      RHMAX = R(1)
      KMAX = 1
      IF (R(2).LT.RHMAX) GO TO 10
      RHMAX = R(2)
      KMAX = 2
   10 IF (R(3).LT.RHMAX) GO TO 20
      RHMAX = R(3)
      KMAX = 3
   20 IF (R(4).LT.RHMAX) GO TO 30
      RHMAX = R(4)
      KMAX = 4
   30 IF (R(5).LT.RHMAX) GO TO 40
      RHMAX = R(5)
      KMAX = 5
   40 CALL SWITCH (KMAX,1, V, COORD, DXX, N3TM)
C
C **********************************************************************
C***********************************************************************
C COMPUTE THE HCH ANGLES AND STORE RESULTS IN ARRAY AG
C
      CALL ANGLE
C
C***********************************************************************
C
      DO 50 I = 1, 15
         DER(I) = 0.0D0
   50 CONTINUE
C
C  COMPUTE THE MORSE RANGE PARAMETERS ALPH:
C
      CALL CALPHA
C
C
C COMPUTE THE FOUR TRIATOMIC TERMS
C
      CALL TRI (R(1),R(5),R(6),DER(1),DER(5),DER(6),1,3,2,EE,DTOT)
      DER(1) = DER(1)+DTOT
      DER2 = DTOT
      DER3 = DTOT
      DER4 = DTOT
      CALL TRI (R(2),R(5),R(7),DER(2),DR2,DER(7),1,3,2,E,DTOT)
      EE = EE+E
      DER(2) = DER(2)+DER2+DTOT
      DER(1) = DER(1)+DTOT
      DER3 = DER3+DTOT
      DER4 = DER4+DTOT
      DER(5) = DER(5)+DR2
      CALL TRI (R(3),R(5),R(8),DER(3),DR2,DER(8),1,3,2,E,DTOT)
      EE = EE+E
      DER(3) = DER(3)+DER3+DTOT
      DER(1) = DER(1)+DTOT
      DER(2) = DER(2)+DTOT
      DER4 = DER4+DTOT
      DER(5) = DER(5)+DR2
      CALL TRI (R(4),R(5),R(9),DER(4),DR2,DER(9),1,3,2,E,DTOT)
      EE = EE+E
      DER(4) = DER(4)+DER4+DTOT
      DER(1) = DER(1)+DTOT
      DER(2) = DER(2)+DTOT
      DER(3) = DER(3)+DTOT
      DER(5) = DER(5)+DR2
C
C***********************************************************************
C COMPUTE FORCE CONSTANTS
C COMPUTE ATTENUATION TERMS
C
      XA = R(1)-RE(1)
      XB = R(2)-RE(1)
      XC = R(3)-RE(1)
      XD = R(4)-RE(1)
      XA2 = XA*XA
      XB2 = XB*XB
      XC2 = XC*XC
      XD2 = XD*XD
      XXA = R(6)-RE(2)
      XXB = R(7)-RE(2)
      XXC = R(8)-RE(2)
      XXD = R(9)-RE(2)
      EXA11 = EXP(-APARM(2)*R2(6))
      EXA12 = EXP(-APARM(2)*R2(7))
      EXA13 = EXP(-APARM(2)*R2(8))
      EXA14 = EXP(-APARM(2)*R2(9))
      EXA21 = EXP(-APARM(5)*XXA*XXA)
      EXA22 = EXP(-APARM(5)*XXB*XXB)
      EXA23 = EXP(-APARM(5)*XXC*XXC)
      EXA24 = EXP(-APARM(5)*XXD*XXD)
      A11 = 1.0D0-EXA11
      A12 = 1.0D0-EXA12
      A13 = 1.0D0-EXA13
      A14 = 1.0D0-EXA14
      A21 = APARM(3)+APARM(4)*EXA21
      A22 = APARM(3)+APARM(4)*EXA22
      A23 = APARM(3)+APARM(4)*EXA23
      A24 = APARM(3)+APARM(4)*EXA24
      EXF1 = EXP(-A21*XA2)
      EXF2 = EXP(-A22*XB2)
      EXF3 = EXP(-A23*XC2)
      EXF4 = EXP(-A24*XD2)
      F(1) = A11*EXF1
      F(2) = A12*EXF2
      F(3) = A13*EXF3
      F(4) = A14*EXF4
C
C DK'S ARE THE FORCE CONSTANTS
C     FIRST COMPUTE THE K0 SINCE IT IS NOW A FUNCTION OF R(1)
C
      CALL FORCE (DADR1)
      DK(1) = APARM(1)*F(1)*F(2)
      DK(2) = APARM(1)*F(1)*F(3)
      DK(3) = APARM(1)*F(1)*F(4)
      DK(4) = APARM(1)*F(2)*F(3)
      DK(5) = APARM(1)*F(2)*F(4)
      DK(6) = APARM(1)*F(3)*F(4)
C
C***********************************************************************
C COMPUTE THE EQUILIBRIUM ANGLES--THETA AND D(THETA)/DR
C
      CALL THETA (ASF,DASF,ASB,DASB)
      DAA = 0.0D0
C
C***********************************************************************
C COMPUTE CONTRIBUTION TO THE TOTAL ENERGY FROM THE ANGLE TERMS
C
      SUM = 0.0D0
      IF1 = 1
      IF2 = 2
      IF3 = 3
      IB1 = 4
      IB2 = 5
      IB3 = 6
      DO 60 I = 1, 3
         O = AG(I)-ASF
         Y1(I) = O
         Y2(I) = O*O
         SUM = SUM+DK(I)*Y2(I)
   60 CONTINUE
      DO 70 I = 4, 6
         Y1(I) = AG(I)-ASB
         Y2(I) = Y1(I)*Y1(I)
         SUM = SUM+DK(I)*Y2(I)
   70 CONTINUE
      EE = EE+0.5D0*SUM
C
C***********************************************************************
C COMPUTE DERIVATIVES OF THE ANGLE ATTENUATION TERMS WITH RESPECT TO R6,
C                       AND R9
C
      DFR(1) = 2.0D0*APARM(2)*R(6)*EXA11*EXF1+A11*EXF1*XA2*2.0D0*APARM(4
     *   )*APARM(5)*XXA*EXA21
      DFR(2) = 2.0D0*APARM(2)*R(7)*EXA12*EXF2+A12*EXF2*XB2*2.0D0*APARM(4
     *   )*APARM(5)*XXB*EXA22
      DFR(3) = 2.0D0*APARM(2)*R(8)*EXA13*EXF3+A13*EXF3*XC2*2.0D0*APARM(4
     *   )*APARM(5)*XXC*EXA23
      DFR(4) = 2.0D0*APARM(2)*R(9)*EXA14*EXF4+A14*EXF4*XD2*2.0D0*APARM(4
     *   )*APARM(5)*XXD*EXA24
C
C COMPUTE DERIVATIVES OF ATTENUATION TERMS WITH RESPECT TO R1,R2,R3, AND
C
      DF(1) = -2.0D0*A21*XA*F(1)
      DF(2) = -2.0D0*A22*XB*F(2)
      DF(3) = -2.0D0*A23*XC*F(3)
      DF(4) = -2.0D0*A24*XD*F(4)
C
C***********************************************************************
C ADD IN CONTRIBUTION TO DER FROM DK/DR ANGLE TERMS AND D(THETA)/DR TERM
C FIRST THREE ANGLE TERMS
C
      DO 80 I = 1, 3
         STP = 0.5D0*DK(I)*Y2(I)
         W = DK(I)*Y1(I)
         IF (ABS(F(1)).GT.1.0D-10) DER(1)=DER(1)+STP*(DF(1)/F(1)+DADR1/
     *      APARM(1))+W*(-1.0D0/(R(I+1)*ASS(I))+ACS(I)/(R(1)*ASS(I)))
         IF (ABS(F(I+1)).GT.1.0D-10) DER(I+1)=DER(I+1)+STP*DF(I+1)/F(I+
     *      1)+W*(-1.0D0/(R(1)*ASS(I))+ACS(I)/(ASS(I)*R(I+1)))
         IF (ABS(F(1)).GT.1.0D-10) DER(6)=DER(6)+STP*DFR(1)/F(1)
         IF (ABS(F(I+1)).GT.1.0D-10) DER(I+6)=DER(I+6)+STP*DFR(I+1)/F(I
     *      +1)
C
C     DER(1)=DER(1)+STP*(DF(1)/F(1)+DADR1/APARM(1)) + W*(-1.0D0/
C    1(R(I+1)*ASS(I))+ACS(I)/(R(1)*ASS(I)))
C     DER(I+1)=DER(I+1)+STP*DF(I+1)/F(I+1)+W*(-1.0D0/(R(1)*ASS(I))+
C    1ACS(I)/(ASS(I)*R(I+1)))
C     DER(6)=DER(6)+STP*DFR(1)/F(1)
C     DER(I+6)=DER(I+6)+STP*DFR(I+1)/F(I+1)
C
         DER(I+9) = DER(I+9)+W*R(I+9)/(R(1)*R(I+1)*ASS(I))
   80 CONTINUE
C
C **********************************************************************
C FOURTH AND FIFTH ANGLE TERMS
C
      DO 90 I = 4, 5
         STP = 0.5D0*DK(I)*Y2(I)
         W = DK(I)*Y1(I)
         DER(1) = DER(1)+STP*DADR1/APARM(1)
         DER(2) = DER(2)+STP*DF(2)/F(2)+W*(-1.0D0/(ASS(I)*R(I-1))+ACS(I)
     *      /(ASS(I)*R(2)))
         DER(7) = DER(7)+STP*DFR(2)/F(2)
         IF (ABS(F(I-1)).GT.1.0D-10) DER(I-1)=DER(I-1)+STP*DF(I-1)/F(I-
     *      1)+W*(-1.0D0/(ASS(I)*R(2))+ACS(I)/(ASS(I)*R(I-1)))
C
C     DER(I-1)=DER(I-1)+STP*DF(I-1)/F(I-1)+W*(-1.0D0/(ASS(I)*R(2))
C    1+ACS(I)/(ASS(I)*R(I-1)))
C
         IF (ABS(F(I-1)).GT.1.0D-10) DER(I+4)=DER(I+4)+STP*DFR(I-1)/F(I
     *      -1)
C
C     DER(I+4)=DER(I+4)+STP*DFR(I-1)/F(I-1)
C
         DER(I+9) = DER(I+9)+W*R(I+9)/(R(2)*R(I-1)*ASS(I))
   90 CONTINUE
C
C CONTRIBUTION FROM SIXTH ANGLE TERM
C
      STP = 0.5D0*DK(6)*Y2(6)
      W = DK(6)*Y1(6)
      DER(1) = DER(1)+STP*DADR1/APARM(1)
      DER(3) = DER(3)+STP*DF(3)/F(3)+W*(-1.0D0/(ASS(6)*R(4))+ACS(6)/(ASS
     *   (6)*R(3)))
      DER(8) = DER(8)+STP*DFR(3)/F(3)
      DER(4) = DER(4)+STP*DF(4)/F(4)+W*(-1.0D0/(ASS(6)*R(3))+ACS(6)/(ASS
     *   (6)*R(4)))
      DER(9) = DER(9)+STP*DFR(4)/F(4)
      DER(15) = DER(15)+W*R(15)/(ASS(6)*R(3)*R(4))
C
C **********************************************************************
C CONTRIBUTION FROM EQ. ANGLE VARIATION TO D(ANGLE)/DR******
C
      DO 100 KLM = 1, 4
         DER(KLM) = DER(KLM)-(DK(IF1)*Y1(IF1)+DK(IF2)*Y1(IF2)+DK(IF3)*Y1
     *      (IF3))*DASF-(DK(IB1)*Y1(IB1)+DK(IB2)*Y1(IB2)+DK(IB3)*Y1(IB3)
     *      )*DASB
  100 CONTINUE
      DER(5) = DER(5)-(DK(IF1)*Y1(IF1)+DK(IF2)*Y1(IF2)+DK(IF3)*Y1(IF3))*
     *   DAA-(DK(IB1)*Y1(IB1)+DK(IB2)*Y1(IB2)+DK(IB3)*Y1(IB3))*3.0D0*   
     *   SIN(ASF)*COS(ASF)*DAA/SIN(ASB)
C
C **********************************************************************
C
      CALL SWITCH (KMAX,2, V, COORD, DXX, N3TM)
C
C      ENERGY = (EE * 3.674902D-2) + 0.7118873153D0
C
      ENERGY = EE/EVCON+REFV
      DO 110 I = 1, 15
         DER(I) = DER(I)/EVCON
  110 CONTINUE
      IF (ISWTCH.EQ.1) RETURN
C
C      CALCULATE DRDX ARRAY
C
      CALL DRDXC
C
C      CALCULATE DEDX = DEDR(DER) * DRDX
C
      CALL MULT (DER,1,15,DRDX,15,18,DEDX)
      IPT = 1
      DO 120 I = 1, 6
         DXX(IPT) = DEDX(I)
         DXX(IPT+1) = DEDX(I+6)
         DXX(IPT+2) = DEDX(I+12)
         IPT = IPT+3
  120 CONTINUE
C
C     NOW ADD IN THE OUT OF PLANE BEND TERM
C
      CALL OOPB (VOOP,DVOOP,V,COORD,DXX,N3TM)
      ENERGY = ENERGY+VOOP
      DO 130 I = 1, 18
         DXX(I) = DXX(I)+DVOOP(I)
  130 CONTINUE
C
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE TRI (RR1,RR2,RR3,DR1,DR2,DR3,I,J,K,E,DTOT)
C
C ROUTINE TO CALCULATE THREE-BODY ENERGY FOR ABC SYSTEM
C RR1,RR2,RR3 ARE THE THREE INTERPARTICLE DISTANCES
C DR1,DR2,DR3 ARE THE THREE DERIVATIVES WITH RESPECT TO RR1,RR2, AND RR3
C I,J, AND K GIVE THE ARRAY NUMBER FOR THE APPROPRIATE POTENTIAL PARAMET
C E IS THE FINAL THREE-BODY ENERGY
C***********************************************************************
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /POTXCM/ X(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
      COMMON /POTRCM/ AG(6),R2(15),ACS(6),ASS(6)
      COMMON /DERVCP/ DECCR1,DECCR5,DECCR6
      COMMON /ALPDCM/ DALP
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,BP1,CP1
      A = RR1-RE(I)
      B = RR2-RE(J)
      C = RR3-RE(K)
      A1 = EXP(-ALPH(I)*A)
      B1 = EXP(-ALPH(J)*B)
      C1 = EXP(-ALPH(K)*C)
      A2 = A1*A1
      B2 = B1*B1
      C2 = C1*C1
      E1AB = D1(I)*(A2-2.0D0*A1)
      E1AC = D1(J)*(B2-2.0D0*B1)
      E1BC = D1(K)*(C2-2.0D0*C1)
      DE1AB = 2.0D0*D1(I)*(A1-A2)*ALPH(I)
      DAB1 = 2.0D0*D1(I)*(A1-A2)*(A*DALP)
      DE1AC = 2.0D0*D1(J)*(B1-B2)*ALPH(J)
      DAC1 = 2.0D0*D1(J)*(B1-B2)*(B*DALP)
      DE1BC = 2.0D0*ALPH(K)*D1(K)*(C1-C2)
      AA1 = EXP(-BETA(I)*A)
      AA2 = AA1*AA1
      E3AB = D3(I)*(AA2+2.0D0*AA1)
      DE3AB = -2.0D0*D3(I)*(AA1+AA2)*BETA(I)
      DAB3 = -2.0D0*D3(I)*(AA1+AA2)*A*DALP
      BB1 = EXP(-BETA(J)*B)
      BB2 = BB1*BB1
      E3AC = D3(J)*(BB2+2.0D0*BB1)
      DE3AC = -2.0D0*D3(J)*(BB1+BB2)*BETA(J)
      DAC3 = -2.0D0*D3(J)*(BB1+BB2)*B*DALP
      CC1 = EXP(-BETA(K)*C)
      CC2 = CC1*CC1
      E3BC = D3(K)*(CC2+2.0D0*CC1)
      DE3BC = -2.0D0*BETA(K)*D3(K)*(CC1+CC2)
      QAB = (E1AB+E3AB)/2.0D0
      QAC = (E1AC+E3AC)/2.0D0
      QBC = (E1BC+E3BC)/2.0D0
      ALAB = (E1AB-E3AB)/2.0D0
      ALAC = (E1AC-E3AC)/2.0D0
      ALBC = (E1BC-E3BC)/2.0D0
      XX = ALAB-ALBC
      YY = ALBC-ALAC
      ZZ = ALAC-ALAB
      U = (XX*XX+YY*YY+ZZ*ZZ)/2.0D0
      U = SQRT(U)
      E = QAB+QBC+QAC-U
      BB = 4.0D0*U
      DXR = (DAB1-DAB3)/2.0D0
      DYR = (DAC3-DAC1)/2.0D0
      DZR = -(DXR+DYR)
      DUX = XX/(2.0D0*U)
      DUY = YY/(2.0D0*U)
      DUZ = ZZ/(2.0D0*U)
      DUR = DUX*DXR+DUY*DYR+DUZ*DZR
      DQR = (DAB1+DAC1+DAB3+DAC3)/2.0D0
      DTOT = DQR-DUR
      DR1 = (DE1AB+DE3AB)/2.0D0-(2.0D0*ALAB-ALBC-ALAC)*(DE1AB-DE3AB)/BB
      DR2 = (DE1AC+DE3AC)/2.0D0-(2.0D0*ALAC-ALAB-ALBC)*(DE1AC-DE3AC)/BB
      DR3 = (DE1BC+DE3BC)/2.0D0-(2.0D0*ALBC-ALAC-ALAB)*(DE1BC-DE3BC)/BB
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE ANGLE
C
C ROUTINE TO COMPUTE CH4 ANGLES FROM THE INTERPARTICLE DISTANCES
C***********************************************************************
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /POTXCM/ XX(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
      COMMON /POTRCM/ AG(6),R2(15),ACS(6),ASS(6)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      DO 10 I = 1, 3
         X = (R2(1)+R2(I+1)-R2(I+9))/(2.0D0*R(1)*R(I+1))
         ACS(I) = X
         ASS(I) = SQRT(1.0D0-X*X)
         AG(I) = ACOS(X)
   10 CONTINUE
      DO 20 I = 1, 2
         X = (R2(2)+R2(I+2)-R2(I+12))/(2.0D0*R(2)*R(I+2))
         ACS(I+3) = X
         ASS(I+3) = SQRT(1.0D0-X*X)
         AG(I+3) = ACOS(X)
   20 CONTINUE
      X = (R2(3)+R2(4)-R2(15))/(2.0D0*R(3)*R(4))
      ACS(6) = X
      ASS(6) = SQRT(1.0D0-X*X)
      AG(6) = ACOS(X)
      RETURN
      END
C
C***********************************************************************
C***********************************************************************
C
      SUBROUTINE DISTA
C
C ROUTINE TO COMPUTE THE INTERPARTICLE DISTANCES FROM THE CARTESIAN
C     COORDINATES OF THE ATOMS
C***********************************************************************
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DF(15,3)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /POTXCM/ X(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
      COMMON /POTRCM/ AG(6),R2(15),ACS(6),ASS(6)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      DO 10 I = 1, 4
         DF(I,1) = X(I)-X(5)
         DF(I,2) = Y(I)-Y(5)
         DF(I,3) = Z(I)-Z(5)
         R2(I) = DF(I,1)*DF(I,1)+DF(I,2)*DF(I,2)+DF(I,3)*DF(I,3)
         R(I) = SQRT(R2(I))
   10 CONTINUE
      DF(5,1) = X(5)-X(6)
      DF(5,2) = Y(5)-Y(6)
      DF(5,3) = Z(5)-Z(6)
      R2(5) = DF(5,1)*DF(5,1)+DF(5,2)*DF(5,2)+DF(5,3)*DF(5,3)
      R(5) = SQRT(R2(5))
      DO 20 I = 1, 4
         J = I+5
         DF(J,1) = X(I)-X(6)
         DF(J,2) = Y(I)-Y(6)
         DF(J,3) = Z(I)-Z(6)
         R2(J) = DF(J,1)*DF(J,1)+DF(J,2)*DF(J,2)+DF(J,3)*DF(J,3)
         R(J) = SQRT(R2(J))
   20 CONTINUE
      DO 30 I = 1, 3
         J = I+1
         K = I+9
         DF(K,1) = X(1)-X(J)
         DF(K,2) = Y(1)-Y(J)
         DF(K,3) = Z(1)-Z(J)
         R2(K) = DF(K,1)*DF(K,1)+DF(K,2)*DF(K,2)+DF(K,3)*DF(K,3)
         R(K) = SQRT(R2(K))
   30 CONTINUE
      DO 40 I = 1, 2
         J = I+2
         K = I+12
         DF(K,1) = X(2)-X(J)
         DF(K,2) = Y(2)-Y(J)
         DF(K,3) = Z(2)-Z(J)
         R2(K) = DF(K,1)*DF(K,1)+DF(K,2)*DF(K,2)+DF(K,3)*DF(K,3)
         R(K) = SQRT(R2(K))
   40 CONTINUE
      DF(15,1) = X(3)-X(4)
      DF(15,2) = Y(3)-Y(4)
      DF(15,3) = Z(3)-Z(4)
      R2(15) = DF(15,1)*DF(15,1)+DF(15,2)*DF(15,2)+DF(15,3)*DF(15,3)
      R(15) = SQRT(R2(15))
      RETURN
      END
      SUBROUTINE SWITCH (KMAX,LMG, V, COORD, DXX, N3TM)
C
C **********************************************************************
C THIS ROUTINE SWITCHES MATRICES R,R2,DER TO ALLOW FOR DIFFERENT H ATOMS
C      BEING OFF THE CH4 MOLECULE.
C LMG IS A CONTROL PARAMETER
C KMAX IS THE LABEL OF THE H ATOM WITH MAXIMUM C-H DISTANCE.
C IF LMG=1, R AND R2 MATRICES ARE SWITCHED APPROPRIATELY.
C IF LMG=2, R AND R2 MATRICES ARE SWITCHED BACK AND MATRIX DER IS SWITCH
C **********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION COORD(N3TM),DXX(N3TM)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /POTXCM/ X(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
      COMMON /POTRCM/ AG(6),R2(15),ACS(6),ASS(6)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      GO TO (10,20,30,40,50), KMAX
C
C H1 IS MAXIMUM
C
   10 K = 1
      KK = 5
      L = 7
      LL = 10
      M = 8
      MM = 11
      N = 9
      NN = 12
      GO TO 60
C
C H2 IS MAXIMUM
C
   20 K = 2
      KK = 5
      L = 6
      LL = 10
      M = 8
      MM = 13
      N = 9
      NN = 14
      GO TO 60
C
C H3 IS MAXIMUM
C
   30 K = 3
      KK = 5
      L = 6
      LL = 11
      M = 7
      MM = 13
      N = 9
      NN = 15
      GO TO 60
C
C H4 IS MAXIMUM
C
   40 K = 4
      KK = 5
      L = 6
      LL = 12
      M = 7
      MM = 14
      N = 8
      NN = 15
      GO TO 60
C
C H5 IS MAXIMUM
C NO SWITCHES ARE REQUIRED
C
   50 GO TO 80
C
C SWITCH R AND R2 MATRICES
C
   60 STORE = R(K)
      R(K) = R(KK)
      R(KK) = STORE
      STORE = R(L)
      R(L) = R(LL)
      R(LL) = STORE
      STORE = R(M)
      R(M) = R(MM)
      R(MM) = STORE
      STORE = R(N)
      R(N) = R(NN)
      R(NN) = STORE
      R2(K) = R(K)*R(K)
      R2(KK) = R(KK)*R(KK)
      R2(L) = R(L)*R(L)
      R2(LL) = R(LL)*R(LL)
      R2(M) = R(M)*R(M)
      R2(MM) = R(MM)*R(MM)
      R2(N) = R(N)*R(N)
      R2(NN) = R(NN)*R(NN)
      IF (LMG-1) 80, 80, 70
C
C SWITCH DER MATRIX *******
C
   70 STORE = DER(K)
      DER(K) = DER(KK)
      DER(KK) = STORE
      STORE = DER(L)
      DER(L) = DER(LL)
      DER(LL) = STORE
      STORE = DER(M)
      DER(M) = DER(MM)
      DER(MM) = STORE
      STORE = DER(N)
      DER(N) = DER(NN)
      DER(NN) = STORE
   80 RETURN
      END
      SUBROUTINE DRDXC
C
C THIS SUBROUTINE CALCULATES DRDX
C
C     ADDED BY FBB AT THE U. OF MINN.  1/24/84
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /POTXCM/ X(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
      COMMON /POTRCM/ AG(6),R2(15),ACS(6),ASS(6)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
C
C
C ZERO OUT DRDX
C
      DO 10 I = 1, 18
         DO 10 J = 1, 15
            DRDX(J,I) = 0.0D0
   10 CONTINUE
C
C CALCULATE DRDX FOR C - H(1,2,3,4) BONDS
C
      DO 20 I = 1, 4
         DRDX(I,I) = (X(I)-X(5))/R(I)
         DRDX(I,5) = -DRDX(I,I)
         DRDX(I,I+6) = (Y(I)-Y(5))/R(I)
         DRDX(I,11) = -DRDX(I,I+6)
         DRDX(I,I+12) = (Z(I)-Z(5))/R(I)
         DRDX(I,17) = -DRDX(I,I+12)
   20 CONTINUE
C
C CALCULATE DRDX FOR C - H' BOND
C
      DRDX(5,5) = (X(5)-X(6))/R(5)
      DRDX(5,6) = -DRDX(5,5)
      DRDX(5,11) = (Y(5)-Y(6))/R(5)
      DRDX(5,12) = -DRDX(5,11)
      DRDX(5,17) = (Z(5)-Z(6))/R(5)
      DRDX(5,18) = -DRDX(5,17)
C
C CALCULATE DRDX FOR H - H' BONDS
C
      DO 30 I = 1, 4
         DRDX(5+I,I) = (X(I)-X(6))/R(5+I)
         DRDX(5+I,6) = -DRDX(5+I,I)
         DRDX(5+I,I+6) = (Y(I)-Y(6))/R(5+I)
         DRDX(5+I,12) = -DRDX(5+I,I+6)
         DRDX(5+I,I+12) = (Z(I)-Z(6))/R(5+I)
         DRDX(5+I,18) = -DRDX(5+I,I+12)
   30 CONTINUE
C
C CALCULATE DRDX FOR H1 - H(2,3,4) BONDS
C
      DO 40 I = 1, 3
         DRDX(9+I,I+1) = -(X(1)-X(I+1))/R(9+I)
         DRDX(9+I,1) = -DRDX(9+I,I+1)
         DRDX(9+I,I+7) = -(Y(1)-Y(I+1))/R(9+I)
         DRDX(9+I,7) = -DRDX(9+I,I+7)
         DRDX(9+I,I+13) = -(Z(1)-Z(I+1))/R(9+I)
         DRDX(9+I,13) = -DRDX(9+I,I+13)
   40 CONTINUE
C
C CALCULATE DRDX FOR H2 - H(3,4)
C
      DO 50 I = 1, 2
         DRDX(12+I,I+2) = -(X(2)-X(I+2))/R(12+I)
         DRDX(12+I,2) = -DRDX(12+I,I+2)
         DRDX(12+I,I+8) = -(Y(2)-Y(I+2))/R(12+I)
         DRDX(12+I,8) = -DRDX(12+I,I+8)
         DRDX(12+I,I+14) = -(Z(2)-Z(I+2))/R(12+I)
         DRDX(12+I,14) = -DRDX(12+I,I+14)
   50 CONTINUE
C
C CALCULATE DRDX FOR H3 - H4
C
      DRDX(15,4) = -(X(3)-X(4))/R(15)
      DRDX(15,3) = -DRDX(15,4)
      DRDX(15,10) = -(Y(3)-Y(4))/R(15)
      DRDX(15,9) = -DRDX(15,10)
      DRDX(15,16) = -(Z(3)-Z(4))/R(15)
      DRDX(15,15) = -DRDX(15,16)
C
      RETURN
      END
      SUBROUTINE ARRAYO (TITLE,A,N,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS PROGRAM WRITES OUT SMALL MATRICES
C
      DIMENSION A(N,M)
C
      WRITE (6,1000) TITLE
C
      DO 10 I = 1, N
C
         WRITE (6,1050) I,(A(I,J),J=1,M)
C
   10 CONTINUE
      RETURN
C
 1000 FORMAT(///' ',A8,' :'/)
 1050 FORMAT(/I3,6D20.8,(/3X,6D20.8))
C
      END
      SUBROUTINE MULT (A,NA,MA,B,NB,MB,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS SUBROUTINE PERFORMS THE MATRIX MULTIPLICATION :
C
C         C = A X B
C
C
      DIMENSION A(NA,MA),B(NB,MB),C(NA,MB)
C
      DO 30 I = 1, NA
C
         DO 20 J = 1, MB
C
            SUM = 0.0D0
            DO 10 K = 1, MA
C
               SUM = SUM+A(I,K)*B(K,J)
   10       CONTINUE
C
            C(I,J) = SUM
   20    CONTINUE
C
   30 CONTINUE
      RETURN
      END
      SUBROUTINE DZERO (A,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               0220BL05
      DIMENSION A(N)                                                    0220BL05
      DO 10 I = 1, N
         A(I) = 0.0D0
   10 CONTINUE
      RETURN
      END
      SUBROUTINE TRIPLE
C
C     THIS SUBROUTINE COMPUTES THE C-H' SATO PARAMETER WHICH IS A
C     FUNCTION OF R(H'-H) AND THE C-H-H' BEND ANGLE.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,CP1
      COMMON /PDT2CM/ PI,APHI,BPHI,CPHI,PCH4,FCH3,HCH3,RNOT,A3,B3,DELT, 
     *   DIJ,A,B,C0,D,REX
      COMMON /DERVCP/ DECCR1,DECCR5,DECCR6
C
C     COMPUTE C1--IT IS A FUNCTION OF R6--THE H-H' BOND DISTANCE
C
      IF (R(6).GT.15.0D0) THEN
         R6 = R(6)/3.0D0
         YX = (EXP(2.0D0*PI*(R6-REX)/D))**3
      ELSE
         YX = EXP(2.0D0*PI*(R(6)-REX)/D)
      ENDIF
      C1 = -(A*YX/(YX+1.0D0)+B/YX)-C0
      DYXR = (2.0D0*PI/D)*YX
      DCYX = B/YX-(A*YX/(1+YX))/(1+YX)
      DC1R6 = DCYX*DYXR/YX
C
C     COMPUTE THE C-H4-H5 ANGLE
C
      TEM = R(5)**2-R(1)**2-R(6)**2
      TEMBOT = -2.0D0*R(1)*R(6)
      CHECK = TEM/TEMBOT
      IF (CHECK.LE.-1.0D0) THEN
         ALPHA = 0.0D0
         CC(3) = C0
         DECCR1 = 0.0D0
         DECCR5 = 0.0D0
         DECCR6 = 0.0D0
         RETURN
      ELSE
         ALPHA = ACOS(TEM/(-2.0D0*R(1)*R(6)))
         ALPHA = PI-ALPHA
      ENDIF
C
C     NOW COMPUTE THE ANGLE DERIVATIVES (DERIV. ALPHA W.R. R1,R5,R6)
C
      U = TEM/(-2.0D0*R(1)*R(6))
      UI = -1.0D0/(SQRT(1.0D0-U**2))
      DALR5 = -UI*R(5)/(R(1)*R(6))
      TEM2 = 1.0D0/(-2.0D0*R(1)*R(6))
      TEM22 = TEM2*TEM2
      DALR1 = -2.0D0*UI*(R(1)*TEM2-R(6)*TEM*TEM22)
      DALR6 = -2.0D0*UI*(R(6)*TEM2-R(1)*TEM*TEM22)
C
C     COMBINE THE RADIAL AND ANGLE COMPONENT TO COMPUTE THE TRIPLET
C     PARAMETER CC(3) AND THE DERIVATIVES.  HERE TEM IS JUST THE PARTIAL
C     OF CC W.R.T. ALPHA.
C
      DEG20 = PI/9.0D0
      IF (ALPHA.LE.DEG20) THEN
         CC(3) = C0+C1*(SIN(9.0D0*ALPHA/2.0D0))**4
         TEM = 18.0D0*(SIN(9.0D0*ALPHA/2.0D0))**3*COS(9.0D0*ALPHA/    
     *      2.0D0)
         DECCR1 = C1*TEM*DALR1
         DECCR5 = C1*TEM*DALR5
         DECCR6 = DC1R6*(SIN(9.0D0*ALPHA/2.0D0))**4+C1*TEM*DALR6
      ELSE
         CC(3) = C0+C1
         DECCR1 = 0.0D0
         DECCR5 = 0.0D0
         DECCR6 = DC1R6
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE FORCE (DADR1)
C
C     THIS SUBROUTINE COMPUTES THE BENDING FORCE CONSTANT AS A FUNCTION
C     OF R(1). (APARM(1))   IT ALSO COMPUTES THE DERIVATIVE OF THIS
C     TERM WITH RESPECT TO R(1).
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      COMMON /PDT3CM/ FK0,FA,CA,CB
C
      FEX = EXP(-B1*(R(1)-RE(1))**2)
      APARM(1) = FK0+FA*FEX
      DADR1 = -2.0D0*FA*B1*(R(1)-RE(1))*FEX
      RETURN
      END
C
C
      SUBROUTINE CALPHA
C
C     COMPUTE THE C-H AND C-H' MORSE RANGE PARAMETERS FOR THE SINGLET
C     AND TRIPLET ENERGY CURVES.  THESE WILL BE A FUNCTION OF THE
C     AVERAGE C-H BOND LENGTH IN METHANE.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /ICORCM/ R(15),DER(15)
C
      COMMON /PDATCM/ D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3),    
     *   APARM(5),REFV,TAU,CP,B1,C1
      COMMON /PDT3CM/ FK0,FA,CA,CB
      COMMON /ALPDCM/ DALP
C
C     FIRST COMPUTE THE AVERAGE METHANE BOND LENGTH
C
      RAVG = (R(1)+R(2)+R(3)+R(4))/4.0D0
C
      HTAN = TANH(C1*(RAVG-RE(1)))
      ALPH(1) = CA+CB*((HTAN+1.0D0)/2.0D0)
      ALPH(3) = ALPH(1)
      BETA(1) = ALPH(1)
      BETA(3) = ALPH(1)
C
C     NOW THE DERIVATIVES---DALPH WILL BE THE DERIVATIVE OF ALPHA WITH
C     RESPECT TO R1,R2,R3 OR R4
C
      DALP = (C1*CB/8.0D0)*(1.0D0-HTAN**2)
C
      RETURN
      END

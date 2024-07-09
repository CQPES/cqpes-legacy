C
      SUBROUTINE prepot
      use potcm4, only : nder
      use oh3cm; use oh3cm2
C
C   System:    OH + H2
C   Reference: G. C. Schatz and H. Elgersma
C              Chem. Phys. Lett. 73, 21 (1980).
C
C        PREPOT must be called once before any call to POTEN.
C        The potential parameters are included in DATA statements.
C        The coordinates, potential energy, and the derivatives of 
C        of the potential energy with respect to the coordinates are 
C        passed through the common block POTCM:
C                  /POTCM/ R(6), VTOT, DVDR(6).
C        The coordinates, potential energy, and the derivatives of
C        the potential energy with respect to the coordinates for
C        the LEPS part of the potential are passed through the 
C        common block POT2CM:
C                  /POT2CM/ R(4), ENERGY, DEDR(4)
C        All information passed through the common blocks POTCM and
C        POT2CM are in hartree atomic units.
C
C   The the flags that indicate what calculations should be carried out in 
C   the potential routine are passed through the common block POTCCM:
C                  /POTCCM/ NSURF, NDER, NDUM(8)
C   where:
C        NSURF - which electronic state should be used
C                This option is not used in this potential because 
C                only the ground electronic state is available.
C        NDER  = 0 => no derivatives should be calculated
C        NDER  = 1 => calculate first derivatives
C        NDUM  - these 8 integer values can be used to flag options
C                within the potential; in this potential these options 
C                are not used.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c     COMMON /POT2CM/ R(4), ENERGY, DEDR(4)
c     COMMON /POTCCM/ NSURF, NDER, NDUM(8)
c     COMMON /CONCOM/ XDE(3), XBETA(3), XRE(3), SATO, GAM(3), REOH, 
c    *                REHH, CON(7), ALP(4), CLAM(4), ACON(2)
c     DIMENSION DE(3), BETA(3), RE(3), Z(3), ZPO(3), OP3Z(3), ZP3(3), 
c    *          TZP3(3), TOP3Z(3), DO4Z(3), B(3), X(3), COUL(3), EXCH(3) 
C
c     PARAMETER (R2 = 1.41421356D0)
C
C   Initialize the flag for the potential calculation
C
         NDER = 1
C
C   Set up the values of DE, BETA, and RE for the three-body LEPS potential.
C
         DE(1)   = XDE(1)
         BETA(1) = XBETA(1)
         RE(1)   = XRE(1)
         DE(2)   = XDE(1)
         BETA(2) = XBETA(1)
         RE(2)   = XRE(1)
         DE(3)   = XDE(3)
         BETA(3) = XBETA(3)
         RE(3)   = XRE(3)
C
C   Echo the potential energy surface parameters to unit 6.
C
      WRITE (6,1000) XDE, XBETA, XRE, SATO
      WRITE (6,1200) (GAM(I),I=1,3),REOH,REHH
      WRITE (6,1300) CON
      WRITE (6,1400) ALP,CLAM,ACON
C
      DO 10 I = 1, 3
         Z(I) = SATO
C   Compute useful constants.
         ZPO(I)  = 1.0D0+Z(I)
         OP3Z(I)  = 1.0D0+3.0D0*Z(I)
         TOP3Z(I) = 2.0D0*OP3Z(I)
         ZP3(I)   = Z(I)+3.0D0
         TZP3(I)  = 2.0D0*ZP3(I)
         DO4Z(I)  = DE(I)/4.0D0/ZPO(I)
         B(I)     = BETA(I)*DO4Z(I)*2.0D0
   10 CONTINUE
C
1000  FORMAT (/,2X,T5,'OH + H2 potential energy function',
     *        //, 2X, T5, 'Potential energy surface parameters ',
     *                    'in hartree atomic units:',
     *        /,2X,T5,'Morse and LEPS parameters:',
     *        /,2X,T5,'Dissociation energies:', T31,1P,3E13.6,
     *        /,2X,T5,'Morse betas:', T31,1P,3E13.6,
     *        /,2X,T5,'Equilibrium bond lengths:',T31,1P,3E13.6,
     *        /,2X,T5,'Sato parameter:', T31, 1PE13.6)
1200  FORMAT (/,2X,T5,'GAM:',T20,1P,3E13.6,
     *        /,2X,T5,'REOH, REHH:',T20,1P,2E13.6)
1300  FORMAT (/,2X,T5,'CON:',T20,1P,4E13.6,(/,2X,T20,1P,4E13.6))
1400  FORMAT (/,2X,T5,'ALP:',T20,1P,4E13.6,
     *        /,2X,T5,'CLAM:',T20,1P,4E13.6, 
     *        /,2X,T5,'ACON:',T20,1P,2E13.6)
C
      RETURN
      END SUBROUTINE prepot
C
      SUBROUTINE POT
      use potcm4, only : nder
      use oh3cm; use oh3cm2
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   Initialize the variable used for storing the energy.
C
      ENERGY = 0.D0
C
      DO 20 I = 1, 3
         X(I)    = EXP(-BETA(I)*(RX(I)-RE(I)))
         COUL(I) = DO4Z(I)*(ZP3(I)*X(I)-TOP3Z(I))*X(I)
         EXCH(I) = DO4Z(I)*(OP3Z(I)*X(I)-TZP3(I))*X(I)
         ENERGY  = ENERGY + COUL(I)
   20 CONTINUE
      RAD = SQRT((EXCH(1)-EXCH(2))**2+(EXCH(2)-EXCH(3))**2+(EXCH(3)-EXCH
     *   (1))**2)
      ENERGY = ENERGY - RAD/R2 
C
C   Compute the derivatives of the energy with respect to the internal 
C   coordinates.
         IF (NDER .EQ. 1) THEN
             S = EXCH(1) + EXCH(2) + EXCH(3)
             DO 30 I = 1, 3
                   DEDR(I) = B(I)*X(I)*((3.0D0*EXCH(I)-S)/R2*
     *                       (OP3Z(I)*X(I)-ZP3(I))/RAD-
     *                       ZP3(I)*X(I)+OP3Z(I))
30           CONTINUE
         ENDIF
C
      RETURN
      END subroutine pot
C
c     BLOCK DATA
c     use oh3cm
C
C   This is the block data subprogram for the OH + H2 potential energy 
C   function.
C
c     IMPLICIT DOUBLE PRECISION (A-H, O-Z)
c     COMMON /CONCOM/ XDE(3),XBETA(3),XRE(3),SATO,GAM(3),REOH,REHH,
c    *                CON(7),ALP(4),CLAM(4),ACON(2)
C
c     DATA XDE / 0.148201D0, 0.0275690D0, 0.151548D0 /  
c     DATA XBETA / 1.260580D0, 0.924180D0, 1.068620D0 /  
c     DATA XRE / 1.863300D0, 2.907700D0, 1.428600D0 /
c     DATA SATO / 0.10D0 /
c     DATA GAM / 2.399700D0, 1.058350D0, 2.399700D0 /
c     DATA REOH / 1.808090D0 /
c     DATA REHH / 2.861590D0 /
c     DATA CON / -.0015920D0, 0.026963D0, 0.0014689D0, 0.080011D0,
c    *           0.085816D0, -0.063179D0, 0.101380D0 /
c     DATA ALP / 4.773D0, 7.14D0, 2.938D0, 5.28D0 /
c     DATA CLAM / 0.10D0, 0.10D0, 0.20D0, 0.03D0 /
c     DATA ACON / 0.10D0, 0.009D0 /
c
c     END
C
      SUBROUTINE poten
      use potcm4
      use oh3cm
C
C   This subprogram evaluates the OH + H2 potential energy and derivatives
C   of the potential with respect to the internal coordinates.
C   The subprogram PREPEF must be called once before any calls to this 
C   subprogram. 
C   All calculations in this subprogram are in hartree atomic units.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c     COMMON /POTCM/ R(6), VTOT, DVDR(6)
c     COMMON /POT2CM/ RSEND(4), ENERGY, DEDR(4)
c     COMMON /POTCCM/ NSURF, NDER, NDUM(8)
c     COMMON /CONCOM/ DE(3),BETA(3),RE(3),SATO,GAM(3),REOH,REHH,CON(7), 
c    *   ALP(4),CLAM(4),ACON(2)

C
C
C   Zero the array which will contain the derivatives of the energy 
C   with respect to the internal coordinates.
C
         IF (NDER .EQ. 1) THEN
             DO 10 I = 1, 6
                   DVDR(I) = 0.0D0
10           CONTINUE
         ENDIF
C
C   Calculate the Morse part of the potential energy.
      VTOT=VMOR(XDE(1),XBETA(1),XRE(1),R(1))+VMOR(XDE(2),XBETA(2),
     * XRE(2),R(4))+VMOR(XDE(2),XBETA(2),XRE(2),R(5))
C   Calculate the derivatives of the Morse part of the potential energy.
         IF (NDER .EQ. 1) THEN
             DVDR(1) = DVDR(1)+DVMOR(XDE(1),XBETA(1),XRE(1),R(1))
             DVDR(4) = DVDR(4)+DVMOR(XDE(2),XBETA(2),XRE(2),R(4))
             DVDR(5) = DVDR(5)+DVMOR(XDE(2),XBETA(2),XRE(2),R(5))
         ENDIF
C   Initialize the coordinates for the three-body LEPS part of the potential.
      RX(1) = R(2)
      RX(2) = R(3)
      RX(3) = R(6)
C
C   Calculate the three-body LEPS portion of the potential and update the 
C   energy term.
      CALL POT
      VTOT = VTOT+ENERGY
C   Update the array containing the derivatives with the LEPS derivatives.
         IF (NDER .EQ. 1) THEN
             DVDR(2) = DVDR(2)+DEDR(1)
             DVDR(3) = DVDR(3)+DEDR(2)
             DVDR(6) = DVDR(6)+DEDR(3)
         ENDIF
C   Initialize the coordinates for the H2O part of the potential for H1 and H2.
      RX(1) = R(1)
      RX(2) = R(2)
      RX(3) = R(4)
C   Calculate the H2O part of the potential and update the energy term.
      CALL VH2O
      VTOT = VTOT+ENERGY
C   Update the array containing the derivatives with the H2O derivatives.
         IF (NDER .EQ. 1) THEN
             DVDR(1) = DVDR(1)+DEDR(1)
             DVDR(2) = DVDR(2)+DEDR(2)
             DVDR(4) = DVDR(4)+DEDR(3)
         ENDIF
C   Initialize the coordinates for the H2O part of the potential for H1 and H3.
      RX(1) = R(1)
      RX(2) = R(3)
      RX(3) = R(5)
C   Calculate the H2O part of the potential and update the energy term.
      CALL VH2O
      VTOT = VTOT+ENERGY
C   Update the array containing the derivatives with the H2O derivatives.
         IF (NDER .EQ. 1) THEN
             DVDR(1) = DVDR(1)+DEDR(1)
             DVDR(3) = DVDR(3)+DEDR(2)
             DVDR(5) = DVDR(5)+DEDR(3)
         ENDIF
C   Initialize the coordinates for the four-body part of the potential.
      RX(1) = R(2)
      RX(2) = R(3)
      RX(3) = R(4)
      RX(4) = R(5)
C   Calculate the four-body part of the potential and update the energy term.
      CALL V4POT
      VTOT = VTOT+ENERGY
C   Update the array containing the derivatives.
         IF (NDER .EQ. 1) THEN
             DVDR(2) = DVDR(2)+DEDR(1)
             DVDR(3) = DVDR(3)+DEDR(2)
             DVDR(4) = DVDR(4)+DEDR(3)
             DVDR(5) = DVDR(5)+DEDR(4)
         ENDIF
C
C   Adjust the potential to the correct zero, which corresponds to
C   OH(R=RE) and H2(R=RE) at infinity.
C
      VTOT = VTOT-2.0D0*XDE(2)+XDE(3)
C
      RETURN
C   Define the statement functions VMOR and DVMOR which evaluate the Morse
C   diatomic potential energy  and the derivatives of the Morse potential.
C    
      CONTAINS
      DOUBLE PRECISION FUNCTION VMOR(D,B,T,RR)
      IMPLICIT NONE
      REAL(8) :: D, B, T, RR
      VMOR = D*(1.0D0-EXP(-B*(RR-T)))**2
      RETURN
      END FUNCTION VMOR
      DOUBLE PRECISION FUNCTION DVMOR(D,B,T,RR)
      IMPLICIT NONE
      REAL(8) :: D, B, T, RR
      DVMOR = 2.0D0*B*D*(1.0D0-EXP(-B*(RR-T)))*EXP(-B*(RR-T))
      RETURN
      END FUNCTION DVMOR
      END subroutine poten
C*****
      SUBROUTINE V4POT
      use potcm4,only : nder
      use oh3cm, only : rx,energy,dedr,alp,clam,acon
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /CONCOM/ DUM(22),A(4),C(4),COF(2)
C     COMMON /POT2CM/ R(4),E,DEDR(4)
C     COMMON /POTCCM/ NSURF, NDER, NDUM(8)
      dimension :: R(4),A(4),C(4),COF(2)
      R(:) = RX(:)
      A(:) = ALP(:)
      C(:) = CLAM(:)
      COF(:) = ACON(:)
C
      T1 = EXP(-C(1)*(R(1)-A(1))**2-C(1)*(R(2)-A(1))**2-C(3)*(R(3)-A(3))
     *   **2-C(3)*(R(4)-A(3))**2)*COF(1)
      T2 = EXP(-C(2)*(R(1)-A(2))**2-C(2)*(R(2)-A(2))**2-C(4)*(R(3)-A(4))
     *   **2-C(4)*(R(4)-A(4))**2)*COF(2)
      ENERGY = T1+T2
         IF (NDER .EQ. 1) THEN
             DEDR(1) = -2.0D0*(T1*C(1)*(R(1)-A(1))+T2*C(2)*(R(1)-A(2)))
             DEDR(2) = -2.0D0*(T1*C(1)*(R(2)-A(1))+T2*C(2)*(R(2)-A(2)))
             DEDR(3) = -2.0D0*(T1*C(3)*(R(3)-A(3))+T2*C(4)*(R(3)-A(4)))
             DEDR(4) = -2.0D0*(T1*C(3)*(R(4)-A(3))+T2*C(4)*(R(4)-A(4)))
         ENDIF
C
      RETURN
      END subroutine v4pot
C*****
C
      SUBROUTINE VH2O
      use potcm4,only : nder, ndum
      use oh3cm, only : RX,ENERGY,DEDR,GAM,REOH,REHH,CON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION S(3),Q(3),DQ(3),X(3),DP(3),C(7)
C     COMMON /CONCOM/ DUM1(10),GAM(3),REOH,REHH,C(7),DUM2(10)
c     COMMON /CONCOM/ XDE(3), XBETA(3), XRE(3), SATO, GAM(3), REOH, 
c    *                REHH, CON(7), ALP(4), CLAM(4), ACON(2)
c     COMMON /POT2CM/ R(4),E,DEDR(4)
c     COMMON /POTCCM/ NSURF, NDER, NDUM(8)
C
C     XMAX1 FOR WHEN TANH SET=1.0 ON VAX
C     XMAX2 FOR PREVENTING OVERFLOWS ON VAX
C
      DATA XMAX1,XMAX2 / 15.0D0,43.0D0 /
      C(:) = CON(:)
C
C     STATEMENT FUNCTION
C
      S(1) = RX(1)-REOH
      S(3) = RX(2)-REOH
      S(2) = RX(3)-REHH
      DO 30 I = 1, 3
         X(I) = 0.5D0*GAM(I)*S(I)
         Q(I) = 1.0D0-TANH(X(I))
         IF (X(I).LT.XMAX1) GO TO 20
         IF (X(I).LT.XMAX2) GO TO 10
         Q(I) = 0.0D0
         DQ(I) = 0.0D0
         GO TO 30
   10    Q(I) = TANLG(X(I))
   20    DQ(I) = -0.5D0*GAM(I)/COSH(X(I))**2
   30 CONTINUE
      P = C(1)+C(2)*(S(1)+S(3))+C(3)*S(2)+0.5D0*C(4)*(S(1)*S(1)+S(3)*S(
     *    3))+0.5D0*C(5)*S(2)*S(2)+C(6)*S(2)*(S(1)+S(3))+C(7)*S(1)*S(3)
      ENERGY = Q(1)*Q(2)*Q(3)*P
      IF (NDER .EQ. 1) THEN 
          DP(1) = C(2)+C(4)*S(1)+C(6)*S(2)+C(7)*S(3)
          DP(2) = C(3)+C(5)*S(2)+C(6)*(S(1)+S(3))
          DP(3) = C(2)+C(4)*S(3)+C(6)*S(2)+C(7)*S(1)
          DO 40 I = 1, 3
                TRM1 = 0.0D0
                IF (Q(I).EQ.0.0D0) GO TO 40
                TRM1 = DQ(I)/Q(I)
   40           DEDR(I) = ENERGY*(TRM1+(DP(I)/P))
          TEMP = DEDR(2)
          DEDR(2) = DEDR(3)
          DEDR(3) = TEMP
      ENDIF
C
      RETURN
      CONTAINS
       DOUBLE PRECISION FUNCTION TANLG(XX) 
       IMPLICIT NONE
       REAL(8), INTENT(IN) :: XX
        TANLG = 2.0D0/(1.0D0+DEXP(2.0D0*XX))
      RETURN
      END FUNCTION TANLG
      END subroutine VH2O
C*****

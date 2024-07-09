        SUBROUTINE SETUP(N3TM)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
        COMMON/VBINCM/A1(171),A2(171),A3(171),A4(171),A5(171),ALF(171),
     2       BET(171),X1EQ(171),X2EQ(171),FI(171),FIJ(171),AR2,TAR2,BR2
     2       ,ALR2,BTR2,ATET,BTET,CTET,RH,RHC,RHS                       11FEB89ST
     2       ,A6(171),A7(171),RCT,BTP                                   16FEB89
C       DIMENSION FC(18,18,5)
C       DIMENSION AL(7),BT(7)
C    The above lines were commented because the variables are not used. 0423TA02
C
C   N3TMMN = 3 * NATMAX
C   NATMAX = the number of atoms represented by this potential function
C
C   The variable N3TMMN is the miNImum value of N3TM allowed to be 
C   passed by the calling routine fOR the number of cartesian 
C   coordinates needed to represent the full system represented by this 
C   potential energy surface routine.
C   N3TM must be greater than OR equal to N3TMMN.
C
      PARAMETER (N3TMMN = 18)
C
C  CHECK THE NUMBER OF CARTESIAN COORDINATES SET BY THE CALLING PROGRAM
C
      IF (N3TM .LT. N3TMMN) THEN
          WRITE (6, 1000) N3TM, N3TMMN
          STOP 'SETUP 1'
      ENDIF
C
C  OPEN THE FILES WHICH CONTAIN THE POTENTIAL DATA
C
       OPEN (UNIT=2, FILE='potcmc2.dat', STATUS='OLD', 
     *       FORM='FORMATTED', ERR=100)
C
       OPEN (UNIT=4, FILE='potcmc1.dat', STATUS='OLD', 
     *       FORM='FORMATTED', ERR=100)
C
        WRITE (6, 1100)
        CALL PRELLR
        CALL PREPOT
C
C  CLOSE THE POTENTIAL DATA FILES
C
       CLOSE (UNIT=2)
       CLOSE (UNIT=4) 
C
1000     FORMAT(/,2X,T5,'WARNING: N3TM is set equal to ',I3,
     *                  ' but this potential routine',
     *          /,2X,T14,'requires N3TM be greater than or ',
     *                   'equal to ',I3,/)
1100    FORMAT(/,2X,T5,'Setup has been called for the ClCH3Cl ',
     *                 'surface S')
C
        RETURN
C
  100 WRITE(6,*)'ERROR OPENING POTENTIAL DATA FILE'
      STOP 'SETUP 2'
C
      END
C
C     PREPOT FOR LEPSLR
      SUBROUTINE PRELLR
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
      R2 = SQRT(2.0D0)
C    
C   READ POTENTIAL ENERGY SURFACE PARAMETERS
C        ENERGIES IN KCAL/MOL, LENGTHS IN ANGSTOMS
C        DELZ,ZSLP UNITLESS, RM IN ANGSTROM
      READ (4,501) (D(I),RE(I),BETA(I),Z(I),I = 1,3)
      READ (4,501)  DELZ,ZSLP,RM                                        13OCT88
  501 FORMAT (4F20.5)
C   READ IN LONG RANGE TERM PARAMETERS                                  04DEC87
C        AQ1 IN INVERSE ANGSTROM, AQ4 IN ANGSTROM                       25AUG88
C        CO1 IN INVERSE ANGSTROM, RECO IN ANGSTROM                      25AUG88
C        ALL ELSE UNITLESS                                              04DEC87
C   NOTE:THERE IS NO AALP1, DUE TO A CHANGE IN FNAL FORM ON 8/1/88      01AUG88
      READ (4,501) AQ1,AQ2,AQ3,AQ4                                      04DEC87
      READ (4,501) AALP2,AALP3,AALP4,AALP5                              01AUG88
      READ (4,501) CO1,RECO,AQ5                                         26DEC88
C
      EASYM = 0.55149589D0                                              08MAR89hS
      WRITE (6,602) D,RE,BETA,Z
      WRITE (6,604) DELZ,ZSLP,RM                                        13OCT88
      WRITE (6,603) AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2   CO1,RECO,EASYM                                                 08MAR89
  602 FORMAT (/,2X,T5,'Potential energy surface parameters for VLEPS',
     *        /,2X,T5, 'Bond', T47, 'ClMe', T58, 'MeCl', T69, 'ClCl',
     *        /, 2X, T5, 'Dissociation energies (kcal/mol):', 
     *        T44, F10.5, T55, F10.5, T66, F10.5,
     *        /, 2X, T5, 'Equilibrium bond lengths (Angstroms):', 
     *        T44, F10.5, T55, F10.5, T66, F10.5,
     *        /, 2X, T5, 'Morse beta parameters (Angstroms**-1):', 
     *        T44, F10.5, T55, F10.5, T66, F10.5,
     *        /, 2X, T5, 'Sato parameters:', 
     *        T44, F10.5, T55, F10.5, T66, F10.5)
C
  603 FORMAT(/,2X,T5,'Parameters for the long range term',
     *       /,2X,T5,'Charge fit coeff. (1-5)',T44,3(F10.5,1X),
     *       /,2X,T44,2(F10.5,1X),
     *       /,2X,T5,'Polarizability fit coeff. (1-4)',
     *            T44,3(F10.5,1X),/,2X,T44,F10.5,
     *       /,2X,T5,'Cut off coeff. (1,2)',T44,2(F10.5,1X),
     *       /,2X,T5,'Reactant energy',T44,F13.8)
  604 FORMAT (/,2X,T5,'Sato switching',T44,3(F10.5,1X))
      DO  10 I = 1,3
C   CONVERT TO ATOMIC UNITS
      D(I)=D(I)/627.5095D0
      RE(I) = RE(I)/0.52917706D0
      BETA(I) = BETA(I)*0.52917706D0
10    CONTINUE                                                          13OCT88
      RM = RM/0.52917706D0                                              13OCT88
      ZSLP = ZSLP*0.52917706D0                                          13OCT88 
C   COMPUTE USEFUL CONSTANTS                                            13OCT88
      DZDR(3) = 0.D0
      ZPO(3) = 1.0D0 + Z(3)                                             13OCT88
      OP3Z(3) = 1.0D0 + 3.0D0*Z(3)                                      13OCT88
      TOP3Z(3) = 2.0D0*OP3Z(3)                                          13OCT88
      ZP3(3) = Z(3) + 3.0D0                                             13OCT88
      TZP3(3) = 2.0D0*ZP3(3)                                            13OCT88
      DO4Z(3) = D(3)/4.0D0/ZPO(3)                                       13OCT88
      B(3) = BETA(3)*DO4Z(3)*2.0D0                                      13OCT88
C   CONVERT LONG RANGE PARAMETERS TO ATOMIC UNITS ALSO                  04DEC87
      CONV2 = (0.52917706D0)**2                                         15OCT88
      CONV3 = (0.52917706D0)*CONV2                                      15OCT88
      AQ1 = AQ1*0.52917706D0                                            04DEC87
      AQ4 = AQ4/0.52917706D0                                            22AUG88
      AQ5 = AQ5*CONV2                                                   15OCT88
      AALP2 = AALP2/CONV3                                               04DEC87
      AALP3 = AALP3/CONV3                                               04DEC87
      AALP4 = AALP4/CONV3                                               04DEC87
      AALP5 = AALP5/CONV3                                               04DEC87
      CO1 = CO1*0.52917706D0                                            26DEC88 
      RECO = RECO/0.52917706D0                                          26DEC88 
      EASYM = EASYM/627.5095D0                                          18OCT88
      RETURN
      END
        SUBROUTINE PREPOT
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
        COMMON/VBINCM/A1(171),A2(171),A3(171),A4(171),A5(171),ALF(171),
     2       BET(171),X1EQ(171),X2EQ(171),FI(171),FIJ(171),AR2,TAR2,BR2
     2       ,ALR2,BTR2,ATET,BTET,CTET,RH,RHC,RHS                       11FEB89ST
     2       ,A6(171),A7(171),RCT,BTP                                   16FEB89
        DIMENSION FC(18,18,5)
        DIMENSION AL(7),BT(7)
C       READ IN OTHER CONSTANTS, AND CONVERT TO ATOMIC UNITS
        READ(2,462) AR2,BR2,ALR2,BTR2                                   11FEB89ST
        READ(2,462) ATET,BTET,CTET
        READ(2,462) RH
462     FORMAT(4F20.5)
        WRITE(6,463) AR2,BR2,ALR2,BTR2,ATET,BTET,CTET,RH
463     FORMAT(/,1X,'Parameters for the equilibrium cartesian coords ',
     2   'as a fcn of RC',/1X,'for R2',14X,4F10.5/1X,'for THETA',11X,
     2    3F10.5/1X,'CH distance',9X,1F10.5/)
C       CONVERT TO ATOMIC UNITS AND RADIANS
        BRANG2 = 0.52917706D0*0.52917706D0
        ANGBOR = 1.D0/0.52917706D0
        PI = ACOS(-1.0D0)
        RADCN = PI/180.D0
        AR2 = AR2*ANGBOR
        BR2 = BR2*ANGBOR
        ALR2 = ALR2*ANGBOR
        BTR2 = BTR2*BRANG2
        ATET = ATET*RADCN
        BTET = BTET*0.52917706D0
        CTET = CTET*RADCN
        RH = RH*ANGBOR
C       COMPUTE USEFUL CONSTANTS
        TAR2 = 2.0D0 * AR2
        RHC = RH*0.5D0
        RHS = RH*SQRT(3.D0)*0.5D0
C       READ IN CARTESIAN FORCE CONTSTANTS FROM ABINITIO CALCULATIONS
        DO 10 I=1,18
         IF(I.LE.5)THEN
           MAXRD = I
         ELSE                
           MAXRD = 5
         END IF
         DO 210 K=1,5
           READ(2,500) NDUM,(FC(I,J,K),J=1,MAXRD)
210      CONTINUE
 10     CONTINUE
500     FORMAT(I3,5F14.6)
        DO 15 I=6,18
         IF(I.LE.10)THEN
           MAXRD = I
         ELSE
           MAXRD = 10
         END IF
         DO 215 K=1,5
           READ(2,500) NDUM,(FC(I,J,K),J=6,MAXRD)
215      CONTINUE
 15     CONTINUE
        DO 20 I=11,18
         IF(I.LE.15)THEN
           MAXRD = I
         ELSE
           MAXRD = 15
         END IF
         DO 220 K=1,5
           READ(2,500) NDUM,(FC(I,J,K),J=11,MAXRD)
220      CONTINUE
 20     CONTINUE
        DO 25 I=16,18
         DO 225 K=1,5
           READ(2,500) NDUM,(FC(I,J,K),J=16,I)    
225      CONTINUE
 25     CONTINUE
C       COMPUTE FITS TO FORCE CONSTANTS, IN EH/A0**2
        BTP = 0.25D0
        RCT =  2.54273315D0
        RCF =  -RCT                                                     16FEB89
        RCT2 = RCT*RCT
        RCE4 = EXP(-4.D0*RCT2)                                          16FEB89
        RCE8 = RCE4*RCE4                                                16FEB89
        DUM = 1.0D0
        AL(1) = 0.403D0
        AL(3) = 0.234D0
        AL(6) = 0.88D0
        AL(7) = 0.40D0
        BT(3) = 0.58D0
        BT(6) = 0.7D0
        BT(7) = 0.15D0
        GAM6 = 1.18D0
        GAM7 = 2.20D0
        X16 = -2.14D0
        X17 = -2.40D0 
        DO 120 I=1,18
         DO 140 J=1,I
           NIJ = ((I*I - I)/2  + 1 + (J-1) ) 
           DA5A1 = ABS(FC(I,J,5)) - ABS(FC(I,J,1))
           D15 = FC(I,J,1) - FC(I,J,5)
           D13 = FC(I,J,1) - FC(I,J,3)
           AD15 = ABS(D15)
           AD13 = ABS(D13)
           AD23 = ABS(FC(I,J,2) - FC(I,J,3))
           AD25 = ABS(FC(I,J,2) - FC(I,J,5))
           AD41 = ABS(FC(I,J,4) - FC(I,J,1))
          IF(D13.NE.0.0D0)THEN
           IF(DA5A1.EQ.0.D0)THEN
             FI(NIJ) = FC(I,J,3)
             FIJ(NIJ) = D13
             IF(AD15.EQ.0.0D0)THEN                                      14FEB89
               IF(AD13.LT.AD23)THEN
                 A1(NIJ) = 1.0D0
                 A2(NIJ) = -1.0D0
                 A3(NIJ) = 0.0D0
                 A5(NIJ) = 0.0D0
                 A6(NIJ) = 0.0D0                                        16FEB89
                 A7(NIJ) = 0.0D0                                        16FEB89
                 X1EQ(NIJ) = 0.0D0
                 X2EQ(NIJ) = DUM
                 ALF(NIJ) = AL(1)
                 BET(NIJ) = DUM
                 XINV = 1.0D0/RCT2
                 D21 = FC(I,J,2) - FC(I,J,1)
                 GT = 1.D0 + EXP( AL(1)*RCT2 ) * (D21/D13)
                 A4(NIJ) = XINV * GT
               ELSE
                 A1(NIJ) = 1.0D0
                 A2(NIJ) = -1.0D0
                 A3(NIJ) = 0.0D0
                 A4(NIJ) = 0.0D0
                 A5(NIJ) = 0.0D0
                 A6(NIJ) = 0.0D0                                        16FEB89
                 A7(NIJ) = 0.0D0                                        16FEB89
                 X1EQ(NIJ) = 0.0D0
                 X2EQ(NIJ) = DUM
                 D12 = FC(I,J,1) - FC(I,J,2)                            14FEB89
                 RAT = (D12/D13)                                        14FEB89
                 ALF(NIJ) = -(1.0D0/RCT2)*LOG(RAT)                      14FEB89
                 BET(NIJ) = DUM  
               END IF
             ELSE
               IF(AD13.LT.AD23)THEN
                 A1(NIJ) = 0.0D0
                 A2(NIJ) = 0.0D0
                 A4(NIJ) = 0.0D0
                 A5(NIJ) = 1.0D0
                 A6(NIJ) = 0.0D0                                        16FEB89
                 A7(NIJ) = 0.0D0                                        16FEB89
                 X1EQ(NIJ) = 0.0D0
                 X2EQ(NIJ) = 0.0D0
                 ALF(NIJ) = AL(3)
                 BET(NIJ) = BT(3)
                 TH2 = TANH( BT(3) * RCT )
                 D23 = FC(I,J,2) - FC(I,J,3)
                 RAT = EXP( AL(3)*RCT2 ) / RCT
                 A3(NIJ) = RAT * ( (D23/D13) - TH2 )
               ELSE
                 A1(NIJ) = 0.0D0
                 A2(NIJ) = 0.0D0
                 A3(NIJ) = 0.0D0
                 A4(NIJ) = 0.0D0
                 A5(NIJ) = 1.0D0
                 A6(NIJ) = 0.0D0                                        16FEB89
                 A7(NIJ) = 0.0D0                                        16FEB89
                 X1EQ(NIJ) = DUM
                 X2EQ(NIJ) = 0.0D0
                 ALF(NIJ) = DUM
                 D23 = FC(I,J,2) - FC(I,J,3)                
                 RAT = D23/D13
                 ATH = 0.5D0*LOG((1.D0 + RAT)/(1.D0 - RAT))
                 BET(NIJ) = (1.D0/RCT) * ATH
               END IF      
             END IF
           ELSE
             FI(NIJ) = FC(I,J,5)
             FIJ(NIJ) = D15
             RA2515 = AD25/AD15
             RA4151 = AD41/AD15
             IF(RA2515.GT.1.05D0)THEN
               IF(FC(I,J,1).EQ.0.0D0) THEN
                 A1(NIJ) = 0.50D0
                 A3(NIJ) = 0.0D0
                 A4(NIJ) = 0.0D0
                 A5(NIJ) = 0.50D0
                 ALF(NIJ) = AL(6) 
                 BET(NIJ) = BT(6) 
                 X1EQ(NIJ) = - X16
                 D21 = FC(I,J,2) - FC(I,J,1)
                 A2(NIJ) = GAM6 * (D21/D15)
                 CA2 = A2(NIJ) * EXP(-AL(6) * X16 * X16 )
                 D35 = FC(I,J,3) - FC(I,J,5)
                 RAT = D35 / D15
                 ARGLN = (RAT - CA2)/(1.0D0 - RAT + CA2)
                 X2EQ(NIJ) = - LOG(ARGLN) / (2.0D0 * BT(6))
                 GTIL4 = .5D0 + .5D0*TANH(BET(NIJ)*(RCF-X2EQ(NIJ))) 
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCF-X1EQ(NIJ))**2)      16FEB89
                 GTIL2 = .5D0 + .5D0*TANH(BET(NIJ)*(RCT-X2EQ(NIJ)))
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCT-X1EQ(NIJ))**2)      16FEB89
                 D25 = FC(I,J,2) - FC(I,J,5)                             16FEB89
                 D45 = FC(I,J,4) - FC(I,J,5)                             16FEB89
                 G2 = D25/D15                                            16FEB89
                 G4 = D45/D15                                            16FEB89
                 DG2 = G2 - GTIL2                                        16FEB89
                 DG4 = G4 - GTIL4                                        16FEB89
                 A6(NIJ) = (DG2 + DG4*RCE4)/(RCF*(RCE8 - 1.D0) )         16FEB89
                 A7(NIJ) = (DG4 + DG2*RCE4)/(RCT*(RCE8 - 1.D0) )         16FEB89
               ELSE
                 A1(NIJ) = 0.50D0
                 A3(NIJ) = 0.0D0
                 A4(NIJ) = 0.0D0
                 A5(NIJ) = 0.50D0
                 ALF(NIJ) = AL(7) 
                 BET(NIJ) = BT(7) 
                 X1EQ(NIJ) = - X17
                 D21 = FC(I,J,2) - FC(I,J,1)
                 A2(NIJ) = GAM7 * (D21/D15)
                 CA2 = A2(NIJ) * EXP(-AL(7) * X17 * X17 )
                 D35 = FC(I,J,3) - FC(I,J,5)
                 RAT = D35 / D15
                 ARGLN = (RAT - CA2)/(1.0D0 - RAT + CA2)
                 X2EQ(NIJ) = - LOG(ARGLN) / (2.0D0 * BT(7))
                 GTIL4 = .5D0 + .5D0*TANH(BET(NIJ)*(RCF-X2EQ(NIJ)))     
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCF-X1EQ(NIJ))**2)      16FEB89
                 GTIL2 = .5D0 + .5D0*TANH(BET(NIJ)*(RCT-X2EQ(NIJ)))
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCT-X1EQ(NIJ))**2)      16FEB89
                 D25 = FC(I,J,2) - FC(I,J,5)                             16FEB89
                 D45 = FC(I,J,4) - FC(I,J,5)                             16FEB89
                 G2 = D25/D15                                            16FEB89
                 G4 = D45/D15                                            16FEB89
                 DG2 = G2 - GTIL2                                        16FEB89
                 DG4 = G4 - GTIL4                                        16FEB89
                 A6(NIJ) = (DG2 + DG4*RCE4)/(RCF*(RCE8 - 1.D0) )         16FEB89
                 A7(NIJ) = (DG4 + DG2*RCE4)/(RCT*(RCE8 - 1.D0) )         16FEB89
               END IF
             ELSE
               IF(RA4151.GT.1.05D0)THEN
                 IF(FC(I,J,5).EQ.0.0D0)THEN
                   A1(NIJ) = 0.50D0
                   A3(NIJ) = 0.0D0
                   A4(NIJ) = 0.0D0
                   A5(NIJ) = 0.50D0
                   ALF(NIJ) = AL(6)
                   BET(NIJ) = BT(6)
                   X1EQ(NIJ) = X16
                   D45 = FC(I,J,4) - FC(I,J,5)
                   A2(NIJ) = GAM6 * (D45/D15)
                   CA2 = A2(NIJ) * EXP(-AL(6) * X16 * X16 )
                   D35 = FC(I,J,3) - FC(I,J,5)
                   RAT = D35 / D15
                   ARGLN = (RAT - CA2)/(1.0D0 - RAT + CA2)
                   X2EQ(NIJ) = - LOG(ARGLN) / (2.0D0 * BT(6))
                   GTIL4 = .5D0 + .5D0*TANH(BET(NIJ)*(RCF-X2EQ(NIJ)))
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCF-X1EQ(NIJ))**2)      16FEB89
                   GTIL2 = .5D0 + .5D0*TANH(BET(NIJ)*(RCT-X2EQ(NIJ)))
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCT-X1EQ(NIJ))**2)      16FEB89
                   D25 = FC(I,J,2) - FC(I,J,5)                           16FEB89
                   D45 = FC(I,J,4) - FC(I,J,5)                           16FEB89
                   G2 = D25/D15                                          16FEB89
                   G4 = D45/D15                                          16FEB89
                   DG2 = G2 - GTIL2                                      16FEB89
                   DG4 = G4 - GTIL4                                      16FEB89
                   A6(NIJ) = (DG2 + DG4*RCE4)/(RCF*(RCE8 - 1.D0) )       16FEB89
                   A7(NIJ) = (DG4 + DG2*RCE4)/(RCT*(RCE8 - 1.D0) )       16FEB89
                 ELSE
                   A1(NIJ) = 0.50D0
                   A3(NIJ) = 0.0D0
                   A4(NIJ) = 0.0D0
                   A5(NIJ) = 0.50D0
                   ALF(NIJ) = AL(7)
                   BET(NIJ) = BT(7)
                   X1EQ(NIJ) = X17
                   D45 = FC(I,J,4) - FC(I,J,5)
                   A2(NIJ) = GAM7 * (D45/D15)
                   CA2 = A2(NIJ) * EXP(-AL(7) * X17 * X17 )
                   D35 = FC(I,J,3) - FC(I,J,5)
                   RAT = D35 / D15
                   ARGLN = (RAT - CA2)/(1.0D0 - RAT + CA2)
                   X2EQ(NIJ) = - LOG(ARGLN) / (2.0D0 * BT(7))
                   GTIL4 = .5D0 + .5D0*TANH(BET(NIJ)*(RCF-X2EQ(NIJ)))
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCF-X1EQ(NIJ))**2)      16FEB89
                   GTIL2 = .5D0 + .5D0*TANH(BET(NIJ)*(RCT-X2EQ(NIJ)))
     2                  + A2(NIJ)*EXP(-ALF(NIJ)*(RCT-X1EQ(NIJ))**2)      16FEB89
                   D25 = FC(I,J,2) - FC(I,J,5)                           16FEB89
                   D45 = FC(I,J,4) - FC(I,J,5)                           16FEB89
                   G2 = D25/D15                                          16FEB89
                   G4 = D45/D15                                          16FEB89
                   DG2 = G2 - GTIL2                                      16FEB89
                   DG4 = G4 - GTIL4                                      16FEB89
                   A6(NIJ) = (DG2 + DG4*RCE4)/(RCF*(RCE8 - 1.D0) )       16FEB89
                   A7(NIJ) = (DG4 + DG2*RCE4)/(RCT*(RCE8 - 1.D0) )       16FEB89
                 END IF
               ELSE
                 A1(NIJ) = 0.50D0
                 A3(NIJ) = 0.0D0
                 A4(NIJ) = 0.0D0
                 A5(NIJ) = 0.50D0
                 A6(NIJ) = 0.0D0                                         16FEB89
                 A7(NIJ) = 0.0D0                                         16FEB89
                 X1EQ(NIJ) = 0.0D0
                 X2EQ(NIJ) = 0.0D0
                 D35 = FC(I,J,3) - FC(I,J,5)
                 D25 = FC(I,J,2) - FC(I,J,5)
                 D42 = FC(I,J,4) - FC(I,J,2)
                 A2(NIJ) = (D35/D15) - 0.5D0
                 RAT = (D15 + D42)/(D15 - D42)
                 BET(NIJ) = -(0.5D0/RCT)*LOG(RAT)
                 TPRT = 0.5D0 + 0.5D0*TANH(BET(NIJ)*RCT)
                 ARG = ( (D25/D15) - TPRT )/A2(NIJ)
                 IF (ARG.LE.0.0D0) THEN
C                 this applies only to (4,1),(5,2),(16,1) and (17,2)
C                 which are related by symmetry. Later try to fit with
C                 one of the type 6 forms?
                  ALF(NIJ) = 1.0D0
                 ELSE
                 ALF(NIJ) = - (1.D0/RCT2)*LOG(ARG)
                 END IF
               END IF
             END IF
           END IF                  
          ELSE                                                          14FEB89
            A1(NIJ) = 0.0D0
            A2(NIJ) = 0.0D0
            A3(NIJ) = 0.0D0
            A4(NIJ) = 0.0D0
            A5(NIJ) = 0.0D0
            A6(NIJ) = 0.0D0                                              16FEB89
            A7(NIJ) = 0.0D0                                              16FEB89
            X1EQ(NIJ) = DUM
            X2EQ(NIJ) = DUM
            ALF(NIJ) = DUM
            BET(NIJ) = DUM
            FI(NIJ) = FC(I,J,1)
            FIJ(NIJ) = 0.0D0
          END IF
          IF(NIJ.EQ.7.OR.NIJ.EQ.12.OR.NIJ.EQ.121.OR.NIJ.EQ.138)THEN      16FEB89
            POLY = A2(NIJ) 
            GAUS2 = EXP(-ALF(NIJ)*(RCT-X1EQ(NIJ))**2)
            GAUS4 = EXP(-ALF(NIJ)*(RCF-X1EQ(NIJ))**2)
            ATH2 = BET(NIJ)*(RCT-X2EQ(NIJ))
            ATH4 = BET(NIJ)*(RCF-X2EQ(NIJ))
            TH2 = A5(NIJ)*TANH(ATH2)
            TH4 = A5(NIJ)*TANH(ATH4)
            GTIL2 = A1(NIJ) + GAUS2*POLY + TH2 
            GTIL4 = A1(NIJ) + GAUS4*POLY + TH4 
            D15 = FC(I,J,1) - FC(I,J,5)
            D25 = FC(I,J,2) - FC(I,J,5)
            D45 = FC(I,J,4) - FC(I,J,5)
            G2 = D25/D15
            G4 = D45/D15
            DG2 = G2 - GTIL2 
            DG4 = G4 - GTIL4 
            A6(NIJ) = (DG2 + DG4*RCE4)/(RCF*(RCE8 - 1.D0) )
            A7(NIJ) = (DG4 + DG2*RCE4)/(RCT*(RCE8 - 1.D0) )
          END IF
          IF(NIJ.EQ.36.OR.NIJ.EQ.64.OR.NIJ.EQ.100)THEN                  16FEB89
            ALF(36) = 1.0
            BET(64) = ABS(BET(64))
            BET(100) = ABS(BET(100))
            POLY = A2(NIJ) 
            GAUS2 = EXP(-ALF(NIJ)*(RCT-X1EQ(NIJ))**2)
            GAUS4 = EXP(-ALF(NIJ)*(RCF-X1EQ(NIJ))**2)
            ATH2 = BET(NIJ)*(RCT-X2EQ(NIJ))
            ATH4 = BET(NIJ)*(RCF-X2EQ(NIJ))
            TH2 = A5(NIJ)*TANH(ATH2)
            TH4 = A5(NIJ)*TANH(ATH4)
            GTIL2 = A1(NIJ) + GAUS2*POLY + TH2 
            GTIL4 = A1(NIJ) + GAUS4*POLY + TH4 
            D13 = FC(I,J,1) - FC(I,J,3)
            D23 = FC(I,J,2) - FC(I,J,3)
            D43 = FC(I,J,4) - FC(I,J,3)
            G2 = D23/D13
            G4 = D43/D13
            DG2 = G2 - GTIL2 
            DG4 = G4 - GTIL4 
            A6(NIJ) = (DG2 + DG4*RCE4)/(RCF*(RCE8 - 1.D0) )
            A7(NIJ) = (DG4 + DG2*RCE4)/(RCT*(RCE8 - 1.D0) )
          END IF
140      CONTINUE
120     CONTINUE
        RETURN
        END
C
C   ClCH3Cl (gas-phase) potential energy surface S of Tucker et al.
C
        SUBROUTINE SURF(V, X, DX, N3TM)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
        COMMON/LLRCM/R(3),ELLR,DEDR(3),RC
        COMMON/VBINCM/A1(171),A2(171),A3(171),A4(171),A5(171),ALF(171),
     2       BET(171),X1EQ(171),X2EQ(171),FI(171),FIJ(171),AR2,TAR2,BR2
     2       ,ALR2,BTR2,ATET,BTET,CTET,RH,RHC,RHS                       11FEB89ST
     2       ,A6(171),A7(171),RCT,BTP                                   16FEB89
        DIMENSION X(N3TM), DX(N3TM)
        DIMENSION FFIT(18,18)
        DIMENSION DR1DX(18),DR2DX(18),DR3DX(18),XND(18),XN(18),X0(18)
        DIMENSION DU3BDX(18),DUVBDX(18),DUVBDY(18)
        DIMENSION DRCDX(18),DX0DRC(18),DFDRC(18,18),DKDX(18)
C       FIND NEW X,Y,Z COORDINATES, XN
        DO 387 IX=1,16,3
          IY = IX + 1
          IZ = IX + 2
          XN(IX) = X(IX) - X(1)
          XN(IY) = X(IY) - X(2)
          XN(IZ) = X(IZ) - X(3)
387     CONTINUE
C       FIND R1,R2,R3
        R1S = (X(1)-X(4))**2 + (X(2)-X(5))**2 + (X(3)-X(6))**2
        R2S = (X(1)-X(16))**2 + (X(2)-X(17))**2 + (X(3)-X(18))**2
        R3S = (X(4)-X(16))**2 + (X(5)-X(17))**2 + (X(6)-X(18))**2
        R(1) = SQRT(R1S)
        R(2) = SQRT(R2S)
        R(3) = SQRT(R3S)
C       FIND RC,RC2, AND THE 3 BODY ENREGY AND DERIVATIVES
        CALL POTLLR
        RC2 = RC*RC
C       EVALUATE THE NIJ CARTESIAN FORCE CONSTANTS AT RC
C       ALSO EVALUATE THE NIJ DERIVATIVES OF THESE FORCE CONTSTS. W.R.T. RC
        DO 200 I=1,18
          DO 250 J=1,I
            NIJ = ((I*I - I)/2  + 1 + (J-1) ) 
            POLY = A2(NIJ) + A3(NIJ)*RC + A4(NIJ)*RC2
            GAUS = EXP(-ALF(NIJ)*(RC-X1EQ(NIJ))**2)
            GAUS2 = EXP(-(RC-RCT)**2)                                    16FEB89
            GAUS4 = EXP(-(RC+RCT)**2)                                    16FEB89
            CORRT = (A6(NIJ)*GAUS2 + A7(NIJ)*GAUS4)                      16FEB89
            CORR = RC * CORRT                                            16FEB89
            ATH = BET(NIJ)*(RC-X2EQ(NIJ))
            TH = A5(NIJ)*TANH(ATH)
            G = A1(NIJ) + GAUS*POLY + TH + CORR                          16FEB89
            FFIT(I,J) = FI(NIJ) + FIJ(NIJ)*G
            TRM1 = -2.D0*ALF(NIJ)*(RC-X1EQ(NIJ))*POLY
            TRM2 = A3(NIJ) + 2.0D0*A4(NIJ)*RC
            TRM12 = GAUS*(TRM1 + TRM2)
            TRMC1 = -2.D0*(RC-RCT)*A6(NIJ)*GAUS2                         16FEB89
            TRMC2 = -2.D0*(RC+RCT)*A7(NIJ)*GAUS4                         16FEB89
            TRMC = CORRT + RC * (TRMC1 + TRMC2)                          16FEB89
            IF(ABS(ATH).GE.44.44D0) THEN
             TRM3 = 0.D0
            ELSE
             TRM3 = BET(NIJ)*A5(NIJ)/(COSH(ATH)*COSH(ATH))
            END IF
            DFDRC(I,J) = FIJ(NIJ)*( TRM12 + TRM3 + TRMC )                16FEB89
250       CONTINUE
200     CONTINUE
C       NOW FIND THE EQUILIBRIUM VALUES OF X(I) AT RC
C       COMPUTE R1(RC),R2(RC) AND THETA(RC)
        TR2 = RC/TAR2
        T2R2 = SQRT(TR2*TR2 + 1.0D0)
        EXR2 = ALR2 * EXP(-BTR2*RC2)                                    11FEB89ST
        R2F = AR2 * ( TR2 + T2R2 ) + BR2 + EXR2                         11FEB89ST
        R1F = R2F - RC
        TETA = -ATET*TANH(BTET*RC) + CTET
        STETA = SIN(TETA)
        CTETA = COS(TETA)
C       EVALUATE THE CORRECTION TO K(18,6)                              12FEB89
        RPD = R(3) - (R1F + R2F)                                        10MAR89
        RPD2 = RPD**2
        RPG = EXP(-BTP*RPD2)                                            10MAR89
        DFDRP = - FFIT(18,6) * 2.0*BTP*RPD * RPG
        FFIT(18,6) = FFIT(18,6)*RPG
C       EVALUATE THE EQUILIBRIUM VALUES                                 23FEB89
        X0(1) =  0.D0
        X0(2) =  0.D0
        X0(3) =  0.D0
        X0(4) =  0.D0
        X0(5) =  0.D0
        X0(6) =  R1F
        X0(7) =  RH*STETA
        X0(8) =  0.D0
        X0(9) = -RH*CTETA
        X0(10) = -RHC*STETA
        X0(11) = -RHS*STETA
        X0(12) = -RH*CTETA
        X0(13) = -RHC*STETA
        X0(14) =  RHS*STETA
        X0(15) = -RH*CTETA
        X0(16) = 0.D0
        X0(17) = 0.D0
        X0(18) = -R2F
C       NOW EVALUATE THE DISPLACEMENT CARTESIANS, XND
        DO 376 IX=1,18
          XND(IX) = XN(IX) - X0(IX)
376     CONTINUE
C       NOW EVALUATE UVIB
C       NOTE THAT BECAUSE WE FIX C AT (0,0,0), XND(1)-XND(3) ARE ALWAYS
C       ZERO, AND THUS WE EXCLUDE THEM FROM THE ENERGY SUM. NOTE THAT
C       THE ASSOCIATED FORCE CONSTANTS, ALTHOUGH THEY PLAY NO ROLE IN THE
C       ENERGY DETERMINATION, DO HELP DETERMINE THE DERIVATIVES.
        SUM1 = 0.0D0
        DO 439 IE=4,18
          DO 437 JE=4,IE
            SUM1 = SUM1 + FFIT(IE,JE)*XND(IE)*XND(JE)
437       CONTINUE
439     CONTINUE
        SUM2 = 0.0D0
        DO 443 IE=4,18
          SUM2 = SUM2 + 0.5D0*FFIT(IE,IE)*XND(IE)*XND(IE)
443     CONTINUE
        EVIB = SUM1 - SUM2
C       ADD UVIB AND ULLR
        V = EVIB + ELLR
C       NOW EVALUATE (BY THE CHAIN RULE) DULLR/DXI
        DO 873 ID=1,3
          DR1DX(ID) = (X(ID) - X(ID+3))/R(1)
          DR2DX(ID) = (X(ID) - X(ID+15))/R(2)
873     CONTINUE
        DO 874 ID=4,6
          DR1DX(ID) = (X(ID) - X(ID-3))/R(1)
          DR3DX(ID) = (X(ID) - X(ID+12))/R(3)
874     CONTINUE
        DO 876 ID=16,18
          DR2DX(ID) = (X(ID) - X(ID-15))/R(2)
          DR3DX(ID) = (X(ID) - X(ID-12))/R(3)
876     CONTINUE
        DO 877 ID=1,3
          DU3BDX(ID) = DEDR(1)*DR1DX(ID) + DEDR(2)*DR2DX(ID)
877     CONTINUE
        DO 878 ID=4,6
          DU3BDX(ID) = DEDR(1)*DR1DX(ID) + DEDR(3)*DR3DX(ID)
878     CONTINUE
        DO 879 ID=16,18
          DU3BDX(ID) = DEDR(2)*DR2DX(ID) + DEDR(3)*DR3DX(ID)
879     CONTINUE
        DO 881 ID=7,15
          DU3BDX(ID) = 0.0D0
881     CONTINUE
C       FIND DRC/DX(KD) FOR KD=1-6,16-18
        DO 726 KD=1,3
         DRCDX(KD) = DR2DX(KD) - DR1DX(KD)
726     CONTINUE
        DO 727 KD=4,6
         DRCDX(KD) =  - DR1DX(KD)                                        30DEC88
727     CONTINUE
        DO 729 KD=16,18
         DRCDX(KD) = DR2DX(KD)                                           30DEC88
729     CONTINUE                
C       FIND DX0(KD)/DRC                                                 23FEB89
        DO 731 KD=1,5
         DX0DRC(KD) = 0.D0
731     CONTINUE
        TG6 = 2.D0*BTR2*RC*EXR2                                          11FEB89
        TRM6 = TR2/T2R2                       
        BRC = BTET*RC
        IF(ABS(BRC).GE.44.44D0) THEN
          CSHBRC = 0.D0
        ELSE
          CSHBRC = 1.D0/(COSH(BRC)*COSH(BRC))
        END IF
        DSTET = -ATET*BTET*CTETA*CSHBRC
        DCTET =  ATET*BTET*STETA*CSHBRC
        DX0DRC(6) = -0.5D0*(1.D0 - TRM6) - TG6                           11FEB89
        DX0DRC(7) = RH*DSTET
        DX0DRC(8) = 0.D0
        DX0DRC(9) = -RH*DCTET
        DX0DRC(10) = -RHC*DSTET
        DX0DRC(11) = -RHS*DSTET
        DX0DRC(12) = DX0DRC(9)
        DX0DRC(13) = DX0DRC(10)
        DX0DRC(14) = -DX0DRC(11)
        DX0DRC(15) = DX0DRC(9)
        DX0DRC(16) = 0.D0
        DX0DRC(17) = 0.D0
        DX0DRC(18) = -0.5D0*(1.D0 + TRM6) + TG6                          11FEB89
C       EVALUATE D(R30)/D(RC) WHICH IS NEEDED FOR DFDRC(18,6)
        DRPDRC = DX0DRC(6) - DX0DRC(18)                                 12FEB89
        DFDRC(18,6) = DFDRC(18,6) * RPG - DFDRP*DRPDRC                  12FEB89
C       NOW EVALUATE DUVIB/DXND(K)
        DO 452 KD=1,18
         SUM3 = 0.0D0
         DO 453 ID=1,KD
           SUM3 = SUM3 + FFIT(KD,ID) * XND(ID)
453      CONTINUE
         DO 454 ID=KD+1,18
           SUM3 = SUM3 + FFIT(ID,KD) * XND(ID)
454      CONTINUE
         DUVBDX(KD) = SUM3
452     CONTINUE
C       CORRECT THE DERIVATIVES OF X(1),X(2),X(3) FOR THE FACT THAT WE
C       NEED THE DERIVATIVE WITH RESPECT TO X, NOT WITH RESPECT TO XN
        SUM1 = 0.D0
        SUM2 = 0.D0
        SUM3 = 0.D0
        DO 922 J=4,16,3
         SUM1 = SUM1 - DUVBDX(J)
         SUM2 = SUM2 - DUVBDX(J+1)
         SUM3 = SUM3 - DUVBDX(J+2)
922     CONTINUE
        DUVBDX(1) = SUM1
        DUVBDX(2) = SUM2
        DUVBDX(3) = SUM3
C       ADD FOR KD=1-6,16-18, ADD THE CHAIN RULE TERM FOR DX0DX(KD)
        DO 456 KD=1,6
         SUM0 = 0.D0
         DO 457 ID=1,18
          SUM0 = SUM0 + DUVBDX(ID)*DX0DRC(ID) 
457      CONTINUE                                                       
         DUVBDY(KD) = DUVBDX(KD) - DRCDX(KD)*SUM0
456     CONTINUE
        DO 458 KD=16,18
         SUM0 = 0.D0
         DO 459 ID=1,18
          SUM0 = SUM0 + DUVBDX(ID)*DX0DRC(ID)
459      CONTINUE
         DUVBDY(KD) = DUVBDX(KD) - DRCDX(KD)*SUM0
458     CONTINUE
        DO 356 KD=7,15
         DUVBDY(KD) = DUVBDX(KD)
356     CONTINUE
C       NOW ADD THE DERIVATIVE TERMS DO TO THE DEPENDENCE OF THE FC'S ON RC
        DR3DX(1) = 0.D0                                                 10MAR89
        DR3DX(2) = 0.D0                                                 10MAR89
        DR3DX(3) = 0.D0                                                 10MAR89
        DO 342 KK=1,6
         SUMK = 0.0D0
         SUMI = 0.0D0
         DO 343 I=1,18
          DO 344 J=1,I        
           SUMK = SUMK + DFDRC(I,J)*XND(I)*XND(J)
344       CONTINUE
          SUMI = SUMI + DFDRC(I,I)*XND(I)*XND(I)
343      CONTINUE
         DKDX(KK) = DRCDX(KK)*(SUMK - 0.5D0*SUMI) 
     2              + DR3DX(KK) * DFDRP*XND(18)*XND(6)                  10MAR89
342     CONTINUE
        DO 346 KK=16,18
         SUMK = 0.0D0
         SUMI = 0.0D0
         DO 347 I=1,18
          DO 348 J=1,I        
           SUMK = SUMK + DFDRC(I,J)*XND(I)*XND(J)
348       CONTINUE
          SUMI = SUMI + DFDRC(I,I)*XND(I)*XND(I)
347      CONTINUE
         DKDX(KK) = DRCDX(KK)*(SUMK - 0.5D0*SUMI)
     2              + DR3DX(KK) * DFDRP*XND(18)*XND(6)                  10MAR89
346     CONTINUE                                                   
        DO 383 KK=1,6
         DUVBDY(KK) = DUVBDY(KK) + DKDX(KK)
383     CONTINUE
        DO 384 KK=16,18
         DUVBDY(KK) = DUVBDY(KK) + DKDX(KK)
384     CONTINUE
C       ADD PARTIAL DERIVATIVES TO YEILD DX(I)
        DO 462 KD=1,18
          DX(KD) = DUVBDY(KD) + DU3BDX(KD)
462     CONTINUE
        RETURN
        END
C
C
C
C     ENTRY POT FOR LEPSLR
      SUBROUTINE POTLLR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
      COMMON/LLRCM/R(3),E,DEDR(3),RC
      DIMENSION X(3),COUL(3),EXCH(3)
      DIMENSION QRC(3),ALPH(3),QRC2(3)                                  07DEC87
C     DIMENSION COTRM(3),ULRI(3),AQR(3),AQR4(3)                         0423TA02
      DIMENSION COTRM(3),ULRI(3)                                        0423TA02
      DIMENSION CTARG1(3)                                               25AUG88
      DIMENSION TD(3),DCDR(3),ALPP(3),QP(3),QP3(3)                      26DEC88
      DO 50 I=1,2                                                       13OCT88
      ARGZ = ZSLP*(R(I) - RM)                                           15OCT88
      ZTMP = Z(I) + DELZ*0.5D0*(1.D0 + TANH(ARGZ) )                     15OCT88
      IF(ABS(ARGZ).GE.44.44D0) THEN                                     15OCT88
        CZ = 1.D36                                                      15OCT88
      ELSE                                                              15OCT88
        CZ = (COSH(ARGZ))**2                                            15OCT88
      END IF                                                            15OCT88
      DZDR(I) = 0.5D0*DELZ*ZSLP/CZ                                      15OCT88
C   COMPUTE USEFUL CONSTANTS                                            13OCT88
      ZPO(I) = 1.0D0 + ZTMP                                             13OCT88
      OP3Z(I) = 1.0D0 + 3.0D0*ZTMP                                      13OCT88
      TOP3Z(I) = 2.0D0*OP3Z(I)                                          13OCT88
      ZP3(I) = ZTMP + 3.0D0                                             13OCT88
      TZP3(I) = 2.0D0*ZP3(I)                                            13OCT88
      DO4Z(I) = D(I)/4.0D0/ZPO(I)                                       13OCT88
   50 B(I) = BETA(I)*DO4Z(I)*2.0D0                                      13OCT88
      S = 0.0D0
      DO 21 I = 1,3
      X(I) = EXP(-BETA(I)*(R(I)-RE(I)))
      COUL(I) = DO4Z(I)*(ZP3(I)*X(I)-TOP3Z(I))*X(I)
      EXCH(I) = DO4Z(I)*(OP3Z(I)*X(I)-TZP3(I))*X(I)
   21 S = S+EXCH(I)
      RAD = SQRT((EXCH(1)-EXCH(2))**2+(EXCH(2)-EXCH(3))**2+
     1      (EXCH(3)-EXCH(1))**2)
      E = -RAD/R2
      DO 22 I = 1,3
      DEDR(I) = 0.D0                                                    03JUL83
      IF(X(I).LT.1.D-30) GO TO 22                                       03JUL83
      TZ = (3.0D0*EXCH(I)-S)/R2                                         15OCT88
      T= TZ*(OP3Z(I)*X(I)-ZP3(I))                                       03JUL83
C
C     PRINT OUT A WARNING IF DIVIDE BY ZERO IS GOING TO OCCUR--NOTE
C     THIS WILL NOT BE PRINTED OUT FOR THE CASE OF 0/0.                 11/21/85
      IF(ABS(RAD).LT.1.D-32.AND.ABS(T).GT.1.D-12) THEN
        WRITE(6,6000) T,RAD     
 6000   FORMAT(' IN LEPS POTENTIAL T,RAD=',1P,2E15.7,'  T/RAD SET TO T')
      ELSE IF(ABS(RAD).GT.1.D-32) THEN
        T = T/RAD 
        TZ = TZ/RAD
      END IF
C
      DEDRZ = DZDR(I)*(DO4Z(I)*X(I)*(X(I)-6.D0)-(COUL(I)/ZPO(I)) -
     2        TZ*(DO4Z(I)*X(I)*(3.D0*X(I)-2.D0) - (EXCH(I)/ZPO(I))))    15OCT88
      DEDR(I) = B(I)*X(I)*(T                                            03JUL83
     1          -ZP3(I)*X(I)+OP3Z(I)) + DEDRZ                           15OCT88 
   22 E = E+COUL(I)
      E = E+D(2)
C     NOW ADD THE LONG RANGE TERM                                       04DEC87
C     R(1) = R(CL-CH3), R(2) = R(CH3-CL') , R(3) = R(CL-CL')            05DEC87
C     WHERE CL' IS THE LEAVING GROUP                                    05DEC87
      RC = R(2) - R(1)                                                  05DEC87
      RC3 = -RC                                                         22AUG88
      FACTH = RC - AQ4                                                  22AUG88
      FACTHP = RC3 - AQ4                                                22AUG88
      AQR3 = AQ1*(1.D0-EXP(-AQ5*R(3)**2))
      ARGTH = AQR3*FACTH                                                22AUG88
      ARGTHP = AQR3*FACTHP                                              22AUG88
      QRC(1) = AQ3 + AQ2*0.5D0*(TANH(ARGTH)+ 1.0D0)                     25AUG88
      QRC(3) = AQ3 + AQ2*0.5D0*(TANH(ARGTHP)+ 1.0D0)                    25AUG88
      QRC(2) = -1.0D0 - QRC(1) -QRC(3)                                  06DEC87
      QRC2(1) = QRC(1)**2                                               07DEC87
      QRC2(3) = QRC(3)**2                                               07DEC87
      QRC2(2) = QRC(2)**2                                               07DEC87
      ALPH(1) = AALP2*QRC(1) + AALP3                                    01AUG88
      ALPH(3) = AALP2*QRC(3) + AALP3                                    01AUG88
      ALPH(2) = AALP4*QRC(2) + AALP5                                    05DEC87
C     The index in alphp is the index of the associated charge-         18SEP88
C     permanent dipole distance                                         18SEP88
C     NOTE: THE PRESCRIPTION USED TO COVER ALL IJ PAIRS IS WRITTEN      05DEC87
C     IN SUCH A WAY THAT THE R(I) AS DEFINED ABOVE GIVE THE CORRECT     05DEC87
C     DISTANCE R-IJ; EG. R(I) = R-IJ                                    05DEC87
      ULR = 0.0D0                                                       05DEC87
      DO 100 I=1,3                                                      05DEC87
        J = I + 1                                                       05DEC87
        IF(J.GT.3) J = 1                                                05DEC87
C       THIS IF IS TO AVOID DIVIDE BY ZEROES                            05DEC87
        IF(R(I).NE.0.D0)THEN                                            05DEC87
          RI2 = R(I)**2                                                 18SEP88
          RI4 = R(I)**4                                                 05DEC87
          RDIF = R(I) - RECO                                            26DEC88
          CTARG1(I) = CO1*RDIF                                          26DEC88
          COTRM(I) =(0.5D0*(1.0D0+TANH(CTARG1(I)) ))**2                 07SEP88
          UEL = (QRC(I)*QRC(J)) /  R(I)                                 09SEP88 
          UINDI = (ALPH(I)*QRC2(J)) / (2.0D0 * RI4)                     26AUG88
          UINDJ = (ALPH(J)*QRC2(I)) / (2.0D0 * RI4)                     26AUG88
C         TRMIJ = UEL + UPERM - UINDI - UINDJ (UPERM=0; IT'S UNDEFINED) 18SEP88
          TRMIJ = UEL - UINDI - UINDJ                                   11JUN89
        ELSE                                                            05DEC87
          TRMIJ = 0.0D0                                                 05DEC87
        END IF                                                          05DEC87
        ULRI(I) = TRMIJ                                                 07JAN88
C       NOTE: IF R=0 SUCH THAT TRMIJ IS SET = 0.0, THIS TRMIJ ALREADY   26AUG88
C       "INCLUDES" THE COTRM--HOWEVER, SINCE COTRM IS 0 FOR R=0, RE-    26AUG88
C       MULTIPLYING IT IS INCONSEQUENTIAL                               26AUG88
        TRMIJ = COTRM(I)*TRMIJ                                          26AUG88
        ULR = TRMIJ + ULR                                               05DEC87
100   CONTINUE                                                          05DEC87
      E = E + ULR                                                       05DEC87
      E = E + EASYM                                                     18OCT88
C     ADD A GAUSSIAN IN RC TO LOWER THE BARRIER TO THE SEMIEMPERICAL VALUEFEB89
      COF = 2.0D0*(0.52917706D0**2)
      EGAU = -0.002278850D0*EXP(-COF*(RC**2))
      E = E + EGAU
      DEGDRC = -2.D0*COF*RC*EGAU 
      DEGDR1 = -DEGDRC
      DEGDR2 = DEGDRC
C     NOW CALCULATE DERIVATIVES OF ULR                                  05DEC87
C     THE NEXT 2 SETS OF IF STATEMENTS WERE INSERTED TO AVOID OVERFLOW  19JAN88
C     ON THE VAX WHEN TRYING TO CALCULATE CPLS +/OR CMNS                19JAN88
C     THEY CAN BE SET DIFFERENTLY ON THE CRAY WHERE MUCH HIGHER         19JAN88
C     EXPONENTIALS ARE ALLOWED (YEILDING SLIGHTLY MORE ACCURATE         19JAN88
C     DERIVATIVES FOR VERY LARGE VALUES OF RC)                          19JAN88
      IF(ABS(ARGTH).GE.44.44D0) THEN                                    19JAN88
        CPLS = 1.D36                                                    19JAN88
      ELSE                                                              19JAN88
        CPLS = (COSH(ARGTH))**2                                         07JAN88
      END IF                                                            19JAN88
      IF(ABS(ARGTHP).GE.44.44D0) THEN                                   19JAN88
        CMNS = 1.D36                                                    19JAN88
      ELSE                                                              19JAN88
        CMNS = (COSH(ARGTHP))**2                                        07JAN88
      END IF                                                            19JAN88
      AQ22 = 0.5D0*AQ2
      QP(1) = AQ22*AQR3/CPLS
      QP(3) = - AQ22*AQR3/CMNS
      QP(2) = -(QP(1) + QP(3))
      DAQR3 = AQ1*AQ5*2.D0*R(3)*EXP(-(AQ5*R(3)**2))
      QP3(1) = AQ22*DAQR3*FACTH/CPLS
      QP3(3) = AQ22*DAQR3*FACTHP/CMNS
      QP3(2) = -(QP3(1) + QP3(3))
      ALPP(1) = AALP2
      ALPP(2) = AALP4
      ALPP(3) = ALPP(1)
      SUM1 = 0.D0
      SUM2 = 0.D0
      SUM3 = 0.D0
      DO 140 I=1,3                                                      10OCT88
        J = I+1                                                         10OCT88
        IF(J.GT.3) J=1                                                  10OCT88
        RI2 = R(I)**2                                                   10OCT88
        RI3 = RI2*R(I)
        RI4 = RI2*RI2
        RI5 = RI3*RI2                                                   10OCT88
        TQ1 = -(QP(I)*QRC(J)+QRC(I)*QP(J))/R(I)
        TA1A = (ALPP(I)*QP(I)*QRC2(J) + 2.D0*ALPH(I)*QRC(J)*QP(J) )
        TA1B = (ALPP(J)*QP(J)*QRC2(I) + 2.D0*ALPH(J)*QRC(I)*QP(I) )
        TA1 = (TA1A + TA1B)/(2.D0*RI4)
        TMP1 = (TQ1 + TA1)                                              26DEC88
        DULR1 = TMP1*COTRM(I)
        DULR2 = - DULR1
        TQ3 = (QP3(I)*QRC(J)+QRC(I)*QP3(J))/R(I)
        TA3A =-(ALPP(I)*QP3(I)*QRC2(J) + 2.D0*ALPH(I)*QRC(J)*QP3(J) )
        TA3B =-(ALPP(J)*QP3(J)*QRC2(I) + 2.D0*ALPH(J)*QRC(I)*QP3(I) )
        TA3 = (TA3A + TA3B)/(2.D0*RI4)
        TMP3 = (TQ3 + TA3)                                              26DEC88
        DULR3 = TMP3*COTRM(I)    
        TQ = - QRC(I)*QRC(J)/RI2
        TA = 2.D0*(ALPH(I)*QRC2(J) + ALPH(J)*QRC2(I))/RI5
        SUM1 = DULR1 + SUM1
        SUM2 = DULR2 + SUM2
        SUM3 = DULR3 + SUM3
        TD(I) = (TQ + TA)*COTRM(I)                                      26DEC88
        IF(ABS(CTARG1(I)).GE.44.44D0) THEN                              10OCT88
          CC = 1.D36                                                    10OCT88
        ELSE                                                            10OCT88
          CC = (COSH(CTARG1(I)))**2                                     10OCT88
        END IF                                                          10OCT88
        DCDR(I) = SQRT(COTRM(I))*CO1/CC
140   CONTINUE                                                          10OCT88
      SDULR1 = SUM1 + TD(1)
      SDULR2 = SUM2 + TD(2)
      SDULR3 = SUM3 + TD(3)
      TDULR1 = SDULR1 + ULRI(1)*DCDR(1)                                 26DEC88
      TDULR2 = SDULR2 + ULRI(2)*DCDR(2)                                 26DEC88
      TDULR3 = SDULR3 + ULRI(3)*DCDR(3)
      DEDR(1) = TDULR1 + DEDR(1) + DEGDR1                               23FEB89
      DEDR(2) = TDULR2 + DEDR(2) + DEGDR2                               23FEB89
      DEDR(3) = TDULR3 + DEDR(3)                                        10OCT88
9373  CONTINUE                                                          10OCT88
      RETURN
      END
     

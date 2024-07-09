        SUBROUTINE GASPOT(R,RC,RC2,QRC,QP,QP3,VGAS,DGASDX)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER(NATMAX=12,N3ATMX=3*NATMAX)
        COMMON /POTXCM/ V,X(N3ATMX),DX(N3ATMX)
        COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
        COMMON/VBINCM/A1(171),A2(171),A3(171),A4(171),A5(171),ALF(171),
     2       BET(171),X1EQ(171),X2EQ(171),FI(171),FIJ(171),AR2,TAR2,BR2
     2       ,ALR2,BTR2,ATET,BTET,CTET,RH,RHC,RHS                       11FEB89ST
     2       ,A6(171),A7(171),RCT,BTP                                   16FEB89
        DIMENSION R(3),DEDR(3)
        DIMENSION FFIT(18,18)
        DIMENSION DR1DX(18),DR2DX(18),DR3DX(18),XND(18),XN(18),X0(18)
        DIMENSION DU3BDX(18),DUVBDX(18),DUVBDY(18)
        DIMENSION DRCDX(18),DX0DRC(18),DFDRC(18,18),DKDX(18)
        DIMENSION QRC(3),QP(3),QP3(3),DGASDX(N3ATMX)
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
        CALL POTLLR(QRC,QP,QP3,R,ELLR,DEDR,RC)
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
        DFDRP = - FFIT(18,6) * 2.0D0*BTP*RPD * RPG                      09/95KAN
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
        VGAS = EVIB + ELLR
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
          DGASDX(KD) = DUVBDY(KD) + DU3BDX(KD)
462     CONTINUE
        RETURN
        END
C
      SUBROUTINE POTLLR(QRC,QP,QP3,R,ELLR,DEDR,RC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
      DIMENSION R(3),DEDR(3)
      DIMENSION X(3),COUL(3),EXCH(3)
      DIMENSION QRC(3),ALPH(3),QRC2(3)                                  07DEC87
      DIMENSION COTRM(3),ULRI(3)                                        01AUG88
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
 6000   FORMAT(' IN LEPS POTENTIAL T,RAD=',1P,2E15.7,'  T/RAD SET TO T')1113GL92
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
C     peRManent dipole distance                                         18SEP88
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
      ELLR = E + EASYM                                                  23MAR89
C     ADD A GAUSSIAN IN RC TO LOWER THE BARRIER TO THE SEMIEMPERICAL VALUEFEB89
      COF = 2.0D0*(0.52917706D0**2)
      EGAU = -0.002278850D0*EXP(-COF*(RC**2))
      ELLR = ELLR + EGAU                                                24MAR89
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
      IF(ABS(ARGTH).GE.44.44D0) THEN                                    6/19YP91
        CP = 1.D36                                                      10OCT88
      ELSE                                                              10OCT88
        CP = (COSH(ARGTH))**2                                           10OCT88
      END IF                                                            10OCT88
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
C     
        SUBROUTINE PREGAS
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
        CALL PRELLR
C       READ IN OTHER CONSTANTS, AND CONVERT TO ATOMIC UNITS
        READ(7,462) AR2,BR2,ALR2,BTR2                                   11FEB89ST
        READ(7,462) ATET,BTET,CTET
        READ(7,462) RH
462     FORMAT(4F20.5)
        WRITE(6,463) AR2,BR2,ALR2,BTR2,ATET,BTET,CTET,RH
463     FORMAT(/1X,'PARAMETERS FOR THE EQUILIBRIUM CARTESIAN COORDS ',
     2   'AS A FN OF RC',/1X,'FOR R2',14X,4F10.5/1X,'FOR THETA',11X,
     2    3F10.5/1X,'CH DISTANCE',9X,1F10.5/)
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
           READ(7,500) NDUM,(FC(I,J,K),J=1,MAXRD)
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
           READ(7,500) NDUM,(FC(I,J,K),J=6,MAXRD)
215      CONTINUE
 15     CONTINUE
        DO 20 I=11,18
         IF(I.LE.15)THEN
           MAXRD = I
         ELSE
           MAXRD = 15
         END IF
         DO 220 K=1,5
           READ(7,500) NDUM,(FC(I,J,K),J=11,MAXRD)
220      CONTINUE
 20     CONTINUE
        DO 25 I=16,18
         DO 225 K=1,5
           READ(7,500) NDUM,(FC(I,J,K),J=16,I)    
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
C                TYPE 1
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
C                TYPE 2
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
C                TYPE 3
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
C                TYPE 4
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
C                TYPE 7A
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
C                TYPE 7B
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
C                  TYPE 6A
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
C                  TYPE 6B
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
C                TYPE 5
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
C                 one of the type 6 foRMs?
                  ALF(NIJ) = 1.0D0                                      09/95KAN
                 ELSE
                 ALF(NIJ) = - (1.D0/RCT2)*LOG(ARG)
                 END IF
               END IF
             END IF
           END IF                  
          ELSE                                                          14FEB89
C           TYPE 10
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
            ALF(36) = 1.0D0                                             09/95KAN
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
C     PREPOT FOR LEPSLR
      SUBROUTINE PRELLR
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
  602 FORMAT (/36H POTENTIAL ENERGY SURFACE PARAMETERS//13H SATO-POLANYI
     1 //5H BOND,20X,2HAB,8X,2HBC,8X,2HAC//15H DISS. ENERGIES,5X,
     2 3F10.5//12H EQUILIBRIUM,8X,3F10.5//11H MORSE BETA,9X,3F10.5//
     3 16H SATO PARAMETERS,4X,3F10.5/)
  603 FORMAT(/1X,'LONG RANGE TERM PARAMETERS'/1X,'CHARGE FIT ',         18JAN88
     2 'COEFF. (1-5)',9X,4F10.5/32X,1F10.5/1X,
     2 'POLARIZABILITY FIT COEFF. (1-4)',1X,
     2 4F10.5,/1X,'CUT OFF COEFF. (1,2)',12X,2F10.5,/1X,
     2 'REACTANT ENERGY',27X,1F13.8/)                                    26DEC88
  604 FORMAT (/16H SATO SWITCHING ,4X,3F10.5/)                           13OCT88
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
C
        SUBROUTINE SETUP(N3TM)
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
        COMMON/VBINCM/A1(171),A2(171),A3(171),A4(171),A5(171),ALF(171),
     2       BET(171),X1EQ(171),X2EQ(171),FI(171),FIJ(171),AR2,TAR2,BR2
     2       ,ALR2,BTR2,ATET,BTET,CTET,RH,RHC,RHS                       11FEB89ST
     2       ,A6(171),A7(171),RCT,BTP                                   16FEB89
        COMMON/RWKCM/DW(3),ALFW(3),F12W,R0W,TET0W,Q2W,AHHW,ALFHHW,
     2        AOHW,ALFOHW,RMW,RSTARW,C6W,C8W,C10W,AOOW,ALFOOW,
     2        FOO1W,FOO2W,GNOO1W,GNOO2W,WOR0W,TQ2W,FQ2W,RDONW,
     2        NWT,NWIS
        COMMON/SSINCM/AQC,CQC,ALFQC,CH6(6),CH12(6),CO6(6),CO12(6),
     2                AEH(6),AEO(6),ALFH,ALFO,QH,QN,CO4(6)
        COMMON/E0COM/E0REAC
C       THIS ROUTINE INTERFACES ALL THE SETUP ROUTINES-THE GAS PHASE
C       (WHICH INCLUDES BOTH THE SETUP FOR UVIB AND FOR ULLR), THE
C       RWK2M MODIFIED WATER-WATER POTENTIAL, AND THE SOLUTE-SOLVENT POTENTIAL
C
C       E0REAC IS MINUS THE ENERGY OF THE REACTANTS, WHICH WILL BE ADDED
C       TO THE POTENTIAL AT ALL GEOMETRIES SO THAT ALL ENERGIES ARE RELATIVE
C       TO THE REACTANT'S AS ZERO OF ENERGY
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
      PARAMETER (N3TMMN = 27)
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
       OPEN (UNIT=4, FILE='potcwmc3b.dat', STATUS='OLD', 
     *       FORM='FORMATTED', ERR=100)
C
       OPEN (UNIT=7, FILE='potcwmcvib.dat', STATUS='OLD', 
     *       FORM='FORMATTED', ERR=100)
C
C
        E0REAC = 0.033920220D0
C
        CALL PREGAS
        CALL PRERWK
        CALL PRESS
C
C  CLOSE THE POTENTIAL DATA FILES
C
       CLOSE (UNIT=4)
       CLOSE (UNIT=7) 
C
1000     FORMAT(/,2X,T5,'WARNING: N3TM is set equal to ',I3,
     *                  ' but this potential routine',
     *          /,2X,T14,'requires N3TM be greater than or ',
     *                   'equal to ',I3,/)
C
        RETURN
C
  100 WRITE(6,*)'ERROR OPENING POTENTIAL DATA FILE'
      STOP 'SETUP 2'
C
        END
C        
        SUBROUTINE PRESS
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER(NATMAX=12,N3ATMX=3*NATMAX)
        PARAMETER(NWAT=(NATMAX-6)/3,NWAT1=NWAT-1)
        COMMON/TIPCM/DN,QHT,QM,RDONW1,R0CL
        COMMON/SSINCM/AQC,CQC,ALFQC,CH6(6),CH12(6),CO6(6),CO12(6),
     2                AEH(6),AEO(6),ALFH,ALFO,QH,QN,CO4(6)
        COMMON/RWKCM/DW(3),ALFW(3),F12W,R0W,TET0W,Q2W,AHHW,ALFHHW,
     2        AOHW,ALFOHW,RMW,RSTARW,C6W,C8W,C10W,AOOW,ALFOOW,
     2        FOO1W,FOO2W,GNOO1W,GNOO2W,WOR0W,TQ2W,FQ2W,RDONW,
     2        NWT,NWIS
C
C
C       THIS ROUTINE EVALUATES CONSTANTS NESCESSARY WHEN EVALUATING
C       THE SOLUTE-SOLVENT INTERACTION POTENTIAL
C       
C       UNIT CONVERSION PARAMETERS
        XKCAL = 1.D0/627.5095D0
        XANG = 1.D0/.52917706D0
        XIANG = 0.52917706D0
C       FIRST EVALUATE THE FIXED CHARGES ON THE WATER H'S AND N SITES
        QH = SQRT(Q2W)
        QN = -2.D0*QH
        QM=-0.95D0
        QHT=-QM/2.0D0
C       THE CHARGE PARAMETERS FOR FITTING THE CHARGE ON THE CARBON AS
C       A FUNCTION OF RC--CONVERT TO ATOMIC UNITS
        AQC = 0.214D0  
        CQC = -0.397D0
        ALFQC = 0.740D0*XIANG*XIANG
C       THE NON-COULOMBIC PARAMETERS: IN KCAL/MOL AND ANGSTROM
C       CONVERT TO ATOMIC UNITS SUBSEQUENTLY
C       NOTE: THE INDECES REFER TO THE SOLUTE ATOMS AS FOLLOWS
C           1  CARBON
C           2  CHLORINE
C           3  HYDROGEN
C           4  HYDROGEN
C           5  HYDROGEN
C           6  CHLORINE
C
        R0CL = R0W
        CH6(1) = 19.0353591D0
        CH6(2) =  0.0D0
        CH6(3) =  8.24252534D0
        CH6(4) =  8.24252534D0
        CH6(5) =  8.24252534D0
        CH6(6) =  0.D0
        CH12(1) = 6189.60681D0
        CH12(2) = 0.D0
        CH12(3) = 1452.69011D0
        CH12(4) = 1452.69011D0
        CH12(5) = 1452.69011D0
        CH12(6) =  0.D0
        CO4(1)=0.0D0
        CO4(2)=11.83333D0/XKCAL/XANG**4
        CO4(3)=0.0D0
        CO4(4)=0.0D0
        CO4(5)=0.0D0
        CO4(6)=11.83333D0/XKCAL/XANG**4
        CO6(1) = 110.689075D0
        CO6(2) =  -49.43258D0/XKCAL/XANG**6
        CO6(3) = 3.44879279D0
        CO6(4) = 3.44879279D0
        CO6(5) = 3.44879279D0
        CO6(6) =  -49.43258D0/XKCAL/XANG**6
        CO12(1) = 276483.570D0
        CO12(2) =  0.0D0
        CO12(3) = 3320.71268D0
        CO12(4) = 3320.71268D0
        CO12(5) = 3320.71268D0
        CO12(6) =  0.0D0
C       NOW CONVERT TO A.U.
        XCON4=XKCAL*(XANG**4)
        XCON6 = XKCAL*(XANG**6)
        XCON12 = XKCAL*(XANG**12)
        DO 100 I=1,6
         CO4(I)=CO4(I)*XCON4
         CH6(I) = CH6(I)*XCON6
         CO6(I) = CO6(I)*XCON6
         CH12(I) = CH12(I)*XCON12
         CO12(I) = CO12(I)*XCON12
100     CONTINUE
C       THE REMAINING NON-COULOMBIC PARAMETERS ARE ALREADY IN A.U.
        AEH(1) = 0.D0
        AEH(2) = 163.74D0
        AEH(3) = 0.D0
        AEH(4) = 0.D0
        AEH(5) = 0.D0
        AEH(6) = 163.74D0
        AEO(1) = 0.D0
        AEO(2)=1.426768D0
        AEO(3) = 0.D0
        AEO(4) = 0.D0
        AEO(5) = 0.D0
        AEO(6)=1.426768D0
        ALFH = 3.153588D0
        ALFO = 0.1497264D0
       WRITE(6,*)('#### 4,6,H,O,ALFH,ALFO='),CO4(6),CO6(6),AEH(6),
     2        AEO(6),ALFH,ALFO
  222  FORMAT(6E14.7)
        RETURN
        END
C
        SUBROUTINE SSPOT(R,RC,RC2,QRC,QP,QP3,XN1,XN2,XN3,VSS,DSSDX)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER(NATMAX=12,N3ATMX=3*NATMAX)
        PARAMETER(NWAT=(NATMAX-6)/3,NWAT1=NWAT-1)
        COMMON /POTXCM/ V,X(N3ATMX),DX(N3ATMX)
        COMMON/TIPCM/DN,QHT,QM,RDONW1,R0CL
        COMMON/SSINCM/AQC,CQC,ALFQC,CH6(6),CH12(6),CO6(6),CO12(6),
     2                AEH(6),AEO(6),ALFH,ALFO,QH,QN,CO4(6)
        COMMON/RWKCM/DW(3),ALFW(3),F12W,R0W,TET0W,Q2W,AHHW,ALFHHW,
     2        AOHW,ALFOHW,RMW,RSTARW,C6W,C8W,C10W,AOOW,ALFOOW,
     2        FOO1W,FOO2W,GNOO1W,GNOO2W,WOR0W,TQ2W,FQ2W,RDONW,
     2        NWT,NWIS
C
        DIMENSION QRC(3),QP(3),QP3(3),R(3)
        DIMENSION XN1(NWAT),XN2(NWAT),XN3(NWAT)
        DIMENSION DSSDX(N3ATMX),XMI(3),XM1(NWAT),XM2(NWAT),XM3(NWAT)
        DIMENSION XNI(3),QSU(6),DQDRC(6),DQDR3(6),RM(6),
     2            RH1(6),RH2(6),RO(6),RN(6),VL6H1(6),VL6H2(6),
     2            VL6O(6),VL12H1(6),VL12H2(6),VL12O(6),
     2            VLEH1(6),VLEH2(6),VLEO(6),VCH1(6),VCH2(6),
     2            VCN(6),VC(6),VL4O(6)
        DIMENSION DR1DX(18),DR2DX(18),DR3DX(18)
        DIMENSION DSSDXS(NWAT,18),DSSDXW(6,N3ATMX)
        DIMENSION DRH1DX(N3ATMX),DRH2DX(N3ATMX),DRODX(N3ATMX),
     1            DRNDX(N3ATMX)
C
C       THIS ROUTINE EVALUATES THE SOLUTE-SOLVENT INTERACTION POTENTIAL
C
C       EVALUATE THE SOLUTE CHARGES, QSU(6)
        QSU(2) = QRC(1)
        QSU(6) = QRC(3)
        EXQC = AQC * EXP(-ALFQC*RC2) 
        QSU(1) = EXQC + CQC
        QTMP = (QRC(2) - QSU(1))/3.D0
        QSU(3) = QTMP
        QSU(4) = QTMP
        QSU(5) = QTMP
C       EVALUATE DERIVATIVES OF QSU(J) W.R.T. R1,R2 AND R3
        DQDRC(2) =   QP(1)
        DQDRC(6) =   QP(3)
        DQDR3(2) =   QP3(1)
        DQDR3(6) =   QP3(3)
        DQDRC(1) = - 2.D0*ALFQC*RC*EXQC
        DQDR3(1) = 0.D0
        DO 15 K=3,5
         DQDRC(K) =   ( QP(2) - DQDRC(1) )/3.D0
         DQDR3(K) =   QP3(2)/3.D0
15      CONTINUE
C       ALSO EVALUATE DERIVATIVES OF R1,R2,R3 W.R.T. SOLUTE CARTESIANS
        DO 25 L=1,3
         M = L-1
         DR1DX(1+M) = (X(1+M) - X(4+M))/R(1)
         DR1DX(4+M) = (X(4+M) - X(1+M))/R(1)
         DR1DX(7+M) = 0.D0
         DR1DX(10+M) = 0.D0
         DR1DX(13+M) = 0.D0
         DR1DX(16+M) = 0.D0
         DR2DX(1+M) = (X(1+M) - X(16+M))/R(2)
         DR2DX(4+M) = 0.D0
         DR2DX(7+M) = 0.D0
         DR2DX(10+M) = 0.D0
         DR2DX(13+M) = 0.D0
         DR2DX(16+M) = (X(16+M) - X(1+M))/R(2)
         DR3DX(1+M) = 0.D0
         DR3DX(4+M) = (X(4+M) - X(16+M))/R(3)
         DR3DX(7+M) = 0.D0
         DR3DX(10+M) = 0.D0
         DR3DX(13+M) = 0.D0
         DR3DX(16+M) = (X(16+M) - X(4+M))/R(3)
25      CONTINUE
C       LOOP OVER ALL WATER MOLECULES
C       SET TERMS IN SUMS OVER I EQUAL TO ZERO
        DO 40 J=1,6
c         K = 3*(I-1) + 1         Mistakenly put I and J, changed 1104BE05
         K = 3*(J-1) + 1
         DO 30 L=1,3
          M = L-1
          DSSDX(K+M) = 0.D0
30       CONTINUE
40      CONTINUE
        VSUMI = 0.D0
        DO 110 I=1,NWT
         IF (I.GT.NWIS) GO TO 110
         NI = 9*(I-1)
         NI19 = NI + 19
         NI22 = NI + 22
         NI25 = NI + 25
         XNI(1) = XN1(I)
         XNI(2) = XN2(I)
         XNI(3) = XN3(I)
         XMI(1)=(XNI(1)-X(NI25))*RDONW1/RDONW+X(NI25)
         XMI(2)=(XNI(2)-X(NI25+1))*RDONW1/RDONW+X(NI25+1)
         XMI(3)=(XNI(3)-X(NI25+2))*RDONW1/RDONW+X(NI25+2)
         XM1(I)=XMI(1)
         XM2(I)=XMI(2)
         XM3(I)=XMI(3)
         VSUMJ = 0.D0
C        NOW EVALUATE THE REQUIRED DISTANCES, AND POWERS THEREOF
         DO 200 J=1,6
          K = 3*(J-1) + 1
          RH1(J) = SQRT(  ( X(NI19)   - X(K)   )**2 
     2                 +  ( X(NI19+1) - X(K+1) )**2
     2                 +  ( X(NI19+2) - X(K+2) )**2 )
          RH2(J) = SQRT(  ( X(NI22)   - X(K)   )**2 
     2                 +  ( X(NI22+1) - X(K+1) )**2
     2                 +  ( X(NI22+2) - X(K+2) )**2 )
          RO(J)  = SQRT(  ( X(NI25)   - X(K)   )**2 
     2                 +  ( X(NI25+1) - X(K+1) )**2
     2                 +  ( X(NI25+2) - X(K+2) )**2 )
          RN(J)  = SQRT(  ( XN1(I)    - X(K)   )**2 
     2                 +  ( XN2(I)    - X(K+1) )**2
     2                 +  ( XN3(I)    - X(K+2) )**2 )
          RM(J)  = SQRT(  ( XM1(I)    - X(K)   )**2
     2                 +  ( XM2(I)    - X(K+1) )**2
     2                 +  ( XM3(I)    - X(K+2) )**2 )
          R6H1 = RH1(J)**6                 
          R6H1I = 1.D0/R6H1
          R6H2 = RH2(J)**6
          R6H2I = 1.D0/R6H2
          R4O=RO(J)**4
          R4OI=1.0D0/R4O
          R6O = RO(J)**6
          R6OI = 1.D0/R6O
C         EVALUATE NON-COULOMBIC ENERGY TERMS
          VL6H1(J) = -CH6(J)/R6H1
          VL6H2(J) = -CH6(J)/R6H2
          VL4O(J)=-CO4(J)/R4O
          VL6O(J) = -CO6(J)/R6O
          VL12H1(J) = CH12(J)*R6H1I*R6H1I
          VL12H2(J) = CH12(J)*R6H2I*R6H2I
          VL12O(J) = CO12(J)*R6OI*R6OI
          VLEH1(J)  = AEH(J)*EXP(-ALFH*RH1(J))
          VLEH2(J)  = AEH(J)*EXP(-ALFH*RH2(J))
          VLEO(J)  = AEO(J)*EXP(-ALFO*RO(J))
C         EVALUATE COULOMBIC ENERGY TERMS
          VCH1(J) = QSU(J)*QH/RH1(J)
          VCH2(J) = QSU(J)*QH/RH2(J)
          VCN(J) = QSU(J)*QN/RN(J)
          IF (J.EQ.2.OR.J.EQ.6)THEN
          VLEO(J)  = AEO(J)*EXP(-ALFO*RO(J)**2)
          VCH1(J)=QSU(J)*QHT/RH1(J)
          VCH2(J)=QSU(J)*QHT/RH2(J)
          VCN(J)=QSU(J)*QM/RM(J)
          ENDIF
C         SUM ENERGY TERMS
          VL6 = VL6H1(J) + VL6H2(J) + VL6O(J)
          VL12 = VL12H1(J) + VL12H2(J) + VL12O(J)
          VL4 = VL4O(J)
          VLE = VLEH1(J) + VLEH2(J) + VLEO(J)
          VC(J) = VCH1(J) + VCH2(J) + VCN(J)
          VJ = VL6 + VL12 + VL4 + VLE + VC(J)
          PCLO=VL4O(J)+VL6O(J)+VL12O(J)+VLEO(J)
          PCLH=VLEH1(J)+VLEH2(J)+VL6H1(J)+VL6H2(J)+VL12H1(J)+VL12H2(J)
          PCLC=VC(J)
          VSUMJ = VSUMJ + VJ
200      CONTINUE
         VSUMI = VSUMI + VSUMJ
C
C        NOW EVALUATE DERIVATIVES OF VC W.R.T. INTRA-SOLUTE DISTANCES
         DVDR3 = 0.D0
         DVDRC = 0.D0
         DO 279 J=1,6
          DVDRC =  DVDRC + VC(J)*DQDRC(J)/QSU(J)
          DVDR3 =  DVDR3 + VC(J)*DQDR3(J)/QSU(J)
279      CONTINUE
         DVDR1 = -DVDRC
         DVDR2 =  DVDRC
C        NOW EVALUATE DERIVATIVES W.R.T. DISTANCES R OF THE INTERACTION
C        OF WATER MOLECULE I WITH THE SOLUTE
         DO 300 J=1,6
          DVDRH1 = -(6.D0*VL6H1(J) + 12.D0*VL12H1(J) + VCH1(J))/RH1(J)
     2        -(ALFH*VLEH1(J))
          DVDRH2 = -(6.D0*VL6H2(J) + 12.D0*VL12H2(J) + VCH2(J))/RH2(J)
     2        -(ALFH*VLEH2(J))
          DVDRO = -(4.0D0*VL4O(J)+6.D0*VL6O(J) + 12.D0*VL12O(J) )/RO(J)
     2              - ALFO*VLEO(J)
          DVDRN = - VCN(J)/RN(J)
          IF (J.EQ.2.OR.J.EQ.6) DVDRO = -(4.0D0*VL4O(J)+6.D0*VL6O(J) + 
     2       12.D0*VL12O(J) )/RO(J)
     2              - 2.0D0*RO(J)*ALFO*VLEO(J)
          IF (J.EQ.2.OR.J.EQ.6)DVDRN=-VCN(J)/RM(J)
C         NOW TRANSFORM TO DERIVATIVES WITH RESPECT TO CARTESIANS
C         NOW TRANSFORM TO DERIVATIVES WITH RESPECT TO CARTESIANS
C         FIRST EVALUATE THE DERIVATIVES OF R WITH RESPECT TO THE 
C         CARTESIANS OF WATER MOLECULE I, THEN WITH RESPECT TO THE
C         SOLUTE CARTESIANS
          K = 3*(J-1) + 1
          DO 340 L=1,3
           M = L-1
           DRH1DX(NI19+M) = (X(NI19+M) - X(K+M))/RH1(J)
           DRH2DX(NI22+M) = (X(NI22+M) - X(K+M))/RH2(J)
           DRODX(NI25+M) = (X(NI25+M) - X(K+M))/RO(J)
           DRH1DX(K+M) = -DRH1DX(NI19+M)
           DRH2DX(K+M) = -DRH2DX(NI22+M)
           DRODX(K+M) = -DRODX(NI25+M)
           DRNDX(K+M) =  (X(K+M) - XNI(1+M))/RN(J)
           DRNDX(NI19+M) = -RDONW*DRNDX(K+M)
           DRNDX(NI22+M) = -RDONW*DRNDX(K+M)
           DRNDX(NI25+M) = -(1.D0-2.D0*RDONW)*DRNDX(K+M)
           IF (J.EQ.2.OR.J.EQ.6) THEN
            DRNDX(K+M)=(X(K+M)-XMI(1+M))/RM(J)
           DRNDX(NI19+M) = -RDONW1*DRNDX(K+M)
           DRNDX(NI22+M) = -RDONW1*DRNDX(K+M)
           DRNDX(NI25+M) = -(1.0D0-2.D0*RDONW1)*DRNDX(K+M)
           ENDIF
C          NOW TRANSFORM
           DSSDXW(J,NI19+M)= DVDRH1*DRH1DX(NI19+M) + DVDRN*DRNDX(NI19+M)
           DSSDXW(J,NI22+M)= DVDRH2*DRH2DX(NI22+M) + DVDRN*DRNDX(NI22+M)
           DSSDXW(J,NI25+M)= DVDRO *DRODX(NI25+M)  + DVDRN*DRNDX(NI25+M)
           DSSDXS(I,K+M) = DVDRH1*DRH1DX(K+M)    
     2                   + DVDRH2*DRH2DX(K+M)    
     2                   + DVDRO*DRODX(K+M)    
     2                   + DVDRN*DRNDX(K+M)
     2                   + DVDR1*DR1DX(K+M)
     2                   + DVDR2*DR2DX(K+M)
     2                   + DVDR3*DR3DX(K+M)
C          THE NEXT TERM IS THE SUM OF THE DERIVATIVE, 
C          FOR THE K+M-TH CARTESIAN SOLUTE COORDINATE, 
C          OVER ALL WATER MOLECULES I
           DSSDX(K+M) = DSSDX(K+M) + DSSDXS(I,K+M)
340       CONTINUE
300      CONTINUE 
C        NOW SUM OVER SOLUTE INDICES, J, FOR X Y AND Z VALUES OF
C        THE DERIVATIVES W/ RESPECT TO WATER CARTESIAN COORDINATES
         DO 400 L=1,3
          M=L-1
          SUM19 = 0.D0
          SUM22 = 0.D0
          SUM25 = 0.D0
          DO 410 J=1,6
           SUM19 = SUM19 + DSSDXW(J,NI19+M)
           SUM22 = SUM22 + DSSDXW(J,NI22+M)
           SUM25 = SUM25 + DSSDXW(J,NI25+M)
410       CONTINUE
          DSSDX(NI19+M) = SUM19
          DSSDX(NI22+M) = SUM22
          DSSDX(NI25+M) = SUM25
400      CONTINUE
C        NOW CLOSE THE LOOP OVER I (DIFFERENT WATER MOLECULES)
110      CONTINUE
        VSS = VSUMI
        RETURN
        END
C       
        SUBROUTINE SURF(ENERGY, COORD, DCOORD, N3TM)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER(NATMAX=12,N3ATMX=3*NATMAX)
        PARAMETER(NWAT=(NATMAX-6)/3,NWAT1=NWAT-1)
        COMMON /POTXCM/ V,X(N3ATMX),DX(N3ATMX)
        COMMON/LRINCM/D(3),RE(3),BETA(3),Z(3),DELZ,ZSLP,RM,
     2       AQ1,AQ2,AQ3,AQ4,AQ5,AALP2,AALP3,AALP4,AALP5,
     2       CO1,RECO,EASYM,R2,DZDR(3),ZPO(3),OP3Z(3),TOP3Z(3),
     2       ZP3(3),TZP3(3),DO4Z(3),B(3)
        COMMON/VBINCM/A1(171),A2(171),A3(171),A4(171),A5(171),ALF(171),
     2       BET(171),X1EQ(171),X2EQ(171),FI(171),FIJ(171),AR2,TAR2,BR2
     2       ,ALR2,BTR2,ATET,BTET,CTET,RH,RHC,RHS                       11FEB89ST
     2       ,A6(171),A7(171),RCT,BTP                                   16FEB89
        COMMON/RWKCM/DW(3),ALFW(3),F12W,R0W,TET0W,Q2W,AHHW,ALFHHW,
     2        AOHW,ALFOHW,RMW,RSTARW,C6W,C8W,C10W,AOOW,ALFOOW,
     2        FOO1W,FOO2W,GNOO1W,GNOO2W,WOR0W,TQ2W,FQ2W,RDONW,
     2        NWT,NWIS
        COMMON/SSINCM/AQC,CQC,ALFQC,CH6(6),CH12(6),CO6(6),CO12(6),
     2                AEH(6),AEO(6),ALFH,ALFO,QH,QN,CO4(6)
        COMMON/E0COM/E0REAC
C
        DIMENSION QRC(3),QP(3),QP3(3),R(3),DGASDX(N3ATMX)
        DIMENSION XN1(NWAT),XN2(NWAT),XN3(NWAT),DRWKDX(N3ATMX)
        DIMENSION DSSDX(N3ATMX)
C
        DIMENSION COORD(N3TM),DCOORD(N3TM)
C
C       THIS ROUTINE CALLS THE INDIVIDUAL PIECES OF THE POTENTIAL--
C       THE GAS PHASE(INCLUDING UVIB AND ULLR), THE WATER-WATER POTENTIAL
C       AND THE SOLUTE-SOLVENT INTERACTION POTENTIAL--ADDS THE ENERGY
C       TERMS AND COMBINES THE DERIVATIVES TO GIVE THE TOTAL POTENTIAL V
C       AND THE TOTAL DERIVATIVES WITH RESPECT TO CARTESIANS, DX
C
C       SET ALL THE DERIVATIVES EQUAL TO ZERO, BECAUSE ONLY THE NON-ZERO
C       ELEMENTS WILL BE EVALUATED, AND WE WILL NEED TO PERFORM A SUM
        DO 110 I=1,N3ATMX
         DGASDX(I) = 0.D0
         DRWKDX(I) = 0.D0
         DSSDX(I) = 0.D0
         X(I) = 0.D0
110     CONTINUE
C
C    PLACE THE CARTESIAN COORDINATES FROM THE CALLING PROGRAM INTO THE 
C    CORRESPONDING POTENTIAL ARRAY
C
        DO 111 I = 1, 27
               X(I) = COORD(I)
  111   CONTINUE
C
        CALL GASPOT(R,RC,RC2,QRC,QP,QP3,VGAS,DGASDX)
C       NOTE: QRC(3) ARE THE 3  BODY CHARGES AS A FUNCTION OF RC AND R3
C             QP(3) ARE THE DERIVATIVES OF THESE CHARGES W/ RESPECT TO RC
C             QP3(3) ARE THE DERIVATIVES OF THESE CHARGES W/ RESPECT TO R3
C
        CALL RWKPOT(XN1,XN2,XN3,VRWK,DRWKDX)
C       NOTE: XN1(I),XN2(I), AND XN3(I) ARE THE X, Y AND Z CARTESIAN
C             COORDINATES OF THE SITE N OF NEGATIVVE CHARGE ON THE
C             I-TH WATER MOLECULE
C   
        CALL SSPOT(R,RC,RC2,QRC,QP,QP3,XN1,XN2,XN3,VSS,DSSDX)
C
C       NOW SUM THE ELEMENTS OF THE POTENTIAL
        IF(NWIS.LE.0)E0REAC=0.0D0
        ENERGY = VGAS + VRWK + VSS + E0REAC
C       NOW SUM THE DERIVATIVES
        DO 210 I=1,N3ATMX
         DX(I) = DGASDX(I) + DRWKDX(I) + DSSDX(I)
210     CONTINUE
         DO 211 I = 1, 27
  211           DCOORD(I) = DX(I)
        RETURN
        END
C        
        SUBROUTINE PRERWK
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/TIPCM/DN,QHT,QM,RDONW1,R0CL
        COMMON/RWKCM/DW(3),ALFW(3),F12W,R0W,TET0W,Q2W,AHHW,ALFHHW,
     2        AOHW,ALFOHW,RMW,RSTARW,C6W,C8W,C10W,AOOW,ALFOOW,
     2        FOO1W,FOO2W,GNOO1W,GNOO2W,WOR0W,TQ2W,FQ2W,RDONW,
     2        NWT,NWIS
        COMMON/BUNKCM/F11,F12,F13,F33,F111,F112,F113,F123,F133,F333,
     2        F1111,F1112,F1113,F1122,F1123,F1133,F1233,F1333,F3333
C
C       SET UP CONSTANTS, IN ATOMIC UNITS, FOR THE REIMERS AND WATTS, 
C       MODIFIED BY COKER, MILLER AND WATTS INTRAMOLECULAR WATER
C       POTENTIAL AS WELL AS FOR THE REIMERS, WATTS AND KLEIN
C       INTERMOLECULAR WATER POTENTIAL
C       FIRST, INPUT THE PARAMETERS IN KCAL/MOL AND ANGSTROM AND DEGREES
C       AND MULTIPLY BY THE PROPER CONVERSION FACTOR FOR ATOMIC UNITS
C
C       NUMBER OF WATER MOLECULES (NUMBER IN DIMENSIONS CAN BE LARGER)
        NWT = 2
        NWIS = NWT
C
C       CONVERSION FACTORS
        XKCAL = 1.D0/627.5095D0
        XANG = 1.D0/0.52917706D0
        XIANG = 0.52917706D0
        XANG2 = XANG*XANG
        XANG4 = XANG2*XANG2
        XANG8 = XANG4*XANG4
        XIA2 = XIANG*XIANG
C       INTRAMOLECUAR PARAMETERS
        DN=0.185D0*XANG
        F12W = -15.15333D0*XIA2*XKCAL
        R0W = 0.9572D0*XANG
        TET0W = 104.52D0*(ACOS(-1.D0)/180.D0)                           09/95KAN
        XJKCAL=XKCAL*6.022045D+02/4.184D0
        FRR=8.437D0*XJKCAL*XIANG**2
        F11=0.55D0*XJKCAL
        F12=-0.08531D0*XJKCAL*XIANG**2
        F13=0.3644D0*XJKCAL*XIANG
        F33=0.71758D0*XJKCAL
        F111=0.0D0
        F112=0.3963D0*XJKCAL*XIANG**3
        F113=0.0D0
        F123=-0.3163D0*XJKCAL*XIANG**2                                  09/95KAN
        F133=-0.2910D0*XJKCAL*XIANG
        F333=-0.6538D0*XJKCAL
        F1111=0.0557D0*XJKCAL
        F1112=0.0D0
        F1113=0.0D0
        F1122=0.0D0
        F1123=0.0D0
        F1133=-0.212*XJKCAL*XIANG**2
        F1233=0.0D0
        F1333=SQRT(FRR/2.0D0/F11)
        F3333=-1.098D0*XJKCAL                                           09/95KAN
C
C       INTERMOLECULAR PARAMETERS
        Q2W = 119.53D0*XANG*XKCAL
        AHHW = 631.92D0*XKCAL
        ALFHHW = 3.2806D0*XIANG
        AOHW = 2.0736D0*XKCAL
        ALFOHW = 7.3615D0*XIANG
        RMW = 1.63781D0*XANG
        RSTARW = 0.948347D0
        C6W = 625.45D0*XKCAL*XANG2*XANG4
        C8W = 3390.D0*XKCAL*XANG8
        C10W = 21200.D0*XKCAL*XANG8*XANG2
        AOOW = 3.2049D06*XKCAL
        ALFOOW = 4.9702D0*XIANG
        DONW = 0.26D0*XANG
        FOO1W = 3.8845D0*(XIANG**2.326D0)
        FOO2W = 1.7921D0*XIANG
        GNOO1W = 1.8817D0*XIANG
        GNOO2W = 0.2475D0*XIA2
C       
C       NOW COMPUTE USEFUL CONSTANTS
        WOR0W = 1.0D0/R0W
        TQ2W = 2.D0*Q2W
        FQ2W = 4.D0*Q2W
        RDONW = DONW/(2.D0*R0W*COS(0.5D0*TET0W) )
        RDONW1=RDONW/DONW*DN
        RETURN
        END
C
        SUBROUTINE RWKPOT(XN1,XN2,XN3,VRWK,DRWKDX)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER(NATMAX=12,N3ATMX=3*NATMAX)
        PARAMETER(NWAT=(NATMAX-6)/3,NWAT1=NWAT-1)
        COMMON /POTXCM/ V,X(N3ATMX),DX(N3ATMX)
        COMMON/RWKCM/DW(3),ALFW(3),F12W,R0W,TET0W,Q2W,AHHW,ALFHHW,
     2        AOHW,ALFOHW,RMW,RSTARW,C6W,C8W,C10W,AOOW,ALFOOW,
     2        FOO1W,FOO2W,GNOO1W,GNOO2W,WOR0W,TQ2W,FQ2W,RDONW,
     2        NWT,NWIS
        COMMON/BUNKCM/F11,F12,F13,F33,F111,F112,F113,F123,F133,F333,
     2        F1111,F1112,F1113,F1122,F1123,F1133,F1233,F1333,F3333
        COMMON/E0COM/E0REAC
        DIMENSION DR1DX(N3ATMX),DR2DX(N3ATMX),DR3DX(N3ATMX),
     2            DVADX(N3ATMX),DR11DX(N3ATMX),DR12DX(N3ATMX),
     2            DR21DX(N3ATMX),DR22DX(N3ATMX),DRO1DX(N3ATMX),
     2            DRO2DX(N3ATMX),DR1ODX(N3ATMX),DR2ODX(N3ATMX),
     2            DROODX(N3ATMX),DR1NDX(N3ATMX),DR2NDX(N3ATMX),
     2            DRN1DX(N3ATMX),DRN2DX(N3ATMX),DRNNDX(N3ATMX),
     2            DVEDX(N3ATMX),DRWKDX(N3ATMX) 
        DIMENSION R1(NWAT),R2(NWAT),R3(NWAT),
     2        AHOH(NWAT),CDHOH2(NWAT),SDHOH2(NWAT),DVADX1(NWAT),
     2        DVADX2(NWAT),DVADX3(NWAT),XN1(NWAT),XN2(NWAT),XN3(NWAT),
     2        DVADR1(NWAT),DVADR2(NWAT),DVADR3(NWAT),EINTRA(NWAT)
        DIMENSION R11(NWAT,NWAT1),R12(NWAT,NWAT1),R21(NWAT,NWAT1),
     2        R22(NWAT,NWAT1),RO1(NWAT,NWAT1),RO2(NWAT,NWAT1),
     2        R1O(NWAT,NWAT1),R2O(NWAT,NWAT1),ROO(NWAT,NWAT1),
     2        R1N(NWAT,NWAT1),R2N(NWAT,NWAT1),RN1(NWAT,NWAT1),
     2        RN2(NWAT,NWAT1),RNN(NWAT,NWAT1),VPR(NWAT,NWAT1),
     2        DV11DR(NWAT,NWAT1),DV12DR(NWAT,NWAT1),
     2        DV21DR(NWAT,NWAT1),DV22DR(NWAT,NWAT1),
     2        DVO1DR(NWAT,NWAT1),DVO2DR(NWAT,NWAT1),
     2        DV1ODR(NWAT,NWAT1),DV2ODR(NWAT,NWAT1),
     2        DV1NDR(NWAT,NWAT1),DV2NDR(NWAT,NWAT1),
     2        DVN1DR(NWAT,NWAT1),DVN2DR(NWAT,NWAT1),
     2        DVNNDR(NWAT,NWAT1),DVOODR(NWAT,NWAT1)
        DIMENSION DVPRDX(NWAT,NWAT1,N3ATMX),XNI(3),XNJ(3)
C
C       EVALUATE THE INTRAMOLECULAR POTENTIAL FOR EACH WATER MOLECULE
        SUMVA = 0.D0
        DO 110 I=1,NWT
         NI = 9*(I-1)
         NI19 = NI + 19
         NI22 = NI + 22
         NI25 = NI + 25
         R1SQ  =   (X(NI19)   - X(NI25)  )**2 
     2           + (X(NI19+1) - X(NI25+1))**2 
     2           + (X(NI19+2) - X(NI25+2))**2 
         R2SQ  =   (X(NI22)   - X(NI25)  )**2 
     2           + (X(NI22+1) - X(NI25+1))**2 
     2           + (X(NI22+2) - X(NI25+2))**2  
         R3SQ =   (X(NI19)   - X(NI22)  )**2 
     2           + (X(NI19+1) - X(NI22+1))**2 
     2           + (X(NI19+2) - X(NI22+2))**2  
         R1(I) = SQRT( R1SQ )
         R2(I) = SQRT( R2SQ )
         R3(I) = SQRT( R3SQ )
         IF (R1(I).EQ.0.0D0.OR.R2(I).EQ.0.0D0) THEN
            NWIS = I-1
            IF (NWIS.EQ.1) E0REAC = 0.0236522267D0
            GO TO 110
         ENDIF
         AHOH(I) = ACOS( (R1SQ + R2SQ - R3SQ)/( 2.D0*R1(I)*R2(I) ) )
         DHOH2 = 0.5D0 * ( AHOH(I) - TET0W )
         CDHOH2(I) = COS(DHOH2)
         SDHOH2(I) = SIN(DHOH2)
         D2=0.5D0
         D3=1.0D0/3.0D0
         D6=1.0D0/6.0D0
         D8=1.0D0/8.0D0
         D12=1.0D0/12.0D0
         D24=1.0D0/24.0D0
         AKH=F1333
         X1=R1(I)-R0W
         XR1=X1
         X1=1.0D0-EXP(-AKH*X1)
         X2=R2(I)-R0W
         XR2=X2
         X2=1.0D0-EXP(-AKH*X2)
         X3=AHOH(I)-TET0W
         V2ND=F11*X1**2+F11*X2**2+D2*F33*X3**2
     $        +F12*X1*X2/AKH**2+F13*(X1+X2)*X3/AKH
         V3RD=D6*F333*X3**3
     $          + D2*(F112/AKH**3+F12/AKH**2)*(X1*X1*X2+X1*X2*X2)
     $          + F13*(X1*X1*X3+X2*X2*X3)*D2/AKH
     $          + (F133*X1*X3*X3+F133*X2*X3*X3)*D2/AKH
     $          + F123*X1*X2*X3/AKH**2
         V4TH=F1111*X1**4+F1111*X2**4+D24*F3333*X3**4
     $          + D2**2*(F1133/AKH**2+F133/AKH)*(X1**2+X2**2)*X3**2
         EINTRA(I) = V2ND + V3RD + V4TH
         SUMVA = SUMVA + EINTRA(I)
C        NOW, WHILE STILL IN THE DO LOOP OVER NWT, EVALUATE SOME TERMS
C        WHICH WILL BE USEFUL IN THE DERIVATIVE CALCULATION. THE BULK OF
C        THE DERIVATIVE CALUCULATION WILL BE DONE SUBSEQUENTLY.
C        (A IS FOR THE A IN 'INTRA')
         DVADX1(I) = F11*X1/D2+F12*X2/AKH**2+F13*X3/AKH
     $             + D2*(F112/AKH**3+F12/AKH**2)*(X1*X2/D2+X2**2)
     $             + D2*F133*X3**2/AKH+F13*X1*X3/AKH+F123*X2*X3/AKH**2
     $          + D2*(F1133/AKH**2+F133/AKH)*X1*X3**2+F1111*X1**3/D2**2
         DVADX2(I) = F11*X2/D2+F12*X1/AKH**2+F13*X3/AKH
     $             + D2*(F112/AKH**3+F12/AKH**2)*(X1**2+X1*X2/D2)
     $             + D2*F133*X3**2/AKH+F13*X2*X3/AKH+F123*X1*X3/AKH**2
     $          + D2*(F1133/AKH**2+F133/AKH)*X2*X3**2+F1111*X2**3/D2**2
         DVADX3(I) = F33*X3+F13*(X1+X2)/AKH+D2*F333*X3**2+F133*(X1+X2)
     $             *X3/AKH+D2*F13*(X1**2+X2**2)/AKH+F123*X1*X2/AKH**2
     $             +D6*F3333*X3**3+D2*(F1133/AKH**2+F133/AKH)*(X1**2
     $             +X2**2)*X3
110      CONTINUE
111      VA = SUMVA
C        NOW EVALUATE THE INTERMOLECULAR POTENTIAL. 
C        FIRST EVALUATE THE COORDINATES OF SITE N(I) ON MOLECULE I
C        FOR ALL I
         DO 170 I=1,NWT
         IF (I.GT.NWIS) GO TO 170
          NI = 9*(I-1)
          NI19 = NI + 19
          NI22 = NI + 22
          NI25 = NI + 25
          XN1(I) = RDONW*( X(NI19) + X(NI22) - 2.D0*X(NI25)) 
     2              + X(NI25)
          XN2(I) = RDONW*( X(NI19+1) + X(NI22+1) - 2.D0*X(NI25+1)) 
     2              + X(NI25+1)
          XN3(I) = RDONW*( X(NI19+2) + X(NI22+2) - 2.D0*X(NI25+2)) 
     2              + X(NI25+2)
170      CONTINUE
C        NOW SUM OVER ALL PAIRS OF WATER MOLECULES.
         SUMVPR = 0.D0
         DO 310 I=2,NWT
          IF (I.GT.NWIS) GO TO 310
          NI = 9*(I-1)
          NI19 = NI + 19
          NI22 = NI + 22
          NI25 = NI + 25
          DO 305 J=1,I-1
           NJ = 9*(J-1)
           NJ19 = NJ + 19
           NJ22 = NJ + 22
           NJ25 = NJ + 25
C          FIRST FIND ALL REQUIRED SITE-SITE DISTANCES
           R11(I,J) = SQRT( ( X(NI19)   - X(NJ19)   )**2
     2                   +  ( X(NI19+1) - X(NJ19+1) )**2
     2                   +  ( X(NI19+2) - X(NJ19+2) )**2 )
           R12(I,J) = SQRT( ( X(NI19)   - X(NJ22)   )**2
     2                   +  ( X(NI19+1) - X(NJ22+1) )**2
     2                   +  ( X(NI19+2) - X(NJ22+2) )**2 )
           R21(I,J) = SQRT( ( X(NI22)   - X(NJ19)   )**2
     2                   +  ( X(NI22+1) - X(NJ19+1) )**2
     2                   +  ( X(NI22+2) - X(NJ19+2) )**2 )
           R22(I,J) = SQRT( ( X(NI22)   - X(NJ22)   )**2
     2                   +  ( X(NI22+1) - X(NJ22+1) )**2
     2                   +  ( X(NI22+2) - X(NJ22+2) )**2 )
           RO1(I,J) = SQRT( ( X(NI25)   - X(NJ19)   )**2
     2                   +  ( X(NI25+1) - X(NJ19+1) )**2
     2                   +  ( X(NI25+2) - X(NJ19+2) )**2 )
           RO2(I,J) = SQRT( ( X(NI25)   - X(NJ22)   )**2
     2                   +  ( X(NI25+1) - X(NJ22+1) )**2
     2                   +  ( X(NI25+2) - X(NJ22+2) )**2 )
           R1O(I,J) = SQRT( ( X(NI19)   - X(NJ25)   )**2
     2                   +  ( X(NI19+1) - X(NJ25+1) )**2
     2                   +  ( X(NI19+2) - X(NJ25+2) )**2 )
           R2O(I,J) = SQRT( ( X(NI22)   - X(NJ25)   )**2
     2                   +  ( X(NI22+1) - X(NJ25+1) )**2
     2                   +  ( X(NI22+2) - X(NJ25+2) )**2 )
           ROO(I,J) = SQRT( ( X(NI25)   - X(NJ25)   )**2
     2                   +  ( X(NI25+1) - X(NJ25+1) )**2
     2                   +  ( X(NI25+2) - X(NJ25+2) )**2 )
           R1N(I,J) = SQRT( ( X(NI19)   - XN1(J)    )**2
     2                   +  ( X(NI19+1) - XN2(J)    )**2
     2                   +  ( X(NI19+2) - XN3(J)    )**2 )
           R2N(I,J) = SQRT( ( X(NI22)   - XN1(J)    )**2
     2                   +  ( X(NI22+1) - XN2(J)    )**2
     2                   +  ( X(NI22+2) - XN3(J)    )**2 )
           RN1(I,J) = SQRT( ( XN1(I)    - X(NJ19)   )**2
     2                   +  ( XN2(I)    - X(NJ19+1) )**2
     2                   +  ( XN3(I)    - X(NJ19+2) )**2 )
           RN2(I,J) = SQRT( ( XN1(I)    - X(NJ22)   )**2
     2                   +  ( XN2(I)    - X(NJ22+1) )**2
     2                   +  ( XN3(I)    - X(NJ22+2) )**2 )
           RNN(I,J) = SQRT( ( XN1(I)    - XN1(J)    )**2
     2                   +  ( XN2(I)    - XN2(J)    )**2
     2                   +  ( XN3(I)    - XN3(J)    )**2 )
C          NOW EVALUATE THE POTENTIAL TERMS
C          START WITH THE 4 HH POTENTIALS
           EXP11 = AHHW * EXP( - ALFHHW * R11(I,J) )
           EXP12 = AHHW * EXP( - ALFHHW * R12(I,J) )
           EXP21 = AHHW * EXP( - ALFHHW * R21(I,J) )
           EXP22 = AHHW * EXP( - ALFHHW * R22(I,J) )
           V11 = EXP11 + Q2W/R11(I,J)
           V12 = EXP12 + Q2W/R12(I,J)
           V21 = EXP21 + Q2W/R21(I,J)
           V22 = EXP22 + Q2W/R22(I,J)
C          NOW THE OH TERMS
           EXPO1 = EXP( - ALFOHW * ( RO1(I,J) - RMW )  )
           EXPO2 = EXP( - ALFOHW * ( RO2(I,J) - RMW )  )
           EXP1O = EXP( - ALFOHW * ( R1O(I,J) - RMW )  )
           EXP2O = EXP( - ALFOHW * ( R2O(I,J) - RMW )  )
           VO1 = AOHW * (EXPO1 - 1.D0) * (EXPO1 - 1.D0) - AOHW
           VO2 = AOHW * (EXPO2 - 1.D0) * (EXPO2 - 1.D0) - AOHW
           V1O = AOHW * (EXP1O - 1.D0) * (EXP1O - 1.D0) - AOHW
           V2O = AOHW * (EXP2O - 1.D0) * (EXP2O - 1.D0) - AOHW
C          NOW THE TERMS INVOLVING N SITES
           V1N = - TQ2W/R1N(I,J)
           V2N = - TQ2W/R2N(I,J)
           VN1 = - TQ2W/RN1(I,J)
           VN2 = - TQ2W/RN2(I,J)
           VNN =   FQ2W/RNN(I,J)
C          NOW THE OO TERM
           EXPF = EXP(- FOO2W * ROO(I,J) )
           FNF = 1.D0 - FOO1W * ROO(I,J)**2.326D0 * EXPF
           GOO16 = GNOO1W/3.D0
           GOO26 = GNOO2W/SQRT(3.D0)
           GOO18 = GNOO1W/4.D0
           GOO28 = GNOO2W/2.D0
           GOO110 = GNOO1W/5.D0
           GOO210 = GNOO2W/SQRT(5.D0)
           ROO2 = ROO(I,J)*ROO(I,J)
           FNG6 = 1.D0 - EXP( - GOO16*ROO(I,J) - GOO26*ROO2 )
           FNG8 = 1.D0 - EXP( - GOO18*ROO(I,J) - GOO28*ROO2 )
           FNG10 = 1.D0 - EXP( - GOO110*ROO(I,J) - GOO210*ROO2 )
           RRSTR = RSTARW * ROO(I,J)
           C6T = C6W * (FNG6/RRSTR)**6
           C8T = C8W * (FNG8/RRSTR)**8
           C10T = C10W * (FNG10/RRSTR)**10
           CTRM = C6T + C8T + 1.5D0*C10T
           EXPOO = AOOW * EXP( - ALFOOW * ROO(I,J) )
           VOO = EXPOO - FNF * CTRM
C          NOW SUM THE TERM TO GET THE INTERACTION POTENTIAL FOR
C          THE IJ PAIR OF WATER MOLECULES
           VPR(I,J) = V11 + V12 + V21 + V22 + VO1 + VO2 + V1O + V2O
     2                    + V1N + V2N + VN1 + VN2 + VNN + VOO
           SUMVPR = SUMVPR + VPR(I,J)
C          NOW COMPUTE QUANTITIES WHICH WILL BE USEFUL IN THE DERIVATIVES
C          WE COMPUTE HERE ALL DERIVATIVES WITH RESPECT TO DISTANCES R
C          FOR THE I,J PAIR...TERM BY TERM
C          NOTE: DVKLDR MEANS D(V-KL)/D(R-KL), SINCE VKL DEPENDS ONLY ON
C          RKL AND NOT ON ANY OTHER R TERMS
C          START WITH THE HH TERMS AND DISTANCES
           DV11DR(I,J) = -ALFHHW * EXP11 - Q2W/(R11(I,J)*R11(I,J))
           DV12DR(I,J) = -ALFHHW * EXP12 - Q2W/(R12(I,J)*R12(I,J))
           DV21DR(I,J) = -ALFHHW * EXP21 - Q2W/(R21(I,J)*R21(I,J))
           DV22DR(I,J) = -ALFHHW * EXP22 - Q2W/(R22(I,J)*R22(I,J))
C          NOW THE OH TERMS
           DVO1DR(I,J) = -2.D0*ALFOHW*AOHW*EXPO1*(EXPO1 - 1.D0)
           DVO2DR(I,J) = -2.D0*ALFOHW*AOHW*EXPO2*(EXPO2 - 1.D0)
           DV1ODR(I,J) = -2.D0*ALFOHW*AOHW*EXP1O*(EXP1O - 1.D0)
           DV2ODR(I,J) = -2.D0*ALFOHW*AOHW*EXP2O*(EXP2O - 1.D0)
C          NOW THE TERMS INVOLVING AN N SITE
           DV1NDR(I,J) = - V1N/R1N(I,J)
           DV2NDR(I,J) = - V2N/R2N(I,J)
           DVN1DR(I,J) = - VN1/RN1(I,J)
           DVN2DR(I,J) = - VN2/RN2(I,J)
           DVNNDR(I,J) = - VNN/RNN(I,J)
C          NOW THE TERMS NESCESSARY FOR THE OO TERMS, AND THE OO TERM
           DFNFDR =  2.326D0 * (FNF - 1.D0)/ ROO(I,J)
     2              - FOO2W * (FNF - 1.D0)
           DG6DR = ( GOO16 + 2.D0*ROO(I,J)*GOO26 ) * (1.D0 - FNG6)
           DG8DR = ( GOO18 + 2.D0*ROO(I,J)*GOO28 ) * (1.D0 - FNG8)
           DG10DR = ( GOO110 + 2.D0*ROO(I,J)*GOO210 ) * (1.D0 - FNG10)
           DC6DR = 6.D0 * C6T * ( DG6DR/FNG6  -  1.D0/ROO(I,J) )
           DC8DR = 8.D0 * C8T * ( DG8DR/FNG8  -  1.D0/ROO(I,J) )
           DC10DR = 1.5D0*10.D0*C10T*( DG10DR/FNG10  -  1.D0/ROO(I,J) )
           DCTDR = DC6DR + DC8DR + DC10DR
           DVOODR(I,J) = -ALFOOW*EXPOO - DFNFDR*CTRM - FNF*DCTDR
C          NOW CLOSE THE DO LOOP
305       CONTINUE    
310      CONTINUE
C        E IS FOR THE E IN INTERMOLECULAR
         VE = SUMVPR
C        NOW SUM INTRA AND INTER MOLECULAR POTENTIALS
         VRWK = VA + VE
C        NOW TRANSFORM THE DERIVATIVES OF THE INTRAMOLECULAR POTENTIAL
C        WHICH ARE WITH RESPECT TO S TO DERIVATIVES WITH REPECT TO 
C        CARTESIAN COORDINATES. DO FOR EACH WATER MOLECULE I.
         DO 510 I=1,NWT
          IF (I.GT.NWIS) GO TO 510
C         FIRST THE COMPONENTS: START WITH D(HOH)/DR,
C         THE DERIVATIVE OF THE HOH ANGLE WITH RESPECT TO R1,R2 AND R3
          DEN = - 1.D0/SQRT( 1.D0 - (COS(AHOH(I)))**2 )
          R1SQ = R1(I)*R1(I)
          R2SQ = R2(I)*R2(I)
          R3SQ = R3(I)*R3(I)
          DHOHD1 = DEN*0.5D0*( 1.D0/R2(I) - R2(I)/R1SQ
     2                        + R3SQ/(R1SQ*R2(I)) )
          DHOHD2 = DEN*0.5D0*( 1.D0/R1(I) - R1(I)/R2SQ
     2                        + R3SQ/(R2SQ*R1(I)) )
          DHOHD3 = -DEN*R3(I)/(R1(I)*R2(I)) 
          DX1D1=AKH*EXP(-AKH*(R1(I)-R0W))
          DX2D2=AKH*EXP(-AKH*(R2(I)-R0W))
C         NOW SUM TO FIND DVA/DRK FOR EACH I(THE DERIVATIVE OF VA
C         WITH RESPECT TO INTERNUCLEAR DISTANCES, R3 IS R12 IN NOTES)
         DVADR1(I) = DVADX1(I)*DX1D1 + DVADX3(I)*DHOHD1
         DVADR2(I) = DVADX2(I)*DX2D2 + DVADX3(I)*DHOHD2
         DVADR3(I) = DVADX3(I)*DHOHD3
510      CONTINUE
C        NOW TRANSFORM THE DERIVATIVES WITH RESPECT TO DISTANCES
C        TO DERIVATIVES WITH RESPECT TO CARTESIAN COORDINATES
C
C        WE START WITH THE INTRAMOLECULAR POTENTIAL
C        FIRST, FOR EACH WATER MOLECULE I, FIND THE DERIVATIVE OF
C        R1, R2 AND R3 WITH RESPECT TO THE NINE CARTEIANS FOR THAT WATER
C        AND WE THEN FIND THE DERIVATIVE OF VA(I) W.R.T. EACH X
C        SINCE VA(I) ONLY DEPENDS ON THE X FOR THAT I, 
C        DVA(I)/DX = DVA/DX
         DO 610 I=1,NWT
           IF (I.GT.NWIS) GO TO 610
           NI = 9*(I-1)
           NI19 = NI + 19
           NI191 = NI19 + 1
           NI192 = NI19 + 2
           NI22 = NI + 22
           NI221 = NI22 + 1
           NI222 = NI22 + 2
           NI25 = NI + 25
           NI251 = NI25 + 1
           NI252 = NI25 + 2
           DR1DX(NI19)  = (X(NI19)  - X(NI25)  )/R1(I)
           DR1DX(NI191) = (X(NI191) - X(NI251) )/R1(I)
           DR1DX(NI192) = (X(NI192) - X(NI252) )/R1(I)
           DR1DX(NI22)  = 0.0D0
           DR1DX(NI221) = 0.0D0
           DR1DX(NI222) = 0.0D0
           DR1DX(NI25)  = -DR1DX(NI19)
           DR1DX(NI251) = -DR1DX(NI191)
           DR1DX(NI252) = -DR1DX(NI192)
           DR2DX(NI19)  = 0.0D0
           DR2DX(NI191) = 0.0D0
           DR2DX(NI192) = 0.0D0
           DR2DX(NI22)  = (X(NI22)  - X(NI25)  )/R2(I)
           DR2DX(NI221) = (X(NI221) - X(NI251) )/R2(I)
           DR2DX(NI222) = (X(NI222) - X(NI252) )/R2(I)
           DR2DX(NI25)  = - DR2DX(NI22)
           DR2DX(NI251) = - DR2DX(NI221)
           DR2DX(NI252) = - DR2DX(NI222)
           DR3DX(NI19)  = (X(NI19)  - X(NI22)  )/R3(I)
           DR3DX(NI191) = (X(NI191) - X(NI221) )/R3(I)
           DR3DX(NI192) = (X(NI192) - X(NI222) )/R3(I)
           DR3DX(NI22)  = - DR3DX(NI19)
           DR3DX(NI221) = - DR3DX(NI191)
           DR3DX(NI222) = - DR3DX(NI192)
           DR3DX(NI25)  = 0.0D0
           DR3DX(NI251) = 0.0D0
           DR3DX(NI252) = 0.0D0
C          NOW USE THE CHAIN RULE TO FIND D VA(I)/DX
           DVADX(NI19)  = DVADR1(I)*DR1DX(NI19)
     2                  + DVADR2(I)*DR2DX(NI19)
     2                  + DVADR3(I)*DR3DX(NI19)
           DVADX(NI191) = DVADR1(I)*DR1DX(NI191)
     2                  + DVADR2(I)*DR2DX(NI191)
     2                  + DVADR3(I)*DR3DX(NI191)
           DVADX(NI192) = DVADR1(I)*DR1DX(NI192)
     2                  + DVADR2(I)*DR2DX(NI192)
     2                  + DVADR3(I)*DR3DX(NI192)
           DVADX(NI22)  = DVADR1(I)*DR1DX(NI22)
     2                  + DVADR2(I)*DR2DX(NI22)
     2                  + DVADR3(I)*DR3DX(NI22)
           DVADX(NI221) = DVADR1(I)*DR1DX(NI221)
     2                  + DVADR2(I)*DR2DX(NI221)
     2                  + DVADR3(I)*DR3DX(NI221)
           DVADX(NI222) = DVADR1(I)*DR1DX(NI222)
     2                  + DVADR2(I)*DR2DX(NI222)
     2                  + DVADR3(I)*DR3DX(NI222)
           DVADX(NI25)  = DVADR1(I)*DR1DX(NI25)
     2                  + DVADR2(I)*DR2DX(NI25)
     2                  + DVADR3(I)*DR3DX(NI25)
           DVADX(NI251) = DVADR1(I)*DR1DX(NI251)
     2                  + DVADR2(I)*DR2DX(NI251)
     2                  + DVADR3(I)*DR3DX(NI251)
           DVADX(NI252) = DVADR1(I)*DR1DX(NI252)
     2                  + DVADR2(I)*DR2DX(NI252)
     2                  + DVADR3(I)*DR3DX(NI252)
610      CONTINUE
C        NOW WE WISH TO TRANSFORM FROM THE DERIVATIVE OF THE INTERMOLECULAR
C        POTENTIAL IN TERMS OF RXY(I,J) WHERE XY ARE THE SITE TYPES FOR
C        THE SITE ON W#I AND W#J, RESPECTIVELY
C
C        FIRST, WE LOOP OVER ALL I,J PAIRS. WE FIND HERE FIRST THE
C        DERIVATIVE OF RXY(I,J) W.R.T. ALL CARTESIAN COORDINATES FOR
C        WHICH THE DERIVATIVE IS NON-ZERO
C
C        THEN, WE USE THE CHAIN RULE TO FIND D( VPR(I,J) )/DX FOR ALL
C        X FOR WHICH THIS TERM IS NON-ZERO...THERE WILL BE 18 OF THESE
C        9 ASSOCIATED WITH W#I AND 9 ASSOCIATED WITH W#J
         DO 650 I=2,NWT
          IF (I.GT.NWIS) GO TO 650
          NI = 9*(I-1)
          NI19 = NI + 19
          NI22 = NI + 22
          NI25 = NI + 25
          XNI(1) = XN1(I)
          XNI(2) = XN2(I)
          XNI(3) = XN3(I)
          DO 645 J=1,I-1
           NJ = 9*(J-1)
           NJ19 = NJ + 19
           NJ22 = NJ + 22
           NJ25 = NJ + 25
           XNJ(1) = XN1(J)
           XNJ(2) = XN2(J)
           XNJ(3) = XN3(J)
C          SUM 640 IS OVER X Y AND Z COMPONENTS FOR A GIVEN SITE
           DO 640 L=1,3
            K = L-1
C           THE HH DERIVATIVES
            DR11DX(NI19+K) = ( X(NI19+K) - X(NJ19+K) )/R11(I,J)
            DR11DX(NJ19+K) = - DR11DX(NI19+K)
            DR12DX(NI19+K) = ( X(NI19+K) - X(NJ22+K) )/R12(I,J)
            DR12DX(NJ22+K) = - DR12DX(NI19+K)
            DR21DX(NI22+K) = ( X(NI22+K) - X(NJ19+K) )/R21(I,J)
            DR21DX(NJ19+K) = - DR21DX(NI22+K)
            DR22DX(NI22+K) = ( X(NI22+K) - X(NJ22+K) )/R22(I,J)
            DR22DX(NJ22+K) = - DR22DX(NI22+K)
C           THE OH, HO AND OO DERIVATIVES
            DRO1DX(NI25+K) = ( X(NI25+K) - X(NJ19+K) )/RO1(I,J)
            DRO1DX(NJ19+K) = - DRO1DX(NI25+K)
            DRO2DX(NI25+K) = ( X(NI25+K) - X(NJ22+K) )/RO2(I,J)
            DRO2DX(NJ22+K) = - DRO2DX(NI25+K)
            DR1ODX(NI19+K) = ( X(NI19+K) - X(NJ25+K) )/R1O(I,J)
            DR1ODX(NJ25+K) = - DR1ODX(NI19+K)
            DR2ODX(NI22+K) = ( X(NI22+K) - X(NJ25+K) )/R2O(I,J)
            DR2ODX(NJ25+K) = - DR2ODX(NI22+K)
            DROODX(NI25+K) = ( X(NI25+K) - X(NJ25+K) )/ROO(I,J)
            DROODX(NJ25+K) = - DROODX(NI25+K)
C           THE HN AND NH DERIVATIVES
            DR1NDX(NI19+K) = ( X(NI19+K) - XNJ(1+K) )/R1N(I,J)
            DR1NDX(NJ19+K) = - RDONW*DR1NDX(NI19+K)
            DR1NDX(NJ22+K) = - RDONW*DR1NDX(NI19+K)
            DR1NDX(NJ25+K) = - (1.D0 - 2.D0*RDONW)*DR1NDX(NI19+K)
            DR2NDX(NI22+K) = ( X(NI22+K) - XNJ(1+K) )/R2N(I,J)
            DR2NDX(NJ19+K) = - RDONW*DR2NDX(NI22+K)
            DR2NDX(NJ22+K) = - RDONW*DR2NDX(NI22+K)
            DR2NDX(NJ25+K) = - (1.D0 - 2.D0*RDONW)*DR2NDX(NI22+K)
            DRN1DX(NJ19+K) = ( X(NJ19+K) - XNI(1+K) )/RN1(I,J)
            DRN1DX(NI19+K) = - RDONW*DRN1DX(NJ19+K)
            DRN1DX(NI22+K) = - RDONW*DRN1DX(NJ19+K)
            DRN1DX(NI25+K) = - (1.D0 - 2.D0*RDONW)*DRN1DX(NJ19+K)
            DRN2DX(NJ22+K) = ( X(NJ22+K) - XNI(1+K) )/RN2(I,J)
            DRN2DX(NI19+K) = - RDONW*DRN2DX(NJ22+K)
            DRN2DX(NI22+K) = - RDONW*DRN2DX(NJ22+K)
            DRN2DX(NI25+K) = - (1.D0 - 2.D0*RDONW)*DRN2DX(NJ22+K)
C           THE NN DERIVATIVES
            DRNNDT = ( XNI(1+K) - XNJ(1+K) )/RNN(I,J)
            DRNNDX(NI19+K) =   RDONW*DRNNDT
            DRNNDX(NI22+K) =   RDONW*DRNNDT
            DRNNDX(NI25+K) =   (1.D0 - 2.D0*RDONW)*DRNNDT
            DRNNDX(NJ19+K) = - RDONW*DRNNDT
            DRNNDX(NJ22+K) = - RDONW*DRNNDT
            DRNNDX(NJ25+K) = - (1.D0 - 2.D0*RDONW)*DRNNDT
C           NOW USE THE CHAIN RULE TO FIND THE DERIVATIVES OF 
C           VPR(I,J) WITH RESPECT TO THE 18 RELEVANT CARTESIAN COORDINATES
C 
C           THE DERIVATIVES W.R.T. CARTESIAN COORDS OF W#I
            DVPRDX(I,J,NI19+K) = DV11DR(I,J)*DR11DX(NI19+K) 
     2                         + DV12DR(I,J)*DR12DX(NI19+K) 
     2                         + DV1ODR(I,J)*DR1ODX(NI19+K) 
     2                         + DV1NDR(I,J)*DR1NDX(NI19+K) 
     2                         + DVN1DR(I,J)*DRN1DX(NI19+K) 
     2                         + DVN2DR(I,J)*DRN2DX(NI19+K) 
     2                         + DVNNDR(I,J)*DRNNDX(NI19+K) 
            DVPRDX(I,J,NI22+K) = DV21DR(I,J)*DR21DX(NI22+K) 
     2                         + DV22DR(I,J)*DR22DX(NI22+K) 
     2                         + DV2ODR(I,J)*DR2ODX(NI22+K) 
     2                         + DV2NDR(I,J)*DR2NDX(NI22+K) 
     2                         + DVN1DR(I,J)*DRN1DX(NI22+K) 
     2                         + DVN2DR(I,J)*DRN2DX(NI22+K) 
     2                         + DVNNDR(I,J)*DRNNDX(NI22+K) 
            DVPRDX(I,J,NI25+K) = DVO1DR(I,J)*DRO1DX(NI25+K) 
     2                         + DVO2DR(I,J)*DRO2DX(NI25+K) 
     2                         + DVOODR(I,J)*DROODX(NI25+K) 
     2                         + DVN1DR(I,J)*DRN1DX(NI25+K) 
     2                         + DVN2DR(I,J)*DRN2DX(NI25+K) 
     2                         + DVNNDR(I,J)*DRNNDX(NI25+K) 
C           THE DERIVATIVES W.R.T. CARTESIAN COORDS OF W#J
            DVPRDX(I,J,NJ19+K) = DV11DR(I,J)*DR11DX(NJ19+K) 
     2                         + DV21DR(I,J)*DR21DX(NJ19+K) 
     2                         + DVO1DR(I,J)*DRO1DX(NJ19+K) 
     2                         + DVN1DR(I,J)*DRN1DX(NJ19+K) 
     2                         + DV1NDR(I,J)*DR1NDX(NJ19+K) 
     2                         + DV2NDR(I,J)*DR2NDX(NJ19+K) 
     2                         + DVNNDR(I,J)*DRNNDX(NJ19+K) 
            DVPRDX(I,J,NJ22+K) = DV12DR(I,J)*DR12DX(NJ22+K) 
     2                         + DV22DR(I,J)*DR22DX(NJ22+K) 
     2                         + DVO2DR(I,J)*DRO2DX(NJ22+K) 
     2                         + DVN2DR(I,J)*DRN2DX(NJ22+K) 
     2                         + DV1NDR(I,J)*DR1NDX(NJ22+K) 
     2                         + DV2NDR(I,J)*DR2NDX(NJ22+K) 
     2                         + DVNNDR(I,J)*DRNNDX(NJ22+K) 
            DVPRDX(I,J,NJ25+K) = DV1ODR(I,J)*DR1ODX(NJ25+K) 
     2                         + DV2ODR(I,J)*DR2ODX(NJ25+K) 
     2                         + DVOODR(I,J)*DROODX(NJ25+K) 
     2                         + DV1NDR(I,J)*DR1NDX(NJ25+K) 
     2                         + DV2NDR(I,J)*DR2NDX(NJ25+K) 
     2                         + DVNNDR(I,J)*DRNNDX(NJ25+K) 
640        CONTINUE
645       CONTINUE
650      CONTINUE                                
C        NOW SUM DVPRDX(I,J,K) OVER ALL (I,J) PAIRS FOR EACH K..ONLY INCLUDE
C        I,J PAIRS IN TH SUM FOR WHICH DVPRDX(I,J,K) IS NON-ZERO
         DO 750 N=1,NWT
          IF (N.GT.NWIS) GO TO 750
          NK = 9*(N-1) 
          NK19 = NK + 19
          NK22 = NK + 22
          NK25 = NK + 25
          DO 735 L=1,3
           LM = L-1
           SUMI19 = 0.D0
           SUMI22 = 0.D0
           SUMI25 = 0.D0
           SUMJ19 = 0.D0
           SUMJ22 = 0.D0
           SUMJ25 = 0.D0
           IF(N+1.GT.NWT)GO TO 731
           DO 730 I=N+1,NWT
            SUMI19 = SUMI19 + DVPRDX(I,N,NK19+LM)
            SUMI22 = SUMI22 + DVPRDX(I,N,NK22+LM)
            SUMI25 = SUMI25 + DVPRDX(I,N,NK25+LM)
730        CONTINUE
731        CONTINUE
           IF(N-1.LT.1)GO TO 721
           DO 720 J=1,N-1
            SUMJ19 = SUMJ19 + DVPRDX(N,J,NK19+LM)
            SUMJ22 = SUMJ22 + DVPRDX(N,J,NK22+LM)
            SUMJ25 = SUMJ25 + DVPRDX(N,J,NK25+LM)
720        CONTINUE
721        CONTINUE
           DVEDX(NK19+LM) = SUMI19 + SUMJ19
           DVEDX(NK22+LM) = SUMI22 + SUMJ22
           DVEDX(NK25+LM) = SUMI25 + SUMJ25
C          AND INTER AND INTRA MOLECULAR POTENTIAL DERIVATIVES FOR THE GIVEN X
           DRWKDX(NK19+LM) = DVEDX(NK19+LM) + DVADX(NK19+LM)
           DRWKDX(NK22+LM) = DVEDX(NK22+LM) + DVADX(NK22+LM)
           DRWKDX(NK25+LM) = DVEDX(NK25+LM) + DVADX(NK25+LM)
735       CONTINUE
750      CONTINUE
         RETURN
         END


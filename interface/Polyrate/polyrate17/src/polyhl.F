C**********************************************************************
C  HBT
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION hbt(A,C,S0,L,SMEP)
C
C     CALLED BY:
C               ZOCVCL, ZOCFRE
C
C     THIS FUNCTION RETURNS THE VALUE OF AN HYPERBOLIC TANGENT AT SMEP
C 
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION L
C
      PARAMETER(EXPLIM = 600.0)
C
      X  = (SMEP - S0) / L
      IF (X .GT. EXPLIM) THEN
         HBT =  A + C
      ELSE IF (X .LT. -EXPLIM) THEN
         HBT = -A + C
      ELSE
         HBT = A * TANH(X) + C
      ENDIF
C
      RETURN
C
      END FUNCTION hbt
C
C
C***********************************************************************
C  HINDRT  
C***********************************************************************
C
C     CALC REDUCED MOMENT OF INERTIA FOR A TORSIONAL MODE
C     Position vector X is mass-scaled, i.e., X = x * (amass/redm)**0.5,
C     where x is the cartesian coordinates of the system and amass is 
C     mass of the corresponding atom
C     Normal mode vector COFM is in cartesian
C
C
C     ADDED INTO POLYRATE BY YI-PING 3/18/91
C     For reference, see D.G.Truhlar, J. Comput. Chem. 12, 2661,1991
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C     CALLED BY
C            ANHARM
C 
      SUBROUTINE hindrt (IOP,IFRQ,NEND,IMIN,COFM,FMIHRM)
      use common_inc
      use perconparam, only : natoms,n3tm
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ANG(3),ANGO(3),COFM(N3TM),IDXSB(NATOMS),INDM(NATOMS)
C
C INITIALIZE DATA
C
      DENOM = 0.D0
      FMOM1 = 0.D0
      FMOM2 = 0.D0
      FMIHRM = 0.D0
      DO 5 I = 1, NATOMS
         INDM(I) = 0
5     CONTINUE
C
C  COMPUTE THE ANGULAR MOMENTUM FOR THE HINDMERED INTERNAL ROTATION
C
      IF (IOP.GE.0.AND.IOP.LT.7) THEN                                    0311YC98
         IEND = NARR -1
         MARR = 1
         IF (IEND.GE.1) THEN
            DO 10 I = 1, IEND
               IF (S.GE.SRARR(I)) MARR = MARR + 1
10          CONTINUE
         ENDIF
         NSB = NTRNUM(5,IFRQ)                                            0521YC99
         NOSYM = NTRSIG(5,IFRQ,IMIN)                                     0521YC99
         L = 0
         DO 15 I = 1, NEND, 3
            L = L + 1
            INDM(L) = L
15       CONTINUE
         NATOML = L                                                      4/6YL92
         DO 20 I = 1, NSB
             IDXSB(I) = NTRISB(5,IFRQ,I)                                 0521YC99
20       CONTINUE
      ELSE
         KOP = ABS(IOP)
         NSB = NTRNUM(KOP,NF(KOP)-IFRQ+1)                                0521YC99
         NOSYM = NTRSIG(KOP,NF(KOP)-IFRQ+1,IMIN)                         0521YC99
         L = 0
         DO 25 I = 1, NEND, 3
            L = L + 1
            INDM(L) = IATSV(L,KOP)
25       CONTINUE
         NATOML = L                                                      4/6YL92
         DO 30 I = 1, NSB
            IDXSB(I) = NTRISB(KOP,NF(KOP)-IFRQ+1,I)                      0521YC99
30       CONTINUE
      ENDIF
      MS = 2
      NS = 3
      DO 40 I = 1, 3
         ANGO(I) = 0.D0
         ANG(I) = 0.D0
         DO 50 J = 1, NATOML                                             4/6YL92
            KS = 3*(INDM(J) - 1) + MS
            LS = 3*(INDM(J) - 1) + NS
            ANGO(I) = ANGO(I) + X(KS)*COFM(LS) - X(LS)*COFM(KS)
50       CONTINUE
         DO 70 J = 1, NSB
            KS = 3*(IDXSB(J) - 1) + MS
            LS = 3*(IDXSB(J) - 1) + NS
            ANG(I) = ANG(I) - COFM(KS) * X(LS) + COFM(LS) * X(KS)
70       CONTINUE
         ANGO(I) = ANGO(I) - ANG(I)
         DENOM = DENOM + ANG(I)**2
         MS = NS
         NS = 5 - MS - I
40    CONTINUE
C
C------------------------------------------------------------------------------
C  THE FOLLOWING WRITE STATEMENTS ARE FOR INPUT OF PROGRAM INTROT.F, AND CAN
C  BE DELETED IF DESIRED
C
C      WRITE (FU6,*) NATOML        
C      WRITE (FU6,*) NSB
C     WRITE (FU6,*) (IDXSB(I),I=1,NSB)
C      WRITE (FU6,'(3(1X,9(F12.8,2X),/))') (X(I),I=1,27)                      XXXX
C      WRITE (FU6,'(3(1X,9(F12.8,2X),/))') (COFM(I),I=1,27)                   XXXX
C------------------------------------------------------------------------------
C
C      IF (LGS(4).NE.0) THEN               
C         WRITE (FU6,920) (IDXSB(I),I=1,NSB)
C         WRITE (FU6,930) (ANG(I),I=1,3)  
C         WRITE (FU6,940) (ANGO(I),I=1,3)
C      ENDIF
C
C DEFINE UNIT VECTOR OF THE HINDMERED INTERNAL ROTATIAON AXIS
C
      DO 80 I = 1, 3
         ANG(I) = ANG(I)/SQRT(DENOM)
80    CONTINUE
      SUMP = 0.D0
      SUMPT = 0.D0
C
C CALCULATE REDUCED MOMENT OF INERTIA
C
      DO 90 I = 1, NATOML                                                4/6YL92
         PROJX = 0.D0
         DO 100 J = 1, 3
            L = 3*(INDM(I)-1)+J
            PROJX = PROJX + X(L)*ANG(J)
            FMOM2 = FMOM2 + X(L)**2
100      CONTINUE
         SUMP = SUMP + PROJX**2
90    CONTINUE
      FMOM2 = FMOM2 - SUMP
      DO 110 I = 1, NSB
         PROJX = 0.D0
         DO 120 J = 1, 3
            L = 3*(IDXSB(I)-1)+J
            PROJX = PROJX + X(L)*ANG(J)
            FMOM1 = FMOM1 + X(L)**2
120      CONTINUE
         SUMPT = SUMPT + PROJX**2
110   CONTINUE 
      FMOM1 = FMOM1 - SUMPT
      FMOM2 = FMOM2 - FMOM1
c      FMIHRM = REDM*FMOM1*FMOM2/((FMOM1+FMOM2)*DBLE(NOSYM**2))
      FMIHRM = REDM*FMOM1*FMOM2/(FMOM1+FMOM2)
C
C      WRITE (FU6,990) FMOM1*CKGM2*REDM,FMOM2*CKGM2*REDM,FMIHRM*CKGM2
C      WRITE (FU6,950) (ANG(I),I=1,3)
C
920   FORMAT (//,1X,'The angular momentum of group 1:',9I2)
930   FORMAT (5X,3(5X,F15.9))
940   FORMAT (//,1X,'The angular momentum of group 2:',/,5X,
     * 3(5X,F15.9))
950   FORMAT (//,1X,'The direction of the rotational axis is ',
     * 3(5X,F10.7))
990   FORMAT (//,1X,'The moment of inertia of group 1, 2 and overall',
     * ' are (IN 10**-47 KGM2)',/,5X,3(5X,G21.15)) 
      RETURN
      END SUBROUTINE hindrt                                         
C
C
C***********************************************************************
C  HQSC
C***********************************************************************
C
      SUBROUTINE hqsc (XMU,A,B,NQ,E,EMAX,IERR)
      use perconparam, only : pi
C
C  SEMICLASSICAL SOLUTION OF HARMONIC-QUARTIC POTENTIAL FOR A>0,B<0
C     REWRITTEN BY BCG DEC. 1986
C
C     CALLED BY:
C                WKBPOT
C     CALLS:
C            ELLIP
C
C   Parameter include statement added 7/23/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DATA EPSX,EPS2 / 1.0D-8,1.0D-13 /
C*
      IERR = 0                                                           7/19/B91
      TST = EPSX
      XN = DBLE(NQ)+0.5D0
      IF (B.EQ.0.0D0) THEN
C
C  harmonic for B=0
C
         E = XN*SQRT(2.0D0*A/XMU)
      ELSE
C
C  harmonic-quartic
C
         BP = -B
         T1 = 0.5D0*A
         V0 = T1*T1/BP
         T1 = SQRT(XMU*A)
         THMX = 2.0D0*T1*A/(3.0D0*PI*BP)
         DTHC = 2.0D0*T1/(PI*A)
         E1 = V0
         FN1 = THMX-XN
         IF (FN1.LT.0.0D0) THEN
C
C  Quantum number is above the maximum; set energy to V0 and return
C
            E = 1.00001D0*V0
            IERR = 1                                                     7/19B91
         ELSE
C
C  Linear approximation to energy root
C
            E2 = XN*V0/THMX
            EL = 0.0D0
            EG = V0
C
C  Root search for energy level
C
   10       CONTINUE
C
C     Evaluate phase integral
C
            T1 = SQRT((V0-E)/V0)
            T2 = 1.0D0+T1
            RTT2 = SQRT(T2)
            RS = (1.0D0-T1)/T2
            CALL ELLIP (RS,EL1,EL2)
            TH = THMX*RTT2*(EL2-T1*EL1)
            FN2 = TH-XN
            IF (ABS(FN2).GT.EPSX) THEN
C*
C            ETST1 = E
C     Not converged, get next approximation to the energy
C
               IF (FN2.GT.0.0D0) EG = MIN(E2,EG)
               IF (FN2.LT.0.0D0) EL = MAX(E2,EL)
               E = 0.0D0
               IF (ABS(FN2).LT.ABS(FN1)) THEN
C
C     Take Newton-Raphson step; need derivative of phase integral
C
                  DTH = DTHC*EL1/RTT2
                  IF (DTH.NE.0.0D0) E = E2-FN2/DTH
               ENDIF
C
C     If we can't take a Newton-Raphson step take average of the minimum
C        and maximum energies.
C
               IF (E.LE.EL.OR.E.GE.EG) E = 0.5D0*(EL+EG)
               E1 = E2
               E2 = E
               FN1 = FN2
C
C  Loop back over interations in the root search
C
c               IF(E.NE.0.0D0)THEN
c               TST = ETST1 - E
c               ENDIF
c               IF(ABS(TST).GT.EPS2) GO TO 10
                GO TO 10
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE hqsc
C
C***********************************************************************
C  HRSET
C***********************************************************************
      SUBROUTINE hrset (SCOR,WM,FMOMM,FMHRT,IDTT,IFRQQ)
      use common_inc
C
C   CALC NEEDED INFOR FOR EVALUATING HINDERED ROTOR PARTITION FUNCTION
C
C   CALLED BY
C            HRPART
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
c      IFRQQ = NF(JTYPE)+1-IFRQ     
c      JFREQQ = ISHFT+NF(JTYPE)+1-IFRQ      
c
c     if the frequency is read in directly
c
c     IF((abs(TOROME(IDTT,IFRQQ,1)).LT.EPS).OR.(abs(SCOR).GT.EPS))        0611BL00
c    *   TOROME(IDTT,IFRQQ,1) = WM                                        0611BL00

c     IF((abs(TORMI(IDTT,IFRQQ,1)).LT.EPS).OR.(abs(SCOR).GT.EPS))         0611BL00
c    *   TORMI(IDTT,IFRQQ,1) = FMOMM                                      0611BL00
c
c     if the momentum of inertia for the initial configuration is read
c     in directly 
c
c     IF(READI(IDTT,IFRQQ).GT.EPS)TORMI(IDTT,IFRQQ,1)=READI(IDTT,IFRQQ) 0326JZ10 

      MTOL = NTRM(IDTT,IFRQQ)
c     MTOL = 0     
c     DO IMIN = 1, NTRNB(IDTT,IFRQQ)    
c       MTOL = MTOL + NTRSIG(IDTT,IFRQQ,IMIN)    
c     ENDDO      
c
      FMHRT = 0.0d0    
c
      IF (NTRLEV(IDTT,IFRQQ).NE.3) THEN        
        MINL = NTRNB(IDTT,IFRQQ)      
      ELSE  
        MINL = 1     
      ENDIF    
c 
      DO IMIN = 1, MINL     
c
c OW: calc I based on freq and barrier
c
        IF (NTRSCH(IDTT,IFRQQ).EQ.1) THEN   
          IF (IMIN.EQ.1) TOROME(IDTT,IFRQQ,IMIN)=WM
c reset some values if it is not read in
          IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                               0109BE07
     *        TORW(IDTT,IFRQQ,IMIN) = TORW(IDTT,IFRQQ,1)
          IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                               0109BE07
     *        TOROME(IDTT,IFRQQ,IMIN) = TOROME(IDTT,IFRQQ,1)
          TORMI(IDTT,IFRQQ,IMIN) = 0.5d0*TORW(IDTT,IFRQQ,IMIN) 
     *        *((MTOL/TOROME(IDTT,IFRQQ,IMIN))**2)     
c
c RO: calc W based on freq and I
c
        ELSEIF (NTRSCH(IDTT,IFRQQ).EQ.2) THEN         
          IF (IMIN.EQ.1) THEN     
            TORMI(IDTT,IFRQQ,IMIN) = FMOMM
          ELSE       
c reset some values if it is not read in      
C            IF ((NTRLEV(IDTT,IFRQ).EQ.3).OR.  
            IF ((NTRLEV(IDTT,IFRQQ).EQ.3).OR.                           0423TA02
     *          (TORU(IDTT,IFRQQ,IMIN).EQ.0.0))                         0109BE07     
     *           TORMI(IDTT,IFRQQ,IMIN) = TORMI(IDTT,IFRQQ,1)  
            ENDIF       
            IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                             0109BE07     
     *          TOROME(IDTT,IFRQQ,IMIN) = WM
                TORW(IDTT,IFRQQ,IMIN)=     
     *          2.0d0*TORMI(IDTT,IFRQQ,IMIN)  
     *          *((TOROME(IDTT,IFRQQ,IMIN)/MTOL)**2)      
c
c CO: calc W based on freq and I
c
        ELSEIF (NTRSCH(IDTT,IFRQQ).EQ.3) THEN     
          IF (IMIN.EQ.1) THEN    
            TORMI(IDTT,IFRQQ,1) = FMOMM
          ELSE 
c reset some values if it is not read in  
            IF ((NTRLEV(IDTT,IFRQQ).EQ.3).OR.       
     *        (TORU(IDTT,IFRQQ,IMIN).EQ.0.0))                           0109BE07
     *         TORMI(IDTT,IFRQQ,IMIN) = TORMI(IDTT,IFRQQ,1)    
          ENDIF     
          IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                               0109BE07
     *        TOROME(IDTT,IFRQQ,IMIN) = WM
          TORW(IDTT,IFRQQ,IMIN)=       
     *        2.0d0*TORMI(IDTT,IFRQQ,IMIN)     
     *        *((TOROME(IDTT,IFRQQ,IMIN)/MTOL)**2)     
c
c RW: calc freq based on W and I
c
        ELSEIF (NTRSCH(IDTT,IFRQQ).EQ.4) THEN     
          IF (IMIN.EQ.1) THEN     
            TORMI(IDTT,IFRQQ,1) = FMOMM
          ELSE    
c reset some values if it is not read in    
            IF ((NTRLEV(IDTT,IFRQQ).EQ.3).OR.     
     *          (TORU(IDTT,IFRQQ,IMIN).EQ.0.0))                         0109BE07    
     *           TORMI(IDTT,IFRQQ,IMIN) = TORMI(IDTT,IFRQQ,1)   
            IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                             0109BE07
     *            TORW(IDTT,IFRQQ,IMIN) = TORW(IDTT,IFRQQ,1)   
          ENDIF    
          TOROME(IDTT,IFRQQ,IMIN) =     
     *           SQRT((TORW(IDTT,IFRQQ,IMIN))/     
     *          (2.0d0*TORMI(IDTT,IFRQQ,IMIN)))*MTOL    
c
c CW: calc freq based on W and I
c
        ELSEIF (NTRSCH(IDTT,IFRQQ).EQ.5) THEN     
          IF (IMIN.EQ.1) THEN    
            TORMI(IDTT,IFRQQ,IMIN) = FMOMM
          ELSE     
c reset some values if it is not read in    
            IF ((NTRLEV(IDTT,IFRQQ).EQ.3).OR.     
     *          (TORU(IDTT,IFRQQ,IMIN).EQ.0.0))                         0109BE07   
     *           TORMI(IDTT,IFRQQ,IMIN) = TORMI(IDTT,IFRQQ,1) 
            IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                             0109BE07
     *          TORW(IDTT,IFRQQ,IMIN) = TORW(IDTT,IFRQQ,1)  
          ENDIF   
          TOROME(IDTT,IFRQQ,IMIN) =    
     *           SQRT((TORW(IDTT,IFRQQ,IMIN))/    
     *          (2.0d0*TORMI(IDTT,IFRQQ,IMIN)))*MTOL   
c 
c RWO: Keep all, use R
c
        ELSEIF (NTRSCH(IDTT,IFRQQ).EQ.6) THEN   
          IF (IMIN.EQ.1) THEN
            TORMI(IDTT,IFRQQ,1) = FMOMM
          ELSE
c reset some values if it is not read in
            IF ((NTRLEV(IDTT,IFRQQ).EQ.3).OR.
     *          (TORU(IDTT,IFRQQ,IMIN).EQ.0.0))                         0109BE07
     *           TORMI(IDTT,IFRQQ,IMIN) = TORMI(IDTT,IFRQQ,1)
            IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                             0109BE07
     *            TORW(IDTT,IFRQQ,IMIN) = TORW(IDTT,IFRQQ,1)
          ENDIF
          IF (TOROME(IDTT,IFRQQ,IMIN).EQ.0)
     *        TOROME(IDTT,IFRQQ,IMIN) = WM 
c
c CWO: Keep all, use C
c
        ELSEIF (NTRSCH(IDTT,IFRQQ).EQ.7) THEN 
          IF (IMIN.EQ.1) THEN
            TORMI(IDTT,IFRQQ,1) = FMOMM
          ELSE
c reset some values if it is not read in
            IF ((NTRLEV(IDTT,IFRQQ).EQ.3).OR.
     *        (TORU(IDTT,IFRQQ,IMIN).EQ.0.0))                           0109BE07
     *         TORMI(IDTT,IFRQQ,IMIN) = TORMI(IDTT,IFRQQ,1)
          ENDIF
          IF (TORU(IDTT,IFRQQ,IMIN).EQ.0)                               0109BE07  
     *        TOROME(IDTT,IFRQQ,IMIN) = WM
        ELSE 
          STOP 'HRSET SCHEME > 7 '    
        ENDIF   
        FMHRT = FMHRT + NTRSIG(IDTT,IFRQQ,IMIN)*
     *                         TORMI(IDTT,IFRQQ,IMIN)
      ENDDO      
C     FMHRT = FMHRT/DBLE(MINL)     
      FMHRT = FMHRT/DBLE(MTOL)     
      RETURN
      END SUBROUTINE hrset
c***********************************************************************
c     FACTORIAL
c***********************************************************************
c
c     CALLED BY
c            BESF
c
c     Created 10/27/05 by Ben Ellingson                                 1027BE05
c     Calculates the factorial of number 'm'
c
      function factorial(m)
      implicit double precision (a-h,o-z)
      factorial = 1
      if(m.gt.0) then
         do i = 1, m
            factorial = factorial*i
         enddo
      endif
      return
      end function factorial
C
c***********************************************************************
c     BESF
c***********************************************************************
c
c     CALLED BY
c            HRPART
c
c     Created 12/06/05 by Ben Ellingson                                 1206BE05
c     Calculates the I0 of number 'besz'
c
      function besf(besz)
      implicit doubleprecision (a-h,o-z) 
        BTOL = 0.001                                                     1102BE05
c BTOL is the maximum allowed ratio between I and I-1 BESF.              1102BE05
        BESF = 0                                                         1102BE05
        I = 0                                                            1102BE05
        BESRATIO = 0.9                                                   1102BE05
        DO WHILE (BESRATIO.GT.BTOL)                                      1102BE05
          BESNEW = (BESZ**(2*I))/((FACTORIAL(I)**2)*(2**(2*I)))          1102BE05
c          WRITE(FU6,*) BESNEW                                           1102BE05
c          WRITE(FU6,*) BESF                                             1102BE05
          IF (BESF.NE.0) THEN                                            1102BE05
            BESRATIO = ABS(BESNEW / BESF)                                1102BE05
          ENDIF                                                          1102BE05
c          WRITE(FU6,*) BESRATIO                                         1102BE05
          BESF = BESF + BESNEW                                           1102BE05
          I = I + 1                                                      1102BE05
        END DO                                                           1102BE05
      return
      end function besf
C
C
C***********************************************************************
C  HRPART
C***********************************************************************
C
C   CALC PARTITION FUNCTION FOR A TORSIONAL MODE WITH THE HINDERED ROTOR
C   APPROXIMATION
C
C     ADDED INTO POLYRATE BY YI-PING 3/18/91
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C
C     Debugged and methods added Spring-Winter of 2005 by Ben Ellingson
C
C     Debugged and reorganized April 2010 by Jingjing Zheng
C
C     CALLED BY
C            RATE
C
      FUNCTION hrpart (SCOR,WM,FMOMM,BKT,JTYPE,IFRQ,IMHR)
      use common_inc
      use perconparam
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
c  Variables for the AS method and Bessel function:
      DOUBLE PRECISION ASPA, ASPB, ASX, ASY, BESF, BESZ, BESNEW, BTOL, 
     >                 BESZR, BESZL, ASPAR, ASPBR
c  Variable for ZPC
      DOUBLE PRECISION DELTAZP
c
C-------------------------------------------------------------------------
C CALC PARTITION FUNCTION
C-------------------------------------------------------------------------
c
      MTOL = NTRM(JTYPE,IFRQ)
      NMIN = NTRNB(JTYPE,IFRQ)
      TORSIG = DBLE(MTOL)/DBLE(NMIN)                                    0326JZ10
C     QFR = SQRT(2.D0*PI*FMHRT*BKT)/TORSIG
c
      QHA = 0.0d0
      QI = 0.0d0
C     if the momentum of inertia for the initial configuration is read
c     in directly 
c
      IF(READI(JTYPE,IFRQ).GT.EPS)TORMI(JTYPE,IFRQ,1)=READI(JTYPE,IFRQ) 0326JZ10 
c
c     if the frequency is read in directly
c
      IF((abs(TOROME(JTYPE,IFRQ,1)).LT.EPS).OR.(abs(SCOR).GT.EPS))        0611BL00
     *   TOROME(JTYPE,IFRQ,1) = WM                                        0611BL00

      IF((abs(TORMI(JTYPE,IFRQ,1)).LT.EPS).OR.(abs(SCOR).GT.EPS))         0611BL00
     *   TORMI(JTYPE,IFRQ,1) = FMOMM                                      0611BL00
C
      DO I = 2, NMIN
        IF(TOROME(JTYPE,IFRQ,I).LT.EPS) 
     *     TOROME(JTYPE,IFRQ,I)= TOROME(JTYPE,IFRQ,1)
        IF(TORMI(JTYPE,IFRQ,I).LT.EPS)
     *     TORMI(JTYPE,IFRQ,I)= TORMI(JTYPE,IFRQ,1)
      ENDDO
c
C
C  data of H2O2 for debugging
c     TOROME(JTYPE,IFRQ,1)=381.9d0/219474.7d0
c     TOROME(JTYPE,IFRQ,2)=381.9d0/219474.7d0
c     TORMI(JTYPE,IFRQ,1)= 2755.9D0
c     TORMI(JTYPE,IFRQ,2)= 2755.9D0
c
C  MN or Chuang-Truhlar method
C
      IF (NTRMTD(JTYPE,IFRQ).EQ.1) THEN
C
c     set up correct values for
      CALL HRSET(SCOR,WM,FMOMM,FMHRT,JTYPE,IFRQ)
      QFR = SQRT(2.D0*PI*FMHRT*BKT)/TORSIG
c
c  FULL calculation
c
        IF (NTRLEV(JTYPE,IFRQ).EQ.1) THEN
          DO I = 1, NMIN
            UI = TORU(JTYPE,IFRQ,I)
            WI = TOROME(JTYPE,IFRQ,I)
            QHA = QHA + (EXP(-(UI+0.5d0*WI)/BKT))/(1-EXP(-WI/BKT))
            QI = QI + EXP(-UI/BKT)/WI
          ENDDO
          QI = BKT*QI
          IF (ABS(LGS(7)).GT.3) WRITE (FU6,101)
c
c  SF 
c
        ELSEIF (NTRLEV(JTYPE,IFRQ).EQ.2) THEN
          WI = TOROME(JTYPE,IFRQ,1)
          QHA = EXP(-0.5d0*WI/BKT)/(1-EXP(-WI/BKT))
          QI = BKT/WI
          SUM = 0.0d0
          DO I = 2, NMIN
            SUM = SUM + EXP(-TORU(JTYPE,IFRQ,I)/BKT)
          ENDDO
          SUM  = 1+ SUM
          QHA = QHA * SUM
          QI = QI * SUM
          IF (ABS(LGS(7)).GT.3) THEN
            WRITE (FU6,102)
            WRITE (FU6,104) SUM
          ENDIF
c
c SC
c
        ELSEIF (NTRLEV(JTYPE,IFRQ).EQ.3) THEN
          WI = TOROME(JTYPE,IFRQ,1)
C       QHA = EXP(-0.5d0*WI/BKT)/(1-EXP(-WI/BKT))
C
C Here we should use MC-HO partition function
C
          QHA = DBLE(NMIN)*EXP(-0.5D0*WI/BKT)/(1.D0-EXP(-WI/BKT))
          QI = BKT/WI
          IF (ABS(LGS(7)).GT.3) WRITE (FU6,103)
        ELSE
           STOP 'ERROR in HRPART, LEVEL > 3'
        ENDIF
c
        HRPART= QHA*DTANH(QFR/QI)                                      
C
c RPG and AS Method
c
      ELSEIF (NTRMTD(JTYPE,IFRQ).EQ.2.OR.NTRMTD(JTYPE,IFRQ).EQ.4) THEN  0326JZ10
        WI = TOROME(JTYPE,IFRQ,1)
        WW = 0.0D0
        TMI = 0.0D0
        HRPART = 0.0D0
        QHA = 0.0D0
        DO I = 1, NMIN
          WW = WW + TORWL(JTYPE,IFRQ,I)+TORWR(JTYPE,IFRQ,I)
          TMI = TMI + TORMI(JTYPE,IFRQ,I)
          QHAI = EXP(-0.5D0*TOROME(JTYPE,IFRQ,I)/BKT)/
     *           (1.D0-EXP(-TOROME(JTYPE,IFRQ,I)/BKT))
          QCHAI = BKT/TOROME(JTYPE,IFRQ,I)
          HRPART = HRPART + QHAI/QCHAI
          QHA = QHA + QHAI
        ENDDO
        WW = WW/DBLE(NMIN)/2.0D0
        FMHRT = TMI/DBLE(NMIN)
        HRPART = HRPART/DBLE(NMIN) 
C       QHA = EXP(-0.5D0*WI/BKT)/(1.D0-EXP(-WI/BKT))
C       QCHA= BKT/WI
        QFR = SQRT(2.D0*PI*FMHRT*BKT)/TORSIG
        BESZ = WW/(2.0D0*BKT)
C       HRPART = QHA*QFRI/QCHA*EXP(-0.5D0*WI/BKT)*EXP(-BESZ)*BESF(BESZ)
C       HRPART = QHA*QFR/QCHA*EXP(-BESZ)*BESF(BESZ)
        HRPART = HRPART*QFR*EXP(-BESZ)*BESF(BESZ)
C
        IF (NTRMTD(JTYPE,IFRQ).EQ.4) THEN
        ASX = 1.D0/QFR                                                  
        ASY = DSQRT(WW/(2.0d0*BKT))                                     0326JZ10
        ASPA = 0.003235d0*ASX - 0.026252d0*(ASX**2) +0.110460d0*(ASX**3)
     >  - 0.20334d0*(ASX**4) + 0.130633d0*(ASX**5)
     >  - 0.010112d0*ASY
     >  + 0.650122d0*ASX*ASY + 0.067112d0*(ASX**2)*ASY
     >  + 0.088807d0*(ASX**3)*ASY
     >  - 0.014290d0*(ASX**4)*ASY - 0.364852d0*ASY**2
     >  + 0.913073d0*ASX*ASY**2 - 0.021116d0*(ASX**2)*ASY**2
     >  - 0.092086d0*(ASX**3)*ASY**2 - 0.415689d0*(ASY**3)
     >  - 1.128961d0*ASX*(ASY**3) + 0.233009d0*(ASX**2)*(ASY**3)
     >  + 0.421334d0*(ASY**4) + 0.505139d0*ASX*(ASY**4)
     >  - 0.215088d0*(ASY**5)                                           
        ASPB =-0.067113d0*ASX + 0.772485d0*(ASX**2)-3.0674131d0*(ASX**3)0326JZ10
     >  + 4.595051d0*(ASX**4) -2.101341d0*(ASX**5)+0.0158d0*ASY
     >  + 0.102119d0*ASX*ASY - 0.555270d0*(ASX**2)*ASY
     >  - 1.125261d0*(ASX**3)*ASY
     >  + 0.071884d0*(ASX**4)*ASY
     >  - 0.397330d0*ASY**2 + 2.284956d0*ASX*ASY**2 
     >  + 0.850046d0*(ASX**2)*ASY**2
     >  - 0.174240d0*(ASX**3)*ASY**2 - 0.451875d0*ASY**3
     >  - 2.136226d0*ASX*(ASY**3) + 0.303469d0*(ASX**2)*(ASY**3)
     >  + 0.470837d0*(ASY**4) + 0.675898d0*ASX*(ASY**4)
     >  - 0.226287d0*(ASY**5)                                          
        BESZ = WW/(2.D0*BKT)                                            0326JZ10
        HRPART = HRPART*(1.D0+ASPB*EXP(-BESZ))/(1.D0+ASPA*EXP(-BESZ))   0326JZ10
        ENDIF
C  SRPG Method
C
      ELSEIF (NTRMTD(JTYPE,IFRQ).EQ.3) THEN                           
        HRPART = 0.0D0
        QHA = 0.0D0
        TMI = 0.0D0
        DO I = 1, NMIN 
c       DO I = 1, MTOL 
c         QFRI = SQRT(2.D0*PI*TORMI(JTYPE,IFRQ,I)*BKT)/TORSIG           0326JZ10
          QFRI = SQRT(2.D0*PI*TORMI(JTYPE,IFRQ,I)*BKT)                  0326JZ10
          UI = TORU(JTYPE,IFRQ,I)
          WI = TOROME(JTYPE,IFRQ,I)
          TMI = TMI + TORMI(JTYPE,IFRQ,I)
          QHAI = (EXP(-(UI+0.5d0*WI)/BKT))/(1.0D0-EXP(-WI/BKT))
          QCHA = BKT/WI
          BESZL = TORWL(JTYPE,IFRQ,I)/(2.0*BKT)
          BESZR = TORWR(JTYPE,IFRQ,I)/(2.0*BKT)
c         WRITE(FU6,'(2x,a1,i1,a3,f8.2)') 'U',I,' = ',UI
c         WRITE(FU6,'(2x,a5,i1,a3,f8.2)') 'Omega',I,' = ',WI*AUTOCM
c         WRITE(FU6,'(2x,a13,e12.4)') 'Harmonic Q = ',QHA
c         WRITE(FU6,*) 'QFR = ',QFRI
c         WRITE(FU6,*) QCHA
c         WRITE(FU6,*) BESZL
c         WRITE(FU6,*) BESZR
c         WRITE(FU6,*) RATIOL(JTYPE,IFRQ,I)
c         WRITE(FU6,*) RATIOR(JTYPE,IFRQ,I)
c         WRITE(FU6,*) BESF(BESZL)
c         WRITE(FU6,*) BESF(BESZR)
          HRPART = HRPART + EXP(-UI/BKT)* (QHAI * QFRI/QCHA) * 
     >             (RATIOL(JTYPE,IFRQ,I) * EXP(-BESZL) * BESF(BESZL)+
     >             RATIOR(JTYPE,IFRQ,I) * EXP(-BESZR) * BESF(BESZR))
          QHA = QHA + QHAI
        ENDDO
c       FMHRT = TMI/DBLE(MTOL)
        FMHRT = TMI/DBLE(NMIN)
        QFR = SQRT(2.D0*PI*FMHRT*BKT)/TORSIG
c       QHA = QHA/TORSIG
 
c
C
C  SAS Method
C
      ELSEIF (NTRMTD(JTYPE,IFRQ).EQ.5) THEN                            
        HRPART = 0.0D0
        QHA = 0.0D0
        TMI = 0.0D0 
c       DO I = 1, MTOL
        DO I = 1, NMIN
          UI = TORU(JTYPE,IFRQ,I)
          WI = TOROME(JTYPE,IFRQ,I)
          TMI = TMI + TORMI(JTYPE,IFRQ,I)
          QHAI = (EXP(-(UI+0.5d0*WI)/BKT))/(1.D0-EXP(-WI/BKT))
          QCHA = BKT/WI
          BESZL = TORWL(JTYPE,IFRQ,I)/(2.d0*BKT)
          BESZR = TORWR(JTYPE,IFRQ,I)/(2.d0*BKT)
c         QFRI = SQRT(2.D0*PI*TORMI(JTYPE,IFRQ,I)*BKT)/TORSIG           0326JZ10
          QFRI = SQRT(2.D0*PI*TORMI(JTYPE,IFRQ,I)*BKT)
c       ASX = 1.0d0/QFR                                                 1027BE05
C       ASY = TORWL(JTYPE,IFRQ,1)/BKT                                   1027BE05
        ASX = 1.0D0/QFRI                                                0326JZ10
        ASY = DSQRT(TORWL(JTYPE,IFRQ,I)/(2.0d0*BKT))                    0326JZ10
        ASPA = 0.003235d0*ASX - 0.026252d0*(ASX**2) +0.110460d0*(ASX**3)
     >  - 0.20334d0*(ASX**4) + 0.130633d0*(ASX**5)
     >  - 0.010112d0*ASY
     >  + 0.650122d0*ASX*ASY + 0.067112d0*(ASX**2)*ASY
     >  + 0.088807d0*(ASX**3)*ASY
     >  - 0.014290d0*(ASX**4)*ASY - 0.364852d0*ASY**2
     >  + 0.913073d0*ASX*ASY**2 - 0.021116d0*(ASX**2)*ASY**2
     >  - 0.092086d0*(ASX**3)*ASY**2 - 0.415689d0*(ASY**3)
     >  - 1.128961d0*ASX*(ASY**3) + 0.233009d0*(ASX**2)*(ASY**3)
     >  + 0.421334d0*(ASY**4) + 0.505139d0*ASX*(ASY**4)
     >  - 0.215088d0*(ASY**5)                                           
        ASPB =-0.067113d0*ASX + 0.772485d0*(ASX**2)-3.0674131d0*(ASX**3)0326JZ10
     >  + 4.595051d0*(ASX**4) -2.101341d0*(ASX**5)+0.0158d0*ASY
     >  + 0.102119d0*ASX*ASY - 0.555270d0*(ASX**2)*ASY
     >  - 1.125261d0*(ASX**3)*ASY
     >  + 0.071884d0*(ASX**4)*ASY
     >  - 0.397330d0*ASY**2 + 2.284956d0*ASX*ASY**2 
     >  + 0.850046d0*(ASX**2)*ASY**2
     >  - 0.174240d0*(ASX**3)*ASY**2 - 0.451875d0*ASY**3
     >  - 2.136226d0*ASX*(ASY**3) + 0.303469d0*(ASX**2)*(ASY**3)
     >  + 0.470837d0*(ASY**4) + 0.675898d0*ASX*(ASY**4)
     >  - 0.226287d0*(ASY**5)                                           
        ASY = DSQRT(TORWR(JTYPE,IFRQ,I)/(2.0d0*BKT))                    0326JZ10 
        ASPAR = 0.003235d0*ASX - 0.026252d0*(ASX**2) +0.11046d0*(ASX**3)
     >      - 0.203340d0*(ASX**4) + 0.130633d0*(ASX**5)
     >      - 0.010112d0*ASY
     >      + 0.650122d0*ASX*ASY + 0.067112d0*(ASX**2)*ASY
     >      + 0.088807d0*ASX**3*ASY
     >      - 0.014290d0*(ASX**4)*ASY - 0.364852d0*ASY**2
     >      + 0.913073d0*ASX*ASY**2 - 0.021116d0*(ASX**2)*ASY**2
     >      - 0.092086d0*(ASX**3)*ASY**2 - 0.415689d0*ASY**3
     >      - 1.128961d0*ASX*(ASY**3) + 0.233009d0*(ASX**2)*(ASY**3)
     >      + 0.421334d0*(ASY**4) + 0.505139d0*ASX*(ASY**4)
     >      - 0.215088d0*(ASY**5)                                      
        ASPBR =-0.067113d0*ASX +0.772485d0*(ASX**2)-3.0674131d0*(ASX**3)0326JZ10
     >     + 4.595051d0*(ASX**4) - 2.101341d0*(ASX**5) + 0.0158d0*ASY
     >     + 0.102119d0*ASX*ASY - 0.555270d0*(ASX**2)*ASY
     >     - 1.125261d0*(ASX**3)*ASY
     >     + 0.071884d0*(ASX**4)*ASY
     >     - 0.397330d0*ASY**2 + 2.284956d0*ASX*ASY**2 
     >     + 0.850046d0*(ASX**2)*ASY**2
     >     - 0.174240d0*(ASX**3)*ASY**2 - 0.451875d0*ASY**3
     >     - 2.136226d0*ASX*(ASY**3) + 0.303469d0*(ASX**2)*(ASY**3)
     >     + 0.470837d0*(ASY**4) + 0.675898d0*ASX*(ASY**4)
     >     - 0.226287d0*(ASY**5)                                       
          ASL = (1.d0+ASPB*EXP(-BESZL))/(1.d0+ASPA*EXP(-BESZL))
          ASR = (1.d0+ASPBR*EXP(-BESZR))/(1.d0+ASPAR*EXP(-BESZR))
          HRPART = HRPART + EXP(-UI/BKT)*QHAI * QFRI / QCHA *           0326JZ10
     >            (RATIOL(JTYPE,IFRQ,I)*ASL * EXP(-BESZL)* BESF(BESZL)
     >            + RATIOR(JTYPE,IFRQ,I)*ASR* EXP(-BESZR)* BESF(BESZR)) 
          QHA = QHA + QHAI
        ENDDO
c       FMHRT = TMI/DBLE(MTOL)
        FMHRT = TMI/DBLE(NMIN)
        QFR = SQRT(2.D0*PI*FMHRT*BKT)/TORSIG
c       QHA = QHA/TORSIG
C
C  Free Rotor Method
      ELSEIF (NTRMTD(JTYPE,IFRQ).EQ.6) THEN                              0131BE07
        TMI = 0.0D0
        DO I = 1, NMIN
          TMI = TMI + TORMI(JTYPE,IFRQ,I)
        ENDDO
        FMHRT = TMI/NMIN
        QFR = SQRT(2.D0*PI*FMHRT*BKT)/TORSIG
        HRPART = QFR                                                     0131BE07
c  using SC level to calculate QHA and QI for output purpose
        WI = TOROME(JTYPE,IFRQ,1)
        QHA = DBLE(NMIN)*EXP(-0.5D0*WI/BKT)/(1.D0-EXP(-WI/BKT))
        QI = BKT/WI
      ENDIF
c Now calculated I0 Bessel function for AS and HRDS schemes              1102BE05
C     IF (TORINTRP(JTYPE,IFRQ).EQ.2.OR.TORINTRP(JTYPE,IFRQ).EQ.3) THEN   1102BE05
C       HRPART = HRPART * BESF(BESZ)                                     1102BE05
C     ENDIF                                                              1102BE05
c ZPC scheme                                                             1028BE05
c     IF (TORZPC(JTYPE,IFRQ).EQ.2) THEN                                  1028BE05
c       DELTAZP = TOROME(JTYPE,IFRQ,1)/                                  1028BE05
c    >            (2 + 16 * (TORW(JTYPE,IFRQ,1)/TOROME(JTYPE,IFRQ,1)))   1028BE05
c       HRPART = HRPART * EXP(DELTAZP/BKT)                               1028BE05
c        WRITE(FU6,*) TOROME(JTYPE,IFRQ,1)                               1028BE05
c        WRITE(FU6,*) TORW(JTYPE,IFRQ,1)                                 1028BE05
c     ENDIF                                                              1028BE05
c
C
      IF (ABS(LGS(7)).GT.3) THEN
         WRITE (FU6,100) IMHR,JTYPE
c         WRITE (FU6,110) FMOMM                                          0615WH94
c  Now it prints out the 'I' used to calculate QFR                       0511BE05
         WRITE (FU6,110) FMHRT                                           0511BE05
         WRITE (FU6,111) NMIN, MTOL
         WRITE (fu6,190) TORSIG
         IF (NTRLEV(JTYPE,IFRQ).NE.3) THEN
            MINL = NTRNB(JTYPE,IFRQ)
         ELSE
            MINL = 1
         ENDIF
         WRITE (FU6,120) 
         WRITE (FU6,121)
         WRITE (FU6,122)
         IF(NMIN.EQ.1.AND.MTOL.GT.1) NTRSIG(JTYPE,IFRQ,1)=MTOL
         DO I = 1, MINL
           WRITE (FU6,125) I,NTRSIG(JTYPE,IFRQ,I),
     *             TOROME(JTYPE,IFRQ,I)*AUTOCM, 
     *             TORW(JTYPE,IFRQ,I)*CKCAL,
     *             TORU(JTYPE,IFRQ,I)*AUTOCM,
     *             TORMI(JTYPE,IFRQ,I)
         ENDDO
         WRITE (FU6,120)
         WRITE (FU6,130) QFR                                            0615WH94
         WRITE (FU6,140) QHA                                            0615WH94
         IF (NTRMTD(JTYPE,IFRQ).EQ.1)                                   0409JZ10
     *   WRITE (FU6,145) QI
         WRITE (FU6,150) HRPART                                         0615WH94
c         WRITE (99,1919) BKT/BK,FMOMM,QFR,QHA,HRPART                    0521YC99
 1919   FORMAT (F15.4,4E15.6)                                           0521YC99
      ENDIF
C
      RETURN
100   FORMAT ( 3X,'Mode ',I3,' of jtype ',I3,
     *            ' is treated as a hindered rotor.') 
101   FORMAT ( 3X,'CTLEVEL = FULL calculation.')
102   FORMAT ( 3X,'CTLEVEL = Single-Frequency calculation.')
103   FORMAT ( 3X,'CTLEVEL = Single-conformer calculation.')
104   FORMAT ( 3X,'Single-Frequency Factor = ',1PE12.4)
110   FORMAT ( 3X,'Total Moment of inertia = ',1PE12.4,' a.u.')   
111   FORMAT ( 3X,'P = ',I5,10X,'M = ',I5)
120   FORMAT ( 1X,75(1H=))
121   FORMAT ( 4x,'Minima',4x,'SIGMA',5x,'w',7x,'Barrier Height',7x,
     *         'U',15x,'I')
122   FORMAT ( 1X,8x,13x,'(cm-1)',6x,'(kcal/mol)',7x,'(cm-1)',10x,
     *         '(a.u.)',/1X,75(1H-))
125   FORMAT ( 3X,I5,3x,I5,3x,F9.3,5x,F9.3,7x,F9.3,5x,1PE12.4)
c 
130   FORMAT ( 3X,'Free rotor       partition function = ',1PE12.4)    
140   FORMAT ( 3X,'MC harmonic vib. partition function = ',1PE12.4)   
145   FORMAT ( 3X,'Intermediate     partition function = ',1PE12.4)
150   FORMAT ( 3X,'Hindered rotor   partition function = ',1PE12.4,/)   
190   FORMAT ( 3X,'Total Symmetry number = ',F4.1)  
      END FUNCTION hrpart                                          
C
C***********************************************************************
C  INITZE
C***********************************************************************
C
      SUBROUTINE initze
C
      use common_inc; use cm
      use perconparam; use rate_const
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  get the number of points along the MEP
c
      nsdm=0
      call get_number_of_mep_points
C
C Subroutine written to initialize variables, and arrays which are used 
C in version 5.0 of POLYRATE  Written 9/28/92  WH
C
C Changes have been made to be consistent with the new keyword input
C structure.  05/03/94
C
C Note: this subroutine does not initialize all the variables and arrays
C       most are still done within the subroutines that use them.
C
C     CALLED BY:
C                MAIN
C
C   Initialize the logical flags for the various tunneling options
C
      LCDSC  = .FALSE.
      LLCG   = .FALSE.
      LLCGG  = .FALSE.
      LMEP   = .FALSE.
      LTUN   = .FALSE.
      LSIGN  = .FALSE.                                                  0907WH94
      LBATH  = .FALSE.                                                  0317YC99
      LFOPT  = .FALSE.                                                  0317YC99
C
C  Initialize the logical flag for the VTST-IC calculation 
C  
      LZOC   = .FALSE.
      SSPMAX  = 0.0d0                                                   0606YC98
C
C
C  INITIALIZE THE ARRAYS USED TO STORE MU EFFECTIVE FOR THE SCT 
C  TUNNELING METHODS.
C
      call mem_initze
C
C  Initialize the energies of the stationary points along the MEP
C
      VP1S = 0.0D0
      VP1A = 0.0D0
      VP2S = 0.0D0
      VP2A = 0.0D0
C
      S = 0.0D0; egrndr=0.d00
C
      IFWKB = 0
C
      RETURN
C
      END subroutine initze
C
C***********************************************************************
C  INTEGR
C***********************************************************************
C
      SUBROUTINE integr (STEP,IST,NFUNC,KL,FISEN,LGS31)
      use perconparam
C
C     Integration driver for PATH subroutine for polyatomic VTST code.
C
C     Argument variables:
C        STEP  Integration step size
C        IST   Step counter
C        NFUNC No. of function calls
C        KL    Set to 0 to start integration
C        FISEN Sign of STEP (i.e., direction of S-propagation)
C
C     Include statement was added 6/18/91
C
C     CALLED BY:
C                PATH
C     CALLS:
C            INTEUL,INTPM                                               5/23B88
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IF (LGS31.EQ.0.OR.LGS31.EQ.2) THEN                                7/1/91VM
C
         CALL INTEUL (STEP,KL,NFUNC,FISEN)                              7/1/91VM
C
      ELSEIF (LGS31.EQ.30.OR.LGS31.EQ.32) THEN                          7/1/91VM
C
         CALL INTEUL (STEP,KL,NFUNC,FISEN)                              7/1/91VM
C
      ELSEIF (LGS31 .EQ. 5 .OR. LGS31 .EQ. 35) THEN                     7/1/91VM
C
        CALL INTPM (STEP, KL, NFUNC, FISEN)                             5/23B88
C
      ENDIF
C
      RETURN
      END subroutine integr
C
C***********************************************************************
C  INTEUL
C***********************************************************************
C
      SUBROUTINE inteul (H,KL,NFUNC,FISEN)
      use common_inc
      use perconparam
      use rate_const, only : v3
C
C     Euler one-step integration method
C
C     Called by INTEGR in polyatomic VTST program
C     Reference: "Applied Numerical Methods", by B. Carnahan,
C     H. A. Luther and J. O. Wilkes (Wiley, New York, 1969) P. 344
C
C     This is equivalent to the gradient following method
C     used in the original PATH routine.
C
C     M. J. Redmon 16-Oct-85
C
C     Include statements were added 6/18/91
C
C     CALLED BY:
C                INTEGR
C     CALLS:
C            INTFNC
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     Advance one step, using minus sign on DX assumes DX passed to
C        this routine is the actual gradient, not its negative
C
      IF (KL .EQ. 0 .AND. (LGS(31) .GE. 30 .AND. LGS(31) .LE. 32)) THEN 7/1/91VM
         DO 10 I = 1,N3                                                 7/1/91VM
            X(I) = X(I) - H*DX(I) + FISEN*(H*H*V3(I))/2.0D0             7/1/91VM
   10    CONTINUE                                                       7/1/91VM
      ELSE                                                              7/1/91VM
         DO 11 I = 1, N3
            X(I) = X(I)-DX(I)*H
   11    CONTINUE
      ENDIF                                                             7/1/91VM
      KL = KL + 1                                                       7/1/91VM
      S = S+H*FISEN
C
C     Call function to get negative of the derivative vector at new
C        point; sign of DX is changed in INTEGR before returning to
C        PATH
C
      IF (LGS(36).EQ.0) CALL INTFNC                                      6/5S89
      NFUNC = NFUNC+1
      RETURN
      END subroutine inteul
C
C***********************************************************************
C  INTFNC
C***********************************************************************
C
      SUBROUTINE intfnc
      use common_inc
      use perconparam, only : fu6
C
C     Returns NEGATIVE of normalized gradient for PATH integration
C     subroutines in polyatomic VTST code.
C
C     Modeled after NORMOD subroutine.
C
C     Author: M. J. Redmon
C             Chemical Dynamics
C             1550 W. Henderson Road
C             Columbus, OH 43220
C             15-Oct-85
C
C     COMMON Variables used:
C        N3   No. degrees of freedom (no. equations)
C        X    Mass weighted atomic coordinates
C        DX   Coordinate derivatives
C
C       Include statements added 6/18/91
C
C     CALLED BY:
C                INTEUL,IRCX
C     CALLS:
C            TRANS,FIRST
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
c     Compute gradient
c
c      call first(1)
      call ghook(1,iproc)
C
C     Find maximum derivative component
C
      DXMAX = 0.0D0
      DO 10 I = 1, N3
         T = ABS(DX(I))
         IF (T.GT.DXMAX) THEN
            DXMAX = T
         ENDIF
   10 CONTINUE
C
      IF (DXMAX.EQ.0.0D0) THEN
C
C        Error condition: print gradient and return
C
         WRITE (FU6,1000) (DX(I),I=1,N3)
C
      ELSE
C
C        Determine the normalization factor
C
         DXNORM = 0.0D0
         DO 20 I = 1, N3
            DX(I) = DX(I)/DXMAX
            DXNORM = DXNORM+DX(I)*DX(I)
   20    CONTINUE
         DXNORM = SQRT(DXNORM)
         DXMAG = DXNORM*DXMAX
C
C        Normalize the gradient vector
C
         DO 30 I = 1, N3
            DX(I) = DX(I)/DXNORM
   30    CONTINUE
C
      ENDIF
C
      RETURN
C
 1000 FORMAT (' INTFNC: DXNORM=0,  DX='/(1X,1P,10E13.5))
C
      END subroutine intfnc
C
C**********************************************************************
C INTPM2
C**********************************************************************
C
C     CALLED BY INTPM
C
      SUBROUTINE intpm2 (N, G0, XLAM, TS, F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION G0(N), XLAM(N)
C Evaluate ds/dt for Page-McIver gradient following algorithm
      F = 0.0D0
      DO 10 I = 1,N
         T = G0(I)*EXP(-XLAM(I)*TS)
         F = F + T*T
   10 CONTINUE
      F = SQRT(F)
      RETURN
      END SUBROUTINE intpm2
C
C**********************************************************************
C  INTRPL
C**********************************************************************
      SUBROUTINE intrpl (FUNC, SS, F, STP, JJ, NSDM, NS, ND1, ND2, N1, 
     *   N2)
C
C     Called by:
C               LCSET, LCNWTN, LCG34                                    0708JC00
C
C     Call POLINT
C
C     POLYNOMIAL INTERPOLATION USING POLINT
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION FUNC(ND1,ND2,NSDM), SS(NSDM), F(ND1,ND2)
      PARAMETER (NN=4)
      DIMENSION XA(NN),YA(NN)
C
      JNL = JJ - NN/2
      IF(JNL.LT.0) JNL = 0
      IF(JNL+NN .GT. NS) JNL = NS-NN
      DO 20 I2 = 1,N2
      DO 20 I1 = 1,N1
         DO 10 IJ = 1,NN
            YA(IJ) = FUNC(I1,I2,JNL+IJ)
            XA(IJ) = SS(JNL+IJ)
 10      CONTINUE
         CALL POLINT(XA,YA,NN,STP,YY,DYY)
         F(I1,I2) = YY
 20   CONTINUE
      RETURN
      END SUBROUTINE intrpl
C
C**********************************************************************
C INVRT 
C**********************************************************************
C
      SUBROUTINE invrt(N,A)
C
C     Invert an one-dimensional array of size N
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N)
C 
      IHALF = N / 2 
      DO 100 I = 1, IHALF
         TEMP = A(I)
         A(I) = A(N + 1 - I)
         A(N + 1 - I) = TEMP
100   CONTINUE
C
      RETURN
C 
      END SUBROUTINE invrt

C
C**********************************************************************
C INVRT2 
C**********************************************************************
C
      SUBROUTINE invrt2(N1,N2,NP1,A)
C
C     Invert a two-dimensional array of size N1,N2 
C     according to N2, the order in N1 will not be changed
C     Note: N1,N2 are used sizes and NP1 is the physical size
C           of the first dimension and  NP1 >= N1
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NP1,N2)
C 
      JHALF = N2 / 2 
      DO 100 J = 1, JHALF
         DO 100 I = 1, N1
            TEMP = A(I,J)
            A(I,J) = A(I, N2 - J + 1)
            A(I, N2 - J + 1) = TEMP
100   CONTINUE
      
C
      RETURN
C 
      END SUBROUTINE invrt2

C
C**********************************************************************
C INVRT3 
C**********************************************************************
C
      SUBROUTINE invrt3(N1,N2,N3,NP1,NP2,A)
C
C     Invert a three-dimensional array of size N1,N2,N3 
C     according to N3, the order in N1,N2  will not be changed
C     Note: N1,N2,N3 are used sizes and NP1,NP2 are physical
C           sizes. NP1 >= N1, NP2 >= N2
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NP1,NP2,N3)
C 
      KHALF = N3 / 2 
      DO 100 K = 1, KHALF
         DO 100 J = 1, N2
            DO 100 I = 1, N1
               TEMP = A(I,J,K)
               A(I,J,K) = A(I,J,N3 - K + 1)
               A(I,J,N3 - K + 1) = TEMP
100   CONTINUE
C
      RETURN
C 
      END SUBROUTINE invrt3

C
C***********************************************************************
C  IRCX
C***********************************************************************
C
      SUBROUTINE ircx (DELX,DELT2X,DIFFDX,FISEN,XOLD,DXOLD,NFUNC)
      use common_inc
      use perconparam, only : n3tm,fu6
      use keyword_interface, only : gufac6
C
C  Stabilization step for Euler one-step gradient following.
C     Given a point on the IRC and a predicted point by EULER method,
C     find the next point on the IRC, by the algorithm of K.  Ishida, K.
C     Morokuma, A.  Komornicki JCP 66, 2153 (1977) with modifications as
C     described by M.W.  Schmidt, M.S.  Gordon, and M.  Dupuis, JACS
C     107, 2585 (1985).
C
C  Rewritten by B.C. Garrett, Nov. 20, 1986
C
C  On input XOLD, and DXOLD are the coords and norm'd grad for the last
C  MEP point (Q0), while X, DX, and V contain the coords, norm'd grad,
C  and potential for the new MEP point (Q1).  The magnitude of the
C  gradient vector is passed from subroutine INTFNC in variable DXMAG of
C  COMMON BLOCK IRCCM
C
C     CALLED BY:
C                PATH
C     CALLS:
C            INTFNC
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 02/07/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION XOLD(N3TM),DXOLD(N3TM)
      DIMENSION Q1(N3TM),G1(N3TM),DIR(N3TM)
C
C
C  Loop over euler stepsize
C
   10 CONTINUE
C
C  Save current coord, norm'd grad, and potential
C
      DO 20 I = 1, N3
         Q1(I) = X(I)
         G1(I) = DX(I)
   20 CONTINUE
      V1 = V
C
C  Compute the linear search direction and deriv of V along this vector
C     note that DX and DXOLD are already normalized
C
      TNORM = 0.0D0
      DO 30 I = 1, N3
         T = DXOLD(I)-DX(I)
         TNORM = TNORM+T*T
         DIR(I) = T
   30 CONTINUE
C
C  If TNORM is too small then the gradients are almost the same and
C     stablization is unnecessary
C
      IF (TNORM.GT.DIFFDX*DIFFDX) THEN                                  15/1/91VM
         TNORM = SQRT(TNORM)
         DVDD = 0.0D0
         DO 40 I = 1, N3
            T = DIR(I)/TNORM
            DVDD = DVDD+T*DX(I)
            DIR(I) = T
   40    CONTINUE
         DVDD = DVDD*DXMAG
C
C  Find approx minimum along that direction; first take step of length
C     DELT2X along DIR
C
         IF (DVDD.LT.0.0D0) THEN
            STEPX = DELT2X
         ELSE
            STEPX = -DELT2X
         ENDIF
         DO 50 I = 1, N3
            X(I) = Q1(I)+STEPX*DIR(I)
   50    CONTINUE
C         CALL INTFNC                                                   0213BL05
         CALL EHOOK(1,iproc)                                            0213BL05

         NFUNC = NFUNC+1
C
C  Parabolic fit to V at Q1 and Q2 and G at Q1 to approx minimum in V
C     along DIR
C
         T = STEPX*DVDD
         DIRMIN = -0.5D0*T*STEPX/(V-V1-T)
C
C  Check if length of STEP is acceptable
C
         IF (ABS(DIRMIN-STEPX).GT.2.0D0*DELT2X) THEN
C
C  Step is too large, reject Euler step
C
            VMIN = V1-0.25D0*T*T/(V-V1-T)
C           WRITE (FU6,1000) S,DELX,DELT2X,DIFFDX,DIRMIN,V1,VMIN          15/1/91VM
            WRITE (FU6,1000) S/GUFAC6,DELX/GUFAC6,DELT2X/GUFAC6,
     *      DIFFDX/GUFAC6,DIRMIN,V1,VMIN                                0405JZ07
            
            IF (DELX.GT.DELT2X) THEN
C
C  Halve Euler stepsize and take new Euler step from 'OLD' coord
C
               write(6,*) 'HALVING EULER '

               DELX = 0.5D0*DELX
               DO 60 I = 1, N3
                  X(I) = XOLD(I)-DELX*DXOLD(I)
   60          CONTINUE
               CALL INTFNC
               NFUNC = NFUNC+1
               GO TO 10
            ENDIF
C
C  Stabilization never worked so just take last Euler step reset coord,
C     norm'd grad, and v
C
            DO 70 I = 1, N3
               X(I) = Q1(I)
               DX(I) = G1(I)
   70       CONTINUE
            V = V1
C
C  Update S
C
            S = S+DELX*FISEN
         ELSE
C
C  Stabil worked, step to new coord and get norm'd grad and potential
C
            DELS = 0.0D0
            DO 80 I = 1, N3
               X(I) = Q1(I)+DIRMIN*DIR(I)
               T = X(I)-XOLD(I)
               DELS = DELS+T*T
   80       CONTINUE
            DELS = SQRT(DELS)
            CALL INTFNC
            DXMAGO = DXMAG                                              1/3/91VM
            NFUNC = NFUNC+1
C
C  Update s
C
            S = S+DELS*FISEN
         ENDIF
C
      ELSE                                                              3/2/91VM
C
         S = S+FISEN*DELX                                               3/2/91VM
C
      ENDIF
      RETURN
C
 1000 FORMAT (' IN IRCX, STEP REJECTED, NEW EULER STEP TAKEN FOR HALF',
     *   ' THE STEPSIZE',/,' S,DEL,DELTA2,DIFFD,DIRMIN,V1,VMIN=',       151/91VM
     *   1P,7E13.5)
C
      END SUBROUTINE ircx
C
C***********************************************************************
C IVTST0
C***********************************************************************
C
      SUBROUTINE ivtst0
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : gufac6,iunit6
C
C     Called by: ZOCUPD
C
C     This is a subroutine designed for doing zero-order IVTST.
C     Written by Wei-Ping Hu, 07/21/93
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   Set up the grid on the reaction path
C
C     Determine the number of points on the reaction path
C
      LSAVE = NINT((SLP-SLM)/(DEL*DBLE(INH)))+1                         1105PF97
C
C     Give s values to these points
C   
      DO 10 ISS = 1, LSAVE 
         SSUBI(ISS) = SLM + (ISS-1) * DEL*DBLE(INH)                     1105PF97
10    CONTINUE
      SSUBI(LSAVE) = SLP
C
C     Determine the position index for saddle point     
C
      NSHLF = NINT(-SLM/(DEL*DBLE(INH))) + 1                            1105PF97
C
C     Update the saddle point information (barrier and frequencies)
C
      VCLAS(NSHLF) = BARRA
      ISHFT = N3 - NF(5)
      DO 20 IMM = 1, NF(5)
         WETS(IMM,NSHLF) = WESADA(IMM)
         FREQ(IMM+ISHFT) = WETS(IMM,NSHLF)
20    CONTINUE
C
C     Determine the adiabatic energy of the saddle point
C
      V = BARRA
      CALL ZEROPT(2)
C     
C     Update the moments of inertia
C      
      FMOM(1) = FMIR1A
      FMOM(2) = FMIR2A
      FMOM(3) = FMIP1A  
      FMOM(4) = FMIP2A
      FMOM(5) = FMISPA
      FMITS(NSHLF) = FMISPA
      DO 25 I=1,5
         WRITE(FU6,1000) I, FMOM(I)
25    CONTINUE 
C
C     Update the reduced moment of inertia
C
      IC = 0
      DO 30 IRE = 1, 4
         NFREQ = NF(IRE)
         DO 30 IMM = 1, NFREQ
            IC = IC + 1
            IF (MODER(IRE,IMM) .EQ. 9) FMIHR(IC) = HRMIR(IC)
30    CONTINUE
      DO 40 IMM = 1, NF(5)
         IF (MODETS(1,IMM) .EQ. 9) THEN
            FMIHTS(IMM,NSHLF) = HRMITS(IMM)
            WRITE(FU6,2000) NF(5) - IMM + 1, FMIHTS(IMM,NSHLF)
         ENDIF
40    CONTINUE
C
C     Interpolate VMEP
C
      AV = EPRD
      BV = 2 * BARRA - AV + 2 * (BARRA * (BARRA - AV)) ** 0.5
      RANGE = 2 * BARRA * (BARRA - AV) / REDM / ABS(TSWIM) ** 2 / BV 
      RANGE = RANGE ** 0.5
      S0V = -RANGE * LOG((AV + BV) / (BV - AV))
      CV = 0.0 
C     WRITE(FU6,3000) AV*CKCAL,BV*CKCAL,CV*CKCAL,S0V,REDM*CAU,
C    *                ABS(TSWIM)*AUTOCM,RANGE 
      IF (IUNIT6.EQ.1)
     *WRITE(FU6,3000) AV*CKCAL,BV*CKCAL,CV*CKCAL,S0V/GUFAC6,REDM*CAU,
     *                ABS(TSWIM)*AUTOCM,RANGE/GUFAC6 
      IF (IUNIT6.EQ.0)
     *WRITE(FU6,3002) AV*CKCAL,BV*CKCAL,CV*CKCAL,S0V/GUFAC6,REDM*CAU,
     *                ABS(TSWIM)*AUTOCM,RANGE/GUFAC6
      DO 50 ISS = 1, LSAVE
         SMEP = SSUBI(ISS)
         VCLAS(ISS) = ECKART(AV,BV,CV,S0V,RANGE,SMEP)
50    CONTINUE
C
C     Interpolate VaG
C
      AV = VAP - VAR
      DVAG = VAD - VAR
      BV = 2 * DVAG - AV + 2 * (DVAG * (DVAG - AV)) ** 0.5
      CV = VAR
      S0V = -RANGE * LOG((AV + BV) / (BV - AV))
C     WRITE(FU6,3100) AV*CKCAL,BV*CKCAL,CV*CKCAL,S0V 
      IF (IUNIT6.EQ.1) 
     *  WRITE(FU6,3100) AV*CKCAL,BV*CKCAL,CV*CKCAL,S0V/GUFAC6           0405JZ07
      IF (IUNIT6.EQ.0)
     *  WRITE(FU6,3102) AV*CKCAL,BV*CKCAL,CV*CKCAL,S0V/GUFAC6           0405JZ07
      DO 60 ISS = 1, LSAVE
         SMEP = SSUBI(ISS)
         VADIB(ISS) = ECKART(AV,BV,CV,S0V,RANGE,SMEP)
60    CONTINUE
C
      RETURN
1000  FORMAT(/6X,'For IOP = ',I1,' I = ',1P,E10.4,' a.u.')
2000  FORMAT(/6X,'For the saddle point,',/6X,
     * 'the reduced moment of inertia of mode ',I3,' = ',
     *  1P,E10.4,' a.u.')
3000  FORMAT(/6X,'In the VMEP(s) Eckart function,'
     *       /6X,'A  = ',F12.4,' (kcal/mol)',
     *       /6X,'B  = ',F12.4,' (kcal/mol)',
     *       /6X,'C  = ',F12.4,' (kcal/mol)',
     *       /6X,'S0 = ',F12.4,' (bohr)',
     *       /6X,'u  = ',F12.6,' (a.m.u)',
     *       /6X,'w  = ',F12.4,'i cm**-1',
     *       /6X,'L  = ',F12.6,' (bohr)')
3002  FORMAT(/6X,'In the VMEP(s) Eckart function,'
     *       /6X,'A  = ',F12.4,' (kcal/mol)',
     *       /6X,'B  = ',F12.4,' (kcal/mol)',
     *       /6X,'C  = ',F12.4,' (kcal/mol)',
     *       /6X,'S0 = ',F12.4,' (angstrom)',
     *       /6X,'u  = ',F12.6,' (a.m.u)',
     *       /6X,'w  = ',F12.4,'i cm**-1',
     *       /6X,'L  = ',F12.6,' (angstrom)')                           0405JZ07
3100  FORMAT(/6X,'In the Va^G(s) Eckart function',                      06/96ELC
     *       /6X,'A  = ',F12.4,' (kcal/mol)',
     *       /6X,'B  = ',F12.4,' (kcal/mol)',
     *       /6X,'C  = ',F12.4,' (kcal/mol)',
     *       /6X,'S0 = ',F12.4,' (bohr)')
3102  FORMAT(/6X,'In the Va^G(s) Eckart function',                      0405JZ07
     *       /6X,'A  = ',F12.4,' (kcal/mol)',
     *       /6X,'B  = ',F12.4,' (kcal/mol)',
     *       /6X,'C  = ',F12.4,' (kcal/mol)',
     *       /6X,'S0 = ',F12.4,' (angstrom)')
C
      END SUBROUTINE ivtst0
C
C***********************************************************************
C  ISWAP
C***********************************************************************
C
      SUBROUTINE iswap(IX,IY)
C
C     This subroutine swap twp integers
C
C     Called by :
C                    CUSSPL , ICFDRP
C
      IMPLICIT NONE
      INTEGER IX,IY
      INTEGER ITEMP

      ITEMP = IX
      IX = IY
      IY = ITEMP
      RETURN
      END subroutine iswap
C
C***********************************************************************
C  KAPVA
C***********************************************************************
C
      SUBROUTINE kapva
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface, only : gufac6,iunit6
      use potmod, only : rtelct
      use kintcm, only : ilct
C
C     CALCULATES TUNNELING CORRECTIONS FOR SET OF T'S
C     AT PRESENT, DOES ZCT (MEPSAG), SCT (CD-SCSAG),                    0423TA02
C     LCG3, AND LCG4                                                    0708JC00
C     SECOND METHOD DONE BY EFFECTIVE MASS CALCULATION
C
C     CALLED BY:
C                RATE
C     CALLS:
C            KG1,VSPLIN,PSAG,FIVPT,BOLTZ,LCG34,QRSENE                   0708JC00
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/29/91
C   MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C   FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C   SCSAG is removed in version 5.0 10/16/92
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save                                                              0601YC98
C
      DIMENSION ESV(NSV),TRNPT1(NSV),TRNPT2(NSV),PED(2,NSV)             7/14YL92
      DIMENSION PE(2,NSV),SUM(4),STP(6),PLCG(2,NSV)                     1016WH92
      DIMENSION PMOMT(2,NSV)                                            1106YL92
      DIMENSION TMPS(3),TMPV(3),EPREP(4,40)                             0609WH94
      DIMENSION DEDNRC(0:MAXWKB), ENRC(0:MAXWKB)                        0620WH94
      CHARACTER*2 ICHAR(6)
      CHARACTER*1 CDOM(3,NSV)
C
C     CHARACTER*5 ATYP,APACD                                            0117GL92
      LOGICAL LNEGCD                                                    1016WH92
      character * 4 swlct(2)                                            0708JC00
      data swlct/'LCG3','LCG4'/                                         0708JC00
C
C     I do not find any use anymore so I commented the following two lines 
C     the line above defining ATYP and APACD as characters.             0423TA02
C     ATYP = 'MEP  '                                                    0430GL92
C     APACD = 'CD-SC'                                                   0117GL92
C
C     SET UP N AND 2N+1 QUADRATURES FOR DE AND DS INTEGRALS
C
C     Restore the original NQ12 and NQ22 values
C     The NQ12 and NQ22 value are changed in this subroutine
C     but we need the original values to do ZOC correction
C     so we store those value into NQQ1,NQQ2 in read5 and use them here
C     the way we programmed this subroutine looks ugly, which should be
C     fixed sometime later!
C
      NQ12 = NQQ1
      NQ22 = NQQ2
C
      NQ1 = NQ12
      NQ2 = NQ22
      CALL KG1 (NQ1,PT,WT)
      CALL KG1 (NQ2,PT2,WT2)
      NQ12 = 2*NQ1+1
      NQ22 = 2*NQ2+1
      IF (ilct.gt.0) THEN                                               0423TA02
         WRITE (FU6,1000) swlct(ilct)                                   0708JC00
      ELSE                                                              0423TA02
         WRITE (FU6,900)                                                0423TA02
      ENDIF                                                             0423TA02
      WRITE (FU6,2000)                                                  1026RS94
      IF (.NOT. LCDSC) WRITE (FU6, 1002)                                2608GL91
      WRITE (FU6,1001)NQ12,NQ22,NSEG,NSEG2
C     CLOSE (6)
C     STOP 'kapva 1'
C
      IF (LGS2(12) .LE. 1) THEN
         EZ = MAX(VAR,VAP)
!        write(6,*) 'EZ =',EZ*ckcal
!        EZ = PEMIN
      ELSE                                                              0609WH94
C
C     Looking for the bottom of the VaG for quantized tunneling calculations
C
         VAGMIN = VADIB(1)                                              0609WH94
         SVGMIN = SSUBI(1)
         ISMIN = 1                                                      0609WH94
         DO 5 IS = 2,NSHLF                                              0609WH94
            IF (VADIB(IS) .LT. VAGMIN) THEN                             0609WH94
               VAGMIN = VADIB(IS)                                       0609WH94
               ISMIN = IS                                               0609WH94
               SVGMIN = SSUBI(ISMIN)                                    0609WH94
            ENDIF                                                       0609WH94
5        CONTINUE                                                       0609WH94
         IF (ISMIN .GT. 1) THEN                                         0609WH94
            SVGMIN = VADIB(ISMIN)                                       0609WH94
            TMPS(1) = SSUBI(ISMIN-1)                                    0609WH94
            TMPS(2) = SSUBI(ISMIN)                                      0609WH94
            TMPS(3) = SSUBI(ISMIN+1)                                    0609WH94
            TMPV(1) = VADIB(ISMIN-1)                                    0609WH94
            TMPV(2) = VADIB(ISMIN)                                      0609WH94
            TMPV(3) = VADIB(ISMIN+1)                                    0609WH94
            CALL TREPT(1,TMPS,TMPV,SVGMIN,VAGMIN)                       0609WH94
            WRITE(FU6,1050) VAGMIN * CKCAL, SVGMIN                      0609WH94
         ENDIF                                                          0609WH94
         EZ = VAGMIN                                                    0609WH94
      ENDIF                                                             0609WH94
C
1050  FORMAT(/2X,'The lower limit for calculating tunneling is ',F10.4, 0609WH94
     *          ' kcal/mol.',
     *       /2X,'The s value at this energy is ',F10.5,' bohr.',/)
C
      WRITE (FU6,1100)
C
C     GENERATE SPLINE FITS FOR VA AND CURVATURES
C
      CALL VSPLIN (LGS,LSAVE,SSUBI,SMAX,VADIB,VAD,IMAX,IMXINT)          8/26YL91
      SCVT(NTEMP+1) = SMAX
C
C     USING SPLINE FIT, COMPUTE KAPPA CAG AND WIGNER KAPPA
C
      VDIFF1 = VADIB(NSHLF)-VAD
      DO 10 IX = 1, NTEMP
         KAPCAG(IX) = EXP(BETA(IX)*VDIFF1)
         KAPW(IX) = 1.0D0+(WSTAR*BETA(IX))**2/24.0D0
   10 CONTINUE
      VM = VAD-EZ
      IF (VM.GT.0.0D0) GO TO 30
      DO 20 IT = 1, NTEMP
         KAPSAG(IT) = 1.0D0
         KAPLCG(IT) = 1.0D0                                             5/10DL90
         KACDSC(IT) = 1.0D0                                             8/26YL91
         KACOMT(IT) = 1.0D0                                             1106YL92
         KAMOMT(IT) = 1.0D0                                             1106YL92
   20 CONTINUE
      WRITE (FU6,1800)
      GO TO 80
   30 CONTINUE
C
      LNEGCD = .FALSE.                                                  8/26YL91
C
C     PE(1,N) AND PE(2,N) REFER TO DIFFERENT QUADRATURES
C
      IF (LGS(28).EQ.0) THEN
         N = NQ12/2
         PEMIN = 0.5D0*(1.0D0+PT(N))*VM+EZ
      ELSE
         IF (PEMIN.GT.VAD) THEN
            PEMIN = VAD
            WRITE (FU6,1900)
         ENDIF
      ENDIF
      IF (LCDSC) THEN                                                   8/21YL91
         IF (LGS(5).NE.0) WRITE (FU6,1150)                                    ..
C        WRITE (FU6,1200)                                                     ..
         IF(IUNIT6.EQ.1) WRITE (FU6,1200)                               0405JZ07
         IF(IUNIT6.EQ.0) WRITE (FU6,1210)                               0405JZ07
      ENDIF                                                             8/21YL91
      IE = 0
      DO 40 ISEG = 1, NSEG
         DO 40 N = 1, NQ12
            IE = IE+1
            E = EZ+0.5D0*(2.0D0*DBLE(ISEG)-1.0D0+PT(N))*VM/DBLE(NSEG)
            ESV(IE) = E
            CALL PSAG (E,PE(1,IE),PED(1,IE),TRNPT1(IE),
     *                 TRNPT2(IE),NTP,STP,SSUBI,ICHAR,IMAX,VAD,
     *                 SMAX,LNEGCD,IMXINT)                              1016WH92
C*
            E = E*CKCAL
            NM = MIN(NTP,6)
C
C    CDSCSAG probability
C
            IF (LCDSC) THEN                                             8/21YL91
C              WRITE (FU6,1400) E,PE(2,IE),PED(2,IE),   
C    *                         NTP,(STP(J),ICHAR(J),J=1,NM)                ..
C           ELSE                                                           ..
C              WRITE (FU6,1300) E,PE(2,IE),NTP,(STP(J),                    ..
C    *                         ICHAR(J),J= 1,NM)                           ..
               WRITE (FU6,1400) E,PE(2,IE),PED(2,IE),   
     *               NTP,(STP(J)/GUFAC6,ICHAR(J),J=1,NM)                0405JZ07
            ELSE                                                           ..
               WRITE (FU6,1300) E,PE(2,IE),NTP,(STP(J)/GUFAC6,             ..
     *                         ICHAR(J),J= 1,NM)                        0405JZ07
            ENDIF                                                       8/21YL91
   40 CONTINUE
C
C    SCSAG has been removed from version 5.0
C
      NEMAX = IE                                                        6/14DL91
C VAD is stored because changes during the LCG34 calculations           0708JC00
      VMAX = VAD     
C
C*
C      INTRODUCE THE CALL TO LCG34                                      0708JC00
       IF (LLCG) THEN                                                   2709GL91
C         IF (.NOT. LLCGG) THEN                                         2709L91
             IF (VAR .GE. VAP) THEN                                     0719WH94
                IEXOG = 0                                               6/14DL91
                WRITE (FU6,1020)                                        6/14DL91
             ELSE                                                       6/14DL91
                IEXOG = -1                                              6/14DL91
                WRITE (FU6,1030)                                        6/14DL91
             ENDIF                                                      6/14DL91
C         ENDIF                                                         6/14DL91
          CALL LCG34 (ESV,TRNPT1,TRNPT2,PLCG,NEMAX)                     Jun03AFR
          CALL PBTSRT (NSV,2,PED,PLCG,PMOMT)                            1106YL92
       END IF                                                           6/14DL91
C*
      VAD = VMAX      
      IF (LNEGCD) WRITE (FU6,1700)                                      8/26YL91
C
C
C     Print all the tunneling probabilities
C
      IF (LLCG) THEN
         WRITE(FU6,1720) swlct(ilct)                                    0708JC00
         DO 44 I = 1,NEMAX
            CDOM(1,I) = ' '
            CDOM(2,I) = ' '
            CDOM(3,I) = ' ' 
            IF (PED(2,I) .GT. PLCG(2,I)) THEN 
               CDOM(2,I) = '*'
            ELSE
               CDOM(3,I) = '*'
            ENDIF
            WRITE(FU6,1730) I,ESV(I)*CKCAL,PE(2,I),CDOM(1,I),
     *              PED(2,I),CDOM(2,I),PLCG(2,I),CDOM(3,I),PMOMT(2,I)
44       CONTINUE
      ENDIF
C      
      DO 45 I = 1,NTEMP
         DO 45 J = 1,4
            EPREP(J,I) = 0.0D0
45    CONTINUE
C
C     Calculate the quantized reactant energy states
C
      IF (LGS2(12) .GE. 1) THEN                                         0719WH94
         CALL QRSENE(VM,NB,ENE0,ENRC,DEDNRC)                            0620WH94
C        NBOUND = NB                                                    0719WH94
C        The NBOUND value is defined now in qrsene subroutine.          0522TA02
         DO 47 I = 0, NBOUND                                            0719WH94
            ENLVRC(I) = ENRC(I) + ENE0                                  0719WH94
47       CONTINUE                                                       0719WH94
      ENDIF                                                             0719WH94
C
C     BOLTZMANN AVERAGE
C
      WRITE(FU6,2600)                                                   0522TA02
      IF (LGS2(12) .EQ. 0) WRITE(FU6,1601) NQ2,NQ22,NQ2,NQ22,           0522TA02
     *                                     NQ1,NQ1,NQ12,NQ12            0522TA02
      WRITE(FU6,1602)                                                   09/96ELC
      DO 50 IT = 1, NTEMP
         BET = BETA(IT)
C
C     RESULTS FOR VARIOUS QUADRATURE COMBOS IN SUM(4)
C
         CALL BOLTZ (BET,VM,PE,SUM,SUMNOT,ESV,EMAX,VAD,IT,1,            0625TA02
     *               NB,ENE0,ENRC,DEDNRC)                               0620WH94
         KAPSAG(IT) = SUM(4)
         KNTZCT(IT) = SUMNOT                                            0625TA02
         EPREP(1,IT) = EMAX
         IF (LGS2(12) .NE. 0 .AND. IT .EQ. 1)                           0522TA02
     *           WRITE(FU6,2601) NQ2,NQ22,NQ2,NQ22,NQ1,NQ1,NQ12,NQ12    0522TA02
         WRITE (FU6,1603) TEMP(IT),SUM                                  09/96ELC
   50 CONTINUE
C
C  CDSCSAG
C
      IF (LCDSC) THEN                                                   8/21YL91
      WRITE(FU6,1604)                                                   09/96ELC
C        WRITE (FU6,1501) NSEG,NQ1,NSEG,NQ1,NSEG,NQ12,NSEG,NQ12         1216WH92
         DO 65 IT = 1, NTEMP                                            0430GL92
            BET = BETA(IT)                                                  ..
            CALL BOLTZ (BET,VM,PED,SUM,SUMNOT,ESV,EMAX,VAD,IT,2,        0625TA02
     *                  NB,ENE0,ENRC,DEDNRC)                            0620WH94
            KACDSC(IT) = SUM(4)                                             ..
            KNTSCT(IT) = SUMNOT                                         0625TA02
            EPREP(2,IT) = EMAX
            IF (LGS2(12) .NE. 0 .AND. IT .EQ. 1)                        0522TA02
     *           WRITE(FU6,2601) NQ2,NQ22,NQ2,NQ22,NQ1,NQ1,NQ12,NQ12    0522TA02
C           WRITE (FU6,1600) TEMP(IT),KAPCAG(IT),SUM,EMAX,PER               ..
            WRITE (FU6,1603) TEMP(IT),SUM                               09/96ELC
   65    CONTINUE                                                           ..
      ELSE                                                                  ..
         DO 73 IT = 1, NTEMP                                                ..
            KACDSC(IT) = 1.0D0                                              ..
   73    CONTINUE                                                           ..
      ENDIF                                                             8/21YL91
C
C   SCSAG has been removed from version 5.0
C
C   LCG3 or LCG4                                                        0708JC00
C
      IF (LLCG) THEN                                                    0927GL91
      WRITE(FU6,1605) swlct(ilct)                                       0708JC00
C       WRITE(FU6,1550) NGS0,NGS0,NGS0,NGS0,                            4/30GL92
C    *     NSEG,NQ1,NSEG,NQ1,NSEG,NQ12,NSEG,NQ12                        5/10DL90
        DO 72 IT = 1,NTEMP                                              5/10DL90
           BET = BETA (IT)                                              5/10DL90
C          JTUN=ICLT+2                                                  0708JC00
           JTUN=ILCT+2                                                  0423TA02
           CALL BOLTZ (BET,VM,PLCG,SUM,SUMNOT,ESV,EMAX,VAD,IT,JTUN,     0625TA02
     *                 NB,ENE0,ENRC,DEDNRC)                             0620WH94
           KAPLCG(IT) = SUM(4)                                          5/10DL90
           KNTLCT(IT) = SUMNOT                                          0625TA02
           EPREP(3,IT) = EMAX
           IF (LGS2(12) .NE. 0 .AND. IT .EQ. 1)                         0522TA02
     *           WRITE(FU6,2601) NQ2,NQ22,NQ2,NQ22,NQ1,NQ1,NQ12,NQ12    0522TA02
C          WRITE(FU6,1600) TEMP(IT),KAPCAG(IT),SUM,EMAX,PER             5/10DL90
           WRITE (FU6,1603) TEMP(IT),SUM                                09/96ELC
   72   CONTINUE                                                        5/10DL90
C       WRITE(FU6,1551) NGS0,NGS0,NGS0,NGS0,                            1106YL92
C    *     NSEG,NQ1,NSEG,NQ1,NSEG,NQ12,NSEG,NQ12                        1106YL92
      WRITE(FU6,1606)                                                   09/96ELC
        DO 74 IT = 1,NTEMP                                              1106YL92
           BET = BETA (IT)                                              1106YL92
           CALL BOLTZ (BET,VM,PMOMT,SUM,SUMNOT,ESV,EMAX,VAD,IT,5,       0625TA02
     *                 NB,ENE0,ENRC,DEDNRC)                             0620WH94
           KAMOMT(IT) = SUM(4)                                          1106YL92
           KNTMOT(IT) = SUMNOT                                          0625TA02
           EPREP(4,IT) = EMAX
           IF (LGS2(12) .NE. 0 .AND. IT .EQ. 1)                         0522TA02
     *           WRITE(FU6,2601) NQ2,NQ22,NQ2,NQ22,NQ1,NQ1,NQ12,NQ12    0522TA02
           WRITE (FU6,1603) TEMP(IT),SUM                                09/96ELC
   74   CONTINUE                                                        1106YL92
        CALL PBTSRT(40,1,KACDSC,KAPLCG,KACOMT)                          1106YL92
      ELSE                                                              5/10DL90
        DO 75 IT = 1,NTEMP                                              5/10DL90
           KAPLCG(IT) = 1.0D0                                           5/10DL90
   75   CONTINUE                                                        5/10DL90
      END IF                                                            5/10DL90
C
C     Write representative tunneling energy
C
      IF (ilct.gt.0) THEN                                               0423TA02
         WRITE (FU6,1500) swlct(ilct)                                   0708JC00
         DO 85 IT = 1,NTEMP                                             0616WH94
            WRITE(FU6,1600) TEMP(IT),KAPCAG(IT),(EPREP(J,IT),J=1,4)     0616WH94
   85    CONTINUE                                                       0616WH94
      ELSE
         WRITE (FU6,1510)                                               0423TA02
         DO 86 IT = 1,NTEMP                                             0423TA02
            WRITE(FU6,1610) TEMP(IT),KAPCAG(IT),(EPREP(J,IT),J=1,2)     0423TA02
   86    CONTINUE                                                       0423TA02
      ENDIF
C           
   80 CONTINUE
  

c --- save representative tunneling energy for LCT tunneling path plot  0408PJ01
      do it = 1, ntemp                                                  0408PJ01
         rtelct(it) = eprep(3,it)                                       0408PJ01
      end do                                                            0408PJ01

      RETURN
C
  900 FORMAT(//1X,7(1H*),' Vibrationally adiabatic ground-state',       0423TA02
     *  ' transmission coefficients ',7(1H*),
     *//2X,'     +/CAG',
     * /2X,'kappa        - the classical transmission coefficient',
     *                   ' for correcting',
     * /2X,'               the classical threshold of conventional TST'
     *,//2X,'     ZCT',
     * /2X,'kappa        - semiclassical transmission coefficient for',
     * /2X,'               tunneling along MEP with zero curvature',
     *//2X,'     SCT',
     * /2X,'kappa        - semiclassical transmission coefficient for',
     * /2X,'               tunneling along MCP, in small curvature ',
     *                    'limit.')
 1000 FORMAT(//1X,7(1H*),' Vibrationally adiabatic ground-state',       1207WH92
     *  ' transmission coefficients ',7(1H*),
     *//2X,'     +/CAG',
     * /2X,'kappa        - the classical transmission coefficient',
     *                   ' for correcting',
     * /2X,'               the classical threshold of conventional TST'
C     *,//2X,'     MEPSAG',
     *,//2X,'     ZCT',                                                 0423TA02
     * /2X,'kappa        - semiclassical transmission coefficient for',
     * /2X,'               tunneling along MEP with zero curvature',
C     *//2X,'     CD-SCSAG',
     *//2X,'     SCT',                                                  0423TA02
     * /2X,'kappa        - semiclassical transmission coefficient for',
     * /2X,'               tunneling along MCP, in small curvature ',
     *                    'limit',  
     *//2X,'     ',a4,                                                  0708JC00
     * /2X,'kappa        - semiclassical transmission coefficient for',
     *                   ' tunneling with',
     * /2X,'               large curvature ground-state approximation.')
2000  FORMAT(//2X,'All tunneling corrections given below are ',         1026RS94
     *   'normalized using',
     * /2X,'the probability at the maximum of the ground-state',
     *     ' adiabatic curve.',
     *//2X,'To obtain the proper SAG kappa for conventional TST or CVT,'
     *,/2X,'multiply the kappa mentioned above by the corresponding',
     * /2X,'classical transmission coefficient.')
 1002 FORMAT(' ******** NOTE ********',                                 8/26YL91
     * /,' Centrifugal-dominant small-curvature tunneling is supported',8/26YL91
     * /,' only when vibrational normal modes are treated harmonically',8/26YL91
     * /,' **********************')                                     8/26YL91
 1001 FORMAT(/2X,'Number of Kronrod quadrature points used in the ',    09/96ELC
     *           'Boltzmann average = ',I3,
     *      //2X,'Number of Kronrod quadrature points used in ',        09/96ELC
     *      ' evaluating',/2X,'the theta integral for MEP and',         09/96ELC
C    *      ' CD-SCSAG =',I3,                                           09/96ELC
     *      ' SCT =',I3,                                                0423TA02
     *      //2X,'Number of equal segments for Boltzmann average =',I3, 09/96ELC
     *      //2X,'Number of equal segments for theta integral =',I3,    09/96ELC
     *      //2X,'NTP  = number of turning points') 
 1020 FORMAT(/1X,5(1H*),' The exoergic direction is the forward ',      6/14DL91
     * 'reaction',//)                                                   6/14DL91
 1030 FORMAT(/1X,5(1H*),' The exoergic direction is the reverse ',      6/14DL91
     * 'reaction',//)                                                   6/14DL91
 1100 FORMAT(/1X,10(1H*),1X,'Semiclassical transmission probabilities',
     *       ' and coefficients',
     * //3X,'NOTE: ** after a turning point implies that',
     *  /3X,'      E is less than V at that s value.') 
 1150 FORMAT(/, 5X,'**************************************************',9/21YL91
     * /, 5X,'Although anharmonicity is included in calculating',       9/21YL91
C    * /, 5X,'partition functions and the adiabatic potential, it',     9/21YL91
C    * /, 5X,'is included in CD-SCSAG only through the adiabatic',      9/21YL91
     * /, 5X,'partition functions and the adiabatic potential, it is ', 0423TA02
     * /, 5X,'included in SCT (CD-SCSAG) only through the adiabatic',   0423TA02
     * /, 5X,'ground state potential for the turning points in the',    9/21YL91
C    * /, 5X,'CD-SCSAG method are evaluated harmonically.',             9/21YL91
     * /, 5X,'SCT (CD-SCSAG) method are evaluated harmonically.',       0423TA02
     * /, 5X,'**************************************************')      9/21YL91
 1200 FORMAT(//1X,78('-'),/26X,'Transmission probabilities',/1X,78('-'),0621WH94
C    * /1X,' E(kcal)',8X,'MEPSAG',7X,'CD-SCSAG',4X,
     * /1X,' E(kcal)',8X,' ZCT  ',7X,'  SCT   ',4X,                     0423TA02
     * 'NTP ','  turning points (bohr)',/)
 1210 FORMAT(//1X,78('-'),/26X,'Transmission probabilities',/1X,78('-'),0405JZ07
     * /1X,' E(kcal)',8X,' ZCT  ',7X,'  SCT   ',4X,                     
     * 'NTP ','  turning points (angstrom)',/)
 1300 FORMAT(1X,F8.4,2X,1P,E14.4,14X,2X,I5,2(0PF9.3,A2),:,
     *                          /46X,2(0PF9.3,A2),:,
     *                          /46X,2(0PF9.3,A2))
 1400 FORMAT(1X,F8.4,2X,1P,2E14.4,I5,2X,2(0PF9.3,A2),:,
     *                          /46X,2(0PF9.3,A2),:,
     *                          /46X,2(0PF9.3,A2))
 1500 FORMAT(/21X,11X,'Representative Tunneling Energy',                06/96ELC
     *' (kcal/mol)  ' /10X,' kappa^+/CAG(T) ',3X,47(1H-),               06/96ELC
C    * /6X,'T(K)',19X,'    MEPSAG  ','  CD-SCSAG   ','     ',a4,'   ',  0708JC00
     * /6X,'T(K)',19X,'      ZCT   ','      SCT    ','     ',a4,'   ',  0423TA02
     *               '    muOMT   ',/)
 1510 FORMAT(/21X,11X,'Representative Tunneling Energy',
     *' (kcal/mol)  ' /10X,' kappa^+/CAG(T) ',3X,47(1H-),
     *  /6X,'T(K)',32X,'ZCT',16X,'SCT',/)
C1501 FORMAT(/ T18,5H+/CAG,T53,'kappa^CD-SCSAG (T)',/,                  06/96ELC
 1501 FORMAT(/ T18,5H+/CAG,T53,'kappa^SCT (T)',/,                       0423TA02
     1       T15,11Hkap     (T) ,T38,50(1H-),/
     2       T6,4HT(K),T32,4(6X,2HM=,I2,'*',I2),13X,'Erep')
 1550 FORMAT(/T18,5H+/CAG,T57,'kappaLCG (T)',/,                         0708JC00
     1       T15,11Hkap     (T) ,T38,50(1H-)/T38,2('Gaussian',5X,
     2       'Trapezoidal',2X),/T38,4(5HNGS0=,I4,4X),7X,   
     3       'Erep',/T6,4HT(K),T32,4(6X,2HM=,I2,'*',I2))                     
 1551 FORMAT(/T18,5H+/CAG,T57,'kappamuOMT (T)',/,                       1106YL92
     1       T15,11Hkap     (T) ,T38,50(1H-)/T38,2('Gaussian',5X,       1106YL92
     2       'Trapezoidal',2X),/T38,4(5HNGS0=,I4,4X),7X,                1106YL92
     3       'Erep',/T6,4HT(K),T32,4(6X,2HM=,I2,'*',I2))                1106YL92
 2600 FORMAT(//1x,78('-'),/26X,'Transmission coefficients',/1x,78('-'), 09/96ELC
     * /,2x,'M = no. of points per segment for imaginary action integ.',09/96ELC
     * /,2x,'N = no. of points per segment for Boltzmann average')      0522TA02
 1601 FORMAT(/,T16,4(8x,2HM=,I3),/,2x,4HT(K),T16,4(8x,2HN= ,I3))        0522TA02
 2601 FORMAT(/,T16,4(8x,2HM=,I3),/,2x,4HT(K),T16,4(8x,2HN= ,I3),/)      0522TA02
C1602 FORMAT(/,40X,' MEPSAG ',/)                                        09/96ELC
 1602 FORMAT(/,38X,'ZCT',/)                                             0423TA02
 1603 FORMAT(2X,F6.1,10X,4(1PE13.4))                                    09/96ELC
C1604 FORMAT(/,40X,'CD-SCSAG',/)                                        09/96ELC
 1604 FORMAT(/,38X,'SCT',/)                                             0423TA02
 1605 FORMAT(/,37X,a4,/)                                                0708JC00
 1606 FORMAT(/,37X,'mOMT',/)                                            09/96ELC
 1600 FORMAT(2X,F9.2,2X,F12.4,3X,4F12.4)                                0616WH94
 1610 FORMAT(2X,F9.2,2X,F12.4,3X,2F19.4)                                0423TA02
C1700 FORMAT(//1X,'For the CD-SCSAG method the curvature correction to',
 1700 FORMAT(//1X,'For the SCT method the curvature correction to',     0423TA02
     */1X,'the mass was negative and set to zero for the calculation',/)
 1720 FORMAT(/1X,78('-'),                                               0617WH94
     *      /20X,'Table of Tunneling Probabilities',/1X,78('-'),
     *     //2X,'Note: The ''*'' marks the dominant tunneling ',
     *          'mechanism at that energy.',/,
C    */1X,' IE','    E(kcal/mol)','   MEPSAG   ',3X,'  CD-SCSAG  ',3X,
     */1X,' IE','    E(kcal/mol)','     ZCT    ',3X,'     SCT    ',3X,  0423TA02
     *                           '     ',a4,'   ',3X,'    muOMT',/)     0708JC00
 1730 FORMAT(1X,I3,1X,F12.4,2X,1P,3(E12.4,A1,2X),E12.4)                 0617WH94
 1800 FORMAT(/1X,39HNo barrier therefore kappas set to one. /)
 1900 FORMAT (/1X, 'PEMIN reset to the maximum of the adiabatic',
     *   ' barrier')
C
      END subroutine kapva
C
C***********************************************************************
C  KG1
C***********************************************************************
C
      SUBROUTINE kg1 (N,PT,WT)
      use perconparam
C
C       NODES AND WEIGHTS FOR KRONROD QUADRATURE
C
C     CALLED BY:
C                KAPVA
C
C     INCLUDE FILE ADDED 9/10/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION PT(81), WT(81,2)
      M = 2*N+1
C
      IF (N.EQ.2) THEN
C
C         (N,2N+1)=    2    5
C
         PT(1) = -9.25820099772551D-01
         PT(2) = -5.77350269189619D-01
         PT(3) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 1.00000000000000D0
         WT(3,1) = 0.0D0
         WT(1,2) = 1.97979797979798D-01
         WT(2,2) = 4.90909090909090D-01
         WT(3,2) = 6.22222222222224D-01
C
      ELSEIF (N.EQ.3) THEN
C
C         (N,2N+1)=    3    7
C
         PT(1) = -9.60491268708019D-01
         PT(2) = -7.74596669241468D-01
         PT(3) = -4.34243749346798D-01
         PT(4) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 5.55555555555539D-01
         WT(3,1) = 0.0D0
         WT(4,1) = 8.88888888888832D-01
         WT(1,2) = 1.04656226026467D-01
         WT(2,2) = 2.68488089868333D-01
         WT(3,2) = 4.01397414775962D-01
         WT(4,2) = 4.50916538658474D-01
C
      ELSEIF (N.EQ.4) THEN
C
C         (N,2N+1)=    4    9
C
         PT(1) = -9.76560250737570D-01
         PT(2) = -8.61136311594013D-01
         PT(3) = -6.40286217496310D-01
         PT(4) = -3.39981043584844D-01
         PT(5) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 3.47854845137421D-01
         WT(3,1) = 0.0D0
         WT(4,1) = 6.52145154862517D-01
         WT(5,1) = 0.0D0
         WT(1,2) = 6.29773736654728D-02
         WT(2,2) = 1.70053605335723D-01
         WT(3,2) = 2.66798340452285D-01
         WT(4,2) = 3.26949189601452D-01
         WT(5,2) = 3.46442981890137D-01
C
      ELSEIF (N.EQ.5) THEN
C
C         (N,2N+1)=    5   11
C
         PT(1) = -9.84085360094845D-01
         PT(2) = -9.06179845938627D-01
         PT(3) = -7.54166726570851D-01
         PT(4) = -5.38469310105661D-01
         PT(5) = -2.79630413161783D-01
         PT(6) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 2.36926885056196D-01
         WT(3,1) = 0.0D0
         WT(4,1) = 4.78628670499345D-01
         WT(5,1) = 0.0D0
         WT(6,1) = 5.68888888888814D-01
         WT(1,2) = 4.25820367510819D-02
         WT(2,2) = 1.15233316622473D-01
         WT(3,2) = 1.86800796556493D-01
         WT(4,2) = 2.41040339228648D-01
         WT(5,2) = 2.72849801912558D-01
         WT(6,2) = 2.82987417857491D-01
C
      ELSEIF (N.EQ.6) THEN
C
C         (N,2N+1)=    6   13
C
         PT(1) = -9.88703202612676D-01
         PT(2) = -9.32469514203099D-01
         PT(3) = -8.21373340865030D-01
         PT(4) = -6.61209386466240D-01
         PT(5) = -4.63118212475301D-01
         PT(6) = -2.38619186083182D-01
         PT(7) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 1.71324492379181D-01
         WT(3,1) = 0.0D0
         WT(4,1) = 3.60761573048112D-01
         WT(5,1) = 0.0D0
         WT(6,1) = 4.67913934572650D-01
         WT(7,1) = 0.0D0
         WT(1,2) = 3.03961541198198D-02
         WT(2,2) = 8.36944404469064D-02
         WT(3,2) = 1.37320604634447D-01
         WT(4,2) = 1.81071994323138D-01
         WT(5,2) = 2.13209652271962D-01
         WT(6,2) = 2.33770864116995D-01
         WT(7,2) = 2.41072580173465D-01
C
      ELSEIF (N.EQ.7) THEN
C
C         (N,2N+1)=    7   15
C
         PT(1) = -9.91455371120814D-01
         PT(2) = -9.49107912342697D-01
         PT(3) = -8.64864423359769D-01
         PT(4) = -7.41531185599360D-01
         PT(5) = -5.86087235467687D-01
         PT(6) = -4.05845151377365D-01
         PT(7) = -2.07784955007895D-01
         PT(8) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 1.29484966168870D-01
         WT(3,1) = 0.0D0
         WT(4,1) = 2.79705391489244D-01
         WT(5,1) = 0.0D0
         WT(6,1) = 3.81830050505098D-01
         WT(7,1) = 0.0D0
         WT(8,1) = 4.17959183673426D-01
         WT(1,2) = 2.29353220105292D-02
         WT(2,2) = 6.30920926299785D-02
         WT(3,2) = 1.04790010322250D-01
         WT(4,2) = 1.40653259715526D-01
         WT(5,2) = 1.69004726639268D-01
         WT(6,2) = 1.90350578064786D-01
         WT(7,2) = 2.04432940075299D-01
         WT(8,2) = 2.09482141084727D-01
C
      ELSEIF (N.EQ.9) THEN
C
C         (N,2N+1)=    9   19
C
         PT(1) = -9.94678160677338D-01
         PT(2) = -9.68160239507569D-01
         PT(3) = -9.14963507249681D-01
         PT(4) = -8.36031107326573D-01
         PT(5) = -7.34486765183931D-01
         PT(6) = -6.13371432700553D-01
         PT(7) = -4.75462479112458D-01
         PT(8) = -3.24253423403770D-01
         PT(9) = -1.64223563614982D-01
         PT(10) = 2.96604890223891D-14
         WT(1,1) = 0.0D0
         WT(2,1) = 8.12743883615985D-02
         WT(3,1) = 0.0D0
         WT(4,1) = 1.80648160694843D-01
         WT(5,1) = 0.0D0
         WT(6,1) = 2.60610696402914D-01
         WT(7,1) = 0.0D0
         WT(8,1) = 3.12347077039973D-01
         WT(9,1) = 0.0D0
         WT(10,1) = 3.30239355001209D-01
         WT(1,2) = 1.43047756438390D-02
         WT(2,2) = 3.96318951602612D-02
         WT(3,2) = 6.65181559402743D-02
         WT(4,2) = 9.07906816887265D-02
         WT(5,2) = 1.11789134684418D-01
         WT(6,2) = 1.30001406855341D-01
         WT(7,2) = 1.45239588384366D-01
         WT(8,2) = 1.56413527788484D-01
         WT(9,2) = 1.62862827440115D-01
         WT(10,2) = 1.64896012828350D-01
C
      ELSEIF (N.EQ.10) THEN
C
C         (N,2N+1)=   10   21
C
         PT(1) = -9.95657163025811D-01
         PT(2) = -9.73906528517077D-01
         PT(3) = -9.30157491355708D-01
         PT(4) = -8.65063366688911D-01
         PT(5) = -7.80817726586413D-01
         PT(6) = -6.79409568298979D-01
         PT(7) = -5.62757134668601D-01
         PT(8) = -4.33395394129214D-01
         PT(9) = -2.94392862701457D-01
         PT(10) = -1.48874338981601D-01
         PT(11) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 6.66713443087090D-02
         WT(3,1) = 0.0D0
         WT(4,1) = 1.49451349150563D-01
         WT(5,1) = 0.0D0
         WT(6,1) = 2.19086362515949D-01
         WT(7,1) = 0.0D0
         WT(8,1) = 2.69266719309986D-01
         WT(9,1) = 0.0D0
         WT(10,1) = 2.95524224714692D-01
         WT(11,1) = 0.0D0
         WT(1,2) = 1.16946388673718D-02
         WT(2,2) = 3.25581623079647D-02
         WT(3,2) = 5.47558965743520D-02
         WT(4,2) = 7.50396748109199D-02
         WT(5,2) = 9.31254545836975D-02
         WT(6,2) = 1.09387158802297D-01
         WT(7,2) = 1.23491976262066D-01
         WT(8,2) = 1.34709217311474D-01
         WT(9,2) = 1.42775938577060D-01
         WT(10,2) = 1.47739104901339D-01
         WT(11,2) = 1.49445554002917D-01
C
      ELSEIF (N.EQ.12) THEN
C
C         (N,2N+1)=   12   25
C
         PT(1) = -9.96933922529593D-01
         PT(2) = -9.81560634246620D-01
         PT(3) = -9.50537795943120D-01
         PT(4) = -9.04117256370380D-01
         PT(5) = -8.43558124161156D-01
         PT(6) = -7.69902674194224D-01
         PT(7) = -6.84059895470057D-01
         PT(8) = -5.87317954286593D-01
         PT(9) = -4.81339450478153D-01
         PT(10) = -3.67831498998131D-01
         PT(11) = -2.48505748320468D-01
         PT(12) = -1.25233408511434D-01
         PT(13) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 4.71753363865584D-02
         WT(3,1) = 0.0D0
         WT(4,1) = 1.06939325995315D-01
         WT(5,1) = 0.0D0
         WT(6,1) = 1.60078328543288D-01
         WT(7,1) = 0.0D0
         WT(8,1) = 2.03167426723049D-01
         WT(9,1) = 0.0D0
         WT(10,1) = 2.33492536538344D-01
         WT(11,1) = 0.0D0
         WT(12,1) = 2.49147045813361D-01
         WT(13,1) = 0.0D0
         WT(1,2) = 8.25771143316839D-03
         WT(2,2) = 2.30360840389822D-02
         WT(3,2) = 3.89152304692995D-02
         WT(4,2) = 5.36970176077562D-02
         WT(5,2) = 6.72509070508398D-02
         WT(6,2) = 7.99202753336017D-02
         WT(7,2) = 9.15494682950491D-02
         WT(8,2) = 1.01649732279060D-01
         WT(9,2) = 1.10022604977644D-01
         WT(10,2) = 1.16712053501757D-01
         WT(11,2) = 1.21626303523948D-01
         WT(12,2) = 1.24584164536156D-01
         WT(13,2) = 1.25556893905475D-01
C
      ELSEIF (N.EQ.15) THEN
C
C         (N,2N+1)=   15   31
C
         PT(1) = -9.98002298693400D-01
         PT(2) = -9.87992518020345D-01
         PT(3) = -9.67739075679141D-01
         PT(4) = -9.37273392400584D-01
         PT(5) = -8.97264532344082D-01
         PT(6) = -8.48206583410331D-01
         PT(7) = -7.90418501442467D-01
         PT(8) = -7.24417731360095D-01
         PT(9) = -6.50996741297419D-01
         PT(10) = -5.70972172608489D-01
         PT(11) = -4.85081863640239D-01
         PT(12) = -3.94151347077514D-01
         PT(13) = -2.99180007153169D-01
         PT(14) = -2.01194093997383D-01
         PT(15) = -1.01142066918719D-01
         PT(16) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 3.07532419961866D-02
         WT(3,1) = 0.0D0
         WT(4,1) = 7.03660474880725D-02
         WT(5,1) = 0.0D0
         WT(6,1) = 1.07159220467161D-01
         WT(7,1) = 0.0D0
         WT(8,1) = 1.39570677926137D-01
         WT(9,1) = 0.0D0
         WT(10,1) = 1.66269205816981D-01
         WT(11,1) = 0.0D0
         WT(12,1) = 1.86161000015526D-01
         WT(13,1) = 0.0D0
         WT(14,1) = 1.98431485327072D-01
         WT(15,1) = 0.0D0
         WT(16,1) = 2.02578241925522D-01
         WT(1,2) = 5.37747987292339D-03
         WT(2,2) = 1.50079473293162D-02
         WT(3,2) = 2.54608473267154D-02
         WT(4,2) = 3.53463607913758D-02
         WT(5,2) = 4.45897513247648D-02
         WT(6,2) = 5.34815246909279D-02
         WT(7,2) = 6.20095678006707D-02
         WT(8,2) = 6.98541213187283D-02
         WT(9,2) = 7.68496807577206D-02
         WT(10,2) = 8.30805028231332D-02
         WT(11,2) = 8.85644430562116D-02
         WT(12,2) = 9.31265981708256D-02
         WT(13,2) = 9.66427269836236D-02
         WT(14,2) = 9.91735987217921D-02
         WT(15,2) = 1.00769845523875D-01
         WT(16,2) = 1.01330007014792D-01
C
      ELSEIF (N.EQ.20) THEN
C
C         (N,2N+1)=   20   41
C
         PT(1) = -9.98859031588275D-01
         PT(2) = -9.93128599185077D-01
         PT(3) = -9.81507877450248D-01
         PT(4) = -9.63971927277896D-01
         PT(5) = -9.40822633831758D-01
         PT(6) = -9.12234428251306D-01
         PT(7) = -8.78276811252285D-01
         PT(8) = -8.39116971822200D-01
         PT(9) = -7.95041428837557D-01
         PT(10) = -7.46331906460135D-01
         PT(11) = -6.93237656334752D-01
         PT(12) = -6.36053680726494D-01
         PT(13) = -5.75140446819709D-01
         PT(14) = -5.10867001950807D-01
         PT(15) = -4.43593175238725D-01
         PT(16) = -3.73706088715412D-01
         PT(17) = -3.01627868114913D-01
         PT(18) = -2.27785851141632D-01
         PT(19) = -1.52605465240924D-01
         PT(20) = -7.65265211334905D-02
         PT(21) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 1.76140071391723D-02
         WT(3,1) = 0.0D0
         WT(4,1) = 4.06014298003696D-02
         WT(5,1) = 0.0D0
         WT(6,1) = 6.26720483341199D-02
         WT(7,1) = 0.0D0
         WT(8,1) = 8.32767415767082D-02
         WT(9,1) = 0.0D0
         WT(10,1) = 1.01930119817242D-01
         WT(11,1) = 0.0D0
         WT(12,1) = 1.18194531961518D-01
         WT(13,1) = 0.0D0
         WT(14,1) = 1.31688638449168D-01
         WT(15,1) = 0.0D0
         WT(16,1) = 1.42096109318361D-01
         WT(17,1) = 0.0D0
         WT(18,1) = 1.49172986472609D-01
         WT(19,1) = 0.0D0
         WT(20,1) = 1.52753387130707D-01
         WT(21,1) = 0.0D0
         WT(1,2) = 3.07358371852059D-03
         WT(2,2) = 8.60026985564299D-03
         WT(3,2) = 1.46261692569712D-02
         WT(4,2) = 2.03883734612667D-02
         WT(5,2) = 2.58821336049512D-02
         WT(6,2) = 3.12873067770327D-02
         WT(7,2) = 3.66001697582008D-02
         WT(8,2) = 4.16688733279735D-02
         WT(9,2) = 4.64348218674977D-02
         WT(10,2) = 5.09445739237286D-02
         WT(11,2) = 5.51951053482860D-02
         WT(12,2) = 5.91114008806397D-02
         WT(13,2) = 6.26532375547812D-02
         WT(14,2) = 6.58345971336183D-02
         WT(15,2) = 6.86486729285214D-02
         WT(16,2) = 7.10544235534440D-02
         WT(17,2) = 7.30306903327866D-02
         WT(18,2) = 7.45828754004991D-02
         WT(19,2) = 7.57044976845567D-02
         WT(20,2) = 7.63778676720808D-02
         WT(21,2) = 7.66007119179997D-02
C
      ELSEIF (N.EQ.30) THEN
C
C         (N,2N+1)=   30   61
C
         PT(1) = -9.99484410050492D-01
         PT(2) = -9.96893484074594D-01
         PT(3) = -9.91630996870406D-01
         PT(4) = -9.83668123279688D-01
         PT(5) = -9.73116322501127D-01
         PT(6) = -9.60021864968258D-01
         PT(7) = -9.44374444748561D-01
         PT(8) = -9.26200047429226D-01
         PT(9) = -9.05573307699910D-01
         PT(10) = -8.82560535792024D-01
         PT(11) = -8.57205233546061D-01
         PT(12) = -8.29565762382735D-01
         PT(13) = -7.99727835821841D-01
         PT(14) = -7.67777432104801D-01
         PT(15) = -7.33790062453224D-01
         PT(16) = -6.97850494793279D-01
         PT(17) = -6.60061064126623D-01
         PT(18) = -6.20526182989213D-01
         PT(19) = -5.79345235826359D-01
         PT(20) = -5.36624148141996D-01
         PT(21) = -4.92480467861775D-01
         PT(22) = -4.47033769538073D-01
         PT(23) = -4.00401254830392D-01
         PT(24) = -3.52704725530867D-01
         PT(25) = -3.04073202273621D-01
         PT(26) = -2.54636926167882D-01
         PT(27) = -2.04525116682305D-01
         PT(28) = -1.53869913608576D-01
         PT(29) = -1.02806937966733D-01
         PT(30) = -5.14718425552887D-02
         PT(31) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 7.96819249620989D-03
         WT(3,1) = 0.0D0
         WT(4,1) = 1.84664683111015D-02
         WT(5,1) = 0.0D0
         WT(6,1) = 2.87847078833273D-02
         WT(7,1) = 0.0D0
         WT(8,1) = 3.87991925696209D-02
         WT(9,1) = 0.0D0
         WT(10,1) = 4.84026728305875D-02
         WT(11,1) = 0.0D0
         WT(12,1) = 5.74931562175982D-02
         WT(13,1) = 0.0D0
         WT(14,1) = 6.59742298821788D-02
         WT(15,1) = 0.0D0
         WT(16,1) = 7.37559747377015D-02
         WT(17,1) = 0.0D0
         WT(18,1) = 8.07558952294163D-02
         WT(19,1) = 0.0D0
         WT(20,1) = 8.68997872010757D-02
         WT(21,1) = 0.0D0
         WT(22,1) = 9.21225222377777D-02
         WT(23,1) = 0.0D0
         WT(24,1) = 9.63687371746342D-02
         WT(25,1) = 0.0D0
         WT(26,1) = 9.95934205867908D-02
         WT(27,1) = 0.0D0
         WT(28,1) = 1.01762389748416D-01
         WT(29,1) = 0.0D0
         WT(30,1) = 1.02852652893556D-01
         WT(31,1) = 0.0D0
         WT(1,2) = 1.38901369867700D-03
         WT(2,2) = 3.89046112709980D-03
         WT(3,2) = 6.63070391593121D-03
         WT(4,2) = 9.27327965951780D-03
         WT(5,2) = 1.18230152534964D-02
         WT(6,2) = 1.43697295070458D-02
         WT(7,2) = 1.69208891890532D-02
         WT(8,2) = 1.94141411939424D-02
         WT(9,2) = 2.18280358216092D-02
         WT(10,2) = 2.41911620780806D-02
         WT(11,2) = 2.65099548823332D-02
         WT(12,2) = 2.87540487650412D-02
         WT(13,2) = 3.09072575623878D-02
         WT(14,2) = 3.29814470574838D-02
         WT(15,2) = 3.49793380280601D-02
         WT(16,2) = 3.68823646518213D-02
         WT(17,2) = 3.86789456247276D-02
         WT(18,2) = 4.03745389515360D-02
         WT(19,2) = 4.19698102151642D-02
         WT(20,2) = 4.34525397013561D-02
         WT(21,2) = 4.48148001331625D-02
         WT(22,2) = 4.60592382710070D-02
         WT(23,2) = 4.71855465692992D-02
         WT(24,2) = 4.81858617570872D-02
         WT(25,2) = 4.90554345550298D-02
         WT(26,2) = 4.97956834270743D-02
         WT(27,2) = 5.04059214027823D-02
         WT(28,2) = 5.08817958987495D-02
         WT(29,2) = 5.12215478492588D-02
         WT(30,2) = 5.14261285374591D-02
         WT(31,2) = 5.14947294294517D-02
C
      ELSEIF (N.EQ.40) THEN
C
C         (N,2N+1)=   40   81
C
         PT(1) = -9.99707559258702D-01
         PT(2) = -9.98237709710512D-01
         PT(3) = -9.95250573446071D-01
         PT(4) = -9.90726238699406D-01
         PT(5) = -9.84722839864247D-01
         PT(6) = -9.77259949983733D-01
         PT(7) = -9.68323126854152D-01
         PT(8) = -9.57916819213754D-01
         PT(9) = -9.46071837162499D-01
         PT(10) = -9.32812808278623D-01
         PT(11) = -9.18149543072900D-01
         PT(12) = -9.02098806968837D-01
         PT(13) = -8.84692008701087D-01
         PT(14) = -8.65959503212213D-01
         PT(15) = -8.45923985587312D-01
         PT(16) = -8.24612230833264D-01
         PT(17) = -8.02060566140248D-01
         PT(18) = -7.78305651426486D-01
         PT(19) = -7.53379803438939D-01
         PT(20) = -7.27318255189900D-01
         PT(21) = -7.00162977487331D-01
         PT(22) = -6.71956684614155D-01
         PT(23) = -6.42739524305576D-01
         PT(24) = -6.12553889667957D-01
         PT(25) = -5.81447065829131D-01
         PT(26) = -5.49467125095116D-01
         PT(27) = -5.16660607386385D-01
         PT(28) = -4.83075801686166D-01
         PT(29) = -4.48764513638160D-01
         PT(30) = -4.13779204371586D-01
         PT(31) = -3.78171435473590D-01
         PT(32) = -3.41994090825745D-01
         PT(33) = -3.05302441735243D-01
         PT(34) = -2.68152185007231D-01
         PT(35) = -2.30598521880715D-01
         PT(36) = -1.92697580701347D-01
         PT(37) = -1.54506879379390D-01
         PT(38) = -1.16084070675229D-01
         PT(39) = -7.74865883312827D-02
         PT(40) = -3.87724175060129D-02
         PT(41) = 0.0D0
         WT(1,1) = 0.0D0
         WT(2,1) = 4.52127709857483D-03
         WT(3,1) = 0.0D0
         WT(4,1) = 1.04982845311367D-02
         WT(5,1) = 0.0D0
         WT(6,1) = 1.64210583818923D-02
         WT(7,1) = 0.0D0
         WT(8,1) = 2.22458491942125D-02
         WT(9,1) = 0.0D0
         WT(10,1) = 2.79370069799963D-02
         WT(11,1) = 0.0D0
         WT(12,1) = 3.34601952825375D-02
         WT(13,1) = 0.0D0
         WT(14,1) = 3.87821679744755D-02
         WT(15,1) = 0.0D0
         WT(16,1) = 4.38709081856796D-02
         WT(17,1) = 0.0D0
         WT(18,1) = 4.86958076350388D-02
         WT(19,1) = 0.0D0
         WT(20,1) = 5.32278469839487D-02
         WT(21,1) = 0.0D0
         WT(22,1) = 5.74397690994031D-02
         WT(23,1) = 0.0D0
         WT(24,1) = 6.13062424929203D-02
         WT(25,1) = 0.0D0
         WT(26,1) = 6.48040134565959D-02
         WT(27,1) = 0.0D0
         WT(28,1) = 6.79120458152260D-02
         WT(29,1) = 0.0D0
         WT(30,1) = 7.06116473913001D-02
         WT(31,1) = 0.0D0
         WT(32,1) = 7.28865823957805D-02
         WT(33,1) = 0.0D0
         WT(34,1) = 7.47231690579957D-02
         WT(35,1) = 0.0D0
         WT(36,1) = 7.61103619006116D-02
         WT(37,1) = 0.0D0
         WT(38,1) = 7.70398181642697D-02
         WT(39,1) = 0.0D0
         WT(40,1) = 7.75059479784290D-02
         WT(41,1) = 0.0D0
         WT(1,2) = 7.87863323894401D-04
         WT(2,2) = 2.20748573572679D-03
         WT(3,2) = 3.76522867934199D-03
         WT(4,2) = 5.27194271488540D-03
         WT(5,2) = 6.73181348520741D-03
         WT(6,2) = 8.19757638675139D-03
         WT(7,2) = 9.67540148401719D-03
         WT(8,2) = 1.11313216640276D-02
         WT(9,2) = 1.25543847685172D-02
         WT(10,2) = 1.39625598669806D-02
         WT(11,2) = 1.53613263591024D-02
         WT(12,2) = 1.67345324750026D-02
         WT(13,2) = 1.80738684088182D-02
         WT(14,2) = 1.93876458943179D-02
         WT(15,2) = 2.06790432735282D-02
         WT(16,2) = 2.19381873358330D-02
         WT(17,2) = 2.31589310133770D-02
         WT(18,2) = 2.43456901822734D-02
         WT(19,2) = 2.55002176031301D-02
         WT(20,2) = 2.66157374990246D-02
         WT(21,2) = 2.76876261110610D-02
         WT(22,2) = 2.87183868410922D-02
         WT(23,2) = 2.97089272777766D-02
         WT(24,2) = 3.06543608914116D-02
         WT(25,2) = 3.15512236191153D-02
         WT(26,2) = 3.24009825076059D-02
         WT(27,2) = 3.32040443412576D-02
         WT(28,2) = 3.39568628342097D-02
         WT(29,2) = 3.46569358434976D-02
         WT(30,2) = 3.53051447086219D-02
         WT(31,2) = 3.59016027836283D-02
         WT(32,2) = 3.64438265303411D-02
         WT(33,2) = 3.69301695340487D-02
         WT(34,2) = 3.73611800254692D-02
         WT(35,2) = 3.77368012630936D-02
         WT(36,2) = 3.80554637788524D-02
         WT(37,2) = 3.83163240051747D-02
         WT(38,2) = 3.85197417499508D-02
         WT(39,2) = 3.86655554391411D-02
         WT(40,2) = 3.87530293787524D-02
         WT(41,2) = 3.87821047642829D-02
C
      ELSE
         WRITE (FU6,1000) N
         STOP 'KG1 1'
      ENDIF
C
      MP = M+1
      DO 10 I = 1, N
         II = MP-I
         PT(II) = -PT(I)
         DO 10 J = 1, 2
            WT(II,J) = WT(I,J)
   10 CONTINUE
      RETURN
C
 1000     FORMAT(1X,10(1H*),31H NUMBER OF QUADRATURE POINTS IS,I3/12X,
     * 57HCHOOSE A VALUE FROM 2-7,9,10,12,15,20,30,40 AND TRY AGAIN )
C
      END subroutine kg1
C
C**********************************************************************
C  LCGIT
C**********************************************************************
      SUBROUTINE lcgit(ii,ie,iz0,iz1,qdx)
      use common_inc
      use perconparam
      use rate_const,only : nsplic
      use kintcm, only : ilcrst
C     Written by AFR, jun 2001, put in by AFR, may 2002
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION QDX(NQD)
      DIMENSION ISIC(NQD)
C
      NSP1D=NSPLIC(II)
      IF(IZ1-IZ0.LE.NSP1D) RETURN
C
      DO I=1,NQD
       ISIC(I)=0
       VLCIC(I)=0.0D0
      ENDDO
      IS=(IZ1-IZ0)/(NSP1D-1)
      DO IC=1,NSP1D-1
       ISIC(IC+1)=IZ0+IC*IS
      ENDDO
      ISIC(1)=IZ0
      ISIC(NSP1D)=IZ1
      DO I=1,NSP1D
       IF(ILCRST.EQ.1) THEN
        IF(VEFRST(II,IE,ISIC(I)).NE.0.0D0) THEN
         VLCIC(I)=VEFRST(II,IE,ISIC(I))
         ZETIC(I)=QDX(ISIC(I))
        ELSE
         Z=QDX(ISIC(I))
         CALL LCPATH(X,Z)
         CALL EHOOK(1,iproc)
         VLCIC(I)=V
         ZETIC(I)=Z
        ENDIF
       ELSE
        Z=QDX(ISIC(I))
        CALL LCPATH(X,Z)
        CALL EHOOK(1,iproc)
        VLCIC(I)=V
        ZETIC(I)=Z
       ENDIF
      ENDDO
      END subroutine lcgit
C
C***********************************************************************
C  LCGSRT
C***********************************************************************
      SUBROUTINE lcgsrt (PLCG3U, PSORT, NE, NNPROD)
      use perconparam

C
C   Sort the uniformized probabilites over all open states. 
C   
C
C     Called by LCG34
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DOUBLE PRECISION PLCG3U(MAXPS, NSV), PSORT(NSV)
C
C
      DO 10 I = 1, NE
            PSORT(I) = 0.D0
            PMAX     = PLCG3U(1, I)
      DO 20 J = 2, NNPROD
            IF (PLCG3U(J, I) .GT. PMAX) PMAX = PLCG3U(J, I)
  20  CONTINUE
            PSORT(I) = PMAX
 10   CONTINUE
      RETURN
      END subroutine lcgsrt
C
C**********************************************************************
C  LCNORM
C**********************************************************************
      SUBROUTINE lcnorm(COF,XQ,QM,N3TM,N3,NV,NF)
C
C     Called by:
C               LCGTH                                                   0708JC00
C
C Find the projections of XQ along the normal coordinates COF
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION COF(N3TM,NV),XQ(N3TM),QM(NV)
C
      DO 20 JF=1,NF
         QM1 = 0.0D0
         DO 10 IF=1,N3
            QM1 = QM1 + COF(IF,JF)*XQ(IF)
10       CONTINUE
         QM(JF) = QM1
20    CONTINUE
C     WRITE(FU6,25)(QM(I),I=1,N3)
25    FORMAT(2X,'QM =',9E14.5)
      RETURN
      END SUBROUTINE lcnorm
C
C**********************************************************************
C  LCNWTN
C**********************************************************************
      SUBROUTINE lcnwtn (IE,XLIN,NDIMX,UALF,SX,ZETA,IR,IERR,            7/14YL92
     *   LCGX,ZETOLD)
      use common_inc
      use perconparam, only : n3tm,fu6
      use rate_const
C
C     Called by:
C               LCGTH                                                   0708JC00
C     Call:
C          LCPATH, LOCATE, INTRPL, AND THE FUNCTION XDOTP
C*
C This subroutine determines the value of s along a MEP that corresponds
C    to a geometry at ZETA along the LCG path such that the vector from
C    MEPX to LCX is orthogonal to the mass-scaled gradient vector (DX)
C    on the MEP at s or the distance from MEPX to LCX is sufficiently
C    small.
C    MEPX is the mass-weighted coordinate vector on the MEP at s.
C    LCX is the mass-weighted coordinate vector on the LCG path at zeta.
C*
C  PARAMETERS AND COMMON BLOCKS MODIFIED 6/29/91
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION MEPX(N3TM),LCX(N3TM),LCGX(N3TM)                  05/08B91
      DIMENSION XLIN(N3TM)                                              7/14YL92
C
      DATA LNITER,NSTP,TOL1,TOLD/501,500,1.0D-7,1.0D-8/
      DATA ONE /1.0D0/
C*
      IERR = 0
C Compute the coordinate vector LCX at zeta
      CALL LCPATH(LCX,ZETA)
C Get geometry, gradient at old s
      CALL LOCATE(SSUBI,LSAVE,SX,JN)                                    7/14YL92
      CALL INTRPL (GEOM, SSUBI, MEPX, SX, JN, LSAVE, LSAVE, 1, N3TM,    7/14YL92
     * 1, N3)                                                           7/14YL92
      CALL INTRPL (DXSV, SSUBI, DX, SX, JN, LSAVE, LSAVE, 1, N3TM, 1,   7/14YL92
     * N3)                                                              7/14YL92
C Compute vector from LCX to MEPX and its length UALF.
      DSQ = 0.0D0
      DO 10 I2 = 1,N3
         XLIN(I2) = LCX(I2) - MEPX(I2)
         DSQ = DSQ + XLIN(I2)*XLIN(I2)
10    CONTINUE
      UALF = SQRT(DSQ)
C Skip next section if UALF is sufficiently small
      IF (UALF.GT.TOL1) THEN
C Start search for where dot product of the gradient with XLIN is zero.
C    Initial guess is the last s value used, increasing s will decrease
C    the value of the dot product.
C Compute dot product of gradient with the vector XLIN
         DOTP = XDOTP(DX,XLIN,N3)
         IF(DOTP.LT.0.0) THEN
            SM = SSUBI(1)
            SP = SX
         ELSE
            SM = SX
            SP = SSUBI(LSAVE)                                           7/14YL92
         END IF
C Initial guess at step
         DELS = (ZETA-ZETOLD)*XDOTP(DX,LCGX,N3)                          5/08B91
         ZETOLD = ZETA                                                   5/08B91
         IFLG = 0
C Step s by DELS until DOTP changes sign
20       CONTINUE
         SOLD = SX
         DOTPO = DOTP
         SX = SX + DELS
         IF(SX.GE.SSUBI(LSAVE) .OR. SX.LE.SSUBI(1)) THEN                7/14YL92
            WRITE (FU6,*) ' PROBLEM FINDING S(ZETA) FOR ZETA=',ZETA,       5/08B91
     *       'SX = ',SX  
            IERR = 1                                                     5/08B91
            IF (SX.GT.SSUBI(LSAVE)) SX=SSUBI(LSAVE)                     7/14YL92
            IF (SX.LT.SSUBI(1)) SX=SSUBI(1)                                      5/08B91
            RETURN                                                       5/08B91
         END IF                                                          5/08B91
C Get geometry, gradient at s
         CALL LOCATE(SSUBI,LSAVE,SX,JN)                                 7/14YL92
         CALL INTRPL (GEOM, SSUBI, MEPX, SX, JN, LSAVE, LSAVE, 1, N3TM, 7/14YL92
     *    1, N3)                                                        7/14YL92
         CALL INTRPL (DXSV, SSUBI, DX, SX, JN, LSAVE, LSAVE, 1, N3TM, 1,7/14YL92
     *    N3)                                                           7/14YL92
C Compute vector from LCX to MEPX and its length UALF
         DSQ = 0.0D0
         DO 30 I2 = 1,N3
            XLIN(I2) = LCX(I2) - MEPX(I2)
            DSQ = DSQ + XLIN(I2)*XLIN(I2)
30       CONTINUE
         UALF = SQRT(DSQ)
         IF (UALF.GT.TOL1) THEN
C Compute dot product of gradient with the vector XLIN
            DOTP = XDOTP(DX,XLIN,N3)
            IF (ABS(DOTP) .GT. TOLD) THEN
               IF (DOTP*DOTPO.GT.0.0) THEN
                  IF (SX.GT.SSUBI(1) .AND. SX.LT.SSUBI(LSAVE)) GO TO 20 7/14YL92
                  IF (IFLG.EQ.0) THEN
                     IF (SX.LE.SSUBI(1)) SX = SSUBI(1)
                     IF (SX.GE.SSUBI(LSAVE)) SX = SSUBI(LSAVE)          7/14YL92
                     IFLG = 1
                     GO TO 20
                  ELSE
                     WRITE(FU6,*) ' Problem in LCNWTN, DOTP did not',
     *                    ' change sign'
                     IERR = 1
                     RETURN
                  END IF
               END IF
            END IF
         END IF
      END IF
C
      IF (DELS.LT.0.0) THEN
         SM = SX
         SP = SOLD
      ELSE
         SM = SOLD
         SP = SX
      END IF
      IC = 0
C Newton root search
40    CONTINUE
         IC = IC + 1
         IF (UALF.GT.TOL1.AND.ABS(DOTP).GT.TOLD.AND.IC.LT.LNITER) THEN
C Newton step
            DELS = -DOTP*(SX-SOLD)/(DOTP-DOTPO)
            IF (SX+DELS.LE.SM.OR.SX+DELS.GE.SP) DELS=0.5*(SM+SP)-SX
            SOLD = SX
            DOTPO = DOTP
            SX = SX + DELS
C Get geometry, gradient at s
            CALL LOCATE(SSUBI,LSAVE,SX,JN)                              7/14YL92
            CALL INTRPL (GEOM, SSUBI, MEPX, SX, JN, LSAVE, LSAVE,1,N3TM,7/14YL92
     *         1, N3)                                                   7/14YL92
            CALL INTRPL (DXSV, SSUBI, DX, SX, JN, LSAVE, LSAVE,1,N3TM,1,7/14YL92
     *         N3)                                                      7/14YL92
C Compute vector from LCX to MEPX and its length UALF
            DSQ = 0.0D0
            DO 50 I2 = 1,N3
               XLIN(I2) = LCX(I2) - MEPX(I2)
               DSQ = DSQ + XLIN(I2)*XLIN(I2)
50          CONTINUE
            UALF = SQRT(DSQ)
            IF (UALF.GT.TOL1) THEN
C Compute dot product of gradient with the vector XLIN
              DOTP = XDOTP(DX,XLIN,N3)
              IF (DOTP.GT.0.0) SM = SX
              IF (DOTP.LT.0.0) SP = SX
            ENDIF
         GO TO 40
      END IF
      IF (IC.GE.LNITER) THEN
         WRITE (FU6,6000) IC,SX,DOTP,UALF
         IERR = 1
      END IF
      RETURN
6000  FORMAT(' IN LCNWTN, ROOT SEARCH FOR S(ZETA) DID NOT CONVERGE',/,
     *  ' IC,S,DOTP,UALF=', I5,1P,3E13.5)
      END SUBROUTINE lcnwtn
C
C**********************************************************************
C  LCPATH
C**********************************************************************
      SUBROUTINE lcpath(XX,ZETA)
      use common_inc
      use perconparam, only : n3tm
      use rate_const
C
C     Called by:
C               LCGTH,LCNWTN                                            0708JC00
C*
C Calculate the LCG tunneling path.
C
C     X is the mass-scaled cartesian vector at ZETA
C     X0 and X1 are the mass-scaled cartesian vector on the MEP at the
C        adiabatic turning points.  They are computed in LCSET.
C*
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/29/91
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DOUBLE PRECISION XX(N3TM)
C
      TERM = (ZETA - ZETA0)/(ZETA1 - ZETA0)
      DO 10 I=1,N3TM      
         XX(I) = RX0(I) + TERM*(RX1(I) - RX0(I))
 10   CONTINUE
      RETURN
      END SUBROUTINE lcpath
C
C***********************************************************************
C  LCPROB
C***********************************************************************
      SUBROUTINE lcprob (PLCG3P, PLCG3U, IE, NNPROD)
      use perconparam
C
C     Calculates the LCG probabilities from the primitive values.       0708JC00
C     This subroutine was restructured by G. Lynch 10/12/91
C
C     Called by LCG34                                                   0708JC00
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DOUBLE PRECISION PLCG3P(MAXPS, NSV), PLCG3U(MAXPS, NSV)           
C
C*   DETERMINE THE LCG3 UNIFORMIZED PROBABILITIES FROM THE PRIMITIVE 
C    PROBABILITIES AT EACH ENERGY FOR EACH PRODUCT STATE 
C
      DO 10 I = 1, NNPROD
         PVAG = PLCG3P(I, IE)                                           7/14YL92
         PLCG3U(I,IE) = 0.5D0                                           7/14YL92
      DO 20 J = 1, IE-1                                                 7/14YL92
         PIXX = PLCG3P(I, J)
            IF(PIXX .GT. 0.0D0) THEN
               TERM = PIXX / (1.0D0 + PIXX)
C
               PLC = (1.0D0 + (1.0D0 / PVAG - 1.0D0) * PIXX /
     *                (2.0D0 * PVAG)) * TERM
               PLCG3U(I, J) = PLC
            ELSE
               PLCG3U(I, J) = 0.0D0
            ENDIF
 20   CONTINUE
 10   CONTINUE
C
      RETURN
      END SUBROUTINE lcprob 
C
C**********************************************************************
C  LCPROJ
C**********************************************************************
      SUBROUTINE lcproj(LGTM,X0,X1,SMLQP,N3TM,N3,NV,NM)
C
C     Called by:
C               LCGTH, LCSET                                            0708JC00
C*
C Calculate the projection of the path from X0 to X1 along the normal
C     mode coordinates LGTM.  The columns of LGTM give the direction of
C     the normal coordinates in 3N dimension.
C*
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION X0(N3TM),X1(N3TM),SMLQP(NV),LGTM(N3TM,NV)
C
      SUM2 = 0.0D0
      DO 20 I=1,NM
         SUM1 = 0.0D0
         DO 10 J=1,N3
            TERM = (X1(J) - X0(J))*LGTM(J,I)
            SUM1 = SUM1 + TERM
10       CONTINUE
         SMLQP(I) = SUM1
         SUM2 = SUM2 + SUM1*SUM1
20    CONTINUE
      SUM2 = SQRT(SUM2)                                                  7/29B91
      IF (SUM2.EQ.0.0D0) RETURN                                          7/29B91
      DO 30 I=1,NM
         SMLQP(I) = SMLQP(I)/SUM2                                        7/29B91
30    CONTINUE
      RETURN
      END SUBROUTINE lcproj
C
C**********************************************************************
C  LCSET
C**********************************************************************
      SUBROUTINE lcset(IE,IPROD,ENER,STP0,STP1,ZETA,LCGX,SINX,SINXP,
     * COSX,COSXP,TAU,TAUPR)
      use common_inc
      use perconparam
      use rate_const
C
C Set up LCG tunneling path and get angles and vibrational period at 
C   adiabatic turning points
C
C Called by:
C      LCG3
C
C Call:
C      LCSTX, LOCATE, INTRPL, LCPROJ, LCVIB, AND FUNCTION LCXTAU
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 03/07/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C LCGX is the unit vector along the tunneling direction
      DOUBLE PRECISION LCGX(N3)
      DOUBLE PRECISION LCXTAU
      DIMENSION SMLQ(NVIBM),EVM(NVIBM),RXX(N3TM)
C
C The following common blocks are used to transfer needed arrays
C     COMMON /TXXCM/ BCUR(NVIBM,NSDM), TRNPL(NVIBM,NSDM),
C    *   TRNPG(NVIBM,NSDM), DXSV(N3TM,NSDM), COFSV(N3TM,NVIBM,NSDM)
C     COMMON /TX1CM/ RX0(N3TM),RX1(N3TM),ZETA0,ZETA1
C     COMMON /TX3CM/ NG,NGS0,IOT,IEXOG,DELNT
C
      call lcset_mem
      call fdiag_mem(N3TM)
      IF (IPROD .NE. 0) THEN
         IF (IEXOG.EQ.0) THEN
            STPX = STP0
         ELSE
            STPX = STP1
         ENDIF
C
         CALL LOCATE(SSUBI,LSAVE,STPX,JJX)                           
C
         CALL INTRPL (GEOM,SSUBI,RXX,STPX,JJX,NSDM,LSAVE,1,N3TM,1,N3) 
C
         CALL LCSTX (IE,IPROD,ENER,STXX,RXX,JJX)    
C
         IF (IEXOG.EQ.0) THEN
             STP1 = STXX
         ELSE
             STP0 = STXX
         ENDIF 
      ENDIF
C Locate the index on the SSUBI grid corresponding to the turning point 
C    in the reactant region and interpolate the require data
      CALL LOCATE(SSUBI,LSAVE,STP0,JJ0)                                 7/14YL92
C Interpolations:
C   Geometry at left turning point
      CALL INTRPL(GEOM, SSUBI, RX0, STP0, JJ0, NSDM, 
     *            LSAVE, 1, N3TM, 1, N3)                                7/14YL92
C   Gradient vector at left turning point
      CALL INTRPL (DXSV, SSUBI, DX, STP0, JJ0, NSDM, 
     *             LSAVE, 1, N3TM, 1, N3)                               7/14YL92
C   Eigenvectors at left turning point
      CALL INTRPL (COFSV, SSUBI, COFX, STP0, JJ0, NSDM, 
     *             LSAVE, N3TM, N3TM, N3, N3M7)                         7/14YL92
C   Frequencies at left turning point
C
C Locate the index on the SSUBI grid corresponding to the turning point
C    in the product region and interpolate the require data
      CALL LOCATE(SSUBI,LSAVE,STP1,JJ1)                                 7/14YL92
C Interpolations:
C   Geometry at right turning point
      CALL INTRPL(GEOM, SSUBI, RX1, STP1, JJ1, NSDM, 
     *            LSAVE, 1, N3TM, 1, N3)                                7/14YL92
C
C Determine the unit vector along the tunneling path and store in LCGX
      ZETA = 0.0D0
      DO 10 I = 1,N3                                                    7/14YL92
         TERM = RX1(I) - RX0(I)
         LCGX(I) = TERM
         ZETA = ZETA + TERM*TERM
 10   CONTINUE
      ZETA = SQRT(ZETA)
      DO 15 I=1,N3                                                      7/14YL92
         LCGX(I) = LCGX(I)/ZETA
 15   CONTINUE
C
C Calculate the angle between the unit vectors dx and lcgx.
      COSX = 0.0D0
      DO 35 K=1,N3                                                      7/14YL92
         COSX = COSX + DX(K)*LCGX(K)
 35   CONTINUE
      COSX = MAX(-1.0D0,COSX)
      COSX = MIN(1.0D0,COSX)
      COSX = ABS(COSX)
      SINX = SQRT(1.0D0 - COSX*COSX)
C Get the projection SMLQ of the path from RX0 to RX1 along the normal
C     mode coordinates COFX.
      CALL LCPROJ(COFX,RX0,RX1,SMLQ,N3TM,N3,NVIBM,N3M7)                 7/14YL92
C
C    ASSIGN MODE(I) ACCORDING TO IN WHICH MEP RANGE STP0 IS             6/30YL91
C                                                                          ..
      MARR = 1                                                             ..
      IF (LGS(5).GT.21) THEN                                               ..
         NARL = NARR - 20 - 1                                              ..
         DO  50 IARR = 1, NARL                                             ..
             IF (STP0.GE.SRARR(IARR)) MARR = MARR + 1                      ..
50       CONTINUE                                                          ..
      ENDIF                                                                ..
      DO 60 IFRQ = 1, N3M7                                                 ..
         MODE(IFRQ) = MODETS(MARR,IFRQ)                                    ..
60    CONTINUE                                                          6/30YL91
      CALL INTRPL (WETS,SSUBI,FREQ,STP0,JJ0,NSDM,LSAVE,1, NVIBM,1,      7/14YL92
     *  N3M7)                                                           7/14YL92
C
C Interpolates the anharmonicities at the left turning
C This part is commented to save comtutation time.  In future, when LCG3
C method is compatible with anharmonicity options, this part should be 
C uncommented, and should be modified if necessary.
C                                                        Y.-P. Liu
C      IF (LGS(5) .NE. 0) THEN                                          7/14YL92
C         CALL INTRPL (XETS, SS, ANHARM, STP0, JJ0, NSDM, LSAVE,1,NVIBM,   ..
C     *      1, N3M7)                                                      ..
C         CALL INTRPL (Y0TS, SS, Y00, STP0, JJ0, NSDM, LSAVE,1,NVIBM, 1,   ..
C     *      N3M7)                                                         ..
C      END IF                                                           7/14YL92
C Get the zero point energy of the individual modes
      NV = 0
      CALL LCVIB (NV,EVM,VIBE)
C Get the vibrational period
      TAU = LCXTAU(EVM,SMLQ,NVIBM,N3M7)
C Interpolations:
C    Gradient vector at right turning point
      CALL INTRPL (DXSV, SSUBI, DX, STP1, JJ1, NSDM, 
     *             LSAVE, 1, N3TM, 1, N3)                               7/14YL92
C    normal mode eigenvector at right turning point
      CALL INTRPL (COFSV, SSUBI, COFX, STP1, JJ1, NSDM, 
     *             LSAVE, N3TM, N3TM, N3, N3M7)                         7/14YL92
C
C Calculate the angle between the unit vectors dx and lcgx.
      COSXP = 0.0D0
      DO 37 K=1,N3                                                      7/14YL92
         COSXP = COSXP + DX(K)*LCGX(K)
 37   CONTINUE
      COSXP = MAX(-1.0D0,COSXP)
      COSXP = MIN(1.0D0,COSXP)
      COSXP =ABS(COSXP)
      SINXP = SQRT(1.0D0 - COSXP*COSXP)
C Get projection normal coordinates
      CALL LCPROJ(COFX,RX0,RX1,SMLQ,N3TM,N3,NVIBM,N3M7)                 7/14YL92
C
C    ASSIGN MODE(I) ACCORDING TO IN WHICH MEP RANGE STP1 IS             6/30YL91
C                                                                          ..
      MARR = 1                                                             ..
      IF (LGS(5).GT.21) THEN                                               ..
         NARL = NARR - 20 - 1                                              ..
         DO  70 IARR = 1, NARL                                             ..
             IF (STP1.GE.SRARR(IARR)) MARR = MARR + 1                      ..
70       CONTINUE                                                          ..
      ENDIF                                                                ..
      DO 80 IARR = 1, N3M7                                                 ..
         MODE(IARR) = MODETS(MARR,IARR)                                    ..
80    CONTINUE                                                          6/30YL91
C    Frequencies at right turning point
      CALL INTRPL (WETS, SSUBI, FREQ, STP1, JJ1, NSDM, LSAVE, 1,NVIBM,1,7/14YL92
     *   N3M7)                                                             ..
C                                                                          ..
C Interpolates the anharmonicities at the left turning                     ..
C This part is commented to save comtutation time.  In future, when LCG3   ..
C method is compatible with anharmonicity options, this part should be     ..
C uncommented, and should be modified if necessary.                        ..
C                                                        Y.-P. Liu         ..
C      IF (LGS(5) .NE. 0) THEN                                             ..
C         CALL INTRPL (XETS, SSUBI, ANHARM, STP1, JJ1, NSDM, LSAVE, 1,     ..
C     *      NVIBM, 1, N3M7)                                               .. 
C         CALL INTRPL (Y0TS, SS, Y00, STP1, JJ1, NSDM, LSAVE, 1,NVIBM,1,   ..
C     *      N3M7)                                                         ..
C      END IF                                                           7/14YL92
C Get vibrational energy levels for the individual modes
      CALL LCVIB (NV,EVM,VIBE)
C Get the vibrational period.
      TAUPR = LCXTAU(EVM,SMLQ,NVIBM,N3M7)
C
 40   RETURN
      END SUBROUTINE lcset
C
C***********************************************************************
C  LCSINT
C***********************************************************************
      SUBROUTINE lcsint(ST,ESV,SINX,THETA,THETT,TAU,REDMX,PE1,PE2,
     *   IE0,NEMX)
      use common_inc
      use perconparam
      use rate_const, only: ngs0
C
C     Integration of tunneling amplitudes over tunneling path termini
C
C     Called by:
C               LCG34                                                   0708JC00
C     Call:
C          GAUSQD
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 22/07/91
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION ST(NEMX),ESV(NEMX),SINX(NEMX),TAU(NEMX)
      DIMENSION THETA(NEMX),THETT(NEMX),PE1(NEMX),PE2(NEMX)
      DIMENSION SX(NQD),WTX(NQD),DY(4)                                  7/14YL92
C
      DO 10 IE = 1,NEMX
         PE1(IE) = 0.0D0
         PE2(IE) = 0.0D0
10    CONTINUE
C
C Loop over s values on the energy save grid.  NGS0 point quadrature
C   is used in each interval from ST(IS-1) to ST(IS).  In each interval
C   the contribution to each open energy (from IS to NEMX-1) is 
C   accumulated.
      IS0 = IE0 - 1
      NFIT = NEMX - IS0 + 1
      DO 100 IS = IE0,NEMX-1
         S1 = ST(IS-1)
         S2 = ST(IS)
C Set up quadrature grid
         IF (S1.LT.S2) THEN
            CALL GAUSQD(S1,S2,SX,WTX,NGS0)
         ELSE
            CALL GAUSQD(S2,S1,SX,WTX,NGS0)
         END IF
         JJ = IS - 1                                                    7/14YL92
         NN = 4                                                             ..
         IF (LGS2(9).GT.1) NN = LGS2(9)                                     ..
         NM = NN / 2                                                        ..
         IF (JJ .EQ. IS0) NM = 1                                            ..
         IF (JJ. EQ. (NEMX-1)) NM = 3                                       ..
         JNL = JJ - NM + 1                                              7/14YL92
C Loop over quadrature points to calculate the contribution to the
C   integral from s1 to s2
         DO 90 IQ = 1,NGS0
            SXX = SX(IQ)
C Interpolate tunneling integrals, vibrational period, tunneling energy,
C    and sin(angle between tunneling path and grad)
            CALL POLINT(ST(JNL),THETA(JNL),NN,SXX,YTH,DYY)
            CALL POLINT(ST(JNL),THETT(JNL),NN,SXX,YTT,DYY)
            CALL POLINT(ST(JNL),TAU(JNL),NN,SXX,YT,DYY)
            CALL POLINT(ST(JNL),ESV(JNL),NN,SXX,YV,DYY)
            CALL POLINT(ST(JNL),SINX(JNL),NN,SXX,YS,DYY)
            TERM = WTX(IQ)*YS/YT
            TERM1 = TERM*EXP(-YTH)
            TERM2 = TERM*EXP(-YTT)
            DO 80 IE = IS,NEMX-1
               VR = 2.0D0*(ESV(IE) - YV)/REDMX
               IF(VR.GT.0.0D0) THEN
                  VR = SQRT(VR)
                  PE1(IE) = PE1(IE) + TERM1/VR
                  PE2(IE) = PE2(IE) + TERM2/VR
               ENDIF
80          CONTINUE
90       CONTINUE
100   CONTINUE
      RETURN
      END subroutine lcsint
C
C***********************************************************************
C LCSTX
C***********************************************************************
C
      SUBROUTINE lcstx(IE,IPROD,ENER,STXX,RXX,JJX)
      use common_inc
      use perconparam
      use rate_const
C
C For excited state product set up adiabatic potential and get
C    turning point in product state
C
C If the LCT print option is turned on then the values of wp are stored
C in the array wpsv for each MEP save grid point.
C 
C     CALLED BY: LCSET
C
C     CALL: SPL1D1, SPL1B1, LCTP, LCPROJ
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION AA(NSDM),ABX(NSDM),AC(NSDM),AD(NSDM),
     *   SCR(NSDM),IOOP(2)                                              7/14YL92
      DIMENSION SMLQ(NVIBM),RXX(N3TM)                                   7/14YL92
C
      IF (IEXOG.EQ.0) THEN
         LIBEG = JJX + 1                                                6/24DL91
         IEND = LSAVE                                                   7/14YL92
         IXX = LSAVE                                                    7/14YL92
         IF (NCUTF.EQ.1) THEN                                           7/14YL92
            IXX = ICUTF - 1                                                 ..
            IEND = ICUTF - 1                                                ..
         ENDIF                                                          7/14YL92
      ELSE
         IEND = JJX
         LIBEG = 1
         IXX = 1
         IF (NCUTF.EQ.2) THEN                                           7/14YL92
            LIBEG = ICUTF                                                   ..
            IXX = ICUTF                                                     ..
         ENDIF                                                          7/14YL92
      ENDIF
C
C Calculate excited-state adiabatic potential 
      DO 40 IS = LIBEG, IEND                                          
C Get projected normal mode coordinates for the path from RX0 to the
C   IS grid point in the product valley
            CALL LCPROJ(COFSV(1,1,IS),RXX,GEOM(1,IS),SMLQ,
     *         N3TM,N3,NVIBM,N3M7)                                      7/14YL92
C Calculate the frequency of the p mode
            TERM = 0.0D0
            DO 20 I = 1, N3M7
               IF (WETS(I,IS).GT.0) TERM = TERM+(SMLQ(I)*WETS(I,IS))**2 7/14YL92
20          CONTINUE
            WP = SQRT(TERM)
            VADEX(IS) = VADIB(IS) + DBLE(IPROD)*WP
C
C If iot is equal to 2 then save the values of wp in the array wpsv
C so that they can be printed to unit fu22 in the subroutine lcg3.f
C
                IF (IOT .EQ. 2) WPSV(IS) = WP                           1/10/GL91
C
C Find the turning points for each generalized normal mode at save grids
C      along the exoergic direction
            DO 30 JJ = 1, N3M7
               IF (WETS(JJ,IS).LE.0.0D0) THEN
                  TPLXX(JJ,IS) = 0.0D0
               ELSE
                  EQM0 = WETS(JJ,IS)                                    7/14YL92
                  EDLQM = DBLE(IPROD)*(SMLQ(JJ)*EQM0)**2/WP             7/14YL92
                  EQM = 0.5D0*EQM0 + EDLQM                              7/14YL92
                  TPLXX(JJ,IS) = EQM                                    7/14YL92
               ENDIF
30          CONTINUE
40       CONTINUE
C
C Spline fit adiabatic potential curve
         IOOP(1) = 5
         IOOP(2) = 5
         NSDEX = IEND - LIBEG + 1
         CALL SPL1D1 (NSDEX,SSUBI(LIBEG),VADEX(LIBEG),
     *                SCR,IOOP,1,AA(LIBEG),ABX(LIBEG),AC(LIBEG))
         CALL SPL1B1 (NSDEX,SSUBI(LIBEG),VADEX(LIBEG),SCR,1,AA(LIBEG),
     *      ABX(LIBEG),AC(LIBEG),AD(LIBEG))
C
C Find the turning point on the product side
         CALL LCTP (LIBEG,IEND,IXX,LSAVE,ENER,STXX,VADEX,AA,ABX,AC,AD)  7/14YL92 
C
      RETURN
C
      END SUBROUTINE lcstx
C
C***********************************************************************
C LCTP
C***********************************************************************
      SUBROUTINE lctp(IBEG,IEND,IXX,NSD,E,SXX,VX,A,B,C,D)
      use common_inc
      use perconparam
      use rate_const
C
C Find the turning point in the adiabatic potential in the product 
C    valley 
C  
C     CALLED BY: LCSTX
C
C     CALL: CUBIC2
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION VX(NSD),A(NSD),B(NSD),C(NSD),D(NSD)
      DIMENSION RRT(3),AIRT(3),SRT(4)
C
      LOGICAL LSET
C
C
      LSET = .FALSE.
      IF (E.LE.VX(IXX)) THEN
         SXX = SSUBI(IXX)
      ELSE
C Scan grid for E < VX(IS)
         IF (IEXOG.EQ.0) THEN
             IS = NSD
10           CONTINUE
               IF (LSET.OR.IS.LE.IBEG) GO TO 15
               IS = IS - 1
               LSET = (E - VX(IS)) .LT. 0.0D0
               GO TO 10
15           CONTINUE
         ELSE
             IS = 1
20           CONTINUE
               IF (LSET.OR.IS.GE.IEND) GO TO 25
               IS = IS + 1
               LSET = (E - VX(IS)) .LT. 0.0D0
               GO TO 20
25           CONTINUE
             IS = IS - 1
         ENDIF
      ENDIF
      IF (LSET) THEN       
C Get turning point in spline fit from solving the cubic equation
         S1 = SSUBI(IS)
         S2 = SSUBI(IS+1)
         DD = D(IS) - E
         CALL CUBIC2 (A(IS),B(IS),C(IS),DD,NREAL,RRT,AIRT)
         NRT = 0
C Check for root is within bounds
         IF (NREAL.GE.1) THEN
            DO 30 I = 1, NREAL
               IF (RRT(I).LT.S1.OR.RRT(I).GT.S2) GO TO 30
               NRT = NRT + 1
               SRT(NRT) = RRT(I)
30          CONTINUE
         ENDIF
         IF (NRT.GT.0) THEN
            SXX = SRT(NRT)
         ELSE
            SXX = S2
            IF (IEXOG .EQ.-1) SXX = S1
         ENDIF
      ELSE
         SXX = SSUBI(IXX)
      ENDIF
C
      RETURN
      END SUBROUTINE lctp
C
C**********************************************************************
C  LCVCOR
C**********************************************************************
C
      SUBROUTINE lcvcor(N,MC,LC,QMX,QSML,FRQX,FRQP,XE,REDM,DEXWQM,
     *VCOR,VMX,EVX,NV,NN,IE)
c     SUBROUTINE lcvcor(N,MC,LC,QMX,QSML,FRQX,FRQP,XE,REDM,
c    *VCOR,VMX,EVX,NV,NN,IE)
C
C  Calculate the corrections for the energy in the modes that are still 
C    within their vibrational turning points.  Note that tunneling into
C    excited states products is considered here.
C
C     Called by:
C               LCGTH                                                   0708JC00
C
C     Call:
C          LCVMOD
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER MC(NV),LC(NV)
C QM   - the projections of vector from the MEP at s to the LCG path at
C        zeta along the normal coordinates.
C QSML - normalized projections of the tunneling vector along the 
C        normal modes.
C FRQX - frequencies in each mode.
C XE   - anhamonicity in each mode.
      DOUBLE PRECISION QMX(NV),QSML(NV),FRQX(NV),XE(NV)
      DIMENSION DEXWQM(NV)
C
c     ckcal=627.5095d0
      SUM = 0.0D0
      EVX = 0.0D0
      VMX = 0.0D0
      DO 10 J = 1,NN
         IF (MC(J).EQ.0) THEN
C Get vibrational energy and potential for a mode J
            FRQ = FRQX(J)                                                TTTT
            QM = QMX(J)                                                  TTTT
            XE1 = XE(J)                                                  TTTT
            LG = LC(J)                                                   TTTT
            CALL LCVMOD(FRQ,QM,REDM,XE1,LG,EV,VM)                        TTTT
            IF (N.GT.0) THEN
C For excited state add in excitation energy in that mode
               DEV = 0.0D0                                              7/14YL92
c              IF(FRQX(J).GT.0) DEV = DBLE(N)*(FRQX(J)*QSML(J))**2/FRQP 7/14YL92
               IF(FRQX(J).GT.0) DEV = DEXWQM(J)
               EV = EV + DEV
            END IF
C Contribution to the correction term is the difference in the
C    vibrational energy and the potential at QMX(J)
            SUM = SUM + EV - VM
            EVX = EVX + EV
            VMX = VMX + VM
         ENDIF
   10 CONTINUE
      VCOR = SUM
C
      RETURN
      END SUBROUTINE lcvcor
C
C**********************************************************************
C  LCVIB
C**********************************************************************
      SUBROUTINE lcvib (NV,EVM,ZPE)
      use common_inc
      use perconparam
C
C     PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C     Called by:
C               LCSET
C     Call:
C          FUNCTION EVIB
C
C Calculate the vibrational energy EVM for each mode in state NV.
C     nv is the quantum numer and evm is the energy.
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
      DIMENSION EVM(NVIBM)
C
      AFLAG = '   '
      N3M7 = NF(5)            
      ZPE = 0.0D0
C IS is used in EVIB only for WKB.  Currently WKB is not allowed with
C    LCG so it is just a dummy.
      IS = 2
C-----------------------------------------------------------------------6/30YL91
C  MODE(I) is assigned in the subroutine LCSET according to which MEP range
C  STP0 or STP1 belong to
C
      IF (LGS(5).GE.21) AFLAG = 'SET'                                   6/30YL91
      DO 10 I = 1, N3M7
        IF (AFLAG.EQ.'SET') LGS(5) = MODE(I)
        IKBM = I
        IF((LGS(5).EQ.0.OR.LGS(5).EQ.9) .AND. FREQ(I).LE.0.0D0) THEN    7/14YL92
           EVM(I) = 0.0D0
        ELSE
           FRQDMY = FREQ(I)                                             7/14YL92
           ANHDMY = ANHRM(I)                                            7/14YL92
           Y00DMY = Y00(I)                                              7/14YL92
           EVM(I) = EVIB(FRQDMY,ANHDMY,NV,Y00DMY,IS)                    7/14YL92
           ZPE = ZPE+EVM(I)
        END IF
   10 CONTINUE
C
       IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20                           6/30YL91
      RETURN
      END SUBROUTINE lcvib 
C
C**********************************************************************
C  LCVMOD
C**********************************************************************
      SUBROUTINE lcvmod(FREQ,QM,REDM,XE,LC,EVIBM,VMD)
C
C Calculates vibrational energy EVIBM and potential VMD at QM for a 
C    given mode
C
C     Called by:
C               LCVCOR
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DATA TOLER / 1.0D-06 /
C
      EVIBM = 0.0D0                                                      5/7YL93
      VMD = 0.0D0                                                        5/7YL93
      IF (FREQ.GT.0.0D0) THEN                                            5/7YL93
         IF (LC.EQ.0.OR.LC.EQ.9) THEN                                    5/7YL93
C  Harmonic
            EVIBM = 0.5D0*FREQ
            VMD = FREQ*QM
            VMD = 0.5D0*REDM*VMD*VMD
         ELSE IF(LC.EQ.1.OR.LC.EQ.2.AND.XE.GE.-TOLER) THEN
C  Morse
            DE = FREQ/(4.0D0*XE)
            BT = SQRT(2.0D0*REDM*FREQ*XE)   
            VMD = DE*(EXP(-BT*QM) - 1.0D0)**2
            EVIBM = FREQ*(1.0D0 - XE/2.0D0)/2.0D0
         ELSE IF (LC.EQ.2 .AND. XE.LT.-TOLER) THEN
C  Quadratic- quartic
            K4 = XE/24.0D0
            VMD = 0.5D0*REDM*(FREQ*QM)**2 + K4*QM**4
            EVIBM = EBND(FREQ,XE,0.D0,REDM)                             0113GL92
         ENDIF
      ENDIF                                                             0507YL93
      continue
      RETURN
      END SUBROUTINE lcvmod
C
C**********************************************************************
C  LCXINT
C**********************************************************************
      DOUBLE PRECISION FUNCTION lcxint(DOTIX,VEFIX,E,NQ,IX,IOG)         0708JC00
C
C     Called by: LCGTH                                                  0708JC00
C
C     Call FUNCTION XDOTP
C
C This function is used to compute the integrand in the phase integral
C    along the adiabatic section only.  The integrand is weighted by
C    the projection of the LCG path along the true tunneling path in the
C    adiabatic region - the MEP.                                        0708JC00
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DOTIX(NQ),VEFIX(NQ)                                     0708JC00
C
      VEF=VEFIX(IX)                                                     0708JC00
      DOT=DOTIX(IX)                                                     0708JC00
      DIF = VEF - E
      XINTGL = 0.0D0
      IF (DIF.GT.0.0D0) THEN
         TERM = SQRT(DIF)
         TERM = DOT*TERM                                                0708JC00
         IF (IOG.EQ.0) THEN                                             2/18DL91
            IF (TERM .GT. 0.0D0) XINTGL = TERM
         ELSEIF (IOG.EQ.-1) THEN                                        2/18DL91
            IF (TERM .LT. 0.0D0) XINTGL = -TERM                         2/18DL91
         ENDIF                                                          2/18DL91
      ENDIF
      LCXINT = XINTGL
      RETURN
      END function lcxint
C
C**********************************************************************
C  LCXTAU
C**********************************************************************
      DOUBLE PRECISION FUNCTION lcxtau(EVIBM,QMP,NV,NN)
      use perconparam
C
C Called by LCSET
C
C Calculate the vibrational period in the p mode
C     QMP is the projection of the LCG tunneling path along the normal
C     mode coordinates.
C     EVIBM contains the vibrational energy of each mode.
C
C   PARAMATERS MODIFIED 6/29/91
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DOUBLE PRECISION EVIBM(NV),QMP(NV)
C
      SUM = 0.0D0
      DO 10 I=1,NN
C
C     Although EVIBM(I) is set to zero if the mode is treated as H.O. and the
C     frequency is zero or imaginary, a check is added here to further ensure
C     no contribution from imaginary frequency modes               Y.-P.Liu
C
         IF (EVIBM(I).GT.0)  SUM = SUM + (2.0D0*EVIBM(I)*QMP(I))**2     7/14YL92
10    CONTINUE
      OMEGP = SQRT(SUM)
      IF (OMEGP .LE. 0) STOP
      LCXTAU = 2.0D0*PI/OMEGP
      RETURN
      END function lcxtau
C
C**********************************************************************
C  LCZCRT
C**********************************************************************
      SUBROUTINE lczcrt(IPROD,IE,IR,IZ,TPL,TPG,BCR,QM,OMEG,MC,NV,NN,ICH,
     *                  LPRYL,IU)
C
C     Called by:
C               LCGTH                                                   0708JC00
C
C Check if the current coordinate is in the adiabatic or nonadiabatic
C    region.  In the adiabatic region if QM(i) is inside the turning
C    points of mode i (TPL(i) and TPG(i)) and if sum BCR(i)*QM(i) < 1
C    (where sum is over those mode with frequencies > 0).
C    ICH=0 if in adiabatic region, MC(i) set nonzero for each mode
C    outside its turning point.
C QM - the projections of the vector from the MEP at s to the LCG path
C      at zeta along the normal coordinates.
C BCR- curvature component in each mode
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION TPL(NV),TPG(NV),BCR(NV),QM(NV),OMEG(NV)
      LOGICAL LPRYL
C
      INTEGER MC(NV)
C
      ICH = 0
      DO 20 I=1,NN
         IF (OMEG(I) .LT. 0.0D0) THEN
            MC(I) = 0 
         ELSE IF (ABS(QM(I)) .LE. ABS(TPL(I)) ) THEN                    7/14YL92
            MC(I) = 0
         ELSE
            MC(I) = 1
            ICH = 1
         END IF
 20   CONTINUE
      TM2 = 0.0D0
      DO 10 J=1,NN
         IF (OMEG(J).GT.0.0D0) TM2 = TM2 - BCR(J)*QM(J)
 10   CONTINUE
      IF (TM2 .GE. 1) THEN
         ICH = 2
      END IF
      IF (LPRYL) THEN                                                   7/14YL92
         DO 100 I = 1, NN                                                   ..
            WRITE (IU,9999) IE,IZ,I,TPL(I),QM(I),TPG(I),BCR(I),             ..
     * BCR(I)*QM(I),MC(I)                                                   ..
100      CONTINUE                                                           ..
      ENDIF                                                             7/14YL92
      RETURN
9999  FORMAT (1X,I4,1X,I4,1X,I4,1P,5(1X,E11.4),0P,1X,I2)                7/14YL92
      END SUBROUTINE lczcrt
C
C***********************************************************************
C  LIN2
C***********************************************************************
C
      SUBROUTINE lin2 (A11,A12,A21,A22,B1,B2,X1,X2)
C
C   SOLVES TWO SIMULTANEOUS LINEAR EQUATIONS IN THE MATRIX FORM AX=B
C
C     CALLED BY:
C                QQPOT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      BOT = A22-(A21*A12/A11)
      TOP = B2-(A21*B1/A11)
      X2 = TOP/BOT
      TOP = B1-A12*X2
      X1 = TOP/A11
      RETURN
      END SUBROUTINE lin2
C
C***********************************************************************
C LINMN
C***********************************************************************
C  Line search routine called by NEWT routine.
C
C  Include statements were added 6/20/91
C
C  CALLS : MNBRAK,BRENT
C
      SUBROUTINE linmn (XL,XIL,N,FRET, stpmx)
      use common_inc
      use perconparam
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      EXTERNAL F1DIM
C
      DIMENSION XL(N3TM),XIL(N3TM)
      DATA TOL /1.0D-4/
      IF(.NOT.ALLOCATED(XCOM)) ALLOCATE(XCOM(N3TM))
      IF(.NOT.ALLOCATED(XICOM)) ALLOCATE(XICOM(N3TM))
C
      NCOM = N
      DO 10 I = 1,N
         XCOM(I) = XL(IND(I))
         XICOM(I) = XIL(I)
 10   CONTINUE
C
      AX = 0.0D0
      if(stpmx .gt. 2.0d0) then                                          IR0595
         XX = 1.0D0                                                      IR0595
         BX = 2.0D0                                                      IR0595
      else                                                               IR0595
         XX = stpmx*0.5D0                                                IR0595
         BX = stpmx                                                      IR0595
      endif                                                              IR0595
      CALL MNBRAK(AX,XX,BX,FA,FX,FB,F1DIM)
      FRET = BRENT(AX,XX,BX,F1DIM,TOL,XMIN)
      DO 20 I = 1,N
        XIL(I) = XIL(I)*XMIN
 20   CONTINUE
      RETURN
      END SUBROUTINE linmn
C
C**********************************************************************
C  LOCATE
C**********************************************************************
      SUBROUTINE locate(XX,N,X,J)
      use perconparam
C
C     Called by:
C               LCSET, LCNWTN, LCGTH
C
C*    USES THE BISECTION METHOD
C     GIVEN AN ARRAY XX OF LENGTH N, AND A VALUE FOR X, A
C     VALUE J IS RETURNED SUCH THAT X IS BETWEEN XX(J) AND
C     XX(J+1).
C*    NOTE: XX MUST BE MONOTONIC.
C*    J .EQ. 0 OR N DENOTES X IS OUT OF RANGE
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DOUBLE PRECISION XX(N)
C     INITIALIZE LOWER AND UPPER LIMITS
      JL = 0
      JU = N+1
 10   IF(JU-JL.GT.1) THEN
        JM = (JU + JL)/2
        IF((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM))) THEN
          JL = JM
        ELSE
          JU = JM
        ENDIF
        GO TO 10
      ENDIF
      J = JL
      IF(J.EQ.0.OR.J.EQ.N) WRITE(FU6,*)' X OUT OF RANGE IN LOCATE'
      RETURN
      END SUBROUTINE locate
C
C***********************************************************************
C  LOCS
C***********************************************************************
C
      SUBROUTINE locs (IS,S,SS,NSMAX)
C
C     LOCS  - locate position of S in grid such that SS(IS) < or = S <
C
C  Called by:
C            AITKEN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION SS(NSMAX)                                               1215YL91
C
C  check if first point is it
C
      IF (S.LT.SS(2)) THEN
         IS = 1
      ELSEIF (S.GE.SS(NSMAX)) THEN
         IS = NSMAX
      ELSE
C
C  search for IS, initial guess passed from calling routine
C
         IS = MAX(2,IS)
         IS = MIN(IS,NSMAX-1)
C
C  left and right bounds on IS
C
         ISL = 2
         ISR = NSMAX-1
         IF (S.GE.SS(IS)) THEN
            ISL = IS
         ELSE
            ISR = IS
         ENDIF
C
C  search until left and right bounds differ by one
C        DO WHILE (ISR - ISL .GT. 1)
C
   10    CONTINUE
         IF (ISR-ISL.LE.1) GO TO 20
C
C  next guess assumes S on grid is linear with IS
C
         IIS = IS+(S-SS(IS))/(SS(IS)-SS(IS-1))
         IIS = MAX(ISL,IIS)
         IIS = MIN(IIS,ISR)
         IF (IIS.EQ.IS) THEN
            IIS = (ISL+ISR)/2
         ELSEIF (IIS.EQ.ISL) THEN
            IIS = ISL+1
         ELSEIF (IIS.EQ.ISR) THEN
            IIS = ISR-1
         ENDIF
         IS = IIS
         IF (S.GE.SS(IS)) THEN
            ISL = IS
         ELSE
            ISR = IS
         ENDIF
         GO TO 10
C
C        END DO
C
   20    CONTINUE
         IS = ISL
      ENDIF
      RETURN
      END SUBROUTINE locs
C         
C***********************************************************************
C LINMNR
C***********************************************************************
C
C
      SUBROUTINE linmnr (P,XI,N,FRET)
      use perconparam, only : n3tm
      use common_inc, only : xcom,xicom,ncom
C
C CALLED BY: FRPRMN
C
C CALL MNBRAK, BRENT AND USES F1DI (SLIGHTLY DIFFERENT FROM F1DIM)
C
C GIVEN AN N-DIMENSIONAL POINT P(1:N) AND AN N-DIMENSIONAL DIRECTION 
C XI(1:N), MOVES AND RESETS P TO WHERE THE FUNCTION FUNC(P) TAKES ON A
C MINIMUM ALONG THE DIRECTION XI FROM P, AND REPLACES XI BY THE ACTUAL
C VECTOR DISPLACEMENTTHAT P WAS MOVED. ALSO RETURNS AS FRET THE VALUE OF
C FUNC AT THE RETURNED LOCATION P. THIS IS ACCOMPLISHED BY CALLING THE 
C ROUTINES MNBRAK AND BRENT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION P(N3TM),XI(N3TM)
      EXTERNAL F1DI
      DATA TOL /1.D-4/
      if(.not.allocated(xcom)) allocate(xcom(n3tm))
      if(.not.allocated(xicom)) allocate(xicom(n3tm)) 
C
C SET UP THE COMMON BLOCK
C
      NCOM=N
      DO 10 J=1,N
         XCOM(J)=P(J)
         XICOM(J)=XI(J)
 10   CONTINUE
C
C INITIAL GUESS FOR BRACKETTING
C
      AX=0.D0
      XX=1.D0
      CALL MNBRAK (AX,XX,BX,FA,FX,FB,F1DI)
      FRET=BRENT(AX,XX,BX,F1DI,TOL,XMIN)
C
C CONSTRUCT THE VECTOR RESULTS TO RETURN
C
      DO 20 J=1,N
         XI(J)=XMIN*XI(J)
         P(J)=P(J)+XI(J)
 20   CONTINUE
      RETURN
      END SUBROUTINE linmnr
C***********************************************************************
      SUBROUTINE hindrt1 (IOP,IFRQ,NEND,IMIN,FMIHRM)
      use common_inc
      use perconparam
C***********************************************************************
c     this program calculates the reduced moment of inertial of a
c     asymmetric gyroscope
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      DIMENSION INDM(NATOMS),IDXSB(NATOMS)
      DIMENSION ITAG(2)
      DIMENSION ALPHAM(3,3),BETAM(3),BEROTM(3)
      LOGICAL   LSYM
C
C INITIALIZE DATA
C
      FMIHRM = 0.D0
      DO I = 1, NATOMS
         INDM(I) = 0
      ENDDO
C
C SET UP DATA
C
c case 1: saddle point and generalized transition states
c
      IF (IOP.GE.0) THEN      
         IEND = NARR -1
         MARR = 1
         IF (IEND.GE.1) THEN
            DO I = 1, IEND
               IF (S.GE.SRARR(I)) MARR = MARR + 1
            ENDDO
         ENDIF
         NSB = NTRNUM(5,IFRQ)                                           0521YC99
         NOSYM = NTRSIG(5,IFRQ,IMIN)                                    0521YC99
         L = 0
         DO I = 1, NEND, 3
            L = L + 1
            INDM(L) = L
         ENDDO
         NATOML = L  
         DO I = 1, NSB
             IDXSB(I) = NTRISB(5,IFRQ,I)                                0521YC99
         ENDDO
         ITAG(1) = NTRBND(5,IFRQ,1)                                     0521YC99
         ITAG(2) = NTRBND(5,IFRQ,2)                                     0521YC99
      ELSE
c
c case 2: reactants and products
c
         KOP = ABS(IOP)
         ISHFT = NF(KOP)-IFRQ+1
         NSB = NTRNUM(KOP,ISHFT)                                        0521YC99
         NOSYM = NTRSIG(KOP,ISHFT,IMIN)                                 0521YC99
         L = 0
         DO 25 I = 1, NEND, 3
            L = L + 1
            INDM(L) = IATSV(L,KOP)
25       CONTINUE
         NATOML = L              
         DO 30 I = 1, NSB
            IDXSB(I) = NTRISB(KOP,ISHFT,I)                              0521YC99
30       CONTINUE
         ITAG(1) = NTRBND(KOP,ISHFT,1)                                  0521YC99
         ITAG(2) = NTRBND(KOP,ISHFT,2)                                  0521YC99
      ENDIF
c
c  have the geom in ang
c
c      write (fu6,*) 'Calculating asymmetric rotor'
c      write (fu6,*) 'Input orientation in bohr'
      CALL TRANS(2,N3,AMASS,X,DX)  
c      DO I = 1,NATOML
c         write (6,1991) INDM(I),(X(INDM(I)*3-J),J=2,0,-1)
c      ENDDO
c      write (fu6,*) 'Gyroscope with ',NSB,' atoms.'
c      write (fu6,*) 'list ',(IDXSB(J),J=1,NSB)
c      write (fu6,*) 'rotating along ',(ITAG(J),J=1,2)
c
      LSYM = .FALSE.
      MASSGM = AMASS(3*(IDXSB(1)))
      DO I = 2, NSB
        if (AMASS(3*IDXSB(I)).EQ.MASSGM) THEN
           LSYM = .TRUE.
        endif
        MASSGM = AMASS(3*IDXSB(I))
      ENDDO
c
c      write (6,*) 'The gyrator is symmetric ',LSYM
c
c      DO I = 1,NSB 
c         write (6,1991) IDXSB(I),(X(IDXSB(I)*3-J),J=2,0,-1)
c      ENDDO 
c
      TOTM = 0.0d0
      DO I = 1,NATOML
        TOTM = TOTM + svmas(i)*CAU
      ENDDO
c      write (6,*) 'TOTM = ',TOTM
c
c     calc reduced moment of inertia
c
        call asymrmi (lsym,natoml,indm,x,amass,totm,nsb,itag,idxsb,
     >                rmi,amm,umm,BETAM,alpham,berotm)
c        rmi = rmi/(nosym*nosym)
        FMIHRM = rmi/CAU
c        write (fu6,*) ' reduced moment of innertial is ',
c     >                rmi, FMIHRM
1991  format (i5,3F15.7)
1992  format (3F15.7)
      CALL TRANS(1,N3,AMASS,X,DX)
c
      END SUBROUTINE hindrt1


C
C**********************************************************************
C  LCG34
C**********************************************************************
       SUBROUTINE lcg34 (ESV,TRNPT1,TRNPT2,PLCG3,NEMAX)
       use common_inc
       use perconparam
       use rate_const
       use potmod, only : esvjac,s0jac,s1jac
       use kintcm
C Subroutine rewritten by AFR Jun 2003
C LCT calculation of the tunneling probabilities                        0708JC00
C    Restructured 5/20/91 by BCG.  Loop over energies put inside
C    this subroutine instead of in KAPVA.  Loop over energies
C    nested inside loop over final vibrational state.  Integration
C    along tunneling path to get tunneling integral moved to subroutine
C    LCGTHE.                                                             Jun03AFR
C
C    On input:
C       ESV    : the array that stores the energies for tunneling
C       TRNPT1 : the classical turning point on the reactant side (i.e. s < 0)
C       TRNPT2 : the classical turning point on the product side (i.e. s > 0)
C                TRNPT1 and TRNPT2 were determined in PSAG previously.
C       NEMAX  : the number of energy grid
C 
C    On output:
C       PLCG3  : the sorted uniformized tunneling probability at each energy
C
C Called by:
C      KAPVA
C
C Calls: LCGNAD, LCPROB, LCSINT, LCSET, POLINT, LCGTHE AND THE FUNCTION:Jun03AFR
C        EXTRP
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 03/07/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
C
C TRNPT1, and TRNPT2 are the tunneling energies, and left and
C    right turning points.  They are set in call to PSAG from KAPVA.
C
      DIMENSION TRNPT1(NSV),TRNPT2(NSV),PLCG3(2,NSV)                    7/14YL92
C 
      DIMENSION THETA(NSV),THETT(NSV)                                   7/14YL92
      DIMENSION ESV(NSV)
C
      DOUBLE PRECISION LCGX(N3TM)                                       7/14YL92
      DIMENSION PE1(NSV),PE2(NSV),PP1(NSV),PP2(NSV)
      DIMENSION PESUM1(MAXPS,NSV),PESUM2(MAXPS,NSV)                     9/12GL91
      DIMENSION PEUNF1(MAXPS,NSV),PEUNF2(MAXPS,NSV)                     9/12GL91
      DIMENSION SINX(NSV),SINXP(NSV),COSX(NSV),COSXP(NSV)
      DIMENSION ST(NSV),STPR(NSV),TAU(NSV),TAUPR(NSV)
      DIMENSION XA(4),YA(4)
C
      DIMENSION QDX(NQD),QDW(NQD),VADIZ(NQD),VICCRT(NQD),DOTIZ(NQD),    Jun03AFR
     &VCORIZ(NQD),STIZ(NQD)                                             Jun03AFR

C Dimesion array to store the values of the excited-state adiabatic 
C potential at each save grid point.
C
      DIMENSION VADXSV(MAXPS,NSDML)                                     1/10GL91
      LOGICAL LPRFU4(NSV)
      call lcg_mem
      call grddx_mem
      call pot_mem
C   
C Restart option for LCT and ILCT1D
c
c     DO II=1,MAXPS
c      DO I=1,NSV            
c       DO J=1,NQD            
c        VEFRST(II,I,J)=0.0D0  
c       ENDDO              
c      ENDDO                
c     ENDDO
      IF(ILCRST.EQ.1) THEN   
       DO I=1,MAXPS*NSV*NQD   
        READ(FU48,*,END=333) II,I1,I2,VRST
        VEFRST(II,I1,I2)=VRST
       ENDDO                
333   ENDIF                
C
      IF(ILCT.EQ.1.AND.ILCGIT.EQ.0) WRITE (FU6,596)                     0507AR02
      IF(ILCT.EQ.2.AND.ILCGIT.EQ.0) WRITE (FU6,597)                     0507AR02
      IF(ILCT.EQ.1.AND.ILCGIT.GT.0) WRITE (FU6,598)                     Jun03AFR
      IF(ILCT.EQ.2.AND.ILCGIT.GT.0) WRITE (FU6,599)                     Jun03AFR
      WRITE (FU6,600) NG
C
      IF (IOT .EQ. 2) WRITE (FU22, 615)                                 1/10GL91
C
C Initialize arrays, etc.
C
      N3M7 = NF(5)
      SLCUTF = SSUBI(1)                                                 7/14YL92
      SRCUTF = SSUBI(LSAVE)                                             7/14YL92
      JJ = NSHLF
C Get interpolated values for BCUR at the saddle point
      DO 5 I=1,N3M7
         XP1 = SSUBI(JJ-1)
         XP2 = SSUBI(JJ+1)
         YP1 = BCUR(I,JJ-1)
         YP2 = BCUR(I,JJ+1)
         BCUR(I,JJ) = EXTRP(XP1,XP2,YP1,YP2,0.0D0)
5     CONTINUE
C Change sign on DXSV so that it is continuous and points in the 
C   product direction.  This affects the calculation of the dot product
C   in LCNWTN and the projection of the tunneling path along the MEP
C   calculated in LCXINT
C This should only be done once. So the LSIGN is set to true the first time
C around, and this will not be done again the next time lcg3 is called.
C
      IF (.NOT. LSIGN) THEN                                             0909WH94
      DO 8 J = 1,LSAVE
         IF (SSUBI(J) .GT. 0.0D0) THEN
            DO 7 I = 1,N3
               DXSV(I,J) = -DXSV(I,J)
7           CONTINUE
         END IF
8     CONTINUE
      LSIGN = .TRUE.                                                    0909WH94
      ENDIF
C Initialize arrays of tunneling energies, turning points, and probs
      DO 9 IE = 1,NEMAX
         ST(IE) = TRNPT1(IE)
         STPR(IE) = TRNPT2(IE)
      DO 9 J = 1, MAXPS
         PESUM1(J,IE) = 0.0D0
         PESUM2(J,IE) = 0.0D0
9     CONTINUE
C Add an extra point in the energy grid, i.e., the barrier maximum
      NEMX = NEMAX + 1
      ESV(NEMX) = VMAX
      ST(NEMX) = SMAX
      STPR(NEMX) = SMAX
C
C Determin the number of accessible states, using equation (10) in the 
C POLYRATE 4 paper.
C
      NPROD = 0
      IF (.NOT. LLCGG) CALL NSTATE(ESV,TRNPT1,TRNPT2,NEMAX,NPROD)       7/14YL92
C
      NPXSET = 0                                                        1024GL91
      IF (LGS(9) .LT. -1) THEN                                          1024GL91
          NPXSET = ABS(LGS(9)) - 1                                      1024GL91
          IF (NPXSET.GT.NPROD) WRITE (FU6,6061)                         7/14YL92
          NPROD  = MIN(NPXSET, NPROD)                                   7/14YL92
      ENDIF                                                             1024GL91
C
C Check the number of product states (NPROD+1) over which the calculation
C will be performed against the maximum allowed (MAXPS).
C
      IF (NPROD .GT. MAXPS) THEN                                        9/12GL91
          WRITE (FU6,606) NPROD, MAXPS                                  9/12GL91
          NPROD = MAXPS                                                 9/12GL91
      ENDIF                                                             9/12GL91
C
      WRITE (FU6,605) NPROD                                             6/24DL91
      NNPROD = NPROD + 1                                                6/24DL91
C==============================================================================
C Loop over final states.
C
C  The following actions are taken within the loop:
C  1. Set up a loop over tunneling energies.
C     Within the energy loop:
C     If the classical turning point of tunneling into ground final state is 
C     within the save grid (i.e., IFLAG .NE. 2), LCSET is called,
C     For  a given tunneling energy and a given final state, LCSET gives:
C        (1) the classical turning point on the exoergic side, (STP0 or STP1),
C        (2) the unit vector, LCGX, from STP0 to STP1,
C        (3) the tunneling path, and its length (ZETA),
C        (4) the angel between the MEP and the tunneling path (i.e., COSX and 
C            COSXP and the vibrational period (TAU and TAUPR) at STP0 and STP1,
C        (5) the potential of the final state on the MEP grid (This is done for
C            the grid points between the classical turning point (STP0 or STP1) 
C            and the end point of the grid on the exeorgic side, when the final 
C            state is not a ground state),
C        (6) IFLAG, =1, product state is not open,
C
C     If IFLAG .NE. (1 or 2), 
C        (1) LCGTH is called to evaluate the theta integral (THET and TRPTH)
C            for a given tunneling path,
C        (2) quantities at the maximum of the ground adiabatic barrier are 
C            obtained by extrapolation,
C
C  2. LCSINT is called to carry out the integration of the tunneling amplitudes 
C     over the tunneling path termini.  Constributions are neglected from 
C     regions where the adiabatic turning points are beyond the reaction 
C     coordinate range.
C
      DO 200 IIPROD = 1, NNPROD                                         6/24DL91
         IPROD = IIPROD - 1                                             6/24DL91
         WRITE (FU6, 601) IPROD
         IE0 = NEMX+1
         NELAST = NEMX
         IF (IPROD.EQ.0) NELAST = NEMX-1
         IF (LGS2(8).EQ.1.AND.INUMI(IIPROD).NE.0) THEN                  7/14YL92
            WRITE (FU41,405) IPROD                                      Jun03AFR
            WRITE (FU41,410)                                            0708JC00
            WRITE (FU42,405) IPROD                                      Jun03AFR
            WRITE (FU42,420)                                            0708JC00
            WRITE (FU43,405) IPROD                                     
            WRITE (FU43,430)                                            0708JC00
            WRITE (FU44,405) IPROD
            WRITE (FU44,440)                                            0708JC00
            WRITE (FU45,405) IPROD
            WRITE (FU45,450)                                            0708JC00
            WRITE (FU46,405) IPROD
            WRITE (FU46,460)                                            0708JC00
            WRITE (FU47,405) IPROD
            WRITE (FU47,470)                                            0708JC00
         ENDIF
C-----------------------------------------------------------------------------
c counter to know how many energies have a nonadiabatic region at a
C given nprod value
C Loop over tunneling energies
C
C  Constants within each loop:
C        ENER  : the tunneling energy
C
         DO 100 IE = 1,NELAST
            LPRYL = .FALSE.                                             7/14YL92
            LPRFU4(IE) = .FALSE.
            IF (LGS2(8).NE.0.AND.INUMI(IIPROD).NE.0) THEN               7/14YL92
            DO 1718 IDUMYL = 1, INUMI(IIPROD)                           7/14YL92
               IF ((IE.GE.ILB(IDUMYL,IIPROD)).AND.(IE.LE.IUB(IDUMYL,    7/14YL92
     *             IIPROD))) THEN
                             LPRFU4(IE) = .TRUE.                        jul03AFR
                             LPRYL=.TRUE.                               7/14YL92
               ENDIF
1718        CONTINUE                                                    7/14YL92
            ENDIF                                                       7/14YL92
            ENER = ESV(IE)
            ENGRD(IE) = ESV(IE)
            STP0 = ST(IE)
            STP1 = STPR(IE)
            THETA(IE) = 100.0
            THETT(IE) = 100.0
            IF ((IEXOG.EQ.0.AND.(STP0.GT.SLCUTF.AND.(IPROD.NE.0.OR.     7/14YL92
     *         STP1.LT.SRCUTF))).OR.(IEXOG.EQ.-1.AND.(STP1.LT.SRCUTF        ..
     *         .AND.(IPROD.NE.0.OR.STP0.GT.SLCUTF)))) THEN                  ..
               IF (IPROD.LE.NPACC(IE)) THEN                                 ..
                  IFLG = 0  
                  SINXD = 0.0D0                                             ..
                  SINXPD = 0.0D0                                            ..
                  CALL LCSET(IE,IPROD,ENER,STP0,STP1,ZETA,LCGX,SINXD,       ..
     *                       SINXPD,COSXD,COSXPD,TAUD,TAUPRD)
                  SINX(IE) = SINXD                                          ..
                  SINXP(IE) = SINXPD                                        ..
                  COSX(IE) = COSXD                                          ..
                  COSXP(IE) = COSXPD                                        ..
                  TAU(IE) = TAUD                                            ..
                  TAUPR(IE) = TAUPRD                                    7/14YL92
                  ZETAOLD=ZETA
               ELSE                                                     7/14YL92
                  IFLG = 1        
               ENDIF                                                    7/14YL92
            ELSE
               IFLG = 2
            END IF
            IF (IFLG.EQ.2) THEN
               WRITE (FU6,603) IE,ENER*CKCAL
            ELSE IF (IFLG.EQ.1) THEN
               WRITE (FU6,602) IE,ENER*CKCAL
            ELSE
C Stores information,                                                   Jun03AFR                    
               IE0 = MIN(IE,IE0)
               CALL LCGNAD(IE,IPROD,ENER,STP0,STP1,ZETA,LCGX,
     &QDX,QDW,VADIZ,DOTIZ,VCORIZ,VICCRT,STIZ,EIZ0,EIZ1,VEFIZ0,VEFIZ1,
     &IZ0IE,IZ1IE)                                 
               TPGRD(IE,1)=STP0
               TPGRD(IE,2)=STP1
               IZGRD0(IE)=IZ0IE
               IZGRD1(IE)=IZ1IE
               ZETGRD(IE)=ZETAOLD
               VEGRD(IE,1)=VEFIZ0
               VEGRD(IE,2)=VEFIZ1
               EANH(IE,1)=EIZ0
               EANH(IE,2)=EIZ1
               IE0GRD=IE0
               DO L=1,NG
                QDNAD(IE,L)=QDX(L)
                QDWNAD(IE,L)=QDW(L)
                VADGRD(IE,L)=VADIZ(L)
                DOTGRD(IE,L)=DOTIZ(L)
                VINAD(IE,L)=VICCRT(L) 
                VCGRD(IE,L)=VCORIZ(L)
                STGRD(IE,L)=STIZ(L)
               ENDDO
               
               IF (IEXOG.EQ.0) THEN                                     7/14YL92
                  STPR(IE) = STP1                                       7/14YL92
               ELSE                                                     7/14YL92
                  ST(IE) = STP0                                         7/14YL92
               ENDIF                                                    7/14YL92
            END IF
100      CONTINUE
C-----------------------End of loop over energies-------------------------------
C 
C ______________________________\\\///____________________
C Evaluates the theta integral (Jun 03 AFR)
C ------------------------------///\\\\-------------------
C
         LASTEN=NEMAX
         IPGRD=IPROD
         IF(IPROD.NE.0) LASTEN=NEMAX+1
         CALL LCGTHE(IPGRD,LASTEN,THETA,THETT,LPRFU4)
C
C If iot is equal to 2 then store the values of the excited-state 
C adiabatic potentials at each save grid point
C
C
c        write(6,*) 'IPGRD ',IPGRD
c        write(6,*) 'LASTEN',LASTEN
c        write(6,*) 'THETA ',THETA
c        write(6,*) 'THETT ',THETT
c        write(6,*) 'LPRFU4',LPRFU4
c        stop
         IF (IOT .EQ. 2) THEN
            DO 70 I1 = 1, LSAVE
                  VADXSV(IIPROD,I1) = VADEX(I1)
   70       CONTINUE
         ENDIF
C
C For IPROD=0 get quantities at last energy point (adiabatic maximum)
         IF (IPROD.EQ.0) THEN
            THETA(NEMX) = 0.0D0
            THETT(NEMX) = 0.0D0
            STPR(NEMX) = SMAX
C Get extrapolated values for tau, taupr, cosx, cosxp at the
C adiabatic maximum.
            DO 110 IF=1,4
               MM = NEMX-5+IF
               XA(IF) = ST(MM)
               YA(IF) = TAU(MM)
110         CONTINUE
            CALL POLINT(XA,YA,4,SMAX,YOUT,DYY)
            TAU(NEMX) = YOUT
            DO 111 IF=1,4
               MM = NEMX-IF
               XA(IF) = STPR(MM)
               YA(IF) = TAUPR(MM)
111         CONTINUE
            CALL POLINT(XA,YA,4,SMAX,YOUT,DYY)
            TAUPR(NEMX) = YOUT
            DO 112 IF=1,4
               MM = NEMX-5+IF
               XA(IF) = ST(MM)
               YA(IF) = COSX(MM)
112         CONTINUE
            CALL POLINT(XA,YA,4,SMAX,YOUT,DYY)
            COSX(NEMX) = YOUT
            DO 113 IF=1,4
               MM = NEMX-IF
               XA(IF) = STPR(MM)
               YA(IF) = COSXP(MM)
113         CONTINUE
            CALL POLINT(XA,YA,4,SMAX,YOUT,DYY)
            COSXP(NEMX) = YOUT
            T = EXP(-2.0D0*THETA(NEMX))
            P1 = T/(1.0D0+T)
            T = EXP(-2.0D0*THETT(NEMX))
            P2 = T/(1.0D0+T)
         END IF
C
         IE0 = IE0 + 1
         IF (IE0.GE.NEMX) THEN
            WRITE (FU6,*) ' ALL TURNING POINTS OFF GRID'
            STOP 'LCG34 1'                                              0708JC00
         ELSE
            WRITE (FU6,710) NGS0                                        0624WH94
            IF (IE0 .GT. 2) WRITE (FU6,720) IE0-1                       0624WH94
            WRITE (FU6,730)                                             0624WH94

C    For gaussian quadrature
            CALL LCSINT(ST,ESV,SINX,THETA,THETT,TAU,REDM,
     *         PE1,PE2,IE0,NEMX)
C    For trapezoidal rule
            CALL LCSINT(STPR,ESV,SINXP,THETA,THETT,TAUPR,REDM,
     *         PP1,PP2,IE0,NEMX)


c --- Add a counter for jacobian representative tunnelin path plot      0412PJ01
      iejac = 0                                                         0412PJ01


C Construct unnormalized probabilities for this state
            DO 140 IE = IE0-1,NEMX-1
               PE1(IE) = (PE1(IE) + PP1(IE))*(PE1(IE) + PP1(IE))        8/8B91
               PE2(IE) = (PE2(IE) + PP2(IE))*(PE2(IE) + PP2(IE))        8/8B91
               IF (IPROD.EQ.0) THEN                                     8/8B91
                  CSR = MAX(0.0D0,COSX(IE))                             8/8B91
                  CSP = MAX(0.0D0,COSXP(IE))                            8/8B91
                  COSTM = 0.5D0*(CSR + CSP)                             8/8B91
                  COSTM = COSTM*COSTM                                   8/8B91
                  PE1(IE) = PE1(IE) + COSTM*EXP(-2.0*THETA(IE))         8/8B91
                  PE2(IE) = PE2(IE) + COSTM*EXP(-2.0*THETT(IE))         8/8B91
               END IF                                                   8/8B91
               WRITE(FU6,608) IE,ESV(IE)*CKCAL,PE2(IE),                 0617WH94
     *                        ST(IE),STPR(IE)




c --- save representative tunneling energy and turning point            0412PJ01
c     for the Jacobian tunneling path plot                              0412PJ01
               iejac = iejac + 1                                        0412PJ01
               esvjac(iiprod, iejac) =  esv(ie)*ckcal                   0412PJ01
               s0jac(iiprod, iejac) = st(ie)                            0412PJ01
               s1jac(iiprod, iejac) = stpr(ie)                          0412PJ01
               ipdjac = iiprod                                          0412PJ01


140         CONTINUE
         END IF





C 4-point interpolation for the probability at the adiabatic maximum
         DO 150 I=1,4
            M = NEMX-5+I
            XA(I) = ESV(M)
            YA(I) = PE1(M)
150      CONTINUE
         XIN = ESV(NEMX)
         CALL POLINT(XA,YA,4,XIN,YOUT,DYY)
         PE1(NEMX) = YOUT
         DO 160 I=1,4
            M = NEMX-5+I
            YA(I) = PE2(M)
160      CONTINUE
         CALL POLINT(XA,YA,4,XIN,YOUT,DYY)
         PE2(NEMX) = YOUT
         WRITE(FU6,610) YOUT
C Add contribution to sum over final states
C
          IF (IIPROD .EQ. 1) THEN                                       9/12GL91
              DO 170 IE = 1, NEMX                                       7/14YL92
                 PESUM1(IIPROD,IE) = PE1(IE)                            9/12GL91
                 PESUM2(IIPROD,IE) = PE2(IE)                            9/12GL91
  170         CONTINUE                                                  9/12GL91
          ELSEIF (IIPROD .GT. 1) THEN                                   9/12GL91
           DO 171 IE = 1, NEMX                                          7/14YL92
            PESUM1(IIPROD,IE) = PESUM1(IPROD,IE) + PE1(IE)              9/12GL91
            PESUM2(IIPROD,IE) = PESUM2(IPROD,IE) + PE2(IE)              9/12GL91
  171      CONTINUE                                                     9/12GL91
          ENDIF                                                         9/12GL91
C
200    CONTINUE
C
C If the LCT print option (IOT) is turned on then print the following
C information at each save grid point: 
C      the value of s, the tunneling energy, the classical energy at s,
C      the ground-state adiabatic energy at s,
C      the frequency of the p mode at s, and 
C      the adiabatic energy at that product states.
C
       IF (IOT .EQ. 2) THEN
           DO 71 I = 1, LSAVE
              WRITE (FU22, 1000) SSUBI(I),
     *                 VCLAS(I)*CKCAL,WPSV(I)*AUTOCM,VADIB(I)*CKCAL,
     *              (VADXSV(IPD,I)*CKCAL,IPD=2,NNPROD)
   71      CONTINUE
       ENDIF
C===================End of loop over final states==============================
C
C Call LCPROB to get the uniformized LCG probabilities from the primitive
C probabilities in PESUM1 and PESUM2
C
        CALL LCPROB (PESUM1, PEUNF1, NEMX, NNPROD)                      7/14YL92
        CALL LCPROB (PESUM2, PEUNF2, NEMX, NNPROD)                      7/14YL92
C  Write out all the uniformized probabilities
C
      IF (NNPROD.GT.1) THEN                                             7/14YL92
         IF (IOT .GT. 0) THEN
            WRITE (FU22, 613)
            DO 221 I = 1, NNPROD
               IPRD = I - 1
               WRITE (FU22, 612) IPRD
               DO 221 J = 1, NEMAX                                      7/14YL92
                  WRITE (FU22, 700) J,ESV(J)*CKCAL,PEUNF1(I, J),        0623WH94
     *                            PEUNF2(I,J)
  221       CONTINUE
         ENDIF
C
C    Call LCGSRT to sort the uniformized probabilities
C
         CALL LCGSRT (PEUNF1, PE1, NEMAX, NNPROD)                       7/14YL92
         CALL LCGSRT (PEUNF2, PE2, NEMAX, NNPROD)                       7/14YL92
      ELSE                                                              7/14YL92
         DO 225 I = 1, NEMAX                                            7/14YL92
            PE1(I) = PEUNF1(1,I)                                        7/14YL92
            PE2(I) = PEUNF2(1,I)                                        7/14YL92
225      CONTINUE                                                       7/14YL92
      ENDIF
C
C  Pack the array PLCG3 with the final sorted, uniformized probabilities
C  and write these probabilities.
C
        DO 230 I = 1, NEMAX
               PLCG3(1, I) = PE1(I)
               PLCG3(2, I) = PE2(I)
  230   CONTINUE
C
      RETURN
C
596   FORMAT (/1X,'==============  LCG3 CALCULATION ==============')    0708JC00
597   FORMAT (/1X,'==============  LCG4 CALCULATION ==============')    0708JC00
598   FORMAT (/1X,'====== Interpolated LCG3 CALCULATION ==========')    0507AR02
599   FORMAT (/1X,'====== Interpolated LCG4 CALCULATION ==========')    0507AR02
600   FORMAT (//2X,'Number of quadrature points used in THETA ',        0708JC00
     *'integral =', I5)                                                 0708JC00
601   FORMAT (//1X,'*****  PRODUCT STATE = ',I2,' *****',/)             0624WH94
C    * T21, '1/(1+EXP(2*THETA))',T53,'TURNING PTS',T78,'COSINE FACTORS',
C    * T105,'VIB. PERIOD',/,T5,'IE',T11,'Energy',T21,'QUAD',T34,'TRAP',
C    * T43,3('  React.-side','   Prod.-side'))
602   FORMAT (2X,'at IE = ',I4,'   E = ',F10.4,' kcal',                 0624WH94
     *    5X,'product state is not open')
603   FORMAT (2X,'at IE = ',I4,'   E = ',F10.4,' kcal',                 0624WH94
     *    5X,'turning point is off grid')
604   FORMAT (2X,I4,F10.5,1P,2E13.5,0P,6F13.3)
605   FORMAT (2X,'The highest excited state considered in LCG ',        0708JC00
     *       'calculation is Npmax = ',I2)
606   FORMAT (//,5('*'),' WARNING ',5('*'),/2X,
     *'The highest excited state, Npmax, for this calculation is',/2X,
     *'larger than the maximum, MAXPS, allowed for this executable.',
     * /2X,'Npmax, MAXPS = ',2I3,/,2X,'Npmax will be set equal to ',
     * 'MAXPS - 1',//)
6061  FORMAT (//,10('*'),2X,
     * 'The number of accessible states is ',I2,/,2X,                   7/14YL92
     * 'which is less than the number that is requested.')              7/14YL92
710   FORMAT(/2X,'No. of gauss quadrature points used in integration',  0624WH94
     */2X,'of tunneling amplitudes over the tunneling termini, NGS0 = ',
     * I3)
720   FORMAT(/2X,'Turning points off grid or product state not open ',
     * 'for IE < ',I3)
730   FORMAT(/1X,' IE','   E(kcal/mol)  ','  primitive P(E)',2X,
     *'  Turning Points',/)
608   FORMAT(1X,I3,1X,F12.4,3X,1P,E14.4,2X,0P,2X,2F8.3)
C       /,' COS2X is the contribution at the adiabatic turning point.'
C    * ,/,' PE1 is the primitive tunneling probability using Gauss-',
C    * 'Legendre quadrature for THETA.',/,' PE2 is the primitive',
C    * ' tunneling probability using the trapezoidal rule for THETA.',/,
C    * T5,'IE',T11,'Energy',T21,'COS2X',T34,'PE1',T47,'PE2')
700   FORMAT(2X,I4,F10.5,1P,3E13.5)
609   FORMAT(2X,'PE1 at the adiabatic maximum from 4-point fit',E12.4)  1201WH92
610   FORMAT(/2X,'P(E) at the adiabatic maximum from 4-point fit',E12.4)1201WH92
611   FORMAT(/,' **THE PROBABILITY BEFORE UNIFORMIZATION**')            1027WH92
612   FORMAT(' ** Npmax = ', I5,/,T5,'IE',T10,'Energy',T20,'Gaussian',  1201WH92
     * T32,'Trapezoidal')
613   FORMAT(/,3X,'UNIFORMIZED PROBABILITY ', //)
614   FORMAT(/,3X,'SORTED UNIFORMIZED PROBABILITIES ', //,              1201WH92
     *  T5,'IE',T10,'Energy',T20,'Gaussian',T32,'Trapezoidal')
615   FORMAT(/,3X,'EXTRA INFORMATION FROM THE LCT CALCULATION :',       1201WH92
     * /3X,'s    = the value of s on the MEP at the ith grid point',
     * /3X,'VMEP = the classical energy at s (kcal/mol)',
     * /3X,'Va^G = the ground-state adiabatic energy at s (kcal/mol)',  06/96ELC
     * /3X,'wp   = the frequency of mode p at s',
     * /3X,'Vag  = the adiabatic energy at (s,np) (kcal/mol)',//,
     * 3X,' s(a0) ',6X,'VMEP',8X,'wp',5X,'Va^G',5X,'Vag',/)             06/96ELC
 1000  FORMAT(1X,F9.4,2F10.2,1X,6F8.2,:,/,(39X,5F8.2))                  0624WH94
C Text for units fu41-fu47 (7/14YL92) modified by AFR (put in by JCC, 0708JC00)
  405  FORMAT(///,1X,' DATA FOR IPROD = ',I3)
  410  FORMAT(//,1X,'Contribution from the adiabatic and non-adiabatic',
     * ' regions:',//,3X,    
     * 'IE        : the index of the energy grid point',/,3X,
     * 'Energy    : the tunneling energy',/,3X,
     * 'I         : the index of the grid point on the tunneling path',
     * /,3X,'Iadi      : the theta integrand',/,3X,
     * 'W         : the gauss quadrature weighting factor',/,3X,
     * '(*) boundary of the adiabatic/non-adiabatic region',
     * /,/,4X,'IE',3X,'Energy',5X,'I',6X,'Iadi',8X,'W')
  420  FORMAT(//,1X,'Contribution from the non-adiabatic region:',//,3X, 
     * 'IE        : the index of the energy grid point',/,3X,
     * 'I         : the index of the grid point on the tunneling path',
     * /,3X,'ZETA      : the length of the tunneling path at I',/,3X,
     * 'Vc        : the classical energy',/,3X,
     * 'VCORR     : the zero-point energy correction term',/,3X,
     * 'ECORR     : the anharmonicity correction term',/,3X,
     * 'VICC      : the interpolated correction',/,3X,    
     * 'VEFF      : the effective potential',/,3X,        
     * 'VAD       : the vibrationally adiabatic potential',
     * /,/,4X,'IE',3X,'IZ',3X,'ZETA',6X,'Vc',                           0824JC00
     * 6X,'VCorr',4X,'ECorr',4X,'VICC',5X,'VEFF',5X,'VAD') 
  430  FORMAT(//,1X,'Value of theta before return to LCG',/,/,3X,
     * 'IE        : the index of the energy grid point',/,3X,
     * 'Energy    : the tunneling energy',/,3X,
     * 'Theta     : the theta integral',/,3X,   
     * 'I0,I1     : the left and the right grid boundary indexes of',
     * /,14X,' the nonadiabatic region',/,3X,
     * 'zcrt0 and zcrt1: the left and the right boundary of the ',    
     * 'nonadiabatic region',/,/,4X,'IE',3X,'Energy',5X,'Theta',6X,        
     * 'I0',4X,'I1',5X,'zcrt0', 6X,'zcrt1')                                                
  440  FORMAT(/,/,3X,
     * 'IE       : the index of the energy grid point',/,3X, 
     * 'ENERGY   : the tunneling energy',/,3X,
     * 'ZETA     : the length of the tunneling path',/,3X,
     * 'Vmax     : the maximum of the potential',/,3X,
     * 'ZETMAX   : the location of the maximum potential',/,3X,
     * '(*) There is a non-adiabatic region at this tunneling energy', 
     * /,/,4X,'IE',4X,'Energy',6X,'ZETA',5X,'Vmax',6x,'ZETMAX')
  450  FORMAT(/,/,3X,
     * 'IE       : the index of the energy grid point',/,3X,
     * 'Energy   : the tunneling energy',/,3X,
     * 'I        : the index the grid point on the tunneling path',
     * /,3X,'S0       : the left termini on the MEP',/,3X,
     * 'S        : the s value on the MEP to which the point',/,14X,
     * 'on the tunneling path corresponds',/,3X,
     * 'S1       : the right termini on the MEP',
     * /,/,4X,'IE',3X,'Energy',5X,'I',7X,'S0',9X,'S',10X,'S1')  
  460  FORMAT(/,/,3X,
     * 'IE       : the index of the energy grid point',/,3X,
     * 'Energy   : the tunneling energy',/,3X,
     * 'I        : the index of the grid point on the tunneling path',
     * /,3X,'Vag      : the adiabatic potential',/,3X,
     * 'COS      : the cosine between the tunneling path and the MEP',
     * /,/,4X,'IE',3X,'Energy',5X,'I',5X,'Vag',8X,'COS')
  470  FORMAT(/,/,3X,'IE       : the index of the energy grid point',  
     * /,3X,'Energy   : the tunneling energy',/,3X,'I        : the ',   
     * 'index of the grid point on the tunneling path',/,3X,            
     * 'Mode     : the index of normal mode',/,3X,'left tp  : the left',
     * ' turning point of the mode',/,3X,'right tp : the right turning',
     * ' point of the mode',/,3X,'QM       : the projection of the',    
     * ' point on the tunneling',/,14X,'path onto each eigenvector',/,  
     * 3X,'Bm       : the curvature',/,3X,'ICH   = 0:  the point is',  
     * ' between the vibrational turning point',/,11X,'1: otherwise',  
     * //,3X,'IE',4X,'I',2X,'Mode',3X,'left tp',7X,'QM',7X,'right TP', 
     * 7X,'Bm',8X,'Bm*Qm',3X,'ICH')
C
      END SUBROUTINE lcg34         
C
C**********************************************************************
C  LCGNAD
C**********************************************************************
      SUBROUTINE lcgnad(IE,IPROD,ENER,STP0,STP1,ZETA,LCGX,QDX,
     &QDW,VADIZ,DOTIZ,VCORIZ,VICCRT,STIZ,EIZ0,EIZ1,VEFIZ0,VEFIZ1,
     &IZ0,IZ1)
      use common_inc
      use perconparam
      use rate_const
      use kintcm
C    Rewritten by AFR, Jun 2003
C    Modified jun/jul 00 (AFR), put in by JCC to include LCG4
C    Modified may 02 by AFR to include ILCG
C
C    Calculate the tunneling integral for energy ENER and the product
C    state IPROD.  The adiabatic turning points STP0 and STP1, the path
C    length ZETA, and path vector LCGX are set up in LCSET called from
C    LCG34.  
C
C On input:
C    IE     : the index of the energy grid point
C    IPROD  : the final state
C    ENER   : the tunneling energy
C    STP0   : the classical tunneling point on the reactant side
C    STP1   : the classical tunneling point on the product side
C    LCGX   : the unit vector from STP0 to STP1
C
C On output:
C    THET   : the theta integral evaluated by gauss quadrutures
C    TRPTH  : the theta integral evaluated by the trapezoidal rule
C
C Called by:
C      LCG34
C
C Calls: GAUSQD, LCGIT, LCTV4, LCNORM, LCNWTN, LCPATH, LCVCOR, LCZCRT, LOCATE,
C        INTRPL, VICLCG, ENERG AND THE FUNCTIONS:
C        LCXINT, ZOCVCL
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 01/07/91
C
      use dxiz, only : iz0old,iz1old
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)     
C
C
      DOUBLE PRECISION LCGX(N3TM)
C
      DIMENSION XLIN(N3TM),QM(NVIBM),X1X(N3TM),QPRJ(NVIBM)
      DIMENSION QDX(NQD),QDW(NQD)
      DIMENSION MC(NVIBM)                                               7/14YL92
C
      DIMENSION TPLX(NVIBM), TPGX(NVIBM), DEXWQM(NVIBM)
C
c      DIMENSION VADSCR(1,1)                                             1/13GL92
c      EQUIVALENCE (VAD, VADSCR(1,1))                                    1/13GL92
      DIMENSION VADIZ(NQD),VCORIZ(NQD)                                  0708JC00
      DIMENSION VADIZ0(NQD),VADIZ1(NQD)                                 0708JC00
      DIMENSION DOTIZ(NQD),DOTP0(NQD),DOTP1(NQD)                        0708JC00
      DIMENSION VICCRT(NQD)
      DIMENSION STIZ(NQD)                                               0708JC00
      DIMENSION vadscr(1,1),vcldum(1,1)

      DIMENSION VCLX(NQD)
c
C ***
      DIMENSION XX(N3TM),RXX0(N3TM),RXX1(N3TM),DXX(N3TM)  
C     The above line was commented because variables are not used.      0423TA02
C ***
      LOGICAL IENONAD
C IENONAD is true if there is a nonadiabatic reagion at a  give
C         tunneling energy
      IENONAD=.FALSE.
c     vadscr(1,1)=vad
c     write(6,*) 'VAD =', VAD
C
C RX0 = geometry on MEP at left adiabatic turning point
C RX1 = geometry on MEP at right adiabatic turning point
C
      N3M7 = NF(5)                                                      1021WH92
      NSD = LSAVE
C
C Define the origin for the zeta parameter at the left turning point.
C GAUSQD is used to determine the zeta values and the weights
C for the gauss integration over zeta. NG is the no. of quadrature
C     points.
      ZETA0 = 0.0D0
      ZETA1 = ZETA
      CALL GAUSQD(ZETA0,ZETA1,QDX,QDW,NG)
C
C=============================================================================
C Loop over two directions of integration - from reactant (IR=1) and
C product (IR=2) sides (One of them is in the exeorgic direction).
C
C  First, initialize variables and arrays for the first direction.
C
      IR = 1
      STPX = STP0
      IDIRZ = 1
      IZ = 0
      ZETOLD = ZETA0
C ***
50    CONTINUE
C-----------------------------------------------------------------------------
C Loop over grid points in adiabatic region
C IZ is the index of the zeta parameter on the tunneling path.
C
C For each IZ, the following actions are taken:
C    1. Finds the s value, i.e., STPX, that zeta(IZ) corresponds to. This is 
C       done in LCNWTN.  In LCNWTN, the vector from STPX to zeta(IZ), i.e.,
C       XLIN is also calculated.
C    2. Interpolates vibrational eigenvectors. This is done in INTRPL.
C    3. Calculates the projection (QM) of XLIN onto the eigenvector of each 
C       mode. This is done in LCNORM.
C    4. If STPX > STP1 for the first direction, or STPX < STP0 for the 2nd 
C       direction, zeta(IZ) is considered to be in the non-adiabatic region
C       (ICH = 2).  In this case, the loop over IZ ends.  Otherwise, further 
C       actions (5-9) are taken-----
C    5. Interpolates frequencies and anharmonicities.  This is done by several 
C       calls to INTRPL.
C    6. Evaluate the turning point of each mode.  If direction IR starts from
C       the endoergic side, the interpolated frequencies is used to calculate 
C       the turning points.  Otherwise, the energy partitioned into each mode
C       is first interpolated among the grid (TPLX) set up previously in LCSET, 
C       and then the turning point is calculated using the interpolated energy
C       and the frequency of the mode.
C       Note that VADEX,TPLX are only computed for grid points from LIBEG to 
C       NSDEX+LIBEG-1 (=IEND).  Use the offset LIBEG to insure only the
C       computed values are used in the interpolation. 
C    7. Interpolates the curvatures.
C    8. The point zeta(IZ) is checked by the conditions of Eqn(30a) or Eqn(30b) 
C       in the POLYRATE 4 paper to determin whether zeta(IZ) is within the 
C       adiabatic region. If it is within the adiabatic region, ICH is set to 0,
C       otherwise, ICH is set to 1.  This is done in LCZCRT. MC(i) set nonzero 
C       for each mode outside its turning point.
C    9. Interpolates and stores the adiabatic potential at STPX.  
C       If direction IR starts 
C       from the exeorgic side, the adiabatic potential of state IPROD is 
C       interpolated among a grid (VADEX) evaluated previously in LCSET. 
C       Otherwise, the adiabatic ground state potential is used for 
C       interpolation. It also stores the corrcetion potential at every
C       adiabatic grid point
C   10. If ICH = 1, the loop over IZ for the direction IR ends.  
C
C If adiabatic regions overlap the default is to take
C the minimum of the two adiabatic potentials. If ILCT > 1 LCG4 option is on.
C
C All the units poly.fu41-fu46 are printed at the end of the subroutine.
C
C
60       CONTINUE
            IZ = IZ + IDIRZ
            ZETA = QDX(IZ)
            CALL LCNWTN(IE,XLIN,N3TM,UAL,STPX,ZETA,IR,IERR,             7/14YL92
     *        LCGX,ZETOLD)
C Store stpx and the dot product between the normalized gradient along the MEP
C and the unit vector along the linear path
            STIZ(IZ)=STPX                                               0708JC00
            IF (IR.EQ.1) THEN                                           0708JC00
             DOTP0(IZ) = XDOTP(DX,LCGX,N3)                              0708JC00
            ELSE                                                        0708JC00
             DOTP1(IZ) = XDOTP(DX,LCGX,N3)                              0708JC00
            ENDIF                                                       0708JC00
C
            IF (IERR .NE. 0) STOP 'LCGTH 1'    
C
            CALL LOCATE(SSUBI,NSD,STPX,JJX)
            CALL INTRPL(COFSV,SSUBI,COFX,STPX,JJX,NSDM,NSD,N3TM,N3TM,
     *       N3,N3M7)
            CALL LCNORM(COFX,XLIN,QM,N3TM,N3,NVIBM,N3M7)
C
C seek for the nonadiabatic region
C
            IF (IR.EQ.1.AND.STPX.GT.STP1 .OR. IR.EQ.2.AND.STPX.LT.STP0) 8/1B91
     *        THEN                                                      8/1B91
               ICH = 2                                                  8/1B91
            ELSE                                                        8/1B91
               MARR = 1                                                 7/14YL92
               IF (LGS(5).GT.21) THEN                                       ..
                  NARL = NARR-20-1                                          ..
                  DO 63 IARR = 1, NARL                                      ..
                     IF (STPX.GE.SRARR(IARR)) MARR = MARR + 1               ..
63                CONTINUE                                                  ..
               ENDIF                                                        ..
               DO 67 IFRQ = 1,N3M7                                          ..
                  MODE(IFRQ) = MODETS(MARR,IFRQ)                            ..
67             CONTINUE                                                 7/14YL92
               CALL INTRPL(WETS,SSUBI,FREQ,STPX,JJX,NSDM,NSD,1,NVIBM,1, 7/14YL92
     *                     N3M7)                                        7/14YL92
C
C Since LCG is not supported for anharmonicity options other than the hindered
C rotator option, the following part is commented to save computation time.
C In future, if LCG method is compatible with other anharmonicity, this part
C should be uncommented, and should be modified if necessary.
C                                                               Y.-P. Liu
C
C            IF (LGS(5) .NE. 0) THEN                                    7/14YL92
C               CALL INTRPL(XETS,SSUBI,ANHARM,STPX,JJX,NSDM,NSD,1,NVIBM,7/14YL92
C     *          1,N3M7)                                                7/14YL92
C               CALL INTRPL(Y0TS,SSUBI,Y00,STPX,JJX,NSDM,NSD,1,NVIBM,1, 7/14YL92
C     *          N3M7)                                                  7/14YL92
C            END IF                                                     7/14YL92
             IF (IPROD.EQ.0.OR.(IEXOG.EQ.0.AND.IR.EQ.1).OR.             7/29DL91
     *          (IEXOG.EQ.-1.AND.IR.EQ.2)) THEN                         7/29DL91
               DO 70 IDMY = 1, N3M7                                     7/14YL92
                  FQDMY = FREQ(IDMY)                                        ..
                  IF (FQDMY.LE.0.D0) THEN                                   ..
                     TPLX(IDMY) = -1.0D+10                                  ..
                  ELSE                                                      ..
                     TPLX(IDMY) = -1.0D0/SQRT(REDM*FREQ(IDMY))              ..
                  ENDIF                                                     ..
                  TPGX(IDMY) = -1.D0*TPLX(IDMY)                         7/14YL92
                  DEXWQM(IDMY) = 0.0D0
70             CONTINUE
             ELSE
               JJXEX = JJX-LIBEG+1
               CALL INTRPL(TPLXX(1,LIBEG),SSUBI(LIBEG),TPLX,STPX,JJXEX, 7/14YL92
     *         NSDM,NSDEX,1,NVIBM,1,N3M7)                                   ..
               DO 75 IDMY = 1, N3M7                                         ..
                  FQDMY = TPLX(IDMY)                                        ..
                  IF (FQDMY.LE.0.D0) THEN                                   ..
                     TPLX(IDMY) = -1.0D+10                                  ..
                     DEXWQM(IDMY) = 0.0D0
                  ELSE                                                      ..
                     DEXWQM(IDMY) = TPLX(IDMY)-0.5D0*FREQ(IDMY)
                     TPLX(IDMY) = -1.0D0*SQRT(TPLX(IDMY)/                   ..
     *                           (0.5D0*REDM*(FREQ(IDMY)**2)))              ..
                  ENDIF                                                     ..
                  TPGX(IDMY) = -1.D0*TPLX(IDMY)                         7/14YL92
75             CONTINUE
             END IF

            CALL INTRPL(BCUR,SSUBI,BM1,STPX,JJX,NSDM,NSD,1,NVIBM,1,N3M7)
             CALL LCZCRT(IPROD,IE,IR,IZ,TPLX,TPGX,BM1,QM,FREQ,MC,NVIBM,
     *       N3M7,ICH,LPRYL,FU47)                                       7/14YL92
C
C Compute the correction term for the effective potential in the
C adiabatic region and at the adiabatic/nonadiabatic boundary:
C    1) Get geometry X1 on MEP at s=STPX 
C    2) Compute the normalized projections of the vector (X1-RX0) along the
C       mormal modes(COFX) 
C    3) Compute the frequency of the p mode 
C    4) Compute the effective-potential correction term  
C (moved here by AFR, jun 2000)
             IF (IPROD.EQ.0.OR.(IEXOG.EQ.0.AND.IR.EQ.1).OR.             7/29DL91
     *          (IEXOG.EQ.-1.AND.IR.EQ.2)) THEN                         7/29DL91
               CALL LCVCOR(0,MC,MODE,QM,QPRJ,FREQ,FRQP,ANHRM,         
     *                     REDM,DEXWQM,VCOR,VMDX,EVIBMX,NVIBM,N3M7,IE)
             ELSE
               CALL INTRPL(GEOM,SSUBI,X1X,STPX,JJX,NSDM,NSD,1,N3TM
     *                     ,1,N3)                                       7/29DL91
               CALL LCPROJ(COFX,RX0,X1X,QPRJ,N3TM,N3,NVIBM,N3M7)        7/29DL91
               FRQP=0.0D0                                               7/29DL91
               DO 80 J = 1, N3M7                                        7/29DL91
                 IF(FREQ(J).GT.0) FRQP = FRQP+(FREQ(J)*QPRJ(J))**2      7/14YL92
   80          CONTINUE                                                 7/29DL91
               FRQP = SQRT(FRQP)                                        7/29DL91
               CALL LCVCOR(IPROD,MC,MODE,QM,QPRJ,FREQ,FRQP,ANHRM,         
     *                     REDM,DEXWQM,VCOR,VMDX,EVIBMX,NVIBM,N3M7,IE)
             END IF                                                     7/29DL91
C End of moved chunk. Store the correction term 
             VCORIZ(IZ)=VCOR  

C Compute the vibrationally adiabatic potential 
               IF (IPROD.EQ.0.OR.(IEXOG.EQ.0.AND.IR.EQ.1).OR.           7/14YL92
     *            (IEXOG.EQ.-1.AND.IR.EQ.2)) THEN
                  CALL INTRPL(VADIB,SSUBI,VADSCR,STPX,JJX,NSDM,NSD,     7/14YL92
     *                        1,1,1,1)                                  7/14YL92
                  CALL INTRPL(VCLAS,SSUBI,VCLDUM,STPX,JJX,NSDM,NSD,
     *                        1,1,1,1)                            
               ELSE                                                     7/14YL92
                  CALL INTRPL(VADEX(LIBEG),SSUBI(LIBEG),VADSCR,STPX,    7/14YL92
     *                       JJXEX, NSDM,NSDEX,1,1,1,1)                 7/14YL92
C Compute the classical potential
c                 CALL INTRPL(VCLAS,SSUBI,VCLDUM,STPX,JJXEX,NSDM,NSD,
c    *                        1,1,1,1)                            
               ENDIF                                                    7/14YL92
               VAD = VADSCR(1,1)
               VCLX(IZ)=VCLDUM(1,1)+VMDX
C
C Store the vibrationally adiabatic potential in both directions        0708JC00
            
              IF(IR.EQ.1) THEN                                          0708JC00
               VADIZ0(IZ)=VAD                                           0708JC00
              ELSE                                                      0708JC00
               VADIZ1(IZ)=VAD                                           0708JC00
              ENDIF                                                     0708JC00

              IF (ICH.EQ.0) THEN                                        0708JC00
                IF(IR.EQ.1) THEN                                        0708JC00
                  IZ0=IZ+1                                              0708JC00
                  if (iz0.gt.ng) iz0=ng                                 1117BE05
                ELSE                                                    0708JC00
                  IZ1=IZ-1                                              0708JC00
                  if (iz1.eq.0) iz1=1                                   1117BE05
                ENDIF                                                   0708JC00
C
                IF ((IDIRZ .GT. 0 .AND. IZ .LT. NG).OR.       
     *           (IDIRZ .LT. 0 .AND. IZ .GT. 1)) GO TO 60  
C
            ENDIF                                                       0708JC00
C
         ENDIF                                                          0708JC00
C---------------End of the loop over IZ for the direction IR -----------------
C
C If IR = 1, the related variables and arrays are initialized for loop over IZ
C for the direction IR = 2.  Also if only part of the tunneling path is within
C the adiabatic region, the correction term to the potential is evaluated 
C in the adiabatic region and at the boundary for IR = 1 accordint 
C to Eqn(33) in the POLYRATE 4 paper.  
C The correction term to the potential is calculated in LCVCOR.
C
         IF(IR.EQ.1)THEN
C Initialize variables for loop from product side
            IR = 2
            STPX = STP1
            IDIRZ = -1
            IZ = NG+1
            ZETOLD = ZETA1                                              05/08B91
C
C Start the loop over IZ for the direction IR = 2
C
         GO TO 50
      END IF
C
C=================== End of the loop over IR =================================
C
C Defines the vibrationally adiabatic potential in different situations 0708JC00
C If IVAVG=0 considers the minimum of the VAD otherwise interpolates
C Also computes the interpolated correction to the potential if any
      IF(LZOC) THEN                                                     Jun03AFR
        DO 85 IZ=1,NG                                                   Jun03AFR
          VICCRT(IZ)=ZOCVCL(STIZ(IZ))                                   Jun03AFR
85       CONTINUE                                                       Jun03AFR
      ENDIF                                                             Jun03AFR
      IF(IZ1.LT.IZ0) THEN                                               Jun03AFR
        DO 86 IZ=1,IZ1-1                                                Jun03AFR
          VADIZ(IZ)=VADIZ0(IZ)                                          Jun03AFR
          DOTIZ(IZ)=DOTP0(IZ)                                           Jun03AFR
86      CONTINUE                                                        Jun03AFR
        DO 87 IZ=IZ0+1,NG                                               Jun03AFR
          VADIZ(IZ)=VADIZ1(IZ)                                          Jun03AFR
          DOTIZ(IZ)=DOTP1(IZ)                                           Jun03AFR
87      CONTINUE                                                        Jun03AFR
        IF(IVAVG.EQ.0) THEN                                             Jun03AFR
          DO 88 IZ=IZ1,IZ0                                              1117BE05
            VADIZ(IZ)=MIN(VADIZ0(IZ),VADIZ1(IZ))                        Jun03AFR
            DOTIZ(IZ)=MIN(DOTP0(IZ),DOTP1(IZ))                          Jun03AFR
88        CONTINUE                                                      Jun03AFR
        ELSE                                                            Jun03AFR
          DO 89 IZ=IZ1,IZ0                                              Jun03AFR
C Here TERM1 =(IZ-IZ1)/(IZ0-IZ1) is wrong because IZ, IZ1, IZ0 are only
C indexes. It is commented by J. Zheng and R. M. Paneda Aug. 2008.
C           TERM1=(IZ-IZ1)/(IZ0-IZ1)                                    Jun03AFR
            TERM1=(QDX(IZ)-QDX(IZ1))/(QDX(IZ0)-QDX(IZ1))                0805JZ08
            VADIZ(IZ)=VADIZ1(IZ)+TERM1*(VADIZ0(IZ)-VADIZ1(IZ))          Jun03AFR
            DOTIZ(IZ)=DOTP1(IZ)+TERM1*(DOTP0(IZ)-DOTP1(IZ))             Jun03AFR
89        CONTINUE                                                      Jun03AFR
        ENDIF                                                           Jun03AFR
      ELSE                                                              Jun03AFR
        DO 90 IZ=1,IZ0                                                  Jun03AFR
          VADIZ(IZ)=VADIZ0(IZ)                                          Jun03AFR
          DOTIZ(IZ)=DOTP0(IZ)                                           Jun03AFR
90      CONTINUE                                                        Jun03AFR
        DO 91 IZ=IZ1,NG                                                 Jun03AFR
          VADIZ(IZ)=VADIZ1(IZ)                                          Jun03AFR
          DOTIZ(IZ)=DOTP1(IZ)                                           Jun03AFR
91      CONTINUE                                                        Jun03AFR
      ENDIF                                                             Jun03AFR
                                                                        Jun03AFR
C Maximum of the vibrational adiabatic potential                        Jun03AFR
C The grid point is stored in IZMX. This is the first point to search for
C a LCG4 nonadiabatic region 
      VLCMAX=0.0D0                                                      Jun03AFR
      DO 92 IZ=1,NG                                                     Jun03AFR
        VTEMP=VADIZ(IZ)                                                 Jun03AFR
        IF(VTEMP.GT.VLCMAX) THEN                                        Jun03AFR
          ZETAMX=QDX(IZ)                                                Jun03AFR
          IZMX=IZ                                                       Jun03AFR
          VLCMAX=VTEMP                                                  Jun03AFR
        ENDIF                                                           Jun03AFR
92    CONTINUE                                                          Jun03AFR
C
      IZ0OLD=IZ0                                                        Jun03AFR
      IZ1OLD=IZ1                                                        Jun03AFR

C ===================
C Nonadiabatic region 
C ===================
      IF (IZ1 .GE. IZ0) THEN                                            Jun03AFR
C IVEF sums NVEF to the last tunneling path that has a LCG3 nonadiabatic region
       IVEF=IE+NVEF                                                     Jun03AFR
C
       IENONAD=.TRUE.                                                   Jun03AFR
       LGSIC6 = LGSIC(6)                                                Jun03AFR
       VOCCLC = VZOCLC                                                  Jun03AFR
       DO 130 IZ=IZ0,IZ1                                                Jun03AFR
        VICCRT(IZ)=0.0D0                                                Jun03AFR
130    CONTINUE                                                         Jun03AFR
       NDIMMY = IZ1 - IZ0 + 1                                           Jun03AFR
       IF (LGSIC6.NE.0 .AND. LZOC)                                      Jun03AFR
     *      CALL VICLCG(LGSIC6,ZETA1,STP0,STP1,VOCCLC,QDX,VICCRT,       0824YC98
     *                  NDIMMY,IZ0,IVIC)                                0806WH93
C
C === Looking for a LCG4 nonadiabatic region in a LCG3 adiabatic region ===
C
       CALL LCTV4(ILCT,QDX,VICCRT,VADIZ,VCORIZ,ENER,EIZ0,EIZ1,          Jun03AFR
     &VEFIZ0,VEFIZ1,IE,IZ0,IZ1,IENONAD,IPROD)                           Jun03AFR
      ELSE                                                              Jun03AFR
C === Only if NVEF > 0
C
       IF(ILCT.GT.0.AND.IVEF.GE.IE) THEN                                Jun03AFR
        IZM0=IZMX                                                       Jun03AFR
        IZM1=IZMX+1                                                     Jun03AFR
        CALL LCTV4(ILCT,QDX,VICCRT,VADIZ,VCORIZ,ENER,EIZ0,EIZ1,         Jun03AFR
     & VEFIZ0,VEFIZ1,IE,IZM0,IZM1,IENONAD,IPROD)                        Jun03AFR
C IENONAD is true if a LCG4 nonadiabatic region is found                Jun03AFR
        IF(IENONAD) THEN                                                Jun03AFR
         IZ0=IZM0                                                       Jun03AFR
         IZ1=IZM1                                                       Jun03AFR
         WRITE(FU6,133) ENER*CKCAL,IZ0,IZ1                              Jun03AFR
        ELSE                                                            Jun03AFR
C
C This line is commented by J.Zheng and R. Meana Paneda Aug. 2008 to 
C remove some confusing output when LCG3 is used. 
C        WRITE(FU6,134) IE,ENER*CKCAL                                   Jun03AFR
         IF(ILCT.GT.1) WRITE(FU6,134) IE,ENER*CKCAL                     0807JZ08
        ENDIF                                                           Jun03AFR
       ENDIF                                                            Jun03AFR
      ENDIF                                                             Jun03AFR
      QDIZ0=QDX(IZ0)
      QDIZ1=QDX(IZ1)
133   FORMAT(/,1X,'There is a LCG4 nonadiabatic region at tunneling ',  Jun03AFR
     &'energy = ',f8.4,' kcal/mol',                                     Jun03AFR
     &/,4X,'Grid points: IZ0 = ',I4,'  IZ1 = ',I4,/)                    Jun03AFR
134   FORMAT(/,1X,'At IE = ',I4,'  E = ',f8.4,' kcal/mol ',             Jun03AFR
     &'LCG4 nonadiabatic region not found')                             Jun03AFR

      
C _____________________________________________________________________________
C
C
      RETURN
C
      END
C
C *************************************************************************
C     SUBROUTINE LCTV4
C *************************************************************************
C     Written by AFR, jun 2000. Put in by JCC, aug 2000.
      subroutine lctv4(ilcg,qdx,viccrt,vadiz,vcoriz,ener,eiz0,eiz1,
     &vefiz0,vefiz1,ie,iz0,iz1,ienon,iprod)
      use common_inc
      use perconparam
      use kintcm; use rate_const
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c     On input
c      ilcg = 1 does LCG3 
c      ilcg = 2 does LCG4
c      qdx  = number of quadrature points
c      viccrt = interpolated potential 
c      vadiz  = vibrational adiabatic potential
c      vcoriz = correction to the effective potential
c      ener   = energy
c      ie     = grid point of ener
c     On output
c      xintsv = value of the theta integral at iz
c      iz0 = new value of the boundary adiabatic/non-adiabatic region (R side)
c      iz1 = new value of the boundary adiabatic/non-adiabatic region (P side)
c
      DOUBLE PRECISION LCVEF
      integer, intent(in) :: iprod
c
      DIMENSION VADIZ(NQD)
      DIMENSION QDX(NQD),VICCRT(NQD),VCORIZ(NQD)
      LOGICAL IENON
C
      EDUM0=0.0D0
      EDUM1=0.0D0
      EIZ0=0.0D0
      EIZ1=0.0D0
      ii=iprod + 1
C 
      IZ0B=IZ0
      IZ1B=IZ1
C
C Compute the effective potential at the LCG3 boundaries
      VEF(IZ0) = 
     & LCVEF(II,IE,QDX,VICCRT,VCORIZ,EDUM0,EDUM1,IZ0,IZ0,IZ1,0)         0228AR05
      VEF(IZ1) = 
     & LCVEF(II,IE,QDX,VICCRT,VCORIZ,EDUM0,EDUM1,IZ1,IZ0,IZ1,0)         0228AR05
      VEF0=VEF(IZ0)
      VEF1=VEF(IZ1)
      VAD0=VADIZ(IZ0)
      VAD1=VADIZ(IZ1)
      VEFIZ0=VXPR(IZ0)
      VEFIZ1=VXPR(IZ1)
      IF(ILCSTR.EQ.1) WRITE(FU49,*) II,IE,IZ0,VEFIZ0                    0228AR05
      IF(ILCSTR.EQ.1) WRITE(FU49,*) II,IE,IZ1,VEFIZ1                    0228AR05
c
C Anharmonicity
      IF(ILCG.GT.1) THEN
       IF(IENON) THEN
        IF(VEF0.LT.VAD0) EIZ0=VAD0-VEF0
        IF(VEF1.LT.VAD1) EIZ1=VAD1-VEF1
       ELSE
        IF(VEF0.GT.VAD0.OR.VEF1.GT.VAD1) THEN
         IENON=.TRUE.
        ELSE
         RETURN
        ENDIF
       ENDIF
      ENDIF

C In LCNAD VEF = VAD at the boundaries
      EBPR(IZ0)=EIZ0
      EBPR(IZ1)=EIZ1
      VEF(IZ0)=VEF0+EIZ0
      VEF(IZ1)=VEF1+EIZ1
C
C NOTE: from now on the values of IZ0 and IZ1 may change if LCT4 is on
C
C if vef0 > vad0 extends the nonadiabatic region (R side)
      IF(ILCG.GT.1.AND.VEF0.GT.VAD0) THEN
c      write(fu42,*) 'Region I where VEF(IZ0) > VAD(IZ0)'
       DO 30 IZ=IZ0B-1,1,-1
        VEF(IZ) = 
     &   LCVEF(II,IE,QDX,VICCRT,VCORIZ,EDUM0,EDUM1,IZ,IZ,IZ,0)
        IF(VEF(IZ).LE.VADIZ(IZ)) THEN
         GOTO 31
        ELSE
         IZ0=IZ
         VEFIZ0=VXPR(IZ0)
         IF(ILCSTR.EQ.1) WRITE(FU49,*) II,IE,IZ0,VEFIZ0                 0228AR05
        ENDIF
30     CONTINUE
31     CONTINUE
       IF(ILCSTR.EQ.1) WRITE(FU49,*) II,IE,IZ0,VEFIZ0                   0228AR05
      ENDIF
        
C if vef1 > vad1 extends the nonadiabatic region (P side)
      IF(ILCG.GT.1.AND.VEF1.GT.VAD1) THEN
c      write(fu42,*) 'Region III where VEF(IZ1) > VAD(IZ1)'
       DO 40 IZ=IZ1B+1,NG
        VEF(IZ) = 
     &   LCVEF(II,IE,QDX,VICCRT,VCORIZ,EDUM0,EDUM1,IZ,IZ,IZ,0)          0228AR05
        IF(VEF(IZ).LE.VADIZ(IZ)) THEN
         GOTO 41
        ELSE
         IZ1=IZ
         VEFIZ1=VXPR(IZ1)
         IF(ILCSTR.EQ.1) WRITE(FU49,*) II,IE,IZ1,VEFIZ1                 0228AR05
        ENDIF
40     CONTINUE
41     CONTINUE
       IF(ILCSTR.EQ.1) WRITE(FU49,*) II,IE,IZ1,VEFIZ1                   0228AR05
      ENDIF

      END

C
C *************************************************************************
C     FUNCTION LCVEF
C *************************************************************************
C     Rewritten by AFR, jun 2003. 
      DOUBLE PRECISION FUNCTION lcvef(ii,ie,qdx,viccrt,vcoriz,eiz0,eiz1,
     & iz,izr,izp,iopt)
      use common_inc
      use perconparam
      use kintcm
      use rate_const
C
C Function that calculates the potential of the nonadiabatic region
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION QDX(NQD),VICCRT(NQD),VCORIZ(NQD)                        0507AR02
C
      ZETA=QDX(IZ)
      VICC=VICCRT(IZ)
      ZCRT0=QDX(IZR)
      ZCRT1=QDX(IZP)
      TERM1=ZCRT1-ZCRT0
      VCOR0=VCORIZ(IZR)
      VCOR1=VCORIZ(IZP)
      CALL LCPATH(X,ZETA)
C
C Calculate classical energy allowing restart and interpolation
C
      IF(IOPT.NE.0) THEN
       NSP1D=NSPLIC(II)
       IF((ILCGIT.EQ.0).OR.(ILCGIT.EQ.2).OR.
     & (ILCGIT.EQ.1.AND.(IZP-IZR.LE.NSP1D))) THEN
        IF(ILCRST.EQ.1) THEN
         IF(VEFRST(II,IE,IZ).NE.0.0D0) THEN
          V=VEFRST(II,IE,IZ)
         ELSE
          CALL EHOOK(1,iproc)
         ENDIF
        ELSE
         CALL EHOOK(1,iproc)
        ENDIF
       ELSE
        CALL SPL(ZETIC,VLCIC,NSP1D,ZETA,V,7,TENSION)
       ENDIF
      ELSE
       IF(ILCRST.EQ.1) THEN
        IF(VEFRST(II,IE,IZ).NE.0.0D0) THEN
         V=VEFRST(II,IE,IZ)
        ELSE
         CALL EHOOK(1,iproc)
        ENDIF
       ELSE
        CALL EHOOK(1,iproc)
       ENDIF
      ENDIF
C
      VXPR(IZ)=V
C
      V = V + VICC 
C
      IF (TERM1.LT.1.0D-10) THEN 
        VBCORR = 0.5D0*(VCOR1 + VCOR0)
        EBCORR = 0.5D0*(EIZ1 + EIZ0)  
      ELSE                             
        ZTERM = (ZETA - ZCRT0)/TERM1
        VBCORR = VCOR0 + ZTERM*(VCOR1 - VCOR0)
        EBCORR = EIZ0 + ZTERM*(EIZ1 -EIZ0)
      ENDIF 
      VCORPR(IZ)=VBCORR
      EBPR(IZ)=EBCORR
      LCVEF = V + VBCORR + EBCORR 
C
      RETURN
      END FUNCTION LCVEF
C
C**********************************************************************
C  LCGRD
C**********************************************************************
      SUBROUTINE lcgrd(XS,YS,ZS,NXI,NYI,LASTEN,IIPGRD)
      use common_inc
      use perconparam
      use kintcm, only : ilcstr,ilcrst
      use rate_const 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Subroutine written by AFR, Jun 2003
C Builds the grid for the 2D spline 

      DIMENSION QDX(NQD)
      DIMENSION XS(NSV),YS(NQD),ZS(NSV,NQD)
      DIMENSION NXARR(NSV)
     
      NSD=LSAVE
      ZETA0=0.0D0

C Grid over energies
       ETOP=ENGRD(LASTEN)
       EGR=ENGRD(IE0GRD)
       IE0=IE0GRD
       LENER=LASTEN-IE0+1
       NXINEW=NXI
       STEPEN=REAL(LENER)/REAL(NXI-1)
       DO I=1,NXI
        RKI=STEPEN*(I-1)
        NXARR(I)=MIN(INT(RKI-0.5),INT(RKI+0.5))+IE0
        IF(I.GT.1.AND.I.LT.NXI) THEN
         IF(NXARR(I-1).EQ.NXARR(I)) NXARR(I)=NXARR(I)+1
        ELSE IF(I.EQ.NXI) THEN
         IF(NXARR(I-1).EQ.NXARR(I)) NXINEW=NXI-1
        ENDIF
       ENDDO  
       NXI=NXINEW

       WRITE(FU6,400) NXI,NYI
 400  FORMAT (//,2X,'USING A 2D GRID TO CALCULATE LCT KAPPA FACTORS ',/,
     &4X,'GRID POINTS ALONG ENERGIES : ',I4,/,
     &4X,'GRID POINTS ALONG THE TUNNELING PATH : ',I4,//)
C
       DO I=1,NXI
        IE=NXARR(I)
        XS(I)=(ENGRD(IE)-EGR)/(ETOP-EGR)
        IZ0=IZGRD0(IE)
        IZ1=IZGRD1(IE)
        EIZ0=EANH(IE,1)
        EIZ1=EANH(IE,2)
        VCOR0=VCGRD(IE,IZ0) 
        VCOR1=VCGRD(IE,IZ1)
        ZETA1=ZETGRD(IE)
C
        STP0=TPGRD(IE,1)
        CALL LOCATE(SSUBI,NSD,STP0,JJX)
        CALL INTRPL(GEOM,SSUBI,RX0,STP0,JJX,NSDM,LSAVE,1,N3TM,1,N3)
C
        STP1=TPGRD(IE,2)
        CALL LOCATE(SSUBI,NSD,STP1,JJX)
        CALL INTRPL(GEOM,SSUBI,RX1,STP1,JJX,NSDM,LSAVE,1,N3TM,1,N3)
C
        DO IZ=1,NG
         QDX(IZ)=QDNAD(IE,IZ)
        ENDDO
        ZCRT0=QDX(IZ0)
        ZCRT1=QDX(IZ1)
        TERM1=ZCRT1-ZCRT0
        DO J=1,NYI
         YS(J)=REAL(J-1)/REAL(NYI-1)
         YSS=YS(J)*NG
         IZ=MIN(INT(YSS-0.5),INT(YSS+0.5))
         IF(IZ.EQ.0) IZ=1
         YS(J)=QDX(IZ)/ZETGRD(IE)
         IF(IZ.EQ.1) YS(J)=0.0d0                                           
         IF(IZ.EQ.0) YS(J)=0.0D0
         IF(IZ.GE.NG) YS(J)=1.0d0
         IF(IZ.EQ.NG-1) THEN
          YS(J)=1.0d0
          IZ=NG
         ENDIF

         IF(IZ.GE.IZ0.AND.IZ.LE.IZ1) THEN
          Z=QDX(IZ)
          ZTERM=(Z-ZCRT0)/TERM1
          CALL LCPATH(X,Z)
          IF(ILCRST.EQ.1) THEN
           IF(VEFRST(IIPGRD,IE,IZ).NE.0.0D0) THEN
            V=VEFRST(IIPGRD,IE,IZ)
           ELSE
            CALL EHOOK(1,iproc)
           ENDIF
          ELSE
           CALL EHOOK(1,iproc)
          ENDIF
          IF(ILCSTR.EQ.1) WRITE(FU49,*) IIPGRD,IE,IZ,V
          VICC=VINAD(IE,IZ)
          VBCORR = VCOR0 + ZTERM*(VCOR1 - VCOR0)
          EBCORR = EIZ0 + ZTERM*(EIZ1 -EIZ0)
          ZS(I,J)= V+VICC+VBCORR+EBCORR
         ELSE
          ZS(I,J)=VADGRD(IE,IZ)
         ENDIF
         IF(IZ.EQ.0.OR.IZ.GE.NG) ZS(I,J)=ENGRD(IE)
        ENDDO
       ENDDO
       return
       end SUBROUTINE lcgrd

C
C**********************************************************************
C  LCGTR
C**********************************************************************
      SUBROUTINE lcgtr(IE,IZ,LASTEN,XTRF,YTRF,ITRF)
      use common_inc
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
C Subroutine written by AFR, Jun 2003
c
      ETOP=ENGRD(LASTEN)
      EGRD=ENGRD(IE0GRD)
      EDIF=ETOP-EGRD
      XDUM=ENGRD(IE)
      ZETA=ZETGRD(IE)
      YDUM=QDNAD(IE,IZ)
      IF(ITRF.EQ.0) THEN
       XTRF=(XDUM-EGRD)/EDIF
       YTRF=YDUM/ZETA
      ELSE
       XTRF=XDUM
       YTRF=YDUM
      ENDIF
      RETURN
      END SUBROUTINE lcgtr
C
C**********************************************************************
C  LCGTHE
C**********************************************************************
      SUBROUTINE lcgthe(IPGRD,LASTEN,THEG,THET,LPRFU4)
      use common_inc
      use perconparam
      use kintcm
      use rate_const
      use dxiz, only : iz0old,iz1old
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Subroutine written by AFR, Jun 2003
C Evaluates: a)the potential along the nonadiabatic region 
C            b)the theta integral
C Called by LCG34
      DOUBLE PRECISION LCVEF,LCXINT
      DIMENSION TRPWT(NQD),QDX(NQD),VICCRT(NQD),VCORIZ(NQD),
     &VADIZ(NQD),DOTIZ(NQD),QDW(NQD),STIZ(NQD)
      DIMENSION XS2D(NSV),YS2D(NQD),ZS2D(NSV,NQD)
      DIMENSION XINTSV(NQD)
      DIMENSION THET(NSV),THEG(NSV)
      LOGICAL IENONAD,LPRFU4(NSV)
     
      NSD=LSAVE
      IIPGRD=IPGRD+1

      DO IE=1,NSV
       THEG(IE)=0.0D0
       THET(IE)=0.0D0
      ENDDO

      IF(ILCGIT.EQ.2) THEN
       NXI=NSPLIX(IIPGRD)
       NYI=NSPLIY(IIPGRD)
       LENER=LASTEN-IE0GRD+1
       IF(LENER.LT.NXI) NXI=LENER
       CALL LCGRD(XS2D,YS2D,ZS2D,NXI,NYI,LASTEN,IIPGRD)
      ENDIF

      DO IE=IE0GRD,LASTEN
       IENONAD=.FALSE.
       DO IZ=1,NG
        QDX(IZ)=QDNAD(IE,IZ)
        QDW(IZ)=QDWNAD(IE,IZ)
        VICCRT(IZ)=VINAD(IE,IZ)
        VCORIZ(IZ)=VCGRD(IE,IZ)
        VADIZ(IZ)=VADGRD(IE,IZ)
        DOTIZ(IZ)=DOTGRD(IE,IZ)
        STIZ(IZ)=STGRD(IE,IZ)
       ENDDO
       DO I = 2,NG-1
         TRPWT(I) = 0.5D0*(QDX(I+1)-QDX(I-1))
       ENDDO
       TRPWT(1) = 0.5D0*(QDX(2)-QDX(1))
       TRPWT(NG) = 0.5D0*(QDX(NG)-QDX(NG-1))
       IZ0=IZGRD0(IE)
       IZ1=IZGRD1(IE)
       EIZ0=EANH(IE,1)
       EIZ1=EANH(IE,2)
       VEF(IZ0)=VEGRD(IE,1)
       VEF(IZ1)=VEGRD(IE,2)
       ENER=ENGRD(IE)
       XV1=ENER
c Maximum of the potential
       VLCMAX=0.0D0
       DO IZ=1,NG
        VTEMP=VADIZ(IZ)
        IF(VTEMP.GT.VLCMAX) THEN
         ZETAMX=QDX(IZ)
         IZMX=IZ
         VLCMAX=VTEMP
        ENDIF
       ENDDO
c
       IF(IZ1.GE.IZ0) THEN 
        IENONAD=.TRUE.
        ZETA0=0.0D0
        ZETA1=ZETGRD(IE)
c
        STP0=TPGRD(IE,1)
        CALL LOCATE(SSUBI,NSD,STP0,JJX)
        CALL INTRPL(GEOM,SSUBI,RX0,STP0,JJX,NSDM,LSAVE,1,N3TM,1,N3)
C
        STP1=TPGRD(IE,2)
        CALL LOCATE(SSUBI,NSD,STP1,JJX)
        CALL INTRPL(GEOM,SSUBI,RX1,STP1,JJX,NSDM,LSAVE,1,N3TM,1,N3)

        IF(ILCGIT.EQ.1) CALL LCGIT(IIPGRD,IE,IZ0,IZ1,QDX)
C
        DO IZ=IZ0,IZ1
          IF(ILCGIT.LE.1) THEN
            IF(IZ.GE.IZ0.OR.IZ.LE.IZ1) THEN                             0228AR05
             VEF(IZ) =
     &        LCVEF(IIPGRD,IE,QDX,VICCRT,VCORIZ,EIZ0,EIZ1,IZ,IZ0,IZ1,1)
            ENDIF
           XINTSV(IZ)=0.0D0
           DIF = VEF(IZ) - ENER
            IF(DIF.GT.0.0D0) XINTSV(IZ) = SQRT(DIF)
          ELSE
           YV1=QDX(IZ) 
           CALL LCGTR(IE,IZ,LASTEN,XV1,YV1,0) 
           CALL SPL2D(NXI,NYI,XS2D,YS2D,ZS2D,XV1,YV1,ZV1,NSV,NQD)
           XINTSV(IZ)=0.0D0
           VEF(IZ)=ZV1
           vadiz(iz)=zv1
           DIF = VEF(IZ) - ENER
             IF(DIF.GT.0.0D0) XINTSV(IZ) = SQRT(DIF)
          ENDIF
        ENDDO
        DO IZ=1,IZ0-1       
         XINTSV(IZ) = LCXINT(DOTIZ,VADIZ,ENER,NQD,IZ,0)
        ENDDO
        DO IZ=IZ1+1,NG
         XINTSV(IZ) = LCXINT(DOTIZ,VADIZ,ENER,NQD,IZ,0)  
        ENDDO
       ELSE
        DO IZ=1,NG
         XINTSV(IZ) = LCXINT(DOTIZ,VADIZ,ENER,NQD,IZ,0)
        ENDDO
       ENDIF
C 
       DO IZ=1,NG                      
        THEG(IE) = THEG(IE) + QDW(IZ)*XINTSV(IZ)  
        THET(IE) = THET(IE) + TRPWT(IZ)*XINTSV(IZ)
       ENDDO                                
C
       THEG(IE) = SQRT(2.0D0*REDM)*THEG(IE)
       THET(IE) = SQRT(2.0D0*REDM)*THET(IE)
C _____________________________________________________________________________
C
C ==================
C Write fu41 to fu46
C ==================
C Section written by AFR, jun 2003
      IF (LPRFU4(IE)) THEN
       DO 300 IZ=1,NG
        IF(IZ.EQ.IZ0.OR.IZ.EQ.IZ1) THEN
         WRITE (FU41,411) IE,ENER*CKCAL,IZ,XINTSV(IZ)*    
     &     SQRT(2.0D0*REDM),QDW(IZ)
         IF(IENONAD) WRITE(FU42,420) IE,IZ,QDX(IZ),VXPR(IZ)*CKCAL,
     &    VCORPR(IZ)*CKCAL,EBPR(IZ)*CKCAL,
     &    VICCRT(IZ)*CKCAL,VEF(IZ)*CKCAL,VADIZ(IZ)*CKCAL
        ELSE
         WRITE (FU41,410) IE,ENER*CKCAL,IZ,XINTSV(IZ)*    
     &     SQRT(2.0D0*REDM),QDW(IZ)
         IF(IZ.GT.IZ0.AND.IZ.LT.IZ1) THEN
          IF(IZ.LT.IZ0OLD.OR.IZ.GT.IZ1OLD) THEN 
           WRITE(FU42,420) IE,IZ,QDX(IZ),
     &     VXPR(IZ)*CKCAL,VCORPR(IZ)*CKCAL,EBPR(IZ)*CKCAL,
     &     VICCRT(IZ)*CKCAL,VEF(IZ)*CKCAL
          ENDIF
         ELSE
          WRITE (FU45,450) IE,ENER*CKCAL,IZ,STP0,STIZ(IZ),STP1 
          WRITE (FU46,460) IE,ENER*CKCAL,IZ,VADIZ(IZ)*CKCAL,DOTIZ(IZ)
         ENDIF
        ENDIF
 300   CONTINUE
       ZCRT0=QDX(IZ0)
       ZCRT1=QDX(IZ1)
       IF(IZ1.EQ.NG+1) ZCRT0=0.0D0
       IF(IZ0.EQ.0) ZCRT1=0.0D0
       WRITE (FU43,430) IE,ENER*CKCAL,THET(IE),IZ0,IZ1,QDX(IZ0),QDX(IZ1) 
       IF(IENONAD) THEN
        WRITE (FU44,441) IE,ENER*CKCAL,ZETA1,VLCMAX*CKCAL,ZETAMX
       ELSE
        WRITE (FU44,440) IE,ENER*CKCAL,ZETA1,VLCMAX*CKCAL,ZETAMX
       ENDIF
      ENDIF
C
C Write the nonadiabatic potential in unit fu49 for a future restart
C
      DO IZ=1,NG
C       IF(IENONAD.AND.ILCSTR.EQ.1) THEN
        IF(IENONAD.AND.ILCSTR.EQ.1.AND.ILCGIT.LE.1) THEN                0228AR05
        IF(VXPR(IZ).GT.0.0D0) WRITE(FU49,*) IIPGRD,IE,IZ,VXPR(IZ)
       ENDIF
      ENDDO

C
 410  FORMAT (2X,I4,F10.5,1X,I4,2X,1P,2(1X,E10.3))                       
 411  FORMAT (2X,I4,F10.5,1X,I4,'*',1X,1P,2(1X,E10.3))
 420  FORMAT (2x,I4,1X,I4,1X,f8.5,6(1x,f8.4))
 430  FORMAT (2X,I4,F10.5,1X,1P,E10.3,0P,2(2X,I4),2(1X,F10.5))   
 440  FORMAT (2X,I4,1X,4(2X,F8.4))
 441  FORMAT (2X,I4,'*',4(2X,F8.4)) 
 450  FORMAT (2X,I4,F10.5,1X,I4,3(1X,F10.5))                          
 460  FORMAT (2X,I4,F10.5,1X,I4,2(1X,F10.5))                   
 497  FORMAT (//,2X,'SQUARE GRID OF POINTS USED BY THE 2D SPLINE ', 
     &'UNDER TENSION',/)
 498  FORMAT (2X,'GRID POINTS ALONG ENERGIES : ',I4,2X,
     &'GRID POINTS ALONG THE TUNNELING PATH : ',I4)
 499  FORMAT (/,'     I     J     TUN. ENER.   TUN. PATH      VAG ',/)
 500  FORMAT (2x,I4,2X,I4,3(5x,f8.5))

      ENDDO

      IF(ILCGIT.EQ.2) THEN 
      WRITE(FU42,497)
      WRITE(FU42,498) NXI,NYI
      WRITE(FU42,499)
       DO I=1,NXI
        DO J=1,NYI
         WRITE(FU42,500) I,J,XS2D(I),YS2D(J),ZS2D(I,J)
        ENDDO
       ENDDO
      ENDIF
C

      RETURN
      END SUBROUTINE lcgthe

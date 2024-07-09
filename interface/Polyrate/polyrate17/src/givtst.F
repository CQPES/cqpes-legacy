C***********************************************************************
C  BCALCP 
C***********************************************************************
C
      SUBROUTINE bcalcp(redm,ncoord,lorder,nfreq)
      use gtst
      use perconparam
C
C     CALCULATES PAGE-McIVER CURVATURE FACTORS BK,F AND KAPPA
C
C     CALLED BY:  
C                 KAPPAS
C     CALLS    :  
C                 DERV, MUSCCD
C     
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      dimension bkf(nfre,10),dkap(10),dvs1(maxcar)
      dimension nfreq(10)
C
C  Compute BK,F for bound modes at TS
C
      CALL DERV(ncoord,lorder)
C
      DO 30 K= 1, NFREQ(5)
         SUM = 0.0D0
         DO 20 I = 1, NCOORD
C
C Page's eqn 49, but plug in eqn 19   
C
            SUM = SUM - V3(I)*COFSP(I,K)
20       CONTINUE
C
         BKF(K,5) = SUM
30    CONTINUE
c
c  Compute BK,F for bound modes at extra point
c
      DO 35 I = 1, NCOORD                                    
         DVS1(I)  = 0.0D0                                
      DO 35 J = 1, NCOORD                            
C
C see Page's paper eqn 48, modified the common list dervcm
C
        DVS1(I) = DVS1(I) - FORC1(I,J)*DX(6,J)/XNORM
C         DVS1(I) = DVS1(I) - FORC1(I,J)*DX(6,J)
   35 CONTINUE
c
      DO 40 K= 1, NFREQ(6)
         SUM = 0.0D0
         DO 50 I = 1, NCOORD
            SUM =  SUM + DVS1(I)*COFEX1(I,K)
50       CONTINUE
C
         BKF(K,6) = -SUM*DSIGN(1.0D0,DS)
40    CONTINUE
c
c  Calculate curvature kappa for printing
c
      DO 70 L = 5, 5+LORDER
      SUM = 0.0D0
      DO 65 K = 1, NFREQ(L)
         SUM = SUM+BKF(K,L)*BKF(K,L)
65    CONTINUE
      DKAP(L) = SQRT(SUM)
70    CONTINUE
c
      WRITE (FU6,99)
      WRITE (FU6,100)
      WRITE (FU6,110) 
      WRITE (FU6,120)
      DO K= 1, NFREQ(6)
         WRITE(FU6,130) K,BKF(K,5),BKF(K,6)
      ENDDO
      WRITE (FU6,140) DKAP(5),DKAP(6)
      WRITE (FU6,150)
      WRITE (FU6,160)
      WRITE (FU6,99)
c
      if (iscsag.eq.0) then
         CALL MUSCCD(dkap,bkf,redm,lorder,nfreq)
      else
         CALL MUSC(bkf,redm,lorder,nfreq)
      endif

 99   FORMAT(/,1X,78(1H-))
100   FORMAT(1x,'As a check, the curvature components are calculated')
110   FORMAT(1x,'at the extra point as well as the saddle point.',/)
120   FORMAT(1x,t6,'k',t10,'BmF@saddle point',t28,'BmF@extra point',/)
130   FORMAT(1x,I5,1X,1P,2E16.6)
140   FORMAT(1x,'total',1X,1P,2E16.6)
150   FORMAT(/,1x,'In the rest of the calculations, the curvature')
160   FORMAT(1x,'vector at the extra point is not used.')

      RETURN
      END SUBROUTINE bcalcp
C
C
C********************************************************************
C     DERV
C********************************************************************
      SUBROUTINE derv(ncoord,lorder)
      use perconparam
      use gtst
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VT1(MAXCAR),VT2(MAXCAR),VT3(MAXCAR),VT4(MAXCAR),
     * FIN(MAXCAR,MAXCAR),FTH(MAXCAR,MAXCAR),ISCR(MAXCAR)
C
C     CALLED BY : 
C                    BCALCP
C     CALLS     :    
C                    MXLNEQ
C
C     Calculate third derivatives
C
      DO 8 I = 1, NCOORD
         DO 11 J = 1,NCOORD 
c         DO 11 J=1,I                                 
            IF (LORDER .EQ. 1) THEN
               FTH(I,J)=(FORC1(I,J)-FORCS(I,J))/(ABS(DS))    
            END IF
            FTH(J,I) = FTH(I,J)                               
  11     CONTINUE                                              
   8  CONTINUE
C
C     Calculation of the curvature vector related
C     to third derivatives.
C
      DO 12 I = 1, NCOORD                                          
          VT1(I) = 0.0D0                                        
          VT4(I) = 0.0D0                                        
      DO 12 J = 1, NCOORD                                     
          VT1(I) = VT1(I) + FTH(I,J)*VECSV(J)              
          VT4(I) = VT4(I) + FORCS(I,J)*VECSV(J)                 
  12  CONTINUE                                                 
C
      ST1 = 0.0D0                                              
      ST2 = 0.0D0                                              
      DO 13 I = 1, NCOORD                                          
         ST1 = ST1 + VECSV(I)*VT1(I)                          
         ST2 = ST2 + VECSV(I)*VT4(I)                           
13    CONTINUE                                                 
C
      DO 14 I = 1, NCOORD                                          
         VT2(I) = ST1*VECSV(I)                                
         VT3(I) = VT1(I) - VT2(I)                              
      DO 14 J = 1, NCOORD                                      
         FIN(I,J) = -FORCS(I,J)                              
         IF (I .EQ. J) FIN(I,J)= 2.0D0*ST2 - FORCS(I,J)        
  14  CONTINUE     
C
      CALL MXLNEQ(FIN, NCOORD, MAXCAR, DET, JRANK, EPS,
     *                    ISCR, 0,NCOORD)
C
      DO 15 I = 1, NCOORD                                          
         V3(I) = 0.0D0                                      
      DO 15 J = 1, NCOORD                                   
         V3(I) = V3(I) + FIN(I,J)*VT3(J)                    
  15  CONTINUE                                           
      RETURN
      END SUBROUTINE derv
c*******************************************************************
c     dogleg
c*******************************************************************
      SUBROUTINE dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)
c
      integer n,lr
      double precision delta
      double precision r(lr),diag(n),qtb(n),x(n),wa1(n),wa2(n)
c
c
c     given an m by n matrix a, an n by n nonsingular diagonal
c     matrix d, an m-vector b, and a positive number delta, the
c     problem is to determine the convex combination x of the
c     gauss-newton and scaled gradient directions that minimizes
c     (a*x - b) in the least squares sense, subject to the
c     restriction that the euclidean norm of d*x be at most delta.
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization of a. that is, if a = q*r, where q has
c     orthogonal columns and r is an upper triangular matrix,
c     then dogleg expects the full upper triangle of r and
c     the first n components of (q transpose)*b.
c
c     the subroutine statement is
c
c       subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an input array of length lr which must contain the upper
c         triangular matrix r stored by rows.
c
c       lr is a positive integer input variable not less than
c         (n*(n+1))/2.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       delta is a positive input variable which specifies an upper
c         bound on the euclidean norm of d*x.
c
c       x is an output array of length n which contains the desired
c         convex combination of the gauss-newton direction and the
c         scaled gradient direction.
c
c       wa1 and wa2 are work arrays of length n.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm
c
c       fortran-supplied ... dabs,dmax1,dmin1,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c*******************************************************************
      integer i,j,jj,jp1,k,l
      double precision alpha,bnorm,epsmch,gnorm,one,qnorm,sgnorm,sum,
     *                 temp,zero
      double precision dpmpar,enorm
      data one,zero /1.0d0,0.0d0/

c     epsmch is the machine precision.

      epsmch = dpmpar(1)

c     first, calculate the gauss-newton direction.

      jj = (n*(n + 1))/2 + 1
      do 50 k = 1, n
         j = n - k + 1
         jp1 = j + 1
         jj = jj - k
         l = jj + 1
         sum = zero
         if (n .lt. jp1) go to 20
         do 10 i = jp1, n
            sum = sum + r(l)*x(i)
            l = l + 1
   10       continue
   20    continue
         temp = r(jj)
         if (temp .ne. zero) go to 40
         l = j
         do 30 i = 1, j
            temp = dmax1(temp,dabs(r(l)))
            l = l + n - i
   30       continue
         temp = epsmch*temp
         if (temp .eq. zero) temp = epsmch
   40    continue
         x(j) = (qtb(j) - sum)/temp
   50    continue

c     test whether the gauss-newton direction is acceptable.

      do 60 j = 1, n
         wa1(j) = zero
         wa2(j) = diag(j)*x(j)
   60    continue
      qnorm = enorm(n,wa2)
      if (qnorm .le. delta) go to 140

c     the gauss-newton direction is not acceptable.
c     next, calculate the scaled gradient direction.

      l = 1
      do 80 j = 1, n
         temp = qtb(j)
         do 70 i = j, n
            wa1(i) = wa1(i) + r(l)*temp
            l = l + 1
   70       continue
         wa1(j) = wa1(j)/diag(j)
   80    continue

c     calculate the norm of the scaled gradient and test for
c     the special case in which the scaled gradient is zero.

      gnorm = enorm(n,wa1)
      sgnorm = zero
      alpha = delta/qnorm
      if (gnorm .eq. zero) go to 120

c     calculate the point along the scaled gradient
c     at which the quadratic is minimized.

      do 90 j = 1, n
         wa1(j) = (wa1(j)/gnorm)/diag(j)
   90    continue
      l = 1
      do 110 j = 1, n
         sum = zero
         do 100 i = j, n
            sum = sum + r(l)*wa1(i)
            l = l + 1
  100       continue
         wa2(j) = sum
  110    continue
      temp = enorm(n,wa2)
      sgnorm = (gnorm/temp)/temp

c     test whether the scaled gradient direction is acceptable.

      alpha = zero
      if (sgnorm .ge. delta) go to 120

c     the scaled gradient direction is not acceptable.
c     finally, calculate the point along the dogleg
c     at which the quadratic is minimized.

      bnorm = enorm(n,qtb)
      temp = (bnorm/gnorm)*(bnorm/qnorm)*(sgnorm/delta)
      temp = temp - (delta/qnorm)*(sgnorm/delta)**2
     *       + sqrt((temp-(delta/qnorm))**2
     *               +(one-(delta/qnorm)**2)*(one-(sgnorm/delta)**2))
      alpha = ((delta/qnorm)*(one - (sgnorm/delta)**2))/temp
  120 continue

c     form appropriate convex combination of the gauss-newton
c     direction and the scaled gradient direction.

      temp = (one - alpha)*dmin1(sgnorm,delta)
      do 130 j = 1, n
         x(j) = temp*wa1(j) + alpha*x(j)
  130    continue
  140 continue
      return
      end SUBROUTINE dogleg
c*******************************************************************
c     function dpmpar
c*******************************************************************
      double precision function dpmpar(i)
      integer i

c     This function provides double precision machine parameters
c
c      dpmpar(1) = the machine precision,
c      dpmpar(2) = the smallest magnitude,
c      dpmpar(3) = the largest magnitude.
c
c    
      double precision dmach(3)

      data dmach(1) /1.0d-20/
      data dmach(2) /1.0d-38/
      data dmach(3) /1.0d38/

      dpmpar = dmach(i)
      return
      end function dpmpar
C***********************************************************************
C ECKARS  
C*********************************************************************
      SUBROUTINE eckars(iop,redm,wi,lorder)
      use perconparam
      use gtst
c
c     Interpolates: 
c
c     IOP = 1 : FOR THE CLASSICAL POTENTIAL CURVE.
c         = 2 : FOR THE VIBRATIONALLY GROUND-STATE ADIABATIC ENERGY CURVE
c
c     AVC    = A 
c     BVC    = B 
c     GAMMAV = -1/L
c     XMVC = S0
c
c     CALLED BY:
c                IRPH, IRPHZ, KAPPAS
c
      implicit double precision (A-H,O-Z)
C
C  All values are in atomic unit.
C
      if (iop .eq. 1) then
         v0 = vc/ckcal
         v1 = dele/ckcal
      else
         v0 = vag
         v1 = delh
      endif
C
C  Determine the parameters of the Eckart potential A,B,XM,GAMMAV.
C
      AVC = v1
      BVC = ((2.0d0*v0-v1) + 2.0d0*sqrt(v0*(v0-v1)))
C
C GAMMA and XM are calculated from the imaginary frequency for
C the classical PES curve, and they will be used as initial guess
C for fitting to the 4th point to obtain the VA(s) curve.
C
      if(iop .eq. 1) then
         YM = (AVC + BVC)/(BVC - AVC)
         T1 = YM*(1.0D0+YM)*(AVC*(1.0D0+2.0D0*YM) +
     *        BVC*(1.0D0-2.0D0*YM))
         T2 = 3.0D0*YM**2*(AVC*(1.0D0+YM) + BVC*(1.0D0-YM))
         GAMMAV = wi*(1+Ym)**2*SQRT(-redm/(T1-T2))
         XMVC = 1.D0/GAMMAV*LOG((AVC+BVC)/(BVC-AVC))
         IF(LORDER .EQ. 0) GOTO 30
      endif
C
      GAMMA0 = GAMMAV
      C = LOG((AVC+BVC)/(BVC-AVC))
      IF(LORDER .EQ. 0) GOTO 20
      XT = X4
      VT = V4
5     DO 10 I = 1,1000
          Y0 = EXP(GAMMA0*XT + C)
          V0 = (AVC + BVC/(1.0D0+Y0))*(Y0/(1.0D0+Y0)) 
          DV0 = XT*Y0*(AVC*(1.0D0+Y0)+BVC*(1.0D0-Y0))/(1.0D0+Y0)**3
          GAMMAV = GAMMA0 - (V0 - VT)/DV0
C   change the limit to 2.0E-10 from 1.0E-10
          IF(ABS(GAMMAV-GAMMA0) .LT. 2.0D-10) GOTO 20
          GAMMA0 = GAMMAV
10    CONTINUE
      WRITE(fu6,*)'ROOT SEARCH FOR GAMMA FAILS AFTER 1000 ITERATIONS'
      STOP     
c   
 20   CONTINUE
c
      XMVC =  C/GAMMAV
c
 30   continue
      if ( iop .eq. 1) then
         write(fu6,1000)
         write(fu6,3000) avc*ckcal,bvc*ckcal,gammav,xmvc
C
C  Save GAMMAV of Vmep for the Eckart of the reduced mass
C
         gammam = 2.d0*gammav
      else
         write(fu6,2000)
         write(fu6,3000) avc*ckcal,bvc*ckcal,gammav,xmvc
      endif
1000  FORMAT(/,1X,'Parameters of the Eckart function for VMEP (a.u.)')
2000  FORMAT(/,1X,'Parameters of the Eckart function for VA(S) (a.u.)')
3000  FORMAT(/,5X,'V(s) = AY/(1+Y) + BY/(1+Y)**2    Y = EXP(GAMMA',
     * '*(s-So))'/,10X,'A = ',7x,F15.6,5X,'B = ',F15.6,                 06/96ELC
     * /,10x,'GAMMA = -1/L = ',F11.6,5X,'So = ',F14.6)                  06/96ELC
      RETURN
      END SUBROUTINE eckars
c
C***********************************************************************
C ECKRT
C***********************************************************************
C
      SUBROUTINE eckrt(lorder)
      use perconparam, only : fu6
      use gtst
c
c     Interpolates generalized frequencies
c
c     IFCN =  -1   Inverted Eckart
c          =   1   Eckart
c          =   0   Hyperbolic Tangent  TANH
c
c     CALLED BY:
c                IRPH
c     CALLS:
c                HYBRD1
C
C
      implicit double precision(a-h,o-z)
c
c use MINIPACK's routines
c
      external fcn, dpmpar
c
c use NAG's routines
  
c      EXTERNAL C05NBF,FCN
c      EXTERNAL F06EJF,X02AJF
c
      DIMENSION X(3),WA(100),FVEC(3)
      DATA LWA /100/,CMCAL /349.7551D0/
c
c    value defined in common fvt1cm and fvt2cm by irph
c
c     sz(1) - s value at the saddle point (0.0)
c     sz(2) - s value at the first extra point
c     sz(3) - s value at the second extra point
c     fz(1) - the ith transition state frequency  
c     fz(2) - the ith extra point number 1 frequency
c     fz(3) - the ith extra point number 2 frequency
c     fp   - the ith product frequency
c     fr   - the ith reactant frequency
c     n    = 2 for first order
C
C
C  Decide which function to use
C
      DS = SZ(2) - SZ(1)
      DF = FP - FR
      FSLP = (FZ(2)-FZ(1))/DS
      DFTS = FZ(1) - FZ(2)
      DFR1 = FZ(1) - FR
      DFR2 = FZ(2) - FR
      DFP1 = FZ(1) - FP
      DFP2 = FZ(2) - FP
      F1OLD = FZ(1)
      F2OLD = FZ(2)
C
      WRITE(fu6,900)
      WRITE(fu6,'(1X,6F15.7,/)') FR*CMCAL,(FZ(I)*CMCAL,I=1,NPT),FP*CMCAL
C
C CASE 1: FZ(1) AND FZ(2) > FR AND FP --->  ECKART
C
      IF(DFR1.GT.0.0D0.AND.DFR2.GT.0.0D0.AND.DFP1.GT.0.0D0
     *   .AND.DFP2.GT.0.0D0)THEN
         IFCN = 1
         WRITE(fu6,*) ' ECKART FUNCTION IS USE TO FIT'
C
C CASE 2: FZ(1) AND FZ(2) < FR AND FP ----> INVERTED ECKART
C
      ELSEIF(DFR1.LT.0.0D0.AND.DFR2.LT.0.0D0.AND.DFP1.LT.0.0D0
     *       .AND.DFP2.LT.0.0D0) THEN
          IFCN = -1
          WRITE(fu6,*) ' INVERTED ECKART IS USED FOR THE FIT'           06/96ELC
C
C  CASE 3:      FR > FZ(1),FZ(2) > FP
C         OR    FR < FZ(1),FZ(2) < FP AND FSLP*DF > 0  ---> TANH
C
      ELSEIF(((DFR1.GT.0.0D0.AND.DFR2.GT.0.0D0.AND.DFP1.LT.0.0D0
     *  .AND.DFP2.LT.0.)
     *  .OR. (DFR1.LT.0.0D0.AND.DFR2.LT.0.0D0.AND.DFP1.GT.0.0D0
     *  .AND.DFP2.GT.0.))
     *  .AND. (FSLP*DF .GT. 0.0D0)) THEN
           IFCN = 0
           WRITE(fu6,*) ' HYPERBOLIC TANGENT FUNCTION TANH IS USED'
      ELSE
           WRITE(fu6,*) ' NEED TO USE DIFFERENT END POINTS'
           INFO = 5
           RETURN
      ENDIF
C
C
      ALFNI = 0.D0
      ALFPI = 0.D0
      CNCM = 10.D0/CMCAL
      IF (IFCN .NE. 0) THEN
         ESIGN = IFCN
         ASIGN = SIGN(1.D0,-DFTS)
         C = FR
         A = FP - FR
         F0 = FZ(1)
         F1 = FZ(2)
         S0 = SZ(1)
         S1 = SZ(2)
        IF (LORDER .EQ. 1) THEN
           WNI = A*(1.0D0-EXP(-ALF*(S1-S0)))+
     *          (F0-C)*EXP(-ALF*(S1-S0))+C
           WPI = (F0-C)*EXP(ALF*(S1-S0)) + C
10         IF (WNI .LT. WPI) THEN
             WLOW = WNI
             WHIGH = WPI
          ELSE
             WLOW = WPI
             WHIGH = WNI
          ENDIF
          WRITE(fu6,*) ' THE MAXIMUM RANGE OF W(S1) ',WLOW*CMCAL,
     *               WHIGH*CMCAL
C
              TNI = (F1-C-A )/(F0-C-A)
              TPI = (F1-C)/(F0-C)
             IF (F1.LE.WLOW) THEN
                   IF (WNI .LT. WPI) THEN
                     TNI = (F1 - C - A - CNCM)/(F0 - C - A)
                     IF(TNI .LT. 1.0D0) THEN
                     ALFNI = -1.0D0/(S1-S0)*LOG(TNI)
                     ALF = ALFNI
                     ELSE
                     ALFNI = -1.D20
                     END IF
                   ELSE
                     TPI = (F1 - C - CNCM)/(F0 - C)
                     IF(TPI .GT. 1.0D0)THEN
                     ALFPI = 1.0D0/(S1-S0)*LOG(TPI)
                     ALF = ALFPI
                     ELSE
                     ALFPI = -1.D20
                     END IF
                   END IF
             ELSE IF (F1.GE. WHIGH) THEN
                   IF (WNI .GT. WPI) THEN
                     TNI = (F1 - C - A + CNCM)/(F0 - C - A)
                     IF(TNI .LT. 1.0D0) THEN
                     ALFNI = -1.0D0/(S1-S0)*LOG(TNI)
                     ALF = ALFNI
                     ELSE
                     ALFNI = -1.D20
                     END IF
                   ELSE
                     TPI = (F1 - C + CNCM)/(F0 - C)
                     IF(TPI .GT. 1.0D0) THEN
                     ALFPI = 1.0D0/(S1-S0)*LOG(TPI)
                     ALF = ALFPI
                     ELSE
                     ALFPI = -1.0D20
                     END IF
                   END IF
             END IF
              IF ( ALFNI.LT.0.0D0 .OR.  ALFPI.LT.0.0D0) THEN
                 WRITE(fu6,*) ' BOTH NEW ALP ARE NEGATIVE OR UNDEFINED'
                 WRITE(fu6,*) ' NEED NEW END POINTS'
                 INFO = 5
                 RETURN
              ELSE
                 WRITE(fu6,*) ' NEW ALF = ', ALF
              END IF
C
              WNI = A*(1.0D0-EXP(-ALF*(S1-S0)))+
     *              (F0-C)*EXP(-ALF*(S1-S0))+C
              WPI = (F0-C)*EXP(ALF*(S1-S0)) + C
              WRITE(fu6,*) ' THE NEW MAXIMUM RANGE OF W(S1)', WNI*CMCAL,
     *        WPI*CMCAL
C
        END IF
      ENDIF
C
c use NAG's routines
c      tol = sqrt(x02ajf(1))
c use MINPACK's routines
       tol =  sqrt(dpmpar(1))
c
      NSTEP = 0
      NFLAG = 0
      INFO = 1
C
C  Initial guess solution
C
      IF (IFCN .EQ. 0) THEN
          NPT = 2
          C = (FR + FP)/2.0D0
          A = (FP - FR)/2.0D0
          X(1) =  SIGN(1.D0,DFR1/DF-0.5D0)*0.005D0
          STEP = SIGN(0.005D0,X(1))
          X(2) = 1.0D0
          X2INI =  X(2)
      ELSE
          V0 = FZ(1) - C
          B =  2.D0*V0 - A + ESIGN*2.D0*SQRT(V0*(V0-A))
          X(1) = B
          X(2) = 0.005D0
          X2INI = 0.005D0
      ENDIF
100   CONTINUE
c use NAG's routines
c     CALL C05NBF(FCN,NPT,X,FVEC,TOL,WA,LWA,INFO)
c     write(fu6,*)'INFO= ',INFO
c
c use MINIPACK's routines
       CALL HYBRD1(FCN,NPT,X,FVEC,TOL,INFO,WA,LWA)
       if(info.eq.1) info = 0
c
c      Convergence problems
c 
c use NAG's routines
c     FNORM = F06EJF(NPT,FVEC,1)
c
c use MINIPACK's routines
      FNORM = ENORM(NPT,FVEC)
c
c
c  For 1st Order
c
      if(lorder .eq. 1) then
c
c  Exit conditions
c
      IF(NSTEP .EQ. 0 .AND. INFO .EQ. 0) GOTO 300
      IF(IFCN.NE.0 .AND. NSTEP.NE.0 .AND. INFO.NE.0) GOTO 300
      IF(IFCN.NE.0 .AND. NSTEP.EQ.0 .AND. ABS(DFTS*CMCAL) .LT. 3.0D0
     * .AND. INFO.NE.0) GOTO 300
C
C  Reiterate for eckart
C
        IF ( IFCN .NE. 0 .AND. NFLAG.EQ.0) THEN
          IF (NSTEP .EQ. 0) THEN
             WRITE(fu6,1500)
             WRITE(fu6,1600)
          END IF
           B = X(1)
           WMAX = ((A+B)**2/(4.D0*B) + C)*CMCAL
           IF(NSTEP.EQ.0)WRITE(fu6,2000) FZ(2)*CMCAL,WMAX,FNORM,INFO,
     *                                 (X(I),I=1,NPT)
           NSTEP = NSTEP + 1
           FZ(2) = FZ(1) + DFLOAT(NSTEP)*ASIGN*(2.0D0/CMCAL)
           IF ( ABS(F2OLD-FZ(2)) .LE. 1.0D0/CMCAL) THEN
              FZ(2) = F2OLD
              NFLAG= 1
           ENDIF
C
           GOTO 100
        ENDIF
      endif
c
c  Reiterate for hyperbolic tan
c
      if(ifcn .eq. 0 .and. info .ne. 0) then
          nstep = nstep + 1
          x(1) = x(1) + step
          x(2) = x2ini
          if(nstep .gt. 20) then
             goto 300
          else
             goto 100
          endif
      endif
c
300   continue
C
C  Put back the original values for FZ
C
      if(info .ne. 0) then
          fz(1) = f1old
          fz(2) = f2old
      else
c
c     Converged results
c
          b = x(1)
          if (ifcn.ne.0) then
c            SM is So (see JCP 95,8875 (1991))
             sm = x(2)
          else
             gamma = x(2)
          endif
      endif
C
      if(info .eq. 0) then
           if(ifcn.ne.0) then
              write(fu6,1000) a*cmcal,b*cmcal,c*cmcal,alf,sm,fnorm
           else if (ifcn .eq. 0) then
              write(fu6,1200)a*cmcal,b,c*cmcal,gamma,fnorm
           endif
      else if(info .ne. 0) then
           if(ifcn .ne. 0) then
             write(fu6,1001) fnorm,info
           else if (ifcn .eq. 0) then 
             write(fu6,*) 'unable to interpolate mode i'
           endif
      endif
C
  900 format(/,1X,' SEARCHING FOR AN INTERPOLATING FUNCTION WHICH',
     * ' GOES THROUGH')                                                 06/96ELC
 1000 format(//,10X,'FINAL APPROXIMATE SOLUTION (a.u.)',//,5X,
     * 'a = ',0PF10.2,                                                  06/96ELC
     * 2X,'b = ',F15.6,2X,'c = ',F10.2,2X,/,5X,'1/l   = ',F12.6,2X,     06/96ELC
     * 'so = ',1PE15.5,/5X,'SQUARE NORM = ',E15.5)                      06/96ELC
 1001 format(//,10X,'SQUARE NORM = ',E15.5,5X,'INFO = ',I2/)
 1100 format(//,10X,'FINAL APPROXIMATE SOLUTION (a.u.)',//,10X,         09/96ELC
     * 'A = ',F10.2,
     * 5X,'B = ',F15.6,5X,'C = ',F10.2,/,5X,'D=',F15.6,
     *  'ALPHA = ',F10.6,5X,
     * 'SM = ',1PE15.5,/5X,'SQUARE NORM = ',E15.5/)
 1200 format(//,10X,'FINAL APPROXIMATE SOLUTION (a.u.)',//,10X,         09/96ELC
     * 'a = ',F10.2,                                                    06/96ELC
     * 5X,'b = ',1PE15.5,5X,'c = ',0PF10.2,/,10X,'1/l = ',F10.6,/10X,   06/96ELC
     * 'SQUARE NORM = ',1PE15.5)
 1500 format(/,1X,' ********* AUTOMATED SEARCH **********',/)
 1600 format(4X,'W(S1)',8X,'WMAX',10X,'NORM',8X,'INFO',10X,'B',12X,
     * 'X0',/)
 1700 format(4X,'W(S1)',8X,'WMAX',10X,'NORM',8X,'INFO',10X,'B',12X,
     * 'X0',12X,'ALP',/)
 2000 format(1X,F8.2,4X,E8.2,8X,E12.6,I4,3F13.5)
 200  return
      end SUBROUTINE eckrt
c
c*******************************************************************
c     function enorm
c*******************************************************************
      double precision function enorm(n,x)
      integer n
      double precision x(n)
c
c
c     given an n-vector x, this function calculates the
c     euclidean norm of x.
c
c     the euclidean norm is computed by accumulating the sum of
c     squares in three different sums. the sums of squares for the
c     small and large components are scaled so that no overflows
c     occur. non-destructive underflows are permitted. underflows
c     and overflows do not occur in the computation of the unscaled
c     sum of squares for the intermediate components.
c     the definitions of small, intermediate and large components
c     depend on two constants, rdwarf and rgiant. the main
c     restrictions on these constants are that rdwarf**2 not
c     underflow and rgiant**2 not overflow. the constants
c     given here are suitable for every known computer.
c
c       n is a positive integer input variable.
c
c       x is an input array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c*******************************************************************
      integer i
      double precision agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs,
     *                 x1max,x3max,zero
      data one,zero,rdwarf,rgiant /1.0d0,0.0d0,3.834d-20,1.304d19/
      s1 = zero
      s2 = zero
      s3 = zero
      x1max = zero
      x3max = zero
      floatn = n
      agiant = rgiant/floatn
      do 90 i = 1, n
         xabs = dabs(x(i))
         if (xabs .gt. rdwarf .and. xabs .lt. agiant) go to 70
            if (xabs .le. rdwarf) go to 30

c              sum for large components.

               if (xabs .le. x1max) go to 10
                  s1 = one + s1*(x1max/xabs)**2
                  x1max = xabs
                  go to 20
   10          continue
                  s1 = s1 + (xabs/x1max)**2
   20          continue
               go to 60
   30       continue

c              sum for small components.

               if (xabs .le. x3max) go to 40
                  s3 = one + s3*(x3max/xabs)**2
                  x3max = xabs
                  go to 50
   40          continue
                  if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2
   50          continue
   60       continue
            go to 80
   70    continue

c           sum for intermediate components.

            s2 = s2 + xabs**2
   80    continue
   90    continue

c     calculation of norm.

      if (s1 .eq. zero) go to 100
         enorm = x1max*sqrt(s1+(s2/x1max)/x1max)
         go to 130
  100 continue
         if (s2 .eq. zero) go to 110
            if (s2 .ge. x3max)
     *         enorm = sqrt(s2*(one+(x3max/s2)*(x3max*s3)))
            if (s2 .lt. x3max)
     *         enorm = sqrt(x3max*((s2/x3max)+(x3max*s3)))
            go to 120
  110    continue
            enorm = x3max*sqrt(s3)
  120    continue
  130 continue
      return
      end function enorm
C
C*****************************************************************
C EPARTI
C*****************************************************************
C
C  CALCULATE ELECTRONIC PARTITION FUNCTION
C
C
      double precision function eparti(iop,bkt,ndeg,elec)
      implicit double precision (a-h,o-z)
      DIMENSION NDEG(5,3),ELEC(5,3)
C
      SUM = 0.0D0
      DO 10 I = 1,3
         IF (NDEG(IOP,I) .NE. 0) THEN
            SUM = SUM + NDEG(IOP,I)*EXP(-ELEC(IOP,I)/BKT)
         ENDIF
10    CONTINUE
      EPARTI = SUM
      RETURN
      END function eparti
C
C***********************************************************************
C FCN
C***********************************************************************
      SUBROUTINE fcn(N,X,FVEC,IFLAG)
      use gtst
c
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c
c  IFCN =  -1   Inverted Eckart
c       =   1   Eckart
c       =   0   Hyperbolic Tangent  TANH
c
      DIMENSION X(N),FVEC(N)
C
      BN = X(1)
      IF (IFCN .NE. 0) THEN
          SMN = X(2)
          IF(NPT.EQ.3) ALF= X(3)
      ELSE
          GAMM = X(2)
      ENDIF
      DO 10 I=1,N
         IF (IFCN .NE. 0) THEN
              XP = ALF*(SZ(I)- SMN)
C
C  Machine dependent condition
C
              IF(XP .GT. 40.D0) THEN
                 VF = A + C
              ELSE
                 Y = EXP(ALF*(SZ(I)-SMN))
                 VF = (A*Y/(1.0D0+Y) + BN*Y/(1.0D0+Y)**2) + C
              END IF
         ELSE
              VF = C + A*TANH(GAMM*(SZ(I)-BN))
         ENDIF
         FVEC(I) = VF - FZ(I)
10    CONTINUE
      RETURN
      END SUBROUTINE fcn
C
C***********************************************************************
C  FDIAG2 
C***********************************************************************
C
      SUBROUTINE fdiag2(kop,iop,na,amasrk,redm)
      use perconparam; use gtst
C
C    The section of code that diagonalizes the F matrix and optional
C    prints out normal modes.
C 
C
C     CALLS:
C            RST
C
      implicit double precision (a-h,o-z)
C
      dimension  na(10),scr(maxcar),scr2(maxcar),fre(maxcar)
C
C
C Diagonalize F to obtain force constants and normal mode directions.
C if not at stationary point, use normalized grad(v) to project out of F
C the zero modes
C
      NEND =3*NA(KOP)
      CALL RST (MAXCAR,NEND,FORC,FRE,1,COF,SCR,SCR2,IERR)
      DO 16 J = 1,NEND
         FREQ(KOP,J) = FRE(J)
16    CONTINUE
      IF (IERR.NE.0) THEN
         WRITE (fu6,1000) IERR
         STOP 2
      ENDIF
C
C Change phase of each eigenvector so that the largest component is
C    always positive
C
      DO 30 J = 1, NEND
         COFMAX = ABS(COF(1,J))
         IMAX = 1
         DO 10 I = 2, NEND
            IF (ABS(COF(I,J)).GT.COFMAX) THEN
               COFMAX = ABS(COF(I,J))
               IMAX = I
            ENDIF
   10    CONTINUE
         IF (COF(IMAX,J).LT.0.0D0) THEN
            DO 20 I = 1, NEND
               COF(I,J) = -COF(I,J)
   20       CONTINUE
         ENDIF
   30 CONTINUE
C
C Check here if zero-frequency modes should be swapped with ones with
C    large imaginery values.
C
      IF (KOP.LT.5) THEN
         IF (IOP .EQ. 2) THEN
            ISHFT = 5
         ELSE
            ISHFT = 6
         ENDIF
      ELSE
         IF (IOP.EQ.2) THEN
            ISHFT = 6
         ELSE
            ISHFT = 7
         ENDIF
      ENDIF
      DO 40 I = 1, NEND
         IF (FREQ(KOP,I) .EQ. 0.0D0) GO TO 40
         FREQ(KOP,I) = SQRT(ABS(FREQ(KOP,I)/REDM))*SIGN(1.0D0,
     *                              FREQ(KOP,I))
C         INTOUT(I) = 0
   40 CONTINUE
      ISW1 = 1
C
C If at the saddle point skip the lowest eigenvalue which corresponds
C    to the reaction coordinate motion.
C
      IF (KOP.EQ.5) ISW1 = 2
      ISW2 =  ISHFT
      NPOS = NEND-ISHFT
  45  DO 60 J = 1, NPOS
         ISW2 = ISW2+1
C         IF (FREQ(KOP,ISW2).GT.2.E-5) GO TO 70                        
         IF (ABS(FREQ(KOP,ISW2)).GT.ABS(FREQ(KOP,ISW1))) GO TO 70     
C
C    The magnitude of the negative eigenvalue is larger so swap.
C
         DO 50 K = 1, NEND
            T = COF(K,ISW1)
            COF(K,ISW1) = COF(K,ISW2)
            COF(K,ISW2) = T
   50    CONTINUE
         FREQI = FREQ(KOP,ISW1)
         FREQ(KOP,ISW1) = FREQ(KOP,ISW2)
         FREQ(KOP,ISW2) = FREQI
         ISW1 = ISW1+1
   60 CONTINUE
   70 CONTINUE
      ISW1 = 1
      IF (KOP.EQ.5) ISW1 = 2
      ISW2 = ISHFT
      IF (ABS(FREQ(KOP,ISW2+1)) .LT. ABS(FREQ(KOP,ISW1))) GOTO 45
      RETURN
C
1000   FORMAT (/ 1X, 30(1H*),7H IERR =,I5)
C
      END subroutine fdiag2
c
c*******************************************************************
c     fdjac1
c*******************************************************************
      subroutine fdjac1(fcn,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,
     *                  wa1,wa2)
c
      integer n,ldfjac,iflag,ml,mu
      double precision epsfcn
      double precision x(n),fvec(n),fjac(ldfjac,n),wa1(n),wa2(n)
c
c
c     this subroutine computes a forward-difference approximation
c     to the n by n jacobian matrix associated with a specified
c     problem of n functions in n variables. if the jacobian has
c     a banded form, then function evaluations are saved by only
c     approximating the nonzero terms.
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions see hybrid1.
c
c       fjac is an output n by n array which contains the
c         approximation to the jacobian matrix evaluated at x.
c
c       ldfjac is a positive integer input variable not less than n
c         which specifies the leading dimension of the array fjac.
c
c       iflag is an integer variable which can be used to terminate
c         the execution of fdjac1. see description of fcn.
c
c       ml is a nonnegative integer input variable which specifies
c         the number of subdiagonals within the band of the
c         jacobian matrix. if the jacobian is not banded, set
c         ml to at least n - 1.
c
c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       mu is a nonnegative integer input variable which specifies
c         the number of superdiagonals within the band of the
c         jacobian matrix. if the jacobian is not banded, set
c         mu to at least n - 1.
c
c       wa1 and wa2 are work arrays of length n. if ml + mu + 1 is at
c         least n, then the jacobian is considered dense, and wa2 is
c         not referenced.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar
c
c       fortran-supplied ... dabs,dmax1,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c*******************************************************************
      integer i,j,k,msum
      double precision eps,epsmch,h,temp,zero
      double precision dpmpar
C
      external fcn                                                      081696PF
      data zero /0.0d0/
C
c     epsmch is the machine precision.

      epsmch = dpmpar(1)

      eps = sqrt(dmax1(epsfcn,epsmch))
      msum = ml + mu + 1
      if (msum .lt. n) go to 40

c        computation of dense approximate jacobian.

         do 20 j = 1, n
            temp = x(j)
            h = eps*dabs(temp)
            if (h .eq. zero) h = eps
            x(j) = temp + h
            call fcn(n,x,wa1,iflag)
            if (iflag .lt. 0) go to 30
            x(j) = temp
            do 10 i = 1, n
               fjac(i,j) = (wa1(i) - fvec(i))/h
   10          continue
   20       continue
   30    continue
         go to 110
   40 continue

c        computation of banded approximate jacobian.

         do 90 k = 1, msum
            do 60 j = k, n, msum
               wa2(j) = x(j)
               h = eps*dabs(wa2(j))
               if (h .eq. zero) h = eps
               x(j) = wa2(j) + h
   60          continue
            call fcn(n,x,wa1,iflag)
            if (iflag .lt. 0) go to 100
            do 80 j = k, n, msum
               x(j) = wa2(j)
               h = eps*dabs(wa2(j))
               if (h .eq. zero) h = eps
               do 70 i = 1, n
                  fjac(i,j) = zero
                  if (i .ge. j - mu .and. i .le. j + ml)
     *               fjac(i,j) = (wa1(i) - fvec(i))/h
   70             continue
   80          continue
   90       continue
  100    continue
  110 continue
      return
      end subroutine fdjac1
C
C***********************************************************************
C  FITMX
C***********************************************************************
C
      SUBROUTINE fitmx(F,S,N,ISTART,ISTOP,SMAX,FMAX,IMAX)
      use perconparam
C
      implicit double precision (a-h,o-z)
C
C     FIND MAX OF FUNCTION F WITH 5 POINT FITS
C
C     CALLED BY:
C                GIVTST
C     CALLS:
C                FIVFT
C
      DIMENSION XPT(5),GPT(5),F(*),S(*)
C
C
C     FIND LARGEST F
C
      FMAX = 0.0D0
      DO 10 I = ISTART,ISTOP
         IF (F(I).GT.FMAX) THEN
            FMAX = F(I)
            IMAX = I
         ENDIF
   10 CONTINUE
      XMAX = S(IMAX)
C
C     ERROR CONDITION - MAKE THIS A NON-FATAL ERROR
C
      IF (IMAX.LE.(ISTART+2).OR.IMAX.GE.(ISTOP-2)) THEN
         WRITE (fu6,1000)
      ENDIF
C
C     FIVE POINT FIT.
C
   70 CONTINUE
      IF (IMAX.EQ.2) IMAX = 3
      IF (IMAX.EQ.(N-1)) IMAX = N-2
      J = IMAX-3
      DO 80 I = 1, 5
         XPT(I) = S(J+I)
         GPT(I) = F(J+I)
   80 CONTINUE
      CALL FIVFT (XPT,GPT,XMAX,FMAX)
      SMAX = XMAX
  100 RETURN
C
 1000 FORMAT(1X,20(1H$),28H  LARGEST F IS NEAR ENDPOINT)
C
      END SUBROUTINE fitmx
C
C***********************************************************************
C  FIVFT
C***********************************************************************
C
      SUBROUTINE fivft (X,F,XMAX,FMAX)
      use perconparam
C
C     COMPUTES QUARTIC FIT F=AX4+BX3+CX2+DX+E THRU FIVE POINTS --
C     (X(I),F(I),I=1,5)
C     X(IMAX) WHERE F(IMAX)=MAX(F(I)) (I=1,5) IS USED AS INITIAL
C     GUESS FOR SOLUTION OF XMAX
C
C     CALLED BY:
C                FITMX
C     CALLS:
C                MXLNEQ, CUBIC
C
      implicit double precision (a-h,o-z)
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
c      CALL MXLNEQ(AB,5,5,DET,JRANK,EPS,ISCR,-1,5) 
      CALL MXLNEQ(AB,5,5,DET,JRANK,EPS,ISCR,-1,6)                       1103BE05
      IF (JRANK.LT.5) STOP 50
      A = AB(1,6)
      B = AB(2,6)
      C = AB(3,6)
      D = AB(4,6)
      E = AB(5,6)
C
C     TO FIND MAX, NEED TO SOLVE CUBIC EQUATION F'=0
C
      AA = 4.0D0*A
      BB = 3.0D0*B
      CC = C*2.0D0
      XGUES = XMAX
      CALL CUBIC (AA,BB,CC,D,XGUES,ANS)
      XMAX = ANS
      FMAX = E+XMAX*(D+XMAX*(C+XMAX*(B+XMAX*A)))
      RETURN
      END SUBROUTINE fivft
c
c***********************************************************************
c  getfrq
c***********************************************************************
c
      SUBROUTINE getfrq(k,icode,nfreq,amasrk,wi,redm,na,index,x,y,z,
     * amass,rot)
      use perconparam
      use gtst
c
c     Obtains vibrational frequencies in cm**-1 and generalized eigenvectors
c
c     CALLED BY:
c                GIVTST
c
c     CALLS:     TRANS2, PROJTI, FDIAG2
c
      implicit double precision (a-h,o-z)
c
      dimension na(10),nfreq(10),index(10,natoms)
      dimension x(natoms),y(natoms),z(natoms),amass(natoms)
      dimension tfreq(maxcar),tcof(maxcar,maxcar)                       0211YC97
      real(8) :: rot(3,3)
      CHARACTER*1 XYZ(3,N3TM)                                           0211YC97
C
      if(k.gt.5) then
         call trans2(k,1,na,index,redm,x,y,z,amass)
         call projti(k,icode,na,amasrk,rot)
      endif
      call fdiag2(k,icode,na,amasrk,redm)
C
      if(k.le.4) then
        if(icode.eq. 2) then
          ishift = 5
        else
          ishift = 6
        endif
        else
        if(icode.eq.2) then
          ishift = 6
          else
          ishift = 7
         endif
        endif
c
        if( k .eq. 5) then
           wi = -freq(k,1)
           do 37 j = 1,3*na(k)
              vecsv(j) = cof(j,1)
 37        continue
           write(fu6,1250) wi*autocm
           if(iprint.eq.1) then                                         0211YC97
             write(fu6,1251)                                            0211YC97
             write(fu6,1249) (vecsv(j),j=1,3*na(k))                     0211YC97
           endif                                                        0211YC97
        endif
C
        do 38 i = 1,nfreq(k)
           ishift = ishift + 1
           freq(k,i) = freq(k,ishift)
c
          do 39 j = 1,3*na(k)
             cof(j,i) = cof(j,ishift)
C
C  Save eigenvectors for the interpolation of the reduced mass
C
             if (k .eq. 5) then
                cofsp(j,i) = cof(j,i)
                else if (k .eq. 6) then
                  cofex1(j,i) = cof(j,i)
              end if
   39     continue
   38   continue
c
c Canonical ordering
c
      do i = 1,nfreq(k)                                                 0211YC97
        tfreq(i) = freq(k,nfreq(k)-i+1)                                 0211YC97
        do j = 1,3*na(k)                                                0211YC97
        tcof(j,i) = cof(j,nfreq(k)-i+1)                                 0211YC97
        enddo                                                           0211YC97
      enddo                                                             0211YC97
C      write(fu6,1252) (freq(k,i)*autocm,i=1,nfreq(k))
       write(fu6,1252) (tfreq(i)*autocm,i=1,nfreq(k))                   0211YC97
      if(iprint.eq.1) then
         write(fu6,1253)
C
       DO 110 I = 1,nfreq(k)                                            0211YC97
         XYZ(1,I) = 'X'                                                 0211YC97
         XYZ(2,I) = 'Y'                                                 0211YC97
         XYZ(3,I) = 'Z'                                                 0211YC97
 110   CONTINUE                                                         0211YC97
        do 40 m=1,nfreq(k),2                                            0211YC97
            lstr = m
            lend = m+1                                                  0211YC97
            if(lend .gt. nfreq(k)) lend = nfreq(k)                      0211YC97
            if (lend.eq.lstr) then                                      0211YC97
               write (fu6,1259) lstr                                    0211YC97
            else                                                        0211YC97
               write(FU6,1260) lstr, lend                               0211YC97
            endif                                                       0211YC97
            write(fu6,1261)(tfreq(II)*AUTOCM,II=LSTR,LEND)              0211YC97
            WRITE(FU6,1262) ((XYZ(MM,LL),MM=1,3),LL=LSTR,LEND)          0211YC97
            nat = 1                                                     0211YC97
            do 41 i=1,3*na(k),3
              jstr = I
              jend = I+2
              write(FU6,1254) index(k,nat),                             0211YC97
     *                 ((tcof(j,l),j=jstr,jend),l=lstr,lend)

            nat = nat+1                                                 0211YC97
  41        continue
            write(FU6,1255)                                             0603YC96
  40    continue
      endif
      return
1249  format(3(F7.3,2X),3X)                                             0211YC97
1250  format(/,1X,'IMAGINARY FREQUENCY (CM**-1)',//,10X,F10.2,'i')      0605YC96
1251  format(/,1X,'NORMALIZED EIGENVECTOR')
1252  format(/,1X,'VIBRATIONAL FREQUENCIES (CM**-1)',/,(1X,5F10.2))
1253  format(/,1X,'NORMALIZED EIGENVECTORS')
1254  FORMAT(1X,I3,5X,2(3(F7.3,2X),3X))                                 0211YC97
1255  FORMAT(/)
1259  FORMAT(1X,4HMode,15X,I3)                                          0211YC97
1260  FORMAT(1X,4HMode,15X,I3,27X,I3)                                   0211YC97
1261  FORMAT(1X,'omega (cm**-1)',2X,F9.2,1X,(19X,F9.2,1X))              0211YC97
1262  FORMAT(/,1X,4HAtom,2(3(8X,A1),3X))                                0211YC97
      end SUBROUTINE getfrq
c***********************************************************************
c     givtst
c***********************************************************************
      SUBROUTINE givtst(nfcvt,isct,izct,
     > lorder,smaxg,sming,slm,slp,del,nede,ele,
     > nratom,iatsv,xr,icod,nq12,nq22,ntemp,tem,
     > redm,sigmaf,sigmar,amasd,iclasv,                                 0528JC97
     > iunit6,gufac6)
      use gtst
      use gtst1
      use perconparam
c
c     This is a driver for interpolated vtst (global methods).
c     Rates calculations assume equilibrium geometry and
c     vibrational frequencies at the reactants, transition state, 
c     products, and one point on the mep are available. The codes
c     were written originally by Truong and Gonzalze-Lafont (KAN 7/95)
c
c     CALLED BY:
c                MAIN
c
c
      implicit double precision (a-h,o-z)
c
      dimension na(10),x(natoms),y(natoms),z(natoms),amass(natoms),
     *          index(10,natoms),tempge(3*natoms)
      dimension  ndeg(5,3),amasr(10),qv(10),zrpt(10)
      dimension  icode(10),elec(5,3),nfreq(10)
      dimension freqp(2*nfre),freqr(2*nfre),frets(nfre)
      dimension amasd(natoms),tem(40),icod(5),xr(maxcar,5)
      dimension iatsv(natoms,4),nratom(4),nede(15),ele(15)
      real(8) :: rot(3,3)
      character*20 unit(2)
      character*80 word(40)
      logical lsec,leof,lfreq
      data unit(1) /'(sec**-1)           '/,
     *     unit(2) /'(cm**3/molecule-sec)'/
c
c
c  AMASD - Sqrt(xmass(i)/redm) (pass in from main)
c  AMASS  - Atomic mass of atoms
c  AMASR  - Molecular mass 
c  ICODE  values from main.
c         1 - Atomic species
c         2 - Special Diatomic species
c         3 - Linear polyatomic species
c         4 - Non-linear polyatomic species
c  ZRPTR  is  zero-point energy of reactants
c  BK     is Boltzmann constant in au unit
c  RCONST is gas constant in kcal/mol-K
c  CONK0  is k0 in  au^3 (value of reaction quotient at standard state)
c  CNVRT  is convertion factor of the rate from au to cgs
c  AMAU   is convertion factor mass to au
c  ABOHR  is convertion factor from angstrom to bohr
c
      data ABOHR /1.88972652D0/
      data AMAU  /1822.844987D0/
      call givtst_mem
c
c fu29  = 29
c
      ir = 29
      write(fu6,999)
      smin = slp
      smax = slm
      dels = del
      nqk = nq12 
      nqt = nq22 
      nr = 1
      np = 1
      if(icod(1).ne.0.and.icod(2).ne.0) nr = 2
      if(icod(3).ne.0.and.icod(4).ne.0) np = 2
      call rgen(labini)
c
      l = 0
      do k = 1,5
         if(k.le.4) na(k) = nratom(k)
         icode(k) = icod(k)
c 
c        This version of IVTST can't handle special treatment for
c        diatomics. The ICODE 
c            1 - Atomic species
c            2 - Linear polyatomic species
c            3 - Linear polyatomic species
c            are used.
c
         if(icod(k).eq.4) icode(k) = 3
         if(icod(k).eq.3) icode(k) = 2
         do i = 1,3
            l = l + 1
            ndeg(k,i) = nede(l)
            elec(k,i) = ele(l)
         enddo
      enddo
c
      do i = 1,ntemp
         t(i)  = tem(i)
      enddo
      do i = 1,natom
         amass(i) = redm*amasd(i)*amasd(i)
      enddo
c
c     compute the relative translational reduced mass for reactants
c
      r1mas = 0.0D0                                                     05/96/KN
      do ims = 1,nratom(1)                                              05/96/KN
         r1mas = r1mas + amass(iatsv(ims,1))                            05/96/KN
      enddo                                                             05/96/KN
      r2mas = 0.0D0                                                     05/96/KN
      do ims = 1,nratom(2)                                              05/96/KN
         r2mas = r2mas + amass(iatsv(ims,2))                            05/96/KN
      enddo                                                             05/96/KN
      redmf = (r1mas*r2mas)/(r1mas+r2mas)                               0612YC96
c
c  Initialize zero-point energy
c
      zrptr = 0.0D0
      zrptp = 0.0D0
c
c loop over the reactants, product and transition states, and extra points
c
c
      do 90 k = 1, 5+lorder
          if((k.eq.2.and.nr.eq.1).or.(k.eq.4.and.np.eq.1)) goto 90
c
c          Read geometry
c
          if(k.eq.1 .or. k.eq.3 .or. k.ge.5) then
             if(k.gt.5) then
c              Read in the E1GEOM section name
                call readln(ir,word,nword,lsec,leof)
                If(.not. lsec .or. word(1) .ne. 'E1GEOM') then
                   write(fu6,6000)
                   write(fu6,7000) word(1)
                   stop
                endif
               read(ir,*)(tempge(im), im = 1,3*natom)
             endif
             do 10 i = 1,natom
                if(k.gt.5) then
c                  read(ir,*) x(i),y(i),z(i)
c                   call readln(ir,word,nword,lsec,leof)
c                   if(.not.lsec.and..not.leof) then
c                     x(i) = cfloat(word(1))
c                     y(i) = cfloat(word(2))
c                     z(i) = cfloat(word(3))
                   x(i) = tempge(i*3-2)
                   y(i) = tempge(i*3-1)
                   z(i) = tempge(i*3)
c                   endif
                   if (labini.eq.1) then
                     x(i) = x(i)*abohr
                     y(i) = y(i)*abohr
                     z(i) = z(i)*abohr
                   endif
                else
                   x(i) = xr(i*3-2,k)
                   y(i) = xr(i*3-1,k)
                   z(i) = xr(i*3,k)
                endif
   10        continue
c           
c            Read END of E1GEOM
             if(k.gt.5) then
               call readln(ir,word,nword,lsec,leof)
               If(lsec .or. word(1) .ne. 'END') then
                  write(fu6,8000)
                  write(fu6,7000) word(1)
                  stop
               endif
             endif
          endif
c
c
          if (k .le. 4) then
             do 12 i = 1,natom
                 do 14 l = 1,na(k)
                    index(k,l) = iatsv(l,k)
   14            continue
   12         continue
          elseif(k .ge. 5) then
             icode(k) = icode(5)
             na(k) = natom
             ncoord = 3*na(k)
             do 15 i = 1,natom
               index(k,i) = i
15           continue
          endif
c
c         Calculate number of internal vibrations
c
          call numfrq(k,icode(k),nfreq,na(k))
c
          if (k .le. 2) write(fu6,1050) k
          if (k.eq.3) write(fu6,1055) 1
          if (k.eq.4) write(fu6,1055) 2
          if (k .eq. 5) write(fu6,1060)
          if (k .ge. 6) write(fu6,1070)
C
          write(fu6,1080)
          sum = 0.0d0
          do 20 i = 1,na(k)
             j = index(k,i)
             write(fu6,1100) amass(j)/amau,x(j),y(j),z(j)
             sum = sum + amass(j)
   20     continue
c
c         Molecular mass in au
c
          amasr(k) = sum
c
c Read gradients and transform into mass scaled coordinate if needed
c
          if(k.ge.5) call rgrd(k,labini,ncoord,na,index,redm,
     *                          x,y,z,amass)
          if(icode(k).ne.1) then
             iflag = 0
             amasrk = amasr(k)
c
c            Calculate the moment of inertia or its product of reactant K
c
             rot=0.d00
             call inerta(k,icode(k),iflag,amasrk,fmome,na,x,y,z,amass,
     *       index,redm,rot)
             fmom(k) =  fmome
             if (icode(k).LE.2) then                                    06/96ELC
              write(fu6,1224) fmome                                     06/96ELC
             else                                                       06/96ELC
              write(fu6,1225) fmome                                     06/96ELC
             endif                                                      06/96ELC
c
c             Read frequencies
c   
             if(na(k) .ge.2) then
c
                call rhfsec(k,lfreq)
c
                if(lfreq) then
c
c Read frequencies
c modified rfreq to read in the imaginary frequency
c
                   call rfreq(k,nfreq,wi)                               0606YC96
                else
c
c Read and save hessian matrix for Page McIver
c curvature calculations and Get the frequencies
c
                   call rhess(k,labini,na,index,x,y,z,amass,redm)
                   call getfrq(k,icode(k),nfreq,amasrk,wi,redm,na,
     *                        index,x,y,z,amass,rot)
                endif 
c               Sum up the frequencies
                call zrpte(k,nfreq,zrpt)
             endif
          endif
c
c  Evaluate the zero-point energy of reactants and products
c
       if(k.le.2.) zrptr = zrptr + zrpt(k)
       if(k.gt.2.and.k.lt.5) zrptp = zrptp + zrpt(k)
c
 90    continue
c  
c   end of looping of the geometries
c
       write(fu6,1210) vc,dele,zrptp*ckcal,zrptr*ckcal
c
1210  format(/,64('-'),//,T22,'Reaction Energetics',//,64('-')//,       06/96ELC
     *      1X,'Classical:',/,2x,'Barrier (V(s=0)-V^R):',               06/96ELC
     *      T45,F10.4,' kcal/mol ',/,2X,
     * 'Endoergicity (V^P-V^R):',T45,F10.4,' kcal/mol',                 06/96ELC
     * /,2X,'Zero point energy (ZPE) Products',T45,F10.4,' kcal/mol',   06/96ELC
     * /,2X,'Zero point energy (ZPE) Reactants',T45,F10.4, ' kcal/mol') 09/96ELC
c
c     compute Delta H from frequencies if not given
c
      if(delh.eq.0.0) then
         delh = dele/ckcal + zrptp - zrptr
      endif
      vap = zrptr + delh                                                0612YC96
      var = zrptr                                                       0612YC96
      vag = vc/ckcal + zrpt(5) - zrptr                                  0613YC96
      write (fu6,1258) vag*ckcal                                        0613YC96
      write (fu6,1259) delh*ckcal                                       0612YC96
c
c  Read limits for interpolation and for Delta G calculation
C
      write(fu6,1255) redm/amau
1255  format(/,1x,'Forward reduced mass',T45,F10.6,' amu')
      write(fu6,1256) fmome
1256  format(1x,'Extrapolate moment of inertia from',T45,
     > e15.6)
C
c  if lorder=1, calculate the geometry at s = -ds
c
      if (lorder .eq. 1) then
          do 95 i = 1,natom
            x(i) = sqrt(redm/amass(i))*(2.0d0*x0(1,i)-x0(2,i))
            y(i) = sqrt(redm/amass(i))*(2.0d0*y0(1,i)-y0(2,i))
            z(i) = sqrt(redm/amass(i))*(2.0d0*z0(1,i)-z0(2,i))
  95      continue
          iflag = 1; rot=0.d00
          call inerta(6,icode(6),iflag,amasr(6),fmome,na,x,y,z,amass,
     *       index,redm,rot)
          fmom(7) = fmome
       endif
c
c  Interpolate the generalized frequencies and moment of inertia
c  and the derivative of the eigenvectors orthogonal to the
c  reaction path
c
       if (lorder .eq. 0) then
           delht = dele/ckcal + zrptp - zrptr
           sma = 0.0d0
           vamax = vag + zrptr 
           if(izct.eq.1) call irphz(redm,wi,freqr,freqp,frets,zrptr,
     >                lorder,nfreq,npts,nshf,iunit6,gufac6)             0405JZ07
       endif
       if (lorder .eq. 1) then
           call irph(redm,wi,zrptr,lorder,nfreq,npts,nshf,iunit6,gufac6)0405JZ06
c           Calculate max Vag(s).
            call fitmx(vad,sx,npts,1,npts,smax,vamax,imax)
            vag = vamax
c
c           Save the VAD grid for fitting the Eckart function to the
c           VAD curve in the Eckart-parabolic potential.
c
            if(abs(smax - sx(nshf)) .gt. 0.5d0*dels) then
                x4 =  - smax
                v4 = vad(nshf)
            else
                x4 = ds - smax
                v4 = vmep/ckcal + zrpt(6) - zrptr
            endif
       endif
       
C      write(fu6,1260) smax,vamax*ckcal
       if(iunit6.eq.1) write(fu6,1260) smax/gufac6,vamax*ckcal          0405JZ07
       if(iunit6.eq.0) write(fu6,1270) smax/gufac6,vamax*ckcal          0405JZ07
c
c  Loop over temperature
c
       do 400 it = 1,ntemp
          tt  = t(it)
          bkt = bk*tt
          rt  = rconst*tt
          write(fu6,2000) tt
c
c         Calculate the translational partion function
c
          if (nr .eq. 2) then
             qtr = conk0*(redmf*bkt/(2.0d0*pi))**1.5d0
          else
             qtr = 1.0d0
          endif
C
C         Calculate the vibrational partition function
C
          do 100 k = 1,5
              nf   = nfreq(k)
              prod = 1.0d0
              if (nf .eq. 0) goto 222
              do 200 l = 1,nf
                prod = prod*vparti(freq(k,l),bkt,iclasv)                0808JC00
  200         continue
  222         continue
            qv(k) = prod
  100     continue
C
C         Calculate the reactant and transition state partion functions
C
          if(nr .gt. 1) then
             qvr = qv(1)*qv(2)
             qr1 = rparti(fmom(1),bkt,icode(1))
             qr2 = rparti(fmom(2),bkt,icode(2))
             qrr = qr1*qr2
             qer = eparti(1,bkt,ndeg,elec)*eparti(2,bkt,ndeg,elec)
          else
             qvr = qv(1)
             qrr = rparti(fmom(1),bkt,icode(1))
             qer = eparti(1,bkt,ndeg,elec)
          endif
C
          phir = qtr*qrr*qvr*qer
          frefac = cnvrt*sigmaf*bkt/(2.d0*pi)
c
c         Output for individual reactant partition functions.
c
          if(iprint.eq.1) then
             write(fu6,2100)
             if(nr .gt. 1) then
               write(fu6,2150) qer,qtr,qr1*qr2,qv(1)*qv(2),phir
             else
               write(fu6,2300)
               write(fu6,2400) qtr,qrr,qvr,qer,phir
             endif
             write(fu6,2110)
             write(fu6,2160)
          endif
c
c   qet    -   electronic partition function of transition state
c   qrt    -   rotational partition function of transition state
c   qv(5)  -   vibrational partition function of transition state
c
          qet = eparti(5,bkt,ndeg,elec)
          if(lorder .eq. 0) then
             qrt = rparti(fmom(5),bkt,icode(5))
             qvt = qv(5)
             qgt = qet*qrt*qvt
             if (iprint.eq.1) then
                 write (fu6,2350)
                 write (fu6,2400) qet,qrt,qvt,qgt,frefac
             endif
             delg0 = rt*((vc/ckcal/bkt) - log(qgt) + log(phir))
             conf(it) = frefac*exp(-delg0/rt)
             tcag(it) = 1.0d0                                           0620YC96
             write(fu6,2650) 0.0,conf(it),unit(nr)
             goto 400
          endif
          if(iprint.eq.1) write(fu6,2500)
c
c         Find the limits of s for the Delta G calculation
c
          nptl = int(sming/dels)
          sming =nptl*dels
          nptr = int(smaxg/dels)
          smaxg = nptr*dels
          nptsg = nptr - nptl + 1
C
          J=1
105       continue
          if (sming .le. sx(j)) go to 110
          j = j + 1
          go to 105
110       continue
          istart = j
          j = 1
115       continue
          if (smaxg .le. sx(j)) go to 120
          j = j + 1
          goto 115
120       continue
          istop = j
c
c         Loop over s
c
          do 300 is = istart,istop
             qrt = rparti(gfmom(is),bkt,icode(5))
             nf = nfreq(6)
             vclass = vs(is)
             prod = 1.0d0
             do 350 l = 1,nf
               qvs = vparti(gfreq(l,is),bkt,iclasv)                     0808JC00
               prod = prod*qvs
350          continue
             qvt = prod
             qgt = qet*qrt*qvt
c
c            Calculate Delta G
c
             delg(is) = rt*((vclass/bkt) - log(qgt) + log(phir))
             if(iprint.eq.1) then
                write(fu6,2600) sx(is),qet,qrt,qvt,qgt,delg(is)
             endif
300       continue
c
          conf(it) = frefac*exp(-delg(nshf)/rt)
          tcag(it) = exp((vad(nshf)-vag)/bkt)                           0620YC96
c
c         Find max Delta G.
c
          call fitmx(delg,sx,nptsg,istart,istop,s5,g5,imax)
          scvt(it) = s5
C
C     FIND VA AT S*CVT, with linear interpolation                       0620YC96
C 
          if (lorder.ge.1) then                                         0620YC96
            do i = 1, npts                                              0620YC96 
              if (sx(i).ge.s5) then                                      0620YC96
                sl = sx(i-1)                                             0620YC96
                vl = vad(i-1)                                           0620YC96
                vr = vad(i)                                             0620YC96
                sr = sx(i)                                               0620YC96
                goto 399                                                0620YC96
              endif                                                     0620YC96
            enddo                                                       0620YC96
399         v5 = vl + ((vr-vl)/(sr-sl)) * (s5-sl)                       0620YC96
C
C     COMPUTE CVT/CAG CORRECTION AND CVT RATES, See FINOUT
C
            cag(it) = exp((v5-vag)/bkt)                                 0620YC96
          endif                                                         0620YC96
          cvt(it) = frefac*exp(-g5/rt)                                  0620YC96
C         write(fu6,2700) scvt(it),cvt(it),unit(nr)                     0620YC96
          write(fu6,2700) scvt(it)/gufac6,cvt(it),unit(nr)              0405JZ07
400    continue                                                         0620YC96
C
C  Calculate transmission coefficients. Change print format             0620YC96
C
       if(isct.eq.1.or.izct.eq.1)  then
         write(fu6,2800)
         call kapa(wi,redm,ntemp,ncoord,lorder,npts,nfreq,iunit6,gufac6)0405JZ07
         write (fu6,3500) lorder,unit(nr)
         write (fu15,3500) lorder,unit(nr) 
         do I = 1,ntemp
           confw(i) = conf(i)*wig(i)
           confz(i) = conf(i)*tcag(i)*tcsag(i)                          0620YC96
           if (lorder.ge.1) then        
              cvtw(i) = cvt(i)*wig(i)
              cvtg(i) = cvt(i)*cag(i)                                   0620YC96
              cvtz(i) = cvtg(i)*tcsag(i)
              cvts(i) = cvtg(i)*tcscg(i)
           endif
           write(fu6,4000) t(i),conf(i),confw(i),confz(i)
           write(fu15,4010) t(i),conf(i),confw(i),confz(i)
         enddo
         if (lorder.ge.1) then
            if (iscsag.eq.0) then 
               write(fu6,3000) unit(nr)
               write(fu15,3000) unit(nr)
            else
               write(fu6,3100) unit(nr)
               write(fu15,3100) unit(nr)
            endif
           do i = 1,ntemp
             write(fu6,5000) t(i),cvt(i),cvtw(i),
     *                 cvtg(i),cvtz(i),cvts(i)
             write(fu15,5010) t(i),cvt(i),cvtw(i),
     *                 cvtg(i),cvtz(i),cvts(i)
           enddo
         endif
       endif
c
c print tst only
c
       if(nfcvt.eq.0)  then
       write(fu6,3500) unit(nr)
       write(fu15,3500) unit(nr)
        do i = 1,ntemp
             write(fu6,5200) t(i),conf(i)
             write(fu15,5210) t(i),conf(i)
         enddo
       endif
c
 999   format(/,1X,79(1H*)//,30X,'IVTST'/,10X,'INTERPOLATED VARIA',
     * 'TIONAL TRANSITION STATE THEORY',/)
1050   format(/,1X,20(1H*),' REACTANT',I4,/)
1055   format(/,1X,20(1H*),' PRODUCT',I4,/)
1060   format(/,1X,20(1H*),' SADDLE POINT ',/)
1070   format(/,1X,20(1H*),' EXTRA POINT ALONG THE MEP',/)
1080   format(20X,'CARTESIAN COORDINATES (A.U.)',//6X,'ATOMIC MASS',
     * ' (AMU)',12X,'X',9X,
     * 'Y',9X,'Z',/)
1100   format(5X,F10.4,15X,3F10.6)
1200   format(/,1X,'ELECTRONIC DEGENERACY AND ENERGY (A.U.):',//,
     * 3(I5,1X,F12.6))
1224   format(/,1X,'MOMENT OF INERTIA',24X,1PE15.8,' A.U.')             06/96ELC
1225   format(/,1X,'PRODUCT OF PRINCIPAL MOMENTS OF INERTIA:',2X,       06/96ELC
     *       1PE15.8,' A.U.')                                           06/96ELC
1258   format(1x,'Including ZPE:',/,2x,'Barrier (Va^G(s=0)-Va^GR):',    06/96ELC
     *        T45,F10.4,' kcal/mol')                                    06/96ELC
1259   format(2x,'Endoergicity (Va^GP-Va^GR):',                         06/96ELC
     *        T45,F10.4,' kcal/mol',/,64('-'))                          06/96ELC
1260   format(//,10X,'Vibrationally Adiabatic Ground-State Maximum',//
     * 10X,'s^AG = ',F10.6,' bohrs',3X,'Va^AG = ',F10.4,' kcal/mol')    06/96ELC
1270   format(//,10X,'Vibrationally Adiabatic Ground-State Maximum',//  
     * 10X,'s^AG = ',F10.6,' angstroms',3X,'Va^AG = ',F10.4,' kcal/mol')0405JZ07
2000   format(/,1X,10(1H*),'  TEMPERATURE T = ',F8.1,' K  ',10(1H*))    06/96ELC
2100   format(/1X,78(1H-)/,2X,                                           
     *'Reactant partition functions in CGS units (Phi)',/1X,78(1H-)/,
     *3X,'Qelec',5X,'Ptrans',7X,'Qrot',8X,
     *'Qvib',8X,'Phi')
2110   FORMAT(2X,'Note: Phi_rel is the relative translational',         06/96ELC
     * ' partition function',/,2x,'in CGS unit. (Ptrans = Phi trans)')
2150   FORMAT(F9.5,1P,4E12.4,/1x,78(1H-))       
2160   FORMAT(2X,'Note: Phi is the product of all the partition',    
     * ' functions.')                                 
2300   format(7X,'PTR',12X,'QRR',12X,'QVR',12X,'QER',12X,'PHI',/)
2350   format(7x,'QET',12X,'QRT',12X,'QVT',12X,'QGT',12X,'prefec',/)
2400   format(2X,7(1PE12.6,3X),//)
2500   format(/,10X,'Generalized T.S. partition functions and delta G',
     * ' (kcal/mol)  VS. s ',//,4X,'s',9X,'Qelec',10X,'Qrot',10X,'Qvib',
     * 10X,'Phi',11X,'DELG',/)
2600   format(1X,F6.3,2X,5(1PE12.5,2X))
2650   format(/,2X,'s* = ',F10.5,5X,'TST RATE CONSTANT = ',1PE12.4,  
     * 1X,A20/)
2700   format(/,2X,'s* = ',F10.5,5X,'CVT RATE CONSTANT = ',1PE12.4,
     * 1X,A20/)
2800   format(/,1X,5(1H*),' Vibrationally Adiabatic Ground-State ',
C     * 'Transmission Coefficients ',5(1H*)//,'MEPSAG:',/,'transmission',
     * 'Transmission Coefficients ',5(1H*)//,'ZCT:',/,'transmission',   0423TA02
     * ' coefficients for tunneling along the MEP w semiclassical',
     * ' app.')
3000   format(//,10X,'FINAL RATE CONSTANTS ',A20,//,
     *  2X,'T(K)',6X,'CVT',8X,'CVT/W',6X,'CVT/CAG',
C     *  4X,'CVT/MEPSAG',2X,'CVT/CD-SCSAG',/)
     *  5X,'CVT/ZCT',5X,'CVT/SCT',/)                                    0423TA02
3100   format(//,10X,'FINAL RATE CONSTANTS ',A20,//,
     *  2X,'T(K)',6X,'CVT',8X,'CVT/W',6X,'CVT/CAG',
C     *  4X,'CVT/MEPSAG',2X,'CVT/SCSAG',/)
     *  5X,'CVT/ZCT',5X,'CVT/SCSAG',/)                                  0423TA02
3500   format(//,10X,' FINAL RATE CONSTANTS ','IVTST-',I1,' ',A20,//,
C     * 2X,'T(K)',12X,'TST',10X,'TST/W',7X,'TST/MEPSAG',/)
     * 2X,'T(K)',12X,'TST',10X,'TST/W',8X,'TST/ZCT',/)                  0423TA02
4000   format(1X,F6.1,4X,3(4X,1PE10.4))
4010   format(1X,F6.1,3X,3(6X,1PE8.2))                                  0522TA02
5000   format(F6.1,1X,6(2X,1PE10.4))
5010   format(F6.1,6(4X,1PE8.2))                                        0522TA02
5200   format(F6.1,1X,2X,1PE10.4)
5210   format(F6.1,4X,1PE8.2)                                           0522TA02
6000   format(/1X,'expecting *E1GEOM for input')
7000   format(/1X,'Unrecoginized keyword: ',A12)
8000   format(/1X,'expecting END for input')
       return
       end subroutine givtst
c
c*******************************************************************
c     hybrd
c*******************************************************************
      SUBROUTINE hybrd(fcn,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,diag,
     *                 mode,factor,nprint,info,nfev,fjac,ldfjac,r,lr,
     *                 qtf,wa1,wa2,wa3,wa4)
      integer n,maxfev,ml,mu,mode,nprint,info,nfev,ldfjac,lr
      double precision xtol,epsfcn,factor
      double precision x(n),fvec(n),diag(n),fjac(ldfjac,n),r(lr),
     *                 qtf(n),wa1(n),wa2(n),wa3(n),wa4(n)
      external fcn
c
c
c     the purpose of hybrd is to find a zero of a system of
c     n nonlinear functions in n variables by a modification
c     of the powell hybrid method. the user must provide a
c     subroutine which calculates the functions. the jacobian is
c     then calculated by a forward-difference approximation.
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions see hybrid1.
c
c       n is a positive integer input variable set to the number
c         of functions and variables.

c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.

c       fvec is an output array of length n which contains
c         the functions evaluated at the output x.

c       xtol is a nonnegative input variable. termination
c         occurs when the relative error between two consecutive
c         iterates is at most xtol.

c       maxfev is a positive integer input variable. termination
c         occurs when the number of calls to fcn is at least maxfev
c         by the end of an iteration.

c       ml is a nonnegative integer input variable which specifies
c         the number of subdiagonals within the band of the
c         jacobian matrix. if the jacobian is not banded, set
c         ml to at least n - 1.

c       mu is a nonnegative integer input variable which specifies
c         the number of superdiagonals within the band of the
c         jacobian matrix. if the jacobian is not banded, set
c         mu to at least n - 1.

c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       diag is an array of length n. if mode = 1 (see
c         below), diag is internally set. if mode = 2, diag
c         must contain positive entries that serve as
c         multiplicative scale factors for the variables.
c
c       mode is an integer input variable. if mode = 1, the
c         variables will be scaled internally. if mode = 2,
c         the scaling is specified by the input diag. other
c         values of mode are equivalent to mode = 1.
c
c       factor is a positive input variable used in determining the
c         initial step bound. this bound is set to the product of
c         factor and the euclidean norm of diag*x if nonzero, or else
c         to factor itself. in most cases factor should lie in the
c         interval (.1,100.). 100. is a generally recommended value.
c
c       nprint is an integer input variable that enables controlled
c         printing of iterates if it is positive. in this case,
c         fcn is called with iflag = 0 at the beginning of the first
c         iteration and every nprint iterations thereafter and
c         immediately prior to return, with x and fvec available
c         for printing. if nprint is not positive, no special calls
c         of fcn with iflag = 0 are made.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0   improper input parameters.
c
c         info = 1   relative error between two consecutive iterates
c                    is at most xtol.
c
c         info = 2   number of calls to fcn has reached or exceeded
c                    maxfev.
c
c         info = 3   xtol is too small. no further improvement in
c                    the approximate solution x is possible.
c
c         info = 4   iteration is not making good progress, as
c                    measured by the improvement from the last
c                    five jacobian evaluations.
c
c         info = 5   iteration is not making good progress, as
c                    measured by the improvement from the last
c                    ten iterations.
c
c       nfev is an integer output variable set to the number of
c         calls to fcn.
c
c       fjac is an output n by n array which contains the
c         orthogonal matrix q produced by the qr factorization
c         of the final approximate jacobian.
c
c       ldfjac is a positive integer input variable not less than n
c         which specifies the leading dimension of the array fjac.
c
c       r is an output array of length lr which contains the
c         upper triangular matrix produced by the qr factorization
c         of the final approximate jacobian, stored rowwise.
c
c       lr is a positive integer input variable not less than
c         (n*(n+1))/2.
c
c       qtf is an output array of length n which contains
c         the vector (q transpose)*fvec.
c
c       wa1, wa2, wa3, and wa4 are work arrays of length n.
c
c       subprograms called
c
c       user-supplied ...... fcn

c       minpack-supplied ... dogleg,dpmpar,enorm,fdjac1,
c                            qform,qrfac,r1mpyq,r1updt
c
c       fortran-supplied ... dabs,dmax1,dmin1,min0,mod
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c*******************************************************************
      integer i,iflag,iter,j,jm1,l,msum,ncfail,ncsuc,nslow1,nslow2
      integer iwa(1)
      logical jeval,sing
      double precision actred,delta,epsmch,fnorm,fnorm1,one,pnorm,
     *                 prered,p1,p5,p001,p0001,ratio,sum,temp,xnorm,
     *                 zero
      double precision dpmpar,enorm
      data one,p1,p5,p001,p0001,zero
     *     /1.0d0,1.0d-1,5.0d-1,1.0d-3,1.0d-4,0.0d0/

c     epsmch is the machine precision.
c
      epsmch = dpmpar(1)

      info = 0
      iflag = 0
      nfev = 0

c     check the input parameters for errors.

      if (n .le. 0 .or. xtol .lt. zero .or. maxfev .le. 0
     *    .or. ml .lt. 0 .or. mu .lt. 0 .or. factor .le. zero
     *    .or. ldfjac .lt. n .or. lr .lt. (n*(n + 1))/2) go to 300
      if (mode .ne. 2) go to 20
      do 10 j = 1, n
         if (diag(j) .le. zero) go to 300
   10    continue
   20 continue

c     evaluate the function at the starting point
c     and calculate its norm.

      iflag = 1
      call fcn(n,x,fvec,iflag)
      nfev = 1
      if (iflag .lt. 0) go to 300
      fnorm = enorm(n,fvec)

c     determine the number of calls to fcn needed to compute
c     the jacobian matrix.

      msum = min0(ml+mu+1,n)

c     initialize iteration counter and monitors.

      iter = 1
      ncsuc = 0
      ncfail = 0
      nslow1 = 0
      nslow2 = 0

c     beginning of the outer loop.

   30 continue
         jeval = .true.

c        calculate the jacobian matrix.

         iflag = 2
         call fdjac1(fcn,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,wa1,
     *               wa2)
         nfev = nfev + msum
         if (iflag .lt. 0) go to 300

c        compute the qr factorization of the jacobian.

         call qrfac(n,n,fjac,ldfjac,.false.,iwa,1,wa1,wa2,wa3)

c        on the first iteration and if mode is 1, scale according
c        to the norms of the columns of the initial jacobian.

         if (iter .ne. 1) go to 70
         if (mode .eq. 2) go to 50
         do 40 j = 1, n
            diag(j) = wa2(j)
            if (wa2(j) .eq. zero) diag(j) = one
   40       continue
   50    continue

c        on the first iteration, calculate the norm of the scaled x
c        and initialize the step bound delta.

         do 60 j = 1, n
            wa3(j) = diag(j)*x(j)
   60       continue
         xnorm = enorm(n,wa3)
         delta = factor*xnorm
         if (delta .eq. zero) delta = factor
   70    continue

c        form (q transpose)*fvec and store in qtf.

         do 80 i = 1, n
            qtf(i) = fvec(i)
   80       continue
         do 120 j = 1, n
            if (fjac(j,j) .eq. zero) go to 110
            sum = zero
            do 90 i = j, n
               sum = sum + fjac(i,j)*qtf(i)
   90          continue
            temp = -sum/fjac(j,j)
            do 100 i = j, n
               qtf(i) = qtf(i) + fjac(i,j)*temp
  100          continue
  110       continue
  120       continue

c        copy the triangular factor of the qr factorization into r.

         sing = .false.
         do 150 j = 1, n
            l = j
            jm1 = j - 1
            if (jm1 .lt. 1) go to 140
            do 130 i = 1, jm1
               r(l) = fjac(i,j)
               l = l + n - i
  130          continue
  140       continue
            r(l) = wa1(j)
            if (wa1(j) .eq. zero) sing = .true.
  150       continue

c        accumulate the orthogonal factor in fjac.

         call qform(n,n,fjac,ldfjac,wa1)

c        rescale if necessary.

         if (mode .eq. 2) go to 170
         do 160 j = 1, n
            diag(j) = dmax1(diag(j),wa2(j))
  160       continue
  170    continue

c        beginning of the inner loop.

  180    continue

c           if requested, call fcn to enable printing of iterates.

            if (nprint .le. 0) go to 190
            iflag = 0
            if (mod(iter-1,nprint) .eq. 0) call fcn(n,x,fvec,iflag)
            if (iflag .lt. 0) go to 300
  190       continue

c           determine the direction p.

            call dogleg(n,r,lr,diag,qtf,delta,wa1,wa2,wa3)

c           store the direction p and x + p. calculate the norm of p.

            do 200 j = 1, n
               wa1(j) = -wa1(j)
               wa2(j) = x(j) + wa1(j)
               wa3(j) = diag(j)*wa1(j)
  200          continue
            pnorm = enorm(n,wa3)

c           on the first iteration, adjust the initial step bound.

            if (iter .eq. 1) delta = dmin1(delta,pnorm)

c           evaluate the function at x + p and calculate its norm.

            iflag = 1
            call fcn(n,wa2,wa4,iflag)
            nfev = nfev + 1
            if (iflag .lt. 0) go to 300
            fnorm1 = enorm(n,wa4)

c           compute the scaled actual reduction.

            actred = -one
            if (fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2

c           compute the scaled predicted reduction.

            l = 1
            do 220 i = 1, n
               sum = zero
               do 210 j = i, n
                  sum = sum + r(l)*wa1(j)
                  l = l + 1
  210             continue
               wa3(i) = qtf(i) + sum
  220          continue
            temp = enorm(n,wa3)
            prered = zero
            if (temp .lt. fnorm) prered = one - (temp/fnorm)**2

c           compute the ratio of the actual to the predicted
c           reduction.

            ratio = zero
            if (prered .gt. zero) ratio = actred/prered

c           update the step bound.

            if (ratio .ge. p1) go to 230
               ncsuc = 0
               ncfail = ncfail + 1
               delta = p5*delta
               go to 240
  230       continue
               ncfail = 0
               ncsuc = ncsuc + 1
               if (ratio .ge. p5 .or. ncsuc .gt. 1)
     *            delta = dmax1(delta,pnorm/p5)
               if (dabs(ratio-one) .le. p1) delta = pnorm/p5
  240       continue

c           test for successful iteration.

            if (ratio .lt. p0001) go to 260

c           successful iteration. update x, fvec, and their norms.

            do 250 j = 1, n
               x(j) = wa2(j)
               wa2(j) = diag(j)*x(j)
               fvec(j) = wa4(j)
  250          continue
            xnorm = enorm(n,wa2)
            fnorm = fnorm1
            iter = iter + 1
  260       continue

c           determine the progress of the iteration.

            nslow1 = nslow1 + 1
            if (actred .ge. p001) nslow1 = 0
            if (jeval) nslow2 = nslow2 + 1
            if (actred .ge. p1) nslow2 = 0

c           test for convergence.

            if (delta .le. xtol*xnorm .or. fnorm .eq. zero) info = 1
            if (info .ne. 0) go to 300

c           tests for termination and stringent tolerances.

            if (nfev .ge. maxfev) info = 2
            if (p1*dmax1(p1*delta,pnorm) .le. epsmch*xnorm) info = 3
            if (nslow2 .eq. 5) info = 4
            if (nslow1 .eq. 10) info = 5
            if (info .ne. 0) go to 300

c           criterion for recalculating jacobian approximation
c           by forward differences.

            if (ncfail .eq. 2) go to 290

c           calculate the rank one modification to the jacobian
c           and update qtf if necessary.

            do 280 j = 1, n
               sum = zero
               do 270 i = 1, n
                  sum = sum + fjac(i,j)*wa4(i)
  270             continue
               wa2(j) = (sum - wa3(j))/pnorm
               wa1(j) = diag(j)*((diag(j)*wa1(j))/pnorm)
               if (ratio .ge. p0001) qtf(j) = sum
  280          continue

c           compute the qr factorization of the updated jacobian.

            call r1updt(n,n,r,lr,wa1,wa2,wa3,sing)
            call r1mpyq(n,n,fjac,ldfjac,wa2,wa3)
            call r1mpyq(1,n,qtf,1,wa2,wa3)

c           end of the inner loop.

            jeval = .false.
            go to 180
  290    continue

c        end of the outer loop.

         go to 30
  300 continue

c     termination, either normal or user imposed.

      if (iflag .lt. 0) info = iflag
      iflag = 0
      if (nprint .gt. 0) call fcn(n,x,fvec,iflag)
      return
      end SUBROUTINE hybrd
C*******************************************************************
c     hybrd1
C*******************************************************************
      SUBROUTINE hybrd1(fcn,n,x,fvec,tol,info,wa,lwa)
C
      integer n,info,lwa
      double precision tol
      double precision x(n),fvec(n),wa(lwa)
      external fcn
c
c
c     the purpose of hybrd1 is to find a zero of a system of
c     n nonlinear functions in n variables by a modification
c     of the powell hybrid method. this is done by using the
c     more general nonlinear equation solver hybrd. the user
c     must provide a subroutine which calculates the functions.
c     the jacobian is then calculated by a forward-difference
c     approximation.
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
c
c         subroutine fcn(n,x,fvec,iflag)
c         integer n,iflag
c         double precision x(n),fvec(n)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ---------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of hybrd1.
c         in this case set iflag to a negative integer.
c
c       n is a positive integer input variable set to the number
c         of functions and variables.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length n which contains
c         the functions evaluated at the output x.
c
c       tol is a nonnegative input variable. termination occurs
c         when the algorithm estimates that the relative error
c         between x and the solution is at most tol.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0   improper input parameters.
c
c         info = 1   algorithm estimates that the relative error
c                    between x and the solution is at most tol.
c
c         info = 2   number of calls to fcn has reached or exceeded
c                    200*(n+1).
c
c         info = 3   tol is too small. no further improvement in
c                    the approximate solution x is possible.
c
c         info = 4   iteration is not making good progress.
c
c       wa is a work array of length lwa.
c
c       lwa is a positive integer input variable not less than
c         (n*(3*n+13))/2.
c
c        subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... hybrd
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more

c*******************************************************************
      integer index,j,lr,maxfev,ml,mode,mu,nfev,nprint
      double precision epsfcn,factor,one,xtol,zero
      data factor,one,zero /1.0d2,1.0d0,0.0d0/
      info = 0

c     check the input parameters for errors.

      if (n .le. 0 .or. tol .lt. zero .or. lwa .lt. (n*(3*n + 13))/2)
     *   go to 20

c     call hybrd.

      maxfev = 200*(n + 1)
      xtol = tol
      ml = n - 1
      mu = n - 1
      epsfcn = zero
      mode = 2
      do 10 j = 1, n
         wa(j) = one
   10    continue
      nprint = 0
      lr = (n*(n + 1))/2
      index = 6*n + lr
      call hybrd(fcn,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,wa(1),mode,
     *           factor,nprint,info,nfev,wa(index+1),n,wa(6*n+1),lr,
     *           wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1))
      if (info .eq. 5) info = 4
   20 continue
      return

      end subroutine hybrd1
C
C****************************************************************
C INERTA 
C****************************************************************
      SUBROUTINE inerta(K,IOP,IFLAG,SUM,FMOM,na,x,y,z,amass,
     * index,redm,rot)
      use perconparam
      use gtst1
c
c    Calculate the moment of inertia  for linear molecule or
c    diatomic, or the product of moment of inertia for non-
c    linear molecule in AU.
c
c     CALLED BY:
c                GIVTST
c
c     CALLS:
c                MXLNEQ
c
      implicit double precision (a-h,o-z)
C
      real(8), intent(out) :: rot(3,3)
      dimension na(10),x(natoms),y(natoms),z(natoms),amass(natoms),
     *              index(10,natoms)
      DIMENSION ISCR(5)
      DIMENSION IND(NATOMS)
C
      IF (IFLAG. EQ. 0) THEN
          NAT = NA(K)
          DO 5 I = 1,NAT
             IND(I) = INDEX(K,I)
5         CONTINUE
       ELSE
          NAT = NATOM
          DO 10 I = 1,NAT
             IND(I) = I
10        CONTINUE
       ENDIF
C
C calculate the c. o. m.
C
      SUM1 = 0.0D0
      SUM2 = 0.0D0
      SUM3 = 0.0D0
      DO 20 I = 1,NAT
         J = IND(I)
         SUM1 = SUM1 + AMASS(J)*X(J)
         SUM2 = SUM2 + AMASS(J)*Y(J)
         SUM3 = SUM3 + AMASS(J)*Z(J)
20    CONTINUE
        XCM = SUM1/SUM
        YCM = SUM2/SUM
        ZCM = SUM3/SUM
C
      DO 25 I = 1,NAT
         J = IND(I)
         AMASSF(J) = AMASS(J)/REDM
25    CONTINUE
      IF (IOP .EQ. 2) THEN
C
C LINEAR MOLECULE
C
        SUM = 0.0D0
        DO 30 I = 1,NAT
          J = IND(I)
          D2 = (X(J)-XCM)**2 + (Y(J)-YCM)**2 + (Z(J)-ZCM)**2
          SUM = SUM + AMASS(J)*D2
30      CONTINUE
        FMOM = SUM
      ELSE
C
C NON-LINEAR  MOLECULE . FMOM IS ACTUALLY THE PRODUCT OF ALL
C THREE PRINCIPLE MOMENT OF INERTIAS IA*IB*IC.
C
         ROT = 0.0D0
C
         DO 50 I = 1,NAT
            J = IND(I)
            ROT(1,1)=ROT(1,1)+AMASSF(J)*((Y(J)-YCM)**2+(Z(J)-ZCM)**2)
            ROT(2,2)=ROT(2,2)+AMASSF(J)*((X(J)-XCM)**2+(Z(J)-ZCM)**2)
            ROT(3,3)=ROT(3,3)+AMASSF(J)*((X(J)-XCM)**2+(Y(J)-YCM)**2)
            ROT(1,2)=ROT(1,2)-AMASSF(J)*(X(J)-XCM)*(Y(J)-YCM)
            ROT(1,3)=ROT(1,3)-AMASSF(J)*(X(J)-XCM)*(Z(J)-ZCM)
            ROT(2,3)=ROT(2,3)-AMASSF(J)*(Y(J)-YCM)*(Z(J)-ZCM)
50       CONTINUE
         ROT(2,1) = ROT(1,2)
         ROT(3,1) = ROT(1,3)
         ROT(3,2) = ROT(2,3)
         CALL MXLNEQ(ROT,3,3,DET,JRANK,EPS,ISCR,0,3)
         IF (JRANK .LT. 3) THEN
           WRITE(fu6,*) ' PROBLEM WITH MXLNEQ IN SUBROUTINE INERTA '
           STOP 1
         END IF
          FMOM = DET*REDM**3
      ENDIF
      IF (K .GE. 5) THEN
         DO  60 I = 1,NATOM
            J = K - 4
            X0(J,I) = SQRT(AMASS(I)/REDM)*(X(I) - XCM)
            Y0(J,I) = SQRT(AMASS(I)/REDM)*(Y(I) - YCM)
            Z0(J,I) = SQRT(AMASS(I)/REDM)*(Z(I) - ZCM)
60       CONTINUE
      ENDIF
      RETURN
      END SUBROUTINE inerta
C*******************************************************************
C IRPH
C*******************************************************************
C
      SUBROUTINE irph(redm,wi,zrptr,lorder,nfreq,npts,nshf,
     *                iunit6,gufac6)
      use perconparam
      use gtst
c
c     Interpolates s, Vmep, Vag, moment of inertia and generalized 
c     frequencies for First-order IVTST.
c
c     CALLED BY:
c               GIVTST
c     CALLS:
c               QUADR, ECKRT, ECKARS, VIVT, SORTG
c
      implicit double precision (a-h,o-z)
      save                                                              0601YC98
c     dimension freqp(2*nfre),freqr(2*nfre),iwe(nfre,nsdm),f(2),x(2)
c     dimension frets(nfre),fres1(nfre),wm(3)
      dimension nfreq(10)
      real(8), allocatable :: freqp(:),freqr(:),f(:),x(:)
      real(8), allocatable :: frets(:),fres1(:),wm(:)
      integer, allocatable :: iwe(:,:)
      data  cmcal /349.7551d0/
      if(.not.allocated(freqp)) then
        allocate(freqp(2*nfre),freqr(2*nfre),iwe(nfre,nsdm),f(2),x(2))
        freqp=0d0; freqr=0d0; iwe=0; f=0d0; x=0d0;
        allocate(frets(nfre),fres1(nfre),wm(3))
        frets=0d0; fres1=0d0; wm=0d0
      endif
c
      nptl = int(smin/dels)
      smin = nptl*dels
      nshf = -nptl + 1
      nptr = int(smax/dels)
      smax = nptr*dels
      npts = nptr-nptl + 1
c
c  Interpolate s
c
      do 10 i = 1,npts
        sx(i) = smin + (i-1)*dels
10    continue
c
c  Interpolate the moment of inertia and generalized frequencies.
c
      x(1) = ds
      f(1) = fmom(6)
      x(2) = -ds
      f(2) = fmom(7)
      c2 =   fmom(5)
      call quadr(f,x,a2,b2,c2)
      write(fu6,400) a2,b2,c2
400   FORMAT(//1X,'Quadratic coefs for I(S):',1X,'A =',
     *       1PE12.5,1X,'lambda =',E12.5,1X,'chi =',E12.5/)             09/96ELC
c
      do 20 is = 1,npts
        gfmom(is) =  a2*sx(is)**2 + b2*sx(is) + c2
  20  continue
c
c  Interpolate Vc(s) (VMEP) by a 4 points fit Eckart fxn.
c
      x4 = ds
      v4 = vmep/ckcal
c
c interpolate mep
c
      call eckars(1,redm,wi,lorder)
      do 30 i = 1,npts
           vs(i) = vivt(sx(i))
c
c          Set VAD initially equal to VS
c
           vad(i) = vs(i)
  30  continue
c
c  Interpolate generalized frequencies.
c      Order frequencies
c
      nfrer = nfreq(1) + nfreq(2)
      nfrep = nfreq(3) + nfreq(4)
      do 40 i = 1,nfrer
          if (i .le. nfreq(1)) then
             freqr(i) = freq(1,i)
          else
             k = i - nfreq(1)
             freqr(i) = freq(2,k)
          endif
40    continue
      call sortg(nfrer,freqr)
      do 50 i = 1,nfrep
         if (i .le. nfreq(3)) then
            freqp(i) = freq(3,i)
         else
            k = i - nfreq(3)
            freqp(i) = freq(4,k)
         endif
50    continue
      call sortg(nfrep,freqp)
      nfgt = nfreq(6)
      do 60 i = 1,nfgt
         frets(i) = freq(5,i)
         fres1(i) = freq(6,i)
60    continue
      call sortg(nfgt,frets)
      call sortg(nfgt,fres1)
C
      write(fu6,450)
450   format(//,10(1H*),' INTERPOLATING FREQUENCIES ',10(1H*),//,
     * 2X,'Eckart function: omega(s) =  ay/(1+y) + by/(1+y)**2 + c',    06/96ELC
     * 4X,' y = EXP((s-so)/l)',//,2X,'Hyperbolic TAN: omega(s)',        06/96ELC
     * ' =  a*TANH((s-s0)/l) + c ',//)                                  06/96ELC
C
      write(fu6,*)' Frequencies of reactant(s)'
      write(fu6,3000) (freqr(i)*autocm,i=1,nfrer)
      if(ds.lt.0.0) then
         write(fu6,*)' Frequencies of extra point number 1'
         write(fu6,3000) (fres1(i)*autocm,i=1,nfgt)
         write(fu6,*)' Frequencies of transition state'
         write(fu6,3000) (frets(i)*autocm,i=1,nfgt)
      else
         write(fu6,*)' Frequencies of transition state'
         write(fu6,3000) (frets(i)*autocm,i=1,nfgt)
         write(fu6,*)' Frequencies of extra point number 1'
         write(fu6,3000) (fres1(i)*autocm,i=1,nfgt)
      endif
      write(fu6,*)' Frequencies of product(s)'
      write(fu6,3000) (freqp(i)*autocm,i=1,nfrep)
3000  format(1x,11f7.1)
C
      wm(1) = freqr(1)
      wm(2) = freqp(1)
      wm(3) = frets(1)
C
C  Find the maximum value of the frequencies
C
      call sortg(3,wm)
      wmax = wm(1)*ckcal
C
      do 15 j=1,10
        do 25 i=1,nfgt
         par(i,j) = 0.d0
25      continue
15    continue
C
      do 70 i = 1,nfgt
         write(fu6,500) i
 500  format(//,40(1h*),5x,'Interpolating mode',i5)
C
         sz(1) = 0.0d0
         sz(2) = ds
         fz(1) = frets(i)*ckcal
         fz(2) = fres1(i)*ckcal
         fr = freqr(i)*ckcal
         fp = freqp(i)*ckcal
         alf =gammav
         npt = 2
         lstep = 0
         maxind = 500                                                   1012BE06
80       call eckrt(lorder)
c
c  Save the parameters of each interpolating function
c
c normal termination with info = 1 for MINPACK 
         if (info .eq. 0) then
            par(i,1) = ifcn
            par(i,2) = A
            par(i,3) = B
            par(i,4) = C
            if (ifcn .eq. 0) par(i,5) = gamma
            if (ifcn .ne. 0) then
              par(i,6) = sm
              par(i,7) = alf
            end if
          end if
C
C  Compute the max of the eckart to see if it > wmax   or
C  if it cannot find a solution to the non-linear system of equations.
C  take the nearest reactant or product endpoint.
C
          if(ifcn .ne. 0) emax = (a+b)**2/(4.d0*b)+c
c
          if(emax .gt. wmax .or. info .gt. 0) then
c
              if( ds .gt. 0.0d0) then
                    call nearw(i,freqr,frets,dfr1,0,lstep)
              else
                    call nearw(i,freqp,frets,dfp1,0,lstep)
              endif
              lstep = lstep + 1
c
              if (lstep .le. maxind) then
c
                  if(ds .gt. 0.d0) then
                   fr = freqr(i)*ckcal
                   write(fu6,*)' new reactant end point w =',fr*cmcal
                  else
                   fp = freqp(i)*ckcal
                   write(fu6,*)' new product end point w =',fp*cmcal
                  endif
                goto 80
c
              else
c
                write(fu6,*) ' unable to interpolate mode', i
                stop
              endif
c
          endif
c
c   Add zero-point energy to VAD
c
          do 110 is = 1,npts
             ss = sx(is)
             vad(is) = vad(is) + zerp(i,is,ss)
110       continue
c
70     continue
c
c      write(fu6,900) lorder,smin,smax,dels
       if(iunit6.eq.1) write(fu6,900) lorder,smin/gufac6,smax/gufac6,
     *                                dels/gufac6                       0405JZ07
       if(iunit6.eq.0) write(fu6,910) lorder,smin/gufac6,smax/gufac6,
     *                                dels/gufac6                       0405JZ07
       do 120 i = 1,npts
          do 130 k = 1,nfreq(6)
            iwe(k,i) = int(gfreq(k,i)*autocm)
130       continue
          write(fu6,1000) sx(i),vs(i)*ckcal,vad(i)*ckcal,
     *      gfmom(i),(iwe(j,i),j=1,nfreq(6))
120    continue
c
C       do 140 i= 1,npts                                                0613YC96
C           vad(i) = vad(i) - zrptr                                     0613YC96
C140    continue                                                        0613YC96
c
       return
 900   FORMAT(/,1X,15(1H*),3X,I1,'-ORDER INTERPOLATED RESULTS   ',      0603YC96
     * 15(1H*),//,10X,F6.3,' bohrs <  S  < ',F6.3,' bohrs',             06/96ELC
     *  '    STEP SIZE = ',F10.5,' bohrs',//,1X,T2,'s(bohr)',           06/96ELC
C    *   T14, 'VMEP', T23, 'Va^G',T34,'det I  (a.u.)',                  06/96ELC
C    *       T49,'    Frequencies (cm**-1)', /)                         06/96ELC
     *   T18, 'VMEP', T27, 'Va^G',T38,'det I  (a.u.)',                  0405JZ07
     *       T53,'    Frequencies (cm**-1)', /)                         0405JZ07
 910   FORMAT(/,1X,15(1H*),3X,I1,'-ORDER INTERPOLATED RESULTS   ',      0405JZ07
     * 15(1H*),//,10X,F6.3,' angstroms <  S  < ',F6.3,' angstroms',     
     *  '    STEP SIZE = ',F10.5,' angstroms',//,1X,T2,'s(angstrom)',   
     *   T18, 'VMEP', T27, 'Va^G',T38,'det I  (a.u.)',                  
     *       T53,'    Frequencies (cm**-1)', /)                         
1000   FORMAT(1X,F7.3,1x, 2(F9.4,1x),1PE15.4,1x,5(1X,I5),               0603YC96
     *       /,(45X,5(1X,I5)))
       END SUBROUTINE irph
C*******************************************************************
C IRPHZ
C*******************************************************************
C
      SUBROUTINE irphz(redm,wi,freqr,freqp,frets,zrptr,
     >    lorder,nfreq,npts,nshf,iunit6,gufac6)
      use perconparam; use gtst
c
c     Interpolates s, Vmep, Vag for Zero-order IVTST
c
c     CALLED BY:
c               GIVTST
c     CALLS:
c            ECKARS, VIVT, sortg
c
      implicit double precision (a-h,o-z)
      dimension freqp(2*nfre),freqr(2*nfre),frets(nfre)
      dimension nfreq(10)
c
      nptl = int(smin/dels)
      smin = nptl*dels
      nshf = -nptl + 1
      nptr = int(smax/dels)
      smax = nptr*dels
      npts = nptr-nptl + 1
c
c  Interpolate s
c
      do 10 i = 1,npts
        sx(i) = smin + (i-1)*dels
10    continue
c
      call eckars(1,redm,wi,lorder)
      do 30 i = 1,npts
           vs(i) = vivt(sx(i))
  30  continue
      write(fu6,900) lorder,smin/gufac6,smax/gufac6,dels/gufac6         0405JZ07
      call eckars(2,redm,wi,lorder)
C     write (fu6,799)
      if(iunit6.eq.1) write (fu6,799)                                   0405JZ07
      if(iunit6.eq.0) write (fu6,798)                                   0405JZ07
      do 35 i = 1,npts
         vad(i) = vivt(sx(i)) + zrptr
         write(fu6,800) sx(i)/gufac6,vs(i)*ckcal,vad(i)*ckcal            0405JZ07
  35  continue
c
c      Order frequencies
c
      nfrer = nfreq(1) + nfreq(2)
      nfrep = nfreq(3) + nfreq(4)
      do 40 i = 1,nfrer
          if (i .le. nfreq(1)) then
            freqr(i) = freq(1,i)
          else
             k = i - nfreq(1)
             freqr(i) = freq(2,k)
          endif
40    continue
      call sortg(nfrer,freqr)
      do 50 i = 1,nfrep
         if (i .le. nfreq(3)) then
            freqp(i) = freq(3,i)
         else
            k = i - nfreq(3)
            freqp(i) = freq(4,k)
         endif
50    continue
      call sortg(nfrep,freqp)
      nfgt = nfreq(5)
      do 60 i = 1,nfgt
         frets(i) = freq(5,i)
60    continue
      call sortg(nfgt,frets)
C
C Print out order of frequencies
C
      write(fu6,*)' Frequencies of Reactant'
      write(fu6,300) (freqr(i)*autocm,i=1,nfrer)
      write(fu6,*)' Frequencies of transition state'
      write(fu6,300) (frets(i)*autocm,i=1,nfgt)
      write(fu6,*)' Frequencies of product'
      write(fu6,300) (freqp(i)*autocm,i=1,nfrep)
300   format(1x,11f7.1)
799   format(/,2x,'s(bohr)',t16,'VMEP(kcal/mol)',t34,
     * 'Va^G(kcal/mol)',/)  
798   format(/,2x,'s(angstrom)',t16,'VMEP(kcal/mol)',t34,
     * 'Va^G(kcal/mol)',/)                                              0405JZ07
800   FORMAT(1X,F8.4,t19,F8.4,t34,F8.4)
900   FORMAT(//,1X,15(1H*),3X,I1,'-ORDER INTERPOLATED RESULTS   ',
     * 15(1H*),//,10X,F6.3,' <  S  < ',F6.3,'    STEP SIZE = ',F10.5,
     * /)
      return
      end SUBROUTINE irphz
C********************************************************************
C KAPA
C********************************************************************
      SUBROUTINE kapa(wi,redm,ntemp,ncoord,lorder,npts,nfreq,
     * iunit6,gufac6)
      use perconparam
      use gtst, only : iprint,iscsag
      use gtst1
c
c     Calculates the transimission coefiecients by Wigner, untruncated
c     parabola, and Skodje and Truhlar methods.
c      Ref. for Truhlar paper J. Phys. Chem. 85,624(1981)
c
c     CALLED BY:
c                GIVTST
c     CALLS:
c                ECKARS, KAPPAS
c
      implicit double precision (a-h,o-z)
      DIMENSION nfreq(10)
      save                                                              0601YC98
c     Calculate transmission coefficients by using Eckart-Parabolic
c     potential.
c
      CALL ECKARS(2,redm,wi,lorder)
      CALL KAPPAS(redm,ntemp,ncoord,lorder,npts,nfreq,iunit6,gufac6)    0405JZ07
C
C     Calculate the Wigner correction
C
      DO 10 I = 1,NTEMP
        BETA = 1.0D0/(BK*T(I))
        WIG(I) = 1.D0 + (BETA*WI)**2/24.0d0
10    CONTINUE
c
700   WRITE(fu6,1000)
      IF (LORDER .EQ. 0) THEN
         WRITE(fu6,1500)
      ELSE
         if (iscsag.eq.0) then
           WRITE(fu6,2000)
         else
           write(fu6,2100)
         endif
      ENDIF
      DO 600 I = 1,NTEMP
         IF (LORDER .EQ. 0) THEN
           WRITE(fu6,2500) T(I),WIG(I),TCAG(I),TCSAG(I)
         ELSE
           WRITE(fu6,3000) T(I),WIG(I),CAG(I),TCSAG(I),TCSCG(I)
         ENDIF
600   CONTINUE
c
1000  FORMAT(//,15X,'FINAL TRANSMISSION COEFFICIENTS',/)
C 1500  FORMAT(2X,'T(K)',9X,'WIGNER',7X,'TST/CAG',8X,'MEPSAG'/)
C 2000  FORMAT(2X,'T(K)',9X,'WIGNER',7X,'CVT/CAG',8X,'MEPSAG',
C     *  7X,'CD-SCSAG',/)
C 2100  FORMAT(2X,'T(K)',9X,'WIGNER',7X,'CVT/CAG',8X,'MEPSAG',
C     *  7X,'SCSAG',/)
1500  FORMAT(2X,'T(K)',9X,'WIGNER',7X,'TST/CAG',8X,'ZCT'/)              0423TA02
2000  FORMAT(2X,'T(K)',9X,'WIGNER',7X,'CVT/CAG',8X,'ZCT',               0423TA02
     *  11X,'SCT',/)                                                    0423TA02
2100  FORMAT(2X,'T(K)',9X,'WIGNER',7X,'CVT/CAG',8X,'ZCT',               0423TA02
     *  10X,'SCSAG',/)                                                  0423TA02
2500  FORMAT(1X,0PF6.1,5X,1PE10.4,2(4X,E10.4))                          06/96ELC
3000  FORMAT(1X,0PF6.1,5X,1PE10.4,3(4X,E10.4))                          06/96ELC
3500  FORMAT(1X,0PF6.3,2X,F6.3)                                         06/96ELC
      RETURN
      END SUBROUTINE kapa
C
C*********************************************************************
C KAPPAS
C*********************************************************************
C  Calculate the transmission coefficients using uniform semiclassical
C  WKB method (MEPSAG or ZCT) and small curvature method (SCSAG)
C
      SUBROUTINE kappas(redm,ntemp,ncoord,lorder,npts,nfreq,
     * iunit6,gufac6)
      use perconparam; use gtst
      use gtst1, only : nqk,nqk2,nqt,nqt2,t,tcag,cag,
     *wig,tcsag,tcscg
      
C
      implicit double precision (a-h,o-z)      
      real(8) :: PTK(81),WTK(81,2)
      DIMENSION SUMZ(4),SUMS(4),TE(2,81),TES(2,81),TEZ(2,81)
c     DIMENSION STP(6),ICHAR(6)
      DIMENSION STP(6)
      character*2 ichar(6)
      dimension nfreq(10)
      save                                                              0601YC98

C     EPS was 1D-5 here
C
C  Set up N and 2N + 1 quadratures for de and ds integrals.
C
      CALL KG1(NQK,PTK,WTK)
      CALL KG1(NQT,PTT,WTT)
      NQK2 = 2*NQK + 1
      NQT2 = 2*NQT + 1
c
c  start calculating the tunneling probabilities at the var or vap
c
      ETHR = MAX(VAR,VAP)                                               0612YC96
C
C  Generate spline fit for Va
C
      NSMAX = NPTS
      CALL VSPLI2 (NSMAX,SX,SMAX,VAD,VMAX,IMAX)
c
      VB = VMAX - ETHR
      LNEG =.FALSE.
C
C  Find the Eckart parameters for redmu in the scsag calculation
C
      if(lorder .gt. 0) then
c        Calculates PAGE-McIVER curvature factors BK,F and KAPPA
         CALL bcalcp(redm,ncoord,lorder,nfreq)
      endif
C
      WRITE(fu6,500)
C     WRITE(fu6,600) NQT,NQT2
      IF(IUNIT6.EQ.1) WRITE(fu6,600) NQT,NQT2                           0405JZ07
      IF(IUNIT6.EQ.0) WRITE(FU6,610) NQT,NQT2                           0405JZ07
      DO 30 N = 1,NQK2
          E = 0.5D0*(1.0D0 + PTK(N))*VB + ETHR
          CALL TSC (redm,E,TE(1,N),NTP,STP,SX,ICHAR,IMAX,VMAX,0)
          DO 10 J = 1,2
            TEZ(J,N) = TE(J,N)
10        CONTINUE
        IF (LORDER .GT. 0) THEN
          CALL TSC (redm,E,TE(1,N),NTP,STP,SX,ICHAR,IMAX,VMAX,1)
          DO 20 J = 1,2
            TES(J,N) = TE(J,N)
20        CONTINUE
        END IF
          NM = MIN0(NTP,6)
        if(lorder.gt.0) then
          write(fu6,601) e*ckcal,(tez(j,n),j=1,2),(tes(j,n),j=1,2),
     *   ntp,(stp(j)/gufac6,ichar(j),j=1,nm)                            0405JZ07
        else
          write(fu6,599) e*ckcal,(tez(j,n),j=1,2),
     *    ntp,(stp(j)/gufac6,ichar(j),j=1,nm)                           0405JZ07
        endif
30    CONTINUE
      WRITE(fu6,602) NQT,NQT2,NQT,NQT2,NQK,NQK,NQK2,NQK2
c 
      DO 100 IT = 1,NTEMP
          BET = 1.0D0/(BK*T(IT))                                        0612YC96
          XN = 0.5D0*BET*VB
          DO 40 J = 1,4
             SUMZ(J) = 0.0D0
40           SUMS(J) = 0.0D0
          DO 60 N = 1,NQK2
             TEMP = 2.D0*SINH(XN*(1.0D0-PTK(N)))
             DO 50 J = 1,2
                J0 = 2*(J-1)
                W = WTK(N,J)*TEMP
             DO 50 K = 1,2
                II = K + J0
                SUMZ(II) = SUMZ(II) + W*TEZ(K,N)
               IF (LORDER .GT. 0) THEN
                SUMS(II) = SUMS(II) + W*TES(K,N)
               END IF
50           CONTINUE
60        CONTINUE
          DO 70 II = 1,4
             SUMZ(II) = 1.0D0 + XN*SUMZ(II)
            IF (LORDER .GT. 0) THEN
             SUMS(II) = 1.0D0 + XN*SUMS(II)
            END IF
70        CONTINUE
          WRITE(fu6,603) T(IT),(SUMZ(LL),LL=1,4)
            IF (LORDER .GT. 0) THEN
               if (iscsag.eq.0) then
                 WRITE(fu6,604) T(IT),(SUMS(LL),LL=1,4)
               else
                 WRITE(fu6,605) T(IT),(SUMS(LL),LL=1,4)
               endif
            END IF
          TCSAG(IT) = SUMZ(4)
         IF (LORDER .GT. 0) THEN
          TCSCG(IT) = SUMS(4)
         END IF
100   CONTINUE
      RETURN
C
599    format(F12.5,1x,2e12.5,i4,3x,6(f6.3,a2))
500    FORMAT(//3X,'NOTE: ** after a turning point implies that',
     *  /3X,'      E is less than V at that s value.')
600    FORMAT(/10X,
     1 55HN = Number of quadrature points used in evaluating T(E)/10X,
     2 57HM = Number of quadrature points used in Boltzmann average//,
     3 T27,'TRANSMISSION PROBABILITIES',12X,'TURNING POINTS (bohrs)',   06/96ELC
     4 /T3,11HE(kcal/mol),                                              06/96ELC
     5 6x,2HN=,I3,6x,2HM=,I3,T63,'No.',T67,'Left',T75,'Right')          06/96ELC
610     FORMAT(/10X,
     1 55HN = Number of quadrature points used in evaluating T(E)/10X,
     2 57HM = Number of quadrature points used in Boltzmann average//,
     3 T27,'TRANSMISSION PROBABILITIES',12X,'TURNING POINTS (angstroms)'0405JZ07
     4 ,/T3,11HE(kcal/mol),                                             
     5 6x,2HN=,I3,6x,2HM=,I3,T63,'No.',T67,'Left',T75,'Right')          
601   FORMAT(F12.5,1X,4E12.5,I3,1X,6(0PF5.2,A2))
602   FORMAT(//35X,26H TRANSMISSION COEFFICIENTS,//T18,
     1 4(8X,2HN=,I3) /,2X,4HT(K),T18,4(8X,2HM=,I3)/)
C 603   FORMAT(1X,F6.1,3X,'MEPSAG',3X,4(1PE13.4))
C 604   FORMAT(1X,F6.1,3X,'CD-SCSAG',1X,4(1PE13.4))
603   FORMAT(1X,F6.1,4X,'ZCT  ',3X,4(1PE13.4))                          0423TA02
604   FORMAT(1X,F6.1,4X,'SCT  ',3X,4(1PE13.4))                          0423TA02
605   FORMAT(1X,F6.1,4X,'SCSAG',3X,4(1PE13.4))
      END SUBROUTINE kappas
C
C***********************************************************************
C  MUSC 
C***********************************************************************
      SUBROUTINE musc(bkf,redm,lorder,nfreq)
      use perconparam; use gtst; use gtst1, only : redmue
C
C Computes mueff for SCSAG (Save for comparision)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension tps(10,maxcar),dertp(10)
      dimension nfreq(10)
      dimension bkf(nfre,10)
c
c  Compute turning points
c  Loop over vibrational modes
c                                                                 
      DO 30 K = 5,5+LORDER
         DO 20 I = 1,NFREQ(K)
            TPS(K,I) = -SQRT(1.0D0/(REDM*FREQ(K,I)))
   20    CONTINUE
   30 CONTINUE
c
      PROD = 1.0D0
      DO 40 I = 1, NFREQ(5)
C
C  Compute derivative of turning point with finite differencing
C
         DERTP(5) = (TPS(6,I) - TPS(5,I))/ABS(DS)
         W2 = DERTP(5)*DERTP(5)
         TPS(5,I) = TPS(5,I)*DSIGN(1.0D0,BKF(I,5))
c 
c  change to absolute value of ARG, according to POLYRATE4.0 paper
c
         ARG = ABS(TPS(5,I)*BKF(I,5))                                   0606YC96
         ARG2 = ARG*ARG
C
C  SC factor
C
         XARG = -2.0D0*ARG-ARG2+W2                                      0606YC96
         IF (XARG.LT.0.0D0) PROD = PROD*EXP(XARG)
C
C  SC effective mass
C
   40 CONTINUE
      REDMUE(5) = REDM*PROD
      WRITE(FU6,*) 'SC effective mass at TS = ', redmue(5)
C
      RETURN
C
      END SUBROUTINE musc
C
C***********************************************************************
C  MUSCCD 
C***********************************************************************
      SUBROUTINE musccd(dkap,bkf,redm,lorder,nfreq)
      use perconparam; use gtst
      use gtst1, only : redmue
      implicit real(8) (a-h,o-z)
C
C Computes mueff for CD-SCSAG
c
      dimension tps(10),dertp(10)
      dimension nfreq(10)
      dimension bkf(nfre,10),dkap(10)
c
c  Compute turning points
c  Loop over vibrational modes
c       
      DO 30 K = 5,5+LORDER
         TPS(K) = 0.0D0
         DO 20 I = 1,NFREQ(K)
           IF(FREQ(K,I).GT.0.0) THEN
              TPS(K) = TPS(K) + (BKF(I,K)*FREQ(K,I))**2
           ENDIF
   20    CONTINUE
         TPS(K) = SQRT(DKAP(K)/(REDM*SQRT(TPS(K))))                     0819YC96
   30 CONTINUE
c
c     Compute derivative of turning point with finite differencing
c
      DERTP(5) = (TPS(6) - TPS(5))/ABS(DS)
      W2 = DERTP(5)*DERTP(5)
      PROD = 1.0D0
      ARG =  TPS(5)*DKAP(5)
      ARG2 = ARG*ARG
C
C  SC factor
C
      XARG =-2.0D0*ARG-ARG2+W2
      IF (XARG.LT.0.0D0) PROD = PROD*EXP(XARG)
C
C  SC effective mass
C
      REDMUE(5) = REDM*PROD
      if(iprint.eq.1) then
        write(fu6,*) 'SC effective mass at TS = ', redmue(5)
      endif
      RETURN
      END SUBROUTINE musccd
C
C***********************************************************************
C NEARW
C***********************************************************************
C
      SUBROUTINE nearw(JW,W,WB,DF,IOP,LSTEP)
      use perconparam
c
      implicit double precision (a-h,o-z)
      DIMENSION W(*),WB(*)
C
      IF (LSTEP .EQ. 0)THEN
        IF (IOP .EQ. 0) W(JW) = WB(JW)
        ASIGN = SIGN(1.0D0,DF)
      END IF
C      IF (JW .NE. 9) W(JW) = W(JW) + ASIGN*1.0D0*CMTOAU
C      IF(JW .EQ. 9) W(JW) = W(JW) + ASIGN*315.0D0*CMTOAU
      W(JW) = W(JW) + ASIGN*1.0D0*CMTOAU                                1012BE06
      RETURN
      END SUBROUTINE nearw
c***********************************************************************
c  numfrq
c***********************************************************************
c
      SUBROUTINE numfrq(k,icode,nfreq,na)
c
c     Calculates the number of internal vibrations 
c
c     CALLED BY:
c                GIVTST
c
      implicit double precision (a-h,o-z)
      dimension  nfreq(10)
      if(k.le.4) then
          if(icode.eq.1) then
             nfreq(k) = 0
          elseif(icode.eq.2) then
             nfreq(k) = 3*na - 5
          else
             nfreq(k) = 3*na -6
          endif
      else
          if(icode.eq. 2) then
             nfreq(k) = 3*na - 6
           else
             nfreq(k) = 3*na - 7
          endif
      endif
      return
      end SUBROUTINE numfrq
C***********************************************************************
C  PROJTI
C***********************************************************************
C
      SUBROUTINE projti(kop,iop,na,amasrk,rot)
      use perconparam
      use gtst
      use gtst1, only : p,xp,amassp 
C
C     CALCULATES PROJECTED FORCE CONSTANT MATRIX AT MONSTATIONARY POINTS
C
C     CALLED BY:
C               FDIAG2
C     CALLS:
C               MXLNEQN
C
      implicit double precision (a-h,o-z)
C
      DIMENSION TENS(3,3,3),rot(3,3)
      dimension na(10)
C
      DATA TENS / 5*0.D0,-1.D0,0.D0,1.D0,3*0.D0,1.D0,3*0.D0,-1.D0,
     *    3*0.D0,-1.D0,0.D0,1.D0,5*0.D0 /
C
C
      NEND =3*NA(KOP)
C
C     COMPUTE P MATRIX
C
      DO 100 IP = 1, NA(KOP)
         INDX = 3*(IP-1)
         DO 100 JP = 1, IP
            JNDX = 3*(JP-1)
            DO 90 IC = 1, 3
               JEND = 3
               IF (JP.EQ.IP) JEND = IC
               DO 90 JC = 1, JEND
                  SUM = 0.0D0
C
C
C  FOR GAS PHASE
C
C      NON-LINEAR CASE
C
                  IF (IOP.EQ.3) THEN
                     DO 80 IA = 1, 3
                        DO 80 IB = 1, 3
                           IF (TENS(IA,IB,IC)) 50, 80, 50
   50                      DO 70 JA = 1, 3
                              DO 70 JB = 1, 3
                                 IF (TENS(JA,JB,JC)) 60, 70, 60
   60                            SUM = SUM+TENS(IA,IB,IC)*TENS(JA,JB,JC)
     *                              *ROT(IA,JA)*XP(INDX+IB)*XP(JNDX+JB)
   70                      CONTINUE
   80                CONTINUE
                  ENDIF
                  II = INDX+IC
                  JJ = JNDX+JC
                  IF (IOP.EQ.3) THEN
                     P(II,JJ) = SUM+DX(KOP,II)*DX(KOP,JJ)
C
C  LINEAR CASE
C
                  ELSEIF (IOP.EQ.2) THEN
                     IF (IC.EQ.3.AND.JC.EQ.3) THEN
                        P(II,JJ) = DX(KOP,II)*DX(KOP,JJ)
                     ELSEIF (IC.NE.3.AND.IC.EQ.JC) THEN
                        P(II,JJ) = XP(INDX+3)*XP(JNDX+3)/FMOM(KOP)
                     ELSEIF (IC.NE.JC) THEN
                        P(II,JJ) = 0.0D0
                     ENDIF
                  ENDIF
                  IF (IC.EQ.JC) P(II,JJ) = P(II,JJ)+SQRT(AMASSP(II)
     *                                       *AMASSP(JJ))/AMASRK
   90       CONTINUE
  100 CONTINUE
      DO 110 I = 1, NEND
         DO 110 J = 1, I
            P(I,J) = -P(I,J)
            IF (I.EQ.J) P(I,J) = 1.0D0+P(I,J)
  110 CONTINUE
      DO 120 I = 1, NEND
         DO 120 J = 1, I
            P(J,I) = P(I,J)
  120 CONTINUE
C
C     POST AND PREMULTIPLY F BY P.  USE COF FOR SCRATCH
C
      DO 140 I = 1, NEND
         DO 140 J = 1, NEND
            SUM = 0.0D0
            DO 130 K = 1, NEND
               SUM = SUM+FORC(I,K)*P(K,J)
  130       CONTINUE
            COF(I,J) = SUM
  140 CONTINUE
      DO 160 I = 1, NEND
         DO 160 J = 1, NEND
            SUM = 0.0D0
            DO 150 K = 1, NEND
               SUM = SUM+P(I,K)*COF(K,J)
  150       CONTINUE
            FORC(I,J) = SUM
  160 CONTINUE
      RETURN
      END subroutine projti
C
C***********************************************************************
C  PSATX
C***********************************************************************
C
      SUBROUTINE psatx(IFLG,E,SL,SR,SN,XT,ICHRL,ICHRR,IMAX,VMAX)
      use perconparam
      use gtst, only : fmom,freq,sx,vs,vad,gfreq,gfmom,delg
C
C     PSATX     - find turning points in adiabatic barrier
C                 added 10/13/1986  by bcg.
C
C  Called by:
C    TSC
C
C   Calls:
C     PSATX2
C
      implicit double precision (a-h,o-z)
      logical lrt
      character*2 ichrl,ichrr,istar,iblnk
C*
      DATA ISTAR / '**'/,IBLNK / '  '/
C
      save                                                               0601YC98

      ICHRL = IBLNK
      ICHRR = IBLNK
C
      SGN = 1.0D0
      ITP = 1
c
c     IFLG = 0, start from left most point
c
      IF (IFLG.EQ.0) THEN
         IS0 = 1
         LRT = .FALSE.
C
C        check if the first point is higher than the energy, if so use the
C        first point as the turning point
C
         IF (E.LT.VAD(1)) THEN
            SGN = -1.0D0
C
            ICHRL = ISTAR
            SL = SX(1)
            ITP = 2
            IFLG = 1
         ENDIF
      ENDIF
c
      IF (ITP.EQ.1) THEN
         CALL PSATX2 (IFLG,E,S,IS0,LRT,SGN,ITP,ICHRR,IMAX,VMAX)
         SL = S
         SGN = -1.0D0
      ENDIF
c
c     IFLG .NE. 0, start from last point
c 
      IF (IFLG.NE.0) THEN
         CALL PSATX2 (IFLG,E,S,IS0,LRT,SGN,ITP,ICHRR,IMAX,VMAX)
         SR = S
         SN = (SL+SR)*0.5D0
         XT = (SR-SL)*0.5D0
      ENDIF
      RETURN
      END SUBROUTINE psatx
C
C***********************************************************************
C  PSATX2
C***********************************************************************
C
      SUBROUTINE psatx2(IFLG,E,S,IS0,LRT,SGN,ITP,ICHRR,IMAX,VMAX)
      use perconparam
      use gtst ; use gtst1
C
C     PSATP2    - find turning points in adiabatic barrier
C                 added 10/13/1986  by bcg.
C
c     input:    E
c               IS0   index of turning point
c               LRT  .F.  use new grid
c               SGN  -1 : for SL, 1 : for SR
c               ITP   1 : search for SL, 2 : search for SR
c               IMAX  index for VMAX
c               
c     output:   
c               S    = turning point
c               IFLG = 1,  successfully found tps
c               IFLG = 0,  no (more) tps found
c
C  Called by:
C     PSATX
C
C  Calls:
C     CUBIC2
C
      implicit double precision (a-h,o-z)
      logical lrt,lset
      character*2 ichrr,istar
      Save NRC, NRT, SRT

C*
      
      DIMENSION RRT(3),AIRT(3),SRT(4)
      DATA ISTAR / '**'/
C
c
      IFLG = 1
      IF (LRT) THEN
C
C  a root from the cubic polynomial left over, use it before searching
C     through the grid again
C
         NRC = NRC+1
         S = SRT(NRC)
c
c       write(fu6,6601) sgn,nrc,nrt,srt(nrc)
c6601  format('lrt=.t.,sgn,nrc,nrt,srt=',f3.1,2i3,4f15.7)
c
         IF (NRC.EQ.NRT) LRT = .FALSE.
      ELSE
         LSET = .FALSE.
C
C  search grid until E-V changes sign or end of grid is hit
C
C
c         write(fu6,6602) is0,sgn
c6602     format('lrt=.f.,is0=',i5,',sgn=',f5.1)	
         IS = IS0
         IF (E.GT.VMAX) THEN
            IF (SGN.GT.0.0D0) THEN
               IF (IS.LT.NS) THEN
                  IS = IMAX+1
                  IS0 = NS
                  LSET = .TRUE.
               ENDIF
            ELSE
               IS = IMAX+2
               IS0 = NS
               LSET = .TRUE.
            ENDIF
         ELSE
   10       CONTINUE
            IF (LSET.OR.IS.GE.NS) GO TO 20
            IS = IS+1
            LSET = (E-VAD(IS))*SGN.LE.0.
            GO TO 10
   20       CONTINUE
            IS0 = IS
         ENDIF
         IF (LSET) THEN
C
C  E-V changed sign between grid points IS-1 and IS
C
            IS = IS-1
C
c            write(fu6,6603)is,e,sx(is),sx(is+1),vad(is),vad(is+1)
c6603        format('is=',i5,',e=',1pe15.7,',sx(is), sx(is+1)=',
c     *      0p2f15.7,',vad(is),vad(is+1)=',1p2e15.7)
C
C  S1 and S2 are left and right bounds on the turning point
C
            S1 = SX(IS)
            S2 = SX(IS+1)
            DD = DV(IS)-E
C
c           write(fu6,6610) av(is),bv(is),cv(is),dv(is),dd
c6610        format('coefficients'/,1x,1p5e15.7)
C
C  V is expressed as a cubic spline, solve for zeros of cubic polynomial
C
            CALL CUBIC2 (AV(IS),BV(IS),CV(IS),DD,NREAL,RRT,AIRT)
            NRT = 0
            IF (NREAL.GE.1) THEN
C
C
c            write(fu6,6604) nreal,rrt(1),rrt(2),rrt(3)
c6604        format(1x,i3,'real roots',2x,1p3e15.7)
C
C  readl roots found, check if they are between S1 and S2
C
               NRT = 0
               DO 30 I = 1, NREAL
                  IF (RRT(I).LT.S1.OR.RRT(I).GT.S2) GO TO 30
                  NRT = NRT+1
                  SRT(NRT) = RRT(I)
c               write(fu6,6605) nrt,srt(nrt)
c6605           format(1x,i3,'roots in range',2x,1p4e15.7)
   30          CONTINUE
            ENDIF
            IF (NRT.GT.0) THEN
C
C  valid real roots found, use lowest one
C
               NRC = 1
               S = SRT(1)
               IF (NRT.GT.1) LRT = .TRUE.
            ELSE
C
C  no valid real root found, use the bound
C
               IF (ITP.EQ.1) S = S1
               IF (ITP.EQ.2) S = S2
C
c            write(fu6,6606) s,s1,s2
c6606         format('no real roots,s,s1,s2=',3f15.7)
            ENDIF
         ELSE
C
C  ran into end of grid
C
            IF (SGN.LT.0.D0) THEN
C
C  looking for right turning point but couldn't find it
C
               ICHRR = ISTAR
C
c              write(fu6,6001) sx(ns),e*ckcal,vad(ns)*ckcal
c6001           format(10x,'tp-at right most point s=',f10.5,' ,
c    *         e=',1pe13.5,',is less than v=',e13.5)
               S = SX(NS)
            ELSE
C
C  looking for left turning point but couldn't find it, return with
C     IFLG=0 (signals end of tps)
C
               IFLG = 0
            ENDIF
         ENDIF
      ENDIF
      RETURN
C
      END SUBROUTINE psatx2
c*******************************************************************
c     qform
c*******************************************************************
      SUBROUTINE qform(m,n,q,ldq,wa)
      integer m,n,ldq
      double precision q(ldq,m),wa(m)
c
c
c     this subroutine proceeds from the computed qr factorization of
c     an m by n matrix a to accumulate the m by m orthogonal matrix
c     q from its factored form.
c
c       m is a positive integer input variable set to the number
c         of rows of a and the order of q.
c
c       n is a positive integer input variable set to the number
c         of columns of a.
c
c       q is an m by m array. on input the full lower trapezoid in
c         the first min(m,n) columns of q contains the factored form.
c         on output q has been accumulated into a square matrix.
c
c       ldq is a positive integer input variable not less than m
c         which specifies the leading dimension of the array q.
c
c       wa is a work array of length m.
c
c     subprograms called
c
c       fortran-supplied ... min0
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c*******************************************************************
      integer i,j,jm1,k,l,minmn,np1
      double precision one,sum,temp,zero
      data one,zero /1.0d0,0.0d0/
c
c     zero out upper triangle of q in the first min(m,n) columns.

      minmn = min0(m,n)
      if (minmn .lt. 2) go to 30
      do 20 j = 2, minmn
         jm1 = j - 1
         do 10 i = 1, jm1
            q(i,j) = zero
   10       continue
   20    continue
   30 continue

c     initialize remaining columns to those of the identity matrix.

      np1 = n + 1
      if (m .lt. np1) go to 60
      do 50 j = np1, m
         do 40 i = 1, m
            q(i,j) = zero
   40       continue
         q(j,j) = one
   50    continue
   60 continue

c     accumulate q from its factored form.

      do 120 l = 1, minmn
         k = minmn - l + 1
         do 70 i = k, m
            wa(i) = q(i,k)
            q(i,k) = zero
   70       continue
         q(k,k) = one
         if (wa(k) .eq. zero) go to 110
         do 100 j = k, m
            sum = zero
            do 80 i = k, m
               sum = sum + q(i,j)*wa(i)
   80          continue
            temp = sum/wa(k)
            do 90 i = k, m
               q(i,j) = q(i,j) - temp*wa(i)
   90          continue
  100       continue
  110    continue
  120    continue
      return
      end SUBROUTINE qform
c
c*******************************************************************
      SUBROUTINE qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
C
      integer m,n,lda,lipvt
      integer ipvt(lipvt)
      logical pivot
      double precision a(lda,n),rdiag(n),acnorm(n),wa(n)
c
c
c     this subroutine uses householder transformations with column
c     pivoting (optional) to compute a qr factorization of the
c     m by n matrix a. that is, qrfac determines an orthogonal
c     matrix q, a permutation matrix p, and an upper trapezoidal
c     matrix r with diagonal elements of nonincreasing magnitude,
c     such that a*p = q*r. the householder transformation for
c     column k, k = 1,2,...,min(m,n), is of the form
c
c                           t
c           i - (1/u(k))*u*u
c
c     where u has zeros in the first k-1 positions. the form of
c     this transformation and the method of pivoting first
c     appeared in the corresponding linpack subroutine.
c
c
c       m is a positive integer input variable set to the number
c         of rows of a.
c
c       n is a positive integer input variable set to the number
c         of columns of a.
c
c       a is an m by n array. on input a contains the matrix for
c         which the qr factorization is to be computed. on output
c         the strict upper trapezoidal part of a contains the strict
c         upper trapezoidal part of r, and the lower trapezoidal
c         part of a contains a factored form of q (the non-trivial
c         elements of the u vectors described above).
c
c       lda is a positive integer input variable not less than m
c         which specifies the leading dimension of the array a.
c
c       pivot is a logical input variable. if pivot is set true,
c         then column pivoting is enforced. if pivot is set false,
c         then no column pivoting is done.
c
c       ipvt is an integer output array of length lipvt. ipvt
c         defines the permutation matrix p such that a*p = q*r.
c         column j of p is column ipvt(j) of the identity matrix.
c         if pivot is false, ipvt is not referenced.
c
c       lipvt is a positive integer input variable. if pivot is false,
c         then lipvt may be as small as 1. if pivot is true, then
c         lipvt must be at least n.
c
c       rdiag is an output array of length n which contains the
c         diagonal elements of r.
c
c       acnorm is an output array of length n which contains the
c         norms of the corresponding columns of the input matrix a.
c         if this information is not needed, then acnorm can coincide
c         with rdiag.
c
c       wa is a work array of length n. if pivot is false, then wa
c         can coincide with rdiag.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm
c
c       fortran-supplied ... dmax1,sqrt,min0
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c*******************************************************************
      integer i,j,jp1,k,kmax,minmn
      double precision ajnorm,epsmch,one,p05,sum,temp,zero
      double precision dpmpar,enorm
      data one,p05,zero /1.0d0,5.0d-2,0.0d0/

c     epsmch is the machine precision.

      epsmch = dpmpar(1)

c     compute the initial column norms and initialize several arrays.

      do 10 j = 1, n
         acnorm(j) = enorm(m,a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
   10    continue

c     reduce a to r with householder transformations.

      minmn = min0(m,n)
      do 110 j = 1, minmn
         if (.not.pivot) go to 40

c        bring the column of largest norm into the pivot position.

         kmax = j
         do 20 k = j, n
            if (rdiag(k) .gt. rdiag(kmax)) kmax = k
   20       continue
         if (kmax .eq. j) go to 40
         do 30 i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
   30       continue
         rdiag(kmax) = rdiag(j)
         wa(kmax) = wa(j)
         k = ipvt(j)
         ipvt(j) = ipvt(kmax)
         ipvt(kmax) = k
   40    continue

c        compute the householder transformation to reduce the
c        j-th column of a to a multiple of the j-th unit vector.

         ajnorm = enorm(m-j+1,a(j,j))
         if (ajnorm .eq. zero) go to 100
         if (a(j,j) .lt. zero) ajnorm = -ajnorm
         do 50 i = j, m
            a(i,j) = a(i,j)/ajnorm
   50       continue
         a(j,j) = a(j,j) + one

c        apply the transformation to the remaining columns
c        and update the norms.

         jp1 = j + 1
         if (n .lt. jp1) go to 100
         do 90 k = jp1, n
            sum = zero
            do 60 i = j, m
               sum = sum + a(i,j)*a(i,k)
   60          continue
            temp = sum/a(j,j)
            do 70 i = j, m
               a(i,k) = a(i,k) - temp*a(i,j)
   70          continue
            if (.not.pivot .or. rdiag(k) .eq. zero) go to 80
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k)*sqrt(dmax1(zero,one-temp**2))
            if (p05*(rdiag(k)/wa(k))**2 .gt. epsmch) go to 80
            rdiag(k) = enorm(m-j,a(jp1,k))
            wa(k) = rdiag(k)
   80       continue
   90       continue
  100    continue
         rdiag(j) = -ajnorm
  110    continue
      return
      end  SUBROUTINE qrfac
C***********************************************************************
C QUADR
C***********************************************************************
C
      SUBROUTINE quadr(f,x,a,b,c)
C
      implicit double precision (a-h,o-z)
      dimension f(2),x(2)
C
      delta = x(1)**2*x(2) - x(1)*x(2)**2
      a = (x(2)*(f(1)-c) - x(1)*(f(2)-c))/delta
      b = (x(1)**2*(f(2)-c) - x(2)**2*(f(1)-c))/delta
      return
      end SUBROUTINE quadr
c*******************************************************************
c     subroutine r1mpyq
c*******************************************************************
      SUBROUTINE r1mpyq(m,n,a,lda,v,w)
C
      integer m,n,lda
      double precision a(lda,n),v(n),w(n)
c
c
c     given an m by n matrix a, this subroutine computes a*q where
c     q is the product of 2*(n - 1) transformations
c
c           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
c
c     and gv(i), gw(i) are givens rotations in the (i,n) plane which
c     eliminate elements in the i-th and n-th planes, respectively.
c     q itself is not given, rather the information to recover the
c     gv, gw rotations is supplied.
c
c     the subroutine statement is
c
c
c       m is a positive integer input variable set to the number
c         of rows of a.
c
c       n is a positive integer input variable set to the number
c         of columns of a.
c
c       a is an m by n array. on input a must contain the matrix
c         to be postmultiplied by the orthogonal matrix q
c         described above. on output a*q has replaced a.
c
c       lda is a positive integer input variable not less than m
c         which specifies the leading dimension of the array a.
c
c       v is an input array of length n. v(i) must contain the
c         information necessary to recover the givens rotation gv(i)
c         described above.
c
c       w is an input array of length n. w(i) must contain the
c         information necessary to recover the givens rotation gw(i)
c         described above.
c
c     subroutines called
c
c       fortran-supplied ... dabs,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c*******************************************************************
      integer i,j,nmj,nm1
      double precision cos,one,sin,temp
      data one /1.0d0/

c     apply the first set of givens rotations to a.

      nm1 = n - 1
      if (nm1 .lt. 1) go to 50
      do 20 nmj = 1, nm1
         j = n - nmj
         if (dabs(v(j)) .gt. one) cos = one/v(j)
         if (dabs(v(j)) .gt. one) sin = sqrt(one-cos**2)
         if (dabs(v(j)) .le. one) sin = v(j)
         if (dabs(v(j)) .le. one) cos = sqrt(one-sin**2)
         do 10 i = 1, m
            temp = cos*a(i,j) - sin*a(i,n)
            a(i,n) = sin*a(i,j) + cos*a(i,n)
            a(i,j) = temp
   10       continue
   20    continue

c     apply the second set of givens rotations to a.

      do 40 j = 1, nm1
         if (dabs(w(j)) .gt. one) cos = one/w(j)
         if (dabs(w(j)) .gt. one) sin = sqrt(one-cos**2)
         if (dabs(w(j)) .le. one) sin = w(j)
         if (dabs(w(j)) .le. one) cos = sqrt(one-sin**2)
         do 30 i = 1, m
            temp = cos*a(i,j) + sin*a(i,n)
            a(i,n) = -sin*a(i,j) + cos*a(i,n)
            a(i,j) = temp
   30       continue
   40    continue
   50 continue
      return
      end SUBROUTINE r1mpyq
c*******************************************************************
c     r1updt
c*******************************************************************
      SUBROUTINE r1updt(m,n,s,ls,u,v,w,sing)
C
      integer m,n,ls
      logical sing
      double precision s(ls),u(m),v(n),w(m)
c
c
c     given an m by n lower trapezoidal matrix s, an m-vector u,
c     and an n-vector v, the problem is to determine an
c     orthogonal matrix q such that
c
c                   t
c           (s + u*v )*q
c
c     is again lower trapezoidal.
c
c     this subroutine determines q as the product of 2*(n - 1)
c     transformations
c
c           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
c
c     where gv(i), gw(i) are givens rotations in the (i,n) plane
c     which eliminate elements in the i-th and n-th planes,
c     respectively. q itself is not accumulated, rather the
c     information to recover the gv, gw rotations is returned.
c
c
c       m is a positive integer input variable set to the number
c         of rows of s.
c
c       n is a positive integer input variable set to the number
c         of columns of s. n must not exceed m.
c
c       s is an array of length ls. on input s must contain the lower
c         trapezoidal matrix s stored by columns. on output s contains
c         the lower trapezoidal matrix produced as described above.
c
c       ls is a positive integer input variable not less than
c         (n*(2*m-n+1))/2.
c
c       u is an input array of length m which must contain the
c         vector u.
c
c       v is an array of length n. on input v must contain the vector
c         v. on output v(i) contains the information necessary to
c         recover the givens rotation gv(i) described above.
c
c       w is an output array of length m. w(i) contains information
c         necessary to recover the givens rotation gw(i) described
c         above.
c
c       sing is a logical output variable. sing is set true if any
c         of the diagonal elements of the output s are zero. otherwise
c         sing is set false.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar
c
c       fortran-supplied ... dabs,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more,
c     john l. nazareth
c
c*******************************************************************
      integer i,j,jj,l,nmj,nm1
      double precision cos,cotan,giant,one,p5,p25,sin,tan,tau,temp,
     *                 zero
      double precision dpmpar
      data one,p5,p25,zero /1.0d0,5.0d-1,2.5d-1,0.0d0/

c     giant is the largest magnitude.

      giant = dpmpar(3)

c     initialize the diagonal element pointer.

      jj = (n*(2*m - n + 1))/2 - (m - n)

c     move the nontrivial part of the last column of s into w.

      l = jj
      do 10 i = n, m
         w(i) = s(l)
         l = l + 1
   10    continue

c     rotate the vector v into a multiple of the n-th unit vector
c     in such a way that a spike is introduced into w.

      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 nmj = 1, nm1
         j = n - nmj
         jj = jj - (m - j + 1)
         w(j) = zero
         if (v(j) .eq. zero) go to 50

c        determine a givens rotation which eliminates the
c        j-th element of v.

         if (dabs(v(n)) .ge. dabs(v(j))) go to 20
            cotan = v(n)/v(j)
            sin = p5/sqrt(p25+p25*cotan**2)
            cos = sin*cotan
            tau = one
            if (dabs(cos)*giant .gt. one) tau = one/cos
            go to 30
   20    continue
            tan = v(j)/v(n)
            cos = p5/sqrt(p25+p25*tan**2)
            sin = cos*tan
            tau = sin
   30    continue

c        apply the transformation to v and store the information
c        necessary to recover the givens rotation.

         v(n) = sin*v(j) + cos*v(n)
         v(j) = tau

c        apply the transformation to s and extend the spike in w.

         l = jj
         do 40 i = j, m
            temp = cos*s(l) - sin*w(i)
            w(i) = sin*s(l) + cos*w(i)
            s(l) = temp
            l = l + 1
   40       continue
   50    continue
   60    continue
   70 continue

c     add the spike from the rank 1 update to w.

      do 80 i = 1, m
         w(i) = w(i) + v(n)*u(i)
   80    continue

c     eliminate the spike.

      sing = .false.
      if (nm1 .lt. 1) go to 140
      do 130 j = 1, nm1
         if (w(j) .eq. zero) go to 120

c        determine a givens rotation which eliminates the
c        j-th element of the spike.

         if (dabs(s(jj)) .ge. dabs(w(j))) go to 90
            cotan = s(jj)/w(j)
            sin = p5/sqrt(p25+p25*cotan**2)
            cos = sin*cotan
            tau = one
            if (dabs(cos)*giant .gt. one) tau = one/cos
            go to 100
   90    continue
            tan = w(j)/s(jj)
            cos = p5/sqrt(p25+p25*tan**2)
            sin = cos*tan
            tau = sin
  100    continue

c        apply the transformation to s and reduce the spike in w.

         l = jj
         do 110 i = j, m
            temp = cos*s(l) + sin*w(i)
            w(i) = -sin*s(l) + cos*w(i)
            s(l) = temp
            l = l + 1
  110       continue

c        store the information necessary to recover the
c        givens rotation.

         w(j) = tau
  120    continue

c        test for zero diagonal elements in the output s.

         if (s(jj) .eq. zero) sing = .true.
         jj = jj + (m - j + 1)
  130    continue
  140 continue

c     move w back into the last column of the output s.

      l = jj
      do 150 i = n, m
         s(l) = w(i)
         l = l + 1
  150    continue
      if (s(jj) .eq. zero) sing = .true.
      return
      end SUBROUTINE r1updt
C
C*********************************************************************
C REDMU 
C*********************************************************************
      double precision function redmu(s,redm)
      use gtst, only : gammam
      use gtst1, only : redmue
c
c     Evaluate the effective mass for a given value of s
c         for IVTST (eqn. 18a in JCP 95, 8875(1991)
c
      implicit double precision (a-h,o-z)
c
      bmu = (redmue(5) - redm)*4.0d0 
      y = exp(gammam*s)
      redmu = redm + bmu*y/((1.0d0 + y)**2)
      return
      end function redmu
c
c***********************************************************************
c  rfreq 
c***********************************************************************
C
c modified to read in imaginary frequency                               0606YC96
c 
      SUBROUTINE rfreq(k,nfreq,wi)
      use perconparam
      use gtst
c
c     Reads in frequencies
c
c     CALLED BY:
c                GIVTST
c
      implicit double precision (a-h,o-z)
      dimension nfreq(10)
      character*80 word(40)
      logical lsec,leof
c
      ir = 29
      kf = 0 
 50   call readln(ir,word,nword,lsec,leof)
      If(word(1) .ne. 'END') then
        if(.not.lsec.and..not.leof) then
          do 34 i=1,nword
            kf = kf + 1
            freq(k,kf) = cfloat(word(i))
            freq(k,kf) = freq(k,kf)*CMTOAU
   34     continue
        endif
          goto 50
      endif
c
      if(kf.gt.nfreq(k)) then
c
c Read in the imaginary frequency, which is the last in the list of TSfreq
c
         if (k.eq.5) then                                               0606YC96
            wi = abs(freq(k,kf))                                        0606YC96
            write(fu6,1251) wi*AUTOCM                                   0606YC96
         else      
            write (fu6,1300) nfreq(k)
            stop
         endif
      endif
c
c Sort frequencies in Canonical order, for bound modes, just descending order
c
      do i = 1, nfreq(k)                                                0211YC97
        do j = i+1, nfreq(k)                                            0211YC97
         if (freq(k,i).lt.freq(k,j)) then                               0211YC97
            tempfr = freq(k,i)                                          0211YC97
            freq(k,i) = freq(k,j)                                       0211YC97
            freq(k,j) = tempfr                                          0211YC97
         endif                                                          0211YC97
        enddo                                                           0211YC97
      enddo                                                             0211YC97
      write (fu6,1252) (freq(k,i)*AUTOCM,i=1,nfreq(k))
      return
1251  format(/,1X,'IMAGINARY FREQUENCY (CM**-1)',//,10x,F10.2,'i')      0606YC96
1252  format(/,1X,'VIBRATIONAL FREQUENCIES (CM**-1)',//,(10X,F10.2))
1300  format(/,1X,'There should be only ',I4,' frequencies')
      end SUBROUTINE rfreq
c***********************************************************************
c     RGEN 
c***********************************************************************
c
      SUBROUTINE rgen(labini)
      use perconparam; use gtst
c     
c     Reads general infomation for IVTST calculations
c
c     CALLED BY:
c                FDIAG2
c     CALLS:
c                READLN, CFLOAT, ICINT
c
      implicit double precision (a-h,o-z)
c
      character*80 word(40)
      logical lsec,leof
c
c     vmep  - classical energy at the extra point
c     ds    - s value at the extra point
c     ds    - s value at the extra point
c     vc    - barrier of reaction
c     dele  - delta E of reaction 
c     delh  - delta H of reaction
c     WER  (reactant frequencies)
c
      ir = 29
c     set default values
      labini = 1
      iprint = 0
      iscsag = 0
c
c     Read in the section name
c
      call readln(ir,word,nword,lsec,leof)
      If(.not. lsec .or. word(1) .ne. 'IVTST') then
         write(fu6,6000)
         stop
      endif
c
c     Read in the first keyword
c
      call readln(ir,word,nword,lsec,leof)
c
100   if (.not. lsec .and. .not. leof) then
c
c UNITS  
         if(word(1).EQ.'UNITS') THEN
            if (nword .eq. 2) then
               labini = icint(word(2))
            else
               write(fu6,5200) 'UNITS',1
               stop
            endif
c
c PRINT  
         elseif(word(1).EQ.'PRINT') then
            if (nword .eq. 2) then
               iprint = icint(word(2))
            else
               write(fu6,5200) 'PRINT',1
               stop
            endif
c
c BARRI   
         elseif(WORD(1).EQ.'BARRI') then
            if (nword .eq. 2) then
               vc = cfloat(word(2))
            else
               write(fu6,5200) 'BARRI',1
               stop
            endif
c
c DELTAE
         elseif(word(1).EQ.'DELTAE') then
            if (nword .eq. 2) then
               dele = cfloat(word(2))
            else
               write(fu6,5200) 'DELTAE',1
               stop
            endif
c
c SCSAG
         elseif(word(1).EQ.'SCSAG') then
            iscsag = 1
c
c DELEX1
c
         elseif(word(1).EQ.'DELEX1') then
            if (nword .eq. 2) then
               vmep = cfloat(word(2))
            else
               write(fu6,5200) 'DELEX1',1
               stop
            endif
c
c SEX1
         elseif(word(1).EQ.'SEX1') then
            if (nword .eq. 2) then
               ds = cfloat(word(2))
            else
               write(fu6,5200) 'SEX1',1
               stop
            endif
         elseif(word(1).EQ.'END') then
            goto  200
         else
            write(fu6,7000) word(1)
            stop
         endif
C finished processing a keyword, read the next nonblank, noncomment line
         call readln(ir,word,nword,lsec,leof)
         goto 100
c
c end of unit fu71 input
c
      endif
 200  continue
c
      return
 1220 format(/,1X,'At s = ',F6.3,'   Classical Energy = ',F10.3,
     * ' kcal/mol')
 5200 format(/1X,'The keyword ',A12,' must have ',I1,' argument(s).')
 7000 format(/1X,'Unrecoginized keyword: ',A12)
 6000 format(/1X,'The first line of unit fu29 input must be *IVTST')
      end SUBROUTINE rgen
C
c***********************************************************************
c  rgrd
c***********************************************************************
c
      SUBROUTINE rgrd(k,labini,ncoord,na,index,redm,x,y,z,amass)
      use gtst
      use perconparam
c
c     Reads in gradients and mass-scales them.
c
c     CALLED BY:
c                GIVTST
c     CALLS:
c                TRANS2
c
      implicit double precision (a-h,o-z)
      dimension na(10),x(natoms),y(natoms),z(natoms),amass(natoms),
     *              index(10,natoms)
      character*80 word(40)
      character*6 sname
      logical lsec,leof
c
c  Read gradients for extra points on the MEP
c
      ir = 29
      if(k .eq. 5) then
          do 30 i=1,ncoord
             dx(k,i) = 0.0d0
 30       continue
      endif
c
      if(k .gt. 5) then
          sname = 'E1GRAD'
          call readln(ir,word,nword,lsec,leof)
          If(.not. lsec .or. word(1) .ne. sname) then
             write(fu6,6000)sname
             write(fu6,7000) word(1)
             stop
          endif
          read(ir,*)(dx(k,i), i = 1,ncoord)
c          do 40 i = 1,ncoord/3
c             call readln(ir,word,nword,lsec,leof)
c             if(.not.lsec.and..not.leof) then
c                dx(k,i*3-2) = cfloat(word(1))
c                dx(k,i*3-1) = cfloat(word(2))
c                dx(k,i*3)   = cfloat(word(3))
c             endif
c   40     continue
c
c         read END
c
          call readln(ir,word,nword,lsec,leof)
          If(lsec .or. word(1) .ne. 'END') then
             write(fu6,8000)
             write(fu6,7000) word(1)
             stop
          endif
c
c         convert to mass-scaled gradients
c
          if(labini.eq.1) call trans2(k,2,na,index,redm,x,y,z,amass)
c
c         Normalize gradients
c
          sum=0.d0
          do 52 i=1,ncoord
             sum = sum+ dx(k,i)*dx(k,i)
 52       continue
          xnorm= sqrt(sum)
          do 63 i=1,ncoord
             dx(k,i)= dx(k,i)/xnorm
 63       continue
      endif
      return
6000  format(/1X,'expecting *',A6,' for input')
7000  format(/1X,'Unrecoginized keyword: ',A12)
8000  format(/1X,'expecting END for input')
      end SUBROUTINE rgrd
c***********************************************************************
c  rhess
c***********************************************************************
c
      SUBROUTINE rhess(k,labini,na,index,x,y,z,amass,redm)
      use gtst
      use perconparam
c
c     Reads in Hessians and mass-scales them.
c
c     CALLED BY:
c                GIVTST
c     CALLS:
c                TRANS2
c
      implicit double precision (a-h,o-z)
      dimension na(10),index(10,natoms),x(natoms),y(natoms),z(natoms),
     * amass(natoms)
      character*80 word(40)
      logical lsec,leof
c
      ir = 29
C
C     free packed force constants input    1002YC96
C
      l  = 0
      j  = 1
      N  = 3*na(k)

      do im = 1,N
      do jm = 1,N
        forc(im,jm)=0.0d0
      enddo 
      enddo

 50   call readln(ir,word,nword,lsec,leof)
      if (word(1).eq.'END') then
          if (j.ne.N.or.l.ne.N) then
             write(fu6,*) 'NOT ENOUGH NUMBERS ENTERED FOR A PACKED', 
     *                   N,' BY ',N,'HESSIAN'
             stop 'rhess 1'
          endif
      else  
        do 61 i = 1,nword
           l = l+1
           if (l.gt.j) then
             j = j+1
             l = 1
           endif
           if (j.gt.N) then
             write(fu6,*) 'TOO MANY NUMBERS ENTERED FOR A PACKED',N,
     *             ' BY ',N,'HESSIAN'
             stop 'rhess 2'
           endif
           forc(j,l) = cfloat(word(i))
 61   continue
      goto 50
      endif
      if (iprint.eq.1) then 
        write (fu6,*) 'Cartesian force constants read in'
        do im = 1,N
           write(fu6,1200) (forc(im,jm),jm=1,im)
        enddo
      endif
c
c   convert to mass-scaled hessian
c
      if (labini.eq.1) call trans2(k,3,na,index,redm,x,y,z,amass)
c
      do 36 j = 1,N
      do 36 l = 1,j
          forc(l,j) = forc(j,l)
 36   continue
c
c  Save Hessian matrix for Page McIver curvature calculation
c
      do 60 i = 1,N
      do 60 j = 1,N
        if (k .eq. 5) forcs(i,j) = forc(i,j)
        if (k .eq. 6) forc1(i,j) = forc(i,j)
 60   continue
1200  FORMAT (2X,1P,5E15.5)
      return
      end SUBROUTINE rhess
c
c***********************************************************************
c  rfhsec
c***********************************************************************
c
      SUBROUTINE rhfsec(k,lfreq)
      use perconparam
c
c     Checks whether frequencies or hessians to read in.
c
c     CALLED BY:
c                GIVTST
c
      implicit double precision (a-h,o-z)
      character*80 word(40)
      character*6 fname, hname
      logical lsec,leof,lfreq
c
      ir = 29
      lfreq = .false.
c
c     Read in the HESS section name
c     Read in the XnFREQ section name
c
      if(k.eq.1) hname = 'R1HESS'
      if(k.eq.2) hname = 'R2HESS'
      if(k.eq.3) hname = 'P1HESS'
      if(k.eq.4) hname = 'P2HESS'
      if(k.eq.5) hname = 'TSHESS'
      if(k.eq.6) hname = 'E1HESS'
      if(k.eq.1) fname = 'R1FREQ'
      if(k.eq.2) fname = 'R2FREQ'
      if(k.eq.3) fname = 'P1FREQ'
      if(k.eq.4) fname = 'P2FREQ'
      if(k.eq.5) fname = 'TSFREQ'
      if(k.eq.6) fname = 'E1FREQ'
c
      call readln(ir,word,nword,lsec,leof)
      If(.not.lsec.or.word(1).ne.hname.and.word(1).ne.fname)then
         write(fu6,6000)hname,fname
         write(fu6,7000) word(1)
         stop
      endif
      If(word(1).eq.fname) lfreq =.true.
      return
6000  format(/1X,'expecting *',A6,' or *',A6,' for input')
7000  format(/1X,'Unrecoginized keyword: ',A12)
      end SUBROUTINE rhfsec
C*****************************************************************
C RPARTI
C*****************************************************************
C
C  CALCULAT THE ROTATIONAL PARTITION FUNCTION IN AU UNIT
C
C
      double precision function rparti(fi,bkt,iop)
      use perconparam
      implicit double precision (a-h,o-z)
C
      RPARTI = 0.0D0
      IF (IOP .EQ. 1) THEN
c atom
         RPARTI = 1.0D0
      ELSEIF (IOP .EQ. 2) THEN
C
C linear molecule
C
         RPARTI = 2.0D0*FI*BKT
       ELSE
C
C non-linear molecule
C
         RPARTI = SQRT(8.D0*PI*FI*BKT**3)
      ENDIF
      RETURN
      END function rparti
C
C***********************************************************************
C  RST
C***********************************************************************
C
      SUBROUTINE rst (NM,N,A,W,MATZ,Z,FV1,FV2,IERR)
      use perconparam
C
C     PATCH ROUTINE USING RSPP TO DO DIAGONALIZATION
C
C     CALLED BY:
C                FDIAG2
C     CALLS:
C            RSPP
C
      implicit double precision (a-h,o-z)
      DIMENSION A(NM,N),W(N),Z(NM,N),FV1(N),FV2(N)
      DIMENSION B(NB)
      NV = N*(N+1)/2
      L = 1
      DO 10 J = 1, N
         DO 10 I = 1, J
            B(L) = A(I,J)
            L = L+1
   10 CONTINUE
      CALL RSPP (NM,N,NV,B,W,MATZ,Z,FV1,FV2,IERR)
      RETURN
      END SUBROUTINE rst
C************************************************************************
C sortg
C************************************************************************
C
      SUBROUTINE sortg(n,f)
c
c     Sorts elements in array f in descending order
c    
      implicit double precision (a-h,o-z)
      dimension f(*)
c
      isw = 1
10    continue
      if(isw .eq. 0) goto 30
      isw = 0
      do 20 i = n,2,-1
         if (f(i) .le. f(i-1)) goto 20
            isw = isw + 1
            temp = f(i-1)
            f(i-1) = f(i)
            f(i) = temp
20    continue
      goto 10
30    return
      end SUBROUTINE sortg
C
C***********************************************************************
C  TRANS2 
C***********************************************************************
C
      SUBROUTINE trans2(kop,iop,na,index,redm,x,y,z,amass)
      use gtst
      use gtst1, only : xp,amassp
      use perconparam
      implicit double precision (a-h,o-z)
C
C     MASS-SCALES CARTESIAN COORDINATES (IOP = 1)
C     MASS-SCALES CARTESIAN GRADIENTS (IOP = 2)
C     MASS-SCALES CARTESIAN HESSIAN   (IOP = 3)
C
      dimension amasst(maxcar)
      dimension na(10),x(natoms),y(natoms),z(natoms),amass(natoms),
     *              index(10,natoms)
C
      NEND =3*NA(KOP)
C
      L = 1
      DO 10 I=1,NA(KOP)
        XP(L) = X(I)
        XP(L + 1) = Y(I)
        XP(L + 2) = Z(I)
        J = INDEX(KOP,I)
        AMASSP(L) = AMASS(J)
        AMASSP(L + 1) = AMASS(J)
        AMASSP(L + 2) = AMASS(J)
        L = L+3
10    CONTINUE
C
      DO 20 L=1,NEND
         AMASST(L) = SQRT(AMASSP(L)/REDM)
20    CONTINUE
      DO 30 I = 1, NEND
         IF (IOP.EQ.1) THEN
            XP(I) = XP(I)*AMASST(I)
         ELSE IF(IOP .EQ. 2)THEN
            DX(KOP,I) = DX(KOP,I)/AMASST(I)
         ELSE IF(IOP .EQ. 3)THEN
            DO 40 J = 1,I
              FORC(I,J) = FORC(I,J)/(AMASST(I)*AMASST(J))
40          CONTINUE
         ENDIF
30    CONTINUE
      RETURN
      END SUBROUTINE trans2
C*********************************************************************
C TSC 
C*********************************************************************
      SUBROUTINE tsc (redm,E,TE,NTP,STP,SS,ICHAR,IMAX,VMAX,IOP)
      use gtst
      use gtst1, only : NQT,NQT2,av,bv,cv,dv,ns
      use perconparam
C
C     LOCATES POINTS WHERE E=VA AND CALCULATES THETA= INTEGRAL OF
C     IM(PS) BETWEEN THEM
C     TE(I) IS THE PROBAB(E) FOR THE I-TH QUADRATURE (N OR 2N+1)
C
      implicit double precision (a-h,o-z)
      EXTERNAL REDMU
      DIMENSION TE(2),SUM(2),THETA(2),TAB(3)
c     DIMENSION STP(6),SS(1),ICHAR(6)
      DIMENSION STP(6),SS(*)                                            1020BE05
      character*2 ichrl,ichrr
      character*2 ichar(6)
      save                                                              0601YC98
C
C Find the turning points
C
      NTP = 0
C
C     FOLLOWING VALUES SET IN CASE NO TURNING POINTS FOUND
C
      DO 10 I = 1, 2
         TE(I) = 0.5D0
         THETA(I)= 0.0D0
   10 CONTINUE
C
      IFLG = 0
   20 CONTINUE
      CALL PSATX(IFLG,E,SL,SR,SN,XT,ICHRL,ICHRR,IMAX,VMAX)
      IF (IFLG.EQ.0) GO TO 100
C
C     PAIR OF T.P.'S FOUND. -- CALCULATE THETA INTEGRAL.
C     I=1,2 FOR DIFFERENT QUADRATURES (N AND 2N+1)
C     FOR MCPSAG, INCLUDE CURVATURE CORRECTION, WHICH IS REALLY AN
C     EFFECTIVE MU
C
      DO 30 I = 1, 2
         SUM(I) = 0.0D0
   30 CONTINUE
      SRXMU = SQRT(REDM)
      DO 60 N = 1, NQT2
         S = SN + XT*PTT(N)
         CALL SPL1B2 (NS,SS,av,bv,cv,dv,s,TAB,0)
         T = TAB(1)-E
         IF (T.LT.0.0D0) GO TO 60
         T = SQRT(T)
         IF (IOP .EQ. 0) THEN
               DO 40 I = 1, 2
                  SUM(I) = SUM(I)+T*WTT(N,I)*SRXMU
   40          CONTINUE
C
C  Include curvature corrections for scsag
C
         ELSE
               REDMUS = REDMU(S,redm)
200            FORMAT(1X,F6.3,2X,F6.3)
             IF (REDMUS .LE. 0.D0) THEN
                LNEG = .TRUE.
             ELSE
                T = T*SQRT(REDMUS)
                DO 50 I = 1, 2
                  SUM(I) = SUM(I)+T*WTT(N,I)
   50           CONTINUE
             END IF
         END IF
   60 CONTINUE
C
C     ADD CONTRIBUTION OF T.P. PAIR TO OVERALL THETA AND P(E)
C     NOTE -- NO CONNECTION FORMULAS USED
C
      T = SQRT(2.0D0)*XT
      DO 70 I = 1,2
         TE(I) = 0.0D0
         THETA(I) = THETA(I)+T*SUM(I)
         TE(I) = 1.0D0/(1.0D0 + EXP(2.0D0*THETA(I)))
70    CONTINUE
C
      NTP = NTP+2
C
C     SAVE UP TO THREE T.P. PAIRS
C
      IF (NTP.GT.6) GO TO 20
      STP(NTP-1) = SL
      STP(NTP) = SR
      ICHAR(NTP-1) = ICHRL
      ICHAR(NTP) = ICHRR
C
C     LOOP BACK TO SEARCH FOR MORE T.P. PAIRS
C
      GO TO 20
100   CONTINUE
      RETURN
      END SUBROUTINE tsc 
C*********************************************************************
C     vivt(x)
C*********************************************************************
      double precision function vivt(x)
      use gtst
c
c     Calculates the Eckart potential
c
c     vivt(x) = Ay/(1+y) + By/(1+y)**2        y = exp(gamma*(x-xm))
c
c     CALLED BY:
c                IRPHZ, IRPH
c
      implicit double precision (a-h,o-z)
      
      y = exp(gammav*(x+xmvc))
      vivt = avc*y/(1.0d0+y) + bvc*y/(1.0d0+y)**2
      return
      end function vivt
C****************************************************************
C VPARTI
C****************************************************************
C
C  CALCULATE THE VIBRATIONAL PARTITION FUNCTION  USING HARMONIC
C  APPROXIMATION.
C
      double precision function vparti(w,bkt,iclasv)                    0808JC00
      implicit double precision (a-h,o-z)
C
C     IF SELECTED, THE VIBRATIONAL PARTITION FUNCTION IS GOING TO BE    0528JC97
C     CALCULATED CLASSICALLY (ALWAYS HARMONIC, NO CHECK OF ANHARMONICITY0528JC97
C     IS MADE).                                                         0528JC97
C                                                                       0528JC97
      IF (ICLASV.EQ.1) THEN                                             0528JC97
C                                                                       0528JC97
C     CHECK THE FREQUENCY IS NOT IMAGINARY                              0528JC97
C                                                                       0528JC97
            IF (W.LE.0.D0) THEN                                         0528JC97
                  VPARTI=1.0D10                                         0528JC97
            ELSE                                                        0528JC97
                  VPARTI=BKT/W                                          0528JC97
            ENDIF                                                       0528JC97
            RETURN                                                      0528JC97
      ENDIF                                                             0528JC97
C
      sum = exp(-w/(2.d0*bkt))
      vparti = sum/(1.d0 - sum*sum)
      return
      end function vparti
C
C***********************************************************************
C  VSPLIN  --> VSPLI2
C***********************************************************************
C
      SUBROUTINE vspli2 (NSMAX,S,SMAX,V,VMAX,IMAX)
      use gtst1
      use perconparam
C
C     CALLED BY:
C                 KAPVA
C     CALLS:
C           SPL1D1,SPL1B1,SPL1B2
C
      implicit double precision (a-h,o-z)
      DIMENSION SCR(NSDM),IOP(2),S(NSDM),V(NSDM),ROOT(2),TAB(3)
      IERRCN = 0
      NS = NSMAX
C
C     GENERATE SPLINE FIT TO VA
C
      IOP(1) = 5
      IOP(2) = 5
      SCR(NS) = 0.0D0
      CALL SPL1D1 (NS,S,V,SCR,IOP,1,AV,BV,CV)
      CALL SPL1B1 (NS,S,V,SCR,1,AV,BV,CV,DV)
C
C     FIND SPLINE FIT MAX OF VAD
C     FIRST FIND LARGEST V VALUE
C
      VMAX = 0.0D0
      DO 10 I = 1, NS
         IF (V(I).LT.VMAX) GO TO 10
         VMAX = V(I)
         IMAX = I
   10 CONTINUE
      IF (IMAX.EQ.1.OR.IMAX.EQ.NS) IERRCN = 1
      IF (IERRCN.NE.0) THEN
         VMAX = V(IMAX)
         SMAX = S(IMAX)
         WRITE(fu6,*) 'MAXIMUM OF ADIABATIC BARRIER AT EDGE OF GRID'
         STOP 'VSPLI2 1'
      ELSE
         IM = IMAX-1
C
C     FIND SMAX FROM SPLINE FIT COEFFICIENTS
C     LOOK ON BOTH SIDES OF LARGEST V AND CHOOSE LARGEST VMAX
C
         IMX = 0
         VMAX = 0.0D0
         DO 30 ISIDE = 1, 2
            S1 = S(IM)
            S2 = S(IM+1)
            ARG = BV(IM)*BV(IM)-3.0D0*AV(IM)*CV(IM)
            IF (ARG.GE.0.0D0) THEN
               DET = SQRT(ARG)
               ROOT(1) = (-BV(IM)+DET)/(3.0D0*AV(IM))
               ROOT(2) = (-BV(IM)-DET)/(3.0D0*AV(IM))
               DO 20 IROOT = 1, 2
                  IF (ROOT(IROOT).LE.S2.AND.ROOT(IROOT).GE.S1) THEN
                     CALL SPL1B2 (NS,S,AV,BV,CV,DV,ROOT(IROOT),TAB,0)
                     IF (TAB(1).GT.VMAX) THEN
                        VMAX = TAB(1)
                        SMAX = ROOT(IROOT)
                        IMX = IM
                     ENDIF
                  ENDIF
   20          CONTINUE
            ENDIF
            IM = IM+1
   30    CONTINUE
         IF (IMX.EQ.0) THEN
            WRITE (fu6,1000)
            STOP 'VSPLI2 2'
         ENDIF
         WRITE (fu6,1100) SMAX,VMAX,VMAX*CKCAL
         IMAX = IMX
      ENDIF
C
 1000 FORMAT(1X,20(1H$),33H  UNABLE TO FIND SMAX FROM SPLINE)
 1100 FORMAT(/,6X,23HFROM SPLINE FIT, SMAX =,F12.6,/,1x,7HVMAX = ,
     *1PE20.10,' HARTREES',3H  (,0PF9.4,6H kcal))
C
      END
C***********************************************************************
C     ZERP
C***********************************************************************
      DOUBLE PRECISION FUNCTION ZERP(J,L,X)
      use gtst
      use perconparam
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      ECK(XI)= A*EXP(ALF*(XI-SM))/(1.D0+EXP(ALF*(XI-SM))) +
     *         B*EXP(ALF*(XI-SM))/(1.D0+EXP(ALF*(XI-SM)))**2 + C
      WTAH(XI) = A*TANH(GAMMA*(XI-B)) + C
c
      XI=X
      IFCN = PAR(J,1)
      A = PAR(J,2)
      B = PAR(J,3)
      C = PAR(J,4)
      IF (IFCN .EQ. 0)GAMMA = PAR(J,5)
      IF (IFCN .NE. 0)THEN
         SM = PAR(J,6)
         ALF = PAR(J,7)
         XP = ALF*(XI - SM)
      END IF
C
      IF (IFCN .NE. 0) THEN
C
C   Machine dependent instruction
C
           IF (XP .GT. 40.0D0) THEN
             GFREQ(J,L) = (A + C)/CKCAL
C 
           ELSE
             GFREQ(J,L) = ECK(XI)/CKCAL
           END IF
      ELSEIF (IFCN .EQ. 0)THEN
            GFREQ(J,L) = WTAH(XI)/CKCAL
      ENDIF
C
      IF (GFREQ(J,L) .LT. 0.0D0) THEN
         ZERP = 0.0D0
      ELSE
         ZERP=  0.5D0*GFREQ(J,L)
      END IF
C
      RETURN
      END function zerp
c***********************************************************************
c  zrpte
c***********************************************************************
c
      SUBROUTINE zrpte(k,nfreq,zrpt)
      use gtst
      use perconparam
c
c     Evaluates the zero-point energy of reactants.
c
c     CALLED BY:
c                GIVTST
c
      implicit double precision (a-h,o-z)
c
      dimension zrpt(10),nfreq(10)
      sum = 0.0D0
      do 50 I = 1,nfreq(k)
        sum = sum + 0.5d0*freq(k,i)
50    continue
      zrpt(k) = sum
      return
      end SUBROUTINE zrpte

c***********************************************************************
C  CUBST
C***********************************************************************
C
      SUBROUTINE cubst
C
C     Calculate parameters for cubic starting algorithm
C
C     CALLED BY:
C                PATH
C     CALL:
C                MXLNEQ,TRANS,FIRST
C
      use keyword_interface
      use energetics_mod
      use common_inc
      use rate_const, only : v3
      use perconparam
      use rate_const, only : vecsv
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      allocate(v3(n3)); v3=0.d00
      call cubst_local_mem
C
C              Save initial value of X and make initial step 
C              on the product side
C
               DO 1 I=1,N3                                              7/1/91VM
                  XS(I)=X(I)                                            7/1/91VM
                  X(I)=XS(I)+VECSV(I)*2.0D0*D3LX                        7/1/91VM
   1           CONTINUE                                                 7/1/91VM
C
C              Transformation to cartesian coordinates
C
               CALL TRANS(2,N3,AMASS,X,DX)                              7/1/91VM
               DO 2 I=1,N3                                              7/1/91VM
                  XIX(I)=X(I)                                           7/1/91VM
   2           CONTINUE                                                 7/1/91VM
C
C              Calculation of the force constant matrix
C
               call hhook(xix,f1,dlx,n,0,0,0)                           0301YC97
C
C              Initial step on the reactant side
C
               DO 6 I=1,N3                                              7/1/91VM
                  X(I)=XS(I)-VECSV(I)*2.0D0*d3lx                        7/1/91VM
   6           CONTINUE                                                 7/1/91VM
C
C              Transformation to cartesian coordinates
C
               CALL TRANS(2,N3,AMASS,X,DX)                              7/1/91VM
               DO 7 I=1,N3                                              7/1/91VM
                  XIX(I)=X(I)                                           7/1/91VM
   7           CONTINUE                                                 7/1/91VM
C
C              Calculation of the force constant matrix 
C              (second derivatives of the energy with respect to x) 
C              and of the third derivatives of the energy 
C              with respect to x with Central Differences.
C
               call hhook(xix,f2,dlx,n,0,0,0)                           0301YC97
C
               H23=2.0D0*D3LX                                           6/2RS94
               DO 8 I=1,N3                                                ...
                  DO 8 J=1,I                                              ...
                     FTH(I,J)=(F1(I,J)-F2(I,J))/(2.0D0*H23)               ...
                     FTH(J,I) = FTH(I,J)                                  ...
   8           CONTINUE                                                 6/2RS94
C
C              Calculation of the curvature vector related 
C              to third derivatives.
C
               DO 12 I = 1, N3                                          7/1/91VM
                  VT1(I) = 0.0D0                                        7/1/91VM
                  VT4(I) = 0.0D0                                        7/1/91VM
                  DO 12 J = 1, N3                                       7/1/91VM
                     VT1(I) = VT1(I) + FTH(I,J)*VECSV(J)                7/1/91VM
                     VT4(I) = VT4(I) + F(I,J)*VECSV(J)                  7/1/91VM
  12           CONTINUE                                                 7/1/91VM
C
               ST1 = 0.0D0                                              7/1/91VM
               ST2 = 0.0D0                                              7/1/91VM
               DO 13 I = 1, N3                                          7/1/91VM
                  ST1 = ST1 + VECSV(I)*VT1(I)                           7/1/91VM
                  ST2 = ST2 + VECSV(I)*VT4(I)                           7/1/91VM
  13           CONTINUE                                                 7/1/91VM
C
               DO 14 I = 1, N3                                          7/1/91VM
                  VT2(I) = ST1*VECSV(I)                                 7/1/91VM
                  VT3(I) = VT1(I) - VT2(I)                              7/1/91VM
                  DO 14 J = 1, N3                                       7/1/91VM
                     FIN(I,J) = -F(I,J)                                 7/1/91VM
                     IF (I .EQ. J) THEN                                 7/1/91VM
                        FIN(I,J) = 2.0D0*ST2 - F(I,J)                   7/1/91VM
                     ENDIF                                              7/1/91VM
                     IF (J .EQ. N3) THEN
                        FIN(I,J + 1) = VT3(I)
                     ENDIF
  14           CONTINUE                                                 7/1/91VM
C
               CALL MXLNEQ(FIN, N3, N3TM, DET, JRANK, EPS,              7/1/91VM
     *                    IWORK, -1, N3+1)                              7/1/91VM
               IF (JRANK .LT. N3) WRITE (FU6,*) 'JRANK = ', JRANK
C
               DO 15 I = 1, N3                                          7/1/91VM
                  X(I)=XS(I)                                            7/1/91VM
                  DX(I) = VECSV(I)                                      3/6/91BG
                  V3(I) = FIN(I,N3+1)                                   7/1/91VM
  15           CONTINUE                                                 7/1/91VM
C
      RETURN
C
      END subroutine cubst
C***********************************************************************
C  DERIV2
C***********************************************************************
C
      SUBROUTINE deriv2 (N)
C
C     CALCULATES THE HESSIAN MATRIX BY USING QUADRATIC FITS TO THE
C     SURFACE.  ONLY THE POTENIAL IS USED, NOT THE 1ST DERIVATIVES.
C     FOR FORMULAS USED, SEE ABRAMOWITZ AND STEGUN, "HANDBOOK OF
C     MATHEMATICAL FUNCTIONS," 1964. 25.3.23 AND 25.3.26.
C     WRITTEN BY KEN DYKEMA, 7/16/84.
C
C     Include statements were added 6/18/91
C
C     CALLED BY:
C               NORMOD
C     CALLS:
C           ENERG 
C
 
C 
      use common_inc
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IPROC=0
C
      CALL EHOOK(0,iproc)                                               0301YC97
C
      V00 = V
      STP2 = DERSTP**2
      DO 10 I = 1, N
         XOLD = X(IND(I))
         X(IND(I)) = X(IND(I))+DERSTP
         CALL EHOOK(0,iproc)                                            0301YC97
C
         V10 = V
         X(IND(I)) = XOLD-DERSTP
         CALL EHOOK(0,iproc)                                            0301YC97
C
         F(I,I) = (V10-2.0D0*V00+V)/(STP2*AMASS(IND(I))**2)
         X(IND(I)) = XOLD
   10 CONTINUE
      DO 30 I = 1, N-1
         DO 20 J = I+1, N
            XIOLD = X(IND(I))
            XJOLD = X(IND(J))
            X(IND(I)) = X(IND(I))+DERSTP
            X(IND(J)) = X(IND(J))+DERSTP
            CALL EHOOK(0,iproc)                                         0301YC97
C
            V11 = V
            X(IND(J)) = XJOLD-DERSTP
            CALL EHOOK(0,iproc)                                         0301YC97
C
            V1N1 = V
            X(IND(I)) = XIOLD-DERSTP
            CALL EHOOK(0,iproc)                                         0301YC97
C
            VN1N1 = V
            X(IND(J)) = XJOLD+DERSTP
            CALL EHOOK(0,iproc)                                         0301YC97
C
            F(I,J) = (V11-V1N1-V+VN1N1)/(4.0D0*STP2*AMASS(IND(I))*AMASS(
     *         IND(J)))
            F(J,I) = F(I,J)
            X(IND(I)) = XIOLD
            X(IND(J)) = XJOLD
   20    CONTINUE
   30 CONTINUE
      RETURN
      END subroutine deriv2
c
C***********************************************************************
C  DERIV24
C***********************************************************************
C
      SUBROUTINE derv24 (NEND)
C
C     THIS SUBROUTINE COMPUTES THE SECOND DERIVATIVES BY FITTING THE
C     ANALYTICAL FIRST DERIVATIVES TO A FOURTH ORDER POLYNOMIAL AROUND
C     THE DESIRED VALUE AND THEN TAKING THE DERIVATIVE OF THIS POLY.
C     ADDED 11/13/85.
C
C
C     Include statements were added 6/18/91
C
C     CALLED BY:
C               NORMOD
C     CALLS:
C           FIRST
C
C
      use common_inc
      use perconparam, only : n3tm
c     use energetics_mod
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      integer, intent(in) :: nend
      real(8), allocatable :: f2(:),xxi(:)
      if(allocated(f2))deallocate(f2)
      allocate(f2(n3tm)); f2=0.d00
      if(allocated(xxi))deallocate(xxi)
      allocate(xxi(n3)); xxi=0.d00
C
C
      H = DLX
C
C     SAVE INITIAL GEOMETRY SO IT CAN BE RESTORED AT THE END
C
      DO 10 I = 1, N3
         XXI(I) = X(I)
   10 CONTINUE
C
C     COMPUTE THE DERIVATIVE BY USING STEP SIZES OF H AND 2H
C
      DO 60 I = 1, NEND
         X(IND(I)) = XXI(IND(I))-2.0D0*H
         call ghook(0,iproc)                                            0301YC97
C
         DO 20 J = 1, I
            F2(J) = DX(IND(J))
   20    CONTINUE
         X(IND(I)) = XXI(IND(I))+2.0D0*H
         call ghook(0,iproc)                                            0301YC97
C
         DO 30 J = 1, I
            F2(J) = DX(IND(J))-F2(J)
   30    CONTINUE
         X(IND(I)) = XXI(IND(I))-H
         call ghook(0,iproc)                                            0301YC97
C
         DO 40 J = 1, I
            F(I,J) = DX(IND(J))
   40    CONTINUE
         X(IND(I)) = XXI(IND(I))+H
         call ghook(0,iproc)                                            0301YC97
C
         DO 50 J = 1, I
            F(I,J) = (8.0D0*(DX(IND(J))-F(I,J))-F2(J))/(12.0D0*H)
            F(I,J) = F(I,J)/(AMASS(IND(I))*AMASS(IND(J)))
            f(j,i) = f(i,j)                                             6/6RS94
   50    CONTINUE
         X(IND(I)) = XXI(IND(I))
   60 CONTINUE
      RETURN
      END subroutine derv24
c
C***********************************************************************
C  gsetm
C***********************************************************************
C
c    This routine was written on 6/7/94.  It contains the preliminary
c    geometry optimization calculations needed when POLYRATE is used
c    with MOPAC.  Most of this code used to be in the POLYAT subroutine.
C
      SUBROUTINE gsetm(IOP)
C
C     Called By:
C                OHOOK
C
      use perconparam
      use common_inc
      implicit double precision (a-h,o-z)
c
c     Set-up indicies search is carried out over        
c                                                                    
      if (iop.eq.5) then
         do 30 i = 1,n3
30          ind(i) = i
         write(fu6,1800)                                                1221WH94
         write (fu6,1700) (nedeg(i),elec(i),i=13,15)                    1221WH94
      else 
         ndim(iop) = 3*nratom(iop)
         l = 0                   
         nrend = nratom(iop)    
         do 20 i = 1, nrend    
               jnd = 3*(iatom(i)-1) 
         do 20 j = 1, 3            
               l = l+1            
               ind(l) = jnd+j    
20       continue               
      end if
c
 1700 format(1x,'Electronic degeneracies and energies (a.u.) = ',
     * i4,2x,f12.8,/,(47x,i4,2x,f12.8))
 1800 format(//1X,32(1H*),' Saddle point ',32(1H*)/)
      return
      end subroutine gsetm
c
c***********************************************************************
c  gsetp
c***********************************************************************
c
c    This routine was written on 6/7/94.  It contains the preliminary
c    geometry optimization calculations needed when POLYRATE is used
c    with an analytical surface.  Most of this code used to be in
c    the POLYAT subroutine.
c
      SUBROUTINE gsetp(IOP)
c
c     Called By:
c                OHOOK
c
      
c
      use common_inc
      use perconparam, only : fu6
      use keyword_interface, only : iunit6,gufac6
      implicit double precision (a-h,o-z)
C       if(iopc == 4) stop 'prod2'
c
c     Prepare saddle point optimization
c
      if (iop.eq.5) then
         do 10 i = 1,n3 
            x(i) = xr(i,5)
10       continue
       if(iunit6.eq.1) then                                             0405JZ07
         write (fu6,1600)
         write (fu6,1650) (x(i)/gufac6,i=1,n3)
       else
         write (fu6,1610)
         write (fu6,1650) (x(i)/gufac6,i=1,n3)
       endif                                                            0405JZ07
         write (fu6,1700)
         write (fu6,1800) (nedeg(i),elec(i),i=13,15)
      end if
c 
      ndimx = ndim(iop)
      do 20 i = 1,ndimx
            ind(i) = indx(i,iop)
20    continue
      write (fu6,1900) iop,ndim(iop),(ind(i),i=1,ndimx)
C
 1600 FORMAT(//1X,'********* Saddle point:'//6X,
     *'Initial saddle point geometry in space-fixed cartesians (bohrs)',1207WH92
     * //15X,1HX,13X,1HY,13X,1HZ)
 1610 FORMAT(//1X,'********* Saddle point:'//6X,
     *'Initial saddle point geometry in space-fixed cartesians ',
     *'(angstroms)',//15X,1HX,13X,1HY,13X,1HZ)                          0405JZ07
 1650 FORMAT (6X,3F14.9)
 1700 FORMAT(//1X,32(1H*),' Saddle point ',32(1H*)/)
 1800 format(1x,'Electronic degeneracies and energies (a.u.) = ',
     * i4,2x,f12.8,/,(47x,i4,2x,f12.8))
 1900 FORMAT(6X,9HFor IOP =,I2,5X,6HNDIM =,I3,5X,5HIND =,24I3,  
     *       (/,36X,5HIND =,24I3))                             
C
      return
      end SUBROUTINE gsetp
C
C***********************************************************************
C  INTPM
C***********************************************************************
C
      SUBROUTINE intpm (H, KL, NFUNC, FISEN)
      use common_inc
      use rate_const, only : fsv,inh,v3,ivar
      use energetics_mod
      use perconparam
C
C     Page-McIver gradient following algorithm
C
C     Iclude statements were added 6/18/91
C
C     CALLED BY:
C                INTEGR
C     CALLS:
C            RS,INTPM2,TRANS,DERIV2,DERV24,RSPDRV,SECCEN,FIRST
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      call mem_intpm(n3tm,1)
      IPROC=0
      KK = 0                                                            0101JZ13
      IF (KL .EQ. 0) THEN
         KK = 0                                                         5/6/90VM
C
C     First step is Euler step
C
         DO 10 I = 1,N3
            IF (LGS(31) .EQ. 35) THEN                                   7/1/91VM
               X(I) = X(I) - H*DX(I) + FISEN*(H*H*V3(I))/2.0D0          7/1/91VM
            ELSE                                                        7/1/91VM
               X(I) = X(I) - H*DX(I)
            ENDIF                                                       7/1/91VM
   10    CONTINUE
      ELSE
C
C     Diagonalize F
C
         CALL RSPDRV (N3TM,N3,FSV,XLAM,1,U0,SCR,SCR2,IERR)              0206WH93
         IF(IERR.NE.0) THEN
            WRITE(FU6,6000) IERR
            STOP 'INTPM 1'
         ENDIF
C
C     Construct U0(transpose)*DX and put into SCR,
C     and scale XLAM by DXMAG.
C
         DO 20 I = 1,N3
            XLAM(I) = XLAM(I)/DXMAG
            SCR(I) = 0.0D0
            DO 20 J = 1,N3
               SCR(I) = SCR(I) + U0(J,I)*DX(J)
   20    CONTINUE
C
C     Integrate expression that relates DELS to T
C
         DELT = 0.2D0*H
         T = 0.0D0
C
C     Loop over step size for integration (DELT)
C
         IC1 = 0
   30    CONTINUE
            IC1 = IC1 + 1
            TS = T
            DELT = 0.5D0*DELT
            T = 0.0D0
C
C     Get integrand at T=0
C
            CALL INTPM2 (N3, SCR, XLAM, T, FT)
            DELS = 0.0D0
C
C     Integration loop, integrate until DELS>H
C
            IC2 = 0
   40       CONTINUE
               IC2 = IC2 + 1
               IF (IC2 .GT. 2000) THEN
                  WRITE (FU6, 6003) IC2
                  STOP 'INTPM 2'
               ENDIF
               SLAST = DELS
               FTLAST = FT
               T = T + DELT
               CALL INTPM2 (N3, SCR, XLAM, T, FT)
               DELS = DELS + 0.5D0*DELT*(FT + FTLAST)
            IF (DELS .LT. H) GO TO 40
C
C     Integration complete, linearly interpolate T to DELS=H
C
            T = T - (H-DELS)*DELT/(SLAST-DELS)
C
C     Check for convergence of integral
C
         IF (DABS(1.0D0 - TS/T) .GT. 1.D-6) THEN
            IF (IC2 .GT. 1000 .OR. IC1 .GT. 10) THEN
               WRITE (FU6, 6002) T, TS
               IF (DABS(1.0D0 - TS/T) .GT. 1.D-3) STOP 'INTPM 3'
            ELSE
               GO TO 30
            ENDIF
         ENDIF
C
C     Construct alpha*g0'
C
         DO 50 I = 1,N3
            xx = DEXP(-XLAM(I)*T)
            SCR2(I) = (xx - 1.0D0)*SCR(I)/XLAM(I)
   50    CONTINUE
C
C     Take step and unscale XLAM
C
         DO 60 I = 1,N3
            XLAM(I) = DXMAG*XLAM(I)
            DO 60 J = 1,N3
               X(I) = X(I) + U0(I,J)*SCR2(J)
   60    CONTINUE
      ENDIF
C
C     RETURN AT THIS POINT IF ONLY THE NEXT POINT ON THE MEP IS DESIRED
C
      IF (LGS(36).NE.0) THEN                                             6/5S89
          S = S + H*FISEN                                                6/5S89
          KL = KL + 1
          IF (IVAR .EQ. 1) IVAR = 0                                     5/6/90VM
          RETURN                                                         6/5S89
       ENDIF                                                             6/5S89
C
C     Transform to Cartesian coordinates
C
      CALL TRANS(2,N3,AMASS,X,DX)                                       9/18YL92
C
C     Compute force constant matrix
C
      IF (KL .EQ. KK .OR. IVAR .EQ. 1) THEN                             5/6/90VM
         IF (IVAR .EQ. 1) THEN                                          011591VM
            IVAR = 0                                                    5/6/90VM
            KK = KL                                                     011591VM
            GO TO 55                                                    011591VM
         ENDIF                                                          011591VM
         IF (KL .EQ. 0 .AND. INH .NE. 1) THEN                           5/6/90VM
            KK = KK + INH - 1                                           5/6/90VM
            GO TO 65                                                    5/6/90VM
         ENDIF                                                          5/6/90VM
   55    KK = KK + INH                                                  5/6/90VM
   65    DO 70 I=1,N3                                                   5/6/90VM
            XXI(I)=X(I)
   70    CONTINUE
         call hhook(xxi,f,dlx,n3,0,0,iproc)                             0301YC97
         DO 110 J=1,N3
            X(J) = XXI(J)
            DO 110 I=1,J
               FSV(I,J)=F(I,J)                                          7/1/91VM
               FSV(J,I)=F(J,I)                                          7/1/91VM
  110    CONTINUE
C
      ENDIF                                                             5/6/90VM  
C
C     Compute gradient
C
      call ghook(0,iproc)                                               0301YC97
C
C     Return to mass-weighted coordinates
C
      CALL TRANS(1,N3,AMASS,X,DX)                                       9/18YL92
C
C     Find maximum derivative component
C
      DXMAX = 0.0D0
      DO 120 I = 1,N3
         T = DABS(DX(I))
         IF (T .GT. DXMAX) THEN
            DXMAX = T
         ENDIF
  120 CONTINUE
C
      IF (DXMAX .LE. 0.0D0) THEN
         WRITE (FU6, 6001) (DX(I),I=1,N3)
         STOP 'INTPM 4'
      ELSE
C
C        Determine the normalization factor
C
         DXNORM = 0.0D0
         DO 130 I = 1,N3
            DX(I) = DX(I)/DXMAX
            DXNORM = DXNORM + DX(I)*DX(I)
  130    CONTINUE
         DXNORM = SQRT(DXNORM)
C
C        Normalize the gradient vector
C
         DO 140 I = 1,N3
            DX(I) = DX(I)/DXNORM
  140    CONTINUE
         DXMAG = DXNORM*DXMAX
      ENDIF
      S = S + H*FISEN
      NFUNC = NFUNC + 1
      KL = KL + 1
      RETURN
C
 6000 FORMAT (/ 1X, 30(1H*), ' ERROR IN CALL TO RS, IERR=', I5)
 6001 FORMAT (/ 1X, 30(1H*), ' GRADIENT VECTOR IS ZERO, DX=',
     *   / (1X, 1P,8E15.7))
 6002 FORMAT (' IN INTPM, ITERATIONS OVER STEP SIZE TO FIND S(T) NOT',
     *   ' CONVERGED, T, TS=', 1P,2E15.7)
 6003 FORMAT (' INTEGRATION OVER T TO FIND S(T) NOT COMPLETED',
     *   ' AFTER', I5, ' STEPS')
C
      END
C
C***********************************************************************
C NEWT                             
C***********************************************************************
C
      SUBROUTINE newt(IOP)
C
C performs newton and quasi-newton (BFGS) geometry optimization         IR0495
C
C
      use common_inc
      use rate_const, only : fsv,inh
      use energetics_mod
      use perconparam , only : fu6,ckcal,natom,n3tm,eps
      use cm, only : convg,lbath,scale,stptol,iprxnt,convgt
      use kintcm, only : ihrec,ihrect,ibfgst
      use keyword_interface, only : iunit6,gufac6
      use dxiz, only : dlx2
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL linmin, FAIL                                              IR0495
C
      save                                                              0601YC98
C
      if(.not.allocated(hessin)) allocate(hessin(n3tm,n3tm))
      call mem_newton(n3tm,1)
      call mem_intpm(n3tm,1)
      IPROC=0
      NCOUNT = 0
      NRECAL = 0
      N = NDIM(IOP)
C--Next two lines commented here to avoid printing the header           021197LC
C  for V and X data when NOPRINT is selected                            021197LC
C
C     WRITE(FU6,1000)
C     WRITE(FU6,1010) (IND(I),I=1,N)
C

      iretry=1

C  Set flag for type of search performed: 
c
      cnvg  =  convg
      idorec = ihrec
      linmin=.true.
      IF (IOP .GT. 4) THEN
          ihunit = 0                        
          cnvg  =  convgt
          idorec = ihrect
          ibfgs = ibfgst
          linmin=.false.
      ENDIF
C
C  Set up the default convergent values for the energy and derivative.
C
      IF(CNVG .LE. EPS) THEN
         GTOL = 1.0D-7
      ELSE
         GTOL = CNVG
      ENDIF
      IF(NITER .LT. 0) GOTO 300
      H2 = 2.0D0*DLX2
C
C  Calculate starting energy and gradient.
C
      call ghook(0,iproc)                                               0301YC97
C
      GCOMP = ABS(DX(IND(1)))
      DO 5 I = 2,N 
         ABGC= ABS(DX(IND(I)))
         If (GCOMP.lt.ABGC)  GCOMP=ABGC
  5   CONTINUE    
c
c  Check if the geometry is already optimized
c
      IF(GCOMP.LE.GTOL) THEN  
        WRITE(FU6,*) '     Starting Geometry is a stationary point.'    
        RETURN                                                         
      ENDIF                                                           
C
C  Do initial inverse Hessian
C
      IF( ihunit .ne. 0 ) then
        do 10 j=1,n
           do 10 i=1,n
              hessin(i,j)=0.0D0
 10     continue
        do 20 i=1,n
 20        hessin(i,i)=1.0D0
      else
           call hhook(x,f,dlx2,n,0,1,iproc)                             0301YC97
          DO 40 I = 1,N
             DO 30 J = 1,I
                HESSIN(I,J) = F(I,J)*AMASS(ind(I))*AMASS(ind(J))
                HESSIN(J,I) = HESSIN(I,J)
 30          CONTINUE
 40       CONTINUE
          CALL MXLNEQ(HESSIN,N,N3TM,DETF,JRANK,EPS,WORK,0,N)            
      ENDIF
C
C  Main loop over iteration
C
      DO 200 ITS = 1,NITER
        DO 50 I = 1,N3
           DX0(I) = DX(I)
           X0(I) = X(I)
 50     CONTINUE
C
C  Calculate Newton-Raphson step
C
        DO 100 I = 1,N
           XXI(I) = 0.0D0
           DO 90 J = 1,N
              XXI(I) = XXI(I) - HESSIN(I,J)*DX0(IND(J))
 90        CONTINUE
 100    CONTINUE
        STEPC = ABS(XXI(1))
        MXIND = 1
        DO 105 I = 2,N
           ABXXI= ABS(XXI(I))
           If (STEPC.lt.ABXXI)  then 
             STEPC=ABXXI
             MXIND=I
           endif
 105    CONTINUE
C
C   ...then Scale the Newton-Raphson step
C
        stepmx = scale/stepc
        IF (linmin) then
           CALL LINMN(X0,XXI,N,FRET, stepmx)
           stepc=abs(xxi(mxind))
        else if (stepc .gt. scale) then
           do 115 i=1,n
 115          xxi(i)= xxi(i)*stepmx
        endif
C
C   Move to new geometry and calculate new energy and gradient
C
        DO 120 I = 1,N                                                  6/13B89
           X(IND(I)) = X0(IND(I)) + XXI(I) 
120     CONTINUE                                                        6/13B89
        call ghook(0,iproc)                                             0301YC97
C
        GCOMP = ABS(DX(IND(1)))
        DO 130 I = 2,N 
           ABGC= ABS(DX(IND(I)))
           If (GCOMP.lt.ABGC)  GCOMP=ABGC
130     CONTINUE      
C
C Test for convergence
C
        IF(GCOMP.LE.GTOL) THEN 
          WRITE(FU6,1300) ITS
          GOTO 300
        ENDIF
        IF (STEPC .LT. STPTOL) THEN
           FAIL=.TRUE.
           write(fu6,1350)
           if(iretry .eq. 0) GOTO 300
        ENDIF
        If(FAIL) then 
           write(fu6,'("Switching to full Newton")')
           idorec=1
           nrecal=idorec
           linmin=.false.
           FAIL=.False.
        ElSE
           NRECAL=NRECAL+1
        ENDIF
C
C  Write out the information at the ITS iteration.
C
c
C--Header for extra printing when PRINT is active                       021197LC
        IF (IPRXNT .NE. 0 .AND. ITS .LT. 2) THEN                        021197LC
         WRITE(FU6,1000)                                                021197LC
         WRITE(FU6,1010)                                                021197LC
        ENDIF
        If (iprxnt .ne. 0) then                                         021197LC
         DO 135  J = 1,N                                                021197LC
           IF (J .LT. 2) THEN                                           021197LC
              write(fu6,1020) ITS, V, IND(J), X(IND(J)), DX(IND(J))     021197LC
           ELSE
              write(fu6,1030) IND(J), X(IND(J)), DX(IND(J))             021197LC
           ENDIF
135      CONTINUE
C          write(fu6,'("Step = ",I4)') ITS                              021197LC
C          WRITE(FU6,1100) V,(X(IND(I)),I=1,N)                          021197LC
C          IF(LGS(1).EQ.2) WRITE(FU6,1200)(DX(IND(I)),I=1,N)            021197LC
C       endif                                                           021197LC
        endif                                                           021197LC
c
c  Calculate new inverse hessian
c
        if( NRECAL .eq. idorec ) then
          NRECAL=0
          If(iprxnt.ne.0) write(fu6,'("Hessian recalculated")')  
c get exact inverse hessian
           call hhook(x,f,dlx2,n,0,1,iproc)                             0301YC97
          DO 150 I = 1,N
             DO 140 J = 1,I
                HESSIN(I,J) = F(I,J)*AMASS(ind(I))*AMASS(ind(J))
                HESSIN(J,I) = HESSIN(I,J)
 140         CONTINUE
 150      CONTINUE
          CALL MXLNEQ(HESSIN,N,N3TM,DETF,JRANK,EPS,WORK,0,N)            
        ELSE
          IF(ibfgs .eq. 1) then
c
c   do BFGS update
c
             If(iprxnt.ne.0) write(fu6,
     *          '("Hessian updated using BFGS formula")')
             do 152 i=1,N
 152           deltag(i)= dx(ind(i)) - dx0(ind(i))
             dgxdot=0.0d0
             do 153 i=1,N
 153            dgxdot = dgxdot + xxi(i)*deltag(i)
             do 155 j=1,N
                do 155 i=1,N
c
c   ... build ( I - Dx(Dg)t/DOT(Dx,Dg) )
c
                   BFGS1(i,j)= -XXI(I)*deltag(j) / dgxdot
 155         continue
             do 156 i=1,N
 156            BFGS1(i,i)= 1.0d0 + BFGS1(i,i)
c
c   ... calculate (BFGS1)(HESSIN)(BFGS1)t
c
             call DGEMM('N','T',N, N, N, 1.0d0, HESSIN, N3TM,
     &                  BFGS1, N3TM, 0.0d0, BFGS2, N3TM)
             call DGEMM('N','N',N, N, N, 1.0d0, BFGS1, N3TM, 
     &                  BFGS2, N3TM, 0.0d0, HESSIN, N3TM)
c
c   ... and add Dx(Dx)t/DOT(Dx,Dg) to it.
c
             do 170 j=1,N
               do 170 i=1,N
                 HESSIN(i,j) = HESSIN(i,j) + xxi(i)*xxi(j)/dgxdot
 170         continue
          ELSE
            If (iprxnt .ne. 0) write(fu6,'("Hessian kept frozen")')
          ENDIF
        ENDIF
c End main loop
 200  CONTINUE
C
C Maximum number of iterations exceeded
C
      WRITE(FU6,1400)
      FAIL=.TRUE.
 300  CONTINUE
C
C Write out the final results
C
C     WRITE(FU6,1700)
      IF(IUNIT6.EQ.1) WRITE(FU6,1700)                                   0405JZ07
      IF(IUNIT6.EQ.0) WRITE(FU6,1702)                                   0405JZ07
      WRITE(FU6,1710)

      IF (IOP .NE. 5) THEN
         DO 350 J = 1,NRATOM(IOP)
            LSTR = 3 * IATOM(J) - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) IATOM(J),(X(L)/GUFAC6,L=LSTR,LEND)          0405JZ07
 350     CONTINUE
C
         WRITE(FU6,1712)                                                0405JZ07
C
         WRITE(FU6,1720)
         DO 360 J = 1,NRATOM(IOP)
            LSTR = 3 * IATOM(J) - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) IATOM(J),(DX(L),L=LSTR,LEND)
 360     CONTINUE
      ELSE
         DO 370 J = 1, NATOM
            LSTR = 3 * J - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) J,(X(L)/GUFAC6,L=LSTR,LEND)                 0405JZ07
370      CONTINUE

         IF (LBATH) THEN                                                0317YC99
            WRITE(FU6,1801) X(N3)                                       0317YC99
         ENDIF                                                          0317YC99
C
         WRITE(FU6,1712)                                                0405JZ07
         WRITE(FU6,1720)
         DO 380 J = 1, NATOM
            LSTR = 3 * J - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) J,(DX(L),L=LSTR,LEND)
380      CONTINUE
c
         IF (LBATH) THEN                                                0317YC99
            WRITE(FU6,1801) DX(N3)                                      0317YC99
         ENDIF                                                          0317YC99
C
      ENDIF
C
      WRITE(FU6,1600) V,V*CKCAL                                         1216WH92
      IF(FAIL) STOP 'NEWT 1'
      RETURN
C
 1000 FORMAT(/1X,'Energy (a.u.), geometry (unscaled a.u.) and ',        021197LC
     * 'gradient at each',/,1x,'optimization iteration')                021197LC
C1010 FORMAT(1X,'      V         ',' X(i),i=',3I16,/,(25X,3I16))        0610WH94
 1010 FORMAT(/1X,'Step',10X,'V',8X,'IND',9X,'X',14X,'DX',/,1X,68('-'))  021197LC
 1020 FORMAT(1X,I3,2X,1P,E16.8,I3,3X,2(E16.8))                          021197LC
 1030 FORMAT(22X,I3,3X,2(E16.8))                                        021197LC
 1100 FORMAT (/1X,1P,E16.8,8X,3E16.6,/,(25X,3E16.6))                    0610WH94
 1200 FORMAT ( 18X,'DX(i)',1P,/,(25X,3E16.6))                           0610WH94
 1300 FORMAT(/,' Search has converged after ',I4,' iterations')
 1350 FORMAT('Max. displacement component change smaller than STPTOL.')
 1400 FORMAT(/,' MAXIMUM NUMBER OF ITERATIONS EXCEEDED',/)
 1600 FORMAT (/,'  V = ',1PE16.8,' hartrees  (',0P,F16.8,' kcal/mol)')  1216WH92
C1700 FORMAT (/1X,'Final geometry and derivatives in unscaled',         1201WH92
C    * ' cartesians (a.u.)')
 1700 FORMAT (/1X,'Final geometry in unscaled Cartesians (bohrs)')      0405JZ07
 1702 FORMAT (/1X,'Final geometry in unscaled Cartesians (angstroms)')  0405JZ07
 1710 FORMAT (/1X,4HAtom,11X,'X',15X,'Y',15X,'Z',/)
 1712 FORMAT (/1X,'Final derivatives in unscaled Cartesians (a.u.)')    0405JZ07
 1720 FORMAT (/1X,4HAtom,11X,'DX',14X,'DY',14X,'DZ',/)
 1800 FORMAT (1X,I3,4X,1P,3E16.6)
 1801 FORMAT (1X,'EFF SOLVENT',12X,E16.6)                               0317YC99
C
      END
C
C***********************************************************************
C  NORMOD
C***********************************************************************
C
      SUBROUTINE normod (IOP,STEPX,FISEN)
  
C
C     COMPUTES NORMAL MODE FREQUENCIES AND DIRECTIONS
C     AT MASS-WEIGHTED POINT X(N3TM)
C     FOR IOP=1, ONLY NORMALIZED GRAD(V) COMPUTED
C     FOR IOP=2, ALSO GET NORMAL MODES
C     FOR IOP=3, PROJECT IS CALLED  ????????
C     FOR IOP=-1 TO -4, GET NORMAL MODES FOR REACTANTS OR PRODUCTS
C     FOR IOP.GT.0, IND(I)=I
C
C     IOP = 7,8 (IOP = 2,3 for TS)                                      0725YC97
C
C     Include statements were added 6/20/91
C
C     CALLED BY:
C                POLYAT,MAIN,PATH
C     CALLS:
C            TRANS,DERIV2,FIRST,DERV24,FDIAG,CORTRM,GRADDR,ANCOEF,
C            ANHARM,ZEROPT,NOROUT,CHKFRE
C
      use common_inc
      use rate_const
      use energetics_mod
      use perconparam; use kintcm; use cm
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL ISSAD                                                     0317YC99
C
      imem = 1
      call mem_normod(n3tm,imem)
      IPROC=0
C
      IF ((IOP.LT.5).OR.(ISTATU(5).LE.2.AND.IOP.GE.7).OR.(INOSAD.EQ.1   0729YC97
     *    .AND.IOP.GE.7)) THEN                                          0725YC97
c
c     fix for sun workatations
c
      IF (IOP.GE.7)  THEN                                               0311YC98
         JOP = IOP - 5                                                  0311YC98
      ELSE                                                              0311YC98
         JOP = IOP                                                      0311YC98
      ENDIF                                                             0311YC98
C
      LPTBCR = LGS2(15)
      KOP = ABS(JOP)
      NEND = N3                                    
      EGRNDT = 0.D0
      H2 = 2.0D0*DLX 
C
C     SET FLAG FOR DESIRED METHOD OF DETERMINING 2ND DERIV ALONG RXN PATH
C
      IF (JOP.GT.0) THEN
         NDERIV = 0
         IF (LGS(25).EQ.2) NDERIV = 1
      ENDIF
      IF (JOP.LT.3 .OR. (LGS(31) .NE. 5 .AND. LGS(31) .NE. 35)) THEN    7/1/91VM
C
C     FOR A POINT ALONG THE REACTION COORDINATE (JOP=3) THE HESSIAN AND 5/23B88
C     GRADIENT ARE ALREADY COMPUTED IF THE PAGE-MCIVER INTEGRATION      5/23B88
C     PROCEDURE IS USED.                                                5/23B88
C
C     TRANSFORM POINT TO CARTESIAN COORDINATES
C
         IF (JOP.LT.0) THEN
            NEND = NDIM(KOP)
         ELSE
            DO 5 I = 1, N3TM                                            1106YL92
               XMSCD(I) = X(I)                                              ..
5           CONTINUE                                                    1106YL92
            CALL TRANS (2,N3,AMASS,X,DX)                                9/18YL92
         ENDIF
C
C     NOW COMPUTE THE SECOND DERIVATIVE FOR ALL OPTIONS EXCEPT JOP = 1
C     AND CONVERT TO MASS-WEIGHTED COORDS
C
         IF (JOP.NE.1) THEN 
           DO 10 I = 1, N3
               EFNDTP(I) = 0.0D0
   10      CONTINUE                  

       call hhook(X,F,DLX,NEND,0,1,0)                                   0202BL05 
C
          DO 63 I = 1, NEND
             DO 63 J = 1, NEND
                FSP(J,I) = F(J,I)
63        CONTINUE
C
C FOR MORATE                                                            6/13T89
C PROJECT OUT THE TRANSLATIONAL AND ROTATIONAL CONTAIMINATIONS          6/13T89
C FOR JOP = 3, IT IS DONE EXPLICITLY IN ROUTINE PROJCT                  6/13T89
C                                                                       6/13T89
C            IF (potnam.eq.'mopac'.OR. LGS2(2) .NE. 0) THEN
c            IF (isup.eq.1.or.lgs2(2).ne.0) then                        0327YC97
c
c only nosupermol can project out RT of reactants and products
c and well and ts can be projected or not
c
      IF ((isup.eq.1.and.iop.lt.0.and.iproj(abs(iop)).ne.0).or.
     +    (iop.gt.0.and.iproj(5).ne.0).or.
     +    (iop.lt.-4.and.iproj(abs(iop)).ne.0)) THEN 
               IF (JOP .NE. 3) THEN                                     6/13T89
                  DO 62 I = 1,NEND                                      6/13T89
                    DO 62 J = 1,NEND                                    6/13T89
                       SUM = 0.0D0                                      6/13T89
                       DO 61 K = 1,NEND                                 6/13T89
                          SUM = SUM + F(I,K)*PROJ(K,J)
  61                   CONTINUE                                         6/13T89
                       TEMPX(I,J) = SUM 
  62              CONTINUE                                              6/13T89
                  DO 68 I = 1,NEND                                      6/13T89
                     DO 68 J = 1,NEND                                   6/13T89
                        SUM = 0.0D0                                     6/13T89
                        DO 66 K = 1,NEND                                6/13T89
                           SUM = SUM + PROJ(I,K)*TEMPX(K,J) 
  66                    CONTINUE                                        6/13T89
                        F(I,J) = SUM                                    6/13T89
  68              CONTINUE                                              6/13T89
               ENDIF                                                    6/13T89
            ENDIF 


          ENDIF
C
         IF (LGS(30).LT.0) THEN                                         9/6YL91
C                                                                          ..
C Put F into temp. storage for writing out later                           ..
C                                                                          ..
            DO 100 I = 1, NEND                                             ..
               DO 100 J = 1, NEND                                          ..
                  FSV(I,J) = F(I,J)                                        ..
  100       CONTINUE                                                       ..
         ENDIF                                                          9/6YL91
C
         IF (JOP.GE.0) THEN
C
C     COMPUTE NORMALIZED GRAD(V) AND
C     RESTORE X TO MASS-WEIGHTED COORDS
C
             CALL TRANS(1,N3,AMASS,X,DX)
            DO 65 I = 1, N3TM                                           1106YL92
               X(I) = XMSCD(I)                                          1106YL92
65          CONTINUE                                                    1106YL92
            DXMAX = 0.0D0
            DO 70 I = 1, N3
C
C     DXXP WILL STORE THE UNNORMALIZED DERIVATIVES SO THEY CAN BE PRINTED
C     OUT LATER WITH THE OPTIONAL OUTPUT.
C
               DXXP(I) = DX(I)
               T = ABS(DX(I))
               IF (T.GT.DXMAX) THEN
                  DXMAX = T
               ENDIF
   70       CONTINUE
            IF (DXMAX.EQ.0.0D0) THEN
               WRITE (FU6,1000) (DX(I),I=1,N3)
            ELSE
               DXNORM = 0.0D0
               DO 80 I = 1, N3
                  DX(I) = DX(I)/DXMAX
                  DXNORM = DXNORM + DX(I)*DX(I)
   80          CONTINUE
               DXNORM = SQRT(DXNORM)
               DO 90 I = 1, N3
                  DX(I) = DX(I)/DXNORM
   90          CONTINUE
               DXMAG = DXNORM*DXMAX
            ENDIF
         ENDIF
C
C THE UNNORMALIZED DERIVATIVES IS STORED AT DXP                         1110DL89
C FOR THE PAGE-MCIVER INTEGRATION                                       1110DL89
C
      ELSEIF (LGS(31).EQ.5.OR.LGS(31).EQ.35) THEN                       9/6YL91
        DO 110 I = 1, N3                                                1110DL89
        DXXP(I) = DX(I)*DXMAG
  110   CONTINUE                                                        1110DL89
      ENDIF                                                             1110DL89
      ELSE                                                              0725YC97
        DO I = 1,N3                                                     0725YC97
         DO J = 1,N3                                                    0725YC97
           F(I,J)=TEMHES(I,J)/(AMASS(IND(I))*AMASS(IND(J)))             0725YC97
           FSP(I,J) = F(I,J)                                            0725YC97
           FSV(I,J) = F(I,J)                                            0725YC97
         ENDDO                                                          0725YC97
        ENDDO                                                           0725YC97
      ENDIF                                                             0725YC97
C
C OPTIONally diagonalize F
C
C  to reset the iop flag in case of status = 4
c
      IF (IOP.GE.7)  THEN                                               0311YC98
         JOP = IOP - 5                                                  0311YC98
      ELSE                                                              0311YC98
         JOP = IOP                                                      0311YC98
      ENDIF                                                             0311YC98
c
      IF (JOP.NE.1) THEN
C       scale frequencies and/or call some routines for RODS
C
        CALL MON(IOP,JOP,ISSAD)                                         0213BL05
C
c
         IF (iclf.eq.1) 
     >      CALL REPFL (IOP,NEND,S,BARRS,WSTAR,REDM,EPRD,EWP,EWR,FLSR,  0707YC98
     >                 FLSP,FRELOW,FREQ,IFQLOW,IREPR,LGS)               0707YC98
c
         IF((IRODS.EQ.1.OR.IVRP.EQ.1).AND.JOP.EQ.3) THEN                1114PF97
            CALL ENDRODS (JOP)                                          0219PF98
         ENDIF                                                          1114PF97
C
         IF (LPTBCR.GT.0) THEN                                          1106YL92
            CALL CALC_CORIOLIS(IOP,JOP,NEND)
         ENDIF                                                          1106YL92
C
C     OPTIONALLY COMPUTE ANHARMONICITIES OR DUNHAM COEFFICIENTS
C
         IF (LGS(5).GT.0.OR.LGS(33).EQ.1) CALL ANHARM (JOP)
C
C     COMPUTE ADIABATIC POTENTIAL
C
         IF (LPTBCR.GT.0) THEN
ccc            V = VSAV
            DO IX = 1, N3TM
              DX(IX) = DXSAV(IX)
            ENDDO
         ENDIF
         CALL ZEROPT (IOP)                                              9/18YL92
C
C     OPTIONAL OUTPUT
C
C     PRINT OUT THE BOND DISTANCE, BOND ANGLE INFORMATION FOR ALL THE SAVE
C     GRID POINTS
C
         IF (IOP.GE.0.AND.IPRCD.EQ.1)                                   0601YC98
     *          CALL PRCORD(FU28,S,V,X,NATOM,NPRCA,IPRCA,AMASS,2)       0601YC98
C
         IF ((MOD(LSAVE,NPRSMD).EQ.0).or.
     *       (MOD(LSAVE,IPRDIS).EQ.0)) CALL NOROUT (JOP,DXXP)           0626YC97
      ENDIF
      RETURN
C
 1000 FORMAT (' DXNORM=0,  DX='/(1X,1P,10D13.5))
C 
      END SUBROUTINE normod
C
C**********************************************************************
C  CALC_CORIOLIS
C**********************************************************************
C
      SUBROUTINE CALC_CORIOLIS(IOP,JOP,NEND)
      use common_inc
      use energetics_mod
      use perconparam , only : n3tm,ckcal,autocm,fu6
      use kintcm, only : idcpt
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
            N3M7 = NF(5)                                                1106YL92
ccc            IF (JOP.LT.0) N3M7 = NF(KOP)                                1106YL92
            ISHFT1 = NEND - N3M7 + 1                                    1106YL92
            CALL CHKFRE(N3M7,FREQ(ISHFT1),IFDMY)                        0601YC98
            NMOD = N3M7 - IFDMY                                         0601YC98
            VSAV = V                                                    0601YC98
            DO IX = 1,N3TM                                              0601YC98
               DXSAV(IX) = DX(IX)                                       0601YC98
            ENDDO                                                       0601YC98
C
            IF (NMOD.GT.0) THEN                                         1106YL92
               IF (LPTBCR.EQ.2.OR.LPTBCR.EQ.12)                         1106YL92
     *            CALL CORTRM(NMOD,NEND,N3,COF,PVEC,ZETAPT)             0206WH93
c
c       The geometry is in mass-scaled coordinates (except for the
c       reactants and products.)
c
c               IF (IOP.GT.0)  CALL TRANS (2,N3,AMASS,X,DX)             0601YC98
               IF (IOP.LT.0)  CALL TRANS (1,N3,AMASS,X,DX)
               CALL GRADDR(NMOD,N3,NEND,LGS2(15),REDM,DLX,X,AMASS,      0601YC98
     *                      FREQ,COF,FIJK,FIIJJ,S)                      0601YC98
               IF (IOP.LT.0)  CALL TRANS (2,N3,AMASS,X,DX)
c               IF (IOP.GT.0)  CALL TRANS (1,N3,AMASS,X,DX)             0601YC98
               IF (LPTBCR.EQ.13) THEN                                   0601YC98
                 IF (IDCPT.EQ.1) THEN                                   0601YC98
                    CALL DCPT(NMOD,NEND,LPTBCR,FU6,FREQ,                0601YC98
     *                       FIJK,FIIJJ,EGRNDT,EFNDTP,ENUT,S,ANCO,SUME) 0601YC98
                 ELSE IF (IDCPT.EQ.10) THEN                             0601YC98
                    CALL NDCPT(NMOD,NEND,LPTBCR,FU6,FREQ,               0601YC98
     *                       FIJK,FIIJJ,EGRNDT,EFNDTP,ENUT,ANCO,SUME,   0601YC98
     *                       S,ISEL)                                    0601YC98
                 ELSE IF (IDCPT.EQ.20) THEN                             0601YC98
                    CALL PTWO(NMOD,NEND,LPTBCR,FU6,FREQ,                0601YC98
     *                       FIJK,FIIJJ,EGRNDT,EFNDTP,ENUT,ANCO,SUME)   0601YC98
                 ELSE IF (IDCPT.EQ.30) THEN                             0601YC98
                    CALL DPTWO(NMOD,NEND,LPTBCR,FU6,FREQ,               0601YC98
     *                       FIJK,FIIJJ,EGRNDT,EFNDTP,ENUT,ANCO,SUME)   0601YC98
                 ENDIF                                                  0601YC98
               ELSE                                                     0601YC98
                    CALL ANCOEF(NMOD,NEND,LPTBCR,FU6,FREQ,BEROT,        0601YC98
     *                     ZETAPT,FIJK,FIIJJ,EGRNDT,EFNDTP,ENUT,ANCO)   0601YC98
               ENDIF                                                    0601YC98
            ENDIF                                                       1106YL92
            SUMALL = 0.0d0                                              0601YC98
            DO IG = 1,9                                                 0601YC98
              SUMALL=SUMALL+SUME(IG)                                    0601YC98
            ENDDO                                                       0601YC98
            WRITE (50,9997) S,V*CKCAL,SUMALL*CKCAL,(V+SUMALL)*CKCAL,    0601YC98
     *                      EGRNDT*CKCAL                                0601YC98
9997  FORMAT(F9.4,4F15.6)                                               0601YC98
            WRITE (92,9996)S,6*FIJK(5,5,5)*AUTOCM,6*FIJK(4,4,4)*AUTOCM, 0601YC98
     *                     6*FIJK(3,3,3)*AUTOCM,6*FIJK(2,2,2)*AUTOCM,   0601YC98
     *                     6*FIJK(1,1,1)*AUTOCM                         0601YC98
            WRITE (94,9996)S,24*FIIJJ(5,5)*AUTOCM,24*FIIJJ(4,4)*AUTOCM, 0601YC98
     *                     24*FIIJJ(3,3)*AUTOCM,24*FIIJJ(2,2)*AUTOCM,   0601YC98
     *                     24*FIIJJ(1,1)*AUTOCM                         0601YC98
9996  FORMAT(F9.4,5F15.6)                                               0601YC98
            WRITE (56,9998) S, SUME(3)*AUTOCM,SUME(4)*AUTOCM            0601YC98
     *             ,SUME(6)*AUTOCM,SUME(9)*AUTOCM
            WRITE (58,9998) S, SUME(1)*AUTOCM,SUME(2)*AUTOCM
     *             ,SUME(7)*AUTOCM,SUME(8)*AUTOCM
            WRITE (60,9999) S,ENUT*AUTOCM,ANCO(5,5)*AUTOCM,
     *                ANCO(4,4)*AUTOCM,ANCO(3,3)*AUTOCM,
     *                ANCO(2,2)*AUTOCM,ANCO(1,1)*AUTOCM
            WRITE (62,9999) S,ANCO(5,4)*AUTOCM,
     *                ANCO(5,3)*AUTOCM,ANCO(5,2)*AUTOCM,
     *                ANCO(5,1)*AUTOCM,ANCO(4,3)*AUTOCM,
     *                ANCO(4,2)*AUTOCM
            WRITE (64,9998) S,ANCO(4,1)*AUTOCM,
     *                ANCO(3,2)*AUTOCM,ANCO(3,1)*AUTOCM,
     *                ANCO(2,1)*AUTOCM
            AAAA = SUME(2)+SUME(3)+SUME(7)+SUME(9)
            WRITE (48,9999) S,(SUME(2)+SUME(3))*AUTOCM,
     *      (SUME(7)+SUME(9))*AUTOCM,
     *       AAAA*AUTOCM,(AAAA+SUME(8))*AUTOCM,
     *      (AAAA+SUME(8)+SUME(6))*AUTOCM,
     *      (AAAA+SUME(8)+SUME(6)+SUME(4))*AUTOCM
9998  FORMAT(F9.4,4F15.6)
9999  FORMAT(F9.4,6F15.6)                                               0601YC98
      RETURN
      END
C
C*******************************************************************************
C  MON
C*******************************************************************************
C
      SUBROUTINE MON(IOP,JOP,ISSAD) 

      use common_inc
      use potmod, only : ifqfac
      use rate_const, only : freqfac,lgs3
      use perconparam , only : n3tm
      use kintcm, only : ivrp,irods
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL ISSAD
C
         IF((IRODS.EQ.1.OR.IVRP.EQ.1).AND.JOP.EQ.3) THEN                1114PF97
            CALL DORODS(JOP,0)                                          0219PF98
         ENDIF                                                          1114PF97
c          CALL ICFDIAG IF INTERNAL COORDINATES ARE BEING USED          07/95KAN
c        AND IF THE SAVE POINT IS AN ORDINARY ONE                       07/95KAN
c                                                                       07/95KAN
         IF((LGS2(39).NE.0).AND.((JOP.EQ.3).OR.(IOP.GE.7)))THEN         0317YC99
            if (iop.ge.7) then                                          0317YC99
              ISSAD  = .TRUE.                                           0317YC99
            else                                                        0317YC99
              ISSAD  = .FALSE.                                          0317YC99
            endif                                                       0317YC99
            CALL ICFDIAG (JOP,ISSAD)                                    0317YC99
c
c      Use redundant internal coordinates for reactants & products      0110PJ01
c      only when it is not explicitly declared not to be used or        0214PJ01  
c      when scaling force constants are used (curv2, curv3)             0111PJ01
c
         ELSE IF ( (lgs2(39) .ge. 3)                                    0211PJ01
     *            .and. (jop .lt. 0)                                    0111PJ01
     *            .and. (lgs3(2) .ne. 0 .or. lgs3(1) .ne. 0)) then      0214PJ01
C --- To test the effect of R.P internal to the saddle point            0214PJ01
C    *            .and. (ifcfac .eq. 1)) then                           0111PJ01 
            issad = .false.                                             0111PJ01
            CALL ICFDRP (jop, issad)                                    0111PJ01
         ELSE                                                           07/95KAN
            CALL FDIAG (JOP)                                            07/95KAN
         ENDIF                                                          07/95KAN
c
c --- scale all frequnecies using FRQSCLPJ                              0815PJ01
c
         IF  (ifqfac .eq. 1) THEN                                       0815PJ01
             do i = 1, n3tm                                             0815PJ01  
                 freq(i) = freq(i) * freqfac                            0815PJ01
             end do                                                     0815PJ01
         ENDIF                                                          0815PJ01
c
      RETURN
      END
C
C***********************************************************************
C  POLYAT
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/20/91
C    RESTRUCTURING DONE 10/25/91 and 6/7/94
C    FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 
C
      SUBROUTINE polyat (VADX,IOP)
      use common_inc
      use rate_const, only : freqfac
      use perconparam , only : n3tm,cmtoau
      use kintcm
      use keyword_interface, only : potnam
      use cm, only : steng,temwer
C
C     SETS UP DATA FOR POLYATOMIC REACTANT FOR ROUTINE REACT
C
C     CALLED BY:
C                REACT
C     CALLS:
C            CENTER,NORMOD,RPHWRT,RPHSET
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save                                                              0601YC98
C
      IPROC=0                                                           0227BL05
C
C   Optimize the geometry for this chemical species
C        
C   IF REACTANTS AND READ IN EZER0 THEN NO NEED TO EVALUATE THE 
C   ENERGIES.
C
      IF (ISTATU(IOP).LE.2) THEN                                        0725YC97
         IF ((IOP.GT.2.OR.LGS2(4).NE.2).AND.(LGS(30).LE.0)) THEN
            IF (ISTATU(IOP).EQ.0) THEN                                  0725YC97
                   CALL OHOOK(IOP,iproc)                                0301YC97
            ELSE                                                        0409YC97
                   CALL GSETP(IOP)
                   CALL EHOOK(0,iproc)                                  0409YC97
            ENDIF                                                       0409YC97
         ELSE
            IF (INITG(IOP).EQ.0) THEN                                   0514PF97
                CALL GSETP(IOP)
            ELSE
              IF (ISTATU(IOP).EQ.0) THEN                                0714YC97
                   CALL OHOOK(IOP,iproc)
              ELSE
                   CALL GSETP(IOP)
              ENDIF
            ENDIF 
         ENDIF                                                          0409YC97
c         if (lgs(35).eq.0) then                        
         if (isup.eq.0) then                                            0327YC97
             NDIM(IOP) = 3*NRATOM(IOP)                                  1107GL92
             L = 0                                                      1107GL92
             NREND = NRATOM(IOP)                                        1107GL92
             DO 30 I = 1, NREND                                         1107GL92
                   JND = 3*(IATOM(I)-1)                                 1107GL92
             DO 30 J = 1, 3                                             1107GL92
                   L = L+1                                              1107GL92
                   IND(L) = JND+J                                       1107GL92
30           CONTINUE                                                   1107GL92
         ENDIF                                                          1107GL92
      ELSE                                                              0725YC97
        V = STENG(IOP)                                                  0725YC97
      ENDIF                                                             0725YC97
      if (potnam.ne.'aces') then                                        0327YC97
         IF (ICODE(IOP) .GE.2) CALL CENTER (IOP,1)                      1125JC97
      else                                                              0327YC97
         IF (ICODE(IOP) .GE. 3) CALL CENTER (IOP,1)                     1125JC97
      ENDIF                                                             0327YC97
C
C     OBTAIN FREQUENCIES AND ANHARMONICITIES FOR REACTANTS OR PRODUCTS
C     REDEFINE IND ARRAY FOR NORMOD
C
      IF (ISTATU(IOP).NE.6) THEN                                        0725YC97
      IF (LGS(30).LE.0) THEN
          IF (JXFREQ(IOP).NE.0) CALL NORMOD (-IOP,STEPX,FISEN)          0317YC99
         IF (LGS(30).LT.0) CALL RPHWRT (IOP)
      ELSE    
         if (lgs(30).eq.1) then                                         073096PF
           CALL RPHSET (IOP)
         elseif (lgs(30).eq.2) then                                     073096PF
           call rph40(IOP)                                              073096PF
         elseif (lgs(30).eq.3) then                                     0810JC97
           call rph31(IOP)                                              0810JC97
         endif                                                          073096PF
      ENDIF
      ELSE                                                              0725YC97
        NCUB = 3*NRATOM(IOP)                                            0725YC97
        DO I = 1, N3TM                                                  0725YC97
          FREQ(I) = 0.0d0                                               0725YC97
        ENDDO                                                           0725YC97
        DO I = 1,NF(IOP)                                                0725YC97
          IF (IFREU(IOP).EQ.0) THEN                                     0807YC97
                 FREQ(NCUB-I+1) = TEMWER(IOP,I)                         0109BE07
          ELSE                                                          0807YC97
                 FREQ(NCUB-I+1) = TEMWER(IOP,I)*CMTOAU                  0109BE07
          ENDIF                                                         0807YC97
          IF (ISCALERP.EQ.1) THEN                                       0109BE07
            FREQ(NCUB-I+1) = FREQ(NCUB-I+1)*FREQFAC                     0109BE07
          ENDIF                                                         0109BE07
        ENDDO                                                           0725YC97
        CALL ZEROPT (-IOP)                                              0725YC97
      ENDIF                                                             0725YC97
      VADX = VADX+VAD
C
C**********************************************************************
C
C     IF LGS2(4) is equal to 1 then the zero of energy is 
C     calculated from the energy of the reactants in the 
C     subprogram REACT.  Upon return, if one of the reactants is
C     a polyatomic then REACT assumes that the energy in V is the 
C     energy of that species (at its equilibrium geometry). 
C     To ensure that this is true the potential is called 
C     before returning to REACT so that V has the correct value.
C
C     Note: EZER0 is not yet determined and should not be 
C           subtracted from this energy. 
C       7/29/GL91
C
C**********************************************************************
C
C     IF (LGS2(4) .EQ. 1 .AND. IOP .LT. 3) then
C     IF (LGS2(4) .EQ. 1) then
C       ezer0 = 0.0d0                                                   6/2RS94
C       call energ(0)                                                   6/2RS94
C     end if
C
C**********************************************************************
C
      RETURN
C
 1200 FORMAT(/1X,'For IOP =',I2,5X,'NDIM = ',I3,5X,'IND = ',5I4,        0622WH94
     *       /,(32X,'IND = ',5I4))                                      0622WH94
C
      END subroutine polyat
C
C***********************************************************************
C  REACT
C***********************************************************************
C
C   FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C
      SUBROUTINE react (IOP)
      use common_inc
      use perconparam
      use rate_const
      use kintcm
      use keyword_interface
      use cm
C
C     COMPUTES GEOMETRY AND PROPS OF THE IOP-TH SPECIE WITH NEWT
C     IOP=1 OR 2 FOR REACTANTS, IOP=3 OR 4 FOR PRODUCTS, IOP=7 FOR WELLR, 
C     AND IOP=8 FOR WELLP
C
C
C     CALLED BY:
C               DOREPR 
C     CALLS:
C            RPHSET,RPHWRT,DIATOM,POLYAT,RPHINT,ENROUT,FIRST,ENERG
C
C*
C*    DIMENSION OF WER,XER, ETC. CHANGED TO 2*N3TM    S/7/21/1987
C*
C    The interface between this subprogram and electronic structure
C    packages, i.e. LGS(35)=1, has been modified 11/07/GL92.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*3 AFLAG
C
      CHARACTER*1 IMA(N3TM)
      DIMENSION WOUT(N3TM)
      save                                                              0601YC98

      IPROC=0                                                           0227BL05
C
C     READ IN STARTING GEOMETRY FOR REACTANTS OR PRODUCTS HAS BEEN MOVED
C     TO READ5 ROUTINE
C
      IWWHR = 0
      IFWKB = 0
      IF (IOP.EQ.1.OR.IOP.EQ.3.OR.IOP.EQ.7.OR.IOP.EQ.8) THEN            0801PF97
         VADX = 0.0D0
         IF (IOP.NE.7.AND.IOP.NE.8) THEN 
            EPRD = 0.0D0
         end if

c                                                                       
c move from the "if (initg(iop) .eq. 0)" block                          0222PJ02
c                                                                       0222PJ02
            DO 10 I = 1,N3                                              0222PJ02 
               X(I) = XR(I,IOP)                                         0222PJ02
10          CONTINUE                                                    0222PJ02
c

          IF (INITG(IOP).EQ.0) THEN                                     0514PF97

c                                                                       
c move out of the "if" block, comment here                              0222PJ02
c                                                                       0222PJ02
c            DO 10 I = 1,N3                                             0222PJ02
c               X(I) = XR(I,IOP)                                        0222PJ02
c10          CONTINUE                                                   0222PJ02
c

            IF (IOP.EQ.1) THEN
               WRITE (FU6,1100)
               IF (IUNIT6.EQ.1) WRITE (FU6,1150)                        0405JZ07
               IF (IUNIT6.EQ.0) WRITE (FU6,1160)                        0405JZ07
            ELSEIF (IOP.EQ.3) THEN                                      0801PF97
               WRITE (FU6,1200)
               IF (IUNIT6.EQ.1) WRITE (FU6,1250)                        0405JZ07
               IF (IUNIT6.EQ.0) WRITE (FU6,1260)                        0405JZ07
            ELSEIF (IOP.EQ.7) THEN                                      0801PF97
               WRITE (FU6,4100)                                         0801PF97
               WRITE (FU6,4150)                                         0801PF97
C              WRITE (FU6,4175)                                         0801PF97
               IF (IUNIT6.EQ.1) WRITE (FU6,4175)                        0405JZ07
               IF (IUNIT6.EQ.0) WRITE (FU6,4176)                        0405JZ07
               IWWHR = 1                                                0801PF97
            ELSEIF (IOP.EQ.8) THEN                                      0801PF97
               IF (IWWHR.EQ.0) WRITE (FU6,4100)                         0801PF97
               WRITE (FU6,4250)                                         0801PF97
c              WRITE (FU6,4275)                                         0801PF97
               IF (IUNIT6.EQ.1) WRITE (FU6,4275)                        0405JZ07
               IF (IUNIT6.EQ.0) WRITE (FU6,4276)                        0405JZ07
            ENDIF
C           WRITE (FU6,1300) (J+1,(X(3*J+I),I=1,3),J=0,NATOM-1)         0610WH94
            WRITE (FU6,1300) (J+1,(X(3*J+I)/GUFAC6,I=1,3),J=0,NATOM-1)  0405JZ07
         ENDIF
      ENDIF
C
C
C ---------------------------------------------------------------------
C     THIS NEXT SECTION OF REACT EXAMINES ONE REACTANT OR PRODUCT
C ---------------------------------------------------------------------
C
C     FIRST WRITE OUT SPECIES HEADER
C
      IF (IOP.LE.2) THEN
         WRITE (FU6,1350) IOP
      ELSE IF (IOP.LE.4) THEN                                           0911JC97
         JIOP = IOP-2
         WRITE (FU6,1400) JIOP
      ELSE IF (IOP.EQ.7) THEN                                           0911JC97
         WRITE (FU6,1410)                                               0911JC97
      ELSE                                                              0911JC97
         WRITE (FU6,1420)                                               0911JC97
      ENDIF
      NRX = NRATOM(IOP)
C
C  COPY INDEX LIST OF ATOMS FOR CURRENT USE
C
      DO 20 I = 1,NRX
         IATOM(I) = IATSV(I,IOP)
20    CONTINUE
      IF (ICODE(IOP) .LT. 0) THEN
         WRITE (FU6,1710) IOP,NRATOM(IOP),(IATOM(I),I=1,NRATOM(IOP))
      ELSEIF (ICODE(IOP).EQ.1) THEN
         WRITE (FU6,1600) IOP,IATOM(1)
      ELSEIF (ICODE(IOP).EQ.2) THEN
         WRITE (FU6,1650) IOP,IATOM(1),IATOM(2)
      ELSEIF (ICODE(IOP).GT.2) THEN
         WRITE (FU6,1700) IOP,NRATOM(IOP),(IATOM(I),I=1,NRATOM(IOP))
      ENDIF
C
C     WRITE ELECTRONIC DEGENERACIES AND ENERGIES (IN A.U.)
C                MAXIMUM OF THREE ELECTRONIC STATES
C
      JEND = 3*IOP
      JBEG = JEND-2
      WRITE (FU6,1800) (NEDEG(I),ELEC(I),I=JBEG,JEND)
C
C     INPUT CHOICE OF ANHARMONICITY FOR THE VARIOUS VIBRATIONAL MODES
C        THIS INFORMATION IS SAVED IN MODER FOR LATER USE
C
      NFREQ = NF(IOP)
      ISHFT = 3*NRATOM(IOP) - NFREQ
      IF (ICODE(IOP) .NE. 1) THEN
         DO 30 I = 1,NFREQ
            MODE(I) = MODER(IOP,I)
 30      CONTINUE
      ENDIF
C     IF (ICODE(IOP).LT.0) NFREQ = NFREQ - 1
      IF (ICODE(IOP).NE.1) WRITE (FU6,1950) (MODE(I),I=NFREQ,1,-1)
C*
C
      IF (ICODE(IOP).EQ.1) THEN
C
C                                                     ATOMIC SPECIES
C
         FMOM(IOP) = 0.0D0                                              0112WH92
C
C  check if ezer0 is read in, then don't need to evaluate the energies
c  of reactants
c
         IF (ISTATU(IOP).LE.2) THEN                                     0725YC97
C        IF (IOP.GT.2.OR.LGS2(4).NE.2) THEN
         IF ((IOP.GT.2.AND.IOP.LE.4).OR.LGS2(4).NE.2) THEN
C
C  NORMAL CASE FOR ATOMS, JUST GET THE ENERGY 
C  FOR MORATE, GEOMETRY IS OBTAINED WITH OPTIMIZATION
C
            IF (INITG(IOP).EQ.0) THEN                                   0514PF97
                CALL EHOOK(0,iproc)                                     0301YC97
            ELSE
                CALL OHOOK(IOP,iproc)                                         0301YC97
C             WRITE(FU6,4000) V*CKCAL                                   0504WH94
            ENDIF
         ELSE
            IF (INITG(IOP).EQ.0) THEN                                   0514PF97
               CALL GSETP(IOP)
            ELSE
C               IF (IOPT(IOP).EQ.1) THEN
               IF (ISTATU(IOP).EQ.0) THEN                               0714YC97
C
                  CALL OHOOK(IOP,iproc)
               ELSE
                  CALL GSETP(IOP)
               ENDIF
            ENDIF
         ENDIF
         ELSE                                                           0725YC97
            V = STENG(IOP)                                              0725YC97
         ENDIF                                                          0725YC97
         IF (LGS(30).EQ.1) THEN                                         073096PF
             CALL RPHSET(IOP)
         elseif (lgs(30).eq.2) then                                     073096PF
             call rph40(IOP)                                            073096PF
         elseif (lgs(30).eq.3) then                                     0810JC97
             call rph31(IOP)                                            0810JC97
         endif                                                          073096PF
         IF (LGS(30).LT.0) CALL RPHWRT (IOP)
      ELSEIF (ICODE(IOP).EQ.2) THEN
C
C                                                   DIATOMIC SPECIES
C
         IF (INITG(IOP).EQ.0) THEN                                      0514PF97
            CALL DIATOM (VADX,GSEX,IOP)
         ELSE
            CALL POLYAT (VADX,IOP)
         ENDIF
         IF (LGS(30).EQ.1) THEN                                         073096PF
            CALL RPHSET(IOP)
         elseif (lgs(30).eq.2) then                                     073096PF
            call rph40(IOP)                                             073096PF
         elseif (lgs(30).eq.3) then                                     0810JC97
            call rph31(IOP)                                             0810JC97
         endif                                                          073096PF
         IF (LGS(30).LT.0) CALL RPHWRT (IOP)
      ELSE
C
C                                                 POLYATOMIC SPECIES
C
         CALL POLYAT (VADX,IOP)
      ENDIF
C
C     Calculate zero of energy (it is assumed at this point in the calcu
C     that LGS(6) > 0).  If LGS(35) = 0; then the zero of energy is equa
C     the potential energy of reactant 1 for a unimolecular reaction and
C     potential energy of reactant 2 for a bimolecular reaction.
C     If LGS(35) = 1, the zero of energy is equal to the sum of the pote
C     energies of the reactants.
C
      EGRNDR(IOP) = EGRNDT
      IF (ICODE(IOP).NE.1) THEN
C
C     STORE FREQS AND ANHARMS IN PROPER SPOTS
C
         NEND = NF(IOP)
         L = 1
c  array wer saves the frequencies of reactants and products
c  frequencies
         IF (IOP.LT.5.AND.IOP.NE.1) THEN                                0202YC98
            DO 40 II = 1, IOP-1
               L = L+NF(II)
   40       CONTINUE
         ENDIF
c
c set up index for wew array for wells                                  0202YC98
c
         IF (IOP.EQ.7) THEN                                             0202YC98
            L = 1                                                       0202YC98
         ELSEIF (IOP.EQ.8) THEN                                         0202YC98
            L = 1 + NF(7)                                               0202YC98
         ENDIF                                                          0202YC98
      ENDIF
      AFLAG = '   '
      IF (LGS(5).GE.21) AFLAG = 'SET'
      DO 50 II = 1, NEND
         EFNDTR(L) = EFNDTP(II)
         IF (AFLAG.EQ.'SET') LGS(5) = MODE(II)
c
c store the harmonic, anharmonic constants for wells
c
         IF (IOP.LT.5) THEN                                             0202YC98
           WER(L) = FREQ(II+ISHFT)                                      0202YC98
           IF (LGS(5).EQ.9) THEN
             FMIHR(L) = FMOMHR(NF(IOP)-II+1)
           ENDIF
           XER(L) = ANHRM(II)
         ELSE                                                           0202YC98
           WEW(L) = FREQ(II+ISHFT)                                      0202YC98
           IF (LGS(5).EQ.9) THEN
             FMIHW(L) = FMOMHR(NF(IOP)-II+1)
           ENDIF
           XEW(L) = ANHRM(II)
         ENDIF                                                          0202YC98
C
C   FOR WKB OPTION, STORE ZEROPOINT EIGENVALUES IN WGSEX FOR USE BY VPAR
C
         if (iop.le.4) then                                             9/25BCG00
C         IF (LGS(33).EQ.1) THEN                                        9/25BCG00
            IF (ICODE(IOP).EQ.2 .AND. INITG(IOP).EQ.0) THEN             0514PF97
C            IF (ICODE(IOP).EQ.2 .AND. LGS(35).EQ.0) THEN
               WGSEX(L) = GSEX
            ELSE
               WGSEX(L) = GSE(II)
            ENDIF
         ENDIF
c         IF (LGS(5).EQ.7.OR.LGS(5).EQ.8) Y00R(L) = AB(II)
         IF (LGS(5).EQ.7.OR.LGS(5).EQ.8) THEN                           0601YC98
           IF (IOP.LT.5) THEN
               Y00R(L) = AB(II)
           ELSE
               Y0W(L) = AB(II)
           ENDIF
         ENDIF
         L = L+1
   50 CONTINUE

      IF (AFLAG.EQ.'SET') LGS(5) = NARR + 20
C
C     WRITE OUT FREQUENCIES AND ANHARMONICITIES
C
      IF (ICODE(IOP).NE.1) THEN
C        IF(ICODE(IOP).EQ.2.AND.LGS(35).EQ.0)
        IF(ICODE(IOP).EQ.2.AND.INITG(IOP).EQ.0)                         0514PF97
     *      WRITE(FU6,2000)INTOUT(1),GSEX
         NSTOP = 3*NRATOM(IOP)
         DO 60 K = 1,NSTOP
            IF (FREQ(K) .GE. 0.0D0) THEN
               WOUT(K) = FREQ(K)
               IMA(K) = ' '
            ELSE
               WOUT(K) = -FREQ(K)
               IMA(K) = 'i'
            ENDIF
   60    CONTINUE
         WRITE (FU6,2050)
         WRITE(FU6,2100) (NSTOP+1-I,WOUT(I),IMA(I),WOUT(I)*CEV,
     *  IMA(I),WOUT(I)*AUTOCM,IMA(I),WOUT(I)*CKCAL,IMA(I),I=NSTOP,1,-1)
         WRITE (FU6,2150)
C
C  Print out scaled frequencies
C
         IF (IFRFAC.NE.0) THEN                                          0808JC00
            WRITE (FU6, 2025)                                           0808JC00
            WRITE(FU6,2100) (NSTOP+1-I,WOUT(I)*FREQFAC,IMA(I),          0808JC00
     *      WOUT(I)*CEV*FREQFAC,                                        0808JC00
     *      IMA(I),WOUT(I)*AUTOCM*FREQFAC,IMA(I),WOUT(I)*CKCAL*FREQFAC, 0808JC00
     *      IMA(I),I=NSTOP,1,-1)                                        0808JC00
            WRITE (FU6,2150)                                            0808JC00
         ENDIF                                                          0808JC00
C
         IF (LGS(5).EQ.1.OR.LGS(5).EQ.2)
     *              WRITE (FU6,2200) (Y00(I),I=NEND,1,-1)
C
C   Write out information for the state selected rate calculation.
C
         IF(LGS(23).NE.0 .AND. ICODE(IOP) .EQ. 2) THEN
             WRITE(FU6,3500)
             IF(L9(IOP,1) .EQ. -1) WRITE(FU6,3600)
             IF(L9(IOP,1) .EQ. 0)  WRITE(FU6,3700)
             IF(L9(IOP,1) .EQ. 1)  WRITE(FU6,3800)
         ENDIF
      ENDIF
C
C     SAVE AND WRITE OUT REACTION ENDOERGICITY AND ADIABATIC LIMITS
C     REWRITE FOR THE VERSION 7.3                                  0402YC97
C
C     FOR ONE REACTANT LGS(6)=3,4
C
      IF (LGS(6).GE.3.AND.IOP.EQ.1.AND.LGS2(4).NE.2) THEN
           EZER0 = V
      ENDIF
C
C     FOR TWO REACTANTS LGS(6)=1,2
C
      IF (LGS(6).LE.2.AND.IOP.LE.2.AND.LGS2(4).NE.2) THEN
         IF (ISUP.EQ.1.OR.IPOT.EQ.1) THEN
               EZER0 = EZER0 + V
         ELSE 
               EZER0 = V
         ENDIF
      ENDIF
C
      IF (IOP.EQ.3) V = V - EZER0
C
C     FOR ONE PRODUCT LGS(6)=2,4
C
      IF (IOP.EQ.3) THEN                                                1201JC97
          EPRD = V
      ENDIF
C
C     FOR TWO PRODUCTS LGS(6)=1,3
C
      IF ((LGS(6).EQ.1.OR.LGS(6).EQ.3).AND.IOP.EQ.4) THEN
          IF (ISUP.EQ.1.OR.IPOT.EQ.1) THEN
                EPRD = EPRD + V
          ELSE
                EPRD = V - EZER0
          ENDIF
      ENDIF
C
c      WRITE (6,*) 'in react',' V = ',V, ' EZER0 = ',
c    &        EZER0, ' EPRD = ',EPRD, ' of IOP',IOP
c
c  add in the contribution of the bath mode in zero point energy        0317YC99
c 
         IF (ibathm.eq.1) then                                          0317YC99
            VSOL = 0.5d0*(PI/(4.0d0*frict))                             0317YC99
         ENDIF                                                          0317YC99
      IF (IOP.EQ.4.OR.(IOP.EQ.3.AND.(LGS(6).EQ.2.OR.LGS(6).EQ.4))) THEN
c
c  add in the contribution of the bath mode in zero point energy        0317YC99
c
         IF (ibathm.eq.1) then                                          0317YC99
            write (6,*) 'Contribution from solvent coordinate ',        0317YC99
     >        ' to the product ZPE ',VSOL*CKCAL,' kcal/mol.'            0317YC99
            VADX = VADX + VSOL                                          0317YC99
         ENDIF                                                          0317YC99
         VAP = VADX + EPRD
         CALL ENROUT(VAR,VAP,EPRD,IFRFAC,FREQFAC)
      ELSEIF (IOP.EQ.2.OR.(IOP.EQ.1.AND.LGS(6).GT.2)) THEN
c
c  add in the contribution of the bath mode in zero point energy        0317YC99
c
         IF (ibathm.eq.1) then                                          0317YC99
            write (6,*) 'Contribution from solvent coordinate ',        0317YC99
     >        ' to the reactant ZPE ',VSOL*CKCAL,' kcal/mol.'           0317YC99
            VADX = VADX + VSOL                                          0317YC99
         ENDIF                                                          0317YC99
         VAR = VADX
      ENDIF
c
c update the x array to xr                                              0202YC98
c
      DO I = 1, N3TM                                                    0202YC98
           XR(I,IOP) = X(I)                                             0202YC98
      ENDDO                                                             0202YC98
C
C     REACTANTS WELL                                                    0911JC97
C                                                                       0911JC97
      IF (IOP.EQ.7) THEN                                                0911JC97
         V = V - EZER0                                                  0911JC97
         EWR = EPRD                                                     1201JC97
c
c  add in the contribution of the bath mode in zero point energy
c
         IF (ibathm.eq.1) then                                          0317YC99
            write (6,*) 'Contribution of the solvent coordinate',       0317YC99
     >          ' to the wellr ZPE ',VSOL*CKCAL,' kcal/mol'             0317YC99
            VADX = VADX + VSOL                                          0317YC99
         ENDIF                                                          0317YC99
         VADP = VADX + V                                                1201JC97
         CALL WRTWEL(V,EWR,VADP,VAR,VAP,1,IFRFAC,FREQFAC)               0808JC00
         EWR = V                                                        0202YC98
      ENDIF                                                             0911JC97
C
C     PRODUCTS  WELL                                                    0911JC97
C                                                                       0911JC97
      IF (IOP.EQ.8) THEN                                                0911JC97
         V = V - EZER0                                                  0911JC97
         EWP = EPRD                                                     1201JC97
c
c  add in the contribution of the bath mode in zero point energy        0317YC99
c
         IF (ibathm.eq.1) then                                          0317YC99
            write (6,*) 'Contribution of the solvent coordinate',       0317YC99
     >          ' to the wellp ZPE ',VSOL*CKCAL,' kcal/mol'             0317YC99 
            VADX = VADX + VSOL                                          0317YC99
         ENDIF                                                          0317YC99
         VADP = VADX + V                                                1201JC97
         CALL WRTWEL(V,EWP,VADP,VAR,VAP,2,IFRFAC,FREQFAC)               0808JC00
         EWP = V                                                        0202YC98
      ENDIF                                                             0911JC97
C
      RETURN
C
 1100 FORMAT(//1X,10(1H*),' Reactants:'/)
 1150 FORMAT(1X,'Initial guess for geometry of reactants in ',
     *'space-fixed cartesians (bohrs):',
     *//17X,1HX,15X,1HY,15X,1HZ)                                        0610WH94
 1160 FORMAT(1X,'Initial guess for geometry of reactants in ',
     *'space-fixed cartesians (angstroms):',
     *//17X,1HX,15X,1HY,15X,1HZ)                                        0405JZ07
 1200 FORMAT(//1X,10(1H*),' Products:'/)
 1250 FORMAT(1X,'Initial guess for geometry of products in ',
     *'space-fixed cartesians (bohrs):',
     *//17X,1HX,15X,1HY,15X,1HZ)                                        0610WH94
 1260 FORMAT(1X,'Initial guess for geometry of products in ',
     *'space-fixed cartesians (angstroms):',                            
     *//17X,1HX,15X,1HY,15X,1HZ)                                        0405JZ07
 1300 FORMAT(1X,I3,4X,1P,3E16.6)                                        0610WH94
 1350 FORMAT(//1X,33(1H*),' Reactant ',I1,1X,33(1H*)/)                  0211YC97
 1400 FORMAT(//1X,33(1H*),' Product ',I1,1X,34(1H*)/)                   0211YC97
 1410 FORMAT(//1X,31(1H*),' Reactants well ',31(1H*)/)                  0911JC97
 1420 FORMAT(//1X,31(1H*),' Products well ',32(1H*)/)                   0911JC97
 1600 FORMAT(6X,'IOP = ',I2,':',/6X,'Species is atom #',I2)
 1650 FORMAT(6X,'IOP = ',I2,':'/6X
     *,'Species is a diatom involving atoms',I3,' and',I3)
 1700 FORMAT(6X,'For IOP = ',I2,':'/6X
     1,'Species is a ',I2,'-atom polyatomic made up of atoms: ',10I3,
     2        : / (56X,10I3))
 1710 FORMAT(6X,'For IOP = ',I2,':'/6X,'Species is a '
     1,I2,'-atom(s) in field and made up of atoms: ',10I3,
     2 : / (61X,10I3))
 1800 FORMAT(1X,'Electronic degeneracies and energies (a.u.) =',        0610WH94
     * I4,2X,F12.8,/,(46X,I4,2X,F12.8))
 1950 FORMAT(/1X,'Anharmonicity for mode(i) is: ',10I3/(31X,10I3))      0610WH94
 2000 FORMAT(1X, 13HMode method =,I3,5X,21Hground-state energy =,
     * 1PE14.6,' hartrees')
 2025 FORMAT(//1X,78(1H-)/,24X,' Scaled Frequencies ',/1X,78(1H-),      0808JC00
     */16X,'a.u.',12X,'eV',10X,'cm**-1',9X,'kcal',/)                    0808JC00
 2050 FORMAT(//1X,78(1H-)/,24X,'Harmonic Frequencies',/1X,78(1H-),      0824YC98
     */16X,'a.u.',12X,'eV',10X,'cm**-1',9X,'kcal',/)
 2100 FORMAT(2X,'Mode ',I3,4X,F10.7,1X,A1,4X,F6.4,1X,A1,4X,F9.2,        0610WH94
     *1X,A1,4X,F8.4,1X,A1)
 2150 FORMAT(1X,78(1H-))
 2200 FORMAT(6X,17HThird derivatives,3(/6X,8F15.10))
 3500 FORMAT(/,'       For state-selected rate calculation',/)
 3600 FORMAT(10X,'   vibrational mode is restricted to be in the'
     *,' ground-state.')
 3700 FORMAT(10X,'   vibrational mode is thermalized.')
 3800 FORMAT(10X,'   vibrational mode is excited to its first',
     *' excited state.')
 4000 FORMAT(/15X,'HEAT OF FORMATION = ',F14.5,' KCAL/MOL')
 4100 FORMAT(//1X,10(1H*),' Wells:'/)                                   0801PF97
 4150 FORMAT(//1X,33(1H*),' WELLR ',I1,1X,36(1H*)/)                     0801PF97
 4175 FORMAT(1X,'Initial guess for geometry of wellr in ',              0801PF97
     *'space-fixed cartesians (bohrs):',
     *//17X,1HX,15X,1HY,15X,1HZ)
 4176 FORMAT(1X,'Initial guess for geometry of wellr in ',              0405JZ07
     *'space-fixed cartesians (angstroms):',
     *//17X,1HX,15X,1HY,15X,1HZ)
 4250 FORMAT(//1X,33(1H*),' WELLP ',I1,1X,36(1H*)/)                     0801PF97
 4275 FORMAT(1X,'Initial guess for geometry of wellp in ',              0801PF97
     *'space-fixed cartesians (bohrs):',
     *//17X,1HX,15X,1HY,15X,1HZ)
 4276 FORMAT(1X,'Initial guess for geometry of wellp in ',              0405JZ07
     *'space-fixed cartesians (angstroms):',
     *//17X,1HX,15X,1HY,15X,1HZ)
C
      END SUBROUTINE react
c
C***********************************************************************
C  SADDLE
C***********************************************************************
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
C    FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 
C
      SUBROUTINE saddle
      use common_inc; use perconparam
      use rate_const
      use kintcm
      use keyword_interface
      use cm
C
C     COMPUTES SADDLE POINT GEOMETRY WITH SUBROUTINE NEWT
C     DEFINING X1=X2=X3=X6=X8=X9 = 0 RECOMMENDED
C
C     CALLED BY:
C                DOSAGE
C     CALLS:
C            GHOOK
C
C   The include file esp.inc has been removed in version 5.0         1021GL92
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IPROC=0
 
      IF (LGS(34) .LT. 0) ICODE(5) = -4
C
C     FIND SADDLE POINT
C
      IF (ISTATU(5).EQ.0) THEN                                          0725YC97
        IF (LGS(30).LE.0) THEN                                          0210JC97
                IF (ISTATU(5).EQ.0) THEN                                0210JC97
C
                                  CALL OHOOK(5,iproc)                   0227BL05
                ELSE                                                    0210JC97
                                  CALL GSETP(5)                         0210JC97
                ENDIF                                                   0210JC97
        ENDIF                                                           0210JC97
      V = V + EZER0                                                     0623RS95
      ELSE                                                              0725YC97
        CALL GSETP(5)                                                   0725YC97
        V = STENG(5)                                                    0725YC97
        V = V - EZER0                                                   0725YC97
      ENDIF                                                             0725YC97
C
C     FOR LATER CALCS, SET IND(I)=I
C
      DO 60 I = 1, N3
         IND(I) = I
   60 CONTINUE
      NDIM(5) = N3
      DO 70 I = 1, NATOM
         IATOM(I) = I
   70 CONTINUE
C
      return
      end subroutine saddle
c ***************************************************************************
c  sadeng
c ***************************************************************************
c
      subroutine sadeng
      use common_inc; use perconparam
      use rate_const, only : freqfac
      use kintcm
      use keyword_interface, only : gufac6,iunit6
      use cm
c
C     This routine was removed from MAIN in version 5.0.  It computes
c     the saddle point energetics including the normal modes.
c
C     CALLS:
C          read5,option,setlgs,setvar 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      call sad_mem
c
c     *****   Calculate saddle point or input starting geometry (lgs(1)=0)
c
      if (lgs(34) .ne. 0) then
          nf(5) = n3 - 1
      else if (icode(5) .eq. 3) then
          nf(5) = n3 - 6
      else
          nf(5) = n3 - 7
      endif
c
      if (lgs(1).eq.0) then
          ndim(5) = n3
          do 90 i = 1, natom
             iatom(i) = i
   90     continue
          do 100 i = 1, n3
             ind(i) = i
             x(i) = xr(i,5)
  100     continue
          if (LBATH) ind(n3+1) = n3+1                                   0317YC99
c
c     lgs(1)=0 and istatu(5)=0 (i.e., NOSADDLE/NOOPT) is not allowed    0210JC97
c     in the current version. Therefore, this line will not be true.    0210JC97
c
          if (lgs(30).le.0) call ghook(0,iproc)                               0327YC97
c
c         write (fu6,1600)
          if(iunit6.eq.1) write (fu6,1600)                              0405JZ07
          if(iunit6.eq.0) write (fu6,1610)                              0405JZ07
          write (fu6,1650) (x(i)/gufac6,i=1,n3)                         0405JZ07
          write (fu6,1700)
          write (fu6,1750) (nedeg(i),elec(i),i=13,15)
      else
          call saddle
          s = 0.0d0
      endif
c
c     *****    put center of mass at origin and convert to mass-weighted x
c              also get inertia moment or their product using subroutine center
C
         if (lgs(30).le.0 .and. lgs(34) .eq. 0) call center (5,1)       1125JC97
c         call trans (1,n3,amass,x,dx)
c
c     store the coordinates                                             0317YC99
c
         IF (LBATH.AND.S.EQ.0) THEN                                     0317YC99
           X(N3) = 0.0d0                                                0317YC99
           DO I = 1,N3                                                  0317YC99
             XSSAV(I) = X(I)                                            0317YC99 
           ENDDO
           LFOPT = .TRUE.                                               0317YC99
 1992      format (3F15.8)                                              0317YC99
           write (6,*) ' XSSAV ='                                       ..
           write (6,1992) (XSSAV(I),I=1,N3)                             ..
           F1  = PI*PI*REDM/(16.0d0*FRICT)                              ..
           write (6,*) ' AMASS ,REDM',REDM                              ..
           write (6,1992) (AMASS(I),I=1,N3)                             ..
           F11 = F1/FRICT                                               ..
           write (6,*) ' F = ',F11                                      ..
           IF (ircoup.EQ.0) THEN                                        ..
             DO I = 1,N3-1                                              ..
               DIFFU(I)=SQRT(BK*BATEMP/(DIFFU(I)*F1)*EXP(-PI*PI/32))    ..
             ENDDO                                                      ..
             write (6,*) ' Coupling constants = FC'                     ..
             write (6,1992) (F11*DIFFU(I),I=1,N3)                       ..
           ELSEIF (ircoup.EQ.1) THEN                                    .. 
             write (6,*) ' Coupling constants = FC'                     ..
             write (6,1992) (DIFFU(I),I=1,N3)                           ..
             DO I = 1,N3-1                                              ..
               DIFFU(I) = DIFFU(I)/F11                                  ..
             ENDDO                                                      ..
           ENDIF                                                        ..
          ENDIF                                                         0317YC99
c
         call trans (1,n3,amass,x,dx)
c
c     *****   Compute frequencies and normal modes at saddle point
c
c
         call dosafr
C
C           WRITE OUT SADDLE POINT ENERGETICS
C
         call wrthok(5)
c
         CALL SENOUT(EPRD,V,VAR,VAP,VAD,IFRFAC,FREQFAC)                 0808JC00
c
      return
c
 1600 format(1X,'****** Initial starting point geometry (bohrs)',//,        JC97
     *15x,1hx,13x,1hy,13x,1hz)                                              JC97
 1610 format(1X,'****** Initial starting point geometry (angstroms)',//,
     *15x,1hx,13x,1hy,13x,1hz)
 1650 format (6x,3F14.9)                                                    JC97
 1700 FORMAT(//1X,31(1H*),' Starting point ',31(1H*)/)                      JC97
 1750 format(//1X,'Electronic degeneracies and energies (a.u.) = ',
     *i4,2x,f12.8,/,(47x,i4,2x,f12.8))
C 1800 format (/,6X,'FOR GTS GEOMETRIES, ICODE=', I2)
C 1900 format(6X,'ANHARMONICITY CHOSEN FOR MODE I: ',(30I3))
C 2000 format(////1X,78(1H-),/53X,' SADDLE POINT ENERGETICS',
C     */1X,78(1H-))
C 2100 format(70X,'Hartrees',11X,'eV',12X,'cm-1',12X,'kcal')
C 2200 format(9X,'CLASSICAL ENERGY w/re CLASSICAL REACTANTS',11X,
C     * 2F16.4,F16.3,F15.4)
C 2300 format(9X,'CLASSICAL ENERGY w/re CLASSICAL PRODUCTS',12X,
C     * 2F16.4,F16.3,F15.4)
C 2400 format(9X,'ZERO POINT ENERGY w/re CLASSICAL REACTANTS',10X,
C     * 2F16.4,F16.3,F15.4)
C 2500 format(9X,'ZERO POINT ENERGY w/re CLASSICAL PRODUCTS',11X,
C     * 2F16.4,F16.3,F15.4)
C 2600 format(9X,'ZERO POINT ENERGY w/re ZERO POINT REACTANTS',9X,
C     * 2F16.4,F16.3,F15.4)
C 2700 format(9X,'ZERO POINT ENERGY w/re ZERO POINT PRODUCTS',10X,
C     * 2F16.4,F16.3,F15.4)
C 2800 format(9X,'ZERO POINT ENERGY w/re CLASSICAL SADDLE POINT',7X,
C     * 2F16.4,F16.3,F15.4/1X,132(1H-))
c
      end subroutine sadeng                                       
c
c***********************************************************************
c  SECCEN
c***********************************************************************
c
      subroutine seccen (xii,fmat,mw,sstp)
      use common_inc; use perconparam
c
c     Calculate second derivatives by method of central differences
c     Added by R. Steckler 6/2/94
c
c     This routine calculates the hessian matrix using numerical 
c     differentiation of the first derivatives.  It has the options
c     of using an initial geometry in cartesian (mw=0) or 
c     mass-weighted coordinates (mw=1).  The first derivatives are
c     obtained using the routine FIRST.
c     The geometry is specified by xii and the resulting hessian is
c     returned in fmat.
c
c     Called by:
C               cubst, normod, intpm 
C     Calls:
C                first
C
      implicit double precision (a-h,o-z)
C
      real(8) ::  xii(n3tm),fmat(n3tm,n3tm),dx0(n3tm)
c
c     save original gradient
c
      do 4 i=1,n3                                                        IR0495
 4       dx0(i) = dx(i)                                                  IR0495
c
c     Optionally transform to cartesian coordinates
      if (mw.eq.1) call trans(2,n3,amass,xii,dx)
c
c     Calculation of the force constant matrix using central differences
c         (second derivatives of the energy with respect to x) 
c
      h2=2.0d0*sstp
      do 1 i=1,n3 
         x(i)=xii(i)-sstp
         call ghook(0,iproc)                                                  0301YC97
c
         do 2 j=1,i 
             fmat(i,j)=dx(j)
   2     continue
c
         x(i)=xii(i)+sstp
         call ghook(0,iproc)                                                  0301YC97
C
          do 3 j=1,i
            fmat(i,j)=(dx(j)-fmat(i,j))/(h2*amass(i)*amass(j))
            fmat(j,i) = fmat(i,j)
   3      continue
          x(i)=xii(i)
   1  continue
c
c     Optionally restore X to mass-weighted coordinates
      if (mw.eq.1) call trans(1,n3,amass,xii,dx)
c
      do 5 i=1,n3                                                        IR0495
 5       dx(i) = dx0(i)                                                  IR0495
      return
      end subroutine seccen
C
C***********************************************************************
C  SECCEP
C***********************************************************************
C
      subroutine seccep (mw,sstp,nend)
      use common_inc; use perconparam
c
c     Calculate second derivatives by method of central differences
c     Added by R. Steckler 6/2/94
c
c     This routine calculates the hessian matrix using numerical 
c     differentiation of the first derivatives.  It is identical to SECCEN
c     except it computes only part of the matrix.  This is needed when
c     a geometry optimization is done using a limited number of degrees
c     of freedom.
c
c     The subroutine has the options of using an initial geometry in 
c     cartesian (mw=0) or mass-weighted coordinates (mw=1).  The first 
c     derivatives are obtained using the routine FIRST.
c
c     Called by:
c               normod
c     Calls:
c                first
c
      implicit double precision (a-h,o-z)
c
      dimension xxi(n3tm), dx0(n3tm)                                     IR0495
c
c
c
c     Save initial geometry and gradient so it can be restored at the end
c
      do 10 i = 1, n3
         xxi(i) = x(i)
         dx0(i) = dx(i)                                                  IR0495
   10 continue
c
c     Optionally transform to cartesian coordinates
      if (mw.eq.1) call trans(2,n3,amass,x,dx)
c
c     Calculation of the force constant matrix using central differences
c         (second derivatives of the energy with respect to x) 
c
      h2=2.0d0*sstp
      do 1 i=1,nend 
         x(ind(i))=xxi(ind(i))-sstp
         call ghook(0,iproc)                                                  0301YC97
c
         do 2 j=1,i 
             f(i,j)=dx(ind(j))
   2     continue
c
         x(ind(i))=xxi(ind(i))+sstp
         call ghook(0,iproc)
C
          do 3 j=1,i
            f(i,j)=(dx(ind(j))-f(i,j))/(h2*amass(ind(i))
     *         *amass(ind(j)))
            f(j,i) = f(i,j)
   3      continue
          x(ind(i))=xxi(ind(i))
   1  continue
      do 4 i=1,n3                                                        IR0495
 4       dx(i)=dx0(i)                                                    IR0495
c
c     Optionally restore X to mass-weighted coordinates
      if (mw.eq.1) call trans(1,n3,amass,xxi,dx)
c
      return
      end subroutine seccep

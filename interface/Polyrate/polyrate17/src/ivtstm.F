C *****************************************************************************
       subroutine armuef
       use common_inc
       use perconparam
       use ivtst1
       use rate_const
C *****************************************************************************
C
C  Calculate the argument of the exponential in the mueff ecuation for the
C  points along the grid given in the fu30 or fu40 units. This argument is
C  stored in the matrix armcsc.
C
C  Called by rphint
C  
C  Calls rphmef
C
       implicit double precision (a-h,o-z)
       dimension frq(n3tm),bcrv(nvibm),tmparm(nsdim)
       save
C
C  Only goes through this subroutine the first time it is called
C
       do i=1,nsdim
          armcsc(i)=0.D0
       enddo
       n3m7=nf(5)
C
C  We will calculate the curvature components for the saddle point by
C  linear interpolation, but we will only use them if necessary
C
            t = -ss(issp-1)/(ss(issp+1)-ss(issp-1))
            do i = 1, nvibm
               bfs(issp,i) = (1.0d0-t)*abs(bfs(issp-1,i)) +
     *                       t*abs(bfs(issp+1,i))
            enddo
C
C  First we will start from the saddle point (ISSP) going downhill to 
C  reactants and then to products, using also the first point in the 
C  opposite direction.
C
       iendr=3
       iendp=nss-2
       lcount=0
       do 10 i=issp+1,iendr+1,-1
C
C  If we have information about more than one point at this side of the
C  reaction path, we will not use the saddle point information (since 
C  the curvature is an interpolated value, not calculated)
C
          if (i.eq.issp) then
             ilim=issp-iendr+1
             if (ilim.ge.3) goto 10
          endif
          st=ss(i)
          do j=1,nvibm
              bcrv(j)=bfs(i,j)
              frq(j)=ws(i,j)
          enddo
          lcount=lcount+1
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,xad)
          tmparm(lcount)=xarg
          if (lcount.eq.3) tmparm(2)=xad
10     continue
          st=ss(iendr)
          do j=1,nvibm
              bcrv(j)=bfs(iendr,j)
              frq(j)=ws(iendr,j)
          enddo
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,dum)
          tmparm(lcount+1)=xarg
          call rphmef(n3,1,lcount,frq,bcrv,redm,st,xarg,dum)
          tmparm(lcount+2)=xarg
C
C  Now reorder the information so that it matches the right order
C
          j=0
          do i=iendr,issp
              armcsc(i)=tmparm(lcount+2-j)   
              j=j+1
          enddo
          prfst=tmparm(2)                                               0120JC98
C
C  Now calculate the product side
C
       lcount=0
       do 20 i=issp-1,iendp-1
          if (i.eq.issp) then
             ilim=issp-iendp+1
             if (ilim.ge.3) goto 20
          endif
          st=ss(i)
          do j=1,nvibm
              bcrv(j)=bfs(i,j)
              frq(j)=ws(i,j)
          enddo
          lcount=lcount+1
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,xad)
          tmparm(lcount)=xarg
          if (lcount.eq.3) tmparm(2)=xad
20     continue
C
          st=ss(iendp)
          do j=1,nvibm
              bcrv(j)=bfs(iendp,j)
              frq(j)=ws(iendp,j)
          enddo
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,xad)
          tmparm(lcount+1)=xarg
          call rphmef(n3,1,lcount,frq,bcrv,redm,st,xarg,xad)
          tmparm(lcount+2)=xarg
C
          j=0
          do i=iendp,issp,-1
                armcsc(i)=tmparm(lcount+2-j)
                j=j+1
          enddo
          rcfst=tmparm(2)                                               0120JC98
C
          armcsc(issp-1)=rcfst                                          0120JC98
          armcsc(issp+1)=prfst                                          0120JC98
       lcount=0
       return
       end
C *****************************************************************************
      SUBROUTINE calcs (xx1,xx2,sval,kkk)
      use common_inc
      use perconparam
C *****************************************************************************
C
C     this routine computes the distance between two geometries along the
C     DCP following the algorithm proposed by Chen 
C     Theor. Chim. Acta 1989,75,481
C
C     on input:
C        -xx1 and xx2 are the reference and the final geometries, respectively
C       
C     on output
C        -sval is the distance between xx1 and the rotated xx2
C
C     CALLED BY:
C                RPHRD2,POLYAT,MAIN
C     CALLS:
C            MXLNEQ
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION XX1(n3tm),xx2(n3tm),xscr(n3tm)
      dimension a(3,3),b(3),y(3),scr(3,4)
      dimension rota(3,3)
      dimension kkk(natoms)
      data tol /1.d-9/
      data b2a /0.529177d0/
C
C     the center of mass of the points is placed on the origin of coordinates
C
      ipp=1
      nend=n3
c
      dist=ssx(xx1,xx2,nend)
      if (lgs(34).eq.1) then                                            0317YC99
        sval = dist                                                     0317YC99
        return                                                          0317YC99
      endif                                                             0317YC99
      do 10 i=1,nend
         x(i)=xx1(i)
 10   continue
      call center (5,0)
      do 15 i=1,nend
         xx1(i)=x(i)
 15   continue
      do 20 i=1,nend
         x(i)=xx2(i)
 20   continue      
      call center (5,0)
      do 30 i=1,nend
         xx2(i)=x(i)
 30   continue
c
      dist=ssx(xx1,xx2,nend)
c
      do i=1,natom
         ix = 3*iatom(i)-2
         iy = 3*iatom(i)-1
         iz = 3*iatom(i)
      end do
c
c compute the parameters for eq (7) in the paper
c
 1    do 35 i=1,3
         b(i)=0.0d0
         do 35 j=1,3
           a(i,j)=0.0d0
 35   continue

      do 40 i=1,natom

         xm=AMASS(3*(IATOM(I)))**2  

         ix = 3*iatom(i)-2
         iy = 3*iatom(i)-1
         iz = 3*iatom(i)

         a(1,1) = a(1,1) + xm*(- xx2(ix)*xx1(ix) - xx2(iy)*xx1(iy))
         a(2,1) = a(2,1) + xm*(- xx2(iz)*xx1(iy) )
         a(3,1) = a(3,1) + xm*(  xx2(iz)*xx1(ix) )

         a(1,2) = a(1,2) + xm*(- xx2(iz)*xx1(iy) )
         a(2,2) = a(2,2) + xm*(- xx2(ix)*xx1(ix) - xx2(iz)*xx1(iz))
         a(3,2) = a(3,2) + xm*(- xx2(iy)*xx1(ix) )

         a(1,3) = a(1,3) + xm*(  xx2(iz)*xx1(ix) )
         a(2,3) = a(2,3) + xm*(- xx2(iy)*xx1(ix) )
         a(3,3) = a(3,3) + xm*(- xx2(iy)*xx1(iy) - xx2(iz)*xx1(iz))

         b(1)   = b(1)   + xm*(  xx2(iy)*xx1(ix) - xx2(ix)*xx1(iy))
         b(2)   = b(2)   + xm*(  xx2(iz)*xx1(ix) - xx2(ix)*xx1(iz))
         b(3)   = b(3)   + xm*(  xx2(iz)*xx1(iy) - xx2(iy)*xx1(iz))

 40   continue
c
c solve the system (6) in the paper
c
      if (icode(5).eq.4) then 
          call lin (a,b,y,scr,det,1,3,3,1,3)
      else
          y(1)=0.D0
          y(2)=0.D0
          y(3)=0.D0
      endif
c
c ensure convergence
c
      do i=1,3
         if (abs(y(i)).gt.0.3d0) then
            if (y(i).gt.0.3d0)  xxn=0.3d0
            if (y(i).lt.-0.3d0) xxn=-0.3d0
            write (6,*) ipp,' shifting value for y(',i,') from '
     *,y(i),'to ',xxn 
            y(i)=xxn
         end if
      end do
c
c obtain sinus and cosinus of the three rotation angles
c
      sal = y(1)
      cal = dsqrt(1.0d0-sal**2)
      sbe = y(2)
      cbe = dsqrt(1.0d0-sbe**2)
      sga = y(3)
      cga = dsqrt(1.0d0-sga**2)
      
c
c construct the rotation matrix
c
      rota(1,1) =   cal*cbe
      rota(1,2) = - sal*cga - cal*sbe*sga
      rota(1,3) =   sal*sga - cal*sbe*cga
      
      rota(2,1) =   sal*cbe
      rota(2,2) =   cal*cga - sal*sbe*sga
      rota(2,3) = - cal*sga - sal*sbe*cga
  
      rota(3,1) =   sbe
      rota(3,2) =   cbe*sga
      rota(3,3) =   cbe*cga
c
c rotate coordinates xx2 
c
      do i=1,n3
         xscr(i)=xx2(i)
      end do
      do 50 i=1,natom
         do j=1,3
            sum =0.0d0
            do k=1,3
               idum = 3*(iatom(i)-1)+k
               sum = sum + rota(j,k)*xscr(idum)         
            end do
            idum = 3*(iatom(i)-1)+j
            xx2(idum)=sum
         end do
 50   continue
      do i=1,n3
         x(i)=xx2(i)
      end do
      call center(5,0)
      dist=ssx(xx1,xx2,nend)
      ipp=ipp+1
      if (ipp.gt.1000) stop 'CALCS: failed convergence'
c
c this is the normal return
c
      if ((abs(b(1))+abs(b(2))+abs(b(3))).lt.tol) then
c
c It is possible to get "some" minimum distance but it has to be the 
c global minimum, thus, we have to rotate 180 degrees in the three directions
c
         do k=1,3
            do i=1,natom
               do l=1,3
                  idum = 3*(iatom(i)-1)+l
                  xscr(idum)=xx2(idum)
                  if (l.ne.k) xscr(idum)=(-1.d0)*xx2(idum)
               end do
            end do
            dist2=ssx(xx1,xscr,nend)
            if (dist2.lt.dist) then
               do i=1,nend
                  xx2(i)=xscr(i)
               end do
               goto 1
            end if
         end do
      sval = dist
c
      return
      end if
      goto 1
      
1000  format (3f10.5)
2000  format (4f10.5)
3000  format (4f15.7)
4000  format (i4,3f10.5)
      end SUBROUTINE calcs 
C *****************************************************************************
      SUBROUTINE calcub (X1,Y1,D1,X2,Y2,D2,A0,B1,C2,D3)
      use perconparam
C *****************************************************************************
C
C  JCC  7/31/97
C
C  This subroutine calculates the coeficients A,B,C and D for the polynomial
C  y=A + Bx + Cx**2 + Dx**3 given two points, X1 and X2, its values Y1 and 
C  Y2, and the derivatives at these points, D1 and D2
C
      implicit real(8) (a-h,o-z)
      DIMENSION A(5,5), IN(5)
      DIMENSION B(4,1), C(4,4)
C
C  The matrices are:
C
C               C                      X          B
C
C |  1  X1  X1**2.D0    X1**3.D0 |   | A |      | Y1 |
C |  1  X2  X2**2.D0    X2**3.D0 |   | B |  =   | Y2 |
C |  0   1      2*X1  3*X1**2.D0 |   | C |      | D1 |
C |  0   1      2*X2  3*X2**2.D0 |   | D |      | D2 |
C
C  So, the matrix for the input for mxlneq are:
C
C                  A
C
C |  1  X1  X1**2.D0    X1**3.D0  Y1 |
C |  1  X2  X2**2.D0    X2**3.D0  Y2 |
C |  0   1      2*X1  3*X1**2.D0  D1 |
C |  0   1      2*X2  3*X2**2.D0  D2 |
C
        A(1,1)=1.D0
        A(1,2)=X1
        A(1,3)=X1*X1
        A(1,4)=X1*X1*X1
        A(1,5)=Y1
        A(2,1)=1.D0
        A(2,2)=X2
        A(2,3)=X2*X2
        A(2,4)=X2*X2*X2
        A(2,5)=Y2
        A(3,1)=0.D0
        A(3,2)=1.D0
        A(3,3)=2.D0*X1
        A(3,4)=3.D0*X1*X1
        A(3,5)=D1
        A(4,1)=0.D0
        A(4,2)=1.D0
        A(4,3)=2.D0*X2
        A(4,4)=3.D0*X2*X2
        A(4,5)=D2
        A(5,1)=0.0D0
        A(5,2)=0.D0
        A(5,3)=0.D0
        A(5,4)=0.D0
        A(5,5)=0.D0
        NN=4
        IDA=5 
        MM=-1
        NABSM=5
        call mxlneq (A,NN,IDA,DETT,JRANK,EPS,IN,MM,NABSM)      
C
C  The output is in the 5th coulmn:
C 
        A0=A(1,5)
        B1=A(2,5)
        C2=A(3,5)
        D3=A(4,5)
C
C  Checking
C
c       sol=A(1,5)+A(2,5)*X1+A(3,5)*X1*X1+A(4,5)*X1**3.D0
c       write (*,*) sol,y1
c       sol=A(1,5)+A(2,5)*X2+A(3,5)*X2*X2+A(4,5)*X2**3.D0
c       write (*,*) sol,y2
c       sol=A(2,5)+A(3,5)*X1*2.D0+3.D0*A(4,5)*X1**2.D0
c       write (*,*) sol,d1
c       sol=A(2,5)+A(3,5)*X2*2.D0+3.D0*A(4,5)*X2**2.D0
c       write (*,*) sol,d2
C
C  The following is added by JZ 7/22/2007
C
C       C(1,1)=1.D0
C       C(1,2)=X1
C       C(1,3)=X1*X1
C       C(1,4)=X1*X1*X1
C       C(2,1)=1.D0
C       C(2,2)=X2
C       C(2,3)=X2*X2
C       C(2,4)=X2*X2*X2
C       C(3,1)=0.D0
C       C(3,2)=1.D0
C       C(3,3)=2.D0*X1
C       C(3,4)=3.D0*X1*X1
C       C(4,1)=0.D0
C       C(4,2)=1.D0
C       C(4,3)=2.D0*X2
C       C(4,4)=3.D0*X2*X2
C
C       B(1,1)=Y1
C       B(2,1)=Y2
C       B(3,1)=D1
C       B(4,1)=D2
C       call gaussj (C,4,4,B,1,1)
C
C       A0=B(1,1)
C       B1=B(2,1)
C       C2=B(3,1)
C       D3=B(4,1) 
C
        return
        end SUBROUTINE calcub
C *****************************************************************************
      SUBROUTINE findlv(V1,V0,V2,S,V,RANGE)
C****************************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C     This subroutine calculate the range parameter for an Eckart function
C     passing through reactants, products, saddle point and an extra point,
C     with a higher precision that the original one.
C     The calculation of L can be improved to make it faster and more
C     precisely.
C
      A = V2 - V1
      C = V1
      B = (2.0*V0-A-2.0*C)+ 2.0 * ((V0-C)*(V0-A-C))**0.5
      XACC=.1000D-6
      X1=0.010D0
      X2=1.D1
      RANGE=ZRIDDR(V,A,B,C,S,X1,X2,XACC)
      RETURN
      END SUBROUTINE findlv
C *****************************************************************************
      subroutine rphmef(n3,iflag,lcount,freq,bcurv,redm,s,xarg,xad)
C *****************************************************************************
      use perconparam
C
C    Computes mueff for cdscsag
C    Because of derivs of tps, we now get results at previous save
C    point unless IFLAG .gt. 0, signifying end of path
C
C     CALLS:
C           TPCDSC, QUADFT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION Z(3),ATP(3),STPC(3),TPC(3,2),BKAPM(2),FREDMY(N3TM),
     *FREQ(N3TM),BCURV(NVIBM)
C
      SAVE STPC,TPC,BKAPM
C
C If at end of the grid reuse turning point and zero-point energy
C    derivative info from previous points
C
      BKAP=0.D0
      ISHFT = N3 - N3M7
      KK = 1                                                       
      DO 45 I = 1, N3                                               
             FREDMY(I) = FREQ(I)
45    CONTINUE                                                       
      DO 47 I = 1, NVIBM
             BKAP = BKAP + BCURV(I)*BCURV(I)
47    CONTINUE     
      BKAP=dsqrt(BKAP)
      IF (IFLAG.EQ.0) THEN
        CALL TPCDSC(BCURV,BKAP,FREDMY,n3,REDM,TP)
         TPC(1,KK) = TPC(2,KK)                                        
         TPC(2,KK) = TPC(3,KK)                                          
         TPC(3,KK) = TP                                                
         STPC(1) = STPC(2)
         STPC(2) = STPC(3)
         STPC(3) = S
      ENDIF
C
C    If first time through skip to end; effective mass factors are
C    computed for the first grid point in special section of code
C    when LCOUNT=3.
C
C    If second time through coumpute overlaps then skip end; effective
C    mass factors are computed for the second grid point on the next
C    pass (i.e. for LCOUNT=3) but in the normal section of code.
C
        IF (LCOUNT.GT.2) THEN                                     
C
C    LCOUNT > 2, therefore compute effective mass factors for the current
C    grid point if IFLAG = 0, otherwise for the previous grid point.
C
            PROD = 1.0D0
C
            IF (IFLAG.EQ.0) THEN
C
C  Compute quantities at previous grid point
C
                ARG = TPC(2,KK)*BKAPM(1)                         
                ARG2 = ARG*ARG
C
C  Derivative of the turning pt. wrt s
C
                DO 20 J = 1, 3
                   ATP(J) = ABS(TPC(J,KK))                      
   20           CONTINUE
                CALL QUADFT (STPC,ATP,Z)
                W = Z(2)+2.0D0*Z(3)*STPC(2)
            ELSE
C
C  At end of grid, compute quantities at current grid point
C
                ARG = TPC(3,KK)*BKAP                                 
                ARG2 = ARG*ARG
C
C  Derivative of the turning pt. wrt s
C
                DO 30 J = 1, 3
                   ATP(J) = ABS(TPC(J,KK))                            
   30           CONTINUE
                CALL QUADFT (STPC,ATP,Z)
                W = Z(2)+2.0D0*Z(3)*STPC(3)
            ENDIF
C
C  SC factor
C
            W2 = W*W
            XARG = -2.0D0*ARG-ARG2+W2
            IF (XARG.LT.0.0D0) PROD = PROD*EXP(XARG)
C
C  SC effective mass
C
              XMUEXP = REDM*PROD
                IF (IFLAG.EQ.0.AND.LCOUNT.EQ.3) THEN
C
C  Extra section to generate results at first save point
C
                 PROD = 1.0D0
                 ARG = TPC(1,KK)*BKAPM(2)
                 ARG2 = ARG*ARG
C
C  Derivative of the turning pt. wrt s
C
                 DO 50 J = 1, 3
                    ATP(J) = ABS(TPC(J,KK))
   50            CONTINUE
                 CALL QUADFT (STPC,ATP,Z)
                 W = Z(2)+2.0D0*Z(3)*STPC(1)
                 W2 = W*W
C
C  SC factor
C
                 XAD = -2.0D0*ARG-ARG2+W2
                 IF (XAD.LT.0.0D0) PROD = PROD*EXP(XAD)
                 XMUEXP = REDM*PROD
             ENDIF
        ENDIF
C
C Shift storeage of BFm's and save current coefficient matrix
C
C
      BKAPM(2) = BKAPM(1)
      BKAPM(1) = BKAP
      RETURN
C
      END subroutine rphmef
C *****************************************************************************
      SUBROUTINE spl(x,y,n,s,v,idat,tension)
C****************************************************************************
      use perconparam, only : nsdm
C
C     JCC 7/29/97
C
C     This subroutine is a driver between the Polyrate subroutines and
C     the TSPACK subroutines
C     For information about the meaning of the parameters, see the 
C     header of the TSPSS subroutine
C
C IDAT = 7 corresponds to the ILCG method
C
      implicit double precision(A-H,O-Z)
      save ipas
      real(8), allocatable :: savarr(:,:,:)
      save savarr
C      dimension x(nsdm*2),y(nsdm*2),y2(nsdm*2),w(nsdm*2),wk(22*nsdm),  0423TA02
      dimension x(nsdm*2),y(nsdm*2),w(nsdm*2),wk(22*nsdm),              0423TA02
C     *sigma(nsdm*2),ys(nsdm*2),yp(nsdm*2), ipas(6), savarr(6,2,nsdm*2)
     *sigma(nsdm*2),ys(nsdm*2),yp(nsdm*2), ipas(7)
      logical unif
      logical per
      if(.not.allocated(savarr))then
        allocate(savarr(7,2,nsdm*2)); savarr=0.d00
      end if
c
c     We will save the data from tspss so that the program only goes
c     trough that subroutine once for each set of data
c
      if (idat.ne.0) then
         if (idat.eq.ipas(idat)) goto 10
         ipas(idat)=idat
      endif
c
c     Parameters for tspss
c
      unif=.false.
      if (tension.ge.0.d0) then                                         0911JZ08
        unif = .true.                                                   0911JZ08
      endif                                                             0911JZ08
      per=.false.
      err=1.0D-5
      do i=1,n
         w(i)=1./(err**2.)
         if(tension.lt.0.0d0) then                                      0911JZ08
           sigma(i)=0.                                                  0911JZ08
         else                                                           0911JZ08
           sigma(i)=tension                                             0911JZ08
         endif                                                          0911JZ08
         ys(n)=0.
         yp(n)=0.
      enddo
      sm=n
      smtol=dsqrt(2./sm)
      lwk=11*n
c
c     Parameters for tspsi
c
      ncd=2
c     iendc=0
      iendc=3
c
c     option 1:
c     Evaluation of the first derivatives (yp) and the abcissae (ys)
c
c        call tspss(n,x,y,per,unif,w,sm, smtol,lwk,
c    *              wk,sigma,ys,yp,
c    *              nit,ier)
c
c     Calculation of the desired value
c
c        v=hval(s,n,x,ys,yp,sigma,ier)

c
c     option 2:
c     Evaluation of the first derivatives (yp) and the tension factors (sigma)
c
         call tspsi(n,x,y,ncd,iendc,per,unif,lwk,wk,
     *              yp,sigma,ier)
c
c     Store yp and sigma
c
c  If statements added 1118BE05 to keep arrays in bounds
c
      if (idat.ne.0) then                                               1118BE05
         do i=1,n
            savarr(idat,1,i)=yp(i)
            savarr(idat,2,i)=sigma(i)
         enddo
      endif                                                             1118BE05
         goto 20
c
c set yp and sigma to the appropriate arrays
c
10       do i=1,n
            if (idat.ne.0) then                                         1118BE05
              yp(i)=savarr(idat,1,i)
              sigma(i)=savarr(idat,2,i)
            endif                                                       1118BE05
         enddo
c     Calculation of the desired value
c
20       v=hval(s,n,x,y,yp,sigma,ier)
c

         return
         end SUBROUTINE spl
C *****************************************************************************
      SUBROUTINE splnb(nss,vim,redm,si,vi,ws,t,rv,inmf,lgs6,
     *                   irepr,lbexp,tension)
C ***************************************************************************** 
      use perconparam
C
C     JCC  7/29/97
C
C     This subroutine calculate the value of BFS for a given S using
C     spline under tension.
C     The spline fit is based on B vs atan(2.D0*(S-S0/TL)), where S0 and 
C     TL are calculated using the imaginary frequency at the saddle 
C     point and half the differences in energy between the saddle
C     point, reactants or products.
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION SI(nsdim),VI(nsdim),S(nsdim),V(nsdim),SA(nsdim)
      DIMENSION WFSP(nsdim),WS(nsdim,nvibm), IREPR(8)
C
      ALIM=1.0D-7
C
C     Check LBEXP (if we are using fu30 or fu40 it has not been initialized)
C
      if (lbexp.eq.0) lbexp=3
C
C     We start asigning the values for V, BSP and S in its right order.
C
      V1=0.D0
      V0=VI(NSDIM-2)
      V2=VI(NSDIM)
      S(1)=SI(1)
      V(1)=VI(1)
      WFSP(1)=0.D0
      if (irepr(7).eq.1) then
            v1=vi(2)
            v(1)=vi(2)
            s(1)=si(2)
      endif
      IBEG=3
      IEND=NSS-2
      IDIFF1=1
      IDIFF2=0
      DO i=IBEG,IEND
        S(i-IDIFF1)=SI(i)
        V(i-IDIFF1)=VI(i)
C
C     For the interpolation we will use the absolute values of BFS
C
        WFSP(i-IDIFF1)=ABS(WS(i,inmf))
      ENDDO
      S(IEND+IDIFF2)=SI(NSS)
      V(IEND+IDIFF2)=VI(NSS)
      WFSP(IEND+IDIFF2)=0.D0
      if (irepr(8).eq.1) then
            v2=vi(nsdim-1)
            s(iend+idiff2)=si(nsdim-1)
            v(iend+idiff2)=vi(nsdim-1)
      endif
C
      N=IEND+IDIFF2
C
C     Calculate S0 and TL
C
      A = (V0 - V1)
      B = (V0 - V2)
      WK=(VIM)**2.D0*REDM
      AL1=dsqrt(A/WK)
      AL2=dsqrt(B/WK)
C
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1 + AL2)/2.D0
C
C     For the curvature we take L as L/2
C
      TL=TL/2.D0
      S0=(-AL1+AL2)/2.D0
C
C     We give the values for the asymptotes in reactants and products
C

      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
                SA(1)=-1.D0
      ELSE
                SA(1)=2./PI*ATAN(((S(1)-S0)/TL)**LBEXP)
      ENDIF
      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
                SA(N)=1.D0
      ELSE
                SA(N)=2./PI*ATAN(((S(N)-S0)/TL)**LBEXP)
      ENDIF
C



      DO i=2,N-1
        IF (ABS(S(i)).LT.ALIM) THEN 
C
C     We look for the saddle point and calculate the curvature by
C     linear interpolation. We also calculate SA.
C
              NSP=i
              TINT=-s(i-1)/(s(i+1)-s(i-1))
              WFSP(i)=(1.d0-TINT)*WFSP(i-1)+TINT*WFSP(i+1)
        ENDIF
        SA(i)=2./PI*ATAN(((S(i)-S0)/TL)**LBEXP)
      ENDDO
C
C     Calculate SA for the value of S given and make the interpolation.
C
        TA=2./PI*ATAN(((T-S0)/TL)**LBEXP)


        CALL SPL(SA,WFSP,N,TA,RV,2,TENSION)

      RETURN
      END SUBROUTINE splnb
C *****************************************************************************
      SUBROUTINE splrmi(nss,vim,redm,si,vi,ws,t,rv,inmf,lgs6,irepr,
     *                  tension)
      use perconparam
C****************************************************************************
C
C     JCC 9/26/97
C
C     This subroutine calculate the value of the reduced moment of inertia
C     for a hindered rotor at a given S using spline under tension.
C     The spline fit is based on 1/red.m.i. vs atan((S-S0/TL)), where S0 and
C     TL are calculated using the imaginary frequency at the saddle
C     point and half the differences in energy between the saddle
C     point, reactants or products.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION SI(nsdim),VI(nsdim),S(nsdim),V(nsdim),SA(nsdim)
      DIMENSION WFSP(nsdim),WS(nsdim,nvibm), IREPR(8), g(nvibm)
C
      ALIM=1.0D-7
C
C
C     We start setting some values
C
      IBEG=3
      IEND=NSS-2
      IDIFF1=1
      IDIFF2=0
      M=IEND+IDIFF2
C
C     Look for the saddle point                                         0125JC98
C                                                                       0125JC98
      DO I=IBEG,IEND                                                    0125JC98
         IF (ABS(SI(I)).LT.ALIM) INSP=I                                 0125JC98
      ENDDO                                                             0125JC98
      WSSP = WS(INSP,INMF)                                              0125JC98
C
      V1=0.D0
      V0=VI(NSDIM-2)
      V2=VI(NSDIM)
      S(1)=SI(1)
      V(1)=VI(1)
      ifst=1
      if (irepr(7).eq.1) then
            v1=vi(2)
            v(1)=vi(2)
            s(1)=si(2)
            ifst=2
      endif
C
      DO i=IBEG,IEND
         S(i-IDIFF1)=SI(i)
         V(i-IDIFF1)=VI(i)
         WFSP(i-IDIFF1)=1.D0/dsqrt(WS(i,INMF)/WSSP)                      0125JC98
      ENDDO
      S(IEND+IDIFF2)=SI(NSS)
      V(IEND+IDIFF2)=VI(NSS)
      ilst=nsdim
C
      if (irepr(8).eq.1) then
            v2=vi(nsdim-1)
            s(iend+idiff2)=si(nsdim-1)
            v(iend+idiff2)=vi(nsdim-1)
            ilst=nsdim-1
      endif
C
C     We will set the asymptotic values at reactants and products. We will 
C     correlate the lower with the lower, the second with the second lower,
C     and so on. If there is one more value at the reaction path than at
C     reactants or well, we will correlate with infinity.
C
            do i=1,nvibm
             g(i)=0.D0
            enddo
            c=1.D30
            nt1=1
            nt2=0
            ind1=1
            ind2=0
            do 10 i=1,nvibm
              if (i.eq.nvibm) goto 10
              if (ws(3,inmf).lt.ws(3,i)) then
                  ind1=ind1+1
              endif
              if (ws(3,i).gt.0.1D0) nt1=nt1+1
10          continue
            ind1=nt1-ind1+1
            do j=1,nvibm
              if (ws(ifst,j).gt.0.1D0) then
                  nt2=nt2+1
                  g(nt2)=ws(ifst,j)
              endif
            enddo
            if (ind1.gt.nt2) goto 22
            do i=2,nt2
               do j=1,i
                  if (g(i).lt.g(j)) then
                      tmp=g(i)
                      g(i)=g(j)
                      g(j)=tmp
                  endif
               enddo
            enddo
            wfsp(1)=1.D0/dsqrt(g(ind1)/wssp)                             0125JC98
            goto 30
22          wfsp(1)=0.D0
C
30          do i=1,nvibm
             g(i)=0.D0
            enddo
            c=1.D30
            nt1=1
            nt2=0
            ind1=1
            ind2=0
            do 110 i=1,nvibm
              if (i.eq.nvibm) goto 110
              if (ws(nss-2,inmf).lt.ws(nss-2,i)) then
                  ind1=ind1+1
              endif
              if (ws(nss-2,i).gt.0.1D0) nt1=nt1+1
110         continue
            ind1=nt1-ind1+1
            do j=1,nvibm
              if (ws(ilst,j).gt.0.1D0) then
                  nt2=nt2+1
                  g(nt2)=ws(ilst,j)
              endif
            enddo
            if (ind1.gt.nt2) goto 122
            do i=2,nt2
               do j=1,i
                  if (g(i).lt.g(j)) then
                      tmp=g(i)
                      g(i)=g(j)
                      g(j)=tmp
                  endif
               enddo
            enddo
            wfsp(M)=1.D0/dsqrt(g(ind1)/wssp)                             0125JC98
            goto 130
122         wfsp(M)=0.D0
C
C     Calculate S0 and TL
C
130   A = (V0 - V1)
      B = (V0 - V2)
      WK=(VIM)**2.D0*REDM
      AL1=dsqrt(A/WK)
      AL2=dsqrt(B/WK)
C
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1+AL2)/2.D0
      S0=(-AL1+AL2)/2.D0
C
C     Values for the asymptotes:
C
      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
                SA(1)=-1.D0
      ELSE
                SA(1)=2./PI*ATAN((S(1)-S0)/TL)
      ENDIF
C
      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
                SA(M)=1.D0
      ELSE
                SA(M)=2./PI*ATAN((S(M)-S0)/TL)
      ENDIF
C
C     Look for the saddle point and calculate SA
C
      DO i=2,M-1
        IF (ABS(S(I)).LT.ALIM) NSP=I
        SA(i)=2./PI*ATAN((S(i)-S0)/TL)
      ENDDO
C
C     Calculate SA for the given S and interpolate
C
        TA=2./PI*ATAN((T-S0)/TL)
        CALL SPL(SA,WFSP,M,TA,RV,6,TENSION)
C
C     Calculate the reduced moment of inertia
C
        RV=WSSP*(1.D0/RV)**2.D0                                         0125JC98
C
      RETURN
      END SUBROUTINE splrmi
C

C     
C *****************************************************************************
      SUBROUTINE splnf(nss,vim,redm,si,vi,ws,t,rv,inmf,lgs6,irepr,
     *                 tension)
      use perconparam
C****************************************************************************
C
C     JCC 7/29/97
C
C     This subroutine calculate the value of freq for a given S using
C     spline under tension.
C     The spline fit is based on freq vs atan((S-S0/TL)), where S0 and 
C     TL are calculated using the imaginary frequency at the saddle 
C     point and half the differences in energy between the saddle
C     point, reactants or products.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION SI(nsdim),VI(nsdim),S(nsdim),V(nsdim),SA(nsdim)
      DIMENSION WFSP(nsdim),WS(nsdim,nvibm), IREPR(8)
C
      ALIM=1.0D-7
C
C     We start setting some values
C
      V1=0.D0
      V0=VI(NSDIM-2)
      V2=VI(NSDIM)
      S(1)=SI(1)
      V(1)=VI(1)
      WFSP(1)=WS(1,INMF)
      if (irepr(7).eq.1) then
            v1=vi(2)
            v(1)=vi(2)
            s(1)=si(2)
            wfsp(1)=ws(2,inmf)
      endif
      IBEG=3
      IEND=NSS-2
      IDIFF1=1
      IDIFF2=0
      DO i=IBEG,IEND
         S(i-IDIFF1)=SI(i)
         V(i-IDIFF1)=VI(i)
         WFSP(i-IDIFF1)=WS(i,INMF)
      ENDDO
      S(IEND+IDIFF2)=SI(NSS)
      V(IEND+IDIFF2)=VI(NSS)
      WFSP(IEND+IDIFF2)=WS(NSS,INMF)
C
      if (irepr(8).eq.1) then
            v2=vi(nsdim-1)
            s(iend+idiff2)=si(nsdim-1)
            v(iend+idiff2)=vi(nsdim-1)
            wfsp(iend+idiff2)=ws(nsdim-1,inmf)
      endif
C
      M=IEND+IDIFF2
C
C     Calculate S0 and TL
C
      A = (V0 - V1)
      B = (V0 - V2)
      WK=(VIM)**2.D0*REDM
      AL1=dsqrt(A/WK)
      AL2=dsqrt(B/WK)
C
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1+AL2)/2.D0
      S0=(-AL1+AL2)/2.D0
C
C     Values for the asymptotes:
C
      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
                SA(1)=-1.D0
      ELSE
                SA(1)=2./PI*ATAN((S(1)-S0)/TL)
      ENDIF
C
      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
                SA(M)=1.D0
      ELSE
                SA(M)=2./PI*ATAN((S(M)-S0)/TL)
      ENDIF
C
C     Look for the saddle point and calculate SA
C
      DO i=2,M-1
        IF (ABS(S(I)).LT.ALIM) NSP=I
        SA(i)=2./PI*ATAN((S(i)-S0)/TL)
      ENDDO
C
C     Calculate SA for the given S and interpolate
C
        TA=2./PI*ATAN((T-S0)/TL)
        CALL SPL(SA,WFSP,M,TA,RV,3,TENSION)
      RETURN
      END SUBROUTINE splnf
C *****************************************************************************
      SUBROUTINE splnm(nss,vim,redm,si,vi,ws,t,rv,lgs6,irepr,tension)
      use perconparam
C****************************************************************************
C
C     JCC  7/29/97
C
C     This subroutine calculate the value of 1/I for a given S using
C     a spline under tension fit to the values at the points in
C     fu30 input. The interpolated magnitude is 1/sqrt(I).
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION SI(nsdim),VI(nsdim),S(nsdim),V(nsdim),SA(nsdim)
      DIMENSION WFSP(nsdim),WS(nsdim), IREPR(8)
C
      ALIM=1.0D-7
C
C     We start giving the values of S, I and V to the correspondant arrays
C
      V1=0.D0
      V0=VI(NSDIM-2)
      V2=VI(NSDIM)
      IBEG=3
      IEND=NSS-2
      IDIFF1=1
      IDIFF2=0
C                                                                       0114JC98
C     Look for the saddle point                                         0114JC98
C                                                                       0114JC98
      DO I=IBEG,IEND-1                                                  0114JC98
         IF (ABS(SI(I)).LT.ALIM) INSP=I                                 0114JC98
      ENDDO                                                             0114JC98
C
      S(1)=SI(1)
      V(1)=VI(1)
      WFSP(1)=0.D0
      if (irepr(7).eq.1) then
            v1=vi(2)
            v(1)=vi(2)
            s(1)=si(2)
            wfsp(1)=dsqrt(ws(2)/ws(insp))                               0114JC98
      endif
      DO I=IBEG,IEND
         S(I-IDIFF1)=SI(i)
         V(I-IDIFF1)=VI(i)
         WFSP(I-IDIFF1)=dsqrt(WS(I)/WS(INSP))                            0114JC98
      ENDDO
      S(IEND+IDIFF2)=SI(NSS)
      V(IEND+IDIFF2)=VI(NSS)
      WFSP(IEND+IDIFF2)=0.D0                                            0114JC98
      if (irepr(8).eq.1) then
            v2=vi(nsdim-1)
            s(iend+idiff2)=si(nsdim-1)
            v(iend+idiff2)=vi(nsdim-1)
            wfsp(iend+idiff2)=dsqrt(ws(nsdim-1)/ws(insp))                0114JC98
      endif
C   
      N=IEND+IDIFF2
C
C     Calculation of S0 and TL
C
      A = (V0 - V1)
      B = (V0 - V2)
      WK=(VIM)**2.D0*REDM
      AL1=dsqrt(A/WK)
      AL2=dsqrt(B/WK)
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1 + AL2)/2.D0
      S0=(-AL1+AL2)/2.D0
C
C
C     Values for the asymptotes:
C
      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
                SA(1)=-1.D0
      ELSE
                SA(1)=2./PI*ATAN((S(1)-S0)/TL)
                WFSP(1)=dsqrt(WS(1)/WS(INSP))                            0114JC98
      ENDIF
      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
                SA(N)=1.D0
      ELSE
                SA(N)=2./PI*ATAN((S(N)-S0)/TL)
                WFSP(N)=dsqrt(WS(N)/WS(INSP))                            0114JC98
      ENDIF
C
C     We look for the saddle point and calculate SA
C
      DO i=2,N-1
        SA(i)=2./PI*ATAN((S(i)-S0)/TL)
      ENDDO
C
C     Now, we will convert the S given into a value of SA and call the
C     spline subroutine
C
        TA=2./PI*ATAN((T-S0)/TL)
        CALL SPL(SA,WFSP,N,TA,RV,4,TENSION)
C
C     Calculate the moment of inertia
C
        RV=WS(INSP)*(RV**2.D0)                                          0114JC98
C
      RETURN
      END SUBROUTINE splnm
C *****************************************************************************
      SUBROUTINE splnmf (SHINP,TAB)
      use common_inc
      use rate_const
      use perconparam; use ivtst1
      use kintcm
C *****************************************************************************
C
C     JCC  7/30/97
C
C     This subroutine calculate the value of mueff for a given S using
C     spline under tension.
C     The spline fit is based on mueff vs atan(S-S0/TL), where S0 and
C     TL are calculated using the imaginary frequency at the saddle
C     point and half the differences in energy between the saddle
C     point, reactants or products.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension TAB(3)
      DIMENSION ST(nsdim),SA(nsdim)
      DIMENSION WFSP(nsdim)
C
      ALIM=1.0D-7
C
      do i=1,3
        tab(i)=0.D0
      enddo
      V1=0.D0
      V0=VS(NSDIM-2)
      V2=VS(NSDIM)
      ST(1)=SS(1)
      WFSP(1)=0.D0
      if (irepr(7).eq.1) then
            v1=vs(2)
            st(1)=ss(2)
      endif
      IBEG=3
      IEND=NSS-2-1
      IDIFF1=1
      IDIFF2=0
      INC=0
      DO i=IBEG,IEND
C
C     Eliminate the saddle point (mueff no calculated)
C
         IF (ABS(SS(i)).LT.ALIM) INC=1
         ST(i-IDIFF1)=SS(i+INC)
         WFSP(i-IDIFF1)=ARMCSC(i+INC)
      ENDDO
      ST(IEND+IDIFF2)=SS(NSS)
      WFSP(IEND+IDIFF2)=0.D0
      if (irepr(8).eq.1) then
            v2=vs(nsdim-1)
            st(iend+idiff2)=ss(nsdim-1)
      endif
C
      M=IEND+IDIFF2
C
C     Calculate S0 and TL
C
      A = (V0 - V1)
      B = (V0 - V2)
      WK=(WSTAR)**2.D0*REDM
      AL1=dsqrt(A/WK)
      AL2=dsqrt(B/WK)
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1 + AL2)/2.D0
      S0=(-AL1+AL2)/2.D0
C
C
C     Values for the asymptotes:
C
      IF ((LGS(6).EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS(6).EQ.2.AND.IREPR(7).EQ.0)) THEN
                SA(1)=-1.D0
      ELSE
                SA(1)=2./PI*ATAN((ST(1)-S0)/TL)
      ENDIF
      IF ((LGS(6).EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS(6).EQ.3.AND.IREPR(8).EQ.0)) THEN
                SA(M)=1.D0
      ELSE
                SA(M)=2./PI*ATAN((ST(M)-S0)/TL)
      ENDIF
C
      DO i=2,M-1
        SA(i)=2./PI*ATAN((ST(i)-S0)/TL)
      ENDDO
C
C     Calculate SA for the value of S given and make the interpolation.
C
        NSP=ISSP
        TA=2./PI*ATAN((SHINP-S0)/TL)
        CALL SPL(SA,WFSP,M,TA,RV,5,TENSION)
        PROD=1.D0
        IF (RV.LT.0.D0) PROD=PROD*EXP(RV)
        TAB(1)=REDM*PROD
C
      RETURN
      END SUBROUTINE splnmf
C *****************************************************************************
      SUBROUTINE splnv(nss,issp,vim,redm,si,vi,t,rv,lgs6,
     *                   xmfr,xmfp,irepr,tension)
      use perconparam
C****************************************************************************
C
C     JCC   7/29/97
C
C     The first time we enter this subroutine we will calculate the
C     energy at twenty extra points along the reaction path (plus
C     the points in fu30 or fu40).
C     On the next entrances, it calculates the value of V for a given S 
C     using spline under tension of the data calculated previously.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE NMC,gv,gs
      real(8), allocatable :: gv(:),gs(:)
C
      DIMENSION RL(NSDIM), SI(NSDIM), VI(NSDIM), S(NSDIM), V(NSDIM), 
     *SA(NSDIM),PFX(3),PFY(3),ADS(25),ADV(25),TMPSA(NSDIM),TMPRL(NSDIM),
     *GAS(NSDIM+25), IREPR(8)
      if(.not.allocated(gv))then
        allocate(gv(nsdim+25),gs(nsdim+25))
        gv=0.d00; gs=0.d00
      end if
      
C
      ALIM=1.0D-7
C
C     We start givig values to the energies and S of react, prod and sp.
C     We have to eliminate the energy from the two extra points.
C
      V1=0.D0
      V0=VI(NSDIM-2)
      V2=VI(NSDIM)
      S(1)=SI(1)
      V(1)=VI(1)
      if (irepr(7).eq.1) then
            v1=vi(2)
            v(1)=vi(2)
            s(1)=si(2)
      endif
C
      IBEG=3
      IEND=NSS-2
      IDIFF1=1
      IDIFF2=0
      DO i=IBEG,IEND
        S(i-IDIFF1)=SI(i)
        V(i-IDIFF1)=VI(i)
      ENDDO
      S(IEND+IDIFF2)=SI(NSS)
      V(IEND+IDIFF2)=VI(NSS)
      if (irepr(8).eq.1) then
            v2=vi(nsdim-1)
            s(iend+idiff2)=si(nsdim-1)
            v(iend+idiff2)=vi(nsdim-1)
      endif
C
      N=IEND+IDIFF2
      NSP=ISSP-IDIFF1
C
C     We calculate S0 and TL and some parameters of an Eckart curve
C
      A = V2 - V1
      C = V1
      B = (2.0*V0-A-2.0*C)+ 2.0 * ((V0-C)*(V0-A-C))**0.5
      A2 = (V0 - V1)
      B2 = (V0 - V2)
      WK=(VIM)**2.D0*REDM
      AL1=dsqrt(A2/WK)
      AL2=dsqrt(B2/WK)
C
C     In order to avoid problems is A2 and B2 are too different (very
C     asymmetric reaction) we limit the value of AL1 and AL2
C
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1 + AL2)/2.D0
      S0=(-AL1+AL2)/2.D0
C
C     If this is the first time in this subroutine, we need to calculate
C     the additional values of the energy
C
      IF (NMC.NE.1) THEN
      NMC=1
C
C *** We will start with the calculations for the reactant side of a
C     2->2 or 2->1 reaction
C
      IF ((LGS6.EQ.1.OR.LGS6.EQ.2).AND.IREPR(7).EQ.0) THEN
      DO i=2,NSP-1
        TS=S(i)
        TV=V(i)
C
C     We calculate L using a modified version of the findl 
C     subroutine
C
           CALL FINDLV(V1,V0,V2,TS,TV,RANGE)
C
C     We change the variable from S to SA
C
        SA(i)=2./PI*DATAN((S(i)-S0)/TL)
        RL(i)=RANGE
      ENDDO
C
C     For the saddle point calculate L using the imaginary freq
C
      RANGE = dsqrt((2.0D0*V0*(V0-V2))/(REDM*(VIM*VIM)*B))
      SA(NSP)=2./PI*DATAN((S(NSP)-S0)/TL)
      RL(NSP)=RANGE
C
C     L for reactants and products is calculated by linear interpolation 
C     of the values of L along the reaction path
C     Reactants:
C
        SA(1)=-1.D0
        RL(1)=DMAX1(RL(2)-(RL(3)-RL(2))/(SA(3)-SA(2))*
     *(SA(2)-SA(1)),0.D0)
C
C     Another option would be a cuadratic fit to the last three points
C
      IF (NSP.ge.4) THEN   
         DO in=1,3
           PFX(in)=SA(in+1)
           PFY(in)=RL(in+1)
         ENDDO
         SA(1)=-1.D0
         SEXT=SA(1)
         CALL TREPT (0,PFX,PFY,SEXT,RLEXT)
         RL(1)=DMAX1(RLEXT,0.D0)
      ENDIF
C    
C     Now we have a complete set of values of L. We will calculate the new
C     V values from an Eckart function using the L obtained by spline fit
C     to the calculated L.
C
C     We need 10 extra points between the last point in the grid (SA(2)
C     and the reactants (SA(1)=-1).
C
      XSTEP=ABS((SA(2)-SA(1))/11.D0)
      TA=SA(2)
      M=NSP
      DO J=1,M
         TMPSA(J)=SA(J)
         TMPRL(J)=RL(J)
      ENDDO
      DO I=1,10
         TA=TA-XSTEP
C
C     Call the spline subroutine for getting L at that point
C
        CALL SPL(TMPSA,TMPRL,M,TA,RLS,0,TENSION)
C
C     Estimate S from the mapped value
C
        TNW=TL*TAN(TA*PI/2.D0)+S0
C
C     We calculate V using an Eckart function and the interpolated L
C
        SE0= -RLS * LOG((A+B)/(B-A))
        RV=ECKART(A,B,C,SE0,RLS,TNW)
C
C     And T and RV will be saved as additional V values
C
        ADS(I)=TNW
        ADV(I)=RV
C
      ENDDO
C
C *** Now, the reactant side for a 1->2 or 1->1 reaction
C
      ELSE IF (IREPR(7).EQ.1.OR.LGS6.EQ.3.OR.LGS6.EQ.4) THEN
C
C     Define SINC as the distance between s(reactants) and a (the last
C     point in the middle region).
C
      SINC=1.D-2
C
C     Derivatives and values at the end points (upper and lower)
C
      DVUP=(V(3)-V(2))/(S(3)-S(2))
      DVLW=0.D0
      VUP=V(2)
      SUP=S(2)
      VLW=V(1)+.5*REDM*XMFR*(SINC)**2.D0
      SLW=S(1)+SINC
C
C     Now we have to calculate A0,B1,C2 and D3 for fitting 
C     V=A0+B1s+C2s**2+D3s**3
C
      CALL CALCUB (SLW,VLW,DVLW,SUP,VUP,DVUP,A0,B1,C2,D3)
C
C     Using A,B,C and D we calculate V as a cubic polinomial function
C     of S for ten points between the reactant well and the las point
C
      XSTEP=ABS((S(2)-S(1))/11.D0)
      TA=S(2)
      DO I=1,10
         TA=TA-XSTEP
         ADS(I)=TA
         ADV(I)=A0+B1*ADS(I)+C2*ADS(I)**2.D0+D3*ADS(I)**3.D0
      ENDDO
C
C *** Now, the product side for a 2->2 or 1->2 reaction
C
      ENDIF
      IF ((LGS6.EQ.3.OR.LGS6.EQ.1).AND.IREPR(8).EQ.0) THEN 
C
C     For the saddle point calculate L using the imaginary freq
C
      RANGE = dsqrt((2.0D0*V0*(V0-V2))/(REDM*(VIM*VIM)*B))
      SA(NSP)=2./PI*DATAN((S(NSP)-S0)/TL)
      RL(NSP)=RANGE
C
      DO i=NSP+1,N-1
        TS=S(i)
        TV=V(i)
C
C     We calculate L using a modified version of the findl
C     subroutine
C
        CALL FINDLV(V1,V0,V2,TS,TV,RANGE)
C
C     We change the variable from S to SA
C
        SA(i)=2./PI*DATAN((S(i)-S0)/TL)
        RL(i)=RANGE
      ENDDO
C
C     L for reactants and products is calculated by linear interpolation
C     of the values of L along the reaction path
C     Products:
C
        SA(N)=1.D0
        RL(N)=DMAX1(RL(N-1)-(RL(N-2)-RL(N-1))/
     *(SA(N-2)-SA(N-1))*(SA(N-1)-SA(N)),0.D0)
C
C     Another option would be a cuadratic fit to the last three points
C
      IF ((N-NSP).GE.3) THEN
         DO in=1,3
           PFX(in)=SA(N-in)
           PFY(in)=RL(N-in)
         ENDDO
         SA(N)=1.D0
         SEXT=SA(N)
         CALL TREPT (0,PFX,PFY,SEXT,RLEXT)
         RL(N)=DMAX1(RLEXT,0.D0)
      ENDIF
C
C     No we have a complete set of values of L. We will calculate the new
C     V values from an Eckart function using the L obtained by spline fit
C     to the calculated L.
C
C     We need 10 extra points between the last point in the grid (SA(N-1))
C     and the products (SA(N)=1)
C
C
      XSTEP=ABS((SA(N)-SA(N-1))/11.D0)
      TA=SA(N-1)
      M=N-NSP+1
      DO J=1,M
           TMPSA(J)=SA(NSP+J-1)
           TMPRL(J)=RL(NSP+J-1)
      ENDDO
      DO I=1,10
         TA=TA+XSTEP
C
C     Call the spline subroutine for getting L at that point
C
         CALL SPL(TMPSA,TMPRL,M,TA,RLS,0,TENSION)
C
C     Estimate S from the mapped value
C
         TNW=TL*TAN(TA*PI/2.D0)+S0
C
C     We calculate V using an Eckart function and the interpolated L
C
         SE0= -RLS * LOG((A+B)/(B-A))
         RV=ECKART(A,B,C,SE0,RLS,TNW)
C
C     And T and RV will be saved as additional V values
C
         ADS(25-I)=TNW
         ADV(25-I)=RV
C
      ENDDO
C
C *** Now, the product side for a 2->1 or 1->1 reaction
C
      ELSE IF (IREPR(8).EQ.1.OR.LGS6.EQ.2.OR.LGS6.EQ.4) THEN
C
C     Define SINC as the distance between s(reactants) and a (the last
C     point in the middle region).
C
      SINC=1.D-2
C
C     Derivatives and values at the end points (upper and lower)
C
      DVUP=(V(N-2)-V(N-1))/(S(N-2)-S(N-1))
      DVLW=0.D0
      VUP=V(N-1)
      SUP=S(N-1)
      VLW=V(N)+.5*REDM*XMFP*(SINC)**2.D0
      SLW=S(N)+SINC
C
C     Now we have to calculate A0,B1,C2 and D3 for fitting 
C     V=A0+B1s+C2s**2+D3s**3
C
      CALL CALCUB (SLW,VLW,DVLW,SUP,VUP,DVUP,A0,B1,C2,D3)
C
C     Using A,B,C and D we calculate V as a cubic polinomial function
C     of S for ten points between the reactant well and the las point
C
      XSTEP=ABS((S(N-1)-S(N))/11.D0)
      TA=S(N-1)
      DO I=1,10
        TA=TA+XSTEP
        ADS(25-I)=TA
        ADV(25-I)=A0+B1*ADS(25-I)+C2*ADS(25-I)**2.D0+D3*ADS(25-I)**3.D0
      ENDDO
C
      ENDIF
C
C     Reorder and merge the original and additional information
C
      GS(1)=S(1)
      GV(1)=V(1)
      DO I=1,10
       GS(I+1)=ADS(11-I)
       GV(I+1)=ADV(11-I)
      ENDDO
      DO I=1,N-2
       GS(I+11)=S(I+1)
       GV(I+11)=V(I+1)
      ENDDO
      DO I=1,10
       GS(N+9+I)=ADS(25-I)
       GV(N+9+I)=ADV(25-I)
      ENDDO
      GS(N+20)=S(N)
      GV(N+20)=V(N)
C
      ENDIF
C  
C     Now we will interpolate V using splines based on both the 
C     original and the aditional data
C
      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
                GAS(1)=-1.D0
      ELSE
                GAS(1)=2./PI*ATAN((GS(1)-S0)/TL)
      ENDIF
      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
                GAS(N+20)=1.D0
      ELSE
                GAS(N+20)=2./PI*ATAN((GS(N+20)-S0)/TL)
      ENDIF
      DO I=2,N+19
        GAS(I)=2./PI*ATAN((GS(i)-S0)/TL)
      ENDDO
C
      TA=2./PI*ATAN((T-S0)/TL)
      CALL SPL(GAS,GV,N+20,TA,RV,1,TENSION)
      RETURN
      END SUBROUTINE splnv
C *****************************************************************************
      FUNCTION  ssx (xx1,xx2,nend)
      use common_inc
      use perconparam
C *****************************************************************************
C
C     this routine computes the distance between two cartesian geometries 
C
C
C     CALLED BY:
c            calcs
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION XX1(n3tm),xx2(n3tm)

      sum=0.0d0
      do i=1,nend 
         sum=sum + (amass(i)*(xx1(i)-xx2(i)))**2
      end do
      ssx=dsqrt(sum)

      return
         
      end FUNCTION ssx 
C *****************************************************************************
        DOUBLE PRECISION FUNCTION zriddr(V,A,B,C,SMEP,X1,X2,XACC)
      use perconparam
C *****************************************************************************
C
C       TAKEN FROM NUMERICAL RECIPES IN FORTRAN
C       THIS SUBROUTINE OPRIMIZES THE RANGE PARAMETER FOR MINIMIZING
C       THE DIFFERENCE BETWEEN THE GIVEN AND THE CALCULATED VALUES
C       OF V AT THE EXTRA POINT
C       IT USES RIDDERS' METHOD
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER (MAXIT=60,UNUSED=-1.11E30)
        S0 = -X1 * DLOG((A+B)/(B-A))
        fl=(V-ECKART(A,B,C,S0,x1,SMEP))
        S0 = -X2 * DLOG((A+B)/(B-A))
        fh=(V-ECKART(A,B,C,S0,X2,SMEP))
        if((fl.gt.0..and.fh.lt.0.).or.(fl.lt.0..and.fh.gt.0.))then
                xl=x1
                xh=x2
                zriddr=UNUSED
                do 11 j=1,MAXIT
                   xm=0.5*(xl+xh)
                   S0 = -xm * DLOG((A+B)/(B-A))
                   fm=(V-ECKART(A,B,C,S0,xm,SMEP))
                   s=dsqrt(fm**2-fl*fh)
                   if(s.eq.0.)return
                   xnew=xm+(xm-xl)*(sign(1.0d0,fl-fh)*fm/s)
                   if (dabs(xnew-zriddr).le.xacc) return
                   zriddr=xnew
                   S0 = -zriddr * LOG((A+B)/(B-A))
                   fnew=(V-ECKART(A,B,C,S0,zriddr,SMEP))
                   if (fnew.eq.0.) return
                   if(dsign(fm,fnew).ne.fm) then
                      xl=xm
                      fl=fm
                      xh=zriddr
                      fh=fnew
                      else if(dsign(fl,fnew).ne.fl) then
                      xh=zriddr
                      fh=fnew
                   else if(dsign(fh,fnew).ne.fh) then
                      xl=zriddr
                      fl=fnew
                   else
                      write(fu6,*) 'never get here in zriddr'
                      return
                   endif
                   if(dabs(xh-xl).le.xacc) return
11              continue
                write(fu6,*) 'CAUTION: zriddr exceed maximum iterations'
                return
        else if (fl.eq.0.) then
                zriddr=x1
        else if (fh.eq.0.) then
                zriddr=x2
        else
                write(fu6,*)  V,A,B,C,SMEP,X1,X2,XACC
                write(fu6,*) 'CAUTION: root must be bracketed in zriddr'
                write(fu6,*) 'UNABLE TO FIND L(eck) FOR S = ', SMEP
                write(fu6,*) 'AN UNPHYSICAL VALUE OF 0.0 WILL BE USED'
                zriddr=0.D0
                return
        endif
        return
        END function zriddr
C *****************************************************************************
      FUNCTION ENDSLP (X1,X2,X3,Y1,Y2,Y3,SIGMA)
C *****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     REAL FUNCTION ENDSLP (X1,X2,X3,Y1,Y2,Y3,SIGMA)
C     REAL X1, X2, X3, Y1, Y2, Y3, SIGMA
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/13/91
C
C   Given data values associated with a strictly increasing
C or decreasing sequence of three abscissae X1, X2, and X3,
C this function returns a derivative estimate at X1 based on
C the tension spline H(x) which interpolates the data points
C and has third derivative equal to zero at X1.  Letting S1
C denote the slope defined by the first two points, the est-
C mate is obtained by constraining the derivative of H at X1
C so that it has the same sign as S1 and its magnitude is
C at most 3*abs(S1).  If SIGMA = 0, H(x) is quadratic and
C the derivative estimate is identical to the value computed
C by Subroutine PYC1 at the first point (or the last point
C if the abscissae are decreasing).
C
C On input:
C
C       X1,X2,X3 = Abscissae satisfying either X1 < X2 < X3
C                  or X1 > X2 > X3.
C
C       Y1,Y2,Y3 = Data values associated with the abscis-
C                  sae.  H(X1) = Y1, H(X2) = Y2, and H(X3)
C                  = Y3.
C
C       SIGMA = Tension factor associated with H in inter-
C               val (X1,X2) or (X2,X1).
C
C Input parameters are not altered by this function.
C
C On output:
C
C       ENDSLP = (Constrained) derivative of H at X1, or
C                zero if the abscissae are not strictly
C                monotonic.
C
C Module required by ENDSLP:  SNHCSH
C
C Intrinsic functions called by ENDSLP:  ABS, DMAX1, DMIN1,
C                                          EXP
C
C***********************************************************
C
C     REAL COSHM1, COSHMS, DUMMY, DX1, DXS, S1, SIG1, SIGS,
C    .     T
C
      DX1 = X2 - X1
      DXS = X3 - X1
      IF (DX1*(DXS-DX1) .LE. 0.) GO TO 2
      SIG1 = DABS(SIGMA)
      IF (SIG1 .EQ. 0.) THEN
C
C SIGMA = 0:  H is the quadratic interpolant.
C
        T = (DX1/DXS)**2
        GO TO 1
      ENDIF
      SIGS = SIG1*DXS/DX1
      IF (SIGS .LE. .5) THEN
C
C 0 < SIG1 < SIGS .LE. .5:  compute approximations to
C   COSHM1 = COSH(SIG1)-1 and COSHMS = COSH(SIGS)-1.
C
        CALL SNHCSH (SIG1, DUMMY,COSHM1,DUMMY)
        CALL SNHCSH (SIGS, DUMMY,COSHMS,DUMMY)
        T = COSHM1/COSHMS
      ELSE
C
C SIGS > .5:  compute T = COSHM1/COSHMS.
C
        T = DEXP(SIG1-SIGS)*((1.-DEXP(-SIG1))/
     .                      (1.-DEXP(-SIGS)))**2
      ENDIF
C
C The derivative of H at X1 is
C   T = ((Y3-Y1)*COSHM1-(Y2-Y1)*COSHMS)/
C       (DXS*COSHM1-DX1*COSHMS).
C
C ENDSLP = T unless T*S1 < 0 or abs(T) > 3*abs(S1).
C
    1 T = ((Y3-Y1)*T-Y2+Y1)/(DXS*T-DX1)
      S1 = (Y2-Y1)/DX1
      IF (S1 .GE. 0.) THEN
        ENDSLP = DMIN1(DMAX1(0.D0,T), 3.*S1)
      ELSE
        ENDSLP = DMAX1(DMIN1(0.D0,T), 3.*S1)
      ENDIF
      RETURN
C
C Error in the abscissae.
C
    2 ENDSLP = 0.
      RETURN
      END FUNCTION ENDSLP
C *****************************************************************************
      FUNCTION HVAL (T,N,X,Y,YP,SIGMA, IER)
C *****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N, IER
C     REAL    T, X(N), Y(N), YP(N), SIGMA(N)
      DIMENSION  X(N), Y(N), YP(N), SIGMA(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This function evaluates a Hermite interpolatory tension
C spline H at a point T.  Note that a large value of SIGMA
C may cause underflow.  The result is assumed to be zero.
C
C   Given arrays X, Y, YP, and SIGMA of length NN, if T is
C known to lie in the interval (X(I),X(J)) for some I < J,
C a gain in efficiency can be achieved by calling this
C function with N = J+1-I (rather than NN) and the I-th
C components of the arrays (rather than the first) as par-
C ameters.
C
C On input:
C
C       T = Point at which H is to be evaluated.  Extrapo-
C           lation is performed if T < X(1) or T > X(N).
C
C       N = Number of data points.  N .GE. 2.
C
C       X = Array of length N containing the abscissae.
C           These must be in strictly increasing order:
C           X(I) < X(I+1) for I = 1,...,N-1.
C
C       Y = Array of length N containing data values.
C           H(X(I)) = Y(I) for I = 1,...,N.
C
C       YP = Array of length N containing first deriva-
C            tives.  HP(X(I)) = YP(I) for I = 1,...,N, where
C            HP denotes the derivative of H.
C
C       SIGMA = Array of length N-1 containing tension fac-
C               tors whose absolute values determine the
C               balance between cubic and linear in each
C               interval.  SIGMA(I) is associated with int-
C               erval (I,I+1) for I = 1,...,N-1.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       IER = Error indicator:
C             IER = 0  if no errors were encountered and
C                      X(1) .LE. T .LE. X(N).
C             IER = 1  if no errors were encountered and
C                      extrapolation was necessary.
C             IER = -1 if N < 2.
C             IER = -2 if the abscissae are not in strictly
C                      increasing order.  (This error will
C                      not necessarily be detected.)
C
C       HVAL = Function value H(T), or zero if IER < 0.
C
C Modules required by HVAL:  INTRVL, SNHCSH
C
C Intrinsic functions called by HVAL:  ABS, EXP
C
C***********************************************************
C
      INTEGER I, IP1
C     REAL    B1, B2, CM, CM2, CMM, D1, D2, DUMMY, DX, E,
C    .        E1, E2, EMS, S, S1, SB1, SB2, SBIG, SIG, SM,
C    .        SM2, TM, TP, TS, U, Y1
      INTEGER INTRVL
C
      DATA SBIG/985./
      IF (N .LT. 2) GO TO 1
C
C Find the index of the left end of an interval containing
C   T.  If T < X(1) or T > X(N), extrapolation is performed
C   using the leftmost or rightmost interval.
C
      IF (T .LT. X(1)) THEN
        I = 1
        IER = 1
      ELSEIF (T .GT. X(N)) THEN
        I = N-1
        IER = 1
      ELSE
        I = INTRVL (T,N,X)
        IER = 0
      ENDIF
      IP1 = I + 1
C
C Compute interval width DX, local coordinates B1 and B2,
C   and second differences D1 and D2.
C
      DX = X(IP1) - X(I)
      IF (DX .LE. 0.) GO TO 2
      U = T - X(I)
      B2 = U/DX
      B1 = 1. - B2
      Y1 = Y(I)
      S1 = YP(I)
      S = (Y(IP1)-Y1)/DX
      D1 = S - S1
      D2 = YP(IP1) - S
      SIG = DABS(SIGMA(I))
      IF (SIG .LT. 1.D-20) THEN
C
C SIG = 0:  H is the Hermite cubic interpolant.
C
        HVAL = Y1 + U*(S1 + B2*(D1 + B1*(D1-D2)))
      ELSEIF (SIG .LE. .5) THEN
C
C 0 .LT. SIG .LE. .5:  use approximations designed to avoid
C   cancellation error in the hyperbolic functions.
C
        SB2 = SIG*B2
        CALL SNHCSH (SIG, SM,CM,CMM)
        CALL SNHCSH (SB2, SM2,CM2,DUMMY)
        E = SIG*SM - CMM - CMM
        HVAL = Y1 + S1*U + DX*((CM*SM2-SM*CM2)*(D1+D2) +
     .                         SIG*(CM*CM2-(SM+SIG)*SM2)*D1)
     .                         /(SIG*E)
      ELSE
C
C SIG > .5:  use negative exponentials in order to avoid
C   overflow.  Note that EMS = EXP(-SIG).  In the case of
C   extrapolation (negative B1 or B2), H is approximated by
C   a linear function if -SIG*B1 or -SIG*B2 is large.
C
        SB1 = SIG*B1
        SB2 = SIG - SB1
        IF (-SB1 .GT. SBIG  .OR.  -SB2 .GT. SBIG) THEN
          HVAL = Y1 + S*U
        ELSE
          E1 = DEXP(-SB1)
          E2 = DEXP(-SB2)
          EMS = E1*E2
          TM = 1. - EMS
          TS = TM*TM
          TP = 1. + EMS
          E = TM*(SIG*TP - TM - TM)
          HVAL = Y1 + S*U + DX*(TM*(TP-E1-E2)*(D1+D2) + SIG*
     .                         ((E2+EMS*(E1-2.)-B1*TS)*D1 +
     .                          (E1+EMS*(E2-2.)-B2*TS)*D2))/
     .                          (SIG*E)
        ENDIF
      ENDIF
      RETURN
C
C N is outside its valid range.
C
    1 HVAL = 0.
      IER = -1
      RETURN
C
C X(I) .GE. X(I+1).
C
    2 HVAL = 0.
      IER = -2
      RETURN
      END  FUNCTION HVAL
C *****************************************************************************
      INTEGER FUNCTION INTRVL (T,N,X)
C *****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N
C     REAL    T, X(N)
      DIMENSION  X(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/06/90
C
C   This function returns the index of the left end of an
C interval (defined by an increasing sequence X) which
C contains the value T.  The method consists of first test-
C ing the interval returned by a previous call, if any, and
C then using a binary search if necessary.
C
C On input:
C
C       T = Point to be located.
C
C       N = Length of X.  N .GE. 2.
C
C       X = Array of length N assumed (without a test) to
C           contain a strictly increasing sequence of
C           values.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       INTRVL = Index I defined as follows:
C
C                  I = 1    if  T .LT. X(2) or N .LE. 2,
C                  I = N-1  if  T .GE. X(N-1), and
C                  X(I) .LE. T .LT. X(I+1) otherwise.
C
C Modules required by INTRVL:  None
C
C***********************************************************
C
      INTEGER IH, IL, K
C     REAL    TT
C
      SAVE IL
      DATA IL/1/
      TT = T
      IF (IL .GE. 1  .AND.  IL .LT. N) THEN
        IF (X(IL) .LE. TT  .AND.  TT .LT. X(IL+1)) GO TO 2
      ENDIF
C
C Initialize low and high indexes.
C
      IL = 1
      IH = N
C
C Binary search:
C
    1 IF (IH .LE. IL+1) GO TO 2
        K = (IL+IH)/2
        IF (TT .LT. X(K)) THEN
          IH = K
        ELSE
          IL = K
        ENDIF
        GO TO 1
C
C X(IL) .LE. T .LT. X(IL+1)  or  (T .LT. X(1) and IL=1)
C                            or  (T .GE. X(N) and IL=N-1)
C
    2 INTRVL = IL
      RETURN
      END FUNCTION INTRVL
C *****************************************************************************
      SUBROUTINE SIGS (N,X,Y,YP,TOL, SIGMA, DSMAX,IER)
C****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N, IER
C     REAL    X(N), Y(N), YP(N), TOL, SIGMA(N), DSMAX
      DIMENSION X(N), Y(N), YP(N), SIGMA(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   07/08/92
C
C   Given a set of abscissae X with associated data values Y
C and derivatives YP, this subroutine determines the small-
C est (nonnegative) tension factors SIGMA such that the Her-
C mite interpolatory tension spline H(x) preserves local
C shape properties of the data.  In an interval (X1,X2) with
C data values Y1,Y2 and derivatives YP1,YP2, the properties
C of the data are
C
C       Monotonicity:  S, YP1, and YP2 are nonnegative or
C                        nonpositive,
C  and
C       Convexity:     YP1 .LE. S .LE. YP2  or  YP1 .GE. S
C                        .GE. YP2,
C
C where S = (Y2-Y1)/(X2-X1).  The corresponding properties
C of H are constant sign of the first and second deriva-
C tives, respectively.  Note that, unless YP1 = S = YP2, in-
C finite tension is required (and H is linear on the inter-
C val) if S = 0 in the case of monotonicity, or if YP1 = S
C or YP2 = S in the case of convexity.
C
C   SIGS may be used in conjunction with Subroutine PYC2
C (or PYC2P) in order to produce a C-2 interpolant which
C preserves the shape properties of the data.  This is
C achieved by calling PYC2 with SIGMA initialized to the
C zero vector, and then alternating calls to SIGS with
C calls to PYC2 until the change in SIGMA is small (refer to
C the parameter descriptions for SIGMA, DSMAX and IER), or
C the maximum relative change in YP is bounded by a toler-
C ance (a reasonable value is .01).  A similar procedure may
C be used to produce a C-2 shape-preserving smoothing curve
C (Subroutine SMCRV).
C
C   Refer to Subroutine SIGBI for a means of selecting mini-
C mum tension factors to satisfy more general constraints.
C
C On input:
C
C       N = Number of data points.  N .GE. 2.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values (or
C           function values computed by SMCRV) associated
C           with the abscissae.  H(X(I)) = Y(I) for I =
C           1,...,N.
C
C       YP = Array of length N containing first derivatives
C            of H at the abscissae.  Refer to Subroutines
C            PYC1, PYC1P, PYC2, PYC2P, and SMCRV.
C
C       TOL = Tolerance whose magnitude determines how close
C             each tension factor is to its optimal value
C             when nonzero finite tension is necessary and
C             sufficient to satisfy the constraint:
C             abs(TOL) is an upper bound on the magnitude
C             of the smallest (nonnegative) or largest (non-
C             positive) value of the first or second deriva-
C             tive of H in the interval.  Thus, the con-
C             straint is satisfied, but possibly with more
C             tension than necessary.  TOL should be set to
C             0 for optimal tension.
C
C The above parameters are not altered by this routine.
C
C       SIGMA = Array of length N-1 containing minimum val-
C               ues of the tension factors.  SIGMA(I) is as-
C               sociated with interval (I,I+1) and SIGMA(I)
C               .GE. 0 for I = 1,...,N-1.  SIGMA should be
C               set to the zero vector if minimal tension
C               is desired, and should be unchanged from a
C               previous call in order to ensure convergence
C               of the C-2 iterative procedure.
C
C On output:
C
C       SIGMA = Array containing tension factors for which
C               H(x) preserves the properties of the data,
C               with the restriction that SIGMA(I) .LE. 85
C               for all I (unless the input value is larger).
C               The factors are as small as possible (within
C               the tolerance), but not less than their
C               input values.  If infinite tension is re-
C               quired in interval (X(I),X(I+1)), then
C               SIGMA(I) = 85 (and H is an approximation to
C               the linear interpolant on the interval),
C               and if neither property is satisfied by the
C               data, then SIGMA(I) = 0 (unless the input
C               value is positive), and thus H is cubic in
C               the interval.
C
C       DSMAX = Maximum increase in a component of SIGMA
C               from its input value.  The increase is a
C               relative change if the input value is
C               nonzero, and an absolute change otherwise.
C
C       IER = Error indicator and information flag:
C             IER = I if no errors were encountered and I
C                     components of SIGMA were altered from
C                     their input values for 0 .LE. I .LE.
C                     N-1.
C             IER = -1 if N < 2.  SIGMA is not altered in
C                      this case.
C             IER = -I if X(I) .LE. X(I-1) for some I in the
C                      range 2,...,N.  SIGMA(J-1) is unal-
C                      tered for J = I,...,N in this case.
C
C Modules required by SIGS:  SNHCSH, STOR
C
C Intrinsic functions called by SIGS:  ABS, DMAX1, DMIN1,
C                                        EXP, SIGN, dsqrt
C
C***********************************************************
C
      INTEGER I, ICNT, IP1, LUN, NIT, NM1
C     REAL    A, C1, C2, COSHM, COSHMM, D0, D1, D1D2, D1PD2,
C    .        D2, DMAX, DSIG, DSM, DX, E, EMS, EMS2, F, F0,
C    .        FMAX, FNEG, FP, FTOL, RTOL, S, S1, S2, SBIG,
C    .        SCM, SGN, SIG, SIGIN, SINHM, SSINH, SSM, STOL,
C    .        T, T0, T1, T2, TM, TP1
C     REAL    STOR
C
      DATA SBIG/985./,  LUN/-1/
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 9
C
C Compute an absolute tolerance FTOL = abs(TOL) and a
C   relative tolerance RTOL = 100*MACHEPS.
C
      FTOL = DABS(TOL)
      RTOL = 1.
    1 RTOL = RTOL/2.
        IF (STOR(RTOL+1.) .GT. 1.) GO TO 1
      RTOL = RTOL*200.
C
C Initialize change counter ICNT and maximum change DSM for
C   loop on intervals.
C
      ICNT = 0
      DSM = 0.
      DO 8 I = 1,NM1
        IF (LUN .GE. 0) WRITE (LUN,100) I
  100   FORMAT (//1X,'SIGS -- INTERVAL',I4)
        IP1 = I + 1
        DX = X(IP1) - X(I)
        IF (DX .LE. 0.) GO TO 10
        SIGIN = SIGMA(I)
        IF (SIGIN .GE. SBIG) GO TO 8
C
C Compute first and second differences.
C
        S1 = YP(I)
        S2 = YP(IP1)
        S = (Y(IP1)-Y(I))/DX
        D1 = S - S1
        D2 = S2 - S
        D1D2 = D1*D2
C
C Test for infinite tension required to satisfy either
C   property.
C
        SIG = SBIG
        IF ((D1D2 .EQ. 0.  .AND.  S1 .NE. S2)  .OR.
     .      (S .EQ. 0.  .AND.  S1*S2 .GT. 0.)) GO TO 7
C
C Test for SIGMA = 0 sufficient.  The data satisfies convex-
C   ity iff D1D2 .GE. 0, and D1D2 = 0 implies S1 = S = S2.
C
        SIG = 0.
        IF (D1D2 .LT. 0.) GO TO 3
        IF (D1D2 .EQ. 0.) GO TO 7
        T = DMAX1(D1/D2,D2/D1)
        IF (T .LE. 2.) GO TO 7
        TP1 = T + 1.
C
C Convexity:  Find a zero of F(SIG) = SIG*COSHM(SIG)/
C   SINHM(SIG) - TP1.
C
C   F(0) = 2-T < 0, F(TP1) .GE. 0, the derivative of F
C     vanishes at SIG = 0, and the second derivative of F is
C     .2 at SIG = 0.  A quadratic approximation is used to
C     obtain a starting point for the Newton method.
C
        SIG = dsqrt(10.*T-20.)
        NIT = 0
C
C   Top of loop:
C
        nitjcc=0                                                        1121JC97
    2   IF (SIG .LE. .5) THEN
          CALL SNHCSH (SIG, SINHM,COSHM,COSHMM)
          T1 = COSHM/SINHM
          FP = T1 + SIG*(SIG/SINHM - T1*T1 + 1.)
        ELSE
C
C   Scale SINHM and COSHM by 2*EXP(-SIG) in order to avoid
C     overflow with large SIG.
C
          EMS = DEXP(-SIG)
          SSM = 1. - EMS*(EMS+SIG+SIG)
          T1 = (1.-EMS)*(1.-EMS)/SSM
          FP = T1 + SIG*(2.*SIG*EMS/SSM - T1*T1 + 1.)
        ENDIF
C
        F = SIG*T1 - TP1
        IF (LUN .GE. 0) WRITE (LUN,110) SIG, F, FP
  110   FORMAT (5X,'CONVEXITY -- SIG = ',E15.8,
     .          ', F(SIG) = ',E15.8/1X,35X,'FP(SIG) = ',
     .          E15.8)
        NIT = NIT + 1
C
C   Test for convergence.
C
        IF (FP .LE. 0.) GO TO 7
        DSIG = -F/FP
        IF (ABS(DSIG) .LE. RTOL*SIG  .OR.  (F .GE. 0.  .AND.
     .      F .LE. FTOL)  .OR.  ABS(F) .LE. RTOL) GO TO 7
C
C   Update SIG.
C
        SIG = SIG + DSIG
C
C   Added by JCC in order to prevent an infinite loop
        nitjcc=nitjcc+1                                                 1121JC97    
        if (nitjcc.gt.250) then
                           write (6,*) 'Warning in SIGS: Too ',         1121JC97
     *                       'slow convergence in sig. Loop ',          1121JC97
     *                       'stopped. Examine carefully the results'   1121JC97    
        else                                                            1121JC97
C
        GO TO 2
        endif                                                           1121JC97
C
C Convexity cannot be satisfied.  Monotonicity can be satis-
C   fied iff S1*S .GE. 0 and S2*S .GE. 0 since S .NE. 0.
C
    3   IF (S1*S .LT. 0.  .OR.  S2*S .LT. 0.) GO TO 7
        T0 = 3.*S - S1 - S2
        D0 = T0*T0 - S1*S2
C
C SIGMA = 0 is sufficient for monotonicity iff S*T0 .GE. 0
C   or D0 .LE. 0.
C
        IF (D0 .LE. 0.  .OR.  S*T0 .GE. 0.) GO TO 7
C
C Monotonicity:  find a zero of F(SIG) = SIGN(S)*HP(R),
C   where HPP(R) = 0 and HP, HPP denote derivatives of H.
C   F has a unique zero, F(0) < 0, and F approaches abs(S)
C   as SIG increases.
C
C   Initialize parameters for the secant method.  The method
C     uses three points:  (SG0,F0), (SIG,F), and
C     (SNEG,FNEG), where SG0 and SNEG are defined implicitly
C     by DSIG = SIG - SG0 and DMAX = SIG - SNEG.
C
        SGN = SIGN(1.d0,S)
        SIG = SBIG
        FMAX = SGN*(SIG*S-S1-S2)/(SIG-2.)
        IF (FMAX .LE. 0.) GO TO 7
        STOL = RTOL*SIG
        F = FMAX
        F0 = SGN*D0/(3.*(D1-D2))
        FNEG = F0
        DSIG = SIG
        DMAX = SIG
        D1PD2 = D1 + D2
        NIT = 0
C
C   Top of loop:  compute the change in SIG by linear
C     interpolation.
C
    4   DSIG = -F*DSIG/(F-F0)
        IF (LUN .GE. 0) WRITE (LUN,120) DSIG
  120   FORMAT (5X,'MONOTONICITY -- DSIG = ',E15.8)
        IF (DABS(DSIG) .GT. DABS(DMAX)) GO TO 6
C
C   Restrict the step-size such that abs(DSIG) .GE. STOL/2.
C     Note that DSIG and DMAX have opposite signs.
C
        IF (DABS(DSIG) .LT. STOL/2.) DSIG = -DSIGN(STOL/2.,
     .                              DMAX)
C
C   Update SIG, F0, and F.
C
        SIG = SIG + DSIG
        F0 = F
        IF (SIG .LE. .5) THEN
C
C   Use approximations to the hyperbolic functions designed
C     to avoid cancellation error with small SIG.
C
          CALL SNHCSH (SIG, SINHM,COSHM,COSHMM)
          C1 = SIG*COSHM*D2 - SINHM*D1PD2
          C2 = SIG*(SINHM+SIG)*D2 - COSHM*D1PD2
          A = C2 - C1
          E = SIG*SINHM - COSHMM - COSHMM
        ELSE
C
C   Scale SINHM and COSHM by 2*EXP(-SIG) in order to avoid
C     overflow with large SIG.
C
          EMS = DEXP(-SIG)
          EMS2 = EMS + EMS
          TM = 1. - EMS
          SSINH = TM*(1.+EMS)
          SSM = SSINH - SIG*EMS2
          SCM = TM*TM
          C1 = SIG*SCM*D2 - SSM*D1PD2
          C2 = SIG*SSINH*D2 - SCM*D1PD2
C
C   R is in (0,1) and well-defined iff HPP(X1)*HPP(X2) < 0.
C
          F = FMAX
          IF (C1*(SIG*SCM*D1 - SSM*D1PD2) .GE. 0.) GO TO 5
          A = EMS2*(SIG*TM*D2 + (TM-SIG)*D1PD2)
          IF (A*(C2+C1) .LT. 0.) GO TO 5
          E = SIG*SSINH - SCM - SCM
        ENDIF
C
        F = (SGN*(E*S2-C2) + dsqrt(A*(C2+C1)))/E
C
C   Update number of iterations NIT.
C
    5   NIT = NIT + 1
        IF (LUN .GE. 0) WRITE (LUN,130) NIT, SIG, F
  130   FORMAT (1X,10X,I2,' -- SIG = ',E15.8,', F = ',
     .          E15.8)
C
C   Test for convergence.
C
        STOL = RTOL*SIG
        IF ( DABS(DMAX) .LE. STOL  .OR.  (F .GE. 0.  .AND.
     .      F .LE. FTOL)  .OR.  DABS(F) .LE. RTOL ) GO TO 7
        DMAX = DMAX + DSIG
        IF ( F0*F .GT. 0.  .AND.  DABS(F) .GE. DABS(F0) )
     .     GO TO 6
        IF (F0*F .LE. 0.) THEN
C
C   F and F0 have opposite signs.  Update (SNEG,FNEG) to
C     (SG0,F0) so that F and FNEG always have opposite
C     signs.  If SIG is closer to SNEG than SG0 and abs(F) <
C     abs(FNEG), then swap (SNEG,FNEG) with (SG0,F0).
C
          T1 = DMAX
          T2 = FNEG
          DMAX = DSIG
          FNEG = F0
          IF ( DABS(DSIG) .GT. DABS(T1)  .AND.
     .         DABS(F) .LT. DABS(T2)         ) THEN
C
            DSIG = T1
            F0 = T2
          ENDIF
        ENDIF
        GO TO 4
C
C   Bottom of loop:  F0*F > 0 and the new estimate would
C     be outside of the bracketing interval of length
C     abs(DMAX).  Reset (SG0,F0) to (SNEG,FNEG).
C
    6   DSIG = DMAX
        F0 = FNEG
        GO TO 4
C
C  Update SIGMA(I), ICNT, and DSM if necessary.
C
    7   SIG = DMIN1(SIG,SBIG)
        IF (SIG .GT. SIGIN) THEN
          SIGMA(I) = SIG
          ICNT = ICNT + 1
          DSIG = SIG-SIGIN
          IF (SIGIN .GT. 0.) DSIG = DSIG/SIGIN
          DSM = DMAX1(DSM,DSIG)
        ENDIF
    8   CONTINUE
C
C No errors encountered.
C
      DSMAX = DSM
      IER = ICNT
      RETURN
C
C N < 2.
C
    9 DSMAX = 0.
      IER = -1
      RETURN
C
C X(I+1) .LE. X(I).
C
   10 DSMAX = DSM
      IER = -IP1
      RETURN
      END SUBROUTINE SIGS
C *****************************************************************************
      SUBROUTINE SNHCSH (X, SINHM,COSHM,COSHMM)
C****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     REAL X, SINHM, COSHM, COSHMM
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This subroutine computes approximations to the modified
C hyperbolic functions defined below with relative error
C bounded by 4.7E-12 for a floating point number system with
C sufficient precision.  For IBM 370 single precision, the
C relative error was found to be bounded by 2.1E-4 for all
C x, and 6.0E-6 for abs(x) .LE. .5.
C
C   Note that the 13-digit constants in the data statements
C below may not be acceptable to all compilers.
C
C On input:
C
C       X = Point at which the functions are to be
C           evaluated.
C
C X is not altered by this routine.
C
C On output:
C
C       SINHM = sinh(X) - X.
C
C       COSHM = cosh(X) - 1.
C
C       COSHMM = cosh(X) - 1 - X*X/2.
C
C Modules required by SNHCSH:  None
C
C Intrinsic functions called by SNHCSH:  ABS, EXP
C
C***********************************************************
C
C     REAL AX, C1, C2, C3, C4, EXPX, F, XC, XS, XSD2, XSD4
C
      DATA C1/.1666666666659E0/,
     .     C2/.8333333431546E-2/,
     .     C3/.1984107350948E-3/,
     .     C4/.2768286868175E-5/
      AX = ABS(X)
      XS = AX*AX
      IF (AX .LE. .5) THEN
C
C Approximations for small X:
C
        XC = X*XS
        SINHM = XC*(((C4*XS+C3)*XS+C2)*XS+C1)
        XSD4 = .25*XS
        XSD2 = XSD4 + XSD4
        F = (((C4*XSD4+C3)*XSD4+C2)*XSD4+C1)*XSD4
        COSHMM = XSD2*F*(F+2.)
        COSHM = COSHMM + XSD2
      ELSE
C
C Approximations for large X:
C
        EXPX = DEXP(AX)
        SINHM = -(((1./EXPX+AX)+AX)-EXPX)/2.
        IF (X .LT. 0.) SINHM = -SINHM
        COSHM = ((1./EXPX-2.)+EXPX)/2.
        COSHMM = COSHM - XS/2.
      ENDIF
      RETURN
      END SUBROUTINE SNHCSH
C *****************************************************************************
      FUNCTION STOR (X)
C *****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
C     REAL FUNCTION STOR (X)
C     REAL X
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This function forces its argument X to be stored in a
C memory location, thus providing a means of determining
C floating point number characteristics (such as the machine
C precision) when it is necessary to avoid computation in
C high precision registers.
C
C On input:
C
C       X = Value to be stored.
C
C X is not altered by this function.
C
C On output:
C
C       STOR = Value of X after it has been stored and
C               possibly truncated or rounded to the single
C               precision word length.
C
C Modules required by STOR:  None
C
C***********************************************************
C
C     REAL Y
c     COMMON/STCOM/Y
      Y = X
      STOR = Y
      RETURN
      END
C *****************************************************************************
      SUBROUTINE TSPSI (N,X,Y,NCD,IENDC,PER,UNIFRM,LWK, WK,
     .                  YP,SIGMA, IER)
C *****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N, NCD, IENDC, LWK, IER
      LOGICAL PER, UNIFRM
      DIMENSION  X(N), Y(N), WK(LWK), YP(N), SIGMA(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   07/08/91
C
C   This subroutine computes a set of parameter values which
C define a Hermite interpolatory tension spline H(x).  The
C parameters consist of knot derivative values YP computed
C by Subroutine PYC1, PYC1P, PYC2, or PYC2P, and tension
C factors SIGMA computed by Subroutine SIGS (unless UNIFRM =
C TRUE).  Alternative methods for computing SIGMA are pro-
C vided by Subroutine TSPBI and Functions SIG0, SIG1, and
C SIG2.
C
C   Refer to Subroutine TSPSS for a means of computing
C parameters which define a smoothing curve rather than an
C interpolatory curve.
C
C   The tension spline may be evaluated by Subroutine TSVAL1
C or Functions HVAL (values), HPVAL (first derivatives),
C HPPVAL (second derivatives), and TSINTL (integrals).
C
C On input:
C
C       N = Number of data points.  N .GE. 2 and N .GE. 3 if
C           PER = TRUE.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values asso-
C           ciated with the abscissae.  H(X(I)) = Y(I) for
C           I = 1,...,N.  If NCD = 1 and PER = TRUE, Y(N)
C           is set to Y(1).
C
C       NCD = Number of continuous derivatives at the knots.
C             NCD = 1 or NCD = 2.  If NCD = 1, the YP values
C             are computed by local monotonicity-constrained
C             quadratic fits.  Otherwise, a linear system is
C             solved for the derivative values which result
C             in second derivative continuity.  Unless
C             UNIFRM = TRUE, this requires iterating on
C             calls to PYC2 or PYC2P and calls to SIGS, and
C             generally results in more nonzero tension
C             factors (hence more expensive evaluation).
C
C       IENDC = End condition indicator for NCD = 2 and PER
C               = FALSE (or dummy parameter otherwise):
C               IENDC = 0 if YP(1) and YP(N) are to be com-
C                         puted by monotonicity-constrained
C                         parabolic fits to the first three
C                         and last three points, respective-
C                         ly.  This is identical to the
C                         values computed by PYC1.
C               IENDC = 1 if the first derivatives of H at
C                         X(1) and X(N) are user-specified
C                         in YP(1) and YP(N), respectively.
C               IENDC = 2 if the second derivatives of H at
C                         X(1) and X(N) are user-specified
C                         in YP(1) and YP(N), respectively.
C               IENDC = 3 if the end conditions are to be
C                         computed by Subroutine ENDSLP and
C                         vary with SIGMA(1) and SIGMA(N-1).
C
C       PER = Logical variable with value TRUE if and only
C             H(x) is to be a periodic function with period
C             X(N)-X(1).  It is assumed without a test that
C             Y(N) = Y(1) in this case.  On output, YP(N) =
C             YP(1).  If H(x) is one of the components of a
C             parametric curve, this option may be used to
C             obtained a closed curve.
C
C       UNIFRM = Logical variable with value TRUE if and
C                only if constant (uniform) tension is to be
C                used.  The tension factor must be input in
C                SIGMA(1) in this case and must be in the
C                range 0 to 85.  If SIGMA(1) = 0, H(x) is
C                piecewise cubic (a cubic spline if NCD =
C                2), and as SIGMA increases, H(x) approaches
C                the piecewise linear interpolant.  If
C                UNIFRM = FALSE, tension factors are chosen
C                (by SIGS) to preserve local monotonicity
C                and convexity of the data.  This often
C                improves the appearance of the curve over
C                the piecewise cubic fit.
C
C       LWK = Length of work space WK:  no work space is
C             needed if NCD = 1; at least N-1 locations
C             are required if NCD = 2; another N-1 locations
C             are required if PER = TRUE; and an additional
C             N-1 locations are required for the convergence
C             test if SIGS is called (UNIFRM = FALSE):
C
C             LWK GE 0    if NCD=1
C             LWK GE N-1  if NCD=2, PER=FALSE, UNIFRM=TRUE
C             LWK GE 2N-2 if NCD=2, PER=TRUE,  UNIFRM=TRUE
C             LWK GE 2N-2 if NCD=2, PER=FALSE, UNIFRM=FALSE
C             LWK GE 3N-3 if NCD=2, PER=TRUE,  UNIFRM=FALSE
C
C   The above parameters, except possibly Y(N), are not
C altered by this routine.
C
C       WK = Array of length at least LWK to be used as
C            temporary work space.
C
C       YP = Array of length .GE. N containing end condition
C            values in positions 1 and N if NCD = 2 and
C            IENDC = 1 or IENDC = 2.
C
C       SIGMA = Array of length .GE. N-1 containing a ten-
C               sion factor (0 to 85) in the first position
C               if UNIFRM = TRUE.
C
C On output:
C
C       WK = Array containing convergence parameters in the
C            first two locations if IER > 0 (NCD = 2 and
C            UNIFRM = FALSE):
C            WK(1) = Maximum relative change in a component
C                    of YP on the last iteration.
C            WK(2) = Maximum relative change in a component
C                    of SIGMA on the last iteration.
C
C       YP = Array containing derivatives of H at the
C            abscissae.  YP is not altered if -4 < IER < 0,
C            and YP is only partially defined if IER = -4.
C
C       SIGMA = Array containing tension factors.  SIGMA(I)
C               is associated with interval (X(I),X(I+1))
C               for I = 1,...,N-1.  SIGMA is not altered if
C               -4 < IER < 0 (unless IENDC is invalid), and
C               SIGMA is constant (not optimal) if IER = -4
C               or IENDC (if used) is invalid.
C
C       IER = Error indicator or iteration count:
C             IER = IC .GE. 0 if no errors were encountered
C                      and IC calls to SIGS and IC+1 calls
C                      to PYC1, PYC1P, PYC2 or PYC2P were
C                      employed.  (IC = 0 if NCD = 1).
C             IER = -1 if N, NCD, or IENDC is outside its
C                      valid range.
C             IER = -2 if LWK is too small.
C             IER = -3 if UNIFRM = TRUE and SIGMA(1) is out-
C                      side its valid range.
C             IER = -4 if the abscissae X are not strictly
C                      increasing.
C
C Modules required by TSPSI:  ENDSLP, SIGS, SNHCSH, STOR,
C                               PYCOEF, PYC1, PYC1P, PYC2,
C                               PYC2P
C
C Intrinsic functions called by TSPSI:  ABS, DMAX1
C
C***********************************************************
C
      INTEGER I, ICNT, IERR, ITER, MAXIT, NM1, NN
C     REAL    DSMAX, DYP, DYPTOL, E, SBIG, SIG, STOL,
C    .        YP1, YPN
C
      DATA SBIG/85./
C
C Convergence parameters:
C
C   STOL = Absolute tolerance for SIGS
C   MAXIT = Maximum number of PYC2/SIGS iterations
C   DYPTOL = Bound on the maximum relative change in a
C              component of YP defining convergence of
C              the PYC2/SIGS iteration when NCD = 2 and
C              UNIFRM = FALSE
C
      DATA STOL/0./,  MAXIT/99/,  DYPTOL/.01/
C
C Test for invalid input parameters (other than X and
C   IENDC).
C
      NN = N
      NM1 = NN - 1
      IF (NN .LT. 2  .OR.  (PER  .AND.  NN .LT. 3)  .OR.
     .    NCD .LT. 1  .OR.  NCD .GT. 2) GO TO 11
      IF (UNIFRM) THEN
        IF ( NCD .EQ. 2  .AND.  (LWK .LT. NM1  .OR.
     .       (PER  .AND.  LWK .LT. 2*NM1)) ) GO TO 12
        SIG = SIGMA(1)
        IF (SIG .LT. 0.  .OR.  SIG .GT. SBIG) GO TO 13
      ELSE
        IF ( NCD .EQ. 2  .AND.  (LWK .LT. 2*NM1  .OR.
     .       (PER  .AND.  LWK .LT. 3*NM1)) ) GO TO 12
        SIG = 0.
      ENDIF
C
C Initialize iteration count ITER, and store uniform
C   tension factors, or initialize SIGMA to zeros.
C
      ITER = 0
      DO 1 I = 1,NM1
        SIGMA(I) = SIG
    1   CONTINUE
      IF (NCD .EQ. 1) THEN
C
C NCD = 1.
C
        IF (.NOT. PER) THEN
          CALL PYC1 (NN,X,Y, YP,IERR)
        ELSE
          CALL PYC1P (NN,X,Y, YP,IERR)
        ENDIF
        IF (IERR .NE. 0) GO TO 14
        IF (.NOT. UNIFRM) THEN
C
C   Call SIGS for UNIFRM = FALSE.
C
          CALL SIGS (NN,X,Y,YP,STOL, SIGMA, DSMAX,IERR)
        ENDIF
        GO TO 10
      ENDIF
C
C NCD = 2.
C
      IF (.NOT. PER) THEN
C
C   Nonperiodic case:  call PYC2 and test for IENDC or X
C     invalid.
C
        YP1 = YP(1)
        YPN = YP(NN)
        CALL PYC2 (NN,X,Y,SIGMA,IENDC,IENDC,YP1,YPN,
     .             WK, YP,IERR)
        IF (IERR .EQ. 1) GO TO 11
        IF (IERR .GT. 1) GO TO 14
      ELSE
C
C   Periodic fit:  call PYC2P.
C
        CALL PYC2P (NN,X,Y,SIGMA,WK, YP,IERR)
        IF (IERR .GT. 1) GO TO 14
      ENDIF
      IF (UNIFRM) GO TO 10
C
C   Iterate on calls to SIGS and PYC2 (or PYC2P).  The first
C     N-1 WK locations are used to store the derivative
C     estimates YP from the previous iteration.
C
C   DYP is the maximum relative change in a component of YP.
C   ICNT is the number of tension factors which were
C        increased by SIGS.
C   DSMAX is the maximum relative change in a component of
C         SIGMA.
C
      DO 4 ITER = 1,MAXIT
        DYP = 0.
        DO 2 I = 2,NM1
          WK(I) = YP(I)
    2     CONTINUE
        CALL SIGS (NN,X,Y,YP,STOL, SIGMA, DSMAX,ICNT)
        IF (.NOT. PER) THEN
          CALL PYC2 (NN,X,Y,SIGMA,IENDC,IENDC,YP1,YPN,
     .               WK(NN), YP,IERR)
        ELSE
          CALL PYC2P (NN,X,Y,SIGMA,WK(NN), YP,IERR)
        ENDIF
        DO 3 I = 2,NM1
          E = DABS(YP(I)-WK(I))
          IF (WK(I) .NE. 0.) E = E/DABS(WK(I))
          DYP = DMAX1(DYP,E)
    3     CONTINUE
        IF (ICNT .EQ. 0  .OR.  DYP .LE. DYPTOL) GO TO 5
    4   CONTINUE
      ITER = MAXIT
C
C Store convergence parameters in WK.
C
    5 WK(1) = DYP
      WK(2) = DSMAX
C
C No error encountered.
C
   10 IER = ITER
      RETURN
C
C Invalid input parameter N, NCD, or IENDC.
C
   11 IER = -1
      RETURN
C
C LWK too small.
C
   12 IER = -2
      RETURN
C
C UNIFRM = TRUE and SIGMA(1) outside its valid range.
C
   13 IER = -3
      RETURN
C
C Abscissae are not strictly increasing.
C
   14 IER = -4
      RETURN
      END SUBROUTINE TSPSI
C *****************************************************************************
      SUBROUTINE PYC1 (N,X,Y, YP,IER)
C****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N, IER
      DIMENSION X(N), Y(N), YP(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/17/91
C
C   This subroutine employs a three-point quadratic interpo-
C lation method to compute local derivative estimates YP
C associated with a set of data points.  The interpolation
C formula is the monotonicity-constrained parabolic method
C described in the reference cited below.  A Hermite int-
C erpolant of the data values and derivative estimates pre-
C serves monotonicity of the data.  Linear interpolation is
C used if N = 2.  The method is invariant under a linear
C scaling of the coordinates but is not additive.
C
C On input:
C
C       N = Number of data points.  N .GE. 2.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values asso-
C           ciated with the abscissae.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       YP = Array of length N containing estimated deriv-
C            atives at the abscissae unless IER .NE. 0.
C            YP is not altered if IER = 1, and is only par-
C            tially defined if IER > 1.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N < 2.
C             IER = I if X(I) .LE. X(I-1) for some I in the
C                     range 2,...,N.
C
C Reference:  J. M. Hyman, "Accurate Monotonicity-preserving
C               Cubic Interpolation",  LA-8796-MS, Los
C               Alamos National Lab, Feb. 1982.
C
C Modules required by PYC1:  None
C
C Intrinsic functions called by PYC1:  ABS, DMAX1, DMIN1,
C                                        SIGN
C
C***********************************************************
C
      INTEGER I, NM1
C     REAL    ASI, ASIM1, DX2, DXI, DXIM1, S2, SGN, SI,
C    .        SIM1, T
C
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 2
      I = 1
      DXI = X(2) - X(1)
      IF (DXI .LE. 0.) GO TO 3
      SI = (Y(2)-Y(1))/DXI
      IF (NM1 .EQ. 1) THEN
C
C Use linear interpolation for N = 2.
C
        YP(1) = SI
        YP(2) = SI
        IER = 0
        RETURN
      ENDIF
C
C N .GE. 3.  YP(1) = S1 + DX1*(S1-S2)/(DX1+DX2) unless this
C   results in YP(1)*S1 .LE. 0 or abs(YP(1)) > 3*abs(S1).
C
      I = 2
      DX2 = X(3) - X(2)
      IF (DX2 .LE. 0.) GO TO 3
      S2 = (Y(3)-Y(2))/DX2
      T = SI + DXI*(SI-S2)/(DXI+DX2)
      IF (SI .GE. 0.) THEN
        YP(1) = DMIN1(DMAX1(0.D0,T), 3.*SI)
      ELSE
        YP(1) = DMAX1(DMIN1(0.D0,T), 3.*SI)
      ENDIF
C
C YP(I) = (DXIM1*SI+DXI*SIM1)/(DXIM1+DXI) subject to the
C   constraint that YP(I) has the sign of either SIM1 or
C   SI, whichever has larger magnitude, and abs(YP(I)) .LE.
C   3*min(abs(SIM1),abs(SI)).
C
      DO 1 I = 2,NM1
        DXIM1 = DXI
        DXI = X(I+1) - X(I)
        IF (DXI .LE. 0.) GO TO 3
        SIM1 = SI
        SI = (Y(I+1)-Y(I))/DXI
        T = (DXIM1*SI+DXI*SIM1)/(DXIM1+DXI)
        ASIM1 = DABS(SIM1)
        ASI = DABS(SI)
        SGN = DSIGN(1.0d0,SI)
        IF (ASIM1 .GT. ASI) SGN = DSIGN(1.0d0,SIM1)
        IF (SGN .GT. 0.) THEN
          YP(I) = DMIN1(DMAX1(0.D0,T), 3.*DMIN1(ASIM1,ASI))
        ELSE
          YP(I) = DMAX1(DMIN1(0.D0,T), -3.*DMIN1(ASIM1,ASI))
        ENDIF
    1   CONTINUE
C
C YP(N) = SNM1 + DXNM1*(SNM1-SNM2)/(DXNM2+DXNM1) subject to
C   the constraint that YP(N) has the sign of SNM1 and
C   abs(YP(N)) .LE. 3*abs(SNM1).  Note that DXI = DXNM1 and
C   SI = SNM1.
C
      T = SI + DXI*(SI-SIM1)/(DXIM1+DXI)
      IF (SI .GE. 0.) THEN
        YP(N) = DMIN1(DMAX1(0.D0,T), 3.*SI)
      ELSE
        YP(N) = DMAX1(DMIN1(0.D0,T), 3.*SI)
      ENDIF
      IER = 0
      RETURN
C
C N is outside its valid range.
C
    2 IER = 1
      RETURN
C
C X(I+1) .LE. X(I).
C
    3 IER = I + 1
      RETURN
      END SUBROUTINE PYC1
C *****************************************************************************
      SUBROUTINE PYC1P (N,X,Y, YP,IER)
C****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N, IER
      DIMENSION    X(N), Y(N), YP(N)
C     REAL    X(N), Y(N), YP(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/17/91
C
C   This subroutine employs a three-point quadratic interpo-
C lation method to compute local derivative estimates YP
C associated with a set of N data points (X(I),Y(I)).  It
C is assumed that Y(N) = Y(1), and YP(N) = YP(1) on output.
C Thus, a Hermite interpolant H(x) defined by the data
C points and derivative estimates is periodic with period
C X(N)-X(1).  The derivative-estimation formula is the
C monotonicity-constrained parabolic fit described in the
C reference cited below:  H(x) is monotonic in intervals in
C which the data is monotonic.  The method is invariant
C under a linear scaling of the coordinates but is not
C additive.
C
C On input:
C
C       N = Number of data points.  N .GE. 3.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values asso-
C           ciated with the abscissae.  Y(N) is set to Y(1)
C           on output unless IER = 1.
C
C   Input parameters, other than Y(N), are not altered by
C this routine.
C
C On output:
C
C       YP = Array of length N containing estimated deriv-
C            atives at the abscissae unless IER .NE. 0.
C            YP is not altered if IER = 1, and is only par-
C            tially defined if IER > 1.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N < 3.
C             IER = I if X(I) .LE. X(I-1) for some I in the
C                     range 2,...,N.
C
C Reference:  J. M. Hyman, "Accurate Monotonicity-preserving
C               Cubic Interpolation",  LA-8796-MS, Los
C               Alamos National Lab, Feb. 1982.
C
C Modules required by PYC1P:  None
C
C Intrinsic functions called by PYC1P:  ABS, DMAX1, DMIN1,
C                                         SIGN
C
C***********************************************************
C
      INTEGER I, NM1
C     REAL    ASI, ASIM1, DXI, DXIM1, SGN, SI, SIM1, T
C
      NM1 = N - 1
      IF (NM1 .LT. 2) GO TO 2
      Y(N) = Y(1)
C
C Initialize for loop on interior points.
C
      I = 1
      DXI = X(2) - X(1)
      IF (DXI .LE. 0.) GO TO 3
      SI = (Y(2)-Y(1))/DXI
C
C YP(I) = (DXIM1*SI+DXI*SIM1)/(DXIM1+DXI) subject to the
C   constraint that YP(I) has the sign of either SIM1 or
C   SI, whichever has larger magnitude, and abs(YP(I)) .LE.
C   3*min(abs(SIM1),abs(SI)).
C
      DO 1 I = 2,NM1
        DXIM1 = DXI
        DXI = X(I+1) - X(I)
        IF (DXI .LE. 0.) GO TO 3
        SIM1 = SI
        SI = (Y(I+1)-Y(I))/DXI
        T = (DXIM1*SI+DXI*SIM1)/(DXIM1+DXI)
        ASIM1 = DABS(SIM1)
        ASI = DABS(SI)
        SGN = DSIGN(1.0d0,SI)
        IF (ASIM1 .GT. ASI) SGN = DSIGN(1.0d0,SIM1)
        IF (SGN .GT. 0.) THEN
          YP(I) = DMIN1(DMAX1(0.D0,T), 3.*DMIN1(ASIM1,ASI))
        ELSE
          YP(I) = DMAX1(DMIN1(0.D0,T), -3.*DMIN1(ASIM1,ASI))
        ENDIF
    1   CONTINUE
C
C YP(N) = YP(1), I = 1, and IM1 = N-1.
C
      DXIM1 = DXI
      DXI = X(2) - X(1)
      SIM1 = SI
      SI = (Y(2) - Y(1))/DXI
      T = (DXIM1*SI + DXI*SIM1)/(DXIM1+DXI)
      ASIM1 = DABS(SIM1)
      ASI = DABS(SI)
      SGN = DSIGN(1.0d0,SI)
      IF (ASIM1 .GT. ASI) SGN = DSIGN(1.0d0,SIM1)
      IF (SGN .GT. 0.) THEN
        YP(1) = DMIN1(DMAX1(0.D0,T), 3.*DMIN1(ASIM1,ASI))
      ELSE
        YP(1) = DMAX1(DMIN1(0.D0,T), -3.*DMIN1(ASIM1,ASI))
      ENDIF
      YP(N) = YP(1)
C
C No error encountered.
C
      IER = 0
      RETURN
C
C N is outside its valid range.
C
    2 IER = 1
      RETURN
C
C X(I+1) .LE. X(I).
C
    3 IER = I + 1
      RETURN
      END SUBROUTINE PYC1P
C *****************************************************************************
      SUBROUTINE PYC2 (N,X,Y,SIGMA,ISL1,ISLN,BV1,BVN,
     .                 WK, YP,IER)
C****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N, ISL1, ISLN, IER
      DIMENSION    X(N), Y(N), SIGMA(N), WK(N), YP(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/14/91
C
C   This subroutine solves a linear system for a set of
C first derivatives YP associated with a Hermite interpola-
C tory tension spline H(x).  The derivatives are chosen so
C that H(x) has two continuous derivatives for all x and H
C satisfies user-specified end conditions.
C
C On input:
C
C       N = Number of data points.  N .GE. 2.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values asso-
C           ciated with the abscissae.  H(X(I)) = Y(I) for
C           I = 1,...,N.
C
C       SIGMA = Array of length N-1 containing tension
C               factors.  SIGMA(I) is associated with inter-
C               val (X(I),X(I+1)) for I = 1,...,N-1.  If
C               SIGMA(I) = 0, H is the Hermite cubic interp-
C               olant of the data values and computed deriv-
C               atives at X(I) and X(I+1), and if all
C               tension factors are zero, H is the C-2 cubic
C               spline interpolant which satisfies the end
C               conditions.
C
C       ISL1 = Option indicator for the condition at X(1):
C              ISL1 = 0 if YP(1) is to be estimated inter-
C                       nally by a constrained parabolic
C                       fit to the first three points.
C                       This is identical to the method used
C                       by Subroutine PYC1.  BV1 is not used
C                       in this case.
C              ISL1 = 1 if the first derivative of H at X(1)
C                       is specified by BV1.
C              ISL1 = 2 if the second derivative of H at
C                       X(1) is specified by BV1.
C              ISL1 = 3 if YP(1) is to be estimated inter-
C                       nally from the derivative of the
C                       tension spline (using SIGMA(1))
C                       which interpolates the first three
C                       data points and has third derivative
C                       equal to zero at X(1).  Refer to
C                       ENDSLP.  BV1 is not used in this
C                       case.
C
C       ISLN = Option indicator for the condition at X(N):
C              ISLN = 0 if YP(N) is to be estimated inter-
C                       nally by a constrained parabolic
C                       fit to the last three data points.
C                       This is identical to the method used
C                       by Subroutine PYC1.  BVN is not used
C                       in this case.
C              ISLN = 1 if the first derivative of H at X(N)
C                       is specified by BVN.
C              ISLN = 2 if the second derivative of H at
C                       X(N) is specified by BVN.
C              ISLN = 3 if YP(N) is to be estimated inter-
C                       nally from the derivative of the
C                       tension spline (using SIGMA(N-1))
C                       which interpolates the last three
C                       data points and has third derivative
C                       equal to zero at X(N).  Refer to
C                       ENDSLP.  BVN is not used in this
C                       case.
C
C       BV1,BVN = Boundary values or dummy parameters as
C                 defined by ISL1 and ISLN.
C
C The above parameters are not altered by this routine.
C
C       WK = Array of length at least N-1 to be used as
C            temporary work space.
C
C       YP = Array of length .GE. N.
C
C On output:
C
C       YP = Array containing derivatives of H at the
C            abscissae.  YP is not defined if IER .NE. 0.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N, ISL1, or ISLN is outside its
C                     valid range.
C             IER = I if X(I) .LE. X(I-1) for some I in the
C                     range 2,...,N.
C
C Modules required by PYC2:  ENDSLP, SNHCSH, PYCOEF
C
C Intrinsic function called by PYC2:  ABS
C
C***********************************************************
C
      INTEGER I, NM1, NN
C     REAL    D, D1, D2, DX, R1, R2, S, SD1, SD2, SIG, YP1,
C    .        YPN
C     REAL    ENDSLP
C
      NN = N
      IF (NN .LT. 2  .OR.  ISL1 .LT. 0  .OR.  ISL1 .GT. 3
     .    .OR.  ISLN .LT. 0  .OR.  ISLN .GT. 3) GO TO 3
      NM1 = NN - 1
C
C Set YP1 and YPN to the endpoint values.
C
      IF (ISL1 .EQ. 0) THEN
        IF (NN .GT. 2) YP1 = ENDSLP (X(1),X(2),X(3),Y(1),
     .                               Y(2),Y(3),0.d0)
      ELSEIF (ISL1 .NE. 3) THEN
        YP1 = BV1
      ELSE
        IF (NN .GT. 2) YP1 = ENDSLP (X(1),X(2),X(3),Y(1),
     .                               Y(2),Y(3),SIGMA(1))
      ENDIF
      IF (ISLN .EQ. 0) THEN
        IF (NN .GT. 2) YPN = ENDSLP (X(NN),X(NM1),X(NN-2),
     .                              Y(NN),Y(NM1),Y(NN-2),0.d0)
      ELSEIF (ISLN .NE. 3) THEN
        YPN = BVN
      ELSE
        IF (NN .GT. 2) YPN = ENDSLP (X(NN),X(NM1),X(NN-2),
     .                      Y(NN),Y(NM1),Y(NN-2),SIGMA(NM1))
      ENDIF
C
C Solve the symmetric positive-definite tridiagonal linear
C   system.  The forward elimination step consists of div-
C   iding each row by its diagonal entry, then introducing a
C   zero below the diagonal.  This requires saving only the
C   superdiagonal (in WK) and the right hand side (in YP).
C
      I = 1
      DX = X(2) - X(1)
      IF (DX .LE. 0.) GO TO 4
      S = (Y(2)-Y(1))/DX
      IF (NN .EQ. 2) THEN
        IF (ISL1 .EQ. 0  .OR.  ISL1 .EQ. 3) YP1 = S
        IF (ISLN .EQ. 0  .OR.  ISLN .EQ. 3) YPN = S
      ENDIF
C
C Begin forward elimination.
C
      SIG = ABS(SIGMA(1))
      CALL PYCOEF (SIG,DX, D1,SD1)
      R1 = (SD1+D1)*S
      WK(1) = 0.
      YP(1) = YP1
      IF (ISL1 .EQ. 2) THEN
        WK(1) = SD1/D1
        YP(1) = (R1-YP1)/D1
      ENDIF
      DO 1 I = 2,NM1
        DX = X(I+1) - X(I)
        IF (DX .LE. 0.) GO TO 4
        S = (Y(I+1)-Y(I))/DX
        SIG = DABS(SIGMA(I))
        CALL PYCOEF (SIG,DX, D2,SD2)
        R2 = (SD2+D2)*S
        D = D1 + D2 - SD1*WK(I-1)
        WK(I) = SD2/D
        YP(I) = (R1 + R2 - SD1*YP(I-1))/D
        D1 = D2
        SD1 = SD2
        R1 = R2
    1   CONTINUE
      D = D1 - SD1*WK(NM1)
      YP(NN) = YPN
      IF (ISLN .EQ. 2) YP(NN) = (R1 + YPN - SD1*YP(NM1))/D
C
C Back substitution:
C
      DO 2 I = NM1,1,-1
        YP(I) = YP(I) - WK(I)*YP(I+1)
    2   CONTINUE
      IER = 0
      RETURN
C
C Invalid integer input parameter.
C
    3 IER = 1
      RETURN
C
C Abscissae out of order or duplicate points.
C
    4 IER = I + 1
      RETURN
      END SUBROUTINE PYC2
C *****************************************************************************
      SUBROUTINE PYC2P (N,X,Y,SIGMA,WK, YP,IER)
C****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER N, IER
      DIMENSION X(N), Y(N), SIGMA(N), WK(*), YP(N)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This subroutine solves a linear system for a set of
C first derivatives YP associated with a Hermite interpola-
C tory tension spline H(x).  The derivatives are chosen so
C that H(x) has two continuous derivatives for all x, and H
C satisfies periodic end conditions:  first and second der-
C ivatives of H at X(1) agree with those at X(N), and thus
C the length of a period is X(N) - X(1).  It is assumed that
C Y(N) = Y(1), and Y(N) is not referenced.
C
C On input:
C
C       N = Number of data points.  N .GE. 3.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values asso-
C           ciated with the abscissae.  H(X(I)) = Y(I) for
C           I = 1,...,N.
C
C       SIGMA = Array of length N-1 containing tension
C               factors.  SIGMA(I) is associated with inter-
C               val (X(I),X(I+1)) for I = 1,...,N-1.  If
C               SIGMA(I) = 0, H is the Hermite cubic interp-
C               olant of the data values and computed deriv-
C               atives at X(I) and X(I+1), and if all
C               tension factors are zero, H is the C-2 cubic
C               spline interpolant which satisfies the end
C               conditions.
C
C The above parameters are not altered by this routine.
C
C       WK = Array of length at least 2N-2 to be used as
C            temporary work space.
C
C       YP = Array of length .GE. N.
C
C On output:
C
C       YP = Array containing derivatives of H at the
C            abscissae.  YP is not defined if IER .NE. 0.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N is outside its valid range.
C             IER = I if X(I) .LE. X(I-1) for some I in the
C                     range 2,...,N.
C
C Modules required by PYC2P:  SNHCSH, PYCOEF
C
C Intrinsic function called by PYC2P:  ABS
C
C***********************************************************
C
      INTEGER I, NM1, NM2, NM3, NN, NP1, NPI
C     REAL    D, D1, D2, DIN, DNM1, DX, R1, R2, RNM1, S,
C    .        SD1, SD2, SDNM1, SIG, YPNM1
C
      NN = N
      IF (NN .LT. 3) GO TO 4
      NM1 = NN - 1
      NM2 = NN - 2
      NM3 = NN - 3
      NP1 = NN + 1
C
C The system is order N-1, symmetric, positive-definite, and
C   tridiagonal except for nonzero elements in the upper
C   right and lower left corners.  The forward elimination
C   step zeros the subdiagonal and divides each row by its
C   diagonal entry for the first N-2 rows.  The superdiago-
C   nal is stored in WK(I), the negative of the last column
C   (fill-in) in WK(N+I), and the right hand side in YP(I)
C   for I = 1,...,N-2.
C
      I = NM1
      DX = X(NN) - X(NM1)
      IF (DX .LE. 0.) GO TO 5
      S = (Y(1)-Y(NM1))/DX
      SIG = DABS(SIGMA(NM1))
      CALL PYCOEF (SIG,DX, DNM1,SDNM1)
      RNM1 = (SDNM1+DNM1)*S
      I = 1
      DX = X(2) - X(1)
      IF (DX .LE. 0.) GO TO 5
      S = (Y(2)-Y(1))/DX
      SIG = DABS(SIGMA(1))
      CALL PYCOEF (SIG,DX, D1,SD1)
      R1 = (SD1+D1)*S
      D = DNM1 + D1
      WK(1) = SD1/D
      WK(NP1) = -SDNM1/D
      YP(1) = (RNM1+R1)/D
      DO 1 I = 2,NM2
        DX = X(I+1) - X(I)
        IF (DX .LE. 0.) GO TO 5
        S = (Y(I+1)-Y(I))/DX
        SIG = DABS(SIGMA(I))
        CALL PYCOEF (SIG,DX, D2,SD2)
        R2 = (SD2+D2)*S
        D = D1 + D2 - SD1*WK(I-1)
        DIN = 1./D
        WK(I) = SD2*DIN
        NPI = NN + I
        WK(NPI) = -SD1*WK(NPI-1)*DIN
        YP(I) = (R1 + R2 - SD1*YP(I-1))*DIN
        SD1 = SD2
        D1 = D2
        R1 = R2
    1   CONTINUE
C
C The backward elimination step zeros the superdiagonal
C   (first N-3 elements).  WK(I) and YP(I) are overwritten
C   with the negative of the last column and the new right
C   hand side, respectively, for I = N-2, N-3, ..., 1.
C
      NPI = NN + NM2
      WK(NM2) = WK(NPI) - WK(NM2)
      DO 2 I = NM3,1,-1
        YP(I) = YP(I) - WK(I)*YP(I+1)
        NPI = NN + I
        WK(I) = WK(NPI) - WK(I)*WK(I+1)
    2   CONTINUE
C
C Solve the last equation for YP(N-1).
C
      YPNM1 = (R1 + RNM1 - SDNM1*YP(1) - SD1*YP(NM2))/
     .        (D1 + DNM1 + SDNM1*WK(1) + SD1*WK(NM2))
C
C Back substitute for the remainder of the solution
C   components.
C
      YP(NM1) = YPNM1
      DO 3 I = 1,NM2
        YP(I) = YP(I) + WK(I)*YPNM1
    3   CONTINUE
C
C YP(N) = YP(1).
C
      YP(N) = YP(1)
      IER = 0
      RETURN
C
C N is outside its valid range.
C
    4 IER = 1
      RETURN
C
C Abscissae out of order or duplicate points.
C
    5 IER = I + 1
      RETURN
      END SUBROUTINE PYC2P
C *****************************************************************************
      SUBROUTINE PYCOEF (SIGMA,DX, D,SD)
C****************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     REAL SIGMA, DX, D, SD
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This subroutine computes the coefficients of the deriva-
C tives in the symmetric diagonally dominant tridiagonal
C system associated with the C-2 derivative estimation pro-
C cedure for a Hermite interpolatory tension spline.
C
C On input:
C
C       SIGMA = Nonnegative tension factor associated with
C               an interval.
C
C       DX = Positive interval width.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       D = Component of the diagonal term associated with
C           the interval.  D = SIG*(SIG*COSHM(SIG) -
C           SINHM(SIG))/(DX*E), where SIG = SIGMA and E =
C           SIG*SINH(SIG) - 2*COSHM(SIG).
C
C       SD = Subdiagonal (superdiagonal) term.  SD = SIG*
C            SINHM(SIG)/E.
C
C Module required by PYCOEF:  SNHCSH
C
C Intrinsic function called by PYCOEF:  EXP
C
C***********************************************************
C
C     REAL COSHM, COSHMM, E, EMS, SCM, SIG, SINHM, SSINH,
C    .     SSM
C
      SIG = SIGMA
      IF (SIG .LE. 0.) THEN
C
C SIG = 0:  cubic interpolant.
C
        D = 4.d00/DX
        SD = 2.d00/DX
      ELSEIF (SIG .LE. .5) THEN
C
C 0 .LT. SIG .LE. .5:  use approximations designed to avoid
C                      cancellation error in the hyperbolic
C                      functions when SIGMA is small.
C
        CALL SNHCSH (SIG, SINHM,COSHM,COSHMM)
        E = (SIG*SINHM - COSHMM - COSHMM)*DX
        D = SIG*(SIG*COSHM-SINHM)/E
        SD = SIG*SINHM/E
      ELSE
C
C SIG > .5:  scale SINHM and COSHM by 2*EXP(-SIG) in order
C            to avoid overflow when SIGMA is large.
C
        EMS = DEXP(-SIG)
        SSINH = 1. - EMS*EMS
        SSM = SSINH - 2.*SIG*EMS
        SCM = (1.-EMS)*(1.-EMS)
        E = (SIG*SSINH - SCM - SCM)*DX
        D = SIG*(SIG*SCM-SSM)/E
        SD = SIG*SSM/E
      ENDIF
      RETURN
      END SUBROUTINE PYCOEF
C********************************************************************
C     IVTMH
c
c     do ivtstm interpolation via hooks
c     called by : PATH, ELRPH
c     calls     : calcs, dpswap,pregpt,armuef1,ivtma,ivtmd
C********************************************************************
      subroutine ivtmh(level,ngpt)
      use common_inc
      use perconparam
      use rate_const
      use ivtst1; use cm
      use kintcm
      use keyword_interface, only : gufac6,iunit6
      implicit double precision (a-h,o-z)
c
      save
c      common /amfcm/ armcc(nsdm)
      dimension kkk(natoms),geor(n3tm),geop(n3tm),geow(n3tm)
c      dimension xlcd(nsdim),xhcd(nsdim)
      dimension ssmp(2,NPT31),ssvp(2,NPT31)
      logical   lanhar
c
      call rph40_mem
      call ivtst1_mem
      write (fu6,1998)
1998  format (1x,79(1H-)) 
      write (fu6,*) ' Mapped Interpolated VTST via hooks',
     >              ' IVTST-M is used to extend the MEP'
      write (fu6,1998)
      if (level.eq.1) then
c
c determine the direction of the branches
c
        if (isen.eq.-1) then
           idr1 = 1
           idr2 = 2
        endif
        if (isen.eq.1) then
           idr1 = 2
           idr2 = 1
        endif
c exponent of L for splinb
        write (fu6,*) ' *IVTSTMOPT'
        write (fu6,*) '  Power of L for spline Bmf = ', lbexp
        write (fu6,*) '  inm31 = ',inm31
        write (fu6,*) '  sincw = ', sincw/gufac6                        0405JZ07
c
c set up indecies for array manipulations
c
        ibeg = nste(idr1) + 1
        iend = lsave - nste(idr2)
        nss = iend - ibeg + 5
        issp = nshlf - ibeg + 3
        if (LGS(5).GT.0.OR.LGS(33).EQ.1) then
            lanhar = .true.
        else
            lanhar = .false.
        endif
        write (fu6,*) ' *Storage information'
        write (fu6,*) '  Number of save grid points (lsave)= ', lsave 
        write (fu6,*) '  Half point (nshlf)= ', nshlf
        write (fu6,*) '  SLM starts at (ibeg)= ', ibeg
        write (fu6,*) '  SLP ends at (iend)= ', iend
        write (fu6,*) '  Number of gradient points (nss)= ',nss
        write (fu6,*) '  Location of TS (issp)= ', issp
        write (fu6,*) '  Number of interpolated points (nsdim)= ',nsdim
        write (fu6,*) ' *Interpolation Information'                     0317YC99
        write (fu6,*) '  Number of frequencies (nf)= ',(nf(i),i=1,8)    0317YC99
        write (fu6,*) '  lgs(6) = ',lgs(6)                              0317YC99
        write (fu6,*) '  irepr = ',(irepr(i),i=1,8)                     0317Yc99
c
        nfrqr = nf(1) + nf(2)
        nfrqp = nf(3) + nf(4)
        nfreq = nf(5)
        ishft = n3 - nf(5)
c
c   set up the extra points for calculating distances in s
c
c   one reactant
c
        if (irepr(1).eq.1) then                                         0317YC99
        if (lgs(6).eq.3.or.lgs(6).eq.4.or.irepr(7).eq.1) then
           do i = 0,natom-1
             do j = 1, 3
               geor(3*i+j) = geom(3*i+j,ibeg)/amass(3*i+j)
             enddo
           enddo
           if (lbath) then
             geor(3*natom+1) = 0.0d0
           endif
        endif
        endif                                                           0317YC99
c
c   one product
c
        if (irepr(3).eq.1) then                                         0317YC99
        if (lgs(6).eq.2.or.lgs(6).eq.4.or.irepr(8).eq.1) then
           do i = 0,natom-1
              do j = 1, 3
                geop(3*i+j) = geom(3*i+j,iend)/amass(3*i+j)
              enddo
           enddo
           if (lbath) then
             geop(3*natom+1) = 0.0d0
           endif
        endif
        endif                                                           0317Yc99
c
c   for one reactant
c
        if (irepr(1).eq.1) then                                         0317YC99
        if (lgs(6).eq.3.or.lgs(6).eq.4) then
           do i = 0,nratom(1)-1
             do j = 1,3
               geow(3*i+j)=xr(3*i+j,1)
             enddo
           enddo
           if (lbath) then
             geor(3*nratom(1)+1) = 0.0d0
           endif
           call calcs(geow,geor,sr,kkk)
           sr = -1.0d0 * sr + ssubi(ibeg)
           ss(1) = sr
           write (fu6,*) '  Reactant location (sr)= ', ss(1)
           do i = 1, nfrqr-1
              ides = i+1
              ws(1,i) = wer(ides)
              fmirs(1,i) = fmihr(ides)
              IF (lanhar) THEN
                xk3s(1,i) = xer(ides)
                xk4s(1,i) = y00r(ides)
              ENDIF
           enddo
           if (lgs(34).ne.0) then                                       0317Yc99
              fmom(1) = 1.0E+30                                         0317Yc99
           endif                                                        0317YC99
           fmoms(1) = 1/fmom(1)
           if (lbath) then
             FRSOL = (PI/(4*FRICT))
             ws(1,nfrqr) = frsol
c
c sort frequencies in ascending order, to match with the
c saddle point
c
             do i = 1, nfrqr
                do j = i, nfrqr
                  if (ws(1,j).lt.ws(1,i)) then
                     call dpswap(ws(1,i),ws(1,j))
                     IF (lanhar) THEN
                         call dpswap(xk3s(1,i),xk3s(1,j))
                         call dpswap(xk4s(1,i),xk4s(1,j))
                     ENDIF
                  endif
                enddo
             enddo
           endif
c
c       for two reactants
c
        else
           do i = 1, nfrqr
              ws(1,i) = wer(i)
              fmirs(1,i) = fmihr(i)
              IF (lanhar) THEN
                xk3s(1,i) = xer(i)
                xk4s(1,i) = y00r(i)
              ENDIF
           enddo
           if (lbath) then                                              0317YC99
             FRSOL = (PI/(4*FRICT))                                     0317Yc99
             ws(1,nfrqr+1) = frsol                                      0317YC99
           endif                                                        0317Yc99
c
c sort frequencies in ascending order, to match with the
c saddle point
c
           do i = 1, nfreq
              do j = i, nfreq
                if (ws(1,j).lt.ws(1,i)) then
                   call dpswap(ws(1,i),ws(1,j))
                   IF (lanhar) THEN
                       call dpswap(xk3s(1,i),xk3s(1,j))
                       call dpswap(xk4s(1,i),xk4s(1,j))
                   ENDIF
                endif
              enddo
           enddo
           fmoms(1) = 0.0d0
        endif
        endif                                                           0317Yc99
c
c  for one product
c
        if (irepr(3).eq.1) then                                         0317Yc99
        if (lgs(6).eq.2.or.lgs(6).eq.4) then
           do i = 0,nratom(3)-1
             do j = 1,3
               geow(3*i+j)=xr(3*i+j,3)
             enddo
           enddo
           call calcs(geow,geop,sp,kkk)
           sp = sp + ssubi(iend)
           ss(nsdim) = sp
           write (fu6,*) '  Product location (sp)= ', ss(nsdim)
           do i = 1, nfrqp-1
             ides = i+nfrqr+1
             ws(nsdim,i) = wer(ides)
             fmirs(nsdim,i) = fmihr(ides)
               IF (lanhar) THEN
                 xk3s(nsdim,i) = xer(ides)
                 xk4s(nsdim,i) = y00r(ides)
               ENDIF
           enddo
           if (lgs(34).ne.0) then                                       0317Yc99
              fmom(3) = 1.0E+30                                         0317Yc99
           endif                                                        0317Yc99
           fmoms(nsdim) = 1/fmom(3)                                     0317Yc99
           if (lbath) then                                              0317Yc99
             FRSOL = (PI/(4*FRICT))                                     0317Yc99
             ws(nsdim,nfrqp) = frsol                                    0317Yc99
c
c sort in ascending order, to match the frequencies of saddle           0317YC99
c point
c
             do i = 1, nfrqp                                            0317Yc99
               do j = i, nfrqp                                          0317Yc99
                 if (ws(nsdim,j).lt.ws(nsdim,i)) then                   0317Yc99
                    call dpswap(ws(nsdim,i),ws(nsdim,j))                0317Yc99
                    IF (lanhar) THEN                                    0317Yc99
                       call dpswap(xk3s(nsdim,i),xk3s(nsdim,j))         0317Yc99
                       call dpswap(xk4s(nsdim,i),xk4s(nsdim,j))         0317Yc99
                    ENDIF                                               0317Yc99
                 endif                                                  0317Yc99
               enddo                                                    0317Yc99
             enddo                                                      0317Yc99
           endif                                                        0317Yc99
c
c       for two products
c
        else
           do i = 1, nfrqp
             ides = i + nfrqr
             ws(nsdim,i) = wer(ides)
             fmirs(nsdim,i) = fmihr(ides)
             IF (lanhar) THEN
               xk3s(nsdim,i) = xer(ides)
               xk4s(nsdim,i) = y00r(ides)
             ENDIF
           enddo
           if (lbath) then                                              0317Yc99
             FRSOL = (PI/(4*FRICT))                                     0317Yc99
             ws(nsdim,nfrqp+1) = frsol                                  0317Yc99
           endif                                                        0317Yc99
c
c sort in ascending order, to match the frequencies of saddle
c point
c
           do i = 1, nfreq
             do j = i, nfreq
               if (ws(nsdim,j).lt.ws(nsdim,i)) then
                  call dpswap(ws(nsdim,i),ws(nsdim,j))
                  IF (lanhar) THEN
                     call dpswap(xk3s(nsdim,i),xk3s(nsdim,j))
                     call dpswap(xk4s(nsdim,i),xk4s(nsdim,j))
                  ENDIF
               endif
             enddo
           enddo
           fmoms(nsdim) = 0.0d0
        endif
        endif                                                           0317Yc99
c
c  for reactant well
c
        if (irepr(7).eq.1) then
           do i = 0,nratom(7)-1
             do j = 1,3
               geow(3*i+j)=xr(3*i+j,7)
             enddo
           enddo
           if (lbath) then
              geow(3*nratom(7)+1) = 0.0d0
           endif
           call calcs(geow,geor,swr,kkk)
           swr = -1.0d0 * swr + ssubi(ibeg)
           ss(2) = swr
           write (fu6,*) '  Reactant well location (swr)= ', ss(2)
           do i = 1, nf(7)-1
             ides = i + 1
             ws(2,i) = wew(ides)
             IF (lanhar) THEN
               xk3s(2,i) = xew(ides)
               xk4s(2,i) = y0w(ides)
             ENDIF
           enddo
           if (lgs(34).ne.0) then                                       0317Yc99
              fmom(7) = 1.0E+30                                         0317Yc99
           endif                                                        0317Yc99
           fmoms(2) = 1.0d0/fmom(7)
           if (lbath) then                                              0317Yc99
             FRSOL = (PI/(4*FRICT))                                     0317Yc99
             ws(2,nf(7)) = frsol                                        0317Yc99
c
c sort in ascending order, to match the frequencies of saddle
c point
c
             do i = 1, nf(7)                                            0317Yc99
               do j = i, nf(7)                                          0317Yc99
                 if (ws(2,j).lt.ws(2,i)) then                           0317Yc99
                    call dpswap(ws(2,i),ws(2,j))                        0317Yc99
                    IF (lanhar) THEN                                    0317Yc99
                       call dpswap(xk3s(2,i),xk3s(2,j))                 0317Yc99
                       call dpswap(xk4s(2,i),xk4s(2,j))                 0317Yc99
                    ENDIF                                               0317YC99
                 endif                                                  0317YC99
               enddo                                                    0317Yc99
             enddo                                                      0317Yc99
           endif                                                        0317Yc99
        endif
c
c   for product well
c
        if (irepr(8).eq.1) then
           do i = 0,nratom(8)-1
             do j = 1,3
               geow(3*i+j)=xr(3*i+j,8)
             enddo
           enddo
           if (lbath) then
             geow(3*nratom(8)+1) = 0.0d0
           endif
           call calcs(geow,geop,swp,kkk)
           swp = swp + ssubi(iend)
           ss(nsdim-1) = swp
           write (fu6,*) '  Product well location (swp)= ', ss(nsdim-1)
           do i = 1, nf(8)-1
             ides = i + nf(7) + 1
             ws(nsdim-1,i) = wew(ides)
             IF (lanhar) THEN
               xk3s(nsdim-1,i) = xew(ides)
               xk4s(nsdim-1,i) = y0w(ides)
             ENDIF
           enddo
           if (lgs(34).ne.0) then                                       0317Yc99
              fmom(8) = 1.0E+30                                         0317Yc99
           endif                                                        0317Yc99
           fmoms(nsdim-1) = 1.0d0/fmom(8)
           if (lbath) then                                              0317Yc99
             FRSOL = (PI/(4*FRICT))                                      ..
             ws(nsdim-1,nf(8)) = frsol                                   ..
c
c sort in ascending order, to match the frequencies of saddle            ..
c point
c
             do i = 1, nf(8)                                             ..
               do j = i, nf(8)                                           ..
                 if (ws(nsdim-1,j).lt.ws(nsdim-1,i)) then                ..
                    call dpswap(ws(nsdim-1,i),ws(nsdim-1,j))             ..
                    IF (lanhar) THEN                                     ..
                       call dpswap(xk3s(nsdim-1,i),xk3s(nsdim-1,j))      ..
                       call dpswap(xk4s(nsdim-1,i),xk4s(nsdim-1,j))      ..
                    ENDIF                                                ..
                 endif                                                   ..
               enddo                                                     ..
             enddo                                                       ..
           endif                                                        0317YC99
        endif
c
c   set up arrays for calling ivtstm routines
c
        xmfr = wew(1)
        xmfp = wew(nf(7)+1)
c
        vs(1) = 0.0d0
        vs(nsdim) = EPRD
        vs(2) = EWR
        vs(nsdim-1) = EWP
        vs(nsdim-2) = vclas(nshlf)
        if (lgs(34).ne.0) then                                          0317Yc99
          fmom(5) = 1.0E+30                                             0317Yc99
        endif                                                           0317Yc99
        fmoms(nsdim-2) = 1/fmom(5)
        do i = 1, nf(5)
           ws(nsdim-2,i) = wets(i,nshlf)
           IF (lanhar) THEN
             xk3s(nsdim-2,i) = xets(i,nshlf)
             xk4s(nsdim-2,i) = y0ts(i,nshlf)
           ENDIF
        enddo
c
c up date data if there is no product or product well                   0317Yc99
c take the last point on the path as product                            ..
c
        if (irepr(3).ne.1.and.irepr(8).ne.1) then                       ..
          write (fu6,*) '  No product found, use the last point',       ..
     >       ' in the reaction path instead'                            ..
          ss(nsdim)=ssubi(iend)                                         ..
          vs(nsdim)=vclas(iend)                                         ..
          do j = 1, nfreq                                               ..
            ws(nsdim,j) = wets(j,iend)                                  ..
            IF (lanhar) THEN                                            ..
              xk3s(nsdim,j) = xets(j,iend)                              ..
              xk4s(nsdim,j) = y0ts(j,iend)                              ..
            ENDIF                                                       ..
            fmirs(nsdim,j)= FMIHTS(j,iend)                              ..
            bfs(nsdim,j) = bcur(j,iend)                                 ..
          enddo                                                         ..
          if (lgs(34).ne.0) then                                        ..
            fmits(iend) = 1.0E+30                                       ..
          endif                                                         ..
          fmoms(nsdim) = 1/fmits(iend)                                  ..
c          xlcd(nsdim) = xlcdsc(iend)                                   .. 
c          xhcd(nsdim) = xhcdsc(iend)                                   ..
        endif                                                           ..
        write (fu6,*)                                                   ..
        write (fu6,*) '     REACTANT  ',' REACTANTW ',' SADDLE PT ',    ..
     >                '   PRODUCTW  ','  PRODUCT  '                     ..
        write (fu6,*) '     --------  ',' --------- ',' --------- ',    ..
     >                '   --------  ','  -------  '                     ..
  6     format (5F12.3)                                                 ..
C       write (fu6,*) '  S (bohr)'                                      ..
        if(iunit6.eq.0) write (fu6,*) '  S (angstrom)'                  0405JZ07
        if(iunit6.eq.1) write (fu6,*) '  S (bohr)'                      0405JZ07
C       write (fu6,6) ss(1),ss(2),ss(nsdim-2),ss(nsdim-1),ss(nsdim)     ..
        write (fu6,6) ss(1)/gufac6,ss(2)/gufac6,ss(nsdim-2)/gufac6,
     >                ss(nsdim-1)/gufac6,ss(nsdim)/gufac6               0405JZ07
        write (fu6,*) '  V (kcal/mol)'                                  ..
        write (fu6,6) vs(1)*CKCAL,vs(2)*CKCAL,vs(nsdim-2)*CKCAL,        ..
     >                vs(nsdim-1)*CKCAL,vs(nsdim)*CKCAL                 ..
        write (fu6,*) '  Vibrational frequencies (cm-1)'                ..
        do i = 1,nfreq                                                  .. 
         write (fu6,6) ws(1,i)*AUTOCM,ws(2,i)*AUTOCM,                   ..
     >                 ws(nsdim-2,i)*AUTOCM,                            ..
     >                 ws(nsdim-1,i)*AUTOCM,ws(nsdim,i)*AUTOCM          ..
        enddo                                                           ..
        write (fu6,*) '  Moments of Inertia in a.u.'                    ..
  8     format (5E12.5)                                                 ..
        write (fu6,8) fmoms(1),fmoms(2),fmoms(nsdim-2),fmoms(nsdim-1),  ..
     >                fmoms(nsdim)                                      0317YC99
c
c   for points along the path
c
        L = 3
        do i = ibeg, iend
          ss(L)=ssubi(i)
          vs(L)=vclas(i)
          do j = 1, nfreq
            ws(L,j) = wets(j,i)
            IF (lanhar) THEN
              xk3s(L,j) = xets(j,i)
              xk4s(L,j) = y0ts(j,i)
            ENDIF
            fmirs(L,j)= FMIHTS(j,i)
            bfs(L,j) = bcur(j,i)
          enddo
          if (lgs(34).ne.0) then                                        0317Yc99
            fmits(i) = 1.0E+30                                          0317Yc99
          endif                                                         0317Yc99
          fmoms(L) = 1/fmits(i)                                         0317YC99
c          xlcd(L) = xlcdsc(i)
c          xhcd(L) = xhcdsc(i)
          L = L + 1
        enddo
c
c  attatch to the end of the pat
c
        ss(NSS-1) = ss(NSDIM-1)
        vs(NSS-1) = vs(NSDIM-1)
        fmoms(NSS-1) = fmoms(NSDIM-1)
        ss(NSS) = ss(NSDIM)
        vs(NSS) = vs(NSDIM)
        fmoms(NSS) = fmoms(NSDIM)
        do i = 1, nfreq
          ws(nss,i) = ws(nsdim,i)
          ws(nss-1,i) = ws(nsdim-1,i)
          IF (lanhar) then
            xk3s(nss,i) = xk3s(nsdim,i)
            xk4s(nss,i) =xk4s(nsdim,i)
            xk3s(nss-1,i) = xk3s(nsdim-1,i)
            xk4s(nss-1,i) = xk4s(nsdim-1,i)
          ENDIF
        enddo
c
c gradient points for s,v, and 1/I
c
        call pregpt (ngpt,ismsp,isvsp,vclas(nshlf),fmits(nshlf),
     *             ssmp,ssvp)
c
c spline fit for the extra points
c
        IF(LGS(3) .EQ. 3) THEN
          IOP = 1
        ELSE
          IOP = 3
        ENDIF
c
c  update first direction
c  
c        if (iexrct.eq.1) then
          call ivtmd (1,nste(idr1),ibeg,iend,isvsp,ishft,nfreq,
     *                  ngpt,swr,sr,xmfr,xmfp,ssvp,ssmp)
c        endif
c
c  update second direction
c
c        if (iexprd.eq.1) then
          call ivtmd (lsave-nste(idr2)+1,lsave,ibeg,iend,isvsp,
     *                  ishft,nfreq,ngpt,swp,sp,xmfr,xmfp,ssvp,ssmp)
c        endif
      endif
c end of level = 1
c=======
c
c  MUCDSC, obtain the exponent
c
       if (LCDSC) call armuef1(nshlf)
c
c  obtain cdscmu or zocmcd
c
c   correct the mucdsc along the reaction path, so the points nearby saddle point
c   is smooth, this might change the result a little
c
      IF (ifxmf.EQ.1) THEN                                              0317Yc99
        call ivtma (1,lsave,level,redm,                                 ..
     *            cdscmu,zocmcd,armcc)                                  ..
      ELSE                                                              ..
          call ivtma (1,nste(idr1)+1,level,redm,                        ..
     *            cdscmu,zocmcd,armcc)                                  ..
          call ivtma (lsave-nste(idr2),lsave,level,redm,                ..
     *            cdscmu,zocmcd,armcc)                                  .. 
      ENDIF                                                             0317YC99
c
      do i = 1, lsave
        sum =0.0
        do j = 1,nfreq
          sum = sum+ bcur(j,i)*bcur(j,i)
        enddo
        sbkap(i) = dsqrt(sum)
      enddo
      write (fu6,1998)
 7    format (1x,F7.4,9E15.4)
      return
      end subroutine ivtmh
c  
C *****************************************************************************
      subroutine pregpt (ngpt,ismsp,isvsp,vsp,asp,
     *             ssmp,ssvp)
      use perconparam
      use rate_const
      use common_inc
C *****************************************************************************
      implicit double precision (A-H,O-Z)
c
c    this is not the optimum routine, bubble sort takes too much time
c    need to figure out how to shift without sorting
c
      dimension ssmp(2,NPT31),ssvp(2,NPT31)
c     if is a symmetric path, then copy the one just followed and make
c     up the whole path
c
      if (lgs(3).lt.0.0d0) then
        do i = 1,ngpt
          save31(1,ngpt+i) = -save31(1,i)
          save31(2,ngpt+i) = save31(2,i)
          save31(3,ngpt+i) = save31(3,i)
        enddo
        ngpt = ngpt + ngpt
      endif
      ngpt = ngpt + 1
      save31(1,ngpt) = 0.0d0
      save31(2,ngpt) = vsp
      save31(3,ngpt) = 1.0d0/asp
      do i = 1, ngpt
        do j = i, ngpt
          if (save31(1,j).lt.save31(1,i)) then
            do k = 1,3
              call dpswap(save31(k,i),save31(k,j))
            enddo
          endif
        enddo
      enddo
      do i = 1, ngpt
         ssmp(1,i) = save31(1,i)
         ssmp(2,i) = save31(3,i)
         ssvp(1,i) = save31(1,i)
         ssvp(2,i) = save31(2,i)
         if (save31(1,i).eq.0.0d0) then
            ismsp = i
            isvsp = i
         endif
      enddo
      return
      end subroutine pregpt
C *****************************************************************************
      subroutine ivtma (ibeg,iend,level,redm,cdscmu,zocmcd,armcc)
      use perconparam
C *****************************************************************************
c     this subroutine uses the interpolated expotent and calculate the
c     muCDSC along path, only extrapolated point are being updated
c
      implicit double precision (a-h,o-z)
      dimension cdscmu(nsdm),zocmcd(nsdm),armcc(nsdm)
      save

      do i = ibeg,iend
          PROD=1.D0
          IF (armcc(i).LT.0.D0) PROD=PROD*EXP(armcc(i))
          if (level.eq.1) then
            cdscmu(i)=REDM*PROD
          elseif (level.eq.2) then
            zocmcd(i)=REDM*PROD
c  in zocupd, the muCDSC has been copied from the array zocmcd
c  since the routine IVTMH is called after this rearrangement,
c  we need to copy the new values in order to have correct
c  rate calculations
            cdscmu(i)=zocmcd(i)
          endif
      enddo
      return
      end subroutine ivtma
C *****************************************************************************
      subroutine ivtmd (ista,ifin,ibeg,iend,isvsp,ishft,nfreq,ngpt,
     *                  swel,sur,xmfr,xmfp,ssvp,ssmp)
      use common_inc
      use perconparam
      use rate_const; use cm; use kintcm
C *****************************************************************************
c     this subroutine use calculates values for the extrapolated
c     points
c
      implicit double precision (a-h,o-z)
      dimension ssmp(2,NPT31),ssvp(2,NPT31)
      save
c
      ssta = 0.0d0
      if (swel.ne.0) ssta = swel
      if (sur.ne.0.and.swel.eq.0) ssta = sur
      if (ssta.lt.ssubi(ista).and.ista.eq.1) ssta=0.0d0
      if (ssta.gt.ssubi(ifin).and.ista.ne.1) ssta=0.0d0 
      if (ssta.ne.0) then
        if (ista.eq.1) then
          snstp = (ssubi(ibeg)-ssta)/(ifin-ista+1)
          sstart = ssta
        else
          sstart = ssubi(ista-1)
          snstp = (ssta-sstart)/(ifin-ista+1)
        endif
        write (fu6,*)
        write (fu6,*) ' *The range of extrapolation has been modified'
        if (ista.eq.1) then
          write (fu6,17) ssubi(ibeg),ssta,snstp
        else
          write (fu6,18) ssubi(iend),ssta,snstp
        endif
 17    format ('   Toward reactant direction, from ',
     >        F7.4,' to ',F7.4,' with step ',F7.4)
 18    format ('   Toward product direction, from ',
     >        F7.4,' to ',F7.4,' with step ',F7.4)
      endif 
c
      do i = ista,ifin
         if (ssta.ne.0) then
            if (ista.eq.1) then
               ssubi(i) = sstart + (i-1) * snstp
            else
               ssubi(i) = sstart + (i-ista+1) * snstp
            endif
         endif
c
c    spline fit vmep
c
          call splv31(ngpt,nss,isvsp,issp,wstar,redm,ssvp,ss,
     *                    vs,ssubi(i),vclas(i),lgs(6),
     *                    xmfr,xmfp,inm31,sincw,irepr)
c
c
c just for the save grid
c         call splnv(nss,issp,wstar,redm,ss,vs,ssubi(i),vclas(i),
c     *                    lgs(6),xmfr,xmfp,irepr,tension)
c
         do j = 1, n3tm
            freq(j) = 0.0d0
         enddo
         sum = 0.0d0
         do j=1,nfreq

c
c    spline fit frequencies
c
           call splnf(nss,wstar,redm,ss,vs,ws,ssubi(i),wets(j,i),
     *                 j,lgs(6),irepr,tension)
           freq(j+ishft)= wets(j,i)

c
c    spline fit the cubic and quartic force constants for anharmonicity
c
           IF (LGS(5).GT.0.OR.LGS(33).EQ.1) then
             call splnf(nss,wstar,redm,ss,vs,xk3s,ssubi(i),xets(j,i),
     *             j,lgs(6),irepr,tension)
             anhrm(j) = xets(j,i)
             call splnf(nss,wstar,redm,ss,vs,xk4s,ssubi(i),y0ts(j,i),
     *             j,lgs(6),irepr,tension)
           ENDIF

c
c    spline fit the reduced moment of inertia
c
           IF (LGS(5).GE.21.AND.MODE(J).EQ.9) THEN
              CALL splrmi(nss,wstar,redm,ss,vs,fmirs,ssubi(i),
     *                 FMIHTS(j,i),j,lgs(6),irepr,tension)
           ENDIF

c
c    spline fit the curvature components
c

           call splnb(nss,wstar,redm,ss,vs,bfs,ssubi(i),bcur(j,i),
     *                  j,lgs(6),irepr,lbexp,tension)
           sum = sum + bcur(j,i)*bcur(j,i)
         enddo
         sbkap(i) = dsqrt(sum)
c
         v = vclas(i)
         call zeropt(iop)
         vadib(i) = vad
c
c    spline fit the moment of inertia
c
            call splm31(ngpt,nss,wstar,redm,ss,vs,fmoms,
     *                    ssmp,ssubi(i),tt,LGS(6),irepr)
c            call splnm(nss,wstar,redm,ss,vs,fmoms,ssubi(i),tt,
c     *                     LGS(6),irepr,tension)
         fmits(i) = 1.0d0/tt
c
c    spline fit the exponent for mucdsc
c

c        if (level.eq.1) then
c         call splnxg(nss,wstar,redm,ss,vs,xlcd,ssubi(i),cdscmu(i),
c     *                     lgs(6),irepr)
c        elseif (level.eq.2) then
c         call splnxg(nss,wstar,redm,ss,vs,xhcd,ssubi(i),zocmcd(i),
c     *                     lgs(6),irepr)
c        endif
c
      enddo
c
      return
      end subroutine ivtmd
c
C *****************************************************************************
c      SUBROUTINE splnxg(nss,vim,redm,si,vi,armcsc,t,rv,lgs6,irepr)
C****************************************************************************
c      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c      INCLUDE 'param.inc'
c      INCLUDE 'percon.inc'
c      DIMENSION SI(nsdim),VI(nsdim),S(nsdim),V(nsdim),SA(nsdim)
c      DIMENSION ARMCSC(nsdim),IREPR(8),WFSP(nsdim)
cC
c      ALIM=1.0D-7
cC
c      V1=0.D0
c      V0=VI(NSDIM-2)
c      V2=VI(NSDIM)
c      S(1) = SI(1)
c      V(1) = VI(1)
c      WFSP(1)=0.D0
c      if (irepr(7).eq.1) then
c            v1=vi(2)
c            V(1)=vi(2)
c            s(1)=si(2)
c     endif
c     IBEG=3
c      IEND=NSS-2-1
c      IDIFF1=1
c      IDIFF2=0
c      INC=0
c     DO I=IBEG,IEND
cC
cC     Eliminate the saddle point (mueff no calculated)
cC
c         IF (ABS(SI(i)).LT.ALIM) INC=1
c         S(i-IDIFF1)=SI(i+INC)
c         V(I-IDIFF1)=VI(I+INC)
c         WFSP(i-IDIFF1)=ARMCSC(i+INC)
c      ENDDO
c      SI(IEND+IDIFF2)=S(NSS)
c      WFSP(IEND+IDIFF2)=0.D0
c      if (irepr(8).eq.1) then
c            v2=vi(nsdim-1)
c            s(iend+idiff2)=si(nsdim-1)
c            v(iend+idiff2)=vi(nsdim-1)
c      endif
cC
c      M=IEND+IDIFF2
cC
Cc     Calculate S0 and TL
cC
c      A = (V0 - V1)
c      B = (V0 - V2)
c      WK=(VIM)**2.D0*REDM
c      AL1=dsqrt(A/WK)
c      AL2=dsqrt(B/WK)
c      AL1=DMIN1(AL1,2.D0*AL2)
c      AL2=DMIN1(AL2,2.D0*AL1)
c      TL=(AL1 + AL2)/2.D0
c      S0=(-AL1+AL2)/2.D0
cC
cC     Values for the asymptotes:
cC
c      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
c     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
c                SA(1)=-1.D0
c      ELSE
c                SA(1)=2./PI*ATAN((S(1)-S0)/TL)
c      ENDIF
c      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
c     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
c                SA(M)=1.D0
c      ELSE
c                SA(M)=2./PI*ATAN((S(M)-S0)/TL)
c      ENDIF
cC
cC     We look for the saddle point and calculate SA
cC
c      DO i=2,M-1
c        IF (ABS(S(i)).LT.ALIM) NSP=i
c        SA(i)=2./PI*ATAN((S(i)-S0)/TL)
c      ENDDO
C
C     Calculate SA for the value of S given and make the interpolation.
C
c        TA=2./PI*ATAN((T-S0)/TL)
c        CALL SPL(SA,WFSP,M,TA,RV,5)
c        PROD=1.D0
c        IF (RV.LT.0.D0) PROD=PROD*EXP(RV)
c        RV=REDM*PROD
cC
c      RETURN
c      END
c
c spline fit mueffective directly
c
c      SUBROUTINE spmuef (NSS,WSTAR,REDM,SS,VS,BFS,S,MUCDF,
c     *                  I,LGS6,IREPR,LBEXP,NFREQ,WETS,BCUR,LCOUNT)
c      implicit double precision(a-h,o-z)
c      include 'param.inc'
c      include 'percon.inc'
c      dimensions SS(nsdim),VS(nsdim),BCUR(nvibm,nsdm),
c     *     IREPR(8),BCRV(nvibm),FRQ(nvibm),BFS(nvibm,nsdim)
c      do j = 1,nfreq
c        call splnb(nss,wstar,redm,ss,vs,bfs,s,bcur(j,i),
c     *                        i,lgs6,irepr,lbexp)
c      enddo
c      do j=1,nvibm
c         bcrv(j)=bcur(i,j)
c         frq(j)=wets(i,j)
c      enddo
c      call rphmef(n3m7,n3,0,lcount,frq,bcrv,redm,ss,xarg,xad)
c      PROD=1.D0
c      IF (XARG.LT.0.D0) PROD=PROD*EXP(XARG)
c      MUCDF=REDM*PROD
c      RETURN
c      END
C
C *****************************************************************************
       subroutine armuef1(issp1)
       use common_inc
       use perconparam
       use ivtst1
       use rate_const
C *****************************************************************************
C
C  Calculate the exponent in the mueff ecuation for the
c  extrapolated points, this is a duplicated copy (with few changes) of the
c  subroutine armuef (which is used for the IVTSTM via files option).
c  however, since the arrays are different and it is better to leave by itself.
c
c  Called by IVTMH
C  Calls rphmef
C
       implicit double precision (a-h,o-z)
C
       dimension frq(n3tm),bcrv(nvibm),tmparm(nsdim)
C
C  Only goes through this subroutine the first time it is called
C
       do i=1,nsdm
          armcc(i)=0.0d0
       enddo
c
       n3m7=nf(5)
C
C  We will calculate the curvature components for the saddle point by
C  linear interpolation, but we will only use them if necessary
C
      t = -ssubi(issp1-1)/(ssubi(issp1+1)-ssubi(issp1-1))
      do i = 1, nvibm
         bcur(i,issp1) = (1.0d0-t)*dabs(bcur(i,issp1-1)) +
     *                  t*dabs(bcur(i,issp+1))
      enddo
C
C  First we will start from the saddle point (ISSP1) going downhill to
C  reactants and then to products, using also the first point in the
C  opposite direction.
C
       iendr= 1
       iendp= lsave
c       iendr = 3
c       iendp = lsave - 2
       lcount=0
       do 10 i=issp1+1,iendr+1,-1
C
C  If we have information about more than one point at this side of the
C  reaction path, we will not use the saddle point information (since
C  the curvature is an interpolated value, not calculated)
C
          if (i.eq.issp1) then
             ilim=issp1-iendr+1
             if (ilim.ge.3) goto 10
          endif
          st=ssubi(i)
          do j=1,nvibm
              bcrv(j)=bcur(j,i)
              frq(j)=wets(j,i)
          enddo
          lcount=lcount+1
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,xad)
          tmparm(lcount)=xarg
          if (lcount.eq.3) tmparm(2)=xad
10     continue
          st=ssubi(iendr)
          do j=1,nvibm
              bcrv(j)=bcur(j,iendr)
              frq(j)=wets(j,iendr)
          enddo
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,dum)
          tmparm(lcount+1)=xarg
C
C  Now reorder the information so that it matches the right order
C
          j=0
          do i=iendr,issp1
              armcc(i)=tmparm(lcount+2-j)
              j=j+1
          enddo
C
C  Now calculate the product side
C
       lcount=0
       do 20 i=issp1-1,iendp-1
         if (i.eq.issp1) then
             ilim=issp1-iendp+1
             if (ilim.ge.3) goto 20
          endif
          st=ssubi(i)
          do j=1,nvibm
              bcrv(j)=bcur(j,i)
              frq(j)=wets(j,i)
          enddo
          lcount=lcount+1
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,xad)
          tmparm(lcount)=xarg
          if (lcount.eq.3) tmparm(2)=xad
20     continue
          st=ss(iendp)
          do j=1,nvibm
              bcrv(j)=bcur(j,iendp)
              frq(j)=wets(j,iendp)
          enddo
          call rphmef(n3,0,lcount,frq,bcrv,redm,st,xarg,xad)
          tmparm(lcount+1)=xarg
C
          j=0
          do i=iendp,issp1,-1
                armcc(i)=tmparm(lcount+2-j)
                j=j+1
          enddo
C
       lcount=0
       return
       end subroutine armuef1
c***********************************************************************
c     rivtm
c***********************************************************************
c
      subroutine rivtm(string,istrt)
      use common_inc
      use perconparam
      use keyword_interface, only : gufac5
      use rate_const
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      call rline(fu5,string,istrt,isect,iend)
C
C     Starting version 2010, default is set to polyz
C     
      inm31=2

      do while(string(istrt:istrt+2).ne.'end')
c
c POLYS   
        if (string(istrt:istrt+4).eq.'polys') then
             inm31=1
c POLYZ
        else if (string(istrt:istrt+4).eq.'polyz') then
             inm31=2
c NOPOLYS
        else if (string(istrt:istrt+6).eq.'nopolys') then               0601YC98
             inm31=3
c SINCW
        else if (string(istrt:istrt+4).eq.'sincw') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: sincw variable must have an argument'
               stop
             else
               sincw = cfloat(string(istrt:80))*gufac5
             end if
c LBEXP
        else if (string(istrt:istrt+4).eq.'lbexp') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: lbexp variable must have an argument'
               stop
             else
               lbexp = icint(string(istrt:80))
               if (mod(lbexp,2).eq.0) then
                write(fu6,*)'ERROR: lbexp must be an integer odd number'
               endif
             end if
c FIXMUEF
        else if (string(istrt:istrt+6).eq.'fixmuef') then               0317Yc99
             ifxmf = 1                                                  0317Yc99
c TENSION
        else if (string(istrt:istrt+6).eq.'tension') then               0911JZ08
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: tension variable must have an ',
     *                     'argument'
               stop
             else
               tension=cfloat(string(istrt:80))
               if (tension.lt.0.or.tension.gt.80) then
                 write(fu6,*)'ERROR: tension must be between 0 and 85'
               endif
             endif
        else
             write(fu6,1200) string(istrt:80)
             stop 'rivtm'
        endif
      call rline(fu5,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu5 ivtstmopt keyword',/A80)
      return
      end subroutine rivtm

C____________________________________________________________________
C Set of subroutines which incorporate a 2D spline under tension
C Based on a package by A. K. CLINE AND R. J. RENKA
C Incorporated to POLYRATE by AFR, May 2003
C____________________________________________________________________

      subroutine spl2d(NXI,NYI,X,Y,Z,XF,YF,ZF,NXA,NYA)
      use perconparam
      use keyword_interface, only : gufac5
      use rate_const
      implicit double precision (a-h,o-z)

      DIMENSION X(NXA),Y(NYA),Z(NXA,NYA)
      DIMENSION ZP(NXA,NYA,3),TEMP2(2*NYA+NXA)
C
C  Do SURF1 setup.
C
      SIGMA = 1.
C     ISF   = 255
      CALL SURF1(NXI,NYI,X,Y,Z,NXI,ZP,TEMP2,SIGMA,
     &IERR,NXA,NYA)
      IF (IERR .NE. 0) THEN
        write(6,*) 'Error return from SURF1 =',IERR
        STOP
      ENDIF

C
C  Get interpolated points using SURF2.
C
      zf = SURF2(XF,YF,NXI,NYI,X,Y,Z,NXI,ZP,SIGMA,
     &NXA,NYA)
C
      return
      end subroutine spl2d
C
      SUBROUTINE SURF1 (M,N,X,Y,Z,IZ,ZP,TEMP,SIGMA,IERR,
     &NXA,NYA)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER M,N,IZ,IERR,NXA,NYA
      REAL*8 X(NXA),Y(NYA),Z(NXA,NYA),OPT(1),ZP(NXA,NYA,3),
     &TEMP(2*NYA+NXA),SIGMA
C
C                      FROM THE SPLINE UNDER TENSION PACKAGE
C                       CODED BY A. K. CLINE AND R. J. RENKA
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C
C                               MODIFIED BY ALFRED H. MORRIS
C                               NAVAL SURFACE WEAPONS CENTER
C                                          DAHLGREN VIRGINIA
C
C THIS SUBROUTINE DETERMINES THE PARAMETERS NECESSARY TO
C COMPUTE AN INTERPOLATORY SURFACE PASSING THROUGH A RECT-
C ANGULAR GRID OF FUNCTIONAL VALUES. THE SURFACE DETERMINED
C CAN BE REPRESENTED AS THE TENSOR PRODUCT OF SPLINES UNDER
C TENSION. THE X- AND Y-PARTIAL DERIVATIVES AROUND THE
C BOUNDARY AND THE X-Y-PARTIAL DERIVATIVES AT THE FOUR
C CORNERS MAY BE SPECIFIED OR OMITTED. FOR ACTUAL MAPPING
C OF POINTS ONTO THE SURFACE IT IS NECESSARY TO CALL THE
C FUNCTION SURF2.
C
C ON INPUT--
C
C   M IS THE NUMBER OF GRID LINES IN THE X-DIRECTION, I. E.
C   LINES PARALLEL TO THE Y-AXIS (M .GE. 2).
C
C   N IS THE NUMBER OF GRID LINES IN THE Y-DIRECTION, I. E.
C   LINES PARALLEL TO THE X-AXIS (N .GE. 2).
C
C   X IS AN ARRAY OF THE M X-COORDINATES OF THE GRID LINES
C   IN THE X-DIRECTION. THESE SHOULD BE STRICTLY INCREASING.
C
C   Y IS AN ARRAY OF THE N Y-COORDINATES OF THE GRID LINES
C   IN THE Y-DIRECTION. THESE SHOULD BE STRICTLY INCREASING.
C
C   Z IS AN ARRAY OF THE M * N FUNCTIONAL VALUES AT THE GRID
C   POINTS, I. E. Z(I,J) CONTAINS THE FUNCTIONAL VALUE AT
C   (X(I),Y(J)) FOR I = 1,...,M AND J = 1,...,N.
C
C   IZ IS THE ROW DIMENSION OF THE MATRIX Z USED IN THE
C   CALLING PROGRAM (IZ .GE. M).
C
C   OPT IS AN OPTION VECTOR. IF NO BOUNDARY CONDITIONS ARE
C   TO BE IMPOSED ON THE SURFACE THEN LET OPT BE OF LENGTH 1
C   AND SET OPT(1)=0. OTHERWISE, SEE THE DESCRIPTION OF SURF
C   IN THE NSWC LIBRARY MANUAL.
C
C   ZP IS AN ARRAY OF AT LEAST 3*M*N LOCATIONS.
C
C   TEMP IS AN ARRAY OF AT LEAST N+N+M LOCATIONS WHICH IS
C   USED FOR SCRATCH STORAGE.
C
C   SIGMA CONTAINS THE TENSION FACTOR. THIS VALUE INDICATES
C   THE CURVINESS DESIRED. IF ABS(SIGMA) IS NEARLY ZERO
C   (E. G. .001) THE RESULTING SURFACE IS APPROXIMATELY THE
C   TENSOR PRODUCT OF CUBIC SPLINES. IF ABS(SIGMA) IS LARGE
C   (E. G. 50.) THE RESULTING SURFACE IS APPROXIMATELY
C   BI-LINEAR. IF SIGMA EQUALS ZERO TENSOR PRODUCTS OF
C   CUBIC SPLINES RESULT. A STANDARD VALUE FOR SIGMA IS
C   APPROXIMATELY 1. IN ABSOLUTE VALUE.
C
C ON OUTPUT--
C
C   ZP CONTAINS THE VALUES OF THE XX-, YY-, AND XXYY-PARTIAL
C   DERIVATIVES OF THE SURFACE AT THE GIVEN NODES.
C
C   IERR CONTAINS AN ERROR FLAG,
C        = 0 FOR NORMAL RETURN,
C        = 1 IF N IS LESS THAN 2 OR M IS LESS THAN 2,
C        = 2 IF THE X-VALUES OR Y-VALUES ARE NOT STRICTLY
C            INCREASING,
C        = 3 THE OPTION VECTOR HAS AN ERROR.
C
C THIS SUBROUTINE REFERENCES PACKAGE MODULES CEEZ, TERMS,
C AND SNHCSG.
C
C-----------------------------------------------------------
C
      INTEGER IND(8),LOC(8),NUM(8)
      DATA NUM(5)/1/, NUM(6)/1/, NUM(7)/1/, NUM(8)/1/
C
      OPT(1)=0.0d0
      MM1 = M-1
      MP1 = M+1
      NM1 = N-1
      NP1 = N+1
      NPM = N+M
      IERR = 0
      IF (N .LE. 1 .OR. M .LE. 1) GO TO 46
      IF (Y(N) .LE. Y(1)) GO TO 47
C
C PROCESS THE OPTION VECTOR
C
      NUM(1) = N
      NUM(2) = N
      NUM(3) = M
      NUM(4) = M
      IND(1) = 0
      IND(2) = 0
      IND(3) = 0
      IND(4) = 0
      IND(5) = 0
      IND(6) = 0
      IND(7) = 0
      IND(8) = 0
C
      L = 1
  100 KEY = OPT(L)
      IF (KEY) 48,110,101
  101 IF (KEY .GT. 8) GO TO 48
      IND(KEY) = 1
      L = L+1
      LOC(KEY) = L
      L = L + NUM(KEY)
      GO TO 100
C
C DENORMALIZE TENSION FACTOR IN Y-DIRECTION
C
  110 SIGMAY = ABS(SIGMA)*FLOAT(N-1)/(Y(N)-Y(1))
C
C OBTAIN Y-PARTIAL DERIVATIVES ALONG Y = Y(1)
C
      IF (IND(3) .EQ. 0) GO TO 2
      L = LOC(3)
      DO 1 I = 1,M
        ZP(I,1,1) = OPT(L)
    1   L = L+1
      GO TO 5
    2 DELY1 = Y(2)-Y(1)
      DELY2 = DELY1+DELY1
      IF (N .GT. 2) DELY2 = Y(3)-Y(1)
      IF (DELY1 .LE. 0. .OR. DELY2 .LE. DELY1) GO TO 47
      CALL CEEZ (DELY1,DELY2,SIGMAY,C1,C2,C3,N)
      DO 3 I = 1,M
    3   ZP(I,1,1) = C1*Z(I,1)+C2*Z(I,2)
      IF (N .EQ. 2) GO TO 5
      DO 4 I = 1,M
    4   ZP(I,1,1) = ZP(I,1,1)+C3*Z(I,3)
C
C OBTAIN Y-PARTIAL DERIVATIVES ALONG Y = Y(N)
C
    5 IF (IND(4) .EQ. 0) GO TO 7
      L = LOC(4)
      DO 6 I = 1,M
        NPI = N+I
        TEMP(NPI) = OPT(L)
    6   L = L+1
      GO TO 10
    7 DELYN = Y(N)-Y(NM1)
      DELYNM = DELYN+DELYN
      IF (N .GT. 2) DELYNM = Y(N)-Y(N-2)
      IF (DELYN .LE. 0. .OR. DELYNM .LE. DELYN) GO TO 47
      CALL CEEZ (-DELYN,-DELYNM,SIGMAY,C1,C2,C3,N)
      DO 8 I = 1,M
        NPI = N+I
    8   TEMP(NPI) = C1*Z(I,N)+C2*Z(I,NM1)
      IF (N .EQ. 2) GO TO 10
      DO 9 I = 1,M
        NPI = N+I
    9   TEMP(NPI) = TEMP(NPI)+C3*Z(I,N-2)
   10 IF (X(M) .LE. X(1)) GO TO 47
C
C DENORMALIZE TENSION FACTOR IN X-DIRECTION
C
      SIGMAX = DABS(SIGMA)*DFLOAT(M-1)/(X(M)-X(1))
C
C OBTAIN X-PARTIAL DERIVATIVES ALONG X = X(1)
C
      IF (IND(1) .EQ. 0) GO TO 12
      L = LOC(1)
      DO 11 J = 1,N
        ZP(1,J,2) = OPT(L)
   11   L = L+1
      IF (IND(5)+IND(7) .EQ. 2) GO TO 15
   12 DELX1 = X(2)-X(1)
      DELX2 = DELX1+DELX1
      IF (M .GT. 2) DELX2 = X(3)-X(1)
      IF (DELX1 .LE. 0. .OR. DELX2 .LE. DELX1) GO TO 47
      CALL CEEZ (DELX1,DELX2,SIGMAX,C1,C2,C3,M)
      IF (IND(1) .EQ. 1) GO TO 15
      DO 13 J = 1,N
   13   ZP(1,J,2) = C1*Z(1,J)+C2*Z(2,J)
      IF (M .EQ. 2) GO TO 15
      DO 14 J = 1,N
   14   ZP(1,J,2) = ZP(1,J,2)+C3*Z(3,J)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (X(1),Y(1))
C
   15 IF (IND(5) .EQ. 0) GO TO 16
      L = LOC(5)
      ZP(1,1,3) = OPT(L)
      GO TO 17
   16 ZP(1,1,3) = C1*ZP(1,1,1)+C2*ZP(2,1,1)
      IF (M .GT. 2) ZP(1,1,3) = ZP(1,1,3)+C3*ZP(3,1,1)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (X(1),Y(N))
C
   17 IF (IND(7) .EQ. 0) GO TO 18
      L = LOC(7)
      ZXY1NS = OPT(L)
      GO TO 19
   18 ZXY1NS = C1*TEMP(N+1)+C2*TEMP(N+2)
      IF (M .GT. 2) ZXY1NS = ZXY1NS+C3*TEMP(N+3)
C
C OBTAIN X-PARTIAL DERIVATIVE ALONG X = X(M)
C
   19 IF (IND(2) .EQ. 0) GO TO 21
      L = LOC(2)
      DO 20 J = 1,N
        NPMPJ = NPM+J
        TEMP(NPMPJ) = OPT(L)
   20   L = L+1
   21 IF (IND(6)+IND(8) .EQ. 2) GO TO 24
      DELXM = X(M)-X(MM1)
      DELXMM = DELXM+DELXM
      IF (M .GT. 2) DELXMM = X(M)-X(M-2)
      IF (DELXM .LE. 0. .OR. DELXMM .LE. DELXM) GO TO 47
      CALL CEEZ (-DELXM,-DELXMM,SIGMAX,C1,C2,C3,M)
      IF (IND(2) .EQ. 1) GO TO 24
      DO 22 J = 1,N
        NPMPJ = NPM+J
   22   TEMP(NPMPJ) = C1*Z(M,J)+C2*Z(MM1,J)
      IF (M .EQ. 2) GO TO 24
      DO 23 J = 1,N
        NPMPJ = NPM+J
   23   TEMP(NPMPJ) = TEMP(NPMPJ)+C3*Z(M-2,J)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (X(M),Y(1))
C
   24 IF (IND(6) .EQ. 0) GO TO 25
      L = LOC(6)
      ZP(M,1,3) = OPT(L)
      GO TO 26
   25 ZP(M,1,3) = C1*ZP(M,1,1)+C2*ZP(MM1,1,1)
      IF (M .GT. 2) ZP(M,1,3) = ZP(M,1,3)+C3*ZP(M-2,1,1)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (X(M),Y(N))
C
   26 IF (IND(8) .EQ. 0) GO TO 27
      L = LOC(8)
      ZXYMNS = OPT(L)
      GO TO 28
   27 ZXYMNS = C1*TEMP(NPM)+C2*TEMP(NPM-1)
      IF (M .GT. 2) ZXYMNS = ZXYMNS+C3*TEMP(NPM-2)
C
C SET UP RIGHT HAND SIDES AND TRIDIAGONAL SYSTEM FOR Y-GRID
C PERFORM FORWARD ELIMINATION
C
   28 DEL1 = Y(2)-Y(1)
      IF (DEL1 .LE. 0.) GO TO 47
      DELI = 1./DEL1
      DO 29 I = 1,M
   29   ZP(I,2,1) = DELI*(Z(I,2)-Z(I,1))
      ZP(1,2,3) = DELI*(ZP(1,2,2)-ZP(1,1,2))
      ZP(M,2,3) = DELI*(TEMP(NPM+2)-TEMP(NPM+1))
      CALL TERMS (DIAG1,SDIAG1,SIGMAY,DEL1)
      DIAGI = 1./DIAG1
      DO 30 I = 1,M
   30   ZP(I,1,1) = DIAGI*(ZP(I,2,1)-ZP(I,1,1))
      ZP(1,1,3) = DIAGI*(ZP(1,2,3)-ZP(1,1,3))
      ZP(M,1,3) = DIAGI*(ZP(M,2,3)-ZP(M,1,3))
      TEMP(1) = DIAGI*SDIAG1
      IF (N .EQ. 2) GO TO 34
      DO 33 J = 2,NM1
        JM1 = J-1
        JP1 = J+1
        NPMPJ = NPM+J
        DEL2 = Y(JP1)-Y(J)
        IF (DEL2 .LE. 0.) GO TO 47
        DELI = 1./DEL2
        DO 31 I = 1,M
   31     ZP(I,JP1,1) = DELI*(Z(I,JP1)-Z(I,J))
        ZP(1,JP1,3) = DELI*(ZP(1,JP1,2)-ZP(1,J,2))
        ZP(M,JP1,3) = DELI*(TEMP(NPMPJ+1)-TEMP(NPMPJ))
        CALL TERMS (DIAG2,SDIAG2,SIGMAY,DEL2)
        DIAGIN = 1./(DIAG1+DIAG2-SDIAG1*TEMP(JM1))
        DO 32 I = 1,M
   32     ZP(I,J,1) = DIAGIN*(ZP(I,JP1,1)-ZP(I,J,1)-
     *                        SDIAG1*ZP(I,JM1,1))
        ZP(1,J,3) = DIAGIN*(ZP(1,JP1,3)-ZP(1,J,3)-
     *                      SDIAG1*ZP(1,JM1,3))
        ZP(M,J,3) = DIAGIN*(ZP(M,JP1,3)-ZP(M,J,3)-
     *                      SDIAG1*ZP(M,JM1,3))
        TEMP(J) = DIAGIN*SDIAG2
        DIAG1 = DIAG2
   33   SDIAG1 = SDIAG2
   34 DIAGIN = 1./(DIAG1-SDIAG1*TEMP(NM1))
      DO 35 I = 1,M
        NPI = N+I
   35   ZP(I,N,1) = DIAGIN*(TEMP(NPI)-ZP(I,N,1)-
     *                      SDIAG1*ZP(I,NM1,1))
      ZP(1,N,3) = DIAGIN*(ZXY1NS-ZP(1,N,3)-
     *                    SDIAG1*ZP(1,NM1,3))
      TEMP(N) = DIAGIN*(ZXYMNS-ZP(M,N,3)-
     *                  SDIAG1*ZP(M,NM1,3))
C
C PERFORM BACK SUBSTITUTION
C
      DO 37 J = 2,N
        JBAK = NP1-J
        JBAKP1 = JBAK+1
        T = TEMP(JBAK)
        DO 36 I = 1,M
   36     ZP(I,JBAK,1) = ZP(I,JBAK,1)-T*ZP(I,JBAKP1,1)
        ZP(1,JBAK,3) = ZP(1,JBAK,3)-T*ZP(1,JBAKP1,3)
   37   TEMP(JBAK) = ZP(M,JBAK,3)-T*TEMP(JBAKP1)
C
C SET UP RIGHT HAND SIDES AND TRIDIAGONAL SYSTEM FOR X-GRID
C PERFORM FORWARD ELIMINATION
C
      DEL1 = X(2)-X(1)
      IF (DEL1 .LE. 0.) GO TO 47
      DELI = 1./DEL1
      DO 38 J = 1,N
        ZP(2,J,2) = DELI*(Z(2,J)-Z(1,J))
   38   ZP(2,J,3) = DELI*(ZP(2,J,1)-ZP(1,J,1))
      CALL TERMS (DIAG1,SDIAG1,SIGMAX,DEL1)
      DIAGI = 1./DIAG1
      DO 39 J = 1,N
        ZP(1,J,2) = DIAGI*(ZP(2,J,2)-ZP(1,J,2))
   39   ZP(1,J,3) = DIAGI*(ZP(2,J,3)-ZP(1,J,3))
      TEMP(N+1) = DIAGI*SDIAG1
      IF (M  .EQ. 2) GO TO 43
      DO 42 I = 2,MM1
        IM1 = I-1
        IP1 = I+1
        NPI = N+I
        DEL2 = X(IP1)-X(I)
        IF (DEL2 .LE. 0.) GO TO 47
        DELI = 1./DEL2
        DO 40 J = 1,N
          ZP(IP1,J,2) = DELI*(Z(IP1,J)-Z(I,J))
   40     ZP(IP1,J,3) = DELI*(ZP(IP1,J,1)-ZP(I,J,1))
        CALL TERMS (DIAG2,SDIAG2,SIGMAX,DEL2)
        DIAGIN = 1./(DIAG1+DIAG2-SDIAG1*TEMP(NPI-1))
        DO 41 J = 1,N
          ZP(I,J,2) = DIAGIN*(ZP(IP1,J,2)-ZP(I,J,2)-
     *                        SDIAG1*ZP(IM1,J,2))
   41     ZP(I,J,3) = DIAGIN*(ZP(IP1,J,3)-ZP(I,J,3)-
     *                        SDIAG1*ZP(IM1,J,3))
        TEMP(NPI) = DIAGIN*SDIAG2
        DIAG1 = DIAG2
   42   SDIAG1 = SDIAG2
   43 DIAGIN = 1./(DIAG1-SDIAG1*TEMP(NPM-1))
      DO 44 J = 1,N
        NPMPJ = NPM+J
        ZP(M,J,2) = DIAGIN*(TEMP(NPMPJ)-ZP(M,J,2)-
     *                      SDIAG1*ZP(MM1,J,2))
   44   ZP(M,J,3) = DIAGIN*(TEMP(J)-ZP(M,J,3)-
     *                      SDIAG1*ZP(MM1,J,3))
C
C PERFORM BACK SUBSTITUTION
C
      DO 45 I = 2,M
        IBAK = MP1-I
        IBAKP1 = IBAK+1
        NPIBAK = N+IBAK
        T = TEMP(NPIBAK)
        DO 45 J = 1,N
          ZP(IBAK,J,2) = ZP(IBAK,J,2)-T*ZP(IBAKP1,J,2)
   45     ZP(IBAK,J,3) = ZP(IBAK,J,3)-T*ZP(IBAKP1,J,3)
      RETURN
C
C TOO FEW POINTS
C
   46 IERR = 1
      RETURN
C
C POINTS NOT STRICTLY INCREASING
C
   47 IERR = 2
      RETURN
C
C THE OPTION VECTOR HAS AN ERROR
C
   48 IERR = 3
      RETURN
      END SUBROUTINE SURF1

      FUNCTION SURF2 (XX,YY,M,N,X,Y,Z,IZ,ZP,SIGMA,
     &NXA,NYA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER M,N,IZ,NXA,NYA
c     REAL*8 XX,YY,X(M),Y(N),Z(IZ,N),ZP(M,N,*),SIGMA
      REAL*8 XX,YY,X(NXA),Y(NYA),Z(NXA,NYA),ZP(NXA,NYA,3),
     &SIGMA
C
C                      FROM THE SPLINE UNDER TENSION PACKAGE
C                       CODED BY A. K. CLINE AND R. J. RENKA
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C
C THIS FUNCTION INTERPOLATES A SURFACE AT A GIVEN COORDINATE
C PAIR USING A BI-SPLINE UNDER TENSION. THE SUBROUTINE SURF1
C SHOULD BE CALLED EARLIER TO DETERMINE CERTAIN NECESSARY
C PARAMETERS.
C
C ON INPUT--
C
C   XX AND YY CONTAIN THE X- AND Y-COORDINATES OF THE POINT
C   TO BE MAPPED ONTO THE INTERPOLATING SURFACE.
C
C   M AND N CONTAIN THE NUMBER OF GRID LINES IN THE X- AND
C   Y-DIRECTIONS, RESPECTIVELY, OF THE RECTANGULAR GRID
C   WHICH SPECIFIED THE SURFACE.
C
C   X AND Y ARE ARRAYS CONTAINING THE X- AND Y-GRID VALUES,
C   RESPECTIVELY, EACH IN INCREASING ORDER.
C
C   Z IS A MATRIX CONTAINING THE M * N FUNCTIONAL VALUES
C   CORRESPONDING TO THE GRID VALUES (I. E. Z(I,J) IS THE
C   SURFACE VALUE AT THE POINT (X(I),Y(J)) FOR I = 1,...,M
C   AND J = 1,...,N).
C
C   IZ CONTAINS THE ROW DIMENSION OF THE ARRAY Z AS DECLARED
C   IN THE CALLING PROGRAM.
C
C   ZP IS AN ARRAY OF 3*M*N LOCATIONS STORED WITH THE
C   VARIOUS SURFACE DERIVATIVE INFORMATION DETERMINED BY
C   SURF1.
C
C AND
C
C   SIGMA CONTAINS THE TENSION FACTOR (ITS SIGN IS IGNORED).
C
C THE PARAMETERS M, N, X, Y, Z, IZ, ZP, AND SIGMA SHOULD BE
C INPUT UNALTERED FROM THE OUTPUT OF SURF1.
C
C ON OUTPUT--
C
C   SURF2 CONTAINS THE INTERPOLATED SURFACE VALUE.
C
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C
C THIS FUNCTION REFERENCES PACKAGE MODULES INTRV2 AND
C SNHCSG.
C
C-----------------------------------------------------------
C
C INLINE ONE DIMENSIONAL CUBIC SPLINE INTERPOLATION
C
      RHERMZ (F1,F2,FP1,FP2) = 
     *           (F2*DEL1+F1*DEL2)/DELS-DEL1*
     *                        DEL2*(FP2*(DEL1+DELS)+
     *                              FP1*(DEL2+DELS))/
     *                        (6.*DELS)
C
C INLINE ONE DIMENSIONAL SPLINE UNDER TENSION INTERPOLATION
C
       RHERMNZ (F1,F2,FP1,FP2,SIGMAP) = 
     *           (F2*DEL1+F1*DEL2)/DELS
     *          +(FP2*(SINHM1*DEL2-DEL1*(2.*(COSHP1+1.)*
     *                           SINHP2+SIGMAP*COSHP1*DEL2))
     *           +FP1*(SINHM2*DEL1-DEL2*(2.*(COSHP2+1.)*
     *                           SINHP1+SIGMAP*COSHP2*DEL1))
     *          )/(SIGMAP*SIGMAP*DELS*(SINHMS+SIGMAP*DELS))
C
C DENORMALIZE TENSION FACTOR IN X AND Y DIRECTION
C
      SIGMAX = DABS(SIGMA)*DFLOAT(M-1)/(X(M)-X(1))
      SIGMAY = DABS(SIGMA)*DFLOAT(N-1)/(Y(N)-Y(1))
C
C DETERMINE Y INTERVAL
C
      JM1 = INTRV2 (YY,Y,N)
      J = JM1+1
C
C DETERMINE X INTERVAL
C
      IM1 = INTRV2 (XX,X,M)
      I = IM1+1
      DEL1 = YY-Y(JM1)
      DEL2 = Y(J)-YY
      DELS = Y(J)-Y(JM1)
      IF (SIGMAY .NE. 0.) GO TO 1
C
C PERFORM FOUR INTERPOLATIONS IN Y-DIRECTION
C
      ZIM1 = RHERMZ(Z(I-1,J-1),Z(I-1,J),ZP(I-1,J-1,1),
     *                                  ZP(I-1,J,1))
      ZI = RHERMZ(Z(I,J-1),Z(I,J),ZP(I,J-1,1),ZP(I,J,1))
      ZXXIM1 = RHERMZ(ZP(I-1,J-1,2),ZP(I-1,J,2),
     *                ZP(I-1,J-1,3),ZP(I-1,J,3))
      ZXXI = RHERMZ(ZP(I,J-1,2),ZP(I,J,2),
     *              ZP(I,J-1,3),ZP(I,J,3))
      GO TO 2
    1 DELP1 = (DEL1+DELS)/2.
      DELP2 = (DEL2+DELS)/2.
      CALL SNHCSG (SINHM1,DUMMY,SIGMAY*DEL1,-1)
      CALL SNHCSG (SINHM2,DUMMY,SIGMAY*DEL2,-1)
      CALL SNHCSG (SINHMS,DUMMY,SIGMAY*DELS,-1)
      CALL SNHCSG (SINHP1,DUMMY,SIGMAY*DEL1/2.,-1)
      CALL SNHCSG (SINHP2,DUMMY,SIGMAY*DEL2/2.,-1)
      CALL SNHCSG (DUMMY,COSHP1,SIGMAY*DELP1,1)
      CALL SNHCSG (DUMMY,COSHP2,SIGMAY*DELP2,1)
      ZIM1 = RHERMNZ(Z(I-1,J-1),Z(I-1,J),ZP(I-1,J-1,1),
     *               ZP(I-1,J,1),SIGMAY)
      ZI = RHERMNZ(Z(I,J-1),Z(I,J),ZP(I,J-1,1),ZP(I,J,1),
     *             SIGMAY)
      ZXXIM1 = RHERMNZ(ZP(I-1,J-1,2),ZP(I-1,J,2),
     *                 ZP(I-1,J-1,3),ZP(I-1,J,3),SIGMAY)
      ZXXI = RHERMNZ(ZP(I,J-1,2),ZP(I,J,2),
     *               ZP(I,J-1,3),ZP(I,J,3),SIGMAY)
C
C PERFORM FINAL INTERPOLATION IN X-DIRECTION
C
    2 DEL1 = XX-X(IM1)
      DEL2 = X(I)-XX
      DELS = X(I)-X(IM1)
      IF (SIGMAX .NE. 0.) GO TO 3
      SURF2 = RHERMZ(ZIM1,ZI,ZXXIM1,ZXXI)
      RETURN
    3 DELP1 = (DEL1+DELS)/2.
      DELP2 = (DEL2+DELS)/2.
      CALL SNHCSG (SINHM1,DUMMY,SIGMAX*DEL1,-1)
      CALL SNHCSG (SINHM2,DUMMY,SIGMAX*DEL2,-1)
      CALL SNHCSG (SINHMS,DUMMY,SIGMAX*DELS,-1)
      CALL SNHCSG (SINHP1,DUMMY,SIGMAX*DEL1/2.,-1)
      CALL SNHCSG (SINHP2,DUMMY,SIGMAX*DEL2/2.,-1)
      CALL SNHCSG (DUMMY,COSHP1,SIGMAX*DELP1,1)
      CALL SNHCSG (DUMMY,COSHP2,SIGMAX*DELP2,1)
      SURF2 = RHERMNZ(ZIM1,ZI,ZXXIM1,ZXXI,SIGMAX)
      RETURN
      END FUNCTION SURF2

      SUBROUTINE TERMS (DIAG,SDIAG,SIGMA,DEL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      REAL*8 DIAG,SDIAG,SIGMA,DEL
C
C                      FROM THE SPLINE UNDER TENSION PACKAGE
C                       CODED BY A. K. CLINE AND R. J. RENKA
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C
C THIS SUBROUTINE COMPUTES THE DIAGONAL AND SUPERDIAGONAL
C TERMS OF THE TRIDIAGONAL LINEAR SYSTEM ASSOCIATED WITH
C SPLINE UNDER TENSION INTERPOLATION.
C
C ON INPUT--
C
C   SIGMA CONTAINS THE TENSION FACTOR.
C
C AND
C
C   DEL CONTAINS THE STEP SIZE.
C
C ON OUTPUT--
C
C               (SIGMA*DEL*COSH(SIGMA*DEL) - SINH(SIGMA*DEL)
C   DIAG = DEL*--------------------------------------------.
C                     (SIGMA*DEL)**2 * SINH(SIGMA*DEL)
C
C                   SINH(SIGMA*DEL) - SIGMA*DEL
C   SDIAG = DEL*----------------------------------.
C                (SIGMA*DEL)**2 * SINH(SIGMA*DEL)
C
C AND
C
C   SIGMA AND DEL ARE UNALTERED.
C
C THIS SUBROUTINE REFERENCES PACKAGE MODULE SNHCSG.
C
C-----------------------------------------------------------
C
      IF (SIGMA .NE. 0.) GO TO 1
      DIAG = DEL/3.
      SDIAG = DEL/6.
      RETURN
    1 SIGDEL = SIGMA*DEL
      CALL SNHCSG (SINHM,COSHM,SIGDEL,0)
      DENOM = DEL/((SINHM+SIGDEL)*SIGDEL*SIGDEL)
      DIAG = DENOM*(SIGDEL*COSHM-SINHM)
      SDIAG = DENOM*SINHM
      RETURN
      END SUBROUTINE TERMS

      INTEGER FUNCTION INTRV2 (X, T, M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 X, T(M)
C-----------------------------------------------------------------------
C
C             LOCATION OF A VALUE X IN A SEQUENCE T
C
C                       ----------------
C
C     INPUT--
C
C        X IS A REAL NUMBER.
C
C        T IS AN ARRAY OF NONDECREASING VALUES. IT IS
C          ASSUMED THAT T(1) .LT. T(M).
C
C        M IS THE LENGTH OF T (M .GE. 2).
C
C     OUTPUT--
C
C        INTRV2 HAS THE VALUE I WHEN  T(I) .LE. X .LT. T(I+1).
C        OTHERWISE, IF L IS THE INTEGER WHERE T(L) .LT. T(L+1)
C        AND T(L+1) = ... = T(M), THEN INTRV2 HAS THE VALUE I
C        WHERE
C                  I = 1   IF X .LT. T(1)
C                  I = L   IF X .GE. T(L)
C
C-----------------------------------------------------------------------
      IF (X .LT. T(2)) GO TO 100
      TM = T(M)
      I = M - 1
   10    IF (T(I) .LT. TM) GO TO 20
         I = I - 1
         GO TO 10
   20 IF (X .GE. T(I)) GO TO 110
      IL = 2
      IR = I
C
C     BISECTION SEARCH
C
   30 I = (IL + IR)/2
      IF (I .EQ. IL) GO TO 110
      IF (X - T(I)) 40,60,50
   40 IR = I
      GO TO 30
   50 IL = I
      GO TO 30
C
C     CASE WHEN X = T(I) FOR SOME I
C
   60 IF (X .LT. T(I+1)) GO TO 110
      I = I + 1
      GO TO 60
C
C     LEFT END
C
  100 INTRV2 = 1
      RETURN
C
C     NORMAL EXIT
C
  110 INTRV2 = I
      RETURN
      END function INTRV2

      SUBROUTINE SNHCSG (SINHM,COSHM,X,ISW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER ISW
      REAL*8 SINHM,COSHM,X,CUT(5)
C
C                      FROM THE SPLINE UNDER TENSION PACKAGE
C                       CODED BY A. K. CLINE AND R. J. RENKA
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C                          MODIFIED BY A.H. MORRIS (NSWC/DL)
C
C THIS SUBROUTINE RETURNS APPROXIMATIONS TO
C       SINHM(X) = SINH(X)-X
C       COSHM(X) = COSH(X)-1
C AND
C       COSHMM(X) = COSH(X)-1-X*X/2
C
C ON INPUT--
C
C   X CONTAINS THE VALUE OF THE INDEPENDENT VARIABLE.
C
C   ISW INDICATES THE FUNCTION DESIRED
C           = -1 IF ONLY SINHM IS DESIRED,
C           =  0 IF BOTH SINHM AND COSHM ARE DESIRED,
C           =  1 IF ONLY COSHM IS DESIRED,
C           =  2 IF ONLY COSHMM IS DESIRED,
C           =  3 IF BOTH SINHM AND COSHMM ARE DESIRED.
C
C ON OUTPUT--
C
C   SINHM CONTAINS THE VALUE OF SINHM(X) IF ISW .LE. 0 OR
C   ISW .EQ. 3 (SINHM IS UNALTERED IF ISW .EQ.1 OR ISW .EQ.
C   2).
C
C   COSHM CONTAINS THE VALUE OF COSHM(X) IF ISW .EQ. 0 OR
C   ISW .EQ. 1 AND CONTAINS THE VALUE OF COSHMM(X) IF ISW
C   .GE. 2 (COSHM IS UNALTERED IF ISW .EQ. -1).
C
C AND
C
C   X AND ISW ARE UNALTERED.
C
C-----------------------------------------------------------
C
      DATA SP5/.255251817302048E-09/,
     *     SP4/.723809046696880E-07/,
     *     SP3/.109233297700241E-04/,
     *     SP2/.954811583154274E-03/,
     *     SP1/.452867078563929E-01/,
     *     SQ1/-.471329214363072E-02/
      DATA CP5/.116744361560051E-08/,
     *     CP4/.280407224259429E-06/,
     *     CP3/.344417983443219E-04/,
     *     CP2/.232293648552398E-02/,
     *     CP1/.778752378267155E-01/,
     *     CQ1/-.545809550662099E-02/
      DATA ZP3/5.59297116264720E-07/,
     *     ZP2/1.77943488030894E-04/,
     *     ZP1/1.69800461894792E-02/,
     *     ZQ4/1.33412535492375E-09/,
     *     ZQ3/-5.80858944138663E-07/,
     *     ZQ2/1.27814964403863E-04/,
     *     ZQ1/-1.63532871439181E-02/
      DATA CUT(1)/1.65/, CUT(2)/1.2/, CUT(3)/1.2/, CUT(4)/2.7/,
     *     CUT(5)/1.65/
C
      XX = X
      AX = DABS(XX)
      XS = XX*XX
      IF (AX .GE. CUT(ISW+2)) EXPX = DEXP(AX)
C
C SINHM APPROXIMATION
C
      IF (ISW .EQ. 1 .OR. ISW .EQ. 2) GO TO 2
      IF (AX .GE. 1.65) GO TO 1
      SINHM = ((((((SP5*XS+SP4)*XS+SP3)*XS+SP2)*XS+SP1)*XS+1.)
     *        *XS*XX)/((SQ1*XS+1.)*6.)
      GO TO 2
    1 SINHM = -(((AX+AX)+1./EXPX)-EXPX)/2.
      IF (XX .LT. 0.) SINHM = -SINHM
C
C COSHM APPROXIMATION
C
    2 IF (ISW .NE. 0 .AND. ISW .NE. 1) GO TO 4
      IF (AX .GE. 1.2) GO TO 3
      COSHM = ((((((CP5*XS+CP4)*XS+CP3)*XS+CP2)*XS+CP1)*XS+1.)
     *        *XS)/((CQ1*XS+1.)*2.)
      GO TO 4
    3 COSHM = ((1./EXPX-2.)+EXPX)/2.
C
C COSHMM APPROXIMATION
C
    4 IF (ISW .LE. 1) RETURN
      IF (AX .GE. 2.70) GO TO 5
      COSHM = ((((ZP3*XS+ZP2)*XS+ZP1)*XS+1.)*XS*XS)/(((((ZQ4
     *        *XS+ZQ3)*XS+ZQ2)*XS+ZQ1)*XS+1.)*24.)
      RETURN
    5 COSHM = (((1./EXPX-2.)-XS)+EXPX)/2.
      RETURN
      END SUBROUTINE SNHCSG

      SUBROUTINE CEEZ (DEL1,DEL2,SIGMA,C1,C2,C3,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      REAL*8 DEL1,DEL2,SIGMA,C1,C2,C3
C
C                      FROM THE SPLINE UNDER TENSION PACKAGE
C                       CODED BY A. K. CLINE AND R. J. RENKA
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C
C THIS SUBROUTINE DETERMINES THE COEFFICIENTS C1, C2, AND C3
C USED TO DETERMINE ENDPOINT SLOPES. SPECIFICALLY, IF
C FUNCTION VALUES Y1, Y2, AND Y3 ARE GIVEN AT POINTS X1, X2,
C AND X3, RESPECTIVELY, THE QUANTITY C1*Y1 + C2*Y2 + C3*Y3
C IS THE VALUE OF THE DERIVATIVE AT X1 OF A SPLINE UNDER
C TENSION (WITH TENSION FACTOR SIGMA) PASSING THROUGH THE
C THREE POINTS AND HAVING THIRD DERIVATIVE EQUAL TO ZERO AT
C X1. OPTIONALLY, ONLY TWO VALUES, C1 AND C2 ARE DETERMINED.
C
C ON INPUT--
C
C   DEL1 IS X2-X1 (.GT. 0.).
C
C   DEL2 IS X3-X1 (.GT. 0.). IF N .EQ. 2, THIS PARAMETER IS
C   IGNORED.
C
C   SIGMA IS THE TENSION FACTOR.
C
C AND
C
C   N IS A SWITCH INDICATING THE NUMBER OF COEFFICIENTS TO
C   BE RETURNED. IF N .EQ. 2 ONLY TWO COEFFICIENTS ARE
C   RETURNED. OTHERWISE ALL THREE ARE RETURNED.
C
C ON OUTPUT--
C
C   C1, C2, AND C3 CONTAIN THE COEFFICIENTS.
C
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C
C THIS SUBROUTINE REFERENCES PACKAGE MODULE SNHCSG.
C
C-----------------------------------------------------------
C
      IF (N .EQ. 2) GO TO 2
      IF (SIGMA .NE. 0.) GO TO 1
      DEL = DEL2-DEL1
C
C TENSION .EQ. 0.
C
      C1 = -(DEL1+DEL2)/(DEL1*DEL2)
      C2 = DEL2/(DEL1*DEL)
      C3 = -DEL1/(DEL2*DEL)
      RETURN
C
C TENSION .NE. 0.
C
    1 CALL SNHCSG (DUMMY,COSHM1,SIGMA*DEL1,1)
      CALL SNHCSG (DUMMY,COSHM2,SIGMA*DEL2,1)
      DELP = SIGMA*(DEL2+DEL1)/2.
      DELM = SIGMA*(DEL2-DEL1)/2.
      CALL SNHCSG (SINHMP,DUMMY,DELP,-1)
      CALL SNHCSG (SINHMM,DUMMY,DELM,-1)
      DENOM = COSHM1*(DEL2-DEL1)-2.*DEL1*(SINHMP+DELP)*
     *        (SINHMM+DELM)
      C1 = 2.*(SINHMP+DELP)*(SINHMM+DELM)/DENOM
      C2 = -COSHM2/DENOM
      C3 = COSHM1/DENOM
      RETURN
C
C TWO COEFFICIENTS
C
    2 C1 = -1./DEL1
      C2 = -C1
      RETURN
      END SUBROUTINE CEEZ
C -----------------------------------------------------------------
C         End of the 2D spline package
C -----------------------------------------------------------------
C
C Gauss-Jordan Elimination
C
      SUBROUTINE gaussj(a,n,np,b,m,mp)
      INTEGER m,mp,n,np,NMAX
      REAL a(np,np),b(np,mp)
      PARAMETER (NMAX=50)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL big,dum,pivinv
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                pause 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END SUBROUTINE gaussj
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.


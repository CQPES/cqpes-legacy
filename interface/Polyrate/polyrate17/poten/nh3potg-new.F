C
         SUBROUTINE surf (V, X, DX, N3TM)
         use potcm4
C
C   This is a driver routine which can be used to link a calling program
C   that supplies the cartesian coordinates in an array X in the parameter
C   list with a potential energy surface routine that accepts the position
C   of the atoms in internal coordinates.  This routine will convert the 
C   cartesian coordinates to internal coordinates by calling the subprogram
C   COORD4 and call the potential energy function POTEN.  This subprogram 
C   will take the energy and the derivatives of the energy with respect to
C   the internal coordinates calculated in POTEN and convert the derivatives
C   to derivatives with respect to the atomic cartesians.  The energy and
C   the derivatives with respect to the cartesians are passed to the calling
C   program through V and DV, respectively, in the parameter list.  
C   All calculations are in hartree atomic units.
C
C   This subprogram assumes that the potential energy function that will
C   be called to obtain the energy and the derivatives of the energy with 
C   respect to the internal coordinates is in the following form:
C        1) the subprogram name is POTEN
C        2) the subprogram POTEN has no argument list, all information is 
C           passed to and from the potential energy subprogram through 
C           the common block POTCM:
C                                   /POTCM/ R(6), VTOT, DVDR(6)
C           where R(6)   : array of the internal coordinates in bohr
C                 V      : energy in hartree atomic units
C                 DVDR(6): array of the derivatives of the energy with respect
C                          to the internal coordinates in atomic units.
C
C   The integer N3TM which is also passed is used to dimension the 
C   arrays X and DX and needs to be greater than or equal to 3 * NATOMS
C   where NATOMS is the number of atoms represented by this potential.
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
         integer, intent(in) :: N3TM
         real(8), intent(in) :: X(N3TM)
         real(8) ::  DX(N3TM)
         real(8) ::  gU11(12),gU22(12),gU12(12),gV1(12),gV2(12)
C
C   Transform the cartesian coordinates to internal coordinates.
C
C        CALL COORD4(X, N3TM)
C
C   Calculate the energy.
C
C        CALL POTEN
         call pot(x,u11,u22,u12,v1,v2,gu11,gu22,gu12,gv1,gv2)
C        V = v2
         V = U22
         DX(:) =gU22(:)
C
         RETURN
         END subroutine surf
C*****
C*******************************************************************
      SUBROUTINE pot(Xcart,U11,U22,U12,V1,V2,
     1           gU11,gU22,gU12,
     2           gV1,gV2)
C*******************************************************************
C   System:                        NH3
C   Functional form:               polynomial 2x2 diabatic fit
C   Common name:                   NH3
C   Number of derivatives:         5
C   Number of bodies:              4
C   Number of electronic surfaces: 1
C   Interface:
C
C  Notes: 
C This potential was finalized based on the function forms in Ref. 1.
C The potential gives the singlet ground state (U11), the first
C singlet excited state (U22), the coupling (U12) energies, the
C adiabatic energies (V1 and V2), and their cartersian gradients.
C This potential has additional correction terms used for fitting
C to the experimental dissociation limits.
C
C References: 
C      1) Nangia, S.; Truhlar, D.G. J. Chem. Phys. 2006, 124, 124309
C      2) Li, Z.H.; Valaro, R., Truhlar, D.G. Theor. Chem. Acc. 2006,
C         submitted
C
C  Units:
C       energies    - hartrees
C       coordinates - bohrs
C  All gradients are in hartree/bohr
C  Format for   Xcart 
C  Xcart(1:3)   is N
C  Xcart(4:6)   is H1
C  Xcart(7:9)   is H2
C  Xcart(10:12) is H3

      implicit none
C I/O variables
      integer NATOM
      parameter (NATOM = 4)
      double precision Xcart(12) 
      double precision U11
      double precision U22
      double precision U12
      double precision V1
      double precision V2
      double precision gU11(12)
      double precision gU22(12)
      double precision gU12(12)
      double precision gV1(12)
      double precision gV2(12)

C Local variables
C cart(12) local Xcart in ANG, x1,y1,z1,...,x4,y4,z4
      double precision cart(12)
      integer i,j

C Unit conversion parameters
      double precision gconv
      double precision autoang
      double precision autoev
      parameter(autoang=0.5291772108d0)
      parameter(autoev=27.2113845d0)
      double precision ANG_TO_BOHR
      double precision EV_TO_HARTREE
      parameter (EV_TO_HARTREE= 1.d0/autoev)
      parameter (ANG_TO_BOHR=1.0d0/autoang)
      parameter (gconv = EV_TO_HARTREE/ANG_TO_BOHR)


C convert geometry from bohr to ang 
      do i=1,12
          cart(i)= Xcart(i)*autoang
      end do

C get energies and gradients and convert to atomic unit
      call packpot(cart,U11,U22,U12,V1,V2,gU11,gU22,gU12,gV1,gV2)
      U11 = U11 * EV_TO_HARTREE
      U22 = U22 * EV_TO_HARTREE
      U12 = U12 * EV_TO_HARTREE
      V1  = V1  * EV_TO_HARTREE
      V2  = V2  * EV_TO_HARTREE
      do i=1,12
        gU11(i) = gU11(i) * gconv
        gU22(i) = gU22(i) * gconv
        gU12(i) = gU12(i) * gconv
        gV1(i)  = gV1(i)  * gconv
        gV2(i)  = gV2(i)  * gconv
      enddo

      return
      end

C23456789
      SUBROUTINE prepot
        call potcoeff
      return
      end

C!*************************************************************!!
      SUBROUTINE packpot(coord,U11,U22,U12,V1,V2,
     &                       gcU11,gcU22,gcU12,gcV1,gcV2)
C!*************************************************************!!
      implicit none
C--------------------------------------------------------------!
C I/O variables
C--------------------------------------------------------------!
C coord: x1,y1,z1...x4,y4,z4
      double precision coord(12)
      double precision U11,U22,U12,V1,V2
      double precision gcU11(12)
      double precision gcU22(12)
      double precision gcU12(12)
      double precision gcV1(12)
      double precision gcV2(12)
C--------------------------------------------------------------!
C Common blocks for coefficients and indices
C--------------------------------------------------------------!
      integer NV
      parameter (NV=7)
      integer NCU1,NCU2,NCU12
      parameter (NCU1=62,NCU2=71,NCU12=45)
      integer nhindU1(NCU1,NV)
      integer nhindU2(NCU2,NV)
      integer nhindU12(NCU12,NV)
      double precision nhcoeU1(NCU1)
      double precision nhcoeU2(NCU2)
      double precision nhcoeU12(NCU12)
      common /c_nhcoe/  nhcoeU1, nhcoeU2, nhcoeU12,
     &                  nhindU1, nhindU2, nhindU12

C Distances 1-3: NH, 4-6: HH
      double precision rr(6)
C gradients of distances to cartesian coordinates
      double precision grr(12,6)
      double precision derep(6)
C gradients of integernal coordinates
      double precision gU11(12)
      double precision gU22(12)
      double precision gU12(12)
      double precision gV1(12)
      double precision gV2(12)
C--------------------------------------------------------------!
C Parameter List
C--------------------------------------------------------------!
      double precision PI
      parameter (  PI    = 3.1415926535897930d0 )

C--------------------------!
C Internal coordinates used
C--------------------------!
C internal coodinates: three bond distances, three bond angles, one
C trisector angle
      double precision angint(7)
C gradients of angle internal coordinates to cartesian coordinates
      double precision gangint(12,7)
C Bowman internal coodinates: three bond distances, three bond angles
C (the three angles in the projected plane), one trisector angle
      double precision bowint(7)
C gradients of bowman internal coordinates to cartesian coordinates
      double precision gbowint(12,7)

C-------------------------!
C Misc
C-------------------------!
      integer i,ii,j,k,N
      double precision erep
C to save the first three bond distances. If they are calculated in any
C subroutines, avoid to calculate them again
       double precision rrrrr(3)
       logical lrrrrr
       common /calcrrrr/ rrrrr,lrrrrr
       lrrrrr = .false.
C=================================================================================!
C This code computes the fitted pot using the polynomial funcs as
C described by Bowman for NH3
C
C INPUT FILES : TWO INPUT FILES ARE NEEDED 
C	[1]. Coefficient File	
C	[2]. Index File (contains the powers for the polynomial)
C

! Initialize energy derivatives
      do i=1,12
           gcU11(i) = 0.d0
           gcU22(i) = 0.d0
           gcU12(i) = 0.d0
           gcV1(i)  = 0.d0
           gcV2(i)  = 0.d0
      enddo
! Initialize gangint,and dbow
      do i=1,7
        do j=1,12
          gangint(j,i) = 0.d0
          gbowint(j,i) = 0.d0
        enddo
      enddo

C Calculate internal coodinates
      call toBowman(coord,bowint,gbowint)
      call carttoang(coord,angint,gangint)
C Calculate the distances
      call rarray(4,coord,12,rr,grr,6)
C Calculate bowman internal coodinates
C The 7th internal for angint and bowint are both beta
      angint(7)= bowint(7)
C Derivatives of 7 (beta) are the same for gangint and gbowint, which
C are calculated in toBowman
      do j=1,12
         gangint(j,7) = gbowint(j,7)
      enddo

C     Calculate U11,U22,wighout repulsive
      call sumpoly(NCU1,nhindU1,angint,nhcoeU1,U11,gU11,'U11')
      call sumpoly(NCU2,nhindU2,angint,nhcoeU2,U22,gU22,'U22')
      call sumpoly(NCU12,nhindU12,angint,nhcoeU12,U12,gU12,'U12')

C     Convert derivatives of internal coordinates to Catersian by chain
C     rule U12 used bowint while other used angint, thus must convert to
C     the same cartesian derivs before calling getAdiabat
      do j=1,12
      do i=1,7
           gcU11(j) = gcU11(j) + gU11(i) * gangint(j,i)
           gcU22(j) = gcU22(j) + gU22(i) * gangint(j,i)
           gcU12(j) = gcU12(j) + gU12(i) * gangint(j,i)
      enddo
      enddo

      call repulsive(rr,erep,derep,'U11')
      U11 = U11 + erep
      do j=1,12
      do i=1,6
           gcU11(j)  = gcU11(j) + derep(i) * grr(j,i)
      enddo
      enddo
      call repulsive(rr,erep,derep,'U22')
      U22 = U22 + erep
      do j=1,12
      do i=1,6
           gcU22(j)  = gcU22(j) + derep(i) * grr(j,i)
      enddo
      enddo

C     Calculate V1,V2 from U11 and U22
      call getAdiabat(12, U11,  U22,  U12,  V1,  V2,
     &                  gcU11,gcU22,gcU12,gcV1,gcV2)



 1    format(7x,a9,i3,a2,d20.6)
 2    format(7x,d20.10)
 4    format(7x,20f16.5)

      return
      END

C!******************************************!!
      SUBROUTINE sumpoly(NC,ind0,bowint,coeff,V,gV,cpot)
C!******************************************!!
C    This part are all the same for U,U1,U12 except the poly normial
C    called
      implicit none
      integer NC
      integer ind0(NC,7)
      double precision bowint(7)
      double precision x(7),xin(7),dx(3),dxin(3)
      double precision g(3),gin(3)
      double precision g1(3),g1in(3)
      double precision x7(0:7)
      double precision dx7(0:7)
      double precision coeff(NC)
      double precision V
      double precision gV(7)
      character*3 cpot
      
      integer  i,j,k,p
      integer  i1,i2,i3,ip(3)
      double precision  S(3,3)
      double precision  fval
      double precision  fsum
      double precision  y(7)
      double precision  geoInt(7)
C     intemediate storage of derivatives
C dy(i,j) = dy(i)/dx(j)
      double precision  dgeoInt(7,7)
      double precision  dy(7,7)
      double precision  dfval(7),dfsum(7)
      integer indp(7)
      double precision  cos1,cos2,cos3
      double precision  dbeta

C Parameters
      double precision PI,deg2au
      parameter ( PI     = 3.1415926535897930d0 )
      parameter ( deg2au = PI/180.d0            )
      double precision HBa
      parameter (  HBa    = 0.5d0*PI                    )

C U11
      double precision FRb,FRc,ALP,cosGTa
C U22
      double precisionRe,FRd,dk
C U12
      double precision FRU12b,FRU12c
      if (cpot.ne.'U12') then
        cos1  = dcos(  bowint(4)*deg2au )
        cos2  = dcos(  bowint(5)*deg2au )
        cos3  = dcos(  bowint(6)*deg2au )
        dx(1) = dsin(  bowint(4)*deg2au )*deg2au
        dx(2) = dsin(  bowint(5)*deg2au )*deg2au
        dx(3) = dsin(  bowint(6)*deg2au )*deg2au
        x(7)  = bowint(7)
      endif
      dbeta = bowint(7)*deg2au-HBa
      if (cpot.eq.'U11') then
        FRb = .1872506360D+01
        FRc = .4278236310D+00
        ALP = .1009053173D+00
        cosGTa = -0.2870261592d0
        x(1) = dexp(-FRb*(bowint(1) - FRc))
        x(2) = dexp(-FRb*(bowint(2) - FRc))
        x(3) = dexp(-FRb*(bowint(3) - FRc))
        x(4) = cosGTa - cos1
        x(5) = cosGTa - cos2
        x(6) = cosGTa - cos3
        g(1) = dexp( -ALP * bowint(1)*bowint(1) )
        g(2) = dexp( -ALP * bowint(2)*bowint(2) )
        g(3) = dexp( -ALP * bowint(3)*bowint(3) )
        do i=0,2
          x7(i)  = dcos( dble(i) * dbeta )
          dx7(i) = -dble(i)*dsin( dble(i) * dbeta )*deg2au
        enddo
      elseif (cpot.eq.'U22') then
        FRb =  .1633658498D+01
        FRc =  .1987056751D+00
        ALP =  .3037603850D+00
        Re  =  .7147714247D+00
        FRd =  .4546981630D+01
        dk  =  -.2617146615D+00
        cosGTa = -0.5d0
        x(1) = dexp(-FRb*(bowint(1) - FRc))
        x(2) = dexp(-FRb*(bowint(2) - FRc))
        x(3) = dexp(-FRb*(bowint(3) - FRc))
        x(4) = cosGTa - cos1
        x(5) = cosGTa - cos2
        x(6) = cosGTa - cos3
        g(1) = dexp( -ALP * bowint(1)*bowint(1) )
        g(2) = dexp( -ALP * bowint(2)*bowint(2) )
        g(3) = dexp( -ALP * bowint(3)*bowint(3) )
        g1(1) = dexp(-FRd*(bowint(1)-Re)**2)
        g1(2) = dexp(-FRd*(bowint(2)-Re)**2)
        g1(3) = dexp(-FRd*(bowint(3)-Re)**2)
        do i=0,2
          x7(i)  = dcos( dble(i) * dbeta ) +
     &                0.5d0*dk*dbeta*dbeta/(0.05d0+dbeta*dbeta)
          dx7(i) = -dble(i)*dsin( dble(i) * dbeta ) * deg2au
     &          + dk*dbeta*deg2au/(0.05d0+dbeta*dbeta)
     &          - dk*dbeta**3*deg2au/(0.05d0+dbeta*dbeta)**2
        enddo
      elseif (cpot.eq.'U12') then
        FRU12c = .1501070926D+01
        FRU12b = 1.00d0
        g(1) =  dtanh(FRU12b*(bowint(1)-FRU12c))
        g(2) =  dtanh(FRU12b*(bowint(2)-FRU12c))
        g(3) =  dtanh(FRU12b*(bowint(3)-FRU12c))
        do i=3,5
          x7(i) = -dsin( dble(i)*dbeta )
          dx7(i)= -dcos( dble(i)*dbeta )*dble(i)*deg2au
        enddo
      endif
      
C     Initialize V,gV
      V = 0.0d0
      do i=1,7
        gV(i) = 0.d0
      enddo
      do i1=1,3
      do i2=1,3
      do i3=1,3
        if(i2==i1) cycle
        if(i3==i1) cycle
        if(i3==i2) cycle
C       symmetrize x to get y
        ip(1) = i1
        ip(2) = i2
        ip(3) = i3
        call  symOpt(3,ip,S)
        call  getSym7(S,x,xin)
        call  getSym3(S,dx,dxin)
        call  getSym3(S,g,gin)
        call  getSym7g(S,bowint,geoInt,dgeoInt)
C       symmetrize x to get y
        if (cpot.eq.'U22') then
          call getSym3(S,g1,g1in)
        endif
        do k=1,NC
          do i=1,7
            indp(i)  = ind0(k,i)
          enddo
          p = indp(7)

          if (cpot.eq.'U11') then
            call U11poly(indp,geoInt,xin,dxin,gin,x7(p),
     &                   dx7(p),fval,dfval)
          elseif (cpot.eq.'U22') then
            call U22poly(indp,geoInt,xin,dxin,gin,g1in,
     &                   x7(p),dx7(p),fval,dfval)
          elseif (cpot.eq.'U12') then
            call U12poly(indp,geoInt,gin,x7(p),
     &                   dx7(p),fval,dfval)
          endif
          V = V + fval * coeff(k)
          do i=1,7
C           apply chain rule
            do j=1,7
              gV(i) = gV(i) + dfval(j)*dgeoInt(j,i)*coeff(k)
            enddo
          enddo
        end do
      end do
      end do
      end do

 11   format(1x,a,7i2,10f21.4)
 12   format(1x,a5,10f21.4)
 13   format(1x,10f21.4)

      return
      END

!!**********************************************!!
      subroutine symOpt(N,ip,S)
!!**********************************************!!
      implicit none
      integer N,ip(N)
      double precision   S(N,N)

      integer i,j
      do i=1,N
      do j=1,N
        S(i,j) = 0.d0
      enddo
      enddo

      do i=1,N
        S(i,ip(i)) = 1.d0
      end do

      return
      END

!!****************************************!!
      SUBROUTINE getSym7g(S,x,y,dy)
!!****************************************!!
      implicit none
      integer N
      parameter (N=7)
      double precision x(N)
      double precision y(N)
C dy(i,j) = dy(i)/dx(j)
      double precision dy(N,N)
      
      integer  i,j,k
      double precision  S(3,3)
      do i=1,N
        y(i) = 0.d0
        do j=1,N
        dy(i,j) = 0.d0
        enddo
      enddo
      do i=1,3
      do j=1,3
        y(i) = y(i) + S(i,j)*x(j)
        y(i+3) = y(i+3) + S(i,j)*x(j+3)
        dy(i,j) = S(i,j)
        dy(i+3,j+3) = S(i,j)
      enddo
      enddo
      y(7)   = x(7)
      dy(7,7) = 1.d0

      return
      END

!!****************************************!!
      SUBROUTINE getSym7(S,x,y)
!!****************************************!!
      implicit none
      integer N
      parameter (N=7)
      double precision x(N)
      double precision y(N)
      
      integer  i,j,k
      double precision  S(3,3)
      do i=1,N
        y(i) = 0.d0
      enddo
      do i=1,3
      do j=1,3
        y(i) = y(i) + S(i,j)*x(j)
        y(i+3) = y(i+3) + S(i,j)*x(j+3)
      enddo
      enddo
      y(7)   = x(7)

      return
      END


!!****************************************!!
      SUBROUTINE getSym3(S,x,y)
!!****************************************!!
      implicit none
      double precision x(3)
      double precision y(3)
      
      integer  i,j,k
      double precision  S(3,3)
      do i=1,3
        y(i) = 0.d0
      enddo
      do i=1,3
      do j=1,3
        y(i) = y(i) + S(i,j)*x(j)
      enddo
      enddo

      return
      END


C
!!****************************************************************!!
      subroutine repulsive(rr,erep,derep,cpot)
!!****************************************************************!!
C Additional repuslive terms
      implicit none

      double precision rr(6),erep,derep(6)
      character*3 cpot
      double precision BNH1,BNH2,BHH
C     exponental coefficient
      double precision AlpNH1,AlpNH2,AlpHH
C     Center of Gaussian Function
      double precision R0NH,R0HH
C      parameter (R00 = 0.0d0)
      double precision R0
      double precision erepi,erepNH,erepHH
C second additional repulsive terms added to NH repulsion
      double precision erepNH1,erepNH2
C Used for asymtotic correction
      double precision ecorrlimit
C Derivatives: derepXH: 1-3 NH repul. derivs, 4-6, HH repul. derivs
      double precision derepXH(6),derepNH1,derepNH2,derepi
      double precision decorrlimit
C U11
C     DE between the dissociation limit and exp
      double precision deu11
      double precision shiftu11
C U22
C     DE between the dissociation limit and exp
      double precision deu22
C     energy shift to make the U22 (==V2) planar minimum in agreement
C     with exp. All the energy are relative to V1
      double precision shiftu22
      double precision deltaE
      double precision eshift
C     Re for the correction function
C     1.024d0: experimental NH distance for NH2 (2B1, ground state)
      double precision Recor
      double precision ecor1
      double precision decor1(6)
C     gamma
      double precision gam
C      parameter (gam  = 1.81d0 )
      parameter (gam  = 3.00d0 )
      parameter (Recor=2.0d0*1.024d0)
      integer i,j
C  misc
      double precision tmp
      double precision dtmp
      double precision dr,dr1,dr2
      double precision rr1

C      Re0 = 1.004d0

      R0NH  = -0.2d0
      R0HH  = -0.2d0
C              Exp        Theor
      shiftu11 =  0.00189d0 
      deu11    =  0.21967d0
      shiftu22 =  0.04726d0 
      deu22    =  0.19700d0

      if (cpot.eq.'U11') then
C Adjust AlpNH and BNH to make sure that  (U11)
C     rNH = 0.4, erepNH= 10.00
C     rNH = 0.5, erepNH=  1.91
C     rNH = 0.6, erepNH=  0.31
C     rNH = 0.7, erepNH=  0.04
C     rNH = 0.8, erepNH=  0.00
        AlpNH2  =   11.00d0
        BNH2    =  209.83d0
        eshift  = shiftu11
        deltaE  = deu11
      elseif (cpot.eq.'U22') then
C Adjust AlpNH and BNH to make sure that  (U22)
C     rNH = 0.4, erepNH= 20.00
C     rNH = 0.5, erepNH=  3.83
C     rNH = 0.6, erepNH=  0.61
C     rNH = 0.7, erepNH=  0.08
C     rNH = 0.8, erepNH=  0.01
        AlpNH2  =   11.00d0
        BNH2    =  419.66d0
        eshift  = shiftu22
        deltaE  = deu22
      endif
C Fitted
      BNH1    = .32363D+02
      BHH     = .12743D+02
      AlpNH1  = .40668D+01
      AlpHH   = .41735D+01

      erep    = 0.d0
      erepNH1 = 0.d0
      erepNH2 = 0.d0
      erepHH  = 0.d0
      ecorrlimit = 0.d0
      ecor1 = 0.d0
      IF (cpot.ne.'U12') THEN
      do i=1,3
        dr      = rr(i)-R0NH
        rr1     = 1.d0/rr(i)
        erepi   = dexp(-AlpNH1*dr*dr) * rr1
        erepNH1 = erepNH1+ erepi
        derepNH1= -erepi*( 2.d0*AlpNH1*dr + rr1 )

        erepi   = dexp(-AlpNH2*dr*dr) * rr1
        erepNH2 = erepNH2 + erepi
        derepNH2= -erepi*( 2.d0*AlpNH2*dr + rr1 )

        derepXH(i) = BNH1*derepNH1 + BNH2*derepNH2

C Correction to right limit
C        erepi   = 0.5d0*(1.d0+dtanh(gam*sum[rr(i)-Recor]) )
        tmp     = dtanh(gam*(rr(i)-Recor)) 
C       d(tanh) = (sech)**2 = 1-(tanh)**2
        dtmp    = gam*(1.d0-tmp*tmp)
        erepi   = 0.5d0*( 1.d0 + tmp )
        ecor1   = ecor1 + erepi
        decor1(i)  = 0.5d0*dtmp
      enddo
      erepNH     = BNH1*erepNH1 + BNH2*erepNH2

      do i=4,6
        dr      = rr(i)-R0HH
        rr1     = 1.d0/rr(i)
        erepi   = dexp(-AlpHH*dr*dr) * rr1
        erepHH  = erepHH + erepi
        derepXH(i) =-BHH*erepi*( 2.d0*AlpHH*dr + rr1 )
        decor1(i) = 0.d0
      enddo
      erepHH = BHH * erepHH

      do i=1,6
        decorrlimit =  deltaE*decor1(i)
        derep(i)  = derepXH(i) + decorrlimit
      enddo

      ecorrlimit = eshift + deltaE*ecor1
      erep = erepNH + erepHH + ecorrlimit
      ENDIF

      return
      end

!!****************************************************************!!
      SUBROUTINE U11poly(ind0,geoInt,xin,dxin,gin,x7,dx7,ans,g_ans)
!!****************************************************************!!
      implicit none
      integer  ind0(7)
      double precision geoInt(7),gin(3)
      double precision xin(7),dxin(3)
      double precision ans
      double precision g_ans(7)
!-------------------------------!
! Parameter List
!-------------------------------!
      double precision PI,FRb,FRc,GTa,HBa,ALP,cosGTa
      double precision deg2au
      parameter ( PI     = 3.1415926535897930d0 )
      parameter ( deg2au = PI/180.d0            )
C Experimental value: Landolt-Börnstein, Numerical Data and Function 
C Relationships in Science and Technology II(7,2) (Springer, New York,
C 1976). Cited in J. Pesonen, A. Miani, and L. Halonen, J. Chem. Phys.
C 115, 1243 (2001)
      parameter ( GTa    = 106.68*PI/180.0d0    )
C CoS(GTa) = CoS(106.28)
      parameter ( cosGTa = -0.2870261592d0      )
      parameter ( HBa    =  0.5d0*PI            )
!-------------------------------!
! Local Variables
!-------------------------------!
      double precision  g1
      double precision  g2
      double precision  g3
      double precision  x1
      double precision  x2
      double precision  x3
      double precision  x4
      double precision  x5
      double precision  x6
      double precision  x7
      double precision  y(7)
      double precision  beta,r1,r2,r3,the1,the2,the3
      integer  i,j,k,l,m,n,p
      integer  ind
!-------------------------------!
! Derivatives
!-------------------------------!
      double precision  y123,y4567
      double precision  dg1,dg2,dg3
      double precision  dx1,dx2,dx3,dx4,dx5,dx6,dx7
      double precision  dy71,dy72,dy73,dy77
      double precision  dy(3)
      double precision  dy42,dy43,dy44
      double precision  dy51,dy53,dy55
      double precision  dy61,dy62,dy66
      double precision  tmp

C  parameters to be optimized: 
C                 GTa(theta0),FRb(gamma),FRc(r0),ALP(delta)
      FRb = .1872506360D+01
      FRc = .4278236310D+00
      ALP = .1009053173D+00

!==========================================================================!
! geoInt contains the internals in the following format
! geoInt(1) : r1
! geoInt(2) : r2
! geoInt(3) : r3
! geoInt(4) : theta1
! geoInt(5) : theta2
! geoInt(6) : theta3
! geoInt(7) : beta
!==========================================================================!
        l = ind0(4)
        m = ind0(5)
        n = ind0(6)
        p = ind0(7)
C        r1 = geoInt(1)
C        r2 = geoInt(2)
C        r3 = geoInt(3)
C      the1 = geoInt(4)*deg2au
C      the2 = geoInt(5)*deg2au
C      the3 = geoInt(6)*deg2au
C      beta = geoInt(7)*deg2au

C      g1= dexp( -ALP * r1*r1 )
C      g2= dexp( -ALP * r2*r2 )
C      g3= dexp( -ALP * r3*r3 )
      g1= gin(1)
      g2= gin(2)
      g3= gin(3)

! dgi = dgi/dri (i=1,2,3)
      dg1= -2.d0*ALP*geoInt(1)*g1
      dg2= -2.d0*ALP*geoInt(2)*g2
      dg3= -2.d0*ALP*geoInt(3)*g3


! Energy and Derivatives
! f(r)  = 1 - exp[-B(r-C)]
! dxi = dxi/dri (i=1,2,3) A,B,C are constants
! df/dr = B exp[-B(r-C)]
      do i=1,3
        ind = ind0(i)
        if (ind.eq.0) then
          dy(i)= 0.d0
          y(i) = 1.d0
        else
C          x1   = dexp(-FRb*(geoInt(i) - FRc))
          x1   = xin(i)
          dx1  = FRb*x1
          x1   = 1.d0 - x1
          tmp  = x1**(ind-1)
          dy(i)= dble(ind)* tmp * dx1
          y(i) = tmp*x1
        endif
      enddo

! De = 1.d0 in GT function, geoInt(4-6) are theta1,theta2,theta3
C     x* = dcos(GTa) - dcos(  theta )
! dy4i = dy(4)/dri (i=1,2,3), dy44 = dy(4)/dtheta1
      if (l.eq.0) then
         dy42 = 0.d0
         dy43 = 0.d0
         dy44 = 0.d0
         y(4) = 1.d0
      else
C         x4  = cosGTa - dcos(  the1 )
C         dx4  = dsin(  the1 ) * deg2au
         x4  = xin(4)
         dx4 = dxin(1)
         tmp  = (x4*g2*g3)**(l-1)
         dy42 = dble(l)* tmp *  x4 * dg2 *  g3
         dy43 = dble(l)* tmp *  x4 *  g2 * dg3
         dy44 = dble(l)* tmp * dx4 *  g2 *  g3
         y(4) = tmp*x4*g2*g3
      endif

! dy5i = dy(5)/dri (i=1,2,3), dy55 = dy(5)/dtheta2
      if (m.eq.0) then
         dy51 = 0.d0
         dy53 = 0.d0
         dy55 = 0.d0
         y(5) = 1.d0
      else
C         x5  = cosGTa - dcos(  the2 )
C         dx5  = dsin(  the2 ) * deg2au
         x5  = xin(5)
         dx5 = dxin(2)
         tmp  = (x5*g1*g3)**(m-1)
         dy51 = dble(m)* tmp *  x5 * dg1 *  g3
         dy53 = dble(m)* tmp *  x5 *  g1 * dg3
         dy55 = dble(m)* tmp * dx5 *  g1 *  g3
         y(5) = tmp*x5*g1*g3
      endif

! dy6i = dy(6)/dri (i=1,2,3), dy66 = dy(6)/dtheta3
      if (n.eq.0) then
         dy61 = 0.d0
         dy62 = 0.d0
         dy66 = 0.d0
         y(6) = 1.d0
      else
C         x6  = cosGTa - dcos(  the3 )
C         dx6  = dsin(  the3 ) * deg2au
         x6  = xin(6)
         dx6 = dxin(3)
         tmp  = (x6*g1*g2)**(n-1)
         dy61 = dble(n)* tmp *  x6 * dg1 *  g2
         dy62 = dble(n)* tmp *  x6 *  g1 * dg2
         dy66 = dble(n)* tmp * dx6 *  g1 *  g2
         y(6) = tmp*x6*g1*g2
      endif

!  geoInt(7) beta; HBa PI/2
C      tmp = dble(p) * (beta-HBa)
C      x7  = dcos( tmp )
!   dx7/dbeta, not in degree
C      dx7 = -dble(p)*dsin( tmp ) * deg2au

! dy7i = dy(7)/dri (i=1,2,3), dy77 = dy(7)/dbeta
      tmp  = 1.d0-x7
      dy71 = tmp* dg1 *  g2 *  g3
      dy72 = tmp*  g1 * dg2 *  g3
      dy73 = tmp*  g1 *  g2 * dg3
      dy77 =-dx7*  g1 *  g2 *  g3
      y(7) = 1.0d0 + tmp*g1*g2*g3 

      y123 = y(1)*y(2)*y(3)
      y4567= y(4)*y(5)*y(6)*y(7)
      ans  = y123 * y4567

! Derivatives ans = y1 * y2 * y3 * y4 * y5 * y6 * y7
!     dans / dr1
      g_ans(1) = dy(1)* y(2) * y(3) * y4567       +
     &           y123 * y(4) * dy51 * y(6) * y(7) + 
     &           y123 * y(4) * y(5) * dy61 * y(7) + 
     &           y123 * y(4) * y(5) * y(6) * dy71
!     dans / dr2
      g_ans(2) = y(1) * dy(2)* y(3) * y4567       +
     &           y123 * dy42 * y(5) * y(6) * y(7) + 
     &           y123 * y(4) * y(5) * dy62 * y(7) + 
     &           y123 * y(4) * y(5) * y(6) * dy72
!     dans / dr3
      g_ans(3) = y(1) * y(2) * dy(3)* y4567       +
     &           y123 * dy43 * y(5) * y(6) * y(7) +
     &           y123 * y(4) * dy53 * y(6) * y(7) + 
     &           y123 * y(4) * y(5) * y(6) * dy73
!     dans / dtheta1
      g_ans(4) = y123 * dy44 * y(5) * y(6) * y(7)
!     dans / dtheta2
      g_ans(5) = y123 * y(4) * dy55 * y(6) * y(7)
!     dans / dtheta3
      g_ans(6) = y123 * y(4) * y(5) * dy66 * y(7)
!     dans / dbeta
      g_ans(7) = y123 * y(4) * y(5) * y(6) * dy77

 10   format(1x,12g12.5)

      return
      END

!!****************************************************************!!
      SUBROUTINE U22poly(ind0,geoInt,xin,dxin,gin,g1in,x7,dx7,ans,g_ans)
!!****************************************************************!!
      implicit none
      integer ind0(7)
      double precision geoInt(7),gin(3),g1in(3)
      double precision xin(7),dxin(3)
      double precision ans
      double precision g_ans(7)
!-------------------------------!
! Parameter List
!-------------------------------!
      double precision PI
      double precision FRb,FRc,GTa,HBa,ALP,Re,FRd,cosGTa
C      double precision deg2au
C      parameter (  PI     = 3.1415926535897930d0        )
C      parameter (  deg2au = PI/180.d0                   )
C      parameter (  GTa    = 2.0d0*PI/3.0d0              )
C CoS(GTa) = CoS(120.0) = -1/2
C      parameter (  cosGTa = -0.5d0                      )
C      parameter (  HBa    = 0.5d0*PI                    )
C Correction to umbralla bending vibration mode
      double precision dk
C FRd, and Re: used for Gaussian function
!-------------------------------!
! Local varibales
!-------------------------------!
      double precision  g1
      double precision  g2
      double precision  g3
      double precision  x1,x11
      double precision  x2,x21
      double precision  x3,x31
      double precision  x4
      double precision  x5
      double precision  x6
      double precision  x7
      double precision  y(7)
      double precision  beta,r1,r2,r3,the1,the2,the3
      integer  i,j,k,l,m,n,p
      integer  ind
!-------------------------------!
! Derivatives
!-------------------------------!
      double precision  y123,y4567
      double precision  dg1,dg2,dg3,g1g2,g2g3,g1g3
      double precision  dx1,dx2,dx3,dx4,dx5,dx6,dx7
      double precision  dy71,dy72,dy73,dy77
      double precision  dy(3)
      double precision  dy11,dy12,dy13
      double precision  dy21,dy22,dy23
      double precision  dy31,dy32,dy33
      double precision  dy42,dy43,dy44
      double precision  dy51,dy53,dy55
      double precision  dy61,dy62,dy66
      double precision  tmp,dtmp(3)
      double precision  dr

C
C Wed Oct 18 14:57:07 CDT 2006, with additional NH2 (linear)
      FRb =  .1633658498D+01
      FRc =  .1987056751D+00
      ALP =  .3037603850D+00
      Re  =  .7147714247D+00
      FRd =  .4546981630D+01
      dk  =  -.2617146615D+00
           
!======================================================================!
! geoInt contains the internals in the following format
! geoInt(1) : r1
! geoInt(2) : r2
! geoInt(3) : r3
! geoInt(4) : theta1
! geoInt(5) : theta2
! geoInt(6) : theta3
! geoInt(7) : beta
!======================================================================!
        l = ind0(4)
        m = ind0(5)
        n = ind0(6)
        p = ind0(7)
C        r1 = geoInt(1)
C        r2 = geoInt(2)
C        r3 = geoInt(3)
C      the1 = geoInt(4)*deg2au
C      the2 = geoInt(5)*deg2au
C      the3 = geoInt(6)*deg2au
C      beta = geoInt(7)*deg2au

C      g1 = dexp( -ALP * r1*r1 )
C      g2 = dexp( -ALP * r2*r2 )
C      g3 = dexp( -ALP * r3*r3 )
      g1= gin(1)
      g2= gin(2)
      g3= gin(3)
      g1g2 = g1*g2
      g1g3 = g1*g3
      g2g3 = g2*g3

! dgi = dgi/dri (i=1,2,3)
      dg1= -2.d0*ALP*geoInt(1)*g1
      dg2= -2.d0*ALP*geoInt(2)*g2
      dg3= -2.d0*ALP*geoInt(3)*g3


! Energy and Derivatives
! f(r)  = 1 - exp[-B(r-C)]
! dxi = dxi/dri (i=1,2,3) A,B,C are constants
! df/dr = B exp[-B(r-C)]
      do i=1,3
        ind = ind0(i)
        if (ind.eq.0) then
          dy(i)= 0.d0
          y(i) = 1.d0
        elseif (ind.le.2) then
C          x1   = dexp(-FRb*(geoInt(i) - FRc))
          x1   = xin(i)
          dx1  = FRb*x1
          x1   = 1.d0 - x1
          tmp  = x1**(ind-1)
          dy(i)= dble(ind)* tmp * dx1
          y(i) = tmp*x1
        endif
      enddo
C Li on Suggested by Don. (R-Re)^2 exp(-a (R-Re)^2)
      if (ind0(1).eq.3) then
        dr   = geoInt(1) - Re
C        x1   = dexp(-FRd*dr*dr)
        x1   = g1in(1)
        dx1  = -2.d0*FRd*dr*x1
        y(1) = x1 * g2g3
        dy11 = dx1* g2g3
        dy12 = x1 * dg2*g3
        dy13 = x1 * g2*dg3
      else
              dy11=dy(1)
              dy12=0.d0
              dy13=0.d0
      endif
      if (ind0(2).eq.3) then
        dr   = geoInt(2) - Re
C        x1   = dexp(-FRd*dr*dr)
        x1   = g1in(2)
        dx1  = -2.d0*FRd*dr*x1
        y(2) = x1 * g1g3
        dy21 = x1 * dg1*g3
        dy22 = dx1* g1g3
        dy23 = x1 * g1*dg3
      else
              dy21=0.d0
              dy22=dy(2)
              dy23=0.d0
      endif
      if (ind0(3).eq.3) then
        dr   = geoInt(3) - Re
C        x1   = dexp(-FRd*dr*dr)
        x1   = g1in(3)
        dx1  = -2.d0*FRd*dr*x1
        y(3) = x1 * g1g2
        dy31 = x1 * dg1*g2
        dy32 = x1 * g1*dg2
        dy33 = dx1* g1g2
      else
              dy31=0.d0
              dy32=0.d0
              dy33=dy(3)
      endif


C     x* = dcos(GTa) - dcos(  theta )
C     GTa = 120.0 degree, dcos(GTa) = -0.5d0
! dy5i = dy(5)/dri (i=1,2,3), dy55 = dy(5)/dtheta2

      if (l.eq.0) then
         dy42 = dg2*g3
         dy43 = g2*dg3
         dy44 = 0.d0
         y(4) = 1.d0 + g2g3
      else
C          x4  = cosGTa - dcos(  the1 )
         x4  = xin(4)
         dx4 = dxin(1)
C         dx4  = dsin(  the1 ) * deg2au
         tmp  = x4**(l-1)
         dy42 = tmp *  x4 * dg2 *  g3
         dy43 = tmp *  x4 *  g2 * dg3
         dy44 = dble(l)* tmp * dx4 *  g2g3
         y(4) = 1.d0+tmp*x4*g2g3
      endif

      if (m.eq.0) then
         dy51 = dg1*g3
         dy53 = g1*dg3
         dy55 = 0.d0
         y(5) = 1.d0 + g1g3
      else
C          x5  = cosGTa - dcos(  the2 )
C         dx5  = dsin(  the2 ) * deg2au
         x5  = xin(5)
         dx5 = dxin(2)
         tmp  = x5**(m-1)
         dy51 = tmp *  x5 * dg1 *  g3
         dy53 = tmp *  x5 *  g1 * dg3
         dy55 = dble(m)* tmp * dx5 *  g1g3
         y(5) = 1.d0+tmp*x5*g1g3
      endif

! dy6i = dy(6)/dri (i=1,2,3), dy66 = dy(6)/dtheta3
      if (n.eq.0) then
         dy61 = dg1*g2
         dy62 = g1*dg2
         dy66 = 0.d0
         y(6) = 1.d0 + g1g2
      else
C         x6  = cosGTa - dcos(  the3 )
C         dx6  = dsin(  the3 ) * deg2au
         x6  = xin(6)
         dx6 = dxin(3)
         tmp  = x6**(n-1)
         dy61 = tmp *  x6 * dg1 *  g2
         dy62 = tmp *  x6 *  g1 * dg2
         dy66 = dble(n)* tmp * dx6 *  g1g2
         y(6) = 1.d0+tmp*x6*g1g2
      endif

!  geoInt(7) beta; HBa PI/2
C      y(7) = 1 + (1-x7)*g1*g2*g3
C      tmp = dble(p) * (beta-HBa)
C      x7  = dcos( tmp )
!   dx7/dbeta, not in degree
C      dx7 = -dble(p)*dsin( tmp ) * deg2au
C      tmp = beta-HBa
C      x7 = dcos( dble(p) * tmp ) + 0.5d0*dk*tmp*tmp/(0.05d0+tmp*tmp)
C      dx7 = -dble(p)*dsin( dble(p) * tmp ) * deg2au
C     &      + dk*tmp*deg2au/(0.05d0+tmp*tmp)
C     &      - dk*tmp**3*deg2au/(0.05d0+tmp*tmp)**2

! dy7i = dy(7)/dri (i=1,2,3), dy77 = dy(7)/dbeta
      tmp  = 1.d0-x7
      dy71 = tmp* dg1 *  g2g3
      dy72 = tmp*  g1g3 * dg2
      dy73 = tmp*  g1g2 * dg3
      dy77 =-dx7*  g1 *  g2g3
      y(7) = 1.0d0 + tmp*g1*g2g3

      y123 = y(1)*y(2)*y(3)
      y4567= y(4)*y(5)*y(6)*y(7)
      ans  = y123 * y4567

! Derivatives ans = y1 * y2 * y3 * y4 * y5 * y6 * y7
!     dans / dr1
      g_ans(1) = dy11 * y(2) * y(3) * y4567       +
     &           y(1) * dy21 * y(3) * y4567       +
     &           y(1) * y(2) * dy31 * y4567       +
     &           y123 * y(4) * dy51 * y(6) * y(7) + 
     &           y123 * y(4) * y(5) * dy61 * y(7) + 
     &           y123 * y(4) * y(5) * y(6) * dy71
!     dans / dr2
      g_ans(2) = dy12 * dy(2)* y(3) * y4567       +
     &           y(1) * dy22 * y(3) * y4567       +
     &           y(1) * y(2) * dy32 * y4567       +
     &           y123 * dy42 * y(5) * y(6) * y(7) + 
     &           y123 * y(4) * y(5) * dy62 * y(7) + 
     &           y123 * y(4) * y(5) * y(6) * dy72
!     dans / dr3
      g_ans(3) = dy13 * y(2) * dy(3)* y4567       +
     &           y(1) * dy23 * y(3) * y4567       +
     &           y(1) * y(2) * dy33 * y4567       +
     &           y123 * dy43 * y(5) * y(6) * y(7) +
     &           y123 * y(4) * dy53 * y(6) * y(7) + 
     &           y123 * y(4) * y(5) * y(6) * dy73
!     dans / dtheta1
      g_ans(4) = y123 * dy44 * y(5) * y(6) * y(7)
!     dans / dtheta2
      g_ans(5) = y123 * y(4) * dy55 * y(6) * y(7)
!     dans / dtheta3
      g_ans(6) = y123 * y(4) * y(5) * dy66 * y(7)
!     dans / dbeta
      g_ans(7) = y123 * y(4) * y(5) * y(6) * dy77

 10   format(1x,20g12.5)

      return
      END

!!****************************************************************!!
      SUBROUTINE U12poly(ind0,geoInt,gin,HBU12,d7,ans,g_ans)
!!****************************************************************!!
      implicit none
      integer ind0(7)
      double precision  geoInt(7),gin(3)
      double precision  ans
C Derivatives of 7 internal coordinates
      double precision g_ans(7)
!-------------------------------!
! Parameter List
!-------------------------------!
      double precision PI
      parameter ( PI = 3.1415926535897930d0 )
!-------------------------------!
!! Parameter List (U12)
!!------------------------------!!
! FRU12a    parameters used to define angular for U22
! FRU12b    parameters used to define angular for U22
! FRU12c    parameters used to define angular for U22
      double precision  FRU12a,FRU12b,FRU12c,GTU12a,HBU12a
      double precision  ALP
      double precision deg2au
      parameter (deg2au = PI/180.d0 )

      parameter ( FRU12b = 1.00d0           )
C      parameter ( FRU12c = 2.0150d0        )
      parameter ( GTU12a = 2.0d0*PI/3.0d0   )
      parameter ( HBU12a = 0.5d0*PI         )

      
!-------------------------------!
! Misc
!-------------------------------!
      integer  i,j,ind
      double precision  z1(6),tz,tf,z123,z456
      double precision  HBU12,FRU121,FRU122,FRU123
      double precision theta,beta
      double precision y(7)
      double precision tmp
C Derivatives
      double precision  dz1(6)
      double precision  d7,dFR1,dFR2,dFR3

C  parameters to be optimized: 
C                 FRU12c(r04)
      FRU12c = .1501070926D+01


!======================================================================!
! geoInt contains the internals in the following format
! geoInt(1) : r1
! geoInt(2) : r2
! geoInt(3) : r3
! geoInt(4) : theta1
! geoInt(5) : theta2
! geoInt(6) : theta3
! geoInt(7) : beta
!======================================================================!
!
       do i=1,3
         ind   = ind0(i)
         if (ind.eq.0) then
           dz1(i) = 0.d0
           z1(i)  = 1.d0
         else
           z1(i)  = geoInt(i)**(ind-1)
           dz1(i) = dble(ind)*z1(i)
           z1(i)  = z1(i)*geoInt(i)
         endif
       end do

       do i=4,6
         ind    = ind0(i)
         if (ind.eq.0) then
           dz1(i) = 0.d0
           z1(i)  = 1.d0
         else
C  Unit degree/A
           theta  = geoInt(i) * deg2au
           z1(i)  = theta**(ind-1)
           dz1(i) = dble(ind)*z1(i) * deg2au
           z1(i)  = z1(i)*theta
         endif
       end do

C       beta  = geoInt(7)*deg2au
C       tmp   =  dble(ind0(7))*(beta-HBU12a)
C       HBU12 = -dsin( tmp )

!  d_HBU12 / d geoInt(7)
C       d7  = -dcos( tmp ) * dble(ind0(7)) * deg2au

! f(r) = 1 - tanh[-B(r-C)]
!  d tanh(x) = sech(x)^2 = 1 - tanh(x)^2
! df(r)/dr =  - {1 - tanh[-B(r-C)] }^2 * B

C       tmp    = dtanh( FRU12b*(geoInt(1)-FRU12c) )
       tmp    = gin(1)
       FRU121 = 1.d0 - tmp
       dFR1   = -(1.d0 - tmp*tmp)*FRU12b

C       tmp    = dtanh( FRU12b*(geoInt(2)-FRU12c) )
       tmp    = gin(2)
       FRU122 = 1.d0 - tmp
       dFR2   = -(1.d0 - tmp*tmp)*FRU12b

C       tmp    = dtanh( FRU12b*(geoInt(3)-FRU12c) )
       tmp    = gin(3)
       FRU123 = 1.d0 - tmp
       dFR3   = -(1.d0 - tmp*tmp)*FRU12b

       z123=  z1(1)*z1(2)*z1(3)
       z456=  z1(4)*z1(5)*z1(6)
       tz  =  z123*z456
       tf  =  FRU121 * FRU122 * FRU123
       ans =  tz * tf * HBU12
C       ans =  z1* HBU12 * FRU121 * FRU122 * FRU123

       g_ans(1)  =  dz1(1)*z1(2)*z1(3)*z456*tf*HBU12 +
     &              tz * HBU12 * dFR1 * FRU122 * FRU123
       g_ans(2)  =  z1(1)*dz1(2)*z1(3)*z456*tf*HBU12 +
     &              tz * HBU12 * FRU121 * dFR2 * FRU123
       g_ans(3)  =  z1(1)*z1(2)*dz1(3)*z456*tf*HBU12 +
     &              tz * HBU12 * FRU121 * FRU122 * dFR3
       g_ans(4)  =  z123*dz1(4)*z1(5)*z1(6)*tf*HBU12
       g_ans(5)  =  z123*z1(4)*dz1(5)*z1(6)*tf*HBU12
       g_ans(6)  =  z123*z1(4)*z1(5)*dz1(6)*tf*HBU12
       g_ans(7)  =  tz * tf * d7

 10   format(1x,20g12.5)

       return
       END

!!**************************************************!!
       SUBROUTINE getAdiabat(NV,  U11, U22, U12, V1, V2,
     &                           gU11,gU22,gU12,gV1,gV2)
!!**************************************************!!
       implicit none
C input
       integer NV
       double precision U11,U22,U12
       double precision gU11(NV),gU22(NV),gU12(NV)
C Output
       double precision V1,V2
       double precision gV1(NV),gV2(NV)
       
       integer i,j
       double precision x,y,dx,dy

       x = U11+U22
       y = dsqrt( ((U22-U11)**2) + (4.0d0*U12*U12) )

       V1 = 0.5d0*(x - y)
       V2 = 0.5d0*(x + y)

C Derivatives
       do i=1,NV
         dx = gU11(i) + gU22(i)
         dy = ( (U22-U11)*( gU22(i)-gU11(i) ) + 4.d0*U12*gU12(i) ) / y
         gV1(i) = 0.5d0 * (dx - dy)
         gV2(i) = 0.5d0 * (dx + dy)
       enddo

       return
       END

!!***************************************************************!!
      SUBROUTINE calcindU1(ind,M1,M2,M3,M4,M5,M6,M7,NC)
!!***************************************************************!!
      implicit none
      integer NC
      integer M1
      integer M2
      integer M3
      integer M4
      integer M5
      integer M6
      integer M7
      integer ind(NC,7)

      integer i1,i2,i3,i4,i5,i6,i7,iNC



       iNC = 0
       do i7=1, M7
       do i1=0, M1
       do i2=i1,M2
       do i3=i2,M3
       do i4=0, M4
       do i5=i4,M5
       do i6=i5,M6
        if( i1+i2+i3+i4+i5+i6 .eq. 0 ) cycle
        if(i1+i2+i3 .gt. 4) cycle
        if(i4+i5+i6 .gt. 2) cycle
        iNC = iNC + 1
        ind(iNC,1) = i1
        ind(iNC,2) = i2
        ind(iNC,3) = i3
        ind(iNC,4) = i4
        ind(iNC,5) = i5
        ind(iNC,6) = i6
        ind(iNC,7) = i7

      end do 
      end do 
      end do 
      end do 
      end do 
      end do 
      end do 

      return
      END

!!***************************************************************!!
      SUBROUTINE calcindU2(ind,M1,M2,M3,M4,M5,M6,M7,NC)
!!***************************************************************!!
      implicit none
      integer NC
      integer MNC
      integer M1
      integer M2
      integer M3
      integer M4
      integer M5
      integer M6
      integer M7
      integer ind(NC,7)

      integer i1,i2,i3,i4,i5,i6,i7,iNC



       iNC = 0
       do i7=0, M7
       do i1=0, M1
       do i2=i1,M2
       do i3=i2,M3
       do i4=0, M4
       do i5=i4,M5
       do i6=i5,M6
        if( i1+i2+i3+i4+i5+i6 .gt. 0 .and. i7.eq.0) cycle
        if( i1+i2+i3+i4+i5+i6 .eq. 0 .and. i7.gt.0) cycle
        if( i1+i2+i3 .gt. 5 ) cycle
        if( i4+i5+i6 .gt. 2 ) cycle
        if( i1+i2+i3 .ge. 4 ) then
          if ( i1.eq.2 .or. i2.eq.2 .or .i3.eq.2 ) cycle
        endif
        iNC = iNC + 1
        ind(iNC,1) = i1
        ind(iNC,2) = i2
        ind(iNC,3) = i3
        ind(iNC,4) = i4
        ind(iNC,5) = i5
        ind(iNC,6) = i6
        ind(iNC,7) = i7

      end do 
      end do 
      end do 
      end do 
      end do 
      end do 
      end do 
C      NC=iNC

      return
      END



!!***************************************************************!!
      SUBROUTINE calcindU12(ind,M1,M2,M3,M4,M5,M6,M7,NC)
!!***************************************************************!!
      implicit none
      integer NC
      integer M1
      integer M2
      integer M3
      integer M4
      integer M5
      integer M6
      integer M7
      integer ind(NC,7)

      integer i1,i2,i3,i4,i5,i6,i7,iNC



       iNC = 0
       do i7=3, M7
       do i1=0, M1
       do i2=i1,M2
       do i3=i2,M3
       do i4=0, M4
       do i5=i4,M5
       do i6=i5,M6
        if( i1+i2+i3+i4+i5+i6 .eq. 0 ) cycle
        if( i1+i2+i3 .gt. 2 ) cycle
        if( i4+i5+i6 .gt. 2 ) cycle
        iNC = iNC + 1
        ind(iNC,1) = i1
        ind(iNC,2) = i2
        ind(iNC,3) = i3
        ind(iNC,4) = i4
        ind(iNC,5) = i5
        ind(iNC,6) = i6
        ind(iNC,7) = i7

      end do 
      end do 
      end do 
      end do 
      end do 
      end do 
      end do 

      return
      END


C********************************************************
      subroutine potcoeff
C********************************************************
      implicit none

C
C Integer constants
      integer NV
      parameter (NV=7)
      integer NCU1,NCU2,NCU12
      parameter (NCU1=62,NCU2=71,NCU12=45)
      integer nhindU1(NCU1,NV)
      integer nhindU2(NCU2,NV)
      integer nhindU12(NCU12,NV)
      double precision nhcoeU1(NCU1)
      double precision nhcoeU2(NCU2)
      double precision nhcoeU12(NCU12)
      common /c_nhcoe/  nhcoeU1, nhcoeU2, nhcoeU12,
     &                  nhindU1, nhindU2, nhindU12

      integer i,j
      integer M1,M2,M3,M4,M5,M6,M7,M7U12,MU2
      parameter (M2=2,M3=3,M4=4,M5=5,M6=6,M7=7,M7U12=5)

C Get indices
      call calcindU1(nhindU1,M2,M2,M2,M2,M2,M2,M2,NCU1)
      call calcindU2(nhindU2,M3,M3,M3,M2,M2,M2,M2,NCU2)
      call calcindU12(nhindU12,M2,M2,M2,M2,M2,M2,M5,NCU12)
 
C U11 and U22 fitted with additional points of linear NH2 
 
C  U11 Coefficients:
       nhcoeU1(  1)=     -.1698267005D+01
       nhcoeU1(  2)=     -.3116975297D+01
       nhcoeU1(  3)=      .4989324584D+02
       nhcoeU1(  4)=      .6140536234D+02
       nhcoeU1(  5)=     -.9137903351D+02
       nhcoeU1(  6)=     -.4170308921D+02
       nhcoeU1(  7)=     -.2822911145D+03
       nhcoeU1(  8)=     -.1324724944D+03
       nhcoeU1(  9)=      .4554289660D+02
       nhcoeU1( 10)=      .6482512608D+02
       nhcoeU1( 11)=      .1622981341D+03
       nhcoeU1( 12)=     -.1418440334D+03
       nhcoeU1( 13)=      .3128761841D+03
       nhcoeU1( 14)=      .2291158383D+03
       nhcoeU1( 15)=      .4212981689D+03
       nhcoeU1( 16)=      .5904053030D+03
       nhcoeU1( 17)=     -.2011576383D+03
       nhcoeU1( 18)=     -.2847074833D+03
       nhcoeU1( 19)=     -.4207881721D+03
       nhcoeU1( 20)=     -.3401129381D+03
       nhcoeU1( 21)=     -.1017600665D+03
       nhcoeU1( 22)=     -.3135766858D+02
       nhcoeU1( 23)=      .7766878674D+02
       nhcoeU1( 24)=     -.1863622757D+03
       nhcoeU1( 25)=     -.2454112660D+03
       nhcoeU1( 26)=     -.2311744548D+03
       nhcoeU1( 27)=     -.7810834823D+02
       nhcoeU1( 28)=      .1476582353D+03
       nhcoeU1( 29)=      .2809941380D+03
       nhcoeU1( 30)=      .3033038990D+03
       nhcoeU1( 31)=      .6163060145D+02
       nhcoeU1( 32)=      .1175366540D+01
       nhcoeU1( 33)=      .3888738002D+01
       nhcoeU1( 34)=     -.3651170315D+02
       nhcoeU1( 35)=     -.3522094635D+02
       nhcoeU1( 36)=      .8722843200D+02
       nhcoeU1( 37)=      .7398300089D+02
       nhcoeU1( 38)=      .1872947617D+03
       nhcoeU1( 39)=      .1217028644D+03
       nhcoeU1( 40)=     -.3865307026D+02
       nhcoeU1( 41)=     -.9112045320D+02
       nhcoeU1( 42)=     -.8906173604D+02
       nhcoeU1( 43)=      .5130386478D+02
       nhcoeU1( 44)=     -.2935045013D+03
       nhcoeU1( 45)=     -.3045399445D+03
       nhcoeU1( 46)=     -.2456762591D+03
       nhcoeU1( 47)=     -.4827738566D+03
       nhcoeU1( 48)=      .1665284893D+03
       nhcoeU1( 49)=      .3478380563D+03
       nhcoeU1( 50)=      .1826085704D+03
       nhcoeU1( 51)=      .3027131358D+03
       nhcoeU1( 52)=      .1219769642D+03
       nhcoeU1( 53)=      .4171232808D+02
       nhcoeU1( 54)=     -.9647425161D+01
       nhcoeU1( 55)=      .2035053367D+03
       nhcoeU1( 56)=      .2297924112D+03
       nhcoeU1( 57)=      .2731855209D+03
       nhcoeU1( 58)=      .3746015525D+02
       nhcoeU1( 59)=     -.1574954942D+03
       nhcoeU1( 60)=     -.2708242232D+03
       nhcoeU1( 61)=     -.3486651786D+03
       nhcoeU1( 62)=     -.1927783168D+02
 
C  U22 Coefficients:
       nhcoeU2(  1)=     -.5839297267D+01
       nhcoeU2(  2)=      .1948308393D+03
       nhcoeU2(  3)=      .1476399426D+03
       nhcoeU2(  4)=     -.1952770539D+03
       nhcoeU2(  5)=     -.4956220164D+03
       nhcoeU2(  6)=      .1455034861D+04
       nhcoeU2(  7)=     -.6852363440D+03
       nhcoeU2(  8)=     -.7980707978D+03
       nhcoeU2(  9)=     -.7150307241D+02
       nhcoeU2( 10)=     -.1898884973D+04
       nhcoeU2( 11)=      .6944385319D+03
       nhcoeU2( 12)=      .1278073813D+04
       nhcoeU2( 13)=      .2496161582D+03
       nhcoeU2( 14)=     -.6605419506D+03
       nhcoeU2( 15)=     -.2259061202D+03
       nhcoeU2( 16)=      .3208341955D+03
       nhcoeU2( 17)=      .1438880740D+04
       nhcoeU2( 18)=     -.2462117089D+04
       nhcoeU2( 19)=      .1264999815D+03
       nhcoeU2( 20)=      .1618808427D+04
       nhcoeU2( 21)=     -.4277636246D+03
       nhcoeU2( 22)=      .2889205530D+04
       nhcoeU2( 23)=     -.4525182746D+03
       nhcoeU2( 24)=     -.2216782595D+04
       nhcoeU2( 25)=     -.5474397436D+03
       nhcoeU2( 26)=      .9523771905D+03
       nhcoeU2( 27)=      .8001804807D+03
       nhcoeU2( 28)=     -.2413673183D+03
       nhcoeU2( 29)=     -.4689796908D+03
       nhcoeU2( 30)=     -.1226082702D+03
       nhcoeU2( 31)=      .2013096370D+03
       nhcoeU2( 32)=      .2497300043D+03
       nhcoeU2( 33)=      .2354985426D+03
       nhcoeU2( 34)=     -.1609355796D+03
       nhcoeU2( 35)=     -.5933048064D+03
       nhcoeU2( 36)=     -.2301959837D+03
       nhcoeU2( 37)=     -.1750594296D+03
       nhcoeU2( 38)=     -.1343884916D+03
       nhcoeU2( 39)=      .1997871612D+03
       nhcoeU2( 40)=      .4919735429D+03
       nhcoeU2( 41)=     -.1486208797D+04
       nhcoeU2( 42)=      .6482490789D+03
       nhcoeU2( 43)=      .7895412677D+03
       nhcoeU2( 44)=      .3391285547D+02
       nhcoeU2( 45)=      .1900289391D+04
       nhcoeU2( 46)=     -.6724668674D+03
       nhcoeU2( 47)=     -.1274908946D+04
       nhcoeU2( 48)=     -.2563702497D+03
       nhcoeU2( 49)=      .6673491569D+03
       nhcoeU2( 50)=      .2226064703D+03
       nhcoeU2( 51)=     -.3178753276D+03
       nhcoeU2( 52)=     -.1352904130D+04
       nhcoeU2( 53)=      .2450815486D+04
       nhcoeU2( 54)=     -.1269876680D+03
       nhcoeU2( 55)=     -.1626526640D+04
       nhcoeU2( 56)=      .4191476857D+03
       nhcoeU2( 57)=     -.2837798016D+04
       nhcoeU2( 58)=      .4598348283D+03
       nhcoeU2( 59)=      .2208871680D+04
       nhcoeU2( 60)=      .5496794539D+03
       nhcoeU2( 61)=     -.9765928983D+03
       nhcoeU2( 62)=     -.7638623676D+03
       nhcoeU2( 63)=      .2378197831D+03
       nhcoeU2( 64)=      .4359353643D+03
       nhcoeU2( 65)=      .9157000100D+02
       nhcoeU2( 66)=     -.2028118062D+03
       nhcoeU2( 67)=     -.2304177594D+03
       nhcoeU2( 68)=     -.2325038433D+03
       nhcoeU2( 69)=      .1797589106D+03
       nhcoeU2( 70)=      .5547260471D+03
       nhcoeU2( 71)=      .2316886253D+03
 
C  U12 Coefficients:
       nhcoeU12( 1)=     -.1370792834D+01
       nhcoeU12( 2)=      .6836772012D+00
       nhcoeU12( 3)=      .6331956838D+00
       nhcoeU12( 4)=      .1048352694D+01
       nhcoeU12( 5)=      .1450863054D+01
       nhcoeU12( 6)=     -.6761575220D+00
       nhcoeU12( 7)=     -.1763525455D+01
       nhcoeU12( 8)=     -.2757812968D+00
       nhcoeU12( 9)=     -.1771035818D+00
       nhcoeU12(10)=      .1521156999D+00
       nhcoeU12(11)=      .3338335016D+00
       nhcoeU12(12)=     -.7011767589D+00
       nhcoeU12(13)=      .1057293370D+00
       nhcoeU12(14)=     -.1499798375D+00
       nhcoeU12(15)=      .8059440388D+00
       nhcoeU12(16)=      .1100511326D+01
       nhcoeU12(17)=     -.9054527734D+00
       nhcoeU12(18)=     -.1502093390D+00
       nhcoeU12(19)=     -.5755857753D+00
       nhcoeU12(20)=     -.1859473317D+01
       nhcoeU12(21)=      .1376357981D+01
       nhcoeU12(22)=      .8781480771D+00
       nhcoeU12(23)=     -.3398810498D+00
       nhcoeU12(24)=      .1058482524D+01
       nhcoeU12(25)=     -.8694281520D+00
       nhcoeU12(26)=      .1168933914D+00
       nhcoeU12(27)=      .9741155480D+00
       nhcoeU12(28)=     -.5185373065D+00
       nhcoeU12(29)=      .4463417737D+00
       nhcoeU12(30)=     -.8328439796D+00
       nhcoeU12(31)=     -.4614413328D+00
       nhcoeU12(32)=      .3513046496D+00
       nhcoeU12(33)=     -.6307182534D-01
       nhcoeU12(34)=      .6508886978D+00
       nhcoeU12(35)=      .3787965679D+00
       nhcoeU12(36)=     -.6678165549D+00
       nhcoeU12(37)=      .1524235094D+00
       nhcoeU12(38)=      .1114227008D+00
       nhcoeU12(39)=     -.3191010676D+00
       nhcoeU12(40)=      .5041174980D+00
       nhcoeU12(41)=     -.2955290687D+00
       nhcoeU12(42)=     -.7789245841D+00
       nhcoeU12(43)=      .5060705599D+00
       nhcoeU12(44)=     -.2054781302D+00
       nhcoeU12(45)=      .1836178250D+00
 
 

        return
        END
C                           DISCLAIMER
C
C   This file was generated on 05/01/06 by the version of
C   ADIFOR compiled on June, 1998.
C
C   ADIFOR was prepared as an account of work sponsored by an
C   agency of the United States Government, Rice University, and
C   the University of Chicago.  NEITHER THE AUTHOR(S), THE UNITED
C   STATES GOVERNMENT NOR ANY AGENCY THEREOF, NOR RICE UNIVERSITY,
C   NOR THE UNIVERSITY OF CHICAGO, INCLUDING ANY OF THEIR EMPLOYEES
C   OR OFFICERS, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
C   ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETE-
C   NESS, OR USEFULNESS OF ANY INFORMATION OR PROCESS DISCLOSED, OR
C   REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
C
C!*********************************************************************!
      subroutine toBowman(x, bow, g_bow)
C!*********************************************************************!
        implicit none
        integer natom
        parameter (natom = 4)
C     X in one array with x1,y1,z1,...x4,y4,z4
        double precision x(3 * natom)
        double precision bow(7)
        double precision pi
        parameter (pi = 3.1415926535897930d0)
C to save the first three bond distances. If they are calculated in any
C subroutines, avoid to calculate them again
       double precision rrrrr(3)
       logical lrrrrr
       common /calcrrrr/ rrrrr,lrrrrr
C
C This function returns the Bowman  
C coordinates from cartesian coordinates
C All angles are in DEGREES
C
C Output:
C bow(1) : r1
C bow(2) : r2
C bow(3) : r3
C bow(4) : theta1
C bow(5) : theta2
C bow(6) : theta3
C bow(7) : beta
C
C Input: 
C coord(1,:) : N
C coord(2,:) : H1
C coord(3,:) : H2
C coord(4,:) : H3
C
        double precision r(3)
        double precision beta(3)
        double precision theta(3)
C
C------------------------!
C Parameter list
C------------------------!
        integer iout1, inp1
        double precision tol
        parameter (iout1 = 0)
        parameter (inp1 = 5)
        parameter (tol = 1.0d-4)
C
C------------------------!
C Geometry
C------------------------!
        double precision cartn(natom, 3)
        double precision cartp(natom, 3)
C
C------------------------!
C Unit Vectors
C------------------------!
        double precision ua(3), ub(3), uc(3)
        double precision ut(3)
        double precision uba(3), uca(3), ucb(3)
C
C------------------------!
C Eq. of Plane
C------------------------!
        double precision a1
        double precision a2
        double precision a3
        double precision d
        double precision dist
C
C------------------------!
C Misc
C------------------------!
        integer i, j, k, n, m
        double precision xt, yt, zt, w
        double precision b(3, 3)
        double precision a(3, 3)
C
C=====================================================================
C Get data from user
C
C---------------------------------------------------------------------
C Now we will evaluate the direction of the trisector vector 
C the unit vectors along Ha, Hb and Hc bonds are labelled as 
C uA, uB, and uC. The trisector direction is refered as uT 
C
C cartN has the origin on the N atom
        integer g_pmax_
        parameter (g_pmax_ = 12)
        integer g_i_
        double precision d5_b, d3_p, d7_b, d2_v, d3_v, d8_b, d2_b, d3_b,
     * d1_w, d4_v
        double precision d5_v, d1_p, d7_v, d2_p, d4_b, g_cartn(g_pmax_, 
     *natom, 3), g_x(g_pmax_, 3 * natom), g_d1_w(g_pmax_), g_r(g_pmax_, 
     *3), g_ua(g_pmax_, 3)
        double precision g_ub(g_pmax_, 3), g_uc(g_pmax_, 3), g_uba(g_pma
     *x_, 3), g_uca(g_pmax_, 3), g_ucb(g_pmax_, 3), g_a(g_pmax_, 3, 3), 
     *g_b(g_pmax_, 3, 3), g_ut(g_pmax_, 3), g_w(g_pmax_), g_beta(g_pmax_
     *, 3)
        double precision g_a1(g_pmax_), g_a2(g_pmax_), g_a3(g_pmax_), g_
     *d(g_pmax_), g_xt(g_pmax_), g_yt(g_pmax_), g_zt(g_pmax_), g_cartp(g
     *_pmax_, natom, 3), g_dist(g_pmax_), g_theta(g_pmax_, 3)
        double precision g_bow(g_pmax_, 7)
        integer g_ehfid
        save g_zt, g_cartp, g_dist, g_theta
        save g_b, g_ut, g_w, g_beta, g_a1, g_a2, g_a3, g_d, g_xt, g_yt
        save g_cartn, g_d1_w, g_r, g_ua, g_ub, g_uc, g_uba, g_uca, g_ucb
     *, g_a
        external g_distplane
        external g_det3
        data g_ehfid /0/
C
C        call ehsfid(g_ehfid, 'toBowman','g_toBowman.f')
        do i=1,12
        do j=1,12
          if (i.eq.j) then
                  g_x(i,j)=1.d0
          else
                  g_x(i,j)=0.d0
          endif
        enddo
        enddo
C
        do i = 1, natom
          do g_i_ = 1, g_pmax_
            g_cartn(g_i_, i, 1) = -g_x(g_i_, 1) + g_x(g_i_, 3 * i - 2)
          enddo
          cartn(i, 1) = x(3 * i - 2) - x(1)
C--------
          do g_i_ = 1, g_pmax_
            g_cartn(g_i_, i, 2) = -g_x(g_i_, 2) + g_x(g_i_, 3 * i - 1)
          enddo
          cartn(i, 2) = x(3 * i - 1) - x(2)
C--------
          do g_i_ = 1, g_pmax_
            g_cartn(g_i_, i, 3) = -g_x(g_i_, 3) + g_x(g_i_, 3 * i)
          enddo
          cartn(i, 3) = x(3 * i) - x(3)
C--------
        enddo
C
C Get the bond distances
        do i = 2, natom
          d2_v = cartn(i, 1) * cartn(i, 1)
          d3_p = 2.0d0 * cartn(i, 1)
          d4_v = cartn(i, 2) * cartn(i, 2)
          d2_p = 2.0d0 * cartn(i, 2)
          d7_v = cartn(i, 3) * cartn(i, 3)
          d1_p = 2.0d0 * cartn(i, 3)
          do g_i_ = 1, g_pmax_
            g_d1_w(g_i_) = d1_p * g_cartn(g_i_, i, 3) + d2_p * g_cartn(g
     *_i_, i, 2) + d3_p * g_cartn(g_i_, i, 1)
          enddo
          if (lrrrrr) then
                  d2_v = rrrrr(i-1)
          else
            d1_w = d2_v + d4_v + d7_v
            d2_v = sqrt(d1_w)
            rrrrr(i-1) = d2_v
          endif

C RVM Corrected
          if ( d1_w .gt. 0.0d0 ) then
C          if (d2_v .gt. 0.d0) then
            d1_p = 1.0d0 / (2.0d0 *  d2_v)
          endif
          do g_i_ = 1, g_pmax_
            g_r(g_i_, i - 1) = d1_p * g_d1_w(g_i_)
          enddo
          r(i - 1) = d2_v

C--------
        enddo
        lrrrrr=.true.
C
C get the units vectors along Ha, Hb, and Hc
        do i = 1, 3
          d3_v = cartn(2, i) / r(1)
          d2_b = 1.0d0 / r(1)
          d3_b = (-d3_v) / r(1)
          do g_i_ = 1, g_pmax_
            g_ua(g_i_, i) = d3_b * g_r(g_i_, 1) + d2_b * g_cartn(g_i_, 2
     *, i)
          enddo
          ua(i) = d3_v
C--------
          d3_v = cartn(3, i) / r(2)
          d2_b = 1.0d0 / r(2)
          d3_b = (-d3_v) / r(2)
          do g_i_ = 1, g_pmax_
            g_ub(g_i_, i) = d3_b * g_r(g_i_, 2) + d2_b * g_cartn(g_i_, 3
     *, i)
          enddo
          ub(i) = d3_v
C--------
          d3_v = cartn(4, i) / r(3)
          d2_b = 1.0d0 / r(3)
          d3_b = (-d3_v) / r(3)
          do g_i_ = 1, g_pmax_
            g_uc(g_i_, i) = d3_b * g_r(g_i_, 3) + d2_b * g_cartn(g_i_, 4
     *, i)
          enddo
          uc(i) = d3_v
C--------
        enddo
C
C The trisector direction satisfies the following eq 
C (by definition of it being trisector)
C uT . uA = cos(beta)
C uT . uB = cos(beta)
C uT . uC = cos(beta)
C
C These three equation can be modified to:
C uT . (uB-uA) = 0
C uT . (uC-uA) = 0
C uT . (uC-uB) = 0
C
        do i = 1, 3
          do g_i_ = 1, g_pmax_
            g_uba(g_i_, i) = -g_ua(g_i_, i) + g_ub(g_i_, i)
          enddo
          uba(i) = ub(i) - ua(i)
C--------
          do g_i_ = 1, g_pmax_
            g_uca(g_i_, i) = -g_ua(g_i_, i) + g_uc(g_i_, i)
          enddo
          uca(i) = uc(i) - ua(i)
C--------
          do g_i_ = 1, g_pmax_
            g_ucb(g_i_, i) = -g_ub(g_i_, i) + g_uc(g_i_, i)
          enddo
          ucb(i) = uc(i) - ub(i)
C--------
        enddo
C
C Find the eq of the plane defined by 
C points uBA, uCA, and uCB.
C The normal to that plane is the trisector
C directions 
C
C The eq of a plane 
C      Ax + By + Cz + D = 0
C defined by points:
C (x1,y1,z1) (x2,y2,z2) and (x3,y3,z3) is 
C
C     | 1 y1 z1 |      | x1 1 z1 |      | x1 y1 1 |   | x1 y1 z1 |
C A = | 1 y2 z2 |  B = | x2 1 z2 |  C = | x2 y2 1 |  D = -| x2 y2 z2 |
C     | 1 y3 z3 |      | x3 1 z3 |      | x3 y3 1 |       | x3 y3 z3 |
C
C
C The eq of the normal to the plane is given by the vector {A,B,C}
C
        do i = 1, 3
          do g_i_ = 1, g_pmax_
            g_a(g_i_, 1, i) = g_uba(g_i_, i)
          enddo
          a(1, i) = uba(i)
C--------
          do g_i_ = 1, g_pmax_
            g_a(g_i_, 2, i) = g_uca(g_i_, i)
          enddo
          a(2, i) = uca(i)
C--------
          do g_i_ = 1, g_pmax_
            g_a(g_i_, 3, i) = g_ucb(g_i_, i)
          enddo
          a(3, i) = ucb(i)
C--------
        enddo
        do i = 1, 3
          do j = 1, 3
            do k = 1, 3
              do g_i_ = 1, g_pmax_
                g_b(g_i_, j, k) = g_a(g_i_, j, k)
              enddo
              b(j, k) = a(j, k)
C--------
            enddo
          enddo
          do j = 1, 3
            do g_i_ = 1, g_pmax_
              g_b(g_i_, j, i) = 0.0d0
            enddo
            b(j, i) = 1.0d0
C--------
          enddo
          call g_det3(b, g_b, ut(i), g_ut(1, i))
        enddo
C
        d4_b = ut(3) + ut(3)
        d7_b = ut(2) + ut(2)
        d8_b = ut(1) + ut(1)
        do g_i_ = 1, g_pmax_
          g_d1_w(g_i_) = d4_b * g_ut(g_i_, 3) + d7_b * g_ut(g_i_, 2) + d
     *8_b * g_ut(g_i_, 1)
        enddo
        d1_w = ut(1) * ut(1) + ut(2) * ut(2) + ut(3) * ut(3)
        d2_v = sqrt(d1_w)

        if ( d1_w .gt. 0.0d0 ) then
           d1_p = 1.0d0 / (2.0d0 *  d2_v)
        else
C           call ehufDO (9,d1_w, d2_v, d1_p,
C     +g_ehfid,
C     +302)
        endif
        do g_i_ = 1, g_pmax_
          g_w(g_i_) = d1_p * g_d1_w(g_i_)
        enddo
        w = d2_v
C--------
        do i = 1, 3
          d3_v = ut(i) / w
          d2_b = 1.0d0 / w
          d3_b = (-d3_v) / w
          do g_i_ = 1, g_pmax_
            g_ut(g_i_, i) = d3_b * g_w(g_i_) + d2_b * g_ut(g_i_, i)
          enddo
          ut(i) = d3_v
C--------
        enddo
C
C Get the value of  beta (angle between any H-N and {A,B,C}) 
        do g_i_ = 1, g_pmax_
          g_w(g_i_) = ut(3) * g_ua(g_i_, 3) + ua(3) * g_ut(g_i_, 3) + ut
     *(2) * g_ua(g_i_, 2) + ua(2) * g_ut(g_i_, 2) + ut(1) * g_ua(g_i_, 1
     *) + ua(1) * g_ut(g_i_, 1)
        enddo
        w = ut(1) * ua(1) + ut(2) * ua(2) + ut(3) * ua(3)
C--------
        d2_v = acos(w)
        
        if ( abs(w) .lt. 1.0d0 ) then
           d1_p = -1.0d0 / sqrt ((1.0d0-w)*(1.0d0+w))
        else
C           call ehufDO (14,w, d2_v, d1_p,
C     +g_ehfid,
C     +335)
        endif
        d4_b = 1.0d0 / pi * 180.0d0 * d1_p
        do g_i_ = 1, g_pmax_
          g_beta(g_i_, 1) = d4_b * g_w(g_i_)
        enddo
        beta(1) = d2_v * 180.0d0 / pi
C--------
C     beta(2) = dacos( dot_product(uT,uB) ) * 180.0d0/PI
C     beta(3) = dacos( dot_product(uT,uC) ) * 180.0d0/PI
C
C---------------------------------------------------------------------!
C Now we define the eq. of the plane
C     a1x + a2y + a3z + d = 0
C such that the normal to the plane is uT and 
C the Ha hydrogen lies on that plane
C
C The values of {a1,a2,a3} are the same as
C the values of uT(1),uT(2),uT(3). We will use
C the coordinates of Ha to find the value of d
C
        do g_i_ = 1, g_pmax_
          g_a1(g_i_) = g_ut(g_i_, 1)
        enddo
        a1 = ut(1)
C--------
        do g_i_ = 1, g_pmax_
          g_a2(g_i_) = g_ut(g_i_, 2)
        enddo
        a2 = ut(2)
C--------
        do g_i_ = 1, g_pmax_
          g_a3(g_i_) = g_ut(g_i_, 3)
        enddo
        a3 = ut(3)
C--------
        do g_i_ = 1, g_pmax_
          g_w(g_i_) = ut(3) * g_cartn(g_i_, 2, 3) + cartn(2, 3) * g_ut(g
     *_i_, 3) + ut(2) * g_cartn(g_i_, 2, 2) + cartn(2, 2) * g_ut(g_i_, 2
     *) + ut(1) * g_cartn(g_i_, 2, 1) + cartn(2, 1) * g_ut(g_i_, 1)
        enddo
        w = ut(1) * cartn(2, 1) + ut(2) * cartn(2, 2) + ut(3) * cartn(2,
     * 3)
C--------
        do g_i_ = 1, g_pmax_
          g_d(g_i_) = -g_w(g_i_)
        enddo
        d = -w
C--------
C
C---------------------------------------------------------------------!
C Project all the atoms on the plane
C The projected geometry is known as cartP
C 
C The project of a point A on the plane P is denoted by point B
C The relation between rA and rB is given as :
C rB = rA - (dist*uT)
C where dist is the distance of point A from the plane.
C
        do i = 1, natom
          do g_i_ = 1, g_pmax_
            g_xt(g_i_) = g_cartn(g_i_, i, 1)
          enddo
          xt = cartn(i, 1)
C--------
          do g_i_ = 1, g_pmax_
            g_yt(g_i_) = g_cartn(g_i_, i, 2)
          enddo
          yt = cartn(i, 2)
C--------
          do g_i_ = 1, g_pmax_
            g_zt(g_i_) = g_cartn(g_i_, i, 3)
          enddo
          zt = cartn(i, 3)
C--------
          call g_distplane(a1, g_a1, a2, g_a2, a3, g_a3, d, g_d, xt, g_x
     *t, yt, g_yt, zt, g_zt, dist, g_dist)
          do j = 1, 3
            do g_i_ = 1, g_pmax_
              g_cartp(g_i_, i, j) = (-dist) * g_ut(g_i_, j) + (-ut(j)) *
     * g_dist(g_i_) + g_cartn(g_i_, i, j)
            enddo
            cartp(i, j) = cartn(i, j) - dist * ut(j)
C--------
          enddo
        enddo
C
        do i = 2, natom
          do j = 1, 3
            do g_i_ = 1, g_pmax_
              g_cartp(g_i_, i, j) = -g_cartp(g_i_, 1, j) + g_cartp(g_i_,
     * i, j)
            enddo
            cartp(i, j) = cartp(i, j) - cartp(1, j)
C--------
          enddo
        enddo
C
        do i = 1, 3
          do g_i_ = 1, g_pmax_
            g_cartp(g_i_, 1, i) = 0.0d0
          enddo
          cartp(1, i) = 0.0d0
C--------
        enddo
C---------------------------------------------------------
C The theta are labelled as:
C H1-N-H2 : theta(1)
C H1-N-H3 : theta(2)
C H2-N-H3 : theta(3)
C
C Note that theta <= 180 because we are using acos
C and acos gives angle in the range [-PI,PI]
C Also all the angles are +ve
C     
C
        k = 0
        do j = 2, natom
          do i = j + 1, natom
            k = k + 1
            d2_v = cartp(i, 1) * cartp(i, 1)
            d3_p = 2.0d0 * cartp(i, 1)
            d4_v = cartp(i, 2) * cartp(i, 2)
            d2_p = 2.0d0 * cartp(i, 2)
            d7_v = cartp(i, 3) * cartp(i, 3)
            d1_p = 2.0d0 * cartp(i, 3)
            do g_i_ = 1, g_pmax_
              g_d1_w(g_i_) = d1_p * g_cartp(g_i_, i, 3) + d2_p * g_cartp
     *(g_i_, i, 2) + d3_p * g_cartp(g_i_, i, 1)
            enddo
            d1_w = d2_v + d4_v + d7_v
            d2_v = sqrt(d1_w)

            if ( d1_w .gt. 0.0d0 ) then
               d1_p = 1.0d0 / (2.0d0 *  d2_v)
            else
C               call ehufDO (9,d1_w, d2_v, d1_p,
C     +g_ehfid,
C     +473)
            endif
            do g_i_ = 1, g_pmax_
              g_xt(g_i_) = d1_p * g_d1_w(g_i_)
            enddo
            xt = d2_v
C--------
            d2_v = cartp(j, 1) * cartp(j, 1)
            d3_p = 2.0d0 * cartp(j, 1)
            d4_v = cartp(j, 2) * cartp(j, 2)
            d2_p = 2.0d0 * cartp(j, 2)
            d7_v = cartp(j, 3) * cartp(j, 3)
            d1_p = 2.0d0 * cartp(j, 3)
            do g_i_ = 1, g_pmax_
              g_d1_w(g_i_) = d1_p * g_cartp(g_i_, j, 3) + d2_p * g_cartp
     *(g_i_, j, 2) + d3_p * g_cartp(g_i_, j, 1)
            enddo
            d1_w = d2_v + d4_v + d7_v
            d2_v = sqrt(d1_w)

            if ( d1_w .gt. 0.0d0 ) then
               d1_p = 1.0d0 / (2.0d0 *  d2_v)
            else
C               call ehufDO (9,d1_w, d2_v, d1_p,
C     +g_ehfid,
C     +498)
            endif
            do g_i_ = 1, g_pmax_
              g_yt(g_i_) = d1_p * g_d1_w(g_i_)
            enddo
            yt = d2_v
C--------
            do g_i_ = 1, g_pmax_
              g_zt(g_i_) = cartp(i, 3) * g_cartp(g_i_, j, 3) + cartp(j, 
     *3) * g_cartp(g_i_, i, 3) + cartp(i, 2) * g_cartp(g_i_, j, 2) + car
     *tp(j, 2) * g_cartp(g_i_, i, 2) + cartp(i, 1) * g_cartp(g_i_, j, 1)
     * + cartp(j, 1) * g_cartp(g_i_, i, 1)
            enddo
            zt = cartp(i, 1) * cartp(j, 1) + cartp(i, 2) * cartp(j, 2) +
     * cartp(i, 3) * cartp(j, 3)
C--------
            d4_v = xt * yt
            d5_v = zt / d4_v
            d2_b = 1.0d0 / d4_v
            d3_b = (-d5_v) / d4_v
            d4_b = d3_b * yt
            d5_b = d3_b * xt
            do g_i_ = 1, g_pmax_
              g_w(g_i_) = d5_b * g_yt(g_i_) + d4_b * g_xt(g_i_) + d2_b *
     * g_zt(g_i_)
            enddo
            w = d5_v
C--------
            if (w .gt. 1.0d0) then
              do g_i_ = 1, g_pmax_
                g_w(g_i_) = 0.0d0
              enddo
              w = 1.0d0
C--------
            endif
            if (w .lt. (-1.0d0)) then
              do g_i_ = 1, g_pmax_
                g_w(g_i_) = 0.0d0
              enddo
              w = -1.0d0
C--------
            endif
            d2_v = acos(w)
            
            if ( abs(w) .lt. 1.0d0 ) then
               d1_p = -1.0d0 / sqrt ((1.0d0-w)*(1.0d0+w))
            else
C               call ehufDO (14,w, d2_v, d1_p,
C     +g_ehfid,
C     +547)
            endif
            do g_i_ = 1, g_pmax_
              g_d1_w(g_i_) = d1_p * g_w(g_i_)
            enddo
            d1_w = d2_v
            d2_v = abs(d1_w)

            if (d1_w .gt. 0.0d0) then
               d1_p =  1.0d0
            else if (d1_w .lt. 0.0d0) then
               d1_p = -1.0d0
            else
C               call ehufDO (3,d1_w, d2_v, d1_p,
C     +g_ehfid,
C     +562)
            endif
            d4_b = 1.0d0 / pi * 180.0d0 * d1_p
            do g_i_ = 1, g_pmax_
              g_theta(g_i_, k) = d4_b * g_d1_w(g_i_)
            enddo
            theta(k) = d2_v * 180.0d0 / pi
C--------
          enddo
        enddo
C---------------------------------------------------------
C Now we we will relabel theta such that
C H1-N-H2 : theta(3)
C H1-N-H3 : theta(2)
C H2-N-H3 : theta(1)
        do g_i_ = 1, g_pmax_
          g_xt(g_i_) = g_theta(g_i_, 1)
        enddo
        xt = theta(1)
C--------
        do g_i_ = 1, g_pmax_
          g_theta(g_i_, 1) = g_theta(g_i_, 3)
        enddo
        theta(1) = theta(3)
C--------
        do g_i_ = 1, g_pmax_
          g_theta(g_i_, 3) = g_xt(g_i_)
        enddo
        theta(3) = xt
C--------
C---------------------------------------------------------
C all theta should be +ve and <= 180
C    do k=1,3
C        if(theta(k) < 0.0d0) theta(k) = -theta(k)
C        !if(theta(k) > 180.0d0) theta(k) = theta(k) - 180.0d0
C    end do
C
C
C
        do g_i_ = 1, g_pmax_
          g_bow(g_i_, 1) = g_r(g_i_, 1)
        enddo
        bow(1) = r(1)
C--------
        do g_i_ = 1, g_pmax_
          g_bow(g_i_, 2) = g_r(g_i_, 2)
        enddo
        bow(2) = r(2)
C--------
        do g_i_ = 1, g_pmax_
          g_bow(g_i_, 3) = g_r(g_i_, 3)
        enddo
        bow(3) = r(3)
C--------
        do g_i_ = 1, g_pmax_
          g_bow(g_i_, 4) = g_theta(g_i_, 1)
        enddo
        bow(4) = theta(1)
C--------
        do g_i_ = 1, g_pmax_
          g_bow(g_i_, 5) = g_theta(g_i_, 2)
        enddo
        bow(5) = theta(2)
C--------
        do g_i_ = 1, g_pmax_
          g_bow(g_i_, 6) = g_theta(g_i_, 3)
        enddo
        bow(6) = theta(3)
C--------
        do g_i_ = 1, g_pmax_
          g_bow(g_i_, 7) = g_beta(g_i_, 1)
        enddo
        bow(7) = beta(1)
C--------
C
        return
      end
C
C!********************************************************!!
      subroutine g_distplane(a1, g_a1, a2, g_a2, a3, g_a3, d, g_d, x, g_
     *x, y, g_y, z, g_z, dist, g_dist)
C!********************************************************!!
        implicit none
C
        double precision a1
        double precision a2
        double precision a3
        double precision d
        double precision x
        double precision y
        double precision z
        double precision dist
C
C This subroutine calculates the dist between the 
C point (x,y,z) and the plane (a1x+a2y+a3z+d=0)
C
C dist = a1x + a2y + a3z + d
C      ----------------------
C      sqrt(a**2+b**2+c**2)
C
        integer g_pmax_
        parameter (g_pmax_ = 12)
        integer g_i_
        double precision d16_b, d15_b, d14_b, d13_b, d1_w, d3_p, d2_v, d
     *1_p, d4_v, d10_b
        double precision d9_b, d7_v, d16_v, d15_v, d2_b, d2_p, d4_b, g_d
     *1_w(g_pmax_), g_a1(g_pmax_), g_a2(g_pmax_)
        double precision g_a3(g_pmax_), g_dist(g_pmax_), g_x(g_pmax_), g
     *_y(g_pmax_), g_z(g_pmax_), g_d(g_pmax_)
        integer g_ehfid
        save g_d1_w
        data g_ehfid /0/
C
C        call ehsfid(g_ehfid, 'distplane','g_toBowman.f')
C
        d2_v = a1 * a1
        d3_p = 2.0d0 * a1
        d4_v = a2 * a2
        d2_p = 2.0d0 * a2
        d7_v = a3 * a3
        d1_p = 2.0d0 * a3
        do g_i_ = 1, g_pmax_
          g_d1_w(g_i_) = d1_p * g_a3(g_i_) + d2_p * g_a2(g_i_) + d3_p * 
     *g_a1(g_i_)
        enddo
        d1_w = d2_v + d4_v + d7_v
        d15_v = sqrt(d1_w)

        if ( d1_w .gt. 0.0d0 ) then
           d1_p = 1.0d0 / (2.0d0 *  d15_v)
        else
C           call ehufDO (9,d1_w, d15_v, d1_p,
C     +g_ehfid,
C     +695)
        endif
        d16_v = (a1 * x + a2 * y + a3 * z + d) / d15_v
        d2_b = 1.0d0 / d15_v
        d4_b = (-d16_v) / d15_v * d1_p
        d9_b = d2_b * z
        d10_b = d2_b * a3
        d13_b = d2_b * y
        d14_b = d2_b * a2
        d15_b = d2_b * x
        d16_b = d2_b * a1
        do g_i_ = 1, g_pmax_
          g_dist(g_i_) = d4_b * g_d1_w(g_i_) + d2_b * g_d(g_i_) + d10_b 
     ** g_z(g_i_) + d9_b * g_a3(g_i_) + d14_b * g_y(g_i_) + d13_b * g_a2
     *(g_i_) + d16_b * g_x(g_i_) + d15_b * g_a1(g_i_)
        enddo
        dist = d16_v
C--------
C
C
        return
      end
C
C!********************************************************!!
      subroutine g_det3(a, g_a, ans, g_ans)
C!********************************************************!!
        implicit none
        double precision a(3, 3)
        double precision ans
C
C Compute determinant of a 3x3 matrix
C
        double precision x, y, z
C
        integer g_pmax_
        parameter (g_pmax_ = 12)
        integer g_i_
        double precision d9_b, d8_b, d7_b, d6_b, d8_v, g_x(g_pmax_), g_a
     *(g_pmax_, 3, 3), g_y(g_pmax_), g_z(g_pmax_), g_ans(g_pmax_)
        integer g_ehfid
        save g_x, g_y, g_z
        data g_ehfid /0/
C
C        call ehsfid(g_ehfid, 'det3','g_toBowman.f')
C
        d8_v = a(2, 2) * a(3, 3) - a(2, 3) * a(3, 2)
        d6_b = (-a(1, 1)) * a(3, 2)
        d7_b = (-a(1, 1)) * a(2, 3)
        d8_b = a(1, 1) * a(3, 3)
        d9_b = a(1, 1) * a(2, 2)
        do g_i_ = 1, g_pmax_
          g_x(g_i_) = d7_b * g_a(g_i_, 3, 2) + d6_b * g_a(g_i_, 2, 3) + 
     *d9_b * g_a(g_i_, 3, 3) + d8_b * g_a(g_i_, 2, 2) + d8_v * g_a(g_i_,
     * 1, 1)
        enddo
        x = a(1, 1) * d8_v
C--------
        d8_v = a(2, 1) * a(3, 3) - a(2, 3) * a(3, 1)
        d6_b = (-a(1, 2)) * a(3, 1)
        d7_b = (-a(1, 2)) * a(2, 3)
        d8_b = a(1, 2) * a(3, 3)
        d9_b = a(1, 2) * a(2, 1)
        do g_i_ = 1, g_pmax_
          g_y(g_i_) = d7_b * g_a(g_i_, 3, 1) + d6_b * g_a(g_i_, 2, 3) + 
     *d9_b * g_a(g_i_, 3, 3) + d8_b * g_a(g_i_, 2, 1) + d8_v * g_a(g_i_,
     * 1, 2)
        enddo
        y = a(1, 2) * d8_v
C--------
        d8_v = a(2, 1) * a(3, 2) - a(2, 2) * a(3, 1)
        d6_b = (-a(1, 3)) * a(3, 1)
        d7_b = (-a(1, 3)) * a(2, 2)
        d8_b = a(1, 3) * a(3, 2)
        d9_b = a(1, 3) * a(2, 1)
        do g_i_ = 1, g_pmax_
          g_z(g_i_) = d7_b * g_a(g_i_, 3, 1) + d6_b * g_a(g_i_, 2, 2) + 
     *d9_b * g_a(g_i_, 3, 2) + d8_b * g_a(g_i_, 2, 1) + d8_v * g_a(g_i_,
     * 1, 3)
        enddo
        z = a(1, 3) * d8_v
C--------
C
        do g_i_ = 1, g_pmax_
          g_ans(g_i_) = g_z(g_i_) + (-g_y(g_i_)) + g_x(g_i_)
        enddo
        ans = x - y + z
C--------
C
C
C
        return
      end

C!******************************************!!
      subroutine carttoang(cart,ans,gangint)
C!******************************************!!
C
C This function converts cartesian coordinates
C to internal coordinates (dist and bond angles)
C
C Cartesian coordinate format: x1,y1,z1,x2,y2,z2....xn,yn,zn
C N  : cart(1-3)
C H1 : cart(4-6)
C H2 : cart(7-9)
C H3 : cart(10-11)
C
C Dist in Ang. Angles in degrees
C Internal coordinate format:
C H1-N-H2 : theta3
C H1-N-H3 : theta2
C H2-N-H3 : theta1
C
C[r1,r2,r3,theta1,theta2,theta3]
C-----------------------------------------------------
      implicit none

C I/O variables
      integer NATOM
      parameter ( NATOM = 4 )
      double precision PI
      parameter ( PI = 3.1415926535897930d0 )
      double precision cart(3*NATOM)
      double precision gangint(12,7)

C Local variables
      integer i,j,k
      double precision r(3),t(3)
      double precision dt(3)
      double precision theta(3)
      double precision cartH(3,3)
      double precision ans(7)
C to save the first three bond distances. If they are calculated in any
C subroutines, avoid to calculate them again
       double precision rrrrr(3)
       logical lrrrrr
       common /calcrrrr/ rrrrr,lrrrrr

C initialize gangint
      do i=1,7
        do j=1,12
          gangint(j,i)=0.d0
        enddo
      enddo
C Put N at the origin and 
C and get N-H bond vectors
      k=0
      do i=2,NATOM
        k=k+1
        cartH(k,1)=cart(3*i-2)-cart(1) 
        cartH(k,2)=cart(3*i-1)-cart(2) 
        cartH(k,3)=cart(3*i  )-cart(3) 
      end do
 
C calculate bond distances
      if (lrrrrr) then
        do i=1,3
                r(i) = rrrrr(i)
        enddo
      else
        do i=1,3
          r(i)=dsqrt(cartH(i,1)*cartH(i,1) + cartH(i,2)*cartH(i,2) +
     &               cartH(i,3)*cartH(i,3) )
        enddo
        lrrrrr=.true.
      endif
C dr/dx,cartH(i) is a function of the cartesian coord. of the i+1 atom
      do i=1,3
        do j=1,3
          k = 3*i + j
C         dr(i)/dx(2),dr(i)/dx(3),dr(i)/dx(4)
          gangint(k,i) = cartH(i,j)/r(i)
C         dr(i)/dx(1)
          gangint(j,i) = -gangint(k,i)
        enddo
      end do
C------------------------------------------------------
C calculate bond angles
C
      t(1) = (cartH(2,1)*cartH(3,1)+cartH(2,2)*cartH(3,2)+ 
     &        cartH(2,3)*cartH(3,3))/(r(2)*r(3))
      t(2) = (cartH(1,1)*cartH(3,1)+cartH(1,2)*cartH(3,2)+ 
     &        cartH(1,3)*cartH(3,3))/(r(1)*r(3))
      t(3) = (cartH(1,1)*cartH(2,1)+cartH(1,2)*cartH(2,2)+ 
     &        cartH(1,3)*cartH(2,3))/(r(1)*r(2))
      do i=1,3
C    Due to numerical errors, t maybe larger than 1.0 by a tiny value
C        by definition 0 <= dacos(t) <= PI
        if (t(i).gt.1.d0) then
              t(i)=1.d0
        elseif (t(i).lt.-1.d0) then
              t(i)=-1.d0
        endif
        theta(i) =  dacos( t(i) )
C       convert to degrees
        theta(i) = theta(i)*180.0d0/PI
      end do

C return answer
      do i=1,3
        ans(i)   = r(i)
        ans(3+i) = theta(i)
      enddo

C   Derivatives of the three angles
!    dtheta1/dt1  theta = acos(t) 
      do i=1,3
      if (dabs(t(i)).ge.1.d0) then
C         write(*,*) "Abnormal structure: N-H2-H3 is linear"
C         stop
        dt(i)=0.d0
      else
        dt(i) = -(1.d0/dsqrt( 1.d0-t(i)*t(i) ))*180.d0/PI
      endif
      enddo
!    dtheta1/dx3,dtheta1/dx4,dtheta1/dx1. dtheta1/dx2=0
!    theta1 is a function of atom 1,3,4
      do j=1,3
        k  = 6+j
        gangint(k,4) = cartH(3,j)/(r(2)*r(3)) - gangint(k,2)*t(1)/r(2)
        gangint(k,4) = dt(1) * gangint(k,4)
        k  = 9+j
        gangint(k,4) = cartH(2,j)/(r(2)*r(3)) - gangint(k,3)*t(1)/r(3)
        gangint(k,4) = dt(1) * gangint(k,4)
        gangint(j,4) =-(cartH(2,j)+cartH(3,j))/(r(2)*r(3)) -    
     &       ( r(3)*gangint(j,2)+r(2)*gangint(j,3) ) *  t(1)/(r(2)*r(3))
        gangint(j,4) = dt(1) * gangint(j,4)
      enddo

!    dtheta2/dt2  theta = acos(t) 
!    dtheta2/dx2,dtheta2/dx4,dtheta2/dx1. dtheta2/dx3=0
!    theta2 is a function of atom 1,2,4
      do j=1,3
        k = 3+j
        gangint(k,5) = cartH(3,j)/(r(1)*r(3)) - gangint(k,1)*t(2)/r(1)
        gangint(k,5) = dt(2) * gangint(k,5)
        k = 9+j
        gangint(k,5) = cartH(1,j)/(r(1)*r(3)) - gangint(k,3)*t(2)/r(3)
        gangint(k,5) = dt(2) * gangint(k,5)
        gangint(j,5) =-(cartH(1,j)+cartH(3,j))/(r(1)*r(3)) -   
     &       ( r(1)*gangint(j,3)+r(3)*gangint(j,1) ) *  t(2)/(r(1)*r(3))
        gangint(j,5) = dt(2) * gangint(j,5)
      enddo

!    dtheta3/dt3  theta = acos(t) 
!    dtheta3/dx2,dtheta3/dx3,dtheta3/dx1. dtheta3/dx4=0
!    theta3 is a function of atom 1,2,3
      do j=1,3
        k = 3+j
        gangint(k,6) = cartH(2,j)/(r(1)*r(2)) - gangint(k,1)*t(3)/r(1)
        gangint(k,6) = dt(3) * gangint(k,6)
        k = 6+j
        gangint(k,6) = cartH(1,j)/(r(1)*r(2)) - gangint(k,2)*t(3)/r(2)
        gangint(k,6) = dt(3) * gangint(k,6)
        gangint(j,6) =-(cartH(1,j)+cartH(2,j))/(r(1)*r(2)) -          
     &       ( r(1)*gangint(j,2)+r(2)*gangint(j,1) ) *  t(3)/(r(1)*r(2))
        gangint(j,6) = dt(3) * gangint(j,6)
      enddo


      return
      END 

C*******************************************!
      subroutine cart_to_polar(x,ans)
C*******************************************!
      implicit none
      double precision x(3)
      double precision ans(3)
      double precision PI
      parameter ( PI = 3.1415926535897930d0 )
C
C ans(1) : r
C ans(2) : phi (0-2PI)  !in degrees
C ans(3) : theta (0-PI) !in degrees
C
      double precision  t

      ans(1) = dsqrt( x(1)*x(1) + x(2)*x(2) + x(3)*x(3) )
      if (ans(1).ne.0.d0) then
        t      = dacos(x(3)/ans(1))      !angle is between [-PI,PI]
        ans(3) = dasin(dsin(t))          !angle is between [0,2PI]

        if( x(1) .ne. 0.0d0 ) then
          ans(2) = datan(x(2)/x(1))
        else 
          ans(2) = 0.50d0 * PI
          if (x(2) .eq. 0.0d0) ans(2) = 0.0d0
        end if

C Convert to degrees
        ans(2) = ans(2) * 180.0d0/PI
        ans(3) = ans(3) * 180.0d0/PI
      else
        ans(2) = 0.d0
        ans(3) = 0.d0
      endif

      return
      END

C*******************************************!
      subroutine polar_to_cart(p,ans)
C*******************************************!
      implicit none
      double precision p(3)
      double precision ans(3)
      double precision PI
      parameter ( PI = 3.1415926535897930d0 )
C
C p(1) : r
C p(2) : phi (0-2PI)  !in degrees
C p(3) : theta (0-PI) !in degrees
C
      double precision  phi,theta
      double precision  sinth

      phi = p(2) * PI/180.0d0
      theta = p(3) * PI/180.0d0

      sinth = dsin(theta)

      ans(1) = p(1) * dcos(phi) * sinth
      ans(2) = p(1) * dsin(phi) * sinth
      ans(3) = p(1) * dcos(theta)


      return
      END

C*********************************************!
      subroutine fromBowman(bow,coord)
C*********************************************!
      implicit none
      double precision  bow(7)
      double precision  coord(12)
      double precision PI
      parameter ( PI = 3.1415926535897930d0 )
C
C This function returns the cartesian 
C coordinates from bowman coordinates for 4 atom systems ONLY
C All angles are in DEGREES
C
C Input:
C bow(1) : r1
C bow(2) : r2
C bow(3) : r3
C bow(4) : theta1
C bow(5) : theta2
C bow(5) : theta3
C bow(7) : beta
C
C Output: 
C coord(1:3)   : N   x,y,z
C coord(4:6)   : H1  x,y,z
C coord(7:9)   : H2  x,y,z
C coord(10:12) : H3  x,y,z
C
      double precision r1,r2,r3
      double precision theta1,theta2,theta3
      double precision phi1,phi2,phi3
      double precision beta
      double precision ph1(3)
      double precision ph2(3)
      double precision ph3(3)
      double precision  ans(3)
      integer i,j
      double precision  small
      small = 1.d-3

C define stuff
      r1 = bow(1)
      r2 = bow(2)
      r3 = bow(3)
      theta1 = bow(4)
      theta2 = bow(5)
      theta3 = bow(6)
      beta = bow(7)
C
C z-axis is the trisector vector
C G1, G2, G3 are the projection of H1, H2 and H3
C in the xy plane
C 
C  G2
C   \
C    N--G1-------> x-axis
C   /
C  G3
C
C Angle definitions:
C G1-N-G2 : theta3
C G1-N-G3 : theta2
C G2-N-G3 : theta1
C

C define phi 
      phi1 = 0.0d0
      phi2 = theta3
C the following line (Shikha) is correct only when the sum of
C the three theta is 360.0
C      phi3 = theta1+theta3
      if (dabs(360.d0-(theta1+theta2+theta3)).lt.small) then
        phi3 = theta1+theta3
      elseif (dabs(theta1-theta2-theta3).lt.small) then
        phi3 = 360.d0-theta2
      elseif (dabs(theta2-theta1-theta3).lt.small) then
        phi3 =  theta2
      elseif (dabs(theta3-theta1-theta2).lt.small) then
        phi3 =  theta2
      else
        write(6,*) "Is this geometry possible?"
        write(6,*) bow
        stop
      endif

C vector for H1 in polar
      ph1(1) = r1       
      ph1(2) = phi1     !in degrees
      ph1(3) = beta     !in degrees

C vector for H2 in polar
      ph2(1) = r2
      ph2(2) = phi2   !G2 makes theta2 with x-axis
      ph2(3) = beta

C vector for H3 in polar
      ph3(1) = r3
      ph3(2) = phi3   !G3 makes theta2+theta3 with x-axis
      ph3(3) = beta

C convert to cartesian vectors
C N is at the origin
      coord(1) = 0.0d0
      coord(2) = 0.0d0
      coord(3) = 0.0d0
C H1
      call polar_to_cart(ph1,ans)
      do i=1,3
        coord(3+i) = ans(i)
      enddo
C H2
      call polar_to_cart(ph2,ans)
      do i=1,3
        coord(6+i) = ans(i)
      enddo
C H3
      call polar_to_cart(ph3,ans)
      do i=1,3
        coord(9+i) = ans(i)
      enddo


      return
      END

!!************************************************************!!
      subroutine angle_to_cart(angint,coord)
!!************************************************************!!
!
! Convert Valence Internal Coordinates (VIC)
! to cartesian coordinates. for 4 atom systems only
!
!
! Cartesian coordinate format:
! N  : cart(1:3)
! H1 : cart(4:6)
! H2 : cart(7:9)
! H3 : cart(10:12)
!
! Dist in Ang. Angles in degrees
! Internal coordinate format:
! H1-N-H2 : theta3
! H1-N-H3 : theta2
! H2-N-H3 : theta1
!
![r1,r2,r3,theta1,theta2,theta3]
!
      implicit none
!--------------------
! I/O variables
!--------------------
      double precision  angint(7)
      double precision  coord(12)
!--------------------
! Local variables
!--------------------
      double precision  r1
      double precision  r2
      double precision  r3
      double precision  theta1
      double precision  theta2
      double precision  theta3
      double precision  uH3(3)
!--------------------
! Misc
!--------------------
      double precision  degtopi
      parameter (  degtopi = 3.1415926535897930d0/180.d0 )
      double precision  x
      double precision  costh1,costh2,costh3,sinth3
      double precision  small
      parameter (small = 1.d-10)
      integer i

! initialize
        r1 = angint(1)
        r2 = angint(2)
        r3 = angint(3)
        theta1 = angint(4)*degtopi
        theta2 = angint(5)*degtopi
        theta3 = angint(6)*degtopi
        do i = 1,12
          coord(i) = 0.0d0
        enddo
        costh2 = dcos(theta2)
        costh3 = dcos(theta3)
        sinth3 = dsin(theta3)

! N is at origin

! H1 is along x
        coord(4) = r1
        
! H2 is in the xy plane
        coord(7) = r2*costh3
        coord(8) = r2*sinth3

! H3 is in the xyz p
! If H2 along x: sin(theta3) = 0), put H3 on xz plane
C theta3 = 0   theta1=theta2
C theta3 = PI  theta1=PI-theta2
! unit vector along the N-H3 bond (uH3)
      uH3(1) = costh2
      if ( dabs(sinth3).lt.small ) then
        uH3(2) = 0.d0
        uH3(3) = dsin(theta2)
      else
        costh1 = dcos(theta1)
        uH3(2) = ( costh1 - costh2*costh3 )/sinth3
        x = uH3(1)**2 + uH3(2)**2
        if( x .le. 1.d0) then
                uH3(3) = dsqrt(1.0d0-x)
C  Due to numerical errors, x may be just marginally larger than 1.d0
C  (planar structure)
        elseif( (x-1.d0) .le. small) then
              uH3(3) = 0.d0
        else
           write(*,*)'ERROR: Set of three bond angles is unphysical'
           write(*,*) r1,r2,r3,theta1/degtopi,
     &      theta2/degtopi,theta3/degtopi
           stop
        endif
      endif
      coord(10) = r3 * uH3(1)
      coord(11) = r3 * uH3(2)
      coord(12) = r3 * uH3(3)

      return
      END

C--------------------------------------------
      subroutine rarray(natoms,x,mx,r,dr,mr)
C calculate the rij array N(N-1)/2 elements
C x:  x1,y1,z1,...,xn,yn,zn
C dr(i,j): derivatives of jth r to i th cart coord
      implicit none

      integer natoms
C dimension of x and r, must be larger than 3*natoms and
C natoms*(natoms-1)/2 respectively
      integer mx,mr
      double precision x(mx),r(mr),dr(mx,mr)

      integer i,j,k,ii,ij,nr
      double precision dx,dy,dz
C to save the first three bond distances. If they are calculated in any
C subroutines, avoid to calculate them again
       double precision rrrrr(3)
       logical lrrrrr
       common /calcrrrr/ rrrrr,lrrrrr
      nr = natoms*(natoms-1)/2

      do i=1,nr
        do j = 1,3*natoms
          dr(j,i) = 0.d0
        enddo
      enddo

      k = 0
      do i = 1,natoms
        ii = 3*(i-1)
      do j = i+1,natoms
        k  = k+1
        ij = 3*(j-1)
        dx = x(ij+1) - x(ii+1)
        dy = x(ij+2) - x(ii+2)
        dz = x(ij+3) - x(ii+3)
C Since N is always the first atom, r(1) to r(3) are always the first
C three NH distances
        if (lrrrrr.and.(k.le.3)) then
          r(k) = rrrrr(k)
        else
          r(k) = dsqrt( dx*dx + dy*dy + dz*dz )
        endif
        dr(ij+1,k) =  dx/r(k)
        dr(ij+2,k) =  dy/r(k)
        dr(ij+3,k) =  dz/r(k)
        dr(ii+1,k) = -dr(ij+1,k)
        dr(ii+2,k) = -dr(ij+2,k)
        dr(ii+3,k) = -dr(ij+3,k)
      enddo
      enddo
      if (.not.lrrrrr) then
        do k=1,3
          rrrrr(k)=r(k)
        enddo
      endif
      lrrrrr=.true.

      return
      end

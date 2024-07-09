C
C   System:    H2O2
C   Functional Form: 
C   Common name:  Koput-Carter-Handy
C   Number of derivatives:  1
C   Number of bodies:  4
C   Number of electronic surfaces:  1
C   Interface:  Section-2
C
C   References: Koput, J.; Carter, S.; Handy, N. C.;
C               J. Phys. Chem. A, 102 (1998) 6325.
C
C   Notes:
C
C
      SUBROUTINE SURF(V,COORD,DX,N3TM)

      IMPLICIT NONE

      DOUBLE PRECISION  V, COORD(N3TM), DX(N3TM)
      DOUBLE PRECISION  DXX(N3TM), X(N3TM) 

      INTEGER  I,N3TM

C Changing units from bohr to angstroms
C
      DO 10 I = 1, 12
         x(I) = COORD(I)*0.52918d0
 10   CONTINUE

      DO i = 1, 12
        DX(i)=0.0D0
      ENDDO

      call POTENTIAL(v,x,dx,dxx,12)

      DO I = 1, 12
        X(I) = X(I)/0.52918D0
        DX(I) = DX(I)*0.52918D0
        DXX(I) = DXX(I) * 0.52918D0 * 0.52918D0
      ENDDO

      END

c**************************************************************************

      SUBROUTINE POTENTIAL(V, X, DX, DXX, N3TM)
C
C   System:    H2O2, see Koput, J.; Carter, S.; Handy, N. C.;
C              J. Phys. Chem. A, 102 (1998) 6325
C              Prepared for Polyrate by VMA, October 2002.
C
C   Reference: Koput,Carter,&Handy, JPC A, 102, 6325 (1998).
C
C   All the information passed to and from the potential energy surface
C   routine is in hartree atomic units.
C
C        This potential is written such that:
C                       X(1)  - X(3)  : X, Y, Z for H1
C                       X(4)  - X(6)  : X, Y, Z for O1
C                       X(7)  - X(9)  : X, Y, Z for O2
C                       X(10) - X(12) : X, Y, Z for H2
C
      IMPLICIT NONE

      DOUBLE PRECISION COORD(N3TM),DX(N3TM),DXX(N3TM),X(N3TM)
      DOUBLE PRECISION V, PASS(6)

      DOUBLE PRECISION GX(12,12), GV(12,12)
      DOUBLE PRECISION GXX(12,12,12), GVV(12,12,12)

      DOUBLE PRECISION TEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     * TEMP6,TEMP7,TEMP8,TEMP9,TEMP10,TEMP11,TEMP12,
     * TEMP13,TEMP14,TEMP15,TEMP16,TEMP17,TEMP18,
     * TEMP19,TEMP20,TEMP21,TEMP22,TEMP23,TEMP24,
     * TEMP25,TEMP26,TEMP27,TEMP28,TEMP29,TEMP30,
     * TEMP31,TEMP32,TEMP33,TEMP34,TEMP35,TEMP36,
     * TEMP37,TEMP38,TEMP39,TEMP40,TEMP41,TEMP42,
     * TEMP43,TEMP44,TEMP45,TEMP46,TEMP47,TEMP48,
     * TEMP49,TEMP50,TEMP51,TEMP52,TEMP53,TEMP54,
     * TEMP55,TEMP56,TEMP57,TEMP58,TEMP59,TEMP60,
     * TEMP61,TEMP62,TEMP63,TEMP64,TEMP65,TEMP66

      DOUBLE PRECISION TEMPX,TEMP1X,TEMP2X,TEMP3X,TEMP4X,TEMP5X,
     * TEMP6X,TEMP7X,TEMP8X,TEMP9X,TEMP10X,TEMP11X,TEMP12X,
     * TEMP13X,TEMP14X,TEMP15X,TEMP16X,TEMP17X,TEMP18X,
     * TEMP19X,TEMP20X,TEMP21X,TEMP22X,TEMP23X,TEMP24X,
     * TEMP25X,TEMP26X,TEMP27X,TEMP28X,TEMP29X,TEMP30X,
     * TEMP31X,TEMP32X,TEMP33X,TEMP34X,TEMP35X,TEMP36X,
     * TEMP37X,TEMP38X,TEMP39X,TEMP40X,TEMP41X,TEMP42X,
     * TEMP43X,TEMP44X,TEMP45X,TEMP46X,TEMP47X,TEMP48X,
     * TEMP49X,TEMP50X,TEMP51X,TEMP52X,TEMP53X,TEMP54X,
     * TEMP55X,TEMP56X,TEMP57X,TEMP58X,TEMP59X,TEMP60X,
     * TEMP61X,TEMP62X,TEMP63X,TEMP64X,TEMP65X,TEMP66X
     
      INTEGER N3TM, I, J, II

      DOUBLE PRECISION ROHLOW,ROHHIGH,ROOLOW,ROOHIGH,RHHLOW

      PARAMETER (ROHLOW = 1.0D0)
      PARAMETER (ROHHIGH = 4.0D0)
      PARAMETER (ROOLOW = 1.8D0)
      PARAMETER (ROOHIGH = 4.0D0)
      PARAMETER (RHHLOW = 0.7D0)

C
C Call Koput, Carter, and Handy PES
C

      CALL POT(V,X)

c
c  Initialize derivatives
c
       do j=1,N3TM
           DX(j)=0.0d0
       enddo

       GX(1,1) = 1.0D0
       GX(2,2) = 1.0D0
       GX(3,3) = 1.0D0
       GX(4,4) = 1.0D0
       GX(5,5) = 1.0D0
       GX(6,6) = 1.0D0
       GX(7,7) = 1.0D0
       GX(8,8) = 1.0D0
       GX(9,9) = 1.0D0
       GX(10,10) = 1.0D0
       GX(11,11) = 1.0D0
       GX(12,12) = 1.0D0

       GX(1,2) = 0.0D0
       GX(1,3) = 0.0D0
       GX(1,4) = 0.0D0
       GX(1,5) = 0.0D0
       GX(1,6) = 0.0D0
       GX(1,7) = 0.0D0
       GX(1,8) = 0.0D0
       GX(1,9) = 0.0D0
       GX(1,10) = 0.0D0
       GX(1,11) = 0.0D0
       GX(1,12) = 0.0D0

       GX(2,1) = 0.0D0
       GX(2,3) = 0.0D0
       GX(2,4) = 0.0D0
       GX(2,5) = 0.0D0
       GX(2,6) = 0.0D0
       GX(2,7) = 0.0D0
       GX(2,8) = 0.0D0
       GX(2,9) = 0.0D0
       GX(2,10) = 0.0D0
       GX(2,11) = 0.0D0
       GX(2,12) = 0.0D0

       GX(3,1) = 0.0D0
       GX(3,2) = 0.0D0
       GX(3,4) = 0.0D0
       GX(3,5) = 0.0D0
       GX(3,6) = 0.0D0
       GX(3,7) = 0.0D0
       GX(3,8) = 0.0D0
       GX(3,9) = 0.0D0
       GX(3,10) = 0.0D0
       GX(3,11) = 0.0D0
       GX(3,12) = 0.0D0

       GX(4,1) = 0.0D0
       GX(4,2) = 0.0D0
       GX(4,3) = 0.0D0
       GX(4,5) = 0.0D0
       GX(4,6) = 0.0D0
       GX(4,7) = 0.0D0
       GX(4,8) = 0.0D0
       GX(4,9) = 0.0D0
       GX(4,10) = 0.0D0
       GX(4,11) = 0.0D0
       GX(4,12) = 0.0D0

       GX(5,1) = 0.0D0
       GX(5,2) = 0.0D0
       GX(5,3) = 0.0D0
       GX(5,4) = 0.0D0
       GX(5,6) = 0.0D0
       GX(5,7) = 0.0D0
       GX(5,8) = 0.0D0
       GX(5,9) = 0.0D0
       GX(5,10) = 0.0D0
       GX(5,11) = 0.0D0
       GX(5,12) = 0.0D0

       GX(6,1) = 0.0D0
       GX(6,2) = 0.0D0
       GX(6,3) = 0.0D0
       GX(6,4) = 0.0D0
       GX(6,5) = 0.0D0
       GX(6,7) = 0.0D0
       GX(6,8) = 0.0D0
       GX(6,9) = 0.0D0
       GX(6,10) = 0.0D0
       GX(6,11) = 0.0D0
       GX(6,12) = 0.0D0

       GX(7,1) = 0.0D0
       GX(7,2) = 0.0D0
       GX(7,3) = 0.0D0
       GX(7,4) = 0.0D0
       GX(7,5) = 0.0D0
       GX(7,6) = 0.0D0
       GX(7,8) = 0.0D0
       GX(7,9) = 0.0D0
       GX(7,10) = 0.0D0
       GX(7,11) = 0.0D0
       GX(7,12) = 0.0D0

       GX(8,1) = 0.0D0
       GX(8,2) = 0.0D0
       GX(8,3) = 0.0D0
       GX(8,4) = 0.0D0
       GX(8,5) = 0.0D0
       GX(8,6) = 0.0D0
       GX(8,7) = 0.0D0
       GX(8,9) = 0.0D0
       GX(8,10) = 0.0D0
       GX(8,11) = 0.0D0
       GX(8,12) = 0.0D0

       GX(9,1) = 0.0D0
       GX(9,2) = 0.0D0
       GX(9,3) = 0.0D0
       GX(9,4) = 0.0D0
       GX(9,5) = 0.0D0
       GX(9,6) = 0.0D0
       GX(9,7) = 0.0D0
       GX(9,8) = 0.0D0
       GX(9,10) = 0.0D0
       GX(9,11) = 0.0D0
       GX(9,12) = 0.0D0

       GX(10,1) = 0.0D0
       GX(10,2) = 0.0D0
       GX(10,3) = 0.0D0
       GX(10,4) = 0.0D0
       GX(10,5) = 0.0D0
       GX(10,6) = 0.0D0
       GX(10,7) = 0.0D0
       GX(10,8) = 0.0D0
       GX(10,9) = 0.0D0
       GX(10,11) = 0.0D0
       GX(10,12) = 0.0D0

       GX(11,1) = 0.0D0
       GX(11,2) = 0.0D0
       GX(11,3) = 0.0D0
       GX(11,4) = 0.0D0
       GX(11,5) = 0.0D0
       GX(11,6) = 0.0D0
       GX(11,7) = 0.0D0
       GX(11,8) = 0.0D0
       GX(11,9) = 0.0D0
       GX(11,10) = 0.0D0
       GX(11,12) = 0.0D0

       GX(12,1) = 0.0D0
       GX(12,2) = 0.0D0
       GX(12,3) = 0.0D0
       GX(12,4) = 0.0D0
       GX(12,5) = 0.0D0
       GX(12,6) = 0.0D0
       GX(12,7) = 0.0D0
       GX(12,8) = 0.0D0
       GX(12,9) = 0.0D0
       GX(12,10) = 0.0D0
       GX(12,11) = 0.0D0

c
c  Calculate first derivatives using a analytic gradients
c

      CALL G_SURF(12, V, GV, 1, X, GX, 12)

C  TRANSPOSE GV

      TEMPX = GV(2,1)
      GV(2,1) = GV(1,2)
      GV(1,2) = TEMPX

      TEMP2X = GV(3,1)
      GV(3,1) = GV(1,3)
      GV(1,3) = TEMP2X

      TEMP3X = GV(4,1)
      GV(4,1) = GV(1,4)
      GV(1,4) = TEMP3X

      TEMP4X = GV(5,1)
      GV(5,1) = GV(1,5)
      GV(1,5) = TEMP4X

      TEMP5X = GV(6,1)
      GV(6,1) = GV(1,6)
      GV(1,6) = TEMP5X

      TEMP6X = GV(7,1)
      GV(7,1) = GV(1,7)
      GV(1,7) = TEMP6X

      TEMP7X = GV(8,1)
      GV(8,1) = GV(1,8)
      GV(1,8) = TEMP7X

      TEMP8X = GV(9,1)
      GV(9,1) = GV(1,9)
      GV(1,9) = TEMP8X

      TEMP9X = GV(10,1)
      GV(10,1) = GV(1,10)
      GV(1,10) = TEMP9X

      TEMP10X = GV(11,1)
      GV(11,1) = GV(1,11)
      GV(1,11) = TEMP10X

      TEMP11X = GV(12,1)
      GV(12,1) = GV(1,12)
      GV(1,12) = TEMP11X

      TEMP12X = GV(2,3)
      GV(2,3) = GV(3,2)
      GV(3,2) = TEMP12X

      TEMP13X = GV(2,4)
      GV(2,4) = GV(4,2)
      GV(4,2) = TEMP13X

      TEMP14X = GV(2,5)
      GV(2,5) = GV(5,2)
      GV(5,2) = TEMP14X

      TEMP15X = GV(2,6)
      GV(2,6) = GV(6,2)
      GV(6,2) = TEMP15X

      TEMP16X = GV(2,7)
      GV(2,7) = GV(7,2)
      GV(7,2) = TEMP16X

      TEMP17X = GV(2,8)
      GV(2,8) = GV(8,2)
      GV(8,2) = TEMP17X

      TEMP18X = GV(2,9)
      GV(2,9) = GV(9,2)
      GV(9,2) = TEMP18X

      TEMP19X = GV(2,10)
      GV(2,10) = GV(10,2)
      GV(10,2) = TEMP19X

      TEMP20X = GV(2,11)
      GV(2,11) = GV(11,2)
      GV(11,2) = TEMP20X

      TEMP21X = GV(2,12)
      GV(2,12) = GV(12,2)
      GV(12,2) = TEMP21X

      TEMP22X = GV(3,4)
      GV(3,4) = GV(4,3)
      GV(4,3) = TEMP22X

      TEMP23X = GV(3,5)
      GV(3,5) = GV(5,3)
      GV(5,3) = TEMP23X

      TEMP24X = GV(3,6)
      GV(3,6) = GV(6,3)
      GV(6,3) = TEMP24X

      TEMP25X = GV(3,7)
      GV(3,7) = GV(7,3)
      GV(7,3) = TEMP25X

      TEMP26X = GV(3,8)
      GV(3,8) = GV(8,3)
      GV(8,3) = TEMP26X

      TEMP27X = GV(3,9)
      GV(3,9) = GV(9,3)
      GV(9,3) = TEMP27X

      TEMP28X = GV(3,10)
      GV(3,10) = GV(10,3)
      GV(10,3) = TEMP28X

      TEMP29X = GV(3,11)
      GV(3,11) = GV(11,3)
      GV(11,3) = TEMP29X

      TEMP30X = GV(3,12)
      GV(3,12) = GV(12,3)
      GV(12,3) = TEMP30X

      TEMP31X = GV(4,5)
      GV(4,5) = GV(5,4)
      GV(5,4) = TEMP31X

      TEMP32X = GV(4,6)
      GV(4,6) = GV(6,4)
      GV(6,4) = TEMP32X

      TEMP33X = GV(4,7)
      GV(4,7) = GV(7,4)
      GV(7,4) = TEMP33X

      TEMP34X = GV(4,8)
      GV(4,8) = GV(8,4)
      GV(8,4) = TEMP34X

      TEMP35X = GV(4,9)
      GV(4,9) = GV(9,4)
      GV(9,4) = TEMP35X

      TEMP36X = GV(4,10)
      GV(4,10) = GV(10,4)
      GV(10,4) = TEMP36X

      TEMP37X = GV(4,11)
      GV(4,11) = GV(11,4)
      GV(11,4) = TEMP37X

      TEMP38X = GV(4,12)
      GV(4,12) = GV(12,4)
      GV(12,4) = TEMP38X

      TEMP39X = GV(5,6)
      GV(5,6) = GV(6,5)
      GV(6,5) = TEMP39X

      TEMP40X = GV(5,7)
      GV(5,7) = GV(7,5)
      GV(7,5) = TEMP40X

      TEMP41X = GV(5,8)
      GV(5,8) = GV(8,5)
      GV(8,5) = TEMP41X

      TEMP42X = GV(5,9)
      GV(5,9) = GV(9,5)
      GV(9,5) = TEMP42X

      TEMP43X = GV(5,10)
      GV(5,10) = GV(10,5)
      GV(10,5) = TEMP43X

      TEMP44X = GV(5,11)
      GV(5,11) = GV(11,5)
      GV(11,5) = TEMP44X

      TEMP45X = GV(5,12)
      GV(5,12) = GV(12,5)
      GV(12,5) = TEMP45X

      TEMP46X = GV(6,7)
      GV(6,7) = GV(7,6)
      GV(7,6) = TEMP46X

      TEMP47X = GV(6,8)
      GV(6,8) = GV(8,6)
      GV(8,6) = TEMP47X

      TEMP48X = GV(6,9)
      GV(6,9) = GV(9,6)
      GV(9,6) = TEMP48X

      TEMP49X = GV(6,10)
      GV(6,10) = GV(10,6)
      GV(10,6) = TEMP49X

      TEMP50X = GV(6,11)
      GV(6,11) = GV(11,6)
      GV(11,6) = TEMP50X

      TEMP51X = GV(6,12)
      GV(6,12) = GV(12,6)
      GV(12,6) = TEMP51X

      TEMP52X = GV(7,8)
      GV(7,8) = GV(8,7)
      GV(8,7) = TEMP52X

      TEMP53X = GV(7,9)
      GV(7,9) = GV(9,7)
      GV(9,7) = TEMP53X

      TEMP54X = GV(7,10)
      GV(7,10) = GV(10,7)
      GV(10,7) = TEMP54X

      TEMP55X = GV(7,11)
      GV(7,11) = GV(11,7)
      GV(11,7) = TEMP55X

      TEMP56X = GV(7,12)
      GV(7,12) = GV(12,7)
      GV(12,7) = TEMP56X

      TEMP57X = GV(8,9)
      GV(8,9) = GV(9,8)
      GV(9,8) = TEMP57X

      TEMP58X = GV(8,10)
      GV(8,10) = GV(10,8)
      GV(10,8) = TEMP58X

      TEMP59X = GV(8,11)
      GV(8,11) = GV(11,8)
      GV(11,8) = TEMP59X

      TEMP60X = GV(8,12)
      GV(8,12) = GV(12,8)
      GV(12,8) = TEMP60X

      TEMP61X = GV(9,10)
      GV(9,10) = GV(10,9)
      GV(10,9) = TEMP61X

      TEMP62X = GV(9,11)
      GV(9,11) = GV(11,9)
      GV(11,9) = TEMP62X

      TEMP63X = GV(9,12)
      GV(9,12) = GV(12,9)
      GV(12,9) = TEMP63X

      TEMP64X = GV(10,11)
      GV(10,11) = GV(11,10)
      GV(11,10) = TEMP64X

      TEMP65X = GV(10,12)
      GV(10,12) = GV(12,10)
      GV(12,10) = TEMP65X

      TEMP66X = GV(11,12)
      GV(11,12) = GV(12,11)
      GV(12,11) = TEMP66X


      DO I = 1, 12
         DX(I) = GV(1,I)
      ENDDO

c
c  Initialize second derivatives
c
       do j=1,N3TM
           DXX(j)=0.0d0
       enddo


       DO I = 1, 12

       GXX(1,2,I) = 0.0D0
       GXX(1,3,I) = 0.0D0
       GXX(1,4,I) = 0.0D0
       GXX(1,5,I) = 0.0D0
       GXX(1,6,I) = 0.0D0
       GXX(1,7,I) = 0.0D0
       GXX(1,8,I) = 0.0D0
       GXX(1,9,I) = 0.0D0
       GXX(1,10,I) = 0.0D0
       GXX(1,11,I) = 0.0D0
       GXX(1,12,I) = 0.0D0

       GXX(2,1,I) = 0.0D0
       GXX(2,3,I) = 0.0D0
       GXX(2,4,I) = 0.0D0
       GXX(2,5,I) = 0.0D0
       GXX(2,6,I) = 0.0D0
       GXX(2,7,I) = 0.0D0
       GXX(2,8,I) = 0.0D0
       GXX(2,9,I) = 0.0D0
       GXX(2,10,I) = 0.0D0
       GXX(2,11,I) = 0.0D0
       GXX(2,12,I) = 0.0D0

       GXX(3,1,I) = 0.0D0
       GXX(3,2,I) = 0.0D0
       GXX(3,4,I) = 0.0D0
       GXX(3,5,I) = 0.0D0
       GXX(3,6,I) = 0.0D0
       GXX(3,7,I) = 0.0D0
       GXX(3,8,I) = 0.0D0
       GXX(3,9,I) = 0.0D0
       GXX(3,10,I) = 0.0D0
       GXX(3,11,I) = 0.0D0
       GXX(3,12,I) = 0.0D0

       GXX(4,1,I) = 0.0D0
       GXX(4,2,I) = 0.0D0
       GXX(4,3,I) = 0.0D0
       GXX(4,5,I) = 0.0D0
       GXX(4,6,I) = 0.0D0
       GXX(4,7,I) = 0.0D0
       GXX(4,8,I) = 0.0D0
       GXX(4,9,I) = 0.0D0
       GXX(4,10,I) = 0.0D0
       GXX(4,11,I) = 0.0D0
       GXX(4,12,I) = 0.0D0

       GXX(5,1,I) = 0.0D0
       GXX(5,2,I) = 0.0D0
       GXX(5,3,I) = 0.0D0
       GXX(5,4,I) = 0.0D0
       GXX(5,6,I) = 0.0D0
       GXX(5,7,I) = 0.0D0
       GXX(5,8,I) = 0.0D0
       GXX(5,9,I) = 0.0D0
       GXX(5,10,I) = 0.0D0
       GXX(5,11,I) = 0.0D0
       GXX(5,12,I) = 0.0D0

       GXX(6,1,I) = 0.0D0
       GXX(6,2,I) = 0.0D0
       GXX(6,3,I) = 0.0D0
       GXX(6,4,I) = 0.0D0
       GXX(6,5,I) = 0.0D0
       GXX(6,7,I) = 0.0D0
       GXX(6,8,I) = 0.0D0
       GXX(6,9,I) = 0.0D0
       GXX(6,10,I) = 0.0D0
       GXX(6,11,I) = 0.0D0
       GXX(6,12,I) = 0.0D0

       GXX(7,1,I) = 0.0D0
       GXX(7,2,I) = 0.0D0
       GXX(7,3,I) = 0.0D0
       GXX(7,4,I) = 0.0D0
       GXX(7,5,I) = 0.0D0
       GXX(7,6,I) = 0.0D0
       GXX(7,8,I) = 0.0D0
       GXX(7,9,I) = 0.0D0
       GXX(7,10,I) = 0.0D0
       GXX(7,11,I) = 0.0D0
       GXX(7,12,I) = 0.0D0

       GXX(8,1,I) = 0.0D0
       GXX(8,2,I) = 0.0D0
       GXX(8,3,I) = 0.0D0
       GXX(8,4,I) = 0.0D0
       GXX(8,5,I) = 0.0D0
       GXX(8,6,I) = 0.0D0
       GXX(8,7,I) = 0.0D0
       GXX(8,9,I) = 0.0D0
       GXX(8,10,I) = 0.0D0
       GXX(8,11,I) = 0.0D0
       GXX(8,12,I) = 0.0D0

       GXX(9,1,I) = 0.0D0
       GXX(9,2,I) = 0.0D0
       GXX(9,3,I) = 0.0D0
       GXX(9,4,I) = 0.0D0
       GXX(9,5,I) = 0.0D0
       GXX(9,6,I) = 0.0D0
       GXX(9,7,I) = 0.0D0
       GXX(9,8,I) = 0.0D0
       GXX(9,10,I) = 0.0D0
       GXX(9,11,I) = 0.0D0
       GXX(9,12,I) = 0.0D0

       GXX(10,1,I) = 0.0D0
       GXX(10,2,I) = 0.0D0
       GXX(10,3,I) = 0.0D0
       GXX(10,4,I) = 0.0D0
       GXX(10,5,I) = 0.0D0
       GXX(10,6,I) = 0.0D0
       GXX(10,7,I) = 0.0D0
       GXX(10,8,I) = 0.0D0
       GXX(10,9,I) = 0.0D0
       GXX(10,11,I) = 0.0D0
       GXX(10,12,I) = 0.0D0

       GXX(11,1,I) = 0.0D0
       GXX(11,2,I) = 0.0D0
       GXX(11,3,I) = 0.0D0
       GXX(11,4,I) = 0.0D0
       GXX(11,5,I) = 0.0D0
       GXX(11,6,I) = 0.0D0
       GXX(11,7,I) = 0.0D0
       GXX(11,8,I) = 0.0D0
       GXX(11,9,I) = 0.0D0
       GXX(11,10,I) = 0.0D0
       GXX(11,12,I) = 0.0D0

       GXX(12,1,I) = 0.0D0
       GXX(12,2,I) = 0.0D0
       GXX(12,3,I) = 0.0D0
       GXX(12,4,I) = 0.0D0
       GXX(12,5,I) = 0.0D0
       GXX(12,6,I) = 0.0D0
       GXX(12,7,I) = 0.0D0
       GXX(12,8,I) = 0.0D0
       GXX(12,9,I) = 0.0D0
       GXX(12,10,I) = 0.0D0
       GXX(12,11,I) = 0.0D0
 
       ENDDO

       
      DO I = 1, 12
       DO J = 1, 12
        DO II = 1, 12
         GXX(I,J,II) = 0.0D0
        ENDDO
       ENDDO
      ENDDO

       GXX(1,1,1) = 1.0D0
       GXX(2,2,2) = 1.0D0
       GXX(3,3,3) = 1.0D0
       GXX(4,4,4) = 1.0D0
       GXX(5,5,5) = 1.0D0
       GXX(6,6,6) = 1.0D0
       GXX(7,7,7) = 1.0D0
       GXX(8,8,8) = 1.0D0
       GXX(9,9,9) = 1.0D0
       GXX(10,10,10) = 1.0D0
       GXX(11,11,11) = 1.0D0
       GXX(12,12,12) = 1.0D0

      DO I = 1, 12
       DO J = 1, 12
        DO II = 1, 12
        ENDDO
       ENDDO
      ENDDO


       DO I = 1,12
         DO J = 1, 12
         GV(I,j) = 0.0D0
         GX(I,j) = 0.0D0
         ENDDO
       ENDDO

c
c  Calculate first derivatives using a analytic gradients
c

      CALL G_ANALYTIC(12, 12, V, GV, GVV, 1, 1, X, GX, GXX, 12, 12)

C  TRANSPOSE GVV

      DO I = 1, 12
         DXX(I) = GVV(1,I,I)/0.529177d0
      ENDDO

      RETURN

      END


c**************************************************************************

      SUBROUTINE POT(V, X)
C
C   System:    H2O2, see Koput, J.; Carter, S.; Handy, N. C.; 
C              J. Phys. Chem. A, 102 (1998) 6325
C              Prepared for Polyrate by VMA, October 2002.
C
C   Reference: Koput,Carter,&Handy, JPC A, 102, 6325 (1998).
C
C   All the information passed to and from the potential energy surface
C   routine is in hartree atomic units.
C
C        This potential is written such that:
C                       X(1)  - X(3)  : X, Y, Z for H1
C                       X(4)  - X(6)  : X, Y, Z for O1
C                       X(7)  - X(9)  : X, Y, Z for O2
C                       X(10) - X(12) : X, Y, Z for H2
C

      IMPLICIT NONE

      DOUBLE PRECISION N, N1, N2
      DOUBLE PRECISION V, PI

      DOUBLE PRECISION COORD(12),DX(12),X(12),PASS(6)

      DOUBLE PRECISION C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
      DOUBLE PRECISION C11,C12,C13,C14,C15,C16,C17,C18

      DOUBLE PRECISION RAB,RBC,RCD,RAC,RBD,RAD
      DOUBLE PRECISION RH1O1,RO1O2,RO2H2
      DOUBLE PRECISION D1, D2, D, AH1OO, AOOH2, THOOH
  
      DOUBLE PRECISION XCOMP1,YCOMP1,ZCOMP1
      DOUBLE PRECISION XCOMP2,YCOMP2,ZCOMP2
      DOUBLE PRECISION XCOMP3,YCOMP3,ZCOMP3
      DOUBLE PRECISION XCOMP4,YCOMP4,ZCOMP4

      DOUBLE PRECISION X_NORMAL1,Y_NORMAL1,Z_NORMAL1
      DOUBLE PRECISION X_NORMAL2,Y_NORMAL2,Z_NORMAL2

      DOUBLE PRECISION ROHLOW,ROHHIGH,ROOLOW,ROOHIGH,RHHLOW

      PARAMETER (ROHLOW = 1.0D0)
      PARAMETER (ROHHIGH = 4.0D0)
      PARAMETER (ROOLOW = 1.8D0)
      PARAMETER (ROOHIGH = 4.0D0)
      PARAMETER (RHHLOW = 0.7D0)

C  Calculate energy at the configuration space point
C  NOTE :: This is not necessarily a quadrature point

C Expecting input in cartesians

       pi=4.d0*atan(1.d0)

       C1 = X(1) - X(4)
       C2 = X(2) - X(5)
       C3 = X(3) - X(6)
       C4 = X(4) - X(7)
       C5 = X(5) - X(8)
       C6 = X(6) - X(9)
       C7 = X(7) - X(10)
       C8 = X(8) - X(11)
       C9 = X(9) - X(12)

       C10 = X(1) - X(7)
       C11 = X(2) - X(8)
       C12 = X(3) - X(9)
       C13 = X(4) - X(10)
       C14 = X(5) - X(11)
       C15 = X(6) - X(12) 
       c16=x(1)-x(10)
       c17=x(2)-x(11)
       c18=x(3)-x(12)

C
C H1 -- O1 -- O2 -- H2
C   rH1O1 rO1O2 rO2H2
C 
C  angle H1O1O2 = AH1OO
C  angle O1O2H2 = AOOH2
C
C  TORSION HOOH = THOOH

       rab=sqrt(c1**2+c2**2+c3**2)
       rbc=sqrt(c4**2+c5**2+c6**2)
       rcd=sqrt(c7**2+c8**2+c9**2)
       rac=sqrt(c10**2+c11**2+c12**2)
       rbd=sqrt(c13**2+c14**2+c15**2)
       rad=sqrt(c16**2+c17**2+c18**2)
       RH1O1 =rab
       RO1O2 =rbc
       RO2H2 =rcd   

C The potential can give weird values for unusual geometries
C replace with some high value in these unphysical regions and
C then exit
C

       N1 = (-C4)*C1 + (-C5)*C2 + (-C6)*C3
       d1=ro1o2*rh1o1  
       AH1OO = ACOS(N1/D1)

       N2 = -(C7*C4 + C8*C5 + C9*C6)
       d2=ro2h2*ro1o2
       AOOH2 = ACOS(N2/D2)

C Calculate torsion angle HOOH

       XCOMP1 = X(4) - X(1)
       YCOMP1 = X(5) - X(2)
       ZCOMP1 = X(6) - X(3)

       XCOMP2 = C10
       YCOMP2 = C11
       ZCOMP2 = C12

       X_NORMAL1 = YCOMP1 * ZCOMP2 - ZCOMP1 * YCOMP2
       Y_NORMAL1 = ZCOMP1 * XCOMP2 - XCOMP1 * ZCOMP2
       Z_NORMAL1 = XCOMP1 * YCOMP2 - YCOMP1 * XCOMP2

       XCOMP3 = X(7) - X(4)
       YCOMP3 = X(8) - X(5)
       ZCOMP3 = X(9) - X(6)

       XCOMP4 = C13
       YCOMP4 = C14
       ZCOMP4 = C15

       X_NORMAL2 = YCOMP3 * ZCOMP4 - ZCOMP3 * YCOMP4
       Y_NORMAL2 = ZCOMP3 * XCOMP4 - XCOMP3 * ZCOMP4
       Z_NORMAL2 = XCOMP3 * YCOMP4 - YCOMP3 * XCOMP4

       N = X_NORMAL1 * X_NORMAL2 + Y_NORMAL1 * Y_NORMAL2 +
     * Z_NORMAL1 * Z_NORMAL2

       D = (SQRT(X_NORMAL1 * X_NORMAL1 + Y_NORMAL1 * Y_NORMAL1
     * + Z_NORMAL1 * Z_NORMAL1)) *
     * (SQRT(X_NORMAL2 * X_NORMAL2 + Y_NORMAL2 * Y_NORMAL2
     * + Z_NORMAL2 * Z_NORMAL2))

       THOOH = ACOS(N/D)  ! in radians

      PASS(1) = RH1O1
      PASS(2) = RO1O2
      PASS(3) = RO2H2
      PASS(4) = AH1OO
      PASS(5) = AOOH2
      PASS(6) = THOOH

C
C Call Koput, Carter, and Handy PES
C

      CALL VIBPOT(PASS,V)

      RETURN
  
      END

c**************************************************************************

      SUBROUTINE VIBPOT(COORD,V)

      IMPLICIT NONE

      DOUBLE PRECISION V, PI

      DOUBLE PRECISION COORD(6), Q1P(0:4), Q2P(0:4), Q3P(0:4), Q4P(0:4)
      DOUBLE PRECISION Q5P(0:4), Q6P(0:4)
      DOUBLE PRECISION IND1(152), IND2(152), IND3(152), IND4(152)
      DOUBLE PRECISION IND5(152), IND6(152), COEFF(152)

      DOUBLE PRECISION RM_OO, RM_OH, ANGLE_OOH, R1, R2, R3
      DOUBLE PRECISION A1, A2, Q1, Q2, Q3, Q4, Q5, Q6

      INTEGER I

      DOUBLE PRECISION ZOE

      data zoe /3.995139982058d-3/

      data ind1 /0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0,  
     *  1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0,     
     *  1, 2, 1, 0, 2, 0, 1, 0, 2, 0, 0, 0, 1, 1, 0, 1,     
     *  0, 0, 1, 1, 1, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 2,     
     *  0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 3, 1, 0, 3, 0,     
     *  0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0, 0,     
     *  1, 1, 2, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 2, 0,     
     *  0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 3, 0, 0, 0, 2,     
     *  0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1,     
     *  1, 0, 0, 0, 1, 0, 0, 0/

      data ind2 /0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1,    
     *      0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0,    
     *      2, 1, 0, 1, 0, 2, 0, 1, 0, 2, 0, 0, 1, 0, 1, 0,    
     *      1, 0, 1, 1, 0, 1, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0,    
     *      2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 3, 1, 0, 1, 0, 3,    
     *      0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0,    
     *      1, 1, 0, 2, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 2,    
     *      0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0, 0,    
     *      2, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1,    
     *      0, 1, 0, 0, 0, 1, 0, 0/

      data ind3 /0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1,   
     *  0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 2, 2,   
     *  2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,   
     *  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0,   
     *  0, 0, 4, 0, 0, 0, 0, 2, 2, 2, 2, 0, 0,   
     *  0, 3, 3, 3, 3, 1, 1, 1, 1, 0, 0, 0, 0,   
     *  0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 1, 1, 1,   
     *  1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,   
     *  1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1,   
     *  1, 0, 0, 0, 0, 3, 0, 0, 0, 0, 1, 1, 1,   
     *  0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1,   
     *  0, 0, 0, 0, 1, 0, 0, 0, 0/

      data ind4 /0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0,    
     * 0, 1, 0, 0, 1, 1, 0, 0, 0, 3, 0, 0, 0,    
     * 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0,    
     * 2, 0, 1, 1, 2, 0, 1, 0, 0, 1, 1, 1, 0,    
     * 1, 1, 0, 0, 0, 4, 0, 0, 0, 2, 0, 2, 0,    
     * 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 3, 0,    
     * 1, 0, 1, 3, 0, 1, 0, 0, 1, 1, 2, 0, 1,   
     * 0, 0, 1, 1, 2, 2, 0, 1, 1, 1, 2, 2, 1,    
     * 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1,    
     * 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0, 0, 0,    
     * 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0,    
     * 0, 1, 0, 1, 0, 0, 0, 1, 0/

      data ind5 /0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0,    
     * 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 3, 0,    
     * 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 1,    
     * 2, 0, 1, 0, 2, 1, 0, 0, 1, 1, 0, 1, 0,    
     * 1, 1, 1, 0, 0, 0, 0, 4, 0, 0, 0, 2, 0,    
     * 2, 2, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0,   
     * 3, 0, 1, 3, 1, 0, 0, 1, 1, 0, 1, 0, 2,    
     * 0, 1, 1, 0, 2, 1, 0, 2, 1, 1, 2, 1, 1,    
     * 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0,    
     * 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 3, 0, 0,    
     * 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0,    
     * 1, 0, 0, 1, 1, 0, 0, 0, 0, 1/

      data ind6 /1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,  
     *  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
     *  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
     *  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
     *  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
     *  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
     *  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     
     *  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    
     *  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    
     *  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,    
     *  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    
     *  2, 2, 2, 2, 3, 3, 3, 3, 3/

      data coeff /0.00482409d0, 0.00325127d0, 0.00026630d0, 
     * 0.00004135d0, 1.08154998d0,   
     * 0.86097723d0,  0.86097723d0,  0.11016112d0,  
     * 0.11016112d0, -0.03637740d0,   
     * -0.03637740d0,  0.17491854d0,  0.17491854d0,  
     * 0.00057054d0, -0.00137967d0,   
     * -0.00137967d0,  0.00062152d0,  0.00062152d0,  
     * 0.01375726d0, -1.15691421d0,   
     * -0.25495918d0, -0.25495918d0, -0.02272830d0, 
     * -0.02272830d0, -0.18381415d0,   
     * -0.18381415d0, -0.34627237d0, -0.34627237d0,  
     * 0.13974588d0,  0.13974588d0,   
     * -0.27290455d0, -0.27290455d0, -0.00674721d0, 
     * -0.00674721d0, -0.02179545d0,   
     * -0.02179545d0, -0.02125643d0, -0.02125643d0, 
     * -0.00491968d0, -0.00491968d0,   
     * 0.00233773d0,  0.00233773d0, -0.00050066d0, 
     * -0.00050066d0,  0.01817536d0,   
     * -0.04666009d0, -0.04666009d0, -0.02424748d0, 
     * -0.02424748d0, -0.01727148d0,   
     * -0.00420506d0, -0.00420506d0, -0.00647944d0, 
     * -0.00647944d0, -1.06749007d0,   
     * -0.35741007d0, -0.35741007d0, -0.00796836d0, 
     * -0.00796836d0, -0.42556742d0,   
     * -0.42556742d0,  0.06278896d0,  0.06278896d0, 
     * -0.04010419d0, -0.04010419d0,   
     * -0.00993912d0,  0.47562894d0,  0.47562894d0, 
     * -0.40830627d0, -0.40830627d0,   
     * 0.22073222d0,  0.22073222d0,  0.07828212d0,  
     * 0.07828212d0, -0.02954687d0,   
     * -0.02954687d0,  0.03057888d0,  0.03057888d0, 
     * -0.06363999d0,-0.06363999d0,   
     * -0.00373964d0, -0.00373964d0, -0.04114668d0, 
     * 0.11249614d0,  0.11249614d0,   
     * 0.02616679d0,  0.02616679d0, -0.07824425d0,  
     * 0.04266205d0,  0.04266205d0,   
     * -0.07420432d0, -0.07420432d0, -0.08251268d0, 
     * -0.08251268d0,  0.00270940d0,   
     * 0.00270940d0,  0.00199953d0,  0.00199953d0, 
     * -0.01292325d0, -0.01292325d0,   
     * -0.02074323d0, -0.02074323d0, -0.00789732d0, 
     * -0.00789732d0, -0.01435326d0,   
     * -0.00180710d0, -0.00180710d0, -0.01135671d0, 
     * -0.01135671d0,  0.00020655d0,   
     * -0.00492533d0, -0.00492533d0,  0.00270990d0,  
     * 0.00270990d0,  0.00376086d0,   
     * 0.00376086d0,  0.00044732d0,  0.00044732d0,  
     * 0.00569979d0, -0.00244774d0,   
     * -0.00244774d0, -0.02065564d0,  0.05249331d0, 
     * -0.02490299d0, -0.02490299d0,   
     * 0.00391460d0,  0.00391460d0,  0.08893744d0,  
     * 0.08893744d0, -0.01051618d0,   
     * 0.00120479d0,  0.00120479d0, -0.00111888d0, 
     * -0.00111888d0,  0.00884757d0,   
     * 0.00416289d0,  0.00416289d0,  0.00126763d0,  
     * 0.00126763d0, -0.00706563d0,   
     * -0.00706563d0, -0.00840146d0, -0.00840146d0, 
     * -0.00139219d0,  0.00801673d0,   
     * 0.00801673d0,  0.00463860d0, -0.00096051d0,  
     * 0.00019906d0,  0.00019906d0,   
     * -0.00057576d0, -0.00057576d0/

C   convert offset parameters to bohr and radians

      rm_oo=1.456199d0 
      rm_oh=0.962755d0
      ANGLE_OOH=100.9059d0      

      pi=4.d0*atan(1.d0)
      angle_ooh=angle_ooh*pi/180.d0

C
C Potential energy surface for H2O2
C Jacek Koput, Stuart Carter, and Nicholas Handy, J. Phys. Chem. A
C 1998, volume 102, pages 6325-6330
C
C V is the potential energy in hartrees 
C minimum configuration is at R(OH)=0.96265, R(OO)=1.45248, 
C theta(OOH)=99.909, and dihed angle=112.456 degrees

C      R1 = COORD(1)                ! radius from H1 to O1 (in bohr)
C      R2 = COORD(2)                ! radius from O1 to O2 (in bohr) 
C      R3 = COORD(3)                ! radius from O2 to H2 (in bohr)
C      A1 = COORD(4)                ! angle between H1-O1-O2 (in radians)
C      A2 = COORD(5)                ! angle between O1-O2-H2 (in radians)
C      Q6 = COORD(6)                ! HOOH torsion angle (in radians)

      R1 = COORD(1)               
      R2 = COORD(2)              
      R3 = COORD(3)          
      A1 = COORD(4)           
      A2 = COORD(5)               
      Q6 = COORD(6)   

C Q1,Q2, and Q3 represent the stretching modes
C Q4 and Q5  are the bending modes
C Q6 is the torsional mode 
C Q1, Q2, Q3 are dimensionless, Q4, Q5, and Q6 are in radians

      Q3 = (R2 - RM_OO) / R2      
      Q1 = (R1 - RM_OH) / R1     
      Q2 = (R3 - RM_OH) / R3
      Q4 = A1 - ANGLE_OOH         
      Q5 = A2 - ANGLE_OOH



      do i=0,4
      q1p(i)=1.d0
      q2p(i)=1.d0
      q3p(i)=1.d0
      q4p(i)=1.d0
      q5p(i)=1.d0
      q6p(i)=1.d0
      enddo


      do i=1,4
       q1p(i)=q1p(i-1)*q1
       q2p(i)=q2p(i-1)*q2
       q3p(i)=q3p(i-1)*q3
       q4p(i)=q4p(i-1)*q4
       q5p(i)=q5p(i-1)*q5
       q6p(i)=cos(i*q6)
      enddo

      v=zoe   

      do i=1,152
        v=v+coeff(i)*q1p(ind1(i))*q2p(ind2(i))*q3p(ind3(i))*  
     *          q4p(ind4(i))*q5p(ind5(i))*q6p(ind6(i))
      enddo


      return

      END



C                           DISCLAIMER
C
C   This file was generated on 12/09/02 by the version of
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
C**************************************************************************
C
      subroutine g_surf(g_p_, v, g_v, ldg_v, x, g_x, ldg_x)
C
C   System:    H2O2, see Koput, J.; Carter, S.; Handy, N. C.; 
C              J. Phys. Chem. A, 102 (1998) 6325
C              Prepared for Polyrate by VMA, October 2002.
C
C   Reference: Koput,Carter,&Handy, JPC A, 102, 6325 (1998).
C
C   All the information passed to and from the potential energy surface
C   routine is in hartree atomic units.
C
C        This potential is written such that:
C                       X(1)  - X(3)  : X, Y, Z for H1
C                       X(4)  - X(6)  : X, Y, Z for O1
C                       X(7)  - X(9)  : X, Y, Z for O2
C                       X(10) - X(12) : X, Y, Z for H2
C

      IMPLICIT NONE

      DOUBLE PRECISION N, N1, N2
      DOUBLE PRECISION V, PI

      DOUBLE PRECISION COORD(12),DX(12),X(12),PASS(6)

      DOUBLE PRECISION C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
      DOUBLE PRECISION C11,C12,C13,C14,C15,C16,C17,C18

      DOUBLE PRECISION RAB,RBC,RCD,RAC,RBD,RAD
      DOUBLE PRECISION RH1O1,RO1O2,RO2H2
      DOUBLE PRECISION D1, D2, D, AH1OO, AOOH2, THOOH
  
      DOUBLE PRECISION XCOMP1,YCOMP1,ZCOMP1
      DOUBLE PRECISION XCOMP2,YCOMP2,ZCOMP2
      DOUBLE PRECISION XCOMP3,YCOMP3,ZCOMP3
      DOUBLE PRECISION XCOMP4,YCOMP4,ZCOMP4

      DOUBLE PRECISION X_NORMAL1,Y_NORMAL1,Z_NORMAL1
      DOUBLE PRECISION X_NORMAL2,Y_NORMAL2,Z_NORMAL2

      DOUBLE PRECISION ROHLOW,ROHHIGH,ROOLOW,ROOHIGH,RHHLOW

      PARAMETER (ROHLOW = 1.0D0)
      PARAMETER (ROHHIGH = 4.0D0)
      PARAMETER (ROOLOW = 1.8D0)
      PARAMETER (ROOHIGH = 4.0D0)
      PARAMETER (RHHLOW = 0.7D0)


        integer g_pmax_
        parameter (g_pmax_ = 12)
        integer g_i_, g_p_, ldg_x, ldg_v
        double precision d2_w, d1_p, d7_v, d2_p, d4_b, d5_b, d3_p, d2_v,
     * d3_v, d7_b
        double precision d2_b, d3_b, d1_w, d4_v, d8_b, g_c1(g_pmax_), g_
     *x(ldg_x, 12), g_c2(g_pmax_), g_c3(g_pmax_), g_c4(g_pmax_)
        double precision g_c5(g_pmax_), g_c6(g_pmax_), g_c7(g_pmax_), g_
     *c8(g_pmax_), g_c9(g_pmax_), g_c10(g_pmax_), g_c11(g_pmax_), g_c12(
     *g_pmax_), g_c13(g_pmax_), g_c14(g_pmax_)
        double precision g_c15(g_pmax_), g_d1_w(g_pmax_), g_rab(g_pmax_)
     *, g_rbc(g_pmax_), g_rcd(g_pmax_), g_rh1o1(g_pmax_), g_ro1o2(g_pmax
     *_), g_ro2h2(g_pmax_), g_n1(g_pmax_), g_d1(g_pmax_)
        double precision g_ah1oo(g_pmax_), g_n2(g_pmax_), g_d2(g_pmax_),
     * g_aooh2(g_pmax_), g_xcomp1(g_pmax_), g_ycomp1(g_pmax_), g_zcomp1(
     *g_pmax_), g_xcomp2(g_pmax_), g_ycomp2(g_pmax_), g_zcomp2(g_pmax_)
        double precision g_x_normal1(g_pmax_), g_y_normal1(g_pmax_), g_z
     *_normal1(g_pmax_), g_xcomp3(g_pmax_), g_ycomp3(g_pmax_), g_zcomp3(
     *g_pmax_), g_xcomp4(g_pmax_), g_ycomp4(g_pmax_), g_zcomp4(g_pmax_),
     * g_x_normal2(g_pmax_)
        double precision g_y_normal2(g_pmax_), g_z_normal2(g_pmax_), g_n
     *(g_pmax_), g_d2_w(g_pmax_), g_d(g_pmax_), g_thooh(g_pmax_), g_pass
     *(g_pmax_, 6), g_v(ldg_v)
        integer g_ehfid
        save g_pass
        save g_xcomp4, g_ycomp4, g_zcomp4, g_x_normal2, g_y_normal2, g_z
     *_normal2, g_n, g_d2_w, g_d, g_thooh
        save g_zcomp1, g_xcomp2, g_ycomp2, g_zcomp2, g_x_normal1, g_y_no
     *rmal1, g_z_normal1, g_xcomp3, g_ycomp3, g_zcomp3
        save g_ro1o2, g_ro2h2, g_n1, g_d1, g_ah1oo, g_n2, g_d2, g_aooh2,
     * g_xcomp1, g_ycomp1
        save g_c11, g_c12, g_c13, g_c14, g_c15, g_d1_w, g_rab, g_rbc, g_
     *rcd, g_rh1o1
        save g_c1, g_c2, g_c3, g_c4, g_c5, g_c6, g_c7, g_c8, g_c9, g_c10
        external g_vibpot


C
C  Calculate energy at the configuration space point
C  NOTE :: This is not necessarily a quadrature point
C
C Expecting input in cartesians
C
        data g_ehfid /0/

        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        pi = 4.d0 * atan(1.d0)
C
        do g_i_ = 1, g_p_
          g_c1(g_i_) = -g_x(g_i_, 4) + g_x(g_i_, 1)
        enddo
        c1 = x(1) - x(4)
C--------
        do g_i_ = 1, g_p_
          g_c2(g_i_) = -g_x(g_i_, 5) + g_x(g_i_, 2)
        enddo
        c2 = x(2) - x(5)
C--------
        do g_i_ = 1, g_p_
          g_c3(g_i_) = -g_x(g_i_, 6) + g_x(g_i_, 3)
        enddo
        c3 = x(3) - x(6)
C--------
        do g_i_ = 1, g_p_
          g_c4(g_i_) = -g_x(g_i_, 7) + g_x(g_i_, 4)
        enddo
        c4 = x(4) - x(7)
C--------
        do g_i_ = 1, g_p_
          g_c5(g_i_) = -g_x(g_i_, 8) + g_x(g_i_, 5)
        enddo
        c5 = x(5) - x(8)
C--------
        do g_i_ = 1, g_p_
          g_c6(g_i_) = -g_x(g_i_, 9) + g_x(g_i_, 6)
        enddo
        c6 = x(6) - x(9)
C--------
        do g_i_ = 1, g_p_
          g_c7(g_i_) = -g_x(g_i_, 10) + g_x(g_i_, 7)
        enddo
        c7 = x(7) - x(10)
C--------
        do g_i_ = 1, g_p_
          g_c8(g_i_) = -g_x(g_i_, 11) + g_x(g_i_, 8)
        enddo
        c8 = x(8) - x(11)
C--------
        do g_i_ = 1, g_p_
          g_c9(g_i_) = -g_x(g_i_, 12) + g_x(g_i_, 9)
        enddo
        c9 = x(9) - x(12)
C--------
C
        do g_i_ = 1, g_p_
          g_c10(g_i_) = -g_x(g_i_, 7) + g_x(g_i_, 1)
        enddo
        c10 = x(1) - x(7)
C--------
        do g_i_ = 1, g_p_
          g_c11(g_i_) = -g_x(g_i_, 8) + g_x(g_i_, 2)
        enddo
        c11 = x(2) - x(8)
C--------
        do g_i_ = 1, g_p_
          g_c12(g_i_) = -g_x(g_i_, 9) + g_x(g_i_, 3)
        enddo
        c12 = x(3) - x(9)
C--------
        do g_i_ = 1, g_p_
          g_c13(g_i_) = -g_x(g_i_, 10) + g_x(g_i_, 4)
        enddo
        c13 = x(4) - x(10)
C--------
        do g_i_ = 1, g_p_
          g_c14(g_i_) = -g_x(g_i_, 11) + g_x(g_i_, 5)
        enddo
        c14 = x(5) - x(11)
C--------
        do g_i_ = 1, g_p_
          g_c15(g_i_) = -g_x(g_i_, 12) + g_x(g_i_, 6)
        enddo
        c15 = x(6) - x(12)
C--------
        c16 = x(1) - x(10)
        c17 = x(2) - x(11)
        c18 = x(3) - x(12)
C
C
C H1 -- O1 -- O2 -- H2
C   rH1O1 rO1O2 rO2H2
C 
C  angle H1O1O2 = AH1OO
C  angle O1O2H2 = AOOH2
C
C  TORSION HOOH = THOOH
C
        d2_v = c1 * c1
        d3_p = 2.0d0 * c1
        d4_v = c2 * c2
        d2_p = 2.0d0 * c2
        d7_v = c3 * c3
        d1_p = 2.0d0 * c3
        do g_i_ = 1, g_p_
          g_d1_w(g_i_) = d1_p * g_c3(g_i_) + d2_p * g_c2(g_i_) + d3_p * 
     *g_c1(g_i_)
        enddo
        d1_w = d2_v + d4_v + d7_v
        d2_v = sqrt(d1_w)

        if ( d1_w .gt. 0.0d0 ) then
           d1_p = 1.0d0 / (2.0d0 *  d2_v)
        else
c           call ehufDO (9,d1_w, d2_v, d1_p,
c     +g_ehfid,
c     +201)
        endif
        do g_i_ = 1, g_p_
          g_rab(g_i_) = d1_p * g_d1_w(g_i_)
        enddo
        rab = d2_v
C--------
        d2_v = c4 * c4
        d3_p = 2.0d0 * c4
        d4_v = c5 * c5
        d2_p = 2.0d0 * c5
        d7_v = c6 * c6
        d1_p = 2.0d0 * c6
        do g_i_ = 1, g_p_
          g_d1_w(g_i_) = d1_p * g_c6(g_i_) + d2_p * g_c5(g_i_) + d3_p * 
     *g_c4(g_i_)
        enddo
        d1_w = d2_v + d4_v + d7_v
        d2_v = sqrt(d1_w)

        if ( d1_w .gt. 0.0d0 ) then
           d1_p = 1.0d0 / (2.0d0 *  d2_v)
        else
c           call ehufDO (9,d1_w, d2_v, d1_p,
c     +g_ehfid,
c     +226)
        endif
        do g_i_ = 1, g_p_
          g_rbc(g_i_) = d1_p * g_d1_w(g_i_)
        enddo
        rbc = d2_v
C--------
        d2_v = c7 * c7
        d3_p = 2.0d0 * c7
        d4_v = c8 * c8
        d2_p = 2.0d0 * c8
        d7_v = c9 * c9
        d1_p = 2.0d0 * c9
        do g_i_ = 1, g_p_
          g_d1_w(g_i_) = d1_p * g_c9(g_i_) + d2_p * g_c8(g_i_) + d3_p * 
     *g_c7(g_i_)
        enddo
        d1_w = d2_v + d4_v + d7_v
        d2_v = sqrt(d1_w)

        if ( d1_w .gt. 0.0d0 ) then
           d1_p = 1.0d0 / (2.0d0 *  d2_v)
        else
c           call ehufDO (9,d1_w, d2_v, d1_p,
c     +g_ehfid,
c     +251)
        endif
        do g_i_ = 1, g_p_
          g_rcd(g_i_) = d1_p * g_d1_w(g_i_)
        enddo
        rcd = d2_v
C--------
        rac = sqrt(c10 ** 2 + c11 ** 2 + c12 ** 2)
        rbd = sqrt(c13 ** 2 + c14 ** 2 + c15 ** 2)
        rad = sqrt(c16 ** 2 + c17 ** 2 + c18 ** 2)
        do g_i_ = 1, g_p_
          g_rh1o1(g_i_) = g_rab(g_i_)
        enddo
        rh1o1 = rab
C--------
        do g_i_ = 1, g_p_
          g_ro1o2(g_i_) = g_rbc(g_i_)
        enddo
        ro1o2 = rbc
C--------
        do g_i_ = 1, g_p_
          g_ro2h2(g_i_) = g_rcd(g_i_)
        enddo
        ro2h2 = rcd
C--------
C
C The potential can give weird values for unusual geometries
C replace with some high value in these unphysical regions and
C then exit
C
C
C      if((min(rab,rcd,rac,rbd).gt.rohhigh) .or.
C     !(min(rab,rcd,rac,rbd).lt.rohlow)  .or.
C     !(rbc.gt.roohigh) .or. (rbc.lt.roolow) .or.
C     !(rad.lt.rhhlow)) then
C      vdc=1000.d0
C      return
C      endif
C
        do g_i_ = 1, g_p_
          g_n1(g_i_) = (-c6) * g_c3(g_i_) + (-c3) * g_c6(g_i_) + (-c5) *
     * g_c2(g_i_) + (-c2) * g_c5(g_i_) + (-c4) * g_c1(g_i_) + (-c1) * g_
     *c4(g_i_)
        enddo
        n1 = (-c4) * c1 + (-c5) * c2 + (-c6) * c3
C--------
        do g_i_ = 1, g_p_
          g_d1(g_i_) = ro1o2 * g_rh1o1(g_i_) + rh1o1 * g_ro1o2(g_i_)
        enddo
        d1 = ro1o2 * rh1o1
C--------
        d3_v = n1 / d1
        d2_b = 1.0d0 / d1
        d3_b = (-d3_v) / d1
        do g_i_ = 1, g_p_
          g_d1_w(g_i_) = d3_b * g_d1(g_i_) + d2_b * g_n1(g_i_)
        enddo
        d1_w = d3_v
        d2_v = acos(d1_w)
        
        if ( abs(d1_w) .lt. 1.0d0 ) then
           d1_p = -1.0d0 / sqrt ((1.0d0-d1_w)*(1.0d0+d1_w))
        else
c           call ehufDO (14,d1_w, d2_v, d1_p,
c     +g_ehfid,
c     +316)
        endif
        do g_i_ = 1, g_p_
          g_ah1oo(g_i_) = d1_p * g_d1_w(g_i_)
        enddo
        ah1oo = d2_v
C--------
C
        do g_i_ = 1, g_p_
          g_n2(g_i_) = (-c9) * g_c6(g_i_) + (-c6) * g_c9(g_i_) + (-c8) *
     * g_c5(g_i_) + (-c5) * g_c8(g_i_) + (-c7) * g_c4(g_i_) + (-c4) * g_
     *c7(g_i_)
        enddo
        n2 = -(c7 * c4 + c8 * c5 + c9 * c6)
C--------
        do g_i_ = 1, g_p_
          g_d2(g_i_) = ro2h2 * g_ro1o2(g_i_) + ro1o2 * g_ro2h2(g_i_)
        enddo
        d2 = ro2h2 * ro1o2
C--------
        d3_v = n2 / d2
        d2_b = 1.0d0 / d2
        d3_b = (-d3_v) / d2
        do g_i_ = 1, g_p_
          g_d1_w(g_i_) = d3_b * g_d2(g_i_) + d2_b * g_n2(g_i_)
        enddo
        d1_w = d3_v
        d2_v = acos(d1_w)
        
        if ( abs(d1_w) .lt. 1.0d0 ) then
           d1_p = -1.0d0 / sqrt ((1.0d0-d1_w)*(1.0d0+d1_w))
        else
c           call ehufDO (14,d1_w, d2_v, d1_p,
c     +g_ehfid,
c     +350)
        endif
        do g_i_ = 1, g_p_
          g_aooh2(g_i_) = d1_p * g_d1_w(g_i_)
        enddo
        aooh2 = d2_v
C--------
C
C Calculate torsion angle HOOH
C
        do g_i_ = 1, g_p_
          g_xcomp1(g_i_) = -g_x(g_i_, 1) + g_x(g_i_, 4)
        enddo
        xcomp1 = x(4) - x(1)
C--------
        do g_i_ = 1, g_p_
          g_ycomp1(g_i_) = -g_x(g_i_, 2) + g_x(g_i_, 5)
        enddo
        ycomp1 = x(5) - x(2)
C--------
        do g_i_ = 1, g_p_
          g_zcomp1(g_i_) = -g_x(g_i_, 3) + g_x(g_i_, 6)
        enddo
        zcomp1 = x(6) - x(3)
C--------
C
        do g_i_ = 1, g_p_
          g_xcomp2(g_i_) = g_c10(g_i_)
        enddo
        xcomp2 = c10
C--------
        do g_i_ = 1, g_p_
          g_ycomp2(g_i_) = g_c11(g_i_)
        enddo
        ycomp2 = c11
C--------
        do g_i_ = 1, g_p_
          g_zcomp2(g_i_) = g_c12(g_i_)
        enddo
        zcomp2 = c12
C--------
C
        do g_i_ = 1, g_p_
          g_x_normal1(g_i_) = (-zcomp1) * g_ycomp2(g_i_) + (-ycomp2) * g
     *_zcomp1(g_i_) + ycomp1 * g_zcomp2(g_i_) + zcomp2 * g_ycomp1(g_i_)
        enddo
        x_normal1 = ycomp1 * zcomp2 - zcomp1 * ycomp2
C--------
        do g_i_ = 1, g_p_
          g_y_normal1(g_i_) = (-xcomp1) * g_zcomp2(g_i_) + (-zcomp2) * g
     *_xcomp1(g_i_) + zcomp1 * g_xcomp2(g_i_) + xcomp2 * g_zcomp1(g_i_)
        enddo
        y_normal1 = zcomp1 * xcomp2 - xcomp1 * zcomp2
C--------
        do g_i_ = 1, g_p_
          g_z_normal1(g_i_) = (-ycomp1) * g_xcomp2(g_i_) + (-xcomp2) * g
     *_ycomp1(g_i_) + xcomp1 * g_ycomp2(g_i_) + ycomp2 * g_xcomp1(g_i_)
        enddo
        z_normal1 = xcomp1 * ycomp2 - ycomp1 * xcomp2
C--------
C
        do g_i_ = 1, g_p_
          g_xcomp3(g_i_) = -g_x(g_i_, 4) + g_x(g_i_, 7)
        enddo
        xcomp3 = x(7) - x(4)
C--------
        do g_i_ = 1, g_p_
          g_ycomp3(g_i_) = -g_x(g_i_, 5) + g_x(g_i_, 8)
        enddo
        ycomp3 = x(8) - x(5)
C--------
        do g_i_ = 1, g_p_
          g_zcomp3(g_i_) = -g_x(g_i_, 6) + g_x(g_i_, 9)
        enddo
        zcomp3 = x(9) - x(6)
C--------
C
        do g_i_ = 1, g_p_
          g_xcomp4(g_i_) = g_c13(g_i_)
        enddo
        xcomp4 = c13
C--------
        do g_i_ = 1, g_p_
          g_ycomp4(g_i_) = g_c14(g_i_)
        enddo
        ycomp4 = c14
C--------
        do g_i_ = 1, g_p_
          g_zcomp4(g_i_) = g_c15(g_i_)
        enddo
        zcomp4 = c15
C--------
C
        do g_i_ = 1, g_p_
          g_x_normal2(g_i_) = (-zcomp3) * g_ycomp4(g_i_) + (-ycomp4) * g
     *_zcomp3(g_i_) + ycomp3 * g_zcomp4(g_i_) + zcomp4 * g_ycomp3(g_i_)
        enddo
        x_normal2 = ycomp3 * zcomp4 - zcomp3 * ycomp4
C--------
        do g_i_ = 1, g_p_
          g_y_normal2(g_i_) = (-xcomp3) * g_zcomp4(g_i_) + (-zcomp4) * g
     *_xcomp3(g_i_) + zcomp3 * g_xcomp4(g_i_) + xcomp4 * g_zcomp3(g_i_)
        enddo
        y_normal2 = zcomp3 * xcomp4 - xcomp3 * zcomp4
C--------
        do g_i_ = 1, g_p_
          g_z_normal2(g_i_) = (-ycomp3) * g_xcomp4(g_i_) + (-xcomp4) * g
     *_ycomp3(g_i_) + xcomp3 * g_ycomp4(g_i_) + ycomp4 * g_xcomp3(g_i_)
        enddo
        z_normal2 = xcomp3 * ycomp4 - ycomp3 * xcomp4
C--------
C
        do g_i_ = 1, g_p_
          g_n(g_i_) = z_normal1 * g_z_normal2(g_i_) + z_normal2 * g_z_no
     *rmal1(g_i_) + y_normal1 * g_y_normal2(g_i_) + y_normal2 * g_y_norm
     *al1(g_i_) + x_normal1 * g_x_normal2(g_i_) + x_normal2 * g_x_normal
     *1(g_i_)
        enddo
        n = x_normal1 * x_normal2 + y_normal1 * y_normal2 + z_normal1 * 
     *z_normal2
C--------
C
        d4_b = z_normal1 + z_normal1
        d7_b = y_normal1 + y_normal1
        d8_b = x_normal1 + x_normal1
        do g_i_ = 1, g_p_
          g_d1_w(g_i_) = d4_b * g_z_normal1(g_i_) + d7_b * g_y_normal1(g
     *_i_) + d8_b * g_x_normal1(g_i_)
        enddo
        d1_w = x_normal1 * x_normal1 + y_normal1 * y_normal1 + z_normal1
     * * z_normal1
        d4_b = z_normal2 + z_normal2
        d7_b = y_normal2 + y_normal2
        d8_b = x_normal2 + x_normal2
        do g_i_ = 1, g_p_
          g_d2_w(g_i_) = d4_b * g_z_normal2(g_i_) + d7_b * g_y_normal2(g
     *_i_) + d8_b * g_x_normal2(g_i_)
        enddo
        d2_w = x_normal2 * x_normal2 + y_normal2 * y_normal2 + z_normal2
     * * z_normal2
        d2_v = sqrt(d1_w)

        if ( d1_w .gt. 0.0d0 ) then
           d2_p = 1.0d0 / (2.0d0 *  d2_v)
        else
c           call ehufDO (9,d1_w, d2_v, d2_p,
c     +g_ehfid,
c     +497)
        endif
        d4_v = sqrt(d2_w)

        if ( d2_w .gt. 0.0d0 ) then
           d1_p = 1.0d0 / (2.0d0 *  d4_v)
        else
c           call ehufDO (9,d2_w, d4_v, d1_p,
c     +g_ehfid,
c     +506)
        endif
        d4_b = d2_v * d1_p
        d5_b = d4_v * d2_p
        do g_i_ = 1, g_p_
          g_d(g_i_) = d4_b * g_d2_w(g_i_) + d5_b * g_d1_w(g_i_)
        enddo
        d = d2_v * d4_v
C--------
C
C  We can occasionally get "NaNs" if we don't trap for special cases while calcu
Clating
C  the dihedral angle.  Acos wants an argument in [-1,1], but slight roundoff ca
Cn push us
C  outside this range.  Also if certain triatoms are collinear then the dihedral
C is not defined
C  and we set these cases to 0 degrees
C
C      if(d.eq.0.d0)then 
C       thooh=0.d0
C      else
C       argnd=n/d
C      if(argnd.ge.1.d0)then
C       thooh=0.d0
C      elseif(argnd.le.-1.d0)then
C       thooh=pi
C      else
        d3_v = n / d
        d2_b = 1.0d0 / d
        d3_b = (-d3_v) / d
        do g_i_ = 1, g_p_
          g_d1_w(g_i_) = d3_b * g_d(g_i_) + d2_b * g_n(g_i_)
        enddo
        d1_w = d3_v
        d2_v = acos(d1_w)
        
        if ( abs(d1_w) .lt. 1.0d0 ) then
           d1_p = -1.0d0 / sqrt ((1.0d0-d1_w)*(1.0d0+d1_w))
        else
c           call ehufDO (14,d1_w, d2_v, d1_p,
c     +g_ehfid,
c     +547)
        endif
        do g_i_ = 1, g_p_
          g_thooh(g_i_) = d1_p * g_d1_w(g_i_)
        enddo
        thooh = d2_v
C--------
C      endif
C      endif
C
        do g_i_ = 1, g_p_
          g_pass(g_i_, 1) = g_rh1o1(g_i_)
        enddo
        pass(1) = rh1o1
C--------
        do g_i_ = 1, g_p_
          g_pass(g_i_, 2) = g_ro1o2(g_i_)
        enddo
        pass(2) = ro1o2
C--------
        do g_i_ = 1, g_p_
          g_pass(g_i_, 3) = g_ro2h2(g_i_)
        enddo
        pass(3) = ro2h2
C--------
        do g_i_ = 1, g_p_
          g_pass(g_i_, 4) = g_ah1oo(g_i_)
        enddo
        pass(4) = ah1oo
C--------
        do g_i_ = 1, g_p_
          g_pass(g_i_, 5) = g_aooh2(g_i_)
        enddo
        pass(5) = aooh2
C--------
        do g_i_ = 1, g_p_
          g_pass(g_i_, 6) = g_thooh(g_i_)
        enddo
        pass(6) = thooh
C--------
C
C
C Call Koput, Carter, and Handy PES
C
C
        call g_vibpot(g_p_, pass, g_pass, g_pmax_, v, g_v, ldg_v)
C
        return
C
      end
C
C**************************************************************************
C
      subroutine g_vibpot(g_p_, coord, g_coord, ldg_coord, v, g_v, ldg_v
     *)
C

      IMPLICIT NONE

      DOUBLE PRECISION V, PI

      DOUBLE PRECISION COORD(6), Q1P(0:4), Q2P(0:4), Q3P(0:4), Q4P(0:4)
      DOUBLE PRECISION Q5P(0:4), Q6P(0:4)
      DOUBLE PRECISION IND1(152), IND2(152), IND3(152), IND4(152)
      DOUBLE PRECISION IND5(152), IND6(152), COEFF(152)

      DOUBLE PRECISION RM_OO, RM_OH, ANGLE_OOH, R1, R2, R3
      DOUBLE PRECISION A1, A2, Q1, Q2, Q3, Q4, Q5, Q6

      INTEGER I

      DOUBLE PRECISION ZOE


        integer g_pmax_
        parameter (g_pmax_ = 12)
        integer g_i_, g_p_, ldg_coord, ldg_v
        double precision d14_b, d13_b, d7_v, d11_b, d10_b, d9_b, d8_b, d
     *7_b, d2_v, d3_v
        double precision d6_b, d2_b, d3_b, d1_w, d1_p, d11_v, d5_v, d9_v
     *, g_r1(g_pmax_), g_coord(ldg_coord, 6)
        double precision g_r2(g_pmax_), g_r3(g_pmax_), g_a1(g_pmax_), g_
     *a2(g_pmax_), g_q6(g_pmax_), g_q3(g_pmax_), g_q1(g_pmax_), g_q2(g_p
     *max_), g_q4(g_pmax_), g_q5(g_pmax_)
        double precision g_q1p(g_pmax_, 0:4), g_q2p(g_pmax_, 0:4), g_q3p
     *(g_pmax_, 0:4), g_q4p(g_pmax_, 0:4), g_q5p(g_pmax_, 0:4), g_q6p(g_
     *pmax_, 0:4), g_d1_w(g_pmax_), g_v(ldg_v)
        integer g_ehfid
        save g_q5, g_q1p, g_q2p, g_q3p, g_q4p, g_q5p, g_q6p, g_d1_w
        save g_r1, g_r2, g_r3, g_a1, g_a2, g_q6, g_q3, g_q1, g_q2, g_q4
        intrinsic dble

        data zoe /3.995139982058d-3/
C
        data ind1 /0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0,
     * 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 2, 1, 0, 2, 0, 1, 0,
     * 2, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 4, 0, 0, 0, 2, 0, 0,
     * 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 3, 1, 0, 3, 0, 0, 0, 1, 1,
     * 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0, 0, 1, 1, 2, 0, 1, 0, 1, 0, 0, 1,
     * 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 3, 0, 0, 0, 2,
     * 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0,
     * 0, 0/
C
        data ind2 /0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1,
     * 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 2, 1, 0, 1, 0, 2, 0, 1,
     * 0, 2, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 4, 0, 0, 0, 2, 0,
     * 0, 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 3, 1, 0, 1, 0, 3, 0, 0, 1, 0,
     * 1, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0, 1, 1, 0, 2, 0, 1, 0, 1, 0, 0,
     * 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0, 0,
     * 2, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1,
     * 0, 0/
C
        data ind3 /0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
     * 0, 3, 0, 0, 0, 0, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 4, 0, 0, 0, 0, 2, 2, 2,
     * 2, 0, 0, 0, 3, 3, 3, 3, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2,
     * 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
     * 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 3, 0, 0, 0, 0, 1,
     * 1, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0,
     * 0, 0/
C
        data ind4 /0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1,
     * 1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 2,
     * 0, 1, 1, 2, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 4, 0, 0, 0, 2,
     * 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 3, 0, 1, 0, 1, 3, 0, 1,
     * 0, 0, 1, 1, 2, 0, 1, 0, 0, 1, 1, 2, 2, 0, 1, 1, 1, 2, 2, 1, 0, 0,
     * 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0,
     * 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0,
     * 1, 0/
C
        data ind5 /0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0,
     * 1, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 1, 2, 0,
     * 1, 0, 2, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 4, 0, 0, 0,
     * 2, 0, 2, 2, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 3, 0, 1, 3, 1, 0, 0,
     * 1, 1, 0, 1, 0, 2, 0, 1, 1, 0, 2, 1, 0, 2, 1, 1, 2, 1, 1, 2, 0, 0,
     * 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 3, 0,
     * 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,
     * 0, 1/
C
        data ind6 /1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
     * 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     * 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,
     * 3, 3/
C
        data coeff /0.00482409d0, 0.00325127d0, 0.00026630d0, 0.00004135
     *d0, 1.08154998d0, 0.86097723d0, 0.86097723d0, 0.11016112d0, 0.1101
     *6112d0, -0.03637740d0, -0.03637740d0, 0.17491854d0, 0.17491854d0, 
     *0.00057054d0, -0.00137967d0, -0.00137967d0, 0.00062152d0, 0.000621
     *52d0, 0.01375726d0, -1.15691421d0, -0.25495918d0, -0.25495918d0, -
     *0.02272830d0, -0.02272830d0, -0.18381415d0, -0.18381415d0, -0.3462
     *7237d0, -0.34627237d0, 0.13974588d0, 0.13974588d0, -0.27290455d0, 
     *-0.27290455d0, -0.00674721d0, -0.00674721d0, -0.02179545d0, -0.021
     *79545d0, -0.02125643d0, -0.02125643d0, -0.00491968d0, -0.00491968d
     *0, 0.00233773d0, 0.00233773d0, -0.00050066d0, -0.00050066d0, 0.018
     *17536d0, -0.04666009d0, -0.04666009d0, -0.02424748d0, -0.02424748d
     *0, -0.01727148d0, -0.00420506d0, -0.00420506d0, -0.00647944d0, -0.
     *00647944d0, -1.06749007d0, -0.35741007d0, -0.35741007d0, -0.007968
     *36d0, -0.00796836d0, -0.42556742d0, -0.42556742d0, 0.06278896d0, 0
     *.06278896d0, -0.04010419d0, -0.04010419d0, -0.00993912d0, 0.475628
     *94d0, 0.47562894d0, -0.40830627d0, -0.40830627d0, 0.22073222d0, 0.
     *22073222d0, 0.07828212d0, 0.07828212d0, -0.02954687d0, -0.02954687
     *d0, 0.03057888d0, 0.03057888d0, -0.06363999d0, -0.06363999d0, -0.0
     *0373964d0, -0.00373964d0, -0.04114668d0, 0.11249614d0, 0.11249614d
     *0, 0.02616679d0, 0.02616679d0, -0.07824425d0, 0.04266205d0, 0.0426
     *6205d0, -0.07420432d0, -0.07420432d0, -0.08251268d0, -0.08251268d0
     *, 0.00270940d0, 0.00270940d0, 0.00199953d0, 0.00199953d0, -0.01292
     *325d0, -0.01292325d0, -0.02074323d0, -0.02074323d0, -0.00789732d0,
     * -0.00789732d0, -0.01435326d0, -0.00180710d0, -0.00180710d0, -0.01
     *135671d0, -0.01135671d0, 0.00020655d0, -0.00492533d0, -0.00492533d
     *0, 0.00270990d0, 0.00270990d0, 0.00376086d0, 0.00376086d0, 0.00044
     *732d0, 0.00044732d0, 0.00569979d0, -0.00244774d0, -0.00244774d0, -
     *0.02065564d0, 0.05249331d0, -0.02490299d0, -0.02490299d0, 0.003914
     *60d0, 0.00391460d0, 0.08893744d0, 0.08893744d0, -0.01051618d0, 0.0
     *0120479d0, 0.00120479d0, -0.00111888d0, -0.00111888d0, 0.00884757d
     *0, 0.00416289d0, 0.00416289d0, 0.00126763d0, 0.00126763d0, -0.0070
     *6563d0, -0.00706563d0, -0.00840146d0, -0.00840146d0, -0.00139219d0
     *, 0.00801673d0, 0.00801673d0, 0.00463860d0, -0.00096051d0, 0.00019
     *906d0, 0.00019906d0, -0.00057576d0, -0.00057576d0/
C
C      data rm_oo, rm_oh, ANGLE_OOH/1.456199d0, 0.962755d0, 100.9059d0/
C
C   convert offset parameters to bohr and radians
C
        data g_ehfid /0/
C
c        call ehsfid(g_ehfid, 'vibpot','g_surf.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        rm_oo = 1.456199d0
        rm_oh = 0.962755d0
        angle_ooh = 100.9059d0
C
        pi = 4.d0 * atan(1.d0)
C        rm_oo = rm_oo / 0.529177d0
C        rm_oh = rm_oh / 0.529177d0
        angle_ooh = angle_ooh * pi / 180.d0
C
C
C Potential energy surface for H2O2
C Jacek Koput, Stuart Carter, and Nicholas Handy, J. Phys. Chem. A
C 1998, volume 102, pages 6325-6330
C
C V is the potential energy in hartrees 
C minimum configuration is at R(OH)=0.96265, R(OO)=1.45248, 
C theta(OOH)=99.909, and dihed angle=112.456 degrees
C
C      R1 = COORD(1)                ! radius from H1 to O1 (in bohr)
C      R2 = COORD(2)                ! radius from O1 to O2 (in bohr) 
C      R3 = COORD(3)                ! radius from O2 to H2 (in bohr)
C      A1 = COORD(4)                ! angle between H1-O1-O2 (in radians)
C      A2 = COORD(5)                ! angle between O1-O2-H2 (in radians)
C      Q6 = COORD(6)                ! HOOH torsion angle (in radians)
C
        do g_i_ = 1, g_p_
          g_r1(g_i_) = g_coord(g_i_, 1)
        enddo
        r1 = coord(1)
C--------
        do g_i_ = 1, g_p_
          g_r2(g_i_) = g_coord(g_i_, 2)
        enddo
        r2 = coord(2)
C--------
        do g_i_ = 1, g_p_
          g_r3(g_i_) = g_coord(g_i_, 3)
        enddo
        r3 = coord(3)
C--------
        do g_i_ = 1, g_p_
          g_a1(g_i_) = g_coord(g_i_, 4)
        enddo
        a1 = coord(4)
C--------
        do g_i_ = 1, g_p_
          g_a2(g_i_) = g_coord(g_i_, 5)
        enddo
        a2 = coord(5)
C--------
        do g_i_ = 1, g_p_
          g_q6(g_i_) = g_coord(g_i_, 6)
        enddo
        q6 = coord(6)
C--------
C
C Q1,Q2, and Q3 represent the stretching modes
C Q4 and Q5  are the bending modes
C Q6 is the torsional mode 
C Q1, Q2, Q3 are dimensionless, Q4, Q5, and Q6 are in radians
C
        d3_v = (r2 - rm_oo) / r2
        d3_b = (-d3_v) / r2 + 1.0d0 / r2
        do g_i_ = 1, g_p_
          g_q3(g_i_) = d3_b * g_r2(g_i_)
        enddo
        q3 = d3_v
C--------
        d3_v = (r1 - rm_oh) / r1
        d3_b = (-d3_v) / r1 + 1.0d0 / r1
        do g_i_ = 1, g_p_
          g_q1(g_i_) = d3_b * g_r1(g_i_)
        enddo
        q1 = d3_v
C--------
        d3_v = (r3 - rm_oh) / r3
        d3_b = (-d3_v) / r3 + 1.0d0 / r3
        do g_i_ = 1, g_p_
          g_q2(g_i_) = d3_b * g_r3(g_i_)
        enddo
        q2 = d3_v
C--------
        do g_i_ = 1, g_p_
          g_q4(g_i_) = g_a1(g_i_)
        enddo
        q4 = a1 - angle_ooh
C--------
        do g_i_ = 1, g_p_
          g_q5(g_i_) = g_a2(g_i_)
        enddo
        q5 = a2 - angle_ooh
C--------
C
C
C
        do i = 0, 4
          do g_i_ = 1, g_p_
            g_q1p(g_i_, i) = 0.0d0
          enddo
          q1p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            g_q2p(g_i_, i) = 0.0d0
          enddo
          q2p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            g_q3p(g_i_, i) = 0.0d0
          enddo
          q3p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            g_q4p(g_i_, i) = 0.0d0
          enddo
          q4p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            g_q5p(g_i_, i) = 0.0d0
          enddo
          q5p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            g_q6p(g_i_, i) = 0.0d0
          enddo
          q6p(i) = 1.d0
C--------
        enddo
C
C
        do i = 1, 4
          do g_i_ = 1, g_p_
            g_q1p(g_i_, i) = q1p(i - 1) * g_q1(g_i_) + q1 * g_q1p(g_i_, 
     *i - 1)
          enddo
          q1p(i) = q1p(i - 1) * q1
C--------
          do g_i_ = 1, g_p_
            g_q2p(g_i_, i) = q2p(i - 1) * g_q2(g_i_) + q2 * g_q2p(g_i_, 
     *i - 1)
          enddo
          q2p(i) = q2p(i - 1) * q2
C--------
          do g_i_ = 1, g_p_
            g_q3p(g_i_, i) = q3p(i - 1) * g_q3(g_i_) + q3 * g_q3p(g_i_, 
     *i - 1)
          enddo
          q3p(i) = q3p(i - 1) * q3
C--------
          do g_i_ = 1, g_p_
            g_q4p(g_i_, i) = q4p(i - 1) * g_q4(g_i_) + q4 * g_q4p(g_i_, 
     *i - 1)
          enddo
          q4p(i) = q4p(i - 1) * q4
C--------
          do g_i_ = 1, g_p_
            g_q5p(g_i_, i) = q5p(i - 1) * g_q5(g_i_) + q5 * g_q5p(g_i_, 
     *i - 1)
          enddo
          q5p(i) = q5p(i - 1) * q5
C--------
          d2_b = dble(i)
          do g_i_ = 1, g_p_
            g_d1_w(g_i_) = d2_b * g_q6(g_i_)
          enddo
          d1_w = dble(i) * q6
          d2_v = cos(d1_w)
          d1_p = -sin(d1_w)
          do g_i_ = 1, g_p_
            g_q6p(g_i_, i) = d1_p * g_d1_w(g_i_)
          enddo
          q6p(i) = d2_v
C--------
        enddo
C
        do g_i_ = 1, g_p_
          g_v(g_i_) = 0.0d0
        enddo
        v = zoe
C--------
C
        do i = 1, 152
          d3_v = coeff(i) * q1p(ind1(i))
          d5_v = d3_v * q2p(ind2(i))
          d7_v = d5_v * q3p(ind3(i))
          d9_v = d7_v * q4p(ind4(i))
          d11_v = d9_v * q5p(ind5(i))
          d6_b = q6p(ind6(i)) * q5p(ind5(i))
          d7_b = q6p(ind6(i)) * d9_v
          d8_b = d6_b * q4p(ind4(i))
          d9_b = d6_b * d7_v
          d10_b = d8_b * q3p(ind3(i))
          d11_b = d8_b * d5_v
          d13_b = d10_b * d3_v
          d14_b = d10_b * q2p(ind2(i)) * coeff(i)
          do g_i_ = 1, g_p_
            g_v(g_i_) = d11_v * g_q6p(g_i_, ind6(i)) + d7_b * g_q5p(g_i_
     *, ind5(i)) + d9_b * g_q4p(g_i_, ind4(i)) + d11_b * g_q3p(g_i_, ind
     *3(i)) + d13_b * g_q2p(g_i_, ind2(i)) + d14_b * g_q1p(g_i_, ind1(i)
     *) + g_v(g_i_)
          enddo
          v = v + d11_v * q6p(ind6(i))
C--------
        enddo
C
C
        return
C
      end
C
C
C
C                           DISCLAIMER
C
C   This file was generated on 01/03/03 by the version of
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
C                           DISCLAIMER
C
C   This file was generated on 01/03/03 by the version of
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
C**************************************************************************
C

C      CALL G_ANALYTIC(GPP, GP, V, GV, GVV, LDGVV, LDGV, X, 
C    * GX, GXX, LDGXX, LDGX)

C      CALL G_ANALYTIC(12, 12, V, GV, GVV, 1, 1, X, GX, GXX, 12, 12)

      subroutine G_ANALYTIC(g2p, g_p_, v, g_v, g_g_v, ldg_g_v, ldg_v, x,
     * g_x, g_g_x, ldg_g_x, ldg_x)
C
C   System:    H2O2, see Koput, J.; Carter, S.; Handy, N. C.; 
C              J. Phys. Chem. A, 102 (1998) 6325
C              Prepared for Polyrate by VMA, October 2002.
C
C   Reference: Koput,Carter,&Handy, JPC A, 102, 6325 (1998).
C
C   All the information passed to and from the potential energy surface
C   routine is in hartree atomic units.
C
C        This potential is written such that:
C                       X(1)  - X(3)  : X, Y, Z for H1
C                       X(4)  - X(6)  : X, Y, Z for O1
C                       X(7)  - X(9)  : X, Y, Z for O2
C                       X(10) - X(12) : X, Y, Z for H2
C

      IMPLICIT NONE

      DOUBLE PRECISION N, N1, N2
      DOUBLE PRECISION V, PI

      DOUBLE PRECISION COORD(12),DX(12),X(12),PASS(6)

      DOUBLE PRECISION C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
      DOUBLE PRECISION C11,C12,C13,C14,C15,C16,C17,C18

      DOUBLE PRECISION RAB,RBC,RCD,RAC,RBD,RAD
      DOUBLE PRECISION RH1O1,RO1O2,RO2H2
      DOUBLE PRECISION D1, D2, D, AH1OO, AOOH2, THOOH
  
      DOUBLE PRECISION XCOMP1,YCOMP1,ZCOMP1
      DOUBLE PRECISION XCOMP2,YCOMP2,ZCOMP2
      DOUBLE PRECISION XCOMP3,YCOMP3,ZCOMP3
      DOUBLE PRECISION XCOMP4,YCOMP4,ZCOMP4

      DOUBLE PRECISION X_NORMAL1,Y_NORMAL1,Z_NORMAL1
      DOUBLE PRECISION X_NORMAL2,Y_NORMAL2,Z_NORMAL2

      DOUBLE PRECISION ROHLOW,ROHHIGH,ROOLOW,ROOHIGH,RHHLOW

      PARAMETER (ROHLOW = 1.0D0)
      PARAMETER (ROHHIGH = 4.0D0)
      PARAMETER (ROOLOW = 1.8D0)
      PARAMETER (ROOHIGH = 4.0D0)
      PARAMETER (RHHLOW = 0.7D0)


        integer g_pmax_
        parameter (g_pmax_ = 12)
        integer g2pmax
        parameter (g2pmax = 12)
        integer g_i_, g_p_, ldg_x, ldg_v
        double precision d2_w, d1_p, d7_v, d2_p, d4_b, d5_b, d3_p, d2_v,
     * d3_v, d7_b
        double precision d2_b, d3_b, d1_w, d4_v, d8_b, g_c1(g_pmax_), g_
     *x(ldg_x, 12), g_c2(g_pmax_), g_c3(g_pmax_), g_c4(g_pmax_)
        double precision g_c5(g_pmax_), g_c6(g_pmax_), g_c7(g_pmax_), g_
     *c8(g_pmax_), g_c9(g_pmax_), g_c10(g_pmax_), g_c11(g_pmax_), g_c12(
     *g_pmax_), g_c13(g_pmax_), g_c14(g_pmax_)
        double precision g_c15(g_pmax_), g_d1_w(g_pmax_), g_rab(g_pmax_)
     *, g_rbc(g_pmax_), g_rcd(g_pmax_), g_rh1o1(g_pmax_), g_ro1o2(g_pmax
     *_), g_ro2h2(g_pmax_), g_n1(g_pmax_), g_d1(g_pmax_)
        double precision g_ah1oo(g_pmax_), g_n2(g_pmax_), g_d2(g_pmax_),
     * g_aooh2(g_pmax_), g_xcomp1(g_pmax_), g_ycomp1(g_pmax_), g_zcomp1(
     *g_pmax_), g_xcomp2(g_pmax_), g_ycomp2(g_pmax_), g_zcomp2(g_pmax_)
        double precision g_x_normal1(g_pmax_), g_y_normal1(g_pmax_), g_z
     *_normal1(g_pmax_), g_xcomp3(g_pmax_), g_ycomp3(g_pmax_), g_zcomp3(
     *g_pmax_), g_xcomp4(g_pmax_), g_ycomp4(g_pmax_), g_zcomp4(g_pmax_),
     * g_x_normal2(g_pmax_)
        double precision g_y_normal2(g_pmax_), g_z_normal2(g_pmax_), g_n
     *(g_pmax_), g_d2_w(g_pmax_), g_d(g_pmax_), g_thooh(g_pmax_), g_pass
     *(g_pmax_, 6), g_v(ldg_v)
        integer g_ehfid
        save g_pass
        save g_xcomp4, g_ycomp4, g_zcomp4, g_x_normal2, g_y_normal2, g_z
     *_normal2, g_n, g_d2_w, g_d, g_thooh
        save g_zcomp1, g_xcomp2, g_ycomp2, g_zcomp2, g_x_normal1, g_y_no
     *rmal1, g_z_normal1, g_xcomp3, g_ycomp3, g_zcomp3
        save g_ro1o2, g_ro2h2, g_n1, g_d1, g_ah1oo, g_n2, g_d2, g_aooh2,
     * g_xcomp1, g_ycomp1
        save g_c11, g_c12, g_c13, g_c14, g_c15, g_d1_w, g_rab, g_rbc, g_
     *rcd, g_rh1o1
        save g_c1, g_c2, g_c3, g_c4, g_c5, g_c6, g_c7, g_c8, g_c9, g_c10
        integer g__pmax_
        parameter (g__pmax_ = 12)
        integer g2i, g2p, ldg_g_x, ldg_g_v
        double precision g_g_c1(g__pmax_, g_pmax_), g_g_x(ldg_g_x, ldg_x
     *, 12), g_g_c2(g__pmax_, g_pmax_), g_g_c3(g__pmax_, g_pmax_), g_g_c
     *4(g__pmax_, g_pmax_), g_g_c5(g__pmax_, g_pmax_), g_g_c6(g__pmax_, 
     *g_pmax_), g_g_c7(g__pmax_, g_pmax_), g_g_c8(g__pmax_, g_pmax_), g_
     *g_c9(g__pmax_, g_pmax_)
        double precision g_g_c10(g__pmax_, g_pmax_), g_g_c11(g__pmax_, g
     *_pmax_), g_g_c12(g__pmax_, g_pmax_), g_g_c13(g__pmax_, g_pmax_), g
     *_g_c14(g__pmax_, g_pmax_), g_g_c15(g__pmax_, g_pmax_), g_g_d1_w(g_
     *_pmax_, g_pmax_), g_g_rab(g__pmax_, g_pmax_), g_g_rbc(g__pmax_, g_
     *pmax_), g_g_rcd(g__pmax_, g_pmax_)
        double precision g_g_rh1o1(g__pmax_, g_pmax_), g_g_ro1o2(g__pmax
     *_, g_pmax_), g_g_ro2h2(g__pmax_, g_pmax_), g_g_n1(g__pmax_, g_pmax
     *_), g_g_d1(g__pmax_, g_pmax_), g_g_ah1oo(g__pmax_, g_pmax_), g_g_n
     *2(g__pmax_, g_pmax_), g_g_d2(g__pmax_, g_pmax_), g_g_aooh2(g__pmax
     *_, g_pmax_), g_g_xcomp1(g__pmax_, g_pmax_)
        double precision g_g_ycomp1(g__pmax_, g_pmax_), g_g_zcomp1(g__pm
     *ax_, g_pmax_), g_g_xcomp2(g__pmax_, g_pmax_), g_g_ycomp2(g__pmax_,
     * g_pmax_), g_g_zcomp2(g__pmax_, g_pmax_), g_g_x_normal1(g__pmax_, 
     *g_pmax_), g_g_y_normal1(g__pmax_, g_pmax_), g_g_z_normal1(g__pmax_
     *, g_pmax_), g_g_xcomp3(g__pmax_, g_pmax_), g_g_ycomp3(g__pmax_, g_
     *pmax_)
        double precision g_g_zcomp3(g__pmax_, g_pmax_), g_g_xcomp4(g__pm
     *ax_, g_pmax_), g_g_ycomp4(g__pmax_, g_pmax_), g_g_zcomp4(g__pmax_,
     * g_pmax_), g_g_x_normal2(g__pmax_, g_pmax_), g_g_y_normal2(g__pmax
     *_, g_pmax_), g_g_z_normal2(g__pmax_, g_pmax_), g_g_n(g__pmax_, g_p
     *max_), g_g_d2_w(g__pmax_, g_pmax_), g_g_d(g__pmax_, g_pmax_)
        double precision g_g_thooh(g__pmax_, g_pmax_), g_g_pass(g__pmax_
     *, g_pmax_, 6), g_g_v(ldg_g_v, ldg_v)
        integer g_ehfid
        save g_g_pass
        save g_g_xcomp4, g_g_ycomp4, g_g_zcomp4, g_g_x_normal2, g_g_y_no
     *rmal2, g_g_z_normal2, g_g_n, g_g_d2_w, g_g_d, g_g_thooh
        save g_g_zcomp1, g_g_xcomp2, g_g_ycomp2, g_g_zcomp2, g_g_x_norma
     *l1, g_g_y_normal1, g_g_z_normal1, g_g_xcomp3, g_g_ycomp3, g_g_zcom
     *p3
        save g_g_ro1o2, g_g_ro2h2, g_g_n1, g_g_d1, g_g_ah1oo, g_g_n2, g_
     *g_d2, g_g_aooh2, g_g_xcomp1, g_g_ycomp1
        save g_g_c11, g_g_c12, g_g_c13, g_g_c14, g_g_c15, g_g_d1_w, g_g_
     *rab, g_g_rbc, g_g_rcd, g_g_rh1o1
        save g_g_c1, g_g_c2, g_g_c3, g_g_c4, g_g_c5, g_g_c6, g_g_c7, g_g
     *_c8, g_g_c9, g_g_c10
        external g_g_vibpot

C        data rohlow, rohhigh /1.d0, 4.d0/
C        data roolow, roohigh, rhhlow /1.8d0, 4.d0, 0.7d0/

C
C  Calculate energy at the configuration space point
C  NOTE :: This is not necessarily a quadrature point
C
C Expecting input in cartesians
C
        data g_ehfid /0/
C
        if (g2p .gt. g2pmax) then
          print *, 'Parameter g2p is greater than g2pmax'
          stop
        endif
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        pi = 4.d0 * atan(1.d0)
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c1(g2i, g_i_) = g_g_x(g2i, g_i_, 1) + (-g_g_x(g2i,
     * g_i_, 4))
          enddo
          g_c1(g_i_) = -g_x(g_i_, 4) + g_x(g_i_, 1)
C--------
        enddo
        c1 = x(1) - x(4)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c2(g2i, g_i_) = g_g_x(g2i, g_i_, 2) + (-g_g_x(g2i,
     * g_i_, 5))
          enddo
          g_c2(g_i_) = -g_x(g_i_, 5) + g_x(g_i_, 2)
C--------
        enddo
        c2 = x(2) - x(5)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c3(g2i, g_i_) = g_g_x(g2i, g_i_, 3) + (-g_g_x(g2i,
     * g_i_, 6))
          enddo
          g_c3(g_i_) = -g_x(g_i_, 6) + g_x(g_i_, 3)
C--------
        enddo
        c3 = x(3) - x(6)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c4(g2i, g_i_) = g_g_x(g2i, g_i_, 4) + (-g_g_x(g2i,
     * g_i_, 7))
          enddo
          g_c4(g_i_) = -g_x(g_i_, 7) + g_x(g_i_, 4)
C--------
        enddo
        c4 = x(4) - x(7)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c5(g2i, g_i_) = g_g_x(g2i, g_i_, 5) + (-g_g_x(g2i,
     * g_i_, 8))
          enddo
          g_c5(g_i_) = -g_x(g_i_, 8) + g_x(g_i_, 5)
C--------
        enddo
        c5 = x(5) - x(8)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c6(g2i, g_i_) = g_g_x(g2i, g_i_, 6) + (-g_g_x(g2i,
     * g_i_, 9))
          enddo
          g_c6(g_i_) = -g_x(g_i_, 9) + g_x(g_i_, 6)
C--------
        enddo
        c6 = x(6) - x(9)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c7(g2i, g_i_) = g_g_x(g2i, g_i_, 7) + (-g_g_x(g2i,
     * g_i_, 10))
          enddo
          g_c7(g_i_) = -g_x(g_i_, 10) + g_x(g_i_, 7)
C--------
        enddo
        c7 = x(7) - x(10)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c8(g2i, g_i_) = g_g_x(g2i, g_i_, 8) + (-g_g_x(g2i,
     * g_i_, 11))
          enddo
          g_c8(g_i_) = -g_x(g_i_, 11) + g_x(g_i_, 8)
C--------
        enddo
        c8 = x(8) - x(11)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c9(g2i, g_i_) = g_g_x(g2i, g_i_, 9) + (-g_g_x(g2i,
     * g_i_, 12))
          enddo
          g_c9(g_i_) = -g_x(g_i_, 12) + g_x(g_i_, 9)
C--------
        enddo
        c9 = x(9) - x(12)
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c10(g2i, g_i_) = g_g_x(g2i, g_i_, 1) + (-g_g_x(g2i
     *, g_i_, 7))
          enddo
          g_c10(g_i_) = -g_x(g_i_, 7) + g_x(g_i_, 1)
C--------
        enddo
        c10 = x(1) - x(7)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c11(g2i, g_i_) = g_g_x(g2i, g_i_, 2) + (-g_g_x(g2i
     *, g_i_, 8))
          enddo
          g_c11(g_i_) = -g_x(g_i_, 8) + g_x(g_i_, 2)
C--------
        enddo
        c11 = x(2) - x(8)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c12(g2i, g_i_) = g_g_x(g2i, g_i_, 3) + (-g_g_x(g2i
     *, g_i_, 9))
          enddo
          g_c12(g_i_) = -g_x(g_i_, 9) + g_x(g_i_, 3)
C--------
        enddo
        c12 = x(3) - x(9)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c13(g2i, g_i_) = g_g_x(g2i, g_i_, 4) + (-g_g_x(g2i
     *, g_i_, 10))
          enddo
          g_c13(g_i_) = -g_x(g_i_, 10) + g_x(g_i_, 4)
C--------
        enddo
        c13 = x(4) - x(10)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c14(g2i, g_i_) = g_g_x(g2i, g_i_, 5) + (-g_g_x(g2i
     *, g_i_, 11))
          enddo
          g_c14(g_i_) = -g_x(g_i_, 11) + g_x(g_i_, 5)
C--------
        enddo
        c14 = x(5) - x(11)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_c15(g2i, g_i_) = g_g_x(g2i, g_i_, 6) + (-g_g_x(g2i
     *, g_i_, 12))
          enddo
          g_c15(g_i_) = -g_x(g_i_, 12) + g_x(g_i_, 6)
C--------
        enddo
        c15 = x(6) - x(12)
C--------
        c16 = x(1) - x(10)
        c17 = x(2) - x(11)
        c18 = x(3) - x(12)
C
C
C H1 -- O1 -- O2 -- H2
C   rH1O1 rO1O2 rO2H2
C 
C  angle H1O1O2 = AH1OO
C  angle O1O2H2 = AOOH2
C
C  TORSION HOOH = THOOH
C
        d2_v = c1 * c1
        d3_p = 2.0d0 * c1
        d4_v = c2 * c2
        d2_p = 2.0d0 * c2
        d7_v = c3 * c3
        d1_p = 2.0d0 * c3
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1_w(g2i, g_i_) = d3_p * g_g_c1(g2i, g_i_) + d2_p * 
     *g_g_c2(g2i, g_i_) + d1_p * g_g_c3(g2i, g_i_)
          enddo
          g_d1_w(g_i_) = d1_p * g_c3(g_i_) + d2_p * g_c2(g_i_) + d3_p * 
     *g_c1(g_i_)
C--------
        enddo
        d1_w = d2_v + d4_v + d7_v
        d2_v = sqrt(d1_w)
C
        d1_p = 1.0d0 / (2.0d0 * d2_v)
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_rab(g2i, g_i_) = d1_p * g_g_d1_w(g2i, g_i_)
          enddo
          g_rab(g_i_) = d1_p * g_d1_w(g_i_)
C--------
        enddo
        rab = d2_v
C--------
        d2_v = c4 * c4
        d3_p = 2.0d0 * c4
        d4_v = c5 * c5
        d2_p = 2.0d0 * c5
        d7_v = c6 * c6
        d1_p = 2.0d0 * c6
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1_w(g2i, g_i_) = d3_p * g_g_c4(g2i, g_i_) + d2_p * 
     *g_g_c5(g2i, g_i_) + d1_p * g_g_c6(g2i, g_i_)
          enddo
          g_d1_w(g_i_) = d1_p * g_c6(g_i_) + d2_p * g_c5(g_i_) + d3_p * 
     *g_c4(g_i_)
C--------
        enddo
        d1_w = d2_v + d4_v + d7_v
        d2_v = sqrt(d1_w)
C
        d1_p = 1.0d0 / (2.0d0 * d2_v)
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_rbc(g2i, g_i_) = d1_p * g_g_d1_w(g2i, g_i_)
          enddo
          g_rbc(g_i_) = d1_p * g_d1_w(g_i_)
C--------
        enddo
        rbc = d2_v
C--------
        d2_v = c7 * c7
        d3_p = 2.0d0 * c7
        d4_v = c8 * c8
        d2_p = 2.0d0 * c8
        d7_v = c9 * c9
        d1_p = 2.0d0 * c9
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1_w(g2i, g_i_) = d3_p * g_g_c7(g2i, g_i_) + d2_p * 
     *g_g_c8(g2i, g_i_) + d1_p * g_g_c9(g2i, g_i_)
          enddo
          g_d1_w(g_i_) = d1_p * g_c9(g_i_) + d2_p * g_c8(g_i_) + d3_p * 
     *g_c7(g_i_)
C--------
        enddo
        d1_w = d2_v + d4_v + d7_v
        d2_v = sqrt(d1_w)
C
        d1_p = 1.0d0 / (2.0d0 * d2_v)
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_rcd(g2i, g_i_) = d1_p * g_g_d1_w(g2i, g_i_)
          enddo
          g_rcd(g_i_) = d1_p * g_d1_w(g_i_)
C--------
        enddo
        rcd = d2_v
C--------
        rac = sqrt(c10 ** 2 + c11 ** 2 + c12 ** 2)
        rbd = sqrt(c13 ** 2 + c14 ** 2 + c15 ** 2)
        rad = sqrt(c16 ** 2 + c17 ** 2 + c18 ** 2)
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_rh1o1(g2i, g_i_) = g_g_rab(g2i, g_i_)
          enddo
          g_rh1o1(g_i_) = g_rab(g_i_)
C--------
        enddo
        rh1o1 = rab
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_ro1o2(g2i, g_i_) = g_g_rbc(g2i, g_i_)
          enddo
          g_ro1o2(g_i_) = g_rbc(g_i_)
C--------
        enddo
        ro1o2 = rbc
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_ro2h2(g2i, g_i_) = g_g_rcd(g2i, g_i_)
          enddo
          g_ro2h2(g_i_) = g_rcd(g_i_)
C--------
        enddo
        ro2h2 = rcd
C--------
C
C The potential can give weird values for unusual geometries
C replace with some high value in these unphysical regions and
C then exit
C
C
C      if((min(rab,rcd,rac,rbd).gt.rohhigh) .or.
C     !(min(rab,rcd,rac,rbd).lt.rohlow)  .or.
C     !(rbc.gt.roohigh) .or. (rbc.lt.roolow) .or.
C     !(rad.lt.rhhlow)) then
C      vdc=1000.d0
C      return
C      endif
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_n1(g2i, g_i_) = (-c1) * g_g_c4(g2i, g_i_) + (-c4) * 
     *g_g_c1(g2i, g_i_) + (-c2) * g_g_c5(g2i, g_i_) + (-c5) * g_g_c2
     *(g2i, g_i_) + (-c3) * g_g_c6(g2i, g_i_) + (-c6) * g_g_c3(g2i
     *, g_i_)
          enddo
          g_n1(g_i_) = (-c6) * g_c3(g_i_) + (-c3) * g_c6(g_i_) + (-c5) *
     * g_c2(g_i_) + (-c2) * g_c5(g_i_) + (-c4) * g_c1(g_i_) + (-c1) * g_
     *c4(g_i_)
C--------
        enddo
        n1 = (-c4) * c1 + (-c5) * c2 + (-c6) * c3
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1(g2i, g_i_) = rh1o1 * g_g_ro1o2(g2i, g_i_) + ro1o2
     * * g_g_rh1o1(g2i, g_i_)
          enddo
          g_d1(g_i_) = ro1o2 * g_rh1o1(g_i_) + rh1o1 * g_ro1o2(g_i_)
C--------
        enddo
        d1 = ro1o2 * rh1o1
C--------
        d3_v = n1 / d1
        d2_b = 1.0d0 / d1
        d3_b = (-d3_v) / d1
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1_w(g2i, g_i_) = d2_b * g_g_n1(g2i, g_i_) + d3_b * 
     *g_g_d1(g2i, g_i_)
          enddo
          g_d1_w(g_i_) = d3_b * g_d1(g_i_) + d2_b * g_n1(g_i_)
C--------
        enddo
        d1_w = d3_v
        d2_v = acos(d1_w)
C
        d1_p = (-1.0d0) / sqrt((1.0d0 - d1_w) * (1.0d0 + d1_w))
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_ah1oo(g2i, g_i_) = d1_p * g_g_d1_w(g2i, g_i_)
          enddo
          g_ah1oo(g_i_) = d1_p * g_d1_w(g_i_)
C--------
        enddo
        ah1oo = d2_v
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_n2(g2i, g_i_) = (-c4) * g_g_c7(g2i, g_i_) + (-c7) * 
     *g_g_c4(g2i, g_i_) + (-c5) * g_g_c8(g2i, g_i_) + (-c8) * g_g_c5
     *(g2i, g_i_) + (-c6) * g_g_c9(g2i, g_i_) + (-c9) * g_g_c6(g2i
     *, g_i_)
          enddo
          g_n2(g_i_) = (-c9) * g_c6(g_i_) + (-c6) * g_c9(g_i_) + (-c8) *
     * g_c5(g_i_) + (-c5) * g_c8(g_i_) + (-c7) * g_c4(g_i_) + (-c4) * g_
     *c7(g_i_)
C--------
        enddo
        n2 = -(c7 * c4 + c8 * c5 + c9 * c6)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d2(g2i, g_i_) = ro1o2 * g_g_ro2h2(g2i, g_i_) + ro2h2
     * * g_g_ro1o2(g2i, g_i_)
          enddo
          g_d2(g_i_) = ro2h2 * g_ro1o2(g_i_) + ro1o2 * g_ro2h2(g_i_)
C--------
        enddo
        d2 = ro2h2 * ro1o2
C--------
        d3_v = n2 / d2
        d2_b = 1.0d0 / d2
        d3_b = (-d3_v) / d2
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1_w(g2i, g_i_) = d2_b * g_g_n2(g2i, g_i_) + d3_b * 
     *g_g_d2(g2i, g_i_)
          enddo
          g_d1_w(g_i_) = d3_b * g_d2(g_i_) + d2_b * g_n2(g_i_)
C--------
        enddo
        d1_w = d3_v
        d2_v = acos(d1_w)
C
        d1_p = (-1.0d0) / sqrt((1.0d0 - d1_w) * (1.0d0 + d1_w))
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_aooh2(g2i, g_i_) = d1_p * g_g_d1_w(g2i, g_i_)
          enddo
          g_aooh2(g_i_) = d1_p * g_d1_w(g_i_)
C--------
        enddo
        aooh2 = d2_v
C--------
C
C Calculate torsion angle HOOH
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_xcomp1(g2i, g_i_) = g_g_x(g2i, g_i_, 4) + (-g_g_x(
     *g2i, g_i_, 1))
          enddo
          g_xcomp1(g_i_) = -g_x(g_i_, 1) + g_x(g_i_, 4)
C--------
        enddo
        xcomp1 = x(4) - x(1)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_ycomp1(g2i, g_i_) = g_g_x(g2i, g_i_, 5) + (-g_g_x(
     *g2i, g_i_, 2))
          enddo
          g_ycomp1(g_i_) = -g_x(g_i_, 2) + g_x(g_i_, 5)
C--------
        enddo
        ycomp1 = x(5) - x(2)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_zcomp1(g2i, g_i_) = g_g_x(g2i, g_i_, 6) + (-g_g_x(
     *g2i, g_i_, 3))
          enddo
          g_zcomp1(g_i_) = -g_x(g_i_, 3) + g_x(g_i_, 6)
C--------
        enddo
        zcomp1 = x(6) - x(3)
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_xcomp2(g2i, g_i_) = g_g_c10(g2i, g_i_)
          enddo
          g_xcomp2(g_i_) = g_c10(g_i_)
C--------
        enddo
        xcomp2 = c10
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_ycomp2(g2i, g_i_) = g_g_c11(g2i, g_i_)
          enddo
          g_ycomp2(g_i_) = g_c11(g_i_)
C--------
        enddo
        ycomp2 = c11
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_zcomp2(g2i, g_i_) = g_g_c12(g2i, g_i_)
          enddo
          g_zcomp2(g_i_) = g_c12(g_i_)
C--------
        enddo
        zcomp2 = c12
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_x_normal1(g2i, g_i_) = zcomp2 * g_g_ycomp1(g2i, g_i_
     *) + ycomp1 * g_g_zcomp2(g2i, g_i_) + (-ycomp2) * g_g_zcomp1(g2i
     *, g_i_) + (-zcomp1) * g_g_ycomp2(g2i, g_i_)
          enddo
          g_x_normal1(g_i_) = (-zcomp1) * g_ycomp2(g_i_) + (-ycomp2) * g
     *_zcomp1(g_i_) + ycomp1 * g_zcomp2(g_i_) + zcomp2 * g_ycomp1(g_i_)
C--------
        enddo
        x_normal1 = ycomp1 * zcomp2 - zcomp1 * ycomp2
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_y_normal1(g2i, g_i_) = xcomp2 * g_g_zcomp1(g2i, g_i_
     *) + zcomp1 * g_g_xcomp2(g2i, g_i_) + (-zcomp2) * g_g_xcomp1(g2i
     *, g_i_) + (-xcomp1) * g_g_zcomp2(g2i, g_i_)
          enddo
          g_y_normal1(g_i_) = (-xcomp1) * g_zcomp2(g_i_) + (-zcomp2) * g
     *_xcomp1(g_i_) + zcomp1 * g_xcomp2(g_i_) + xcomp2 * g_zcomp1(g_i_)
C--------
        enddo
        y_normal1 = zcomp1 * xcomp2 - xcomp1 * zcomp2
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_z_normal1(g2i, g_i_) = ycomp2 * g_g_xcomp1(g2i, g_i_
     *) + xcomp1 * g_g_ycomp2(g2i, g_i_) + (-xcomp2) * g_g_ycomp1(g2i
     *, g_i_) + (-ycomp1) * g_g_xcomp2(g2i, g_i_)
          enddo
          g_z_normal1(g_i_) = (-ycomp1) * g_xcomp2(g_i_) + (-xcomp2) * g
     *_ycomp1(g_i_) + xcomp1 * g_ycomp2(g_i_) + ycomp2 * g_xcomp1(g_i_)
C--------
        enddo
        z_normal1 = xcomp1 * ycomp2 - ycomp1 * xcomp2
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_xcomp3(g2i, g_i_) = g_g_x(g2i, g_i_, 7) + (-g_g_x(
     *g2i, g_i_, 4))
          enddo
          g_xcomp3(g_i_) = -g_x(g_i_, 4) + g_x(g_i_, 7)
C--------
        enddo
        xcomp3 = x(7) - x(4)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_ycomp3(g2i, g_i_) = g_g_x(g2i, g_i_, 8) + (-g_g_x(
     *g2i, g_i_, 5))
          enddo
          g_ycomp3(g_i_) = -g_x(g_i_, 5) + g_x(g_i_, 8)
C--------
        enddo
        ycomp3 = x(8) - x(5)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_zcomp3(g2i, g_i_) = g_g_x(g2i, g_i_, 9) + (-g_g_x(
     *g2i, g_i_, 6))
          enddo
          g_zcomp3(g_i_) = -g_x(g_i_, 6) + g_x(g_i_, 9)
C--------
        enddo
        zcomp3 = x(9) - x(6)
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_xcomp4(g2i, g_i_) = g_g_c13(g2i, g_i_)
          enddo
          g_xcomp4(g_i_) = g_c13(g_i_)
C--------
        enddo
        xcomp4 = c13
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_ycomp4(g2i, g_i_) = g_g_c14(g2i, g_i_)
          enddo
          g_ycomp4(g_i_) = g_c14(g_i_)
C--------
        enddo
        ycomp4 = c14
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_zcomp4(g2i, g_i_) = g_g_c15(g2i, g_i_)
          enddo
          g_zcomp4(g_i_) = g_c15(g_i_)
C--------
        enddo
        zcomp4 = c15
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_x_normal2(g2i, g_i_) = zcomp4 * g_g_ycomp3(g2i, g_i_
     *) + ycomp3 * g_g_zcomp4(g2i, g_i_) + (-ycomp4) * g_g_zcomp3(g2i
     *, g_i_) + (-zcomp3) * g_g_ycomp4(g2i, g_i_)
          enddo
          g_x_normal2(g_i_) = (-zcomp3) * g_ycomp4(g_i_) + (-ycomp4) * g
     *_zcomp3(g_i_) + ycomp3 * g_zcomp4(g_i_) + zcomp4 * g_ycomp3(g_i_)
C--------
        enddo
        x_normal2 = ycomp3 * zcomp4 - zcomp3 * ycomp4
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_y_normal2(g2i, g_i_) = xcomp4 * g_g_zcomp3(g2i, g_i_
     *) + zcomp3 * g_g_xcomp4(g2i, g_i_) + (-zcomp4) * g_g_xcomp3(g2i
     *, g_i_) + (-xcomp3) * g_g_zcomp4(g2i, g_i_)
          enddo
          g_y_normal2(g_i_) = (-xcomp3) * g_zcomp4(g_i_) + (-zcomp4) * g
     *_xcomp3(g_i_) + zcomp3 * g_xcomp4(g_i_) + xcomp4 * g_zcomp3(g_i_)
C--------
        enddo
        y_normal2 = zcomp3 * xcomp4 - xcomp3 * zcomp4
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_z_normal2(g2i, g_i_) = ycomp4 * g_g_xcomp3(g2i, g_i_
     *) + xcomp3 * g_g_ycomp4(g2i, g_i_) + (-xcomp4) * g_g_ycomp3(g2i
     *, g_i_) + (-ycomp3) * g_g_xcomp4(g2i, g_i_)
          enddo
          g_z_normal2(g_i_) = (-ycomp3) * g_xcomp4(g_i_) + (-xcomp4) * g
     *_ycomp3(g_i_) + xcomp3 * g_ycomp4(g_i_) + ycomp4 * g_xcomp3(g_i_)
C--------
        enddo
        z_normal2 = xcomp3 * ycomp4 - ycomp3 * xcomp4
C--------
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_n(g2i, g_i_) = x_normal2 * g_g_x_normal1(g2i, g_i_) 
     *+ x_normal1 * g_g_x_normal2(g2i, g_i_) + y_normal2 * g_g_y_norma
     *l1(g2i, g_i_) + y_normal1 * g_g_y_normal2(g2i, g_i_) + z_norma
     *l2 * g_g_z_normal1(g2i, g_i_) + z_normal1 * g_g_z_normal2(g2i,
     * g_i_)
          enddo
          g_n(g_i_) = z_normal1 * g_z_normal2(g_i_) + z_normal2 * g_z_no
     *rmal1(g_i_) + y_normal1 * g_y_normal2(g_i_) + y_normal2 * g_y_norm
     *al1(g_i_) + x_normal1 * g_x_normal2(g_i_) + x_normal2 * g_x_normal
     *1(g_i_)
C--------
        enddo
        n = x_normal1 * x_normal2 + y_normal1 * y_normal2 + z_normal1 * 
     *z_normal2
C--------
C
        d4_b = z_normal1 + z_normal1
        d7_b = y_normal1 + y_normal1
        d8_b = x_normal1 + x_normal1
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1_w(g2i, g_i_) = d8_b * g_g_x_normal1(g2i, g_i_) + 
     *d7_b * g_g_y_normal1(g2i, g_i_) + d4_b * g_g_z_normal1(g2i, g_
     *i_)
          enddo
          g_d1_w(g_i_) = d4_b * g_z_normal1(g_i_) + d7_b * g_y_normal1(g
     *_i_) + d8_b * g_x_normal1(g_i_)
C--------
        enddo
        d1_w = x_normal1 * x_normal1 + y_normal1 * y_normal1 + z_normal1
     * * z_normal1
        d4_b = z_normal2 + z_normal2
        d7_b = y_normal2 + y_normal2
        d8_b = x_normal2 + x_normal2
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d2_w(g2i, g_i_) = d8_b * g_g_x_normal2(g2i, g_i_) + 
     *d7_b * g_g_y_normal2(g2i, g_i_) + d4_b * g_g_z_normal2(g2i, g_
     *i_)
          enddo
          g_d2_w(g_i_) = d4_b * g_z_normal2(g_i_) + d7_b * g_y_normal2(g
     *_i_) + d8_b * g_x_normal2(g_i_)
C--------
        enddo
        d2_w = x_normal2 * x_normal2 + y_normal2 * y_normal2 + z_normal2
     * * z_normal2
        d2_v = sqrt(d1_w)
C
        d2_p = 1.0d0 / (2.0d0 * d2_v)
        d4_v = sqrt(d2_w)
C
        d1_p = 1.0d0 / (2.0d0 * d4_v)
        d4_b = d2_v * d1_p
        d5_b = d4_v * d2_p
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d(g2i, g_i_) = d5_b * g_g_d1_w(g2i, g_i_) + d4_b * g
     *_g_d2_w(g2i, g_i_)
          enddo
          g_d(g_i_) = d4_b * g_d2_w(g_i_) + d5_b * g_d1_w(g_i_)
C--------
        enddo
        d = d2_v * d4_v
C--------
C
C  We can occasionally get "NaNs" if we don't trap for special cases while calcu
Clating
C  the dihedral angle.  Acos wants an argument in [-1,1], but slight roundoff ca
Cn push us
C  outside this range.  Also if certain triatoms are collinear then the dihedral
C is not defined
C  and we set these cases to 0 degrees
C
C      if(d.eq.0.d0)then 
C       thooh=0.d0
C      else
C       argnd=n/d
C      if(argnd.ge.1.d0)then
C       thooh=0.d0
C      elseif(argnd.le.-1.d0)then
C       thooh=pi
C      else
        d3_v = n / d
        d2_b = 1.0d0 / d
        d3_b = (-d3_v) / d
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_d1_w(g2i, g_i_) = d2_b * g_g_n(g2i, g_i_) + d3_b * g
     *_g_d(g2i, g_i_)
          enddo
          g_d1_w(g_i_) = d3_b * g_d(g_i_) + d2_b * g_n(g_i_)
C--------
        enddo
        d1_w = d3_v
        d2_v = acos(d1_w)
C
        d1_p = (-1.0d0) / sqrt((1.0d0 - d1_w) * (1.0d0 + d1_w))
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_thooh(g2i, g_i_) = d1_p * g_g_d1_w(g2i, g_i_)
          enddo
          g_thooh(g_i_) = d1_p * g_d1_w(g_i_)
C--------
        enddo
        thooh = d2_v
C--------
C      endif
C      endif
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_pass(g2i, g_i_, 1) = g_g_rh1o1(g2i, g_i_)
          enddo
          g_pass(g_i_, 1) = g_rh1o1(g_i_)
C--------
        enddo
        pass(1) = rh1o1
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_pass(g2i, g_i_, 2) = g_g_ro1o2(g2i, g_i_)
          enddo
          g_pass(g_i_, 2) = g_ro1o2(g_i_)
C--------
        enddo
        pass(2) = ro1o2
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_pass(g2i, g_i_, 3) = g_g_ro2h2(g2i, g_i_)
          enddo
          g_pass(g_i_, 3) = g_ro2h2(g_i_)
C--------
        enddo
        pass(3) = ro2h2
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_pass(g2i, g_i_, 4) = g_g_ah1oo(g2i, g_i_)
          enddo
          g_pass(g_i_, 4) = g_ah1oo(g_i_)
C--------
        enddo
        pass(4) = ah1oo
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_pass(g2i, g_i_, 5) = g_g_aooh2(g2i, g_i_)
          enddo
          g_pass(g_i_, 5) = g_aooh2(g_i_)
C--------
        enddo
        pass(5) = aooh2
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_pass(g2i, g_i_, 6) = g_g_thooh(g2i, g_i_)
          enddo
          g_pass(g_i_, 6) = g_thooh(g_i_)
C--------
        enddo
        pass(6) = thooh
C--------
C
C
C Call Koput, Carter, and Handy PES
C
C
        call g_g_vibpot(g2p, g_p_, pass, g_pass, g_g_pass, g2pmax, g
     *_pmax_, v, g_v, g_g_v, ldg_g_v, ldg_v)
C
        return
C
      end
C
C**************************************************************************
C
      subroutine g_g_vibpot(g2p, g_p_, coord, g_coord, g_g_coord, ldg_
     *g_coord, ldg_coord, v, g_v, g_g_v, ldg_g_v, ldg_v)
C
      IMPLICIT NONE

      DOUBLE PRECISION V, PI

      DOUBLE PRECISION COORD(6), Q1P(0:4), Q2P(0:4), Q3P(0:4), Q4P(0:4)
      DOUBLE PRECISION Q5P(0:4), Q6P(0:4)
      DOUBLE PRECISION IND1(152), IND2(152), IND3(152), IND4(152)
      DOUBLE PRECISION IND5(152), IND6(152), COEFF(152)

      DOUBLE PRECISION RM_OO, RM_OH, ANGLE_OOH, R1, R2, R3
      DOUBLE PRECISION A1, A2, Q1, Q2, Q3, Q4, Q5, Q6

      INTEGER I

      DOUBLE PRECISION ZOE

        integer g_pmax_
        parameter (g_pmax_ = 12)
        integer g_i_, g_p_, ldg_coord, ldg_v
        double precision d14_b, d13_b, d7_v, d11_b, d10_b, d9_b, d8_b, d
     *7_b, d2_v, d3_v
        double precision d6_b, d2_b, d3_b, d1_w, d1_p, d11_v, d5_v, d9_v
     *, g_r1(g_pmax_), g_coord(ldg_coord, 6)
        double precision g_r2(g_pmax_), g_r3(g_pmax_), g_a1(g_pmax_), g_
     *a2(g_pmax_), g_q6(g_pmax_), g_q3(g_pmax_), g_q1(g_pmax_), g_q2(g_p
     *max_), g_q4(g_pmax_), g_q5(g_pmax_)
        double precision g_q1p(g_pmax_, 0:4), g_q2p(g_pmax_, 0:4), g_q3p
     *(g_pmax_, 0:4), g_q4p(g_pmax_, 0:4), g_q5p(g_pmax_, 0:4), g_q6p(g_
     *pmax_, 0:4), g_d1_w(g_pmax_), g_v(ldg_v)
        integer g_ehfid
        save g_q5, g_q1p, g_q2p, g_q3p, g_q4p, g_q5p, g_q6p, g_d1_w
        save g_r1, g_r2, g_r3, g_a1, g_a2, g_q6, g_q3, g_q1, g_q2, g_q4
        intrinsic dble
        integer g__pmax_
        parameter (g__pmax_ = 12)
        integer g2i, g2p, ldg_g_coord, ldg_g_v
        double precision g_g_r1(g__pmax_, g_pmax_), g_g_coord(ldg_g_coor
     *d, ldg_coord, 6), g_g_r2(g__pmax_, g_pmax_), g_g_r3(g__pmax_, g_pm
     *ax_), g_g_a1(g__pmax_, g_pmax_), g_g_a2(g__pmax_, g_pmax_), g_g_q6
     *(g__pmax_, g_pmax_), g_g_q3(g__pmax_, g_pmax_), g_g_q1(g__pmax_, g
     *_pmax_), g_g_q2(g__pmax_, g_pmax_)
        double precision g_g_q4(g__pmax_, g_pmax_), g_g_q5(g__pmax_, g_p
     *max_), g_g_q1p(g__pmax_, g_pmax_, 0:4), g_g_q2p(g__pmax_, g_pmax_,
     * 0:4), g_g_q3p(g__pmax_, g_pmax_, 0:4), g_g_q4p(g__pmax_, g_pmax_,
     * 0:4), g_g_q5p(g__pmax_, g_pmax_, 0:4), g_g_q6p(g__pmax_, g_pmax_,
     * 0:4), g_g_d1_w(g__pmax_, g_pmax_), g_g_v(ldg_g_v, ldg_v)
        integer g_ehfid
       save g_g_q5, g_g_q1p, g_g_q2p, g_g_q3p, g_g_q4p, g_g_q5p, g_g_q6
     *p, g_g_d1_w
        save g_g_r1, g_g_r2, g_g_r3, g_g_a1, g_g_a2, g_g_q6, g_g_q3, g_g
     *_q1, g_g_q2, g_g_q4

        data zoe /3.995139982058d-3/
C
        data ind1 /0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0,
     * 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 2, 1, 0, 2, 0, 1, 0,
     * 2, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 4, 0, 0, 0, 2, 0, 0,
     * 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 3, 1, 0, 3, 0, 0, 0, 1, 1,
     * 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0, 0, 1, 1, 2, 0, 1, 0, 1, 0, 0, 1,
     * 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 3, 0, 0, 0, 2,
     * 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0,
     * 0, 0/
C
        data ind2 /0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1,
     * 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 2, 1, 0, 1, 0, 2, 0, 1,
     * 0, 2, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 4, 0, 0, 0, 2, 0,
     * 0, 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 3, 1, 0, 1, 0, 3, 0, 0, 1, 0,
     * 1, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0, 1, 1, 0, 2, 0, 1, 0, 1, 0, 0,
     * 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0, 0,
     * 2, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1,
     * 0, 0/
C
        data ind3 /0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
     * 0, 3, 0, 0, 0, 0, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 4, 0, 0, 0, 0, 2, 2, 2,
     * 2, 0, 0, 0, 3, 3, 3, 3, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2,
     * 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
     * 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 3, 0, 0, 0, 0, 1,
     * 1, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0,
     * 0, 0/
C
        data ind4 /0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1,
     * 1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 2,
     * 0, 1, 1, 2, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 4, 0, 0, 0, 2,
     * 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 3, 0, 1, 0, 1, 3, 0, 1,
     * 0, 0, 1, 1, 2, 0, 1, 0, 0, 1, 1, 2, 2, 0, 1, 1, 1, 2, 2, 1, 0, 0,
     * 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0,
     * 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0,
     * 1, 0/
C
        data ind5 /0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0,
     * 1, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 1, 2, 0,
     * 1, 0, 2, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 4, 0, 0, 0,
     * 2, 0, 2, 2, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 3, 0, 1, 3, 1, 0, 0,
     * 1, 1, 0, 1, 0, 2, 0, 1, 1, 0, 2, 1, 0, 2, 1, 1, 2, 1, 1, 2, 0, 0,
     * 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 3, 0,
     * 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,
     * 0, 1/
C
        data ind6 /1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
     * 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     * 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,
     * 3, 3/
C
        data coeff /0.00482409d0, 0.00325127d0, 0.00026630d0, 0.00004135
     *d0, 1.08154998d0, 0.86097723d0, 0.86097723d0, 0.11016112d0, 0.1101
     *6112d0, -0.03637740d0, -0.03637740d0, 0.17491854d0, 0.17491854d0, 
     *0.00057054d0, -0.00137967d0, -0.00137967d0, 0.00062152d0, 0.000621
     *52d0, 0.01375726d0, -1.15691421d0, -0.25495918d0, -0.25495918d0, -
     *0.02272830d0, -0.02272830d0, -0.18381415d0, -0.18381415d0, -0.3462
     *7237d0, -0.34627237d0, 0.13974588d0, 0.13974588d0, -0.27290455d0, 
     *-0.27290455d0, -0.00674721d0, -0.00674721d0, -0.02179545d0, -0.021
     *79545d0, -0.02125643d0, -0.02125643d0, -0.00491968d0, -0.00491968d
     *0, 0.00233773d0, 0.00233773d0, -0.00050066d0, -0.00050066d0, 0.018
     *17536d0, -0.04666009d0, -0.04666009d0, -0.02424748d0, -0.02424748d
     *0, -0.01727148d0, -0.00420506d0, -0.00420506d0, -0.00647944d0, -0.
     *00647944d0, -1.06749007d0, -0.35741007d0, -0.35741007d0, -0.007968
     *36d0, -0.00796836d0, -0.42556742d0, -0.42556742d0, 0.06278896d0, 0
     *.06278896d0, -0.04010419d0, -0.04010419d0, -0.00993912d0, 0.475628
     *94d0, 0.47562894d0, -0.40830627d0, -0.40830627d0, 0.22073222d0, 0.
     *22073222d0, 0.07828212d0, 0.07828212d0, -0.02954687d0, -0.02954687
     *d0, 0.03057888d0, 0.03057888d0, -0.06363999d0, -0.06363999d0, -0.0
     *0373964d0, -0.00373964d0, -0.04114668d0, 0.11249614d0, 0.11249614d
     *0, 0.02616679d0, 0.02616679d0, -0.07824425d0, 0.04266205d0, 0.0426
     *6205d0, -0.07420432d0, -0.07420432d0, -0.08251268d0, -0.08251268d0
     *, 0.00270940d0, 0.00270940d0, 0.00199953d0, 0.00199953d0, -0.01292
     *325d0, -0.01292325d0, -0.02074323d0, -0.02074323d0, -0.00789732d0,
     * -0.00789732d0, -0.01435326d0, -0.00180710d0, -0.00180710d0, -0.01
     *135671d0, -0.01135671d0, 0.00020655d0, -0.00492533d0, -0.00492533d
     *0, 0.00270990d0, 0.00270990d0, 0.00376086d0, 0.00376086d0, 0.00044
     *732d0, 0.00044732d0, 0.00569979d0, -0.00244774d0, -0.00244774d0, -
     *0.02065564d0, 0.05249331d0, -0.02490299d0, -0.02490299d0, 0.003914
     *60d0, 0.00391460d0, 0.08893744d0, 0.08893744d0, -0.01051618d0, 0.0
     *0120479d0, 0.00120479d0, -0.00111888d0, -0.00111888d0, 0.00884757d
     *0, 0.00416289d0, 0.00416289d0, 0.00126763d0, 0.00126763d0, -0.0070
     *6563d0, -0.00706563d0, -0.00840146d0, -0.00840146d0, -0.00139219d0
     *, 0.00801673d0, 0.00801673d0, 0.00463860d0, -0.00096051d0, 0.00019
     *906d0, 0.00019906d0, -0.00057576d0, -0.00057576d0/
C
C      data rm_oo, rm_oh, ANGLE_OOH/1.456199d0, 0.962755d0, 100.9059d0/
C
C   convert offset parameters to bohr and radians
C
        data g_ehfid /0/
C
        if (g2p .gt. g__pmax_) then
          print *, 'Parameter g2p is greater than g__pmax_'
          stop
        endif
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        rm_oo = 1.456199d0
        rm_oh = 0.962755d0
        angle_ooh = 100.9059d0
C
        pi = 4.d0 * atan(1.d0)
C        rm_oo = rm_oo / 0.529177d0
C        rm_oh = rm_oh / 0.529177d0
        angle_ooh = angle_ooh * pi / 180.d0
C
C
C Potential energy surface for H2O2
C Jacek Koput, Stuart Carter, and Nicholas Handy, J. Phys. Chem. A
C 1998, volume 102, pages 6325-6330
C
C V is the potential energy in hartrees 
C minimum configuration is at R(OH)=0.96265, R(OO)=1.45248, 
C theta(OOH)=99.909, and dihed angle=112.456 degrees
C
C      R1 = COORD(1)                ! radius from H1 to O1 (in bohr)
C      R2 = COORD(2)                ! radius from O1 to O2 (in bohr) 
C      R3 = COORD(3)                ! radius from O2 to H2 (in bohr)
C      A1 = COORD(4)                ! angle between H1-O1-O2 (in radians)
C      A2 = COORD(5)                ! angle between O1-O2-H2 (in radians)
C      Q6 = COORD(6)                ! HOOH torsion angle (in radians)
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_r1(g2i, g_i_) = g_g_coord(g2i, g_i_, 1)
          enddo
          g_r1(g_i_) = g_coord(g_i_, 1)
C--------
        enddo
        r1 = coord(1)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_r2(g2i, g_i_) = g_g_coord(g2i, g_i_, 2)
          enddo
          g_r2(g_i_) = g_coord(g_i_, 2)
C--------
        enddo
        r2 = coord(2)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_r3(g2i, g_i_) = g_g_coord(g2i, g_i_, 3)
          enddo
          g_r3(g_i_) = g_coord(g_i_, 3)
C--------
        enddo
        r3 = coord(3)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_a1(g2i, g_i_) = g_g_coord(g2i, g_i_, 4)
          enddo
          g_a1(g_i_) = g_coord(g_i_, 4)
C--------
        enddo
        a1 = coord(4)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_a2(g2i, g_i_) = g_g_coord(g2i, g_i_, 5)
          enddo
          g_a2(g_i_) = g_coord(g_i_, 5)
C--------
        enddo
        a2 = coord(5)
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_q6(g2i, g_i_) = g_g_coord(g2i, g_i_, 6)
          enddo
          g_q6(g_i_) = g_coord(g_i_, 6)
C--------
        enddo
        q6 = coord(6)
C--------
C
C Q1,Q2, and Q3 represent the stretching modes
C Q4 and Q5  are the bending modes
C Q6 is the torsional mode 
C Q1, Q2, Q3 are dimensionless, Q4, Q5, and Q6 are in radians
C
        d3_v = (r2 - rm_oo) / r2
        d3_b = (-d3_v) / r2 + 1.0d0 / r2
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_q3(g2i, g_i_) = d3_b * g_g_r2(g2i, g_i_)
          enddo
          g_q3(g_i_) = d3_b * g_r2(g_i_)
C--------
        enddo
        q3 = d3_v
C--------
        d3_v = (r1 - rm_oh) / r1
        d3_b = (-d3_v) / r1 + 1.0d0 / r1
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_q1(g2i, g_i_) = d3_b * g_g_r1(g2i, g_i_)
          enddo
          g_q1(g_i_) = d3_b * g_r1(g_i_)
C--------
        enddo
        q1 = d3_v
C--------
        d3_v = (r3 - rm_oh) / r3
        d3_b = (-d3_v) / r3 + 1.0d0 / r3
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_q2(g2i, g_i_) = d3_b * g_g_r3(g2i, g_i_)
          enddo
          g_q2(g_i_) = d3_b * g_r3(g_i_)
C--------
        enddo
        q2 = d3_v
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_q4(g2i, g_i_) = g_g_a1(g2i, g_i_)
          enddo
          g_q4(g_i_) = g_a1(g_i_)
C--------
        enddo
        q4 = a1 - angle_ooh
C--------
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_q5(g2i, g_i_) = g_g_a2(g2i, g_i_)
          enddo
          g_q5(g_i_) = g_a2(g_i_)
C--------
        enddo
        q5 = a2 - angle_ooh
C--------
C
C
C
        do i = 0, 4
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q1p(g2i, g_i_, i) = 0.0d0
            enddo
            g_q1p(g_i_, i) = 0.0d0
C--------
          enddo
          q1p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q2p(g2i, g_i_, i) = 0.0d0
            enddo
            g_q2p(g_i_, i) = 0.0d0
C--------
          enddo
          q2p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q3p(g2i, g_i_, i) = 0.0d0
            enddo
            g_q3p(g_i_, i) = 0.0d0
C--------
          enddo
          q3p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q4p(g2i, g_i_, i) = 0.0d0
            enddo
            g_q4p(g_i_, i) = 0.0d0
C--------
          enddo
          q4p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q5p(g2i, g_i_, i) = 0.0d0
            enddo
            g_q5p(g_i_, i) = 0.0d0
C--------
          enddo
          q5p(i) = 1.d0
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q6p(g2i, g_i_, i) = 0.0d0
            enddo
            g_q6p(g_i_, i) = 0.0d0
C--------
          enddo
          q6p(i) = 1.d0
C--------
        enddo
C
C
        do i = 1, 4
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q1p(g2i, g_i_, i) = q1 * g_g_q1p(g2i, g_i_, i - 1)
     * + q1p(i - 1) * g_g_q1(g2i, g_i_)
            enddo
            g_q1p(g_i_, i) = q1p(i - 1) * g_q1(g_i_) + q1 * g_q1p(g_i_, 
     *i - 1)
C--------
          enddo
          q1p(i) = q1p(i - 1) * q1
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q2p(g2i, g_i_, i) = q2 * g_g_q2p(g2i, g_i_, i - 1)
     * + q2p(i - 1) * g_g_q2(g2i, g_i_)
            enddo
            g_q2p(g_i_, i) = q2p(i - 1) * g_q2(g_i_) + q2 * g_q2p(g_i_, 
     *i - 1)
C--------
          enddo
          q2p(i) = q2p(i - 1) * q2
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q3p(g2i, g_i_, i) = q3 * g_g_q3p(g2i, g_i_, i - 1)
     * + q3p(i - 1) * g_g_q3(g2i, g_i_)
            enddo
            g_q3p(g_i_, i) = q3p(i - 1) * g_q3(g_i_) + q3 * g_q3p(g_i_, 
     *i - 1)
C--------
          enddo
          q3p(i) = q3p(i - 1) * q3
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q4p(g2i, g_i_, i) = q4 * g_g_q4p(g2i, g_i_, i - 1)
     * + q4p(i - 1) * g_g_q4(g2i, g_i_)
            enddo
            g_q4p(g_i_, i) = q4p(i - 1) * g_q4(g_i_) + q4 * g_q4p(g_i_, 
     *i - 1)
C--------
          enddo
          q4p(i) = q4p(i - 1) * q4
C--------
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q5p(g2i, g_i_, i) = q5 * g_g_q5p(g2i, g_i_, i - 1)
     * + q5p(i - 1) * g_g_q5(g2i, g_i_)
            enddo
            g_q5p(g_i_, i) = q5p(i - 1) * g_q5(g_i_) + q5 * g_q5p(g_i_, 
     *i - 1)
C--------
          enddo
          q5p(i) = q5p(i - 1) * q5
C--------
          d2_b = dble(i)
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_d1_w(g2i, g_i_) = d2_b * g_g_q6(g2i, g_i_)
            enddo
            g_d1_w(g_i_) = d2_b * g_q6(g_i_)
C--------
          enddo
          d1_w = dble(i) * q6
          d2_v = cos(d1_w)
          d1_p = -sin(d1_w)
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_q6p(g2i, g_i_, i) = d1_p * g_g_d1_w(g2i, g_i_)
            enddo
            g_q6p(g_i_, i) = d1_p * g_d1_w(g_i_)
C--------
          enddo
          q6p(i) = d2_v
C--------
        enddo
C
        do g_i_ = 1, g_p_
          do g2i = 1, g2p
            g_g_v(g2i, g_i_) = 0.0d0
          enddo
          g_v(g_i_) = 0.0d0
C--------
        enddo
        v = zoe
C--------
C
        do i = 1, 152
          d3_v = coeff(i) * q1p(ind1(i))
          d5_v = d3_v * q2p(ind2(i))
          d7_v = d5_v * q3p(ind3(i))
          d9_v = d7_v * q4p(ind4(i))
          d11_v = d9_v * q5p(ind5(i))
          d6_b = q6p(ind6(i)) * q5p(ind5(i))
          d7_b = q6p(ind6(i)) * d9_v
          d8_b = d6_b * q4p(ind4(i))
          d9_b = d6_b * d7_v
          d10_b = d8_b * q3p(ind3(i))
          d11_b = d8_b * d5_v
          d13_b = d10_b * d3_v
          d14_b = d10_b * q2p(ind2(i)) * coeff(i)
          do g_i_ = 1, g_p_
            do g2i = 1, g2p
              g_g_v(g2i, g_i_) = g_g_v(g2i, g_i_) + d14_b * g_g_q1p(
     *g2i, g_i_, ind1(i)) + d13_b * g_g_q2p(g2i, g_i_, ind2(i)) + d1
     *1_b * g_g_q3p(g2i, g_i_, ind3(i)) + d9_b * g_g_q4p(g2i, g_i_, 
     *ind4(i)) + d7_b * g_g_q5p(g2i, g_i_, ind5(i)) + d11_v * g_g_q6p(
     *g2i, g_i_, ind6(i))
            enddo
            g_v(g_i_) = d11_v * g_q6p(g_i_, ind6(i)) + d7_b * g_q5p(g_i_
     *, ind5(i)) + d9_b * g_q4p(g_i_, ind4(i)) + d11_b * g_q3p(g_i_, ind
     *3(i)) + d13_b * g_q2p(g_i_, ind2(i)) + d14_b * g_q1p(g_i_, ind1(i)
     *) + g_v(g_i_)
C--------
          enddo
          v = v + d11_v * q6p(ind6(i))
C--------
        enddo
C
C
        return
C
      end
C
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
      PARAMETER (N3TMMN = 12)
C
C  CHECK THE NUMBER OF CARTESIAN COORDINATES SET BY THE CALLING PROGRAM
C
      WRITE (6, 1300)
      IF (N3TM .LT. N3TMMN) THEN
          WRITE (6, 6000) N3TM, N3TMMN
          STOP 'SETUP 1'
      ENDIF
C
      RETURN
C
1300  FORMAT(/,2X,T5,'SETUP has been called for the H2O2 ',
     *               'potential energy surface')
6000  FORMAT(/,2X,T5,'Warning: N3TM is set equal to ',I3,
     *                  ' but this potential routine',
     *          /,2X,T14,'requires N3TM be greater than or ',
     *                   'equal to ',I3,/)
C
      END


C
      SUBROUTINE chain3(X, DX, N3TM)
c     use potcm3, only : r, energy,dedr
      use potcm3
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /POTCM/ R(3), ENERGY, DEDR(3)
      DIMENSION X(N3TM), DX(N3TM), Y(3)
C
C   This subprogram uses the chain rule to calculate the derivatives of the
C   energy with respect to the cartesian coordinates from the derivatives
C   with respect to the internal coordinates for a three-body system.
C   The integer, N3TM, passed by the calling program must be greater than 
C   or equal to 9.
C   The convention assumed in this program is as follows:
C                  R(1) = R(A-B)
C                  R(2) = R(B-C)
C                  R(3) = R(C-A)
C                  X(1) - X(3) : X, Y, Z for atom A
C                  X(4) - X(6) : X, Y, Z for atom B
C                  X(7) - X(9) : X, Y, Z for atom C
C
         DO 10 I = 1, 3
               Y(I) = DEDR(I)/R(I)
10       CONTINUE
C
         DX(1) = (X(1)-X(4))*Y(1)-(X(7)-X(1))*Y(3)
         DX(2) = (X(2)-X(5))*Y(1)-(X(8)-X(2))*Y(3)
         DX(3) = (X(3)-X(6))*Y(1)-(X(9)-X(3))*Y(3)
         DX(4) = -(X(1)-X(4))*Y(1)+(X(4)-X(7))*Y(2)
         DX(5) = -(X(2)-X(5))*Y(1)+(X(5)-X(8))*Y(2)
         DX(6) = -(X(3)-X(6))*Y(1)+(X(6)-X(9))*Y(2)
         DX(7) = -(X(4)-X(7))*Y(2)+(X(7)-X(1))*Y(3)
         DX(8) = -(X(5)-X(8))*Y(2)+(X(8)-X(2))*Y(3)
         DX(9) = -(X(6)-X(9))*Y(2)+(X(9)-X(3))*Y(3)
C
         RETURN
         END subroutine chain3

C
      SUBROUTINE chain4(X, DX, N3TM)
      use potcm4 
C
C   This subprogram uses the chain rule to calculate the derivatives of
C   the energy with respect to the cartesian coordinates from the derivatives
C   with respect to the internal coordinates for a four-body system, ABCD.
C   The integer N3TM, passed by the calling routine, must be greater than or 
C   equal to 12.
C   The convention assumed in this subprogram is as follows:
C                  R(1) : R(A-B)
C                  R(2) : R(A-C)
C                  R(3) : R(A-D)
C                  R(4) : R(B-C)
C                  R(5) : R(B-D)
C                  R(6) : R(C-D)
C                  X(1)  - X(3)  : X, Y, Z for atom A
C                  X(4)  - X(6)  : X, Y, Z for atom B
C                  X(7)  - X(9)  : X, Y, Z for atom C
C                  X(10) - X(12) : X, Y, Z for atom D
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        COMMON /POTCM/ R(6), ENERGY, DEDR(6)
         integer, intent(in) :: N3TM
         real(8), intent(in) :: X(N3TM)
         real(8) :: DX(N3TM), Y(6)
C
         DO 10 I = 1, 6
c              Y(I) = DEDR(I)/R(I)
           Y(I) = DVDR(I)/R(I)
10       CONTINUE
C
         DX(1) = (X(1)-X(4))*Y(1)+(X(1)-X(7))*Y(2)+(X(1)-X(10))*Y(3)
         DX(2) = (X(2)-X(5))*Y(1)+(X(2)-X(8))*Y(2)+(X(2)-X(11))*Y(3)
         DX(3) = (X(3)-X(6))*Y(1)+(X(3)-X(9))*Y(2)+(X(3)-X(12))*Y(3)
         DX(4) = (X(4)-X(1))*Y(1)+(X(4)-X(7))*Y(4)+(X(4)-X(10))*Y(5)
         DX(5) = (X(5)-X(2))*Y(1)+(X(5)-X(8))*Y(4)+(X(5)-X(11))*Y(5)
         DX(6) = (X(6)-X(3))*Y(1)+(X(6)-X(9))*Y(4)+(X(6)-X(12))*Y(5)
         DX(7) = (X(7)-X(1))*Y(2)+(X(7)-X(4))*Y(4)+(X(7)-X(10))*Y(6)
         DX(8) = (X(8)-X(2))*Y(2)+(X(8)-X(5))*Y(4)+(X(8)-X(11))*Y(6)
         DX(9) = (X(9)-X(3))*Y(2)+(X(9)-X(6))*Y(4)+(X(9)-X(12))*Y(6)
         DX(10) = (X(10)-X(1))*Y(3)+(X(10)-X(4))*Y(5)+(X(10)-X(7))*Y(6)
         DX(11) = (X(11)-X(2))*Y(3)+(X(11)-X(5))*Y(5)+(X(11)-X(8))*Y(6)
         DX(12) = (X(12)-X(3))*Y(3)+(X(12)-X(6))*Y(5)+(X(12)-X(9))*Y(6)
C
         RETURN
         END subroutine chain4
C*****

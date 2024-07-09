C
      SUBROUTINE coord3(X, N3TM)
      use potcm3, only : r, energy,dedr
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     COMMON /POTCM/ R(3), ENERGY, DEDR(3)
      DIMENSION X(N3TM)
C
C   This subprogram transforms the coordinates of a three-body system, ABC,
C   from atomic cartesians to interatomic distances.  
C   The integer, N3TM, passed by the calling routine must be greater than or
C   equal to 9.  
C   The convention assumed in this subprogram is as follows:
C                  X(1) - X(3) : X, Y, Z of atom A
C                  X(4) - X(6) : X, Y, Z of atom B
C                  X(7) - X(9) : X, Y, Z of atom C
C                  R(1) = R(A-B)
C                  R(2) = R(B-C)
C                  R(3) = R(C-A)
C
         M = 0
         K = -2
         L = 1
         DO 10 J = 1, 3
               M = M+1
               K = K+3
               L = L+3
               IF (J.EQ.3) L = 1
               R(M) = SQRT((X(K)-X(L))**2 + (X(K+1)-X(L+1))**2 +
     *                     (X(K+2)-X(L+2))**2)
10       CONTINUE
C
         RETURN
         END subroutine coord3
C*****

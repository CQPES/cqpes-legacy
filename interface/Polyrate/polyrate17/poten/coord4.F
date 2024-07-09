C
         SUBROUTINE coord4(X, N3TM)
         use potcm4, only : r
C
C   This subprogram transforms the coordinates from atomic cartesians
C   to interatomic distances for a four-body system, ABCD.
C   The integer N3TM, passed by the calling routine, must be greater than 
C   or equal to 12.
C   The convention assumed in this subprogram is as follows:
C                  X(1)  - X(3)  : X, Y, Z of atom A
C                  X(4)  - X(6)  : X, Y, Z of atom B
C                  X(7)  - X(9)  : X, Y, Z of atom C
C                  X(10) - X(12) : X, Y, Z of atom D
C                  R(1) = R(A-B)
C                  R(2) = R(A-C)
C                  R(3) = R(A-D)
C                  R(4) = R(B-C)
C                  R(5) = R(B-D)
C                  R(6) = R(C-D)
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        COMMON /POTCM/ R(6), ENERGY, DEDR(6)
         real(8), intent(in) ::  X(N3TM)
C
         M = 0
         DO 20 I = 1, 3
               ITIM = 4-I
               K    = 3*(I-1)+1
               L    = K
            DO 10 J = 1, ITIM
                  M = M+1
                  L = L+3
                  R(M) = SQRT((X(K)-X(L))**2 + (X(K+1)-X(L+1))**2 +
     *                        (X(K+2)-X(L+2))**2)
10          CONTINUE
20       CONTINUE
C
         RETURN
         END subroutine coord4
C*****

C
         SUBROUTINE surf (V, X, DX, N3TM)
         use potcm3
C
C   This is a driver routine which can be used to link a calling program
C   that supplies the cartesian coordinates in an array X in the parameter
C   list with a potential energy surface routine that accepts the position
C   of the atoms in internal coordinates.  This routine will convert the 
C   cartesian coordinates to internal coordinates by calling the subprogram
C   COORD3, and call the potential energy function POT.  It will take the 
C   energy and the derivatives of the energy with respect to the internal 
C   coordinates from POT, convert the derivatives to derivatives with respect 
C   to cartesian coordinates by calling the subprogram CHAIN3, and pass this 
C   information to the calling routine through the parameters in the parameter
C   list.  All calculations are carried out in hartree atomic units. 
C
C   This subprogram assumes that the potential energy function that will
C   be called to obtain the energy and the derivatives of the energy
C   with respect to the internal coordinates has the following form:
C        1) the subprogram name is POT
C        2) the subprogram POT has no argument list, all information is 
C           passed to and from the potential energy subprogram through
C           the common block POTCM:
C                                   /POTCM/R(3), ENERGY, DEDR(3)
C           where R(3)    : array containing the internal coordinates in bohr
C                 ENERGY  : energy in hartree atomic units
C                 DEDR(3) : array containing the derivatives of the energy with
C                           respect to the internal coordinates.
C
C   The integer N3TM which is also passed is used to dimension the 
C   arrays X and DX and needs to be greater than or equal to 3 * NATOMS
C   where NATOMS is the number of atoms represented by this potential.
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c        COMMON /POTCM/ R(3),ENERGY,DER(3)
c        COMMON /POTCCM/ NSURF, NDER, NDUM(8)
         DIMENSION X(N3TM), DX(N3TM)
C
         CALL COORD3(X, N3TM)
         CALL POT
C
         V = ENERGY
C
         IF (NDER .EQ. 1) CALL CHAIN3(X, DX, N3TM)
c        CALL CHAIN3(X, DX, N3TM)
C
         RETURN
         END subroutine surf
C*****

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
C        COMMON /POTCM/ R(6), VTOT, DVDR(6)
C        COMMON /POTCCM/ NSURF, NDER, NDUM(8)
         integer, intent(in) :: N3TM
         real(8), intent(in) :: X(N3TM)
         real(8) ::  DX(N3TM)
C
C   Transform the cartesian coordinates to internal coordinates.
C
         CALL COORD4(X, N3TM)
C
C   Calculate the energy.
C
         CALL POTEN
         V = VTOT
C
C   Transform the derivatives of the energy with respect to the internal 
C   coordinates to derivatives with respect to the atomic cartesians.
C
         IF (NDER .EQ. 1) CALL CHAIN4(X, DX, N3TM)
C
         RETURN
         END subroutine surf
C*****

C
      SUBROUTINE prepot
      use potcm3
      use clhbr
C
C   System:           ClHBr
C   Functional form:  Extended LEPS (London-Erying-Polyani-Sato)
C
C   References for the potential parameters and the potential functional form:
C   Morse Parameters: D. J. Douglas, J. C. Polanyi, and J. J. Sloan
C                     Chem. Phys. 13, 15 (1976).
C   Sato Parameters:  V. K. Babamov, V. Lopez, and R. A. Marcus
C                     J. Chem. Phys. 78, 5621 (1983).
C   Functional form:  P. J. Kuntz, E. M. Nemth, J. C. Polanyi, S. D. Rosner,
C                     and C. E. Young 
C                     J. Chem. Phys. 44, 1168 (1966)
C
C   PREPOT must be called once before any calls to POT.
C   The potential parameters are included in DATA statements.
C   Coordinates, potential energy, and derivatives are passed 
C   through the common block POTCM:
C                  COMMON /POTCM/ R(3), ENERGY, DEDR(3)
C   The potential parameters are passed through the common blocks
C   SATOCM and LEPSCM.
C   All the information passed through the common blocks POTCM, SATOCM,
C   and LEPSCM are in hartree atomic units.  
C
C        This potential is written such that:
C                       R(1) = R(Cl-H)
C                       R(2) = R(H-Br)
C                       R(3) = R(Br-Cl)
C
C   The the flags that indicate what calculations should be carried out in 
C   the potential routine are passed through the common block POTCCM:
C                  /POTCCM/ NSURF, NDER, NDUM(8)
C   where:
C        NSURF - which electronic state should be used.
C                This option is not used for this potential as only the 
C                ground electronic state is available.
C        NDER  = 0 => no derivatives should be calculated
C        NDER  = 1 => calculate first derivatives
C        NDUM  - these 8 integer values can be used to flag options
C                within the potential; in this potential these options 
C                are not used.
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c        COMMON /POTCM/ R(3), ENERGY, DEDR(3)
c        COMMON /POTCCM/ NSURF, NDER, NDUM(8)
c        COMMON /SATOCM/ D(3), RE(3), BETA(3), Z(3) 
c        COMMON /LEPSCM/ ZPO(3), OP3Z(3), ZP3(3), TZP3(3), TOP3Z(3), 
c    *                   DO4Z(3), B(3)
         PARAMETER (CKCAL = 627.5095D0)
         PARAMETER (CANGS =   0.529177106D0)
         DIMENSION DEI(3), REI(3), BETAI(3), ZI(3)
C
C   Data statements for the potential parameters; the energy parameters are in
C   kcal/mol, and the lengths are in Angstroms.
C
         DATA DEI/ 106.4D0, 90.24D0, 52.09D0/
         DATA REI/ 1.275D0, 1.414D0, 2.136D0/
         DATA BETAI/1.867D0, 1.851D0, 1.923D0/
         DATA ZI/0.02D0, 0.02D0, 0.0D0/
C
C   Set the flags for the type of calculations to be carried out by the 
C   potential.
C        
         NDER = 1
         WRITE (6, 100) DEI, REI, BETAI, ZI
C
100   FORMAT (/, 2X, T5, 'PREPOT has been called for Cl + HBr',
     *        /, 2X, T5, 'Extended LEPS function',
     *        //, 2X, T5, 'Potential energy surface parameters:',
     *        /, 2X, T5, 'Bond', T47, 'Cl-H', T58, 'H-Br', T69, 'Br-Cl',
     *        /, 2X, T5, 'Dissociation energies (kcal/mol):', 
     *        T44, F10.5, T55, F10.5, T66, F10.5,
     *        /, 2X, T5, 'Equilibrium bond lengths (Angstroms):', 
     *        T44, F10.5, T55, F10.5, T66, F10.5,
     *        /, 2X, T5, 'Morse beta parameters (Angstroms**-1):', 
     *        T44, F10.5, T55, F10.5, T66, F10.5,
     *        /, 2X, T5, 'Sato parameters:', 
     *        T44, F10.5, T55, F10.5, T66, F10.5,/)
C
      DO  10 I = 1,3
             Z(I)    = ZI(I)
C   Convert to atomic units
             D(I)    = DEI(I)/CKCAL
             RE(I)   = REI(I)/CANGS
             BETA(I) = BETAI(I)*CANGS
C   Compute useful constants
             ZPO(I)   = 1.0D0 + Z(I)
             OP3Z(I)  = 1.0D0 + 3.0D0*Z(I)
             TOP3Z(I) = 2.0D0*OP3Z(I)
             ZP3(I)   = Z(I) + 3.0D0
             TZP3(I)  = 2.0D0*ZP3(I)
             DO4Z(I)  = D(I)/4.0D0/ZPO(I)
             B(I)     = BETA(I)*DO4Z(I)*2.0D0
10    CONTINUE
C
      RETURN
      END subroutine prepot
C*****
C
         SUBROUTINE pot
         use potcm3
         use clhbr
C
C   System:          ABC
C   Functional form: Extended LEPS (London-Erying-Polyani-Sato)
C   References:      P. J. Kuntz, E. M. Nemth, J. C. Polanyi, S. D. Rosner,
C                    and C. E. Young 
C                    J. Chem. Phys. 44, 1168 (1966)
C
C        PREPOT must be called once before any calls to POT.
C        The potential parameters are initialized in the subprogram PREPOT.
C        Coordinates, potential energy, and derivatives are passed 
C        through the common block POTCM:
C                  COMMON /POTCM/ R(3), ENERGY, DEDR(3)
C        The constants for the potential are passed through the common blocks
C        SATOCM and LEPSCM.
C        All the information passed through the common blocks POTCM, SATOCM, 
C        and LEPSCM are in hartree atomic units.  
C
C        For the reaction: A + BC -> AB + C we write:
C                          R(1) = R(A-B)
C                          R(2) = R(B-C)
C                          R(3) = R(C-A)
C
C   NOTE: The potential energy at the reactant asympotote is not equal to
C         zero, it is set equal to the energy of the reactant diatomic.
C
C   The the flags that indicate what calculations should be carried out in 
C   the potential routine are passed through the common block POTCCM:
C                  /POTCCM/ NSURF, NDER, NDUM(8)
C   where:
C        NSURF - which electronic surface should be used.
C                In this potential this option is not used; the ground
C                electronic state is the only one available.
C        NDER  = 0 => no derivatives should be calculated
C        NDER  = 1 => calculate first derivatives
C        NDUM  - these 8 integer values can be used to flag options
C                within the potential; in this potential these options 
C                are not used.
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c        COMMON /POTCM/R(3), ENERGY, DEDR(3)
c        COMMON /POTCCM/ NSURF, NDER, NDUM(8)
c        COMMON /SATOCM/ D(3), RE(3), BETA(3), Z(3) 
c        COMMON /LEPSCM/ ZPO(3), OP3Z(3), ZP3(3), TZP3(3), TOP3Z(3), 
c    *                   DO4Z(3), B(3)
         DIMENSION X(3), COUL(3), EXCH(3)
         PARAMETER (R2 = 1.41421356D0)
C
C   Initialize the variable used in the calculation of the energy.
C
         ENERGY = 0.D0
C
C   Compute the energy.
C
         DO 10 I = 1,3
               X(I)    = EXP(-BETA(I)*(R(I)-RE(I)))
               COUL(I) = DO4Z(I)*(ZP3(I)*X(I)-TOP3Z(I))*X(I)
               EXCH(I) = DO4Z(I)*(OP3Z(I)*X(I)-TZP3(I))*X(I)
               ENERGY  = ENERGY + COUL(I)
10       CONTINUE
C
         RAD = SQRT((EXCH(1)-EXCH(2))**2 + (EXCH(2)-EXCH(3))**2 +
     *              (EXCH(3)-EXCH(1))**2)
C
         ENERGY = ENERGY - RAD/R2 
C
C   Compute the derivatives of the energy with respect to the internal
C   coordinates.
C
         IF (NDER .EQ. 1) THEN
             S = EXCH(1) + EXCH(2) + EXCH(3)
             DO 20 I = 1,3
                   DEDR(I) = B(I)*X(I)*((3.0D0*EXCH(I)-S)/R2*
     *                       (OP3Z(I)*X(I)-ZP3(I))/RAD-
     *                       ZP3(I)*X(I)+OP3Z(I))
20           CONTINUE
         ENDIF
C
      RETURN
      END subroutine pot
C*****

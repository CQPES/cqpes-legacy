C
C  This file provides dummy SURF and SETUP routines for unit fu30 input cases.
C
      SUBROUTINE SURF (V, X, DX, N3TM)
C*
C   This potential routine is a dummy routine which, if called, will
C   write an error message to unit 6 and stop.
C
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
         DIMENSION X(N3TM), DX(N3TM)
C
         WRITE (6, 100) 
  100    FORMAT(/,5X,60('*'),//,10X,
     *          'THIS POTENTIAL IS A DUMMY POTENTIAL AND SHOULD ',/,10X,
     *          'NOT BE CALLED FOR ENERGIES AND/OR DERIVATIVES.',
     *          //,5X,60('*'))
C
         STOP 'SURF 1'
C
         END
C*****
C
      SUBROUTINE SETUP (N3TM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   This is the SETUP routine for the dummy potential.  It checks that
C   it is called with the number of cartesians set greater than zero
C   and returns.
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
      PARAMETER (N3TMMN = 0)
C
      IF (N3TM .LT. N3TMMN) THEN
C
C  CHECK THE NUMBER OF CARTESIAN COORDINATES SET BY THE CALLING PROGRAM
C
          WRITE (6, 1000) N3TM, N3TMMN
          STOP 'SETUP 1'
      ENDIF
C
1000     FORMAT(/,2X,T5,'WARNING: N3TM is set equal to ',I3,
     *                  ' but this potential routine',
     *          /,2X,T14,'requires N3TM be greater than or ',
     *                   'equal to ',I3,/)
C
      RETURN
C
      END
C*****

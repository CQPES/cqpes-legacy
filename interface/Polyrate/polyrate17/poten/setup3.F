C
         SUBROUTINE setup (N3TM)
C
C   This is a generic SETUP subprogram for a three body system.  This 
C   subprogram checks the value of N3TM and calls the subprogram PREPOT,
C   which initializes the parameters for the potential energy routine.
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
         PARAMETER (N3TMMN = 9)
C
C   Check the number of cartesian coordinates set by the calling program.
C
         IF (N3TM .LT. N3TMMN) THEN
             WRITE (6, 1000) N3TM, N3TMMN
             STOP 'SETUP 1'
         ENDIF
C
         CALL PREPOT
C
         RETURN
C
1000     FORMAT(/,2X,T5,'WARNING: N3TM is set equal to ',I3,
     *                  ' but this potential routine',
     *          /,2X,T14,'requires N3TM be greater than or ',
     *                   'equal to ',I3,/)
C
         END subroutine setup
C*****

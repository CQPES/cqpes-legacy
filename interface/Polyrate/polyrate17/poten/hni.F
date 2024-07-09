      SUBROUTINE SETUP(NCRDM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C VERSION 4
C
C Set up data for EDIM potential and perform some error checking; 
C must be called once before any calls to the main driver SURF.
C
C Atoms are partitioned into covalent gas-phase atoms and metal surface
C    atoms.  The metal atoms are further divided into a primary zone
C    and secondary zone; the primary zone also includes all covalent
C    atoms.  All atoms in the primary zone are allowed to move and
C    their derivatives (wrt spaced fixed cartesians) are computed.
C The potential is designed to treat an arbitrary number of covalent
C    atoms, but in the current version only up to 3 is coded.  The
C    calculation of the components needed for the DIM is in place, but
C    the DIM evaluation for more than 3 atom on a surface is not
C    implemented. Furthermore, only one type of gas-phase atom may now
C    be treated.
C Unscaled cartesian coordinates are passed thru parameter list and
C    are in atomic units.  Energy and derivatives wrt unscaled cartesian
C    coordinates are returned to the calling routine in atomic units.
C    The EAM data is such that using coordinates in Angstroms gives
C    energies in eV and derivatives in eV/Angstroms.  All other data is
C    input in atomic units unless explicitly stated before the read
C    statement and is converted to use eV and Anstroms internally.  BOHR
C    converts from au to Angstoms, CEV converts from Hartree to eV, and
C    CONVF converts from eV/Angstroms to Hartree/bohr.
C The parameter list must match the variable type and dimensioning used
C    in the calling routine.  The dimension is set by the variable NCRDM
C    NCRDM is three times the maximum number of atoms allowed to move in
C    the calling program; this must be at equal to or greater than the 
C    the number of atoms allowed to move in this potential energy 
C    surface.  
C The maximum number of atoms allowed to move in this surface routine is
C    set by the parameter NATDM; more may be included in the calculation
C    by adjusting this parameter (keeping in mind that this potential
C    is currently only coded for 3 gas atoms). 
C The number of atoms allowed to move in a given calculation using this
C    surface is set by the input variables NCOV and NMETP which are 
C    explained below.  
C The SETUP subroutine also does some error checking, in particular 
C    if the number of gas atoms NCOV > 3, or NATDM < NCOV + NMETP,
C    or, NCRDM < NCOV + NMETP then error messages will be written to 
C    unit 6 and the calculation stopped.
C The coordinates of only the moving atoms (those in the primary
C    zone) are passed through parameter list and are assumed to be
C    arranged in the X array as follows:
C                               X-ARRAY INDICES
C      ATOM               X           Y           Z
C    gas atom k      3*(k-1)+1   3*(k-1)+2   3*(k-1)+3, k=1,NCOV
C    surface atom i  3*(i-1)+1   3*(i-1)+2   3*(i-1)+3, i=NCOV+1,NATPRM
C    where NCOV   = number of gas atoms
C          NATPRM = number of atoms in primary zone (gas + metal)
C
C NATMX  - maximum number of atoms (gas + solid) including moving and
C          fixed geometries.
C NCOVMX - maximum number of gas atoms
C NVCDM  - maximum number of 'bonds' (cov-cov and cov-metal)
C NATDM  - maximum number of atoms (gas + solid) allowed to move
C
      PARAMETER (NATDM = 3)
C
      PARAMETER (NATMX=401, NCOVMX=3, NVCDM=(NCOVMX*(NCOVMX+1))/2)
      COMMON /SURCM1/ R(3,NATMX), DIJ(NATMX,NATMX),
     *   DIJDR(3,NATMX,NATMX), VCOV(NVCDM), DVCOV(3,NATDM,NVCDM),
     *   VCOV3(NVCDM), DVCOV3(3,NATDM,NVCDM), DVMET(3,NATDM),
     *   RHOS(NATMX), RHO(NATMX), RHOP(NATMX,NATMX), RHOI(NATMX),
     *   F(NATMX), FP(NATMX), EZERO, ITYPE(NATMX), NATPRM, NATPRP,
     *   NCOV, NCOVP, NMET, NMETP,NATOMS, NBOND, NBONDC, INDGS,
     *   NCUT(NATMX), ID(NATMX,NATMX)
C
C NDIMDM - maximum dimension needed for DIM calculation. (For NCOV=2,
C          NDIM=2, and for NCOV=3, NDIM=5.)
      PARAMETER (NDIMDM=5)
      COMMON /SURCM2/ H(NDIMDM,NDIMDM), S(NDIMDM,NDIMDM), EIS1(NDIMDM),
     *   EIS2(NDIMDM), U(NDIMDM,NDIMDM), EIG(NDIMDM),
     *   DHDR(NDIMDM,NDIMDM), DVDR(3,NATDM)
C
C NTYMDM - maximum number of metal types
C NTYGDM - maximum number of non-metal types (gas types)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      PARAMETER (NMORDM=(NTYGDM*(NTYGDM+1))/2)
      COMMON /SMORCM/ DE(NMORDM), ALFM(NMORDM), REQ(NMORDM),
     *   SATO(NMORDM)
      COMMON /STRICM/ TRIPH(3,NTYGDM)
      COMMON /SCUTCM/ RCUT(2,NTYPDM),DCUT(NTYPDM)
      COMMON /SRHOCM/ CRI(10,NTYPDM), CI(10,NTYPDM), SI(10,NTYPDM),
     *   FACT(10,NTYPDM), RNSPAR(2,NTYPDM), NI(10,NTYPDM), NTS(NTYPDM),
     *   NTP(NTYPDM), NE(NTYPDM)
      COMMON /SZRCM/ Z0(NTYPDM), BETA(NTYPDM), ALPHA(NTYPDM), CONV
      COMMON /SFCM/  FPAR(12,NTYPDM)
C
      DATA BOHR/0.52917706E0/, CEV/27.21161E0/
C
C NCOV   - number of gas atoms
C NMETP  - number of metal atoms allowed to move (primary zone)
C NMETS  - number of metal atoms in secondary zone (total metal atoms
C          minus NMETP).
C
C OPEN THE POTENTIAL DATA FILE
C
       OPEN (UNIT=4, FILE='pothni.dat', STATUS='OLD', 
     *       FORM='FORMATTED', ERR=100)
C
C
      READ (4, *) NCOV, NMETP, NMETS
C NATPRM - number of atoms in primary zone
C
      NATPRM = NCOV + NMETP
C
C Check that the primary zone has at least one atom and less than
C the maximum set by the parameter NATDM
C
      IF (NATPRM .LE. 0) THEN
         WRITE (6, *) ' Number of atoms in the primary zone must be'
         WRITE (6, *) ' greater than zero but is', NATPRM
         STOP 'SETUP 2'
      ELSE IF (NATPRM .GT. NATDM) THEN
         WRITE (6, 1000) NATPRM, NATDM
         STOP 'SETUP 3'
      END IF
C
C Check the number of gas atoms read in from unit 4
C
      IF (NCOV .GT. NCOVMX) THEN
          WRITE (6, 1100) NCOVMX, NCOV
          STOP 'SETUP 4'
      ENDIF
C
C Check that the calling program has passes the correct number
C of cartesian coordinates for the atoms in the primary zone.
C
      IF (NCRDM .LT. (3 * NATPRM)) THEN
          WRITE (6, 1200) NCRDM, 3*NATPRM
          STOP 'SETUP 5'
      ENDIF
C
      NATPRP = NATPRM + 1
C NMET   - number of metal atoms
      NMET = NMETP + NMETS
C NATOMS - total number of atoms
      NATOMS = NMET + NCOV
C NCOVP  - index of first metal atom
      NCOVP = NCOV + 1
C NBONDC - number of gas-gas bonds
      NBONDC = (NCOV*(NCOV-1))/2
C NBOND  - total number of bond (gas-gas + gas-metal)
      NBOND = NBONDC + NCOV
C INDGS  - index of first gas-metal bond)
      INDGS = NBONDC + 1
      WRITE (6, 600)  NCOV, NMETP, NMETS, NATPRM, NMET, NATOMS, NBOND
C
C Morse parameters for covalent-covalent bonds and Sato parameters
C    DE    - Morse dissociation energy (in Hartree)
C    ALPM  - Morse range parameter (in 1/bohr)
C    REQ   - Morse equilibrium geometry (in bohr)
C    SATCOV- cov-cov Sato parameter
      WRITE (6, 601)
      IT = 0
      DO 10 IT1 = 1,NTYGDM
         DO 10 IT2 = 1,IT1
            IT = IT + 1
            READ (4, *) DE(IT), ALFM(IT), REQ(IT), SATO(IT)
            WRITE (6, 602) IT1+NTYMDM, IT2+NTYMDM, DE(IT), ALFM(IT),
     *         REQ(IT), SATO(IT)
C   Convert energies to eV, lengths to Angstroms
            DE(IT) = DE(IT)*CEV
            ALFM(IT) = ALFM(IT)/BOHR
            REQ(IT) = REQ(IT)*BOHR
            SATO(IT) = 0.5E0*(1.0E0-SATO(IT))/(1.0E0+SATO(IT))
   10 CONTINUE
C    TRIPH - parameters for gas-metal triplet curves
      WRITE (6, 603)
      DO 20 IT = 1,NTYGDM
         READ (4, *) (TRIPH(I,IT), I=1,3)
         WRITE (6, 604) IT+NTYMDM, (TRIPH(I,IT), I=1,3)
   20 CONTINUE
C   RNSPAR - parameters for effective number of s electrons in the rho
C            calculations
      WRITE (6, 605)
      DO 30 IT = 1,NTYMDM
         READ (4, *) (RNSPAR(I, IT), I=1,2)
         WRITE (6, 604) IT, (RNSPAR(I, IT), I=1,2)
   30 CONTINUE
      WRITE (6, 606)
      DO 35 IT = 1,NTYGDM
         READ (4, *) (RNSPAR(I, IT+NTYMDM), I=1,2)
         WRITE (6, 604) IT+NTYMDM, RNSPAR(1,IT+NTYMDM)
   35 CONTINUE
      READ (4, *) (FPAR(I,3), I=1,2)
      WRITE (6, 620) (FPAR(I,3), I=1,2)
C   Parameters for pair repulsion terms
      WRITE (6, 607)
      DO 40 IT = 1,NTYPDM
         READ (4, *) Z0(IT), BETA(IT), ALPHA(IT)
         WRITE (6, 604) IT, Z0(IT), BETA(IT), ALPHA(IT)
   40 CONTINUE
C   Parameters for switching function
      WRITE (6, 608)
      DO 50 IT = 1,NTYPDM
         READ (4, *) RCUT(1,IT),RCUT(2,IT)
         DCUT(IT) = RCUT(1,IT) - RCUT(2,IT)
         WRITE (6, 604) IT, RCUT(1,IT),RCUT(2,IT)
   50 CONTINUE
C   Zero of energy read in Hartree
      READ (4, *) EZERO
      WRITE (6, 609) EZERO
      IF (NCOV .GT. 1) THEN
C   Set up overlap matrix
         IF (NCOV .EQ. 2) THEN
            NDIM = 2
            S(1,1) = 4.0E0
            S(1,2) = -2.0E0
            S(2,2) = 4.0E0
         ELSE IF (NCOV .EQ. 3) THEN
            NDIM = 5
            S(1,1) = 1.0E0
            S(1,2) = 0.25E0
            S(2,2) = 1.0E0
            S(1,3) = -0.5E0
            S(2,3) = -0.5E0
            S(3,3) = 1.0E0
            S(1,4) = -0.5E0
            S(2,4) = -0.5E0
            S(3,4) = 0.25E0
            S(4,4) = 1.0E0
            S(1,5) = -0.5E0
            S(2,5) = -0.5E0
            S(3,5) = 0.25E0
            S(4,5) = 0.25E0
            S(5,5) = 1.0E0
         END IF
C    Get Cholesky factorization of S
         DO 120 I = 1,NDIM
            DO 120 J = 1,I
               H(J,I) = 0.0E0
  120    CONTINUE
         CALL REDUC (NDIMDM, NDIM, 1, H, S, EIS2, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (6, 6000) IERR
            STOP
         END IF
      END IF
C
      READ (4,*) (ITYPE(I),I=1,NATPRM)
      WRITE (6, 610) (ITYPE(I),I=1,NATPRM)
      IF (NATOMS .GT. NATMX) THEN
         WRITE (6, *) ' NATOMS=', NATOMS, ' .GT. NATMX=', NATMX
         STOP
      END IF
      IF (NATPRP .LE. NATOMS) THEN
C  Read in geometries of atoms that are fixed to their lattice sites
C     (secondary zone)
         DO 140 I = NATPRP,NATOMS
            READ (4,*) ITYPE(I), (R(J,I),J=1,3)
C  Convert from bohr to angstrom.
            DO 130 J = 1,3
               R(J,I) = R(J,I)*BOHR
  130       CONTINUE
  140    CONTINUE
      END IF
C
C  Calculate cri constants matrix for rhom
      DO 160 IT = 1,NTYPDM
         NN = NTS(IT)+NTP(IT)
         DO 150 I=1,NN
            EX = FLOAT(NI(I,IT))+0.5
            CRI(I,IT) = CI(I,IT)*(2.0*SI(I,IT))**EX/SQRT(FACT(I,IT))
  150    CONTINUE
  160 CONTINUE
C
C Get electron density at each atom in the secondary zone due to all
C    other atoms in the secondary zone.
      DO 170 I = 1,NATOMS
         RHOS(I) = 0.0
  170 CONTINUE
      IF (NATPRP.LE.NATOMS) THEN
C First get interatomic distances and set up lookup table.
         CALL SURR2 (NATPRP, NATOMS, R, DIJ, ITYPE, NCUT, ID, NATMX)
         DO 190 I = NATPRP,NATOMS
            NMAX = NCUT(I)
            IF (NMAX .GT. 0) THEN
C Evaluate density from atom i at all atoms j in the lookup list for
C    atom i.
               CALL SURRHO(R(3,I), DIJ(1,I), RHOI, RHOP, ITYPE(I),
     *            NATOMS, NMAX, ID(1,I))
               DO 180 JJ = 1,NMAX
                  J = ID(JJ,I)
                  RHOS(J) = RHOS(J) + RHOI(J)
  180          CONTINUE
            END IF
  190    CONTINUE
      END IF
C
C  CLOSE THE POTENTIAL DATA FILE
C
      CLOSE (UNIT = 4)
C
      RETURN
  600 FORMAT (/,2X,T5,'SETUP has been called for the ',
     *                'EDIM potential version 5',
     *   //,2X,T5,'Number of covalent atoms=', T50, I5, /,
     *   2X,T5,'Number of metal atoms in primary zone=', T50, I5, /,
     *   2X,T5,'Number of metal atoms in secondary zone=', T50, I5, /,
     *   2X,T5,'Total number of atoms in primary zone=', T50, I5, /,
     *   2X,T5,'Total number of metal atoms=', T50, I5, /,
     *   2X,T5,'Total number of atoms=', T50, I5, /,
     *   2X,T5,'Number of cov-cov and cov-met bonds=', T50, I5)
  601 FORMAT (/,2X,T5,'Covalent-covalent Morse parameters', /,
     * 2X,T5,'Atom type', 5X, 'D', 14X, 'Alf', 12X, 'Req', 12X, 'Sato')
  602 FORMAT (5X, 2I5, 1P,4E15.7)                                       1113GL92
  603 FORMAT (/,2X,T5,'Parameters defining cov-met triplet interaction',
     *        /,2X,T7,'Atom', 5X, 'Parameters')
  604 FORMAT (5X, I5, 1P,4E15.7)                                        1113GL92
  605 FORMAT (/,2X,T5,'Effective number of s electrons for metal', /,
     *          2X,T7, 'Atom', 5X, 'Bulk', 11X, 'Surface')
  606 FORMAT (/,2X,T5,'Effective number of s electrons for covalent',
     *   ' atoms', /, 2X, T7, 'Atom')
  607 FORMAT (/,2X,T5,'Parameters for pair repulsion terms', /, 2X,T7,
     *   'Atom', 5X, 'Z0', 13X, 'Beta', 11X, 'Alpha')
  608 FORMAT (/, 2X, T5, 'Parameters for switching function', /,
     *   2X, T7, 'Atom', 5X, 'RCUT1', 11X, 'RCUT2')
  609 FORMAT (/, 2X, T5, 'Zero of energy =', T21, 1PE17.10, ' Hartree')
  610 FORMAT (2X, T5, 'For primary zone, ITYPE = ', (T31,16I3,/))
  620 FORMAT (/, 5X, 'Parameters for embedding function for atom 3:', 
     *        /, 2X,T5,'F1 = ', T12, 1PE15.7, T40,'F2 = ', T47,1PE15.7)
 1000 FORMAT(/,3X,5(1H*),'Number of atoms in the primary zone, ',
     *       'NATPRM = ',I5,' is larger than the maximum set by',
     *       ' the parameter NATDM = ',/)
 1100 FORMAT(/,3X,5(1H*),'This surface is coded for ',I2,
     *       'gas-phase atoms, but the number read in from unit',
     *       ' 4 is ',I3,/)
 1200 FORMAT(/,3X,5(1H*),'The calling routine has passed only',
     *       I5,' cartesian coordinates, but the potential ',
     *       'requires ',I5,/)
 6000 FORMAT (' In SETUP, error with Eispack routine REDUC, IERR=', I5)
C
  100 WRITE(6,*)'Error opening potential data file'
      STOP 'SETUP 1'
C
      END
      BLOCK DATA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Block data for EDIM potentials
C
C NTYMDM - maximum number of metal types
C NTYGDM - maximum number of non-metal types (gas types)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      COMMON /SRHOCM/ CRI(10,NTYPDM), CI(10,NTYPDM), SI(10,NTYPDM),
     *   FACT(10,NTYPDM), RNSPAR(2,NTYPDM), NI(10,NTYPDM), NTS(NTYPDM),
     *   NTP(NTYPDM), NE(NTYPDM)
      COMMON /SZRCM/ Z0(NTYPDM), BETA(NTYPDM), ALPHA(NTYPDM), CONV
      COMMON /SFCM/  FPAR(12,NTYPDM)
C
      DATA CONV /14.3888/
C
C Ni embedding energy
      DATA (FPAR(I,1),I=1,12) / -121.9313915, 0.0876895, 18.2047475,
     *  10.588929, -205.250512, 51.8830662, 0.2,0.21, -2.837911371E10,
     * -7.56258116E8, -5.66038368E6, -19.0929912/
C H embedding energy
      DATA (FPAR(I,2), I=1,2) / -70.5460962, 6.9507109/
C
C Ni electron density
      DATA NE(1),NTS(1),NTP(1) /10, 8, 2/
C                             1s    2s    3s    4s    3d
      DATA (NI(I,1),I=1,10) / 1, 1, 2, 2, 3, 3, 4, 4, 3, 3/
      DATA (FACT(I,1),I=1,10) /2., 2., 24., 24., 720., 720., 40320.,
     *   40320., 720.,720./
      DATA (CI(I,1),I=1,10) / -0.00389, -0.02991, -0.03189, 0.15289,
     *   -0.20048, -0.05423, 0.49292, 0.61875, 0.4212, 0.70658/
      DATA (SI(I,1),I=1,10) / 54.88885, 38.48431, 27.42703, 20.88204,
     *   10.95707, 7.31958, 3.9265, 2.15289, 12.67582, 5.43253 /
C
C H electron density
      DATA NE(2), NTS(2), NTP(2) /1, 1, 0/
      DATA NI(1,2) /1/
      DATA FACT(1,2) /2./
      DATA CI(1,2) / 1.0/
      DATA SI(1,2) / 1.88972652/

C O electron density
      DATA NE(3),NTS(3),NTP(3) /6, 4, 2/
C                            1s    2s    2p
      DATA (NI(I,3),I=1,6) / 1, 1, 2, 2, 2, 2/
      DATA (FACT(I,3),I=1,6) /2., 2., 24., 24., 24., 24./
      DATA (CI(I,3),I=1,6) / 0.01587, -0.29963, 0.70761, 0.37450,
     *   0.33221, 0.74483/
      DATA (SI(I,3),I=1,6) / 17.89480, 12.92567, 5.08130, 3.16716,
     *    6.98384, 3.13543/
C
      END
      SUBROUTINE SURF(E, X, DX, NCRDM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Driver for evaluation of potential for gas-metal-surface interactions.
C
      PARAMETER (NATDM = 3)
      PARAMETER (NATMX=401, NCOVMX=3, NVCDM=(NCOVMX*(NCOVMX+1))/2)
      COMMON /SURCM1/ R(3,NATMX), DIJ(NATMX,NATMX),
     *   DIJDR(3,NATMX,NATMX), VCOV(NVCDM), DVCOV(3,NATDM,NVCDM),
     *   VCOV3(NVCDM), DVCOV3(3,NATDM,NVCDM), DVMET(3,NATDM),
     *   RHOS(NATMX), RHO(NATMX), RHOP(NATMX,NATMX), RHOI(NATMX),
     *   F(NATMX), FP(NATMX), EZERO, ITYPE(NATMX), NATPRM, NATPRP,
     *   NCOV, NCOVP, NMET, NMETP,NATOMS, NBOND, NBONDC, INDGS,
     *   NCUT(NATMX), ID(NATMX,NATMX)
C
C NDIMDM - maximum dimension needed for DIM calculation. (For NCOV=2,
C          NDIM=2, and for NCOV=3, NDIM=5.)
      PARAMETER (NDIMDM=5)
      COMMON /SURCM2/ H(NDIMDM,NDIMDM), S(NDIMDM,NDIMDM), EIS1(NDIMDM),
     *   EIS2(NDIMDM), U(NDIMDM,NDIMDM), EIG(NDIMDM),
     *   DHDR(NDIMDM,NDIMDM), DVDR(3,NATDM)
C
      DIMENSION X(NCRDM), DX(NCRDM)
C
      DATA BOHR/0.52917706E0/, CEV/27.21161E0/, CONVF/0.01944673836E0/
C
C Convert coordinates to Angstroms
      I = 0
      DO 10 II = 1,NATPRM
         DO 5 J = 1,3
            I = I + 1
            R(J,II) = X(I)*BOHR
    5    CONTINUE
   10 CONTINUE
C
C Get interatomic distances and set up lookup table
      CALL SURR1 (NCOV, NCOVP, NATPRM, NATOMS, R, DIJ, DIJDR, ITYPE,
     *   NCUT, ID, NATMX)
C
C Get covalent potentials and derivatives and derivatives
      IF (NCOV .GT. 1) CALL SURCOV (NCOV, NBONDC, NATPRM, NATDM, NATMX,
     *   ITYPE, DIJ, DIJDR, VCOV, DVCOV, VCOV3, DVCOV3)
C
C Get EAM potentials for gas-metal interactions and metal-metal
C    interactions and their derivatives
      CALL SUREAM (NCOV, NMET, NATPRM, NATOMS, NCOVP, NMETP, NVCDM,
     *   NATDM, NATMX, R, ITYPE, DIJ, DIJDR, VCOV(INDGS),
     *   DVCOV(1,1,INDGS), VCOV3(INDGS), DVCOV3(1,1,INDGS), VMET, DVMET,
     *   RHOS, RHO, RHOP, RHOI, F, FP, NCUT, ID)
C
C Evaluate DIM for potential and derivatives
      CALL SURDIM (NCOV, NATDM, NBOND, NATPRM, VCOV, DVCOV, VCOV3,
     *   DVCOV3, NDIMDM, H, S, EIS1, EIS2, U, EIG, DHDR, V, DVDR)
C
C Total energy is DIM potential plus the metal-metal interactions
      E = (V + VMET)/CEV + EZERO
C
C Evaluate derivative and convert to au
      I = 0
      DO 20 II = 1,NATPRM
         DO 20 J = 1,3
            I = I + 1
            DX(I) = (DVDR(J,II) + DVMET(J,II))*CONVF
   20 CONTINUE
C
      RETURN
      END
      SUBROUTINE SURCOV (NCOV, NBONDC, NATPRM, NATDM, NATMX, ITYPE,
     *   DIJ, DIJDR, VCOV, DVCOV, VCOV3, DVCOV3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Compute covalent-covalent pair potentials and derivatives
C
      DIMENSION DIJ(NATMX,NATMX), DIJDR(3,NATMX,NATMX),
     *   VCOV(NBONDC), DVCOV(3,NATDM,NBONDC), VCOV3(NBONDC),
     *   DVCOV3(3,NATDM,NBONDC), ITYPE(NATMX)
C
C NTYMDM - maximum number of metal types
C NTYGDM - maximum number of non-metal types (gas types)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      PARAMETER (NMORDM=(NTYGDM*(NTYGDM+1))/2)
      COMMON /SMORCM/ DE(NMORDM), ALFM(NMORDM), REQ(NMORDM),
     *   SATO(NMORDM)
C
      DO 10 J = 1,NBONDC
         DO 10 I = 1,NATPRM
            DO 10 K = 1,3
               DVCOV(K,I,J) = 0.0E0
               DVCOV3(K,I,J) = 0.0E0
   10 CONTINUE
      INDX = 0
      DO 50 J = 2,NCOV
         JM = J - 1
         IT1 = ITYPE(J) - NTYMDM
         DO 50 I = 1,JM
            IT2 = ITYPE(I) - NTYMDM
            ITG = MAX(IT1,IT2)
            IT = (ITG*(ITG-1))/2 + MIN(IT1,IT2)
            INDX = INDX + 1
            T = -ALFM(IT)*(DIJ(I,J)-REQ(IT))
            AEXP = EXP(T)
            T = DE(IT)*AEXP
            VCOV(INDX) = T*(AEXP-2.0E0)
            TD = -2.0E0*ALFM(IT)*T
            DV = TD*(AEXP-1.0E0)
            T = SATO(IT)*T
            VCOV3(INDX) = T*(AEXP+2.0E0)
            TD = -2.0E0*ALFM(IT)*T
            DV3 = TD*(AEXP+1.0E0)
            DO 40 K = 1,3
               DR = DIJDR(K,I,J)
               T = DV*DR
               DVCOV(K,I,INDX) = DVCOV(K,I,INDX) + T
               DVCOV(K,J,INDX) = DVCOV(K,J,INDX) - T
               T = DV3*DR
               DVCOV3(K,I,INDX) = DVCOV3(K,I,INDX) + T
               DVCOV3(K,J,INDX) = DVCOV3(K,J,INDX) - T
   40       CONTINUE
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SURDIM (NCOV, NATDM, NBOND, NATPRM, VCOV, DVCOV,
     *   VCOV3, DVCOV3, NDIMDM, H, S, EIS1, EIS2, U, E, DHDR, V, DVDR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Evaluate DIM potential and derivatives wrt cartesian coordinates
C
      DIMENSION VCOV(NBOND), DVCOV(3,NATDM,NBOND), VCOV3(NBOND),
     *  DVCOV3(3,NATDM,NBOND), H(NDIMDM,NDIMDM), S(NDIMDM,NDIMDM),
     *   EIS1(NDIMDM), EIS2(NDIMDM), U(NDIMDM,NDIMDM), E(NDIMDM),
     *   DHDR(NDIMDM,NDIMDM), DVDR(3,NATDM)
C
      IF (NCOV .EQ. 1) THEN
C Only one covalent atom, V and DVDR are set to the one gas-surface
C    interaction term
         V = VCOV(1)
         DO 10 I = 1,NATPRM
            DO 10 J = 1,3
               DVDR(J,I) = DVCOV(J,I,1)
   10    CONTINUE
         RETURN
      ELSE
C More than one covalent atom; compute Q's and J's and put them back
C    into VCOV and DVCOV
         QSUM = 0.0E0
         XJSUM = 0.0E0
         DO 20 I = 1,NBOND
            T1 = VCOV(I)
            T3 = VCOV3(I)
            Q = 0.5E0*(T1 + T3)
            VCOV(I) = Q
            QSUM = QSUM + Q
            XJ = 0.5E0*(T1 - T3)
            VCOV3(I) = XJ
            XJSUM = XJSUM + XJ
   20    CONTINUE
C Construct upper triangle of NDIMxNDIM Hamiltonian matrix.
         IF (NCOV .EQ. 2) THEN
            NDIM = 2
            XJM = VCOV3(2) + VCOV3(3)
            H(1,1) = 2.0E0*(2.0E0*(QSUM + VCOV3(1)) - XJM)
            H(1,2) = -2.0E0*(QSUM + VCOV3(1) + XJM)
            H(2,2) = 2.0E0*(2.0E0*(QSUM + XJM) - VCOV3(1))
         ELSE IF (NCOV .EQ. 3) THEN
            NDIM = 5
            T = QSUM - 0.5E0*XJSUM
            H(1,1) = T + 1.5E0*(VCOV3(1) + VCOV3(6))
            H(1,2) = 0.25E0*(T + 1.5E0*(VCOV3(1) - VCOV3(2) + VCOV3(4) +
     *         VCOV3(3) + VCOV3(5) + VCOV3(6)))
            H(2,2) = T + 1.5E0*(VCOV3(4) + VCOV3(3))
            H(1,3) = -0.5E0*(T + 1.5E0*(VCOV3(1) + VCOV3(6)))
            H(2,3) = -0.5E0*(T + 1.5E0*(VCOV3(1) - VCOV3(2) + VCOV3(4) +
     *         VCOV3(3)))
            H(3,3) = T + 1.5E0*VCOV3(1)
            H(1,4) = -0.5E0*(T + 1.5E0*(VCOV3(1) + VCOV3(4) + VCOV3(5) +
     *         VCOV3(6)))
            H(2,4) = -0.5E0*(T + 1.5E0*(VCOV3(4) + VCOV3(3) +
     *         VCOV3(5) + VCOV3(6)))
            H(3,4) = H(1,2)
            H(4,4) = T + 1.5E0*(VCOV3(4) + VCOV3(5) + VCOV3(6))
            H(1,5) = -0.5E0*(T + 1.5E0*(VCOV3(1) - VCOV3(2) + VCOV3(3) +
     *         VCOV3(6)))
            H(2,5) = -0.5E0*(T + 1.5E0*(VCOV3(4) + VCOV3(3)))
            H(3,5) = H(1,2)
            H(4,5) = H(1,2)
            H(5,5) = T + 1.5E0*VCOV3(3)
         END IF
      END IF
      NM = NDIM - 1
      DO 30 I = 1,NM
         IP = I + 1
         DO 30 J = IP,NDIM
            H(J,I) = H(I,J)
   30 CONTINUE
C
C Diagonalize H using Eispack routine; put eigenvectors into U,
C    eigenvalues into E
      CALL REDUC (NDIMDM, NDIM, -1, H, S, EIS2, IERR)
      IF (IERR .NE. 0) THEN
         WRITE (6, 6000) IERR
         STOP
      END IF
      CALL TRED2 (NDIMDM, NDIM, H, E, EIS1, U)
      CALL TQL2P (NDIMDM, NDIM, E, EIS1, U, IERR)
      IF (IERR .NE. 0) THEN
         WRITE (6, 6001) IERR
         STOP
      END IF
      CALL REBAK (NDIMDM, NDIM, S, EIS2, 1, U)
C
      V = E(1)
C
C Evaluate derivatives of lowest root
      DO 70 I = 1,NATPRM
         DO 70 K = 1,3
            DQSUM = 0.0E0
            DXJSUM = 0.0E0
            DO 40 J = 1,NBOND
               T1 = DVCOV(K,I,J)
               T3 = DVCOV3(K,I,J)
               DQ = 0.5E0*(T1 + T3)
               DVCOV(K,I,J) = DQ
               DQSUM = DQSUM + DQ
               DXJ = 0.5E0*(T1 - T3)
               DVCOV3(K,I,J) = DXJ
               DXJSUM = DXJSUM + DXJ
   40       CONTINUE
            IF (NCOV .EQ. 2) THEN
               DXJM = DVCOV3(K,I,2) + DVCOV3(K,I,3)
               DHDR(1,1) = 2.0E0*(2.0E0*(DQSUM + DVCOV3(K,I,1)) - DXJM)
               DHDR(1,2) = -2.0E0*(DQSUM + DVCOV3(K,I,1) + DXJM)
               DHDR(2,2) = 2.0E0*(2.0E0*(DQSUM + DXJM) - DVCOV3(K,I,1))
            ELSE
               T = DQSUM - 0.5E0*DXJSUM
               DHDR(1,1) = T + 1.5E0*(DVCOV3(K,I,1) + DVCOV3(K,I,6))
               DHDR(1,2) = 0.25E0*(T + 1.5E0*(DVCOV3(K,I,1) -
     *            DVCOV3(K,I,2) + DVCOV3(K,I,4) + DVCOV3(K,I,3) +
     *            DVCOV3(K,I,5) + DVCOV3(K,I,6)))
               DHDR(2,2) = T + 1.5E0*(DVCOV3(K,I,4) + DVCOV3(K,I,3))
               DHDR(1,3) = -0.5E0*(T + 1.5E0*(DVCOV3(K,I,1) +
     *            DVCOV3(K,I,6)))
               DHDR(2,3) = -0.5E0*(T + 1.5E0*(DVCOV3(K,I,1) -
     *            DVCOV3(K,I,2) + DVCOV3(K,I,4) + DVCOV3(K,I,3)))
               DHDR(3,3) = T + 1.5E0*DVCOV3(K,I,1)
               DHDR(1,4) = -0.5E0*(T + 1.5E0*(DVCOV3(K,I,1) +
     *            DVCOV3(K,I,4) + DVCOV3(K,I,5) + DVCOV3(K,I,6)))
               DHDR(2,4) = -0.5E0*(T + 1.5E0*(DVCOV3(K,I,4) +
     *            DVCOV3(K,I,3) + DVCOV3(K,I,5) + DVCOV3(K,I,6)))
               DHDR(3,4) = DHDR(1,2)
               DHDR(4,4) = T + 1.5E0*(DVCOV3(K,I,4) + DVCOV3(K,I,5) +
     *            DVCOV3(K,I,6))
               DHDR(1,5) = -0.5E0*(T + 1.5E0*(DVCOV3(K,I,1) -
     *            DVCOV3(K,I,2) + DVCOV3(K,I,3) + DVCOV3(K,I,6)))
               DHDR(2,5) = -0.5E0*(T + 1.5E0*(DVCOV3(K,I,4) +
     *            DVCOV3(K,I,3)))
               DHDR(3,5) = DHDR(1,2)
               DHDR(4,5) = DHDR(1,2)
               DHDR(5,5) = T + 1.5E0*DVCOV3(K,I,3)
            END IF
            DO 50 L = 1,NM
               LP = L + 1
               DO 50 M = LP,NDIM
                  DHDR(M,L) = DHDR(L,M)
   50       CONTINUE
            SUM = 0.0E0
            DO 60 L = 1,NDIM
               T1 = U(L,1)
               DO 60 M = 1,NDIM
                  T2 = T1*U(M,1)
                  SUM = SUM + T2*DHDR(L,M)
   60       CONTINUE
            DVDR(K,I) = SUM
   70 CONTINUE
C
      RETURN
 6000 FORMAT (' IN CALL TO EISPACK ROUTINE REDUC, IERR =', I5)
 6001 FORMAT (' IN CALL TO EISPACK ROUTINE TQL2, IERR =', I5)
      END
      SUBROUTINE SUREAM (NCOV, NMET, NATPRM, NATOMS, NCOVP, NMETP,
     *   NVCDM, NATDM, NATMX, R, ITYPE, DIJ, DIJDR, VCOV, DVCOV, VCOV3,
     *   DVCOV3, VMET, DVMET, RHOS, RHO, RHOP, RHOI, F, FP, NCUT, ID)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Calculate the potential and derivatives for the EAM potential
C
      DIMENSION R(3,NATOMS), ITYPE(NATOMS), DIJ(NATMX,NATMX),
     *   DIJDR(3,NATMX,NATMX), VCOV(NCOV), DVCOV(3,NATDM,NCOV),
     *   VCOV3(NCOV), DVCOV3(3,NATDM,NCOV), DVMET(3,NATDM),
     *   RHOS(NATOMS), RHO(NATOMS), RHOP(NATMX,NATMX), RHOI(NATMX),
     *   F(NATOMS), FP(NATOMS), NCUT(NATMX), ID(NATMX,NATMX)
C
C NTYMDM - maximum number of metal types
C NTYGDM - maximum number of non-metal types (gas types)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      PARAMETER (NMORDM=(NTYGDM*(NTYGDM+1))/2)
      COMMON /SMORCM/ DE(NMORDM), ALFM(NMORDM), REQ(NMORDM),
     *   SATO(NMORDM)
      COMMON /STRICM/ TRIPH(3,NTYGDM)
      COMMON /SFCM/ FPAR(12,NTYPDM)
C
C Initialization
C    Set electron density to stored value computed in SETUP
      DO 10 I = 1,NATOMS
         RHO(I) = RHOS(I)
   10 CONTINUE
      DO 20 I = 1,NCOV
         DO 20 J = 1,NATPRM
             DO 20 K = 1,3
                 DVCOV(K,J,I) = 0.0E0
                 DVCOV3(K,J,I) = 0.0E0
   20 CONTINUE
      DO 30 J = 1,NATPRM
         DO 30 K = 1,3
            DVMET(K,J) = 0.0E0
   30 CONTINUE
C
C Calculate total electron density rho for all atoms.  For atoms in the
C    primary zone contributions are computed to all other atoms.  For
C    atoms in the secondary zone, their contributions are computed only
C    for atoms in the primary zone, their contributions to atoms in the
C    secondary zone are stored in ROHS.
      DO 50 I = 1,NATOMS
         NMAX = NCUT(I)
         IF (NMAX .GT. 0) THEN
            CALL SURRHO (R(3,I), DIJ(1,I), RHOI, RHOP(1,I), ITYPE(I),
     *         NATOMS, NMAX, ID(1,I))
            DO 40 JJ = 1,NMAX
               J = ID(JJ,I)
               RHO(J) = RHO(J) + RHOI(J)
   40       CONTINUE
         END IF
   50 CONTINUE
C
C Calculate F and dF/dro for all atoms
      DO 80 I = 1,NATOMS
         IF(ITYPE(I).EQ.1) THEN
C    Ni atom
            IF (RHO(I) .GT. FPAR(8,1)) THEN
               F(I) = FPAR(12,1)
               FP(I) = 0.0
            ELSEIF (RHO(I) .GT. FPAR(7,1)) THEN
               DEL = RHO(I) - FPAR(8,1)
               DEL2 = DEL*DEL
               F(I) = FPAR(12,1) + DEL2*DEL*(FPAR(11,1) +
     *            DEL*(FPAR(10,1) + DEL*FPAR(9,1)))
               FP(I) = DEL2*(3.0*FPAR(11,1) + DEL*(4.0*FPAR(10,1) +
     *            DEL*5.0*FPAR(9,1)))
            ELSE
               T1 = -FPAR(2,1)*RHO(I)
               T = FPAR(1,1)*EXP(T1)
               F(I) = T*RHO(I)
               FP(I) = T*(1.0 + T1)
               T1 = -FPAR(4,1)*RHO(I)
               T2 = FPAR(3,1)*RHO(I)
               T = T2*T2*EXP(T1)
               F(I) = F(I) + T2*T
               FP(I) = FP(I) + T*(3.0*FPAR(3,1) - FPAR(4,1)*T2)
               T1 = -FPAR(6,1)*RHO(I)
               T = FPAR(5,1)*EXP(T1)
               F(I) = F(I) + T*RHO(I)
               FP(I) = FP(I) + T*(1.0 + T1)
            ENDIF
         ELSE IF (ITYPE(I) .EQ. 2) THEN
C    H atom
            T1 = -FPAR(2,2)*RHO(I)
            T = FPAR(1,2)*EXP(T1)
            F(I) = T*RHO(I)
            FP(I) = T*(1.0 + T1)
         ELSE IF (ITYPE(I) .EQ. 3) THEN
C    O atom
            T1 = FPAR(2,3)*RHO(I)
            T = FPAR(1,3) + T1
            F(I) = RHO(I)*T
            FP(I) = T + T1
         ENDIF
   80 CONTINUE
C
C Get pair potentials and derivatives for covalent-metal interactions
C    and total covalent-metal potential.
      DO 120 J = 1,NCOV
C Contribution to covalent-metal potential from embedding function
         VCOV(J) = F(J)
         VCOV3(J) = -F(J)
         FPJ = FP(J)
C
C   Gas-metal Sato parameter and its derivative.
         IT = ITYPE(J) - NTYMDM
         A = TRIPH(1,IT)
         B = TRIPH(2,IT)
         C = TRIPH(3,IT)
         T = TANH(A*(RHO(J)-B))
         SATGM = 0.5*T + C
         DSATGM = 0.5*A*(1. - T*T)
C
         NMAX = NCUT(J)
         IF (NMAX .GT. 0) THEN
            DO 110 II = 1,NMAX
               I = ID(II,J)
               D = DIJ(I,J)
C Calculate phi and d(phi)/dRij
               CALL SURZIJ(D,ITYPE(I),ITYPE(J),PHI,PHIP)
C Contribution to total energy from pair potential
               VCOV(J) = VCOV(J) + PHI
               VCOV3(J) = VCOV3(J) + PHI
C Calculate d(rhoa)i/dRij
               RHOPI = RHOP(J,I)
               T = RHOPI*FPJ
               PSIP = PHIP + T
               PSIP3 = SATGM*(PHIP - T)
               DO 90 K = 1,3
                  DRDX = DIJDR(K,I,J)
                  T = PSIP*DRDX
                  DVCOV(K,J,J) = DVCOV(K,J,J) - T
                  IF (I.LE.NATPRM) DVCOV(K,I,J) = DVCOV(K,I,J) + T
                  T = PSIP3*DRDX
                  DVCOV3(K,J,J) = DVCOV3(K,J,J) - T
                  IF (I.LE.NATPRM) DVCOV3(K,I,J) = DVCOV3(K,I,J) + T
   90          CONTINUE
C Calculate d(rhoa)j/dRij
               RHOPJ = RHOP(I,J)
               DEAM = FP(I)*RHOPJ
C Contribution to derivative of metal-metal potential from embedding
C    function
               DO 100 K = 1,3
                  T = DEAM*DIJDR(K,I,J)
                  DVMET(K,J) = DVMET(K,J) - T
                  IF (I.LE.NATPRM) DVMET(K,I) = DVMET(K,I) + T
  100          CONTINUE
  110       CONTINUE
C
            DO 115 II = 1,NMAX
               I = ID(II,J)
               DSATDR = DSATGM*RHOP(J,I)
               HPFP = DSATDR*VCOV3(J)
               DO 114 K = 1,3
                  DRDX = DIJDR(K,I,J)
                  T = HPFP*DRDX
                  DVCOV3(K,J,J) = DVCOV3(K,J,J) - T
                  IF (I.LE.NATPRM) DVCOV3(K,I,J)=DVCOV3(K,I,J) + T
  114          CONTINUE
  115       CONTINUE
         END IF
C
         VCOV3(J) = SATGM*VCOV3(J)
  120 CONTINUE
C
C Get pair potentials and derivatives for metal-metal interactions
C    and total up metal-metal potential
C
C Contribution to energy from embedding function
      VMET = F(NCOVP)
      JSTRT = NCOVP + 1
      DO 150 J = JSTRT,NATOMS
C Contribution to energy from embedding function
         VMET = VMET + F(J)
         IF (J .LE. NATPRM) THEN
            IMAX = J-1
         ELSE
            IMAX = NATPRM
         END IF
         NMAX = NCUT(J)
         IF (NMAX .GT. 0) THEN
            FPJ = FP(J)
            DO 140 II = 1,NMAX
               I = ID(II,J)
               IF (I.GE.NCOVP .AND. I.LE.IMAX) THEN
                  D = DIJ(I,J)
C Calculate phi and d(phi)/dRij
                  CALL SURZIJ(D,ITYPE(I),ITYPE(J),PHI,PHIP)
C Contribution to metal-metal energy from phi
                  VMET = VMET + PHI
C    Calculate d(rhoa)i/dRij
                  RHOPI = RHOP(J,I)
                  PSIP = PHIP + RHOPI*FPJ
C    Calculate d(rhoa)j/dRij
                  RHOPJ = RHOP(I,J)
                  PSIP = PSIP + FP(I)*RHOPJ
                  DO 130 K = 1,3
                     T = PSIP*DIJDR(K,I,J)
                     DVMET(K,I) = DVMET(K,I) + T
                     IF(J.LE.NATPRM) DVMET(K,J) = DVMET(K,J) - T
  130             CONTINUE
               END IF
  140       CONTINUE
         END IF
  150 CONTINUE
      RETURN
      END
      SUBROUTINE SURR1 (NCOV, NCOVP, NATPRM, NATOMS, R, DIJ, DIJDR,
     *   ITYPE, NCUT, ID, NATMX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Evaluate Rij and dRij/dxi between all atoms in the primary zone with
C    all other atoms, for Rij less than RCUT include interaction in the
C    lookup list except for covalent-covalent interactions.
C
      DIMENSION R(3,NATOMS), DIJ(NATMX,NATMX), DIJDR(3,NATMX,NATMX),
     *   ITYPE(NATOMS), NCUT(NATMX), ID(NATMX,NATMX)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      COMMON /SCUTCM/ RCUT(2,NTYPDM), DCUT(NTYPDM)
      DATA EPS /1.0E-13/
C
      DO 20 I = 1,NATPRM
         DIJ(I,I) = 0.0
         DIJDR(1,I,I) = 0.0
         DIJDR(2,I,I) = 0.0
         DIJDR(3,I,I) = 0.0
         IF ((I+1).LE.NATOMS) THEN
            DO 10 J = I+1,NATOMS
               T1 = R(1,I) - R(1,J)
               T2 = R(2,I) - R(2,J)
               T3 = R(3,I) - R(3,J)
               D = T1*T1 + T2*T2 + T3*T3
               D = SQRT(D)
               IF (D .LT. 1.E-13) THEN
                  WRITE(6,'(1X,4HATOM,I3,3HAND,I3,18H ARE TOO CLOSE, D=
     *                  ,1PE16.6)') I,J,D 
                  STOP 100
               ENDIF 
               DIJDR(1,I,J) = T1/D
               DIJDR(2,I,J) = T2/D
               DIJDR(3,I,J) = T3/D
               DIJ(I,J) = D
               DIJ(J,I) = D
               DIJDR(1,J,I) = -DIJDR(1,I,J)
               DIJDR(2,J,I) = -DIJDR(2,I,J)
               DIJDR(3,J,I) = -DIJDR(3,I,J)
   10       CONTINUE
         ENDIF
   20 CONTINUE
      DO 40 I = 1,NATOMS
         IF (I.LE.NCOV) THEN
            JSTRT = NCOVP
            JFINI = NATOMS
         ELSEIF (I.LE.NATPRM) THEN
            JSTRT = 1
            JFINI = NATOMS
         ELSE
            JSTRT = 1
            JFINI = NATPRM
         END IF
         N = 0
         ITYPI = ITYPE(I)
         DO 30 J = JSTRT,JFINI
            IF(I .NE. J .AND. DIJ(I,J) .LT. RCUT(2,ITYPI)) THEN
               N = N + 1
               ID(N,I) = J
            END IF
   30    CONTINUE
         NCUT(I) = N
   40 CONTINUE
      RETURN
      END
      SUBROUTINE SURR2 (NATPRP, NATOMS, R, DIJ, ITYPE, NCUT, ID, NATMX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Evaluate Rij between all atoms in the secondary zone (from NATRPR to
C    NATOMS)
C
      DIMENSION R(3,NATOMS), DIJ(NATMX,NATMX), ITYPE(NATOMS),
     *   NCUT(NATMX), ID(NATMX,NATMX)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      COMMON /SCUTCM/ RCUT(2,NTYPDM), DCUT(NTYPDM)
      DATA EPS /1.0E-13/
C
      DIJ(NATPRP,NATPRP) = 0.0
      DO 20 I = NATPRP+1,NATOMS
         DO 10 J = NATPRP,I-1
            T1 = R(1,I) - R(1,J)
            T2 = R(2,I) - R(2,J)
            T3 = R(3,I) - R(3,J)
            D = T1*T1 + T2*T2 + T3*T3
            D = SQRT(D)
            DIJ(I,J) = D
            DIJ(J,I) = D
   10    CONTINUE
         DIJ(I,I) = 0.0
   20 CONTINUE
      DO 40 I = NATPRP,NATOMS
         N = 0
         ITYPI = ITYPE(I)
         DO 30 J = NATPRP,NATOMS
            IF(I .NE. J .AND. DIJ(I,J) .LT. RCUT(2,ITYPI)) THEN 
               N = N + 1
               ID(N,I) = J
            END IF
   30    CONTINUE
         NCUT(I) = N
   40 CONTINUE
      RETURN
      END
      SUBROUTINE SURRHO (Z, DIJ, RHOI, RHOP, ITYPI, NATOMS, NMAX, ID)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Compute contribution of atomic density for atom i to all atoms j in
C    its lookup table.
C                 a
C    RHOI(J) = rho (R  )
C                 i  ij
C                   a
C    RHOP(J) = d[rho (R  )]/dR
C                   i  ij     ij
C
      DIMENSION DIJ(NATOMS), RHOI(NATOMS), RHOP(NATOMS), ID(NATOMS)
C NTYMDM - maximum number of metal types
C NTYGDM - maximum number of non-metal types (gas types)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      PARAMETER (NMORDM=(NTYGDM*(NTYGDM+1))/2)
      COMMON /SCUTCM/ RCUT(2,NTYPDM), DCUT(NTYPDM)
      COMMON /SRHOCM/ CRI(10,NTYPDM), CI(10,NTYPDM), SI(10,NTYPDM),
     *   FACT(10,NTYPDM), RNSPAR(2,NTYPDM), NI(10,NTYPDM), NTS(NTYPDM),
     *   NTP(NTYPDM), NE(NTYPDM)
C
      DATA PI /3.1415926/
C
      RNS = RNSPAR(1,ITYPI)
      IF (ITYPI .LE. NTYMDM .AND. ABS(Z) .LT. 0.5) THEN
         RNS = RNSPAR(2,ITYPI)
      END IF
      RNP = FLOAT(NE(ITYPI)) - RNS
      RCUT1 = RCUT(1,ITYPI)
      RCUT2 = RCUT(2,ITYPI)
      DC = DCUT(ITYPI)
C
      DO 20 M = 1,NMAX
         J = ID(M)
         RIJ = DIJ(J)
         IF( RIJ .LE. RCUT1) THEN
           S = 1.0
           DS = 0.0
         ELSE
           RR1 = RIJ - RCUT1
           RR2 = RIJ - RCUT2
           S = RR2/DC + (SIN(PI*((RR1+RR2)/DC)))/(2.0*PI)
           DS = (1.0 + COS(PI*((RR1+RR2)/DC)))/DC
         ENDIF
         SUMS = 0.0
         DSUMS = 0.0
         IF (NTS(ITYPI).GT.0 .AND. RNS.NE.0.0) THEN
            DO 10 K = 1,NTS(ITYPI)
               NEXP = NI(K,ITYPI) - 1
               T = CRI(K,ITYPI)*RIJ**NEXP*EXP(-SI(K,ITYPI)*RIJ)
               DT = T*(FLOAT(NEXP)/RIJ - SI(K,ITYPI))
               SUMS = SUMS + T
               DSUMS = DSUMS + DT
  10        CONTINUE
         END IF
         SUMD = 0.0
         DSUMD = 0.0
         IF (NTP(ITYPI).GT.0 .AND. RNP.NE.0.0) THEN
            K = NTS(ITYPI)
            DO 15 KK = 1,NTP(ITYPI)
               K = K + 1
               NEXP = NI(K,ITYPI) - 1
               T = CRI(K,ITYPI)*RIJ**NEXP*EXP(-SI(K,ITYPI)*RIJ)
               DT = T*(FLOAT(NEXP)/RIJ - SI(K,ITYPI))
               SUMD = SUMD + T
               DSUMD = DSUMD + DT
  15        CONTINUE
         END IF
C
         RHOMT = (RNS*SUMS*SUMS + RNP*SUMD*SUMD)/(4.*PI)
         RHOI(J) =  RHOMT*S
         DRHODR = (RNS*SUMS*DSUMS + RNP*SUMD*DSUMD)/(2.*PI)
         RHOP(J) = DRHODR*S + DS*RHOMT
  20  CONTINUE
      RETURN
      END
      SUBROUTINE SURZIJ (R,ITYPE,JTYPE,PHI,DPHI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  Calculate the two body interactions and its derivative.
C
C   Z(R) = Z0*(1+BETA*R)*EXP(-ALPHA*R)*S
C   S(R) = (R-(RC+DC)/(-DC) - 1/2PI*SIN(PI*(2R-2RC-DC)/DC))  RC<R<RC+DC
C   Z0(1) for M, Z0(2) for H, etc.
C
C NTYMDM - maximum number of metal types
C NTYGDM - maximum number of non-metal types (gas types)
      PARAMETER (NTYMDM=1, NTYGDM=2, NTYPDM=NTYMDM+NTYGDM)
      PARAMETER (NMORDM=(NTYGDM*(NTYGDM+1))/2)
      COMMON /SCUTCM/ RCUT(2,NTYPDM), DCUT(NTYPDM)
      COMMON /SZRCM/ Z0(NTYPDM), BETA(NTYPDM), ALPHA(NTYPDM), CONV
      DIMENSION ZI(2), DZI(2), LTYP(2)
      DATA PI /3.1415926/
C
      LTYP(1) = ITYPE
      LTYP(2) = JTYPE
      DO 20 I = 1,2
         RCUT1 = RCUT(1,LTYP(I))
         RCUT2 = RCUT(2,LTYP(I))
         DC = DCUT(LTYP(I))
         IF( R .LE. RCUT1) THEN
           S = 1.0
           DS = 0.0
         ELSE
           RR1 = R - RCUT1
           RR2 = R - RCUT2
           S = RR2/DC + (SIN(PI*((RR1+RR2)/DC)))/(2.0*PI)
           DS = (1.0 + COS(PI*((RR1+RR2)/DC)))/DC
         ENDIF
         T1 = Z0(LTYP(I))*EXP(-ALPHA(LTYP(I))*R)
         ZI(I) = T1*(1.0 + BETA(LTYP(I))*R)
         DZI(I) = -ALPHA(LTYP(I))*ZI(I) + BETA(LTYP(I))*T1
         DZI(I) = DZI(I)*S + DS*ZI(I)
         ZI(I)  = ZI(I)*S
 20   CONTINUE
      PHI = ZI(1)*ZI(2)/R
      DZ2 = ZI(1)*DZI(2) + ZI(2)*DZI(1)
      DPHI = CONV*(DZ2 - PHI)/R
      PHI = CONV*PHI
      RETURN
      END
C Eispack routines
      SUBROUTINE REBAK(NM,N,B,DL,M,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER I,J,K,M,N,I1,II,NM
      DOUBLE PRECISION B(NM,N),DL(N),Z(NM,M)
      DOUBLE PRECISION X
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE REBAKA,
C     NUM. MATH. 11, 99-110(1968) BY MARTIN AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971).
C
C     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A GENERALIZED
C     SYMMETRIC EIGENSYSTEM BY BACK TRANSFORMING THOSE OF THE
C     DERIVED SYMMETRIC MATRIX DETERMINED BY  REDUC.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX SYSTEM,
C
C        B CONTAINS INFORMATION ABOUT THE SIMILARITY TRANSFORMATION
C          (CHOLESKY DECOMPOSITION) USED IN THE REDUCTION BY  REDUC
C          IN ITS STRICT LOWER TRIANGLE,
C
C        DL CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATION,
C
C        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED,
C
C        Z CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED
C          IN ITS FIRST M COLUMNS.
C
C     ON OUTPUT-
C
C        Z CONTAINS THE TRANSFORMED EIGENVECTORS
C          IN ITS FIRST M COLUMNS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      IF (M .EQ. 0) GO TO 200
C
      DO 100 J = 1, M
C     ********** FOR I=N STEP -1 UNTIL 1 DO -- **********
         DO 100 II = 1, N
            I = N + 1 - II
            I1 = I + 1
            X = Z(I,J)
            IF (I .EQ. N) GO TO 80
C
            DO 60 K = I1, N
   60       X = X - B(K,I) * Z(K,J)
C
   80       Z(I,J) = X / DL(I)
  100 CONTINUE
C
  200 RETURN
C     ********** LAST CARD OF REBAK **********
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE REDUC(NM,N,ISGN,A,B,DL,IERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER I,J,K,N,I1,J1,NM,NN,IERR
      DOUBLE PRECISION A(NM,N),B(NM,N),DL(N)
      DOUBLE PRECISION X,Y
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE REDUC1,
C     NUM. MATH. 11, 99-110(1968) BY MARTIN AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971).
C
C     THIS SUBROUTINE REDUCES THE GENERALIZED SYMMETRIC EIGENPROBLEM
C     AX=(LAMBDA)BX, WHERE B IS POSITIVE DEFINITE, TO THE STANDARD
C     SYMMETRIC EIGENPROBLEM USING THE CHOLESKY FACTORIZATION OF B.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRICES A AND B.  IF THE CHOLESKY
C          FACTOR L OF B IS ALREADY AVAILABLE, N SHOULD BE PREFIXED
C          WITH A MINUS SIGN,
C
C        A AND B CONTAIN THE REAL SYMMETRIC INPUT MATRICES.  ONLY THE
C          FULL UPPER TRIANGLES OF THE MATRICES NEED BE SUPPLIED.  IF
C          N IS NEGATIVE, THE STRICT LOWER TRIANGLE OF B CONTAINS,
C          INSTEAD, THE STRICT LOWER TRIANGLE OF ITS CHOLESKY FACTOR L,
C
C        DL CONTAINS, IF N IS NEGATIVE, THE DIAGONAL ELEMENTS OF L.
C
C     ON OUTPUT-
C
C        A CONTAINS IN ITS FULL LOWER TRIANGLE THE FULL LOWER TRIANGLE
C          OF THE SYMMETRIC MATRIX DERIVED FROM THE REDUCTION TO THE
C          STANDARD FORM.  THE STRICT UPPER TRIANGLE OF A IS UNALTERED,
C
C        B CONTAINS IN ITS STRICT LOWER TRIANGLE THE STRICT LOWER
C          TRIANGLE OF ITS CHOLESKY FACTOR L.  THE FULL UPPER
C          TRIANGLE OF B IS UNALTERED,
C
C        DL CONTAINS THE DIAGONAL ELEMENTS OF L,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          7*N+1      IF B IS NOT POSITIVE DEFINITE.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      IERR = 0
      NN = IABS(N)
      IF (ISGN .LT. 0) GO TO 100
C     ********** FORM L IN THE ARRAYS B AND DL **********
      DO 80 I = 1, N
         I1 = I - 1
C
         DO 80 J = I, N
            X = B(I,J)
            IF (I .EQ. 1) GO TO 40
C
            DO 20 K = 1, I1
   20       X = X - B(I,K) * B(J,K)
C
   40       IF (J .NE. I) GO TO 60
            IF (X .LE. 0.0E0) GO TO 1000
            Y = SQRT(X)
            DL(I) = Y
            GO TO 80
   60       B(J,I) = X / Y
   80 CONTINUE
C     ********** FORM THE TRANSPOSE OF THE UPPER TRIANGLE OF INV(L)*A
C                IN THE LOWER TRIANGLE OF THE ARRAY A **********
  100 DO 200 I = 1, NN
         I1 = I - 1
         Y = DL(I)
C
         DO 200 J = I, NN
            X = A(I,J)
            IF (I .EQ. 1) GO TO 180
C
            DO 160 K = 1, I1
  160       X = X - B(I,K) * A(J,K)
C
  180       A(J,I) = X / Y
  200 CONTINUE
C     ********** PRE-MULTIPLY BY INV(L) AND OVERWRITE **********
      DO 300 J = 1, NN
         J1 = J - 1
C
         DO 300 I = J, NN
            X = A(I,J)
            IF (I .EQ. J) GO TO 240
            I1 = I - 1
C
            DO 220 K = J, I1
  220       X = X - A(K,J) * B(I,K)
C
  240       IF (J .EQ. 1) GO TO 280
C
            DO 260 K = 1, J1
  260       X = X - A(J,K) * B(I,K)
C
  280       A(I,J) = X / DL(I)
  300 CONTINUE
C
      GO TO 1001
C     ********** SET ERROR -- B IS NOT POSITIVE DEFINITE **********
 1000 IERR = 7 * N + 1
 1001 RETURN
C     ********** LAST CARD OF REDUC **********
      END
      SUBROUTINE TQL2P(NM,N,D,E,Z,IERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER I,J,K,L,M,N,II,L1,NM,MML,IERR
      DOUBLE PRECISION D(N),E(N),Z(NM,N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,MACHEP
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2,
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND
C     WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD.
C     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
C     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
C     FULL MATRIX TO TRIDIAGONAL FORM.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY,
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
C          THE IDENTITY MATRIX.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1,2,...,IERR-1,
C
C        E HAS BEEN DESTROYED,
C
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C                For the VAX it is set to 2**(-55)
C                **********
C
      MACHEP = 2.0D0**(-46)
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E(I-1) = E(I)
C
      F = 0.0E0
      B = 0.0E0
      E(N) = 0.0E0
C
      DO 240 L = 1, N
         J = 0
         H = MACHEP * (ABS(D(L)) + ABS(E(L)))
         IF (B .LT. H) B = H
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********
         DO 110 M = L, N
            IF (ABS(E(M)) .LE. B) GO TO 120
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 220
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     ********** FORM SHIFT **********
         L1 = L + 1
         G = D(L)
         P = (D(L1) - G) / (2.0E0 * E(L))
         R = SQRT(P*P+1.0E0)
         D(L) = E(L) / (P + SIGN(R,P))
         H = G - D(L)
C
         DO 140 I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
C     ********** QL TRANSFORMATION **********
         P = D(M)
         C = 1.0E0
         S = 0.0E0
         MML = M - L
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
         DO 200 II = 1, MML
            I = M - II
            G = C * E(I)
            H = C * P
            IF (ABS(P) .LT. ABS(E(I))) GO TO 150
            C = E(I) / P
            R = SQRT(C*C+1.0E0)
            E(I+1) = S * P * R
            S = C / R
            C = 1.0E0 / R
            GO TO 160
  150       C = P / E(I)
            R = SQRT(C*C+1.0E0)
            E(I+1) = S * E(I) * R
            S = 1.0E0 / R
            C = C * S
  160       P = C * D(I) - S * G
            D(I+1) = H + S * (C * G + S * D(I))
C     ********** FORM VECTOR **********
            DO 180 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * H
               Z(K,I) = C * Z(K,I) - S * H
  180       CONTINUE
C
  200    CONTINUE
C
         E(L) = S * P
         D(L) = C * P
         IF (ABS(E(L)) .GT. B) GO TO 130
  220    D(L) = D(L) + F
  240 CONTINUE
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********
      DO 300 II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
         DO 260 J = II, N
            IF (D(J) .GE. P) GO TO 260
            K = J
            P = D(J)
  260    CONTINUE
C
         IF (K .EQ. I) GO TO 300
         D(K) = D(I)
         D(I) = P
C
         DO 280 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  280    CONTINUE
C
  300 CONTINUE
C
      GO TO 1001
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = L
 1001 RETURN
C     ********** LAST CARD OF TQL2 **********
      END
      SUBROUTINE TRED2(NM,N,A,D,E,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER I,J,K,L,N,II,NM,JP1
      DOUBLE PRECISION A(NM,N),D(N),E(N),Z(NM,N)
      DOUBLE PRECISION F,G,H,HH,SCALE
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED2,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX TO A
C     SYMMETRIC TRIDIAGONAL MATRIX USING AND ACCUMULATING
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C     ON OUTPUT-
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO,
C
C        Z CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX
C          PRODUCED IN THE REDUCTION,
C
C        A AND Z MAY COINCIDE.  IF DISTINCT, A IS UNALTERED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, N
C
         DO 100 J = 1, I
            Z(I,J) = A(I,J)
  100 CONTINUE
C
      IF (N .EQ. 1) GO TO 320
C     ********** FOR I=N STEP -1 UNTIL 2 DO -- **********
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 2) GO TO 130
C     ********** SCALE ROW (ALGOL TOL THEN NOT NEEDED) **********
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(Z(I,K))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = Z(I,L)
         GO TO 290
C
  140    DO 150 K = 1, L
            Z(I,K) = Z(I,K) / SCALE
            H = H + Z(I,K) * Z(I,K)
  150    CONTINUE
C
         F = Z(I,L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         Z(I,L) = F - G
         F = 0.0E0
C
         DO 240 J = 1, L
            Z(J,I) = Z(I,J) / H
            G = 0.0E0
C     ********** FORM ELEMENT OF A*U **********
            DO 180 K = 1, J
  180       G = G + Z(J,K) * Z(I,K)
C
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
  200       G = G + Z(K,J) * Z(I,K)
C     ********** FORM ELEMENT OF P **********
  220       E(J) = G / H
            F = F + E(J) * Z(I,J)
  240    CONTINUE
C
         HH = F / (H + H)
C     ********** FORM REDUCED A **********
         DO 260 J = 1, L
            F = Z(I,J)
            G = E(J) - HH * F
            E(J) = G
C
            DO 260 K = 1, J
               Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)
  260    CONTINUE
C
  290    D(I) = H
  300 CONTINUE
C
  320 D(1) = 0.0E0
      E(1) = 0.0E0
C     ********** ACCUMULATION OF TRANSFORMATION MATRICES **********
      DO 500 I = 1, N
         L = I - 1
         IF (D(I) .EQ. 0.0E0) GO TO 380
C
         DO 360 J = 1, L
            G = 0.0E0
C
            DO 340 K = 1, L
  340       G = G + Z(I,K) * Z(K,J)
C
            DO 360 K = 1, L
               Z(K,J) = Z(K,J) - G * Z(K,I)
  360    CONTINUE
C
  380    D(I) = Z(I,I)
         Z(I,I) = 1.0E0
         IF (L .LT. 1) GO TO 500
C
         DO 400 J = 1, L
            Z(I,J) = 0.0E0
            Z(J,I) = 0.0E0
  400    CONTINUE
C
  500 CONTINUE
C
      RETURN
C     ********** LAST CARD OF TRED2 **********
      END



C
C***********************************************************************
      PROGRAM HROTOR
C***********************************************************************
C
C This program is designed to calculate the reduced moment of inertia
C and the partition function of a hindered-rotation vibration mode by using
C the input position vector and vibration eigenvector. This program will take
C the position vector in cartesian coordinates and the eigenvector 
C in mass-scaled or cartesian coordinates, in the latter case the scaling mass 
C of the system must be provided.
C
C     Version 2.0, by Wei-Ping Hu, May 1994
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C     The following constants are copied from POLYRATE percon.inc
C
      PARAMETER(CAU=5.48593D-4,CKG=1.66024D-27,CKGM2=2.550946698D-4)
      PARAMETER(AUTOKG = CKG * CAU)
      PARAMETER(PI = 3.141592654D0)
      PARAMETER(NATOM=32,NTEMP=32)
      PARAMETER (AUTOCM = 2.19474627D+05) 
      PARAMETER (BK = 3.166830D-6) 
C
      DIMENSION EIGEN(NATOM,3),X(NATOM,3)
      DIMENSION V(2,3),U(2,3),VBOND(3)
      DIMENSION SUBGI(2,2), NSB(2), RMI(2)
      DIMENSION LGSRMI(10), ISB(2,NATOM), IGROUP(NATOM)
      DIMENSION AMASS (NATOM),CM(3), T(NTEMP), F(2,NTEMP)
      DIMENSION QHARM(NTEMP),QHIR(2,NTEMP),QFR(2,NTEMP)
C
C     EIGEN  -- eigenvector of the vibration mode
C     X      -- position vector
C     V, U   -- vector and unit vector of rotational axis 
C               the program calculate the axis of each subgroup
C               these axis should be parallel in a perfect hindered rotor
C     SUBGI  -- moment of inertia of subgroup calculated from 
C               rotation axis from both subgroup
C     RMI    -- reduced moment of inertia calculated from both axis
C     NSB    -- number of atoms in each subgroup
C     ISB    -- atom index in each subgroup
C     LGSRMI -- input options
C              LGSRMI(1) = number of atoms
C              LGSRMI(2) = unit for the position vector in cartesian
C                        = 0, bohr
C                        = 1, angstrom
C              LGSRMI(3) = coordinate system for the eigenvector
C                        = 0, mass scaled
C                        = 1, cartesians 
C              LGSRMI(4) = number of temperature to calculate partition function
C 
      CHARACTER*79 TITLE1,TITLE2
C
C     Calculate the conversion factor
C
      AUTOA = SQRT(CKGM2 * 1.0D-47 / AUTOKG) * 1.0D10
C
C     Read in options and titles
C
      WRITE(*,150)
      READ(*,*) (LGSRMI(I),I=1,4)
      WRITE(*,200) (LGSRMI(I),I=1,4)
      WRITE(*,*)'Run titles:'
      READ(*,'(A79)') TITLE1
      WRITE(*,*) TITLE1
      READ(*,'(A79)') TITLE2
      WRITE(*,*) TITLE2
C  
C     Read in atomic masses and the scaling mass
C
      IF (LGSRMI(3) .EQ. 0) THEN
         READ(*,*) SMASS
      ELSE
         SMASS = 1.0D0 
      ENDIF
      WRITE(*,210) SMASS
      READ(*,*) (AMASS(I),I=1,LGSRMI(1))
      WRITE(*,220) (I,AMASS(I),I=1,LGSRMI(1))
      TOTALM = 0.0D0
      DO 10 I = 1, LGSRMI(1)
         TOTALM = TOTALM + AMASS(I)
10    CONTINUE
C
C     Read in position vector
C
      IF (LGSRMI(2) .EQ. 0) THEN
         WRITE(*,300)
      ELSE
         WRITE(*,310)
      ENDIF
      READ(*,*) ((X(I,J),J=1,3),I=1,LGSRMI(1))
      WRITE(*,320) (I,(X(I,J),J=1,3),I=1,LGSRMI(1))
C
C     Find the center of mass and move it to the origin
C
      CM(1) = 0.0D0
      CM(2) = 0.0D0
      CM(3) = 0.0D0
      DO 15 I = 1, LGSRMI(1)
         CM(1) = CM(1) + AMASS(I) * X(I,1)
         CM(2) = CM(2) + AMASS(I) * X(I,2)
         CM(3) = CM(3) + AMASS(I) * X(I,3)
15    CONTINUE
      CM(1) = CM(1) / TOTALM
      CM(2) = CM(2) / TOTALM
      CM(3) = CM(3) / TOTALM
      WRITE(*,*) 'Center of Mass: ',CM(1),CM(2),CM(3)
      DO 20 I = 1, LGSRMI(1)
         DO 20 J = 1, 3
            X(I,J) = X(I,J) - CM(J)
20    CONTINUE 
      WRITE(*,*) 'after putting origin at center of mass'    
      WRITE(*,320) (I,(X(I,J),J=1,3),I=1,LGSRMI(1))
C
C     Read in eigenvector
C
      IF (LGSRMI(3) .EQ. 0) THEN
         WRITE(*,400)
      ELSE
         WRITE(*,410)
      ENDIF
      READ(*,*) ((EIGEN(I,J),J=1,3),I=1,LGSRMI(1))
      WRITE(*,320) (I,(EIGEN(I,J),J=1,3),I=1,LGSRMI(1))
C    
C     Read in symmetry and subgroup
C
      READ(*,*) SIGMA,NSB(1),(ISB(1,I),I=1,NSB(1))
      NSB(2) = LGSRMI(1) - NSB(1)
C
C     Find the atom index of the other subgroup
C
      DO 22 I = 1, LGSRMI(1)
         IGROUP(I) = 0
22    CONTINUE
      DO 23 I = 1, NSB(1)
         J = ISB(1,I)
         IGROUP(J) = 1
23    CONTINUE
      J = 0
      DO 24 I = 1, LGSRMI(1)
        IF (IGROUP(I) .EQ. 0) THEN
           J = J + 1
           ISB(2,J) = I
        ENDIF
24    CONTINUE
      WRITE(*,500) SIGMA,(ISB(1,I),I=1,NSB(1))
      WRITE(*,510) (ISB(2,I),I=1,NSB(2))
C
C     Read the bond that separates the two subgroups
C
      READ(*,*) IBOND1, IBOND2
      WRITE(*,520) IBOND1,IBOND2 
C 
C     Read in frequency and temperatures if needed
C
      IF (LGSRMI(4) .GT. 0) THEN
         READ(*,*) FREQ
         WRITE(*,550) FREQ
         READ(*,*) (T(I),I=1,LGSRMI(4))
         WRITE(*,560) LGSRMI(4),(T(I),I=1,LGSRMI(4))
      ENDIF
C
C     Convert the mass to atomic unit
C  
      DO 30 I = 1, LGSRMI(1)
         AMASS(I) = AMASS(I) / CAU
30    CONTINUE
      SMASS = SMASS / CAU
C
C
C     Convert the position vector in bohrs
C
      IF (LGSRMI(2) .EQ. 0) THEN
         FACTOR = 1.0D0
      ELSE
         FACTOR = 1.0D0 / AUTOA
      ENDIF
      DO 40 I = 1, LGSRMI(1)
         DO 40 J = 1, 3
            X(I,J) =  X(I,J) * FACTOR
40    CONTINUE
C
C     Convert the eigenvector to unscaled cartesian system if needed
C
      IF (LGSRMI(3) .EQ. 0) THEN
         DO 50 I = 1, LGSRMI(1)
            SCALE = SQRT(AMASS(I) / SMASS)
            DO 50 J = 1, 3
               EIGEN(I,J) = EIGEN(I,J) / SCALE
50       CONTINUE
      ENDIF
C
C     Convert the frequency to a.u.
C
      FREQ = FREQ / AUTOCM
C
C     Now, everything is in unscaled cartesian coordinate 
C     and in atomic units.
C
C     Calculate the rotational axis from both subgroup
C
      DO 70 I = 1, 2
         V(I,1) = 0.0D0
         V(I,2) = 0.0D0
         V(I,3) = 0.0D0
         DO 60 J = 1, NSB(I)
            IATOM = ISB(I,J)
            FMASS = AMASS(IATOM)
            V(I,1)= V(I,1) + X(IATOM,2)*EIGEN(IATOM,3)*FMASS 
     *                     - X(IATOM,3)*EIGEN(IATOM,2)*FMASS 
            V(I,2)= V(I,2) + X(IATOM,3)*EIGEN(IATOM,1)*FMASS
     *                     - X(IATOM,1)*EIGEN(IATOM,3)*FMASS 
            V(I,3)= V(I,3) + X(IATOM,1)*EIGEN(IATOM,2)*FMASS
     *                     - X(IATOM,2)*EIGEN(IATOM,1)*FMASS 
60       CONTINUE
         FACTOR = SQRT(V(I,1)**2+V(I,2)**2+V(I,3)**2)
C        WRITE(*,*) '|V| = ',FACTOR
         U(I,1) = V(I,1) / FACTOR           
         U(I,2) = V(I,2) / FACTOR           
         U(I,3) = V(I,3) / FACTOR           
70    CONTINUE
C
C     Calculate the moment of inertia of the subgroups
C     Use the rotation axis calculated from both subgroup 
C
      DO 90 I = 1, 2
         SUBGI(1,I) = 0.0D0
         SUBGI(2,I) = 0.0D0
         DO 90 J = 1, NSB(I)
            IATOM = ISB(I,J)
            FMASS = AMASS(IATOM)
            TEMP1 = X(IATOM,1)**2.0D0 + X(IATOM,2)**2.0D0 +
     *              X(IATOM,3)**2.0D0
            TEMP2 = X(IATOM,1)*U(1,1)+X(IATOM,2)*U(1,2)+
     *                                X(IATOM,3)*U(1,3)
            TEMP3 = X(IATOM,1)*U(2,1)+X(IATOM,2)*U(2,2)+
     *                                X(IATOM,3)*U(2,3)
            SUBGI(1,I) = SUBGI(1,I) + (TEMP1 - TEMP2**2.0D0) * FMASS
            SUBGI(2,I) = SUBGI(2,I) + (TEMP1 - TEMP3**2.0D0) * FMASS
90    CONTINUE
C
C     Calculate the reduced moment of inertia and the angel between the
C     two rotational axis
C
      RMI(1) = SUBGI(1,1)*SUBGI(1,2)/SIGMA**2/(SUBGI(1,1)+SUBGI(1,2))
      RMI(2) = SUBGI(2,1)*SUBGI(2,2)/SIGMA**2/(SUBGI(2,1)+SUBGI(2,2))
      DOT = U(1,1)*U(2,1)+U(1,2)*U(2,2)+U(1,3)*U(2,3)
      IF (ABS(DOT) .GE. 1.0D0) DOT = SIGN(1.0D0,DOT)
      ANGEL = ACOS(DOT)
C
      VLENG = 0.0D0
      DO 95 I = 1,3
         VBOND(I) = X(IBOND1,I) - X(IBOND2,I)
         VLENG = VLENG + VBOND(I) ** 2.0D0
95    CONTINUE
      VLENG = SQRT(VLENG) 
      DO 96 I = 1,3
         VBOND(I) = VBOND(I) / VLENG
96    CONTINUE
C
      DOT = U(1,1)*VBOND(1) + U(1,2)*VBOND(2) + U(1,3)*VBOND(3)
      IF (ABS(DOT) .GE. 1.0D0) DOT = SIGN(1.0D0,DOT)
      ANGEL1 = ACOS(DOT)
      DOT = U(2,1)*VBOND(1) + U(2,2)*VBOND(2) + U(2,3)*VBOND(3)
      IF (ABS(DOT) .GE. 1.0D0) DOT = SIGN(1.0D0,DOT)
      ANGEL2 = ACOS(DOT)
C
C     WRITE(*,*) SUBGI(1,1),SUBGI(1,2)
C     WRITE(*,*) SUBGI(2,1),SUBGI(2,2)
      WRITE(*,600) RMI(1), RMI(1)*CKGM2, RMI(2), RMI(2)*CKGM2
      WRITE(*,620) ANGEL*180.0D0/PI
      WRITE(*,630) ANGEL1*180.0D0/PI
      WRITE(*,640) ANGEL2*180.0D0/PI
      IF (LGSRMI(4) .LE. 0) THEN
         WRITE(*,1000)
         STOP
      ENDIF
C
C     Calculate harmonic and hindered-rotor partition functions
C
      E = 0.5D0 * FREQ
      DO 110 I = 1, LGSRMI(4)   
         BETA = 1.0D0 / BK / T(I)
         SUM  = EXP(-E*BETA)
         QHARM(I) = SUM / ( 1.0D0 - SUM**2 )
         QFR(1,I) = SQRT(2.0D0*PI*RMI(1)/BETA) 
         QFR(2,I) = SQRT(2.0D0*PI*RMI(2)/BETA)
         F(1,I) = TANH(QFR(1,I)*BETA*FREQ)
         F(2,I) = TANH(QFR(2,I)*BETA*FREQ)
         QHIR(1,I) = QHARM(I) * F(1,I)
         QHIR(2,I) = QHARM(I) * F(2,I)
110   CONTINUE
      WRITE(*,800)
      WRITE(*,810) (T(I),QHARM(I),QFR(1,I),QFR(2,I),
     *                   QHIR(1,I),QHIR(2,I),I=1,LGSRMI(4))
C
      WRITE(*,1000)
C
      STOP
C
150   FORMAT(/16X,
     1'************************************************',/16X,
     2'*       HROTOR PROGRAM Version 2.0, 1994       *',/16X,
     3'*                                              *',/16X,
     4'* Calculation of the reduced moment of inertia *',/16X,
     5'* and partition function of a hindered-rotor   *',/16X,
     6'************************************************')
200   FORMAT(/1X,'LGSRMI options: ',10I3)
210   FORMAT(/1X,'The scaling mass is ',F10.4,' a.m.u.')
220   FORMAT(/,(1X,'Atom ',I2,' has a mass of ',F10.6,' a.m.u.'))
300   FORMAT(/1X,'The geometry is read in bohrs.')
310   FORMAT(/1X,'The geometry is read in angstroms.')
320   FORMAT(/1X,'  Atom  ','      X     ','      Y     ',
     *                      '      Z     ',/,(1X,I5,3X,3F12.6)) 
400   FORMAT(/1X,'The eigenvector is read in mass-scaled coordinate.')
410   FORMAT(/1X,'The eigenvector is read in cartesian coordinate.')
500   FORMAT(/1X,'The hindered-rotor has a symmetry number ',F6.2,'.',
     *       /1X,'The first  subgroup consists of atoms ',(10I3))
510   FORMAT( 1X,'The second subgroup consists of atoms ',(10I3))
520   FORMAT(/1X,'The bond that separates the two subgroup is between ',
     *           'atoms ',I2,' and ',I2,'.')  
550   FORMAT(/1X,'The harmonic frequency is ',F10.2,' cm**-1.')
560   FORMAT(/1X,'The partition functions will be calculated at ',
     *            I2,' temperatures.',/1X,'They are (in kelvin):',
     *            (/1X,8F10.2))
600   FORMAT(/1X,'The calculated reduced moment of inertia are:',/1X,
     * '(1) using the calculated rotational axis of subgroup 1:',/1X,
     * F12.4,' a.u. or ',F12.4, ' 10E-47 kg m**2',/1X,
     * '(2) using the calculated rotational axis of subgroup 2:',/1X,
     * F12.4,' a.u. or ',F12.4, ' 10E-47 kg m**2')
620   FORMAT(/1X,'The angel between these two axes is ',F8.2,' deg.')
630   FORMAT(/1X,'The angel between the bond and U1 is ',F8.2,' deg.')
640   FORMAT(/1X,'The angel between the bond and U2 is ',F8.2,' deg.')
700   FORMAT(/1X,3F10.4)
800   FORMAT(/1X,'In the following output:',/1X,
     *'QHARM   is harmonic partition function',/1X,
     *'QFR(1)  is free rotor partition function using axis 1',/1X,
     *'QFR(2)  is free rotor partition function using axis 2',/1X,
     *'QHIR(1) is hindered-rotor partition function using axis 1',/1X,
     *'QHIR(2) is hindered-rotor partition function using axis 2')
810   FORMAT(/1X,'   T(K) ','         QHARM','        QFR(1)',
     *     '        QFR(2)','       QHIR(1)','       QHIR(2)',/,
     * (1X,F8.2,5F14.6))
1000  FORMAT(/1X,28('*'),' END OF RMI OUTPUT ',28('*'))
C
      END 
C
          

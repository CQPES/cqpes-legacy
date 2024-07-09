C
C**********************************************************************
C  DETMI
C**********************************************************************
C
      PROGRAM DETMI
C
C  This program calculates the determinant of the moments of inertia.
C
C  This is an utility program in the POLYRATE 5.1 distribution package.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(CAU=5.48593D-4,CKG=1.66024D-27,CKGM2=2.550946698D-4)
      PARAMETER(AUTOKG = CKG * CAU)
      PARAMETER(NATOM=32)
C
      DIMENSION  CM(3),AMASS(NATOM),POS(NATOM,3),FMI(3,3),
     *           R2(NATOM),LGSMI(10)
      CHARACTER*79  TITLE1,TITLE2
C
C  Open the data and the output file
C
      OPEN (51,FILE='detmi.dat',STATUS='OLD',FORM='FORMATTED')
      OPEN (52,FILE='detmi.out',STATUS='UNKNOWN',FORM='FORMATTED')
C
C  Read in options and data
C
      READ(51,*) (LGSMI(I),I=1,3)
      READ(51, '(A79)') TITLE1
      READ(51, '(A79)') TITLE2
      DO 5 I = 1, LGSMI(1)
         READ(51,*) AMASS(I),(POS(I,J),J=1,3)
5     CONTINUE
C
C  Close data file
C
      CLOSE(51)
C
C  Calculate the conversion factor
C
      AUTOA = SQRT(CKGM2 * 1.0D-47 / AUTOKG) * 1.0D10
C
C  Convert coordinates to atomic unit (a.u.), calculate the total AMASS
C
      TM = 0.0
      IF (LGSMI(3) .EQ. 0) THEN
         FACTOR = 1.0
      ELSE
         FACTOR = AUTOA
      ENDIF
      DO 20 I = 1, LGSMI(1)
         DO 10 J = 1,3
            POS(I,J) = POS(I,J) / FACTOR 
10       CONTINUE
         TM = TM + AMASS(I)
20    CONTINUE
C
C  Calculate the center of MASS
C
      DO 40 I = 1, 3
         CM(I) = 0.0
         DO 30 J = 1, LGSMI(1)
            CM(I) = CM(I) + AMASS(J) * POS(J,I)
30       CONTINUE
         CM(I) = CM(I) / TM
40    CONTINUE
C 
C  Print information to output file
C
      WRITE(52,200)
      WRITE(52,210)
      WRITE(52,300)
      WRITE(52,200)
      WRITE(52,320)
      WRITE(52,330) TITLE1, TITLE2
      WRITE(52,340) LGSMI(1)
      IF (LGSMI(2) .EQ. 1) THEN 
         WRITE(52,345) 'linear.   '
      ELSE 
         WRITE(52,345) 'nonlinear.'
      ENDIF   
      WRITE(52,350)
      WRITE(52,400)
      WRITE(52,450) (I,AMASS(I),(POS(I,J)*AUTOA,J=1,3),I=1,LGSMI(1))
      WRITE(52,460) (CM(I)*AUTOA,I=1,3)
      WRITE(52,360)
      WRITE(52,400)
      WRITE(52,450) (I,AMASS(I),(POS(I,J),J=1,3),I=1,LGSMI(1))
      WRITE(52,460) (CM(I),I=1,3)
C
C  Convert AMASS to a.u.
C
      DO 50 I = 1, LGSMI(1)
         AMASS(I) = AMASS(I) / CAU
50    CONTINUE
C
      IF (LGSMI(2) .EQ. 1) THEN
C
C    Linear geometry
C
         DO 60 I = 1, LGSMI(1)
            R2(I) = 0.0
            DO 60 J = 1, 3
               R2(I) = R2(I) + (POS(I,J) - CM(J)) ** 2
60       CONTINUE
         DETI = 0.0
         DO 70 I = 1, LGSMI(1)
            DETI = DETI + AMASS(I) * R2(I)
70       CONTINUE
C 
         WRITE(52,500) DETI, DETI * CKGM2 * 1.0D-47,
     *                 DETI * CKGM2 * 1.0D-40, DETI * CKGM2,
     *                 DETI * CAU
C
      ELSE
C
C     Non-linear geometry
C
         FMI(1,1) = 0.0
         DO 80 I = 1, LGSMI(1)
            FMI(1,1) = FMI(1,1) + AMASS(I) *
     *               ((POS(I,2)-CM(2))**2 + (POS(I,3)-CM(3))**2)
80       CONTINUE
         FMI(2,2) = 0.0
         DO 85 I = 1, LGSMI(1)
            FMI(2,2) = FMI(2,2) + AMASS(I) *
     *               ((POS(I,1)-CM(1))**2 + (POS(I,3)-CM(3))**2)
85       CONTINUE
         FMI(3,3) = 0.0
         DO 90 I = 1, LGSMI(1)
            FMI(3,3) = FMI(3,3) + AMASS(I) *
     *               ((POS(I,1)-CM(1))**2 + (POS(I,2)-CM(2))**2)
90       CONTINUE
         FMI(1,2) = 0.0
         DO 95 I = 1, LGSMI(1)
            FMI(1,2) = FMI(1,2) + AMASS(I) *
     *                (POS(I,1)-CM(1)) * (POS(I,2)-CM(2)) 
95       CONTINUE
         FMI(1,3) = 0.0
         DO 100 I = 1, LGSMI(1)
            FMI(1,3) = FMI(1,3) + AMASS(I) *
     *                (POS(I,1)-CM(1)) * (POS(I,3) - CM(3))
100      CONTINUE
         FMI(2,3) = 0.0
         DO 110 I = 1, LGSMI(1)
            FMI(2,3) = FMI(2,3) + AMASS(I) *
     *                (POS(I,2)-CM(2)) * (POS(I,3) - CM(3))
110      CONTINUE
C
         DETI =     FMI(1,1) * FMI(2,2) * FMI(3,3) + 
     *          2 * FMI(1,2) * FMI(2,3) * FMI(1,3) +
     *              FMI(1,3) * FMI(1,3) * FMI(2,2) -
     *              FMI(2,3) * FMI(2,3) * FMI(1,1) -
     *              FMI(1,2) * FMI(1,2) * FMI(3,3)
C
         WRITE(52,510) DETI,DETI * CKGM2**3 * 1.0D-141,
     *                 DETI * CKGM2**3 * 1.0D-120, DETI * CKGM2**3,
     *                 DETI * CAU**3
      ENDIF
C
      WRITE(52,600)
C
      CLOSE(52)
C 
200   FORMAT(/,70('*'))
210   FORMAT(/,10X,'DETMI  Ver. 1.1,  by Wei-Ping Hu, December, 1993')
300   FORMAT(/,10X,'Calculate the determinant of moments of inertia',
     *        /10X,'with geometry in cartesian coordinates.')
320   FORMAT(/1X,'Run Titles:',/)
330   FORMAT(1X,A79) 
340   FORMAT(/1X,'There are ',I2,' atoms.')
345   FORMAT( 1X,'Geometry is ',A10)
350   FORMAT(//18X,'*** Input geometry in Angstroms ***')
360   FORMAT(/18X, '*** Input geometry in bohrs ***')
400   FORMAT(/5X,'Atom','  Mass (a.m.u.)',6X,'X',11X,'Y',11X,'Z',/) 
450   FORMAT(6X,I2,F16.6,3F12.6)
460   FORMAT(/6X,'XCM =',F12.6,5X,'YCM =',F12.6,5X,'ZCM =',F12.6) 
500   FORMAT(//6X,'Moment of inertia = ',1P,E15.6,' a.u.',
     *        /6X,'                    ',   E15.6,' kg m**2',
     *        /6X,'                    ',   E15.6,' g cm**2',
     *        /6X,'                    ',   E15.6,' 10**-40 g cm**2',
     *        /6X,'                    ',   E15.6,' a.m.u. bohr**2')
510   FORMAT(//6X,'Det I = ',1P,E15.6,' a.u.',
     *        /6X,'        ',   E15.6,' kg**3 m**6',
     *        /6X,'        ',   E15.6,' g**3 cm**6',
     *        /6X,'        ',   E15.6,' 10**-120 g**3 cm**6',
     *        /6X,'        ',   E15.6,' a.m.u.**3 bohr**6')
600   FORMAT(/1X,24('*'),' END OF DETMI OUTPUT ',23('*'))
C
      END
                    

      PROGRAM FINDB
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      WRITE(*,*) 
      WRITE(*,*)'*********************'
      WRITE(*,*)'* PROGRAM TO FIND B *'
      WRITE(*,*)'*********************'
      WRITE(*,*) 
C 
      WRITE(*,*)'Input the energy in (kcal/mol) of the saddle point'
      READ(*,*) BARR
C
      WRITE(*,*)'Input the s (in bohrs) and the energy ',
     *          '(in kcal/mol) of the stationary point:'
      READ(*,*) SW, VW
C
      WRITE(*,*) 'Enter the additional point, s (in bohrs) and ',
     *           'VMEP(s) (in kcal/mol):'
      READ(*,*) S,V
C
      C = VW
      ERROLD = 1.0D20
      DO 10 IL = 1,50000
         B = IL * 0.01
         A = (BARR-VW) * EXP(B)
C        WRITE(*,*) A,B,C,S0,RANGE
         ERR =  V - COG(A,B,C,SW,S) 
         IF (ABS(ERR) .LT. ERROLD) THEN
            ERROLD=ABS(ERR)
            TEMPA = A
            TEMPB = B
         ENDIF 
10    CONTINUE
      A = TEMPA
      B = TEMPB
      WRITE(*,50) A,C,SW
      WRITE(*,100) B
      VALUE = COG(A,B,C,SW,S)
      WRITE(*,200) VALUE, V - VALUE
C
 50   FORMAT(1X,'A = ',E10.4,' kcal/mol, ','C = ',F10.6,' kcal/mol', 
     *          ', SW = ',F10.4,' bohr.')
100   FORMAT(1X,'The closest B to meet the requirement is ',F8.3)
200   FORMAT(1X,'The cut-off gaussian function is ',F9.4,
     *          ' and the error is ',F9.4,' kcal/mol.')
C
      END
C
C**********************************************************************
C  COG
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION cog(A,B,C,S0,SMEP)
C
C     CALLED BY:
C               ZOCVCL, ZOCFRE
C
C     THIS FUNCTION RETURNS THE VALUE OF A CUT-OFF GAUSSIAN AT SMEP
C     B MUST BE POSITIVE!
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER( EXPLIM = 600.0 )
C
      IF ( ABS(SMEP) .GE. ABS(S0) ) THEN
         COG = C
      ELSE
         X  = -B / (1.0D0 - (SMEP/S0) ** 2.0D0)
         IF (X .LT. -EXPLIM) THEN
            COG = C
         ELSE
            COG = A * EXP(X) + C
         ENDIF
      ENDIF     
C
      RETURN
C
      END
C     

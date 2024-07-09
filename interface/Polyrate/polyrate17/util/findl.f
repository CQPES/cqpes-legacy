      PROGRAM FINDL
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      WRITE(*,*)
      WRITE(*,*)'*********************'
      WRITE(*,*)'* PROGRAM TO FIND L *'
      WRITE(*,*)'*********************'
      WRITE(*,*)'Input the energy of the three points (in kcal/mol): '
      WRITE(*,*)'(In the order of reactant-side complex, saddle point,'
      WRITE(*,*)' and product-side complex)'
      READ(*,*) V1,V0,V2 
C
      WRITE(*,*) 'Enter the additional point, s (in bohrs) and ',
     *           'VMEP(s) (in kcal/mol) :'
C  
      READ(*,*) S,V
C
      A = V2 - V1
      C = V1
      B = (2.0*V0-A-2.0*C)+ 2.0 * ((V0-C)*(V0-A-C))**0.5
      ERROLD = 1000.0
      DO 10 IL = 1,1000
         RANGE= IL * 0.01
         S0 = -RANGE * LOG((A+B)/(B-A))
C        WRITE(*,*) A,B,C,S0,RANGE
         ERR =  V - ECKART(A,B,C,S0,RANGE,S) 
         IF (ABS(ERR) .LT. ERROLD) THEN
            ERROLD=ABS(ERR)
            TEMPL = RANGE
            TEMPS0 = S0
         ENDIF 
10    CONTINUE
      RANGE = TEMPL
      S0 = TEMPS0
      WRITE(*,50) A,B,C,S0
      WRITE(*,100) RANGE
      VALUE = ECKART(A,B,C,S0,RANGE,S)
      WRITE(*,200) VALUE, V - VALUE
C
 50   FORMAT(1X,'A = ',F10.6,/1X,'B = ',F10.6,/1X,'C = ',F10.6, 
     *      /1X,'S0 = ',F10.6)
100   FORMAT(1X,'The closest L to meet the requirement is',F8.3,
     *          ' bohr.')
200   FORMAT(1X,'The Eckart function is ',F10.6,' and the error is ',
     * F10.6,' kcal/mol.')
C
      END
C     
C**********************************************************************
C  ECKART
C**********************************************************************
C
      DOUBLE PRECISION FUNCTION eckart(A,B,C,S0,L,SMEP)
C
C     CALLED BY:
C               ZOCVCL, ZOCFRE
C
C     THIS FUNCTION RETURN THE VALUE OF AN ECKART FUNCTION AT SMEP
C 
C                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION L
C
      PARAMETER(EXPLIM = 600.0)
C
      X  = (SMEP - S0) / L
      IF (X .GT. EXPLIM) THEN
         ECKART = A + C
      ELSE IF (X .LT. -EXPLIM) THEN
         ECKART = C
      ELSE
         Y = EXP(X)
         ECKART = A*Y / (1.D0+Y) + B*Y / (1.D0+Y) ** 2.D0 + C
      ENDIF
C
      RETURN
C
      END

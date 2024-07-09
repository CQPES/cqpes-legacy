      SUBROUTINE DGEDI(A,LDA,N,IPVT,DET,WORK,JOB)                       
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(LDA,*),DET(2),WORK(*),IPVT(*)                         
C                                                                       
C     DGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX            
C     USING THE FACTORS COMPUTED BY DGECO OR DGEFA.                     
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                THE OUTPUT FROM DGECO OR DGEFA.                        
C                                                                       
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .                
C                                                                       
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C        IPVT    INTEGER(N)                                             
C                THE PIVOT VECTOR FROM DGECO OR DGEFA.                  
C                                                                       
C        WORK    DOUBLE PRECISION(N)                                    
C                WORK VECTOR.  CONTENTS DESTROYED.                      
C                                                                       
C        JOB     INTEGER                                                
C                = 11   BOTH DETERMINANT AND INVERSE.                   
C                = 01   INVERSE ONLY.                                   
C                = 10   DETERMINANT ONLY.                               
C                                                                       
C     ON RETURN                                                         
C                                                                       
C        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.               
C                OTHERWISE UNCHANGED.                                   
C                                                                       
C        DET     DOUBLE PRECISION(2)                                    
C                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.           
C                OTHERWISE NOT REFERENCED.                              
C                DETERMINANT = DET(1) * 10.0**DET(2)                    
C                WITH  1.0 .LE. ABS(DET(1)) .LT. 10.0                   
C                OR  DET(1) .EQ. 0.0 .                                  
C                                                                       
C     ERROR CONDITION                                                   
C                                                                       
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS     
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.           
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY      
C        AND IF DGECO HAS SET RCOND .GT. 0.0 OR DGEFA HAS SET           
C        INFO .EQ. 0 .                                                  
C                                                                       
C     LINPACK. THIS VERSION DATED 08/14/78 .                            
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     BLAS DAXPY,DSCAL,DSWAP                                            
C     FORTRAN ABS,MOD                                                   
C                                                                       
C     COMPUTE DETERMINANT                                               
C                                                                       
      IF (JOB/10 .EQ. 0) GO TO 70                                       
         DET(1) = 1.0D+00                                               
         DET(2) = 0.0D+00                                               
         TEN = 10.0D+00                                                 
         DO 50 I = 1, N                                                 
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)                        
            DET(1) = A(I,I)*DET(1)                                      
C        ...EXIT                                                        
            IF (DET(1) .EQ. 0.0D+00) GO TO 60                           
   10       IF (DABS(DET(1)) .GE. 1.0D+00) GO TO 20                      
               DET(1) = TEN*DET(1)                                      
               DET(2) = DET(2) - 1.0D+00                                
            GO TO 10                                                    
   20       CONTINUE                                                    
   30       IF (DABS(DET(1)) .LT. TEN) GO TO 40                          
               DET(1) = DET(1)/TEN                                      
               DET(2) = DET(2) + 1.0D+00                                
            GO TO 30                                                    
   40       CONTINUE                                                    
   50    CONTINUE                                                       
   60    CONTINUE                                                       
   70 CONTINUE                                                          
C                                                                       
C     COMPUTE INVERSE(U)                                                
C                                                                       
      IF (MOD(JOB,10) .EQ. 0) GO TO 150                                 
         DO 100 K = 1, N                                                
            A(K,K) = 1.0D+00/A(K,K)                                     
            T = -A(K,K)                                                 
            CALL DSCAL(K-1,T,A(1,K),1)                                  
            KP1 = K + 1                                                 
            IF (N .LT. KP1) GO TO 90                                    
            DO 80 J = KP1, N                                            
               T = A(K,J)                                               
               A(K,J) = 0.0D+00                                         
               CALL DAXPY(K,T,A(1,K),1,A(1,J),1)                        
   80       CONTINUE                                                    
   90       CONTINUE                                                    
  100    CONTINUE                                                       
C                                                                       
C        FORM INVERSE(U)*INVERSE(L)                                     
C                                                                       
         NM1 = N - 1                                                    
         IF (NM1 .LT. 1) GO TO 140                                      
         DO 130 KB = 1, NM1                                             
            K = N - KB                                                  
            KP1 = K + 1                                                 
            DO 110 I = KP1, N                                           
               WORK(I) = A(I,K)                                         
               A(I,K) = 0.0D+00                                         
  110       CONTINUE                                                    
            DO 120 J = KP1, N                                           
               T = WORK(J)                                              
               CALL DAXPY(N,T,A(1,J),1,A(1,K),1)                        
  120       CONTINUE                                                    
            L = IPVT(K)                                                 
            IF (L .NE. K) CALL DSWAP(N,A(1,K),1,A(1,L),1)               
  130    CONTINUE                                                       
  140    CONTINUE                                                       
  150 CONTINUE                                                          
      RETURN                                                            
      END SUBROUTINE DGEDI
                                                           
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)                               
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(LDA,*),IPVT(*)                                        
C                                                                       
C     DGEFA FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION.  
C                                                                       
C     DGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED            
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.          
C     (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA) .                   
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                THE MATRIX TO BE FACTORED.                             
C                                                                       
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .                
C                                                                       
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C     ON RETURN                                                         
C                                                                       
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS         
C                WHICH WERE USED TO OBTAIN IT.                          
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE       
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER          
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.       
C                                                                       
C        IPVT    INTEGER(N)                                             
C                AN INTEGER VECTOR OF PIVOT INDICES.                    
C                                                                       
C        INFO    INTEGER                                                
C                = 0  NORMAL VALUE.                                     
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR       
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES        
C                     INDICATE THAT DGESL OR DGEDI WILL DIVIDE BY ZERO  
C                     IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE   
C                     INDICATION OF SINGULARITY.                        
C                                                                       
C     LINPACK. THIS VERSION DATED 08/14/78 .                            
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     BLAS DAXPY,DSCAL,IDAMAX                                           
C                                                                       
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING                        
C                                                                       
      INFO = 0                                                          
      NM1 = N - 1                                                       
      IF (NM1 .LT. 1) GO TO 70                                          
      DO 60 K = 1, NM1                                                  
         KP1 = K + 1                                                    
C                                                                       
C        FIND L = PIVOT INDEX                                           
C                                                                       
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1                             
         IPVT(K) = L                                                    
C                                                                       
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED          
C                                                                       
         IF (A(L,K) .EQ. 0.0D+00) GO TO 40                              
C                                                                       
C           INTERCHANGE IF NECESSARY                                    
C                                                                       
            IF (L .EQ. K) GO TO 10                                      
               T = A(L,K)                                               
               A(L,K) = A(K,K)                                          
               A(K,K) = T                                               
   10       CONTINUE                                                    
C                                                                       
C           COMPUTE MULTIPLIERS                                         
C                                                                       
            T = -1.0D+00/A(K,K)                                         
            CALL DSCAL(N-K,T,A(K+1,K),1)                                
C                                                                       
C           ROW ELIMINATION WITH COLUMN INDEXING                        
C                                                                       
            DO 30 J = KP1, N                                            
               T = A(L,J)                                               
               IF (L .EQ. K) GO TO 20                                   
                  A(L,J) = A(K,J)                                       
                  A(K,J) = T                                            
   20          CONTINUE                                                 
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)                  
   30       CONTINUE                                                    
         GO TO 50                                                       
   40    CONTINUE                                                       
            INFO = K                                                    
   50    CONTINUE                                                       
   60 CONTINUE                                                          
   70 CONTINUE                                                          
      IPVT(N) = N                                                       
      IF (A(N,N) .EQ. 0.0D+00) INFO = N                                 
      RETURN                                                            
      END SUBROUTINE DGEFA
                                                            
      LOGICAL FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine (version 1.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
*
*     RETURN
*
*     End of LSAME
*
      END FUNCTION LSAME

      SUBROUTINE RSP(A,N,MATZ,W,Z)
      use perconparam
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(*),  W(N), Z(N,N)
*******************************************************************
*
*   EISPACK DIAGONALIZATION ROUTINES: TO FIND THE EIGENVALUES AND
*           EIGENVECTORS (IF DESIRED) OF A REAL SYMMETRIC PACKED MATRIX.
* ON INPUT-      N  IS THE ORDER OF THE MATRIX  A,
*                A  CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
*                   PACKED MATRIX STORED ROW-WISE,
*             MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF ONLY
*                   EIGENVALUES ARE DESIRED,  OTHERWISE IT IS SET TO
*                   ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND
*                   EIGENVECTORS.
* ON OUTPUT-     W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER,
*                Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO,
*
*******************************************************************
* THIS SUBROUTINE WAS CHOSEN AS BEING THE MOST RELIABLE. (JJPS)
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C FV and FV2 are scratch arrays which shouldo be dimensioned to the
C to the order of the biggest diagonalizable matrix
c
      DIMENSION FV1(N3TM),FV2(N3TM)
      SAVE
      NV=(N3TM*(N3TM+1))/2 
      NM=N
C
20    call tred3(n,nv,a,w,fv1,fv2)
      IF (MATZ .NE. 0) GO TO 30
C     ********** FIND EIGENVALUES ONLY **********
      call tqlrat(n,w,fv2,ierr)
      GO TO 60
C     ********** FIND BOTH EIGENVALUES AND EIGENVECTORS **********
   30 DO 50    I = 1, N
C
         DO 40    J = 1, N
            Z(J,I)=0.0D0
   40    CONTINUE
C
         Z(I,I)=1.0D0
   50 CONTINUE
C
      call tql2(nm,n,w,fv1,z,ierr)
      IF (IERR .NE. 0) GO TO 70
      call trbak3(nm,n,nv,a,n,z,ierr)                                   IR0494
   60 return
C     ********** LAST CARD OF RSP **********
   70 write(6,'( "Error",I3," in RSP" )') ierr                          IR0494
      RETURN
      END SUBROUTINE RSP

      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end subroutine daxpy

      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end function ddot

      SUBROUTINE  dgemm (TRANSA,TRANSB,M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMM .
*
      END SUBROUTINE dgemm 

      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end subroutine  dscal

      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end subroutine  dswap

      integer function idamax(n,dx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c
      double precision dx(*),dmax
      integer i,incx,ix,n
c
      idamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end  function idamax

      SUBROUTINE tql2 (NM,N,D,E,Z,IERR)
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION D(N),E(N),Z(NM,N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,MACHEP
C
C     REAL SQRT,ABS,SIGN
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
C                **********
C
      DATA ZERO,ONE,TWO / 0.0D0,1.0D0,2.0D0 /
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
      MACHEP = TWO**(-46)
C
      IERR = 0
      IF (N.EQ.1) GO TO 160
C
      DO 10 I = 2, N
         E(I-1) = E(I)
   10 CONTINUE
C
      F = ZERO
      B = ZERO
      E(N) = ZERO
C
      DO 110 L = 1, N
         J = 0
         H = MACHEP*(DABS(D(L))+DABS(E(L)))
         IF (B.LT.H) B = H
C
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********
C
         DO 20 M = L, N
            IF (DABS(E(M)).LE.B) GO TO 30
C
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
   20    CONTINUE
C
C
   30    IF (M.EQ.L) GO TO 100
   40    IF (J.EQ.30) GO TO 150
         J = J+1
C
C     ********** FORM SHIFT **********
C
         L1 = L+1
         G = D(L)
         P = (D(L1)-G)/(TWO*E(L))
C
C        R = DSQRT(P*P+ONE)
C
         IF (DABS(P).GT.1.0D+16) THEN
            R = DABS(P)
         ELSE
            R = DSQRT(P*P+ONE)
         ENDIF
         D(L) = E(L)/(P+DSIGN(R,P))
         H = G-D(L)
C
         DO 50 I = L1, N
            D(I) = D(I)-H
   50    CONTINUE
C
         F = F+H
C
C     ********** QL TRANSFORMATION **********
C
         P = D(M)
         C = ONE
         S = ZERO
         MML = M-L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
         DO 90 II = 1, MML
            I = M-II
            G = C*E(I)
            H = C*P
            IF (DABS(P).LT.DABS(E(I))) GO TO 60
            C = E(I)/P
            R = DSQRT(C*C+ONE)
            E(I+1) = S*P*R
            S = C/R
            C = ONE/R
            GO TO 70
   60       C = P/E(I)
            R = DSQRT(C*C+ONE)
            E(I+1) = S*E(I)*R
            S = ONE/R
            C = C*S
   70       P = C*D(I)-S*G
            D(I+1) = H+S*(C*G+S*D(I))
C
C     ********** FORM VECTOR **********
C
            DO 80 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S*Z(K,I)+C*H
               Z(K,I) = C*Z(K,I)-S*H
   80       CONTINUE
C
   90    CONTINUE
C
         E(L) = S*P
         D(L) = C*P
         IF (DABS(E(L)).GT.B) GO TO 40
  100    D(L) = D(L)+F
  110 CONTINUE
C
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********
C
      DO 140 II = 2, N
         I = II-1
         K = I
         P = D(I)
C
         DO 120 J = II, N
            IF (D(J).GE.P) GO TO 120
            K = J
            P = D(J)
  120    CONTINUE
C
         IF (K.EQ.I) GO TO 140
         D(K) = D(I)
         D(I) = P
C
         DO 130 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  130    CONTINUE
C
  140 CONTINUE
C
      GO TO 160
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
  150 IERR = L
  160 RETURN
C
C     ********** LAST CARD OF TQL2 **********
C
      END SUBROUTINE tql2 

      SUBROUTINE tqlrat (N,D,E2,IERR)
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION D(N),E2(N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,MACHEP
C
C     REAL SQRT,ABS,SIGN
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT,
C     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
C     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.
C
C     ON INPUT-
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE
C          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
C          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
C          THE SMALLEST EIGENVALUES,
C
C        E2 HAS BEEN DESTROYED,
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
C                **********
C
      DATA ZERO,ONE,TWO / 0.0D0,1.0D0,2.0D0 /
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
      MACHEP = TWO**(-46)
C
      IERR = 0
      IF (N.EQ.1) GO TO 140
C
      DO 10 I = 2, N
         E2(I-1) = E2(I)
   10 CONTINUE
C
      F = ZERO
      B = ZERO
      E2(N) = ZERO
C
      DO 120 L = 1, N
         J = 0
         H = MACHEP*(DABS(D(L))+DSQRT(E2(L)))
         IF (B.GT.H) GO TO 20
         B = H
         C = B*B
C
C     ********** LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT **********
C
   20    DO 30 M = L, N
            IF (E2(M).LE.C) GO TO 40
C
C     ********** E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
   30    CONTINUE
C
   40    IF (M.EQ.L) GO TO 80
   50    IF (J.EQ.30) GO TO 130
         J = J+1
C
C     ********** FORM SHIFT **********
C
         L1 = L+1
         S = DSQRT(E2(L))
         G = D(L)
         P = (D(L1)-G)/(TWO*S)
         R = DSQRT(P*P+ONE)
         D(L) = S/(P+DSIGN(R,P))
         H = G-D(L)
C
         DO 60 I = L1, N
            D(I) = D(I)-H
   60    CONTINUE
C
         F = F+H
C
C     ********** RATIONAL QL TRANSFORMATION **********
C
         G = D(M)
         IF (G.EQ.ZERO) G = B
         H = G
         S = ZERO
         MML = M-L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
         DO 70 II = 1, MML
            I = M-II
            P = G*H
            R = P+E2(I)
            E2(I+1) = S*R
            S = E2(I)/R
            D(I+1) = H+S*(H+D(I))
            G = D(I)-E2(I)/G
            IF (G.EQ.ZERO) G = B
            H = G*P/R
   70    CONTINUE
C
         E2(L) = S*G
         D(L) = H
C
C     ********** GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST **********
C
         IF (H.EQ.ZERO) GO TO 80
         IF (DABS(E2(L)).LE.DABS(C/H)) GO TO 80
         E2(L) = H*E2(L)
         IF (E2(L).NE.ZERO) GO TO 50
   80    P = D(L)+F
C
C     ********** ORDER EIGENVALUES **********
C
         IF (L.EQ.1) GO TO 100
C
C     ********** FOR I=L STEP -1 UNTIL 2 DO -- **********
C
         DO 90 II = 2, L
            I = L+2-II
            IF (P.GE.D(I-1)) GO TO 110
            D(I) = D(I-1)
   90    CONTINUE
C
  100    I = 1
  110    D(I) = P
  120 CONTINUE
C
      GO TO 140
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
  130 IERR = L
  140 RETURN
C
C     ********** LAST CARD OF TQLRAT **********
C
      END SUBROUTINE tqlrat

      SUBROUTINE trbak3 (NM,N,NV,A,M,Z,IERR)
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(NV),Z(NM,M)
      DOUBLE PRECISION H,S
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRBAK3,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL SYMMETRIC
C     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
C     SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  TRED3.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A
C          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT,
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANSFORMATIONS
C          USED IN THE REDUCTION BY  TRED3  IN ITS FIRST
C          N*(N+1)/2 POSITIONS,
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
C     NOTE THAT TRBAK3 PRESERVES VECTOR EUCLIDEAN NORMS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DATA ZERO / 0.0D0 /
      IF (M.EQ.0) GO TO 60
      IF (N.EQ.1) GO TO 60
C
      DO 50 I = 2, N
         L = I-1
         IZ = (I*L)/2
         IK = IZ+I
         H = A(IK)
         IF (H.EQ.ZERO) GO TO 50
C
         DO 40 J = 1, M
            S = ZERO
            IK = IZ
C
            DO 10 K = 1, L
               IK = IK+1
               S = S+A(IK)*Z(K,J)
   10       CONTINUE
C
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
C
            S = S/H
            IK = IZ
C
            DO 30 K = 1, L
               IK = IK+1
               IF (DABS(H).GT.1.0D0) GO TO 20
               SA = S*A(IK)
               IF (DABS(SA).LT.DABS(H)*1.6D+38) GO TO 20
               IERR = 1111
               RETURN
   20          CONTINUE
               Z(K,J) = Z(K,J)-S*A(IK)/H
   30       CONTINUE
C
   40    CONTINUE
C
   50 CONTINUE
C
   60 RETURN
C
C     ********** LAST CARD OF TRBAK3 **********
C
      END SUBROUTINE trbak3

      SUBROUTINE tred3 (N,NV,A,D,E,E2)
C
C     CALLED BY:
C                 RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(NV),D(N),E(N),E2(N)
      DOUBLE PRECISION F,G,H,HH,SCALE
C
C     REAL SQRT,ABS,SIGN
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED3,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX, STORED AS
C     A ONE-DIMENSIONAL ARRAY, TO A SYMMETRIC TRIDIAGONAL MATRIX
C     USING ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT-
C
C        N IS THE ORDER OF THE MATRIX,
C
C        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A
C          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT,
C
C        A CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
C          INPUT MATRIX, STORED ROW-WISE AS A ONE-DIMENSIONAL
C          ARRAY, IN ITS FIRST N*(N+1)/2 POSITIONS.
C
C     ON OUTPUT-
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL
C          TRANSFORMATIONS USED IN THE REDUCTION,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO,
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C     ********** FOR I=N STEP -1 UNTIL 1 DO -- **********
C
      scale=0.d00
      DATA ZERO / 0.0D0 /
      DO 90 II = 1, N
         I = N+1-II
         L = I-1
         IZ = (I*L)/2
         H = ZERO
         SCALE = ZERO
         IF (L.LT.1) GO TO 20
C
C     ********** SCALE ROW (ALGOL TOL THEN NOT NEEDED) **********
C
         DO 10 K = 1, L
            IZ = IZ+1
            D(K) = A(IZ)
            sc = DABS(D(K))
            SCALE = SCALE + sc
   10    CONTINUE
C
         IF (SCALE.NE.ZERO)GO TO 30
   20    E(I) = ZERO
         E2(I) = ZERO
         GO TO 80
C
   30    DO 40 K = 1, L
            D(K) = D(K)/SCALE
            hh=D(K)*D(K)
            H = H+ hh
   40    CONTINUE
C
         E2(I) = SCALE*SCALE*H
         F = D(L)
         G = -DSIGN(DSQRT(H),F)
         E(I) = SCALE*G
         H = H-F*G
         D(L) = F-G
         A(IZ) = SCALE*D(L)
         IF (L.EQ.1) GO TO 80
         F = ZERO
C
         DO 60 J = 1, L
            G = ZERO
            JK = (J*(J-1))/2
C
C     ********** FORM ELEMENT OF A*U **********
C
            DO 50 K = 1, L
               JK = JK+1
               IF (K.GT.J) JK = JK+K-2
               gg=A(JK)*D(K)
               G = G + gg
   50       CONTINUE
C
C     ********** FORM ELEMENT OF P **********
C
            E(J) = G/H
            ff = E(J)*D(J)
            F = F + ff
   60    CONTINUE
C
         HH = F/(H+H)
         JK = 0
C
C     ********** FORM REDUCED A **********
C
         DO 70 J = 1, L
            F = D(J)
            G = E(J)-HH*F
            E(J) = G
C
            DO 70 K = 1, J
               JK = JK+1
               ak = -F*E(K)-G*D(K)
               A(JK) = A(JK) + ak
   70    CONTINUE
C
   80    D(I) = A(IZ+1)
         A(IZ+1) = SCALE*DSQRT(H)
   90 CONTINUE
C
      RETURN

C
C     ********** LAST CARD OF TRED3 **********
C
      END SUBROUTINE tred3

      SUBROUTINE xerbla( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP 'XERBLA 1'
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END SUBROUTINE xerbla

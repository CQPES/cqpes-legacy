        PROGRAM reord30
C       
C       This program takes the fu30 output file of a testrun using the
C       WRITEFU30 option and reorders the points so that they follow the
C       required order (from the more negative to the more positive 
C       value of S). Further modifications in the file are required before
C       it can runs in a Polyrate calculation: Indexes of frequencies, and
C       information for extrapolation.
C
C       This code is not optimized, it is highly memory demanding and not
C       as fast as it can be.
C      
C       4/1/97 JC
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C 
C       Since the maximum number of points in fu30 is 400, we will
C       dimensionate everything up to 400. For each point, we will assume
C       that no more than 400 lines of information are required.
C       Therefore, the matrix POINT requires about 13 Mb
C      
        CHARACTER*80 POINT(400,400)
        CHARACTER*80 LINE
        CHARACTER*12 BEG
        CHARACTER*4 PNT
        DIMENSION S(400)
C
        OPEN (UNIT=30, STATUS='OLD', FILE='poly.fu30')
        OPEN (UNIT=90, STATUS='NEW', FILE='poly.fu90')
C
        BEG='BEGIN POINTS'
        PNT='S,V='
        INP=0
        INL=0
C
C       We will read the fu30 file line by line and we will write it in the 
C       output file until we met the title for starting the information on 
C       the reaction path, 'BEGIN POINTS ALONG REACTION COORDINATE'
C
 1      READ (30,2,END=11) LINE
        DO 5 INIPOS=1,3
           IF (LINE(INIPOS:INIPOS+11).NE.BEG) THEN
              IF (INIPOS.EQ.3) WRITE (90,2) LINE
              GOTO 5
           ELSE
              WRITE (90,2) LINE
              READ (30,*) NPTS
              WRITE (90,'(I3)') NPTS
              GOTO 21
           ENDIF
 5      CONTINUE
        GOTO 1
C
C       Now it has writen everything up to the reaction path and knows the
C       number of points to be readed. Now it will look for 'S,V=' at the
C       header of every point and store all the information about the point
C       in the POINT array
C
21      READ (30,2,END=31) LINE
        DO 15 INIPOS=1,3
            IF (LINE(INIPOS:INIPOS+11).NE.PNT) THEN
              IF (INIPOS.EQ.3) THEN
                    INL=INL+1
                    POINT(INP,INL)=LINE
              ENDIF
              GOTO 15
           ELSE
C
C       We will check that all the points have the same nuber of lines
C
              IF (ISAVENL.EQ.0) THEN
                          ISAVENL=INL
              ELSE
                  IF (ISAVENL.NE.INL) GOTO 11
              ENDIF
C
              INL=1
              INP=INP+1
              POINT(INP,INL)=LINE
              GOTO 21
           ENDIF
15      CONTINUE
        GOTO 21
C
C       Now all the information is stored in the array POINT. In the second
C       line we have the value of S. We will recover it using the CFLOAT
C       function (in interface.f)
C
31      DO I=1,INP
            S(I)=cfloat(POINT(I,2))
        ENDDO
C
C       And now, we look for the minor value of S and write the information
C       about that point on the file fu90.
C       The value of S in that point is set to 1.D10, this way we ensure it
C       will not appear again.
C       This part can be improved to make it faster.
C
        DO J=1,INP
          LOWINP=1
C
          DO I=2,INP
             IF (S(LOWINP).GT.S(I)) LOWINP=I
          ENDDO
C
          S(LOWINP)=1.D10
C
          DO INLIN=1,ISAVENL
             WRITE (90,2) POINT(LOWINP,INLIN)
          ENDDO
C
        ENDDO
C
        CLOSE (30)
        CLOSE (90)
C
        STOP
C
11      WRITE (90,*) 'Error reading the poly.fu30 file'
        STOP
C
 2      FORMAT (A80)
C
        END
C
C**********************************************************************
C CFLOAT
C**********************************************************************
C
      FUNCTION cfloat(STRING)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C JC  INCLUDE 'param.inc'
C
      CHARACTER*80 STRING,NUMBER
      CHARACTER CH
      LOGICAL LEXP,LDEC
C
      LEXP = .FALSE.
      LDEC = .FALSE.
      LENGTH = LEN(STRING)
      IF (LENGTH .EQ. 0) THEN
         CFLOAT = 0.0D0
         RETURN
      ENDIF
C     WRITE(FU6,*) LENGTH,STRING
C
C     Find the first nonblank character
C
      I = 1
10    IF (STRING(I:I) .EQ. ' ' .AND. I .LE. LENGTH) THEN
         I = I + 1
         GOTO 10
      ENDIF
C
C     If it is a blank string set function to zero
C
      IF (I .GT. LENGTH) THEN
         CFLOAT = 0.0D0
         RETURN
      ENDIF
      IBEG = I
C
C     Find the first blank character after the number
C
      I = IBEG+1
20    IF (STRING(I:I) .NE. ' ' .AND. I .LE. LENGTH) THEN
         I = I + 1
         GOTO 20
      ENDIF
      IEND = I-1
C
C     Stripe the blanks before and after the number
C
      NUMBER = STRING(IBEG:IEND)
      LENGTH = IEND - IBEG + 1
C
C     Make sure there is no blank left
C
      IF (INDEX(NUMBER,' ') .LE. LENGTH) THEN
C JC     WRITE(FU6,1000) STRING
         STOP 'CFLOAT 1'
      ENDIF
C
C     Find the decimal point
C
      IDEC = INDEX(NUMBER,'.')
      IF (IDEC .NE. 0) LDEC = .TRUE.
C
C     Find the exponential symbol
C
      IUE = INDEX(NUMBER,'E')
      ILE = INDEX(NUMBER,'e')
      IUD = INDEX(NUMBER,'D')
      ILD = INDEX(NUMBER,'d')
      ISUM = IUE + ILE + IUD + ILD
      IEXP = MAX0(IUE,ILE,IUD,ILD)
      IF (ISUM .GT. IEXP) THEN
C JC     WRITE(FU6,1000) STRING
         STOP 'CFLOAT 2'
      ENDIF
      IF (IEXP .NE. 0) THEN
         LEXP = .TRUE.
      ELSE
         IEXP = LENGTH + 1
      ENDIF
C
      IF (.NOT. LDEC) IDEC = IEXP
C
C     Get the number before decimal
C
      IBEG = 2
      IF (NUMBER(1:1) .EQ. '+') THEN
         SIGN = 1.0D0
      ELSEIF(NUMBER(1:1) .EQ. '-') THEN
         SIGN = -1.0D0
      ELSE
         SIGN = 1.0D0
         IBEG = 1
      ENDIF
      IF (IBEG .EQ. IEXP) THEN
         F1 = 1.0D0
      ELSE
         F1 = 0.0D0
      ENDIF
      DO 50 I = IBEG,IDEC-1
         CH = NUMBER(I:I)
         IF (CH .GE. '0' .AND. CH .LE. '9') THEN
            N = ICHAR(CH) - ICHAR('0')
            F1 = F1 * 10.0D0 + DBLE(N)
         ELSE
C JC        WRITE(FU6,1000) STRING
            STOP 'CFLOAT 3'
         ENDIF
50    CONTINUE
C
C     Get the number after decimal
C
      F2 = 0.0D0
      IF (LDEC) THEN
         J = 0
         DO 60 I = IDEC+1,IEXP-1
            CH = NUMBER(I:I)
            IF (CH .GE. '0' .AND. CH .LE. '9') THEN
               N = ICHAR(CH) - ICHAR('0')
               F2 = F2 * 10.0D0 + DBLE(N)
               J = J + 1
            ELSE
C JC           WRITE(FU6,1000) STRING
               STOP 'CFLOAT 4'
            ENDIF
60       CONTINUE
         F2 = F2 / 10.0D0 ** DBLE(J)
      ENDIF
C
C    Get the exponent
C
      ESIGN = 1.0D0
      F3 = 0.0D0
      IF (LEXP) THEN
         IBEG = IEXP + 2
         IF (NUMBER(IEXP+1:IEXP+1) .EQ. '+') THEN
            ESIGN = 1.0D0
         ELSEIF(NUMBER(IEXP+1:IEXP+1) .EQ. '-') THEN
            ESIGN = -1.0D0
         ELSE
            ESIGN = 1.0D0
            IBEG = IEXP + 1
         ENDIF
         DO 70 I = IBEG,LENGTH
            CH = NUMBER(I:I)
            IF (CH .GE. '0' .AND. CH .LE. '9') THEN
               N = ICHAR(CH) - ICHAR('0')
               F3 = F3 * 10.0D0 + DBLE(N)
            ELSE
C JC           WRITE(FU6,1000) STRING
               STOP 'CFLOAT 5'
            ENDIF
70       CONTINUE
      ENDIF
C
      CFLOAT = (SIGN * (F1 + F2)) * 10.0D0 ** (ESIGN*F3)
C
      RETURN
C
1000  FORMAT(/1X,'Illegal number: ',A80)
C
      END
c

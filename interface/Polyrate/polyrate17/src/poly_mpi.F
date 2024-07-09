      SUBROUTINE POLY_MPI_WAITFORJOB(irank,isize)
!     use common_inc
      use perconparam
      use kintcm
!     include 'param.inc' 
!     include 'percon.inc'
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'
      INTEGER status(MPI_STATUS_SIZE)
      LOGICAL DONE
      DIMENSION X(N3TM)
C
C
      DONE=.FALSE.
C
9999  if (.NOT.DONE) then
C
       call MPI_RECV(imess,1,MPI_INTEGER, 0,
     +              1, MPI_COMM_WORLD, status, ierr)
C
c  if kill signal is being sent
       if (imess .EQ. 0) DONE=.TRUE.
C
c  if system info is being sent
       if (imess .EQ. 1) then
         CALL RECIEVE_COMMON                                            0101BL05
         IF (IPOT.EQ.0)CALL SETUP(N3TM)
       endif
C
c  if request for energy is being sent
       if (imess .EQ. 2) then
         call RUN_EHOOK(1)
       endif
C
c  if request for gradient is being sent
       if (imess .EQ. 3) then
         call RUN_GHOOK(1)
       endif
C
c  if request for hessian is being sent
      if (imess .EQ. 4) then
      CALL MPI_RECV(NQFREQ,1,MPI_INTEGER, 0, 2, 
     +                        MPI_COMM_WORLD, status, ierr)
      CALL MPI_RECV(X, N3TM, MPI_DOUBLE_PRECISION, 0,3,
     +                        MPI_COMM_WORLD, status, ierr)
C
      CALL RUN_HHOOK(X,NQFREQ)
      endif
      goto 9999
      endif
C      
      RETURN
      END

     
      SUBROUTINE FIRE_ALL_WORKERS(ierr)      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'
      call MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)
      do 10 i=1,isize-1
10      call MPI_SEND(0, 1, MPI_INTEGER, i,
     +                          1, MPI_COMM_WORLD, ierr)         
      RETURN
      END

      
      SUBROUTINE NEXT_AVAIL_PROC(LBUSY,IPROC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'
      LOGICAL LBUSY(512), FOUND
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)
      FOUND=.FALSE.
      I=1
      IF (.NOT.FOUND) THEN        
        IF (LBUSY(I)) GOTO 9999
        I=I+1
      ENDIF
9999  CONTINUE                                                          0103BL05
      IPROC=I      
      RETURN
      END


      SUBROUTINE UPDATE_STATUS(LBUSY)                                   0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'               
      INTEGER status(MPI_STATUS_SIZE)      
      LOGICAL LBUSY(512), FLAG
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)
      IZTAG=1
      DO 10 I=1,ISIZE-1                                                 0103BL05
        CALL MPI_IPROBE(I,IZTAG,MPI_COMM_WORLD,FLAG,status,ierr)
        LBUSY(I)=(.NOT.FLAG)                                            0103BL05        
10    CONTINUE
      RETURN
      END


      SUBROUTINE FREQ_SUBMIT(X,NEXTTAG,IPROC,STEPX,JRUN,NJRUN)          0202BL05
      use perconparam
      use common_inc
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'                                                  
!     include 'param.inc' 
!     include 'percon.inc'      
      DIMENSION JRUN(512)                                               0202BL05
!     DIMENSION X(N3TM)                                                 0210BL05
C  add one to NJRUN and add this tag to the JRUN list      
      NJRUN=NJRUN+1
      JRUN(NJRUN)=NEXTTAG
      ITO=IPROC
C
C send the message to do a freq calc 
C
      CALL MPI_SEND(4, 1, MPI_INTEGER, ITO,1,
     +                         MPI_COMM_WORLD, ierr)
C send the tag for the return message
      CALL MPI_SEND(NEXTTAG,1,MPI_INTEGER,ITO,2,
     +                         MPI_COMM_WORLD, ierr)
C sned the current geometry
      CALL MPI_SEND(X,N3TM, MPI_DOUBLE_PRECISION, ITO,3, 
     +                        MPI_COMM_WORLD, ierr)
C      
      NEXTTAG=NEXTTAG+1                                                 0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05


      SUBROUTINE MULTI_NORMOD(IPATH_FREQ,NPATH_FREQ,NEXTTAG,LBUSY,
     +                        JRUN,NJRUN,GEOMM,DERIVM,LSAVES)           0103BL05
      use perconparam
      use common_inc
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LBUSY(512)                                                0103BL05
      include 'mpif.h'                                                  0212BL05
!     include 'param.inc'
!     include 'percon.inc'
!     include 'common.inc'
      SAVE
      DIMENSION JRUN(512)
      DIMENSION DXXP(N3TM),XMSCD(N3TM)
      DIMENSION DXSAV(N3TM)                                             0601YC98
      DIMENSION ANCO(N3TM,N3TM),SUME(9)                                 0601YC98
      DIMENSION GEOMM(N3TM,NSDM)                                        0212BL05
      DIMENSION LSAVES(NSDM)                                            0212BL05
      DIMENSION DERIVM(N3TM,NSDM)                                       0212BL05
      DIMENSION IPATH_FREQ(NSDM)
      LOGICAL ISSAD                                                     0317YC99
      INTEGER STATUS(MPI_STATUS_SIZE)
C
C
C     
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     COMPUTES NORMAL MODE FREQUENCIES AND DIRECTIONS
C     AT MASS-WEIGHTED POINT X(N3TM)
C     FOR IOP=1, ONLY NORMALIZED GRAD(V) COMPUTED
C     FOR IOP=2, ALSO GET NORMAL MODES
C     FOR IOP=3, PROJECT IS CALLED  ????????
C     FOR IOP=-1 TO -4, GET NORMAL MODES FOR REACTANTS OR PRODUCTS
C     FOR IOP.GT.0, IND(I)=I
C
C     IOP = 7,8 (IOP = 2,3 for TS)                                      0725YC97
C
C     Include statements were added 6/20/91
C
C     CALLED BY:
C                POLYAT,MAIN,PATH
C     CALLS:   
C            TRANS,DERIV2,FIRST,DERV24,FDIAG,CORTRM,GRADDR,ANCOEF,
C            ANHARM,ZEROPT,NOROUT,CHKFRE
C
C
      DO IFR=1,NPATH_FREQ
C      
      CALL MPI_RECV(F,N3TM*N3TM,MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,
     +               IPATH_FREQ(IFR),MPI_COMM_WORLD, status, ierr)
C
C
      LSAVE=LSAVES(IFR)
      DO I=1,N3TM
        X(I)=GEOMM(I,IFR)
        DX(I)=DERIVM(I,IFR)
      ENDDO
C      
C IOP is 3 for the reaction path
      IOP=3 
      JOP=3
C
      LPTBCR = LGS2(15)
      KOP = ABS(JOP)
      NEND = N3
      EGRNDT = 0.D0
      H2 = 2.0D0*DLX
C
      IF (.TRUE.) THEN
C  
C     SET FLAG FOR DESIRED METHOD OF DETERMINING 2ND DERIV ALONG RXN PAT
C
      NDERIV = 0
      IF (LGS(25).EQ.2) NDERIV = 1
C
C 100 ********************
      IF (JOP.LT.3 .OR. (LGS(31) .NE. 5 .AND. LGS(31) .NE. 35)) THEN    7/1/91VM
C     
C     FOR A POINT ALONG THE REACTION COORDINATE (JOP=3) THE HESSIAN AND 5/23B88
C     GRADIENT ARE ALREADY COMPUTED IF THE PAGE-MCIVER INTEGRATION      5/23B88
C     PROCEDURE IS USED.                                                5/23B88
C     
C     TRANSFORM POINT TO CARTESIAN COORDINATES
C     
            DO 5 I = 1, N3TM                                            1106YL92
               XMSCD(I) = X(I)                                              ..
5           CONTINUE                                                    1106YL92
            CALL TRANS (2,N3,AMASS,X,DX)                                9/18YL92
C
C     NOW COMPUTE THE SECOND DERIVATIVE FOR ALL OPTIONS EXCEPT JOP = 1
C     AND CONVERT TO MASS-WEIGHTED COORDS
C
         IF (JOP.NE.1) THEN
           DO 10 I = 1, N3
               EFNDTP(I) = 0.0D0
   10      CONTINUE
C
C           call hhook(X,F,DLX,NEND,0,1,0)                               0202BL05 
C
          DO 63 I = 1, NEND
             DO 63 J = 1, NEND
                FSP(J,I) = F(J,I)
63        CONTINUE
C
C FOR MORATE                                                            6/13T89
C PROJECT OUT THE TRANSLATIONAL AND ROTATIONAL CONTAIMINATIONS          6/13T89
C FOR JOP = 3, IT IS DONE EXPLICITLY IN ROUTINE PROJCT                  6/13T89
C                                                                       6/13T89
C            IF (potnam.eq.'mopac'.OR. LGS2(2) .NE. 0) THEN
c            IF (isup.eq.1.or.lgs2(2).ne.0) then                        0327YC97
c
c only nosupermol can project out RT of reactants and products
c and well and ts can be projected or not
c
         ENDIF
C 100 ********************


C
C 200 ********************
         IF (LGS(30).LT.0) THEN                                         9/6YL91 
C                                                                          ..
C Put F into temp. storage for writing out later                           ..
C                                                                          ..
            DO 100 I = 1, NEND                                             ..   
               DO 100 J = 1, NEND                                          ..   
                  FSV(I,J) = F(I,J)                                        ..   
  100       CONTINUE                                                       ..   
         ENDIF                                                          9/6YL91 
C
C 200 ********************


C 300 ********************
         IF (JOP.GE.0) THEN
C
C     COMPUTE NORMALIZED GRAD(V) AND
C     RESTORE X TO MASS-WEIGHTED COORDS
C
             CALL TRANS(1,N3,AMASS,X,DX)
            DO 65 I = 1, N3TM                                           1106YL92
               X(I) = XMSCD(I)                                          1106YL92
65          CONTINUE                                                    1106YL92
            DXMAX = 0.0D0
            DO 70 I = 1, N3
C
C     DXXP WILL STORE THE UNNORMALIZED DERIVATIVES SO THEY CAN BE PRINTED
C     OUT LATER WITH THE OPTIONAL OUTPUT.
C     
               DXXP(I) = DX(I)
               T = ABS(DX(I))
C 310 ********
               IF (T.GT.DXMAX) THEN
                  DXMAX = T
               ENDIF
C 310 ********
   70       CONTINUE
C 320 ********
            IF (DXMAX.EQ.0.0D0) THEN
               WRITE (FU6,1000) (DX(I),I=1,N3)
            ELSE
               DXNORM = 0.0D0
               DO 80 I = 1, N3
                  DX(I) = DX(I)/DXMAX
                  DXNORM = DXNORM + DX(I)*DX(I)
   80          CONTINUE
               DXNORM = SQRT(DXNORM)
               DO 90 I = 1, N3
                  DX(I) = DX(I)/DXNORM
   90          CONTINUE
               DXMAG = DXNORM*DXMAX
            ENDIF
C 320 ********
         ENDIF
C 300 ********************
C
C THE UNNORMALIZED DERIVATIVES IS STORED AT DXP                         1110DL89
C FOR THE PAGE-MCIVER INTEGRATION                                       1110DL89
C
      ELSEIF (LGS(31).EQ.5.OR.LGS(31).EQ.35) THEN                       9/6YL91
        DO 110 I = 1, N3                                                1110DL89
        DXXP(I) = DX(I)*DXMAG
  110   CONTINUE                                                        1110DL89
      ENDIF                                                             1110DL89
      ELSE                                                              0725YC97
        DO I = 1,N3                                                     0725YC97
         DO J = 1,N3                                                    0725YC97
           F(I,J)=TEMHES(I,J)/(AMASS(IND(I))*AMASS(IND(J)))             0725YC97
           FSP(I,J) = F(I,J)                                            0725YC97
           FSV(I,J) = F(I,J)                                            0725YC97
         ENDDO                                                          0725YC97
        ENDDO                                                           0725YC97
      ENDIF                                                             0725YC97
C
C 400 ***********************************************
C OPTIONally diagonalize F
C
         JOP = IOP                                                      0311YC98
C 400 ***********************************************

C       scale frequencies and/or call some routines for RODS
C
        CALL MON(IOP,JOP,ISSAD) 
c
         IF (iclf.eq.1)
     >      CALL REPFL (IOP,NEND,S,BARRS,WSTAR,REDM,EPRD,EWP,EWR,FLSR,  0707YC98
     >                 FLSP,FRELOW,FREQ,IFQLOW,IREPR,LGS)               0707YC98
c
         IF((IRODS.EQ.1.OR.IVRP.EQ.1).AND.JOP.EQ.3) THEN                1114PF97
            CALL ENDRODS (JOP)                                          0219PF98
         ENDIF                                                          1114PF97C
C
         IF (LPTBCR.GT.0) THEN                                          1106YL92
            CALL CALC_CORIOLIS(DXSAV,ANCO,SUME)                         0202BL05
         ENDIF                                                          1106YL92C
C
C     OPTIONALLY COMPUTE ANHARMONICITIES OR DUNHAM COEFFICIENTS
C
         IF (LGS(5).GT.0.OR.LGS(33).EQ.1) CALL ANHARM (JOP)
C
C     COMPUTE ADIABATIC POTENTIAL
C 
         IF (LPTBCR.GT.0) THEN
            V = VSAV
            DO IX = 1, N3TM
              DX(IX) = DXSAV(IX)
            ENDDO
         ENDIF
         CALL ZEROPT (IOP)                                              0220BL05
C
C     OPTIONAL OUTPUT
C
C     PRINT OUT THE BOND DISTANCE, BOND ANGLE INFORMATION FOR ALL THE SAVE
C     GRID POINTS
C
         IF (IOP.GE.0.AND.IPRCD.EQ.1)                                   
     *          CALL PRCORD(FU28,S,V,X,NATOM,NPRCA,IPRCA,AMASS,2)       
C
         IF ((MOD(LSAVE,NPRSMD).EQ.0).or.                               0220BL05
     *       (MOD(LSAVE,IPRDIS).EQ.0)) CALL NOROUT (JOP,DXXP)           0220BL05
C
C
      CALL SSAVE (LSAVE,N3M7,BKAP,2)                                    0220BL05
C
      ENDDO     
C END OF MAIN LOOP OVER FREQUENCIES
C
      CALL CLEAN_RUN(NPATH_FREQ,IPATH_FREQ,NJRUN,JRUN)                  0220BL05
C
 1000 FORMAT (' DXNORM=0,  DX='/(1X,1P,10D13.5))
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      RETURN                                                            0103BL05
      END                                                               0103BL05


      SUBROUTINE CLEAN_RUN(NITEMS,ILIST,NJRUN,JRUN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C this routine removes NITEMS from the JRUN list
      use common_inc
      use perconparam
!     include 'param.inc'
!     include 'percon.inc'
!     include 'common.inc'
      DIMENSION JRUN(512)
      DIMENSION ILIST(NSDM)
      NCHECK=0
C
      DO 10 I=1,NITEMS                                                  0202BL05
        DO 20 J=1,NJRUN
          IF (ILIST(I).EQ.JRUN(J)) THEN
            JRUN(J)=0
            NCHECK=NCHECK+1
          ENDIF 
20      CONTINUE      
10    CONTINUE                                                          0202BL05
      IF (NCHECK.NE.NITEMS) WRITE(*,*) 'WRONG NUMBER OF ITEMS CLEANED'
C push the empty spots (0s) out of JRUN
      DO 30 I=1,NJRUN
        IF (JRUN(I).EQ.0) THEN
          DO 40 J=1,I
            JRUN(I)=JRUN(I+1)
40        CONTINUE        
        ENDIF
30    CONTINUE
      NJRUN=NJRUN-NITEMS                                                0202BL05
C zero out the array of jobs input
      DO I=1,NITEMS
        ILIST(I)=0
      ENDDO
      NITEMS=0
      RETURN
      END


      SUBROUTINE UPDATE_COMMON                                          0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               0103BL05
C this is only run by proc0
C this routine will update any required common blocks of workers
      include 'mpif.h'                                                  1115BL04
      use common_inc
      use perconparam
!     include 'param.inc'                                               0118BL05
!     include 'percon.inc'                                              0118BL05
!     include 'common.inc'                                              0118BL05
      call MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)                    0118BL05
      do 10 i=1,isize-1                                                  0101BL05
C       tell waiting worker that common data will be updated            0101BL05
        call MPI_SEND(1,1,MPI_INTEGER, i, 1, MPI_COMM_WORLD, ierr)      0101BL05
C       tell the waiting worker                                         0101BL05
        call MPI_SEND(AMASS,N3TM,MPI_DOUBLE_PRECISION, i, 2,            0101BL05
     *                MPI_COMM_WORLD, ierr)                             0101BL05
        call MPI_SEND(IND,N3TM,MPI_INTEGER, i, 3,                       0101BL05
     *                MPI_COMM_WORLD, ierr)                             0101BL05
        call MPI_SEND(N3,1,MPI_INTEGER, i, 4,                           0101BL05
     *                MPI_COMM_WORLD, ierr)                             0101BL05
        call MPI_SEND(IND,N3TM,MPI_INTEGER, i, 5,                       0101BL05
     *                MPI_COMM_WORLD, ierr)                             0101BL05
        call MPI_SEND(IPOT,1,MPI_INTEGER, i, 6,                         0101BL05
     *                MPI_COMM_WORLD, ierr)                             0101BL05
C  add additional variables to be shared here
C  don't forget to also add them to RECIEVE_COMMON
10    CONTINUE                                                          0101BL05
C
      RETURN                                                            0103BL05
      END                                                               0103BL05

      SUBROUTINE CHECK_PARALLEL(PARALLEL)                               0101BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PARALLEL                                                  0101BL05

      RETURN                                                            0101BL05
      END                                                               0101BL05

      SUBROUTINE RECIEVE_COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               0103BL05
      include 'mpif.h'                                                  1115BL04
      use common_inc
      use perconparam
!     include 'param.inc'                                               0103BL05
!     include 'percon.inc'                                              0103BL05
!     include 'common.inc'                                              0103BL05
      INTEGER STATUS(MPI_STATUS_SIZE)
      call MPI_RECV(AMASS,N3TM,MPI_DOUBLE_PRECISION, 0,                 0103BL05
     +               2, MPI_COMM_WORLD, status, ierr)                   0103BL05
      call MPI_RECV(IND,N3TM,MPI_INTEGER, 0,                            0103BL05
     +               3, MPI_COMM_WORLD, status, ierr)                   0103BL05
      call MPI_RECV(N3,1,MPI_INTEGER, 0,                                0103BL05
     +               4, MPI_COMM_WORLD, status, ierr)                   0103BL05
      call MPI_RECV(IND,N3TM,MPI_INTEGER, 0,                            0103BL05
     +               5, MPI_COMM_WORLD, status, ierr)                   0103BL05
      call MPI_RECV(IPOT,1,MPI_INTEGER, 0,                              0103BL05
     +               6, MPI_COMM_WORLD, status, ierr)                   0103BL05
      RETURN
      END
      
      SUBROUTINE POLY_MPI_INIT(irank,isize,ierr)                          0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               0103BL05
      include 'mpif.h'                                                  1115BL04
!     use common_inc
      use perconparam
!     include 'param.inc'                                               0103BL05
!     include 'percon.inc'                                              0103BL05
      call MPI_INIT(ierr)                                               1115BL04
      call MPI_COMM_RANK(MPI_COMM_WORLD, irank, ierr)                    1115BL04
      call MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)                    1115BL04
      RETURN                                                            0103BL05
      END                                                               0103BL05


      SUBROUTINE RUN_EHOOK(NQFREQ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C  Run by any but Proc 0.
C  This routine calls HHOOK and returns the frequencies in a
C  non-blocking send.
      include 'mpif.h'
      INTEGER status(MPI_STATUS_SIZE)
C
      call EHOOK(1,IPROC)
C
      call MPI_COMM_RANK(MPI_COMM_WORLD, irank, ierr)
C
      call MPI_ISEND(ENERGY,1,MPI_DOUBLE_PRECISION,0,NQFREQ,
     +               MPI_COMM_WORLD, ireq,ierr)
C
C
      RETURN
      END

      SUBROUTINE RUN_GHOOK(NQFREQ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C  Run by any but Proc 0.
C  This routine calls HHOOK and returns the frequencies in a
C  non-blocking send.
      include 'mpif.h'
      use perconparam
!     include 'param.inc' 
!     include 'percon.inc'
      integer status(MPI_STATUS_SIZE)
      DOUBLE PRECISION data
      DIMENSION DX(N3TM)
C       
      call GHOOK(1,IPROC)
C
      call MPI_COMM_RANK(MPI_COMM_WORLD, irank, ierr)
C
      call MPI_ISEND(DX,N3TM,MPI_DOUBLE_PRECISION,0,NQFREQ,
     +               MPI_COMM_WORLD, ireq,ierr)
C
C
      RETURN
      END

      SUBROUTINE RUN_HHOOK(XDUM,NQFREQ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C  Run by any but Proc 0.
C  This routine calls HHOOK and returns the frequencies in a
C  non-blocking send.
      include 'mpif.h'
      use common_inc
      use perconparam
!     include 'param.inc' 
!     include 'percon.inc'
!     include 'common.inc'                                              0217BL05

      INTEGER status(MPI_STATUS_SIZE)
      DIMENSION XDUM(N3TM)
C      DIMENSION  X(N3TM), F(N3TM,N3TM)                                 0217BL05
C          
      do I=1,N3TM
        X(I)=XDUM(I)
      enddo

      call MPI_COMM_RANK(MPI_COMM_WORLD, irank, ierr)
C
      DLX=1.0D-5
      call HHOOK(X,F,DLX,N3,0,1,irank)      
C    
      call MPI_ISEND(F,N3TM*N3TM,MPI_DOUBLE_PRECISION,0,NQFREQ,
     +               MPI_COMM_WORLD, ireq,ierr)
C     
C    
      RETURN
      END


      SUBROUTINE GRAD_SUBMIT(NEXTTAG,IPROC,STEPX,JRUN,NJRUN)            0202BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'
      DIMENSION JRUN(512)                                               0202BL05
C  add one to NJRUN and add this tag to the JRUN list
      NJRUN=NJRUN+1
      JRUN(NJRUN)=NEXTTAG      
C
C
      CALL MPI_SEND(3, 1, MPI_INTEGER, ito,
     +                        1, MPI_COMM_WORLD, ierr)

      


      NEXTTAG=NEXTTAG+1                                                 0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05

      SUBROUTINE ENER_SUBMIT(NEXTTAG,IPROC,STEPX,JRUN,NJRUN)            0202BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'
      DIMENSION JRUN(512)                                               0202BL05
C  add one to NJRUN and add this tag to the JRUN list
      NJRUN=NJRUN+1
      JRUN(NJRUN)=NEXTTAG
C      
      CALL MPI_SEND(2, 1, MPI_INTEGER, ito,
     +                        1, MPI_COMM_WORLD, ierr)
C
      NEXTTAG=NEXTTAG+1                                                 0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05
C


      SUBROUTINE MULTI_NORMOD(IPATH_FREQ,NEXTTAG,LBUSY)                 0103BL05
      use perconparam
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LBUSY(512)                                                0103BL05
      DIMENSION IPATH_FREQ(NSDM)
      WRITE(6,*) 'YOU SHOULD NEVER SEE THIS.  PLEASE EMAIL software',   0103BL05   
     *           '@comp.chem.umn.edu'                                   0103BL05
      WRITE(6,*) 'ERROR IN DUMMY MULTI_NORMOD'                          0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05
 
      SUBROUTINE FREQ_SUBMIT(X,NEXTTAG,IPROC,STEPX,JRUN,NJRUN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      real(8) :: x(*)
      integer :: jrun(512)
      WRITE(6,*) 'YOU SHOULD NEVER SEE THIS.  PLEASE EMAIL software',   0103BL05
     *           '@comp.chem.umn.edu'                                   0103BL05
      WRITE(6,*) 'ERROR IN DUMMY NORMOD_SUBMIT'                         0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05
     
      SUBROUTINE FIRE_ALL_WORKERS(ierr)                                 0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN                                                            0103BL05
      END                                                               0103BL05

      SUBROUTINE MPI_FINALIZE(ierr)                                     0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN                                                            0103BL05
      END                                                               0103BL05
      
      SUBROUTINE MPI_INIT_dummy(ierr)                                   0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WRITE(6,*) 'YOU SHOULD NEVER SEE THIS.  PLEASE EMAIL software',   0103BL05
     *           '@comp.chem.umn.edu'                                   0103BL05
      WRITE(6,*) 'ERROR IN DUMMY MPI_INIT'                              0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05
  
      SUBROUTINE NEXT_AVAIL_PROC(LBUSY,IPROC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LBUSY(512)
      WRITE(6,*) 'YOU SHOULD NEVER SEE THIS.  PLEASE EMAIL software',   0103BL05
     *           '@comp.chem.umn.edu'                                   0103BL05
      WRITE(6,*) 'ERROR IN DUMMY NEXT_AVAIL_PROC'                       0103BL05
      RETURN
      END

      SUBROUTINE POLY_MPI_INIT(irank,isize,ierr)                        0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN                                                            0103BL05
      END 

      SUBROUTINE UPDATE_COMMON                                          0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WRITE(6,*) 'YOU SHOULD NEVER SEE THIS.  PLEASE EMAIL software',   0103BL05
     *           '@comp.chem.umn.edu'                                   0103BL05
      WRITE(6,*) 'ERROR IN DUMMY MUPDATE_PATH'                          0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05

      SUBROUTINE POLY_MPI_WAITFORJOB(IRANK,ISIZE)                       0103BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WRITE(6,*) 'YOU SHOULD NEVER SEE THIS.  PLEASE EMAIL software',   0103BL05
     *           '@comp.chem.umn.edu'                                   0103BL05
      WRITE(6,*) 'ERROR IN DUMMY POLY_MPI_WAITFORJOB'                   0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05
      
      SUBROUTINE CHECK_PARALLEL(PARALLEL)                               0101BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PARALLEL                                                  0101BL05
      PARALLEL=.FALSE.                                                  0101BL05
      RETURN                                                            0101BL05
      END                                                               0101BL05

      SUBROUTINE UPDATE_STATUS(LBUSY)                                   0212BL05
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LBUSY(512)                                                0212BL05
      WRITE(6,*) 'YOU SHOULD NEVER SEE THIS.  PLEASE EMAIL software',   0103BL05
     *           '@comp.chem.umn.edu'                                   0103BL05
      WRITE(6,*) 'ERROR IN DUMMY UPDATE_STATUS'                         0103BL05
      RETURN                                                            0103BL05
      END                                                               0103BL05

      SUBROUTINE MPI_Abort(MPI_COMM_WORLD,MPI_ERRORCODE,IERR)           0814JZ08
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               0814JZ08
      RETURN                                                            0814JZ08
      END                                                               0814JZ08

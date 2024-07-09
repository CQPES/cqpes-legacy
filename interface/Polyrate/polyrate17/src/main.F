c***********************************************************************
c  MAIN
c***********************************************************************
c
c     This routine was completely rewritten in January 1994 for
c     version 6.0.  The input is now in a keyword format and is
c     handled in 'read5.f'. Calculations that were done within this
c     routine have now been moved so that this is just a calling
c     routine.
c
c     PROGRAM FOR POLYATOMIC VARIATIONAL TST CALCULATIONS
c
c     CALLS:
c          dorest,dorph,dorepr,dosage,dosafr,dopnm
c          elrph,headr,option,fiopen,initze,restor,dattim,title,setup,
c          rphwrt,rphset,react,saddle,center,trans,normod,path,
c          restrt,nextpt,rate,tsrate,fcinpt,fcmep,fcrate,
c          read5,setlgs,setvar
c
      use common_inc
      use perconparam
      use gf
      use path_mod
      use kintcm; use cm
      use rate_const
      use keyword_interface
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
c      include 'mpif.h'
c
      character*20 finame
      character*7  fistat
      logical lexit, PARALLEL, LBUSY(512)                               0101BL05
      DIMENSION JRUN(512)                                               0101BL05
c
c     Open the input and output files (fu5 and fu6).  All other
c     files will be opened later in fiopen.
c
c   Use the character variables FISTAT (file status), FINAME (filename), and
c   the subprogram OPENFI to open the input data file and the long output files
c   which are linked to FORTRAN units fu5 and fu6, respectively.
c
      call del_old_units
c
c    Read into core file POLY.FU5
c
      nlines_inp=0
      call get_file('poly.fu5',nlines_inp,poly_fu5)
      call find_esp_mode
      call find_numatoms
!
!  Now ESP file
!
      call get_esp
      lexit    = .false.
c
      irank=0                                                           0101BL05
c
      fistat = 'unknown'
      finame = 'poly.fu6'
c
      NEXTTAG=1001                                                      0101BL05
C
      CALL POLY_MPI_INIT(irank,isize,ierr)                              0101BL05
C
      MPI: if (irank .ne. 0) then
          call POLY_MPI_WAITFORJOB(irank,isize)
          goto 666
        else MPI
          NJRUN=0; jrun= 0
          call openfi (fu6, fistat, finame, lexit)
          if (lexit) goto 3000
c
          fistat = 'old'
          finame = 'poly.fu5'
c
          call openfi (fu5, fistat, finame, lexit)
c
c     Write out program header to unit fu6
          call headr
c
c
c     this call needs to be integrated into the new format -- init routine
          call initze
c
c
c     Read in all input from unit 5 and write out the summary table
          call read5(PARALLEL)
c
c     Using keywords - set LGS flags to their correct values.  Here all
c     that do not depend on a stationary point will be set.
c     Later they will be altered for each successive reactant, product
c     and for the saddle point.
c
          call setlgs

c
c     Using the keywords set the variables to their correct values.  This
c     is a translator from the new interface to the old internal structure.
c     Here just the global parameters will be set (setvar).  Later they will
c     be altered for each successive reactant, product and saddle point using
c     stvarj.
c
          call setvar
c
c     The next routine checks if the calculation will be done in parallel.
c     If the code has not properly linked the MPI routines, it will set
c     PARALLEL to .FALSE.
c
          call check_parallel(PARALLEL)                                     0101BL05
C     The subroutine check_parallel determines if the parallel execution
C     is permitted.
C
c
c     This subroutine was added to allow all files to be opened internally.
c     This is for compability with other machines and operating systems.
c
          call fiopen
c
c     Check options and read in fu50 for alll calculations except
c     restart runs.  This is to allow a restart calculation where
c     VTST-IC was not requested in the original run but is desired
c     using the MEP stored on fu1.
c     This was modified on 1/4/96 by R. Steckler

c
      if (lgs(8).le.0) then
c
c        Read in unit fu50 for VTST-IC calculations
         if (lgs2(11) .ne. 0) call readic
c
c        Do IVTST calculations                                          07/95KAN
c                                                                       07/95KAN
         if (lgs2(38) .ge. 0 .and. lgs(8) .le. 0) then                  07/95KAN
           if (ipot.eq.3) then                                          0327YC97
            if (ifrfac.ne.0) then                                       0814JC00
            write (fu6,*) 'Error: IVTST and FREQSCALE is not supported' 0814JC00
            CALL FCRATE                                                 0814JC00

              call FIRE_ALL_WORKERS(ierr)                               1115BL04
              call MPI_FINALIZE(ierr)                                   1115BL04

            stop 'IVTST ERROR'                                          0814JC00
            else                                                        0814JC00
c
              call givtst(nfcvt,isct,izct,                              07/95KAN
     >        ivtst,slpg,slmg,slp,slm,del,nedeg,elec,                   07/95KAN
     >        nratom,iatsv,xr,icode,nq12,nq22,ntemp,temp,
     >        redm,sigmaf,sigmar,xmass,iclasv,                          0808JC00
     >        iunit6,gufac6)                                            0405JZ07
              write(fu6,4500)                                           09/95KAN
              CALL FCRATE

                call FIRE_ALL_WORKERS(ierr)                             1115BL04
                call MPI_FINALIZE(ierr)                                 1115BL04

              stop 'IVTST DONE'                                         09/95KAN
            endif                                                       0814JC00
           else                                                         0327YC97
            write (fu6,*) 'Fatal error: IVTST required unit29 as ',     0327YC97
     >           'POTENTIAL'                                            0327YC97
            CALL FCRATE

              call FIRE_ALL_WORKERS(ierr)                               1115BL04
              call MPI_FINALIZE(ierr)                                   1115BL04

            stop 'IVTST ERROR'
           endif
         endif                                                          09/95KAN
c
c        Check for consistencies between chosen options
         call option
c
c        Close the input data files which have been used up to this point.
         call fcinpt
      end if
c
c   **** POLYRATE calculations begin here: ********
c
c
c     Determine if it is a restart calculation: lgs(8)>0 for restart
c     If so, make sure lgs and variables are set at the value used
c     in the previous path calculations.
c
      IF (LGS(8).GT.0) THEN
         call dorest
         call option
         call fcinpt
         call prepj(5)
      else
C
C     Do the zero-order IVTST calculation
C
         if (lgsic(10) .eq. 1) then
            call prepj(5)
            call zocupd
            call rate
            write(fu6,4500)
            call fcrate
              call FIRE_ALL_WORKERS(ierr)                               1115BL04
              call MPI_FINALIZE(ierr)                                   1115BL04

            stop 'main 1'                                               0701YC97
         endif
c
c                       Set up RPH information or potential surface
         call dorph
c
c                       Optionally compute reactant and product properties
         call dorepr
c
c        bath mode // we do not correct for reactants and products
c
         if (ibathm.eq.1) then                                          0317YC99
           LBATH = .true.                                               0317Yc99
           if (LGS2(39).GE.3) numint = numint + 1                       0317Yc99
         endif                                                          0317Yc99
c
c                       Compute saddle point and its energetics
c
         if (irepr(5).eq.1) then                                        0808YC97
         call prepj(5)

         call sadeng
c
c                      Calculate the parameters in the zero order ivtst
c                      correction for the classical energies and vibrational
c                      frequencies along the mep
c
        endif                                                           0808YC97
        if (lgs2(11).ne.0) then
           call zoc3p
           if (lgs(3) .ne. 0) call zocpar
           call zocprn
         endif                                                          09/95KAN
c
        if (lgs(3).ne.0) write (fu6,2900)
c
c
c                       compute steepest descents path and
c                       projected normal modes along the path.
c
         call prepj(6)                                                  0522RS95
         call dopnm(PARALLEL,NEXTTAG,LBUSY,JRUN,NJRUN)                  0101BL05
         if (ipath.ne.0.and.lgs2(11).eq.0) call findl                   1203YC96
      endif
c
c   Close the input and output data files used up to this point.
c
      call fcmep
c
c   The path is now computed.  Here the rate constants are computed
c   without and optionally using the zero-order IVTST option.
c        lgs2(11) = 0   do not use VTST-IC-0
c        lgs2(11) = 1   do the corrected calculation only
c        lgs2(11) = 2   do the uncorrected first then the coorected
c
      if (lgs2(11).ge.1) then
          if (lgs2(11).eq.2) then
             if (lgs(7).gt.0.or.lgs(9).ne.0) call rate
             if (lgs(7).le.-1) call tsrate
          endif
c
c     Update the information at each gts to include the zero order correction.
          call zocupd
          lzoc = .true.
      endif
c
c     if frequencies are scaled, do twice the calculation (only for non IC)
c
      if (ifrfac.ne.0) then
         ifrfac=0
         if (lgs(7).gt.0.or.lgs(9).ne.0) call rate
         if (lgs(7).le.-1) call tsrate
         ifrfac=1
         write(fu6,4400)
         if (lgs(7).gt.0.or.lgs(9).ne.0) call rate
         if (lgs(7).le.-1) call tsrate
      else
         if (lgs(7).gt.0.or.lgs(9).ne.0) call rate
         if (lgs(7).le.-1) call tsrate
      endif
      write(fu6,4500)
C
      if (lgs2(13) .ne.0 .or. lgs2(14) .ne. 0) call prmep               0706WH94
c
c     Plot the representative tunneling path in mass-scaled Jacobian    0411PJ01
c
      if(lgs3(3) .ne. 0) call rtpjac                                    0411PJ01
c
C
C   Close all remaining input and output data files.
C
      call fcrate
ctrace numstep
c               open (unit=99,file='polyout')
c               ishft = n3-nf(5)
c               write (99,*) dlx, conf(1),freq(ishft+1)*autocm,
c     *              wer(1)*AUTOCM,wer(nf(1)+nf(2)+1)*AUTOCM
c               close (99)
cend trace

      endif MPI
      call FIRE_ALL_WORKERS(ierr)
666   call MPI_FINALIZE(ierr)
      stop 'exit polyrate'                                              0701YC97
C
 2900 FORMAT(//1X,78(1H*),                                              0620WH94
     * /1X,'After this, all energies are given w/re to classical'
     * /1X,'reactants and output is in a.u. and distance unit denpends '
     * /1X,'on OUTUNIT keyword unless stated otherwise.',               0405JZ07
     * /1X,78(1H*))
c
c  The error statement for units fu5 and fu6 are written to unit 6
c  (assumed to be standard out) because if there is a problem
C  with units fu5 or fu6 then the code cannot use fu6 for its output
c  and the following error message may not be written.
C
 3000 WRITE (6, *) 'Error opening the long output file'
      CALL FCRATE
      STOP 'MAIN 5'                                                     0701YC97
 4400 FORMAT(//1X,21('*'),' CALCULATION WITH SCALED FREQUENCIES ',
     *       21('*'))
 4500 FORMAT(//1X,25('*'),' END OF POLYRATE LONG OUTPUT ',25('*'))
C
      END

C
C***********************************************************************
C  PATH
C***********************************************************************
C
      SUBROUTINE path(PARALLEL,NEXTTAG,LBUSY,JRUN,NJRUN)                0101BL05
C
C     Calculate reaction path for polyatomic VTST code
C
C     Compute reaction path starting from saddle point, in both
C        directions, using one of several optional integration
C        routines.
C
C     CALLED BY:
C                DOPNM
C     CALLS:
C           CUBST,SSAVE,INTEFR,SYMCHK,IRCX,BCALC,
C           NORMOD,RPHWRT,EXTRAP,COLSHF,BCALC0,MEPINV,MEPOUT
C
C   PARAMETERS AND COMMON BLOCKS MODIFIED 6/30/91
C   MODIFICATIONS FOR CDSCSAG WERE MADE 26/08/91
C   FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR
C
C   The include file esp.inc has been removed in version 5.0            1021GL92
C
C
      use path_mod
      use common_inc
      use perconparam
      use kintcm, only : inosad,intmu
      use rate_const
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c      include 'mpif.h'                                                  1115BL04

      LOGICAL LEXIT,LBCALC,LLSAVE,LMORE,LGRDSV,LFIRST                   0325PF98
      LOGICAL LMOREGO,LTOPNST,PARALLEL,LBUSY(512)
      DIMENSION JRUN(512)
      save
C
C     EPSX: Small additive number for equivalence testing.
C
ccc      DATA EPSX /5.0D-8/                                                7/1/91VM
C
C     Write out section header and perform initial tasks before         1229BL04
C     following the path                                                1229BL04
C                                                                       1229BL04
C
      call mem_path(1,n3tm)
      CALL INIT_PATH(BKAP,FISEN,LDEL,NCPT,NGPT,szpesp,
     *               varorp,vmepdw,VOLD,vreact,vrorp,vzpedw)
C
      CALL INIT_SAVE(BKAP,N3M7,szpesp,varorp,vmepdw,VOLD,
     *               vreact,vrorp,vzpedw,xsign,PARALLEL)
C
C     If this is a parallel calculation, update the worker processes
C     on the basic system info
C
      IF (PARALLEL) CALL UPDATE_COMMON                                  0101BL05
C
      LEXIT = .FALSE.
      IDIREC = 1
      LCOUNT = 0
      MARRSP = 1                                                        6/30YL91
      IF (LGS(5).GT.21) THEN                                               ..
         IEND = NARR - 1                                                   ..
         DO 25 IARR = 1, IEND                                              ..
            IF (0.GE.SRARR(IARR)) MARRSP = MARRSP + 1                      ..
25       CONTINUE                                                          ..
      ENDIF                                                                ..
      MARR = MARRSP                                                     6/30YL91
C
C
      IWRDY=0
      CALL SLOOPS(NGPT,NCPT,LEXIT,FISEN,IWRDY,LDEL,vmepdw,vzpedw,
     *            szpesp,IDIREC,LCOUNT,MARRSP,MARR,LBCALC,              1223BL04
     *                  LLSAVE,LMORE,LGRDSV,LFIRST,DXB,DXOLD,
     *                  XOLD,BKAP,NEXTTAG,LBUSY,PARALLEL,JRUN,GEOMM,    0101BL05
     *                  NJRUN,DERIVM,LSAVES)                            0101BL05
C  end of SERIAL path                                                   1117BL04
      IF (.NOT.LEXIT) THEN
         IF (LCDSC) CALL BCALC0(LGS,NSHLF,SSUBI,CDSCMU,INTMU)           0327YC97
         IF (LGS2(11).NE.0) CALL BCALC0(LGS,NSHLF,SSUBI,ZOCMCD,INTMU)   0327YC97
C
C        Invert array storeage if SSUBI array in descending order
C
         IF (ISEN.GE.0)  CALL MEPINV                                    1026WH92
      ENDIF
c
c put IVTSTM through hooks, the fast way is to rearrange the arrays     0202YC98
c into the format we want as in fu30/40 then try the routine calls      0202YC98
c
      if (lgs(12).ne.0.AND.INOSAD.NE.1) CALL IVTMH(1,ngpt)              0202YC98
c
      CALL WRITEOUT_ENDPATH(IBEG,LNCOL,IEND)                            0101BL05
C
      RETURN
      END

      SUBROUTINE SLOOPS(NGPT,NCPT,LEXIT,FISEN,IWRDX,LDEL,vmepdw,
     *                  vzpedw,szpesp,IDIREC,LCOUNT,MARRSP,MARR,LBCALC, 1223BL04
     *                  LLSAVE,LMORE,LGRDSV,LFIRST,DXB,DXOLD,
     *                  XOLD,BKAP,NEXTTAG,LBUSY,PARALLEL,JRUN,GEOMM,    1223BL04
     *                  NJRUN,DERIVM,LSAVES)                            1223BL04
      use common_inc
      use perconparam
      use rate_const
      use kintcm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LEXIT,LBCALC,LLSAVE,LMORE,LGRDSV,LFIRST                   1227BL04
      LOGICAL LMOREGO,LTOPNST,PARALLEL                                  1227BL04
      LOGICAL LBUSY(512)                                                0101BL05
      DIMENSION JRUN(512)                                               0202BL05
      DIMENSION DXB(N3TM),DXOLD(N3TM),SBX(3),XOLD(N3TM)
      DIMENSION IPATH_FREQ(NSDM)
      DIMENSION GEOMM(N3TM,NSDM)                                        0212BL05
      DIMENSION LSAVES(NSDM)                                            0212BL05
      DIMENSION DERIVM(N3TM,NSDM)                                       0212BL05
C START OF SLOOPS
C
C====================================================================
C
      NPATH_FREQ=0
C
C     Loop over directions of s
C
   30 CONTINUE
      CALL INIT_STEP(IST,NFUNC,KL,IWRDX,LFIRST,LGRDSV,LBCALC,           1227BL04
     *               SLAST,SBX,DXOLD,LGS4,ISTOPS,SZPE,SZPESP,
     *               FISEN)                                             1227BL04
C
C--------------------------------------------------------------------
C         Integration loop over NST steps
C
   50 CONTINUE
C
C             Increment step counter
C
      IST = IST+1
C
C                Save current position and derivative
      CALL SAVE_POS_PRINT(DXB,DXOLD,IST,LBCALC,LGRDSV,LGS4,             1227BL04
     *                    LLSAVE,STEP0,STEPX,XOLD,fisen)
C
C                Call integration driver to step to new geometry on path
C
      CALL INTEGR (STEPX,IST,NFUNC,KL,FISEN,LGS(31))
C
C
C     Check to see if point needs to be saved and whether rods or euler
C     stabilization is to be performed
C
      CALL CHECK_SAVE_STABILIZE(BKAP,FISEN,LDEL,LCOUNT,MARR,
     *                          MARRSP,LBCALC,LLSAVE,LMORE,LGRDSV,      0101BL05
     *                          LFIRST,DXB,DXOLD,SBX,XOLD,IST,          0101BL05
     *                          NFUNC,SLAST,STEPX,IPATH_FREQ,NEXTTAG,   0101BL05
     *                          LBUSY,PARALLEL,JRUN,GEOMM,NPATH_FREQ,   0101BL05
     *                          NJRUN,DERIVM,LSAVES)                    0101BL05
C
         IF (LBCALC.AND.(IRODS.EQ.1.OR.IVRP.EQ.1)) THEN                 0325PF98
            LCOUNT = LCOUNT+1                                           0325PF98
            IFLAG = 0                                                   0325PF98
            IF (IST.EQ.1) IFLAG = -1                                    0325PF98
C
            DO 120 I = 1, N3M7                                          0325PF98
              DO 125 J = 1, N3                                          0325PF98
                COFBSV(J,I) = COF(J,I)                                  0325PF98
                COF(J,I) = COFTSV(J,I)                                  0325PF98
 125          CONTINUE                                                  0325PF98
 120        CONTINUE                                                    0325PF98
C
C                   Compute B for the last step and effective masses at 0325PF98
C                   previous grid point                                 0325PF98
C
            CALL BCALC (SBX,DXOLD,DXB,STEPX,LDEL,IFLAG,LCOUNT,BKAP)
C
            DO 130 I = 1, N3M7                                          0325PF98
              DO 135 J = 1, N3                                          0325PF98
                COF(J,I) = COFBSV(J,I)                                  0325PF98
 135          CONTINUE                                                  0325PF98
 130        CONTINUE                                                    0325PF98
C
            LBCALC = .FALSE.                                            0325PF98
            IF (LGS(30).LT.0.AND.LGRAD.EQ.1)                            0325PF98
     *          WRITE (FU30,2800) (BCURV(I),I=1,N3M7)                   0325PF98
         ENDIF                                                          0325PF98
C
      CALL WRITEOUT_STEP(IWRDX,LLSAVE,SBX)                              1230BL04
      CALL WRITEOUT_STEP2(IWRDX,LLSAVE,SBX,DXOLD)                       1230BL04
c
c store the gradient steps s,v, and 1/I for interpolations for extrapolations
c with ivtstm method
c
      if ((lgs(12).ne.0).and.inosad.ne.1) then                          0202YC98
      call rph31_mem
         ncpt = ncpt + 1                                                0202YC98
         if (mod(ncpt,ini).eq.0) then                                   0202YC98
           ngpt = ngpt + 1                                              0202YC98
           if (lgs(34).eq.0) then
             call trans(2,n3,amass,x,dx)                                0202YC98
             call center (5,0)                                          0202YC98
             call trans(1,n3,amass,x,dx)                                0202YC98
             save31(3,ngpt)=1.d0/fmom(5)                                0202YC98
           else
             save31(3,ngpt)=1.d0/1.d+30
           endif
           save31(1,ngpt)=s                                             0202YC98
           save31(2,ngpt)=v                                             0202YC98
         endif                                                          0202YC98
      endif                                                             0202YC98
C Determine if a special stop condition exists                          1227BL04
      LTOPNST=.FALSE.
C
      CALL CHECK_CON(LTOPNST,N3M7,LDEL,vmepdw,vzpedw,IDIREC,            1227BL04
     *                     LCOUNT,MARRSP,LLSAVE,LMORE,LGRDSV,ISTOPS,    1227BL04
     *                     BKAP,PARALLEL,SZPE,ist)                          0608BE05
C
      IF (LTOPNST) GOTO 50                                              1227BL04
C
C         End integration loop over NST steps
C--------------------------------------------------------------------
C
C collect all frequency calculations and perform a normal mode analysis
C on each of these points

      IF (PARALLEL) THEN                                                0111BL05
cccc        CALL MULTI_NORMOD(IPATH_FREQ,NPATH_FREQ,NEXTTAG,LBUSY,
cccc     +                    JRUN,NJRUN,GEOMM,DERIVM,LSAVES)           0103BL05
      ENDIF                                                             0111BL05

      CALL CALC_CURV(DXB,DXOLD,FISEN,IDIREC,LBCALC,LCOUNT,LDEL,         1231BL04
     *               LGS4,N3M7,IST,NFUNC,SBX,STEPX,BKAP)                1231BL04
      LMOREGO=.FALSE.                                                   1227BL04
      CALL FINISH_LOOPS(LEXIT,FISEN,N3M7,LDEL,IDIREC,LCOUNT,            1227BL04
     *                  MARRSP,MARR,LBCALC,LMORE,LMOREGO,LGS4)          1227BL04
C
      IF (LMOREGO) GOTO 30                                              1227BL04
C
C     End of loop over directions of s
C====================================================================
C
 2800 FORMAT(' BF=', /, (1X, 1PE19.10, 3E20.10))
C end of sloops
      RETURN
      END

      SUBROUTINE CHECK_CON(LTOPNST,N3M7,LDEL,vmepdw,vzpedw,IDIREC,      1227BL04
     *                     LCOUNT,MARRSP,LLSAVE,LMORE,LGRDSV,ISTOPS,    1227BL04
     *                     BKAP,PARALLEL,SZPE,ist)
      use common_inc
      use perconparam
      use rate_const; use kintcm

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1227BL04
      LOGICAL LLSAVE,LMORE,LGRDSV,LFIRST,LTOPNST,PARALLEL               1227BL04
C
      IF (LLSAVE) THEN
C
C                Save point so put GTS info into RESCOM arrays
C
         LSAVE = LSAVE +LDEL
         IF (PARALLEL) THEN                                             0224BL05
           CALL SSAVE (LSAVE,BKAP,1)
         ELSE                                                           0224BL05
           CALL SSAVE (LSAVE,BKAP,0)
         ENDIF                                                          0224BL05
C
C     Determine if the special stop conditions are met to stop calculating
C     the reaction path at a saved point and then change the limits.
C
         if (isstop.eq.2.and.istops.eq.0) then                          0423TA02
            if (v.lt.vmepdw.and.s.lt.0.d0) then                         0423TA02
               slm = s                                                  0423TA02
               istops = 1                                               0423TA02
            elseif (v.lt.vmepdw.and.s.gt.0.d0) then                     0423TA02
               slp = s                                                  0423TA02
               istops = 1                                               0423TA02
            endif                                                       0423TA02
         elseif (isstop.eq.-2.and.istops.eq.0) then                     0423TA02
            if (vad.lt.vzpedw.and.s.lt.0.d0) then                       0423TA02
               slm = s                                                  0423TA02
               istops = 1                                               0423TA02
            elseif (vad.lt.vzpedw.and.s.gt.0.d0) then                   0423TA02
               slp = s                                                  0423TA02
               istops = 1                                               0423TA02
            endif                                                       0423TA02
         endif                                                          0423TA02
C
C     Save the ZPE value of the saved point for special stop at current point
C     based on the VaG value.
C
         if (isstop.eq.-1) szpe = vad - v                               0423TA02
C
      ENDIF
C
C     Determine if the special stop conditions are met, and then change
C     the limits of calculating the reaction path.
C
      if (isstop.eq.1.and.istops.eq.0) then                             0423TA02
         if (v.lt.vmepdw.and.s.lt.0.d0) then                            0423TA02
            slm = s                                                     0423TA02
            istops = 1                                                  0423TA02
         elseif (v.lt.vmepdw.and.s.gt.0.d0) then                        0423TA02
            slp = s                                                     0423TA02
            istops = 1                                                  0423TA02
         endif                                                          0423TA02
      elseif (isstop.eq.-1.and.istops.eq.0) then                        0423TA02
         if ((v+szpe).lt.vzpedw.and.s.lt.0.d0) then                     0423TA02
            slm = s                                                     0423TA02
            istops = 1                                                  0423TA02
         elseif ((v+szpe).lt.vzpedw.and.s.gt.0.d0) then                 0423TA02
            slp = s                                                     0423TA02
            istops = 1                                                  0423TA02
         endif                                                          0423TA02
      endif                                                             0423TA02
C
C             Check for step loop exit condition
C
      IF (.NOT.((S.LE.SLM).OR.(S.GE.SLP))) THEN                         1105PF97
         LMORE = .FALSE.                                                6/07YL92
         IF (IDIREC.EQ.1.AND.LGS(3).GT.0.AND.LGS(1).NE.0) THEN              ..
            IF (LSAVE.GT.1) THEN                                            ..
               LMORE = .TRUE.                                               ..
            ELSE                                                            ..
               WRITE (FU6,4100)                                             ..
            ENDIF                                                           ..
          ELSEIF ((IDIREC.EQ.2.AND.LGS(3).GT.0).OR.(LGS(1).EQ.0)) THEN      ..
            IF (LSAVE.LT.NSDM) THEN                                         ..
               LMORE = .TRUE.                                               ..
            ELSE                                                            ..
               WRITE (FU6,4200)                                             ..
            ENDIF                                                           ..
          ELSEIF (IDIREC.EQ.1.AND.LGS(3).LT.0) THEN                         ..
            IF ((2*(NSDM-LSAVE)+1).LT.NSDM) THEN                            ..
               LMORE = .TRUE.                                               ..
            ELSE                                                            ..
               WRITE (FU6,4300)                                             ..
            ENDIF                                                           ..
          ENDIF                                                             ..
         LTOPNST=.FALSE.                                                1227BL04
         IF ((IST.LT.NST).AND.LMORE) LTOPNST=.TRUE.                     1227BL04
      ENDIF
4100  FORMAT(/,2X,50('*'),/,2X,'Warning!!!'                             6/07YL92
     *,/,2X,'Number of save points in the first',                       6/07YL92
     *' direction has',/,2X,'reached the limit, NSDM.',/,2X,50('*'))    6/07YL92
4200  FORMAT(/,2X,50('*'),/,2X,'Warning!!!'                             6/07YL92
     *,/,2X,'Number of save points has reached the',                    6/07YL92
     *' limit, NSDM.',/,2X,50('*'))                                     6/07YL92
4300  FORMAT(/,2X,50('*'),/,2X,'Warning!!!'                             6/07YL92
     *,/,2X,'Total number of save points after',                        6/07YL92
     *' reflection has',/,2X,'reached the limit, NSDM.',/,2X,50('*'))   6/07YL92
C end check_con
      RETURN
      END SUBROUTINE CHECK_CON

      SUBROUTINE FINISH_LOOPS(LEXIT,FISEN,N3M7,LDEL,IDIREC,LCOUNT,      1227BL04
     *                       MARRSP,MARR,LBCALC,LMORE,LMOREGO,LGS4)     1227BL04
      use common_inc
      use perconparam
      use rate_const
C                                                                       1227BL04
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      LOGICAL LEXIT,LBCALC,LMORE,LMOREGO                                1227BL04
C
      IF (IDIREC.NE.2) THEN
         IF (LGS(1).EQ.0) THEN
            LEXIT = .TRUE.
         ENDIF
C
         IF (.NOT.LEXIT) THEN
C
C               Shift results in RESCOM arrays
C
            LNCOL = NSDM-LSAVE+1
            CALL COLSHF (LNCOL)
C
C               Set up for other s direction
C
            FISEN = -FISEN
            LCOUNT = 0
            IFWKB = 0
            LDEL = 1
            LSAVE = LNCOL
            IF (LGS(3) .LT. 0) LSAVE = 2*LNCOL - 1
            NSHLF = LNCOL
C
C                   Reset X to saddle point geometry
C
            DO 80 I = 1, N3
               X(I) = GEOM(I,NSHLF)
   80       CONTINUE
            IF (LGS(9).NE.0) THEN                                       9/18YL92
               DO 81 I = 1, N3                                              ..
                  DO 82 II = 1, N3                                          ..
                     COFX(II,I) = CSV(II,I)                                 ..
   82             CONTINUE                                                  ..
                  IF (ABS(LGS(9)).GE.2) THEN                                ..
                     SGN1(I) = SGN2(I)                                      ..
                     IN1(I) = IN2(I)                                        ..
                  ENDIF                                                     ..
   81           CONTINUE                                                    ..
             ENDIF                                                      9/18YL92
            MARR = MARRSP                                               6/30YL91
C
C               Return to top of loop over direction os S
C
            IDIREC = 2
            IF (LGS(3) .GE. 0) THEN
               IF (LSAVE.LT.NSDM) THEN                                  6/07YL92
                  LMORE = .TRUE.                                            ..
               ELSE                                                         ..
                  WRITE (FU6,4200)                                          ..
               ENDIF                                                        ..
               IF (LMORE) LMOREGO=.TRUE.                                1227BL04
            ENDIF                                                       6/07YL92
         ENDIF
      ENDIF
4200  FORMAT(/,2X,50('*'),/,2X,'Warning!!!'                             6/07YL92
     *,/,2X,'Number of save points has reached the',                    6/07YL92
     *' limit, NSDM.',/,2X,50('*'))                                     6/07YL92
C end finish_loops
      RETURN
      END SUBROUTINE FINISH_LOOPS


      SUBROUTINE INIT_STEP(IST,NFUNC,KL,IWRDX,LFIRST,LGRDSV,LBCALC,     1227BL04
     *                    SLAST,SBX,DXOLD,LGS4,ISTOPS,SZPE,SZPESP,
     *                    FISEN)
      use perconparam
      use common_inc
      use rate_const
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1227BL04
c
      LOGICAL LFIRST,LGRDSV,LBCALC                                      1227BL04
      DIMENSION DXOLD(N3TM),SBX(3)                                      1227BL04
C START OF INIT_STEP
C
C     Initialization
C         step counter
C
      IST = 0
C
C        Switch for setting the eigenvector of Hessian                  5/10DL90
C
      INDPH = 1                                                         9/18YL92
C
C         Function call counter
C
      NFUNC = 0
C
C         Counter used by integrators
C
      KL = 0
C
C         Flag for writing DX to RPH file
C
      IWRDX = 0
C
C         Flags for RODS and VRP
C
      LRODS = .FALSE.                                                   0325PF98
      LFIRST = .TRUE.                                                   0325PF98
      LGRDSV = .FALSE.                                                  0325PF98
C
C         Flag for calculating B's
C
      LBCALC = .FALSE.
      S = 0.0D0
      SLAST = 0.0D0
C
C            Compute rph info
C
      SBX(1) = 0.0D0
      SBX(2) = 0.0D0
      SBX(3) = 0.0D0                                                    0325PF98
C
C               Initial DX and DXOLD
C
      DO 40 I = 1, N3
         IF (LGS(1).NE.0) DX(I) = -FISEN*VECSV(I)
         DXOLD(I) = DX(I)
   40 CONTINUE
C
      LGS4 = LGS(4)
C
C         Flag for special stop encountered already
C
      istops = 0                                                        0423TA02
      szpe = szpesp                                                     0423TA02
C END OF INIT_STEP
      RETURN                                                            1227BL04
      END SUBROUTINE INIT_STEP

      SUBROUTINE INIT_PATH(BKAP,FISEN,LDEL,NCPT,NGPT,szpesp,
     *                     varorp,vmepdw,VOLD,vreact,vrorp,vzpedw)
      use common_inc
      use perconparam
      use path_mod
      use keyword_interface
      use kintcm
      use rate_const
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1227BL04
C
      WRITE (FU6,1050)
C
      NGPT=0                                                            0202YC98
      NCPT=0                                                            0202YC98
      IFWKB = 0
      IF(IUNIT6.EQ.1) THEN                                              0405JZ07
       WRITE (FU6,1250) (NST+NST2),INH,SLP,SLM                          1105PF97
      ELSE
       WRITE (FU6,1260) (NST+NST2),INH,SLP/GUFAC6,SLM/GUFAC6
      ENDIF                                                             0405JZ07
C
      if (isstop.gt.0) write (fu6,1254) (1.d0-fracdw),fracdw            0423TA02
      if (isstop.lt.0) write (fu6,1255) (1.d0-fracdw),fracdw            0423TA02
C
      IF (LGS(27) .EQ. 0) THEN                                          1203WH92
         WRITE(FU6,1261)                                                ..
      ELSE                                                              ..
         WRITE(FU6,1262)                                                ..
      ENDIF                                                             1203WH92
      IF (ISEN .EQ. 1) THEN                                             6/28T90
         WRITE(FU6,1251)                                                6/28T90
      ELSE                                                              6/28T90
         WRITE(FU6,1252)                                                6/28T90
      ENDIF                                                             6/28T90
C
      FISEN = DBLE(ISEN)
C
      IF (LGS(31).EQ.0) WRITE (FU6,1450)
      IF (LGS(31).EQ.2) WRITE (FU6,1550)
      IF (LGS(31).EQ.5) WRITE (FU6,1660)                                5/23B88
      IF (LGS(31) .EQ. 30) WRITE(FU6,1460)                              7/1/91VM
      IF (LGS(31) .EQ. 32) WRITE(FU6,1560)                              7/1/91VM
      IF (LGS(31) .EQ. 35) WRITE(FU6,1670)                              7/1/91VM
      WRITE (FU6,1700) NST0,DEL1,DEL
      IF (LGS(31) .EQ. 2 .OR. LGS(31) .EQ. 32) THEN                     7/1/91VM
         WRITE (FU6,2850) DELTA2, DIFFD                                 150191VM
      ENDIF
C
      IF (ABS(LGS(4)).NE. 0) WRITE (FU6,1900) SPRNT
      WRITE (FU6,1350) SOB,SOE
C
      NST = NST+NST2
C
      IF (LGS(11).NE.0) THEN
C
         WRITE (FU6,2100) (SSPEC(I),I=1,NSPEC)
      ENDIF
C
C    SET THE FLAG FOR CDSCSAG
C
C     IWRDX = 0
      IWRDX(:) = 0
C
      N3M7 = NF(5)                                                      9/18YL92
C
      IF (LGS(5).EQ.0) WRITE (FU6,1005)
      IF (LGS(1).NE.0) THEN
         LSAVE = NSDM
         LDEL = -1
      ELSE
         LSAVE = 0
         LDEL = 1
      ENDIF
C
C     WRITE(FU6,2130)                                                   0613WH94
      IF(IUNIT6.EQ.1) WRITE(FU6,2130)                                   0405JZ07
      IF(IUNIT6.EQ.0) WRITE(FU6,2140)                                   0405JZ07
C
 1005 FORMAT(/2X,'Note: ZPE of imaginary frequencies will be set to',
     *       /2X,'      zero when using the harmonic approximation.')   0612WH94
 1050 FORMAT(//1X,32(1H*),' Reaction path ',32(1H*))                    0612WH94
 1250 FORMAT(/2X,'All s values and step sizes are in ',
     *           'mass-scaled bohrs.',/,
     */2X,'For the path of steepest descent:',
     */6X,'Max no. of steps in each direction = ',I7,
     */6X,'Hessian grid multiple (INH) = ',I5,
     */6X,'Path stopped if s > ',F10.6,' or s < ' ,F10.6)
 1260 FORMAT(/2X,'All s values and step sizes are in ',                 0405JZ07
     *           'mass-scaled angstroms.',/,
     */2X,'For the path of steepest descent:',
     */6X,'Max no. of steps in each direction = ',I7,
     */6X,'Hessian grid multiple (INH) = ',I5,
     */6X,'Path stopped if s > ',F10.6,' or s < ' ,F10.6)
 1251 FORMAT(/2X,'The initial step is in the direction of positive s.') 1203WH92
 1252 FORMAT(/2X,'The initial step is in the direction of negative s.') 1203WH92
 1254 format(6x,'or V becomes smaller than',/6x,'[',                    0423TA02
     *    f5.3,' * V(saddle point) + ',f5.3,' * V(rorp)]',/6x,          0423TA02
     *    'where V(rorp) is the higher of reactants or products.')      0423TA02
 1255 format(5x,'or Va^G becomes smaller than',/6x,'[',                 0423TA02
     *    f5.3,' * V+ZPE(saddle point) + ',f5.3,' * V+ZPE(rorp)]',/6x,  0423TA02
     *    'where V+ZPE(rorp) is the higher of reactants or products.')  0423TA02
 1261 FORMAT(/2X,'The direction of the unbound eigenvector at the ',
     *    'saddle point ',/2X,'is toward the product side.')
 1262 FORMAT(/2X,'The direction of the unbound eigenvector at the ',
     *    'saddle point ',/2X,'is toward the reactant side.')
 1350 FORMAT(/2X,'Extra mode information is printed between s =',       1201WH92
     * F9.4,' and',F9.4)
 1450 FORMAT(/2X,'Use fixed-step Euler integrator with no',
     * ' stabilization')
 1460 FORMAT(/2X,'Use fixed-step Euler integrator with no',             7/1/91VM
     * ' stabilization',/2X,'Evaluation of the reaction path at the',   7/1/91VM
     * ' saddle point is ',/2X,'within the local cubic',                7/1/91VM
     * ' approximation to the energy')                                  7/1/91VM
 1550 FORMAT(/2X,'Use fixed-step Euler integrator with stabilization',
     * /2X,'by the Dupuis-Gordon method (ES1).')
 1560 FORMAT(/2X,'Use fixed-step Euler integrator with stabilization',  7/1/91VM
     * /2X,'by the Dupuis-Gordon method (ES1).',
     * /2X,'Evaluation of the reaction path at the saddle point is',
     * /2X,'within the local cubic approximation to the energy')
 1660 FORMAT(/2X,'Use fixed-step local quadratic (Page-McIver)',        5/23B88
     * ' integrator.')                                                  5/23B88
 1670 FORMAT(/2X,'Use fixed-step local quadratic (Page-McIver)',        7/1/91VM
     * ' integrator.',
     * /2X,'Evaluation of the reaction path at the saddle point is',
     * /2X,'within the local cubic approximation to the energy.')       7/1/91VM
 1700 FORMAT(/2X,'For the first ',I5,' steps,   DEL1  =', 1PE13.5,
     *       /2X,'and for the remaining steps, DEL   =', 1PE13.5)       1019WH92
 1900 FORMAT(/2X,'Detailed reaction path information printed if |s|',
     * ' > ',F10.6)
 2100 FORMAT(/2X,'Special save points:',/,(5F11.6))
 2130 FORMAT(/1X,21('*'),' Detailed reaction path information ',
     *           21('*'),
     *      //23X,'(s in mass-scaled bohrs)',
     *       /23X,'(V in hartrees)',
     *       /23X,'(X,Y,Z in unscaled bohrs)',
     *       /23X,'(DX,DY,DZ in unscaled hartree/bohr)')
 2140 FORMAT(/1X,21('*'),' Detailed reaction path information ',        0405JZ07
     *           21('*'),
     *      //23X,'(s in mass-scaled angstroms)',
     *       /23X,'(V in hartrees)',
     *       /23X,'(X,Y,Z in unscaled angstroms)',
     *       /23X,'(DX,DY,DZ in unscaled hartree/bohr)')
 2850 FORMAT(/2X,'For stabilization, DELTA2=', 1PE13.5,', DIFFD=',E13.5,
     * /2X,'DIFFD is referred as OMEGA in Ref. 17 of ',
     *     'the POLYRATE manual bibliography.')                         150191VM
C end init_path
      RETURN
      END SUBROUTINE INIT_PATH

      SUBROUTINE SAVE_POS_PRINT(DXB,DXOLD,IST,LBCALC,LGRDSV,LGS4,       1227BL04
     *                          LLSAVE,STEP0,STEPX,XOLD,fisen)
      use perconparam
      use common_inc !, only : lgs,dx,sspec
      use kintcm
      use rate_const
      IMPLICIT REAL(8) (A-H,O-Z)
c
      LOGICAL LGRDSV,LBCALC,LLSAVE                                      1227BL04
      DIMENSION DXB(N3TM),DXOLD(N3TM),XOLD(N3TM)                        1227BL04
      IF(.NOT.ALLOCATED(X)) ALLOCATE(X(N3TM))
C
         STEP0 = STEPX
         IF ((IRODS.EQ.1.OR.IVRP.EQ.1).AND.LGRDSV) THEN                 0325PF98
            DO 65 I = 1, N3                                             0325PF98
               XOLD(I) = X(I)                                           0325PF98
               DXB(I) = DXOLD(I)                                        0325PF98
               DXOLD(I) = DX(I)                                         0325PF98
   65       CONTINUE                                                    0325PF98
            LGRDSV = .FALSE.                                            0325PF98
         ENDIF                                                          0325PF98
C
         IF (IRODS.EQ.0.AND.IVRP.EQ.0) THEN                             0325PF98
            DO 60 I = 1, N3
               XOLD(I) = X(I)
               IF (LBCALC) DXB(I) = DXOLD(I)
               DXOLD(I) = DX(I)
   60       CONTINUE
         ENDIF                                                          0325PF98
C
         IF (IRODS.EQ.1.AND.LRODS.AND.IST.NE.1) THEN                    0325PF98
            DO 70 J = 1, N3                                             0325PF98
               DX(J) = GGUARD(J)                                        0325PF98
   70       CONTINUE                                                    0325PF98
            LRODS = .FALSE.                                             0325PF98
         ENDIF
C
         DXMAGO = DXMAG                                                 1/3/91VM
C
C                Reset step size                                        5/23B88
C                                                                       5/23B88
         IF (IST.GT.NST0) THEN                                          5/23B88
            STEPX = DEL                                                 5/23B88
         ELSE                                                           5/23B88
            STEPX = DEL1                                                5/23B88
         ENDIF                                                          5/23B88
C
      LLSAVE = .FALSE.
      IF (LGS(11).NE.0) THEN
C
C                Check for special save point and signal to print out
C                normal mode eigenvectors if desired - added 25-Mar-85
C
         LGS(4) = LGS4
         DO 75 ISPEC = 1, NSPEC
            TEST = 0.0D0
            IF (FISEN.GT.0.0D0.AND.SSPEC(ISPEC).GT.0.0D0) TEST =
     *          SSPEC(ISPEC)-S
            IF (FISEN.LT.0.0D0.AND.SSPEC(ISPEC).LT.0.0D0) TEST = S-
     *         SSPEC(ISPEC)
            IF (TEST.GT.EPSX.AND.TEST.LT.1.1D0*STEPX) THEN
               LLSAVE = .TRUE.
               IVAR = 1                                                 5/6/90VM
               STEPX = TEST
               IF (LGS(11).EQ.2) LGS(4) = 2
               SSPEC(ISPEC) = 0.0D0
            ENDIF
   75    CONTINUE
      ENDIF
C
C                Compute and/or write out rph data at save points
C                and integrate one step
C
C                Check if a special step should be taken to hit a save
C                   point
C
         IF (.NOT.LLSAVE) THEN
            IF (LGS(31) .EQ. 5 .OR. LGS(31) .EQ. 35) THEN               011591VM
               IF (MOD(IST,INH).EQ.0) IVAR = 1                          1105PF97
            ENDIF
         ENDIF



C END SAVE_POS_PRINT



      RETURN
      END SUBROUTINE SAVE_POS_PRINT

      SUBROUTINE WRITEOUT_STEP(IWRDX,LLSAVE,SBX)
      use common_inc
      use perconparam
      use rate_const
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1227BL04
      LOGICAL LLSAVE                                                    1227BL04
      DIMENSION SBX(3)                                                  1227BL04
C
         IF (LGS(30).EQ.-1) THEN                                        0810JC97
           IF (IWRDX.EQ.1) THEN
C
C   Write DX in fu30 with the same coordinate system as LLOPT(3)        0812YC97
C
C             unscaled DX                                               0812YC97
C
              IF (LPGRD.EQ.0.OR.LPGRD.LE.-2) THEN                       0812YC97
                  WRITE (FU30,2750) S,(DX(I)*AMASS(I)*DXMAG,I=1,N3)     0812YC97
C
C             mass-scaled -DX                                           0812YC97
C
              ELSE IF (LPGRD.EQ.99) THEN                                0812YC97
                  WRITE (FU30,2750) S,(-DX(I)*DXMAG,I=1,N3)             0812YC97
C
C             unscaled   -DX                                            0812YC97
C
              ELSE IF (LPGRD.EQ.100) THEN                               0812YC97
                  WRITE (FU30,2750) S,(-DX(I)*AMASS(I)*DXMAG,I=1,N3)    0812YC97
C
C             mass-scaled DX                                            0812YC97
C
              ELSE                                                      0812YC97
                  WRITE (FU30,2750) S,(DX(I)*DXMAG,I=1,N3)              0812YC97
              ENDIF                                                     0812YC97
            IWRDX = 0
           ENDIF
         ENDIF                                                          0810JC97
C
         IF (LGS(30).EQ.-3) THEN                                        0810JC97
C
           WRITE (FU31,2102)                                            0810JC97
2102       FORMAT (//, 1X,'*POINT')                                     0810JC97
           WRITE (FU31,2104) S                                          0810JC97
           WRITE (FU31,2106) V                                          0810JC97
2104       FORMAT (/,1X,'  SMEP',5x, 1PE19.10)                          0810JC97

2106       FORMAT (/,1X,'  VMEP',5x, 3E20.10)                           0810JC97

           WRITE (FU31,1298)                                            0810JC97
1298       FORMAT (/,1X,'  GEOM')                                       0810JC97
           IF (LLOPT(3).EQ.0.OR.LLOPT(3).EQ.-1.or.LLOPT(3)              0810JC97
     *        .eq.100.or.llopt(3).eq.99) THEN                           0810JC97
C
C   Write out unscaled X     1026YC96
C
            WRITE (FU31,1300) (X(I)/AMASS(I),I=1,N3)                    0810JC97
           ELSE                                                         0810JC97
C
C   Write out mass-scaled X  1026YC96
C
            WRITE (FU31,1300) (X(I),I=1,N3)                             0810JC97
1300        FORMAT (1X, 1PE19.10, 2E20.10)                              0810JC97

           ENDIF                                                        0810JC97
          WRITE (FU31,1302)                                             0810JC97
1302      FORMAT (1X,'  END')                                           0810JC97
C
C   Write out rph information, DX is mass-weighted
C
           WRITE (FU31,1304)                                            0810JC97
1304       FORMAT (/,1X,'  GRADS')                                      0810JC97
         IF (LLOPT(3).EQ.0.OR.LLOPT(3).LE.-2) THEN                      0810JC97
C
C   Write out unscaled DX, un-normalized  1026YC96
C
            WRITE (FU31,1300) (DX(I)*AMASS(I)*DXMAG,I=1,N3)             0810JC97
         ELSE IF (LLOPT(3).EQ.99) THEN                                  0810JC97
C
C   Write out mass-scaled forces, un-normalized   1026YC96
C
            WRITE (FU31,1300) (-DX(I)*DXMAG,I=1,N3)                     0810JC97
         ELSE IF (LLOPT(3).EQ.100) THEN                                 0810JC97
C
C   Write out unscaled, un-normalized forces  1026YC96
C
            WRITE (FU31,1300) (-DX(I)*AMASS(I)*DXMAG,I=1,N3)            0810JC97
C
C   Write out mass-scaled DX, un-normalized   1026YC96
C
         ELSE                                                           0810JC97
            WRITE (FU31,1300) (DX(I)*DXMAG,I=1,N3)                      0810JC97
         ENDIF                                                          0810JC97
         WRITE (FU31,1302)                                              0810JC97
         ENDIF                                                          0810JC97
C
 2750 FORMAT(' s,DX='/ 1X, 1PE19.10/ (1X, 1PE19.10, 2E20.10))           0117GL92
      RETURN
      END subroutine WRITEOUT_STEP

      SUBROUTINE WRITEOUT_STEP2(IWRDX,LLSAVE,SBX,DXOLD)
      use common_inc
      use perconparam
      use kintcm
      use rate_const; use cm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1227BL04
c
      LOGICAL LLSAVE                                                    1227BL04
      DIMENSION SBX(3),DXOLD(N3TM)                                      1227BL04
         IF (LLSAVE.AND.LGS(30).LT.0) THEN
C
C                   Write out rph information, DX is mass-weighted
C
            CALL RPHWRT (9)
            IF (LGRAD.GE.3) THEN
C
C   Write DX in fu30 with the same coordinate system as LLOPT(3)        0812YC97
C
C             unscaled DX                                               0812YC97
C
              IF (LPGRD.EQ.0.OR.LPGRD.LE.-2) THEN                       0812YC97
                  WRITE (FU30,2750) SBX(2),
     *                               (DXOLD(I)*AMASS(I)*DXMAGO,I=1,N3)  1128JC97
C
C             mass-scaled -DX                                           0812YC97
C
              ELSE IF (LPGRD.EQ.99) THEN                                0812YC97
                  WRITE (FU30,2750) SBX(2),(-DXOLD(I)*DXMAGO,I=1,N3)    1128JC97
C
C             unscaled   -DX                                            0812YC97
C
              ELSE IF (LPGRD.EQ.100) THEN                               0812YC97
                  WRITE (FU30,2750) SBX(2),
     *                               (-DXOLD(I)*AMASS(I)*DXMAGO,I=1,N3) 1128JC97
C
C             mass-scaled DX                                            0812YC97
C
              ELSE                                                      0812YC97
                  WRITE (FU30,2750) SBX(2),(DXOLD(I)*DXMAGO,I=1,N3)     1128JC97
              ENDIF                                                     0812YC97
              IF (LGRAD.GE.4) IWRDX = 1
            ENDIF
         ENDIF
C
         IF ((IPRSTP.EQ.1).OR.(LGS(4).NE.0.AND.LLSAVE)) THEN            0507YC97
C
C                   Print geometry, potential and gradient
C
            IF (ABS(S).GE.SPRNT) THEN
               WRITE(FU6,2140) S,V                                      0613WH94
               IF (LGS2(5) .GT. 0)                                      9/10GL91
     *             WRITE (FU6,2150) (POTINF(I),I=1,LGS2(5))             9/10GL91
               WRITE (FU6,2200)
     *           (J,(X (3*J-3+I)/AMASS(3*J-3+I),I=1,3),J=1,NATOM)       0613WH94
               IF (LBATH) WRITE(fu6,2251) X(N3)/AMASS(N3)               0317Yc99
C
               WRITE (FU6,2250)
     *           (J,(DX(3*J-3+I)*DXMAG*AMASS(3*J-3+I),I=1,3),J=1,NATOM) 0613WH94
               IF (LBATH) WRITE(fu6,2251) DX(N3)*DXMAG*AMASS(N3)        0317Yc99
C
            ENDIF
         ENDIF
 2140 FORMAT(/1X,'s = ',F9.5,'    V = ',F14.10)
 2150 FORMAT(/1X,'POT inf : ',/(4X,1P,4E15.6))                          0613WH94
 2251 FORMAT (1X,'EFF SOLVENT',12X,E15.6)                               0317Yc99
 2200 FORMAT(/2X,'Atom',11X,'X',14X,'Y',14X,'Z',/,(I5,4X,1P,3E15.6))    0613WH94
 2250 FORMAT(/2X,'Atom',10X,'DX',13X,'DY',13X,'DZ',/,(I5,4X,1P,3E15.6)) 0613WH94
 2750 FORMAT(' s,DX='/ 1X, 1PE19.10/ (1X, 1PE19.10, 2E20.10))           0117GL92
      RETURN
      END SUBROUTINE WRITEOUT_STEP2

      SUBROUTINE CALC_CURV(DXB,DXOLD,FISEN,IDIREC,LBCALC,LCOUNT,LDEL,
     *                     LGS4,N3M7,IST,NFUNC,SBX,STEPX,BKAP)
      use common_inc
      use perconparam
      use kintcm
C                                                                       1227BL04
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1227BL04
c
      LOGICAL LEXIT,LBCALC                                              1227BL04
      DIMENSION DXB(N3TM),DXOLD(N3TM)                                   1227BL04
      DIMENSION SBX(3)
C
      IF (LGS(30).LE.0) THEN
         WRITE (FU6,2400) IDIREC,IST,NFUNC
      ENDIF
      LGS(4) = LGS4
C
      IF (LGS(3) .NE. 3) THEN
        IF (LCDSC) THEN                                                 2/10GL91
           IF (IRODS.EQ.1.OR.IVRP.EQ.1) LBCALC = .TRUE.                 0325PF98
           IF (LBCALC) THEN
C
C               Compute B for the last step and effective masses at
C               previous grid point
C
              IFLAG = -1
              LCOUNT = LCOUNT+1
              CALL BCALC (SBX,DXOLD,DXB,STEPX,LDEL,
     *                     IFLAG,LCOUNT,BKAP)                           0812YC97
           ENDIF
C
C            Calculate effective masses at last grid point
C
           IFLAG = 1
           CALL BCALC (SBX,DXOLD,DXB,STEPX,LDEL,IFLAG,LCOUNT,BKAP)
        ENDIF
C
        IF(IDIREC.EQ.LGS(12).OR.LGS(12).EQ.3) THEN
           CALL EXTRAP (LDEL,FISEN,IDIREC)
        ENDIF
      ENDIF
2400  FORMAT(/1X,'For direction', I3,                                   0613WH94
     * /5X,'number of integration steps was ',I8,
     * /5X,'number of function calls was    ',I8)
      RETURN
      END SUBROUTINE CALC_CURV

      SUBROUTINE INIT_SAVE(BKAP,N3M7,szpesp,varorp,vmepdw,VOLD,
     *                     vreact,vrorp,vzpedw,xsign,PARALLEL)
      use rate_const, only : vecsv,fracdw
      use common_inc
      use kintcm, only : isstop
      use perconparam, only : n3tm,maxcar
      use path_mod
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL :: PARALLEL
      if(.not.allocated(vecsv)) allocate(vecsv(maxcar))
      IF (LGS(1).NE.0) THEN
            XSIGN = 1.0D0
            IF (LGS(27).EQ.-1) XSIGN = -XSIGN
C
C           Save initial direction vector
C
            DO 20 I = 1, N3
               VECSV(I) = COF(I,1)*XSIGN
               DX(I) = VECSV(I)                                         3/6/91BG
   20       CONTINUE
C
            IF (LGS(31) .GE. 30 .AND. LGS(31) .LE. 35) THEN             7/1/91VM
               VOLD = V
               DO 1 I=1,N3                                              7/1/91VM
                  XS(I)=X(I)                                            7/1/91VM
   1           CONTINUE                                                 7/1/91VM
               CALL CUBST                                               6/8/94RS
               V = VOLD                                                 1231WH94
            ENDIF                                                       7/1/91VM
C
C        Put s.p. info into RESCOM arrays
C
         IF (PARALLEL) THEN
           CALL SSAVE (LSAVE,BKAP,1)
         ELSE
           CALL SSAVE (LSAVE,BKAP,0)
         ENDIF
C
C     Determine the values for V or VaG when special stop is desired
C     and save the value of ZPE for the saddle point.
C
         if (isstop.ne.0) then                                          0423TA02
            vreact = 0.d0                                               0423TA02
            vrorp = dmax1(vreact,eprd)                                  0423TA02
            varorp = dmax1(var,vap)                                     0423TA02
            vmepdw = (1-fracdw)*v + fracdw*vrorp                        0423TA02
            vzpedw = (1-fracdw)*vad + fracdw*varorp                     0423TA02
            szpesp = vad - v                                            0423TA02
         endif                                                          0423TA02
C
      ENDIF
      RETURN
      END SUBROUTINE INIT_SAVE

      SUBROUTINE WRITEOUT_ENDPATH(IBEG,LNCOL,IEND)
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c     Write out MDMOVIE file if MDMOVIE keyword (imdmov) is used.
c
      if (imdmov.eq.1) then
         DO 84 I = 1,LSAVE
           if(imdmov.eq.1) write(FU65,2551)ssubi(i),vclas(i)*ckcal,     10/9RS95
     *        ((GEOM(3*K-3+J,I)/AMASS(3*K-3+J),J=1,3),K=1,NATOM)
   84    CONTINUE
      end if
C
C        Write out summary of reaction path geometries
C
      IF (LGS(3).EQ.2 .AND. LGS(4) .EQ. 0) THEN                         0613WH94
         WRITE (FU6,2410)
         DO 85 I = 1,LSAVE
            WRITE(FU6,2550)SSUBI(I),                                    0613WH94
     *        (K,(GEOM(3*K-3+J,I)/AMASS(3*K-3+J),J=1,3),K=1,NATOM)
   85    CONTINUE
      ELSE IF (LGS(3) .EQ. -2) THEN
         IBEG = LNCOL
         IEND = LSAVE
         DO 95 I = IBEG,IEND
            WRITE(FU6,2550)SSUBI(I),                                    0613WH94
     *        (K,(GEOM(3*K-3+J,I)/AMASS(3*K-3+J),J=1,3),K=1,NATOM)
   95    CONTINUE
      ENDIF
C
C     Print MEP information to FU6
C
      CALL MEPOUT                                                       9/30WH92
C
C
 2410 FORMAT(/1X,6(1H*),' Space-fixed cartesian coordinates vs'         0613WH94
     *,' reaction coordinate (a.u.) ',6(1H*)/)
 2550 FORMAT(/1X,'s = ',F10.5,
     *       /1X,'Atom',12X,'X',14X,'Y',14X,'Z',/,(I5,4X,1P,3E15.6))    0613WH94
 2551 FORMAT(1X,F10.5,5x,f9.4,
     *       /(4X,1P,3E15.6))                                           1009RS95
      RETURN
      END SUBROUTINE WRITEOUT_ENDPATH

      SUBROUTINE CHECK_SAVE_STABILIZE(BKAP,FISEN,LDEL,LCOUNT,MARR,
     *                                MARRSP,LBCALC,LLSAVE,LMORE,LGRDSV,0101BL05
     *                                LFIRST,DXB,DXOLD,SBX,XOLD,IST,    0101BL05
     *                             NFUNC,SLAST,STEPX,IPATH_FREQ,NEXTTAG,0101BL05
     *              LBUSY,PARALLEL,JRUN,GEOMM,NPATH_FREQ,NJRUN,DERIVM,  0101BL05
     *              LSAVES)
      use common_inc
      use perconparam
      use rate_const
      use kintcm; use energetics_mod, only : fsp
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION JRUN(512)
      LOGICAL LBCALC,LLSAVE,LMORE,LGRDSV,LFIRST,LBUSY(512),PARALLEL     0101BL05
      DIMENSION DXB(N3TM),DXOLD(N3TM),XOLD(N3TM),SBX(3),IPATH_FREQ(NSDM)0101BL05
      DIMENSION GEOMM(N3TM,NSDM)
      DIMENSION LSAVES(NSDM)
      DIMENSION DERIVM(N3TM,NSDM)
C                Check if this point should be saved
C
         IF (.NOT.LLSAVE) THEN
            IDIV=MAX(INT(DEL/DEL1),1)                                   1202JC97
            IF (IST.GT.NST0) THEN                                       1202JC97
                IF (NST0.EQ.0) THEN                                     1202JC97
                   IF (MOD(IST,INH).EQ.0) LLSAVE = .TRUE.               1202JC97
                ELSE                                                    1202JC97
                   INEFST = IST - NST0 + INT(NST0/(IDIV))               1202JC97
                   IF (MOD(INEFST,INH).EQ.0) LLSAVE = .TRUE.            1202JC97
                ENDIF                                                   1202JC97
            ELSE                                                        1202JC97
                ISFST = INH * IDIV                                      1202JC97
                IF (MOD(IST,ISFST).EQ.0) LLSAVE = .TRUE.                1202JC97
            ENDIF                                                       1202JC97
         ENDIF
C
C                   Check if stabilization is to be performed.
C                   Stabilization allowed only with Euler one-step
C
         IF (LGS(31) .EQ. 2 .OR. LGS(31) .EQ. 32) THEN                  7/1/91VM
C
C                   Modified Schmidt, Gordon, Dupuis method (ES1)
C
            S = S-FISEN*STEPX
            DELTA1 = STEPX
            CALL IRCX (DELTA1,DELTA2,DIFFD,FISEN,XOLD,DXOLD,NFUNC)      150191VM

            LLSAVE = LLSAVE.AND.DELTA1.EQ.STEPX
         ENDIF
C
         IF (IRODS.EQ.0.AND.IVRP.EQ.0) THEN                             0325PF98
C                Shift storage of s used in calculating B's.
C
            SBX(3) = SBX(2)
            SBX(2) = SBX(1)
            SBX(1) = S
C
C                Check if B should be computed
C
            IF (LBCALC) THEN
               LCOUNT = LCOUNT+1
               IFLAG = 0
               IF (IST.EQ.1) IFLAG = -1
C
C                   Compute B for the last step and effective masses at
C                   previous grid point
C
               CALL BCALC (SBX,DXOLD,DXB,STEPX,LDEL,IFLAG,
     *                     LCOUNT,BKAP)                                 0812YC97
               LBCALC = .FALSE.
               IF (LGS(30).LT.0.AND.LGRAD.EQ.1)
     *            WRITE (FU30,2800) (BCURV(I),I=1,N3M7)
            ENDIF
         ENDIF                                                          0325PF98
C
C                Check for save point
C
         IF (LLSAVE) THEN
            IF (IRODS.EQ.1.OR.IVRP.EQ.1) THEN                           0325PF98
               SBX(3) = SBX(2)                                          0325PF98
               SBX(2) = SBX(1)                                          0325PF98
               SBX(1) = S                                               0325PF98
               LGRDSV = .TRUE.                                          0325PF98
            ENDIF                                                       0325PF98
            SLAST = S
            IF(LGS(3) .EQ. 3) THEN
              IOP = 1                                                   6/20T88
            ELSE                                                        6/20T88
              IOP = 3                                                   6/20T88
            ENDIF                                                       6/20T88
C
C                   Compute V, Grad and normal modes at new geometry
C                   If LGS(3) = 3, IRC only, set IOP=1 for cal Grad only
C
            IF ((LGS(31) .EQ. 5 .OR. LGS(31) . EQ. 35)                  1/3/91VM
     *         .AND. LGS(29) .EQ. 2) THEN                               1/3/91VM
               DO 71 I = 1, N3                                          1/3/91VM
               DO 71 J = 1, N3                                          1/3/91VM
                  FSP(I,J) = F(I,J)                                     1/3/91VM
   71          CONTINUE                                                 1/3/91VM
            ENDIF                                                       1/3/91VM
            IF (LGS(5).GT.21) THEN                                      6/30YL91
               IF (MARR.GT.1.AND.S.LT.SRARR(MARR-1)) THEN                  ..
                  MARR = MARR -1                                           ..
               ELSEIF (MARR.LT.NARR.AND.S.GE.SRARR(MARR)) THEN             ..
                  MARR = MARR + 1                                          ..
               ENDIF                                                       ..
            ENDIF                                                          ..
            DO 72 IFRQ = 1, N3M7                                           ..
               MODE(IFRQ) = MODETS(MARR,IFRQ)                              ..
72          CONTINUE                                                    6/30YL91
C
            IF (.NOT.LFIRST.AND.(IRODS.EQ.1.OR.IVRP.EQ.1)) THEN         0325PF98
               DO 110 I = 1, N3M7                                       0325PF98
                 DO 115 J = 1, N3                                       0325PF98
                   COFTSV(J,I) = COF(J,I)                               0325PF98
 115             CONTINUE                                               0325PF98
 110           CONTINUE                                                 0325PF98
            ENDIF                                                       0325PF98
C
C            write(6,*) 'calling normod  ',IOP,STEPX,FISEN
            if (PARALLEL) then
              IPATH_FREQ(IST/INH)=NEXTTAG                               0106BL05
              CALL UPDATE_STATUS(LBUSY)
              CALL NEXT_AVAIL_PROC(LBUSY,IPROC)                         0106BL05
              LSAVES(IST/INH) = LSAVE
              DO M=1,N3TM
                GEOMM(M,IST/INH) = X(M)
                DERIVM(M,IST/INH) = DX(M)
              ENDDO
C
              NPATH_FREQ=NPATH_FREQ+1
C
              CALL FREQ_SUBMIT(X,NEXTTAG,IPROC,STEPX,JRUN,NJRUN)        0202BL05
            else
              CALL NORMOD(IOP,STEPX,FISEN)                              1118PF97
            endif
C
C****************************************************************************
C   For LGS(35)=1; the computation of the bond orders and charges in
C   this subprogram has been removed in version 5.0.                 1021GL92
C****************************************************************************
C
C                   Check if B should be computed on next pass
C
            IF (IRODS.EQ.1.OR.IVRP.EQ.1) THEN                           0325PF98
               IF (.NOT.LFIRST) LBCALC = LCDSC                          0325PF98
               LFIRST = .FALSE.                                         0325PF98
            ENDIF                                                       0325PF98
C
            IF (IRODS.EQ.0.AND.IVRP.EQ.0) LBCALC = LCDSC                0325PF98
C
            if (imeff.eq.1)lbcalc = .true.                              1010RS95
C
         ENDIF
 2800 FORMAT(' BF=', /, (1X, 1PE19.10, 3E20.10))
C end check_save
      RETURN
      END SUBROUTINE CHECK_SAVE_STABILIZE

C Subroutine that deletes old outputfiles
      subroutine del_old_units
      logical :: exist
      inquire(file='poly.fu6',exist=exist)
      if(exist)call del_file('poly.fu6')
      inquire(file='poly.fu14',exist=exist)
      if(exist)call del_file('poly.fu14')
      inquire(file='poly.fu15',exist=exist)
      if(exist)call del_file('poly.fu15')
      inquire(file='poly.fu22',exist=exist)
      if(exist)call del_file('poly.fu22')
C
      inquire(file='esp.fu61',exist=exist)
      if(exist)call del_file('esp.fu61')
      end subroutine del_old_units

C Delete a file 
      subroutine del_file(fname)
      character(*) fname
      open(unit=111,file=trim(fname),status='old')
      close(111,status='delete')
      end subroutine del_file

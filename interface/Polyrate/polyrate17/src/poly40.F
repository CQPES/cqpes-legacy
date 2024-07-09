C***********************************************************************
C  CONV40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE conv40(IOP,IS,N,NFREQ,found)
      use common_inc, only : x,f
      use perconparam
      use fu_40
C
C This subroutine converts the geometries, gradients, and hessians to
C atomic units.
C
C     CALLED BY:  RD40RP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      logical found(21)
      character*10 geomu,gradu,hessu
C
C C1 = angstrom/bohr,  C2 = hartree/joule
      parameter (C1 = .5291772, C2 = 2.293710E17)
C
      geomu = gmunit
      gradu = gdunit
      hessu = hsunit
      if (geomu(1:2).eq.'MS') geomu = geomu(3:)
      if (gradu(1:2).eq.'MS') gradu = gradu(3:)
      if (hessu(1:2).eq.'MS') hessu = hessu(3:)
C
C if GEOM was read in and X not already in bohr...
      if (found(7).and.geomu.ne.'BOHR') then
C set conversion constant C
         if (geomu.eq.'ANG') then
            C = 1.0D0 / C1
         elseif (geomu.eq.'SI') then
            C = 1.0D10 / C1
         else
            write(fu6,1000) 'GEOMUNIT'
            stop 'conv40 1'
         endif
         do 10 I = 1,N
            X(I) = C * X(I)
 10      continue
      endif
C
C if gradient read in and units not already hartree per bohr...
      if (gradu.ne.'BOHR') then
C  set conversion constant C
         if (gradu.eq.'ANG') then
            C = C1
         elseif (gradu.eq.'SI') then
            C = C2 * C1 * 1.0D-10
         else
            write(fu6,1000) 'GRADUNIT'
            stop 'conv40 2'
         endif
C  GRADS was read in
         if (found(9)) then
            do 20 I = 1,N
               DX40(I) = C * DX40(I)
 20         continue
         endif
C  GRADSB1 was read in
         if (found(11)) then
            do 30 I = 1,N
               DX140(I) = C * DX140(I)
 30         continue
         endif
C  GRADSB2 was read in
         if (found(13)) then
            do 40 I = 1,N
               DX240(I) = C * DX240(I)
 40         continue
         endif
      endif
C
C if hessian matrix read in and not already in hartree per bohr^2
      if (found(3).and.hessu.ne.'BOHR') then
C  set conversion constant C
         if (hessu.eq.'ANG') then
            C = C1**2
         elseif (hessu.eq.'SI') then
            C = C1**2 * C2 * 1.0D-20
         else
            write(fu6,1000) 'HESSUNIT'
         endif
         do 50 I = 1,N
            do 50 J = 1,N
               F(I,J) = C * F(I,J)
 50      continue
      endif
c
C
      return
C
 1000 format(1x,10A,': unit not supported')
C
      end SUBROUTINE conv40
C***********************************************************************
C  CRVVEC
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE crvvec(IOP,IS,N,NFREQ)
      use common_inc
      use perconparam
      use rate_const
      use kintcm
C
C This subroutine computes the curvature vector when unit40 input is
C used.  This code is the same as a part of RPHRD2.  (RPHRD2 is called 
C instead when unit30 input is used.)
C
C     CALLED BY:  PTSEC40
C     CALLS:  RPHDXN, RPHB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Normalize (and mass-weIght if necessary) DX
C
      CALL RPHDXN (LOPT(3),DX,DXN,AMASS,N)
      DXMAG  = DXN
      DXNORM = DXN
      IF (LOPT(6).EQ.2.AND.IRODS.EQ.0) THEN
         SB(3) = S
C
C Calculate the curvature vector BF at the previous grid point
C
C         IF (((LGS(6).LE.2.AND.IS.GT.3).OR.(LGS(6).GT.2.AND.IS.GT.2))   0318PF98
C     *      .AND.IRODS.EQ.0) THEN                                       0318PF98
C            IF (((LGS(6).LE.2.AND.IS.EQ.4).OR.(LGS(6).GT.2.AND.IS.EQ.3))0318PF98
C     *      .AND.IRODS.EQ.0) THEN                                       0318PF98
         IF (IS.GT.3) THEN
            IF (IS.EQ.4) THEN
               NBS = 2
            ELSE
               NBS = 3
            ENDIF
            ST = SS(IS-1)
            CALL RPHB (NBS,ST,N,NFREQ,SB,DX1,DX2,DX,COF,BCURV)
            DO 60 I = 1, NFREQ
               IF (dabs(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
               BFS(IS-1,IFREQ(I)) = BCURV(I)
   60       CONTINUE
         ENDIF
      elseIF (LOPT(6).GT.2) THEN
C
C Use additional vector(s) near s to compute curvature vector
C
C Normalize (and mass-weight if necessary) DX
         CALL RPHDXN (LOPT(3),DX2,DX2N,AMASS,N)           
         IF (LOPT(6).GT.3) THEN
C
C Normalize (and mass-weight if necessary) DX1
            CALL RPHDXN (LOPT(3),DX1,DXN1,AMASS,N)         
         ENDIF
      ENDIF
C
      return
c debugging
 1000 FORMAT (1X,A79,/,(2X,1P,5E15.5))
 1200 FORMAT (2X,1P,5E15.5)
 1300 FORMAT (1X,A79)
c
      end SUBROUTINE crvvec

C***********************************************************************
C  DEFLT40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE deflt40(IOP,NFREQ,found)
      use common_inc
      use perconparam, only : nvibm,fu6
      use rate_const
      use fu_40
C
C This subroutine determines if the needed unit fu40 keywords were read
C in for the IOPth species.  Default values are assigned when necessary
C for missing information (if a default for the missing keyword exists).
C
C    IOP = 1,2, read in reactant information
C    IOP = 3,4, read in product information
C    IOP = 5, read in saddle point information
C    IOP = 6, read in RPH information for points along the MEP 
C    IOP = 7, reactant well                                             0729PF97
C    IOP = 8, product well                                              0729PF97
C8   IOP = 15, read in EXTRAP40 section                                 081896PF
C
C     CALLED BY:
C                RD40RP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      dimension jfreq(nvibm)
      logical found(21)
C
C8    if (IOP .ne. 15) then                                             0801PF97
      if (IOP.ne.6) then                                                0801PF97
C
C not found(1) - S keyword was not read
      if(.not.found(1)) then
         if (IOP.gt.5) then 
            write(fu6,*)
     *'S, a required keyword, was absent in a POINT40 section'
            stop 'DEFLT40 1'
         endif
      endif
C
C not found(2) - V keyword was not read
      if(.not.found(2)) then
         write(fu6,*)
     * 'V, a required keyword, was not read in for IOP = ',IOP
         stop 'DEFLT40 2'
      endif
C
C not found(3) - HESSIAN keyword was not read
      if(.not.found(3).and.frqsrc.eq.'HESSIAN'.and.icode(iop).ne.1) then1126JC97
         write(fu6,*)
     *'FREQSOURCE=HESSIAN but the hessian was not found for IOP=',IOP
         stop 'DEFLT40 3'
      endif
C
C not found(4) - FREQ keyword was not read
      if(.not.found(4) .and. frqsrc.eq.'READ'.and.icode(iop).ne.1) then 1126JC97
         write(fu6,*)
     *'FREQSOURCE=READ but the frequencies were not found for IOP=',IOP
         stop 'DEFLT40 4'
      endif
C
C not found(5) - 3RDDERIV keyword was not read
      if(.not.found(5) .and. anhm40.ne.'NONE'.and.icode(iop).ne.1) then 1126JC97
         write(fu6,*)
     *'ANHARM<>NONE but the third derivatives were not found for IOP=',
     *IOP
         stop 'DEFLT40 5'
      endif
C
C not found(6) - 4THDERIV keyword was not read
      if(.not.found(6).and.anhm40.eq.'QUARTIC'.and.icode(iop).ne.1) then1126JC97
         write(fu6,*)
     *'ANHARM=QUARTIC but the fourth derivatives were not found for IOP
     *=',IOP
         stop 'DEFLT40 6'
      endif
C
C not found(14) - IFREQ keyword was not found
      if(.not.found(14)) then
         do 140 I = 1, NFREQ
            ifreq(I) = I
 140     continue
C
C Reorder indexes when the reaction is unimolecular or there are wells  1126JC97
C
          if (                                                          1126JC97
     *        iop.eq.7.or.                                              1126JC97
     *        iop.eq.8.or.                                              1126JC97
     *       (iop.eq.1.and.(lgs(6).eq.3.or.lgs(6).eq.4)).or.            1126JC97
     *       (iop.eq.3.and.(lgs(6).eq.2.or.lgs(6).eq.4))                1126JC97
     *       ) then                                                     1126JC97
               savifr=ifreq(nfreq)                                      1126JC97
               do i=nfreq-1,1,-1                                        1126JC97
                   ifreq(i+1)=ifreq(i)                                  1126JC97
               enddo                                                    1126JC97
               ifreq(1)=savifr                                          1126JC97
           endif                                                        1126JC97
C                                                                       1126JC97
C Reorder indexes for 2 reactants or 2 products                         1126JC97
C                                                                       1126JC97
          if (                                                          1126JC97
     *       ( (iop.eq.1.or.iop.eq.2).and.                              1126JC97
     *       (lgs(6).eq.1.or.lgs(6).eq.2) ).                            1126JC97
     *       or.                                                        1126JC97
     *       ( (iop.eq.3.or.iop.eq.4).and.                              1126JC97
     *       (lgs(6).eq.1.or.lgs(6).eq.3) )                             1126JC97
     *       ) then                                                     1126JC97
               do i=1,nfreq                                             1126JC97
                   jfreq(i)=ifreq(nfreq-i+1)                            1126JC97
               enddo                                                    1126JC97
               do i=1,nfreq                                             1126JC97
                   ifreq(i)=nf(5)+1-jfreq(i)                            1126JC97
               enddo                                                    1126JC97
           endif                                                        1126JC97
C                                                                       1126JC97
      endif
C
C not found(8) - WSTAR keyword was not read
         if(IOP.eq.5 .and. frqsrc.eq.'READ' .and. .not.found(8)) then
            write(fu6,*)
     *'FREQSOURCE=READ but WSTAR was not read in the SADDLE40 section'
            stop 'DEFLT40 8'
         endif
C
C check additional keywords for POINT40 section
C
      elseif (iop.eq.6) then                                            0801PF97
C not found(7) - GEOM keyword was not read
         if(.not.found(7)) then 
            write(fu6,*) 'The GEOM keyword was not read, IOP =',IOP
            stop 'DEFLT40 7'
         endif
C
C not found(9) - GRADS keyword was not found
         if(.not.found(9)) then 
            write(fu6,*)
     *'The GRADS keyword was not found in a POINT40 section'
            stop 'DEFLT40 9'
         endif
C
C not found(10) - SB1 keyword was not found in a POINT40 section
         if(.not.found(10) .and. grdder.ne.'NOEXTRA') then
            write(fu6,*)
     *'The SB1 keyword was not found in a POINT40 section'
            stop 'DEFLT40 10'
         endif
C
C not found(11) - GRADSB1 keyword was not found in a POINT40 section
         if(.not.found(11) .and. grdder.ne.'NOEXTRA') then
            write(fu6,*)
     *'The GRADSB1 keyword was not found in a POINT40 section'
            stop 'DEFLT40 11'
         endif
C
C not found(12) - SB2 keyword was not found in a POINT40 section
         if(.not.found(12) .and. grdder.eq.'TWOEXTRA') then
            write(fu6,*)
     *'The SB2 keyword was not found in a POINT40 section'
            stop 'DEFLT40 12'
         endif
C
C not found(13) - GRADSB2 keyword was not found in a POINT40 section
         if(.not.found(13).and.grdder.eq.'TWOEXTRA') then 
            write(fu6,*)
     *'The GRADSB2 keyword was not found in a POINT40 section'
            stop 'DEFLT40 13'
         endif
C
C not found(15) - BFS keyword was not found in a POINT40 section
         if(.not.found(15) .and. curv40.eq.'READ') then 
            write(fu6,*)
     *'The BFS keyword was not found in a POINT40 section'
            stop 'DEFLT40 15'
         endif
C
C
C Check required keywords for EXTRAP40 section                          081696PF
C
C8 Extrapolation is no longer used as of version 8.0.  This section will
C8 not be called and can be deleted in future versions.
C8
      elseif (IOP .eq. 15) then                                         081696PF
C not found(16) - SEXR keyword was not found in EXTRAP40 section        081696PF
         if (.not. found(16) .and. ifitvr.ne.0) then                    081696PF
            write(fu6,*)                                                081696PF
     &     'The SEXR keyword was not found in the EXTRAP40 section'     081696PF
            stop 'DEFLT40 16'                                           081696PF
         endif                                                          081696PF
C
C not found(17) - SEXP keyword was not found in EXTRAP40 section        081696PF
         if (.not. found(17) .and. ifitvp.ne.0) then                    081696PF
            write(fu6,*)                                                081696PF
     &     'The SEXP keyword was not found in the EXTRAP40 section'     081696PF
            stop 'DEFLT40 17'                                           081696PF
         endif                                                          081696PF
C
C not found(18) - VEXR keyword was not found in the EXTRAP40 section    081696PF
         if (.not. found(18) .and. ifitvr.ne.0) then                    081696PF
            write(fu6,*)                                                081696PF
     &     'The VEXR keyword was not found in the EXTRAP40 section'     081696PF
            stop 'DEFLT40 18'                                           081696PF
         endif                                                          081696PF
C
C not found(19) - VEXP keyword was not found in the EXTRAP40 section    081696PF
         if (.not. found(19) .and. ifitvp.ne.0) then                    081696PF
            write(fu6,*)                                                081696PF
     &     'The VEXP keyword was not found in the EXTRAP40 section'     081696PF
            stop 'DEFLT40 19'                                           081696PF
         endif                                                          081696PF
C
C not found(20) - OPTCODER keyword was not found in EXTRAP40 section    082096PF
         if (.not. found(20)) then
            write(fu6,*)                                                081696PF
     &     'The OPTCODER keyword was not found in the EXTRAP40 section' 081696PF
            stop 'DEFLT40 20'                                           081696PF
         endif                                                          081696PF
C
C not found(21) - OPTCODEP keyword was not found in EXTRAP40 section    082096PF
         if (.not. found(21)) then
            write(fu6,*)                                                081696PF
     &     'The OPTCODEP keyword was not found in the EXTRAP40 section' 081696PF
            stop 'DEFLT40 21'                                           081696PF
         endif                                                          081696PF
C
      endif                                                             081696PF
C
      return
      end SUBROUTINE deflt40
C***********************************************************************
C  EXTRP40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE extrp40
      use common_inc
      use perconparam, only : nvibm,fu6
      use rate_const
C
C
C     CALLED BY:  RPH40
C     CALLS:  RPHFIT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C8  This subroutine will not be called and can be deleted in future version
C8  of polyrate.  Extrapolation has been removed as of version 8.0
C8
      IF (LGS(34) .NE. 0) THEN                                          11/20T87
         ISHFT = 1                                                         ..
      ELSE IF (ICODE(5).EQ.3) THEN                                      11/20T87
         ISHFT = 6
      ELSE
         ISHFT = 7
      ENDIF
      NFREQ = N3-ISHFT
C
C Read in extra points for extrapolation
C
      LOPT(7) = 0
      LOPT(8) = 0
C8      call rd40extr
C
C For NEXTPT option, only option code 0 is allowed                      0821YC96
C
      IF (((IFITVR.NE.0).OR.(IFITVP.NE.0)).AND.(LGS(36).NE.0)) THEN
           WRITE (FU6,3000)                                             0821YC96
           STOP ' EXTRP40/NEXTPT '                                      0821YC96
      ENDIF                                                             0821YC96
3000  FORMAT (//' WARNING ! The NEXTPT option requires option code = 0')
C
      IF (LOPT(6).EQ.1.AND.LGS(1).GT.0) THEN
C
C Set curvature components for saddle point by linear interpolation
C
         T = -SS(ISSP-1)/(SS(ISSP+1)-SS(ISSP-1))
         DO 30 I = 1, NFREQ
            BFS(ISSP,I) = (1.0D0-T)*BFS(ISSP-1,I)+T*BFS(ISSP+1,I)
   30    CONTINUE
      ENDIF
C
C Fit long range exponential tails
C
      CALL RPHFIT (NSS,NFREQ,IERR)
C
      NINT1 = LOPT(2)
      NINTMX = NSS
CPF      IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) NINTMX = NINTMX-2              6/5S89
CPF      IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) NINTMX = NINTMX-2
         NINTMX = NINTMX - 5                                            0801PF97
         NINT1 = MIN(NINT1,NINTMX)
         NINTH = (NINT1+1)/2
C
      return
C
      end subroutine extrp40
C***********************************************************************
C  GENSEC40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE gensec40
      use common_inc
      use perconparam
      use rate_const
C
C Read in and process the gen40 section.
C
C     CALLS:  RD40GEN, LOPT40
C     CALLED BY:  RPH40
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C read in general section (gen40) keywords
      call rd40gen
C
C translate gen40 keywords to the LOPT array
      call lopt40
C
      IF (LOPT(6).GT.1.AND.LOPT(4).EQ.0) THEN
         WRITE (FU6,2600)
         STOP 'RPHSET 1'
      ENDIF
      IF (LOPT(2).EQ.-1) LOPT(2)=500                                    1202JC97
C
C set number of degrees of freedom (?)
      if (LGS(34) .ne. 0) then
         N3M7 = N3-1
      else
         N3M7 = N3-7
      endif
C
      DO 20 IS = 1, NSDIM
         DO 10 I = 1, N3M7
            WS(IS,I) = 0.0D0
            XK3S(IS,I) = 0.0D0
            XK4S(IS,I) = 0.0D0
            BFS(IS,I) = 0.0D0
            FMIRS(IS,I) = 0.0D0                                         0925JC97
 10      CONTINUE
         VS(IS) = 0.0D0
         FMOMS(IS) = 0.0D0
 20   CONTINUE
      ISSP = -1
C
      return
C
 2600 FORMAT (/1X,'You have selected the omegas to be read in directly',0622WH94
     */1X,'and the BFs to be computed.  However, to compute the BFs',
     */1X,'it is necessary to read in the F matrix.')
C
      end SUBROUTINE gensec40
C***********************************************************************
C  IMOM40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE imom40(IOP,IS,N,NFREQ)
      !use common_inc, only : xxc,x,lgs,icode,fmom
      use common_inc
      use perconparam
      use rate_const
C
C This subroutine computes the moment of inertia when unit40 input is
C used.  This code is nearly the same as part of RPHRD2.
C
C     CALLED BY:  PTSEC40, SADSEC40
C     CALLS:  RPHTRX, CENTER
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      if(lopt(1).gt.1) write(fu6,1000) 'X',(X(I),I=1,N)
C
C If in mass-weighted change to cartesian
C
      IF (LOPT(3).LE.-2.OR.LOPT(3).GE.1) CALL RPHTRX (N,AMASS,X,2)
C
C Copy the Cartesian coordinates before they are being shifted.
C
      DO 55 I = 1, N                                                    06/95KAN
         XXC(I) = X(I)                                                  06/95KAN
         XXS(I,IS)=X(I)                                                 0429JC97
   55 CONTINUE                                                          06/95KAN
C
C Shift origin to center of mass and compute moment of inertia
C
      KOP = iabs(IOP)                                                     6/3T90
C
      IF(LGS(34).NE.0 .AND. ((IOP.GT.4.AND.IOP.NE.7.AND.IOP.NE.8)       0801PF97
     *                      .OR. ICODE(KOP).LT.0))THEN                  0801PF97
         FMOMS(IS) = 1.0D+30                                            11/20T87
      ELSE
         CALL CENTER (5,1)                                              0712YC99
         FMOMS(IS) = 1.0D0/FMOM(5)
      ENDIF
      CALL RPHTRX (N,AMASS,X,1)
C
      return
c debugging
 1000 FORMAT (1X,A2,/,(2X,1P,5E15.5))
 1200 FORMAT (2X,1P,5E15.5)
 1300 FORMAT (1X,A79)
c
      end SUBROUTINE imom40
C***********************************************************************
C  IRFGT0
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE irfgt0(IOP,IS,N,NFREQ)
      use common_inc
      use perconparam
      use rate_const
      use kintcm
      use fu_40
C
C This subroutine performs further calculations needed when unit40 input
C is used.  This is similar to part of RPHRD2.  (RPHRD2 is called instead
C when unit30 input is used.)  The code deals with the frequencies,
C hessians, and curvature vectors.
C
C     CALLED BY:  RPSEC40, PTSEC40, SADSEC40
C     CALLS:  RPHTRF, ICFDIAG, FDIAG, ANHARM, ZEROPT, NOROUT, RPHB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      real(8), allocatable :: savcof(:,:)
      logical :: issad
      SAVE SAVCOF
      if(.not.allocated(savcof))then
        allocate(savcof(n3tm,n3tm)); savcof=0.d00
      end if
C
      if(lopt(1).gt.1) write(fu6,*) 'IFREQ',(IFREQ(I),I=1,NFREQ)
c
      ISHFT = N-NFREQ
      IF (IOP.LE.4.OR.IOP.EQ.7.OR.IOP.EQ.8) THEN                        0801PF97
         IOPF = -IOP
      ELSEIF (IOP.EQ.5) THEN
         IOPF = 2
      ELSE
         IOPF = 3
      ENDIF
      IF (LOPT(4).NE.0) THEN
C
         if (lopt(1).gt.1) then
            write(fu6,*) hsform,' HESSIAN'
            JF = N
            do 70 I = 1, N
               if (lopt(4).lt.0) JF = I
               write(fu6,1200) (F(I,J),J=1,JF)
 70         continue
         endif
c
C
         IF (LGS(36).EQ.0) THEN                                         1110DL89
C
C Transform to mass-weighted coordinates
C
            IF (iabs(LOPT(4)).EQ.1) CALL RPHTRF (N,AMASS,F,1)
C
C Diagonalize F matrix
C
            IF (IRODS.EQ.1.AND.IOP.EQ.6) THEN                           0317PF98
               DO 75 I = 1, N3TM                                        0317PF98
                  DO 76 J = 1, N3TM                                     0317PF98
                     SAVCOF(I,J) = COF (I,J)                            0317PF98
 76               CONTINUE                                              0317PF98
 75            CONTINUE                                                 0317PF98
               CALL DORODS (IOPF,IS)                                    0318OF98
            ENDIF                                                       0317PF98
C
            IF((LGS2(39).NE.0.AND.IOP.GE.6.AND.IOP.NE.7.
     *                           AND.IOP.NE.8))THEN 
               issad=.false.
               CALL ICFDIAG (IOP,ISSAD)                                 0317Yc99
            ELSE                                                        07/95KAN
               CALL FDIAG (IOPF)                                        07/95KAN
            ENDIF                                                       07/95KAN
C
            IF (IRODS.EQ.1.AND.IOP.EQ.6) THEN                           0317PF98
               CALL ENDRODS (IOPF)                                      0318PF98
            ENDIF                                                       0317PF98
C
         ENDIF                                                          1110DL89
      endif
C
C Reorder and store frequencies
C
      DO 90 I = 1, NFREQ
         IFRQ = IFREQ(I)
         WS(IS,IFRQ) = FREQ(I+ISHFT)
         XK3S(IS,IFRQ) = XK3(I)
         XK4S(IS,IFRQ) = XK4(I)
   90 CONTINUE
C
C sort freq in descending order                                         0211YC97
C
         DO I = 1,NFREQ                                                 0211YC97
           DO J = I+1, NFREQ                                            0211YC97
            IF (FREQ(I+ISHFT).GT.FREQ(J+ISHFT)) THEN                    0211YC97
                 TEMPF=FREQ(I+ISHFT)                                    0211YC97
                 FREQ(I+ISHFT) = FREQ(J+ISHFT)                          0211YC97
                 FREQ(J+ISHFT) = TEMPF                                  0211YC97
            ENDIF                                                       0211YC97
           ENDDO                                                        0211YC97
         ENDDO                                                          0211YC97
C
      IF (LGS(5).GT.0)  THEN                                            0925JC97
            CALL ANHARM (IOPF)                                          0925JC97
            DO I = 1, NFREQ                                             0925JC97
                 FMIRS(IS,I) = FMOMHR(I+ISHFT)                          0925JC97
            ENDDO                                                       0925JC97
      ENDIF                                                             0925JC97
      CALL ZEROPT (IOPF)                                                9/18YL92
      DO 100 I = 1, N
         DXP(I) = DX(I)
  100 CONTINUE
      IF (IOPF.NE.2 .OR. LOPT(4).NE.0) CALL NOROUT (IOPF,DXP)           0427WH93
C
      IF (IOP.GT.5.AND.IOP.NE.7.AND.IOP.NE.8) THEN                      0801PF97
C
C compute curvature vector B (if LOPT(6)=2, then B already
C    computed, unless RODS is on)
C
         IF (LOPT(6).GT.2) THEN
            NBS = LOPT(6)-1
            CALL RPHB (NBS,S,N,NFREQ,SB,DX1,DX,DX2,COF,BCURV)
            DO 110 I = 1, NFREQ
               IF (dabs(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
               BFS(IS,IFREQ(I)) = BCURV(I)
  110       CONTINUE
         ELSEIF (LOPT(6).EQ.2.AND.IRODS.EQ.1) THEN                      0317PF98
            SB(3) = S                                                   0317PF98
            IF (IS.GT.3) THEN                                           0317PF98
               IF (IS.EQ.4) THEN                                        0317PF98
                  NBS = 2                                               0317PF98
               ELSE                                                     0317PF98
                  NBS = 3                                               0317PF98
               ENDIF                                                    0317PF98
               ST = SS(IS-1)                                            0317PF98
               CALL RPHB(NBS,ST,N,NFREQ,SB,DX1,DX2,DX,SAVCOF,BCURV)     0317PF98
               DO 120 I = 1, NFREQ                                      0317PF98
                  IF (dabs(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0         0317PF98
                  BFS(IS-1,IFREQ(I)) = BCURV(I)                         0317PF98
 120           CONTINUE                                                 0317PF98
            ENDIF                                                       0317PF98
         ENDIF
      ENDIF
C
      RETURN
c
 1000 FORMAT (1X,A79,/,(2X,1P,5E15.5))
 1200 FORMAT (2X,1P,5E15.5)
C
      END SUBROUTINE irfgt0
C***********************************************************************
C  LOPT40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE lopt40
      use common_inc
      use perconparam
      use rate_const
      use fu_40
      use kintcm
C
C Translate the gen40 section keywords to LOPT
C
C     CALLED BY:  GENSEC40
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C LOPT(1) - print option
      if (prnt40 .eq. 'NOEXTRA') then
         LOPT(1) = 0
      elseif (prnt40 .eq. 'SUMMARY') then
         LOPT(1) = 1
      elseif (prnt40 .eq. 'INPUT') then
         LOPT(1) = 2
      elseif (prnt40 .eq. 'INTERMEDIATE') then
         LOPT(1) = 3
      elseif (prnt40 .eq. 'DEBUGGING') then
         LOPT(1) = 4
      else
         write(fu6,*) prnt40
         stop 'RD40GEN 17'
      endif
C
C LOPT(2) - maximum number of points used in Lagrange interpolation
      LOPT(2) = mxlpts
C
C LOPT(3) - option for coodinate and gradient input
      if (gmunit(1:2) .eq. 'MS') then
         if (gdunit(1:2) .eq. 'MS') then
            LOPT(3) = 1
         else
            LOPT(3) = -2
         endif
      else
         if (gdunit(1:2) .eq. 'MS') then
            LOPT(3) = -1
         else
            LOPT(3) = 0
         endif
      endif
C
C LOPT(4) - option for frequency input
      if (frqsrc .eq. 'READ') then
         LOPT(4) = 0
      elseif (frqsrc .eq. 'HESSIAN') then
         if (hsform .eq. 'PACKED') then
            if (hsunit(1:2) .eq. 'MS') then
               LOPT(4) = -2
            else
               LOPT(4) = -1
            endif
         elseif (hsform .eq. 'FULL') then
            if (hsunit(1:2) .eq. 'MS') then
               LOPT(4) = 2
            else
               LOPT(4) = 1
            endif
         else
            write(fu6,*) hsform
            stop 'RD40GEN 18'
         endif
      else
         write(fu6,*) frqsrc
         stop 'RD40GEN 19'
      endif
C
C LOPT(5) - option for selecting anharmonicity input
      if (anhm40 .eq. 'NONE') then
         LOPT(5) = 0
      elseif (anhm40 .eq. 'CUBIC') then
         LOPT(5) = 1
      elseif (anhm40 .eq. 'QUARTIC') then
         LOPT(5) = 2
      else
         write(fu6,*) anhm40
         stop 'RD40GEN 20'
      endif
C
C LOPT(6) - option for selecting curvature component B input
      if (curv40 .eq. 'SETZERO') then
         LOPT(6) = 0
      elseif (curv40 .eq. 'READ') then
         LOPT(6) = 1
      elseif (curv40 .eq. 'COMPUTE') then
         if (grdder .eq. 'NOEXTRA') then
            LOPT(6) = 2
         elseif (grdder .eq. 'ONEEXTRA') then
            LOPT(6) = 3
         elseif (grdder .eq. 'TWOEXTRA') then
            LOPT(6) = 4
         else
            write(fu6,*) grdder
            stop 'RD40GEN 21'
         endif
      else
         write(fu6,*) curv40
         stop 'RD40GEN 22'
      endif
C
      if (lopt(6).ne.2.and.irods.eq.1) then                             0317PF98
         stop 'RD40GEN23: Can not use extra gradients with RODS'        0317PF98
      endif                                                             0317PF98
C
      write(fu6,1100) (LOPT(i),I=1,40)
      write(fu6,1200)
C
      return
C
 1100 format(1x,'LOPT = ',20I3,/8X,20I3)
 1200 format(/1X,78(1H*))
      end SUBROUTINE lopt40

C***********************************************************************
C  PTSEC40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE ptsec40(DELV)
      use common_inc
      use perconparam
      use rate_const
      use fu_40
      use kintcm
C
C Read in and process the point40 sections of unit40
C
C     CALLED BY:  RPH40
C     CALLS:  RD40RP, SHFTSAD, IMOM40, CRVVEC, IRFGT0, RPHB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      VSHIFT = DELV
      IF (NPTS.GT.NSDIM-5) THEN
         WRITE (FU6,1400) NSDIM
         STOP 'ptsec40 1'
      ENDIF
      IF ( LGS(34) .NE. 0) THEN                                         11/20T87
         NFREQ = N3 -1                                                     ..
      ELSE IF (ICODE(5).EQ.3) THEN                                         ..
         NFREQ = N3-6                                                      ..
      ELSE                                                                 ..
         NFREQ = N3-7                                                      ..
      ENDIF                                                             11/20T87
C  The initial storage numbering scheme is fixed as follows:            0801PF97
C     IS = 1          Reactants                                         0801PF97
C     IS = 2          Reactant Well                                     0801PF97
C     IS = 3          First Path Point                                  0801PF97
C     IS = NSDIM - 2  Saddle Point                                      0801PF97
C     IS = NSDIM - 1  Product Well                                      0801PF97
C     IS = NSDIM      Products                                          0801PF97
C  Therefore, the first path point is placed in IS = 3 and the          0801PF97
C  last point is placed in NSLAST = NSS - 2                             0801PF97
C
      IS = 3                                                            0801PF97
      NSS = NPTS + 5                                                    0801PF97
      NSLAST = NSS - 2                                                  0801PF97
C
C8    NSS = NPTS+2
C8    IF (LGS(1).GT.0) NSS = NSS+1
C8    IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                              6/5S89
C8       NSS = NSS+1
C8       IS = 2
C8    ELSE
C8       IS = 1
C8    ENDIF
C8    IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN                              6/5S89
C8       NSS = NSS+1
C8       NSLAST = NSS-2
C8    ELSE
C8       NSLAST = NSS-1
C8    ENDIF
C
C set file unit40 to the beginning
      rewind fu40
C
C    Loop over grid points in s
C
   30 CONTINUE
      IF (LOPT(6).EQ.2) THEN
C
C       Shift gradient vectors used to compute BF
C
         DO 40 I = 1, N3
            DX1(I) = DX2(I)
            DX2(I) = DX(I)
   40    CONTINUE
         SB(1) = SB(2)
         SB(2) = SB(3)
      ENDIF
C
C    Read in RPH info
C
      call rd40rp (6,IS,N3,NFREQ)
      backspace fu40
C
C check if the saddle point values can be shifted
      if(lgs(1).gt.0.and.ISSP.lt.0.and.s.gt.0.0D0) 
     *                       call shftsad(6,IS,N3,NFREQ)
C Put s and V in grid
      SS(IS) = S
      VS(IS) = V-VSHIFT
C
      call imom40(6,IS,N3,NFREQ)
C
C put gradients and sb's in the proper place
      if(grdder.ne.'NOEXTRA') then
         SB(2) = S
         SB(3) = SB140
         if(grdder.eq.'TWOEXTRA') SB(1) = SB240
      else
         SB(3) = S
      endif
C
C
      do 80 I = 1, N3
         DX(I) = DX40(I)
         if(grdder.ne.'NOEXTRA') DX2(I) = DX140(I)
         if(grdder.eq.'TWOEXTRA') DX1(I) = DX240(I)
 80   continue
C
      if (lopt(1).gt.1) then
         write(fu6,*) 'SS(',IS,'),VS(',IS,') =',SS(IS),VS(IS)
         write(fu6,1001) 'DX',(DX(I),I=1,N3)
         write(fu6,1001) 'DX1',(DX1(I),I=1,N3)
         write(fu6,1001) 'DX2',(DX2(I),I=1,N3)
      endif
 1001 format(1x,A3,1P,10(/5E15.6))
C
      call crvvec(6,IS,N3,NFREQ)
C
C put bfs in its proper place if the curvature vector B was read in.
      if(curv40 .eq. 'READ') then
         do 85 I = 1,NFREQ
            BFS(IS,IFREQ(I)) = BFS40(I)
  85     continue
      endif
C
      call irfgt0(6,IS,N3,NFREQ)
C
C      IF (IS.LT.NSLAST) GO TO 30
      IF (IS.LT.NSLAST) THEN                                            0801PF97
         IS = IS + 1                                                    0801PF97
         GOTO 30                                                        0801PF97
      ENDIF                                                             0801PF97
C
C    End of loop over grid points
C
C    Put product values at NSS
C
      SS(NSS) = SS(NSDIM)
      FMOMS(NSS) = FMOMS(NSDIM)
      VS(NSS) = VS(NSDIM)
      DO 50 I = 1, NFREQ
         WS(NSS,I) = WS(NSDIM,I)
         IF (LOPT(5).GT.0) XK3S(NSS,I) = XK3S(NSDIM,I)
         IF (LOPT(5).GT.1) XK4S(NSS,I) = XK4S(NSDIM,I)
   50 CONTINUE
C
C    Put wellp values at NSS-1 if it exists, otherwise leave blank      0801PF97
C
      IF (irepr(8).eq.1) THEN                                           0801PF97
      SS(NSS-1) = SS(NSDIM-1)                                           0801PF97
      FMOMS(NSS-1) = FMOMS(NSDIM-1)                                     0801PF97
      VS(NSS-1) = VS(NSDIM-1)                                           0801PF97
      DO 55 I = 1, NFREQ                                                0801PF97
         WS(NSS-1,I) = WS(NSDIM-1,I)                                    0801PF97
         IF (LOPT(5).GT.0) XK3S(NSS-1,I) = XK3S(NSDIM-1,I)              0801PF97
         IF (LOPT(5).GT.1) XK4S(NSS-1,I) = XK4S(NSDIM-1,I)              0801PF97
   55 CONTINUE                                                          0801PF97
      ENDIF                                                             0801PF97
C
      IF (LOPT(6).EQ.2) THEN
C
C    Calcuate BF at next-to-last grid point
C
         ST = SS(NSLAST)
         NBS = 2
         CALL RPHB (NBS,ST,N3,NFREQ,SB,DX1,DX2,DX,COF,BCURV)
         DO 60 I = 1, NFREQ
            IF (dabs(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
            BFS(NSLAST,IFREQ(I)) = BCURV(I)
   60    CONTINUE
      ELSEIF (LOPT(6).GT.2) THEN
C
C    Interpolate BF at saddle point from points on either side
C
         DO 70 I = 1, NFREQ
            BFS(ISSP,I) = 0.5D0*(BFS(ISSP-1,I)+BFS(ISSP+1,I))
   70    CONTINUE
      ENDIF
c
      return
C
 1400 format (/1X,'Maximum number of input points is ',I5)
C
      end subroutine ptsec40
C
C***********************************************************************
C  RD40EXTR
C***********************************************************************
C Written by Patton Fast.  August, 1996
C
C
C    Parameters and common blocks modified 081696
C
      SUBROUTINE rd40extr
      use perconparam, only : fu6,fu40
      use rate_const
C
C  Read the extrap40 section of unit40
C
C
C  CALLED BY:  EXTRP40
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      integer PFIOP
      character*80 word(40)
      logical lsec, leof
      logical found(21)
C
      do i=1,21                                                         1126JC97
        found(i)=.false.                                                1126JC97
      enddo                                                             1126JC97
C
C8  This subroutine is not called in version 8.0 and is scheduled for 
C8  deletion in a future version.  The extra point for extrapolation is
C8  not used.
C8
C initialize
      ss(3) = 0.0D0                                                     0801PF97
      vs(3) = 0.0D0                                                     0801PF97
      ss(NSS - 2) = 0.0D0                                               0801PF97
      vs(NSS - 2) = 0.0D0                                               0801PF97

      rewind fu40
C
C
C  Find the *extrap40 section
C
 20   call readln (fu40,word,nword,lsec,leof)
C
      if (leof) then
         write(fu6,*) 'EXTRAP40'
         write(fu6,*) 'leof =',leof
         stop 'RD40EXTR 1'
      endif
      if (.not. lsec .or. word(1) .ne. 'EXTRAP40') goto 20
C
C Read in extrap40 keyword
C
 30   call readln (fu40,word,nword,lsec,leof)
      if (.not. lsec .and. .not. leof) then
C
C OPTCODER
       if (word(1) .eq. 'OPTCODER') then
        found(20) = .true.
        if (nword .eq. 2) then
          ifitvr = icint(word(2))
        else
          write(fu6,*) 'OPTCODER'
          stop 'RD40EXTR 6'
        endif
        goto 30
C
C OPTCODEP
       elseif (word(1) .eq. 'OPTCODEP') then
        found(21) = .true.
        if (nword .eq. 2) then
          ifitvp = icint(word(2))
        else
         write(fu6,*) 'OPTCODEP'
         stop 'RD40EXTR 7'
        endif
        goto 30
       endif
C
C if optcode is zero, then the extra point is not used and the values
C  are set to zero.
C
        if (ifitvr.ne.0 .or. ifitvp.ne.0) then
C
C SEXR
         if (word(1) .eq. 'SEXR') then
            found(16) = .true.
            if (nword .eq. 2) then
               ss(3) = cfloat(word(2))                                  0801PF97
            else
               write(fu6,*) 'SEXR'
               stop 'RD40EXTR 2' 
            endif
C
C SEXP
         elseif (word(1) .eq. 'SEXP') then
             found(17) = .true.
             if (nword .eq. 2) then
               ss(NSS - 2) = cfloat(word(2))                            0801PF97
            else
               write(fu6,*) 'SEXP'
               stop 'RD40EXTR 3'
            endif
C
C VEXR
         elseif (word(1) .eq. 'VEXR') then
             found(18) = .true.
             if (nword .eq. 2) then
               vs(3) = cfloat(word(2))                                  0801PF97
             else
               write(fu6,*) 'VEXR'
               stop 'RD40EXTR 4'
             endif
C
C VEXP
         elseif (word(1) .eq. 'VEXP') then
             found(19) = .true.
             if (nword .eq. 2) then
               vs(NSS - 2) = cfloat(word(2))                            0801PF97
             else
               write(fu6,*) 'VEXP'
               stop 'RD40EXTR 5'
             endif
C
C stop if keyword isn't recognized
         else
             write(fu6,*) word(1)
             stop 'RD40EXTR 6'
         endif
C
C read in another keyword
         goto 30
        endif
      endif
C
C Check if the required keywords are present
C
C PFIOP =15, used to only check SEXR, SEXP, VEXR, and VEXP when in EXTRP40
C
      PFIOP = 15
      call deflt40 (PFIOP,nfreq,found)
C
      return
C
      end SUBROUTINE rd40extr
C
C***********************************************************************
C  RD40GEN
C******************************p****************************************
C Written by Steven Clayton.  June, 1996
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE rd40gen
      use fu_40; use perconparam
C
C Read the gen40 section of file fu40.
C 
C
C     CALLED BY:  GENSEC40
C     CALLS:  READLN, TITLE, DATTIM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      character*80 word(40)
      logical lsec,leof
C
C Specify the default.
C
      prnt40 = 'NOEXTRA'
      mxlpts = 3
      gmunit = 'BOHR'
      gdunit = 'BOHR'
      frqsrc = 'READ'
      frequ  = 'WAVEN'                                                  012597PF
      hsform = 'FULL'
      hsunit = 'BOHR'
      anhm40 = 'NONE'
      curv40 = 'SETZERO'
      grdder = 'NOEXTRA'
      npts = 2                                                          073196PF
      extrpr = .false.
      extrpp = .false.
C 
C
C Find the *gen40 section.
 10   call readln(fu40,word,nword,lsec,leof)
c
      if (leof) then
         write(fu6,*) 'GEN40'
         write(fu6,*) 'leof = ',leof
         stop 'RD40GEN 1'
      endif
      if (.not. lsec .or. word(1) .ne. 'GEN40') goto 10
C
C read in next gen40 keyword
 20   call readln(fu40,word,nword,lsec,leof)
      if (.not. lsec .and. .not. leof) then
C
C TITLE
         if (word(1) .eq. 'TITLE') then
            call title(0,fu40,3)
            call dattim(fu6)
            call title(1,fu6,3)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'TITLE'
               stop 'RD40GEN 2'
            endif
C
C PRINT
         elseif (word(1) .eq. 'PRINT') then
            if (nword .eq. 2) then
               prnt40 = word(2)
            else
               write(fu6,*) 'PRINT'
               stop 'RD40GEN 3'
            endif
C
C MAXLPTS
         elseif (word(1) .eq. 'MAXLPTS') then
            if (nword .eq. 2) then
               mxlpts = icint(word(2))
            else
               write(fu6,*) 'MAXLPTS'
               stop 'RD40GEN 4'
            endif
C
C GEOMUNIT
         elseif (word(1) .eq. 'GEOMUNIT') then
            if (nword .eq. 2) then
               gmunit = word(2)
            else
               write(fu6,*) 'GEOMUNIT'
               stop 'RD40GEN 5'
            endif
C
C GRADUNIT
         elseif (word(1) .eq. 'GRADUNIT') then
            if (nword .eq. 2) then
               gdunit = word(2)
            else
               write(fu6,*) 'GRADUNIT'
               stop 'RD40GEN 6'
            endif
C
C FREQSOURCE
         elseif (word(1) .eq. 'FREQSOURCE') then
            if (nword .eq. 2) then
               frqsrc = word(2)
            else
               write(fu6,*) 'FREQSOURCE'
               stop 'RD40GEN 7'
            endif
C
C FREQUNIT
         elseif (word(1) .eq. 'FREQUNIT') then
            if (nword .eq. 2) then
               frequ = word(2)
            else
               write(fu6,*) 'FREQU'
               stop 'RD40GEN 17'
            endif
C
C HESSFORM
         elseif (word(1) .eq. 'HESSFORM') then
            if (nword .eq. 2) then
               hsform = word(2)
            else
               write(fu6,*) 'HESSFORM'
               stop 'RD40GEN 8'
            endif
C
C HESSUNIT
         elseif (word(1) .eq. 'HESSUNIT') then
            if (nword .eq. 2) then
               hsunit = word(2)
            else
               write(fu6,*) 'HESSUNIT'
               stop 'RD40GEN 9'
            endif
C
C ANHARM
         elseif (word(1) .eq. 'ANHARM') then
            if (nword .eq. 2) then
               anhm40 = word(2)
            else
               write(fu6,*) 'ANHARM'
               stop 'RD40GEN 10'
            endif
C
C CURVATURE
         elseif (word(1) .eq. 'CURVATURE') then
            if (nword .eq. 2) then
               curv40 = word(2)
            else
               write(fu6,*) 'CURVATURE'
               stop 'RD40GEN 11'
            endif
C
C GRADDER
         elseif (word(1) .eq. 'GRADDER') then
            if (nword .eq. 2) then
               grdder = word(2)
            else
               write(fu6,*) 'GRADDER'
               stop 'RD40GEN 12'
            endif
C
C NPTS
         elseif (word(1) .eq. 'NPTS') then
            if (nword .eq. 2) then
               npts = icint(word(2))
            else
               write(fu6,*) 'NPTS'
               stop 'RD40GEN 13'
            endif
C
C stop if keyword isn't recognized
         else
            write(fu6,*) word(1)
            stop 'RD40GEN 16'
         endif
C
C read in another keyword.
         goto 20
      endif

      write(fu6,1000) prnt40,mxlpts,gmunit,gdunit,hsunit,frqsrc,frequ,
     *              hsform,anhm40,curv40,grdder,npts,extrpr,extrpp
C
      return
C
 1000 format(1X,'print = ',A12,/1X,'maxlpts = ',I3,/1X,'geomunit = ',A6,
     */1X,'gradunit = ',A6,/1X,'hessunit = ',A6,/1X,'freqsource = ',A7,
     */1X,'frequnit = ',A6,/1X,'hessform = ',A6,/1X,'anharm = ',A7,
     */1X,'curvature = ',A7,/1X,'gradder = ',A8,/1X,'npts = ',I3,
     */1X,'extrapr = ',L2,/1X,'extrapp = ',L2)
C
      end SUBROUTINE rd40gen
C***********************************************************************
C  RD40RP
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
C    PARAMETERS AND COMMON BLOCKS MODIFIED 6/19/91
C
      SUBROUTINE rd40rp(IOP,IS,N,NFREQ)
      use fu_40
      use rate_const
      use perconparam
      use common_inc
C
C Read the reactant or product section of file fu40.
C
C NOTE:  N is now 3*(the N from RPH40) or NEND from RPH40
C
C     CALLED BY:  RPSEC40, SADSEC40, PTSEC40
C     CALLS:  READLN, RDHESS40, DEFLT40, CONV40
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      character*80 word(40)
      dimension jfreq(nvibm)
      logical lsec,leof
      logical found(21)
C
      do i=1,21                                                         1126JC97
        found(i)=.false.                                                1126JC97
      enddo                                                             1126JC97
c
C Find the reactant or product section according to IOP:
C   IOP=1  -  first reactant (*R140)
C   IOP=2  -  second reactant (*R240)
C   IOP=3  -  first product (*P140)
C   IOP=4  -  second product (*P240)
C   IOP=7  -  reactant well (*WR40)                                     0729PF97
C   IOP=8  -  product well (*WP40)                                      0729PF97
C
C Set file fu40 position to the beginning unless IOP = 6 (POINT section)
      if(IOP .ne. 6) rewind fu40                                        0729PF97
C
      write(fu6,*) 'IOP in RD40RP = ',iop                               0729PF97

C Find the unit40 section corresponding to IOP
 10   call readln(fu40,word,nword,lsec,leof)
      if (leof) then
         write(6,*) 'end of file (unit40), IOP = ',IOP
         stop 'RD40RP 1'
      elseif (.not. (lsec .and.
     *   ((IOP .eq. 1 .and. word(1) .eq. 'R140')
     *   .or. (IOP .eq. 2 .and. word(1) .eq. 'R240')
     *   .or. (IOP .eq. 3 .and. word(1) .eq. 'P140')
     *   .or. (IOP .eq. 4 .and. word(1) .eq. 'P240')
     *   .or. (IOP .eq. 5 .and. word(1) .eq. 'SADDLE40')
     *   .or. (IOP .eq. 7 .and. word(1) .eq. 'WR40')                    0729PF97
     *   .or. (IOP .eq. 8 .and. word(1) .eq. 'WP40')                    0729PF97
     *   .or. (IOP .eq. 6 .and. word(1) .eq. 'POINT40')))) then
         goto 10
      endif

C
C read in the next reactant or product keyword
c
      if(.not.allocated(dx140))then
         allocate(dx140(n3tm)); dx140=0.d00
         allocate(dx240(n3tm)); dx240=0.d00
      end if
 20   call readln(fu40,word,nword,lsec,leof)
      if (.not. lsec .and. .not. leof) then
C
C S
         if (word(1).eq.'SVALUE'.or.word(1).eq.'SMEP') then
            found(1) = .true.
            s = cfloat(word(2))
            write(fu6,*) 's = ',s
C
C V
         elseif(word(1).eq.'VVALUE'.or.word(1).eq.'VMEP') then
            found(2) = .true.
            v = cfloat(word(2))
            write(fu6,*) 'v = ',v
C
C HESSIAN
         elseif(word(1).eq.'HESSIAN' .and. frqsrc.eq.'HESSIAN') then
            found(3) = .true.
            call rdhess40(N)
            write(fu6,*) 'hessian read successfully, IOP = ',IOP
C
C FREQ
         elseif(word(1).eq.'FREQ' .and. frqsrc.eq.'READ') then          080596PF
            found(4) = .true.
            ISHFT = N-NFREQ
            do 85 I = 1, ISHFT
               FREQ(I) = 0.0D0
 85         continue
C Read in frequencies in cm-1 (default) or au.                          012597PF
            if (frequ .eq. 'WAVEN') then                                012597PF
               read(fu40,*) (freq(I+ISHFT), I=1,NFREQ)                  012597PF
               do 90 I = 1,NFREQ                                        012597PF
                  freq(I+ISHFT) = freq(I+ISHFT) * CMTOAU                012597PF
 90            continue                                                 012597PF
            elseif (frequ .eq. 'AU') then                               012597PF
               read(fu40,*) (freq(I+ISHFT),I=1,NFREQ)                   012597PF
            endif                                                       012597PF
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'FREQ'
               stop 'rd40rp 5'
            endif
C
C 3RDDERIV
         elseif(word(1).eq.'3RDDERIV' .and. anhm40.ne.'NONE') then
            found(5) = .true.
            read(fu40,*) (XK3(I),I=1,NFREQ)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) '3RDDERIV'
               stop 'rd40rp 7'
            endif
C
C 4THDERIV
         elseif(word(1).eq.'4THDERIV' .and. anhm40.eq.'QUARTIC') then
            found(6) = .true.
            read(fu40,*) (XK4(I),I=1,NFREQ)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) '4THDERIV'
               stop 'rd40rp 9'
            endif
C
C read in additional keywords for the SADDLE40 and POINT40 sections
C
C WSTAR
         elseif(IOP.eq.5.and.
     *            (word(1).eq.'WSTAR'.and.frqsrc.eq.'READ')) then
            found(8) = .true.
            if (frequ .eq. 'WAVEN') then
               WSTAR = cfloat(word(2))
               WSTAR = WSTAR * CMTOAU
            elseif (frequ .eq. 'AU') then
               WSTAR = cfloat(word(2))
            endif
C
C GEOM
         elseif(IOP.eq.6.and.word(1).eq.'GEOM') then                    0729PF97
            found(7) = .true.
            read(fu40,*) (X(I),I=1,N)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'GEOM'
               stop 'rd40rp 10'
            endif
C
C IFREQ
        elseif(word(1).eq.'IFREQ') then
          found(14) = .true.
          read(fu40,*) (IFREQ(I),I=1,NFREQ)
          call readln(fu40,word,nword,lsec,leof)
          if (word(1) .ne. 'END') then
            write(fu6,*) 'IFREQ'
            stop 'rd40rp 11'
          endif
C
C Reorder indexes when the reaction is unimolecular or there are wells  1126JC97
C
          if (                                                          1126JC97
     *        iop.eq.7.or.                                              1126JC97
     *        iop.eq.8.or.                                              1126JC97
     *       (iop.eq.1.and.(lgs(6).eq.3.or.lgs(6).eq.4)).or.            1126JC97
     *       (iop.eq.3.and.(lgs(6).eq.2.or.lgs(6).eq.4))                1126JC97
     *       ) then                                                     1126JC97
               savifr=ifreq(nfreq)                                      1126JC97
               do i=nfreq-1,1,-1                                        1126JC97
                   ifreq(i+1)=ifreq(i)                                  1126JC97
               enddo                                                    1126JC97
               ifreq(1)=savifr                                          1126JC97
           endif                                                        1126JC97
C                                                                       1126JC97
C Reorder indexes for 2 reactants or 2 products                         1126JC97
C                                                                       1126JC97
          if (                                                          1126JC97
     *       ( (iop.eq.1.or.iop.eq.2).and.                              1126JC97
     *       (lgs(6).eq.1.or.lgs(6).eq.2) ).                            1126JC97
     *       or.                                                        1126JC97
     *       ( (iop.eq.3.or.iop.eq.4).and.                              1126JC97
     *       (lgs(6).eq.1.or.lgs(6).eq.3) )                             1126JC97
     *       ) then                                                     1126JC97
               do i=1,nfreq                                             1126JC97
                   jfreq(i)=ifreq(nfreq-i+1)                            1126JC97
               enddo                                                    1126JC97
               do i=1,nfreq                                             1126JC97
                   ifreq(i)=nf(5)+1-jfreq(i)                            1126JC97
               enddo                                                    1126JC97
           endif                                                        1126JC97
C                                                                       1126JC97
C
C GRADS
         elseif(IOP.eq.6.and.word(1).eq.'GRADS') then                   0729PF97
            found(9) = .true.
            read(fu40,*) (DX40(I),I=1,N)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'GRADS'
               stop 'rd40rp 12'
            endif
C
C FORCES                                                                083096PF
         elseif(IOP.eq.6.and.word(1).eq.'FORCES') then                  0729PF97
            found(9) = .true.                                           083096PF
            read(fu40,*) (DX40(I),I=1,N)                                083096PF
            do 15 j = 1,N                                               083096PF
              DX40(j) = DX40(j) * (-1.0d0)                              083096PF
 15         continue                                                    083096PF
            call readln(fu40,word,nword,lsec,leof)                      083096PF
            if (word(1) .ne. 'END') then                                083096PF
               write(fu6,*) 'FORCES'                                    083096PF
               stop 'rd40rp 16'                                         083096PF
            endif                                                       083096PF
C
C SB1
         elseif(IOP.eq.6.and.
     *            (word(1).eq.'SB1'.and.grdder.ne.'NOEXTRA')) then      0729PF97
            found(10) = .true.
            SB140 = cfloat(word(2))
C
C GRADSB1
         elseif(IOP.eq.6.and.
     *            (word(1).eq.'GRADSB1'.and.grdder.ne.'NOEXTRA')) then  0729PF97
            found(11) = .true.
            read(fu40,*) (DX140(I),I=1,N)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'GRADSB1'
               stop 'rd40rp 13'
            endif
C
C FORCESB1                                                              083096PF
         elseif(IOP.eq.6.and.                                           0729PF97
     *            (word(1).eq.'FORCESB1'.and.grdder.ne.'NOEXTRA')) then 083096PF
            found(11) = .true.                                          083096PF
            read(fu40,*) (DX140(I),I=1,N)                               083096PF
            do 25 j = 1,N                                               083096PF
              DX140(j) = DX140(j) * (-1.0d0)                            083096PF
 25         continue                                                    083096PF
            call readln(fu40,word,nword,lsec,leof)                      083096PF
            if (word(1) .ne. 'END') then                                083096PF
               write(fu6,*) 'FORCESB1'                                  083096PF
               stop 'rd40rp 17'                                         083096PF
            endif                                                       083096PF
C
C SB2
         elseif(IOP.eq.6.and.
     *            (word(1).eq.'SB2'.and.grdder.eq.'TWOEXTRA')) then     0729PF97
            found(12) = .true.
            SB240 = cfloat(word(2))
C
C GRADSB2
         elseif(IOP.eq.6.and.
     *            (word(1).eq.'GRADSB2'.and.grdder.ne.'NOEXTRA')) then  0729PF97
            found(13) = .true.
            read(fu40,*) (DX240(I),I=1,N)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'GRADSB2'
               stop 'rd40rp 14'
            endif
C
C FORCESB2
         elseif(IOP.eq.6.and.
     *            (word(1).eq.'FORCESB2'.and.grdder.ne.'NOEXTRA')) then 0729PF97
            found(13) = .true.
            read(fu40,*) (DX240(I),I=1,N)
            do 35 j = 1,N
              DX240(j) = DX240(j) * (-1.0d0)
 35         continue
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'FORCESB2'
               stop 'rd40rp 18'
            endif
C
C BFS
         elseif(IOP.eq.6.and.word(1).eq.'BFS'.and.curv40.eq.'READ') then0729PF97
            found(15) = .true.
            read(fu40,*) (BFS40(I),I=1,NFREQ)
            call readln(fu40,word,nword,lsec,leof)
            if (word(1) .ne. 'END') then
               write(fu6,*) 'BFS'
               stop 'rd40rp 15'
            endif
C
C Stop if keyword isn't recognized.
         else
            write(fu6,*) word(1)
            stop 'rd40rp 11'
         endif
C
C read in another keyword
         goto 20
      endif
C
C Check if the required keywords were present.
C Also assign any appropriate default values for this reactant or product.
      call deflt40(IOP,NFREQ,found)
C convert units (if necessary)
      call conv40(IOP,IS,N,NFREQ,found)
C
      return
      end SUBROUTINE rd40rp
C***********************************************************************
C  RDHESS40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE rdhess40(N)
      use fu_40
      use perconparam
      use common_inc
C
C Read a hessian matrix from unit40.
C
C     CALLED BY:  RD40RP
C     CALLS:  READLN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      integer N,nword,j,k
      character*80 word(40)
      logical lsec,leof
C
      k = 0
      j = 1
 10   call readln(fu40,word,nword,lsec,leof)
      if(word(1) .eq. 'END') then
         if(j.ne.N .or. k.ne.N) then
            write(fu6,*) 'NOT ENOUGH NUMBERS ENTERED FOR A ',hsform,
     *                   N,' BY ',N,'HESSIAN'
            stop 'RDHESS40 1'
         endif
C
         return
C
      else
         do 20 i = 1, nword
            k = k + 1
C
C hessian input in packed form
            if(hsform .eq. 'PACKED') then
               if(k .gt. j) then
                  j = j + 1
                  k = 1
               endif
               if(j .gt. N) goto 100
               F(j,k) = cfloat(word(i))
               F(k,j) = F(j,k)
C
c hessian input in full form
            else
               if(k .gt. N) then
                  j = j + 1
                  k = 1
               endif
               if(j .gt. N) goto 100
               F(j,k) = cfloat(word(i))
            endif
 20      continue
      endif
      goto 10
C
 100  write(fu6,*) 'TOO MANY NUMBERS ENTERED FOR A ',hsform,N,' BY ',N
     *             ,'HESSIAN'
      stop 'RDHESS40 1'
C
      end SUBROUTINE rdhess40
C***********************************************************************
C  RPH40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE rph40 (IOP)
      use rate_const
C
C Read electronic structure data from file fu40.  Fu40 is a keyword
C version of the old unit fu30.  This subroutine is used instead of
C RPHSET, RPHRD1, and RPHRD2 when the electronic structure file input
C is in keyword (unit fu40) instead of formatted (unit fu30) form.
C
C    IOP = 0, first call to rph40
C    IOP = 1,2, read in reactant information
C    IOP = 3,4, read in product information
C    IOP = 5, read in saddle point information
C    IOP = 6, read in RPH information for points along the MEP                  
C    IOP = 7, reactant well                                             0727PF97
C    IOP = 8, product well                                              0727PF97
C
C     CALLED BY:  POLYAT, REACT, DOPNM, DORPH, DOSAFR
C     CALLS:  GENSEC40, RPSEC40, SADSEC40, PTSEC40, EXTRP40
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c DELV might be better off in a common block, but for now it must be passed
c to the *sec40 subroutines.
      SAVE  DELV
      vsave=0.d00
      call rph40_mem
C
C read and process the general (gen40) section
      if (iop .eq. 0) then
         call gensec40
C
C Read and process reactant or product or well species information
      elseif (iop.le.4.or.iop.eq.7.or.iop.eq.8) then                    0801PF97
         call rpsec40(IOP,DELV)
C
C Read and process saddle point information
      elseif (IOP.eq.5) then
         call sadsec40(DELV)
c
C Read and process points along the MEP
      else
         call ptsec40(DELV)
C
      NINT1 = LOPT(2)                                                   0923JC97
      NINTMX = NSS                                                      0923JC97
      NINTMX = NINTMX - 5                                               0923JC97
      NINT1 = MIN(NINT1,NINTMX)                                         0923JC97
      NINTH = (NINT1+1)/2                                               0923JC97
C
C extrapolate VaG and mueff 
C (This has not been implemented yet: for now EXTRP40 attempts to duplicate
C  the RPHSET extrapolation for VMEP and frequencies only.)
C
C Extrapolation for fu40 is not supprted in version 7.0
C
C8 Extrpolation routines are in the process of being deleted from polyrate.
C8 Version 8.0 will no longer use the extrapolation routines, and will be
C8 commented out in this version and then deleted in future versions
C8
C8       call extrp40
      endif
C
      return
      end SUBROUTINE rph40
C***********************************************************************
C  RPSEC40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE rpsec40(IOP,DELV)
      use common_inc
      use perconparam, only : nsdim
      use rate_const
C
C Read in and process a reactant or product section.
C
C     CALLED BY:  RPH40
C     CALLS:  RD40RP, IRFGT0
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save vsave
C
      if (IOP .le. 2) then
C For reactants store quantities in first location of grids.
         IS = 1
      elseif (iop.le.4) then                                            0801PF97
C For products store quantities in last location of grids.
         IS = NSDIM
      elseif (iop.eq.7) then                                            0801PF97
C For wellr store qunatities in second location                         0801PF97
         IS = 2                                                         0801PF97
      elseif (iop.eq.8) then                                            0801PF97
C For wellp store quantities in first to last location                  0801PF97
         IS = NSDIM - 1                                                 0801PF97
      endif
      N = NRATOM(IOP)
C ATOMIC reactant or product species
      if(ICODE(IOP).eq.1) then
         call rd40rp(IOP,IS,0,0)
C
C DIATOMIC reactant or product species
      elseif(ICODE(IOP).eq.2) then
         call rd40rp(IOP,IS,1,1)
         IFRQ = IFREQ(1)
         WS(IS,IFRQ) = FREQ(1)
C store anharmonicity data
         IF(LOPT(5).GT.0) XK3S(IS,IFRQ) = XK3(1)
         IF(LOPT(5).GT.1) XK4S(IS,IFRQ) = XK4(1)
C
C POLYATOMIC reactant or product species
      elseif(ICODE(IOP).ge.3) then
C NEND = number of coordinates;  NFREQ = number of frequencies
         NEND = 3*N
         if(ICODE(IOP) .eq. 3) then
            NFREQ = NEND-5
         else
            NFREQ = NEND-6
         endif
         call rd40rp(IOP,IS,NEND,NFREQ)
         call irfgt0(IOP,IS,NEND,NFREQ)
c NEW CODE 1011BE06
       elseif(ICODE(IOP) .lt. 0) then
C NEND = number of coordinates;  NFREQ = number of frequencies
         NEND = 3*N
         NFREQ = NEND
         call rd40rp(IOP,IS,NEND,NFREQ)
         call irfgt0(IOP,IS,NEND,NFREQ)
      endif
C
C Put s and V in grid
      SS(IS) = S
      VS(IS) = V-VSHIFT
C
C    Calculate potential shift and store asymptotic values of V
C
      IF (IOP.EQ.1) THEN
         DELV = VS(IS)
         VS(IS) = 0.0D0
         FMOMS(IS) = 1.0D0/FMOM(1)
         SS(IS) = S
      ELSEIF (IOP.EQ.2) THEN
         DELV = DELV+VS(IS)
         VS(IS) = 0.0D0
         FMOMS(IS) = 0.0D0
         SS(IS) = -100.0D0
      ELSEIF (IOP.EQ.3) THEN
         VS(IS) = VS(IS)-DELV
         VSAVE = VS(IS)
         FMOMS(IS) = 1.0D0/FMOM(3)
         SS(IS) = S
      ELSEIF (IOP.EQ.4) THEN                                            0801PF97
         VS(IS) = VS(IS)+VSAVE
         FMOMS(IS) = 0.0D0
         SS(IS) = 100.0D0
      ELSEIF (IOP.EQ.7) THEN                                            0801PF97
         VS(IS) = VS(IS) - DELV                                         0801PF97
         FMOMS(IS) = 1.0D0/FMOM(7)                                      0801PF97
         SS(IS) = S                                                     0801PF97
      ELSEIF (IOP.EQ.8) THEN                                            0801PF97
         VS(IS) = VS(IS) - DELV                                         0801PF97
         FMOMS(IS) = 1.0D0/FMOM(8)                                      0801PF97
         SS(IS) = S                                                     0801PF97
      ENDIF
C
      return
C
      end SUBROUTINE rpsec40
C***********************************************************************
C  SADSEC40
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE sadsec40(DELV)
      use common_inc
      use perconparam
      use rate_const
C
C Read in and process the saddle point section of unit40
C
C     CALLED BY:  RPH40
C     CALLS:  RD40RP, IMOM40, IRFGT0, NOROUT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Saddle point parameters stored in location NSDIM-2 for now
C
      VSHIFT = DELV
      IS = NSDIM-2                                                      0801PF97
      IF (LGS(34) .NE. 0) THEN                                          11/20T87
         NFREQ = N3 - 1                                                    ..
      ELSE IF (ICODE(5).EQ.3) THEN                                         ..
         NFREQ = N3-6                                                      ..
      ELSE                                                                 ..
         NFREQ = N3-7                                                      ..
      ENDIF                                                             11/20T87
C
C    Get RPH info from file unit40
      call rd40rp(5,IS,N3,NFREQ)
C
C geometry already read in fu5
      do 5 i = 1, N3
        X(I) = XR(I,5)
 5    continue
      DO 11 I=1,N3TM                                                    0429JC97
         XXS(I,IS)=XR(I,5)                                              0429JC97
11    CONTINUE                                                          0429JC97
C
C Since in fu5 the coordinates are unscaled, we could need to
C mass-scale them for consistency
C
      IF (LOPT(3).LE.-2.OR.LOPT(3).EQ.1) CALL RPHTRX (N,AMASS,X,1)      1126JC97
C
C Put s and V in grid
      SS(IS) = S
      VS(IS) = V-VSHIFT
C
      call imom40(5,IS,N3,NFREQ)
      call irfgt0(5,IS,N3,NFREQ)
C
      IF (LOPT(4).NE.0) THEN
         WSTAR = FREQ(1)
      ELSE
C
C    If F matrix not read in, read in imaginary freq. at saddle point
         FREQ(1) = - dabs(WSTAR)                                         0427WH93
         CALL NOROUT(2,DXP)                                             0427WH93
      ENDIF
      IF (LOPT(6).EQ.2) THEN
C
C    Save eigenvector for unbound motion along reaction path for
C       computing B at saddle point
C
         IF (LGS(27).EQ.-1) THEN
            SGN = 1.0D0
         ELSE
            SGN = -1.0D0
         ENDIF
         DO 10 I = 1, N3
            DXSAD(I) = SGN*COF(I,1)
            DO 10 J = 1, N3
               COFSAD(I,J) = COF(I,J)
   10    CONTINUE
      ENDIF
      DO 20 I = 1, NFREQ
         IFRSAD(I) = IFREQ(I)
   20 CONTINUE
C
      return
      end SUBROUTINE sadsec40
C***********************************************************************
C  SHFTSAD
C***********************************************************************
C Written by Steven Clayton.  June, 1996
C
      SUBROUTINE shftsad(IOP,IS,N,NFREQ)
      use common_inc
      use perconparam, only : nsdim
      use rate_const
C
C This subroutine shifts the saddle point when unit40 input is used.
C It performs the same calculations as part of RPHRD2, which is used for
C unit30 input.
C
C     CALLED BY:  PTSEC40
C     CALLS:  RPHB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C put saddle point values in location IS
C
      ISSP = IS
      SS(IS) = 0.0D0
      VS(IS) = VS(NSDIM-2)                                              0801PF97
      DO 10 I = 1, NFREQ
         WS(IS,I) = WS(NSDIM-2,I)                                       0801PF97
         XK3S(IS,I) = XK3S(NSDIM-2,I)                                   0801PF97
         XK4S(IS,I) = XK4S(NSDIM-2,I)                                   0801PF97
         FMIRS(IS,I) = FMIRS(NSDIM-2,I)                                 0925JC97
   10 CONTINUE
      FMOMS(IS) = FMOMS(NSDIM-2)                                        0801PF97
      DO 20 I = 1, N
         DX(I) = DXSAD(I)                                               0824YC98
   20 CONTINUE
      IF (LOPT(6).EQ.2) THEN
C
C Calcuate the curvature vector BF at the previous grid point
C
         SB(3) = 0.0D0
         IF ((LGS(6).LE.2.AND.IS.GT.3).OR.(LGS(6).GT.2.AND.IS.GT.2))    1110DL89
     *      THEN                                                        1110DL89
            IF ((LGS(6).LE.2.AND.IS.EQ.4).OR.(LGS(6).GT.2.AND.IS.EQ.3)) 1110DL89
     *         THEN                                                     1110DL89
               NBS = 2
            ELSE
               NBS = 3
            ENDIF
            ST = SS(IS-1)
            CALL RPHB (NBS,ST,N,NFREQ,SB,DX1,DX2,DX,COF,BCURV)
            DO 30 I = 1, NFREQ
               IF (dabs(BCURV(I)).LT.1.0D-8) BCURV(I) = 0.0D0
               BFS(IS-1,IFREQ(I)) = BCURV(I)
   30       CONTINUE
         ENDIF
C
C Reset mode numbers, gradient vectors, coefficient matrix, and sb
C
         DO 40 I = 1, NFREQ
            IFREQ(I) = IFRSAD(I)
   40    CONTINUE
         DO 50 I = 1, N
            DX1(I) = DX2(I)
            DX2(I) = DX(I)
            DO 50 J = 1, N
               COF(I,J) = COFSAD(I,J)
   50    CONTINUE
         SB(1) = SB(2)
         SB(2) = SB(3)
      ENDIF
      IS = IS+1
C
      return
      end  SUBROUTINE shftsad

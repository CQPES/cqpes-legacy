c
c     only works with full 3N parameters
c
c
      SUBROUTINE efmain(IOP,DLX2,convg,convgt,JNITER)
      use keyword_interface, only : iunit6,gufac6
      use efmain_mod; use perconparam
      use common_inc , only : x,v,ind,dx,iatom,ndim,nratom
      use cm, only : ddmax,rmax,rmin,omin
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
**********************************************************************
*
*   EF IS A QUASI NEWTON RAPHSON OPTIMIZATION ROUTINE BASED ON
*      Jacs Simons P-RFO algorithm as implemented by Jon Baker
*      (J.COMP.CHEM. 7, 385). Step scaling to keep length within
*      trust radius is taken from Culot et al. (Theo. Chim. Acta 82, 189)
*      The trust radius can be updated dynamically according to Fletcher.
*      Safeguards on valid step for TS searches based on actual/predicted
*      function change and change in TS mode are own modifications
*
*  ON ENTRY XPARAM = VALUES OF PARAMETERS TO BE OPTIMISED.
*           NVAR   = NUMBER OF PARAMETERS TO BE OPTIMISED.
*
*  ON EXIT  XPARAM = OPTIMISED PARAMETERS.
*           FUNCT  = HEAT OF FORMATION IN KCAL/MOL.
*
*  Current version implementing combined NR, P-RFO and QA algorithm
*      together with thrust radius update and step rejection was
*      made october 1992 by F.Jensen, Odense, DK
*
**********************************************************************
C

      LOGICAL LUPD,lts,lrjk,lorjk,rrscal,donr,gnmin 
      DATA  ICALCN,ZERO,ONE,TWO    /0,0.D0,1.D0,2.D0/
      DATA tmone /1.0d-1/, TMTWO/1.0D-2/, TMSIX/1.0D-06/
      data three/3.0d0/, four/4.0d0/, 
     1pt25/0.25d0/, pt5/0.50d0/, pt75/0.75d0/
      data demin1/2.0d-2/, gmin/5.0d0/
c
      DATA ANGSAU   /1.8897265D0/
C
C
      call allocate_efmain(n3tm)
      NVAR = NDIM(IOP)
C
      write (6,*) 'NVAR = ',NVAR
      write (6,*) 'ind = ',(IND(I),I=1,nvar)
C
      DO I = 1, NVAR
        XPARAM(I)=X(IND(I))/ANGSAU
      ENDDO
C
      WRITE (6,*) '======================= EF ========================='
C
C      Zero out some variables
C
      eigval=0.d00; grad=0.d00; fx=0.d00
      ODD=0.0d0
      ODMAX=0.0d0
      OOLDE=0.0d0
C
C     GET ALL INITIALIZATION DATA
      LDUMP=0
      NSTEP=0
      IHESS=0
      LAST=0
      NTIME=0
      ILOOP=1
C
C     FOR REACTANTS, PRODUCTS AND WELLS
C
      IF(IOP.NE.5) THEN
         MODE=0
         IGTHES=0
         IUPD  =2
         NEGREQ=0
         if (ddmax.eq.0) ddmax=0.5d0
      ELSE
         MODE=1
         IGTHES=1
         IUPD  =1
         NEGREQ=1
         if (rmin.eq.0) rmin=0.0d0
         if (rmax.eq.0) rmax=4.0d0
         if (omin.eq.0) omin=0.8d0
         if (ddmaxts.eq.0) then
             ddmax = 0.3d0
         else
             ddmax = ddmaxts
         endif
      ENDIF
c
c Initialize the rest of options
c
      rrscal=.false.
      donr=.true.
      gnmin=.false.
      IF (iprxnt.eq.1) IPRNT = 1
      MXSTEP=JNITER
      IRECLC=999999
      DMIN=1.0D-3
      DMAX=0.2D0
      if (IOP.EQ.5) then
          TOL2 = convgt*ANGSAU*CKCAL
      else
          TOL2 = convg*ANGSAU*CKCAL
      endif
C
C      CALL EFSTAR(IOP,XPARAM,FUNCT,IHESS,NTIME,ILOOP,IGTHES,MXSTEP,IRECLC,
C     *           IUPD,DMAX,DDMAX,dmin,TOL2,TOTIME,nvar,LUPD,ldump,
C     *           rrscal,donr,gnmin)
C
C lts: t- find ts/ f- find minima
      LTS = .false.
      IF (NEGREQ.EQ.1) LTS = .true.
      LORJK = .false.
C
C     In MOPAC, the initialization of LUPD is done in EFSTAR like this:
C      LUPD=(INDEX(KEYWRD,' IUPD=0') .EQ. 0)
C     so because here the IUPD is not zero, LUPD should be true.
C
      LUPD = .true.                                                     0423TA02
c
c     osmin is smallest step for which a ts-mode overlap less than omin
c     will be rejected. for updated hessians there is little hope of
c     better overlap by reducing the step below 0.005. for exact hessian
c     the overlap should go toward one as the step become smaller, but
c     don't allow very small steps 
      osmin=0.005d0
      if(ireclc.eq.1)osmin=0.001d0
C
C     CHECK THAT GEOMETRY IS NOT ALREADY OPTIMIZED
C
      CALL GHOOK(0,iproc)
c      write (6,*) 'dx ='
c      write (6,*) (dx(i),i=1,NVAR)
C
      FUNCT = V*CKCAL
      DO I = 1,NVAR
         GRAD(I) = DX(IND(I))*ANGSAU*CKCAL
      ENDDO
C
      RMX = GRAD(1)
      DO I = 2,NVAR
        IF (ABS(GRAD(I)).GE.RMX) RMX = ABS(GRAD(I))
      ENDDO
c      RMX=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))
      IF (RMX.LT.TOL2) THEN
        write (6,*) 'The original geometry is a stationary point'
        write (6,*) 'RMX = ', RMX, ' TOL2 = ',TOL2
        RETURN
      ENDIF
C
C
C     GET INITIAL HESSIAN. IF ILOOP IS .LE.0 THIS IS AN OPTIMIZATION RESTART
C     AND HESSIAN SHOULD ALREADY BE AVAILABLE
C
c      IF (ILOOP .GE. 0) CALL EFGHES(XPARAM,IGTHES,NVAR,ILOOP,
c     >                              TOTIME,HESS,GRAD)
c
      IF (ILOOP .GE. 0) call hhook(x,f,dlx2,nvar,0,0,0)
      DO I = 1,NVAR
        DO J = 1, I 
           F(I,J) = F(I,J)*ANGSAU*ANGSAU*CKCAL
        ENDDO
      ENDDO
c
      DO I = 1,NVAR
         DO J = 1,I
            HESS(I,J) = F(I,J)*AMASS(ind(I))*AMASS(ind(J))
            HESS(J,I) = HESS(I,J)
         ENDDO
      ENDDO
C
C     START OF MAIN LOOP
C     WE NOW HAVE GRADIENTS AND A HESSIAN. IF THIS IS THE FIRST
C     TIME THROUGH DON'T UPDATE THE HESSIAN. FOR LATER LOOPS ALSO
C     CHECK IF WE NEED TO RECALCULATE THE HESSIAN
c      IFLEPO=0
      itime=0
   10 CONTINUE
c     store various things for possibly omin rejection
      do 30 i=1,nvar
         oldfx(i)=fx(i)
         ooldf(i)=oldf(i)
         oldeig(i)=eigval(i)
         do 20 j=1,nvar
         oldhss(i,j)=hess(i,j)
           oldu(i,j)=u(i,j)
20       continue
30    continue
C
C update hessian
C
      IF (IHESS.GE.IRECLC) THEN
         ILOOP=1
         IHESS=0
         if (igthes.ne.3)IGTHES=1
c         CALL EFGHES(XPARAM,IGTHES,NVAR,ILOOP,
c     >                              TOTIME,HESS,GRAD)
      ENDIF
      call hhook(x,f,dlx2,nvar,0,0,0)
      DO I = 1,NVAR
        DO J = 1,I
           F(I,J) = F(I,J)*ANGSAU*ANGSAU*CKCAL
        ENDDO
      ENDDO
c
      DO I = 1,NVAR
        DO  J = 1,I
           HESS(I,J) = F(I,J)*AMASS(ind(I))*AMASS(ind(J))
           HESS(J,I) = HESS(I,J)
        ENDDO
      ENDDO

      WRITE(6,*) ' XPARAM in ang '
      WRITE(6,98) (XPARAM(I),I=1,NVAR)
      WRITE(6,*) ' GRADIENTS in kcal/ang'
      WRITE(6,98) (GRAD(I),I=1,NVAR)
98    format (5x,3F15.7)
C
C        PRINT RESULTS IN CYCLE
C
      GNFINA= GRAD(1)
      DO I = 2,NVAR
        IF (ABS(GRAD(I)).GE.GNFINA) GNFINA = ABS(GRAD(I))
      ENDDO
c      GNFINA=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))    
c      TIME2=SECOND()
c      if (itime.eq.0) time1=time0
c      TSTEP=TIME2-TIME1
c      IF (TSTEP.LT.ZERO)TSTEP=ZERO
c       TLEFT=TOTIME-TIME2                                                IR0494
c       TIME1=TIME2
c      itime=itime+1
c      WRITE(6,40) NSTEP+1, TSTEP, TLEFT, GNFINA, FUNCT
c   40 FORMAT(' CYCLE:',I4,' TIME:',F7.2,' TIME LEFT:',F9.1,
c     1       ' GRAD.:',F10.3,' HEAT:',G13.7)
c      IF (TLEFT .LT. TSTEP*TWO) GOTO 280
      WRITE(6,40) NSTEP+1, GNFINA/(ANGSAU*CKCAL), FUNCT/CKCAL
   40 FORMAT(' CYCLE:',I4,' GRAD COMP:',F15.7,' ENERGY:',F13.7,' a.u.')
C      IF (TLEFT .LT. TSTEP*TWO) GOTO 280
c
c count steps
c
      IHESS=IHESS+1
      NSTEP=NSTEP+1
C
C        TEST FOR CONVERGENCE
C
      RMX = GRAD(1)
      DO I = 2,NVAR
        IF (ABS(GRAD(I)).GE.RMX) RMX = ABS(GRAD(I))
      ENDDO
c      RMX=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                                IR0494
      IF (RMX.LT.TOL2)GOTO 250
      OLDE  = FUNCT
      oldgn = rmx
      DO 60 I=1,NVAR
         OLDF(I)=GRAD(I)
60    CONTINUE
C
C     if the optimization is in cartesian coordinates, we should remove
C     translation and rotation modes. Possible problem if run is in
C     internal but with exactly 3*natoms variable (i.e. dummy atoms
C     are also optimized).
c      write(6,'(10x,"Optiminzing using CARTESIAN coordinates..")')  
c      call prjfc(hess,xparam,nvar)
c
c      CALL RSPDRV (N3TM,NVAR,HESS,EIGVAL,1,U,SCR,SCR2,IERR) 
c      DO 90 I=1,NVAR
c         IF (ABS(EIGVAL(I)).LT.TMSIX) EIGVAL(I)=ZERO
c   90 CONTINUE
c
C
C     Diagonalize F
C
      IJ=0
      DO 80 I=1,NVAR
         DO 80 J=1,I
            IJ=IJ+1
            HESSC(IJ)=HESS(J,I)
   80 CONTINUE
      CALL RSP(HESSC,NVAR,NVAR,EIGVAL,UC)
      IJ=0
      DO 90 I=1,NVAR
         IF (ABS(EIGVAL(I)).LT.TMSIX) EIGVAL(I)=ZERO
         DO 90 J=1,NVAR
            IJ=IJ+1
            U(J,I)=-UC(IJ)
   90 CONTINUE
c
      CALL EFPHES(NVAR)
      IF (MXSTEP.EQ.0) nstep=0
      IF (MXSTEP.EQ.0) GOTO 280                                                 
c
      NEG=0                                                                     
      DO 100 I=1,NVAR                                                           
         IF (EIGVAL(I) .LT. ZERO)NEG=NEG+1                                     
  100 CONTINUE                                                                  
      IF (IPRNT.GE.1)WRITE(6,110)NEG,(eigval(i),i=1,neg)
  110 FORMAT(/,10X,'HESSIAN HAS',I3,' NEGATIVE EIGENVALUE(S)',6f7.1,/)
c     if an eigenvalue has been zero out it is probably one of the T,R modes
c     in a cartesian optimization. zero corresponding fx to allow formation
c     of step without these contributions. a more safe criteria for deciding
c     whether this actually is a cartesian optimization should be put in 
c     some day...
      DO 120 I=1,NVAR                                                           
         FX(I)=DDOT(NVAR,U(1,I),1,GRAD,1)                               IR0494
         if (abs(eigval(i)).eq.zero) fx(i)=zero
  120 CONTINUE 
c
c     form geometry step d
130   CALL EFORMD(EIGVAL,FX,NVAR,DMAX,OSMIN,LTS,LRJK,LORJK,RRSCAL,DONR)
c
c     if lorjk is true, then ts mode overlap is less than omin, reject prev step
c
      if (lorjk) then
         if (iprnt.ge.1)write(6,*)'      Now undoing previous step'
         dmax=odmax
         dd=odd
         olde=oolde
         do 131 i=1,nvar
            fx(i)=oldfx(i)
                   oldf(i)=ooldf(i)
            eigval(i)=oldeig(i)
            do 132 j=1,nvar
               hess(i,j)=oldhss(i,j)
               u(i,j)=oldu(i,j)
132         CONTINUE
131      CONTINUE
         DO 140 I=1,NVAR
            XPARAM(I)=XPARAM(I)-D(I)
140      CONTINUE
         dmax=min(dmax,dd)/two
         odmax=dmax
         odd=dd
         nstep=nstep-1
         if (dmax.lt.dmin) goto 230
      if (iprnt.ge.1)write(6,*)
     1'      Finish undoing, now going for new step'
         goto 130
      endif
C
C  FORM NEW TRIAL XPARAM AND GEO
C
      DO 150 I=1,NVAR
         XPARAM(I)=XPARAM(I)+D(I)
  150 CONTINUE
C
C     COMPARE PREDICTED E-CHANGE WITH ACTUAL 
C
      depre=zero
      imode=1
      if (mode.ne.0)imode=mode
      do 160 i=1,nvar
         xtmp=xlamd
         if (lts .and. i.eq.imode) xtmp=xlamd0
         if (abs(xtmp-eigval(i)).lt.tmtwo) then
         ss=zero
         else
         ss=skal*fx(i)/(xtmp-eigval(i))
         endif
         frodo=ss*fx(i) + pt5*ss*ss*eigval(i)
c        write(6,88)i,fx(i),ss,xtmp,eigval(i),frodo
         depre=depre+frodo
160   continue
c88   format(i3,f10.3,f10.6,f10.3,4f10.6)
C
C     GET GRADIENT FOR NEW GEOMETRY 
C
      do i = 1, nvar
        x(ind(i)) = xparam(i)*ANGSAU
      enddo
      call ghook(0,iproc)
      FUNCT = V*CKCAL
c      write (6,*) 'new energy = ',funct,'old = ',olde
c      write (6,*) 'new dx ='
c      write (6,*) (dx(i),i=1,nvar)
      DO I = 1, N3TM
         GRAD(I) = 0.0d0
      ENDDO
      DO I = 1,NVAR
         GRAD(I) = DX(IND(I))*ANGSAU*CKCAL
      ENDDO
c
      IF(GNMIN) GNTEST=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                   IR0494
      DEACT = FUNCT-OLDE
      RATIO = DEACT/DEPRE
      if(iprnt.ge.1)WRITE(6,170)DEACT,DEPRE,RATIO       
  170 FORMAT(5X,'ACTUAL, PREDICTED ENERGY CHANGE, RATIO',2F10.3,F10.5)
      lrjk=.false.
C     if this is a minimum search, don't allow the energy to raise
      if (.not.lts .and. funct.gt.olde) then
         if (iprnt.ge.1)write(6,180)funct,min(dmax,dd)/two
180      format(1x,'energy raises ',f10.4,' rejecting step, ',
     $             'reducing dmax to',f7.4)
         lrjk=.true.
      endif
      if (gnmin .and. gntest.gt.oldgn) then
         if (iprnt.ge.1)write(6,181)gntest,min(dmax,dd)/two
181      format(1x,'gradient norm raises ',f10.4,' rejecting step, ',
     $             'reducing dmax to',f7.4)
         lrjk=.true.
      endif
      if (lts .and. (ratio.lt.rmin .or. ratio.gt.rmax) .and.
     $(abs(depre).gt.demin1 .or. abs(deact).gt.demin1)) then
         if (iprnt.ge.1)write(6,190)min(dmax,dd)/two
190   format(1x,'unacceptable ratio,',
     $          ' rejecting step, reducing dmax to',f7.4)
         lrjk=.true.
      endif
      if (lrjk) then
         DO 200 I=1,NVAR
            XPARAM(I)=XPARAM(I)-D(I)
200      CONTINUE
         dmax=min(dmax,dd)/two
         if (dmax.lt.dmin) goto 230
         goto 130
      endif
      IF(IPRNT.GE.1)WRITE(6,210)DD
  210 FORMAT(5X,'STEPSIZE USED IS',F9.5)
      IF(IPRNT.GE.2) THEN
         WRITE(6,'('' CALCULATED STEP'')')
         WRITE(6,'(3X,8F9.5)')(D(I),I=1,NVAR)
      ENDIF
C
C     POSSIBLE USE DYNAMICAL TRUST RADIUS
      odmax=dmax
      odd=dd
      oolde=olde
      IF (LUPD .and. ( (RMX.gt.gmin) .or.
     $    (abs(depre).gt.demin1 .or. abs(deact).gt.demin1) ) ) THEN
c     Fletcher recommend dmax=dmax/4 and dmax=dmax*2
c     these are are a little more conservative since hessian is being updated
c     don't reduce trust radius due to ratio for min searches
      if (lts .and. ratio.le.tmone .or. ratio.ge.three)
     $    dmax=min(dmax,dd)/two
      if (lts .and. ratio.ge.pt75 .and. ratio.le.(four/three) 
     $                  .and. dd.gt.(dmax-tmsix)) 
     $   dmax=dmax*sqrt(two)
c     allow wider limits for increasing trust radius for min searches
      if (.not.lts .and. ratio.ge.pt5 
     $                  .and. dd.gt.(dmax-tmsix)) 
     $   dmax=dmax*sqrt(two)
c     be brave if  0.90 < ratio < 1.10 ...
      if (abs(ratio-one).lt.tmone) dmax=dmax*sqrt(two)
      dmax=max(dmax,dmin-tmsix)
      dmax=min(dmax,ddmax)
      ENDIF
c     allow stepsize up to 0.1 in the end-game where changes are less 
c     than demin1 and gradient is less than gmin
      IF (LUPD .and. RMX.lt.gmin .and.
     $   (abs(depre).lt.demin1 .and. abs(deact).lt.demin1) )
     $    dmax=max(dmax,tmone)
      if(iprnt.ge.1)WRITE(6,220)DMAX
 220  FORMAT(5X,'CURRENT TRUST RADIUS = ',F7.5)                  
230   if (dmax.lt.dmin) then
         write(6,240)dmin
240      format(/,5x,'TRUST RADIUS NOW LESS THAN ',F7.5,' OPTIMIZATION',
     $   ' TERMINATING',/,5X,
     1' GEOMETRY MAY NOT BE COMPLETELY OPTIMIZED')
         goto 270
      endif

C     CHECK STEPS AND ENOUGH TIME FOR ANOTHER PASS
      if (nstep.ge.mxstep) goto 280
C     IN USER UNFRIENDLY ENVIROMENT, SAVE RESULTS EVERY 1 CPU HRS
c      ITTEST=AINT((TIME2-TIME0)/TDUMP)
c      IF (ITTEST.GT.NTIME) THEN
c         LDUMP=1
c         NTIME=MAX(ITTEST,(NTIME+1))
c         IPOW(9)=2
c         TT0=SECOND()-TIME0
c         CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT,
c     1              IPOW)
c      ELSE
c         LDUMP=0
c      ENDIF
C     RETURN FOR ANOTHER CYCLE
      GOTO 10                                                                  
C
C     ****** OPTIMIZATION TERMINATION ******
C
  250 CONTINUE
      WRITE(6,260)RMX,TOL2
  260 FORMAT(/,5X,'RMS GRADIENT =',F9.5,'  IS LESS THAN CUTOFF =',
     1F9.5,//)
  270 IFLEPO=15
      LAST=1
C     SAVE HESSIAN ON FILE 9
c      IPOW(9)=2
c      TT0=SECOND()-TIME0
c      CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT,
c     1           IPOW)
C     CALL COMPFG TO CALCULATE ENERGY FOR FIXING MO-VECTOR BUG
c      CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .FALSE.)
c      RETURN
  280 CONTINUE
C     WE RAN OUT OF TIME or too many iterations. DUMP RESULTS
c      IF (TLEFT .LT. TSTEP*TWO) THEN
c         WRITE(6,290)
c  290    FORMAT(/,5X,'NOT ENOUGH TIME FOR ANOTHER CYCLE')
c      ENDIF
      IF (nstep.ge.mxstep) THEN
         WRITE(6,300)
  300    FORMAT(/,5X,'EXCESS NUMBER OF OPTIMIZATION CYCLES')
      ENDIF
      FUNCT = V*CKCAL
c      IPOW(9)=1
c      TT0=SECOND()-TIME0
c      CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT,
c     1           IPOW)
c
C
C Write out the final results
C
C     WRITE(FU6,1700)
      IF(IUNIT6.EQ.1) WRITE(FU6,1700)                                   0405JZ07
      IF(IUNIT6.EQ.0) WRITE(FU6,1702)                                   0405JZ07
      WRITE(FU6,1710)

      IF (IOP .NE. 5) THEN
         DO 350 J = 1,NRATOM(IOP)
            LSTR = 3 * IATOM(J) - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) IATOM(J),(X(L)/GUFAC6,L=LSTR,LEND)          0405JZ07
 350     CONTINUE
C
         WRITE(FU6,1712)                                                0405JZ07
         WRITE(FU6,1720)
         DO 360 J = 1,NRATOM(IOP)
            LSTR = 3 * IATOM(J) - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) IATOM(J),(DX(L),L=LSTR,LEND)
 360     CONTINUE
      ELSE
         DO 370 J = 1, NATOM
            LSTR = 3 * J - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) J,(X(L)/GUFAC6,L=LSTR,LEND)                 0405JZ07
370      CONTINUE
C
         WRITE(FU6,1712)                                                0405JZ07
         WRITE(FU6,1720)
         DO 380 J = 1, NATOM
            LSTR = 3 * J - 2
            LEND = LSTR + 2
            WRITE(FU6,1800) J,(DX(L),L=LSTR,LEND)
380      CONTINUE
      ENDIF
      WRITE(FU6,1600) V,V*CKCAL
C
 1600 FORMAT (/,'  V = ',1PE16.8,' hartrees  (',0P,F16.8,' kcal/mol)')  1216WH92
C1700 FORMAT (/1X,'Final geometry and derivatives in unscaled',         1201WH92
C    * ' cartesians (a.u.)')
 1700 FORMAT (/1X,'Final geometry in unscaled Cartesians (bohrs)')      0405JZ07
 1702 FORMAT (/1X,'Final geometry in unscaled Cartesians (angstroms)')  0405JZ07
 1710 FORMAT(/1X,4HAtom,11X,'X',15X,'Y',15X,'Z',/)
 1712 FORMAT (/1X,'Final derivatives in unscaled Cartesians (a.u.)')    0405JZ07
 1720 FORMAT(/1X,4HAtom,11X,'DX',14X,'DY',14X,'DZ',/)
 1800 FORMAT (1X,I3,4X,1P,3E16.6)
c

      RETURN
      END SUBROUTINE efmain

      SUBROUTINE EFORMD(EIGVAL,FX,NVAR,DMAX,OSMIN,TS,LRJK,LORJK,RRSCAL,
     >                  DONR)
c      use common_inc
      use perconparam
      use efmain_mod, only :OLDF,D,VMODE,
     $       U,DD,xlamd,xlamd0,skal,
     $       MODE,NSTEP,NEGREQ,IPRNT
      use cm, only : RMAX,RMIN,OMIN,DDMAX
c
C     This version forms geometry step by either pure NR, P-RFO or QA
C     algorithm, under the condition that the steplength is less than dmax
c
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION LAMDA,lamda0
      logical ts,rscal,frodo1,frodo2,lrjk,lorjk,rrscal,donr              IR0794
c
      DIMENSION EIGVAL(N3TM),FX(N3TM)
c
      DATA ZERO/0.0D0/, HALF/0.5D0/, TWO/2.0D+00/, TOLL/1.0D-8/         
      DATA STEP/5.0D-02/, TEN/1.0D+1/, ONE/1.0D+0/, BIG/1.0D+3/         
      DATA FOUR/4.0D+00/
      DATA TMTWO/1.0D-2/, TMSIX/1.0D-06/, SFIX/1.0D+01/
      DATA EPS1/1.0D-12/ 
C                                                                       
      MAXIT=999                                                         
      NUMIT=0                                                           
      SKAL=ONE
      rscal=rrscal
      it=0
      jt=1
      if (ts) then
      IF(MODE.NE.0) THEN 
      CALL EFOVLP(dmax,osmin,NEWMOD,NVAR,lorjk)
      if (lorjk) return
C                                                                               
C  ON RETURN FROM EFOVLP, NEWMOD IS THE TS MODE
C                                                                               
      IF(NEWMOD.NE.MODE .and. iprnt.ge.1) WRITE(6,1000) MODE,NEWMOD
1000  FORMAT(5X,'WARNING! MODE SWITCHING. WAS FOLLOWING MODE ',I3,             
     $       ' NOW FOLLOWING MODE ',I3)                                         
      MODE=NEWMOD                                                               
      IT=MODE                                                                   
      ELSE
      IT=1
      ENDIF
      eigit=eigval(it)
      IF (IPRNT.GE.1) THEN                                                      
         WRITE(6,900)IT,EIGIT
         WRITE(6,910)(U(I,IT),I=1,NVAR)                                         
900      FORMAT(/,5X,'TS MODE IS NUMBER',I3,' WITH EIGENVALUE',F9.1,/,          
     *5X,'AND COMPONENTS',/)                                                    
910      FORMAT(5X,8F9.4)                                                       
      ENDIF                                                                     
      endif
      if (it.eq.1) jt=2
      eone=eigval(jt)                                                    
      ssmin=max(abs(eone)*eps1,(ten*eps1))
      ssmax=max(big,abs(eone))
      ssmax=ssmax*big
      sstoll=toll
      d2max=dmax*dmax                                                   
c     write(6,*)'from formd, eone, ssmin, ssmax, sstoll',
c    $eone,ssmin,ssmax,sstoll
      
C  SOLVE ITERATIVELY FOR LAMDA                                          
C  INITIAL GUESS FOR LAMDA IS ZERO EXCEPT NOTE THAT                     
C  LAMDA SHOULD BE LESS THAN EIGVAL(1)                                  
C  START BY BRACKETING ROOT, THEN HUNT IT DOWN WITH BRUTE FORCE BISECT. 
C                                                                       
         frodo1=.false.
         frodo2=.false.
         LAMDA=ZERO                                                     
         lamda0=zero
      if (ts .and. eigit.lt.zero .and. eone.ge.zero .and. donr) then
         if (iprnt.ge.1) then
         write(6,*)' ts search, correct hessian, trying pure NR step'
         endif
         goto 776
      endif
      if (.not.ts .and. eone.ge.zero .and. donr) then
         if (iprnt.ge.1) then
         write(6,*)' min search, correct hessian, trying pure NR step'
         endif
         goto 776
      endif
5     if (ts) then
         lamda0=eigval(it)+sqrt(eigval(it)**2+four*fx(it)**2)
         lamda0=lamda0*half
         if (iprnt.ge.1)WRITE(6,1030) LAMDA0 
      endif
         SSTEP = STEP                                                          
         IF(EONE.LE.ZERO) LAMDA=EONE-SSTEP                              
         IF(EONE.GT.ZERO) SSTEP=EONE                                           
         BL = LAMDA - SSTEP                                             
         BU = LAMDA + SSTEP*HALF                                        
20       FL = ZERO                                                      
         FU = ZERO                                                      
         DO 30 I = 1,NVAR                                               
            if (i.eq.it) goto 30
            FL   = FL + (FX(I)*FX(I))/(BL-EIGVAL(I))                    
            FU   = FU + (FX(I)*FX(I))/(BU-EIGVAL(I))                    
30       CONTINUE                                                       
         FL = FL - BL                                                   
         FU = FU - BU                                                   
c        write(6,*)'bl,bu,fl,fu from brack'                             
c        write(6,668)bl,bu,fl,fu                                        
c668     format(6f20.15)
         IF (FL*FU .LT. ZERO) GOTO 40                                   
         BL = BL - (EONE-BL)                                            
         BU = BU + HALF*(EONE-BU)                                       
         IF (BL.LE.-SSMAX) then
            BL = -SSMAX
            frodo1=.true.
         endif
         IF (abs(eone-bu).le.ssmin) then
            BU = EONE-SSMIN           
            frodo2=.true.
         endif
         IF (frodo1.and.frodo2) THEN              
            WRITE(6,*)'NUMERICAL PROBLEMS IN BRACKETING LAMDA',
     $                    EONE,BL,BU,FL,FU
            write(6,*)' going for fixed step size....'                       
            goto 450                                                           
         ENDIF                                                          
         GOTO 20                                                        
                                                                        
40       CONTINUE                                                       
         NCNT = 0                                                       
         XLAMDA = ZERO                                                  
50       CONTINUE                                                       
         FL = ZERO                                                      
         FU = ZERO                                                      
         FM = ZERO                                                      
         LAMDA = HALF*(BL+BU)                                                  
         DO 60 I = 1,NVAR                                               
            if (i.eq.it) goto 60
            FL   = FL + (FX(I)*FX(I))/(BL-EIGVAL(I))                    
            FU   = FU + (FX(I)*FX(I))/(BU-EIGVAL(I))                    
            FM   = FM + (FX(I)*FX(I))/(LAMDA-EIGVAL(I))                 
60       CONTINUE                                                       
         FL = FL - BL                                                   
         FU = FU - BU                                                   
         FM = FM - LAMDA                                                
c        write(6,*)'bl,bu,lamda,fl,fu,fm from search'                   
c        write(6,668)bl,bu,lamda,fl,fu,fm                               
         IF (ABS(XLAMDA-LAMDA).LT.sstoll) GOTO 776
         NCNT = NCNT + 1                                                
         IF (NCNT.GT.1000) THEN                                         
            WRITE(6,*)'TOO MANY ITERATIONS IN LAMDA BISECT',
     $                    BL,BU,LAMDA,FL,FU
            STOP                                                        
         ENDIF                                                          
         XLAMDA = LAMDA                                                 
         IF (FM*FU.LT.ZERO) BL = LAMDA                                  
         IF (FM*FL.LT.ZERO) BU = LAMDA                                  
         GOTO 50                                                        
C                                                                       
776   if (iprnt.ge.1) WRITE(6,1031) LAMDA 
C                                                                       
C  CALCULATE THE STEP                                                   
C                                                                       
      DO 310 I=1,NVAR                                                   
      D(I)=ZERO                                                         
310   CONTINUE                                                          
      DO 330 I=1,NVAR                                                   
      if (lamda.eq.zero .and. abs(eigval(i)).lt.tmtwo) then
      temp=zero
      else
      TEMP=FX(I)/(LAMDA-EIGVAL(I))                                      
      endif
      if (i.eq.it) then
      TEMP=FX(IT)/(LAMDA0-EIGVAL(IT)) 
      endif
      if (iprnt.ge.5) write(6,*)'formd, delta step',i,temp
      DO 320 J=1,NVAR                                                   
      D(J)=D(J)+TEMP*U(J,I)                                             
320   CONTINUE                                                          
330   CONTINUE                                                          
      DD=SQRT(DDOT(NVAR,D,1,D,1))                                       IR0494
      if(lamda.eq.zero .and. lamda0.eq.zero .and.iprnt.ge.1)
     1 write(6,777)dd
777   format(1x,'pure NR-step has length',f10.5)
      if(lamda.ne.zero .and. lamda0.ne.-lamda .and.iprnt.ge.1) 
     1write(6,778)dd
778   format(1x,'P-RFO-step   has length',f10.5)
      if (dd.lt.(dmax+tmsix)) then
         xlamd=lamda
         xlamd0=lamda0
         return
      endif
      if (lamda.eq.zero .and. lamda0.eq.zero) goto 5
      if (rscal) then
         SKAL=DMAX/DD
         DO 160 I=1,NVAR
            D(I)=D(I)*SKAL
160      CONTINUE
         DD=SQRT(DDOT(NVAR,D,1,D,1))                                     IR0494
         IF(IPRNT.GE.1)WRITE(6,170)SKAL
170      FORMAT(5X,'CALCULATED STEP SIZE TOO LARGE, SCALED WITH',F9.5)
         xlamd=lamda
         xlamd0=lamda0
         return
      endif

450      LAMDA=ZERO                                                     
         frodo1=.false.
         frodo2=.false.
         SSTEP = STEP                                                          
         IF(EONE.LE.ZERO) LAMDA=EONE-SSTEP                              
         if (ts .and. -eigit.lt.eone) lamda=-eigit-sstep
         IF(EONE.GT.ZERO) SSTEP=EONE                                           
         BL = LAMDA - SSTEP                                             
         BU = LAMDA + SSTEP*HALF                                        
520      FL = ZERO                                                      
         FU = ZERO                                                      
         DO 530 I = 1,NVAR                                              
            if (i.eq.it) goto 530
            FL   = FL + (FX(I)/(BL-EIGVAL(I)))**2                       
            FU   = FU + (FX(I)/(BU-EIGVAL(I)))**2                       
530      CONTINUE                                                       
         if (ts) then
            FL   = FL + (FX(IT)/(BL+EIGVAL(IT)))**2                       
            FU   = FU + (FX(IT)/(BU+EIGVAL(IT)))**2                       
         endif
         FL = FL - d2max                                                
         FU = FU - d2max                                                
c        write(6,*)'bl,bu,fl,fu from brack2'                            
c        write(6,668)bl,bu,fl,fu                                        
         IF (FL*FU .LT. ZERO) GOTO 540                                  
         BL = BL - (EONE-BL)                                            
         BU = BU + HALF*(EONE-BU)                                       
         IF (BL.LE.-SSMAX) then
            BL = -SSMAX
            frodo1=.true.
         endif
         IF (abs(eone-bu).le.ssmin) then
            BU = EONE-SSMIN           
            frodo2=.true.
         endif
         IF (frodo1.and.frodo2) THEN              
            WRITE(6,*)'NUMERICAL PROBLEMS IN BRACKETING LAMDA',
     $                    EONE,BL,BU,FL,FU
            write(6,*)' going for fixed level shifted NR step...'
c           both lamda searches failed, go for fixed level shifted nr    
c           this is unlikely to produce anything useful, but maybe we're lucky
            lamda=eone-sfix                                                    
            lamda0=eigit+sfix
            rscal=.true.                                                
            goto 776                                                    
         ENDIF                                                          
         GOTO 520                                                       
                                                                        
540      CONTINUE                                                       
         NCNT = 0                                                       
         XLAMDA = ZERO                                                  
550      CONTINUE                                                       
         FL = ZERO                                                      
         FU = ZERO                                                      
         FM = ZERO                                                      
         LAMDA = HALF*(BL+BU)                                                  
         DO 560 I = 1,NVAR                                              
            if (i.eq.it) goto 560
            FL   = FL + (FX(I)/(BL-EIGVAL(I)))**2                       
            FU   = FU + (FX(I)/(BU-EIGVAL(I)))**2                       
            FM   = FM + (FX(I)/(LAMDA-EIGVAL(I)))**2                    
560      CONTINUE                                                       
         if (ts) then
            FL   = FL + (FX(IT)/(BL+EIGVAL(IT)))**2                       
            FU   = FU + (FX(IT)/(BU+EIGVAL(IT)))**2                       
            FM   = FM + (FX(IT)/(LAMDA+EIGVAL(IT)))**2                    
         endif
         FL = FL - d2max                                                
         FU = FU - d2max                                                
         FM = FM - d2max                                                
c        write(6,*)'bl,bu,lamda,fl,fu,fm from search2'                  
c        write(6,668)bl,bu,lamda,fl,fu,fm                               
         IF (ABS(XLAMDA-LAMDA).LT.sstoll) GOTO 570                        
         NCNT = NCNT + 1                                                
         IF (NCNT.GT.1000) THEN                                         
            WRITE(6,*)'TOO MANY ITERATIONS IN LAMDA BISECT',
     $                    BL,BU,LAMDA,FL,FU
            STOP                                                        
         ENDIF                                                          
         XLAMDA = LAMDA                                                 
         IF (FM*FU.LT.ZERO) BL = LAMDA                                  
         IF (FM*FL.LT.ZERO) BU = LAMDA                                  
         GOTO 550                                                       
C                                                                       
570      CONTINUE                                                       
         lamda0=-lamda
         rscal=.true.                                                   
         goto 776                                                       
C                                                                       
1030  FORMAT(1X,'lamda that maximizes along ts modes =   ',F15.5)       
1031  FORMAT(1X,'lamda that minimizes along all modes =  ',F15.5)       
      END SUBROUTINE EFORMD
                                                              
      SUBROUTINE efovlp(dmax,osmin,NEWMOD,NVAR,lorjk)
      use energetics_mod
      use efmain_mod
      use perconparam             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
c      COMMON /EFCM/ RMAX,RMIN,OMIN,DDMAX,DDMAXTS
c
c      COMMON/OPEFCM/OLDF(N3TM),D(N3TM),VMODE(N3TM),
c     $U(N3TM,N3TM),DD,xlamd,xlamd0,skal,
c     $MODE,NSTEP,NEGREQ,IPRNT
c
      dimension xo(N3TM)
      logical lorjk
      DATA ICALCN /0/                                                   IR1294
c
      numcal = 1
C
C  ON THE FIRST STEP SIMPLY DETERMINE WHICH MODE TO FOLLOW
C
c     IF(NSTEP.EQ.1) THEN
      IF(ICALCN.NE.NUMCAL) THEN                                         IR1294
         ICALCN=NUMCAL                                                  IR1294
         IF(MODE.GT.NVAR)THEN
            WRITE(6,*)'ERROR!! MODE IS LARGER THAN NVAR',MODE
            STOP
         ENDIF
         IT=MODE
         if (iprnt.ge.1) WRITE(6,40) MODE
   40 FORMAT(1X,'HESSIAN MODE FOLLOWING SWITCHED ON'/
     1    ,1X,'FOLLOWING MODE ',I3)
C
      ELSE
C
C  ON SUBSEQUENT STEPS DETERMINE WHICH HESSIAN EIGENVECTOR HAS
C  THE GREATEST OVERLAP WITH THE MODE WE ARE FOLLOWING
C
         IT=1
         lorjk=.false.
         TOVLP=DDOT(NVAR,U(1,1),1,VMODE,1)                              IR0494
         TOVLP=ABS(TOVLP)
c        xo(1)=tovlp
         DO 10 I=2,NVAR
            OVLP=DDOT(NVAR,U(1,I),1,VMODE,1)                            IR0494
            OVLP=ABS(OVLP)
c           xo(i)=ovlp
            IF(OVLP.GT.TOVLP) THEN
               TOVLP=OVLP
               IT=I
            ENDIF
   10    CONTINUE
C
         if (iprnt.ge.5) then
         do 11 j=1,5
         xxx=0.d0
         do 12 i=1,nvar
         if (xo(i).gt.xxx)ix=i
         if (xo(i).gt.xxx)xxx=xo(i)
12       continue
         xo(ix)=0.d0
         write(6,*)'overlaps',ix,xxx
11       continue
         endif

         if(iprnt.ge.1)WRITE(6,30) IT,TOVLP
         if (tovlp.lt.omin) then
            if (dmax.gt.osmin) then
            lorjk=.true.
            if (iprnt.ge.1)write(6,31)omin
            return
            else
            if (iprnt.ge.1)write(6,32)omin,dmax,osmin
            endif
         endif
      ENDIF
   30 FORMAT(5X,'OVERLAP OF CURRENT MODE',I3,' WITH PREVIOUS MODE IS ',
     $       F6.3)
   31 FORMAT(5X,'OVERLAP LESS THAN OMIN',
     1F6.3,' REJECTING PREVIOUS STEP')
   32 FORMAT(5X,'OVERLAP LESS THAN OMIN',F6.3,' BUT TRUST RADIUS',F6.3,
     $          ' IS LESS THAN',F6.3,/,5X,' ACCEPTING STEP')
C
C  SAVE THE EIGENVECTOR IN VMODE
C
      DO 20 I=1,NVAR
         VMODE(I)=U(I,IT)
   20 CONTINUE
C
      NEWMOD=IT
      RETURN
C
      END SUBROUTINE EFOVLP

      SUBROUTINE efphes(NVAR)
      use perconparam; use efmain_mod
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
c      DIMENSION EIGVAL(N3TM),HESS(N3TM,N3TM)
c
         IF (IPRNT.GE.4) THEN
         WRITE(6,*)' '
         WRITE(6,*)'              HESSIAN MATRIX'
         LOW=1
         NUP=8
540      NUP=MIN(NUP,NVAR)
         WRITE(6,1000) (I,I=LOW,NUP)
         DO 550 I=1,NVAR
         WRITE(6,1010) I,(HESS(I,J),J=LOW,NUP)
550      CONTINUE
         NUP=NUP+8
         LOW=LOW+8
         IF(LOW.LE.NVAR) GOTO 540
         ENDIF
         WRITE(6,*)' '
         WRITE(6,*)'              HESSIAN EIGENVALUES AND -VECTORS'
         LOW=1
         NUP=8
560      NUP=MIN(NUP,NVAR)
         WRITE(6,1000) (I,I=LOW,NUP)
         WRITE(6,1020) (EIGVAL(I),I=LOW,NUP)
         DO 570 I=1,NVAR
         WRITE(6,1030) I,(U(I,J),J=LOW,NUP)
570      CONTINUE
         NUP=NUP+8
         LOW=LOW+8
         IF(LOW.LE.NVAR) GOTO 560
1000     FORMAT(/,3X,8I9)
1010     FORMAT(1X,I3,8F9.1)
1020     FORMAT(/,4X,8F9.1,/)
1030     FORMAT(1X,I3,8F9.4)
      RETURN
      END SUBROUTINE EFPHES

      SUBROUTINE PRJFC(F,xparam,nvar)
      use perconparam
      use common_inc, only : AMASS,DLX,REDM,S,DEMIN,
     * FMOM,EPRD,REDMF,REDMR,SIGMAF,SIGMAR,ELEC,VAD,VAR,VAP,
     * WSTAR,DQQP,ELAST,D3LX,EWR,EWP
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c 
C                                                                       
C  CALCULATES PROJECTED FORCE CONSTANT MATRIX (F).                      
C  THIS ROUTINE CAME ORIGINALLY FROM POLYRATE. IT IS USED BY PERMISSION
C  OF D. TRUHLAR. THE CURRENT VERSION IS LIFTED FROM GAMESS AND 
C  ADAPTED BY F.JENSEN, ODENSE, DK             
C  IF WE ARE AT A STATIONARY POINT (STPT=.T.), I.E. GNORM .LT. 10,      
C  THEN THE ROTATIONAL AND TRANSLATIONAL MODES ARE PROJECTED OUT        
C  AND THEIR FREQUENCIES BECOME IDENTICAL ZERO. IF NOT AT A STATIONARY  
C  POINT THEN THE MASS-WEIGHTED GRADIENT IS ALSO PROJECTED OUT AND      
C  THE CORRESPONDING FREQUENCY BECOME ZERO.                             
C ************************************************                      
C   X : MASS-WEIGHTED COORDINATE                                        
C   DX: NORMALIZED MASS-WEIGHTED GRADIENT VECTOR                        
C   F : MASS-WEIGHTED FORCE CONSTANT MATRIX                             
C   RM: INVERSION OF SQUARE ROOT OF MASS                                
C   P, COF: BUFFER                                                          
c
      DIMENSION X(N3TM),RM(N3TM),F(N3TM,N3TM),                  
     *          P(N3TM,N3TM),COF(N3TM,N3TM)                     
      DIMENSION TENS(3,3,3),ROT(3,3),SCR(3,3),ISCR(6),CMASS(3),det(2)
      dimension coord(3,natoms),dx(N3TM),xparam(N3TM),atmass(natoms)
      PARAMETER (ZERO=0.0d+00, ONE=1.0d+00,               
     *           CUT5=1.0d-05, CUT8=1.0d-08)                            
C                                                                       
C TOTALLY ASYMMETRIC CARTESIAN TENSOR.                                  
      DATA TENS/ 0.0d+00,  0.0d+00,  0.0d+00,                           
     X           0.0d+00,  0.0d+00, -1.0d+00,                           
     X           0.0d+00,  1.0d+00,  0.0d+00,                           
     Y           0.0d+00,  0.0d+00,  1.0d+00,                           
     Y           0.0d+00,  0.0d+00,  0.0d+00,                           
     Y          -1.0d+00,  0.0d+00,  0.0d+00,                           
     Z           0.0d+00, -1.0d+00,  0.0d+00,                           
     Z           1.0d+00,  0.0d+00,  0.0d+00,                           
     Z           0.0d+00,  0.0d+00,  0.0d+00  /                         
C                                                                       
      natm=nvar/3
      nc1=nvar
      l = 1
      do i = 1,nvar,3
        atmass(l) = amass(i)
        l = l + 1
      enddo
      ij=1
      do 2 i=1,natm                                                    
         coord(1,i)=xparam(ij)
         coord(2,i)=xparam(ij+1)
         coord(3,i)=xparam(ij+2)
         ij=ij+3
2     continue                                                          
C     CALCULATE 1/SQRT(MASS)                                            
      L=0                                                               
      DO 3 I=1,NATM                                                     
         TMP=ONE/SQRT(ATMASS(I))                                        
         DO 3 J=1,3                                                     
            L=L+1                                                       
3           RM(L)=TMP                                                   
C     PREPARE GRADIENT                                                  
         DO 4 I=1,NC1                                                   
4        DX(I)=ZERO                                                     
C     FIND CMS AND CALCULATED MASS WEIGHTED COORDINATES                 
      totm=zero                                                         
      cmass(1)=zero                                                     
      cmass(2)=zero                                                     
      cmass(3)=zero                                                     
      DO 6 I=1,NATM                                                     
         TOTM=TOTM+ATMASS(I)                                            
         DO 6 J=1,3                                                     
            CMASS(J)=CMASS(J)+ATMASS(I)*COORD(J,I)                      
6     CONTINUE                   
      DO 7 J=1,3                                                        
7     CMASS(J)=CMASS(J)/TOTM                                            
      L=0                                                               
      DO 8 I=1,NATM                                                     
         DO 8 J=1,3                                                     
         TMP=SQRT(ATMASS(I))                                            
             L=L+1                                                      
             X(L)=TMP*(COORD(J,I)-CMASS(J))                             
8     CONTINUE                                                          
c      WRITE(6,9020)                                                     
c      CALL prsq(f,nc1,nc1,n3tm,1,6)                                          
c9020  FORMAT(/1X,'ENTER THE SUBROUTINE <PRJFC>'//                       
c     *        1X,'UNPROJECTED FORCE CONSTANT MATRIX (HARTREE/BOHR**2)') 
c      WRITE(6,*)' MASS-WEIGHTED COORDINATES AND CORRESPONDING GRADIENT' 
c      DO 9 I=1,NC1                                                      
c9       WRITE(6,*)X(I),DX(I)                                           
c                                                                       
C 2. COMPUTE INERTIA TENSOR.                                            
      DO 10 I=1,3                                                       
       DO 10 J=1,3                                                      
   10   ROT(I,J)=ZERO                                                   
      DO 20 I=1,NATM                                                    
       L=3*(I-1)+1                                                      
       ROT(1,1)=ROT(1,1)+X(L+1)**2+X(L+2)**2                            
       ROT(1,2)=ROT(1,2)-X(L)*X(L+1)                                    
       ROT(1,3)=ROT(1,3)-X(L)*X(L+2)                                    
       ROT(2,2)=ROT(2,2)+X(L)**2+X(L+2)**2                              
       ROT(2,3)=ROT(2,3)-X(L+1)*X(L+2)                                  
   20  ROT(3,3)=ROT(3,3)+X(L)**2+X(L+1)**2                              
      ROT(2,1)=ROT(1,2)                                                 
      ROT(3,1)=ROT(1,3)                                                 
      ROT(3,2)=ROT(2,3)                                                 
C                                                                       
CHECK THE INERTIA TENSOR.                                               
      CHK=ROT(1,1)*ROT(2,2)*ROT(3,3)                                    
      IF(ABS(CHK).GT.CUT8) GO TO 21                                     
c     WRITE(6,23)                                                       
c  23 FORMAT(/1X,'MATRIX OF INERTIA MOMENT')                            
c     CALL PRSQ(ROT,3,3,3,3)                                              
      IF(ABS(ROT(1,1)).GT.CUT8) GO TO 11                                
C X=0                                                                   
      IF(ABS(ROT(2,2)).GT.CUT8) GO TO 12                                
C X,Y=0                                                                 
      IF(ABS(ROT(3,3)).GT.CUT8) GO TO 13                                
      WRITE(6,14) ROT(1,1),ROT(2,2),ROT(3,3)                            
   14 FORMAT(1X,'EVERY DIAGONAL ELEMENTS ARE ZERO ?',3F20.10)           
      RETURN                                                            
C                                                                       
C* 1. X,Y=0 BUT Z.NE.0                                                  
   13 ROT(3,3)=ONE/ROT(3,3)                                             
      GO TO 22                                                          
C Y.NE.0                                                                
   12 IF(ABS(ROT(3,3)).GT.CUT8) GO TO 15                                
C* 2. X,Z=0 BUT Y.NE.0                                                  
      ROT(2,2)=ONE/ROT(2,2)                                             
      GO TO 22                                                          
C X.NE.0                                                                
   11 IF(ABS(ROT(2,2)).GT.CUT8) GO TO 16                                
      IF(ABS(ROT(3,3)).GT.CUT8) GO TO 17                                
C* 3. Y,Z=0 BUT X.NE.0                                                  
      ROT(1,1)=ONE/ROT(1,1)                                             
      GO TO 22                                                          
C* 4. X,Y.NE.0 BUT Z=0                                                  
   16 DET(1)=ROT(1,1)*ROT(2,2)-ROT(1,2)*ROT(2,1)
      TRP=ROT(1,1)                                                      
      ROT(1,1)=ROT(2,2)/DET(1)
      ROT(2,2)=TRP/DET(1)                                          
      ROT(1,2)=-ROT(1,2)/DET(1)
      ROT(2,1)=-ROT(2,1)/DET(1)                                         
      GO TO 22                                                          
C* 5. X,Z.NE.0 BUT Y=0                                                  
   17 DET(1)=ROT(1,1)*ROT(3,3)-ROT(1,3)*ROT(3,1)
      TRP=ROT(1,1)                        
      ROT(1,1)=ROT(3,3)/DET(1)
      ROT(3,3)=TRP/DET(1)                                          
      ROT(1,3)=-ROT(1,3)/DET(1)
      ROT(3,1)=-ROT(3,1)/DET(1)                                         
      GO TO 22                                                          
C* 6. Y,Z.NE.0 BUT X=0                                                  
   15 DET(1)=ROT(3,3)*ROT(2,2)-ROT(3,2)*ROT(2,3)
      TRP=ROT(3,3)                        
      ROT(3,3)=ROT(2,2)/DET(1)
      ROT(2,2)=TRP/DET(1)                                          
      ROT(3,2)=-ROT(3,2)/DET(1)
      ROT(2,3)=-ROT(2,3)/DET(1)                                         
      GO TO 22                                                          
   21 CONTINUE                                                          
C                                                                       
C.DEBUG.                                                                
c      CALL PRSQ(TENS(1,1,1),3,3,3,3)                                     
c      CALL PRSQ(TENS(1,1,2),3,3,3,3)                                     
c      CALL PRSQ(TENS(1,1,3),3,3,3,3)                                     
c      CALL PRSQ(ROT,3,3,3,3)                                             
C                                                                       
C 4. COMPUTE INVERSION MATRIX OF ROT.                                   
C     CALL MXLNEQ(ROT,3,3,DET,JRNK,EPS,SCR,+0)                          
C     IF(JRNK.LT.3) STOP 1                                              
      INFO=0                                                            
      CALL DGEFA(ROT,3,3,ISCR,INFO)                                     
      IF(INFO.NE.0) STOP                                                
      DET=ZERO                                                          
      CALL DGEDI(ROT,3,3,ISCR,DET,SCR,1)                                
C                                                                       
   22 CONTINUE                                                          
c     WRITE (6,702)                                                     
c 702 FORMAT(/1X,'INVERSE MATRIX OF MOMENT OF INERTIA.')                
c     CALL PRSQ(ROT,3,3,3,3)                                              
C                                                                       
C 5. TOTAL MASS ---> TOTM.                                              
C                                                                       
C 6. COMPUTE P MATRIX                                                   
C    ----------------                                                   
      DO 100 IP=1,NATM                                                  
       INDX=3*(IP-1)                                                    
       DO 100 JP=1,IP                                                   
        JNDX=3*(JP-1)                                                   
        DO 70 IC=1,3                                                    
         JEND=3                                                         
         IF(JP.EQ.IP) JEND=IC                                           
         DO 70 JC=1,JEND                                                
          SUM=ZERO                                                      
          DO 50 IA=1,3                                                  
           DO 50 IB=1,3                                                 
            IF(TENS(IA,IB,IC).EQ.0) GO TO 50                            
            DO 30 JA=1,3                                                
             DO 30 JB=1,3                                               
              IF(TENS(JA,JB,JC).EQ.0) GO TO 30                          
              SUM=SUM+TENS(IA,IB,IC)*TENS(JA,JB,JC)*ROT(IA,JA)*         
     &                X(INDX+IB)*X(JNDX+JB)                             
   30         CONTINUE                                                  
   50       CONTINUE                                                    
          II=INDX+IC                                                    
          JJ=JNDX+JC                                                    
          P(II,JJ)=SUM+DX(II)*DX(JJ)                                    
          IF(IC.EQ.JC) P(II,JJ)=P(II,JJ)+ONE/(RM(II)*RM(JJ)*TOTM)       
   70     CONTINUE                                                      
  100   CONTINUE                                                        
C                                                                       
C 7. COMPUTE DELTA(I,J)-P(I,J)                                          
      DO 110 I=1,NC1                                                    
       DO 110 J=1,I                                                     
        P(I,J)=-P(I,J)                                                  
        IF(I.EQ.J) P(I,J) = ONE +P(I,J)                                 
  110   CONTINUE                                                        
C                                                                       
C 8. NEGLECT SMALLER VALUES THAN 10**-8.                                
      DO 120 I=1,NC1                                                    
       DO 120 J=1,I                                                     
        IF(ABS(P(I,J)).LT.CUT8) P(I,J)=ZERO                             
        P(J,I)=P(I,J)                                                   
  120   CONTINUE                                                        
C                                                                       
C.DEBUG.                                                                
c     WRITE(6,703)                                                      
c 703 FORMAT(/1X,'PROJECTION MATRIX')                                   
c     CALL PRSQ(P,NC1,NC1,NC1)                                          
c     CALL PRSQ(P,NC1,NC1,maxpar,3)                                       
C                                                                       
C 10. POST AND PREMULTIPLY F BY P.                                      
C     USE COF FOR SCRATCH.                                              
      DO 150 I=1,NC1                                                    
       DO 150 J=1,NC1                                                   
        SUM=ZERO                                                        
        DO 140 K=1,NC1                                                  
  140    SUM=SUM+F(I,K)*P(K,J)                                          
  150   COF(I,J)=SUM                                                    
C                                                                       
C 11. COMPUTE P*F*P.                                                    
      DO 200 I=1,NC1                                                    
       DO 200 J=1,NC1                                                   
        SUM=ZERO                                                        
        DO 190 K=1,NC1                                                  
  190    SUM=SUM+P(I,K)*COF(K,J)                                        
  200   F(I,J)=SUM                                                      
C                                                                       
c      WRITE(6,9030)                                                     
c      CALL prsq(f,nc1,nc1,n3tm,1,6)                                          
c9030  FORMAT(/1X,'LEAVE THE SUBROUTINE <PRJFC>'//                       
c     *        1X,'PROJECTED FORCE CONSTANT MATRIX (HARTREE/BOHR**2)')   
      RETURN                                                            
      END SUBROUTINE PRJFC

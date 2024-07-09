c***********************************************************************
c  EHOOK
c***********************************************************************
c
      subroutine ehook(mw,iproc)
      use common_inc
      use perconparam; use kintcm; use cm
c
c     This routine was added on June 2, 1994.  It is the driver for
c     the energy routines in the modular version of POLYRATE.
c
c     This routine optionally accepts either unscaled (mw=0) or mass-scaled 
c     Cartesian coordinates and transforms them to Cartesian coordinates (mw=1).
c     The energy is computed using the Cartesian coordinates.  If the
c     mass-scaled option was chosen (mw=1), the coordinates are
c     transformed back to mass-scaled at the end.  

c     To add a new potential driver, one just needs to add a new block to 
c     the 'if block' with a call to a routine that accepts Cartesian 
c     coordinates (non-mass-scaled) and returns the energy.
c
c     Driver for energetics
c       The appropriate potential will be called according to the POTENTIAL 
c       keyword set in the GENERAL section.  This keyword is passed 
c       through the ipot variable:
c
c           ipot               potential
c
c            0                 hooks
c            1                 unit30/unit40
c            2                 ACES
c            3                 unit29
c
      implicit double precision (a-h,o-z)
c
      dimension fmat(n3tm,n3tm)
c
C      write (6,*) '%%%%%%%%%% calling ehook  %%%%%%%%%%%%%'
c
c     Optionally convert the mass-scaled coordinates to Cartesian
c
      if (mw.eq.1) call ytrans(2,n3,amass,x,dx)                         0723PF97
c
c     Compute the potential using the appropriate routines
c
      if (ipot.eq.0.and.lgs(30).le.0) then                              1024YC96
         call surf(v,x,dx,n3tm)
c      else if (ipot.eq.1) then
c         call mopeg(v,x,dx,.false.,n3tm)
      else if (ipot.eq.2) then
         call acalc(1,fmat,n3tm)
      endif
c
c     Put the energy at an appropriate zero of energy
      if (lezer0) v = v-ezer0                     
c
c     new format, the bath term is a function of X instead of S
c
      IF (LBATH) THEN                                                   0317YC99 
        CALL EFFBATH(0)
      ENDIF                                                             0317YC99
c
c     Optionally transform back to mass-scaled coordinates
      if (mw.eq.1) call ytrans(1,n3,amass,x,dx)                         0714PF97
c
      return
      end subroutine ehook                                       
c
c***********************************************************************
c  GHOOK 
c***********************************************************************
c
      subroutine ghook(mw,iproc)
      use common_inc
      use perconparam; use kintcm; use cm

c     This routine was added on June 2, 1994.  It is the driver for
c     the first derivative routines in the modular version of POLYRATE.
c
c     This routine optionally accepts the mass-scaled Cartesian 
c     coordinates and transforms them to Cartesian coordinates (mw=1).
c     The Cartesian coordinates are used when computing the energy and
c     first derivatives of the energy with respect to each Cartesian 
c     coordinate.  If the mass-scaled option was chosen (mw=1), the 
c     coordinates and first derivatives are transformed back to mass-
c     scaled coordinates at the end.  
c
c     To add a new potential driver, one just needs to add a new block to 
c     the 'if block' with a call to a routine that accepts Cartesian 
c     coordinates (non-mass-scaled) and returns the energy and first
c     derivatives.
c
c     Driver for gradients
c       The appropriate potential will be called according to the POTENTIAL 
c       keyword set in the GENERAL section.  This keyword is passed 
c       through the ipot variable:
c
c           ipot               potential
c
c            0                 hooks
c            1                 unit30/unit40
c            2                 ACES
c            3                 unit29
c
c
      implicit double precision (a-h,o-z)
      dimension fmat(n3tm,n3tm)
c
C      write (6,*) '%%%%%%%%%% calling ghook  %%%%%%%%%%%%%'
c
c     Optionally convert the mass-scaled coordinates to Cartesian coordinates
c
      if (mw.eq.1) call ytrans(2,n3,amass,x,dx)                         0714PF97
c
c     Compute the potential using the appropriate routines
c
      if (ipot.eq.0.and.lgs(30).le.0) then                              1024YC96

         call surf(v,x,dx,n3tm)
C
C     PUT THE LINKING ROUTINE WITH ELECTRONIC STRUCTURE PROGRAM HERE 
C     IT SHOULD RETURN THE ENERGY (DENOTED BY V)
C                          GRADIENTS (DENOTED BY DX)
C
c      else if (ipot.eq.1) then
c         call mopeg(v,x,dx,.true.,n3tm)
      else if (ipot.eq.2) then
          call acalc(2,fmat,n3tm)
      end if
c
c     Put the energy at an appropriate zero of energy
c
      if (lezer0) v = v-ezer0
c
c   calculate the derivative and energy, should be in Cartesian coord
c 
      IF (LBATH) THEN                                                   0317YC99
        CALL EFFBATH(1)
      ENDIF                                                             0317YC99
c
c     Transfer back to mass-scaled coordinates
      if (mw.eq.1) call ytrans(1,n3,amass,x,dx)                         0714PF97
c
      return
      end subroutine ghook                                          
c
c***********************************************************************
c  OHOOK
c***********************************************************************
c
      subroutine ohook (iop,iproc)
      use common_inc
      use perconparam; use kintcm; use cm
c
c     This routine was rewrittenadded on June 6, 1994.  It is the driver
c     for the geometry optimization in the modular version of POLYRATE.
c
c     This routine optionally accepts a initial guess to the Cartesian
c     coordinates of the full system and returns the optimized geometry
c     and energy
c
c     The geometry is optimized for a given chemical species:
c         For IOP = 1, or 2; the chemical species is reactant 1 or 2.
c         For IOP = 3, or 4; the chemical species is product 1 or 2.
c         For IOP = 5, the chemical species is the saddle point.
c
c     To add a new potential driver, one just needs to add a new block to 
c     the 'if block' with a call to a routine that accepts the initial
c     guess Cartesians (x) and returns the optimized geometry in the
c     array x and the energy (V).
c
c     Driver for optimization
c       The appropriate potential will be called according to the POTGEOM 
c       keyword set in the GENERAL section.  This keyword is passed 
c       through the ispot variable:
c
c           igpot              Method from
c
c            0                 POLYRATE subprogram NEWT
c            1                 ohook
c            2                 ACES
c
c
      implicit double precision (a-h,o-z)
c
      dimension fmat(n3tm,n3tm)
c
c      write (6,*) '%%%%%%%%%% calling ohook  %%%%%%%%%%%%%'
c
c     Set-up the geometry arrays for the different potentials.
c
      if (igpot.eq.0) then
        call gsetp(iop)
      else if (igpot.eq.1) then
        call gsetp(iop)
c       call gsetm(iop)
      else if (ipot.eq.2) then
        if (iop.eq.5) call gseta
      else if (ipot.eq.3) then
      end if
c
c     Compute the optimized geometry
c
      if (igpot.eq.0) then
        if (ieft.eq.1.and.iop.eq.5) then                                0317YC99
          call efmain(iop,dlx2,convg,convgt,jniter)                     0317YC99
        elseif (ief.eq.1.and.iop.ne.5) then                             0317YC99
          call efmain(iop,dlx2,convg,convgt,jniter)                     0317YC99
        else
          call newt(iop)
        endif
      else if (igpot.eq.1) then
        call ynewt(iop,iproc)                                           0227BL05
c        call gsetm(iop)
c        call mopopt(iop)
      else if (ipot.eq.2) then
         call acalc(4,fmat,n3tm)
      else if (ipot.eq.3) then
      end if
c
c     Put the energy at an appropriate zero of energy
      if (lezer0) v = v-ezer0
c
      return
      end subroutine ohook                                        
c***********************************************************************
c  PREP
c***********************************************************************
c
      subroutine prep
      use common_inc
      use perconparam; use kintcm
c
c     This routine was added on July 12, 1994.  It is the driver for
c     all preparation routines needed to set up an electronic structure
c     calculation.
c
c     This routine is called after all POLYRATE input has been read in
c     and before the table of input is written to fu6.  It is also
c     before any calculations are done.  This routine should be used
c     to call any electronic structure preparation routines.  No 
c     prep code should be written directly in this subroutine.  Everything
c     must be put in a separate subroutine.

c     To add a new potential driver, one just needs to add a new block to 
c     the 'if block' with a call to the appropriate routine.
c
c     This routine is called if the user specifies that the potential,
c     second derivatives or geometry optimizations are not to be done
c     using an analytic surface with the POLYRATE routines.  The method
c     used is given by the variable ipot, ispot, and/or igpot.
c
c      ipot,ispot,igpot         potential
c
c            0                  analytic PES
c            1                  MOPAC
c            2                  ACES
c            3                  GAMESS
c
c
      implicit double precision (a-h,o-z)
c
c
c      write (6,*) '%%%%%%%%%% calling prep  %%%%%%%%%%%%%'
c
c     Call the appropriate prep routines
c
      if (ipot .eq. 0) call setup(n3tm)                                 1220WH94
c      if (ipot.eq.1.or.ispot.eq.1.or.igpot.eq.1) then
c         call mopset(NATOMS)
c      if (ipot .eq. 1) call mopset(NATOMS)                             0317YC97
      if (ipot.eq.2.or.ispot.eq.2.or.igpot.eq.2) then
      end if
c
      return
      end subroutine prep                                         
c
c***********************************************************************
c  PREPJ
c***********************************************************************
c
      subroutine prepj(jtype)
      use common_inc
      use perconparam; use kintcm; use cm
c
c     This routine was added on May 23, 1995.  It is the species dependent
c     driver for all preparation routines needed to set up an electronic 
c     structure calculation.  Any initializations that are dependent
c     on wheter the reactants, products, saddle point, or generalized
c     transition state is computed should be done here.  JTYPE specifies
c     the species:
c
c           JTYPE               Species
c             1                 Reactant #1
c             2                 Reactant #2
c             3                 Product #1
c             4                 Product #2
c             5                 Saddle point
c             6                 Generalized transition states
c             7                 Reactant well                           0729PF97
c             8                 Product well                            0729PF97
c
c     This routine is called after each respective call to stvarj.
c     No prep code should be written directly in this subroutine.
c     Everything must be put in a separate subroutine.
 
c     To add a new potential driver, one just needs to add a new block to
c     the 'if block' with a call to the appropriate routine.
c
c     This routine is called if the user specifies that the potential,
c     second derivatives or geometry optimizations are not to be done
c     using an analytic surface with the POLYRATE routines.  The method
c     used is given by the variable ipot, ispot, and/or igpot.
c
c      ipot,ispot,igpot         potential
c
c            0                  analytic PES
c            1                  MOPAC
c            2                  ACES
c            3                  GAMESS
c
c
      implicit double precision (a-h,o-z)
c
c      write (6,*) '%%%%%%%%%% calling prepj  %%%%%%%%%%%%%'
c
c     Set the variables that are needed for all potentials
c
      if (jtype.ne.6) then
         call stlgsj(jtype)
         call stvarj(jtype)
      end if
c
c     Call the appropriate prep routines
c
      if (ipot.eq.1.or.ispot.eq.1.or.igpot.eq.1) then
      else if (ipot.eq.2.or.ispot.eq.2.or.igpot.eq.2) then
         call aceset(jtype)
      end if
c
      return
      end subroutine prepj
c***********************************************************************
c  HHOOK 
c***********************************************************************
c
      subroutine hhook(xii,fmat,sstp,nend,mw,icent,iproc)
      use common_inc
      use perconparam; use kintcm; use cm
c
c     This routine was added on June 6, 1994.  It is the driver for
c     the second derivative routines in the modular version of POLYRATE.
c
c     This routine optionally accepts the mass-scaled Cartesian 
c     coordinates and transforms them to Cartesian coordinates (mw=1).
c     The Cartesian coordinates are used when computing the energy and
c     second derivatives of the energy with respect to each Cartesian 
c     coordinate.  If the mass-scaled option was chosen (mw=1), the 
c     coordinates and first derivatives are transformed back to mass-
c     scaled at the end.  
c
c     To add a new potential driver, one just needs to add a new block to 
c     the 'if block' with a call to a routine that accepts Cartesian 
c     coordinates (non-mass-scaled) and returns the Hessian matrix (F)
c
c     Driver for Hessian
c       The appropriate potential will be called according to the HESSCAL 
c       keyword set in the SECOND section.  This keyword is passed 
c       through the ispot variable:
c
c           ispot              Method
c
c            0                 Numerical difference of ghook 
c            1                 hhook 
c            2                 ACES
c            3                 GAMESS
c
c
      implicit double precision (a-h,o-z)
c
      dimension xii(n3tm), fmat(n3tm,n3tm)
c
C      write (6,*) '%%%%%%%%%% calling hhook  %%%%%%%%%%%%%'
C      write (6,*) 'IPROC IS: ',iproc

       if (iproc .ne. 0) then
         do i=1,n3tm
           x(i)=xii(i)
         enddo
       endif
c
c     Optionally convert the mass-scaled coordinates to Cartesian coordinates
c
      if (mw.eq.1) call ytrans(2,n3,amass,x,dx)                         0714PF97
c
c     Compute the potential using the appropriate routines
c
      if (ispot.eq.0) then
          if(lgs(25) .eq. 0) then
             if(icent.eq.0) call ysecen(xii,fmat,mw,sstp)               0714PF97
             if(icent.eq.1) call ysecep(mw,sstp,nend,iproc)             0714PF97
          else if(lgs(25) .eq. 1) then
                call yderv2(nend,iproc)                                 1117BL04
          else if(lgs(25) .eq. 2) then
                call yder24(nend)                                       0714PF97
          endif
c
c HHOOK must also return the gradient
c

           call surf(v,x,dx,n3tm)
c 
      else if (ispot.eq.1) then      
C
C For POLYRATE, the HHOOK and GHOOK options in SECOND section for the
c keyword HESSCAL accomplish the same thing                             0317YC97
C
           if(lgs(25) .eq. 0) then
             if(icent.eq.0) call ysecen(xii,fmat,mw,sstp)               0714PF97
             if(icent.eq.1) call ysecep(mw,sstp,nend,iproc)             0714PF97
          else if(lgs(25) .eq. 1) then
                call yderv2(nend,iproc)                                 0714PF97
          else if(lgs(25) .eq. 2) then
                call yder24(nend)                                       0714PF97
          endif
c
c HHOOK must also return the gradient
c
           call surf(v,x,dx,n3tm)
c
C
C OR PUT IN YOUR CODE.  HERE IS AN EXAMPLE FOR MOPAC                    0317YC99
C
c          call mophes(xii, fmat, n3tm)                                 1220WH94
c
c          if (LBATH.AND.LFOPT) THEN
c             F1 = PI*PI*REDM/(16.0d0*FRICT)
c             F2 = F1/FRICT
c             DO I = 1,NEND-1
c               FMAT(NEND,I) = -F2*DIFFU(I)
c      >                   /(amass(ind(nend))*amass(ind(i)))
c               FMAT(I,NEND) = FMAT(NEND,I) 
c             ENDDO
c             FMAT(NEND,NEND)=F2
c     >                  /(amass(ind(nend))*amass(ind(nend)))
c
c     this is just a check to mimic the hhook call, however, the 'gas phase'
c     hessian is not pure in this case because mixing is added, however
c     the following lines are required for the pure gas phase hessian cases
c
c             DO I = 1,NEND-1
c              DO J = 1,I
c                FMAT(I,J) = FMAT(I,J)+F2*DIFFU(I)*DIFFU(J)
c     >                       /(amass(ind(i))*amass(ind(j)))
c                FMAT(J,I) = FMAT(I,J) 
c              ENDDO
c             ENDDO
c          endif
c
c          do 100 i = 1, nend
c             do 100 j = 1, i 
c                fmat(i,j) = fmat(i,j) / (amass(ind(i))*amass(ind(j)))  0117WH95
c                fmat(j,i) = fmat(i,j)
c 100       continue

      else if (ipot.eq.2) then
         call acalc(3,fmat,n3tm)
      else if (ipot.eq.3) then
      end if
C
c
c     Put the energy at an appropriate zero of energy
      if (lezer0) v = v-ezer0
c
c
c   calculate the derivative and energy, should be in Cartesian coord
c
      IF (LBATH) THEN                                                   0317YC99
        CALL EFFBATH(2)
      ENDIF                                                             0317YC99
c
c     Transfer back to mass-scaled coordinates
      if (mw.eq.1) call ytrans(1,n3,amass,x,dx)                         0714PF97
C
C      write (6,*) '%%%%%%%%%% exiting hhook  %%%%%%%%%%%%%'
      return
C
      end subroutine hhook                                        
c ***********************************************************************
c YTRANS
c ***********************************************************************
c
      subroutine ytrans(iop,n3,amass,x,dx)
      use perconparam
c
c     This subroutine was added by Patton Fast on July 14, 1997.  It is a
c     duplicate copy of the TRANS routine in POLYRATE.
c
      implicit double precision(a-h, o-z)
c
      dimension x(n3tm), dx(n3tm), amass(n3tm)
c
      DO 10 I = 1, N3
         IF (IOP.EQ.2) THEN
            DX(I) = DX(I)*AMASS(I)
            X(I) = X(I)/AMASS(I)
         ELSE
            DX(I) = DX(I)/AMASS(I)
            X(I) = X(I)*AMASS(I)
         ENDIF
 10   CONTINUE
      RETURN
      END subroutine ytrans
c
c ************************************************************************
c YDERV2
c ************************************************************************
c
      subroutine yderv2(n,iproc)
      use common_inc
      use perconparam
c
c     This subroutine was added by Patton Fast on July 14, 1997.  It is a
c     duplicate copy of the DERIV2 routine in POLYRATE.
c
      implicit double precision (a-h, o-z)
c
      CALL EHOOK(0,iproc)
C
      V00 = V
      STP2 = DERSTP**2
      DO 10 I = 1, N
         XOLD = X(IND(I))
         X(IND(I)) = X(IND(I))+DERSTP
C          
         CALL EHOOK(0,iproc)
C
         V10 = V
         X(IND(I)) = XOLD-DERSTP
         CALL EHOOK(0,iproc)                                                  
C
         F(I,I) = (V10-2.0D0*V00+V)/(STP2*AMASS(IND(I))**2)
         X(IND(I)) = XOLD
   10 CONTINUE
      DO 30 I = 1, N-1
         DO 20 J = I+1, N
            XIOLD = X(IND(I))
            XJOLD = X(IND(J))
            X(IND(I)) = X(IND(I))+DERSTP
            X(IND(J)) = X(IND(J))+DERSTP
            CALL EHOOK(0,iproc) 
C
            V11 = V
            X(IND(J)) = XJOLD-DERSTP
            CALL EHOOK(0,iproc) 
C
            V1N1 = V
            X(IND(I)) = XIOLD-DERSTP
            CALL EHOOK(0,iproc) 
C
            VN1N1 = V
            X(IND(J)) = XJOLD+DERSTP
            CALL EHOOK(0,iproc)
C
            F(I,J) = (V11-V1N1-V+VN1N1)/(4.0D0*STP2*AMASS(IND(I))*AMASS(
     *         IND(J)))
            F(J,I) = F(I,J)
            X(IND(I)) = XIOLD
            X(IND(J)) = XJOLD
   20    CONTINUE
   30 CONTINUE
      RETURN
      END subroutine yderv2
c
c ************************************************************************
c YDER24
c ************************************************************************
c
      subroutine yder24(nend)
      use common_inc
      use perconparam
c
c     This subroutine was added by Patton Fast on July 14, 1997.  It is a 
c     duplicate copy of the DERIV24 routine in POLYRATE.
c
      implicit double precision(a-h, o-z)
c
      dimension xxi(n3tm), f2(n3tm)
c
      H = DLX
C
C     SAVE INITIAL GEOMETRY SO IT CAN BE RESTORED AT THE END
C
      DO 10 I = 1, N3
         XXI(I) = X(I)
   10 CONTINUE
C
C     COMPUTE THE DERIVATIVE BY USING STEP SIZES OF H AND 2H
C
      DO 60 I = 1, NEND
         X(IND(I)) = XXI(IND(I))-2.0D0*H
         call ghook(0,iproc)
C
         DO 20 J = 1, I
            F2(J) = DX(IND(J))
   20    CONTINUE
         X(IND(I)) = XXI(IND(I))+2.0D0*H
         call ghook(0,iproc)
C
         DO 30 J = 1, I
            F2(J) = DX(IND(J))-F2(J)
   30    CONTINUE
         X(IND(I)) = XXI(IND(I))-H
         call ghook(0,iproc)
C
         DO 40 J = 1, I
            F(I,J) = DX(IND(J))
   40    CONTINUE
         X(IND(I)) = XXI(IND(I))+H
         call ghook(0,iproc) 
C
         DO 50 J = 1, I
            F(I,J) = (8.0D0*(DX(IND(J))-F(I,J))-F2(J))/(12.0D0*H)
            F(I,J) = F(I,J)/(AMASS(IND(I))*AMASS(IND(J)))
            f(j,i) = f(i,j)
   50    CONTINUE
         X(IND(I)) = XXI(IND(I))
   60 CONTINUE
      RETURN
      END subroutine yder24
c
c ***********************************************************************
c YSECEN
c ***********************************************************************
c
      subroutine ysecen(xii,fmat,mw,sstp)
      use common_inc
      use perconparam
c
c     This subroutine was added by Patton Fast on July 14, 1997.  It is a 
c     duplicate copy of the SECCEN routine in POLYRATE.
c
      implicit double precision (a-h,o-z)
C
      dimension  xii(n3tm),fmat(n3tm,n3tm),dx0(n3tm)
c
c     save original gradient
c
      do 4 i=1,n3 
 4       dx0(i) = dx(i)
c
c     Optionally transform to Cartesian coordinates
      if (mw.eq.1) call trans(2,n3,amass,xii,dx)
c
c     Calculation of the force constant matrix using central differences
c         (second derivatives of the energy with respect to x) 
c
      h2=2.0d0*sstp
      do 1 i=1,n3 
         x(i)=xii(i)-sstp
         call ghook(0,iproc)
c
         do 2 j=1,i 
             fmat(i,j)=dx(j)
   2     continue
c
         x(i)=xii(i)+sstp
         call ghook(0,iproc)
C
          do 3 j=1,i
            fmat(i,j)=(dx(j)-fmat(i,j))/(h2*amass(i)*amass(j))
            fmat(j,i) = fmat(i,j)
   3      continue
          x(i)=xii(i)
   1  continue
c
c     Optionally restore X to mass-scaled coordinates
      if (mw.eq.1) call trans(1,n3,amass,xii,dx)
c
      do 5 i=1,n3 
 5       dx(i) = dx0(i)
      return
      end subroutine ysecen
c
c **********************************************************************
c YSECEP
c **********************************************************************
c
      subroutine ysecep(mw,sstp,nend,iproc)
      use common_inc
      use perconparam
c
c     This subroutine was added by Patton Fast on July 14, 1997.  It is a 
c     duplicate copy of the SECCEP routine in POLYRATE.
c
      implicit double precision (a-h,o-z)
c
      dimension xxi(n3tm), dx0(n3tm)  
c
c
c
c     Save initial geometry and gradient so it can be restored at the end
c
      do 10 i = 1, n3
         xxi(i) = x(i)
         dx0(i) = dx(i)
   10 continue
c
c     Optionally transform to Cartesian coordinates
      if (mw.eq.1) call trans(2,n3,amass,x,dx)
c
c     Calculation of the force constant matrix using central differences
c         (second derivatives of the energy with respect to x) 
c
      h2=2.0d0*sstp
      do 1 i=1,nend 
         x(ind(i))=xxi(ind(i))-sstp
C
         call ghook(0,iproc)
c
         do 2 j=1,i 
             f(i,j)=dx(ind(j))
   2     continue
c
         x(ind(i))=xxi(ind(i))+sstp
         call ghook(0,iproc)
C
          do 3 j=1,i
            f(i,j)=(dx(ind(j))-f(i,j))/(h2*amass(ind(i))
     *         *amass(ind(j)))
            f(j,i) = f(i,j)
   3      continue
          x(ind(i))=xxi(ind(i))
   1  continue
      do 4 i=1,n3 
 4       dx(i)=dx0(i)
c
c     Optionally restore X to mass-scaled coordinates
      if (mw.eq.1) call trans(1,n3,amass,xxi,dx)
c
      return
      end subroutine ysecep
c
c ***********************************************************************
c YNEWT
c ***********************************************************************
c
      subroutine ynewt(iop,iproc)
      use common_inc
      use perconparam
      use kintcm; use cm
      use keyword_interface
c 
c     This subroutine was added by Patton Fast on July 14, 1997.  It is a 
c     duplicate copy of the NEWT routine in POLYRATE.
c
      implicit double precision (a-h, o-z)
c
      integer work
      logical linmin, fail
c
      dimension hessin(n3tm, n3tm), bfgs1(n3tm, n3tm)
      dimension bfgs2(n3tm, n3tm)
      dimension xxi(n3tm), x0(n3tm), dx0(n3tm), work(n3tm), deltag(n3tm)
c
      IPROC=0
      NCOUNT = 0
      NRECAL = 0
      N = NDIM(IOP)
C--Next two lines commented here to avoid printing the header
C  for V and X data when NOPRINT is selected 
C
C     WRITE(FU6,1000)
C     WRITE(FU6,1010) (IND(I),I=1,N)
C


C  Set flag for type of search performed: 
c
      cnvg  =  convg
      idorec = ihrec
      linmin=.true.
      IF (IOP .GT. 4) THEN
          ihunit = 0                        
          cnvg  =  convgt
          idorec = ihrect
          ibfgs = ibfgst
          linmin=.false.
      ENDIF
C
C  Set up the default convergent values for the energy and derivative.
C
      IF(CNVG .LE. EPS) THEN
         GTOL = 1.0D-7
      ELSE
         GTOL = CNVG
      ENDIF
      IF(NITER .LT. 0) GOTO 300
      H2 = 2.0D0*DLX2
C
C  Calculate starting energy and gradient.
C
      call ghook(0,iproc)
C
      GCOMP = ABS(DX(IND(1)))
      DO 5 I = 2,N 
         ABGC= ABS(DX(IND(I)))
         If (GCOMP.lt.ABGC)  GCOMP=ABGC
  5   CONTINUE    
c
c  Check if the geometry is already optimized
c
      IF(GCOMP.LE.GTOL) THEN  
        WRITE(FU6,*) '     Starting Geometry is a stationary point.'    
        RETURN                                                         
      ENDIF                                                           
C
C  Do initial inverse Hessian
C
      IF( ihunit .ne. 0 ) then
        do 10 j=1,n
           do 10 i=1,n
              hessin(i,j) = 0.0D0
 10     continue
        do 20 i=1,n
 20        hessin(i,i)=1.0D0
      else
           call hhook(x,f,dlx2,n,0,1,iproc)
          DO 40 I = 1,N
             DO 30 J = 1,I
                HESSIN(I,J) = F(I,J)*AMASS(ind(I))*AMASS(ind(J))
                HESSIN(J,I) = HESSIN(I,J)
 30          CONTINUE
 40       CONTINUE
          CALL MXLNEQ(HESSIN,N,N3TM,DETF,JRANK,EPS,WORK,0,N)            
      ENDIF
C
C  Main loop over iteration
C
      DO 200 ITS = 1,NITER
        DO 50 I = 1,N3
           DX0(I) = DX(I)
           X0(I) = X(I)
 50     CONTINUE
C
C  Calculate Newton-Raphson step
C
        DO 100 I = 1,N
           XXI(I) = 0.0D0
           DO 90 J = 1,N
              XXI(I) = XXI(I) - HESSIN(I,J)*DX0(IND(J))
 90        CONTINUE
 100    CONTINUE
        STEPC = ABS(XXI(1))
        MXIND = 1
        DO 105 I = 2,N
           ABXXI= ABS(XXI(I))
           If (STEPC.lt.ABXXI)  then 
             STEPC=ABXXI
             MXIND=I
           endif
 105    CONTINUE
C
C   ...then Scale the Newton-Raphson step
C
        stepmx = scale/stepc
        IF (linmin) then
           CALL LINMN(X0,XXI,N,FRET, stepmx)
           stepc=abs(xxi(mxind))
        else if (stepc .gt. scale) then
           do 115 i=1,n
 115          xxi(i)= xxi(i)*stepmx
        endif
C
C   Move to new geometry and calculate new energy and gradient
C
        DO 120 I = 1,N 
           X(IND(I)) = X0(IND(I)) + XXI(I) 
120     CONTINUE                        
        call ghook(0,iproc) 
C
        GCOMP = ABS(DX(IND(1)))
        DO 130 I = 2,N 
           ABGC= ABS(DX(IND(I)))
           If (GCOMP.lt.ABGC)  GCOMP=ABGC
130     CONTINUE      
C
C Test for convergence
C
        IF(GCOMP.LE.GTOL) THEN 
          WRITE(FU6,1300) ITS
          GOTO 300
        ENDIF
        IF (STEPC .LT. STPTOL) THEN
           FAIL=.TRUE.
           write(fu6,1350)
           if(iretry .eq. 0) GOTO 300
        ENDIF
        If(FAIL) then 
           write(fu6,'("Switching to full Newton")')
           idorec=1
           nrecal=idorec
           linmin=.false.
           FAIL=.False.
        ElSE
           NRECAL=NRECAL+1
        ENDIF
C
C  Write out the information at the ITS iteration.
C
c
C--Header for extra printing when PRINT is active
        IF (IPRXNT .NE. 0 .AND. ITS .LT. 2) THEN
         WRITE(FU6,1000) 
         WRITE(FU6,1010)
        ENDIF
        If (iprxnt .ne. 0) then 
         DO 135  J = 1,N 
           IF (J .LT. 2) THEN 
              write(fu6,1020) ITS, V, IND(J), X(IND(J)), DX(IND(J)) 
           ELSE
              write(fu6,1030) IND(J), X(IND(J)), DX(IND(J)) 
           ENDIF
135      CONTINUE
C          write(fu6,'("Step = ",I4)') ITS
C          WRITE(FU6,1100) V,(X(IND(I)),I=1,N)
C          IF(LGS(1).EQ.2) WRITE(FU6,1200)(DX(IND(I)),I=1,N)
C       endif
        endif
c
c  Calculate new inverse hessian
c
        if( NRECAL .eq. idorec ) then
          NRECAL=0
          If(iprxnt.ne.0) write(fu6,'("Hessian recalculated")')  
c get exact inverse hessian
           call hhook(x,f,dlx2,n,0,1,iproc)
          DO 150 I = 1,N
             DO 140 J = 1,I
                HESSIN(I,J) = F(I,J)*AMASS(ind(I))*AMASS(ind(J))
                HESSIN(J,I) = HESSIN(I,J)
 140         CONTINUE
 150      CONTINUE
          CALL MXLNEQ(HESSIN,N,N3TM,DETF,JRANK,EPS,WORK,0,N)            
        ELSE
          IF(ibfgs .eq. 1) then
c
c   do BFGS update
c
             If(iprxnt.ne.0) write(fu6,
     *          '("Hessian updated using BFGS formula")')
             do 152 i=1,N
 152           deltag(i)= dx(ind(i)) - dx0(ind(i))
             dgxdot=0.0d0
             do 153 i=1,N
 153            dgxdot = dgxdot + xxi(i)*deltag(i)
             do 155 j=1,N
                do 155 i=1,N
c
c   ... build ( I - Dx(Dg)t/DOT(Dx,Dg) )
c
                   BFGS1(i,j)= -XXI(I)*deltag(j) / dgxdot
 155         continue
             do 156 i=1,N
 156            BFGS1(i,i)= 1.0d0 + BFGS1(i,i)
c
c   ... calculate (BFGS1)(HESSIN)(BFGS1)t
c
             call DGEMM('N','T',N, N, N, 1.0d0, HESSIN, N3TM,
     &                  BFGS1, N3TM, 0.0d0, BFGS2, N3TM)
             call DGEMM('N','N',N, N, N, 1.0d0, BFGS1, N3TM, 
     &                  BFGS2, N3TM, 0.0d0, HESSIN, N3TM)
c
c   ... and add Dx(Dx)t/DOT(Dx,Dg) to it.
c
             do 170 j=1,N
               do 170 i=1,N
                 HESSIN(i,j) = HESSIN(i,j) + xxi(i)*xxi(j)/dgxdot
 170         continue
          ELSE
            If (iprxnt .ne. 0) write(fu6,'("Hessian kept frozen")')
          ENDIF
        ENDIF
c End main loop
 200  CONTINUE
C
C Maximum number of iterations exceeded
C
      WRITE(FU6,1400)
      FAIL=.TRUE.
 300  CONTINUE
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
C
      ENDIF
C
      WRITE(FU6,1600) V,V*CKCAL 
      IF(FAIL) STOP
      RETURN
C
 1000 FORMAT(/1X,'Energy (a.u.), geometry (unscaled a.u.) and ',
     * 'gradient at each',/,1x,'optimization iteration')
C1010 FORMAT(1X,'      V         ',' X(i),i=',3I16,/,(25X,3I16))
 1010 FORMAT(/1X,'Step',10X,'V',8X,'IND',9X,'X',14X,'DX',/,1X,68('-'))
 1020 FORMAT(1X,I3,2X,1P,E16.8,I3,3X,2(E16.8))
 1030 FORMAT(22X,I3,3X,2(E16.8))
 1100 FORMAT (/1X,1P,E16.8,8X,3E16.6,/,(25X,3E16.6))
 1200 FORMAT ( 18X,'DX(i)',1P,/,(25X,3E16.6)) 
 1300 FORMAT(/,' Search has converged after ',I4,' iterations')
 1350 FORMAT('Max. displacement component change smaller than STPTOL.')
 1400 FORMAT(/,' MAXIMUM NUMBER OF ITERATIONS EXCEEDED',/)
 1600 FORMAT (/,'  V = ',1PE16.8,' hartrees  (',0P,F16.8,' kcal/mol)')
C1700 FORMAT (/1X,'Final geometry and derivatives in unscaled',
C    * ' Cartesians (a.u.)')
 1700 FORMAT (/1X,'Final geometry in unscaled Cartesians (bohrs)')      0405JZ07
 1702 FORMAT (/1X,'Final geometry in unscaled Cartesians (angstroms)')  0405JZ07
 1710 FORMAT(/1X,4HAtom,11X,'X',15X,'Y',15X,'Z',/)
 1712 FORMAT (/1X,'Final derivatives in unscaled Cartesians (a.u.)')    0405JZ07
 1720 FORMAT(/1X,4HAtom,11X,'DX',14X,'DY',14X,'DZ',/)
 1800 FORMAT (1X,I3,4X,1P,3E16.6)
C
      END subroutine ynewt

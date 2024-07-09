c
c***********************************************************************
      subroutine ehook(mw,iproc)
c***********************************************************************
c
c     This routine optionally accepts the mass-weighted cartesian 
c     coordinates and transforms them to cartesian coordinates (mw=1).
c     The energy is computed using the cartesian coordinates.  If the
c     mass-weighted option was chosen (mw=1), the coordinates are
c     transformed back to mass-weighted at the end.  
c
      use perconparam
      use common_inc
      use cm
      use gaussrate_param; use gaussrate_comm
      implicit real(8) (a-h,o-z)
c
      real(8) fmat(n3tm,n3tm)
c     
c     If it is a Grate restart, read the energy     
c     
      irout=1
      if (irest.eq.1) call grest(irout,dummy)
      if (irout.eq.0) return
c
c     Optionally convert the mass-weighted coordinates to cartesian
c
      if (mw.eq.1) call trans(2,n3,amass,x,dx)
c
c     Compute the potential using the appropriate routines
c
c     If the energy corresponds to a stationary point, check if it has
c     already been calculated and stored in redinf.
c
        if (iop.eq.6) then                                              1128JC97
            i=5
            nacat=natom
        else 
          if (iop.gt.0) then
            call redinf(iop,fmat,ierr)
         write(6,*) 'EHOOK1: IERR,IOP, V',ierr, IOP, V
            if (ierr.eq.0) goto 1
            i=iop
          else
            i=-iop
            iop=-iop
          endif
            if (iop.eq.5) then
                nacat=natom
            else
                nacat=nratom(iop)
            endif
        endif
c
c     If the point has not been stored, it calculates it
c
        ich=icharge(i)
        imul=imultipl(i)
        call escener(natoms,ich,imul,natom,label,
     *iatsv,v,x,iop,nacat,iproc,ivrc)
        write(6,*) 'EHOOK: IOP, V', IOP, V
c
c     Optionally transfer back to mass-weighted coordinates
1     if (mw.eq.1) call trans(1,n3,amass,x,dx)
c
c     Put the energy at an appropriate zero of energy
      if (lezer0) v = v-ezer0     
c
c     new format, the bath term is a function of X instead of S
c
      IF (LBATH) THEN
        CALL EFFBATH(0)
      ENDIF
c     
c     If it is a Grate restart, write the energy
c     
      if (irest.eq.1) then
          irout=5
          call grest(irout,dummy)
      endif
c
      return
      end subroutine ehook                                       
c
c***********************************************************************
      subroutine ghook(mw,iproc)
c***********************************************************************
c
c     This routine optionally accepts the mass-weighted cartesian 
c     coordinates and transforms them to cartesian coordinates (mw=1).
c     The cartesian coordinates are used when computing the energy and
c     first derivatives of the energy with respect to each cartesian 
c     coordinate.  If the mass-weighted option was chosen (mw=1), the 
c     coordinates and first derivatives are transformed back to mass-
c     weighted at the end.  
c
c
      use perconparam
      use common_inc
      use cm
      use gaussrate_param; use gaussrate_comm
      implicit real(8) (a-h,o-z)
c
      real(8) fmat(n3tm,n3tm)
c     
c     If it is a Grate restart, read the energy and first der.     
c     
      irout=2
      if (irest.eq.1) call grest(irout,dummy)
      if (irout.eq.0) return
c
c     Optionally convert the mass-weighted coordinates to cartesian
c
      if (mw.eq.1) call trans(2,n3,amass,x,dx)
c
c     Compute the potential using the appropriate routines
c     If the calculation corresponds to a stationary point, check if it has
c     already been calculated and stored in redinf.
c
        if (iop.eq.6) then                                              1128JC97
            i=5
            nacat=natom
        else
            call redinf(iop,fmat,ierr)
            if (ierr.eq.0) goto 1
            i=iop
            if (iop.eq.5) then
                nacat=natom
            else
                nacat=nratom(iop)
            endif
        endif
c
c     If the point has not been stored, it calculates it
c
        ich=icharge(i)
        imul=imultipl(i)
        call escfirst(natoms,ich,imul,label,
     *iatsv,v,x,dx,i,nacat)
c
c     Put the energy at an appropriate zero of energy
c
1     if (lezer0) v = v-ezer0
c
c     Calculate the derivative and energy, should be in Cartesian
c     coordinates
c
      IF (LBATH) THEN
        CALL EFFBATH(1)
      ENDIF
c
c
c     Transfer back to mass-weighted coordinates
      if (mw.eq.1) call trans(1,n3,amass,x,dx)
c
c
c     If it is a Grate restart, write the energy and first der.
c
      if (irest.eq.1) then
          irout=6
          call grest(irout,dummy)
      endif
c
      return
      end subroutine ghook                                         
c
c***********************************************************************
         subroutine ohook (jtype,iproc)
c***********************************************************************
c
c     This routine optionally accepts a initial guess to the cartesian
c     coordinates of the full system and returns the optimized geometry
c     and energy
C
c     If Gaussian is optimizing, all the optimization has been carried out 
c     in prep. Therefore, this subroutine only reads the and prints out
c     the results of the calculations in prep
c
c     The geometry is optimized for a given chemical species:
c       JTYPE = 1, or 2; the chemical species is reactant 1 or 2.
c       JTYPE = 3, or 4; the chemical species is product 1 or 2.
c       JTYPE = 5, the chemical species is the saddle point.
c       JTYPE = 6, or 7; the chemical species is reactants or 
c                            products wells.
c
      use perconparam
      use common_inc
      use cm; use keyword_interface, only : gufac6,iunit6
      use kintcm, only : igpot,isup
      use gaussrate_param; use gaussrate_comm
      implicit real(8) (a-h,o-z)
c
c     dimension fmat(n3tm,n3tm)                                         0701TA03
c     Variable fmat was not used so it was taken out in Gaussrate 9.1.  0701TA03
c     
c     if Polyrate is going to do the optimization, call newt.
c
      if (igpot.eq.0) then
         call gsetp(jtype)
         if (nratom(jtype).gt.1) then
            call newt(jtype)
         else 
            call ehook(0,iproc)
         endif
         if (lezer0) v = v-ezer0
         return
      endif
      if (isup.eq.1) call gsetp(jtype)
c
c     else:
c     If it is a Grate restart, read the geom, energ and 1st der.
c     
      irout=4
      if (irest.eq.1) call grest(irout,dummy)
      if (irout.eq.0) goto 3
c
      if (jtype.le.2) then
           v=enresv
            if (isup.eq.1) then
                if (jtype.eq.1) v=ereac1
                if (jtype.eq.2) v=ereac2
            endif
           do m=1,n3tm
              x(m)=geresv(m)
             dx(m)=fsresv(m)
           enddo
      else if (jtype.le.4) then
           v=enprsv
            if (isup.eq.1) then
                if (jtype.eq.3) v=eprod1
                if (jtype.eq.4) v=eprod2
            endif
           do m=1,n3tm
              x(m)=geprsv(m)
             dx(m)=fsprsv(m)
           enddo
      else if (jtype.eq.7) then                                         1128JC97
           v=enwrsv                                                     1128JC97
           do m=1,n3tm                                                  1128JC97
              x(m)=gewrsv(m)                                            1128JC97
             dx(m)=fswrsv(m)                                            1128JC97
           enddo                                                        1128JC97
      else if (jtype.eq.8) then                                         1128JC97
           v=enwpsv                                                     1128JC97
           do m=1,n3tm                                                  1128JC97
              x(m)=gewpsv(m)                                            1128JC97
             dx(m)=fswpsv(m)                                            1128JC97
           enddo                                                        1128JC97
      else
           v=enstsv
           do m=1,n3tm
              x(m)=gestsv(m)
             dx(m)=fsstsv(m)
           enddo
      endif
c    
c     Print out the results of the Gaussian optimization
c
c     Geometry
c
3     if (jtype.eq.5) write(fu6,1006)
C     write (fu6,1001)
      if(iunit6.eq.1) write(fu6,1001)                                   0629JZ07
      if(iunit6.eq.0) write(fu6,1002)                                   0629JZ07
      write (fu6,1011)
      inat=1
1     if (jtype.ne.5) then
         inum = iatsv(inat,jtype)
      else 
         inum = inat
      endif
c        write(fu6,1021)inum,(x(3*(inum-1)+j), j=1,3)
         write(fu6,1021)inum,(x(3*(inum-1)+j)/gufac6, j=1,3)            0629JZ07
      inat=inat+1
      if (jtype.eq.5) then                                              1128JC97
           if (inat.le.natom) goto 1
      else
         if(inat.le.natom) then
           if (iatsv(inat,jtype).ne.0) goto 1
         endif
      endif
      write (fu6,1011)
c
c     Gradients
c
      write (fu6,1031)
      write (fu6,1011)
      inat=1
2     if (jtype.ne.5) then                                              0615JC98
         inum = iatsv(inat,jtype)
      else
         inum = inat
      endif
         write(fu6,1021)inum,(dx(3*(inum-1)+j), j=1,3)
      inat=inat+1
      if (jtype.eq.5) then                                              1128JC97
           if (inat.le.natom) goto 2
      else
         if(inat.le.natom) then
           if (iatsv(inat,jtype).ne.0) goto 2
         endif
      endif
      write (fu6,1011)
c
c     Calculate the derivative and energy, should be in Cartesian
c     coordinates
c
      IF (LBATH) THEN
        CALL EFFBATH(1)
      ENDIF
c
c     If it is a Grate restart, write the geom., energy and 1st der.
c     
      if (irout.eq.0) return
      if (irest.eq.1) then
          irout=8
          call grest(irout,dummy)
      endif
      return
1001     format(/4x,'Geometry optimized using Gaussian (a.u.)')
1002     format(/4x,'Geometry optimized using Gaussian (angstrom)')
1006     format(//1X,32(1H*),' Saddle point ',32(1H*)//)
1011     format(/4x,55('-'),/)
1021     format(4x,i2,8x,3(f11.6,4x))
1031     format(/4x,'Gradients of the optimized structure (a.u.)')
      end subroutine ohook                                          
c
c***********************************************************************
        SUBROUTINE ginpene(ICHARGEJ,IMULTIPLJ,LABEL,iatsv,COORD,
     *  jtype,nacat,NATESC)
c***********************************************************************
c
c     This subroutine writes the input for an energy calculation
c
      use perconparam
C     use cm; 
      use gaussrate_param; use gaussrate_comm
      IMPLICIT real(8) (A-H,O-Z)
C
C     DIMENSION COORD(N3TM),DX(N3TM)                                    0701TA03
C     Variable DX was not used so it was taken out in Gaussrate 9.1.    0701TA03
      real(8) COORD(N3TM)                                               0701TA03
      integer LABEL(NATESC),IATSV(NATESC,8)                             1128JC97
C
      OPEN (UNIT=FU81,STATUS='NEW',FILE='esp.fu81')
C
c   Write the link0 commands
C
       if (inlcer.gt.0) then
         DO ILINIP=1,inlcer
         WRITE(FU81,'(a80)') cerblk(ILINIP)
         ENDDO
       endif
C
C   Write the keywords line 
C
       DO ILINIP=1,inlene(jtype)
       WRITE(FU81,'(a80)') enerot(jtype,ILINIP)
       ENDDO
c
       WRITE(FU81,1001)
C
C   Write charge and multiplicity and atomic symbols and coordinates
C
      WRITE(FU81,1011) ICHARGEJ,IMULTIPLJ
c
      i=1
1        if (jtype.ne.5.and.jtype.ne.6) then                            1128JC97
            inum = iatsv(i,jtype)
         else
            inum = i
         end if
         ilab = label(inum)
         write(FU81,1021)ilab,(coord(3*(inum-1)+j), j=1,3)
      i=i+1
      if (jtype.eq.5.or.jtype.eq.6) then                                1128JC97
               if (i.le.natom) then
                     goto 1
               endif
      else
               if (iatsv(i,jtype).ne.0) then
                     goto 1
               endif
      endif
      WRITE(FU81,1031)
C
c   Write the basis set
C
       if (inlbas.gt.0) then
         DO ILINIP=1,inlbas
         WRITE(FU81,'(a80)') basblk(ILINIP)
         ENDDO
       endif
      WRITE(FU81,1031)
C
      CLOSE (FU81)
C
      RETURN
C
1001  FORMAT (/,1x,'Gaussian calculation: energy',/)
1011  FORMAT (1x,I3,1x,I3)
1021  FORMAT (1x,I3,5x,3(f14.8,3x))
1031  FORMAT (/,$)
C
      END subroutine ginpene
c
c***********************************************************************
        SUBROUTINE ginpfrs(ICHARGEJ,IMULTIPLJ,LABEL,iatsv,COORD,
     *  jtype,nacat,NATESC)
c***********************************************************************
C
C   Subroutine that writes the appropriate GAUSSIAN 09/03/98/94 input 
C   file for a first derivative calculation
C
      use perconparam
c     use cm;
      use gaussrate_param; use gaussrate_comm
      IMPLICIT real(8) (A-H,O-Z)
C
C     DIMENSION COORD(N3TM),DX(N3TM)                                    0701TA03
C     Variable DX was not used so it was taken out in Gaussrate 9.1.    0701TA03
      DIMENSION COORD(N3TM)                                             0701TA03
      DIMENSION LABEL(NATESC),IATSV(NATESC,8)                           1128JC97
C
      OPEN (UNIT=FU81,STATUS='NEW',FILE='esp.fu81')
C
c   Write the link0 commands
C
       if (inlcer.gt.0) then
         DO ILINIP=1,inlcer
         WRITE(FU81,'(a80)') cerblk(ILINIP)
         ENDDO
       endif
C
C   Write the keywords line 
C
       DO ILINIP=1,inlfrs(jtype)
       WRITE(FU81,'(a80)') firrot(jtype,ILINIP)
       ENDDO
       WRITE(FU81,1001)
C
C   Write charge and multiplicity and atomic symbols and coordinates
C
      WRITE(FU81,1011) ICHARGEJ,IMULTIPLJ
c
      i=1
1        if (jtype.ne.5.and.jtype.ne.6) then                            1128JC97
            inum = iatsv(i,jtype)
         else
            inum = i
         end if
         ilab = label(inum)
         write(FU81,1021)ilab,(coord(3*(inum-1)+j), j=1,3)
      i=i+1
      if (jtype.eq.5.or.jtype.eq.6) then                                1128JC97
           if (i.le.natom) goto 1
      else
          if(i.le.natom) then
               if (iatsv(i,jtype).ne.0) goto 1
          endif
      endif
      WRITE(FU81,1031)
C
c   Write the basis set
C
       if (inlbas.gt.0) then
         DO ILINIP=1,inlbas
         WRITE(FU81,'(a80)') basblk(ILINIP)
         ENDDO
       endif
      WRITE(FU81,1031)
      CLOSE (FU81)
C
      RETURN
C
1001  FORMAT (/,1x,'Gaussian calculation: first derivatives',/)
1011  FORMAT (1x,I3,1x,I3)
1021  FORMAT (1x,I3,5x,3(f14.8,3x))
1031  FORMAT (/,$)
C
      END subroutine ginpfrs
c
c***********************************************************************
        SUBROUTINE ginpsec(ICHARGEJ,IMULTIPLJ,LABEL,IATSV,COORD,
     *  jtype,nacat,NATESC)
c***********************************************************************
C
C   Subroutine that writes the appropriate GAUSSIAN 09/03/98/94 input 
C   file for performing a second derivative calculation
C
      use perconparam
c     use cm;
      use gaussrate_param; use gaussrate_comm
      IMPLICIT real(8) (A-H,O-Z)
C
C     DIMENSION COORD(N3TM),DX(N3TM)                                    0701TA03
C     Variable DX was not used so it was taken out in Gaussrate 9.1.    0701TA03
      DIMENSION COORD(N3TM)                                             0701TA03
      DIMENSION LABEL(NATESC),IATSV(NATESC,8)                           1128JC97
C
      OPEN (UNIT=FU81,STATUS='NEW',FILE='esp.fu81')
C
c   Write the link0 commands
C
       if (inlcer.gt.0) then
         DO ILINIP=1,inlcer
         WRITE(FU81,'(a80)') cerblk(ILINIP)
         ENDDO
       endif
C
C   Write the keywords line 
C
       DO ILINIP=1,inlscr(jtype)
       WRITE(FU81,'(a80)') secrot(jtype,ILINIP)
       ENDDO
      WRITE(FU81,1001) 
C
C   Write charge and multiplicity and atomic symbols and coordinates
C
      WRITE(FU81,1011) ICHARGEJ,IMULTIPLJ
c
c   Include only the atoms present on the desired system
c
      i=1
1        if (jtype.ne.5.and.jtype.ne.6) then                            1128JC97
c
c   If it is reactant or product, only some atoms are involved in the
c   calculation. 
c
            inum = iatsv(i,jtype)
         else
c
c   If it is the saddle point, all the atoms are involved
c
            inum = i
         end if
         ilab = label(inum)
         write(FU81,1021)ilab,(coord(3*(inum-1)+j), j=1,3)
      i=i+1
c
c   Check when it has to stop
c
      if (jtype.eq.5.or.jtype.eq.6) then                                1128JC97
               if (i.le.natom) then
                     goto 1
               endif
      else
         if(i.le.natom) then
               if (iatsv(i,jtype).ne.0) then
                     goto 1
               endif
         endif
      endif
      WRITE(FU81,1031)
C
c   Write the basis set
C
       if (inlbas.gt.0) then
         DO ILINIP=1,inlbas
         WRITE(FU81,'(a80)') basblk(ILINIP)
         ENDDO
       endif
      WRITE(FU81,1031)
      CLOSE (FU81)
C
      RETURN
C
1001  FORMAT (/,1x,'Gaussian calculation: second derivatives',/)
1011  FORMAT (1x,I3,1x,I3)
1021  FORMAT (1x,I3,5x,3(f14.8,3x))
1031  FORMAT (/,$)
C
      END subroutine ginpsec
c
c***********************************************************************
      SUBROUTINE goutsec (IP,V,COORD,TMPDX,FMAT,JTYPE,IATSV,
     *NACAT,NATESC)
c***********************************************************************
C
C  Subroutine that extracts the geometry, energy, first and second 
C  derivatives information from the GAUSSIAN 09/03/98/94 formatted 
C  checkpoint file (Test.FChk)
C
      use perconparam
      use cm;
      use gaussrate_param; use gaussrate_comm
      IMPLICIT real(8) (A-H,O-Z)
C
C
C     DIMENSION COORD(N3TM),DX(N3TM),FMAT(N3TM,N3TM)                    0701TA03
C     Variable DX was not used so it was taken out in Gaussrate 9.1.    0701TA03
      real(8) ::  COORD(N3TM),FMAT(N3TM,N3TM)                           0701TA03
      real(8) ::  TMPFMAT(N3TM,N3TM)                                    1128JC97
      real(8) ::  TMPFM(N3TM*N3TM), TMPDX(N3TM)
      integer ::  IATSV(NATESC,8)
      CHARACTER*80 STRREAD
      CHARACTER*80 CASE
      CHARACTER*25 SECID
      CHARACTER*16 FORCEID
      CHARACTER*16 FRCIDE1
      CHARACTER*12 ENEID
      CHARACTER*29 GEOMID
      LOGICAL FINDE, FINDC, FINDG, FINDH
C     CHARACTER*1 CHARKEY(10)                                           0701TA03
C     Variable CHARKEY was not used so                                  0701TA03
C     it was taken out in Gaussrate 9.1.                                0701TA03
c
c     IP = 0 obtain energy
c     IP = 1 obtain energy and geometry
c     IP = 2 obtain energy, geometry, and gradient
c     IP = 3 obtain energy, geometry, gradient, and hessian
c
c     This subroutine was reorganized to make it compatible with both G03 and 
c     G09, because G03 and G09 Test.fchk files have energy, geometry, 
c     gradient, and hessian in different order.
c     -- by Jingjing Zheng Nov. 2009
c
      do i=1,n3tm
        tmpdx(i)=0
      enddo
      do i=1,n3tm*n3tm
        tmpfm(i)=0
      enddo
      FINDE = .FALSE.
      FINDC = .FALSE.
      FINDG = .FALSE.
      FINDH = .FALSE.
C
      OPEN (UNIT=FU85,STATUS='UNKNOWN',FILE='Test.FChk')
C
C     It looks for the energy, coordinates, gradient, and Hessian by means 
C     of their keywords (ID), respectively.
C
      ENEID='total energy'
      LENE = 11
      GEOMID='current cartesian coordinates'
      LENC = 28
c     In G03 revision E.1 and G09 the title is 'Cartesian gradients'
c     and in earlier versions, the title is 'Cartesian forces'
      FORCEID='cartesian forces'
      FRCIDE1='cartesian gradie'
      LENG = 15  
      SECID='cartesian force constants'
      LENH = 24
C
1015  READ (FU85,1041,END=1002) STRREAD
      STRREAD=CASE(STRREAD)
C
C     Look for energy by means of the title  'Total energy' 
c
      DO 120 ILINPOS=1,3 
          IF (STRREAD(ILINPOS:ILINPOS+LENE).NE.ENEID) THEN
               GOTO 120 
          ELSE
               V=cfloat(STRREAD(46:80))   
               FINDE = .TRUE.
               GOTO 150 
          ENDIF
120   CONTINUE
150   IF (FINDE.AND.IP.LT.1) GOTO 1999
c
c     look for the cartesian coordinates, by means
c     of the title 'Current cartesian coordinates'
C
      DO 6120 ILINPOS=1,3
          IF (STRREAD(ILINPOS:ILINPOS+LENC).NE.geomid) THEN
             GOTO 6120
          ELSE
             I=0
             NUMDXN=NACAT*3
6035         IF (NUMDXN.GT.5) THEN
               READ (FU85,*) (COORD(I+J), J=1,5)
               NUMDXN=NUMDXN-5
               I=I+5
               GOTO 6035
             ELSE
               READ (FU85,*) (COORD(I+j), J=1,NUMDXN)
             ENDIF
             FINDC = .TRUE.
             GOTO 200 
          ENDIF
6120  CONTINUE
c
c     Look for the cartesian forces, by means
c     of the title 'Cartesian forces' or 'Cartesian gradie'
c
200   IF (FINDE.AND.FINDC.AND.IP.EQ.1) GOTO 1999
      DO 220 ILINPOS=1,3
          IF (STRREAD(ILINPOS:ILINPOS+15).ne.FORCEID. and .             0820JC97
     *        STRREAD(ILINPOS:ILINPOS+15).ne.FRCIDE1) THEN              0820JC97
              GOTO 220
          ELSE
              I=0
              NUMDXN=NACAT*3
2025          IF (NUMDXN.GT.5) THEN
                READ (FU85,*) (TMPDX(I+J), J=1,5)
                NUMDXN=NUMDXN-5
                I=I+5
                GOTO 2025
              ELSE
                READ (FU85,*) (TMPDX(I+j), J=1,NUMDXN)
              ENDIF
              FINDG = .TRUE.
              GOTO 300
          ENDIF
220   CONTINUE
c
c     Look for the second derivative information
c     by means of the title 'Cartesian forces constants' 
c
300   IF (FINDE.AND.FINDG.AND.IP.EQ.2) GOTO 1999
      DO 320 ILINPOS=1,3
          IF (STRREAD(ILINPOS:ILINPOS+24).ne.SECID) THEN
              GOTO 320
          ELSE
              I=0
              NUMDXN=((NACAT*3)*(NACAT*3-1))/2+NACAT*3
3025          IF (NUMDXN.GT.5) THEN
                 READ (FU85,*) (TMPFM(I+J), J=1,5)
                 NUMDXN=NUMDXN-5
                 I=I+5
                 GOTO 3025
              ELSE
                 READ (FU85,*) (TMPFM(I+J), J=1,NUMDXN)
              ENDIF
              FINDH = .TRUE.
              GOTO 3035
          ENDIF
320   CONTINUE
C
C   Now we need a reordering of the second derivative information
C   We will convert the vector of triangular force constants, TMPFM,
C   into the hessian matrix TMPFMAT
C
3035  IF(FINDH) THEN
        K=1
        DO 410 INX=1,3*NACAT
        DO 420 INY=1,INX
          TMPFMAT(INX,INY)=TMPFM(K)
          IF (INX.NE.INY) TMPFMAT(INY,INX)=TMPFMAT(INX,INY)
          K=K+1
420     CONTINUE
410     CONTINUE
C
        if (jtype.ne.5.and.jtype.ne.6) then                             1128JC97
C
C   Now, according to the ordering of the atoms at the saddle point,
C   we reorder the hessian information
C
         do l=1,nacat
         do m=1,nacat
           do i=1,3
           do j=1,3
             innx=3*(iatsv(l,jtype)-1)+i
             inny=3*(iatsv(m,jtype)-1)+j
             inox=3*(l-1)+i
             inoy=3*(m-1)+j
               FMAT(innx,inny)=TMPFMAT(inox,inoy)
           enddo
           enddo
         enddo
         enddo
        else
C
C   If we have calculated the saddle point Hessian, we don't need the
C   reordering
C
         do innx=1,n3tm
         do inny=1,n3tm
             FMAT(innx,inny)=TMPFMAT(innx,inny)
         enddo
         enddo
        endif
C
      ENDIF
      GOTO 1015
c
1002  IF (IP.EQ.0.AND. .NOT.FINDE) THEN
         WRITE (FU6,*) 'Error searching Gaussian checkpoint file for ',
     +                 'energy '                                        0701TA03
C        CALL MPI_Abort(MPI_COMM_WORLD,MPI_ERRORCODE,IERR)
         IF(IVRC.EQ.1) then                                             0626JZ08
          V = 10.000D0
          GOTO 1999
         ELSE
          STOP
         ENDIF
      ELSEIF (IP.EQ.1.AND.(.NOT.FINDC.OR..NOT.FINDE)) THEN
         WRITE (FU6,*) 'Error searching Gaussian checkpoint file for ', 0701TA03
     +                 'Cartesian coordinates'                    
         STOP
      ELSEIF (IP.EQ.2.AND.(.NOT.FINDG.OR..NOT.FINDE)) THEN
         WRITE (FU6,*) 'Error searching Gaussian checkpoint file for ', 0701TA03
     +                 'gradient'
         STOP
      ELSEIF (IP.EQ.3.AND.(.NOT.FINDH.OR..NOT.FINDG.OR..NOT.FINDE))THEN
         WRITE (FU6,*) 'Error searching Gaussian checkpoint file for ', 0701TA03
     +                 'Hessian'
         STOP
      ENDIF
c
1999  CLOSE (FU85)
      RETURN
1041  FORMAT (A80)
      END subroutine goutsec
c
c***********************************************************************
       subroutine grest(irout,fmat)
c***********************************************************************
C
C    This subroutine compares the geometry with the one in the file and
C    if they are the same, it reads the properties of the point, so that
C    it will not be calculated again by means of Gaussian.
C    If the properties were stored, it returns irout=0
C
      use perconparam
      use common_inc, only : x,dx,iatsv,v
      use cm, only : ivrc
      use gaussrate_param; use gaussrate_comm
      implicit real(8) (a-h,o-z)
      dimension fmat(n3tm,n3tm)
C     fallout=1.0D-8                                                    0825JC00
      icontrw=0
C
C     The first thing we have to do is to check if we only want to write
C     (then, irout>4) or we want reading and writing. In the last case:
C
1     if (irout.le.4) then
C
C     First, if the previous comparison was ok, we read everything and put 
C     it on an array for the next comparisons
C     Note: read and write icharge and imultipl for irread = 4 or irout = 4
C     cases are commented
C
        read (fu83,*,END=999) irread
        if (irread.eq.4) read(fu83,*,END=999)ip,icharge(ip),imultipl(ip)
        read (fu83,*,END=999) vread
        do icount=1,3*natom
        read (fu83,*,END=999) georead(icount)
        enddo
        do icount=1,3*natom
        read (fu83,*,END=999) dxread(icount)
        enddo
        if (irread.eq.3) then
        do icount=1,3*natom
        do jcount=1,3*natom
          read (fu83,*,END=999) fmread(icount,jcount)
        enddo
        enddo
      endif
C
C    If the stored point does the same calculation that we want to do,
C    continue. Otherwise, the program looks for a place where it is done
C
        if (irout.ne.irread) then
               if (icontrw.eq.0) then
                  icontrw=1
                  rewind fu83
                  goto 1
               else
                  goto 1
               endif
        endif
C
C    If it is a geometry optimization, it reads geometry, energy and 1st der.
C
        if (irout.eq.4) then
          v=vread
          do icount=1,natom*3
             x(icount)=georead(icount)
          enddo
          do icount=1,natom*3
             dx(icount)=dxread(icount)
          enddo
C
c   and now writes it to gwrite
c
          write (fu84,*) irread
          if (irread.eq.4) write(fu84,*)ip,icharge(ip),imultipl(ip)
          write (fu84,*) v
          do icount=1,3*natom
          write (fu84,*) x(icount)
          enddo
          do icount=1,3*natom
          write (fu84,*) dx(icount)
          enddo
c   
c   and sets irout=0 to indicate that everything was ok.
c       
          irout=0
          return
        else
c
C    Now, if it wasn't a geometry optimization, compare the geometries. 
C    If the one to be used in the calculations is different from the one in 
C    gwrite, it returns (it doesn't allow rotations or translations in order
C    to avoid problems along the reaction path). If IOP<5 (i.e., before
C    enetring the reaction path calculations) the comparison between
C    geometries is not performed. It is assumed that for reactants,
C    products and saddle point everything is exactly the same than
C    in the original calculation
C    The same for wells (IOP>6)
C
          if (iop.eq.6) then                                            1128JC97
          do icount=1,natom*3
            diff=x(icount)-georead(icount)
            if (abs(diff).gt.fallout) then  
               if (icontrw.eq.0) then
                  icontrw=1
                  rewind fu83
                  goto 1
               else
                  goto 1
               endif
            endif
          enddo
          endif
C          
C    If both geometries are the same, it reads the stored information
C    and writes to gwrite
C
          v=vread
          if (irout.gt.1) then
            do icount=1,natom*3
               dx(icount)=dxread(icount)
            enddo
          endif
          if (irout.eq.3) then
            do icount=1,natom*3
             do jcount=1,natom*3
               fmat(icount,jcount)=fmread(icount,jcount)
             enddo
            enddo
          endif
        write (fu84,*) irread
        write (fu84,*) vread
        do icount=1,3*natom
        write (fu84,*) georead(icount)
        enddo
        do icount=1,3*natom
        write (fu84,*) dxread(icount)
        enddo
        if (irout.eq.3) then
        do icount=1,3*natom
        do jcount=1,3*natom
         write (fu84,*) fmread(icount,jcount)
        enddo
        enddo
        endif
c   
c   and sets irout=0 to indicate that everything was ok.
c       
          irout=0
      endif
      return
c
c     if irout>4 (i.e., we only want writing):
c
      else
          irout=irout-4
          write (fu84,*) irout
          if(irout.eq.4) write(fu84,*)iop,icharge(iop),imultipl(iop)
          write (fu84,*) v
          do icount=1,3*natom
               write (fu84,*) x(icount)
          enddo
          do icount=1,3*natom
               write (fu84,*) dx(icount)
          enddo
          if (irout.eq.3) then
             do icount=1,3*natom
                do jcount=1,3*natom
                   write (fu84,*) fmat(icount,jcount)
                enddo
             enddo
           endif
      endif
999   if (icontrw.eq.0) then
         icontrw=1
         rewind fu83
         goto 1
      else
         rewind fu83                                                    0505JZ08
         RETURN
      endif
      RETURN
      END subroutine grest
c
c***********************************************************************
      subroutine prep
c***********************************************************************
c
c     This routine is the driver for all preparation routines needed 
c     to set up a gaussrate calculation.
c
c     This routine is called after all POLYRATE input has been read in
c     and before the table of input is written to fu6.  It is also
c     before any calculations are done.  This routine is used
c     to call all electronic structure preparation routines. 
c
      use perconparam
      use common_inc
      use cm; use keyword_interface
      use kintcm
      use gaussrate_param; use gaussrate_comm
      use gr_eshscm
      implicit real(8) (a-h,o-z)
c
c     common /eshscm/ fmat(n3tm,n3tm)
      real(8) :: xcm(3),nrs(8)
      call allocate_grmem
      call sad_mem
      if(.not.allocated(fmat)) allocate(fmat(n3tm,n3tm))
      if(.not.allocated(numci)) then 
         allocate(numci(1)); numci(1)=0
      endif
c
c     Check if it is the first call to this subroutine
c
      if (numci(1).eq.0) then
c
c     if it is, set up some variables and start the Gaussian 
c     optimization procedure (if selected) and the hessian 
c     calculations for reactant and products.
c
      ereac=0.D0
      ereac1=0.D0
      ereac2=0.D0
      eprod=0.D0
      eprod1=0.D0
      eprod2=0.D0
      nrs(:) = nratom(:)
      nrs(5) = natom
      vjdep(:) = 0.D0
      icharge(:) = 0
      imultipl(:) = 1
      ioptgr(:) = 0
      call setup
c
c     Set isavect. If it is set to zero, the computer time is reduced as
c     much as possible, but it can be dangerous for optimizing geometries,
c     calculating anharmonicities, and second (or higher) derivatives by means 
c     of Polyrate methods. If it is set to one, some calculations may be 
c     repeated, but it will be much safer for the above mentioned
c     calculations. If isavect=2, the calculation is completely safe,
c     no check for saving time is done and derivatives will be calculated
c     correctly.
c     isavect=1 has been removed, since it saves little computer time
c     and there is a certain risk of failure
c
      isavect=0
      do 3 inesp=1,8                                                    1128JC97
       if (inesp.eq.6) goto 3                                           1128JC97
       if (ispot.eq.0) isavect=3
       if (igpot.eq.0) then
        if (istatu(inesp).eq.0) then                                    0729YC97
               if (nrs(inesp).gt.1) then
                     isavect=2
                     goto 1
               endif
        endif
       endif
        if (iharm(inesp).ne.1) then
            do inm=1,nxmod(inesp)
             if (vharmr(inm,inesp).ne.'harmonic '.and.
     *           vharmr(inm,inesp).ne.'hinrot   ') then
                       isavect=3
             endif
            enddo
        endif
3     continue
c
c
1     numci(1)=1 
      if (isavect.eq.2) then
             if (isup.eq.1) return
             if (isup.eq.0) then
                    write (FU6,1001)
                    stop 'prep 1'
             endif
      endif
      if (isavect.eq.3) then
             if (isup.eq.1) isavect=2
             if (isup.eq.0) then
                    write (FU6,1001)
                    stop 'prep 2'
             endif
      endif
C
C     Calculations for reactants
C
      if (istatu(1).ne.6.or.istatu(2).ne.6) then                        0725YC97
      do iopc = 1,2                                          
        if (irepr(iopc).ne.0) then
            if (istatu(iopc).ne.6) then                                 0729YC97
            if (nratom(iopc).gt.1) then
              if (ioptgr(iopc).eq.1.and.istatu(iopc).lt.2) then         0725YC97
                 call geoin(iopc,ircm)
                 if (ircm.eq.1) call rchmul(iopc)
              else
                 imultipl(iopc)=nedeg(3*iopc-2)                         0725YC97
                 do i=1,nratom(iopc)                                    0729YC97
                     inum = iatsv(i,iopc)                               0729YC97
                     do j = 2,0,-1                                      0729YC97
                        x(inum*3-j) = xr(inum*3-j,1)                    0729YC97
                     enddo                                              0729YC97
                 enddo                                                  0729YC97
              endif
              if (isavect.ne.2) then
                call secin(iopc)
              else
                itemp=iop
                iop=iopc
                call ehook(0,iproc)
                iop=itemp
              endif
            else
              if (ioptgr(iopc).eq.1) then
                 call enatin(iopc,ircm)
                 if (ircm.eq.1) call rchmul(iopc)
              else
                 itemp=iop
                 iop=-iopc
                 call ehook(0,iproc)
                 iop=itemp
              endif
            endif
            else                                                        0729YC97
              V = steng(iopc)                                           0729YC97
              do i=1,nratom(iopc)                                       0729YC97
              inum = iatsv(i,iopc)                                      0729YC97
                 do j = 2,0,-1                                          0729YC97
                    x(inum*3-j) = xr(inum*3-j,1)                        0729YC97
                 enddo                                                  0729YC97
              enddo                                                     0729YC97
            endif                                                       0729YC97
c         vjdep(iopc)=v
        ereac=ereac+V
        if (isup.eq.1) then
            if (iopc.eq.1) then                                         1201JC97
               ereac1=V
            else
               ereac2=V
            endif
        endif
        endif
      enddo
C
C     saves the result for the following calculations on these points
C
        if (isup.eq.0) then
            itemp=iop
            iop=5
            call ehook(0,iproc)
            iop=itemp
            ereac=v
        endif
       enresv=ereac
       if (isup.eq.0) ezer0=ereac
       do i=1,n3tm
       geresv(i)=x(i)
       xr(i,1)=x(i)
       x(i)=0.D0
       fsresv(i)=DX(i)
       DX(i)=0.D0
        do j=1,n3tm
        hsresv(i,j)=fmat(i,j)
        fmat(i,j)=0.d0
        enddo
       enddo
      endif                                                             0725YC97
C
C     Calculations for products
C
      if (istatu(3).ne.6.or.istatu(4).ne.6) then                        0725YC97
      do iopc = 3,4
        if (irepr(iopc).ne.0) then
            if (istatu(iopc).ne.6) then                                 0729YC97
            if (nratom(iopc).gt.1) then
              if (ioptgr(iopc).eq.1.and.istatu(iopc).lt.2) then         0725YC97
                 call geoin(iopc,ircm)
                 if (ircm.eq.1) call rchmul(iopc)
              else
                 imultipl(iopc)=nedeg(3*iopc-2)                         0725YC97
                 do i=1,nratom(iopc)                                    0729YC97
                     inum = iatsv(i,iopc)                               0729YC97
                     do j = 2,0,-1                                      0729YC97
                        x(inum*3-j) = xr(inum*3-j,3)                    0729YC97
                     enddo                                              0729YC97
                 enddo                                                  0729YC97
              endif
                 if (isavect.ne.2) then
                 call secin(iopc)
                 endif
            else
              if (ioptgr(iopc).eq.1) then
                 call enatin(iopc,ircm)
                 if (ircm.eq.1) call rchmul(iopc)
              else
                 itemp=iop
                 iop=-iopc
                 call ehook(0,iproc)    
                 iop=itemp
              endif
            endif
            else                                                        0729YC97
              V = steng(iopc)                                           0729YC97
              do i=1,nratom(iopc)                                       0729YC97
              inum = iatsv(i,iopc)                                      0729YC97
                 do j = 2,0,-1                                          0729YC97
                    x(inum*3-j) = xr(inum*3-j,3)                        0729YC97
                 enddo                                                  0729YC97
              enddo                                                     0729YC97
            endif                                                       0729YC97
c         vjdep(iopc)=v
      eprod=eprod+v
        if (isup.eq.1) then
            if (iopc.eq.3) then                                         1201JC97
               eprod1=V
             write(6,*) 'EPROD1 =',eprod1
            else
               eprod2=V
            endif
        endif
        endif
      enddo
        if (isup.eq.0) then
            itemp=iop
            iop=5
            call ehook(0,iproc)
            iop=itemp
            eprod=v
        endif
C
C     saves the result for the following calculations on these points
C
       enprsv=eprod
       do i=1,n3tm
       geprsv(i)=x(i)
       xr(i,3)=x(i)
       x(i)=0.D0
       fsprsv(i)=DX(i)
       dx(i)=0.D0
        do j=1,n3tm
        hsprsv(i,j)=fmat(i,j)
        fmat(i,j)=0.D0
        enddo
       enddo
      endif                                                             0725YC97
C
C     Calculations for wells                                            1128JC97
C
      if (istatu(7).ne.6.or.istatu(8).ne.6) then                        1128JC97
       do iopc = 7,8                                                    1128JC97
        if (irepr(iopc).ne.0) then                                      1128JC97
            if (istatu(iopc).ne.6) then                                 1128JC97
              if (ioptgr(iopc).eq.1.and.istatu(iopc).lt.2) then         1128JC97
                 call geoin(iopc,ircm)                                  1128JC97
                 if (ircm.eq.1) call rchmul(iopc)                       1128JC97
              else                                                      1128JC97
                 imultipl(iopc)=nedeg(3*iopc-2)                         1128JC97
                 do i=1,nratom(iopc)                                    1128JC97
                     inum = iatsv(i,iopc)                               1128JC97
                     do j = 2,0,-1                                      1128JC97
                        x(inum*3-j) = xr(inum*3-j,iopc)                 1128JC97
                     enddo                                              1128JC97
                 enddo                                                  1128JC97
              endif                                                     1128JC97
                 if (isavect.ne.2) then                                 1128JC97
                    call secin(iopc)                                    1128JC97
                 endif                                                  1128JC97
            else                                                        1128JC97
              V = steng(iopc)                                           1128JC97
              do i=1,nratom(iopc)                                       1128JC97
              inum = iatsv(i,iopc)                                      1128JC97
                 do j = 2,0,-1                                          1128JC97
                    x(inum*3-j) = xr(inum*3-j,iopc)                     1128JC97
                 enddo                                                  1128JC97
              enddo                                                     1128JC97
            endif                                                       1128JC97
        endif                                                           1128JC97
C
C     saves the result for the following calculations on these points
C
        if (iopc.eq.7) then                                             1128JC97
           enwrsv=V                                                     1128JC97
           do i=1,n3tm                                                  1128JC97
              gewrsv(i)=x(i)                                            1128JC97
              xr(i,7)=x(i)                                              1128JC97
              x(i)=0.D0                                                 1128JC97
              fswrsv(i)=DX(i)                                           1128JC97
              dx(i)=0.D0                                                1128JC97
              do j=1,n3tm                                               1128JC97
                hswrsv(i,j)=fmat(i,j)                                   1128JC97
                fmat(i,j)=0.D0                                          1128JC97
              enddo                                                     1128JC97
           enddo                                                        1128JC97
        else                                                            1128JC97
           enwpsv=V                                                     1128JC97
           do i=1,n3tm                                                  1128JC97
              gewpsv(i)=x(i)                                            1128JC97
c             xr(i,7)=x(i)                                              1128JC97
              xr(i,8)=x(i)                                              0309JZ10
              x(i)=0.D0                                                 1128JC97
              fswpsv(i)=DX(i)                                           1128JC97
              dx(i)=0.D0                                                1128JC97
              do j=1,n3tm                                               1128JC97
                hswpsv(i,j)=fmat(i,j)                                   1128JC97
                fmat(i,j)=0.D0                                          1128JC97
              enddo                                                     1128JC97
           enddo                                                        1128JC97
        endif                                                           1128JC97
       enddo                                                            1128JC97
      endif                                                             1128JC97
C
C     Calculations for starting geometry
C
      iopc=5
      if (irepr(5).eq.1) then                                           0811YC97
         if (ioptgr(iopc).eq.1.and.istatu(5).lt.2) then                 0725YC97
            call geoin(iopc,ircm)
            if (ircm.eq.1) call rchmul(iopc)
         else
            imultipl(5)=nedeg(13)                                       0725YC97
                 do inatnop=1,n3tm
                 x(inatnop)=xr(inatnop,5)
                 enddo
         endif
c   
c   For avoiding problems related to reorientation, we will translate 
c   origin of coordinates to the center of masses of the optimized 
c   saddle point (energy, gradients and hessians will not change, but
c   it will be safer for the calculation of the reaction path).
c
           totm=0.D0
           do i=1,3
              xcm(i)=0.D0
           enddo
           do k=1,natom
             ams=amass(k*3)**2.0D0
             totm=totm+ams
             do l=1,3
               xcm(l)=xcm(l)+x((k-1)*3+l)*ams
             enddo
           enddo
           do k=1,natom
               do l=1,3
                  x((k-1)*3+l)=x((k-1)*3+l)-xcm(l)/totm
               enddo
           enddo
c
c   Now, it calculates the gradients and hessian for the saddle point
c
           if (istatu(5).lt.4) then                                     0725YC97
                 if (isavect.ne.2) then
                 call secin(iopc)
                 endif
           endif                                                        0725YC97
                 estar=v
C
C     Saves the result for the following calculations on these points
C
       enstsv=estar
       do i=1,n3tm
       gestsv(i)=x(i)
       xr(i,5)=x(i)
       x(i)=0.D0
       fsstsv(i)=DX(i)
       dx(i)=0.D0
        do j=1,n3tm
        hsstsv(i,j)=fmat(i,j)
        fmat(i,j)=0.D0
        enddo
       enddo
      endif
      endif                                                             0811YC97
c
      return
1001  format (3x,'Gaussrate error: SUPERMOL keyword is only supported ',
     *'along with the',/,3x,'following options:',//,7x,'OHOOK option ',
     *'in OPTTS and OPTMIN',/,7x,'HHOOK option in HESSCAL',/,7x,'HAR',
     *'MONIC or HINROT approximation for vibrational modes')
      end subroutine prep                                        
c
c***********************************************************************
      subroutine prepj(jtype)
c***********************************************************************
c
c     This routine is the species dependent
c     driver for all preparation routines needed to set up an electronic 
c     structure calculation.  Any initializations that are dependent
c     on whether the reactants, products, saddle point, or generalized
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
c             7                 Reactant well
c             8                 Product well
c
c     This routine is called after each respective call to stvarj.
c
c
c     use perconparam
c     use common_inc
c     use cm;
      use gaussrate_comm
      use gr_eshscm
      implicit real(8) (a-h,o-z)
c     common /eshscm/ fmat(n3tm,n3tm)
      if (jtype.ne.6) then
         call stlgsj(jtype)
         call stvarj(jtype)
      end if
      iop=jtype
      return
      end subroutine prepj
c
c***********************************************************************
       SUBROUTINE rchmul(jtype)
c***********************************************************************
c
c     This subroutine reads the charge and multiplicity from a
c     Gaussian formated Checkpoint file
c
      use perconparam, only : fu6
c     use common_inc
C     use cm;
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
      CHARACTER*80 STRREAD
      CHARACTER*80 CASE
      CHARACTER*6  CHARID
      CHARACTER*12 MULTID
C
C     It starts looking for the charge by means of the title
C     'Charge'
C
      OPEN (UNIT=FU85,STATUS='UNKNOWN',FILE='Test.FChk')
C
      CHARID='charge'
1015  READ (FU85,1041,END=1005) STRREAD
      DO 120 ILINPOS=1,3
          STRREAD=CASE(STRREAD)
          IF (STRREAD(ILINPOS:ILINPOS+5).ne.charid) THEN
                    GOTO 120
             ELSE
                    is=50
10                  IF (STRREAD(is:is).EQ. ' ') then
                      is=is+1
                      goto 10
                    ENDIF
                    icharge(jtype)=icint(strread(is:80))
                    GOTO 6025
          ENDIF
120   CONTINUE
      GOTO 1015
1005  WRITE (FU6,*) 'Error reading Gaussian 09/03/98/94 checkpoint file'   0701TA03
C     CALL MPI_Abort(MPI_COMM_WORLD,MPI_ERRORCODE,IERR)
      STOP
c
c     Now, it will look for the multiplicity, by means
c     of the title 'Multiplicity'
c
6025  MULTID='multiplicity'
6015  READ (FU85,1041,END=1005) STRREAD
      DO 6120 ILINPOS=1,3
          STRREAD=CASE(STRREAD)
          IF (STRREAD(ILINPOS:ILINPOS+11).ne.multid) THEN
                    GOTO 6120
             ELSE
                    is=50
20                  IF (STRREAD(is:is).EQ. ' ') then
                      is=is+1
                      goto 20
                    ENDIF
                    imultipl(jtype)=icint(strread(is:80))
                    GOTO 1025
          ENDIF
6120  CONTINUE
      GOTO 6015
1025  close (FU85)
      return
1041  format (a80)
      end subroutine rchmul
c
c***********************************************************************
       subroutine rcodgr(icount,slinrd)
c***********************************************************************
c
c     Subroutine to read in the input for the routes 
c
      use perconparam, only : fu6
      use gaussrate_param
      implicit real(8) (a-h,o-z)
      character*80 slinrd(100)
      character * 80 string, case, tempchar
      character * 3 qend
      external case
c
      icount=0
c
c   Read the line
c
1     read (fu70,'(a80)') string
      iblnk = 1
c
c   Check if it is the 'end' line
c
      do while (iblnk.eq.1)
         i=1
         do while (iblnk.eq.1.and.i.le.80)
            if (string(i:i).ne.' ') iblnk=0
            i=i+1
         enddo
      enddo
      tempchar=case(string)
      qend=tempchar(i-1:i+1)
c 
c   If it is not the end line, add the readed line to the array containing 
c   the input. Otherwise, finish reading
c
      if (qend.ne.'end') then
         slinrd(icount+1)=string
         icount=icount+1
         if (icount.eq.100) then
              write (fu6,*) 'Input too long. Maximum: 100 lines'
              stop
         endif
         goto 1
      else
         return
      endif
      return
      end subroutine rcodgr
c
c***********************************************************************
      SUBROUTINE redinf(i,fmat,ierr)
c***********************************************************************
C
C   This subroutine reads the information about reactants, products and
C   saddle point and gives that information to polyrate, avoiding the
C   recalculation of properties that were previously calculated.
C   That information was stored in the matrices that go through the
C   "savcom" common.
C   The information on wells has also been added (JC, 11/28/97)
C
C   The subroutine checks that the geometry is the same that the one it
C   has stored; otherwise, ierr=1 and Polyrate will calculate the
C   information elsewhere.
C
      use perconparam
      use common_inc
      use kintcm, only : isup
      use gaussrate_param; use gaussrate_comm
      implicit real(8) (a-h,o-z)
      real(8) ::  fmat(n3tm,n3tm),storcm(3),ancm(3)
      ierr=0
      alimg=1.0D-12
      storcm(:)=0.D0
      ancm(:)=0.D0
      totm=0.D0
C
C     First, check the input geometry against the stored one, measuring
C     the distance between the atoms and the center of mass in both
C     cases (this is only valid for stationary points, since along the
C     reaction coordinate, global translation or rotation will cause
C     problems). If isavect=1, then the check is made for the global
C     positions instead of the positions with respect to the center
C     of masses. If isavect=2, no check is done. This is the safer option,
C     to be used when calculating anharmonicities or numerical second
C     derivatives
C
      if (isavect.eq.2) then
               ierr=1
               return
      endif
      if (isavect.eq.1) then
          if (i.ne.5) then                                              1128JC97
            do j=1,natom
              do k=1,natom
                if (j.eq.iatsv(k,i)) then
                    do l=1,3
                      if (i.lt.3) then
                         dif = x((j-1)*3+l)-geresv((j-1)*3+l)
                      else
                         dif = x((j-1)*3+l)-geprsv((j-1)*3+l)
                      endif
                      if (abs(dif).gt.alimg) then
                         ierr=1
                         return
                      endif
                    enddo
                 endif
              enddo
            enddo
          else
            do j=1,natom
                    do l=1,3
                         dif = x((j-1)*3+l)-gestsv((j-1)*3+l)
                      if (abs(dif).gt.alimg) then
                         ierr=1
                         return
                      endif
                    enddo
            enddo
          endif
       endif
      if (i.ne.5) then                                                  1128JC97
          do 1 j=1,natom
           do 2 k=1,natom
             if (j.eq.iatsv(k,i)) then
                 ams=amass(j*3)**2.0D0
                 totm=totm+ams
                do l=1,3
c
c   Calculate the center of mass of the current position (ancm) and of
c   the stored positions (storcm).
c
                 ancm(l)=ancm(l)+x((j-1)*3+l)*ams
                 if (i.lt.3) then
                  storcm(l)=storcm(l)+geresv((j-1)*3+l)*ams
                 else if (i.le.4) then                                  1128JC97
                  storcm(l)=storcm(l)+geprsv((j-1)*3+l)*ams
                 else if (i.eq.7) then                                  1128JC97
                  storcm(l)=storcm(l)+gewrsv((j-1)*3+l)*ams             1128JC97
                 else if (i.eq.8) then                                  1128JC97
                  storcm(l)=storcm(l)+gewpsv((j-1)*3+l)*ams             1128JC97
                 endif
                enddo
             endif
2          continue
1         continue
          do 3 j=1,natom
           do 4 k=1,natom
             if (j.eq.iatsv(k,i)) then
                dif=0.D0
                disnx=0.D0
                dissx=0.D0
                do l=1,3
c
c   Calculate the distance between the atoms and the center of mass
c   in both cases and compare them
c
                  disnx=disnx+(x((j-1)*3+l)-ancm(l)/totm)**2.0D0
                  if (i.lt.3) then
                   dissx=dissx+(geresv((j-1)*3+l)-storcm(l)/totm)**2.0D0
                  else if (i.le.4) then                                 1128JC97
                   dissx=dissx+(geprsv((j-1)*3+l)-storcm(l)/totm)**2.0D0
                  else if (i.eq.7) then                                 1128JC97
                   dissx=dissx+(gewrsv((j-1)*3+l)-storcm(l)/totm)**2.0D01128JC97
                  else if (i.eq.8) then                                 1128JC97
                   dissx=dissx+(gewpsv((j-1)*3+l)-storcm(l)/totm)**2.0D01128JC97
                  endif
                enddo
                  dif=sqrt(disnx)-sqrt(dissx)
                    if (abs(dif).gt.alimg) then
                        ierr=1
                        return
                    endif
             endif
4          continue
3         continue
      endif
C
      if (i.eq.5) then                                                  1128JC97
C
C   We have to do the same for the saddle point, although a bit simpler, 
C   since all the atoms are involved
C
          do 5 k=1,natom
            ams=amass(k*3)**2.0D0
            totm=totm+ams
            do l=1,3
c
c   Calculate the center of mass of the current position (ancm) and of
c   the stored positions (storcm).
c
               ancm(l)=ancm(l)+x((k-1)*3+l)*ams
               storcm(l)=storcm(l)+gestsv((k-1)*3+l)*ams
            enddo
5        continue
                dif=0.D0
                disnx=0.D0
                dissx=0.D0
          do 6 k=1,natom
            do l=1,3
c
c   Calculate the distance between the atoms and the center of mass
c   in both cases and compare them
c
                 disnx=disnx+(x((k-1)*3+l)-ancm(l)/totm)**2.0D0
                 dissx=dissx+(gestsv((k-1)*3+l)-storcm(l)/totm)**2.0D0
            enddo
                 dif=sqrt(disnx)-sqrt(dissx)
                 if (abs(dif).gt.alimg) then
                        ierr=1
                        return
                 endif
6         continue
      endif
C
C   If the program is still in this subroutine, it is because the 
C   comparison of geometries indicated that we are at the stored
C   point. Therefore, now it has to read the saved information 
C
      if (i.le.2) then
            v=enresv
            if (isup.eq.1) then
                if (i.eq.1) v=ereac1
                if (i.eq.2) v=ereac2
            endif
            do k=1,n3tm
            x(k)=geresv(k)
            dx(k)=fsresv(k)
               do l=1,n3tm
                   fmat(k,l)=hsresv(k,l)
               enddo
            enddo
      else if (i.le.4) then
            v=enprsv
            if (isup.eq.1) then
                if (i.eq.3) v=eprod1
                if (i.eq.4) v=eprod2
            endif
            do k=1,n3tm
            x(k)=geprsv(k)
            dx(k)=fsprsv(k)
               do l=1,n3tm
                 fmat(k,l)=hsprsv(k,l)
               enddo
            enddo
      else if (i.eq.5) then                                             1128JC97
            v=enstsv
            do k=1,n3tm
            x(k)=gestsv(k)
            dx(k)=fsstsv(k)
               do l=1,n3tm
                 fmat(k,l)=hsstsv(k,l)
               enddo
            enddo
      else if (i.eq.7) then                                             1128JC97
            v=enwrsv                                                    1128JC97
            do k=1,n3tm                                                 1128JC97
            x(k)=gewrsv(k)                                              1128JC97
            dx(k)=fswrsv(k)                                             1128JC97
               do l=1,n3tm                                              1128JC97
                 fmat(k,l)=hswrsv(k,l)                                  1128JC97
               enddo                                                    1128JC97
            enddo                                                       1128JC97
      else if (i.eq.8) then                                             1128JC97
            v=enwpsv                                                    1128JC97
            do k=1,n3tm                                                 1128JC97
            x(k)=gewpsv(k)                                              1128JC97
            dx(k)=fswpsv(k)                                             1128JC97
               do l=1,n3tm                                              1128JC97
                 fmat(k,l)=hswpsv(k,l)                                  1128JC97
               enddo                                                    1128JC97
            enddo                                                       1128JC97
      else                                                              1128JC97
            write (fu6,*) 'Something strange happened in REDINF. It',   1128JC97
     *                    ' entered with jtype=', i                     1128JC97
      endif
      return
      end subroutine redinf
c
c***********************************************************************
      subroutine renergr(string,istrt,jtype)
c***********************************************************************
c
c     Subroutine to read in the input for the route for the
c     energy calculations
c
c     use perconparam
c     use common_inc
c     use cm;
      use gaussrate_comm
      implicit double precision (a-h,o-z)
      character*80 slinrd(100)
      character*80 string
c     character * 80 string, case
c     character * 3 qend                                                0701TA03
c     Variable qend was not used so it was taken out in Gaussrate 9.1.  0701TA03
      external case
c
       call rcodgr(icount,slinrd)
       if (jtype.ne.9) then                                             1128JC97
          inlene(jtype)=icount
          do i=1,icount
             enerot(jtype,i)=slinrd(i)
          enddo
       else
          do j=1,8                                                      1128JC97
          inlene(j)=icount
            do i=1,icount
               enerot(j,i)=slinrd(i)
            enddo
          enddo
       endif
c
      return
      end subroutine renergr
c
c***********************************************************************
       subroutine rfirstgr(string,istrt,jtype)
c***********************************************************************
c
c     Subroutine to read in the input for the route for the
c     first derivative calculations
c
      use gaussrate_comm
      implicit real(8) (a-h,o-z)
      character*80 slinrd(100)
      character*80 string
c
      call rcodgr(icount,slinrd)
      if (jtype.ne.9) then                                              1128JC97
          inlfrs(jtype)=icount
          do i=1,icount
                 firrot(jtype,i)=slinrd(i)
          enddo
      else
         do j=1,8                                                       1128JC97
         inlfrs(j)=icount                    
           do i=1,icount
              firrot(j,i)=slinrd(i)
           enddo
         enddo
      endif
 
      return
      end subroutine rfirstgr
c
c***********************************************************************
      subroutine rgenbas(string,istrt,jtype)
c***********************************************************************
c
c     Subroutine to read in a general basis set
c
      use gaussrate_comm, only : inlbas,basblk
      implicit real(8) (a-h,o-z)
      character*80 slinrd(100)
      character*80 string
c
       call rcodgr(icount,slinrd)
          inlbas=icount
            do i=1,icount
               basblk(i)=slinrd(i)
            enddo
c
      return
      end subroutine rgenbas
c
c***********************************************************************
       subroutine rgengr(string,iend,istrt)
c***********************************************************************
c
c     Subroutine to read in the input in the general section. By
c     now, it only includes the restart option
c
      use perconparam, only : fu6
      use gaussrate_comm
      implicit real(8) (a-h,o-z)
      character * 80 string
c
c     Set the defaults:
c
c     GRRESTART: ON
      irest=1
c
c     RSTTOL: 1.0D-8                                                     0825JC00
      fallout=1.0D-8                                                    0825JC00
c
c     Read in first keyword in this section
c
      call rlingr(string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c GRRESTART
         if (string(istrt:istrt+4).eq.'grres') then
            irest = 1
c NOGRRESTART
         else if (string(istrt:istrt+6).eq.'nogrres') then
            irest = 0
c RSTTOL                                                                 0825JC00
         else if (string(istrt:istrt+5).eq.'rsttol') then                0825JC00
            call rword(string,istrt,ierr)                                0825JC00
            if(ierr.ne.1) then                                           0825JC00
               fallout = cfloat(string(istrt:80))                        0825JC00
               write(fu6,*)'WARNING: RSTTOL changed to', fallout         0825JC00
            else                                                         0825JC00
               write(fu6,*)'ERROR reading keyword RSTTOL in esp.fu70'    0825JC00
            stop                                                         0825JC00
            end if                                                       0825JC00
         else
            write(fu6,1200) string(istrt:80)
            stop
         end if
         call rlingr(string,istrt,isect,iend)
      end do
c
1200  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' grgeneral section',/A80)
c
      return
      end subroutine rgengr
c
c***********************************************************************
      SUBROUTINE rinpgr
c***********************************************************************
C
      use perconparam, only : fu6
      use cm, only : ivrc
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
      character*80 string                                               0701TA03
      inlcer=0
      inlbas=0
      do i=1,8                                                          1128JC97
       inlscr(i)=1
       inlfrs(i)=1
       inlene(i)=1
       secrot(i,1) = '#hf/sto-3g units=au fchk nosymm freq=noraman'
       firrot(i,1) = '#hf/sto-3g units=au fchk nosymm force'
       enerot(i,1) = '#hf/sto-3g units=au fchk nosymm'
      enddo
c
C     This subroutine reads the input of the Gaussrate options. It
C     starts reading if it is a Grate restart run. If it is, open
c     the reading file and the writing file.
c
      call rlingr(string,istrt,isect,iend)
c
C     Those error check lines are equivalents to those when reading fu5
c
      if (iend.eq.1) then
         write(fu6,1000)
1000     format(1x,'error:  input file was empty')
         stop
      end if
c
      if (isect.ne.1) then
         write(fu6,1100)
1100     format(1x,'error:  first non-comment, non-blank line must be'
     *             ,' a section header that begins with a *')
         stop
      end if
c
c     Now, it reads the sections in grate input
c
      do while (iend.eq.0)
         j = istrt + 1
         if (string(j:j+5).eq.'grgene') then
               call rgengr(string,iend,istrt)
         else if (string(j:j+7).eq.'grreact1') then
               call rstatgr(string,iend,istrt,1)
         else if (string(j:j+7).eq.'grreact2') then
               call rstatgr(string,iend,istrt,2)
         else if (string(j:j+6).eq.'grprod1') then
               call rstatgr(string,iend,istrt,3)
         else if (string(j:j+6).eq.'grprod2') then
               call rstatgr(string,iend,istrt,4)
         else if (string(j:j+6).eq.'grstart') then
               call rstatgr(string,iend,istrt,5)
         else if (string(j:j+5).eq.'grpath') then
               call rstatgr(string,iend,istrt,6)
         else if (string(j:j+6).eq.'grwellr') then                      1128JC97
               call rstatgr(string,iend,istrt,7)                        1128JC97
         else if (string(j:j+6).eq.'grwellp') then                      1128JC97
               call rstatgr(string,iend,istrt,8)                        1128JC97
         else if (string(j:j+7).eq.'grcommon') then
               call rstatgr(string,iend,istrt,9)                        1128JC97
         else
               write(fu6,1200) string(j-1:80)
               stop
         end if
      end do
c
1200  format(3x,'Error:  the following is not a valid section name',
     *         /A80)
c
      if (irest.eq.1) then
        if(ivrc.eq.1) then
         OPEN (UNIT=FU83,FILE='../esp.fu83')                            0818JZ08
        else                                                            0818JZ08
         OPEN (UNIT=FU83,FILE='./esp.fu83')
        endif                                                           0818JZ08
         OPEN (UNIT=FU84,STATUS='NEW',FILE='esp.fu84')
         write(fu6,2001)
      endif
c
2001     format(/,5x,'Gaussrate restart calculation',/)
C
      RETURN
      END subroutine rinpgr
c
c***********************************************************************
      subroutine rlincer(string,istrt,jtype)
c***********************************************************************
c
c     subroutine to read in the link0 options
c
      use gaussrate_comm
      implicit real(8) (a-h,o-z)
      character*80 slinrd(100)
      character * 80 string
c
       call rcodgr(icount,slinrd)
          inlcer=icount
            do i=1,icount
               cerblk(i)=slinrd(i)
            enddo
c
      return
      end subroutine rlincer
c
c***********************************************************************
      SUBROUTINE rlingr(string,istrt,isect,iend)
c***********************************************************************
C
c     This subroutine is equivalent to rline in interface.f, but a bit
c     modified for reading from unit fu70
c
c      istrt:   location in 80 character string that is the first non-blank
c      isect:   a 1 is returned if the first non-blank character is a *
c               signalling the start of a section
c      iend:    a 1 is returned if the end of file 5 is encountered
c      string:  80 character string that is the contents of the current line
c
      use gaussrate_param, only : fu70
      implicit real(8) (a-h,o-z)
      character*80 string,case
      external case
c
1000  format(A80)
c
c       Initialize variables
c
      isect = 0
      iend = 0
      iblnk = 1
c
c     Read in next line in file 5 - find first non-blank character and
c     determine if it is a comment (# sign signals a comment).  Once a
c     comment is found the rest of the line is skipped (by setting the
c     counter to the last character [i=80])
c
      do while (iblnk.eq.1)
         read(fu70,1000,end=9999) string
         i = 1
         do while (iblnk.eq.1.and.i.le.80)
            if (string(i:i).ne.' ') then
               if (string(i:i).ne.'#') then
                   iblnk = 0
                else
                   i = 80
                end if
            end if
            i = i+1
         end do
       end do
c
c      Set location of first non-blank character
       istrt = i-1
c
c      Check if it is a section header (first character a *)
c
       if (string(istrt:istrt).eq.'*') isect=1
c
c      Strip of any trailing comments on the line
c
       do 1 j=istrt,80
1        if (string(j:j).eq.'#') string(j:80) = ' '
c
c      Change to lower case
c
       string = case(string)
       return
9999   iend=1
       return
       end subroutine rlingr
c
c***********************************************************************
       subroutine rsecgr(string,istrt,jtype)
c***********************************************************************
c
c     Subroutine to read in the input for the route for the
c     second derivative calculations
c
      use gaussrate_comm
      implicit real(8) (a-h,o-z)
      character*80 slinrd(100)
      character*80 string
c
       call rcodgr(icount,slinrd)
       if (jtype.ne.9) then                                             1128JC97
          inlscr(jtype)=icount
          do i=1,icount
             secrot(jtype,i)=slinrd(i)
          enddo
       else
          do j=1,8                                                      1128JC97
          inlscr(j)=icount
            do i=1,icount
               secrot(j,i)=slinrd(i)
            enddo
          enddo
       endif
c
      return
      end subroutine rsecgr
c
c***********************************************************************
       subroutine rstatgr(string,iend,istrt,jtype)
c***********************************************************************
c
c     Subroutine to read in the input for all the stationary points
c
      use perconparam, only : fu6
      use gaussrate_comm
      implicit real(8) (a-h,o-z)
      character*80 string
c
c     Jtype = 1  first reactant
c     Jtype = 2  second reactant
c     Jtype = 3  first product
c     Jtype = 4  second product
c     Jtype = 5  saddle point or starting stationary point
c     Jtype = 6  all the points along the path
c     Jtype = 7  reactant well
c     Jtype = 8  product well
c     Jtype = 9  all the stationary points and points along the path
c
c     read in first keyword in this section
c
      call rlingr(string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
C GROPT
         if (string(istrt:istrt+4).eq.'gropt') then
            write (fu6,1150)                                            1128JC97
            stop                                                        1128JC97
C NOGROPT
         else if (string(istrt:istrt+6).eq.'nogropt') then
            write (fu6,1150)                                            1128JC97
            stop                                                        1128JC97
C CHARGE
         else if (string(istrt:istrt+5).eq.'charge') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
              if (jtype.ne.9.and.jtype.ne.6) then                       1128JC97
               icharge(jtype) = icint(string(istrt:80))
              else 
               write(fu6,1050)
               stop
              endif
            else
               write(fu6,1100)
               stop
            end if
C MULTIPLICITY
         else if (string(istrt:istrt+5).eq.'multip') then
            call rword(string,istrt,ierr)
            if(ierr.ne.1) then
              if (jtype.ne.9.and.jtype.ne.6) then                       1128JC97
                 imultipl(jtype) = icint(string(istrt:80))
              else 
                 write(fu6,1050)
                 stop
              endif
            else
               write(fu6,1100)
               stop
            end if
C GRLINK0 (only allowed in GRCOMMON)
         else if (string(istrt:istrt+6).eq.'grlink0') then
            if (jtype.eq.9) then                                        1128JC97
              call rlincer(string,istrt,jtype)
            else
              write(fu6,1250)
              stop
            endif
C GRBASIS (only allowed in GRCOMMON)
         else if (string(istrt:istrt+6).eq.'grbasis') then
            if (jtype.eq.9) then                                        1128JC97
              call rgenbas(string,istrt,jtype)
            else
              write(fu6,1250)
              stop
            endif
C GRENER
         else if (string(istrt:istrt+5).eq.'grener') then
            call renergr(string,istrt,jtype)
C GRFIRST
         else if (string(istrt:istrt+6).eq.'grfirst') then
            call rfirstgr(string,istrt,jtype)
C GRSEC
         else if (string(istrt:istrt+4).eq.'grsec') then
            call rsecgr(string,istrt,jtype)
         else
            write(fu6,1200) string(istrt:80)
            stop
         end if
         call rlingr(string,istrt,isect,iend)
      end do
c
1050  format(3x,'Error:  MULTIPLICITY and CHARGE can not be in',
     *       ' COMMON or PATH sections',/)
1100  format(3x,'Error reading CHARGE or MULTIPLICITY',/)
1150  format(3x,'Error:  GROPT and NOGROPT are not longer valid',
     *       ' keywords',/)
1200  format(3x,'Error:  the following is not a valid keyword in',
     *       ' this section',/A80)
1250  format(3x,'Error:  grlink0 and grbasis are only allowerd in',
     *       ' the common section',/)
c
      return
      end subroutine rstatgr
c
c***********************************************************************
      subroutine hhook(xii,fmat,sstp,nend,mw,icent,iproc)
c***********************************************************************
c
c     This routine optionally accepts the mass-weighted cartesian 
c     coordinates and transforms them to cartesian coordinates (mw=1).
c     The cartesian coordinates are used when computing the energy and
c     second derivatives of the energy with respect to each cartesian 
c     coordinate.  If the mass-weighted option was chosen (mw=1), the 
c     coordinates and first derivatives are transformed back to mass-
c     weighted at the end.  
c
      use perconparam
      use common_inc
      use cm; use kintcm, only : ispot
      use gaussrate_param; use gaussrate_comm
      implicit real(8) (a-h,o-z)
      dimension xii(n3tm), fmat(n3tm,n3tm), tmpfmat(n3tm,n3tm)
      dimension bup(n3tm)
c     
c
c     Optionally convert the mass-weighted coordinates to cartesian
c
      if (mw.eq.1) call trans(2,n3,amass,x,dx)
c
c     If the hessian is calculated using polyrate:
c
      if (ispot.eq.0) then
          if (lgs(25).eq.0) then
             if (icent.eq.0) then
                 do 75 i=1,n3
 75                 bup(i) = x(i)
                 call seccen (bup,fmat,mw,sstp)
                 goto 1
             endif
             if (icent.eq.1) then
                 call seccep (mw,sstp,nend)
                 goto 1
             endif
          else if (lgs(25).eq.1) then
                 call deriv2(nend)
                 goto 1
          else if (lgs(25).eq.2) then
                 call derv24(nend)
                 goto 1
          endif
       endif
c
c     If it is calculated using Gaussian:
c     If it is a Grate restart, read the energy, 1st and 2nd der.
c     
      irout=3
      if (irest.eq.1) call grest(irout,fmat)
      if (irout.eq.0) return
c
c     If the calculation corresponds to a stationary point, check if it has
c     already been calculated and stored in redinf.
c
        if (iop.eq.6) then                                              1128JC97
            i=5
            nacat=natom
        else 
            if (iop.eq.5) then
                nacat=natom
            else
                nacat=nratom(iop)
            endif
            i=iop
            call redinf(iop,fmat,ierr)
            if (ierr.eq.0) goto 149 
        endif
               ich=icharge(i)
               imul=imultipl(i)
               do 119 indfmex=1,n3tm
                do 129 indfmey=1,n3tm
                 fmat(indfmex,indfmey)=0.D0
129             continue
119            continue
               call escsec(natoms,ich,imul,label,
     *iatsv,v,x,dx,fmat,iop,nacat)
c
c     Reorder the stored hessian (if we are dealing with reactants or
c     products) and convert into mass-weighted
c
149       if (iop.ne.5.and.iop.ne.6) then                               1128JC97
            do l=1,n3tm
            do m=1,n3tm
                tmpfmat(l,m)=fmat(l,m)
                fmat(l,m)=0.0D0
            enddo
            enddo
            do l=1,nratom(iop)
            do m=1,nratom(iop)
                  do k=1,3
                  do n=1,3
                     innx=3*(iatsv(l,iop)-1)+k
                     inny=3*(iatsv(m,iop)-1)+n
                     inox=3*(l-1)+k
                     inoy=3*(m-1)+n
                          fmat(inox,inoy)=tmpfmat(innx,inny)/
     *                     amass(ind(inox))/amass(ind(inoy))
                  enddo
                  enddo
              enddo
              enddo
           else
c
c   If it is the saddle point, we only have to convert the Hessian
c   into mass-weighted
c
c     For non-equilibrium solvation, need to add an extra harmonic term
c
            nend = natom*3
c
            if (LBATH.AND.LFOPT) THEN
              nend = nend + 1
              F1 = PI*PI*REDM/(16.0d0*FRICT)
              F2 = F1/FRICT
              DO I = 1,NEND-1
                 FMAT(NEND,I) = -F2*DIFFU(I)
                 FMAT(I,NEND) = FMAT(NEND,I)
              ENDDO
              FMAT(NEND,NEND)=F2
              DO I = 1,NEND-1
               DO J = 1,I
                 FMAT(I,J) = FMAT(I,J)+F2*DIFFU(I)*DIFFU(J)
                 FMAT(J,I) = FMAT(I,J)
               ENDDO
              ENDDO
            endif
c              do l=1,natom*3
c              do m=1,natom*3
              do l=1,nend
              do m=1,nend
                fmat(l,m)=fmat(l,m)/amass(ind(l))/amass(ind(m))
              enddo
              enddo
           endif
c
c     Put the appropriate zero of energy
c
      if (lezer0) v = v-ezer0   
c
c   calculate the derivative and energy, should be in Cartesian coord
c
      IF (LBATH) THEN
        CALL EFFBATH(2)
      ENDIF
c
 
c
c     Transfer back to mass-weighted coordinates
1     if (mw.eq.1) call trans(1,n3,amass,x,dx)
c
c     If it is a Grate restart, write the energy, 1st and 2nd der.
c     If ispot=0, the calculations of first derivatives or/and energies were 
c     already stored and we don't need to save the information about fmat.
c
      if (ispot.ne.0) then
        if (irest.eq.1) then
          irout=7
          call grest(irout,fmat)
        endif
      endif
c
      return
      end subroutine hhook                                          
c
c***********************************************************************
      SUBROUTINE setup
c***********************************************************************
C
C   This subroutine writes a title for the polyrate long output and
C   call the subroutine for reading Gaussrate input
C
      use perconparam
      use kintcm
      use cm, only : ivrc
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
C
c     call allocate_grmem
      if(ivrc.eq.1) then                                                0818JZ08
        open (unit=fu70,status='old',file='../esp.fu70')                0818JZ08
      else                                                              0818JZ08
        open (unit=fu70,status='old',file='./esp.fu70') 
      endif                                                             0818JZ08
      call rinpgr
      close (fu70)
C
         WRITE (FU6,100) 
C
100   FORMAT (/,2X,T5,'GAUSSIAN will carry out the electronic'
     *        /,2X,T5,'structure calculations needed for obtaining the'
     *        /,2X,T5,'required data for the POLYRATE calculation',/)
C
C     Check compatibility of options and set gropt
C
      if (isup.eq.0) then
         write (fu6,200)
         stop 'setup 1'
      endif
c
c     OHOOK optimization:
c
      if (igpot.eq.1) then                                              1203JC97
         do 10 i=1,8                                                    1203JC97
           if (i.eq.6) goto 10                                          1203JC97
           if (irepr(i).eq.0) goto 10                                   1203JC97
c
c     check all the systems to be optimized have initgeo=hooks
c     and set ioptgr=1
c
           if (istatu(i).eq.0) then                                     1203JC97
              if (initg(i).eq.1) then                                   1203JC97
                  ioptgr(i) = 1                                         1203JC97
              else                                                      1203JC97
                  write (fu6,300) i                                     1203JC97
                  stop 'setup 2'                                        1203JC97
              endif                                                     1203JC97
            else                                                        1203JC97
c
c     check all the systems not to be optimized have initgeo=geom 
c     and set ioptgr=0
c
              if (initg(i).eq.0) then                                   1203JC97
                  ioptgr(i) = 0                                         1203JC97
              else                                                      1203JC97
                  write (fu6,400) i                                     1203JC97
                  stop 'setup 3'                                        1203JC97
              endif                                                     1203JC97
            endif                                                       1203JC97
10       continue                                                       1203JC97
c
c     BFGS or NH optimizations
c
      else                                                              1203JC97
         do 20 i=1,8                                                    1203JC97
           if (i.eq.6) goto 20                                          1203JC97
           if (irepr(i).eq.0) goto 20                                   1203JC97
c
c     check all the systems have initgeo=geom 
c
           if (initg(i).eq.0) then                                      1203JC97
                  ioptgr(i) = 0                                         1203JC97
           else                                                         1203JC97
                  write (fu6,500) i                                     1203JC97
                  stop 'setup 3'                                        1203JC97
           endif                                                        1203JC97
20       continue                                                       1203JC97
      endif                                                             1203JC97
C
c
200   FORMAT (/,2X,T5,'Error: Gaussrate requires the no-supermolecule'
     *        /,2X,T5,'mode. The program will stop',/)
300   FORMAT (/,2x,T5,'Error: OHOOK has been selected for the '         1203JC97
     *        /,2X,T5,'optimization, and the species ', I3,' is going'  1203JC97
     *        /,2X,T5,'to be optimized, so the geometry should be'      1203JC97
     *        /,2X,T5,'given in HOOKS (INITGEO=HOOKS).'/)               1203JC97   
400   FORMAT (/,2x,T5,'Error: OHOOK has been selected for the '         1203JC97   
     *        /,2X,T5,'optimization, and the species ', I3,' is not '   1203JC97   
     *        /,2X,T5,'going to be optimized, so the geometry should '  1203JC97     
     *        /,2X,T5,'be given in FU5 (INITGEO=GEOM).'/)               1203JC97 
500   FORMAT (/,2x,T5,'Error: BFGS or NR has been selected for the '    1203JC97   
     *        /,2X,T5,'optimization, so for the species ', I3,' the'    1203JC97   
     *       /,2X,T5,'geometry should be given in FU5 (INITGEO=GEOM).'/)1203JC97 
C
      RETURN
      END subroutine setup
c
c***********************************************************************
      SUBROUTINE store(V,COORD,DX,FMAT,INDOP)
c***********************************************************************
C
C    This subroutine checks if the first derivative calculation is going 
C    to be carried out on a point where the second derivative has been
C    calculated right before this calcularion. If it is, it reads the
C    first derivative instead of calculating it, so that computing time
C    is saved.
C
      use perconparam, only : n3tm
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
      real(8) :: coord(n3tm),dx(n3tm),fmat(n3tm,n3tm)
      difall=1.0D-10
      if (indop.eq.1) then
         storev=v
         do instr=1,n3tm
            storecoord(instr)=coord(instr)
            storedx(instr)=dx(instr)
              do jnstr=1,n3tm
               storefmat(instr,jnstr)=fmat(instr,jnstr)
              enddo
         enddo
      else
        do incont=1,n3tm
             dif=COORD(incont)-STORECOORD(incont)
             if (abs(dif).gt.difall) goto 101
        enddo
        v=storev
        do incont=1,n3tm
           dx(incont)=storedx(incont)
        enddo
         indop=1
      endif
101   return
      end subroutine store
c

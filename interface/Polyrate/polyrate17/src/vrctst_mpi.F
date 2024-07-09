C*****************************************************************
C This is main subroutine used to perform VRC-TST calculations
C The references of the theoretical details in this implementation are:
C   1) Georgievskii Y. and Klippenstein S.J. JCP 2003, 118, 5442.
C   2) Georgievskii Y. and Klippenstein S.J. JPC A 2003, 107, 9776.
C It uses SPRNG random number generator for parallel Monte Carlo 
C integration.
C
C Called by
C
C   main
C
C By Jingjing Zheng Oct. 2007; MPI implementation on May 2008
c
C****************************************************************
C
      subroutine vrctst(irank,isize)
      use perconparam
      use common_inc
      use rate_const
      use cm
      use keyword_interface
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'
C     SPRNG Related
#define SIMPLE_SPRNG
#define USE_MPI 1
#include "sprng_f.h"
      SPRNG_POINTER stream
c     parameter (maxe=16,maxj=60,jstep=5)
c     parameter (maxe=16,maxj=60)
c     parameter (maxs=90)
      dimension qcvt(maxs,40),qcvt2(maxs,40),qt(40),qt2(40),ip(2),
     *          err1(maxs,40),sabs(maxs),
     *          rcvt(40),rmute(40),rmutej(40),deltg(maxs,40),
     *          eqconst(40),rcvtr(40),rmuter(40),rmutejr(40)
      dimension etot(40,maxe),dnej(maxs,40,maxe,0:maxj),
     *          dne(maxs,40,maxe),dnee(maxs,40,maxe)
      dimension dnee2(maxs,40,maxe),dnej2(maxs,40,maxe,0:maxj),
     *          err2(maxs,40,maxe),err3(maxs,40,maxe,0:maxj)
      dimension dnejt(40,maxe,0:maxj),dnet(40,maxe),dneet(40,maxe)
     *          ,dnee2t(40,maxe),dnej2t(40,maxe,0:maxj)
      dimension endpts(2),bmuvt(maxe),tmuvt(maxe),wmuvt(maxe)
      dimension xpt(3),gpt(3)
      dimension tmp_mpi_qcvt(maxs,40),tmp_mpi_qcvt2(maxs,40),
     *          tmp_mpi_dnee(maxs,40,maxe),tmp_mpi_dne(maxs,40,maxe),
     *          tmp_mpi_dnej(maxs,40,maxe,0:maxj)
      dimension tmp_mpi_dnee2(maxs,40,maxe),
     *          tmp_mpi_dnej2(maxs,40,maxe,0:maxj)
      dimension iminc(40)
c
c add Feb 1 2010
      dimension nes(10),nessub(10),mpi_nes(10)

c     dimension tmp_mpi_1(2000000),tmp_mpi_2(2000000)
c     integer checkPT,seed
c
      dimension QVSV(N6TM)
      character*14 restart
      character*3  AFLAG
      logical      debug
      debug=.false.
  
c     write the header for VRC-TST
c
      write(fu6,1000) 
      write(fu15,2000)
      write(fu15,2100)
      write(fu15,2200)

      do i = 1, 10
       nes(i) = 0
      enddo
c
      jmax=jmax/jstep
      if(jmax.gt.maxj) then
        write(fu6,*) 'Angular momentum quantum number J exceeds the ',
     *              'limit of array with the current JSTEP. '
        write(fu6,*) 'Increas MAXJ value in param.inc, or increase ',
     *               'JSTEP in the input file'
        stop 
      endif
c     initialize some array and variables for VRC-TST
c
      call initvrc
C
C     Check 2*svl should larger than the distance 
C     between any pivotpoint pairs those are within the same reactant 
C
c     svl2=4.0d0*svl*svl
c     do jtype = 1, 2
c      if(npvt(jtype).ge.2) then
c       do n1=1,npvt(jtype)
c       do n2=n1+1,npvt(jtype) 
c         dist=0.0d0
c         do k = 2, 0, -1
c          dist=dist+(xpp(n1*3-k,jtype)-xpp(n2*3-k,jtype))**2
c         enddo
c         if (dist.gt.svl2) then
c          svl2=dist
c          svl = 0.5d0*dsqrt(dist)
c          write(fu6,'(a24,f5.2)') 'change lower s bound to ', svl
c         endif
c       enddo
c       enddo
c      endif
c     enddo
C
c    make sure s is larger than the sum of two reactive atom van der Walls
C    radii plus the corresponding pivot point to the atom distance
c
c     call checks  
c
c     Check the number of dividing surfaces 
c
      ndvs=(svu-svl)/svs
      if(ndvs.gt.maxs) then
        write(fu6,*) 'Number of dividing surfaces exceeds the maximum ',
     *               'value ',maxs,', change MAXS value in param.inc'
        stop
      endif
c
      nc   = maxs*40
      nme  = nc*maxe
      nmej = nme*(maxj+1)
c
c
C     Initialize the array ENDPTS which is used in the gaussq subroutine
C
      endpts(1) = 0.D0                                                 
      endpts(2) = 0.D0         
      nemvt=nniter
c 
c     calculate reduced mass of two reactants
c
      r1mas=0.0d0
      do 100 I= 1, nratom(1)
        r1mas = r1mas + svmas(iatsv(i,1))
 100  continue
      r2mas=0.0d0
      do 110 I= 1, nratom(2)
        r2mas = r2mas + svmas(iatsv(i,2))
 110  continue
      redmf = (r1mas*r2mas)/(r1mas+r2mas)
c
c     Loop over the different lengthes of reaction coordinate vector s
C
      is=1
      sabs(is)=svl
c
      write(fu6,*) 'Running at the stage: '
      do 300 while (sabs(is).le.svu)
      write(fu6,'(a3,f5.2)') 's= ',sabs(is)/gufac6
C
C     We use Modified Lagged Fibonacci Generator in SPRNG as default
C     which corrpesonds to igtype = 4
c
      iseed = 985456376
      stream = init_sprng(igtype,iseed,SPRNG_DEFAULT)
C     write(fu6,*) ' The following generator is used in SPRNG:'
      junk = print_sprng()
c
c     Determine nodes and weigths for Gauss-Laguerre quadrature
c
      call gaussq (6,nniter,0.0d0,0.0d0,0,ENDPTS,BMUVT,TMUVT,WMUVT) 
c
      ne0=0
      dele=-0.2d0*1.593601d-3
c     el=-3.0d0/ckcal
      do 180 itemp=1,ntemp
         bkt=bk*temp(itemp)
         qcvt(is,itemp)=0.0d0
         qcvt2(is,itemp)=0.0d0
c        perr1(is,itemp)=0.0d0
c        perr2(is,itemp)=0.0d0
         do i=1,nemvt+ne0
           dne(is,itemp,i)=0.0d0
           dnee(is,itemp,i)=0.0d0
           dnee2(is,itemp,i)=0.0d0
            if(i.le.nemvt) then
            etot(itemp,i)=bkt*tmuvt(i)
            else
            etot(itemp,i)=dble(i-nemvt)*dele
            endif
         do j=0,jmax
           dnej(is,itemp,i,j)=0.0d0
           dnej2(is,itemp,i,j)=0.0d0
         enddo
         enddo
 180  continue
      ntotp1=npvt(1)
      ntotp2=npvt(2)
      npp=0
      nacp=0
c
c       MPI Load Balance Checking
c       MPI_t1 = MPI_Wtime()
c       gtype = 2
c       checkPT = 0
c       print *, 'Check Pointing indicator ',checkPT
c       print *,' -1 for a continue run '
c       print *,'  0 for a run whose randum numbers can be reproduced'
c       print *,'  1 for a new run starting from any random numbers'
c       if(irank.lt.10) then
c       write(restart, '("../restart.00",I1)') irank
c       write(*, *) ' ID=', irank, ' Restart is named as ',restart
c       else if (irank.ge.10.and.irank.lt.100) then
c       write(restart, '("../restart.0",I2)') irank
c       write(*, *) ' ID=', irank, ' Restart is named as ',restart
c       else if (irank.ge.100.and.irank.lt.1000) then
c       write(restart, '("../restart.",I3)') irank
c       write(*, *) ' ID=', irank, ' Restart is named as ',restart
c       end if

c      if(checkPT.eq.0) then
c      For a run to repeat the previous random numbers
c      iseed = 985456376
c      stream = init_sprng(igtype,iseed,SPRNG_DEFAULT)

c      else if (checkPT.eq.1) then
c      for a new run
c      iseed = make_sprng_seed() !produce a new seed each time program is run
c      stream = init_sprng(igtype,iseed,SPRNG_DEFAULT)
c      else if (checkPT.eq.-1) then
C      for a continue run
c      open(130, file = restart, status = 'old', form = 'unformatted')
c         read(130) size
c         read(130) buffer1
c         stream = unpack_sprng(buffer1)
c         close(130)
c      endif
c
c      loop over all the facets in the MDS
c
       do 200 i1=1, ntotp1
       do 210 i2=1, ntotp2
          ip(1)=i1
          ip(2)=i2
c         if(svrc(i1,i2).le.0.0) goto 210
          npp=npp+1
          call vrcnej(ip,sabs(is),nemvt+ne0,qt,qt2,etot,dnet,dnejt,
     *    dnej2t,dneet,dnee2t,na,irank,isize,stream,nessub)          
          nacp=nacp+na
          do 250 itemp=1, ntemp
             qcvt(is,itemp)=qcvt(is,itemp)+qt(itemp)
             qcvt2(is,itemp)=qcvt2(is,itemp)+qt2(itemp)
c            perr1(is,itemp)=perr1(is,itemp)+sig1(itemp)
c            perr2(is,itemp)=perr2(is,itemp)+sig2(itemp)
             do 260 i=1,nemvt+ne0
c              write(fu6,2500) etot(i)*ckcal,dnet(i)
               dne(is,itemp,i)=dne(is,itemp,i)+dnet(itemp,i)
               dnee(is,itemp,i)=dnee(is,itemp,i)+dneet(itemp,i)
               dnee2(is,itemp,i)=dnee2(is,itemp,i)+dnee2t(itemp,i)
             do 262 j=0,jmax
               dnej(is,itemp,i,j)=dnej(is,itemp,i,j)+dnejt(itemp,i,j)
               dnej2(is,itemp,i,j)=dnej2(is,itemp,i,j)+dnej2t(itemp,i,j)
c              write(fu6,2540) etot(itemp,i)*ckcal,j*jstep,dnejt(itemp,i,j)
 262         continue
 260         continue
 250      continue
c
        do ne = 1, 10
          nes(ne)=nes(ne) + nessub(ne)
        enddo
        
 210    continue
 200    continue
c
        sabs(is+1) = sabs(is) + svs     
        is=is+1
C
 300   continue
       nsstep=is-1
  
c      write(fu6,*) 'Coming to MPI Reduction part now'
c
C      Synchronization  
       CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
c
c      check the time used for MPI_Reduce
       time1 = MPI_Wtime()    
c     
c      Collect the results from each processor and sum them to processor 0
c
       CALL MPI_Reduce(qcvt,tmp_mpi_qcvt,nc,MPI_DOUBLE_PRECISION, 
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)
c
       Call MPI_Barrier(MPI_COMM_WORLD, ierr)
       CALL MPI_Reduce(qcvt2,tmp_mpi_qcvt2,nc,MPI_DOUBLE_PRECISION,
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)
c
c      Call MPI_Barrier(MPI_COMM_WORLD, ierr)
c      CALL MPI_Reduce(perr2,tmp_mpi_perr2,nc,MPI_DOUBLE_PRECISION,
c    * MPI_SUM, 0, MPI_COMM_WORLD, ierr)
c
       Call MPI_Barrier(MPI_COMM_WORLD, ierr)
       CALL MPI_Reduce(dnee,tmp_mpi_dnee,nme,MPI_DOUBLE_PRECISION,
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)
c
       Call MPI_Barrier(MPI_COMM_WORLD, ierr)
       CALL MPI_Reduce(dnee2,tmp_mpi_dnee2,nme,MPI_DOUBLE_PRECISION,
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)
c
       Call MPI_Barrier(MPI_COMM_WORLD, ierr)
       CALL MPI_Reduce(dne,tmp_mpi_dne,nme,MPI_DOUBLE_PRECISION,
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)
c
       Call MPI_Barrier(MPI_COMM_WORLD, ierr)
       CALL MPI_Reduce(dnej,tmp_mpi_dnej,nmej,MPI_DOUBLE_PRECISION,
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)

       Call MPI_Barrier(MPI_COMM_WORLD, ierr)
       CALL MPI_Reduce(dnej2,tmp_mpi_dnej2,nmej,MPI_DOUBLE_PRECISION,
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)

       CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
c
       CALL MPI_Reduce(nes,mpi_nes,10,MPI_INTEGER,
     * MPI_SUM, 0, MPI_COMM_WORLD, ierr)
c
       CALL MPI_Barrier(MPI_COMM_WORLD, ierr)     
c
       dtime = MPI_Wtime() - time1
       IF (irank.eq.0) then
       write(fu6,'(a16,F10.4,a7)') 'MPI Reduce time = ',dtime,' second'
       endif
c             
c    Starting using Rank 0 to calculate final results
c
        if(irank.eq.0) then
c
c    statistics of energy distribution 
c
        do ne = 1, 10
          if (ne.eq.1) then
          write(fu6,'(a26,I8)')'No. of repulsive energies ',mpi_nes(ne)
          elseif (ne.eq.10) then
          write(fu6,'(a8,F5.2,I8)')'   V <= ',-8.d0,mpi_nes(ne)
          else
          write(fu6,'(F5.2,a8,F5.2,I8)')-1.*(ne-1),' < V <= ',-1.*(ne-2)
     *                               ,mpi_nes(ne)

          endif
        enddo
c
        nacp=nacp*isize
        write(fu6,2110) nacp
C
C    Number of available states N(s,T,E), N(s,T,E,J) 
C
        do 400 is=1,nsstep
        do itemp=1, ntemp
          qcvt(is,itemp) = tmp_mpi_qcvt(is,itemp)/dble(nacp)
c
c ............... Average percentage error of Monte Carlo Integration
c
c        perr1(is,itemp)=tmp_mpi_perr1(is,itemp)/dble(nacp)
c        perr2(is,itemp)=tmp_mpi_perr2(is,itemp)/dble(nacp)
         qcvt2(is,itemp) = tmp_mpi_qcvt2(is,itemp)/dble(nacp)
c        terr=perr1(is,itemp)**2-perr2(is,itemp)
         terr=qcvt(is,itemp)**2-qcvt2(is,itemp)
c        err(is,itemp)=1.0d2*dsqrt(abs(terr/dble(nacp)))/perr1(is,itemp)0128JZ10
c        err1(is,itemp)=1.0d2*dsqrt(abs(terr/dble(nacp)))/qcvt(is,itemp)
         err1(is,itemp)=67.45*dsqrt(abs(terr/dble(nacp)))/qcvt(is,itemp)
        end do

        do 410 itemp=1,ntemp
c          write(fu6,2430) temp(itemp)
c          write(fu6,2420)
c          qcvt(is,itemp)=qcvt(is,itemp)/dble(nacp)
        do 420 i=1,nemvt+ne0
           dne(is,itemp,i)=tmp_mpi_dne(is,itemp,i)/dble(nacp)
           dnee(is,itemp,i)=tmp_mpi_dnee(is,itemp,i)/dble(nacp)
           dnee2(is,itemp,i)=tmp_mpi_dnee2(is,itemp,i)/dble(nacp)
C
C   estimate MC errors for each N(E) 
C
           terr=dnee(is,itemp,i)**2-dnee2(is,itemp,i)
           err2(is,itemp,i)=67.45*dsqrt(abs(terr/dble(nacp)))/
     *                      dnee(is,itemp,i)
c          write(fu6,2500) etot(itemp,i)*ckcal,dne(is,itemp,i),
c    *                     dnee(is,itemp,i)
 420     continue
c       write(fu6,2520)
        do 430 i=1,nemvt+ne0
        do 440 j=0,jmax
           dnej(is,itemp,i,j)=tmp_mpi_dnej(is,itemp,i,j)/dble(nacp)
           dnej2(is,itemp,i,j)=tmp_mpi_dnej2(is,itemp,i,j)/dble(nacp)
c          dnej(is,itemp,i,j)=dnej(is,itemp,i,j)/dble(nacp)
c          write(fu6,2540)etot(itemp,i)*ckcal,j*jstep,dnej(is,itemp,i,j)
C    
C   estimate MC errors for each N(E) 
C          
           terr=dnej(is,itemp,i,j)**2-dnej2(is,itemp,i,j)
           if (dnej(is,itemp,i,j).gt.0.) then
             err3(is,itemp,i,j)=67.45*dsqrt(abs(terr/dble(nacp)))/
     *                       dnej(is,itemp,i,j)
           else
             err3(is,itemp,i,j)= 0.0
           endif
 440    continue
 430    continue
 410    continue
c
 400    continue 
c          
c    END of calculating number of available states N(s,T,E), N(s,T,E,J)
c
c    output the N(E)
c
        ncol=4
        if(mod(nemvt+ne0,ncol).eq.0) then
         nl = (nemvt+ne0)/ncol-1
        else
         nl = (nemvt+ne0)/ncol
        endif
        write(fu6,2500)
        do itemp = 1, ntemp
           write(fu6,'(/a6,F8.1,a2)') '   T =',temp(itemp),' K'
           write(fu6,2510)
           do ie = 0, nl
             k2 = nemvt+ne0-ncol*(ie+1)
             if (k2. ge. 0) then
               write(fu6,2520) (etot(itemp,ncol*ie+k)*ckcal,k=1,ncol)
               do is = 1, nsstep
                  write(fu6,2530) sabs(is)/gufac6,
     *                 (dnee(is,itemp,ncol*ie+k),
     *                  err2(is,itemp,ncol*ie+k),k=1,ncol)
               enddo
             else 
               k2 = -k2
               write(fu6,2520) (etot(itemp,ncol*ie+k)*ckcal,k=1,k2)
               do is = 1, nsstep
                  write(fu6,2530) sabs(is)/gufac6,
     *                 (dnee(is,itemp,ncol*ie+k),
     *                  err2(is,itemp,ncol*ie+k),k=1,k2) 
               enddo
             endif
           enddo
        enddo
c
c     output the N(E,J)
c
        ncol=3
        write(fu6,2550)
        do itemp = 1, ntemp
        do ie = 1, nemvt+ne0
c
           write(fu6,2560) temp(itemp),etot(itemp,ie)*ckcal
           write(fu6,2570)
           do j = 0, jmax/(ncol+1)
              k2=jmax - ncol*(j+1)
              if (k2.ge.0) then
                 write(fu6,2580) ((j*(ncol+1)+k)*jstep,k=0,ncol)
                 do is = 1, nsstep
                    write(fu6,2590) sabs(is)/gufac6,
     *                    (dnej(is,itemp,ie,j*(ncol+1)+k),
     *                     err3(is,itemp,ie,j*(ncol+1)+k),k=0,ncol)
                 enddo
              else
                 k2 = -k2
                 write(fu6,2580) ((j*(ncol+1)+k)*jstep,k=0,k2)
                 do is = 1, nsstep
                    write(fu6,2590) sabs(is)/gufac6,
     *                    (dnej(is,itemp,ie,j*(ncol+1)+k),
     *                     err3(is,itemp,ie,j*(ncol+1)+k),k=0,k2)
                 enddo
              endif
           enddo          
c         
        enddo
        enddo
c
        do 600 itemp=1, ntemp
           bkt=bk*temp(itemp)
c
c    Reactant translational, rotational, and electronic partition functions
c    Some pieces of following codes are take from subroutine rate
c    we only consider A + B -> AB case here.
c
c    Two Reactants
c ............... TRANSLATION
c
        if (icode(1).gt.0.or.icode(2).gt.0) then
           qtr = ((redmf*bkt)/tpi)**1.5D0
           qtrcc = qtr*conk0
c          if(debug) write(fu6,*) 'Qtr= ',qtrcc
        else 
           qtr = 1.0d0
           qtrcc = 1.0d0
        endif             
c
c ............... ROTATION
c
        if (icode(1).gt.0.and.icode(2).gt.0) then
           qrr=rpart(fmom(1),bkt,icode(1))*rpart(fmom(2),bkt,icode(2))
c          if(debug) write(fu6,*) 'Qrot= ', qrr
        else if(icode(1).gt.0.and.lgs(34).ne.0) then
           qrr= rpart(fmom(1),bkt,icode(1))
        else
           qrr= 1.0d0
        endif 
c
c ............... ElECTRONIC
c
        qer= epart(1,bkt)*epart(2,bkt)
c       if(debug) write(fu6,*) 'Qer= ',qer
c
        ptotr=sigmaf*(1.0d0/tpi)/(qtrcc*qrr*qer)
C
C   Calculations for equilibrium constants and reverse rate
C
      IF (ibrate.eq.1) THEN

c
C   One  PRODUCT
C   .....................  TRANSLATION
C
        IF (ICODE(3) .GT. 0 .AND. LGS(34) .NE. 0) THEN       
           QTP = ((REDMR*BKT)/(2.0D0*PI))**1.5D0             
           QTPCC = QTP*CONK0                                 
        ELSE                                                 
           QTP = 1.0D0                                       
           QTPCC = 1.0D0                                     
        ENDIF                                                
C
C   ................ ROTATION
C
        IF (ICODE(3) .GT. 0) THEN
           QRP = RPART(FMOM(3),BKT,ICODE(3))
        ELSE                                                 
           QRP = 1.0D0                                       
        ENDIF                                                
C
C   ...............  ELECTRONIC
C
        QEP = EPART(3,BKT)
C
        ptotp=sigmar*(1.0d0/tpi)/(qtpcc*qrp*qep)     
C
C    REACTANT AND PRODUCT VIBRATIONAL PFS
C
         FAC37 = DBLE(LGS(37))*LOG(10.0D0)                              6/13T89
         IOP = 1
         IBEG = 1
C*         PROD = 1.0D0                                                 6/13T89
         SUM = 0.0D0                                                    6/13T89
         AFLAG = '   '
         IF (LGS(5).GE.21) AFLAG = 'SET'                                6/30YL91
         DO 140 IY = 1, 2 
            IF (NF(IY).NE.0.AND.LGS2(15).EQ.0) THEN                     1106YL92
               IENDR = NF(IY)+IBEG-1
               J = 1
               DO 130 IX = IBEG, IENDR
c                  IF (AFLAG.EQ.'SET') LGS(5) = MODER(IY,J)             0317Yc99
                  LGS(5) = MODER(IY,J)                                  0317Yc99
                  L0 = LRP(IX)
                  IKBM = IX
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IF (IY .EQ. 1) THEN
                        IMHR = NF(1)+1-IX
                     ELSE
                        IMHR = NF(1)+NF(2)+1-IX
                     ENDIF
                     IF (IY.EQ.2) THEN
                       IXI = IX - NF(1)
                     ELSE
                       IXI = IX
                     ENDIF
                     QVSV(IX)=HRPART(0.d0,WER(IX),TORMI(IY,IXI,1),      1020BE06
     *                               BKT,IY,IXI,IMHR)                   1020BE06
                  ELSE                                                     ..
                     QVSV(IX)=VPART(WER(IX),XER(IX),BKT,DEMIN,IOP,         ..
     *                              Y00R(IX))                              ..
                  ENDIF                                                 6/30YL91
                  J = J+1
C*                  PROD = PROD*QVSV(IX)                                6/13T89
                    SUM = SUM + LOG(QVSV(IX))                           6/13T89
  130          CONTINUE
C               IF (LGS2(12) .NE. 0) THEN
C               0719WH94
C Commented by Lucas; it should be changed to the following line
C so that under HO approximation, vib P.F. of R and P shold not be
C changed
               IF ((LGS2(12).NE.0).AND.(LGS2(12).NE.1)) THEN            2017Lucas
                  IM = NF(1)+1-IWR
                  SUM = SUM - LOG(QVSV(1)*QVSV(IM))                     0719WH94
                  CALL QTQVIB(BKT,QVIBF,QVIBR)                          0719WH94
                  QVSV(1)  = QVIBF                                      0719WH94
                  QVSV(IM) = QVIBR                                      0719WH94
                  IF (IM .EQ. 1) QVSV(IM) = QVIBF                       0719WH94
                  SUM = SUM + LOG(QVIBF*QVIBR)                          0719WH94
               ENDIF
C
            ELSEIF (NF(IY).NE.0.AND.LGS2(15).NE.0) THEN                 1106YL92
               NMOD = NF(IY)                                                ..
               EGRNDT = EGRNDR(IY)                                          ..
               DUMMY = PTQVIB(NMOD,N3TM,EGRNDT,EFNDTR(IBEG),                ..
     *                        WER(IBEG),BKT)                                ..
               PROD = PROD * DUMMY                                          ..
               SUM = SUM + LOG(DUMMY)                                       ..
            ENDIF                                                       1106YL92
            IBEG = IBEG+NF(IY)
            IOP = IOP+1
  140    CONTINUE
         SUMRE = SUM                                                    0423TA02
         QVR = EXP(SUM+FAC37)                                           6/13T89
         IBEGP = IBEG
         SUM = 0.0D0                                                    6/13T89
         EMAX = DEMIN-EPRD
         IVP = 4
C        IF (LGS(6).EQ.2) IVP = 3
         IVP = 3                                                        0111JZ10
         DO 160 IY = 3, IVP
            IF (NF(IY).NE.0.AND.LGS2(15).EQ.0) THEN                     1106YL92
               IENDP = NF(IY)+IBEG-1
               J = 1
               DO 150 IX = IBEG, IENDP
                  LGS(5) = MODER(IY,J)                                  0317Yc99
                  L0 = LRP(IX)
                  IKBM = IX
                  IF (LGS(5).EQ.9) THEN                                 6/30YL91
                     IF (IY .EQ. 3) THEN                                0615WH94
                        IMHR = NF(1)+NF(2)+NF(3)+1-IX                   0615WH94
                     ELSE                                               0615WH94
                        IMHR = NF(1)+NF(2)+NF(3)+NF(4)+1-IX             0615WH94
                     ENDIF     
                     IF (IY.EQ.4) THEN
                       IXI = IX - NF(1) - NF(2) - NF(3)
                     ELSE
                       IXI = IX - NF(1) - NF(2)
                     ENDIF
                     QVSV(IX)=HRPART(0.d0,WER(IX),TORMI(IY,IXI,1),      1020BE06
     *                               BKT,IY,IXI,IMHR)                   1020BE06
                  ELSE                                                     ..
                     QVSV(IX)=VPART(WER(IX),XER(IX),BKT,EMAX,IOP,          ..
     *                              Y00R(IX))                              ..
                  ENDIF                                                 6/30YL91
                  J = J+1
C*                  PROD = PROD*QVSV(IX)                                 6/13T89
                  SUM = SUM + LOG(QVSV(IX))                              6/13T89
  150          CONTINUE
            ELSEIF (NF(IY).NE.0.AND.LGS2(15).NE.0) THEN                 1106YL92
               EGRNDT = EGRNDR(IY)                                          ..
               NMOD = NF(IY)                                                ..
               DUMMY = PTQVIB(NMOD,N3TM,EGRNDT,EFNDTR(IBEG),                ..
     *                        WER(IBEG),BKT)                                ..
               PROD = PROD * DUMMY                                          ..
               SUM = SUM + LOG(DUMMY)                                       ..
            ENDIF                                                       1106YL92
            IBEG = IBEG+NF(IY)
            IOP = IOP+1
  160    CONTINUE
         SUMPR = SUM                                                    0423TA02
         QVP = EXP(SUM+FAC37)                                            6/13T89
C
C     Equilibrium constants
C
         eqconst(itemp) = ptotr*qvp/(ptotp*qvr)*exp(-eprd/bkt)
C
C    END of calculations for equilibrim constants and reverse rates
       ENDIF
c
c
c ...............Canonical TST RATE CONSTANT
c
        imin = 1
        qmin=qcvt(1,itemp)
        smin=sabs(imin)
        do 500 is=1,nsstep-1
c          if(qcvt(is,itemp).gt.qcvt(is+1,itemp)) then
           if(qmin.gt.qcvt(is+1,itemp)) then
            qmin = qcvt(is+1,itemp)
            imin = is+1
            smin=sabs(imin)
           endif
c
c ...............Calculate Generalized TS DeltaG at CVT level 
c
           deltg(is,itemp) =LOG(bkt/(tpi*ptotr*epart(5,bkt)*
     *                      qcvt(is,itemp)))
           deltg(is,itemp) =deltg(is,itemp)*rconst*temp(itemp)
c
 500    continue
c
           deltg(is,itemp) =LOG(bkt/(tpi*ptotr*epart(5,bkt)*
     *                      qcvt(is,itemp)))
           deltg(is,itemp) =deltg(is,itemp)*rconst*temp(itemp)
c
        iminc(itemp) = imin
        if(imin.ne.1.and.imin.ne.nsstep) then
         k = imin-2
         do 510 i = 1, 3
          xpt(i) = sabs(k+i)
          gpt(i) = qcvt(k+i,itemp)
 510     continue
         if(gpt(2).lt.gpt(1).and.gpt(2).lt.gpt(3).and.
     *       gpt(2).gt.0.0d0) then
         call trept(1,xpt,gpt,smin,qmin)
         if(qmin.lt.0.d0) qmin=gpt(2)
c        write(fu6,*) xpt(1),xpt(2),xpt(3),smin
c        write(fu6,*) gpt(1),gpt(2),gpt(3),qmin
         endif
c        write(fu6,*) 'nsstep= ',nsstep,'imin= ',imin
c        write(fu6,*) 'Three Point Fit'
        endif
        rcvt(itemp)=ptotr*epart(5,bkt)*qmin*cnvrt
        rcvtr(itemp)=rcvt(itemp)/eqconst(itemp)
        scvt(itemp)=smin
C
c
c ...............Microcanonical RATE
c
        qmute=0.0d0
        qmute0=0.0d0
        do 540 ie=1,nemvt+ne0
           qmin = dnee(1,itemp,ie)
           imin=1
           do is = 1, nsstep-1
c           if(dnee(is,itemp,ie).gt.dnee(is+1,itemp,ie)) then
            if(qmin.gt.dnee(is+1,itemp,ie)) then
              qmin = dnee(is+1,itemp,ie)
              imin = is+1
            endif
           enddo    
           if(imin.ne.1.and.imin.ne.nsstep) then
             k = imin - 2
             do i=1, 3
              xpt(i)=sabs(k+i)
              gpt(i)=dnee(k+i,itemp,ie)
             enddo           
             if(gpt(2).lt.gpt(1).and.gpt(2).lt.gpt(3).and.
     *          gpt(2).gt.0.0d0) then
             call trept(1,xpt,gpt,smin,qmin)
             if(qmin.lt.0.d0) qmin=gpt(2)
c            write(fu6,*) xpt(1),xpt(2),xpt(3),smin
c            write(fu6,*) gpt(1),gpt(2),gpt(3),qmin
             endif
c            if (qmin.gt.gpt(2)) then
c               write(fu6,*) 'warning: a max is found instead of min',
c    *            'T= ',temp(itemp),'E= ',etot(xpt(2),ie)
c            endif
           elseif((imin.eq.1.or.imin.eq.nsstep).and.qmin.gt.1.d0) then
             write(fu6,2120) sabs(imin)/gufac6,temp(itemp), 
     *                       etot(itemp,ie)*ckcal
           endif
           if(ie.le.nemvt) then
           qmute=qmute+qmin*wmuvt(ie)
           else
             if(ie.eq.nemvt+1.or.ie.eq.nemvt+ne0) then
             qmute0=qmute0+qmin*exp(-etot(itemp,ie)/bkt)
             elseif(mod(ie-nemvt,2).eq.0) then
             qmute0=qmute0+2.0d0*qmin*exp(-etot(itemp,ie)/bkt)
             else
             qmute0=qmute0+4.0d0*qmin*exp(-etot(itemp,ie)/bkt)
             endif
           endif
540     continue
        qmute=qmute*bkt+qmute0*abs(dele)/3.0d0
        rmute(itemp)=ptotr*epart(5,bkt)*qmute*cnvrt
        rmuter(itemp)=rmute(itemp)/eqconst(itemp)
c
c ...............E,J-resolved Microcanonical RATE
c
        qmutej=0.0d0
        qmutej0=0.0d0
c       write(fu6,*) ' Minimized N(E,J) at E,J-resolved level, T= ', 
c    *                temp(itemp)
        do 550 ie=1, nemvt+ne0
        do 555 ij=0, jmax     
c          dnejt(itemp,ie,ij)=dnej(1,itemp,ie,ij)
           qmin=dnej(1,itemp,ie,ij)
           imin=1
           do is=1, nsstep-1
c            if(dnej(is,itemp,ie,ij).gt.dnej(is+1,itemp,ie,ij)) then
             if(qmin.gt.dnej(is+1,itemp,ie,ij)) then
c              dnejt(itemp,ie,ij)=dnej(is+1,itemp,ie,ij)
               qmin=dnej(is+1,itemp,ie,ij)
               imin = is+1
             endif
           enddo
           if(imin.ne.1.and.imin.ne.nsstep) then
              k = imin - 2
              do i=1,3
                xpt(i)=sabs(k+i)
                gpt(i)=dnej(k+i,itemp,ie,ij)
              enddo
              if(gpt(2).lt.gpt(1).and.gpt(2).lt.gpt(3).and.
     *          gpt(2).gt.0.0d0) then
              call trept(1,xpt,gpt,smin,qmin)
              if(qmin.lt.0.d0) qmin=gpt(2)
c             write(fu6,*) xpt(1),xpt(2),xpt(3),smin
c             write(fu6,*) gpt(1),gpt(2),gpt(3),qmin
              endif
              if (qmin.gt.gpt(2)) then
c               write(fu6,2130)  sabs(imin)/gufac6,temp(itemp),
c    +          etot(imin,ie)*ckcal,ij*jstep    
                qmin=dnej(imin,itemp,ie,ij)
              endif
           elseif((imin.eq.1.or.imin.eq.nsstep).and.qmin.gt.1.d0) then
             write(fu6,2130) sabs(imin)/gufac6,temp(itemp),
     *                       etot(itemp,ie)*ckcal,ij*jstep
           endif
           dnejt(itemp,ie,ij)=qmin
c          write(fu6, *) 'E= ',etot(itemp,ie),' J= ',ij,' N(E,J)= ',
c    *                   dnejt(itemp,ie,ij)
555     continue
550     continue

c       write(fu6,*) ' Minimized N(E) at E,J-resolved level, T= ', 
c    *                temp(itemp)
        do ie=1,nemvt+ne0
         dnet(itemp,ie)=0.0d0
        enddo
        do 560 ie=1,nemvt+ne0
        do 565 ij=0,jmax
           if(ij.eq.0.or.ij.eq.jmax) then
             dnet(itemp,ie)=dnet(itemp,ie)+dnejt(itemp,ie,ij)
           elseif(mod(ij,2).eq.0.and.ij.ne.jmax) then
             dnet(itemp,ie)=dnet(itemp,ie)+2.0d0*dnejt(itemp,ie,ij)
           else
             dnet(itemp,ie)=dnet(itemp,ie)+4.0d0*dnejt(itemp,ie,ij)
           endif
565     continue
           dnet(itemp,ie)=dnet(itemp,ie)*jstep/3.0d0
c          write(fu6,*) 'E= ',etot(itemp,ie)*ckcal,'N(E)= ',
c    *                  dnet(itemp,ie) 
           if(ie.le.nemvt) then
           qmutej=qmutej+dnet(itemp,ie)*wmuvt(ie)
           else
             if(ie.eq.nemvt+1.or.ie.eq.nemvt+ne0) then
             qmutej0=qmutej0+dnet(itemp,ie)*exp(-etot(itemp,ie)/bkt)
             elseif(mod(ie-nemvt,2).eq.0) then
             qmutej0=qmutej0+2.0d0*dnet(itemp,ie)*
     *               exp(-etot(itemp,ie)/bkt)
             else
             qmutej0=qmutej0+4.0d0*dnet(itemp,ie)*
     *               exp(-etot(itemp,ie)/bkt)
             endif
           endif
560     continue
        qmutej=qmutej*bkt+qmutej0*abs(dele)/3.0d0
        rmutej(itemp)=ptotr*epart(5,bkt)*qmutej*cnvrt
        rmutejr(itemp)=rmutej(itemp)/eqconst(itemp)
c
        write(fu15,2400) temp(itemp),rcvt(itemp),rmute(itemp),
     *                   rmutej(itemp)
600     continue
c
c       output the free energy along reaction coordinate
c       at canonical level
c
        write(fu6,2230)
        ncol=6
        nr = ntemp/ncol
        if (mod(ntemp,ncol).eq.0) nr = nr  - 1  
        do kt = 0,nr
           k2 = ntemp-ncol*(kt+1)
           if (k2.ge.0) then
           write(fu6,2430)(temp(ncol*kt+k1),k1=1,ncol)
           else
           k2 = -k2
           write(fu6,2430)(temp(ncol*kt+k1),k1=1,k2)
           endif
           do ks = 1, nsstep
             if (k2.ge.0) then
             write(fu6,2240)sabs(ks)/gufac6,
     *            (deltg(ks,ncol*kt+k1),k1=1,ncol)
             else 
             k2 = - k2
             write(fu6,2240)sabs(ks)/gufac6,
     *            (deltg(ks,ncol*kt+k1),k1=1,k2)
             endif
           enddo
        enddo
c
c       output the rate constants 
c
C       Forward CVT RATE
C
        write(fu6,2250)
        do itemp=1,ntemp       
        imin = iminc(itemp)
        write(fu6,2300) temp(itemp),rcvt(itemp),scvt(itemp)/gufac6,
     *                  err1(imin,itemp)
        enddo
C
C       Forward muVT and E,J-muVT RATE
C
        write(fu6,2260)
        write(fu6,2270)
        do itemp=1,ntemp
          write(fu6,2320) temp(itemp),rmute(itemp),rmutej(itemp)
        enddo
C
C      output the equilibrium constant and reverse rate
C
       write(fu6,3000)
       IF (ibrate.eq.1) THEN
        do itemp=1,ntemp
         write(fu6,3100) temp(itemp),rcvtr(itemp),rmuter(itemp),
     *                  rmutejr(itemp)
        enddo
        write(fu6,3200)
        do itemp=1,ntemp
         write(fu6,3300) temp(itemp),eqconst(itemp),1.0d0/eqconst(itemp)
        enddo
      ENDIF
C
C   The follwoing is used for debug and exploring work
C
c     write(fu6,*) 'MC error in % for integrals in CVT'
c     ncol=6
c     nr = ntemp/ncol
c     if (mod(ntemp,ncol).eq.0) nr = nr  - 1  
c       do kt = 0,nr
c          k2 = ntemp-ncol*(kt+1)
c          if (k2.ge.0) then
c          write(fu6,2430)(temp(ncol*kt+k1),k1=1,ncol)
c          else
c          k2 = -k2
c          write(fu6,2430)(temp(ncol*kt+k1),k1=1,k2)
c          endif
c          do ks = 1, nsstep
c            if (k2.ge.0) then
c            write(fu6,2240)sabs(ks)/gufac6,
c    *            (err1(ks,ncol*kt+k1),k1=1,ncol)
c            else 
c            k2 = - k2
c            write(fu6,2240)sabs(ks)/gufac6,
c    *            (err1(ks,ncol*kt+k1),k1=1,k2)
c            endif
c          enddo
c       enddo
 


      ENDIF ! IF ( IRANK.EQ.0 ) THEN
c
1000  format(//80('-'),/25X,'Starting VRC-VTST calculations',/,80('-'))
2000  format(1X,'The following are VRC-VTST calculation results.')
2100  format(/1X,'Summary of forward rate constants ',
     *      '(cm**3 molecule-1 s-1) :')
2110  format(/3X,'Total number of sampling for each dividing surface is'
     *       ,I8)
2120  format(/3X,'warning: N(E) min reach the boundary of s at: ','s=',
     *       f4.1,1X,'T=',f7.2,1X,'E=',f6.2)
2130  format(/3X,'warning: N(E,J) min reach the boundary of s at: ',
     *       's= ',f4.1,1X,'T=',f7.2,1X,'E=',f6.2,2X,'J=',I4)
2200  format(/4X,'T(K)',10X,'CVT',6X,'muVT(E)',3X,'muVT(E,J)')
2230  format(/3X,20('-'),'Free Energy using CVT (kcal/mol)',20('-'),/)
2240  format(4X,f5.2,7X,6f9.2)
2250  format(/3X,16('-'),'Canonical Rate Constants (cm**3 molecule-1 s-1
     *)',16('-'),/)
2260  format(/3X,13('-'),'Mirocanonical Rate Constants ',
     *        '(cm**3 molecule-1 s-1)',13('-'),/)
2270  format(/4X,'T(K)',5X,'muVT',10X,'E,J-muVT')
2300  format(3X,'CVT (',F7.2,') = ',1PE15.7,4X,0P,'s = ',f6.2,5X,
     *       'MC error(%) = ',f8.2)
c2320  format(3X,'muVT (',F7.2,') = ',1PE15.7)
2320  format(3X,F7.2,1PE15.7,0P,1PE15.7)
2340  format(3X,'E,J-muVT (',F7.2,') = ',1PE15.7)
2400  format(1X,f8.2,5X,1PE10.2,0P,1PE10.2,0P,1PE10.2)
2420  format(1X,'E (kcal/mol)',4X,'E,J-resolved N(E)',4X,
     *       'E-resolved N(E)')
2430  format(/5X,'s',4X,'T(K)= ',6f9.1,/)
c2500  format(2X,f8.2,6X,1PE12.4,0P,9X,1PE12.4)
c2520  format(/1X,'E (kcal/mol)',7X,'J',9X,'N(E,J)')
c2540  format(2X,f8.2,8X,I3,5X,1PE12.4)
2500  format(/80('*')/,'*',24X,'Number of available states N(E)',23X,'*'
     *       ,/80('*'))
2510  format(/4X,'s',4X,16('~'),' E (kcal/mol) ',16('~'),/)
2520  format(7X,4(F12.2,5X))
2530  format(F7.2,1PE12.4,'(',F5.1,'%)',0P,1PE12.4,'(',F5.1,'%)',0P,
     * 1PE12.4,'(',F5.1,'%)',0P,1PE12.4,'(',F5.1,'%)')
2550  format(/80('*')/,'*',23X,'Number of available states N(E,J)',22X,
     *       '*',/80('*'))
2560  format(/3X,12('*'),'Temperature =',F8.1,' K',2X,'Total Energy =',
     *       F8.2,' kcal/mol',12('*')) 
2570  format(/4X,'s',4X,15('~'),' Total angular momentum quantum number 
     *J ',15('~'),/)
2580  format(7X,4(12X,I4))
2590  format(F7.2,1PE10.2'(',F5.1,'%)',3(0P,1PE10.2,'(',F5.1,'%)'))
3000  format(/3X,24('-'),'Reverse Rate Constants (s-1)',24('-'),/,
     *       4X,'Temp(K)',6X,'CVT',14X,'muVT',12X,'E,J-muVT',/)
3100  format(3X,F7.2,1PE15.7,4X,0P,1PE15.7,0P,1PE15.7)
3200  format(/3X,11('-'),'Equilibrium Constants (cm**3/molecule or molec
     *ule/cm**3)',11('-'),/,4X,'Temp(K)',4X,'Forward',8X,'Reverse',/)
3300  format(3X,F7.2,1PE15.7,0P,1PE15.7,0P)
      return
      end
c
c
      subroutine vrcnej(ip,sabs,nemvt,qt,qt2,etot,dne,dnej,dnej2,dnee,
     * dnee2,nacp,irank,isize,stream,nessub)
      use perconparam
      use common_inc
      use rate_const
      use cm
c
c*******************************************************************
c
c subroutine to calculate number of available states of transitional modes 
c using VRC-TST algorithm at canonical, microcanonical, and E-J resolved 
c microcanonical level.
c
c*******************************************************************
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'mpif.h'
!     include 'param.inc'
!     include 'percon.inc'
!     include 'common.inc'
      logical debug,lclose
c     parameter (maxe=16,maxj=60,jstep=5)
c     parameter (maxe=16,maxj=60)
      dimension xrcm(3,2),tp(3),dpcom(2),xtemp(n3tm)
      dimension nrfd(2),ct(2),st(2),cph(2),sph(2),cps(2),sps(2),
     *          ainv(3,3),a12inv(3),svec(3),usvec(3)
      dimension dd(2,3),fpmoi(5,3),fpvec(5,3,3),dmcint(40),dmc2(40),
     *          ip(2),xpprot(n3pt,2),qt(40),qt2(40),abc(5,3)
      dimension dnej(40,maxe,0:maxj),dne(40,maxe),etot(40,maxe),
     *          dnee(40,maxe),dnee2(40,maxe),dnej2(40,maxe,0:maxj)
C  
C added Feb. 1 2010
C
      dimension nessub(10) 
C     SPRNG Related
#define SIMPLE_SPRNG
#define USE_MPI 1
#include "sprng_f.h"
      SPRNG_POINTER stream
c
      debug=.false.

      do i = 1,10
       nessub(i) = 0
      enddo
c
c     sabs=svrc(ip(1),ip(2))
c initialize array dnej(e,j)
c
      do itemp=1,ntemp
      do i=1,nemvt
         dne(itemp,i)=0.0d0
         dnee(itemp,i)=0.0d0
         dnee2(itemp,i)=0.0d0
      do j=0,jmax
         dnej(itemp,i,j)=0.0d0
         dnej2(itemp,i,j)=0.0d0
      enddo
      enddo
      enddo
C
c calculate coordinates of each fragment relative to its pivot point
c     
      do 100 j=1,2
c
         do k=2,0,-1
           xtemp(3-k)=xpp(3*ip(j)-k,j)
         enddo
         ntot=npvt(j)
         do 95 i=1,ntot
            inum=ippsv(i,j)
            do 85 k=2,0,-1
               xpp(inum*3-k,j)=xpp(inum*3-k,j) - xtemp(3-k)
85          continue
95       continue
c
        ntot=nratom(j)
        do 90 i=1,ntot
           inum=iatsv(i,j)
           do 80 k=2,0,-1
              xrp(inum*3-k)=xrp(inum*3-k) - xtemp(3-k)
80          continue
90       continue
c
100    continue
c
c calculate center of mass of two reatants in their separated status
c
      do j=1,2
        do i=1,n3tm
        xtemp(i)=xrp(i)
        enddo
        call reactcom(j,tp,xtemp)
        do ix=1,3
          xrcm(ix,j)=tp(ix)
        enddo
      enddo
c 
c calculate the length of vector connecting COM of each fragment and 
c corresponding pivot point
c
      do j=1,2
        dtmp = 0.0d0
        do ix = 2, 0, -1
          dtmp= dtmp + (xrcm(3-ix,j)-xpp(3*ip(j)-ix,j))**2
        enddo
        dpcom(j) = dsqrt(dtmp)
      enddo
c
c  nmona is the number of monoatomic fragment
c  nrfd(i) is the rotational freedom of fragments
c  trot is total orientational freedom 
c
      nmona=0
      ntrot=3
      do 120 j=1,2
       If (icode(j).eq.4) then
       nrfd(j)=3
       else if(icode(j).eq.3.or.icode(j).eq.2) then
       nrfd(j)=2
       else if(icode(j).eq.1) then
       nrfd(j)=0
       nmona=nmona+1
       else
       write(fu6,*)'The type of reactant is not correct'
       stop
       endif
       ntrot=ntrot+nrfd(j)
120    continue
c
c Initialize MC integration array
c
       do 110 itemp=1,ntemp
          dmcint(itemp)=0.0d0
          dmc2(itemp)=0.0d0
110    continue
c
       do 115 j=1,2
         if(icode(j).gt.1) call pmoi(j,0,fpmoi,fpvec,abc)
115    continue

         prodi=1.0d0
         do 140 j=1,2
c      
c        Atomic species case
c
          if(icode(j).eq.1) then
             goto 140
c
c        Linear species case 
c         
          else if (icode(j).eq.2.or.icode(j).eq.3) then
             prodi=prodi*tpi*fpmoi(j,1)
c
c        Nonlinear species case
c
          else
             do 145 irf=1,3
c            write(fu6,*) 'J, I, MOI',fpmoi(j,irf)
             prodi=prodi*dsqrt(tpi*fpmoi(j,irf))
145          continue
          endif
c       
140      continue
c
c Starting Crude Monte Carlo integration 
c NCM is total number of steps for MC integration 
c
      if(nmc.le.isize) then
        nmcrank=1
      elseif (mod(nmc,isize).eq.0) then
        nmcrank=nmc/isize
      else
        nmcrank=nmc/isize + 1
      endif
      nacp=0
c     imc=0
c     write(fu6,*) 'NMCRANK = ',nmcrank
      do 700 while (nacp.lt.nmcrank)
c
c sample the geometric paramters randomly
c obtain the randomly determined solid angles (theta,phi,psi) for
c the two fragments and for the line of two pivot points
c
        do 150 j=1,2        
        if(icode(j).eq.1) then
          ct(j)=1.0d0
          st(j)=1.0d0
          goto 150
         endif
         ct(j)=2.0d0*(sprng(stream)-0.5d0)
         st(j)=sin(dacos(ct(j)))
         if(nrfd(j).ge.2) then
            tpang=tpi*sprng(stream)
            cph(j)=dcos(tpang)
            sph(j)=dsin(tpang)
         endif
         if(nrfd(j).eq.3) then
            tpang=tpi*sprng(stream)
            cps(j)=dcos(tpang)
            sps(j)=dsin(tpang)
         endif
150     continue
C       write(fu6,*)'cos(theta1) = ',ct(1)
C       write(fu6,*)'cos(theta2) = ',ct(2)
        ct12  = 2.0d0*(sprng(stream)-0.5d0)
        st12  = sin(dacos(ct12))
        cot12 = ct12/st12
        phi12 = tpi*sprng(stream)
        cp12  = cos(phi12)
        sp12  = sin(phi12)
C       write(fu6,*)'sin(theta12) = ',st12
c
c calculate vector s and positions of fragments
c
c 1) First step is to determine pivot point 2 space coordinates
c    Here we put the pivot point 1 at origin
c    Acutually this step determines the vector of reaction coordinate s
c    
        a12inv(1) = st12*cp12
        a12inv(2) = st12*sp12
        a12inv(3) = ct12
        do i=1,3
          svec(i)=a12inv(i)*sabs
          usvec(i)=a12inv(i)
        enddo
c
c 2) determine fragment 1 and 2 space-fixed coordinates
c    and also other pivot points coordinates
c
       do 200 j=1,2
c
       if(icode(j).eq.1) then
         iam=iatsv(1,j)
         inum=ippsv(1,j)
         do 210 k=2, 0, -1
          x(3*iam-k) = svec(3-k)
          xpprot(3*inum-k,j)= svec(3-k)
210      continue
c        if(debug) write(fu6,5000) iam,(x(3*iam-k)/gufac6,k=2,0,-1)
c        if(debug) write(fu6,5020) j,inum,
c    *               (xpprot(3*inum-k,j)/gufac6,k=2,0,-1)
         goto 200
       endif
       if(icode(j).eq.2.or.icode(j).eq.3) then
       sps(j)=0.0d0
       cps(j)=1.0d0
       endif
c
       ainv(1,1)= -sps(j)*sph(j)+ct(j)*cph(j)*cps(j)
       ainv(1,2)= -cps(j)*sph(j)-ct(j)*cph(j)*sps(j)
       ainv(1,3)=   st(j)*cph(j)
       ainv(2,1)=  sps(j)*cph(j)+ct(j)*sph(j)*cps(j)
       ainv(2,2)=  cps(j)*cph(j)-ct(j)*sph(j)*sps(j)
       ainv(2,3)=   st(j)*sph(j)
       ainv(3,1)= -cps(j)* st(j)
       ainv(3,2)=  sps(j)* st(j)
       ainv(3,3)=   ct(j)
c
c fragement 1 and 2 coordinates
c
       ntotr = nratom(j)
c
        do 220 i=1,ntotr 
          iam = iatsv(i,j)
          do 230 k1=2, 0, -1
             x(3*iam-k1) = 0.0d0
          do 235 k2=2, 0, -1
             x(3*iam-k1) = x(3*iam-k1) + ainv(3-k1,3-k2)*xrp(3*iam-k2)
235       continue
230       continue
c         
          if (j.eq.2) then
            do 250 k=2, 0, -1
              x(3*iam-k)=x(3*iam-k)+svec(3-k)
250         continue 
          endif
c            if(debug) write(fu6,5000) iam,(x(3*iam-k)/gufac6,k=2,0,-1)
c5000         format(5X,I2,5X,3f12.6)
 
220     continue
c
c the other pivot point coordinates
c
        ntotp = npvt(j)
c         
         do 260 i=1,ntotp
           inum= ippsv(i,j)
           do 270 k1=2, 0, -1
             xpprot(3*inum-k1,j) = 0.0d0
           do 275 k2=2, 0, -1
             xpprot(3*inum-k1,j)=xpprot(3*inum-k1,j) + ainv(3-k1,3-k2)*
     *                           xpp(3*inum-k2,j)
275        continue
270        continue
c
           if(j.eq.2) then
             do 280 k=2, 0, -1
               xpprot(3*inum-k,j) = xpprot(3*inum-k,j) + svec(3-k)
280          continue
           endif
c          if(debug) write(fu6,5020) j,inum,
c    *               (xpprot(3*inum-k,j)/gufac6,k=2,0,-1)
c5020       format(3X,2I2,5X,3f12.6)

260      continue
 
200    continue
c
c check the distances between all pivot point pairs
c
       do 282 i1=1,npvt(1)
       do 284 i2=1,npvt(2)
         dpp = 0.0d0
         if(i1.ne.ip(1).or.i2.ne.ip(2)) then
          do k = 2, 0, -1
           dpp=dpp+(xpprot(3*i1-k,1)-xpprot(3*i2-k,2))**2 
          enddo
          dpp=dsqrt(dpp)
c         spp=svrc(i1,i2)
          spp=sabs
          if(dpp.le.spp) goto 700
         endif
284    continue
282    continue
c
c output the geometry for debugging purpose
c
      nacp=nacp+1
      if(debug) then
       write(fu6,5001) nacp
       write(fu6,5010) (svec(k)/gufac6,k=1,3)
       do j=1,2
         do i1=1,nratom(j)
          iam=iatsv(i1,j)
          write(fu6,5000) iam,(x(3*iam-k)/gufac6,k=2,0,-1)
         enddo
         do i2=1,npvt(j)
          inum=ippsv(i2,j)
          write(fu6,5020) j,inum, (xpprot(3*inum-k,j)/gufac6,k=2,0,-1)
         enddo
       enddo
      endif
c
5010  format(5X,'P2',5X,3f12.6)
5000  format(5X,I2,5X,3f12.6)
5001  format(/,'No.',I4,' Monte Carlo point',/)
5020  format(3X,2I2,5X,3f12.6)
c
c check the repulsivity of H atoms on the separated fragments
c
c      call hrepul (vrep)
c      if(debug) write(fu6,*) ' H atoms repulsivity: ', vrep
c
c 3. Potential Energy
c
c If there is very close contact between two fragments, set v=1.0 hartree
c      call repul(lclose)
c      if(lclose) then
c         v=1.0
c         goto 292
c      endif
       call ehook(0,0)
c      if(vfac.ne.1.0d0.and.vfac.ne.0.0d0) v=v*vfac
c      write(fu6,*) ' Potential Energy: ', V
c
c statistic energy distribution
c
       do ne = 2, 9
         eh = -dble(ne-2)/ckcal
         el = -dble(ne-1)/ckcal
         if (v.le.eh.and.v.gt.el) then
            nessub(ne) = nessub(ne) + 1
         endif
       enddo 
       if (v.gt.0.0) nessub(1) = nessub(1) +1
       if (v.lt.-8.d0/ckcal) nessub(10) = nessub(10) + 1
c
C 4. determine the vector d1 or d2 connecting each fragment center of mass
C    and corresponding pivot point
C
       do j=1,2
         call reactcom(j,tp,x)
         do ix=1,3
           xrcm(ix,j)=tp(ix)
         enddo
       enddo
c
       do 290 j=1,2
       do 295 k=1,3
         if(j.eq.1) then
          dd(j,k)= 0.0d0 - xrcm(k,j)    
         else
          dd(j,k)= svec(k) - xrcm(k,j)
         endif
295    continue
290    continue
c
c 5. calculate each fragment moments of inertia and the unit vectors along 
c    principal axis
       do 300 j=1,2
         if(icode(j).gt.1) call pmoi(j,0,fpmoi,fpvec,abc)
 300    continue
c
c DEBUG
c
c      if(debug) then
c       do iam=1, natom
c          write(fu6,5000) iam,(x(3*iam-k)/gufac6,k=2,0,-1)
c       enddo
c      endif
c
c 6. calculate Kinematic factor PHI
c
       phik=0.0d0
       do 400 j=1,2
         tt=0.0d0
c
c  Atomic species case
c
       if(icode(j).eq.1) then
        goto 400
c
c  Linear species case
c
       else if (icode(j).eq.2.or.icode(j).eq.3) then
         do 410 i=1,3
            tt=usvec(i)*fpvec(j,3,i)+tt
410      continue
         phik=phik+redmf*(dpcom(j)**2)*(1.0d0-tt**2)/fpmoi(j,1)
c
c  Nonlinear species case
c
       else
          tp(1)=usvec(2)*dd(j,3)-usvec(3)*dd(j,2)
          tp(2)=usvec(3)*dd(j,1)-usvec(1)*dd(j,3)
          tp(3)=usvec(1)*dd(j,2)-usvec(2)*dd(j,1)
         do 420 irf=1,3
           do 430 k=1,3
              tt=tt+tp(k)*fpvec(j,irf,k)
430        continue
           phik=phik+redmf*(tt**2)/fpmoi(j,irf)
420      continue
       endif
400    continue
       phik=dsqrt(1.0d0+phik)
       if(debug) write(fu6,5100) phik
5100   format(2X,'Kinematic factor PHI',2X,f6.3)
c
c 7) Loop over temperatures 
c
      do 450 itemp=1, ntemp
        bkt=bk*temp(itemp)
        tt=phik*exp(-1.0d0*v/bkt)
        dmcint(itemp)=dmcint(itemp)+tt
        dmc2(itemp)=dmc2(itemp)+tt**2
        if(debug) write(fu6,*) ' PHI*Exp(-V/kT) ',tt
450   continue
c
c*****************************************************
c          
c calculate N(E,J,Q) (J. Zheng March 31 2008)
c      
c*****************************************************
c      
      call pmoi(5,0,fpmoi,fpvec,abc)
      piq=1.0d0
      do 500 irf=1,3
        piq=piq*dsqrt(tpi*fpmoi(5,irf))
500   continue
c        
c loop over angular momentum J ,total energy E, and temperature
c        
      do 510 itemp=1,ntemp
      do 520 ie=1,nemvt
        te=etot(itemp,ie)-v
        if(te.gt.0.0d0) then
         tmp=phik*te**(dble(ntrot)/2.0d0-0.5d0)
         dnee(itemp,ie)=dnee(itemp,ie)+tmp
         dnee2(itemp,ie)=dnee2(itemp,ie)+tmp**2
        endif
      do 530 ij=0, jmax
        j=ij*jstep
        rj=dble(j)
c
c        if (j.eq.0) rj=0.5d0
c        call averjn(te,rj,abc,ntrot-3,dnejq,v)
c           d1 = dnejq
         call averj(te,rj,abc,ntrot-3,dnejq)
c           d2 = dnejq
c        if (d1.gt.0.0) then
c         write(fu6,'(f8.2,2X,E12.4,2X,E12.4)') (d2-d1)/d1*100.0,d1,d2
c        endif
c
c      if(j.eq.0) write(fu6,*) 'J = ',J,'  N(E,J,Q) = ',dnejq
c       if (j.eq.0) then
         dnejq=dnejq*phik*redmf*(2.0d0*rj+1.0d0)**2*(sabs**2)*2.0d0
     *       *(pi**(nmona-1))*prodi/dgamma(dble(ntrot)/2.0d0-1.0d0)/piq
c       else
c        dnejq=dnejq*phik*redmf*dble(j)**2*(sabs**2)*8.0d0*
c    *        (pi**(nmona-1))*prodi/dgamma(dble(ntrot)/2.0d0-1.0d0)/piq
c       endif
c       write(fu6,*) 'J = ',J,'  N(E,J,Q) = ',dnejq
        dnej(itemp,ie,ij)=dnej(itemp,ie,ij)+dnejq
        dnej2(itemp,ie,ij)=dnej2(itemp,ie,ij)+dnejq**2
c       dne(ie)=dne(ie)+dnejq
530   continue
520   continue
510   continue

700   continue
701   continue
c
c out of Monte Carlo Sampling
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c loop over temperature 
c
      do 800 itemp=1, ntemp
c      sig1(itemp)=dmcint(itemp)
c      sig2(itemp)=dmc2(itemp)
c
c percentage error of Monte Carlo
c
c      if(debug) write(fu6,5200) temp(itemp),dmcint(itemp),sig2(itemp)
5200   format(2X,'Monte Carlo Integration <PHI*exp(-v/bkt)>: ',f8.2,
     *        E15.6,f8.2,'%')
c
      do 730 i=1,nemvt
         tmp = 2.0d0*redmf*(sabs**2)*(pi**(nmona-2))*prodi/
     *         dgamma(dble(ntrot)/2.0d0+0.5d0)
         dnee(itemp,i)=dnee(itemp,i)*tmp
         dnee2(itemp,i)=dnee2(itemp,i)*tmp**2
 730  continue
c
c sum over J to calculate N(E). This is only used for checking purpose
c
      do 710 i=1,nemvt
      do 712 j=0,jmax
         if(j.eq.0.or.j.eq.jmax) then 
           dne(itemp,i)=dne(itemp,i)+dnej(itemp,i,j)
         elseif(mod(j,2).eq.0.and.j.ne.jmax) then
           dne(itemp,i)=dne(itemp,i)+2.0d0*dnej(itemp,i,j)
         else
           dne(itemp,i)=dne(itemp,i)+4.0d0*dnej(itemp,i,j)
         endif
 712   continue
           dne(itemp,i)=dne(itemp,i)*jstep/3.0d0
 710   continue 
c
      bkt=bk*temp(itemp)
      qt(itemp)=2.0d0*redmf*(pi**(nmona-2))*(sabs**2)*
     *             (bkt**((dble(ntrot)+1.0d0)/2.0d0))
     *             *prodi*dmcint(itemp)
      qt2(itemp)=(qt(itemp)/dmcint(itemp))**2*dmc2(itemp)
C
c     if(debug) write(fu6,*) 'Qtransitional ',qt(itemp)
 800  continue
c
      return
      end
c*********************************************************************
c
c REACTCOM
c
c********************************************************************
c
c subroutine to calculate center of mass for reactants
c
      subroutine reactcom(jtype,xcm,xdum)
      use perconparam
      use common_inc
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!     include 'param.inc'
!     include 'percon.inc'
!     include 'common.inc'
      dimension xcm(3),xdum(n3tm)
c
c jtype must be less than 5
c
      do i=1,3
       xcm(i) = 0.0d0
      enddo
c
      ntot = nratom(jtype)      
      totm = 0.0d0
      do 10 i = 1, ntot
        inum = iatsv(i,jtype)
c       totm = totm + svmas(label(inum))
        totm = totm + amass(3*iatom(i))**2
10    continue
c
c      do 50 i =1,ntot
c         inum = iatsv(i,jtype)
c         do 60 k=2, 0, -1
c           xcm(3-k)= xcm(3-k) + xdum(inum*3-k)*svmas(label(inum))
c60       continue
c50    continue
c 
c      do 80 i = 1, 3
c        xcm(i) = xcm(i)/totm
c80    continue
c
         DO 40 K = 1, 3
         SUM = 0.D00
         DO 20 I = 1, ntot 
            INUM = IATSV(I,jtype)
            SUM = SUM+XDUM(3*(INUM-1)+K)*AMASS(3*(IATOM(I)-1)+K)**2
   20    CONTINUE
         XCM(K) = SUM/TOTM
   40 CONTINUE

      return
      end
c**************************************************************************
c This is the ran1 routine from "Numerical Recipes: The Art of
c Computing", 2nd edition, by W.H. Press, S.A. Teukolsky,
c W.T. Vetterling, and B.P. Flannery.
c  It is the "minimal" random number generator of Park & Miller
c with a Bays-Durham shuffle (and added safeguards) which returns
c a uniform random deviate between 0.0 and 1.0 (exclusive of
c end point values).
c  Call with idum a negative integer to initialize; thereafter do not
c alter idum between successive calls for deviates in a sequence.
c  rnmx should approximate the largest floating value less than 1.
c**************************************************************************
c
      function ran1(idum)
      integer idum, ia,im,iq,ir,ntab,ndiv
      double precision ran1,am,eps,rnmx
      parameter(ia=16807, im=2147483647, am = 1.0/im, iq=127773,ir=2836,
     $   ntab=32, ndiv=1+(im-1)/ntab, eps=1.2d-7, rnmx=1.d0-eps)
      integer j,k,iv(ntab),iy
      save  iv,iy
      data iv/ntab*0/,iy/0/

      if (idum.le.0 .or. iy.eq.0) then   !initialize
         idum = max(-idum,1)
         do j=ntab+8, 1, -1
            k = idum/iq
            idum = ia*(idum-k*iq) - ir*k
            if (idum.lt.0) idum = idum + im
            if(j.le.ntab) iv(j) = idum
         enddo
         iy = iv(1)
      endif
      k = idum/iq                 !start here when not initializing
      idum = ia*(idum-k*iq) - ir*k
      if (idum.lt.0) idum = idum + im
      j = 1 + iy/ndiv
      iy = iv(j)
      iv(j) = idum
      ran1 = min(am*iy,rnmx)
      return
      end

C
C***********************************************************************
C  PMOI
C***********************************************************************
C
      SUBROUTINE pmoi (IOP,IMPR,FPMOI,FPVEC,ABC)
      use perconparam
      use common_inc
C     
C     This subroutine is modified base on CENTER to calculate principal
C     moment of inertia IA, IB, and IC and  their unit vector along the 
C     ith principal axis. It is used for VRC-TST calculations.
C     01/25/07 by JZ
C
C     TRANSLATES CENTER OF MASS OF SYSTEM TO SADDLE POINT OR TO
C     C. O. M. OF A REACTANT OR PRODUCT SPECIES
C     IOP=1 OR 2 FOR REACTANTS, IOP=3 OR 4 FOR PRODUCTS
C     IOP=5 FOR A SYSTEM AS A WHOLE
C     ALSO CALCULATES MOMEMT OF INERTIA OR PRODUCT OF ALL 3
C     FOR S. P. OR REACTANT OR PRODUCT
C
C     CALLED BY:
C            VRCNEJ
C     CALLS:
C            RSPDRV
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
!     include 'param.inc'
!     include 'percon.inc'
!     include 'common.inc'
C
      LOGICAL LDEBUG                                                    1106YL92
C*
      DIMENSION XCM(3),TENS(3,3,3),ISCR(3),SCR1(3),SCR2(3)              1106YL92
      DIMENSION FPMOI(5,3),FPVEC(5,3,3),ABC(5,3),ROT(3,3)
C
      DATA TENS / 5*0.0D0,-1.0D0,0.0D0,1.0D0,3*0.0D0,1.0D0,3*0.0D0,
     *            -1.0D0,3*0.0D0,-1.0D0,0.0D0,1.0D0,5*0.0D0 /
C
C
      LDEBUG = .FALSE.                                                  1106YL92
C     LPTBCR = LGS2(15)
      IF (LDEBUG) THEN
      IF(IOP.EQ.1.OR.IOP.EQ.2) THEN
      WRITE(FU6,1400) IOP
      ELSE IF (IOP.EQ.5) THEN
      WRITE(FU6,1410) 
      ENDIF
      ENDIF

      IBEG = 1
      IEND = NATOM
      IF (IOP.LT.5) IEND = NRATOM(IOP)
      TOTM = 0.D00
      DO 10 I = IBEG, IEND
         INUM = IATSV(I,IOP)
         TOTM = TOTM+AMASS(3*IATOM(I))**2
   10 CONTINUE
      DO 40 K = 1, 3
         SUM = 0.D00
         DO 20 I = IBEG, IEND
            INUM = IATSV(I,IOP)
            SUM = SUM+X(3*(INUM-1)+K)*AMASS(3*(IATOM(I)-1)+K)**2
   20    CONTINUE
         XCM(K) = SUM/TOTM
C
C         MOVE C. O. M. OF FULL SYSTEM TO C. O. M. OF SPECIES IOP
C
         DO 30 I = K, N3, 3
            X(I) = X(I)-XCM(K)
   30    CONTINUE
   40 CONTINUE
      IF (IMPR.EQ.1) WRITE (FU6,1000) (XCM(K),K=1,3)                    1125JC97
C
C     COMPUTE  MOMENT OF INERTIA IN A.U.
C
      IF (ICODE(IOP).LE.3) THEN
C
C         LINEAR MOLECULE
C         ALIGN MOLECULE WITH X-AXIS FOR MORATE
C         BUT ALONG Z-AXIS FOR POLYRATE TESTRUNS
C         IOLIN =1,2,3 MEANS X,Y,Z AXIS.
C
         SUM = 0.0D0
         JV = 1
         T = ABS(X(1))+ABS(X(2))+ABS(X(3))
         IF (T.LT.1.0D-08) JV = 4
         XX1 = X(JV)
         X2 = X(JV+1)
         X3 = X(JV+2)
         DO 50 I = IBEG, IEND
            L = 3*IATSV(I,IOP)-2
            TX = (X(L)**2+X(L+1)**2+X(L+2)**2)
            SUM = SUM+TX*AMASS(L)**2
   50    CONTINUE
         DO 54 I = 1, 3
         DO 55 J = 1, 3
         FPVEC(IOP,I,J)=0.0D0
   55    CONTINUE
   54    CONTINUE
         I1= 3*IATSV(1,IOP)-2
         I2= 3*IATSV(2,IOP)-2
         TT=DSQRT((X(I1)-X(I2))**2+(X(I1+1)-X(I2+1))**2+
     *            (X(I1+2)-X(I2+2))**2)
         FPVEC(IOP,3,1)=DABS(X(I1)-X(I2))/TT
         FPVEC(IOP,3,2)=DABS(X(I1+1)-X(I2+1))/TT
         FPVEC(IOP,3,3)=DABS(X(I1+2)-X(I2+2))/TT          
C        XMOM = SUM
C        FMOM(IOP) = REDM*SUM
         FPMOI(IOP,1)= SUM*REDM
         FPMOI(IOP,2)=FPMOI(IOP,1)
         FPMOI(IOP,3)=0.0d0
C
C Calculate rotational constants A,B,C; linear molecule has no A, and B=C
C We simply set A = 0.0
         ABC(IOP,1) = 0.0D0
         ABC(IOP,2) = 0.5D0/FPMOI(IOP,2)
         ABC(IOP,3) = ABC(IOP,2)
C
         IF (LDEBUG) WRITE (FU6,1300) (FPMOI(IOP,I), I=1,3)             0125JZ07
         IF (LDEBUG) WRITE (FU6,1310) (ABC(IOP,I)*AUTOCM, I=1,3)
         IF (LDEBUG) WRITE(FU6,219) ((FPVEC(IOP,I,J),I=1,3),J=1,3)
c
      ELSE
C
C         NON-LINEAR MOLECULE -- FIND PRINCIPAL MOMENT OF INERTIA TENSOR
C         IA, IB, and IC AS WELL AS UNIT VECTOR ALONG WITH PRINCIPAL AXIS
C
         DO 60 I = 1, 3
            DO 60 J = 1, 3
               ROT(I,J) = 0.0D0
   60    CONTINUE
         DO 70 I = IBEG, IEND
            L = 3*IATSV(I,IOP)-2
            ROT(1,1) = ROT(1,1)+(X(L+1)**2+
     *                 X(L+2)**2)*AMASS(L)**2
            ROT(1,2) = ROT(1,2)-X(L)*X(L+1)*AMASS(L)**2
            ROT(1,3) = ROT(1,3)-X(L)*X(L+2)*AMASS(L)**2
            ROT(2,2) = ROT(2,2)+(X(L)**2+
     *                 X(L+2)**2)*AMASS(L)**2
            ROT(2,3) = ROT(2,3)-X(L+1)*X(L+2)*AMASS(L)**2
            ROT(3,3) = ROT(3,3)+(X(L)**2+
     *                 X(L+1)**2)*AMASS(L)**2
   70    CONTINUE
         ROT(2,1) = ROT(1,2)
         ROT(3,1) = ROT(1,3)
         ROT(3,2) = ROT(2,3)

            DO 210 I = 1, 3                                             1106YL92
               BEROT(I) = 0.D0                                          1106YL92
               DO 210 J = 1, 3                                          1106YL92
                  PVEC(I,J) = 0.D0                                      1106YL92
210         CONTINUE                                                    1106YL92
            CALL RSPDRV(3,3,ROT,BEROT,1,PVEC,SCR1,SCR2,IERR)            1106YL92
            FMOM(IOP) = BEROT(1)*BEROT(2)*BEROT(3)*REDM**3              0601YC98

            DO 300 I = 1, 3                                             0125JZ07
              FPMOI(IOP,I)=BEROT(I)*REDM                                0125JZ07
              ABC(IOP,I)=0.5D0/FPMOI(IOP,I)
              DO 310 J = 1, 3                                           0125JZ07
               FPVEC(IOP,I,J) = PVEC(I,J)                               0125JZ07
310           ENDDO                                                     0125JZ07
300         ENDDO                                                       0125JZ07

      IF (LDEBUG) WRITE (FU6,1300) (FPMOI(IOP,I), I=1,3)                0125JZ07
      IF (LDEBUG) WRITE (FU6,1310) (ABC(IOP,I)*AUTOCM, I=1,3)
      IF (LDEBUG) WRITE(FU6,219) ((FPVEC(IOP,I,J),I=1,3),J=1,3)    
       
      ENDIF
C     IF (ICODE(IOP).LE.3.AND.IMPR.EQ.1) WRITE (FU6,1100) FMOM(IOP)     1125JC97
C     IF (ICODE(IOP).GT.3.AND.IMPR.EQ.1) WRITE (FU6,1200) FMOM(IOP)     1125JC97
C
C       MOVE C. O. M. AND UNIT VECTOR ALONG WITH PRINCIPAL AXIS BACK
C
      DO 340 K = 1, 3
        DO 350 I = K, N3, 3
         X(I) = X(I) + XCM(K)
350     CONTINUE
        IF(ICODE(IOP).GT.3) THEN
         DO 360 J = 1, 3
          FPVEC(IOP,J,K) = FPVEC(IOP,J,K) + XCM(K)
360      CONTINUE
        ENDIF
340   CONTINUE

      RETURN
C
  219 FORMAT(' corresponding eigenvectors:', 3(/,5X,3F15.10))           1106YL92
 1000 FORMAT(/1X,'New center of mass is at ',3(E15.6),                  0610WH94
     1       /1X,'bohrs with respect to previous origin.')
 1100 FORMAT(1X,'Moment of inertia (a.u.) = ',E14.6)
 1200 FORMAT(1X,'Products of three principal moments of inertia ',      1110DL89
     1'(a.u.) = ',E14.6)                                                0610WH94
 1300 FORMAT(1X,'Principal moment of inertia (a.u.) =',3E14.6)          0119JZ07
 1310 FORMAT(1X,'Rotational constants (in wavenumber)',3E14.6)
 1400 FORMAT(/1X,'For REACTANT ',I1)
 1410 FORMAT(/1X,'For GENERILIZED TS STATE')
C
      END
c********************************************************************
c CROSSPROD
c********************************************************************
c
      subroutine crossprod(A,B,C)
c  This subroutine is to calculate two vectors cross product
C  A x B = C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension A(3), B(3), C(3)

      C(1) = A(2)*B(3) - A(3)*B(2)
      C(2) = A(3)*B(1) - A(1)*B(3)
      C(3) = A(1)*B(2) - A(2)*B(1)
C
      return
      END
c******************************************************************
c SAVEXRP
c******************************************************************
c
      subroutine savexrp
      use perconparam
      use common_inc
      use cm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c     include 'param.inc'
c     include 'percon.inc'
c     include 'common.inc'
c
c calculate coordinates of each fragment and the other pivot points 
c relative to the 1st pivot point of reactant 1
c     
      do 100 j=1,2
c
         ntot=npvt(j)
         do 95 i=1,ntot
            inum=ippsv(i,j)
            do 85 k=2,0,-1
               xpp(inum*3-k,j)=xpvt(inum*3-k,j) - xpvt(3-k,j)
85          continue
95       continue
c
         ntot=nratom(j)
         do 90 i=1,ntot
            inum=iatsv(i,j)
            do 80 k=2,0,-1
               xrp(inum*3-k)=xr(inum*3-k,1) - xpvt(3-k,j)
80          continue
90       continue
c
100    continue
c
      return
      end
c******************************************************************
c
c AVERJ
c
c******************************************************************
c
      subroutine averj(te,rj,abc,nv,dnejq)
c
c This subrotine is to calculate the averaging over all possible 
c orientations of the vector J while its length is fixed -- <...>_omega_J
c in Eq. 2.39 in JCP V118, 5442, 2003.
c
c    called by
c              vrcnej
c    call
c              betai
c        
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
      PARAMETER (PI = 3.141592654D0, TPI = 6.283185307D0)
      dimension abc(5,3)
c
c Here we use composite Simpson's rule to do numerical integral
c nint must be even
c
      np=50
      dzai=tpi/dble(np)
      dnejq=0.0d0
      s=dble(nv)
      a=abc(5,1)
      b=abc(5,2)
      c=abc(5,3)
c
c     rj = dble(j)
c     if (j.eq.0) rj=0.5
C
C    CASE 1: C*(J+0.5)**2<= E-V <= A*(J+0.5)**2
C
      if (te.le.a*rj*(rj+1.0).and.te.ge.c*rj*(rj+1.0)) then      
c     if (te.le.a*(rj+0.5d0)**2.and.te.ge.c*(rj+0.5d0)**2) then      
c     if (te.le.a*rj*rj.and.te.ge.c*rj*rj) then
c   
        do 20 i=0,np
          zai=dble(i)*dzai
          bs=b*(dsin(zai))**2.d0+c*(dcos(zai))**2.d0
          e1=te-bs*rj*(rj+1.0d0)
c         e1=te-bs*(rj+0.5d0)**2
c         e1=te-bs*rj*rj
          if (e1.lt.0.) goto 20
          amb=dsqrt(a-bs)
          term = (e1**(s/2.d0))/amb
          if(i.eq.0.or.i.eq.np) then
            dnejq=dnejq + term
          elseif(i.ne.0.and.i.ne.np.and.mod(i,2).eq.0) then
            dnejq=dnejq + 2.0d0*term
          else
            dnejq=dnejq + 4.0d0*term
          endif
 20    continue
        dnejq=dnejq*dgamma(0.5d0)*dgamma(s/2.d0+0.5d0)/dgamma(s/2.d0
     *        +1.d0)*(rj+0.5d0)
c       dnejq=dnejq*dgamma(0.5d0)*dgamma(s/2.d0+0.5d0)/dgamma(s/2.d0
c    *        +1.d0)*rj
C
C    CASE 2: A*J*(J+1) <= E-V s is even
C
      elseif(te.ge.a*rj*(rj+1.d0).and.mod(nv,2).eq.0) then
c     elseif(te.ge.a*(rj+0.5d0)**2.and.mod(nv,2).eq.0) then
c     elseif(te.gt.a*rj*rj.and.mod(nv,2).eq.0) then
        term=0.5d0**(s/2.d0-1.d0)
        do iv = 1, nv/2
           term=term*(1.0d0+dble(iv-1)*2.0d0)
        enddo
        term=term/dgamma(s/2.0d0+1.d0)
        e2=te-a*rj*(rj+1.d0)
c       e2=te-a*(rj+0.5d0)**2
c       e2=te-a*rj*rj
        do 30 i=0,np
          zai=dble(i)*dzai
          bs=b*(dsin(zai))**2.d0+c*(dcos(zai))**2.d0
          e1=te-bs*rj*(rj+1.d0)
c         e1=te-bs*(rj+0.5d0)**2
c         e1=te-bs*rj*rj
          amb=dsqrt(a-bs)
c
          term1=tt1(nv/2-1,e1,e2,0)*(2.0d0*rj+1.0d0)
c         term1=tt1(nv/2-1,e1,e2,0)*2.0d0*rj
          term2=term*(e1**(s/2.0d0))/amb
          tmp = (2.d0*rj+1.d0)*amb/2.0d0/dsqrt(e1)
c         if(tmp-1.d0.gt.0.d0.and.tmp-1.d0.le.1.d-3) then
            term2=term2*dasin(min(1.d0,tmp))
c         else
c           goto 30
c         endif
          if(i.eq.0.or.i.eq.np) then
            dnejq=dnejq + term1 + term2
          elseif(i.ne.0.and.i.ne.np.and.mod(i,2).eq.0) then
            dnejq=dnejq + 2.0d0*(term1+term2)
          else
            dnejq=dnejq + 4.0d0*(term1+term2)
          endif
 30     continue
        dnejq=dnejq*(rj+0.5d0)
c       dnejq=dnejq*rj
C
C    CASE 3: A*J*(J+1) <= E-V s is odd
C
      elseif(te.ge.a*rj*(rj+1.d0).and.mod(nv,2).eq.1) then
c     elseif(te.ge.a*(rj+0.5d0)**2.and.mod(nv,2).eq.1) then
          e1=te-bs*rj*(rj+1.0d0)
          e2=te-a*rj*(rj+1.0d0)
c         e1=te-bs*(rj+0.5d0)**2
c         e2=te-a*(rj+0.5d0)**2
          do 40 i = 0, np 
          zai=dble(i)*dzai
          bs=b*(dsin(zai))**2+c*(dcos(zai))**2
          amb=dsqrt(a-bs)
  
c           tmp = (rj+0.5)*amb/dsqrt(e1)
c           if (abs(tmp).gt.1.0) goto 40
  
          if(i.eq.0.or.i.eq.np) then
            dnejq = dnejq + tt1((nv-1)/2,e1,e2,1)
          elseif(i.ne.0.and.i.ne.np.and.mod(i,2).eq.0) then
            dnejq = dnejq + 2.d0*tt1((nv-1)/2,e1,e2,1)
          elseif(mod(i,2).eq.1) then
            dnejq = dnejq + 4.d0*tt1((nv-1)/2,e1,e2,1)
          endif
 40     continue
        dnejq=dnejq*(rj+0.5d0)*(2.d0*rj+1.d0)
      endif
c
      dnejq=dnejq*dzai/3.0d0/(pi*(2.0*rj+1.0)**2)
c     dnejq=dnejq*dzai/3.0d0/(pi*4.0*rj*rj)
c
      return
      end
c
      FUNCTION tt1(nn,e1,e2,id)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       if(id.eq.0) then
         ns = (nn+1)*2
       else
         ns = 2*nn+1
       endif
       tt1 = 0.0d0
      do 10 iv = 0, nn
         ll1 = 1
         do j = 1, iv
            ll1 = ll1*(ns-1+(j-1)*(-2))
         enddo
         ll2 = 1
         do j = 1, iv+1
c           if (ns+(j-1)*(-2).eq.0) then
c             stop 'll = 0'
c           endif
            ll2 = ll2*(ns+(j-1)*(-2))
         enddo
         r = dble(ll1)/dble(ll2)
         p = dble(ns)/2.d0-0.5d0-dble(iv)
         tt1 = tt1 + r*(e1**iv)*(e2**p)
 10   continue  
      return
      end
c
c
      FUNCTION beta1(a,b)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c     USES dgamma
c     write(6,*) a, b
      beta1=dgamma(a)*dgamma(b)/dgamma(a+b)
      return
      end     
c
      FUNCTION betai(a,b,x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     REAL betai,a,b,x
CU    USES betacf,gammln
C     REAL bt,betacf,gammln
      if(x.lt.0..or.x.gt.1.) stop 'bad argument x in betai'
      if(x.eq.0..or.x.eq.1.)then
        bt=0.
      else
        bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.-x))
      endif
      if(x.lt.(a+1.)/(a+b+2.))then
        betai=bt*betacf(a,b,x)/a
        return
      else
        betai=1.-bt*betacf(b,a,1.-x)/b
        return
      endif
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
c
      FUNCTION betacf(a,b,x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     INTEGER MAXIT
C     REAL betacf,a,b,x,EPS,FPMIN
      PARAMETER (MAXIT=100,EPS=3.d-7,FPMIN=1.d-30)
C     INTEGER m,m2
C     REAL aa,c,d,del,h,qab,qam,qap
      qab=a+b
      qap=a+1.
      qam=a-1.
      c=1.
      d=1.-qab*x/qap
      if(abs(d).lt.FPMIN)d=FPMIN
      d=1./d
      h=d
      do 11 m=1,MAXIT
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
      stop 'a or b too big, or MAXIT too small in betacf'
1     betacf=h
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
C
      FUNCTION gammln(xx)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     REAL gammln,xx
C     INTEGER j
C     DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      DIMENSION cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ,4-#.
c
c******************************************************************
c
c INITVRC
c
c*****************************************************************
c
      subroutine initvrc
      use perconparam
      use common_inc
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!     include 'param.inc'
!     include 'percon.inc'
!     include 'common.inc'
c
c In VRC-TST calculation, START section is not give in the fu5 file
c therefore, the array iatsv need to be initialized.
c
      I=1
      Do 20 j=1,2
       Do 40 n=1,nratom(j)
        iatsv(i,5)=iatsv(n,j)
        i=i+1
40     continue
20    continue
      return
      end

      subroutine averjn(te,rj,abc,nv,dnejq,vq)
      use perconparam
      use common_inc
c
c This subrotine is to calculate the averaging over all possible 
c orientations of the vector J while its length is fixed -- <...>_omega_J
c in Eq. 2.39 in JCP V118, 5442, 2003.
c Numerically evaluate two dimensional integral using Monte Carlo method
c    called by
c              vrcnej
c    call
c              betai
c        
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     include 'param.inc'
C     include 'percon.inc'
C     include 'common.inc'
c#define SIMPLE_SPRNG
C#define USE_MPI 1
c#include "sprng_f.h"
c     SPRNG_POINTER st
c
      real*8 k,ran1
      dimension abc(5,3)
c
c Here we use composite Simpson's rule to do numerical integral
c nint must be even
c
c     iseed=9987345
      iseed=-10
c     st = init_sprng(4,iseed,SPRNG_DEFAULT)
      nmctemp=4000
c     dzai=tpi/dble(nint)
c     dk=dble(j)/dble(nint)
      dnejq=0.0d0
      vv=(dble(nv)-1.0d0)/2.0d0
c
c
c replace j by j+0.5
c
c     rj=dble(j)
c     if(j.eq.0) rj = 0.5d0
c
      a=abc(5,1)
      do 20 i=1, nmctemp
        zai=tpi*ran1(iseed)
c       zai=tpi*sprng(st)
        k=rj*(1.0d0-2.0d0*ran1(iseed))
c       k=rj*(1.0d0-2.0d0*sprng(st))
        bs=abc(5,2)*dsin(zai)**2+abc(5,3)*dcos(zai)**2
        term=te-bs*rj*(rj+1)-(a-bs)*k*k
c       term=te-bs*rj*rj-(a-bs)*k*k
c       term=te-bs*(rj+0.5d0)**2-(a-bs)*k*k
        if(term.lt.0) goto 20
        dnejq=dnejq+term**vv
20    continue
c     dnejq=dnejq/dble(nmctemp)
      dnejq=dnejq/dble(nmctemp)

      return
      end
C
      subroutine checks
      use perconparam
      use common_inc
      use cm
c    This subroutine is to make sure s is larger than the sum of two 
c    reactive atom van der Walls radii plus the corresponding 
C    pivot point to the atom distance
C  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!     include 'param.inc'
!     include 'percon.inc'
!     include 'common.inc'
      dimension drm(2)
      do 100 jtype =1, 2
        drm(jtype) = 0.0d0
        ntot = npvt(jtype)
  
        do 90 i = 1, ntot
          inum=ippsv(i,jtype)
          d = 0.0d0
          do k = 2 ,0, -1
            d=d+(xpp(inum*3-k,jtype)-xrp(ip2r(inum,jtype)*3-k))**2
          enddo
          d = dsqrt(d) + xvdw(ip2r(inum,jtype))
c         d = dsqrt(d) 
          if(d.gt.drm(jtype)) drm(jtype) = d
 90     continue
 100  continue 
c     dd = drm(1) + drm(2)
      dd = max(drm(1),drm(2))
      if (svl.lt.dd) then
         svl = dd
         write(fu6,'(a24,f5.2)') '  The smallerst s is changed to ', svl
      endif
      return
      end

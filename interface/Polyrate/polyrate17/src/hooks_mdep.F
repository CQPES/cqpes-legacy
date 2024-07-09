c***********************************************************************
      SUBROUTINE enatin(iopc,ircm)
c***********************************************************************
C
C   This subroutine calls Gaussian for an atomic calculation
C   If one of the reactants or products in an atomic species,
C   it calls this subroutine instead of geoin and secin (since
C   optimization or hessian calculation are meaningless).
C
      use perconparam
      use common_inc, only : x,dx,iatsv,v
      use cm, only : ivrc
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
      DIMENSION TMPCOORD(N3TM),TMPDX(N3TM),DMFMAT(N3TM,N3TM)
c
c     If it is a Grate restart, read the energy
c
      ircm=1
      irout=1
      iop = iopc
      if (irest.eq.1) call grest(irout,dummy)
      if (irout.eq.0) then
            ircm=0
            return
      endif
c
      TMPCOORD(:)=0.D0
      TMPDX(:)=0.D0
C
C   The G09/G03/G98/G94 start running
C
      if (ivrc.eq.1) then
       if (iopc.eq.1) CALL SYSTEM('../shuttle esp.fu71 esp.fu82')       0818JZ08
       if (iopc.eq.2) CALL SYSTEM('../shuttle esp.fu72 esp.fu82')       0818JZ08
       if (iopc.eq.3) CALL SYSTEM('../shuttle esp.fu73 esp.fu82')       0818JZ08
       if (iopc.eq.4) CALL SYSTEM('../shuttle esp.fu74 esp.fu82')       0818JZ08
       if (iopc.eq.5) CALL SYSTEM('../shuttle esp.fu75 esp.fu82')       0818JZ08
       if (iopc.eq.7) CALL SYSTEM('../shuttle esp.fu77 esp.fu82')       0818JZ08
       if (iopc.eq.8) CALL SYSTEM('../shuttle esp.fu78 esp.fu82')       0818JZ08
      else
       if (iopc.eq.1) CALL SYSTEM('./shuttle esp.fu71 esp.fu82')        0612BE06
       if (iopc.eq.2) CALL SYSTEM('./shuttle esp.fu72 esp.fu82')        0612BE06
       if (iopc.eq.3) CALL SYSTEM('./shuttle esp.fu73 esp.fu82')        0612BE06
       if (iopc.eq.4) CALL SYSTEM('./shuttle esp.fu74 esp.fu82')        0612BE06
       if (iopc.eq.5) CALL SYSTEM('./shuttle esp.fu75 esp.fu82')        0612BE06
       if (iopc.eq.7) CALL SYSTEM('./shuttle esp.fu77 esp.fu82')        0612BE06
       if (iopc.eq.8) CALL SYSTEM('./shuttle esp.fu78 esp.fu82')        0612BE06
      endif
C
C     if iop=2 or 4, we shift the optimized geometry, so that is does not
C     coincide with the iop=1 geometry
C
      if (iopc.eq.2.or.iopc.eq.4) then
         shft=30.D0
       else
         shft=0.D0
      endif
C
C   And now, we get the information from G09/G03/G98/G94 output
C
      NACAT=1
      CALL GOUTSEC (1,V,TMPCOORD,TMPDX,DMFMAT,IOPC,IATSV,
     *NACAT,NATOMS)
c
c   Now, we need a reordering of the coordinates and gradients
c
      do 1 j=1,natom
         do 2 k=1,natom
            if (j.eq.iatsv(k,iopc)) then
                do 3 L=1,3
                 X((j-1)*3+L)=shft
                 DX((j-1)*3+L)=0.D0
3               continue
            endif
2        continue
1        continue
c
c     If it is a Grate restart, write the energy
c
      if (irest.eq.1) then
          irout=5
          call grest(irout,dummy)
      endif
      return
      end subroutine enatin
c
c***********************************************************************
       SUBROUTINE escener(NATESC,ICHARGEJ,IMULTIPLJ,NATOM,LABEL,
     *IATSV,V,COORD,JTYPE,NACAT,iproc,IVRC)
c***********************************************************************
C
      use perconparam
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
C     DIMENSION LABEL(NATESC),COORD(N3TM),DX(N3TM)                      0701TA03
C     Variable DX was not used so it was taken out in Gaussrate 9.1.    0701TA03
      DIMENSION LABEL(NATESC),COORD(N3TM)                               0701TA03
      DIMENSION IATSV(NATESC,8)                                         1128JC97
      DIMENSION DMDX(N3TM),DMX(N3TM),DMFMAT(N3TM,N3TM)
C
C   The first step is writing the heading of G09/G03/G98/G94 for a single-point
C   energy calculation 
C
C   First of all, write input options
C
      CALL ginpene(ICHARGEJ,IMULTIPLJ,LABEL,iatsv,
     *COORD,jtype,nacat,NATESC)
C
C   Now, the G09/G03/G98/G94 start running
C
      if(ivrc.eq.1) then
        CALL SYSTEM('../shuttle esp.fu81 esp.fu82')                     0818JZ08
      else
        CALL SYSTEM('./shuttle esp.fu81 esp.fu82')                      0612BE06
      endif
C
C   And now, we get the information from G09/G03/G98/G94 output
C
      CALL goutsec (0,V,DMX,DMDX,DMFMAT,JTYPE,IATSV,
     *nacat,NATESC)                                                     0625JZ08
c
      RETURN
C
      END subroutine escener
c
c***********************************************************************
       SUBROUTINE escfirst(NATESC,ICHARGEJ,IMULTIPLJ,LABEL,
     *IATSV,V,COORD,DX,JTYPE,NACAT)
c***********************************************************************
C
      use perconparam
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
      DIMENSION LABEL(NATESC),COORD(N3TM),DX(N3TM)
      DIMENSION IATSV(NATESC,8)                                         1128JC97
      DIMENSION DMX(N3TM),TMPDX(N3TM),DMFMAT(N3TM,N3TM)
C
C   First, compare the geometry with the stored one, so we don't have to
C   repeat first derivative calculations (the geometres have to be identical:
C   no rotations or translations are allowed). If they are identical,
C   the first derivative information will be readed instead of calculated
C   and the program will return IDOP=1
C
      IDOP=0
      call store(V,COORD,DX,FMAT,IDOP)
      if (IDOP.EQ.1) then
         return 
      endif
C   
C   Write Gaussian input options
C
      CALL GINPFRS(ICHARGEJ,IMULTIPLJ,LABEL,iatsv,
     *COORD,jtype,nacat,NATESC)
C
C   Now, the G09/G03/G98/G94 start running
C
      CALL SYSTEM('./shuttle esp.fu81 esp.fu82')                        0612BE06
C
C   And now, we get the information from G09/G03/G98/G94 output
C
      CALL goutsec (2,V,DMX,TMPDX,DMFMAT,JTYPE,IATSV,
     *nacat,NATESC)
c
c     If the system is not the saddle point or any point along the
c     reaction coordinate, reordering of the gradients is required
c
      if (jtype.ne.5.and.jtype.ne.6) then                               1128JC97
      do 1 j=1,natom
         do 2 k=1,natom
            if (j.eq.iatsv(k,jtype)) then
                do 3 L=1,3
                 DX((j-1)*3+L)=TMPDX((k-1)*3+L)
3               continue
            endif
2        continue
1        continue
      else
        do 4 nj=1,natom
             do 5 L=1,3
                 DX((nj-1)*3+L)=TMPDX((nj-1)*3+L)
5       continue
4       continue
      endif
c
      RETURN
      END subroutine escfirst
c
c***********************************************************************
       SUBROUTINE escsec(NATESC,ICHARGEJ,IMULTIPLJ,LABEL,
     *IATSV,V,COORD,DX,FMAT,JTYPE,NACAT)
c***********************************************************************
C
      use perconparam
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
      DIMENSION LABEL(NATESC),COORD(N3TM),DX(N3TM),FMAT(N3TM,N3TM)
      DIMENSION IATSV(NATESC,8)                                         1128JC97
      DIMENSION DMX(N3TM),TMPDX(N3TM)
C
C   First of all, write Gaussian input options
C
      CALL ginpsec(ICHARGEJ,IMULTIPLJ,LABEL,iatsv,
     *COORD,jtype,nacat,NATESC)
C
C   Now, the G09/G03/G98/G94 start running
C
      if(ivrc.eq.1) then
        CALL SYSTEM('../shuttle esp.fu81 esp.fu82')                     0818JZ08
      else
        CALL SYSTEM('./shuttle esp.fu81 esp.fu82')                      0612BE06
      endif
C
C   And now, we get the information from G09/G03/G98/G94 output
C
      CALL goutsec (3,V,DMX,TMPDX,FMAT,JTYPE,IATSV,
     *nacat,NATESC)
c
c     If the system is not the saddle point or any point along the
c     reaction coordinate, reordering of the gradients is required
c
      if (jtype.ne.5.and.jtype.ne.6) then                               1128JC97
      do 1 j=1,natom
         do 2 k=1,natom
            if (j.eq.iatsv(k,jtype)) then
                do 3 L=1,3
                 DX((j-1)*3+L)=TMPDX((k-1)*3+L)
3               continue
            endif
2        continue
1        continue
      else
        do 4 nj=1,natom
             do 5 L=1,3
                 DX((nj-1)*3+L)=TMPDX((nj-1)*3+L)
5       continue
4       continue
      endif
c
C     We will store V, COORD, DX and FMAT so, in case we need it again 
C     for the same geometry, we will have it
C
      CALL store(v,coord,dx,fmat,1)
C
      RETURN
C
      END subroutine escsec
c
c***********************************************************************
       SUBROUTINE geoin(iopc,ircm)
c***********************************************************************
c
c     This subroutine calls Gaussian for optimizing a stationary point,
c     receiving the optimized geometry, energy of the optimized 
c     stationary point and residual gradients after the optimization.
c
      use perconparam
      use common_inc, only : nratom,x,dx,iatsv,v
      use cm, only : ivrc
      use gaussrate_param; use gaussrate_comm
      IMPLICIT REAL(8) (A-H,O-Z)
      DIMENSION TMPCOORD(N3TM),TMPDX(N3TM),DUMFMAT(N3TM,N3TM)
c
c     If it is a Grate restart, read the geom, energ and 1st der.
c
      ircm=1
      irout=4
      iop = iopc
      if (irest.eq.1) call grest(irout,dummy)
      if (irout.eq.0) then
            ircm=0
            return
      endif
c
c     Inicialize matrices
c
      do 99 ininic=1,n3tm
      TMPCOORD(ininic)=0.D0
      TMPDX(ininic)=0.D0
99    continue
C
C   The G09/G03/G98/G94 start running
C
      if (ivrc.eq.1) then
       if (iopc.eq.1) CALL SYSTEM('../shuttle esp.fu71 esp.fu82')       0818JZ08
       if (iopc.eq.2) CALL SYSTEM('../shuttle esp.fu72 esp.fu82')       0818JZ08
       if (iopc.eq.3) CALL SYSTEM('../shuttle esp.fu73 esp.fu82')       0818JZ08
       if (iopc.eq.4) CALL SYSTEM('../shuttle esp.fu74 esp.fu82')       0818JZ08
       if (iopc.eq.5) CALL SYSTEM('../shuttle esp.fu75 esp.fu82')       0818JZ08
       if (iopc.eq.7) CALL SYSTEM('../shuttle esp.fu77 esp.fu82')       0818JZ08
       if (iopc.eq.8) CALL SYSTEM('../shuttle esp.fu78 esp.fu82')       0818JZ08
      else
       if (iopc.eq.1) CALL SYSTEM('./shuttle esp.fu71 esp.fu82')        0612BE06
       if (iopc.eq.2) CALL SYSTEM('./shuttle esp.fu72 esp.fu82')        0612BE06
       if (iopc.eq.3) CALL SYSTEM('./shuttle esp.fu73 esp.fu82')        0612BE06
       if (iopc.eq.4) CALL SYSTEM('./shuttle esp.fu74 esp.fu82')        0612BE06
       if (iopc.eq.5) CALL SYSTEM('./shuttle esp.fu75 esp.fu82')        0612BE06
       if (iopc.eq.7) CALL SYSTEM('./shuttle esp.fu77 esp.fu82')        0612BE06
       if (iopc.eq.8) CALL SYSTEM('./shuttle esp.fu78 esp.fu82')        0612BE06
      endif

c
c     If iop=2 or 4, we shift the optimized geometry, so that is does not 
c     coincide with the iop=1 geometry
c
      if (iopc.eq.2.or.iopc.eq.4) then
         shft=30.D0
       else 
         shft=0.D0
      endif
c
c   And now, we get the information from G09/G03/G98/G94 output
c
      if (iopc.ne.5) then                                               1128JC97
          NACAT=NRATOM(iopc)
      else
          NACAT=NATOM
      endif
      CALL GOUTSEC (2,V,TMPCOORD,TMPDX,DUMFMAT,IOPC,IATSV,
     *NACAT,NATOMS)
c
c   Restore the geometry and gradients of the atoms 
c   excluded in the optimization
c
      if (iopc.ne.5.and.iopc.ne.6) then                                 1128JC97
c
c   If it is not the saddle point, reordering is needed, since not 
c   all the atoms are included in all the reactants and products
c
      do 1 j=1,natom
         do 2 k=1,natom
            if (j.eq.iatsv(k,iopc)) then
                do 3 L=1,3
                 X((j-1)*3+L)=TMPCOORD((k-1)*3+L)+shft
                 DX((j-1)*3+L)=TMPDX((k-1)*3+L)
3               continue
            endif
2        continue
1        continue
      else
c
c   If it is the saddle point, all the atoms are present in it
c
        do 4 nj=1,natom
             do 5 L=1,3
                 X((nj-1)*3+L)=TMPCOORD((nj-1)*3+L)
                 DX((nj-1)*3+L)=TMPDX((nj-1)*3+L)
5       continue
4       continue
      endif
C
c
c     If it is a Grate restart, write the geom., energy and 1st der.
c
      if (irest.eq.1) then
          call rchmul(iopc)
          irout=8
          call grest(irout,dummy)
      endif
      RETURN
      END subroutine geoin
c
c***********************************************************************
      SUBROUTINE secin(iopc)
c***********************************************************************
C
C   This subroutine calls Gaussian for a second derivative calculation.
C
      use perconparam
      use common_inc, only : nratom,x,dx,iatsv,v
      use cm, only : ivrc,label
      use gaussrate_param; use gaussrate_comm
      use gr_eshscm
      IMPLICIT REAL(8) (A-H,O-Z)
c     dimension fmat(n3tm,n3tm)
      dimension tmpcoord(n3tm),tmpdx(n3tm)
      if(.not.allocated(fmat)) allocate(fmat(n3tm,n3tm))
C
c     If it is a Grate restart, read the energy, 1st and 2nd der.
c
      irout=3
      if (irest.eq.1) call grest(irout,fmat)
      if (irout.eq.0) return
c
      if (iopc.ne.5.and.iopc.ne.6) then                                 1128JC97
         NACAT=NRATOM(iopc)
         ichargej=icharge(iopc)
         imultiplj=imultipl(iopc)
      else
         NACAT=NATOM
         ichargej=icharge(5)
         imultiplj=imultipl(5)
      endif
C
C   The first step is writing the G09/G03/G98/G94 input file
C
      CALL GINPSEC(ICHARGEJ,IMULTIPLJ,LABEL,iatsv,X,
     *iopc,nacat,NATOMS)
C
C   Now, the G09/G03/G98/G94 start running
C
      if(ivrc.eq.1) then
        CALL SYSTEM('../shuttle esp.fu81 esp.fu82')                     0818JZ08
      else
        CALL SYSTEM('./shuttle esp.fu81 esp.fu82')                      0612BE06
      endif
C
C   And now, we get the information from G09/G03/G98/G94 output: Geometry,
C   energy, gradients and hessian.
C
      CALL GOUTSEC (3,V,TMPCOORD,TMPDX,FMAT,IOPC,IATSV,
     *NACAT,NATOMS)
C
c   Restore the geometry and gradients of the atoms
c   excluded in the optimization
c
      if (iopc.ne.5.and.iopc.ne.6) then                                 1128JC97
c
c   If it is not the saddle point, reordering is needed, since not
c   all the atoms are included in all the reactants and products
c
      do 1 j=1,natom
         do 2 k=1,natom
            if (j.eq.iatsv(k,iopc)) then
                do 3 L=1,3
                 X((j-1)*3+L)=TMPCOORD((k-1)*3+L)
                 DX((j-1)*3+L)=TMPDX((k-1)*3+L)
3               continue
            endif
2        continue
1        continue
      else
c
c   If it is the saddle point, all the atoms are present in it
c
        do 4 nj=1,natom
             do 5 L=1,3
                 X((nj-1)*3+L)=TMPCOORD((nj-1)*3+L)
                 DX((nj-1)*3+L)=TMPDX((nj-1)*3+L)
5       continue
4       continue
      endif
c
c     If it is a Grate restart, write the energy, 1st and 2nd der.
c
      if (irest.eq.1) then
          irout=7
          call grest(irout,fmat)
      endif
      RETURN
C
      END subroutine secin
c

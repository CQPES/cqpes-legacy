c
c***********************************************************************
c     ropt31
c***********************************************************************
c
      subroutine ropt31
      use common_inc; use perconparam
      use rate_const
c
c     JCC   8/10/97
c
c     subroutine to read the general section of an incomplete fu31 in
c     order to set the options (LLOPT flags) for writting it.
c
c     called by rphwrt
c
c     calls rgen31, rline
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
      call rline(fu31,string,istrt,isect,iend)
c
c     If the fu31 file is empty, polyrate will write it from the scratch
c     and set the LLOPT flags to the default values
c
      if (iend.eq.1) then
         rewind fu31
         write(fu31,1000)
         call dfgn31
         do i=1,40
            LLOPT(i)=LOPT(i)
         enddo
1000     format(1x,'*GENERAL',/)
         return
      end if
c
      if (isect.ne.1) then
         write(fu6,1100)
1100     format(1x,'error:  first non-comment, non-blank line must be'
     *             ,' a section header that begins with a *')
         stop 'ropt31-1'
      end if
c
      if (string(istrt+1:istrt+6).eq.'genera') then
             call dfgn31
             call rgen31(string,iend,istrt)
             do i=1,40
                LLOPT(i)=LOPT(i)
             enddo
      else
             write(fu6,1200) 
             stop 'ropt31-2'
      endif
c
1200  format(1x,'Error: Only the general section can be present in a',
     *' writefu31 run')
      return
      end subroutine ropt31
c***********************************************************************
c     dfgn31
c***********************************************************************
c
      subroutine dfgn31
      use common_inc
      use perconparam
      use rate_const

c
c     JCC   8/10/97
c
c     subroutine to set the LOPT defaults for a fu31 file
c
c     called by ropt31, redf31
c
      implicit double precision (a-h,o-z)
c
c
c     set to dummy numbers the limits:
c
      uglig=-1.D10
      uglih=-1.D10
      uglsg=1.D10
      uglsh=1.D10
c
c     Initialize LOPT(40)
c
      do i=1,40
          LOPT(i)=0
      enddo
c
c     set defaults:
c
c     no recalculate s (KIES calculation)
c
      irecs=0
c
c     Unimolecular treatment: polys
c
c      inm31=1                                                            0601YC98
c
c      sincw=0.01D0                                                       0601YC98
c
c     Exponent LBEXP for curvature components:
c
c      lbexp=3                                                            0601YC98
c
c     Don't write anything (only accepted value)
c
      LOPT(1)=0
c
c     INTMUEFF=OFF: Interpolate the effective mu for cd-scsag tunneling
c
      LOPT(2)=500
c
c     X and DX in unscaled cartesians
c
      LOPT(3)=0
c
c     F in unscaled coordinates and packed 
c
      LOPT(4)=-1
c
c     No anharmonic data (only accepted value)
c
      LOPT(5)=0
c
c     Write out gradient on save grid (only accepted value)
c
      LOPT(6)=2
c
c     All the othe options can only be zero
c
      return
      end subroutine dfgn31
c***********************************************************************
c     rgen31
c***********************************************************************
c
      subroutine rgen31(string,iend,istrt)
      use common_inc
      use perconparam
      use rate_const
c
c     JCC   8/10/97
c
c     subroutine to read the general section of fu31 
c
c     called by ropt31
c
c     calls rline,rgem31,rgrd31,rhss31
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     set some defaults
c
      igem=1
      igrd=1
      ipck=1
      ihss=1
c
      call rline(fu31,string,istrt,isect,iend)
c
      do while (isect.eq.0.and.iend.eq.0)
c
c     INTMUEFF
      if (string(istrt:istrt+7).eq.'intmueff') then
         LOPT(2)=-500
c
c     NOINTMUEFF (default)
      else if (string(istrt:istrt+9).eq.'nointmueff') then
         LOPT(2)=500
c
c     KIES (recalculation of s)
      else if (string(istrt:istrt+3).eq.'kies') then
         IRECS=1
c
c     NOKIES (default)
      else if (string(istrt:istrt+5).eq.'nokies') then
         IRECS=0
c
c     Everything has to be in bohr and hartrees.
c     It will only accept gradients, no forces.
c
c     GEOM 
      else if (string(istrt:istrt+3).eq.'geom') then
          call rgem31(string,istrt)
c     GRADS
      else if (string(istrt:istrt+4).eq.'grads') then
          call rgrd31(string,istrt,igrd)
c     HESSIAN
      else if (string(istrt:istrt+6).eq.'hessian') then
          call rhss31(string,istrt,ipck,ihss)
c
c     Treatement od unimolecular side
c     UNIMOL
c      else if (string(istrt:istrt+5).eq.'unimol') then
c          call runm31(string,istrt)
c
c     For testing, we include the keywords grid
c     it has to be used as use  lim.inf.grad,
c     lim.sup.grad, lim.inf.hess and lim.sup.hes.
c     (uglig,uglsg,uglih,uglsh)
c
c     UGLIG
      else if (string(istrt:istrt+4).eq.'uglig') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: uglig variable must have an argument'
               stop
             else
               uglig = cfloat(string(istrt:80))
             end if
c     UGLSG
      else if (string(istrt:istrt+4).eq.'uglsg') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: uglsg variable must have an argument'
               stop
             else
               uglsg = cfloat(string(istrt:80))
             end if
c     UGLIH
      else if (string(istrt:istrt+4).eq.'uglih') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: uglih variable must have an argument'
               stop
             else
               uglih = cfloat(string(istrt:80))
             end if
c     UGLSH
      else if (string(istrt:istrt+4).eq.'uglsh') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: uglsh variable must have an argument'
               stop
             else
               uglsh = cfloat(string(istrt:80))
             end if
cc     LBEXP
c      else if (string(istrt:istrt+4).eq.'lbexp') then
c             call rword(string,istrt,ierr)
c             if (ierr.eq.1) then
c               write(fu6,*)'ERROR: lbexp variable must have an argument'
c               stop
c             else
c               lbexp = icint(string(istrt:80))
c               if (mod(lbexp,2).eq.0) then
c                write(fu6,*)'ERROR: lbexp must be an integer odd number'
c               endif
c             end if
cc
      else
        write(fu6,1200) string(istrt:80)
        stop 'rgen31-1'
      endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
c
      if (lgs(30).lt.0) then
        if (igem.eq.1.and.igrd.eq.1) LOPT(3)=0
        if (igem.eq.1.and.igrd.eq.2) LOPT(3)=-1
        if (igem.eq.2.and.igrd.eq.2) LOPT(3)=1
        if (igem.eq.2.and.igrd.eq.1) LOPT(3)=-2
      else
        if (igrd.eq.1) LOPT(3)=0
        if (igrd.eq.2) LOPT(3)=-1
      endif
c
      if (ipck.eq.1.and.ihss.eq.1) LOPT(4)=-1
      if (ipck.eq.1.and.ihss.eq.2) LOPT(4)=-2
      if (ipck.eq.2.and.ihss.eq.1) LOPT(4)=1
      if (ipck.eq.2.and.ihss.eq.2) LOPT(4)=2
c
1200  format(3x,'Error:  the following is not a valid keyword in the',
     *       ' fu31 general section',/A80)
c
      return
      end subroutine rgen31
c***********************************************************************
c     runm31
c***********************************************************************
c
      subroutine runm31(string,istrt)
      use perconparam, only : fu6,fu31
      use keyword_interface, only : gufac5
      use rate_const
c
c     JCC   8/10/97
c
c     subroutine to read the options for unimolecular reactions from fu31
c
c     called by rgen31
c
c     calls rline
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     set the defaults
c
c     inm31=1
C     above line is commented by J. Zheng Mar. 2010
C     Starting version 2010, default is set to polyz
C     
      inm31=2
c
      call rline(fu31,string,istrt,isect,iend)
      do while(string(istrt:istrt+2).ne.'end')
c
        if (string(istrt:istrt+4).eq.'polys') then
             inm31=1
        else if (string(istrt:istrt+4).eq.'polyz') then
             inm31=2
        else if (string(istrt:istrt+5).eq.'nopoly') then
             inm31=3
        else if (string(istrt:istrt+4).eq.'sincw') then
             call rword(string,istrt,ierr)
             if (ierr.eq.1) then
               write(fu6,*)'ERROR: sincw variable must have an argument'
               stop
             else
               sincw = cfloat(string(istrt:80))*gufac5                  0405JZ07
             end if
        else
             write(fu6,1200) string(istrt:80)
             stop 'runm31-1'
        endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu31 unimol keyword',/A80)
      return
      end subroutine runm31
c***********************************************************************
c     rgem31
c***********************************************************************
c
      subroutine rgem31(string,istrt)
      use perconparam
      use rate_const
c
c     JCC   8/10/97
c
c     subroutine to read the options for geometry of fu31
c
c     called by rgen31
c
c     calls rline
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     set the defaults 
c
      iugm31=0
c
      call rline(fu31,string,istrt,isect,iend)
      do while(string(istrt:istrt+2).ne.'end')
c
        if (string(istrt:istrt+1).eq.'ms') then
             igem=2
        else if (string(istrt:istrt+2).eq.'uns') then
             igem=1
        else if (string(istrt:istrt+2).eq.'ang') then
             iugm31=1
        else if (string(istrt:istrt+1).eq.'au') then
             iugm31=0
        else
             write(fu6,1200) string(istrt:80)
             stop 'rgem31-1'
        endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu31 geom keyword',/A80)
      return
      end subroutine rgem31
c***********************************************************************
c     rgrd31
c***********************************************************************
c
      subroutine rgrd31(string,istrt,igrd)
      use rate_const
      use perconparam
c
c     JCC   8/10/97
c
c     subroutine to read the options for gradients of fu31
c
c     called by rgen31
c
c     calls rline
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     set the defaults
c
      iugr31=0
      igof31=0
c
      call rline(fu31,string,istrt,isect,iend)
      do while(string(istrt:istrt+2).ne.'end')
c
        if (string(istrt:istrt+1).eq.'ms') then
             igrd=2
        else if (string(istrt:istrt+2).eq.'uns') then
             igrd=1
        else if (string(istrt:istrt+2).eq.'ang') then
             iugr31=1
        else if (string(istrt:istrt+1).eq.'au') then
             iugr31=0
        else if (string(istrt:istrt+4).eq.'grads') then
             igof31=0
        else if (string(istrt:istrt+5).eq.'forces') then
             igof31=1
        else
             write(fu6,1200) string(istrt:80)
             stop 'rgrd31-1'
        endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu31 grads keyword',/A80)
      return
      end subroutine rgrd31
c***********************************************************************
c     rhss31
c***********************************************************************
c
      subroutine rhss31(string,istrt,ipck,ihss)
      use rate_const
      use perconparam
c
c     JCC   8/10/97
c
c     subroutine to read the options for hessians of fu31
c
c     called by rgen31
c
c     calls rline
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     set the defaults
c
      iuhs=0
c
      call rline(fu31,string,istrt,isect,iend)
      do while(string(istrt:istrt+2).ne.'end')
c
        if (string(istrt:istrt+1).eq.'ms') then
             ihss=2
        else if (string(istrt:istrt+2).eq.'uns') then
             ihss=1
        else if (string(istrt:istrt+3).eq.'full') then
             ipck=2
        else if (string(istrt:istrt+3).eq.'pack') then
             ipck=1
        else if (string(istrt:istrt+2).eq.'ang') then
             iuhs31=1
        else if (string(istrt:istrt+1).eq.'au') then
             iuhs31=0
        else
             write(fu6,1200) string(istrt:80)
             stop 'rgem31-1'
        endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu31 hessian keyword',/A80)
      return
      end subroutine rhss31
c***********************************************************************
c     rph31
c***********************************************************************
c
      subroutine rph31(iop)
      use rate_const
      use perconparam
      use common_inc
c
c     JCC   8/10/97
c
c     subroutine to read the data from fu31
c
      implicit double precision (a-h,o-z)
c
c     IOP=0: First call to rph31. Read the options (rgen31) and read
c     all the information from fu31 
c
      call rph31_mem
      if (iop.eq.0) then
         write (fu6,*) 'Information read from unit fu31'
         npoints=0
         nhess=0
         do i=1,n3s31
          do j=1,npt31
            save31(i,j)=0.D0
          enddo
         enddo
         if (lgs(34).ne.0) then
            n3m7 = n3 - 1
         else
            n3m7 = n3 - 7
         endif
         do i=1,nsdim
C          do j=1,n3tm
           do j=1,nvibm                                                 0406JZ10
             ws(i,j)=0.D0
             bfs(i,j)=0.D0
             fmirs(i,j) = 0.0D0                                         0925JC97
           enddo
           vs(i)=0.D0
           fmoms(i)=0.D0
         enddo
         call dfgn31
         call rall31
c
c     IOP=1-5,7,8: Reactant or products or saddle point or wells
c
      else if (iop.ne.6) then
         itp=iop
         call intf31(itp)
      else 
         call nost31
      endif
      return
      end subroutine rph31
c
c***********************************************************************
c     rall31
c***********************************************************************
c
      subroutine rall31
      use common_inc
      use perconparam
      use rate_const
      use keyword_interface
c
c     JCC   8/10/97
c
c     subroutine to read all the data from fu31 and store it in array
c
      implicit double precision (a-h,o-z)
      character * 80 string
c 
      call rline(fu31,string,istrt,isect,iend)
c     
c     error check - check that the file wasn't empty and that the
c     first line found was the start of a section
c
      if (iend.eq.1) then
         write(fu6,1000)
1000     format(1x,'error:  input file was empty')
         stop 'rall31-1'
      end if
c
      if (isect.ne.1) then
         write(fu6,1100)
1100     format(1x,'error:  first non-comment, non-blank line must be'
     *             ,' a section header that begins with a *')
         stop 'rall31-2'
      end if
c
c     Call the correct routine to read in the data in this section.
c     control will come back here once another section header has
c     been found.  This will continue until an end-of-file mark
c     has been encoutered.
c
      do while (iend.eq.0)
         j = istrt + 1
         if (string(j:j+5).eq.'genera') then
               call rgen31(string,iend,istrt)
         else if (string(j:j+5).eq.'stop31') then
               write (fu6,1200)
1200           format (1x,'Warning: STOP31 ignored in file fu31.') 
               call rline(fu31,string,istrt,isect,iend)
         else if (string(j:j+5).eq.'react1') then
               call rpnt31(string,iend,istrt,1)
         else if (string(j:j+5).eq.'react2') then
               call rpnt31(string,iend,istrt,2)
         else if (string(j:j+4).eq.'prod1') then
               call rpnt31(string,iend,istrt,3)
         else if (string(j:j+4).eq.'prod2') then
               call rpnt31(string,iend,istrt,4)
         else if (string(j:j+5).eq.'saddle') then
               call rpnt31(string,iend,istrt,5)
         else if (string(j:j+4).eq.'point') then
               call rpnt31(string,iend,istrt,6)
         else if (string(j:j+4).eq.'wellr') then
               call rpnt31(string,iend,istrt,7)
         else if (string(j:j+4).eq.'wellp') then
               call rpnt31(string,iend,istrt,8)
         else
               write(fu6,1300) string(j-1:80)
               stop 'rall31-3'
         end if
      end do
c
1300  format(3x,'Error:  the following is not a valid section name',
     *         /A80)
c
c     Recalculate s
c
      if (irecs.eq.1) then
          call recs31
      endif
c
c     eliminate the zero of energy (all the points have already been
c     corrected)
c
      if (cezero.eq.'read') ezer0=0.D0
c
c     Reordering of the information
c
      nprpnt=npoint-3
      if (lgs(6).eq.1) nprpnt=nprpnt-2
      if (lgs(6).eq.2.or.lgs(6).eq.3) nprpnt=nprpnt-1
      write (fu6,2000) nprpnt
2000  Format (1x,'Number of nonstationary grid points: ',
     *I6)
      call ords31
      return
      end subroutine rall31
c***********************************************************************
c     rpnt31
c***********************************************************************
c
      subroutine rpnt31(string,iend,istrt,itp)
      use perconparam
      use common_inc
      use rate_const
      use keyword_interface
c
c     JCC   8/10/97
c
c     subroutine to read all the data from fu31 and store it in array
c
      implicit double precision (a-h,o-z)
      character*80 string
      character*80 word(40)
      logical lsec,leof
c
c
c     initialize the conversion factors for the geometries and gradients
c
             if (iugm31.eq.1) then                                      1104JC97
                 gmfac=1.88972652D0                                     1104JC97
             else                                                       1104JC97
                 gmfac=1.D0                                             1104JC97
             endif                                                      1104JC97
c
             if (iugr31.eq.1) then                                      1104JC97
                 grfac=1.88972652D0                                     1104JC97
             else                                                       1104JC97
                 grfac=1.D0                                             1104JC97
             endif                                                      1104JC97
c
             if (igof31.eq.1) then                                      1104JC97
                 grfac=-grfac                                           1104JC97
             endif                                                      1104JC97
c
c     counting the number of points:
c
      npoint=npoint+1
      save31 (1,npoint)=itp
c
c     read the information and store it in the array save31
c     the order of the information in the array is:
c               Position          Information
c                  1                 ITP
c
c                  2                 NDATA
c
c                  3                  S
c
c                  4                  V
c
c               5,n3tm+4              X
c            n3tm+5,2*n3tm+4          DX
c
c               2*n3tm+5              Index of hessian in hess31
c  
c     and ndata is calculated depending on the information, adding the
c     following:
c
c     F: 10000
c     V:  1000
c     X:   100
c    DX:    10
c     S:     1
c     
c     i.e.:
c               NDATA              Information in fu31
c                   1                S
c                1001                V and S
c                1101                S, X and V
c                1110                S, X, V, DX
c               11111                S, X, V, DX, F
c
      call rline(fu31,string,istrt,isect,iend)
      do while (isect.eq.0.and.iend.eq.0)
         if (string(istrt:istrt+3).eq.'vmep') then
             call rword (string, istrt, ierr)
             if (ierr.ne.1) then
                 save31(4,npoint)=cfloat(string(istrt:80))
                 if (cezero.eq.'read') save31(4,npoint) =
     *                                 save31(4,npoint) - ezer0
                 save31(2,npoint)=save31(2,npoint)+1000.D0
             else
                 write (fu6,*) 'Error reading V in fu31, point',
     *                          npoint 
                 stop 'rpnt31-1'
             endif
         else if (string(istrt:istrt+3).eq.'smep') then
             call rword (string, istrt, ierr)
             if (ierr.ne.1) then
                 save31(3,npoint)=cfloat(string(istrt:80))
                 if (save31(3,npoint).lt.uglig. or.
     *               save31(3,npoint).gt.uglsg) then
                     idelet=1
                 else
                     idelet=0
                 endif
                 save31(2,npoint)=save31(2,npoint)+1.D0
             else
                 write (fu6,*) 'Error reading S in fu31, point',
     *                          npoint
                 stop 'rpnt31-2'
             endif
         else if (string(istrt:istrt+3).eq.'geom') then
             if (itp.ne.6) then
                  write (fu6,*) 'Error: Geom only allowed in point'
                  stop 'rpnt31-3'
             endif
             save31(2,npoint)=save31(2,npoint)+100.D0
             read (fu31,*) (X(I),I=1,N3)
             call readln(fu31,word,nword,lsec,leof)
             if (word(1) .ne. 'END') then
                write(fu6,*) 'Error reading the geom of point', npoint
                stop 'rpnt31-4'
             endif
c
c if the coordinates are mass-scaled, transform to unscaled
c
cjc
             if (igem.eq.2) call rphtrx (n3,amass,x,2)
cjc
             do i=1,n3
                save31(i+4,npoint)=X(i)*gmfac
             enddo
         else if (string(istrt:istrt+4).eq.'grads') then
             if (itp.ne.6) then
                  write (fu6,*) 'Error: Grads only allowed in point'
                  stop 'rpnt31-5'
             endif
             save31(2,npoint)=save31(2,npoint)+10.D0
             read (fu31,*) (DX(I),I=1,N3)
             call readln(fu31,word,nword,lsec,leof)
             if (word(1) .ne. 'END') then
                write(fu6,*) 'Error reading grads in point', npoint
                stop 'rpnt31-6'
             endif
             do i=1,n3
                save31(i+n3tm+4,npoint)=DX(i)*grfac
             enddo
         else if (string(istrt:istrt+6).eq.'hessian') then
             if (itp.lt.5) then
                 N=3*nratom(itp)
             else
                 N=3*natom
             endif
             call rhes31(N)
             nhess=nhess+1
             save31(2*n3tm+5,npoint)=dble(nhess)
             do i=1,n3tm
              do j=1,n3tm
                hess31(i,j,nhess)=F(i,j)
              enddo
             enddo
             save31(2,npoint)=save31(2,npoint)+10000.D0
         else if (string(istrt:istrt+5).eq.'enerxn') then
             if ((lgs(6).eq.1.or.lgs(6).eq.3).and.itp.ne.4) then
                 write (fu6,1201) 
1201   format(1x,'Error: the keyword enerxn must be in the prod2',
     * ' section of fu31')
                 stop 'rpnt31-7'
             else if ((lgs(6).eq.2.or.lgs(6).eq.4).and.itp.ne.3)
     *            then
                 write (fu6,1202) 
1202   format(1x,'Error: the keyword enerxn must be in the prod1',
     * ' section of fu31')
                 stop 'rpnt31-8'
             else
               call rword (string, istrt, ierr)
               if (ierr.ne.1) then
                   save31(4,npoint)=cfloat(string(istrt:80))
                   if (cezero.eq.'read') save31(4,npoint) =
     *                                   save31(4,npoint) - ezer0
                   save31(2,npoint)=save31(2,npoint)+1000.D0
               else
                   write (fu6,*) 'Error reading enerxn in fu31'
                   stop 'rpnt31-9'
               endif
             endif
         else if (string(istrt:istrt+5).eq.'enesad') then
             if (itp.ne.5) then
                 write (fu6,1203) 
1203   format(1x,'Error: the keyword enersad must be in the saddle',
     * ' section of fu31')
                 stop 'rpnt31-10'
             else
               call rword (string, istrt, ierr)
               if (ierr.ne.1) then
                   save31(4,npoint)=cfloat(string(istrt:80))
                   if (cezero.eq.'read') save31(4,npoint) =
     *                                   save31(4,npoint) - ezer0
                   save31(2,npoint)=save31(2,npoint)+1000.D0
               else
                   write (fu6,*) 'Error reading enesad in fu31'
                   stop 'rpnt31-11'
               endif
            endif
         else if (string(istrt:istrt+7).eq.'enewellr') then
             if (itp.ne.7) then
                 write (fu6,1204)
                 stop 'rpnt31-12'
1204   format(1x,'Error: the keyword enewellr must be in the wellr',
     * ' section of fu31')
             else
               call rword (string, istrt, ierr)
               if (ierr.ne.1) then
                   save31(4,npoint)=cfloat(string(istrt:80))
                   if (cezero.eq.'read') save31(4,npoint) =
     *                                   save31(4,npoint) - ezer0
                   save31(2,npoint)=save31(2,npoint)+1000.D0
               else
                   write (fu6,*) 'Error reading enewellr in fu31'
                   stop 'rpnt31-13'
               endif
            endif
         else if (string(istrt:istrt+7).eq.'enewellp') then
             if (itp.ne.8) then
                 write (fu6,1205)
                 stop 'rpnt31-14'
1205   format(1x,'Error: the keyword enewellp must be in the wellp',
     * ' section of fu31')
             else
               call rword (string, istrt, ierr)
               if (ierr.ne.1) then
                   save31(4,npoint)=cfloat(string(istrt:80))
                   if (cezero.eq.'read') save31(4,npoint) =
     *                                   save31(4,npoint) - ezer0
                   save31(2,npoint)=save31(2,npoint)+1000.D0
               else
                   write (fu6,*) 'Error reading enewellp in fu31'
                   stop 'rpnt31-15'
               endif
            endif
         else
            write(fu6,1200) string(istrt:80)
            stop 'rpnt31-16'
         end if
         ifu31=fu31
         call rline(ifu31,string,istrt,isect,iend)
      end do
1200  format(1x,'Error: The following keyword is not valid in fu31',
     */A80)
      if (idelet.eq.1.and.save31(1,npoint).gt.5.9D0.and.
     *  save31(1,npoint).lt.6.1D0) then
           do i=1,n3s31
             save31(i,npoint)=0.D0
           enddo
           npoint=npoint-1
      endif
      return
      end subroutine rpnt31
c***********************************************************************
c     rhes31
c***********************************************************************
c
      subroutine rhes31(N)
      use common_inc
      use perconparam
      use rate_const
c
c     JCC   8/10/97
c
c     subroutine to read hessian matrix from unit 31
c
      implicit double precision (a-h,o-z)
c
C      character * 80 string
C      The above line was commented because variable is not used.       0423TA02
      character*80 word(40)
      logical lsec,leof
c
c     initialize the conversion factors for the hessians
c
             if (iuhs31.eq.1) then                                      1104JC97
                 hsfac=1.88972652D0                                     1104JC97
             else                                                       1104JC97
                 hsfac=1.D0                                             1104JC97
             endif                                                      1104JC97
c
      k = 0
      j = 1
 10   call readln(fu31,word,nword,lsec,leof)
      if(word(1) .eq. 'END') then
         if(j.ne.N .or. k.ne.N) then
            write(fu6,*) 'Not enough numbers entered for a ',
     *                   N,' by ',N,'Hessian'
            stop 'rhes31 1'
         endif
C
         return
C
      else
         do 20 i = 1, nword
            k = k + 1
C
C hessian input in packed form
            if (lopt(4).lt.0) then
               if(k .gt. j) then
                  j = j + 1
                  k = 1
               endif
               if(j .gt. N) goto 100
               F(j,k) = cfloat(word(i))*hsfac
               F(k,j) = F(j,k)
C
c hessian input in full form
            else
               if(k .gt. N) then
                  j = j + 1
                  k = 1
               endif
               if(j .gt. N) goto 100
               F(j,k) = cfloat(word(i))*hsfac
            endif
 20      continue
      endif
      goto 10
C
 100  write(fu6,*) 'Too many numbers entered for a ',N,' by ',N
     *             ,'hessian'
      stop 'rhes31 2'
C
      end subroutine rhes31
c***********************************************************************
c     ords31
c***********************************************************************
c
      subroutine ords31
      use common_inc
      use perconparam
      use rate_const
c
c     JCC   8/10/97
c
c     subroutine to reorder the information on array save31
c
      implicit double precision (a-h,o-z)
      alim = 1.D-5
c
c     calculate number of save points (itp=6, save31(2,i)=11111)
c     and the number of save points at each side of the saddle
c     point
c
      NPTS=0
      NPOS=0
      NNEG=0
c
      do i=1,npoint
       if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0) then
           if(save31(3,i).lt.uglih.or.save31(3,i).gt.uglsh) then
              if(save31(2,i).gt.11110.9D0) then
                save31(2,i)=save31(2,i)-10000.D0
              endif
           endif
           if(save31(2,i).gt.11110.9D0) then
               NPTS=NPTS+1
               if (save31(3,i).gt.0.D0) then
                  NPOS=NPOS+1
               else 
                  NNEG=NNEG+1
               endif
           endif
       endif
      enddo
      if (npts.gt.nsdim-5) then
         write (fu6,*) 'Error: Maximum nuber of save points: ',nsdim
         stop 'ords31-1'
      endif
c
      write (fu6,*) 'Number of nonstationary save points: ',NPTS
c     NSS=NPTS+2
c     IF (LGS(1).GT.0) NSS = NSS+1
c     IF (LGS(6).EQ.1.OR.LGS(6).EQ.2) THEN                         
c        NSS = NSS+1
c        NSF = 3
c     ELSE
c        NSF = 2
c     ENDIF
c     IF (LGS(6).EQ.1.OR.LGS(6).EQ.3) THEN                          
c        NSS = NSS+1
c        NSL = NSS-2
c     ELSE
c        NSL = NSS-1
c     ENDIF
      NSS=NPTS+5                                                        0922JC97
      NSF=3                                                             0922JC97
      NSL=NSS-2
      ISSP=NSF+NNEG
      ICHECK=NSL-NPOS
      IF (ISSP.NE.ICHECK) THEN
          write (fu6,*) 'Error counting the number of points.'
          write (fu6,*) 'Unable to determine the saddle point index,'
          write (fu6,*) 'ISSP = ',ISSP,', ICHECK =',ICHECK
          stop 'ords31-2'
      ENDIF
      return
      end  subroutine ords31
c***********************************************************************
c     intf31
c***********************************************************************
c
      subroutine intf31(itp)
      use common_inc
      use perconparam
      use rate_const
      use kintcm
c
c     JCC   8/10/97
c
      implicit double precision (a-h,o-z)
      dimension tmpfrq(nvibm)
      alim = 1.D-5
c
c     look for the system in save31
c
      i=1
10    if (dabs(save31(1,i)-itp).lt.alim) then
         call stat31(itp,i)
         goto 20
      else 
         i=i+1
         if (i.le.npoint) then
            goto 10
         else
            write (fu6,*) 'Error: system iop= ',itp,
     *                    ' not found in fu31'
            stop 'intf31-1'
         endif
      endif
c
c     if itp=2, reorder the reactant frequencies 
c
20    if (itp.eq.2) then
c
        if (lgs(34).ne.0) then
          ishft=1
        else if (icode(5).eq.3) then
          ishft=6
        else
          ishft=7
        endif
        n=3*natom
        nfreq=n-ishft
        do i=1,nvibm
          tmpfrq(i)=ws(1,i)
          ws(1,i)=-1.D10
           do k=1,2
            do j=1,nvibm
             if (ws(k,j).gt.tmpfrq(i)) then
                 tmp=ws(k,j)
                 ws(k,j)=tmpfrq(i)
                 tmpfrq(i)=tmp
             endif
            enddo
           enddo
        enddo
        do i=1,nvibm
           ws(1,i)=0.D0
           ws(2,i)=0.D0
        enddo
        do i=1,nfreq
           ws(1,nfreq+1-i)=tmpfrq(i)
        enddo
      endif
c
c     if itp=4, reorder the product frequencies
c
      if (itp.eq.4) then
c
        if (lgs(34).ne.0) then
          ishft=1
        else if (icode(5).eq.3) then
          ishft=6
        else
          ishft=7
        endif
        n=3*natom
        nfreq=n-ishft
        do i=1,nvibm
          tmpfrq(i)=ws(3,i)
          ws(3,i)=-1.D10
           do k=3,4
            do j=1,nvibm
             if (ws(k,j).gt.tmpfrq(i)) then
                 tmp=ws(k,j)
                 ws(k,j)=tmpfrq(i)
                 tmpfrq(i)=tmp
             endif
            enddo
           enddo
        enddo
        do i=1,nvibm
           ws(3,i)=0.D0
           ws(4,i)=0.D0
        enddo
        do i=1,nfreq
           ws(3,nfreq+1-i)=tmpfrq(i)
        enddo
      endif
c
c     if itp=5, eliminate the lower frequency for reactant and/or
c     product in a unimolecular reaction from the interpolation array.
c
      if (itp.eq.5) then
         if (lgs(6).eq.3.or.lgs(6).eq.4) then
            do i=2,nvibm
              ws(1,i-1)=ws(1,i)
            enddo
            ws(1,nvibm)=0.D0
         endif  
         if (lgs(6).eq.2.or.lgs(6).eq.4) then
            do i=2,nvibm
              ws(3,i-1)=ws(3,i)
            enddo
            ws(3,nvibm)=0.D0
         endif  
c
c     The same for wells, but moving them to their position
c
         if (irepr(7).eq.1) then
            do i=2,nvibm
              ws(2,i-1)=ws(7,i)
            enddo
            do i=1,nvibm
              ws(7,i)=0.D0
              fmirs(2,i)=fmirs(7,i)
              fmirs(7,i)=0.D0
            enddo
         endif 
         if (irepr(8).eq.1) then
            do i=2,nvibm
              ws(nsdim-1,i-1)=ws(8,i)
            enddo
            do i=1,nvibm
              ws(8,i)=0.D0
              fmirs(nsdim-1,i)=fmirs(8,i)
              fmirs(8,i)=0.D0
            enddo
         endif 
      endif
c
c     if itp=5 move the product freq from the position 3 to nss
c     and from 5 to issp
c
      if (itp.eq.5) then
        if (lgs(6).eq.4.and.nss.eq.5) then
         do i=1,nvibm
           ws(nsdim,i)=ws(3,i)
           ws(nsdim-2,i)=ws(5,i)
           ws(3,i)=ws(nsdim-2,i)
           ws(5,i)=ws(nsdim,i)
           fmirs(nsdim,i)=fmirs(3,i)
           fmirs(nsdim-2,i)=fmirs(5,i)
           fmirs(3,i)=fmirs(nsdim-2,i)
           fmirs(5,i)=fmirs(nsdim,i)
         enddo
        else
        do i=1,nvibm
           ws(nss,i)=ws(3,i)
           ws(nsdim,i)=ws(3,i)
           ws(3,i)=0.D0
           ws(4,i)=0.D0
           fmirs(nss,i)=fmirs(3,i)
           fmirs(nsdim,i)=fmirs(3,i)
           fmirs(3,i)=0.D0
           fmirs(4,i)=0.D0
           if (issp.ne.5) then
               ws(issp,i)=ws(5,i)
               ws(5,i)=0.D0
               fmirs(issp,i)=fmirs(5,i)
               fmirs(5,i)=0.D0
           endif
           ws(nsdim-2,i)=ws(issp,i)
           ws(nss-1,i)=ws(nsdim-1,i)
           fmirs(nsdim-2,i)=fmirs(issp,i)
           fmirs(nss-1,i)=fmirs(nsdim-1,i)
        enddo
        endif
      endif
      return
      end subroutine intf31
c***********************************************************************
c     stat31
c***********************************************************************
c
      subroutine stat31(itp,inds)
      use common_inc
      use perconparam
      use rate_const 
c     use kintcm, only : iproj
      use kintcm
c     use energetics_mod
c
c     JCC   8/10/97
c
c     subroutine to calculate stationary point information about the ITP
c     species stored in the IND podition of array save31. 
c
      implicit double precision (a-h,o-z)
      dimension tempx(n3tm,n3tm)
      alim=1.D-5
c
      v=0.D0
c
c     Option morse diatomic not valid here
c
      if (icode(itp).eq.2) then
          write (fu6,1000) 
          stop 'stat31-1'
      endif
1000  format(1x,'Sorry, no anharmonicity available. Run again without',
     *' Morse treatment',/,1x,'for diatomics')
c    
c     if it is atomic, no information required. If it is the last product,
c     then the energy is the exoergicity. If it happens, ITP=4 (it can not
c     be a reaction with only a product being an atomic species)
c     
      if (icode(itp).eq.1) then
          if (itp.eq.4) then
                vs(nss)=save31(4,inds)
                vs(nsdim)=save31(4,inds)
                v=save31(4,inds)
          endif
          return
      endif
c
c     check the required infomation is stored: Hessians and, for the last
c     product, the energy. Therefore, for the last product NDATA>=11000
c     and for the other systems, NDATA>=10000
c
      if (lgs(6).eq.1.or.lgs(6).eq.3) then
          if (itp.eq.4.or.itp.eq.7.or.itp.eq.8) then
               if(save31(2,inds).lt.10999.D0) then
                  write (fu6,1100)
                  stop 'stat31-2'
               else
                 if (itp.eq.4) then
                    vs(nss)=save31(4,inds)
                    vs(nsdim)=save31(4,inds)
                    v=save31(4,inds)
                 else if (itp.eq.7) then
                    vs(2)=save31(4,inds)
                    v=save31(4,inds)
                 else if (itp.eq.8) then
                    vs(nss-1)=save31(4,inds)
                    vs(nsdim-1)=save31(4,inds)
                    v=save31(4,inds)
                 endif
               endif
          else if (save31(2,inds).lt.9999.D0) then
               write (fu6,1100)
               stop 'stat31-3'
          endif
      else
          if (itp.eq.3.or.itp.eq.7.or.itp.eq.8) then
               if (save31(2,inds).lt.10999.D0) then
                  write (fu6,1100)
                  stop 'stat31-4'
               else
                 if (itp.eq.3) then
                    vs(nss)=save31(4,inds)
                    vs(nsdim)=save31(4,inds)
                    v=save31(4,inds)
                 else if (itp.eq.7) then
                    vs(2)=save31(4,inds)
                    v=save31(4,inds)
                 else if (itp.eq.8) then
                    vs(nss-1)=save31(4,inds)
                    vs(nsdim-1)=save31(4,inds)
                    v=save31(4,inds)
                 endif
               endif
          else if (save31(2,inds).lt.9999.D0) then
               write (fu6,1100)
               stop 'stat31-5'
          endif
      endif
1100  format(1x,'Error: Not enough information for a reactant or a',
     *' product.',/,1x,'The information required is the hessian for',
     *' systems with two',/,1x,' or more atoms, and the energy for ',
     *'the last product',/,1x,' and wells.')
c
c     Now we check that the saddle point has energy and hessian
c     and store the energy
c
      if (itp.eq.5) then
        if (save31(2,inds).lt.10999.D0) then
          write (fu6,1200)
          stop 'stat31-6'
        else
          vs(issp)=save31(4,inds)
          vs(nsdim-2)=save31(4,inds)
          v=save31(4,inds)
        endif
      endif
1200  format(1x,'Error: Not enough information for the saddle point.', 
     */,1x,'The information required is the hessian and energy.')
c
c     Moments of inertia:
c
      IF (ITP.EQ.1) THEN
         FMOMS(1) = 1.0D0/FMOM(1)
      ELSEIF (ITP.EQ.2) THEN
         FMOMS(1) = 0.0D0
      ELSEIF (ITP.EQ.3) THEN
         FMOMS(NSS) = 1.0D0/FMOM(3)
         FMOMS(NSDIM) = 1.0D0/FMOM(3)
      ELSEIF (ITP.EQ.4) THEN
         FMOMS(NSS) = 0.0D0
         FMOMS(NSDIM) = 0.0D0
      ELSEIF (ITP.EQ.7) THEN
         FMOMS(2) = 1.0D0/FMOM(7)
      ELSEIF (ITP.EQ.8) THEN
         FMOMS(NSDIM-1) = 1.0D0/FMOM(8)
         FMOMS(NSS-1) = 1.0D0/FMOM(8)
      ELSEIF (ITP.EQ.5) THEN
         do i=1,n3tm
           x(i)=xr(i,5)
           xxs(i,issp)=xr(i,5)
         enddo
         if(lgs(34).ne.0 .and. itp.gt.4 )then    
            fmoms(issp) = 1.0D+30                                       
            fmoms(nsdim-2) = 1.0D+30                                       
         else
            call center (5,1)
            fmoms(issp) = 1.0d0/fmom(5)
            fmoms(nsdim-2) = 1.0d0/fmom(5)
         endif
cjc
         call rphtrx (n3,amass,x,1)
cjc
      ENDIF
c
c     diagonalize the hessian and store the frequencies. We will store them
c     as ws(itp,nfreq) by now
c
      if (itp.eq.5) then 
         if (lgs(34).ne.0) then
           ishft=1
         else if (icode(5).eq.3) then
           ishft=6
         else
           ishft=7
         endif
         n=3*natom
         nfreq=n-ishft
      else 
         if (icode(itp).eq.3) then
             ishft=5
         else
             ishft=6
         endif
         n=3*nratom(itp)
         nfreq=n-ishft
      endif
      if (itp.eq.5) then
         iopf=2
      else
         iopf=-itp
      endif
c
c     Put the information from save31 to F
c
      ihs=int(save31(2*n3tm+5,inds))
      do i=1,n3tm
        do j=1,n3tm
           F(i,j)=hess31(i,j,ihs)
        enddo
      enddo
     
c
C
      if (iabs(lopt(4)).eq.1) call rphtrf(n,amass,F,1)
c
      if (itp.eq.5) then
         iopf=2
      else
         iopf=-itp
      endif
C added by Jingjing Zheng, following code is from subroutine normod
C PROJECT OUT THE TRANSLATIONAL AND ROTATIONAL CONTAIMINATIONS          6/13T89
C FOR JOP = 3, IT IS DONE EXPLICITLY IN ROUTINE PROJCT                  6/13T89
c only nosupermol can project out RT of reactants and products
c and well and ts can be projected or not
c
c        NEND = NDIM(itp)
c        IF ((isup.eq.1.and.itp.lt.0.and.iproj(abs(itp)).ne.0).or.
c    +    (itp.gt.0.and.iproj(5).ne.0).or.
c    +    (itp.lt.-4.and.iproj(abs(itp)).ne.0)) THEN 
c                 DO 62 I = 1,NEND                                      6/13T89
c                   DO 62 J = 1,NEND                                    6/13T89
c                      SUM = 0.0D0                                      6/13T89
c                      DO 61 K = 1,NEND                                 6/13T89
c                         SUM = SUM + F(I,K)*PROJ(K,J)
c61                    CONTINUE                                         6/13T89
c                      TEMPX(I,J) = SUM 
c62               CONTINUE                                              6/13T89
c                 DO 68 I = 1,NEND                                      6/13T89
c                    DO 68 J = 1,NEND                                   6/13T89
c                       SUM = 0.0D0                                     6/13T89
c                       DO 66 K = 1,NEND                                6/13T89
c                          SUM = SUM + PROJ(I,K)*TEMPX(K,J) 
c66                     CONTINUE                                        6/13T89
c                       F(I,J) = SUM                                    6/13T89
c68               CONTINUE                                              6/13T89
c        ENDIF 
c
c
c     Diagonalize
c
      call fdiag(iopf)
c
      call zeropt(iopf)                                                 0317Yc99
      call norout (iopf,dxp)                                            0317Yc99
c      if (itp.eq.5) call norout (iopf,dxp)                             0317Yc99
c
c     Store the frequencies
c
      do ifrq=1,nfreq
          ws(itp,ifrq)=freq(ifrq+ishft)
      enddo
c                                                                       0814JC98
c     Store the saddle point imaginary frequency eigenvector            0814JC98
c                                                                       0814JC98
      if (lgs(27).eq.-1) then                                           0814JC98
          sgn = 1.0D0                                                   0814JC98
      else                                                              0814JC98
          sgn = -1.0D0                                                  0814JC98
      endif                                                             0814JC98
      do i = 1, n3tm                                                    0814JC98
          dxsad(i) = sgn*cof(i,1)                                       0814JC98
      enddo                                                             0814JC98
c                                                                       0814JC98
C
C sort freq in descending order                                        
C
      do i = 1,nfreq                                               
        do j = i+1, nfreq                                         
          if (freq(i+ishft).gt.freq(j+ishft)) then                
              tempf=freq(i+ishft)                               
              freq(i+ishft) = freq(j+ishft)                    
              freq(j+ishft) = tempf                           
          endif                                               
        enddo                                               
      enddo                                                
c
         IF (LGS(5).GT.0)  THEN                                         0925JC97
            CALL ANHARM (IOPF)                                          0925JC97
            DO I = 1, NFREQ                                             0925JC97
                 FMIRS(ITP,I) = FMOMHR(I+ISHFT)                         0925JC97
            ENDDO                                                       0925JC97
         ENDIF                                                          0925JC97
      call zeropt(iopf)
c
      return
      end subroutine stat31
c***********************************************************************
c     nost31
c***********************************************************************
c
      subroutine nost31
      use common_inc
      use perconparam
      use rate_const
      use kintcm; use potmod
c
c     JCC   8/10/97
c
c     subroutine to process the information about non-stationary points
c     along the reaction path
c
      implicit double precision (a-h,o-z)
      dimension sx(3)
      dimension insv31(nsdm)
      alim=1.D-5
      is=nsf
      n=3*natom
      icount=1
  
c
c     Look for the value of s for the save point closer to reactants
c     still unread
c
1     if (is.eq.issp) is=is+1
      stemp=1.D10
      indst=0
      do i=1,npt31
        if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0.
     *    and.save31(2,i).gt.9999.D0.and.
     *      save31(3,i).lt.stemp) then
                indst=i
                stemp=save31(3,i)
        endif
      enddo
c
c     Mark this save point as read. From now on, we will only use it
c     if we need its gradient, so we set NDATA=NDATA-10000
c
      save31(2,indst)=save31(2,indst)-10000.D0      
c
c     If this is a rods calculation, we save indst in insv31
c     So, we mark it as a save point for a future curvature calculation.
c
      if (irods.eq.1.or.ivrp.eq.1) then                                 0223JC98
           insv31(icount)=indst                                         0223JC98
           icount=icount+1                                              0223JC98
      endif                                                             0223JC98
c
c     store s and the energy in its array
c
      ss(is)=save31(3,indst)
      s=ss(is)
      vs(is)=save31(4,indst)
      v=vs(is)
c
c     calculate and save the moment of inertia
c
      do i=1,n3tm
        x(i)=save31(i+4,indst)
        xxs(i,is)=x(i)
      enddo
      call center(5,0)
      fmoms(is)=1.D0/fmom(5)
cjc
      call rphtrx(n3,amass,x,1)
cjc
c
c     calculate and save the projected frequencies
c
c     1. Read gradients and normalize and mass-weight
c
      do i=1,n3tm
        dx(i)=save31(i+4+n3tm,indst)
      enddo
      call rphdxn(lopt(3),dx,dxn,amass,n)
      dxmag=dxn
      dxnorm=dxn
c
c     2. Read hessian and transform into mass-weighted
c
      ihs=int(save31(2*n3tm+5,indst))
      do i=1,n3tm
        do j=1,n3tm
           F(i,j)=hess31(i,j,ihs)
        enddo
      enddo
      if (iabs(lopt(4)).eq.1) call rphtrf(n,amass,F,1)
c
c     3. Diagonalize
c
      iopf=3
c
      if (irods.eq.1.or.ivrp.eq.1) then                                 1017PF97
         call dorods (3,is)                                             0219PF98
         save31(4,indst)=vs(is)                                         1002JC97
         v=vs(is)                                                       1002JC97
         do i=1,n3tm                                                    0223JC98
            save31(i+4+n3tm,indst)=dx(i)                                0223JC98
         enddo                                                          0223JC98
      end if                                                            1017PF97
c
      if (lgs2(39).ne.0) then
         call icfdiag(6,.false.)                                        0317Yc99
      else
         call fdiag(3)
      endif
c
      if (irods.eq.1.or.ivrp.eq.1) then                                 1017PF97
         call endrods (3)                                               0219PF98
      end if                                                            1017PF97
c
c     Store freqs
c
      if (lgs(34).ne.0) then
         ishft=1
      else if (icode(5).eq.3) then
         ishft=6
      else
         ishft=7
      endif
      nfreq=3*natom-ishft
      do i=1,nfreq
        ws(is,i)=freq(i+ishft)
c
c --- scale all frequencies using FREQSCALE                             0216JZ09
c
        if (ifqfac.eq.1) ws(is,i)=ws(is,i)*freqfac                      0216JZ09
c
      enddo
c
c     sort freq
c
      do i = 1,nfreq
        do j = i+1, nfreq
          if (freq(i+ishft).gt.freq(j+ishft)) then
              tempf=freq(i+ishft)
              freq(i+ishft) = freq(j+ishft)
              freq(j+ishft) = tempf
          endif
        enddo
      enddo
c
         IF (LGS(5).GT.0)  THEN                                         0925JC97
            CALL ANHARM (3)                                             1001JC97
            DO I = 1, NFREQ                                             0925JC97
                 FMIRS(IS,I) = FMOMHR(I+ISHFT)                          0925JC97
            ENDDO                                                       0925JC97
         ENDIF                                                          0925JC97
      call zeropt(iopf)
      do i=1,n3tm
        dxp(i)=dx(i)
      enddo
      if (lopt(4).ne.0) call norout (iopf,dxp)
c
c     If this is not a rods calculation, calculate curvature now.
c     If it is a rods calculation, the curvature is calculated
c     after all the information on the points has been procesed.
c
      if (irods.ne.1.and.ivrp.ne.1) then                                0223JC98
c    
c     1. Look for the points closer to the one we are using
c        with gradients
c
      indl=0
      indr=0
      sl=1.D10
      sr=1.D10
      do 10 i=1,npt31
        if (i.eq.indst) goto 10
        if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0) then
            tmp=save31(2,i)
            if (tmp.lt.9999.D0) then
                if (tmp.gt.999.D0) tmp=tmp-1000.D0
                if (tmp.gt.99.D0) tmp=tmp-100.D0
                if (tmp.lt.9.D0) goto 10
            endif  
            tmp=save31(3,i)-s
            if (dabs(tmp).lt.sl.and.tmp.lt.0.D0) then
                indl=i
                sl=dabs(tmp)
            else if (dabs(tmp).lt.sr.and.tmp.gt.0.D0) then
                indr=i
                sr=dabs(tmp)
            endif
        endif
10    continue
c     
c     indl is the index of the closest point in the reactant side
c     indr is the index of the closest point in the product side
c     if one of them is 0, is the first or last point at the grid
c     if both are 0, something weird happened
c
c     2. Calculate curvature components
c
      if (indr.eq.0.and.indl.eq.0) then
          write (fu6,1000)
          stop 'nost31-1'
      endif
1000  format(1x,'Error: Unable to find the gradients for calculating',
     *' curvature components')
c
      sx(2)=s
      if (indr.ne.0.and.indl.ne.0) then
          nbs=3
          sx(1)=save31(3,indl)
          sx(3)=save31(3,indr)
          do i=1,n3tm
            dx1(i)=save31(n3tm+4+i,indl)
            dx2(i)=save31(n3tm+4+i,indr)
          enddo
          call rphdxn(lopt(3),dx1,dxn,amass,n)
          call rphdxn(lopt(3),dx2,dxn,amass,n)
      else 
          nbs=2
          if (indr.ne.0) then
            sx(3)=save31(3,indr)
            do i=1,n3tm
              dx2(i)=save31(n3tm+4+i,indr)
            enddo
            call rphdxn(lopt(3),dx2,dxn,amass,n)
          else 
            sx(3)=save31(3,indl)
            do i=1,n3tm
              dx2(i)=save31(n3tm+4+i,indl)
            enddo
            call rphdxn(lopt(3),dx2,dxn,amass,n)
          endif
      endif
c                                                                       0814JC98
c     Check if the closest point is the saddle point by checkin a       0814JC98
c     change in the sign. If it happens, set the correspondent S = 0    0814JC98
c     and DX = DXSAD                                                    0814JC98
c                                                                       0814JC98
c     check if the closest point to the right is the saddle point:      0814JC98
c                                                                       0814JC98
      if (sx(3)*s.lt.0) then                                            0814JC98
          sx(3)=0.d0                                                    0814JC98
          do i=1,n3tm                                                   0814JC98
             dx2(i)=dxsad(i)                                            0814JC98
          enddo                                                         0814JC98
      endif                                                             0814JC98
c                                                                       0814JC98
c     check if the closest point to the left is the saddle point:       0814JC98
c                                                                       0814JC98
      if (sx(1)*s.lt.0) then                                            0814JC98
          sx(1)=0.d0                                                    0814JC98
          do i=1,n3tm                                                   0814JC98
             dx1(i)=dxsad(i)                                            0814JC98
          enddo                                                         0814JC98
      endif                                                             0814JC98
c                                                                       0814JC98
      call rphb (nbs,s,n,nfreq,sx,dx1,dx,dx2,cof,bcurv)
c
c     3. Save curvature components
c
      do i=1,nfreq
        if (dabs(bcurv(i)).lt.1.D-8) bcurv(i)=0.D0
        bfs(is,i)=bcurv(i)
      enddo
c
      endif
      is = is+1
      if (is.le.nsl) goto 1
c
c     If this is a rods calculation, we have to calculate the curvature
c     components now, when we have changed all the gradients for the save
c     points into the vectors normal to the dividing surface
c     In this calculation we can only use the save points, since they
c     are updated with the new gradient. The gradient points cannot
c     be used. The save points have save31(2)<0
c
      if (irods.eq.1.or.ivrp.eq.1) then                                 0223JC98
      is=nsf
      do kcount=1,icount-1                                              0223JC98
c
c     Read the useful information and diagonalize (again) the Hessian
c
      indst=insv31(kcount)
      s=save31(3,indst)                                                 0223JC98
      v=save31(4,indst)                                                 0223JC98
      ihs=int(save31(2*n3tm+5,indst))                                   0223JC98
      do i=1,n3tm                                                       0223JC98
         x(i)=save31(i+4,indst)                                         0223JC98
         dx(i)=save31(i+4+n3tm,indst)                                   0223JC98
           do j=1,n3tm                                                  0223JC98
              F(i,j)=hess31(i,j,ihs)                                    0223JC98
           enddo                                                        0223JC98
      enddo                                                             0223JC98
cjc
      call rphtrx(n3,amass,x,1)                                         0224JC98
cjc
      if (iabs(lopt(4)).eq.1) call rphtrf(n,amass,F,1)                   0223JC98
      if (lgs2(39).ne.0) then                                           0223JC98
         call icfdiag(6,.false.)                                        0317Yc99
      else                                                              0223JC98
         call fdiag(3)                                                  0223JC98
      endif                                                             0223JC98
c
c     1. Look for the closest save points
c
      if (kcount.eq.1) then                                             0223JC98
               indl=0                                                   0223JC98
               indr=2                                                   0223JC98
      else if (kcount.eq.icount-1) then                                 0223JC98
               indl=icount-2                                            0223JC98
               indr=0                                                   0223JC98
      else                                                              0223JC98
               indl=kcount-1                                            0223JC98
               indr=kcount+1                                            0223JC98
      endif                                                             0223JC98
c
c     indl is the index of the closest point in the reactant side
c     indr is the index of the closest point in the product side
c     if one of them is 0, is the first or last point at the grid
c     if both are 0, something weird happened
c
      if (indr.eq.0.and.indl.eq.0) then                                 0223JC98
          write (fu6,1000)                                              0223JC98
          stop 'nost31-2'                                               0223JC98
      endif                                                             0223JC98
c
c     2. Calculate curvature components
c
      sx(2)=s                                                           0223JC98
      if (indr.ne.0.and.indl.ne.0) then                                 0223JC98
          nbs=3                                                         0223JC98
          sx(1)=save31(3,insv31(indl))                                  0223JC98
          sx(3)=save31(3,insv31(indr))                                  0223JC98
          do i=1,n3tm                                                   0223JC98
            dx1(i)=save31(n3tm+4+i,insv31(indl))                        0223JC98
            dx2(i)=save31(n3tm+4+i,insv31(indr))                        0223JC98
          enddo                                                         0223JC98
          call rphdxn(lopt(3),dx1,dxn,amass,n)                          0815PJ01
          call rphdxn(lopt(3),dx2,dxn,amass,n)                          0815PJ01
      else                                                              0223JC98
          nbs=2                                                         0223JC98
          if (indr.ne.0) then                                           0223JC98
            sx(3)=save31(3,insv31(indr))                                0223JC98
            do i=1,n3tm                                                 0223JC98
              dx2(i)=save31(n3tm+4+i,insv31(indr))                      0223JC98
            enddo                                                       0223JC98
            call rphdxn(lopt(3),dx2,dxn,amass,n)                        0815PJ01
          else                                                          0223JC98
            sx(3)=save31(3,insv31(indl))                                0223JC98
            do i=1,n3tm                                                 0223JC98
              dx2(i)=save31(n3tm+4+i,insv31(indl))                      0223JC98
            enddo                                                       0223JC98
            call rphdxn(lopt(3),dx2,dxn,amass,n)                        0815PJ01
          endif                                                         0223JC98
      endif                                                             0223JC98
c                                                                       0814JC98
c     Check if the closest point is the saddle point by checkin a       0814JC98
c     change in the sign. If it happens, set the correspondent S = 0    0814JC98
c     and DX = DXSAD                                                    0814JC98
c                                                                       0814JC98
c     check if the closest point to the left is the saddle point:       0814JC98
c                                                                       0814JC98
      if (sx(3)*s.lt.0) then                                            0814JC98
          sx(3)=0.d0                                                    0814JC98
          do i=1,n3tm                                                   0814JC98
             dx2(i)=dxsad(i)                                            0814JC98
          enddo                                                         0814JC98
      endif                                                             0814JC98
c                                                                       0814JC98
c     check if the closest point to the left is the saddle point:       0814JC98
c                                                                       0814JC98
      if (sx(1)*s.lt.0) then                                            0814JC98
          sx(1)=0.d0                                                    0814JC98
          do i=1,n3tm                                                   0814JC98
             dx1(i)=dxsad(i)                                            0814JC98
          enddo                                                         0814JC98
      endif                                                             0814JC98
c                                                                       0814JC98
      call rphb (nbs,s,n,nfreq,sx,dx1,dx,dx2,cof,bcurv)                 0223JC98
c
c     3. Save curvature components
c
      do i=1,nfreq                                                      0223JC98
        if (dabs(bcurv(i)).lt.1.D-8) bcurv(i)=0.D0                       0223JC98
        bfs(is,i)=bcurv(i)                                              0223JC98
      enddo                                                             0223JC98
c
      is = is+1                                                         0223JC98
      if (is.eq.issp) is=is+1                                           0223JC98
      enddo                                                             0223JC98
c
      endif                                                             0223JC98
c
      return
      end subroutine nost31
c***********************************************************************
c     arrsm 
c***********************************************************************
c
      subroutine arrsm(npsm,ispsm,ssm)
      use common_inc, only : x,fmom
      use perconparam
      use rate_const
c
c     JCC   8/10/97
c
c     create an array ssm with all the values of s and m with increasing
c     s
c
      implicit double precision (a-h,o-z)
      dimension ssm(2,npt31)
      alim=1.D-7
      do i=1,2
       do j=1,npt31 
        ssm(i,j)=0.D0
       enddo
      enddo
c
c     calculate the value of m for all the points along the reaction
c     path
c 
      sextr=0.D0
      sextp=0.D0
      do i=1,n3tm
        do j=1,2
         xxext(i,j)=0.D0
        enddo
      enddo
      j=0
      do 10 i=1,npt31
        if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0) then
            tmp=save31(2,i)
            if (tmp.lt.9999.D0) then
                if (tmp.gt.999.D0) tmp=tmp-1000.D0
                if (tmp.lt.99.D0) goto 10
            endif
            j=j+1
            ssm(1,j)=save31(3,i)
            do k=1,n3tm
               x(k)=save31(k+4,i)
            enddo
c
c     look for the geometries closest to reactants and products and put
c     them in the array xxext
c
            if (save31(3,i).lt.sextr) then
               sextr=save31(3,i)
               xxext(1,1)=save31(3,i)
               do k=1,n3tm
                  xxext(k+1,1)=x(k)
               enddo
            endif
            if (save31(3,i).gt.sextp) then
               sextp=save31(3,i)
               xxext(1,2)=save31(3,i)
               do k=1,n3tm
                  xxext(k+1,2)=x(k)
               enddo
            endif
c
            call center(5,0)
            ssm(2,j)=1.D0/fmom(5)
        endif
10    continue
      npsm=j+1
c
c     sort according to s and put the saddle point in the right position
c
      ssm(1,npsm)=0.0D0
      ssm(2,npsm)=fmoms(issp)
      call sort31(npsm,ssm)
c
      do i=1,npsm
         if (dabs(ssm(1,i)).lt.alim) ispsm=i
      enddo
c
      return
      end subroutine arrsm
c***********************************************************************
c     arrsv 
c***********************************************************************
c
      subroutine arrsv(npsv,ispsv,ssv)
      use common_inc
      use perconparam
      use rate_const
c
c     JCC   8/10/97
c
c     create an array ssv with all the values of s and v with increasing
c     s
c
      implicit double precision (a-h,o-z)
      dimension ssv(2,npt31)
      alim=1.D-5
      do i=1,2
       do j=1,npt31 
        ssv(i,j)=0.D0
       enddo
      enddo
c
      j=0
      do 10 i=1,npt31
        if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0) then
            tmp=save31(2,i)
            if (tmp.lt.999.D0) goto 10
            j=j+1
            ssv(1,j)=save31(3,i)
            ssv(2,j)=save31(4,i)
        endif
10    continue
      npsv=j+1
c
c     sort according to s and put the saddle point in the right position
c
      ssv(1,npsv)=0.0D0
      ssv(2,npsv)=vs(issp)
      call sort31(npsv,ssv)
c
      do i=1,npsv
         if (dabs(ssv(1,i)).lt.alim) ispsv=i
      enddo
c
      return
      end subroutine arrsv
c***********************************************************************
c     recs31
c***********************************************************************
c
      subroutine recs31
      use common_inc
      use perconparam
      use rate_const
c
c     JCC   11/17/97
c
c     create an array ssp with all the values of the indexes sorted
c     according to increasing s, with s>0, and another ssn with s<0
c
      implicit double precision (a-h,o-z)
      dimension ssp(2,npt31), ssn(2,npt31), xref(n3tm), xrot(n3tm)
      integer :: idum(natoms)
      do i=1,2
       do j=1,npt31
        ssp(i,j)=0.D0
        ssn(i,j)=0.D0
       enddo
      enddo
      do i = 1,natom
         iatom(i) = i
      enddo
c
c     sort according to s and put the ordered indexes in ssp and ssn
c
      jp=0
      do 10 i=1,npt31
        if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0.and.
     *      save31(3,i).gt.0.0D0) then
            jp=jp+1
            ssp(1,jp)=save31(3,i)
            ssp(2,jp)=i
        endif
10    continue
      call sort31(jp,ssp)
c
      jn=0
      do 20 i=1,npt31
        if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0.and.
     *      save31(3,i).lt.0.0D0) then
            jn=jn+1
            ssn(1,jn)=save31(3,i)
            ssn(2,jn)=i
        endif
20    continue
      call sort31(jn,ssn)
c
c     Now in ssp we have the indexes ordered from the point closer
c     to the saddle point to the point closer to products. Let's
c     calculate the value of s using Chen's algorithm and let's
c     put it on save31(3,i)
c
      do i=1,n3tm
         xref(i)=xr(i,5)
      enddo
      sref=0.D0
      do i=1,jp
          indpnt=ssp(2,i)
          do j=1,n3tm
             xrot(j)=save31(4+j,indpnt)
          enddo
          sval=0.D0; idum=0
          call calcs(xref,xrot,sval,idum)
          sref=sref+sval
          do j=1,n3tm
              xref(j)=xrot(j)
          enddo
          write (fu6,1000) save31(3,indpnt), sref
          save31(3,indpnt)=sref
      enddo
c
c     Now in ssn we have the indexes ordered from the point closer
c     to reactants to the point closer to the saddle point, so we
c     will run an inverted loop and we will
c     calculate the value of s using Chen's algorithm and 
c     put it on save31(3,i)
c
      do i=1,n3tm
         xref(i)=xr(i,5)
      enddo
      sref=0.D0
      do i=jn,1,-1
          indpnt=ssn(2,i)
          do j=1,n3tm
             xrot(j)=save31(4+j,indpnt)
          enddo
          sval=0.D0; idum=0
          call calcs(xref,xrot,sval,idum)
          sref=sref+sval
          do j=1,n3tm
              xref(j)=xrot(j)
          enddo
          write (fu6,1000) save31(3,indpnt), -sref
          save31(3,indpnt)=-sref
      enddo
c
      return
1000  format (3x,'S in fu 31 = ',f10.5,'   S calculated = ', f10.5)
      end subroutine recs31
c***********************************************************************
c     sort31
c***********************************************************************
c
      subroutine sort31(n,ra)
c
c     JCC   8/10/97
c
c     sort the array arr according to its first column (s) 
c     
c     Heapsort method, from numerical recipes in fortran
c
      implicit double precision (a-h,o-z)
      dimension ra(2,n)
c 
      if (n.lt.2) stop 'Sort31-1'
      l=n/2+1
      ir=n
10    continue
      if(l.gt.1)then 
          l=l-1
          rra=ra(1,l)
          rrb=ra(2,l)
      else 
          rra=ra(1,ir)
          rrb=ra(2,ir)
          ra(1,ir)=ra(1,1) 
          ra(2,ir)=ra(2,1) 
          ir=ir-1 
          if(ir.eq.1)then 
                    ra(1,1)=rra 
                    ra(2,1)=rrb
                    return
          endif
      endif
      i=l 
      j=l+l
20    if(j.le.ir)then 
         if(j.lt.ir)then
            if(ra(1,j).lt.ra(1,j+1))j=j+1 
         endif
         if(rra.lt.ra(1,j))then 
            ra(1,i)=ra(1,j)
            ra(2,i)=ra(2,j)
            i=j
            j=j+j
         else 
            j=ir+1
         endif
      goto 20
      endif
      ra(1,i)=rra 
      ra(2,i)=rrb 
      goto 10
      end subroutine sort31
c***********************************************************************
c     splm31
c***********************************************************************
c
       subroutine splm31(npsm,nss,vim,redm,si,vi,ws,ssm,t,rv,lgs6,irepr)
       use perconparam
c
c     JCC   8/10/97
c
C
C     This subroutine calculate the value of 1/I for a given S using
C     a spline under tension fit to the values at the points in
C     fu30 input. The interpolated magnitude is 1/sqrt(I).
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION SI(nsdim),VI(nsdim),S(npt31),V(npt31),SA(npt31)
      DIMENSION WFSP(npt31),WS(nsdim),SSM(2,NPT31),IREPR(8)
C
      ALIM=1.D-6                                                        0114JC98
      N=NPSM+2
C                                                                       0114JC98
C     Look for the saddle point                                         0114JC98
C                                                                       0114JC98
      DO I=2,NPSM-1                                                     0114JC98
         IF (dabs(SSM(1,I)).LT.ALIM) INSP=I                              0114JC98
      ENDDO                                                             0114JC98
C
      V1=0.D0
      V0=VI(NSDIM-2)
      V2=VI(NSDIM)
      S(1)=SI(1)
      S(N)=SI(NSS)
      IF (LGS6.EQ.1) THEN
             WFSP(1)=0.D0
             WFSP(N)=0.D0
      ELSE IF (LGS6.EQ.2) THEN
             WFSP(1)=0.D0
             WFSP(N)=dsqrt(WS(NSS)/SSM(2,INSP))                          0114JC98
      ELSE IF (LGS6.EQ.3) THEN
             WFSP(1)=dsqrt(WS(1)/SSM(2,INSP))                            0114JC98
             WFSP(N)=0.D0
      ELSE IF (LGS6.EQ.4) THEN
             WFSP(1)=dsqrt(WS(1)/SSM(2,INSP))                            0114JC98
             WFSP(N)=dsqrt(WS(NSS)/SSM(2,INSP))                          0114JC98
      ENDIF
      DO I=1,NPSM
         S(I+1)=SSM(1,I)
         WFSP(I+1)=dsqrt(SSM(2,I)/SSM(2,INSP))                           0114JC98
      ENDDO
      if (irepr(7).eq.1) then
            v1=vi(2)
            v(1)=vi(2)
            s(1)=si(2)
            wfsp(1)=dsqrt(ws(2)/SSM(2,INSP))                             0114JC98
      endif
      if (irepr(8).eq.1) then
            v2=vi(nsdim-1)
            s(n)=si(nsdim-1)
            v(n)=vi(nsdim-1)
            wfsp(n)=dsqrt(ws(nsdim-1)/SSM(2,INSP))                       0114JC98
      endif
C
C     Calculation of S0 and TL
C
      A = (V0 - V1)
      B = (V0 - V2)
      WK=(VIM)**2.D0*REDM
      AL1=dsqrt(A/WK)
      AL2=dsqrt(B/WK)
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1 + AL2)/2.D0
      S0=(-AL1+AL2)/2.D0
C
C
C     Values for the asymptotes:
C
      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
                SA(1)=-1.D0
      ELSE
                SA(1)=2./PI*ATAN((S(1)-S0)/TL)
      ENDIF
      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
                SA(N)=1.D0
      ELSE
                SA(N)=2./PI*ATAN((S(N)-S0)/TL)
      ENDIF
C
C     Calculate SA
C
      DO i=2,N-1
        SA(i)=2./PI*ATAN((S(i)-S0)/TL)
      ENDDO
C
C     Now, we will convert the S given into a value of SA and call the
C     spline subroutine
C
        TA=2./PI*ATAN((T-S0)/TL)
        CALL SPL31(SA,WFSP,N,TA,RV,1)
C
C     Calculate the moment of inertia
C
        RV=SSM(2,INSP)*(RV**2.D0)                                       0114JC98
C
      RETURN
      END subroutine splm31
c***********************************************************************
c     splv31
c***********************************************************************
c
      subroutine splv31(npsv,nss,ispsv,issp,vim,redm,ssv,si,
     *                  vi,t,rv,lgs6,xmfr,xmfp,inm31,sinc,irepr)
      use perconparam
c
c     JCC   8/10/97
c
C
C     The first time we enter this subroutine we will calculate the
C     energy at twenty extra points along the reaction path (plus
C     the points in fu31)
C     On the next entrances, it calculates the value of V for a given S
C     using spline under tension of the data calculated previously.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE NMC
      real(8), allocatable :: gs(:),gv(:)
      SAVE GS
      SAVE GV
C
      DIMENSION RL(NPT31), SI(NSDIM), VI(NSDIM), S(NPT31), V(NPT31),
     *SA(NPT31),PFX(3),PFY(3),ADS(25),ADV(25),TMPSA(NPT31),TMPRL(NPT31),
     *GAS(NPT31+25),SSV(2,NPT31),
     *GAV(NPT31+25),IREPR(8)
      if(.not.allocated(gs))then
         allocate(gs(npt31+25),gv(npt31+25)); gs=0.d00; gv=0.d00
      end if
C
C     We start givig values to the energies and S of react, prod and sp.
C
      N=NPSV+2
      NSP=ISPSV+1
      V1=0.D0
      V0=VI(NSDIM-2)
      V2=VI(NSDIM)
      S(1)=SI(1)
      V(1)=VI(1)
      S(N)=SI(NSS)
      V(N)=VI(NSS)
      if (irepr(7).eq.1) then
            v1=vi(2)
            v(1)=vi(2)
            s(1)=si(2)
      endif
      DO I=1,NPSV
        S(I+1)=SSV(1,I)
        V(I+1)=SSV(2,I)
      ENDDO
      if (irepr(8).eq.1) then
            v2=vi(nsdim-1)
            s(n)=si(nsdim-1)
            v(n)=vi(nsdim-1)
      endif
C
C     We calculate S0 and TL and some parameters of an Eckart curve
C
      A = V2 - V1
      C = V1
      B = (2.0*V0-A-2.0*C)+ 2.0 * ((V0-C)*(V0-A-C))**0.5
      A2 = (V0 - V1)
      B2 = (V0 - V2)
      WK=(VIM)**2.D0*REDM
      AL1=dsqrt(A2/WK)
      AL2=dsqrt(B2/WK)
C
C     In order to avoid problems is A2 and B2 are too different (very
C     asymmetric reaction) we limit the value of AL1 and AL2
C
      AL1=DMIN1(AL1,2.D0*AL2)
      AL2=DMIN1(AL2,2.D0*AL1)
      TL=(AL1 + AL2)/2.D0
      S0=(-AL1+AL2)/2.D0
C
C     If this is the first time in this subroutine, we need to calculate
C     the additional values of the energy
C
      IF (NMC.NE.1) THEN
      NMC=1
C
C *** We will start with the calculations for the reactant side of a
C     2->2 or 2->1 reaction
C
      IF ((LGS6.EQ.1.OR.LGS6.EQ.2).AND.IREPR(7).EQ.0) THEN
      DO i=2,NSP-1
        TS=S(i)
        TV=V(i)
C
C     We calculate L using a modified version of the findl
C     subroutine
C
        CALL FINDLV(V1,V0,V2,TS,TV,RANGE)
C
C     We change the variable from S to SA
C
        SA(i)=2./PI*ATAN((S(i)-S0)/TL)
        RL(i)=RANGE
      ENDDO
C
C     For the saddle point calculate L using the imaginary freq
C
      RANGE = dsqrt((2.0D0*V0*(V0-V2))/(REDM*(VIM*VIM)*B))
      SA(NSP)=2./PI*ATAN((S(NSP)-S0)/TL)
      RL(NSP)=RANGE
C
C     L for reactants and products is calculated by linear interpolation
C     of the values of L along the reaction path
C     Reactants:
C
        SA(1)=-1.D0
        RL(1)=DMAX1(RL(2)-(RL(3)-RL(2))/(SA(3)-SA(2))*
     *(SA(2)-SA(1)),0.D0)
C
C     Another option would be a cuadratic fit to the last three points
C
      IF (NSP.ge.4) THEN
         DO in=1,3
           PFX(in)=SA(in+1)
           PFY(in)=RL(in+1)
         ENDDO
         SA(1)=-1.D0
         SEXT=SA(1)
         CALL TREPT (0,PFX,PFY,SEXT,RLEXT)
         RL(1)=DMAX1(RLEXT,0.D0)
      ENDIF
C
C     Now we have a complete set of values of L. We will calculate the new
C     V values from an Eckart function using the L obtained by spline fit
C     to the calculated L.
C
C     We need 10 extra points between the last point in the grid (SA(2)
C     and the reactants (SA(1)=-1).
C
      XSTEP=dabs((SA(2)-SA(1))/11.D0)
      TA=SA(2)
      M=NSP
      DO J=1,M
         TMPSA(J)=SA(J)
         TMPRL(J)=RL(J)
      ENDDO
      DO I=1,10
         TA=TA-XSTEP
C
C     Call the spline subroutine for getting L at that point
C
        CALL SPL31(TMPSA,TMPRL,M,TA,RLS,0)
C
C     Estimate S from the mapped value
C
        TNW=TL*TAN(TA*PI/2.D0)+S0
C
C     We calculate V using an Eckart function and the interpolated L
C
        SE0= -RLS * LOG((A+B)/(B-A))
        RV=ECKART(A,B,C,SE0,RLS,TNW)
C
C     And T and RV will be saved as additional V values
C
        ADS(I)=TNW
        ADV(I)=RV
C
      ENDDO
C
C *** Now, the reactant side for a 1->2 or 1->1 reaction
C
      ELSE IF (IREPR(7).EQ.1.OR.LGS6.EQ.3.OR.LGS6.EQ.4) THEN
C
C     Define SINC as the distance between s(reactants) and a (the last
C     point in the middle region).
C
C     SINC=1.D-2
C
C     Treatment using polynomial in s:
C
      IF (INM31.EQ.1) THEN
C
C     Derivatives and values at the end points (upper and lower)
C
         DVUP=(V(3)-V(2))/(S(3)-S(2))
         DVLW=0.D0
         VUP=V(2)
         SUP=S(2)
         VLW=V(1)+.5*REDM*XMFR*(SINC)**2.D0
         SLW=S(1)+SINC
C
C     Now we have to calculate A0,B1,C2 and D3 for fitting
C     V=A0+B1s+C2s**2+D3s**3
C
         CALL CALCUB (SLW,VLW,DVLW,SUP,VUP,DVUP,A0,B1,C2,D3)
C
C     Using A,B,C and D we calculate V as a cubic polinomial function
C     of S for ten points between the reactant well and the las point
C
         XSTEP=dabs((S(2)-S(1))/11.D0)
         TA=S(2)
         DO I=1,10
            TA=TA-XSTEP
            ADS(I)=TA
            ADV(I)=A0+B1*ADS(I)+C2*ADS(I)**2.D0+D3*ADS(I)**3.D0
         ENDDO
C
C     Treatment using polynomial on z:
C
      ELSE IF (INM31.EQ.2) THEN
C
C     Derivatives and values at the end points (upper and lower)
C
         DVUP=(V(3)-V(2))/(2.D0/PI*ATAN((S(3)-S0)/TL)-
     *                     2.D0/PI*ATAN((S(2)-S0)/TL))
         DVLW=0.D0
         VUP=V(2)
         VLW=V(1)+.5*REDM*XMFR*(SINC)**2.D0
         SUP=2.D0/PI*ATAN((S(2)-S0)/TL)
         SLW=2.D0/PI*ATAN((S(1)+SINC-S0)/TL)
C
C     Now we have to calculate A0,B1,C2 and D3 for fitting
C     V=A0+B1z+C2z**2+D3z**3
C
         CALL CALCUB (SLW,VLW,DVLW,SUP,VUP,DVUP,A0,B1,C2,D3)
C
C     Using A,B,C and D we calculate V as a cubic polinomial function
C     of Z for ten points between the reactant well and the las point
C
         XSTEP=dabs((2.D0/PI*ATAN((S(2)-S0)/TL)
     *         -2.D0/PI*ATAN((S(1)-S0)/TL))/11.D0)
         TA=2.D0/PI*ATAN((S(2)-S0)/TL)
         DO I=1,10
            TA=TA-XSTEP
            ADS(I)=TL*TAN(TA*PI/2.D0)+S0
            ADV(I)=A0+B1*TA+C2*TA**2.D0+D3*TA**3.D0
         ENDDO
C
C     Treatment using no polynomial:
C
      ELSE IF (INM31.EQ.3) THEN
C
         ADV(10)=V(1)+.5*REDM*XMFR*(SINC)**2.D0
         ADS(10)=S(1)+SINC
      ENDIF
C
C *** Now, the product side for a 2->2 or 1->2 reaction
C
      ENDIF
      IF ((LGS6.EQ.3.OR.LGS6.EQ.1).AND.IREPR(8).EQ.0) THEN
C
C     For the saddle point calculate L using the imaginary freq
C
      RANGE = DSQRT((2.0D0*V0*(V0-V2))/(REDM*(VIM*VIM)*B))
      SA(NSP)=2./PI*DATAN((S(NSP)-S0)/TL)
      RL(NSP)=RANGE
C
      DO i=NSP+1,N-1
        TS=S(i)
        TV=V(i)
C
C     We calculate L using a modified version of the findl
C     subroutine
C
        CALL FINDLV(V1,V0,V2,TS,TV,RANGE)
C
C     We change the variable from S to SA
C
        SA(i)=2./PI*DATAN((S(i)-S0)/TL)
        RL(i)=RANGE
      ENDDO
C
C     L for reactants and products is calculated by linear interpolation
C     of the values of L along the reaction path
C     Products:
C
        SA(N)=1.D0
        RL(N)=DMAX1(RL(N-1)-(RL(N-2)-RL(N-1))/
     *(SA(N-2)-SA(N-1))*(SA(N-1)-SA(N)),0.D0)

C
C     Another option would be a cuadratic fit to the last three points
C
      IF ((N-NSP).GE.3) THEN
         DO in=1,3
           PFX(in)=SA(N-in)
           PFY(in)=RL(N-in)
         ENDDO
         SA(N)=1.D0
         SEXT=SA(N)
         CALL TREPT (0,PFX,PFY,SEXT,RLEXT)
         RL(N)=DMAX1(RLEXT,0.D0)
      ENDIF
C
C     No we have a complete set of values of L. We will calculate the new
C     V values from an Eckart function using the L obtained by spline fit
C     to the calculated L.
C
C     We need 10 extra points between the last point in the grid (SA(N-1))
C     and the products (SA(N)=1)
C
C

      XSTEP=dabs((SA(N)-SA(N-1))/11.D0)
      TA=SA(N-1)
      M=N-NSP+1
      DO J=1,M
           TMPSA(J)=SA(NSP+J-1)
           TMPRL(J)=RL(NSP+J-1)
      ENDDO
      DO I=1,10
         TA=TA+XSTEP
C
C     Call the spline subroutine for getting L at that point
C
         CALL SPL31(TMPSA,TMPRL,M,TA,RLS,0)
C
C     Estimate S from the mapped value
C
         TNW=TL*TAN(TA*PI/2.D0)+S0
C
C     We calculate V using an Eckart function and the interpolated L
C
         SE0= -RLS * LOG((A+B)/(B-A))
         RV=ECKART(A,B,C,SE0,RLS,TNW)
C
C     And T and RV will be saved as additional V values
C
         ADS(25-I)=TNW
         ADV(25-I)=RV
C
      ENDDO



C
C *** Now, the product side for a 2->1 or 1->1 reaction
C
      ELSE IF (IREPR(8).EQ.1.OR.LGS6.EQ.2.OR.LGS6.EQ.4) THEN
C
C     Define SINC as the distance between s(products) and a (the last
C     point in the middle region).
C
C     SINC=1.D-3
C
C     Treatment using polynomial in s:
C
      IF (INM31.EQ.1) THEN
C
C     Derivatives and values at the end points (upper and lower)
C
         DVUP=(V(N-2)-V(N-1))/(S(N-2)-S(N-1))
         DVLW=0.D0
         VUP=V(N-1)
         SUP=S(N-1)
         VLW=V(N)+.5*REDM*XMFP*(SINC)**2.D0
         SLW=S(N)-SINC
C
C     Now we have to calculate A0,B1,C2 and D3 for fitting
C     V=A0+B1s+C2s**2+D3s**3
C
         CALL CALCUB (SLW,VLW,DVLW,SUP,VUP,DVUP,A0,B1,C2,D3)
C
C     Using A,B,C and D we calculate V as a cubic polinomial function
C     of S for ten points between the reactant well and the las point
C
         XSTEP=dabs((S(N-1)-S(N))/11.D0)
         TA=S(N-1)
         DO I=1,10
           TA=TA+XSTEP
           ADS(25-I)=TA
           ADV(25-I)=A0+B1*ADS(25-I)+C2*ADS(25-I)**2.D0+D3*ADS(25-I)**3.D0
         ENDDO
C
C     Treatment using polynomial on z:
C
      ELSE IF (INM31.EQ.2) THEN
C
C     Derivatives and values at the end points (upper and lower)
C
         DVUP=(V(N-2)-V(N-1))/(2.D0/PI*ATAN((S(N-2)-S0)/TL)-
     *                     2.D0/PI*ATAN((S(N-1)-S0)/TL))
         DVLW=0.D0
         VUP=V(N-1)
         SUP=2.D0/PI*ATAN((S(N-1)-S0)/TL)
         VLW=V(N)+.5*REDM*XMFR*(SINC)**2.D0
         SLW=2.D0/PI*ATAN((S(N)-SINC-S0)/TL)
C
C     Now we have to calculate A0,B1,C2 and D3 for fitting
C     V=A0+B1z+C2z**2+D3z**3
C
         CALL CALCUB (SLW,VLW,DVLW,SUP,VUP,DVUP,A0,B1,C2,D3)
C
C     Using A,B,C and D we calculate V as a cubic polinomial function
C     of Z for ten points between the reactant well and the las point
C
         XSTEP=dabs((2.D0/PI*DATAN((S(N-1)-S0)/TL)
     *         -2.D0/PI*DATAN((S(N)-S0)/TL))/11.D0)
         TA=2.D0/PI*DATAN((S(N-1)-S0)/TL)
         DO I=1,10
            TA=TA+XSTEP
            ADS(25-I)=TL*DTAN(TA*PI/2.D0)+S0
            ADV(25-I)=A0+B1*TA+C2*TA**2.D0+D3*TA**3.D0
         ENDDO
C
C     Treatment using no polynomial:
C
      ELSE IF (INM31.EQ.3) THEN
C
         ADV(15)=V(N)+.5*REDM*XMFP*(SINC)**2.D0
         ADS(15)=S(N)-SINC
      ENDIF
c
      ENDIF
C
C *** Reorder and merge the original and additional information
C
      GS(1)=S(1)
      GV(1)=V(1)
      DO I=1,10
       GS(I+1)=ADS(11-I)
       GV(I+1)=ADV(11-I)
      ENDDO
      DO I=1,N-2
       GS(I+11)=S(I+1)
       GV(I+11)=V(I+1)
      ENDDO
      DO I=1,10
       GS(N+9+I)=ADS(25-I)
       GV(N+9+I)=ADV(25-I)
      ENDDO
      GS(N+20)=S(N)
      GV(N+20)=V(N)
C
      ENDIF

C
C     Now we will interpolate V using splines based on both the
C     original and the aditional data
C
      IF ((LGS6.EQ.1.AND.IREPR(7).EQ.0).OR.
     *   (LGS6.EQ.2.AND.IREPR(7).EQ.0)) THEN
                GAS(1)=-1.D0
                GAV(1)=GV(1)
      ELSE
                GAS(1)=2./PI*DATAN((GS(1)-S0)/TL)
                GAV(1)=GV(1)
      ENDIF
      IF ((LGS6.EQ.1.AND.IREPR(8).EQ.0).OR.
     *   (LGS6.EQ.3.AND.IREPR(8).EQ.0)) THEN
                GAS(N+20)=1.D0
                GAV(N+20)=GV(N+20)
      ELSE
                GAS(N+20)=2./PI*DATAN((GS(N+20)-S0)/TL)
                GAV(N+20)=GV(N+20)
      ENDIF
      DO I=2,N+19
        GAS(I)=2./PI*DATAN((GS(i)-S0)/TL)
        GAV(I)=GV(I)
      ENDDO
C
C     if the reaction has one reactant or/and one product and nopoly
C     is chosen, eliminate the 9 spaces in the array
C
      NNSPLV=N+20
      IF ((LGS6.EQ.3.OR.LGS6.EQ.4.OR.IREPR(7).EQ.1).AND.INM31.EQ.3) THEN
         NNSPLV=NNSPLV-9
         DO I=3,NNSPLV
         GAS(I)=GAS(I+9)
         GAV(I)=GAV(I+9)
         ENDDO
      ENDIF
      IF ((LGS6.EQ.2.OR.LGS6.EQ.4.OR.IREPR(8).EQ.1).AND.INM31.EQ.3) THEN
         GAS(NNSPLV-10)=GAS(NNSPLV-1)
         GAV(NNSPLV-10)=GAV(NNSPLV-1)
         GAS(NNSPLV-9)=GAS(NNSPLV)
         GAV(NNSPLV-9)=GAV(NNSPLV)
         NNSPLV=NNSPLV-9
      ENDIF
C
      TA=2./PI*ATAN((T-S0)/TL)
      CALL SPL31(GAS,GAV,NNSPLV,TA,RV,1)
C
      RETURN
      END subroutine splv31
c***********************************************************************
c     spl31
c***********************************************************************
c
      subroutine spl31(x,y,n,s,v,idat)
      use perconparam
c
c     JCC   8/10/97
c
C     The savarr array is now using idat+1 instead of idat because      1020BE05
C     idat could equal 0
C
C     This subroutine is a driver between the Polyrate subroutines and
C     the TSPACK subroutines
C     For information about the meaning of the parameters, see the
C     header of the TSPSS subroutine
C
      implicit double precision(A-H,O-Z)
      save ipas
      real(8), allocatable :: savarr(:,:,:)
      save savarr
      real(8), intent(inout) ::  x(npt31*2),y(npt31*2)
C
      real(8), allocatable :: w(:),wk(:),sigma(:),ys(:),yp(:)
      integer :: ipas(5)
      logical unif
      logical per
      if(.not.allocated(savarr))then
        allocate(savarr(2,2,npt31*2)); savarr=0.d00
      end if
      allocate(w(npt31*2),wk(22*npt31),sigma(npt31*2),ys(npt31*2),
     1yp(npt31*2))
c
c     We will save the data from tspss so that the program only goes
c     trough that subroutine once for each set of data
c
      if (idat.ne.0) then
         if (idat.eq.ipas(idat)) goto 10
         ipas(idat)=idat
      endif
c
c     Parameters for tspss
c
      unif=.false.
      per=.false.
      err=1.0D-5
      do i=1,n
         w(i)=1./(err**2.)
         sigma(i)=0.
         ys(n)=0.
         yp(n)=0.
      enddo
      sm=n
      smtol=dsqrt(2./sm)
      lwk=11*n
c
c     Parameters for tspsi
c
      ncd=2
c     iendc=0
      iendc=3
c
c     option 1:
c     Evaluation of the first derivatives (yp) and the abcissae (ys)
c
c        call tspss(n,x,y,per,unif,w,sm, smtol,lwk,
c    *              wk,sigma,ys,yp,
c    *              nit,ier)
c
c     Calculation of the desired value
c
c        v=hval(s,n,x,ys,yp,sigma,ier)
c
c     option 2:
c     Evaluation of the first derivatives (yp) and the tension factors (sigma)
c
         call tspsi(n,x,y,ncd,iendc,per,unif,lwk,wk,
     *              yp,sigma,ier)
c
c     Store yp and sigma
c
         do i=1,n
            savarr(idat+1,1,i)=yp(i)
            savarr(idat+1,2,i)=sigma(i)
         enddo
         goto 20
c
c set yp and sigma to the appropriate arrays
c
10       do i=1,n
            yp(i)=savarr(idat+1,1,i)
            sigma(i)=savarr(idat+1,2,i)
         enddo
c     Calculation of the desired value
c
20       v=hval(s,n,x,y,yp,sigma,ier)
c
         deallocate(w,wk,sigma,ys,yp)
         return
         end subroutine spl31

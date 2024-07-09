      program rotate
c
c By JCC, using pieces of code by JVF and some Polyrate subroutines
c
c This program read from a fu31 input file plus an additional input file
c with the masses and number of atoms and perform a rotation of the geometries
c and a calculation of the straight segment that separates these 
c geometries, using the rotation procedure by Chen, TCA 1989,75,481
c
c additional input (unit 5):
c  -first line: number of atoms (natom) and number of atoms in:
c               reactant 1, reactant 2, product 1, product 2, reactant well, 
c               and product well (7 numbers, 0 if any species does not exist)
c  -second line: reduced mass (redm) and mass of all the atoms in amu (sckk)
c  -third line: 1 if you want to correct the values of s; 0 if you
c               want to keep the same values as in poly.fu31
c  -next lines: atomic numbers and coordinates of the reference point,
c               usually the saddle point (there must be four numbers 
c               per line)
c
      implicit double precision (a-h,o-z)
      character * 80 string
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
      dimension xsp(natoms,3)
      dimension rotas(3,3),rotaf(n3tm,n3tm)
      dimension tmpf1(n3tm,n3tm),tmpf2(n3tm,n3tm)
      dimension srsp(nsdim)
      dimension xx1(n3tm),xx2(n3tm),xxsp(n3tm),sckk(n3tm)
      dimension kkk(natoms),ksp(natoms)
      dimension tmph(n3tm*n3tm), tmpd(n3tm)
c 
c define input and output files
c
      open (unit=fu5, file = "poly.fu5", status="old")
      open (unit=fu31, file = "poly.fu31", status="old")
      open (unit=fu6, file = "poly.fu6", status="unknown")
      open (unit=fu60, file = "poly.fu60", status="unknown")
c
      read (fu5,*) natom, (nratom(i), i=1,6)
      n=natom
      n3=3*n
      read (fu5,*) redm,(sckk(j),j=1,n)
      read (fu5,*) ichng
c
c initialize some important variables
c
      iop=1
c
c (iop is a variable kept for the sake of consistency with previous versions
c of the code. The only possible value is 1)
c
      nratom(7)=nratom(5)
      nratom(8)=nratom(6)
      nratom(5)=natom
      nratom(6)=natom
c
c set lgs(6)
c
      lgs(6)=1
      if (nratom(2).eq.0) lgs(6)=lgs(6)+2
      if (nratom(4).eq.0) lgs(6)=lgs(6)+1
c
      do i=1,n
         iatom(i)=i
         do j=3,1,-1
         amass(3*i-j+1)=sqrt(sckk(i)/redm)
         end do
      end do
c
c read reference point from unit 5 (generally the SP of the reaction)
c
      do j=1,n
         read (fu5,*) ksp(j),(xsp(j,i),i=1,3)
         do i=1,3
            xxsp(3*j-(3-i))=xsp(j,i)
         end do
      end do
c
c write some stuff in the log file
c
      write (fu6,200)
      write (fu6,210)
      write (fu6,340) natom
c
      write (fu6,354)
      write (fu6,400)
      write (fu6,450) (xxsp(i),i=1,3*n)
c
c now read fu31
c
      call ropt31 (string,istrt)
c
c echo the information for the reactants, products, and wells, 
c and the saddle point.
c
      call rall31 (string,istrt,npos,nneg,NEPTS,NENEG,NEPOS,
     *             NGPTS,NGNEG,NGPOS)
      do 1040 j=1,8
         if (j.eq.6) goto 1040
         if (nratom(j).ne.0) then
            write (fu60,*) 
            if (j.eq.1) write(fu60,*) '*REACT1'
            if (j.eq.2) write(fu60,*) '*REACT2'
            if (j.eq.3) write(fu60,*) '*PROD1'
            if (j.eq.4) write(fu60,*) '*PROD2'
            if (j.eq.5) write(fu60,*) '*SADDLE'
            if (j.eq.7) write(fu60,*) '*WELLR'
            if (j.eq.8) write(fu60,*) '*WELLP'
            write (fu60,*) 
            do i=1,npt31
               if (nint(save31(1,i)).eq.j) then
                  v = save31(4,i)
                  k = nint(save31(2*n3tm+5,i))
                  do k1=1,3*nratom(j)
                     do k2=1,3*nratom(j)
                        f(k1,k2)=hess31(k1,k2,k)
                     enddo
                  enddo
                  goto 1050
               endif
            enddo
1050        if ((j.eq.4).or.(j.eq.3.and.nratom(4).eq.0)) then
               write (fu60,*) 'ENERXN     ', V
               write (fu60,*)
            endif
            if (j.eq.5) then
               write (fu60,*) 'ENESAD     ', V
               write (fu60,*)
            endif
            if (j.eq.7) then
               write (fu60,*) 'ENEWELLR   ', V
               write (fu60,*)
            endif
            if (j.eq.8) then
               write (fu60,*) 'ENEWELLP   ', V
               write (fu60,*)
            endif
            if (nratom(j).ne.1) then
               write (fu60,*) 'HESSIAN'
               if (lopt(4).lt.0) then
                  do k1=1,3*nratom(j)
                     write (fu60,'(4E20.10)') (F(k1,k2), k2=1,k1)
                  enddo
               else
                  do k1=1,3*nratom(j)
                     write (fu60,'(4E20.10)')(F(k1,k2),k2=1,3*nratom(j))
                  enddo
               endif
               write (fu60,*) 'END'
               write (fu60,*)
            endif
         endif
1040  continue
c
c set the number of points
c
      write (fu6,800) npts
      write (fu6,810) nneg
      write (fu6,820) npos
      write (fu6,840) nepts
      write (fu6,810) neneg
      write (fu6,820) nepos
      np = nepts
      npr = NENEG
      npp = NEPOS
      if (np.gt.nsdm) then
         write (fu6,*) 'Error: NSDM parameter is too small.'
         write (fu6,*) 'It has to be larger than the number of points'
         write (fu6,*) 'in poly.fu31. Change it in the param.inc file'
         write (fu6,*) 'and recompile.'
         stop 'low nsdm'
      endif
c
c take the reference geometry
c
      write (fu6,*) 'Reorienting the reactant side'
      do j=1,natom
         kkk(j)=ksp(j)
         do i=1,3
            xx1(3*j-(3-i))=xsp(j,i)
         end do
      end do
      write (fu6,355) 0
      write (fu6,400)
      write (fu6,450) (xx1(i),i=1,n3)
c
c reorient points until npr is reached. I
c
      sr = 0.d0
      do k=1,npr
         sint=0.0d0
         if (k.le.npr) write (fu6,360) k
         write (fu6,400)
         ipoint=0
         spoint=-1.D10
         do j=1,npoint
            if (nint(save31(1,j)).eq.6.and.save31(2,j).gt.0.and
     *          .save31(3,j).lt.0.d0) then
               if (save31(3,j).gt.spoint) then
                   spoint=save31(3,j)
                   ipoint=j
               endif
            endif
         enddo
         save31(2,ipoint)=-save31(2,ipoint)
         do j=1,n3
            xx2(j)=save31(4+j,ipoint)
         end do
         if (lopt(3).eq.1.or.lopt(3).eq.-2) then
            do j=1,n3
               xx2(j)=xx2(j)/amass(j)
            end do
         endif
         write (fu6,450) (xx2(i),i=1,3*n)
c
c call the subroutine that computes s differences
c
         call calcs (xx1,xx2,sval,rotas,kkk)
         sr = sr - sval
         if (ichng.eq.1) save31(3,ipoint) = sr
         srsp(k)=sval
         do i1=1,3
            do i2=1,3
               i3 = ((i1-1)*3+i2)
               efndt(i3,ipoint)=rotas(i1,i2)
            enddo
         enddo
         write (fu6,500) srsp(k)
         write (fu6,550) sr
         write (fu6,*) "Rotated geometry:"
         write (fu6,400)
         write (fu6,450) (xx2(i),i=1,n3)
         write (fu6,*) 
         write (fu6,*) "Rotation matrix:"
         write (fu6,450) (efndt(i,ipoint),i=1,9)
         write (fu6,*) 
         do i=1,3*n
            xx1(i)=xx2(i)
         end do
         if (lopt(3).eq.1.or.lopt(3).eq.-2) then
            do i=1,n3
               xx2(j)=xx2(j)*amass(j)
            end do
         endif
         if (iugm31.eq.1) then     
            do j=1,n3
               xx2(j)=xx2(j)/1.88972652D0
            end do
         endif      
         do j=1,n3
            save31(4+j,ipoint)=xx2(j)
         end do
c
c write in fu60 the information about this points:
c 
         write (fu60,*)
         write (fu60,*) "*POINT"
         write (fu60,*)
c write recalculated s and v
         write (fu60,'(a8,5x,f16.10)') '   SMEP    ', save31(3,ipoint)
         write (fu60,'(a8,5x,f16.10)') '   VMEP    ', save31(4,ipoint)
         write (fu60,*)
c write geometry
         write (fu60,*) '   GEOM'
         write (fu60,'(3(3x,f16.10))') (xx2(i),i=1,n3)
         write (fu60,*) '   END'
         write (fu60,*)
c rotate gradient
         i=nint(-save31(2,ipoint))
         if (i.gt.10000) i=i-10000
         if (i.gt.1100) i=i-1100
         if (i.gt.1000) i=i-1000
         if (i.ge.10) then
            do i=1,natom
               do j=1,3
                   sum =0.0d0
                   do l=1,3
                       idum = n3tm+4+(3*(i-1)+l)
                       sum = sum + rotas(j,l)*save31(idum,ipoint)
                   end do
                   idum = 3*(i-1)+j
                   tmpd(idum)=sum
               end do
            enddo
            do i=1,n3
               save31(n3tm+4+i,ipoint)=tmpd(i)
            enddo
c write gradient
            write (fu60,*) '   GRADS'
            write (fu60,'(3(3x,f16.10))') 
     *            (save31(n3tm+4+i,ipoint),i=1,n3)
            write (fu60,*) '   END'
            write (fu60,*)
         endif
c rotate hessian
         i=nint(-save31(2,ipoint))
         if (i.gt.10000) then
            mpoint = nint(save31(2*n3tm+5,ipoint))
            do i=1,n3
               do j=1,n3
                  rotaf(i,j)=0.d0
               enddo
            enddo
            do i=1,natom
               idum=(i-1)*3
               do j=1,3
                  do l=1,3
                     rotaf(idum+j,idum+l)=rotas(j,l)
                  enddo
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  tmpf1(i,j)=0.d0
                  tmpf2(i,j)=0.d0
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  do l=1,n3
                     tmpf1(i,j)=tmpf1(i,j)+rotaf(i,l)*hess31(l,j,mpoint)
                  enddo
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  do l=1,n3
                     tmpf2(i,j)=tmpf2(i,j)+tmpf1(i,l)*rotaf(j,l)
                  enddo
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  hess31(i,j,mpoint)=tmpf2(i,j)
               enddo
            enddo
c write hessian
            write (fu60,*) '   HESSIAN'
            if (lopt(4).lt.0.or.lopt(4).eq.99) then
               do i=1,n3
                  write (fu60,'(4E20.10)') (hess31(i,j,mpoint), j=1,i)
               enddo
            else
               do i=1,n3
                  write (fu60,'(4E20.10)') (hess31(i,j,npoint), j=1,n3)
               enddo
            endif
            write (fu60,*) '   END'
            write (fu60,*)
         endif
c
c end of the loop
c
      end do
c
c take the reference geometry
c
      write (fu6,*) 'Reorienting the product side'
      do j=1,n
         kkk(j)=ksp(j)
         do i=1,3
            xx1(3*j-(3-i))=xsp(j,i)
         end do
      end do
      write (fu6,355) 0
      write (fu6,400)
      write (fu6,450) (xx1(i),i=1,3*n)
c
c reorient points until npp is reached.
c
      sp = 0.d0
      do k=1,npp
         sint=0.0d0
         if (k.le.npp) write (fu6,360) k
         write (fu6,400)
         ipoint=0
         spoint=1.D10
         do j=1,npoint
            if (nint(save31(1,j)).eq.6.and.save31(2,j).gt.0.and
     *          .save31(3,j).gt.0.d0) then
               if (save31(3,j).lt.spoint) then
                   spoint=save31(3,j)
                   ipoint=j
               endif
            endif
         enddo
         save31(2,ipoint)=-save31(2,ipoint)
         do j=1,n3
            xx2(j)=save31(4+j,ipoint)
         end do
         if (lopt(3).eq.1.or.lopt(3).eq.-2) then
            do j=1,n3
               xx2(j)=xx2(j)/amass(j)
            end do
         endif
         write (fu6,450) (xx2(i),i=1,3*n)
c
c call the subroutine that computes s differences
c
         call calcs (xx1,xx2,sval,rotas,kkk)
         sp = sp + sval
         if (ichng.eq.1) save31(3,ipoint) = sp
         srsp(k)=sval
         do i1=1,3
            do i2=1,3
               i3 = ((i1-1)*3+i2)
               efndt(i3,ipoint)=rotas(i1,i2)
            enddo
         enddo
         write (fu6,500) srsp(k)
         write (fu6,550) sp
         write (fu6,*) "Rotated geometry:"
         write (fu6,400) 
         write (fu6,450) (xx2(i),i=1,3*n)
         write (fu6,*) 
         write (fu6,*) "Rotation matrix:"
         write (fu6,450) (efndt(i,ipoint),i=1,9)
         write (fu6,*) 
         do i=1,3*n
            xx1(i)=xx2(i)
         end do
         if (lopt(3).eq.1.or.lopt(3).eq.-2) then
            do i=1,n3
               xx2(j)=xx2(j)*amass(j)
            end do
         endif
         if (iugm31.eq.1) then     
            do j=1,n3
               xx2(j)=xx2(j)/1.88972652D0
            end do
         endif      
         do j=1,n3
            save31(4+j,ipoint)=xx2(j)
         end do
c
c write in fu60 the information about this points:
c 
         write (fu60,*)
         write (fu60,*) "*POINT"
         write (fu60,*)
c write recalculated s and v
         write (fu60,'(a8,5x,f16.10)') '   SMEP    ', save31(3,ipoint)
         write (fu60,'(a8,5x,f16.10)') '   VMEP    ', save31(4,ipoint)
         write (fu60,*)
c write geometry
         write (fu60,*) '   GEOM'
         write (fu60,'(3(3x,f16.10))') (xx2(i),i=1,n3)
         write (fu60,*) '   END'
         write (fu60,*)
c rotate gradient
         i=nint(-save31(2,ipoint))
         if (i.gt.10000) i=i-10000
         if (i.gt.1100) i=i-1100
         if (i.gt.1000) i=i-1000
         if (i.ge.10) then
            do i=1,natom
               do j=1,3
                   sum =0.0d0
                   do l=1,3
                       idum = n3tm+4+(3*(i-1)+l)
                       sum = sum + rotas(j,l)*save31(idum,ipoint)
                   end do
                   idum = 3*(i-1)+j
                   tmpd(idum)=sum
               end do
            enddo
            do i=1,n3
               save31(n3tm+4+i,ipoint)=tmpd(i)
            enddo
c write gradient
            write (fu60,*) '   GRADS'
            write (fu60,'(3(3x,f16.10))') 
     *            (save31(n3tm+4+i,ipoint),i=1,n3)
            write (fu60,*) '   END'
            write (fu60,*)
         endif
c rotate hessian
         i=nint(-save31(2,ipoint))
         if (i.gt.10000) then
            mpoint = nint(save31(2*n3tm+5,ipoint))
            do i=1,n3
               do j=1,n3
                  rotaf(i,j)=0.d0
               enddo
            enddo
            do i=1,natom
               idum=(i-1)*3
               do j=1,3
                  do l=1,3
                     rotaf(idum+j,idum+l)=rotas(j,l)
                  enddo
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  tmpf1(i,j)=0.d0
                  tmpf2(i,j)=0.d0
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  do l=1,n3
                     tmpf1(i,j)=tmpf1(i,j)+rotaf(i,l)*hess31(l,j,mpoint)
                  enddo
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  do l=1,n3
                     tmpf2(i,j)=tmpf2(i,j)+tmpf1(i,l)*rotaf(j,l)
                  enddo
               enddo
            enddo
            do i=1,n3
               do j=1,n3
                  hess31(i,j,mpoint)=tmpf2(i,j)
               enddo
            enddo
c write hessian
            write (fu60,*) '   HESSIAN'
            if (lopt(4).lt.0.or.lopt(4).eq.99) then
               do i=1,n3
                  write (fu60,'(4E20.10)') (hess31(i,j,mpoint), j=1,i)
               enddo
            else
               do i=1,n3
                  write (fu60,'(4E20.10)') (hess31(i,j,npoint), j=1,n3)
               enddo
            endif
            write (fu60,*) '   END'
            write (fu60,*)
         endif
c
c end of the loop
c
      end do
c
      write (fu6,600)
      stop
c
200   FORMAT(/,70('*'))
210   FORMAT(/,10X,'ROTATE 1.0, by Corchado & Villa, November 1998')
340   FORMAT(/1X,'There are ',I2,' atoms.')
354   FORMAT(//18X,'*** Reference geometry    ***')
355   FORMAT(//18X,'*** First geometry, point number ',i4,' ***')
360   FORMAT(/18X, '*** Next geometry, point number  ',i4,' ***')
400   FORMAT(/18X,'X',11X,'Y',11X,'Z',/)
450   FORMAT(13x,3F12.6)
500   FORMAT (/,'DISTANCE BETWEEN TWO LAST GEOMETRIES: s=',f10.5)
550   FORMAT (/,'VALUE OF S FOR THE CURRENT POINT: s=',f10.5)
600   FORMAT(/1X,23('*'),' END OF ROTATE OUTPUT ',23('*'))
700   FORMAT(3x,F12.6)
800   FORMAT(/1X,'There are ',I3,' nonstationary hessian points.')
810   FORMAT(1X,I3,' are on the reactant side and')
820   FORMAT(1X,I3,' on the product side.')
830   FORMAT(/1X,'There are ',I3,' nonstationary gradient points.')
840   FORMAT(/1X,'There are ',I3,' nonstationary energy points.')
c
      end
C
C***********************************************************************
C  calcs
C***********************************************************************
C
      SUBROUTINE calcs (xx1,xx2,sval,rotas,kkk)
C
C     this routine computes the distance between two geometries along the
C     DCP following the algorithm proposed by Chen Theor. Chim. Acta 1989,75,481
C
C     on input:
C        -xx1 and xx2 are the reference and the final geometries, respectively
C       
C     on output
C        -sval is the distance between xx1 and the rotated xx2
C
C     CALLED BY:
C                RPHRD2,POLYAT,MAIN
C     CALLS:
C            MXLNEQ
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
C
C
      DIMENSION XX1(n3tm),xx2(n3tm),xscr(n3tm),xx2s(n3tm),xx2b(n3tm)
      dimension a(3,3),b(3),y(3),scr(3,4)
      dimension rota(3,3), rotas(3,3), tmprot(3,3)
      dimension kkk(natoms)
      data tol /1.d-5/
      data nfrac /20/
      data shft /0.2/
C
C     the center of mass of the points is placed on the origin of coordinates
C
      ipp=1
      nend=n3
      
      dist=ssx(xx1,xx2,nend)
      do 10 i=1,nend
         x(i)=xx1(i)
 10   continue
      call center (5)
      do 15 i=1,nend
         xx1(i)=x(i)
 15   continue
      do 20 i=1,nend
         x(i)=xx2(i)
 20   continue      
      call center (5)
      do 30 i=1,nend
         xx2(i)=x(i)
         xx2s(i)=x(i)
         xx2b(i)=x(i)
 30   continue
c
      gdist=ssx(xx1,xx2,nend)
c
c Initialize rotation matrix
c
      do i=1,3
        do j=1,3
          rotas(i,j)=0.d0
        enddo
      enddo
      do i=1,3
          rotas(i,i)=1.d0
      enddo
c
c rotate the molecule in order to get the best initial orientation
c
      alp=0.d0
      bet=0.d0
      gam=0.d0
      do 42 icosal=1,nfrac
         do 44 icosbet=1,nfrac
            do 46 icosgam=1,nfrac
c
c construct the rotation matrix (Euler)
c
               alp=2.d0*pi/dble(nfrac)*dble(icosal-1)
               bet=2.d0*pi/dble(nfrac)*dble(icosbet-1)
               gam=2.d0*pi/dble(nfrac)*dble(icosgam-1)

               cal=cos(alp)
               cbe=cos(bet)
               cga=cos(gam)

               sal=sin(alp)
               sbe=sin(bet)
               sga=sin(gam)

               rota(1,1) =  cal*cga - cbe*sga*sal
               rota(1,2) =  cal*sga + cbe*cga*sal
               rota(1,3) =  sal*sbe
      
               rota(2,1) = -sal*cga - cbe*sga*cal
               rota(2,2) = -sal*sga + cbe*cga*cal
               rota(2,3) =  cal*sbe
  
               rota(3,1) =  sbe*sga
               rota(3,2) = -sbe*cga
               rota(3,3) =  cbe
c
c
c rotate coordinates xx2 
c
               do i=1,3*natom
                  xscr(i)=xx2s(i)
               end do

               do i=1,natom
                  do j=1,3
                     sum =0.0d0
                     do k=1,3
                        idum = 3*(iatom(i)-1)+k
                        sum = sum + rota(j,k)*xscr(idum)
                     end do
                     idum = 3*(iatom(i)-1)+j
                     xx2(idum)=sum
                  end do
               enddo
 
               do i=1,3*natom
                  x(i)=xx2(i)
               end do
               call center(5)
               do i=1,3*natom
                  xx2(i)=x(i)
               end do
               dist=ssx(xx1,xx2,nend)
c
c check for the minimum distance          
c
               df=dist-gdist
               if (df.lt.tol) then
                       gdist = dist
                       do i = 1, 3*natom
                         xx2b(i) = xx2(i)
                       enddo      
                       do i=1,3
                         do j=1,3
                            rotas(i,j) = rota (i,j)
                         enddo
                       enddo
               endif
46          continue
44       continue
42    continue
c
c     set xx2 = xx2b
c
      do i=1,3*natom
         xx2(i)=xx2b(i)
      enddo
c
c compute the parameters for eq (7) in the paper
c
 1    do 35 i=1,3
         b(i)=0.0d0
         do 35 j=1,3
           a(i,j)=0.0d0
 35   continue

      do 90 i=1,natom

         xm=AMASS(3*(IATOM(I)))**2  

         ix = 3*iatom(i)-2
         iy = 3*iatom(i)-1
         iz = 3*iatom(i)

         a(1,1) = a(1,1) + xm*(- xx2(ix)*xx1(ix) - xx2(iy)*xx1(iy))
         a(2,1) = a(2,1) + xm*(- xx2(iz)*xx1(iy) )
         a(3,1) = a(3,1) + xm*(  xx2(iz)*xx1(ix) )

         a(1,2) = a(1,2) + xm*(- xx2(iz)*xx1(iy) )
         a(2,2) = a(2,2) + xm*(- xx2(ix)*xx1(ix) - xx2(iz)*xx1(iz))
         a(3,2) = a(3,2) + xm*(- xx2(iy)*xx1(ix) )

         a(1,3) = a(1,3) + xm*(  xx2(iz)*xx1(ix) )
         a(2,3) = a(2,3) + xm*(- xx2(iy)*xx1(ix) )
         a(3,3) = a(3,3) + xm*(- xx2(iy)*xx1(iy) - xx2(iz)*xx1(iz))

         b(1)   = b(1)   + xm*(  xx2(iy)*xx1(ix) - xx2(ix)*xx1(iy))
         b(2)   = b(2)   + xm*(  xx2(iz)*xx1(ix) - xx2(ix)*xx1(iz))
         b(3)   = b(3)   + xm*(  xx2(iz)*xx1(iy) - xx2(iy)*xx1(iz))

 90   continue
c
c solve the system (6) in the paper
c
      call lin (a,b,y,scr,det,1,3,3,1,3)
c
c ensure convergence
c
      do i=1,3
         if (abs(y(i)).gt.shft) then
            if (y(i).gt.shft)  xxn=shft
            if (y(i).lt.-shft) xxn=-shft
            y(i)=xxn
         end if
      end do
c
c obtain sinus and cosinus of the three rotation angles
c
2     sal = y(1)
      cal = sqrt(1.0d0-sal**2)
      sbe = y(2)
      cbe = sqrt(1.0d0-sbe**2)
      sga = y(3)
      cga = sqrt(1.0d0-sga**2)
      
c
c construct the rotation matrix
c
      rota(1,1) =   cal*cbe
      rota(1,2) = - sal*cga - cal*sbe*sga
      rota(1,3) =   sal*sga - cal*sbe*cga
      
      rota(2,1) =   sal*cbe
      rota(2,2) =   cal*cga - sal*sbe*sga
      rota(2,3) = - cal*sga - sal*sbe*cga
  
      rota(3,1) =   sbe
      rota(3,2) =   cbe*sga
      rota(3,3) =   cbe*cga
c
c rotate coordinates xx2 
c
      do i=1,3*natom
         xscr(i)=xx2(i)
      end do

      do 50 i=1,natom
         do j=1,3
            sum =0.0d0
            do k=1,3
               idum = 3*(iatom(i)-1)+k
               sum = sum + rota(j,k)*xscr(idum)         
            end do
            idum = 3*(iatom(i)-1)+j
            xx2(idum)=sum
         end do
 50   continue
c
c add this rotation matrix to rotas
c
      do i=1,3
         do j=1,3
            tmprot(i,j)=0.d0
         enddo
      enddo
      do i=1,3
         do j=1,3
            do k=1,3
               tmprot(i,j)=tmprot(i,j)+rota(i,k)*rotas(k,j)
            enddo
         enddo
      enddo
      do i=1,3
         do j=1,3
            rotas(i,j)=tmprot(i,j)
         enddo
      enddo
c

      do i=1,3*natom
         x(i)=xx2(i)
      end do
      call center(5)
      dist=ssx(xx1,xx2,nend)
      ipp=ipp+1
      if (ipp.gt.100) then
         write (fu6,*) 'CALCS: failed convergence'
         stop 'CALCS: failed convergence'
      end if
      if (dist.gt.gdist) then
         irecap=irecap+1
         irr=mod((irecap),3)+1
         y(irr)=-y(irr)
         goto 2
      end if
      gdist=dist

c
c this is the normal return
c
      if ((abs(b(1))+abs(b(2))+abs(b(3))).lt.tol) then 
          sval = dist
          return
      endif
      goto 1
      
1000  format (3f10.5)
2000  format (4f10.5)
3000  format (4f15.7)
4000  format (i4,3f10.5)
      end
C

C
C***********************************************************************
C  ssx
C***********************************************************************
C
      FUNCTION  ssx (xx1,xx2,nend)
C
C     this routine computes the distance between two cartesian geometries 
C
C
C     CALLED BY:
c            calcs
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
 
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'

      DIMENSION XX1(n3tm),xx2(n3tm)

      sum=0.0d0
      do i=1,nend 
         sum=sum + (amass(i)*(xx1(i)-xx2(i)))**2
      end do
      ssx=sqrt(sum)

      return
         
      end 

C
C***********************************************************************
C  CENTER
C***********************************************************************
C
      SUBROUTINE center (IOP)
C
C     TRANSLATES CENTER OF MASS OF SYSTEM TO SADDLE POINT OR TO
C     C. O. M. OF A REACTANT OR PRODUCT SPECIES
C     IOP=1 OR 2 FOR REACTANTS, IOP=3 OR 4 FOR PRODUCTS
C     IOP=5 FOR SADDLE POINT
C     ALSO CALCULATES MOMEMT OF INERTIA OR PRODUCT OF ALL 3
C     FOR S. P. OR REACTANT OR PRODUCT
C
C     Include statements were added 6/18/91
C     FORMAT STATEMENTS MODIFIED TO MAKE OUTPUT MORE CLEAR 04/30/92
C
C     CALLED BY:
C                RPHRD2,POLYAT,MAIN
C     CALLS:
C            MXLNEQ
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
C
      LOGICAL LDEBUG                                                    1106YL92
C*
      DIMENSION XCM(3)
C
      LDEBUG = .FALSE.                                                  1106YL92
      LPTBCR = LGS2(15)
      IBEG = 1
      IEND = NATOM
      IF (IOP.LT.5) IEND = NRATOM(IOP)
      TOTM = 0.D00
      DO 10 I = IBEG, IEND
         TOTM = TOTM+AMASS(3*IATOM(I))**2
   10 CONTINUE
      DO 40 K = 1, 3
         SUM = 0.D00
         DO 20 I = IBEG, IEND
            SUM = SUM+X(3*(IATOM(I)-1)+K)*AMASS(3*(IATOM(I)-1)+K)**2
   20    CONTINUE
         XCM(K) = SUM/TOTM
C
C         MOVE C. O. M. OF FULL SYSTEM TO C. O. M. OF SPECIES IOP
C
         DO 30 I = K, N3, 3
            X(I) = X(I)-XCM(K)
   30    CONTINUE
   40 CONTINUE
C     WRITE (FU6,1000) (XCM(K),K=1,3)          
C 
      return
C1000 FORMAT(/1X,'New center of mass is at ',3(1PE15.6),                0610WH94
C    1       /1X,'bohrs with respect to previous origin.')
C
      END
C
C
C***********************************************************************
C  RSPDRV
C***********************************************************************
C
      SUBROUTINE rspdrv (NM,N,A,W,MATZ,Z,FV1,FV2,IERR)
C
C     PATCH ROUTINE USING RSP TO DO DIAGONALIZATION
C     Subroutine name has been changed from 'RST' since Ver 5.0
C
C     CALLED BY:
C                FDIAG, INTPM
C     CALLS:
C            RSPP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)      
C
      INCLUDE 'param.inc'
      INCLUDE 'percon.inc'
C
      DIMENSION A(NM,N),W(N),Z(NM,N),FV1(N),FV2(N)
      DIMENSION B(N3TM*(N3TM+1)/2)
      NV = N*(N+1)/2
      L = 1
      DO 10 J = 1, N
         DO 10 I = 1, J
            B(L) = A(I,J)
            L = L+1
   10 CONTINUE
      CALL RSPP (NM,N,NV,B,W,MATZ,Z,FV1,FV2,IERR)
      RETURN
      END
C
C***********************************************************************
C  RSPP
C***********************************************************************
C
      SUBROUTINE rspp (NM,N,NV,A,W,MATZ,Z,FV1,FV2,IERR)
C
C     CALLED BY:
C                 RSPDRV
C     CALLS:
C                 TQL2,TQLRAT,TRBAK3,TRED3
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(NV),W(N),Z(NM,N),FV1(N),FV2(N)
C
C     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF
C     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK)
C     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED)
C     OF A REAL SYMMETRIC PACKED MATRIX.
C
C     ON INPUT-
C
C        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C        DIMENSION STATEMENT,
C
C        N  IS THE ORDER OF THE MATRIX  A,
C
C        NV  IS AN INTEGER VARIABLE SET EQUAL TO THE
C        DIMENSION OF THE ARRAY  A  AS SPECIFIED FOR
C        A  IN THE CALLING PROGRAM.  NV  MUST NOT BE
C        LESS THAN  N*(N+1)/2,
C
C        A  CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
C        PACKED MATRIX STORED ROW-WISE,
C
C        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF
C        ONLY EIGENVALUES ARE DESIRED,  OTHERWISE IT IS SET TO
C        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS.
C
C     ON OUTPUT-
C
C        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER,
C
C        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO,
C
C        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN
C        ERROR COMPLETION CODE DESCRIBED IN SECTION 2B OF THE
C        DOCUMENTATION.  THE NORMAL COMPLETION CODE IS ZERO,
C
C        FV1  AND  FV2  ARE TEMPORARY STORAGE ARRAYS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DATA ZERO,ONE / 0.0D0,1.0D0 /
      IF (N.LE.NM) GO TO 10
      IERR = 10*N
      GO TO 60
   10 IF (NV.GE.(N*(N+1))/2) GO TO 20
      IERR = 20*N
      GO TO 60
C
   20 CALL TRED3 (N,NV,A,W,FV1,FV2)
      IF (MATZ.NE.0) GO TO 30
C
C     ********** FIND EIGENVALUES ONLY **********
C
      CALL TQLRAT (N,W,FV2,IERR)
      GO TO 60
C
C     ********** FIND BOTH EIGENVALUES AND EIGENVECTORS **********
C
   30 DO 50 I = 1, N
C
         DO 40 J = 1, N
            Z(J,I) = ZERO
   40    CONTINUE
C
         Z(I,I) = ONE
   50 CONTINUE
C
      CALL TQL2 (NM,N,W,FV1,Z,IERR)
      IF (IERR.NE.0) GO TO 60
      CALL TRBAK3 (NM,N,NV,A,N,Z,IERR)
   60 RETURN
C
C     ********** LAST CARD OF RSP **********
C
      END
C***********************************************************************
C  TQL2
C***********************************************************************
C
      SUBROUTINE tql2 (NM,N,D,E,Z,IERR)
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION D(N),E(N),Z(NM,N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,MACHEP
C
C     REAL SQRT,ABS,SIGN
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2,
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND
C     WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD.
C     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
C     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
C     FULL MATRIX TO TRIDIAGONAL FORM.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY,
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
C          THE IDENTITY MATRIX.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1,2,...,IERR-1,
C
C        E HAS BEEN DESTROYED,
C
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C                **********
C
      DATA ZERO,ONE,TWO / 0.0D0,1.0D0,2.0D0 /
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
      MACHEP = TWO**(-46)
C
      IERR = 0
      IF (N.EQ.1) GO TO 160
C
      DO 10 I = 2, N
         E(I-1) = E(I)
   10 CONTINUE
C
      F = ZERO
      B = ZERO
      E(N) = ZERO
C
      DO 110 L = 1, N
         J = 0
         H = MACHEP*(ABS(D(L))+ABS(E(L)))
         IF (B.LT.H) B = H
C
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********
C
         DO 20 M = L, N
            IF (ABS(E(M)).LE.B) GO TO 30
C
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
   20    CONTINUE
C
   30    IF (M.EQ.L) GO TO 100
   40    IF (J.EQ.30) GO TO 150
         J = J+1
C
C     ********** FORM SHIFT **********
C
         L1 = L+1
         G = D(L)
         P = (D(L1)-G)/(TWO*E(L))
C
C        R = SQRT(P*P+ONE)
C
         IF (ABS(P).GT.1.0D+16) THEN
            R = ABS(P)
         ELSE
            R = SQRT(P*P+ONE)
         ENDIF
         D(L) = E(L)/(P+SIGN(R,P))
         H = G-D(L)
C
         DO 50 I = L1, N
            D(I) = D(I)-H
   50    CONTINUE
C
         F = F+H
C
C     ********** QL TRANSFORMATION **********
C
         P = D(M)
         C = ONE
         S = ZERO
         MML = M-L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
         DO 90 II = 1, MML
            I = M-II
            G = C*E(I)
            H = C*P
            IF (ABS(P).LT.ABS(E(I))) GO TO 60
            C = E(I)/P
            R = SQRT(C*C+ONE)
            E(I+1) = S*P*R
            S = C/R
            C = ONE/R
            GO TO 70
   60       C = P/E(I)
            R = SQRT(C*C+ONE)
            E(I+1) = S*E(I)*R
            S = ONE/R
            C = C*S
   70       P = C*D(I)-S*G
            D(I+1) = H+S*(C*G+S*D(I))
C
C     ********** FORM VECTOR **********
C
            DO 80 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S*Z(K,I)+C*H
               Z(K,I) = C*Z(K,I)-S*H
   80       CONTINUE
C
   90    CONTINUE
C
         E(L) = S*P
         D(L) = C*P
         IF (ABS(E(L)).GT.B) GO TO 40
  100    D(L) = D(L)+F
  110 CONTINUE
C
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********
C
      DO 140 II = 2, N
         I = II-1
         K = I
         P = D(I)
C
         DO 120 J = II, N
            IF (D(J).GE.P) GO TO 120
            K = J
            P = D(J)
  120    CONTINUE
C
         IF (K.EQ.I) GO TO 140
         D(K) = D(I)
         D(I) = P
C
         DO 130 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  130    CONTINUE
C
  140 CONTINUE
C
      GO TO 160
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
  150 IERR = L
  160 RETURN
C
C     ********** LAST CARD OF TQL2 **********
C
      END
C
C***********************************************************************
C  TQLRAT
C***********************************************************************
C
      SUBROUTINE tqlrat (N,D,E2,IERR)
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION D(N),E2(N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,MACHEP
C
C     REAL SQRT,ABS,SIGN
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT,
C     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
C     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.
C
C     ON INPUT-
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE
C          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
C          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
C          THE SMALLEST EIGENVALUES,
C
C        E2 HAS BEEN DESTROYED,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C                **********
C
      DATA ZERO,ONE,TWO / 0.0D0,1.0D0,2.0D0 /
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
      MACHEP = TWO**(-46)
C
      IERR = 0
      IF (N.EQ.1) GO TO 140
C
      DO 10 I = 2, N
         E2(I-1) = E2(I)
   10 CONTINUE
C
      F = ZERO
      B = ZERO
      E2(N) = ZERO
C
      DO 120 L = 1, N
         J = 0
         H = MACHEP*(ABS(D(L))+SQRT(E2(L)))
         IF (B.GT.H) GO TO 20
         B = H
         C = B*B
C
C     ********** LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT **********
C
   20    DO 30 M = L, N
            IF (E2(M).LE.C) GO TO 40
C
C     ********** E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
   30    CONTINUE
C
   40    IF (M.EQ.L) GO TO 80
   50    IF (J.EQ.30) GO TO 130
         J = J+1
C
C     ********** FORM SHIFT **********
C
         L1 = L+1
         S = SQRT(E2(L))
         G = D(L)
         P = (D(L1)-G)/(TWO*S)
         R = SQRT(P*P+ONE)
         D(L) = S/(P+SIGN(R,P))
         H = G-D(L)
C
         DO 60 I = L1, N
            D(I) = D(I)-H
   60    CONTINUE
C
         F = F+H
C
C     ********** RATIONAL QL TRANSFORMATION **********
C
         G = D(M)
         IF (G.EQ.ZERO) G = B
         H = G
         S = ZERO
         MML = M-L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
         DO 70 II = 1, MML
            I = M-II
            P = G*H
            R = P+E2(I)
            E2(I+1) = S*R
            S = E2(I)/R
            D(I+1) = H+S*(H+D(I))
            G = D(I)-E2(I)/G
            IF (G.EQ.ZERO) G = B
            H = G*P/R
   70    CONTINUE
C
         E2(L) = S*G
         D(L) = H
C
C     ********** GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST **********
C
         IF (H.EQ.ZERO) GO TO 80
         IF (ABS(E2(L)).LE.ABS(C/H)) GO TO 80
         E2(L) = H*E2(L)
         IF (E2(L).NE.ZERO) GO TO 50
   80    P = D(L)+F
C
C     ********** ORDER EIGENVALUES **********
C
         IF (L.EQ.1) GO TO 100
C
C     ********** FOR I=L STEP -1 UNTIL 2 DO -- **********
C
         DO 90 II = 2, L
            I = L+2-II
            IF (P.GE.D(I-1)) GO TO 110
            D(I) = D(I-1)
   90    CONTINUE
C
  100    I = 1
  110    D(I) = P
  120 CONTINUE
C
      GO TO 140
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
  130 IERR = L
  140 RETURN
C
C     ********** LAST CARD OF TQLRAT **********
C
      END
C          
C***********************************************************************
C  TRBAK3
C***********************************************************************
C
      SUBROUTINE trbak3 (NM,N,NV,A,M,Z,IERR)
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(NV),Z(NM,M)
      DOUBLE PRECISION H,S
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRBAK3,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL SYMMETRIC
C     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
C     SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  TRED3.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A
C          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT,
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANSFORMATIONS
C          USED IN THE REDUCTION BY  TRED3  IN ITS FIRST
C          N*(N+1)/2 POSITIONS,
C
C        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED,
C
C        Z CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED
C          IN ITS FIRST M COLUMNS.
C
C     ON OUTPUT-
C
C        Z CONTAINS THE TRANSFORMED EIGENVECTORS
C          IN ITS FIRST M COLUMNS.
C
C     NOTE THAT TRBAK3 PRESERVES VECTOR EUCLIDEAN NORMS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DATA ZERO / 0.0D0 /
      IF (M.EQ.0) GO TO 60
      IF (N.EQ.1) GO TO 60
C
      DO 50 I = 2, N
         L = I-1
         IZ = (I*L)/2
         IK = IZ+I
         H = A(IK)
         IF (H.EQ.ZERO) GO TO 50
C
         DO 40 J = 1, M
            S = ZERO
            IK = IZ
C
            DO 10 K = 1, L
               IK = IK+1
               S = S+A(IK)*Z(K,J)
   10       CONTINUE
C
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
C
            S = S/H
            IK = IZ
C
            DO 30 K = 1, L
               IK = IK+1
               IF (ABS(H).GT.1.0D0) GO TO 20
               SA = S*A(IK)
               IF (ABS(SA).LT.ABS(H)*1.6D+38) GO TO 20
               IERR = 1111
               RETURN
   20          CONTINUE
               Z(K,J) = Z(K,J)-S*A(IK)/H
   30       CONTINUE
C
   40    CONTINUE
C
   50 CONTINUE
C
   60 RETURN
C
C     ********** LAST CARD OF TRBAK3 **********
C
      END
C
C
C***********************************************************************
C  TRED3
C***********************************************************************
C
      SUBROUTINE tred3 (N,NV,A,D,E,E2)
C
C     CALLED BY:
C                 RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(NV),D(N),E(N),E2(N)
      DOUBLE PRECISION F,G,H,HH,SCALE
C
C     REAL SQRT,ABS,SIGN
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED3,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX, STORED AS
C     A ONE-DIMENSIONAL ARRAY, TO A SYMMETRIC TRIDIAGONAL MATRIX
C     USING ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT-
C
C        N IS THE ORDER OF THE MATRIX,
C
C        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A
C          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT,
C
C        A CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
C          INPUT MATRIX, STORED ROW-WISE AS A ONE-DIMENSIONAL
C          ARRAY, IN ITS FIRST N*(N+1)/2 POSITIONS.
C
C     ON OUTPUT-
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL
C          TRANSFORMATIONS USED IN THE REDUCTION,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO,
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C     ********** FOR I=N STEP -1 UNTIL 1 DO -- **********
C
      DATA ZERO / 0.0D0 /
      DO 90 II = 1, N
         I = N+1-II
         L = I-1
         IZ = (I*L)/2
         H = ZERO
         SCALE = ZERO
         IF (L.LT.1) GO TO 20
C
C     ********** SCALE ROW (ALGOL TOL THEN NOT NEEDED) **********
C
         DO 10 K = 1, L
            IZ = IZ+1
            D(K) = A(IZ)
            SCALE = SCALE+ABS(D(K))
   10    CONTINUE
C
         IF (SCALE.NE.ZERO) GO TO 30
   20    E(I) = ZERO
         E2(I) = ZERO
         GO TO 80
C
   30    DO 40 K = 1, L
            D(K) = D(K)/SCALE
            H = H+D(K)*D(K)
   40    CONTINUE
C
         E2(I) = SCALE*SCALE*H
         F = D(L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE*G
         H = H-F*G
         D(L) = F-G
         A(IZ) = SCALE*D(L)
         IF (L.EQ.1) GO TO 80
         F = ZERO
C
         DO 60 J = 1, L
            G = ZERO
            JK = (J*(J-1))/2
C
C     ********** FORM ELEMENT OF A*U **********
C
            DO 50 K = 1, L
               JK = JK+1
               IF (K.GT.J) JK = JK+K-2
               G = G+A(JK)*D(K)
   50       CONTINUE
C
C     ********** FORM ELEMENT OF P **********
C
            E(J) = G/H
            F = F+E(J)*D(J)
   60    CONTINUE
C
         HH = F/(H+H)
         JK = 0
C
C     ********** FORM REDUCED A **********
C
         DO 70 J = 1, L
            F = D(J)
            G = E(J)-HH*F
            E(J) = G
C
            DO 70 K = 1, J
               JK = JK+1
               A(JK) = A(JK)-F*E(K)-G*D(K)
   70    CONTINUE
C
   80    D(I) = A(IZ+1)
         A(IZ+1) = SCALE*SQRT(H)
   90 CONTINUE
C
      RETURN
C
C     ********** LAST CARD OF TRED3 **********
C
      END
C
C
C***********************************************************************
C  MXLNEQ
C***********************************************************************
C
      SUBROUTINE mxlneq (A,NN,IDA,DETT,JRANK,EPS,IN,MM,NABSM)           9/20DL90
C
C PROGRAMMED BY R. HOTCHKISS, U. COMP. CTR., U. OF MINN., REVISED OCT.73
C      modified for the VAX by Bruce Garrett, Nov. 1980
C*
C*    Dimensions changed by Tom Joseph Jan. 1988.
C*
C*    Solves for the matrix X in C*X = B. C must be a sq. matrix
C*    A(NN,NN+abs(MM)) contains matrices C and B, A(i,j) = C(i,j)
C*    A(i,j+NN) = B(i,j)
C*    IDA - first dim. of A, NN - order of matrix C (ie. # of eqns.)
C*    IN(NN) - work array
C*    M = 0 is used to calculate inverse of C   
C*    Notice that NABSM = NN + ABS(MM)                                  9/20DL90
C     CALLED BY:
C                FIVPT,NEWT,PROJCT,TREPT
C
C
C   INCLUDE FILE ADDED 15/08/91
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      include 'param.inc'                                               15/08/GL91
C
      DOUBLE PRECISION MACHEP
      DIMENSION A(IDA,NABSM),IN(IDA)                                    9/20DL90
C
C     DATA ZERO,ONE/0.0D0,1.0D0/,MACHEP/1.0D35/
C     REVISED BY N.ABUSALBI TO ALLOW A DET OF THE ORDER OF 10**37
C
      DATA ZERO,ONE / 0.0D0,1.0D0 /,MACHEP / 1.0D+38 /
      DATA INTMX / 400000 /
C
C INITIATE SOME LOCAL AND OUTPUT VARIABLES
C
      JRANK = NN
      N = NN
      ID = IDA
      OPS = EPS
C
C error checking
C
      IF (N.GT.0.AND.N.LE.ID) GO TO 10
      WRITE (FU6,1700)
      WRITE (FU6,1000) N,ID
      STOP 'MXLNEQ 1'
   10 IF (ID.GE.N.AND.ID.LT.INTMX) GO TO 20
      WRITE (FU6,1700)
      WRITE (FU6,1100) ID,N,INTMX
      STOP 'MXLNEQ 2'
   20 IF ((ONE+OPS).NE.ONE.AND.OPS.GE.ZERO) GO TO 30
      WRITE (FU6,1700)
      WRITE (FU6,1200) OPS
      STOP 'MXLNEQ 3'
   30 MOD = MM
      NM = ABS(MM)
      IF (NM.LT.INTMX) GO TO 40
      WRITE (FU6,1700)
      WRITE (FU6,1300) MM,INTMX
      STOP 'MXLNEQ 4'
   40 NM = NN+NM
      NMIDA = NM*ID*2
      IF (NMIDA.LT.INTMX) GO TO 50
      WRITE (FU6,1700)
      WRITE (FU6,1400) NMIDA,INTMX
      STOP 'MXLNEQ 5'
   50 CONTINUE
C
C  end of error checking
C CONTINUE TO INITIALIZE
C
      K1 = 1
      NFLAG = 0
      DET = ONE
C
C MOD IS -1 DET ONLY (this version does not have this option)
C        = or > 0 FOR INV, DET AND 0 OR MORE SETS OF LIN EQNS
C        <-1 FOR LIN EQNS ONLY AND DET
C MAIN GAUSS JORDAN LOOP BEGINS
C
      DO 150 K = 1, N
C
C SEARCH FOR LARGEST PIVOT CANDIDATE IN REMAINING LOWER RIGHT SQUARE
C  MATRIX
C
         PIV = ZERO
         L = K
         DO 60 I = K, N
            P = ABS(A(I,K))
            IF (PIV.GE.P) GO TO 60
            PIV = P
            L = I
   60    CONTINUE
C
C PIVOT WITH ABS VALUE PIV AND SUBSCRIPTS L AND M HAS BEEN FOUND
C
         PIVOT = A(L,K)
C
C CONTINUE IF PIV LARGER THAN USER EPS
C
         IF (PIV.GT.OPS) GO TO 80
         IF (EPS.EQ.OPS) JRANK = K-1
C
C EPS TEST FAILED, CHECK FOR ZERO PIVOT
C
         IF (PIV.GT.ZERO) GO TO 70
C
C PIVOT IS ZERO, TERMINATE PROGRAM UNLESS MOD=-1,IE, DET ONLY CASE
C      this version does not have the det only mode
C ZERO PIVOT MEANS ZERO DET AND EXIT IF DET ONLY MODE
C
         WRITE (FU6,1700)
         WRITE (FU6,1500) K
         STOP 'MXLNEQ 6'
C
C ISSUE NON-FATAL MESSAGE, PIV .LE. EPS
C
   70    WRITE (FU6,1700)
         WRITE (FU6,1600) K,EPS
C
C SET OPS TO 0 SO SOLUTION MAY CONTINUE AFTER ERROR MESSAGE
C
         OPS = ZERO
         PIV = PIVOT
C
C CALCULATE DETERMINANT AND CHECK FOR OVERFLOW
C
   80    DET = PIVOT*DET
         IF (ABS(DET).LT.MACHEP) GO TO 90
         WRITE (FU6,1700)
         WRITE (FU6,1800) K,DET
         STOP 'MXLNEQ 7'
C
C RESET LEADING ROW DO INDEX FOR DET ONLY AND LIN EQN ONLY CASE
C
   90    IF (MOD.LT.0) K1 = K
C
C SAVE PIVOT INDEX
C
         IN(K) = L
C
C CHECK FOR ROW INTERCHANGE
C
         IF (L.EQ.K) GO TO 110
         DET = -DET
C
C INTERCHANGE ROW CONTAINING PIVOT AND CURRENT ROW
C ONLY PARTIAL ROWS NEED BE EXCHANGED FOR DET ONLY OR LIN EQN ONLY
C  SINCE LOWER LEFT PARTIALLY FORMED TRIANGLE IS NOT NEEDED
C
         DO 100 J = K1, NM
            Z = A(L,J)
            A(L,J) = A(K,J)
            A(K,J) = Z
  100    CONTINUE
C
C PIVOT ELEMENT IS NOW ON DIAGONAL
C SAVE DIVISION TIME BY USING RECIPROCAL OF PIVOT
C
  110    PIVOT = ONE/PIVOT
C
C PRE-DIVIDE NECESSARY PORTION OF PIVOT ROW
C
         DO 120 J = K1, NM
            A(K,J) = A(K,J)*PIVOT
  120    CONTINUE
C
C SET PIVOT ELEMENT TO ZERO SO MAIN REDUCTION STEP DOESNT OPERATE ON
C  PIVOT ROW
C
         A(K,K) = ZERO
C
C SWEEP THROUGH ALL OR PART OF MATRIX USING KTH ROW, PIVOT ROW, TO
C  REDUCE THE MATRIX
C
         DO 140 I = K1, N
            Z = A(I,K)
            IF (Z.EQ.ZERO) GO TO 140
C
C THIS CHECK NOT ONLY PREVENTS OPERATING ON PIVOT ROW BUT CATCHES
C  OTHER ZEROES IN PIVOT COLUMNS. THESE OTHER ZEROES WOULD LEAVE JTH
C  ROW UNCHANGED IN FOLLOWING LOOP SO CONSIDERABLE TIME MAY BE SAVED BY
C  SKIPPING OPERATION
C
            DO 130 J = K1, NM
               A(I,J) = A(I,J)-Z*A(K,J)
  130       CONTINUE
C
C THE INVERSE IS CREATED IN PLACE BY SUBSTITUTING AN IDENTITY MATRIX
C  COL BY COL, SINCE WE ARE SUBT. THE PIVOT ROW FROM OFF DIAGONAL 0
C  ELEMENTS AT THIS POINT, WE NOW PLACE -A(I,K)/A(K,K) AT THIS POINT IN
C  THE PIVOT COL
C
            A(I,K) = -Z*PIVOT
  140    CONTINUE
C
C SIMILARLY DIVIDING PIVOT ROW BY THE PIVOT IS EQUIVALENT TO PLACING
C  ONE/A(K,K) AT THE PIVOT POINT FOR THE INVERSE
C
         A(K,K) = PIVOT
  150 CONTINUE
      IF (N.EQ.1) GO TO 210
      IF (MOD.GE.0) GO TO 180
C
C BACK SUBSTITUTION FOR LIN EQN ONLY CASE
C
      K1 = K1+1
      DO 170 K = K1, NM
         I = N
         DO 170 L = 2, N
            I1 = I
            I = I-1
            Z = ZERO
            DO 160 J = I1, N
               Z = Z+A(I,J)*A(J,K)
  160       CONTINUE
            A(I,K) = A(I,K)-Z
  170 CONTINUE
      GO TO 210
C
C FINAL REORDERING OF MATRIX
C
  180 K = N
C
C SKIP LAST STEP SINCE NO INTERCHANGE COULD OCCUR THERE
C
      DO 200 J = 2, N
C
C PERFORM INTERCHANGES IN EXACT REVERSE ORDER OF PREVIOUS EXECUTION
C
         K = K-1
C
C ROW INTERCHANGE DURING INVERSION IMPLIES COL INTERCHANGE HERE
C
         M = IN(K)
         IF (M.EQ.K) GO TO 200
C
C COL INTERCHANGE
C
         DO 190 I = 1, N
            Z = A(I,K)
            A(I,K) = A(I,M)
            A(I,M) = Z
  190    CONTINUE
  200 CONTINUE
  210 DETT = DET
      RETURN
C
 1000 FORMAT(11H arg 2, n = ,I10/7X,4H id=,I10/
     *       29H n must be .ge. 1 and .le. id)
 1100 FORMAT(12H arg 3, id = ,I10 /9X,3H n=,I10/
     *       27H id must be .ge. N and .le.,I10)
 1200 FORMAT(13H arg 6, eps = ,1PE13.5/
     *       33H eps must be .ge. zero and finite)
 1300 FORMAT(11H arg 8, m = ,I10/
     *   20H abs(m) must be .le. ,I10 )
 1400 FORMAT(7H size = ,I10/
     *       36H size = id*(n+abs(m))*2 must be .le. ,I10)
 1500 FORMAT(4H k =,I10/
     *       46H at step k a gauss-jordan pivot value was zero)
 1600 FORMAT(4H k = ,I10/6H eps = ,1PE13.5/
     *   50H at step k a gauss-jordan pivot value was .le. eps )
 1700 FORMAT(15H *** mxlneq *** )
 1800 FORMAT(4H k = ,I10/6H det = ,1PE13.5/
     *        27H at step k det is too large )
C
      END
C
C
C***********************************************************************
C     LIN
C***********************************************************************
C
      SUBROUTINE lin(S,B,Y,T,DET,IND,NS,NRB,NCB,NRY)
C
C            SOLVE A SET OF SIMULTANEOUS LINEAR EQUATIONS
C
C                S * Y = B  (SOLVE FOR Y)
C
C            AND EVALUATE THE DETERMINANT OF S.
C
C    QUANTUM CHEMISTRY GROUP - THE UNIVERSITY OF WASHINGTON - SEATTLE
C
C      THE INVERSE OF S(*) WILL BE FOUND IF NCB=0 ON INPUT.
C      ONLY THE DETERMINANT OF S(*) WILL BE FOUND IF NCB=-1 ON INPUT.
C
C   ON INPUT:
C   --------
C        S(*) - ARRAY CONTAINING THE ELEMENTS OF LEFT HAND ARRAY
C        B(*) - ARRAY OF DIMENSION NRB ROWS BY NCB COLUMNS
C               (WHERE NRB IS >= NS)
C        IND  = 0 IF S IS A SYMMETRIC UPPER TRIANGULAR ARRAY.
C             = 1 IF S IS A SQUARE ARRAY
C        NS   - DIMENSION OF S(*), IE. IF S(*) IS SQUARE THIS IS THE
C               NUMBER OF ROWS OR COLUMNS IN S(*).
C        NRB  - NO. OF ROWS OF THE REGION IN WHICH B(*) IS STORED
C        NCB  - NO. OF COLUMNS OF MATRIX B(*)
C        NRY  - NO. OF ROWS DESIRED IN OUTPUT REGION Y(*)
C
C  ON OUTPUT:
C  ---------
C        OUTPUT WILL BE IN SAME FORMAT AS ARRAY S(*)
C        Y - RECTANGULAR ARRAY OF DIMENSION NS BY NCB AND
C             MAY BE STORED IN Y WITH NRY AS THE NO. OF ROWS
C             (AS LONG AS NRY IS GREATER THAN OR EQUAL TO NS)
C   SCRATCH STORAGE
C        T - DIMENSION NS BY (NS+NCB)
C
C    IF S IS STORED AS SQUARE, T(1) AND S(1) MAY COINCIDE.
C      IF SO, S WILL BE DESTROYED.
C    T(NS**2+1) AND B(1) MAY COINCIDE.
C      IF SO, B WILL BE DESTROYED.
C    Y(1) AND B(1) MAY COINCIDE.
C      IF SO, B WILL BE REPLACED BY Y.
C  HENCE,FOR MINIMUM MEMORY USE, ENTER WITH S ALREADY IN SQUARE
C  FORM, B(1)=S(NS**2+1)=Y(1) AND T(1)=S(1)
C
C     CALLED BY:
C              FORMA,VECCON
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION S(1),B(1),Y(1),T(1)
      DATA ZERO/0.0D0/, TENM9/1.0D-10/, ONE/1.0D0/
C
      NDIM=NS
      NRDIM=NRB
      NCOL=NCB
      NRXDIM=NRY
      IERR=0
      ITIME=2
      NMAX=NDIM
      NMIN=NMAX-1
      NMXSQR=NMAX*NMAX
      NST=NMXSQR+1
      NMAXP=NMAX+1
      SMAX=ZERO
C
C     --- TEST FORMAT OF S(*) ---
C
      IF(IND.EQ.0) GO TO 110
C
C     --- MOVE SQUARE S(*) INTO T(*) ---
C
         DO 100 I=1,NMXSQR
            T(I)=S(I)
            SMAX=MAX(SMAX,ABS(S(I)))
100      CONTINUE
         GO TO 140
110   CONTINUE
      K=1
      IDIM=0
      DO 130 I=1,NMAX
         IJ=I
         JI=IDIM+1
         DO 120 J=1,I
            T(IJ)=S(K)
            T(JI)=S(K)
            SMAX=MAX(SMAX,ABS(S(I)))
            JI=JI+1
            K=K+1
            IJ=IJ+NMAX
120      CONTINUE
         IDIM=IDIM+NMAX
130   CONTINUE
140   CONTINUE
C
C     --- SET THRESHOLD FOR SMALL DIAGONAL ELEMENTS ---
C
      THRESH=SMAX*TENM9
      IF(NCOL.GT.0) GO TO 170
      IF(NCOL.LT.0) GO TO 195
C
C     --- INVERSE WANTED, SET UNIT MATRIX INTO T(*) ---
C
         NCT=NMAX+NMAX
         IJ=NCT*NMAX
         DO 150 I=NST,IJ
            T(I)=ZERO
150      CONTINUE
         IDIM=NMXSQR-NMAX
         DO 160 I=1,NMAX
            IDIM=IDIM+NMAXP
            T(IDIM)=ONE
160      CONTINUE
         GO TO 200
C
C     --- SOLUTION TO LINEAR EQUATIONS WANTED.  PUT B(*) INTO T(*) ---
C
170   CONTINUE
      NCT=NMAX+NCOL
      K=NMXSQR
      IDIM=0
      DO 190 I=1,NCOL
         JI=K
         IJ=IDIM
         DO 180 J=1,NMAX
            JI=JI+1
            IJ=IJ+1
            T(JI)=B(IJ)
180      CONTINUE
         K=K+NMAX
         IDIM=IDIM+NRDIM
190   CONTINUE
      GO TO 200
C  ONLY DETERMINANT WANTED
195   CONTINUE
      NCT=NMAX
200   CONTINUE
      DET=ONE
      IN=0
C
C     --- ELIMINATION TO MAKE LOWER TRIANGLE OF S(*) ALL ZEROES ---
C
      IF(NMIN.EQ.0) GO TO 265
         DO 260 I=1,NMIN
            IMAX=I
            JS=I+1
C  SELECT MAXIMUM ELEMENT IN COLUMN AS PIVOT
            DO 210 J=JS,NMAX
               JN=IN+J
               IMN=IN+IMAX
               TDIFF=ABS(T(JN))-ABS(T(IMN))
               IF(TDIFF.GT.ZERO) IMAX=J
210         CONTINUE
            IMIMAX=I-IMAX
            IF(IMIMAX.EQ.0) GO TO 230
C  INTERCHANGE ROWS TO BRING MAXIMUM ELEMENT TO PIVOT POSITION
               JI=I+IN
               DET=-DET
               IMN=IMAX+IN
               DO 220 J=I,NCT
                  X=T(JI)
                  T(JI)=T(IMN)
                  T(IMN)=X
                  JI=JI+NMAX
                  IMN=IMN+NMAX
220            CONTINUE
230         CONTINUE
               II=I+IN
C  IF MAXIMUM ELEMENT IS ZERO, THE MATRIX IS SINGULAR
            IF(T(II).EQ.ZERO) GO TO 998
C  ELIMINATION ON COLUMN I, STARTING IN ROW I+1
            DO 250 J=JS,NMAX
               JI=J+IN
               IF(T(JI).EQ.ZERO) GO TO 250
                  X=-T(JI)/T(II)
                  IJ=II+NMAX
                  IMN=JI+NMAX
                  DO 240 K=JS,NCT
                     T(IMN)=T(IMN)+X*T(IJ)
                     IJ=IJ+NMAX
                     IMN=IMN+NMAX
240               CONTINUE
250         CONTINUE
            IN=IN+NMAX
260      CONTINUE
C  EVALUATE DETERMINANT AS PRODUCT OF DIAGONAL ELEMENTS.
C  TEST IF ANY DIAGONAL ELEMENT IS SMALL.
  265     CONTINUE
            IFSM=0
            J=1
            DO 270 I=1,NMAX
               DET=DET*T(J)
               IF(ABS(T(J)).LE.THRESH) IFSM=J
               J=J+NMAX+1
270         CONTINUE
C  RETURN IF ONLY DETERMINANT WAS WANTED
            IF(NCOL.LT.0)RETURN
            IF(IFSM.NE.0) GO TO 999
            IF(NMIN.EQ.0) GO TO 320
C  ELIMINATE UPPER TRIANGLE OF S
            IN=NMXSQR-NMAX
         DO 310 I=1,NMIN
            IA=NMAXP-I
            II=IN+IA
            IJ=IN+1
            DO 300 J=2,IA
               IF(T(IJ).EQ.ZERO) GO TO 295
                  X=-T(IJ)/T(II)
                  JI=NMXSQR+J-1
                  IMN=NMXSQR+IA
                  DO 290 K=NMAXP,NCT
                     T(JI)=T(JI)+X*T(IMN)
                     JI=JI+NMAX
                     IMN=IMN+NMAX
290               CONTINUE
  295             CONTINUE
                  IJ=IJ+1
300         CONTINUE
            IN=IN-NMAX
310      CONTINUE
C  DIVIDE BY DIAGONALS OF S
320   CONTINUE
      IF(NCOL.NE.0) GO TO 22
      K=1
      IN=0
      IF(IND.EQ.0)GO TO 231
C  SOLVING LINEAR EQUATION, OR S INVERSE, S NOT SYMMETRIC
      JUP=NMAX
      GO TO 232
 22   IN=0
      JUP=NCOL
232   DO 43 I=1,NMAX
      IJ=NMXSQR+I
      II=IN+I
      IX=I
      DO 44 J=1,JUP
      Y(IX)=T(IJ)/T(II)
      IJ=IJ+NMAX
      IX=IX+NRXDIM
  44  CONTINUE
      IN=IN+NMAX
  43  CONTINUE
      RETURN
C  S INVERSE, STORED AS UPPER TRIANGLE SYMMETRIC
231   CONTINUE
      DO 45 I=1,NMAX
      II=IN+I
      JI=NMXSQR+I
      DO 46 J=1,I
      Y(K)=T(JI)/T(II)
      K=K+1
      JI=JI+NMAX
  46  CONTINUE
      IN=IN+NMAX
  45  CONTINUE
      GO TO 1000
C
C      --- ERROR BRANCH ---
C
998   CONTINUE
      IF(NCOL.LT.0) THEN
        DET=ZERO
      ELSE
        WRITE(6,900)
900   FORMAT(1X,52HTHE MAXIMUM ELEMENT IS ZERO. THE MATRIX IS SINGULAR.)
        STOP
      END IF
      GO TO 1000
999   CONTINUE
        WRITE(6,910) T(IFSM),THRESH
910   FORMAT(1X,'A DIAGONAL ELEMENT IS SMALLER THAN THRESHOLD.',/,
     *1X,'ELEMENT =',E12.5,' THRESHOLD =',E12.5)
        STOP
1000  CONTINUE
      RETURN
C...END OF LIN
      END
      function icint(svalue)
c
c   Function which takes a character string which is really a integer and
c   converts it to an integer.  Error checking not possible using ichar is
c   done.  Program will stop if an illegal character is passed via the 
c   string 'svalue'.
c
      implicit double precision (a-h,o-z)
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
c
      character * 80 svalue
c
c     check for negative or positive sign (- or +)
c
      icint = 0
      istart = 1
      isign = +1
      if (svalue(1:1).eq.'+') then
         isign = +1
         istart = 2
      else if(svalue(1:1).eq.'-') then
         isign = -1
         istart = 2
      end if
c
c     now convert each character to an integer - and store in icint
c
      k = istart
      do while (svalue(k:k).ne.' ')
         j = ichar(svalue(k:k)) - ichar('0')
         if (j.lt.0.or.j.gt.9) then
            write(fu60,1000)svalue
            stop
         end if
1000     format(3x,'ERROR: illegal digit in an integer: ',A80)
c
         icint = 10.*icint + float(j)
         k = k+1
      end do
      icint = icint * isign
c
      return
      end
C
      FUNCTION cfloat(STRING)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      INCLUDE 'param.inc'
C
      CHARACTER*80 STRING,NUMBER
      CHARACTER CH
      LOGICAL LEXP,LDEC
C
      LEXP = .FALSE.
      LDEC = .FALSE.
      LENGTH = LEN(STRING)
      IF (LENGTH .EQ. 0) THEN
         CFLOAT = 0.0D0
         RETURN
      ENDIF
C     WRITE(FU6,*) LENGTH,STRING
C
C     Find the first nonblank character
C
      I = 1
10    IF (STRING(I:I) .EQ. ' ' .AND. I .LE. LENGTH) THEN
         I = I + 1
         GOTO 10
      ENDIF
C
C     If it is a blank string set function to zero
C
      IF (I .GT. LENGTH) THEN
         CFLOAT = 0.0D0
         RETURN
      ENDIF
      IBEG = I
C
C     Find the first blank character after the number
C
      I = IBEG+1
20    IF (STRING(I:I) .NE. ' ' .AND. I .LE. LENGTH) THEN
         I = I + 1
         GOTO 20
      ENDIF
      IEND = I-1
C
C     Stripe the blanks before and after the number
C
      NUMBER = STRING(IBEG:IEND)
      LENGTH = IEND - IBEG + 1
C   
C     Make sure there is no blank left
C
      IF (INDEX(NUMBER,' ') .LE. LENGTH) THEN
         WRITE(FU6,1000) STRING
         STOP 'CFLOAT 1'
      ENDIF
C
C     Find the decimal point
C
      IDEC = INDEX(NUMBER,'.')
      IF (IDEC .NE. 0) LDEC = .TRUE.
C
C     Find the exponential symbol
C
      IUE = INDEX(NUMBER,'E')
      ILE = INDEX(NUMBER,'e')
      IUD = INDEX(NUMBER,'D')
      ILD = INDEX(NUMBER,'d')
      ISUM = IUE + ILE + IUD + ILD
      IEXP = MAX0(IUE,ILE,IUD,ILD)
      IF (ISUM .GT. IEXP) THEN
         WRITE(FU6,1000) STRING
         STOP 'CFLOAT 2'
      ENDIF
      IF (IEXP .NE. 0) THEN
         LEXP = .TRUE.
      ELSE
         IEXP = LENGTH + 1
      ENDIF
C
      IF (.NOT. LDEC) IDEC = IEXP
C
C     Get the number before decimal
C
      IBEG = 2
      IF (NUMBER(1:1) .EQ. '+') THEN
         SIGN = 1.0D0
      ELSEIF(NUMBER(1:1) .EQ. '-') THEN
         SIGN = -1.0D0
      ELSE
         SIGN = 1.0D0
         IBEG = 1
      ENDIF
      IF (IBEG .EQ. IEXP) THEN
         F1 = 1.0D0
      ELSE
         F1 = 0.0D0
      ENDIF
      DO 50 I = IBEG,IDEC-1
         CH = NUMBER(I:I)
         IF (CH .GE. '0' .AND. CH .LE. '9') THEN
            N = ICHAR(CH) - ICHAR('0')
            F1 = F1 * 10.0D0 + DBLE(N)
         ELSE
            WRITE(FU6,1000) STRING
            STOP 'CFLOAT 3'
         ENDIF
50    CONTINUE
C
C     Get the number after decimal 
C
      F2 = 0.0D0
      IF (LDEC) THEN
         J = 0
         DO 60 I = IDEC+1,IEXP-1
            CH = NUMBER(I:I)
            IF (CH .GE. '0' .AND. CH .LE. '9') THEN
               N = ICHAR(CH) - ICHAR('0')
               F2 = F2 * 10.0D0 + DBLE(N)
               J = J + 1
            ELSE
               WRITE(FU6,1000) STRING
               STOP 'CFLOAT 4'
            ENDIF
60       CONTINUE
         F2 = F2 / 10.0D0 ** DBLE(J)
      ENDIF
C
C    Get the exponent
C
      ESIGN = 1.0D0
      F3 = 0.0D0
      IF (LEXP) THEN 
         IBEG = IEXP + 2
         IF (NUMBER(IEXP+1:IEXP+1) .EQ. '+') THEN
            ESIGN = 1.0D0
         ELSEIF(NUMBER(IEXP+1:IEXP+1) .EQ. '-') THEN
            ESIGN = -1.0D0
         ELSE
            ESIGN = 1.0D0
            IBEG = IEXP + 1
         ENDIF
         DO 70 I = IBEG,LENGTH
            CH = NUMBER(I:I)
            IF (CH .GE. '0' .AND. CH .LE. '9') THEN
               N = ICHAR(CH) - ICHAR('0')
               F3 = F3 * 10.0D0 + DBLE(N)
            ELSE
               WRITE(FU6,1000) STRING
               STOP 'CFLOAT 5'
            ENDIF
70       CONTINUE
      ENDIF 
C
      CFLOAT = (SIGN * (F1 + F2)) * 10.0D0 ** (ESIGN*F3)
C
      RETURN
C
1000  FORMAT(/1X,'Illegal number: ',A80)
C
      END
c
      subroutine ropt31 (string,istrt)
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
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
      character * 80 string
c
      write(fu60,1000)
1000  format(1x,'*GENERAL',/)
c
      call rline(fu31,string,istrt,isect,iend)
c
      if (isect.ne.1) then
         write(fu6,1100)
1100     format(1x,'error:  first non-comment, non-blank line must be'
     *             ,' a section header that begins with a *')
         stop 'ropt31-1'
      end if
c
      if (string(istrt+1:istrt+6).eq.'genera') then
         call rgen31(string,iend,istrt)
      else
         write(fu6,1200) 
         stop 'ropt31-2'
      endif
c
1200  format(1x,'Error: The general section must be the first present',
     *' in a writefu31 file')
      return
      end
c
      subroutine rline(iunitr,string,istrt,isect,iend)
c
c      This subroutine finds the first non-comment and non-blank line 
c      and the location of the first character on that line.  The contents
c      of the line are placed in the variable string.  It will 
c      then change it to all lower case using the routine 'case'.
c      A flag is set if the line is the start of a new section.
c
c      Modified to read from unit iunitr       0810JC97
c
c      istrt:   location in 80 character string that is the first non-blank
c      isect:   a 1 is returned if the first non-blank character is a * 
c               signalling the start of a section
c      iend:    a 1 is returned if the end of file 5 is encountered
c      string:  80 character string that is the contents of the current line
c
      implicit double precision (a-h,o-z)
c
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
c
      character * 80 string,case
      external case
c
1000  format(A80)
c
c       initialize variables
c
      isect = 0
      iend = 0
      iblnk = 1
c
c     read in next line in file 5 - find first non-blank character and
c     determine if it is a comment (# sign signals a comment).  Once a
c     comment is found the rest of the line is skipped (by setting the
c     counter to the last character [i=80])
c
      do while (iblnk.eq.1)
         read(iunitr,1000,end=9999) string
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
c      set location of first non-blank character
       istrt = i-1
c
c      check if it is a section header (first character a *)
c
       if (string(istrt:istrt).eq.'*') isect=1
c
c      strip of any trailing comments on the line
c
       do 1 j=istrt,80
1        if (string(j:j).eq.'#') string(j:80) = ' '
c
c      change to lower case
c
       string = case(string)
       return
9999   iend=1
       return
       end
c
      subroutine rgen31(string,iend,istrt)
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
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
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
         write(fu60,*) 'intmueff'
         LOPT(2)=-500
c
c     NOINTMUEFF (default)
      else if (string(istrt:istrt+9).eq.'nointmueff') then
         write(fu60,*) 'nointmueff'
         LOPT(2)=500
c
c     KIES (recalculation of s)
      else if (string(istrt:istrt+3).eq.'kies') then
         IRECS=1
         write(fu60,*) 'kies'
c
c     NOKIES (default)
      else if (string(istrt:istrt+5).eq.'nokies') then
         IRECS=0
         write(fu60,*) 'nokies'
c
c     Everything has to be in bohr and hartrees.
c     It will only accept gradients, no forces.
c
c     GEOM 
      else if (string(istrt:istrt+3).eq.'geom') then
          write(fu60,*) 'geom'
          call rgem31(string,istrt)
          write(fu60,*) 'end'
c     GRADS
      else if (string(istrt:istrt+4).eq.'grads') then
          write(fu60,*) 'grads'
          call rgrd31(string,istrt,igrd)
          write(fu60,*) 'end'
c     HESSIAN
      else if (string(istrt:istrt+6).eq.'hessian') then
          write(fu60,*) 'hessian'
          call rhss31(string,istrt,ipck,ihss)
          write(fu60,*) 'end'
c
      else
        write(fu6,1200) string(istrt:80)
        stop 'rgen31-1'
      endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
c
      if (igem.eq.1.and.igrd.eq.1) LOPT(3)=0
      if (igem.eq.1.and.igrd.eq.2) LOPT(3)=-1
      if (igem.eq.2.and.igrd.eq.2) LOPT(3)=1
      if (igem.eq.2.and.igrd.eq.1) LOPT(3)=-2
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
      end
c
      subroutine rgem31(string,istrt)
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
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
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
             write(fu60,*) '   ms'
        else if (string(istrt:istrt+2).eq.'uns') then
             igem=1
             write(fu60,*) '   uns'
        else if (string(istrt:istrt+2).eq.'ang') then
             iugm31=1
             write(fu60,*) '   ang'
        else if (string(istrt:istrt+1).eq.'au') then
             iugm31=0
             write(fu60,*) '   au'
        else
             write(fu6,1200) string(istrt:80)
             stop 'rgem31-1'
        endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu31 geom keyword',/A80)
      return
      end
      subroutine rgrd31(string,istrt,igrd)
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
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
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
             write(fu60,*) '   ms'
        else if (string(istrt:istrt+2).eq.'uns') then
             igrd=1
             write(fu60,*) '   uns'
        else if (string(istrt:istrt+2).eq.'ang') then
             iugr31=1
             write(fu60,*) '   ang'
        else if (string(istrt:istrt+1).eq.'au') then
             iugr31=0
             write(fu60,*) '   au'
        else if (string(istrt:istrt+4).eq.'grads') then
             igof31=0
             write(fu60,*) '   grads'
        else if (string(istrt:istrt+5).eq.'forces') then
             igof31=1
             write(fu60,*) '   forces'
        else
             write(fu6,1200) string(istrt:80)
             stop 'rgrd31-1'
        endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu31 grads keyword',/A80)
      return
      end
c
      subroutine rhss31(string,istrt,ipck,ihss)
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
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
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
             write(fu60,*) '   ms'
        else if (string(istrt:istrt+2).eq.'uns') then
             ihss=1
             write(fu60,*) '   uns'
        else if (string(istrt:istrt+3).eq.'full') then
             ipck=2
             write(fu60,*) '   full'
        else if (string(istrt:istrt+3).eq.'pack') then
             ipck=1
             write(fu60,*) '   pack'
        else if (string(istrt:istrt+2).eq.'ang') then
             iuhs31=1
             write(fu60,*) '   ang'
        else if (string(istrt:istrt+1).eq.'au') then
             iuhs31=0
             write(fu60,*) '   au'
        else
             write(fu6,1200) string(istrt:80)
             stop 'rgem31-1'
        endif
      call rline(fu31,string,istrt,isect,iend)
      enddo
1200  format(3x,'Error:  the following is not a valid option for the',
     *       ' fu31 hessian keyword',/A80)
      return
      end
         function case (string)
c
c   Function which takes a string of 80 characters and converts the 
c   upper case letters in the string to lower case letters
c
      implicit double precision (a-h,o-z)
         character*80 string, case
         character * 1 xlett
c
         do 10 i = 1, 80
               xlett = string(i:i)
               itry = ichar (xlett)
                if (xlett .ge. 'A' .and. xlett .le. 'Z') then 
                    itry = itry + 32
                    string (i:i) = char (itry)
                endif
   10     continue
c
                case  = string 
c
          return
          end
c
      subroutine rall31 (string,istrt,npos,nneg,NEPTS,NENEG,NEPOS,
     *                   NGPTS,NGNEG,NGPOS)
c
c     JCC   8/10/97
c
c     subroutine to read all the data from fu31 and store it in array
c
      implicit double precision (a-h,o-z)
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
      character * 80 string
c 
c     call rline(fu31,string,istrt,isect,iend)
c     
c     Call the correct routine to read in the data in this section.
c     control will come back here once another section header has
c     been found.  This will continue until an end-of-file mark
c     has been encoutered.
c
      do while (iend.eq.0)
         j = istrt + 1
         if (string(j:j+5).eq.'react1') then
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
         else if (string(j:j+5).eq.'genera') then
         else
               write(fu6,1300) string(j-1:80)
               stop 'rall31-3'
         end if
      end do
c
1300  format(3x,'Error:  the following is not a valid section name',
     *         /A80)
c
c     Reordering of the information
c
      nprpnt=npoint-3
      if (lgs(6).eq.1) nprpnt=nprpnt-2
      if (lgs(6).eq.2.or.lgs(6).eq.3) nprpnt=nprpnt-1
      write (fu6,*)
      write (fu6,2000) nprpnt
2000  Format (1x,'Number of nonstationary grid points: ',
     *I6)
      call ords31 (npos,nneg,NEPTS,NENEG,NEPOS,NGPTS,NGNEG,NGPOS)
      return
      end
c
      subroutine rpnt31(string,iend,istrt,itp)
c
c     JCC   8/10/97
c
c     subroutine to read all the data from fu31 and store it in array
c
      implicit double precision (a-h,o-z)
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
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
                save31(i+n3tm+4,npoint)=DX(i)
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
         call rline(fu31,string,istrt,isect,iend)
      end do
1200  format(1x,'Error: The following keyword is not valid in fu31',
     */A80)
      return
      end
      SUBROUTINE readln(IUNIT,WORD,NWORD,LSEC,LEOF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER*80  LINE,SENAME,STRING,UPCASE
      CHARACTER*80  WORD(40)
      LOGICAL       LEOF,LSEC
C
      LSEC = .FALSE.
      LEOF = .FALSE.
      NWORD = 0
C
10    READ(IUNIT,'(A80)',END=500) STRING
C
      IP = 0
C
C     Find the first nonblank character in a line
C
20    IP = IP + 1
      IF (STRING(IP:IP) .EQ. ' ' .AND. IP .LE. 80) GO TO 20           
C
C     If it all blank or it is a comment line then read the next line
C
      IF (IP .GT. 80 .OR. STRING(IP:IP) .EQ. '#')  GO TO 10
C
C     Convert to uppercase
C
      LINE = UPCASE(STRING)
C
C     Find the position of a comment character, if any,
C     in a nonblank noncomment line
C
      ICOM = IP 
50    ICOM = ICOM + 1
      IF (LINE(ICOM:ICOM) .NE. '#' .AND. ICOM .LE. 80)  GOTO 50         1103WH94
C
55    IF (IP .LT. ICOM) THEN
         IBEG = IP
60       IP = IP + 1
         IF (LINE(IP:IP) .NE. ' ' .AND. IP .LT. ICOM) GOTO 60 
         NWORD = NWORD + 1
         WORD(NWORD) = LINE(IBEG:IP-1)
70       IP = IP + 1
         IF (LINE(IP:IP) .EQ. ' ' .AND. IP .LT. ICOM) GOTO 70
         GOTO 55
      ENDIF
C
      IF (WORD(1)(1:1) .EQ. '*') THEN
         SENAME = WORD(1)(2:)
         WORD(1) = SENAME
         LSEC = .TRUE.
      ENDIF
C
      RETURN
C
500   LEOF = .TRUE.
C
      RETURN
C
      END
      subroutine rword(string,istrt,ierr)
c
c     This subroutine is a utility routine that takes a character string
c     and searches from the current character (istrt) for the first
c     character that occurs after at least one blank (ie for the next 
c     word on a line).  It returns the location of that character in
c     istrt.  ierr is set to 1 if no other characters were found in 
c     the string.
c
      implicit double precision (a-h,o-z)
      character * 80 string
c
c     clear flags
      jblank = 0
      ierr = 0
c
c     search for the first blank - signaling the end of the current word
c
      do while (istrt.le.79.and.jblank.eq.0)
         istrt = istrt+1
         if (string(istrt:istrt).eq.' ') then
c                                               find first letter of next word
            jblank = 1
            do while (string(istrt:istrt).eq.' '.and.ierr.eq.0)
               istrt = istrt + 1
               if (istrt.gt.80) ierr=1
            end do
         end if
         if (istrt.eq.80) ierr=1
      end do
      return
      end
      CHARACTER*80 FUNCTION upcase(STRING)
C
C   Function which takes a string of 80 characters and converts the 
C   lower case letters in the string to upper case letters
C   This functions is a modified version of CASE which was written
C   by Rozeanne Steckler
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*80 STRING,LINE
      CHARACTER * 1 XLETT
C
      LINE = STRING
      DO 10 I = 1, 80
         XLETT = LINE(I:I)
         ITRY = ICHAR (XLETT)
         IF (XLETT .GE. 'a' .AND. XLETT .LE. 'z') THEN 
            ITRY = ITRY - 32
            LINE(I:I) = CHAR (ITRY)
         ENDIF
10    CONTINUE
C
      UPCASE  = LINE 
C
      RETURN
C
      END
      subroutine rhes31(N)
c
c     JCC   8/10/97
c
c     subroutine to read hessian matrix from unit 31
c
      implicit double precision (a-h,o-z)
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
      character * 80 string
      character*80 word(40)
      logical lsec,leof
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
 100  write(fu6,*) 'Too many numbers entered for a ',N,' by ',N
     *             ,'hessian'
      stop 'rhes31 2'
C
      end
c
      subroutine ords31 (npos,nneg,NEPTS,NENEG,NEPOS,NGPTS,NGNEG,NGPOS)
c
c     JCC   8/10/97
c
c     subroutine to reorder the information on array save31
c
      implicit double precision (a-h,o-z)
      include 'param.inc'
      include 'percon.inc'
      include 'common.inc'
      alim = 1.D-5
c
c     calculate number of save points (itp=6, save31(2,i)=11111)
c     and the number of save points at each side of the saddle
c     point
c
      NPTS=0
      NGPTS=0
      NEPTS=0
      NPOS=0
      NGP0S=0
      NEPOS=0
      NNEG=0
      NGNEG=0
      NENEG=0
c
      do i=1,npoint
       if (save31(1,i).gt.5.9D0.and.save31(1,i).lt.6.1D0) then
           NEPTS=NEPTS+1
           if (save31(3,i).gt.0.D0) then
              NEPOS=NEPOS+1
              if(save31(2,i).gt.11110.9D0) then
                  NPTS=NPTS+1
                  NPOS=NPOS+1
              endif
           else
              NENEG=NENEG+1
              if(save31(2,i).gt.11110.9D0) then
                  NPTS=NPTS+1
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
      write (fu6,2000) NPTS
c
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
2000  Format (1x,'Number of nonstationary save points: ',
     *I6)
      end

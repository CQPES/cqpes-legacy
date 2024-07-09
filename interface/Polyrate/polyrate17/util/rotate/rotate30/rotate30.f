      program rotate
c
c By JCC, using pieces of code by JVF and some Polyrate subroutines
c
c This program read from a fu30 input file plus an additional input file
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
c               want to keep the same values as in poly.fu30
c  -next lines: atomic numbers and coordinates of the reference point,
c               usually the saddle point (there must be four numbers 
c               per line)
c
      implicit double precision (a-h,o-z)
      character*80 echo
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
      character*80 echo1(nsdm), echo2(nsdm), echo3(nsdm), 
     *             echo4(nsdm), echo5(nsdm)
c 
c define input and output files
c
      open (unit=fu5, file = "poly.fu5", status="old")
      open (unit=fu30, file = "poly.fu30", status="old")
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
      nratom(7)=natom
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
c now start reading fu30
c
c skip the comment line
c
      read (fu30,'(a80)') echo
      write (fu60,'(a80)') echo
c
c read LOPT options
c
      read (fu30,*) lopt
      if (lopt(4).eq.0) then
         write (fu6,*) "This program only works with fu30 with Hessians"
         write (fu6,*) "If you don't input it, I can't reorient it!"
         stop "No Hessian"
      else if (lopt(5).ne.0) then
         write (fu6,*) "This program only works with fu30 with no"
         write (fu6,*) "anharmonic information. Sorry!"
         stop "Anharmonic"
      else if (lopt(6).ne.2) then
         write (fu6,*) "This program only works with lopt(6)=2 in fu30."
         write (fu6,*) "All the other options don't have reorientation"
         write (fu6,*) "problems (lopt(6) < 2) or they cannot be solved"
         write (fu6,*) "(lopt(6) > 2 ) because the geometry of the"
         write (fu6,*) "extra points is not input."
         stop "LOPT(6)"
      else if (lopt(3).ne.-2.and.lopt(3).ne.-1.and.lopt(3).ne.0.and.
     *         lopt(3).ne.1.and.lopt(3).ne.99.and.lopt(3).ne.100) then
         write (fu6,*) "Invalid option for lopt(3)"
         stop "LOPT(3)"
      else if (lopt(4).ne.-2.and.lopt(4).ne.-1.and.lopt(4).ne.0.and.
     *         lopt(4).ne.1.and.lopt(4).ne.2) then
         write (fu6,*) "Invalid option for lopt(4)"
         stop "LOPT(4)"
      endif
      write (fu60,*) lopt
c
c skip the title line, Values of S and V, and IFREQ line, and hessian
c if it exist for the reactants, products, and wells, and the saddle point.
c
      do j=1,7
         if (nratom(j).ne.0) then
            do i=1,2
               read (fu30,'(a80)') echo
               write (fu60,'(a80)') echo
            enddo
            if (nratom(j).eq.1) then
                read(fu30,*)
               write (fu60,*)
            else
                read(fu30,'(a80)') echo
                write (fu60,'(a80)') echo
            endif
            if (nratom(j).gt.1) then
                read (fu30,'(a80)') echo
                write (fu60,'(a80)') echo
                if (lopt(4).lt.0.or.lopt(4).eq.99) then
                   jf=nint(3*nratom(j)*(3*nratom(j)+1)/2.0)
                   read (fu30,*) (tmph(l), l=1,jf)
                   lc=0
                   do l=1,3*nratom(j)
                      write (fu60,'(4E20.10)') (tmph(lc+l2), l2=1,l)
                      lc=lc+l
                   enddo
                else
                   jf=3*nratom(j)*3*nratom(j)
                   read (fu30,*) (tmph(l), l=1,jf)
                   do l=1,3*nratom(j)
                      write (fu60,'(4E20.10)') 
     *                (tmph((l-1)*3*nratom(j)+l2), l2=1,3*nratom(j))
                   enddo
                endif
            endif
         endif
      enddo
c
c skip the comment card and read the number of points
c
      read (fu30,'(a80)') echo
      write (fu60,'(a80)') echo
      read (fu30,*) np
      write (fu6,800) np
      write (fu60,*) np
      if (np.gt.nsdm) then
         write (fu6,*) 'Error: NSDM parameter is too small.'
         write (fu6,*) 'It has to be larger than the number of points'
         write (fu6,*) 'in poly.fu30. Change it in the param.inc file'
         write (fu6,*) 'and recompile.'
         stop 'low nsdm'
      endif
c
c start the loop for reading all the points and saving them in 
c arrays
c
c The information will be stored in the following arrays:
c s           -> SSUBI(NSDM)
c energy      -> VCLAS(NSDM)
c geometries  -> GEOM(N3TM,NSDM)
c gradients   -> FMIHTS(N3TM,NSDM)
c hessians    -> HESS31(N3TM,N3TM,NSDM)
c rotat. matx.-> EFNDT(N3TM,NSDM)
c
c so we will initialize them
c
      do i=1,nsdm
         echo1(i)=" "
         echo2(i)=" "
         echo3(i)=" "
         echo4(i)=" "
         echo5(i)=" "
         ssubi(i) = 0.d0
         vclas(i) = 0.d0
         do j=1,n3tm
            geom (j,i) = 0.d0
            fmihts (j,i) = 0.d0
            do k=1,n3tm
               hess31(k,j,i) = 0.d0
            enddo
         enddo
      enddo
c
c    npr :      #number of points in the reactants region (SP not included)
c    npp :      #number of points in the products region (SP not included)
c
      npr = 0
      npp = 0
c
c and now read the information
c
      do ipoint = 1, np
c
c read title1
         read (fu30,'(a80)') echo1(ipoint)
c read s and v
         read (fu30,*) ssubi(ipoint), vclas(ipoint)
c count the number of points on the reactant and product side
         if (ssubi(ipoint).lt.0.d0) then
            npr = npr + 1
         else if (ssubi(ipoint).gt.0.d0) then
            npp = npp + 1
         endif
c read title2
         read (fu30,'(a80)') echo2(ipoint)
c read geometry
         read (fu30,*) (geom(i,ipoint), i=1,n3)
c transform to unscaled if the geometry is mass-scaled
         if (lopt(3).eq.1.or.lopt(3).eq.-2) then
            do i=1,n3
               geom(i,ipoint)=geom(i,ipoint)/amass(i)
            end do
         endif
c read title3
         read (fu30,'(a80)') echo3(ipoint)
c read gradients
         read (fu30,*) (fmihts(i,ipoint), i=1,n3)
c read ifreq
         read (fu30,'(a80)') echo4(ipoint)
c read hessian title
         read (fu30,'(a80)') echo5(ipoint)
c read hessian and put it in the hess31 array 
         read (fu30,*) (tmph(i), i=1,jf)
         if (lopt(4).lt.0.or.lopt(4).eq.99) then
            k=1
            do i=1,n3
               do j=1,i
                  hess31(i,j,ipoint) = tmph(k)                                 
                  hess31(j,i,ipoint) = tmph(k)                                 
                  k=k+1                                         
               enddo                                            
            enddo
         else 
            k=1
            do i=1,n3
               do j=1,n3
                  hess31(i,j,ipoint) = tmph(k)                                 
                  k=k+1                                         
               enddo                                            
            enddo
         endif
c
c end of the loop
c
      enddo
c
c check that npp + npr = np (it ought to be the same!)
c
      if ((npp+npr).ne.np) then
         write (fu6,*) 'Error: The number of points on the product side'
         write (fu6,*) 'plus the number of points on the reactant side'
         write (fu6,*) 'does not equal the number of points input.'
         stop 'np check'
      endif
      write (fu6,810) npr
      write (fu6,820) npp
c
c take the reference geometry
c
      write (fu6,*) 'Reorienting the reactant side'
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
c reorient points until npr is reached. I
c
      sr = 0.d0
      do k=1,npr
         sint=0.0d0
         if (k.le.npr) write (fu6,360) k
         write (fu6,400)
         npoint=npr-k+1
         do j=1,n3
            xx2(j)=geom(j,npoint)
         end do
         write (fu6,450) (xx2(i),i=1,3*n)
c
c call the subroutine that computes s differences
c
         call calcs (xx1,xx2,sval,rotas,kkk)
         sr = sr - sval
         if (ichng.eq.1) ssubi(npoint) = sr
         srsp(k)=sval
         do i1=1,3
            do i2=1,3
               i3 = ((i1-1)*3+i2)
               efndt(i3,npoint)=rotas(i1,i2)
            enddo
         enddo
         write (fu6,500) srsp(k)
         write (fu6,550) sr
         write (fu6,*) "Rotated geometry:"
         write (fu6,400)
         write (fu6,450) (xx2(i),i=1,3*n)
         write (fu6,*) 
         write (fu6,*) "Rotation matrix:"
         write (fu6,450) (efndt(i,npoint),i=1,9)
         write (fu6,*) 
         do i=1,3*n
            xx1(i)=xx2(i)
         end do
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
         npoint=npr+k
         do j=1,n3
            xx2(j)=geom(j,npoint)
         end do
         write (fu6,450) (xx2(i),i=1,3*n)
c
c call the subroutine that computes s differences
c
         call calcs (xx1,xx2,sval,rotas,kkk)
         sp = sp + sval
         if (ichng.eq.1) ssubi(npoint) = sp
         srsp(k)=sval
         do i1=1,3
            do i2=1,3
               i3 = ((i1-1)*3+i2)
               efndt(i3,npoint)=rotas(i1,i2)
            enddo
         enddo
         write (fu6,500) srsp(k)
         write (fu6,550) sp
         write (fu6,*) "Rotated geometry:"
         write (fu6,400) 
         write (fu6,450) (xx2(i),i=1,3*n)
         write (fu6,*) 
         write (fu6,*) "Rotation matrix:"
         write (fu6,450) (efndt(i,npoint),i=1,9)
         write (fu6,*) 
         do i=1,3*n
            xx1(i)=xx2(i)
         end do
      end do
c
c write in fu60 the information about the points:
c starting the loop
c
      do npoint=1,np
c write recalculated s and v
         write (fu60,'(a80)') echo1(npoint)
         write (fu60,'(f16.10,5x,f16.10)') ssubi(npoint), vclas(npoint)
c recover rotation matrix
         do i1=1,3
            do i2=1,3
               i3 = ((i1-1)*3+i2)
               rotas(i1,i2) = efndt(i3,npoint)
            enddo
         enddo
c rotate geometry
         do i=1,natom
            do j=1,3
                sum =0.0d0
                do k=1,3
                    idum = 3*(i-1)+k
                    sum = sum + rotas(j,k)*geom(idum,npoint)
                end do
                idum = 3*(i-1)+j
                tmpd(idum)=sum
            end do
         enddo
         do i=1,n3
            geom(i,npoint)=tmpd(i)
         enddo
c write geometry
         write (fu60,'(a80)') echo2(npoint)
c transform to mass-weighted if the geometry initially was mw
         if (lopt(3).eq.1.or.lopt(3).eq.-2) then
            do i=1,n3
               geom(i,npoint)=geom(i,npoint)*amass(i)
            end do
         endif
         write (fu60,'(3(3x,f16.10))') (geom(i,npoint),i=1,n3)
c rotate gradient
         do i=1,natom
            do j=1,3
                sum =0.0d0
                do k=1,3
                    idum = 3*(i-1)+k
                    sum = sum + rotas(j,k)*fmihts(idum,npoint)
                end do
                idum = 3*(i-1)+j
                tmpd(idum)=sum
            end do
         enddo
         do i=1,n3
            fmihts(i,npoint)=tmpd(i)
         enddo
c write gradient
         write (fu60,'(a80)') echo3(npoint)
         write (fu60,'(3(3x,f16.10))') (fmihts(i,npoint),i=1,n3)
c rotate hessian
         do i=1,n3
            do j=1,n3
               rotaf(i,j)=0.d0
            enddo
         enddo
         do i=1,natom
            idum=(i-1)*3
            do j=1,3
               do k=1,3
                  rotaf(idum+j,idum+k)=rotas(j,k)
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
               do k=1,n3
                  tmpf1(i,j)=tmpf1(i,j)+rotaf(i,k)*hess31(k,j,npoint)
               enddo
            enddo
         enddo
         do i=1,n3
            do j=1,n3
               do k=1,n3
                  tmpf2(i,j)=tmpf2(i,j)+tmpf1(i,k)*rotaf(j,k)
               enddo
            enddo
         enddo
         do i=1,n3
            do j=1,n3
               hess31(i,j,npoint)=tmpf2(i,j)
            enddo
         enddo
c write hessian
         write (fu60,'(a80)') echo4(npoint)
         write (fu60,'(a80)') echo5(npoint)
         if (lopt(4).lt.0.or.lopt(4).eq.99) then
            do i=1,n3
               write (fu60,'(4E20.10)') (hess31(i,j,npoint), j=1,i)
            enddo
         else
            do i=1,n3
               write (fu60,'(4E20.10)') (hess31(i,j,npoint), j=1,n3)
            enddo
         endif
c
c end of the loop
c
      enddo
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
800   FORMAT(/1X,'There are ',I3,' nonstationary points.')
810   FORMAT(1X,I3,' are on the reactant side and')
820   FORMAT(1X,I3,' on the product side.')
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

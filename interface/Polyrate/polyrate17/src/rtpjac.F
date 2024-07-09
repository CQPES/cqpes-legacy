c
c***********************************************************************
c     RTPJAC
c***********************************************************************
c
      subroutine rtpjac
      use common_inc
      use perconparam
      use potmod
      use rate_const
c
      implicit double precision (a-h,o-z)
c
c     This subroutine is only used for bimolecular atom transfer reaction:
c         1. convert the MEP to pesdo-Jacobian coordinate.
c         2. find out the representative tunneling path in LCT
c            at the given temperature.
c         3. create the MATLAB format input file for plotting
c
      dimension intsec(natoms)                                          0115PJ02 
      dimension geomi(3*natoms),geomjac(3*natoms,nsdm), atmjac(natoms)  0115PJ02     
      dimension xcmsA(3), xcmsB(3), xcmsC(3), xcmsBC(3) 
      dimension sjac(nsdm), xjac(nsdm), yjac(nsdm)                      0115PJ02
      dimension rtes0(maxps), rtes1(maxps), rtex0(maxps), rtex1(maxps),
     *          rtey0(maxps),rtey1(maxps)
c


c --- Define the A + BC psedo system

c --- Initialization
      do i = 1, natom
          intsec(i) = 0
      end do
      inum = 0

c --- case 1: A + BC -> C + AB
      call intersc(1,3,intsec,inum)
      if (inum .eq. 0) then
          idA = 1
          idC = 3
          call intersc(2,4,intsec,inum)
          if(inum .ne. 1) then 
              write(fu24,50)
              return
          end if
          idB =  intsec(inum)
      end if

c --- case 2: A + BC -> AB + C
      call intersc(1,4,intsec, inum)
      if (inum .eq. 0) then
          idA = 1
          idC = 4
          call intersc(2,3, intsec, inum)
          if(inum .ne. 1) then
              write(fu24,50)
              return
          end if
          idB = intsec(inum)
      end if

c --- case 3: BC + A -> AB + C
      call intersc(2,4, intsec, inum)
      if (inum .eq. 0) then
          idA = 2
          idC = 4
          call intersc(1,3, intsec, inum)
          if(inum .ne. 1) then
              write(fu24,50)
              return
          end if
          idB = intsec(inum)
      end if

c --- case 4: BC + A -> C + AB
      call intersc(2,3, intsec, inum)
      if (inum .eq. 0) then
          idA = 2
          idC = 3
          call intersc(1,4, intsec, inum)
          if(inum .ne. 1) then
              write(fu24,50)
              return
          end if
          idB = intsec(inum)
      end if


50    format('Error: RTPJAC only works for atom transfer reaction.')           


c --- Convert the geometry of MEP to scaled Jacobian coordinate

      isbeg =  nshlf - nshlf  / itvmep * itvmep
      if (isbeg .eq. 0) isbeg = itvmep
      nstep = (lsave - isbeg) / itvmep + 1
    
      autoa = 0.529177d0
      is = isbeg

      do i = 1, nstep
          sjac(i) = ssubi(is)

c --- pack geometry of A into geomjac
          jbeg = 1
          jend = nratom(idA)
          do j = jbeg, jend
              iat = iatsv(j-jbeg+1,idA)
              atmjac(j) = svmas(iat)*cau
              do k = 2, 0, -1
                  geomjac(3*j-k,i) =   geom(3*iat-k,is)
     &                               * amass(3*iat-k)*autoa
              end do
          end do
       

c --- Pack geometry of B into geomjac
          jbeg = jend + 1
          jend = jend + 1
          atmjac(jbeg) = svmas(idB)*cau
          do k = 2, 0, -1
              geomjac(3*jbeg-k,i) =   geom(3*idB-k,is)
     &                              * amass(3*idB-k)*autoa
          end do



c --- Pack geometry of C into gemojac
          jbeg = jend + 1
          jend = jend + nratom(idC)
          do j = jbeg, jend
              iat = iatsv(j-jbeg+1,idC)
              atmjac(j) = svmas(iat)*cau
              do k = 2, 0, -1
                  geomjac(3*j-k,i) =   geom(3*iat-k,is)
     &                               * amass(3*iat-k)*autoa
              end do
          end do
 

          is = is + itvmep
      end do



c --- Up to now, the sequence in geomjac is A-B-C

      nrA = nratom(idA)
      nrB = 1
      nrC = nratom(idC)
      natm = natom

c --- mass scale factor for Jacobi y
      tmsA = 0
      tmsB = 0
      tmsC = 0

      do j = 1, nrA
          tmsA = tmsA + atmjac(j)
      end do

      do j = nrA + 1, nrA + nrB
          tmsB = tmsB + atmjac(j)
      end do

      do j = nrA + nrB + 1, nrA + nrB + nrC
          tmsC = tmsC + atmjac(j)
      end do

      rdmBC = (tmsB + tmsC)/(tmsB * tmsC)
      rdmABC = (tmsA + (tmsB + tmsC))/(tmsA * (tmsB + tmsC))

      facms = sqrt(rdmABC/rdmBC)


      do i = 1, nstep

          do j = 1, natom*3
              geomi(j) = geomjac(j,i)
          end do


c --- zero out xmscA, etc
          do j = 1, 3
              xcmsA(j) = 0
              xcmsBC(j) = 0
              xcmsB(j) = 0
              xcmsC(j) = 0
          end do

c --- center of mass of A
          ibeg = 1
          iend = nrA
          call cms(geomi, atmjac, natm, ibeg, iend, xcmsA)

c --- center fo mass of BC
          ibeg = nrA + 1
          iend = nrA + nrB + nrC
          call cms(geomi, atmjac, natm, ibeg, iend, xcmsBC)


c --- distance between cmsA and cmsBC, Jacobian x
          xtp = 0
          do j = 1, 3
             xtp = xtp + (xcmsA(j) - xcmsBC(j))**2
          end do

          xjac(i) = sqrt(xtp)

c --- center of mass of C
          ibeg = nrA + nrB + 1
          iend = nrA + nrB + nrC
          call cms(geomi, atmjac, natm, ibeg, iend, xcmsC)

c --- center of mass of B
          ibeg = nrA + 1
          ibeg = nrA + nrB
          call cms(geomi, atmjac, natm, ibeg, iend, xcmsB)

c --- mass scaled distnace between cmsB and cmsC, Jacobian y
          xtp = 0
          do j = 1, 3
              xtp = xtp + (xcmsC(j) - xcmsB(j))**2
          end do

          yjac(i) = facms * sqrt(xtp)

      end do

c --- print the MEP in pesdo-Jacobian x,y
      write(fu24, 100)
      write(fu24, 150)
      write(fu24, 200)
      do i = 1, nstep
          write(fu24, 300) sjac(i), xjac(i),  yjac(i)
      end do
      write(fu24, 400)
      write(fu24, 450)
      write(fu24, 470)


100   format(3x, 'MEP IN MASS-SCALED JACOBIAN COORDINATES')
150   format(3x, '---------------------------------------------',/)
200   format(7x, 's(bohr)', 6x, 'x', 9x, 'y')
300   format(3x, f10.4, f10.4, f10.4)
400   format(//, 3x, 'REPRSENTATIVE TUNNELING TURNING POINTS')
450   format(3x, '--------------------------------------') 
470   format(/,6x, 'T(K)',5x,'NPS',3x,'s(bohr)',6x,'x',9x,'y')



c --- go over the LCT representative tunneling energy
      do it = 1, ntemp
         rtejac = rtelct(it)

c --- interpolation the turning point s0 and s1
         do  i = 1, ipdjac

             j = 1
             jbreak = 0
             do while ((j .le. iejac-1) .and. (jbreak .eq. 0))
            

c --- find s values at turning points s0, s1

                 if ( (esvjac(i,j) .le. rtejac) .and.
     *               (rtejac .le.  esvjac(i,j+1)) ) then

                     e1 = esvjac(i,j)
                     e2 = esvjac(i,j+1)

                     s01 = s0jac(i,j)
                     s02 = s0jac(i,j+1)

                     s11 = s1jac(i,j)
                     s12 = s1jac(i,j+1)

                     call linint(e1,e2,rtejac, s01,s02, s0)
                     call linint(e1,e2,rtejac, s11,s12, s1)

                     rtes0(i) = s0
                     rtes1(i) = s1

c --- interpolate the Jacobian x,y at s0, s1

                     k = 1
                     kbreak = 0
                     do while ((k .le. nstep-1) .and. (kbreak .eq. 0))     
                         if ((sjac(k) .le. s0) .and.
     *                      (s0 .le. sjac(k+1)) ) then

                             p1 = sjac(k)
                             p2 = sjac(k+1)

                             xx1 = xjac(k)
                             xx2 = xjac(k+1)

                             yy1 = yjac(k)
                             yy2 = yjac(k+1)

                             call linint(p1,p2,s0, xx1,xx2,tx0)
                             call linint(p1,p2,s0, yy1,yy2,ty0)
                             kbreak = 1 
                         end if  
                         k = k + 1
                     end do
                     rtex0(i) =  tx0
                     rtey0(i) =  ty0
                          
                     k = 1
                     kbreak = 0
                     do while ((k .le. nstep-1) .and. (kbreak .eq. 0))
                         if ((sjac(k) .le. s1) .and.
     *                       (s1 .le. sjac(k+1)) ) then

                             p1 = sjac(k)
                             p2 = sjac(k+1)

                             xx1 = xjac(k)
                             xx2 = xjac(k+1)

                             yy1 = yjac(k)
                             yy2 = yjac(k+1)

                             call linint(p1,p2,s1,xx1,xx2,tx1)
                             call linint(p1,p2,s1,yy1,yy2,ty1)
    
                             kbreak = 1
                         end if
                         k = k + 1
                     end do
                     rtex1(i) = tx1
                     rtey1(i) = ty1

                     jbreak = 1
                 end if 
                    
                 j = j + 1 

             end do

          end do


c --- Print the s value and the turning point Jacobian x and y
          do i = 1, ipdjac
              if (i .eq. 1) then 
                  write(fu24,500) temp(it), i-1, rtes0(i), 
     *                            rtex0(i),rtey0(i)
                  write(fu24,600) rtes1(i), rtex1(i),rtey1(i)
              else 
                  write(fu24,700) i-1, rtes0(i), rtex0(i), rtey0(i)
                  write(fu24,800) rtes1(i), rtex1(i), rtey1(i)
              end if
          end do
    
      end do


500   format(3x,f8.2,3x,I3,3x,f7.4,3x,f7.4,3x,f7.4)
600   format(20x,f7.4,3x,f7.4,3x,f7.4)
700   format(14x,I3,3x,f7.4,3x,f7.4,3x,f7.4)
800   format(20x,f7.4,3x,f7.4,3x,f7.4)

      

c --- close file
      close(fu24)


      return

      end subroutine rtpjac

c
c*************************************************************************
c     CMS
c*************************************************************************
c
      subroutine cms(geomj,atmj,n,ibeg, iend, xcms)
c
c     This subroutine is to calculate the center of
c     mass of a subset of the molecular system
c
c     Called by:
c             rtpjac
c
      implicit double precision (a-h,o-z)
c
      dimension geomj(n*3), atmj(n)
      dimension sum(3), xcms(3)
c
     
      do i = 1, 3
         sum(i) = 0
      end do

      tmass = 0

      do i = ibeg, iend
          do j = 2, 0, -1
              sum(3-j) = sum(3-j) + atmj(i) * geomj(3*i-j)
          end do
          tmass = tmass + atmj(i)
      end do

      do j = 1, 3
          xcms(j) = sum(j)/tmass
      end do
     
      return

      end subroutine cms
c
c***********************************************************************
c     INTERSC
c***********************************************************************
c
      subroutine intersc(jtype1, jtype2, intsec,inum)
      use common_inc
      use perconparam
c
c     This subroutine is to find out the intersection of
c     two molecules.
c
c     Called by:
c           rtpjac
c

      implicit double precision (a-h, o-z)
c
      dimension intsec(natom)
c
  
      nr1 = nratom(jtype1)
      nr2 = nratom(jtype2)

      inum = 0
      do i = 1, nr1
          do j = 1, nr2
              if (iatsv(i,jtype1) .eq. iatsv(j,jtype2)) then
                  inum = inum + 1
                  intsec(inum) = iatsv(i, jtype1)
              end if
          end do
      end do

      return

      end subroutine intersc


c
c************************************************************
c     LININT
c************************************************************
c
      subroutine linint(a1,a2, a, b1, b2, b)
c
      implicit double precision (a-h,o,z)
c
c     This subroutine is doing linear interpolation
c
c     Called by:
c           rtpjac
c

      fac = (a-a1)/(a2-a1)
      b = (b2-b1) * fac + b1

      return
      end subroutine linint



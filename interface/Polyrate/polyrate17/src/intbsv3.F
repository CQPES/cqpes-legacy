c
c***********************************************************************
c     ICFDRP
c***********************************************************************
c
      subroutine icfdrp(iop, issad)
      use common_inc
      use perconparam
      use potmod
      use kintcm; use cm
      use rate_const; use intbsv
c
c     Created by: Jingzhi Pu, Jan. 2001
c
c     This is the driver for the internal coordinate generalized normal
c     mode analysis for reactants and products
c
c     Called by:
c              normod
c     Calls:
c           prsq, trans, trlfrp, tranlf, projct, icoutrp, prntfc, bmat,
c           bimat,  bimat2, trang, btens, formf2, tranfc, expnd, projf,
c           gfdiag, gfdia2, veccon, icsvrp, matx

      implicit double precision (a-h,o-z)

c --- Set parameters for array size
c      parameter (maxca2 = maxcar * (maxcar + 1)/2)
c      parameter (maxin2 = maxint * (maxint + 1)/2)
c      parameter (maxcor = 2*maxint*maxcar + maxint*maxin2 + maxin2 +
c     *           maxin2 + maxint*maxcar)


      logical issad, icpr, redun, reorder
      character*2 al

      dimension al(natoms), xx(n3tm), dxint(maxint), amassx(n3tm)
      dimension xc(n3tm), yc(n3tm), zc(n3tm), dxx(n3tm)
c      dimension fl(maxca2)
      dimension scr1(n3tm*maxint)
      dimension scr2(n3tm*maxint)
      dimension scr3(n3tm*maxint)
      dimension gm(maxint*maxint)
      dimension amasin(n3tm*n3tm)
      dimension fintr(maxint*maxint)
      dimension freqi(n3tm)
      dimension avec(n3tm*n3tm)
      
      dimension freqisc(n3tm)
      dimension dxintb(maxint)                                          0214PJ01
      dimension flgmb(maxint)                                           0215PJ01
      dimension avecb(n3tm*n3tm)                                        0214PJ01
      dimension amasinb(n3tm*n3tm)                                      0215PJ01
      dimension scr1b(n3tm*maxint)                                      0214PJ01
      dimension scr2b(n3tm*maxint)                                      0214PJ01
      dimension scr3b(n3tm*maxint)                                      0214PJ01
      dimension fintrb(maxint*maxint)                                   0215PJ01
      dimension ggib(maxint*maxint)                                     0215PJ01
      dimension egnmb(maxint*maxint)                                    0215PJ01
      dimension gmb(maxint*maxint)                                      0215PJ01


     
c --- Projection from redundant to nonredundant
      dimension flgm(maxint)
      dimension egnm(maxint*maxint)
      dimension ggi(maxint*maxint)

c --- Store the orientation of the coordinate system for L bend
      dimension t(3,3)

c --- An array that stores everything
c      dimension core(maxcor)

c --- Internal coordinate with renaming
      dimension IBLR(2, maxint), IBAR(3, maxint),
     *          ITOR(4, maxint), ILBER(3, maxint)

c --- Internal coordinate without renaming
      dimension IBLG(2, maxint), IBAG(3, maxint),
     *          ITOG(4, maxint), ILBEG(3, maxint)
      call intbsv_mem

c --- Don't use save to eliminate the effect from product2 to saddle pt 0213PJ01
c     save                                                            


c --- Set jtype (1-R1, 2-R2, 3-P1, 4-P2)
      jtype = iabs(iop)

c --- Set flag
      icpr = .false.                                                    0621PJ01
      reorder = .false.

      if (icpr) then
           write(fu6, *) 'ICFDRP IS CALLED FOR JTYPE = ', jtype
      endif


c --- Set redundant internal coordinate flag
      redun = .false.
      if (lgs2(39).ge.5) then
          redun =.true.
      end if

c --- Set number of atom for jtype species
      nr = nratom(jtype)
      nr3 = nr * 3
      maxintr = nr3 + 6

c --- Set number of cartesian coordinates and internal coordinate
      ncartr = nr3
      numintr = nintpj(jtype)
      nintr = nintpj(jtype)

c --- Check for number of internal coordinates
c     for curv3 option the maximum allowed is maxint
c     for curv2 option the maximum is nr3-5 or nr3-6

      if (redun) then
          if ( nintr .gt. maxintr ) then
              write (fu6,100) maxintr, nintr
              stop 'ICFDRP 1'
          end if

      else if ((icode(jtype).eq.2) .or. (icode(jtype).eq.3)) then
          if (nintr .gt. nr3-5) then
              write (fu6,100) nr3-5, nintr
              stop 'ICFDRP 2'
          end if

      else if (nintr .gt. nr3-6) then
          write (fu6,100) nr3-6, nintr
          stop 'ICFDRP 3'

      end if

 100  format (1x,'Wrong number of internal coordinates only ',i5,
     *        ' are allowed.',/1x,'You have ',i5,' coordinates.')


c --- Make local copies of position and gradients
      do i = 1, nr
          do j = 1, 3
              iloca = (i-1) * 3 + j
              iglob = (iatsv(i, jtype) - 1) * 3 + j

              xx(iloca) = x(iglob)
              dxx(iloca) = dx(iglob)
              amassx(iloca) = amass(iglob)
          end do
      end do


c --- debugging translate the f mat into fl without un-massscaled
      call trlfrp(nr3, fl, f)


c --- Debugging information Pu, display info. before do anything

      if(icpr)then
         write(fu6,*)'*********************************************'        
         write(fu6,'(/,
     *    '' *** (ICDFRP: S VALUE FROM COMMON ***'')')
         write(fu6,'(1x,f6.3)') s
         write(6,'(/,
     *   '' *** (ICDFRP: UNSCALED CARTESIAN COORDINATES ***'')')
         write(fu6,'(1x,3e20.7)')(xx(i),i=1,nr3)
         write(fu6,'(/,
     *   '' *** (ICDFRP: DXMAG AND DXNORM ***'')')
         write(fu6,'(2e20.7)')dxmag,dxnorm
         write(fu6,'(/,
     *   '' *** (ICDFRP: UNSCALED CARTESIAN GRADIENTS ***'')')
         write(fu6,'(1x,3e20.7)')(dxx(i),i=1,nr3)
         write(6,'(/,
     *   '' *** (ICDFRP: CARTESIAN HESSIAN ***'')')
c        call prsq(f,nr3,nr3,n3tm,n3tm,fu6)
         call prntfc(fl,nr3,maxca2)
         write(fu6,'(/,
     *   '' *** (ICDFRP: AMASSX ***'')')
         write(fu6,'(1x,3e20.10)')(amassx(i),i=1,nr3)
         write(fu6, *)'**********************************************'
      end if


c --- Transfer the symmetric force constant matrix into lower trangle
c     and un-mass scaling, coz the reactant and TS both use mass-scaled
c     hessian

      call tranlf(2, nr3, f, amassx)


c --- PROJCT is only called for generalized TS, not for R. P.
c     Project out trans/rot/cart grad from cart hessian
c     call projct


c --- Global mapping, so that BM formation can use global atom index
c     Mapping the position vector to cartesian x, y, z
      do i = 1, nr3
         xc(i) = xx(3*i-2)
         yc(i) = xx(3*i-1)
         zc(i) = xx(3*i)
      end do



c --- In debug mode, print out the cartesian gradient
c     and connectivities

      if (icpr) then
         write(fu6, *) 'AFTER PROJCT:'
         write(fu6,'(/,
     *   '' *** (ICFDRP: UNSCALED CARTESIAN COORDINATES ***'')')
         write(fu6,'(3f20.9)')(xx(i),i=1,nr3)
      end if


c --- Make a local copy of the internal coordinate for jtype
      NBLR = NBLPJ(jtype)
      NBAR = NBAPJ(jtype)
      NTOR = NTOPJ(jtype)
      NLBER = NLBEPJ(jtype)

c --- make the local copy onf internal coordinate with renaming
c     to local atom index for each type. The purpose is to make
c     the atom index is no more than nratom(jtype), which is
c     compatible with the x, dx, dxx dimension.

      do i = 1, NBLR
          do j = 1, 2
              do k = 1, nratom(jtype)
                   if (IBLPJ(j, i, jtype).eq.iatsv(k, jtype)) then
                          IBLR(j, i) = k
                   end if
              end do
          end do
      end do

      do i = 1, NBAR
          do j = 1, 3
               do k = 1, nratom(jtype)
                    if (IBAPJ(j, i, jtype).eq.iatsv(k, jtype)) then
                           IBAR(j, i) = k
                    end if
               end do
          end do
      end do

      do i = 1, NTOR
          do j = 1, 4
               do k = 1, nratom(jtype)
                    if (ITOPJ(j, i, jtype).eq.iatsv(k, jtype)) then
                           ITOR(j, i) = k
                    end if
               end do
          end do
      end do

      do i = 1, NLBER
          do j = 1, 3
               do k = 1, nratom(jtype)
                    if (ILBEPJ(j, i, jtype).eq.iatsv(k, jtype)) then
                           ILBER(j, i) = k
                    end if
               end do
          end do
      end do


c --- Add a local reorder of the internal coordiate
c     for  the purpose of debugging, compare with a whole set
      if (reorder) then

          do i = 1, NBLR
              if (IBLR(1,i) .lt. IBLR(2,i)) then
                  call iswap(IBLR(1,i), IBLR(2,i))
              end if
          end do

          do i = 1, NBAR
              if (IBAR(1,i) .lt. IBAR(3,i)) then
                  call iswap(IBAR(1,i), IBAR(3,i))
              end if
          end do

          do i = 1, NTOR
              if (ITOR(1,i) .lt. ITOR(4,i)) then
                  call iswap(ITOR(1,i), ITOR(4,i))
                  call iswap(ITOR(2,i), ITOR(3,i))
              end if
          end do

          do i = 1, NLBER
              if (ILBER(1,i) .lt. ILBER(3,i)) then
                  call iswap(ILBER(1,i), ILBER(3,i))
              end if
          end do


c --- Order according to the increasing order of first atom
 300      invers=0
          do i=1,NBLR - 1
              icode1 = IBLR(1,i)*nr + IBLR(2,i)
              icode2 = IBLR(1,i+1)*nr + IBLR(2,i+1)
              if (icode1 .gt. icode2) then
                  invers = invers + 1
                  do j = 1, 2
                      call iswap(IBLR(j,i), IBLR(j, i+1))
                  end do
              end if
          end do
          if (invers .gt. 0) goto 300

 400      invers=0
          do i=1,NBAR - 1
              icode1 = IBAR(1,i)*nr + IBAR(2,i)*nr + IBAR(3,i)
              icode2 = IBAR(1,i+1)*nr + IBAR(2,i+1)*nr + IBAR(3,i)

              if (icode1 .gt. icode2) then
                  invers = invers + 1
                  do j = 1, 3
                      call iswap(IBAR(j,i), IBAR(j, i+1))
                  end do
              end if
          end do
          if (invers .gt. 0) goto 400

 500      invers=0
          do i=1,NTOR - 1
              icode1 =  ITOR(1,i)*nr + ITOR(2,i)*nr
     *                + ITOR(3,i)*nr + ITOR(4,i)
              icode2 =  ITOR(1,i+1)*nr + ITOR(2,i+1)*nr
     *                + ITOR(3,i+1)*nr + ITOR(4,i+1)
              if (icode1 .gt. icode2) then
                  invers = invers + 1
                  do j = 1, 4
                      call iswap(ITOR(j,i), ITOR(j, i+1))
                  end do
              end if
          end do
          if (invers .gt. 0) goto 500

600       invers = 0
          do i=1, NLBER-1
              icode1 =(ILBER(1,i)*nr + ILBER(2,i))*nr + ILBER(3,i)
              icode2 =(ILBER(1,i+1)*nr + ILBER(2,i+1))*nr + ILBER(3,i+1)

              if (icode1 .gt. icode2) then
                  invers = invers + 1
                  do j = 1, 3
                      call iswap(ILBER(j,i), ILBER(j,i+1))
                  end do
              end if
          end do
          if (invers .gt. 0) goto 600


      end if


c --- Check the internal coordinate using non-renaming atom index

      if (icpr.or.(lgs(4).ne.0.and.(mod(lsave,nprsmd).eq.0))) then

          do i = 1, NBLR
              do j = 1, 2
                  IBLG(j,i) = IBLPJ(j, i, jtype)
              end do
          end do
          do i = 1, NBAR
              do j = 1, 3
                  IBAG(j, i) = IBAPJ(j, i, jtype)
              end do
          end do
          do i = 1, NTOR
              do j = 1, 4
                  ITOG(j, i) = ITOPJ(j, i, jtype)
              end do
          end do
          do i = 1, NLBER
              do j = 1, 3
                  ILBEG(j, i) = ILBEPJ(j, i, jtype)
              end do
          end do

          call icoutrp(NBLR, NBAR, NTOR, NLBER,
     *                 IBLG, IBAG, ITOG, ILBEG,
     *                 IBLR, IBAR, ITOR, ILBER,
     *                 nr, xc, yc, zc, al)
      end if
      icpr = .false.                                                    0203PJ01


c --- Check unscaled gradients after PROJCT
      if (icpr) then
         write (fu6, *) 'AFTER PROJCT'
         write(fu6,'(/,
     *   '' *** (ICFDRP: UNSCALED CARTESIAN GRADIENTS ***'')')
         write(fu6,'(3f20.9)')(dxx(i),i=1,nr3)
         write(fu6,*)'NON-MASS-WEIGHTED FORCE CONSTANT MATRIX'
         call prntfc(fl,nr3,maxca2)
      end if


c --- The nintr in bmat can be preserved, coz the bmat is irow++
c     form, thus the bmat is continous saved, nintr is only for
c     define the bmat dimension, has nothing to do with index..

c --- Set the BMAT start index
      ibm=1

c --- Zero out the space for saving BMAT (eliminate 'save'effect)
      call zrout(nintr, nr*3+1, core(ibm))

c --- Calculate Wilson B matrix (dq/dx)
C     call bmat(NBLR, NBAR, NTOR, NLBER,
C    *          IBLR, IBAR, ITOR, ILBER,
C    *          xc, yc, zc, nr, nintr, core(ibm), t)
      call bmat(NBLR, NBAR, NTOR,NIMP,NLBER,
     *          IBLR, IBAR, ITOR,IMP,ILBER,
     *          xc, yc, zc, nr, nintr, core(ibm), t)                    0413JZ14

c --- Debugging Pu, check BMAT
      if (icpr) then
           nbmat = nintr*ncartr
           write(fu6, *) 'RiGHT AFTER BMAT, THE CORE IS : '
           write(fu6, 30) (core(i), i = 1, nbmat)
 30        format(1x, 5e20.7)
      end if

c --- adding 1 to indicate dy/dy for collective solvent coordinate
c     since the solvent mode is assumed to be an internal coordinate
c     other derivatives are zeros, same as the elements in C tensors.
c     This is appended at the Bmat with size nintr*ncartr
      if (lbath) then
          core(nintr*ncartr) = 1.0d0
      end if





c --- Calculate the generalized inverse of Wilson matrix A (dx/dr)
c     The starting index is set right after the maximum BMAT
      ibi = ibm + maxcar * maxint

      if (redun) then
          call bimat2 (core(ibm), core(ibi), amassx,
     *                 amasin, gm, ggi, flgm, egnm, redm,
     *                 nr, nintr, ncartr)
      else
          call bimat (core(ibm),core(ibi),amassx,
     *                amasin, gm, redm,
     *                nr, nintr, ncartr)
      endif

c --- Debugging Pu, check BIMAT
      if (icpr) then
          nbimat = nintr * ncartr
          write(fu6, *) 'THE BIMAT IS :'
          write(fu6, 40) (core(i), i = ibi, ibi + nbimat - 1)
 40       format(1x, 5e20.7)
      end if

c --- Debugging Pu, check the eigen vector and eigen value
      if (icpr) then
          write(fu6,'(/,
     *   '' *** (ICFDRP: FLGM VECTOR ***'')')
          do i = 1,nintr
              write (fu6,*) 'EIGENVALUE ', i,' = ',flgm(i)
          end do
          write(fu6,'(/,
     *    '' *** (ICFDRP: EGNM - MATRIX ***'')')
          call prsq(egnm, nintr, nintr, nintr, nintr,6)
      end if




c --- Convert gradients in cartestians to internal coordinates
      call trang (dxint, core(ibi),dxx, maxint,
     *            maxcar, nintr, ncartr)

c --- Debugging Pu, check the internal gradient DXINT
      if (icpr) then
         write(fu6,'(/,
     *   '' *** (ICFDRP: B - MATRIX ***'')')
         call prsq(core(ibm),nintr, ncartr, nintr, ncartr,6)
         write(fu6,'(/,
     *   '' *** (ICFDRP: A - MATRIX ***'')')
         call prsq(core(ibi), nr3, nintr, ncartr, nintr, 6)
         write(fu6,'(/,
     *   '' *** (ICFDRP: INTERNAL COORD GRAD ***'')' )
         write(fu6,'(5e15.6)') (dxint(i), i=1,nintr)
      end if





c --- Calculate the tensor (x/r.r^2/x^2.dx/dr, C mat?)
c     The starting index is right after the maximum BIMAT
      ibt = ibi+ maxcar * maxint
      call btens (NBLR, NBAR, NTOR, NLBER,
     *            IBLR, IBAR, ITOR, ILBER,
     *            xc, yc, zc, nr, core(ibi), core(ibt),
     *            nintr, ncartr, maxint, maxin2, t)





c --- The if2 starting index is after the maximum C tensor
      if2 = ibt + maxin2*maxint

C --- Form the second term of internal force constant matrix
      call formf2 (core(if2), core(ibt), dxint, maxint,
     *             maxin2, nintr)

c --- Debugging Pu, check FORMF2
      if (icpr) then
          write(fu6,*)'finter: g*BT Matrix'
          call prntfc(core(if2), nintr, maxin2)
      end if

c --- Tranform cartesian FC matrix into internal coordinate
      ihfc = if2 + maxin2
      iwork = ihfc + maxin2

      call tranfc (fl, core(ibi), core(iwork), core(ihfc),
     *             core(if2), nr, maxint, maxcar, maxin2,
     *             nintr, ncartr)

c --- Expand to complete force constant matrix
      call expnd (core(ihfc), fintr, nintr,0)

c --- back up varibles if scaling frequencies is used                   0215PJ01
      if (lgs3(1) .eq. 1) then
          do i = 1, maxint
              dxintb(i) = dxint(i)
              flgmb(i) = flgm(i)
          end do

          do i = 1, n3tm * maxint
              scr1b(i) = scr1(i)
              scr2b(i) = scr2(i)
              scr3b(i) = scr3(i)
          end do 
   
          do i = 1, n3tm * n3tm
              avecb(i) = avec(i)
              amasinb(i) = amasin(i)
          end do 
        
          do i = 1, maxint*maxint
              fintrb(i) = fintr(i)
              ggib(i) = ggi(i)
              gmb(i) = gm(i)
              egnmb(i) = egnm(i)
          end do
      end if 

c --- Print the FC matrix in internal coordinate
      if (icpr) then

         if(lgs3(1) .ne. 0) then                                        0211PJ01
             write(fu6, *) 'BEFORE SCALING FORCE CONSTANT: '
         end if 

         write(fu6,'(/,
     *   '' *** (ICFDRP: NON-MASS-WEIGHTED INTERNAL F-MATRIX ***'')')
         call prntfc(core(ihfc), nintr, maxin2)
         write(fu6,'(/,
     *   '' *** (ICFDRP: FULL F-MATRIX ***'')')
         call prsq(fintr, nintr, nintr, nintr, nintr,6)
         write(fu6,'(/,
     *   '' *** (ICFDRP: G-MATRIX ***'')')
         call prsq(gm,nintr, nintr, nintr, nintr, fu6)
      end if


c --- Scaling the force constant matrix                                 0215PJ01
      if( lgs3(1) .ne. 0) then               
         call fcscl(fintr, nintr, jtype)                              
      end if

c --- Output the force constant after scaling
      if (icpr .and. lgs3(1) .ne. 0) then                               0211PJ01
         write(fu6,*) 'AFTER SCALING : '
         write(fu6,'(/,
     *   '' *** (ICFDRP: NON-MASS-WEIGHTED INTERNAL F-MATRIX ***'')')
         call prntfc(core(ihfc), nintr, maxin2)
         write(fu6,'(/,
     *   '' *** (ICFDRP: FULL F-MATRIX ***'')')
         call prsq(fintr, nintr, nintr, nintr, nintr, 6)
         write(fu6,'(/,
     >   '' *** (ICFDRP: g-MATRIX ***'')')
         call prsq(gm, nintr, nintr, nintr, nintr, fu6)
      end if



c --- For scaled force constants                                        0213PJ01
      if (lgs3(1) .ne. 0) then


c --- When the scaling FC is used, we project the scaled copy of        0213PJ01
c     FC matrix fintrsc

          if (redun) then                             
              call projre (dxint, ggi, fintr, scr1, nintr)              0214PJ01  
              if (icpr) then
                  write(fu6,*)'For scaled FC matrix'
                  write(fu6,'(/,
     *       '' *** (ICFDRP: INTERNAL COORD GRAD (AFTER PROJRE) ***'')')     
                  write(fu6,'(5e15.6)') (dxint(i), i=1,nintr)           0214PJ01
                  write(fu6,'(/,
     *       '' *** (ICFDRP: FULL F-MATRIX (AFTER PROJRE) ***'')')
                 call prsq(fintr, nintr, nintr, nintr, nintr,6)
              end if
          end if


c --- Diagnalize the scaled Force constants to obtain the scaled freq   0213PJ01
          if (redun) then
              call gfdia2 (issad, fintr, freqisc, avec, flgm, egnm,
     *                     scr1, scr2, scr3, nintr)
          else
              call gfdiag (issad, gm, fintr, freqisc, avec,
     *                     scr1, scr2, scr3, nintr)
          endif


      end if




c --- For unscaled force constants
      if (lgs3(1) .lt. 2) then
        

c --- Recover the scratch space if the scaled frequencies is used       0215PJ01
          if (lgs3(1) .eq. 1) then
              do i = 1, maxint
                  dxint(i) = dxintb(i)
                  flgm(i) = flgmb(i)
              end do

              do i = 1, n3tm * maxint
                  scr1(i) = scr1b(i)
                  scr2(i) = scr2b(i)
                  scr3(i) = scr3b(i)
              end do
  
              do i = 1, n3tm * n3tm
                  avec(i) = avecb(i)
                  amasin(i) = amasinb(i)
              end do

              do i = 1, maxint*maxint
                  fintr(i) = fintrb(i)
                  ggi(i) = ggib(i)
                  gm(i) = gmb(i)
                  egnm(i) = egnmb(i)
              end do
          end if



c --- For redundant internal coordinates, we have to project to the     0213PJ01
c     nonredundant internal coordinate system to perform GF analysis
c     when scaling force constant is not used or scale only the freq

          if (redun) then
             call projre (dxint, ggi, fintr, scr1, nintr)
             if (icpr) then
                 write(fu6,*) 'For the unscaled FC matrix'
                 write(fu6,'(/,
     *       '' *** (ICFDRP: Internal coord grad (after projre) ***'')')
                 write(fu6,'(5e15.6)') (dxint(i), i=1,nintr)
                 write(fu6,'(/,
     *       '' *** (ICFDRP: full f-matrix (after projre) ***'')')
                 call prsq(fintr, nintr, nintr, nintr, nintr,6)
             end if
          end if

  

c --- Diagnailze the unscaled force constant matrix to get ordinary     0213PJ01
c     normal mode eigen vectors if the scaling force constant is     
c     used only for potential or force constant scaling is not used 

          if (redun) then                                         
              call gfdia2 (issad, fintr, freqi, avec, flgm, egnm,
     *                     scr1, scr2, scr3, nintr)
          else
              call gfdiag (issad, gm, fintr, freqi, avec,
     *                     scr1, scr2, scr3, nintr)
          end if

    
      end if 


c --- Save the scaled frequencies to the normal work space              0213PJ01
      if (lgs3(1) .ne. 0) then                                         
          do i = 1, n3tm                                              
              freqi(i) = freqisc(i)                                  
          end do                                                    
      end if                                                       




c --- Debugging Pu, check frequencies, print results
      if (icpr) then
         write(fu6, 3000)(autocm*freqi(i),i=1,nintr)
         write(fu6, 4000)
         call prsq(avec, nintr, nintr, nintr, nintr, fu6)
 3000    format(/1x,'VIBRATIONAL FREQUENCIES IN CM-1',
     *   /,(1x,4f19.3))
 4000    format(/1x,'UNNORMALIZED NORMAL MODE VECTORS')
      end if


c --- Convert the eigenvectors from internal to mass wgted cartesian coordinates
      call veccon(avec, gm, core(ibi), scr1,scr2,scr3,
     *            amassx, nintr, ncartr, redun)


c --- Move the unbound mode to first position for later calculation
c     Comment this section for R. P.
      if (issad) then
          freq(1) = freqi(1)
          do j = 1, ncartr
              cof(j,1) = avec(j)
          end do
      end if


c --- check the vibrational freq before removing the zero
      if (icpr) then
          write(fu6, *) 'BEFORE REMOVEC IS CALLED: '
          write(fu6, *)  (freqi(i), i = 1, nr3)
      end if

c --- Debugging
      if (icpr) then
          write(fu6, *) 'ICODE(JTYPE) IS : ', icode(jtype)
      end if

c --- If redundant coordinate used, remove the eigenvectors with zero eigenvalues
      if (redun) then
         if (icode(jtype).eq.2.or.icode(jtype).eq.3) then
            nintr = nr3 - 5
         else
            nintr = nr3 - 6
         end if
         call removec (avec, freqi, scr3, nintr, numintr, ncartr)
      end if

c --- Check freq after removec is called.
      if (icpr) then

          write(fu6, *) 'After REMOVEC is called: '
          write(fu6, *)  (freqi(i), i = 1, nr3)
      end if



c --- Debugging Pu, print result before transfer data to the normal work space
      if (icpr) then
          write(fu6, 5000)
          call prsq (avec, ncartr, nintr, ncartr, nintr, fu6)
 5000     format(/1x,'AFTER VECCON, NORMAL MODE VECTORS IN MW CARTS')
      end if



c --- Store the frequencies eigenvectors into the expected region,
c     overwriting the cartesian information already obtined.
c     Save freq and cof in common block

      call icsvrp(issad, iop, avec, freqi, scr2, scr3, scr1,
     *            nintr, ncartr)



c --- Check the freq in normal working space
      if (icpr) then
          write(fu6, *) 'AFTER ICSVRP IS CALLED :'
          write(fu6, *) (freq(i), i = 1, nr3)
      end if


c --- Check the normal mode eigenvector in normal working space
      if (icpr) then
          write(fu6, 6000)
          call prsq(cof, ncartr, ncartr, n3tm, n3tm,fu6)
 6000     format(/1x,'FULL SET OF CART NORMAL MODE VECTORS ')


c --- Check the orthogonality of final vectors
         inds = 1
         do i = 1, ncartr
             do j = 1, ncartr
                  scr1(inds) = cof(j, i)
                  inds = inds + 1
             end do
         end do


c --- Form the overlap matrix of the vectors
         call matx (scr2, 1, scr1, 0, scr1, 1,
     *              ncartr, ncartr, ncartr, ncartr)

         write(fu6, 7000)
 7000    format(/1x,'OVERLAP MATRIX')
         call prsq(scr2, ncartr, ncartr, ncartr, ncartr, fu6)

      end if

c --- ALL DONE



      return
      end subroutine icfdrp

c
c***********************************************************************
c     ICSVRP
c***********************************************************************
c
      subroutine icsvrp(issad,iop,cofint,frqint,tcof,scr1,scr2,n1,n2)
      use common_inc
      use perconparam, only : fu6
      use kintcm
      use rate_const
c
c     Created by: Jingzhi Pu, Jan. 25, 2001 
c
c     ICSVRP copies frequencies and eigenvectors for bound
c     modes from internal coordinate work spaces to usual
c     spaces used by cartesian coordinate procedures.
c
c     Called by:
c              icfdrp
c
c     Calls:
c          matx,jacscf,absord,ovrlp
c
      implicit double precision (a-h,o-z)
c
      dimension cofint(n2,n1),frqint(n1),tcof(n2),scr1(n2,n2)
     *          ,scr2(n2*n2),iscr(n2*n2)

      logical lwrite
      logical issad    
      logical icpr 
      data epsx/1.0d-10/,zero/0.0d00/,one/1.0d00/

c --- For reactants and products
      issad = .false.   
      icpr = .false.


c --- Initialize
      if (issad) then
          ibbeg = 2     
      else         
          ibbeg = 1 
      end if      
      lgs4 = lgs(4)
      lgs9 = lgs(9)
      if(iabs(lgs9).ge.2) lwrite = lgs4.gt.0 .or.
     *    (sob.lt.soe .and. s.ge.sob .and. s.le.soe) .or.
     *    (soe.lt.sob .and. s.ge.soe .and. s.le.sob)

      nc = n2
      ni = n1
  
      jtype = iabs(iop) 
      nr = nratom(jtype)  
      nr3 = nr * 3       

      n3m7 = nf(jtype)  
      nend = nr3       

c --- Check molecule type        
      if (icpr) then
          write(fu6, *) 'FOR JTYPe = ', jtype, ' N3M7 IS ', n3m7   
          write(fu6, *) 'FOR JTYPE = ', jtype, ' NEND IS ', nend  
      end if 


c --- Zero out the coefficient matrix
      do i = ibbeg, nc                                                
          do j = 1, n3tm
              cof(j, i) = zero
          end do  
      end do
  

c --- Form the projection operator for the bound mode vibrational
c     normal mode eigen vectors
c     scr2 = cofint * cofint(tr)

      ishfti = 2
      call matx (scr2,-1,cofint(1,ishfti),1,cofint(1,ishfti),0,
     *           nc,ni-1,nc,ni-1)



c --- Check projection operator
      if (icpr) then
          write (fu6, *) 'FOR JTYPE: ', jtype
          write (fu6, *) 'AFTER PROJECTION OPERATOR IS FORMED'
          write (fu6, 111) (scr2(i), i=1, nc*nc)
 111      format (1x, 5e20.7)
      end if
     

c --- Subtract the projector from the unit matrix
      inds = 0
      do i =1, nc
          do j = 1, i-1
            scr2(inds + j) = -scr2(inds + j)
          end do 
          scr2(inds + i) = one - scr2(inds + i)
          inds = inds+i
      end do



c --- Check 
      if (icpr) then
          write(fu6, *) 'AFTER SUBSTRACT THE PROJECTOR'
          write (fu6, 112) (scr2(i), i=1, nc*nc)
 112      format (1x, 5e20.7)
      end if 




c --- Diagonalize the projected unit matrix and order the resulting
c     eigenvalues and eigenvectors.  the projected bound modes should
c     correspond to zero eigenvectors

      call jacscf (scr2,scr1,tcof,nc,-1,epsx)





c --- Check JACSF 
      if (icpr) then
          write(fu6,*) 'AFTER JACSCF, SCR2 IS :'
          write (fu6, 113) (scr2(i), i=1, nc*nc)
 113      format (1x, 5e20.7)

          write(fu6,*) 'AFTER JACSCF, SCR1 IS :'
          write (fu6, 114) ((scr1(i,j), i=1, nc), j= 1, nc)
 114      format (1x, 5e20.7)
      end if 

      iscr=0
      call absord (scr1,tcof,iscr,scr2(nc+1),nc,nc,nc)


c --- Debugging                                                         0127PJ01
      if (icpr) then
          write(fu6,*) 'AFTER ABSORD, SCR1 IS :'
          write (fu6, 115) ((scr1(i,j), i=1, nc), j= 1, nc)
 115      format (1x, 5e20.7)
      end if


c --- Place the eigenvectors of the unbound modes into the first locations
c     of cof.  these will correspond to the nonzero eigenvalues of the
c     projected unit matrix.  as eiegenvectors of the projected force constant
c     matrix they will be assigned zero eigenvalues in freq.

      iend = nend - n3m7
      ishfti = n3m7
      do i = ibbeg, iend     
          freq(i) = zero
          do j = 1, nend
              cof(j,i) = scr1(j,ishfti+i)
          end do
      end do
 
c --- Fill the rest of the cof and freq arrays with the vectors and
c     frequencies determined during the internal coordinate normal mode
c     analysis
 
      ishftc = nend - n3m7

c --- For R. P., ibbeg is 2, n3m7 is n3-7, frqint and frqcof            0128PJ01
c     should be count from the begining, not unbound mode               0128PJ01
      ishfti = 0                                                        0128PJ01

c     ishfti = 1
      do i = 1, n3m7
          freq(ishftc + i) = frqint(ishfti + i)
          do j = 1, nend
              cof(j, ishftc+i) = cofint(j, ishfti+i)
          end do

c --- Debugging
          if (icpr) then
              write(fu6, 116) (cof(j,ishftc+i), j = 1, nend)            0128PJ01
          end if

      end do

c --- debugging
      if (icpr) then   
          write(fu6, *) 'THE COFINT MAT IS :'                           0128PJ01
          write(fu6, 116) ((cofint(i,j), i = 1, nc), j = 1, ni)         0128PJ01 
 116      format(1x, 6e20.7)                                            0128PJ01
      end if






c --- Check the freq in normal working space                            0127PJ01
      if (icpr) then
          write(fu6, *) 'BEFORE SORT IN ICSVRP, FREQ :'                 0127PJ01
          write(fu6, *) (freq(i), i = 1, nr3)                           0127PJ01


c --- Check the normal mode vector                                      0128PJ01
          write(fu6, *) 'NORMAL MODE VECOTR IN COF'                     0128PJ01           
          write(fu6, 117) ((cof(i,j),i=1, nc), j=1, nc)                 0128PJ01
 117  format(1x, 6e20.7) 

c --- Check the normal mode eigenvector in normal working space         0127PJ01
c     must have a bug, because before ICSVRP, the vector is OK          0127PJ01
c     Attention: the COF is declared as N3 * N3 mat
          write(fu6, 6000)                                              0127PJ01
          call prsq(cof, nc, nc, n3tm, n3tm,fu6)                        0129PJ01
 6000     format(/1x,'NORMAL MODE VECTOR IN ICSVRP: ')                  0127PJ01

      end if

c --- Add a sort to the bounded modes    

      do i = 1, n3m7 - 1    
        do j = i+1, n3m7                                            
           if (freq(ishftc+i) .gt. freq(ishftc+j)) then               
                tempf = freq(ishftc+i)                             
                freq(ishftc+i) = freq(ishftc+j)                   
                freq(ishftc+j) = tempf                           
                do k = 1, nend           
                    tmcof = cof(k,ishftc+i)    
                    cof(k,ishftc+i) = cof(k, ishftc+j)     
                    cof(k,ishftc+j) = tmcof    
                end do                       
           end if                           
        end do                             
      end do                              



c --- Check the freq in normal working space                            0127PJ01
      if (icpr) then
          write(fu6, *) 'AFTER SORTING IN ICSVRP, FREQ :'          
          write(fu6, *) (freq(i), i = 1, nr3)                     

          write(fu6, 6001)                                  
          call prsq(cof, ncartr, ncartr, n3tm, n3tm,fu6)   
 6001     format(/1x,'AFTER SORTING IN ICSVRP, NORMAL VECTOR: ')   
      end if



c --- The following code is lifted from subroutine FDIAG (polyag.f)
c     it reorders the vectors/frequencies according to the
c     conventions expected by polyrate
c     Calculate the overlap integral and check the frequency matching

      if (iop.gt.0 .and. lgs(9).ne.0 .and. indph.ne.0 .and. ireord.ne.0)     
     *   call ovrlp(lwrite,nr3,n3m7,cof,cofx,freq,s)


c --- Change phase of each eigenvector so that the largest component is
c     always positive

      do j = 1, nend

          cofmax = 0.0d0
          imaxx = in1(j)
          if (iabs(lgs(9)).ge.2.and.indph.ne.0)
     *        a = sgn1(j) *sign(1.0d0,cof(imaxx,j))

          do i = 1, nend
              if (dabs(cof(i,j)) .gt. (cofmax+epsx)) then
                  cofmax = dabs(cof(i,j))
                  imaxx = i
              end if
          end do

          if (iabs(lgs(9)).lt.2 .or. indph.eq.0)
     *        a = dsign(1.0d0,cof(imaxx,j))

          do i = 1, nend
              cof(i,j) = a * cof(i,j)
              cofx(i,j) = cof(i,j)
          end do

          if (iabs(lgs(9)) .ge. 2) then
              in1(j) = imaxx
              sgn1(j) = dsign(1.0d0,cof(imaxx,j))
          end if   

      end do


      if (iop .gt. 0 .and. indph .eq. 0)  then

          do i = 1, nend
              do k = 1,nend
                  csv(k,i) = cof(k,i)
              end do
              if (iabs(lgs(9)) .ge. 2) then
                  sgn2(i) = sgn1(i)
                  in2(i) = in1(i)
              end if
          end do

      elseif (iop .lt. 0) then

          do k = 1, nend
              do i = 1, n3
                  tcof(i) = 0.d0
              end do

              do i = 1, nend
                  tcof(ind(i)) =  cof(i,k)
              end do

              do i = 1, n3
                    cof(i,k) = tcof(i)
              end do
          end do

      end if

      return
      end subroutine icsvrp

c
c***********************************************************************
c     SWAP           
c***********************************************************************
c
      subroutine swap (i, j)
c
c     Created by: Jingzhi Pu, Feb 01, 2001.
c
c     Called by:
c              icfdrp 
c
      implicit double precision (a-h, o-z)
c
      itemp = i
      i = j
      j = itemp

      return
      end subroutine swap


c
c***********************************************************************
c     ZROUT
c***********************************************************************
c
      subroutine zrout(ni, nj, A)
c  
c     Created by: Jingzhi Pu, Feb. 02, 2001.
c
c     Called by:
c              icfdrp 
c
      implicit double precision (a-h, o-z)
c
      dimension A(ni, nj)
c
      do i = 1, ni
          do j = 1, nj
              A(i, j) = 0.0d0
          end do
      end do

      return
      end subroutine zrout


c
c***********************************************************************
c     TRLFRP                                                               
c***********************************************************************
c
      subroutine trlfrp(n3,fl,f)
      use perconparam
c   
c     Created by: Jingzhi Pu, Jan 25, 2001. 
c
c     Called by:
c              ICFDRP (replaced  by TRANLF at Jan 30.)
c
      implicit double precision (a-h,o-z)
c
      dimension fl(maxca2),f(n3tm,n3tm)                                 
c

c --- Save the lower triangle of f to fl

      ip = 1
      do i=1, n3
         do j = 1, i
            fl(ip) = f(i,j) 
            ip = ip + 1
         end do
      end do

      return
      end subroutine trlfrp

c
c********************************************************************** 
c     FACORD                                                           
c**********************************************************************
c
      subroutine facord
      use common_inc
      use perconparam
      use rate_const
c   
c     Created by : Jingzhi Pu, Jan. 2001                              
c
c     Order the force constant scaling factor accordint to the       
c     actual sequence of internal coordinates                       
c
c     Called by:                                                   
c            intcor
c  
      implicit double precision (a-h,o-z)                                                      

      dimension FACBC(MAXINT)                                           0115PJ02 


c --- Back up the scaling factor array                      

      do i = 1, NUMINT
          FACBC(i) = FCFAC(i)
      end do

 
c --- Set fc scaling index counter                         

      indfc = 0                                           


c --- Find out the sequence of coordinate to be scaled,  
c     which is saved in inpbl, inpba, inpto and inplbe. 
c     set the scaling factors to correspoding coordingate  

      do i = 1, NBL                                       
          indfc = indfc + 1                              
          FCFAC(indfc) = FACBC(INPBL(i))                
      end do                                           

      do j = 1, NBA                                   
          indfc = indfc + 1                          
          FCFAC(indfc) = FACBC(INPBA(j))
      end do                                        

      do k = 1, NTO                                
          indfc = indfc + 1
          FCFAC(indfc) = FACBC(INPTO(k))          
      end do                                     

      DO L = 1, NLBE                            

c --- Double count the linear bends in fc matrix,   
c     so the increasing neighbor in fcfac has the same 
c     scaling factor                                  

          indfc = indfc + 1                          
          FCFAC(indfc) = FACBC( INPLBE(L) )         

          indfc = indfc + 1                        
          FCFAC(INDFC) = FACBC( INPLBE(L) )       
      end do                                     

      return
      end subroutine facord


c 
c**********************************************************************
c     FCSCL                                                           
c********************************************************************** 
c
      subroutine fcscl(FINT,NINT,jtype)                                
      use rate_const
      use perconparam
c
c     Created by : Jingzhi Pu, Jan. 2001
c
c     Scaling the internal coordinate force constant                  
c
c     jtype --- 1~4 Reactants and products                           
c           --- 5,6 saddle point and GTS                            
c           --- 7,8 reactant and product well                      
c
c     Called by:                                                  
c              icfdiag, cvcoor                                   
c 
      implicit double precision (a-h,o-z)      
c
      dimension FINT(NINT, NINT)            

C --- Mutiply the diagnal elements of the fc matrix with    
c     scaling fcators                                      

      if ((jtype .eq. 5) .or. (jtype .eq. 6))then         
          do i = 1, NINT                                 
              FINT(i,i) = FINT(i,i) * FCFAC(i)          
          end do                                       
      else if (jtype .le. 4) then                     
          do i = 1, NINT                             
              FINT(i,i) = FINT(i,i) * FCFACPJ(i, jtype)   
          end do                                         
      end if                                            

      return                                           
      end subroutine fcscl




c
c********************************************************************** 
c     RFCFAC                                                           
c**********************************************************************
c
      subroutine rfcfac(ierr)
      use rate_const
      use perconparam
c
c     Created by : Jingzhi Pu, Jan. 2001
c
c     subroutine to read force constant scaling factors in           
c     FCSCALE section. The scaling factor is described in           
c     a list varible, which has a integer index indicating which   
c     internal coordinate is supposed to be scaled. The sequence  
c     of the index is according to the defination of the internal   
c     coordinate in the path section. Then the index is followed by
c     a float type number as the scaling factor for the spcific   
c     internal coordinate. The default scaling factor for all    
c     internal coordinate is 1.0d0.                             
c
c     Called by :
c          read5
c
c     Calls :  
c          rword
c
      implicit double precision (a-h,o-z)                                          
      character *80 string                                 

      ierr = 0                                            

c --- Set all the scaling factors to default value 1.0d0 
      do i = 1, MAXINT                                  
          fcfac(i) = 1.0d0                             
      end do                                          


c --- Read index of the internal coordinate and scaling factors  
c     an end is found                                           

      icount = 0                                               
      call rline(fu5,string,istrt,isect,iend)                 
      do while (string(istrt:istrt+2).ne.'end')              

c --- First read in integer index as coordniate sequence number         

          ib = istrt                                                   
          call rword(string,istrt,ierr)                               

c --- Give out the erro message and quit  
          if (ierr.eq.1) then            
              write(fu6,1000)           
1000          format(3x,'ERROR:  must specify the scaling factor for',  
     *             ' the specified internal coordinate')               
              stop                                                    
          end if

          ie = istrt - 1                                             
          icord = icint(string(ib:ie))                              

c --- Check the index range                                             0621PJ01
          if (icord .gt. NINP) then                                     0621PJ01 
              write(fu6, 1100)                                          0621PJ01
              stop                                                      0621PJ01 
          end if                                                        0621PJ01  
1100  format(3x, 'ERROR:  The index of internal coordinate in',         0621PJ01
     *       ' FCSCALE is out of range.')                               0621PJ01
 
c --- Assign the scaling factors    
          fcfac(icord) = cfloat(string(istrt:80))  


c --- Check the number of coordinate to be scaled  
          icount = icount + 1                     

c --- Read in the next force constant to be scaled                
          call rline(fu5,string,istrt,isect,iend)                

      end do   

c --- set the nubmer of coordinate to be scaled                         0217PJ01
      NSCFAC = icount                                                   0217PJ01

      return                     
      end subroutine rfcfac


c
c***********************************************************************
c     INTCOR
c*********************************************************************** 
c
      subroutine intcor(iop)
      use common_inc
      use perconparam
      use rate_const
c
c     Created by : Jingzhi Pu, Jan. 2001
c     
c     This subroutine is used for define the internal coordinates of 
c     reactants and products by spliting the redundant internal coor-
c     dinates of transition state into subsets. In such subsets for 
c     reactants and products, only those internal coordinates involv
c     the component atom in corresponding species are kept. a
c
c     This subroutine is a prerequisite of the normal mode analysis
c     of reactants and products in internal coordinates.
c
c     jtype = 1, 2    the first and second reactants 
c     jtype = 3, 4    the first and second products
c
c     iop is option for wether the scaling force constants are use
c     iop --- 0, don't scaling the FC, set FCFAC array to 1.0d0
c     iop --- 1, scaling FC factors have already been read, split
c                them along the R., P. internal coordinates as well 
c     
c     This subroutine is only called after the internal coordinates     0217PJ01 
c     are already defined by INTDEF                                     0217PJ01
c 
c     Called by :
c               rpath 
c
c     Calls : 
c            facord
c
      implicit double precision (a-h, o-z)
c
      logical subat, subset, icpr
      dimension IBLR(2,MAXINT), IBAR(3, MAXINT),                        0119PJ01 
     *           ITOR(4,MAXINT), ILBER(3,MAXINT)                        0119PJ01
c

      icpr = .false.                                                    0203PJ01  

c --- If scaling factor not read in yet, set them to unity              0217PJ01
c     If already be read in, then reorder it against the actual         0217PJ01
c     interal coordinates            
      if (iop .eq. 0) then                                              0217PJ01
          do i = 1, NUMINT                                              0217PJ01
              FCFAC(i) = 1.0d0                                          0217PJ01
          end do                                                        0217PJ01
      else if (iop .eq. 1) then                                         0217PJ01
          call facord                                                   0217PJ01
      end if                                                            0217PJ01



c --- Initilize all the counters
      do jtype = 1,4 
          NBLPJ(jtype) = 0
          NBAPJ(jtype) = 0
          NTOPJ(jtype) = 0
          NLBEPJ(jtype) = 0
      end do

c --- Initialize the force constant scaling factor to 1.0d0
      do i = 1, MAXINT
          do j = 1, 4
              FCFACPJ(i, j) = 1.0d0
          end do
      end do
   

      do jtype = 1, 4

c --- Force constant scaling factor counter
          indfc = 0

c --- Split bond link to subset for each species
          do i = 1, NBL

              j = 1
              subset = .true.

              do while ((j .le. 2) .and. (subset .eqv. .true.))

                  k = 1
                  subat = .false.

                  do while ((k .le. nratom(jtype)) .and.
     *                      (subat .eqv. .false.))
                      if (IBL(j, i) .eq. iatsv(k, jtype)) then
                          subat = .true.
                      end if
                      k = k + 1
                  end do

                  if (subat .eqv. .false.) then
                      subset = .false.
                  end if

                  j = j + 1
              end do

              if (subset .eqv. .true.) then

                  NBLPJ(jtype) = NBLPJ(jtype) + 1

                  do j = 1, 2
                       IBLPJ(j, NBLPJ(jtype), jtype) = IBL(j,i)
                  end do

                  indfc = indfc + 1
                  FCFACPJ(indfc, jtype) = FCFAC(i)

              end if
          end do

c --- Split bond angle to subset for each species
          do i = 1, NBA

              j = 1
              subset = .true.

              do while ((j .le. 3) .and. (subset .eqv. .true.))

                  k = 1
                  subat = .false.

                  do while ((k .le. nratom(jtype)) .and.
     *                      (subat .eqv. .false.))
                      if (IBA(j, i) .eq. iatsv(k, jtype)) then
                          subat = .true.
                      end if
                      k = k + 1
                  end do

                  if (subat .eqv. .false.) then
                      subset = .false.
                  end if

                  j = j + 1
              end do

              if (subset .eqv. .true.) then

                  NBAPJ(jtype) = NBAPJ(jtype) + 1

                  do j = 1, 3
                       IBAPJ(j, NBAPJ(jtype), jtype) = IBA(j,i)
                  end do

                  indfc = indfc + 1
                  FCFACPJ(indfc, jtype) = FCFAC(NBL+i)

              end if
          end do


c --- Split torsion to subset for each species
          do i = 1, NTO

              j = 1
              subset = .true.

              do while ((j .le. 4) .and. (subset .eqv. .true.))

                  k = 1
                  subat = .false.

                  do while ((k .le. nratom(jtype)) .and.
     *                      (subat .eqv. .false.))
                      if (ITO(j, i) .eq. iatsv(k, jtype)) then
                          subat = .true.
                      end if
                      k = k + 1
                  end do

                  if (subat .eqv. .false.) then
                      subset = .false.
                  end if

                  j = j + 1
              end do

              if (subset .eqv. .true.) then

                  NTOPJ(jtype) = NTOPJ(jtype) + 1

                  do j = 1, 4
                       ITOPJ(j, NTOPJ(jtype), jtype) = ITO(j,i)
                  end do

                  indfc = indfc + 1
                  FCFACPJ(indfc, jtype) = FCFAC(NBL+NBA+i)               

              end if
          end do


c --- Split doubly degenerated linear bend to subset for each species
          do i = 1, NLBE

              j = 1
              subset = .true.

              do while ((j .le. 3) .and. (subset .eqv. .true.))

                  k = 1
                  subat = .false.

                  do while ((k .le. nratom(jtype)) .and.
     *                      (subat .eqv. .false.))
                      if (ILBE(j, i) .eq. iatsv(k, jtype)) then
                          subat = .true.
                      end if
                      k = k + 1
                  end do

                  if (subat .eqv. .false.) then
                      subset = .false.
                  end if

                  j = j + 1
              end do

              if (subset .eqv. .true.) then

                  NLBEPJ(jtype) = NLBEPJ(jtype) + 1

                  do j = 1, 3
                       ILBEPJ(j, NLBEPJ(jtype), jtype) = ILBE(j,i)
                  end do
              
                  do k = 1, 2  
                     indfc = indfc + 1
                     FCFACPJ(indfc,jtype) = FCFAC(NBL+NBA+NTO+2*(i-1)+k)     
                  end do

              end if
          end do


      end do

c --- calculate the internal coordinate number for each species
      do jtype = 1, 4 
          NINTPJ(jtype) =  NBLPJ(jtype) + NBAPJ(jtype)
     *                   + NTOPJ(jtype) + 2 * NLBEPJ(jtype)
      end do



c***************************************************************************
c --- DEBUG  PU
c     check the spliting of internal cooridinate and corresponding force
c     constant scaler factor

      
      if (icpr) then   


c --- Check the internal coordinate sequence for each speciese

      do jtype = 1, 4 
    
      indfc = 0                                    
  
      write (fu6, *) '--- jtype = ', jtype

      write(fu6,*) 'BOND LINK'                                     
      do i = 1, NBLPJ(jtype)
          indfc = indfc + 1                                                    
          write(fu6,*) indfc, ': ', IBLPJ(1,i,jtype), 
     *                 '-', IBLPJ(2,i,jtype), ' FC: ',
     *                 FCFACPJ(indfc, jtype) 
      end do                                                          

      write(fu6,*) 'BOND ANGLE'                                      
      do i = 1, NBAPJ(jtype)        
          indfc = indfc + 1                                        
          write(fu6,*) indfc, ': ', IBAPJ(1,i,jtype),  
     *                 '-', IBAPJ(2,i,jtype), '-', IBAPJ(3,i,jtype),       
     *                 ' FC: ', FCFACPJ(indfc, jtype) 
      end do                                                     

      write(fu6,*) 'TORSION'                                         
      do i = 1, NTOPJ(jtype)                                                 
          indfc = indfc + 1
          write(fu6,*) indfc, ': ', ITOPJ(1,i,jtype), 
     *                 '-', ITOPJ(2,i,jtype), '-', ITOPJ(3,i,jtype), 
     *                 '-', ITOPJ(4,i,jtype), ' FC: ', 
     *                 FCFACPJ(indfc, jtype)
           
      end do                                                      

      write(fu6,*) 'LINEAR BOND'                                 
      do i = 1, NLBEPJ(jtype)                                                  
          do j = 1, 2
              indfc = indfc + 1
              write(fu6,*) indfc, ': ', ILBEPJ(1,i,jtype), 
     *                     '=', ILBEPJ(2,i,jtype), '=', 
     *                     ILBEPJ(3,i,jtype), ' FC: ', 
     *                     FCFACPJ(indfc, jtype)    
          end do
      end do                                

      end do                       



c --- Renaming to local atom index checking for scaling factor 

c --- make the local copy onf internal coordinate with renaming   
c     to local atom index for each type. The purpose is to make  
c     the atom index is no more than nratom(jtype), which is    
c     compatable with the x, dx, dxx dimension.                

      write(fu6, *) 'AFTER RENAMING'
      do jtype = 1, 4

      NBLR = NBLPJ(jtype)
      NBAR = NBAPJ(jtype)
      NTOR = NTOPJ(jtype)
      NLBER = NLBEPJ(jtype)
      
      do i = 1, NBLR
          do j = 1, 2
               do k = 1, nratom(jtype)
                    if (IBLPJ(j, i, jtype).eq.iatsv(k, jtype)) then
                           IBLR(j, i) = k
                    end if
               end do
          end do
     
      end do

      do i = 1, NBAR
          do j = 1, 3
               do k = 1, nratom(jtype)
                    if (IBAPJ(j, i, jtype).eq.iatsv(k, jtype)) then
                           IBAR(j, i) = k
                    end if
               end do
          end do
      end do

      do i = 1, NTOR
          do j = 1, 4
               do k = 1, nratom(jtype)
                    if (ITOPJ(j, i, jtype).eq.iatsv(k, jtype)) then
                           ITOR(j, i) = k
                    end if
               end do
          end do
      end do

      do i = 1, NLBER
          do j = 1, 3
               do k = 1, nratom(jtype)
                    if (ILBEPJ(j, i, jtype).eq.iatsv(k, jtype)) then  
                           ILBER(j, i) = k
                    end if
               end do
          end do
      end do


c --- Print out the internal coordinates with renamed local atom index  
      indfc = 0
c
      write (fu6, *) '--- JTYPE = ', jtype
c
      write(fu6,*) 'BOND LINK'
      do i = 1, NBLR
          indfc = indfc + 1
          write(fu6,*) indfc, ': ', IBLR(1,i),
     *                 '-', IBLR(2,i), ' FC: ',
     *                 FCFACPJ(indfc, jtype)
      end do

      write(fu6,*) 'BOND ANGLE'
      do i = 1, NBAR
          indfc = indfc + 1
          write(fu6,*) indfc, ': ', IBAR(1,i),
     *                 '-', IBAR(2,i), '-', IBAR(3,i),
     *                 ' FC: ', FCFACPJ(indfc, jtype)
      end do

      write(fu6,*) 'TORSION'
      do i = 1, NTOR
          indfc = indfc + 1
          write(fu6,*) indfc, ': ', ITOR(1,i),
     *                 '-', ITOR(2,i), '-', ITOR(3,i),
     *                 '-', ITOR(4,i), ' FC: ',
     *                 FCFACPJ(indfc, jtype)

      end do

      write(fu6,*) 'LINEAR BOND'
      do i = 1, NLBER
          do j = 1, 2
              indfc = indfc + 1
              write(fu6,*) indfc, ': ', ILBER(1,i),
     *                     '=', ILBER(2,i), '=',
     *                     ILBER(3,i), ' FC: ',
     *                     FCFACPJ(indfc, jtype)
          end do
      end do


      end do


      end if


c --- END DEBUG  PU -----
c**********************************************************************
  
      return
      end subroutine intcor

c
c***********************************************************************
c     ICOUTRP
c***********************************************************************
c
      subroutine icoutrp(NBL,NBA,NTO,NLBE,   
     *                   IBL1,IBA1,ITO1,ILBE1,
     *                   IBL2,IBA2,ITO2,ILBE2,
     *                   NATOM,x,y,z,AL )
      use perconparam, only : fu6
c
c     Created by: Jingzhi Pu, Jan 03, 2001 
c 
c     Called by:
c              icfdrp
c
      implicit double precision (a-h,o-z)
c
c     prints out the internal coordinates
c
      character*2 AL
c
      dimension IBL1(2,NBL),IBA1(3,NBA),ITO1(4,NTO),ILBE1(3,NLBE)               
      dimension IBL2(2,NBL),IBA2(3,NBA),ITO2(4,NTO),ILBE2(3,NLBE) 
      dimension x(NATOM),y(NATOM),z(NATOM),AL(NATOM)
c
      parameter (ONE=1.d0, NINETY=90.d0)
c
      conv=NINETY/dasin(ONE)
c
      write(fu6,'('' *** Internal coordinates ***'',/)')


c --- BOND LENGTHS

      write(fu6,'('' BOND LENGTHS ('',i3,'')'',/)') NBL
      do 100 j=1,NBL/3+1
      imin=3*j-2
      imax=min(3*j,NBL)
      write(fu6,'(3(1x,i3,1x,''('',a2,i2,'' - '',a2,i2,'')'',f9.5))')     
     * (i,AL(IBL1(1,i)),IBL1(1,i),AL(IBL1(2,i)),IBL1(2,i),
     * dist(IBL2(1,i),IBL2(2,i),x,y,z,NATOM),i=imin,imax)
 100  continue  



c --- BOND ANGLES

      write(fu6,'(/,'' BOND ANGLES ('',i3,'')'',/)') NBA
      do 200 j=1,NBA/2+1
      imin=2*j-1
      imax=min(2*j,NBA)
      write(fu6,'(2(1x,i3,1x,''('',a2,i2,'' - '',a2,i2,'' - '',a2,i2,
     > '')'',f9.3))') (i+NBL,AL(IBA1(1,i)),IBA1(1,i),AL(IBA1(2,i)),
     > IBA1(2,i),AL(IBA1(3,i)),IBA1(3,i),
     > CONV*angl(IBA2(1,i),IBA2(2,i),IBA2(3,i),x,y,z,NATOM),i=imin,imax)   
 200  continue



c --- TORSIONAL ANGLES

      WRITE(fu6,'(/,'' TORSIONAL ANGLES ('',I3,'')'',/)') NTO
      DO 300 J=1,NTO/2+1
      imin=2*j-1
      imax=min(2*j,NTO)
      write(fu6,'(2(1x,i3,1x,''('',a2,i2,'' - '',a2,i2,'' - '',a2,i2,     
     > '' - '',a2,i2,'')'',f8.2))') (i+NBL+NBA,AL(ITO1(1,i)),ITO1(1,i),
     > AL(ITO1(2,i)),ITO1(2,i),AL(ITO1(3,i)),ITO1(3,i),AL(ITO1(4,i)),
     > ITO1(4,i),CONV*ptors(ITO2(1,i),ITO2(2,i),ITO2(3,i),ITO2(4,i),
     > x,y,z,NATOM),i=imin,imax)
  300 continue



c --- LINEAR BEND 

      write(fu6,'(/,'' LINEAR BENDS ('',I3,'')'',/)') NLBE
      do 400 j=1,NLBE/2+1
      imin=2*j-1
      imax=min(2*j,NLBE)
      write(fu6,'(2(1x,i3,1x,''('',a2,i2,'' = '',a2,i2,'' = '',a2,i2,   
     > '')'',f9.3))') (i+NBL+NBA+NTO,
     > AL(ILBE1(1,i)),ILBE1(1,i),AL(ILBE1(2,i)),
     > ILBE1(2,i),AL(ILBE1(3,i)),ILBE1(3,i),
     > CONV*angl(ILBE2(1,i),ILBE2(2,i),ILBE2(3,i),x,y,z,NATOM),
     > i=imin,imax)  
 400  continue
 
      return
      end subroutine icoutrp



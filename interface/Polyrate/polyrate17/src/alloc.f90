
      subroutine allocate_efmain(n3tm)
      use efmain_mod
!
!  Allocate local variables for the EFMAIN routine
!
      if(allocated(eigval))deallocate(eigval)
      allocate(eigval(n3tm)); eigval=0.d00
      if(allocated(oldhss))deallocate(oldhss)
      allocate(oldhss(n3tm,n3tm)); oldhss=0.d00
!
      if(.not.allocated(hessc)) then
        allocate(hessc(n3tm*(n3tm+1)/2)); hessc=0.d00
      endif
      if(.not.allocated(oldu)) then
        allocate(oldu(n3tm,n3tm)); oldu=0.d00
      endif
      if(.not.allocated(ooldf)) then
        allocate(ooldf(n3tm)); ooldf=0.d00
      endif
      if(.not.allocated(fx)) then
        allocate(fx(n3tm)); fx=0.d00
      endif
      if(.not.allocated(grad)) then
        allocate(grad(n3tm)); grad=0.d00
      endif
      if(.not.allocated(hess)) then
        allocate(hess(n3tm,n3tm)); hess=0.d00
      endif
      if(.not.allocated(xparam)) then
        allocate(xparam(n3tm)); xparam=0.d00
      endif
!
      if(allocated(d))deallocate(d)
      allocate(d(n3tm)); d=0.d00
      
      if(.not.allocated(vmode)) then
        allocate(vmode(n3tm)); vmode=0.d00
      endif
      if(.not.allocated(u)) then
        allocate(u(n3tm,n3tm)); u=0.d00
      endif
!the one element in amass(n3tm+1) is reserved for using bath
      if(.not.allocated(amass)) then
        allocate(amass(n3tm+1),f(n3tm,n3tm))
      endif
      end subroutine allocate_efmain

      subroutine alloc_energ(n3tm)
      use energetics_mod
      if(allocated(f2))deallocate(f2)
      allocate(f2(n3tm,n3tm)); f2=0.d00
      allocate(xxi(n3tm),hessin(n3tm,n3tm))
      end subroutine alloc_energ

      subroutine mem_intpm(n3tm,imem)
      use energetics_mod
      integer, intent(in) :: n3tm,imem
!
!   IMEM = 1 - allocate
!   IMEM = 2 - deallocate
!   IMEM = 3 - allocate if NOT already allocated
!
       select case(imem)
         case(1)
           if(.not.allocated(u0)) then
             allocate(u0(n3tm,n3tm)); u0=0d0
           endif
           if(.not.allocated(scr)) then
              allocate(scr(n3tm)); scr=0d0
           endif
           if(.not.allocated(scr2)) then
              allocate(scr2(n3tm)); scr2=0d0
           endif
           if(.not.allocated(xxi)) then
              allocate(xxi(n3tm)); xxi=0d0
           endif
           if(.not.allocated(xlam)) then
              allocate(xlam(n3tm)); xlam=0d0
           endif
       end select
       end subroutine mem_intpm

      subroutine mem_newton(n3tm,imem)
      use energetics_mod
      integer, intent(in) :: n3tm,imem
!
!   IMEM = 1 - allocate
!   IMEM = 2 - deallocate
!   IMEM = 3 - allocate if NOT already allocated
!
       select case(imem)
         case(1)
           if(.not.allocated(x0)) then
             allocate(x0(n3tm)); x0=0.d0
           endif
           if(.not.allocated(dx0)) then
             allocate(dx0(n3tm)); dx0=0.d0
           endif
           if(.not.allocated(work)) then
             allocate(work(n3tm)); work=0
           endif
           if(.not.allocated(bfgs1)) then
             allocate(bfgs1(n3tm,n3tm)); bfgs1=0.d0
           endif
           if(.not.allocated(bfgs2)) then
             allocate(bfgs2(n3tm,n3tm)); bfgs2=0.d0
           endif
           if(.not.allocated(deltag)) then
             allocate(deltag(n3tm)); deltag=0.d0
           endif
       end select
       end subroutine mem_newton

       subroutine mem_normod(n3tm,imem)
       use energetics_mod
       integer, intent(in) :: n3tm,imem
!
!   IMEM = 1 - allocate
!   IMEM = 2 - deallocate
!   IMEM = 3 - deallocate if already allocated and reallocate
!
       select case(imem)
         case(1)
           if(.not.allocated(xmscd)) then
           allocate(xmscd(n3tm),dxsav(n3tm), anco(n3tm,n3tm),tempx(n3tm,n3tm),&
                    dxxp(n3tm),fsp(n3tm,n3tm))
           xmscd=0.d00; dxsav=0.d00; anco=0; tempx=0.d00; fsp=0.d0; dxxp=0.d0
           endif
         case(2)
           if(allocated(xmscd))deallocate(xmscd)
           if(allocated(dxsav))deallocate(dxsav)
           if(allocated(anco))deallocate(anco)
           if(allocated(tempx))deallocate(tempx)
           if(allocated(fsp))deallocate(fsp)
           if(allocated(dxxp))deallocate(dxxp)
         case(3)
           if(allocated(xmscd))deallocate(xmscd)
           if(allocated(dxsav))deallocate(dxsav)
           if(allocated(anco))deallocate(anco)
           if(allocated(tempx))deallocate(tempx)
           if(allocated(fsp))deallocate(fsp)
           if(allocated(dxxp))deallocate(dxxp)
           allocate(xmscd(n3tm),dxsav(n3tm), anco(n3tm,n3tm),tempx(n3tm,n3tm),&
                    dxxp(n3tm),fsp(n3tm,n3tm))
           xmscd=0.d00; dxsav=0.d00; anco=0; tempx=0.d00; fsp=0.d0; dxxp=0.d0
       end select
       end subroutine mem_normod

       subroutine mem_path(imem,n3tm)
       use perconparam, only : nsdm
       use path_mod
       integer, intent(in) :: n3tm,imem
!
!   IMEM = 1 - allocate
!   IMEM = 2 - deallocate
!   IMEM = 3 - deallocate if already allocated and reallocate
!
       select case(imem)
         case(1)
           allocate(xs(n3tm), iwrdx(n3tm))
           xs=0.d00; iwrdx=0
         case(2)
           if(allocated(xs))deallocate(xs)
           if(allocated(iwrdx))deallocate(iwrdx)
       end select
       if(.not.allocated(geomm)) then
           allocate(geomm(n3tm,nsdm)); geomm=0d0
       endif
       if(.not.allocated(derivm)) then
           allocate(derivm(n3tm,nsdm)); derivm=0d0
       endif
       if(.not.allocated(lsaves)) then
           allocate(lsaves(nsdm)); lsaves=0
       endif
       end subroutine mem_path

       subroutine allocate1(num)
       use cm, only : label,xmass,rtmp,itmp,xvdw
       if(.not.allocated(label))then
           allocate(label(1)); label=0
         else
           if(allocated(itmp))deallocate(itmp)
           is1=size(label)
           allocate(itmp(is1)); itmp=label; deallocate(label)
           allocate(label(num)); label(1:is1)=itmp; label(num)=0
           deallocate(itmp)
       end if
!
       if(.not.allocated(xmass))then
           allocate(xmass(1)); xmass=0.d00
         else
           if(allocated(rtmp))deallocate(rtmp)
           is1=size(xmass)
           allocate(rtmp(is1)); rtmp=xmass; deallocate(xmass)
           allocate(xmass(num)); xmass(1:is1)=rtmp; xmass(num)=0.d00
           deallocate(rtmp)
       end if
!
       if(.not.allocated(xvdw))then
           allocate(xvdw(1)); xvdw=0.d00
         else
           if(allocated(rtmp))deallocate(rtmp)
           is1=size(xvdw)
           allocate(rtmp(is1)); rtmp=xvdw; deallocate(xvdw)
           allocate(xvdw(num)); xvdw(1:is1)=rtmp; xvdw(num)=0.d00
           deallocate(rtmp)
       end if
       end subroutine allocate1

       subroutine get_number_of_mep_points
       use perconparam; use gf; use path_mod
       use rate_const, only : del, inh
       use common_inc, only : slm,slp,lgs,n3
!
!  Action depends on the kind of the ESP input
!
       nlines=0; nsdm=0
       if(mode_esp==30)then                        !   FU30
           do i=1,nlines_esp
             a=poly_esp(i)
             call low2up(a)
             ipos=index(a,'$HESS FOR POINT')
             if(ipos/=0)nsdm=nsdm+1
           end do
         elseif(mode_esp==31)then                  !   FU31
           do i=1,nlines_esp
             a=poly_esp(i)
             call low2up(a)
             ipos=index(a,'*POINT')
             if(ipos/=0)nsdm=nsdm+1
           end do
         elseif(mode_esp==40)then                  !   FU40
           do i=1,nlines_esp
             a=poly_esp(i)
             call low2up(a)
             ipos=index(a,'*point40')
             if(ipos/=0)nsdm=nsdm+1
           end do
         else
           nsdm=nsdm_percon
       end if
       isavep=dint((slp-slm)/(del*dble(inh)))
       if(nsdm==0)nsdm=isavep
       nsdm=max0(isavep,nsdm)+3
       nsdim=nsdm; NPT31 = INMM*NSDM
       nsdml=nsdm
       IF (LGS(34) .NE. 0) THEN
            N3M7 = N3 -1
         ELSE
            N3M7 = N3-7
       ENDIF  
       end subroutine get_number_of_mep_points

       subroutine find_esp_mode
       use path_mod; use rate_const
       use common_inc, only : slm,slp
       logical :: slp_read,slm_read,inh_read,del_read
       mode_esp=0
       do i=1,nlines_inp
         a=poly_fu5(i)
         call low2up(a)
         ipos = index(a,'POTENTIAL')
         if(ipos==0)cycle
         ipos = index(a,'UNIT29')
         if(ipos/=0)then
            mode_esp=29; exit
         end if
         ipos = index(a,'UNIT30')
         if(ipos/=0)then
            mode_esp=30; exit
         end if
         ipos = index(a,'UNIT31')
         if(ipos/=0)then
            mode_esp=31; exit
         end if
         ipos = index(a,'UNIT40')
         if(ipos/=0)then
            mode_esp=40; exit
         end if   
       end do
!
!  SLP, SLM, DEL,INH
!
       slp_read=.false.;slm_read=.false.;inh_read=.false.; del_read=.false.
       
       do i=1,nlines_inp
         a=poly_fu5(i)
         call low2up(a)
         ipos = index(a,'SLP')
         if(ipos>0)then
            read(a(ipos+3:),*)slp; slp_read=.true.
            cycle
         end if
         ipos = index(a,'SLM')
         if(ipos>0)then
            read(a(ipos+3:),*)slm; slm_read=.true.
            cycle
         end if
         ipos = index(a,'INH')
         if(ipos>0)then
            read(a(ipos+3:),*)inh; inh_read=.true.
            cycle
         end if
         ipos = index(a,'SSTEP')
         if(ipos>0)then
            read(a(ipos+5:),*)del; del_read=.true.
            cycle
         end if 
       end do
!
!  Default values
!
       if(.not.inh_read)inh=9
       if(.not.del_read)del=5.d-4
       if(.not.slm_read)slm=-1.d00
       if(.not.slp_read)slp=1.d00
       end subroutine find_esp_mode

       subroutine get_esp
       use path_mod; use gf
       character(10) :: esp_file
       if(mode_esp==0)return
       esp_file(:)=' '
       select case(mode_esp)
          case(29)
            esp_file='poly.fu29'
          case(30)
            esp_file='poly.fu30'
          case(31)
            esp_file='poly.fu31'
          case(40)
            esp_file='poly.fu40'
          case default
            write(*,*)'An error occured - wrong number of the ESP file=',mode_esp
            stop 'Error termination of POLYRATE'
       end select
       nlines_esp=0
       call get_file(trim(esp_file),nlines_esp,poly_esp)
       end subroutine get_esp
!
!***************************************************************************************
!
       subroutine mem_initze
       use rate_const; use perconparam
       use common_inc
       allocate(cdscmu(nsdm),zocmcd(nsdm))
       cdscmu=0.d00; zocmcd=0.d00
!
!  Initialize the arrays used to store the semiempirical frequencies
!  of the stationary points used in Zero-Order-IVTST correction
!
       allocate(frp1s(n3tm),frp2s(n3tm),frp1a(n3tm),frp2a(n3tm))
       frp1s=0.d00; frp2s=0.d00; frp11=0.d00;frp2a=0.d00
!
!
!  INITIALIZE THE ARRAY USED TO PASS EXTRA INFORMATION FROM THE 
!  POTENTIAL ENERGY SURFACE (this info is printed only and is not
!                            needed by the VTST calculation)
!
       allocate(potinf(npotpt)); potinf=0.d00
!
!  INITIALIZE THE ARRAY LRP(N6TM) WHICH IS USED IN THE CALCULATION OF
!  THE REACTANT AND PRODUCT VIBRATIONAL PARTITION FUNCTIONS TO INDICATE
!  WHETHER OR NOT MODE(I) IS THERMAL OR NOT.  ALL MODES ARE THERMAL 
!  UNLESS LGS(23) IS NONZERO.
!
      allocate(lrp(n6tm)); lrp=0
!
      D3LX = 0.D0
!
      allocate(inumi(maxps),ilb(idumit,maxps),iub(idumit,maxps))
      inumi=0; ilb=0; iub=0
!
      allocate(npacc(nsv),mpacc(nsv)); npacc=0; mpacc=0
!
      VZOCLC = 0.D0
      allocate(sgn1(n3tm),sgn2(n3tm),in1(n3tm),in2(n3tm))
      sgn1=1.d00; sgn2=1.d00; in1=1; in2=1
!
!
!---------------------------------------------------------------------------
!     THE FOLLOWING INITILIZATION IS MOVED FROM MAIN.F
!
!     SET THE FLAG FOR LCG3-4 OVERLAP CALCULATION
!
      INDPH = 0
      L0 = 0
!
     allocate(efndtr(n6tm)); efndtr=0.d00
!
!     WRITE OUT PARAMETERS USED FOR ANHARMONIC CALCULATIONS
!     FOR HARMONIC CALCULATION LGS(5) = 0.
!
     allocate(anhrm(n6tm),y00(n6tm),y00r(n6tm),xer(n6tm))
      anhrm=0.d00; y00=0.d00; y00r=0.d00; xer=0.d00
      end subroutine mem_initze
!
!***************************************************************************************
!
      subroutine find_numatoms
      use path_mod; use perconparam
      istart=0
      do i=1,nlines_inp
         a=poly_fu5(i)
         call low2up(a)
!        ipos=index(a,'*START')
         ipos=index(a,'*GENERAL')
         if(ipos/=0)then
            istart=i;  exit
         end if
      end do
!     if(istart==0)then
!        write(*,*)'No  *START keyword found in the POLY.FU5 file'
!        stop 'Error termination of POLYRATE'
!     end if
!
      igeom=0
      do i=istart+1,nlines_inp
         a=poly_fu5(i)
         call low2up(a)
!        ipos=index(a,'GEOM')
         ipos=index(a,'ATOMS')
         if(ipos/=0)then
            igeom=i;  exit
         end if
      end do
      if(igeom==0)then
!        write(*,*)'No  GEOM keyword found in the *START '
         write(*,*)'No  ATOMS keyword found in the *GENERAL '
         write(*,*)'section of the  POLY.FU5 file'
         stop 'Error termination of POLYRATE'
      end if
!
      natom=0
      do i=igeom+1,nlines_inp
         a=poly_fu5(i)
         if(len_trim(a)==0)cycle
         call low2up(a)
         ipos=index(a,'END')
         if(ipos/=0)then
             exit
           else
             natom=natom+1
         end if
      end do
      natoms=natom
      n3tm=3*natom; n6tm=2*n3tm
      maxint = 3*natom + 6; maxcar=n3tm
      nfre=3*natom-5
      maxd = natoms*(natoms+1)/2
!
!  For INTxx routines
!
!      MAXCA2=MAXCAR*(MAXCAR+1)/2
      maxca2 = n3tm*(n3tm-1)/2 + n3tm
      NB = MAXCAR*(MAXCAR+1)/2
      MAXIN2=MAXINT*(MAXINT+1)/2
      MAXCOR=2*MAXINT*MAXCAR+MAXINT*MAXIN2+MAXIN2+MAXIN2+MAXINT*MAXCAR
      end subroutine find_numatoms
!
!***************************************************************************************
!
      subroutine setanh_mem
      use memanh; use perconparam
      use common_inc
      if(.not.allocated(ntemp1)) then
        allocate(ntemp1(n3tm), ntemp2(n3tm), nsub(n3tm,n3tm))
        allocate(ntemp5(n3tm,2), ntemp3(n3tm), ntemp4(n3tm),               &
        ntemp6(n3tm,NMAXP))
        allocate(ntemp7(n3tm), ntemp8(n3tm),ntemp9(n3tm),                  &
        ntemp10(n3tm),temp11(n3tm))
        allocate(tempr1(n3tm,NMAXP),tempr2(n3tm,NMAXP),tempr3(n3tm,NMAXP), &
        tempr4(n3tm,NMAXP),tempr5(n3tm,NMAXP),tempr6(n3tm,NMAXP))
        allocate(tempr7(n3tm,NMAXP),tempr8(n3tm,NMAXP))
      endif
!
      if(.not.allocated(torintrp))then
        allocate(torintrp(narrs,n3tm),torzpc(narrs,n3tm))
        torintrp=0.d00; torzpc=0.d00
      end if
      end subroutine setanh_mem
!
!***************************************************************************************
!
      subroutine default_mem
      use perconparam; use cm
      use common_inc
      use rate_const
      use keyword_interface, only : cmodet,vharmr
!the one element in amass(n3tm+1) is reserved for using bath
      allocate(svmas(natom),amass(n3tm+1),indx(n3tm,8),indx0(n3tm,8), &
      modets(narrs,n3tm),l9(9,n3tm))
      svmas=0.d00; amass=0.d00; indx=0; modets=0; l9=0; indx0=0
      allocate(cmodet(narrs,n3tm)); cmodet= 'harmonic '
!
      allocate(sanhrm(narrs,narrs)); sanhrm=0.d00
      allocate(ntrnb(8,n3tm),ntrsch(8,n3tm),ntrlev(8,n3tm),ntrmtd(8,n3tm),    &
      ntrm(8,n3tm),ntrnum(8,n3tm),ntrbnd(8,n3tm,2),readi(8,n3tm))
      allocate(ntrsig(8,n3tm,nmaxp),torome(8,n3tm,nmaxp),torw(8,n3tm,nmaxp),  &
      toru(8,n3tm,nmaxp),tormi(8,n3tm,nmaxp),torwl(8,n3tm,nmaxp),             &
      torwr(8,n3tm,nmaxp),ratiol(8,n3tm,nmaxp),ratior(8,n3tm,nmaxp),          &
      ntrisb(8,n3tm,natom))
      ntrsig=0
!
      allocate(vharmr(n3tm,8),moder(8,n3tm),mode(n3tm),icnst(n3tm,8),ln3(2,n3tm))
      vharmr(:,:)(:)=' '; moder=0; mode=0; ln3=0
!
!  Initialize the icnst array which determines if a particular coordinate
!  will be optimized. A value of 0 means that coord. is held constant
!
      icnst=1
!     
      end subroutine default_mem
!
!***************************************************************************************
!
      subroutine readic_mem
      use perconparam
      use rate_const, only : ifrr,ifrp,wesada,wera,icfr
      if(.not.allocated(ifrr)) then
        allocate(ifrr(n3tm),ifrp(n3tm),wesada(n3tm),wera(n6tm))
        irff=0; ifrp=0; wesada=0.d00; wera=0.d00
      endif
      if(.not.allocated(icfr)) then
         allocate(icfr(n3tm));ifrc=0
      endif
      end subroutine readic_mem
!
!***************************************************************************************
!
      subroutine  sad_mem
      use common_inc; use perconparam
      use rate_const
      if(.not.allocated(x))then
         allocate(x(n3tm));x=0d0 
      endif
      if(.not.allocated(dx)) then
         allocate(dx(n3tm)); dx= 0d0
      endif
      if(.not.allocated(f)) then
         allocate(f(n3tm,n3tm)); f= 0d0
      endif
         
      if(.not.allocated(freq)) then
         allocate(freq(n3tm)); freq=0d0
      endif
      if(.not.allocated(wer)) then
        allocate(wer(n6tm)); wer=0d0
      endif
      if(.not.allocated(cof)) then
         allocate(cof(n3tm,n3tm)); cof=0.d0
      endif
      if(.not.allocated(ind)) then
         allocate(ind(n3tm),iatom(natom))
         ind=0; iatom=0
      endif
!
      if(.not.allocated(xxs))then
         allocate(xxc(n3tm),xxs(n3tm,nsdim))
         xxc=0.d00; xxs=0.d00
      end if
      if(.not.allocated(proj))then
         allocate(proj(n3tm,n3tm)); proj=0.d00
      end if
      if(.not.allocated(csv))then
         allocate(csv(n3tm,n3tm)); csv=0.d00
      end if
      if(.not.allocated(dxsad))then
         allocate(dxsad(n3tm)); dxsad=0.d00
      end if
      if(.not.allocated(cofsad)) then
         allocate(cofsad(n3tm,n3tm),ifrsad(nvibm))
         cofsad=0.d00; ifrsad=0
      endif
      if(.not.allocated(wesads)) then
         allocate(wesads(n3tm)); wesads=0d0
      endif  
      end subroutine  sad_mem
!
!***************************************************************************************
!
      subroutine intbsv_mem
      use perconparam; use intbsv
!
      if(.not.allocated(core)) then
         allocate(core(maxcor));core=0.d0
      endif
      if(.not.allocated(fl)) then
         allocate(fl(maxca2));fl=0.d0
      endif
      end subroutine intbsv_mem
!
!***************************************************************************************
!
      subroutine dorepr_mem
      use perconparam
      use rate_const
      use common_inc; use cm
      if(.not.allocated(ifqlow))then
         allocate(ifqlow(n3tm)); ifqlow=0
      end if     
      if(.not.allocated(frelow)) then
         allocate(frelow(3,n3tm)); frelow=0d0
      endif
      if(.not.allocated(x))then
         allocate(x(n3tm)); x=0.d00
      end if
      if(.not.allocated(f))then
         allocate(f(n3tm,n3tm)); f=0.d00
      end if
      if(.not.allocated(iatom))then
         allocate(iatom(natom)); iatom=0
      end if
      if(.not.allocated(ind))then
         allocate(ind(n3tm)); ind=0
      end if
      if(.not.allocated(ss))then
         allocate(ss(nsdim)); ss=0.d00
      end if
      if(.not.allocated(ifreq))then
         allocate(ifreq(nvibm)); ifreq=0
      end if
      if(.not.allocated(dx)) then
         allocate(dx(n3tm)); dx = 0d0
      end if
      if(.not.allocated(xk3) ) then
        allocate(xk3(nvibm)); xk3=0.d00 
      endif
      if(.not.allocated(xk4)) then
         allocate(xk4(nvibm));axk4=0d0
      endif
      if(.not.allocated(ab)) then
          allocate(ab(n6tm));ab=0d0
      endif
      if(.not.allocated(gse)) then
          allocate(gse(n6tm)); gse=0d0
      endif
      if(.not.allocated(dxp)) then
          allocate(dxp(n3tm)); dxp=0d0
      endif
      if(.not.allocated(efndtp)) then 
          allocate(efndtp(n3tm)); efndtp=0.d00 
      endif
!
      if(.not.allocated(fmihw)) then
       allocate(fmihw(n6tm),xew(n6tm),wgsex(n6tm),y0w(n6tm))
        fmihw=0.d00; xew=0.d00;wgsex=0.d00; y0w=0.d00
      endif
!
      if(.not.allocated(wer)) then
         allocate(wer(n6tm))
         wer=0.d00
      endif
      if(.not.allocated(freq))then
         allocate(freq(n3tm)); freq=0.d00
      end if
!
      if(.not.allocated(proj))then
         allocate(proj(n3tm,n3tm)); proj=0.d00
      end if
      if(.not.allocated(fmihr))then
         allocate(fmihr(n6tm)); fmihr=0.d00
      end if
      if(.not.allocated(wew))then
         allocate(wew(n6tm)); wew=0.d00
      end if
      if(.not.allocated(cofbsv)) then
         allocate(cofbsv(n3tm,n3tm),coftsv(n3tm,n3tm));cofbsv=0d0;coftsv=0d0
      endif
      end subroutine dorepr_mem
!
!***************************************************************************************
!
      subroutine rphset_mem
      use rate_const; use intbsv
      use perconparam
      use common_inc, only : lgs,n3,freq,cof
  
         IF (LGS(34) .NE. 0) THEN
            N3M7 = N3 -1
         ELSE
            N3M7 = N3-7
         ENDIF
       allocate(ws(nsdim,nvibm),xk3s(nsdim,nvibm),xk4s(nsdim,nvibm),           &
	   bfs(nsdim,nvibm),fmirs(nsdim,nvibm))
       ws=0.d00; xk3s=0.d00; xk4s=0.d00; bfs=0.d00; fmirs=0.d00
       allocate(ssubi(nsdm),vclas(nsdm),fmits(nsdm),wets(nvibm,nsdm),        &
       xets(nvibm,nsdm),y0ts(nvibm,nsdm))
       ssubi=0.d00;vclas=0.d00;fmits=0.d00;wets=0.d00;xets=0.d00;y0ts=0.d00
!
!  FREQ
!
       if(.not.allocated(freq))then
          allocate(freq(n3tm)); freq=0.d00
       end if
       if(.not.allocated(cof))then
          allocate(cof(n3tm,n3tm)); cof=0.d00
       end if
!
!  CORE - scratch region
!
       if(allocated(core))deallocate(core)
       allocate(core(maxcor)); core=0.d00
!
!  FL
!
       if(allocated(fl))deallocate(fl)
       allocate(fl(maxca2)); fl=0.d00
       end subroutine rphset_mem

       subroutine dorph_mem
       use perconparam
       use rate_const; use common_inc
       allocate(vs(nsdim),fmoms(nsdim))
       vs=0.d00; fmoms=0.d00
       if(.not.allocated(iatom))then
          allocate(iatom(natoms)); iatom=0
       end if
       if(.not.allocated(ind))then
          allocate(ind(n3tm)); ind=0
       end if
       if(.not.allocated(proj))then
          allocate(proj(n3tm,n3tm)); proj=0.d00
       end if
       if(.not.allocated(fsv))then
          allocate(fsv(n3tm,n3tm)); fsv=0.d00
       end if
       end subroutine dorph_mem
       
       subroutine fdiag_mem(nend)
       use common_inc; use perconparam
       if(.not.allocated(freq))then
            allocate(freq(nend)); freq=0.d00
         elseif(size(freq)/=nend)then
            deallocate(freq); allocate(freq(nend)); freq=0.d00
       end if
!
       if(.not.allocated(cof))then
            allocate(cof(n3tm,nend)); cof=0.d00
          elseif(size(cof,2)/=nend)then
            deallocate(cof); allocate(cof(n3tm,nend)); cof=0.d00
       end if
!
       if(.not.allocated(cofx))then
            allocate(cofx(n3tm,nend)); cofx=0.d00
          elseif(size(cofx,2)/=nend)then
            deallocate(cofx); allocate(cofx(n3tm,nend)); cofx=0.d00
       end if
!
       if(.not.allocated(intout))allocate(intout(n6tm))
       end subroutine fdiag_mem


       subroutine zocpar_mem
       use rate_const; use perconparam
       allocate(iffun(n3tm),af(n3tm),bf(n3tm),cf(n3tm),s0f(n3tm),af1(n3tm),bf1(n3tm), &
       cf1(n3tm),s0f1(n3tm),af2(n3tm),bf2(n3tm),cf2(n3tm),s0f2(n3tm))
       iffun=0; af=0.d00; bf=0.d00; cf=0.d00; s0f=0.d00; af1=0.d00; bf1=0.d00
       cf1=0.d00; af2=0.d00; bf2=0.d00; cf2=0.d00; s0f2=0.d00
       end subroutine zocpar_mem

      subroutine dopnm_mem
      use rate_const; use perconparam
      use common_inc; use cm
      use path_mod
      if(.not.allocated(dx1)) then
        allocate(dx1(n3tm),dx2(n3tm),bcurv(nvibm))
        dx1=0.d00; dx2=0.d00; bcurv=0.d00
      endif
!
      if(.not.allocated(ifitwr)) then
      allocate(ifitwr(nvibm),awr(3,nvibm),ak3r(3,nvibm),ak4r(3,nvibm),iftk3r(nvibm),    &
      iftk4r(nvibm),abfr(3,nvibm),amir(3,nvibm),amip(3,nvibm),ak4p(3,nvibm),            &
      iftk4p(nvibm),ifitwp(nvibm),awp(3,nvibm),iftk3p(nvibm),ak3p(3,nvibm),abfp(3,nvibm))
      ifitwr=0; awr=0.d00; ak3r=0.d00;ak4r=0.d00;iftk3r=0;iftk4r=0;abfm=0.d00;amir=0.d00
      amip=0.d00; ak4p=0.d00;iftk4p=0;ifitwp=0;awp=0.d00;iftk3p=0;ak3p=0.d00;abfp=0.d00
      endif
!
      if(.not.allocated(fmihts))then
         allocate(fmihts(n3tm,nsdm)); fmihts=0.d00
      end if
      if(.not.allocated(fmomhr))then
         allocate(fmomhr(n3tm)); fmomhr=0.d00
      end if
      if(.not.allocated(cofsv))then
         allocate(cofsv(n3tm,n3tm,nsdm)); cofsv=0.d00
      end if
      if(.not.allocated(dxsv))then
         allocate(dxsv(n3tm,nsdm)); dxsv=0.d00
      end if
      if(.not.allocated(vadib))then
         allocate(vadib(nsdm)); vadib=0.d00
      end if
!
      if(.not.allocated(geom))then
         allocate(geom(n3tm,nsdm),egrnd(nsdm))
         geom=0.d00;egrnd=0.d00
      end if
!
      if(.not.allocated(efndt))then
         allocate(efndt(n3tm,nsdm))
         efndt=0.d00
      end if
!
      if(.not.allocated(efndtp))then
         allocate(efndtp(n3tm))
         efndtp=0.d00
      end if
!
      if(.not.allocated(sbkap))then
         allocate(sbkap(nsdm)); sbkap=0.d00
      end if
!
      if(.not.allocated(bcur)) then
        allocate(bcur(nvibm,nsdm),ewkb0(nvibm,nsdm))
        bcur=0.d00; ewkb0=0.d00
      endif
      if(.not.allocated(delg)) then
         allocate(delg(nsdm)); delg=0d0
      endif
      if(.not.allocated(delig))then
         allocate(delig(nsdm)); delig=0d0
      end if
!
      if(.not.allocated(dxb)) then
        allocate(dxb(n3tm),dxold(n3tm),xold(n3tm))
        dxb=0.d00; dxold=0.d00; xold=0.d0
      endif
!
      if(.not.allocated(ssubi))then
         allocate(ssubi(nsdm)); ssubi=0.d00
      end if
      if(.not.allocated(vclas))then
         allocate(vclas(nsdm),fmits(nsdm))
         vclas=0.d00; fmits=0.d00
      end if
      if(.not.allocated(wets))then
         allocate(wets(nvibm,nsdm),xets(nvibm,nsdm),y0ts(nvibm,nsdm))
         wets=0.d00; xets=0.d00; y0ts=0.d00
      end if
      if(.not.allocated(fsv))then
         allocate(fsv(n3tm,n3tm)); fsv=0.d00
      end if
      end subroutine dopnm_mem

      subroutine rate_mem
      use rate_const; use perconparam
      if(.not.allocated(aspl))then
         allocate(aspl(nsdm),bspl(nsdm),cspl(nsdm),dspl(nsdm))
         aspl=0.d00; bspl=0.d00; cspl=0.d00; dspl=0.d00
      end if
      end subroutine rate_mem
!
!************************************************************************
!
      subroutine rpath_mem
      use common_inc; use perconparam; use rate_const
!
      allocate(ibl(2,maxint),ilbe(3,maxint),iba(3,maxint),ito(4,maxint))
      allocate(imp(4,maxint))
      ibl=0;ilbe=0;iba=0;ito=0;imp=0
!
      allocate(INPBL(MAXINT),INPLBE(MAXINT),INPBA(MAXINT),INPTO(MAXINT),INPIM(MAXINT),&
      INFCFAC(MAXINT))
      INPBL=0; INPLBE=0; inpba=0; inpto=0; inpim=0; infcfac=0
!
      allocate(fcfac(maxint),FCFACPJ(MAXINT,4),iblpj(2,maxint,4),IBAPJ(3,MAXINT,4), &
      ITOPJ(4,MAXINT,4),IMPPJ(4,MAXINT,4),ILBEPJ(3,MAXINT,4))
      fcfac=0.d00; fcfacpj=0.d00; iblpj=0; ibapj=0; itopj=0; ilbepj=0
      end subroutine rpath_mem
!
!*********************************************************************************
!
      subroutine givtst_mem
      use gtst1; use perconparam; use gtst
      allocate(amassf(natoms),x0(5,natoms),y0(5,natoms),z0(5,natoms))
      amassf=0.d00; x0=0.d00; y0=0.d00; z0=0.d00

      allocate(freq(10,maxcar),sx(nsdm),vs(nsdm),vad(nsdm),gfreq(maxcar,nsdm),gfmom(nsdm),  &
      delg(nsdm),forc(maxcar,maxcar),cof(maxcar,maxcar),forcs(maxcar,maxcar),&
      forc1(maxcar,maxcar))
      freq=0.d00; vs=0.d00;vad=0.d00;gfreq=0.d00;gfmom=0.d00; sx=0.d00
      delg=0.d00;forc=0.d00;cof=0.d00;forcs=0.d00;forc1=0.d00
!
      allocate(AV(NSDM),BV(NSDM),CV(NSDM),DV(NSDM))
      av=0.d00; bv=0.d00; cv=0.d00; dv=0.d00
!
      allocate(xp(maxcar),amassp(maxcar),vecsv(maxcar),cofsp(maxcar,nfre),cofex1(maxcar,nfre), &
      v3(maxcar),p(maxcar,maxcar),dx(10,maxcar),par(nfre,10))
      xp=0.d00; amassp=0.d00; vecsv=0.d00; cofsp=0.d00; cofex1=0.d00;v3=0.d00
      p=0.d00; dx=0.d00; par=0.d00
      end subroutine givtst_mem
!
!********************************************************************************
!
      subroutine rph31_mem
      use rate_const; use perconparam;use common_inc; use intbsv
      if(.not.allocated(ws))then
         allocate(ws(nsdm,nvibm)); ws=0.d00
      end if
      if(.not.allocated(fmirs))then
         allocate(fmirs(nsdm,nvibm)); fmirs=0.d00
      end if
      if(.not.allocated(bfs))then
         allocate(bfs(nsdm,nvibm)); bfs=0.d00
      end if
      if(.not.allocated(vs))then
         allocate(vs(nsdm)); vs=0.d00
      end if
      if(.not.allocated(fmoms))then
         allocate(fmoms(nsdm)); fmoms=0.d00
      end if
      if(.not.allocated(f))then
         allocate(f(n3tm,n3tm)); f=0.d00
      end if
      if(.not.allocated(x))then
         allocate(x(n3tm)); x=0.d00
      end if
      if(.not.allocated(dx))then
         allocate(dx(n3tm)); dx=0.d00
      end if
!
      if(.not.allocated(hess31))then
         allocate(hess31(n3tm,n3tm,nsdm)); hess31=0.d00
      end if
!
      if(.not.allocated(save31)) then
        allocate(save31(N3S31,NPT31)); save31=0d0
      endif
      if(.not.allocated(dxsad))then
         allocate(dxsad(n3tm)); dxsad=0.d00
      end if
      if(.not.allocated(cof))then
         allocate(cof(n3tm,n3tm)); cof=0.d00
      end if
      if(.not.allocated(fmomhr))then
         allocate(fmomhr(n3tm)); fmomhr=0.d00
      end if
!
!  INTBSVx
!
      if(allocated(fl))deallocate(fl)
      allocate(fl(maxca2)); fl=0.d00
      if(allocated(core))deallocate(core)
      allocate(core(maxcor)); core=0.d00
      end subroutine rph31_mem
!
!**********************************************************************
!
      subroutine rph40_mem
      use rate_const; use perconparam;use common_inc;; use fu_40
      use intbsv
      if(.not.allocated(ws))then
         allocate(ws(nsdm,nvibm)); ws=0.d00
      end if
      if(.not.allocated(fmirs))then
         allocate(fmirs(nsdm,nvibm)); fmirs=0.d00
      end if
      if(.not.allocated(bfs))then
         allocate(bfs(nsdm,nvibm)); bfs=0.d00
      end if
!
      if(.not.allocated(xk3s))then
         allocate(xk3s(nsdm,nvibm)); xk3s=0.d00
      end if
      if(.not.allocated(xk4s))then
         allocate(xk4s(nsdm,nvibm)); xk4s=0.d00
      end if
      if(.not.allocated(freq))then
         allocate(freq(n3tm)); freq=0.d00
      end if
!
      if(.not.allocated(dx40))then
         allocate(dx40(n3tm)); dx40=0.d00
      end if
      if(.not.allocated(cof))then
         allocate(cof(n3tm,n3tm)); cof=0.d00
      end if
!
!  INTBSVx
!
      if(allocated(fl))deallocate(fl)
      allocate(fl(maxca2)); fl=0.d00
      if(allocated(core))deallocate(core)
      allocate(core(maxcor)); core=0.d00
      end subroutine rph40_mem

      subroutine zucupd_mem
      use common_inc; use perconparam; use rate_const
      if(.not.allocated(hrmits)) then
         allocate(hrmits(n3tm)); hrmits =0d0
      endif
      if(.not.allocated(hrmir))then
         allocate(hrmir(n6tm)); hrmir=0.d00
      end if
      if(.not.allocated(wer))then
         allocate(wer(n6tm)); wer=0.d00
      end if
      if(.not.allocated(freq))then
         allocate(freq(n3tm)); freq=0.d00
      end if
      if(.not.allocated(ab))then
         allocate(ab(n6tm)); ab=0.d00
      end if
      if(.not.allocated(gse))then
         allocate(gse(n6tm)); gse=0.d00
      end if
      if(.not.allocated(ssubi))then
         allocate(ssubi(nsdm)); ssubi=0.d00
      end if
      if(.not.allocated(vclas))then
         allocate(vclas(nsdm)); vclas=0.d00
      end if
      if(.not.allocated(wets))then
         allocate(wets(nvibm,nsdm)); wets=0.d00
      end if
      if(.not.allocated(fmihts))then
         allocate(fmihts(n3tm,nsdm)); fmihts=0.d00
      end if
      if(.not.allocated(vadib))then
         allocate(vadib(nsdm)); vadib=0.d00
      end if
      if(.not.allocated(fmits))then
         allocate(fmits(nsdm)); fmits=0.d00
      end if
      if(.not.allocated(delg))then
         allocate(delg(nsdm)); delg=0.d00
      end if
      if(.not.allocated(delig))then
         allocate(delig(nsdm)); delig=0.d00
      end if
      if(.not.allocated(xets))then
         allocate(xets(nvibm,nsdm)); xets=0.d00
      end if
      if(.not.allocated(y0ts))then
         allocate(y0ts(nvibm,nsdm)); y0ts=0.d00
      end if
      if(.not.allocated(srarr))then
         allocate(srarr(narrl)); srarr=0.d00
      end if
      end subroutine zucupd_mem
!
!****************************************************************************
!
      subroutine rstat_mem
      use cm; use perconparam
      if(.not.allocated(temwer))then
         allocate(temwer(8,n3tm),temge(n3tm,n3tm),temhes(n3tm,n3tm))
         temwer=0.d00; temge=0.d00; temhes=0.d00
         allocate(spics(nsdim),spicv(nsdim))
         spics=0.d00; spicv=0.d00
      end if
      if(.not.allocated(ixmode))then
         allocate(ixmode(n3tm,8)); ixmode=0
      end if
      end subroutine rstat_mem
!
!*****************************************************************************
!
      subroutine sst_mem
      use sst; use perconparam
      if(.not.allocated(dmtor))then
         allocate(dmtor(ntor),dbw(n3tm,nsdm),dbwtmp(nsdm),torbh(ntor,nsdm),tbh(ntor),detds(nsdm))
         dmtor=0.d00; dbw=0.d00; dbwtmp=0.d00; torbh=0.d00;tbh=0d0;detds=0.d0
      end if
      end subroutine sst_mem
       
      subroutine lcg_mem
      use perconparam, only : maxps,nsv,nqd
      use common_inc, only : vefrst,vlcic,zetic
      if (.not.allocated(vefrst)) then
         allocate(vefrst(MAXPS,NSV,NQD),vlcic(nqd),zetic(nqd))
         vefrst =0.d0; vlcic = 0.d0; zetic=0.d0
      endif
      end subroutine lcg_mem

      subroutine grddx_mem
      use perconparam, only : NSV, NQD
      use common_inc
      if(.not.allocated(vinad)) then
        allocate(VINAD(NSV,NQD),VADGRD(NSV,NQD),DOTGRD(NSV,NQD),QDWNAD(NSV,NQD),&
                 VCGRD(NSV,NQD),STGRD(NSV,NQD),TPGRD(NSV,2),EANH(NSV,2),VEGRD(NSV,2),&
                 QDNAD(NSV,NQD),ENGRD(NSV),ZETGRD(NSV))
        vinad=0d0; vadgrd=0d0;dotgrd=0d0;qdwnad=0d0; vcgrd=0d0; stgrd=0d0;
        tpgrd=0d0; eanh=0d0; vegrd=0d0; qdnad=0d0; engrd=0d0; zetgrd=0d0
        allocate(IZGRD0(NSV),IZGRD1(NSV))
        izgrd0=0; izgrd1=0
      endif
      end subroutine grddx_mem
     
      subroutine lcset_mem
      use perconparam, only : n3tm,nvibm, NQD,nsdml
      use rate_const, only : rx0, rx1,vadex,tplxx,tprx,wpsv
      use common_inc, only : bm1, bm2, vxpr, vcorpr,ebpr,vef
        if(.not.allocated(rx0)) then
          allocate(rx0(n3tm),rx1(n3tm),bm1(nvibm),bm2(nvibm))   
           rx0=0d0; rx1=0d0; bm1=0d0; bm2=0d0
          allocate(VXPR(NQD),VCORPR(NQD),EBPR(NQD),VEF(NQD))
           vxpr=0d0; vcorpr=0d0; ebpr=0d0; vef=0d0
          allocate(vadex(nsdml),tplxx(nvibm,nsdml),tprx(nvibm,nsdml),wpsv(nsdml))
           vadex=0d0; tplxx=0d0; tprx=0d0; wpsv=0d0
        endif
      end subroutine lcset_mem

      subroutine pot_mem
      use perconparam, only : maxps, nsv
      use potmod, only : esvjac,s0jac,s1jac
         if(.not.allocated(esvjac)) then
           allocate(ESVJAC(MAXPS+1,NSV),S0JAC(MAXPS+1,NSV),S1JAC(MAXPS+1,NSV))
           esvjac=0d0; s0jac=0d0; s1jac=0d0
         endif
      end subroutine pot_mem
      
      subroutine anh_mem
      use perconparam, only : n3tm, ncr2
      use common_inc, only: xi,dxi,ri,vec1,vec2,vec3,vec4,vec5
        if(.not.allocated(xi)) then
          allocate(xi(n3tm),dxi(n3tm),ri(ncr2),vec1(n3tm),vec2(n3tm),&
           vec3(n3tm),vec4(n3tm),vec5(n3tm))
          xi=0d0; dxi=0d0; ri=0d0; vec1=0d0; vec2=0d0; vec3=0d0;
          vec4=0d0; vec5=0d0
        endif
      end subroutine anh_mem

      subroutine ivtst1_mem
      use perconparam, only: nsdm,nsdim
      use ivtst1
      if(.not.allocated(armcc)) then
         allocate(armcc(nsdm),armcsc(nsdim))
         armcc=0d0;armcsc=0d0
      endif
      end subroutine ivtst1_mem

      subroutine cubst_local_mem
      use energetics_mod
      use perconparam, only : n3tm 
      if(allocated(f2))deallocate(f2)
      allocate(f2(n3tm,n3tm)); f2=0.d00
      allocate(f1(n3tm,n3tm),fin(n3tm,n3tm+1),fth(n3tm,n3tm))
      allocate(vt1(n3tm),vt2(n3tm),vt3(n3tm),vt4(n3tm),xix(n3tm),xs(n3tm))
      allocate(iwork(n3tm))
      end subroutine cubst_local_mem
      
      subroutine wkb_mem
      use rate_const, only : etp,tp1,tp2
      use perconparam, only : n3tm
      if(.not.allocated(etp)) then
        allocate(etp(n3tm),tp1(n3tm),tp2(n3tm)); etp=0d0; tp1=0d0; tp2=0d0
      endif 
      end subroutine wkb_mem
     
      subroutine bath_mem
      use cm
      use perconparam, only: n3tm
      if(.not.allocated(diffu)) then
         allocate(diffu(n3tm)); diffu=0d0
      endif
      end subroutine bath_mem
      
      subroutine vrc_mem
      use perconparam
      use cm
      if(.not.allocated(xpp))  allocate(xpp(n3pt,2))
      if(.not.allocated(xpvt)) allocate(xpvt(n3pt,2))
      if(.not.allocated(svrc)) allocate(svrc(npivots,npivots))
      if(.not.allocated(xrp))  allocate(xrp(n3tm))
      return
      end subroutine vrc_mem

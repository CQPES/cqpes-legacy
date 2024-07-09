
       module fu_40
       save
       character(6) :: gmunit, gdunit, hsunit,hsform
       character(7) :: frqsrc, frequ, anhm40, curv40
       character(8) :: grdder
       character(12) :: prnt40
       integer :: npts,mxlpts
       logical :: extrpr,extrpp
       real(8) :: sb140,sb240
       real(8), allocatable :: dx40(:),dx140(:),dx240(:),bfs40(:)
       end module fu_40
!
!**************************************************************************************
!
       module kintcm
!
!  KINTCM - 1 line only !!!!
!
       integer :: iunxt,irepr(8),jniter,jxfreq(8),ifprnt,iproj(8),ipath,iscalerp,    &
       isym,ncnst(8),ndiat(8),imor(8),imtyp(8),imorqq(8),iqqwkb(8),iwkb(8),ivary(8), &
       nline(5),iolin(8),ilina(8),ikbprt(8),ikbqua(8),iharm(8),iqqsem(8),            &
       inxtpt,isfrst,ieuler,ies1,ipagem,iexrct,iexprd,iprstp,                        &
       iprsve,iprsmd,iwign,izct,ispec,ispcpr,nstep,isct,iprdis,                      &
       isplne,nlang,ifrate,ibrate,nrev,nsigf,nsigr,                                  &
       ngflag,ieact,ianaly,itst,nfcvt,icvt,muvt,nfus,iprg,iprigt,iprt,               &
       iprvib,igspec,igtemp,imerg,itunnl,irate,iemin,nexcit,                         &
       nregon,nptinf,iscsag,nmodes(8),nmnumb(8),ipprob,ipfreq,nxmod(8),              &
       ifirst,ihess,imeff, ilct,ivavg,ilcrst,ilcstr,ilcgit,                          &
       ipdat,ipgrid,intlct,ivicm,ivico,ipvib,ipvibp,ipvibc,                          &
       icheck,ivic,iscale,mniter,ifit1,ifit2,mnprmv,ipot,ispot,igpot,ivtst,nvarj(8), &
       ihrec, ihrect, ibfgs, ibfgst, ihunit, iretry,nlistl(11),ibasis,izmat,         &
       ireord,iezeru,initg(8),isup,intmu,inosad,iclasv,nprsmd,istatu(8),ifreu(8),    &
       irods,ivrp,nfcus,ncusmx,igtlog,iqrnst,isstop,iqrnsq,iclf, iprcd, idcpt, isel, &
       ibathm,ieft,ief,iprmd(8),idvmep,ircoup,ifrfac,incrf
       real(8) :: freqbottom  
       end module kintcm
!
!**************************************************************************************
!
       module keyword_interface
       save

       integer :: linef(4),icmod,imdmov,iwrt30,icrst,nratmd(8),iwrt31,iunit5,iunit6, &
       ivice
       character(3) :: prpart
       character(5) :: fstep, coord, state
       character(6) :: intdef
       character(7) :: pdirect, curv, psign
       character(8) :: potnam, potsec, potgeo
       character(9) :: numtyp,cezero
!
       character(8)  :: idmn(8), mortyp(8)
       character(15) :: avar(8,30)
       character(70) :: nlistf(4,5),nlist(11,5)
       character(80) :: ftitle(5), otitle(5)
!
       character(9),  allocatable :: vharmr(:,:), cmodet(:,:)
       character(30), allocatable :: basis(:), zmat(:,:)
!
!    UNIT
!
       real(8) :: gufac5,gufac6
       end module keyword_interface
!
!**************************************************************************************
!
       module cm
       logical :: lbath,lfopt
       integer :: iwrt62,ivrc,nmc,npvt(2),igtype,jstep,jmax,iejmuvt,nniter
       integer :: iprxnt,irex,nspic
!
       integer,allocatable :: label(:), icnst(:,:),ixmode(:,:),nmode(:),             &
       ippsv(:,:),ip2r(:,:),ifqlow(:)
!
       real(8) :: sdel,svl,svu,svs,frict,batemp,rmax,rmin,omin,ddmax,ssmaxts,flsr,   &
       flsp,sspmax,spfac,steng(8),scale,dlx1,convg,convgt,stptol,              &
       xdemin(8),xantlr(8),xdqqp(8,2),xwkbtl(8),xnmstp,delex,alpha,gtemp(40),        &
       etpair(10,2),analt(40),xsmmvt,xspmvt,sdebg1,sdebg2,egrid
!
       real(8),allocatable :: xpp(:,:), xrp(:),xpvt(:,:),svrc(:,:),diffu(:),xssav(:),&
       spics(:),spicv(:),sbkap(:),temwer(:,:),temge(:,:),temhes(:,:),sanhrm(:,:)
       real(8), allocatable :: xmass(:),xvdw(:),frelow(:,:)
!
!  Scratch storage
!
       integer, allocatable :: itmp(:)
       real(8), allocatable :: rtmp(:)
       end module cm
!
!**************************************************************************************      
!
       module dxiz
       SAVE GV
       SAVE GAS
       real(8) :: dlx2
       integer :: iz0old,iz1old
       real(8), allocatable :: gv(:),gas(:)
       end module dxiz
!
!**************************************************************************************
!
       module common_inc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************************************************!
!*                                                                                      *!
!*                                COMMON.INC  file                                      *!
!*                                                                                      *!
!****************************************************************************************!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
       integer :: n3,n3m6,lgs(39),icode(8),nratom(8),nf(8),lsave,nedeg(24),    &
       lgs2(39),nderiv
!
       integer, allocatable :: iatom(:),mode(:),moder(:,:)
!
       real(8) :: dlx,redm,s,demin,fmom(8),eprd,redmf,redmr,sigmaf,sigmar,elec(24),  &
       vad,var,vap,wstar,dqqp(2),elast,d3lx,ewr,ewp
       real(8),allocatable :: amass(:),f(:,:),xi(:),dxi(:),ri(:),vec1(:),vec2(:),    &
       vec3(:),vec4(:),vec5(:)
!
!**************************************************************************************
!
!
!     CONTAINS THE SCRATCH SPACE USED FOR THE CURVILINEAR COORDINATE
!     BASED FREQUENCY CALCULATIONS.  ICCRCM HAS THE CORRESPONDING FLAGS.
!
       integer :: nbl,nba,nto,nimp,nlbe,numint
       integer, allocatable :: ibl(:,:),iba(:,:),ilbe(:,:),ito(:,:),imp(:,:)
       real(8), allocatable :: scracc(:,:,:)
!
!  End of internal coordinate variables
!
       logical :: lpryl
       integer :: lgrad,lpgrd,icutf,ncutf,inums,iunit(8),nspec,isen,narr,ncom, &
       niter
       
       integer, allocatable :: inumi(:),ilb(:,:),iub(:,:),intout(:),modets(:,:)
!
!      real(8) :: slcutf, srcutf,derstp,sob,soe,vmxwkb,ezer0,conw(40),concag(40),    &
!      cvtcag(40),civt(40),cmvtca(40),cuvtca(40),zctkt(40,6),cdkt(40,6),clkt(40,6),  &
!      comtkt(40,6),uomtkt(40,6),cust(40),custr(40),custf(40),re(8),de(8),be(8)
       real(8) :: slcutf, srcutf,derstp,sob,soe,vmxwkb,ezer0,conw(40),concag(40),    &
       cvtcag(40),cmvtca(40),cuvtca(40),zctkt(40,6),cdkt(40,6),clkt(40,6),  &
       comtkt(40,6),uomtkt(40,6),cust(40),custr(40),custf(40),re(8),de(8),be(8)
       

       real(8), allocatable :: bcurv(:),tps(:,:,:),bm1(:),bm2(:),cofx(:,:),freq(:),  &
       cof(:,:),anhrm(:),wer(:),xer(:),y00(:),y00r(:),sspec(:),gse(:),ab(:),         &
       fmomhr(:),fmihr(:),srarr(:),cofbsv(:,:),coftsv(:,:),wew(:),xew(:),y0w(:),     &
       fmihw(:),xcom(:),xicom(:),xr(:,:)
!
!   TOR
!
       integer, allocatable :: ntrnb(:,:),ntrsch(:,:),ntrlev(:,:),ntrnum(:,:),       &
       ntrsig(:,:,:),ntrisb(:,:,:),ntrbnd(:,:,:),ntrm(:,:),ntrmtd(:,:)
       real(8), allocatable :: torw(:,:,:),toru(:,:,:),torome(:,:,:),tormi(:,:,:),   &
       torintrp(:,:),torzpc(:,:),torwl(:,:,:),torwr(:,:,:),ratiol(:,:,:),            &
       ratior(:,:,:),readi(:,:)
!
!  IRCCM
!
       real(8) :: dxnorm, dxmag, dxmago
!
!  LTUNCM, LENECM
!
       logical :: lcdsc,llcg, llcgg, lmep, ltun, lsign,lezer0
!
!  MASSCM, IMASCM, MINCOM, IMINCM, NPNCOM, OUTPCM
!
       integer :: npair,it1(10),it2(10),ntrat,itr(41),ndim(8)
       integer, allocatable :: iatsv(:,:),ind(:),indx(:,:),indx0(:,:),npacc(:),mpacc(:)
       real(8) :: step,cnvg,antlr
       real(8), allocatable :: svmas(:)
!
!  PATHCM, IPTHCM, PCVTCM, PERCM
!
       integer :: nst,nst2,nst0
!      real(8) :: slp,slm,sprnt,del1,delta2,diffd,civtf(40),civtr(40),sicvt(40),     &
!      pemin,per,ez,fqm0
       real(8) :: slp,slm,sprnt,del1,delta2,diffd,sicvt(40),     &
       pemin,per,ez,fqm0
       real(8), allocatable :: delig(:)
!
!  PGCOM, IPGCOM, PHASCM, IPHSCM, PEFCOM
!
       integer :: npt,indph  
       integer, allocatable :: in1(:),in2(:)
       real(8) :: ptpg(100),wtpg(100),qpg,dredm(4),v
       real(8), allocatable :: csv(:,:),sgn1(:),sgn2(:),x(:),xxc(:),dx(:)
!
!  PROJCM, PROVCM, PTBCM, GQDACM, IQDKCM, QDKACM, QDTCM
!
       integer :: nq12,nseg,nqq1,nq22,nseg2,nqq2
       real(8) :: vtt,despmag,egrndr(8),berot(3),pvec(3,3),egrndt,          &
       pt(81),wt(81,2)
       real(8), allocatable :: proj(:,:),xnou(:),gguard(:),savex(:),efndtr(:),       &
       egrnd(:),efndt(:,:),fijk(:,:,:),fiijj(:,:),efndtp(:),zetapt(:,:,:)
!
!  QDTHCM
!
       real(8) :: pt2(81),wt2(81,2)
!
!  GRDDX, GRDDX2
!
      real(8), allocatable :: vinad(:,:),VADGRD(:,:),DOTGRD(:,:),QDWNAD(:,:),        &
      VCGRD(:,:),STGRD(:,:),TPGRD(:,:),EANH(:,:),VEGRD(:,:),QDNAD(:,:),ENGRD(:),     &
      ZETGRD(:)
      integer, allocatable :: IZGRD0(:),IZGRD1(:)
      integer :: IE0GRD 
!
!  VEFFCM Common block with LCNAD and LCGTHE
!
      real(8), allocatable :: VXPR(:),VCORPR(:),EBPR(:),VEF(:)
!
!     LCRSCM Common with subroutine LCG. Restart from unit fu48
!     LCICOM Common with LCGIT
!
      real(8), allocatable :: vefrst(:,:,:),vlcic(:),zetic(:)
!
      end module common_inc
!
!**************************************************************************************
!
       module rate_const
!
!  RATECM, IRATCM, RESCOM, IRESCM, RODSCM
!
       logical :: lrods
       integer :: ngspec,iswr,iswp,nshlf
       real(8) :: slma(40),slpa(40),vfac,slmg,slpg,freqfac,del,delsv
       real(8), allocatable :: delg(:),ssubi(:),geom(:,:),vclas(:),fmits(:),          &
       wets(:,:),xets(:,:),vadib(:),y0ts(:,:),fmihts(:,:),cdscmu(:),zocmcd(:)
!
!  RPHACM, RPHCM, IRPHCM RPHGCM,IRPGCM,RPHWCM
!
       integer :: issp,lopt(40),nss,nint1,ninth,ifitvr,ifitvp,npoint,nsf,             &
       nsl,inm31,nhess,lbexp,iugm31,iugr31,iuhs31,igof31,irecs,igem,ifxmf,nexp,       &
       llopt(40)
       integer, allocatable :: ifreq(:),ifrsad(:),ifitwr(:),iftk3r(:),iftk4r(:),      &
       ifitwp(:),iftk3p(:),iftk4p(:)                              
!
       real(8) :: vshift,sb(3),avr(3),afir(3),avp(3),afip(3),uglig,uglsg,uglih,uglsh, &
       sincw,tension,sign1,sign2
       real(8), allocatable :: xk3(:),xk4(:),ss(:),vs(:),ws(:,:),xk3s(:,:),           &
       xk4s(:,:),fmoms(:),bfs(:,:),x1(:),dx1(:),dx2(:),dxsad(:),dxp(:),cofsad(:,:),   &
       awr(:,:),ak3r(:,:),ak4r(:,:),abfr(:,:),awp(:,:),ak3p(:,:),amir(:,:),           &
       amip(:,:),fmirs(:,:),ak4p(:,:),abfp(:,:),xxs(:,:),save31(:,:),xxext(:,:),      &
       hess31(:,:,:),fsv(:,:)
!
!  RSTOCM, SPLHCM, SPLNCM, ISPLCM, STOPCM, SUM1CM, SURFCM, TEMPCM, ITMPCM
!
       integer :: nspl,ntemp
!
       real(8) :: sincr,fracdw,sum1,temp(40),beta(40),kapcag(40),kapsag(40),          &
       scvt(40),kapw(40),kapcvt(40),conf(40),conr(40),cvtf(40),cvtr(40),kaplcg(40),   &
       kacdsc(40),kacomt(40),kamomt(40),kntzct(40),kntsct(40),kntlct(40),kntmot(40)         

       real(8), allocatable :: ach(:),bch(:),cch(:),dch(:),aspl(:),bspl(:),cspl(:),   &
       dspl(:),potinf(:)
!
!  THDECM, TRNPCM, TX1CM, TX2CM, ITX2CM, TX3CM, ITX3CM, ITX3CM, ITX3CN, TX5CM
!
       integer :: imax,ncol,nvef,ng,ngs0,iot,iexog
       integer, allocatable :: npex(:),nsplic(:),nsplix(:),nspliy(:)
       real(8) :: zeta0,zeta1,smax,vmax,delnt
       real(8), allocatable :: v3(:),tpl(:),tpg(:),rx0(:),rx1(:),vadex(:),tplxx(:,:), &
       tprx(:,:),wpsv(:),vecsv(:)
!
!  ITX5CM,TXXCM
!
       integer :: libeg,nsdex 
       real(8), allocatable :: bcur(:,:),dxsv(:,:),cofsv(:,:,:)
!
!  VACOM, IVACOM, VARCOM, IVRCOM
!
       integer :: l0,jswitc,ivar, inh, inm, isi, iri, iti,ini
       integer, allocatable :: lrp(:),ln3(:,:),l9(:,:)       
       real(8) :: switc,odelsv
!
!  VTMUCM, VTUSCM, WKBCOM, IWKBCM, WKBZCM, IWBZCM, XTRACM, IXTRCM
!
       integer :: ifwkb,kbquad,kbprnt,ikbm,ikbs,nste(2),iwha(2)
       real(8) :: cmuvtf(40),cmuvtr(40),cusvtf(40),cusvtr(40),wkbtol,dlex(2),alph(2)
       real(8), allocatable :: tp1(:),tp2(:),etp(:),ewkb0(:,:),wgsex(:)
!
!  ZOCCM, IZOCCM, LZOCCM, IQSTCM, SQSTCM
!
       logical :: lzoc,lzcrst
       integer :: ivfun,imfun,lgsic(40),iwr, nbound, iqrst
       integer, allocatable :: iffun(:),icfr(:),ifrr(:),ifrp(:)
!
       real(8) :: tswim,barra,barrs,erxn,av, bv, cv, s0v, range, vzoclc,avs,bvs,cvs,  &
       s0va,s0vs,rangea,ranges,av1,av2,bv1,bv2,cv1,cv2,s0v1,s0v2,sp1,sp2,vp1a,vp2a,   &
       vp1s,vp2s,fmispa, fmisps,fmir1a, fmir2a, fmip1a, fmip2a,fmim1a,srw
       real(8), allocatable :: wera(:),wesads(:),wesada(:),af(:),bf(:),cf(:),s0f(:),  &
       af1(:), bf1(:), cf1(:), s0f1(:),af2(:), bf2(:), cf2(:), s0f2(:),frp1a(:),      &
       frp2a(:),frp1s(:),frp2s(:),hrmir(:), hrmits(:), enlvrc(:)
!
!  IPRMCM, FCSCCM, FCFACM, INTRCM, FCFPCM
!
       integer :: itvmep,nfrind,ifrind(10),iprmep,ixmol,nprca,iprca(4),ninp, nscfac,  &
       ifcfac, isclpt, icartrp, lgs3(5),nblpj(4),nbapj(4),ntopj(4),nlbepj(4),nintpj(4)                                           
       integer, allocatable :: inpbl(:),inplbe(:),inpba(:),inpto(:),inpim(:),infcfac(:),       & 
       iblpj(:,:,:),ibapj(:,:,:),itopj(:,:,:),imppj(:,:,:),ilbepj(:,:,:)
!
       real(8), allocatable :: fcfac(:),fcfacpj(:,:)
       end module rate_const
!
!**************************************************************************************
!
       module potmod
!
!  JACCOM, ITJACM, IFRSCM
!
       integer :: ipdjac, iejac, irtpjac,ifqfac
       real(8) :: tempjac,rtejac, rtelct(40)
       real(8), allocatable :: esvjac(:,:),s0jac(:,:),s1jac(:,:)
!
!  Define double precision variables
!
      real(8) :: kapcag,kapsag,kapw,kapcvt,kaplcg, kacdsc,kamomt,kacomt,kntzct,kntsct, &
      kntlct,kntmot
      end module potmod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************************************************!
!*                                                                                      *!
!*                                 PARAM.INC   file                                     *!
!*                                                                                      *!
!****************************************************************************************!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module perconparam
      implicit real(8) (a-h,o-z)
!
!   PARAMETER STATEMENTS FOR POLYRATE 
!
!   THESE ARE THE ONLY PARAMETERS WHICH SHOULD BE MODIFIED BY THE USER.
!
!   NOTE:   IF THESE PARAMETERS ARE MODIFIED THE SOURCE CODE FOR 
!           POLYRATE MUST BE RECOMPILED 
!
!               NSDM      =    THE MAXIMUM NUMBER OF SAVE POINTS 
!                              ALONG THE MINIMUM ENERGY PATH
!
!               NSDML     =    THE MAXIMUM NUMBER OF SAVE POINTS 
!                              FOR THE TUNNELING CALCULATIONS 
!                              (NSDML .LE. NSDM)
!
!               NPOTPT    =    MAXIMUM NUMBER OF PIECES OF INFORMATION
!                              WHICH MAY BE PASSED FROM THE POTENTIAL
!                              ENERGY SURFACE (THIS INFORMATION IS NOT
!                              USED IN THE CALCULATION BUT MAY BE 
!                              PRINTED TO UNIT FU6 USING THE LGS2(5)
!                              FLAG IN THE UNIT FU5 INPUT DECK)
!
!               MAXPS     =    MAXIMUM NUMBER OF PRODUCT STATES ALLOWED
!                              FOR THE LCG3 TUNNELING CALCULATION
!
!               MAXWKB    =    MAXIMUM NUMBER OF REACTANT STATES ALLOWED FOR THE
!                              QUANTIZED REACTANT STATE TUNNELING CALCULATION 
!
!               NMSPEC    =    MAXIMUM NUMBER OF EXTRA SAVE POINTS
!
!               INMM      =    MAXIMUM VALUE FOR RATIO BETWEEN GRADIENT
!                              SAVE POINTS AND HESSIAN SAVE POINTS NUMBERS
!                              (USED ONLY FOR FU31 RUNS)
!
!               FU#       =    FORTRAN UNIT NUMBER FOR SPECIFYING THE
!                              POLYRATE INPUT AND OUTPUT FILES 
!                              (SEE THE POLYRATE MANUAL FOR A FULL 
!                               DESCRIPTION OF THE FILES ASSOCIATED 
!                               WITH EACH #)
!
!               MAXS      =    MAXIMUM NUMBER OF DIVIDING SURFACES IN VTST WITH 
!                              VARIABLE REACTION COORDINATE AND MDS FOR BARRIERLESS
!                              ASSOCIATION REACTION
!             
!               MAXE      =    MAXIMUM NUMBER OF E GRIDS FOR MIRCOCANONICAL N(E,J) 
!
!               MAXJ      =    MAXIMUM NUMBER FOR the DIMENSION OF ARRAY N(E,J). IT 
!                              EQUAL TO MAXJ = JMAX/JSTEP
!
!
!      integer, parameter :: nsdim=901
!
!      PARAMETER (NSDM = 901) 
!
!      PARAMETER (NSDML = 901)
!
!  The variables below serves as dimensions of the allocatable arrays
!
!
!  NATOM, NATOMS - Actual number of atoms in the system
!
      integer :: natom,n3tm,n6tm,ncr2,n3tm1,n3s31,nvib,nvibm,nfre,natoms, &
      maxd,nb,n3m7
      integer :: maxint,maxcar,maxca2,maxin2,maxcor
!
!  NSDIM  =  Number of MEP points if the direct dynamics is used
!         =  901 otherwise
!
      integer :: nsdim,nsdm,nsdml,npt31
      integer, parameter :: nsdm_percon=901
!
      PARAMETER (NPIVOTS = 6)
!
!      PARAMETER (MAXINT=3*NATOMS+6, MAXCAR=3*NATOMS)
!
      PARAMETER (NPOTPT = 1)
!
      PARAMETER (MAXPS = 5)
!
      PARAMETER (MAXWKB = 50)
!
      PARAMETER (NMSPEC = 20)
!
      PARAMETER (INMM = 10)
!
      PARAMETER (MAXS = 80, MAXE=24, MAXJ=150)
!
!  Note: the integer specification must be made before the parameter 
!        declaration.
!
      integer :: FU1,  FU2,  FU3,  FU5,  FU6,  FU8
      integer :: FU14, FU15, FU18, FU19
      integer :: FU20, FU21, FU22, FU25, FU26, FU27, FU28
      integer :: FU30, FU31
      integer :: FU71, FU72, FU73, FU74, FU75, FU77, FU78
      integer :: FU40, FU41, FU42, FU43, FU44, FU45, FU46, FU47
      integer :: FU50, FU51
      integer :: FU60, FU61, FU62, FU65
      integer :: FU48, FU49
!
      integer, parameter :: fu24=24
!

      PARAMETER (FU1 = 1, FU2 = 2, FU3 = 3, FU5 = 5, FU6 = 6, FU8 = 8)
!
      PARAMETER (FU14 = 14, FU15 = 15, FU18 = 18, FU19 = 19)
!
      PARAMETER (FU20 = 20, FU21 = 21, FU22 = 22)
!
      PARAMETER (FU25 = 25, FU26 = 26, FU27 = 27, FU28 = 28)
!
      PARAMETER (FU30 = 30, FU31 = 31)
!
      PARAMETER (FU71 = 71, FU72 = 72, FU73 = 73, FU74 = 74, FU75 = 75, &
                FU77 = 77, FU78 = 78)
!
      PARAMETER (FU40 = 40, FU41 = 41, FU42 = 42, FU43 = 43)
      PARAMETER (FU44 = 44, FU45 = 45, FU46 = 46, FU47 = 47)
      PARAMETER (FU48 = 48, FU49 = 49)
      PARAMETER (FU50 = 50, FU51 = 51, FU60 = 60, FU61 = 61, FU62 = 62, &
                FU65 = 65)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************************************************!
!*                                                                                      *!
!*                                 PERCON.INC   file                                     *!
!*                                                                                      *!
!****************************************************************************************!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
!#######################################################################
!
!  percon.inc
!
!    PERMANENT CONSTANTS FOR THE POLYRATE CODE
!
!    IT IS RECOMMENDED THAT THESE CONSTANTS NOT BE MODIFIED BY THE USER
! 
!#######################################################################
!
!  number of sparce grid point
!
!
!      PARAMETER (N3TM = 3*NATOMS, NVIBM = N3TM - 1)
!
      PARAMETER (N3PT = 3*NPIVOTS)
!
!      PARAMETER (N6TM = 2*N3TM, NCR2 = NATOMS*(NATOMS-1)/2 )
!
      PARAMETER (NARRS= 9, NARRL=NARRS-1, NMAXP = 10)
!
      PARAMETER (IDUMIT = 5)
!
      PARAMETER (NQD = 200)
!
      PARAMETER (NSV = 245)
!
!      PARAMETER (N3TM1 = N3TM+1)
!
!      PARAMETER (N3S31 = 2*N3TM+5)
!
!     PARAMETER (NPT31 = INMM*NSDM)
!  a small number
!
      real(8), parameter :: EPS = 1.0D-14,epsx = 5.0d-8
!
      PARAMETER (PI = 3.141592654D0, TPI = 6.283185307D0)
!
!  unit conversion from a.u. to cm-1
!    
      PARAMETER (AUTOCM = 2.19474627D+05, CMTOAU = 4.556335D-6) 
!
!  Boltzmann constant in a.u.
!
      PARAMETER (BK = 3.166830D-6) 
!
!  CAU - convert a.u. to amu*bohr2
!  CEV - convert a.u. to eV
!
!      PARAMETER (CAU = 5.48579905D-04, CEV = 27.21161D0) 
      real(8), parameter :: CAU = 5.48579905D-04, CEV = 27.21161D0
!
!  Convert a.u. to kcal/mol
!
      PARAMETER (CKCAL = 627.5095D0)
!
!  CKG - convert amu to kg
!  CKGM2 - convert amu*bohr2 to kgm2
!
      PARAMETER (CKG = 1.66024D-27, CKGM2 = 2.550946698D-4)
!
!  CNVRT - is convertion factor of the rate from au to cgs
!  CONK0 - is k0 in  au^3 
!
      PARAMETER (CNVRT = 4.13413789D+16, CONK0 = 6.748336825D+24)
!
!  Gas constant in kcal/mol-deg K
!
      PARAMETER (RCONST = 1.98717D-3)
! 
      end module perconparam
!
!**************************************************************************************
!
      module efmain_mod
!
!  OPEFCM
!
      real(8) :: xlamd,xlamd0,skal,dd,ddmaxts
      real(8), allocatable :: EIGVAL(:),oldfx(:),oldeig(:),fx(:),                        &
      hessc(:),hess(:,:),xparam(:),grad(:),uc(:),oldf(:),d(:),vmode(:),                  &
      u(:,:),oldhss(:,:), oldu(:,:),ooldf(:)
      integer :: mode,nstep
!
!  AMASCM
!
      real(8), allocatable :: amass(:),f(:,:)
      real(8) :: dlx,redm,s,demin,fmom(8),eprd,redfm,redmr,sigmaf,sigmar,elec(21),vad,   &
      var,vap,wstar,dqqp(2),elast,d3lx,ewr,ewp
      integer :: NEGREQ,IPRNT
!
!  EFPRCM
!
      integer :: iprxnt
      end module efmain_mod
!
!**************************************************************************************
!
      module energetics_mod
!
!  Local variables for file ENERGETICS.F
!
      real(8), allocatable :: f1(:,:),f2(:,:),fin(:,:),fth(:,:),&
      vt1(:),vt2(:),vt3(:),vt4(:),xix(:),xs(:)
      integer, allocatable :: iwork(:)
      real(8), allocatable ::  scr(:), scr2(:), xlam(:),u0(:,:)
!
!   INTPM, NEWTON
!
      real(8), allocatable :: xxi(:),hessin(:,:)
!
!   NEWTON
!
      real(8), allocatable :: x0(:), dx0(:),bfgs1(:,:), bfgs2(:,:), deltag(:)
      integer, allocatable :: work(:)
!
!   NORMOD
!
      real(8) :: sume(9),sbx(3)
      real(8), allocatable :: dxxp(:),xmscd(:),dxsav(:), anco(:,:),tempx(:,:)
!
!   EFCM
!
      real(8) ::RMAX,RMIN,OMIN,DDMAX,DDMAXTS
!   PREFCM
      real(8),allocatable :: fsp(:,:)
      end module energetics_mod
!
!**************************************************************************************
!
      module path_mod
!
!  INIT_PATH
!
      real(8), allocatable :: xs(:)
      integer, allocatable :: iwrdx(:),lsaves(:)
      real(8), allocatable :: dxb(:),dxold(:),xold(:),geomm(:,:),derivm(:,:)
!
      character(80), pointer :: poly_esp(:),poly_fu5(:)
      integer :: nlines_esp,nlines_inp,mode_esp
!
! MODE_ESP - set to nonzero value if direct dynamics is used
!          = 30 if POLY.FU30
!          = 31    POLY.FU31
      character(80) :: a
      end module path_mod
!
!**************************************************************************************
!
      module gf
      interface get_file
      
        subroutine closed_file2table(filename,ndim,table)
        character(80), pointer :: table(:)
        character(*), intent(in) :: filename
        integer, intent(in) :: ndim
        end subroutine closed_file2table
!
        subroutine opened_file2table(num_unit,ndim,table)
        character(80), pointer :: table(:)
        integer, intent(out) :: ndim
        end subroutine opened_file2table
      end interface get_file
      end module gf

      module upplow
      public; save
      character(1),dimension(26) :: upper,lower
      data upper/'A','B','C','D','E','F','G','H','I','J','K','L','M', &
      'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      data lower/'a','b','c','d','e','f','g','h','i','j','k','l','m', &
      'n','o','p','q','r','s','t','u','v','w','x','y','z'/
      character(1),dimension(22) :: no_num
      data no_num/',','[',']','|','?','>','!','@','#','$','%',&
       '^','&','*','-','_','=','+','/','\','{','}'/
      character(1),dimension(17) :: in_numbers
      data in_numbers/'1','2','3','4','5','6','7','8','9','0','e','E',&
      'd','D','.','-','+'/
      end module upplow
!
!**************************************************************************************
!
      module memanh
      integer, allocatable :: ntemp1(:),ntemp2(:), nsub(:,:),ntemp6(:,:), &
      ntemp5(:,:), ntemp3(:), ntemp4(:),ntemp7(:), ntemp8(:),ntemp9(:),   &
      ntemp10(:)
      real(8), allocatable :: tempr1(:,:),tempr2(:,:),tempr3(:,:),  &
      tempr4(:,:),tempr5(:,:),tempr6(:,:),tempr7(:,:),tempr8(:,:),temp11(:)
      end module memanh

      module ivtst1
      real(8), allocatable :: armcsc(:),armcc(:)
      end module ivtst1
!
!**************************************************************************************
!
      module intbsv
!
!  An array that stores everything
!
      real(8), allocatable :: core(:),fl(:)
      end module intbsv
!
!**************************************************************************************
!
      module gtst
!
!  DERVCM
!
      real(8), allocatable :: dx(:,:),forc(:,:),cof(:,:),forcs(:,:),forc1(:,:)
      real(8) :: xnorm
!
!  EIGCM, RPHKCM, 
!
      real(8), allocatable :: cofsp(:,:), cofex1(:,:)
      real(8) :: vmep,smin,smax,ds,dels
!
!  OUTCOM
!
      integer :: iprint,iscsag
!
!  ENECM, PTIVCM, PTMUCM
!
      real(8) :: vc,dele,vag,delh,v4,x4,var,vap,avc,bvc,gammav,xmvc,gammam
!
!  FVT1CM, FVT2CM, DFECM
!
      real(8) ::  SZ(3),FZ(3),FP,FR,A,B,C,SM,ALF,GAMMA,DFR1,DFP1,DFP2
      integer :: IFCN,INFO,NPT
!
!  RPHICM
!
      real(8), allocatable :: freq(:,:),sx(:),vs(:),vad(:),gfreq(:,:),gfmom(:),delg(:)
      real(8) :: fmom(10)
!
!  THDCOM
!
      real(8),allocatable :: v3(:),vecsv(:)
!
!  PARMCM
!
    real(8), allocatable :: par(:,:)
!
!  CURCOM
!
    logical :: lneg
!
!    QDTCM
!
    real(8) :: PTT(81),WTT(81,2)
    end module gtst
!
!**************************************************************************************
!
    module gtst1
!
!  GEOCOM
!
      real(8), allocatable :: x0(:,:),y0(:,:),z0(:,:),amassf(:)
!
!  KAPCM, RATCM
!
      real(8) :: t(40),tcag(40),cag(40),wig(40),tcsag(40),tcscg(40),conf(40),confw(40), &
      confz(40),cvt(40),cvtw(40),cvtz(40),cvts(40),scvt(40),cvtg(40)
!
!  QDKICM, QDTICM
!
      integer :: nqk,nqk2,nqt,nqt2
!
!  SPLCM
!

      real(8), allocatable :: av(:),bv(:),cv(:),dv(:)
!
!  PRJCOM
!
      real(8), allocatable :: p(:,:)
!
!  MUEFCM
!
      real(8) :: redmue(10)
!
!  SCCOM
!
      real(8), allocatable  :: xp(:),amassp(:)
!
!  ISPLCM
!
      integer :: ns
!
      end module gtst1

      module sst
      integer :: lsst,ntor
      real(8) :: detd,totmass
      real(8), allocatable :: dmtor(:),dbw(:,:),dbwtmp(:),torbh(:,:),tbh(:),detds(:)
      end module sst
     
      module potcm3
       integer :: nsurf, nder, ndum(8)
       real(8) :: r(3),energy, dedr(3)
      end module potcm3
 
      module potcm4
       integer :: nsurf, nder, ndum(8)
       real(8) :: r(6),vtot, dvdr(6)
      end module potcm4

!  module for clhbr potential
      module clhbr
       real(8) :: D(3),RE(3), BETA(3),Z(3)
       real(8) :: ZPO(3),OP3Z(3),ZP3(3),TZP3(3),TOP3Z(3),DO4Z(3),B(3)
      end module clhbr

!  module for oh3 potential
      module oh3cm
       real(8) :: RX(4),ENERGY,DEDR(4)
       real(8) :: XDE(3),XBETA(3),XRE(3),SATO,GAM(3),REOH,REHH, CON(7),&
                  ALP(4), CLAM(4), ACON(2)
      DATA XDE / 0.148201D0, 0.0275690D0, 0.151548D0 /  
      DATA XBETA / 1.260580D0, 0.924180D0, 1.068620D0 /  
      DATA XRE / 1.863300D0, 2.907700D0, 1.428600D0 /
      DATA SATO / 0.10D0 /
      DATA GAM / 2.399700D0, 1.058350D0, 2.399700D0 /
      DATA REOH / 1.808090D0 /
      DATA REHH / 2.861590D0 /
      DATA CON / -.0015920D0, 0.026963D0, 0.0014689D0, 0.080011D0,0.085816D0, -0.063179D0, 0.101380D0 /
      DATA ALP / 4.773D0, 7.14D0, 2.938D0, 5.28D0 /
      DATA CLAM / 0.10D0, 0.10D0, 0.20D0, 0.03D0 /
      DATA ACON / 0.10D0, 0.009D0 /
      end module oh3cm

      module oh3cm2
      real(8) :: DE(3), BETA(3), RE(3), Z(3), ZPO(3), OP3Z(3), ZP3(3), &
               TZP3(3), TOP3Z(3), DO4Z(3), B(3), X(3), COUL(3), EXCH(3)
      real(8), PARAMETER :: R2 = 1.41421356D0
      end module oh3cm2

      module h3potcm
!     real(8) :: r1,r2,r3,POTE1,DEDR(3)
!     integer :: nsurf, nder, ndum(8)
      real(8) :: pote2,dh3u(3),coup(3)
      end module h3potcm
      
      module h3potcm1
      real(8) :: ALPH2,ALPHA5,ALPH0,ALPH1,AL0,AL1,AL2,AL3,AZ2,BETA1   &
         ,BETA2,BETA3,BET0,BET1,CD0,CD1,CHH(3),CK0(3),CK1(3),HFD,HFA1, &
         HFA2,HFA3,HFGAM,H2RM,H2RMT,H2R0,RHOL,SQRT3,XPAR(15)

      real(8) :: PER,PER2,R12,R22,R32,QCOORD,DQ1,DQ2,DQ3,RHO,DRHO1,  &
         DRHO2,DRHO3,S,S2,DS1,DS2,DS3,CPHI3,DCPHI1,DCPHI2,DCPHI3      
      real(8) :: CORRS(3),DCORRS(3),CORRT(3),DCORRT(3),DAMP(3,3), DDAMP(3,3)

      DATA RHOL / 2.4848D0 /
      DATA ALPH0,ALPH1,BET0,BET1 / 25.9528D0,1.1868D0,15.7381D0, 0.09729D0 /
      DATA ALPHA5,BETA1,BETA2,BETA3 / 8.2433033D-3,0.53302897D0, &
         0.39156612D-1,0.69996945D0 /
      DATA ALPH2 / 4.735364D-1 /
      DATA AL0,AL1,AL2,AL3 / 0.45024472D+1,-0.62467617D+1,0.40966542D+1,  &
                             0.21813012D+1 /
      DATA AZ2 / 4.453649D-4 /
      DATA CD0,CD1 / 6.333404D-3,-1.726839D-3 /
      DATA CHH / 6.499027D0,1.243991D+2,3285.8D0 /
      DATA CK0 / -1.372843D-1,-1.638459D-1,-1.973814D-1 /
      DATA CK1 / 1.011204D0,9.988099D-1,9.399411D-1 /
      DATA HFD,HFA1,HFA2,HFA3,HFGAM / 0.15796326D0,2.1977034D0, &
         1.2932502D0,0.64375666D0,2.1835071D0 /
      DATA H2RM,H2R0,H2RMT / 1.401D0,6.928203D0,7.82D0 /
      DATA SQRT3 / 1.73205080756887D0 /
      DATA XPAR / -0.9286579D-2,0.2811592D-3,-0.4665659D-5,0.20698D-7,  &
                  0.2903613D+2,-0.2934824D+1,0.7181886D0,-0.3753218D0,  &
                  -0.1114538D+1,0.2134221D+1,-0.4164343D0,0.2022584D0,  &
                  -0.4662687D-1,-0.4818623D+2,0.2988468D0 /
      end module h3potcm1


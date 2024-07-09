!***********************************************************************
!
!     Gaussrate.inc include file
!
!***********************************************************************
!
      module gaussrate_param
      integer fu70
      integer fu81, fu82, fu83, fu84, fu85
      parameter (fu70=70)
      parameter (fu81=81, fu82=82, fu83=83, fu84=84, fu85=85)
      end module gaussrate_param

      module gaussrate_comm
      integer :: icharge(8),imultipl(8)                                 
      integer :: ioptgr(8),inlscr(8),inlfrs(8),inlene(8)                
      integer :: iop,isavect,irread,iopread,icodgdr,inlcer,inlbas,irest
      real(8), allocatable :: geresv(:),geprsv(:),gestsv(:),gewrsv(:),gewpsv(:), &
               fsresv(:),fsprsv(:),fsstsv(:),fswrsv(:),fswpsv(:),hsresv(:,:), &
               hsprsv(:,:),hsstsv(:,:),hswrsv(:,:),hswpsv(:,:),&
               georead(:),dxread(:),fmread(:,:)
      real(8) :: vjdep(8)
      real(8) :: enresv,enprsv,enstsv,enwrsv,enwpsv,eprod1,eprod2,ereac1,ereac2
      real(8) :: vread,fallout
      character*80 :: secrot(8,100),firrot(8,100),enerot(8,100),cerblk(100),basblk(100)
!
      real(8) :: storev
      real(8), allocatable :: storecoord(:),storedx(:),storefmat(:,:)
      end module gaussrate_comm

      module gr_eshscm
      real(8),allocatable :: fmat(:,:)
      integer, allocatable :: numci(:)
      end module gr_eshscm

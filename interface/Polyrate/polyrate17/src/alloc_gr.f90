      subroutine allocate_grmem
      use perconparam, only : n3tm
      use gaussrate_comm
      allocate(geresv(n3tm),geprsv(n3tm),gestsv(n3tm),gewrsv(n3tm),&
               gewpsv(n3tm),fsresv(n3tm),fsprsv(n3tm),fsstsv(n3tm),&
               fswrsv(n3tm),fswpsv(n3tm),georead(n3tm),dxread(n3tm),&
               fmread(n3tm,n3tm),hsresv(n3tm,n3tm),hsprsv(n3tm,n3tm),&
               hsstsv(n3tm,n3tm),hswrsv(n3tm,n3tm),hswpsv(n3tm,n3tm) )
      allocate(storecoord(n3tm),storedx(n3tm),storefmat(n3tm,n3tm))
      end subroutine allocate_grmem    

       module ch4oinc 
       real(8) :: theta0(4,4),dtheta0(4,4,4)    ! angles
       real(8) :: rcb,rch(4),r0ch,rbh(4),r0hh   ! bonds
       real(8) :: tcb(3),tch(4,3),tbh(4,3)      ! coords
       real(8) :: fdelta(4),hdelta(4)           ! delta1
       real(8) :: dfdelta(4,4),dhdelta(4,4)     ! delta2
       real(8) :: fk0(4,4),f1(4),dfdc(4,4,4),dfdh(4,4,4)  ! force1
       real(8) :: fkinf,ak,bk,aa1,aa2,aa3,aa4             ! force2
       real(8) :: a1s,b1s,a2s,b2s,a3s,b3s                 ! fsw1
       real(8) :: s1(4),ds1(4),s2(4),ds2(4)               ! ip1
!      real(8) :: nnc,nnb,nnh(4),nc(3),nhb(3),nh(4,3)     ! ndx
       integer :: nnc,nnb,nnh(4),nc(3),nhb(3),nh(4,3)     ! ndx
       real(8) :: s3(4),ds3(4)                            ! op1
       real(8) :: fch3,hch3                               ! params
       real(8) :: q(150),pdot(150)                        ! qpdot
       real(8) :: aphi,bphi,cphi                          ! sphil
       real(8) :: atheta,btheta,ctheta                    ! stheta1
       real(8) :: d1ch,d3ch,a1ch,b1ch,c1ch,d1hh,d3hh,ahh,d1cb,d3cb,acb ! stret1
       real(8) :: sphi(4),dsphi(4),stheta(4),dstheta(4)   ! switch1  
       end module ch4oinc 

       module ch4oinc1
! ICORCM
       real(8) :: RR(15),DERR(15)                         
! PDATCM
       real(8) :: D1(3),D3(3),ALPH(3),RE(3),BETA(3),CC(3),AA(3), &
                  APARM(5),REFV,TAU,CP,B1,C1
! POTXCM
       real(8) :: X(6),Y(6),Z(6),ENERGY,DRDX(15,18),DEDX(18)
       end module ch4oinc1


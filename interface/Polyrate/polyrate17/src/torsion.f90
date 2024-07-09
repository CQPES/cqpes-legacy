double precision function bes0(x)
!  Calculates the modified Bessel function of order 0, I_0(x),
!  using series expansion (Formula 9.6.10 of Abromowitz and Stegun).
!  Validated against values in table 9.8 (p. 416-422)
implicit none
integer :: k
double precision, parameter :: tol=1.d-30
double precision :: sum, x, term, xh, sumo
sum=1d0
term=1d0
xh=x/2d0
k=0
do
  k=k+1
  term=term*xh/dble(k)
  sumo=sum
  sum=sum+term*term
  if( (sum-sumo)/sum .lt. tol)exit
  if(k.gt.1000)exit
enddo
bes0=sum
end

double precision function bes1(x)
!  Calculates the modified Bessel function of order 1, I_1(x),
!  using series expansion (Formula 9.6.10 of Abromowitz and Stegun).
!  Validated against values in table 9.8 (p. 416-422)
implicit none
integer :: k
double precision, parameter :: tol=1.d-30
double precision :: sum, x, term, xh, sumo
sum=1d0
term=1d0
xh=x/2d0
k=0
do
  k=k+1
  term=term*xh/dble(k)
  sumo=sum
  sum=sum+term*term/(dble(k+1))
  if( (sum-sumo)/sum .lt. tol)exit
  if(k.gt.1000)exit
enddo
bes1=sum*xh
end

subroutine matprint(iunit,amat,ni,nj,ndim)
!
! This subroutine prints the elements of the ni rows and nj columns of array amat.
!
implicit none
integer, intent(in) :: ni, nj, ndim, iunit
integer :: numcol, jcol, jmin, i, jmax, im, j
double precision, intent(in) :: amat(ndim,ndim)

jcol=5
numcol=5
jmin=1
do
  jmax=min(jcol,nj)
  write(iunit,'(2x,"m-n",9(5x,i4,5x))')(im,im=jmin,jmax)                                  
  do i=1,ni                                                      
    write(iunit,'(1x,i4,1x,9es14.6)')i,(amat(i,j),j=jmin,jmax)                             
  enddo
  jmin=jmax+1
  jcol=jcol+numcol                                                  
  if(jmax.eq.nj) exit
enddo    
write(iunit,'(2x)') ! add a blank line at the end
CALL FLUSH(IUNIT)

end subroutine matprint

subroutine vibtor(gmat,fmat,ntor,nf,fscl,mtor,detd,tbh)
implicit none
integer, intent(in) :: ntor,nf
integer :: i, j, nontor, info, lwork
integer :: jcol, jmin, jmax
integer :: indx(nf)
double precision, allocatable :: work(:),g12(:,:),g21(:,:),g11(:,:),g22(:,:)
double precision, allocatable :: gfnontor(:,:),wrn(:),win(:),vln(:,:),vrn(:,:)
double precision, allocatable :: gnontor(:,:),fnontor(:,:),ftor(:,:),gtor(:,:)
double precision, allocatable :: gftor(:,:),hw(:,:)
double precision,intent(in) :: fscl
double precision :: detd
double precision,intent(in) :: gmat(nf,nf),fmat(nf,nf),mtor(ntor)
double precision :: tbh(ntor),ntfreq(nf)
double precision :: wrt(ntor),wit(ntor),vlt(ntor,ntor),vrt(ntor,ntor)
double precision :: gf(nf,nf),vb(nf,nf)
double precision :: wr(nf),wi(nf),vl(nf),vr(nf),freq(nf)

nontor = nf-ntor

if(.not.allocated(g11)) then
  allocate (g11(nontor,nontor),g22(ntor,ntor),g12(nontor,ntor),g21(ntor,nontor))
  allocate (gfnontor(nontor,nontor))
  allocate (gnontor(nontor,nontor),fnontor(nontor,nontor))
  allocate (wrn(nontor),win(nontor),vln(nontor,nontor),vrn(nontor,nontor))
  allocate (gtor(ntor,ntor),ftor(ntor,ntor),gftor(ntor,ntor),hw(ntor,ntor))
endif

! scale F matrix 
! fmat(:,:) = fmat(:,:)*fscl**2
!
! Use GF method to do t torsional frequencies
!
 g11(:,:) = gmat(1:nontor,1:nontor)
 gnontor(:,:) = g11(:,:)
 g22(:,:) = gmat(nontor+1:nf,nontor+1:nf)
 g12(:,:) = gmat(1:nontor,nontor+1:nf)
 g21(:,:) = gmat(nontor+1:nf,1:nontor)
 ftor(:,:)= fmat(nontor+1:nf,nontor+1:nf)*fscl**2
 fnontor(:,:) = fmat(1:nontor,1:nontor)*fscl**2

! Calculate (G11)-1 
 call dgetrf(nontor,nontor,g11,nontor,indx,info)
 if (info.ne.0 ) then
    write(6,*) "dgetrf exit abnormally"
    stop
 endif
 lwork = -1
 allocate(work(1))
 call dgetri(nontor,g11,nontor,indx,work,lwork,info)
 lwork=int(work(1))
 deallocate (work)
 allocate( work(lwork) )
 call dgetri(nontor,g11,nontor,indx,work,lwork,info)
 deallocate(work)
 if (info.ne.0 ) then
    write(6,*) "dgetri exit abnormally"
    stop
 endif
 gtor(:,:) = g22(:,:) - matmul(g21(:,:),matmul(g11(:,:),g12(:,:)  ) ) 
!!inverse Gtor matrix to get Kilpatric and Pitzer D matrix
 g22(:,:) = gtor(:,:)
 call dgetrf(ntor,ntor,g22,ntor,indx,info)
 if (info.ne.0 ) then
    write(6,*) "dgetrf exit abnormally"
    stop
 endif
 lwork = -1
 allocate(work(1))
 call dgetri(ntor,g22,ntor,indx,work,lwork,info)
 lwork=int(work(1))
 deallocate (work)
 allocate( work(lwork) )
 call dgetri(ntor,g22,ntor,indx,work,lwork,info)
 deallocate(work)
 if (info.ne.0 ) then
    write(6,*) "dgetri exit abnormally"
    stop
 endif
!calculate determinant of D matrix
 lwork = -1
 allocate(work(1))
  call dgeev('N','N',ntor,g22,ntor,WRT,WIT,VLT,ntor,VRT,ntor,WORK,LWORK,INFO)
 lwork=int(work(1))
 deallocate (work)
 allocate( work(lwork) )
 call dgeev('N','V',ntor,g22,ntor,WRT,WIT,VLT,ntor,VRT,ntor,WORK,LWORK,INFO)
 deallocate (work)
 if(info.ne.0) then
    write(6,*) "dgeev exited abnormally in diagonalizing D matrix"
 endif
 detd = 1d0
 do i = 1, ntor
   detd = detd*wrt(i)
 enddo
 write(6,'(a,es14.6)') 'Determinant of D matrix', detd
!
! scale F_tor matrix is scaled by M values
!
  do i = 1, ntor
  do j = 1, ntor
    ftor(i,j) = ftor(i,j)/(mtor(i)*mtor(j))  
  enddo
  enddo
  hw(:,:) = ftor(:,:)

 wrt = 0d0
 wit = 0d0
 lwork = -1
 allocate(work(1))
  call dgeev('N','N',ntor,hw,ntor,WRT,WIT,VLT,ntor,VRT,ntor,WORK,LWORK,INFO)
 lwork=int(work(1))
 deallocate (work)
 allocate( work(lwork) )
 call dgeev('V','N',ntor,hw,ntor,WRT,WIT,VLT,ntor,VRT,ntor,WORK,LWORK,INFO)
 deallocate (work)
 if(info.ne.0) then
    write(6,*) "dgeev exited abnormally in subroutine vibtor"
 endif

 call shell(ntor,wrt) !sort the frequencies
 do i = 1, ntor
   tbh(i)= 2d0*wrt(i)
   if(tbh(i) < 0d0) then
    write(6,*) 'Negative torsional barrier'
!   stop
   endif
 enddo

 write(6,*) "  Effective torsional barrier (kcal/mol)"
 jcol = 6
 jmin = 1
 do
  jmax = min(jcol,ntor)
  write(6,'(1x,6(f12.4))') ((tbh(j))*627.5096d0,j=jmin,jmax)
  jmin = jmax + 1
  jcol = jcol + 6
  if(jmax .eq. ntor) exit
 enddo
!
! test non-torsional frequencies
!gfnontor(:,:) = matmul(gnontor,fnontor)
!lwork = -1
!allocate(work(1))
!call dgeev('N','N',nontor,gfnontor,nontor,WRN,WIN,VLN,nontor,VRN,nontor,WORK,LWORK,INFO)
!lwork=int(work(1))
!deallocate (work)
!allocate( work(lwork) )
!call dgeev('N','N',nontor,gfnontor,nontor,WRN,WIN,VLN,nontor,VRN,nontor,WORK,LWORK,INFO)
!deallocate (work)
!if(info.ne.0) then
!   write(6,*) "dgeev exited abnormally in subroutine vib"
!endif
!call shell(nontor,wrn) !sort the frequencies
!do i = 1, nontor
!  ntfreq(i)= sign(1.d0,wrn(i))*sqrt(abs(wrn(i)))
!enddo

!write(6,*) "Coupled Nontorsional Freqeuncies  "
!jcol = 10
!jmin = 1
!do
! jmax = min(jcol,nontor)
! write(6,'(1x,a,10(f12.4))') 'NTF', (ntfreq(j)*219474.63d0,j=jmin,jmax)
! jmin = jmax + 1
! jcol = jcol + 10
! if(jmax .eq. nontor) exit
!enddo

!deallocate(g11,g22,g12,g21)
!deallocate(gfnontor)
!deallocate(gnontor,fnontor)
!deallocate(wrn,win,vln,vrn)
!deallocate (gtor,ftor,gftor,hw)
return
end subroutine vibtor

subroutine shell(n,arr)
!
! This is a simple variant (more sophisticated versions exist) of the Shell sorting algorithm.
! It is a convenient choice here because we are sorting limited data that is already nearly well ordered
! For large data sets another code should be chosen.
! 
implicit none
integer :: n, nn, lognb2, m, k, j, l, i
double precision, parameter :: ALN2I=1.d0/0.69314718d0,TINY=1.d-5 
double precision :: ARR(N), t

LOGNB2=INT(ALOG(FLOAT(N))*ALN2I+TINY)
m=n
do nn=1,LOGNB2
  m=m/2; k=n-m
  do j=1,k
    i=j
    10    continue
    l=i+m
    if(ARR(l).LT.ARR(i)) then
      t=ARR(i)
      ARR(i)=ARR(l)
      ARR(l)=t
      i=i-m
      if(i.GE.1) GOTO 10
    end if
  end do
end do
end subroutine shell

subroutine invlaplace(etot,W,NVIBM,FMITS,IS,NS,DEN)
use perconparam, only : bk,tpi,ckcal,autocm
use sst
!
! This subroutine is to use inverse Laplace transform by the steepest-descents method 
! to calculate number of states and density of state for a given energy E0.
! Note etot is relative to ZPE (ZPE is set to 0).
!
implicit none
integer,intent(in) :: nvibm,is
integer :: ntp,niter,i
real(8), intent(in) :: etot,fmits,W(NVIBM)
real(8), intent(out) :: ns, den
real(8) :: zpe,beta,cv,fac,tol,thresh,dt,e1
real(8) :: tp(5),therm_e(5),qrvtc(5)
real(8), parameter :: tinyfreq=4.556335E-07 ! 0.1 cm-1 in au

ns = 0d0; den=0d0
dt = 0.5d0 ! delta T for calculating heat capacity numerically
ntp = 5    ! number of temperature
tol = 1d-4

! If etot is lower than the lowest frequency
! then the density and sum of states are only from overall rotation
!  
! 1. Search the lowest first excited vibrational state
 e1 = 10.d0
 zpe = 0d0
 do i = 1, nvibm
  if(e1.gt.w(i).and.w(i).gt.tinyfreq) e1 = w(i)
  zpe = zpe + w(i)
 enddo
 zpe = zpe/2d0

! If etot < the lowest frequency then
 if(etot < e1) then
!  Note rotational symmetry is not considered here
   fac = dsqrt(2d0*fmits*etot)
   ns  = 8d0*fac*etot/3d0
   den = 4d0*fac
   return
 endif

!begin inverse Laplace transform using steep-descrent method

! initial guess beta and temperature
tp(1) = 1d4
tp(2) = tp(1) - dt
tp(3) = tp(1) + dt
tp(4) = tp(1) - dt*2d0
tp(5) = tp(1) + dt*2d0
beta = 1d0/(tp(1)*bk)
niter = 0
thresh = 10d0 ! initialize threshold 
!
! Newton's method to optimize the beta (or T) at which thermal energy is 
! equal to etot
!
 do while (thresh .gt. tol)

  call pftor(fmits,w,nvibm,detds(is),torbh(:,is),dbw(:,is),dmtor,ntor,tp,ntp,&
             dt,zpe,cv,therm_e,qrvtc)
!Thermal energy is relative to ZPE 
  therm_e(1) = therm_e(1) -zpe
! using Newton method to find next beta
  beta = beta - (etot-therm_e(1))/(bk*tp(1)**2*cv)
  tp(1) = 1d0/(bk*beta)
  tp(2) = tp(1) - dt
  tp(3) = tp(1) + dt
  tp(4) = tp(1) - dt*2d0
  tp(5) = tp(1) + dt*2d0
  thresh = abs(therm_e(1)-etot)*ckcal
  niter = niter + 1
 enddo
! note that the zero of energy of qrvtc is the ZPE
 den = qrvtc(1)*exp(beta*(etot))/(tp(1)*dsqrt(bk*tpi*cv))
 ns = den/beta
return
end subroutine invlaplace

subroutine pftor(ppmi,freq,nvibm,detd,tbh,dbw,mtor,ntor,temp,ntemp,dt,zpe,cv1,e,qrvtc)
! 
! subroutine is for calculating ro-vibrational partition function with torsional
! anharmonicity based on a coupled torsional potential
!
use perconparam, only : bk,tpi,n3tm,autocm
implicit none
integer, intent(in) :: nvibm,ntor,ntemp
integer :: it,itor,i,j,nt_1_5,nt_2_5,nt_3_5,nt_4_5
real(8), intent(in) :: ppmi,freq(nvibm),detd,tbh(ntor),dbw(n3tm),mtor(ntor),temp(ntemp),dt,zpe
real(8), intent(out):: e(ntemp),qrvtc(ntemp),cv1
real(8) :: qho(ntemp),qrot(ntemp),ftor(ntemp),cv(ntemp),t1,t2,omega,beta,bkt,bes0,bes1
real(8), parameter :: tinyfreq=4.556335E-07 ! 0.1 cm-1 in au

!initialization
e     = 0d0
qho   = 0d0
qrvtc = 0d0
ftor  = 1d0
cv    = 0d0

! calculate Harmonic partition function Q^HO
 call pfho(1,nvibm,temp(1),freq,zpe,qho(1))

! calculate ro-vibrational partition function 

   it = 1
   bkt = bk*temp(it)
   qrot(it) = 2.d0*dsqrt(tpi*ppmi)*(bkt**1.5d0)
   qrvtc(it) = qrot(it)*qho(it) 
   do itor = 1, ntor
      t1 = 0.5d0*tbh(itor)/bkt
      ftor(it) = ftor(it)*dsqrt(tpi/bkt)/mtor(itor)*exp(-t1)*bes0(t1)
   enddo  ! end itor
   ftor(it)=ftor(it)*dsqrt(detd)
   do j = 1, nvibm
     if(freq(j).gt.tinyfreq) ftor(it)=ftor(it)*freq(j)
   enddo
   do j = 1, n3tm
      if(dbw(j).gt.tinyfreq) then
         ftor(it)=ftor(it)/dbw(j)
      endif
   enddo
   qrvtc(it) = qrvtc(it)*ftor(it)

! Thermodynamic functions
do it = 1, ntemp
   bkt = bk*temp(it)
   beta = 1d0/bkt
   e(it) = 1.5d0*bkt
   t1 = 0d0
   do j = 1,nvibm
      if(freq(j).gt.tinyfreq) then        
        t2 = exp(-freq(j)*beta)
        t1 = t1 + 0.5d0*freq(j)*(1.0d0+t2)/(1.0d0-t2)
      endif
    enddo
    e(it) = e(it) + t1

    t1 = 0.d0
    do itor =1 ,ntor
       t2 = tbh(itor)/2d0
       t1 = t1 + t2*(1d0-bes1(beta*t2)/bes0(beta*t2))-0.5d0/beta
    enddo
    e(it) = e(it) + t1
enddo ! end it
!
! calculate constant volumn heat capacity using finite difference
!
nt_1_5=ntemp/5
nt_2_5=2*nt_1_5
nt_3_5=3*nt_1_5
nt_4_5=4*nt_1_5
do it = 1, nt_1_5
   cv(it)  = (e(nt_3_5+it) - 8d0*e(nt_1_5+it) + &
             8d0*e(nt_2_5+it) - e(nt_4_5+it))/(12.d0*dt)
enddo
cv1 = cv(1)

return
end subroutine pftor


subroutine pfho(ntemp,nf,temp,freq,zpe,qho)
!
! subroutine to calculate harmonic oscillator partition function
! The logrithm form is used to prevent overflow/underflow.
! ZPE is used for zero of energy so that Q^HO approach to 1 at low-T limit
!
use perconparam, only : bk
implicit none
integer, intent(in) :: ntemp, nf
integer ::  it, m
real(8), intent(in) :: temp(ntemp),freq(nf),zpe
real(8), intent(out) :: qho(ntemp)
real(8) :: bkt,omega,logho(ntemp)
real(8), parameter :: tinyfreq=4.556335E-07  ! 0.1 cm-1 in au

do it = 1, ntemp
    bkt = bk*temp(it)
    logho(it) = zpe/bkt
    do m = 1, nf
       omega = freq(m)
       if(omega.gt.tinyfreq) then
        logho(it) = logho(it) -omega/bkt/2d0 - log(1d0-exp(-omega/bkt))
       endif
    enddo
    qho(it) = exp(logho(it))
enddo
return
end subroutine pfho



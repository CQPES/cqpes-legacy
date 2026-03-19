!***************************************************************************
!                         FINN PES for OH+CH3OH
!***************************************************************************
!-->  program to get potential energy for a given geometry after NN fitting
!-->  global variables are declared in this module
!-->  Written by Kaisheng Song at 10 December 2022
module nnparam
    implicit none
    !***************************************************************************
    !natom     ==>Number of atoms
    !nfi    ==>Number of morse-like potential
    !***************************************************************************
    real*8, parameter::alpha = 1.d0
    integer, parameter::natom = 5, npip = 82
    integer, parameter::nbond = natom*(natom - 1)/2
    integer ninput, noutput, nscale
    integer nhid3, nlayer3, ifunc3
    integer nwe3, nodemax3
    integer, allocatable::nodes3a(:)
    real*8, allocatable::weight3a(:, :, :), bias3a(:, :), pdel3a(:), pavg3a(:)
end module nnparam
!***************************************************************************
subroutine evvdvdx(xcart,v)
    !***************************************************************************
    !Subroutine to calculate the average potential energy v and analytical gradient dvdxa
    !Call pes_init to read files and initialize before calling evvdvdx().
    !v         ==>potential energy(in eV)
    !***************************************************************************
    use nnparam
    use basis, only: bemsav
    implicit none
    integer i, j, k, ndriv
    real*8 v, vpes, dvdx(1:natom*3)
    real*8 xcart(1:3, 1:natom), xvec(3, nbond), xbond(1:nbond), rij(1:nbond)
    real*8 txinput(1:ninput), pip(1:ninput+1)
    ! :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    !> calculate distance vector
    k = 1
    do i = 1, natom - 1
        do j = i + 1, natom
            xvec(:, k) = xcart(:, i) - xcart(:, j)
            rij(k) = dsqrt(dot_product(xvec(:, k), xvec(:, k)))
            k = k + 1
        end do
    end do

    !> Morse-like
    xbond(:) = dexp(-rij(:)/alpha)

    !> generate pip
    call bemsav(xbond, pip)
    txinput(1:ninput) = pip(2:ninput+1)

    !调用PES生成vpes,dedp
    call pot3a(txinput, v)

    return
end subroutine evvdvdx
!****************************************************************!
!-->  read NN weights and biases from matlab output
subroutine pes_init
    use nnparam
    implicit none
    integer i, j, ihid, iwe, inode1, inode2, ilay1, ilay2
    integer ibasis, npd, iterm, ib, nfile1, nfile2
    real*8, allocatable::tpw1(:, :), tpw2(:, :), nxw1(:, :), nxw2(:, :)
    character f1*80

    nfile1 = 4
    nfile2 = 7
    open (nfile1, file='weights.txt', status='old')
    open (nfile2, file='biases.txt', status='old')

    ! read (nfile1, *)
    ! read (nfile2, *)
    read (nfile1, *) ninput, nhid3, noutput
    nscale = ninput + noutput
    nlayer3 = nhid3 + 2 !additional one for input layer and one for output
    allocate (nodes3a(nlayer3), pdel3a(nscale), pavg3a(nscale))
    nodes3a(1) = ninput
    nodes3a(nlayer3) = noutput
    read (nfile1, *) (nodes3a(ihid), ihid=2, nhid3 + 1)
    nodemax3 = 0
    do i = 1, nlayer3
        nodemax3 = max(nodemax3, nodes3a(i))
    end do
    allocate (weight3a(nodemax3, nodemax3, 2:nlayer3), bias3a(nodemax3, 2:nlayer3), tpw1(ninput, nodes3a(2)), &
              tpw2(nodes3a(2), nodes3a(3)), nxw1(nodes3a(2), ninput), nxw2(nodes3a(3), nodes3a(2)))
    weight3a = 0.d0
    bias3a = 0.d0
    read (nfile1, *) ifunc3, nwe3

    read (nfile1, *) (pdel3a(i), i=1, nscale)
    read (nfile1, *) (pavg3a(i), i=1, nscale)
    iwe = 0
    do ilay1 = 2, nlayer3
        ilay2 = ilay1 - 1
        do inode1 = 1, nodes3a(ilay1)
        do inode2 = 1, nodes3a(ilay2) !
            read (nfile1, *) weight3a(inode2, inode1, ilay1)
            iwe = iwe + 1
        end do
        read (nfile2, *) bias3a(inode1, ilay1)
        iwe = iwe + 1
        end do
    end do
    if (iwe .ne. nwe3) then
        write (*, *) 'provided number of parameters ', nwe3
        write (*, *) 'actual number of parameters ', iwe
        write (*, *) 'nwe not equal to iwe, check input files or code'
        stop
    end if
    tpw1(:, :) = weight3a(1:ninput, 1:nodes3a(2), 2)
    tpw2(:, :) = weight3a(1:nodes3a(2), 1:nodes3a(3), 3)
    nxw1 = transpose(tpw1)
    nxw2 = transpose(tpw2)
    weight3a(1:nodes3a(2), 1:ninput, 2) = nxw1(:, :)
    weight3a(1:nodes3a(3), 1:nodes3a(2), 3) = nxw2(:, :)

    close (nfile1)
    close (nfile2)
    write (*, *) 'initialization done'

end subroutine pes_init
!*************************************************************************
subroutine pot3a(x,vpot3)
    use nnparam
    implicit none
    integer i, j, k, m, neu1, neu2, neu3, ndriv
    real*8 x(ninput), vpot3
    real*8 w1(1:nodes3a(2), 1:ninput)
    real*8 w2(1:nodes3a(3), 1:nodes3a(2))
    real*8 w3(1, 1:nodes3a(3))
    real*8 y0(1:ninput, 1), y1t(1:nodes3a(2), 1), y1(1:nodes3a(2), 1)
    real*8 y2t(1:nodes3a(3), 1), y2(1:nodes3a(3), 1)
    real*8 df2(1:nodes3a(3)), diagdf2(1:nodes3a(3), 1:nodes3a(3))
    real*8 df1(1:nodes3a(2)), diagdf1(1:nodes3a(2), 1:nodes3a(2))
    real*8 y3, y3t(1, 1)
    real*8 dvdy2(1, 1:nodes3a(3))
    real*8 dy2dy1(1:nodes3a(3), 1:nodes3a(2))
    real*8 dvdy1(1, 1:nodes3a(2))
    real*8 dy1dy0(1:nodes3a(2), 1:ninput), dvdy0(1, 1:ninput)
    real*8 dvdp(1:ninput)
    real*8 dvdg(1, 1:ninput)
    real*8 ALPH, BETA
    real*8, external::tranfun

    dvdg = 0.d0
    ALPH = 1.D0
    BETA = 0.D0

    !将输入层归一化为y0
    y0(:, 1) = (x(:) - pavg3a(1:ninput))/pdel3a(1:ninput)

    neu1 = nodes3a(2); neu2 = nodes3a(3); neu3 = nodes3a(4)

    w1(:, :) = weight3a(1:neu1, 1:ninput, 2)
    w2(:, :) = weight3a(1:neu2, 1:neu1, 3)
    w3(1, :) = weight3a(1:neu2, 1, 4)

    !-->.....evaluate the hidden layer
    call DGEMM('N', 'N', neu1, 1, ninput, ALPH, w1, neu1, y0, ninput, BETA, y1t, neu1)
    y1(:, 1) = dtanh(y1t(:, 1) + bias3a(1:neu1, 2))

    call DGEMM('N', 'N', neu2, 1, neu1, ALPH, w2, neu2, y1, neu1, BETA, y2t, neu2)
    y2(:, 1) = dtanh(y2t(:, 1) + bias3a(1:neu2, 3))

    call DGEMM('N', 'N', 1, 1, neu2, ALPH, w3, 1, y2, neu2, BETA, y3t, 1)
    y3 = y3t(1, 1)
    !求出势能面返回的能量，并逆转换为归一化前的原始数据
    vpot3 = (y3 + bias3a(1, 4))*pdel3a(nscale) + pavg3a(nscale)

    if (ndriv .eq. 1) then!反向传播求能量对输入层的偏导 dvdg

        df2 = 1 - y2(:, 1)*y2(:, 1) !求dtanh的导数=1-dtanh**2
        diagdf2 = 0.d0
        forall (i=1:neu2) diagdf2(i, i) = df2(i) !向量转为对角矩阵

        !求出dvdy2 能量对第二层隐藏层的偏导 dy2dy1=w2*diagdf2
        call DGEMM('N', 'N', neu2, neu1, neu2, ALPH, diagdf2, neu2, w2, neu2, BETA, dy2dy1, neu2)

        df1 = 1 - y1(:, 1)*y1(:, 1) !求dtanh的导数=1-dtanh**2
        diagdf1 = 0.d0

        forall (i=1:neu1) diagdf1(i, i) = df1(i) !向量转为对角矩阵

        !求第二层隐藏层对第一层隐藏层的偏导 dy1dy0=w1*diagdf1
        call DGEMM('N', 'N', neu1, ninput, neu1, ALPH, diagdf1, neu1, w1, neu1, BETA, dy1dy0, neu1)

        !求出能量对第一层隐藏层的偏导 dvdy1=dy3dy2*dy2dy1
        call DGEMM('N', 'N', 1, neu1, neu2, ALPH, w3, 1, dy2dy1, neu2, BETA, dvdy1, 1)

        !求出能量对输入层的偏导dvdy0=dvdy1*dy1dy0  dy1dy0=w1为第一层隐藏层对输入层的偏导
        call DGEMM('N', 'N', 1, ninput, neu1, ALPH, dvdy1, 1, dy1dy0, neu1, BETA, dvdy0, 1)

        !将dvdy0逆转换为归一化前的原始数据
        dvdg(1, :) = dvdy0(1, 1:ninput)*pdel3a(nscale)/pdel3a(1:ninput)

    end if
    !enddo

    return
end subroutine pot3a
!**************************************************************************
function tranfun(x, ifunc)
    implicit none
    integer ifunc
    real*8 tranfun, x
    !c    ifunc=1, transfer function is hyperbolic tangent function, 'tansig'
    !c    ifunc=2, transfer function is log sigmoid function, 'logsig'
    !c    ifunc=3, transfer function is pure linear function, 'purelin'. It is imposed to the output layer by default
    if (ifunc .eq. 1) then
        tranfun = dtanh(x)
    else if (ifunc .eq. 2) then
        tranfun = 1d0/(1d0 + exp(-x))
    else if (ifunc .eq. 3) then
        tranfun = x
    end if
    return
end function tranfun
!**************************************************************************
FUNCTION LOGSIG(X)
    REAL*8 X, LOGSIG
    LOGSIG = 1.d0/(1.d0 + DEXP(-X))
    RETURN
END FUNCTION LOGSIG
!**************************************************************************
subroutine evdvdx(dvdg, dpdr, drdx, dvdx)
    use nnparam
    implicit none
    integer i, j, k
    real*8 ALPH, BETA
    real*8 dvdx(1:natom*3), xcart(1:3, 1:natom)
    real*8 drdx(1:nbond, 1:natom*3), x(1:ninput)
    real*8 dEdx(1, 1:natom*3), dpdr(1:ninput, 1:nbond), dvdp(1, 1:ninput)
    real*8 dpdx(1:ninput, 1:natom*3)
    real*8 dvdxa(1:natom*3), dvdg(1, 1:ninput)

    ALPH = 1.D0
    BETA = 0.D0
    dpdx = 0d0

    !求出输入层对坐标xyz的偏导dpdx=dpdr*drdx
    call DGEMM('N', 'N', ninput, natom*3, nbond, ALPH, dpdr, ninput, drdx, nbond, BETA, dpdx, ninput)

    dvdxa = 0.d0
    dEdx = 0.d0

    dvdp(1, 1:ninput) = dvdg(1, 1:ninput)

    !求出能量对坐标xyz的偏导数dEdx=dvdp*dpdx
    call DGEMM('N', 'N', 1, natom*3, ninput, ALPH, dvdp, 1, dpdx, ninput, BETA, dEdx, 1)

    dvdx(:) = dEdx(1, :)!取负值

    return
end subroutine evdvdx

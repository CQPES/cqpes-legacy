subroutine pot0(i3n, V)
    use nnparam
    implicit real(kind=8) (a-h, o-z)
    integer, parameter :: nd1 = 100
    integer, parameter :: ndp = 10
    real(kind=8), parameter :: c1 = 0.04184000d0
    real(kind=8), parameter :: eV2kcal = 23.060541945329334d0
    !> Q : coords in Angstrom
    !> PDOT: gradient
    common /qpdot/ q(3 * nd1), pdot(3 * nd1)
    !> xc, xct: coordinates
    real(kind=8) :: xc(3, 5)
    !> V: potential energy
    real(kind=8) :: V

    !> order: C1 H2 H3 H4 H5
    !> if the atoms have to be reordered, do it here
    xc(:, 1) = (/q( 1), q( 2), q( 3)/) ! C1
    xc(:, 2) = (/q( 4), q( 5), q( 6)/) ! H2
    xc(:, 3) = (/q( 7), q( 8), q( 9)/) ! H3
    xc(:, 4) = (/q(10), q(11), q(12)/) ! H4
    xc(:, 5) = (/q(13), q(14), q(15)/) ! H5

    call evvdvdx(xc, V)

    !> eV -> kcal/mol
    V = V * eV2kcal
    !> kcal/mol to integration unit
    V = V * c1

    return
end subroutine pot0

subroutine dpeshon(i3n)
    implicit real(kind=8) (a-h, o-z)
    integer, parameter :: nd1 = 100
    integer, parameter :: ndp = 10
    !> Q : coords in Angstrom
    !> PDOT: gradient
    common /qpdot/ q(3 * nd1), pdot(3 * nd1)
    !> stepsize
    real(kind=8), parameter :: hinc = 0.001d0 ! Angstrom
    real(kind=8) :: V1, V2
    integer :: i

    do i = 1, i3n
        !> backward
        q(i) = q(i) - hinc
        call pot0(i3n, V1)
        q(i) = q(i) + hinc

        !> forward
        q(i) = q(i) + hinc
        call pot0(i3n, V2)
        q(i) = q(i) - hinc

        !> diff
        pdot(i) = (V2 - V1) / (2.0d0 * hinc)
    end do

    return
end subroutine dpeshon

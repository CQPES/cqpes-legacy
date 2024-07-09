
!***********************************************************************************************************************
!     interface for polyrate
subroutine surf(v, x, dvdx, n3tm)
    ! v: output, real*8, potential energy at current coordinate
    ! x: input, real*8, dimension: n3tm, coordinates of each atom flatten to 1d array
    ! dvdx: output, real*8, dimension: n3tm, gradient, same shape as `x`
    ! n3tm: input, integer, number of coordinates variables, 3 * number of atoms
    implicit none
    integer, intent(in) :: n3tm
    real(kind = 8), intent(in) :: x(n3tm)
    real(kind = 8), intent(out) :: v, dvdx(n3tm)

    integer :: i
    real(kind = 8) :: delta, ct(3, int(n3tm / 3)), v1, v2, xx(n3tm)

    !*******************************************************************************************************************
    ! calculate potential energy `v` using pes
    ! note that pipNN requires coordinates in **Angstrom** and output potential energy in **eV**
    ! polyrate will pass coordinates in **Bohr** and receive potential energy in **Hartree** (both in a.u.)
    xx(:) = x(:)
    call pesinterface(xx, n3tm, v)
    !*******************************************************************************************************************
    ! calculate gradient at each `x(i)`
    delta = 5.0d-03 ! Bohr
    do i = 1, n3tm
        ! finite-difference method
        ! xx(i) + delta
        xx(i) = xx(i) + delta
        call pesinterface(xx, n3tm, v1)
        xx(i) = xx(i) - delta

        ! xx(i) - delta
        xx(i) = xx(i) - delta
        call pesinterface(xx, n3tm, v2)
        xx(i) = xx(i) + delta

        ! d(v)/d(x) = (v(x + delta) - v(x - delta)) / (2.0 * delta)
        dvdx(i) = (v1 - v2) / (2.0d0 * delta)
    end do
end subroutine surf

subroutine pesinterface(x, n3tm, vpes)
    use nnparam
    implicit none
    integer, intent(in) :: n3tm
    real(kind = 8), intent(in) :: x(n3tm)
    real(kind = 8), intent(out) :: vpes

    integer :: i
    real(kind = 8) :: ct(3, int(n3tm / 3))
    !*******************************************************************************************************************
    ! coordinate converting section
    ! convert `x` to matrix with shape(3, natom)
    ! dat file order:
    ! 1 C  -> x(1: 3)
    ! 2 H  -> x(4: 6)
    ! 3 H  -> x(7: 9)
    ! 4 H  -> x(10: 12)
    ! 5 H  -> x(13: 15)
    ! as atom order in dat file is defined the same as in pes-nn, no need to change the order
    !*******************************************************************************************************************
    do i = 1, int(n3tm / 3)
        ct(:, i) = (/x(i * 3 - 2), x(i * 3 - 1), x(i * 3)/)
    end do
    !*******************************************************************************************************************
    ! calculate potential energy `vpes`
    ! polyrate will pass the coordinates in unit **Bohr**, and receive the potential energy in unit **Hartree**
    ! ct: Bohr -> Angstrom
    ct(:, :) = ct(:, :) * 0.5291772d0
    ! call pes-nn, here `ct` is passed to `pipNN()` and `vpes` is used to receive pred val
    ! `ct` is in the unit Angstrom and `vpes` is in eV
    call evvdvdx(ct, vpes)
    ! vpes: eV -> Hartree
    vpes = vpes / 27.2114d0
    !*******************************************************************************************************************
    return
end subroutine pesinterface

subroutine setup(n3tm)
    use nnparam
    implicit none
    integer, intent(in) :: n3tm

    integer, parameter :: n3tmmn = 3 * natom ! 7 atoms, (x, y, z)

    if (n3tm < n3tmmn) then
        write (6, *) 'WARNING: N3TM is set equal to ', n3tm
        write (6, *) 'but this potential routine requires N3TM be greater than or equal to ', n3tmmn
        stop "SETUP 1"
    endif

    write (6, *) 'read file'
    call pes_init()
    write (*, *) 'init done'
    write (6, *) 'read done'
    return
end subroutine setup
! end of interface
!***********************************************************************************************************************
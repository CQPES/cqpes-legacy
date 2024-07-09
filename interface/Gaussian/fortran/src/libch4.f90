!> wrapper for `pes_init`
subroutine init()
    use nnparam
    implicit none

    call pes_init()
end subroutine init

!> wrapper for potential energy calculation
subroutine calc_energy(coords, natom, vpes)
    implicit none

    !> number of atoms
    integer, intent(in) :: natom
    !> coordinates in Angstroms
    real(kind=8), intent(in) :: coords(natom, 3)
    !> potential energy in Hartree
    real(kind=8), intent(out) :: vpes

    !> constants, Hartree in eV
    real(kind=8), parameter :: Eh2eV = 27.211399d0

    !> pes min, in Hartree
    real(kind=8), parameter :: vpes_min = -40.450888013207d0

    call evvdvdx(transpose(coords), vpes)
    vpes = vpes / Eh2eV
    vpes = vpes + vpes_min
end subroutine calc_energy

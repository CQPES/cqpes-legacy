program main
    use nnparam
    implicit none

    !> command line arguments
    character(len=80) :: xyz_file
    integer :: argc

    !> file io
    integer :: xyz_unit
    integer :: stat

    !> xyz
    integer :: num_atoms
    character(len=80) :: title
    character(len=10), allocatable :: symbol(:)
    real(kind=8), allocatable :: coords(:, :)

    !> potential energy
    real(kind=8) :: V

    !> loop
    integer :: i

    !> parse arguments
    argc = iargc()

    if (argc .ne. 1) then
        print *, 'Usage:'
        print *, '    ./test.x CH4.xyz'
        stop
    end if

    !> pes_init
    call pes_init()

    call getarg(1, xyz_file)

    !> read xyz file
    xyz_unit = 10
    open (unit=xyz_unit, file=trim(xyz_file), status="old")

    !> read contents
    do
        !> number of atoms
        read (unit=xyz_unit, fmt=*, iostat=stat) num_atoms

        !> title
        read (unit=xyz_unit, fmt="(A)", iostat=stat) title

        !> cartesian coordinates
        allocate (symbol(num_atoms))
        allocate (coords(3, num_atoms))

        do i = 1, num_atoms
            read (unit=xyz_unit, fmt=*, iostat=stat) symbol(i), coords(:, i)
        end do

        !> EOF?
        if (stat .ne. 0) then
            exit
        end if

        ! call print_xyz(num_atoms, title, symbol, coords)
        call evvdvdx(coords, V)

        print *, V

        !> deallocate
        deallocate (symbol)
        deallocate (coords)
    end do

    !> close xyz file
    close (xyz_unit)

end program main

subroutine print_xyz(num_atoms, title, symbol, coords)
    implicit none

    integer, intent(in) :: num_atoms
    character(len=*), intent(in) :: title
    character(len=*), intent(in) :: symbol(num_atoms)
    real(kind=8), intent(in) :: coords(3, num_atoms)

    integer :: i

    print *, num_atoms
    print *, title

    do i = 1, num_atoms
        print *, symbol(i), coords(:, i)
    end do
end subroutine print_xyz

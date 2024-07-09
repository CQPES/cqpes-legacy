
        subroutine opened_file2table(num_unit,ndim,table)
        character(80), pointer :: table(:)
        character(80) :: a
        integer, intent(out) :: ndim
        rewind (num_unit)
        n=0
        do
          read(num_unit,'(a80)',iostat=io)a
          n=n+1
          if(io/=0)exit
        end do
        if(associated(table))nullify(table)
        allocate(table(n)); table(:)(:)=' '
        rewind(num_unit)
        ndim=n
        do i=1,ndim
          read(num_unit,'(a80)',iostat=io)a
          table(i)=a
        end do
        end subroutine opened_file2table

        subroutine closed_file2table(filename,ndim,table)
        character(80), pointer :: table(:)
        character(80) :: a
        character(*), intent(in) :: filename
        num_unit=151
        open(unit=num_unit,file=filename,status='old')
        rewind (num_unit)
        n=0
        do
          read(num_unit,'(a80)',iostat=io)a
          if(io/=0)exit
          n=n+1
        end do
        if(associated(table))nullify(table)
        allocate(table(n)); table(:)(:)=' '
        rewind(num_unit)
        ndim=n
        do i=1,ndim
          read(num_unit,'(a80)',iostat=io)a
          table(i)=a
        end do
        close(unit=num_unit,status='keep')
        end subroutine closed_file2table

      subroutine low2up(string)
      use upplow
      character(*) :: string
      ll=len(string)
      do i=1,ll
       do j=1,26
        if(ichar(string(i:i)).eq.ichar(lower(j)))then
          string(i:i)=upper(j)
          exit   
        end if
       end do
      end do
      end subroutine low2up
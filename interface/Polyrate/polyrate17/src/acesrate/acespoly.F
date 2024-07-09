c ***************************************************************************
c     aceset
c ***************************************************************************
c
      subroutine aceset(jtype)
c
c     This subroutine is called by prepj.  It prepares species dependent
c     data needed by the the ACES II package.  This version is a dummy
c     routine included in the POLYRATE distribution.  To use the ACES II
c     POLYRATE interface one needs the package ACESRATE.
c
c
      write(6,*)'ERROR - aceset called.  ACESRATE needed to use'
      write(6,*)'        the POLYRATE/ACES II interface'
c
      stop
      end
c ***************************************************************************
c     rbasis
c ***************************************************************************
c
c
      subroutine rbasis
c
      write(6,*)'BASIS keyword is not valid with this version of' 
      write(6,*)'POLYRATE'
      stop
      end
c ************************************************************************
c     acalc
c ************************************************************************
c
      subroutine acalc(ntyp,fmat)
c
c     Subroutine to interface with the ACES II package.  This version
c     is just a dummy routine.  To interface with ACES II you need
c     the ACESRATE package.
c
      write(6,*) 'Error - Routine acalc called'
      write(6,*) '        POLYRATE does not support the ACES interface'
      write(6,*) '        ACESRATE is necessary'
c
      stop
      end
c ***************************************************************************
c     gseta
c ***************************************************************************
c
c
      subroutine gseta
c
c     Subroutine to interface with the ACES II package.  This version
c     is just a dummy routine.  To interface with ACES II you need
c     the ACESRATE package.
c
      write(6,*) 'Error - Routine gseta called'
      write(6,*) '        POLYRATE does not support the ACES interface'
      write(6,*) '        ACESRATE is necessary'
c
      stop
      end

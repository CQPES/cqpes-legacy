c***********************************************************************
c     dattim
c***********************************************************************
c
      subroutine dattim (io)
c
c     write date and time to the device io using a call to date_and_time.
c     Titus V. Albu (May 16, 2002)
c
      implicit double precision (a-h, o-z)
c 
      character * 11  datepr
      character *  8  timepr
      character *  8  date
      character * 10  time
      character *  5  zone
      character *  3  month
      character *  3  months(12)
      integer istuff (8)
c
      data months /'Jan','Feb','Mar','Apr','May','Jun',
     *             'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      call date_and_time(date,time,zone,istuff)
c
      if (istuff(2).gt.0.and.istuff(2).le.12) then
         month = months(istuff(2))
      else
         imod   = ichar(date(5:5))-48
         imou   = ichar(date(6:6))-48
         imonth = imod*10 + imou
         if (imonth.gt.0.and.imonth.le.12) then
            month = months(imonth)
         else
            month = '   '
         endif
      endif
c
      if (month.ne.'   '.and.date(1:4).ne.'    '
     *                  .and.date(7:8).ne.'  ') then
         datepr(1:2) = date(7:8)
         datepr(3:3) = '-'
         datepr(4:6) = month(1:3)
         datepr(7:7) = '-'
         datepr(8:11)= date(1:4)
         if (datepr(1:1).eq.'0') datepr(1:1) = ' '
      else
         datepr = ' '
      endif
c
      if (time(1:2).ne.'  '.and.time(3:4).ne.'  '
     *                     .and.time(5:6).ne.'  ') then
         timepr(1:2) = time(1:2)
         timepr(3:3) = ':'
         timepr(4:5) = time(3:4)
         timepr(6:6) = ':'
         timepr(7:8) = time(5:6)
      else
         timepr = ' '
      endif
c
      write(io,1000) datepr,timepr
 1000 format(3x,a11,55x,a8)
c
      return
      end

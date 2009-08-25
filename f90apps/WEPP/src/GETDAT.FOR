c
c
      subroutine getdat(un,date,must,name)
c
c     parameter:
c        un - the file unit
c        date - date to read
c        mustset - if 0 `date' can be 0, if 1 `date' must be > 0
c
      character*12 name
      integer un, date, must
c
c     local:
c        min - min is set to 0 or 1 depending on `must'
c
      integer min
c
      if (must.eq.0) then
        min = 0
      else
        min = 1
      end if
c
      call readin(un,date,min,366,name)
c     print*,'DATE: ',date
c
      return
      end

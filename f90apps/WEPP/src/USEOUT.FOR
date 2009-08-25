      integer function useout(mesg)
      character*(*) mesg
c
      character*1 ans
c
      write (6,1100) mesg
      read (5,1000,err=10) ans
c
      if (ans.eq.'y'.or.ans.eq.'Y') then
        useout = 1
        return
      end if
c
   10 useout = 0
      if (ans.ne.'n'.and.ans.ne.'N') write (6,1200) mesg
c
      return
c
 1000 format (a1)
 1100 format (/,' Do you want ',a,' output (Y/N)? [N] --> ')
 1200 format (' *** NOTE ***',/,' N assumed for ',a,' output',/,
     1       ' *** NOTE ***',/)
c
      end
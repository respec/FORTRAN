      subroutine verchk(un,verson,check,mesg,ver)
c
c     This SR compares CHECK and VERSION to determine
c     if the datafile is compatible with the read formats.
c
c      parameter:
c         un - the file unit
c         version - version as read
c         check - version to check against
c         mesg - message to show if bad
c
      integer un
      real verson,check, ver
      character*21 mesg
c
c
c     read the version from the file.
c
      read (un,*) verson
c     print *, 'data file version= ', version
c     print *, 'check file version= ', check
c
c     check against the acceptable version.
c
      if (verson.lt.check) then
        write (6,*) ' *** ERROR ***'
        write (6,1000) mesg, ver
c       print *, 'data file version= ', version
c       print *, 'check file version= ', check
        stop
      end if
c
c
      return
 1000 format (1x,a21,' FILE IS NOT COMPATIBLE WITH WEPP VERSION ',
     1        f7.3,'.')
      end

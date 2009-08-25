      subroutine strip(oldnam,newnam)
c
c     + + + PURPOSE + + +
c
c     SR STRIP strips all characters except the 8 character prefix to a
c     file name. Used for the creation of the initial condition name in
c     the initial condition file.
c
c     Called from CONTIN.
c     Author(s): Whittemore
c     Reference in User Guide:
c
c     Version: This module not yet recoded.
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      character*51 oldnam
      character*8 newnam
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     newnam - name to be passed back to contin to be used as scenario
c              name in initial condition file.
c     oldnam - name read in from screen or shell file
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      integer spos, filpos, tpos
c
c     + + + LOCAL DEFINITIONS + + +
c
c     spos   - position counter
c     filpos - position in newnam for character from oldnam
c     tpos   - position counter
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c
      newnam = '        '
c
      if (len(oldnam).le.8) then
        newnam = oldnam(1:8)
        return
      end if
c
      filpos = 8
c
      do 10 spos = len(oldnam), 1, -1
c
        if (oldnam(spos:spos).eq.'.') then
          tpos = spos - 1
c
          do while (oldnam(tpos:tpos).ne.'/'.and.oldnam(tpos:tpos).ne.
     1        '\\')
            newnam(filpos:filpos) = oldnam(tpos:tpos)
            filpos = filpos - 1
            tpos = tpos - 1
            if (tpos.lt.1.or.filpos.lt.1) return
          end do
c
          return
c
        else if (oldnam(spos:spos).eq.'/'.or.oldnam(spos:spos).eq.'\\')
     1      then
          tpos = spos + 1
c
          do while (oldnam(tpos:tpos).ne.'/'.and.oldnam(tpos:tpos).ne.
     1        '\\')
            newnam(filpos:filpos) = oldnam(tpos:tpos)
            filpos = filpos - 1
            tpos = tpos - 1
            if (tpos.lt.1.or.filpos.lt.1) return
          end do
c
          return
c
        end if
   10 continue
c
      newnam = oldnam(len(oldnam)-8:len(oldnam))
      if (newnam.eq.'        ') newnam = oldnam(1:(len(oldnam)))
c
      return
      end

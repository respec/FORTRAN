c
      subroutine newtil(tilseq,ntill,iplane,oldind)
c
c     on the first day of simulation and at the end of a tilage day
c     the next tilage date is found
      integer tilseq, ntill,iplane,oldind
      include 'pmxpln.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
c
      include 'cupdate.inc'
c
c  oldind - old tilage index
c  newind - new tilage index
c
      integer  newind,indtil
c
      oldind = indxy(iplane)
      newind = 0
      do 10 indtil = 1, ntill
        if (oldind.eq.0) then
          if (newind.eq.0) then
            if (mdate(indtil,tilseq).gt.sdate) newind = indtil
          else if (mdate(indtil,tilseq).lt.mdate(newind,tilseq).and.
     1        mdate(indtil,tilseq).gt.sdate) then
            newind = indtil
          end if
        else
          if (newind.eq.0) then
            if (mdate(indtil,tilseq).gt.mdate(oldind,tilseq)) newind =
     1          indtil
          else if (mdate(indtil,tilseq).lt.mdate(newind,tilseq).and.
     1        mdate(indtil,tilseq).gt.mdate(oldind,tilseq)) then
            newind = indtil
          end if
        end if
   10 continue
      if (newind.eq.0) then
        indxy(iplane) = oldind
      else
        indxy(iplane) = newind
      end if
c
      return
      end

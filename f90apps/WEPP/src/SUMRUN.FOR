      subroutine sumrun(wmelt)
c********************************************************************
c                                                                   *
c  This subroutine is called from SR CONTIN to generate a summary   *
c  of the number of runoff events and total runoff volume generated *
c  during the simulation period.                                    *
c                                                                   *
c********************************************************************
c
      real wmelt
c
      include 'pmxelm.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxhil.inc'
c
c********************************************************************
c                                                                   *
c   Common Blocks                                                   *
c                                                                   *
c********************************************************************
c
      include 'cavloss.inc'
c
      include 'cclim.inc'
      include 'cefflen.inc'
c      read: efflen(mxplan)
      include 'chydrol.inc'
c
      include 'cirriga.inc'
c
      include 'cslpopt.inc'
c
      include 'cstmflg.inc'
c
      include 'cstruc.inc'
      include 'csumirr.inc'
      include 'csumout.inc'
c
c********************************************************************
c                                                                   *
c   sumout variables updated                                        *
c     nrunot(mxplan),nrunoy(mxplan),nrunom(13,mxplan)               *
c     trunot(mxplan),trunoy(mxplan),trunom(13,mxplan)               *
c                                                                   *
c********************************************************************
cReza put IF here 3/4/94
c Change to include consideration of irrigation-induced runoff.
c dcf 8/25/94
c
      if(rain(iplane) .gt. 0.0 .or. irrund(iplane) .gt. 0.0) then
        nrunot(iplane) = nrunot(iplane) + 1
        nrunoy(iplane) = nrunoy(iplane) + 1
        nrunom(iplane) = nrunom(iplane) + 1
c
        trunot(iplane) = trunot(iplane) + (runoff(iplane)*1000.0) *
     1    efflen(iplane) / totlen(iplane)
        trunoy(iplane) = trunoy(iplane) + (runoff(iplane)*1000.0) *
     1    efflen(iplane) / totlen(iplane)
        trunom(iplane) = trunom(iplane) + (runoff(iplane)*1000.0) *
     1    efflen(iplane) / totlen(iplane)
c
c    monthly rainfall runoff totals 06-27-94 sjl
c
      else
c Reza 3/7/94.
c
c   XXX this appears to be incorrect.  Will calculate melt runoff if
c       irrigation has occured on a day without rainfall in the middle
c       of the summer.  06-22-94 04:49pm  sjl
c       however there is probably a flag for frozen soil
c       that i do not know about, i added following condition for my work
c
c
c    ...if 5 day avg temp less than freezing count runoff as melt runoff
c    otherwise it will be calculated as irrigation runoff06-27-94 10:49am  sjl
c
c XXX   The following causes problems with the water balance, because
c       melt runoff events on days in which average temp is greater
c       than 0 degrees Centigrade are not added in.  Change this so that
c       it checks melt values (wmelt(iplane)) -     dcf  7/6/94
c       if(tmnavg.lt.0)then
c
        if(wmelt .gt. 0.0)then
          nmunot(iplane) = nmunot(iplane) + 1
          nmunoy(iplane) = nmunoy(iplane) + 1
          nmunom(iplane) = nmunom(iplane) + 1
c
          tmunot(iplane) = tmunot(iplane) + (runoff(iplane)*1000.0) *
     1      efflen(iplane) / totlen(iplane)
          tmunoy(iplane) = tmunoy(iplane) + (runoff(iplane)*1000.0) *
     1      efflen(iplane) / totlen(iplane)
          tmunom(iplane) = tmunom(iplane) + (runoff(iplane)*1000.0) *
     1      efflen(iplane) / totlen(iplane)
        endif

      endif
c
      return
      end

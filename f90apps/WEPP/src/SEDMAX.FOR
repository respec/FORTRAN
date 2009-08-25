      subroutine sedmax(jnum,amax,amin,ptmax,ptmin)
c
c     + + + PURPOSE + + +
c     Finds the maximum and minimum detachment and deposition from segments.
c
c     Called from SEDSTA.
c     Author(s):
c     Reference in User Guide:
c
c     Changes:
c         1) The line:
c                do 10 i=ibegin,iend
c            was changed to:
c                do 10 i=ibegin+1,iend .
c         2) Code was added to find *middle* of region, if a series
c            of numbers match the maximum detachment/deposition.
c         3) Position of 3rd & 4th parameters switched.  Corresponding
c            change must be made in SEGSTA.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/26/91 - 04/29/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxpts.inc'
      include 'pmxseg.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer jnum
      real amax(mxseg), amin(mxseg), ptmax(mxseg), ptmin(mxseg)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     jnum  - detachment/deposition region number
c     amax  - maximum detachment/deposition for region
c     amin  - minimum detachment/deposition for region
c     ptmax - location of maximum detachment/deposition
c     ptmin - location of minimum detachment/deposition
c
c     + + + COMMON BLOCKS + + +
      include 'csedld.inc'
c       read: dstot(mxpts),stdist(mxpts),ibegin,iend,jflag(mxseg),
c             lseg
c
c     + + + LOCAL VARIABLES + + +
      integer  iptmin,jptmin,iptmax,jptmax,i
      real  chkval
c
c     + + + LOCAL DEFINITIONS + + +
c     iptmin - point where min. is first encountered
c     jptmin - point where min. is last encountered
c     iptmax - point where max. is first encountered
c     jptmax - point where max. is last encountered
c     chkval - amount 2 numbers must differ by to be "different"
c
c     + + + SAVES + + +
c
c     + + + EQUIVALENCES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + FUNCTION DECLARATIONS + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + INPUT FORMATS + + +
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
      iptmin = ibegin
      iptmax = ibegin
      jptmin = ibegin
      jptmax = ibegin
      amax(jnum) = dstot(ibegin)
      ptmax(jnum) = stdist(ibegin)
      amin(jnum) = dstot(ibegin)
      ptmin(jnum) = stdist(ibegin)
c
      chkval = dstot(ibegin) * 0.0001
c
c     For a segment where DETACHMENT is occurring....
      if (jflag(lseg).eq.1) then
c
        do 10 i = ibegin + 1, iend
c
          if (dstot(i).gt.amax(jnum)) then
            if ((dstot(i)-amax(jnum)).gt.chkval) then
              amax(jnum) = dstot(i)
              ptmax(jnum) = stdist(i)
              iptmax = i
              jptmax = i
            else
c             write (6,'(" Same Max detected #1", e12.7)') dstot(i)
              jptmax = i
            end if
          end if
c
c
          if (dstot(i).lt.amin(jnum)) then
            if ((amin(jnum)-dstot(i)).gt.chkval) then
              amin(jnum) = dstot(i)
              ptmin(jnum) = stdist(i)
              iptmin = i
              jptmin = i
            else
c             write (6,'(" Same Min detected #1", e12.7)') dstot(i)
              jptmin = i
            end if
          end if
c
   10   continue
c
        if (iptmin.ne.jptmin) then
c         write (6,'(" Averaging Mins #1")')
          i = (iptmin+jptmin) / 2
          amin(jnum) = dstot(i)
          ptmin(jnum) = stdist(i)
        end if
c
        if (iptmax.ne.jptmax) then
c         write (6,'(" Averaging Maxes #1")')
          i = (iptmax+jptmax) / 2
          amax(jnum) = dstot(i)
          ptmax(jnum) = stdist(i)
        end if
c
c
c     For a segment where DEPOSITION (negative detachment) is occurring....
      else if (jflag(lseg).eq.0) then
c
c
        do 20 i = ibegin, iend
c
          if (dstot(i).lt.amax(jnum)) then
            if ((amax(jnum)-dstot(i)).gt.chkval) then
              amax(jnum) = dstot(i)
              ptmax(jnum) = stdist(i)
              iptmax = i
              jptmax = i
            else
c             write (6,'(" Same Max detected #2", e12.7)') dstot(i)
              jptmax = i
            end if
          end if
c
c
          if (dstot(i).gt.amin(jnum)) then
            if ((dstot(i)-amin(jnum)).gt.chkval) then
              amin(jnum) = dstot(i)
              ptmin(jnum) = stdist(i)
              iptmin = i
              jptmin = i
            else
c             write (6,'(" Same Min detected #2", e12.7)') dstot(i)
              jptmin = i
            end if
          end if
c
   20   continue
c
        if (iptmin.ne.jptmin) then
c         write (6,'(" Averaging Mins #2")')
          i = (iptmin+jptmin) / 2
          amin(jnum) = dstot(i)
          ptmin(jnum) = stdist(i)
        end if
c
        if (iptmax.ne.jptmax) then
c         write (6,'(" Averaging Maxes #2")')
          i = (iptmax+jptmax) / 2
          amax(jnum) = dstot(i)
          ptmax(jnum) = stdist(i)
        end if
c
c
      else if (jflag(lseg).eq.2) then
        iptmax = ibegin
        iptmin = ibegin
      end if
c
      return
      end

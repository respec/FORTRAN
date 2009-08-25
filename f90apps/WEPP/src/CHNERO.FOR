      subroutine chnero(ichplt,sdate,nptsc,toplen)
c
c     + + + PURPOSE + + +
c
c     Called from SR WSHDRV for each channel element routed
c     during each runoff event.
c
c     Called from: SR WSHDRV
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcrp.inc'
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real toplen
      integer ichplt, sdate, nptsc
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     rain   - event depth of rainfall
c     toplen - top length of effective channel
c     ichplt - flag indicator if channel plotting output
c     sdate  - julian date of runoff event
c     nptsc  - number of computational channel segments
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchvar.inc'
      include 'cpart1.inc'
      include 'cstore.inc'
      include 'cstruct.inc'
      include 'ctemp.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real topvol, latvol, efflen(mxplan)
      integer i
      save efflen
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     topvol         -
c     latvol         -
c     efflen(mxplan) -
c
c     Integer Variables
c
c     i -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     chnrt
c     strout
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     calculate the top and lateral inflow runoff volumes
c
c     three possible top and lateral inflow cases on channel:
c
c     Case I   - both top inflow and lateral inflow
c     Case II  - top inflow only, no lateral inflow
c     Case III - lateral inflow only, no top inflow
c
c     effective length calculations only valid for Case I -
c     both top and lateral flow inflow into channel
c
      topvol = tmpvol(nhtop(ielmt)) + tmpvol(nitop(ielmt)) +
     1    tmpvol(ncleft(ielmt)) + tmpvol(ncrght(ielmt)) +
     1    tmpvol(nctop(ielmt))
c
      latvol = (runvol(ielmt)-topvol)
c
      if (latvol.gt.0.0) then
cd   Modified by S. Dun, May 25, 2005 for debuging a crush case
cd        chnlef = chnlen(ichan) * (1.0+(topvol/latvol))
cd        toplen = chnlef - chnlen(ichan)
cd
          if ((solwpv.eq.2006).and.(topvol/latvol .gt. 1e+5)) then
              chnlef = chnlen(ichan)
              toplen = 0.0
          else
              chnlef = chnlen(ichan) * (1.0+(topvol/latvol))
              toplen = chnlef - chnlen(ichan)
          endif
cd    End Modifying
      else
        chnlef = chnlen(ichan)
        toplen = 0.0
      end if
c
      efflen(ichan) = chnlef
c
c     add the top length of the effective channel to the computational
c     segment distance and convert from meters to feet
c
      nptsc = 11
c
      do 10 i = 1, nptsc
        x(i) = (chnx(ichan,i)+toplen) * 3.281
        slope(i) = sin(atan(chnslp(ichan,i)))
   10 continue
c
c     write channel conditions prior to simulation
c
c     if (flgout(ichan).ge.5) call chnvar(ichan,chnlen(ichan))
c
c     compute channel hydraulics and erosion
c
      call chnrt(nptsc,sdate,ichplt,latvol)
c
c     write event output
c
      call strout(elmt(ielmt),flgout(ichan),npart,wsarea(ielmt),ielmt,
     1    ichan,sdate,nelmt)
c
      return
      end

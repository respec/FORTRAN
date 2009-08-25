
c     begin include file cchpar.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /chpar/    ishape(mxplan),chnlen(mxplan),chnz(mxplan),
     1                  chnnbr(mxplan),ncsseg(mxplan),
     1                  chnx(mxplan,mxcseg),chnslp(mxplan,mxcseg),
     1                  chnwid(mxplan),flgout(mxplan),watsum,wattyp

c     + + + VARIABLE DECLARATIONS + + +

      real              chnlen,chnz,chnnbr,chnx,chnslp,chnwid
      integer           ishape,ncsseg,flgout,watsum,wattyp

c     + + + VARIABLE DEFINITIONS + + +

c     ishape(mxplan)        :
c     chnlen(mxplan)        :
c     chnz(mxplan)          :
c     chnnbr(mxplan)        :
c     ncsseg(mxplan)        :
c     chnx(mxplan,mxcseg)   :
c     chnslp(mxplan,mxcseg) :
c     chnwid(mxplan)        :
c     flgout(mxplan)        :
c     watsum                : watershed output option (currently overrides
c                             flgout)
c
c                             0) annual average watershed values; abbreviated
c                             1) yearly average watershed values; abbreviated
c                             2) monthly average watershed values; abbreviated
c                             3) annual average subwatershed values; abbreviated
c                             4) yearly average subwatershed values; abbreviated
c                             5) monthly average subwatershed values; abbreviated
c
c     wattyp                : 0) annual average
c                           : 1) yearly, annual average
c                           : 2) monthly, yearly, annual average
c
c     end include file cchpar.inc

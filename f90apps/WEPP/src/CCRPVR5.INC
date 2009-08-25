
c     begin include file ccrpvr5.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /crpvr5/   basmat(mxplan),basal(mxplan),isimyr,
     1                  ncount(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              basmat,basal
      integer           isimyr,ncount

c     + + + VARIABLE DEFINITIONS + + +

c     basmat(mxplan) : plant basal area at maturity (m^2/ha)
c     basal(mxplan)  : plant basal area (m^2)
c     isimyr         :
c     ncount(mxplan) : a counter for number of days after
c                      senescence when canopy cover starts
c                      to decay

      include 'ccrpvr5a.inc'

c     end include file ccrpvr5.inc


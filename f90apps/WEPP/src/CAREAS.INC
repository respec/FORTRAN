
c     begin include file careas.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /areas/    arecom(3,mxplan),kscru(mxplan),ksmac(mxplan),
     1                  kscan(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              arecom,kscru,ksmac,kscan

c     + + + VARIABLE DEFINITIONS + + +

c     arecom(3,mxplan) : area fractions for use in IRS to divide soil surface
c                        area into 3 parts:
c                        1 - bare and crusted
c                        2 - under cover and uncrusted
c                        3 - under canopy and uncrusted
c     kscru(mxplan)    : saturated hydraulic conductivity for the
c                        crusted area
c     ksmac(mxplan)    : saturated hydraulic conductivity for the
c                        covered area
c     kscan(mxplan)    : saturated hydraulic conductivity for the
c                        canopy area

c     end include file careas.inc


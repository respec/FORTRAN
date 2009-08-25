
c     begin include file csumirr.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /sumirr/ are used to maintain a record of the
c     number of irrigations that occur, irrigation amount, number of
c     irrigations that occur on days also having rainfall, and runoff
c     attributed to irrigation.

      common /sumirr/   irrund(mxplan),irrunm(mxplan),irrunt(mxplan),
     1                  irruny(mxplan),ncommm,ncommt,ncommy,
     1                  nirrm,nirrt,nirry,tirrm,tirrt,tirry,irper



c     + + + VARIABLE DECLARATIONS + + +

      integer           nirrt,nirry,nirrm,ncommt,ncommy,ncommm
      real              tirrt,tirry,tirrm,irrund,irrunt,irruny,irrunm,
     1                  irper


c     + + + VARIABLE DEFINITIONS + + +

c     irrund(mxplan)    : runoff attributed to irrigation for the day
c                         (m)
c     irrunm(mxplan)    : runoff attributed to irrigation for each month
c                         of the current year (m)
c     irrunt(mxplan)    : total runoff attributed to irrigation for the
c                         simulation (m)
c     irruny(mxplan)    : runoff attributed to irrigation for the
c                         current year (m)
c     ncommm            : number of times irrigation and rainfall occur
c                         on the same day for each month of the current
c                         year
c     ncommt            : total number of times irrigation and rainfall
c                         occur on the same day
c     ncommy            : number of times irrigation and rainfall occur
c                         on the same day for the current year
c     nirrm             : number of irrigation events for each month of
c                         the current year
c     nirrt             : total number of irrigation events for the
c                         simulation
c     nirry             : number of irrigation events for the current
c                         year
c     tirrm             : irrigation amount for each month of the
c                         current year (m)
c     tirrt             : total irrigation amount for the simulation (m)
c     tirry             : irrigation amount for the current year (m)
c
c     end include file csumirr.inc


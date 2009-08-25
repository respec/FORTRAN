
c     begin include file cupdate.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /update/   day,mon,year,sdate,mdate(mxtill,mxtlsq),
     1                  indxy(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           day,mon,year,sdate,mdate,indxy

c     + + + VARIABLE DEFINITIONS + + +

c     day                  : day of year
c     mon                  : month of year
c     year                 : year of simulation
c     sdate                : date of year in Julian date
c     mdate(mxtill,mxtlsq) : Julian date on which tillage occurs
c     indxy(mxplan)        : management operation index

c     end include file cupdate.inc


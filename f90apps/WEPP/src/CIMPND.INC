
c     begin include file cimpnd.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /impnd/ are generic variables
c     needed by the impoundment component

      common /impnd/ ipond, npond, ipd(0:mximp), impond,
     1               filday(mximp), filflg(mximp), filyr(mximp),
     1               otflg(mximp), otyr(mximp), oteos(mximp),
     1               tcf(0:mximp)

c     + + + VARIABLE DECLARATIONS + + +

      integer ipond, npond, ipd, impond, filday, filflg, filyr, otflg,
     1    otyr, oteos
      real tcf

c     + + + VARIABLE DEFINITIONS + + +

c     ipond         : current impoundment
c     npond         : total number of impoundments
c     ipd(0:mximp)  : counter for the current impoundment
c     impond        : flag for impoundment routing
c     filday(mximp) : date that the impoundment fills with sediment
c     filflg(mximp) : flag for impoundment filling with sediment
c     filyr(mximp)  : year that the impoundment fills with sediment
c     otflg(mximp)  : flag for impoundment overtopping
c     otyr(mximp)   : number of days impoundment overtops in a year
c     oteos(mximp)  : number of days impoundment overtops during simulation
c     tcf(0:mximp)  : impoundment time of concentration (hrs)

c     end include file cimpnd.inc



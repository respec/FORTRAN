
c     begin include file cintgrl.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /intgrl/   si(mxtime+1),ii

c     + + + VARIABLE DECLARATIONS + + +

      integer           ii
c     real              si
      double precision  si

c     + + + VARIABLE DEFINITIONS + + +

c     si(mxtime+1) : integral of rainfall excess
c     ii :           index for time array (current time is
c                    between t(ii) and t(ii+1)

c     end include file cintgrl.inc



c     begin include file cirinfl.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /irinfl/ define the calibrated Kostiakov-Lewis
c     infiltration function.

      common /irinfl/   kosta,kostf,kostk

c     + + + VARIABLE DECLARATIONS + + +

      real              kosta,kostf,kostk

c     + + + VARIABLE DEFINITIONS + + +

c     kosta : exponent of time for the first term of the Kostiakov-
c             Lewis infiltration function (no units)
c     kostf : coefficient of time for the second term of the Kostiakov-
c             Lewis infiltration function (m^2/s)
c     kostk : coefficient of time for the first term of the Kostiakov-
c             Lewis infiltration function (m^2/s^b) where b depends on
c             the value of kosta

c     end include file cirinfl.inc


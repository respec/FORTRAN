c     begin include file cunicon

c     + + + COMMON BLOCK DESCRIPTION + + +
c      this include file holds the output option and units flag values
c      for unit conversions

      common /unicon/ units, outopt

c
c     + + + VARIABLE DECLARATIONS + + +
c

      integer          units, outopt
c
c    + + + VARIABLE DEFINITIONS
c
c    units    :   0=metric, 1=english, default is english
c    outopt   :   output option for basic soil output
c
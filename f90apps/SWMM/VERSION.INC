C This common was implemented to make it easier to track, update,
C and document program version numbers.  They are removed from the code
C and placed in separate FORTRAN File VERSION.FOR which is called from
C MAIN SWMM program immediately upon program execution.
      COMMON /VER1/VERID,DESTRING(5)
      CHARACTER*6 VERID
      CHARACTER*45 DESTRING



c     begin include file cprams2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /prams2/   alpha(mxplan),norun(mxplan),alphay(mxplan),
     1                  watblf(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           norun,watblf
      real              alpha,alphay

c     + + + VARIABLE DEFINITIONS + + +

c     alpha(mxplan)     : Chezy depth-discharge coefficient
c     norun(mxplan)     : flag for runoff occurrence
c     alphay(mxplan)    : previous days alpha value
c     watblf(mxplan)    : flag for water balance calculated

c     end include file cprams2.inc


      function irflow(x1)
c
c     -- XXX -- Note: This function is used in a peculiar way.  It is
c                     called by a pointer passed to a subroutine.
c                     Perhaps one might include it in the calling
c                     subroutine? -- CRM -- 7/30/93.
c
c     + + + PURPOSE + + +
c     This function calculates the ratio of the area of flow function to
c     the derivative of the area of flow function.  This ratio is used
c     by subprogram NEWRAP.
c
c     Author(s): E. R. Kottwitz
c     Reference in User Guide: Chapter 12
c
c     + + + ARGUMENT DECLARATIONS + + +
      real      x1
c
c     + + + ARGUMENT DEFINITIONS + + +
c     x1     - old estimate of root of the function
c
c     + + + COMMON BLOCKS + + +
      include 'ciraflo.inc'
c       read: aqexp,ircon1,ircon2
c
c     + + + FUNCTION DELCLARATIONS + + +
      real  irflow
c
c     + + + END SPECIFICATIONS + + +
c
c     ---- (WEPP Equation 12.11, divided by derivative of WEPP equation
c          12.11 with respect to X1)
      irflow = (x1**aqexp+ircon1*x1+ircon2)/(aqexp*x1**(aqexp-1.0)+
     1         ircon1)
      return
      end

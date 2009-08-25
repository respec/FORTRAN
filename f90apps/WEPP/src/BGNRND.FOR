      subroutine bgnrnd(x0)
c
c     + + + PURPOSE + + +
c     Initializes the  seed  for  the  random  number generator,
c     RND.  This routine, with debug lines activated, checks to
c     see if the uniform c random number generator, RAND, will
c     work in double.  (Changes the value of xrand in the common
c     block RNDM.)
c
c     Called from HDRIVE
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c          1) Statement:     if (xrand.le.0.d0) then
c             Changed to:    if (xrand.lt.1.d0) then
c          2) SAVE statement, which saves ALL local variables, removed.
c          3) DATA statements added to BLKDATA, as per 6/9/91 telcon
c             with Jeff Stone.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 03/27/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real x0
c
c     + + + ARGUMENT DEFINITIONS + + +
c     x0     -
c
c     + + + COMMON BLOCKS + + +
      include 'crndm.inc'
c     modify: aa,mrnd,xrand
c
c     + + + LOCAL VARIABLES + + +
      real u
c
c     + + + LOCAL DEFINITIONS + + +
c     u     -
c
c     + + + SAVES + + +
      save /rndm/
c
c     + + + END SPECIFICATIONS + + +
c
c------------------------------------------------------------------
c Set AA and MRND to values that make a reasonable random number
c generator.
c------------------------------------------------------------------
c
c  XXX -- Do we need a DATA statement here?  Using the current
c         approach, we will get the same "random numbers" each
c         time the program executes on the same machine.
c         CRM -- 6/03/91.
c
c      Yes, Jeff Stone called to say we do.  DATA statement
c      added to BLKDATA.   CRM -- 8/9/91.
c
c      Original Code:
c     aa=16807.d0
c     mrnd=2147483647.d0
c
c test for double precision operation.
c
      xrand = aa * mrnd
      u = aa
      if ((xrand.eq.xrand+1.d0).or.(u.eq.u+1.)) then
        write (6,*)
     1      ' insufficent precision for random number generator.'
        stop
      end if
c
c------------------------------------------------------------------
c     Initializes xrand to  min(max(int(X0),1), (MRND-1)).
c     This ensures xrand is initially positive and less than MRND.
c------------------------------------------------------------------
c
c     initialize xrand
c
      xrand = aint(x0)
      if (xrand.lt.1.d0) then
        xrand = 1.
      else if (xrand.ge.mrnd) then
        xrand = mrnd - 1.d0
      end if
c
      return
      end

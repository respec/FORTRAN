      subroutine dblex
c
c     + + + PURPOSE + + +
c     Generates step functions to represent delta-t and delta--
c     intensity, for cases where intensity is NOT constant, by
c     solving a double exponential distribution function.
c     (Updates INTDL(20) & TIMEDL(20). )
c
c     Note: Either DBLEX is called from DISAG, or CONST is, but
c           NOT BOTH!
c
c     This is a double exponential distribution
c      1. for 0 <= timedl(i) <= tp
c         i(timedl(i)) = a*exp(b*timedl(i))
c         timedl(i+1) = (1.0/b)*log(1.0 + b*fq/a)
c      2. for tp <= timedl(i) <= 1.0
c         i(timedl(i)) = ip*exp(-c*(timedl(i)-tp))
c         timedl(i) = tp-(1.0/c)*log(1.0 - (c/ip)*(fq-tp))
c
c     Called from DISAG.
c     Author(s): Lane, Lopez, Stone
c     Reference in User Guide:
c
c     Changes:
c           1) Statement:     if(timep .ge. 1.)timep=0.99
c              Changed to:    if(timep .gt. 0.99) timep=0.99
c              Note: This ALSO seems to cause changes in the predicted
c                    point of maximum detachment, when the detachment
c                    rate is nearly constant.  CRM 4/9/91 (See APPMTH.)
c           2) Statement removed:  intdl(1) = a
c              (It was being re-calculated before it was used, in the
c              "do 20" loop.)
c           3) The "do 20" loop was eliminated by integrating it's
c              code into the "do 10" loop.
c           4) SAVE statement, which saves ALL local variables, removed.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/09/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c     includes for sdate tracking debug
      include 'pmxpln.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'cupdate.inc'
c     end includes for sdate tracking
c
c     + + + COMMON BLOCKS + + +
      include 'cdiss2.inc'
c     modify: timedl(20)
c      write: intdl(20)
c
      include 'cdiss11.inc'
c       read: ninten(mxplan)
c
      include 'cdiss3.inc'
c       read: timep, ip, deltfq
c
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      integer err, i, i1
      real a, u, b, d, fqx
c
c     + + + LOCAL DEFINITIONS + + +
c     err    - Flag. 0: equation solved.
c                    1: no solution for given A.
c     a      - constant in the equation:  1 - exp(-U) = A*U.
c              (in this routine, A = 1/Ip.)
c     u      - see A above
c     b      - coefficient in double exponential
c     d      - coefficient in double exponential
c     fqx    - For idntermediate calcs. Starts with the value of 0.
c
c     + + + SUBROUTINES CALLED + + +
c     eqroot
c
c     + + + END SPECIFICATIONS + + +
c
c                                                                 *
c******************************************************************
c
c
c     Check to make sure Ip is in range so machine can make the
c     calculations without a machine overflow, make Ip <= 60.0
c     if Ip was greater than 60.0
c
      if (ip.gt.60.0) ip = 60.0
      if (timep.gt.0.99) timep = 0.99
c
c     Newton's method for B and then A in i(t)=a*exp(b*t)
c
c     Original Code:
c     u = eqroot(1./ip,err)
      call eqroot(1./ip,err,u)
      b = u / timep
      a = ip * exp(-u)
c
c     The formulas for dissagregation give u=btp=d(1-tp).
c
      d = u / (1.-timep)
c
c     (TIMEDL(1) and INTDL(NINT) are initialized in DISAG.)
c
      timedl(ninten(iplane)) = 1.0
      fqx = 0.0
      do 10 i = 1, ninten(iplane) - 1
        i1 = i + 1
        if (i.lt.(ninten(iplane)-1)) then
          fqx = fqx + deltfq
          if (fqx.le.timep) then
            timedl(i1) = (1.0/b) * log(1.0+(b/a)*fqx)
          else
            timedl(i1) = timep - (1.0/d) * log(1.0-(d/ip)*(fqx-timep))
          end if
        end if
c
c          intdl(i) = deltfq / (timedl(i1)-timedl(i))
c
c     inserted to avoid devide by zero when using SALFORD95 Compiler
c     11/12/98. Added again 4-24-2008 when extensive floating trap
c     compiler options are used on Intel compiler.
c
        if ((timedl(i1)-timedl(i)).gt.0)then
          intdl(i) = deltfq / (timedl(i1)-timedl(i))
        else
c         print*, sdate,timedl(i1), timedl(i)
          intdl(i) = deltfq / 0.00001
        endif
c
c     end of SALFORD95 insert
c
   10 continue
c
      return
      end

      subroutine fslpar(ichan,z,endman,leff,ibeg,flag1,c1,c3,endslp,ssfe
     1    )
c
c     + + + PURPOSE + + +
c
c     SR FSLPAR computes the friction slope at the channel outlet.
c
c     Called from: SR CHNRT
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxpln.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real c1, c3, endman, endslp, ssfe, z, leff
      integer ibeg, ichan, flag1
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     c1     -
c     c3     -
c     endman -
c     endslp -
c     ssfe   -
c     z      -
c     leff   -
c     ibeg   -
c     ichan  -
c     flag1  -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchflo.inc'
      include 'cchtrl.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real nsfe, crit, fslq, unifor, ytr
c
c     + + + LOCAL DEFINITIONS + + +
c
c     nsfe   -
c     crit   -
c     fslq   -
c     unifor -
c     ytr    -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     crit
c     fslq
c     unifor
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      ibeg = 1
c
c     compute constant c3 in the normalized equation for spatially
c     varied flow
c
      c1 = (z**2.5/(2.0*sqrt(z**2.0+1.0))) ** (2.0/3.0)
c
c     compute normal depth (ynor) and critical depth (ycr) and set
c     depth at the end of current channel element
c
c     normal depth calculations are performed on channels having
c     triangular, rectangular and trapezoidal shapes
c
c     critical depth calculations are performed assuming the channel
c     has a triangular shape
c
      ynor = unifor(endslp,z,endman,qe)
c
      ycr = crit(z,qe)
      ye = ynor
c
c     return if assuming sf = so (cflags = ienslp = 2)
c
      if (cflags.eq.2) return
c
c     compute control depth at the outlet control
c
c     if critical depth at oulet control
c
      if (icntrl(ichan).eq.1) ytr = ycr
c
c     if normal depth at oulet control
c
      if (icntrl(ichan).eq.2.or.icntrl(ichan).eq.0) ytr =
c     if (icntrl(ichan).eq.2) ytr =
     1    unifor(ctlslp(ichan),ctlz(ichan),ctln(ichan),qe)
c
c     if normal flow in the channel reach
c
      if (icntrl(ichan).eq.3) ytr =
     1    unifor(ctlslp(ichan),ctlz(ichan),endman,qe)
c
c     if rating curve at outlet control
c
      if (icntrl(ichan).eq.4) ytr = rcoset(ichan) + (qe/rccoef(ichan))
     1    ** (1.0/rcexp(ichan))
c
c     normal depth (ynor) is greater than critical depth (ycr)
c
      if (ynor.le.ycr) then
c
        cflags = 2
c
c       Baffaut correction follows:          dcf  11/25/96
c       if (icntrl(ichan).le.1.and.ytr.le.ynor) then
        if (icntrl(ichan).eq.1.and.ytr.le.ynor) then
c       Baffaut correction ends.             dcf  11/25/96
c
          ye = ynor
          sfe = endslp
          return
        end if
c
        ye = ytr
        sfe = fslq(qe,endman,c1,ye)
        return
c
      end if
c
      cflags = 1
c
      if (icntrl(ichan).ne.1) then
        if (ytr.lt.ynor) ytr = ynor
        nsfe = endman
      else
c
        nsfe = nbarch
      end if
c
      flag1 = 5
c
      ye = ytr
      sfe = fslq(qe,nsfe,c1,ye)
c
c     Baffaut correction follows:        dcf  11/25/96
c     ssfe = (endslp-sfe) * ye / leff
      ssfe = (endslp-sfe) * leff / ye
c     Baffaut correction ends.           dcf  11/25/96
c
      c3 = 2.0 * beta * qe ** 2.0 / (agrav*z**2.0*ye**5.0)
c
      return
      end

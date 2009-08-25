      subroutine frichn(q,n,z,c1,c3,i,flag1,slope,xstar,leff,ye,xbeg,
     1    ibeg,ssfb,ssfe,sf)
c
c     + + + PURPOSE + + +
c
c     SR FRICHN uses regression equations which approximate the
c     spatially varied flow equation to compute the energy slope at
c     points above the channel outlet control.
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
c     + + + ARGUMENT DECLARATIONS + + +
c
      real c1, c3, q, sf, slope, ssfb, ssfe, xbeg, ye, z, xstar, leff
      integer i, ibeg, flag1
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     c1    -
c     c3    -
c     q     -
c     sf    -
c     slope -
c     ssfb  -
c     ssfe  -
c     xbeg  -
c     ye    -
c     z     -
c     xstar -
c     leff  -
c     i     -
c     ibeg  -
c     flag1 -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real intrpl, n, crit, fslq, sfstar, ssf, sstar, ycrch, sc
c
c     + + + LOCAL DEFINITIONS + + +
c
c     intrpl -
c     n      -
c     crit   -
c     fslq   -
c     sfstar -
c     ssf    -
c     sstar  -
c     ycrch  -
c     sc     -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     crit
c     fslq
c     intrpl
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     write(6,*) q,xstar,xbeg,ibeg,ssfb,ssfe,sf
c
      ycrch = crit(z,q)
      sc = fslq(q,n,c1,ycrch)
c
      if (slope.ge.sc) then
        sf = slope
        return
      end if
c
      sstar = slope * leff / ye
c
      if (flag1.ne.1) then
        if (flag1.eq.2) go to 20
        if (flag1.eq.3) go to 40
        if (flag1.eq.4) go to 50
        if (c3.lt.0.3) go to 10
      end if
c
      flag1 = 1
c
      if (xstar.gt.0.90001) then
        ssf = intrpl(xbeg,ssfb,1.0,ssfe,xstar)
c        write(6,*) 'xbeg = ',xbeg,'; xstar = ',xstar
c        write(6,*) 'ssfb = ',ssfb,'; ssfe  = ',ssfe,'; ssf = ',ssf
        go to 60
      end if
c
      if (sstar.ge.0.0.and.sstar.le.1.2) then
        ssf = 0.2777 - 3.3110 * xstar +
     1    9.1683 * xstar ** 2 - 8.9551 * xstar ** 3
      endif
      if (sstar.gt.1.2.and.sstar.le.4.8) then
        ssf = 2.6002 - 8.0678 * xstar +
     1    15.6502 * xstar ** 2 - 11.7998 * xstar ** 3
      endif
      if (sstar.gt.4.8.and.sstar.le.20.0) then
        ssf = 3.8532 - 12.9501 * xstar
     1    + 21.1788 * xstar ** 2 - 12.1143 * xstar ** 3
      endif
      if (sstar.gt.20.0) then
        ssf = 0.0
      endif
c
      ibeg = i
      ssfb = ssf
c
      go to 60
c
   10 continue
c
      flag1 = 2
      if (.not.(c3.lt.0.3.and.c3.ge.0.03)) go to 30
c
   20 continue
c
      if (sstar.ne.0.0) then
c
        if (xstar.gt.0.80001) then
          ssf = intrpl(xbeg,ssfb,1.0,ssfe,xstar)
          go to 60
        end if
c
        ssf = 2.0553 - 6.9875 * xstar + 11.4184 * xstar ** 2 - 6.4588 *
     1      xstar ** 3
        ibeg = i
        ssfb = ssf
c
        go to 60
c
      end if
c
      ssf = 0.0392 - 0.4774 * xstar + 1.0775 * xstar ** 2 - 1.3694 *
     1    xstar ** 3
      ibeg = i
      ssfb = ssf
c
      go to 60
c
   30 continue
c
      flag1 = 3
      if (.not.(c3.lt.0.03.and.c3.ge.0.007)) go to 50
c
   40 continue
c
      if (sstar.ne.0.0) then
c
        if (xstar.gt.0.80001) then
          ssf = intrpl(xbeg,ssfb,1.0,ssfe,xstar)
          go to 60
        end if
c
        ssf = 1.5386 - 5.2042 * xstar + 8.4477 * xstar ** 2 - 4.7401 *
     1      xstar ** 3
        ibeg = i
        ssfb = ssf
        go to 60
      end if
c
      ssf = 0.0014 - 0.0162 * xstar - 0.0926 * xstar ** 2 - 0.0377 *
     1    xstar ** 3
      ibeg = i
      ssfb = ssf
c
      go to 60
c
   50 continue
c
      flag1 = 4
c
      if (sstar.ne.0.0) then
c
        if (xstar.gt.0.70001) then
          ssf = intrpl(xbeg,ssfb,1.0,ssfe,xstar)
          go to 60
        end if
c
        ssf = 1.2742 - 4.7020 * xstar + 8.4755 * xstar ** 2 - 5.3332 *
     1      xstar ** 3
        ibeg = i
        ssfb = ssf
      else
        ssf = -0.0363 * xstar ** 2
        ibeg = i
        ssfb = ssf
      end if
c
   60 continue
c
      sfstar = sstar - ssf
      sf = sfstar * ye / leff
c
      return
      end

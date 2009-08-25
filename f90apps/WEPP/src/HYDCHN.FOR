      subroutine hydchn(flagc,q,sf,c1,z,wb,w,n,crsh,covsh,effsh)
c
c     + + + PURPOSE + + +
c
c     SR HYDCHN computes hydraulics in the channels.
c
c     Called from: SRS CHNRT, DCAP
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
      real c1, covsh, crsh, effsh, q, sf, w, n, wb, z
      integer flagc
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     c1    -
c     covsh -
c     crsh  -
c     effsh -
c     q     -
c     sf    -
c     w     -
c     n     -
c     wb    -
c     z     -
c     flagc -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real nt, lc, mulsh, xlc(16), fglc(16), xxb(27), fhxb(27), a, ap,
     1    dif, glc, hxb, rcov, rsh, rstar, xb, xbn, xbo, y, v
      integer ifrus, itrat, m
c
c     + + + LOCAL DEFINITIONS + + +
c
c     nt       -
c     lc       -
c     mulsh    -
c     xlc(16)  -
c     fglc(16) -
c     xxb(27)  -
c     fhxb(27) -
c     a        -
c     ap       -
c     dif      -
c     glc      -
c     hxb      -
c     rcov     -
c     rsh      -
c     rstar    -
c     xb       -
c     xbn      -
c     xbo      -
c     y        -
c     v        -
c     ifrus    -
c     itrat    -
c     m        -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     table
c
c     + + + DATA INITIALIZATIONS + + +
c
      data xlc /0.0, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14,
     1    0.16, 0.18, 0.2, 0.22, 0.24, 0.26, 0.28/
c
      data fglc /100000.0, 32.91, 15.487, 7.307, 4.849, 3.713, 3.075,
     1    2.676, 2.408, 2.222, 2.089, 1.994, 1.928, 1.884, 1.858,
     1    1.84866/
c
      data xxb /0.0, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14,
     1    0.16, 0.18, 0.2, 0.22, 0.24, 0.26, 0.28, 0.3, 0.32, 0.34,
     1    0.36, 0.38, 0.4, 0.42, 0.44, 0.46, 0.48, 0.5/
c
      data fhxb /0.0, .000474, .00154, .00509, .0104, .0177, 0.0269,
     1    .0384, .0524, .0693, .0897, .114, .1432, 0.1782, .2207, .2724,
     1    .3361, .4159, .5176, .6506, 0.8307, 1.0858, 1.4722, 2.1212,
     1    3.4264, 7.3566, 10000.0/
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (flagc.eq.2) go to 20
      if (flagc.ge.3) go to 50
c
   10 continue
c
c     triangular section
c
      ap = (q*n/(1.49*sqrt(sf))) ** 0.375
      y = ap / (c1**0.375)
      w = 2.0 * y * z
      a = z * (y**2)
      nt = n
c
      go to 60
c
   20 continue
c
c     rectangular section
c
      if (wb.eq.0.0) go to 10
      ap = (q*n/(1.49*sqrt(sf))) ** 0.375
      w = wb
      hxb = (ap/w) ** (8.0/3.0)
c
      if (hxb.le.0.114) then
        xbo = 0.2
c
        do 30 m = 1, 25
          xbn = ((1.0-2.0*xbo)*hxb) ** 0.6
c         added check to guard against divide by zero 9-21-2004 jrf
            if (xbn.eq.0.0) xbn = 1.0e-10
            dif = abs((xbn-xbo)/xbn)
          if (dif.le.0.001) go to 40
          xbo = xbn
   30   continue
c
        if (m.ge.25) then
          write (3,1000)
        end if
c
   40   continue
c
        xb = xbn
      else
        ifrus = 4
        itrat = 27

c dcf Fix added to prevent program stop in
c     subroutine TABLE

        if (hxb.gt.9999.99) hxb = 9999.99

        call table(ifrus,itrat,xxb,fhxb,hxb,xb)
      end if
c
      y = w * xb / (1.0-2.0*xb)
      a = y * w
      nt = n
c
      go to 60
c
   50 continue
c
c     naturally eroded section
c
      ap = (q*nbarch/(1.49*sqrt(sf))) ** 0.375
      glc = ap * wtdh2o * sf / crsh
      if (glc.lt.1.84866) go to 70
      ifrus = 3
      itrat = 16
c
c dcf Fix to prevent program stop in subroutine TABLE
c     3-11-04
c
      if (glc.gt.99999.999) glc = 99999.999

      call table(ifrus,itrat,xlc,fglc,glc,lc)
c
      rstar = (-0.34707*(0.5-lc)**3) - (0.54213*(0.5-lc)**2) + (0.66383*
     1    (0.5-lc))
      w = (ap/rstar**0.625) * (0.73-1.46*lc)
      if (flagc.eq.4) go to 80
      hxb = (ap/w) ** (8.0/3.0)
      ifrus = 4
      itrat = 27
c
c dcf Fix to prevent program stop in 
c     subroutine TABLE  3-11-04
c
      if (hxb.gt.9999.99) hxb = 9999.99

      call table(ifrus,itrat,xxb,fhxb,hxb,xb)
c
      y = w * xb / (1.0-2.0*xb)
      a = y * w
      nt = nbarch
c
   60 continue
c
c     computation of shear stress components
c
c     added check to guard against divide by zero 9-21-2004 jrf
c
      if (a.eq.0.0) a = 1.0e-10
        v = q / a
      rsh = (v*nbarch/(1.49*sqrt(sf))) ** (1.5)
      rcov = (v*(nt-nbarch)/(1.49*sqrt(sf))) ** (1.5)
      effsh = wtdh2o * rsh * sf
      mulsh = wtdh2o * rcov * sf
      if (mulsh.lt.covsh) go to 80
      n = nbarch
      nt = n
      if (flagc.eq.2) go to 20
      if (flagc.ge.3) go to 50
      go to 10
c
   70 continue
c
      if (wb.eq.0.0) go to 10
      go to 20
c
   80 continue
c
      return
c
 1000 format (/5x,'***** DID NOT CONVERGE IN CHANNEL RECTANGULAR ',
     1    'SECTION',/,11x,'SEE HYDCHN.FOR'//)
      end

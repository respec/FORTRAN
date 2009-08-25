      subroutine trncap(effsh,qs,ielmt,tc)
c
c     + + + PURPOSE + + +
c
c     SR TRNCAP computes transport capacity based on new
c     potential sediment load for each particle class.
c
c     Called from: SRS CASE34, CHNRT, DETACH, ENDDET
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
      include 'pmxprt.inc'
      include 'pmxelm.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real effsh, qs(mxpart), tc(mxpart)
      integer ielmt
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     effsh      -
c     qs(mxpart) -
c     tc(mxpart) -
c     ielmt      -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchprt.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real ws(mxpart), wsqrat(mxpart), coef(mxpart), ycrit(mxpart),
     1    delta(mxpart), sigma(mxpart), p(mxpart), dltrat(mxpart), a,
     1    excap, reyn, shield, smdrat, smdrqt, t, vstar
      integer flagd1, flagd2, flagd3, k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     ws(mxpart)     -
c     wsqrat(mxpart) -
c     coef(mxpart)   -
c     ycrit(mxpart)  -
c     delta(mxpart)  -
c     sigma(mxpart)  -
c     p(mxpart)      -
c     dltrat(mxpart) -
c     a              -
c     excap          -
c     reyn           -
c     shield         -
c     smdrat         -
c     smdrqt         -
c     t              -
c     vstar          -
c
c     Integer Variables
c
c     flagd1 -
c     flagd2 -
c     flagd3 -
c     k      -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     shield
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      vstar = sqrt(effsh/msdh2o)
      coef(cnpart) = vstar * agrav * msdh2o
c
      t = 0.0
c
      do 10 k = 1, cnpart
        coef(k) = coef(cnpart) * crdia(k,ielmt) * crspg(k)
   10 continue
c
      do 20 k = 1, cnpart
c
        if (qs(k).eq.0.0) qs(k) = 1.0e-10
        reyn = vstar * crdia(k,ielmt) / knvis
        ycrit(k) = shield(reyn)
        delta(k) = (vstar**2/(crspg(k)-1.0)/agrav/crdia(k,ielmt)/
     1      ycrit(k)) - 1.0
c
        if (delta(k).le.0.0) then
          delta(k) = 0.0
          p(k) = 0.0
        else
          sigma(k) = delta(k) * 2.45 * crspg(k) ** (-0.4) *
     1        sqrt(ycrit(k))
          p(k) = yalcon * delta(k) * (1.0-1.0/sigma(k)*
     1        alog(1.0+sigma(k)))
        end if
c
        t = t + delta(k)
   20 continue
c
      if (t.eq.0.0) t = 1000.0
c
      do 30 k = 1, cnpart
        dltrat(k) = delta(k) / t
c       ws(k)     = p(k)*dltrat(k)*coef(k)
        ws(k) = p(k) * dltrat(k) * coef(k)
   30 continue
c
   40 continue
c
      flagd1 = 0
      flagd2 = 0
      flagd3 = 0
c
      do 50 k = 1, cnpart
        wsqrat(k) = ws(k) / qs(k)
        if (wsqrat(k).gt.1.0) flagd3 = flagd3 + 1
        if (wsqrat(k).ge.1.0) flagd1 = flagd1 + 1
        if (wsqrat(k).le.1.0) flagd2 = flagd2 + 1
   50 continue
c
      if (flagd2.ne.cnpart) then
c
        if (flagd3.ne.cnpart) then
          if (flagd1.eq.cnpart) go to 100
          go to 70
        end if
c
      end if
c
      do 60 k = 1, cnpart
        tc(k) = ws(k)
   60 continue
c
      return
c
   70 continue
c
      smdrqt = 0.0
      smdrat = 0.0
c
      do 80 k = 1, cnpart
c
        if (wsqrat(k).ge.1.0) then
          smdrqt = smdrqt + qs(k) / coef(k) / p(k)
          ws(k) = qs(k)
        else
          if (wsqrat(k).lt.1.0) smdrat = smdrat + dltrat(k)
        end if
c
   80 continue
c
      excap = 1.0 - smdrqt
c
      do 90 k = 1, cnpart
        if (smdrat.eq.0.0) smdrat = 1000000.0
        if (wsqrat(k).lt.1.0) ws(k) = dltrat(k) / smdrat * excap *
     1      p(k) * coef(k)
   90 continue
c
      go to 40
c
  100 continue
c
      smdrat = 0.0
c
      do 110 k = 1, cnpart
        smdrat = smdrat + qs(k) / (coef(k)*p(k))
  110 continue
c
      a = 1.0 / smdrat
      if ((a.gt.0.9999900).and.(a.lt.1.0000099)) a = 1.000000
c
      do 120 k = 1, cnpart
        tc(k) = a * qs(k)
  120 continue
c
      return
      end

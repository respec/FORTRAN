      subroutine profil(iplane)
c********************************************************************
c   This subroutine reads in slope input data from screen or a      *
c   file and computes dimensionless elevations, horizontal          *
c   distances and slopes (assumed linear between points)            *
c                                                                   *
c********************************************************************
c
      integer i, iplane, k, km
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxslp.inc'
c
c     mxnsl             : maximum number of soil layers
c
c*********************************************************************
c                                                                    *
c   Common Blocks                                                    *
c                                                                    *
c*********************************************************************
c
      include 'cdist.inc'
c
c*********************************************************************
c                                                                    *
c dist variables updated                                             *
c    y(101,mxplan) :                                                 *
c                                                                    *
c*********************************************************************
c
      include 'cslope.inc'
c
      save
      real xstar(mxslp), yu(mxslp), yl(mxslp), slen(mxplan), c(mxslp),
     1    sstar(mxslp)
c
      slen(iplane) = xinput(nslpts(iplane),iplane)
      y(nslpts(iplane),iplane) = 0.0
      do 10 k = 1, nslpts(iplane) - 1
        km = nslpts(iplane) - k
        y(km,iplane) = y(km+1,iplane) + (xinput(km+1,iplane)-
     1      xinput(km,iplane)) * (slpinp(km,iplane)+
     1      slpinp(km+1,iplane)) / 2.0
   10 continue
      avgslp(iplane) = y(1,iplane) / slen(iplane)
c
c     correction by dcf to prevent model bombing - do not allow avgslp=0
c
c     Change of AVGSLP value by Baffaut 1996 - dcf 3/97
c     if (avgslp(iplane).le.0.0) avgslp(iplane) = 0.00001
      if (avgslp(iplane).le.0.0) avgslp(iplane) = 0.000001
      do 20 k = 1, nslpts(iplane)
        sstar(k) = slpinp(k,iplane) / avgslp(iplane)
        xstar(k) = xinput(k,iplane) / slen(iplane)
   20 continue
      do 30 k = 2, nslpts(iplane)
        a(k,iplane) = (sstar(k)-sstar(k-1)) / (xstar(k)-xstar(k-1))
        b(k,iplane) = sstar(k-1) - a(k,iplane) * xstar(k-1)
   30 continue
      yl(1) = 1.0
      xl(1,iplane) = 0.0
      do 40 k = 2, nslpts(iplane)
        yu(k) = yl(k-1)
        xu(k,iplane) = xl(k-1,iplane)
        c(k) = yu(k) + a(k,iplane) * xstar(k-1) ** 2 / 2.0 +
     1      b(k,iplane) * xstar(k-1)
        yl(k) = -a(k,iplane) * xstar(k) ** 2 / 2.0 - b(k,iplane) *
     1      xstar(k) + c(k)
        xl(k,iplane) = xstar(k)
   40 continue
      k = 2
      y(1,iplane) = 1.0
      do 60 i = 2, 101
        xinput(i,iplane) = float(i-1) * 0.01
   50   if (xinput(i,iplane).gt.xstar(k)) then
          k = k + 1
          go to 50
        end if
        y(i,iplane) = -a(k,iplane) * xinput(i,iplane) ** 2 / 2.0 -
     1      b(k,iplane) * xinput(i,iplane) + c(k)
   60 continue
      return
      end

      subroutine spread(init,flg,ichan,nin,xin,yin,nout,xout,yout)
c----------------------------------------------------------------------
c     spread initializes the depth to non-erodible layer in the channel
c     and the channel width; the depth to non-erodible layer and width
c     are also reset to their initial values after each tillage.  also
c     the width and depth values are converted from meters to feet in
c     this subroutine.
c----------------------------------------------------------------------
c
      integer flg, init, ichan, nin, nout
      include 'pmxcsg.inc'
      include 'pmxpln.inc'
c
      real xin(mxcseg), yin(mxplan,mxcseg), xout(mxcseg),
     1    yout(mxplan,mxcseg), y
      integer iin, iout
      iin = 1
      y = yin(ichan,iin) * 3.281
      if (init.eq.0) y = abs(y)
      do 10 iout = 1, nout
        if ((iin.eq.nin).or.(xout(iout).lt.xin(iin))) then
          if (y.gt.0) yout(ichan,iout) = y
        else
          if (flg.eq.1.and.y.gt.0) yout(ichan,iout) = y
          iin = iin + 1
          y = yin(ichan,iin) * 3.281
          if (init.eq.0) y = abs(y)
          if (flg.eq.2.and.y.gt.0) yout(ichan,iout) = y
        end if
   10 continue
      return
      end

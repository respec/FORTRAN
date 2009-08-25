      subroutine sedist(dslost)
c****************************************************************
c                                                               *
c  This subroutine is called from SR SEDSEG and creats a re-    *
c  presentative hillslope profile(sediment load, x-distance,    *
c  delta x) from overland flow elements.                        *
c                                                               *
c****************************************************************
c****************************************************************
c                                                               *
c  Argument                                                     *
c    dslost                                                     *
c                                                               *
c****************************************************************
c
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxpts.inc'
      include 'pmxslp.inc'
      include 'pmxseg.inc'
c
c****************************************************************
c                                                               *
c   Common Blocks                                               *
c                                                               *
c****************************************************************
c
      include 'cdist.inc'
c
      include 'csedld.inc'
c
c*********************************************************************
c                                                                    *
c sedld variables updated                                            *
c   ysdist    :                                                      *
c                                                                    *
c*********************************************************************
c
      include 'cslope.inc'
      include 'cstruc.inc'
c
c
c
c****************************************************************
c                                                               *
c   sedld variables updated                                     *
c       dstot(mxpts),stdist(mxpts),delxx(mxplan)                 *
c                                                               *
c****************************************************************
c
c****************************************************************
c                                                               *
c   local variables                                             *
c     kk   :                                                    *
c     ytot :                                                    *
c     i    :                                                    *
c     k    :                                                    *
c     ydist :                                                   *
c     xdist :                                                   *
c     dist  :                                                   *
c                                                               *
c****************************************************************
c
      real xdist(mxplan,101), dslost(mxplan,100), ydist(mxplan,101),
     1     dist, ytot
      integer i, j, k, kk, ll
c
c****************************************************************
c
      kk = 0
      ytot = 0.
c
c     Correction added 4/3/90 by dcf
c
      if (nplane.gt.1) then
        do 10 i = 2, nplane
          ytot = ytot + slplen(i) * avgslp(i)
   10   continue
      end if
c
c
      do 40 i = 1, nplane
c
c
c       Change to delxx made by dcf 7/6/90 to reduce array size
c       from 1000 to 10.
c
c
        delxx(i) = slplen(i) / 100.0
c
c
        do 30 k = 2, 101
          ll = k - 1
          dist = 0.0
          do 20 j = 1, i
            if (i.ne.j) then
              dist = slplen(i-j) + dist
            end if
   20     continue
          ydist(i,k) = ytot + slplen(i) * avgslp(i) * y(k,i)
          xdist(i,k) = dist + (slplen(i)/100.0) * float(k-1)
          kk = kk + 1
          dstot(kk) = dslost(i,ll)
          stdist(kk) = xdist(i,k)
          ysdist(kk) = ydist(i,k)
c       delxx(kk)=slplen(i)/100.0
   30   continue
        if (i.ne.nplane) ytot = ytot - slplen(i+1) * avgslp(i+1)
   40 continue
c
      return
      end

      subroutine sedsta(jnum,dloss,dsstd,vmax,pmax,vmin,pmin)
c
c*******************************************************************
c                                                                  *
c  This subroutine is called from SR SEDSEG and finds the mean     *
c  and standard deviation of detachment or deposition points       *
c  within a segment. It calls SR SEDMAX.                           *
c                                                                  *
c*******************************************************************
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real dloss, dsstd, pmax, pmin, vmax, vmin
c                                                                  *
c  Arguments                                                       *
c     jnum  -   number of the detachment or deposition region      *
c     dloss -   computed detachment (or deposition) at a point     *
c     dsstd -   computed sample standard deviation of segment      *
c     vmax  -   maximum detachment (or deposition) in a segment    *
c     pmax  -   point of maximum detach. or depos. in segment (m)  *
c     vmin  -   minimum detachment (or deposition) in a segment    *
c     pmin  -   point of minimum detach. or depos. in segment (m)  *
c                                                                  *
c*******************************************************************
c
      include 'pmxpln.inc'
      include 'pmxpts.inc'
      include 'pmxseg.inc'
c
c*******************************************************************
c                                                                  *
c   Common Blocks                                                  *
c                                                                  *
c*******************************************************************
c
      include 'csedld.inc'
c
c*******************************************************************
c                                                                  *
c   Local Variables                                                *
c    deviat :  deviation of detach. (or depos.) from the mean      *
c    dssum  :  sum of the squared deviations from the mean         *
c    dsloss :  sum of the detachment (or deposition) at all pts    *
c    i      :  counter variable                                    *
c    jjplan :  variable to assign OFE number for array assignment  *
c    ncomp  :  used to set jjplan                                  *
c    ncompr :  used to find which OFE point belongs to             *
c                                                                  *
c*******************************************************************
c
      dimension dloss(mxseg), dsstd(mxseg), vmax(mxseg), pmax(mxseg),
     1    vmin(mxseg), pmin(mxseg)
      integer jjplan, ncomp, ncompr, i, jnum
      real deviat, dssum, dsloss, dsdist
c
c*******************************************************************
c
      dssum = 0.0
      dsloss = 0.0
      dsdist = 0.0
      ncompr = 101
      ncomp = 1
c
      do 20 i = ibegin, iend
   10   if (i.lt.ncompr) then
          jjplan = ncomp
        else
          ncompr = ncompr + 100
          ncomp = ncomp + 1
          go to 10
        end if
c
        dsloss = dsloss + delxx(jjplan) * dstot(i)
        dsdist = dsdist + delxx(jjplan)
   20 continue
c
c
      dloss(jnum) = dsloss / dsdist
c
c
      do 30 i = ibegin, iend
c
        deviat = dstot(i) - dloss(jnum)
        dssum = dssum + (deviat*deviat)
   30 continue
c
      if (iend.ne.ibegin) then
        dsstd(jnum) = sqrt((dssum/(iend-ibegin)))
      else
        dsstd(jnum) = 0.0
      end if
c
c     Original Code:
c     call sedmax(jnum,vmax,pmax,vmin,pmin)
      call sedmax(jnum,vmax,vmin,pmax,pmin)
c
      return
      end

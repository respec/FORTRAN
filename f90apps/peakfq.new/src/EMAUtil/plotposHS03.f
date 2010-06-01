c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|subroutine plotposHS
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c    
c    this routine provides plotting positions for censored data 
c      using the Hirsch/Stedinger plotting position formula (hirsch, 1987)
c      Purpose is operational version of peakfq (bulletin 17b
c      implementation) which would include the expected moments
c      algorithm (ema; cohn et al. 1997; 2001)
c
c    N.B:  The returned values are "exceedance probabilities" which are 
c             equal to 1-NonExceedance probabilities
c       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c    
c    References
c    ----------
c
c    @article{hirsch1987plotting,
c      title={{Plotting positions for historical floods and their precision}},
c      author={Hirsch, R.M. and Stedinger, J.R.},
c      journal={Water Resources Research},
c      volume={23},
c      number={4},
c      pages={715--727},
c      year={1987}
c    }
c
c    @article{cohn1997algorithm,
c      title={{An algorithm for computing moments-based flood quantile estimates when historical flood information is available}},
c      author={Cohn, TA and Lane, WL and Baier, WG},
c      journal={Water Resources Research},
c      volume={33},
c      number={9},
c      pages={2089--2096},
c      year={1997}
c    }
c
c    @article{cohn2001confidence,
c      title={{Confidence intervals for Expected Moments Algorithm flood quantile estimates}},
c      author={Cohn, T.A. and Lane, W.L. and Stedinger, J.R.},
c      journal={Water resources research},
c      volume={37},
c      number={6},
c      pages={1695--1706},
c      year={2001}
c    }
c
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        15 apr 2010
c       modified            16 apr 2010
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    subroutine calls
c
c    ppfit                  available in probfun.f
c    arrange                available in probfun.f
c    porder                 available in probfun.f
c    ppsolve                available in probfun.f
c    plpos                  available in probfun.f
c    shellsort              available in probfun.f
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       n.b.  use of plotposHS requires two distinct types of information:
c
c            a.  "observations," which are described by {ql,qu};
c                  specifically, the true value of q is assumed to lie
c                  wihin the interval ql(i) <= q(i) <= qu(i);
c                  if q(i) is known exactly, then ql(i) = q(i) = qu(i);
c
c                  {ql,qu} are required by ema to fit p3 distn to data
c
c            b.  "censoring pattern," which is described by {tl,tu}
c                  {tl,tu} define the interval on which data get observed:
c                  if q is not inside {tl,tu}, q is either left or right 
c                  censored.
c
c                  examples:
c                    {-inf,+inf}  => systematic data (exact value of q
c                      would be measured)
c                    {t,   +inf}  => historical data 
c                      q>=t would be known exactly; 
c                      q<t would be reported as censored ("less-than")
c                
c                    {tl,tu} are required by ema to estimate confidence 
c                      intervals; they are not required for frequency
c                      curve determination
c
c       n.b.  broken record is represented in the time-series data by a
c             threshold value, tl(i), equal to 1.d12 or larger.  This
c             indicates that only flows greater than 1.d12 would have 
c             been recorded.  No such floods have ever occurred on the
c             planet [TAC, 2010, personal communication].
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or 
c                              other)
c            ql(n)      r*8  vector of lower bounds on floods
c            qu(n)      r*8  vector of upper bounds on floods
c            tl(n)      r*8  vector of lower bounds on flood threshold
c            tu(n)      r*8  vector of upper bounds on flood threshold
c
c       n.b.  input data are the same as the first 5 arguments to emafit
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            pe(n)      r*8  estimated exceedance probabilities
c
c       n.b.  output data correspond to every observation that is not identified
c             as broken record.  Censored data are assigned relative plotting
c             positions according to their order in the input time series.
c             
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine plotposHS(n,ql,qu,tl,tu,pe)

      implicit none
      
      integer
     1 n,i,j,n_true,nmax
     
      parameter (nmax=10000)
      
      integer
     1 ix(nmax)
      
      double precision
     1 ql(n),qu(n),tl(n),tu(n),pe(n),infty,
     2 q(nmax),t(nmax),mu,sigma,pp(nmax),xsynth(nmax)
     
      data infty/1.d12/
     
        n_true = 0
      do 10 i=1,n
        if(tl(i) .lt. infty) then
          n_true = n_true + 1
c  for interval data, use average of endpoints to determine order for plotting
          q(n_true) = (ql(i)+qu(i))/2.d0
          t(n_true) = tl(i)
          ix(n_true) = i
        else
          pe(i) = -99.d0
        endif
10    continue

      
      call ppfit(n_true,q,t,mu,sigma,pp,xsynth)
      
      do 20 j=1,n_true
        pe(ix(j)) = 1.d0-pp(j)
20    continue
      
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   test program -- do not include in final version
c       program test_plotposHS
c       
c       implicit double precision (a-h,o-z)
c       
c       double precision lql(100),lqu(100),ltl(100),ltu(100),pp(100)
c       
c       n = 40
c       
c       do 10 i=1,10
c         lql(i) = i+.1
c         lqu(i) = max(8.5,lql(i))
c         ltl(i) = 8.5
c         ltu(i) = 1.d99
c10     continue
c
c       do 20 i=11,20
c         lql(i) = i+.2
c         lqu(i) = max(18.5,lql(i))
c         ltl(i) = 18.5
c         ltu(i) = 1.d99
c20     continue
c         
c       do 30 i=21,30
c         lql(i) = -99
c         lqu(i) = 1.d99
c         ltl(i) = 1.d99
c         ltu(i) = 1.d99
c30     continue
c
c       do 40 i=31,n
c         lql(i) = 4*(i-30)
c         lqu(i) = 4*(i-30)
c         if(i .eq. 35) lqu(i) = lqu(i) + 50
c         ltl(i) = -1.d99
c         ltu(i) = 1.d99
c40     continue
c         
c       call plotposHS(n,lql,lqu,ltl,ltu,pp)
c       
c       do 50 i=1,n
c         write(*,'(i4,5(2x,f10.5))') i,lql(i),lqu(i),ltl(i),ltu(i),
c     1                           pp(i)
c50     continue
c
c       stop
c       end
       


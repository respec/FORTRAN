c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|subroutine emafit
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c    
c    this routine fits the pearson type iii distribution to 
c      a data set using the ema algorithm 
c
c    this was prepared by tim cohn, us geological survey, to support 
c      development of an operational version of peakfq (bulletin 17b
c      implementation) which would include the expected moments
c      algorithm (ema; cohn et al. 1997; 2001)
c       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        09 nov 2003
c       modified            31 dec 2003
c       modified            27 mar 2004  (better small skew approx)
c       modified            07 dec 2004  (typos in comments corrected)
c       modified            07 feb 2007  (major rewrite; reparameterized
c                                         ema in terms of sample moments
c                                         rather than gamma parameters)
c       modified            24 may 2007  changed default pqd values (TAC)
c                                        corrected errors (John England)
c       modified            05 jun 2007  added gbthrsh0 to subroutine call
c                                          permits user to specify lower bound
c       modified            21 jun 2007  corrected error in rmse treatment wrt
c                                          generalized skew
c       modified            13 aug 2007  final low-outlier default procedure
c                                          implemented
c       modified            17 aug 2007  final, final LO default procedure
c                                          implemented
c       modified            25 sep 2007  final 'argh!' w/update to mseg procedure
c                                          to correct for censored data
c                                       
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       n.b.  use of ema requires two distinct types of information:
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
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or 
c                              other)
c            ql(n)      r*8  vector of lower bounds on (log) floods
c            qu(n)      r*8  vector of upper bounds on (log) floods
c            tl(n)      r*8  vector of lower bounds on (log) flood threshold
c            tu(n)      r*8  vector of upper bounds on (log) flood threshold
c            reg_skew   r*8  regional skew
c            reg_mse    r*8  mean square error of regional skew
c                            this variable encodes four distinct cases:
c                            1) reg_mse = 0 ("GENERALIZED SKEW, NO ERROR")
c                                use fixed g = reg_skew w/ mse = 0.0
c                            2) -98 < reg_mse < 0 ("GENERALIZED SKEW, MSE > 0")
c                                use fixed g = reg_skew w/ mse = -reg_mse
c                            3) 0 < reg_mse < 1.d10 ("WEIGHTED SKEW")
c                                use g = weighted average of at-site and 
c                                regional skew (b17b recommendation)
c                            4) 1.d10 < reg_mse ("STATION SKEW")
c                                use g = at-site skew
c
c            gbthrsh0   r*8  critical value for Grubbs-Beck test
c                              N.B. gbthrsh0 codes for 3 cases
c                               1. gbthrsh0 <= -6  ==> Compute GB critical value
c                               2. gbthrsh0 >  -6  ==> gbthrsh0 as crit. val.
c                               3. gbthrsh0 (small, e.g. -5.9) no low out. test
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            cmoms(3,2) r*8  first 3 central moments
c                              using regional info and at-site data in column 1
c                              using just at-site data in column 2
c                              mc = {mean, variance, coeff. skew}
c            pq(17)     r*8  quantiles estimated 
c                            --pq=0.99 corresponds to "100-year flood"
c            yp(17)     r*8  estimated pq-th quantile of fitted p3 distribution
c            ci_low(17) r*8  left end of 95% confidence interval for pq-th 
c                              quantile
c            ci_high(17)r*8  right end of 95% confidence interval for pq-th 
c                              quantile
cprh (09/2009)
c            var_est(*) r*8  variance of estimate for each quantile
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine emafit(n,ql,qu,tl,tu,reg_skew,reg_mse,gbthrsh0,
     1                  cmoms,pq,yp,ci_low,ci_high,var_est)

      implicit none

      integer
     1  i,n,                                                ! input variables
     2  neps,nq
     
Cprh      parameter (nq=15)
C     use full set of probabilities found in PeakFQ
      parameter (nq=32)

      double precision
     1  ql(*),qu(*),tl(*),tu(*),reg_skew,reg_mse,         ! input variables
     2  cmoms(3,2),pq(*),yp(*),ci_low(*),ci_high(*),var_est(*),        ! output variables
     3  pqd(nq),eps(1),gbthrsh0

      data neps/1/,eps/0.95d0/
      
Cprh      data pqd/0.005,0.010,0.050,0.100,0.200,0.3333,0.500,
Cprh     1         0.5708,0.800,0.900,0.960,0.980,0.990,0.995,0.998/
      data  pqd /
     $ 0.9999, 0.9995, 0.9990, 0.9980, 0.9950, 0.9900, 0.9800, 0.9750,
     $ 0.9600, 0.9500, 0.9000, 0.8000, 0.7000, 0.6667, 0.6000, 0.5704, 
     $ 0.5000, 0.4292, 0.4000, 0.3000, 0.2000, 0.1000, 0.0500, 0.0400,
     $ 0.0250, 0.0200, 0.0100, 0.0050, 0.0020, 0.0010, 0.0005, 0.0001/
     
      do 10 i=1,nq
Cprh          pq(i) = pqd(i)
          pq(i) = 1.0 - pqd(i)
        call emafitb(n,ql,qu,tl,tu,reg_skew,reg_mse,neps,eps,
     1                  gbthrsh0,pq(i),
     1                  cmoms,yp(i),ci_low(i),ci_high(i),var_est(i))
10    continue
c
c     correction to adjust for small sample sizes (see tac notes 17 feb 2007)
c
      do 20 i=(nq-1)/2,1,-1
        ci_low(i)  = min(ci_low(i),ci_low(i+1))
        ci_high(i) = min(ci_high(i),ci_high(i+1))
20    continue
      do 30 i=(nq+3)/2,nq
        ci_low(i)  = max(ci_low(i),ci_low(i-1))
        ci_high(i) = max(ci_high(i),ci_high(i-1))
30    continue
     
      return
      end

c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine emafitb
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    the main subroutine
c 
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or 
c                              other)
c            ql(n)      r*8  vector of lower bounds on (log) floods
c            qu(n)      r*8  vector of upper bounds on (log) floods
c            tl(n)      r*8  vector of lower bounds on (log) flood threshold
c            tu(n)      r*8  vector of upper bounds on (log) flood threshold
c            reg_skew   r*8  regional skew
c            reg_mse    r*8  mean square error of regional skew
c                            this variable encodes four distinct cases:
c                            1) reg_mse = 0 ("GENERALIZED SKEW, NO ERROR")
c                                use fixed g = reg_skew w/ mse = 0.0
c                            2) -98 < reg_mse < 0 ("GENERALIZED SKEW, MSE > 0")
c                                use fixed g = reg_skew w/ mse = -reg_mse
c                            3) 0 < reg_mse < 1.d10 ("WEIGHTED SKEW")
c                                use g = weighted average of at-site and 
c                                regional skew (b17b recommendation)
c                            4) 1.d10 < reg_mse ("STATION SKEW")
c                                use g = at-site skew
c            pq         r*8  quantile to be estimated 
c                            --pq=0.99 corresponds to "100-year flood"
c            neps       i*4  number of confidence interval coverages 
c                              (usually equal to neps = 1)
c            eps        r*8  vector of ci coverages (usually just 0.95)
c            gbthrsh0   r*8  critical value for Grubbs-Beck test
c                              (values < -6 result in computed low outlier
c                               test criterion)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            cmoms(3,2) r*8  first 3 central moments 
c                              mc = {mean, variance, coeff. skew}
c            yp         r*8  estimated pq-th quantile of fitted p3 distribution
c            ci_low     r*8  left end of 95% confidence interval for pq-th 
c                              quantile
c            ci_high    r*8  right end of 95% confidence interval for pq-th 
c                              quantile
cprh (09/2009)
c            var_est    r*8  variance of estimate for each quantile
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c

      subroutine emafitb(n,ql_in,qu_in,tl_in,tu_in,reg_skew,reg_mse,
     1                   neps,eps,gbthrsh0,pq,
     2                   cmoms,yp,ci_low,ci_high,var_est)
      
      implicit none
      
      integer 
     1  ntmax,nn,nx
      
      parameter (ntmax=100,nn=100,nx=25000)
      
      integer
     1  n,k,i,nt,neps,nlow,nlow_old,nGBiter,nlow_V
     
      double precision
     1  ql_in(*),qu_in(*),tl_in(*),tu_in(*),reg_skew,reg_mse,pq,  ! input
     2  cmoms(3,2),yp,ci_low(neps),ci_high(neps),var_est(neps),     ! output
     3  yp1,yp2,ci_low1(nn),ci_low2(nn),ci_high1(nn),ci_high2(nn),
     4  skewmin,qP3,parms(3),
     5  ql,qu,tl,tu,
     6  cv_yp_syp(2,2),eps(1),tl2(ntmax),tu2(ntmax),nobs(ntmax),
     7  skew,wt,as_mse,gbthrsh0,gbcrit,gbthresh,
     8  gbcrit_V,gbthresh_V
     
      parameter (skewmin=0.06324555)
      
      double precision
     1  mseg_all
     
      common /tacg01/gbcrit,gbthresh,nlow
      common /tacg02/ql(nx),qu(nx),tl(nx),tu(nx)
      common /tacg03/gbcrit_V(10),gbthresh_V(10),nlow_V(10),nGBiter
      common /jfe001/as_mse

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   1.  check for low outliers
c        a) Identify/recode LOs based on at-site systematic + historical data
c        b) Iterate search for LOs using at-site systematic + historical data
c             combined with regional skew information
c
      do 12 i=1,n
        ql(i) = ql_in(i)
        qu(i) = qu_in(i)
        tl(i) = tl_in(i)
        tu(i) = tu_in(i)
12    continue

c   loop up to 10 times to see if any new LOs uncovered
      do 15 i=1,10
        call gbtest(n,ql,qu,tl,tu,gbthrsh0,
     1              reg_skew,reg_mse,ql,qu,tl,tu)
         gbcrit_V(i)   = gbcrit
         gbthresh_V(i) = gbthresh
         nlow_V(i)     = nlow
       if(nlow .eq. 0) goto 17    ! any new low outliers?
15    continue
            write(*,*) ' Low outlier issues (emafit): nlow = ', nlow,i
            write(*,*) ' User may want to specify threshold'
          stop    ! Should never get here; something is wrong
17    continue
        nGBiter = i  ! added on JRS recomendation

      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   2.  organize data for computing ci and set up the tl and tu vectors
c
      call compress2(n,tl,tu,nt,nobs,tl2,tu2)
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   3.  begin by fitting the p3 distn to the at-site and regional data
c       
c       n.b.  two calls are necessary:
c            a.  compute the at-site moments without employing regional skew
c                   this is used to compute the appropriate weight for regional 
c                   skew (set as_mse=1.0; reg_mse=-99.)
c            b.  final computation using regional skew information
c
      call p3est_ema(n,ql,qu,1.d0,reg_skew,-99.d0,cmoms(1,2))
        as_mse   = mseg_all(nt,nobs,tl2,tu2,cmoms(1,2))
      call p3est_ema(n,ql,qu,as_mse,reg_skew,reg_mse,cmoms(1,1))
        yp       = qP3(pq,cmoms)
        call m2p(cmoms,parms)

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   4.  compute quantiles and confidence intervals
c
      if( abs(cmoms(3,1)) .gt. skewmin) then
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   4.1  for skews far from zero
c
        call var_ema(nt,nobs,tl2,tu2,cmoms,pq,
     1         reg_mse,yp1,cv_yp_syp)
        call ci_ema_m3 
     1    (yp,cv_yp_syp,neps,eps,ci_low,ci_high)
     
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   4.2  for skews close to zero; preserves mean and variance
c
      else                   !  compute a weighted sum/interpolate values
          skew       =  cmoms(3,1)
          cmoms(3,1) = -skewmin  
        call m2p(cmoms,parms)
        call var_ema(nt,nobs,tl2,tu2,cmoms,pq,
     1         reg_mse,yp1,cv_yp_syp)
        call ci_ema_m3
     1    (yp1,cv_yp_syp,neps,eps,ci_low1,ci_high1)

          cmoms(3,1) =  skewmin  
        call m2p(cmoms,parms)
        call var_ema(nt,nobs,tl2,tu2,cmoms,pq,
     1         reg_mse,yp2,cv_yp_syp)
        call ci_ema_m3
     1    (yp2,cv_yp_syp,neps,eps,ci_low2,ci_high2)

            wt = (skew+skewmin)/(2.d0 * skewmin) ! weight to attach to positive skew
c          yp        = (1.d0-wt) * yp1  +  wt * yp2
          cmoms(3,1) = skew

        do 20 i=1,neps
          ci_low(i)  = (1.d0-wt) * ci_low1(i)  +  wt * ci_low2(i)
          ci_high(i) = (1.d0-wt) * ci_high1(i) +  wt * ci_high2(i)
20      continue
      endif
cprh  return variance of estimate (assumes neps=1)
      var_est(1) = cv_yp_syp(1,1)
     
      return
      end

c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine gbtest
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   grubbs-beck test for low outliers
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        07 feb 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    n.b. routine takes input data, uses gb test on systematic data to identify 
c           low outliers, then recodes low outliers as censored values
c           where threshold is set at first above-gb-critical value
c           observation. 
c         routine is not iterative
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (only systematic data)
c            ql_in(n)   r*8  vector of lower bounds on (log) floods
c            qu_in(n)   r*8  vector of upper bounds on (log) floods
c            tl_in(n)   r*8  vector of lower bounds on (log) flood threshold
c            tu_in(n)   r*8  vector of upper bounds on (log) flood threshold
c            gbthrsh0  r*8  critical value for Grubbs-Beck test
c                              (0 or negative values => estimate threshold)
c                              (small value (1e-10) => no low outlier test)
c            as_mse     r*8  mse of at-site skew estimate
c            reg_skew   r*8  regional skew
c            reg_mse    r*8  mean square error of regional skew
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            ql(n)      r*8  vector of lower bounds on (log) floods
c            qu(n)      r*8  vector of upper bounds on (log) floods
c            tl(n)      r*8  vector of lower bounds on (log) flood threshold
c            tu(n)      r*8  vector of upper bounds on (log) flood threshold
c
c    n.b. routine also returns info on low outliers through common block
c
c          common /tacg01/gbcrit,gbthresh,nlow
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine gbtest(n,ql_in,qu_in,tl_in,tu_in,gbthrsh0,
     1   reg_skew,reg_mse,
     2   ql,qu,tl,tu)

      implicit none
      
      integer
     1  n,ns,i,nlow,
     2  nt
     
      double precision
     1  ql_in(*),qu_in(*),tl_in(*),tu_in(*),
     2  ql(*),qu(*),tl(*),tu(*),gbcrit,gbthresh,gbthrsh0,
     3  as_mse,reg_skew,reg_mse,
     4  x(10000),xm,xsum,xss,s,t,cmoms(3),gbtmin,
     5  nobs(10000),tl2(10000),tu2(10000)
     
      double precision
     1  mseg_all

      common /tacg01/gbcrit,gbthresh,nlow
      
      data gbtmin/-6.d0/
!     allows very small positive log(q) values

      
c specified low outlier threshold?
      if(gbthrsh0 .gt. gbtmin) then
        gbthresh  = gbthrsh0        
        gbcrit    = gbthrsh0        
        goto 25
      endif     

c  compute low outlier threshold

        ns   = 0
      do 10 i=1,n
        if(ql_in(i) .eq. qu_in(i) .and. qu_in(i) .gt. gbtmin) then
          ns = ns + 1
          x(ns) = ql_in(i)
        endif
10    continue
      if(ns .le. 5) then
        write(*,*) 'inadequate data for grubbs-beck test',i
        gbcrit   = -99.d0
        gbthresh = -99.d0
      else
          call p3est_ema(n,ql_in,qu_in,1.d0,reg_skew,-99.d0,cmoms)
            call compress2(n,tl_in,tu_in,nt,nobs,tl2,tu2)
            as_mse   = mseg_all(nt,nobs,tl2,tu2,cmoms)
          call p3est_ema(n,ql_in,qu_in,as_mse,reg_skew,reg_mse,cmoms)
          xm      = cmoms(1)
          s       = sqrt(cmoms(2))
          t       = -0.9043+3.345*sqrt(log10(dble(ns))) ! N.B. ns, not n
     1                         -0.4046*log10(dble(ns))  ! Lu formula [JRS]
          gbcrit  = xm - s*t
          gbthresh= 1.d10   ! starting value to find gbthresh
      endif
      
      do 20 i=1,ns
        if(x(i) .gt. gbcrit .and. x(i) .lt. gbthresh) gbthresh=x(i)
20    continue

c  fill in various arrays and compute nlow
25    continue

        nlow = 0
      do 30 i=1,n
        if(qu_in(i) .lt. gbthresh) then
          nlow  = nlow + 1
          qu(i) = gbthresh
c set lower bound to half the observed value
c          ql(i) = qu_in(i)-log(2.d0)
c
c     jfe modify set lower bound to gbtmin (-6.d0)
          ql(i) = gbtmin
        else
          ql(i) = ql_in(i)
          qu(i) = qu_in(i)
        endif
          tl(i) = max(tl_in(i),gbcrit)  ! change made 13 aug 07 (TAC)
          tu(i) = tu_in(i)
30    continue

      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine p3est_ema
c****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     fits pearson type iii distribution to censored data using 
c     an em algorithm and method of moments estimators
c
c     fits binomial-censored data
c
c     author....tim cohn
c     date......february 9, 1994
c
c     modified 2/8/95    tac
c     modified 2/22/95   tac
c     modified 12/16/96  tac  fixed bias-correction factors
c                              and added second convergence criterion
c     modified 11/17/98  tac  conv crit. added
c     modified 03/13/99  tac  removed bias correction factors
c
c****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     modified 10/31/98 tac;  added regional skewness/fixed skewness
c                             capability;  note that call now has two
c                             additional arguments:
c                               reg_skew    is the regional skew
c                               reg_wgt     is the relative weight (0-1) to
c                                           assign regional vs at-site skew
c                                           
c****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or 
c                              other)
c            ql(n)      r*8  vector of lower bounds on (log) floods
c            qu(n)      r*8  vector of upper bounds on (log) floods
c            asmse      r*8  mean square error of at-site skew
c                              this is calculated by mseg
c            reg_skew   r*8  regional skew
c            reg_mse    r*8  mean square error of regional skew
c            pq         r*8  quantile to be estimated 
c                            --pq=0.99 corresponds to "100-year flood"
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables: 
c       ------------------------------------------------------------------------
c
c            cmoms(3)   r*8  first 3 central moments 
c                              mc = {mean, variance, coeff. skew}
c            yp         r*8  estimated pq-th quantile of fitted p3 distribution
c            ci_low     r*8  left end of 95% confidence interval for pq-th 
c                              quantile
c            ci_high    r*8  right end of 95% confidence interval for pq-th 
c                              quantile
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     note: ql(i) is the lower bound for the i-th observation
c           qu(i) is the upper bound
c            if( qu == ql) then we have an ordinary observation at qu
c
c
      subroutine p3est_ema(n,ql,qu,as_mse,reg_skew,reg_mse,moms_out)

      implicit none
      
      integer
     1  n,nsize,i,j,k
      
      common /reg001/rskew,rmse
      
      parameter (nsize=20001)
      
      double precision 
     1     ql(*),qu(*),as_mse,reg_skew,reg_mse,moms_out(3),
     2     moms(3,nsize),d11(nsize),dist_p3,tol,rskew,rmse,asmse
     
      double precision
     1     momsadj
      
      data tol/1.d-10/ 
      data moms(1,1),moms(2,1),moms(3,1)/0.0, 1.0, 0.0/

            asmse = as_mse
            rskew = reg_skew
            rmse  = reg_mse
c            if(rmse .lt. 0.d0) rmse = 1.0d30
      
      do 10 i=2,nsize
        call moms_p3(n,ql,qu,moms(1,i-1),moms(1,i))
          if(rmse .le. 0.d0 .and. rmse .gt. -98.d0) then ! "GENERALIZED, NO MSE"
            moms(3,i) = rskew
          else if (rmse .gt. 0.d0) then                  ! "WEIGHTED"
            moms(3,i) = (rskew*asmse+moms(3,i)*rmse)/(rmse+asmse)
          else if (rmse .lt. -98.d0) then                ! "STATION SKEW
c            moms(3,i) = moms(3,i)
          else if (rmse .ge. 1.d10) then                 ! "STATION SKEW
c            moms(3,i) = moms(3,i)
          endif
          
          moms(3,i) = momsadj(n,ql,moms(1,i))   ! correct

        if(i .gt. 1000) then  !  eliminate cycles of length 2 or 3
          do 50 j=1,3
            write(*,*) '(EMA): Convergence criteria not met, i = ',i
            write(*,*) ' -- Cycling likely: Correction implemented'
            write(*,'(a10,3f10.3)') ' -- M_{i}  ',moms(1,i-1)
            write(*,'(a10,3f10.3)') ' -- M_{i}  ',moms(1,i-1)            
            moms(j,i) = (moms(j,i)  +moms(j,i-1)+moms(j,i-2)+
     1                   moms(j,i-3)+moms(j,i-4)+moms(j,i-5))/6.d0
50        continue
        endif
c
          d11(i) =  dist_p3(moms(1,i-1),moms(1,i))
            if(d11(i) .le. tol) then
               do 20 k=1,3
                 moms_out(k) = moms(k,i)
20             continue
              return
            endif
10    continue
            write(*,*) ' failure to converge in p3_mom'
            do 30 i=1,10
              write(*,*) i,(moms(j,i),j=1,3),d11(i)
30          continue
            do 40 i=nsize-10,nsize
              write(*,*) i,(moms(j,i),j=1,3),d11(i)
40          continue
            stop
      end
 
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine moms_p3
c****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     computes the expected value of a g3 variate with parameters
c     given by mc_old(), whose value is known to lie in the interval (ql,qu)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history (see above)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or 
c                              other)
c            ql(n)      r*8  vector of lower bounds on (log) floods
c            qu(n)      r*8  vector of upper bounds on (log) floods
c            mc_old(3)  r*8  vector of p3 parameters
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            moms(3)    r*8  vector of updated p3 parameters
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine moms_p3(n,ql,qu,mc_old,moms)

      implicit none

      integer 
     1  n,nsize,
     2  n_e,n_c,bcf,i,j,k
     
      parameter (nsize=25000)
      
      double precision
     1     ql(*),qu(*),mc_old(3),moms(3),s_e(3),s_c(3),
     2     m_nc_moms(3,nsize),
     3     sum,c2,c3,n_bcf,
     4     choose

c     note that setting bcf to 0 shuts off bias correction;
c       1997 gets the original Cohn et al. [1997] bcfs
c       2004 gets Griffis et al. [2004] -- the default!
      data bcf/2004/

      if(n .gt. nsize) then
        write(*,*) '(moms_p3): sample size too large ',n
        stop
      endif
      
        n_e         = 0
        moms(1)     = 0.d0
      do 10 i=1,n
        if(ql(i) .eq. qu(i)) n_e = n_e+1
        if((i .gt. 1) .and. (ql(i) .eq. ql(i-1)) .and. 
     1    (qu(i) .eq. qu(i-1)) ) then
            m_nc_moms(1,i) = m_nc_moms(1,i-1)
            m_nc_moms(2,i) = m_nc_moms(2,i-1)
            m_nc_moms(3,i) = m_nc_moms(3,i-1)
        else
          call mP3(ql(i),qu(i),mc_old,m_nc_moms(1,i),3)
        endif
          moms(1) = moms(1)+m_nc_moms(1,i)
10    continue
        moms(1) = moms(1)/n
        n_c     = n - n_e  
      do 30 j=2,3
          s_e(j)      =     0.d0
          s_c(j)      =     0.d0
        do 40 i=1,n
          if(qu(i) .eq. ql(i)) then
            s_e(j)    = s_e(j) + ( ql(i) - moms(1) )**j
          else
               sum = m_nc_moms(j,i) +  (-moms(1))**j
             do 50 k=1,j-1
               sum = sum + choose(j,k)*m_nc_moms(k,i)*(-moms(1))**(j-k)
50          continue
               s_c(j)  =   s_c(j) + sum
          endif             
40    continue 
30    continue
c
c     jfe modify to handle all bcf input cases
      if(bcf .eq. 1997) then
        n_bcf = n_e
        c2          =  n_bcf/(n_bcf-1.d0)
        c3          =  ( n_bcf**2)/( (n_bcf-1.d0)*(n_bcf-2.d0) )
      else if (bcf .eq. 2004) then
        n_bcf = n
        c2          =  n_bcf/(n_bcf-1.d0)
        c3          =  ( n_bcf**2)/( (n_bcf-1.d0)*(n_bcf-2.d0) )
      else  ! no bias correction, all other entered bcf values
        n_bcf = 0.d0
        c2          =  1.d0
        c3          =  1.d0
      endif
        moms(2)     =  ( c2*s_e(2) + s_c(2) )/n
        moms(3)     =  ( c3*s_e(3) + s_c(3) )/( n * moms(2)**1.5d0)
        
      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|double precision function dist_p3
c****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     computes "distance" between two p3 parameter sets (converged?)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history (see above)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            m1(3)      r*8  vector of p3 parameters
c            m2(3)      r*8  vector of p3 parameters
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            dist_p3    r*8  distance between two parameter sets
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision function dist_p3(m1,m2)

      implicit none

      integer 
     1  i,ip(3)
     
      double precision
     1  m1(3),m2(3),sum

      data ip/1,2,0/

            sum      =  0.d0
      do 10 i=1,3
            sum      =  sum + (m1(i)-m2(i))**2/m2(2)**ip(i)/10.**(i-1)
10    continue
            dist_p3  =  sum
            
      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|double precision function momsadj
c****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     this routine adjusts the skew to ensure two things:
c 
c     1.  that the ema algorithm converges; for negative skews, with
c         left-censored data, there are cases where ema diverges without
c         this constraint
c
c     2.  that the largest observation in the data set is within the 
c         support of the fitted distribution
c
      double precision function momsadj(n,ql,m)

      implicit none
      
      integer
     1  n,i
     
      double precision
     1  ql(*),m(3),xmax,parms(3),
     2  sk0,sk141,skxmax
     
      common /tac002/sk0,sk141,skxmax

        xmax = ql(1)
      do 10 i=2,n
        if(ql(i) .gt. xmax) xmax = ql(i)
10    continue
        call m2p(m,parms)
        
        sk0     = m(3)
        sk141   = -1.41d0                           ! 1: m(3) < -1.41
c       N.B. Following line corrected by JFE (TAC, 24 may 2007)
c        skxmax  = 2.d0/sqrt(m(2))/(m(1)-xmax)       ! 2:  parms(1) < xmax
        skxmax  = 2.d0*sqrt(m(2))/(m(1)-xmax)       ! 2:  parms(1) < xmax
        momsadj = max(sk0,sk141,skxmax)
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|double precision function mseg_all
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     computes the mse of station skew for the bulletin 17b analysis (see p. 13)
c
c     n.b.  b17b recommends using h -- the entire period length --
c           for the record length with historical information.
c
c     timothy a. cohn, 2007
c
c     *** do not modify without author''s consent ***
c
c           author.......tim cohn
c           date.........18 sep 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            nthresh      i*4  number of distinct censoring thresholds 
c                                ( (a,b) pairs)
c            nobs(*)      r*8  vector of number of observations corresponding 
c                                to threshold pair (tl(i),tu(i))
c            tl(*)        r*8  vector of lower bounds (a)
c            tu(*)        r*8  vector of upper bounds (b)
c            mc(3)        r*8  vector of estimated lp3 moments
c
c       output variables:
c       ------------------------------------------------------------------------
c            mseg_all     r*8  mse of at-site (station) skew
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
       double precision function mseg_all(nthresh,nobs,tl,tu,mc)

      implicit none
      
      double precision
     1    nobs(*),tl(*),tu(*),mc(3),mc2(3),tl2(100),tu2(100),
     2    s_mn(3,3),s_mc(3,3),mn(3),
     3    skewmin,tneg,tpos,w

      parameter (skewmin=0.06324555)

      integer 
     1    nthresh,i
     
c   begin by pseudo-orthogalizing the parameters
        mc2(1) = 0.d0
        mc2(2) = mc(2)
        mc2(3) = mc(3)
      do 10 i=1,nthresh
        tl2(i) = tl(i) - mc(1)
        tu2(i) = tu(i) - mc(1)
10    continue
      
      if(abs(mc2(3)) .gt. skewmin) then !  straight computation
        call var_mom(nthresh,nobs,tl2,tu2,mc2,s_mn)
        call m2mn(mc2,mn)
        call mn2m_var(mn,s_mn,mc2,s_mc)
        mseg_all = s_mc(3,3)
      else  !  use weighted sum for skew = skewmin, -skewmin
        mc2(3) = -skewmin
        call var_mom(nthresh,nobs,tl2,tu2,mc2,s_mn)
        call m2mn(mc2,mn)
        call mn2m_var(mn,s_mn,mc2,s_mc)
        tneg = s_mc(3,3)
        mc2(3) = skewmin
        call var_mom(nthresh,nobs,tl2,tu2,mc2,s_mn)
        call m2mn(mc2,mn)
        call mn2m_var(mn,s_mn,mc2,s_mc)
        tpos = s_mc(3,3)
        w    = (mc(3)-skewmin)/(2.d0*skewmin)
        mseg_all = w*tneg + (1.d0-w)*tpos
      endif

      
      return
      end

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|double precision function mseg
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     computes the station skew for the bulletin 17b analysis (see p. 13)
c
c     n.b.  b17b recommends using h -- the entire period length --
c           for the record length with historical information.
c
c     copyright, timothy a. cohn, 2000
c
c     *** do not modify without author''s consent ***
c
c           author.......tim cohn
c           date.........11 june 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n         i*4  record length
c            g         r*8  station skew
c
c       output variables:
c       ------------------------------------------------------------------------
c            mseg      r*8  mse of at-site (station) skew
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
       double precision function mseg(n,g)

      implicit none
      
      double precision g,a,b
      integer n
      
        if(abs(g) .le. 0.9) then
         a = -0.33 + 0.08*abs(g)
      else
         a = -0.52 + 0.3 * abs(g)
      endif
      if(abs(g) .le. 1.5) then
         b =  0.94 - 0.26 * abs(g)
      else
         b =  0.55
      endif
         mseg  =  10**(a - b * log(n/10.d0)/log(10.d0) )
      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine ci_ema_m3
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    m03 is like m02 but uses students t distribution in 
c        calculation of confidence intervals
c        these are the "adjusted" ci-s in cohn, lane and stedinger (wrr 2001)
c
c      date..........8 sept 1999 (tac)
c
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            yp         r*8  estimated p-th quantile
c            cv_yp_syp  r*8  estimated cov[yp,syp] (2x2 matrix)
c                              n.b. syp is sqrt(cvypsyp(1,1))
c                                   s_syp is sqrt(cvypsyp(2,2))
c            neps       i    number of quantile pairs to compute
c            eps(*)     r*8  vector of ci coverages (usually 0.90,0.99,0.999)
c
c
c       output variables:
c       ------------------------------------------------------------------------
c            ci_low(*)  r*8  estimated lower critical point of confidence 
c                              interval
c            ci_high(*) r*8  estimated upper critical point of confidence 
c                              interval
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine ci_ema_m3
     1    (yp,cv_yp_syp,neps,eps,ci_low,ci_high)
     
        implicit none
          save

        integer 
     1     i,neps
     
        double precision
     1    yp,cv_yp_syp(2,2),eps(*),ci_high(*),ci_low(*),
     4    beta1,c_min,nu,nu_min,p_high,t,var_xsi_d

        double precision
     1    fp_tnc_icdf
     
        data nu_min/0.5d0/,c_min/0.5d0/

c
c    beta1 is coefficient of regression of syp on yp
c
          beta1       =  cv_yp_syp(1,2)/cv_yp_syp(1,1)
c
c    note computation of nu involves not var[syp], but var[syp-beta1*yp]
c
        var_xsi_d   =  cv_yp_syp(2,2) -
     1                     cv_yp_syp(1,2)**2/cv_yp_syp(1,1)
        nu          =  0.5d0 * cv_yp_syp(1,1)/var_xsi_d
c
c    prevent numerical problems for very small nu (does this arise in practice?)
c
          if(nu .le. nu_min) then
            write(*,*) 'warning: ci_ema_m3'
            write(*,*) '  estimated nu too small',nu
            nu = nu_min
            write(*,*) '  value replaced with   ',nu
        endif

c
c    compute confidence intervals
c    n.b.  sign on t means that low t corresponds to high yp
c

      do 10 i=1,neps
         p_high   =  (1.d0+eps(i))/2.d0
         t        =  fp_tnc_icdf(p_high,nu,0.d0)
       ci_high(i) =  yp + sqrt(cv_yp_syp(1,1))*t/max(c_min,1.d0-beta1*t)
         t        =  -t
       ci_low(i)  =  yp + sqrt(cv_yp_syp(1,1))*t/max(c_min,1.d0-beta1*t)
10      continue
        return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine var_ema
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       given estimated parameters (tau, alpha, beta) based on 
c         censored pearson type 3 data, this program will:
c
c          1.   yp    -- the estimated pth quantile of a pearson type 3 variate
c          2.   syp   -- the estimated standard deviation of yp
c          3.   s_syp -- the standard deviation of the standard deviation of yp
c          4.   cv    -- a (1-epsilon) confidence interval for yp
c
c       n.b.  these are all asymptotic results, but seem to be excellent 
c             approximations in small samples. 
c
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        1 july 2000
c       modified            07 feb 2007  (major rewrite)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            nthresh      i*4  number of distinct censoring thresholds 
c                                ( (a,b) pairs)
c            nobs(*)      r*8  vector of number of observations corresponding 
c                                to threshold
c                                pair (a(i),b(i)
c            tl_in(*)     r*8  vector of lower bounds (a)
c            tu_in(*)     r*8  vector of upper bounds (b)
c            mc_in(3)     r*8  vector of estimated lp3 moments
c            pq           r*8  quantile to be estimated
c            g_r_mse      r*8  mse of g_r (b17) ; 
c                            this variable encodes four distinct cases:
c                            1) g_r_mse = 0 ("GENERALIZED SKEW, NO ERROR")
c                                use fixed g = reg_skew w/ mse = 0.0
c                            2) -98 < g_r_mse < 0 ("GENERALIZED SKEW, MSE > 0")
c                                use fixed g = reg_skew w/ mse = -reg_mse
c                            3) 0 < g_r_mse < 1.d10 ("WEIGHTED SKEW")
c                                use g = weighted average of at-site and 
c                                regional skew (b17b recommendation)
c                            4) 1.d10 < g_r_mse ("STATION SKEW")
c                                use g = at-site skew
c
c            n.b.:  g_r, the regional skewness, 
c                          is set = sign(2,parms(3))/sqrt(parms(2))
c
c       output variables:
c       ------------------------------------------------------------------------
c            yp         r*8  estimated p-th quantile
c            cv_yp_syp  r*8  estimated cov[yp,syp]
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine var_ema
     1    (nthresh,nobs,tl_in,tu_in,mc_in,pq,g_r_mse,yp,cv_yp_syp)
     
        implicit none
          save

        integer 
     1     nthresh,
     2     i,n,nth_p
     
        parameter (nth_p=100)
     
        double precision
     1    nobs(nth_p),tl_in(nth_p),tu_in(nth_p),pq,
     2    g_r_mse,yp,cv_yp_syp(2,2),
     3    mc(3),mc_in(3),parms(3),
     4    tl(nth_p),tu(nth_p),
     4    skewmin,s_mc(3,3),
     4    tmp(2,3),jac(3,2)

        double precision
     1    qP3
     
        data skewmin/0.01d0/

        yp       =   qP3(pq,mc_in)
  
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    orthogonalize (sort of) the first three moments
c      n.b.  note 4-th parameter:  generalized skew (!)
c      why not?  the matrix algebra works, and everything is linear
c
        
        mc(1) = 0.d0
        mc(2) = mc_in(2)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    check to make sure that alpha not too large
c
        if(abs(mc_in(3)) .gt. skewmin) then
          mc(3) = mc_in(3)
        else
          mc(3) = sign(1.d0,mc_in(3))*skewmin
        endif
                
        call m2p(mc,parms)

          n      =   0
        do 20 i=1,nthresh
          n      =   n + nobs(i)
          tl(i)  =   tl_in(i) - mc_in(1)
          tu(i)  =   tu_in(i) - mc_in(1)
20      continue
          nobs(nthresh+1) = n

       
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    compute covariance matrix
c

      call regskew(nthresh,nobs,tl,tu,mc,g_r_mse,s_mc)
      
      call jacq2(pq,mc,jac(1,1))

      call jacs2(nthresh,nobs,tl,tu,mc,g_r_mse,pq,jac(1,2))

      call dmxtyf(3,2,jac,3,3,3,s_mc,3,2,3,tmp,2)

      call dmrrrr(2,3,tmp,2,3,2,jac,3,2,2,cv_yp_syp,2) 

      return
      end

c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine regskew(nthresh,nobs,tl,tu,mc,g_r_mse,s_mc)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     *** do not modify without authors consent ***
c
c           author.......tim cohn
c           date.........10 feb 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------
c            nthresh    i*4  number of distinct censoring thresholds
c            nobs       r*8  number of observations corresponding to 
c                              each threshold 
c            tl(*)      r*8  vector of lower bounds (a) of censoring
c            tu(*)      r*8  vector of upper bounds (b) of censoring
c            mc         r*8  vector of parameters (first 3 central 
c                              moments of lp3 distribution
c            g_r_mse    r*8  scalar mse of regional skewness estimator 
c                            N.B.  the mse codes for four distinct cases
c                              1) mse>0 => weighted skew
c                              2) mse=0 => generalized skew (no uncertainty)
c                              3) -98< mse <0 => generalized skew (mse = -mse) 
c                              4) mse < -98 => station skew 
c                                   (using g_r_mse = 1.d10 is equivalent)
c
c       output variables:
c       ---------------------------------------------------------------
c            s_mc(3,3)  r*8  matrix of covariance of 
c                              n.b: central parameter estimators
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      subroutine regskew(nthresh,nobs,tl,tu,mc,g_r_mse,s_mc)
      
      implicit none
      
      integer
     1  nthresh
      
      double precision
     1  nobs(*),tl(*),tu(*),mc(3),g_r_mse,s_mc(3,3),
     2  s_mn(3,3),mn(3),w
     
      double precision mseg_all
     
      call var_mom(nthresh,nobs,tl,tu,mc,s_mn)
      
      call m2mn(mc,mn)
      
      call mn2m_var(mn,s_mn,mc,s_mc)
      
        if(g_r_mse .gt. 0.d0) then   ! "WEIGHTED"
           w = g_r_mse/(mseg_all(nthresh,nobs,tl,tu,mc) + g_r_mse)
        else if(g_r_mse .ge. -98.d0 .and. g_r_mse .le. 0.d0) then ! "G-w/skew"
          w =  0.d0
        else if(g_r_mse .lt. -98.d0) then ! "STATION"
          w =  1.d0
        else if(g_r_mse .ge. 1.d10) then  ! "STATION"
          w =  1.d0
        endif
      
        s_mc(1,3) = w*s_mc(1,3)
        s_mc(3,1) = s_mc(1,3)
        s_mc(2,3) = w*s_mc(2,3)
        s_mc(3,2) = s_mc(2,3)
        s_mc(3,3) = w**2*s_mc(3,3) + (1.d0-w)**2*abs(g_r_mse)

      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine jacq2(q,mc,jac)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     *** do not modify without authors consent ***
c
c           author.......tim cohn
c           date.........10 feb 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ----------------------------------------------------------------
c            q          r*8  quantile to be estimated (in range (0-1)
c            mc         r*8  vector of parameters (first 3 central 
c                              moments of lp3 distribution
c
c       output variables:
c       ---------------------------------------------------------------
c            ja(3)      r*8  jacobian of quantile wrt mc 
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine jacq2(q,mc,jac)

      implicit none
        
      integer 
     1  ifail,ind
      
      double precision
     1  q,mc(3),jac(3),
     2  mc2,q2,error,qP3
     
      external fjac

      common /fjac01/mc2(3),q2,ind

c  derivative of quantile wrt location parameter      
        jac(1)= 1.d0
c  derivative of quantile wrt scale parameter is ks     
       mc2(1) = 0.d0
       mc2(2) = 1.d0
       mc2(3) = mc(3)
        jac(2)= qP3(q,mc2)/(2.d0*sqrt(mc(2)))
c  derivative of quantile wrt shape parameter is computed numerically     
       mc2(1) = mc(1)
       mc2(2) = mc(2)
        q2    = q      
      do 10 ind=3,3
      call diff(1,0.d0,-0.1d0,0.1d0,fjac,0.d0,0.d0,jac(ind),error,ifail)
10    continue
      return
      end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      double precision function fjac(x)
      implicit none
      integer ind
      double precision x,mc,mc2(3),q,qP3
      common /fjac01/mc(3),q,ind
        mc2(1) = mc(1)
        mc2(2) = mc(2)
        mc2(3) = mc(3)
        mc2(ind) = mc2(ind)+x
      fjac = qP3(q,mc2)
      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|double precision function sypf(nthresh,nobs,tl,tu,mc,g_r_mse,q)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........10 feb 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------
c            nthresh    i*4  number of distinct censoring thresholds
c            nobs       r*8  number of observations corresponding to 
c                              each threshold 
c            tl(*)      r*8  vector of lower bounds (a) of censoring
c            tu(*)      r*8  vector of upper bounds (b) of censoring
c            mc         r*8  vector of parameters (first 3 central 
c                              moments of lp3 distribution
c            g_r_mse    r*8  scalar mse of regional skewness estimator 
c                              (see comments above)
c            q          r*8  quantile to be estimated (in range (0-1)
c
c       output variables:
c       ---------------------------------------------------------------
c            result     r*8  estimated standard deviation of y-hat-q
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision function sypf(nthresh,nobs,tl,tu,mc,g_r_mse,q)
      
      implicit none
      
      integer
     1  nthresh
      
      double precision
     1  nobs(*),tl(*),tu(*),mc(3),g_r_mse,q,s_mc(3,3),
     2  jac(3),tmp(3),result
          
      call regskew(nthresh,nobs,tl,tu,mc,g_r_mse,s_mc)
      
      call jacq2(q,mc,jac)
      
      call dmxtyf(3,1,jac,3,3,3,s_mc,3,1,3,tmp,1)
      call dmrrrr(1,3,tmp,1,3,1,jac,3,1,1,result,1) 
        sypf = sqrt(result)
ctac*
        if(result .le. 0.d0) then
          write(*,*) '============'
          write(*,*) 'sypf:', result,sqrt(result)
          write(*,*) 'mc',mc
          write(*,*) 'tl',tl(1),tl(2)
          write(*,*) 'tu',tu(1),tu(2)
          write(*,*) 'rmse',g_r_mse
          write(*,*) 'q',q
          write(*,*) 'jacq2',jac
          write(*,*) 's_mc ',s_mc(1,1),s_mc(1,2),s_mc(1,3)
          write(*,*) 's_mc ',s_mc(2,1),s_mc(2,2),s_mc(2,3)
          write(*,*) 's_mc ',s_mc(3,1),s_mc(3,2),s_mc(3,3)
          write(*,*) '============'
        endif
ctac*
        
      return
      end
        
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine jacs2
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........10 feb 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------
c            nthresh    i*4  number of distinct censoring thresholds
c            nobs(*)    r*8  number of observations corresponding to 
c                              each threshold 
c            tl(*)      r*8  vector of lower bounds (a) of censoring
c            tu(*)      r*8  vector of upper bounds (b) of censoring
c            mc(*)      r*8  vector of parameters (first 3 central 
c                              moments of lp3 distribution
c            g_r_mse    r*8  scalar mse of regional skewness estimator 
c            q          r*8  quantile to be estimated (in range (0-1)
c
c       output variables:
c       ---------------------------------------------------------------
c            jacss(3)   r*8  jacobian of syp wrt (m,s^2,g)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine jacs2(nthresh,nobs,tl,tu,mc,g_r_mse,q,jacss)

      implicit none
      
      integer
     1  nthresh,i
      
      double precision
     1  nobs(*),tl(*),tu(*),mc(3),g_r_mse,q,jacss(3),
     3  sypf,dgv(3),delta,mc_l(3),mc_u(3),d
     
      data dgv/1.d0,2.d0,6.d0/,d/0.05d0/
      
      do 10 i=1,3
        delta = d*sqrt(dgv(i)*mc(2)/nobs(nthresh+1))
        call mcadj(mc,i,delta,mc_l,mc_u)
        jacss(i) = (sypf(nthresh,nobs,tl,tu,mc_u,g_r_mse,q) -
     1              sypf(nthresh,nobs,tl,tu,mc_l,g_r_mse,q) )/
     1              (2.d0*delta)
10    continue
      return
      end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      subroutine mcadj(m,i,d,mcl,mcu)
      integer i,j
      double precision m(3),mcl(3),mcu(3),d
      do 10 j=1,3
        if(i .eq. j) then
          mcu(j) = m(j)+d
          mcl(j) = m(j)-d
        else
          mcu(j) = m(j)
          mcl(j) = m(j)
        endif
10    continue
      return
      end
          
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine var_mom
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c 
c     program to compute variance of ema estimator
c
c     author.....tim cohn
c     date.......4 april 1999
c      modified..7 june 2000 (tac) --regional skew added
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine var_mom
     1    (nthresh,n_in,tl_in,tu_in,mc,varm)

        implicit none
          save

        integer 
     1    nthresh,
     2    i,it,j,nth_p
     
        parameter (nth_p=100)

        double precision 
     1    tl_in(nth_p),tu_in(nth_p),mc(3),
     2    mnouta(6),mnoutb(6),mnoutc(6),n_in(nth_p),
     3    varm(3,3),a(3,3),ainv(3,3),bc_t(3,3),d(3,3),d_t(3,3),e_x(6),
     4    xinf,mu_x(3,3),nh,n_t,t1(3,3),tl,tu,p1,p2,p3,pa,pb,
     5    vb(3,3),vb_t(3,3),vc(3,3),vc_t(3,3)
     
        double precision
     1    pP3
     
        data xinf/999.d0/
     
            call dset(9,0.d0,vb_t,1)
            call dset(9,0.d0,vc_t,1)
            call dset(9,0.d0,d_t,1)
c
            n_t = 0.d0
c

        do 10 it = 1,nthresh
c
            nh  = n_in(it)
            n_t = n_t + nh
c
c     pa, pb are non-exceedance probabilities
c     p1,p2,p3 are probabilities that x<a, a<x<b, x>b
c
            tl = tl_in(it)
            tu = tu_in(it)

            pa = pP3(tl,mc)
            pb = pP3(tu,mc)
          
            p1 = pa
            p2 = pb - pa
            p3 = 1.d0 - pb
          
c
            call mP3(-xinf,tl,mc,mnouta,6)
            call mP3(tu,xinf,mc,mnoutb,6)
            call mP3(tl,tu,mc,mnoutc,6)
          do 20 j=1,3
            e_x(j)    = mnoutc(j)
            e_x(j+3)  = mnoutc(j+3)
c
            mu_x(j,1) = mnouta(j)
            mu_x(j,2) = e_x(j)
            mu_x(j,3) = mnoutb(j)
20        continue

        call varb(mu_x,nh,p1,p2,p3,vb)
          call dmsum(3,3,vb,3,3,3,vb_t,3,3,3,vb_t,3)
        
        call varc(e_x,nh,p2,vc)
          call dmsum(3,3,vc,3,3,3,vc_t,3,3,3,vc_t,3)

        call d_est(nh,mc,pa,pb,d)

          call dmsum(3,3,d,3,3,3,d_t,3,3,3,d_t,3)

10      continue
        
        do 30 i=1,3
          do 30 j=1,3
            if(i .eq. j) then
                ainv(i,j) = 1.d0 - d_t(i,j)/n_t
            else
                ainv(i,j) = -d_t(i,j)/n_t
            endif
30      continue
        call dlinrg(3,ainv,3,a,3)
      
        call dmsum(3,3,vb_t,3,3,3,vc_t,3,3,3,bc_t,3)
        call dmrrrr(3,3,a,3,3,3,bc_t,3,3,3,t1,3)
        call dmxytf(3,3,t1,3,3,3,a,3,3,3,varm,3)
        
        call dmmult(n_t**(-2),3,3,varm,3,3,3,varm,3)
      
        return
        end
        
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine varb
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine varb(mu_x,nh,p1,p2,p3,vb)

        implicit none
          save
      
        double precision 
     1    mu_x(3,3),nh,p1,p2,p3,vb(3,3),
     2    t1(3,3),vn(3,3)

        vn(1,1) =  nh*p1*(1.d0-p1)
        vn(1,2) = -nh*p1*p2
        vn(1,3) = -nh*p1*p3
        vn(2,1) =  vn(1,2)
        vn(2,2) =  nh*p2*(1.d0-p2)
        vn(2,3) = -nh*p2*p3
        vn(3,1) =  vn(1,3)
        vn(3,2) =  vn(2,3)
        vn(3,3) =  nh*p3*(1.d0-p3)

        call dmrrrr(3,3,mu_x,3,3,3,vn,3,3,3,t1,3)
        call dmxytf(3,3,t1,3,3,3,mu_x,3,3,3,vb,3)

        return
        end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine varc
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine varc(e_x,nh,p2,vc)

        implicit none
          save
      
      integer 
     1    i,j
        double precision 
     1    e_x(6),nh,p2,vc(3,3),
     2    mu_n

          mu_n = p2*nh
        do 10 i=1,3
          do 10 j=1,3
            if(i .gt. j) then
              vc(i,j) = vc(j,i)
            else
              vc(i,j) = mu_n*(e_x(i+j) - e_x(i)*e_x(j))
          endif
10      continue

        return
        end
        
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine d_est
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine d_est(nh,mc,pa,pb,d)

        implicit none
          save
      
        double precision 
     1    mc(3),nh,parms(3),pa,pb,d(3,3),
     2    en1,en3,inf,jacg(3,3),jacl(3,3),pmin,p1,p3,t1,t3

        double precision 
     1    qP3
     
        data inf/1.d19/,pmin/1.d-6/

        call m2p(mc,parms)
        p1 = pa
        p3 = 1.d0 - pb
        
        if(p1 .gt. pmin) then
            t1 = qP3(pa,mc)
          call expmomderiv(parms,-inf,t1,jacl)
          en1 = nh*p1
          call dmmult(en1,3,3,jacl,3,3,3,jacl,3)
        else
          call dset(9,0.d0,jacl,1)
        endif

        if(p3 .gt. pmin) then
            t3 = qP3(pb,mc)
          call expmomderiv(parms,t3,inf,jacg)
          en3 = nh*p3
          call dmmult(en3,3,3,jacg,3,3,3,jacg,3)
        else
          call dset(9,0.d0,jacg,1)
        endif

          call dmsum(3,3,jacl,3,3,3,jacg,3,3,3,d,3)      

        return
        end

c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine mn2m_var(mn,s_mn,mc,s_mc)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to convert variance in mn (non-central moments)
c        into variance in m (central moments)
c
c     author.....tim cohn
c     date.......12 june 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c           mn           =   non-central moments
c           s_mn(3,3)    =   variance-covariance matrix of mn
c           mc           =   central moments
c           s_mc(3,3)    =   variance-covariance matrix of mc
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine mn2m_var(mn,s_mn,mc,s_mc)

      implicit none
        save

      double precision
     1    m1,m2,m3,mn(3),s_mn(3,3),mc(3),s_mc(3,3),
     2    df(3,3),temp(3,3)
     
c
c    1.a  compute central moments from non-central moments
c

      m1    =  mn(1)
      m2    =  mn(2)
      m3    =  mn(3)
      
        call mn2m(mn,mc)
c
c    1.b compute jacobian of central moments with respect to non-central
c            moments
c            n.b. df(2,1) = d(mc[2])/d(m[1])
c
        df(1,1)  =  1.d0
        df(1,2)  =  0.d0
        df(1,3)  =  0.d0

        df(2,1)  =  -2.d0*m1
        df(2,2)  =  1.d0
        df(2,3)  =  0.d0

        df(3,1)  =  (6.d0*m1**2 - 3*m2)/(-m1**2 + m2)**1.5 + 
     1              (3.d0*m1*(2.d0*m1**3 - 3.d0*m1*m2 + m3))/
     2              (-m1**2 + m2)**2.5 
        df(3,2)  =  (-3.d0*m1)/(-m1**2 + m2)**1.5 - 
     1              (3.d0*(2.d0*m1**3 - 3.d0*m1*m2 + m3))/
     2              (2.d0*(-m1**2 + m2)**2.5)
        df(3,3)  =  (-m1**2 + m2)**(-1.5)

c
c    1.c compute asymptotic covariance of central moments
c          df.s_m.df
c
       call dmrrrr(3,3,df,3,3,3,s_mn,3,3,3,temp,3)
       call dmxytf(3,3,temp,3,3,3,df,3,3,3,s_mc,3)

       return
       end
       
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine m2mn_var
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to convert variance in mn (non-central moments)
c        into variance in m (central moments)
c
c     author.....tim cohn
c     date.......12 june 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c           mc           =   central moments
c           s_mc(3,3)    =   variance-covariance matrix of mc
c           mn           =   non-central moments
c           s_mn(3,3)    =   variance-covariance matrix of mn
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine m2mn_var(mc,s_mc,mn,s_mn)

      implicit none
        save

      double precision
     1    mc1,mc2,mc3,mn(3),s_mn(3,3),mc(3),s_mc(3,3),
     2    dr(3,3),temp(3,3)
     
      mc1    =  mc(1)
      mc2    =  mc(2)
      mc3    =  mc(3)
      
        call m2mn(mc,mn)
c
c    4.a compute jacobian of non central moments with respect to central
c            moments
c

        dr(1,1)  =  1.d0
        dr(1,2)  =  0.d0
        dr(1,3)  =  0.d0

        dr(2,1)  =  2.d0*mc1
        dr(2,2)  =  1.d0
        dr(2,3)  =  0.d0

        dr(3,1)  =  3.d0*mc1**2 + 3.d0*mc2
        dr(3,2)  =  3.d0*mc1 + (3.d0*sqrt(mc2)*mc3)/2.d0
        dr(3,3)  =  mc2**1.5
c
c    4.b compute asymptotic covariance of central moments df.s_m.df
c
       call dmrrrr(3,3,dr,3,3,3,s_mc,3,3,3,temp,3)
       call dmxytf(3,3,temp,3,3,3,dr,3,3,3,s_mn,3)

       return
       end
       
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|program testrs
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  *** delete first-column c and compile with imslfake.f and probfun.f
c      routines to run test on all of the enclosed subroutines
c
c
c     author.....tim cohn
c     date.......2 feb 2007
c
c      program testrs;call testroutines();end
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c

c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine mP3(tl,tu,m,mnout,n)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to compute the expected value of the k-th moment of 
c     a pearson type 3 variate between tl and tu with moments m=(mu,sigma^2,g)
c
c     author.....tim cohn
c     date.......2 feb 2007
c     
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine mP3(tl,tu,m,mnout,n)

        implicit none
        
        integer 
     1    n,
     2    i,j,k
     
        double precision 
     1    tl,tu,t,m(3),mnout(*),
     2    a(0:3),b(0:12,0:36),expz(0:36),fp(0:36),fwh(0:36),
     3    mg(36),mwh(36),parms(3),
     4    mu,s,g,tau,zl,zu,pdfu,pdfl,cdfu,cdfl,
     5    w,wg,wwh,zl1,zu1,
     6    choose,fp_g2_mom_trc,fp_z_pdf,fp_z_cdf,whlp2z
     
       data b/481*0.d0/,mg/36*0.d0/,mwh/36*0.d0/
     
          mu   = m(1)
          s    = sqrt(m(2))
          g    = m(3)
          zl   = max(-1d20,min(1d20,whlp2z((tl-mu)/s,g)))
          zu   = max(-1d20,min(1d20,whlp2z((tu-mu)/s,g)))
          cdfu = fp_z_cdf(zu) 
          cdfl = fp_z_cdf(zl)
c
c   test to see if we have an interval to consider
c   n.b. if interval is off support of f, use end closest to 
c     support as observed value
c
      if(cdfu .eq. cdfl) then
        if(abs(tl-mu) .lt. abs(tu-mu)) then
          t  =  tl
        else 
          t  =  tu
        endif
        do 5 k=1,n
          mnout(k) = t**k
5       continue
          return
      endif

      if(n .gt. 12) then
        write(*,*) 'mP3: error; n too large: ',n
        stop
      endif
c
c  compute relative weight to apply to gamma solution vs wilson hilferty
c
      w = max(0.d0,(abs(g)-0.0007)/(0.0010-0.0007))
        if(w .ge. 1.d0) then
          wwh = 0.d0
        else
          wwh  = (1.d0+cos(3.14159265359*w))/2.d0
        endif
      wg  = 1.d0-wwh
c
c  compute moments for abs(g)>0.07 (i.e. large skew) with inc. gamma function 
c
      if(wg .gt. 0.d0) then
          call m2p(m,parms)
            tau = parms(1)
            fp(0) = 1.d0
        do 10 i=1,n
          fp(i) = fp_g2_mom_trc(parms(2),tl-tau,tu-tau,i)
            mg(i) = fp(i)
          do 10 j=0,i-1
            mg(i) = mg(i) + choose(i,j)*tau**(i-j)*fp(j)
10      continue
      endif
c
c  compute moments for small skews using wilson-hilferty transformation
c   n.b. a() vector contains power series expansion
c       f(x,g) = f(z) where x=a.(1,z,z^2,z^3)
c   where x is standardized gamma with skew g
c
c   first compute moments of z given zl < z < zu
c
      if(wwh .gt. 0.d0) then           
          pdfu     = fp_z_pdf(zu)
          pdfl     = fp_z_pdf(zl)
          expz(0)  = 1.d0
          expz(1)  = (-pdfu+pdfl)/(cdfu-cdfl)
            if(cdfu .lt. 1.d0) then
              zu1 = zu
            else
              zu1 = 0.d0
            endif
            if(cdfl .gt. 0.d0) then
              zl1 = zl
            else
              zl1 = 0.d0
            endif
        do 15 i=2,3*n
          expz(i) = (-(zu1**(i-1))*pdfu+(zl1**(i-1))*pdfl)/(cdfu-cdfl) 
     1            + (i-1) * expz(i-2)
15      continue
c
c   now define a() [coefficients in series expansion of wilson-hilferty trans.]
c
          a(0)   = -g*(3888.-108.*g**2+g**4)/23328.
          a(1)   = (1-g**2/36)**2
          a(2)   = (g/6.)*(1-g**2/36.)
          a(3)   = g**2/108.
          
          b(0,0) = 1.d0
c
c   compute coefficients in expected value of censored x
c
              fwh(0) = 1.d0
        do 20 i=1,n
              fwh(i) = 0.d0
          do 20 j=0,3*i
              b(i,j) = 0.d0
            do 30 k=max(0,j-3*(i-1)),min(j,3)
              b(i,j) = b(i,j) + b(i-1,j-k)*a(k)            
30          continue
          fwh(i) = fwh(i) + b(i,j)*expz(j)
20      continue
c
c  de-standardize results (i.e. add in mu and multiply by sigma as approp.)
c
        do 35 i=1,n
            mwh(i) = s**i*fwh(i)
          do 35 j=0,i-1
            mwh(i) = mwh(i) + choose(i,j)*mu**(i-j)*s**j*fwh(j)
35      continue

      endif
c
c   compute weighted sum
c
      do 40 i=1,n
        mnout(i) = wg*mg(i) + wwh*mwh(i)
40    continue
      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      double precision function whlp2z(x,g)
      
      double precision x,g
      
      if(g .eq. 0.d0) then 
        whlp2z = x
      else
        whlp2z = (6.d0/g)*(g**2/36.d0-1.d0+max(0.d0,1.d0+g*x/2.d0)**
     1                       (1.d0/3.d0))
      endif
        return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      double precision function whz2lp(z,g)
      
      double precision z,g
      
      if(g .eq. 0.d0) then 
        whz2lp = z
      else
        whz2lp = (2.d0/g)*(1.d0+g*z/6.d0-g**2/36.d0)**3-2.d0/g
      endif
        return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      double precision function dp3(x,m)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to compute the pdf at x of 
c     a pearson type 3 variate with moments m=(mu,sigma^2,g)
c
c     author.....tim cohn
c     date.......2 feb 2007
c     
      implicit none
           
      double precision
     1  x,m(3),
     2  parms(3),g,mu,s,z,
     3  w,wg,wwh,dp3g,dp3wh,
     4	fp_g3_pdf,fp_z_pdf,whlp2z
     
      g = m(3)
c
c  compute relative weight to apply to gamma solution vs wilson hilferty
c
      w = max(0.d0,(abs(g)-0.0007)/(0.0010-0.0007))
        if(w .ge. 1.d0) then
          wwh = 0.d0
        else
          wwh  = (1.d0+cos(3.14159265359*w))/2.d0
        endif
      wg  = 1.d0-wwh
c
c  compute cdf for abs(g)>0.07 (i.e. large skew) with inc. gamma function 
c
      if(wg .gt. 0.d0) then
        call m2p(m,parms)
        dp3g = fp_g3_pdf(x,parms)
      endif
c
c  compute cdf for small skews using wilson-hilferty transformation
c
      if(wwh .gt. 0.d0) then
        mu = m(1)
        s  = sqrt(m(2))
        z  = whlp2z((x-mu)/s,g)
        dp3wh = fp_z_pdf(z) * (1.d0/s) * (1+(g*(x-mu)/s)/2.)**(-2./3.)
      endif
c
c   compute weighted sum
c
        dp3 = wg*dp3g + wwh*dp3wh
      return
      end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      double precision function pP3(x,m)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to compute the cdf at x of 
c     a pearson type 3 variate with moments m=(mu,sigma^2,g)
c
c     author.....tim cohn
c     date.......2 feb 2007
c     
      implicit none
      
      double precision
     1  x,m(3),
     2  parms(3),g,mu,s,z,
     3  w,wg,wwh,pP3g,pP3wh,
     4	fp_g3_cdf,fp_z_cdf,whlp2z
     
      g = m(3)
c
c  compute relative weight to apply to gamma solution vs wilson hilferty
c
      w = max(0.d0,(abs(g)-0.0007)/(0.0010-0.0007))
        if(w .ge. 1.d0) then
          wwh = 0.d0
        else
          wwh  = (1.d0+cos(3.14159265359*w))/2.d0
        endif
      wg  = 1.d0-wwh
c
c  compute cdf for abs(g)>0.07 (i.e. large skew) with inc. gamma function 
c
      if(wg .gt. 0.d0) then
        call m2p(m,parms)
        pP3g = fp_g3_cdf(x,parms)
      endif
c
c  compute cdf for small skews using wilson-hilferty transformation
c
      if(wwh .gt. 0.d0) then
        mu = m(1)
        s  = sqrt(m(2))
        z  = whlp2z((x-mu)/s,g)
        pP3wh = fp_z_cdf(z)
      endif
c
c   compute weighted sum
c
        pP3 = wg*pP3g + wwh*pP3wh
      return
      end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      double precision function qP3(q,m)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to compute the inverse cdf of 
c     a pearson type 3 variate with moments m=(mu,sigma^2,g)
c
c     author.....tim cohn
c     date.......2 feb 2007
c     
      implicit none
      
      double precision
     1  q,m(3),
     2  parms(3),g,mu,s,
     3  w,wg,wwh,qP3g,qP3wh,
     4	fp_g3_icdf,fp_z_icdf,whz2lp,infinity
     
      data infinity/1.0d31/
     
      g = m(3)
      
      if(q .le. 0.d0) then
        if(g .gt. 0.d0) then
          call m2p(m,parms)
          qP3 = parms(1)
        else
          qp3 = -infinity
          return
        endif
      else if(q .ge. 1.d0) then
        if(g .lt. 0.d0) then
          call m2p(m,parms)
          qP3 = parms(1)
        else
          qp3 = +infinity
          return
        endif
      endif
      
c
c  compute relative weight to apply to gamma solution vs wilson hilferty
c
      w = max(0.d0,(abs(g)-0.0007)/(0.0010-0.0007))
        if(w .ge. 1.d0) then
          wwh = 0.d0
        else
          wwh  = (1.d0+cos(3.14159265359*w))/2.d0
        endif
      wg  = 1.d0-wwh
c
c  compute cdf for abs(g)>0.07 (i.e. large skew) with inc. gamma function 
c
      if(wg .gt. 0.d0) then
        call m2p(m,parms)
        qP3g = fp_g3_icdf(q,parms)
      endif
c
c  compute cdf for small skews using wilson-hilferty transformation
c
      if(wwh .gt. 0.d0) then
        mu = m(1)
        s  = sqrt(m(2))
        qP3wh = mu+s*whz2lp(fp_z_icdf(q),g)
      endif
c
c   compute weighted sum
c
        qP3 = wg*qP3g + wwh*qP3wh
      return
      end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine rP3
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to compute a sample of random pearson type 3 
c       variates with moments m=(mu,sigma^2,g)
c
c     author.....tim cohn
c     date.......2 feb 2007
c     
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     N.B. To initialize:
c         call srand(iseed)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision function rP3(m)
      
      implicit none
      
      double precision 
     1  m,qP3
      
c      real rand
      
c      rP3 = qP3(dble(rand()),m)
      return
      end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c      program testrs;call testroutines();end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      subroutine testroutines()
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to test the following routines
c       pP3(x,m)  -- cdf of lp3
c       qP3(p,m)  -- inverse cdf of lp3
c       dp3(x,m)  -- pdf of lp3
c       mP3(...)  -- non-central moments of lp3
c      
c
c     author.....tim cohn
c     date.......2 feb 2007
c     
      implicit none
      
      integer 
     1  ng,np,
     2  i,j,k,
     3  ier,neval,
     4  limit,lenw,last,iwork(100000),k1
     
      parameter 
     1  (ng=15,np=11)
     
      external ftest01
      
      double precision 
     1  pp(0:np),gg(ng),m,
     2  x,p,abserr,fp(0:3),kk,mnout(3),
     3  work(400000),
     3  qP3,pP3
      
      data pp/0.0000001,0.001,0.010,0.050,0.100,0.250,0.500,
     1        0.7500,0.900,0.950,0.990,0.999/
     
      data gg/-3.0,-2.0,-1.0,-0.50,-0.10,-0.07,-0.01,0.,
     1         0.01,0.07,0.10,0.5,1.0,2.0,3.0/
     
      data limit,lenw/100000,400000/
     
      common /zap111/m(3),kk
     
        write(*,*) 'testing qP3,pP3'
        write(*,'(6a14)') ' g',' mu',' s^2',' pp',' p',' x'
        write(*,*)
        abserr = 0.d0
      do 10 i=1,ng
       do 10 j=1,np
        do 10 k=0,1
          m(1)    = 0.+k
          m(2)    = 1.+k
          m(3)    = gg(i)
          x       = qP3(pp(j),m)
          p       = pP3(x,m)
          abserr  = max(abserr,abs(p-pp(j)))
        if(abs(p-pp(j)) .gt. 1e-6) then
          write(*,'(4f14.4,3e14.5)') m(3),m(1),m(2),pp(j),p,x
        endif
10    continue
        write(*,*)
        write(*,*) ' probabilities tested:'
		write(*,'(7f8.4,/)') pp
        write(*,*)
        write(*,*) ' skews tested:'
		write(*,'(7f8.4,/)') gg
        write(*,*)
        write(*,*) ' maximum absolute error in probability:'
		write(*,'(1f18.8/)') abserr
c
		write(*,*)
        write(*,*) ' *** checking the censored moments ***'
		write(*,*)
          m(1)    = 2.54
          m(2)    = 1.66
      do 20 i=1,ng
          m(3)    = gg(i)
       do 20 j=1,np
          call mP3(qP3(pp(j-1),m),qP3(pp(j),m),m,mnout,3)
            write(*,'(4f10.4,3e14.5)') m(3),m(1),m(2),pp(j),mnout
        do 30 k=0,3
            kk=k
          call dqag(ftest01,qP3(pp(j-1),m),qP3(pp(j),m),
     1                1.d-6,1.d-6,2,fp(k),abserr,neval,ier,
     2                limit,lenw,last,iwork,work)

            if(ier .ne. 0) write(*,*) ' ier (dqag) = ',ier
          if(k .gt. 0) fp(k)=fp(k)/fp(0)
30      continue
            write(*,'(40x,3e14.5)') (fp(k1),k1=1,3)
            write(*,'(10x,3f10.6,3e14.2)') 
     1       pp(j-1),pp(j),
     2       fp(0),
     3       (fp(k1)/mnout(k1)-1.d0,k1=1,3)
20    continue     
      stop
      end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      double precision function ftest01(x)
      implicit none
      
      double precision x,m,kk,arg,dp3
      common /zap111/m(3),kk
      
      if(kk .eq. 0.d0) then
        arg = 1
      else
        arg = x**kk
      endif
        ftest01 = arg*dp3(x,m)
      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|subroutine compress2
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c    
c    compresses the threshold data to reduce redundant computation
c       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        20 sep 2007
c       modified            31 dec 2003
c                                       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or 
c                              other)
c            tl(n)      r*8  vector of lower bounds on (log) floods
c            tu(n)      r*8  vector of upper bounds on (log) floods
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            nt         i*4  number of distinct censoring thresholds 
c            nobs(nt)   r*8  number of observations corresponding to 
c                              threshold
c            tl2(nt)    r*8  lower bounds on (log) flood threshold
c            tu2(nt)    r*8  upper bounds on (log) flood threshold
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine compress2(n,tl,tu,nt,n2,tl2,tu2)
      
      integer 
     1  n,nt
      
      double precision
     1  tl(*),tu(*),n2(*),tl2(*),tu2(*)
     
      nt = 0
      
      do 10 i=1,n
        do 20 j=1,nt
          if(tl(i) .eq. tl2(j) .and. tu(i) .eq. tu2(j)) then
            n2(j) = n2(j)+1.d0
            goto 10
          endif
20      continue
          nt       = nt+1
          n2(nt)   = 1.d0
          tl2(nt)  = tl(i)
          tu2(nt)  = tu(i)
10    continue
        n2(nt+1) = 0
      do 30 i=1,nt
        n2(nt+1) = n2(nt+1)+n2(i)
30    continue

      return
      end
      
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine dqag
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to integrate a univariate function using simpson's rule
c
c     this is a cheap substitute for the dqag routine available from 
c     nist as part of gams library of fortran subroutine
c
c     author.....tim cohn
c     date.......2 feb 2007
c     
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine dqag(f,a,b,
     1                err1,err2,key,result,abserr,neval,ier,
     2                limit,lenw,last,iwork,work)
     
      implicit none
      
      integer
     1  key,neval,ier,limit,lenw,last,iwork(*),npt,i
     
      double precision
     1  f,a,b,err1,err2,result,abserr,work(*),sum,delta,x
     
      external f
      
      data npt /1001/
      
        delta=(b-a)/(npt-1.d0)
      if(delta .eq. 0.d0) then
        result = 0.d0
      else if (delta .lt. 0.d0) then
        write(*,*) 'limits on integral reversed'
        result = 0.d0
      else
          x   = a
          sum = f(x) + 4.d0*f(x+delta) + f(b)
        do 10 i=1,(npt-3)/2
          x   = x+2.d0*delta
          sum = sum+2.d0*f(x)+4.d0*f(x+delta)
10      continue
        result = delta*sum/3.d0
      endif
          ier    =  0
          abserr = -99.d0
        return
      end
      subroutine b17cip(n,moms,pq,peps,cil,ciu)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    computes confidence intervals used by B17B
c
c    tim cohn........24 Nov 2003
c            ........19 feb 2007  (changed call to use moms, not parms)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or other)
c            moms(3)    r*8  fitted parameters of lp3 distribution
c            pq         r*8  flood probability (usually 0.99)
c            peps       r*8  confidence interval coverage for two-sided ci
c                            (peps = 0.95 corresponds to a two-sided coverage of 0.95)
c            cil        r*8  left-hand end of log-space ci
c            ciu        r*8  right-hand end of log-space ci
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      
      implicit none
      
      integer
     1 n
     
      double precision
     1 pq,peps,cil,ciu,
     2 moms(3),klpc,kupc

      call b17ci(n,moms(3),pq,(1.d0+peps)/2.d0,klpc,kupc)
      
      cil = moms(1) + klpc * sqrt(moms(2))     
      ciu = moms(1) + kupc * sqrt(moms(2))
      
      return
      end
          
      subroutine b17ci(n,skew,p,c,klpc,kupc)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    computes confidence intervals used by B17B
c
c    tim cohn........24 Nov 2003
c            ........19 feb 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or other)
c            skew       r*8  skewness of fitted lp3 distribution
c            p          r*8  flood non-exceedance probability (usually 0.99)
c            c          r*8  confidence interval coverage for one-sided ci
c                            (c = 0.95 corresponds to a two-sided coverage of 0.90)
c            klpc       r*8  left-hand factor for ci
c            kupc       r*8  right-hand factor for ci
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c        note that a 90% ci will be constructed by calling:
c
c            call b17ci(n,g,0.99d0,0.95d0,klpc,kupc)
c
c        then the log-space ci is
c            (xbar + s * klpc, xbar + s * kupc)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      
      implicit none
      
      integer
     1 n
     
      double precision
     1 skew,p,c,klpc,kupc,
     2 zc,kgwp,a,b,
     3 fp_z_icdf,kf
     
      zc     = fp_z_icdf(c)
      kgwp   = kf(skew,p)

      a      = 1.d0 - zc**2/(2.d0*(n-1.d0))
      b      =  kgwp**2 - zc**2/n
      
      kupc   =  (kgwp + sqrt(kgwp**2 - a*b))/a
      klpc   =  (kgwp - sqrt(kgwp**2 - a*b))/a
      
      return
      end
            
      
      double precision function kf(skew,prob)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    computes critical points of LP3 distribution 
c    returns "K" value in format used by B17B
c
c    tim cohn........19 feb 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      implicit none
      
      double precision
     1  skew,prob,m(3),qp3
     
      data m/0,1,0/
          
      m(3) = skew

      kf = qp3(prob,m)
      
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c  n.b.  remove the "c" at start of next line to test the program    
c      program testit; call testsub001; return; end

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     this represents the test case given on page 9-3 of B17B
c
c     correct result is:
c      klpc, kupc     2.059     3.026
c      lci, uci       3.515     3.757
c      cil, ciu       3.515     3.757
c
      subroutine testsub001
      
      implicit none
      
      integer
     1 n
     
      double precision
     1 parms(3),moms(3),p,c,klpc,kupc,l,u
      
        moms(1) = 3.d0
        moms(2) = (0.25d0)**2
        moms(3) = 0.20d0
        call m2p(moms,parms)
        n       = 50
        p       = 0.99d0
        c       = 0.95d0
      call b17ci(n,moms(3),p,c,klpc,kupc)

      write(*,'(a,2f10.3)') 'klpc, kupc',klpc,kupc
      
        l = moms(1) + sqrt(moms(2))*klpc
        u = moms(1) + sqrt(moms(2))*kupc
      write(*,'(a,2f10.3)') 'lci, uci  ',l,u
      
      call m2p(moms,parms)
      call b17cip(n,parms,p,2*c-1.d0,l,u)
      write(*,'(a,2f10.3)') 'cil, ciu  ',l,u
      
      stop
      end

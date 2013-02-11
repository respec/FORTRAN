c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|subroutine emafit
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     REVISED VERSION FROM 2011
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
c       modified            21 jun 2007  corrected error in rGmse treatment wrt
c                                          generalized skew
c       modified            13 aug 2007  final low-outlier default procedure
c                                          implemented
c       modified            17 aug 2007  final, final LO default procedure
c                                          implemented
c       modified            25 sep 2007  final 'argh!' w/update to mseg 
c                                          to correct for censored data
c       modified            28 feb 2008  computed moments for both B17B 
c                                          and EMA at-site MSE(skew)
c       modified            18 jun 2008  added constraint to ensure skews 
c                                          within reliable range of computation
c       modified            02 jul 2008  set default MSE for at-site skew to
c                                          equal B17B MSE when only systematic
c                                          data are present ('ADJE')
c       modified            15 sep 2011  set default lskewXmax to .FALSE. 
c                                          this determines whether to 
c                                          adjust skew so that Qmax is inside
c                                          support of fitted distribution
c                                          (previously set to .TRUE.)
c                                          B17B provides no test or correction
c       modified            26 sep 2011  added regional estimate for M 
c                                          this is analogous to region S2, G 
c       modified            24 oct 2011  simplified calls to p3est; results same 
c       modified            08 feb 2012  added return for var_est in  
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       NEW VERSION         05 jan 2011  employs 
c                                          1) Multiple Grubbs-Beck Test
c                                          2) Regional standard deviation 
c
c       modified            08 apr 2011  fixed MGBT to deal with more than 
c                                          50% zero flows
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
c                            NOTE: Zero flows enter as lmissing=-80.d0
c            tl(n)      r*8  vector of lower bounds on (log) flood threshold
c            tu(n)      r*8  vector of upper bounds on (log) flood threshold
c            dtype(n)   c*4  vector describing input data
c                              dtype(i) = "Syst" => systematic data
c                              dtype(i) = "Hist" => historic data
c                              dtype(i) = "Othr" => other
c            reg_SD     r*8  regional standard deviation
c            reg_SD_mse r*8  mean square error of generalized standard deviation
c                            notes:
c                            1) reg_SD_mse = 0 ("GENERALIZED STD DEV, NO ERROR")
c                                use fixed parms(*) = reg_SD w/ mse = 0.0
c                            2) -999 < reg_SD_mse< 0 ("AT-SITE STD DEV")
c                                use at-site estimated standard deviation
c                            3) 0 < reg_SD_mse < 1.d10 ("WEIGHTED STD. DEV.")
c                                use sd = weighted average of at-site and 
c                                regional standard deviation
c            r_G        r*8  regional skew
c            r_G_mse    r*8  mean square error of regional skew
c                            this variable encodes four distinct cases:
c                            1) r_G_mse  = 0 ("GENERALIZED SKEW, NO ERROR")
c                                use fixed g = r_G w/ mse = 0.0
c                            2) -98 < r_G_mse  < 0 ("GENERALIZED SKEW, MSE > 0")
c                                use fixed g = w/ mse = -r_G_mse  
c                            3) 0 < r_G_mse  < 1.d10 ("WEIGHTED SKEW")
c                                use g = weighted average of at-site and 
c                                regional skew (b17b recommendation)
c                            4) 1.d10 < r_G_mse  ("STATION SKEW")
c                                use g = at-site skew
c            gbtype     c*4  type of Grubbs-Beck test
c                               = "GBT"  => standard GB test
c                               = "MGBT" => Multiple GB test
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
c            cmoms(3,3) r*8  first 3 central moments
c                             (,1) using regional info and at-site data
c                             (,2) using just at-site data
c                             (,3) using B17B formula for at-site MSE(G)
c                              mc = {mean, variance, coeff. skew}
c            pq(32)     r*8  quantiles estimated 
c                            --pq=0.99 corresponds to "100-year flood"
c            yp(32)     r*8  estimated pq-th quantile of fitted p3 distribution
c            ci_low(32) r*8  left end of 95% confidence interval for pq-th 
c                              quantile
c            ci_high(32)r*8  right end of 95% confidence interval for pq-th 
c                              quantile
cprh (09/2009)
c            var_est(*) r*8  variance of estimate for each quantile
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       parameters set in common block data/emablk/ (can be reset if desired)
c       ------------------------------------------------------------------------
c       /tacg04/at_site_std  c*4   formula employed for computing mse of
c                                  estimated at-site skew 
c                                    'B17B' => Bulletin 17B MSE formula
c                                    'EMA'  => First-order EMA MSE
c                                    'ADJE' => Adjusted first-order (Default)
c
c       /tacmg1/alph01       r*8   alpha level for MGBT check for low outliers
c                                    Default value is 0.01  [corr. 1% test]
c
c       /tacmg1/alph10       r*8   alpha level for MGBT check for low outliers
c                                    Default value is 0.10 [corr. 10% test]
c
c       /tac002/sk141        r*8   minimum value of skew 
c                                    Default value is -1.41
c
c       /tac002/bcf          r*8   bias correction factor to use for S2, G 
c                                    1997  => Cohn [1997] factors (Default)
c                                    2004  => Griffis et al. [2004] factors
c                                    0     => None
c
c       /tac002/lskewXmax    r*8   do you want to test fitted support to
c                                  include Qmax?
c                                    Default value is .FALSE.
c
c       /tacR01/VarS2opt     c*4   formula employed for computing weighting of
c                                  at-site and regional info for computing S2
c                                  (the problem: MSE of \hat{S^2} proportional
c                                   to \sigma^4; we have two estimates of \sigma
c                                   at-site and regional. What should weight
c                                   reflect?)
c                                    'DF'   => inversely to degrees of freedom
c                                                df = 2*(S^4)/MSE[S^2] (DEFAULT)
c                                    'S2'   => inversely to MSE[S^2_{at-site}]
c                                              and MSE[S^2_{regional}] 
c                                    'S1'   => inversely to MSE[S_{at-site}]
c                                              and MSE[S_{reg.}] (NOT AVAIL.)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine emafit(n,ql,qu,tl,tu,dtype,
     1               reg_SD,reg_SD_mse,r_G,r_G_mse,gbtype,gbthrsh0,
     1                  cmoms,pq,yp,ci_low,ci_high,var_est)

      implicit none

      integer
     1  i,n,                                                ! input variables
     2  neps,nq
     
C      parameter (nq=15)
C     use full set of probabilities found in PeakFQ
      parameter (nq=32)

      double precision
     1  ql(*),qu(*),tl(*),tu(*),reg_SD,reg_SD_mse,r_G,r_G_mse, ! input variables
     2  cmoms(3,3),pq(*),yp(*),ci_low(*),ci_high(*),var_est(*),! output variables
     3  pqd(nq),eps(1),gbthrsh0,
     4  r_M,r_M_mse,r_S2,r_S2_mse
     
      character*4
     1  dtype(*),gbtype

      data neps/1/,eps/0.95d0/
      
Cprh      data pqd/0.005,0.010,0.050,0.100,0.200,0.3333,0.500,
Cprh     1         0.5708,0.800,0.900,0.960,0.980,0.990,0.995,0.998/
      data pqd/0.0001,0.0005,0.001 ,0.002 ,0.005 ,0.010 ,0.020 ,0.025 ,
     1         0.040 ,0.050 ,0.100 ,0.200 ,0.300 ,0.33329,0.400 ,0.4296,
     2         0.5   ,0.5708,0.600 ,0.700 ,0.800 ,0.900 ,0.950 ,0.960 ,
     3         0.975 ,0.980 ,0.990 ,0.995 ,0.998 ,0.999 ,0.9995,0.9999/
     
c    
c     set regional information for mean equal to zero
c
      r_M       =   0.d0   
      r_M_mse   = -99.d0   !  ignore regional info on M
c    
c     calculate stats for variances (S^2) from standard deviations input
c
      r_S2      = reg_SD**2
      r_S2_mse  = 4.d0 * r_S2 * reg_SD_mse  !  first order approximation
      
      do 10 i=1,nq
          pq(i) = pqd(i)
        call emafitb(n,ql,qu,tl,tu,dtype,
     1                  r_M,r_M_mse,r_S2,r_S2_mse,r_G,r_G_mse,
     1                  neps,eps,
     1                  gbtype,gbthrsh0,pq(i),
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

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|subroutine emafitpr
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     REVISED VERSION FROM 2011
c    
c    this routine fits the pearson type iii distribution to 
c      a data set using the ema algorithm. 
c    it includes the ability to employ regional information on 3 parameters
c    otherwise it works the same as emafit
c
c    so far this feature is not adequately documented
c       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        26 sep 2011  added regional estimate for M 
c                                          this is analogous to region S2, G 
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or 
c                              other)
c            ql(n)      r*8  vector of lower bounds on (log) floods
c            qu(n)      r*8  vector of upper bounds on (log) floods
c                            NOTE: Zero flows enter as lmissing=-80.d0
c            tl(n)      r*8  vector of lower bounds on (log) flood threshold
c            tu(n)      r*8  vector of upper bounds on (log) flood threshold
c            dtype(n)   c*4  vector describing input data
c                              dtype(i) = "Syst" => systematic data
c                              dtype(i) = "Hist" => historic data
c                              dtype(i) = "Othr" => other
c            reg_M      r*8  regional mean (M)
c            reg_M_mse  r*8  mean square error of generalized mean
c                            notes:
c                            1) reg_M_mse = 0 ("GENERALIZED MEAN, NO ERROR")
c                                use fixed parms(*) = reg_M w/ mse = 0.0
c                            2) -999 < reg_M_mse< 0 ("AT-SITE MEAN")
c                                use at-site estimated mean
c                            3) 0 < reg_M_mse < 1.d10 ("WEIGHTED MEAN")
c                                use M = weighted average of at-site and 
c                                regional mean
c            reg_SD     r*8  regional standard deviation
c            reg_SD_mse r*8  mean square error of generalized standard deviation
c                            notes:
c                            1) reg_SD_mse = 0 ("GENERALIZED STD DEV, NO ERROR")
c                                use fixed parms(*) = reg_SD w/ mse = 0.0
c                            2) -999 < reg_SD_mse< 0 ("AT-SITE STD DEV")
c                                use at-site estimated standard deviation
c                            3) 0 < reg_SD_mse < 1.d10 ("WEIGHTED STD. DEV.")
c                                use sd = weighted average of at-site and 
c                                regional standard deviation
c            r_G        r*8  regional skew
c            r_G_mse    r*8  mean square error of regional skew
c                            this variable encodes four distinct cases:
c                            1) r_G_mse  = 0 ("GENERALIZED SKEW, NO ERROR")
c                                use fixed g = r_G w/ mse = 0.0
c                            2) -98 < r_G_mse  < 0 ("GENERALIZED SKEW, MSE > 0")
c                                use fixed g = w/ mse = -r_G_mse  
c                            3) 0 < r_G_mse  < 1.d10 ("WEIGHTED SKEW")
c                                use g = weighted average of at-site and 
c                                regional skew (b17b recommendation)
c                            4) 1.d10 < r_G_mse  ("STATION SKEW")
c                                use g = at-site skew
c            gbtype     c*4  type of Grubbs-Beck test
c                               = "GBT"  => standard GB test
c                               = "MGBT" => Multiple GB test
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
c            cmoms(3,3) r*8  first 3 central moments
c                             (,1) using regional info and at-site data
c                             (,2) using just at-site data
c                             (,3) using B17B formula for at-site MSE(G)
c                              mc = {mean, variance, coeff. skew}
c            pq(32)     r*8  quantiles estimated 
c                            --pq=0.99 corresponds to "100-year flood"
c            yp(32)     r*8  estimated pq-th quantile of fitted p3 distribution
c            ci_low(32) r*8  left end of 95% confidence interval for pq-th 
c                              quantile
c            ci_high(32)r*8  right end of 95% confidence interval for pq-th 
c                              quantile
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine emafitpr(n,ql,qu,tl,tu,dtype,
     1               reg_M,reg_M_mse,reg_SD,reg_SD_mse,r_G,r_G_mse,
     2               gbtype,gbthrsh0,
     2               cmoms,pq,yp,ci_low,ci_high,var_est)

      implicit none

      integer
     1  i,n,                                                ! input variables
     2  neps,nq
     
      parameter (nq=32)

      double precision
     1  ql(*),qu(*),tl(*),tu(*),reg_SD,reg_SD_mse,r_G,r_G_mse, ! input variables
     2  cmoms(3,3),pq(*),yp(*),ci_low(*),ci_high(*),var_est(*),! output variables
     3  pqd(nq),eps(1),gbthrsh0,
     4  reg_M,reg_M_mse,r_M,r_M_mse,r_S2,r_S2_mse
     
      character*4
     1  dtype(*),gbtype

      data neps/1/,eps/0.95d0/
      
Cprh      data pqd/0.005,0.010,0.050,0.100,0.200,0.3333,0.500,
Cprh     1         0.5708,0.800,0.900,0.960,0.980,0.990,0.995,0.998/
      data pqd/0.0001,0.0005,0.001 ,0.002 ,0.005 ,0.010 ,0.020 ,0.025 ,
     1         0.040 ,0.050 ,0.100 ,0.200 ,0.300 ,0.3333,0.400 ,0.4296,
     2         0.5   ,0.5708,0.600 ,0.700 ,0.800 ,0.900 ,0.950 ,0.960 ,
     3         0.975 ,0.980 ,0.990 ,0.995 ,0.998 ,0.999 ,0.9995,0.9999/
     
c     1.  bring in regional information for M, S, and g

c     1a. set regional information for mean

      r_M_mse   = reg_M_mse
      if(r_M_mse .le. 0.d0) then
        r_M     =  0.d0
        r_M_mse = -99.d0
      else
        r_M     =  reg_M
        r_M_mse =  reg_M_mse
      endif
        
c     1b. set regional information for variance
c    
c     calculate stats for variances (S^2) from standard deviations input
c     N.B.  This is an approximation. The results would be slightly different
c           if regionalization were done on S^2 or if estimation weighting
c           were done on S. However, this is more convenient given the
c           parametrization based on (M, S2, G) rather than (M, S, G)
c
      r_S2      = reg_SD**2
      r_S2_mse  = 4.d0 * r_S2 * reg_SD_mse  !  first order approximation

c     1c. regional information for skew must be supplied by user as argument
      
      do 10 i=1,nq
          pq(i) = pqd(i)
        call emafitb(n,ql,qu,tl,tu,dtype,
     1                  r_M,r_M_mse,r_S2,r_S2_mse,r_G,r_G_mse,
     1                  neps,eps,
     1                  gbtype,gbthrsh0,pq(i),
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
c            dtype(n)   c*4  vector describing input data
c                              dtype(i) = "Syst" => systematic data
c                              dtype(i) = "Hist" => historic data
c                              dtype(i) = "Othr" => other
c            r_M        r*8  regional mean
c            r_M_mse    r*8  mean square error of regional mean
c                            1) r_M_mse = 0 ("GENERALIZED S2, NO ERROR")
c                                use fixed M = r_M w/ mse = 0.0
c                            2) -999 < r_M_mse < 0 ("AT-SITE MEAN")
c                                use at-site estimated mean (M)
c                            3) 0 < r_M_mse < 1.d10 ("WEIGHTED MEAN")
c                                use M = weighted average of at-site and 
c                                regional variance
c            r_S2       r*8  regional variance (std.dev^2)
c            r_S2_mse   r*8  mean square error of regional variance
c                            1) r_S2_mse = 0 ("GENERALIZED S2, NO ERROR")
c                                use fixed sd = reg_SD w/ mse = 0.0
c                            2) -999 < r_S2_mse < 0 ("AT-SITE VARIANCE")
c                                use at-site estimated variance (S^2)
c                            3) 0 < r_S2_mse < 1.d10 ("WEIGHTED VARIANCE")
c                                use S2 = weighted average of at-site and 
c                                regional variance
c            r_G        r*8  regional skew
c            r_G_mse    r*8  mean square error of regional skew
c                            this variable encodes four distinct cases:
c                            1) r_G_mse  = 0 ("GENERALIZED SKEW, NO ERROR")
c                                use fixed g = r_G w/ mse = 0.0
c                            2) -98 < r_G_mse  < 0 ("GENERALIZED SKEW, MSE > 0")
c                                use fixed g = r_G w/ mse = -r_G_mse  
c                            3) 0 < r_G_mse  < 1.d10 ("WEIGHTED SKEW")
c                                use g = weighted average of at-site and 
c                                regional skew (b17b recommendation)
c                            4) 1.d10 < r_G_mse  ("STATION SKEW")
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
c            cmoms(3,3) r*8  first 3 central moments 
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

      subroutine emafitb(n,ql_in,qu_in,tl_in,tu_in,dtype_in,
     1                   r_M,r_M_mse,r_S2,r_S2_mse,r_G,r_G_mse  ,
     1                   neps,eps,gbtype_in,gbthrsh0,pq,
     2                   cmoms,yp,ci_low,ci_high,var_est)
      
      implicit none
      
      integer 
     1  ntmax,nn,nx
      
      parameter (ntmax=100,nn=100,nx=25000)
      
      integer
     1  n,i,nt,neps,nlow,nzero,nGBiter,nlow_V,it_max
     
      double precision
     1  ql_in(*),qu_in(*),tl_in(*),tu_in(*),
     1  r_M,r_M_mse,r_S2,r_S2_mse,r_G,r_G_mse,pq,                   ! input
     2  cmoms(3,3),yp,ci_low(neps),ci_high(neps),var_est(neps),     ! output
     3  yp1,yp2,ci_low1(nn),ci_low2(nn),ci_high1(nn),ci_high2(nn),
     4  skewmin,qP3,parms(3),
     5  ql,qu,tl,tu,qs,
     6  cv_yp_syp(2,2),eps(1),tl2(ntmax),tu2(ntmax),nobs(ntmax),
     7  skew,wt,as_M_mse,as_S2_mse,as_G_mse,gbthrsh0,gbcrit,gbthresh,
     8  gbcrit_V,gbthresh_V,pvaluew
     
      parameter (skewmin=0.06324555)
      
      double precision
     1  mseg_all,mse_ema
     
      character*4 
     1  at_site_option,at_site_default,at_site_std,gbtype,gbtype_in,
     2  dtype,dtype_in(*),VarS2opt
     
      logical cirun
      
      common /tacg01/gbcrit,gbthresh,pvaluew(10000),qs(10000),
     1               nlow,nzero,gbtype
      common /tacg02/ql(nx),qu(nx),tl(nx),tu(nx),dtype(nx)
      common /tacg03/gbcrit_V(10),gbthresh_V(10),nlow_V(10),nGBiter
      common /tacg04/at_site_option,at_site_default,at_site_std
      common /tacdgb/cirun
      common /tac005/as_M_mse,as_S2_mse,as_G_mse
      common /tacR01/VarS2opt
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   1.  check for low outliers
c        a) Identify/recode LOs based on at-site systematic + historical data
c        b) Iterate search for LOs using at-site systematic + historical data
c             combined with regional skew information
c
      do 12 i=1,n
        ql(i)    = ql_in(i)
        qu(i)    = qu_in(i)
        tl(i)    = tl_in(i)
        tu(i)    = tu_in(i)
12    continue

       gbtype  = gbtype_in
c   loop up to 10 times to see if any new LOs uncovered
      if(gbtype .eq. 'GBit') then
        it_max = 10
      else
        it_max = 1
      endif

      do 15 i=1,it_max
        call gbtest(n,ql,qu,tl,tu,dtype_in,gbthrsh0,
     1              ql,qu,tl,tu)
         gbcrit_V(i)   = gbcrit
         gbthresh_V(i) = gbthresh
         nlow_V(i)     = nlow
       if(nlow .eq. 0) goto 17    ! any new low outliers?
15    continue
        if(gbtype .eq. 'GBit') then
            write(*,*) ' Low outlier issues (emafit): nlow = ', nlow,i
            write(*,*) ' User may want to specify threshold'
            stop    ! Should never get here; something is wrong
        endif
17    continue
        nGBiter = i  ! added on JRS recomendation

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   If we have censoring due to low outliers using new MGBT, the MSE of
c   at-site skew will tend to stay constant or decline because skew 
c   will be driven toward zero.
c   Thus first-order approximation based on fixed censoring, which would 
c   suggest increased MSE, is incorrect. 
c   As a temporary fix, if MGBT is used and low outliers are detected,
c   the B17B formula will be used to estimate the MSE of the skew.
c
      if(gbtype .eq. "MGBT" .and. nlow .gt. 0) then
        at_site_std = "B17B"
      endif
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   2.  organize data for computing ci and set up the tl and tu vectors
c

      call compress2(n,tl,tu,nt,nobs,tl2,tu2)
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   3.  begin by fitting the p3 distn to the at-site and regional data
c       
c       n.b.  three calls are necessary:
c            1.  compute at-site moments without employing regional skew;
c                   this is used to compute the MSE(G) and moments for 
c                   the case of only at-site data employing the B17B 
c                   formula for MSE(G) [this is a very poor approximation
c                   when historical info or left-tail censoring is present].
c                   However, the moments are needed to compute at-site MSEs
c                   for call 2.
c            2.  EMA/B17B computation using regional skew information 
c                   with B17B MSE(G) formula  (B17B eqn 6; p. 13)
c            3.  final computation using regional skew information 
c                   with EMA MSE(G)
c
c  1) Get at-site skews
      call p3est_ema(n,ql,qu,
     1      1.d0,  1.d0,  1.d0,       ! set all at-site MSEs to 1
     2      r_M,      r_S2,     r_G, 
     3      -99.d0, -99.d0,  -99.d0,  ! set all regional MSEs to Infinity
     4      cmoms(1,2))               ! return moments (only skew is needed)

c  2) EMA computation using weighted regional information/B17B MSEs
        at_site_option = "B17B"
        as_M_mse  = mse_ema(nt,nobs,tl2,tu2,cmoms(1,2),1) ! compute true a-s
        as_S2_mse = mse_ema(nt,nobs,tl2,tu2,cmoms(1,2),2) ! MSEs based on at-
        as_G_mse  = mseg_all(nt,nobs,tl2,tu2,cmoms(1,2))  ! site skew cmoms(1,3)
      call p3est_ema(n,ql,qu,
     1      as_M_mse,as_S2_mse,as_G_mse,
     2      r_M,     r_S2,     r_G, 
     3      r_M_mse, r_S2_mse, r_G_mse,
     4      cmoms(1,3))
      
c  3) final computation using weighted regional info
      if(at_site_std .ne. 'B17B') then
        at_site_option = at_site_std
        as_M_mse  = mse_ema(nt,nobs,tl2,tu2,cmoms(1,2),1) ! compute true a-s
        as_S2_mse = mse_ema(nt,nobs,tl2,tu2,cmoms(1,2),2) ! MSEs based on at-
        as_G_mse  = mseg_all(nt,nobs,tl2,tu2,cmoms(1,2))  ! site skew cmoms(1,3)
        call p3est_ema(n,ql,qu,
     1      as_M_mse,as_S2_mse,as_G_mse, ! use at-site MSEs
     2      r_M,     r_S2,     r_G,      ! and weighted regional info
     3      r_M_mse, r_S2_mse, r_G_mse,
     4      cmoms(1,1))                  ! these are the EMA results
       else
         cmoms(1,1) = cmoms(1,3)
         cmoms(2,1) = cmoms(2,3)
         cmoms(3,1) = cmoms(3,3)
       endif
         yp       = qP3(pq,cmoms)
         call m2p(cmoms,parms)

      if(.not. cirun) return
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   4.  compute confidence intervals
c
      if( abs(cmoms(3,1)) .gt. skewmin) then
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   4.1  for skews far from zero
c

        call var_ema(nt,nobs,tl2,tu2,cmoms,pq,
     1         r_S2,
     2         r_M_mse, r_S2_mse, r_G_mse,
     3         yp1,cv_yp_syp)

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
     1         r_S2,
     2         r_M_mse, r_S2_mse, r_G_mse,
     3         yp1,cv_yp_syp)

        call ci_ema_m3
     1    (yp1,cv_yp_syp,neps,eps,ci_low1,ci_high1)

          cmoms(3,1) =  skewmin  
        call m2p(cmoms,parms)
        call var_ema(nt,nobs,tl2,tu2,cmoms,pq,
     1         r_S2,
     2         r_M_mse, r_S2_mse, r_G_mse,
     3         yp2,cv_yp_syp)

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
c   grubbs-beck test for single low outliers
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
c            gbthrsh0   r*8  critical value for Grubbs-Beck test
c                              (0 or negative values => estimate threshold)
c                              (small value (1e-10) => no low outlier test)
c            as_G_mse   r*8  mse of at-site skew estimate
c            r_G        r*8  regional skew
c            r_G_mse    r*8  mean square error of regional skew
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
c      common /tacg01/gbcrit,gbthresh,pvaluew(10000),qs(10000),nlow,nzero,gbtype
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine gbtest(n,ql_in,qu_in,tl_in,tu_in,dtype,gbthrsh0,
     1   ql,qu,tl,tu)

      implicit none
      
      integer
     1  n,ns,i,nlow,klow,
     2  nzero
     
      integer p_nq
      parameter (p_nq=20000)

      double precision
     1  ql_in(*),qu_in(*),tl_in(*),tu_in(*),
     2  ql(*),qu(*),tl(*),tu(*),gbcrit,gbthresh,gbthrsh0,qs,
     3  x(p_nq),xm,s,t,cmoms(3),gbtmin,pvaluew,
     5  lmissing
     
      integer MGBTP
      
      character*4
     1  gbtype,dtype(*)

      common /tacg01/gbcrit,gbthresh,pvaluew(10000),qs(10000),
     1               nlow,nzero,gbtype
      
      data gbtmin/-6.d0/,lmissing/-80.d0/
!     allows very small positive log(q) values
      
c no low outlier test
      if(gbtype .eq. "NONE") then
        gbthresh  = -99.d0        
        gbcrit    =  gbthresh        
        goto 25
      endif     
 
c specified low outlier threshold?
      if(gbtype .eq. "FIXE") then
        gbthresh  = gbthrsh0        
        gbcrit    = gbthrsh0        
        goto 25
      endif
      
c  computed low outlier threshold and count zero flows
c  note that zero flows are automatically treated as low outliers in MGBT
        ns      = 0
        nzero   = 0
      do 10 i=1,n
        if(ql_in(i) .eq. qu_in(i) .and. dtype(i) .eq. 'Syst') then ! N.B. Criterion
          ns = ns + 1
          x(ns) = ql_in(i)
          qs(ns)= 10**x(ns)
          if(qu_in(i) .le. lmissing) nzero = nzero + 1
        endif
10    continue
          call dsvrgn(ns,x,x)
          call dsvrgn(ns,qs,qs)
      if( (ns-nzero) .le. 5) then    ! not enough data>0 to apply low-outlier test
        write(*,*) 'inadequate data for grubbs-beck test',ns-nzero
        write(*,*) 'all zeroes recoded as smaller than: ',qs(nzero+1)
        write(*,*) 'this is the smallest non-zero systematic obs.'
        gbcrit   = x(nzero+1)
        gbthresh = x(nzero+1)
        goto 25
      endif
c
c  Limit of use of MGBT Test; substitute BG when over half zeros
c
      if( (gbtype .eq. 'MGBT') .and. (nzero .ge. ns/2) ) then
        gbtype = 'GBT'
        write(*,*) 'too many zeros for MGBT test (ns, nzero) ',ns,nzero
        write(*,*) 'traditional Grubbs-Beck (GB) used instead'
      endif
c
c  Traditional Grubbs-Beck Test
c
      if(gbtype .eq. 'GBT') then     ! B17B Grubbs-Beck test for 1 outlier
          call p3est_ema(n-nzero,x(nzero+1),x(nzero+1),
     1                      1.d0,  1.d0,  1.d0,   ! At-site MSEs
     2                      0.d0,  1.d0,  0.d0,   ! Regional parameters (dumb)
     3                    -99.d0,-99.d0,-99.d0,   ! use no regional info
     4                      cmoms)                ! get moms
          xm      = cmoms(1)
          s       = sqrt(cmoms(2))
          t       = -0.9043+3.345*sqrt(log10(dble(ns-nzero))) ! N.B. ns, not n
     1                         -0.4046*log10(dble(ns-nzero))  ! Lu formula [JRS]
          gbcrit  = xm - s*t
          gbthresh= 1.d10   ! starting value to find gbthresh
        do 20 i=1,ns
          if(x(i) .ge. gbcrit) then
            gbthresh = min(x(i),gbthresh)
          endif
20      continue
c
c  Multiple Grubbs-Beck Test
c
      else if(gbtype .eq. "MGBT") then  ! Multiple Grubbs-Beck (Cohn, 2011)
          klow = MGBTP(qs,ns,pvaluew)
        if(klow .gt. 0) then
          gbcrit   = x(klow+1)
          gbthresh = gbcrit
        else
          if(nzero .gt. 0) then  ! zero flows are low outliers
            klow = nzero
            gbcrit = x(klow+1)/2.d0  ! smallest obs > 0 divided by 2...why not?
            gbthresh = x(klow+1)
          else
            gbcrit = -99.d0   !  no low outlier issues
            gbthresh = gbcrit
          endif
        endif
      endif
      
c  fill in various arrays and compute nlow
25    continue

          nlow    = 0
      do 30 i=1,n
        if(qu_in(i) .lt. gbcrit) then     ! note:
          nlow  = nlow+1
          qu(i) = gbthresh
          ql(i) = gbtmin
        else
          ql(i) = ql_in(i)
          qu(i) = qu_in(i)
        endif
          tl(i) = max(tl_in(i),gbcrit)  ! change made 13 aug 07 (TAC)
                                        ! note: recode to smallest X>Crit_GB
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
c     modified 2/8/1995    tac
c     modified 2/22/1995   tac
c     modified 12/16/1996  tac  fixed bias-correction factors
c                              and added second convergence criterion
c     modified 11/17/1998  tac  conv crit. added
c     modified 03/13/1999  tac  removed bias correction factors
c     modified 09/28/2011  tac  changed call arguments; added regional M, S2
c
c****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     modified 10/31/98 tac;  added regional skewness/fixed skewness
c                             capability;  note that call now has two
c                             additional arguments:
c                               r_G         is the regional skew
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
c            as_M_mse   r*8  mean square error of at-site mean (M)
c                              this is calculated by mse_ema
c            as_S2_mse  r*8  mean square error of at-site variance (S2)
c                              this is calculated by mse_ema
c            as_G_mse   r*8  mean square error of at-site skew (G)
c                              this is calculated by mseg_all
c            r_M        r*8  regional mean (M)
c            r_S2       r*8  regional variance (std.dev^2) (S2)
c            r_G        r*8  regional skew (G)
c            r_M_mse    r*8  mean square error of regional mean
c            r_S2_mse   r*8  mean square error of regional variance
c            r_G_mse    r*8  mean square error of regional skew
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variable: 
c       ------------------------------------------------------------------------
c
c            cmoms(3)   r*8  first 3 central moments 
c                              mc = {mean, variance, coeff. skew}
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     note: ql(i) is the lower bound for the i-th observation
c           qu(i) is the upper bound
c            if( qu == ql) then we have an ordinary observation at qu
c
c
      subroutine p3est_ema(n,ql,qu,
     1      as_M_mse,as_S2_mse,as_G_mse, ! use at-site MSEs
     2      r_M,     r_S2,     r_G,      ! and weighted regional info
     3      r_M_mse, r_S2_mse, r_G_mse,
     4      moms_out)

      implicit none
      
      integer
     1  n,nsize,i,j,k
      
      character*4 VarS2opt      
      common /tacR01/VarS2opt     
      common /reg001/rM,rMmse,rS2,rS2mse,rG,rGmse
      
      parameter (nsize=20001)
      
      double precision 
     1     ql(*),qu(*),as_M_mse,as_S2_mse,as_G_mse,
     1     r_M,     r_S2,     r_G,
     3     r_M_mse, r_S2_mse, r_G_mse,
     4     moms_out(3),
     2     moms(3,nsize),d11(nsize),dist_p3,tol,
     3     rM,rMmse,rS2,rS2mse,rG,rGmse
     
      double precision
     1     momsadj
      
      data tol/1.d-10/ 
      data moms(1,1),moms(2,1),moms(3,1)/0.0, 1.0, 0.0/

            rG    = r_G
            rGmse = r_G_mse       
      do 10 i=2,nsize
        call moms_p3(n,ql,qu,moms(1,i-1),moms(1,i))
c
c   compute weighted regional mean (M)? 
c    N.B. Uncertainty in at-site mean is based on previous call to 
c         p3est_ema() without regional info. A weighted average is
c         computed based on the mse of the at-site mean computed
c         without regional info and the generalized mean (which 
c         is defined on a "mean map"
c
        if(r_M_mse .gt. 0.d0 .and. r_M_mse .lt. 1.d10) then  ! => weighted M
          rMmse     = r_M_mse
          moms(1,i) = (as_M_mse*r_M + rMmse*moms(1,i))/
     1                 (as_M_mse + rMmse)
        else if (r_M_mse .eq. 0.d0) then
          moms(1,i) = r_M
        else
          ! moms(2,i) = moms(1,i)
        endif

c
c   compute weighted regional variance (S2)? 
c     note: we assume mse of regional estimate proportional to sigma^4
c           thus, if VarS2opt is "DF" (default) we adjust rS2mse
c           (see notes in Cohn 2011)
c           this is a tricky computation, and the arguments are 
c           non-trivial.
c
c
        if(r_S2_mse .ge. 1.d10) then     ! "At-Site/STATION"
c           moms(2,i) = moms(2,i)
        else if(r_S2_mse .lt. 0.d0) then  ! "At-Site/STATION"
c           moms(2,i) = moms(2,i)
        else if(r_S2_mse .eq. 0.d0) then ! "Regional/Generalized"
           moms(2,i) = r_S2
        else                             ! "WEIGHTED"
          if(VarS2opt .eq. 'DF') then
            rS2mse = r_S2_mse*(moms(2,i)/r_S2)**2 ! correct MSE[S] = f(\sigma)
          else if(VarS2opt .eq. 'S2') then
            rS2mse = r_S2_mse ! no adjustment
          else
            rS2mse = r_S2_mse ! no adjustment
          endif
            moms(2,i) = (as_S2_mse*r_S2 + rS2mse*moms(2,i))/
     1                  (as_S2_mse + rS2mse)
        endif

c
c   compute weighted regional skew (G)? 
c
          if(rGmse .le. 0.d0 .and. rGmse .gt. -98.d0) then ! "GENERALIZED, NO MSE"
            moms(3,i) = rG
          else if (rGmse .gt. 0.d0) then                  ! "WEIGHTED"
            moms(3,i) = (rG*as_G_mse+moms(3,i)*rGmse)/
     1                     (rGmse+as_G_mse)
          else if (rGmse .lt. -98.d0) then                ! "STATION SKEW
c            moms(3,i) = moms(3,i)
          else if (rGmse .ge. 1.d10) then                 ! "STATION SKEW
c            moms(3,i) = moms(3,i)
          endif
c
c  following call prevents skew from going below -1.41 and also
c  ensures that largest observation is within support of fitted distn
c
          moms(3,i) = momsadj(n,ql,moms(1,i))  

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
c            if(d11(i) .le. tol) then  ! tac added additional test 15 sep 11
            if( (d11(i) .le. tol) .and. (d11(i) .ge. d11(i-1)) ) then 
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
     4     choose,
     5     sk0,sk141,skxmax
     
      logical lskewXmax

      common /tac002/sk0,sk141,skxmax,lskewXmax,bcf

c     note that setting bcf to 0 shuts off bias correction;
c       1997 gets the original Cohn et al. [1997] bcfs
c       2004 gets Griffis et al. [2004] -- the default!

      if(n .gt. nsize) then
        write(*,*) '(moms_p3): sample size too large ',n
        stop
      endif
      
        n_e         = 0
        moms(1)     = 0.d0
      do 10 i=1,n
        if(ql(i) .eq. qu(i)) n_e = n_e+1
        if( (i .gt. 1) .and. (ql(i) .eq. ql(max(1,i-1))) .and. 
     1    (qu(i) .eq. qu(max(1,(i-1)))) ) then
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
     1  n,i,bcf
     
      double precision
     1  ql(*),m(3),xmax,parms(3),
     2  sk0,sk141,skxmax
     
      logical
     1  lskewXmax
     
      common /tac002/sk0,sk141,skxmax,lskewXmax,bcf

        xmax = ql(1)
      do 10 i=2,n
        if(ql(i) .gt. xmax) xmax = ql(i)
10    continue
        call m2p(m,parms)
        
        sk0     = m(3)
        sk141   = -1.41d0                           ! 1: m(3) < -1.41
        if(lskewXmax) then
          skxmax  = 2.d0*sqrt(m(2))/(m(1)-xmax)     ! 2:  parms(1) < xmax
        else
          skxmax  = sk141
        endif
        
        momsadj = max(sk0,sk141,skxmax)
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****|double precision function mseg_all
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     computes the mse of station skew for the bulletin 17b/17c analysis
c     (see p. 13 B17b)
c
c     n.b.  b17b recommends using h -- the entire period length --
c           for the record length with historical information.
c
c     n.b.  at_site_option, stored in common, determines how things are
c           computed.  
c
c               at_site_option = 'B17B' => Bulletin 17B MSE formula
c                              = 'EMA'  => First-order EMA MSE
c                              = 'ADJE' => Adjusted first-order
c
c           the 'ADJE' result is identical to the 'B17B' result if there is
c           no censored data.  Otherwise, 'ADJE' is the adjusted version 
c           (multiplied by a factor of the ratio 
c
c                bias_adj = MSE(B17B,syst=Nh)/MSE(EMA,syst=Nh)
c
c     timothy a. cohn, 2008
c
c     *** do not modify without author''s consent ***
c
c           author.......tim cohn
c           date.........02 jul 2008
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
     1    nobs(*),tl(*),tu(*),mc(3)
     
      double precision mseg,mse_ema,bias_adj,INF(2)

      integer 
     1    nthresh,i,n,n_adj
     
      character*4 at_site_option,at_site_default,at_site_std
      
      common /tacg04/at_site_option,at_site_default,at_site_std
      
      data INF/-99.d0,99.d0/
      
        n      = 0
      do 10 i=1,nthresh
        n      = n + nobs(i)
10    continue
      
c 
      if(at_site_option .eq. 'B17B') then
        mseg_all = mseg(n,mc(3))
      else if(at_site_option(1:3) .eq. 'EMA') then
        mseg_all = mse_ema(nthresh,nobs,tl,tu,mc,3)
      else if(at_site_option .eq. 'ADJE') then
        n_adj  = min(n,150)
        bias_adj = mseg(n_adj,mc(3)) /
     1             mse_ema(1,dble(n_adj),INF(1),INF(2),mc,3)
        mseg_all = bias_adj * mse_ema(nthresh,nobs,tl,tu,mc,3)
      endif
      
      return
      end

c****|double precision function mse_ema
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     computes the mse of station skew for an ema analysis
c
c     timothy a. cohn, 2008
c
c     *** do not modify without author''s consent ***
c
c           author.......tim cohn
c           date.........1 jul 2008
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
c            kmom         i*4  which moment? (1=mean; 2=variance; 3=skew)
c
c       output variables:
c       ------------------------------------------------------------------------
c            mse_ema      r*8  mse of at-site (station) skew
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision function mse_ema(nthresh,nobs,tl,tu,mc,kmom)

      implicit none
      
      double precision
     1    nobs(*),tl(*),tu(*),mc(3),mc2(3),tl2(100),tu2(100),
     2    s_mn(3,3),s_mc(3,3),mn(3),
     3    skewmin,tneg,tpos,w
     
      parameter (skewmin=0.06324555)

      integer 
     1    nthresh,i,kmom

c   test input value of kmom     
      if(kmom .gt. 3 .or. kmom .lt. 1) then
        write(*,*) 'kmom error ',kmom,' (mse_ema)'
        stop
      endif
      
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
        mse_ema = s_mc(kmom,kmom)
      else  !  use weighted sum for skew = skewmin, -skewmin
        mc2(3) = -skewmin
        call var_mom(nthresh,nobs,tl2,tu2,mc2,s_mn)
        call m2mn(mc2,mn)
        call mn2m_var(mn,s_mn,mc2,s_mc)
        tneg = s_mc(kmom,kmom)
        mc2(3) = skewmin
        call var_mom(nthresh,nobs,tl2,tu2,mc2,s_mn)
        call m2mn(mc2,mn)
        call mn2m_var(mn,s_mn,mc2,s_mc)
        tpos = s_mc(kmom,kmom)
        w    = (mc(3)-skewmin)/(2.d0*skewmin)
        mse_ema = w*tneg + (1.d0-w)*tpos
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
c            r_G_mse      r*8  mse of g_r (b17) ; 
c                            this variable encodes four distinct cases:
c                            1) r_G_mse = 0 ("GENERALIZED SKEW, NO ERROR")
c                                use fixed g = r_G w/ mse = 0.0
c                            2) -98 < r_G_mse < 0 ("GENERALIZED SKEW, MSE > 0")
c                                use fixed g = r_G w/ mse = -r_G_mse  
c                            3) 0 < r_G_mse < 1.d10 ("WEIGHTED SKEW")
c                                use g = weighted average of at-site and 
c                                regional skew (b17b recommendation)
c                            4) 1.d10 < r_G_mse ("STATION SKEW")
c                                use g = at-site skew
c            r_S2       r*8  regional variance (std.dev^2)
c            r_S2_mse   r*8  mean square error of regional variance
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
     1    (nthresh,nobs,tl_in,tu_in,mc_in,pq,
     2         r_S2, r_M_mse, r_S2_mse, r_G_mse,
     4         yp,cv_yp_syp)
     
        implicit none
          save

        integer 
     1     nthresh,
     2     i,n,nth_p
     
        parameter (nth_p=100)
     
        double precision
     1    nobs(nth_p),tl_in(nth_p),tu_in(nth_p),pq,
     2    yp,cv_yp_syp(2,2),
     3    mc(3),mc_in(3),parms(3),
     4    tl(nth_p),tu(nth_p),
     5    skewmin,s_mc(3,3),
     6    tmp(2,3),jac(3,2),
     7    r_S2, r_M_mse, r_S2_mse, r_G_mse
     
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
      call regmoms(nthresh,nobs,tl,tu,mc,
     1                    r_G_mse,r_M_mse,r_S2,r_S2_mse,s_mc)
      
      call jacq2(pq,mc,jac(1,1))

      call jacs2(
     1 nthresh,nobs,tl,tu,mc,r_G_mse,r_M_mse,r_S2,r_S2_mse,pq,jac(1,2))

      call dmxtyf(3,2,jac,3,3,3,s_mc,3,2,3,tmp,2)

      call dmrrrr(2,3,tmp,2,3,2,jac,3,2,2,cv_yp_syp,2) 

      return
      end

c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
c****|subroutine regmoms(nthresh,nobs,tl,tu,mc,r_G_mse,r_S2,r_S2_mse,s_mc)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     *** do not modify without authors consent ***
c
c           author.......tim cohn
c           date.........10 feb 2007
c             modified...16 jun 2011
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
c            r_G_mse    r*8  scalar mse of regional skewness estimator 
c                            N.B.  the mse codes for four distinct cases
c                              1) mse>0 => weighted skew
c                              2) mse=0 => generalized skew (no uncertainty)
c                              3) -98< mse <0 => generalized skew (mse = -mse) 
c                              4) mse < -98 => station skew 
c                                   (using r_G_mse = 1.d10 is equivalent)
c            r_M_mse    r*8  estimated MSE of M
c            r_S2       r*8  regional estimate of variance (S^2)
c            r_S2_mse   r*8  estimated MSE of r_S2
c
c       output variables:
c       ---------------------------------------------------------------
c            s_mc(3,3)  r*8  matrix of covariance of 
c                              n.b: central parameter estimators
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      subroutine regmoms(
     1  nthresh,nobs,tl,tu,mc,r_G_mse,r_M_mse,r_S2,r_S2_mse,s_mc)
      
      implicit none
      
      integer
     1  nthresh
      
      double precision
     1  nobs(*),tl(*),tu(*),mc(3),r_G_mse,s_mc(3,3),
     2  s_mn(3,3),mn(3),wG,r_M,r_M_mse,wM,r_S2,r_S2_mse,rS2mse,wS2
     
      double precision mseg_all

      character*4 VarS2opt
      
      common /tacR01/VarS2opt
     
     
      call var_mom(nthresh,nobs,tl,tu,mc,s_mn)
      
      call m2mn(mc,mn)
      
      call mn2m_var(mn,s_mn,mc,s_mc)

c
c  compute variance of regional skew
c
        if(r_G_mse .gt. 0.d0) then   ! "WEIGHTED"
           wG = r_G_mse/(mseg_all(nthresh,nobs,tl,tu,mc) + r_G_mse)
        else if(r_G_mse .ge. -98.d0 .and. r_G_mse .le. 0.d0) then ! "G-w/skew"
          wG =  0.d0
        else if(r_G_mse .lt. -98.d0) then ! "STATION"
          wG =  1.d0
        else if(r_G_mse .ge. 1.d10) then  ! "STATION"
          wG =  1.d0
        endif
      
        s_mc(1,3) = wG*s_mc(1,3)
        s_mc(3,1) = s_mc(1,3)
        s_mc(2,3) = wG*s_mc(2,3)
        s_mc(3,2) = s_mc(2,3)
        s_mc(3,3) = wG**2*s_mc(3,3) + (1.d0-wG)**2*abs(r_G_mse)

c
c  variance of mean (M)
c

      if(r_M_mse .lt. 1.d10 .and. r_M_mse .ge. 0.d0) then    ! do nothing?
        if(r_M_mse .eq. 0.d0) then       ! "Regional/Generalized"
          wM = 0.d0
        else                             ! "WEIGHTED"
          wM = r_M_mse/(s_mc(1,1)+r_M_mse)
        endif
      
        s_mc(1,2) = wM*s_mc(1,2)
        s_mc(2,1) = s_mc(1,2)
        s_mc(1,3) = wM*s_mc(1,3)
        s_mc(3,1) = s_mc(1,3)
        s_mc(1,1) = wM**2*s_mc(1,1) + (1.d0-wM)**2*abs(r_M_mse)
      endif

c
c  variance of variance (standard deviation)
c
        if(r_S2_mse .gt. 1.d10) return   ! most likely case -- do nothing
               
        if(r_S2_mse .ge. 1.d10) then     ! "At-Site/STATION"
           wS2 = 1.D0
        else if(r_S2_mse .eq. 0.d0) then ! "Regional/Generalized"
           wS2 = 0.d0
        else if(r_S2_mse .lt. 0.d0) then  ! "At-Site/STATION"
           wS2 =  1.d0
        else                             ! "WEIGHTED"
          if(VarS2opt .eq. 'DF') then
            rS2mse = r_S2_mse*(mc(2)/r_S2)**2 ! correct MSE[S] = f(\sigma)
          else if(VarS2opt .eq. 'S2') then
            rS2mse = r_S2_mse ! no adjustment
c         else if(VarS2opt .eq. 'S1') then
c           rS2mse = ???
          else
            rS2mse = r_S2_mse ! no adjustment
          endif
c                                             ! 
          wS2 = rS2mse/(s_mc(2,2)+rS2mse)
        endif
      
        s_mc(1,2) = wS2*s_mc(1,2)
        s_mc(2,1) = s_mc(1,2)
        s_mc(2,3) = wS2*s_mc(2,3)
        s_mc(3,2) = s_mc(2,3)
        s_mc(2,2) = wS2**2*s_mc(2,2) + (1.d0-wS2)**2*abs(rS2mse)

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
     2  mc2,q2,error,qP3,
     3  xmin,xmax
     
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
c  indx=3 involves partial deriv wrt skew; ind=1,2 would be m, s
      do 10 ind=3,3
ctac  ensure skews within range of reliable computation (tac 06/18/08)
        if(mc2(3) .gt. 0.d0) then
          xmin = max(-0.1d0,-mc2(3)/2.d0)
          xmax = 0.1d0
        else
          xmin = -0.1d0
          xmax = min(0.1d0,-mc2(3)/2.d0)
        endif
ctac
      call diff(1,0.d0,xmin,xmax,fjac,0.d0,0.d0,jac(ind),error,ifail)
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
c****|double precision function sypf(nt,nobs,tl,tu,mc,
c                                      r_M_mse,r_G_mse,r_S2,r_S2_mse,q)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     *** do not modify without author''s consent ***
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
c            r_G_mse    r*8  scalar mse of regional skewness estimator 
c                              (see comments above)
c            r_S2       r*8  regional variance (std.dev^2)
c            r_S2_mse   r*8  mean square error of regional variance
c            q          r*8  quantile to be estimated (in range (0-1)
c
c       output variables:
c       ---------------------------------------------------------------
c            result     r*8  estimated standard deviation of y-hat-q
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision function sypf(
     1    nthresh,nobs,tl,tu,mc,
     2       r_G_mse,r_M_mse,r_S2,r_S2_mse,q)
      
      implicit none
      
      integer
     1  nthresh
      
      double precision
     1  nobs(*),tl(*),tu(*),mc(3),r_G_mse,q,s_mc(3,3),
     2  jac(3),tmp(3),result,r_M_mse,r_S2,r_S2_mse
          
      call regmoms(nthresh,nobs,tl,tu,mc,
     1               r_G_mse,r_M_mse,r_S2,r_S2_mse,s_mc)
      
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
          write(*,*) 'rGmse',r_G_mse
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
c     *** do not modify without author''s consent ***
c
c           author.......tim cohn
c           date.........10 feb 2007
c              modified..28 sep 2011 (tac)
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
c            r_G_mse    r*8  scalar mse of regional skewness estimator 
c            r_M_mse    r*8  mean square error of regional mean
c            r_S2       r*8  regional variance (std.dev^2)
c            r_S2_mse   r*8  mean square error of regional variance
c            q          r*8  quantile to be estimated (in range (0-1)
c
c       output variables:
c       ---------------------------------------------------------------
c            jacss(3)   r*8  jacobian of syp wrt (m,s^2,g)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine jacs2(
     1  nthresh,nobs,tl,tu,mc,r_G_mse,r_M_mse,r_S2,r_S2_mse,q,jacss)

      implicit none
      
      integer
     1  nthresh,i
      
      double precision
     1  nobs(*),tl(*),tu(*),mc(3),r_G_mse,q,jacss(3),
     3  sypf,dgv(3),delta,mc_l(3),mc_u(3),d,r_M_mse,r_S2,r_S2_mse
     
      data dgv/1.d0,2.d0,6.d0/,d/0.05d0/
      
      
      do 10 i=1,3
        delta = d*sqrt(dgv(i)*mc(2)/nobs(nthresh+1))
        call mcadj(mc,i,delta,mc_l,mc_u)
        jacss(i) = 
     1     (sypf(nthresh,nobs,tl,tu,mc_u,
     2               r_G_mse,r_M_mse,r_S2,r_S2_mse,q) -
     1      sypf(nthresh,nobs,tl,tu,mc_l,
     2               r_G_mse,r_M_mse,r_S2,r_S2_mse,q) )/
     1              (2.d0*delta)
10    continue
c
c   The following only works if FORTRAN compiler allows re-entrant code
c     i.e. arguments must be passed in the stack (value or addresses)
c
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
      else
        do 37 i=1,n
          mwh(i) = 0.0
37      continue        
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
     4  fp_g3_pdf,fp_z_pdf,whlp2z
     
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
     4  fp_g3_cdf,fp_z_cdf,whlp2z
     
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
     4  fp_g3_icdf,fp_z_icdf,whz2lp,infinity
     
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
      else
        qP3g = 0.0
      endif
c
c  compute cdf for small skews using wilson-hilferty transformation
c
      if(wwh .gt. 0.d0) then
        mu = m(1)
        s  = sqrt(m(2))
        qP3wh = mu+s*whz2lp(fp_z_icdf(q),g)
      else
        qP3wh = 0.0
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
      
      real rand
      
      rP3 = qP3(dble(rand()),m)
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
     3 fp_z_icdf,kfxx
     
      zc     = fp_z_icdf(c)
      kgwp   = kfxx(skew,p)

      a      = 1.d0 - zc**2/(2.d0*(n-1.d0))
      b      =  kgwp**2 - zc**2/n
      
      kupc   =  (kgwp + sqrt(kgwp**2 - a*b))/a
      klpc   =  (kgwp - sqrt(kgwp**2 - a*b))/a
      
      return
      end
            
      
      double precision function kfxx(skew,prob)
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

      kfxx = qp3(prob,m)
      
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
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      blockdata emablk
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     author.....tim cohn
c     date.......27 feb 2008
c     
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      implicit none
      
      double precision
     1  gbcrit,gbthresh,alph01,alph10,pvaluew,qs,
     2  sk0,sk141,skxmax
     
      integer
     1  nlow,nzero,bcf
      
      character*4 
     1  at_site_option,at_site_default,at_site_std,gbtype,VarS2opt
      
      logical lskewXmax
      
      logical cirun
      
      common /tac002/sk0,sk141,skxmax,lskewXmax,bcf
      common /tacg04/at_site_option,at_site_default,at_site_std
      common /tacg01/gbcrit,gbthresh,pvaluew(10000),qs(10000),
     1               nlow,nzero,gbtype
      common /tacmg1/alph01,alph10
      common /tacdgb/cirun
      common /tacR01/VarS2opt
      
      data at_site_option/'ADJE'/
      data at_site_default/'ADJE'/
      data at_site_std/'ADJE'/
      
      data cirun/.TRUE./
      data alph01/0.01d0/,alph10/0.10d0/

      data sk141/-1.41d0/
      data lskewXmax/.FALSE./

      data bcf/2004/

      data VarS2opt/'DF'/
      end
      

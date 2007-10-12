c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      subroutine emafit(n,ql,qu,tl,tu,reg_skew,reg_mse,pq,
     1                  cmoms,yp,ci_low,ci_high)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

c    
c    This routine fits the Pearson Type III distribution to 
c    a data set using the EMA algorithm 
c
c    This was prepared for Aquaterra by Tim Cohn, US Geological Survey
c       
c    Timothy A. Cohn        09 November 2003
c
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or other)
c            ql(n)      r*8  vector of lower bounds on (log) floods
c            qu(n)      r*8  vector of upper bounds on (log) floods
c            tl(n)      r*8  vector of lower bounds on (log) flood threshold
c            tu(n)      r*8  vector of upper bounds on (log) flood threshold
c            reg_skew   r*8  regional skew
c            reg_mse    r*8  mean square error of regional skew
c            pq         r*8  quantile to be estimated 
c                            --pq=0.99 corresponds to "100-year flood"
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       N.B.  There are two distinct concepts here:
c            a.  The observations, which are described by {ql,qu};
c                  specifically, the true value of q is assumed to lie
c                    wihin the interval ql(i) <= q(i) <= qu(i);
c                  if q(i) is known exactly, then ql(i) = q(i) = qu(i);
c
c                  {ql,qu} are required by EMA to fit P3 dist'n to data
c
c            b.  The censoring pattern, which is described by {tl,tu}
c                  {tl,tu} define interval on which data get observed:
c                   if q is not inside {tl,tu}, q is either left or right censored.
c                  Examples:
c                    {-inf,+inf}  => Systematic data (everything known exactly)
c                    {T,   +inf}  => Historical data (q>=t known exactly; q<t censored)
c                
c                  {tl,tu} are required by EMA to estimate confidence intervals
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ---------------------------------------------------------------------------
c            cmoms(3)   r*8  first 3 central moments {mean,variance,coeff. skew}
c            yp         r*8  estimated pq-th quantile of fitted P3 distribution
c            ci_low     r*8  left end of 95% confidence interval for pq-th quantile
c            ci_high    r*8  right end of 95% confidence interval for pq-th quantile
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c

      implicit none

      integer ntmax
      parameter (ntmax=100)
      
      integer
     1  n                                             ! input variables
     
      integer
     1  k,i,nt,neps
     
      double precision
     1  ql(*),qu(*),tl(*),tu(*),reg_skew,reg_mse,pq,  ! input variables
     2  cmoms(3),yp,ci_low,ci_high                    ! output variables

      double precision
     1  reg_wgt,parms(3),syp,s_syp,cv_yp_syp,eps(1),xmu,
     2  tl2(ntmax),tu2(ntmax),nobs(ntmax)
     
      double precision
     1  mseg
     
      data neps/1/,eps/0.95d0/

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   1.  begin by fitting the P3 dist'n to the at-site and regional data
c       
c       N.B.  Two calls are necessary:
c            a.  Compute the at-site moments without employing regional skewn
c                   this is used to compute the appropriate weight for regional skew
c            b.  Final computation using regional skew information
c
        reg_wgt = 0.d0
      call p3_mom_bin(reg_skew,reg_wgt,n,ql,qu,parms)
c       write(*,*) 'sys parms ',parms
c        call p2m(parms,cmoms)
c       write(*,*) 'sys cmoms ',cmoms
c        reg_wgt =  1.d0 - reg_mse/(mseg(n,cmoms(3)) + reg_mse)
c      call p3_mom_bin(reg_skew,reg_wgt,n,ql,qu,parms)
c       write(*,*) 'b17 parms ',parms

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   1.1  fix for skews close to zero; preserves mean and variance
c
      if(parms(2) .gt. 1000.d0) then
        xmu      = parms(1)+parms(2)*parms(3)
        parms(3) = sqrt(parms(2)/1000.d0)*parms(3)
        parms(2) = 1000.d0
        parms(1) = xmu - parms(2)*parms(3)
      endif

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   1.2  convert to central moments
c

        call p2m(parms,cmoms)
c       write(*,*) 'b17 cmoms ',cmoms
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   2.  compute the variance-covariance of predicted values and parameters
c     a.  organize the data
c
        k = 0
      do 10 i=1,n
        if(i.eq.1 .or. tl(i).ne.tl2(k) .or. tu(i).ne.tu2(k)) then
          k=k+1
          tl2(k) = tl(i)
          tu2(k) = tu(i)
          nobs(k) = 1
        else
          nobs(k) = nobs(k) + 1
        endif
10    continue
        nt = k
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     b.  call subroutine var_p_r_ci
c
      write(*,*)'EMAFIT: calling var_p_r_ci'
      write(*,*)'  nt ',nt
      write(*,*)'  tl2 ',tl2
      write(*,*)'  tu2 ',tu2
      write(*,*)'  nobs ',nobs
      write(*,*)'  parms ',parms
      write(*,*)'  pq ',pq
      write(*,*)'  regmse ',reg_mse
c      do 15 i=1,nt
c        write(*,*)'tl2,tu2',tl2(i),tu2(i)
c 15   continue
      call var_p_r_ci(nt,tl2,tu2,nobs,parms,pq,
     1         reg_mse,yp,syp,s_syp,cv_yp_syp)
      write(*,*)'yp,syp,ssyp,cvypsyp ',yp,syp,s_syp,cv_yp_syp

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   3.  compute confidence intervals
c
      call ci_ema_m03
     1    (yp,syp,s_syp,cv_yp_syp,neps,eps,ci_low,ci_high)
       write(*,*)'neps,eps,cilow,cihigh ',neps,eps,ci_low,ci_high
     
      return
      end

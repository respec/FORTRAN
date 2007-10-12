c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     this file contains a collection of fortran subroutines 
c     and functions connected with ema estimation 
c
c     these routines include procedures both to estimate with
c     historical flood information and with regional ("generalized") skew
c
c     author........tim cohn, 14 june 2000
c        modified...24 oct 2003 (tac)
c
c     [see cohn et al., 1997; 2001]
c
c       routine name    purpose
c       ------------    -------
c       ci_ema_m01      computes "simple" confidence intervals
c       ci_ema_m02      computes "adjusted" confidence intervals
c                        --normal approximation
c       ci_ema_m03      computes "adjusted" confidence intervals
c                        --student's t approximation
c       m_g_2p          computes "weighted" skew from "station" and "generalized" skew
c       m2mn_var        computes linearized v-cv of non-central lp-3 moments
c                       based on v-cv of central moments
c       mn2m_var        computes linearized v-cv of central lp-3 moments
c                       based on v-cv of non-central moments
c       var_m_r         combines at-site c-cv of non-central moments
c                       and regional ("generalized") skew  
c                       to get weighted moments estimators (skew) and
c                       the v-cv matrix of non-central moments
c       var_p_r_ci      computes: 
c                          yp    -- the estimated pth quantile of a pearson type 3 variate
c                          syp   -- the estimated standard deviation of yp
c                          s_syp -- the standard deviation of the standard deviation of yp
c                          cv    -- a (1-epsilon) confidence interval for yp
c       var_syp         support routine
c       var_yp          support routine
c       varmom          support routine
c       var_m_r4        combines augmented variance-covariance matrix of parameters
c                          {m1,m2,m3,gg}
c
c   n.b.  these routines are designed to work in the context of other routines contained in:
c       p3_mom_ema_regxxx.f    ema estimation routines
c       probfun_xxx.f          probability functions
c       imslfakexxx.f          fake imsl routines (real imsl is better, if you have access)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       given estimated yp, syp, and cv_yp_syp, this program will compute:
c
c          1.   cv    -- a (1-epsilon) confidence interval for yp
c
c       n.b.  these are all asymptotic results
c             the m03 results are pretty accurate; 
c             m01 not so good in small samples. 
c        these are the "simple" ci's in cohn et al 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     n.b.  two subroutine calls are required to get ci's
c           -first, you need to compute assymptotic variance-covariance
c            matrix of quantile estimator and its estimated
c            standard deviation:
c            
c              call var_p_r_ci(nt,tl,tu,no,parm,pr(1),yp,syp,s_syp,cv_yp_syp)
c
c           -then call the ci formula you want to use:
c        
c              call ci_ema_m03(yp,syp,s_syp,cv_yp_syp,nci,ci,ci_low,ci_high)    
c
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     copyright, timothy a. cohn, 1999
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........september 7, 1999
c            modified....february 12, 2000 (tac)
c            modified....18 june, 2000 (tac)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            yp         r*8  estimated p-th quantile
c            syp        r*8  estimated standard deviation of yp
c            s_syp      r*8  estimated standard error of syp
c            cv_yp_syp  r*8  estimated cov[yp,syp] (2x2 matrix)
c            neps       i    number of quantile pairs to compute
c            eps(*)     r*8  vector of ci coverages (usually 0.90,0.99,0.999)
c
c
c       output variables:
c       ---------------------------------------------------------------------------
c            ci_low(*)  r*8  estimated lower critical point of confidence interval
c            ci_high(*) r*8  estimated upper critical point of confidence interval
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    m01 is the first, truly simple confidence interval formula
c
c      date..........8 sept 1999 (tac)
c
        subroutine ci_ema_m01
     1    (yp,syp,s_syp,cv_yp_syp,neps,eps,ci_low,ci_high)
     
        implicit none
          save
        
        integer 
     1     i,neps
     
        double precision
     1    yp,syp,s_syp,cv_yp_syp(2,2),eps(*),ci_high(*),ci_low(*),
     4    p_high,p_low,z

        double precision
     1    fp_z_icdf

c
c    compute confidence intervals
c    n.b.  sign on t means that low t corresponds to high yp
c

      do 10 i=1,neps
           p_low    =  (1.d0-eps(i))/2.d0
           z        =  fp_z_icdf(p_low)
         ci_low(i)  =  yp + syp*z
           p_high   =  1.d0 - p_low
           z        =  fp_z_icdf(p_high)
         ci_high(i) =  yp + syp*z

10      continue
        return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    m02 includes correction for covariance of yp, syp, in confidence interval formula
c
c      date..........8 sept 1999 (tac)
c
        subroutine ci_ema_m02
     1    (yp,syp,s_syp,cv_yp_syp,neps,eps,ci_low,ci_high)
     
        implicit none
          save

        integer 
     1     i,neps
     
        double precision
     1    yp,syp,s_syp,cv_yp_syp(2,2),eps(*),ci_high(*),ci_low(*),
     4    beta1,p_high,p_low,z

        double precision
     1    fp_z_icdf

          beta1       =  cv_yp_syp(1,2)/cv_yp_syp(1,1)

c
c    compute confidence intervals
c    n.b.  sign on t means that low t corresponds to high yp
c

      do 10 i=1,neps
           p_low    =  (1.d0-eps(i))/2.d0
           z        =  fp_z_icdf(p_low)
         ci_low(i)  =  yp + syp*z/max(0.5d0,1.d0-beta1*z)
           p_high   =  1.d0 - p_low
           z        =  fp_z_icdf(p_high)
         ci_high(i) =  yp + syp*z/max(0.5d0,1.d0-beta1*z)

10      continue
        return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    m03 is like m02 but uses students t distribution in 
c        calculation of confidence intervals
c        these are the "adjusted" ci's in cohn et al 2000
c
c      date..........8 sept 1999 (tac)
c
        subroutine ci_ema_m03
     1    (yp,syp,s_syp,cv_yp_syp,neps,eps,ci_low,ci_high)
     
        implicit none
          save

        integer 
     1     i,neps
     
        double precision
     1    yp,syp,s_syp,cv_yp_syp(2,2),eps(*),ci_high(*),ci_low(*),
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
            write(*,*) 'nu too small',nu
            nu = nu_min
        endif

c
c    compute confidence intervals
c    n.b.  sign on t means that low t corresponds to high yp
c

      do 10 i=1,neps
           p_high   =  (1.d0+eps(i))/2.d0
           t        =  fp_tnc_icdf(p_high,nu,0.d0)
         ci_high(i) =  yp + syp*t/max(c_min,1.d0-beta1*t)
           t        =  -t
         ci_low(i)  =  yp + syp*t/max(c_min,1.d0-beta1*t)

10      continue
        return
      end
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
c     copyright, timothy a. cohn, 2000
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........1 july 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            nthresh      i    number of distinct censoring thresholds ( (a,b) pairs)
c            tl_in(*)     r*8  vector of lower bounds (a)
c            tu_in(*)     r*8  vector of upper bounds (b)
c            nobs(*)      r*8  vector of number of observations corresponding to threshold
c                                pair (a(i),b(i)
c            parms_in(3)  r*8  vector of estimated p-3 parameters (tau,alpha,beta)
c            pq           r*8  quantile to be estimated
c            g_r_mse      r*8  mse of g_r (b17) ; negative values => ignore regional
c                              skewness
c
c            n.b.:  g_r, the regional skewness, is set = sign(2,parms(3))/sqrt(parms(2))
c
c       output variables:
c       ---------------------------------------------------------------------------
c            yp         r*8  estimated p-th quantile
c            syp        r*8  estimated standard deviation of yp
c            s_syp      r*8  estimated standard error of syp
c            cv_yp_syp  r*8  estimated cov[yp,syp]
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        subroutine var_p_r_ci
     1    (nthresh,tl_in,tu_in,nobs,parms_in,pq,
     2     g_r_mse,yp,syp,s_syp,cv_yp_syp)
     
        implicit none
          save

        integer 
     1     nthresh,i,nth_p,
     2     n
     
        parameter (nth_p=100)
     
        double precision
     1    tl_in(nth_p),tu_in(nth_p),nobs(nth_p),parms_in(3),pq,
     2    a_max,g_r_mse,yp,syp,s_syp,cv_yp_syp(2,2),jac_syp(4),
     3    jac_yp(4),mc(3),moms_a(4),parms(3),pdum(3),
     4    t1(4),tl(nth_p),tu(nth_p),
     4    varm_a(4,4),w

        double precision
     1    fp_g3_icdf
     
        data a_max/1.d99/
    
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    orthogonalize (sort of) the first three moments
c      n.b.  note 4-th parameter:  generalized skew (!)
c      why not?  the matrix algebra works, and everything is linear
c
          yp       =   fp_g3_icdf(pq,parms_in)
          parms(1) = - parms_in(2)*parms_in(3)
        parms(2) =   parms_in(2)
        parms(3) =   parms_in(3)

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    check to make sure that alpha is not too large => abs(skew) too small
c       force skew to be +/- 0.05 => alpha = 4/skew^2 = 1600
c
        if(parms(2) .gt. a_max) then
          call p2m(parms,mc)
          write(*,*) 'parms: ',parms
          write(*,*) 'momsc: ',mc
          mc(3)  =   sign(2.d0/sqrt(a_max),mc(3))
          call m2p(mc,parms)
          write(*,*) 'parms: ',parms
          write(*,*) 'momsc: ',mc
        endif
        
          n      =   0
        do 20 i=1,nthresh
          n      =   n + nobs(i)
          tl(i)  =   tl_in(i) + (parms(1) - parms_in(1))
          tu(i)  =   tu_in(i) + (parms(1) - parms_in(1))
20        continue

c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    compute augmented covariance matrix
c
c    n.b.  moms_a is a 4-component vector containing {m1,m2,m3,g_r}
c    
      call var_m_r4(nthresh,tl,tu,parms,nobs,g_r_mse,varm_a)
      
      call p2mn(parms,moms_a)
        moms_a(4)   = sign(2.d0,parms(3))/sqrt(parms(2))
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     n.b.  compute regional weighting factor, w, based on estimator:
c
c       g_avg  =  w * mc(3) + (1.d0-w) * g_r
c
c     also:  mseg is a function based on the bulletin 17b equation for mse of skew
c
c       call mn2m(moms_a,mc)
c       w           =  g_r_mse/(mseg(n,mc(3)) + g_r_mse)

       call m_g_2p(moms_a,n,moms_a(4),g_r_mse,w,pdum)
       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    compute variance of yp
c    

        call var_yp(varm_a,moms_a,w,pq,cv_yp_syp(1,1),jac_yp)
      
        syp  =  sqrt(cv_yp_syp(1,1))
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    compute variance of syp (yep, it works to do it this way!)
c    
        call var_syp
     1    (nthresh,tl,tu,parms,nobs,pq,g_r_mse,w,
     2     cv_yp_syp(2,2),jac_syp)
         
          s_syp  =  sqrt(cv_yp_syp(2,2))
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    compute covariance of yp, syp
c
      call dmxtyf(4,1,jac_yp,4,4,4,varm_a ,4,1,4,t1            ,1)
        call dmrrrr(1,4,t1    ,1,4,1,jac_syp,4,1,1,cv_yp_syp(1,2),1)

        cv_yp_syp(2,1)   =  cv_yp_syp(1,2)

        return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       computes the variance of yp and the jacobian with respect to moments
c          varm         matrix of moments variances
c          moms_a        vector of parameters
c          pq       percentile of distribution
c          varyp       variance of yp
c          jac      output jacobian vector of length 3
c
        subroutine var_yp(varm,moms_a,w,pq,varyp,jac)

        implicit none
          save
      
        double precision 
     1    varm(4,4),moms_a(4),pq,varyp,jac(4,1),
     2    t1(1,4),w
     
      call jacob_yp(moms_a,w,pq,jac)

        call dmxtyf(4,1,jac,4,4,4,varm,4,1,4,t1,1)
        call dmrrrr(1,4,t1,1,4,1,jac,4,1,1,varyp,1)
      
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       computes the jacobian of yp with respect to the moments and g_r
c          p    vector of parameters, (t,a,b,g_r)
c          pq   percentile of distribution
c          jac  output jacobian vector of length 4
c
        subroutine jacob_yp(m,w,pq_in,jac)

        implicit none
          save
      
        double precision 
     1   m(4),w,pq_in,jac(1,4),
     2   alpha,g_r,jt1(3),jact(3,4),p(3),pq,pqs,q1p
     
        double precision
     1    d_g1_inv,fp_g1_icdf

        pq = pq_in

        call mn2p(m,p)
        g_r = sign(2.d0,p(3))/sqrt(p(2))
           
          alpha =  p(2)
          if(p(3) .gt. 0.d0) then
            pqs  =  pq
          else
            pqs  =  1.d0 - pq
          endif
          q1p   =  fp_g1_icdf(pqs,alpha)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   first:  compute partial derivatives
c
c            jt1   =  [  dq/dt  dq/da  dq/db  ]
c
             
        jt1(1)  =  1.d0
        jt1(2)  =  p(3)*d_g1_inv(pqs,alpha)
        jt1(3)  =  q1p

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   next:  call jacob_r_p to compute jacobian of parameters wrt 4 moments:
c
c                        [ dt/dm1,   dt/dm2,   dt/dm3,   dt/dg_r ]
c                        |                                       |
c            jact   =    | da/dm1,   da/dm2,   da/dm3,   da/dg_r ]
c                        |                                       |
c                        [ db/dm1,   db/dm2,   db/dm3,   db/dg_r ]
c
          call jacob_r_p(m,g_r,w,jact)

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   next:  apply chain rule to obtain 
c
c                               [  dq/dm1 ]
c                               |         |
c            jac  = jt1.jact =  |  dq/dm  |
c                               |         |
c                               [  dq/dm3 |
c                               |         |
c                               [  dq/g_r ]
c
        call dmrrrr(1,3,jt1,1,3,4,jact,3,1,4,jac,1)

        return
        end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       computes the variance of syp with respect to the moments
c          varm         matrix of moments variances
c          parms        vector of parameters
c          pq       percentile of distribution
c          jac      output jacobian vector of length 3
c
        subroutine var_syp
     1    (nth_in,tl_in,tu_in,parms,n_in,pq_in,g_r_mse_in,w_in,
     2     varsyp,jac)

        implicit none
          save
      
        integer 
     1    nth_in,
     2    i,nct,nthresh,nth_p
     
        parameter (nth_p=100)
     
        double precision 
     1    tl_in(nth_p),tu_in(nth_p),parms(3),n_in(nth_p),
     2    pq_in,g_r_mse_in,w_in,varsyp,jac(4),
     3    g_r,g_r_mse,m(4),n,t1(4),tl,tu,varm_a(4,4),w
     
        common /zd07/tl(nth_p),tu(nth_p),n(nth_p),nthresh,nct
      
        common /zd10/g_r,g_r_mse,w
         g_r_mse    = g_r_mse_in
         w          = w_in

         nthresh =  nth_in
         nct     =  0
      do 10 i=1,nthresh
         tl(i)   =  tl_in(i)
         tu(i)   =  tu_in(i)
         n(i)    =  n_in(i)
         nct     =  nct+n(i)
10      continue

        call p2mn(parms,m)
      m(4)  =  sign(2.d0,parms(3))/sqrt(parms(2))

      call jacob_syp(m,w,pq_in,jac)

      call var_m_r4(nthresh,tl,tu,parms,n,g_r_mse,varm_a)

        call dmxtyf(4,1,jac,4,4,4,varm_a,4,1,4,t1,1)
 
        call dmrrrr(1,4,t1,1,4,1,jac,4,1,1,varsyp,1)

      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       computes the jacobian of syp with respect to the moments
c          p    vector of parameters
c          pq   percentile of distribution
c          jac  output jacobian vector of length 3
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
        subroutine jacob_syp(m_in,w_in,pq_in,jac)

        implicit none
          save
      
      integer
     1    i,ifail,ifast2,j,nct,nthresh,nth_p
     
        parameter (nth_p=100)
      
        double precision 
     1    m_in(4),w_in,pq_in,jac(4),
     2    acc,delta,deriv,eps,error,g_r,g_r_mse,m,p(3),
     3    pq,n,tl,tu,varm_a(4,4),w,xmin,xmax
     
        double precision
     1    fcdfsy

        external fcdfsy

        common /zd05/m(4),pq,j

        common /zd07/tl(nth_p),tu(nth_p),n(nth_p),nthresh,nct

        common /zd09/ifast2

        common /zd10/g_r,g_r_mse,w

        data eps/1.d-6/,acc/1.d-9/

          w    =  w_in
          pq   =  pq_in
      do 10 i=1,4
          m(i) = m_in(i)
10      continue

          call mn2p(m,p)
c      m(4) =  g_r

        call var_m_r4(nthresh,tl,tu,p,n,g_r_mse,varm_a)

        do 20 j=1,4

             if(j .eq. 1) then
               xmin = (-sqrt(m(2))+m(1))/2.d0 - m(1)
               xmax = ( sqrt(m(2))+m(1))/2.d0 - m(1)
             else if (j .eq. 2) then
               xmin = ( m(1)**2+m(2))/2.d0 - m(2)
               xmax = m(2)
             else if (j .eq. 3) then
               xmin = -abs(m(3)/3.d0)
               xmax =  abs(m(3)/3.d0)
             else if (j .eq. 4) then
               xmin = -sqrt(max(1.d-08,g_r_mse))/10.d0
               xmax = -xmin
             endif
           
c
c      quick derivative
c

ctac
           ifast2 = -99
ctac
           ifast2 =  1
ctac
        if(j .eq. 4 .and. g_r_mse .lt. 0.d0) then
          deriv  =  0.d0
          goto 40
        endif
          if(ifast2 .ne. -99) then
          delta = sqrt(max(1.d-10,varm_a(j,j)))/100.d0
      
          call diff2(1,0.d0,
     2                 max(-delta,xmin),min(delta,xmax),
     3                 fcdfsy,-1.d-3,-acc,
     1                 deriv,error,ifail)
            if(ifail .eq. 0) goto 40
            write(*,*) 'var_syp:  full adaptive derivative used'
        endif
c
c      adaptive numerical derivative
c
            call diff(1,0.d0,xmin,xmax,fcdfsy,-eps,-acc,
     1                   deriv,error,ifail)
             if(ifail .eq. 1) then
              write(*,*) ' precision in jacob_syp',error
           else if(ifail .eq. 2) then
              write(*,*) ' input data incorrect in jacob_syp'
           else if(ifail .eq. 3) then
              write(*,*) ' input interval in jacob_syp too small'
            deriv = 0.d0
           endif
40        continue
          jac(j) = deriv
20      continue

        return
        end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      double precision function fcdfsy(x)
      
        implicit none
          save
      
      integer
     1    it,j,nct,nthresh,nth_p

        parameter (nth_p=100)
     
      double precision 
     1    x,
     2    g_r,g_r_mse,jac(4),m,n,ms(4),p(3),pq,tl,tu,
     3    varm_a(4,4),varyp,w
      
        common /zd05/m(4),pq,j
        common /zd07/tl(nth_p),tu(nth_p),n(nth_p),nthresh,nct
        common /zd10/g_r,g_r_mse,w
      
        do 10 it=1,4
          ms(it) = m(it)
10      continue
          ms(j) = ms(j) + x

        call m_g_2p(ms,nct,ms(4),g_r_mse,w,p)

        if(p(2) .le. 2.d-2 .or. p(2) .ge. 2.d+4
     1     .or. p(3) .eq. 0.d0) then
          fcdfsy = -1.d99
        else
      
        call var_m_r4(nthresh,tl,tu,p,n,g_r_mse,varm_a)

        call var_yp(varm_a,ms,w,pq,varyp,jac)

        fcdfsy = sqrt(varyp)
        endif
      
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       replacement for diff subroutine
c       uses a simple difference equation to get the jacobian
c       
c
         subroutine diff2(nd,x0,d1,d2,f,eps,acc,
     1                   deriv,error,ifail)
        
       implicit none
          save
       
       integer
     1     ifail,nd
     
         double precision 
     1     x0,d1,d2,f,eps,acc,deriv,deriv1,deriv2,error,up,down,
     2     fx,test
     
         external f
       
         fx    =  f(x0)

           if(fx .eq. -1.d99) then
             ifail = 1
             return
           endif
         up    =  f(d2)

         down  =  f(d1)
         deriv1 = (fx-down)/(x0-d1)
         deriv2 = (up-fx)/(d2-x0)
         deriv = (up-down)/(d2-d1)
         if(eps .gt. 0.d0) then
         test = 4.d0*eps**2
       else
         test = 4.d0*eps**2*(1.d0+deriv**2)
       endif
c
         if((deriv1-deriv2)**2 .le. test) then
           ifail = 0
         else
           ifail = 1
         endif
       return
       end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
        subroutine varmom
     1    (nthresh,tl_in,tu_in,parms,n_in,varm)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c 
c     program to compute variance of ema estimator
c
c     author.....tim cohn
c     date.......4 april 1999
c      modified..7 june 2000 (tac) --regional skew added
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        implicit none
          save

        integer 
     1    nthresh,
     2    i,it,j,nth_p
     
        parameter (nth_p=100)

        double precision 
     1    tl_in(nth_p),tu_in(nth_p),parms(3),n_in(nth_p),varm(3,3),
     2    a(3,3),ainv(3,3),bc_t(3,3),d(3,3),d_t(3,3),e_x(6),
     4    mu_x(3,3),nh,n_t,t1(3,3),tl,tu,p1,p2,p3,pa,pb,
     5    vb(3,3),vb_t(3,3),vc(3,3),vc_t(3,3)
     
        double precision
     1    fp_g3_cdf,fp_g3_mom_tra,fp_g3_mom_trb,fp_g3_mom_trc
     

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

            pa = fp_g3_cdf(tl,parms)
          pb = fp_g3_cdf(tu,parms)
          
            p1 = pa
            p2 = pb - pa
            p3 = 1.d0 - pb
          
c
          do 20 j=1,3
            e_x(j)    = fp_g3_mom_trc(parms,tl,tu,j)
            e_x(j+3)  = fp_g3_mom_trc(parms,tl,tu,j+3)
c
          mu_x(j,1) = fp_g3_mom_tra(parms,tl,j)
          mu_x(j,2) = e_x(j)
          mu_x(j,3) = fp_g3_mom_trb(parms,tu,j)
20        continue

        call varb(mu_x,nh,p1,p2,p3,vb)
          call dmsum(3,3,vb,3,3,3,vb_t,3,3,3,vb_t,3)
        
        call varc(e_x,nh,p2,vc)
          call dmsum(3,3,vc,3,3,3,vc_t,3,3,3,vc_t,3)

        call d_est(nh,parms,pa,pb,d)

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
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
        subroutine var_m_r4
     1    (nthresh,tl,tu,parms,n,g_r_mse,varm_a)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c 
c     program to compute variance of ema estimator
c
c     author.....tim cohn
c     date.......19 june 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     n.b.  regional skew not used
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        implicit none
          save

        integer 
     1    nthresh,
     2    i,j,nth_p
     
        parameter (nth_p=100)

        double precision 
     1    tl(nth_p),tu(nth_p),parms(3),n(nth_p),g_r_mse,varm_a(4,4),
     3    varm(3,3)
c
c    compute augmented covariance matrix
c    
      call varmom(nthresh,tl,tu,parms,n,varm)
      
      do 30 i=1,3
          varm_a(i,4) = 0.d0
          varm_a(4,i) = 0.d0
        do 30 j=1,3
          varm_a(i,j) = varm(i,j)
30      continue
            varm_a(4,4) = g_r_mse
        return
      end
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
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        subroutine d_est(nh,parms,pa,pb,d)

        implicit none
          save
      
        double precision 
     1    nh,parms(3),pa,pb,d(3,3),
     2    en1,en3,inf,jacg(3,3),jacl(3,3),pmin,p1,p3,t1,t3

        double precision 
     1    fp_g3_icdf
     
        data inf/1.d19/,pmin/1.d-6/

        p1 = pa
        p3 = 1.d0 - pb
        
        if(p1 .gt. pmin) then
            t1 = fp_g3_icdf(pa,parms)
          call expmomderiv(parms,-inf,t1,jacl)
          en1 = nh*p1
          call dmmult(en1,3,3,jacl,3,3,3,jacl,3)
        else
          call dset(9,0.d0,jacl,1)
        endif

        if(p3 .gt. pmin) then
            t3 = fp_g3_icdf(pb,parms)
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
      subroutine mn2m_var(mn,s_mn,mc,s_mc)
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
c          df.s_m.df'
c
       call dmrrrr(3,3,df,3,3,3,s_mn,3,3,3,temp,3)
       call dmxytf(3,3,temp,3,3,3,df,3,3,3,s_mc,3)

       return
       end
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      subroutine m2mn_var(mc,s_mc,mn,s_mn)
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
        subroutine jacob_r_p(m,gg,w,d)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     copyright, timothy a. cohn, 2000
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........17 june 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            m(3)       r*8  vector of non-central moments of lp-3 distribution
c            gg         r*8  generalized skewness
c            w          r*8  weight to attach to station skewness
c
c       output variables:
c       ---------------------------------------------------------------------------
c            d(3,4)     r*8  jacobian matrix of {tau,alpha,beta} wrt {m1,m2,m3,g}
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        implicit none
          save

      double precision
     1   m(4),gg,w,d(3,4),
     2   g,m1,m2,m3,mc(3),p(3),v,
     3   a,b,t,dgs_dm1,dgs_dm2,dgs_dm3,da_dg,dg_dgs,dg_dgg,sqa,sqv

           m1   =  m(1)
           m2   =  m(2)
           m3   =  m(3)
            
       dg_dgs  =  w
       dg_dgg  =  (1.d0-w)
       
       call mn2m(m,mc)
           g       =  w*mc(3) + (1.d0-w)*gg
         mc(3)   =  g

       call m2p(mc,p)
         t = p(1)
         a = p(2)
         b = p(3)

c alpha = 4/g**2
       da_dg   =  -8.d0/g**3
       
         dgs_dm1 = (6*m1**2 - 3*m2)/(-m1**2 + m2)**1.5 + 
     1              (3*m1*(2*m1**3 - 3*m1*m2 + m3))/
     2              (-m1**2 + m2)**2.5
         d(2,1)  = da_dg*dg_dgs*dgs_dm1

         dgs_dm2 = (-3*m1)/(-m1**2 + m2)**1.5 - 
     1               (3*(2*m1**3 - 3*m1*m2 + m3))/
     2               (2.*(-m1**2 + m2)**2.5)
         d(2,2)  = da_dg*dg_dgs*dgs_dm2

         dgs_dm3 = (-m1**2 + m2)**(-1.5)
         d(2,3)  = da_dg*dg_dgs*dgs_dm3

       dg_dgg  = (1.d0-w)
         d(2,4)  = da_dg*dg_dgg
       
c     write(*,*) da_dg,dg_dgg
c     write(*,*) p

c beta = sqrt((m2-m1**2)/a)*sign(m3)
          v    = m2-m1**2
        sqv  = sqrt(v)
        sqa  = sqrt(a)
         d(3,1) = sign(1.d0,g)*
     1             (-m1/sqrt(v*a) - 0.5*sqv/sqa**3 * d(2,1))

         d(3,2) = sign(1.d0,g)*
     1             (0.5d0/sqrt(v*a) - 0.5*sqv/sqa**3 * d(2,2))

         d(3,3) = sign(1.d0,g)*(-0.5*sqv/sqa**3 * d(2,3))

         d(3,4) = sign(1.d0,g)*(-0.5*sqv/sqa**3 * d(2,4))

c tau = m1 - a*b
         d(1,1) = 1.d0 - (a*d(3,1) + b*d(2,1))

         d(1,2) = - (a*d(3,2) + b*d(2,2))

         d(1,3) = - (a*d(3,3) + b*d(2,3))

         d(1,4) = - (a*d(3,4) + b*d(2,4))

         return
         end

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
        subroutine m_g_2p(m,n,g_r,g_r_mse,w,p)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     copyright, timothy a. cohn, 2000
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........30 june 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            m(3)       r*8  vector of non-central moments of lp-3 distribution
c            n          i*4  number of observations, ns + nh
c            g_r        r*8  regional skewness
c            g_r_mse    r*8  MSE of regional skewness
c
c       output variables:
c       ---------------------------------------------------------------------------
c            w          r*8  weight to attach to station skewness
c            p(3)       r*8  fitted parameters employing weighted skew
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        implicit none
          save

      double precision
     1    m(4),g_r,g_r_mse,w,mc(3),p(3)
     
        double precision mseg
     
        integer
     1    n

        call mn2m(m,mc)
        if(g_r_mse .ge. 0.d0) then
          w     =  g_r_mse/(mseg(n,mc(3)) + g_r_mse)
          mc(3) =  w*mc(3) + (1.d0-w)*g_r
          call m2p(mc,p)
        else
        w     =  1.d0
        call mn2p(m,p)
      endif

        return
        end
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c*_*-*~*_*-*~*             new program begins here            *_*-*~*_*-*~*_*-*~
      subroutine reg_skew(n,mn,s_mn,g_r,g_r_mse,mn_r,s_mn_r)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     program to estimate parameters and compute variance-covariance
c       of ema non-central moments estimators in the presence of 
c       regional skew
c
c     author.....tim cohn
c     date.......06 june 2000
c      modified..13 june 2000 (tac)
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   n.b.  this routine requires as input the v-cv matrix of the non-central
c         moments. these are obtained by a prior call to varmom
c
c           n            i*4 (in)   total length of historical period
c           mn(3)        r*8 (in)   first three non-central moments
c           s_m(3,3)     r*8 (in)   variance-covariance matrix of m
c           g_r          r*8 (in)   estimate of regional log-skew
c           g_r_mse      r*8 (in)   estimated mse of g_r
c                                     mse > 0 implies fit regional info
c                                     mse = 0 implies force skew to equal g_r
c                                     mse < 0 implies ignore regional skew
c           mn_r(3)      r*8 (out)  estimated non-central moments post hoc
c           s_mn_r(3,3)  r*8 (out)   variance-covariance matrix of m_reg
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
        implicit none

      double precision
     1    g_avg,g_r,g_r_mse,mc(3),mn(3),mn_r(3),
     2    s_mc(3,3),s_mn(3,3),s_mn_r(3,3),w,
     3    mse_station
     
        double precision mseg
      
      integer i,j,n

c
c    0.a  test for negative mse => return without making any changes
c
        if(g_r_mse .lt. 0.d0) then
           do 10 i=1,3
               mn_r(i) = mn(i)
             do 10 j=1,3
               s_mn_r(i,j) = s_mn(i,j)
10           continue
         return
      endif
c
c    1.a  compute central moments from non-central moments
c
      call mn2m_var(mn,s_mn,mc,s_mc)
c
c    2.a compute weighting for mse of estimate for at-site gamma
c
        mse_station = mseg(n,mc(3))
c       mse_station = s_mc(3,3)
       
       w      =  g_r_mse/(mse_station + g_r_mse)

c
c    2.b compute at-site gamma
c
       g_avg  =  w * mc(3) + (1.d0-w) * g_r
       mc(3)  =  g_avg

c
c    3.a compute asymptotic covariance of central moments
c
       s_mc(1,1)  = s_mc(1,1) 
       s_mc(1,2)  = s_mc(1,2) 
       s_mc(1,3)  = w * s_mc(1,3)
       s_mc(2,1)  = s_mc(1,2) 
       s_mc(2,2)  = s_mc(2,2) 
       s_mc(2,3)  = w * s_mc(2,3)
       s_mc(3,1)  = s_mc(1,3)
       s_mc(3,2)  = s_mc(2,3)
       s_mc(3,3)  = w**2 * s_mc(3,3) + (1.d0-w)**2 * g_r_mse

c
c    4.a compute jacobian of non central moments with respect to central
c            moments
c
      call m2mn_var(mc,s_mc,mn_r,s_mn_r)

       return
       end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
       double precision function mseg(n,g)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     computes the station skew for the bulletin 17b analysis (see p. 13)
c
c     n.b.  b17b recommends using h -- the entire period length --
c           for the record length with historical information.
c
c     copyright, timothy a. cohn, 2000
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........11 june 2000
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            n         i*4  record length
c            g         r*8  station skew
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        implicit none
          save
      
      double precision g,a,b
      integer n
      
ctac  debug code 6/23/00
c           mseg = 12.30269d0/n
c       return
ctac
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
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c      the following program exactly reproduces table 1 in bulletin 17b
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    program testit
c      double precision skew,answer(10),mseg
c    
c    do 10 skew = 0.0,3.0,.1
c       do 20 i=1,10
c         answer(i)  =  mseg(10*i,skew)
c20         continue
c         write(*,'(f4.1,2x,10f6.3)') skew,answer
c10      continue
c        read(*,*)
c    stop
c    end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      subroutine tjrjtz()
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     copyright, timothy a. cohn, 2000
c
c     *** do not modify without author's consent ***
c
c           author.......tim cohn
c           date.........17 june 2000
c
c   this is nothing more than a test subroutine to verify that m_g_2p andd
c   jacob_r both function correctly
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
  
        implicit none
          save

      double precision
     1   m(4),g,g_r_mse,w,d(3,4),
     2   delta,error,mc(3),mc2(3),
     3   parms(3),pdiff(3),pl(3),pu(3)
     
        integer
     1    i,j,n
     
        data n/100/

1       continue
        write(*,*) 'enter parms (t,a,b,g,g_r_mse)'
        read(*,*) parms,g,g_r_mse
      
        call p2mn(parms,m)
      m(4)  =  g

        write(*,*) 'enter delta'
        read(*,*) delta

        call mn2m(m,mc)
        call m_g_2p(m,n,g,g_r_mse,w,pu)
      call p2m(pu,mc2)
      write(*,'(a,f15.5)') ' station skew    ',mc(3)
      write(*,'(a,f15.5)') ' generalized skew',g
      write(*,'(a,f15.5)') ' weighted skew   ',mc2(3)
      write(*,'(a,f15.5)') ' weight          ',w
      write(*,*)

      call jacob_r_p(m,g,w,d)

          error = 0.d0
        do 10 i=1,4
          m(i) = m(i) + 1.d0*delta
          g  =  m(4)
          call m_g_2p(m,n,g,g_r_mse,w,pu)
          m(i) = m(i) - 2.d0*delta
          g  =  m(4)
          call m_g_2p(m,n,g,g_r_mse,w,pl)
          m(i) = m(i) + 1.d0*delta
          do 20 j=1,3
           pdiff(j) = (pu(j)-pl(j))/(2.d0*delta)
         error    = error + (pdiff(j)-d(j,i))**2
20        continue
           write(*,'(3f15.6)') pdiff
           write(*,'(3f15.6)') (d(j,i),j=1,3)
         write(*,'(1x)')
10      continue
           write(*,'(a,f15.5)') ' sum of squared errors: ',error
        return
        end
c
c         program test_m_g_2p_jacob_r
c         do 10 j=1,10
c     call tjrjtz()
c10       continue 
c     end
          
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    tests for the main subroutines
c
c    also provides linear estimates of variance-covariance for
c       many cases
c    
      subroutine tests001()
        implicit none
          save

        integer 
     1     i,j,n,n_max
     
        parameter (n_max = 2)
     
        double precision
     1    parms(3),
     2    delta,dest(4),g_r_mse,mc(3),jac(4),
     2    mcr(3),mn_r(3),moms_a(4),
     2    p_a(3),pcensl,pcensu,pq,pl(3),pu(3),s_mcr(3,3),
     3    tl(n_max),tu(n_max),varm(3,3),varm_a(4,4),varmr3(3,3),
     3    varyp,varsyp,w,xn(n_max),
     4    ypl,ypu
     
        double precision fp_g3_icdf
     
        write(*,*) ' enter parameters (t,a,b)'
        read(*,*) parms
      
c   print non-central moments without regional info
           call p2mn(parms,moms_a)
         moms_a(4) = sign(2.d0,parms(3))/sqrt(parms(2))
         write(*,'(/,a)') ' moms'
         write(*,'(4f18.6)') moms_a
            
c   print central moments without regional info
           call mn2m(moms_a,mc)
         write(*,'(/,a)') ' central moments'
         write(*,'(3f18.6)') mc

      write(*,*) 'enter systematic record length, ns'
        read(*,*) xn(1)
            tl(1) = -9.d99
            tu(1) =  9.d99
      write(*,*) 'enter historical record length, nh'
        read(*,*) xn(2)
 
      if(xn(2) .gt. 0) then
        write(*,*) 'define interval for which exact values for '
        write(*,*) '  historical floods will be observed '
        write(*,*) 'enter lower censoring probability p[x<tl]'
        read(*,*) pcensl
          pcensl = max(1.d-6,min(0.999999d0,pcensl))
        write(*,*) 'enter upper censoring probability p[x<tu]'
        read(*,*) pcensu
          pcensu = max(1.d-6,min(0.999999d0,pcensu))
            tl(2) =  fp_g3_icdf(pcensl,parms)
            tu(2) =  fp_g3_icdf(pcensu,parms)
      endif
                      
c   compute and print v-cv of non-central moments without regional info
      call varmom(2,tl,tu,parms,xn,varm)
         write(*,'(/,a)') ' varm'
         write(*,'(3f18.6)') varm
      
        write(*,*) ' enter reg_skew mse (g_r_mse)'
      read(*,*) g_r_mse

c   compute weighted skew, weight
         n  =  xn(1)+xn(2)
           call m_g_2p(moms_a,n,moms_a(4),g_r_mse,w,p_a)
         write(*,'(/,a)') ' regional skew, weight'
         write(*,'(3f18.6)') moms_a(4),w
        
c   compute and print non-central moments with regional info
      call p2mn(p_a,moms_a)
         write(*,'(/,a)') ' non-central moments (weighted)'
         write(*,'(4f18.6)') moms_a

c   compute and print central moments with regional info
      call p2m(p_a,mc)
         write(*,'(/,a)') ' central moments (weighted)'
         write(*,'(3f18.6)') mc
         
c   compute and print variance-covariance matrix of non-central moments    
      call reg_skew(n,moms_a,varm,moms_a(4),g_r_mse,mn_r,varmr3)
         write(*,'(/,a)') ' v-cv of non-central moments (weighted)'
         write(*,'(3f18.6)') varmr3
         
c   compute and print variance-covariance matrix of central moments	
        call mn2m_var(moms_a,varmr3,mcr,s_mcr)
         write(*,'(/,a)') ' v-cv of central moments (weighted)'
         write(*,'(3f18.6)') s_mcr
      
c   compute and print augmented variance-covariance matrix of non-central moments	
      do 30 i=1,3
          varm_a(i,4) = 0.d0
          varm_a(4,i) = 0.d0
        do 30 j=1,3
          varm_a(i,j) = varm(i,j)
30      continue
            varm_a(4,4) = g_r_mse
         write(*,'(/,a)') ' moms_a'
         write(*,'(4f18.6)') moms_a
         write(*,'(/,a)') ' varm_a'
         write(*,'(4f18.6)') varm_a
      
         pq  =  0.99d0
      call var_yp(varm_a,moms_a,w,pq,varyp,jac)
         write(*,'(/,a)') ' variance of yp; s.d. of yp'
         write(*,'(3f18.6)') varyp,sqrt(varyp)
         write(*,'(/,a)') ' jacobian of yp'
         write(*,'(4f18.6)') jac
         
      delta  =  0.0001
         
         do 40 j=1,4
           moms_a(j) = moms_a(j) + delta
c         call m_g_2p(moms_a,moms_a(4),w,pu)
             call m_g_2p(moms_a,n,moms_a(4),g_r_mse,w,pu)
           ypu  =   fp_g3_icdf(pq,pu)
           moms_a(j) = moms_a(j) - 2.d0 * delta
c         call m_g_2p(moms_a,moms_a(4),w,pl)
             call m_g_2p(moms_a,n,moms_a(4),g_r_mse,w,pl)
           ypl  =   fp_g3_icdf(pq,pl)
           dest(j) = (ypu-ypl)/(2.d0*delta)
           moms_a(j) = moms_a(j) + delta
40         continue
         write(*,'(/,a)') ' approximate jacobian of yp'
         write(*,'(4f18.6)') dest

        call var_syp
     1    (2,tl,tu,parms,xn,pq,g_r_mse,w,
     2     varsyp,jac)
            write(*,'(/,a)') ' variance of syp; s.d. of syp'
         write(*,'(3f18.6)') varsyp,sqrt(varsyp)
         write(*,'(/,a)') ' jacobian of syp'
         write(*,'(4f18.6)') jac
         do 50 j=1,4
           moms_a(j) = moms_a(j) + delta
c         call m_g_2p(moms_a,moms_a(4),w,pu)
             call m_g_2p(moms_a,n,moms_a(4),g_r_mse,w,pu)
             call var_m_r4(2,tl,tu,pu,xn,g_r_mse,varm_a)
             call var_yp(varm_a,moms_a,w,pq,ypu,jac)
             ypu = sqrt(ypu)
           moms_a(j) = moms_a(j) - 2.d0 * delta
c         call m_g_2p(moms_a,moms_a(4),w,pl)
             call m_g_2p(moms_a,n,moms_a(4),g_r_mse,w,pl)
             call var_m_r4(2,tl,tu,pl,xn,g_r_mse,varm_a)
           call var_yp(varm_a,moms_a,w,pq,ypl,jac)
             ypl = sqrt(ypl)
           dest(j) = (ypu-ypl)/(2.d0*delta)
           moms_a(j) = moms_a(j) + delta
50         continue
         write(*,'(/,a)') ' approximate jacobian of syp'
         write(*,'(4f18.6)') dest

           return
         end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    tests for the main subroutines 
c    
c            program testz001; call tests001(); end
c            program testz001; call tests001(); end

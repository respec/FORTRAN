      subroutine b17cip(n,parms,pq,peps,cil,ciu)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    computes confidence intervals used by B17B
c
c    tim cohn........24 Nov 2003
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       input variables:
c       ---------------------------------------------------------------------------
c            n          i*4  number of observations (censored, uncensored, or other)
c            parms(3)   r*8  fitted parameters of lp3 distribution
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
     1 parms(3),pq,peps,cil,ciu,
     2 moms(3),klpc,kupc

      call p2m(parms,moms)
      
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
c    tim cohn........24 Nov 2003
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      double precision parms(2),prob,skew,fp_z_icdf,fp_g2_icdf  
        
      if(skew .eq. 0.d0) then
          kf = fp_z_icdf(prob)
      else
          parms(1)  = 4.d0/skew**2
        if(skew .lt. 0.d0) then
          parms(2) = -1.d0/sqrt(parms(1))
        else
          parms(2) =  1.d0/sqrt(parms(1))
        endif  
          kf = fp_g2_icdf(prob,parms) - parms(1)*parms(2)
      endif

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
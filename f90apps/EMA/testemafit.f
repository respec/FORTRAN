      program testemafit
c
c
c
      implicit none
      
      double precision 
     1 ql(100),qu(100),tl(100),tu(100),reg_skew,reg_mse,pq,
     2 cmoms(3),yp,ci_low,ci_high,qli,qui,qm,qs,sk,pr(16),t
      
      integer i,n,iyear,nct

      LOGICAL      LFLAG

	DATA PR/.0001,.005,.01,.05,.10,.20,.3333,.5,.5708,
     1          .8,.9,.96,.98,.99,.995,.998/

C     avoid some lahey math errors
      LFLAG = .TRUE.
      CALL INVALOP (LFLAG)
      CALL UNDFL (LFLAG)
      
cprh      reg_skew = 0.1d0
      reg_skew = -.5
cprh      reg_mse = 9.d3
      reg_mse = 0.3025
      pq = 0.99d0
      
        qm = 0.d0
        qs = 0.d0
        sk = 0.d0
cprh        n  = 20
	OPEN(UNIT=11,FILE='test.txt',STATUS='OLD')
	
      N = 0
	DO 10 I=1,100
	  READ(11,*,END=99) IYEAR,NCT,QLI,QUI
cprh      do 10 i=1,n
        n=n+1
        ql(i) = log(qli)
        qu(i) = log(qui)
        tl(i) = -9.0d99
        tu(i) =  9.0d99
        qm = qm+ql(i)
        qs = qs+ql(i)**2
        sk = sk+ql(i)**3
10    continue
99    continue
      CLOSE(11)
        write(*,*) 'mean, var: ',qm/n,(qs-qm**2/n)/(n-1)
        write(*,*) 'Calling EMAFIT'
        write(*,*) '  n ',n
        write(*,*) '  ql ',ql
        write(*,*) '  qu ',qu
        write(*,*) '  tl ',tl
        write(*,*) '  tu ',tu
        write(*,*) '  regskew ',reg_skew
        write(*,*) '  regmse ',reg_mse
        write(*,*) '  pr(1) ',pr(1)
      do 20 i=1,16
        call emafit(n,ql,qu,tl,tu,reg_skew,reg_mse,pr(i),
     1                   cmoms,yp,ci_low,ci_high)
        if (i.eq.1) then 
          write(*,*) 'Cent. Moments: ',cmoms
          write(*,*)
          write(*,2000)
        end if
        t  = 1.D0/(1.D0-pr(i))
        write(*,2010) pr(i),t,exp(yp),exp(ci_low),exp(ci_high)
 20   continue
2000  format('  P[<Q]',t16,'T',t29,'Q',t39,'CI Low',t50,'CI High')
2010  format(f8.4,t12,f10.4,t24,f10.3,t36,f10.3,t48,f10.3)

       stop
       end
       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c****| PeakFQ2EMA
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c    
c    this routine reads the standard output file from the USGS PEAKFQ
c    program, extracts the data and control information from that file,
c    solicits a small amount of additional user information, and then
c    calculates ema-based estimates of the frequency curves
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
c    timothy a. cohn        21 may 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      program PeakFQ2EMA
      
      implicit none
      
      integer nmax,ncmax,nqmax
      parameter (nmax=20000,ncmax=20,nqmax=15)
      
      integer
     1  i,j,ncases,nV(ncmax),
     2  wyV(nmax,ncmax)
     
      double precision
     1  reg_skewV(ncmax),reg_mseV(ncmax),
     2  qlV(nmax,ncmax),quV(nmax,ncmax),
     3  tlV(nmax,ncmax),tuV(nmax,ncmax),
     4  cmoms(3),pq(nqmax),yp(nqmax),ci_low(nqmax),ci_high(nqmax)
     
      character*80 input(1000,ncmax),infile,outfile
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   beginning of executable code
c
      write(*,*) 'enter the input filename'
      read(*,*) infile
      open(unit=11,file=infile,status='old')
      write(*,*) 'enter the output filename'
      read(*,*) outfile
      open(unit=12,file=outfile,status='new')

      call getdata01(ncases,nV,qlV,quV,tlV,tuv,reg_skewV,reg_mseV,
     1                     wyV,input)
      
c       WRITE(*,*) 'NCASES',NCASES
c       WRITE(*,*) 'NV',NV(1),NV(2)
c      do 10 i=1,ncases
      
!              N.B. Following CALL requires (FORTRAN) column major order

c       write(*,*) 'skew',reg_skewV(i),reg_mseV(i)
c       do 80 j=1,nV(i)
c         write(*,*) j,qlV(j,i),quV(j,i),tlV(j,i),tuV(j,i)
c         qlV(j,i) = log10(max(1.d-10,qlV(j,i)))
c         quV(j,i) = log10(max(1.d-10,quV(j,i)))
c         tlV(j,i) = log10(max(1.d-10,tlV(j,i)))
c         tuV(j,i) = log10(max(1.d-10,tuV(j,i)))
c80     continue
c        read(*,*)
         

c      call emafit(nV(i),qlV(1,i),quV(1,i),tlV(1,i),tuV(1,i),
c     1              reg_skewV(i),reg_mseV(i),   
c     2              cmoms,pq,yp,ci_low,ci_high)
c        
c      call printresult(reg_skewV(i),reg_mseV(i),cmoms,pq,yp,
c     1  ci_low,ci_high)
c        
c10    continue

      stop
      end
      
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine getdata01(ncases,nV,qlV,quV,tlV,tuV,
     1                     reg_skewV,reg_mseV,
     2                     wyV,input)
      
      implicit none
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   declaration of parameters
c
      integer nmax,ncmax,nthresh,nqmax
      double precision infinity,Missing
      parameter (nmax=20000,ncmax=20,nthresh=10,Infinity=1.d20,
     1           Missing=-0.99d-10,nqmax=15)
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   declaration of external functions
c
      logical ifany
      
      integer ivmin,ivmax
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   declaration of arrays and variables
c
      integer
     1  ncases,nV(ncmax),wyV(nmax,ncmax),nsh
     
      double precision
     1  qlV(nmax,ncmax),quV(nmax,ncmax),tlV(nmax,ncmax),tuV(nmax,ncmax),
     2                     reg_skewV(ncmax),reg_mseV(ncmax)
      
      integer
     1  i,j,ict,k,
     2  iruns,
     2  n_peak,n_not_used,n_sys,n_historic,n_hist_period,
     3  ihalf,wy(nmax),
     2  nt,wybeg(nthresh),wyend(nthresh),wymin,wymax
     
      double precision
     1  gen_skew,gen_skew_sd,gage_base_q,user_supp_hot,user_supp_lot,
     2  a_plot,
     3  q(nmax),
     4  tl(nthresh),tu(nthresh),
     4  cmoms(3),pq(nqmax),yp(nqmax),ci_low(nqmax),ci_high(nqmax)

     
      
      character*80 input(500,ncmax),line,station
      
      character*11 skew_option
      
      character*10 codes(nmax)
      
      logical
     1  lprint(nmax)
     
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c   beginning of executable code
c
      do 10 iruns =1,ncmax
        do 20 i = 3, 200
            read(11,'(a80)',end=99) input(i,iruns)
            write(12,'(a80)') input(i,iruns)
        if(ifany('Station -',input(i,iruns))) then
          station = input(i,iruns)
        else if(input(i,iruns)(17:31) .eq. 'Number of peaks') then
          read(input(i,iruns)(58:63),'(i7)') n_peak
        else if(input(i,iruns)(17:31) .eq. 'Peaks not used ') then
          read(input(i,iruns)(58:63),'(i7)') n_not_used
        else if(input(i,iruns)(17:31) .eq. 'Systematic peak') then
          read(input(i,iruns)(58:63),'(i7)') n_sys
        else if(input(i,iruns)(17:31) .eq. 'Historic peaks ') then
          read(input(i,iruns)(58:63),'(i7)') n_historic
        else if(input(i,iruns)(17:31) .eq. 'Years of histor') then
          read(input(i,iruns)(58:63),'(i7)') n_hist_period
        else if(input(i,iruns)(17:31) .eq. 'Generalized ske') then
          read(input(i,iruns)(58:63),'(f7.3)') gen_skew
        else if(input(i,iruns)(17:31) .eq. 'Standard error ') then
          read(input(i,iruns)(58:63),'(f7.3)') gen_skew_sd
        else if(input(i,iruns)(17:31) .eq. 'Skew option    ') then
          read(input(i,iruns)(57:67),'(a11)') skew_option
        else if(input(i,iruns)(17:31) .eq. 'Gage base disch') then
          read(input(i,iruns)(58:63),'(f7.1)') gage_base_q
        else if(input(i,iruns)(17:31) .eq. 'User supplied h') then
          if(input(i,iruns)(58:59) .eq. '--') then
            user_supp_hot = 0.d0
          else
            read(input(i,iruns)(58:63),'(f7.0)') user_supp_hot
          endif
        else if(input(i,iruns)(17:31) .eq. 'User supplied l') then
          if(input(i,iruns)(58:59) .eq. '--') then
            user_supp_lot = 0.d0
          else
            read(input(i,iruns)(58:63),'(f7.0)') user_supp_lot
          endif
        else if(input(i,iruns)(17:31) .eq. 'Plotting positi') then
          read(input(i,iruns)(58:63),'(f7.2)') a_plot
        else if(input(i,iruns)(24:42) .eq. 'I N P U T   D A T A') then
          goto 30
        endif
20      continue

30    continue
            reg_skewV(iruns) = gen_skew          ! regional skew info
            reg_mseV(iruns)  = gen_skew_sd**2
            
        do 40 i=i+1,i+4
            read(11,'(a80)',end=99) input(i,iruns)
            write(12,'(a80)') input(i,iruns)
40      continue
              ict   = i
              ihalf = (n_peak+1)/2
        do 50 i=ict+1,ict+ihalf
              j  = i-ict
            read(11,'(a80)',end=99) input(i,iruns)
            write(12,'(a80)') input(i,iruns)
            read(input(i,iruns),'(2(i12,f15.1,a9,1x))') 
     1          wy(j),q(j),codes(j),
     1          wy(j+ihalf),q(j+ihalf),codes(j+ihalf)
50      continue

      write(12,'(///)') 
      write(12,'(t35,a12)') 'EMA Analysis'
      write(12,*) 
      write(12,'(a80)') station
      write(12,*)
      write(12,'(t35,a12)') 'INPUT DATA'
      write(12,*) 
      do 51 i=1,n_peak
        write(12,'(t10,i5,i7,f12.1,a10)') i,wy(i),q(i),codes(i)
        write( *,'(t10,i5,i7,f12.1,a10)') i,wy(i),q(i),codes(i)
51    continue
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c
c   here's where the real work gets done
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c     compute the default thresholds
c     these are given by 
c           (tl,tu) = (0,Infinity) for systematic data
c           (tl,tu) = (Min{historic floods},Infinity) for historic period
c
c     N.B. Negative discharges indicate peaks not used in analysis (convention?)
c
         
          nt = 1
          tl(1)     =  0.d0           ! systematic data
          tu(1)     =  Infinity
            wybeg(1)  =  999999       ! non-informative prior
            wybeg(2)  =  999999
            wyend(1)  = -999999

          
          tl(2)     =  Infinity       ! historical data
          tu(2)     =  Infinity
        do 90 k=1,n_peak
              wy(k) =  abs(wy(k))     !  hist floods id'd by negative wy
          if(q(k) .ge. 0.0) then
            if(ifany('H',codes(k))) then
              nt    = 2
              wybeg(2) = min(wybeg(2),wy(k))
              tl(2)    = min(tl(2),q(k))
            else
              wybeg(1) = min(wybeg(1),wy(k))
              wyend(1) = max(wyend(1),wy(k))
            endif
          endif
90      continue
            wyend(2) = wybeg(1)-1

      write(*,'(///)') 
      write(*,*) ' Enter censoring thresholds corresponding to:'
      write(*,*) 
      write(*,'(a80)') station
      write(*,*) 
      write(*,*) ' Default values computed to be:'
      write(*,*) 
      write(*,1002)
1002    format(' T',t9,'Beg. Year',t21,'End Year',
     1         t38,'T_lower',t53,'T_upper')
      write(*,1001) 1,wybeg(1),wyend(1),tl(1),tu(1)
1001    format(i2,1x,2i14,1p,2d15.5,0p)
      if(nt .eq. 2)  write(12,1001) 2,wybeg(2),wyend(2),tl(2),tu(2)
      write(*,*)
      write(*,*) ' Enter blank line to accept defaults'
      write(*,*) ' To change thresholds, enter desired thresholds'
      write(*,*) ' beginning year, ending year, threshold pair'
      write(*,*) '          (wybeg, wyend, tl, tu)'
      write(*,*)  
      do 92 j=nt+1,10
      read(*,'(a80)') line
      if(line .eq. '') then
        nt = j-1
        goto 95
      else
        read(line,*) wybeg(j),wyend(j),tl(j),tu(j)
      endif
92    continue
95    continue
      write(12,*) ' FINAL Thresholds'
      write(12,*)
      write(12,1002)
      do 98 j=1,nt
        write(12,1001) j,wybeg(j),wyend(j),tl(j),tu(j)
98    continue
            wymin  =  ivmin(nt,wybeg) 
            wymax  =  ivmax(nt,wyend)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  Set all annual thresholds to Missing (implies no knowledge)
c
      do 200 k=wymin,wymax
          i = k-wymin+1
          wyV(i,iruns) =  k
          tlV(i,iruns) =  Missing
          tuV(i,iruns) =  Missing
          qlV(i,iruns) =  Missing    ! Flag initially as missing values
          quV(i,iruns) =  Missing
200   continue

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  Fill in known threshold values
c
      do 210 j=1,nt
        do 210 k=wybeg(j),wyend(j)
          i = k-wymin+1
          tlV(i,iruns) = tl(j)
          tuV(i,iruns) = tu(j)
210   continue

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  Fill in measured peaks
c
      do 220 j=1,n_peak
        if(q(j) .le. Missing) goto 220
          i = wy(j)-wymin+1
          if(tlV(i,iruns) .eq. Missing) then  ! this should never occur
            write(*,*) ' *** Problem ***'
            write(*,*) ' Water Year ',wy(j),' has peak without tl, tu'
          endif
          qlV(i,iruns) = q(j)
          quV(i,iruns) = q(j)
220   continue

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  For years without peaks, assume peak is less than threshold tl
c
      do 230 k=wymin,wymax
          i = k-wymin+1
          if(qlV(i,iruns) .eq. Missing) then  ! no peak for this year
            if(tlV(i,iruns) .gt. 0.d0) then
              qlV(i,iruns) = 0.d0
              quV(i,iruns) = tlV(i,iruns)
            endif
          endif
230   continue

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  collapse the data set, eliminating any periods of missing record
c
          i  =  0
      do 240 j=1,wymax-wymin+1
        if(qlV(j,iruns) .ne. Missing) then
          i = i+1
          tlV(i,iruns) = tlV(j,iruns)
          tuV(i,iruns) = tuV(j,iruns)
          qlV(i,iruns) = qlV(j,iruns)
          quV(i,iruns) = quV(j,iruns) 
          wyV(i,iruns) = wyV(j,iruns)
        endif
240   continue
          nsh = i
          nV(iruns) = nsh

c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  accept edits to existing flood peaks
c
        write(12,'(//,t35,''EMA DATA'',/)')
        write(12,1004)
1004      format(t13,'Year',t25,'Q_low',
     1         t41,'Q_up',t53,'T_low',t69,'T_up')
      do 300 i=1,nsh
        if(i .eq. 1 .or. i .eq. nsh) then
          lprint(i) = .TRUE.
        else if(qlV(i,iruns) .ne. qlV(i-1,iruns)) then
          lprint(i) = .TRUE.
        else if(quV(i,iruns) .ne. quv(i-1,iruns)) then
          lprint(i) = .TRUE.
        else if(tlV(i,iruns) .ne. tlV(i-1,iruns)) then
          lprint(i) = .TRUE.
        else if(tuV(i,iruns) .ne. tuV(i-1,iruns)) then
          lprint(i) = .TRUE.
        else if(wyV(i,iruns) .ne. wyV(i-1,iruns)+1) then
          lprint(i) = .TRUE.
        else 
          lprint(i) = .FALSE.
        endif
300   continue
c
      do 310 i=1,nsh
        if(lprint(i)) then
          write(12,1003) wyV(i,iruns),qlV(i,iruns),quV(i,iruns),
     1                  tlV(i,iruns),tuV(i,iruns)
1003        format(1x,i15,1p,2f15.1,2d15.5)
        else if(lprint(i+1)) then
          write(12,1003) wyV(i,iruns),qlV(i,iruns),quV(i,iruns),
     1                  tlV(i,iruns),tuV(i,iruns)
        else if(lprint(i-1)) then
          write(12,'(t35,''  . . .'')')
        endif
310   continue

c
c   process the data
       do 400 j=1,nV(iruns)
c         write(*,*) j,qlV(j,i),quV(j,i),tlV(j,i),tuV(j,i)
         qlV(j,iruns) = log10(max(1.d-10,qlV(j,iruns)))
         quV(j,iruns) = log10(max(1.d-10,quV(j,iruns)))
         tlV(j,iruns) = log10(max(1.d-10,tlV(j,iruns)))
         tuV(j,iruns) = log10(max(1.d-10,tuV(j,iruns)))
400    continue
c        read(*,*)
         

      call emafit(nV(iruns),qlV(1,iruns),quV(1,iruns),
     1              tlV(1,iruns),tuV(1,iruns),
     1              reg_skewV(iruns),reg_mseV(iruns),   
     2              cmoms,pq,yp,ci_low,ci_high)
        
c      call printresult(reg_skewV(i),reg_mseV(i),cmoms,pq,yp,
c     1  ci_low,ci_high)

      write(12,2001) reg_skewV(iruns),reg_mseV(iruns),
     1               sqrt(reg_mseV(iruns))
2001  format(//,t35,'EMA RESULTS',//,t30,'Generalized Skew',t50,f6.3,
     1  /,t30,'Gen. Skew MSE',t50,f6.3,/,t30,'Gen. Skew SD',t50,f6.3)
     
      write(12,2002) cmoms(1),sqrt(cmoms(2)),cmoms(3)
2002  format(//,t5,' Fitted EMA Moments:',3f10.6)

      write(12,2003)
2003  format(//,t5,' Fitted EMA Frequency Estimates',//,
     2  'Non-Exceedance Prob.',t20,'Quantile',t40,'Ci-Low',
     1  t60,'CI-High',/)
     
      do 410 i=1,nqmax
        write(12,2004) pq(i),10**yp(i),10**ci_low(i),10**ci_high(i)
2004    format(t5,f6.4,t20,3f20.1)
410   continue

c
10    continue
99    continue
       ncases  = iruns-1
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  function to check if a string y contains string x
c    -- used to check if particular remark code is present
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        21 may 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      logical function ifany(x,y)
      character*(*) x,y

        ifany = .FALSE.
      do 10 i=1,len(y)-len(x)+1
        if(x .eq. y(i:i+len(x)-1)) ifany = .TRUE.
10    continue
      return
      end
      

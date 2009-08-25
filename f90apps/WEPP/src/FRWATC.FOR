      subroutine frwatc(wbtofs)
c
c     +++PURPOSE+++
c
c     The purpose of this program is for exchanging soil water content from
c     the frost routine with that from water blance routine and for other routines.
c
c     In other routines, we do not have the finer soil layers. Therefore soil water
c      content were lump together when frost simulation ends.
c
c     At the beginning the frost forms, soil water content were treated uniform in a soil layer
c     and same soil water content were assgined to each fine layer in a soil layer.(in forstN.for)
c     When frost exists, the increase or decrease ammount of soil water in a layer is divieded 
c     into the unfrozen fine layers uniformly.
c 
c     
c     Author(s):  Shuhui Dun, WSU
c     Date: 02/28/2008
c     Verified by: Joan Wu, WSU 
c
c     +++ARGUMENT DECLARATIONS+++
      integer  wbtofs
c
c     +++ARGUMENT DEFINITIONS+++
c    
c     wbtofs - flag to indicate if the soil water content is from water balance 
c              to frost routine or other way around
c              0 for frost to other routines
c              1 for from water balance to frost routine
c
c     +++PARAMETERS+++
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
c
c
c     +++COMMON BLOCKS+++
c
      include  'cstruc.inc'
c       read:  iplane
      include 'cupdate.inc'
c       read:  sdate
      include  'cwint.inc'
c       read:  snodpt(iplane),tfrdp(mxplan),tthawd(mxplan),frdp(mxplan),
c              thdp(mxplan),densg(mxplan)
c
      include 'cflgfs.inc'
c     fine layer for frost simulation
c
      include 'cpfrst.inc'
c
      include 'csaxp.inc'
c     Saxton and Ralwls model coefficients
c
      include 'cwater.inc'
c     read: dg(i,iplane)
c
c
c     +++LOCAL VARIABLES+++
c
      integer  frstn,i,j,fglckw,jend
      real     slufdp,sumfzd,sumsw,sumice,sumst,varchg,
     1         varufd,varsw,varwat,vardp,varufp,
     1         lackw,varswc,wlck,vartmp,nwinc,spcav
c
c     +++LOCAL DEFINITIONS+++
c
c     frstn - frost existing fine layer number in a soil layer
c
c     sldp - fine soil layer unfrozen depth
c     sumfzd - variable for sum of frozen depth
c     sumsw - variable for sum of liquid water
c     sumst - variable for available soil water
c     sumice - variable for sum of ice water ammount
c     fglckw - a flag indictates water shall be get off from next layer
c              0 for no water needs go to next layer
c               1 for there is water needs go to next layer.
c     lackw  - the amount of water could not gave from current layer.
c     nwinc  - the ammount of unfrozen water increased for a large soil layer
c
c     +++DATA INITIALIZATIONS+++
c
c     +++END SPECIFICATIONS+++
c
      if (wbtofs .eq. 0) then
c     from frost to water balance routines
c
      wlck = 0.0
      do 10 i = 1, nsl(iplane)
c  
         sumfzd = 0.
         sumsw = 0.
         sumst = 0.
         sumice = 0.
         jend = nfine(i) 
c         
         do 20 j = 1, jend
c
             sumfzd = sumfzd + slfsd(j,i,iplane)
             sumice = sumice + slsic(j,i,iplane)
c
             slufdp = dg(i,iplane)/jend -slfsd(j,i,iplane)
c 
             if(slufdp.gt.0.001) then           
c
             vartmp = slsw(j,i,iplane) - wlck/slufdp
c
             if (vartmp .le. thetdr(i,iplane)) then
                  wlck = (thetdr(i,iplane) - vartmp)*slufdp
                  slsw(j,i,iplane) = thetdr(i,iplane)
             else
                  slsw(j,i,iplane) = vartmp
                  wlck = 0.0
             endif
             sumst = sumst +(slsw(j,i,iplane) -thetdr(i,iplane))
     1                  *slufdp 
             sumsw = sumsw + slsw(j,i,iplane)*slufdp
             endif
20       continue
c
               frozen(i,iplane) = sumfzd
               frzw(i,iplane) = sumice - 
     1              thetdr(i,iplane)* frozen(i,iplane)
               st(i,iplane) = sumst + nwfrzz(i,iplane)
               soilw(i,iplane) = sumsw + nwfrzz(i,iplane)
               yst(i,iplane) = st(i,iplane)   
c 
10    continue
c      
      elseif (wbtofs .eq. 1) then
c     from water balance to frost routines
      fglckw = 0
      lackw = 0.0
c
      do 30 i = 1, nsl(iplane)
c
      jend = nfine(i)
c
      if (st(i,iplane) .eq. 0.0) then
c
         nwfrzz(i,iplane) = 0.0
         do 25 j = 1, jend
             slsw(j,i,iplane) = thetdr(i,iplane)                    
25       continue
c
      else
c
         varwat = st(i,iplane) - yst(i,iplane)
c
         if ((abs(varwat).gt.1.0e-7) .or.
     1                             (fglckw.eq.1)) then
c        
         if (varwat .lt. 0) then
c        drain water in the frozen zone first
           if( nwfrzz(i,iplane).gt. 0.0 ) then
             varwat = varwat + nwfrzz(i,iplane)
             if (varwat.le.0.0) then
                nwfrzz(i,iplane) = 0.0
             else
                nwfrzz(i,iplane) = varwat
                varwat = 0.0
             endif
           endif
c
           if(varwat.ne.0.0) then              
           do 15 j = 1, jend
c          water drained
              vardp = dg(i,iplane)/jend - slfsd(j,i,iplane)
              if (vardp.gt.0.001)then
                  varchg = (slsw(j,i,iplane) - saxfc(i,iplane))*vardp
c                 drain the first layer
                  if (varchg.gt.0.0) then
                  if (varchg .le. -varwat) then
                      slsw(j,i,iplane) = saxfc(i,iplane)
                      varwat = varwat + varchg
                  else
                      slsw(j,i,iplane) = slsw(j,i,iplane) +varwat/vardp
                      varwat = 0.0
                  endif                  
                  endif
              endif
15         continue
           endif
c
         else
c        water is added in to this layer
            if (frozen(i,iplane) .ge. 0.001) then
c              water infiltrated into a frozen layer
               nwinc = varwat/dg(i,iplane)* frozen(i,iplane)
               spcav = ul(i,iplane)/dg(i,iplane) * frozen(i,iplane) 
     1               - frzw(i,iplane) - nwfrzz(i,iplane)
               if(spcav .lt. 0.0) spcav = 0.0
c
               if (nwinc.gt.spcav) then
                   nwinc = spcav
               endif
c
               nwfrzz(i,iplane) = nwfrzz(i,iplane) + nwinc
               varwat = varwat - nwinc
               if(varwat.lt.1e-5) then
                  nwfrzz(i,iplane) = nwfrzz(i,iplane) + varwat
                  varwat = 0.0
               endif
            endif              
         endif
c        
         varswc = 0.0
         varsw = 0.0
         varufp = dg(i,iplane) - frozen(i,iplane)
         if (varufp .gt. 0.001) then
c             change of soil water content (soil water per meter of soil)
              varsw = varwat /varufp
         endif
c
         if ((abs(varsw) .gt. 0.00001).or.(fglckw.eq.1)) then
         do 40 j = 1, jend
c            need handle possible negative values 
             vardp = dg(i,iplane)/jend - slfsd(j,i,iplane)
             varswc = varsw
c
             if (vardp.gt.0.001) then
             if(fglckw.eq.1) then
                 varswc = varsw - lackw/vardp
                 fglckw = 0
                 lackw = 0.0
             endif
c
             if(varswc.ge.0.0) then
                 slsw(j,i,iplane) = slsw(j,i,iplane) + varswc
             else
                  varchg = slsw(j,i,iplane) - thetdr(i,iplane)
                  if(varchg .ge. (-varswc))then
                      slsw(j,i,iplane) = slsw(j,i,iplane) + varswc
                  else
                      fglckw = 1
                      lackw = -(varswc + (slsw(j,i,iplane) 
     1                                 - thetdr(i,iplane)))*vardp
                      slsw(j,i,iplane) = thetdr(i,iplane)
                  endif
             endif
             endif
40       continue
c
         endif
         endif
c
      endif
30    continue
c
      endif
c
      return
      end

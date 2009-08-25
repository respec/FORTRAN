      subroutine watdst(qwater,flxtim,fgfzft)
c
c     +++PURPOSE+++
c     This function is for unfrozen soil water redistribution when frost exist
c
c     The purpose of this program is to better track water redistribution 
c     cause by frozen front. Saxton and Rawls, 2006 model was used 
c     to estimate soil water potential and hydraulic conductivity.
c
c     Author(s):  Shuhui Dun, WSU
c     Date: 02/25/2008

c     Verified and tested by: 
c
c
c     +++ARGUMENT DECLARATIONS+++
      real  qwater,flxtim
      integer fgfzft
c
c     +++ARGUMENT DEFINITIONS+++
c     qwater - water flux
c     flxtim - time for water redistribution
c     fgfzft - flag for what type of water redistribution,
c              0 for no frozen front, 1 for around frozen front, 
c              2 for unfrozen layers with frozen front in the soil profile
c
c      +++PARAMETERS+++
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
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
      integer  j,i,ffztuf,wklyn,wkflyn,lyblwk,flblwk,flgcal,
     1         ftthln,ftthfl,varki,varwtk,vardrk,varwfk,vardfk,
     1         jend
      real     wtPkpa,wpmj,wpmjp1,kusj,kusjp1,vartim,
     1    smoist,sdepth,varwtp,varkus,varsmc,varflx,varsm,
     1    surthd,topthd,topfdp,btmfdp,varj,varjp1,
     1    varwet,vardry,varsmw,varsmd,swavg,
     1    flthck,flbltk,ftthtk,vartkw,vartkd
c
c     +++LOCAL DEFINITIONS+++
c
c
c     wtPkps - water potential of a soil layer in kpa.
c     wpmj   - j layer water potential in meter. 
c     wpmjp1 - j + 1 layer water potential in meter.
c     kusj - j layer unsatuareated hydraulic conductivity (m/s.)
c     kusjp1 - j+1 layer unsatuareated hydraulic conductivity (m/s.) 
c
c     ffztuf - number of frozen to unfrozen
c
c     wklyn : soil layer number where freezong or thawing front is (working)
c     wkflyn: finer layer number of the working position
c     lyblwk - the soil layer of the finer layer right blew the working finer layer
c     flblwk - the finer soil layer blew the working layer
c
c     smoist - limit for minimum soil moisture (from David Hall)
c     sdepth - limit for minimum depth value (from David Hall)
c
c     varwtp  - depth variable
c     varsm  - soil moisture variable
c     vartim - time of water flux
c     varkus - thickmess variable
c     varsmc - variable for soil moisture change
c
c     surthd -  thawed thickness at surface
c     topthd - thawed thickness below the first frost layer (fist sandwitch frost)
c     topfdp - thickness of the first frost layer
c     btmfdp - depth of the bottom frost layer
c     pdthdp - previous hour surface thaw depth
c
c     ftthln - frozen to unfrozen soil layer number
c     ftthfl - frozen to unfrozen fine soil layer number
c
c     flgcal - calculation flag 
c              0 for to get depth of the bottom frost layer (btmfdp)
c              1 for thickness of the surface tahwed layer (surthd)
c              2 for thickness of the first frost layer (topfdp)
c              3 for thickness of the thawed layer below the first frost layer (topthd)
c
c
c     +++DATA INITIALIZATIONS+++
c
c     +++END SPECIFICATIONS+++
c
      smoist = 0.001
      sdepth = 0.001
      if(frdp(iplane) .lt. sdepth) frdp(iplane) = 0.0
      if(tfrdp(iplane) .lt. sdepth) tfrdp(iplane) = 0.0
c
cd    Added by S. Dun, July 28,2008  
cd    to turn off the water redistribution calculations in winter subroutines
      if (wintRed.eq.1) then
cd    end adding
      ffztuf = 0
c
      do 10 i = 1, nsl(iplane)      
c
      jend = nfine(i)
      if(i.eq.nsl(iplane)) jend = jend -1
c
      do 15 j = 1, jend
c        the two adjecent layers
         wkflyn = j
         wklyn = i
         varflx = 0.0
         if ((j+1).gt.nfine(i)) then
            lyblwk = wklyn + 1
            flblwk = 1
         else
            flblwk = j+1
            lyblwk = wklyn
         endif
c        
         flthck = dg(wklyn,iplane)/nfine(wklyn)
         flbltk = dg(lyblwk,iplane)/nfine(lyblwk)
c
         if ((fgfrst(wkflyn,wklyn,iplane).ne.0).and.
     1              (fgfrst(flblwk,lyblwk,iplane).eq.0)) then
c        From frozen to unfrozen layer
c            one more sandwitch layer
             ffztuf = ffztuf + 1 

             if ((ffztuf.eq.1) .and. (fgfzft.eq.1)) then
c            End of first frost layer and water distribution around frozen front
                  vartim = flxtim
                  slsw(flblwk,lyblwk,iplane) =slsw(flblwk,lyblwk,iplane)
     1                - qwater* vartim/flbltk
c                 We need to figure out how to do with negative values
c                 Now the soil water would not be drained to lower than its wilting point value.
c                 because a limit is set to qwater in its calculation.             
             endif
         endif

         if((ffztuf.eq.1) .and. (fgfzft.eq.2)) then
c        for unfrozen soil with first frozen front in the unfrozen zone
                vartim = flxtim - sltime(wkflyn,wklyn,iplane)
         else
              vartim = flxtim
         endif
c 
c
         if (vartim.gt.60.) then
c        time is greater than 1 minute

         if ((fgfrst(wkflyn,wklyn,iplane).eq.0).and.
     1            (fgfrst(flblwk,lyblwk,iplane).eq.0)) then
c        For two adjecent unfrozen layers

              sltime(wkflyn,wklyn,iplane) = sltime(wkflyn,wklyn,iplane)
     1                                     + vartim
c
              varj = slsw(wkflyn, wklyn, iplane)
              call saxfun(wklyn,varj, varwtp, varkus)
              wpmj = varwtp
              kusj = varkus
c
              varjp1 = slsw(flblwk, lyblwk, iplane)
              call saxfun(lyblwk,varjp1, varwtp, varkus)
              wpmjp1 = varwtp
              kusjp1 = varkus
c
              varkus = ( kusjp1+kusj)/2.
c
c             Apply Darcy's law
c             Because we are using the maximum hydraulic gradient to estimate 
c             soil water migration rate. Therefore we divided by 2. for assuming 
c             migration rate slows dowm due to water depletion in the adjecent layer.
              varflx = -varkus * (1. + (wpmj - wpmjp1)/flthck)/2.
c
c             find which layer gave water and which layer receive water
              if (varflx.gt.0.0) then
                  varsmw = varjp1
                  varwtk = lyblwk
                  varwfk = flblwk 
                  vartkw = flbltk    
                  varsmd = varj
                  vardrk = wklyn
                  vardfk = wkflyn
                  vartkd = flthck
              else
                  varsmw = varj
                  varwtk = wklyn
                  varwfk = wkflyn
                  vartkw = flthck
                  varsmd = varjp1
                  vardrk = lyblwk
                  vardfk = flblwk
                  vartkd = flbltk 
              endif

c             potential soil moisture change in the wetter layer
              varwet = (abs(varflx)*vartim)/vartkw
c             potential soil moisture change in the drier layer
              vardry = (abs(varflx)*vartim)/vartkd
              
              if ((varflx.ne.0.0).and.
     1                      (varsmw .gt. thetdr(varwtk,iplane))) then
c             if wetter layer is drier than its wilting point, 
c             then do not redistribute water.
c
              if (varwet.ge. (varsmw -thetdr(varwtk,iplane))) then
c               averaging the soil moisture 
c               if the wetter layer could drain to its wilting point
                swavg =(slsw(wkflyn,wklyn,iplane)*flthck
     1            +  slsw(flblwk,lyblwk,iplane)*flbltk)
     1            / (flthck + flbltk)
c
                if(swavg .gt. thetdr(vardrk,iplane)) then              
                if(swavg .lt. thetdr(varwtk,iplane)) then
                   slsw(vardfk,vardrk,iplane)=slsw(vardfk,vardrk,iplane)
     1               +(slsw(varwfk,varwtk,iplane)-thetdr(varwtk,iplane))
     1                *vartkw/vartkd
                   slsw(varwfk,varwtk,iplane) = thetdr(varwtk,iplane)
                else
                   slsw(wkflyn,wklyn,iplane) = swavg
                   slsw(flblwk,lyblwk,iplane) = swavg
                endif
                endif
c
              elseif ((vardry + varsmd).ge. varsmw) then
c             average the soil moisture
c             if soil moisture in the drier layer is larger than the wetter after recieving 
c             water from the wetter layer.
                 swavg=(slsw(wkflyn,wklyn,iplane)*flthck
     1            +  slsw(flblwk,lyblwk,iplane)*flbltk)
     1            / (flthck + flbltk)
c
                 if ((swavg .gt.thetdr(varwtk,iplane)).and.
     1                          (swavg .gt.thetdr(vardrk,iplane))) then
c
                    slsw(flblwk,lyblwk,iplane) = swavg
                    slsw(wkflyn,wklyn,iplane) = swavg
                 endif
c  
              else
                  slsw(wkflyn,wklyn,iplane) =slsw(wkflyn,wklyn,iplane)
     1                + varflx* vartim/flthck
                  slsw(flblwk,lyblwk,iplane) =slsw(flblwk,lyblwk,iplane)
     1                - varflx* vartim/flbltk
              endif
              endif
c
         endif
         endif

         if(fgfzft.eq.1) then
c        For water redistribution arounf frozen front
         if ((fgfrst(wkflyn,wklyn,iplane).eq.0).and.
     1             (fgfrst(flblwk,lyblwk,iplane).ne.0)) then
c               reached another frozen layer anf forced to go out of do loop
                return
         endif
         endif
15    continue                        
10    continue
c
cd    Added by S. Dun, July 28,2008  
cd    to turn of the water redistribution calculations in winter subroutines
      endif
cd    end adding
c     --------------------------------------
c     Update frost information
      if (fgfzft .ne. 1) then
c     not for the call for water redistribution around freezing front
           surthd = 0
           topthd = 0
           topfdp = 0
           btmfdp = 0
c
           flgcal = 1
           frsttk(iplane) = 0.0
c
c          check through all the layers
           do 20 i = 1, nsl(iplane)
c
           jend = nfine(i)
c
           do 25 j = 1, jend
c
              if(slfsd(j,i,iplane).le. 0.001) then
c             Only considering frost depth larger tahn 1mm in a fine layer.
                   slfsd(j,i,iplane) = 0.
              endif 
c
c             the two adjecent layers
              wkflyn = j
              wklyn = i
              if ((j+1).gt.nfine(wklyn)) then
                  lyblwk = wklyn + 1
                  flblwk = 1
              else
                  flblwk = j+1
                  lyblwk = wklyn
              endif
c
              flthck = dg(wklyn,iplane)/nfine(wklyn)
c              
              frsttk(iplane) = frsttk(iplane) + 
     1                         slfsd(wkflyn,wklyn,iplane)
c
              if (flgcal .eq. 1) then
c             calculating surthd
                   if(fgfrst(wkflyn,wklyn,iplane).eq.1) then
c                  complete frozen
                       topfdp = topfdp + flthck
                        flgcal = 2
c                       get topfdp next

                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.0) then
c                  complete thawed
                       surthd = surthd + flthck
                        flgcal = 1
c                       get surthd next
c
                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.2) then
c                  frozen on top
                        topfdp = topfdp + slfsd(wkflyn,wklyn,iplane)
                     topthd = topthd + flthck  -
     1                             slfsd(wkflyn,wklyn,iplane)
                        flgcal = 3
c                       get topthd next
c
                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.3) then
c                  frozen on bottom
                      surthd = surthd + flthck -
     1                                slfsd(wkflyn,wklyn,iplane)
                         topfdp = topfdp + slfsd(wkflyn,wklyn,iplane)
                         flgcal = 2
c                        get topfdp next
                   endif
c              
                  elseif (flgcal .eq. 2) then
c             calculating topfdp
                   if(fgfrst(wkflyn,wklyn,iplane).eq.1) then
c                  complete frozen
                      topfdp = topfdp + flthck
                        flgcal = 2
c                       get topfdp next

                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.0) then
c                  complete thawed
                     topthd = topthd + flthck
                        flgcal = 3
c                       get topthd next
c
                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.2) then
c                  frozen on top
                        topfdp = topfdp + slfsd(wkflyn,wklyn,iplane)
                    topthd = topthd + (dg(wklyn,iplane)/nfine(wklyn)) -
     1                             slfsd(wkflyn,wklyn,iplane)
                        flgcal = 3
c                       get topthd next
c
                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.3) then
c                  frozen on bottom
                     topthd = topthd + flthck -
     1                                slfsd(wkflyn,wklyn,iplane)
                         flgcal = 0
c                        get btmfdp next
                         ftthln = wklyn
                         ftthfl = wkflyn
                   endif
c
c
              elseif (flgcal .eq. 3) then
c             calculating topthd
                   if(fgfrst(wkflyn,wklyn,iplane).eq.1) then
c                  complete frozen
                         flgcal = 0
c                        get btmfdp next
                         ftthln = wklyn
                         ftthfl = wkflyn

                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.0) then
c                  complete thawed
                      topthd = topthd + flthck
                        flgcal = 3
c                       get topthd next
c
                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.2) then
c                  frozen on top
                         flgcal = 0
c                        get btmfdp next
                         ftthln = wklyn
                         ftthfl = wkflyn
c
                   elseif (fgfrst(wkflyn,wklyn,iplane).eq.3) then
c                  frozen on bottom
                     topthd = topthd + flthck  -
     1                                slfsd(wkflyn,wklyn,iplane)       
                         flgcal = 0
c                        get btmfdp next
                         ftthln = wklyn
                         ftthfl = wkflyn
                   endif
c
c
              elseif (flgcal .eq. 0) then
c             calculating btmfdp
                   if (fgfrst(wkflyn,wklyn,iplane).ne.0) then
                      if ((wklyn.eq.nsl(iplane))
     1                   .and.(wkflyn.eq.jend)) then 
c                     bottom layer
                         ftthln = wklyn
                         ftthfl = wkflyn               
                      elseif (fgfrst(flblwk,lyblwk,iplane).eq.0) then
c                     form frozen to unfrozen 
                         ftthln = wklyn
                         ftthfl = wkflyn
                       endif
                   endif  
              endif 
25         continue               
20         continue
c
c          Update globle frost variables
c
           if (flgcal .eq. 0) then
c          Sandwitch layer exists
               ftthtk = dg(ftthln,iplane)/nfine(ftthln)
c
               btmfdp = ftthtk* ftthfl
c              depth in the soil layer frost bottom resides
c
c              add in the depth of the top of the soil layer with frost bottom
               if (ftthln .gt. 1) then
                   btmfdp = btmfdp + solthk(ftthln-1,iplane)
               endif
c
               if (fgfrst(ftthfl,ftthln,iplane).eq.2) then
c              top frost in a fine layer    
                   btmfdp = btmfdp + slfsd(ftthfl,ftthln,iplane)
     1                      - ftthtk
               endif
c
               frdp(iplane) = btmfdp
               thdp(iplane) = surthd
               tfrdp(iplane) = thdp(iplane) + topfdp
               tthawd(iplane) = tfrdp(iplane) + topthd               
c
           elseif (flgcal .eq. 1) then
c          no frost layer 
               frdp(iplane) = 0.
               thdp(iplane) = 0.
               tfrdp(iplane) = 0.
               tthawd(iplane) = 0.
c
           else
c          no sandwitch layers, flagcal equals 2 or 3.
               thdp(iplane) = surthd
               frdp(iplane) = thdp(iplane) + topfdp
               tfrdp(iplane) = 0.
               tthawd(iplane) = 0.

           endif
c
c
c     frost cycle
c     
      if((fgcycl(iplane) .eq. 1).and. (thdp(iplane).gt.0.01)) then
c     account a frost cycle when thawing processes alters freezing in 1 cm depth
          fcycle(iplane) = fcycle(iplane) + 1
      endif
c     thawed through 1 cm depth
      if (thdp(iplane).gt. 0.01) then
            fgcycl(iplane) = 0
      else
          fgcycl(iplane) = 1
      endif

      endif
c     ----------------------------------------------                   

c
      return
      end

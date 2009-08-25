      subroutine mlttp(hour)
c
c     +++PURPOSE+++
c     This function is responsible for top thawing
c
c     Author(s):  Shuhui Dun, WSU
c     Date: 02/27/2008
c     Verified by: Joan Wu, WSU
c
c
c     +++ARGUMENT DECLARATIONS+++
      integer  hour
c
c     +++ARGUMENT DEFINITIONS+++
c     hour   - The hour of the day that we are calculating.
c
c     +++PARAMETERS+++
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
      include 'cwater.inc'
c     read: dg(i,iplane)
c
      include 'ccons.inc'
c      read: bdcons, bulk density
c
c     +++LOCAL VARIABLES+++
c
      integer  layerN,flyerN,tpbtfg,lyabwk,flabwk,varfg,
     1         fgfzft,wklyn,wkflyn,i,j,fgwhld,abwkly,abwkfl,
     1         jstart,jend
      real     htreq,ceh2o,lhfh2o,decr,mlteng,mltime,flmlt,
     1         frzdp,ofrzdp,eratio,mdufdp,kres,kufzfl,oslfsd
      real     smoist,sdepth,vardp,varsm,varthk,varsmc,vartmp,
     1         frzwat,spcav,varwat,varthd,tmpvr2,tmpvr1,
     1         flthck,flabtk
c
c     +++LOCAL DEFINITIONS+++
c     htreq  - Heat (energy), required to melt frost of current finer layer (J/m^2).
c     lhfh2o - Latent heat of fusion of water (J/m^3).
c     decr   - Depth that the frost melt in a fine layer (m).
c     eratio - ratio of the requied energy to melt the frost
c     mlteng - engery flux to melt the frost
c     flmlt - time needed to melt a fine layer
c     mltime - total thawing time used
c     kres   - Thermal conductivity of residue layer (W/m C).
c
c     smoist - limit for minimum soil moisture (from David Hall)
c     sdepth - limit for minimum depth value (from David Hall)
c
c     layerN - soil layer number
c     flyerN - finer soil layer number
c     tpbtfg - a flag for  start point or ending point is required,
c              0 for starting point and 1 for ending point.
c
c     fgfzft - flag for what type of water redistribution,
c              0 for no frozen front, 1 for around frozen front, 
c              2 for unfrozen layers with frozen front in the soil profile
c
c     fgwhld - a flag indicates there is holding water on top,
c              0 for it is not a water holding run, 1 for a water holding run.
c
c     wklyn  - soil layer number where freezong or thawing front is (working)
c     wkflyn - finer layer number of the working position
c     lyabwk - the soil layer of the finer layer right above the working finer layer
c     flabwk - the finer soil layer above the working layer
c
c     frzdp  - depth of the frozen front
c     ofrzdp  - depth of the frozen front before update
c
c     vardp  - depth variable
c     varsm  - soil moisture variable
c     varfg  - frost flag varaible
c     varthk - thickmess variable
c     varsmc - variable for maximum water flow rate an adjecent layer can supply
c
c     mdufdp - thickness of unfrozen layers between current melting front 
c              and next frozen layer to melt
c     frzwat - the ammount of liquid water in frozen of a fine layer (m)
c     spcav  - space available for holding liquid water in the frozen zone
c     flthck - current fine layer thickness
c
c     +++DATA INITIALIZATIONS+++
c
c     Latent heat of fusion of ice in J/m3
      data   lhfh2o/3.35e08/
c
c      kres = 0.168
cd      kres = 0.0232
       kres = 0.05
c
c     +++END SPECIFICATIONS+++
c
c     Starting point of thawing
      if (thdp(iplane) .lt. 0.001) then
         frzdp = dg(1,iplane)/nfine(1)/2.
         wklyn = 1
         wkflyn = 1
      else
         frzdp = thdp(iplane)
         call locate(frzdp,layern,flyern,0)
         wklyn = layern
         wkflyn = flyern
      endif
      if((wklyn.eq.nsl(iplane)).and.(wkflyn.eq.FLNbtm)) then
         wkflyn = FLNbtm - 1
      endif
c
c     Check if the work layer should be the one above due to frost heave has no depth
      if((wklyn.eq.1) .and.(wkflyn.eq.1) )then
      else
         abwkfl = wkflyn -1
         if (abwkfl.lt.1) then
             abwkly = wklyn - 1
             abwkfl = nfine(abwkly)
         else
             abwkly = wklyn
         endif
c         varfg = fgfrst(abwkfl,abwkly,iplane)
c         if((varfg.eq.2) .or. (varfg.eq.3)) then
         if(slsic(abwkfl,abwkly,iplane).gt.0.0) then
             wklyn = abwkly
             wkflyn = abwkfl
         endif
      endif      
c
c     Do loop
      mltime = 0.
      flmlt = 0.
      decr = 0
      fgwhld = 0
c
c     There are 3600 seconds in a hourc
c    ***************************************************
      do while (mltime.lt.3600.)
c     Loop till no more energy availabe to thaw the soil
c
c     Locate the thawing front and calculate qout
      if (mltime .eq. 0.0) then
c     starting, surface temperature is greater than 0 and
c     no snow would be on theground when is routine is called.
c
c         Harmonic mean of thermal conductivity above the freezing or thawing front
          tmpvr2 = 0.
c
c         Considering thermal conductivity of the residue
          if (resdep(iplane) .gt. 0.001) then
             tmpvr2 = tmpvr2 + resdep(iplane)/kres
          endif
c
          do 20 i = 1, wklyn
c      
               jstart = 1
c
               if (i .eq. wklyn) then
                   jend = wkflyn
               else 
                   jend = nfine(i)
               endif
               flthck = dg(i,iplane)/nfine(i)
c                        
             do 25 j = jstart, jend
c               Calculate Thermal conductivity of unfrozen soil
                tmpvr1 = 0.5096 + 7.4493 * slsw(j,i,iplane) 
     1                   - 8.7484 * slsw(j,i,iplane) ** 2
                kufzfl = tmpvr1 * (0.0014139*bdcons(i,iplane) - 1.0588)

                if ((i.eq. 1) .and. (j.eq.1)) then
                    vardp = flthck
                elseif ((i .eq. wklyn).and. (j.eq. wkflyn)) then
c
                    vardp = (flthck - slfsd(j, i, iplane))
                else
                    vardp = flthck
                endif
c
                if (kufzfl.gt.0)
     1               tmpvr2 = tmpvr2 + vardp/kufzfl
c
25        continue
20        continue
c

          qoutdm = tmpvr2 
c
      else
c     when thawing front moved down one or many fine layers.
c
c         Calculate Thermal conductivity of unfrozen soil
          varsm = slsw(wkflyn, wklyn, iplane)
          tmpvr1 = 0.5096 + 7.4493 * varsm - 8.7484 * varsm ** 2
          kufzfl = tmpvr1 * (0.0014139*bdcons(wklyn,iplane) - 1.0588)
c
c         The value 10 is for 10 finer layers in each soil layer
          if (kufzfl.gt.0)
     1        qoutdm = qoutdm + decr/kufzfl
c            
          wkflyn = wkflyn + 1
          if (wkflyn .gt. nfine(wklyn)) then
               wklyn = wklyn + 1
               wkflyn = 1
          endif 
c
          if(wklyn.gt.nsl(iplane)) then
c         thaw through
               fgthwd(iplane) = 1                 
               return
           endif
c 
c         Check if current layer is unfrozen
30        varfg = fgfrst(wkflyn, wklyn, iplane)
          varsm = slsw(wkflyn, wklyn, iplane)
c         Calculate Thermal conductivity of unfrozen soil
          tmpvr1 = 0.5096 + 7.4493 * varsm - 8.7484 * varsm ** 2
          kufzfl = tmpvr1 * (0.0014139*bdcons(wklyn,iplane) - 1.0588)
c         current fine layer thickness
          flthck = dg(wklyn,iplane)/nfine(wklyn)  
c
          if(varfg .eq. 0) then
c         a complete unfrozen layer
c
             if (kufzfl.gt.0)
     1            qoutdm = qoutdm + flthck/kufzfl
c
                  wkflyn = wkflyn + 1
              if (wkflyn .gt. nfine(wklyn)) then
                  wklyn = wklyn + 1
                  wkflyn = 1
              endif
c
              if(wklyn.gt.nsl(iplane)) then
c             thaw through
                 fgthwd(iplane) = 1                 
                 return
              endif
c
              goto 30
c
          elseif (varfg .eq. 3) then
c         partialy frozen, frost at bottom
              vardp = (flthck - slfsd(wkflyn, wklyn, iplane))
              if (kufzfl.gt.0)
     1            qoutdm = qoutdm + vardp/kufzfl
          endif
      endif
c
c      Thermal conductivity above first frost layer             
c          ktop = thickness /qoutdm
c
c     Thermal gradient of the layer: 
c          surtmp(hour)/ thickness
c
c     Heat flux from above
c     check for qoutdm <> 0 to prevent crash
c     this needs to be checked 4-24-2008 jrf
      if (qoutdm.ne.0.0) then
          qhtout = surtmp(hour) / qoutdm
      else
          qhtout = 0.0
      endif
c      qhtout = surtmp(hour) / qoutdm
c     -----------------------------------------------------
c
      mlteng = qhtout
c     
      varfg = fgfrst(wkflyn, wklyn, iplane)
c     frozen layer thickness in a fine layer
      varthk = slfsd(wkflyn,wklyn,iplane)
c     current fine layer thickness
      flthck = dg(wklyn,iplane)/nfine(wklyn)
c
c     Engery required to thaw the frost in current layer J/m2                  
      htreq = lhfh2o * slsic(wkflyn,wklyn,iplane)
c
c
c     The calculation mlteng*3600 convert energy flux for mlteng in W/m2 to J/m2.
c     There are 3600s in a hour.
c
      if ((mlteng*(3600.-mltime) - htreq) .lt. 0.0) then
c     The energy is only enough to melt a fraction of frost in current layer
           eratio = mlteng * (3600. - mltime)/htreq 
           decr = eratio * varthk           
c          no more energy
           flmlt = 3600. - mltime
      else                
           flmlt = htreq/mlteng
c
           if(flmlt.lt. 1.0) flmlt =1.0
c          in order to go for next layer, a value of 1 second is assigned for the thawing time
c          when the frozen thickness is too small to keep the program go to next layer
c                        
           decr = varthk
             eratio = 1.         
      endif
c
c     Update fine layer frost variables
      if (decr .lt. 0.001) decr = 0.0
      if ((varthk - decr).lt.0.001) decr = varthk
c
      if (eratio.eq.1.) then
            fgfrst(wkflyn, wklyn, iplane) = 0
c      
            slsw(wkflyn, wklyn, iplane) = (slsw(wkflyn, wklyn, iplane)*
     1            (flthck - slfsd(wkflyn, wklyn, iplane))
     1            + slsic(wkflyn, wklyn, iplane))/flthck

c
            slsic(wkflyn, wklyn, iplane) = 0.0
            slfsd(wkflyn, wklyn, iplane) = 0.0
c
      elseif(decr.gt.0.001) then
c     Partially thawed
c     if thawing depth is greater than 1 mm change the frost flag
            fgfrst(wkflyn, wklyn, iplane) = 3

c
            oslfsd = slfsd(wkflyn, wklyn, iplane)
            slfsd(wkflyn, wklyn, iplane) = slfsd(wkflyn,wklyn,iplane)
     1                                     - decr
              if (slfsd(wkflyn, wklyn, iplane).lt. 0.001) then 
                  slfsd(wkflyn, wklyn, iplane) = 0.0
              endif
c
            slsw(wkflyn, wklyn, iplane) = (slsw(wkflyn, wklyn, iplane)*
     1            (flthck - oslfsd)
     1            + slsic(wkflyn, wklyn, iplane)* eratio) 
     1           /(flthck - slfsd(wkflyn, wklyn, iplane))
c
            slsic(wkflyn, wklyn, iplane) = slsic(wkflyn, wklyn, iplane)*
     1                                     (1 - eratio)
      else
c     Partially thawed, thawing depth is less than 1mm
            vartmp = flthck - slfsd(wkflyn, wklyn, iplane)
c
            if (vartmp .gt. 0.001) then
c           current layer has room to hold the thawed water
               slsw(wkflyn, wklyn, iplane) = slsw(wkflyn, wklyn, iplane)           
     1                     + slsic(wkflyn, wklyn, iplane)* eratio/vartmp 
            else
c           Current layer has no room to hold the thawed water,
c           water then goes to the layer above
c              
               if ((wklyn.eq.1) .and.(wkflyn.eq.1)) then
c              top layer then water ponding
                    watpdg(iplane) = slsic(wkflyn,wklyn,iplane)* eratio
     1                             + watpdg(iplane)
                    fgwhld = 1
               else
c                  Layer numbers of the fine layer below                      
                   flabwk = wkflyn - 1
                   if (flabwk .lt. 1) then
                       lyabwk = wklyn - 1
                       flabwk = nfine(lyabwk)
                   else
                       lyabwk = wklyn
                   endif
c
c               the fine layer thickness below current layer
                flabtk = dg(lyabwk,iplane)/nfine(lyabwk)
                slsw(flabwk,lyabwk,iplane) =slsw(flabwk,lyabwk, iplane)
     1                    + slsic(wkflyn,wklyn,iplane)* eratio/flabtk

                endif
            endif
c
            slsic(wkflyn, wklyn, iplane) = slsic(wkflyn, wklyn, iplane)*
     1                                     (1 - eratio)
c
c           ice in the soil layer is negeligible          
            if(slsic(wkflyn, wklyn, iplane).lt.1e-5) then
                 fgfrst(wkflyn, wklyn, iplane) = 0 
                 slsw(wkflyn,wklyn,iplane) = (slsw(wkflyn,wklyn,iplane)
     1             *(flthck - slfsd(wkflyn,wklyn,iplane))           
     1             + slsic(wkflyn,wklyn,iplane))/flthck
                 slsic(wkflyn, wklyn, iplane) = 0.0
                 slfsd(wkflyn,wklyn,iplane) = 0.0
                 if((wklyn.eq.nsl(iplane)).and.(wkflyn.eq.FLNbtm))
     1                      fgthwd(iplane) = 1 
            endif
c            
      endif
c
      if ((wklyn.eq.1) .and.(wkflyn.eq.1)) then
         if ((fgwhld .ne. 1).and.(watpdg(iplane).gt.0.0)) then
            if(decr.gt. 0.001) then
c
                slsw(wkflyn,wklyn,iplane) =slsw(wkflyn,wklyn,iplane)
     1                           + watpdg(iplane)/decr
                watpdg(iplane) = 0.
             endif
         endif
      endif               
c
c     Handling the liquid (infiltrated) water in the frozen zone
c     Thickness of thawed layer
      varthd = flthck -(varthk-decr)
      if (varthd.gt.0.001) then
c      
        if (nwfrzz(wklyn,iplane).gt.1e-5) then
          if ((frozen(wklyn,iplane).gt.0.001)
     1                              .and. (varthk.gt.0.001)) then
c           liquid water in frozen zone
            frzwat = nwfrzz(wklyn,iplane)/frozen(wklyn,iplane)*varthk
c           available space
            spcav = ul(wklyn,iplane)/dg(wklyn,iplane)*varthk
     1            - slsic(wkflyn,wklyn,iplane)
            if(spcav .lt. 0.0) spcav = 0.0
            if (frzwat.gt.spcav)  frzwat = spcav
c           liquid water released
            varwat = frzwat/varthk*decr
c
            nwfrzz(wklyn,iplane) = nwfrzz(wklyn,iplane) - varwat
            frozen(wklyn,iplane) = frozen(wklyn,iplane) - decr
c
            if ((frozen(wklyn,iplane).lt.0.001) .or.
     1                       (nwfrzz(wklyn,iplane).lt. 0.00001)) then
                slsw(wkflyn,wklyn,iplane) = slsw(wkflyn,wklyn,iplane)
     1               + varwat/varthd  + nwfrzz(wklyn,iplane)/varthd
c
                 nwfrzz(wklyn,iplane) = 0.0
                 frozen(wklyn,iplane) = 0.0
            else           
                slsw(wkflyn,wklyn,iplane) = slsw(wkflyn,wklyn,iplane)
     1                                    + varwat/varthd
            endif
         endif
c 
        else
          slsw(wkflyn,wklyn,iplane) = slsw(wkflyn,wklyn,iplane)
     1                             + nwfrzz(wklyn,iplane)/varthd
          nwfrzz(wklyn,iplane) = 0.0
          frozen(wklyn,iplane) = 0.0
        endif
c
      endif
c 
c     Total thawing time
      mltime = mltime + flmlt
c      
      Enddo
c     End of the do loop
c     *************************************************************
c
      return
      end

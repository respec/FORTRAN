      subroutine mltbtm(hour)
c
c     +++PURPOSE+++
c     This function is responsible for Bottom-up thawing
c
c     The purpose of this program is to simulate melting that occurs from the bottom-up
c     (1) When only one frost layer exists and surface temperature is below 0C , 
c     however heat from soil below is greater than the cold flux from top
c     (2) Heat flux from soil below melts the bottom of the frost.
c
c     Author(s):  Shuhui Dun, WSU
c     Date: 02/26/2008
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
      include 'ctcurv.inc'
c      read: annual air temperature curve coefficients
c
      include 'ccons.inc'
c      read: bdcons, bulk density
c
c
c     +++LOCAL VARIABLES+++
c
      integer  layerN,flyerN,tpbtfg,lyblwk,flblwk,varfg,
     1         fgfzft,wklyn,wkflyn,i,j,LN1mbf,FLN1mb,fgwhld,
     1         jstart,jend,flycn
      real     htreq,ceh2o,lhfh2o,decr,mlteng,mltime,flmlt,
     1         frzdp, ofrzdp,mdufdp,qdrysd,eratio,
     1         tmpbl,tmpdp,dmping,kufzfl,oslfsd,kufz
      real     smoist,sdepth,vardp,varsm,varthk,varsmc,
     1         varwtp,varkus,tmpvr1,tmpvr2,vartmp,
     1         frzwat,spcav,varwat,varthd,flthck,flbltk
c
c     +++LOCAL DEFINITIONS+++
c     htreq  - Heat (energy), required to melt frost of current finer layer (J/m^2).
c     lhfh2o - Latent heat of fusion of water (J/m^3).
c     decr   - Depth that the frost melt in a fine layer (m).
c     eratio - ratio of the requied energy to melt the frost
c     mlteng - engery flux to melt the frost
c     flmlt - time needed to melt a fine layer
c     mltime - total thawing time used
c
c      tmpbl - average temperature 1 meter below frozen front
c      tmpdp - depth for the estimated temperature
c     dmping - dmping depth for yearly cahnge in soil
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
c     wklyn : soil layer number where freezong or thawing front is (working)
c     wkflyn: finer layer number of the working position
c     lyblwk - the soil layer of the finer layer right below the working finer layer
c     flblwk - the finer soil layer below the working layer
c     LN1mbf - the soil layer 1 meter below the forst layer
c     FLN1mb - the fine layer number 1 meter below the forst layer
c
c     frzdp  - depth of the tawing front
c     ofrzdp - depth of tahwing front before update 
c
c     vardp  - depth variable
c     varsm  - soil moisture variable
c     varfg  - frost flag varaible
c     varthk - thickmess variable
c     varsmc - variable for maximum water flow rate an adjecent layer can supply
c
c     mdufdp - thickness of unfrozen layers between current melting front 
c              and next frozen layer to melt
c     qdrysd - the depth where starts to calculate qdry from below 1 meter.
c 
c     frzwat - the ammount of liquid water in frozen of a fine layer (m)
c     spcav  - space available for holding liquid water in the frozen zone
c
c     flycn  - number of fine layers, it changes for the bottom soil layer,
c              it is 10 for other layers.
c     flthck - thickness of a fine layer (m)
c
c     +++DATA INITIALIZATIONS+++
c
c     Latent heat of fusion of ice in J/m3
      data   lhfh2o/3.35e08/

c     +++END SPECIFICATIONS+++
c
c     Starting point of thawing at the bottom of the frost layer
c
      frzdp = frdp(iplane)
      qdrysd = frzdp
      call locate(frzdp,layern,flyern,1) 
      wklyn = layern
      wkflyn = flyern
c
c    Layer numbers of the fine layer below the working layer
      flblwk = wkflyn + 1
      if (flblwk .gt. nfine(wklyn)) then
          lyblwk = wklyn + 1
          flblwk = 1
      else
          lyblwk = wklyn
      endif
      varfg = fgfrst(flblwk,lyblwk,iplane)
      if (varfg.ne.0) then
          wklyn = lyblwk
          wkflyn = flblwk
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
c     Locate the melting front
c     when melting front moved upword one or many fine layers. 
      if (mltime .gt. 0.0) then
c
           wkflyn = wkflyn - 1
           if (wkflyn .lt. 1) then
              wklyn = wklyn - 1
              if(wklyn.lt.1) then
c             thaw through 
                  fgthwd(iplane) = 1
                  slsw(1, 1, iplane) = slsw(1, 1, iplane)
     1                  + watpdg(iplane)/( dg(1,iplane)/nfine(1))
                  watpdg(iplane) = 0.        
                  return
               else          
                  wkflyn = nfine(wklyn)
               endif
           endif
c
c         Check if current layer is unfrozen
          mdufdp = 0.
c
30        varfg = fgfrst(wkflyn, wklyn, iplane)
          if(varfg .eq. 0) then
c         a complete unfrozen layer
c
            mdufdp = mdufdp + dg(wklyn, iplane)/nfine(wklyn)
c
            wkflyn = wkflyn - 1
            if (wkflyn .lt. 1) then
                wklyn = wklyn - 1
                if(wklyn.lt.1) then
c               thaw through 
                    fgthwd(iplane) = 1
                    return
                 else 
                    wkflyn = nfine(wklyn)
                 endif
             endif
c         
             goto 30
c
          elseif (varfg .eq. 2) then
c         partialy frozen, frost at top
             mdufdp = mdufdp + (dg(wklyn, iplane)/nfine(wklyn)
     1                        - slfsd(wkflyn, wklyn, iplane))
c
          endif
c
          ofrzdp = frzdp
          frzdp = frzdp - decr - mdufdp
c
          if (frzflg.eq.4) then
c         Only one frost layer and bottom thawing
c         Heat flux from soil surface needs to be re-estimated.
c         mdfzdp shall be 0 under this condition
c                    
          if (frzdp.le.tilld(iplane)) then
c         thawing front is in the tillage zone now
c
              if (ofrzdp.le.tilld(iplane)) then
c             thawing front was in the tillage zone
                  qoutdm = qoutdm - (decr + mdufdp)/kftill
              else
c             tahwing front was below the tillage zone
                   vardp = (tilld(iplane) - frzdp)              
                   qoutdm = qoutdm - vardp/kftill
     1                 - (decr + mdufdp - vardp)/kfutil
              endif
            else
c         thawing front is below the tillage zone
                qoutdm = qoutdm - (decr + mdufdp)/kfutil
          endif
c
c          Thermal conductivity above first frost layer             
c               ktopf = thickness /qoutdm
c
c         Thermal gradient of the layer: 
c               surtmp(hour)/ (thickness
c
c          Heat flux from above
c          check for possible divide by zero. jrf 5-4-2008
c
           if (qoutdm .ne. 0.0) then
              qhtout = surtmp(hour) / qoutdm
           else
              qhtout = 0
           endif
           endif
c          end condtition for  frzflg = 4
      endif
c
c
      if ((qdrysd - frzdp) .gt. 0.1) then
c
      qdrysd = frzdp
c
c      Estimate temperature 1 meter below the frost layer.
c      If estimated temperature is below 0C, then the heat from blow is 0
      dmping = 2.0
      tmpdp = frzdp + 1.0
      tmpbl = YavgT + YampT * exp(-tmpdp/dmping)
     1        *sin(2*3.14/365. *(sdate-YpshfT)- tmpdp/dmping)
c
c     *********************************************************** 
c     *** Heat conducted from the warm soil blow frost layer  ***
c     *********************************************************** 
c     Calculate QDRY, heat flow from [the dry layers] beneath
c     the frost layer.
c
      if (tmpbl.le.0) then
          qdry = 0.
      else 
c         Estimate thermal conductivity of 1 meter below the frost layer.
c         The harmonic mean of the known layers is used for the whole 1 meter.
c
c         Ending loction of 1 meter blow the frost layer
          vardp = frdp(iplane) + 1.0
c
          if (vardp .gt. solthk(nsl(iplane),iplane)) then
             LN1mbf = nsl(iplane)
               FLN1mb = FLNbtm
          else
             call locate(vardp,layern,flyern,1)
               LN1mbf = layern
               FLN1mb = flyern        
          endif
c
c         Harmonic mean of thermal conductivity for 1 meter below frost
          tmpvr2 = 0.
c
          do 20 i = wklyn, LN1mbf
c            
               if (i .eq. wklyn) then
                   jstart = wkflyn
               else
                   jstart = 1
               endif
c
               if (i .eq. LN1mbf) then
                   jend = FLN1mb
               else 
                   jend = nfine(i)
               endif
c                        
             do 25 j = jstart, jend
c               Calculate Thermal conductivity of unfrozen soil
                tmpvr1 = 0.5096 + 7.4493 * slsw(j,i,iplane) 
     1                   - 8.7484 * slsw(j,i,iplane) ** 2
                kufzfl = tmpvr1 * (0.0014139*bdcons(i,iplane) - 1.0588)
c
c               The value 10 is for 10 finer layers in each soil layer
                if (kufzfl.gt.0) then
                   tmpvr2 = tmpvr2 + (dg(i,iplane)/nfine(i))/kufzfl
                endif               
c
25           continue
20        continue
c         Harmonic mean of thermal conductivity of the 1.0 meter soil
          if (tmpvr2 .gt. 0.0) then
              kufz = 1.0/tmpvr2
          else
c             A value when soil water content is 0.0 and bulk density around 1000kg/m3
              kufz = 0.2
          endif
c 
c          the value 1.0 meter is for the distance between 0C and tepbl       
          qdry = kufz * tmpbl / 1.0
c
      endif
c
      endif
c
c     ------------------------------------------
      if (frzflg.eq.4) then
c     Only one frost layer and bottom thawing
c     Nothing happening on top
         mlteng = qdry + qhtout
      else
c     bottom thawing when top frezzing or thawing
         mlteng = qdry
      endif
c    --------------------------------------------
c
c     number of fine layers
      flycn = nfine(wklyn)
c     current fine layer thickness
      flthck = dg(wklyn,iplane)/flycn
      
c     frozen layer thickness in a fine layer    
      varthk = flthck
c
      varfg = fgfrst(wkflyn, wklyn, iplane)
c
      if ((varfg.eq.2).or. (varfg.eq.3)) then
c     the layer is partially frozen
           varthk = slfsd(wkflyn,wklyn,iplane)
      endif
c     The unit of htreq is J/m2                  
      htreq = lhfh2o * slsic(wkflyn,wklyn,iplane) 
c
c     

c     The calculation mlteng*3600 convert energy flux for mlteng in W/m2 to J/m2.
c     There are 3600 s in a hour.
c
      if ((mlteng*(3600.-mltime) - htreq) .lt. 0.0) then
c     The energy is only enough to melt a fraction of frost in current layer
           eratio = mlteng * (3600. - mltime)/htreq 
           decr = eratio * varthk           
c          no more energy
           flmlt = 3600. - mltime
c
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
      if ((varthk - decr).lt.0.001)  decr = varthk
c
      if (eratio.eq.1.) then
c     whole layer thawed
            fgfrst(wkflyn, wklyn, iplane) = 0
c      
            slsw(wkflyn, wklyn, iplane) = (slsw(wkflyn, wklyn, iplane)*
     1            (flthck - slfsd(wkflyn, wklyn, iplane))
     1            + slsic(wkflyn, wklyn, iplane))/flthck
c
            slsic(wkflyn, wklyn, iplane) = 0.0
            slfsd(wkflyn, wklyn, iplane) = 0.0
c
      elseif (decr.gt.0.001) then
c     Partially thawed, thawing depth is greater than 1mm
            fgfrst(wkflyn, wklyn, iplane) = 2
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
     1            /(flthck - slfsd(wkflyn, wklyn, iplane))
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
c           water then goes to the layer below
               if ((wklyn.eq.nsl(iplane)).and.(wkflyn.eq.FLNbtm)) then
c              the last layer of the soil profile
                   watbtm(iplane) = slsic(wkflyn,wklyn,iplane)* eratio
     1                              + watbtm(iplane)
                   fgwhld = 1
               else
c                  Layer numbers of the fine layer below
                   flblwk = wkflyn + 1
                   if (flblwk .gt. nfine(wklyn)) then
                      lyblwk = wklyn + 1
                      flblwk = 1
                   else
                      lyblwk = wklyn
                   endif
c
c             the fine layer thickness below current layer
               flbltk = dg(lyblwk,iplane)/nfine(lyblwk)
               slsw(flblwk,lyblwk,iplane) = slsw(flblwk,lyblwk, iplane)
     1              + slsic(wkflyn,wklyn,iplane)* eratio/flbltk
               endif
            endif

c
            slsic(wkflyn, wklyn, iplane) = slsic(wkflyn, wklyn,iplane)*
     1                                     (1 - eratio)
c
c           ice in the soil layer is negeligible          
            if(slsic(wkflyn, wklyn, iplane).lt.1e-5) then
                 fgfrst(wkflyn, wklyn, iplane) = 0 
                 slsw(wkflyn,wklyn,iplane) = (slsw(wkflyn,wklyn,iplane)
     1           *(flthck - slfsd(wkflyn,wklyn,iplane))           
     1             + slsic(wkflyn,wklyn,iplane))/flthck
   
                 slsic(wkflyn, wklyn, iplane) = 0.0
                 slfsd(wkflyn,wklyn,iplane) = 0.0
c
c                check if frost layers are thaw through
                 fgthwd(iplane) = 1
                 do 40 i = wklyn, 1, -1           
                    if (i .eq. wklyn) then
                       jstart = wkflyn
                    else
                       jstart = nfine(i)
                    endif                        
                    do 50 j = jstart, 1, -1
                       varfg = fgfrst(j, i, iplane)
                       if (varfg.ne.0) fgthwd(iplane) = 0
50                  continue                       
40               continue
            endif
c  
      endif
c
      if ((wklyn.eq.nsl(iplane)).and.(wkflyn.eq.FLNbtm)) then
         if ((fgwhld .ne. 1).and.(watbtm(iplane).gt.0.0)) then
            if(decr.gt. 0.001) then
c
                slsw(wkflyn,wklyn,iplane) =slsw(wkflyn,wklyn,iplane)           
     1                           + watbtm(iplane)/decr
                watbtm(iplane) = 0.
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
            slsw(wkflyn,wklyn,iplane) = slsw(wkflyn,wklyn,iplane)
     1                                + varwat/varthd
            nwfrzz(wklyn,iplane) = nwfrzz(wklyn,iplane) - varwat
            frozen(wklyn,iplane) = frozen(wklyn,iplane) - decr
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

      subroutine frostN(hour)
c
c     +++PURPOSE+++
c     This is the main driver for the frost subroutines.  Based
c     on the surface, climate and soil conditions it decides
c     which subroutines are to be called.  It considers energy
c     flow between the frozen layers, snow depth, melting or
c     freezing and also calls the infiltration capacity calculations.
c
c     --------------------------------------------------------------------
c     Modified by S. Dun, Feb 20, 2008 to use a finer resolution in soil depth
c
c       Each soil layer was divided into 10 thin layers for frost simulation.
c       In the tillage zone (top two 10cm thick soil layers) the fine layer thickness is 1 cm.
c     While in the untilled zone (20cm thick each layer) the fine layer thickness is 2cm.
c     If a fine layer is partially frozen, frost depth is either on top or on bottom 
c     depending on if it is in a freezing or thawing process 
c     which is recorded as flag index 3 or 4 in fgfrst(10,mxnsl,mxplan). 
c     ---------------------------------------------------------------------
c
      
c     Authors(s):  John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 04/05/93
c
c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c
c     Recoded by Charles R. Meyer  Winter of '96
c     Changes:   (1) Eliminated calls to AVOID2, CAQWET, and CAQDRY.
c                    Incorporated their code into FROST.
c                (2) Added greatly modified diagram from CWINT.INC below
c                    and added many comments related to it.
c                (3) Added check to see if dummy parameters have changed
c                    since last call to FROST.  If not a lot of work can
c                    be avoided.
c     Incorporated into WEPP code by Dennis Flanagan,  3/97
c
c  *********************************************************************
c  *  From CWINT.INC:                                                  *
c  *********************************************************************
c  *
c  *
c  *   The Frozen Layered System:                                 WEPP
c  *   --------------------------                               Variable
c  *                                                              Name:
c  *
c  *     *********************************   - Snow depth         snodpt
c  *
c  *
c  *     _________________________________   - Residue depth
c  *     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   - Residue
c  *     ================================= <-- Surface
c  *     (((((((((((((((((((((((((((((((((
c  *     )))) Soil thawed from above )))))
c  *     --------------------------------- <-- Thaw depth         thdp
c  *     //// Soil frozen from above /////
c  *     --------------------------------- <-- Top Frost depth    tfrdp
c  *     )))) Soil thawed from above )))))
c  *     --------------------------------- <-- Top Thaw depth     tthawd
c  *     \\\\\\\\\\\\ Frozen  \\\\\\\\\\\\
c  *     \\\\\\\\\\\\  Soil  \\\\\\\\\\\\\
c  *     --------------------------------- <-- Frost depth        frdp <= 1.0m
c  *     (((((((((( Stable Temp ((((((((((    (thawed from below,
c  *     ))))) Always Above Freezing )))))     or entire profile
c  *                                           has thawed.)
c  *
c  *   (Note that Top Thaw depth and Top Freeze depth can swap positions.)
c  *
c  *
c  * NOTES:
c  * ======
c  *
c  *   Tilled Layer -      Depth from soil surface to the primary tillage
c  *   ------------        depth for the season previous.  (tilld = 0.2m)
c  *
c  *   Untilled Layer -    Depth from bottom of the tilled layer to the
c  *   --------------      stable soil depth.
c  *
c  *   Stable Soil Depth - Depth @ which soil temperature is stable...
c  *   -----------------   this model assumes this depth is 1 meter below
c  *                       the lowest 0-degree isotherm depth.  If no
c  *                       frost is present, this depth is left at 1 m.
c  *
c  *********************************************************************
c
c
c     +++ARGUMENT DECLARATIONS+++
      integer hour
c
c     +++ARGUMENT DEFINITIONS+++
c     hour   - The hour of the day that we are calculating.
c
c     +++PARAMETERS+++
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'

      include 'pmxtls.inc'
      include 'pmxtil.inc'
c
c     +++COMMON BLOCKS+++
      include  'cclim.inc'
c     read:  tave,vwind,hradmj,hrtemp,rpoth,rad
c
      include  'cwint.inc'
c     read:  snodpt(iplane),tfrdp(mxplan),tthawd(mxplan),frdp(mxplan),
c              thdp(mxplan),densg(mxplan)
c
c     include  'ccrpout.inc'
c     read:  bd
c
      include  'cwater.inc'
c     read:  solthk(mxnsl,mxplan),fctill(mxplan),fcutil(mxplan)
c
      include  'cstruc.inc'
c     read:  iplane
c
      include 'cupdate.inc'
c     for bedug
      include 'cflgfs.inc'
c     fine layer for frost simulation
      include 'ctcurv.inc'
c     read: annual air temperature curve coefficients
      include 'ccons.inc'
c     read: bdcons, bulk density
c
      include 'cpfrst.inc'
c
c     +++LOCAL VARIABLES+++
c
      integer i,j,jstart,jend,LN1mbf,FLN1mb,layerN,flyerN,tpbtfg,
     1   istart
c
      real    kufzfl,kufz,ksnow,kres,tmpbl,tmpdp,dmping,tmpvr1,tmpvr2,
     1        dpfsfl,vardp,factor,sp
c ----- Saved Variables used to avoid unnecessary recalculation:
c
c     +++LOCAL DEFINITIONS+++
c     qdry   - Heat flow from stable soil temperature to the bottom
c               of the frost layer (W/m^2).
c     qwet   - Heat flow required to freeze H2O in the soil (W/m^2).
c     qhtout - Heat flow across the four frozen layers (W/m^2).
c     kufz - Thermal conductivity of unfrozen soil 1 meter blow frost layer (W/m C).
c     ktopf - Thermal conductivity of layers above first frost layer (W/m C).
c     kufzfl - Thermal conductivity of a fine layer unfrozen soil (W/m C).
c     ksnow  - Thermal conductivity of the snow pack (W/m C).
c     kftill - Thermal conductivity of frozen tilled soil (W/m C).
c     kfutil - Thermal conductivity of frozen untilled soil (W/m C).
c     kres   - Thermal conductivity of residue layer (W/m C).
c
c     tmpbl - average temperature 1 meter below frozen front
c     tmpdp - depth for the estimated temperature
c     dmping - dmping depth for yearly cahnge in soil
c     dpfsfl - depth of the first finer layer
c
c     LNfrst - the soil layer where the bottom of the frost is
c     LN1mbf - the soil layer 1 meter blow the forst layer
c     FLNfrs - the fine layer number in LNfrst where frost bottom is
c     FLN1mb - the fine layer number 1 meter blow the forst layer
c     FLNbtm - fine layer number of the bottom of know soil profile.
c     layerN - soil layer number
c     flyerN - finer soil layer number
c     tpbtfg - a flag for  start point or ending point is required,
c              0 for starting point and 1 for ending point.
c     vardp  - depth variable
c
c     frzflg - a flg to indicate freezing or thawing processes
c
c     + + + DATA INITIALIZATIONS + + +
c
cd    Modified by S. Dun, April 19, 2007 
c     The unit of latent heat of fusion of ice needs to be in J/m3
c     to make the unit of heat flow in W/m2
cd      data   lhfh2o/9.3027e04/
      real lhfh2o
      data   lhfh2o/3.35e08/
cd      End modifying
c
c     +++END SPECIFICATIONS+++

c     initializing variables...
        kftill = 1.75
        kfutil = 2.1
c       kres = 0.168 
cd       kres = 0.0232
        kres = 0.05
        kres = kres*kresf
c
c      added ability to use varying thicknesses for fine soil
c      layers. This section defines number of fine layers in each big soil layer
c      for last layer adjust for fewer fine layers. 
c      jrf 7-31-2008
       do 5 i = 1, nsl(iplane)
          if (i.eq.nsl(iplane)) then
c            last layer, thickness for each fine layer, since
c            bottom wepp layers are 200mm 1/10 -> 20mm
c            nfine setting below should be compatible
c            nfine(i)*sp -> wepp big layer thickness
             if (i.gt.2) then
                sp = 200./fineBot
             else
                sp = 100./fineTop
             endif
             nfine(i) = int((dg(i,iplane)*1000)/sp)  
c            if there is any left over add another fine layer             
             if ((nfine(i)*sp).ne.int(dg(i,iplane)*1000)) then
                 nfine(i) = nfine(i) + 1
             endif
          else 
c            this controls how many fine layers are in each big wepp soil
c            layer. The top two layers are handled as one class and the 
c            remaining soil layers handled separetly.                          
             if (i.lt.3) then
                nfine(i) = fineTop
             else
                nfine(i) = fineBot
             endif
          endif
5      continue
       FLNbtm = nfine(nsl(iplane))

c     Find the fine layer number for the bottom of the soil profile (ending point). 
c     The fine layer numbers are used in estimating thermal conductivity 
c     and water residtribution of the unfrozen zone.
cd      if (fsdfg(iplane).eq. 0) then
c          vardp = solthk(nsl(iplane),iplane)
c          call locate(vardp,layern,flyern,1)
c          FLNbtm = flyern        
c      endif
      
c      initiate the soil water content and ice content of finer layers
        if ((frdp(iplane).lt. 0.001).and.
     1                       (slsic(1,1,iplane).lt.0.00001)) then
c       No frost 
            frzflg = 0
            fgthwd(iplane) = 0
c
            do 10 i = 1, nsl(iplane)
                 do 10 j = 1, nfine(i)
                   fgfrst(j,i,iplane) = 0
                   slfsd(j,i,iplane) = 0.0
                   slsw(j,i,iplane) = soilw(i,iplane)/dg(i,iplane)
                   yst(i,iplane) = st(i,iplane)
                   slsic(j,i,iplane) = 0.0                 
10          continue
       elseif(fsdfg(iplane).eq. 0) then
c       initial frost is not zero
            frzflg = 1
            fgthwd(iplane) = 0
c
c           Find the fine layer number for the bottom of initiate frozen layer. 
            vardp = frdp(iplane)
            call locate(vardp,layern,flyern,1)
c        
            do 11 i = 1, layern            
                if (i .eq. layern) then 
                   jend = flyern
                else
                   jend = nfine(i)
                endif
                do 11 j = 1, jend
                   fgfrst(j,i,iplane) = 1
                   slfsd(j,i,iplane) = dg(i,iplane )/nfine(i)
                   slsw(j,i,iplane) = (soilw(i,iplane) - st(i,iplane))
     1                                /dg(i,iplane)
                   yst(i,iplane) = st(i,iplane)
                   slsic(j,i,iplane) = st(i,iplane)
11          continue
c
            if (flyern.eq.nfine(i)) then
                  istart = layern + 1
                  jstart = 1
            else 
                  istart = layern
                  jstart = flyern + 1
            endif
c           
            do 12 i = istart, nsl(iplane)
c
                do 12 j = jstart, nfine(i)
                   fgfrst(j,i,iplane) = 0
                   slfsd(j,i,iplane) = 0.0
                   slsw(j,i,iplane) = soilw(i,iplane)/dg(i,iplane)
                   yst(i,iplane) = st(i,iplane)
                   slsic(j,i,iplane) = 0.0
12          continue
        elseif (hour.eq.1) then
            call frwatc(1)
c           flag = 1 for soil water content from water balnce routine to frost routine
        endif
        
        fsdfg(iplane) = 1
c
c     initialize the water distribution time arry at the begining of an hour
c
      do 15 i = 1, nsl(iplane)
      do 16 j = 1, nfine(i)
            sltime(j,i,iplane) = 0.
16    continue            
15    continue
c
c       Depth of middle of the first finer layer
        dpfsfl = dg(1,iplane )/nfine(1)/2.
c      
c     Find the start location of the unfrozen zone
        vardp = frdp(iplane)
        call locate(vardp,layern,flyern,0)
        LNfrst = layern
        FLNfrs = flyern
c
c
c     *************************************************************
c     ***  BALANCE the HEAT FLOW from the unfrozen soil layers  ***
c     ***  to the frozen layers.  Note that the temperature is  ***
c     ***  normally assumed constant (at 7 degrees Celsius) at  ***
c     ***  depths 1 meter below the freezing front.             ***
c     *************************************************************
c
cd      S. Dun, Jan 05, 2008, The assumption of 7 C has been ahanged.
c     a temperature value is estimated using annual air temperature sin curve 
c     and an assumption of yearly soil damping depth 2 meter 
c     Flowing soil temperature profile equation in G. Campbell "Environmental Physics"
c
c      Estimate temperature 1 meter below the frost layer.
c      If estimated temperature is below 0C, then the heat from blow is 0
      dmping = 2.0
      tmpdp = frdp(iplane) + 1.0
      tmpbl = YavgT + YampT * exp(-tmpdp/dmping)
     1        *sin(2*3.14/365. *(sdate-YpshfT)- tmpdp/dmping)
c
c     *********************************************************** 
c     *** Heat conducted from the warm soil blow frost layer  ***
c     *********************************************************** 
c ------ Calculate QDRY, heat flow from [the dry layers] beneath
c         the frost layer.
c
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
          do 20 i = LNfrst, LN1mbf
c            
             if (i .eq. LNfrst) then
                   jstart = FLNfrs
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
          do 30 j = jstart, jend
c               Calculate Thermal conductivity of unfrozen soil
                tmpvr1 = 0.5096 + 7.4493 * slsw(j,i,iplane) 
     1                   - 8.7484 * slsw(j,i,iplane) ** 2
                kufzfl = tmpvr1 * (0.0014139*bdcons(i,iplane) - 1.0588)
c
c               The value 10 is for 10 finer layers in each soil layer
                if (kufzfl.gt.0) then
                    tmpvr2 = tmpvr2 + dg(i,iplane)/nfine(i)/kufzfl
                endif
c
30        continue
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
c     **************************
c     *** Heat flux from Top ***
c     ************************** 
c
c     Surface temperature would not be greater than 0C if there is snow on ground
      if ((surtmp(hour) .gt. 0.01) .and. snodpt(iplane).gt. 0.001) then
          surtmp(hour) = 0.
      endif              
c 
c         
      if (abs(surtmp(hour)) .lt. 0.01) then
c         Temperature gradient is small.
          qhtout = 0.0
      else
c         Harmonic mean of thermal conductivity above the freezing front
          tmpvr2 = 0.
c
c         Considering thermal conductivity of the snow pack
c         model from Sturm et al., 1997
          if ((snodpt(iplane) .gt. 0.001).and. 
     1                  (densg(iplane).gt.0.)) then
c                ksnow = 0.0000029694 * densg(iplane) ** 2
c
             if (densg(iplane).lt. 156) then
                ksnow = 0.023 + 0.234 * (densg(iplane)/1000.)
             else
                ksnow = 0.138 - 1.01*(densg(iplane)/1000.)
     1                  + 3.233* (densg(iplane)/1000.)** 2
             endif
c
c     It seems themal conductivity estimated here is too small for frost simulation
c      the adjustment factor is from calibration of Morris, MN and Pullman, WA
c
             ksnow = ksnow * ksnowf
c
             tmpvr2 = tmpvr2 + snodpt(iplane)/ksnow
          endif
c         Considering thermal conductivity of the residue
          if (resdep(iplane) .gt. 0.001) then
             tmpvr2 = tmpvr2 + resdep(iplane)/kres
          endif
c
c         Denominator for thermal conductivity from snow and residue
          dmfrsn = tmpvr2
c
c         Considering thermal conductivity of the top frost layer
c         Calculate heat flow through the top layers to extend the top frost or thawd depth.
c
          if (surtmp(hour).lt. 0.0) then
c         When surface temperature below freezing....
c
             if (thdp(iplane).gt. 0.001) then
c            There is a thawing layer on top using the middle of the first fine layer
                 tmpvr1 = 0.
c         
             elseif(tfrdp(iplane).gt.0.001) then
c            The frost is sandwitch layers.the surface layer is a frost layer 
                 tmpvr1 = tfrdp(iplane)
             else
c            No frost sandwitch, using frost depth directly
                 tmpvr1 = frdp(iplane)
             endif
          else
c         When surface temperature above 0C
             if (thdp(iplane).gt. 0.001) then
c            There is a thawing layer on top using the middle of the first fine layer
                 tmpvr1 = thdp(iplane)
             else
                 tmpvr1 = 0.
             endif
          endif
c
c         Set minimum heat conduction distance the middle of the first fine layer
          if (tmpvr1.lt.dpfsfl) tmpvr1 = dpfsfl
c
          if (tmpvr1.le.tilld(iplane)) then
               tmpvr2 = tmpvr2 + tmpvr1/kftill
          else
               tmpvr2 = tmpvr2 + tilld(iplane)/kftill
     1                + (tmpvr1 - tilld(iplane))/kfutil
          endif
c
c          Thermal conductivity above first frost layer             
c               ktopf = (snodpt(iplane) + resdep(iplane) +  tmpvr1) /tmpvr2
c
c         Thermal gradient of the layer: 
c               surtmp(hour)/ (snodpt(iplane) + resdep(iplane) +  tmpvr1)
c
c          Heat flux from above
           qhtout = surtmp(hour) / tmpvr2
c          record the denominator of qhtout
           qoutdm = tmpvr2
      endif
c           
c
c     *************************************
c     *** Freezing or Thawing process ? ***
c     *************************************
c
      if ((tthawd(iplane) .gt. 0.001).or.(thdp(iplane).gt.0.001)) then
c      Sandwitch style frost formed or will be formed
          if (qhtout .lt. 0.0) then
c              top freezing and bottom thawing
               frzflg = 2
          elseif (qhtout .eq. 0.0) then
               if (qdry .eq. 0.0) then
                  frzflg = 0
               else
                  frzflg = 4
               endif
          else
c              thawing both ends
               frzflg = 3
          endif
c
      elseif (frdp(iplane).gt. 0.001) then
c     sigle layer frost
          if (qhtout .le. 0.0) then
              if((qdry + qhtout).lt.0.0) then
c                bottom freezing
                 frzflg = 1
              elseif ((qdry + qhtout).eq.0.0) then
c                balanced
                 frzflg = 0
              else
c                bottom thawing
                 frzflg = 4
              endif
          else
c             thawing both ends
              frzflg = 3
          endif
      else
c     no frost layer
          if((qdry + qhtout).lt.0.0) then
c              bottom freezing
               frzflg = 1
          else
c              balanced
               frzflg = 0
          endif
      endif
c
c     **************************************************
c
      if (frzflg .eq. 1) then
c     bottom freezing
          call frzng(hour)
          call watdst(0.0, 3600., 2)
c
      elseif (frzflg .eq. 2) then
c      top freezing & bottom thawing
          call frzng(hour)
          if (qdry .gt. 0.)  call mltbtm(hour)
         call watdst(0.0, 3600., 2)
c
      elseif (frzflg .eq. 3) then
c     both ends thawing
          call mlttp(hour)
          if ((qdry .gt. 0.).and.(fgthwd(iplane).ne.1)) 
     1        call mltbtm(hour)
          call watdst(0.0, 3600., 0)
c
      elseif (frzflg .eq. 4) then
c     bottom thawing
          call mltbtm(hour)
          call watdst(0.0, 3600., 0)
c
      endif
c
c
      if ((hour.eq.24) .or. (fgthwd(iplane).eq.1)) call frwatc(0)
c     flag = 0 for soil water content from frost routine to other routine
c
c     Check to make sure that computed frost/thaw depths do not
c     exceed soil thickness.   dcf  3/17/97
c
      if(frdp(iplane).gt.solthk(nsl(iplane),iplane))
     1  frdp(iplane)=solthk(nsl(iplane),iplane)
      if(thdp(iplane).gt.solthk(nsl(iplane),iplane))then
        thdp(iplane) = 0.0
        frdp(iplane) = 0.0
      endif
c
      if ((frdp(iplane).eq.0.0).and.(fgfrst(1,1,iplane).ne.0)) then
         frdp(iplane) = 0.005
      endif
c
c     Added by S. Dun, June 16, 2007, for Debuging
cd      Write(62, 1000) sdate, hour, year
1000  Format(1x, 3I6)
cd      do 35 i = 1, nsl(iplane) 
cd      write(62,1500) (slsw(j,i,iplane),j=1,10)
1500  format(1x, 10E12.3)
cd35    continue
c     end adding
c
      return
      end

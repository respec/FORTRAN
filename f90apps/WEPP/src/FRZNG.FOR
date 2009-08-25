      subroutine frzng(hour)
c
c     +++PURPOSE+++
c     This function is responsible for extending the frost depth
c
c     The purpose of this program is to extend the frost depth with
c     excess heat flow when the heat flow through the frozen layer
c     system is greater than heat flow from the thermal conductivity
c     of the unfrozen soil.  The excess heat flow is balanced by the
c     heat of fusion released by freezing water.
c
c     Author(s):  Shuhui Dun, WSU
c     Date: 02/22/2008
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
      include 'csaxp.inc'
c     Saxton and Ralwls model coefficients
c
      include 'cwater.inc'
c     read: dg(i,iplane)
c
      include 'ccons.inc'
c     read: bdcons, bulk density
c
c     +++LOCAL VARIABLES+++
c
      integer  layerN,flyerN,tpbtfg,lyblwk,flblwk,varfg,
     1         fgfzft,wklyn,wkflyn,i,j,varfst,varstp
      real     htreq,ceh2o,lhfh2o,incr,frzeng,fztime,flfzt,
     1         frzftp,wtPkpa,wtpm, kunsat,qwater,frzdp,ofrzdp
      real     smoist,sdepth,vardp,varsm,varthk,varsmc,vardm,
     1         varwtp, varkus,mdfzdp,eratio,tmpvr1,tmpvr2,
     1         kufzfl,oslfsd, varegt,varul,nwfrzt,
     1         qouttp,pfrzw

c
c     +++LOCAL DEFINITIONS+++
c     htreq  - Heat (energy), required to freeze soil of current finer layer (J/m^2).
c     ceh2o  - Coefficient of expansion for water (unitless).
c     lhfh2o - Latent heat of fusion of water (J/m^3).
c     incr   - Depth that the frost formed in a fine layer(m).
c     frzeng - engery flux to freeze the soil
c     flfzt - time needed to freeze a fine layer
c     fztime - total freezing time used
c     eratio - ratio to the requied energy for freezing soil current layer
c
c     frzftp - water potential at frozen front in meter
c     wtPkps - water potential of a soil layer in kpa.
c     wtpm   - water potential in meter. 
c     kunsat - unsaturated hydraulic conductivity m/s.
c     qwater - water flux
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
c     wklyn : soil layer number where freezing or thawing front is (working)
c     wkflyn: finer layer number of the working position
c     lyblwk - the soil layer of the finer layer right below the working finer layer
c     flblwk - the finer soil layer below the working layer
c
c     frzdp  - depth of the frozen front
c     ofrzdp  - depth of the frozen front before update
c
c     vardp  - depth variable
c     varsm  - soil moisture variable
c     varfg  - frost flag varaible
c     varthk - thickness variable
c     varsmc - variable for maximum water flow rate an adjecent layer can supply
c
c     mdfzdp - frost thickness between current frozen front and next unfrozen layer
c     kufzfl - Thermal conductivity of a fine layer unfrozen soil (W/m C).
c
c     +++DATA INITIALIZATIONS+++
c     Phase change expention coefficient, water to ice
      data   ceh2o/1.1/
c     Latent heat of fusion of ice in J/m3
      data   lhfh2o/3.35e08/

c     +++END SPECIFICATIONS+++
c
c
c     Do loop
      fztime = 0.
      flfzt = 0.
      incr = 0
      vardm = 0.0
c
c     When there is thawed water ponding on soil surface
      if(watpdg(iplane).gt.1e-5) then
         vardm = dg(1,iplane)/nfine(1)/kftill
         qouttp = surtmp(hour) / (dmfrsn + vardm)
         htreq = lhfh2o * watpdg(iplane)
c      
         if (htreq .gt. qouttp*3600) then
c           water could be frozen by the energy
            pfrzw = qouttp*3600 / lhfh2o
            fztime = 3600          
         
          else
             fztime = htreq/qouttp
             pfrzw = watpdg(iplane)             
           endif
           slsic(1,1,iplane) = slsic(1,1,iplane) + pfrzw
           watpdg(iplane) = watpdg(iplane)- pfrzw
      endif
c
c     Starting point of the frost extention
      if (frzflg.eq.1) then
c     bottom freezing
         frzdp = frdp(iplane)
         if ((frzdp-solthk(nsl(iplane),iplane)).gt.-0.0001) return
         wklyn = LNfrst
         wkflyn = FLNfrs
      elseif (frzflg.eq.2) then
c     top freezing and bottom thawing
         if (thdp(iplane) .gt. 0.001) then
            frzdp = dg(1,iplane)/nfine(1)/2.
            wklyn = 1
            wkflyn = 1
         else
            frzdp = tfrdp(iplane)
            call locate(frzdp,layern,flyern,0)
             wklyn = layern
             wkflyn = flyern
         endif
      endif
c
c     Do loop
c
c     To first freeze water infiltrated in the frost zone abovove the frozen front
cd      if ((wklyn.ne.1) .or. (wkflyn.ne.1)) then
        if(frdp(iplane).lt. 1.0e-5) then
c
        do 10 i = 1, wklyn
           if (i .eq.wklyn) then
              varstp = wkflyn
           else
              varstp = nfine(i)
           endif
c
           do 20 j = 1, varstp
           if (nwfrzz(i, iplane) .gt. 1e-5) then
              varul = ul(i,iplane)/dg(i,iplane)*slfsd(j,i, iplane)
c
              if (slsic(j, i, iplane) .lt. varul) then
                   varegt = 3600. - fztime
c   
                   call frznw(i,j,varegt,nwfrzt,hour)
c
                   fztime = fztime + nwfrzt
                   if(fztime.ge.3600.) return
              endif
           endif 
20         continue
c
10      continue
c
      endif
c
c     a flag for the first time in the following loop
      varfst = 0
c     initate the varible for the thickness of the sandwitched frozen layer
      mdfzdp = 0.
c
c     There are 3600 seconds in a hour
c    ***************************************************
      do while (fztime.lt.3600.)
c     Loop till no more energy availabe to freeze the soil
c
c     Heat flux from soil surface needs to be re-estimated
c     when frozen front moved down one or many fine layers. 
      if (varfst .ne. 0) then
c
          wklyn = lyblwk
          wkflyn = flblwk
c 
c         Check if current layer is frozen
30        varfg = fgfrst(wkflyn, wklyn, iplane)
c
          if((varfg .ne. 0).and.(nwfrzz(wklyn,iplane).gt. 1e-5)) then
c             freeze the infiltrated water in the frozen zone
              varul = ul(wklyn,iplane)/dg(wklyn,iplane)
     1                            *slfsd(wkflyn, wklyn, iplane)
              if (slsic(wkflyn, wklyn, iplane) .lt. varul) then
                  varegt = 3600. - fztime 
                  call frznw(wklyn,wkflyn,varegt,nwfrzt,hour)
                  fztime = fztime + nwfrzt
                  if(fztime.ge.3600.) return
              endif
          endif
c
          if(varfg .eq. 1) then
c         a complete frozen
c
               mdfzdp = mdfzdp + dg(wklyn, iplane)/nfine(wklyn)
c
               wkflyn = wkflyn + 1
              if (wkflyn .gt. nfine(wklyn)) then
                  wklyn = wklyn + 1
                  wkflyn = 1
              endif
c
              goto 30
c
           elseif (varfg .eq. 2) then
c          partialy frozen, frost at top
              mdfzdp = mdfzdp + slfsd(wkflyn, wklyn, iplane)
c
           endif
c
          ofrzdp = frzdp
          frzdp = frzdp + incr + mdfzdp
          if ((frzdp-solthk(nsl(iplane),iplane)).gt.-0.0001) return
c                                
          if (ofrzdp.le.tilld(iplane)) then
c         freezing front was in the tillage layer
              if (frzdp.le.tilld(iplane)) then
c             freezing front is still in the tillage layer
                   qoutdm = qoutdm + (incr + mdfzdp)/kftill
              else
c             freezing front moved below the tillage layer
                   vardp = (tilld(iplane) - frzdp)
                   qoutdm = qoutdm + vardp/kftill
     1                 + (incr + mdfzdp - vardp)/kfutil
              endif
          else
c         freezing front was in the untilled zone
              qoutdm = qoutdm + (incr + mdfzdp)/kfutil
          endif
c
      endif
c
c     Thermal conductance of dry surface soil is important
      if ((wkflyn .eq. 1).and.(wklyn.eq.1)) then
          tmpvr1 = slsw(wkflyn, wklyn, iplane)
          tmpvr2 = 0.5096 + 7.4493 * tmpvr1 - 8.7484 * tmpvr1 ** 2
          kufzfl = tmpvr1 * (0.0014139*bdcons(wklyn,iplane) - 1.0588)
c
c         The value 10 is for 10 finer layers in each soil layer
c         the value of 2 is for half thickness of a fine layer
          if (kufzfl.gt.0)
     1       vardm = dg(wklyn,iplane)/nfine(wklyn)/4./kufzfl
      else
          vardm = 0.0
      endif   
c
c     Thermal conductivity above first frost layer             
c         ktopf = thickness /qoutdm
c
c      Thermal gradient of the layer: 
c         surtmp(hour)/ (thickness
c
c      Heat flux from above
       qhtout = surtmp(hour) / (qoutdm + vardm)
c
       if ((qhtout+qdry).gt.0.) return
c
c
c     ************************************************
c     *** Water migration caused by freezing front ***
c     ************************************************
c
c       "When water in the soil freezes, the water in the unfrozen soil
c     below is depleted, therefore the hydraulic conductivity of the
c     soil is depleted" George Benoit, July 1992.
c
c    Layer numbers of the fine layer below the freezing layer
      flblwk = wkflyn + 1
      if (flblwk .gt. nfine(wklyn)) then
          lyblwk = wklyn + 1
          flblwk = 1
      else
          lyblwk = wklyn
      endif
c
c     Check if the soil below is unfrozen
      if (lyblwk.gt.nsl(iplane)) then
        varfg = 1
      else
        varfg = fgfrst(flblwk, lyblwk, iplane)
      endif
c
c     qwet condition 
c     ---------------------------------------------------------
      if (varfg .ne. 0) then
c     The layer below is frozen, no soil water migration
         qwet = 0.0
         qwater = 0.0
c
      else
c     The layer below is not frozen on top
c
c ----- Calculate QWET, heat of fusion released by freezing water that
c       migrates to, the freezing front.
c ----- L * K w [P / Z uf] part of eqn 3.8.4.
c
cd    Based on the discussion with Kunio Watanabe, Associate Professor, Mie University, Japan
c      in January 2008 when he was visiting WSU.
c      Soil freezing depression point usually is in a range of -0.01C to -0.25C.
c      Using generalized form of Causias-Capeyron equation, the pressure potentional
c      at frozen front should be in the range of -20 m to - 160 m.
c      Here, we select to use -100 m.
c      dP/dT = L/(T*deltaV), L=3.34E+5 J/kg, T=273K, deltaV=-9.05E-5m3/kg, dP/dT=-13.1MP/C          
c
cd      frzftp = -100
      frzftp = 0.0
c
c     Estimate the water potential of the soil layer below the frozen front
c     using Saxton and Rawls, 2006
c     
      varsm = slsw(flblwk, lyblwk, iplane)
c
      call saxfun(lyblwk,varsm,varwtp, varkus)
      wtpm = varwtp
      kunsat = varkus

c     Heat release rate by freezing water migrated to the frozen front
c     Because we are using the maximum hydraulic gradient to estimate 
c     soil water migration rate. Therefore we divided by 2. for assuming 
c     migration rate slows dowm due to water depletion in the adjecent layer.
c
      if (frzftp . lt. wtpm) then
          qwater = kunsat *(wtpm - frzftp)/ dg(lyblwk,iplane)/2.
c         Hopefully, it would not use up all soil water in the next layer.
c         Limt the water flux rate to the adjecent layer can supply in an hour
c         if soil water could depelete to its wilting point soil water content.
          varsmc = (varsm - thetdr(lyblwk,iplane))
     1             *(dg(lyblwk,iplane)/nfine(lyblwk)) / 3600
c
          if (varsmc .lt. 0.0) then
              varsmc = 0.0
          endif
c
          if(qwater.gt.varsmc) then 
                 qwater = varsmc
          endif
c  
          qwet=lhfh2o * qwater

      else
          qwet = 0.0
          qwater = 0.0
      endif
c
      endif
c     End qwet condition
c     -----------------------------------------------
c
c     Freezing energy
      if (frzflg.eq.1) then
c     bottom freezing
         frzeng = qdry + qhtout
      elseif (frzflg.eq.2) then
c     top freezing and bottom thawing
         frzeng = qhtout
      endif
c     -----------------------------------------------
c
c     unfrozen layer thickness in a fine layer 
      varthk = dg(wklyn,iplane)/nfine(wklyn)   
c
      if ((frzeng + qwet).ge.0.0) then
c     Engery would first be used to freeze the migration soil water
c
c         limit water flow rate to freezing rate 
          qwet = -frzeng    
          qwater = qwet/lhfh2o
          incr = 0.0
c         no more energy
          flfzt = 3600.-fztime
          eratio = 0.0
          amtfrz(iplane) = amtfrz(iplane) + qwater *flfzt
c          Write(6,*) 'frost heave', sdate, hour
c
      else
c     Engery is avaiable to freeze the water in current layer
c     
           varfg = fgfrst(wkflyn, wklyn, iplane)
c
           if (varfg.ne.0) then
c          the layer is partially frozen
                varthk = varthk - slfsd(wkflyn,wklyn,iplane)
           endif
c          The unit of htreq is J/m2                  
           htreq = lhfh2o * slsw(wkflyn,wklyn,iplane) * varthk
c
c     

c          The calculation frzeng*3600 convert energy flux frzeng in W/m2 to J/m2.
c          There are 3600 s in an hour.
c
           if (((frzeng + qwet)*(3600.-fztime) + htreq) .gt. 0.0) then
c          The energy is only enough to freeze current layer
               eratio = -(frzeng + qwet) * (3600. - fztime)/htreq
               incr = eratio * varthk
c              no more energy
               flfzt = 3600. - fztime
           else          
               flfzt = - htreq/(frzeng + qwet)
c
               if(flfzt.lt. 1.0) flfzt =1.0
c              in order to go for next layer, a value of 1 second is assigned for freezing time
c              when the unfrozen thickness is too small to keep the program go to next layer
c
               incr = varthk
               eratio = 1.             
           endif
      endif
c     Before updating fine layer frost varaibles,
c     initate the variable for the thickness of the sandwitched frozen layer
      varfg = fgfrst(wkflyn, wklyn, iplane)
      if (varfg.eq.3) then
          mdfzdp = slfsd(wkflyn, wklyn, iplane)
      else
          mdfzdp = 0.
      endif 
c
c     Update fine layer frost variables
      if (incr .lt. 0.001) incr = 0.0
      if ((varthk - incr).lt.0.001) incr = varthk
c
      if (incr .eq. varthk) then
c     Whole layer is frozen            
           fgfrst(wkflyn, wklyn, iplane) = 1
c
      elseif (incr .eq. 0.0) then 
           if(qwater* flfzt .gt. 0.0) then
c          Frost heave eratio = 0
c          Otherwise enery is not enough to freeze more than 1mm soil
c          then the frost flag would not change.
               fgfrst(wkflyn, wklyn, iplane) = 2
           endif
      else
c          Partially frozen 
           fgfrst(wkflyn, wklyn, iplane) = 2
      endif
c 
c     frost depth in a fine layer
      oslfsd = slfsd(wkflyn, wklyn, iplane)
      slfsd(wkflyn, wklyn, iplane) = slfsd(wkflyn,wklyn,iplane) + incr
c      
      if (slfsd(wkflyn, wklyn, iplane) .gt. 
     1        (dg(wklyn,iplane)/nfine(wklyn))) then
            incr = dg(wklyn,iplane)/nfine(wklyn) - oslfsd
            slfsd(wkflyn, wklyn, iplane) = dg(wklyn,iplane)/nfine(wklyn)
      endif 
c
c     the amount of water in ice form (m)
      slsic(wkflyn, wklyn, iplane) = slsic(wkflyn, wklyn, iplane)
     1          + qwater* flfzt + slsw(wkflyn,wklyn,iplane) * incr
c
c     liquid soil water content in current fine layer would not change.
c
c     Water redistribution around frozen front
      if (qwet .gt. 0.0) then
c         fgfzft - flag for what type of water redistribution,
c         0 for no frozen front,
c         1 for around frozen front, 
c         2 for unfrozen layers with frozen front in the soil profile
          call watdst(qwater, flfzt, 1)
      endif
c
c     Total freezing time
      fztime = fztime + flfzt
c
c     Update the flag for first time in the loop
      varfst = 1
      if (lyblwk.gt.nsl(iplane)) return
c      
      Enddo
c     End of the do loop
c     *************************************************************
c
cd        write(62, 1000) sdate, hour, year, qdy, qhtout,qwet  
1000    format(1x, 3i6, 3e12.2)
c
      return
      end

      subroutine sndrft(irtype,hour,driftf,driftg)
c
c     +++PURPOSE+++
c     This routine calculates the amount of snow drifting taking place.
c     It utilizes information on wind speed and aspect as well as hill
c     slope aspect and surface roughness from tillage activity and
c     crop cover.
c
c     Authors(s): Cully Hession and Bruce Lucord, USDA-ARS-NCSRL
c                 Revised by John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 03/31/93

c     Verified but not tested, Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c
c     +++ARGUMENT DECLARATIONS+++
      real driftf,driftg
      integer irtype,hour
c
c     +++ARGUMENT DEFINITIONS+++
c     driftf - The calculated amount of falling snow drift (m).
c     driftg - The calculated amount of ground drift occurring (m).
c     irtype - The index referring to the crop residue type.
c     hour   - Hour of the day...used for hourly snowfall amount.
c
c
c     +++PARAMETERS+++
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxslp.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
c
c     +++COMMON BLOCKS+++
      include  'cclim.inc'
c       read:  vwind,wind
      include  'ccover.inc'
c       read:  canhgt,cancov
      include  'ccrpout.inc'
c       read:  rrc
      include  'ccrpvr1.inc'
c       read:  rmagt
      include  'ccrpvr5.inc'
c       read:  pltsp,diam
      include  'cdist.inc'
c       read:  slplen
      include  'cslpopt.inc'
c       read:  xslp
      include  'cperen.inc'
c       read:  pop
      include  'crinpt3.inc'
c       read:  basden
      include  'cslope2.inc'
c       read:  avgslp(mxplan)
      include  'cstruc.inc'
c       read:  iplane,nplane
      include  'cwint.inc'
c       read:  azm(iplane or 1),snodpt,drift,snow,densg
      include  'cdecvar.inc'
c       read:  cuthgt(ntype)
c
c     +++LOCAL VARIABLES+++
      save
      real    snodp,height,ppop,rmasso,rmasst,rough,
     1  length,vfrict,stor,trapef,vthf,tranf,vthg,
     2  trang,absazm,scourf,inclf,perd,driftp
c
c     +++LOCAL DEFINITIONS+++
c     snodp  - Adjusted value of snow depth (m).
c     height - Height of the canopy which is above the snow layer (m).
c     ppop   - Represents the plant population (#Plants/ha).
c     rmasso - Standing residue mass before tillage (Kg/ha).
c     rmasst - Standing residue mass after tillage (Kg/ha).
c     rough  - Surface roughness of the top layer of the system (m).
c     length - Length across the cell, parallel to the wind (m).
c     vfrict - Velocity of friction at the snow surface (m/sec).
c     stor   - Storage capacity of the residue layer (m).
c     trapef - Trapping efficiency of the surface (%/100).
c     vthf   - Threshold velocity of falling snow (m/s).
c     tranf  - Transport rate of falling snow (Kg/m-s).
c     vthg   - Threshold velocity of ground snow (Kg/m-s).
c     trang  - Transport rate of ground snow (Kg/m-s).
c     absazm - Adjusted absolute value for azm(iplane) (degrees).
c     scourf - Scouring factor.
c     inclf  - Incline factor.
c     perd   - Total percent of available drifting snow falling on an
c              upslope area. (%)
c     driftp - Snow drift blowing across the upslope area. (%)
c
c     +++OUTPUT FORMATS+++
c2000 format(' Inside Drift Routine',/,' wind dir = ',f10.3,/,
c     1       ' wind vel = ',
c     2         f10.3,
c     3       /,' diam = ',f10.3,/,' ppop = ',f10.3,/,
c     4       ' height = ',f10.3,/,' stor = ',
c     5         f10.3,
c     6       /,' rmasso = ',f10.3,/,' rmasst = ',f10.3,/,' rough = ',
c     7         f10.3,/,
c     8       ' length = ',f10.3,/,' width = ',f10.3,/,' vfrict = ',
c     9       f10.3,/,' tranf = ',f10.3,/,' driftf = ',f10.3,/,
c     a       ' driftg = ',f10.3,/,' trang = ',f10.3,/,' vthg = ',f10.3
c     b       ,/,' scourf = ',f10.3,/,' inclf = ',f10.3,/,' perl = ',
c     c       f10.3,/,' perd = ',f10.3,/,' drift = ',f10.3,/,
c     d       ' basden = ',f10.3,/,' trapef = ',f10.3,/,' driftp = ',
c     e       f10.3,/,' absazm = ',f10.3,/,' slplen = ',f10.3,/)
c
c     +++END SPECIFICATIONS+++

      vthf = 0.087
      driftf=0.0
      snodp = snodpy(iplane)

c -- This calculates the plant population for AGLAND land use
c -- in units of #plants/ha.  Please note that if not AGLAND,
c -- the plant population is defaulted to zero.  This will have
c -- to be expanded to also cover other types of land use.

c -- I snuck the WEPP plant population value over hear.  We must
c -- check to see if this is correct.

c -- I think we should use a different variable than pop(iplane)
c -- here, bring this to the attention of the WEPP team.  jw

      if(lanuse(iplane) .eq. 1) then
       ppop = pop(iplane)
       height = cuthgt(irtype)
      else
       ppop = 0.0
       height = canhgt(iplane)
      endif

      if(height.lt.canhgt(iplane))height = canhgt(iplane)
c
c -- This calculates the standing residue mass before and after tillage
c -- in the units of Kg/ha.

      rmasso = rmagt(iplane) * 10000.0
      rmasst = rmagt(iplane) * 10000.0
      rough = rrc(iplane)

      if (rough .lt. 0.0002) then
       rough = 0.0002
      endif

c -- Assuming four quadrants, if wind is within 45 deg. of the directn
c -- which blows up or down the slope, then length and width are
c -- considered to be equal.

      if ((wind .lt. 0.0001) .and. (wind .gt. -0.0001)) wind = 360.0

      if (ivers.ne.3.and.(azm(1) .lt. 0.1))  azm(1) = 360.0
      if (ivers.ne.3.and.(azm(1) .gt. -0.1)) azm(1) = 360.0
      if (ivers.eq.3.and.(azm(iplane) .lt. 0.1))  azm(iplane) = 360.0
      if (ivers.eq.3.and.(azm(iplane) .gt. -0.1)) azm(iplane) = 360.0

      if (ivers.ne.3)length = slplen(iplane)
c     watershed option
      if (ivers.eq.3)length = totlen(iplane)
c -- Begin calculations, start with velocity of friction @ surface.

      vfrict = 0.0662 + (0.0217 * vwind) + (0.0022 * vwind * vwind)

c -- We must now move on to calculate how much snow will be caught
c -- by the residue.

      if(height .lt. 0.001 .or. diam(irtype) .lt. 0.001 .or.
     1  ppop .lt. 1 .or. rmasst .lt. 0.001 .or. rmasso .lt. 0.001) then
        stor = rough
      else

c -- If Agricultural land...

        if (lanuse(iplane) .eq. 1) then
          basden = diam(irtype) * (ppop**0.5) / 25.0
          trapef = exp(-0.1 * height) - 0.1
          stor   = trapef * height * basden * rmasst/rmasso + rough

        else

c -- Basal density may have to be user-inputted for other than agland.

          trapef = exp(-0.1 * height) - 0.1
          stor   = trapef * height * basden + rough

        endif
      endif

c -- Now we may calculate the amount of falling drift.

c -- If not enough wind to drift falling snow. reza added 2/10/94

      if (vfrict .le. vthf) then

        drift  = 0.0
        driftf = 0.0
        driftg = 0.0
c
c -- Otherwise, friction velocity at surface exceeds the threshold
c -- friction velocity for drifting falling snow.

      else

c -- Falling drift calculations......................................

        if (hrsnow(hour) .gt. 0.0001) then

c -- First, we check to see if storage cap. exceeds avail snow.

          if ((hrsnow(hour) + snodp) .lt. stor) then
            driftf = 0.0
c -- Otherwise, snow exceeds available storage.  Assume no drifting
c -- occurs until all storage is filled.

          else
            tranf = 0.410 * vfrict * vfrict * (vfrict - vthf)
            driftf = 36 * (tranf / length)
c -- Storage capacity is not yet filled.
            if (snodp .lt. stor) then
              if (driftf .gt. (hrsnow(hour) + snodp - stor)) then

c -- Drift rate exceeds available falling snow.

                driftf = snodp + hrsnow(hour) - stor

              endif

c -- Otherwise, the storage is filled.

            else

              if (driftf .gt. hrsnow(hour)) then
                driftf = hrsnow(hour)
              endif

            endif
          endif

c -- Otherwise, no snow is falling.

        else
          driftf = 0.0

        endif

c -- Now, we ignore extremely small values.

        if (driftf .lt. 0.0001) then
          driftf = 0.0
        endif
      endif

c -- Now we calculate the ground drifting that has occurred that hour.
c -- We first check to see if a snowpack exists.

      if (snodp .gt. 0.0) then
        vthg = -0.575 / ((1.0 - sin(atan(avgslp(iplane)/100)))
     1         * (log(densg(iplane))) - 6.91)

c -- We now check if the threshold velocity of drifting ground snow
c -- is greater than friction velocity at the surface or the storage
c -- capacity exceeds the snowdep.

        if (vthg .gt. vfrict) then
c -- Then there is not enough wind to drift ground snow.
          driftg = 0.0

c -- Otherwise, wind friction velocity at the surface exceeds
c -- threshold friction velocity and storage is filled.

        else
          trang = 0.076 * vfrict * vfrict * (vfrict-vthg) / vthg
          driftg = 3600 * trang/(densg(iplane) * length)

c -- If the storage is not yet filled...

          if (snodp .lt. stor) then
            driftg = 0.0

          else
            if(driftg .ge. (snodp-stor)) then
              driftg = snodp - stor
            endif

c -- In this case, all of the snow drifts.
          endif
        endif

c -- Otherwise, no snow exists.

      else
        driftg = 0.0
      endif

      if(driftg .lt. 0.0001) then
        driftg = 0.0
      endif

c -- The falling and ground drifting has been calculated, now we must
c -- make any adjustments that may be required...

      drift = driftf + driftg
cweijun 4/5/94
c       write(31,*)'driftf === ',driftf
c       write(31,*)'driftg === ',driftg
c       write(31,*)' in sndrft, drift === ',drift
cend
c
creza added 2/18/94
c      drift=0.0

c -- If drifting occurs, then we adjust it.

      if (drift .gt. 0.0001) then

c -- Absolute value of slope facing direction - wind direction.

        if (ivers.ne.3) absazm = abs(azm(1)-wind)
        if (ivers.eq.3) absazm = abs(azm(iplane)-wind)
c
        if (absazm .gt. 180.0) then
          absazm = 360.0 - absazm
        endif

c -- Factor to adjust drifting based on direction of wind relative
c -- to slope direction.

        scourf = (absazm / 90.0) - 1.0

c -- Factor to adjust drifting based on the steepness of the slope.

        inclf = 1.0 - sin(atan(avgslp(iplane)))

c -- Snow is drifting into the cell, not out.

        if (scourf .gt. 0.0) then
          perd = exp(-1.0) / (11.0)

c -- "Driftp" distributes drifting snow down wind.

          driftp = 1.0 - perd

c -- Otherwise snow is drifting out (scouring), or no drifting.

        else
          driftp = 1.0
        endif

c -- Now, we have our actual drift calculations..............

        driftf = driftf * scourf * inclf * driftp

c -- Drifting evaporation factor for falling snow...

        if (driftf .gt. 0.0) then
          driftf = driftf * exp(-6.6e-4 * length)
        endif


        driftg = driftg * scourf * inclf * driftp

c -- Drifting evaporation factor for ground snow...

        if (driftg .gt. 0.0) then
          driftg = driftg * exp(-6.6e-4 * length)
        endif
      endif
c reza added 2/18/94
       driftf=0.0
       driftg=0.0
c reza added 2/18/94
c      write(69,2000) wind,vwind,diam(irtype),ppop,height,stor,rmasso,
c     1             rmasst,rough,length,wdth,
c     2             vfrict,tranf,driftf,driftg,trang,vthg,
c     3             scourf,inclf,perl,perd(iplane),drift,basden,trapef,
c     4             driftp,absazm,slplen(iplane)

      return
      end

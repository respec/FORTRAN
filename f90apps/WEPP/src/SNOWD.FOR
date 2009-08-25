      subroutine snowd(irtype,denh2o,iplane,driftf,driftg,snodep,
     1                 densgy,densgt,smelt,hour)
c
c     +++ PURPOSE +++
c     This subroutine handles snowdrift and snow depth calculations.
c     It was originally incorporated into WINTER, but was split out
c     by Charles R. Meyer 6/4/96 to reduce code complexity.
c
c     +++ ARGUMENT DECLARATIONS +++
      integer irtype,iplane,hour
      real    denh2o,driftf,driftg,snodep,densgy,densgt,smelt
c
c     +++ARGUMENT DEFINITIONS+++
c     irtype -
c     iplane -
c     hour   -
c     denh2o -
c     driftf -
c     driftg -
c     snodep -
c     densgy -
c     densgt -
c     smelt  -
c
c     +++ PARAMETERS +++
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxhil.inc'
c
c     +++ COMMON BLOCKS +++
      include 'cwint.inc'
      include 'cclim.inc'
c
cd     Added by S. Dun, June 15, 2007
      real densIc, wdaycy
cd     End adding 
      real setf,wrain
c
c --------------------------
c
c -- SNOW DRIFT calculations are found in the sndrft.for file.
c -- It's purpose is to calculate the amount of drifting and/or
c -- scouring of new and old snow.
c
c --------------------------
c
c -- We know must calculate the depth and density of the snowpack
c -- layer given the weather conditions for the past hour.
c
      snodep = snodpy(iplane)
      snodpt(iplane) = snodep
      densgy = densg(iplane)
      densgt = densgy
cd     Added by S. Dun, June 16, 2007
c     to record the hourly snowmelt for frost simulation
cd     snmlty(iplane) = wmelt(iplane)
cd     End adding 
c
c -- Daily snow settling factor.
c
      if (hour .eq. 1) then
           wdayct(iplane) = wdayct(iplane) + 1
      endif
c
      if (hrsnow(hour) .gt. 0.0)  wdayct(iplane) = 1
c
c -- When there is no snow on the ground.
c
c     Loop L1
      if (snodep .le. 0.0) then
c
c -- If not snowing, then nothing happens.
c
c     Loop L2
        if (hrsnow(hour) .le. 0.0) then
          densgt = 0.0
          densgy = 0.0
          snodep = 0.0         
          snodpt(iplane) = 0.0
          wmelt(iplane)  = 0.0
c
c -- Otherwise, it is snowing and possibly drifting.
c
        else
          snodep = hrsnow(hour) + driftf
          snodpt(iplane) = snodep
c          densgt = 100.0
          densgt = densnf
          densgy = densgt
          wmelt(iplane)=0.0
c -- Now we have to make sure that all of the snow didn't blow away.
c
          if (snodpt(iplane) .le. 0.0) then
            snodpt(iplane) = 0.0
            densgt = 0.0
            densgy = densgt
          endif
c
        endif
c     End Loop L2
c
c -- Otherwise, we look at the case where there was existing snow
c -- on the ground.
c
c     Loop L1
      else
c
c -- If there is no melting occurring.
c
c     Loop M2
c
cd     Modified by S. Dun, July 12, 2007
c     We are using a daily modle in a hourly way, 
c     in the original model, the following temperature means daily average T.  
cd        if (hrtemp .lt. -4.0) then
         if ((tmax + tmin)/2 .lt. 0) then          
cd     End modifying
c
c -- If it is not snowing...but is possibly drifting.
c
cd     Moved and modified by S. Dun, Aug 02, 2007
         if (snodpt(iplane) .gt. 0.0) then
c -- Now we must account for the daily snow settling factor.
C ----- Equation 3.7.1  CRM -- 10/27/95
            setf = ((exp(-(float(wdayct(iplane)) * 2.0)))*0.0416667)
     1             +1.0

            if(densgy.gt.250) setf = 1
            
            densgt = densgy * setf
c
c -- We set an upper cap for snow density.
c
            if (densgt .gt. 522) densgt = 522
c
C ----- Equation 3.7.2  CRM -- 10/27/95
cd            snodpt(iplane) = snodpt(iplane) / setf
              snodpt(iplane) = snodpt(iplane) * densgy/ densgt
         endif
cd    End the moving section

c
c     Loop M3
          if (hrsnow(hour) .le. 0.0) then
            snodep = snodpt(iplane) + driftg
cd            densgt = densgy
c
c -- If all snow on the ground has blown away.
c
            if (snodep .le. 0.0) then
              snodep = 0.0
              snodpt(iplane) = 0.0
              densgt = 0.0
              densgy = 0.0
            endif
c
cd     Moved by S. Dun, Aug 01, 2007
c
cd     Added by S. Dun, July 12, 2007
            wmelt(iplane) = 0.0
cd     End adding
c
c     Loop M3
c --------- It is snowing.  Now we check if it is possibly drifting.
          else
            snodep = snodpt(iplane) + hrsnow(hour)+driftf+driftg
C ----- Equation 3.7.3  CRM -- 10/27/95
C - XXX -- Shouldn't "snodpt" be replaced by the snow depth *yesterday* ?
cd            densgt = ((densgy * (snodpt(iplane) + driftg)) +
cd     1                (100 * (hrsnow(hour) + driftf)))   / snodep
            densgt = ((densgt * snodpt(iplane) + densgy * driftg) +
     1                (densnf * (hrsnow(hour) + driftf))) / snodep
          endif
c     End Loop M3
c    
c -- This ELSE is if temp > -4...ie. melt is occurring.
c
c     Loop M2
        else
          snodep = snodpt(iplane) + hrsnow(hour) + driftf + driftg
C ----- Equation 3.7.3  CRM -- 10/27/95
C - XXX -- Shouldn't "snodpt" be replaced by the snow depth *yesterday* ?
          densgt = ((densgy * (snodpt(iplane) + driftg)) +
     1              (densnf * (hrsnow(hour) + driftf))) / snodep
cd     1              (100 * (hrsnow(hour) + driftf)))   / snodep
c
c     Loop N3
          if (snodep .gt. 0.0) then
c
c -- Note that melt is calculated in terms of meters of water melted.
c
            call melt(irtype,wrain,hour)
c
            if (wmelt(iplane) .gt. 0.0) then
cd              densgy = densg(iplane)
              densgy = densgt
              smelt = (wmelt(iplane) * denh2o) / densgy
            else
               smelt = 0
            endif
c
c -- Nothing to melt.
c
c     Loop N3
          else
            smelt = 0.0
            wmelt(iplane) = 0.0
          endif
c     End of Loop N3
c
c -- Now we must check the new depths.
c
cd     Added by S. Dun. January 25,2008
          snodpt(iplane) = snodep
          densg(iplane) = densgt
cd     End adding
          snodep = snodpt(iplane) - smelt
c
c -- If all of the snow has been melted, we must recalculate wmelt.
c
c     Loop N4
          if (snodep .le. 0.0) then
c
c -- In the below equation, we take the depth of snow that was left,
c -- and convert it into an equivalent depth of melted water.  This
c -- is the maximum amount of melt that could have taken place for
c -- the hour.  The 1000 divisor is the density of water (Kg/m^3).
c
            wmelt(iplane) = (snodpt(iplane) * densg(iplane)) * 0.001
           snodep = 0.0
            densgt = 0.0
c -- Otherwise, not all of the snowpack melted.  It just became more
c -- dense.
c
c     Loop N4
          else
c
c     loop O5
            if (densg(iplane).ge. 350) then
               wmelt(iplane) = (smelt * densg(iplane)) * 0.001
              densgt = densg(iplane)
c     Loop o5
            else
c
               densgt = densg(iplane) * (snodpt(iplane) / snodep)
c
c -- Up until the density of the snowpack reaches 350 Kg/m^3 we don't
c -- allow any water-runoff to occur.  The melting just accounts for
c -- increasing the snowpack density.  However, after 350 Kg/m^3 we
c -- calculate an amount of water available for runoff/infiltration.
c
c     Loop p6
            if (densgt .le. 350) then
              wmelt(iplane) = 0.0
c
cd     Added by S. Dun, June 15, 2007
c     Snow holding capacity is considered here when rain on snow
c
               if(hrrain(hour).gt.0) then
c     Water density is 1000 kg/m^3, then snow density would increase
                    densic = 1000*hrrain(hour)/snodep
                    if (densic.le.(350-densgt)) then
                        hrrain(hour) = 0.
                         densgt = densgt + densic
                    else
                        hrrain(hour) = hrrain(hour)-
     1                    snodep*(350-densgt)/1000
                     densgt = 350.
                    endif
             endif              
c
cd     End adding
c
c     Loop P6
            else
              wmelt(iplane) = ((densgt - 350) * snodep) * 0.001
              densgt = 350
            endif
c     End of P6
           endif 
c     End of O5
          endif
c     End of N4
c
c -- Now we add in new snow after the melt and calculate our new
c -- snow density.
c
c     Loop N5
cd          if (hrsnow(hour) .gt. 0.0) then
cd            snodpt(iplane) = snodep
cd            densg(iplane) = densgt
cd            snodep = snodpt(iplane) + hrsnow(hour) + driftf + driftg
C ----- Equation 3.7.5  CRM -- 10/27/95
C - XXX -- Note: This equation differs from the on in the User Doc.
cd            densgt = ((densg(iplane)) * (snodpt(iplane) + driftg) +
cd     1                (100 * (hrsnow(hour) + driftf))) / snodep
cd          endif
c     End of N5
        endif
c     End of M2
c
c -- Set the global variables and move on to next hour/day.
c
        if (densgt .gt. 522) densgt = 522
        if (snodep .le. 0.0) densgt = 0.0
c
      endif
c     End of L1
      snodpt(iplane) = snodep
      snodpy(iplane) = snodep
      densg(iplane) = densgt
cd     Added by S. Dun, June 16, 2007
c     to record the hourly snowmelt for frost simulation
cd     if (wmelt(iplane).lt.snmlty(iplane)) then
          snmlty(iplane) = wmelt(iplane)
cd     endif
cd     End adding 
c
      return
      end

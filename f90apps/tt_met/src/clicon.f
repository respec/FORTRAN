      subroutine clicon
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls weather inputs to SWAT. Precipitation and
!!    temperature data is read in and the weather generator is called to 
!!    fill in radiation, wind speed and relative humidity as well as 
!!    missing precipitation and temperatures. Adjustments for climate
!!    changes studies are also made in this subroutine.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    elevb(:,:)  |m             |elevation at center of band
!!    elevb_fr(:,:)|none         |fraction of subbasin area within elevation
!!                               |band
!!    elevp(:)    |m             |elevation of precipitation gage station
!!    elevt(:)    |m             |elevation of temperature gage station
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    huminc(:,:) |none          |monthly humidity adjustment. Daily values
!!                               |for relative humidity within the month are
!!                               |raised or lowered by the specified amount.
!!                               |(used in climate change studies)
!!    id1         |julian date   |first day of simulation in year
!!    ifirstpet   |none          |potential ET data search code
!!                               |0 first day of potential ET data located in
!!                               |  file
!!                               |1 first day of potential ET data not located
!!                               |  in file
!!    ipet        |none          |code for potential ET method
!!                               |0 Priestley-Taylor method
!!                               |1 Penman/Monteith method
!!                               |2 Hargreaves method
!!                               |3 read in daily potential ET values
!!    irgage(:)   |none          |HRU rain gage data code (gage # for rainfall
!!                               |data used in HRU)
!!    itgage(:)   |none          |HRU temperature gage data code (gage # for
!!                               |temperature data used in HRU)
!!    iyr         |year          |year currently being simulated (eg 1980)
!!    i_mo        |none          |current month of simulation
!!    nhru        |none          |number of HRUs in watershed
!!    nstep       |none          |number of lines of rainfall data for each
!!                               |day
!!    pcpsim      |none          |rainfall input code
!!                               |1 gage read for each subbasin
!!                               |2 gage simulated for each subbasin
!!    plaps(:)    |mm H2O/km     |precipitation lapse rate: precipitation
!!                               |increase due to increase in elevation
!!    radinc(:,:) |MJ/m^2        |monthly solar radiation adjustment. Daily
!!                               |radiation within the month is raised or
!!                               |lowered by the specified amount. (used in
!!                               |climate change studies)
!!    rfinc(:,:)  |%             |monthly rainfall adjustment. Daily rainfall
!!                               |within the month is adjusted to the specified
!!                               |percentage of the original value (used in
!!                               |climate change studies)
!!    rhsim       |none          |relative humidity input code
!!                               |1 measured data read for each subbasin
!!                               |2 data simulated for each subbasin
!!    slrsim      |none          |solar radiation input code
!!                               |1 measured data read for each subbasin
!!                               |2 data simulated for each subbasin
!!    tlaps(:)    |deg C/km      |temperature lapse rate: temperature increase
!!                               |due to increase in elevation
!!    tmpinc(:,:) |deg C         |monthly temperature adjustment. Daily maximum
!!                               |and minimum temperatures within the month are
!!                               |raised or lowered by the specified amount
!!                               |(used in climate change studies)
!!    tmpsim      |none          |temperature input code
!!                               |1 daily max/min read for each subbasin
!!                               |2 daily max/min simulated for each subbasin
!!    welev(:)    |m             |elevation of weather station used to compile
!!                               |weather generator data
!!    wndsim      |none          |wind speed input code
!!                               |1 measured data read for each subbasin
!!                               |2 data simulated for each subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frad(:,:)   |none          |fraction of solar radiation occuring during
!!                               |hour in day in HRU
!!    hhsubp(:,:) |mm H2O        |precipitation falling during hour in day in
!!                               |HRU
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    hru_rmx(:)  |MJ/m^2        |maximum solar radiation for the day in HRU
!!    ifirstpet   |none          |potential ET data search code
!!                               |0 first day of potential ET data located in
!!                               |  file
!!                               |1 first day of potential ET data not located
!!                               |  in file
!!    pcpband(:,:)|mm H2O        |precipitation for the day in band in HRU
!!    petmeas     |mm H2O        |potential ET value read in for day
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    tavband(:,:)|deg C         |average temperature for the day in band in HRU
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmnband(:,:)|deg C         |minimum temperature for the day in band in HRU
!!    tmpav(:)    |deg C         |average temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    tmxband(:,:)|deg C         |maximum temperature for the day in band in HRU
!!    u10(:)      |m/s           |wind speed for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fradbsb(:)  |none          |hourly solar radiation fractions for subbasin
!!    ib          |none          |counter
!!    idap        |julain date   |day currently being simulated
!!    ii          |none          |counter
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |year currently being simulated
!!    k           |none          |counter
!!    pdif        |mm H2O        |difference in precipitation for station and
!!                               |precipitation for elevation band
!!    rabsb       |MJ/m^2        |generated solar radiation for subbasin
!!    ratio       |none          |fraction change in precipitation due to 
!!                               |elevation changes
!!    rbsb        |mm H2O        |generated precipitation for subbasin
!!    rhdbsb      |none          |generated relative humidity for subbasin
!!    rmxbsb      |MJ/m^2        |generated maximum solar radiation for subbasin
!!    tdif        |deg C         |difference in temperature for station and
!!                               |temperature for elevation band
!!    tmnbsb      |deg C         |generated minimum temperature for subbasin
!!    tmxbsb      |deg C         |generated maximum temperature for subbasin
!!    u10bsb      |m/s           |generated wind speed for subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Min
!!    SWAT: weatgn, clgen, slrgen, rhgen, wndgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: k, inum3sprev, npcpbsb, ii, iyp, idap, ib
      real :: tmxbsb, tmnbsb, rbsb, rhdbsb, rabsb, u10bsb, rmxbsb
      real :: daylbsb, fradbsb(24), tdif, pdif, ratio
      real, dimension (:), allocatable :: rhrbsb, rstpbsb


!! Precipitation: Measured !! - convert input in to mm
!!      if (pcpsim == 1) call pmeas
      subp(1) = precip(ijday) * 25.4

!! Temperature: Measured !! - convert input F to deg C
!!      if (tmpsim == 1) call tmeas
      tmn(1) = (tmin(ijday)-32.)*5./9.
      tmx(1) = (tmax(ijday)-32.)*5./9.

!! Generate Relative Humidity, Wind Speed, Radiation in proper SI units
!!     inum3sprev = 0
      do k = 1, nhru
            call weatgn(k)
            call clgen(k)
            call slrgen(k)
            call rhgen(k)
            call wndgen(k)
          !! set subbasin generated values
!!         inum3sprev = 0
!!         tmxbsb = 0.
!!         tmnbsb = 0.
!!         rbsb = 0.
!!         rhdbsb = 0.
!!         rabsb = 0.
!!         rmxbsb = 0.
!!         daylbsb = 0.
!!         npcpbsb = 0.
!!         u10bsb = 0.
!!         fradbsb = 0.
!!         inum3sprev = hru_sub(k)
!!         tmxbsb = tmx(k)
!!         tmnbsb = tmn(k)
!!         rbsb = subp(k)
!!         rhdbsb = rhd(k)
!!         rabsb = hru_ra(k)
!!         rmxbsb = hru_rmx(k)
!!         daylbsb = dayl(k)
!!         u10bsb = u10(k)
!!         do ii = 1, 24
!!           fradbsb(ii) = frad(k,ii)
!!         end do
!!       end if
        tmpav(k) = (tmx(k) + tmn(k)) / 2.
      end do

!! Climate Change Adjustments !!
!! jb: left in for potential future use
!!     do k = 1, nhru
!!       subp(k) = subp(k) * (1. + rfinc(hru_sub(k),i_mo) / 100.)
!!       if (subp(k) < 0.) subp(k) = 0.
!!       if (nstep > 0) then
!!         do ii = 1, nstep
!!           rainsub(k,ii) = rainsub(k,ii) *                             &
!!    &                              (1. + rfinc(hru_sub(k),i_mo) / 100.)
!!           if (rainsub(k,ii) < 0.) rainsub(k,ii) = 0.
!!         end do
!!         do ii = 1, 24
!!           hhsubp(k,ii) = hhsubp(k,ii) *                               &
!!    &                              (1. + rfinc(hru_sub(k),i_mo) / 100.)
!!           if (hhsubp(k,ii) < 0.) hhsubp(k,ii) = 0.
!!         end do
!!       end if
!!       tmx(k) = tmx(k) + tmpinc(hru_sub(k),i_mo)
!!       tmn(k) = tmn(k) + tmpinc(hru_sub(k),i_mo)
!!       tmpav(k) = tmpav(k) + tmpinc(hru_sub(k),i_mo)
!!       hru_ra(k) = hru_ra(k) + radinc(hru_sub(k),i_mo)
!!       hru_ra(k) = Max(0.,hru_ra(k))
!!       rhd(k) = rhd(k) + huminc(hru_sub(k),i_mo)
!!       rhd(k) = Max(0.01,rhd(k))
!!       rhd(k) = Min(0.99,rhd(k))
!!     end do

!! Elevation Adjustments !!
!! jb: full code left in below for potential future use. Note that the RH and PET calculations are only
!! affected by presence/absence of precipitation, so there is no need to apply a lapse
!! rate to precipitation.  Temperature should be modified.  However, the correction made here
!! is on the difference between the BASINS weather site and the weather generator site,
!! not the subbasin elevation itself.  Therefore, the following code is 
!! substituted (assuming only one hru defined for this application):
       tdif = (sub_elev(1)-welev(1)) * tlaps(1)/1000.
       tmpav(1) = tmpav(1) + tdif
       tmx(1) = tmx(1) + tdif
       tmn(1) = tmn(1) + tdif
       
!!     do k = 1, nhru
!!       if (elevb(1,hru_sub(k)) > 0. .and.                              &
!!    &                                elevb_fr(1,hru_sub(k)) > 0.) then
!!       !! determine temperature and precipitation for individual bands
!!       ratio = 0.
!!       do ib = 1, 10
!!         if (elevb_fr(ib,hru_sub(k)) < 0.) exit
!!         tdif = 0.
!!         pdif = 0.
!!         if (tmpsim == 1) then
!!           tdif = (elevb(ib,hru_sub(k)) -                              &
!!    &      Real(elevt(itgage(hru_sub(k))))) * tlaps(hru_sub(k)) / 1000.
!!         else
!!           tdif = (elevb(ib,hru_sub(k)) - welev(hru_sub(k)))           &
!!    &                                       * tlaps(hru_sub(k)) / 1000.
!!         end if
!!         if (pcpsim == 1) then
!!           pdif = (elevb(ib,hru_sub(k)) -                              &
!!    &      Real(elevp(irgage(hru_sub(k))))) * plaps(hru_sub(k)) / 1000.
!!         else
!!           pdif = (elevb(ib,hru_sub(k)) - welev(hru_sub(k)))           &
!!    &                                       * plaps(hru_sub(k)) / 1000.
!!         end if
!!         tavband(ib,k) = tmpav(k) + tdif
!!         tmxband(ib,k) = tmx(k) + tdif
!!         tmnband(ib,k) = tmn(k) + tdif
!!         if (subp(k) > 0.01) then
!!           pcpband(ib,k) = subp(k) + pdif
!!           if (pcpband(ib,k) < 0.) pcpband(ib,k) = 0.
!!         end if
!!         ratio = ratio + pdif * elevb_fr(ib,hru_sub(k))
!!       end do
!!       !! determine fraction change in precipitation for HRU
!!       if (subp(k) >= 0.01) then
!!         ratio = ratio / subp(k)
!!       else
!!         ratio = 0.
!!       end if
!!       !! determine new overall temperature and precipitation values
!!       !! for HRU
!!       tmpav(k) = 0.
!!       tmx(k) = 0.
!!       tmn(k) = 0.
!!       subp(k) = 0.
!!       do ib = 1, 10
!!         if (elevb_fr(ib,hru_sub(k)) < 0.) exit
!!         tmpav(k) = tmpav(k) + tavband(ib,k) * elevb_fr(ib,hru_sub(k))
!!         tmx(k) = tmx(k) + tmxband(ib,k) * elevb_fr(ib,hru_sub(k))
!!         tmn(k) = tmn(k) + tmnband(ib,k) * elevb_fr(ib,hru_sub(k))
!!         subp(k) = subp(k) + pcpband(ib,k) * elevb_fr(ib,hru_sub(k))
!!       end do
!!       if (nstep > 0) then
!!         do ii = 1, nstep
!!           if (rainsub(k,ii) > 0.01) then
!!             rainsub(k,ii) = rainsub(k,ii) + ratio * rainsub(k,ii)
!!             if (rainsub(k,ii) < 0.) rainsub(k,ii) = 0.
!!           end if
!!         end do
!!         do ii = 1, 24
!!           if (hhsubp(k,ii) > 0.01) then
!!             hhsubp(k,ii) = hhsubp(k,ii) + ratio * hhsubp(k,ii)
!!             if (hhsubp(k,ii) < 0.) hhsubp(k,ii) = 0.
!!           end if
!!         end do
!!       end if
!!       end if
!!     end do


      return
 5000 format (i4,i3,f5.1)
 5100 format (7x,f5.1)
      end

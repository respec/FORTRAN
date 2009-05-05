      subroutine etpot
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates potential evapotranspiration using one
!!    of three methods. If Penman-Monteith is being used, potential plant
!!    transpiration is also calculated.

!!    corrected error in wind representation: wind was assumed at 1.7 m, but u10
!!    was used.  Need for correction confirmed by email from Mike White at 
!!    ARS 4/29/09 - JB
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    laiday(:)  |m**2/m**2      |leaf area index
!!    albday     |none           |albedo for the day in HRU
!!    cht(:)     |m              |canopy height
!!    co2(:)     |ppmv           |CO2 concentration
!!    gsi(:)     |m/s            |maximum stomatal conductance
!!    hru_ra(:)  |MJ/m^2         |solar radiation for the day in HRU
!!    hru_rmx(:) |MJ/m^2         |maximum possible radiation for the day in HRU
!!    hru_sub(:) |none           |subbasin in which HRU is located
!!    icr(:)     |none           |sequence number of crop grown within the
!!                               |current year
!!    idplt(:,:,:)|none           |land cover code from crop.dat
!!    igro(:)    |none           |land cover status code
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    ihru       |none           |HRU number
!!    ipet       |none           |code for potential ET method
!!                               |0 Priestley-Taylor method
!!                               |1 Penman/Monteith method
!!                               |2 Hargreaves method
!!                               |3 read in PET values
!!    nro(:)     |none           |sequence number of year in rotation
!!    petmeas    |mm H2O         |potential ET value read in for day
!!    rhd(:)     |none           |relative humidity for the day in HRU
!!    sno_hru(:) |mm H2O         |amount of water in snow in HRU on current day
!!    sub_elev(:)|m              |elevation of HRU
!!    tmn(:)     |deg C          |minimum air temperature on current day in HRU
!!    tmpav(:)   |deg C          |average air temperature on current day in HRU
!!    tmx(:)     |deg C          |maximum air temperature on current day for HRU
!!    u10(:)     |m/s            |wind speed (measured at 10 meters above 
!!                               |surface)
!!    vpd2(:)    |(m/s)*(1/kPa)  |rate of decline in stomatal conductance per
!!               |               |unit increase in vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et) 
!!                               |that can occur on current day in HRU
!!    pet_day     |mm H2O        |potential evapotranspiration on current day in
!!                               |HRU
!!    vpd         |kPa           |vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chz         |cm            |vegetation height
!!    d           |cm            |displacement height for plant type
!!    dlt         |kPa/deg C     |slope of the saturation vapor pressure-
!!                               |temperature curve
!!    ea          |kPa           |saturated vapor pressure
!!    ed          |kPa           |actual vapor pressure
!!    fvpd        |kPa           |amount of vapro pressure deficit over 
!!                               |threshold value
!!    gma         |kPa/deg C     |psychrometric constant
!!    j           |none          |HRU number
!!    pb          |kPa           |mean atmospheric pressure
!!    pet_alpha  |none           |alpha factor in Priestley-Taylor PET equation
!!    ralb        |MJ/m2         |net incoming radiation for PET
!!    ralb1       |MJ/m2         |net incoming radiation
!!    ramm        |MJ/m2         |extraterrestrial radiation
!!    rbo         |none          |net emissivity
!!    rc          |s/m           |canopy resistance
!!    rho         |MJ/(m3*kPa)   |K1*0.622*xl*rho/pb
!!    rn          |MJ/m2         |net radiation
!!    rn_pet      |MJ/m2         |net radiation for continuous crop cover
!!    rout        |MJ/m2         |outgoing radiation
!!    rto         |none          |cloud cover factor
!!    rv          |s/m           |aerodynamic resistance to sensible heat and
!!                               |vapor transfer
!!    tk          |deg K         |average air temperature on current day for HRU
!!    uzz         |m/s           |wind speed at height zz
!!    xl          |MJ/kg         |latent heat of vaporization
!!    xx          |kPa           |difference between vpd and vpthreshold
!!    zom         |cm            |roughness length for momentum transfer
!!    zov         |cm            |roughness length for vapor transfer
!!    zz          |cm            |height at which wind speed is determined
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Sqrt, Max, Min
!!    SWAT: Ee

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real :: tk, pb, gma, xl, ea, ed, dlt, ramm, ralb1, ralb, xx
      real :: rbo, rto, rn, uzz, zz, zom, zov, rv, rn_pet, fvpd
      real :: rc, rho, rout, d, chz, gsi_adj, pet_alpha

      !! initialize local variables
      ihru = 1
      nhru = 1
      j = 0
      j = ihru

      tk = 0.
      tk = tmpav(j) + 273.15

      !! calculate mean barometric pressure
      pb = 0.
      pb = 101.3 - sub_elev(hru_sub(j)) *                               &
     &                       (0.01152 - 0.544e-6 * sub_elev(hru_sub(j)))

      !! calculate latent heat of vaporization
      xl = 0.
      xl = 2.501 - 2.361e-3 * tmpav(j)

      !! calculate psychrometric constant
      gma = 0.
      gma = 1.013e-3 * pb / (0.622 * xl)

      !! calculate saturation vapor pressure, actual vapor pressure and
      !! vapor pressure deficit
      ea = 0.
      ed = 0.
      vpd = 0.
      ea = Ee(tmpav(j))
      ed = ea * rhd(j)
      vpd = ea - ed

      !!calculate the slope of the saturation vapor pressure curve
      dlt = 0.
      dlt = 4098. * ea / (tmpav(j) + 237.3)**2
	

!! DETERMINE POTENTIAL ET
   !! PENMAN-MONTEITH POTENTIAL EVAPOTRANSPIRATION METHOD

       !! net radiation
         !! calculate net short-wave radiation for PET
          ralb = 0.
          !! albedo correction for snow not implemented
!!         if (sno_hru(j) <= .5) then
            ralb = hru_ra(j) * (1.0 - 0.23) 
!!         else
!!           ralb = hru_ra(j) * (1.0 - 0.8) 
!!         end if
         !! calculate net short-wave radiation for max plant ET
!!         ralb1 = 0.
!!         ralb1 = hru_ra(j) * (1.0 - albday) 
!! 
         !! calculate net long-wave radiation
          !! net emissivity  equation 1.2.20 in SWAT manual
          rbo = 0.
          rbo = -(0.34 - 0.139 * Sqrt(ed))

          !! cloud cover factor equation 1.2.19
          rto = 0.
          rto = 0.9 * (hru_ra(j) / hru_rmx(j)) + 0.1

          !! net long-wave radiation equation 1.2.21
          rout = 0.
          rout = rbo * rto * 4.9e-9 * (tk**4)

          !! calculate net radiation
          rn = 0.
          rn_pet = 0.
!!         rn = ralb1 + rout
          rn_pet = ralb + rout
       !! net radiation

          rho = 0.
          rho = 1710. - 6.85 * tmpav(j)

          if (u10(j) < 0.01) u10(j) = 0.01

        !! potential ET: reference crop alfalfa at 40 cm height
           rv = 0.
           rc = 0.
!!          corrected the following to adjust u10 to 1.7 m
!!         rv = 114. / u10(j)            
           rv = 114. / (u10(j)*(170./1000.)**0.2)
           rc = 49. / (1.4 - 0.4 * co2(hru_sub(j)) / 330.)
           pet_day = (dlt * rn_pet + gma * rho * vpd / rv) /            &
     &                               (xl * (dlt + gma * (1. + rc / rv)))
           pet_day = Max(0., pet_day)
           
!! debug, output intermediate calculations
!!         write(*,*)ijday,i_mo
!!         write(*,*)tmpav(j)
!!         write(*,*)rn_pet/0.041868     !Langleys
!!         write(*,*)ea
!!         write(*,*)ed
!!         write(*,*)rhd(j),vpd,ralb,rout
!!         write(*,*)dlt
!!         write(*,*)gma
!!         write(*,*)u10(j)*(170./1000.)**0.2
!!         write(*,*)pet_day
!!         write(*,*)xl,rho,rv,rc
!!         write(*,*)dlt*rn_pet,gma*rho*vpd/rv,xl*(dlt+gma*(1+rc/rv))
!!!       write(*,*)pb,xl,ea,ed,vpd
!!!       write(*,*)ralb,rbo,rto,rout,tmpav(j)
!!!       write(*,*)hru_ra(j),hru_rmx(j),dlt,rn_pet
!!!       write(*,*)rho,rv,rc,pet_day
!!         pause
 
      return
      end

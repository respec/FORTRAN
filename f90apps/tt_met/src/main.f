      include 'modparm.f'
      program main

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Main program to test subroutine
!!        - J. Butcher, 4/22/09

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    IDmet       |              |BASINS met station ID
!!    istyr       |year          |start year for generation
!!    istdy       | jday         |start Julian day for generation
!!    nbyr        |years         |number of years to generate
!!    numdata     |              |total number of data points
!!    sub_elev(1) |m             |elevation of BASINS weather site
!!    sub_lat(1)  |m             |latitude of BASINS weather site
!!    Precip(:)   |in            |vector daily observed precipitation for period of interest - convert to SI in clicon.for
!!    Tmax(:)     |F             |vector of daily observed TMAX for period of interest - convert to SI in clicon.for
!!    Tmin(:)     |F             |vector of daily observed TMIN for period of interest - convert to SI in clicon.for
!!    CO2         |ppmv          |CO2 concentration (base 330)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    PEVTPM(:)   | in/d         |vector of daily predicted Penman-Monteith PEVT
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!
      use parm
!!     input parms independent of parm.mod common block   
      integer idmet,istyrZ,istdyZ,nbyrZ,numdata,ij
      real sub_elevZ,sub_latZ,CO2Z,sump
      real precipZ(1000),TmaxZ(1000),TminZ(1000), pevtpmZ(1000)
      data numdata /365/

!!     test data for subroutine call
      sump=0.
      nbyrZ=1
      istyrZ=1990
      istdyZ=1
      sub_elevZ = 190.
      sub_latZ = 35.7
      co2Z = 330.
!!      numdata=365
!! test weather generator parameters - now read from file, except for IDWgn
        IDmet = 554
!!      Wlat = 44.4
!!      welev = 10.
!!      do ij=1,12
!!         solarav(ij,1)= 15.
!!         dewpt(ij,1)= 3.
!!         wndav(ij,1) = 5.
!!         pr_w(1,ij,1)=.05
!!         pr_w(2,ij,1)=.10
!!         tmpmn(ij,1)=3.
!!         tmpmx(ij,1)=12.
!!         pcpd(ij)=4.
!!      end do

!! test observed parameters
      do ij=1,numdata    
         precipZ(ij)=0.
         if(mod(float(ij),3.).eq.0)precipZ(ij)=.5
         tmaxZ(ij)=90.
         tminZ(ij)=50.
      end do
!!     end test data

!!     test call to the main subroutine
      call pmpevt(idmet,istyrZ,istdyZ,nbyrZ,sub_elevZ,sub_latZ,       &
    &  CO2Z,numdata,PrecipZ,TmaxZ,TminZ,PevtPMZ)
    
!!     output
      write(*,*)(pevtpmZ(ij),ij=1,numdata)
      do ij=1,numdata
        sump=sump+pevtpmZ(ij)
      end do
      write(*,*)sump
      write (*,1001)
 1001 format (/," Execution successfully completed ")
      stop
      end
      
      
      subroutine pmpevt(idmet,istyrZ,istdyZ,nbyrZ,sub_elevZ,sub_latZ, &
    &  CO2Z,numdata,PrecipZ,TmaxZ,TminZ,PevtPMZ)

!! uncomment the following to compile as subroutine to be linked to another program      
!!      include 'modparm.f'

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This code is taken from SWAT2005 to generate Penman-Monteith PEVT 
!!     using measured precipitation and temperature and generated solar, RH, and wind
!!     The code structure and subroutine names from SWAT are maintained, 
!!     But the program is stripped down to perform only the needed functions.
!!     As a result, some subroutines are just dummy stubs in this implementation.
!!        - J. Butcher, 4/22/09

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!! FROM CALLING PROGRAM - passed with Z suffix and assigned to these common variables below
!!    IDmet       |              |BASINS weather station ID - used to look up weather generator 
!!    istyr       |year          |start year for generation
!!    istdy       | jday         |start Julian day for generation
!!    nbyr        |years         |number of years to generate
!!    numdata     |days          |total number of data points  - no 'Z', not in common
!!    sub_elev(1) |m             |elevation of BASINS weather site
!!    sub_lat(1)  |m             |latitude of BASINS weather site
!!    CO2         |ppmv          |CO2 concentration (base 330)
!!    Precip(:)   |in            |vector daily observed precipitation for period of interest - convert to SI in clicon.for
!!    Tmax(:)     |F             |vector of daily observed TMAX for period of interest - convert to SI in clicon.for
!!    Tmin(:)     |F             |vector of daily observed TMIN for period of interest - convert to SI in clicon.for
!! READ FROM WEATHER GENERATOR FILES
!!    pcpd(:)     |              |average precip days per month from weather generator
!!    pr_w(1,:,:) |none          |probability of wet day after dry day in month from weather generator (used by rhgen)
!!    pr_w(2,:,:) |none          |probability of wet day after wet day in month from weather generator (used by rhgen)
!!    Wlat(1)     |DD            |weather generator station latitude (DD)
!!    Welev(1)    |m             |weather generator station elevation
!!    Solarav(12,1)|             |average solar radiation by month from wgn
!!    Dewpt(12,1) |C             |monthly dewpoint mean values from wgn
!!    Wndav(12,1) |              |monthly wind mean values from wgn
!!    Tmpmn(:,1)  |C             |weather generator monthly average minimum temperature (used by rhgen)
!!    Tmpmx(:,1)  |C             |weather generator monthly average maximum temperature (used by rhgen)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    PEVTPM(:)   |in/d          |vector of daily predicted Penman-Monteith PEVT
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!
      
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: readwgn, simulate, gcycl                         

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
!!     input parms independent of parm.mod common block   
      integer idmet,istyrZ,istdyZ,nbyrZ,numdata,ij,idwgnZ
      real sub_elevZ,sub_latZ,CO2Z
      real precipZ(numdata),TmaxZ(numdata),TminZ(numdata),PevtPMZ(numdata)

!!     note nbyr is used in allocate_parms, which sets array dimensions for other variables      
      nbyr=nbyrZ
      call allocate_parms
      
!! assign pased variables to common blocks
!!    idwgn=idwgnZ
      istyr=istyrZ
      istdy=istdyZ
      nbyr=nbyrZ
      sub_elev(1)=sub_elevZ
      sub_lat(1)=sub_latZ
      CO2(1)=CO2Z
      Do ij=1,numdata
        Precip(ij)=PrecipZ(ij)
        Tmax(ij)=TmaxZ(ij)
        Tmin(ij)=TminZ(ij) 
      End do

!! look up the weather generation station ID idwgn based on BASINS ID
!! this code not yet written - so hardwire for now, with idwgn=idmet

     idwgnz = idmet
!!   call metlkup(idmet,idwgnZ)
     idwgn = idwgnZ



!! process input
		

!!jb    call getallo
!!jb    call allocate_parms
!!jb    call readfile
!!jb    set hru number to 1 for call to readwgn (later becomes day counter)
        i = 1
        hru_sub(1) = 1
!! define days per month (from zeroini.f)
        ndays = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
!!jb define temperature lapse rate as default value of -6 deg C/km (SWAT2005 reads this as a variable from input)
        tlaps(1)=-6.0
!!jb read the appropriate weather generator parameters from statwgn.txt
        call rdwgn
!!jb     old readwgn called directly instead of via readfig and readsub to perform initializations only
        call readwgn
!!       define igen=1 for random number generator -use default series every time
        igen = 1
!!       define first day
        idaf = istdy
        iyr = istyr
!!       initialize consecutive day counter        
        ijday = 0
!!jb       initialize random number generator at this point
        call gcycl
!! initialize wgnold, npcp (from zero2.f)   
      wgnold = 0.
      npcp = 1
!!      pr_w = 0

!! do the calculations
        call simulate
        
!! Reassign results out of common for explicit transfer back
      DO ij=1,numdata
        PevtPMz(ij)=PevtPM(ij)
      End do
      
!!	stop
      return
      end

      
      
      subroutine pmpevt(idmet,istyrZ,istdyZ,nbyrZ,sub_elevZ,sub_latZ,
     &                   CO2Z,numdata,PrecipZ,TmaxZ,TminZ,PevtPMZ)

C  uncomment the following to compile as subroutine to be linked to another program      
C     include 'modparm.f'

C     ~ ~ ~ PURPOSE ~ ~ ~
C     This code is taken from SWAT2005 to generate Penman-Monteith PEVT 
C      using measured precipitation and temperature and generated solar, RH, and wind
C      The code structure and subroutine names from SWAT are maintained, 
C      But the program is stripped down to perform only the needed functions.
C      As a result, some subroutines are just dummy stubs in this implementation.
C         - J. Butcher, 4/22/09

C     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
C     name        |units         |definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
C  FROM CALLING PROGRAM - passed with Z suffix and assigned to these common variables below
C     IDmet       |              |BASINS weather station ID - used to look up weather generator 
C     istyr       |year          |start year for generation
C     istdy       | jday         |start Julian day for generation
C     nbyr        |years         |number of years to generate
C     numdata     |days          |total number of data points  - no 'Z', not in common
C     sub_elev(1) |m             |elevation of BASINS weather site
C     sub_lat(1)  |m             |latitude of BASINS weather site
C     CO2         |ppmv          |CO2 concentration (base 330)
C     Precip(:)   |in            |vector daily observed precipitation for period of interest - convert to SI in clicon.for
C     Tmax(:)     |F             |vector of daily observed TMAX for period of interest - convert to SI in clicon.for
C     Tmin(:)     |F             |vector of daily observed TMIN for period of interest - convert to SI in clicon.for
C  READ FROM WEATHER GENERATOR FILES
C     pcpd(:)     |              |average precip days per month from weather generator
C     pr_w(1,:,:) |none          |probability of wet day after dry day in month from weather generator (used by rhgen)
C     pr_w(2,:,:) |none          |probability of wet day after wet day in month from weather generator (used by rhgen)
C     Wlat(1)     |DD            |weather generator station latitude (DD)
C     Welev(1)    |m             |weather generator station elevation
C     Solarav(12,1)|             |average solar radiation by month from wgn
C     Dewpt(12,1) |C             |monthly dewpoint mean values from wgn
C     Wndav(12,1) |              |monthly wind mean values from wgn
C     Tmpmn(:,1)  |C             |weather generator monthly average minimum temperature (used by rhgen)
C     Tmpmx(:,1)  |C             |weather generator monthly average maximum temperature (used by rhgen)
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

C     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
C     name        |units         |definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
C     PEVTPM(:)   |in/d          |vector of daily predicted Penman-Monteith PEVT
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
C 
      
C     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
C     name        |units         |definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
C     i           |none          |counter
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


C     ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
C     SWAT: readwgn, simulate, gcycl                         

C     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none

      dll_export pmpevt

C      input parms independent of parm.mod common block   
      integer idmet,istyrZ,istdyZ,nbyrZ,numdata,ij,idwgnZ
      real sub_elevZ,sub_latZ,CO2Z
      real precipZ(numdata),TmaxZ(numdata),TminZ(numdata)
      real PevtPMZ(numdata)

      write(*,*) idmet,istyrZ,istdyz,nbyrZ,numdata,ij,idwgnz
      write(*,*) precipz(1),tmaxz(1),tminz(1)

C      note nbyr is used in allocate_parms, which sets array dimensions for other variables      
      nbyr=nbyrZ
      call allocate_parms
      
C  assign pased variables to common blocks
C     idwgn=idwgnZ
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

C  look up the weather generation station ID idwgn based on BASINS ID
C  this code not yet written - so hardwire for now, with idwgn=idmet

      idwgnz = idmet
C     call metlkup(idmet,idwgnZ)
      idwgn = idwgnZ



C  process input
		

C jb    call getallo
C jb    call allocate_parms
C jb    call readfile
C jb    set hru number to 1 for call to readwgn (later becomes day counter)
        i = 1
        hru_sub(1) = 1
C  define days per month (from zeroini.f)
        ndays = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
C jb define temperature lapse rate as default value of -6 deg C/km (SWAT2005 reads this as a variable from input)
        tlaps(1)=-6.0
C jb read the appropriate weather generator parameters from statwgn.txt
        call rdwgn
C jb     old readwgn called directly instead of via readfig and readsub to perform initializations only
        call readwgn
C        define igen=1 for random number generator -use default series every time
        igen = 1
C        define first day
        idaf = istdy
        iyr = istyr
C        initialize consecutive day counter        
        ijday = 0
C jb       initialize random number generator at this point
        call gcycl
C  initialize wgnold, npcp (from zero2.f)   
      wgnold = 0.
      npcp = 1
C       pr_w = 0

C  do the calculations
        call simulate
        
C  Reassign results out of common for explicit transfer back
      DO ij=1,numdata
C        if (PevtPM(ij) .gt. 40.0) then
C          write(*,*) 'in pmpevt PevtPM > 40',PevtPM(ij),ij
C          pause
C        end if 
        PevtPMz(ij)=PevtPM(ij)
      End do
      
C 	stop
      call free_parms
      return
      end

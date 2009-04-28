      subroutine simulate

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine contains the loops governing the modeling of processes
!!    in the watershed 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idaf        |julian date   |beginning day of simulation
!!    idal        |julian date   |ending day of simulation
!!    iyr         |none          |beginning year of simulation
!!    nbyr        |none          |number of years in simulation
!!    sub_lat(:)  |degrees       |latitude of HRU/subbasin
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year in simulation (sequence)
!!    i           |julian date   |current day in simulation--loop counter
!!    id1         |julian date   |first day of simulation in current year
!!    iida        |julian date   |day being simulated (current julian day)
!!    iyr         |year          |current year of simulation (eg 1980)
!!    leapyr      |none          |leap year flag:
!!                               |0  leap year
!!                               |1  regular year
!!    ijday       |counter       |counter of days simulated 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ic          |none          |counter
!!    idlst       |julian date   |last day of simulation in current year
!!    j           |none          |counter
!!    xx          |none          |current year in simulation sequence
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Mod, Real
!!    SWAT: xmon, clicon, command

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: idlst, j, iix, iiz, ic, mon, ii
      real :: xx


      do curyr = 1, nbyr
!        write (*,1234) curyr


        !!determine beginning and ending dates of simulation in current year
        if (Mod(iyr,4) == 0) then 
          leapyr = 0   !!leap year
        else 
          leapyr = 1   !!regular year
        end if

        !! set beginning day of simulation for year
        id1 = 0
        if (curyr == 1 .and. idaf > 0) then
          id1 = idaf
        else
          id1 = 1
        end if

        !! set ending day of simulation for year
!        idlst = 0
!        if (curyr == nbyr .and. idal > 0) then
!          idlst = idal
!        else
          idlst = 366 - leapyr
!        end if
        
        !! set current julian date to begin annual simulation
        iida = 0
        iida = id1

        call xmon


        do i = id1, idlst                            !! begin daily loop
        
          ijday = ijday + 1

          call clicon               !! read in/generate weather

          call command              !! command loop

          iida = i + 1
          call xmon

        end do                                        !! end daily loop

!! perform end-of-year processes

      !! update simulation year
      iyr = iyr + 1
      end do            !!     end annual loop

      return
! 1234 format (1x,' Executing year ', i4)
      end

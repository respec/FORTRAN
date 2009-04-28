      subroutine command
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    for every day of simulation, this subroutine steps through the command
!!    lines in the watershed configuration (.fig) file. 
!!    Retained in this application only to call subbasin which calls etpot

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: subbasin, 
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: idum
      
      call subbasin

      return
      end

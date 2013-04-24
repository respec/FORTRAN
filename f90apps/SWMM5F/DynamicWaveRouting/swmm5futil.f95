module swmm5futil
use DataSizeSpecs

!-----------------------------------------------------------------------------
!  Unit conversion factors
!-----------------------------------------------------------------------------
real(kind=dp), dimension(2, 10), parameter :: arrUcf = &                !(5.0.010 - LR)
    &reshape((/43200.0,   1097280.0 , &         ! RAINFALL (in/hr, mm/hr --> ft/sec)
      &12.0,      304.8     , &         ! RAINDEPTH (in, mm --> ft)
      &1036800.0, 26334720.0, &         ! EVAPRATE (in/day, mm/day --> ft/sec)
      &1.0,       0.3048    , &         ! LENGTH (ft, m --> ft)
      &2.2956e-5, 0.92903e-5, &         ! LANDAREA (ac, ha --> ft2)
      &1.0,       0.02832   , &         ! VOLUME (ft3, m3 --> ft3)
      &1.0,       1.608     , &         ! WINDSPEED (mph, km/hr --> mph)
      &1.0,       1.8       , &         ! TEMPERATURE (deg F, deg C --> deg F)
      &2.203e-6,  1.0e-6    , &         ! MASS (lb, kg --> mg)
      &43560.0,   3048.0  /), &        ! GWFLOW (cfs/ac, cms/ha --> ft/sec)   !(5.0.010 - LR)
      &(/2, 10/))

!      {!  US      SI
!      {43200.0,   1097280.0 },         ! RAINFALL (in/hr, mm/hr --> ft/sec)
!      {12.0,      304.8     },         ! RAINDEPTH (in, mm --> ft)
!      {1036800.0, 26334720.0},         ! EVAPRATE (in/day, mm/day --> ft/sec)
!      {1.0,       0.3048    },         ! LENGTH (ft, m --> ft)
!      {2.2956e-5, 0.92903e-5},         ! LANDAREA (ac, ha --> ft2)
!      {1.0,       0.02832   },         ! VOLUME (ft3, m3 --> ft3)
!      {1.0,       1.608     },         ! WINDSPEED (mph, km/hr --> mph)
!      {1.0,       1.8       },         ! TEMPERATURE (deg F, deg C --> deg F)
!      {2.203e-6,  1.0e-6    },         ! MASS (lb, kg --> mg)
!      {43560.0,   3048.0    }          ! GWFLOW (cfs/ac, cms/ha --> ft/sec)   !(5.0.010 - LR)
!      };

real(kind=dp), dimension(6), parameter :: Qcf = &                 ! Flow Conversion Factors:
     &(/ 1.0,     448.831, 0.64632, &     ! cfs, gpm, mgd --> cfs
       &0.02832, 28.317,  2.4466/)    ! cms, lps, mld --> cfs
       
!
!For use in 'statsrpt'
!
character(6) :: FlowFmt
real(kind=dp) :: Vcf

!
!For use in output and report
!
!--! specific precisions, usually same as real and real(kind=dp)
!integer, parameter :: r6 = selected_real_kind(6) 
!integer, parameter :: r15 = selected_real_kind(15) 

!integer, parameter :: k6 = selected_int_kind(6) 
!integer, parameter :: k15 = selected_int_kind(15)

real(kind=dp), dimension(:), allocatable, save :: SubcatchResults !REAL8
real(kind=dp), dimension(:), allocatable, save :: NodeResults !REAL8
real(kind=dp), dimension(:), allocatable, save :: LinkResults !REAL8

type myoutput
  integer :: datatype
  integer :: index
  real(kind=dp), dimension(:), pointer :: oflow
  real(kind=dp), dimension(:), pointer :: odepth
  real(kind=dp), dimension(:), pointer :: ovolume
end type myoutput

type myTimeseries
  integer :: datatype
  integer :: tohere
  real(kind=dp), dimension(:), pointer :: odates
  real(kind=dp), dimension(:), pointer :: ovalues 
end type myTimeseries

type(myoutput), dimension(:), allocatable, save :: onodes
type(myoutput), dimension(:), allocatable, save :: olinks

type(myTimeseries), dimension(:), allocatable, save :: oTsers

contains
real(kind=dp) function UCF(u)
!
!  Input:   u = integer code of quantity being converetd
!  Output:  returns a units conversion factor
!  Purpose: computes a conversion factor from SWMM's internal
!           units to user's units
!
    use DataSizeSpecs
    use globals
    implicit none
    integer(kind=K2), intent(in) :: u
    real(kind=dp) :: lUCF
    
    if ( u < FLOW ) then
        !lUCF = arrUcf(u, UnitSystem)
        lUCF = arrUcf(UnitSystem, u)
    else            
        lUCF = Qcf(FlowUnits)
    end if
    UCF = lUCF
    return
end function UCF
!
!!=============================================================================
!
INTEGER FUNCTION ReDim(array)
IMPLICIT NONE
REAL(KIND=dp), DIMENSION(:),POINTER :: array
REAL(KIND=dp), DIMENSION(:),ALLOCATABLE :: tmp_arr
INTEGER :: prevSize, lStat
prevSize = SIZE(array, 1)

ALLOCATE(tmp_arr(2*prevSize), stat=lStat)
IF (lStat /= 0) THEN
   ReDim = lStat
   RETURN
END IF
tmp_arr(1:prevSize)=array

DEALLOCATE(array)
ALLOCATE(array(SIZE(tmp_arr, 1)), stat=lStat)
IF (lStat /= 0) THEN
   ReDim = lStat
   RETURN
END IF

array=tmp_arr
ReDim = 0
END FUNCTION ReDim

INTEGER FUNCTION ReDimReal(array)
IMPLICIT NONE
REAL ,DIMENSION(:),POINTER :: array
REAL ,DIMENSION(:),ALLOCATABLE :: tmp_arr
INTEGER :: prevSize, lStat
prevSize = SIZE(array, 1)

ALLOCATE(tmp_arr(2*prevSize), stat=lStat)
IF (lStat /= 0) THEN
   ReDimReal = lStat
   RETURN
END IF
tmp_arr(1:prevSize)=array

DEALLOCATE(array)
ALLOCATE(array(SIZE(tmp_arr, 1)), stat=lStat)
IF (lStat /= 0) THEN
   ReDimReal = lStat
   RETURN
END IF

array=tmp_arr
ReDimReal = 0
END FUNCTION ReDimReal

end module

module infil
!-----------------------------------------------------------------------------
!   infil.h
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    07/30/10   (Build 5.0.019)
!            04/20/11   (Build 5.0.022)
!   Author:  L. Rossman (US EPA)
!
!   Public interface for infiltration functions.
!-----------------------------------------------------------------------------
integer, parameter :: dpi = kind(1.d0)
!---------------------
! Enumerated Constants
!---------------------
!enum InfilType {
integer, parameter :: HORTON = 1                      ! Horton infiltration
integer, parameter :: GREEN_AMPT = 2                  ! Green-Ampt infiltration
integer, parameter :: CURVE_NUMBER = 3               ! SCS Curve Number infiltration

!---------------------
! Horton Infiltration
!---------------------
type THorton
   real(kind=dpi) :: fmin            ! minimum infil. rate (ft/sec)
   real(kind=dpi) :: Fmax            ! maximum total infiltration (ft)
   real(kind=dpi) :: decay           ! decay coeff. of infil. rate (1/sec)
   real(kind=dpi) :: regen           ! regeneration coeff. of infil. rate (1/sec)
   !-----------------------------
   real(kind=dpi) :: tp              ! present time on infiltration curve (sec)
   real(kind=dpi) :: f0              ! initial infil. rate (ft/sec)
end type THorton


!-------------------------
! Green-Ampt Infiltration
!-------------------------
type TGrnAmpt

   real(kind=dpi) :: S               ! avg. capillary suction (ft)
   real(kind=dpi) :: Ks              ! saturated conductivity (ft/sec)
   real(kind=dpi) :: IMDmax          ! max. soil moisture deficit (ft/ft)
   !-----------------------------
   real(kind=dpi) :: IMD             ! current soil moisture deficit
   real(kind=dpi) :: F               ! current cumulative infiltration (ft)
   real(kind=dpi) :: T               ! time needed to drain upper zone (sec)
   real(kind=dpi) :: L               ! depth of upper soil zone (ft)
   real(kind=dpi) :: FU              ! current moisture content of upper zone (ft)
   real(kind=dpi) :: FUmax           ! saturated moisture content of upper zone (ft)
   !char          Sat             ! saturation flag
   logical :: Sat             ! saturation flag
end type TGrnAmpt


!--------------------------
! Curve Number Infiltration
!--------------------------
type TCurveNum
   real(kind=dpi) :: Smax            ! max. infiltration capacity (ft)
   real(kind=dpi) :: regen           ! infil. capacity regeneration constant (1/sec)
   real(kind=dpi) :: Tmax            ! maximum inter-event time (sec)
   !-----------------------------
   real(kind=dpi) :: S               ! current infiltration capacity (ft)
   real(kind=dpi) :: Fcinfil         ! current cumulative infiltration (ft) !F
   real(kind=dpi) :: P               ! current cumulative precipitation (ft)
   real(kind=dpi) :: T               ! current inter-event time (sec)
   real(kind=dpi) :: Se              ! current event infiltration capacity (ft)
   real(kind=dpi) :: fprevinfil      ! previous infiltration rate (ft/sec) !f
end type TCurveNum

!-----------------------------------------------------------------------------
!   Exported Variables
!-----------------------------------------------------------------------------
!extern THorton*   HortInfil
!extern TGrnAmpt*  GAInfil
!extern TCurveNum* CNInfil
type(THorton) ::   HortInfil
type(TGrnAmpt) ::  GAInfil
type(TCurveNum) :: CNInfil

!!-----------------------------------------------------------------------------
!!   Infiltration Methods
!!-----------------------------------------------------------------------------
!void    infil_create(int subcatchCount, int model)
!void    infil_delete(void)
!int     infil_readParams(int model, char* tok[], int ntoks)
!void    infil_initState(int area, int model)
!double  infil_getInfil(int area, int model, double tstep, double rainfall,
!        double runon, double depth)                                           !(5.0.022 - LR)
!
!int     grnampt_setParams(TGrnAmpt *infil, double p[])
!void    grnampt_initState(TGrnAmpt *infil)
!double  grnampt_getInfil(TGrnAmpt *infil, double tstep, double irate,
!        double depth)
!
!#endif

end module infil
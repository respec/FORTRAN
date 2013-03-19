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
   double precision :: fmin            ! minimum infil. rate (ft/sec)
   double precision :: Fmax            ! maximum total infiltration (ft)
   double precision :: decay           ! decay coeff. of infil. rate (1/sec)
   double precision :: regen           ! regeneration coeff. of infil. rate (1/sec)
   !-----------------------------
   double precision :: tp              ! present time on infiltration curve (sec)
   double precision :: f0              ! initial infil. rate (ft/sec)
end type THorton


!-------------------------
! Green-Ampt Infiltration
!-------------------------
type TGrnAmpt

   double precision :: S               ! avg. capillary suction (ft)
   double precision :: Ks              ! saturated conductivity (ft/sec)
   double precision :: IMDmax          ! max. soil moisture deficit (ft/ft)
   !-----------------------------
   double precision :: IMD             ! current soil moisture deficit
   double precision :: F               ! current cumulative infiltration (ft)
   double precision :: T               ! time needed to drain upper zone (sec)
   double precision :: L               ! depth of upper soil zone (ft)
   double precision :: FU              ! current moisture content of upper zone (ft)
   double precision :: FUmax           ! saturated moisture content of upper zone (ft)
   !char          Sat             ! saturation flag
   logical :: Sat             ! saturation flag
end type TGrnAmpt


!--------------------------
! Curve Number Infiltration
!--------------------------
type TCurveNum
   double precision :: Smax            ! max. infiltration capacity (ft)
   double precision :: regen           ! infil. capacity regeneration constant (1/sec)
   double precision :: Tmax            ! maximum inter-event time (sec)
   !-----------------------------
   double precision :: S               ! current infiltration capacity (ft)
   double precision :: Fcinfil         ! current cumulative infiltration (ft) !F
   double precision :: P               ! current cumulative precipitation (ft)
   double precision :: T               ! current inter-event time (sec)
   double precision :: Se              ! current event infiltration capacity (ft)
   double precision :: fprevinfil      ! previous infiltration rate (ft/sec) !f
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
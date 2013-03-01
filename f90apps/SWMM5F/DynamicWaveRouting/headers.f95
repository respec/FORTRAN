include 'macros.f95'
include 'error.f95'
include 'keywords.f95'
include 'globals.f95'

module headers
!-----------------------------------------------------------------------------
!   headers.h
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    6/19/07   (Build 5.0.010)
!   Author:  L. Rossman
!
!   Header files included in most SWMM5 modules.
!
!   DO NOT CHANGE THE ORDER OF THE #INCLUDE STATEMENTS
!-----------------------------------------------------------------------------
use macros
use error
!use datetime
use keywords !use text
!#define  EXTERN extern
use globals !use objects, objects uses enum and consts
end module

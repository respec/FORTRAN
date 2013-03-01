include 'consts.f95'
include 'macros.f95'
include 'enums.f95'
include 'error.f95'
include 'objects.f95'
include 'text.f95'
include 'keywords.f95'

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
use consts
use macros
use enums
use error
!use datetime
use objects
use text
use keywords
!#define  EXTERN extern
use globals
end module

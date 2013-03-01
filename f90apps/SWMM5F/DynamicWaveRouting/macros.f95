module macros
!-----------------------------------------------------------------------------
!   macros.h
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    6/19/07   (Build 5.0.010)
!   Author:  L. Rossman
!-----------------------------------------------------------------------------

contains
!--------------------------------------------------
! Macro to test for successful allocation of memory
!--------------------------------------------------
!#define  MEMCHECK(x)  (((x) == NULL) ? 101 : 0 )

!--------------------------------------------------
! Macro to free a non-null pointer
!--------------------------------------------------
!#define  FREE(x) { if (x) { free(x); x = NULL; } }

!---------------------------------------------------
! Conversion macros to be used in place of functions
!---------------------------------------------------
!#define ABS(x)   (((x)<0) ? -(x) : (x))          /* absolute value of x   */
!#define MIN(x,y) (((x)<=(y)) ? (x) : (y))        /* minimum of x and y    */
!#define MAX(x,y) (((x)>=(y)) ? (x) : (y))        /* maximum of x and y    */
!#define MOD(x,y) ((x)%(y))                       /* x modulus y           */
!#define LOG10(x) ((x) > 0.0 ? log10((x)) : (x))  /* safe log10 of x       */
double precision function LOG10Safe(x)
   double precision, intent(in) :: x
   if (x > 0.0) then
           LOG10Safe = log10(x)
   else
           LOG10Safe = x
   end if
   return
end function LOG10Safe
!#define SQR(x)   ((x)*(x))                       /* x-squared             */
double precision function SQR(x)
   double precision, intent(in) :: x
   SQR = x * x
end function SQR
!#define SGN(x)   (((x)<0) ? (-1) : (1))          /* sign of x             */
integer function SGN(x)
   double precision, intent(in) :: x
   if (x < 0) then
           SGN = -1
   else
           SGN = 1
   end if
end function SGN
!#define SIGN(x,y) ((y) >= 0.0 ? fabs(x) : -fabs(x))

!* uppercase char of x   *!
!#define UCHAR(x) (((x) >= 'a' && (x) <= 'z') ? ((x)&~32) : (x))
character*1 function UCHAR(x)
   character*1, intent(in) :: x
   integer :: j
   j = iachar(x)
   if (j>= iachar("a") .and. j<=iachar("z") ) then
        UCHAR = achar(iachar(x)-32)
   else
        UCHAR = x
   end if
end function UCHAR

!#define ARRAY_LENGTH(x) (sizeof(x)/sizeof(x[0])) /* length of array x     */

!-------------------------------------------------
! Macro to evaluate function x with error checking
!-------------------------------------------------
!#define CALL(x) (ErrorCode = ((ErrorCode>0) ? (ErrorCode) : (x)))

end module

!>```
!>
!> Every test is defined in a separate module using a ``collect`` function, which
!> is exported and added to the ``testsuites`` array in the test runner.
!> All test have a simple interface with just an allocatable [[error_type]] as
!> output to provide the test results.
!>
!>```fortran
module test_suite2
  use testdrive, only : new_unittest, unittest_type, error_type, check
  implicit none
  private

  public :: collect_suite2

contains

    !> Collect all exported unit tests
    subroutine collect_suite2(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
        new_unittest("valid", test_valid), &
        new_unittest("invalid", test_invalid, should_fail=.true.), &
        new_unittest("truth_test_moms_p3", truth_test_moms_p3) &
        ]

    end subroutine collect_suite2

    subroutine test_valid(error)
      type(error_type), allocatable, intent(out) :: error
      ! ...
      call check(error, 1 + 2 == 3)
      if (allocated(error)) return
  
    end subroutine test_valid

    subroutine test_invalid(error)
      type(error_type), allocatable, intent(out) :: error
      ! ...
      call check(error, 1 + 2 == 5)
    end subroutine test_invalid
    
    subroutine truth_test_moms_p3(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: n
      double precision :: ql(250),qu(250),rG,mc_old(3),moms(3),thr
      double precision :: nG
      double precision :: moms_x(3)
      
      integer ::          abcf
      logical ::          alskewXmax
      double precision :: ask0,ask141,askxmax
      
!c       moms_p3 input variables:
!c       ------------------------------------------------------------------------
!c            n          i*4  number of observations (censored, uncensored, or 
!c                              other)
!c            ql(n)      r*8  vector of lower bounds on (log) floods
!c            qu(n)      r*8  vector of upper bounds on (log) floods
!c            rG         r    regional skew value (needed for B17C EQ 7-10)
!c            nG         r    equivalent years of record for regional skew
!c            mc_old(3)  r*8  vector of p3 parameters
!c
!c       moms_p3 output variables:
!c       ------------------------------------------------------------------------
!c            moms(3)    r*8  vector of updated p3 parameters
      
      ! read ql and qu from csv file
      integer :: I
      character(len=8) header(3),dat1

      open(unit=50,file="example_intervals.csv")
      read(50,*) header
      do I=1,200
        read(50,*) dat1,ql(I),qu(I)
      end do
      close(unit=50)
      
      open(unit=50,file="example_truth.csv")
      read(50,*) header
      read(50,*) dat1,moms_x(1),moms_x(2),moms_x(3)
      close(unit=50)
      
      !input values for tests
      n = 200
      nG = 0.0      
      rG = 0.0
      
      abcf = 1997
      alskewXmax = .False.
      ask0 = 0.0
      ask141 = -1.41
      askxmax = 0.0
      call set_common_tac002(ask0,ask141,askxmax,alskewXmax,abcf)
      
      ! test for orig version
      if (.not.allocated(error)) then
          thr = 1.0e-2
      
          mc_old(1) = 0.0
          mc_old(2) = 1.0
          mc_old(3) = 0.0
      
          moms = 0.0
          CALL moms_p3(n,ql,qu,rG,nG,mc_old,moms)
      
          call check(error, moms(1), moms_x(1), 'Problem with truth moms(1)', '', thr)
          if (.not.allocated(error)) then
            call check(error, moms(2), moms_x(2), 'Problem with truth moms(2)', '', thr)
            if (.not.allocated(error)) then
                call check(error, moms(3), moms_x(3), 'Problem with truth moms(3)', '', thr)
            end if 
          end if      
      end if 
      
      ! now do advanced test, with 50+ additional values
      n = 250
      do I=201,250
        ql(I) = -10.0
        qu(I) =  10.0
      end do
      
      abcf = 1997
      alskewXmax = .False.
      ask0 = 0.0
      ask141 = -1.41
      askxmax = 0.0
      call set_common_tac002(ask0,ask141,askxmax,alskewXmax,abcf)
      
      ! test for orig version
      if (.not.allocated(error)) then
          thr = 1.0e-2
      
          mc_old(1) = 0.0
          mc_old(2) = 1.0
          mc_old(3) = 0.0
      
          moms = 0.0
          CALL moms_p3(n,ql,qu,rG,nG,mc_old,moms)
      
          call check(error, moms(1), moms_x(1), 'Problem with truth advanced moms(1)', '', thr)
          if (.not.allocated(error)) then
            call check(error, moms(2), moms_x(2), 'Problem with truth advanced moms(2)', '', thr)
            if (.not.allocated(error)) then
                call check(error, moms(3), moms_x(3), 'Problem with truth advanced moms(3)', '', thr)
            end if 
          end if      
      end if 
      
    end subroutine truth_test_moms_p3

end module test_suite2   
!>```
!>
!> Every test is defined in a separate module using a ``collect`` function, which
!> is exported and added to the ``testsuites`` array in the test runner.
!> All test have a simple interface with just an allocatable [[error_type]] as
!> output to provide the test results.
!>
!>```fortran
module test_suite1
  use testdrive, only : new_unittest, unittest_type, error_type, check  ! check_float_dp
  implicit none
  private

  public :: collect_suite1

contains

    !> Collect all exported unit tests
    subroutine collect_suite1(testsuite)
    
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
        new_unittest("valid", test_valid), &
        new_unittest("invalid", test_invalid, should_fail=.true.), &
        new_unittest("b17ci", test_b17ci), &
        new_unittest("moms_p3", test_moms_p3) &
        ]

    end subroutine collect_suite1

    subroutine test_valid(error)
      type(error_type), allocatable, intent(out) :: error
      
      call check(error, 1 + 2 == 3)
      if (allocated(error)) return
  
    end subroutine test_valid

    subroutine test_invalid(error)
      type(error_type), allocatable, intent(out) :: error
      
      call check(error, 1 + 2 == 4)
    end subroutine test_invalid
    
    subroutine test_b17ci(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: n
      double precision :: skew,p,c,klpc,kupc,klpc_x,kupc_x,thr
      
      n = 100
      skew = 0.05
      p = 0.99
      c = 0.4
      klpc = 0.0
      kupc = 0.0
      CALL b17ci(n, skew, p, c, klpc, kupc)
      klpc_x = 2.31428258234954
      kupc_x = 2.41334552509615
      thr = 1.0e-5
      
      call check(error, klpc, klpc_x, 'Problem with klpc', '', thr)
      if (.not.allocated(error)) then
        call check(error, kupc, kupc_x, 'Problem with kupc', '', thr)
      end if 
    end subroutine test_b17ci

    subroutine test_moms_p3(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: n
      double precision :: ql(34),qu(34),rG,mc_old(3),moms(3),thr
      double precision :: nG_orig,nG_ERL,nG_HWN
      double precision :: moms_x_orig(3),moms_x_ERL(3),moms_x_HWN(3)
      
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
      
      ! expected values from tests
      moms_x_orig(1) = 165.5409898
      moms_x_orig(2) = 40060.40734
      moms_x_orig(3) = 1.423055596
      
      moms_x_ERL(1) = 165.5409898
      moms_x_ERL(2) = 40060.40734
      moms_x_ERL(3) = 1.385585543
      
      moms_x_HWN(1) = 165.5409898
      moms_x_HWN(2) = 40060.40734
      moms_x_HWN(3) = 1.449544227
      
      !input values for tests
      n = 34
      ! nG = n*Wd*as_G_mse/r_G_mse
      !orig version Wd = 1
      nG_orig = n * 1.0 * 0.160 / 0.206
      !ERL version Wd = 1.056
      nG_ERL = n * 1.056 * 0.160 / 0.206
      !HWN version Wd = 0.962
      nG_HWN = n * 0.962 * 0.160 / 0.206
      
      ql(1) = 68.0        
      ql(2) = 20.0        
      ql(3) = 345.0       
      ql(4) = 139.0       
      ql(5) = 680.0       
      ql(6) = 970.0      
      ql(7) = 355.0      
      ql(8) =  76.0      
      ql(9) = 110.0     
      ql(10) = 12.0     
      ql(11) = 117.0     
      ql(12) = 102.0     
      ql(13) =  74.0       
      ql(14) =  82.0      
      ql(15) = 221.0    
      ql(16) =  84.0      
      ql(17) =  55.0      
      ql(18) = 117.0    
      ql(19) = 225.0     
      ql(20) =  94.0      
      ql(21) =   0.0     
      ql(22) = 320.0     
      ql(23) =   0.0     
      ql(24) =   0.0       
      ql(25) =  41.0     
      ql(26) =  27.0    
      ql(27) = 205.0      
      ql(28) = 174.0     
      ql(29) = 100.0      
      ql(30) = 108.0     
      ql(31) = 387.0    
      ql(32) =  29.0      
      ql(33) = 111.0     
      ql(34) = 178.0     
      
      qu(1) =  68.0    
      qu(2) =  20.0    
      qu(3) = 345.0    
      qu(4) = 139.0    
      qu(5) = 680.0    
      qu(6) = 970.0    
      qu(7) = 355.0    
      qu(8) =  76.0    
      qu(9) = 110.0    
      qu(10) = 12.0    
      qu(11) = 117.0    
      qu(12) = 102.0    
      qu(13) =  74.0    
      qu(14) =  82.0    
      qu(15) = 221.0    
      qu(16) =  84.0    
      qu(17) =  55.0    
      qu(18) = 117.0    
      qu(19) = 225.0    
      qu(20) =  94.0    
      qu(21) =  40.0    
      qu(22) = 320.0    
      qu(23) =  40.0    
      qu(24) =  40.0    
      qu(25) =  41.0    
      qu(26) =  27.0    
      qu(27) = 205.0    
      qu(28) = 174.0    
      qu(29) = 100.0    
      qu(30) = 108.0    
      qu(31) = 387.0    
      qu(32) =  29.0    
      qu(33) = 111.0    
      qu(34) = 178.0    

      !ql(1) = 1.8325089
      !qu(1) = 1.8325089
      rG = -0.145
      
      
      ! test for orig version
      if (.not.allocated(error)) then
          thr = 1.0e-2
      
          mc_old(1) = 0.0
          mc_old(2) = 1.0
          mc_old(3) = 0.0
      
          moms = 0.0
          CALL moms_p3(n,ql,qu,rG,nG_orig,mc_old,moms)
      
          call check(error, moms(1), moms_x_orig(1), 'Problem with orig moms(1)', '', thr)
          if (.not.allocated(error)) then
            call check(error, moms(2), moms_x_orig(2), 'Problem with orig moms(2)', '', thr)
            if (.not.allocated(error)) then
                call check(error, moms(3), moms_x_orig(3), 'Problem with orig moms(3)', '', thr)
            end if 
          end if      
      end if 
      
    
      ! test for ERL version
      if (.not.allocated(error)) then
          thr = 1.0e-2
      
          mc_old(1) = 0.0
          mc_old(2) = 1.0
          mc_old(3) = 0.0
      
          moms = 0.0
          CALL moms_p3(n,ql,qu,rG,nG_ERL,mc_old,moms)
      
          call check(error, moms(1), moms_x_ERL(1), 'Problem with ERL moms(1)', '', thr)
          if (.not.allocated(error)) then
            call check(error, moms(2), moms_x_ERL(2), 'Problem with ERL moms(2)', '', thr)
            if (.not.allocated(error)) then
                call check(error, moms(3), moms_x_ERL(3), 'Problem with ERL moms(3)', '', thr)
            end if 
          end if      
      end if 
      
      
      ! test for HWN version
      if (.not.allocated(error)) then
          thr = 1.0e-2
      
          mc_old(1) = 0.0
          mc_old(2) = 1.0
          mc_old(3) = 0.0
      
          moms = 0.0
          CALL moms_p3(n,ql,qu,rG,nG_HWN,mc_old,moms)
      
          call check(error, moms(1), moms_x_HWN(1), 'Problem with HWN moms(1)', '', thr)
          if (.not.allocated(error)) then
            call check(error, moms(2), moms_x_HWN(2), 'Problem with HWN moms(2)', '', thr)
            if (.not.allocated(error)) then
                call check(error, moms(3), moms_x_HWN(3), 'Problem with HWN moms(3)', '', thr)
            end if 
          end if      
      end if 
      
    end subroutine test_moms_p3
    
end module test_suite1
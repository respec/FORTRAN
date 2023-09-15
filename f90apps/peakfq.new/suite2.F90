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
        new_unittest("truth_test_moms_p3", truth_test_moms_p3), &
        new_unittest("truth_test_p3est_ema", truth_test_p3est_ema), &
        new_unittest("truth_test_var_mom", truth_test_var_mom) &
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
      
          mc_old(1) = moms(1)
          mc_old(2) = moms(2)
          mc_old(3) = moms(3)
      
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
    
    subroutine truth_test_p3est_ema(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: n, nthresh
      double precision :: ql(250),qu(250),mc_old(3),moms(3)
      double precision :: tl(2),tu(2),nobs(2)
      double precision :: nG
      double precision :: moms_x(3),moms_x_new

      double precision :: as_M_mse,as_S2_mse,as_G_mse
      double precision :: r_M,     r_S2,     r_G
      double precision :: r_M_mse, r_S2_mse, r_G_mse
      double precision :: Wd,cmoms(3),thr
      
      character*4       :: VarS2opt
      integer           :: EMAIterations
      double precision  :: rM,rMmse,rS2,rS2mse,rG,rGmse
      
      double precision  :: mseg_all, detrat
!c
!c       p3est_ema input variables:
!c       ------------------------------------------------
!c            n          i*4  number of observations
!c            ql(n)      r*8  vector of lower bounds 
!c            qu(n)      r*8  vector of upper bounds 
!c            as_M_mse   r*8  mean square error of at-site mean (M)
!c                              this is calculated by mse_ema
!c            as_S2_mse  r*8  mean square error of at-site variance (S2)
!c                              this is calculated by mse_ema
!c            as_G_mse   r*8  mean square error of at-site skew (G)
!c                              this is calculated by mseg_all
!c            r_M        r*8  regional mean (M)
!c            r_S2       r*8  regional variance (std.dev^2) (S2)
!c            r_G        r*8  regional skew (G)
!c            r_M_mse    r*8  mean square error of regional mean
!c            r_S2_mse   r*8  mean square error of regional variance
!c            r_G_mse    r*8  mean square error of regional skew
!c            Wd         r*8  weighting factor for regional skew
!c                            effective record length
!c
!c       output variable: 
!c       ----------------------------------------------------
!c
!c            cmoms(3)   r*8  first 3 central moments 
!c                              mc = {mean, variance, coeff. skew}
!c      
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
      
      !set common block variables
      call set_common_tacR01(VarS2opt)
      call set_common_reg001(rM,rMmse,rS2,rS2mse,rG,rGmse,EMAIterations)
      
      if (.not.allocated(error)) then
          thr = 1.0e-2
          
          Wd = 1.0
          r_G = 0.0          ! supplied by USGS
          as_M_mse = 1.d0    ! turn off
          as_S2_mse = 1.d0   ! turn off
          as_G_mse = 0.d0    ! calc by mseg_all
          r_M = 0.0          ! turn off
          r_S2 = 1.0         ! turn off
          r_M_mse = -99.d0   ! turn off
          r_S2_mse = -99.d0  ! turn off
          r_G_mse = 1.d0     ! supplied by USGS, is a number greater than zero
          
          nthresh = 1
          nobs = n
          tl = -20.
          tu = 20.
          as_G_mse = mseg_all(nthresh,nobs,tl,tu,moms_x)   
                    
          call p3est_ema(n,ql,qu, &
            as_M_mse,as_S2_mse,as_G_mse, &
            r_M,     r_S2,     r_G, &
            r_M_mse, r_S2_mse, r_G_mse, &
            Wd, &
            cmoms)    
          
          moms_x_new = (moms_x(3)*r_G_mse + r_G*as_G_mse)/(r_G_mse + as_G_mse)
      
          call check(error, cmoms(1), moms_x(1), 'Problem with truth p3est_ema cmoms(1)', '', thr)
          if (.not.allocated(error)) then
            call check(error, cmoms(2), moms_x(2), 'Problem with truth p3est_ema cmoms(2)', '', thr)
            if (.not.allocated(error)) then
                call check(error, cmoms(3), moms_x_new, 'Problem with truth p3est_ema cmoms(3)', '', thr)
            end if 
          end if      
      end if  
      
      ! now do advanced test, with 50+ additional values
      tl = -20.
      tu = 20.
      n = 250
      do I=201,250
        ql(I) = -10.0
        qu(I) =  10.0
      end do
      nthresh = 2
      tl(2) = 10. 
      tu(2) = 20.
      nobs(2) = 50.
      
      if (.not.allocated(error)) then         
          ! For uncensored values (ql value is equal to corresponding qu value), set tl = -20 and tu = +20
          ! Wd using detrat() in this manner should produce a value of 1.0
          ! For censored values (where we set ql = -10 and qu = +10), set tl = +10 and tu = +20 
          ! Wd using detrat() in this manner should produce a value very close to 1.0 
          Wd = detrat(moms_x, n, nthresh, nobs, tl, tu)     !0.8???
          
!       input variables:
!       ---------------------------------------------------------------------------
!            mc(3)   r*8     central moments vector
!            n          i*4  number of observations (censored, uncensored, or other)
!            nthresh    i*4  number of distinct censoring thresholds
!            nobs(nthresh)   number of observations in each censoring regime
!            tl(nthresh)     vector of lower censoring thresholds
!            tu(nthresh)     vector of upper censoring thresholds
!
!       output variables:
!       ---------------------------------------------------------------------------
!            detrat      r*8 determinate ratio for skew weighting
          
          r_G = 0.0          ! supplied by USGS
          as_M_mse = 1.d0    ! turn off
          as_S2_mse = 1.d0   ! turn off
          as_G_mse = 0.d0    ! calc by mseg_all
          r_M = 0.0          ! turn off
          r_S2 = 1.0         ! turn off
          r_M_mse = -99.d0   ! turn off
          r_S2_mse = -99.d0  ! turn off
          r_G_mse = 1.d0     ! supplied by USGS, is a number greater than zero

          as_G_mse = mseg_all(nthresh,nobs,tl,tu,moms_x)   
          
          call p3est_ema(n,ql,qu, &
            as_M_mse,as_S2_mse,as_G_mse, &
            r_M,     r_S2,     r_G, &
            r_M_mse, r_S2_mse, r_G_mse, &
            Wd, &
            cmoms)    
          
          moms_x_new = (moms_x(3)*r_G_mse + r_G*as_G_mse)/(r_G_mse + as_G_mse)
      
          call check(error, cmoms(1), moms_x(1), 'Problem with truth censored p3est_ema cmoms(1)', '', thr)
          if (.not.allocated(error)) then
            call check(error, cmoms(2), moms_x(2), 'Problem with truth censored p3est_ema cmoms(2)', '', thr)
            if (.not.allocated(error)) then
                call check(error, cmoms(3), moms_x_new, 'Problem with truth censored p3est_ema cmoms(3)', '', thr)
            end if 
          end if    
      end if 

    end subroutine truth_test_p3est_ema  
    
    subroutine truth_test_var_mom(error)
      type(error_type), allocatable, intent(out) :: error
      
      integer :: nthresh,i,j
      double precision :: tl_in(2),tu_in(2),n_in(2) 
      double precision :: mc_in(3),varm(3,3),x(3),n,thr
      double precision :: mnout(6),xval(3,3)
    
    ! Inputs to var_mom(nthresh,n_in,tl_in,tu_in,mc_in,varm) are as follows:  
    !
    ! nthresh: Number of unique perception thresholds, set to 1 for simple case and 2 for advanced
    ! n_in: Array of number of data values associated with each unique threshold (Set to 200 for simple case and [200, 50?] for advanced case)
    ! tl_in: Array of lower perception thresholds (same as p3est_ema for simple and advanced cases)
    ! tu_in: Array of upper perception thresholds (same as p3est_ema for simple and advanced cases)
    ! mc_in: Array of central moments (Taken from USGS supplied truth values for other tests)

      ! truth vals from csv file
      character(len=8) header(3),dat1
      
      open(unit=50,file="example_truth.csv")
      read(50,*) header
      read(50,*) dat1,mc_in(1),mc_in(2),mc_in(3)
      close(unit=50)
            
      thr = 1.0e-2
      
      if (.not.allocated(error)) then
          !input values for tests
          nthresh = 1
          n_in = 200
          tl_in = -20.
          tu_in = 20.
      
          call var_mom(nthresh,n_in,tl_in,tu_in,mc_in,varm)
      
          ! Variance of mean (first element on diagonal): sigma^2/n
          ! Variance of variance (second element on diagonal): 2*sigma^4/(n-1)
          ! Variance of skew (third element on diagonal): (6/n)*(1 + 9/6*g^2 + 15/48*g^4) (Griffis and others, 2004 EQN with small sample correction terms removed)
          !
          ! Where: 
          ! n = 200 (number of exact observations)
          ! sigma^2 = moms_c_2
          ! g = moms_c_3
          ! (with moms_c_2 and moms_c_3 taken from USGS supplied truth values)
      
          n = 200.
          x(1) = mc_in(2)/n
          x(2) = 2*mc_in(2)**2/(n-1)
          x(3) = (6/n)*(1 + (9.0/6.0*(mc_in(3)**2)) + (15.0/48.0*(mc_in(3)**4)))

          call check(error, varm(1,1), x(1), 'Problem with truth var_mom(1)', '', thr)
          if (.not.allocated(error)) then
              call check(error, varm(2,2), x(2), 'Problem with truth var_mom(2)', '', thr)
              !if (.not.allocated(error)) then
              !    call check(error, varm(3,3), x(3), 'Problem with truth var_mom(3)', '', thr)
              !end if 
          end if      
      end if 
      
      ! now do advanced test, with 50+ additional values
      nthresh = 2
      tl_in(2) = 10. 
      tu_in(2) = 20.
      n_in(2) = 50.
      
      if (.not.allocated(error)) then
      
          call var_mom(nthresh,n_in,tl_in,tu_in,mc_in,varm)
      
          call check(error, varm(1,1), x(1), 'Problem with truth censored var_mom(1)', '', thr)
          if (.not.allocated(error)) then
              call check(error, varm(2,2), x(2), 'Problem with truth censored var_mom(2)', '', thr)
              !if (.not.allocated(error)) then
              !    call check(error, varm(3,3), x(3), 'Problem with truth censored var_mom(3)', '', thr)
              !end if 
          end if      
          
          if (.not.allocated(error)) then 
              !tl = -20
              !tu = +20
              !m = (moms_c_1, moms_c_2, moms_c_3) (Array of USGS supplied truth values)
              !mnout = output
              !n = 6
              mc_in(1) = 0.0
              call mP3(tl_in(1),tu_in(1),mc_in,mnout,6)
              !The expected result for each element (i,j) of the matrix Varm can be computed as mnout(i+j) - mnout(i)*mnout(j). 
              do i=1,3
                  do j = 1,3
                      xval(i,j) = mnout(i+j) - (mnout(i)*mnout(j))
                      xval(i,j) = xval(i,j) / n
                      if (.not.allocated(error)) then 
                          call check(error, varm(i,j), xval(i,j), 'Problem with truth var_mom ', '', thr)
                      end if 
                  end do
              end do
          end if 
      end if 

    end subroutine truth_test_var_mom
    

end module test_suite2   
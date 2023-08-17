!> For an example setup checkout the ``test/`` directory in this project.
module testdrive
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none
  private

  public :: run_testsuite, run_selected, new_unittest, new_testsuite
  public :: select_test, select_suite
  public :: unittest_type, testsuite_type, error_type
  public :: check, test_failed, skip_test ! check_float_dp
  public :: test_interface, collect_interface
  public :: get_argument, get_variable, to_string


  !> Single precision real numbers
  integer, parameter :: sp = selected_real_kind(6)

  !> Double precision real numbers
  integer, parameter :: dp = selected_real_kind(15)

  !> Char length for integers
  integer, parameter :: i1 = selected_int_kind(2)

  !> Short length for integers
  integer, parameter :: i2 = selected_int_kind(4)

  !> Length of default integers
  integer, parameter :: i4 = selected_int_kind(9)

  !> Long length for integers
  integer, parameter :: i8 = selected_int_kind(18)

  !> Error code for success
  integer, parameter :: success = 0

  !> Error code for failure
  integer, parameter :: fatal = 1

  !> Error code for skipped test
  integer, parameter :: skipped = 77


  !> Error message
  type :: error_type

    !> Error code
    integer :: stat = success

    !> Payload of the error
    character(len=:), allocatable :: message

  contains

    !> Escalate uncaught errors
    final :: escalate_error

  end type error_type


  interface check
    module procedure :: check_stat
    module procedure :: check_logical
    module procedure :: check_float_sp
    module procedure :: check_float_dp
    module procedure :: check_float_exceptional_sp
    module procedure :: check_float_exceptional_dp
    module procedure :: check_complex_sp
    module procedure :: check_complex_dp
    module procedure :: check_complex_exceptional_sp
    module procedure :: check_complex_exceptional_dp
    module procedure :: check_int_i1
    module procedure :: check_int_i2
    module procedure :: check_int_i4
    module procedure :: check_int_i8
    module procedure :: check_bool
    module procedure :: check_string
  end interface check


  interface to_string
    module procedure :: integer_i1_to_string
    module procedure :: integer_i2_to_string
    module procedure :: integer_i4_to_string
    module procedure :: integer_i8_to_string
    module procedure :: real_sp_to_string
    module procedure :: real_dp_to_string
    module procedure :: complex_sp_to_string
    module procedure :: complex_dp_to_string
  end interface to_string


  !> Implementation of check for not a number value, in case a compiler does not
  !> provide the IEEE intrinsic ``ieee_is_nan`` (currently this is Intel oneAPI on MacOS)
  interface is_nan
    module procedure :: is_nan_sp
    module procedure :: is_nan_dp
  end interface is_nan


  abstract interface
    !> Entry point for tests
    subroutine test_interface(error)
      import :: error_type

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

    end subroutine test_interface
  end interface


  !> Declaration of a unit test
  type :: unittest_type

    !> Name of the test
    character(len=:), allocatable :: name

    !> Entry point of the test
    procedure(test_interface), pointer, nopass :: test => null()

    !> Whether test is supposed to fail
    logical :: should_fail = .false.

  end type unittest_type


  abstract interface
    !> Collect all tests
    subroutine collect_interface(testsuite)
      import :: unittest_type

      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

    end subroutine collect_interface
  end interface


  !> Collection of unit tests
  type :: testsuite_type

    !> Name of the testsuite
    character(len=:), allocatable :: name

    !> Entry point of the test
    procedure(collect_interface), pointer, nopass :: collect => null()

  end type testsuite_type


  character(len=*), parameter :: fmt = '(1x, *(1x, a))'


contains


  !> Driver for testsuite
  recursive subroutine run_testsuite(collect, unit, stat, parallel)

    !> Collect tests
    procedure(collect_interface) :: collect

    !> Unit for IO
    integer, intent(in) :: unit

    !> Number of failed tests
    integer, intent(inout) :: stat

    !> Run the tests in parallel
    logical, intent(in), optional :: parallel

    type(unittest_type), allocatable :: testsuite(:)
    integer :: it
    logical :: parallel_

    parallel_ = .true.
    if(present(parallel)) parallel_ = parallel

    call collect(testsuite)

    !$omp parallel do schedule(dynamic) shared(testsuite, unit) reduction(+:stat) &
    !$omp if (parallel_)
    do it = 1, size(testsuite)
      !$omp critical(testdrive_testsuite)
      write(unit, '(1x, 3(1x, a), 1x, "(", i0, "/", i0, ")")') &
        & "Starting", testsuite(it)%name, "...", it, size(testsuite)
      !$omp end critical(testdrive_testsuite)
      call run_unittest(testsuite(it), unit, stat)
    end do

  end subroutine run_testsuite


  !> Driver for selective testing
  recursive subroutine run_selected(collect, name, unit, stat)

    !> Collect tests
    procedure(collect_interface) :: collect

    !> Name of the selected test
    character(len=*), intent(in) :: name

    !> Unit for IO
    integer, intent(in) :: unit

    !> Number of failed tests
    integer, intent(inout) :: stat

    type(unittest_type), allocatable :: testsuite(:)
    integer :: it

    call collect(testsuite)

    it = select_test(testsuite, name)

    if (it > 0 .and. it <= size(testsuite)) then
      call run_unittest(testsuite(it), unit, stat)
    else
      write(unit, fmt) "Available tests:"
      do it = 1, size(testsuite)
        write(unit, fmt) "-", testsuite(it)%name
      end do
      stat = -huge(it)
    end if

  end subroutine run_selected


  !> Run a selected unit test
  recursive subroutine run_unittest(test, unit, stat)

    !> Unit test
    type(unittest_type), intent(in) :: test

    !> Unit for IO
    integer, intent(in) :: unit

    !> Number of failed tests
    integer, intent(inout) :: stat

    type(error_type), allocatable :: error
    character(len=:), allocatable :: message

    call test%test(error)
    if (.not.test_skipped(error)) then
      if (allocated(error) .neqv. test%should_fail) stat = stat + 1
    end if
    call make_output(message, test, error)
    !$omp critical(testdrive_testsuite)
    write(unit, '(a)') message
    !$omp end critical(testdrive_testsuite)
    if (allocated(error)) then
      call clear_error(error)
    end if

  end subroutine run_unittest


  pure function test_skipped(error) result(is_skipped)

    !> Error handling
    type(error_type), intent(in), optional :: error

    !> Test was skipped
    logical :: is_skipped

    is_skipped = .false.
    if (present(error)) then
      is_skipped = error%stat == skipped
    end if

  end function test_skipped


  !> Create output message for test (this procedure is pure and therefore cannot launch tests)
  pure subroutine make_output(output, test, error)

    !> Output message for display
    character(len=:), allocatable, intent(out) :: output

    !> Unit test
    type(unittest_type), intent(in) :: test

    !> Error handling
    type(error_type), intent(in), optional :: error

    character(len=:), allocatable :: label
    character(len=*), parameter :: indent = repeat(" ", 7) // repeat(".", 3) // " "

    if (test_skipped(error)) then
      output = indent // test%name // " [SKIPPED]" &
        & // new_line("a") // "  Message: " // error%message
      return
    end if

    if (present(error) .neqv. test%should_fail) then
      if (test%should_fail) then
        label = " [UNEXPECTED PASS]"
      else
        label = " [FAILED]"
      end if
    else
      if (test%should_fail) then
        label = " [EXPECTED FAIL]"
      else
        label = " [PASSED]"
      end if
    end if
    output = indent // test%name // label
    if (present(error)) then
      output = output // new_line("a") // "  Message: " // error%message
    end if
  end subroutine make_output


  !> Select a unit test from all available tests
  function select_test(tests, name) result(pos)

    !> Name identifying the test suite
    character(len=*), intent(in) :: name

    !> Available unit tests
    type(unittest_type) :: tests(:)

    !> Selected test suite
    integer :: pos

    integer :: it

    pos = 0
    do it = 1, size(tests)
      if (name == tests(it)%name) then
        pos = it
        exit
      end if
    end do

  end function select_test


  !> Select a test suite from all available suites
  function select_suite(suites, name) result(pos)

    !> Name identifying the test suite
    character(len=*), intent(in) :: name

    !> Available test suites
    type(testsuite_type) :: suites(:)

    !> Selected test suite
    integer :: pos

    integer :: it

    pos = 0
    do it = 1, size(suites)
      if (name == suites(it)%name) then
        pos = it
        exit
      end if
    end do

  end function select_suite


  !> Register a new unit test
  function new_unittest(name, test, should_fail) result(self)

    !> Name of the test
    character(len=*), intent(in) :: name

    !> Entry point for the test
    procedure(test_interface) :: test

    !> Whether test is supposed to error or not
    logical, intent(in), optional :: should_fail

    !> Newly registered test
    type(unittest_type) :: self

    self%name = name
    self%test => test
    if (present(should_fail)) self%should_fail = should_fail

  end function new_unittest


  !> Register a new testsuite
  function new_testsuite(name, collect) result(self)

    !> Name of the testsuite
    character(len=*), intent(in) :: name

    !> Entry point to collect tests
    procedure(collect_interface) :: collect

    !> Newly registered testsuite
    type(testsuite_type) :: self

    self%name = name
    self%collect => collect

  end function new_testsuite


  subroutine check_stat(error, stat, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Status of operation
    integer, intent(in) :: stat

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (stat /= success) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Non-zero exit code encountered", more)
      end if
    end if

  end subroutine check_stat


  subroutine check_logical(error, expression, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Result of logical operator
    logical, intent(in) :: expression

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (.not.expression) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Condition not fullfilled", more)
      end if
    end if

  end subroutine check_logical


  subroutine check_float_dp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(dp), intent(in) :: actual

    !> Expected floating point value
    real(dp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(dp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(dp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(expected)
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_float_dp


  subroutine check_float_exceptional_dp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(dp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(actual)) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_float_exceptional_dp


  subroutine check_float_sp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(sp), intent(in) :: actual

    !> Expected floating point value
    real(sp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(sp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(sp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(expected)
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_float_sp


  subroutine check_float_exceptional_sp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(sp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(actual)) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_float_exceptional_sp



  subroutine check_complex_dp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(dp), intent(in) :: actual

    !> Expected floating point value
    complex(dp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(dp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(dp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(abs(expected))
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_complex_dp


  subroutine check_complex_exceptional_dp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(dp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(real(actual)) .or. is_nan(aimag(actual))) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_complex_exceptional_dp


  subroutine check_complex_sp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(sp), intent(in) :: actual

    !> Expected floating point value
    complex(sp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(sp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(sp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(abs(expected))
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value missmatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_complex_sp


  subroutine check_complex_exceptional_sp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(sp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(real(actual)) .or. is_nan(aimag(actual))) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_complex_exceptional_sp



  subroutine check_int_i1(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i1), intent(in) :: actual

    !> Expected integer value
    integer(i1), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value missmatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i1


  subroutine check_int_i2(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i2), intent(in) :: actual

    !> Expected integer value
    integer(i2), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value missmatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i2


  subroutine check_int_i4(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i4), intent(in) :: actual

    !> Expected integer value
    integer(i4), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value missmatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i4


  subroutine check_int_i8(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i8), intent(in) :: actual

    !> Expected integer value
    integer(i8), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value missmatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i8


  subroutine check_bool(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found boolean value
    logical, intent(in) :: actual

    !> Expected boolean value
    logical, intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected .neqv. actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Logical value missmatch", &
          "expected "//merge("T", "F", expected)//" but got "//merge("T", "F", actual), &
          more)
      end if
    end if

  end subroutine check_bool


  subroutine check_string(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found boolean value
    character(len=*), intent(in) :: actual

    !> Expected boolean value
    character(len=*), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Character value missmatch", &
          "expected '"//expected//"' but got '"//actual//"'", &
          more)
      end if
    end if

  end subroutine check_string


  subroutine test_failed(error, message, more, and_more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> A detailed message describing the error
    character(len=*), intent(in) :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Another line of error message
    character(len=*), intent(in), optional :: and_more

    character(len=*), parameter :: skip = new_line("a") // repeat(" ", 11)

    allocate(error)
    error%stat = fatal

    error%message = message
    if (present(more)) then
      error%message = error%message // skip // more
    end if
    if (present(and_more)) then
      error%message = error%message // skip // and_more
    end if

  end subroutine test_failed


  !> A test is skipped because certain requirements are not met to run the actual test
  subroutine skip_test(error, message, more, and_more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> A detailed message describing the error
    character(len=*), intent(in) :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Another line of error message
    character(len=*), intent(in), optional :: and_more

    call test_failed(error, message, more, and_more)
    error%stat = skipped

  end subroutine skip_test


  !> Obtain the command line argument at a given index
  subroutine get_argument(idx, arg)

    !> Index of command line argument, range [0:command_argument_count()]
    integer, intent(in) :: idx

    !> Command line argument
    character(len=:), allocatable, intent(out) :: arg

    integer :: length, stat

    call get_command_argument(idx, length=length, status=stat)
    if (stat /= success) return

    allocate(character(len=length) :: arg, stat=stat)
    if (stat /= success) return

    if (length > 0) then
      call get_command_argument(idx, arg, status=stat)
      if (stat /= success) deallocate(arg)
    end if

  end subroutine get_argument


  !> Obtain the value of an environment variable
  subroutine get_variable(var, val)

    !> Name of variable
    character(len=*), intent(in) :: var

    !> Value of variable
    character(len=:), allocatable, intent(out) :: val

    integer :: length, stat

    call get_environment_variable(var, length=length, status=stat)
    if (stat /= success) return

    allocate(character(len=length) :: val, stat=stat)
    if (stat /= success) return

    if (length > 0) then
      call get_environment_variable(var, val, status=stat)
      if (stat /= success) deallocate(val)
    end if

  end subroutine get_variable


  pure function integer_i1_to_string(val) result(string)
    integer, parameter :: ik = i1
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i1_to_string


  pure function integer_i2_to_string(val) result(string)
    integer, parameter :: ik = i2
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i2_to_string


  pure function integer_i4_to_string(val) result(string)
    integer, parameter :: ik = i4
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i4_to_string


  pure function integer_i8_to_string(val) result(string)
    integer, parameter :: ik = i8
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i8_to_string


  pure function real_sp_to_string(val) result(string)
    real(sp), intent(in) :: val
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer

    write(buffer, '(g0)') val
    string = trim(buffer)

  end function real_sp_to_string


  pure function real_dp_to_string(val) result(string)
    real(dp), intent(in) :: val
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer

    write(buffer, '(g0)') val
    string = trim(buffer)

  end function real_dp_to_string


  pure function complex_sp_to_string(val) result(string)
    complex(sp), intent(in) :: val
    character(len=:), allocatable :: string

    string = "("//to_string(real(val))//", "//to_string(aimag(val))//")"

  end function complex_sp_to_string


  pure function complex_dp_to_string(val) result(string)
    complex(dp), intent(in) :: val
    character(len=:), allocatable :: string

    string = "("//to_string(real(val))//", "//to_string(aimag(val))//")"

  end function complex_dp_to_string


  !> Clear error type after it has been handled.
  subroutine clear_error(error)

    !> Error handling
    type(error_type), intent(inout) :: error

    if (error%stat /= success) then
      error%stat = success
    end if

    if (allocated(error%message)) then
      deallocate(error%message)
    end if

  end subroutine clear_error


  !> Finalizer of the error type, in case the error is not correctly cleared it will
  !> be escalated at runtime in a fatal way
  subroutine escalate_error(error)

    !> Error handling
    type(error_type), intent(inout) :: error

    if (error%stat /= success) then
      write(error_unit, '(a)') "[Fatal] Uncaught error"
      if (allocated(error%message)) then
        write(error_unit, '(a, 1x, i0, *(1x, a))') &
          "Code:", error%stat, "Message:", error%message
      end if
      error stop
    end if

  end subroutine escalate_error


  !> Determine whether a value is not a number without requiring IEEE arithmetic support
  elemental function is_nan_sp(val) result(is_nan)
    !> Value to check
    real(sp), intent(in) :: val
    !> Value is not a number
    logical :: is_nan

    is_nan = .not.((val <= huge(val) .and. val >= -huge(val)) .or. abs(val) > huge(val))
  end function is_nan_sp

  !> Determine whether a value is not a number without requiring IEEE arithmetic support
  elemental function is_nan_dp(val) result(is_nan)
    !> Value to check
    real(dp), intent(in) :: val
    !> Value is not a number
    logical :: is_nan

    is_nan = .not.((val <= huge(val) .and. val >= -huge(val)) .or. abs(val) > huge(val))
  end function is_nan_dp


end module testdrive   
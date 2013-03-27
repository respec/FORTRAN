!include 'macros.f95'
module modDateTime
!-----------------------------------------------------------------------------
!   datetime.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     6/19/07   (Build 5.0.010)
!   Author:   L. Rossman
!
!   double precision functions.
!-----------------------------------------------------------------------------

!typedef double DateTime;

integer, parameter :: Y_M_D = 0
integer, parameter :: M_D_Y = 1
integer, parameter :: D_M_Y = 2
integer, parameter :: NO_DATE = -693594 ! 1/1/0001
integer, parameter :: DATE_STR_SIZE = 12
integer, parameter :: TIME_STR_SIZE = 9

!-----------------------------------------------------------------------------
!  Constants
!-----------------------------------------------------------------------------
character(len=3), dimension(12), parameter :: MonthTxt = &
    &(/ 'JAN', 'FEB', 'MAR', 'APR', &
    &'MAY', 'JUN', 'JUL', 'AUG', &
    &'SEP', 'OCT', 'NOV', 'DEC' /)
integer, dimension(12, 2), parameter :: DaysPerMonth = &     ! days per month
   &reshape((/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &             ! normal years
             &31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/), &             ! leap years
             & (/12, 2/))
integer, parameter :: DateDelta = 693594        ! days since 01/01/00
double precision, parameter :: SecsPerDay = 86400.    ! seconds per day

!-----------------------------------------------------------------------------
!  Shared variables
!-----------------------------------------------------------------------------
integer :: DateFormat
save
contains
!=============================================================================
! Macro to convert charcter x to upper case
!#define UCHAR(x) (((x) >= 'a' && (x) <= 'z') ? ((x)&~32) : (x))

subroutine divMod( n,  d,  aResult,  remainder)

!  Input:   n = numerator
!           d = denominator
!  Output:  result = integer part of n/d
!           remainder = remainder of n/d
!  Purpose: finds integer part and remainder of n/d.

    implicit none
    integer, intent(in) :: n, d
    integer, intent(inout) :: aResult, remainder
    
    if (d == 0) then
        aResult = 0
        remainder = 0
    else
        aResult = n/d
        remainder = n - d*aResult
    end if
end subroutine divMod

!=============================================================================

integer function isLeapYear(year)

!  Input:   year = a year
!  Output:  returns 1 if year is a leap year, 0 if not
!  Purpose: determines if year is a leap year.

    implicit none
    integer, intent(in) :: year
    if ((mod(year, 4)   == 0) .and. &
      &((mod(year, 100) /= 0) .or. &
      & (mod(year, 400) == 0))) then
      isLeapYear = 1
    else
      isLeapYear = 0
    end if
end function isLeapYear

!=============================================================================

integer function datetime_findMonth(month)

!  Input:   month = month of year as character string
!  Output:  returns: month of year as a number (1-12)
!  Purpose: finds number (1-12) of month.

    use macros
    implicit none
    character(*), intent(in) :: month
    integer :: i
    do i = 1, 12
        if (UCHAR(month(1:1)) == MonthTxt(i)(1:1) .and. &
           &UCHAR(month(2:2)) == MonthTxt(i)(2:2) .and. &
           &UCHAR(month(3:3)) == MonthTxt(i)(3:3)) then
           datetime_findMonth = i
           return
        end if
    end do
    datetime_findMonth = 0
    return
end function datetime_findMonth

!=============================================================================

double precision function datetime_encodeDate( year,  month,  day)

!  Input:   year = a year
!           month = a month (1 to 12)
!           day = a day of month
!  Output:  returns encoded value of year-month-day
!  Purpose: encodes year-month-day to a double precision value.

    implicit none
    integer, intent(in) :: year, month, day
    integer :: i, j, mday
    mday = day
    i = isLeapYear(year)
    i = i + 1
    if ((year >= 1) .and. &
    & (year <= 9999) .and. &
    & (month >= 1) .and. &
    & (month <= 12) .and. &
    & (mday >= 1) .and. &
    & (mday <= DaysPerMonth(month, i))) then
        do j = 1, month
           mday = mday + DaysPerMonth(j, i)
        end do
        i = year - 1
        datetime_encodeDate = i*365 + i/4 - i/100 + i/400 + mday - DateDelta
    else
        datetime_encodeDate = -DateDelta
    end if
    return
end function datetime_encodeDate

!=============================================================================

double precision function datetime_encodeTime( hour,  minute,  second)

!  Input:   hour = hour of day (0-24)
!           minute = minute of hour (0-60)
!           second = seconds of minute (0-60)
!  Output:  returns time encoded as fractional part of a day
!  Purpose: encodes hour:minute:second to a double precision value

    implicit none
    integer, intent(in) :: hour, minute, second
    integer :: s
    if ((hour >= 0) .and. &
    & (minute >= 0) .and. &
    & (second >= 0)) then
        s = (hour * 3600 + minute * 60 + second)
        datetime_encodeTime = s/SecsPerDay
    else
        datetime_encodeTime = 0.0
    end if
end function datetime_encodeTime

!=============================================================================

subroutine datetime_decodeDate(date, year, month, day)

!  Input:   date = encoded date/time value
!  Output:  year = 4-digit year
!           month = month of year (1-12)
!           day   = day of month
!  Purpose: decodes double precision value to year-month-day.

    implicit none
    double precision, intent(in) :: date
    integer, intent(inout) :: year, month, day
    
    integer ::  D1, D4, D100, D400
    integer ::  y, m, d, i, k, t

    D1 = 365              !365
    D4 = D1 * 4 + 1       !1461
    D100 = D4 * 25 - 1    !36524
    D400 = D100 * 4 + 1   !146097

    t = int(floor(date)) + DateDelta
    if (t <= 0) then
        year = 0
        month = 1
        day = 1
    else
        t = t - 1
        y = 1
        do while (t >= D400)
            t = t - D400
            y = y + 400
        end do
        call divMod(t, D100, i, d)
        if (i == 4) then
            i = i - 1
            d = d + D100
        end if
        y = y + i*100
        call divMod(d, D4, i, d)
        y = y + i*4
        call divMod(d, D1, i, d)
        if (i == 4) then
            i = i - 1
            d = d + D1
        end if
        y = y + i
        k = isLeapYear(y) !returns either 1 or 0
        k = k + 1 !fortran array subscript starts from 1
        m = 1
        do while (.true.)
            i = DaysPerMonth(m, k)
            if (d < i) exit !break
            d = d - i
            m = m + 1
        end do
        year = y
        month = m
        day = d + 1
    end if
end subroutine datetime_decodeDate

!=============================================================================

subroutine datetime_decodeTime(time, h, m, s)

!  Input:   time = decimal fraction of a day
!  Output:  h = hour of day (0-24)
!           m = minute of hour (0-60)
!           s = second of minute (0-60)
!  Purpose: decodes double precision value to hour:minute:second.

    implicit none
    double precision, intent(in) :: time
    integer, intent(inout) :: h, m, s
    integer :: secs
    integer :: mins
    secs = int(floor((time - floor(time))*SecsPerDay + 0.5))
    call divMod(secs, 60, mins, s)
    call divMod(mins, 60, h, m)
end subroutine datetime_decodeTime

!=============================================================================

subroutine datetime_dateToStr(date, s)

!  Input:   date = encoded date/time value
!  Output:  s = formatted date string
!  Purpose: represents double precision date value as a formatted string.

    implicit none
    double precision, intent(in) :: date
    character(*), intent(inout) :: s
    integer ::  y, m, d
    character(len=DATE_STR_SIZE) :: dateStr
    character(4) :: StrYear
    character(2) :: StrDay
    character(3) :: StrMonth
    call datetime_decodeDate(date, y, m, d)
    write(StrYear, '(i4)') y
100 format('0',(i1))

    if (d < 10) then
        write(StrDay, 100) d
    else
        write(StrDay, '(i2)') d
    end if
    StrMonth = MonthTxt(m)
    select case (DateFormat)
      case (Y_M_D)
        !sprintf(dateStr, "%4d-%3s-%02d", y, MonthTxt(m-1), d)
        dateStr = StrYear//'-'//StrMonth//'-'//StrDay
        !break
      case (M_D_Y)
        !sprintf(dateStr, "%3s-%02d-%4d", MonthTxt(m-1), d, y)
        dateStr = StrMonth//'-'//StrDay//'-'//StrYear
        !break
      case default
        !sprintf(dateStr, "%02d-%3s-%4d", d, MonthTxt(m-1), y)
        dateStr = StrDay//'-'//StrMonth//'-'//StrYear
    end select
    s = dateStr
end subroutine datetime_dateToStr

!=============================================================================

subroutine datetime_timeToStr(time, s)

!  Input:   time = decimal fraction of a day
!  Output:  s = time in hr:min:sec format
!  Purpose: represents double precision time value as a formatted string.

    implicit none
    double precision, intent(in) :: time
    character(*), intent(inout) :: s
    integer ::  hr, min, sec
    character(len=TIME_STR_SIZE) :: timeStr
    character(2) :: StrHr, StrMin, StrSec
    
    call datetime_decodeTime(time, hr, min, sec)
100 format('0',(i1))    
    if (hr < 10) then
       write(StrHr, 100) hr
    else
       write(StrHr, '(i2)') hr
    end if
    
    if (min < 10) then
       write(StrMin, 100) min
    else
       write(StrMin, '(i2)') min
    end if

    if (sec < 10) then
       write(StrSec, 100) sec
    else
       write(StrSec, '(i2)') sec
    end if

!    sprintf(timeStr, "%02d:%02d:%02d", hr, min, sec)
!    strcpy(s, timeStr)
!
    timeStr = StrHr//':'//StrMin//':'//StrSec
    s = timeStr
end subroutine datetime_timeToStr

!=============================================================================

integer function datetime_strToDate(s, d)

!  Input:   s = date as string
!  Output:  d = encoded date
!           returns 1 if conversion successful, 0 if not
!  Purpose: converts string date s to double precision value.
!
    implicit none
    character(*), intent(in) :: s
    double precision, intent(inout) :: d
    
    integer ::  yr, mon, day, n, stat
    character(4) :: month
    character(1) :: sep1, sep2

    yr = 0
    mon = 0
    day = 0
    d = -DateDelta
    if (scan(s, '-') > 0 .or. scan(s, '/') > 0) then
        select case (DateFormat)
          case (Y_M_D)
            !n = sscanf(s, "%d%c%d%c%d", &yr, &sep1, &mon, &sep2, &day)
            read(s, fmt=200, iostat=stat) yr, sep1, mon, sep2, day
200 format(i4,a1,i2,a1,i2) !1999[-/]07[-/]02
            if ( stat /= 0 ) then
                mon = 0
                !n = sscanf(s, "%d%c%3s%c%d", &yr, &sep1, month, &sep2, &day)
                read(s, fmt=201, iostat=stat) yr, sep1, month, sep2, day
201 format(i4,a1,a3,a1,i2) !1999[-/]JUL[-/]02
                if ( stat /= 0 ) then
                    datetime_strToDate = 0 !means failure or false
                    return
                end if
            end if
            !break

          case (D_M_Y)
            !n = sscanf(s, "%d%c%d%c%d", &day, &sep1, &mon, &sep2, &yr)
            read(s, fmt=202, iostat=stat) day, sep1, mon, sep2, yr
202 format(i2,a1,i2,a1,i4) !02[-/]07[-/]1999
            if ( stat /= 0 ) then
                mon = 0
                !n = sscanf(s, "%d%c%3s%c%d", &day, &sep1, month, &sep2, &yr)
                read(s, fmt=203, iostat=stat) day, sep1, month, sep2, yr
203 format(i2,a1,a3,a1,i4) !02[-/]JUL[-/]1999
                if ( stat /= 0 ) then
                    datetime_strToDate = 0 !means failure or false
                    return
                end if
            end if
            !break

          case default ! M_D_Y
            !n = sscanf(s, "%d%c%d%c%d", &mon, &sep1, &day, &sep2, &yr)
            read(s, fmt=202, iostat=stat) mon, sep1, day, sep2, yr !07[-/]02[-/]1999

            if ( stat /= 0 ) then
                mon = 0
                !n = sscanf(s, "%3s%c%d%c%d", month, &sep1, &day, &sep2, &yr)
                read(s, fmt=204, iostat=stat) month, sep1, day, sep2, yr
204 format(a3,a1,i2,a1,i4) !JUL[-/]02[-/]1999
                if ( stat /= 0 ) then
                    datetime_strToDate = 0 !means failure or false
                    return
                end if
            end if
        end select
        if (mon == 0) mon = datetime_findMonth(month)
        d = datetime_encodeDate(yr, mon, day)
    end if
    if (d == -DateDelta) then
       datetime_strToDate = 0
    else
       datetime_strToDate = 1
    end if
    return
end function datetime_strToDate

!=============================================================================

integer function datetime_strToTime(s, t)

!  Input:   s = time as string
!  Output:  t = encoded time,
!           returns 1 if conversion successful, 0 if not
!  Purpose: converts a string time to a double precision value.

    implicit none
    character(*), intent(in) :: s
    double precision, intent(inout) :: t
    
    integer ::  n, hr, min, sec, stat
    min = 0
    sec = 0
    
    t = 0.0
    !n = sscanf(s, "%d:%d:%d", &hr, &min, &sec)
    read(s, fmt=205, iostat=stat) hr, min, sec
205 format(i2,a1,i2,a1,i2) !01:01:01
    !if ( n == 0 ) return 0
    if (stat /=0 ) then
       datetime_strToTime = 0
       return
    end if
    t = datetime_encodeTime(hr, min, sec)
    if ( (hr >= 0) .and. (min >= 0) .and. (sec >= 0) ) then
       datetime_strToTime = 1
    else
       datetime_strToTime = 0
    end if
    return
end function datetime_strToTime

!=============================================================================

subroutine datetime_setDateFormat(fmt)

!  Input:   fmt = date format code
!  Output:  none
!  Purpose: sets date format

    implicit none
    integer, intent(in) :: fmt
    if ( fmt >= Y_M_D .and. fmt <= M_D_Y) DateFormat = fmt
end subroutine datetime_setDateFormat

!=============================================================================

double precision function datetime_addSeconds(date1, seconds)

!  Input:   date1 = an encoded date/time value
!           seconds = number of seconds to add to date1
!  Output:  returns updated value of date1
!  Purpose: adds a given number of seconds to a date/time.

    implicit none
    double precision, intent(in) :: date1, seconds
    integer :: h, m, s
    double precision :: d
    d = floor(date1)
    call datetime_decodeTime(date1, h, m, s)
    datetime_addSeconds = d + (3600.0*h + 60.0*m + s + seconds)/SecsPerDay
    return
end function datetime_addSeconds

!=============================================================================

double precision function datetime_addDays(date1, date2)

!  Input:   date1 = an encoded date/time value
!           date2 = decimal days to be added to date1
!  Output:  returns date1 + date2
!  Purpose: adds a given number of decimal days to a date/time.

    implicit none
    double precision, intent(in) :: date1, date2
    double precision :: d1, d2
    integer :: h1, m1, s1
    integer :: h2, m2, s2
    
    d1 = floor(date1)
    d2 = floor(date2)

    call datetime_decodeTime(date1, h1, m1, s1)
    call datetime_decodeTime(date2, h2, m2, s2)
    datetime_addDays = d1 + d2 + datetime_encodeTime(h1+h2, m1+m2, s1+s2)
    return
end function datetime_addDays

!=============================================================================

integer(kind=SELECTED_INT_KIND(18)) function datetime_timeDiff( date1, date2)

!  Input:   date1 = an encoded date/time value
!           date2 = an encoded date/time value
!  Output:  returns date1 - date2 in seconds
!  Purpose: finds number of seconds between two dates.

    implicit none
    double precision, intent(in) :: date1, date2
    double precision :: d1, d2
    integer ::    h, m, s
    integer(kind=SELECTED_INT_KIND(18)) ::   s1, s2, secs
    d1 = floor(date1)
    d2 = floor(date2)

    call datetime_decodeTime(date1, h, m, s)
    s1 = 3600*h + 60*m + s
    call datetime_decodeTime(date2, h, m, s)
    s2 = 3600*h + 60*m + s
    secs = int(floor((d1 - d2)*SecsPerDay + 0.5))
    secs = secs + (s1 - s2)
    datetime_timeDiff = secs
    return
end function datetime_timeDiff

!=============================================================================

integer function datetime_monthOfYear(date)

!  Input:   date = an encoded date/time value
!  Output:  returns index of month of year (1..12)
!  Purpose: finds month of year (Jan = 1 ...) for a given date.

    implicit none
    double precision, intent(in) :: date
    integer :: year, month, day
    call datetime_decodeDate(date, year, month, day)
    datetime_monthOfYear = month
end function datetime_monthOfYear

!=============================================================================

integer function datetime_dayOfYear(date)

!  Input:   date = an encoded date/time value
!  Output:  returns day of year (1..365)
!  Purpose: finds day of year (Jan 1 = 1) for a given date.

    implicit none
    double precision, intent(in) :: date
    integer :: year, month, day
    double precision :: startOfYear
    call datetime_decodeDate(date, year, month, day)
    startOfYear = datetime_encodeDate(year, 1, 1)
    datetime_dayOfYear = int(floor(date - startOfYear)) + 1
end function datetime_dayOfYear

!=============================================================================

integer function datetime_dayOfWeek(date)

!  Input:   date = an encoded date/time value
!  Output:  returns index of day of week (1..7)
!  Purpose: finds day of week (Sun = 1, ... Sat = 7) for a given date.

    implicit none
    double precision, intent(in) :: date
    integer :: t
    t = int(floor(date)) + DateDelta
    datetime_dayOfWeek = mod(t, 7) + 1
end function datetime_dayOfWeek

!=============================================================================

integer function datetime_hourOfDay(date)

!  Input:   date = an encoded date/time value
!  Output:  returns hour of day (0..23)
!  Purpose: finds hour of day (0 = 12 AM, ..., 23 = 11 PM) for a given date.

    implicit none
    double precision, intent(in) :: date
    integer :: hour, min, sec
    call datetime_decodeTime(date, hour, min, sec)
    datetime_hourOfDay = hour
end function datetime_hourOfDay

!=============================================================================

integer function datetime_daysPerMonth( year,  month)

!  Input:   year = year in which month falls
!           month = month of year (1..12)
!  Output:  returns number of days in the month
!  Purpose: finds number of days in a given month of a specified year.

    implicit none
    integer, intent(in) :: year, month
    integer :: i
    if ( month < 1 .or. month > 12 ) then
       datetime_daysPerMonth = 0
    else
       i = isLeapYear(year)
       i = i + 1
       datetime_daysPerMonth = DaysPerMonth(month, i)
    end if
end function datetime_daysPerMonth

!=============================================================================
!this is to replace the getDateTime routine in the swmm5f.f95
!aStartDateTime is the StartDateTime global
!  Input:   elapsedSec = elapsed seconds
!  Output:  returns date/time value
!  Purpose: finds calendar date/time value for elapsed milliseconds of
!           simulation time.

double precision function getDateTime(aStartDateTime, elapsedSec)
     implicit none
     double precision, intent(in) :: aStartDateTime, elapsedSec
     getDateTime = datetime_addSeconds(aStartDateTime, elapsedSec/1000.0)
end function getDateTime

end module modDateTime

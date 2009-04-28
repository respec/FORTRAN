      subroutine readwgn

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is retained only to set up certain solar radiation variables

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sub_lat(:)  |degrees       |latitude of HRU/subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    daylmn(:)   |hr            |shortest daylength occurring during the year
!!    latcos(:)   |none          |Cos(Latitude)
!!    latsin(:)   |none          |Sin(Latitude)

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real :: xx, lattan, x1, x2, x3
      real :: summx_t, summn_t, summm_p, sum, rnm2, r6, xlv, pcp
!!      real, dimension (12) :: pcpd
      integer :: mon, mdays, j, m1, nda, xrnd

!! variables needed for radiation calcs.
      xx = 0.0
      lattan = 0.0
      x1 = 0.0
      x2 = 0.0
      if (sub_lat(i) < 1.e-4) sub_lat(i) = wlat(i)
      xx = sub_lat(i) / 57.296 
                          !!convert degrees to radians (2pi/360=1/57.296)
      latsin(i) = Sin(xx)
      latcos(i) = Cos(xx)
      lattan = Tan(xx)
!! calculate minimum daylength 
!! daylength=2*acos(-tan(sd)*tan(lat))/omega
!! where solar declination, sd, = -23.5 degrees for minimum daylength in
!!                      northern hemisphere and -tan(sd) = .4348
!!       absolute value is taken of tan(lat) to convert southern hemisphere
!!                      values to northern hemisphere
!!       the angular velocity of the earth's rotation, omega, = 15 deg/hr or
!!                      0.2618 rad/hr and 2/0.2618 = 7.6394
!      x1 = .4348 * Abs(lattan)      
!      if (x1 < 1.) x2 = Acos(x1) 
!                         !!x1 will be >= 1. if sub_lat > 66.5 or < -66.5
!      daylmn(i) = 7.6394 * x2
      
!! variables needed for RH generation (need pr_w(3

!      summx_t = 0.
!      summn_t = 0.
!      summm_p = 0.
!      tmin = 100.
!      tmax = 0.
      pcpdays(i) = 0.
      do mon = 1, 12
        mdays = 0
        tav = 0.
        mdays = ndays(mon+1) - ndays(mon)


        !! calculate values for pr_w if missing or bad
        if (pr_w(2,mon,i) <= pr_w(1,mon,i).or.pr_w(1,mon,i) <= 0.) then
          if (pcpd(mon) < .1) pcpd(mon) = 0.1
          pr_w(1,mon,i) = .75 * pcpd(mon) / mdays
          pr_w(2,mon,i) = .25 + pr_w(1,mon,i)
        else
        !! if pr_w values good, use calculated pcpd based on these values
        !! using first order Markov chain
        pcpd(mon) = mdays * pr_w(1,mon,i) /                             &
     &                              (1. - pr_w(2,mon,i) + pr_w(1,mon,i))
    
        end if

        !! calculate precipitation-related values
        if (pcpd(mon) <= 0.) pcpd(mon) = .001
        pr_w(3,mon,i) = pcpd(mon) / mdays
        pcpdays(i) = pcpdays(i) + pcpd(mon)
      end do


      return
      end

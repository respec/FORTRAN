      subroutine sunmap(r5smap,halfdy,dsunmp)

c
c     +++PURPOSE+++
c     This routine determines the amount of heat unit calories
c     available for melting snow.  The logic as well as the variable
c     comments are taken from "Computational Algorithm for
c     Solar Radiation on Mountain Slopes" paper written by
c     Lloyd W. Swift Jr. and R.J. Luxmoore, 06-73.  The value is then
c     distributed over the daily radiation curve and passed to the
c     weather data structure as HRADMJ.
c
c     Author(s): Cully Hession and Bruce Lucord, USDA-ARS-NCSRL
c                Revised by John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 03/24/93

c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c******************************************************************
c                                                                 *
c     +++ARGUMENT DECLARATIONS+++                                 *
c                                                                 *
c******************************************************************
      real     halfdy,r5smap,dsunmp

c     +++ARGUMENT DEFINITIONS+++
c     r2     - Measured daily solar radiation from cligen file (Ly/day).
c     r3     - Potential solar beam on horizontal surface (MJ/m^2/day).
c     halfdy - Number of hours from sunrise until noon.
c     sdate  - Julian date that is being run.
c
c******************************************************************
c                                                                 *
c     + + + PARAMETERS + + +                                      *
c                                                                 *
c******************************************************************
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxelm.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
c******************************************************************
c                                                                 *
c   Common Blocks                                                 *
c                                                                 *
c******************************************************************
      include 'cupdate.inc'
      include 'cstruc.inc'
c
      include  'cangie.inc'
c     read:    eqlat,delong,radlat,radpot
c
      include  'cclim.inc'
c     read:    rpoth,hradmj
c******************************************************************
c                                                                 *
c     +++LOCAL VARIABLES+++                                       *
c                                                                 *
c******************************************************************
      real   r2, r3
      real   d,e,r1,solcon,t,t7,t6,x,t1,t0,t3,t2,r4,pi,t4,
     1       f,langmj,t8,t9,psolr
c    1       f,cratio,langmj
cd    Added by S. Dun, Nov 13, 2006
      real  ms, sb, sd, cosfi,sindlt, cosdlt, tao, fortao
cd    End adding
c
c     +++LOCAL DEFINITIONS+++
c     d      - Declination of the sun in radian units,(+)North (-)South.
c     e      - Radius vertor of the sun in radian units.
c     r1     - Solar constant factor.
c     solcon - Solar constant (Ly/min).
c     t      - Temporary variable.
c     t7     - Hour Angle of sunset on horizontal surface (degrees).
c     t6     - Hour Angle of sunrise on the equivalent slope surface.
c     x      - Temp variable used to determine HA on the equiv slope.
c     t1     - Hour Angle of sunset on the horizontal surface (deg).
c     t0     - Hour Angle of sunrise on the horizontal surface (deg).
c     t3     - Hour Angle of sunset on the sloping surface (deg).
c     t2     - Hour Angle of sunrise on the sloping surface (deg).
c     r4     - Potential solar beam on the sloping surface (MJ/m^2/day)
c     pi     - Estimated value for 22/7.
c     t4     - Number of hours before solar noon of sunrise on slope.
c     langmj - Conversion factor from Cal/cm^2 = Ly to MJ/M2.
c     f      - Slope factor (r4/r3).
c     r6     - Est solar rad on sloping surface map area (MJ/m^2/day).
c     cratio - Hourly radiation adjustment ratio calculated in RADCURV.
c
c     +++OUTPUT FORMATS+++
c2000 format(' Inside SunMap Routine ',/,' d = ',f10.3,/,' e = ',
c     1       f10.3,/,' r1 = ',
c     2      f10.3,/,' t7 = ',f10.3,/,' t6 = ',f10.3,/,' t3 = ',f10.3,
c     3      /,' t2 = ',f10.3,/,' r4 = ',f10.3,/,' t4 = ',f10.3,/)
c
c     +++END SPECIFICATIONS+++
c
      pi = 3.141593
      solcon = 1.94
      langmj = 0.04184
      r2 = radly
c
c -- begin calculations --


      d  = 0.00698 - 0.4067 * cos((sdate + 10.0) * 0.0172)
      e  = 1.00000 - 0.0167 * cos((sdate -  3.0) * 0.0172)
      r1 = (60.0 * solcon) / (e * e)
      x  = -((sin(eqlat)) / (cos(eqlat))) * ((sin(d)) / (cos(d)))

      if (x .gt. 1.0) x =  1.0
      if (x .lt.-1.0) x = -1.0
c
      t = acos(x)
      t7 = t - delong
      t6 = -t - delong

c -- NOTE that the "x" calculations are written out the long way rather
c --  than by simply using the "tan" function.  This is do to a bug in
c --  Lahey compiler with tangent that Dennis F. found on AT&T 6300's.
c
c     x = -((sin(radlat))/(cos(radlat)))*((sin(d))/(cos(d)))
c
c     Use tangent function - as 6300's obsolete and now using newer
c     Lahey compiler.   dcf  2/22/94
c
      x = - (tan(radlat) * tan(d))

      if (x .gt. 1.0) x =  1.0
      if (x .lt.-1.0) x = -1.0
      t = acos(x)

      t1 = t
      t0 = -t

      t3 = t7
      if (t7 .ge. t1) t3 = t1

      t2 = t6
      if (t6 .le. t0) t2 = t0
c
cd      Modified by S. Dun, Nov 07, 2006 
c      In responsing Erin Brooks (UI) PMET crash, adding in the alternative routines 
c      (supplemental functions) for steep slope at high latitude
c
c      r4 = r1 * ((sin(d) * sin(eqlat) * (t3 - t2) * 12. / pi)
c     1+ (cos(d) * cos(eqlat) * (sin(t3 + delong) - sin(t2 + delong))
c     2* 12. / pi))
c
      If (t3 .lt. t2) t2 = t3
      t6 = t6 + 2*pi
c
      if (t6 .lt. t1) then
         t8 = t6
         t9 = t1
         r4 = r1 * (psolR(d,delong,eqlat,t3,t2) + 
     1        psolR(d,delong,eqlat,t9,t8))
      else
         t7 = t7 - 2*pi
c
         if (t7 .gt. t0) then
             t8 = t0
             t9 = t7
             r4 = r1 * (psolR(d,delong,eqlat,t3,t2) + 
     1             psolR(d,delong,eqlat,t9,t8))
          else
             r4 = r1 * (psolR(d,delong,eqlat,t3,t2))
          endif
c
      endif
c
cd      radpot=r4
c
cd    End modifying
      r4 = r4 * langmj
c reza 3/2/94      radpot = r4

c -- Now the potential radiation on slope is in units of MJ/m^2.

      t4 = t2 * 12.0 / pi
      halfdy = abs(t4)

      r3 = r1 * ((sin(d) * sin(radlat) * (t1 - t0) * 12.0 / pi)
     1     + (cos(d) * cos(radlat) * (sin(t1) - sin(t0)) * 12.0 / pi))
c
cd      Added by S. Dun, Nov 13, 2006
c      Calculate atmosphere transmittance from measured and potential solar radiation
c      reference: Campbell GS and Norman JM, 1998 "Environmental Biophysics"
c     Equation 11.11 to 11.13
c
c      Note: Solar declination adjustment on a specific location was incorperated in 
c      r3 already. r3=Sp0*cosfi
c
c      Solar declination
      sindlt = 0.39785*sin(pi/180*(278.97 + 0.9856*sdate + 1.9165*
     1         sin(pi/180*(356.6 + 0.9856*sdate))))
      cosdlt = sqrt(1 - sindlt**2)
c      Azimuth at solar noon. 
      cosfi = sindlt * sin(radlat)+ cosdlt * cos(radlat)
c      initao = 0.4
c      call transm(r3, r2, cosfi, initao, tao)
c
      fortao = (r2-0.3*r3)
c
      if (fortao .gt. 0) then
         tao = (fortao /(r3*0.7))**cosfi
      else
               tao = 0.4
      endif
c
      If (tao .gt. 0.75) tao = 0.75
      if (tao .lt. 0.4) tao = 0.4
c
c      Calculate direct and diffuse solar radation
      ms = 1/cosfi
c
c      Cloud cover calculation using transmisivity appraoch
c      Clear sky has maximum tao = 0.75; cloud sky has minimum Tao = 0.4.
c
c      r2/r3 = (1-C)*(0.3 + 0.7*MaxTao**(1/cosfi)) + C*(0.3 + 0.7*MinTao**(1/cosfi))
c
      cloudC = (0.3 + 0.7* 0.75**ms - r2/r3)/(0.7*(0.75**ms - 0.4**ms))
      if (cloudC .gt. 1.0) cloudC = 1.0
      if (cloudC .lt. 0.0) cloudC = 0.0
cd      End adding
c
cd      Modified by S. Dun Nov. 11, 2006
c      Now radpot represent the potential solar radiation on horizontal surface
c      instead of on a sloped surface.
c
c      For clearest sky day incoming solar energy
c      tao = 0.75      
c      radpot = r3*(0.7*tao**(1/cosfi) + 0.3)
cd      End modifying
      radpot = r3
      r3 = r3 * langmj
c
c -- Now the potential radiation on horiz is also in MJ/m^2.

cd      f = r4 / r3
cd      Added by S. Dun, Nov 13, 2006
c
      Sb = r3* tao**ms      
      Sd = 0.3* (r3- Sb)
      f = (r4* tao**ms + sd)/(sb+sd)
      solef(iplane) = f
c
      if (sdate .ne. datef) then
cd      write(61,1000) sdate,tao,cosfi,r3,r2,sb, sd, f, cloudC
      endif
c      sunmap was called so many times. I need check it out sometime
      datef = sdate
cd      End adding
c Reza, the next line will make r5smap to be in LY
c because r2=radly, we need to convert r5smap to MJ
      r5smap = f * r2
c Reza  changed the next line, 3/9/94
c --  r5smap = r5smap * langmj

      r5smap = r5smap * langmj
c -- The above was commented out because f and r2 are already in
c -- the desired units.

c -- Make sure that r5smap is in MJ/m*m units.

c      r6 = r5smap / cos(radinc)
c -- The above line is commented out due to Bob's suggestion
c -- we do so.  It was used to project sloping surface to the
c -- horizontal surface (slightly smaller area) resulting in
c -- slightly higher radiation.  We feel that when dealing with
c -- non-mountain slope segments, we don't need such an adjustment.

      rpoth = r3
      dsunmp = d
c
creza 11/16/94
c     if(irdflg .eq. 1) then
c       -- Now we call the RADCUR routine to calc hourly radiation.
c       call radcur(sdate,hour,radlat,d,cratio)
c       cratio = cratio / r3
c       -- Now we know the ratio of daily radiation to hourly radiation.
c       reza, in the next line hradmj is in MJ
c       hradmj = r5smap * cratio
c     endif
creza change 11/16/94
1000      Format(1x,I5,8f10.2)
      return
      end

      subroutine impflo(itr,qi,htw)
c
c     + + + PURPOSE + + +
c
c     SR IMPFLO is an impoundment element subroutine responsible
c     for determining the change in stage and outflow over a
c     time step.  It also calculates the duration of the next time
c     step.
c
c     Called from: IMPMAI
c     Author(s):  Mark Lindley
c     Reference in User Guide:
c
c     Version:
c     Date recoded: 3/01/94
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmximp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real qi, htw
      integer itr
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     itr - outflow regime number
c     htw - depth of tailwater flow in downstream channel
c     qi  - inflow rate
c
c     + + + COMMON BLOCKS + + +
c
      include 'cimitf.inc'
c     read: a,b,c,d,e,ha,ht,hlm,a0,a1,a2,l0,l1,l2,qinf,isize
c
      include 'cimpnd.inc'
c
      include 'cimqot.inc'
c     read: qo,h,t,dttry,deltat
c     write: qon,hn,tn,dtdid,dtnext
c
c       added by S.Dun  March 16,1999
        include 'cimflg.inc'
c       call the impoundment structure indicators
      include 'cimsed.inc'
c       end adding
c
c     + + + LOCAL VARIABLES + + +
c
      real qo1, qo2, qo3, qo4, qo5, qo6, qo7, qo8, qo9, qo10, qo11,
     1    qo12, qo13, qo14, qo15, tsav, hsav, dt, tt, htemp, errmax
      integer flg, k
      doubleprecision   dhlm,dh
c
c     + + + LOCAL DEFINITIONS + + +
c
c     dt     - intermediate time step
c     errmax - error in making one step compared to two half steps
c     flg    - flag for an attempted step that is too large
c     hsav   - intermediate stage (at the beginning of the time step)
c     htemp  - intermediate stage (at the end of the time step)
c     k      - counter for number of attempts made
c     qo1,15 - outflow contribution for each stage-discharge function
c     tsav   - intermediate time
c     tt     - intermediate time step
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     imphnw
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     initialize variables
c 
        if (hlm(itr,ipond).gt.h(ipond)) then
          dhlm=hlm(itr,ipond)
          dh=h(ipond)
        endif
      k = 0
c
      tsav = t(ipond)
      hsav = h(ipond)
      dt = dttry(ipond)
c
   10 tt = 0.5 * dt
c
      flg = 0
c
c     attempting integration with two consecutive half time steps
c
      call imphnw(hsav,flg,tt,htemp,qi,itr,htw)
c
      if (flg.eq.1) then
        dt = 0.5 * dt
        go to 10
      end if
c
      t(ipond) = tsav + tt
c
      call imphnw(htemp,flg,tt,h(ipond),qi,itr,htw)
c
      if (flg.eq.1) then
        dt = 0.5 * dt
        go to 10
      end if
c
      t(ipond) = tsav + dt
c
c     attempting integration with one full time step
c
      call imphnw(hsav,flg,dt,htemp,qi,itr,htw)
c
      if (flg.eq.1) then
        dt = 0.5 * dt
        go to 10
      end if
c
c     checking error between two half steps and one full time step
c     if error is too large the time step is decreased and
c     attempted again, if the error is acceptable, the next
c     suggested time step is computed
c
      htemp = h(ipond) - htemp
      errmax = max(0.0,abs(htemp)) / 1.0e-04
c
      if (errmax.gt.1) then
        dt = 0.9 * dt * errmax ** (-0.25)
        go to 10
      else
        dtdid(ipond) = dt
c
        if (errmax.gt.6.0e-04) then
          dtnext(ipond) = 0.9 * dt * (errmax**(-0.2))
        else
          dtnext(ipond) = 4.0 * dt
        end if
c
      end if
c
      hn(ipond) = h(ipond) + htemp / 15.0
c
c     checking to see that the new stage is within the same flow
c     regime, if it is not the time step is attempted again
c     using the initial time step
c
      k = k + 1
c
c      Modified by S.Dun June 9,1999 
        if(dt.gt.deltat(ipond).and.k.ne.2) then
c      if (k.ne.2) then
c       end modifying
c
        if (hn(ipond).gt.ht(itr+1,ipond)) then
          dt = deltat(ipond)
          goto 10
        else
c
          if (itr.gt.1) then
c
            if (hn(ipond).lt.ht(itr,ipond)) then
              dt = deltat(ipond)
              goto 10
            end if
c
          end if
          endif
c       added by S.Dun April 21,1999
c       If stage range in a time step is around the sediment stage. The time  
c       step should not be longter than the initial time step.
          if(hn(ipond).lt.hmin(ipond)+0.001.and.hsav.gt.hmin(ipond)
     1    +0.001) then
            dt = deltat(ipond)
            goto 10
          end if
c       end adding
      end if
c
c       added by S.Dun May 12,1999
c       if None rock-fill check dam or emergence spillway or filter
c       fence or Strawbales  or trash barrier is present. when the 
c       stage range in a time step is around stage at which the
c       impoundment is overtopped. The time step should be not 
c       longer than 0.0001hr.

          if (firf(ipond).eq.0.and.fies(ipond).eq.0.and.
     1    fiff(ipond).eq.0) then
c
            if(dt.gt.0.0001) then
              if(hn(ipond).gt.hot(ipond).and.hsav.lt.hot(ipond)) then
c                 dt = deltat(ipond)
                  dt=0.0001
                  goto 10
              end if
              if(hn(ipond).lt.hot(ipond).and.hsav.ge.hot(ipond)) then
c               dt = deltat(ipond)
                dt=0.0001
                goto 10
              end if
            endif
          endif

c
      h(ipond) = hsav
      tn(ipond) = tsav + dtdid(ipond)
      t(ipond) = tsav
c
c     computing the new outflow
c
      if (hn(ipond).le.hlm(itr,ipond)) hn(ipond) = hlm(itr,ipond)
c
      qo1 = (b(1,itr,ipond)*(hn(ipond)-ha(1,itr,ipond))**
     1    c(1,itr,ipond))
c
      qo2 = (b(2,itr,ipond)*(hn(ipond)-ha(2,itr,ipond))**
     1    c(2,itr,ipond))
c
      if (htw.gt.a(3,itr,ipond)) then
        qo3 = (b(3,itr,ipond)*(hn(ipond)-(ha(3,itr,ipond)+htw-
     1      a(3,itr,ipond)))**c(3,itr,ipond))
      else
        qo3 = (b(3,itr,ipond)*(hn(ipond)-ha(3,itr,ipond))**
     1      c(3,itr,ipond))
      end if
c
      qo4 = a(4,itr,ipond) * ((hn(ipond)-ha(4,itr,ipond))/
     1    b(4,itr,ipond)) ** c(4,itr,ipond)
c
      qo5 = a(5,itr,ipond) * (((hn(ipond)-ha(5,itr,ipond))/
     1    b(5,itr,ipond)+c(5,itr,ipond))/d(5,itr,ipond)) ** 0.5
c
      if (htw.gt.a(6,itr,ipond)) then
        qo6 = b(6,itr,ipond) * (hn(ipond)-(ha(6,itr,ipond)+htw-
     1      a(6,itr,ipond))) ** c(6,itr,ipond)
      else
        qo6 = b(6,itr,ipond) * (hn(ipond)-ha(6,itr,ipond)) **
     1      c(6,itr,ipond)
      end if
c
      qo7 = a(7,itr,ipond) * ((hn(ipond)-ha(7,itr,ipond))/
     1    b(7,itr,ipond)) ** c(7,itr,ipond)
c
      qo8 = a(8,itr,ipond) * (((hn(ipond)-ha(8,itr,ipond))/
     1    b(8,itr,ipond)+c(8,itr,ipond))/d(8,itr,ipond)) ** 0.5
c
      if (htw.gt.a(9,itr,ipond)) then
        qo9 = b(9,itr,ipond) * (hn(ipond)-(ha(9,itr,ipond)+htw-
     1      a(9,itr,ipond))) ** c(9,itr,ipond)
      else
        qo9 = b(9,itr,ipond) * (hn(ipond)-ha(9,itr,ipond)) **
     1      c(9,itr,ipond)
      end if
c
      qo10 = a(10,itr,ipond) * ((hn(ipond)-ha(10,itr,ipond))/
     1    b(10,itr,ipond)) ** c(10,itr,ipond) + d(10,itr,ipond) * (
     1    hn(ipond)-e(10,itr,ipond)) ** 1.5
c
      qo11 = a(11,itr,ipond) + b(11,itr,ipond) * (hn(ipond)-
     1    ha(11,itr,ipond)) + c(11,itr,ipond) * (hn(ipond)-
     1    ha(11,itr,ipond)) ** 2. + d(11,itr,ipond) * (hn(ipond)-
     1    ha(11,itr,ipond)) ** 3. + e(11,itr,ipond) * (hn(ipond)-
     1    ha(11,itr,ipond)) ** 4.0
c
      qo12 = a(12,itr,ipond) * (hn(ipond)-ha(12,itr,ipond)) + (
     1    b(12,itr,ipond)+c(12,itr,ipond)*(hn(ipond)-d(12,itr,ipond))) *
     1    (hn(ipond)-d(12,itr,ipond)) ** 1.5
c
      qo13 = a(13,itr,ipond) / (b(13,itr,ipond)+c(13,itr,ipond)/(
     1    hn(ipond)-ha(13,itr,ipond))**1.5)
c
      qo14 = a(14,itr,ipond) * (hn(ipond)-ha(14,itr,ipond)) ** 0.5
c
      qo15 = b(15,itr,ipond) * (hn(ipond)-ha(15,itr,ipond)) **
     1    c(15,itr,ipond)
c
      qon(ipond) = min(qo1,qo2,qo3) + min(qo4,qo5,qo6) +
     1    min(qo7,qo8,qo9) + qo10 + qo11 + qo12 + min(qo13,qo14,qo15)
c
      return
      end

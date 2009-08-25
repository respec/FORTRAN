      subroutine imphnw(hin,flg,tinit,hout,qin,itr,htw)
c
c     + + + PURPOSE + + +
c
c     SR IMPHNW is an impoundment element subroutine that computes a
c     new stage given the current stage and time step.
c
c     Called from: IMPFLO
c     Author(s): Mark Lindley
c     Reference in User Guide:
c
c     Version:
c     Date recoded: 3/6/94
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
      real hin, tinit, hout, qin, htw
      integer flg, itr
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     tinit - initial time step
c     flg   - flag indicating too large a time step (flg = 1)
c     hin   - stage at the beginning of the time step
c     hout  - stage at the end of the time step
c     itr   - outflow regime number
c     htw   - depth of tailwater flow in downstream channel
c     qin   - inflow rate
c
c     + + + COMMON BLOCKS + + +
c
      include 'cimitf.inc'
c     read: a,b,c,d,e,ha,ht,hlm,a0,a1,a2,l0,l1,l2,qinf,isize
c
      include 'cimpnd.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real qo1, qo2, qo3, qo4, qo5, qo6, qo7, qo8, qo9, qo10, qo11,
     1    qo12, qo13, qo14, qo15, qout, tt, t6, dht, htp, dhm, dhdt
c
c     + + + LOCAL DEFINITIONS + + +
c
c     dhdt   - intermediate change in stage over change in time
c     dhm    - intermediate change in stage over change in time
c     dht    - intermediate change in stage over change in time
c     htp    - intermediate stage
c     t6     - averaging term used for computation of hout
c     tt     - intermediate time step
c     qo1,15 - outflow contribution for each stage-discharge function
c     qout   - outflow rate
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     computing dh/dt at hin = h
c
      if (hin.lt.hlm(itr,ipond)) then
        flg = 1
        return
      end if
c
      qo1 = (b(1,itr,ipond)*(hin-ha(1,itr,ipond))**c(1,itr,ipond))
c
      qo2 = (b(2,itr,ipond)*(hin-ha(2,itr,ipond))**c(2,itr,ipond))
c
      if (htw.gt.a(3,itr,ipond)) then
        qo3 = (b(3,itr,ipond)*(hin-(ha(3,itr,ipond)+htw-a(3,itr,ipond)))
     1      **c(3,itr,ipond))
      else
        qo3 = (b(3,itr,ipond)*(hin-ha(3,itr,ipond))**c(3,itr,ipond))
      end if
c
      qo4 = a(4,itr,ipond) * ((hin-ha(4,itr,ipond))/b(4,itr,ipond)) **
     1    c(4,itr,ipond)
c
cdcf    Following line commented out, since qq is never used
cdcf    here or anywhere else in the program.  dcf  6-1-2000
cdcf    qq=((hin-ha(5,itr,ipond))/b(5,itr,ipond)+
cdcf 1    c(5,itr,ipond))/d(5,itr,ipond)
c      if (qq.le.0.0) pause
      qo5 = a(5,itr,ipond) * (((hin-ha(5,itr,ipond))/b(5,itr,ipond)+
     1    c(5,itr,ipond))/d(5,itr,ipond)) ** 0.5
c
      if (htw.gt.a(6,itr,ipond)) then
        qo6 = b(6,itr,ipond) * (hin-(ha(6,itr,ipond)+htw-
     1      a(6,itr,ipond))) ** c(6,itr,ipond)
      else
        qo6 = b(6,itr,ipond) * (hin-ha(6,itr,ipond)) ** c(6,itr,ipond)
      end if
c
      qo7 = a(7,itr,ipond) * ((hin-ha(7,itr,ipond))/b(7,itr,ipond)) **
     1    c(7,itr,ipond)
c
      qo8 = a(8,itr,ipond) * (((hin-ha(8,itr,ipond))/b(8,itr,ipond)+
     1    c(8,itr,ipond))/d(8,itr,ipond)) ** 0.5
c
      if (htw.gt.a(9,itr,ipond)) then
        qo9 = b(9,itr,ipond) * (hin-(ha(9,itr,ipond)+htw-
     1      a(9,itr,ipond))) ** c(9,itr,ipond)
      else
        qo9 = b(9,itr,ipond) * (hin-ha(9,itr,ipond)) ** c(9,itr,ipond)
      end if
c
      qo10 = a(10,itr,ipond) * ((hin-ha(10,itr,ipond))/b(10,itr,ipond))
     1    ** c(10,itr,ipond) + d(10,itr,ipond) * (hin-e(10,itr,ipond))
     1    ** 1.5
c
      qo11 = a(11,itr,ipond) + b(11,itr,ipond) * (hin-ha(11,itr,ipond))
     1    + c(11,itr,ipond) * (hin-ha(11,itr,ipond)) ** 2. +
     1    d(11,itr,ipond) * (hin-ha(11,itr,ipond)) ** 3. +
     1    e(11,itr,ipond) * (hin-ha(11,itr,ipond)) ** 4.0
c
      qo12 = a(12,itr,ipond) * (hin-ha(12,itr,ipond)) + (
     1    b(12,itr,ipond)+c(12,itr,ipond)*(hin-d(12,itr,ipond))) * (hin-
     1    d(12,itr,ipond)) ** 1.5
c
      qo13 = a(13,itr,ipond) / (b(13,itr,ipond)+c(13,itr,ipond)/(hin-
     1    ha(13,itr,ipond))**1.5)
c
      qo14 = a(14,itr,ipond) * (hin-ha(14,itr,ipond)) ** 0.5
c
      qo15 = b(15,itr,ipond) * (hin-ha(15,itr,ipond)) **
     1    c(15,itr,ipond)
c
      qout = min(qo1,qo2,qo3) + min(qo4,qo5,qo6) + min(qo7,qo8,qo9) +
     1    qo10 + qo11 + qo12 + min(qo13,qo14,qo15)
c
      dhdt = (qin-qout) / (a0(ipond)+a1(ipond)*hin**a2(ipond))
      tt = 3600. * tinit * 0.5
      t6 = 3600. * tinit / 6.0
      htp = hin + tt * dhdt
c
      if (htp.lt.hlm(itr,ipond)) then
        flg = 1
        return
      end if
c
c     computing dh/dt at hin = htp  (half the time step)
c
      qo1 = (b(1,itr,ipond)*(htp-ha(1,itr,ipond))**c(1,itr,ipond))
c
      qo2 = (b(2,itr,ipond)*(htp-ha(2,itr,ipond))**c(2,itr,ipond))
c
      if (htw.gt.a(3,itr,ipond)) then
        qo3 = (b(3,itr,ipond)*(htp-(ha(3,itr,ipond)+htw-a(3,itr,ipond)))
     1      **c(3,itr,ipond))
      else
        qo3 = (b(3,itr,ipond)*(htp-ha(3,itr,ipond))**c(3,itr,ipond))
      end if
c
      qo4 = a(4,itr,ipond) * ((htp-ha(4,itr,ipond))/b(4,itr,ipond)) **
     1    c(4,itr,ipond)
c
      qo5 = a(5,itr,ipond) * (((htp-ha(5,itr,ipond))/b(5,itr,ipond)+
     1    c(5,itr,ipond))/d(5,itr,ipond)) ** 0.5
c
      if (htw.gt.a(6,itr,ipond)) then
        qo6 = b(6,itr,ipond) * (htp-(ha(6,itr,ipond)+htw-
     1      a(6,itr,ipond))) ** c(6,itr,ipond)
      else
        qo6 = b(6,itr,ipond) * (htp-ha(6,itr,ipond)) ** c(6,itr,ipond)
      end if
c
      qo7 = a(7,itr,ipond) * ((htp-ha(7,itr,ipond))/b(7,itr,ipond)) **
     1    c(7,itr,ipond)
c
      qo8 = a(8,itr,ipond) * (((htp-ha(8,itr,ipond))/b(8,itr,ipond)+
     1    c(8,itr,ipond))/d(8,itr,ipond)) ** 0.5
c
      if (htw.gt.a(9,itr,ipond)) then
        qo9 = b(9,itr,ipond) * (htp-(ha(9,itr,ipond)+htw-
     1      a(9,itr,ipond))) ** c(9,itr,ipond)
      else
        qo9 = b(9,itr,ipond) * (htp-ha(9,itr,ipond)) ** c(9,itr,ipond)
      end if
c
      qo10 = a(10,itr,ipond) * ((htp-ha(10,itr,ipond))/b(10,itr,ipond))
     1    ** c(10,itr,ipond) + d(10,itr,ipond) * (htp-e(10,itr,ipond))
     1    ** 1.5
c
      qo11 = a(11,itr,ipond) + b(11,itr,ipond) * (htp-ha(11,itr,ipond))
     1    + c(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 2. +
     1    d(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 3. +
     1    e(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 4.0
c
      qo12 = a(12,itr,ipond) * (htp-ha(12,itr,ipond)) + (
     1    b(12,itr,ipond)+c(12,itr,ipond)*(htp-d(12,itr,ipond))) * (htp-
     1    d(12,itr,ipond)) ** 1.5
c
      qo13 = a(13,itr,ipond) / (b(13,itr,ipond)+c(13,itr,ipond)/(htp-
     1    ha(13,itr,ipond))**1.5)
c
      qo14 = a(14,itr,ipond) * (htp-ha(14,itr,ipond)) ** 0.5
c
      qo15 = b(15,itr,ipond) * (htp-ha(15,itr,ipond)) **
     1    c(15,itr,ipond)
c
      qout = min(qo1,qo2,qo3) + min(qo4,qo5,qo6) + min(qo7,qo8,qo9) +
     1    qo10 + qo11 + qo12 + min(qo13,qo14,qo15)
c
      dht = (qin-qout) / (a0(ipond)+a1(ipond)*htp**a2(ipond))
      htp = hin + tt * dht
c
      if (htp.lt.hlm(itr,ipond)) then
        flg = 1
        return
      end if
c
c     computing dh/dt at hin = htp  (half the time step)
c
      qo1 = (b(1,itr,ipond)*(htp-ha(1,itr,ipond))**c(1,itr,ipond))
c
      qo2 = (b(2,itr,ipond)*(htp-ha(2,itr,ipond))**c(2,itr,ipond))
c
      if (htw.gt.a(3,itr,ipond)) then
        qo3 = (b(3,itr,ipond)*(htp-(ha(3,itr,ipond)+htw-a(3,itr,ipond)))
     1      **c(3,itr,ipond))
      else
        qo3 = (b(3,itr,ipond)*(htp-ha(3,itr,ipond))**c(3,itr,ipond))
      end if
c
      qo4 = a(4,itr,ipond) * ((htp-ha(4,itr,ipond))/b(4,itr,ipond)) **
     1    c(4,itr,ipond)
c
      qo5 = a(5,itr,ipond) * (((htp-ha(5,itr,ipond))/b(5,itr,ipond)+
     1    c(5,itr,ipond))/d(5,itr,ipond)) ** 0.5
c
      if (htw.gt.a(6,itr,ipond)) then
        qo6 = b(6,itr,ipond) * (htp-(ha(6,itr,ipond)+htw-
     1      a(6,itr,ipond))) ** c(6,itr,ipond)
      else
        qo6 = b(6,itr,ipond) * (htp-ha(6,itr,ipond)) ** c(6,itr,ipond)
      end if
c
      qo7 = a(7,itr,ipond) * ((htp-ha(7,itr,ipond))/b(7,itr,ipond)) **
     1    c(7,itr,ipond)
c
      qo8 = a(8,itr,ipond) * (((htp-ha(8,itr,ipond))/b(8,itr,ipond)+
     1    c(8,itr,ipond))/d(8,itr,ipond)) ** 0.5
c
      if (htw.gt.a(9,itr,ipond)) then
        qo9 = b(9,itr,ipond) * (htp-(ha(9,itr,ipond)+htw-
     1      a(9,itr,ipond))) ** c(9,itr,ipond)
      else
        qo9 = b(9,itr,ipond) * (htp-ha(9,itr,ipond)) ** c(9,itr,ipond)
      end if
c
      qo10 = a(10,itr,ipond) * ((htp-ha(10,itr,ipond))/b(10,itr,ipond))
     1    ** c(10,itr,ipond) + d(10,itr,ipond) * (htp-e(10,itr,ipond))
     1    ** 1.5
c
      qo11 = a(11,itr,ipond) + b(11,itr,ipond) * (htp-ha(11,itr,ipond))
     1    + c(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 2. +
     1    d(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 3. +
     1    e(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 4.0
c
      qo12 = a(12,itr,ipond) * (htp-ha(12,itr,ipond)) + (
     1    b(12,itr,ipond)+c(12,itr,ipond)*(htp-d(12,itr,ipond))) * (htp-
     1    d(12,itr,ipond)) ** 1.5
c
      qo13 = a(13,itr,ipond) / (b(13,itr,ipond)+c(13,itr,ipond)/(htp-
     1    ha(13,itr,ipond))**1.5)
c
      qo14 = a(14,itr,ipond) * (htp-ha(14,itr,ipond)) ** 0.5
c
      qo15 = b(15,itr,ipond) * (htp-ha(15,itr,ipond)) **
     1    c(15,itr,ipond)
c
      qout = min(qo1,qo2,qo3) + min(qo4,qo5,qo6) + min(qo7,qo8,qo9) +
     1    qo10 + qo11 + qo12 + min(qo13,qo14,qo15)
c
      dhm = (qin-qout) / (a0(ipond)+a1(ipond)*htp**a2(ipond))
      htp = hin + 3600. * tinit * dhm
      dhm = dht + dhm
c
      if (htp.lt.hlm(itr,ipond)) then
        flg = 1
        return
      end if
c
c     computing dh/dt at hin = htp  (the whole time step)
c
      qo1 = (b(1,itr,ipond)*(htp-ha(1,itr,ipond))**c(1,itr,ipond))
c
      qo2 = (b(2,itr,ipond)*(htp-ha(2,itr,ipond))**c(2,itr,ipond))
c
      if (htw.gt.a(3,itr,ipond)) then
        qo3 = (b(3,itr,ipond)*(htp-(ha(3,itr,ipond)+htw-a(3,itr,ipond)))
     1      **c(3,itr,ipond))
      else
        qo3 = (b(3,itr,ipond)*(htp-ha(3,itr,ipond))**c(3,itr,ipond))
      end if
c
      qo4 = a(4,itr,ipond) * ((htp-ha(4,itr,ipond))/b(4,itr,ipond)) **
     1    c(4,itr,ipond)
c
      qo5 = a(5,itr,ipond) * (((htp-ha(5,itr,ipond))/b(5,itr,ipond)+
     1    c(5,itr,ipond))/d(5,itr,ipond)) ** 0.5
c
      if (htw.gt.a(6,itr,ipond)) then
        qo6 = b(6,itr,ipond) * (htp-(ha(6,itr,ipond)+htw-
     1      a(6,itr,ipond))) ** c(6,itr,ipond)
      else
        qo6 = b(6,itr,ipond) * (htp-ha(6,itr,ipond)) ** c(6,itr,ipond)
      end if
c
      qo7 = a(7,itr,ipond) * ((htp-ha(7,itr,ipond))/b(7,itr,ipond)) **
     1    c(7,itr,ipond)
c
      qo8 = a(8,itr,ipond) * (((htp-ha(8,itr,ipond))/b(8,itr,ipond)+
     1    c(8,itr,ipond))/d(8,itr,ipond)) ** 0.5
c
      if (htw.gt.a(9,itr,ipond)) then
        qo9 = b(9,itr,ipond) * (htp-(ha(9,itr,ipond)+htw-
     1      a(9,itr,ipond))) ** c(9,itr,ipond)
      else
        qo9 = b(9,itr,ipond) * (htp-ha(9,itr,ipond)) ** c(9,itr,ipond)
      end if
c
      qo10 = a(10,itr,ipond) * ((htp-ha(10,itr,ipond))/b(10,itr,ipond))
     1    ** c(10,itr,ipond) + d(10,itr,ipond) * (htp-e(10,itr,ipond))
     1    ** 1.5
c
      qo11 = a(11,itr,ipond) + b(11,itr,ipond) * (htp-ha(11,itr,ipond))
     1    + c(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 2. +
     1    d(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 3. +
     1    e(11,itr,ipond) * (htp-ha(11,itr,ipond)) ** 4.0
c
      qo12 = a(12,itr,ipond) * (htp-ha(12,itr,ipond)) + (
     1    b(12,itr,ipond)+c(12,itr,ipond)*(htp-d(12,itr,ipond))) * (htp-
     1    d(12,itr,ipond)) ** 1.5
c
      qo13 = a(13,itr,ipond) / (b(13,itr,ipond)+c(13,itr,ipond)/(htp-
     1    ha(13,itr,ipond))**1.5)
c
      qo14 = a(14,itr,ipond) * (htp-ha(14,itr,ipond)) ** 0.5
c
      qo15 = b(15,itr,ipond) * (htp-ha(15,itr,ipond)) **
     1    c(15,itr,ipond)
c
      qout = min(qo1,qo2,qo3) + min(qo4,qo5,qo6) + min(qo7,qo8,qo9) +
     1    qo10 + qo11 + qo12 + min(qo13,qo14,qo15)
c
      dht = (qin-qout) / (a0(ipond)+a1(ipond)*htp**a2(ipond))
c
c     computing hout
c
      hout = hin + t6 * (dhdt+dht+2.0*dhm)
c
      return
      end

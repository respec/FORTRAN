      subroutine sumfrc(enr1,enr2,enr,frc1,frc2,frc,ds,iout1,iflag,
     1    iyear,lun1,noout,nowcrp)
c
c
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxelm.inc'
c
      include 'cpart1.inc'
c
      real enr1, enr2, enr, frc1(mxpart), frc2(mxpart), frc(mxpart),
     1    ds(mxplan,100)
c
      integer iout1, iflag, iyear, lun1, noout, nowcrp
c
c
      integer kk
c
      if (enr2.gt.0.) then
        enr = enr1 / enr2
      else
        enr = 0.0
      end if
c
      do 10 kk = 1, npart
        if (frc2(kk).gt.0.0) then
          frc(kk) = frc1(kk) / frc2(kk)
        else
          frc(kk) = 0.0
        end if
c
   10 continue
c
c
      if (iout1.eq.iflag) call
     1    sedout(iyear,ds,0,0,0,lun1,noout,1,nowcrp)
c
      return
      end

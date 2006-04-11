      subroutine IINTERCON(firsttime)
C     EXTRAN BLOCK
C     CREATE BY CHUCK MOORE OF CDM 
C     TO WRITE INTERMEDIATE OUTPUT RESULTS
      include 'tapes.inc'
      include 'INTCON.INC'
      include 'contr.inc'
      include 'out.inc'
      include 'JUNC.INC'
      Logical firsttime
      if (firsttime) then
      do j=1,nconsav
      pcnerr(j) = 0.0
      icycerr(j) = 0
      khrerr(j) = 0
      tminerr(j) = 0.0
      end do
      end if
CIM   this subroutine initialize stuff.
      nconter=0
      vold = vnow
      do j=1,NJ
      qouo(j)=qou(j)
      qqio(j)=qqi(j)
      end do
      return
      end

      subroutine intercon(writenow)
      include 'tapes.inc'
      include 'INTCON.INC'
      include 'CONTR.INC'
      include 'out.inc'
      include 'JUNC.INC'
      include 'stimer.inc'
      logical writenow
cim this subroutine checks for output required and writes if needed
      nconter = nconter + 1
      if ((nconter.ge.iconter).or.writenow) then
cim  here compute write and save intermediate continuity results.
      call volume(vnow,.FALSE.)
      sumout = 0.0
      sumqin = 0.0
      do j=1,nj
      delqqi = qqi(j) - qqio(j)
      delqou = qou(j) - qouo(j)
      if (delqqi.gt.0.0) sumqin = sumqin + delqqi
      if (delqou.lt.0.0) then
      sumqin = sumqin - delqou
      else
      sumout = sumout + delqou
      end if
      end do
      sum1 = vold+sumqin
      sum2 = vnow+sumout
      TMIN   = FLOAT(MINUTE) + FLOAT(JSEC)/60.0
      KHR    = IFIX(TIME/3600.0)
      write(n6,7000) ICYC,KHR,TMIN
      write(n6,7010) vold,sumqin,sum1
      write(n6,7015) vnow,sumout,sum2
      pererror = (sum1-sum2)/sum1
      call saveint(pererror*100.0,icyc,khr,tmin)
      write(n6,7020) pererror*100.0
      IF (ISOLSKIP.EQ.1) CALL SSSTATUS(2)
 7000 format(' Intermediate Continuity Summary For Period Prior',
     #' To Cycle '
     #,I11,1X,'Time ',I4,' Hr - ',F5.2,' Min')
 7010 format(' Initial Volume = ',1PE14.4,
     #' cu. ft.   Inflow Volume  = ',E14.4,
     #' cu. ft.   Initial Volume + Inflow = ',E14.4)
 7015 format(' Final Volume   = ',1PE14.4,
     #' cu. ft.   Outflow Volume = ',E14.4,
     #' cu. ft.   Final Volume + Outflow  = ',E14.4)
 7020 format(' Percent Error  = ',f14.2)
      call iintercon(.false.)
      endif
      return
      end

      SUBROUTINE SAVEINT(pererror,icyc,khr,tmin)
      include 'tapes.inc'
      include 'intcon.inc'
      do j=1,nconsav
      if (abs(pererror).gt.abs(pcnerr(j))) go to 100
      enddo
      return
 100  do k=nconsav,j+1,-1
      pcnerr(k) = pcnerr(k-1)
      icycerr(k) = icycerr(k-1)
      khrerr(k) = khrerr(k-1)
      tminerr(k) = tminerr(k-1)
      end do
      pcnerr(j) = pererror
      icycerr(j) = icyc
      khrerr(j) = khr
      tminerr(j) = tmin
      return
      end

      subroutine fintercon
cim called from subroutine OUTPUT
      include 'tapes.inc'
      include 'intcon.inc'
      IF (ICONTER.LT.2147483647) then
      write(n6,7000)
      do j=1,nconsav
      if (icycerr(j).gt.0.0)
     + write(n6,7010) pcnerr(j),icycerr(j),khrerr(j),tminerr(j)
      end do
      end if
      return
 7000 FORMAT(//,' THE FOLLOWING ARE THE MAXIMUM ERRORS FOUND IN ALL',
     +' INTERMEDIATE CONTINUITY CHECKS ',/,
     +' THE CONTINUITY ERRORS ARE FOR THE PERIOD PRIOR TO THE INDICATED'
     +,' CYCLE, HOUR, AND MINUTE',/,
     +' PERCENT ERROR  CYCLE      HOUR    MINUTE')
 7010 FORMAT(F10.2,i10,i10,f10.1)
      end

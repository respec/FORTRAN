      subroutine kostia(
     i                  rwdth)
c
c     + + + PURPOSE + + +
c     This subprogram calibrates modified Kostiakov infiltration
c     equation constants for a series of time-infiltration rate pairs
c     where infiltration rate is calculated using the Green and Ampt
c     infiltration function.
c
c     Called from FURROW
c     Author(s): E. R. Kottwitz
c     Reference in User Guide: Chapter 12
c
c     Changes:
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 07/07/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real      rwdth
c
c     + + + ARGUMENT DEFINITIONS + + +
c     rwdth  - assumed row width of current overland flow element (m)
c
c     + + + COMMON BLOCKS + + +
      include 'cavepar.inc'
c     modify: aveks(mxplan),avesm(mxplan)
c
      include 'ccrpout.inc'
c       read: rh(mxplan)
c
      include 'cdist2.inc'
c       read: slplen(mxplan)
c
      include 'ciraflo.inc'
c       read: aqcnst,aqexp,botwid,sidslp
c
      include 'cirfur2.inc'
c       read: dtheta(mxplan),qspply(mxplan),surge,timtot
c     modify: srg
c
      include 'cirinfl.inc'
c     modify: kostf
c      write: kosta,kostk
c
      include 'cirriga.inc'
c       read: irofe,noirr
c
      include 'cparame.inc'
c       read: ks(mxplan),sm(mxplan)
c
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      integer   furint,i,nsteps
      real      avgdp2,deltai,denom,diydt,dlower,dupper,horcon,hormax,
     1          infadj,infil2,infilh(50),infilt,infilv(0:50),infold,
     1          inftmx,irsum1,irsum2,irsum3,irsum4,irsum5,kostfd,natlni,
     1          natlnt,piovr2,qmaxl,qmaxu,testf,time(50),timhmx,vertmx
      real      tmpvr1,tmpvr2,tmpvr3,tmpvr4
c
c     + + + LOCAL DEFINITIONS + + +
c     furint - irrigated furrow interval (1: all furrows irrigated, 2:
c              every other furrow irrigated)
c     i      - do loop counter
c     nsteps - number of infiltration increments
c     avgdp2 - two times the average of dlower and dupper (m)
c     deltai - 1-D vertical distance to the wetting front increment (m)
c     denom  - coefficient used as a denominator in calculating kosta
c              kostk (m**4)
c     diydt  - time derivative if vertical advance distance (m/s)
c     dlower - flow depth at lower end of current OFE for maximum of
c              furrow supply rates assuming steady state infiltration
c              (m)
c     dupper - flow depth at upper end of current OFE for maximum of
c              furrow supply rates (m)
c     horcon - horizontal infiltration constant equal to
c              (2*Ke*Ns)**.5/(change in soil moisture content) (m/s**.5)
c     hormax - maximum horizontal distance to the wetting front (m)
c     infadj - 2-D cumulative infiltration minus Ks*(infiltration
c              opportunity time)*(row spacing) (m**2)
c     infil2 - infilt*infilt (m**4)
c     infilh - 1-D horizontal distance to the wetting front (m)
c     infilt - 2-D cumulative infiltration (volume of water infiltrated
c              per unit furrow length) (m**2)
c     infilv - 1-D vertical distance to the wetting front as determined
c              by SR NEWTON (m)
c     infold - estimate of infiltration sent to SR NEWTON (m)
c     inftmx - cumulative 2-D infiltration at t=timhmx (m**2)
c     irsum1 - summation of infil2 (m**4)
c     irsum2 - summation of infil2*natlnt (m**4)
c     irsum3 - summation of infil2*natlnt**2 (m**4)
c     irsum4 - summation of infil2*natlni (m**4)
c     irsum5 - summation of infil2*natlnt*natlni (m**4)
c     kostfd - estimate of kostf using derivative of 2-D cumulative
c              infiltration function (m**2/s)
c     natlni - natural logarithm of infilt
c     natlnt - natural logarithm of time
c     piovr2 - one-half the ratio of circle area to square of its radius
c     qmaxl  - maximum flow rate at the lower end of the OFE (m**3/s)
c     qmaxu  - maximum furrow supply rate (m**3/s)
c     testf  - estimate of new value of kostf if old value was too large
c              (m**2/s)
c     time   - infiltration opportunity time (s)
c     timhmx - time when horizontal wetting fronts meet (s)
c     vertmx - vertical distance to the wetting front at t=timhmx (m)
c     tmpvr1 - variable to reduce number of calculations (m/s)
c     tmpvr2 - variable to reduce number of calculations (m)
c     tmpvr3 - variable to reduce number of calculations (m)
c
c     + + + SUBROUTINES CALLED + + +
c     newton
c
c     + + + DATA INITIALIZATIONS + + +
      data      nsteps/50/,piovr2/1.57080/
c
c     + + + OUTPUT FORMATS + + +
 2001 format(' *** WARNING ***',/,
     1       'Furrow irrigation flow depth (',f5.1,' mm) exceeds',
     1       ' ridge height (',f5.1,' mm).',/,
     1       ' *** WARNING ***',/)
c
c     + + + END SPECIFICATIONS + + +
c
c     Assume furint = 1.  User will eventually be required to provide
c     this information.
c
      furint = 1
c
c     Initialize variables
c
      infilv(0)=0.0
      inftmx = 0.0
      vertmx = 0.0
      kostfd = 1.0
      aveks(iplane) = ks(iplane)
      avesm(iplane) = sm(iplane)
      tmpvr1 = aveks(iplane)/dtheta(iplane)
      tmpvr2 = avesm(iplane)/dtheta(iplane)
      tmpvr3 = dtheta(iplane)*float(furint)*rwdth
c
c     Determine maximum furrow inflow rate
c
      qmaxu = qspply(1)
      if(noirr.eq.2 .and. surge.ge.2)then
        do 100  srg = 2, surge
          qmaxu = max(qmaxu,qspply(srg))
  100   continue
      endif
c
c     Determine flow depth at the upper end of the OFE
c
      if(sidslp.gt.0.001)then
        dupper = (sqrt(botwid**2+4.0*sidslp*(qmaxu/aqcnst)**(1.0/aqexp))
     1          -botwid)/2.0/sidslp
      else
        dupper = (qmaxu/aqcnst)**(1.0/aqexp)/botwid
      endif
c
c     Print warning to user if dupper exceeds ridge height
c
      if(iplane.eq.irofe .and. dupper.gt.rh(iplane))
     1       write (6,2001)dupper*1000.0,rh(iplane)*1000.0
c
c     Determine horizontal infiltration constant
c
c      ---- The value in parenthesis is from that portion of WEPP
c           Equation 12.3 also in parenthesis.  HORCON is the square
c           root of it.
c           Note that avesm(iplane) = (total head loss)*dtheta(iplane).
c
      horcon = sqrt(2.0*tmpvr1*tmpvr2)
c
c     Determine maximum horizontal wetting distance and time when
c     horizontal wetting fronts meet
c
c      ---- (WEPP Equation 12.5, with W = float(furint)*rwdth)
      hormax = (float(furint)*rwdth-botwid)*0.5
      timhmx = (hormax/horcon)**2
c
c     Determine vertical advance distance at t=timhmx
c
      infold = timhmx*aveks(iplane)
      call newton(timhmx,infold,aveks(iplane),avesm(iplane),vertmx)
      vertmx = vertmx/dtheta(iplane)
c
c     Determine 1-D vert. distance to the wetting front for t=timtot
c
      infold = timtot*aveks(iplane)
      call newton(timtot,infold,aveks(iplane),avesm(iplane),
     1            infilv(nsteps))
      infilv(nsteps) = infilv(nsteps)/dtheta(iplane)
c
c     Determine deltai
c
      deltai = infilv(nsteps)/float(nsteps)
c
c     Determine 1-D horizontal distance to the wetting front for
c     t=timtot
c
      if(timtot.lt.timhmx)then
c
c       ---- (WEPP Equation 12.3)
        infilh(nsteps) = horcon*sqrt(timtot)
      else
        infilh(nsteps) = hormax
      endif
c
c     Determine infiltration rate at t=timtot using 2-D infiltration
c     function
c
c      ---- (WEPP Equation 4.2.3) divided by change in soil water
c           content
      diydt = tmpvr1*(1.0+tmpvr2/infilv(nsteps))
c
c     If the following is true then horizontal wetting fronts have not
c     met.
c
      if(timtot.le.timhmx)then
c
c        ---- Time derivative of WEPP Equation 12.1
        kostfd = (infilh(nsteps)*(dupper/timtot+piovr2*infilv(nsteps)/
     1           (2.0*timtot)+piovr2*diydt)+botwid*diydt)*dtheta(iplane)
      else
        kostfd = diydt*tmpvr3
      endif
c
c     Calculate values for elements of infilh, infilv, and time arrays.
c     Also determine the minimum infiltration rate.
c
      do 400  i = 1, nsteps-1
c
c       Determine 1-D vertical distance to the wetting front
c
        infilv(i) = infilv(i-1)+deltai
c
c       Calculate time required for infiltration depth infilv(i)
c
c        ---- (WEPP Equation 12.2, solved for time (tau in 12.2))
        time(i) = infilv(i)/tmpvr1-avesm(iplane)/aveks(iplane)*
     1            log(1.0+infilv(i)/tmpvr2)
c
c       Determine 1-D horizontal advance distance to the wetting front
c
        if(time(i).lt.timhmx)then
c
c         ---- (WEPP Equation 12.3)
          infilh(i) = horcon*sqrt(time(i))
        else
          infilh(i) = hormax
        endif
c
c       Determine kostf using derivative of 2-D cumulative infiltration
c       function
c
c        ---- (WEPP Equation 4.2.3) divided by change in soil water
c             content
        diydt = tmpvr1*(1.0+tmpvr2/infilv(i))
        if(timtot.le.timhmx)then
c
c          ---- Time derivative of WEPP Equation 12.1
          kostf = (infilh(i)*(dupper/time(i)+piovr2*infilv(i)/(2.0*
     1            time(i))+piovr2*diydt)+botwid*diydt)*dtheta(iplane)
        else
          kostf = diydt*tmpvr3
        endif
        kostfd = min(kostf,kostfd)
  400 continue
c
c     Determine kostf using wetted perimeter at the upper end of the
c     furrow
c
      kostf = aveks(iplane)*(botwid+2.0*dupper*sqrt(1.0+sidslp**2))
c
c     Set kostf to minimum of possible values
c
      kostf = min(kostf,kostfd)
c
c     Determine maximum flow rate at the lower end of the OFE
c
   10 qmaxl = qmaxu-slplen(iplane)*kostf
c
c     Determine 2*(average depth of flow) for steady state conditions
c
      if(qmaxl.gt.0.0)then
        if(sidslp.gt.0.001)then
          dlower = (sqrt(botwid**2+4.0*sidslp*(qmaxl/aqcnst)**(1.0/
     1             aqexp))-botwid)/2.0/sidslp
        else
          dlower = (qmaxl/aqcnst)**(1.0/aqexp)/botwid
        endif
        avgdp2 = dupper+dlower
      else
        avgdp2 = dupper
      endif
c
c     Determine 2-D cumulative infiltration for t=timtot
c
      if(timtot.le.timhmx)then
c
c        ---- (WEPP Equation 12.1)
        infilt = (avgdp2*infilh(nsteps)+botwid*infilv(nsteps)+
     1           piovr2*infilh(nsteps)*infilv(nsteps))*dtheta(iplane)
      else
        inftmx = (avgdp2*hormax+botwid*vertmx+piovr2*hormax*vertmx)*
     1           dtheta(iplane)
        infilt = inftmx+(infilv(nsteps)-vertmx)*float(furint)*rwdth*
     1           dtheta(iplane)
      endif
c
c     Adjust 2-D cumulative infiltration to eliminate steady state
c     infiltration rate
c
      infadj = infilt-kostf*timtot
c
c     Adjust kostf if infadj is less than 0.000001 m**2
c
      if(infadj.lt.0.000001)then
        infadj = 0.000001
        testf = (infilt-infadj)/timtot
        if(testf.lt.0.999*kostf)then
          kostf = testf
        else
          kostf = 0.999*kostf
        endif
        goto 10
      endif
c
c     Determine values for functions of cumulative infiltration and time
c
      natlni = log(infadj)
      infil2 = infadj**2
      natlnt = log(timtot)
c
c     Calculate summations
c
      irsum1 = infil2
      irsum2 = infil2*natlnt
      irsum3 = irsum2*natlnt
      irsum4 = infil2*natlni
      irsum5 = irsum2*natlni
c
c     Initialize variables for calculation loop
c
      i = 0
c
c     Calculation loop
c
  200 i = i+1
c
c       Determine 2-D cumulative infiltration
c
        if(time(i).le.timhmx)then
c
c          ---- (WEPP Equation 12.1)
          infilt = (avgdp2*infilh(i)+botwid*infilv(i)+piovr2*
     1             infilh(i)*infilv(i))*dtheta(iplane)
        else
          infilt = inftmx+(infilv(i)-vertmx)*float(furint)*rwdth*
     1             dtheta(iplane)
        endif
c
c       Adjust 2-D cumulative infiltration to eliminate steady state
c       infiltration rate
c
        infadj = infilt-kostf*time(i)
c
c       Adjust kostf if infadj is less than 0.0001 m**2
c
        if(infadj.lt.0.000001)then
          testf = (infilt-infadj)/time(i)
          if(testf.lt.0.999*kostf)then
            kostf = testf
          else
            kostf = 0.999*kostf
          endif
          goto 10
        endif
c
c       Determine values for functions of infadj and time
c
        natlni = log(infadj)
        infil2 = infadj*infadj
        natlnt = log(time(i))
        tmpvr4 = infil2*natlnt
c
c       Perform summations
c
        irsum1 = irsum1+infil2
        irsum2 = irsum2+tmpvr4
        irsum3 = irsum3+tmpvr4*natlnt
        irsum4 = irsum4+infil2*natlni
        irsum5 = irsum5+tmpvr4*natlni
c
c     Check for completion of calculation loop
c
      if(i.lt.nsteps-1)goto 200
c
c     Determine Kostiakov-Lewis infiltration equation constants
c
      denom = irsum2*irsum2-irsum1*irsum3
      kosta = (irsum2*irsum4-irsum1*irsum5)/denom
      if(kosta.gt.1.0)then
        kosta = 1.0
        kostk = infadj/timtot
      else
        kostk = exp((irsum2*irsum5-irsum3*irsum4)/denom)
      endif
c
      return
      end
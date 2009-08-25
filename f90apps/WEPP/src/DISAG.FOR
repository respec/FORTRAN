      subroutine disag(wmlavg)
c
c     + + + PURPOSE + + +
c     Disaggregates storms into a double exponential intensity
c     pattern with relative time to peak intensity, Tp, and
c     relative maximum intensity, Ip = Max INT/AVE intensity,
c     satisfying 0 < Tp < 1 and Ip >= 1.
c     Also reads precipitation amount P, and storm duration DUR.
c
c     Called from IDAT.
c     Author(s): Lane, Lopez, Stone, FERRIS
c     Reference in User Guide:
c
c     Changes:
c         1) Order of parameters reversed to conform to Coding
c            Convention.
c         2) Code for Cases 1-4 massively re-arranged, and truth
c            table added to comments.
c         3) Local variables PMAX & DMAX deleted.
c         4) van der Sweep corrections added 2/92  -  dcf
c         5) Kottwitz changed IRDEPT to IRDEPT(mxplan) -  dcf
c         6) Stone changed time step from 2 to 5 minutes
c            to smooth out disaggregation function 5/93  dcf
c         7) Savabi changes for new winter routines and handling
c            of snow melt water.  Collapsed Cases 2&3 into Case 2,
c            Case 4 became case 3, Case 5 became Case 4.  Question
c            still of how to handle irrigation and snowmelt - we
c            don't handle it here yet.    dcf  2/24/94
c         8) Changed variable "int()" to "intsty()" to prevent
c            conflicts with external function of same name.  dcf 5/94
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/30/91.
c     Recoded by: Charles R. Meyer.
c     Recoded version updated: 9/13/91,1/92,7/92,5/93,5/94
c     Updated by: Dennis C. Flanagan and Reza Savabi
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real wmlavg      
c
c     + + + ARGUMENT DEFINITIONS + + +
c     xmxint - MAXIMUM RAINFALL INTENSITY
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cdiss1.inc'
c     modify: ninten(mxplan),dur,timem(20),intsty(20)
c
      include 'cdiss2.inc'
c     modify: timedl(20),intdl(20)
c
      include 'cdiss3.inc'
c     modify: timep,ip,p
c      write: deltfq,fq
c
      include 'chydrol.inc'
c       read: rain(mxplan),stmdur
c     modify: avrint
c
      include 'cirriga.inc'
c       read: irdept(mxplan)
c
      include 'cirspri.inc'
c       read: irdur, irint(iplane)
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cupdate.inc'
c         read:sdate
c
      include 'cwint.inc'
c       read: hrmlt(24,mxplan)
c
cd    Added by S. Dun Feb. 04, 2004
      include 'cxmxint.inc'
cd    End adding
c
c     + + + LOCAL VARIABLES + + +
      real avgint, rmxint, frstim, sectim, avmert, timmel, totmel,
     1     wmelmx, wmtp
      integer irflg2, loopfg, i, im
c
c     + + + LOCAL DEFINITIONS + + +
c     avgint - AVERAGE RAINFALL INTENSITY (m/s)
c     rmxint - NON DIMENSIONAL MAXIMUM RAINFALL INTENSITY
c     frstim - real value for previous time when checking if
c              disaggregated time step is less than 5 minutes
c     sectim - real value for current time when checking if
c              disaggregated time step is less than 5 minutes
c     irflg2 - flag.  1=rainfall and irrigation occur on same day, no
c                     snowmelt; 0=all other cases
c     loopfg - flag.  1=exit loop; 2=exit loop and go to 20;
c                     0=next iteration of loop.
c
c     + + + SUBROUTINES CALLED + + +
c     const
c     dblex
c
c     + + + END SPECIFICATIONS + + +
c
c
c     initialize variables
c
      ninten(iplane) = 11
      deltfq = 1.0 / float(ninten(iplane)-1)
      fq = 0.0
      timedl(1) = 0.0
      intdl(ninten(iplane)) = 0.0
      irflg2 = 0
      p = 0
c
c     ****************************
c     Truth Table of       *
c     Cases Covered:       *
c     *
c     | IR | SM | RN | Case  *
c     ---------------------  *
c     |  n |  N |  Y |  1    *
c     |  n |  Y |  Y |  2    *
c     |  n |  Y |  N |  2    *
c     |  y |  N |  n |  3    *
c     |  y |  N |  y |  4    *
c     *
c*****************************
c
c     *** L1 IF ***
      if (irdept(iplane).lt.0.0001.or.wmelt(iplane).ge.0.0001) then
        if (irdept(iplane).ge.0.0001) write (6,1000) iplane
c
c       STMDUR = DURATION OF RAINFALL IF RAINFALL OCCURS (STMGET.FOR)
        if (wmelt(iplane).le.0.0) then

c         Case 1:  RAINFALL ONLY - no irrigation OR SNOWMELT
c
          p = rain(iplane)
          dur = stmdur
        else

c         Case 2:  SNOWMELT  (what about case of irrig & melt??)
c
c         Reza added the following code to disaggregate the melt
c         Assume the following variable is provided by winter routine

          wmelmx=0.0
          timmel=0.0
          totmel=0.0
          do 10 im=1,24
            if(hrmlt(im,iplane).gt.0.000)then
              timmel=timmel+1
              totmel=totmel+hrmlt(im,iplane)
              if(hrmlt(im,iplane).gt.wmelmx)then
                wmelmx=hrmlt(im,iplane)
                wmtp=timmel
              endif
            endif
   10     continue
          avmert=totmel/timmel*wmlavg      
          ip=wmelmx/avmert*wmlavg      
          timep=wmtp/timmel

c
c     stmdur is in h, wmelmx is in m/h
          stmdur=timmel*3600
          dur=stmdur
          xmxint(iplane)=wmelmx/3600*wmlavg      
          p=totmel*wmlavg      
cd    End modifying
        endif
c
c     *** L1 ELSE ***
      else
c
c       *** L2 IF ***
        if (rain(iplane).lt.0.0001) then
c
c         Case 3:  irrigation - no rain OR SNOWMELT
c
          p = irdept(iplane)
          dur = irdur
          avrint = irint(iplane)
          xmxint(iplane) = avrint
          ip = 1.
c
c       *** L2 ELSE ***
        else
c
c         Case 4:  irrigation, rain - NO SNOWMELT
c
          irflg2 = 1
          p = rain(iplane)
          dur = stmdur
c
          if (irdur.gt.stmdur) then
            ninten(iplane) = ninten(iplane) - 1
            deltfq = 1.0 / float(ninten(iplane)-1)
          end if
c
c       *** L2 ENDIF ***
        end if
c     *** L1 ENDIF ***
      end if
c
      if(ip.lt.1.0)ip = 1.0
      if (timep.gt.1.0.or.ip.eq.1.0) then
        timep = 1.0
      else if (timep.le.0.0)then
        timep = 0.01
      else
        continue
      end if
c
   20 continue
      loopfg = 0
c
c     Call CONST if intensity is constant,
c     call DBLEX if it is not.
c
      if (timep.ge.1.0.and.ip.le.1.0) then
        call const
      else
        call dblex
      end if
c
      frstim = timedl(1) * dur
c
      i = 1
   30 continue
      i = i + 1
      sectim = timedl(i) * dur
      if (sectim-frstim.lt.300.) then
c
c       Time step is less than 5 minutes.  Decrease the number
c       of dimensionless steps and try again.
c
        ninten(iplane) = ninten(iplane) - 1
c
        if (ninten(iplane).le.2) then
c
c         Disaggregated rainfall distribution is set to
c         constant intensity with 2 time steps.
c
          timedl(2) = 1.
          intdl(1) = 1.
          intdl(2) = 1.
          ninten(iplane) = 2
        else
c
c         Re-initialize for decreased step.
c
          deltfq = 1.0 / float(ninten(iplane)-1)
          fq = 0.0
          timedl(1) = 0.0
          intdl(ninten(iplane)) = 0.0
          loopfg = 2
        end if
      else
        frstim = sectim
      end if
      if (i.lt.ninten(iplane).and.loopfg.eq.0) go to 30
      if (loopfg.eq.2) go to 20
c
c     Make sure last intensity value is 0.0
c
      intdl(ninten(iplane)) = 0.0
c
c     If rainfall and irrigation occur on the same day,
c     adjust dimensionalized intensities and times.
c
c     *** M0 IF ***
      if (irflg2.ne.0) then
        p = rain(iplane) + irdept(iplane)
        dur = amax1(irdur,stmdur)
        avgint = p / dur
        rmxint = 0.
c
c       Adjust dimensionless intensity, average intensity, and maximum
c       intensity.
c
        do 40 i = 1, ninten(iplane)
          if (timedl(i+1)*stmdur.le.irdur) then
            intdl(i) = (intdl(i)*avrint+irint(iplane)) / avgint
          else if (timedl(i)*stmdur.lt.irdur) then
            intdl(i) = (intdl(i)*avrint+(irint(iplane)*(irdur-
     1          timedl(i)*stmdur))/(timedl(i+1)-timedl(i))/stmdur) /
     1          avgint
          end if
          rmxint = amax1(rmxint,intdl(i))
   40   continue
c
        xmxint(iplane) = amax1(xmxint(iplane),rmxint*avgint)
        avrint = avgint
c
c       If irrigation duration is greater than rainfall duration,
c       add last intensity duration block and adjust dimensionless time
c
        if (irdur.gt.stmdur) then
          ninten(iplane) = ninten(iplane) + 1
          intdl(ninten(iplane)) = 0.
          timedl(ninten(iplane)) = 1.
c
          do 50 i = 2, ninten(iplane) - 1
            timedl(i) = timedl(i) * stmdur / irdur
   50     continue
c
        end if
c
c     *** M0 ENDIF ***
      end if
c
c     Calculate actual time and intensity
c
      do 60 i = 1, ninten(iplane)
        timem(i) = timedl(i) * dur
c
c       changed p and dur to meters and second above so changed here.
c
        intsty(i) = intdl(i) * p / dur
   60 continue
c
      return
 1000 format (3x,' *** WARNING ***',/,' Fixed date sprinkler irrigation'
     1       ,/,3x,' skipped on plane ',i2,' due to snowmelt.',/,
     1       ' *** WARNING ***',/)
      end

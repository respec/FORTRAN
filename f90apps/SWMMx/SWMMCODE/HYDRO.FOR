      SUBROUTINE HYDRO
C=======================================================================
C     Hydro was last updated May 1993 by R.E.D.
C     Updated 8/93 by Chuck Moore, CDM to include infiltration/inflow
C     and new statistical summaries.
C     Correct rainfall total for situation in which THISTO < WET, RED,
C       12/31/93.
C     Cosmetic changes to continuity check printouts, WCH, 1/4/94.
C     Add check for snow on surface and channel/pipe flow in order to 
C       use WETDRY time step during continuous simulations, WCH, 4/7/94. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
C#### C. MOORE, CDM, 8/93.
      INCLUDE 'RDII.INC'
      INCLUDE 'RUNSTAT.INC'
      DIMENSION ZUM(30),REIN(10),BANE(10),JSTA(10)
C=======================================================================
C     DECLARE THESE VARIABLES.  WCH, 11/1/91
C=======================================================================
      REAL   TRN
      DOUBLE PRECISION DTRN
      LOGICAL DELUGE,DESERT
C=======================================================================
C     Compute the initial water stored in channels.
C=======================================================================
      CNT(20)    = 0.0
      IF(NOG.NE.999) THEN
             DO 100 J = 1,NOG
             IF(NPG(J).EQ.1) AX = 0.5*(GS1(J)+GS2(J))*GDEPTH(J)**2 +
     +                                 GWIDTH(J)*GDEPTH(J)
             IF(NPG(J).EQ.2) AX = GWIDTH(J)**2/4.0*(GDEPTH(J) -
     +                                0.5*SIN(2.0*GDEPTH(J)))
             IF(NPG(J).EQ.4) THEN
                       WIDTH = GWIDTH(J)*SQRT(GDEPTH(J)/DFULL(J))
                       AX    = 0.66666667*WIDTH*GDEPTH(J)
                       ENDIF
             IF(NPG(J).NE.3) THEN
                             VOL    = AX*GLEN(J)
                             CNT(20)  = CNT(20) + VOL + QSUR(J)
                             ENDIF
  100        CONTINUE
             ENDIF
C=======================================================================
      DELUGE  = .FALSE.
      DESERT  = .TRUE.
C#######################################################################
C     C. Moore, CDM, 8/93.  Option to pre-process rainfall to compute
C       infiltration/inflow for inlet at each subcatchment.
C#######################################################################
      CALL RDIIRES
C
      NREIN   = NSCRAT(1)
      REWIND NREIN
      IF(IPRN(2).EQ.0) THEN
                       NFLOW   = NSCRAT(4)
                       IF(NFLOW.EQ.0) CALL ERROR(20)
                       REWIND NFLOW
                       ENDIF
      READ(NREIN,END=777,ERR=777) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
      WRITE(N6,2115)  NSTA
      WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
      IF(NRGAG.GT.NSTA) CALL ERROR(115)
C=======================================================================
C     The next DO loop is the driver of the Runoff Block.
C     It will call subroutines Wshed, Qshed amd Gutter.
C=======================================================================
      TRAIN  = TZERO
      TIME   = TZERO
      KWIK   = 0
C#### WCH, 4/7/94.  Initialize KWIKSN and KWIKGT.
                     KWIKSN = 0
      IF(ISNOW.EQ.2) KWIKSN = 1
      KWIKGT = 0
      LWEAT  = 0
      KRAIN  = 0
      NRAIN  = 0
      JWET   = 1
      JDRY   = 0
      MTIME  = 0
C#### WCH, 4/7/94.  ADD PARAMETER KWIKGT
      CALL GUTTER(REIGN,KWIKGT)
C=======================================================================
C     Calculate ending Julian day.
C=======================================================================
      CALL NDATE(LONG,JDAY,TMDAY)
      LONG   = LONG + TZERO
      WRITE(*,23) (LONG/3600.0),JDAY
      DELT   = 0.0
      DLAST  = 0.0
      JDAY   = JULDAY
      TMDAY  = TIMDAY
C=======================================================================
C     Recapture starting date/time.
C=======================================================================
      DO 2000 MBIG = 1,1000000
C=======================================================================
      IF(KRAIN.LE.MRAIN) THEN
C=======================================================================
C     If current time >= rainfall time, need to read new rainfall.
C=======================================================================
         IF(JULDAY.GT.JDAY.OR.(JULDAY.EQ.JDAY.AND.TIMDAY.GE.TMDAY)) THEN
                   DELUGE = .FALSE.
                   DESERT = .TRUE.
 200               READ(NREIN,END=800,ERR=777,IOSTAT=IOS) JDAY,TMDAY,
     +                                THISTO, (BANE(J),J=1,NSTA)
C#### WCH (CDM, C. MOORE), 10/93
                   DO 201 J=1,NSTA
 201               RTOT(J)=RTOT(J)+BANE(J)*THISTO
                   CALL NTIME(JDAY,TMDAY,TRN)
                   DTRN = DBLE(TRN)
                   IF(DABS(DTRN).LT.WET*0.001) TRN = 0.0
                   TRAIN = TRN
                   KRAIN = KRAIN + 1
                   NRAIN = NRAIN + 1
C#######################################################################
C#### WCH, 4/7/94.  If no precipitation  .AND.  snow on ground (KWIKSN
C     = 1) or if overland flow is occuring (KWIK = 1) or channel/pipe 
C     flow is occuring (KWIKGT = 1), use WETDRY time step.  For better
C     accuracy, user must set WETDRY closer to WET, at expense of 
C     computation time.  Check for none of the above (dry conditions 
C     and DRY time step) is made later.
C=======================================================================
                   IF(TRAIN.EQ.0.0)  THEN
C=======================================================================
C     Here, have rain value at this time.  Will assign WET time step
C     later.
C=======================================================================
                                TRAIN  = THISTO + TRAIN
                                JDRY   = 0
                                JWET   = 1
                                ELSE IF(TRAIN.GT.0.0) THEN
C=======================================================================
C     Here, have rain value at future time.  Will use WETDRY time step
C     unless DRY is determined later.
C=======================================================================
                                JDRY   = IFIX(TRAIN/WETDRY)
                                IF(JDRY.EQ.0) THEN
                                              JDRY  = 1
                                              XDELT = TRAIN
                                              ELSE
                                              XDELT = TRAIN/FLOAT(JDRY)
                                              ENDIF
                                TRAIN  = THISTO + TRAIN
                                ELSE
C=======================================================================
C     Here, still need to read rainfall value at time >= current time.
C=======================================================================
                                GO TO 200
                                ENDIF
                   IF(THISTO.LT.WET) THEN
C#######################################################################
C RED (WCH), 12/31/93. If THISTO < WET, change the time step to THISTO
C                      to ensure calculation of correct rainfall amount.
C                      Change the time step, not the rainfall.
C                      Add new variable IMSHORT.
C#######################################################################
C#### Old code:
C####                           DO 300 J = 1,NRGAG
C#### 300                       BANE(J)  = BANE(J)*THISTO/WET
C####                           JWET     = 1
C####                           ELSE
C####                           JWET     = IFIX(THISTO/WET)
C####                           ENDIF
C#### New code:
                                JWET     = 1
                                IMSHORT  = 1
                                ELSE
                                JWET     = IFIX(THISTO/WET)
                                IMSHORT  = 0
C#### Check for some slop.
                                IF(FLOAT(JWET)*WET.LT.THISTO-0.01) THEN
                                     JWET    = JWET + 1
                                     IMSHORT = 2
                                     ENDIF
                                ENDIF
C
                   GO TO 801
C=======================================================================
C     Here if reach end-of-file on precipitation input file.
C=======================================================================
  800              CALL NDATE (LONG,JDAY,TMDAY)
C#### 5/28/93, RED.  REMOVE TRN=TRAIN AND ADD TRAIN=TRN AFTER CALL STMT.
C####             TRN = TRAIN
                   CALL NTIME (JDAY,TMDAY,TRN)
                   TRAIN = TRN
                   JDRY   = IFIX(TRAIN/WETDRY)
                   IF(JDRY.EQ.0) THEN
                            JDRY  = 1
                            XDELT = TRAIN
                            ELSE
                            XDELT = TRAIN/FLOAT(JDRY)
                            ENDIF
                   JWET   = 0
                   DO 810 J = 1,NRGAG
  810              BANE(J)  = 0.0
  801              CONTINUE
                   ENDIF
         ELSE
C=======================================================================
C     Here, have read future rainfall value.  Use zero until reach
C     that time.
C=======================================================================
         DO 820 J = 1,NRGAG
  820    BANE(J)  = 0.0
         ENDIF
C=======================================================================
C     Wet, Wet/Dry or Dry Time Step.
C=======================================================================
      NSTEP  = 1
 2050 MTIME  = MTIME + 1
C=======================================================================
C     DELUGE = .TRUE.   ==> Wet time step.
C     DELUGE = .FALSE.  ==> Dry or wet/dry time step.
C
C     KWIK   =    0     ==> No overland flow.
C     KWIK   =    1     ==> Some overland flow occuring.
C#### WCH, 4/7/94.
C     KWIKSN =    0     ==> No snow on catchment surface.
C     KWIKSN =    1     ==> Snow somewhere in catchment.
C
C     KWIKGT =    0     ==> No channel/pipe routing occuring.
C     KWIKGT =    1     ==> Some channel/pipe routing occuring.
C
C     Set DELUGE = TRUE if reach a time with precipitation.
C=======================================================================
      IF(NSTEP.GT.JDRY) DELUGE = .TRUE.
      IF(DELUGE) THEN
C#### RED (WCH), 12/31/93.  Calculate DELT based on IMSHORT.
                 IF(IMSHORT.EQ.0) DELT   = WET
                 IF(IMSHORT.EQ.1) DELT   = THISTO
                 IF(IMSHORT.EQ.2) DELT   = THISTO/FLOAT(JWET)
C####                 DELT   = WET
                 DO 305 J = 1,NRGAG
  305            REIN(J)  = BANE(J)
C=======================================================================
C     Here if DELUGE is false (not a time step with precipitation).
C
C     Use WETDRY time step for:
C     - remaining overland flow
C     - snow on surface
C     - remaining channel/pipe flow
C     - groundwater flow to channel/pipes.
C
C     Use DRY time step if none of the above.  Thus, DRY applies to 
C     lingering evaporation, infiltration and groundwater ET/seepage.
C
C     NOTE: Time steps are only approximately equal to DRY or WETDRY. 
C     Divide interval between end of precip (or snow) and next precip 
C     into intervals nearest to DRY or WETDRY.  This means that 
C     calculations and time step print-outs do not necessarily come at
C     even time intervals corresponding to DRY or WETDRY entered by the
C     user.  
C=======================================================================
                 ELSE
                 DO 310 J = 1,NRGAG
  310            REIN(J)  = 0.0
C=======================================================================
C#### WCH, 4/7/94.  Change code to add check for snow on surface or 
C     overland flow or channel/pipe flow.  Need to use WETDRY time step 
C     if there is snow because don't know when it will melt.
C
C     For snow, variable KWIKSN is initialized in WSHED and changed 
C     (if snow is present) in SNOW.
C
C     Check for presence of channel/pipe routing in GUTTER.
C=======================================================================
                 IF((ISNOW.GT.0.AND.KWIKSN.EQ.1).OR.KWIKGT.EQ.1) KWIK=1
C=======================================================================
C     Purpose of logical variable DESERT is to ensure that only pass 
C     through following IF-THEN loop once, to set time step at beginning
C     of a dry or wet/dry period.  
C     This is the only place where DRY time step is established.  
C=======================================================================
                 IF(KWIK.EQ.0.AND.DESERT) THEN
                               TMREM = FLOAT(JDRY-NSTEP+1)*XDELT
                               JDRY  = IFIX(TMREM/DRY)
                               IF(JDRY.EQ.0) THEN
                                             JDRY  = 1
                                             DELT  = TMREM
                                             ELSE
                                             DELT = TMREM/FLOAT(JDRY)
                                             ENDIF
                               DESERT = .FALSE.
                               NSTEP  = 1
                               ENDIF
C=======================================================================
C     Similarly, if water is on surface (or KWIK is otherwise set to 1),
C     then only pass through following IF-THEN once, to set up time
C     intervals.
C=======================================================================
                 IF(KWIK.GT.0.AND. .NOT.DESERT) THEN
                               TMREM = FLOAT(JDRY-NSTEP+1)*DELT
                               JDRY  = IFIX(TMREM/WETDRY)
                               IF(JDRY.EQ.0) THEN
                                             JDRY  = 1
C#### WCH, 4/7/94.  CHANGE DELT TO XDELT IN ORDER TO USE THESE CALCS.
                                             XDELT  = TMREM
                                             ELSE
C#### WCH, 4/7/94.  CHANGE DELT TO XDELT IN ORDER TO USE THESE CALCS.
                                             XDELT = TMREM/FLOAT(JDRY)
                                             ENDIF
                               DESERT = .TRUE.
                               NSTEP  = 1
                               ENDIF
                 IF(KWIK.GT.0) DELT = XDELT
                 ENDIF
      REIGN  = REIN(1)
      TIME   = TIME + DELT
      FINF   = CNT(4)
      IF(TIME.GT.LONG) THEN
                       DELT = LONG - TIME + DELT
                       TIME = LONG
                       IF(NRAIN.GT.0) NRAIN = NRAIN - 1
                       ENDIF
C=======================================================================
C#### WCH, 11/30/93.  TRY DEFINITION OF DLAST AND DMEAN CONSISTENT WITH 
C                     OTHER BLOCKS AND COMPUTATIONS.  USE NEW DELT FOR 
C                     DMEAN IF PAST TIME STEP WAS DRY (NO RUNOFF TO 
C                     INLETS, LWEAT = 0).  
C=======================================================================
                     DMEAN = 0.5 * (DLAST + DELT)
      IF(LWEAT.EQ.0) DMEAN = DELT
C     
      CALL STIME(DELT)
      WRITE(*,22) MTIME,TIME/3600.0,JULDAY
C=======================================================================
C     Watershed Elements (Overland Flow).
C=======================================================================
      CALL WSHED(REIN,KWIK)
C#######################################################################
C     C. Moore, CDM, 8/93.  I/I timestep calculations.
C=======================================================================
      IF (RRMAX.GT.0.0) CALL RDIISHED
C=======================================================================
C     Watershed Quality contributions.
C=======================================================================
      IF(KWALTY.EQ.1) CALL QSHED
C=======================================================================
C#### WCH, 9/93.  Add infiltration/inflow to subcatchment flows after
C       performing quality calculations.
C=======================================================================
      IF(RRMAX.GT.0.0) THEN
           DO 220 JK = 1,NOW
  220      WFLOW(JK) = WFLOW(JK) + FLOWII(JK)
           ENDIF
C=======================================================================
C     Channel/Pipe Elements.
C=======================================================================
C#### WCH, 4/7/94.  ADD PARAMETER KWIKGT
      CALL GUTTER(REIGN,KWIKGT)
C=======================================================================
C     Sum inlet flows over the basin.
C=======================================================================
      FLWOFF    = 0.0
      DO 250 JK = 1,NOW
      IF(NGTOI(JK).EQ.0) GO TO 255
      NX        = NGTOI(JK)
  250 FLWOFF    = FLWOFF  + OUTFLW(NX)
  255 CNT(5)    = CNT(5)  + FLWOFF*DMEAN
      FINF      = (CNT(4) - FINF)/DELT
      DLAST = DELT
C=======================================================================
C#### WCH, 11/30/93.  TRY DEFINITION OF DLAST/DMEAN CONSISTENT WITH 
C                     BLOCKS.
C####      IF(LWEAT.EQ.0) DLAST = WET
C####                     LWEAT = KWIK
C=======================================================================
      LWEAT = 1
      IF(FLWOFF.LE.0.0) LWEAT = 0
C
      IF(IPRN(2).EQ.0) WRITE(NFLOW,ERR=778,IOSTAT=IOS) TIME,FLWOFF,FINF
C=======================================================================
      IF(TIME.EQ.LONG) GO TO 2100
      NSTEP = NSTEP + 1
      IF(NSTEP.LE.JDRY+JWET) GO TO 2050
 2000 CONTINUE
 2100 CONTINUE
      CALL DATED
      WRITE(N6,272) MONTH,NDAY,NYEAR,MTIME,JULDAY,TIMDAY,TIMDAY/3600.0,
     +              TIME/3600.0,TIME/86400.0
C=======================================================================
C     Write the extrapolation summary.
C=======================================================================
      WRITE(N6,8000)
      DO 8100 J = 1,NOW,3
      JJ        = J + 2
      IF(JJ.GT.NOW) JJ = NOW
      IF(JCE.EQ.0) WRITE(N6,8020) (NAMEW(K),LND(K),KLND(K),K=J,JJ)
      IF(JCE.EQ.1) WRITE(N6,8021) (KAMEW(K),LND(K),KLND(K),K=J,JJ)
 8100 CONTINUE
C=======================================================================
      IF(NOG.GT.0.AND.NOG.NE.999) THEN
             WRITE(N6,8010)
             DO 8200 J = 1,NOG,3
             JJ        = J + 2
             IF(JJ.GT.NOG) JJ = NOG
             IF(JCE.EQ.0) WRITE(N6,8020) (NAMEG(K),
     +                                    LCHAN(K),KCHAN(K),K=J,JJ)
             IF(JCE.EQ.1) WRITE(N6,8021) (KAMEG(K),
     +                                    LCHAN(K),KCHAN(K),K=J,JJ)
 8200        CONTINUE
             ENDIF
C=======================================================================
C     Calculate final subsurface storage.
C=======================================================================
      GRAREA     = 0.0
      IF(NOGWSC.GT.0) THEN
      DO 286 JE1 = 1,NOGWSC
      GRAREA     = GRAREA + WAREA(JE1)
 286  CNT(16)    = CNT(16)+TH1(JE1)*(GRELEV(JE1)-STG(JE1))*WAREA(JE1) +
     +             POR(JE1)*(STG(JE1)-BELEV(JE1))*WAREA(JE1)
      ELSE
      GRAREA     = TRIBA
      ENDIF
C=======================================================================
C     Compute water volume on subcatchments.
C=======================================================================
                     KKK = 3
      IF(ISNOW.GT.0) KKK = 4
      CNT(10)  = 0.0
      DO 288 K = 1,KKK
      DO 288 J = 1,NOW
  288 CNT(10)  = CNT(10) + WDEPTH(K,J)*WAR(K,J)
C=======================================================================
C     Compute snow volume on subcatchments.
C=======================================================================
      IF(ISNOW.GT.0) THEN
            DO 290 J = 1,NOW
            CNT(8) = CNT(8)+(WSNOW(2,J)+FW(2,J))*WAR(2,J)
            CNT(8) = CNT(8)+(WSNOW(1,J)+FW(1,J))*WAR(4,J)
  290       CNT(8) = CNT(8)+(WSNOW(3,J)+FW(3,J))*(WAR(1,J)+WAR(3,J))
            END IF
      DO 295 K  = 1,NRQ
  295 REFF(1,K) = 0.0
      IF(NOG.EQ.0.OR.NOG.EQ.999) GO TO 350
C=======================================================================
C     Compute water stored in channels.
C=======================================================================
      DO 340 J = 1,NOG
      IF(NPG(J).EQ.3) GO TO 340
C=======================================================================
C     Volume in TRAPEZOIDAL SECTIONS.
C=======================================================================
      IF(NPG(J).EQ.1) AX = 0.5*(GS1(J)+GS2(J))*GDEPTH(J)**2 +
     +                     GWIDTH(J)*GDEPTH(J)
C=======================================================================
C     Volume in circular sections. Compute area of segment of a circle.
C=======================================================================
      IF(NPG(J).EQ.2) AX = GWIDTH(J)**2/4.0*(GDEPTH(J) -
     +                     0.5*SIN(2.0*GDEPTH(J)))
      IF(NPG(J).EQ.4) THEN
                      WIDTH = GWIDTH(J)*SQRT(GDEPTH(J)/DFULL(J))
                      AX    = 0.66666667*WIDTH*GDEPTH(J)
                      ENDIF
      VOL    = AX*GLEN(J)
      CNT(7) = CNT(7) + VOL + QSUR(J)
C=======================================================================
C     Calculate remaining channel pollutant loads.
C     Store the sums in array REFF until reaching print routines.
C=======================================================================
      IF(NQS.NE.0) THEN
                   DO 335 K  = 1,NQS
                   RNEW      = VOL*C(K,J) + CVSUR(K,J)
  335              REFF(1,K) = REFF(1,K)  + RNEW
                   ENDIF
  340 CONTINUE
C=======================================================================
C     Compute continuity error due to computations.
C=======================================================================
  350 SUMALL  = CNT(4) + CNT(6) + CNT(19) + CNT(21) +
     +          CNT(8) + CNT(9) + CNT(10)
C=======================================================================
C     Print values in inches or millimeters over the basin.
C     The number 3630 converts cubic feet per acre to inches.
C=======================================================================
      TY       =  TRIBA*3630.0
      GY       = GRAREA*3630.0/43560.0
      DO 351 J = 1,30
      IF(J.LE.10.OR.J.GE.20) THEN
                             IF (TY.NE.0.0) THEN
                             ZUM(J)   = CNT(J)/TY
                             ELSE
                             ZUM(J)   = 0.0
                             END IF
                             ELSE
                             IF (GY.NE.0.0) THEN 
                             ZUM(J)   = CNT(J)/GY
                             ELSE
                             ZUM(J) = 0.0
                             END IF
                             ENDIF
  351 CONTINUE
      IF (TY.NE.0.0) THEN
                        ZALL    = SUMALL/TY
      ELSE 
                        ZALL    = 0.0
      END IF
      IF(PRCIMP.LT.1.0) ZUM(18) = ZUM(11)/(1.0-PRCIMP)
      IF(PRCIMP.LT.1.0) ZUM(19) = ZUM(12)/(1.0-PRCIMP)
C=======================================================================
C     IF METRIC = 2 ALL SUMMERS WILL HAVE TO BE CONVERTED TO CUBIC
C                   METERS AND MILLIMETERS FROM CUBIC FEET AND
C                   AND INCHES RESPECTIVELY.
C=======================================================================
      DO 355 J = 1,30
      ZUM(J)   = ZUM(J) * CMET(5,METRIC)
 355  CNT(J)   = CNT(J) / CMET(8,METRIC)
      SUMALL   = SUMALL / CMET(8,METRIC)
      ZALL     = ZALL   * CMET(5,METRIC)
C=======================================================================
C     Write the title for the continuity page depending on metric.
C=======================================================================
      IF(METRIC.EQ.1) WRITE (N6,360) CNT(1),ZUM(1)
      IF(METRIC.EQ.2) WRITE (N6,361) CNT(1),ZUM(1)
      IF(ISNOW.GT.0)  WRITE(N6,370)  CNT(2),ZUM(2),CNT(3),ZUM(3)
                      WRITE(N6,380)  CNT(4),ZUM(4),CNT(6),ZUM(6),
     1                               CNT(21),ZUM(21),CNT(10),ZUM(10)
      IF(ISNOW.GT.0)  WRITE(N6,390)  CNT(8),ZUM(8),CNT(9),ZUM(9)
C=======================================================================
C     Calculate the infiltration in inches over the pervious area only.
C=======================================================================
      IF(PRCIMP.LT.1.0)    ZUM4 = ZUM(4) / ( 1.0 - PRCIMP )
      Z1    = CNT(1) + CNT(3)
      Z2    = ZUM(1) + ZUM(3)
                    ERRER = 0.0
      IF(Z2.NE.0.0) ERRER = 100.0*(Z2 - ZALL)/Z2
      WRITE(N6,395) CNT(4),ZUM4,SUMALL,ZALL,Z1,Z2
      WRITE(N6,400) ERRER
C#### C. MOORE, CDM, 8/93.
      WRITE(*,401) ERRER
C#######################################################################
C     C. Moore, CDM, 8/93.  Write the I/I continuity check.
C#######################################################################
      IF (RRMAX.GT.0.0) THEN
           SUMOFRS = 0.0
           DO 3340 J=1,NOW
           DO 3340 I=1,3
 3340      SUMOFRS = SUMOFRS + RDIIR(J,I)*SEWAREA(J)
           SUMOFRS = SUMOFRS/RDIIAREA
           SUMRII  = 0.0
           DO 3350 I=1,3
 3350      SUMRII = SUMRII + CNTRDII(I)
           IF(CNEXCESS.NE.0.0) THEN
                ERRER = ((SUMRII/(RDIIAREA*3630.0)) - 
     +                  SUMOFRS*CNEXCESS)*100.0/(SUMOFRS*CNEXCESS)
                ELSE
                ERRER = 999.0
                IF(SUMRII.EQ.0.0) ERRER = 0.0
                ENDIF
C=======================================================================
C     Include metric options.
C=======================================================================
           IF(METRIC.EQ.1) WRITE(N6,3358)
           IF(METRIC.EQ.2) WRITE(N6,3359)
           IF(METRIC.EQ.1) 
     1       WRITE(N6,3360) CNRAIN*RDIIAREA*43560.0/12.0,CNRAIN,
     2       CNEXCESS*RDIIAREA*43560.0/12.0,CNEXCESS,SUMOFRS,
     3       SUMOFRS*CNEXCESS*RDIIAREA*43560.0/12.0,SUMOFRS*CNEXCESS,
     4       CNTRDII(1),(CNTRDII(1)/(RDIIAREA*3630.0)),
     5       CNTRDII(2),(CNTRDII(2)/(RDIIAREA*3630.0)),
     6       CNTRDII(3),(CNTRDII(3)/(RDIIAREA*3630.0)),
     7       SUMRII,(SUMRII/(RDIIAREA*3630.0))
           IF(METRIC.EQ.2) 
     1       WRITE(N6,3360) CNRAIN*RDIIAREA*43560.0/12.0/35.315,
     2       CNRAIN*25.4,CNEXCESS*RDIIAREA*43560.0/12.0/35.315,
     3       CNEXCESS*25.4,SUMOFRS,
     4       SUMOFRS*CNEXCESS*RDIIAREA*43560.0/12.0/35.315,
     5       SUMOFRS*CNEXCESS*25.4,
     6       CNTRDII(1)/35.315,CNTRDII(1)/(RDIIAREA*3630.0)*25.4,
     7       CNTRDII(2)/35.315,CNTRDII(2)/(RDIIAREA*3630.0)*25.4,
     8       CNTRDII(3)/35.315,CNTRDII(3)/(RDIIAREA*3630.0)*25.4,
     9       SUMRII/35.315,SUMRII/(RDIIAREA*3630.0)*25.4
           IF(IIRDII.EQ.0) THEN
                WRITE(N6,3361) ERRER
                WRITE(*,3362) ERRER
                ELSE
                WRITE(N6,3363)
                ENDIF
           ENDIF
C=======================================================================
C     Write the channel/pipe continuity check.
C=======================================================================
      IF(TY.NE.0.0) THEN 
           ZUM13   = CNT(13)/TY
           ELSE
           ZUM13   = 0.0
           ENDIF
      C1      = CNT(5)  + CNT(7)  + CNT(22)
      C2      = CNT(20) + CNT(21) + CNT(13)
      Z1      = ZUM(5)  + ZUM(7)  + ZUM(22)
C#### C. MOORE, CDM, 8/93
      ZZ1     = CNT(5)  + CNT(7)  + CNT(22)
      Z2      = ZUM(20) + ZUM(21) + ZUM13
                    ERRER = 0.0
C#### C. MOORE, CDM, 8/93
CCC   IF(Z1.NE.0.0) ERRER = 100.0*(Z1 - ZUM(20) - ZUM13 - ZUM(21))/Z1
      IF(ZZ1.NE.0.0) ERRER = 100.0*(ZZ1-CNT(20)-CNT(13)-CNT(21))/ZZ1
      IF(METRIC.EQ.1) WRITE(N6,365) CNT(20),ZUM(20),CNT(7),ZUM(7),
     +                CNT(21),ZUM(21)
      IF(METRIC.EQ.2) WRITE(N6,366) CNT(20),ZUM(20),CNT(7),ZUM(7),
     +                CNT(21),ZUM(21)
C#### C. MOORE, CDM, 8/93
      IF(RRMAX.NE.0.0) THEN
           IF(METRIC.EQ.1) WRITE(N6,369) SUMRII,SUMRII/TY
           IF(METRIC.EQ.2) WRITE(N6,369) SUMRII/35.315,
     +                     SUMRII/TY*25.4
           ERRER = 0.0
           IF(ZZ1.NE.0.0) ERRER = 100.0*(ZZ1-CNT(20)-CNT(13)-CNT(21)-
     +                            SUMRII)/ZZ1
           ENDIF
      WRITE(N6,367)   CNT(13),ZUM13,CNT(22),
     +                ZUM(22),CNT(5),ZUM(5),C2,Z2,C1,Z1,ERRER
C#### C. MOORE, CDM, 8/93
      WRITE(*,368) ERRER
C=======================================================================
C     Write the groundwater continuity check.
C=======================================================================
      IF(NOGWSC.GT.0) THEN
           SUMSBS = ZUM(11)+ZUM(12)+ZUM(13)+ZUM(14)+ZUM(16)
           IF((ZUM(17)+ZUM(15)).NE.0.0) THEN
                      SUBERR = (ZUM(17)+ZUM(15)-SUMSBS)/
     +                         (ZUM(17)+ZUM(15))*100.0
                      ELSE
                      SUBERR = 0.0
                      ENDIF
            WRITE(N6,356)
            IF(METRIC.EQ.1) WRITE(N6,358)
            IF(METRIC.EQ.2) WRITE(N6,357)
            WRITE(N6,870) CNT(17),ZUM(17),
     1                CNT(11),ZUM(11),CNT(12),ZUM(12),CNT(13),ZUM(13),
     2                CNT(14),ZUM(14),CNT(15),ZUM(15),CNT(16),ZUM(16),
     3                CNT(11),ZUM(18),CNT(12),ZUM(19)
            ENDIF
      IF(NOGWSC.NE.0) WRITE(N6,871) SUBERR
C#######################################################################
C     Chuck Moore, CDM, 8/93.  Call routine to print summary statistics
C       for channel/pipes.
C#######################################################################
      CALL SMSTAT
C=======================================================================
C     CALCULATE THE QUALITY LOAD REMAINING ON THE SURFACE
C     AND IN THE CATCHBasinS AT THE END OF THE SIMULATION
C     THE SUMS WILL BE STORED IN ARRAYS PSHED AND PBASIN
C     FOR EVENTUAL PRINTOUT IN SUBROUTINE PRPOLL
C=======================================================================
      IF(NQS.GE.1) THEN
                   SUMSRF   = 0.0
                   SUMCAT   = 0.0
                   DO 500 I = 1,NQS
                   DO 600 K = 1,NOW
                   SUMCAT   = SUMCAT + PBASIN(I,K)
                   DO 600 J = 1,N1
                   SUMSRF   = SUMSRF + PSHED(J,I,K)
 600               CONTINUE
C=======================================================================
C                 Change the units from milligrams to kilograms.
C=======================================================================
                   SUMSRF       = SUMSRF / 1.0E06
                   SUMCAT       = SUMCAT / 1.0E06
                   PSHED(1,I,1) = SUMSRF
                   PBASIN(I,1)  = SUMCAT
                   SUMSRF       = 0.0
                   SUMCAT       = 0.0
 500               CONTINUE
C=======================================================================
C     Calculate the remaining pollutant loads in the channel system.
C     Change the units of REFF from NDIM units * flow to units of KG.
C=======================================================================
                   DO 850  K = 1,NQS
                   ND        = NDIM(K)   + 1
 850               REFF(1,K) = REFF(1,K) * FACT3(ND) / 1.0E06
                   ENDIF
      RETURN
 777  WRITE(N6,9777) IOS
      STOP
 778  WRITE(N6,9778) IOS
      STOP
C=======================================================================
   22 FORMAT('+',25X,'Step=',I10,F10.2,' hours. Julian Date = ',I6)
   23 FORMAT(/,' Beginning time step loop.  End at time ',
     1           F11.2,' hours. Final date is ',I6,/,
     2 ' Current step/time = ',/)
  272 FORMAT(/,
     1' ************************************************',/,
     2' *      End of time step DO-loop in Runoff      *',/,
     3' ************************************************',//,
     4' Final Date (Mo/Day/Year)   = ',T50,I2,'/',I2,'/',I2,/,
     5' Total number of time steps = ',T50,I8,/,
     6' Final Julian Date  =',T45,5X,I8,/,
     7' Final time of day  =',T45,F13.0,' seconds.',/,
     8' Final time of day  =',T45,F13.2,'   hours.',/,
     9' Final running time =',T45,F13.4,'   hours.',/,
     9' Final running time =',T45,F13.4,'    days.')
 2115 FORMAT(//,
     +' ***************************************************',/,
     +' *  Precipitation Interface File Summary           *',/,
     +' *  Number of precipitation station....',I9,'   *',/,
     +' ***************************************************',/)
 2120 FORMAT(' Location Station Number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',I13,/))
 356  FORMAT(/,1H1,/,
     +' ***************************************************',/,
     +' *     Continuity Check for Subsurface Water       *',/,
     +' ***************************************************',/)
 357  FORMAT(70X,'Millimeters over',/,51X,'cubic meters',7X,
     .      'Subsurface Basin')
 358  FORMAT(70X,'Inches over',/,54X,'cubic feet',6X,
     .       'Subsurface Basin')
 360  FORMAT(1H1,/,
     +' **********************************************',/,
     +' *       Continuity Check for Surface Water   *',/,
     +' **********************************************',/,
     1 69X,'Inches over',/, 55X,'cubic feet    Total Basin',/,
     2 ' Total Precipitation (Rain plus Snow)',16X,1PE13.6,0PF11.3)
 361  FORMAT(1H1,/,
     +' **********************************************',/,
     +' *       Continuity Check for Surface Water   *',/,
     +' **********************************************',/,
     1 69X,'Millimeters over',/,55X,'cubic meters  Total Basin',/,
     2 ' Total Precipitation (Rain plus Snow)',16X,1PE13.6,0PF11.3)
 365  FORMAT(//,
     +' **********************************************',/,
     +' *       Continuity Check for Channel/Pipes   *',/,
     +' **********************************************',/,
     1 69X,'Inches over',/,55X,'cubic feet    Total Basin',/,
     2' Initial Channel/Pipe Storage................',
     2  8X,1PE13.6,0PF11.3,/,
     3' Final Channel/Pipe Storage..................',
     2  8X,1PE13.6,0PF11.3,/,
     4' Surface Runoff from Watersheds..............',
     2  8X,1PE13.6,0PF11.3)
 366  FORMAT(//,
     +' **********************************************',/,
     +' *       Continuity Check for Channel/Pipes   *',/,
     +' **********************************************',/,
     1 69X,'Millimeters over',/,55X,'cubic meters  Total Basin',/,
     2' Initial Channel/Pipe Storage................',
     2  8X,1PE13.6,0PF11.3,/,
     3' Final Channel/Pipe Storage..................',
     2  8X,1PE13.6,0PF11.3,/,
     4' Surface Runoff from Watersheds..............',
     2  8X,1PE13.6,0PF11.3)
  368 FORMAT(' Continuity Check for Channel/Pipes =      ',F10.2,
     1 ' Percent')
C#### C. MOORE, CDM, 8/93
  369 FORMAT(
     1' Total I/I Volume ...........................',
     2  8X,1PE13.6,0PF11.3)
  367 FORMAT(
     5' Groundwater Subsurface Inflow...............',
     2  8X,1PE13.6,0PF11.3,/,
     6' Evaporation Loss from Channels..............',
     2  8X,1PE13.6,0PF11.3,/,
     7' Channel/Pipe/Inlet Outflow..................',
     2  8X,1PE13.6,0PF11.3,/,
     7' Initial Storage + Inflow....................',
     2  8X,1PE13.6,0PF11.3,/,
     7' Final Storage + Outflow.....................',
     2  8X,1PE13.6,0PF11.3,/,
     8' ********************************************',/,
     9' * Final Storage + Outflow + Evaporation  - *',/,
     9' * Watershed Runoff - Groundwater Inflow  - *',/,
     9' *     Initial Channel/Pipe Storage         *',/,
     9' *     ----------------------------------   *',/,
     9' *  Final Storage + Outflow  + Evaporation  *',/,
     9' ********************************************',/,
     9' Error.......................................',F11.3,
     9' Percent')
 370  FORMAT (' Total Rain only',37X,1PE13.6,0PF11.3,/,
     1 ' Total Initial Snow Cover (Water equiv.)',13X,1PE13.6,0PF11.3)
 380  FORMAT (' Total Infiltration',34X,1PE13.6,0PF11.3,/,
     1 ' Total Evaporation                  ',17X,1PE13.6,0PF11.3,/,
     2 ' Surface Runoff from Watersheds     ',17X,1PE13.6,0PF11.3,/,
     4 ' Total Water remaining in Surface Storage',12X,1PE13.6,0PF11.3)
 390  FORMAT(' Total Water remaining in Snow Cover',17X,1PE13.6,
     .        0PF11.3,/,' Total Snow removed from Basin (Water equiv.)',
     .        8X,1PE13.6,0PF11.3)
 395  FORMAT(' Infiltration over the Pervious Area...',14X,1PE13.6,
     . 0PF11.3,/,'                       --------        ',/,
     .         ' Infiltration + Evaporation + ',/,
     .         ' Surface Runoff + Snow removal +',/,
     .         ' Water remaining in Surface Storage +',/,
     .         ' Water remaining in Snow Cover.........',
     .           14X,1PE13.6,0PF11.3,/,
     .         ' Total Precipitation + Initial Storage.',
     .           14X,1PE13.6,0PF11.3)
  400 FORMAT(//,' The error in continuity is calculated as',/,
     .      ' ***************************************',/,
     .      ' * Precipitation + Initial Snow Cover  *',/,
     .      ' *      - Infiltration -               *'/,
     .      ' *Evaporation - Snow removal -         *',/,
     .      ' *Surface Runoff from Watersheds -     *',/,
     .      ' *Water in Surface Storage -           *',/,
     .      ' *Water remaining in Snow Cover        *',/,
     .      ' *-------------------------------------*',/,
     .      ' * Precipitation + Initial Snow Cover  *',/,
     .      ' ***************************************',/,
     .      ' Error..................................',
     .       3X,F10.3,' Percent')
  401 FORMAT(/,' Continuity Check for Surface Water =      ',F10.2,
     1 ' Percent')
 870  FORMAT(/,' Total Infiltration',34X,1PE13.6,0PF11.3,/,
     .       ' Total Upper Zone ET',33X,1PE13.6,0PF11.3,/,
     .       ' Total Lower Zone ET',33X,1PE13.6,0PF11.3,/,
     .       ' Total Groundwater flow',30X,1PE13.6,0PF11.3,/,
     .       ' Total Deep percolation',30X,1PE13.6,0PF11.3,/,
     .       ' Initial Subsurface Storage',26X,1PE13.6,0PF11.3,/,
     .       ' Final Subsurface Storage',28X,1PE13.6,0PF11.3,/,
     .       ' Upper Zone ET over Pervious Area',
     .         20X,1PE13.6,0PF11.3,/,
     .       ' Lower Zone ET over Pervious Area',
     .         20X,1PE13.6,0PF11.3)
 871  FORMAT(//,' ******************************************',/,
     .          ' * Infiltration + Initial Storage - Final *',/,
     .          ' * Storage - Upper and Lower Zone ET -    *',/,
     .          ' * Groundwater Flow - Deep Percolation    *',/,
     .          ' * -------------------------------------- *',/,
     .          ' *     Infiltration + Initial Storage     *',/,
     .          ' ******************************************',/,
     .          ' Error ....................................',
     .            F10.3,1X,'Percent',//)
C#### C. MOORE, CDM, 8/93
 3358 FORMAT(1H1,/,
     +' *****************************************************',/,
     +' *       Continuity Check for Infiltration/Inflow    *',/,
     +' *****************************************************',/,
     1 68X,'Inches over',/, 54X,'cubic feet    Sewered Area')
 3359 FORMAT(1H1,/,
     +' *****************************************************',/,
     +' *       Continuity Check for Infiltration/Inflow    *',/,
     +' *****************************************************',/,
     1 68X,'mm over',/, 54X,'cubic meters  Sewered Area')
 3360 FORMAT(
     2 ' Approx. Total Rain for I/I (Avg. of hyetographs)....',
     3 1PE13.6,0PF11.3,/,
     4 ' Total I/I Excess (Rain minus storage losses)........',
     5 1PE13.6,0PF11.3,/,
     6 ' Area Weighted Average Total Ratio......',0PF13.6,/,
     7 ' Approx. Total Infiltration (Ratio*I/I Excess).......',
     8 1PE13.6,0PF11.3,/,
     9 ' Total Curve 1 Outflow Volume........................',
     1 1PE13.6,0PF11.3,/,
     2 ' Total Curve 2 Outflow Volume........................',
     3 1PE13.6,0PF11.3,/,
     4 ' Total Curve 3 Outflow Volume........................',
     5 1PE13.6,0PF11.3,/,
     6 ' Total I/I Outflow Volume............................',
     7 1PE13.6,0PF11.3,//)
 3361 FORMAT(' Approximate Continuity Error .................',
     9 0PF10.3,' Percent',//,
     1 ' Error computed as (Total I/I Outflow Volume - Approx. Total Inf
     2iltration)  /',/,19X,' (Approx. Total Infiltration)')
 3362 FORMAT(' Approximate Continuity Check for I/I     =',F10.2,
     1 ' Percent')
 3363 FORMAT(' Cannot compute I/I continuity error when using existing I
     1/I',/,' NSCRAT(8) rainfall file (IIRDII=1).')
 8000 FORMAT(/,
     +' **************************************************',/,
     +' *     Extrapolation Summary for Watersheds       *',/,
     +' * # Steps ==> Total Number of Extrapolated Steps *',/,
     +' * # Calls ==> Total Number of OVERLND Calls      *',/,
     +' **************************************************',//,
     +'  Subcatch   # Steps   # Calls  Subcatch   # Steps   # Calls  Sub
     +catch   # Steps   # Calls',/,
     +'  --------   -------   -------  --------   -------   -------  ---
     +-----   -------   -------')
8010  FORMAT(/,
     +' **************************************************',/,
     +' *     Extrapolation Summary for Channel/Pipes    *',/,
     +' * # Steps ==> Total Number of Extrapolated Steps *',/,
     +' * # Calls ==> Total Number of GUTNR Calls        *',/,
     +' **************************************************',//,
     +' Chan/Pipe   # Steps   # Calls Chan/Pipe   # Steps   # Calls Chan
     +/Pipe   # Steps   # Calls',/,
     +'  --------   -------   -------  --------   -------   -------  ---
     +-----   -------   -------')
8020  FORMAT(9I10)
8021  FORMAT(3(1X,A9,2I10))
9777  FORMAT(/,' ===> Error !!  Reading precipitation time history',
     1         ' on the NSCRAT(1) file.',/,
     2' Fortran error number =',I5,'. Run stopped from Sub. HYDRO.')
9778  FORMAT(/,' ===> Error !!  Writing inlet hydrograph time history',
     1         ' on the NSCRAT(4) file.',/,
     2' Fortran error number =',I5,'. Run stopped from Sub. HYDRO.')
C=======================================================================
      END

      SUBROUTINE GETCUR(JNO,XFROM,SLOPE,METRIC,KNORM,KCOND,
     +      AFULL,DEEP,TWFULL,XLEN,RN0,RFULL,NSTOP,IDNUM,KDNUM,KSTOP)
C=======================================================================
C     THIS ROUTINE READS THE INPUT FILE AND FINDS
C     FLOW/COEF CURVES FOR SECTIONS SPECIFIED IN EXTRAN OR TRANSPORT.
C     A.C. ROWNEY JUNE 1986, UPDATED J. E. SWANSON MARCH 1987
C     MODIFIED BY R. E. DICKINSON JANUARY, 1988, MAY, 1989.
C     MODIFIED BY WAYNE C. HUBER, HALLOWEEN, 1991 and August 1992.
C     MODIFIED BY WCH, 2/24/93 TO CORRECT HYDRAULIC RADIUS CALCULATION
C      FOR VERTICAL CROSS SECTIONS, AS PER HINTS FROM CHUCK MOORE AT CDM.
C     MODIFIED BY RED, 6/2/93 FOR USE WITH TRANSPORT BLOCK.
C     MODIFIED BY WCH (RED), 9/23/93 TO MULTIPLY STCHL ETC. BY PXSECR.
C     MODIFIED BY WCH, 11/30/93.  IF ROUGHNESS TRANSITION OCCURS EXACTLY
C      AT VERTICAL SECTION, ASSUME VERTICAL WALL SHOULD HAVE MAIN
C      CHANNEL ROUGHNESS.  CHANGE IF STATEMENT TO STRICTLY LT AND GT.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'FLODAT.INC'
      CHARACTER KDNUM*10
      DIMENSION ELSTA(2,100),DUMMYS(3,26)
C WCH MODIFICATION.  NEW ARRAY QTEMP(50)
      DIMENSION QTEMP(50)
      LOGICAL GOTOO,FAILED
C=======================================================================
C     JNO      = CONDUIT INPUT ORDER NUMBER FROM EXTRAN OR TRANSPORT
C     XFROM    = CROSS SECTION IDENTIFICATION NUMBER,OR
C                POWER FUNCTION EXPONENT
C     SLOPE    = AVERAGE CHANNEL SLOPE
C     METRIC   = US CUSTOMARY OR METRIC UNITS
C     KNORM    = IF 0 - CALLED FROM EXTRAN, IF 1 - CALLED FROM TRANSPORT
C     KCOND    = IF 0 - READ NATURAL CROSS SECTIONS
C     KCOND    = IF 1 - CREATE POWER FUNCTION CHANNELS
C     AFULL    = AREA WHEN CONDUIT IS FULL
C     DEEP     = MAXIMUM DEPTH
C     TWFULL   = TOP WIDTH WHEN CONDUIT IS FULL
C     XLEN     = CONDUIT LENGTH
C     RN0      = CHANNEL MANNING'S N
C     RFULL    = HYDRAULIC RADIUS WHEN CONDUIT IS FULL
C     NSTOP    = NUMBER OF ERRORS
C     IDNUM    = CONDUIT NUMBER
C     KDNUM    = CONDUIT NAME
C     KSTOP    = 0.... PRINT NORMALIZED CURVES
C     KSTOP    = 1.... DO NOT PRINT NORMALIZED CURVES
C=======================================================================
C     EXTRAN USES EITHER US CUSTOMARY OR METRIC UNITS INTERNALLY
C     TRANSPORT USES US STANDARD UNITS INTERNALLY
C=======================================================================
      IF(KNORM.EQ.0) KMET = METRIC
      IF(KNORM.EQ.1) KMET = 1
C=======================================================================
C     INITIALIZE AS REQUIRED
C=======================================================================
      NQC(JNO)            = NATUR
      GOTOO               = .FALSE.
      FAILED              = .FALSE.
      NUMST               = 0
      NUMQ(NATUR)         = 26
      PXSECR              = 1.0
      POW                 = XFROM
      QCURVE(NATUR,1,1)   = 0.0
      QCURVE(NATUR,2,1)   = 0.0
      QCURVE(NATUR,3,1)   = 0.0
      DO 100 I            = 1,26
      DO 100 K            = 1,3
  100 DUMMYS(K,I)         = 0.0
      IF(KCOND.EQ.0) XLEN = 0.0
      IF(KCOND.EQ.0.AND.SLOPE.LE.0.0) THEN
                                      IF(JCE.EQ.0) WRITE(N6,9600) IDNUM
                                      IF(JCE.EQ.1) WRITE(N6,9601) KDNUM
                                      STOP
                                      ENDIF
      RTSLOP              = SQRT(SLOPE)
      IF(KNORM.EQ.1.AND.KCOND.EQ.0) TWFULL = 0.0
      IF(KCOND.EQ.1) GO TO 6666
C=================================================================
C READ CARDS. WHEN NC OR GR CARD IS ENCOUNTERED, INCORPORATE VALUES
C             WHEN X1 CARD IS ENCOUNTERED, CHECK IF NAME IS RIGHT
C             IF X1 CARD IS OKAY, USE IT. IF NOT, FORGET IT.
C=======================================================================
C     READ THE CROSS-SECTION DATA
C=======================================================================
C REDUCE LENGTH OF DO LOOP FROM 100000 TO 2200, WCH, 8/28/92
C#######################################################################
      DO 300 KREAD = 1,2200
      READ(N5,*,ERR=888,END=9000) CC
      IF(CC.EQ.'NC'.OR.CC.EQ.'C2'.OR.CC.EQ.'E2') THEN
                    BACKSPACE N5
                    READ(N5,*,ERR=888) CC,XNL,XNR,XNCH
                    IF(XNCH.LE.0.0) WRITE(N6,201)
                    ENDIF
      RN0 = XNCH
      IF(XNCH.LE.0.0) WRITE(N6,201)
      IF(CC.EQ.'X1'.OR.CC.EQ.'C3'.OR.CC.EQ.'E3') THEN
                    BACKSPACE N5
                    READ(N5,*,ERR=888) CC,SECNO,NUMST,STCHL,STCHR,
     +                                 XLOBL,XLOBR,XLEN,PX,PSXECE
                    IF(SECNO.EQ.XFROM) THEN
                                       GOTOO                = .TRUE.
                                       IF(PX.NE.0.0) PXSECR =  PX
                                       ENDIF
                    ENDIF
C
      IF(CC.EQ.'GR'.OR.CC.EQ.'C4'.OR.CC.EQ.'E4'.AND.NUMST.NE.0) THEN
                    BACKSPACE N5
                    DO 210 II = 1,NUMST,5
                    IEND = II + 4
                    IF(IEND.GT.NUMST) IEND = NUMST
                    READ(N5,*,ERR=888) CC,((ELSTA(K,I),K=1,2),I=II,IEND)
  210               CONTINUE
C=======================================================================
C WCH MODIFICATION
C ALLOW ONLY ONE SET OF C3-C4 LINES, IF DESIRED.
C BACKSPACE THE NUMBER OF LINES REQUIRED.
C NREQ = NUMBER OF C4 LINES PLUS ONE C3 LINE.
C=======================================================================
C NEW CORRECTION, 8/92, WCH.  BACKSPACE ONLY IF USING
C  DESIRED C3-C4 LINES.
C=======================================================================
          IF(GOTOO) THEN
                    NREQ = NUMST/5
                    IF(NREQ*5.LT.NUMST) NREQ = NREQ + 1
                    NREQ = NREQ + 1
                    DO 240 J = 1,NREQ
  240               BACKSPACE N5
          ENDIF
                    IF(GOTOO) GO TO 1200
                    ENDIF
  300 CONTINUE
C#######################################################################
C IF ARRIVE HERE, HAVEN'T BEEN ABLE TO FIND MATCH FOR NATURAL CHANNEL.
C WRITE ERROR MESSAGE.  WCH, 8/28/92
C#######################################################################
      WRITE(N6,9040) XFROM, SECNO
      NSTOP = NSTOP+1
      RETURN
C=======================================================================
C     IRREGULAR CHANNEL CALCS FOR EXTRAN OR TRANSPORT CHANNEL ROUTING
C                       TRANSPORT IS NORMALIZED BY AREA.
C                       EXTRAN IS NORMALIZED BY DEPTH.
C=======================================================================
C     MODIFY BY FACTORS ON X1 LINE
C=======================================================================
 1200 CONTINUE
      IF(PXSECR.NE.1.0.OR.PSXECE.NE.0.0) THEN
                                         DO 1300 I  = 1,NUMST
                                         ELSTA(2,I) = PXSECR*ELSTA(2,I)
 1300                                    ELSTA(1,I) = PSXECE+ELSTA(1,I)
C#### WCH (RED), 9/93. 
C#### RED (WCH), 12/31/93.  SHOULD BE PXSECR, NOT PSXECR!!
                                         STCHL      = PXSECR*STCHL
                                         STCHR      = PXSECR*STCHR
                                         ENDIF
C=======================================================================
C     ALL REQUIRED DATA IN PLACE. CREATE CURVES AS REQUIRED.
C     DUMMY OUT OVERBANK ROUGHNESS IF IT WAS NOT SPECIFIED.
C=======================================================================
      IF(XNR.EQ.0.0) XNR = XNCH
      IF(XNL.EQ.0.0) XNL = XNCH
C=======================================================================
C     FIND MIN AND MAX STAGE POINTS.
C     CHECK IF STATIONS ARE IN THE WRONG ORDER.
C=======================================================================
      ELMIN =  99999.0
      ELMAX = -99999.0
      DO 1400 I = 1,NUMST
      IF(ELSTA(1,I).GT.ELMAX) ELMAX = ELSTA(1,I)
      IF(ELSTA(1,I).LT.ELMIN) ELMIN = ELSTA(1,I)
      IF(I.GT.1) THEN
C#### WCH, 2/24/93.  MAKE ERROR CHECK SLIGHTLY MORE LIBERAL.
              IF(ELSTA(2,I)-ELSTA(2,I-1).LT.-0.001) FAILED=.TRUE.
              IF(FAILED) GO TO 9020
              ENDIF
 1400 CONTINUE
C=======================================================================
C     ESTABLISH RANGE ON CURVES.
C=======================================================================
      IF(DEEP.LE.0.0)         DEEP = ELMAX - ELMIN
      IF(DEEP.GT.ELMAX-ELMIN) DEEP = ELMAX - ELMIN
      WIDE        = ELSTA(2,NUMST)-ELSTA(2,1)
      ELDEL       = DEEP/25.0
      DUMMYS(1,1) = 0.0
C=======================================================================
C     DO A FEW BASIC CHECKS.
C=======================================================================
      IF(XNCH.EQ.0.0) THEN
                      WRITE(N6,1405)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
      IF(XLEN.EQ.0.0) THEN
                      WRITE(N6,1406)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
      IF(WIDE.EQ.0.0) THEN
                      WRITE(N6,1407)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
      IF(DEEP.EQ.0.0) THEN
                      WRITE(N6,1408)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
      IF(NUMST.LE.1)  THEN
                      WRITE(N6,1409)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
C=======================================================================
C     FIND STAGES, TOTAL AREAS, FLOWS AND TOP WIDTHS FOR EACH STAGE.
C=======================================================================
C##### RED, 6/2/93.  MOVE TWFULL=0 INSIDE DO-LOOP.
      DO 1500 I         = 2,26
      TWFULL            = 0.0
      DUMMYS(1,I)       = DUMMYS(1,I-1) + ELDEL
      ELEVN             = DUMMYS(1,I)   + ELMIN
      QCURVE(NATUR,1,I) = 0.0
      QCURVE(NATUR,2,I) = 0.0
      QCURVE(NATUR,3,I) = 0.0
      SUMWP             = 0.0
C=======================================================================
C     FIND AREA AND FLOW IN DEPRESSIONS
C=======================================================================
      DO 1450 J = 1,50
 1450 QTEMP(J)  = 0.0
      J         = 0
      ITEMP     = 0
      DO 1490 K = 2,NUMST
C=======================================================================
C WCH MODIFICATION
C MUST COMPUTE FLOW FOR COMPLEX SECTION USING TOTAL AREA AND WETTED
C   PERIMETER.  SUM UNTIL HAVE DRY SEGMENT OR UNTIL MANNING N CHANGES.
C ACCOUNT FOR POSSIBLE "SAWTOOTH" CROSS SECTION.
C USE ARRAY QTEMP TO TEMPORARILY SUM FLOWS UNTIL ALL DEPRESSIONS
C   ACCOUNTED FOR.
C=======================================================================
C  WCH, 2/24/93.  GET RID OF CHECK FOR EQUALITY OF STATIONS.
C  THIS GIVES CORRECT HYDRAULIC RADIUS FOR SECTIONS WITH VERTICAL SIDES.
C  (OLD LINE):  IF(ELSTA(2,K).EQ.ELSTA(2,K-1)) GO TO 1490
C#######################################################################
      IF(ELSTA(1,K-1).GT.ELSTA(1,K)) THEN
                                     SU = ELSTA(1,K-1)
                                     SD = ELSTA(1,K)
                                     ELSE
                                     SU = ELSTA(1,K)
                                     SD = ELSTA(1,K-1)
                                     ENDIF
C=======================================================================
C INCREMENT QTEMP INDEX J EACH TIME ENTER NEW "DEPRESSION" OR CHANGED N.
C=======================================================================
      IF(SD.LT.ELEVN) THEN
                      IF(ITEMP.LE.0) J = J+1
                      IF(J.GT.50) THEN
                                  WRITE(N6,1410)
                                  NSTOP = NSTOP + 1
                                  ENDIF
                      ITEMP = 1
C=======================================================================
C IF LOWER ELEV (SD) LT WATER ELEVN, THEN WET SEGMENT.
C ACCUMULATE AREA, WETTED PERIMETER AND TWIDTH FOR THIS STAGE
C=======================================================================
          WIDTH  = ABS(ELSTA(2,K)   - ELSTA(2,K-1))
          WPERIM = SQRT(WIDTH*WIDTH + (SU-SD)*(SU-SD))
          IF(ELEVN.GE.SU) THEN
                          AREA   = WIDTH*((ELEVN-SU) + (ELEVN-SD))/2.0
                          TWIDTH = WIDTH
                          SUMWP  = SUMWP + WPERIM
                          ELSE
                          RATIO  = (ELEVN-SD)/(SU-SD)
                          AREA   = WIDTH*(SU-SD)/2.0*RATIO*RATIO
                          TWIDTH = WIDTH*RATIO
                          SUMWP  = SUMWP + WPERIM*RATIO
                          ENDIF
C=======================================================================
C USE QTEMP TO ACCUMULATE AREA UNTIL READY TO COMPUTE FLOW FOR DEPRESS.
C USE QCURVE-2 TO SUM TOTAL AREA.
C=======================================================================
C##### RED, 6/2/93.  ADD FOLLOWING LINE:
      TWFULL            = TWFULL   + TWIDTH
      QTEMP(J)          = QTEMP(J) + AREA
      QCURVE(NATUR,2,I) = QCURVE(NATUR,2,I) + AREA
      IF(KNORM.EQ.0) QCURVE(NATUR,3,I) = QCURVE(NATUR,3,I) + TWIDTH
C
C=======================================================================
C IF LOWER ELEV (SD) GT ELEVN, THE DRY SEGMENT.  END FLOW ACCUMULATION.
C OR, IF CHANGE TO NEW MANNINGS N, ALSO END FLOW ACCUM.
C NOTE: IF ELEVN.NE.ELSTA(1,K-1) THEN SMALL ERROR, BECAUSE LOCATION
C   WHERE N CHANGES IS NOT THE SAME AS LOCATION OF WATER SURFACE.
C=======================================================================
                 IF(ELSTA(2,K-1).EQ.STCHL.AND.XNL.NE.XNCH.
     *           OR.ELSTA(2,K-1).EQ.STCHR.AND.XNR.NE.XNCH) GO TO 1480
                 IF(K.LT.NUMST) GO TO 1490
C
                      ENDIF
C
                 IF(ITEMP.LE.0) GO TO 1490
C=======================================================================
C FIND FLOW
C=======================================================================
 1480                                RN = XNCH
C=======================================================================
C#### WCH, 11/30/93.  TRY STRICTLY LT AND GT FOR THESE ROUGHNESS 
C  TRANSITIONS.  AVOID PROBLEM IF TRANSITION IS EXACTLY AT LOCATION
C  OF VERTICAL SIDE, WHERE VERTICAL SIDE IS IN MAIN CHANNEL.
C
C####           IF(ELSTA(2,K-1).LE.STCHL) RN = XNL
C####           IF(ELSTA(2,K-1).GE.STCHR) RN = XNR
C=======================================================================
           IF(ELSTA(2,K-1).LT.STCHL) RN = XNL
           IF(ELSTA(2,K-1).GT.STCHR) RN = XNR
           RADIUS   = QTEMP(J)/SUMWP                    
           AREA     = QTEMP(J)
           QTEMP(J) = CMET(9,KMET)/RN*RADIUS**0.666667*RTSLOP*AREA
           ITEMP    = 0
           SUMWP    = 0.0
C
 1490 CONTINUE
C=======================================================================
C NOW STORE SUM OF DEPRESSION FLOWS OR FLOWS FOR DIFFERENT N-VALUES
C   AS TOTAL FLOW FOR THIS DEPTH.
C=======================================================================
      FLOW     = 0.0
      IF(J.GT.0) THEN
                 DO 1495 LL = 1,J
 1495            FLOW = FLOW + QTEMP(LL)
                 ENDIF
      IF(KNORM.EQ.0) QCURVE(NATUR,1,I) = FLOW
      IF(KNORM.EQ.1) QCURVE(NATUR,3,I) = FLOW
 1500 CONTINUE
C=======================================================================
C=======================================================================
C     CALCULATE AREA, HYDRAULIC RADIUS, AND TOP WIDTH FOR
C               POWER FUNCTION CROSS SECTIONS
C=======================================================================
C     POWER FUNCTION CHANNELS WILL HAVE SKIPPED NATURAL CROSS SECTION
C       INPUT AND CALCS.
C=======================================================================
 6666 CONTINUE
      IF(KCOND.EQ.1) THEN
      PEW        = 1.0/POW
      DEPTH      = 0.0
      DINC       = DEEP/25.0
      WETPER     = 0.0
      DO 2500 I  = 2,26
      DEPTH      = DEPTH + DINC
      CALL POWER(DEPTH,DEEP,TWFULL,WP,POW,DINC)
      WETPER = WETPER + WP
      AREA   = TWFULL*DEPTH*(1.0 - 1.0/(1.0 + POW))
      RADIUS = AREA/WETPER
      TWIDTH = TWFULL*DEPTH**PEW/DEEP**PEW
      FLOW   = CMET(9,KMET)/RN0*RADIUS**0.666667*RTSLOP*AREA
      QCURVE(NATUR,1,I) = RADIUS
      QCURVE(NATUR,2,I) = AREA
      IF(KNORM.EQ.0) QCURVE(NATUR,3,I) = TWIDTH
      IF(KNORM.EQ.1) QCURVE(NATUR,3,I) = FLOW
 2500 CONTINUE
      ENDIF
C=======================================================================
C     CALCULATE EQUIVALENT HYDRAULIC RADIUS,
C     NORMALIZE CURVES, AND FIND MAX VALUES OF AREA AND EQUIV. HYD. RAD.
C=======================================================================
      CONST  =  CMET(9,KMET)/RN0*RTSLOP
      AFULL  =  QCURVE(NATUR,2,26)
      IF(KNORM.EQ.0) QMAX   =  QCURVE(NATUR,1,26)
      IF(KNORM.EQ.1) QMAX   =  QCURVE(NATUR,3,26)
      IF(KCOND.EQ.0) RFULL  =  (QMAX/(AFULL*CONST))**1.5
      IF(KCOND.EQ.1) RFULL  =  QCURVE(NATUR,1,26)
      IF(KNORM.EQ.0) TWFULL =  QCURVE(NATUR,3,26)
C
      DO 1600 I = 2,26
      IF(KCOND.EQ.1) QCURVE(NATUR,1,I) = QCURVE(NATUR,1,I)/RFULL
      IF(KCOND.EQ.0) THEN
C=======================================================================
C NOTE THAT HERE FOR EXTRAN, QCURVE-1 IS CONVERTED FROM FLOW TO HYD RAD.
C=======================================================================
      IF(KNORM.EQ.0) QCURVE(NATUR,1,I) =
     +  ((QCURVE(NATUR,1,I)/(CONST*QCURVE(NATUR,2,I)))**1.5)/RFULL
      IF(KNORM.EQ.1) QCURVE(NATUR,1,I) =
     +  ((QCURVE(NATUR,3,I)/(CONST*QCURVE(NATUR,2,I)))**1.5)/RFULL
                     ENDIF
      QCURVE(NATUR,2,I) = QCURVE(NATUR,2,I)/AFULL
      IF(KNORM.EQ.0) QCURVE(NATUR,3,I) = QCURVE(NATUR,3,I)/TWFULL
      IF(KNORM.EQ.1) QCURVE(NATUR,3,I) = QCURVE(NATUR,3,I)/QMAX
 1600 CONTINUE
C======================================================================
C     CONVERT TO NORMALIZED AREA IF CALLED BY TRANSPORT
C======================================================================
      IF(KNORM.EQ.1) THEN
        I         = 2
        K         = 2
        ELDEL     = 0.04
        AA        = ELDEL
 1800   CONTINUE
        IF(QCURVE(NATUR,2,I).GE.AA) THEN
        XDIFF       = AA                - QCURVE(NATUR,2,I-1)
        XSLOPE      = QCURVE(NATUR,2,I) - QCURVE(NATUR,2,I-1)
        DUMMYS(1,K) = QCURVE(NATUR,1,I-1)  + (QCURVE(NATUR,1,I) -
     +                QCURVE(NATUR,1,I-1)) * XDIFF/XSLOPE
        DUMMYS(2,K) = FLOAT(I-2)*ELDEL + ELDEL * XDIFF/XSLOPE
        DUMMYS(3,K) = QCURVE(NATUR,3,I-1)  + (QCURVE(NATUR,3,I) -
     +                QCURVE(NATUR,3,I-1)) * XDIFF/XSLOPE
        AA          = AA + ELDEL
        K           =  K + 1
        IF(K.EQ.26) GO TO 1810
        ELSE
        I  = I + 1
        ENDIF
        GO TO 1800
 1810   DO 1850 I = 2,25
        QCURVE(NATUR,1,I) = DUMMYS(1,I)
        QCURVE(NATUR,2,I) = DUMMYS(2,I)
 1850   QCURVE(NATUR,3,I) = DUMMYS(3,I)
        ENDIF
C======================================================================
C     PRINT CROSS SECTION OR CONDUIT INFORMATION FOR NATURAL CHANNELS
C     THE 100th IRREGULAR STATION POINT IS SET = 0 TO GIVE
C     CORRECT OUTPUT IN PRINTING ROUTINE.
C======================================================================
      IF(KCOND.EQ.0) THEN
      IF(JCE.EQ.0) WRITE(N6,1690) IDNUM
      IF(JCE.EQ.1) WRITE(N6,1691) KDNUM
      WRITE(N6,1696)  XFROM,NATUR
      IF(METRIC.EQ.1) THEN
                      WRITE(N6,1700) XLEN,ELEVN,SLOPE,DEEP,
     +                               XNL,STCHL,AFULL,XNCH,RFULL
                      WRITE(N6,1701) XNR,STCHR,TWFULL,QMAX
                      IF(KSTOP.EQ.0) WRITE(N6,1710) NUMST,PSXECE,PXSECR
                      ELSE
                      WRITE(N6,2700) XLEN,ELEVN,SLOPE,DEEP,
     +                               XNL,STCHL,AFULL,XNCH,RFULL
                      WRITE(N6,2701) XNR,STCHR,TWFULL,QMAX
                      IF(KSTOP.EQ.0) WRITE(N6,2710) NUMST,PSXECE,PXSECR
                      ENDIF
      ELSTA(1,100) = 0.0
      ELSTA(2,100) = 0.0
      DO 1303 I    = 1,20
      II           = 5*(I-1)+1
      III          = 5*(I-1)+2
      IF(III.GT.NUMST)   III = 100
                          IV = 5*(I-1)+3
      IF(IV.GT.NUMST)     IV = 100
                         IIV = 5*(I-1)+4
      IF(IIV.GT.NUMST)   IIV = 100
                        IIIV = 5*(I-1)+5
      IF(IIIV.GT.NUMST) IIIV = 100
      IF(KSTOP.EQ.0) WRITE(N6,1302) (ELSTA(K,II),K=1,2),
     +                (ELSTA(K,III),K=1,2),(ELSTA(K,IV),K=1,2),
     +                (ELSTA(K,IIV),K=1,2),(ELSTA(K,IIIV),K=1,2)
      IF(IIIV.GE.NUMST) GO TO 1304
 1303 CONTINUE
 1304 CONTINUE
      ENDIF
C======================================================================
C     PRINT CONDUIT INFORMATION FOR POWER FUNCTION CHANNELS
C======================================================================
      IF(KCOND.EQ.1) THEN
      IF(JCE.EQ.0) WRITE(N6,2690) IDNUM
      IF(JCE.EQ.1) WRITE(N6,2691) KDNUM
      IF(METRIC.EQ.1) THEN
                      WRITE(N6,3700) XLEN,POW,DEEP,
     +                               RN0,AFULL,RFULL,TWFULL,NATUR
                      ELSE
                      WRITE(N6,3710) XLEN,POW,DEEP,RN0,AFULL,
     +                               RFULL,TWFULL,NATUR
                      ENDIF
      ENDIF
C=======================================================================
C     WRITE CROSS SECTION DIMENSIONLESS CURVES
C=======================================================================
      IF(KNORM.EQ.0.AND.QCURVE(NATUR,3,1).EQ.0.0)
     +                  QCURVE(NATUR,3,1) = QCURVE(NATUR,3,2)
      IF(KSTOP.EQ.0) THEN
         IF(KNORM.EQ.0) WRITE(N6,1702)
         IF(KNORM.EQ.1) WRITE(N6,1703)
         DO 1900 I = 1,8
         II        = I + 9
         III       = I + 18
         WRITE(N6,3333) I,(QCURVE(NATUR,K,I),K=1,3),II,
     !                    (QCURVE(NATUR,K,II),K=1,3),III,
     !                    (QCURVE(NATUR,K,III),K=1,3)
 1900    CONTINUE
         WRITE(N6,3333) 9,(QCURVE(NATUR,K,9),K=1,3),18,
     !                 (QCURVE(NATUR,K,18),K=1,3)
         ENDIF
      RETURN
C=======================================================================
C     PRINT POSSIBLE INPUT ERRORS
C=======================================================================
C     DATA COULD NOT BE FOUND
C=======================================================================
 9000 CONTINUE
      WRITE(N6,9010) XFROM
      NSTOP = NSTOP+1
      RETURN
C=======================================================================
C     DATA INPUT IN WRONG ORDER
C=======================================================================
 9020 CONTINUE
      WRITE(N6,9030) XFROM
      NSTOP = NSTOP+1
      RETURN
  888 CALL IERROR
C=======================================================================
  201 FORMAT(/,' ===> WARNING !!!  XNCH LE 0.0.')
 1302 FORMAT(5(F10.2,2X,F10.2,4X))
 1405 FORMAT(/,' ===> ERROR !!! MANNINGS "N" OF CENTER CHANNEL IS 0.0.
     +JOB STOPPED.')
 1406 FORMAT(/,' ===> ERROR !!! CHANNEL LENGTH IS 0.0.  JOB STOPPED.')
 1407 FORMAT(/,' ===> ERROR !!! WIDTH OF CHANNEL IS 0.0.  JOB STOPPED.')
 1408 FORMAT(/,' ===> ERROR !!! DEPTH OF CHANNEL IS 0.0.  JOB STOPPED.')
 1409 FORMAT(/,' ===> ERROR !!! NUMBER OF DATA POINTS LE 1.  JOB STOPPED
     +.')
 1410 FORMAT(/,' ===> ERROR !!! NATURAL CHANNEL LIMITED TO MAXIMUM OF
     *50 INTERIOR DEPRESSIONS.  JOB STOPPED.')
 1690 FORMAT(/,9X,'NATURAL CROSS-SECTION INFORMATION FOR CHANNEL ',
     +       I9,/,9X,53('='))
 1691 FORMAT(/,9X,'NATURAL CROSS-SECTION INFORMATION FOR CHANNEL ',
     +       A10,/,9X,53('='))
 1696 FORMAT(9X,' CROSS-SECTION ID (FROM X1 CARD) : ',F7.1,
     +          ' CHANNEL SEQUENCE NUMBER :',I6,/)
 1700 FORMAT(3X,'LENGTH    :',F9.1,' FT ',T55,
     +          ' MAXIMUM ELEVATION        : ',F8.2,'     FT.',/,
     +3X,       'SLOPE     :',F12.4,' FT/FT',T55,
     +          ' MAXIMUM DEPTH            : ',F8.2,'     FT.',/,
     +3X,       'MANNING N :',F11.3,'  TO STATION ',F10.1,T55,
     +          ' MAXIMUM SECTION AREA     : ',F8.2,' SQ. FT.',
     +/,3X,'  "    "  :',F11.3,'  IN MAIN CHANNEL',T55,
     +' MAXIMUM HYDRAULIC RADIUS : ',F8.2,'     FT.')
 1701  FORMAT(3X,'  "    "  :',F11.3,'  BEYOND STATION',F8.1,T55,
     +' MAX TOPWIDTH             : ',F8.2,'     FT.',/,T55,
     +' MAXIMUM UNIFORM FLOW     : ',1PE8.2,' CFS.   ')
 1702 FORMAT(///,26X,' CROSS-SECTION DIMENSIONLESS CURVES ',/,
     +           26X,'       NORMALIZED BY DEPTH',/,
     +      24X,42('-')/
     +      3(' POINT  HYDRAULIC                           '),/,
     +      3('  NO.    RADIUS       AREA     TOPWIDTH     '),/,
     +      3(' -----  ---------    -----     --------     '))
 1703 FORMAT(//,26X,' CROSS-SECTION DIMENSIONLESS CURVES ',/,
     +           26X,'       NORMALIZED BY AREA',/,
     +      24X,42('-')/
     +      3(' POINT  HYDRAULIC                           '),/,
     +      3('  NO.    RADIUS      DEPTH         FLOW     '),/,
     +      3(' -----  ---------     ----     --------     '))
 1710 FORMAT(//,52X,'CROSS-SECTION POINTS',/,
     +          51X,'--------------------',/,10X,
     +'THE FOLLOWING ',I2,' STATIONS WERE READ AND ADJUSTED',
     +F9.3,' FT VERTICALLY AND HORIZONTALLY BY A RATIO OF ',F6.3,//,
     +5('   ELEVATION   STATION    '),/,
     +5('      FT         FT       '),/,
     +5('   ---------   -------    '))
 2690 FORMAT(/,9X,'POWER FUNCTION CROSS-SECTION INFORMATION',
     +       ' FOR CHANNEL ',I9,/,9X,53('='))
 2691 FORMAT(/,9X,'POWER FUNCTION CROSS-SECTION INFORMATION',
     +       ' FOR CHANNEL ',A10,/,9X,53('='))
 2700 FORMAT(3X,'LENGTH    :',F9.1,' METERS.',T55,
     +' MAXIMUM ELEVATION        : ',F8.2,' METERS.',/,
     +3X,'SLOPE     :',F12.4,'  M/M ',T55,
     +' MAXIMUM DEPTH            : ',F8.2,' METERS.',/,
     +3X,'MANNING N :',F11.3,'  TO STATION ',F10.1,T55,
     +' MAXIMUM SECTION AREA     : ',F8.2,' SQ.MET.',
     +/,3X,'  "    "  :',F11.3,'  IN MAIN CHANNEL',T55,
     +' MAXIMUM HYDRAULIC RADIUS : ',F8.2,' METERS.')
 2701  FORMAT(3X,'  "    "  :',F11.3,'  BEYOND STATION',F8.1,T55,
     +' MAX TOPWIDTH             : ',F8.2,' METERS.',/,T55,
     +' MAXIMUM UNIFORM FLOW     : ',1PE8.2,' CMS.   ')
 2710 FORMAT(//,52X,'CROSS-SECTION POINTS',/,
     +          51X,'--------------------',/,10X,
     +'THE FOLLOWING ',I2,' STATIONS WERE READ AND ADJUSTED',
     +F9.3,'  M VERTICALLY AND HORIZONTALLY BY A RATIO OF ',F6.3,//,
     +5('   ELEVATION   STATION    '),/,
     +5('    METERS     METERS     '),/,
     +5('   ---------   -------    '))
 3333 FORMAT(3(I3,3F12.4,5X))
 3700 FORMAT(9X,'LENGTH                   :',F12.1,' FEET.',/,
     +       9X,'EXPONENT OF CHANNEL      :',F12.3,/,
     +       9X,'MAXIMUM DEPTH            :',F12.2,' FEET.',/,
     +       9X,'MANNING N                :',F12.3,/,
     +       9X,'MAXIMUM SECTION AREA     :',F12.2,' SQ. FT.',/,
     +       9X,'MAXIMUM HYDRAULIC RADIUS :',F12.3,' FEET.',/,
     +       9X,'MAXIMUM TOP WIDTH        :',F12.2,' FEET.',/,
     +       9X,'CHANNEL SEQUENCE NUMBER  :',I12)
 3710 FORMAT(9X,'LENGTH                   :',F12.1,' METERS.',/,
     +       9X,'EXPONENT OF CHANNEL      :',F12.3,/,
     +       9X,'MAXIMUM DEPTH            :',F12.2,' METERS.',/,
     +       9X,'MANNING N                :',F12.3,/,
     +       9X,'MAXIMUM SECTION AREA     :',F12.2,' SQ.MET.',/,
     +       9X,'MAXIMUM HYDRAULIC RADIUS :',F12.3,' METERS.',/,
     +       9X,'MAXIMUM TOP WIDTH        :',F12.2,' METERS.',/,
     +       9X,'CHANNEL SEQUENCE NUMBER  :',I12)
 9010 FORMAT(/,' ===> ERROR !! FAILURE TO FIND DATA FOR SECTION',
     +' (',F10.3,')',/,'               JOB ENDED')
 9011  FORMAT(/,'  ===> ERROR, END OF DATA SET FOUND WHILE READING',
     ! 'AREA/STAGE DATA AT JUNCTION # ',I10)
 9012  FORMAT(/,'  ===> ERROR !!, END OF DATA SET FOUND WHILE READING',
     ! 'VOLUME/STAGE DATA AT JUNCTION # ',I10)
 9030 FORMAT(/,' ===> ERROR !! STATIONS NOT IN CORRECT SEQUENCE',
     +' FOR SECTION (',F10.3,')',/,'                JOB ENDED')
 9040 FORMAT(/,' ===> ERROR !! UNABLE TO MATCH NATURAL CHANNEL CROSS SEC
     *TION DATA FOR CHANNEL',F10.1,/,
     *'        LAST SECNO (LINE C3) READ WAS',F10.1,' $$ JOB ENDED.')
 9600 FORMAT(/,' ===> FATAL ERROR !! . CONDUIT SLOPE WAS ZERO',
     +         ' FOR CONDUIT ',I9)
 9601 FORMAT(/,' ===> FATAL ERROR !! . CONDUIT SLOPE WAS ZERO',
     +         ' FOR CONDUIT ',A10)
C=======================================================================
      END

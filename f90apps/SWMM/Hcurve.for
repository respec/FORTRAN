      SUBROUTINE HCURVE(NTYPE)
C     GRAPH RUNOFF BLOCK
C     CALLED FROM RUNOFF NEAR LINES 265 through 2c9
C=======================================================================
C     HCURVE WAS LAST UPDATED BY THE UNIVERSITY OF FLORIDA JANUARY, 1989
C     DIMENSION MODIFICATION, DELETED LOOP, AND NEW PRINT, 12/92 BY WCH
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'LAB.INC'
C=======================================================================
C#### WCH, 12/92.  DIMENSION TO NGW.
CIM INCREASE HYETOGRAPHS  ~~~~~~~~~~~~~~~~~~~~~~~~~
      DIMENSION GWFLWB(NGW),STPOLL(2,NGW),JSTA(MAXRG)
      DIMENSION X(201,2),Y(201,2),Y1(201,2),NPT(2),REIN(maxrg)
cim      DIMENSION GWFLWB(NGW),STPOLL(2,NGW),JSTA(10)
cim      DIMENSION X(201,2),Y(201,2),Y1(201,2),NPT(2),REIN(10)
cim ~~~~~~~~~~~~~~~~~~~~~~~~~~
      INTEGER MPGRW(NGW)
      CHARACTER*30 TITL(6),ORIZ,TSEC,TFIR
      CHARACTER*10 VER1(5),VER2(5),VER3(10),ALPLOT,KPGRW(NGW)
C=======================================================================
      DATA TITL/'RAINFALL HYETOGRAPH           ',
     1 'SURFACE INLET HYDROGRAPH FLOW ','PLOT OF INFILTRATION RATE    ',
     2 'SUBSURFACE OUTLET HYDROGRAPH  ',
     3 'SUBSURFACE STAGE GRAPH        ','SUBSURFACE MOISTURE FRACTION '/
      DATA TFIR/'SUMMATION OVER PERVIOUS AREA '/
      DATA TSEC/'SUMMATION FOR ALL INLETS'/
      DATA ORIZ /'         TIME IN HOURS        '/
      DATA VER1/' RAINFALL ','  RUNOFF  ','   FLOW  ','  STAGE   ',
     +          ' MOISTURE '/
      DATA VER2/'    IN    ','    IN    ','    IN   ','    IN    ',
     +          '    IN    '/
      DATA VER3/'  IN/HR   ','   MM/HR  ','    CFS   ',' CUB M/S  ',
     +          '   CFS    ',' CUB M/S  ','   FEET   ','  METERS  ',
     +          ' FRACTION ',' FRACTION '/
C=======================================================================
      NREIN    = NSCRAT(1)
      NFLOW    = NSCRAT(4)
      ALPLOT   = ' '
C=======================================================================
C      PLOT RAINFALL HYETOGRAPH OR INLET HYDROGRAPH
C=======================================================================
      X(1,1)   = TZERO/3600.0
      Y(1,1)   = 0.0
      HORIZ(1) = ORIZ
      HTITLE(1)= TITL(NTYPE)
      HTITLE(2)= '       '
      MTYPE    = NTYPE
      IF(NTYPE.EQ.3) MTYPE = 1
      IF(NTYPE.EQ.4) MTYPE = 3
      VERT1    = VER1(MTYPE)
      VERT2    = VER2(MTYPE)
      VERT3    = VER3(2*MTYPE-2+METRIC)
      IF(NTYPE.EQ.3) THEN
                     VERT1     = 'INFILTRATN'
                     HTITLE(2) = TFIR
                     ENDIF
C=======================================================================
C     FIND THE STARTING POSITION ON THE RAINFALL INTERFACE FILE
C=======================================================================
      IF(NTYPE.EQ.1) THEN
      I        = 0
      DO 110 J = 1,NRGAG
      REWIND NREIN
      READ(NREIN) NSTA,MRAIN,(JSTA(K),K=1,NSTA)
      I        = I+1
      TNEXT    = TZERO
      JULDAY   = IDATEZ
      TIME     = TZERO
      TIMDAY   = TZERO
      DO 65 L  = 1,MRAIN
      READ(NREIN,END=71) JDAY,TMDAY,THISTO,(REIN(K),K=1,NSTA)
      IF(JDAY.GE.JULDAY.AND.TMDAY.GE.TIMDAY) THEN
                        BACKSPACE NREIN
                        GO TO 66
                        ENDIF
  65  CONTINUE
  66  CONTINUE
C=======================================================================
C     READ RAINFALL FILE
C=======================================================================
      N        = 1
      X(N,1)   = TZERO/3600.0
      Y(N,1)   = 0.0
      DO 70 L  = 1,NRAIN
      READ(NREIN,END=71) JDAY,TMDAY,THISTO,(REIN(K),K=1,NSTA)
      CALL NTIME(JDAY,TMDAY,TRN)
      CALL STIME(TRN)
      TIME     = TIME + TRN
      IF(TIME.GT.TNEXT) THEN
                        N      = N + 1
                        X(N,1) = TNEXT/3600.0
                        Y(N,1) = 0.0
                        N      = N + 1
                        X(N,1) = TIME/3600.0
                        Y(N,1) = 0.0
                        ENDIF
      N        = N+1
      X(N,1) = TIME/3600.0
      Y(N,1) = REIN(J)
      N      = N+1
      TNEXT  = TIME + THISTO
      X(N,1) = TNEXT/3600.0
      Y(N,1) = REIN(J)
      IF(N.GE.199) THEN
                   N      = N+1
                   X(N,1) = X(N-1,1)
                   Y(N,1) = 0.0
                   NPT(1) = N
                   KSTAT = JSTA(J)
                   CALL CURVE(X,Y,NPT,1,KSTAT,ALPLOT)
                   X(1,1) = X(N,1)
                   Y(1,1) = 0.0
                   N = 1
                   ENDIF
   70 CONTINUE
   71 N      = N+1
      X(N,1) = X(N-1,1)
      Y(N,1) = 0.0
      NPT(1) = N
      KSTAT = JSTA(J)
      CALL CURVE(X,Y,NPT,1,KSTAT,ALPLOT)
  110 CONTINUE
      RETURN
      ENDIF
C=======================================================================
C     PLOT THE INLET HYDROGRAPH
C     MAKE THE CONVERSION FROM CFS TO CUBIC METERS PER SECOND (METRIC=2)
C=======================================================================
      IF(NTYPE.EQ.2) THEN
      REWIND NFLOW
      HTITLE(2)= TSEC
      N        = 1
      X(N,1)   = TZERO/3600.0
      Y(N,1)   = 0.0
      KINC     = (MTIME+199)/200 + 1
      NPLOT    = 1
C======================================================================
      DO 130 J = 1,MTIME,KINC
      DO 140 K = 1,KINC
 140  READ(NFLOW,END=135) TIME,FLWOFF,FINF
      N        = N + 1
      X(N,1)   = TIME/3600.0
      Y(N,1)   = FLWOFF
      IF(METRIC.EQ.2) Y(N,1) = Y(N,1) * 0.028316847
  130 CONTINUE
C=======================================================================
  135 NPT(1) = N
      KCE    = JCE
      JCE    = 1
      ALPLOT = 'FLOW SUM'
      CALL CURVE(X,Y,NPT,1,NPLOT,ALPLOT)
      JCE    = KCE
      RETURN
      ENDIF
C=======================================================================
C     PLOT THE IFILTRATION RATE PLOT
C=======================================================================
      IF(NTYPE.EQ.3.AND.PRCIMP.LT.1.0) THEN
      REWIND NFLOW
      TY       = TRIBA*3630.0*(1.0-PRCIMP)
      IF (TY.NE.0.0) THEN 
      TY       = 3600.0/TY
      ELSE 
      TY       = 0.0
      END IF
      HTITLE(1)= TITL(3)
      HTITLE(2)= '                  '
      N        = 1
      X(N,1)   = TZERO/3600.0
      Y(N,1)   = 0.0
      KINC     = (MTIME+199)/200 + 1
      NPLOT    = 1
C======================================================================
      DO 230 J = 1,MTIME,KINC
      DO 240 K = 1,KINC
 240  READ(NFLOW,END=235) TIME,FLWOFF,FINF
      N        = N + 1
      X(N,1)   = TIME/3600.0
      Y(N,1)   = FINF*TY
      IF(METRIC.EQ.2) Y(N,1) = Y(N,1) * 25.4
 230  CONTINUE
C=======================================================================
  235 NPT(1) = N
      KCE    = JCE
      JCE    = 1
      ALPLOT = 'INFILTRA'
      CALL CURVE(X,Y,NPT,1,NPLOT,ALPLOT)
      JCE    = KCE
      RETURN
      ENDIF
C=======================================================================
C     PLOT SUBSURFACE FLOWS, SOIL MOISTURE, AND STAGE
C=======================================================================
      IF(NTYPE.EQ.4) THEN
             NGRND = NSCRAT(6)
      MCOUN        = 0
C#### WCH, 12/92  NOTE, FORMER DO 395 LOOP 1,NSVGW DELTED AT THIS POINT.
      DO 405 JH    = 1,NOGWSC
      IF(NSCSFG(JH).GT.0) THEN
                          MCOUN        = MCOUN + 1
                          MPGRW(MCOUN) = NMSUB(JH)
                          IF(JCE.EQ.1) KPGRW(MCOUN) = KMSUB(JH)
                          ENDIF
 405  CONTINUE
C##### WCH, 12/92
      WRITE (*,8990)
      DO 400 JJ    = 1,NSVGW
C##### WCH, 12/92.  PLOTS CAN BE TIME CONSUMING.  PRINT REASSURANCE.
      WRITE (*,9000) JJ,NSVGW
      REWIND NGRND
      N        = 1
      X(N,1)   = TZERO/3600.0
      Y(N,1)   = 0.0
      KINC     = (MTIME+199)/200 + 1
      DO 330 J = 1,MTIME,KINC
      DO 340 K = 1,KINC
C=======================================================================
C     READ SUBSURFACE PLOT INFORMATION
C=======================================================================
 340  READ(NGRND,END=335) JULDAY,TIMDAY,TIME,(GWFLWB(JI),
     .            (STPOLL(JV,JI),JV=1,2),JI=1,NSVGW)
      N        = N + 1
      X(N,1)   = TIME/3600.0
      Y(N,1)   = GWFLWB(JJ)
      Y1(N,1)  = STPOLL(1,JJ)
      Y1(N,2)  = STPOLL(2,JJ)
 330  CONTINUE
 335  CONTINUE
      NPT(1) = N
C=======================================================================
                    NPLOT = MPGRW(JJ)
      IF(JCE.EQ.1) ALPLOT = KPGRW(JJ)
      HTITLE(1) = TITL(4)
      HTITLE(2) = '      '
      VERT1     = VER1(3)
      VERT2     = VER2(3)
      VERT3     = VER3(5+METRIC-1)
      CALL CURVE(X,Y,NPT,1,NPLOT,ALPLOT)
      DO 300 J  = 1,N
 300  Y(J,1)    = Y1(J,1)
      HTITLE(1) = TITL(5)
      VERT1     = VER1(4)
      VERT2     = VER2(4)
      VERT3     = VER3(7+METRIC-1)
      CALL CURVE(X,Y,NPT,1,NPLOT,ALPLOT)
      DO 350 J  = 1,N
 350  Y(J,1)    = Y1(J,2)
      HTITLE(1) = TITL(6)
      VERT1     = VER1(5)
      VERT2     = VER2(5)
      VERT3     = VER3(9+METRIC-1)
      CALL CURVE(X,Y,NPT,1,NPLOT,ALPLOT)
 400  CONTINUE
C#### WCH, 12/92
      RETURN
      ENDIF
C#######################################################################
 8990 FORMAT (//)
 9000 FORMAT ('+ Constructing no.',I3,' of',I4,' requested groundwater p
     *lots.')
      END

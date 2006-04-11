C
C
C
      SUBROUTINE   PLTONE
C
C     + + + PURPOSE + + +
C     This routine controls all the basic calls to make the plot.
C     It selects the calls for each type of line, axes, and legend
C     based on the information in the CPLTV common block.
C
C     + + + KEYWORDS + + +
C     DISSPLA
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + +   LOCAL VARIABLES   + + +
      INTEGER     I,K,LEGNDS,MAXY,LFTRT,MAXX,MINX,PORR,NCHK,
     #            MINY,IDUM
      INTEGER     I1,I0,ITM
      REAL        XP,YP,YMLEN,RMAX1,BOTOM,STDEV, SF
C     REAL        TST
C
C     + + + FUNCTIONS + + +
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT, ALOG10, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   TMCRVM, TMCRVP, VRTBAR, PLTITL, PBAXIS
      EXTERNAL   XYCURV, LEGNDC, ADDTXT, MATCHL, AUXAXE, ARAXIS
      EXTERNAL   LGAXIS, RHTBDR, AXAXIS, LXAXIS, TMAXIS
      EXTERNAL   GSCHH, GSLN, GSPLCI, GSPMCI
      EXTERNAL   GSFACI, GSTXCI, GSTXP, GPSSZB
C     EXTERNAL   GSMK, GSMKSC
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
      LEGNDS = 1
      YMLEN = YLEN
      DO 10 I = 1,NCRV
        IF (CTYPE(I).EQ.3.OR.CTYPE(I).EQ.4) THEN
C         auxiliary plot used
          YMLEN = YLEN - ALEN - 1.5*SIZEL
        END IF
 10   CONTINUE
C
      CALL GSCHH (SIZEL)
      CALL GSTXP (I0)
C     set check for drawing legend
      NCHK = NCRV
      IF (ALEN .GT. 0.0001) NCHK = NCHK - 1
C     begin loop to draw curves
      DO 200 K = 1,NCRV
        CALL GSLN (I1)
C       set color stuff
        I = COLOR(K)
        ITM= I
        CALL GSPLCI (ITM)
        CALL GSPMCI (ITM)
        CALL GSFACI (ITM)
        CALL GSTXCI (ITM)
C
        IF (SYMBL(K) .NE. 0) THEN
C          CALL GSMK (SYMBL(K))
C          CALL GSMKSC (SF)
C         set symbol size
          SF = 0.1
          CALL GPSSZB (K,SF)
        END IF
        IF (LNTYP(K) .NE. 0) THEN
          CALL GSLN (LNTYP(K))
        END IF
C
        GO TO (110, 120, 130, 130, 150, 160, 170), CTYPE(K)
C
 110    CONTINUE
C         uniform timestep
          IF (DTYPE(K) .EQ. 1) THEN
C           data is mean or sum over time step
            CALL TMCRVM (K)
          ELSE
C           data is point or instantaneous
            CALL TMCRVP (K)
          END IF
          GO TO 190
C
 120    CONTINUE
C         vertical bar from bottom
          CALL VRTBAR (K)
          GO TO 190
C
 130    CONTINUE
C         line on auxiliary plot
          IF (CTYPE(K) .EQ. 3) THEN
C           draw a line or points
            IF (DTYPE(K) .EQ. 1) THEN
C             data is mean or sum over time step
              CALL TMCRVM (K)
            ELSE
C             data is point or instantaneous
              CALL TMCRVP (K)
            END IF
          ELSE
C           draw bar chart
            CALL VRTBAR (K)
          END IF
          GO TO 190
C
 150    CONTINUE
C         date tagged time-series plot
          IF (DTYPE(K) .EQ. 1) THEN
C           data is mean or sum over time step
            CALL TMCRVM (K)
          ELSE
C           data is point or instantaneous
            CALL TMCRVP (K)
          END IF
          GO TO 190
C
 160    CONTINUE
C         X-Y plot
          IDUM = 0
          CALL XYCURV (K,IDUM)
          GO TO 190
C
 170    CONTINUE
C         X-Y plot with symbol size as 3rd variable
          IDUM = 3
          CALL XYCURV (K,IDUM)
          GO TO 190
C
 190    CONTINUE
C
C       draw legend
        IF (LOCLGD(1).GT.-1.5) THEN
          IF (NCHK .GT. 1) THEN
            IF (CTYPE(K).NE.3 .AND. CTYPE(K).NE.4) THEN
C             do when not auxiliary plot
              CALL LEGNDC (K, LEGNDS)
            END IF
          END IF
        END IF
C
 200  CONTINUE
C
C     set color for axes, axes labels, and title
      CALL GSPLCI (LBCOLR)
      CALL GSPMCI (LBCOLR)
      CALL GSFACI (LBCOLR)
      CALL GSTXCI (LBCOLR)
      CALL GSLN (I1)
      CALL GSCHH (SIZEL)
C
      IF (NCHR .GT. 0) THEN
C       add text to plot
        XP = XLEN*FXT
        YP = YMLEN*FYT -SIZEL
        CALL ADDTXT (XP,YP,SIZEL,NCHR,CPR,CTXT)
      END IF
C
      IF (ALEN .GT. 0.0001) THEN
C       since auxiliary axis used, must align labels
        CALL MATCHL (PLMX(3),SIZEL,PLMN(1),PLMX(1),YTYPE(1),TICS(1),
     &               RMAX1)
C
C       label auxiliary axis
        CALL AUXAXE (XLEN,YMLEN,ALEN,SIZEL,YALABL,TICS(3),PLMN(3),
     &               PLMX(3),RMAX1)
C
      ELSE
C       no need to match, set RMAX1 to large number
        RMAX1 = 999.
      END IF
C
C     begin loops to draw axis and tic marks for detailed routines.
C
      IF (YTYPE(1).EQ.1) THEN
C       for left arithmetic axis.
        LFTRT = 1
        CALL ARAXIS (XLEN,YMLEN,YLABL ,SIZEL,PLMN(1),PLMX(1),LFTRT,
     #               RMAX1,TICS(1))
      END IF
      IF (YTYPE(1).EQ.2) THEN
C       for left logarithmic axis.
C       TST= 1.0
C       IF (ABS(PLMN(1)).GT.1.0E-9) TST= PLMX(1)/PLMN(1)
C       IF (TRANSF(1) .EQ. 2 .OR. TST .GT. 9.0) THEN
C         data and scales were not transformed
          MAXY = INT(1.01*ALOG10(PLMX(1)))
          MINY = INT(1.01*ALOG10(PLMN(1)))
C       ELSE
C         transformation not needed
C         MAXY = PLMX(1)
C         MINY = PLMN(1)
C       END IF
        LFTRT = 1
        CALL LGAXIS (XLEN,YMLEN,YLABL ,SIZEL,MINY,MAXY,LFTRT,RMAX1)
      END IF
C
      IF (YTYPE(2).EQ.1) THEN
C       for right arithmetic
        LFTRT = 2
        CALL ARAXIS (XLEN,YMLEN,YXLABL,SIZEL,PLMN(2),PLMX(2),LFTRT,
     #               RMAX1,TICS(2))
      END IF
      IF (YTYPE(2) .EQ. 2) THEN
C       for right logarithmic
        LFTRT = 2
        MAXY = INT(1.01*ALOG10(PLMX(2)))
        MINY = INT(1.01*ALOG10(PLMN(2)))
        CALL LGAXIS (XLEN,YMLEN,YXLABL,SIZEL,MINY,MAXY,LFTRT,RMAX1)
      END IF
C
      IF (YTYPE(2) .EQ. 0) THEN
C       put line on right side
        CALL RHTBDR (YTYPE(1),TICS(1), XLEN, YMLEN, SIZEL, MINY, MAXY)
      END IF
C
      IF (XTYPE .EQ. 0) THEN
C       time (hydrograph) plot
        CALL TMAXIS (XLEN,YMLEN,ALEN,SIZEL,NCRV,TSTEP,TUNITS,
     #               SDATIM,EDATIM,DTYPE,BOTOM)
      ELSE IF (XTYPE.EQ.1) THEN
C       arithmetic x-axis
        CALL AXAXIS (XLEN,YMLEN,XLABL,SIZEL,PLMN(4),PLMX(4),TICS(4),
     &               BOTOM)
      ELSE IF (XTYPE .EQ. 2) THEN
C       logrithmic x-axis
        MAXX = INT(1.01*ALOG10(PLMX(4)))
        MINX = INT(1.01*ALOG10(PLMN(4)))
        CALL LXAXIS (XLEN,YMLEN,XLABL,SIZEL,MINX,MAXX,BOTOM)
      ELSE IF (XTYPE .GE. 3 .AND. XTYPE .LE. 8) THEN
C       probability axis
        STDEV = ABS(PLMN(4))
        IF (PLMX(4) .GT. STDEV) STDEV = PLMX(4)
        PORR = XTYPE - 2
        CALL PBAXIS (XLEN,YMLEN,XLABL,SIZEL,STDEV,PORR,BOTOM)
      END IF
C
C     general plot
      CALL PLTITL(TITL,SIZEL,XLEN,BOTOM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TMCRVP
     I                   (K)
C
C     + + + PURPOSE + + +
C     This routine draws a curve and/or plots points for a single
C     time series.  Options for points off the scale are implemented
C     here.  The values are all transformed and converted to inches
C     before plotted.  Data is assummed to be instantaneous values
C     at the indicated times.
C
C     + + + HISTORY + + +
C     kmflynn  09/22/94  LOCBUF decreased from 505 to 404 because of
C                        limitation in INTRERACTOR software.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - order number of curve in data array
C
CC     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + PARAMETERS + + +
      INTEGER   LOCBUF
      PARAMETER (LOCBUF = 405)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ERR,TR,CUM,LY,LM,NDAYS,J
      INTEGER   PLFLG,ARFLG,CLFLG,LB5,N,M
      INTEGER   I1,L3,KX,KY
      REAL      YMN,YMX,XINC,YINC,BASE,YYLEN,ADJ,ZERO 
      REAL      X(LOCBUF),Y(LOCBUF),XTEMP,YTEMP,HGT,FAC
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNSDA, TIMDIF, PARROW, DAYMON, GMARK1, GMARKN
      EXTERNAL   GSLN, GPL, GFA, GSFAIS, GSFASX, GQMKSC, GSFASI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GQMKSC (ERR,FAC)
      IF (ERR .NE. 0 .OR. ABS(FAC).LT.0.01) FAC = 1.0
      HGT = SYMSIZ(K)*FAC
      ZERO = 0.0  
      I1= 1
      L3= 3
      TR = 0
      LY = SDATIM(1)
      LM = SDATIM(2)
      KY = WCHVR(1,K)
      KX = WCHVR(2,K)
      CUM= 0
      IF (CTYPE(K) .EQ. 1 .OR. CTYPE(K) .EQ. 5) THEN
        BASE = 0.0
        YYLEN = YLEN
        IF (ALEN .GT. 0.0001) YYLEN = YYLEN - ALEN - 1.5*SIZEL
      ELSE
        BASE = YLEN - ALEN
        YYLEN = ALEN
      END IF
      IF (WHICH(KY) .EQ. 1) THEN
C       plot for left axis
        TR = TRANSF(KY)
        YMN = PLMN(1)
        YMX = PLMX(1)
      ELSE IF (WHICH(KY) .EQ. 2) THEN
C       plot for right axis
        TR = TRANSF(KY)
        YMN = PLMN(2)
        YMX = PLMX(2)
      ELSE
        TR = 1
        YMN = PLMN(3)
        YMX = PLMX(3)
      END IF
      IF (TR .GT. 1) THEN
        CALL TRNSDA (TR,YMN,ERR)
        CALL TRNSDA (TR,YMX,ERR)
      END IF
      YINC = YYLEN/(YMX-YMN)
      IF (CTYPE(K) .NE. 5) THEN
C       uniform time step
        IF (TUNITS(K) .EQ. 5) THEN
C         is monthly so set flag
          CALL TIMDIF (SDATIM, EDATIM, 4, 1, NDAYS)
          IF (SDATIM(3).EQ.1 .AND. SDATIM(4).EQ.24) NDAYS = NDAYS + 1
          XINC = XLEN/REAL(NDAYS)
        ELSE
          XINC = XLEN/REAL(BUFPOS(2,KY)+1-BUFPOS(1,KY))
        END IF
      END IF
C
      PLFLG = 0
      CLFLG = 0
      ARFLG = 0
      LB5 = LOCBUF - 5
      N = 0
      XTEMP = 0.0
C
      DO 200 I = BUFPOS(1,KY),BUFPOS(2,KY)
        N = N + 1
        IF (CTYPE(K) .NE. 5) THEN
C         uniform time step
          IF (TUNITS(K) .EQ. 5) THEN
C           special case for monthly data where length of each
C           month may be different
            DO 6 M = 1,TSTEP(K)
              CUM = CUM + DAYMON(LY,LM)
              LM = LM + 1
              IF (LM .GT. 12) THEN
                LM = 1
                LY = LY + 1
              END IF
 6          CONTINUE
            X(N) = REAL(CUM)*XINC
          ELSE
C           all other time-series which are uniform
            X(N) = REAL(I+1-BUFPOS(1,KY))*XINC
          END IF
        ELSE
C         date tagged data, non-unifrom time-step
          J = I - BUFPOS(1,KY) + BUFPOS(1,KX)
          X(N) = YX(J)*XLEN
          XINC = X(N) - XTEMP
        END IF
C
        Y(N) = YX(I)
        IF (TR .GT. 1) CALL TRNSDA(TR,Y(N),ERR)
        Y(N) = (Y(N) -YMN)*YINC
C
        XTEMP = X(N)
C
        IF (Y(N) .LT. 0.0) THEN
C         value off bottom of plot
          GO TO (10,20,30,40), BVALFG(2)
 10       CONTINUE
C           clip, plot a point going off scale
            IF (CLFLG .EQ. -1) THEN
C             previous value also off scale
              Y(N) = 0.0
            ELSE
C             previous value good
              IF (I .GT. BUFPOS(1,KY)) THEN
C               not first point
                ADJ = XINC*Y(N)/(Y(N)-Y(N-1))
                X(N) = X(N) - ADJ
                Y(N) = 0.0
                N = N + 1
                X(N) = X(N-1) + ADJ
                Y(N) = 0.0
                CLFLG = -1
              ELSE
C               first point
                N = N - 1
                CLFLG = -1
              END IF
            END IF
            GO TO 90
C
 20       CONTINUE
C           skip value, do not connect points with previous
            N = N - 1
            PLFLG = 1
            XTEMP = -1.0
            GO TO 90
C
 30       CONTINUE
C           draw arrow
            CALL GSLN(I1)
            CALL PARROW (X(N),BASE,SIZEL,'B')
            CALL GSLN(LNTYP(K))
            N = N - 1
            PLFLG = 1
            ARFLG = 1
            XTEMP = -1.0
            GO TO 90
C
 40       CONTINUE
C           ignore point, connect good points
            N = N - 1
            IF (N .GT. 0) THEN
              XTEMP = X(N)
            ELSE
              XTEMP = -1.0
            END IF
            GO TO 90
C
 90       CONTINUE
C
        ELSE IF (Y(N) .GT. YYLEN) THEN
C         value off top of plot
          GO TO (110,120,130,140), BVALFG(1)
 110      CONTINUE
C           clip, plot at point going off scale
            IF (CLFLG .EQ. 1) THEN
C             previous value off scale
              Y(N) = YYLEN
            ELSE
C             previous value good
              IF (I .GT. BUFPOS(1,KY)) THEN
C               not first point
                ADJ = XINC*(Y(N)-YYLEN)/(Y(N)-Y(N-1))
                X(N) = X(N) -ADJ
                Y(N) = YYLEN
                N = N + 1
                X(N) = X(N-1) + ADJ
                Y(N) = YYLEN
                CLFLG = 1
              ELSE
C               first point
                N = N - 1
                CLFLG = 1
              END IF
            END IF
            GO TO 190
C
 120      CONTINUE
C           skip value, do not connect point with previous
            N = N - 1
            PLFLG = 1
            XTEMP = -1.0
            GO TO 190
C
 130      CONTINUE
C           draw arrow
            CALL GSLN(I1)
            CALL PARROW (X(N),YYLEN+BASE,SIZEL,'T')
            CALL GSLN(LNTYP(K))
            N = N - 1
            XTEMP = -1.0
            PLFLG = 1
            ARFLG = 1
            GO TO 190
C
 140      CONTINUE
C           ignore value, connect good points
            N = N - 1
            IF (N .GT. 0) THEN
              XTEMP = X(N)
            ELSE
              XTEMP = -1.0
            END IF
            GO TO 190
C
 190      CONTINUE
C
        ELSE
C         good point
          IF (CLFLG .NE. 0) THEN
C           but, previous value bad, so adjust
            N = N + 1
            Y(N) = Y(N-1)
            X(N) = X(N-1)
C           recompute previous point
            YTEMP = YX(I-1)
            IF (TR .GT. 1) CALL TRNSDA (TR, YTEMP, ERR)
            YTEMP = (YTEMP-YMN)*YINC
            IF (CLFLG .LT. 0) THEN
C             previous value off bottom
              IF (N .GE. 2) THEN
                ADJ = XINC*Y(N)/(Y(N)-YTEMP)
              ELSE
                ADJ = 0.0
              END IF
              X(N-1) = X(N-1) - ADJ
              Y(N-1) = 0.0
            ELSE
C             previous value off top
              IF (N .GE. 2) THEN
                ADJ = XINC*(YYLEN-Y(N))/(YTEMP-Y(N))
              ELSE
                ADJ = 0.0
              END IF
              X(N-1) = X(N-1) - ADJ
              Y(N-1) = YYLEN
            END IF
            CLFLG = 0
          END IF
        END IF
C
        IF (I .EQ. BUFPOS(2,KY)) PLFLG = 1
C       end of data. so plot buffer
        IF (N .GE. LB5) PLFLG = 1
C       buffer full enough, so plot buffer
        IF (N .LE. 0) PLFLG = 0
C       no data, no plot
C
        IF (PLFLG .EQ. 1) THEN
C         plot data from buffer
          IF (ALEN .GT. 0.0001) THEN
C           auxilary plot, make adjustment
            DO 195 J = 1,N
              Y(J) = Y(J) + BASE
 195        CONTINUE
          END IF
C
          IF (LNTYP(K) .NE. 0) THEN
C           draw a line or point
            IF (N .GT. 1) THEN
C             draw a line
              CALL GPL (N,X,Y)
            ELSE
C             draw a single point
              IF (SYMBL(K) .GT. 0)THEN
                CALL GMARK1(SYMBL(K),HGT,X,Y)
              ELSE
                CALL GMARK1(I1,HGT,X,Y)
              END IF
            END IF
          END IF
C
          IF (SYMBL(K) .NE. 0) THEN
C           plot symbols
            CALL GMARKN (SYMBL(K),HGT,X,Y,N,ZERO)  
          END IF
C
          IF (PATTRN(K).LT.0 .OR. PATTRN(K).GT.1) THEN
C           need a solid fill-in or pattern
            IF (PATTRN(K) .EQ. 2) THEN
C             solid color
              CALL GSFAIS (I1)
            ELSE
C             pattern
              CALL GSFAIS (L3)
              CALL GSFASX (PATTRN(K))
              CALL GSFASI (PATTRN(K))
            END IF
C           set values to close polygon
            X(N+1) = X(N)
            X(N+2) = X(1)
            X(N+3) = X(1)
            Y(N+1) = 0.0 + BASE- (YLEN/5000.)
            Y(N+2) = 0.0 + BASE- (YLEN/5000.)
            Y(N+3) = Y(1)
C           fill polygon
            CALL GFA (N+3,X,Y)
          END IF
          IF (XTEMP .LT. 0 .OR. ARFLG .EQ. 1) THEN
C           like starting over
            N = 0
          ELSE
C           continue lines
C           take off base since it will be added in again
            Y(1) = Y(N) - BASE
            X(1) = X(N)
            N = 1
          END IF
        END IF
C
        IF (N .GT. 0) THEN
          X(N) = XTEMP
        END IF
C
        PLFLG = 0
        ARFLG = 0
C
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TMCRVM
     I                   (K)
C
C     + + + PURPOSE + + +
C     This routine draws a curve and/or plots points for a single
C     time series.  Options for points off the scale are implemented
C     here.  The values are all transformed and converted to inches
C     before plotted.  Data is assummed to be average values over
C     the time step.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - order number of curve in data array
C
CC     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + PARAMETERS + + +
      INTEGER   LOCBUF
      PARAMETER (LOCBUF = 405)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ERR,TR,CUM,LY,LM,NDAYS,J
      INTEGER   PLFLG,ARFLG,LB5,N,M
      INTEGER   I1,L3,KX,KY
      REAL      YMN,YMX,XINC,YINC,BASE,YYLEN,Z,ZERO
      REAL      X(LOCBUF),Y(LOCBUF),XTEMP,FAC,HGT
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNSDA, TIMDIF, PARROW, DAYMON, GMARKN
      EXTERNAL   GSLN, GPL, GFA, GSFAIS, GSFASX, GQMKSC, GSFASI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GQMKSC (ERR,FAC)
      IF (ERR .NE. 0 .OR. ABS(FAC).LT.0.01) FAC = 1.0
      HGT = SYMSIZ(K)*FAC
C 
      ZERO = 0.0
      I1 = 1
      L3= 3
      TR = 0
      LY = SDATIM(1)
      LM = SDATIM(2)
      KY = WCHVR(1,K)
      KX = WCHVR(2,K)
      CUM= 0
      IF (CTYPE(K) .EQ. 1 .OR. CTYPE(K) .EQ. 5) THEN
        BASE = 0.0
        YYLEN= YLEN
        IF (ALEN .GT. 0.0001) YYLEN = YYLEN - ALEN - 1.5*SIZEL
      ELSE
        BASE = YLEN - ALEN
        YYLEN = ALEN
      END IF
      IF (WHICH(KY) .EQ. 1) THEN
C       plot for left axis
        TR = TRANSF(KY)
        YMN= PLMN(1)
        YMX= PLMX(1)
      ELSE IF (WHICH(KY) .EQ. 2) THEN
C       plot for right axis
        TR = TRANSF(KY)
        YMN= PLMN(2)
        YMX= PLMX(2)
      ELSE
        TR = 1
        YMN = PLMN(3)
        YMX = PLMX(3)
      END IF
      IF (TR .GT. 1) THEN
        CALL TRNSDA (TR,YMN,ERR)
        CALL TRNSDA (TR,YMX,ERR)
      END IF
      YINC = YYLEN/(YMX-YMN)
      IF (CTYPE(K) .NE. 5) THEN
C       uniform time step
        IF (TUNITS(K) .EQ. 5) THEN
C         is monthly so set flag
          CALL TIMDIF (SDATIM, EDATIM, 4, 1, NDAYS)
          IF (SDATIM(3).EQ.1 .AND. SDATIM(4).EQ.24) NDAYS = NDAYS + 1
          XINC = XLEN/REAL(NDAYS)
        ELSE
          XINC = XLEN/REAL(BUFPOS(2,KY)+1-BUFPOS(1,KY))
        END IF
      END IF
C
      PLFLG = 0
      ARFLG = 0
      LB5 = LOCBUF - 5
      N = 0
      XTEMP = 0.0
C
      DO 200 I = BUFPOS(1,KY),BUFPOS(2,KY)
        N = N + 2
        IF (CTYPE(K) .NE. 5) THEN
C         uniform time step
          IF (TUNITS(K) .EQ. 5) THEN
C           special case for monthly data where length of each
C           month may be different
            DO 6 M = 1,TSTEP(K)
              CUM = CUM + DAYMON(LY,LM)
              LM = LM + 1
              IF (LM .GT. 12) THEN
                LM = 1
                LY = LY + 1
              END IF
 6          CONTINUE
            X(N) = REAL(CUM)*XINC
          ELSE
C           all other time-series which are uniform
            X(N) = REAL(I+1-BUFPOS(1,KY))*XINC
          END IF
        ELSE
C         date tagged data, non-uniform time-step
          J = I - BUFPOS(1,KY) + BUFPOS(1,KX)
          X(N) = YX(J)*XLEN
          XINC = X(N) - XTEMP
        END IF
C
        X(N-1) = XTEMP
        Y(N) = YX(I)
        IF (TR .GT. 1) CALL TRNSDA(TR,Y(N),ERR)
        Y(N) = (Y(N) -YMN)*YINC
        Y(N-1) = Y(N)
C
        XTEMP = X(N)
C
        IF (Y(N) .LT. 0.0) THEN
C         value off bottom of plot
          GO TO (10,20,30,40), BVALFG(2)
 10       CONTINUE
C           clip, plot a point going off scale
C           previous value also off scale
            Y(N) = 0.0
            Y(N-1) = 0.0
            GO TO 90
C
 20       CONTINUE
C           skip value, do not connect points with previous
            N = N - 2
            PLFLG = 1
            GO TO 90
C
 30       CONTINUE
C           draw arrow
            CALL GSLN(I1)
            Z = (X(N)-X(N-1))/2.0
            CALL PARROW (Z,BASE,SIZEL,'B')
            CALL GSLN(LNTYP(K))
            N = N - 2
            PLFLG = 1
            ARFLG = 1
            GO TO 90
C
 40       CONTINUE
C           ignore point, connect good points
            N = N - 2
            GO TO 90
C
 90       CONTINUE
C
        ELSE IF (Y(N) .GT. YYLEN) THEN
C         value off top of plot
          GO TO (110,120,130,140), BVALFG(1)
 110      CONTINUE
C           clip, plot at point going off scale
C           previous value off scale
            Y(N) = YYLEN
            Y(N-1) = YYLEN
            GO TO 190
C
 120      CONTINUE
C           skip value, do not connect point with previous
            N = N - 2
            PLFLG = 1
            GO TO 190
C
 130      CONTINUE
C           draw arrow
            CALL GSLN(I1)
            Z = (X(N) + X(N-1))/2.0
            CALL PARROW (Z,YYLEN+BASE,SIZEL,'T')
            CALL GSLN(LNTYP(K))
            N = N - 2
            PLFLG = 1
            ARFLG = 1
            GO TO 190
C
 140      CONTINUE
C           ignore value, connect good points
            N = N - 2
            GO TO 190
C
 190      CONTINUE
C
C       ELSE
C         good point
        END IF
C
        IF (I .EQ. BUFPOS(2,KY)) PLFLG = 1
C       end of data. so plot buffer
        IF (N .GE. LB5) PLFLG = 1
C       buffer full enough, so plot buffer
        IF (N .LE. 0) PLFLG = 0
C       no data, no plot
C
        IF (PLFLG .EQ. 1) THEN
C         plot data from buffer
          IF (ALEN .GT. 0.0001) THEN
C           auxilary plot, make adjustment
            DO 195 J = 1,N
              Y(J) = Y(J) + BASE
 195        CONTINUE
          END IF
C
          IF (LNTYP(K) .NE. 0) THEN
C           draw a line or point
C           draw a line
            CALL GPL (N,X,Y)
          END IF
C
          IF (SYMBL(K) .NE. 0) THEN
C           plot symbols
            CALL GMARKN(SYMBL(K),HGT,X,Y,N,ZERO)
          END IF
C
          IF (PATTRN(K).LT.0 .OR. PATTRN(K).GT.1) THEN
C           need a solid fill-in or pattern
            IF (PATTRN(K) .EQ. 2) THEN
C             solid color
              CALL GSFAIS (I1)
            ELSE
C             pattern
              CALL GSFAIS (L3)
              CALL GSFASX (PATTRN(K))
              CALL GSFASI (PATTRN(K))
            END IF
C           set values to close polygon
            X(N+1) = X(N)
            X(N+2) = X(1)
            X(N+3) = X(1)
            Y(N+1) = 0.0 + BASE- (YLEN/5000.)
            Y(N+2) = 0.0 + BASE- (YLEN/5000.)
            Y(N+3) = Y(1)
C           fill polygon
            CALL GFA (N+3,X,Y)
          END IF
          IF (ARFLG .EQ. 1) THEN
C           like starting over
            N = 0
          ELSE
C           continue lines
            X(1) = X(N)
            Y(1) = Y(N) - BASE
            N = 1
          END IF
        END IF
C
        IF (N .GT. 0) X(N) = XTEMP
C
        PLFLG = 0
        ARFLG = 0
C
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PARROW
     I                   (X, Y, SIZ, DIR)
C
C     + + + PURPOSE + + +
C     This routine draws an arrow at the indicated coordinates in
C     inches and points in a direction off the scale.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   X, Y, SIZ
      CHARACTER*1 DIR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     X      - horizontal location on plot, in world coordinates
C     Y      - vertical location on plot, in world coordinates
C     SIZ    - height of lettering, in world coordinates
C     DIR    - character indicating direction of arrow
C              T - top
C              B - bottom
C              R - right side
C              L - left side
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I5
      REAL      PX, PY, SIN, DX(5), DY(5)
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL   GPL
C
C     + + + END SPECIFICATIONS + + +
C
C
      I5 = 5
      PX = X
      PY = Y
      IF (DIR .EQ. 'T') THEN
        SIN = -1.
C       draw arrow pointing up
        DX(1) = PX
        DX(2) = PX - SIZ * 0.3
        DX(3) = PX + SIZ * 0.3
        DX(4) = PX
        DX(5) = PX
        DY(1) = PY
        DY(2) = PY + SIN * SIZ * 0.5
        DY(3) = PY + SIN * SIZ * 0.5
        DY(4) = PY
        DY(5) = PY + SIN * SIZ
        CALL GPL (I5,DX,DY)
      ENDIF
      IF (DIR .EQ. 'B') THEN
        SIN = 1
C       draw arrow pointing down
        DX(1) = PX
        DX(2) = PX - SIZ * 0.3
        DX(3) = PX + SIZ * 0.3
        DX(4) = PX
        DX(5) = PX
        DY(1) = PY
        DY(2) = PY + SIN * SIZ * 0.5
        DY(3) = PY + SIN * SIZ * 0.5
        DY(4) = PY
        DY(5) = PY + SIN * SIZ
        CALL GPL (I5,DX,DY)
      ENDIF
      IF (dir .EQ. 'R') THEN
        SIN = -1
C       draw arrow pointing right
        DX(1) = PX
        DX(2) = PX + SIN * SIZ * 0.5
        DX(3) = PX + SIN * SIZ * 0.5
        DX(4) = PX
        DX(5) = PX + SIN * SIZ
        DY(1) = PY
        DY(2) = PY - SIZ * 0.3
        DY(3) = PY + SIZ * 0.3
        DY(4) = PY
        DY(5) = PY
        CALL GPL (I5,DX,DY)
      ENDIF
      IF (dir .EQ. 'L') THEN
        SIN = 1
C       draw arrow pointing left
        DX(1) = PX
        DX(2) = PX + SIN * SIZ * 0.5
        DX(3) = PX + SIN * SIZ * 0.5
        DX(4) = PX
        DX(5) = PX + SIN * SIZ
        DY(1) = PY
        DY(2) = PY - SIZ * 0.3
        DY(3) = PY + SIZ * 0.3
        DY(4) = PY
        DY(5) = PY
        CALL GPL (I5,DX,DY)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   VRTBAR
     I                   (K)
C
C     + + + PURPOSE + + +
C     This routine draws a bar from the bottom of the plot.  The
C     bars can be open or shaded.  Options for points off the scale
C     are implemented here.  The values are all transformed and
C     converted to inches before plotted.
C
C     + + + KEYWORDS + + +
C     DISSPLA
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - order number of curve in data array
C
CC     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, BVFLG, ERR, TR, CUM, LY, LM, NDAYS, L1, L4, L5
      INTEGER   I1,I3,ITMP,KY
      REAL      X, Y, XO, YMN, YMX, XA(5), YA(5),
     *          YINC, YYLEN, BASE, XINC, XP
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNSDA, TIMDIF, PARROW
      EXTERNAL   DAYMON
      EXTERNAL   GSFAIS, GSLN, GPL, GSFASX, GFA, GSFASI
C
C     + + + END SPECIFICATIONS + + +
C
C
C     BVFLG = 1 SKIP ANY PLOTTING
C     BVFLG = 2 CHANGE COLOR AND/OR LINE, THEN CONNECT GOOD POINTS
C
      I1   = 1
      I3   = 3
      L1   = 1
      L4   = 4
      L5   = 5
      BVFLG= 0
      TR   = 0
      LY   = SDATIM(1)
      LM   = SDATIM(2)
      KY   = WCHVR(1,K)
      CUM  = 0
      IF (CTYPE(K).EQ.2) THEN
        BASE = 0.0
        YYLEN = YLEN
        IF (ALEN .GT. 0.0001) YYLEN = YYLEN - 1.0 - 1.5*SIZEL
      ELSE
        BASE = YLEN - 1.0
        YYLEN = 1.0
      END IF
      IF (WHICH(K) .EQ. 1) THEN
C       plot for left axis
        TR = TRANSF(K)
        YMN = PLMN(1)
        YMX = PLMX(1)
      ELSE IF (WHICH(K) .EQ. 2) THEN
C       plot for right axis
        TR = TRANSF(K)
        YMN = PLMN(2)
        YMX = PLMX(2)
      ELSE
        TR = 1
        YMN = PLMN(3)
        YMX = PLMX(3)
      END IF
      IF (TR .GT. 1) THEN
        CALL TRNSDA (TR,YMN,ERR)
        CALL TRNSDA (TR,YMX,ERR)
      END IF
      YINC = YYLEN/(YMX-YMN)
      IF (TUNITS(K) .EQ. 5) THEN
C       is monthly
        CALL TIMDIF (SDATIM, EDATIM, L4, L1, NDAYS)
        IF (SDATIM(3).EQ.1 .AND. SDATIM(4).EQ.24) NDAYS = NDAYS + 1
        XINC = XLEN/REAL(NDAYS)
      ELSE
        XINC = XLEN/REAL(BUFPOS(2,KY)+1-BUFPOS(1,KY))
      END IF
      XO = 0.0
      IF (PATTRN(K).LT.0 .OR. PATTRN(K).GT.2) THEN
C       some type of hatch style
        CALL GSFAIS (I3)
        CALL GSFASX (PATTRN(K))
        CALL GSFASI (PATTRN(K))
      ELSE
C       hollow or solid
        ITMP = PATTRN(K) - 1
        IF (ITMP .LT. 0) ITMP = 0
        CALL GSFAIS (ITMP)
      END IF
C
      DO 50 I = BUFPOS(1,KY),BUFPOS(2,KY)
        IF (TUNITS(K) .NE. 5) THEN
          X = REAL(I+1-BUFPOS(1,KY))*XINC
        ELSE
C         monthly data
          CUM = CUM + DAYMON(LY,LM)
          LM = LM + 1
          IF (LM .GT. 12) THEN
            LM = 1
            LY = LY + 1
          END IF
          X = REAL(CUM)*XINC
        END IF
        Y = YX(I)
        IF (TR .GT. 1) THEN
          CALL TRNSDA (TR,Y,ERR)
        END IF
        Y = (Y - YMN)*YINC
C
        IF (Y .LT. 0.0) THEN
C         Values off the bottom
          GO TO (10, 20, 30, 40), BVALFG(2)
 10       CONTINUE
            Y = 0.0
            GO TO 90
 20       CONTINUE
            BVFLG = 1
            GO TO 90
 30       CONTINUE
            XP = 0.5*(XO+X)
            CALL GSLN (I1)
            CALL PARROW (XP,BASE,SIZEL,'B')
            CALL GSLN (LNTYP(K))
            BVFLG = 1
            GO TO 90
 40       CONTINUE
            BVFLG = 1
            GO TO 90
 90       CONTINUE
        ELSE IF (Y .GT. YYLEN ) THEN
C         values off the top
          GO TO (110, 120, 130, 140), BVALFG(1)
 110      CONTINUE
            Y = YYLEN
            GO TO 190
 120      CONTINUE
            BVFLG = 1
            GO TO 190
 130      CONTINUE
            XP = 0.5*(XO+X)
            CALL GSLN (I1)
            CALL PARROW (XP, YYLEN+BASE, SIZEL, 'T')
            CALL GSLN (LNTYP(K))
            BVFLG = 1
            GO TO 190
 140      CONTINUE
            BVFLG = 2
            Y = YYLEN
            CALL GSLN (LNTYP(K))
            GO TO 190
 190      CONTINUE
        END IF
C
        IF (BVFLG .NE. 1) THEN
C         good point
          XA(1) = XO
          XA(2) = XO
          XA(3) = X
          XA(4) = X
          XA(5) = XO
          YA(1) = BASE
          YA(2) = Y + BASE
          YA(3) = Y + BASE
          YA(4) = BASE
          YA(5) = BASE
          ITMP  = L4
          CALL GPL (ITMP,XA,YA)
          IF (BVFLG .EQ. 2) THEN
            CALL GSLN (LNTYP(K))
          ELSE
            ITMP= L5
            CALL GFA (ITMP,XA,YA)
          END IF
        END IF
C
        BVFLG = 0
        XO = X
 50   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   XYCURV
     I                    (K,ISYMSZ)
C
C     + + + PURPOSE + + +
C     This routine plots points for two time series plotted against
C     each other.  Options for points off the scale are implemented
C     here.  The values are all transformed and converted to inches
C     before plotted.
C
C     + + + KEYWORDS + + +
C     DISSPLA
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K, ISYMSZ
C
C     + + + ARGUMENT DEFINITIONS + + +
C     K      - order number of curve in data array
C     ISYMSZ - multiplier for sizing symbol based on 3rd variable
C
CC     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + PARAMETERS + + +
      INTEGER   LOCBUF
      PARAMETER (LOCBUF = 100)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, ERR, FLG, M, N, TR1, TR2, J,
     &          PCNT, CLIPFG, KY, KS, KX
      INTEGER   I1
      REAL      Z,XMN,XMX,YMN,YMX,SYMIN,SYMAX,DENOM,XX,YY,
     &          XINC,YINC,BXMN,BXMX,BYMN,BYMX,FAC,X,Y,SX(1),SY(1),
     &          PX(LOCBUF), PY(LOCBUF), XTEMP, YTEMP, SLOPE, HGT
      CHARACTER*1   DEL
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNSDA, PARROW, GMARK1
      EXTERNAL   GPL, GSLN, GQMKSC
C
C     + + + END SPECIFICATIONS + + +
C
C
C     FLG    = 0 - GOOD POINT, PLOT
C            = 1 - PLOT AT THE BOUNDARY, CLIP
C            = 2 - DON'T PLOT, SKIP
C            = 3 - PLOT ARROW AT BOUNDARY
C            = 4 - IGNORE
C     CLIPFG - flag for clipping action
C            = 0 - last point was good
C            = 1 - last point was bad
C
      I1  = 1
      CALL GQMKSC (ERR,FAC)
      IF (ERR .NE. 0 .OR. ABS(FAC).LT.0.01) FAC = 1.0
      HGT = SYMSIZ(K)*FAC
      CLIPFG = 0
      PCNT = 0
      KY = WCHVR(1,K)
      KX = WCHVR(2,K)
      KS = WCHVR(3,K)
C
      PX(1) = 0.0
      PY(1) = 0.0
      IF (WHICH(WCHVR(1,K)) .EQ. 1) THEN
C       curve plotted to left y-axis
        YMX = PLMX(1)
        YMN = PLMN(1)
      ELSE
C       curve plotted to right y-axis
        YMX = PLMX(2)
        YMN = PLMN(2)
      END IF
      XMX = PLMX(4)
      XMN = PLMN(4)
      BXMN = (BLNKIT(1)-0.0001)*XLEN
      BXMX = (BLNKIT(2)-0.0001)*XLEN
      BYMN = (BLNKIT(3)-0.0001)*YLEN
      BYMX = (BLNKIT(4)-0.0001)*YLEN
      IF (ISYMSZ .NE. 0) THEN
C       find range for symbol size
        SYMAX = -1.0E20
        SYMIN = 1.0E20
        DO 10 I = BUFPOS(1,KS),BUFPOS(2,KS)
          IF (SYMIN .GT. YX(I)) SYMIN = YX(I)
          IF (SYMAX .LT. YX(I)) SYMAX = YX(I)
 10     CONTINUE
        DENOM = (SYMAX-SYMIN)/REAL(ISYMSZ)
      END IF
C
      TR1 = TRANSF(WCHVR(1,K))
      TR2 = TRANSF(WCHVR(2,K))
      IF (TR1 .GT. 1) THEN
C       y axis transformation
        CALL TRNSDA (TR1,YMN,ERR)
        CALL TRNSDA (TR1,YMX,ERR)
      END IF
      IF (TR2 .GT. 1) THEN
C       x axis transformation
        CALL TRNSDA (TR2,XMN,ERR)
        CALL TRNSDA (TR2,XMX,ERR)
      END IF
C
      XINC = XLEN/(XMX-XMN)
      YINC = YLEN/(YMX-YMN)
C
      DO 50 M = BUFPOS(1,KY),BUFPOS(2,KY)
        N = BUFPOS(1,KX) + M - BUFPOS(1,KY)
        X = YX(N)
        Y = YX(M)
        IF (TR1. GT. 1) CALL TRNSDA (TR1,Y,ERR)
        IF (TR2 .GT. 1) CALL TRNSDA (TR2,X,ERR)
C
        Y = (Y-YMN)*YINC
        X = (X-XMN)*XINC
        FLG = 0
        IF (Y .LT. 0.0) THEN
          FLG = BVALFG(2)
          DEL = 'B'
        ELSE IF (Y .GT. YLEN) THEN
          FLG = BVALFG(1)
          DEL = 'T'
        END IF
C
        IF (X .LT. 0.0) THEN
          FLG = BVALFG(3)
          DEL = 'L'
        ELSE IF (X .GT. XLEN) THEN
          FLG = BVALFG(4)
          DEL = 'R'
        END IF
C
        IF (X.GT.BXMN.AND.X.LT.BXMX.AND.Y.GT.BYMN.AND.Y.LT.BYMX) THEN
C         point in box to white out (not plot)
          IF (LNTYP(K) .NE. 0) THEN
C           finish up line
            IF (PCNT .GT. 1) THEN
              CALL GPL(PCNT,PX,PY)
            ELSE IF (PCNT .EQ. 1) THEN
C             mark the line with a point
              IF (SYMBL(K) .GT. 0)THEN
                CALL GMARK1(SYMBL(K),HGT,PX(1),PY(1))
              ELSE
                CALL GMARK1(I1,HGT,PX(1),PY(1))
              END IF
            END IF
            PCNT = 0
          END IF
C
        ELSE
C         plot if good
          IF (FLG .EQ. 0 .AND. SYMBL(K) .NE. 0) THEN
            SX(1) = X
            SY(1) = Y
            IF (ISYMSZ .NE. 0) THEN
              J = BUFPOS(1,KS) + M - BUFPOS(1,KY)
              Z = HGT*((YX(J)-SYMIN)/DENOM)
              CALL GMARK1(SYMBL(K),Z,SX,SY)
            ELSE
              CALL GMARK1(SYMBL(K),HGT,SX,SY)
            END IF
          END IF
C
          IF (FLG .EQ. 0) THEN
C           good data value
            IF (LNTYP(K) .NE. 0) THEN
              IF (CLIPFG .EQ. 1) THEN
C               previous value was bad, add additional point
                PCNT = PCNT + 1
                SLOPE = (YTEMP-Y)/(XTEMP-X)
                IF (DEL .EQ. 'B') THEN
                  PY(PCNT) = 0.0
                  PX(PCNT) = XTEMP + (0.0-YTEMP)/SLOPE
                ELSE IF (DEL .EQ. 'T') THEN
                  PY(PCNT) = YLEN
                  PX(PCNT) = XTEMP + (YLEN-YTEMP)/SLOPE
                ELSE IF (DEL .EQ. 'R') THEN
                  PX(PCNT) = XLEN
                  PY(PCNT) = YTEMP + (XLEN-XTEMP)*SLOPE
                ELSE IF (DEL .EQ. 'L') THEN
                  PX(PCNT) = 0.0
                  PY(PCNT) = YTEMP + (0.0-XTEMP)*SLOPE
                END IF
                IF (PCNT .GE. 50) THEN
C                 must plot buffer
                  CALL GPL(PCNT,PX,PY)
                  PX(1) = PX(PCNT)
                  PY(1) = PY(PCNT)
                  PCNT = 1
                END IF
                CLIPFG = 0
              END IF
C
              PCNT = PCNT + 1
              PX(PCNT) = X
              PY(PCNT) = Y
              IF (PCNT .GE. 50) THEN
C               plot the buffer
                CALL GPL (PCNT,PX,PY)
                PX(1) = PX(PCNT)
                PY(1) = PY(PCNT)
                PCNT = 1
              END IF
            END IF
C
          ELSE IF (FLG .EQ. 1) THEN
C           bad data value, clip
            IF (LNTYP(K) .NE. 0) THEN
              IF (CLIPFG .EQ. 0 .AND. PCNT .GT. 0) THEN
C               previous value good, find new location
                PCNT = PCNT + 1
                DENOM = X-XTEMP
                IF (ABS(DENOM) .LT. 1.0E-19) DENOM = 1.0E-19
                SLOPE = (Y-YTEMP)/DENOM     
                IF (DEL .EQ. 'B') THEN
                  PY(PCNT) = 0.0
                  PX(PCNT) = XTEMP + (0.0-YTEMP)/SLOPE
                ELSE IF (DEL .EQ. 'T') THEN
                  PY(PCNT) = YLEN
                  PX(PCNT) = XTEMP + (YLEN-YTEMP)/SLOPE
                ELSE IF (DEL .EQ. 'R') THEN
                  PX(PCNT) = XLEN
                  PY(PCNT) = YTEMP + (XLEN-XTEMP)*SLOPE
                ELSE IF (DEL .EQ. 'L') THEN
                  PX(PCNT) = 0.0
                  PY(PCNT) = YTEMP + (0.0-XTEMP)*SLOPE
                END IF
C               plot out buffer
                IF (PCNT .GT. 1) THEN
                  CALL GPL(PCNT,PX,PY)
                ELSE IF (PCNT .EQ. 1) THEN
C                 mark the line with a point
                  IF (SYMBL(K) .GT. 0)THEN
                    CALL GMARK1(SYMBL(K),HGT,PX(1),PY(1))
                  ELSE
                    CALL GMARK1(I1,HGT,PX(1),PY(1))
                  END IF
                END IF
                PCNT = 0
              END IF
              CLIPFG = 1
            END IF
C
          ELSE IF (FLG .EQ. 2) THEN
C           don't plot point, skip, plot out buffer
            IF (LNTYP(K) .NE. 0) THEN
C             finish up line
              IF (PCNT .GT. 1) THEN
                CALL GPL(PCNT,PX,PY)
              ELSE IF (PCNT .EQ. 1) THEN
C               mark the line with a point
                IF (SYMBL(K) .GT. 0)THEN
                  CALL GMARK1(SYMBL(K),HGT,PX(1),PY(1))
                ELSE
                  CALL GMARK1(I1,HGT,PX(1),PY(1))
                END IF
              END IF
              PCNT = 0
            END IF
C
          ELSE IF (FLG .EQ. 3) THEN
C           bad data value, off scale, plot arrows
            IF (LNTYP(K) .NE. 0) THEN
C             finish up line
              IF (PCNT .GT. 1) THEN
                CALL GPL(PCNT,PX,PY)
              ELSE IF (PCNT .EQ. 1) THEN
C               mark the line with a point
                IF (SYMBL(K) .GT. 0)THEN
                  CALL GMARK1(SYMBL(K),HGT,PX(1),PY(1))
                ELSE
                  CALL GMARK1(I1,HGT,PX(1),PY(1))
                END IF
              END IF
              PCNT = 0
            END IF
C           locate and plot arrow
            XX = X
            YY = Y
            IF (YY .LT. 0.0) YY = 0.0
            IF (YY .GT. YLEN) YY = YLEN
            IF (XX .LT. 0.0) XX = 0.0
            IF (XX .GT. XLEN) XX = XLEN
            CALL GSLN (I1)
            CALL PARROW (XX, YY, SIZEL, DEL)
            CALL GSLN (LNTYP(K))
C
          ELSE IF (FLG .EQ. 4) THEN
C           bad point, do nothing, ignore
C
          END IF
        END IF
C
C       set temp values for clipping process
        XTEMP = X
        YTEMP = Y
 50   CONTINUE
C
      IF (LNTYP(K) .NE. 0 .AND. PCNT .GT. 1) THEN
C       plot buffer to finish line
        CALL GPL (PCNT, PX, PY)
      END IF
C
      RETURN
      END

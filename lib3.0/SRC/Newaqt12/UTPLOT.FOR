C
C
C
      SUBROUTINE   QDTMPL
     I                   (WINDOW,ARHLOG,MNLTZO,ARPCSC,NVAR,WHICH,
     I                    NPTS,NTYPE,BUFMAX,YX,DATE,TS,TU,DTYPE,
     I                    TYPIND,LNTYP,COLOR,PATTRN,SYMBOL,
     I                    CLAB,YLLAB,YRLAB,ALAB,TITL,CMPTYP)
C
C     + + + PURPOSE + + +
C     Call routines to make a time series plot.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ARHLOG(*),NVAR,WHICH(NVAR),NPTS,BUFMAX,NTYPE,
     $              DATE(6),TS,TU,DTYPE(*),
     $              TYPIND(NTYPE),COLOR(*),LNTYP(*),
     $              PATTRN(*),WINDOW,MNLTZO,SYMBOL(*),CMPTYP
      REAL          ARPCSC,YX(BUFMAX)
      CHARACTER*20  CLAB(NVAR)
      CHARACTER*80  YLLAB,YRLAB,ALAB
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITION + + +
C     WINDOW - to make plot in
C     ARHLOG - type of axis - 1- arith, 2-log
C     MNLTZO - allow minimum values to be less than zero on plot,
C              0:no, 1:yes, 2:only to -10% of max, 3:force zero to
C              middle of plot
C     ARPCSC - percent of arithmatic scale to use
C     NVAR   - number of lines to plot
C     WHICH  - for each line,
C              1 - left y-axis
C              2 - right y-axis
C              3 - auxiliary axis
C     NPTS   - number of points (number of time steps)
C     NTYPE  - number of different line types possible
C     BUFMAX - size of data buffer YX
C     YX     - data to be plotted
C     DATE   - start of event (yr,mo,dy,hr,mn,sec)
C     TS     - time step in TU units
C     TU     - time units (1-sec, 2-min, 3-hour, 3-day....)
C     DTYPE  - data type (1 - mean, 2 - point)
C     CLAB   - array of legends for curves
C     YLLAB  - left axis label
C     YRLAB  - right axis label
C     ALAB   - auxillary axis label
C     TITL   - title of plot
C     COLOR  - line colors for each line
C     LNTYP  - line types for each line
C     PATTRN - fill pattern for each line
C     SYMBOL - symbol for each line
C     CMPTYP - computer type
C     TYPIND - index of which set of specs to use with this data set
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,N,ITS(TSMAX),ITU(TSMAX),RETCOD,GTICS(3),NCRV,LOC,
     1            GCTYP(TSMAX),ICLOS,IWAIT,GBVALF(4),GTRANF(TSMAX),
     2            GXTYP,GYTYP(2),LSYMBL(TSMAX),LDTYPE(TSMAX),
     4            EDATE(6),L0,L1,L4,IFLG1,IFLG2,IFLG3,
     5            LCOLOR(TSMAX),LPTTRN(TSMAX),LLNTYP(TSMAX)
      REAL        GVMIN(TSMAX),GVMAX(TSMAX),PLMN(4),PLMX(4),
     &            SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN,
     &            LMIN,LMAX,AMIN,AMAX,RMIN,RMAX
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    GPNCRV, GPDATR, GPCURV, GPVAR, GPSCLE, GPLABL
      EXTERNAL    GPSIZE, ZIPI,  PLTONE, PSTUPW,  PDNPLT, GPWCTM
      EXTERNAL    TIMADD, GPTIME, SCALIT, GPXLEN, HVXLIN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SIZEL,XPHYS,YPHYS
     &      / 0.11,1.5,  1.5  /
      DATA   L0,L1,L4/0,1,4/
C
C     + + + END SPECIFICATIONS + + +
C
C     write (99,*) 'ARHLOG,NVAR,NPTS,BUFMAX',ARHLOG,NVAR,NPTS,BUFMAX
C     write (99,*) 'YX1, 1-5,last 5',(YX(I),I=1,5),(YX(I),I=NPTS-4,NPTS)
C     write (99,*) 'YX2, 1-5,last 5',(YX(I),I=NPTS+1,NPTS+5),
C    1                               (YX(I),I=2*NPTS-4,2*NPTS)
C
C     set number of curves and variables
      NCRV = NVAR
      CALL GPNCRV(NCRV,NVAR)
      LOC= 1
C     save the Y data
      DO 15 I = 1,NCRV
        CALL GPDATR (I,LOC,NPTS,YX(LOC),RETCOD)
        IF (WHICH(I).EQ.1 .OR. WHICH(I).EQ.2) THEN
C         left or right axis
          GTRANF(I) = ARHLOG(WHICH(I))
        ELSE
C         auxiliary axis, use same as left
          GTRANF(I) = ARHLOG(1)
        END IF
        CALL GPWCTM( I, I )
        LOC = LOC + NPTS
 15   CONTINUE
C
C     save curve info
      DO 25 I = 1,NCRV
        IF (WHICH(I) .EQ. 3) THEN
          GCTYP(I) = 3
        ELSE
          GCTYP(I) = 1
        END IF
C
        LSYMBL(I) = SYMBOL(TYPIND(I))
        LLNTYP(I) = LNTYP(TYPIND(I))
        LCOLOR(I) = COLOR(TYPIND(I))
        LPTTRN(I) = PATTRN(TYPIND(I))
        IF (LSYMBL(I).GT.0 .AND. LLNTYP(I).EQ.0) THEN
C         single points
          LDTYPE(I)= 2
        ELSE
C         mean values on a line
          LDTYPE(I)= 1
        END IF
 25   CONTINUE
C
C     patterns
      CALL GPCURV (GCTYP,LLNTYP,LSYMBL,LCOLOR,LPTTRN,CLAB)
C     compute and set data ranges
      LMIN =  1.0E30
      LMAX = -1.0E30
      RMIN =  1.0E30
      RMAX = -1.0E30
      AMIN =  1.0E30
      AMAX = -1.0E30
C
      IFLG1 = 0
      IFLG2 = 0
      IFLG3 = 0
C
      DO 75 I = 1,NCRV
        GVMIN(I) = 1.0E30
        GVMAX(I) = -1.0E30
        IF (WHICH(I) .EQ. 1) THEN
C         left y-axis
          DO 35 N= 1,NPTS
            J = (I-1)*NPTS + N
            IF (YX(J) .GT. GVMAX(I)) GVMAX(I) = YX(J)
            IF (YX(J) .LT. GVMIN(I)) GVMIN(I) = YX(J)
            IF (YX(J) .GT. LMAX) LMAX = YX(J)
C
            IF (YX(J) .LT. LMIN) THEN
C             new min
              IF (ARHLOG(WHICH(I)).EQ.2 .AND. YX(J).GE.0.0) THEN
C               ignore values <0 with log scale
C               new min
                LMIN = YX(J)
              ELSE
C               new min
                LMIN = YX(J)
              END IF
            END IF
 35       CONTINUE
          IF (MNLTZO.EQ.0 .AND. LMIN.LT.0.0) THEN
C           cant allow min less than zero
            LMIN = 0.0
          ELSE IF (MNLTZO.EQ.3) THEN
C           need as much below zero on plot as above
            IF (LMAX.GT.ABS(LMIN) .AND. LMAX.GT.0.0) THEN
C             set lmin to negative of lmax so zero is plot center
              LMIN = -1.0*LMAX
            ELSE IF (LMAX.LT.ABS(LMIN) .AND. LMAX.GT.0.0) THEN
C             set lmax to negative of lmin so zero is plot center
              LMAX = -1.0*LMIN
            ELSE IF (LMAX.LT.0.0) THEN
C             set lmax to negative of lmin so zero is plot center
              LMAX = -1.0*LMIN
            END IF
          END IF
          IFLG1 = 1
        ELSE IF (WHICH(I) .EQ. 2) THEN
C         right y-axis
          DO 45 N= 1,NPTS
            J = (I-1)*NPTS + N
            IF (YX(J) .GT. GVMAX(I)) GVMAX(I) = YX(J)
            IF (YX(J) .LT. GVMIN(I)) GVMIN(I) = YX(J)
            IF (YX(J) .GT. RMAX) RMAX = YX(J)
C
            IF (YX(J) .LT. RMIN) THEN
C             new min
              IF (ARHLOG(WHICH(I)).EQ.2 .AND. YX(J).GE.0.0) THEN
C               ignore values <0 with log scale
C               new min
                RMIN = YX(J)
              ELSE
C               new min
                RMIN = YX(J)
              END IF
            END IF
 45       CONTINUE
          IF (MNLTZO.EQ.0 .AND. RMIN.LT.0.0) THEN
C           cant allow min less than zero
            RMIN = 0.0
          ELSE IF (MNLTZO.EQ.3) THEN
C           need as much below zero on plot as above
            IF (LMAX.GT.ABS(LMIN) .AND. LMAX.GT.0.0) THEN
C             set lmin to negative of lmax so zero is plot center
              LMIN = -1.0*LMAX
            ELSE IF (LMAX.LT.ABS(LMIN) .AND. LMAX.GT.0.0) THEN
C             set lmax to negative of lmin so zero is plot center
              LMAX = -1.0*LMIN
            ELSE IF (LMAX.LT.0.0) THEN
C             set lmax to negative of lmin so zero is plot center
              LMAX = -1.0*LMIN
            END IF
          END IF
          IFLG2 = 1
        ELSE IF (WHICH(I) .EQ. 3) THEN
C         auxiliary axis
          DO 55 N= 1,NPTS
            J = (I-1)*NPTS + N
            IF (YX(J) .GT. GVMAX(I)) GVMAX(I) = YX(J)
            IF (YX(J) .LT. GVMIN(I)) GVMIN(I) = YX(J)
            IF (YX(J) .GT. AMAX) AMAX = YX(J)
            IF (YX(J) .LT. AMIN) AMIN = YX(J)
 55       CONTINUE
          IFLG3 = 1
        END IF
 75   CONTINUE
C
C     make sure mins not so small that they ruin plot scaling
      IF (LMIN.LT.-0.1*LMAX .AND. MNLTZO.EQ.2) THEN
C       change lmin for a more meaningful plot
        LMIN = -0.1*LMAX
      END IF
      IF (RMIN.LT.-0.1*RMAX .AND. MNLTZO.EQ.2) THEN
C       change rmin for a more meaningful plot
        RMIN = -0.1*RMAX
      END IF
      IF (AMIN.LT.-0.1*AMAX .AND. MNLTZO.EQ.2) THEN
C       change amin for a more meaningful plot
        AMIN = -0.1*AMAX
      END IF
C
C     y axis type
      I= 10
      CALL ZIPI (I,ARHLOG(1),GTRANF)
      DO 77 I = 1,NCRV
        IF (WHICH(I).EQ.1 .OR. WHICH(I).EQ.2) THEN
C         left or right axis
          GTRANF(I) = ARHLOG(WHICH(I))
        END IF
 77   CONTINUE
      CALL GPVAR (GVMIN,GVMAX,WHICH,GTRANF,CLAB)
C     WRITE (*,*)'GPVAR: GVMIN,GVMAX,WHICH',GVMIN,GVMAX,WHICH
C
      GTICS(1) = 10
      GTICS(2) = 10
      GTICS(3) = 2
      CALL ZIPI(L4,L0,GBVALF)
C     clip bottom, no connection
      GBVALF(2)= 2
C
C     set up axis scaling
      IF (IFLG1 .EQ. 1) THEN
C       WRITE (*,*) 'ARHLOG,ARPCSC:',ARHLOG(1),ARPCSC
        CALL SCALIT(ARHLOG(1), LMIN, LMAX, PLMN(1), PLMX(1))
        IF (PLMX(1) - PLMN(1) .LT. 1.0E-15) THEN
C         min and max must be different
          PLMX(1) = PLMX(1) + 1.0E-15
        END IF
        IF (ARHLOG(1) .EQ. 1) THEN
C         adjust arith scale
          PLMX(1)= PLMX(1)* ARPCSC/100.0
        END IF
      END IF
      IF (IFLG2 .EQ. 1) THEN
        CALL SCALIT(ARHLOG(2), RMIN, RMAX, PLMN(2), PLMX(2))
        IF (PLMX(2) - PLMN(2) .LT. 1.0E-15) THEN
C         min and max must be different
          PLMX(2) = PLMX(2) + 1.0E-15
        END IF
        IF (ARHLOG(2) .EQ. 1) THEN
C         adjust arith scale
          PLMX(2)= PLMX(2)* ARPCSC/100.0
        END IF
      END IF
      IF (IFLG3 .EQ. 1) THEN
        CALL SCALIT(L1, AMIN, AMAX, PLMN(3), PLMX(3))
        IF (PLMX(3) - PLMN(3) .LT. 1.0E-15) THEN
C         min and max must be different
          PLMX(3) = PLMX(3) + 1.0E-15
        END IF
      END IF
C     WRITE (*,*)'SCALIT: IFLG1,LMIN,LMAX,PLMN1,PLMX1',
C    1                       IFLG1,LMIN,LMAX,PLMN(1),PLMX(1)
C     WRITE (*,*)'SCALIT: IFLG2,LMIN,LMAX,PLMN2,PLMX2',
C    1                       IFLG2,LMIN,LMAX,PLMN(2),PLMX(2)
C     WRITE (*,*)'SCALIT: IFLG3,LMIN,LMAX,PLMN3,PLMX3',
C    1                       IFLG3,LMIN,LMAX,PLMN(3),PLMX(3)
C
      CALL GPSCLE(PLMN,PLMX,GTICS,GBVALF)
C     WRITE (*,*)'GPSCLE: GTICS,GBVALF',GTICS,GBVALF
C
C     define axes
      GXTYP= 0
      GYTYP(1)= 0
      GYTYP(2)= 0
C     assume no aux axis
      ALEN = 0.0
C
      DO 80 I = 1, NCRV
        IF (WHICH(I) .EQ. 1) THEN
          GYTYP(1) = ARHLOG(1)
        ELSE IF (WHICH(I) .EQ. 2) THEN
          GYTYP(2) = ARHLOG(2)
        ELSE IF (WHICH(I) .EQ. 3) THEN
          ALEN = 1.0
        END IF
 80   CONTINUE
C
C     WRITE (*,*) 'GPLABL:GXTYP,GYTYP,ALEN',GXTYP,GYTYP,ALEN
      CALL GPLABL (GXTYP,GYTYP,ALEN,YLLAB,YRLAB,ALAB,TITL)
C     set up times
C     WRITE (*,*) 'TIMADD'
      CALL TIMADD (DATE,TU,TS,NPTS,EDATE)
C     assume all are mean or total
      CALL ZIPI(NCRV, TS, ITS)
      CALL ZIPI(NCRV, TU, ITU)
C     WRITE (*,*) 'GPTIME',NCRV
      CALL GPTIME (ITS,ITU,DATE,EDATE,LDTYPE)
      XLEN = 7.5
      XPAGE= 10.0
      YLEN = 5.0
      YPAGE= 8.0
      CALL GPXLEN (XLEN,XPAGE,YLEN,YPAGE)
C     set up sizes
C     WRITE (*,*) 'GPSIZE',NCRV
      CALL GPSIZE (SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN)
C     set up for plot
C     WRITE (*,*) 'PSTUPW'
      CALL PSTUPW ( WINDOW, RETCOD )
      IF (MNLTZO.EQ.3) THEN
C       want zero line through middle of plot
        I = 4
        CALL HVXLIN (XLEN,YLEN,I)
      END IF
C     make the plot
C     WRITE (*,*) 'PLTONE'
      CALL PLTONE
C     exit from plot, dont wait
      IWAIT= 0
      IF (CMPTYP .NE. 1) THEN
C       dont close workstation
        ICLOS= 0
      ELSE
C       close workstation on pc
        ICLOS= 1
      END IF
      CALL PDNPLT ( WINDOW, ICLOS, IWAIT )
C
      RETURN
      END
C
C
C
      SUBROUTINE   QDFDPL
     I                   (IPLT,ARHLOG,WINDOW,NPTS,BUFMAX,NTYPE,NCRV,
     I                    TYPIND,COLOR,LNTYP,
     I                    CLAB,XLAB,YLLAB,YRLAB,TITL,DEVTYP,DEVCOD,
     I                    CMPTYP,
     M                    YX)
C
C     + + + PURPOSE + + +
C     Plot a flow duration curve using DISSPLA/GKS graphics.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IPLT,ARHLOG,WINDOW,NPTS,BUFMAX,NTYPE,NCRV,
     1              DEVTYP,DEVCOD,TYPIND(NCRV),COLOR(*),LNTYP(*),CMPTYP
      REAL          YX(BUFMAX)
      CHARACTER*20  CLAB(NTYPE)
      CHARACTER*80  XLAB,YLLAB,YRLAB
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IPLT   - number of plot (8 - flow dur, 10 - recess dur)
C     ARHLOG - type of y-axis, 1- arith, 2- log
C     WINDOW - to make plot in
C     NPTS   - number of points in PTS to plot
C     BUFMAX - size of data array
C     NTYPE  - number of different types of lines possible to plot
C     NCRV   - number of curves desired on plot
C     COLOR  - array of colors for plot
C     LNTYP  - array of line types for plot
C     CLAB   - array of legends for curves
C     XLAB   - X-axis label
C     YLLAB  - left axis label
C     YRLAB  - right axis label
C     TITL   - title of plot
C     YX     - values to be plotted
C     DEVTYP - device type
C     DEVCOD - device code
C     CMPTYP - computer type code
C     TYPIND - index of which set of specs to use with this data set
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,L,N,NCI,NCIM1,GSYMBL(TSMAX),RETCOD,IPOS,INDX,
     1            CNUMA,CNUMB(TSMAX),TNUM,NUMA(35),NUMB(TSMAX,35),NVAR,
     2            WHICH(TSMAX),GTICS(4),GBVALF(4),GCTYP(TSMAX),IEXP,
     3            GPATRN(TSMAX),GTRANF(TSMAX),GXTYP,GYTYP(2),II,
     4            ICLOS,IWAIT,I1,GCOLOR(TSMAX),GLNTYP(TSMAX)
      REAL        LMIN,LMAX,CLASS(35),CPCTA(35),CPCTB(TSMAX,35),R,
     1            BOUND(2),C,CR,CLOG,SUMA(35),SUMB(TSMAX,35),SIZEL,
     2            XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN,
     3            GVMIN(TSMAX),GVMAX(TSMAX),PLMN(4),PLMX(4),LYX(400)
      CHARACTER*1 OLAB(20,TSMAX),BLNK
C
C     + + + FUNCTIONS + + +
      REAL        GAUSEX
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG10, INT, REAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    GAUSEX, PSTUPW, PLTONE, PDNPLT, ZIPR, ZIPC
      EXTERNAL    GPNCRV, GPDATR, GPWCXY, GPCURV, GPVAR, SCALIT, GPSCLE
      EXTERNAL    GPSIZE, GPLABL, ZIPI, GPLBXB, GPXLEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SIZEL,XPHYS,YPHYS,ALEN
     &     /  0.12, 2.5,  1.5,  0.0/
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
C
      NVAR= NCRV + 1
      CALL GPNCRV(NCRV,NVAR)
C
C     WRITE(99,*)'IN QDFDPL, IPLT',IPLT
      IF (IPLT.EQ.8) THEN
C       determine bounds for duration analysis
        LMIN= 1.0E6
        LMAX= 1000
        DO 10 I= 1,NPTS
          IF (YX(I).LT.LMIN) LMIN= YX(I)
          IF (YX(I).GT.LMAX) LMAX= YX(I)
 10     CONTINUE
        IEXP= INT(ALOG10(LMAX))
        BOUND(2)= 10.0**(IEXP+1)
        IF (LMIN .LE. 0.0) LMIN = LMAX/1000.0
        IEXP= INT(ALOG10(LMIN))
        BOUND(1)= 10.0**(IEXP)
C       set up class intervals
        CR = (BOUND(1)/BOUND(2))**(1.0/33.0)
        CLASS(1) = 0.0
        CLASS(2) = BOUND(1)
        CLASS(35)= BOUND(2)
        DO 20 N = 1,32
          I = 35 - N
          CLASS(I) = CLASS(I+1)*CR
 20     CONTINUE
C
C       round off class intervals
        DO 30 I = 2,35
          C = CLASS(I)
          CLOG = ALOG10(C) + 0.001
          IF (CLOG.LT.0.0) CLOG = CLOG - 1
          L = INT(CLOG)
          L = L - 1
          C = (C/(10.0**L)) + 0.5
          CLASS(I) = (INT(C))*(10.0**L)
 30     CONTINUE
        NCI = 35
      ELSE
C       set intervals from 0.0 to 1.0 for recession duration
        CLASS(1) = 0.0
        CLASS(21)= 1.0
        DO 35 I= 1,19
          CLASS(I+1)= 0.05*I
 35     CONTINUE
        NCI= 21
      END IF
C
      I= 0
      CALL ZIPI (NCI,I,NUMA)
      CALL ZIPI (TSMAX*NCI,I,NUMB)
      R= 0.0
      CALL ZIPR (NCI,R,SUMA)
      CALL ZIPR (TSMAX*NCI,R,SUMB)
C
C     begin loop to fill class intervals.
      DO 60 N = 1,NPTS
        I = NCI + 1
 40     CONTINUE
          I = I - 1
        IF (YX(N).LT.CLASS(I) .AND. I.GT.1)   GO TO 40
        NUMA(I) = NUMA(I) + 1
        SUMA(I) = SUMA(I) + YX(N)
 60   CONTINUE
C
      TNUM= 0
      DO 70 I= 1,NCI
        TNUM= TNUM+ NUMA(I)
 70   CONTINUE
      CNUMA = TNUM
      CPCTA(1) = 100.0
      DO 80 I = 1,NCI
        IF (NUMA(I).GT.0) THEN
          SUMA(I) = SUMA(I)/REAL(NUMA(I))
        END IF
        IF (I.GT.1) CNUMA = CNUMA - NUMA(I-1)
        CPCTA(I)= 100.0*REAL(CNUMA)/REAL(TNUM)
C       WRITE(99,*)'NUMA,SUMA,CNUMA,TNUM,CPCTA',
C    1                NUMA(I),SUMA(I),CNUMA,TNUM,CPCTA(I)
 80   CONTINUE
C
      NCIM1 = NCI - 1
      DO 90 I = 1,NCIM1
C       Y data
        LYX(I) = CLASS(I+1)
C       X data, simulated
        LYX(I+NCIM1)  = -GAUSEX(0.01*CPCTA(I+1))
 90   CONTINUE
C
      IF (NCRV.GE.2) THEN
        DO 45 II= 1,NCRV-1
          DO 65 N = 1,NPTS
C           do remaining variables
            J = NCI + 1
 50         CONTINUE
              J = J - 1
            IF (YX((II*NPTS)+N).LT.CLASS(J) .AND. J.GT.1)   GO TO 50
            NUMB(II,J) = NUMB(II,J) + 1
            SUMB(II,J) = SUMB(II,J) + YX((II*NPTS)+N)
 65       CONTINUE
C
          CNUMB(II)= TNUM
          CPCTB(II,1) = 100.0
          DO 85 I = 1,NCI
            IF (NUMB(II,I).GT.0) THEN
              SUMB(II,I) = SUMB(II,I)/REAL(NUMB(II,I))
            END IF
            IF (I.GT.1) CNUMB(II) = CNUMB(II) - NUMB(II,I-1)
            CPCTB(II,I)= 100.0*REAL(CNUMB(II))/REAL(TNUM)
C           WRITE(99,*)'NUMB,SUMB,CNUMB,TNUM,CPCTB',
C    1                  NUMB(II,I),SUMB(II,I),CNUMB(II),TNUM,CPCTB(II,I)
 85       CONTINUE
C
          DO 95 I = 1,NCIM1
C           X data, observed
            LYX(I+((II+1)*NCIM1))= -GAUSEX(0.01*CPCTB(II,I+1))
 95       CONTINUE
 45     CONTINUE
      END IF
C
C     save the y data
      IPOS= 1
      INDX= 1
      CALL GPDATR (INDX,IPOS,NCIM1,LYX(IPOS),RETCOD)
C
      DO 15 I= 1,NCRV
C       save the x data
        INDX= INDX+ 1
        IPOS= IPOS+ NCIM1
        CALL GPDATR (INDX,IPOS,NCIM1,LYX(IPOS),RETCOD)
C       save where the data went for this curve
        CALL GPWCXY (I,I1,INDX)
C       set curve info
C       xy plot
        GCTYP(I) = 6
C       no symbols
        GSYMBL(I)= 0
C       no pattern
        GPATRN(I)= 0
C       save curve specs in local array
        GLNTYP(I)= LNTYP(TYPIND(I))
        GCOLOR(I)= COLOR(TYPIND(I))
 15   CONTINUE
      CALL GPCURV (GCTYP,GLNTYP,GSYMBL,GCOLOR,GPATRN,CLAB)
C
C     set up y axis scaling for class interval data
      GVMIN(1) = LYX(1)
      GVMAX(1) = LYX(NCIM1)
      GTRANF(1)= ARHLOG
      WHICH(1) = 1
      DO 25 I= 2,NCRV
        WHICH(I) = 4
        GVMIN(I) = LYX(I*NCIM1)
        GVMAX(I) = LYX(((I-1)*NCIM1)+1)
        GTRANF(I)= 1
 25   CONTINUE
      CALL ZIPC (20*TSMAX,BLNK,OLAB)
      CALL GPVAR (GVMIN,GVMAX,WHICH,GTRANF,OLAB)
C
C     make sure gvmin not so small that it ruins plot scaling
      IF (GVMIN(1).LT.-0.1*GVMAX(1)) THEN
C       change gvmin for a more meaningful plot
        GVMIN(1) = -0.1*GVMAX(1)
      END IF
C     scale data, y axis
      CALL SCALIT (ARHLOG,GVMIN(1),GVMAX(1),
     O             PLMN(1),PLMX(1))
      IF (PLMX(1) - PLMN(1) .LT. 1.0E-15) THEN
        PLMX(1) = PLMX(1) + 1.0E-15
      END IF
      GTICS(1)= 10
C     x axis
      PLMN(4) = -3.0
      PLMX(4) = 3.0
      GTICS(4)= 10
      CALL ZIPI(4,1,GBVALF)
C     write(99,*) 'PLMN:',PLMN
C     write(99,*) 'PLMX:',PLMX
C     write(99,*) 'WHICH',WHICH
C     write(99,*) 'GVMIN',GVMIN
C     write(99,*) 'GVMAX',GVMAX
      CALL GPSCLE (PLMN,PLMX,GTICS,GBVALF)
C
C     define axes, x is probability percent
      GXTYP= 6
      IF (IPLT.EQ.8) THEN
C       log scale
        GYTYP(1)= 2
      ELSE
C       arith scale
        GYTYP(1)= 1
      END IF
      GYTYP(2)= 0
      CALL GPLABL (GXTYP,GYTYP,ALEN,YLLAB,YRLAB,CLAB,TITL)
      CALL GPLBXB (XLAB)
C
      XLEN = 5.0
      XPAGE= 10.0
      YLEN = 5.0
      YPAGE= 8.0
      CALL GPXLEN (XLEN,XPAGE,YLEN,YPAGE)
C     set up sizes
      CALL GPSIZE (SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN)
C
C     make the plot
      CALL PSTUPW (WINDOW,
     O             RETCOD)
      CALL PLTONE
      IWAIT= 0
      IF (CMPTYP .NE. 1) THEN
C       dont close workstation
        ICLOS= 0
      ELSE
C       close workstation on pc
        ICLOS= 1
      END IF
      CALL PDNPLT(WINDOW,ICLOS,IWAIT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLTXXX
     I                   (PLTDEV,WINSCR,WINDIM,
     M                    DEVFIL,
     O                    DEVCOD,CMPTYP,WINACT,PLTCNT)
C
C     + + + PURPOSE + + +
C     figure out where plot is going and set things up
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    PLTDEV,WINSCR,DEVFIL,DEVCOD,CMPTYP,WINACT,PLTCNT
      REAL       WINDIM(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLTDEV - plot device
C     WINSCR - current window open on screen
C     WINDIM - window dimension array
C     DEVFIL - device file for plot
C     DEVCOD - device code
C     CMPTYP - computer type
C     WINACT - window where this plot is going
C     PLTCNT - current plot count
C
C     + + + SAVES + + +
      INTEGER    CURCNT
      SAVE       CURCNT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I
      CHARACTER*64 FNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL     ANPRGT,GPDEVC,GETFUN,GPMTFL,GPXLOC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CURCNT/0/
C
C     + + + END SPECIFICATIONS + + +
C
C     what device code
      I= 39+ PLTDEV
      CALL ANPRGT (I,
     O             DEVCOD)
C     set device type and code
      CALL GPDEVC (PLTDEV,DEVCOD)
C     what type computer
      I= 1
      CALL ANPRGT (I,
     O             CMPTYP)
      IF (CMPTYP.EQ.5 .AND. PLTDEV.GE.2) THEN
C       file unit for plotter, laser, or meta file on aviion
        IF (DEVFIL .EQ. 0) THEN
          I= 1
          CALL GETFUN (I,DEVFIL)
        END IF
        CURCNT= CURCNT+ 1
        FNAME = 'gksplt.out'
        IF (CURCNT .LE. 9) THEN
          WRITE(FNAME(11:11),FMT='(I1)') CURCNT
        ELSE IF (CURCNT .LE. 99) THEN
          WRITE(FNAME(11:12),FMT='(I2)') CURCNT
        ELSE
          WRITE(FNAME(11:13),FMT='(I3)') CURCNT
        END IF
        OPEN (UNIT=DEVFIL,FILE=FNAME,ERR=15,IOSTAT=I)
        GOTO 16
 15     CONTINUE
          WRITE(99,*) 'HSXPLT, ERROR OPENING AVIION PRINTER:',I
          DEVFIL= 0
 16     CONTINUE
C       put printer in plot common
        CALL GPMTFL(DEVFIL)
C       dummy window for hard copy
        WINACT= 9
      ELSE
C       window for this plot on screen
        WINACT= WINSCR
C       send window dimensions to graphics routines
        CALL GPXLOC (WINDIM)
      END IF
C
      PLTCNT= CURCNT
C
      RETURN
      END
C
C
C
      SUBROUTINE   LAYPLT
     I                   (NVAR,NPTS,NTYPE,BUFMAX,
     M                    WHICH,YX,DTYPE,
     M                    LNTYP,COLOR,PATTRN,SYMBOL,CLAB)
C
C     + + + PURPOSE + + +
C     arrange plot layers so that observed is plotted last.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NVAR,WHICH(NVAR),NPTS,NTYPE,
     $              DTYPE(NVAR),BUFMAX,
     $              COLOR(NVAR),LNTYP(NVAR),
     $              PATTRN(NVAR),SYMBOL(NVAR)
      REAL          YX(BUFMAX)
      CHARACTER*20  CLAB(NVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NVAR   - number of lines to plot
C     WHICH  - for each line,
C              1 - left y-axis
C              2 - right y-axis
C              3 - auxiliary axis
C     NPTS   - number of points (number of time steps)
C     NTYPE  - number of different line types possible
C     BUFMAX - size of data buffer YX
C     YX     - data to be plotted
C     DTYPE  - data type (1 - mean, 2 - point)
C     CLAB   - array of legends for curves
C     COLOR  - line colors for each line
C     LNTYP  - line types for each line
C     PATTRN - fill pattern for each line
C     SYMBOL - symbol for each line
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,LOC,TWHICH,TDTYPE,
     $              TCOLOR,TLNTYP,TPATTR,TSYMBO
      REAL          TYX(5000)
      CHARACTER*20  TCLAB
C
C     + + + END SPECIFICATIONS + + +
C
C     store last variable values in temp vars
      I = NVAR
      TWHICH = WHICH(I)
      TDTYPE = DTYPE(I)
      TCOLOR = COLOR(I)
      TLNTYP = LNTYP(I)
      TPATTR = PATTRN(I)
      TSYMBO = SYMBOL(I)
      TCLAB  = CLAB(I)
C
      LOC = (NPTS*(NVAR-1))+1
      DO 10 J = 1,NPTS
        TYX(J) = YX(LOC)
        LOC = LOC + 1
 10   CONTINUE
C
C     move first (obs) values to last spot
      WHICH(I)  = WHICH(1)
      DTYPE(I)  = DTYPE(1)
      COLOR(I)  = COLOR(1)
      LNTYP(I)  = LNTYP(1)
      PATTRN(I) = PATTRN(1)
      SYMBOL(I) = SYMBOL(1)
      CLAB(I)   = CLAB(1)
C
      LOC = (NPTS*(NVAR-1))+1
      DO 20 J = 1,NPTS
        YX(LOC) = YX(J)
        LOC = LOC + 1
 20   CONTINUE
C
C     move temp vars to first spot
      WHICH(1)  = TWHICH
      DTYPE(1)  = TDTYPE
      COLOR(1)  = TCOLOR
      LNTYP(1)  = TLNTYP
      PATTRN(1) = TPATTR
      SYMBOL(1) = TSYMBO
      CLAB(1)   = TCLAB
C
      DO 30 J = 1,NPTS
        YX(J) = TYX(J)
 30   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QDXYPT
     I                   (IPLT,ARHLOG,WINDOW,NPTS,NVAR,
     I                    BUFMAX,YX,CLAB,XLAB,YLLAB,YRLAB,TITL,
     I                    DEVTYP,DEVCOD,GSYMBL,GCOLOR,GLNTYP,CMPTYP)
C
C     + + + PURPOSE + + +
C     Call graphics routines to make an x-y plot.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IPLT,ARHLOG(2),WINDOW,NPTS,NVAR,GLNTYP(*),
     1              BUFMAX,DEVTYP,DEVCOD,GSYMBL(*),GCOLOR(*),CMPTYP
      REAL          YX(BUFMAX)
      CHARACTER*20  CLAB(*)
      CHARACTER*80  XLAB,YLLAB,YRLAB
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITION + + +
C     IPLT   - number of plot
C     ARHLOG - type of axis - 1- arith, 2-log, 1-Y,2-X
C     WINDOW - to make plot in
C     NPTS   - number of points in PTS to plot
C     NVAR   - number of variables
C     BUFMAX - size of data array
C     YX     - values to be plotted
C     CLAB   - array of legends for curves
C     XLAB   - X-axis label
C     YLLAB  - left axis label
C     YRLAB  - right axis label
C     TITL   - title of plot
C     DEVTYP - device type
C     DEVCOD - device code
C     CMPTYP - computer type code
C     GSYMBL - array of symbol types
C     GCOLOR - array of colors
C     GLNTYP - array of line types
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,N,IPOS,RETCOD,WHICH(TSMAX),GTICS(4),GBVALF(4),
     1            GPATRN(TSMAX),GTRANF(TSMAX),INDX,ICLOS,IWAIT,NCRV,
     2            GCTYP(TSMAX),GXTYP,GYTYP(2),AXTYP(2)
      REAL        SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN,
     1            GVMIN(TSMAX),GVMAX(TSMAX),PLMN(4),PLMX(4),XMIN,
     2            XMAX,YMIN,YMAX
      CHARACTER*1 OLAB(20,TSMAX),BLNK
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    GPNCRV, GPDATR, GPCURV, GPVAR, SCALIT, GPSCLE, GPLABL
      EXTERNAL    GPSIZE, PSTUPW, PLTONE, PDNPLT, ZIPI, GPLBXB, GPWCXY
      EXTERNAL    ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN
     &     /  0.12, 10.0, 8.0,  2.5,  1.5,  5.0, 5.0, 0.0/
      DATA  BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE (99,*) 'ARHLOG(1-2),NVAR,NPTS,BUFMAX',
     1              ARHLOG,NVAR,NPTS,BUFMAX
C     WRITE (99,*) 'YX1, 1-5,last 5',(YX(I),I=1,5),(YX(I),I=NPTS-4,NPTS)
C     WRITE (99,*) 'YX2, 1-5,last 5',(YX(I),I=NPTS+1,NPTS+5),
C    1                               (YX(I),I=2*NPTS-4,2*NPTS)
C
C     set axes types
      AXTYP(1) = ARHLOG(1)
      AXTYP(2) = ARHLOG(2)
C
C     number of curves and variables
      NCRV = NVAR/2
      CALL GPNCRV(NCRV, NVAR)
      IPOS= 1
      INDX= 1
      DO 10 I = 1,NCRV
C       save the Y data
        CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C       y data transformations
        GTRANF(INDX)= AXTYP(1)
        IPOS= IPOS+ NPTS
        INDX= INDX+ 1
C       save the X data
        CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C       x data trnsformations
        GTRANF(INDX)= AXTYP(2)
C       save where the data went
        CALL GPWCXY (I,INDX-1,INDX)
        IPOS= IPOS+ NPTS
        INDX= INDX+ 1
 10   CONTINUE
C
C     set curve info
      DO 15 I= 1,NCRV
C       xy plot
        GCTYP(I) = 6
C       no pattern
        GPATRN(I)= 0
 15   CONTINUE
      CALL GPCURV (GCTYP,GLNTYP,GSYMBL,GCOLOR,GPATRN,CLAB)
C
C     set data ranges and scaling
      DO 30 I = 1,NCRV
        J = 1 + 2*(I-1)
        WHICH(J)= 1
        WHICH(J+1)= 4
 30   CONTINUE
C
      DO 40 I = 1, NVAR
        GVMIN(I) = 1.0E30
        GVMAX(I) = -1.0E30
 40   CONTINUE
C
      YMAX = -1.0E30
      YMIN = 1.0E30
      XMAX = -1.0E30
      XMIN = 1.0E30
      DO 25 N = 1,NCRV
        DO 20 I= 1,NPTS
          J = 2*NPTS*(N-1) + I
          K = 1 + (N-1)*2
          IF (YX(J).GT.GVMAX(K)) GVMAX(K)= YX(J)
          IF (YX(J).LT.GVMIN(K)) GVMIN(K)= YX(J)
          IF (YX(J) .GT. YMAX) YMAX = YX(J)
          IF (YX(J) .LT. YMIN) YMIN = YX(J)
          J = J + NPTS
          K = K + 1
          IF (YX(J).GT.GVMAX(K)) GVMAX(K)= YX(J)
          IF (YX(J).LT.GVMIN(K)) GVMIN(K)= YX(J)
          IF (YX(J) .GT. XMAX) XMAX = YX(J)
          IF (YX(J) .LT. XMIN) XMIN = YX(J)
 20     CONTINUE
 25   CONTINUE
C
      CALL ZIPC (20*TSMAX,BLNK,OLAB)
C     set up axes scaling
      CALL GPVAR (GVMIN,GVMAX,WHICH,GTRANF,OLAB)
C     scale data, Y then X
      CALL SCALIT (AXTYP(1),YMIN,YMAX,
     O             PLMN(1),PLMX(1))
      IF (PLMX(1) - PLMN(1) .LT. 1.0E-15) PLMX(1) = PLMX(1) + 1.0E-15
      IF (ABS(PLMN(1)).GT.PLMX(1)) THEN
C       only happens when min val is larger negative, adjust max val
        PLMX(1)= ABS(PLMN(1))
      ELSE IF (PLMN(1).LT.0.0) THEN
C       min val is smaller negative, adjust min val
        PLMN(1)= -PLMX(1)
      END IF
      GTICS(1) = 10
      CALL SCALIT (AXTYP(2),XMIN,XMAX,
     O             PLMN(4),PLMX(4))
      IF (PLMX(4) - PLMN(4) .LT. 1.0E-15) THEN
        PLMX(4) = PLMX(4) + 1.0E-15
      END IF
      IF (IPLT.EQ.5) THEN
C       one tic for each month
        GTICS(4)= 13
        PLMX(4) = 13.0
      ELSE
        GTICS(4) = 10
      END IF
      CALL ZIPI(4,0,GBVALF)
C     write(99,*) 'PLMN:',PLMN
C     write(99,*) 'PLMX:',PLMX
C     write(99,*) 'WHICH',WHICH
C     write(99,*) 'YMIN:',YMIN,YMAX
      CALL GPSCLE (PLMN,PLMX,GTICS,GBVALF)
C
C     define axes
      GXTYP= AXTYP(2)
      GYTYP(1)= AXTYP(1)
      GYTYP(2)= 0
      CALL GPLABL (GXTYP,GYTYP,ALEN,YLLAB,YRLAB,CLAB,TITL)
      CALL GPLBXB (XLAB)
C
C     set up sizes
      CALL GPSIZE (SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN)
C
C     make the plot
      CALL PSTUPW (WINDOW,
     O             RETCOD)
      CALL PLTONE
      IWAIT= 0
      IF (CMPTYP .NE. 1) THEN
C       dont close workstation
        ICLOS= 0
      ELSE
C       close workstation on pc
        ICLOS= 1
      END IF
      CALL PDNPLT(WINDOW,ICLOS,IWAIT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QDBRPL
     I                   (WINDOW,ARHLOG,MNLTZO,ARPCSC,NVAR,WHICH,
     I                    NPTS,NTYPE,BUFMAX,YX,DATE,TS,TU,DTYPE,
     I                    TYPIND,LNTYP,COLOR,PATTRN,SYMBOL,
     I                    CLAB,YLLAB,YRLAB,ALAB,TITL,CMPTYP)
C
C     + + + PURPOSE + + +
C     Call routines to make a bar plot comparing time-series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ARHLOG(2),NVAR,WHICH(NVAR),NPTS,BUFMAX,NTYPE,
     $              DATE(6),TS,TU,DTYPE(*),
     $              TYPIND(NTYPE),COLOR(*),LNTYP(*),
     $              PATTRN(*),WINDOW,MNLTZO,SYMBOL(*),CMPTYP
      REAL          ARPCSC,YX(BUFMAX)
      CHARACTER*20  CLAB(NVAR)
      CHARACTER*80  YLLAB,YRLAB,ALAB
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WINDOW - to make plot in
C     ARHLOG - type of axis - 1- arith, 2-log
C     MNLTZO - allow minimum values to be less than zero on plot,
C              0:no, 1:yes, 2:only to -10% of max
C     ARPCSC - percent of arithmatic scale to use
C     NVAR   - number of lines to plot
C     WHICH  - for each line,
C              1 - left y-axis
C              2 - right y-axis
C              3 - auxiliary axis
C     NPTS   - number of points (number of time steps)
C     NTYPE  - number of different line types possible
C     BUFMAX - size of data buffer YX
C     YX     - data to be plotted
C     DATE   - start of event (yr,mo,dy,hr,mn,sec)
C     TS     - time step in TU units
C     TU     - time units (1-sec, 2-min, 3-hour, 3-day....)
C     DTYPE  - data type (1 - mean, 2 - point)
C     CLAB   - array of legends for curves
C     YLLAB  - left axis label
C     YRLAB  - right axis label
C     ALAB   - auxillary axis label
C     TITL   - title of plot
C     COLOR  - line colors for each line
C     LNTYP  - line types for each line
C     PATTRN - fill pattern for each line
C     SYMBOL - symbol for each line
C     CMPTYP - computer type
C     TYPIND - index of which set of specs to use with this data set
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,N,ITS(TSMAX),ITU(TSMAX),RETCOD,GTICS(3),NCRV,LOC,
     1            GCTYP(TSMAX),ICLOS,IWAIT,LFTRT,MINY,MAXY,LNPTS,
     2            GBVALF(4),GTRANF(TSMAX),GXTYP,GYTYP(2),LSYMBL(TSMAX),
     4            EDATE(6),L0,L1,L4,IFLG1,IFLG2,IFLG3,LCOLOR(TSMAX),
     5            LPTTRN(TSMAX),LLNTYP(TSMAX),LDTYPE(TSMAX)
      REAL        GVMIN(TSMAX),GVMAX(TSMAX),PLMN(4),PLMX(4),
     &            SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN,
     &            LMIN,LMAX,AMIN,AMAX,RMIN,RMAX,YMLEN,RMAX1,BOTOM
C
C     + + + EXTERNALS + + +
      EXTERNAL    GPNCRV, GPDATR, GPCURV, GPVAR, GPSCLE, GPLABL
      EXTERNAL    GPSIZE, ZIPI, PSTUPW,  PDNPLT, GPWCTM
      EXTERNAL    TIMADD, GPTIME, SCALIT, GPXLEN, LEGNDC
      EXTERNAL    TMCRVM, MATCHL, AUXAXE, ARAXIS, RHTBDR, TMAXIS, PLTITL
      EXTERNAL    GSCHH, GSTXP, GSPLCI, GSPMCI, GSFACI, GSTXCI, GSLN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SIZEL,XPHYS,YPHYS
     &      / 0.11,1.5,  1.5  /
      DATA   L0,L1,L4/0,1,4/
C
C     + + + END SPECIFICATIONS + + +
C
C     write (99,*) 'ARHLOG,NVAR,NPTS,BUFMAX',ARHLOG,NVAR,NPTS,BUFMAX
C     write (99,*) 'YX1, 1-5,last 5',(YX(I),I=1,5),(YX(I),I=NPTS-4,NPTS)
C     write (99,*) 'YX2, 1-5,last 5',(YX(I),I=NPTS+1,NPTS+5),
C    1                               (YX(I),I=2*NPTS-4,2*NPTS)
C
C     set number of curves and variables
      NCRV = NVAR
      CALL GPNCRV(NCRV,NVAR)
      LOC= 1
C     save the Y data
      DO 15 I = 1,NCRV
        CALL GPDATR (I,LOC,NPTS,YX(LOC),RETCOD)
        IF (WHICH(I).EQ.1 .OR. WHICH(I).EQ.2) THEN
C         left or right axis
          GTRANF(I) = ARHLOG(WHICH(I))
        ELSE
C         auxiliary axis, use same as left
          GTRANF(I) = ARHLOG(1)
        END IF
        CALL GPWCTM( I, I )
        LOC = LOC + NPTS
 15   CONTINUE
C
C     save curve info
      DO 25 I = 1,NCRV
        IF (WHICH(I) .EQ. 3) THEN
          GCTYP(I) = 3
        ELSE
          GCTYP(I) = 1
        END IF
C
        LSYMBL(I) = SYMBOL(TYPIND(I))
        LLNTYP(I) = LNTYP(TYPIND(I))
        LCOLOR(I) = COLOR(TYPIND(I))
        LPTTRN(I) = PATTRN(TYPIND(I))
        IF (LSYMBL(I).GT.0 .AND. LLNTYP(I).EQ.0) THEN
C         single points
          LDTYPE(I)= 2
        ELSE
C         mean values on a line
          LDTYPE(I)= 1
        END IF
 25   CONTINUE
C
C     patterns
      CALL GPCURV (GCTYP,LLNTYP,LSYMBL,LCOLOR,LPTTRN,CLAB)
C     compute and set data ranges
      LMIN =  1.0E30
      LMAX = -1.0E30
      RMIN =  1.0E30
      RMAX = -1.0E30
      AMIN =  1.0E30
      AMAX = -1.0E30
C
      IFLG1 = 0
      IFLG2 = 0
      IFLG3 = 0
C
      DO 75 I = 1,NCRV
        GVMIN(I) = 1.0E30
        GVMAX(I) = -1.0E30
        IF (WHICH(I) .EQ. 1) THEN
C         left y-axis
          DO 35 N= 1,NPTS
            J = (I-1)*NPTS + N
            IF (YX(J) .GT. GVMAX(I)) GVMAX(I) = YX(J)
            IF (YX(J) .LT. GVMIN(I)) GVMIN(I) = YX(J)
            IF (YX(J) .GT. LMAX) LMAX = YX(J)
C
            IF (YX(J) .LT. LMIN) THEN
C             new min
              IF (ARHLOG(1).EQ.2 .AND. YX(J).GE.0.0) THEN
C               ignore values <0 with log scale
C               new min
                LMIN = YX(J)
              ELSE
C               new min
                LMIN = YX(J)
              END IF
            END IF
 35       CONTINUE
          IF (MNLTZO.EQ.0 .AND. LMIN.LT.0.0) THEN
C           cant allow min less than zero
            LMIN = 0.0
          END IF
          IFLG1 = 1
        ELSE IF (WHICH(I) .EQ. 2) THEN
C         right y-axis
          DO 45 N= 1,NPTS
            J = (I-1)*NPTS + N
            IF (YX(J) .GT. GVMAX(I)) GVMAX(I) = YX(J)
            IF (YX(J) .LT. GVMIN(I)) GVMIN(I) = YX(J)
            IF (YX(J) .GT. RMAX) RMAX = YX(J)
C
            IF (YX(J) .LT. RMIN) THEN
C             new min
              IF (ARHLOG(2).EQ.2 .AND. YX(J).GE.0.0) THEN
C               ignore values <0 with log scale
C               new min
                RMIN = YX(J)
              ELSE
C               new min
                RMIN = YX(J)
              END IF
            END IF
 45       CONTINUE
          IF (MNLTZO.EQ.0 .AND. RMIN.LT.0.0) THEN
C           cant allow min less than zero
            RMIN = 0.0
          END IF
          IFLG2 = 1
        ELSE IF (WHICH(I) .EQ. 3) THEN
C         auxiliary axis
          DO 55 N= 1,NPTS
            J = (I-1)*NPTS + N
            IF (YX(J) .GT. GVMAX(I)) GVMAX(I) = YX(J)
            IF (YX(J) .LT. GVMIN(I)) GVMIN(I) = YX(J)
            IF (YX(J) .GT. AMAX) AMAX = YX(J)
            IF (YX(J) .LT. AMIN) AMIN = YX(J)
 55       CONTINUE
          IFLG3 = 1
        END IF
 75   CONTINUE
C
C     make sure mins not so small that they ruin plot scaling
      IF (LMIN.LT.-0.1*LMAX .AND. MNLTZO.EQ.2) THEN
C       change lmin for a more meaningful plot
        LMIN = -0.1*LMAX
      END IF
      IF (RMIN.LT.-0.1*RMAX .AND. MNLTZO.EQ.2) THEN
C       change rmin for a more meaningful plot
        RMIN = -0.1*RMAX
      END IF
      IF (AMIN.LT.-0.1*AMAX .AND. MNLTZO.EQ.2) THEN
C       change amin for a more meaningful plot
        AMIN = -0.1*AMAX
      END IF
C
C     y axis type
      I= 10
      CALL ZIPI (I,ARHLOG(1),GTRANF)
      DO 77 I = 1,NCRV
        IF (WHICH(I).EQ.1 .OR. WHICH(I).EQ.2) THEN
C         left or right axis
          GTRANF(I) = ARHLOG(WHICH(I))
        END IF
 77   CONTINUE
      CALL GPVAR (GVMIN,GVMAX,WHICH,GTRANF,CLAB)
C     WRITE (*,*)'GPVAR: GVMIN,GVMAX,WHICH',GVMIN,GVMAX,WHICH
C
      GTICS(1) = 10
      GTICS(2) = 10
      GTICS(3) = 2
      CALL ZIPI(L4,L0,GBVALF)
C     clip bottom, no connection
      GBVALF(2)= 2
C
C     set up axis scaling
      IF (IFLG1 .EQ. 1) THEN
C       WRITE (*,*) 'ARHLOG,ARPCSC:',ARHLOG,ARPCSC
        CALL SCALIT(ARHLOG(1), LMIN, LMAX, PLMN(1), PLMX(1))
        IF (PLMX(1) - PLMN(1) .LT. 1.0E-15) THEN
C         min and max must be different
          PLMX(1) = PLMX(1) + 1.0E-15
        END IF
        IF (ARHLOG(1) .EQ. 1) THEN
C         adjust arith scale
          PLMX(1)= PLMX(1)* ARPCSC/100.0
        END IF
      END IF
      IF (IFLG2 .EQ. 1) THEN
        CALL SCALIT(ARHLOG(2), RMIN, RMAX, PLMN(2), PLMX(2))
        IF (PLMX(2) - PLMN(2) .LT. 1.0E-15) THEN
C         min and max must be different
          PLMX(2) = PLMX(2) + 1.0E-15
        END IF
        IF (ARHLOG(2) .EQ. 1) THEN
C         adjust arith scale
          PLMX(2)= PLMX(2)* ARPCSC/100.0
        END IF
      END IF
      IF (IFLG3 .EQ. 1) THEN
        CALL SCALIT(L1, AMIN, AMAX, PLMN(3), PLMX(3))
        IF (PLMX(3) - PLMN(3) .LT. 1.0E-15) THEN
C         min and max must be different
          PLMX(3) = PLMX(3) + 1.0E-15
        END IF
      END IF
C     WRITE (*,*)'SCALIT: IFLG1,LMIN,LMAX,PLMN1,PLMX1',
C    1                       IFLG1,LMIN,LMAX,PLMN(1),PLMX(1)
C     WRITE (*,*)'SCALIT: IFLG2,LMIN,LMAX,PLMN2,PLMX2',
C    1                       IFLG2,LMIN,LMAX,PLMN(2),PLMX(2)
C     WRITE (*,*)'SCALIT: IFLG3,LMIN,LMAX,PLMN3,PLMX3',
C    1                       IFLG3,LMIN,LMAX,PLMN(3),PLMX(3)
C
      CALL GPSCLE(PLMN,PLMX,GTICS,GBVALF)
C     WRITE (*,*)'GPSCLE: GTICS,GBVALF',GTICS,GBVALF
C
C     define axes
      GXTYP= 0
      GYTYP(1)= 0
      GYTYP(2)= 0
C     assume no aux axis
      ALEN = 0.0
C
      DO 80 I = 1, NCRV
        IF (WHICH(I) .EQ. 1) THEN
          GYTYP(1) = ARHLOG(1)
        ELSE IF (WHICH(I) .EQ. 2) THEN
          GYTYP(2) = ARHLOG(2)
        ELSE IF (WHICH(I) .EQ. 3) THEN
          ALEN = 1.0
        END IF
 80   CONTINUE
C
C     WRITE (*,*) 'GPLABL:GXTYP,GYTYP,ALEN',GXTYP,GYTYP,ALEN
      CALL GPLABL (GXTYP,GYTYP,ALEN,YLLAB,YRLAB,ALAB,TITL)
C     set up times
C     WRITE (*,*) 'TIMADD'
      CALL TIMADD (DATE,TU,TS,NPTS,EDATE)
C     assume all are mean or total
      CALL ZIPI(NCRV, TS, ITS)
      CALL ZIPI(NCRV, TU, ITU)
C     WRITE (*,*) 'GPTIME',NCRV
      CALL GPTIME (ITS,ITU,DATE,EDATE,LDTYPE)
      XLEN = 7.5
      XPAGE= 10.0
      YLEN = 5.0
      YPAGE= 8.0
      CALL GPXLEN (XLEN,XPAGE,YLEN,YPAGE)
C     set up sizes
C     WRITE (*,*) 'GPSIZE',NCRV
      CALL GPSIZE (SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN)
C     set up for plot
C     WRITE (*,*) 'PSTUPW'
      CALL PSTUPW ( WINDOW, RETCOD )
C     make the plot
CPRH      CALL PLTONE
C     replace call to PLTONE with call made by PLTONE
C     so that bars and axis are displayed properly
      CALL GSCHH (SIZEL)
      CALL GSTXP (L0)
      J = 1
      DO 100 I= 1,NCRV
C       plot each data set
        CALL GSPLCI (LCOLOR(I))
        CALL GSPMCI (LCOLOR(I))
        CALL GSFACI (LCOLOR(I))
        CALL GSTXCI (LCOLOR(I))
        CALL GSLN (LLNTYP(I))
        CALL TMCRVM (I)
        IF (NCRV .GT. 2) THEN
C         draw legend
          CALL LEGNDC (I,J)
        END IF
 100  CONTINUE
C
C     adjust time number of time intervals so that axes are done properly
      LNPTS= NPTS/NCRV
      CALL TIMADD (DATE,TU,TS,LNPTS,EDATE)
C     assume all are mean or total
      CALL ZIPI(NCRV, TS, ITS)
      CALL ZIPI(NCRV, TU, ITU)
C     WRITE (*,*) 'GPTIME',NCRV
      CALL GPTIME (ITS,ITU,DATE,EDATE,LDTYPE)
C     draw axes
      IF (ALEN.GT.0.0) THEN
C       draw auxilliary axis
        CALL MATCHL (PLMX(3),SIZEL,PLMN(1),PLMX(1),GYTYP(1),GTICS(1),
     O               RMAX1)
C       adjust length of main Y-axis
        YMLEN= YLEN
C       label aux axis
        CALL AUXAXE (XLEN,YMLEN,ALEN,SIZEL,ALAB,GTICS(3),
     I               PLMN(3),PLMX(3),RMAX1)
      ELSE
C       no aux axis
        YMLEN= YLEN
        RMAX1= 999.0
      END IF
C     draw left axis
      LFTRT= 1
      CALL ARAXIS (XLEN,YMLEN,YLLAB,SIZEL,PLMN(1),PLMX(1),LFTRT,
     I             RMAX1,GTICS)
C     draw right axis
      CALL RHTBDR (GYTYP(1),GTICS(1),XLEN,YMLEN,SIZEL,MINY,MAXY)
C     draw X-axis
      CALL TMAXIS (XLEN,YMLEN,ALEN,SIZEL,NCRV,ITS,ITU,DATE,EDATE,LDTYPE,
     O             BOTOM)
C     display title
      CALL PLTITL (TITL,SIZEL,XLEN,BOTOM)
C     exit from plot, dont wait
      IWAIT= 0
      IF (CMPTYP .NE. 1) THEN
C       dont close workstation
        ICLOS= 0
      ELSE
C       close workstation on pc
        ICLOS= 1
      END IF
      CALL PDNPLT ( WINDOW, ICLOS, IWAIT )
C
      RETURN
      END
C
C
C
      SUBROUTINE   QDXYRG
     I                   (IPLT,ARHLOG,WINDOW,NPTS,NVAR,
     I                    BUFMAX,YX,CLAB,XLAB,YLLAB,YRLAB,TITL,
     I                    DEVTYP,DEVCOD,GSYMBL,GCOLOR,CMPTYP,
     I                    ACOEF,BCOEF,RSQUAR)
C
C     + + + PURPOSE + + +
C     Call graphics routines to make an x-y plot with regression line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IPLT,ARHLOG(2),WINDOW,NPTS,NVAR,
     1              BUFMAX,DEVTYP,DEVCOD,GSYMBL(*),GCOLOR(*),CMPTYP
      REAL          YX(BUFMAX),ACOEF,BCOEF,RSQUAR
      CHARACTER*20  CLAB(*)
      CHARACTER*80  XLAB,YLLAB,YRLAB
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITION + + +
C     IPLT   - number of plot
C     ARHLOG - type of axis - 1- arith, 2-log, 1-Y,2-X
C     WINDOW - to make plot in
C     NPTS   - number of points in PTS to plot
C     NVAR   - number of variables
C     BUFMAX - size of data array
C     YX     - values to be plotted
C     CLAB   - array of legends for curves
C     XLAB   - X-axis label
C     YLLAB  - left axis label
C     YRLAB  - right axis label
C     TITL   - title of plot
C     DEVTYP - device type
C     DEVCOD - device code
C     CMPTYP - computer type code
C     ACOEF  - 'a' coefficient in regression line (y=ax+b)
C     BCOEF  - 'b' coefficient in regression line (y=ax+b)
C     RSQUAR - the coef of determination from regression analysis
C     GSYMBL - array of symbol types
C     GCOLOR - array of colors
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,N,IPOS,RETCOD,WHICH(TSMAX),GTICS(4),GBVALF(4),
     1            GPATRN(TSMAX),GTRANF(TSMAX),INDX,ICLOS,IWAIT,TMP1,
     2            GCTYP(TSMAX),GXTYP,GYTYP(2),AXTYP(2),GLNTYP(TSMAX),
     3            CPR,NCHR,TMPSYM(TSMAX),TMP2,NCRV,TMPCOL(TSMAX)
      REAL        SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN,
     1            GVMIN(TSMAX),GVMAX(TSMAX),PLMN(4),PLMX(4),XMIN,XMAX,
     2            YMIN,YMAX,XP,YP,CORCOF
      CHARACTER*1 OLAB(20,TSMAX),CTXT1(120),BLNK
      CHARACTER*80 CTXT
C
C     + + + FUNCTIONS + + +
      INTEGER     ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS,SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL    GPNCRV, GPDATR, GPCURV, GPVAR, SCALIT, GPSCLE, GPLABL
      EXTERNAL    GPSIZE, PSTUPW, PLTONE, PDNPLT, ZIPI, GPLBXB, GPWCXY
      EXTERNAL    ZLNTXT, CVARAR, ADDTXT, ZIPC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('Y =',F10.3,' X +',F10.3,'CORR COEF =',F10.5)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN
     &     /  0.12, 10.0, 8.0,  2.5,  1.5,  5.0, 5.0, 0.0/
      DATA  BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE (99,*) 'ARHLOG(1-2),NVAR,NPTS,BUFMAX',
     1              ARHLOG,NVAR,NPTS,BUFMAX
C     WRITE (99,*) 'YX1, 1-5,last 5',(YX(I),I=1,5),(YX(I),I=NPTS-4,NPTS)
C     WRITE (99,*) 'YX2, 1-5,last 5',(YX(I),I=NPTS+1,NPTS+5),
C    1                               (YX(I),I=2*NPTS-4,2*NPTS)
C
C     set axes types
      AXTYP(1) = ARHLOG(1)
      AXTYP(2) = ARHLOG(2)
C
C     number of curves and variables
      NCRV = NVAR/2
C     need to add curves for 45 degree line and regression line
      TMP1 = NCRV + 2
      TMP2 = NVAR + 4
      CALL GPNCRV(TMP1, TMP2)
      IPOS= 1
      INDX= 1
      DO 10 I = 1,NCRV
C       save the Y data
        CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C       y data transformations
        GTRANF(INDX)= AXTYP(1)
        IPOS= IPOS+ NPTS
        INDX= INDX+ 1
C       save the X data
        CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C       x data trnsformations
        GTRANF(INDX)= AXTYP(2)
C       save where the data went
        CALL GPWCXY (I,INDX-1,INDX)
        IPOS= IPOS+ NPTS
        INDX= INDX+ 1
 10   CONTINUE
C
C     set curve info
      DO 15 I= 1,NCRV
C       xy plot
        GCTYP(I) = 6
C       no lines
        GLNTYP(I) = 0
C       no pattern
        GPATRN(I) = 0
 15   CONTINUE
C     set curve info for 45 degree line and regression line
      TMP1 = NCRV + 1
      GCTYP(TMP1) = 6
      GLNTYP(TMP1)= 1
      GPATRN(TMP1)= 0
      TMP2 = NCRV + 2
      GCTYP(TMP2) = 6
      GLNTYP(TMP2)= 1
      GPATRN(TMP2)= 0
C     set colors for lines
      TMPCOL(1)   = GCOLOR(1)
      TMPCOL(TMP1)= 1
      TMPCOL(TMP2)= GCOLOR(1)
      TMPSYM(1)   = GSYMBL(1)
      TMPSYM(TMP1)= 0
      TMPSYM(TMP2)= 0
      CALL GPCURV (GCTYP,GLNTYP,TMPSYM,TMPCOL,GPATRN,CLAB)
C
C     set data ranges and scaling
      DO 30 I = 1,NCRV+4
        WHICH(I)= 1
 30   CONTINUE
C
      DO 40 I = 1, NVAR
        GVMIN(I) = 1.0E30
        GVMAX(I) = -1.0E30
 40   CONTINUE
C
      YMAX = -1.0E30
      YMIN = 1.0E30
      XMAX = -1.0E30
      XMIN = 1.0E30
      DO 25 N = 1,NCRV+2
        DO 20 I= 1,NPTS
          J = NPTS*(N-1) + I
          K = 1 + (N-1)*2
          IF (YX(J).GT.GVMAX(K)) GVMAX(K)= YX(J)
          IF (YX(J).LT.GVMIN(K)) GVMIN(K)= YX(J)
          IF (YX(J) .GT. YMAX) YMAX = YX(J)
          IF (YX(J) .LT. YMIN) YMIN = YX(J)
          J = J + NPTS
          K = K + 1
          IF (YX(J).GT.GVMAX(K)) GVMAX(K)= YX(J)
          IF (YX(J).LT.GVMIN(K)) GVMIN(K)= YX(J)
          IF (YX(J) .GT. XMAX) XMAX = YX(J)
          IF (YX(J) .LT. XMIN) XMIN = YX(J)
 20     CONTINUE
 25   CONTINUE
C
C     set ymax and xmax to be the same
      IF (YMAX.GT.XMAX) THEN
        XMAX = YMAX
      ELSE
        YMAX = XMAX
      END IF
C
      CALL ZIPC (20*TSMAX,BLNK,OLAB)
C     set up axes scaling
      CALL GPVAR (GVMIN,GVMAX,WHICH,GTRANF,OLAB)
C     scale data, Y then X
      CALL SCALIT (AXTYP(1),YMIN,YMAX,
     O             PLMN(1),PLMX(1))
      IF (PLMX(1) - PLMN(1) .LT. 1.0E-15) PLMX(1) = PLMX(1) + 1.0E-15
      IF (ABS(PLMN(1)).GT.PLMX(1)) THEN
        PLMX(1)= ABS(PLMN(1))
      ELSE IF (PLMX(1).GT.ABS(PLMN(1)).AND.ABS(PLMN(1)).GT.1.0E-5)THEN
        PLMN(1)= -PLMX(1)
      END IF
      GTICS(1) = 10
      CALL SCALIT (AXTYP(2),XMIN,XMAX,
     O             PLMN(4),PLMX(4))
      IF (PLMX(4) - PLMN(4) .LT. 1.0E-15) THEN
        PLMX(4) = PLMX(4) + 1.0E-15
      END IF
      IF (IPLT.EQ.5) THEN
C       one tic for each month
        GTICS(4)= 13
        PLMX(4) = 13.0
      ELSE
        GTICS(4) = 10
      END IF
      CALL ZIPI(4,0,GBVALF)
C     write(99,*) 'PLMN:',PLMN
C     write(99,*) 'PLMX:',PLMX
C     write(99,*) 'WHICH',WHICH
C     write(99,*) 'YMIN:',YMIN,YMAX
      CALL GPSCLE (PLMN,PLMX,GTICS,GBVALF)
C
C     after finding min/max, can fill in points on 45 degree line
C     and points on regression line
      NPTS = 2
      YX(IPOS)   = 0.0
      YX(IPOS+1) = PLMX(1)
      YX(IPOS+2) = 0.0
      YX(IPOS+3) = PLMX(4)
C     save the Y data
      CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C     y data transformations
      GTRANF(INDX)= AXTYP(1)
      IPOS= IPOS+ NPTS
      INDX= INDX+ 1
C     save the X data
      CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C     x data trnsformations
      GTRANF(INDX)= AXTYP(2)
C     save where the data went
      CALL GPWCXY (NCRV+1,INDX-1,INDX)
      IPOS= IPOS+ NPTS
      INDX= INDX+ 1
C     now for regression line
      YX(IPOS)   = (GVMIN(2)*ACOEF)+BCOEF
      YX(IPOS+1) = (GVMAX(2)*ACOEF)+BCOEF
      YX(IPOS+2) = GVMIN(2)
      YX(IPOS+3) = GVMAX(2)
C     save the Y data
      CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C     y data transformations
      GTRANF(INDX)= AXTYP(1)
      IPOS= IPOS+ NPTS
      INDX= INDX+ 1
C     save the X data
      CALL GPDATR (INDX,IPOS,NPTS,YX(IPOS),RETCOD)
C     x data trnsformations
      GTRANF(INDX)= AXTYP(2)
C     save where the data went
      CALL GPWCXY (NCRV+2,INDX-1,INDX)
C
C     define axes
      GXTYP= AXTYP(2)
      GYTYP(1)= AXTYP(1)
      GYTYP(2)= 0
      CALL GPLABL (GXTYP,GYTYP,ALEN,YLLAB,YRLAB,CLAB,TITL)
      CALL GPLBXB (XLAB)
C
C     set up sizes
      CALL GPSIZE (SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN,ALEN)
C
C     make the plot
      CALL PSTUPW (WINDOW,
     O             RETCOD)
      CALL PLTONE
C     add text to plot with equation of line
      IF (RSQUAR.LT.0.0) THEN
C       compute correlation coef from coef of determination,
C       special case if negative number
        CORCOF = -1.0*(SQRT(-1.0*RSQUAR))
      ELSE
        CORCOF= SQRT(RSQUAR)
      END IF
      IF (ACOEF.LT.0.0) THEN
C       neg slope, correlation coef should also be negative
        CORCOF= -1.0*CORCOF
      END IF
      CPR  = 27
      WRITE (CTXT,2000) ACOEF,BCOEF,CORCOF
      NCHR = ZLNTXT(CTXT)
      CALL CVARAR (NCHR,CTXT,NCHR,CTXT1)
      XP = 0.5
      YP = 4.0
      CALL ADDTXT (XP,YP,SIZEL,NCHR,CPR,CTXT1)
C
      IWAIT= 0
      IF (CMPTYP .NE. 1) THEN
C       dont close workstation
        ICLOS= 0
      ELSE
C       close workstation on pc
        ICLOS= 1
      END IF
      CALL PDNPLT(WINDOW,ICLOS,IWAIT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QTFRPL
     I                   (NDSN,ARHLOG,WINDOW,ILH,CMPTYP,
     I                    GLB,CY,CX,GTITL,LCOLOR,
     I                    NZI,CCPA,C,SZ,SE,X,N)
C
C     + + + PURPOSE + + +
C     This routine draws a frequency plot for specified data sets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NDSN,ARHLOG,CMPTYP,WINDOW,ILH(NDSN),NZI(NDSN),
     1             N(NDSN),LCOLOR(NDSN)
      CHARACTER*1  GLB(20,2,NDSN),CY(80),CX(80),GTITL(240)
      REAL         C(27,NDSN),CCPA(27,NDSN),SZ(120,NDSN),SE(27),
     1             X(120,NDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDSN   - number of dsns to plot
C     ARHLOG - log / arith axis flag
C     WINDOW - window to draw plot in
C     ILH    - low or high flag
C     GLB    - label character strings
C     CMPTYP - computer type
C     CY     - y label
C     CX     - x label
C     GTITL  - graph title
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I80,IWAIT,ICLOS,I,RETCOD,IPOS,LEN,NP,NZP,
     1             LL(2),WHICH(2*TSMAX),GCV(TSMAX),GLN(TSMAX),
     2             GSY(TSMAX),GPT(TSMAX),GTRAN(2*TSMAX),GCL(TSMAX),
     3             GTICS(4),GBVALF(4),IIMAX,IIMIN,IDSN,J,K,LX,IMIN,IMAX
      REAL         TMP(120),Z,VMIN(2*TSMAX),VMAX(2*TSMAX),GLOC(2),R0,
     1             GSIZEL,GXPAGE,GYPAGE,GXPHYS,GYPHYS,GXLEN,GYLEN,
     2             GPLMN(4),GPLMX(4),XMIN,XMAX,YMIN,YMAX
      CHARACTER*1  CDUM(80),BLNK
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + FUNCTIONS + + +
      REAL        GAUSEX, SRMIN, SRMAX
C
C     + + + EXTERNALS + + +
      EXTERNAL    PSTUPW, PDNPLT, PLTONE
      EXTERNAL    GAUSEX, SRMIN, SRMAX, GPNCRV, GPDATR, GPWCXY
      EXTERNAL    GPLEDG, GPLABL, ZIPC, GPLBXB
      EXTERNAL    GPCURV, GPVAR, GPSIZE, SCALIT, GPSCLE
C
C     + + + DATA INITIALIZATION + + +
      DATA  I80, BLNK, R0
     #    /  80,  ' ',0.0/
      DATA GBVALF,GTICS/4*1,4*10/
      DATA GSIZEL, GXPAGE, GYPAGE, GXPHYS, GYPHYS, GXLEN, GYLEN
     1    /  0.11,   10.0,    8.0,    1.5,    1.5,   7.5,   5.0/
C
C     + + + END SPECIFICATIONS + + +
C
      YMIN = 1.0E10
      YMAX = -1.0E10
      IDSN = 0
      IPOS = 1
      LEN  = 0
 100  CONTINUE
C       return here for each data set
        IDSN = IDSN + 1
C
        NP = N(IDSN)
        IF (NP .GT. 120) NP = 120
        NZP = NZI(IDSN)
        IF (NZP .GT. 120) NZP = 120
C
C       number of curves and variables
        I = 2*IDSN
        J = 4*IDSN
        CALL GPNCRV (I,J)
C       data to plot
        DO 2 I = 1,NP
          TMP(I) = GAUSEX(SZ(I,IDSN))
 2      CONTINUE
        Z = ABS(TMP(1))
        IF (Z .LT. ABS(TMP(NP))) Z = ABS(TMP(NP))
        J = (4*(IDSN-1)) + 1
        VMIN(J) = -Z
        VMAX(J) = Z
        WHICH(J)= 4
        IPOS = IPOS + LEN
        CALL GPDATR (J,IPOS,NP,TMP,RETCOD)
C
        DO 4 I = 1,NP
          IF (ARHLOG .EQ. 2) THEN
            TMP(I) = 10.0**X(I,IDSN)
          ELSE
            TMP(I) = X(I,IDSN)
          END IF
 4      CONTINUE
        J = (4*(IDSN-1)) + 2
        VMIN(J) = SRMIN(NP,TMP)
        VMAX(J) = SRMAX(NP,TMP)
        WHICH(J)= 1
        IPOS = IPOS + NP
        CALL GPDATR (J,IPOS,NP,TMP,RETCOD)
C
        DO 6 I = 1,27
          TMP(I) = GAUSEX(SE(I))
 6      CONTINUE
        J = (4*(IDSN-1)) + 3
        IF (ILH(IDSN).EQ.1) THEN
C         for high flows
          DO 7 I = 1,27
            IF (TMP(I) .LT. -Z) IMIN = I
            IF (TMP(I) .LT. Z)  IMAX = I
 7        CONTINUE
          IMIN = IMIN + 1
          VMIN(J) = TMP(IMIN)
          VMAX(J) = TMP(IMAX)
        ELSE
C         for low flows
          DO 8 I = 1,27
            IF (TMP(I) .GT. -Z) IMAX = I
            IF (TMP(I) .GT. Z)  IMIN = I
 8        CONTINUE
          IMIN = IMIN + 1
          VMIN(J) = TMP(IMAX)
          VMAX(J) = TMP(IMIN)
        END IF
        LEN = ABS(IMAX-IMIN) + 1
        WHICH(J) = 4
C       drop any zero flows if analysis with logs
        IF (ARHLOG .EQ. 2) THEN
C         values in C are logs when ARHLOG = 2
          IIMIN = IMIN
          IIMAX = IMAX
          IF (NZI(IDSN).GT.0) THEN
            DO 11 I = IMIN, IMAX
              IF (CCPA(I,IDSN) .LT. -10.0) IIMIN = I+1
 11         CONTINUE
          ELSE
            DO 12 I = IMIN, IMAX
              IF (C(I,IDSN) .LT. -10.0) IIMIN = I+1
 12         CONTINUE
          END IF
C         reset IMIN and LEN
          LEN = ABS(IIMAX - IIMIN) + 1
          IMIN = IIMIN
          IMAX = IIMAX
        END IF
        IPOS = IPOS + NP
        CALL GPDATR (J,IPOS,LEN,TMP(IMIN),RETCOD)
C
        IF (NZI(IDSN).GT.0) THEN
          DO 13 I = IMIN,IMAX
            IF (ARHLOG .EQ. 2) THEN
              TMP(I) = 10.0**CCPA(I,IDSN)
            ELSE
              TMP(I) = CCPA(I,IDSN)
            END IF
 13       CONTINUE
        ELSE
          DO 9 I = IMIN,IMAX
            IF (ARHLOG .EQ. 2) THEN
              TMP(I) = 10.0**C(I,IDSN)
            ELSE
              TMP(I) = C(I,IDSN)
            END IF
 9        CONTINUE
        END IF
        J = (4*(IDSN-1)) + 4
        VMIN(J) = SRMIN(LEN,TMP(IMIN))
        VMAX(J) = SRMAX(LEN,TMP(IMIN))
        WHICH(J)= 1
        IPOS = IPOS + LEN
        CALL GPDATR (J,IPOS,LEN,TMP(IMIN),RETCOD)
C
C       set which variable for each curve
        J = (2*(IDSN-1)) + 1
        I = (4*(IDSN-1)) + 1
        K = (4*(IDSN-1)) + 2
        CALL GPWCXY (J,K,I)
        J = (2*(IDSN-1)) + 2
        I = (4*(IDSN-1)) + 3
        K = (4*(IDSN-1)) + 4
        CALL GPWCXY (J,K,I)
C
C       select log or arith for left y-axis
C       if ARHLOG = 1, LL = 1,   if ARHLOG = 2, LL = 2
        LL(1)= ARHLOG
        LL(2)= 0
        LX   = 3
C
        I = (4*(IDSN-1)) + 1
        J = (4*(IDSN-1)) + 3
        IF (VMIN(I).LT.XMIN) THEN
C         new minimum
          XMIN = VMIN(I)
        END IF
        IF (VMIN(J).LT.XMIN) THEN
C         new minimum
          XMIN = VMIN(J)
        END IF
        IF (VMAX(I).GT.XMAX) THEN
C         new maximum
          XMAX = VMAX(I)
        END IF
        IF (VMAX(J).GT.XMAX) THEN
C         new maximum
          XMAX = VMAX(J)
        END IF
        I = (4*(IDSN-1)) + 2
        J = (4*(IDSN-1)) + 4
        IF (VMIN(I).LT.YMIN) THEN
C         new minimum
          YMIN = VMIN(I)
        END IF
        IF (VMIN(J).LT.YMIN) THEN
C         new minimum
          YMIN = VMIN(J)
        END IF
        IF (VMAX(I).GT.YMAX) THEN
C         new maximum
          YMAX = VMAX(I)
        END IF
        IF (VMAX(J).GT.YMAX) THEN
C         new maximum
          YMAX = VMAX(J)
        END IF
C
C       set transformations flags
        J = (4*(IDSN-1)) + 1
        GTRAN(J) = 1
        J = (4*(IDSN-1)) + 2
        GTRAN(J) = LL(1)
        J = (4*(IDSN-1)) + 3
        GTRAN(J) = 1
        J = (4*(IDSN-1)) + 4
        GTRAN(J) = LL(1)
C
C       set curve specs
        J = (2*(IDSN-1)) + 1
        GCV(J) = 6
        GLN(J) = 0
        GSY(J) = 4
        GCL(J) = LCOLOR(IDSN)
        GPT(J) = 0
        J = (2*(IDSN-1)) + 2
        GCV(J) = 6
        GLN(J) = 1
        GSY(J) = 0
        GCL(J) = LCOLOR(IDSN)
        GPT(J) = 0
C
C       go back if more curves to do
      IF (IDSN.LT.NDSN) GO TO 100
C
C     labels and axis type
      CALL ZIPC (I80,BLNK,CDUM)
      CALL GPLABL (LX,LL,R0,CY,CX,CDUM,GTITL)
C     also x-axis label
      CALL GPLBXB (CX)
C
C     determine default x-axis scale based on min/max values
      CALL SCALIT (LX,XMIN,XMAX,GPLMN(4),GPLMX(4))
C     determine default y-axis scale based on min/max values
      CALL SCALIT (LL(1),YMIN,YMAX,GPLMN(1),GPLMX(1))
C     set scale for axes
      GPLMN(2)= 0.0
      GPLMN(3)= 0.0
      GPLMX(2)= 0.0
      GPLMX(3)= 0.0
      CALL GPSCLE (GPLMN,GPLMX,GTICS,GBVALF)
C
C     location of legend
      IF (ILH(1).EQ.1) THEN
        GLOC(1) = 0.05
      ELSE
        GLOC(1) = 0.5
      END IF
      GLOC(2) = 0.9
      CALL GPLEDG (GLOC)
C
      CALL GPVAR (VMIN,VMAX,WHICH,GTRAN,GLB)
      CALL GPCURV (GCV,GLN,GSY,GCL,GPT,GLB)
C
C     set plot sizes
      CALL GPSIZE (GSIZEL,GXPAGE,GYPAGE,GXPHYS,
     I             GYPHYS,GXLEN,GYLEN,R0)

C     generate plot
      IWAIT = 0
      ICLOS = 0
      CALL PSTUPW (WINDOW, RETCOD)
      CALL PLTONE
      IWAIT= 0
      IF (CMPTYP .NE. 1) THEN
C       dont close workstation
        ICLOS= 0
      ELSE
C       close workstation on pc
        ICLOS= 1
      END IF
      CALL PDNPLT (WINDOW,ICLOS,IWAIT)
C
      RETURN
      END

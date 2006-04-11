C
C
C
      SUBROUTINE   GPINIT
C
C     + + + PURPOSE + + +
C     This routine initializes the graphics common blocks and
C     must be called at the beginning of a plotting session.
C
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES ++ +
      INTEGER    I, L0, M1, L80, L240, L6, L1, L4, L10, L120
      REAL       R0, SMALL, LARGE, Z
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPC, ZIPI, ZIPR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/,  L0/0/,  L80/80/,  L240/240/,  M1/-1/,  R0/0.0/
      DATA L1/1/,  L4/4/,  L6/6/,  L10/10/,  L120/120/
      DATA SMALL/-1.0E20/,  LARGE/1.0E20/
C
C     + + + END SPECIFICATIONS + + +
C
      NCRV = 0
      NVAR = 0
      FRPOS = 1
      CALL ZIPI (L4,L10,TICS)
      TICS(3) = 2
      CALL ZIPC(L240,BLNK,TITL)
      CALL ZIPC(L80,BLNK,YLABL)
      CALL ZIPC(L80,BLNK,YALABL)
      CALL ZIPC(L80,BLNK,YXLABL)
      CALL ZIPC (L80,BLNK,XLABL)
C
      I = TSMAX*20
      CALL ZIPC(I,BLNK,LBC)
      CALL ZIPC(I,BLNK,LBV)
C
      XTYPE = M1
      YTYPE(1) = M1
      YTYPE(2) = M1
C
      CALL ZIPI(TSMAX,L0,TSTEP)
      CALL ZIPI(TSMAX,L0,TUNITS)
      CALL ZIPI (L6,L0,SDATIM)
      CALL ZIPI(L6,L0,EDATIM)
      CALL ZIPI(TSMAX,L1,DTYPE)
C
      XPAGE = 8.0
      YPAGE = 6.4
      XLEN = 4.4
      YLEN = 4.2
      ALEN = 0.0
      XPHYS = 1.5
      YPHYS = 1.5
      SIZEL = 0.1
C
      XWINLC(1) = 0.0
      XWINLC(2) = 0.0
      XWINLC(3) = 0.5
      XWINLC(4) = 0.5
C
      LOCLGD(1) = 0.05
      LOCLGD(2) = 0.95
      NCHR = L0
      CPR = 0
      FXT = 0.1
      FYT = 0.9
      CALL ZIPC (L120,BLNK,CTXT)
C
      CALL ZIPR(L4,SMALL,PLMX)
      CALL ZIPR(L4,LARGE,PLMN)
      CALL ZIPR (TSMAX,SMALL,YMAX)
      CALL ZIPR (TSMAX,LARGE,YMIN)
      I = 2*TSMAX
      CALL ZIPI(I,L0,BUFPOS)
C
      CALL ZIPI(TSMAX,L0,SYMBL)
      Z = 0.7*SIZEL
      CALL ZIPR(TSMAX,Z,SYMSIZ)
      CALL ZIPI(TSMAX,L1,LNTYP)
      CALL ZIPI(TSMAX,L0,PATTRN)
      CALL ZIPI(TSMAX,L1,COLOR)
      LBCOLR = L1
      CALL ZIPI(TSMAX,L1,CTYPE)
      CALL ZIPI (TSMAX,L0,WHICH)
      I = 3*TSMAX
      CALL ZIPI (I,L0,WCHVR)
      CALL ZIPI(TSMAX,L1,TRANSF)
      CALL ZIPI(L4,L1,BVALFG)
      CALL ZIPR(L4,R0,BLNKIT)
C
      DEVCOD = 1
      DEVTYP = 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPZIPB
C
C     + + + PURPOSE + + +
C     This routine will reinitialize the data buffer pointers
C     (BUFPOS) for each variable to zero and the free position
C     pointer (FRPOS) to 1.  This routine is used in conjunction
C     with GRDATA.
C
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I0
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      FRPOS = 1
      I = 2*TSMAX
      I0 = 0
      CALL ZIPI (I,I0,BUFPOS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPDATR
     I                    (INUM, IPOS, NV, ARRA,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     This routine places data in the YX array of the plotting common
C     block.  IPOS + NV must not exceed the size of the array, set to
C     6000 on most systems.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM, IPOS, RETCOD, NV
      REAL      ARRA(NV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - index number of the variable (1-TSMAX)
C     IPOS   - starting position in the YX for the data
C     NV     - number of values
C     ARRA   - data points to be plotted
C     RETCOD - return code  0-if ok,  1-not enough space in array
C
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,K
C
C     + + + END SPECIFICATIONS + + +
C
      BUFPOS(1,INUM) = IPOS
      BUFPOS(2,INUM) = IPOS - 1 + NV
C
      IF (IPOS+NV-1 .GT. BUFMAX) THEN
C       not enough space
        RETCOD = 1
      ELSE
C       ok
        DO 10 I = 1,NV
          K = I - 1 + IPOS
          YX(K) = ARRA(I)
 10     CONTINUE
        RETCOD = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPDATA
     I                    (INUM, NV, ARRA,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     This routine places data in the YX array of the plotting common
C     block.  The sum of all the NV values for each call must not
C     exceed the sie of the array, set to 6000 on most systems.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INUM, RETCOD, NV
      REAL      ARRA(NV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INUM   - index number of the variable (1-TSMAX)
C     NV     - number of values
C     ARRA   - data points to be ploted
C     RETCOD - return code  0-if ok,  1-if not enough space in array.
C
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,K
C
C     + + + END SPECIFICATIONS + + +
C
      BUFPOS(1,INUM) = FRPOS
      BUFPOS(2,INUM) = FRPOS - 1 + NV
C
      IF (FRPOS+NV-1 .GT. BUFMAX) THEN
C       not enough space
        RETCOD = 1
      ELSE
C       ok
        DO 10 I = 1,NV
          K = I - 1 + FRPOS
          YX(K) = ARRA(I)
 10     CONTINUE
        RETCOD = 0
      END IF
C
C     set new position for free space
      FRPOS = FRPOS + NV
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLABL
     I                    (GXTYP,GYTYP,GALEN,GYLABL,GYXLBL,
     I                     GYALBL,GTITL)
C
C     + + + PURPOSE + + +
C     This routine puts into a common area the x-axis and y-axes type,
C     and the auxiliary axis length.  The labels for the left y-axis,
C     right y-axis, and auxiliary axis and the plot title are also put
C     into the common area.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GXTYP, GYTYP(2)
      REAL    GALEN
      CHARACTER*1 GTITL(240),GYLABL(80),GYXLBL(80),GYALBL(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GXTYP  - type of X-axis
C                0 - time
C                1 - arithmetic
C                2 - logarithmic
C                3 - probability in percent (99 - 1)
C                4 - recurrence interval, years (1 - 100)
C                5 - probability as a fraction (.99 - .01)
C                6 - probability in percent (1 - 99)
C                7 - recurrence interval, years (100 - 1)
C                8 - probability as a fraction (.01 - .99)
C     GYTYP  - type of Y-axes  (i=1 for left axis, i=2 for right axis)
C                0 - none
C                1 - arithmetic
C                2 - logarithmetic
C     GALEN  - length of auxiliary axis in world coordinates
C     GYLABL - title for the left y-axis
C     GYXLBL - title for the right y-axis
C     GYALBL - title for auxilary plot on top for timeseries plot
C     GTITL  - title for the plot
C
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + END SPECIFICATIONS + + +
C
      ALEN = GALEN
      XTYPE   = GXTYP
      YTYPE(1)= GYTYP(1)
      YTYPE(2)= GYTYP(2)
C
      DO 10 I= 1,240
        TITL(I)= GTITL(I)
 10   CONTINUE
C
      DO 20 I= 1,80
        YLABL(I) = GYLABL(I)
        YXLABL(I)= GYXLBL(I)
        YALABL(I)= GYALBL(I)
 20   CONTINUE
C
C     WRITE (FE,*) 'GPLABL ',XTYPE,YTYPE,YLABL,YXLABL,YALABL,TITL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTIME
     I                    (GTSTEP,GTUNIT,GSDATE,GEDATE,GDTYPE)
C
C     + + + PURPOSE + + +
C     This routine puts, into a common area, specifications
C     for time-series curves.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  GTSTEP(*),GTUNIT(*)
      INTEGER  GSDATE(6),GEDATE(6), GDTYPE(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GTSTEP - time step for each curve in GTUNIT
C     GTUNIT - time units for each curve
C                2 = minutes,  3 = days,  5 = months,  6 = years
C     GSDATE - starting year, month, day, hour, minute of current plot
C                (month=1-12)
C     GEDATE - ending year, month, day, hour, minute of current plot
C                (month=1-12)
C     GDTYPE - data type, 1-mean or sum, 2-point or instantaneous
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,ICRV
C
C     + + + END SPECIFICATIONS + + +
C
      ICRV = NCRV
      IF (ICRV .LT. 1) ICRV = 1
      IF (ICRV .GT. TSMAX) ICRV = TSMAX
C
      DO 10 I= 1,ICRV
        TSTEP(I) = GTSTEP(I)
        TUNITS(I)= GTUNIT(I)
        DTYPE(I)= GDTYPE(I)
 10   CONTINUE
C
      DO 20 I= 1,6
        SDATIM(I)= GSDATE(I)
        EDATIM(I)= GEDATE(I)
 20   CONTINUE
C
C     WRITE (FE,*) 'GPTIME ',NCRV,TSTEP,TUNITS,SDATIM,EDATIM
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSIZE
     I                    (GSIZL,GXPAGE,GYPAGE,GXPHYS,GYPHYS,
     I                     GXLEN,GYLEN,GALEN)
C
C     + + + PURPOSE + + +
C     This routine puts, into a common area, dimensions for the
C     axes and page and position of origin.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL  GXPAGE,GYPAGE,GXPHYS,GYPHYS,GXLEN,GYLEN,GSIZL,GALEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GSIZL  - letter height for labels in inches
C     GXPAGE - horizontal page size in inches
C     GYPAGE - vertical page size in inches
C     GXPHYS - horizontal position of origin in inches
C     GYPHYS - vertical position of origin in inches
C     GXLEN  - length of x-axis in inches
C     GYLEN  - length of y-axis in inches or both main and
C              auxilary axes plus small space between them
C     GALEN  - auxilary plot axis length
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      SIZEL = GSIZL
      XPAGE= GXPAGE
      YPAGE= GYPAGE
      XPHYS= GXPHYS
      YPHYS= GYPHYS
      XLEN = GXLEN
      YLEN = GYLEN
      ALEN = GALEN
C
C     WRITE (FE,*) 'GPSIZE ',SIZEL,XPAGE,YPAGE,XPHYS,YPHYS,XLEN,YLEN
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPXLEN
     M                   (GXLEN,GXPAGE,GYLEN,GYPAGE)
C
C     + + + PURPOSE + + +
C     This routine determines the appropriate length of xlen
C     to fill the plot window
C
C     + + + DUMMY ARGUMENTS + + +
      REAL  GXLEN,GXPAGE,GYLEN,GYPAGE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GXLEN  - length of x-axis in inches
C     GXPAGE - horizontal length of page
C     GYLEN  - length of y-axis in inches
C     GYPAGE - vertical length of page
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL    XRATIO
C
C     + + + END SPECIFICATIONS + + +
C
C     XWINLC(1) - fraction of horizontal distance for upper left corner
C     XWINLC(2) - fraction of vertical distance for upper left corner
C     XWINLC(3) - fraction of horizontal distance for lower left corner
C     XWINLC(4) - fraction of vertical distance for lower left corner
C
      XRATIO= (XWINLC(3)-XWINLC(1))/(XWINLC(4)-XWINLC(2))
      IF (XRATIO.GT.1.0) THEN
C       lengthen x axis
        GXLEN = GXLEN*XRATIO
        GXPAGE= GXPAGE*XRATIO
      END IF
      IF (XRATIO.LT.1.0) THEN
C       lengthen y axis
        GYLEN = GYLEN/XRATIO
        GYPAGE= GYPAGE/XRATIO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTEXT
     I                    (GNCHR,GCPR,GFYT,GFXT,GCTXT)
C
C     + + + PURPOSE + + +
C     This routine puts, into a common area, the text to be
C     put on the plot and values specifying characteristics
C     of the text.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     GNCHR,GCPR
      REAL        GFYT,GFXT
      CHARACTER*1 GCTXT(120)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GNCHR - number of characters to use (up to 120)
C     GCPR  - characters per line
C     GFYT  - fraction(0.0-1.0) of YLEN for upper left corner of text
C     GFXT  - fraction(0.0-1.0) of XLEN for upper left corner of text
C     GCTXT - text to be placed on plot (max 120 characters)
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + END SPECIFICATIONS + + +
C
      FYT = GFYT
      FXT = GFXT
      CPR = GCPR
      NCHR= GNCHR
C
      DO 10 I= 1,120
        CTXT(I)= GCTXT(I)
 10   CONTINUE
C
C     WRITE (FE,*) 'GPTEXT ',FYT,FXT,CPR,NCHR,CTXT
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPCURV
     I                    (GCTYP,GLNTYP,GSYMBL,GCOLOR,GPATRN,GLB)
C
C     + + + PURPOSE + + +
C     This routine puts, into a common area, values specifying
C     symbol type, line type, foreground color, background
C     color, and pattern type.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  GLNTYP(*),GCOLOR(*),GPATRN(*),
     1         GSYMBL(*),GCTYP(*)
      CHARACTER*1   GLB(20,*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GCTYP  - type of curve
C                1 = uniform time step with lines, symbols or pattern
C                2 = uniform time step with bars
C                3 = uniform time step with lines, symbols or pattern
C                    for auxiliary axis
C                4 = uniform time series with bars (auxiliary axis)
C                5 = non-uniform time series (date tagged)
C                6 = x-y plot
C                7 = x-y plot with symbol sized on third variable
C     GLNTYP - linetype
C                0 = none
C                1 = solid
C                2 = dash
C                3 = dot
C                4 = dot-dash
C     GSYMBL - symbol type
C                 0 = none
C                 1 = . (dot)
C                 2 = + (plus)
C                 3 = * (star)
C                 4 = o (circle)
C                 5 = x (X)
C     GCOLOR - line color
C                0 = background (none)
C                1 = black (white)
C                2 = red
C                3 = green
C                4 = blue
C                5 = cyan
C                6 = magenta
C                7 = yellow
C     GPATRN - pattern for shading
C                0 = none
C                1 = hollow
C                2 = solid
C                3 = horizontal lines
C                4 = vertical lines
C                5 = diagonal lines
C     GLB    - label for the n-th time-series
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,J,ICRV
C
C     + + + END SPECIFICATIONS + + +
C
      ICRV = NCRV
      IF (ICRV .LT. 1) ICRV = 1
      IF (ICRV .GT. TSMAX) ICRV = TSMAX
C
      DO 10 I= 1,ICRV
        LNTYP(I) = GLNTYP(I)
        SYMBL(I) = GSYMBL(I)
        COLOR(I) = GCOLOR(I)
        PATTRN(I)= GPATRN(I)
 10   CONTINUE
C
      DO 20 I= 1,ICRV
        CTYPE(I)= GCTYP(I)
 20   CONTINUE
C
      DO 40 I= 1,20
        DO 30 J= 1,ICRV
          LBC(I,J)= GLB(I,J)
 30     CONTINUE
 40   CONTINUE
C
C     WRITE (FE,*) 'GPCURV ',NCRV,CTYPE,LNTYP,SYMBL,COLOR,PATTRN,
C    &                       TRANSF,LBC
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSCLE
     I                    (GPLMN,GPLMX,GTICS,GBVALF)
C
C     + + + PURPOSE + + +
C     This routine puts in the CPLOT common block values for
C     scaling the plot and options when data is out of range.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GTICS(4), GBVALF(4)
      REAL      GPLMN(4), GPLMX(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GPLMN  - minimum value for X and Y axes
C     GPLMX  - maximum value for X and Y axes
C     GTICS  - number of tics for arithmetic axes
C     GBVALF - bad value flag for bottom,top,left,right
C              1- clip, plot at point going off scale
C              2- skip, leave a blank if line drawn
C              3- arrow pointing off scale, lines not connected
C              4- ignore, lines connect good values
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,4
        BVALFG(I)= GBVALF(I)
 10   CONTINUE
      DO 20 I = 1,4
        TICS(I)= GTICS(I)
        PLMX(I)= GPLMX(I)
        PLMN(I)= GPLMN(I)
 20   CONTINUE
C
C     WRITE (FE,*) 'GPSCLE',PLMN,PLMX,TICS,BVALFG
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPVAR
     I                   (GVMIN,GVMAX,GWCH,GTRANF,GLBV)
C
C     + + + PURPOSE + + +
C     This routine puts into the CPLOT common block the minimum
C     and maximum value for each variable and the axis to be
C     used for each variable.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GWCH(*), GTRANF(*)
      REAL      GVMIN(*), GVMAX(*)
      CHARACTER*1 GLBV(20,*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GVMIN  - minimum value for each variable
C     GVMAX  - maximum value for each variable
C     GWCH   - code for which axis for each variable
C              1 - left y-axis
C              2 - right y-axis
C              3 - auxilary axis
C              4 - x-axis
C     GTRANF - transformation type
C                1 = arithmetic (none)
C                2 = logarithmetic
C                3 = Normal (Gaussian) distribution
C     GLBV   - 20 character label for each variable
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I, IVAR, J
C
C     + + + END SPECIFICATIONS + + +
C
      IVAR = NVAR
      IF (IVAR .LT. 1) IVAR = 1
      IF (IVAR .GT. TSMAX) IVAR = TSMAX
C
      DO 20 I = 1,IVAR
        YMIN(I) = GVMIN(I)
        YMAX(I) = GVMAX(I)
        WHICH(I) = GWCH(I)
        DO 10 J = 1,20
          LBV(J,I) = GLBV(J,I)
 10     CONTINUE
 20   CONTINUE
C
      DO 30 J= 1,IVAR
        TRANSF(J)= GTRANF(J)
 30   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPGTEF
     O                    (FEE)
C
C     + + + PURPOSE + + +
C     This routine gets the Fortran unit number assigned to the
C     GKS error file.  GPERFL, GPOPEN, or PGOPEN  must be called
C     first.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FEE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FEE    - Fortran unit number for GKS error file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
C
C     + + + EXTERNALS + + +
C
C     + + + END SPECIFICATIONS + + +
C
      FEE = FE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPDEVC
     I                    (GDEVTY, GDEVCD)
C
C     + + + PURPOSE + + +
C     This routine adds the device type code and the GKS code number
C     of the graphics device to the plotting COMMON block, cplot.inc.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GDEVCD, GDEVTY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GDEVTY - device type code
C              1 - display monitor
C              2 - laser printer
C              3 - pen plotter
C              4 - CGM or GKS meta file
C              5 - DISSPLA meta file
C     GDEVCD - GKS code number for the graphics device, GKS
C              implementation specific for each computer system.
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'

C     + + + COMMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
C
C     + + + END SPECIFICATIONS + + +
C
      DEVCOD = GDEVCD
      DEVTYP = GDEVTY
C
      RETURN
      END
C
C
C  
      SUBROUTINE   GGDATX
     I                   ( NSIZ,
     O                     BFPOS, DAT )
C
C     + + + PURPOSE + + +
C     This routine retrieves the raw data buffer from the graphics 
C     common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NSIZ, BFPOS(2,*)
      REAL      DAT(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSIZ   - number of raw data values being retrieved
C     BFPOS  - array containing discriptive information
C              ?????
C     DAT    - array containing raw data
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N, NCRV, NVAR
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI, GGNCRV
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N = 1, NSIZ
        DAT(N) = YX(N)
 10   CONTINUE
      CALL GGNCRV (NCRV, NVAR )
      N = 2 * NVAR
      CALL COPYI ( N, BUFPOS, BFPOS )
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPDATX
     I                    ( NSIZ,
     O                     BFPOS, DAT )
C     + + + PURPOSE + + +
C     This routine stores the raw data buffer in the graphics 
C     common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NSIZ, BFPOS(2,*)
      REAL      DAT(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSIZ   - number of raw data values being stored
C     BFPOS  - array containing discriptive information
C              ?????
C     DAT    - array containing raw data
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N, NCRV, NVAR
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI, GGNCRV
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N = 1, NSIZ
        YX(N) = DAT(N)
 10   CONTINUE
      CALL GGNCRV (NCRV, NVAR )
      N = 2 * NVAR
      CALL COPYI ( N, BFPOS, BUFPOS )
C
      RETURN
      END

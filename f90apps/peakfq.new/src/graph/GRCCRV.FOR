C
C
C
      SUBROUTINE   GPLBCV
     I                   (INDEX, CLAB)
C
C     + + + PURPOSE + + +
C     This routine puts a curve label in the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INDEX
      CHARACTER*20  CLAB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - order number of curve
C     CLAB   - curve label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L20
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      L20 = 20
      CALL CVARAR (L20, CLAB, L20, LBC(1,INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLBCV
     O                   (INDEX, CLAB)
C
C     + + + PURPOSE + + +
C     This routine gets a curve label from the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INDEX
      CHARACTER*20  CLAB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - order number of curve
C     CLAB   - curve label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L20
C
C     + + + EXTERNALS + + +
      EXTERNAL  CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      L20 = 20
      CALL CARVAR (L20, LBC(1,INDEX), L20, CLAB)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPCTYP
     I                   (INDEX, NUMVAR, CTYP)
C
C     + + + PURPOSE + + +
C     This routine puts values defining curve types in the CPLTV
C     common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, CTYP(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - starting position in CTYPE array where input values
C              are to be stored
C     NUMVAR - number of values to store in CTYPE array
C     CTYP   - array defining type of curve
C              1 - uniform time step with lines or symbols (main plot)
C              2 - uniform time-step with bars (main plot)
C              3 - uniform time-step with lines or symbols
C                  (auxiliary plot on top)
C              4 - uniform time-step with bars (auxiliary plot on top)
C              5 - non-uniform (date-tagged) time-series
C              6 - x-y plot
C              7 - x-y plot with symbol sized on a third variable
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (NUMVAR, CTYP, CTYPE(INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGCTYP
     O                   (INDEX, NUMVAR, CTYP)
C
C     + + + PURPOSE + + +
C     This routine gets values defining curve types from the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, CTYP(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in CTYPE array of first value to be retrieved
C     NUMVAR - number of values to retrieve from CTYPE array
C     CTYP   - array defining type of curve
C              1 - uniform time step with lines or symbols (main plot)
C              2 - uniform time-step with bars (main plot)
C              3 - uniform time-step with lines or symbols
C                  (auxiliary plot on top)
C              4 - uniform time-step with bars (auxiliary plot on top)
C              5 - non-uniform (date-tagged) time-series
C              6 - x-y plot
C              7 - x-y plot with symbol sized on a third variable
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (NUMVAR, CTYPE(INDEX), CTYP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPDTYP
     I                   (INDEX, NUMVAR, DTYP)
C
C     + + + PURPOSE + + +
C     This routine puts the data type for time-series curves in
C     the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, DTYP(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - starting position in DTYPE array where input values
C              are to be stored
C     NUMVAR - number of values to store in DTYPE array
C     DTYP   - array defining data types for time-series curves
C              1 - mean or sum over time step
C              2 - instantaneous or point data
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (NUMVAR, DTYP, DTYPE(INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGDTYP
     O                   (INDEX, NUMVAR, DTYP)
C
C     + + + PURPOSE + + +
C     This routine gets the data type for time-series curves from
C     the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, DTYP(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in DTYPE array of first value to be retrieved
C     NUMVAR - number of values to retrieve from DTYPE array
C     DTYP   - array defining data types for time-series curves
C              1 - mean or sum over time step
C              2 - instantaneous or point data
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (NUMVAR, DTYPE(INDEX), DTYP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTMSU
     I                   (NUMVAR, INDEX, TIMSTP, TIMUNT)
C
C     + + + PURPOSE + + +
C     This routine puts values for curve time steps and time units
C     in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMVAR, INDEX, TIMSTP(NUMVAR), TIMUNT(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - starting position in TSTEP and TUNITS arrays where
C              input values are to be stored
C     NUMVAR - number of values to store in YMIN and YMAX arrays
C     TIMSTP - time step for each curve in TIMUNT units, or 5760 for annual
C     TIMUNT - time units for each curve (2=min, 4=day, 5=mo, 6=yr)
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (NUMVAR, TIMSTP, TSTEP(INDEX))
      CALL COPYI (NUMVAR, TIMUNT, TUNITS(INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTMSU
     O                   (NUMVAR, INDEX, TIMSTP, TIMUNT)
C
C     + + + PURPOSE + + +
C     This routine gets values for curve time steps and time units
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMVAR, INDEX, TIMSTP(NUMVAR), TIMUNT(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in TSTEP and TUNITS arrays of first value
C              to be retrieved
C     NUMVAR - number of values to retrieve from YMIN and YMAX arrays
C     TIMSTP - time step for each curve in TIMUNT units, or 5760 for annual
C     TIMUNT - time units for each curve (2=min, 4=day, 5=mo, 6=yr)
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYI (NUMVAR, TSTEP(INDEX), TIMSTP)
      CALL COPYI (NUMVAR, TUNITS(INDEX), TIMUNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPEDAT
     I                   (EDATE)
C
C     + + + PURPOSE + + +
C     This routine puts the the ending date of the current plot
C     in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   EDATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EDATE  - ending year, month, day, hour, minute of current plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L6
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      L6 = 6
      CALL COPYI (L6, EDATE, EDATIM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGEDAT
     O                   (EDATE)
C
C     + + + PURPOSE + + +
C     This routine gets the ending date of the current plot
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   EDATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EDATE  - ending year, month, day, hour, minute of current plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L6
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      L6 = 6
      CALL COPYI (L6, EDATIM, EDATE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSDAT
     I                   (SDATE)
C
C     + + + PURPOSE + + +
C     This routine puts the starting date of the current plot in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SDATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATE  - starting year, month, day, hour, minute of current plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L6
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      L6 = 6
      CALL COPYI (L6, SDATE, SDATIM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGSDAT
     O                   (SDATE)
C
C     + + + PURPOSE + + +
C     This routine gets the starting date for the current plot
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SDATE(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATE  - starting year, month, day, hour, minute of current plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L6
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      L6 = 6
      CALL COPYI (L6, SDATIM, SDATE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSYMB
     I                   (INDEX, SYMBOL)
C
C     + + + PURPOSE + + +
C     This routine puts the code for the curve symbol in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, SYMBOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in SYMBL array of value to be modified
C     SYMBOL - code for symbol type for curve
C              GKS CODE        SYMBOL
C              --------        ------
C                  0            NONE
C                  1              .
C                  2              +
C                  3              *
C                  4              O
C                  5              X
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      SYMBL(INDEX) = SYMBOL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGSYMB
     O                   (INDEX, SYMBOL)
C
C     + + + PURPOSE + + +
C     This routine gets the code for the curve symbol
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, SYMBOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in SYMBL array of value to be modified
C     SYMBOL - code for symbol type for curve
C              GKS CODE        SYMBOL
C              --------        ------
C                  0            NONE
C                  1              .
C                  2              +
C                  3              *
C                  4              O
C                  5              X
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      SYMBOL = SYMBL(INDEX)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLNTY
     I                   (INDEX, LNTYPE)
C
C     + + + PURPOSE + + +
C     This routine puts the code for the curve line type in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, LNTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in LNTYP array of value to be modified
C     LNTYPE - code for line type for curve
C              GKS CODE        LNTYPE
C              --------        ------
C                  0            NONE
C                  1            SOLID
C                  2            DASH
C                  3             DOT
C                  4           DOT-DASH
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LNTYP(INDEX) = LNTYPE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLNTY
     O                   (INDEX, LNTYPE)
C
C     + + + PURPOSE + + +
C     This routine gets the current code for the curve line type
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, LNTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in LNTYP array of value to be modified
C     LNTYPE - code for line type for curve
C              GKS CODE        LNTYPE
C              --------        ------
C                  0            NONE
C                  1            SOLID
C                  2            DASH
C                  3             DOT
C                  4           DOT-DASH
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LNTYPE = LNTYP(INDEX)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPPATN
     I                   (INDEX, PTTERN)
C
C     + + + PURPOSE + + +
C     This routine puts the code for the curve shading pattern in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, PTTERN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in PATTRN array of value to be modified
C     PTTERN - code for pattern type for curve
C              (note:  only used for time-series plots)
C              GKS CODE        PATTERN
C              --------        -------
C                  0            NONE
C                  1           HOLLOW  (plots as a histogram)
C                  2            SOLID
C                  3            HORIZ
C                  4            VERT
C                  5           DIAGONAL
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PATTRN(INDEX) = PTTERN
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGPATN
     O                   (INDEX, PTTERN)
C
C     + + + PURPOSE + + +
C     This routine gets the current code for the curve shading
C     pattern from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, PTTERN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in PATTRN array of value to be modified
C     PTTERN - code for pattern type for curve
C              (note:  only used for time-series plots)
C              GKS CODE        PATTERN
C              --------        -------
C                  0            NONE
C                  1           HOLLOW  (plots as a histogram)
C                  2            SOLID
C                  3            HORIZ
C                  4            VERT
C                  5           DIAGONAL
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PTTERN = PATTRN(INDEX)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPCOLR
     I                   (INDEX, COLR)
C
C     + + + PURPOSE + + +
C     This routine puts the code for curve color in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, COLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in COLOR array of value to be modified
C     COLR   - code for color for curve
C              GKS CODE        COLOR
C              --------        -----
C                  0         background
C                  1            B/W
C                  2            RED
C                  3           GREEN
C                  4            BLUE
C                  5            CYAN
C                  6          MAGENTA
C                  7           YELLOW
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      COLOR(INDEX) = COLR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGCOLR
     O                   (INDEX, COLR)
C
C     + + + PURPOSE + + +
C     This routine gets the current code for curve color
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, COLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in COLOR array of value to be modified
C     COLR   - code for color for curve
C              GKS CODE        COLOR
C              --------        -----
C                  0         background
C                  1            B/W
C                  2            RED
C                  3           GREEN
C                  4            BLUE
C                  5            CYAN
C                  6          MAGENTA
C                  7           YELLOW
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      COLR = COLOR(INDEX)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPWCTM
     I                   (CURVE, YVAR)
C
C     + + + PURPOSE + + +
C     This routine puts the in the CPLTV common block indicating
C     which y-axis variable corresponds to this curve.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURVE, YVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURVE  - curve number
C     YVAR   - which variable for y-axis for this curve
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      WCHVR(1, CURVE) = YVAR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGWCTM
     O                   (CURVE, YVAR)
C
C     + + + PURPOSE + + +
C     This routine gets the current value from the CPLTV common block
C     indicating which y-axis variable corresponds to this curve.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURVE, YVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURVE  - curve number
C     YVAR   - which variable for y-axis for this curve
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      YVAR = WCHVR(1, CURVE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPWCXY
     I                   (CURVE, YVAR, XVAR)
C
C     + + + PURPOSE + + +
C     This routine puts the values in the CPLTV common block
C     indicating which y-axis variable and which x-axis variable
C     correspond to this curve.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURVE, YVAR, XVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURVE  - curve number
C     YVAR   - which variable for y-axis for this curve
C     XVAR   - which variable for x-axis for this curve
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      WCHVR(1, CURVE) = YVAR
      WCHVR(2, CURVE) = XVAR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGWCXY
     O                   (CURVE, YVAR, XVAR)
C
C     + + + PURPOSE + + +
C     This routine gets the current values from the CPLTV common block
C     indicating which y-axis variable and which x-axis variable
C     correspond to this curve.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURVE, YVAR, XVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURVE  - curve number
C     YVAR   - which variable for y-axis for this curve
C     XVAR   - which variable for x-axis for this curve
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      YVAR = WCHVR(1, CURVE)
      XVAR = WCHVR(2, CURVE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPWCXS
     I                   (CURVE, YVAR, XVAR, SIZVAR)
C
C     + + + PURPOSE + + +
C     This routine puts the values in the CPLTV common block indicating
C     which y-axis and x-axis variables correspond to this curve and
C     which variable should be used to determine symbol size.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURVE, YVAR, XVAR, SIZVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURVE  - curve number
C     YVAR   - which variable for y-axis for this curve
C     XVAR   - which variable for x-axis for this curve
C     SIZVAR - which variable used to determine symbol size
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      WCHVR(1, CURVE) = YVAR
      WCHVR(2, CURVE) = XVAR
      WCHVR(3, CURVE) = SIZVAR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGWCXS
     O                   (CURVE, YVAR, XVAR, SIZVAR)
C
C     + + + PURPOSE + + +
C     This routine gets the current values from the CPLTV common block
C     indicating which y-axis and x-axis variables correspond to this
C     curve and which variable should be used to determine symbol size.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURVE, YVAR, XVAR, SIZVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURVE  - curve number
C     YVAR   - which variable for y-axis for this curve
C     XVAR   - which variable for x-axis for this curve
C     SIZVAR - which variable used to determine symbol size
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMONS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      YVAR = WCHVR(1, CURVE)
      XVAR = WCHVR(2, CURVE)
      SIZVAR = WCHVR(3, CURVE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSSZB
     I                   (INDEX, SYMBSZ)
C
C     + + + PURPOSE + + +
C     This routine puts the size of the symbol in the CPLTV 
C     common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX
      REAL      SYMBSZ 
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in SYMSIZ array of value to be modified
C     SYMBSZ - symbol height in world coordinates
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      SYMSIZ(INDEX) = SYMBSZ
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGSSZB
     I                   (INDEX,
     O                    SYMBSZ)
C
C     + + + PURPOSE + + +
C     This routine gets the size of the symbol from the CPLTV
C     common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX
      REAL      SYMBSZ
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in SYMSIZ array of value to be retrieved
C     SYMBSZ - symbol height in world coordinates
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      SYMBSZ = SYMSIZ(INDEX)
C
      RETURN
      END

C
C
C
      SUBROUTINE   GPNCRV
     I                   (ICRV,IVAR)
C
C     + + + PURPOSE + + +
C     This routine puts in the CPLTV common block the number of
C     variables and curves to be plotted.  This routine should be
C     called before any other routine in the graph library.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  ICRV, IVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ICRV   - number of curves
C     IVAR   - number of variables (same as ICRV for time-series plots)
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NCRV = ICRV
      IF (NCRV .LT. 1) THEN
C       minimum number of curves is 1
        NCRV = 1
        WRITE (FE,*) 'Warning:  set number of curves to', ICRV
        WRITE (FE,*) '          the valid range is 1 to', TSMAX
        WRITE (FE,*) '          ', NCRV, ' will be used.'
      ELSE IF (NCRV .GT. TSMAX) THEN
C       maximum number of curves is tsmax
        NCRV = TSMAX
        WRITE (FE,*) 'Warning:  set number of curves to', ICRV
        WRITE (FE,*) '          the valid range is 1 to', TSMAX
        WRITE (FE,*) '          ', TSMAX, ' will be used.'
      END IF
      NVAR = IVAR
      IF (NVAR .LT. 1) THEN
C       minimum number of variables is 1
        NVAR = 1
        WRITE (FE,*) 'Warning:  set number of variables to', IVAR
        WRITE (FE,*) '          the valid range is 1 to', TSMAX
        WRITE (FE,*) '          ', NVAR, ' will be used.'
      ELSE IF (NVAR .GT. TSMAX) THEN
C       maximum number of variables is tsmax
        NVAR = TSMAX
        WRITE (FE,*) 'Warning:  set number of variables to', IVAR
        WRITE (FE,*) '          the valid range is 1 to', TSMAX
        WRITE (FE,*) '          ', TSMAX, ' will be used.'
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGNCRV
     O                   (ICRV,IVAR)
C
C     + + + PURPOSE + + +
C     This routine gets from the CPLTV common block the number of
C     variables and curves to be plotted.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  ICRV, IVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ICRV   - number of curves
C     IVAR   - number of variables (same as ICRV for time-series plots)
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ICRV = NCRV
      IVAR = NVAR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPVMNX
     I                   (INDEX, NUMVAR, YMN, YMX)
C
C     + + + PURPOSE + + +
C     This routine puts curve variable minimum and maximum values
C     into the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR
      REAL      YMN(NUMVAR), YMX(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - starting position in YMIN and YMAX arrays where
C              input values are to be stored
C     NUMVAR - number of values to store in YMIN and YMAX arrays
C     YMN    - array of minimum values for curve variables
C     YMX    - array of maximum values for curve variables
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYR
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYR (NUMVAR, YMN, YMIN(INDEX))
      CALL COPYR (NUMVAR, YMX, YMAX(INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGVMNX
     O                   (INDEX, NUMVAR, YMN, YMX)
C
C     + + + PURPOSE + + +
C     This routine gets curve variable minimum and maximum values from
C     the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR
      REAL      YMN(NUMVAR), YMX(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in YMIN and YMAX arrays of first value
C              to be retrieved
C     NUMVAR - number of values to retrive from YMIN and YMAX arrays
C     YMN    - array of minimum values for curve variables
C     YMX    - array of maximum values for curve variables
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYR
C
C     + + + END SPECIFICATIONS + + +
C
      CALL COPYR (NUMVAR, YMIN(INDEX), YMN)
      CALL COPYR (NUMVAR, YMAX(INDEX), YMX)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLBVR
     I                   (INDEX, VLAB)
C
C     + + + PURPOSE + + +
C     This routine puts a variable label in the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INDEX
      CHARACTER*20  VLAB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - order number of variable
C     VLAB   - variable label
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
      CALL CVARAR (L20, VLAB, L20, LBV(1,INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLBVR
     O                   (INDEX, VLAB)
C
C     + + + PURPOSE + + +
C     This routine gets a variable label from the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INDEX
      CHARACTER*20  VLAB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - order number of variable
C     VLAB   - variable label
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
      CALL CARVAR (L20, LBV(1,INDEX), L20, VLAB)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPVWCH
     I                   (INDEX, NUMVAR, WHCH)
C
C     + + + PURPOSE + + +
C     This routine puts into the CPLTV common block values defining
C     on which axis each variable should be plotted.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, WHCH(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - starting position in WHICH array where input values
C              are to be stored
C     NUMVAR - number of values to store in WHICH array
C     WHCH   - array defining which axis variables are to be plotted on
C              1 - left
C              2 - right
C              3 - auxilary
C              4 - x-axis
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
      CALL COPYI (NUMVAR, WHCH, WHICH(INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGVWCH
     O                   (INDEX, NUMVAR, WHCH)
C
C     + + + PURPOSE + + +
C     This routine gets values from the CPLTV common block
C     defining on which axis each variable should be plotted.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, WHCH(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in WHICH array of first value to be retrieved
C     NUMVAR - number of values to retrive from WHICH array
C     WHCH   - array defining which axis variables are to be plotted on
C              1 - left
C              2 - right
C              3 - auxilary
C              4 - x-axis
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
      CALL COPYI (NUMVAR, WHICH(INDEX), WHCH)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTRAN
     I                   (INDEX, NUMVAR, TRANS)
C
C     + + + PURPOSE + + +
C     This routine puts transformation types for variables
C     in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, TRANS(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - starting position in TRANSF array where input values
C              are to be stored
C     NUMVAR - number of values to store in TRANSF arras
C     TRANS  - transformation type for variables
C              1 - arithmetic (no transformation)
C              2 - logarithmic
C              3 - normal distribution
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
      CALL COPYI (NUMVAR, TRANS, TRANSF(INDEX))
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTRAN
     O                   (INDEX, NUMVAR, TRANS)
C
C     + + + PURPOSE + + +
C     This routine gets transformation types for variables from the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INDEX, NUMVAR, TRANS(NUMVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INDEX  - position in TRANSF arrays of first value to be retrieved
C     NUMVAR - number of values to retrieve from YMIN and YMAX arrays
C     TRANS  - transformation type for variables
C              1 - arithmetic (no transformation)
C              2 - logarithmic
C              3 - normal distribution
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
      CALL COPYI (NUMVAR, TRANSF(INDEX), TRANS)
C
      RETURN
      END

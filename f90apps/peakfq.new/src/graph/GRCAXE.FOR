C
C
C
      SUBROUTINE   GPSCYL
     I                   (PLMNYL, PLMXYL)
C
C     + + + PURPOSE + + +
C     This routine puts the minimum and maximum values for the left
C     y-axis in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNYL, PLMXYL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNYL - minimum value for left y-axis
C     PLMXYL - maximum value for left y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMN(1) = PLMNYL
      PLMX(1) = PLMXYL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGSCYL
     O                   (PLMNYL, PLMXYL)
C
C     + + + PURPOSE + + +
C     This routine gets the current maximum and minimum values for the
C     left y-axis from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNYL, PLMXYL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNYL - minimum value for left y-axis
C     PLMXYL - maximum value for left y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMNYL = PLMN(1)
      PLMXYL = PLMX(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSCYR
     I                   (PLMNYR, PLMXYR)
C
C     + + + PURPOSE + + +
C     This routine puts the maximum and minimum values for the right
C     y-axis in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNYR, PLMXYR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNYR - minimum value for right y-axis
C     PLMXYR - maximum value for right y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMN(2) = PLMNYR
      PLMX(2) = PLMXYR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGSCYR
     O                   (PLMNYR, PLMXYR)
C
C     + + + PURPOSE + + +
C     This routine gets the current maximum and minimum values for
C     the right y-axis from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNYR, PLMXYR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNYR - minimum value for right y-axis
C     PLMXYR - maximum value for right y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMNYR = PLMN(2)
      PLMXYR = PLMX(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSCAA
     I                   (PLMNAA, PLMXAA)
C
C     + + + PURPOSE + + +
C     This routine puts the maximum and minimum values for the
C     auxiliary axis in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNAA, PLMXAA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNAA - minimum value for auxiliary axis
C     PLMXAA - maximum value for auxiliary axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMN(3) = PLMNAA
      PLMX(3) = PLMXAA
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGSCAA
     O                   (PLMNAA, PLMXAA)
C
C     + + + PURPOSE + + +
C     This routine gets the current maximum and minimum values for
C     the auxiliary axis from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNAA, PLMXAA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNAA - minimum value for auxiliary axis
C     PLMXAA - maximum value for auxiliary axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMNAA = PLMN(3)
      PLMXAA = PLMX(3)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPSCXB
     I                   (PLMNXB, PLMXXB)
C
C     + + + PURPOSE + + +
C     This routine puts the maximum and minimum values for the x-axis
C     in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNXB, PLMXXB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNXB - minimum value for x-axis
C     PLMXXB - maximum value for x-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMN(4) = PLMNXB
      PLMX(4) = PLMXXB
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGSCXB
     O                   (PLMNXB, PLMXXB)
C
C     + + + PURPOSE + + +
C     This routine gets the current maximum and minimum values for the
C     x-axis from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PLMNXB, PLMXXB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLMNXB - minimum value for x-axis
C     PLMXXB - maximum value for x-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PLMNXB = PLMN(4)
      PLMXXB = PLMX(4)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLBYL
     I                   (YLABLL)
C
C     + + + PURPOSE + + +
C     This routine puts the y-axis label in the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABLL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABLL - y-axis label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CVARAR (L80, YLABLL, L80, YLABL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLBYL
     O                   (YLABLL)
C
C     + + + PURPOSE + + +
C     This routine gets the current y-axis label from the CPLTC
C     common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABLL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABLL - y-axis label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CARVAR (L80, YLABL, L80, YLABLL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLBYR
     I                   (YLABLR)
C
C     + + + PURPOSE + + +
C     This routine puts the right y-axis label in the CPLTC
C     common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABLR - right y-axis label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CVARAR (L80, YLABLR, L80, YXLABL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLBYR
     O                   (YLABLR)
C
C     + + + PURPOSE + + +
C     This routine gets the current right y-axis label
C     from the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABLR - right y-axis label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CARVAR (L80, YXLABL, L80, YLABLR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLBAA
     I                   (YLABAA)
C
C     + + + PURPOSE + + +
C     This routine puts the label for the auxiliary plot in the
C     CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABAA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABAA - label for auxiliary plot on top
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CVARAR (L80, YLABAA, L80, YALABL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLBAA
     O                   (YLABAA)
C
C     + + + PURPOSE + + +
C     This routine gets the current label for the auxiliary plot
C     from the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABAA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABAA - label for auxiliary plot on top
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CARVAR (L80, YALABL, L80, YLABAA)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLBXB
     I                   (YLABXB)
C
C     + + + PURPOSE + + +
C     This routine puts the x-axis label in the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABXB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABXB - x-axis label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CVARAR (L80, YLABXB, L80, XLABL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLBXB
     O                   (YLABXB)
C
C     + + + PURPOSE + + +
C     This routine gets the current x-axis label from the
C     CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80  YLABXB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YLABXB - x-axis label
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L80
C
C     + + + EXTERNALS + + +
      EXTERNAL  CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      L80 = 80
      CALL CARVAR (L80, YLABL, L80, YLABXB)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTCYL
     I                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine puts the number of tick marks for the left y-axis
C     in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for left y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICS(1) = TICNUM
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTCYL
     O                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine gets the current number of tick marks for the left
C     y-axis from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for left y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICNUM = TICS(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTCYR
     I                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine puts the number of tick marks for the right y-axis
C     in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for right y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICS(2) = TICNUM
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTCYR
     O                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine gets the current number of tick marks for the right
C     y-axis from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for right y-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICNUM = TICS(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTCAA
     I                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine puts the number of tick marks for the auxiliary
C     axis in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for auxiliary axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICS(3) = TICNUM
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTCAA
     O                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine gets the number of tick marks for the auxiliary
C     axis from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for auxiliary axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICNUM = TICS(3)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTCXB
     I                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine puts the number of tick marks for the x-axis in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for x-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICS(4) = TICNUM
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTCXB
     O                   (TICNUM)
C
C     + + + PURPOSE + + +
C     This routine gets the current number of tick marks for the x-axis
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TICNUM - number of ticks for x-axis
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      TICNUM = TICS(4)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPATYL
     I                   (YTYP)
C
C     + + + PURPOSE + + +
C     This routine puts the value for the left y-axis type the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YTYP   - left y-axis type
C              1 - arithmetic
C              2 - logarithmetic
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      YTYPE(1) = YTYP
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGATYL
     O                   (YTYP)
C
C     + + + PURPOSE + + +
C     This routine gets the current value for the left y-axis type
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YTYP   - left y-axis type
C              1 - arithmetic
C              2 - logarithmetic
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      YTYP = YTYPE(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPATYR
     I                   (YTYP)
C
C     + + + PURPOSE + + +
C     This routine puts the value for the right y-axis type in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YTYP   - right y-axis type
C              0 - none
C              1 - arithmetic
C              2 - logarithmetic
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      YTYPE(2) = YTYP
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGATYR
     O                   (YTYP)
C
C     + + + PURPOSE + + +
C     This routine gets the current value for the right y-axis type
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YTYP   - right y-axis type
C              0 - none
C              1 - arithmetic
C              2 - logarithmetic
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      YTYP = YTYPE(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPATXB
     I                   (XTYP)
C
C     + + + PURPOSE + + +
C     This routine puts the value for the x-axis type in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   XTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XTYP   - x-axis type
C              0 - time
C              1 - arithmetic
C              2 - logarithmetic
C              3 - probability percent (normal distribution) 99 - 1
C              4 - recurrence interval (normal distribution)1 - 100
C              5 - probability fraction (normal distribution) .99 - .01
C              6 - probability percent (normal distribution) 1 - 99
C              7 - recurrence interval (normal distribution) 100 - 1
C              8 - probability fraction (normal distribution) 0.01 - .99
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XTYPE = XTYP
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGATXB
     O                   (XTYP)
C
C     + + + PURPOSE + + +
C     This routine gets the current value for the x-axis type
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   XTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XTYP   - x-axis type
C              0 - time
C              1 - arithmetic
C              2 - logarithmetic
C              3 - probability percent (normal distribution) 99 - 1
C              4 - recurrence interval (normal distribution)1 - 100
C              5 - probability fraction (normal distribution) .99 - .01
C              6 - probability percent (normal distribution) 1 - 99
C              7 - recurrence interval (normal distribution) 100 - 1
C              8 - probability fraction (normal distribution) 0.01 - .99
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XTYP = XTYPE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPXYLN
     I                   (XLNGTH, YLNGTH)
C
C     + + + PURPOSE + + +
C     This routine puts the x- and y-axis lengths in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XLNGTH, YLNGTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLNGTH - length of x-axis (inches)
C     YLNGTH - length of y-axis (inches) of both main and auxiliary
C              axis, including small space between them
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XLEN = XLNGTH
      YLEN = YLNGTH
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGXYLN
     O                   (XLNGTH, YLNGTH)
C
C     + + + PURPOSE + + +
C     This routine gets the current x- and y-axis lengths
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XLNGTH, YLNGTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLNGTH - length of x-axis (inches)
C     YLNGTH - length of y-axis (inches) of both main and auxiliary
C              axis, including small space between them
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XLNGTH = XLEN
      YLNGTH = YLEN
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPALEN
     I                   (ALNGTH)
C
C     + + + PURPOSE + + +
C     This routine puts the auxiliary plot axis length in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   ALNGTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ALNGTH - auxiliary plot axis length
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ALEN = ALNGTH
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGALEN
     O                   (ALNGTH)
C
C     + + + PURPOSE + + +
C     This routine gets the current auxiliary plot axis length
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   ALNGTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ALNGTH - auxiliary plot axis length
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ALNGTH = ALEN
C
      RETURN
      END

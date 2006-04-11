C
C
C
      SUBROUTINE GBOXP
     I                 (IBPLOT,
     O                  NGROUP,NI,X,TITLE)
C
C     + + + PURPOSE + + +
C     get common block info for box plots
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IBPLOT,NGROUP,NI(100)
      REAL*8        X(3000)
      CHARACTER*80  TITLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IBPLOT - which box plot to retrieve
C     NGROUP - total number of groups of samples
C     NI     - number of observations in each group
C     X      - array of data points for each observation in each group
C     TITLE  - plot label
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cestplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYD,COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      NGROUP = NG(IBPLOT)
      TITLE  = CBT(IBPLOT)
C
      CALL COPYI (100,NIB(1,IBPLOT),NI)
      CALL COPYD (3000,DBX(1,IBPLOT),X)
C
      RETURN
      END
C
C
C
      SUBROUTINE GBOXPN
     O                 (NBP)
C
C     + + + PURPOSE + + +
C     get number of box plots available
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NBP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NBP    - number of box plots available
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cestplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NBP = NBPLOT
C
      RETURN
      END
C
C
C
      SUBROUTINE GPLOTN
     O                 (NP)
C
C     + + + PURPOSE + + +
C     get number of plots available
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NP     - number of plots available
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cestplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NP = NPLOT
C
      RETURN
      END
C
C
C
      SUBROUTINE GPLOTP
     I                 (IPLOT,
     O                  NDATA,NFUN,X,A,RANGE,SYMBOL,
     O                  XTITLE,YTITLE,TITLE)
C
C     + + + PURPOSE + + +
C     get common block data for specified plot
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IPLOT,NDATA,NFUN,INC
      REAL          RANGE(4)
      REAL*8        X(5000),A(5000,10)
      CHARACTER*10  SYMBOL
      CHARACTER*80  XTITLE,YTITLE,TITLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IPLOT  - which plot to retrieve
C     NDATA  - number of indep variable data points
C     NFUN   - number of sets of points
C     X      - array of indep variable data points
C     A      - array of dependent variable values
C     RANGE  - min x, max x, min y, max y
C     SYMBOL - symbol used to plot function
C     XTITLE - x axis label
C     YTITLE - y axis label
C     TITLE  - plot label
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cestplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYD
C
C     + + + END SPECIFICATIONS + + +
C
      NDATA  = ND(IPLOT)
      NFUN   = NCRV(IPLOT)
      XTITLE = XT(IPLOT)
      YTITLE = YT(IPLOT)
      TITLE  = CT(IPLOT)
      SYMBOL = SY(IPLOT)
      RANGE(1) = RMM(1)
      RANGE(2) = RMM(2)
      RANGE(3) = RMM(3)
      RANGE(4) = RMM(4)
C
      CALL COPYD (5000,DX(1,IPLOT),X)
      CALL COPYD (5000*NFUN,DA(1,1,IPLOT),A)
C
      RETURN
      END
C
C
C
      SUBROUTINE DPLOTI
C
C     + + + PURPOSE + + +
C     initialize common block for plots
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cestplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      NPLOT = 0
      NBPLOT= 0
      DO 10 I = 1,5
        XT(5) = ' '
        YT(5) = ' '
        CT(5) = ' '
        SY(5) = ' '
 10   CONTINUE
      CBT(1)= ' '
      CBT(2)= ' '
      CBT(3)= ' '
      CBT(4)= ' '
C
      RETURN
      END

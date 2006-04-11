C
C                      COMMON block CPLOTB.INC
C
C     YX(n)       - values of data to be plotted (1 < n <6000)
C     BUFPOS(i,j) -  i = 1,2    j = 1,NVAR
C                   Start(i=1) and end(i=2) position of each variable
C                   in YX data array.
C                   (When bar chart, j is order number of bar within
C                   the cluster, i is the start and end of position
C                   for the value of the first and last cluster.)
C     FRPOS       - first free position in YX array to store values.
C
C
C                     COMMON block CPLOT.INC
C
C                         specs for variables
C     NVAR      - number of variables
C                 (For bar charts, number of clusters)
C     YMIN(j)   - j = 1,NVAR, minimum value for each variable
C     YMAX(j)   - j = 1,NVAR, maximum value for each variable
C     WHICH(j)  - which axis for each variable (j = 1,NVAR)
C                         1 - left y-axis
C                         2 - right y-axis
C                         3 - auxiliary
C                         4 - x-axis
C     LBV(i,j)  - label for j-th variable (or jth cluster if a 
C                 bar chart), j = 1,NVAR, i = 1,20
C     TRANSF(j) - transformation type for each variable (j = 1,NVAR)
C                   0 = none
C                   1 = arithmetic (no transformation)
C                   2 = logarithmic
C                   3 = Normal distribution
C
C                          specs for curves
C     NCRV      - number of curves (or bars per cluster if bar chart)
C     WCHVR(i,k)- which variable for each curve, k = 1, NCRV
C                       i=1 - y-axis variable (left or right)
C                       i=2 - x-axis variable
C                       i=3 - variable for symbol size
C     CTYPE(k)  - type of curve  (k = 1,NCRV)
C                 1 = uniform time step with lines or symbols
C                     (main plot)
C                 2 = uniform time-step with bars (main plot)
C                 3 = uniform time-step with lines or symbols
C                     (auxiliary plot on top)
C                 4 = uniform time-step with bars (auxiliary plot)
C                 5 = non-uniform (date-tagged) time-series
C                 6 = x-y plot
C                 7 = X-Y plot with symbol sized on a third variable
C                 8 = bar chart
C     LBC(i,k)  - label for k-th curve (or kth bar in a cluster),
C                 k = 1,NCRV, i = 1,20
C     DTYPE(k)  - data type for time-series (k = 1,NCRV)
C                  1- mean or sum over time-step (plot as steps)
C                  2- instantaneous or point data (connects points)
C     TSTEP(k)  - time step for each curve in TUNITS units
C                 or 5760 for annual.
C     TUNITS(k) - time units for each curve (2=min,4=day,5=mo,6=yr)
C     SYMBL(k)  - for each curve, code for symbol type (k=1,NCRV)
C                      SYMBOL           GKS CODE
C                     --------          --------
C                       NONE               0
C                          .               1
C                          +               2
C                          *               3
C                          O               4
C                          X               5
C     SYMSIZ(k) - height of symbol for each curve in world coordinates
C                 (k=1,NCRV)
C     LNTYP(k)  - code for line type for each curve, (k=1,NCRV)
C                      SYMBOL           GKS CODE
C                     --------          --------
C                       NONE               0
C                      SOLID               1
C                       DASH               2
C                       DOT                3
C                    DOT-DASH              4
C     PATTRN(k) - code for pattern for each curve or each bar of a 
C                 cluster (k = 1,NCRV)
C                 (note: only used for time-series plots or bar charts)
C                      SYMBOL           GKS CODE
C                     --------          --------
C                       NONE               0
C                      HOLLOW              1    (plots as a histogram)
C                      SOLID               2
C                      HORIZ               3
C                       VERT               4
C                    DIAGONAL              5
C     COLOR(k)  - code for color for each curve, (k=1,NCRV)
C                      SYMBOL           GKS CODE
C                     --------          --------
C                     background           0
C                       B/W                1
C                        RED               2
C                       GREEN              3
C                        BLUE              4
C                       CYAN               5
C                     MAGENTA              6
C                      YELLOW              7
C
C                             specs on axes
C     PLMX(1)   - maximum value for Y-axis.
C     PLMN(1)   - minimum value for Y-axis.
C     PLMN(2)   - maximum value for Y-axis on right side
C     PLMN(2)   - minimum value for Y-axis on right side
C     PLMX(3)   - maximum value for auxiliary axis
C     PLMN(3)   - minimum value for auxiliary axis
C     PLMN(4)   - maximum value for X-axis
C     PLMN(4)   - minimum value for X-axis
C     TICS(i)   - number of tics on axes (default = 10 for main axes,
C                 = 2 for auxiliary axis)
C                      i = 1 for Y-axis on left
C                      i = 2 for Y-axis on right
C                      i = 3 for auxiliary axis
C                      i = 4 for X-axis
C     XTYPE     - type of X-axis
C                 0 = time
C                 1 = arithmetic
C                 2 = logarithmic
C                 3 = probability percent (normal dist) 99 - 1
C                 4 = recurrence interval (normal dist)1 - 100
C                 5 = probability fraction (normal dist) .99 - .01
C                 6 = probability percent (normal dist) 1 - 99
C                 7 = recurrence interval (normal dist) 100 - 1
C                 8 = probability fraction (normal dist) 0.01 - .99
C                 9 = bar chart (side-by-side clusters)
C                 10 = bar chart (stacked clusters)
C     YTYPE(i)  - type of Y-axis         (i=1 for left axis)
C                                        (i=2 for right axis)
C                 0 = none  (applies only to right axis,
C                           left axis must be non-zero)
C                 1 = arithmetic
C                 2 = logarithmetic
C     YLABL(80) - label for the left y-axis
C     YXLABL(80)- label for right y-axis
C     XLABL(80) - label for x-axis (when not time)
C     YALABL(80)- label for auxiliary plot on top
C     SDATIM(6) - starting year, month, day, hour, min of current plot
C     EDATIM(6) - ending year, month, day, hour, min of current plot
C
C                                specs on layout
C     LBCOLR    - color for axes and labels (0-black, 1-white)
C     YLEN      - length of y-axis (inches) of both main and
C                 auxiliary axis plus small space between them.
C     XLEN      - length of x-axis (inches)
C     ALEN      - auxiliary plot axis length
C     YPAGE     - vertical page size in inches.
C     XPAGE     - horizontal page size in inches.
C     XPHYS     - physical origin in inches in horizontal
C     YPHYS     - physical origin in inches in vertical
C     SIZEL     - height of lettering (inches)
C     TITL(240) - title for the plot.
C     XWINLC(i) - location of upper left and lower right corners of the
C                 X-window for a plot as fractions of the size of the
C                  monitor.
C                 (needed only for screen output on systems under X11)
C                 Distance is measured from the upper left corner.
C                  i=1 fraction of horizontal distance for upper left
C                      corner
C                  i=2 fraction of vertical distance for upper left
C                      corner
C                  i=3 fraction of horizontal distance for lower right
C                      corner
C                  i=4 fraction of vertical distance for lower right
C                      corner
C     LOCLGD(i) - x,y = location of legend as fraction of XLEN and YLEN
C                 from origin. (values between 0.0 and 1.0), (i=1,2)
C                 -2.0 = no legend
C                 -1.0 = legend in upper left corner (default location)
C                  
C     BVALFG(i) - bad value flag for
C                 bottom(i=1),top(i=2),left(i=3),right(i=4)
C                   1 - clip, plot at point going off scale
C                   2 - skip, leave blank
C                   3 - plot arrow pointing off scale, don't connect
C                       lines
C                   4 - ignore, connect good points
C     BLNKIT(i) - min(i=1)-max(i=2) on y-axis and min(i=3)-max(i=4)
C                 on x-axis for box for no plotting (as fraction of 
C                 XLEN and YLEN)
C     CTXT(i)   - text to be placed on the plot (max 120 characters)
C     NCHR      - number of characters to use (up to 120)
C     CPR       - characters per line
C     FYT       - fraction (0.0-1.0) of YLEN for upper left corner
C                 of text
C     FXT       - fraction (0.0-1.0) of XLEN for upper left corner
C                 of text
C     DEVCOD    - device code, GKS system dependent number for the four
C                 device categories, DEVTYP, from TERM.DAT
C     DEVTYP    - output device category
C                 1- screen,   2- printer(impact and laser),
C                 3- plotter,  4- meta file
C     FE        - Fortran unit number for GKS error file
C


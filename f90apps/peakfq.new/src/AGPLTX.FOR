C
C
C
      SUBROUTINE   OPTNAX
     I                   ( XTYPE, YTYPE, ALEN,
     O                     OPT )
C
C     + + + PURPOSE + + +
C     Determine combination of axis that is being drawn.
C     
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   XTYPE, YTYPE(2), OPT
      REAL      ALEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XTYPE  - type of x-axis
C              0 - time series
C              >0 - arithmetic, logarithmic, statistical
C     YTYPE  - type of y-axis: (1) left, (2) right
C              0 - not included (right only)
C              1 - airthmetic
C              2 - logarithmic
C     ALEN   - length of auxiliary plot axis
C              > .0001 - auxiliary axis is include
C              <= .0001 - auxiliary axis is not included
C     OPT    - axis combination
C              0 - undefined combination
C              1 - time plot with left y-axis
C              2 - time plot with left y-axis and aux axis
C              3 - time plot with left and right y-axis and aux axis
C              4 - time plot with left and right y-axis
C              5 - x-y plot with left y-axis
C              6 - x-y plot with left and right y-axis
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize axis option to unknown
      OPT = 0
C
C     determine axis option
      IF (XTYPE .EQ. 0) THEN
C       time series plot so there is no x-axis title
        IF (ALEN .GT. .0001) THEN
C         auxilary axis included
          IF (YTYPE(2) .EQ. 0) THEN
C           no right axis
            OPT = 2
          ELSE
C           include right axis
            OPT = 3
          END IF
        ELSE IF (YTYPE(2) .EQ. 0) THEN
C         no right axis and no auxilary axis
          OPT = 1
        ELSE
C         include right axis, no auxilary axis
          OPT = 4
        END IF
      ELSE
C       x-y plot, no auxilary axis
        IF (YTYPE(2) .EQ. 0) THEN
C         no right axis
          OPT = 5
        ELSE
C         include right axis
          OPT = 6
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLTITI
     I                   ( LOC, ROW, LONG, LTXT, TEXT, 
     M                     BUFFER )
C
C     + + + PURPOSE + + +
C     Moves axes titles into the appropriate location in the titles
C     buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LOC, ROW, LONG, LTXT
      CHARACTER*1 TEXT(LTXT), BUFFER(468)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LOC    - starting location in the titles array for the text
C     ROW    - number of "rows" in title text, each of length long
C     LONG   - length of each "row" in text
C     LTXT   - total length of title text
C     TEXT   - plot or axis title
C     BUFFER - buffer for plot and axis titles
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L1, LFND, LEN, POSB, POST, ADD, N
      CHARACTER NXTLIN
C
C     + + + FUNCTIONS + + +
      INTEGER   STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL  STRFND, CHRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NXTLIN, L1
     $     /   '&',  1 /
C
C     + + + END SPECIFICATIONS + + +
C
      LFND = LONG + 1
      POSB = LOC
      POST = 1
      DO 100 N = 1, ROW
        LEN = STRFND ( LFND, TEXT(POST), L1, NXTLIN )
        IF (LEN .LE. 0) THEN
C         next line character not found, save all in this row
          ADD = LONG
          LEN = LONG
          CALL CHRCHR ( LEN, TEXT(POST), BUFFER(POSB) )
        ELSE IF (LEN .EQ. 1) THEN
C         next line character in first position, blank line?
          ADD = 1
          BUFFER(POSB) = ' '
        ELSE
C         found next line character in text, save til next line
          ADD = LEN
          LEN = LEN - 1
          CALL CHRCHR ( LEN, TEXT(POST), BUFFER(POSB) )
        END IF
        POSB = POSB + LONG
        POST = POST + ADD
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLTITO
     I                   ( LOC, ROW, LONG, LTXT, BUFFER,
     O                     TEXT )
C
C     + + + PURPOSE + + +
C     Moves the axes title from the titles buffer in to text.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LOC, ROW, LONG, LTXT
      CHARACTER*1 BUFFER(468), TEXT(LTXT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LOC    - starting location in the titles array for the text
C     ROW    - number of "rows" in title text, each of length long
C     LONG   - length of each "row" in text
C     LTXT   - total length of title text

C     BUFFER - buffer for plot and axis titles
C     TEXT   - plot or axis title
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     N, POST, POSB
      CHARACTER*1 NXTLIN
C
C     + + + FUNCTIONS + + +
      INTEGER     CKNBLK
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHRCHR, CKNBLK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NXTLIN / '&' /
C
C     + + + END SPECIFICATIONS + + +
C
      POSB = LOC
      POST = 1
      DO 100 N = 1, ROW
        CALL CHRCHR ( LONG, BUFFER(POSB), TEXT(POST) )
        IF (N .GT. 1  .AND.  CKNBLK(LONG,TEXT(POST)) .EQ. 1) THEN
C         text is not empty, insert next line delimeter
          TEXT(POST-1) = NXTLIN
        END IF
        POSB = POSB + LONG
        POST = POST + LONG + 1
 100  CONTINUE
      IF (POST .EQ. 237) THEN
C       plot title has line delimeter at the end
        TEXT(POST) = NXTLIN
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLSCLI
     I                   ( TYPE, IPT, RPTS, RPTA, 
     O                     IMIN, IMAX, IDEF, RMIN, RMAX, RDEF )
C
C     + + + PURPOSE + + +
C     Sets the minimum, maximum and default values for the fields
C     number of tics, min and max scale values, min and max values
C     of the data for the scale screen.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TYPE, IPT, RPTS, RPTA
      INTEGER   IMIN(*), IMAX(*), IDEF(*)
      REAL      RMIN(*), RMAX(*), RDEF(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TYPE   - type of scale
C              1 - arithetic
C              2 - logarithmic
C              3 - probability
C     IPT    - array position for tic marks
C     RPTS   - array position for min and max scale values
C     RPTA   - array position for actual data limits
C     IMIN   - minimum number of tic marks
C     IMAX   - maximum number of tic marks
C     IDEF   - default number of tic marks
C     RMIN   - minimum value for scale and of data
C     RMAX   - maximum value for scale and of data
C     RDEF   - default value for scale and of data
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INDX, IMN(3), IMX(3), IDF(3)
      REAL      RMN(3,2), RMX(3,2), RDF(3,2)
C
C     + + + DATA INITIALIZATIONS + + +
C                 arith   log    prob  arith   log    prob
      DATA  IMN /    1,     0,     0 /
      DATA  IMX /   20,     0,     0 /
      DATA  IDF /   10,     0,     0 /
      DATA  RMN / -999., -1.0E9, -9.0, -999., -1.0E9, -9.0 /
      DATA  RMX / -999.,  1.0E9,  9.0, -999.,  1.0E9,  9.0 /
      DATA  RDF /  0.0,    .01,   2.0,  1.0,   100.0,  2.0 /
C
C     + + + END SPECIFICATIONS + + +
C
      INDX = TYPE
      IF (INDX .LT. 1) INDX = 1
      IF (INDX .GT. 3) INDX = 3
C
C     set valid range and defaul for tic marks
      IMIN(IPT) = IMN(INDX)
      IMAX(IPT) = IMX(INDX)
      IDEF(IPT) = IDF(INDX)
C
C     set valid range and default for min and max scale
      RMIN(RPTS) = RMN(INDX,1)
      RMAX(RPTS) = RMX(INDX,1)
      RDEF(RPTS) = RDF(INDX,1)
      RMIN(RPTS+1) = RMN(INDX,2)
      RMAX(RPTS+1) = RMX(INDX,2)
      RDEF(RPTS+1) = RDF(INDX,2)
C
C     set valid range and default for min and max of range of data
      RMIN(RPTA) = RMN(1,1)
      RMAX(RPTA) = RMX(1,1)
      RDEF(RPTA) = RDF(1,1)
      RMIN(RPTA+1) = RMN(1,2)
      RMAX(RPTA+1) = RMN(1,2)
      RDEF(RPTA+1) = RDF(1,2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCALEM
     I                   ( TYPE, SCLE, NVAR, WHICH, YMIN, YMAX,
     O                     PLMN, PLMX )
C
C     This routine determines an appropriate scale based on the minimum
C     and maximum values of curves that will be plotted on the scale.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TYPE, SCLE, NVAR, WHICH(NVAR)
      REAL      YMIN(NVAR), YMAX(NVAR), PLMN, PLMX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TYPE   - indicator for type of scale
C              0 - arithmetic
C              1 - arithmetic (force 0 for min when min < half max)
C              2 - logrithmic (untransformed min & max)
C              3-6 - probability (standard deviates)
C     SCLE   - indicator for location of scale
C              1 - left y-axis
C              2 - right y-axis
C              3 - auxiliary axis
C              4 - x-axis
C     NVAR   - number of variables to be checked
C     WHICH  - which axis does this variable get plotted against
C              1 - left y-axis
C              2 - right y-axis
C              3 - auxiliary axis
C              4 - x-axis
C     YMIN   - minimum value for each variable
C     YMAX   - maximum value for each variable
C     PLMN   - neat overall minimum value for SCLE
C     PLMX   - neat overall maximum value for SCLE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   K
      REAL      MIN, MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL SCALIT
C
C     + + + END SPECIFICATIONS + + +
C
      MIN = 1.0E10
      MAX = 1.0E-10
      DO 100 K = 1, NVAR
C       check each variable
        IF (WHICH(K) .EQ. SCLE) THEN
C         variable to be plotted on scale being determined
          IF (YMAX(K) .GT. MAX) MAX = YMAX(K)
          IF (YMIN(K) .LT. MIN) MIN = YMIN(K)
        END IF
 100  CONTINUE
C
C     adjust min and max to neat limits
      CALL SCALIT ( TYPE, MIN, MAX, PLMN, PLMX )
C
      RETURN
      END

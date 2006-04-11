C
C
C
      SUBROUTINE   SCALIT
     I                 (ITYPE,MMIN,MMAX,
     O                  PLMN,PLMX)
C
C     + + + PURPOSE + + +
C     This routine determines an appropriate scale based on the
C     minimum and maximum values and whether an arithmetic, probability,
C     or logrithmic scale is requested. Minimum and maximum for
C     probability plots must be standard deviates.  For log scales,
C     the minimum and maximum must not be transformed.
C
C     + + + KEYWORDS + + +
C     GKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ITYPE
      REAL      MMIN,MMAX,PLMN,PLMX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - indicator for type of scale
C              0 - arithmetic
C              1 - arithmetic (force 0 for min when min < half max)
C              2 - logrithmic (untransformed min & max)
C              3-6 - probability (standard deviates)
C     MMIN   - user defined minimum value
C     MMAX   - user defined maximum value
C     PLMN   - minimum value to use for axis
C     PLMX   - maximum value to use for axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   A,I,IS
      REAL      R(15),M,X,TMAX
C
C     + + + FUNCTIONS + + +
      REAL   RNDLOW
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT, ALOG10, REAL, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   RNDLOW
C
C     + + + DATA INITIALIZATIONS + + +
      DATA R/.1,.15,.2,.4,.5,.6,.8,1.0,1.5,2.0,4.0,5.0,6.0,8.0,10.0/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ITYPE.EQ.0 .OR. ITYPE.EQ.1) THEN
C       arithmetic scale
C       get next lowest mult of 10
        X = RNDLOW(MMAX)
        IF (X.GT.0.0) THEN
          IS = 1
          I = 1
        ELSE
          IS = -1
          I = 15
        END IF
 20     CONTINUE
          M = R(I)*X
          I = I + IS
        IF (MMAX.GT.M .AND. I.LE.15 .AND. I.GE.1) GO TO 20
        PLMX = M
C
        IF (MMIN.LT.0.5*MMAX .AND. MMIN.GE.0.0 .AND. ITYPE.EQ.1) THEN
          PLMN = 0.0
        ELSE
C         get next lowest mult of 10
          X = RNDLOW(MMIN)
          IF (X.GE.0.0) THEN
            IS = -1
            I = 15
          ELSE
            IS = 1
            I = 1
          END IF
 40       CONTINUE
            M = R(I)*X
            I = I + IS
          IF (MMIN.LT.M .AND. I.GE.1 .AND. I.LE.15) GO TO 40
          PLMN = M
        END IF
C
      ELSE IF (ITYPE .EQ. 2) THEN
C       logarithmic scale
        IF (MMIN.GT.1.0E-9) THEN
          A = INT(ALOG10(MMIN))
        ELSE
C         too small or neg value, set to -9
          A = -9
        END IF
        IF (MMIN.LT.1.0) A = A - 1
        PLMN = 10.0**A
C
        IF (MMAX.GT.1.0E-9) THEN
          A = INT(ALOG10(MMAX))
        ELSE
C         too small or neg value, set to -8
          A = -8
        END IF
        IF (MMAX.GT.1.0) A = A + 1
        PLMX = 10.0**A
C
        IF (PLMN*1.0E7.LT.PLMX) THEN
C         limit range to 7 cycles
          PLMN = PLMX/1.0E7
        END IF
C
      ELSE
C       probability plots - assumes data transformed to normal deviates
        TMAX = ABS(MMAX)
        IF (ABS(MMIN) .GT. TMAX) TMAX = ABS(MMIN)
        TMAX = REAL(INT(TMAX*10.0)+ 1)/10.0
        IF (TMAX .GT. 4.0) TMAX = 4.0
        PLMN = -TMAX
        PLMX = TMAX
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SIZAXE
     I                    (XPAGE, YPAGE, NCRV, CTYPE, YTYPE,
     M                     NLT,NLX,NLY,
     O                     XLEN,YLEN,SIZEL,XPHYS,YPHYS)
C
C     + + + PURPOSE + + +
C     Sizes axes and letters and locates origin, all in inches,
C     based on horizontal and vertical page size and type plot.
C
C     + + + DUMMY AGUMENTS + + +
      REAL      XLEN,YLEN,SIZEL,XPHYS,YPHYS,XPAGE,YPAGE
      INTEGER   CTYPE(20),YTYPE(20),NCRV,NLT,NLX,NLY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XPAGE  - horizontal page size, in inches
C     YPAGE  - vertical page size, in inches
C     NCRV   - number of curves
C     CTYPE  - array of types of curves
C              1 - uniform time step with lines or
C                  symbols (main plot)
C              2 - uniform time step with bars
C                  (main plot)
C              3 - uniform time step with lines or
C                  symbols (auxilary plot on top)
C              4 - uniform time step with bars
C                  (auxilary plot on top)
C              6 - X-Y plot
C     YTYPE  - scale for Y-axis (left first, right second)
C              0 - none
C              1 - arithmetic
C              2 - logarithmic
C     NLT    - number of lines for title of plot
C     NLX    - number of lines for title of X-axis
C     NLY    - number of lines for title of Y-axis
C     XLEN   - length of X-axis, in inches
C     YLEN   - length of y-axis or length of main Y-axis and
C              auxilary axis plus small space between, in inches
C     SIZEL  - height of lettering, in inches
C     XPHYS  - physical origin in horizontal direction, in inches
C     YPHYS  - physical origin in vertical direction, in inches
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,K
      REAL      YMLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, INT, ABS
C
C     + + + EXTERNALS + + +
C     (none)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ABS(NLT) .GT. 3) NLT = 3
      IF (ABS(NLY) .GT. 2) NLY = 2
      IF (ABS(NLX) .GT. 2) NLX = 2
C     compute length of axes, reduce for 1/2 inch margin and
C     reduce for label and title (initial estimate)
      YLEN = (YPAGE - 1.0)/1.275
      I = 0
 10   CONTINUE
C       begin twice thru loop
        I = I + 1
C       reduce for histogram
        YMLEN = YLEN
        DO 5 K = 1,NCRV
          IF (CTYPE(K).EQ.3) THEN
            YMLEN = YLEN -1.0
          END IF
 5      CONTINUE
C
C       estimate letter size based on 40 CHAR axis label
        SIZEL = YMLEN/41.0
        IF (SIZEL .LT. 0.07) SIZEL = 0.07
        SIZEL = 0.01*REAL(INT(SIZEL*100.0))
C
C       reduce for axis label and 1/2 inch margin
        XLEN = XPAGE -1.0 - (7.0+2.0*REAL(NLY))*SIZEL
        IF (YTYPE(2).NE.0) THEN
C         reduce XLEN further for room for axis on the right.
          XLEN = XLEN - (7.0+2.0*REAL(NLY))*SIZEL
        END IF
C
C       set origin
        XPHYS = 10.0*SIZEL + 0.5
        YPHYS = 9.75*SIZEL + 0.5
C
C       reset YLEN
        YLEN = YPAGE - 1.0 - (8.0+2.0*REAL(NLT+NLX))*SIZEL
      IF (I .LT. 2) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRNSDA
     I                   (TRANSF,
     M                    X,
     O                    ERR)
C
C     + + + PURPOSE + + +
C     This routine performs all the transformations needed for the
C     data and the axes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ERR, TRANSF
      REAL   X
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TRANSF - transformation type
C              2 - logrithmic
C              3 - log pearson type III
C     X      - value to be transformed
C     ERR    - return code
C              0 - successful
C              1 - unsuccessful
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG10
C
C     + + + EXTERNALS + + +
C     (none)
C
C     + + + END SPECIFICATIONS + + +
C
      ERR = 0
C
      IF (TRANSF .EQ. 2) THEN
        IF (X .GT. 1.0E-20) THEN
C         OK value for log transform
          X = ALOG10(X)
        ELSE
C         can't take log
          X = -1.0E20
          ERR = 1
        END IF
      END IF
C
      IF (TRANSF .EQ. 3) THEN
C       normal distribution
        ERR = 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ICHOUR
     I                    (DT,
     O                     CHOUR)
C
C     + + + PURPOSE + + +
C     This routine converts an integer number for hour-minutes
C     into a character string with blanks as zeros.
C
C     + + + DUMMY ARGUMENTS  + + +
      INTEGER   DT
      CHARACTER*4 CHOUR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DT     - hour-minutes
C     CHOUR  - character string of hours-minutes with zeros filled
C              in the blanks.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, JUST, OLEN, LEN
      CHARACTER*1  CDT(4), ZRO, BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   INTCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA ZRO/'0'/, BLNK/' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (4A1)
C
C     + + + END SPECIFICATIONS + + +
C
C
      JUST= 0
      LEN = 4
      CALL INTCHR (DT, LEN, JUST, OLEN, CDT)
C     fill leading blanks with '0'S
      DO 10 I = 1,4
        IF (CDT(I) .EQ. BLNK) CDT(I) = ZRO
 10   CONTINUE
C
      WRITE(CHOUR,2000) (CDT(I),I=1,4)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DTSTRG
     I                    (DT,IOPT,SPACE,SIZEL,
     O                     LEN, CSTR)
C
C     + + + PURPOSE + + +
C     This routine takes a date as a 6 integer array and produces
C     a character string with the month abreviated to 4 characters
C     and a comma between the day and year.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEN,IOPT,DT(6)
      REAL   SPACE, SIZEL
      CHARACTER*18 CSTR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DT     - date string (yr,mo,dy,hr,mn,sc)
C     IOPT   - output option
C              1 - year and month
C              2 - year, month and day
C     SPACE  - available space on plot, in inches
C     SIZEL  - height of lettering, in inches
C     LEN    - length of character string
C     CSTR   - output character string containing date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAX,DLEN,MOLEN,YFLG,JUST,LOC,OLEN,
     #          L1,L2,L4,L9,L18,I,LM
      CHARACTER*1 MO(9),BLNK,COMMAB(2),CTMP(18),BK(1)
      CHARACTER*1 MONTH(9,12)
      CHARACTER*1 MON(4,12)
C
C     + + + FUNCTIONS + + +
      INTEGER   STRLNX
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPC, CHRCHR, INTCHR, STRLNX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA COMMAB/',',' '/
      DATA L1,L2,L4,L9,L18/1,2,4,9,18/
      DATA BLNK/' '/, BK/' '/
      DATA MON/'J','A','N',' ','F','E','B',' ','M','A','R',' ',
     #         'A','P','R',' ','M','A','Y',' ','J','U','N','E',
     #         'J','U','L','Y','A','U','G',' ','S','E','P','T',
     #         'O','C','T',' ','N','O','V',' ','D','E','C',' '/
      DATA MONTH/'J','A','N','U','A','R','Y',' ',' ',
     #           'F','E','B','R','U','A','R','Y',' ',
     #           'M','A','R','C','H',' ',' ',' ',' ',
     #           'A','P','R','I','L',' ',' ',' ',' ',
     #           'M','A','Y',' ',' ',' ',' ',' ',' ',
     #           'J','U','N','E',' ',' ',' ',' ',' ',
     #           'J','U','L','Y',' ',' ',' ',' ',' ',
     #           'A','U','G','U','S','T',' ',' ',' ',
     #           'S','E','P','T','E','M','B','E','R',
     #           'O','C','T','O','B','E','R',' ',' ',
     #           'N','O','V','E','M','B','E','R',' ',
     #           'D','E','C','E','M','B','E','R',' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (18A1)
C
C     + + + END SPECIFICATIONS + + +
C
C
      JUST = 1
      CALL ZIPC (L18,BLNK,CTMP)
      CALL ZIPC (L9,BLNK,MO)
C     compute maximum character for space
      MAX = INT(SPACE/SIZEL)
C     length of day (characters)
      DLEN = 1
      LOC  = 0
      IF (DT(3) .GE. 10) DLEN = 2
      LM = DT(2)
      CALL CHRCHR (L9,MONTH(1,LM),MO)
      LEN = 6 + STRLNX(L9,MONTH(1,LM))
C     add space for day if iopt=2
      IF (IOPT .EQ. 2) LEN = LEN + DLEN + 1
      IF (LEN .LE. MAX) THEN
C       space enough for full month
        YFLG = 1
      ELSE
C       not enough space, try abbreviation of month
        CALL ZIPC (L9,BLNK,MO)
        CALL CHRCHR (L4,MON(1,LM),MO)
        MOLEN = STRLNX(L4,MON(1,LM))
        LEN = 6 + MOLEN 
C       add space for day if iopt=2
        IF (IOPT .EQ. 2) LEN = LEN + DLEN + 1
        IF (LEN .LE. MAX) THEN
C         short month OK
          YFLG = 1
        ELSE
C         not enough space, try cutting the year
          LEN = LEN - 6
          IF (LEN .LE. MAX) THEN
C           OK without year
            YFLG = 0
          ELSE
C           bad news, too little space
            IF (IOPT .EQ. 1) THEN
C             just return month character
              LOC = 2
              CALL CHRCHR (L1,MON(1,LM),CTMP)
            ELSE
C             return day only
              LOC = DLEN + 1
              CALL INTCHR (DT(3),DLEN,JUST,OLEN,CTMP)
            END IF
          END IF
        END IF
      END IF
C
      IF (LOC .EQ. 0) THEN
C       fill in array when more than day or month
        LEN = STRLNX(L9,MO)
        CALL CHRCHR (LEN,MO,CTMP)
        LOC = LEN + 1
        IF (IOPT .EQ. 2) THEN
C         add the day
          CALL CHRCHR (L1, BK(1), CTMP(LOC))
          LOC = LOC + 1
          CALL INTCHR (DT(3),DLEN,JUST,OLEN,CTMP(LOC))
          LOC = LOC + OLEN
        END IF
        IF (YFLG .EQ. 1) THEN
C         add the year         
          CALL CHRCHR (L2,COMMAB,CTMP(LOC))
          LOC = LOC + 2
          CALL INTCHR (DT(1),L4,JUST,OLEN,CTMP(LOC))
          LOC = LOC + 4
        END IF
      END IF
C
      LEN = LOC - 1
      WRITE (CSTR,2000) (CTMP(I),I=1,18)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLNSFT
     I                    (LEN,IFLG,
     M                    TITLE,
     O                    OLEN,DECPLA)
C
C     + + + PURPOSE + + +
C     This routine left justifies a string of characters within
C     the buffer TITLE, counts number of non-blank characters,
C     and counts number of decimal places.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,OLEN,DECPLA,IFLG
      CHARACTER*1 TITLE(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - available length of character string
C     IFLG   - 0 - don't delete trailing zeros
C              1 - delete trailing zeros
C     TITLE  -  character string
C     OLEN   - actual length of character string
C     DECPLA - number of decimal places
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L,IS,I,K,TLEN,DOTFLG
      CHARACTER*1 BUFF(132),BLNK,ZERO,DOT
C
C     + + + FUNCTIONS + + +
      INTEGER   STRLNX
C
C     + + + EXTERNALS + + +
      EXTERNAL   STRLNX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/, DOT/'.'/, ZERO/'0'/
C
C     + + + END SPECIFICATIONS + + +
C
      TLEN= LEN
      IF (TLEN.GT.132) TLEN= 132
      I= 0
 10   CONTINUE
        I= I+ 1
      IF (TITLE(I).EQ.BLNK .AND. I.LT.TLEN) GO TO 10
      IF (TITLE(I).EQ.BLNK .AND. I.EQ.TLEN) I= I+ 1
      IS = I
C     found beginning of non-blanks
C
      IF (IS.GT.TLEN.OR.IS.EQ.1) GO TO 50
C       title not all blanks and not currently left justified.
        L= TLEN- (IS- 1)
        DO 30 I =1,TLEN
          BUFF(I) = TITLE(I)
          TITLE(I)= BLNK
 30     CONTINUE
        DO 40 I= 1,L
          K= IS+ I- 1
          TITLE(I)= BUFF(K)
 40     CONTINUE
C
 50   CONTINUE
C
      IF (IFLG.EQ.1) THEN
C       now eliminate trailing 0's and . and get length
        L= TLEN+ 1
        OLEN= -9
 60     CONTINUE
          L= L- 1
          IF (TITLE(L).EQ.DOT) THEN
            TITLE(L)= BLNK
            OLEN= L- 1
          ELSE IF (TITLE(L).EQ.ZERO) THEN
            TITLE(L)= BLNK
          ELSE IF (TITLE(L).NE.BLNK) THEN
            OLEN= L
          END IF
        IF (OLEN.LT.0) GO TO 60
      ELSE
C       just get length of string
        OLEN = STRLNX(TLEN,TITLE)
      END IF
C
      IF (OLEN.GT.TLEN) OLEN= TLEN
C
C     count decimal places
      DECPLA= 0
      I     = 0
      DOTFLG= 0
 70   CONTINUE
        I= I+ 1
        IF (TITLE(I).EQ.DOT) THEN
C         found decimal point
          DOTFLG= 1
        END IF
      IF (DOTFLG.EQ.0 .AND. I.LT.OLEN) GO TO 70
C
      IF (I.LT.OLEN) THEN
C       loop to count decimal places
        DECPLA= -1
 80     CONTINUE
          I= I+ 1
          DECPLA= DECPLA+ 1
        IF (TITLE(I).NE.BLNK .AND. I.LT.TLEN) GO TO 80
        IF (TITLE(I).NE.BLNK .AND. I.EQ.TLEN) DECPLA= DECPLA+ 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LOGCYC
     M                   (PMIN,PMAX,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     This routine checks that the number of cycles for a log scale
C     is appropriate and multiples of ten.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL      PMIN,PMAX
      INTEGER   RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PMIN   - minimum value on axis for log scale
C     PMAX   - maximum value on axis for log scale
C     RETCOD - 0 - ok, 1 - values changed to full cycles
C              2 - too many cycles, minimum increased so only 7
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IPLMN,IPLMX,CMAX,CYCLES
C
C     + + + INTRINSICS + + +
      INTRINSIC  INT, ALOG10, ABS
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
      IF (PMIN.LT.1.0E-9) THEN
        PMIN = 1.0E-9
        RETCOD = 1
      END IF
      IF (PMAX.LT.1.0E-7) THEN
        PMAX = 1.0E-7
        RETCOD = 1
      END IF
C     find even/smaller for min and even/larger for max
      IF (PMIN .LT. 1.0) THEN
        IPLMN = INT(ALOG10(PMIN*1.0001)) -1
      ELSE IF (PMIN .GT. 1.0) THEN
        IPLMN = INT(ALOG10(PMIN *1.0001))
      ELSE
        IPLMN = 0
      END IF
      IF (PMAX .LT. 1.0) THEN
        IPLMX = INT(ALOG10(PMAX*0.9999))
      ELSE IF (PMAX .GT. 1.0) THEN
        IPLMX = INT(ALOG10(PMAX *0.9999))+ 1
      ELSE
        IPLMX = 0
      END IF
C     maximum cycles for graphic devices
      CMAX = 7
      CYCLES = IPLMX - IPLMN
      IF (CYCLES.GT.CMAX) THEN
C       maximum of 7 cycles, minimum changed.
        RETCOD = 2
        IPLMN = IPLMX - CMAX
      END IF
C
      IF (ABS((PMAX - 10.0**IPLMX)/PMAX) .GT. 0.001)  RETCOD = 1
      IF (ABS((PMIN - 10.0**IPLMN)/PMIN) .GT. 0.001)  RETCOD = 1
      PMAX = 10.0**IPLMX
      PMIN = 10.0**IPLMN
C
      RETURN
      END
C
C
C
      REAL FUNCTION   RNDLOW
     I                       (PX)
C
C     + + + PURPOSE + + +
C     This routine sets values less than 1.0E-19 to 0.0 for the
C     plotting routines for bug in DISSPLA/PR1ME.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   PX
C
C     + + + ARGUMENT DEFINITION + + +
C     PX     - real number to be reset if less than 1.0E-19
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   A
      REAL      X,SIGN
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, INT, ALOG10
C
C     + + + END SPECIFICATIONS + + +
C
      SIGN = 1.0
      IF (PX.LT.0.0) SIGN = -1.0
      X = ABS (PX)
      IF (X.LT.1.0E-19) THEN
        RNDLOW = 0.0
      ELSE
        A = INT(ALOG10(X))
        RNDLOW = SIGN*10.0**A
      END IF
C
      RETURN
      END

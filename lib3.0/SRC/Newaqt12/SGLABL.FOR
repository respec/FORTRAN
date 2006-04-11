C
C
C
      SUBROUTINE   SGLABL
     I                   (NDSN,CSCENM,CLOCNM,CCONNM,TU,DTRAN,
     M                    WHICH,TYPIND,
     O                    CNTCON,CNTSEN,CNTLOC,
     O                    ALAB,YRLAB,YLLAB,TITL,CLAB,CTRAN,CTUNIT)
C
C     + + + PURPOSE + + +
C     creates labels for scenario generator plots
C
C     + + + DUMMY ARGUENTS + + +
      INTEGER       NDSN,CNTCON,WHICH(NDSN),TYPIND(NDSN),
     1              CNTSEN,CNTLOC,TU,DTRAN
      CHARACTER*8   CSCENM(NDSN),CLOCNM(NDSN),CCONNM(NDSN),
     1              CTRAN,CTUNIT
      CHARACTER*20  CLAB(NDSN)
      CHARACTER*80  YRLAB,YLLAB,ALAB
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDSN   - number of data sets for which to set labels
C     CNTCON - count of constituents
C     CNTSEN - count of scenarios
C     CNTLOC - count of locations
C     WHICH  - which axis flag
C     TYPIND -
C     CSCENM - scenario names
C     CLOCNM - location names
C     CCONNM - constituent names
C     CLAB   - plot label
C     YRLAB  - right axis label
C     YLLAB  - left axis label
C     ALAB   - aux axis label
C     TITL   - title of plot
C     TU     - time units for data
C     DTRAN  - transformation function for data
C     CTRAN  -
C     CTUNIT -
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmaxd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I8,SLEN,LLEN,CLEN,J,PRECFG
      CHARACTER*1  CSCEN1(8),CLOCN1(8),CCONN1(8),BLNK
      CHARACTER*8  CTMP,UNISEN(MAXD),UNICON(MAXD),UNILOC(MAXD)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CVARAR,LENSTR,STRUNI
C
C     + + + OUTPUT FORMATS + + +
2010  FORMAT (20A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I8    = 8
      YRLAB = ' '
      YLLAB = ' '
      BLNK  = ' '
      PRECFG= 0
C
      IF (DTRAN.EQ.0) THEN
        CTRAN = 'MEAN   '
      ELSE IF (DTRAN.EQ.1) THEN
        CTRAN = 'SUMMED '
      ELSE IF (DTRAN.EQ.2) THEN
        CTRAN = 'MAXIMUM'
      ELSE IF (DTRAN.EQ.3) THEN
        CTRAN = 'MINIMUM'
      END IF
      IF (TU.EQ.1) THEN
        CTUNIT = 'SECONDLY'
      ELSE IF (TU.EQ.2) THEN
        CTUNIT = 'MINUTELY'
      ELSE IF (TU.EQ.3) THEN
        CTUNIT = 'HOURLY  '
      ELSE IF (TU.EQ.4) THEN
        CTUNIT = 'DAILY   '
      ELSE IF (TU.EQ.5) THEN
        CTUNIT = 'MONTHLY '
      ELSE IF (TU.EQ.6) THEN
        CTUNIT = 'YEARLY  '
      END IF
C
      IF (NDSN .EQ. 1) THEN
C       only one curve, put all info in title
        ALAB     = ' '
        YRLAB    = ' '
        YLLAB    = CCONNM(1)
        TITL     = CTUNIT // ' ' // CTRAN // ' ' // CSCENM(1) // ' '
     1             // CCONNM(1)//' at '//CLOCNM(1)
        CLAB(1)  = CSCENM(1) // ' ' // CLOCNM(1)
        WHICH(1) = 1
        TYPIND(1)= 1
      ELSE
C       count occurances of locations
        CALL STRUNI(NDSN,CLOCNM,
     O              CNTLOC,UNILOC)
C       count occurances of constit
        CALL STRUNI(NDSN,CCONNM,
     O              CNTCON,UNICON)
C       count occurances of scenarios
        CALL STRUNI(NDSN,CSCENM,
     O              CNTSEN,UNISEN)
        DO 3 I = 1,NDSN
          CTMP = CCONNM(I)
          IF (CTMP(1:2).EQ.'PR' .AND. CNTCON.GT.1) THEN
C           precip on aux axis
            WHICH(I)= 3
            ALAB    = CTMP
            IF (PRECFG.EQ.0) THEN
C             reduce count of constit for moved prec
              CNTCON= CNTCON- 1
            END IF
            PRECFG  = 1
          ELSE IF (YLLAB.EQ.' ' .OR. YLLAB.EQ.CCONNM(I)) THEN
C           we are plotting another constituent on left
            YLLAB   = CCONNM(I)
            WHICH(I)= 1
          ELSE
C           put on right for lack of a better place
            YRLAB   = CCONNM(I)
            WHICH(I)= 2
          END IF
          TYPIND(I)= I
 3      CONTINUE
C
        IF (CNTCON.EQ.1 .AND. CNTLOC.EQ.1 .AND. PRECFG.EQ.0) THEN
C         only scen varies
          TITL= CTUNIT // ' ' // CTRAN // ' ' //CCONNM(1) // ' at '
     1          // CLOCNM(1)
          DO 4 I= 1,NDSN
            CLAB(I) = CSCENM(I)
 4        CONTINUE
        ELSE IF (CNTSEN.EQ.1 .AND. CNTLOC.EQ.1) THEN
C         only const varies
          TITL= CTUNIT // ' ' // CTRAN // ' ' //CSCENM(1) // ' at '
     1          // CLOCNM(1)
          DO 5 I= 1,NDSN
            CLAB(I) = CCONNM(I)
 5        CONTINUE
        ELSE IF (CNTSEN.EQ.1 .AND. CNTCON.EQ.1) THEN
C         only locn varies
          TITL= CTUNIT // ' ' // CTRAN // ' ' //CSCENM(1) // ' '
     1          // CCONNM(1)
          DO 6 I= 1,NDSN
            CLAB(I) = CLOCNM(I)
 6        CONTINUE
        ELSE IF (CNTLOC.EQ.1) THEN
C         scen and con vary
          TITL= 'Analysis Plot for '// CTUNIT // ' ' // CTRAN // ' '
     1           // ' at ' // CLOCNM(1)
          DO 7 I= 1,NDSN
            CLAB(I) = CSCENM(I) // ' ' // CCONNM(I)
 7        CONTINUE
        ELSE IF (CNTCON.EQ.1) THEN
C         scen and loc vary
          TITL= 'Analysis Plot for '// CTUNIT // ' ' // CTRAN // ' '
     1          // CCONNM(1)
          DO 8 I= 1,NDSN
            CLAB(I) = CSCENM(I) // ' ' // CLOCNM(I)
 8        CONTINUE
        ELSE IF (CNTSEN.EQ.1) THEN
C         locn and con vary
          TITL= 'Analysis Plot for ' // CTUNIT // ' ' // CTRAN // ' '
     1          // CSCENM(1)
          DO 9 I= 1,NDSN
            CLAB(I) = CLOCNM(I) // ' ' // CCONNM(I)
 9        CONTINUE
        ELSE
C         everything varies
          TITL= 'Analysis Plot for ' // CTUNIT // ' '//CTRAN//' Values'
C         initialize curve labels scenario name, reach name
          DO 10 I= 1,NDSN
C           build headers for legend
            CALL CVARAR (I8,CSCENM(I),I8,CSCEN1)
            CALL CVARAR (I8,CLOCNM(I),I8,CLOCN1)
            CALL CVARAR (I8,CCONNM(I),I8,CCONN1)
            CLAB(I) = ' '
            SLEN = LENSTR(I8,CSCEN1)
            LLEN = LENSTR(I8,CLOCN1)
            CLEN = LENSTR(I8,CCONN1)
            IF (SLEN+LLEN+2+CLEN.GT.20) THEN
C             only 20 chars allowed, need to shorten
              CLEN = 18-SLEN-LLEN
            END IF
            WRITE (CLAB(I),2010) (CSCEN1(J),J=1,SLEN),BLNK,
     1                           (CLOCN1(J),J=1,LLEN),BLNK,
     2                           (CCONN1(J),J=1,CLEN)
C            CLAB(I) = CSCENM(I) // ' ' // CLOCNM(I) // ' ' //CCONNM(I)
            WRITE(99,*) CSCENM(I),CLOCNM(I),CCONNM(I)
 10       CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STRUNI
     I                   (NSTR,STR,
     O                    CNT,USTR)
C
C     + + + PURPOSE + + +
C     determine unique strings in str
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NSTR,CNT
      CHARACTER*8 STR(NSTR),USTR(NSTR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSTR   - number of strings to search
C     STR    - strings to search
C     CNT    - count of unique strings
C     USTR   - unique strings
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,MATCH
C
C     + + + END SPECIFICATIONS + + +
C
C     first string is unique
      CNT      = 1
      USTR(CNT)= STR(1)
      DO 20 I= 2,NSTR
C       look thru other strings
        MATCH= 0
        DO 10 J= 1,CNT
C         look thru existing uniques
          IF (STR(I) .EQ. USTR(J)) THEN
C           not unique
            MATCH= 1
          END IF
 10     CONTINUE
        IF (MATCH.EQ.0) THEN
C         unique
          CNT= CNT+ 1
          USTR(CNT)= STR(I)
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   FITLIN
     I                   (NPTS,BUFMAX,YX,
     O                    ACOEF,BCOEF,RSQUAR)
C
C     + + + PURPOSE + + +
C     fit a line through a set of data points using least squares
C     regression.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NPTS,BUFMAX
      REAL          YX(BUFMAX),ACOEF,BCOEF,RSQUAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NPTS   - number of points (number of time steps)
C     BUFMAX - size of data buffer YX
C     YX     - data buffer
C     ACOEF  - 'a' coefficient in regression line (y=ax+b)
C     BCOEF  - 'b' coefficient in regression line (y=ax+b)
C     RSQUAR - 'r squared', the coefficient of determination
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      REAL        SUM1,SUM2,SUM3,SUM4,AVG1,AVG2
C
C     + + + END SPECIFICATIONS + + +
C
      SUM1 = 0.0
      SUM2 = 0.0
      SUM3 = 0.0
      SUM4 = 0.0
C
      DO 10 I = 1,NPTS
        SUM1 = SUM1 + YX(I)
        SUM2 = SUM2 + YX(I+NPTS)
 10   CONTINUE
C
      IF (SUM1.GT.0.0 .AND. SUM2.GT.0.0) THEN
C       go ahead and compute
        AVG1 = SUM1/NPTS
        AVG2 = SUM2/NPTS
C
        DO 20 I= 1,NPTS
          SUM3 = SUM3 + ((YX(I) - AVG1) * (YX(I+NPTS) - AVG2))
          SUM4 = SUM4 + ((YX(I+NPTS) - AVG2) * (YX(I+NPTS) - AVG2))
 20     CONTINUE
C
        ACOEF = SUM3/SUM4
        BCOEF = AVG1 - (ACOEF*AVG2)
C       Y = ACOEF*X + BCOEF
C
        SUM1 = 0
        SUM2 = 0
        DO 30 I = 1,NPTS
          SUM1= SUM1 +((ACOEF*YX(I+NPTS)+BCOEF-AVG1)*
     1                 (ACOEF*YX(I+NPTS)+BCOEF-AVG1))
          SUM2= SUM2 +((YX(I)-AVG1)*(YX(I)-AVG1))
 30     CONTINUE
        RSQUAR = SUM1/SUM2
      ELSE
C       regression doesnt make sense, return zeros
        ACOEF  = 0.0
        BCOEF  = 0.0
        RSQUAR = 0.0
      END IF
C
      RETURN
      END

C
C
C
      SUBROUTINE   TSCBAT
     I                    (WDMFL,DSNCNT,DSN,NDSN,LTITLE,
     I                     NCI,CLASS,FOUT,SELFG,BADVAL,
     I                     SDATIM,EDATIM,TUNITS,TSTEP,DTRAN,
     O                     ZANB,ZB,ZA,ZBNA,ZAB,TNUM,TSDIF,TPDIF,
     O                     TSDIF2,TPDIF2,TBIAS,TPBIAS,STEST,ETOT,
     O                     CPCTA,CPCTB,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     This routine calculates flow-duration statistics and prints
C     the flow-duration table for use in comparing 2 time series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMFL,DSNCNT,DSN(DSNCNT),NDSN,
     1            NCI,FOUT,SELFG,RETCOD,
     2            ZA,ZB,ZAB,ZANB,ZBNA,TNUM,ETOT(8),
     3            TSTEP,TUNITS,SDATIM(6),EDATIM(6),DTRAN
      REAL        CLASS(NCI),BADVAL(2),TSDIF,TSDIF2,
     1            TPDIF,TPDIF2,TBIAS,TPBIAS,STEST,
     2            CPCTA(35),CPCTB(35)
      CHARACTER*1  LTITLE(80,2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number for input direct access file
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers for analysis
C     NDSN   - starting dsn in array
C     NCI    - number of class intervals
C     CLASS  - array of class interval values
C     FOUT   - Fortran unit number of output file
C     SELFG  - select data sets flag, if 1 user can select data sets
C              and values are assumed to be simulated/observed
C     LTITLE - titles for each of two data sets
C     BADVAL - array containing value recognized as missing for
C              each of the time series
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TUNITS - time units
C     TSTEP  - time step
C     DTRAN  - transformation function
C     ZANB   -
C     ZB     -
C     ZA     -
C     ZBNA   -
C     ZAB    -
C     TNUM   -
C     TSDIF  -
C     TPDIF  -
C     TSDIF2 -
C     TPDIF2 -
C     TBIAS  -
C     TPBIAS -
C     STEST  -
C     ETOT   -
C     CPCTA  -
C     CPCTB  -
C     RETCOD - return code
C
C     + + + PARAMETERS + + +
      INTEGER   BUFMAX
      PARAMETER (BUFMAX = 3660)
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      I,J,K,I6,NUMA(35),QFLG,ERRFLG,
     $             TEMP(12),NPTS,
     $             NP,EMATRX(35,8),NUMB(35),A,B,NBVAL
      REAL         SDIF(35),SDIF2(35),BIAS(35),SUMA(35),SUMB(35),
     $             PDIF(35),
     $             PDIF2(35),PBIAS(35),
     $             DIF,X,
     $             ERRINT(7),ZERO,YX(BUFMAX)
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     TIMDIF, TIMADD
      EXTERNAL     CKDATE, CTRSTR
      EXTERNAL     COPYI, WDTGET, TSCOUT
C
C     + + + DATA INITIALIZATIONS   + + +
      DATA ERRINT/-60.0,-30.0,-10.0,0.0,10.0,30.0,60.0/
C
C     + + + END SPECIFICATIONS + + +
C
      I6 = 6
C
      ZA = 0
      ZB = 0
      ZAB = 0
      ZANB = 0
      ZBNA = 0
      NBVAL = 0
C
      DO 136 I = 1,NCI
        NUMA(I) = 0
        NUMB(I) = 0
        BIAS(I) = 0.0
        SUMA(I) = 0.0
        SUMB(I) = 0.0
        SDIF(I) = 0.0
        SDIF2(I) = 0.0
        PDIF(I) = 0.0
        PDIF2(I) = 0.0
        PBIAS(I) = 0.0
        DO 135 K = 1,8
          EMATRX(I,K) = 0
 135    CONTINUE
 136  CONTINUE
C
C     ok so far
      CALL COPYI (I6,EDATIM,TEMP(7))
 142  CONTINUE
C       determine number of values
        CALL COPYI (I6,SDATIM,TEMP)
        CALL TIMDIF (SDATIM,TEMP(7),TUNITS,TSTEP,NPTS)
        IF (NPTS .GT. BUFMAX/2) THEN
C         can only get BUFMAX at a time
          NPTS = BUFMAX/2
        END IF
C
C       begin to fill plot array
        QFLG = 30
        CALL WDTGET (WDMFL,DSN(NDSN),TSTEP,SDATIM,
     I               NPTS,DTRAN,QFLG,TUNITS,
     O               YX(1),RETCOD)
        CALL WDTGET (WDMFL,DSN(NDSN+1),TSTEP,SDATIM,
     I               NPTS,DTRAN,QFLG,TUNITS,
     O               YX(NPTS+1),RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         begin loop to fill class intervals.
          ZERO= 1.0E-9
          DO 150 NP = 1,NPTS
C           A is simulated,   B is observed
            A = NP
            B = NP + NPTS
            IF (ABS(YX(A)-BADVAL(1)) .GT. ZERO  .AND.
     $          ABS(YX(B)-BADVAL(2)) .GT. ZERO) THEN
C             both values are assumed valid
              I = NCI + 1
 144          CONTINUE
                I = I - 1
              IF (YX(A).LT.CLASS(I) .AND. I.GT.1)   GO TO 144
              NUMA(I) = NUMA(I) + 1
              SUMA(I) = SUMA(I) + YX(A)
C
              J = NCI + 1
 146          CONTINUE
                J = J - 1
              IF (YX(B).LT.CLASS(J) .AND. J.GT.1)   GO TO 146
              SUMB(J) = SUMB(J) + YX(B)
              NUMB(J) = NUMB(J) + 1
C
              DIF = YX(A) - YX(B)
              SDIF(J) = SDIF(J) + ABS(DIF)
              SDIF2(J) = SDIF2(J) + (DIF)**2
              IF (ABS(YX(B)) .GT. ZERO) THEN
                PDIF(J) = PDIF(J) + ABS(DIF)/YX(B)
                PDIF2(J) = PDIF2(J) + (DIF/YX(B))**2
                PBIAS(J) = PBIAS(J) + DIF/YX(B)
              END IF
              BIAS(J) = BIAS(J) + DIF
C
C             do zero event counter
              IF (ABS(YX(A)) .LT. ZERO) THEN
                ZA = ZA + 1
                IF (ABS(YX(B)) .LT. ZERO) THEN
                  ZB  = ZB + 1
                  ZAB = ZAB + 1
                ELSE
                  ZANB = ZANB + 1
                END IF
              ELSE IF (ABS(YX(B)) .LT. ZERO) THEN
                ZB   = ZB + 1
                ZBNA = ZBNA + 1
              END IF
C
C             compute error matrix
              K = 1
              X = YX(B)
              IF (X.LE.0.0) X = CLASS(J)
              IF (X.LE.0.0) THEN
                X = 0.0
              ELSE
                X = 100.0*DIF/X
              END IF
 147          CONTINUE
                IF (X .LE. ERRINT(K)) THEN
                  EMATRX(J,K) = EMATRX(J,K) + 1
                ELSE IF (K .LT. 7) THEN
                  K = K + 1
                  GO TO 147
                ELSE
C                 reached last error range
                  K = 8
                  EMATRX(J, K) = EMATRX(J, K) + 1
                END IF
            ELSE
C             bad/missing value on simulated or(and) observed
              NBVAL = NBVAL + 1
            END IF
 150      CONTINUE
C
C         check adjusted start date with end date
          CALL TIMADD (TEMP,TUNITS,TSTEP,NPTS,
     O                 SDATIM)
          CALL COPYI (I6,TEMP(7),EDATIM(1))
          CALL CKDATE (SDATIM,EDATIM,ERRFLG)
C         -1 =sdatim<edatim,  0=they are equal,  1=sdatim>edatim
C         go back to get more data.
        END IF
      IF (ERRFLG.LT.0 .AND. RETCOD.EQ.0) GO TO 142
C
      CALL TSCOUT (LTITLE,
     I             NCI,CLASS,FOUT,SELFG,
     I             ZANB,ZB,ZA,ZBNA,ZAB,NUMA,EMATRX,NUMB,NBVAL,
     I             SDIF,SDIF2,BIAS,SUMA,SUMB,PDIF,PDIF2,PBIAS,
     O             TNUM,TSDIF,TPDIF,TSDIF2,TPDIF2,TBIAS,TPBIAS,
     O             STEST,ETOT,
     O             CPCTA,CPCTB)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSCBWD
     I                    (NPTS,YX,LTITLE,
     I                     NCI,CLASS,FOUT,SELFG,BADVAL,
     I                     SDATIM,EDATIM,TUNITS,TSTEP,DTRAN,
     O                     ZANB,ZB,ZA,ZBNA,ZAB,TNUM,TSDIF,TPDIF,
     O                     TSDIF2,TPDIF2,TBIAS,TPBIAS,STEST,ETOT,
     O                     CPCTA,CPCTB)
C
C     + + + PURPOSE + + +
C     This routine calculates flow-duration statistics and prints
C     the flow-duration table for use in comparing 2 time series,
C     batch version with data as input argument.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NPTS,NCI,FOUT,SELFG,
     2            ZA,ZB,ZAB,ZANB,ZBNA,TNUM,ETOT(8),
     3            TSTEP,TUNITS,SDATIM(6),EDATIM(6),DTRAN
      REAL        CLASS(NCI),BADVAL(2),TSDIF,TSDIF2,
     1            TPDIF,TPDIF2,TBIAS,TPBIAS,STEST,
     2            CPCTA(35),CPCTB(35),YX(NPTS*2)
      CHARACTER*1  LTITLE(80,2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NPTS   - number of data points
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers for analysis
C     NDSN   - starting dsn in array
C     NCI    - number of class intervals
C     CLASS  - array of class interval values
C     FOUT   - Fortran unit number of output file
C     SELFG  - select data sets flag, if 1 user can select data sets
C              and values are assumed to be simulated/observed
C     LTITLE - titles for each of two data sets
C     BADVAL - array containing value recognized as missing for
C              each of the time series
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TUNITS - time units
C     TSTEP  - time step
C     DTRAN  - transformation function
C     ZANB   -
C     ZB     -
C     ZA     -
C     ZBNA   -
C     ZAB    -
C     TNUM   -
C     TSDIF  -
C     TPDIF  -
C     TSDIF2 -
C     TPDIF2 -
C     TBIAS  -
C     TPBIAS -
C     STEST  -
C     ETOT   -
C     CPCTA  -
C     CPCTB  -
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      I,J,K,NUMA(35),
     $             NP,EMATRX(35,8),NUMB(35),A,B,NBVAL
      REAL         SDIF(35),SDIF2(35),BIAS(35),SUMA(35),SUMB(35),
     $             PDIF(35),PDIF2(35),PBIAS(35),
     $             DIF,X,ERRINT(7),ZERO
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     TSCOUT
C
C     + + + DATA INITIALIZATIONS   + + +
      DATA ERRINT/-60.0,-30.0,-10.0,0.0,10.0,30.0,60.0/
C
C     + + + END SPECIFICATIONS + + +
C
      ZA = 0
      ZB = 0
      ZAB = 0
      ZANB = 0
      ZBNA = 0
      NBVAL = 0
C
      DO 136 I = 1,NCI
        NUMA(I) = 0
        NUMB(I) = 0
        BIAS(I) = 0.0
        SUMA(I) = 0.0
        SUMB(I) = 0.0
        SDIF(I) = 0.0
        SDIF2(I) = 0.0
        PDIF(I) = 0.0
        PDIF2(I) = 0.0
        PBIAS(I) = 0.0
        DO 135 K = 1,8
          EMATRX(I,K) = 0
 135    CONTINUE
 136  CONTINUE
C
C     begin loop to fill class intervals.
      ZERO= 1.0E-9
      DO 150 NP = 1,NPTS
C       A is simulated,   B is observed
        A = NP
        B = NP + NPTS
        IF (ABS(YX(A)-BADVAL(1)) .GT. ZERO  .AND.
     $      ABS(YX(B)-BADVAL(2)) .GT. ZERO) THEN
C         both values are assumed valid
          I = NCI + 1
 144      CONTINUE
            I = I - 1
          IF (YX(A).LT.CLASS(I) .AND. I.GT.1)   GO TO 144
          NUMA(I) = NUMA(I) + 1
          SUMA(I) = SUMA(I) + YX(A)
C
          J = NCI + 1
 146      CONTINUE
            J = J - 1
          IF (YX(B).LT.CLASS(J) .AND. J.GT.1)   GO TO 146
          SUMB(J) = SUMB(J) + YX(B)
          NUMB(J) = NUMB(J) + 1
C
          DIF = YX(A) - YX(B)
          SDIF(J) = SDIF(J) + ABS(DIF)
          SDIF2(J) = SDIF2(J) + (DIF)**2
          IF (ABS(YX(B)) .GT. ZERO) THEN
            PDIF(J) = PDIF(J) + ABS(DIF)/YX(B)
            PDIF2(J) = PDIF2(J) + (DIF/YX(B))**2
            PBIAS(J) = PBIAS(J) + DIF/YX(B)
          END IF
          BIAS(J) = BIAS(J) + DIF
C
C         do zero event counter
          IF (ABS(YX(A)) .LT. ZERO) THEN
            ZA = ZA + 1
            IF (ABS(YX(B)) .LT. ZERO) THEN
              ZB  = ZB + 1
              ZAB = ZAB + 1
            ELSE
              ZANB = ZANB + 1
            END IF
          ELSE IF (ABS(YX(B)) .LT. ZERO) THEN
            ZB   = ZB + 1
            ZBNA = ZBNA + 1
          END IF
C
C         compute error matrix
          K = 1
          X = YX(B)
          IF (X.LE.0.0) X = CLASS(J)
          IF (X.LE.0.0) THEN
            X = 0.0
          ELSE
            X = 100.0*DIF/X
          END IF
 147      CONTINUE
          IF (X .LE. ERRINT(K)) THEN
            EMATRX(J,K) = EMATRX(J, K) + 1
          ELSE IF (K .LT. 7) THEN
            K = K + 1
            GO TO 147
          ELSE
C           reached last error range
            K = 8
            EMATRX(J,K) = EMATRX(J,K) + 1
          END IF
        ELSE
C         bad/missing value on simulated or(and) observed
          NBVAL = NBVAL + 1
        END IF
 150  CONTINUE
C
      CALL TSCOUT (LTITLE,
     I             NCI,CLASS,FOUT,SELFG,
     I             ZANB,ZB,ZA,ZBNA,ZAB,NUMA,EMATRX,NUMB,NBVAL,
     I             SDIF,SDIF2,BIAS,SUMA,SUMB,PDIF,PDIF2,PBIAS,
     O             TNUM,TSDIF,TPDIF,TSDIF2,TPDIF2,TBIAS,TPBIAS,
     O             STEST,ETOT,
     O             CPCTA,CPCTB)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSCOUT
     I                    (LTITLE,
     I                     NCI,CLASS,FOUT,SELFG,
     I                     ZANB,ZB,ZA,ZBNA,ZAB,NUMA,EMATRX,NUMB,NBVAL,
     I                     SDIF,SDIF2,BIAS,SUMA,SUMB,PDIF,PDIF2,PBIAS,
     O                     TNUM,TSDIF,TPDIF,TSDIF2,TPDIF2,TBIAS,TPBIAS,
     O                     STEST,ETOT,
     O                     CPCTA,CPCTB)
C
C     + + + PURPOSE + + +
C     This routine prints the flow-duration table for use in comparing
C     2 time series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NCI,FOUT,SELFG,TNUM,EMATRX(35,8),NBVAL,
     2            ZA,ZB,ZAB,ZANB,ZBNA,ETOT(8),NUMA(35),NUMB(35)
      REAL        CLASS(NCI),STEST,CPCTA(35),CPCTB(35),
     1            TSDIF,TSDIF2,TPDIF,TPDIF2,TBIAS,TPBIAS,
     2            SDIF(35),SDIF2(35),BIAS(35),SUMA(35),SUMB(35),
     $            PDIF(35),PDIF2(35),PBIAS(35)
      CHARACTER*1  LTITLE(80,2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCI    - number of class intervals
C     CLASS  - array of class interval values
C     FOUT   - Fortran unit number of output file
C     SELFG  - select data sets flag, if 1 user can select data sets
C              and values are assumed to be simulated/observed
C     LTITLE - titles for each of two data sets
C     ZANB   -
C     ZB     -
C     ZA     -
C     ZBNA   -
C     ZAB    -
C     TNUM   -
C     TSDIF  -
C     TPDIF  -
C     TSDIF2 -
C     TPDIF2 -
C     TBIAS  -
C     TPBIAS -
C     STEST  -
C     ETOT   - total number of data in each deviation interval
C     CPCTA  -
C     CPCTB  -
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      I,K,I0,I8,I80,CNUMA,IVAL(5),CNUMB
      REAL         PCTA(35),PCTB(35),
     $             TSUMA,TSUMB,TPCTA,TPCTB,
     $             PCUMA,PCUMB,PSTERR,PABERR,BI,ABERR,
     $             STERR,PBI
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL, SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZIPI,CTRSTR
C
C     + + + OUTPUT FORMATS   + + +
 2000 FORMAT ('1')
 2001 FORMAT (80A1)
 2003 FORMAT (1X,F9.2,I9,1X,3(F10.3,F10.1))
 2033 FORMAT (1X,F9.2,I9,1X,3(F10.3,8X,'* '))
C2004 FORMAT (1X,F9.2,I10,F10.2,I10,F10.2)
 2005 FORMAT (10X,I9,1X,3(F10.3,F10.1))
 2055 FORMAT (10X,I9,1X,3(F10.3,8X,'* '))
 2007 FORMAT (1X,F9.2,I5,I5,4F10.2,2F10.2)
 2008 FORMAT (9X,I6,I6,F9.2,F10.2,20X,2F10.2//)
 2010 FORMAT (1X,I5,1X,73A1)
 2011 FORMAT (' ')
 2012 FORMAT (1X,F9.2,I7,7I8)
 2013 FORMAT (
     # ' ---------  --------------------------------------------',
     # '----------------'/9X,8I8//)
C2004 FORMAT (1X,F9.2,I10,F10.2,I10,F10.2)
C2005 FORMAT (I8)
C2007 FORMAT (/I7,' values were tagged missing and excluded from',
C    &            ' analysis.')
C2020 FORMAT (1X,F8.1,5(2X,F9.1))
C2021 FORMAT (1X,F8.1,2X,F9.0,2(2X,F9.1,2X,F9.0))
 2061 FORMAT ('                           Mean               Root',
     #        ' mean'/
     #        '   Lower    Number    absolute error(1)     square',
     #        ' error(2)        Bias(3)      '/
     #        '   class      of     ------------------- ---------',
     #        '---------- -------------------'/
     #        '   limit     cases   Average    Percent   Average ',
     #        '   Percent  Average   Percent '/
     #        ' --------- --------- --------- --------- ---------',
     #        ' --------- --------- ---------')
 2062 FORMAT (' --------- --------- --------- --------- ---------',
     #        ' --------- --------- ---------')
 2063 FORMAT (/' Standard error of estimate = ',F10.2/
     # 10X,'= square root((n/n-1)*((tot.col.5)**2-(tot.col.7)**2))'//
     # ' (1) Average = sum(|S-O|/n)'/
     # '     Percent = 100 * (sum(|S-O|/O))/n  for all O > 0'/
     # ' (2) Average = square root(sum((S-O)**2)/n)'/
     # '     Percent = 100 * square root(sum(((S-O)/O)**2)/n) for all',
     # ' O > 0'/
     # ' (3) Average = sum (S-O)/n'/
     # '     Percent = 100 * sum (((S-O)/O)/n)  for all O > 0'/)
 2064 FORMAT (/' Number of pairs with a bad (missing) value not ',
     #         'used in analysis =', I8 )
 2065 FORMAT (/' Standard error of estimate = ',F10.2/
     # 10X,'= square root((n/n-1)*((tot.col.5)**2-(tot.col.7)**2))'//
     # ' (1) Average = sum(|DS1-DS2|/n)'/
     # '     Percent = 100 * (sum(|DS1-DS2|/DS2))/n  for all DS2 > 0'/
     # ' (2) Average = square root(sum((DS1-DS2)**2)/n)'/
     # '     Percent = 100 * square root(sum(((DS1-DS2)/DS2)**2)/n)',
     # ' for all DS2 > 0'/
     # ' (3) Average = sum (DS1-DS2)/n'/
     # '     Percent = 100 * sum (((DS1-DS2)/DS2)/n)  for all DS2 > 0'/)
 2073 FORMAT (
     # '         Cases equal or exceeding lower'/
     # '          limit & less then upper limit     Percen',
     # 't cases'/
     # '           ----------------------------       equa',
     # 'l or        Average of cases'/
     # '   Lower     Cases         Percent         exceedi',
     # 'ng limit   within class limits'/
     # '   class   --------- ------------------- ---------',
     # '---------- -------------------'/
     # '   limit   Sim   Obs Simulated  Observed Simulated',
     # '  Observed Simulated  Observed'/
     # ' --------- ---- ---- --------- --------- ---------',
     # ' --------- --------- ---------'/)
 2074 FORMAT (
     # '         Cases equal or exceeding lower'/
     # '          limit & less then upper limit     Percen',
     # 't cases'/
     # '           ----------------------------       equa',
     # 'l or        Average of cases'/
     # '   Lower     Cases         Percent         exceedi',
     # 'ng limit   within class limits'/
     # '   class   --------- ------------------- ---------',
     # '---------- -------------------'/
     # '   limit   Dsn1 Dsn2  DataSet1  DataSet2  DataSet1',
     # '  DataSet2  DataSet1  DataSet2'/
     # ' --------- ---- ---- --------- --------- ---------',
     # ' --------- --------- ---------'/)
 2075 FORMAT (' --------- ---- ---- --------- --------- ----------',
     # ' --------- --------- ---------')
 2076 FORMAT (/
     #  1X,I8,' Observed values are zero'/
     #  1X,I8,' Simulated values are zero'/
     #  1X,I8,' Observed values are zero when simulated are zero'/
     #  1X,I8,' Observed values are zero when simulated are not'/
     #  1X,I8,' Observed values are not zero when simulated are'/)
 2077 FORMAT (/
     #  1X,I8,' DataSet2 values are zero'/
     #  1X,I8,' DataSet1 values are zero'/
     #  1X,I8,' DataSet2 values are zero when DataSet1 values are zero'/
     #  1X,I8,' DataSet2 values are zero when DataSet1 values are not'/
     #  1X,I8,' DataSet2 values are not zero when DataSet1 values are'/)
 2081 FORMAT (/
     # '   Lower         Number of occurrences between indicated',
     # ' deviations    '/
     # '   class    --------------------------------------------',
     # '-----------------'/
     # '   limit          -60%    -30%    -10%      0%     10% ',     
     # '    30%     60%'/
     # ' ---------  --------------------------------------------',
     # '-----------------')
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I8 = 8
      I80= 80
C
C     table includes error analysis
      TBIAS = 0.0
      TNUM = 0
      TSDIF = 0.0
      TSDIF2 = 0.0
      TPCTA = 0.0
      TPCTB = 0.0
      TSUMA = 0.0
      TSUMB = 0.0
      TPDIF = 0.0
      TPDIF2 = 0.0
      TPBIAS = 0.0
C
      DO 174 I = 1,NCI
        TNUM = TNUM + NUMB(I)
        TSUMA = TSUMA + SUMA(I)
        TSUMB = TSUMB + SUMB(I)
        TSDIF = TSDIF + SDIF(I)
        TSDIF2 = TSDIF2 + SDIF2(I)
        TBIAS = TBIAS + BIAS(I)
        TPDIF = TPDIF + PDIF(I)
        TPDIF2 = TPDIF2 + PDIF2(I)
        TPBIAS = TPBIAS + PBIAS(I)
 174  CONTINUE
C
C     write out heading
      CALL CTRSTR (I80,LTITLE(1,1))
      CALL CTRSTR (I80,LTITLE(1,2))
      WRITE(FOUT,2000)
      WRITE (FOUT,2001) (LTITLE(I,1),I=1,80)
      WRITE (FOUT,2001) (LTITLE(I,2),I=1,80)
      WRITE (FOUT,2011)
      WRITE (FOUT,2061)
C
      PCTA(1) = 0.0
      PCTB(1) = 0.0
      DO 184 I = 1,NCI
        PCTA(I) = 100.0*REAL(NUMA(I))/REAL(TNUM)
        PCTB(I) = 100.0*REAL(NUMB(I))/REAL(TNUM)
        TPCTA = TPCTA + PCTA(I)
        TPCTB = TPCTB + PCTB(I)
        IF (NUMB(I).GE.1) THEN
          ABERR = SDIF(I)/REAL(NUMB(I))
          STERR = SQRT(SDIF2(I)/REAL(NUMB(I)))
          BI = BIAS(I)/REAL(NUMB(I))
          PABERR = 100.0*PDIF(I)/ REAL(NUMB(I))
          PSTERR = 100.0*SQRT(PDIF2(I)/ REAL(NUMB(I)))
          PBI = 100.0*PBIAS(I)/ REAL(NUMB(I))
        ELSE
          ABERR = 0.0
          STERR = 0.0
          BI = 0.0
          PABERR = 0.0
          PSTERR = 0.0
          PBI = 0.0
        END IF
C
        IF (I .EQ. 1  .AND.  ZB .GT. 0) THEN
          WRITE(FOUT,2033) CLASS(I),NUMB(I),ABERR,STERR,BI
        ELSE
          WRITE(FOUT,2003) CLASS(I),NUMB(I),ABERR,
     #                     PABERR,STERR,PSTERR,BI,PBI
        END IF
 184  CONTINUE
C
      WRITE(FOUT,2062)
      TSDIF = TSDIF/REAL(TNUM)
      TSDIF2 = SQRT (TSDIF2/REAL(TNUM))
Ckmf      corrected missplaced sqrt in tpdif and tpdif2, Mar 23, 98
Ckmf      TPDIF = 100.0*SQRT(TPDIF/REAL(TNUM))
Ckmf      TPDIF2 = 100.0*TPDIF2/REAL(TNUM)
      TPDIF = 100.0*TPDIF/REAL(TNUM)
      TPDIF2 = 100.0*SQRT(TPDIF2/REAL(TNUM))
      TPBIAS = 100.0*TPBIAS/REAL(TNUM)
      TBIAS = TBIAS/REAL(TNUM)
      IF (ZB .LT. 1) THEN
        WRITE(FOUT,2005) TNUM,TSDIF,TPDIF,TSDIF2,TPDIF2,
     #                   TBIAS,TPBIAS
      ELSE
        WRITE(FOUT,2055) TNUM,TSDIF,TSDIF2,TBIAS
      END IF
C
      IF (NBVAL .GT. 0) THEN
C       print out number of bad/missing values
        WRITE (FOUT,2064) NBVAL
        WRITE (FOUT,2011)
      END IF
C
C     standard error of estimate
      STEST = SQRT((REAL(TNUM)/REAL(TNUM-1))*(TSDIF2**2
     #        - TBIAS**2))
      IF (SELFG.EQ.1) THEN
C       use sim/obs wording
        WRITE (FOUT,2063) STEST
      ELSE
C       use data set number wording
        WRITE (FOUT,2065) STEST
      END IF
      WRITE(FOUT,2011)
C
C     write 2nd table
C     write heading
      WRITE(FOUT,2000)
      WRITE (FOUT,2001) (LTITLE(I,1),I=1,80)
      WRITE (FOUT,2001) (LTITLE(I,2),I=1,80)
      WRITE (FOUT,2011)
C     WRITE (FOUT,2072) (LBV(I,1),I=1,20), (LBV(I,2),I=1,20)
      IF (SELFG.EQ.1) THEN
C       use sim/obs wording
        WRITE (FOUT,2073)
      ELSE
C       use data set number wording
        WRITE (FOUT,2074)
      END IF
C     write contents of table
      CNUMA = TNUM
      CNUMB = TNUM
      CPCTA(1) = 100.0
      CPCTB(1) = 100.0
      DO 195 I = 1,NCI
        IF (NUMA(I).GT.0) SUMA(I) = SUMA(I)/REAL(NUMA(I))
        IF (NUMB(I).GT.0) SUMB(I) = SUMB(I)/REAL(NUMB(I))
        IF(I.GT.1) CNUMA = CNUMA - NUMA(I-1)
        IF(I.GT.1) CNUMB = CNUMB - NUMB(I-1)
        PCUMA = 100.0*REAL(CNUMA)/REAL(TNUM)
        PCUMB = 100.0*REAL(CNUMB)/REAL(TNUM)
        CPCTA(I) = PCUMA
        CPCTB(I) = PCUMB
        WRITE(FOUT,2007) CLASS(I),NUMA(I),NUMB(I),PCTA(I),
     #             PCTB(I),CPCTA(I),CPCTB(I),SUMA(I),SUMB(I)
 195  CONTINUE
      WRITE (FOUT,2075)
      TSUMA = TSUMA/REAL(TNUM)
      TSUMB = TSUMB/REAL(TNUM)
      WRITE(FOUT,2008) TNUM,TNUM,TPCTA,TPCTB,TSUMA,TSUMB
C
      IF ((ZA+ZB+ZAB+ZANB+ZBNA) .GT. 0) THEN
        WRITE (FOUT,2010)
C       _____ MEASURED ARE ZERO
C       _____ SIMULATED ARE ZERO
C       _____ MEASURED ARE ZERO WHEN SIMULATED ARE ZERO
C       _____ MEASURED ARE ZERO WHEN SIMULATED ARE NOT ZERO
C       _____ MEASURED ARE NOT ZERO WHEN SIMULATED ARE ZERO
        IVAL(1) = ZB
        IVAL(2) = ZA
        IVAL(3) = ZAB
        IVAL(4) = ZBNA
        IVAL(5) = ZANB
        IF (SELFG.EQ.1) THEN
C         use sim/observed wording
          WRITE (FOUT,2076) (IVAL(I),I=1,5)
        ELSE
C         use data set number wording
          WRITE (FOUT,2077) (IVAL(I),I=1,5)
        END IF
      END IF
C
C     write bias table
C     write heading
      WRITE(FOUT,2000)
      WRITE (FOUT,2001) (LTITLE(I,1),I=1,80)
      WRITE (FOUT,2001) (LTITLE(I,2),I=1,80)
      WRITE (FOUT,2081)
C     write contents of table
      CALL ZIPI ( I8, I0, ETOT )
      DO 215 I = 1,NCI
        WRITE(FOUT,2012) CLASS(I),(EMATRX(I,K),K=1,8)
        DO 214 K = 1,8
          ETOT(K) = ETOT(K) + EMATRX(I,K)
 214    CONTINUE
 215  CONTINUE
      WRITE(FOUT,2013) (ETOT(K),K=1,8)
C
      RETURN
      END

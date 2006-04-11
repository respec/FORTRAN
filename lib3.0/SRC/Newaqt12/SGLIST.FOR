C
C
C
      SUBROUTINE   SGLIST
     I                    (MESSFL,WDMSFL,PTHNAM,NUMDSN,DSN,
     I                     CSCENM,CLOCNM,CCONNM)
C
C     + + + PURPOSE + + +
C     List information on up to 30 timeseries from a WDM file.
C     The user is asked to specify formats and the optional sums
C     to be output.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,NUMDSN,DSN(NUMDSN)
      CHARACTER*8 PTHNAM(1),CSCENM(NUMDSN),CLOCNM(NUMDSN),CCONNM(NUMDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     PTHNAM - character string of path of options selected to get here
C     NUMDSN - number of data sets in buffer
C     DSN    - array of data-set numbers in buffer
C     CSCENM - Scenario name assoc with data in dataset
C     CLOCNM - Location name assoc with data in dataset
C     CCONNM - Constituent name assoc with data in dataset
C
C     + + + PARAMETERS + + +
      INTEGER     MXNHDR
      PARAMETER  (MXNHDR = 3)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I0,I1,SGRP,SCLU,RESP,NUMHDR,DSNERR,
     $          CNMDSN,LNMDSN,ACT,SSDATE(2),SEDATE(2),DTRAN
      CHARACTER*8  CDSID,TPTHNM(1)
      CHARACTER*1  TITLE(78),HEADR(250,MXNHDR)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QFCLOS, ZSTCMA, QRESP, ZWNSOP, DTACT, DTGET, SGDATE
      EXTERNAL   TSLINI, TSLOUT, TSLSUM, TSLORS, SGLHDR, TSLIGN, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 40
      I0   = 0
      I1   = 1
C
C     init listing parameters, check number of data sets
      CALL TSLINI (MESSFL,SCLU,NUMDSN,
     O             CNMDSN,DSNERR)
      IF (DSNERR.EQ.0) THEN
C       set default time parameters based on selected data sets
        CALL DTACT (I)
        CALL DTGET (I,
     O              ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TUNITS,
     O              TSTEP,DTRAN)
      END IF
C
 50   CONTINUE
C       do main list menu
        IF (DSNERR.EQ.0) THEN
C         ok to do menu
          CALL ZWNSOP (I1,PTHNAM)
          SGRP= 1
          CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        END IF
C
C       allow 'Prev' command
        I= 4
        CALL ZSTCMA (I,I1)
C
        GO TO (100,200,300,400,500,600), RESP
C
 100    CONTINUE
C         time parameters
          SCLU = 64
          TPTHNM(1) = 'AOL'
          CALL SGDATE (MESSFL,SCLU,TPTHNM)
          CALL DTACT (I)
          CALL DTGET (I,
     O                ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TUNITS,
     O                TSTEP,DTRAN)
          SCLU = 40
          GO TO 900
C
 200    CONTINUE
C         get output options
          LNMDSN = CNMDSN
          CALL TSLOUT (MESSFL,SCLU,PTHNAM,NUMDSN,
     M                 CNMDSN)
          IF (CNMDSN.NE.LNMDSN) THEN
C           different data sets, set default time parameters for them
            CALL DTACT (I)
            CALL DTGET (I,
     O                  ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TUNITS,
     O                  TSTEP,DTRAN)
          END IF
          GO TO 900
C
 300    CONTINUE
C         get summary options
          CALL TSLSUM (MESSFL,SCLU,PTHNAM)
          GO TO 900
C
 400    CONTINUE
C         List/Screen timeseries parameters
          CALL TSLORS (MESSFL,SCLU,PTHNAM)
          GO TO 900
C
 500    CONTINUE
C         generate the listing
          CALL ZIPI (MAXD,DTRAN,TRANS)
C         get the headers for the listing
          CALL SGLHDR (CNMDSN,DSN,MXNHDR,
     I                 DTRAN,CSCENM,CLOCNM,CCONNM,
     O                 TITLE,HEADR,NUMHDR)
          CALL TSLIGN (MESSFL,SCLU,PTHNAM,WDMSFL,CNMDSN,DSN,
     I                 TITLE,NUMHDR,HEADR)
          GO TO 900
C
 600    CONTINUE
C         return to timeseries menu
          GO TO 900
C
 900    CONTINUE
C
C       turn off 'Prev' command
        I= 4
        CALL ZSTCMA (I,I0)
C
      IF (RESP.NE.6) GO TO 50
C
      IF (TORF.EQ.2) THEN
        CALL QFCLOS (FUNIT,I0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SGLHDR
     I                   (NUMDSN,DSN,MXNHDR,
     I                    DTRAN,CSCENM,CLOCNM,CCONNM,
     O                    TITLE,HEADR,NUMHDR)
C
C     + + + PURPOSE + + +
C     Build headers for GENSCN time-series listing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NUMDSN,DSN(NUMDSN),MXNHDR,DTRAN,NUMHDR
      CHARACTER*1 TITLE(78),HEADR(250,MXNHDR)
      CHARACTER*8 CSCENM(NUMDSN),CLOCNM(NUMDSN),CCONNM(NUMDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMDSN - number of data sets being listed
C     DSN    - array of data-set numbers being listed
C     MXNHDR - max number of header rows
C     DTRAN  - transformation for data being listed
C     CSCENM - Scenario name assoc with data in dataset
C     CLOCNM - Location name assoc with data in dataset
C     CCONNM - Constituent name assoc with data in dataset
C     TITLE  - title for time-series listing
C     HEADR  - character array of headers
C     NUMHDR - actual number of header rows to be listed
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,I8,ID,ILEN,HLEN,CNTLOC,CNTCON,CNTSEN
      CHARACTER*1  BLNK(1)
      CHARACTER*8  UNISEN(MAXD),UNICON(MAXD),UNILOC(MAXD),CTRAN,CTUNIT
      CHARACTER*21 CLABL
      CHARACTER*78 TITL
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC, STRUNI, CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      I8 = 8
      BLNK(1) = ' '
C
C     init output arguments
      ILEN = 78
      CALL ZIPC (ILEN,BLNK,TITLE)
      ILEN = 250 * MXNHDR
      CALL ZIPC (ILEN,BLNK,HEADR)
      NUMHDR = 0
C
C     build title for list output
      IF (DTRAN.EQ.0) THEN
        CTRAN = 'AVERAGE'
      ELSE IF (DTRAN.EQ.1) THEN
        CTRAN = 'SUMMED '
      ELSE IF (DTRAN.EQ.2) THEN
        CTRAN = 'MAXIMUM'
      ELSE IF (DTRAN.EQ.3) THEN
        CTRAN = 'MINIMUM'
      END IF
      IF (TUNITS.EQ.1) THEN
        CTUNIT = 'SECONDLY'
      ELSE IF (TUNITS.EQ.2) THEN
        CTUNIT = 'MINUTELY'
      ELSE IF (TUNITS.EQ.3) THEN
        CTUNIT = 'HOURLY  '
      ELSE IF (TUNITS.EQ.4) THEN
        CTUNIT = 'DAILY   '
      ELSE IF (TUNITS.EQ.5) THEN
        CTUNIT = 'MONTHLY '
      ELSE IF (TUNITS.EQ.6) THEN
        CTUNIT = 'YEARLY  '
      END IF
      IF (NUMDSN .EQ. 1) THEN
C       only one data set, put all info in title (no headers)
        TITL=' Timeseries List for ' // CTRAN //' '//CTUNIT//' '//
     $         CSCENM(1) // ' ' // CCONNM(1) // ' at ' // CLOCNM(1)
      ELSE
C       count occurances of locations
        CALL STRUNI(NUMDSN,CLOCNM,
     O              CNTLOC,UNILOC)
C       count occurances of constit
        CALL STRUNI(NUMDSN,CCONNM,
     O              CNTCON,UNICON)
C       count occurances of scenarios
        CALL STRUNI(NUMDSN,CSCENM,
     O              CNTSEN,UNISEN)
        IF (CNTCON.EQ.1 .AND. CNTLOC.EQ.1) THEN
C         only scen varies
          TITL=' Timeseries List for ' // CTRAN // ' ' // CTUNIT
     $          // ' ' // CCONNM(1) // ' at ' // CLOCNM(1)
        ELSE IF (CNTSEN.EQ.1 .AND. CNTLOC.EQ.1) THEN
C         only const varies
          TITL=' Timeseries List for ' // CTRAN // ' ' // CTUNIT
     $          // ' ' // CSCENM(1) // ' at ' // CLOCNM(1)
        ELSE IF (CNTSEN.EQ.1 .AND. CNTCON.EQ.1) THEN
C         only locn varies
          TITL=' Timeseries List for ' // CTRAN // ' ' // CTUNIT
     $          // ' ' // CSCENM(1) // ' ' // CCONNM(1)
        ELSE IF (CNTLOC.EQ.1) THEN
C         scen and con vary
          TITL=' Timeseries List for ' // CTRAN // ' ' // CTUNIT
     $          // ' at ' // CLOCNM(1)
        ELSE IF (CNTCON.EQ.1) THEN
C         scen and loc vary
          TITL=' Timeseries List for '// CTRAN // ' ' // CTUNIT
     $          // ' ' //CCONNM(1)
        ELSE IF (CNTSEN.EQ.1) THEN
C         locn and con vary
          TITL=' Timeseries List for '// CTRAN // ' ' // CTUNIT
     $          // ' ' //CSCENM(1)
        ELSE
C         everything varies
          TITL= ' Scenario Generator Timeseries List for ' // CTRAN
     $           // ' ' // CTUNIT // ' Values'
        END IF
C
C       create header lines
        HLEN= (WIDTH-22)/NUMDSN
        IF (HLEN.GT.12) HLEN = 12
        I  = 26 + HLEN*NUMDSN
        IF (I.LT.WIDTH) WIDTH = I
C
        ILEN= 21
        IF (CNTSEN.GT.1) THEN
C         multiple data sets, multiple scenarios, want this header
          NUMHDR = NUMHDR + 1
          CLABL  = ' Scenario            '
          CALL CVARAR (ILEN,CLABL,ILEN,HEADR(1,NUMHDR))
        END IF
        IF (CNTLOC.GT.1) THEN
C         multiple data sets, multiple locations, want this header
          NUMHDR = NUMHDR + 1
          CLABL  = ' Location            '
          CALL CVARAR (ILEN,CLABL,ILEN,HEADR(1,NUMHDR))
        END IF
        IF (CNTCON.GT.1) THEN
C         multiple data sets, multiple constits, want this header
          NUMHDR = NUMHDR + 1
          CLABL  = ' Constituent         '
          CALL CVARAR (ILEN,CLABL,ILEN,HEADR(1,NUMHDR))
        END IF
        IF (HLEN .LT. 8) THEN
          K = HLEN - 1
        ELSE
          K = 8
        END IF
        DO 10 ID = 1,NUMDSN
C         scenario, location, constituent names into headers
          I = 0
          J= 23 + HLEN-K +HLEN*(ID-1)
          IF (CNTSEN.GT.1) THEN
            I= I+ 1
            CALL CVARAR (I8,CSCENM(ID),K,HEADR(J,I))
          END IF
          IF (CNTLOC.GT.1) THEN
            I= I+ 1
            CALL CVARAR (I8,CLOCNM(ID),K,HEADR(J,I))
          END IF
          IF (CNTCON.GT.1) THEN
            I= I+ 1
            CALL CVARAR (I8,CCONNM(ID),K,HEADR(J,I))
          END IF
 10     CONTINUE
      END IF
C
C     put built title in output arg
      ILEN = 78
      CALL CVARAR (ILEN,TITL,ILEN,TITLE)
C
      RETURN
      END

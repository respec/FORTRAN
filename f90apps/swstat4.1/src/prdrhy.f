C
C
C
      SUBROUTINE   PRODHY
     I                    (MESSFL,WDMFL,IGR,
     M                     DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     Control the calculation of duration hydrographs,
C     duration hydrograph tables and plots. 
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL,IGR,DSNCNT,DSNBMX,DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of users WDM file
C     IGR    - graphics available flag
C              1 - graphics available, 2 - graphics not available
C     DSNCNT - number of data sets in the buffer
C     DSNBMX - size of data set buffer
C     DSNBUF - array of data set numbers to be processed
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      SGRP,RESP,RETCOD,AGAIN,
     &             DELFG,FOUT,PSHADE,DEVICE,ARHLOG,
     &             NCI,IPLOT,PRECFG,IRET,
     &             SCLU,NEWFUN,       DTRAN,DTRAN1,
     &             SIGDIG,DECPLA,FLDWID,ITABL,STMO,EDMO,
     &             I,I0,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I12,I64
      REAL         PCTILE(12)
      CHARACTER*8  PTHNAM
      CHARACTER*64 FLNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL     CVARAR, CARVAR
      EXTERNAL     Q1INIT, QSETIB, QSTCOB, QSTCTF, QSETR
      EXTERNAL     Q1EDIT, QGETIB, QGTCOB, QGTCTF, QGETR
      EXTERNAL     QRESP,  PRNTXT, ZSTCMA, ZGTRET
      EXTERNAL     QFOPFN, QFCLOS, GETFUN
      EXTERNAL     PRWMSE
      EXTERNAL     DRNHYD
      EXTERNAL     STFLOP
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
      I3  = 3
      I4  = 4
      I5  = 5
      I8  = 8
      I6  = 6
      I7  = 7
      I9  = 9
      I10 = 10
      I12 = 12
      I64 = 64
      SCLU= 160
      RETCOD = 0
      I = 35
C     set to defaults
      PCTILE(1) = 0.0
      PCTILE(2) = 0.1
      PCTILE(3) = 0.2
      PCTILE(4) = 0.3
      PCTILE(5) = 0.5
      PCTILE(6) = 0.7
      PCTILE(7) = 0.8
      PCTILE(8) = 0.9
      PCTILE(9) = 1.0
      PCTILE(10) = 0.0
      PCTILE(11) = 0.0
      PCTILE(12) = 0.0
      NCI = 9
      SIGDIG = 5
      FLDWID = 8
      DECPLA = 0
      DEVICE = 1
      ITABL  = 1
      ARHLOG = 2
      PSHADE = 2
      STMO   = 10
      EDMO   = 9
      DTRAN1 = 1
      FOUT   = 0
      FLNAME = 'durhyd.out'
C
C     init output optionS
      IF (IGR.EQ.1) THEN
C       init to output graphics
        IPLOT = 1
      ELSE
C       graphics not available
        IPLOT= 2
      END IF
C     init period of record and time step/units flags
      PRECFG= 1
C     init to default number of percentiles
      NCI = 9  
C
      RESP = 1
 10   CONTINUE
C       durhyd: 1-select,2-modify,3-define,4-analyze,5-return
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
C       allow previous
        I= 4
        CALL ZSTCMA (I,I1)
C
        GO TO (100,200,300,400,500), RESP
C
 100    CONTINUE
C         select data sets to analyze
          PTHNAM = 'SD       '
          CALL PRWMSE (MESSFL,WDMFL,DSNBMX, PTHNAM,
     M                 DSNBUF,DSNCNT)
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          RESP = 2
          GO TO 900
C
 200    CONTINUE
C         modify output options
C         precfg(1-full record ea sta,2-common period,3-each station),
C         stmo(1-jan,...12-dec), edmo(1-jan,...12-dec),
C         dtran(1-avg,2-total,3-max,4-min)
C         iplot(1-y,2-n), device(1-screen,2-laser,3-plotter,4-meta),
C                         arhlog(1-a,2-l),pshade(1-y,2-n)
C         itabl(1-y,2-n),
 210      CONTINUE
C           return here on Oops
            AGAIN = 0
            NEWFUN = 0
            SGRP= 6
            CALL Q1INIT (MESSFL, SCLU, SGRP)
C           set defaults
            CALL QSTCTF (I1,I64,FLNAME)
            CALL QSTCOB (I1,I2,PRECFG)
            CALL QSTCOB (I1,I3,STMO)
            CALL QSTCOB (I1,I4,EDMO)
            CALL QSTCOB (I1,I5,DTRAN1)
            CALL QSTCOB (I1,I6,IPLOT)
            CALL QSTCOB (I1,I7,DEVICE)
            CALL QSTCOB (I1,I8,ARHLOG)
            CALL QSTCOB (I1,I9,PSHADE)
            CALL QSTCOB (I1,I10,ITABL)
            CALL QSETIB (I1,I1,SIGDIG)
            CALL QSETIB (I1,I2,DECPLA)
            CALL QSETIB (I1,I3,FLDWID)
            CALL Q1EDIT (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              CALL QGTCOB (I1,I2,PRECFG)
              CALL QGTCOB (I1,I3,STMO)
              CALL QGTCOB (I1,I4,EDMO)
              CALL QGTCOB (I1,I5,DTRAN1)
              CALL QGTCOB (I1,I6,IPLOT)
              CALL QGTCOB (I1,I7,DEVICE)
              CALL QGTCOB (I1,I8,ARHLOG)
              CALL QGTCOB (I1,I9,PSHADE)
              CALL QGTCOB (I1,I10,ITABL)
              CALL QGETIB (I1,I1,SIGDIG)
              CALL QGETIB (I1,I2,DECPLA)
              CALL QGETIB (I1,I3,FLDWID)
              CALL QGTCTF (I1,I64,FLNAME)
C             open file for output, ignore return code for now
              SGRP = 7
              CALL STFLOP ( MESSFL, SCLU, SGRP,
     M                      FOUT, FLNAME,
     O                      RETCOD )
            ELSE IF (IRET .EQ. -1) THEN
C             oops, try again
              AGAIN = 1
            ELSE
C             assume ok and return
C             user wants back to main duration menu
              IRET= 1
            END IF
          IF (AGAIN .EQ. 1) GO TO 210
C
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          IF (IPLOT.EQ.1 .AND. IGR.EQ.2) THEN
C           user wants graphics, but it is not available
            SGRP= 25
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            IPLOT= 2
          END IF
          RESP = 3
          GO TO 900
C
 300    CONTINUE
C         percentiles (PCTILE)     
 310      CONTINUE
 320        CONTINUE
C             back here on previous from interval values screen
              SGRP= 10
              CALL Q1INIT (MESSFL,SCLU,SGRP)
              CALL QSETR (I12,PCTILE)
              CALL Q1EDIT (IRET)
            IF (IRET .EQ. -1) GO TO 320
            IF (IRET .EQ. 1) THEN
C             user to continue
              CALL QGETR (I12,PCTILE)
C             check number of nonzeros after the first one
              I = 1
              NCI = 1
 325          CONTINUE
                I = I + 1
                IF (PCTILE(I) .GT. 0.00001) NCI = I
              IF (PCTILE(I) .GT. 0.00001 .AND. I .LE. 11) GO TO 325
            ELSE
C             user wants back to main Duration menu
              IRET= 1
            END IF
          IF (IRET.EQ.2) GO TO 310
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          RESP = 4
          GO TO 900
C
 400    CONTINUE
C         do analysis
          IF (DSNCNT.GT.0) THEN
C           data sets to analyze
            IF (FOUT .EQ. 0) THEN
C             no output file open, open default
              CALL GETFUN ( I1, FOUT )
              OPEN ( UNIT = FOUT, FILE=FLNAME, STATUS='UNKNOWN' )
            END IF
            DTRAN = DTRAN1 - 1
            CALL DRNHYD (MESSFL,SCLU,WDMFL,DSNCNT,DSNBUF,FOUT,
     I                   IPLOT,ARHLOG,PSHADE,DEVICE,
     I                   ITABL,DECPLA,SIGDIG,FLDWID,
     I                   NCI,PCTILE,PRECFG,STMO,EDMO,
     I                   DTRAN)
          ELSE
C           nothing to analyze
            SGRP= 19
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          RESP = 1
          GO TO 900
C
 500    CONTINUE
C         all done
          GO TO 900
C
 900    CONTINUE
C
C       turn off previous
        I= 4
        CALL ZSTCMA (I,I0)
C
      IF (RESP.NE.5) GO TO 10
C
C     close files
      DELFG = 0
      CALL QFCLOS (FOUT,DELFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DRNHYD
     I                    (MESSFL,SCLU,WDMFL,DSNCNT,DSN,FOUT,
     I                     IPLOT,ARHLOG,PSHADE,DEVICE,
     I                     ITABL,DECPLA,SIGDIG,FLDWID,
     I                     NCI,PCTILE,FCORS,STMO,EDMO,
     I                     DTRAN)
C
C     + + + PURPOSE + + +
C     This routine calculates flow-duration statistics and prints
C     the flow-duration table and calls routine to plot the flow-
C     duration data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,WDMFL,DSNCNT,DSN(DSNCNT),FOUT,
     I             IPLOT,ARHLOG,PSHADE,DEVICE,
     I             ITABL,DECPLA,SIGDIG,FLDWID,
     I             NCI,FCORS,STMO,EDMO,DTRAN
      REAL         PCTILE(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for input direct access file
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers for analysis
C     FOUT   - Fortran unit number of output file
C     IPLOT  - plotting flag, 2- dont plot, 1- plot        
C     ARHLOG - 1-ARITHMETIC Y-AXIS, 2- LOG Y-AXIS
C     PSHADE - 1-no graphic shading between lines, 2-yes
C     DEVICE - 1-screen, 2-laser, 3-plotter, 4-meta
C     ITABL  - 1-PRINT TABLE, 2-NO TABLE
C     DECPLA - decimal places 
C     SIGDIG - significant digits on output
C     FLDWID - field width for percentile results
C     NCI    - number of class intervals
C     PCTILE - percentiles to be computed
C     FCORS  - indicates how to determine period of record
C              1 - use full period of record
C              2 - use common period of record
C              3 - user will specify period for each data set
C     TSTUFG - indicates time interval
C     STMO   - start month (1-12)
C     EDMO   - end month (1-12)
C     DTRAN  - transformation code
C              0 - average
C              1 - sum
C              2 - max
C              3 - min
C
C     + + + PARAMETERS
      INTEGER      MXYR
      PARAMETER    ( MXYR = 150 )
C
C     + + + PARAMETER DEFINTIONS + + +
C     MXYR   - maximum number of years that can be analyzed
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      NMIS(365), NCNT(365), FPOS,
     $             BGNEND(12), SDATIM(6), EDATIM(6), TEMP(12),
     $             TSTEP, TUNITS, NPTS, TEMPX(6), TEMPY(6),
     $             I, J, K, N, I1, I4, I24, I28, I35, I56, I80,
     &             SGRP, ICHK(7), RESP, RETCOD, ERRFLG,
     &             SCNFG, BASE, DEVCOD, IPT, LPYRS, LPDAY,LPYEAR,
     &             DEVCHG, IWAIT, ICLOS, WSID, OOPMES, QFLG
      REAL         FLOW(366), FLDR(MXYR,365), FPCT(365,12),
     $             PCTLE, PCBASE, PCNEXT, FRACT, FMIN, FMAX
      CHARACTER*1  CTITL(24), LTITLE(80), LSTANO(40), PERIOD(35),
     $             BLNK
      CHARACTER*8  WNDNAM(2)
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL     CHRCHR,         ZIPC,   TIMDIF, ASRTRP
      EXTERNAL     QRESP,  PMXTXI, PRNTXT, PRNTXI
      EXTERNAL     WDTGET
      EXTERNAL     ANPRGT, GPDEVC, GPINIT
      EXTERNAL     PSTUPW, PLTONE, PDNPLT, PROPLT
      EXTERNAL     DSINF2, DHPRNT, GRFLDR
      EXTERNAL     DHDTDS, DHDTCM, DHBEGN, DHLEAP, DHINCR
C
C     + + + DATA INITIALIZATIONS   + + +
      DATA CTITL/'D','u','r','a','t','i','o','n',' ',
     1           'h','y','d','r','o','g','r','a','p','h',
     1           ' ','f','o','r',' '/
      DATA LSTANO/'S','t','a','t','i','o','n',' ','i','d', 30*' '/
      DATA ICHK, WNDNAM
     $    / 7*1, 'Modify  ','SDAM  ' /
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I4  = 4
      I24 = 24
      I28 = 28
      I35 = 35
      I56 = 56
      I80 = 80
      QFLG = 30
      TSTEP = 1
      TUNITS= 4
      BLNK= ' '
      ERRFLG= 0
      RETCOD = 0
      OOPMES = 0
C
      FPOS = 0
C
      IF (FCORS.EQ.2) THEN
C       need to get common time period for analysis
        CALL DHDTCM ( MESSFL, WDMFL, DSNCNT, DSN,
     M                FCORS,
     O                BGNEND, ERRFLG )
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       all ok so far
        IF (IPLOT .EQ. 1) THEN
C         initialize common block for graphics; set device type to screen
          CALL GPINIT
C         get device code
          I = 39 + DEVICE
          CALL ANPRGT (I, DEVCOD)
          CALL GPDEVC (DEVICE, DEVCOD)
        END IF
C
        DO 500 N= 1,DSNCNT
C         do analysis for each data set
          IF (RETCOD .LT. 0) THEN
C           had problems on last data set, but ok to try the next one
            RETCOD = 0
          END IF
          IF (RETCOD .EQ. 0) THEN
C           always continue unless user 'Interrupts' analysis
C           set period of record for analysis
            CALL DHDTDS ( MESSFL, WDMFL, DSN(N), BGNEND, FCORS,
     M                    SDATIM, EDATIM,
     O                    RETCOD )
C           initialize title and get available info from data set
            CALL ZIPC   ( I80, BLNK,  LTITLE)
            CALL CHRCHR ( I24, CTITL, LTITLE)
            CALL ZIPC   ( I28, BLNK,  LSTANO(13) )
            CALL DSINF2 ( WDMFL, DSN(N), I56, I28, I35,
     I                    SDATIM, EDATIM,
     M                    LTITLE(25), LSTANO(13), PERIOD )
          END IF
C
          IF (RETCOD .EQ. 0) THEN
C           ok so far, initialize arrays and counters
            DO 130 J = 1,365
              NCNT(J) = 0
              NMIS(J) = 0
              FLOW(J) = -999.
              DO 120 I = 1, MXYR
                FLDR(I,J) = 0.0
 120          CONTINUE
 130        CONTINUE
C
C           initialize dates and pointers for first retrieval
C           temp(1-6) - start date for retrieval
C           temp(7-12) - end date for retrieval
C           tempx(1-6) - date for begin of first season
C           tempy(1-6) - date for begin of last season
C           get period in 1st season, start dates of 1st and last season
            CALL DHBEGN ( SDATIM, EDATIM, STMO, EDMO,
     O                    TEMP, TEMPX, TEMPY, IPT )
C           find leap day and index to first leap year
            CALL DHLEAP ( TEMPX, STMO, EDMO,
     O                    LPDAY, LPYEAR )
            LPYRS = 4 - LPYEAR
C
 200        CONTINUE
C             retrieve data
              CALL TIMDIF ( TEMP(1), TEMP(7), TUNITS, TSTEP, NPTS )
              CALL WDTGET ( WDMFL, DSN(N), TSTEP, TEMP(1),
     I                      NPTS, DTRAN, QFLG, TUNITS,
     O                      FLOW(IPT), RETCOD )
              NPTS = NPTS + IPT - 1
              IPT = 1
              IF (RETCOD .NE. 0) THEN
C               error reading file, error code &
                SGRP = 24
                CALL PRNTXI (MESSFL,SCLU,SGRP,RETCOD)
              ELSE
C               looks ok, keep going
                IF (LPYRS .EQ. 4  .OR.  LPYRS .EQ. 0) THEN
C                 leap year, send status report
                  SGRP = 23

                  CALL PMXTXI (MESSFL,SCLU,SGRP,I4,SCNFG,I1,I1,
     &                         TEMP(1))
                  SCNFG = -1
                  IF (LPDAY .GT. 0  .AND.  LPDAY .LE. NPTS) THEN
C                   leap day needs to be removed
                    NPTS = NPTS - 1
                    DO 230 J = LPDAY, NPTS
                      FLOW(J) = FLOW(J+1)
 230                CONTINUE
                    FLOW(NPTS+1) = -999.
                  END IF
                  LPYRS = 1
                ELSE
C                 don't need status or to remove leap day
                  LPYRS = LPYRS + 1
                END IF
C               begin loop to fill array
                DO 240 J = 1, NPTS
                  IF (FLOW(J) .GE. 0.0) THEN
                    NCNT(J) = NCNT(J) + 1
                    K = NCNT(J)
                    FLDR(K,J) = FLOW(J)
                  ELSE
C                   count missing values
                    NMIS(J) = NMIS(J) + 1
                  END IF
 240            CONTINUE
C               adjust dates for next year of data
                CALL DHINCR ( TEMPX, TEMPY, SDATIM, EDATIM,
     O                        TEMP, ERRFLG )
                IPT = 1
              END IF
            IF (ERRFLG.LT.0 .AND. RETCOD.EQ.0) GO TO 200
C
            FMIN = 1.0E9
            FMAX = -1.0E9
            IF (RETCOD.EQ.0) THEN
C             still ok
              DO 300 I = 1,365
C               sort the flows
                CALL ASRTRP (NCNT(I), FLDR(1,I))
                DO 280 J = 1,NCI
                  IF (NCNT(I) .LT. 2) THEN
C                   no data, or only 1 good value
                    FPCT(I,J) = -999.0
                  ELSE
C                   compute the percentiles
                    IF (PCTILE(J) .LT. 0.00001) THEN
C                     must be 0, use maximum
                      FPCT(I,J) = FLDR(NCNT(I),I)
                    ELSE IF (PCTILE(J) .GT. 0.9999) THEN
C                     must be 1.0, use minimum
                      FPCT(I,J) = FLDR(1,I)
                    ELSE
C                     compute via linear interpolation
C                     get base location in sorted array for the
C                     percentile (flip-flop percentiles)
                      PCTLE = 1.0 - PCTILE(J)
                      BASE = INT(PCTLE*REAL(NCNT(I)+1))
                      IF (BASE .LT. 1 .OR. BASE .GE. NCNT(I)) THEN
C                       percentile to close to edge for number of points
                        FPCT(I,J) = -999.
                        OOPMES = 1
                      ELSE
C                       get percentile of base
                        PCBASE = REAL(BASE)/REAL(NCNT(I)+1)
C                       get percentile of next larger flow
                        PCNEXT = REAL(BASE+1)/REAL(NCNT(I)+1)
C                       compute fraction on the increment
                        FRACT = (PCTLE-PCBASE)/(PCNEXT-PCBASE)
                        FPCT(I,J) = FLDR(BASE,I)
     $                            + FRACT*(FLDR(BASE+1,I)-FLDR(BASE,I))
                      END IF
                    END IF
                  END IF
C                 find min and max all days all precentiles
                  IF (FPCT(I,J) .LT. FMIN  .AND.
     $                FPCT(I,J) .GE. 0.0)  FMIN = FPCT(I,J)
                  IF (FPCT(I,J) .GT. FMAX) FMAX = FPCT(I,J)
 280            CONTINUE
 300          CONTINUE
C
              IF (OOPMES .EQ. 1) THEN
C               write screen
                SGRP = 26      
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                OOPMES = 0
              END IF
C
              IF (ITABL .EQ. 1) THEN
C               output results to file
                CALL DHPRNT ( FOUT, STMO, EDMO, FPOS,
     I                        DECPLA, SIGDIG, FLDWID,
     I                        LTITLE, LSTANO, PERIOD,
     I                        NCI, PCTILE, FPCT, NCNT, NMIS )
                IF (IPLOT .EQ. 2) THEN
C               no plot, finished computations and table message
                  SGRP = 21
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                END IF
              END IF
C
              IF (IPLOT.EQ.1     .AND.
     $            FMAX .GT. 0.0  .AND.  FMIN .LT. 1.0E8) THEN
C               plotting, set defaults, specs and data
                CALL GRFLDR (PSHADE, LTITLE, PERIOD, NCI,ARHLOG,
     I                       FPCT,PCTILE,FMIN,FMAX,STMO,EDMO,FPOS)
                WSID = 1
 400            CONTINUE
C                 do plotting menu
                  IF (ITABL .EQ. 1) THEN
C                   include message about output file
                    SGRP= 30
                  ELSE
C                   omit message about output file
                    SGRP = 31
                  END IF
                  CALL QRESP (MESSFL,SCLU,SGRP,RESP)
                  IF (RESP.EQ.1) THEN
C                   modify stuff
                    CALL PROPLT (MESSFL,ICHK,WNDNAM,DEVCHG)
                    IF (DEVCHG .EQ. 1) THEN
C                     close current workstation and reset device type
C                     and code
                      IWAIT = 0
                      ICLOS = 1
                      CALL PDNPLT (WSID,ICLOS,IWAIT)
                    END IF
                  ELSE IF (RESP.EQ.2) THEN
C                   generate the plot
                    IWAIT = 0
                    ICLOS = 0
                    CALL PSTUPW (WSID, RETCOD)
                    CALL PLTONE
                    CALL PDNPLT (WSID,ICLOS,IWAIT)
                  END IF
                IF (RESP.NE.3) GO TO 400
C               close workstations
                ICLOS = 1
                IWAIT = 0
                CALL PDNPLT (WSID,ICLOS,IWAIT)
              ELSE IF (IPLOT .EQ. 1) THEN
C               no good data, don't plot
                SGRP = 18
                CALL PRNTXT ( MESSFL, SCLU, SGRP )
              END IF
            END IF
          END IF
 500    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GRFLDR
     I                    (PSHADE, LTITLE, PERIOD, NCI,ARHLOG,
     I                     FPCT,PCTILE,FMIN,FMAX,STMO,EDMO,FPOS)
C
C     + + + PURPOSE + + +
C     This routine fills the common block CPLOT to plot a flow
C     duration curve.
C
C     + + + HISTORY + + +
C     kmf 00/05/01 - corrected dimensions for ymin & ymax from 2 to 12
C
C     + + + DUMMY VARIABLES + + +
      INTEGER     NCI, PSHADE,
     &            ARHLOG,STMO,EDMO,FPOS
      REAL        FPCT(365,12),PCTILE(12),FMIN,FMAX
      CHARACTER*1 LTITLE(80), PERIOD(35)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PSHADE - 1 - no shading, 2 - use shading between lines
C     LTITLE - title generated from data set station id or tstype
C     PERIOD - title generated from period of record
C     NCI    - number of class intervals
C     ARHLOG - 1 - arithmetic axis, 2 - logarithmetic axis
C     FPCT   - array of values to plot
C     PCTILE - percentiles selected by user
C     FMIN   - minimum FLDR value
C     FMAX   - maximum FLDR value
C     STMO   - starting month
C     ENMO   - ending month
C     FPOS   - position in array where season starts
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I35,I80,WHICH(12),    
     $            BVALFG(4),RETCOD,TICS(4),YTYPE(2),XTYPE,
     $            I240, LNTYP(12), COLOR(12), PATRN(12), SYMBL(12),
     $            TRANSF(12),LEN,LOC,NPTS,C1(12),I0,I2,
     $            SDATE(6,12),EDATE(6,12),TUNITS(12),
     $            MEAN(12),TSTEP(12),SIGD,DECP,I109,BFLG,CHRPLN,
     $            NUMCHR,I5,LOC2,CTYPE(12)
      REAL        PLMN(4), PLMX(4), YMIN(12), YMAX(12),
     $            ALEN,LOCLGD(2),XPAG,YPAG,XLEN,YLEN,XORG,YORG,
     $            SIZEL,XP,YP
      CHARACTER*1 TITL(240),YLABL(80),YXLABL(80),
     $            ALAB(80), LBV(20,12), BLNK, CTEXT(120)
C
C     + + + FUNCTIONS + + +
      INTEGER     DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, DECCHX, COPYI,  ZIPI,   ZIPR,   ZIPC
      EXTERNAL    TIMDIF, DAYMON
      EXTERNAL    GPVAR,  GPLABL, GPSCLE, GPDATR, GPNCRV,  GPCURV
      EXTERNAL    GPTIME, GPWCTM, GPLEDG, GPFTXT, SCALIT
      EXTERNAL    GPNTXT, GPCTXT, GPSIZE 
C
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   XPAG, YPAG, XLEN, YLEN,  XORG, YORG, SIZEL
     $     /  8.0,  6.0,  6.0,  4.0,   1.5,  1.5,  0.1/
      DATA  LNTYP, SYMBL, PATRN, CTYPE, MEAN, TUNITS, TSTEP
     $     / 12*1,  12*0,  12*2,  12*1, 12*1,   12*4,  12*1 /
      DATA  SDATE, EDATE, C1
     $     / 72*0,  72*0, 1,2,3,4,5,6,7,6,5,4,3,2 /
      DATA   ALAB,   YXLABL, BLNK, LOCLGD
     $     / 80*' ', 80*' ', ' ',  -2.0,-2.0 /
      DATA YLABL/'S','T','R','E','A','M','F','L','O','W',' ','I','N',
     $           ' ','C','U','B','I','C',' ','F','E','E','T',' ',
     $           'P','E','R',' ','S','E','C','O','N','D',45*' '/
      DATA CTEXT/'P','e','r','c','e,','n','t','i','l','e','s',
     $           109*' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
      I5  = 5
      i35 = 35
      I80 = 80
      I109 = 109
      I240 = 240
C
      CALL GPNCRV (NCI,NCI)   
C
C     set up plot title
      CALL ZIPC (I240,BLNK,TITL)
      CALL CHRCHR (I80,LTITLE,TITL)
      TITL(81) = '&'
      CALL CHRCHR (I35,PERIOD,TITL(82))
C
C     set values so no legend is produced
      CALL GPLEDG (LOCLGD)
C
      XTYPE = 0
      YTYPE(1) = ARHLOG
      YTYPE(2) = 0
      ALEN = 0.0
C     set variable labels
      CALL GPLABL (XTYPE,YTYPE,ALEN,YLABL,YXLABL,ALAB,TITL)
C
      IF (EDMO .GT. STMO) THEN
C       end month in same year
        DO 20 I = 1,NCI
          SDATE(1,I) = 2101
          EDATE(1,I) = 2101
          SDATE(2,I) = STMO
          EDATE(2,I) = EDMO
          SDATE(3,I) = 1
          EDATE(3,I) = DAYMON(2101,EDMO)
          EDATE(4,I) = 24
 20     CONTINUE
      ELSE
C       end month in next year
        DO 30 I = 1,NCI
          SDATE(1,I) = 2101 
          EDATE(1,I) = 2102 
          SDATE(2,I) = STMO
          EDATE(2,I) = EDMO
          SDATE(3,I) = 1
          EDATE(3,I) = DAYMON(2102,EDMO)
          EDATE(4,I) = 24
 30     CONTINUE    
      END IF
      CALL TIMDIF (SDATE,EDATE,TUNITS,TSTEP,NPTS)
      CALL GPTIME (TSTEP,TUNITS,SDATE,EDATE,MEAN)
C
      NUMCHR = 108
      CHRPLN = 36
      CALL GPNTXT (NUMCHR,CHRPLN)
      CALL ZIPC (I109,BLNK,CTEXT(12))
C
      LEN = 20
      LOC = 1
      SIGD = 3
      DECP = 3
      LOC2 = 37
      BFLG = 0    
      DO 15 I = 1,NCI
C       set which axis for each variable
        WHICH(I) = 1
C       set which variable for which curve
        CALL GPWCTM (I,I)
        TRANSF(I) = ARHLOG
        CALL GPDATR (I,LOC,NPTS,FPCT(FPOS+1,I),RETCOD)
        LOC = LOC + NPTS
        CALL DECCHX (PCTILE(I),LEN,SIGD,DECP,LBV(1,I))
C       also add to extra text to be plotted
        CALL DECCHX (PCTILE(I),I5,I2,I2,CTEXT(LOC2))
        IF ((NCI+1)/2 .EQ. I .AND. BFLG .EQ. 0) THEN
C         change LOC2 base to go to next line
          BFLG = 1     
          LOC2 = 73 - 5
        END IF
        LOC2 = LOC2 + 5  
 15   CONTINUE
C     set plot size
      CALL GPSIZE (SIZEL,XPAG,YPAG,XORG,YORG,XLEN,YLEN,ALEN)
C     extra text to be plotted and position of text
      CALL GPCTXT (CTEXT)
      XP = (XLEN - 25.0*SIZEL)/XLEN
      YP = (YLEN - SIZEL)/YLEN 
      CALL GPFTXT (XP,YP)
C
      CALL ZIPR (NCI,FMIN,YMIN)
      CALL ZIPR (NCI,FMAX,YMAX)
      CALL GPVAR (YMIN,YMAX,WHICH,TRANSF,LBV)
C
C     set curve type
      IF (PSHADE .EQ. 1) THEN
C       user wants color fill
        CALL COPYI (NCI,C1,COLOR)
        CALL ZIPI  (NCI,I2,PATRN)
C       set last color to background
        COLOR(NCI) = 0
      ELSE
C       user just wants lines
        CALL ZIPI (NCI,I1,COLOR)
        CALL ZIPI (NCI,I0,PATRN)
      END IF
      CALL GPCURV (CTYPE,LNTYP,SYMBL,COLOR,PATRN,LBV)
C
C     generate axis mins and maxs
      CALL SCALIT (YTYPE(1),FMIN,FMAX,        
     O             PLMN(1),PLMX(1))
      BVALFG(1)= 1
      BVALFG(2)= 1
      BVALFG(3)= 4
      BVALFG(4)= 4
      TICS(1) = 10
      CALL GPSCLE (PLMN,PLMX,TICS,BVALFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DHDTCM
     I                   ( MESSFL, WDMFL, DSNCNT, DSN,
     M                     FCORS,
     O                     BGNEND, RETCOD )
C
C     + + + PURPOSE + + +
C     Gets common time period to be used for analysis
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, DSNCNT, DSN(DSNCNT), FCORS,
     $          BGNEND(12), RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     WDMFL  - Fortran unit number of the wdm file
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers for analysis
C     FCORS  - indicator flag for time period to be used
C              1 - full available period for data set
C              2 - period common to other data sets being processed
C              3 - user-specified time period
C     bgnend - array containing common period to be used for analysis
C              (1-6) begin date
C              (7-12) end date
C     RETCOD - return code
C              0 - everything is ok
C              -1 - unresolved problems with dates
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   AGAIN, CORM, SCLU, SGRP, RESP, RETC, RTCMND, TMPTIM(12),
     $          IVAL(6), I0, I1, I2, I3, I4, I6, I7, I10, I12
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKDATE, COPYI
      EXTERNAL   Q1INIT, Q1EDIT, QSETI, QSETIB, QGETIB, QRESP, ZSTCMA
      EXTERNAL   WTDATE
      EXTERNAL   DBNDRY
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SCLU, I0, I1, I2, I3, I4, I6, I7, I10, I12
     $     / 160,  0,  1,  2,  3,  4,  6,  7,  10,  12 /
C
C     + + + END SPECIFICATIONS + + +
C
C     get common time period for all selected data sets
      CORM = 1
      CALL WTDATE ( WDMFL, DSNCNT, DSN, CORM,
     O              BGNEND(1), BGNEND(7), RETCOD )
      IF (RETCOD .NE. 0) THEN
C       no common period: 1-default to full,2-specify,3-return
        SGRP = 13
        RESP = 1
        CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
        IF (RESP .EQ. 1) THEN
C         use full period
          FCORS  = 1
          RETCOD = 0
        ELSE IF (RESP .EQ. 2) THEN
C         specify period
          FCORS  = 3
          RETCOD = 0
        ELSE
C         assume give it up
          RETCOD = -1
        END IF
      ELSE
C       common time period found, force dates to day boundary
        CALL DBNDRY ( I1, BGNEND(1) )
        CALL DBNDRY ( I2, BGNEND(7) )
        CALL COPYI  ( I3, BGNEND(1), IVAL(1) )
        CALL COPYI  ( I3, BGNEND(7), IVAL(4) )
        CALL COPYI  ( I12, BGNEND, TMPTIM )
C       allow previous
        CALL ZSTCMA (I4,I1)
 100    CONTINUE
C         what common time period
          AGAIN = 0
          SGRP  = 11
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
          CALL QSETI  ( I6, IVAL )
          CALL Q1EDIT ( RTCMND )
          IF (RTCMND .EQ. 1) THEN
C           user wants to contine, get dates and check order
            CALL QGETIB ( I3, I1, TMPTIM(1) )
            CALL QGETIB ( I3, I4, TMPTIM(7) )
            CALL CKDATE ( TMPTIM(1), TMPTIM(7), RETC )
            IF (RETC .GE. 0) THEN
C             dates same or start date after end date
 150          CONTINUE
                AGAIN = 0
                SGRP = 12
                CALL Q1INIT ( MESSFL, SCLU, SGRP )
                CALL QSETI  ( I6, IVAL )
                CALL QSETIB ( I3, I7,  BGNEND(1) )
                CALL QSETIB ( I3, I10, BGNEND(7) )
                CALL Q1EDIT ( RTCMND )
                IF (RTCMND .EQ. 1) THEN
C                 try again on dates
                  CALL QGETIB ( I3, I1, TMPTIM(1) )
                  CALL QGETIB ( I3, I4, TMPTIM(7) )
                  CALL CKDATE ( TMPTIM(1), TMPTIM(7), RETC )
                  IF (RETC .GE. 0) THEN
C                   still problem with dates
                    AGAIN = 1
                  ELSE
C                   dates look ok, save them
                    CALL COPYI ( I12, TMPTIM, BGNEND )
                    RETCOD = 0
                  END IF
                ELSE IF (RTCMND .EQ. -1) THEN
C                 oops, try again
                  AGAIN = 1
                ELSE
C                 assume previous and original dates ok
                  RETCOD = 0
                END IF
              IF (AGAIN .EQ. 1) GO TO 150
            ELSE
C             dates look ok, save them
              CALL COPYI ( I12, TMPTIM, BGNEND )
              RETCOD = 0
            END IF
          ELSE IF (RTCMND .EQ. -1) THEN
C           oops, try again
            AGAIN = 1
          ELSE
C           assume previous and dates are ok
            RETCOD = 0
          END IF
        IF (AGAIN .EQ. 1) GO TO 100
C         turn off previous
          CALL ZSTCMA (I4,I0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DHDTDS
     I                   ( MESSFL,  WDMFL, DSN, DATCMN, FCORS,
     M                     SDATIM, EDATIM, 
     O                     RETCOD )
C
C     + + + PURPOSE + + +
C     Determine period of record to be used
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, DSN, DATCMN(12), FCORS,
     $          SDATIM(6), EDATIM(6), RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     WDMFL  - Fortran unit number of the wdm file
C     DSN    - number of the data set
C     DATCMN - common time period
C     FCORS  - indicator flag for time period to be used
C              1 - full available period for data set
C              2 - period common to other data sets being processed
C              3 - user-specified time period
C     SDATIM - begin date to be used
C     EDATIM - end date to be used
C     RETCOD - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   AGAIN, GPFLG, INUM, SCLU, SGRP, SGRP2, RETC, RTCMND,
     $          TDSFRC, IVAL(7), BGNEND(12), I1, I2, I3, I4, I6
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKDATE, COPYI
      EXTERNAL   Q1INIT, Q1EDIT, QSETI, QGETIB, PRNTXI, PRNTXT
      EXTERNAL   WTFNDT
      EXTERNAL   DBNDRY
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SCLU, I1, I2, I3, I4, I6, GPFLG
     $     /  160,  1,  2,  3,  4,  6,     1 /
C
C     + + + END SPECIFICATIONS + + +
C
C     get data set dates
      CALL WTFNDT ( WDMFL, DSN, GPFLG,
     O              TDSFRC, BGNEND(1), BGNEND(7), RETC )
      IF (RETC .NE. 0) THEN
C       empty data set (-6) or wrong data-set type (-82) 
        SGRP = 22
        CALL PRNTXI ( MESSFL, SCLU, SGRP, DSN )
        RETCOD = -1
      ELSE IF (FCORS .EQ. 2) THEN
C       using common time period
        CALL COPYI ( I6, DATCMN(1), SDATIM )
        CALL COPYI ( I6, DATCMN(7), EDATIM )
        RETCOD = 0
      ELSE
C       using full available (1) or user specified (3) time period
        CALL COPYI ( I6, BGNEND(1), SDATIM )
        CALL COPYI ( I6, BGNEND(7), EDATIM )
C       may not be daily data, force dates to daily time boundary
        CALL DBNDRY ( I1, SDATIM )
        CALL DBNDRY ( I2, EDATIM )
        IF (FCORS .EQ. 3) THEN
C         user will be specifying the dates, initialize to full period
          SGRP = 14
          INUM = 7
          CALL COPYI ( I3, SDATIM, IVAL )
          CALL COPYI ( I3, EDATIM, IVAL(4) )
          IVAL(7) = DSN
 100      CONTINUE
C           get the dates
            AGAIN = 0
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL QSETI  ( INUM, IVAL )
            CALL Q1EDIT ( RTCMND )
            IF (RTCMND .EQ. 1) THEN
C             user wants to continue, get dates
              CALL QGETIB ( I3, I1, SDATIM )
              CALL QGETIB ( I3, I4, EDATIM )
C             check  order of dates
              CALL CKDATE ( SDATIM, EDATIM, RETC )
              IF (RETC .GT. 0) THEN
C               invalid dates
                SGRP2 = 17
                CALL PRNTXT ( MESSFL, SCLU, SGRP2 )
                AGAIN = 1
C               set dates to full in case user exits
                CALL COPYI ( I3, IVAL(1), SDATIM )
                CALL COPYI ( I3, IVAL(4), EDATIM )
              ELSE
C               assume dates ok
                RETCOD = 0
              END IF
            ELSE IF (RTCMND .EQ. -1) THEN
C             oops, try again
              AGAIN = -1
            ELSE IF (RTCMND .EQ. 2) THEN
C             assume Prev (2) and dates are ok
              RETCOD = 0
            END IF
          IF (AGAIN .EQ. 1) GO TO 100
        ELSE
C         assume full period, all ok
          RETCOD = 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DHPRNT
     I                   ( FOUT, STMO, EDMO, FPOS,
     I                     DECPLA, SIGDIG, FLDWID,
     I                     LTITLE, LSTANO, PERIOD,
     I                     NCI, PCTILE, FPCT, NCNT, NMIS )
C
C     + + + PURPOSE + + +
C     Outputs results from duration hydrograph analysis to a file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FOUT, STMO, EDMO, FPOS, DECPLA, SIGDIG, FLDWID,
     $            NCI, NCNT(365), NMIS(365)
      REAL        PCTILE(12), FPCT(365,12)
      CHARACTER*1 LTITLE(80), LSTANO(40), PERIOD(35)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FOUT   - Fortran unit number of output file
C     STMO   - month of starting season
C     EDMO   - month of ending season
C     FPOS   - pointer to position in arrays where data starts
C     DECPLA - decimal places
C     SIGDIG - significant digits on output
C     FLDWID - field width for percentile results
C     LTITLE - title generated from data set station id or tstype
C     LSTANO - title generated form station id or tstype
C     PERIOD - title generated from period of analysis
C     NCI    - number of class intervals
C     PCTILE - percentiles selected by user
C     FPCT   - array of values to plot
C     NCNT   - array of number of values used in computations for each day
C     NMIS   - array of number of invalid values skipped for each day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SDAY, MO, OMO, DPM, WLEN, H1SKIP, HLEN, KMISS,
     $             LEN, LOC, T1LEN, T1WID, T2LEN, T2WID, TBLWID,
     $             I, J, K, I1, I2, I3, I5, I12, I35, I132
      REAL         ZERO, MEAN(12)
      CHARACTER*1  TITLE(132,2), HEAD(132), WHEN(132), OBUF(132),
     $             BLNK, CPCT(10)
      CHARACTER*3  CMIN, CMAX
      CHARACTER*4  CMON(12)
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMON, LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC  REAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON, LENSTR, COPYC, ZIPC, ZIPR, CVARAR, DECCHX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  T1WID, T2WID, I1, I2, I3, I5, I12, I35, I132, ZERO, BLNK
     $     /   80,    40,  1,  2,  3,  5,  12,  35,  132,  0.0, ' ' /
      DATA   CMIN,  CMAX, CPCT
     $     /'Min', 'Max', 'P','e','r','c','e','n','t','i','l','e'/
      DATA   CMON
     $     /'Jan ', 'Feb ', 'Mar ', 'Apr ', 'May ', 'June',
     $      'July', 'Aug ', 'Sept', 'Oct ', 'Nov ', 'Dec ' /
C
C     + + + OUTPUT FORMATS   + + +
 2000 FORMAT ( '1' )
 2001 FORMAT ( 132A1 )
 2002 FORMAT (/, '      Num', 123A1 )
 2003 FORMAT (   ' ', A4, ' yrs', 123A1 )
 2004 FORMAT (   ' ' )
 2021 FORMAT (   I4, I5, 1X, 123A1 )
 2031 FORMAT (/, '    Mean  ', 123A1 )
 2041 FORMAT (/, I7, ' values were tagged missing and excluded',
     &               ' from analysis.')
C
C     + + + END SPECIFICATIONS + + +
C
      KMISS = 0
C
C     build title
      CALL ZIPC ( I132, BLNK, TITLE(1,1) )
      CALL ZIPC ( I132, BLNK, TITLE(1,2) )
      TBLWID = 10 + NCI * FLDWID
      LEN = LENSTR ( T1WID, LTITLE )
      LOC    = (TBLWID - LEN) / 2
      IF (LOC .LT. 2) LOC = 2
      CALL COPYC ( LEN, LTITLE, TITLE(LOC,1) )
      T1LEN = LENSTR ( I132, TITLE(1,1) )
      LEN = LENSTR ( T2WID, LSTANO )
      IF (LEN .GT. 12) THEN
C       include 2nd line in title
        LOC = (TBLWID - LEN) / 2
        IF (LOC .LT. 2) LOC = 2
        CALL COPYC ( LEN, LSTANO, TITLE(LOC,2) )
        T2LEN = LENSTR ( I132, TITLE(1,2) )
      ELSE
C       no 2nd line in title
        T2LEN = 0
      END IF
C
C     build period of analysis
      CALL ZIPC ( I132, BLNK, WHEN )
      LOC = (TBLWID - I35) / 2
      CALL COPYC ( I35, PERIOD, WHEN(LOC) )
      WLEN = LENSTR ( I132, WHEN )
C
C     build column headers
      CALL ZIPC ( I132, BLNK, HEAD )
      H1SKIP = (NCI * FLDWID - 10) / 2
      LOC = 1
      DO 50 I = 1, NCI
C       add percentiles to 2nd row of heading
        IF (PCTILE(I) .LE. 0.0001) THEN
          CALL CVARAR (I3, CMAX, I3, HEAD(LOC+FLDWID-3))
        ELSE IF (PCTILE(I) .GE. 0.9999) THEN
          CALL CVARAR (I3, CMIN, I3, HEAD(LOC+FLDWID-3))
        ELSE
           CALL DECCHX (PCTILE(I), FLDWID, I5, I2, HEAD(LOC))
        END IF
        LOC = LOC + FLDWID
 50   CONTINUE
      HLEN = LENSTR ( I132, HEAD )
C
      SDAY = FPOS 
      MO = STMO
 100  CONTINUE
C       begin loop for months, write title
        WRITE (FOUT,2000)
        WRITE (FOUT,2001) (TITLE(K,1), K = 1, T1LEN)
        IF (T2LEN .GT. 0) THEN
C         include 2nd line of title
          WRITE (FOUT,2001) (TITLE(K,2), K = 1, T2LEN)
        END IF
C       write period of analysis
        WRITE (FOUT,2001) (WHEN(I), I = 1, WLEN)
C       write column headings
        WRITE (FOUT,2002) (BLNK, K = 1, H1SKIP), CPCT
        WRITE (FOUT,2003) CMON(MO), (HEAD(I), I = 1, HLEN)
        WRITE (FOUT,2004)
C
C       write daily values
        DPM = DAYMON ( I1, MO )
        CALL ZIPR (I12, ZERO, MEAN)
        DO 240 I = 1, DPM
C         add day to line
          SDAY = SDAY + 1
          CALL ZIPC ( I132, BLNK, OBUF)
          LOC = 1
          DO 220 J = 1, NCI
C           add each percentile to line
            IF (FPCT(SDAY,J) .LT. 0.0) THEN
              OBUF(LOC+FLDWID-3) = '-'
              OBUF(LOC+FLDWID-2) = '-'
              MEAN(J) = -1.0E20
            ELSE
              CALL DECCHX (FPCT(SDAY,J),FLDWID,SIGDIG,DECPLA,
     $                     OBUF(LOC))
              MEAN(J) = MEAN(J) + FPCT(SDAY,J)
            END IF
            LOC = LOC + FLDWID
 220      CONTINUE
C         write line
          LOC = LOC - 1
          WRITE (FOUT,2021) I, NCNT(SDAY),
     $                      (OBUF(K), K = 1, LOC)
C         count missing
          KMISS = KMISS + NMIS(SDAY)
 240    CONTINUE
C
C       write monthly means
        CALL ZIPC (I132, BLNK, OBUF)
        LOC = 1
        DO 320 J = 1,NCI
C         calculate mean for each percentile
          IF (MEAN(J) .LT. 0.0) THEN
C           missing
            OBUF(LOC+FLDWID-3) = '-'
            OBUF(LOC+FLDWID-2) = '-'
          ELSE
C           looks ok
            MEAN(J) = MEAN(J) / REAL( DPM )
            CALL DECCHX (MEAN(J),FLDWID,SIGDIG,DECPLA,
     $                   OBUF(LOC))
          END IF
          LOC = LOC + FLDWID
 320    CONTINUE
        LOC = LOC - 1
        WRITE (FOUT,2031) (OBUF(K), K = 1, LOC)
C                  
C       increment month
        OMO = MO
        MO = MO + 1
        IF (MO .GT. 12) MO = 1
      IF (OMO .NE. EDMO) GO TO 100
C
      IF (KMISS .GT. 0) THEN
C       write number of missing values encountered
        WRITE (FOUT,2041) KMISS
      END IF
C
      RETURN
      END

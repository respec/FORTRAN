C
C
C
      SUBROUTINE   NDHILO
     I                    (MESSFL,WDMFL,
     M                     DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     This routine gets options from the user and determines n-day
C     high and low flow for each year between a start and end date.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL,DSNCNT,DSNBMX,DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of users WDM file
C     DSNCNT - number of data sets in the buffer
C     DSNBMX - size of data set buffer
C     DSNBUF - array of data set numbers to be processed
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,N,L0,L1, SCLU,SGRP,LORH(2),
     $             FORC,RESP,SORM, DSNO,
     $             MONS(2,3),DAYS(2,2),
     $             POPT,DECPLA,EYR,SYR,FOUT,DOPT,
     $             SIGDIG,TSTUFG,
     $             WIDE,LHDUR(MXDUR,2),NDLH(2),LH,
     $             PREV, ON, OFF, RETC, CONT
      CHARACTER*8  PTHNAM
      CHARACTER*64 FLNAME
C
C     + + + LOCAL DEFINITIONS + + +
C     LORH   - flag for high and/or low flow computations
C              (1) - low flows
C                    1 - calculate, 2 - don't calculate
C              (2) - high flows
C                    1 - calculate, 2 - don't calculate
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETFUN, QFCLOS, PRNTXT, QRESP, ZSTCMA, PRWMSE
      EXTERNAL     CKTUNI, NDPERD, NDDEFN, NDOTOP, NDMONS, NDSEAS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   L0, L1, PREV, ON, OFF
     $     /  0,  1,    4,  1,   0 /
      DATA LHDUR /1,2,3,7,10,30,60,90,183,365,
     $            1,2,3,7,10,30,60,90,183,365/
      DATA MONS / 4,  3, 10,  9, 1, 12 /,
     $     DAYS / 1, 31,  1, 30 /
C
C     + + + debug formats + + +
C
C3100 format (//,' <--  Select: dcnt =', i5,
C    $         /,'              dsnb =', 10i5,
C    $      /, ( '                    ', 10i5 ) )
C3200 format (//,' <--  Modify: popt =', i5,
C    $         /,'              dopt =', i5,
C    $         /,'              dsno =', i5,
C    $         /,'            decpla =', i5,
C    $         /,'            sigdig =', i5,
C    $         /,'              wide =', i5,
C    $         /,'              fout =', i5 )
C3300 format (//,' <--  Define: lorh =', 2i5,
C    $         /,'              sorm =', i5,
C    $         /,'              mons =', 6i5,
C    $         /,'              days =', 4i5,
C    $         /,'             lhdur =', 10i5,
C    $         /,'                    ', 10i5,
C    $         /,'              ndlh =', 2i5 )
C3400 format (//,' <--  Period: forc =', i5,
C    $         /,'               syr =', i5,
C    $         /,'               eyr =', i5 )
C3500 format (//,' --> Analyze: dcnt =', i5,
C    $         /,'              popt =', i5,
C    $         /,'              dopt =', i5,
C    $         /,'              dsno =', i5,
C    $         /,'            decpla =', i5,
C    $         /,'            sigdig =', i5 )
C3501 format (   '              wide =', i5,
C    $         /,'              fout =', i5,
C    $         /,'              lorh =', 2i5,
C    $         /,'              sorm =', i5,
C    $         /,'              mons =', 6i5,
C    $         /,'              days =', 4i5,
C    $         /,'             lhdur =', 10i5,
C    $         /,'                    ', 10i5 )
C3502 format (   '              ndlh =', 2i5,
C    $         /,'              forc =', i5,
C    $         /,'               syr =', i5,
C    $         /,'               eyr =', i5 )
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 154
C
C     need to check time step/units for data sets
      TSTUFG= 0
C     init to full period for each dsn & output to both file and WDM
      FORC = 1
      DOPT = 1
      POPT = 1
      WIDE = 2
      DECPLA= 0
      SIGDIG= 3
      DSNO  = 501
      FOUT  = 0
      FLNAME= 'nday.out'
C     init to do both high and low flow stats
      LORH(1)= 1
      LORH(2)= 1
      SORM = 1
      SYR = 0
      EYR = 0
C
      RESP = 1
 10   CONTINUE
C       ndhilo: 1-Select,2-Modify,3-Define,4-Period,5-Analyze,6-Return
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
C       turn on previous
        CALL ZSTCMA ( PREV, ON )
C
        GO TO (100,200,300,400,500,600), RESP
C
 100    CONTINUE
C         select data sets for analysis
          PTHNAM = 'SN      '
          CALL PRWMSE (MESSFL,WDMFL,DSNBMX, PTHNAM,
     M                 DSNBUF,DSNCNT)
          IF (DSNCNT.GT.0) THEN
C           check time step/units of selected data sets (must be daily or less)
            CALL CKTUNI (MESSFL,SCLU,WDMFL,
     M                   DSNCNT,DSNBUF)
            IF (DSNCNT .GT. 0) THEN
C             time step/units have been checked for data sets
              TSTUFG = 1
            END IF
          END IF
Cdbg      write (99,3100) dsncnt, (dsnbuf(n), n = 1, dsncnt)
          RESP = 2
          GO TO 900
 200    CONTINUE
C         modify output options
          CALL NDOTOP ( MESSFL, SCLU,
     M                  POPT, DOPT, DSNO, DECPLA, SIGDIG, WIDE,
     M                  FLNAME, FOUT )
Cdbg      write (99,3200) popt, dopt, dsno, decpla, sigdig, wide, fout
          RESP = 3
          GO TO 900
 300    CONTINUE
C         Define flow statistics, seasons, and durations
          CALL NDDEFN ( MESSFL, SCLU,
     M                  LORH, SORM, MONS, DAYS, LHDUR,
     O                  NDLH )
Cdbg      write (99,3300) lorh, sorm, mons, days, lhdur, ndlh
          RESP = 4
          GO TO 900
 400    CONTINUE
C         specify period for analysis
          CALL NDPERD ( MESSFL, SCLU, WDMFL, DSNCNT, DSNBUF,
     M                  FORC, SYR, EYR )
Cdbg      write (99,3400) forc, syr, eyr
          RESP = 5
          GO TO 900
 500    CONTINUE
C         perform analysis
          IF (DSNCNT.GT.0 .AND. TSTUFG.EQ.0) THEN
C           check time step/units of selected data sets (must be daily or less)
            CALL CKTUNI (MESSFL,SCLU,WDMFL,
     M                   DSNCNT,DSNBUF)
            IF (DSNCNT .GT. 0) THEN
C             time step/units have been checked for data sets
              TSTUFG = 1
            END IF
          END IF
          IF (DSNCNT.EQ.0) THEN
C           no data sets to analyze
            CONT = 0
            RESP = 1
            SGRP= 10
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
            IF (POPT .EQ. 0  .AND.  DOPT .EQ. 0) THEN
C             no output target defined, default to print
              POPT = 1
            END IF
            IF (POPT .EQ. 1  .AND.  FOUT .EQ. 0) THEN
C             no output file open, open default
              CALL GETFUN ( L1, FOUT )
              FLNAME = 'nday.out'
              OPEN ( UNIT = FOUT, FILE = FLNAME, STATUS = 'UNKNOWN' )
            END IF
            CONT = 1
          END IF
          IF (CONT .EQ. 1) THEN
C           looks ok, continue processing
Cdbg        write (99,3500) dsncnt, popt, dopt, dsno, decpla, sigdig
Cdbg        write (99,3501) wide, fout, lorh, sorm, mons, days, lhdur
Cdbg        write (99,3502) ndlh, forc, syr, eyr
            DO 580 N = 1, DSNCNT
C             process by data set
              DO 560 LH = 1, 2
C               process low (1), then high (2)
                IF (LORH(LH) .EQ. 1) THEN
C                 low/high duration is requested
                  IF (SORM .EQ. 1) THEN
C                   single season for duration
                    CALL NDSEAS ( MESSFL, SCLU, WDMFL, DSNBUF(N), FOUT,
     I                            LH, FORC, DOPT, POPT,
     I                            SYR, EYR, MONS(1,LH), DAYS(1,LH),
     I                            SIGDIG, DECPLA, WIDE,
     M                            LHDUR(1,LH), NDLH(LH), DSNO,
     O                            RETC )
                  ELSE
C                   one or more seasons of a month each
                    CALL NDMONS ( MESSFL, SCLU, WDMFL, DSNBUF(N), FOUT,
     I                            LH, FORC, DOPT, POPT,
     I                            SYR, EYR, MONS(1,3),
     I                            SIGDIG, DECPLA, WIDE,
     M                            LHDUR(1,LH), NDLH(LH), DSNO,
     O                            RETC )
                  END IF
                END IF
  560         CONTINUE
  580       CONTINUE
          END IF
C         turn off interrupt
          I= 16
          CALL ZSTCMA (I,OFF)
          RESP = 1
          GO TO 900
C
 600    CONTINUE
C         all done
          GO TO 900
C
 900    CONTINUE
C       turn off previous
        CALL ZSTCMA ( PREV, OFF )
C
      IF (RESP.NE.6) GO TO 10
C
      IF (POPT .EQ. 1  .AND.  FOUT .GT. 0) THEN
C       close print file
        CALL QFCLOS (FOUT,L0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKTUNI
     I                   (MESSFL, SCLU, WDMFL,
     M                    DSNCNT, DSN )
C
C     + + + PURPOSE + + +
C     Check time units for selected data sets to make sure
C     the time units are daily or less (for NDHILO).
C     The attributes TUNITS and TSSTEP are expected to be present
C     in the date sets.  If either of these attributes is missing,
C     the time units are assumed to be invalid and the data set is
C     dropped from the DSN buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMFL,DSNCNT,DSN(DSNCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number of WDM file
C     DSNCNT - count of data sets in the buffer
C     DSN    - array of data-set numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,SGRP,SAIND,SALEN,TSTEP,TUNITS,TSTUFG,RETCOD,
     $           ZERO, GPFLG, TDSFRC, SDAT(6), EDAT(6)
C
C     + + + LOCAL DEFINITIONS + + +
C     RETCOD - return code from calls to wtfndt & wdbsgi, values:
C                 0 - information successfully retrieved
C                -6 - no data present
C               -81 - data set does not exist
C               -82 - data set exists but is wrong DSTYPE
C               -85 - trying to write to a read-only data set
C              -107 - attribute is not present in the data set
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTFNDT, WDBSGI, PRNTXI, SHIFTI
C
C     + + + END SPECIFICATIONS + + +
C
      ZERO   = 0
      TSTUFG = 0
      SALEN  = 1
      GPFLG  = 1
C
      I = 1
 110  CONTINUE
C       is the data set a time series and does it contain data ?
        CALL WTFNDT ( WDMFL, DSN(I), GPFLG,
     O                TDSFRC, SDAT, EDAT, RETCOD )
        IF (RETCOD .EQ. 0) THEN
C         check time units
          SAIND = 17
          CALL WDBSGI (WDMFL,DSN(I),SAIND,SALEN,
     O                 TUNITS,RETCOD)
          IF (RETCOD .EQ. -107) THEN
C           missing time units, assume default of 4 (day)
            TUNITS = 4
            RETCOD = 0
          END IF
          IF (RETCOD .EQ. 0) THEN
C           are time units ok for ndhilo?
            IF (TUNITS .EQ. 4) THEN
C             expected daily units, check time step
              SAIND = 33
              CALL WDBSGI (WDMFL,DSN(I),SAIND,SALEN,
     O                     TSTEP,RETCOD)
              IF (RETCOD .EQ. 0  .AND.  TSTEP .GT. 1) THEN
C               found time step, but it is too large
                TSTUFG = TSTUFG + 1
                DSN(I) = -DSN(I)
              ELSE IF (RETCOD .EQ. -107) THEN
C               missing time step, assume default of 1
                TSTEP = 1
              END IF
            ELSE IF (TUNITS .GT. 4) THEN
C             time units is greater than a day, invalid
              TSTUFG = TSTUFG + 1
              DSN(I) = -DSN(I)
            END IF
          ELSE
C           dsn missing (-81), or other problem
            DSN(I) = 0
            TSTUFG = TSTUFG + 1
          END IF
        ELSE
C         no data (-6), wrong data set type (-82), dsn does not exist (-81)
          TSTUFG = TSTUFG + 1
          DSN(I) = -DSN(I)
        END IF
        I = I + 1
      IF (I .LE. DSNCNT) GO TO 110
C
      IF (TSTUFG .GT. 0) THEN
C       one or more data sets were omitted, zero and (print message)
        DO 100 I = 1, DSNCNT
          IF (DSN(I) .LT. 0) THEN
C           problem with selected data set
            SGRP = 30
            DSN(I) = -DSN(I)
            CALL PRNTXI ( MESSFL, SCLU, SGRP, DSN(I))
            DSN(I) = 0
          END IF
 100    CONTINUE
C
C       shift good dsn forward
        CALL SHIFTI ( DSNCNT, ZERO, ZERO,
     M                DSN, I )
        DSNCNT = I
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPOUT
     I                    (WDMFL,FOUT,MESSFL,SCLU,DSN,
     I                     SYR,SMO,SDY,EMO,EDY,ND,
     I                     NDUR,LH,SIGDIG,DECPLA,TSFILL,WIDE,
     M                     TYR,QM)
C     + + + PURPOSE + + +
C     This routine produced a table on the output file of the
C     results of the n-day low-high flow analysis.  The table
C     has year for rows and N-day for columns.
C
C     + + + HISTORY + + +
C     kmf 01/05/11  correct rank on low and high n-day flows
C                   low flows ranked from lowest (1) to highest (n)
C                   high flows ranked from highest (1) to lowest (n)
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,FOUT,MESSFL,SCLU,SYR,SMO,SDY,EMO,EDY,
     $          DSN,ND,LH, TYR,DECPLA,SIGDIG,WIDE
      INTEGER   NDUR(ND)
      REAL      QM(MXYRS,MXDUR), TSFILL
C
C     + + + ARGUMENT DEFINITION + + +
C     WDMFL  - Fortran unit number for WDM file
C     FOUT   - Fortran unit number for print file
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     DSN    - dataset number
C     SYR    - starting year
C     SMO    - starting month for year or season
C     SDY    - starting day for year or season
C     EMO    - ending month for year or season
C     EDY    - ending day for year or season
C     ND     - number of different durations
C     NDUR   - durations used
C     LH     - flag, 1=high flows,  2=low flows
C     SIGDIG - number of significant digits
C     DECPLA - number of decimal places
C     TSFILL - value below which is missing data
C     WIDE   - 1=80 column width, 2=132 column width
C     TYR    - number of annual values
C     QM     - buffer of annual flow values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,M,N,L0,L2,L3,L4,L5,L9,L1,
     $            IPOS,LEN,OLEN,JUST,SGRP,CENTER,NSIZE,WMAX,ND1,
     $            IRANK(MXYRS,MXDUR),ITMP(MXYRS),IYR(MXYRS),CHK,ND2,
     $            CML(12),K
      REAL        Q(MXYRS)
      CHARACTER*1 OBUF(132),CTMP(20),CYR(4),BLNK,
     $            CMD(4),CLH(4,2),DASH
C
C     + + + FUNCTIONS + + +
      INTEGER     STRLNX
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRLNX, ZIPC, CHRCHR, GETTXT, CTRSTR, ASRTR
      EXTERNAL    INTCHR, DECCHX, GTMONC, DSINFO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA    CYR,            CMD
     &     / 'Y','e','a','r', '-','d','a','y' /
      DATA  L0, L1, L2, L3, L4, L5, L9
     &     / 0,  1,  2,  3,  4,  5,  9 /
      DATA  BLNK/' '/,   DASH/'-'/
      DATA CLH/' ','L','o','w','H','i','g','h'/
      DATA CML/7,7,5,5,3,4,4,6,8,7,8,8/
C
C     + + + OUTPUT FORMATS + + +
C
 2000 FORMAT('1')
 2001 FORMAT(/)  
 2002 FORMAT(132A1)
 2003 FORMAT(A)
C
C     + + + debug formats + + +
C
C3000 format (//,' ---> wdpout:  dsn =', i5,
C    $         /,'                lh =', i5,
C    $         /,'               syr =', i5,
C    $         /,'               smo =', i5,
C    $         /,'               sdy =', i5,
C    $         /,'               emo =', i5,
C    $         /,'               edy =', i5,
C    $         /,'               tyr =', i5
C    $         /,'                nd =', i5,
C    $         /,'              ndur =', 5i5,
C    $         /,'                    ', 5i5 )
C3001 format (   '                qm =', 5f10.2,
C    $         /, ('                    ', 5f10.2 ))
C
C     + + + END SPECIFICATIONS + + +
C
Cdbg  write (99,3000) dsn, lh, syr, smo, sdy, emo, edy,
Cdbg $                tyr, nd, (ndur(i),i=1,nd)
Cdbg  write (99,3001) ((qm(i,j),j=1,5),i=1,10)
      ND1 = 0
      ND2 = 0
      WMAX = 80 + 52*(WIDE-1)
 10   CONTINUE
C       loop if too much data for width and need two tables
        IF (WIDE .EQ. 2) THEN
C         132 column file
          NSIZE = 124/ND
          IF (NSIZE .GT.14) NSIZE = 14
          CENTER = (ND*NSIZE+6)/2
          IF (CENTER .LT. 50) CENTER = 50
          ND1 = 1
          ND2 = ND
        ELSE
C         80 column file
          NSIZE = 72/(ND - ND2)
          IF (NSIZE .GT. 14) THEN
C           too much space, limit field width
            NSIZE = 14
            ND1 = 1 + ND2
            ND2 = ND
          ELSE IF (NSIZE .LT. 11) THEN
C           cant be that narrow
            NSIZE = 11
            ND1 = 1 + ND2
            IF (ND2 .EQ. 0) THEN
              ND2 = 6
            ELSE
              ND2 = ND
            END IF
          ELSE
C           fits ok
            ND1 = 1
            ND2 = ND
          END IF
          CENTER = ((ND2-ND1+1)*NSIZE+6)/2
          IF (CENTER .LT. 36) CENTER = 36
        END IF
C
C       get station identification string
        CALL DSINFO (WDMFL,DSN,WMAX,OBUF)
        WRITE (FOUT,2000)
        WRITE (FOUT,2001)
        OLEN = CENTER*2 + 1
        IF (OLEN .GT. STRLNX(WMAX,OBUF)) CALL CTRSTR (OLEN,OBUF)
        WRITE (FOUT,2002) (OBUF(K),K=1,WMAX)
        WRITE (FOUT,2003)
C
C       text: mean value and ranking for number of consecutive days
        CALL ZIPC (WMAX,BLNK,OBUF)
        SGRP = 21
        OLEN = 72
        IPOS = CENTER - OLEN/2 + 1
        CALL CHRCHR (L4,CLH(1,LH),OBUF(IPOS))
        IPOS = IPOS + 5
        CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,OBUF(IPOS))
        WRITE (FOUT,2002) (OBUF(K),K=1,WMAX)
C
C       text: for the season _mon_ _day_ to _mon_ _day_
C       text: for the months _ to _
        CALL ZIPC (WMAX,BLNK,OBUF)
        OLEN = 15 + CML(SMO) + 2 + 4 + CML(EMO) + 3
        IPOS = CENTER - OLEN/2
        SGRP = 23
        CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,OBUF(IPOS))
        IPOS = IPOS + OLEN + 1
        CALL GTMONC ( SMO, L9, L1, OLEN, OBUF(IPOS) )
        IPOS = IPOS + OLEN + 1
        JUST = 1
        CALL INTCHR ( SDY, L2, JUST, OLEN, OBUF(IPOS) )
        IPOS = IPOS + OLEN + 1
        OLEN = 4
        SGRP = 24
        CALL GETTXT ( MESSFL, SCLU, SGRP, OLEN, OBUF(IPOS) )
        IPOS = IPOS + OLEN + 1
        CALL GTMONC ( EMO, L9, L1, OLEN, OBUF(IPOS) )
        IPOS = IPOS + OLEN + 1
        CALL INTCHR ( EDY,  L2, JUST, OLEN, OBUF(IPOS) )
        WRITE (FOUT,2002) (OBUF(K),K=1,WMAX)
C
C       text: Discharge, in cubic feet per second
        CALL ZIPC (WMAX, BLNK, OBUF)
        SGRP = 22
        LEN  = 35
        IPOS = CENTER - 36/2
        CALL GETTXT (MESSFL,SCLU,SGRP,LEN,OBUF(IPOS))
        WRITE (FOUT,2002) (OBUF(K),K=1,WMAX)
        WRITE (FOUT,2003)
C
C       put header for columns
        CALL ZIPC (WMAX, BLNK, OBUF)
        IPOS = 2
        CALL CHRCHR (L4,CYR,OBUF(IPOS))
        IPOS = IPOS + 8
        JUST = 0
        DO 20 I = ND1,ND2
          CALL ZIPC (NSIZE, BLNK, CTMP)
          CALL INTCHR (NDUR(I),L4,JUST,OLEN,CTMP)
          CALL CHRCHR (L4,CMD,CTMP(L5))
          CALL CTRSTR (NSIZE,CTMP)
          CALL CHRCHR (NSIZE, CTMP, OBUF(IPOS))
          IPOS = IPOS + NSIZE
 20     CONTINUE
        WRITE (FOUT,2002) (OBUF(K),K=1,WMAX)
        CALL ZIPC (WMAX, BLNK, OBUF)
        CALL ZIPC (L4, DASH, OBUF(2))
        IPOS = 11
        N = NSIZE - 4
        DO 22 I = ND1,ND2
          CALL ZIPC (N,DASH,OBUF(IPOS))
          IPOS = IPOS + NSIZE
 22     CONTINUE
        WRITE (FOUT,2002) (OBUF(K),K=1,WMAX)
C
C       sort out bad years
        M = 0
        DO 25 I = 1,TYR
          CHK = 0
          DO 23 N = ND1,ND2
            IF (QM(I,N) .LE. TSFILL+1.0E-9) CHK = 1
 23     CONTINUE
          IF (CHK .EQ. 0) THEN
            M = M + 1
            IYR(M) = SYR + I - 1
            IF (I .NE. M) THEN
              DO 24 N = ND1,ND2
                QM(M,N) = QM(I,N)
 24           CONTINUE
            END IF
          END IF
 25     CONTINUE
        TYR = M
C
C       sort to get order number
        CALL ZIPC (WMAX, BLNK, OBUF)
        DO 40 N = ND1,ND2
          DO 30 I = 1,TYR
            Q(I) = QM(I,N)
 30       CONTINUE
          CALL ASRTR (L0, TYR, Q, ITMP)
          DO 35 I = 1,TYR
            J = ITMP(I)
            IRANK(J,N) = I
 35       CONTINUE
Ckmf      IF (LH .EQ. 1) THEN
Ckmf        high flow, reverse rank to descending
          IF (LH .EQ. 2) THEN
C           low flow, reverse rank to ascending
            DO 37 I = 1, TYR
              IRANK(I,N) = TYR + 1 - IRANK(I,N)
 37         CONTINUE
          END IF
 40     CONTINUE
C
C       write out table
        LEN = NSIZE - 3
        DO 60 I = 1,TYR
          IPOS = 1
C         convention is year period ends
          CALL INTCHR (IYR(I),L5,JUST,OLEN,OBUF(IPOS))
          IPOS = 7
          DO 50 N = ND1,ND2
            IF (QM(I,N) .LE. TSFILL+1.0E-9) THEN
              J = IPOS + LEN - 3
              CALL  ZIPC (L2,DASH,OBUF(J))
            ELSE
              CALL DECCHX (QM(I,N), LEN, SIGDIG, DECPLA, OBUF(IPOS))
            END IF
            IPOS = IPOS + NSIZE - 3
            CALL INTCHR (IRANK(I,N),L3,JUST,OLEN,OBUF(IPOS))
            IPOS = IPOS + 3
 50       CONTINUE
          WRITE (FOUT,2002) (OBUF(K),K=1,WMAX)
 60     CONTINUE
      IF (ND2 .LT. ND) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDOTOP
     I                   ( MESSFL, SCLU,
     M                     POPT, DOPT, DSNO, DECPLA, SIGDIG, WIDE,
     M                     FILNAM, FOUT )
C
C     + + + PURPOSE + + +
C     Modify Output options for N-day statistical option.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, POPT, DOPT, DSNO, DECPLA, SIGDIG, WIDE,
     $          FOUT
      CHARACTER*64 FILNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     POPT   - flag for output to print file (0-no, 2-yes)
C     DOPT   - flag for output to wdm file (0-no, 2-yes)
C     DSNO   - starting data-set number for output to wdm
C     DECPLA - number of decimal places for printed output
C     SIGDIG - number of significant digits for printed output
C     WIDE   - width of printed table
C              1 - narrow (80 characters or less)
C              2 - wide (132 characters or less)
C     FILNAM - name for output file
C     FOUT   - Fortran unit number of output print file,
C              if not open, this will be 0
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SGRP, IVAL(5), L2, L3, L64, RTCMND, AGAIN,
     $          PREV, OFF, RETC
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q1INIT, QSETI, QSETCO, QSTCTF
      EXTERNAL   Q1EDIT, QGETI, QGETCO, QGTCTF
      EXTERNAL   ZSTCMA
      EXTERNAL   STFLOP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   L2, L3, L64, PREV, OFF
     $     /  2,  3,  64,    4,   0 /
C
C     + + + END SPECIFICATIONS + + +
C
C     dsn for output, no. decimal places, no. significant digits
      IVAL(1) = DSNO
      IVAL(2) = DECPLA
      IVAL(3) = SIGDIG
C     print and/or wdm output
      IF (DOPT .EQ. 1  .AND.  POPT .EQ. 1) THEN
C       wdm and print file output
        IVAL(4) = 1
      ELSE IF (POPT .EQ. 1) THEN
C       print file output only
        IVAL(4) = 2
      ELSE IF (DOPT .EQ. 1) THEN
C       wdm output only
        IVAL(4) = 3
      ELSE
C       invalid DOPT and POPT, default to print
        IVAL(4) = 2
      END IF
C     print width
      IVAL(5) = WIDE
C
 100  CONTINUE
C       modify output options
        SGRP  = 2
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        CALL QSETI ( L3, IVAL )
        CALL QSETCO ( L2, IVAL(4) )
        CALL QSTCTF ( L3, L64, FILNAM )
        CALL Q1EDIT ( RTCMND )
        IF (RTCMND .EQ. 1) THEN
C         user wants to continue,
C         get dsn for output, no. decimal places, no. significant digits
          CALL QGETI ( L3, IVAL )
          DSNO   = IVAL(1)
          DECPLA = IVAL(2)
          SIGDIG = IVAL(3)
C         get print and/or wdm output and print width
          CALL QGETCO ( L2, IVAL(4) )
          IF (IVAL(4) .EQ. 1) THEN
C           output to both wdm and file
            POPT = 1
            DOPT = 1
          ELSE IF (IVAL(4) .EQ. 2) THEN
C           output to file only
            POPT = 1
            DOPT = 0
          ELSE
C           outut to wdm only
            POPT = 0
            DOPT = 1
          END IF
          WIDE = IVAL(5)
C         file for output
          CALL QGTCTF ( L3, L64, FILNAM )
          AGAIN = 0
        ELSE IF (RTCMND .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        ELSE
C         assume Prev, leave output options unchanged
          AGAIN = 0
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
C     turn off Prev
      CALL ZSTCMA ( PREV, OFF )
C
C     open file for output (ignore return code, for now)
      SGRP = 3
      CALL STFLOP ( MESSFL, SCLU, SGRP,
     M              FOUT, FILNAM,
     O              RETC )
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDDEFN
     I                   ( MESSFL, SCLU,
     M                     LOHI, SORM, MONS, DAYS,
     M                     LHDUR, NDLH )
C
C     + + + PURPOSE + + +
C     Define the flow (low and/or high) statistics to be computed, the
C     durations for each flow, and the season/year for each flow.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, LOHI(2), SORM, MONS(2,3), DAYS(2,2),
     $          LHDUR(10,2), NDLH(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     LOHI   - flag for low/high duration option
C              (1) low flows:  1-include, 2-don't include
C              (2) high flows: 1-include, 2-don't include
C     SORM   - flag for season/month option
C              1 - year(s) or season(s) specified for low and/or high
C              2 - individual months specified for low and/or high
C     MONS   - starting and ending months (1-jan, ..., 12-dec)
C              (1,_) - start months
C              (2,_) - end months
C              (_,1) - year or season for low flows (sorm = 1)
C              (_,2) - year or season for high flows (sorm = 1)
C              (_,3) - months for low and/or high (sorm = 2)
C     DAYS   - starting and ending days (sorm = 1)
C              (1,1) - start day, low flow
C              (2,1) - end day, low flow
C              (1,2) - start day, high flow
C              (2,2) - end day, high flow
C     LHDUR  - array of requested durations (0 if not defined)
C              (_,1) - low flows
C              (_,2) - high flows
C     NDLH   - number of flow durations
C              (1) - low
C              (2) - high
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IVAL(12),           SGRP, OPTS(1), RTCMND, AGAIN,
     $          OPCNT, OPLEN, MXSEL(1), MNSEL(1), MAXDAY,
     $          I, J, L0, L1, L4, L6, L8, L9, L10, L20
      INTEGER   KVAL(24), L24
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL  Q1INIT, QSETCO, QSETI, QSETOP, QSETIB, PRNTXT
      EXTERNAL  Q1EDIT, QGETCO, QGETI, QGETOP, QGETIB
      EXTERNAL  SHIFTI, DAYMON, COPYI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L0, L1, L4, L6, L8, L9, L10, L20, L24
     $     / 0,  1,  4,  6,  8,  9,  10,  20,  24 /
      DATA  OPCNT, OPLEN, MXSEL, MNSEL
     $     /    1,     1,     1,     1 /
C
C     + + + debug formats + + +
C
C3000 format (//,' ---> nddefn:  lohi =', 2i5,
C    #         /,'               sorm =', i5,
C    #         /,'               mons =', 6i5,
C    #         /,'               days =', 4i5 )
C3001 format (//,'   <- q1edit:  sorm =', i5,
C    #         /,'               mons =', 6i5,
C    #         /,'               days =', 4i5 )
C
C     + + + END SPECIFICATIONS + + +
C
Cdbg  write (99,3000) lohi, sorm, mons, days
C     seasons or months 1-seasons, 2-months
      OPTS(1) = SORM
C     include low(1), high(2):  1-yes, 2-no
      IVAL(1) = LOHI(1)
      IVAL(2) = LOHI(2)
      CALL COPYI ( L6, MONS, IVAL(3) )
      CALL COPYI ( L20, LHDUR, KVAL )
      CALL COPYI ( L4, DAYS, KVAL(21) )
C     
 100  CONTINUE
C       define the duration options
        SGRP = 5
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        CALL QSETOP ( OPCNT, OPLEN, MXSEL, MNSEL, OPTS )
        CALL QSETI  ( L24, KVAL )
        CALL QSETCO ( L8, IVAL )
        CALL Q1EDIT ( RTCMND )
        IF (RTCMND .EQ. 1) THEN
C         user wants to continue, get durations and seasons
          AGAIN = 0
          CALL QGETOP ( OPLEN, OPTS )
          SORM = OPTS(1)
          CALL QGETI  ( L24, KVAL )
          CALL QGETCO (  L8, IVAL )
C         shift zero values to end, get number of non-zero durations
          CALL SHIFTI ( L10, L0, L0,
     M                  KVAL(1),
     O                  NDLH(1) )
          CALL SHIFTI ( L10, L0, L0,
     M                  KVAL(11),
     O                  NDLH(2) )
          LOHI(1) = IVAL(1)
          LOHI(2) = IVAL(2)
          CALL COPYI ( L6, IVAL(3), MONS )
          CALL COPYI ( L20, KVAL, LHDUR )
          CALL COPYI ( L4, KVAL(21), DAYS )
C         check that days and months are valid combinations
          IF (SORM .EQ. 1) THEN
C           low and/or high seasons, are days valid for month
            DO 240 I = 1, 2
              IF (LOHI(I) .EQ. 1) THEN
C               durations are included
                DO 220 J = 1, 2
                  MAXDAY = DAYMON ( L1, MONS(J,I) )
                  IF (DAYS(J,I) .GT. MAXDAY) THEN
C                   day is invalid for month, default to last day
                    DAYS(J,I) = MAXDAY
                    AGAIN = 1
                  END IF
  220           CONTINUE
              END IF
  240       CONTINUE
            IF (AGAIN .EQ. 1) THEN
C             problem with one of the dates, verify corrections
              SGRP = 6
              CALL PRNTXT ( MESSFL, SCLU, SGRP )
C             update values for screen
              CALL COPYI ( L4, DAYS, KVAL(21) )
            END IF
          END IF
        ELSE IF (RTCMND .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        ELSE
C         assume Prev, leave output options unchanged
          AGAIN = 0
        END IF
Cdbg    write (99,3001) sorm, mons, days
      IF (AGAIN .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDPERD
     I                   ( MESSFL, SCLU, WDMFL, DSNCNT, DSNBUF,
     M                     FORC, SYR, EYR )
C
C     + + + PURPOSE + + +
C     Modify time period for n-day analysis.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMFL, DSNCNT, DSNBUF(DSNCNT),
     $          FORC, SYR, EYR
C
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number of users WDM file
C     DSNCNT - number of data sets in the buffer
C     DSNBUF - array of data set numbers to be processed
C     FORC   - flag indicating period or record to be used
C              1 - full record available for each data set
C              2 - common period specified for all data sets
C     SYR    - starting calendar year for data
C     EYR    - ending calendar year for data
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OPCNT, OPLEN, MXSEL(1), MNSEL(1), OPTS(1),
     $          YRMIN(2), YRMAX(2), YRDEF(2), CMMXFG, LENI, IPOS,
     $          SDATE(6), EDATE(6), YEARS(2), AGAIN, L2, RETC,
     $          RTCMND, SGRP
C
C     + + + LOCAL DEFINITIONS + + +
C     CMMXFG - flag for dates to be retrieved
C              1 - time period common to all data sets
C              2 - maximum time period available
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTDATE
      EXTERNAL   Q1INIT, QSETOP, QSETI, QSCSTI
      EXTERNAL   Q1EDIT, QGETOP, QGETI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   OPCNT, OPLEN, MXSEL, MNSEL, CMMXFG, LENI, IPOS, L2
     $     /     1,     1,     1,     1,      1,    2,    1,  2 /
C
C     + + + END SPECIFICATIONS + + +
C
C     get period of record common to all data sets
      CALL WTDATE ( WDMFL, DSNCNT, DSNBUF, CMMXFG, 
     O              SDATE, EDATE, RETC )
      IF (RETC .EQ. 0  .AND.  SDATE(1) .LE. EDATE(1)) THEN
C       common period found, assume that seasons fall within the year
        YRMIN(1) = SDATE(1)
        YRMAX(1) = EDATE(1)
        YRDEF(1) = SDATE(1)
        YRMIN(2) = SDATE(1)
        YRMAX(2) = EDATE(1)
        YRDEF(2) = EDATE(1)
C       set min, max, defaults for start and end year
        CALL QSCSTI ( LENI, IPOS, YRMIN, YRMAX, YRDEF )
C       initialize screen values
        OPTS(1)  = FORC
 100    CONTINUE
C         get time period option (1-full, 2-common) and optional dates
          SGRP = 7
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
          CALL QSETOP ( OPCNT, OPLEN, MXSEL, MNSEL, OPTS )
          CALL QSETI ( L2, YRDEF )
          CALL Q1EDIT ( RTCMND )
          IF (RTCMND .EQ. 1) THEN
C           user wants to continue get time period option and optional date
            CALL QGETOP ( OPLEN, OPTS )
            CALL QGETI ( L2, YEARS )
            FORC = OPTS(1)
            IF (FORC .EQ.1) THEN
C             use available period of record, leave years undefined
              SYR = 0
              EYR = 0
              AGAIN = 0
            ELSE IF (YEARS(1) .LE. YEARS(2)) THEN
C             use the specified common period of record for all data sets
              SYR = YEARS(1)
              EYR = YEARS(2)
              AGAIN = 0
            ELSE
C             start year must preceed or equal end year
              OPTS(1) = 2
              SGRP = 8
              CALL PRNTXT ( MESSFL, SCLU, SGRP )
              AGAIN = 1
            END IF
          ELSE IF (RTCMND .EQ. -1) THEN
C           oops, try again
            AGAIN = 1
          ELSE
C           assume Prev, leave period options unchanged
            AGAIN = 0
          END IF
        IF (AGAIN .EQ. 1) GO TO 100
      ELSE
C       assume no common time period,
        SGRP = 9
        CALL PRNTXT ( MESSFL, SCLU, SGRP )
        FORC = 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDSVHL
     I                   ( MESSFL, SCLU, WDMFL, DSN, LORH, NDUR,
     I                     SEASBM, SEASBD, SEASEM, SEASED, 
     I                     SDATE, NVAL, QM,
     M                     DSNEW, 
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Copies data set attributes from original time-series data set
C     to the first unused data set, beginnning with data set number
C     DSNEW.  Then updates attributes for n-day annual time series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMFL, DSN, LORH, NDUR, 
     I          SEASBM, SEASBD, SEASEM, SEASED, SDATE(6),
     I          NVAL, DSNEW, RETC
      REAL      QM(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number in message file
C     WDMFL  - Fortran unit number of users WDM file
C     DSN    - data-set number of the source data set
C     LORH   - indicator flag for type of time series
C              1 - low
C              2 - high
C     NDUR   - number of days in duration
C     SEASBM - beginning month of season (1-Jan, 12-Dec)
C     SEASBD - beginning day of season
C     SEASEM - ending month of season (1-Jan, 12-Dec)
C     SEASED - ending day of season
C     SDATE  - starting year of annual series
C     NVAL   - number of years in n-day series
C     QM     - array containing n-day series
C     DSNEW  - data set number
C              input as starting data set number to try
C              returned as data set number actually used
C     RETC   - return code
C               0 - successfully created new data set
C              -1 - unable to create new data sets in wdm file
C
C     + + + SAVES + + +
      INTEGER   CNTBAD
      SAVE      CNTBAD
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NATR, INDX(11), VALU(11), DSTYPE, GPFLG, LREC, GRCNT,
     $          WDMFLN, AGAIN, L1, L2, L3, L4, IVAL(3),OLEN,
     $          MAXLIN, SCNINI, IWRT, SGRP(2,2), SGRP2, JUSTR,
     $          DTOVWR, I, QFLG, TCODE, TSSTEP
      CHARACTER*1 TSTYP(4), LOHI(2)
C
C     + + + LOCAL DEFINITIONS + + +
C     SCNINI - screen initialization flag
C              -1 - don't clear anything,
C               0 - clear in EMIFE mode only
C               1 - always clear
C     IWRT   - user interaction flag
C               1 - write and go
C               0 - write and wait for user acknowledge
C              -1 - dont write yet (EMIFE only)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSCHA, WDDSCL, WDBSAI, WDBSAC, WDTPUT
      EXTERNAL  PMXTXI, PRNTXT
      EXTERNAL  INTCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NATR, CNTBAD, DSTYPE, GPFLG
     $     /  10,      0,      1,     2 /
      DATA  L1, L2, L3, L4, MAXLIN, SCNINI, IWRT, SGRP
     $     / 1,  2,  3,  4,      1,     -1,    1, 16,17, 18,19 /
      DATA  DTOVWR, TSSTEP, TCODE, QFLG,    LOHI, JUSTR
     $     /     0,      1,     6,    0, 'L','H',     0 /
C                 ts- t-    t-   vb-  ts-  ts- seas seas seas seas ts-
C     attribute   byr group code time form step -bg  -nd  -bg  -ed type
      DATA INDX/  27,   34,  17,  85,  84,  33, 256, 257, 446, 447,   1/
      DATA VALU/1800,    7,   6,   1,   1,   1,   0,   0,   0,   0,   0/
C
C     + + + OUTPUT FORMATS + + +
C
 2000 FORMAT (1X, 'ndsvhl error in call to wdbsai:',
     $      /,1X, '               dsn   =', I8,
     $      /,1X, '               dsnew =', I8,
     $      /,1X, '               indx  =', I8,
     $      /,1X, '               valu  =', I8,
     $      /,1X, '               retc  =', I8  )
 2001 FORMAT (1X, 'ndsvhl error in call to wdbsac:',
     $      /,1X, '               dsn   =', I8,
     $      /,1X, '               dsnew =', I8,
     $      /,1X, '               indx  =', I8,
     $      /,1X, '               valu  =', 4A1,
     $      /,1X, '               retc  =', I8  )
 2002 FORMAT (1X, 'ndsvhl error in call to wdtput:',
     $      /,1X, '               dsnew =', I8,
     $      /,1X, '               tstyp =', 4A1,
     $      /,1X, '               tsstep=', I8,
     $      /,1X, '               sdate =', I5,5I3,
     $      /,1X, '               nval  =', I8,
     $      /,1X, '               dtovwr=', I8,
     $      /,1X, '               qflt  =', I8,
     $      /,1X, '               tcode =', I8,
     $      /,1X, '               retc  =', I8  )
C
C     + + + END SPECIFICATIONS + + +
C
C     output wdm file is same as input wdm file
      WDMFLN = WDMFL
C     set seasons and tstype
      VALU(7) = SEASBM
      VALU(8) = SEASEM
      VALU(9) = SEASBD
      VALU(10)= SEASED
      CALL INTCHR ( NDUR, L4, JUSTR, OLEN, TSTYP )
      IF (NDUR .LT. 10) TSTYP(3) = '0'
      IF (NDUR .LT. 100) TSTYP(2) = '0'
      TSTYP(1) = LOHI(LORH)
C
 100  CONTINUE
C       look for next free dsn
        CALL WDSCHA ( WDMFL, DSNEW, DSTYPE, GPFLG,
     O                LREC, GRCNT, RETC )
        IF (RETC .EQ. -81) THEN
C         data set does not exist, create new data set and copy attributes
          CALL WDDSCL ( WDMFL, DSN, WDMFLN, DSNEW, DSTYPE,
     O                  RETC )
          IF (RETC .EQ. 0) THEN
C           add the time, space, and seasons attributes (ignore retc)
            DO 110 I = 1, NATR
              CALL WDBSAI ( WDMFL, DSNEW, MESSFL,
     I                      INDX(I), L1, VALU(I),
     O                      RETC )
              IF (RETC .NE. 0) THEN
C               problem adding attribute
                WRITE (99,2000) DSN, DSNEW, INDX(I), VALU(I), RETC
              END IF
 110        CONTINUE
C           add the tstype attribute (ignore retc)
            CALL WDBSAC ( WDMFL, DSNEW, MESSFL,
     I                    INDX(11), L4, TSTYP,
     O                    RETC )
            IF (RETC .NE. 0) THEN
C             problem adding attribute
              WRITE (99,2001) DSN, DSNEW, INDX(9), TSTYP, RETC
            END IF
            AGAIN = 0
            RETC  = 0
          ELSE
C           unknown problem, try again
            AGAIN = 1
            DSNEW = DSNEW + 1
          END IF
        ELSE IF (RETC .EQ. 0) THEN
C         data set already exists, keep looking
          AGAIN = 1
          DSNEW = DSNEW + 1
        ELSE IF (RETC .EQ. -84) THEN
C         data set is out of valid range, start over at 1
          CNTBAD = CNTBAD + 1
          IF (CNTBAD .LE. 1) THEN
C           have not gone all the way thru range of dsn, keep trying
            AGAIN = 1
            DSNEW = 1
          ELSE
C           infinity check, have been past dsn 1 before, give it up
            AGAIN =  0
            DSNEW =  0
            RETC  = -1
          END IF
        ELSE
C         unknown error, try next data set
          AGAIN = 1
          DSNEW = DSNEW + 1
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
      IF ( RETC .EQ. 0) THEN
C       new data sets successfully created, add n-day series
        CALL WDTPUT ( WDMFL, DSNEW, TSSTEP, SDATE, NVAL, DTOVWR,
     I                QFLG, TCODE, QM,
     O                RETC )
        IF (RETC .EQ. 0) THEN
C         &-day [low/high] flow written to data-set &.
          IVAL(1) = NDUR
          IVAL(2) = DSNEW
          CALL PMXTXI ( MESSFL, SCLU, SGRP(1,LORH),
     I                  MAXLIN, SCNINI, IWRT, L2, IVAL )
        ELSE
C         &-day [low/high] flow NOT written to data-set &, retc = &.
          IVAL(1) = NDUR
          IVAL(2) = DSNEW
          IVAL(3) = RETC
          CALL PMXTXI ( MESSFL, SCLU, SGRP(2,LORH),
     I                  MAXLIN, SCNINI, IWRT, L3, IVAL )
          WRITE (99,2002) DSNEW, TSTYP, TSSTEP, SDATE, NVAL, DTOVWR,
     $                    QFLG, TCODE, RETC
          RETC = -2
        ENDIF
      ELSE
C       really big problem, unable to add data sets to wdm file, give up
        SGRP2 = 45
        CALL PRNTXT ( MESSFL, SCLU, SGRP2 )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDCOMP
     I                   ( LORH, ND, NDUR, RDUR, NVAL, Q, TSFILL, NYR,
     O                     QM, RETCOD )
C
C     + + + PURPOSE + + +
C     From a serious of daily values, computes the n-day high
C     or low series.  The n-day series will NOT be computed if
C     any of the time steps are missing.
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LORH, ND, NDUR(MXDUR), NVAL, NYR, RETCOD
      REAL      RDUR(MXDUR), Q(NVAL), TSFILL, QM(MXYRS,MXDUR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LORH   - indicator flag for type of time series
C              1 - low
C              2 - high
C     ND     - number of durations
C     NDUR   - array containing current durations
C     RDUR   - array containing durations, real form
C     NVAL   - number of days in season
C     Q      - array containing a season of values
C     TSFILL - missing value filler
C     NYR    - order number of year being processed
C     QM     - array of computed durations
C     RETCOD - return code
C               0 - everything ok
C              -1 - season skipped because of missing record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   COUNT, I, J, K, IK
      REAL      SUM(MXDUR)
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + debug formats + + +
C
C3000 format (//,' ---> ndcomp: lorh =', i5,
C    $         /,'                nd =', i5,
C    $         /,'              nval =', i5,
C    $         /,'               nyr =', i5,
C    $         /,'              ndur =', 10i5,
C    $         /,'              rdur =', 10f5.0 )
C3001 format ( /,'  <-- ndcomp: retc =', i5,
C    $         /,'                qm =', 5f10.2,
C    $         /,'                    ', 5f10.2 )
C
C     + + + END SPECIFICATIONS + + +
C
C     check for missing or bad 
Cdbg  write (99,3000) lorh, nd, nval, nyr, ndur, rdur
      COUNT = 0
      DO 100 I = 1, NVAL
Ckmf    IF (Q(I) .LE. TSFILL+1.0E-9) THEN
Ckmf    allow for negative values
        IF (ABS(Q(I)-TSFILL) .LE. 1.0E-9) THEN
C         bad data value
          COUNT = COUNT + 1
        END IF
 100  CONTINUE
C
      IF (COUNT .EQ. 0) THEN
C       no missing or bad record, compute n-day series
        DO 240 I = 1, NVAL
C         for each day in the season
          DO 220 J = 1, ND
C           for each duration
            IF (I .GE. NDUR(J)) THEN
C             cumulate enough time steps to check
              SUM(J) = 0.0
              DO 210 K = 1, NDUR(J)
                IK = I + 1 - K
                SUM(J) = SUM(J) + Q(IK)
 210          CONTINUE
              IF (LORH .EQ. 1) THEN
C               low flow period total
                IF (SUM(J) .LT. QM(NYR,J)) THEN
C                 new n-day low flow
                  QM(NYR,J) = SUM(J)
                END IF
              ELSE
C               high flow period
                IF (SUM(J) .GT. QM(NYR,J)) THEN
C                 new n-day high flow total
                  QM(NYR,J) = SUM(J)
                END IF
              END IF
            END IF
 220      CONTINUE
 240    CONTINUE
C       take average of n-day flow totals
        DO 260 J = 1, ND
          QM(NYR,J) = QM(NYR,J) / RDUR(J)
 260    CONTINUE
        RETCOD = 0
      ELSE
C       record contains missing time steps, no computation
        DO 280 J = 1, ND
          QM(NYR,J) = TSFILL
 280    CONTINUE
        RETCOD = -1
      END IF
Cdbg  write (99,3001) retcod, (qm(nyr,j), j = 1, nd)
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDTMDR
     I                   ( MESSFL, SCLU, WDMFL, DSN,
     I                     FORC, SYR, SMO, SDY, EYR, EMO, EDY,
     M                     DTRAN, ND, NDUR, LHDUR,
     O                     TSDATE, TEDATE, TYR, YDATE, NVAL, TSFILL,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Gets period of available record, time step and unit, and tsfill.
C     If not daily time step, asks user how data should be aggregated.
C     Based on requested season, determines retrieval dates.
C
C     + + + HISTORY + + +
C     kmf 01/05/11  added calls to dbndry to force dates to a day boundary
C                   added calls to sbndry to force start and end dates to
C                   the season boundaries.
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMFL, DSN, FORC,
     $          SYR, SMO, SDY, EYR, EMO, EDY, DTRAN,
     $          ND, NDUR(MXDUR), LHDUR(MXDUR), TSDATE(6), TEDATE(6),
     $          TYR, YDATE(6), NVAL, RETC
      REAL      TSFILL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number of users WDM file
C     DSN    - data-set number of the source data set
C     FORC   - flag indicating period or record to be used
C              1 - full record available for each data set
C              2 - common period specified for all data sets
C     SYR    - starting calendar year for data
C     SMO    - starting month (1-jan, 12-dec)
C     SDY    - starting day
C     EYR    - ending calendar year for data
C     EMO    - ending month (1-jan, 12-dec)
C     EDY    - ending day
C     DTRAN  - data transformation indicator for aggregation
C              0 - average
C              1 - sum
C     ND     - number of durations
C     NDUR   - array containing current durations
C     LHDUR  - array containing both low and high durations
C     TSDATE - starting date
C     TEDATE - ending date
C     TYR    - number of years
C     YDATE  - starting season year (year of last month in season)
C     NVAL   - number of days in season
C     TSFILL - missing value filler
C     RETC   - return code
C               0 - everything was ok
C               1 - user exited from question on aggregation
C              >0 - problem retrieving data
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TSTEP, TUNITS, AORS, SGRP, RTCMND, AGAIN,
     $          I, L0, L1, L2, MAXLIN, SCNINI, IWRT, IVAL(2),
     $          NND, TSD, TUD, TSTEPF, TCDCMP, FWD, BAK
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON, TIMDIF, CMPTIM, SHIFTI, COPYI, DBNDRY, SBNDRY
      EXTERNAL   WDGTTM
      EXTERNAL   Q1INIT, QSETI, QSETCO, Q1EDIT, QGETCO, PRNTXT, PMXTXI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   L0, L1, L2, MAXLIN, SCNINI, IWRT, TSD, TUD, FWD, BAK
     $     /  0,  1,  2,      1,      1,    0,   1,   4,   1,   2 /
C
C     + + + debug formats + + +
C
C3000 format ( /,'  --> ndtmdr:  dsn =', i5,
C    $         /,'              forc =', i5,
C    $         /,'               syr =', i5,
C    $         /,'               smo =', i5,
C    $         /,'               sdy =', i5,
C    $         /,'               eyr =', i5,
C    $         /,'               emo =', i5,
C    $         /,'               edy =', i5 )
C3001 format (   '             dtran =', i5,
C    $         /,'                nd =', i5,
C    $         /,'              ndur =', 10i5,
C    $         /,'             lhdur =', 10i5 )
C3002 format ( /,'  <-- wdgttm: retc =', i5,
C    $         /,'            tsdate =', 6i5,
C    $         /,'            tedate =', 6i5,
C    $         /,'             tstep =', i5,
C    $         /,'            tunits =', i5,
C    $         /,'            tsfill =', f8.1 )
C3003 format ( /,'  <-- cmptim: tstepf=', i5,
C    $         /,'            tcdcmp =', i5 )
C3004 format ( /,'  <-- dbndry:  fwd =', i5,
C    $         /,'               bak =', i5,
C    $         /,'            tsdate =', 6i5,
C    $         /,'            tedate =', 6i5 )
C3005 format ( /,'  <-- sbndry:  fwd =', i5,
C    $         /,'               bak =', i5,
C    $         /,'               smo =', i5,
C    $         /,'               sdy =', i5,
C    $         /,'               emo =', i5,
C    $         /,'               edy =', i5 )
C3006 format (   '            tsdate =', 6i5,
C    $         /,'            tedate =', 6i5 )
C
C     + + + END SPECIFICATIONS + + +
C
Cdbg  write (99,3000) dsn, forc, syr, smo, sdy, eyr, emo, edy
Cdbg  write (99,3001) dtran, nd, ndur, lhdur
C     find out about data set
      CALL WDGTTM ( WDMFL, DSN,
     O              TSDATE, TEDATE, TSTEP, TUNITS, TSFILL, RETC )
Cdbg  write (99,3002) retc, tsdate, tedate, tstep, tunits, tsfill
      IF (RETC .EQ. 0) THEN
C       data set found ok, is data daily?
        CALL CMPTIM ( TUD, TSD, TUNITS, TSTEP,
     O                TSTEPF, TCDCMP )
Cdbg    write (99,3003) tstepf, tcdcmp
        IF (TCDCMP .NE. 0) THEN
C         time units not daily, aggregate: 1-Average, 2-Sum
          AORS = DTRAN + 1
 100      CONTINUE
            SGRP = 40
            CALL Q1INIT (MESSFL, SCLU, SGRP )
            CALL QSETI ( L1, DSN )
            CALL QSETCO ( L1, AORS )
            CALL Q1EDIT ( RTCMND )
            IF (RTCMND .EQ. 1) THEN
C             user wants to continue, get transormation
              CALL QGETCO ( L1, AORS )
              DTRAN = AORS - 1
              RETC  = 0
              AGAIN = 0
            ELSE IF (RTCMND .EQ. -1) THEN
C             oops, try again
              AGAIN = 1
            ELSE
C             assume Prev, user wants out of analysis
              RETC  = 1
              AGAIN = 0
            END IF
          IF (AGAIN .EQ. 1) GO TO 100
        END IF
      END IF
C
      IF (RETC .EQ. 0) THEN
        IF (TSFILL .GT. -1.0E-9) THEN
C         expect tsfill to be < 0.0, make it so
          TSFILL = -999.
        END IF
        IF (TCDCMP .NE. 0) THEN
C         force date to a day boundary
          CALL DBNDRY ( FWD, TSDATE )
          CALL DBNDRY ( BAK, TEDATE )
Cdbg      write (99,3004) fwd, bak, tsdate, tedate
        END IF
C
C       determine period to be considered
        IF (FORC .EQ. 1) THEN
C         using Full available time period
Ckmf      IF (TSDATE(2) .GT. SMO) THEN
C           data starts in month after start of season
C           start analysis in next year (first full year)
Ckmf        TSDATE(1) = TSDATE(1) + 1
Ckmf      END IF
Ckmf      TSDATE(2) = SMO
Ckmf      IF (TEDATE(2) .LT. EMO) THEN
C           data ends in month before end of season
C           end analysis in previous year (last full year)
Ckmf        TEDATE(1) = TEDATE(1) - 1
Ckmf      END IF
Ckmf      TEDATE(2) = EMO
Ckmf      TEDATE(3) = DAYMON(TEDATE(1), TEDATE(2))
          CALL SBNDRY ( FWD, SMO, SDY, TSDATE )
          CALL SBNDRY ( BAK, EMO, EDY, TEDATE )
Cdbg      write (99,3005) fwd, bak, smo, sdy, emo, edy
Cdbg      write (99,3006) tsdate, tedate
Ckmf
        ELSE
C         using Common period
          TSDATE(1) = SYR
          TSDATE(2) = SMO
          TSDATE(3) = SDY
          TEDATE(1) = EYR
          TEDATE(2) = EMO
          IF (TEDATE(2) .EQ. 2  .AND.  EDY .GE. 28) THEN
C           last day in February, check for leap year
            TEDATE(3) = DAYMON( TEDATE(1), TEDATE(2) )
          ELSE
            TEDATE(3) = EDY
          END IF
        END IF
C
C       get number of years
        IF (EMO .GE. SMO) THEN
C         period is within one year
          TYR = TEDATE(1) - TSDATE(1) + 1
          YDATE(1) = TSDATE(1)
        ELSE
C         period spans two years
          TYR = TEDATE(1) - TSDATE(1)
          YDATE(1) = TSDATE(1) + 1
        END IF
        IF (TYR .GT. MXYRS) TYR = MXYRS
C
C       get number of values in this season
        TEDATE(1) = TSDATE(1)
        IF (EMO .LT. SMO) TEDATE(1) = TEDATE(1) + 1
        CALL TIMDIF (TSDATE, TEDATE, TUD, TSD, NVAL)
C
C       check that durations are <= season length
        NND = ND
        DO 200 I = 1,ND
          IF (NDUR(I) .GT. NVAL) THEN
            NND = NND - 1
            NDUR(I) = 0
          END IF
 200    CONTINUE
        IF (NND .NE. ND) THEN
C         one or more durations dropped because > season length
          SGRP = 42
          CALL PRNTXT ( MESSFL, SCLU, SGRP )
          CALL SHIFTI ( MXDUR, L0, L0,
     M                  NDUR,
     O                  ND )
C         update durations for low/high flows
          CALL COPYI ( MXDUR, NDUR, LHDUR )
        END IF
      ELSE
C       problem with data set, skip it
        SGRP = 41
        IVAL(1) = DSN
        IVAL(2) = RETC
        CALL PMXTXI ( MESSFL, SCLU, SGRP, 
     I                MAXLIN, SCNINI, IWRT, L2, IVAL )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDLHSN
     I                   ( MESSFL, SCLU, WDMFL, DSN, LORH, FORC,
     I                     SYR, SMO, SDY, EYR, EMO, EDY,
     M                     DTRAN, ND, NDUR, LHDUR, QM, YDATE, TYR,
     O                     TSFILL, RETC )
C
C     + + + PURPOSE + + +
C     Get period of record for the data set.  Compute the
C     requested durations for the requested season.
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMFL, DSN, LORH, FORC,
     $          SYR, SMO, SDY, EYR, EMO, EDY, DTRAN, ND,
     $          NDUR(MXDUR), LHDUR(MXDUR), RETC, YDATE(6), TYR
      REAL      QM(MXYRS,MXDUR), TSFILL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for WDM file
C     DSN    - dataset number
C     LORH   - flag, 1=high flows,  2=low flows
C     FORC   - flag indicating period or record to be used
C              1 - full record available for each data set
C              2 - common period specified for all data sets
C     SYR    - starting calendar year of season
C     SMO    - starting month of season
C     SDY    - starting day of season
C     EYR    - ending calendar year of season
C     EMO    - ending month of season
C     EDY    - ending day of season
C     DTRAN  - data transformation indicator for aggregation
C              0 - average
C              1 - sum
C     ND     - number of durations
C     NDUR   - array containing current durations
C     LHDUR  - array containing both low and high durations
C     QM     - computed durations
C     YDATE  - starting season year (year of last month in season)
C     TYR    - number of years
C     TSFILL - missing value filler
C     RETC   - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, NVAL, TSDATE(6), TEDATE(6)
      REAL      RDUR(MXDUR)
C
C     + + + INTRINSICS + + +
      INTRINSIC REAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI
      EXTERNAL  NDTMDR, NDYEAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  TSDATE,       TEDATE
     $     /0,0,1,0,0,0,  0,0,0,24,0,0 /
C
C     + + + debug formats + + +
C
C3000 format (//,' ---> ndlhsn:  dsn =', i5,
C    $         /,'              lorh =', i5,
C    $         /,'              forc =', i5,
C    $         /,'               syr =', i5,
C    $         /,'               smo =', i5,
C    $         /,'               sdy =', i5
C    $         /,'               eyr =', i5,
C    $         /,'               emo =', i5,
C    $         /,'               edy =', i5,
C    $         /,'             dtran =', i5,
C    $         /,'                nd =', i5,
C    $         /,'             lhdur =', 10i5,
C    $         /,'              ndur =', 10i5 )
C3001 format ( /,'  <-- ndtmdr:   nd =', i5,
C    $         /,'             lhdur =', 10i5,
C    $         /,'              ndur =', 10i5,
C    $         /,'             dtran =', i5,
C    $         /,'            tsdate =', 6i5,
C    $         /,'            tedate =', 6i5,
C    $         /,'             ydate =', 6i5,
C    $         /,'               tyr =', i5,
C    $         /,'              nval =', i5,
C    $         /,'            tsfill =', f10.2,
C    $         /,'              retc =', i5 )
C3002 format ( /,'  <-- ndyear: nval =', i5,
C    $         /,'            tsdate =', 6i5,
C    $         /,'            tedate =', 6i5,
C    $         /,'              retc =', i5 )
C
C     + + + END SPECIFICATIONS + + +
C
Cdbg  write (99,3000) dsn, lorh, forc, syr, smo, sdy, eyr, emo, edy,
Cdbg $                dtran, nd, lhdur, ndur
      CALL COPYI ( MXDUR, LHDUR, NDUR )
C     get period of record, check time units and tsfill
      CALL NDTMDR ( MESSFL, SCLU, WDMFL, DSN, FORC,
     I              SYR, SMO, SDY, EYR, EMO, EDY,
     M              DTRAN, ND, NDUR, LHDUR,
     O              TSDATE, TEDATE, TYR, YDATE, NVAL, TSFILL,
     O              RETC )
Cdbg  write (99,3001) nd, lhdur, ndur, dtran, tsdate, tedate, ydate,
Cdbg $                tyr, nval, tsfill, retc
      IF (RETC .EQ. 0) THEN
C       still looks good, get real durations
        DO 100 J = 1, MXDUR
          RDUR(J) = REAL ( NDUR(J) )
  100   CONTINUE
C
C       compute durations
        CALL NDYEAR ( MESSFL, SCLU, WDMFL, DSN,
     I                DTRAN, TSFILL, TYR, LORH, ND, NDUR, RDUR,
     M                TSDATE, TEDATE, NVAL, QM,
     O                RETC )
      END IF
Cdbg  write (99,3002) nval, tsdate, tedate, retc
      RETURN
      END
C
C
C
      SUBROUTINE   NDYEAR
     I                    ( MESSFL, SCLU, WDMFL, DSN, DTRAN, TSFILL,
     I                      TYR, LORH, ND, NDUR, RDUR,
     M                      TSDATE, TEDATE, NVAL, QM,
     O                      RETC )
C
C     + + + PURPOSE + + +
C     Compute the durations for the requested year or season
C     for the period of record requested.
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMFL, DSN, DTRAN, NVAL, TYR,
     $          LORH, ND, NDUR(ND), TSDATE(6), TEDATE(6), RETC
      REAL      RDUR(ND), QM(MXYRS,MXDUR), Q(366), TSFILL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for WDM file
C     DSN    - dataset number
C     DTRAN  - data transformation indicator for aggregation
C              0 - average
C              1 - sum
C     TSFILL - missing value filler
C     TYR    - number of years
C     LORH   - flag, 1=high flows,  2=low flows
C     ND     - number of different durations
C     NDUR   - durations used (integer values)
C     RDUR   - durations used (real equivalents of NDUR)
C     TSDATE - starting date
C     TEDATE - ending date
C     NVAL   - number of days in season
C     QM     - computed durations
C     RETC   - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NYR, QFLG, TS, TU, J,
     $          SGRP, LINCNT, MAXL, SCNI, IWRT, L1, ITMP(1)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   NDCOMP
      EXTERNAL   TIMDIF, DAYMON, WDTGET, PMXTXI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   QFLG, TS, TU, MAXL, SCNI, IWRT, L1
     $     /   30,  1,  4,    1,    1,    1,  1 /
C
C     + + + debug formats + + +
C
C
C3000 format (//,' ---> wdtget:  dsn =', i5,
C    $         /,'                ts =', i5,
C    $         /,'                tu =', i5,
C    $         /,'            tsdate =', 6i5,
C    $         /,'              nval =', i5,
C    $         /,'             dtran =', i5,
C    $         /,'              qflg =', i5 )
C3001 format (//,'  <-- wdtget: retc =', i5,
C    $         /,'              q(1) =', 5f10.2,
C    $         /,'              q(n) =', 5f10.2 )
C3002 format (//,' ---> ndcomp: lorh =', i5,
C    $         /,'                nd =', i5,
C    $         /,'              nval =', i5,
C    $         /,'               nyr =', i5,
C    $         /,'            tsfill =', f10.2,
C    $         /,'              ndur =', 10i5,
C    $         /,'              rdur =', 10f5.0 )
C3003 format ( /,'  <-- ndcomp: retc =', i5,
C    $         /,'                qm =', 5f10.2,
C    $         /,'                    ', 5f10.2 )
C3004 format (//,'  <-- wdtget: retc =', i5,
C    $         /,'                 q =', 5f10.2,
C    $       /,( '                    ', 5f10.2 ) )
C     + + + END SPECIFICATIONS + + +
C
C     status:  processing input data set
      SGRP = 12
      CALL PMXTXI ( MESSFL, SCLU, SGRP, MAXL, SCNI, IWRT,
     I              L1, DSN )
C
      LINCNT = 0
      NYR = 1
  100 CONTINUE
C       get year or season of data from WDM
Cdbg    write (99,3000) dsn, ts, tu, tsdate, nval, dtran, qflg
        CALL WDTGET ( WDMFL, DSN, TS, TSDATE, NVAL, DTRAN, QFLG, TU,
     O                Q, RETC )
Cdbg    write (99,3001) retc, (q(j), j = 1, 5), (q(j), j = 361, 365)
Cdbg    write (99,3004) retc, (q(j), j = 1, nval )
        IF (RETC .EQ. 0) THEN
C         data successfully retrieved, compute n-day series
Cdbg      write (99,3002) lorh, nd, nval, nyr, tsfill,  ndur, rdur
          CALL NDCOMP ( LORH, ND, NDUR, RDUR, NVAL, Q, TSFILL, NYR,
     O                  QM, RETC )
Cdbg      write (99,3003) retc, (qm(nyr,j), j = 1, 10)
          IF (RETC .NE. 0) THEN
C           year beginning & has bad values, skipped analysis
            SGRP = 13
            CALL PMXTXI ( MESSFL,SCLU,SGRP,MAXL,-SCNI,IWRT,
     I                    L1,TSDATE(1))
            LINCNT = LINCNT + 1
            RETC = 0
          END IF
        ELSE
C         data read error, values set to -999
          DO 200 J = 1, ND
            QM(NYR,J) = -999.
 200      CONTINUE
          SGRP = 14
          ITMP(1) = RETC
          CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,-SCNI,IWRT,L1,ITMP)
          LINCNT = LINCNT + 1
        END IF
        IF (MOD(NYR,10) .EQ. 0) THEN
C         _ years processed
          SGRP = 15
          ITMP(1) = NYR
          CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,-SCNI,IWRT,L1,ITMP)
          LINCNT = LINCNT + 1
        END IF
C       next year
        NYR = NYR + 1
        TEDATE(1) = TEDATE(1) + 1
        TSDATE(1) = TSDATE(1) + 1
        IF (TEDATE(2) .EQ. 2  .AND.  TEDATE(3) .GE. 28) THEN
C         last day in February, check for leap year
          TEDATE(3) = DAYMON( TEDATE(1), TEDATE(2) )
        END IF
        CALL TIMDIF ( TSDATE, TEDATE, TU, TS,  NVAL)
        IF (LINCNT .GE. 50  .AND.  NYR .LE. TYR) THEN
C         make sure buffer doesn't get blown out
          SGRP = 12
          CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,SCNI,IWRT,L1,DSN)
          LINCNT = 1
        END IF
C       another year of data?
      IF (NYR .LE. TYR) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDSEAS
     I                   ( MESSFL, SCLU, WDMFL, DSN, FOUT,
     I                     LH, FORC, DOPT, POPT,
     I                     SYR, EYR, MONS, DAYS,
     I                     SIGDIG, DECPLA, WIDE,
     M                     LHDUR, ND, DSNO,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Compute requested durations for season specified.
C     Optionally writes durations to wdm data sets (DOPT) and/or
C     prints durations to a file (POPT).
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMFL, DSN, FOUT, LH, FORC, DOPT, POPT,
     $          SYR, EYR, MONS(2), DAYS(2), SIGDIG, DECPLA,
     $          WIDE, LHDUR(MXDUR), ND, DSNO, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for WDM file
C     DSN    - dataset number
C     FOUT   - Fortran unit number for print file
C     LH     _ flag, 1=high flows,  2=low flows
C     FORC   - flag indicating period or record to be used
C              1 - full record available for each data set
C              2 - common period specified for all data sets
C     DOPT   - flag for output to wdm file (0-no, 2-yes)
C     POPT   - flag for output to print file (0-no, 2-yes)
C     SYR    - starting calendar year for data
C     EYR    - ending calendar year for data
C     MONS   - starting and ending months (1-jan, ..., 12-dec)
C              (1) - first month  of season or year
C              (2) - last month of season or year
C     DAYS   - starting and ending days for corresponding MONS
C     SIGDIG - number of significant digits for printed output
C     DECPLA - number of decimal places for printed output
C     WIDE   - width of printed table
C              1 - narrow (80 characters or less)
C              2 - wide (132 characters or less)
C     LHDUR  - array of requested durations (0 if not defined)
C     ND     - number of non-zero durations in LHDUR
C     DSNO   - starting data-set number for output to wdm
C     RETC   - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NDUR(MXDUR), YDATE(6), TYR, J, DTRAN
Cdbg  integer   i
      REAL      QM(MXYRS,MXDUR), TSFILL, ZLOHI(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   NDLHSN, NDSVHL, WDPOUT, ZIPR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA    ZLOHI,          YDATE
     $     / 1.0E20,-1.0E20, 0,1,1,0,0,0 /
C
C     + + + debug format + + +
C
C3000 format (//,' ---> ndseas:  dsn =', i5,
C    $         /,'                lh =', i5,
C    $         /,'              forc =', i5,
C    $         /,'              dopt =', i5,
C    $         /,'              popt =', i5,
C    $         /,'               syr =', i5,
C    $         /,'               eyr =', i5,
C    $         /,'              mons =', 2i5,
C    $         /,'              days =', 2i5,
C    $         /,'                nd =', i5,
C    $         /,'             lhdur =', 10i5,
C    $         /,'              ndur =', 10i5 )
C3001 format ( /,'  <-- ndlhsn:   nd =', i5,
C    $         /,'             lhdur =', 10i5,
C    $         /,'              ndur =', 10i5,
C    $         /,'             ydate =', 6i5,
C    $         /,'               tyr =', i5,
C    $         /,'            tsfill =', f10.2,
C    $         /,'              retc =', i5,
C    $         /,'                qm ='  )
C3002 format ( 10f8.0 )
C3003 format ( /,'  <-- wdpout:  tyr =', i5,
C    $         /,'                qm ='  )
C
C     + + + END SPECIFICATIONS + + +
C
Cdbg  write (99,3000) dsn, lh, forc, dopt, popt, syr, eyr, mons, days,
Cdbg $                nd, lhdur, ndur
      DTRAN = 0
C
      CALL ZIPR ( MXDUYR, ZLOHI(LH), QM )
      CALL NDLHSN ( MESSFL, SCLU, WDMFL, DSN, LH, FORC,
     I              SYR, MONS(1), DAYS(1), EYR, MONS(2), DAYS(2),
     M              DTRAN, ND, NDUR, LHDUR, QM, YDATE, TYR,
     O              TSFILL, RETC )
Cdbg  write (99,3001) nd, lhdur, ndur, ydate, tyr, tsfill, retc
Cdbg  write (99,3002) ( ( qm(i,j), j=1,10 ), i = 1, tyr )
      IF (DOPT.EQ.1 .AND. RETC.EQ.0) THEN
C       put annual time-series on WDM file
        DO 200 J = 1,ND
C         low/high durations to wdm file
          CALL NDSVHL ( MESSFL, SCLU, WDMFL, DSN, LH, NDUR(J),
     I                  MONS(1), DAYS(1), MONS(2), DAYS(2),
     I                  YDATE, TYR, QM(1,J),
     M                  DSNO,
     O                  RETC )
 200    CONTINUE
      END IF
      IF (POPT.GT.0 .AND. RETC.EQ.0) THEN
C       print output to file
        CALL WDPOUT ( WDMFL, FOUT, MESSFL, SCLU, DSN,
     I                YDATE, MONS(1), DAYS(1),
     I                MONS(2), DAYS(2), ND, NDUR, LH,
     I                SIGDIG, DECPLA, TSFILL, WIDE,
     M                TYR, QM )
Cdbg  write (99,3003) tyr
Cdbg  write (99,3002) ( ( qm(i,j), j=1,10 ), i = 1, tyr )
      ELSE IF (RETC.LT.0) THEN
C       problem with this data set, but not stopping analysis
        RETC= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDMONS
     I                   ( MESSFL, SCLU, WDMFL, DSN, FOUT,
     I                     LH, FORC, DOPT, POPT,
     I                     SYR, EYR, MONS,
     I                     SIGDIG, DECPLA, WIDE,
     M                     LHDUR, ND, DSNO,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Compute requested durations for each of the months specified.
C     Optionally writes durations to wdm data sets (DOPT) and/or
C     prints durations to a file (POPT).
C
C     + + + PARAMETERS + + +
      INCLUDE 'pnday.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMFL, DSN, FOUT, LH, FORC, DOPT, POPT,
     $          SYR, EYR, MONS(2), SIGDIG, DECPLA, WIDE,
     $          LHDUR(MXDUR), ND, DSNO, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for WDM file
C     DSN    - dataset number
C     FOUT   - Fortran unit number for print file
C     LH     _ flag, 1=high flows,  2=low flows
C     FORC   - flag indicating period or record to be used
C              1 - full record available for each data set
C              2 - common period specified for all data sets
C     DOPT   - flag for output to wdm file (0-no, 2-yes)
C     POPT   - flag for output to print file (0-no, 2-yes)
C     SYR    - starting calendar year for data
C     EYR    - ending calendar year for data
C     MONS   - starting and ending months (1-jan, ..., 12-dec)
C              (1) - first month to be included
C              (2) - last month to be included
C     SIGDIG - number of significant digits for printed output
C     DECPLA - number of decimal places for printed output
C     WIDE   - width of printed table
C              1 - narrow (80 characters or less)
C              2 - wide (132 characters or less)
C     LHDUR  - array of requested durations (0 if not defined)
C     ND     - number of non-zero durations in LHDUR
C     DSNO   - starting data-set number for output to wdm
C     RETC   - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NDUR(MXDUR), YDATE(6), TYR, DAY1, DAY2, MON, J,
     $          DTRAN, MORE
      REAL      QM(MXYRS,MXDUR), TSFILL, ZLOHI(2)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   NDLHSN, NDSVHL, WDPOUT, ZIPR
      EXTERNAL   DAYMON
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  ZLOHI,          YDATE,      DAY1
     $     /1.0E20,-1.0E20, 0,1,1,0,0,0,   1 /
C
C     + + + debug format + + +
C
C3000 format (//,' ---> ndmons:  dsn =', i5,
C    $         /,'                lh =', i5,
C    $         /,'              forc =', i5,
C    $         /,'              dopt =', i5,
C    $         /,'              popt =', i5,
C    $         /,'               syr =', i5,
C    $         /,'               eyr =', i5,
C    $         /,'              mons =', 2i5,
C    $         /,'                nd =', i5,
C    $         /,'             lhdur =', 5i5,
C    $         /,'                    ', 5i5 )
C
C     + + + END SPECIFICATIONS + + +
C
Cdbg  write (99,3000) dsn, lh, forc, dopt, popt, syr, eyr, mons,
Cdbg $                lhdur, nd, dsno
      MORE  = 1
      DTRAN = 0
      MON   = MONS(1)
  100 CONTINUE
C       compute durations for specified month
        DAY2 = DAYMON( SYR, MON )
        CALL ZIPR ( MXDUYR, ZLOHI(LH), QM )
        CALL NDLHSN ( MESSFL, SCLU, WDMFL, DSN, LH, FORC,
     I                SYR, MON, DAY1, EYR, MON, DAY2,
     M                DTRAN, ND, NDUR, LHDUR, QM, YDATE, TYR,
     O                TSFILL, RETC )
        IF (DOPT.EQ.1 .AND. RETC.EQ.0) THEN
C         put annual time-series on WDM file
          DO 200 J = 1,ND
C           low/high durations to wdm file
            CALL NDSVHL ( MESSFL, SCLU, WDMFL, DSN, LH, NDUR(J),
     I                    MON, DAY1, MON, DAY2,
     I                    YDATE, TYR, QM(1,J),
     M                    DSNO,
     O                    RETC )
 200      CONTINUE
        END IF
        IF (POPT.GT.0 .AND. RETC.EQ.0) THEN
C         print output to file
          CALL WDPOUT ( WDMFL, FOUT, MESSFL, SCLU, DSN,
     I                  YDATE, MON, DAY1, MON, DAY2,
     I                  ND, NDUR, LH, SIGDIG, DECPLA, TSFILL, WIDE,
     M                  TYR, QM )
        ELSE IF (RETC.LT.0) THEN
C         problem with this data set, but not stopping analysis
          RETC= 0
        END IF
        IF (MON .NE. MONS(2)) THEN
C         more months to analyze
          MON = MON + 1
          IF (MON .GT. 12) MON = 1
        ELSE
C         finished last requested month, no more
          MORE = 0
        END IF
      IF (MORE .EQ. 1) GO TO 100
C
      RETURN
      END

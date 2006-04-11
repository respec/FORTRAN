C
C
C
      SUBROUTINE   PROFDR
     I                    (MESSFL,WDMFL,IGR,
     M                     DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     Control the calculation of flow-duration statistics,
C     flow-duration tables and flow-duration plots.
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
      INTEGER      I,L,N,I0,I1,I2,I5,I23,I35,I64,SCLU,SGRP,RESP,RETCOD,
     &             DELFG,FOUT,IVAL(35),CVAL(5,3),INUM,RNUM,CNUM,ILEN,
     &             NCI,RESP2,IPLOT,WOUT,TSTUFG,PRECFG,IRET,IRET2,
     &             ISCN,NEWFUN,INQERR,WNOY(23),WNOYD(23)
      REAL         BOUND(2),CR,CLOG,C,CLASS(35),RVAL(1)
      CHARACTER*1  BLNK,BUFF(80,35),FLNAM1(64),NEWFL1(64)
      CHARACTER*8  PTHNAM
      CHARACTER*64 FLNAME,NEWFIL
C
C     + + + INTRINSICS + + +
      INTRINSIC    ALOG10, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, PRNTXT, QRESPI, QRESPM, QRESCN
      EXTERNAL     Q1INIT, QSETI, QSETR, QSTCTF, QSETCO 
      EXTERNAL     Q1EDIT, QGETI, QGETR, QGTCTF, QGETCO
      EXTERNAL     QFOPFN, QFCLOS, ZSTCMA, ZGTRET, CARVAR, CVARAR
      EXTERNAL     CHRCHR, ZIPI, ZIPC, GETFUN, COPYI
      EXTERNAL     PRWMSE, FLODUR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   I0, I1, I2, I5, I23, I35, I64, BLNK
     $     /  0,  1,  2,  5,  23,  35,  64,  ' ' /
C            by default, calculate the 7 basin characteristics percents
      DATA   WNOYD / 1,1,2,2,1,1,2,2,1,1,1,2,1,1,1,1,2,1,1,2,1,1,1/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
      CALL ZIPI (I35, I0, IVAL)
      CALL COPYI ( I23, WNOYD, WNOY )
      RVAL(1) = 0.0
C
C     message file cluster used
      SCLU= 152
C
C     init output options
      IF (IGR.EQ.1) THEN
C       init to output graphics
        IPLOT = 1
      ELSE
C       graphics not available
        IPLOT= 0
      END IF
      WOUT  = 1
      ISCN = 1
C     open default output file
      CALL GETFUN (I1,FOUT)
      FLNAME = 'DURANL.OUT'
      OPEN (UNIT=FOUT,FILE=FLNAME,STATUS='UNKNOWN')
C     init period of record and time step/units flags
      PRECFG= 1
      TSTUFG= 1
C     init to default number of intervals
      NCI = 35
      BOUND(1) = 1.0
      BOUND(2) = 10000.0
C     set up default class intervals
      CR = (BOUND(1)/BOUND(2))**(1.0/33.0)
      CLASS(1)  = 0.0
      CLASS(2)  = BOUND(1)
      CLASS(NCI)= BOUND(2)
      DO 3 N = 1,32
        I = NCI - N
        CLASS(I) = CLASS(I+1)*CR
 3    CONTINUE
C     WRITE(*,*) 'CLASS',CLASS
C
C     round off class intervals
      DO 5 I = 2,NCI
        C = CLASS(I)
        CLOG = ALOG10(C) + 0.001
        IF (CLOG.LT.0.0) CLOG = CLOG - 1
        L = INT(CLOG)
        L = L - 1
        C = (C/(10.0**L)) + 0.5
        CLASS(I) = (INT(C))*(10.0**L)
 5    CONTINUE
C
      RESP = 1
 10   CONTINUE
C       options:  1-Select, 2-Define, 3-Modify, 4-Analyze, 5-Return
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
C       allow previous
        I= 4
        CALL ZSTCMA (I,I1)
C
        GO TO (100,200,300,500,600), RESP
C
 100    CONTINUE
C         select data sets to analyze
          PTHNAM = 'SD      '
          CALL PRWMSE (MESSFL,WDMFL,DSNBMX, PTHNAM,
     M                 DSNBUF,DSNCNT)
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          RESP = RESP + 1
          GO TO 900
C
 200    CONTINUE
C         modify output options
 210      CONTINUE
C           may return here for Oops or for Prev from failed open
            NEWFUN = 0
            IVAL(1) = PRECFG
            IVAL(2) = TSTUFG
            IVAL(3) = IPLOT + 1
            IVAL(4) = ISCN + 1
            IVAL(5) = WOUT
            SGRP= 2
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL QSETCO ( I5, IVAL )
            I = 6
            CALL CVARAR ( I64, FLNAME, I64, FLNAM1 )
            CALL QSTCTF ( I, I64, FLNAM1 )
            CALL Q1EDIT ( IRET )
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              CALL QGETCO ( I5, IVAL )
              PRECFG = IVAL(1)
              TSTUFG = IVAL(2)
              IPLOT  = IVAL(3) - 1
              ISCN   = IVAL(4) - 1
              WOUT   = IVAL(5)
              CALL QGTCTF ( I, I64, NEWFL1 )
              CALL CARVAR ( I64, NEWFL1, I64, NEWFIL )
              INQUIRE (FILE=NEWFIL, NUMBER=NEWFUN, IOSTAT=INQERR)
              IF (INQERR .NE. 0) THEN
C               invalid top-level directory specified
                SGRP = 4
                CALL PRNTXT (MESSFL, SCLU, SGRP)
C               get user exit command value
                CALL ZGTRET (IRET)
                IF (IRET .EQ. 1) THEN
C                 user chose 'Accept'; reset output file to default
C                 close any current output file
                  IF (FOUT .NE. 0) THEN
                    CALL QFCLOS (FOUT, I0)
                    FOUT = 0
                  END IF
                  CALL GETFUN (I1, FOUT)
                  FLNAME = 'FRQNCY.OUT'
                  OPEN (UNIT=FOUT, FILE=FLNAME, STATUS='UNKNOWN')
                END IF
C               prevent the following IF/THEN from being entered
                NEWFUN = FOUT
              END IF
              IF (NEWFUN .NE. FOUT) THEN
C               output to file different than current output file
C               close current output file
                CALL QFCLOS (FOUT,I0)
                FOUT = 0
C               open file for general output
                SGRP = 3
                CALL QFOPFN (MESSFL,SCLU,SGRP,NEWFL1,I0,
     O                       FOUT,RETCOD)
                IF (RETCOD.NE.0) THEN
C                 problem opening file
                  SGRP = 4
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
C                 get user exit command value
                  CALL ZGTRET (IRET)
                  IF (IRET.EQ.1) THEN
C                   user wants to continue, reset output file to default
                    CALL GETFUN (I1,FOUT)
                    FLNAME= 'FRQNCY.OUT'
                    OPEN (UNIT=FOUT,FILE=FLNAME,STATUS='UNKNOWN')
                  END IF
                ELSE
C                 different file opened successfully
                  CALL CARVAR (I64, NEWFL1, I64, FLNAME)
                END IF
              END IF
            ELSE IF (IRET .EQ. -1) THEN
C             Oops, try again
            ELSE
C             assume Prev to main duration menu
              IRET= 1
            END IF
C           try again if Oops or Prev from file open
          IF (IRET .EQ. 2  .OR.  IRET .EQ. -1) GO TO 210
C
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          IF (IPLOT.GE.1 .AND. IGR.EQ.2) THEN
C           user wants graphics, but it is not available
            SGRP= 25
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            IPLOT= 0
          END IF
          RESP = RESP + 1
          GO TO 900
C
 300    CONTINUE
C         class intervals and percents   
          RESP2= 1
 350      CONTINUE
C           options:  1-Standard, 2-User, 3-Percents, 4-Return
            SGRP = 6
            CALL QRESP (MESSFL,SCLU,SGRP,RESP2)
            IF (RESP2 .EQ. 2) THEN
C             User defined number of class intervals
 420          CONTINUE
C               back here on Prev from interval values screen
                IRET2 = 0
                SGRP= 8
                CALL QRESPI (MESSFL,SCLU,SGRP,NCI)
C               get user exit command value
                CALL ZGTRET (IRET)
                IF (IRET.EQ.1) THEN
C                 user wants to continue, enter class intervals
                  INUM= 1
                  RNUM= 1
                  DO 430 I= 1,NCI
                    IVAL(I)= I
 430              CONTINUE
                  SGRP= 9
                  CALL QRESCN (MESSFL,SCLU,SGRP,INUM,RNUM,I1,NCI,I1,
     M                         IVAL,CLASS,CVAL,BUFF)
C                 get user exit command value
                  CALL ZGTRET (IRET2)
                END IF
C               back to # of intervals if Prev
              IF (IRET2 .EQ. 2) GO TO 420
            ELSE IF (RESP2 .EQ. 1) THEN
C             Standard clas intervals, get bounds
 435          CONTINUE
                SGRP = 7
                CALL Q1INIT ( MESSFL, SCLU, SGRP )
                CALL QSETR ( I2, BOUND )
                CALL Q1EDIT ( IRET )
                IF (IRET.EQ.1) THEN
C                 user wants to continue, enter class intervals
                  CALL QGETR ( I2, BOUND )
C                 set standard number of intervals
                  NCI = 35
C                 set up class intervals
                  CR = (BOUND(1)/BOUND(2))**(1.0/33.0)
                  CLASS(1) = 0.0
                  CLASS(2) = BOUND(1)
                  CLASS(NCI)= BOUND(2)
                  DO 440 N = 1,32
                    I = NCI - N
                    CLASS(I) = CLASS(I+1)*CR
 440              CONTINUE
C                 round off class intervals
                  DO 450 I = 2,NCI
                    C = CLASS(I)
                    CLOG = ALOG10(C) + 0.001
                    IF (CLOG.LT.0.0) CLOG = CLOG - 1
                    L = INT(CLOG)
                    L = L - 1
                    C = (C/(10.0**L)) + 0.5
                    CLASS(I) = (INT(C))*(10.0**L)
 450              CONTINUE
                END IF
C               try again if oops
              IF (IRET .EQ. -1) GO TO 435
            ELSE IF (RESP2 .EQ. 3) THEN
C             Percents WNOY: 1-no, 2-yes
 460          CONTINUE
                SGRP = 5
                CALL Q1INIT ( MESSFL, SCLU, SGRP )
                CALL QSETCO ( I23, WNOY )
                CALL Q1EDIT ( IRET )
                IF (IRET .EQ. 1) THEN
C                 user wants to continue
                  CALL QGETCO ( I23, WNOY )
                END IF
              IF (IRET .EQ. -1) GO TO 460
              IF (WOUT .EQ. 1) THEN
C               may be conflict between wout and wnoy(1-23)
                N = 0
                DO 465 I = 1, I23
                  IF (WNOY(I) .GT. 1) N = N + 1
 465            CONTINUE
                IF (N .GT. 0) THEN
C                 wout off, but selected some wnoy, so what...
                  SGRP = 41
                  CALL QRESP ( MESSFL, SCLU, SGRP, WOUT )
                END IF
              END IF
            END IF
C           back to Modify screen if not Return
          IF (RESP2 .NE. 4) GO TO 350
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          RESP = RESP + 1
          GO TO 900
C
 500    CONTINUE
C         do analysis
          IF (DSNCNT.GT.0) THEN
C           data sets to analyze
            CALL FLODUR (MESSFL,SCLU,WDMFL,DSNCNT,DSNBUF,IPLOT,ISCN,
     I                   NCI,CLASS,PRECFG,TSTUFG,FOUT,WOUT,WNOY)   
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
 600    CONTINUE
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
      SUBROUTINE   FLODUR
     I                    (MESSFL,SCLU,WDMFL,DSNCNT,DSN,IPLOT,ISCN,
     I                     NCI,CLASS,PRECFG,TSTUFG,FOUT,WOUT,WNOY)   
C
C     + + + PURPOSE + + +
C     This routine calculates flow-duration statistics and prints
C     the flow-duration table and calls routine to plot the flow-
C     duration data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,WDMFL,DSNCNT,DSN(DSNCNT),IPLOT,ISCN,
     1            NCI,PRECFG,TSTUFG,FOUT,WOUT,WNOY(23)
      REAL        CLASS(NCI)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for input direct access file
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers for analysis
C     IPLOT  - plotting flag, 0- dont plot, 1- log-normal,
C              2- arith-normal, 3- log-arith, 4- arith-log
C     ISCN   - results to screen flag, 0-don't, 1-do
C     NCI    - number of class intervals
C     CLASS  - array of class interval values
C     PRECFG - indicates how to determine period of record
C              1 - use full period of record
C              2 - use common period of record
C              3 - user will specify period for each data set
C     TSTUFG - indicates how to determine time step/units
C              1 - use data set values
C              2 - allow user to specify time step/units
C     FOUT   - Fortran unit number of output file
C     WOUT   - flag to write percentiles as attributes on wdm file
C              1 - no, 2- yes
C     WNOY   - flag for specific percents to be written to wdm file
C              set of 23 different percents
C              1 - don't save in wdm file
C              2 - do save in wdm file
C
C     + + + PARAMETERS + + +
      INTEGER   BUFMAX
      PARAMETER (BUFMAX = 3660)
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      I,J,K,M,N,I0,I1,I6,I50,I24,I80,NUMA(35),INIT,QFLG,
     &             CONT,ERRFLG,SGRP,GRPFLG,SALEN,SAIND,ICHK(7),I60, 
     &             TEMP(12),RESP,TNUM,CNUMA,DTRAN,IRET,RETCOD,
     &             ILEN,LINES,INUM,CNUM,NPTS,IVAL(13),CVAL(2,3),WSID,
     &             TSTEP,TUNITS,SDATIM(6),EDATIM(6),NMIS,IWRT,ICLOS,
     &             LEN,OLEN,DONFG,CORM,LDEVTY,LDEVCD,WNDFLG, IWAIT,
     &             IXTYP,XTYP,SCLUX,NROW,CMPTYP
      REAL         SUMA(35),PCTA(35),CPCTA(35),TSUMA,RVAL(6),
     &             FLOW(BUFMAX)
      CHARACTER*1  TBUFF(80),CTITL(24),LTITLE(80),BLNK,CXLAB(80)
      CHARACTER*70 CBUFF
      CHARACTER*8  WNDNAM(2)
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     TIMDIF, TIMADD, PMXCNW, ZBLDWR, GETTXT
      EXTERNAL     CKDATE, CHRCHR, CTRSTR, QRESPM, CVARAR
      EXTERNAL     QRESP, GRFDUR, WDBSGI, ZMNSST, LENSTR
      EXTERNAL     COPYI, WDTGET, PRNTXI, ZSTCMA, ZGTRET, WTDATE
      EXTERNAL     PMXTXI, PRNTXT, ZIPC, WTFNDT, WMSGTT, PROPLT
      EXTERNAL     GRFDIT, ANPRGT, FDPCTL, GGATXB, GPLBXB
      EXTERNAL     PDNPLT, PLTONE, PSTUPW, DSINFO
C
C     + + + DATA INITIALIZATIONS   + + +
      DATA CTITL/'F','l','o','w',' ','d','u','r','a','t','i','o','n',
     1           ' ','c','u','r','v','e',' ',' ',' ',' ',' '/
      DATA WNDNAM/'Modify','SDAM'/
      DATA ICHK/1,2,1,1,1,1,1/
C
C     + + + OUTPUT FORMATS   + + +
 2000 FORMAT (1H1)
 2001 FORMAT (80A1)
 2004 FORMAT (1X,F9.2,I10,F10.2,I10,F10.2)
 2007 FORMAT (/I7,' values were tagged missing and excluded from',
     &            ' analysis.')
 2011 FORMAT (1H )
 2020 FORMAT (1X,F8.1,5(2X,F9.1))
 2021 FORMAT (1X,F8.1,2X,F9.0,2(2X,F9.1,2X,F9.0))
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I6  = 6
      I24 = 24
      I50 = 50
      I60 = 60
      I80 = 80
      BLNK= ' '
      DTRAN = 1
      ERRFLG= 0
      RETCOD = 0
C
C     get computer type
      I = 1
      CALL ANPRGT (I, CMPTYP)
C
      IF (PRECFG.EQ.2) THEN
C       need to get common time period for analysis
        CORM = 1
        CALL WTDATE (WDMFL,DSNCNT,DSN,CORM,
     O               SDATIM,EDATIM,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         common period found
 100      CONTINUE
C           back here on bad dates
            ERRFLG= 0
C           allow previous
            I= 4
            CALL ZSTCMA (I,I1)
            CALL COPYI (I6,SDATIM,IVAL)
            CALL COPYI (I6,EDATIM,IVAL(7))
            INUM= 12
            SGRP= 10
            CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,I1,
     O                   IVAL,RVAL,CVAL,TBUFF)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             continue
              CALL COPYI (I6,IVAL,SDATIM)
              CALL COPYI (I6,IVAL(7),EDATIM)
C             check order of dates
              CALL CKDATE (SDATIM,EDATIM,
     O                     ERRFLG)
              IF (ERRFLG.GT.0) THEN
C               dates out of range
                IVAL(2)= IVAL(7)
                IVAL(3)= SDATIM(1)
                IVAL(4)= EDATIM(1)
                INUM= 4
                SGRP= 11
                CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I0,I,IVAL)
              ELSE
C               dates are ok
                ERRFLG= 0
C               save dates in temporary variable
                CALL COPYI (I6,SDATIM,TEMP)
                CALL COPYI (I6,EDATIM,TEMP(7))
              END IF
            ELSE
C             user doesnt want to analyze now
              ERRFLG= -1
            END IF
          IF (ERRFLG.GT.0) GO TO 100
C         turn off previous
          I= 4
          CALL ZSTCMA (I,I0)
        ELSE
C         no common period
          SGRP= 12
          CALL PRNTXT (MESSFL,SCLU,SGRP)
          CALL ZGTRET (IRET)
          IF (IRET.EQ.1) THEN
C           set to use full period
            PRECFG= 1
          ELSE
C           dont do analysis
            ERRFLG= -1
          END IF
        END IF
      END IF
C
      IF (ERRFLG.EQ.0) THEN
C       all ok so far
C
        IF (IPLOT .GE. 1) THEN
C         initialize common block for graphics; set device type to screen
          LDEVTY = 1
          CALL GRFDIT (LDEVTY)
C         get device code
          I = 40
          CALL ANPRGT (I, LDEVCD)
        END IF
C
        DO 500 N= 1,DSNCNT
C         do analysis for each data set
          IF (RETCOD.LT.0) THEN
C           had problems on last data set, but ok to try the next one
            RETCOD= 0
          END IF
          IF (RETCOD.EQ.0) THEN
C           always continue unless user 'Interrupts' analysis
C           get title from attributes
            CALL ZIPC (I80,BLNK,LTITLE)
            CALL DSINFO (WDMFL,DSN(N),I60,LTITLE)
C
            DO 120 I = 1,NCI
              NUMA(I) = 0
              SUMA(I) = 0.0
 120        CONTINUE
            NMIS = 0
C
            IF (PRECFG.NE.2) THEN
C             get start/end dates from data set
              CALL WTFNDT (WDMFL,DSN(N),I1,
     O                     J,SDATIM,EDATIM,RETCOD)
            ELSE
C             using common period from above, put saved dates back in place
              CALL COPYI (I6,TEMP,SDATIM)
              CALL COPYI (I6,TEMP(7),EDATIM)
            END IF
            GRPFLG= 0
            IF (PRECFG.EQ.3) THEN
C             user wants to specify dates
              GRPFLG= 1
              INUM= 12
              CALL COPYI (I6,SDATIM,IVAL)
              CALL COPYI (I6,EDATIM,IVAL(7))
            END IF
C           get time step/units on data set
            SALEN= 1
            SAIND= 33
            CALL WDBSGI (WDMFL,DSN(N),SAIND,SALEN,
     O                   TSTEP,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             time step found, now get units
              SAIND= 17
              CALL WDBSGI (WDMFL,DSN(N),SAIND,SALEN,
     O                     TUNITS,RETCOD)
            END IF
            IF (TSTUFG.EQ.2 .OR. RETCOD.NE.0) THEN
C             user wants (or needs) to specify time step/units
              GRPFLG= GRPFLG+ 2
              IF (GRPFLG.EQ.2) THEN
C               only need time step/units and transformation type
                INUM= 1
              ELSE
C               doing dates too
                INUM= 13
              END IF
              IVAL(INUM)= TSTEP
              CNUM= 2
              CVAL(1,1)= TUNITS
              CVAL(2,1)= DTRAN + 1
            END IF
            IF (GRPFLG.GT.0) THEN
C             user needs to specify something
 140          CONTINUE
C               back here on bad date specification
                ERRFLG= 0
C               show which data set
                SGRP= 13
                CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I1,DSN(N))
C               save text
                CALL ZMNSST
C               allow interrupt
                I= 16
                CALL ZSTCMA (I,I1)
                SGRP= 13+ GRPFLG
                CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,CNUM,
     M                       IVAL,RVAL,CVAL,TBUFF)
C               get user exit command value
                CALL ZGTRET (IRET)
                IF (IRET.EQ.1) THEN
C                 user wants to continue
                  IF (GRPFLG.EQ.2) THEN
                    TSTEP = IVAL(1)
                    TUNITS= CVAL(1,1)
                    DTRAN = CVAL(2,1) -1
                  ELSE
                    CALL COPYI (I6,IVAL,SDATIM)
                    CALL COPYI (I6,IVAL(7),EDATIM)
C                   check order of dates
                    CALL CKDATE (SDATIM,EDATIM,
     O                           ERRFLG)
                    IF (ERRFLG.GT.0) THEN
C                     invalid dates
                      SGRP= 17
                      CALL PRNTXT (MESSFL,SCLU,SGRP)
                    END IF
                    IF (GRPFLG.EQ.3) THEN
                      TSTEP = IVAL(13)
                      TUNITS= CVAL(1,1)
                      DTRAN = CVAL(2,1) - 1
                    END IF
                  END IF
                ELSE
C                 user wants out of analysis
                  RETCOD= 1
                END IF
              IF (ERRFLG.GT.0) GO TO 140
C             turn off interrupt
              I= 16
              CALL ZSTCMA (I,I0)
            END IF
C
            IF (RETCOD.EQ.0) THEN
C             ok so far
              CALL COPYI (I6,EDATIM,TEMP(7))
 200          CONTINUE
C               determine number of values
                CALL COPYI (I6,SDATIM,TEMP)
                CALL TIMDIF (SDATIM,TEMP(7),TUNITS,TSTEP,NPTS)
                IF (NPTS .GT. BUFMAX) THEN
C                 can only get BUFMAX at a time
                  NPTS = BUFMAX
                END IF
C
C               file being read
                SGRP = 23
                CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,J)
C               begin to fill plot array
                QFLG = 30
                CALL WDTGET (WDMFL,DSN(N),TSTEP,SDATIM,
     I                       NPTS,DTRAN,QFLG,TUNITS,
     O                       FLOW(1),RETCOD)
                IF (RETCOD .NE. 0) THEN
C                 error reading file, error code &
                  SGRP = 24
                  CALL PRNTXI (MESSFL,SCLU,SGRP,RETCOD)
                ELSE
C                 begin loop to fill class intervals.
                  DO 240 J = 1,NPTS
                    IF (FLOW(J) .GE. 0.0) THEN
                      I = NCI + 1
 220                  CONTINUE
                        I = I - 1
                      IF (FLOW(J).LT.CLASS(I) .AND. I.GT.1) GO TO 220
C
                      NUMA(I) = NUMA(I) + 1
                      SUMA(I) = SUMA(I) + FLOW(J)
                    ELSE
C                     count missing values
                      NMIS = NMIS + 1
                    END IF
 240              CONTINUE
C
C                 check adjusted start date with end date
                  CALL TIMADD (TEMP,TUNITS,TSTEP,NPTS,
     O                         SDATIM)
                  CALL COPYI (I6,TEMP(7),EDATIM(1))
                  CALL CKDATE (SDATIM,EDATIM,ERRFLG)
C                 -1 =sdatim<edatim,  0=they are equal,  1=sdatim>edatim
C                 go back to get more data.
                END IF
              IF (ERRFLG.LT.0 .AND. RETCOD.EQ.0) GO TO 200
            END IF
C
            IF (RETCOD.EQ.0) THEN
C             still ok
              TSUMA= 0.0
              TNUM = 0
C             write heading
              CALL ZIPC (I60,BLNK,TBUFF)
              CALL CHRCHR (I24,CTITL,TBUFF(16)) 
              WRITE(FOUT,2000)
              WRITE (FOUT,2001) (TBUFF(I),I=1,I60) 
              WRITE (FOUT,2011)
              ILEN = LENSTR (I80,LTITLE)
              IF (ILEN .LT. 50) THEN
                CALL CTRSTR (I50,LTITLE(1))
              END IF
              WRITE (FOUT,2001) BLNK, (LTITLE(I),I=1,79)  
              WRITE (FOUT,2011)
              SGRP= 20
              INIT= 1
 260          CONTINUE
                ILEN= 80
                CALL WMSGTT (MESSFL,SCLU,SGRP,INIT,
     M                       ILEN,
     O                       TBUFF,CONT)
                INIT= 0
                WRITE(FOUT,2001) TBUFF
              IF (CONT.EQ.1) GO TO 260
C
              DO 280 I = 1,NCI
                TNUM = TNUM + NUMA(I)
                TSUMA = TSUMA + SUMA(I)
 280          CONTINUE
              CNUMA  = TNUM
              PCTA(1)= 0.0
              DO 300 I = 1,NCI
                PCTA(I) = 100.0* REAL(NUMA(I))/ REAL(TNUM)
                IF (I.GT.1) THEN
                  CNUMA = CNUMA - NUMA(I-1)
                END IF
                CPCTA(I) = 100.0*REAL(CNUMA)/REAL(TNUM)
                WRITE(FOUT,2004) CLASS(I),NUMA(I),PCTA(I),CNUMA,CPCTA(I)
 300          CONTINUE
C
              WRITE(FOUT,2001)TBUFF
              IF (NMIS .GT. 0) THEN
C               add missing values to output file
                WRITE(FOUT,2007) NMIS
              END IF
C
              IF (ISCN .GE. 1) THEN
C               output results to screen
                IWRT = 1
                INIT = 1
                LEN = 70
                DO 382 I = 1,4
                  SGRP = (ISCN-1)*4 + 25 + I
                  OLEN = 78
                  CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,TBUFF)
                  IF (I .GT. 1) INIT = 0
                  CALL ZBLDWR (OLEN,TBUFF,INIT,IWRT,DONFG)
 382            CONTINUE
C               compute number of rows (3/row) based on NCI
                NROW = (NCI-1)/3 + 1
                DO 390 I = 1,NROW
C                 at least one class interval for this row
                  J = 0
                  DO 385 K = 1,3
                    M = I + (K-1)*NROW
                    IF (M .LE. NCI) THEN
                      J = J + 1
                      RVAL(J) = CLASS(M)
                      J = J + 1
                      IF (ISCN .EQ. 1) THEN
                        RVAL(J) = PCTA(M)
                      ELSE IF (ISCN .EQ. 2) THEN
                        RVAL(J) = NUMA(M)
                      ELSE
                        RVAL(J) = CPCTA(M)
                      END IF
                    ELSE
                      J = J + 1
                      RVAL(J) = 0.0
                      J = J + 1
                      RVAL(J) = 0.0
                    END IF
 385              CONTINUE
                  IF (ISCN .EQ. 2) THEN
                    WRITE (CBUFF,2021) (RVAL(M),M=1,6)
                  ELSE
                    WRITE (CBUFF,2020) (RVAL(M),M=1,6)
                  END IF
                  CALL CVARAR (LEN,CBUFF,LEN,TBUFF)
                  IF (I .EQ. NROW) IWRT = 0
                  CALL ZBLDWR(LEN,TBUFF,INIT,IWRT,DONFG)
 390            CONTINUE
              END IF 
C           
C             compute and output percentiles
              WRITE (FOUT,2011)
              WRITE (FOUT,2000)
              WRITE (FOUT,2011)
              WRITE (FOUT,2001) BLNK, (LTITLE(I),I=1,79)  
              WRITE (FOUT,2011)
              CALL FDPCTL (MESSFL, SCLU, NCI, CLASS, CPCTA,
     I                     FOUT, WOUT, WDMFL, DSN(N), ISCN, WNOY)
              WRITE (FOUT,2011)
              WRITE (FOUT,2011)
C
              LINES = 1
              IF (IPLOT.GE.1) THEN
C               do plotting, set defaults
                CALL GRFDUR (MESSFL,SCLU,CLASS,CPCTA,
     I                       LTITLE,LINES,NCI,IPLOT)
                WSID = 1
 400            CONTINUE
C                 do plotting menu
                  SGRP= 18
                  CALL QRESP (MESSFL,SCLU,SGRP,RESP)
                  IF (RESP.EQ.1) THEN
C                   modify stuff
                    WNDNAM(1)= 'Modify'
                    WNDNAM(2)= 'SDAM'
                    CALL GGATXB (IXTYP)
                    CALL PROPLT (MESSFL,ICHK,WNDNAM,WNDFLG)
                    CALL GGATXB (XTYP)
                    IF (IXTYP .NE. XTYP) THEN
C                     user changed x axis type so change label
                      LEN = 80
                      SCLUX = 153
                      SGRP = 40 + XTYP
                      CALL GETTXT (MESSFL,SCLUX,SGRP,LEN,CXLAB)
                      CALL GPLBXB (CXLAB)
                    END IF
                    IF (WNDFLG .EQ. 1) THEN
C                     user changed device
                      IF (CMPTYP .NE. 1) THEN
C                       not pc, so close old workstation
                        IWAIT = 0
                        ICLOS = 1
                        CALL PDNPLT (WSID,ICLOS,IWAIT)
                      END IF
                    END IF
                  ELSE IF (RESP.EQ.2) THEN
C                   generate the plot
                    IWAIT = 0
                    CALL PSTUPW (WSID, RETCOD)
                    CALL PLTONE
                    IF (CMPTYP .EQ. 1) THEN
C                     pc, always close workstation after plot
                      ICLOS = 1
                    ELSE
C                     for other types, just deactivate workstation
                      ICLOS = 0
                    END IF
                    CALL PDNPLT (WSID,ICLOS,IWAIT)
                  END IF
                IF (RESP.NE.3) GO TO 400
              END IF
            END IF
          END IF
 500    CONTINUE
        IF (CMPTYP .NE. 1) THEN
C         not pc, need to close workstation
          ICLOS = 1
          IWAIT = 0
          CALL PDNPLT (WSID,ICLOS,IWAIT)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GRFDUR
     I                    (MESSFL,SCLU,CLASS,CPCTA,
     I                     LTITLE,NCRV,NCI,IPLOT)
C
C     + + + PURPOSE + + +
C     This routine fills the common block CPLOT to plot a flow duration
C     curve using DISSPLA.
C
C     + + + DUMMY VARIABLES + + +
      INTEGER     MESSFL,SCLU,NCRV ,NCI, IPLOT
      REAL        CLASS(35),CPCTA(35)
      CHARACTER*1 LTITLE(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     CLASS  - top of class interval
C     CPCTA  - probabilities for first time series
C     LTITLE - title generated from data set station id or ts type
C     NCRV   - number of time series to plot
C     NCI    - number of class intervals
C     IPLOT  - plotting flag, 0- dont plot, 1- log-normal,
C              2- arith-normal, 3- log-arith, 4- arith-arith
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I20,I80,SGRP,NCIM1,ILEN,NVAR,WHICH(2),
     &            BVALFG(4),IY,IX,RETCOD,TICS(4),YTYPE(2),XTYPE,CTYPE,
     &            I240, LNTYP, COLOR, PATRN, SYMBL, TRANSF(2)
      CHARACTER*1 CINVL(20),CPROB(20),TITL(240),YLABL(80),YXLABL(80),
     &            ALAB(80), LBV(20,2), BLNK
      REAL        Y(35), X(35), PLMN(4), PLMX(4), YMIN(2), YMAX(2),
     &            ALEN
C
C     + + + FUNCTIONS + + +
      REAL        GAUSEX
C
C     + + + EXTERNALS + + +
      EXTERNAL    GAUSEX, CHRCHR, GETTXT, SCALIT
      EXTERNAL    GPVAR, GPLABL, GPSCLE, GPDATR, GPNCRV, ZIPC, GPCURV
      EXTERNAL    GPWCXY, GPLBXB, LFTSTR 
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CINVL/'C','l','a','s','s',' ','i','n','t','e','r','v','a',
     #           'l',' ',' ',' ',' ',' ',' '/
      DATA CPROB/'N','o','r','m','a','l',' ','d','e','v','i','a','t',
     #            'e','s',' ',' ',' ',' ',' '/
      DATA LNTYP,SYMBL,COLOR,PATRN/1,0,1,0/
      DATA ALAB/80*' '/,   BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I20 = 20
      I80 = 80
      I240 = 240
C
C     variable 1 = percents for flow A
C     variable 2 = flow class intervals
      NVAR = NCRV + 1
      CALL GPNCRV (NCRV,NVAR)
C
      ILEN = 80
      SGRP = 21
      CALL GETTXT (MESSFL,SCLU,SGRP,ILEN,YLABL)
      ILEN = 80
      SGRP = 22
      CALL GETTXT (MESSFL,SCLU,SGRP,ILEN,YXLABL)
C     set up plot title
      CALL ZIPC (I240,BLNK,TITL)
      CALL CHRCHR (I80,LTITLE,TITL)
      CALL LFTSTR (I80,TITL(1))
C
      IF (IPLOT .LE. 2) THEN
        XTYPE = 6
        TRANSF(2) = 1
      ELSE
        XTYPE = 1
        TRANSF(2) = 1
      END IF
      IF (IPLOT.EQ. 1 .OR. IPLOT .EQ. 3) THEN
        YTYPE(1) = 2
        TRANSF(1) = 2
      ELSE
        YTYPE(1) = 1
        TRANSF(1) = 1
      END IF
      YTYPE(2) = 0
      CTYPE = 6
      ALEN = 0.0
C     set variable labels
      CALL GPLABL (XTYPE,YTYPE,ALEN,YLABL,YXLABL,ALAB,TITL)
      CALL GPLBXB (YXLABL)
C
      NCIM1 = NCI - 1
      DO 15 I = 1,NCIM1
        Y(I) = CLASS(I+1)
        IF (XTYPE .EQ. 6) THEN
          X(I) = -GAUSEX(0.01*CPCTA(I+1))
        ELSE
          X(I) = CPCTA(I+1)
        END IF
 15   CONTINUE
      IY = 1
      CALL GPDATR (IY,I1,NCIM1,Y,RETCOD)
C      CALL GPDATR (NCRV,IY,I1,NCIM1,Y,RETCOD)
      IX = 2
      CALL GPDATR (IX,NCI,NCIM1,X,RETCOD)
C      CALL GPDATR (NCRV,IX,NCI,NCIM1,X,RETCOD)
C
      CALL GPWCXY (NCRV,IY,IX)
      WHICH(1) = 1
      WHICH(2) = 4
      YMIN(1) = CLASS(2)
      YMAX(1) = CLASS(NCI)
      YMAX(2) = X(1)
      YMIN(2) = X(NCIM1)
      CALL CHRCHR (I20,CPROB,LBV(1,2))
      CALL CHRCHR (I20,CINVL,LBV(1,1))
C
C     set curve type
      CALL GPCURV (CTYPE,LNTYP,SYMBL,COLOR,PATRN,LBV)
      CALL GPVAR (YMIN,YMAX,WHICH,TRANSF,LBV)
C
C     generate axis mins and maxs
      IF (XTYPE .EQ. 1) THEN
        CALL SCALIT (XTYPE,YMIN(2),YMAX(2),
     O               PLMN(4),PLMX(4))
      ELSE
        PLMN(4) = -3.0
        PLMX(4) = 3.0
      END IF
      CALL SCALIT (YTYPE(1),YMIN(1),YMAX(1),
     O             PLMN(1),PLMX(1))
      BVALFG(1)= 1
      BVALFG(2)= 1
      BVALFG(3)= 4
      BVALFG(4)= 4
      TICS(1) = 10
      TICS(4) = 10
      CALL GPSCLE (PLMN,PLMX,TICS,BVALFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GRFDIT
     I                    (LDEVTY)
C
C     + + + PURPOSE + + +
C     This routine initializes the graphics buffer for
C     flow-duration plots.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LDEVTY
C
C     + + + ARGUMENT DEFINITION + + +
C     LDEVTY - graphics device type 1-screen, 2-printer, 3-plotter
C              4,5-meta files
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DEVTYP, DEVCOD, IND
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT, GPDEVC, GPINIT
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize plot common block
      CALL GPINIT
C
C     set device type
      DEVTYP= LDEVTY
      IND = 39+ LDEVTY
      CALL ANPRGT (IND,DEVCOD)
      CALL GPDEVC (DEVTYP,DEVCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   FDPCTL
     I                    (MESSFL, SCLU, NCI, CLASS, CPCT,
     I                     FOUT, WOUT, WDMSFL, DSN, ISCN, WNOY)
C
C     + + + PURPOSE + + + 
C     This routine computes percentile of flow and outputs results
C     to screen, file or as wdm attributes. Uses linear interpolation
C     from table of values.
C
C     + + + DUMMY ARGUMENTS + + + 
      INTEGER   MESSFL, SCLU, NCI, WOUT, FOUT, ISCN, DSN, WDMSFL,
     $          WNOY(23)
      REAL      CLASS(NCI), CPCT(NCI)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL
C     SCLU
C     NCI
C     CLASS
C     CPCT
C     FOUT
C     WOUT   -
C     WDMSFL
C     DSN
C     ISCN 
C     WNOY   - flag for specific percents to be written to wdm
C              set of 23 different percents
C              1 - don't write to wdm file
C              2 - save in wdm file
C
C     + + + LOCAL VARIABLES + + + 
      INTEGER   I, N, NPCT, SALEN, SGRP, ERRSUM(2), RETCOD, INDX(23),
     $          IRET, NDX, OFF
      REAL      FLOW(23), PTILE(23), FRACT, DIFF
C
C     + + + LOCAL DEFINITIONS + + +
C     ERRSUM - array containing counts and retcod for attribute writes
C              (1) - number of attributes writes attempted
C              (2) - number of unsuccessfull writes to wdm
C              (3) - last bad return code from attribute write
C
C     + + + EXTERNALS + + +
      EXTERNAL  Q1INIT, QSETR, QSETI, Q1EDIT, ZSTCMA
      EXTERNAL  WDBSAR
C
C     + + + FORMATS + + +
 2001 FORMAT (    10X, 'Percent time value was exceeded',
     $        //, 10X, '      %            FLOW  ',
     $         /, 10X, '     ---       ----------',
     $         /,(10X, F8.0, 5X, F12.2))
 2010 FORMAT ( /, ' Warning:  error writting attribute to data set',
     $         /, '          ', I10, ' = dsn',
     $         /, '          ', I10, ' = index',
     $         /, '          ', F10.2, ' = value',
     $         /, '          ', I10, ' = retcod ' )
C
C     + + + DATA INITIALIZATIONS + + + 
      DATA NPCT, NDX, OFF
     $    /  23,   4,   0 /
      DATA PTILE / 99.0, 98.0, 95.0, 90.0, 85.0, 80.0,
     $             75.0, 70.0, 65.0, 60.0, 55.0, 50.0,
     $             45.0, 40.0, 35.0, 30.0, 25.0, 20.0,
     $             15.0, 10.0,  5.0,  2.0,  1.0 /
      DATA INDX  /  427,  428,  211,  212,  429,  430,
     $              213,  214,  431,  432,  433,  215,
     $              434,  433,  436,  437,  216,  438,
     $              439,  217,  440,  441,  442 /
C
C     + + + END SPECIFICATIONS + + +
C
C     turn off Prev
      CALL ZSTCMA ( NDX, OFF )
    
C     compute percentiles
      DO 50 N = 1,NPCT
C       find location in array for percentile
        I = 0
 20     CONTINUE
          I = I + 1
        IF (CPCT(I) .GT. PTILE(N) .AND. I .LT. NCI) GO TO 20
C
C       linearly interpolate
        DIFF = CLASS(I) - CLASS(I-1) 
        FRACT = (CPCT(I) - PTILE(N))/(CPCT(I) - CPCT(I-1))
        FLOW(N) = CLASS(I) - FRACT*DIFF 
 50   CONTINUE
C
C     output table to file
      WRITE (FOUT,2001) (PTILE(N),FLOW(N),N=1,NPCT)
c
      IF (ISCN .GE. 1) THEN
C       send results to the screen
        SGRP = 38
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        CALL QSETR  ( NPCT, FLOW )
        CALL Q1EDIT ( IRET )
      END IF      
C
      IF (WOUT .EQ. 2) THEN
C       write out as attributes
        SALEN = 1
        ERRSUM(1) = 0
        DO 60 N = 1,NPCT
          IF (WNOY(N) .EQ. 2) THEN
C           save this percent in wdm file
            CALL WDBSAR (WDMSFL,DSN,MESSFL,INDX(N),SALEN,FLOW(N),
     O                   RETCOD)
            IF (RETCOD .NE. 0) THEN
C             problem saving the attribute
              WRITE (99,2010) DSN, INDX(N), FLOW(N), RETCOD
              ERRSUM(1) = ERRSUM(1) + 1
              ERRSUM(2) = RETCOD
            END IF
          END IF
 60     CONTINUE
        IF (ERRSUM(1) .GT. 0) THEN
C         & of 7 attributes not written to WDM file
          I = 2
          SGRP = 39
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
          CALL QSETI ( I, ERRSUM)
          CALL Q1EDIT ( IRET )
        END IF
      END IF
C
      RETURN
      END

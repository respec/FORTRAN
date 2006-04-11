C
C
C
      SUBROUTINE   WSGSIM
     I                   (MESSFL,WDMSFL,MXSEN,UCIPTH,SCTRID,UEDFG,
     M                    SACTIV,UNSAVE,JSTSAV,TMPNAM,EMFG)
C
C     + + + PURPOSE + + +
C     manage scenarios and perform simulation in scenario generators
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,MXSEN,UNSAVE,JSTSAV,SACTIV,
     1              WDMSFL,EMFG,SCTRID,UEDFG
      CHARACTER*8   TMPNAM
      CHARACTER*64  UCIPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     WDMSFL - wdm data file unit number
C     MXSEN  - maximum number of scenarios
C     UCIPTH - path to uci files for this basin
C     UEDFG  - user allowed to edit UCI file flag
C     UNSAVE - flag indicating some basin data may be unsaved
C     JSTSAV - flag indicating if active scenario has been saved
C     TMPNAM - name of new scenario not yet saved
C     EMFG   - english/metric units flag (1-eng,2-metric)
C     SACTIV - active scenario number
C     SCTRID - scenario translator id
C
C     + + + PARAMETERS + + +
      INCLUDE  'pmxts.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SCLU,SGRP,RESP,IDUM,I64,FOUND,TMPSEN,
     1              I4,I0,I,I1,I3,UCIEXT(MXTS),NUCIS,STLINE,
     2              I8,J,ILEN,IPOS,K,NSEN
      CHARACTER*1   STRIN1(8*MXTS)
      CHARACTER*8   PTHNAM(1),STRING,USENNM(MXTS),CGNAME
C
C     + + + SAVE VARIABLES + + +
      INTEGER       FILES(15)
      SAVE          FILES
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, PRNTXT, SSACTI, UCIMOD, ZQUIET
      EXTERNAL     CVARAR, SSMODI, STSPEC
      EXTERNAL     CARVAR, SSCOPY, SCOMPU, SSDELE
      EXTERNAL     GGTGLV, GGTGLA, GSTGLV, SSSAVE
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I3    = 3
      I4    = 4
      I8    = 8
      I64   = 64
      SCLU  = 56
C
C     look through scenarios making sure corresponding uci files exist
      STRING = '?SCENARI'
      CALL CVARAR (I8,STRING,I8,STRIN1)
      CALL GGTGLV (I8,I0,
     M             STRIN1,
     O             ILEN)
C
      CGNAME= 'SCENARIO'
      NSEN  = ILEN/I8
      NUCIS = 0
      DO 3 I=1,NSEN
        CALL GGTGLA (CGNAME,I,
     O               UCIEXT(I))
        IF (UCIEXT(I).EQ.1) THEN
C         uci file exists for this, add to buffer
          NUCIS = NUCIS + 1
          IPOS = (8*(I-1))+1
          CALL CARVAR (I8,STRIN1(IPOS),I8,STRING)
          USENNM(NUCIS) = STRING
        END IF
 3    CONTINUE
C
 5    CONTINUE
C       scenario options loop
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        GO TO (10,30,40,50,60,70,80,90),RESP
C
 10     CONTINUE
C         activate existing scenario
          IF (NUCIS.GT.0) THEN
C           at least one scenario exists, go ahead
            STLINE = 2
            CALL SSACTI (MESSFL,SCLU,MXSEN,WDMSFL,UCIPTH,STLINE,
     M                   NUCIS,USENNM,SACTIV,JSTSAV,FILES)
          ELSE
C           no scenarios exist, tell user somethings wrong
            SGRP = 14
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 30     CONTINUE
C         delete scenario
          IF (NUCIS.GT.1) THEN
C           at least two scenarios exist, go ahead
            STLINE = 2
            CALL SSDELE (MESSFL,SCLU,MXSEN,MXTS,STLINE,
     M                   NUCIS,SACTIV,UNSAVE,USENNM)
          ELSE
C           only one scenario exists, tell user
            SGRP = 30
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 40     CONTINUE
C         modify current scenario
          IF (SACTIV .GT. 0) THEN
C           something to modify
            IF (SCTRID.EQ.1) THEN
C             urban scenario translator desired
              IF (SACTIV .LE. NSEN) THEN
C               regular case with established scenario
                CALL SSMODI (MESSFL,USENNM(SACTIV))
              ELSE IF (SACTIV .GT. NSEN) THEN
C               case with new scenario
                CALL SSMODI (MESSFL,TMPNAM)
              END IF
              UNSAVE = 1
              JSTSAV = 0
            ELSE IF (SCTRID.EQ.2) THEN
C             special actions scenario translator desired
              IF (SACTIV .LE. NSEN) THEN
C               regular case with established scenario
                CALL STSPEC (MESSFL,USENNM(SACTIV))
              ELSE IF (SACTIV .GT. NSEN) THEN
C               case with new scenario
                CALL STSPEC (MESSFL,TMPNAM)
              END IF
              UNSAVE = 1
              JSTSAV = 0
            ELSE
C             give not available, use edit message
              SGRP= 35
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            END IF
          ELSE
C           nothing to modify
            SGRP= 36
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 50     CONTINUE
C         edit uci file
          IF (SACTIV .GT. 0) THEN
C           something to edit
            IF (UEDFG.EQ.1) THEN
C             user allowed to edit
              PTHNAM(1) = 'ScE'
              IDUM = 0
              EMFG = 1
              CALL UCIMOD (MESSFL,MESSFL,PTHNAM(1),
     M                     IDUM,EMFG)
              UNSAVE = 1
              JSTSAV = 0
            ELSE
C             user should use translator option
              SGRP = 51
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            END IF
          ELSE
C           nothing to edit
            SGRP= 50
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 60     CONTINUE
C         create new scenario from current scenario
          IF (SACTIV .GT. 0) THEN
C           something to copy
            STLINE = 2
            CALL SSCOPY (MESSFL,SCLU,MXSEN,MXTS,STLINE,
     M                   SACTIV,UNSAVE,JSTSAV,NSEN,NUCIS,USENNM,TMPNAM)
          ELSE
C           nothing to copy
            SGRP= 60
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 70     CONTINUE
C         save current scenario, write uci file
          IF (SACTIV.GT.0) THEN
C           scenario to save
            CALL SSSAVE (MESSFL,SCLU,UCIPTH,SACTIV,TMPNAM,MXTS,
     M                   JSTSAV,NUCIS,USENNM)
          ELSE
C           no uci file in memory to output
            SGRP= 70
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 80     CONTINUE
C         simulate
          IF (SACTIV.GT.0) THEN
C           a scenario is available
            IF (UNSAVE.EQ.0 .OR. JSTSAV.EQ.1) THEN
C             it is not unsaved, ok to simulate
              PTHNAM(1) = 'Sc'
              CALL SCOMPU (MESSFL,WDMSFL,FILES,PTHNAM)
            ELSE
C             tell user need to save first
              SGRP = 39
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            END IF
          ELSE
C           tell user no scenario specified yet
            SGRP = 40
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 100
C
 90     CONTINUE
C         done, return to main menu
          CALL ZQUIET
          GO TO 100
C
 100    CONTINUE
      IF (RESP .LE. 7) GO TO 5
C
C     are there any scenario names in global that should not be?
      DO 250 I = 1,NSEN
        IF (UCIEXT(I).EQ.1) THEN
C         check to see if in local
          IPOS = (8*(I-1))+1
          CALL CARVAR (I8,STRIN1(IPOS),I8,STRING)
          FOUND = 0
          DO 200 J = 1,NUCIS
            IF (USENNM(J).EQ.STRING) THEN
              FOUND = 1
            END IF
 200      CONTINUE
          IF (FOUND.EQ.0) THEN
C           this scenario should be taken out of global
            UCIEXT(I) = -1
          END IF
        END IF
 250  CONTINUE
C     clear out any scenarios marked for deletion
      TMPSEN = NSEN
      DO 270 I = 1,TMPSEN
        IF (UCIEXT(I).EQ.-1) THEN
C         delete this one
          DO 275 J = I,NSEN-1
C           move all scenario names in array down
            DO 277 K = 1,8
C             eight characters per name
              IPOS = ((J-1)*8)+K
              STRIN1(IPOS) = STRIN1(IPOS+8)
 277        CONTINUE
            UCIEXT(J) = UCIEXT(J+1)
 275      CONTINUE
          UCIEXT(NSEN) = 0
          NSEN = NSEN - 1
        END IF
 270  CONTINUE
C     are there any scenario names local that are not in global?
      DO 300 I = 1,NUCIS
        FOUND = 0
        DO 280 J = 1,NSEN
          IPOS = (8*(J-1))+1
          CALL CARVAR (I8,STRIN1(IPOS),I8,STRING)
          IF (USENNM(I).EQ.STRING) THEN
            FOUND = 1
          END IF
 280    CONTINUE
        IF (FOUND.EQ.0) THEN
C         need to add to global list
          IPOS = (8*NSEN)+1
          CALL CVARAR (I8,USENNM(I),I8,STRIN1(IPOS))
          NSEN = NSEN + 1
          UCIEXT(NSEN) = 1
        END IF
 300  CONTINUE
C
C     put back global scenario names
      STRING = 'SCENARIO'
      ILEN = I8*NSEN
      CALL GSTGLV (STRING,I8,ILEN,STRIN1,NSEN,UCIEXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSACTI
     I                   (MESSFL,SCLU,MXSEN,WDMSFL,UCIPTH,STLINE,
     M                    NSEN,SENNAM,SACTIV,JSTSAV,FILES)
C
C     + + + PURPOSE + + +
C     activate existing scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,MXSEN,SCLU,NSEN,SACTIV,JSTSAV,WDMSFL,
     $              FILES(15),STLINE
      CHARACTER*8   SENNAM(MXSEN)
      CHARACTER*64  UCIPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen message cluster
C     MXSEN  - maximum number of scenarios
C     UCIPTH - path to uci files
C     NSEN   - number of scenarios
C     SENNAM - scenario name
C     SACTIV - active scenario number
C     JSTSAV - flag indicating if active scenario has been saved
C     WDMSFL - wdm file unit number
C     FILES  - array of file unit numbers
C     STLINE - line of status window where scenario name is to be shown
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,RESP,I,IRET,I0,I1,I8,I64,
     1              IDUM,EMFG,I4,LOSES,ISEN
      CHARACTER*1   BLNK,SENOPS(8,20)
      CHARACTER*8   PTHNAM(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, QRSPIN, CVARAR, ZIPC
      EXTERNAL     SSREAD, ZSTCMA, ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I4  = 4
      I8  = 8
      I64 = 64
      IDUM= 0
      PTHNAM(1) = 'Sc'
C
      LOSES = 1
      IF (SACTIV.GT.0 .AND. JSTSAV.EQ.0) THEN
C       already data in memory and it hasnt just been saved
        LOSES = 0
        SGRP= 11
        IRET= 2
        CALL QRESP (MESSFL,SCLU,SGRP,
     M              IRET)
        IF (IRET .EQ. 1) THEN
C         user is willing to lose unsaved data
          LOSES= 1
        END IF
      END IF
C
      IF (LOSES .EQ. 1) THEN
C       okay to continue, build menu
C       clear scen option buffer
        I   = MXSEN*8
        BLNK= ' '
        CALL ZIPC (I,BLNK,SENOPS)
C
C       build scenario option character strings
        DO 10 I = 1,NSEN
          CALL CVARAR (I8,SENNAM(I),I8,
     1                 SENOPS(1,I))
 10     CONTINUE
        I = 8
        CALL QRSPIN (NSEN,I,SENOPS)
C
C       allow previous
        I = 4
        CALL ZSTCMA (I,I1)
C
        SGRP = 10
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        CALL ZGTRET (IRET)
C
C       turn previous off
        I = 4
        CALL ZSTCMA (I,I0)
C
        IF (IRET.EQ.1 .AND. RESP.NE.SACTIV) THEN
C         user wants to continue, read current scenario
          ISEN = RESP
          CALL SSREAD (MESSFL,SCLU,WDMSFL,UCIPTH,STLINE,
     I                 PTHNAM,ISEN,SENNAM(ISEN),
     M                 SACTIV,JSTSAV,EMFG,FILES)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSDELE
     I                   (MESSFL,SCLU,MXSEN,MXTS,STLINE,
     M                    NUCIS,SACTIV,UNSAVE,USENNM)
C
C     + + + PURPOSE + + +
C     delete existing scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,MXSEN,SCLU,SACTIV,UNSAVE,MXTS,NUCIS,STLINE
      CHARACTER*8   USENNM(MXTS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen message cluster
C     MXSEN  - maximum number of scenarios
C     SACTIV - active scenario number
C     UNSAVE - flag indicating some basin data may be unsaved
C     MXTS   - max number of time series
C     USENNM - array of scenario names
C     NUCIS  - number of uci files available
C     STLINE - line of status window where scenario name is to be shown
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,I,IRET,I0,I1,I8
      INTEGER       CNUM,PREVFG,DELNUM,CLEN(3)
      CHARACTER*1   BLNK,SENOPS(8,20),CTXT(75)
      CHARACTER*78  STBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, QRSPIN, CVARAR, ZSTCMA, ZGTRET, ZIPC
      EXTERNAL     ZSTADD, Q1INIT, Q1EDIT, QSETCT, DELSCN
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I8 = 8
C
C     clear scen option buffer
      I   = MXSEN*8
      BLNK= ' '
      CALL ZIPC (I,BLNK,SENOPS)
C     build scenario option character strings
      DO 32 I = 1,NUCIS
        CALL CVARAR (I8,USENNM(I),I8,
     1               SENOPS(1,I))
 32   CONTINUE
 34   CONTINUE
        PREVFG = 0
        I = 8
        CALL QRSPIN (NUCIS,I,SENOPS)
C       allow previous
        I = 4
        CALL ZSTCMA (I,I1)
        SGRP = 31
        CALL QRESP (MESSFL,SCLU,SGRP,DELNUM)
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1) THEN
C         make sure user wants to delete this scenario
          SGRP = 32
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          CNUM   = 1
          CLEN(1)= 8
          CALL CVARAR (I8,USENNM(DELNUM),I8,CTXT)
          CALL QSETCT (CNUM,CLEN,I8,CTXT)
          CALL Q1EDIT (IRET)
          IF (IRET.NE.2) THEN
C           user wants to continue
C           batch routine to delete scenario
            CALL DELSCN (MESSFL,USENNM(DELNUM))
C
            IF (DELNUM.LT.NUCIS) THEN
C             not last scenario deleted, need to pack
              DO 37 I = DELNUM,NUCIS-1
                USENNM(I) = USENNM(I+1)
 37           CONTINUE
            END IF
            NUCIS= NUCIS - 1
            UNSAVE = 1
            IF (DELNUM.EQ.SACTIV) THEN
C             just deleted our active scenario, no longer active
              SACTIV = 0
              STBUFF = ' '
              CALL ZSTADD (STLINE,STBUFF)
            ELSE IF (DELNUM.LT.SACTIV) THEN
C             just deleted a scenario, decrement sactiv
              SACTIV = SACTIV - 1
            END IF
          ELSE
C           user mistake, wants back to delete screen
            PREVFG = 1
          END IF
        END IF
      IF (PREVFG.EQ.1) GO TO 34
C     turn previous off
      I = 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSCOPY
     I                   (MESSFL,SCLU,MXSEN,MXTS,STLINE,
     M                    SACTIV,UNSAVE,JSTSAV,NSEN,NUCIS,USENNM,TMPNAM)
C
C     + + + PURPOSE + + +
C     copy an existing scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,MXSEN,SCLU,SACTIV,UNSAVE,NSEN,NUCIS,JSTSAV,
     1              MXTS,STLINE
      CHARACTER*8   USENNM(MXTS),TMPNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen message cluster
C     MXSEN  - maximum number of scenarios
C     SACTIV - active scenario number
C     UNSAVE - flag indicating some basin data may be unsaved
C     NSEN   - number of scenarios
C     NUCIS  - number of available uci files
C     JSTSAV - just saved scenario flag
C     MXTS   - max number of time series
C     USENNM - array of scenario names
C     TMPNAM - name of newly created scenario
C     STLINE - line of status window where scenario name is to be shown
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       TLEN,PREVFG,I,I1,SGRP,CNUM,CLEN(2),I8,I0,I4,I3
      INTEGER       FOUND,ILEN,J,ISTDSN,DSNID,IRET,IVAL(1)
      CHARACTER*1   BLNK,CTXT(16),TMPNM1(8)
      CHARACTER*4   WDID
      CHARACTER*8   CCON,CLOC
      CHARACTER*78  STBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL      ZIPC,ZSTCMA,CVARAR,QSETCT,QGETCT,ZSTADD
      EXTERNAL      QUPCAS,CARVAR,TSESPC,TSDSM,WID2UA,PRNTXT
      EXTERNAL      Q1INIT,QSETI,Q1EDIT,QGETI,WUA2ID,COPSCN
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I3 = 3
      I4 = 4
      I8 = 8
C
      IF (NSEN.LT.MXSEN) THEN
C       room to add another scenario, okay to continue
C       get new scenario name
        TLEN   = 16
        BLNK   = ' '
        CALL ZIPC (TLEN,BLNK,CTXT)
 62     CONTINUE
          PREVFG = 0
C         allow previous
          I = 4
          CALL ZSTCMA (I,I1)
          SGRP = 61
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         set character field for scenario copied from
          CNUM   = 2
          CLEN(1)= 8
          CLEN(2)= 8
          IF (SACTIV.LE.NUCIS) THEN
C           regular case
            CALL CVARAR (I8,USENNM(SACTIV),I8,CTXT)
          ELSE IF (SACTIV.GT.NUCIS) THEN
C           copying from temp scenario
            CALL CVARAR (I8,TMPNAM,I8,CTXT)
          END IF
          CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
          CALL Q1EDIT (IRET)
C         turn previous off
          I = 4
          CALL ZSTCMA (I,I0)
          IF (IRET.NE.2) THEN
C           user wants to continue
            CALL QGETCT (CNUM,CLEN,TLEN,CTXT)
C           get name of new scenario
            CALL QUPCAS (I8,CTXT(9))
            CALL CARVAR (I8,CTXT(9),I8,TMPNAM)
            FOUND = 0
            DO 63 I = 1,NUCIS
C             look for this scenario name in local array
              IF (USENNM(I).EQ.TMPNAM) THEN
C               problem, name already used
                FOUND = 1
              END IF
 63         CONTINUE
            IF (FOUND.EQ.0) THEN
C             okay name
              UNSAVE = 1
              JSTSAV = 0
C             show name of scenario in status window
              IF (STLINE.EQ.2) THEN
C               set up status for genscn
                STBUFF= '     Scenario: '
                ILEN  = 8
                CALL CVARAR (ILEN,TMPNAM,ILEN,TMPNM1)
                CALL CARVAR (ILEN,TMPNM1,ILEN,STBUFF(16:23))
              ELSE
C               set up status for hspexp
                STBUFF= '       calibration: '
                ILEN  = 8
                CALL CVARAR (ILEN,TMPNAM,ILEN,TMPNM1)
                CALL CARVAR (ILEN,TMPNM1,ILEN,STBUFF(21:28))
              END IF
              CALL ZSTADD (STLINE,STBUFF)
C             find first data set in copied scenario
              CCON = '        '
              CLOC = '        '
              CALL TSESPC (USENNM(SACTIV),CLOC,CCON)
C             look for data set meeting specs
              DSNID = 0
              I = 1
              CALL TSDSM (I)
              IF (I.GT.0) THEN
C               found a data set that needs to be replicated
C               convert this dsnid to a wdm id and dsn
                CALL WID2UA (I0,I,
     O                       J,ISTDSN,WDID)
C               do screen for where to put new scenario
                SGRP = 63
                CALL Q1INIT (MESSFL,SCLU,SGRP)
C               set character field for new scenario
                CNUM   = 2
                CLEN(1)= 8
                CLEN(2)= 4
                CALL CVARAR (I8,TMPNAM,I8,CTXT(1))
                CALL CVARAR (I4,WDID,I4,CTXT(9))
                TLEN   = 12
                CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C               set integer field for starting dsn
                IVAL(1) = ISTDSN
                CALL QSETI  (I1,IVAL)
                CALL Q1EDIT (IRET)
                CALL QGETCT (CNUM,CLEN,TLEN,CTXT)
                CALL CARVAR (I4,CTXT(9),I4,WDID)
                CALL QGETI  (I1,IVAL)
                ISTDSN = IVAL(1)
C               convert this wdm id and dsn to a dsnid
                CALL WUA2ID (J,ISTDSN,WDID,
     O                       DSNID)
              END IF
C
C             create new data sets, modify uci file accordingly
              CALL COPSCN (USENNM(SACTIV),TMPNAM,DSNID,I1)
C
              SACTIV = NUCIS + 1
              PREVFG = 0
            ELSE
C             invalid scenario name, enter another
              SGRP = 75
              CALL PRNTXT (MESSFL,SCLU,SGRP)
              PREVFG = 1
            END IF
          END IF
        IF (PREVFG.EQ.1) GO TO 62
      ELSE
C       no room for another scenario
        SGRP= 62
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSSAVE
     I                   (MESSFL,SCLU,UCIPTH,SACTIV,TMPNAM,MXTS,
     M                    JSTSAV,NUCIS,USENNM)
C
C     + + + PURPOSE + + +
C     save a scenario to file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,JSTSAV,SACTIV,
     1              NUCIS,MXTS
      CHARACTER*8   TMPNAM,USENNM(MXTS)
      CHARACTER*64  UCIPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     UCIPTH - path to uci files for this basin
C     JSTSAV - flag indicating if active scenario has been saved
C     TMPNAM - name of new scenario not yet saved
C     SACTIV - active scenario number
C     NUCIS  - number of uci files available
C     MXTS   - max number of time series in directory
C     USENNM - array of scenario names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,UCIFL,I64,SENLEN,EMFG,
     1              REMLEN,I4,RETCOD,I0,I,I1
      CHARACTER*1   FILNAM(64),CUCI(4)
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     PRNTXT, UCIWRT
      EXTERNAL     CVARAR, ZLNTXT, CHRCHR, QFOPFN, PMXCNW
      EXTERNAL     TSDSCL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   CUCI/'.','u','c','i'/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
      I64= 64
C
      IF (SACTIV.LE.NUCIS) THEN
C       a standard scenario to output
C       try opening uci file
        CALL CVARAR(I64,UCIPTH,I64,FILNAM)
C       build uci file name from path name and scenario name
        SENLEN = ZLNTXT(USENNM(SACTIV))
        REMLEN = I64 - ZLNTXT(UCIPTH)
        CALL CVARAR(SENLEN,USENNM(SACTIV),REMLEN,
     O              FILNAM(ZLNTXT(UCIPTH)+1))
        CALL CHRCHR(I4,CUCI,
     1              FILNAM(ZLNTXT(UCIPTH)+SENLEN+1))
        SGRP= 73
        CALL QFOPFN (MESSFL,SCLU,SGRP,FILNAM,I0,
     O               UCIFL,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put window name into screen
          SGRP = 72
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
C         write UCI file info
          EMFG = 1
          CALL UCIWRT (UCIFL,I1)
          JSTSAV = 1
C
C         check data sets, create/delete if needed
          CALL TSDSCL (MESSFL,I1,I1)
C
        ELSE
C         problem opening UCI file for output
          SGRP= 71
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
      ELSE IF (SACTIV.GT.NUCIS) THEN
C       new temporary scenario
C       try opening uci file
        CALL CVARAR(I64,UCIPTH,I64,FILNAM)
C       build uci file name from path name and scenario name
        SENLEN = ZLNTXT(TMPNAM)
        REMLEN = I64 - ZLNTXT(UCIPTH)
        CALL CVARAR(SENLEN,TMPNAM,REMLEN,
     O              FILNAM(ZLNTXT(UCIPTH)+1))
        CALL CHRCHR(I4,CUCI,FILNAM(ZLNTXT(UCIPTH)+SENLEN+1))
        SGRP= 73
        CALL QFOPFN (MESSFL,SCLU,SGRP,FILNAM,I0,
     O               UCIFL,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put window name into screen
          SGRP = 72
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
C         write UCI file info
          EMFG = 1
          CALL UCIWRT (UCIFL,I1)
C         save this scenario
          NUCIS = NUCIS + 1
          USENNM(NUCIS) = TMPNAM
          JSTSAV= 1
C
C         check data sets, create/delete if needed
          CALL TSDSCL (MESSFL,I1,I1)
C
        ELSE
C         problem opening UCI file for output
          SGRP= 71
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCOMPU
     I                   (MESSFL,WDMSFL,FILES,PTHNAM)
C
C     + + + PURPOSE + + +
C     compute option (run hspf)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,WDMSFL,FILES(15)
      CHARACTER*8   PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     WDMSFL - wdm data file unit number
C     FILES  - array of hspf file unit numbers
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       RETCOD,SGRP,SCLU,I1,LPTH
      CHARACTER*48  WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL      HSPF,HDMEST,ZSTWRT,ZLNTXT,ZWNSOP,ZWNSET
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize files to used
C     I= 13
C     CALL ZIPI (I,I0,FILES)
C
C     use wdm data file
C     FILES(14)= WDMSFL
C     use hspf msg file
C     FILES(15)= MESSFL
C
      I1 = 1
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'SImulate ('//PTHNAM(1)(1:LPTH)//'Si)'
      ELSE
C       no pathname
        WNDNAM= 'SImulate (Si)'
      END IF
      CALL ZWNSET (WNDNAM)
C
C     show status
      I1 = 1
      CALL ZSTWRT (I1)
C     proceed to run model
      CALL HSPF (FILES,
     O           RETCOD)
C     simulation complete
      SCLU = 201
      CALL ZWNSET (WNDNAM)
      IF (RETCOD .EQ. 0) THEN
        SGRP = 52
        CALL HDMEST (I1,MESSFL,SCLU,SGRP)
      ELSE IF (RETCOD .EQ. 1) THEN
C       runfg=0 in global block - must stop
        SGRP = 53
        CALL HDMEST (I1,MESSFL,SCLU,SGRP)
      ELSE IF (RETCOD .EQ. 2) THEN
C       errors in input file -  must stop
        SGRP = 54
        CALL HDMEST (I1,MESSFL,SCLU,SGRP)
      ELSE IF (RETCOD .EQ. 3) THEN
C       no run keyword found in input file
        SGRP = 55
        CALL HDMEST (I1,MESSFL,SCLU,SGRP)
      END IF
C
C     close all files
C     DO 10 I= 1, 13
C       IF (FILES(I) .GT. 0) THEN
C         this file must be closed
C         CLOSE (FILES(I))
C       END IF
C10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SSREAD
     I                   (MESSFL,SCLU,WDMSFL,UCIPTH,STLINE,
     I                    PTHNAM,ISEN,SENNAM,
     M                    SACTIV,JSTSAV,EMFG,FILES)
C
C     + + + PURPOSE + + +
C     read in a scenario/uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,SACTIV,JSTSAV,WDMSFL,
     $              FILES(15),STLINE,ISEN,EMFG
      CHARACTER*8   SENNAM,PTHNAM(1)
      CHARACTER*64  UCIPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen message cluster
C     UCIPTH - path to uci files
C     SENNAM - scenario name
C     SACTIV - active scenario number
C     JSTSAV - flag indicating if active scenario has been saved
C     WDMSFL - wdm file unit number
C     FILES  - array of file unit numbers
C     STLINE - line of status window where scenario name is to be shown
C     PTHNAM - path of options chosen to get here
C     ISEN   - integer scenario number
C     EMFG   - english/metric units flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,I,I0,I64,RETCOD,UCIFL,
     1              IDUM,ILEN,J,I4,SENLEN,REMLEN
      CHARACTER*1   FILNAM(64),CUCI(4)
      CHARACTER*78  STBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     CVARAR, QFCLOS
      EXTERNAL     QFOPFN, UCIREA, CARVAR, ZSTADD, ZWRSCR
      EXTERNAL     ZLNTXT, CHRCHR, HDMEST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   CUCI/'.','u','c','i'/
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I4  = 4
      I64 = 64
      IDUM= 0
C
C     okay to continue, try opening uci file
      CALL CVARAR(I64,UCIPTH,I64,FILNAM)
C     build uci file name from path name and scenario name
      SENLEN = ZLNTXT(SENNAM)
      REMLEN = I64 - ZLNTXT(UCIPTH)
      CALL CVARAR(SENLEN,SENNAM,REMLEN,
     O            FILNAM(ZLNTXT(UCIPTH)+1))
      CALL CHRCHR(I4,CUCI,FILNAM(ZLNTXT(UCIPTH)+SENLEN+1))
      SGRP= 12
      CALL QFOPFN (MESSFL,SCLU,SGRP,FILNAM,I0,
     O             UCIFL,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       get UCI file info
C       clear menu box before putting up 'reading uci' message
        STBUFF= ' '
        CALL ZSTADD (STLINE,STBUFF)
        J= 2
        DO 5 I= 2,11
          CALL ZWRSCR (STBUFF,I,J)
 5      CONTINUE
C
        CALL UCIREA (MESSFL,UCIFL,PTHNAM(1),WDMSFL,
     M               IDUM,EMFG,FILES,
     O               RETCOD)
C
        WRITE (99,*)'UCIREA done read,UCIFL',UCIFL
        WRITE (99,*)'  RETCOD ',RETCOD
        WRITE (99,*)'  filnam ',FILNAM
        IF (RETCOD.EQ.0) THEN
C         file opened and read successfully
          SACTIV= ISEN
C         show name of scenario in status window
          IF (STLINE.EQ.2) THEN
C           set up status for genscn
            STBUFF= '     Scenario: '
            ILEN  = 8
            CALL CARVAR (ILEN,SENNAM,ILEN,STBUFF(16:23))
          ELSE
C           set up status for hspexp
            STBUFF= '       calibration: '
            ILEN  = 8
            CALL CARVAR (ILEN,SENNAM,ILEN,STBUFF(21:28))
          END IF
          CALL ZSTADD (STLINE,STBUFF)
          JSTSAV = 1
        ELSE
C         problem reading ucifl
          I= 0
          CALL QFCLOS (UCIFL,I)
          SACTIV= 0
          UCIFL = 0
        END IF
      ELSE
C       problem opening UCI file
        STBUFF= ' '
        CALL ZSTADD (STLINE,STBUFF)
        SCLU = 201
        SGRP = 51
        CALL HDMEST (I0,MESSFL,SCLU,SGRP)
        SACTIV= 0
        UCIFL = 0
      END IF
C
      RETURN
      END

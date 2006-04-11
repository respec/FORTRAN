C
C
C
      SUBROUTINE   PRWMLV
     I                    (MESSFL,WDMSFL,DSNBMX,DSNCNT,DSNBUF)
C
C     + + + PURPOSE + + +
C     This routine is used to view WDM dataset attributes.  The user
C     is asked to specify a set of attributes to be output, and the
C     dataset numbers to be used.  Output may be in either List or Table
C     form and can be output to the CRT or a print file up to 132
C     characters wide, or a flat file up to 250 charcters wide.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,DSNBMX,DSNCNT,DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     DSNBMX - maximum size of DSN array
C     DSNCNT - count of attributes to table
C     DSNBUF - array of DSNCNT dataset numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,I0,I1,I78,SGRP,SCLU,INUM,CNUM,FUNIT,ERR,
     $            TORF,BEGEND,DELFG,SLINES,FLINES,BUFLEN,FULLFG,
     $            KNT,LKNT,ILEN,MAXATR,IRET,RESP,PNT,OLEN,LEFT,
     $            SAIND(40),SALNG(40),SATYP(40),SAWID(40),LORT,
     $            SAPNT(40),SGFD(40),DPLA(40),CVAL(3,3),IVAL(3)
      REAL        RVAL
      DOUBLE PRECISION DVAL
      CHARACTER*1 HEAD(250,5),SANAM(6,40),BLNK(1),TXT(120),
     $            DASH(1),TBUFF(118)
C
C     + + + EXTERNALS + + +
      EXTERNAL    TBAADD, TBAEXC, TBADEF, TBALST, TBAOUT, ZSTADQ, ZQUIET
      EXTERNAL    ZSTCMA, ZGTRET, QRESP, QRESCZ, ZIPC, ZIPI, PRNTXT
      EXTERNAL    CHRCHR, QFOPEN, QFCLOS, WMSGTT, LENSAT, CTRSTR
      EXTERNAL    PRNTXI, TBALIS
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I78   = 78
      BLNK(1)= ' '
      DASH(1)= '-'
      SCLU  = 23
      MAXATR= 40
      LORT  = 1
      TORF  = 1
      FUNIT = 0
      SLINES= 16
      FLINES= 50
      BUFLEN= 78
      FULLFG= 0
      I= 250
      CALL ZIPC (I,BLNK,HEAD(1,1))
C
      I= 5
      CALL ZIPI (MAXATR,I,SGFD)
      I= 0
      CALL ZIPI (MAXATR,I,DPLA)
C
C     start with no attributes being tabled
      KNT   = 0
      LKNT  = 0
C     always have space for data-set numbers
      PNT   = 6
C     start with no period of record
      BEGEND= 0
C     show initial state in Status window
      CALL TBALST (MESSFL,SCLU,BUFLEN,MAXATR,KNT,PNT,LORT,
     I             BEGEND,SAIND,SANAM,SALNG,SATYP)
C
 10   CONTINUE
C       do main attribute table menu
        SGRP= 11
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300,400,500,600,700,800), RESP
C
 100    CONTINUE
C         get output options and open file if needed
C         allow previous
          I= 4
          CALL ZSTCMA (I,I1)
 110      CONTINUE
            INUM= 3
            CNUM= 3
            CALL ZIPI (3*CNUM,I1,CVAL)
            CVAL(1,1)= LORT
            CVAL(2,1)= TORF
            IVAL(1)  = BUFLEN
            IVAL(2)  = SLINES
            IVAL(3)  = FLINES
            ILEN= 118
            CALL ZIPC (ILEN,BLNK,TBUFF)
            CALL CHRCHR (I78,HEAD,TBUFF(41))
C           get user specifications
            SGRP = 24
            CALL QRESCZ (MESSFL,SCLU,SGRP,INUM,I1,I1,CNUM,I1,I1,ILEN,
     M                   IVAL,RVAL,DVAL,CVAL,TBUFF)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              IF (LORT.NE.CVAL(1,1)) THEN
C               need to show change in Status window
                LKNT= KNT+ 1
              END IF
              LORT  = CVAL(1,1)
              TORF  = CVAL(2,1)
              IF (BUFLEN.NE.IVAL(1)) THEN
C               need to show change in Status window
                LKNT= KNT+ 1
              END IF
              BUFLEN= IVAL(1)
              IF (LORT.EQ.2 .AND. FULLFG.EQ.1) THEN
C               can't table full set of attributes, reset to no attributes
                SGRP= 29
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                CALL ZGTRET (IRET)
                KNT = 0
                PNT = 6+ 22*BEGEND
              END IF
              IF (TORF.NE.2 .AND. BUFLEN.GT.78 .AND. IRET.EQ.1) THEN
C               output to screen and characters/line is too long
                SGRP= 27
                CALL PRNTXT (MESSFL,SCLU,SGRP)
C               get user exit command value
                CALL ZGTRET (IRET)
C               reset characters/line to max on screen
                BUFLEN= 78
              END IF
              IF (IRET.EQ.1) THEN
C               continue
                SLINES= IVAL(2)
                FLINES= IVAL(3)
                IF (TORF.GT.1) THEN
C                 file output, open file and get unit number
                  IF (FUNIT.GT.0) THEN
C                   one file already open, close it before opening another
                    DELFG = 0
                    CALL QFCLOS (FUNIT,DELFG)
                  END IF
                  SGRP = 25
                  CALL QFOPEN (MESSFL,SCLU,SGRP,FUNIT,ERR)
C                 get user exit command value
                  CALL ZGTRET (IRET)
                  IF (IRET.EQ.1 .AND. ERR.NE.0) THEN
C                   user trying to continue, but could not open output file
                    SGRP= 26
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                  ELSE IF (IRET.EQ.1) THEN
C                   all ok, put header in place
                    CALL CHRCHR (I78,TBUFF(41),HEAD(1,1))
                  END IF
                END IF
              END IF
            ELSE
C             back to main Table menu
              IRET= 1
            END IF
          IF (IRET.EQ.2) GO TO 110
C         turn off previous
          I= 4
          CALL ZSTCMA (I,I0)
          GO TO 999
C
 200    CONTINUE
C         init to table defaults
          SGRP= 13
          CALL TBADEF (MESSFL,SCLU,SGRP,MAXATR,
     O                 KNT,PNT,BEGEND,SAIND,SANAM,
     O                 SALNG,SATYP,SGFD,DPLA)
C         don't use full set when using standard
          FULLFG= 0
          GO TO 999
C
 300    CONTINUE
C         include full set of attributes available
          IF (LORT.EQ.1) THEN
C           listing attributes, ok to use full set
            KNT   = 1
            SAIND(1)= 0
            FULLFG= 1
          ELSE
C           cant use full set when tabling attributes
            SGRP  = 30
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            FULLFG= 0
          END IF
          GO TO 999
C
 400    CONTINUE
C         include the beginning and ending dates
          IF (BEGEND.EQ.0) THEN
C           currently not doing dates, include them now
            IF (PNT + 22 .GT. BUFLEN .AND. LORT .EQ. 2) THEN
C             output width exceeded by selected attributes
              SGRP= 15
              CALL PRNTXT (MESSFL,SCLU,SGRP)
C             dont add dates
              BEGEND= 0
            ELSE
C             room to add dates
              BEGEND= 1
C             adjust characters used
              PNT= PNT+ 22
C             make sure Status gets updated
              LKNT= KNT+ 1
            END IF
            IF (PNT+20.GE.BUFLEN .AND.
     $          PNT.LE.BUFLEN .AND. LORT.EQ.2) THEN
C             approaching end of output buffer
              SGRP= 17
              LEFT= BUFLEN - PNT + 1
              CALL PRNTXI (MESSFL,SCLU,SGRP,LEFT)
            END IF
          END IF
          GO TO 999
C
 500    CONTINUE
C         add attributes to be tabled
          CALL TBAADD (MESSFL,SCLU,BUFLEN,MAXATR,BEGEND,LORT,
     M                 KNT,PNT,SAIND,SANAM,
     M                 SALNG,SATYP,SGFD,DPLA)
C         wont need to update Status, done in TBAADD
          LKNT= KNT
          GO TO 999
C
 600    CONTINUE
C         remove attributes to be tabled
          CALL TBAEXC (MESSFL,SCLU,MAXATR,BUFLEN,LORT,
     M                 KNT,PNT,BEGEND,SAIND,SANAM,
     M                 SALNG,SATYP,SGFD,DPLA)
C         wont need to update Status, done in TBAEXC
          LKNT= KNT
          GO TO 999
C
 700    CONTINUE
C         execute specified viewing of attributes
          IF (LORT.EQ.1) THEN
C           list attributes for datasets
            IF (KNT.GT.0 .OR. BEGEND.EQ.1) THEN
C             something to list
              CALL TBALIS (MESSFL,SCLU,WDMSFL,DSNCNT,DSNBUF,
     I                     FULLFG,KNT,MAXATR,BEGEND,SAIND,
     I                     TORF,FUNIT,HEAD,SLINES,FLINES)
            END IF
          ELSE
C           table attributes for datasets, initialize headers
            I= 250
            CALL ZIPC (I,BLNK,HEAD(1,2))
            CALL ZIPC (I,BLNK,HEAD(1,3))
            CALL ZIPC (I,BLNK,HEAD(1,4))
C           start filling header buffers
            SGRP= 18
            OLEN= 120
            CALL WMSGTT (MESSFL,SCLU,SGRP,I1,
     M                   OLEN,
     O                   TXT,J)
            IF (BEGEND .EQ. 1) THEN
C             header includes DSN and data begin and end date
              K  = 3
              PNT= 29
              I  = 29
            ELSE
C             header includes DSN
              K  = 2
              PNT= 7
              I  = 7
            END IF
            DO 715 J= 1,K
              CALL CHRCHR (I,TXT(31*(J-1)+1),HEAD(1,J+1))
 715        CONTINUE
C
            IF (KNT .GT. 0) THEN
C             add header for default attributes
              DO 720 K = 1, KNT
                CALL LENSAT (SATYP(K),SAIND(K),SALNG(K),
     O                       SAWID(K))
      writ  e (99,*) 'TYP,IND,LNG,WID,PNT',
     1                SATYP(K),SAIND(K),SALNG(K),SAWID(K),PNT
                SAPNT(K) = PNT
                J= 6
                CALL CHRCHR (J,SANAM(1,K),HEAD(PNT,2))
                IF (SATYP(K).LT.3) CALL CTRSTR(SAWID(K),HEAD(PNT,2))
                CALL ZIPC (SAWID(K),DASH,HEAD(PNT,3))
                PNT = PNT + SAWID(K) + 2
 720          CONTINUE
            END IF
            CALL TBAOUT (WDMSFL,FUNIT,TORF,BUFLEN,BEGEND,MAXATR,
     I                   SLINES,FLINES,HEAD,KNT,DSNCNT,DSNBUF,
     I                   SAIND,SATYP,SALNG,SAWID,SAPNT,SGFD,DPLA)
          END IF
          GO TO 999
C
 800    CONTINUE
C         all done, clear out Status
          CALL ZIPC (I78,BLNK,HEAD)
          DO 810 I= 1,4
            CALL ZSTADQ (I,HEAD,I0)
 810      CONTINUE
C         remove Status window
          CALL ZQUIET
C         make sure status window is not update w/code below
          LKNT= KNT
          GO TO 999
C
 999    CONTINUE
C
C       may need to update Status window
        IF (KNT.NE.LKNT) THEN
C         show what is to be tabled
          CALL TBALST (MESSFL,SCLU,BUFLEN,MAXATR,KNT,PNT,LORT,
     I                 BEGEND,SAIND,SANAM,SALNG,SATYP)
          LKNT= KNT
        END IF
C
      IF (RESP.NE.8) GO TO 10
C
      IF (TORF.GT.1) THEN
C       close output file
        DELFG = 0
        CALL QFCLOS (FUNIT,DELFG)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBADEF
     I                   (MESSFL,SCLU,SGRP,MAXATR,
     O                    KNT,PNT,BEGEND,SAIND,SANAM,
     O                    SALNG,SATYP,SGFD,DPLA)
C
C     + + + PURPOSE + + +
C     Set default attributes to be tabled.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,SGRP,MAXATR,KNT,PNT,BEGEND,SAIND(MAXATR),
     $            SALNG(MAXATR),SATYP(MAXATR),SGFD(MAXATR),DPLA(MAXATR)
      CHARACTER*1 SANAM(6,MAXATR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     SGRP   - group in cluster to read default attributes from
C     MAXATR - maximum number of attributes permitted
C     KNT    - number of attributes selected
C     PNT    - current number of characters per line
C     BEGEND - indicator flag for data begin and end dates
C              1 - output dates
C              2 - do not output dates
C     SAIND  - search attribute index numbers
C     SANAM  - search attribute name
C     SALNG  - search attribute length, in words
C     SATYP  - search attribute types
C              1 - integer
C              2 - real
C              3 - character
C     SGFD   - number of significant digits for real search attribute
C     DPLA   - number of decimal places for real search attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I6,INIT,OLEN,CONT
      CHARACTER*1 TXT(120)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRINT, CHRCHR, WMSGTT
C
C     + + + END SPECIFICATIONS + + +
C
      I6  = 6
      INIT= 1
      OLEN= 120
      CALL WMSGTT (MESSFL,SCLU,SGRP,INIT,
     M             OLEN,
     O             TXT,CONT)
      KNT = CHRINT (I6,TXT)
      INIT= 0
      DO 110 I = 1, KNT
        OLEN= 120
        CALL WMSGTT (MESSFL,SCLU,SGRP,INIT,
     M               OLEN,
     O               TXT,CONT)
        CALL CHRCHR (I6,TXT,SANAM(1,I))
        J= 5
        SAIND(I)= CHRINT(J,TXT(7))
        SALNG(I)= CHRINT(J,TXT(12))
        SATYP(I)= CHRINT(J,TXT(17))
        SGFD(I) = CHRINT(J,TXT(22))
        DPLA(I) = CHRINT(J,TXT(27))
 110  CONTINUE
C     include the beginning and ending dates
      BEGEND= 1
C     set current character point
      PNT   = 81
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBAADD
     I                    (MESSFL,SCLU,BUFLEN,MAXATR,BEGEND,LORT,
     M                     KNT,PNT,SAIND,SANAM,
     M                     SALNG,SATYP,SGFD,DPLA)
C
C     + + + PURPOSE + + +
C     Specify attributes to be added to the table.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,BUFLEN,MAXATR,BEGEND,LORT,KNT,PNT
      INTEGER     SAIND(MAXATR),SALNG(MAXATR),SATYP(MAXATR),
     1            SGFD(MAXATR),DPLA(MAXATR)
      CHARACTER*1 SANAM(6,MAXATR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     BUFLEN - width of output buffer
C     MAXATR - maxiumu number of attributes permitted
C     BEGEND - indicator flag for data begin and end dates
C              0 - do not output dates
C              1 - output dates
C     LORT   - List or Table flag,
C              1 - List attributes
C              2 - Table attributes
C     KNT    - number of attributes selected
C     PNT    - current number of characters per line
C     SAIND  - search attribute index numbers
C     SANAM  - search attribute name
C     SALNG  - search attribute length, in words
C     SATYP  - search attribute types
C              1 - integer
C              2 - real
C              3 - character
C     SGFD   - number of significant digits for real search attribute
C     DPLA   - number of decimal places for real search attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,SGRP,DONFG,INUM,IVAL(2),CVAL(3),
     1            LEFT,LSAWID,IRET
      REAL        RVAL
      CHARACTER*1 TBUFF(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSAT, TBALST
      EXTERNAL    WDSAGX, QRESPM, PRNTXI, PRNTXT, ZSTCMA, ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
C
C     allow previous
      I= 4
      CALL ZSTCMA (I,I1)
C
 100  CONTINUE
C       get search attribute and info
        KNT = KNT + 1
        CALL WDSAGX (MESSFL,
     O               SAIND(KNT),SANAM(1,KNT),
     O               SATYP(KNT),SALNG(KNT),DONFG)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (DONFG.EQ.0 .AND. IRET.EQ.1) THEN
C         user specified another attribute to Add to table
          IF (SAIND(KNT).EQ.39) THEN
C           ALL specified, can't table ALL attributes
            SGRP= 12
            CALL PRNTXT (MESSFL,SCLU,SGRP)
C           dont add this selection
            KNT = KNT - 1
          ELSE
C           determine output width and buffer position
            CALL LENSAT (SATYP(KNT),SAIND(KNT),SALNG(KNT),
     O                   LSAWID)
            IF (PNT+ LSAWID- 1 .GT. BUFLEN .AND. LORT .EQ. 2) THEN
C             output width exceeded by selected attributes
              SGRP= 15
              CALL PRNTXT (MESSFL,SCLU,SGRP)
C             dont add this selection
              KNT = KNT - 1
              IRET= 2
            END IF
            IF (IRET.EQ.1) THEN
C             attribute added to list
              IF (SATYP(KNT).EQ.2) THEN
C               real attribute, get significant figures and decimal places
                INUM = 2
                IVAL(1)= SGFD(KNT)
                IVAL(2)= DPLA(KNT)
                SGRP = 16
                CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,I1,
     M                       IVAL,RVAL,CVAL,TBUFF)
                CALL ZGTRET (IRET)
                IF (IRET.EQ.1) THEN
                  SGFD(KNT)= IVAL(1)
                  DPLA(KNT)= IVAL(2)
                ELSE
C                 back to specify attribute, dont count this one
                  KNT= KNT- 1
                END IF
              ELSE
C               continue processing attribute
                IRET= 1
              END IF
              IF (IRET.EQ.1) THEN
C               user wants to continue
                PNT = PNT + LSAWID + 2
C               show current attributes being tabled in Status
                CALL TBALST (MESSFL,SCLU,BUFLEN,MAXATR,KNT,PNT,LORT,
     I                       BEGEND,SAIND,SANAM,SALNG,SATYP)
                IF (PNT+20.GE.BUFLEN .AND. LORT.EQ.2) THEN
C                 approaching end of output buffer
                  SGRP= 17
                  LEFT= BUFLEN - PNT + 3
                  CALL PRNTXI (MESSFL,SCLU,SGRP,LEFT)
                END IF
              END IF
            END IF
          END IF
        ELSE
C         finished getting attributes
          DONFG= 1
          KNT  = KNT - 1
        END IF
      IF (DONFG.EQ.0 .AND. KNT.LT.MAXATR) GO TO 100
C
C     turn off previous
      I= 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBAEXC
     I                    (MESSFL,SCLU,MAXATR,BUFLEN,LORT,
     M                     KNT,PNT,BEGEND,SAIND,SANAM,
     M                     SALNG,SATYP,SGFD,DPLA)
C
C     + + + PURPOSE + + +
C     Specify attributes to exclude from the table.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,MAXATR,BUFLEN,LORT,KNT,PNT,BEGEND
      INTEGER     SAIND(MAXATR),SALNG(MAXATR),SATYP(MAXATR),
     1            SGFD(MAXATR),DPLA(MAXATR)
      CHARACTER*1 SANAM(6,MAXATR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     MAXATR - maxiumu number of attributes permitted
C     BUFLEN - width of output buffer
C     LORT   - List or Table flag,
C              1 - List attributes
C              2 - Table attributes
C     KNT    - number of attributes selected
C     PNT    - current number of characters per line
C     BEGEND - indicator flag for data begin and end dates
C              0 - do not output dates
C              1 - output dates
C     SAIND  - search attribute index numbers
C     SANAM  - search attribute name
C     SALNG  - search attribute length, in words
C     SATYP  - search attribute types
C              1 - integer
C              2 - real
C              3 - character
C     SGFD   - number of significant digits for real search attribute
C     DPLA   - number of decimal places for real search attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,I6,SGRP,DONFG,IRET,LKNT,
     1            LSAWID,TSAIND,TSALNG,TSATYP
      CHARACTER*1 BLNK(1),TSANAM(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, ZIPC, ZIPI, LENSAT, CHRCHR, TBALST
      EXTERNAL    WDSAGX, PRNTXT, ZSTCMA, ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I6  = 6
      BLNK(1)= ' '
C
C     remove attributes from table
      IF (KNT.GT.0 .OR. BEGEND.EQ.1) THEN
C       attributes (or dates) to remove, allow previous
        I= 4
        CALL ZSTCMA (I,I1)
C
 100    CONTINUE
C         get attribute user wants to remove
          CALL WDSAGX (MESSFL,
     O                 TSAIND,TSANAM,TSATYP,TSALNG,DONFG)
C         get user exit command value
          CALL ZGTRET (IRET)
          IF (IRET.EQ.1 .AND. DONFG.EQ.0) THEN
C           user wants to continue
            IF (TSAIND.EQ.39) THEN
C             ALL specified, clear current attributes to table
              I= 5
              CALL ZIPI (MAXATR,I,SGFD)
              I= 0
              CALL ZIPI (MAXATR,I,DPLA)
              KNT   = 0
              PNT   = 6
              BEGEND= 0
C             exit exclude loop as nothing left to exclude
              DONFG= 1
            ELSE IF (KNT.GT.0) THEN
C             standard attribute, see if it is being tabled
              LKNT= 0
 200          CONTINUE
                LKNT= LKNT+ 1
                I= STRFND(I6,SANAM(1,LKNT),I6,TSANAM)
                IF (I.GT.0) THEN
C                 names match, remove this one
                  CALL LENSAT (SATYP(LKNT),SAIND(LKNT),SALNG(LKNT),
     O                         LSAWID)
C                 adjust character point
                  PNT= PNT- LSAWID- 2
                  IF (LKNT.LT.KNT) THEN
C                   need to reposition attributes
                    DO 300 I= LKNT,KNT-1
                      CALL CHRCHR (I6,SANAM(1,I+1),SANAM(1,I))
                      SAIND(I)= SAIND(I+1)
                      SATYP(I)= SATYP(I+1)
                      SALNG(I)= SALNG(I+1)
                      SGFD(I) = SGFD(I+1)
                      DPLA(I) = DPLA(I+1)
 300                CONTINUE
                  END IF
C                 now remove last one
                  CALL ZIPC (I6,BLNK,SANAM(1,KNT))
                  KNT = KNT- 1
C                 now exit loop
                  LKNT= KNT
                END IF
              IF (LKNT.LT.KNT) GO TO 200
            END IF
C           show current attributes being tabled in Status
            CALL TBALST (MESSFL,SCLU,BUFLEN,MAXATR,KNT,PNT,LORT,
     I                   BEGEND,SAIND,SANAM,SALNG,SATYP)
          ELSE
C           back to main Contents menu
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 100
C       turn off previous
        I= 4
        CALL ZSTCMA (I,I0)
      ELSE
C       no attributes selected to table to be removed
        SGRP= 20
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBALST
     I                    (MESSFL,SCLU,BUFLEN,MAXATR,KNT,PNT,LORT,
     I                     BEGEND,SAIND,SANAM,SALNG,SATYP)
C
C     + + + PURPOSE + + +
C     Display list of attributes to be tabled in Status window.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,BUFLEN,MAXATR,KNT,PNT,LORT,BEGEND
      INTEGER     SAIND(MAXATR),SALNG(MAXATR),SATYP(MAXATR)
      CHARACTER*1 SANAM(6,MAXATR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     BUFLEN - width of output buffer
C     MAXATR - maxiumu number of attributes permitted
C     KNT    - number of attributes selected
C     PNT    - current number of characters per line
C     LORT   - List or Table flag,
C              1 - List attributes
C              2 - Table attributes
C     BEGEND - indicator flag for data begin and end dates
C              1 - output dates
C              2 - do not output dates
C     SAIND  - search attribute index numbers
C     SANAM  - search attribute name
C     SALNG  - search attribute length, in words
C     SATYP  - search attribute types
C              1 - integer
C              2 - real
C              3 - character
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I0,I1,I2,I6,I78,SGRP,LEFT,ILIN,IPOS,LSAWID
      CHARACTER*1  BLNK(1)
      CHARACTER*78 STBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (STBUF1,STBUFF)
      CHARACTER*1  STBUF1(78)
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSAT, CHRCHR, ZSTADQ, ZIPC
      EXTERNAL     GETTXT, INTCHR, CHRDEL
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
      I6  = 6
      I78 = 78
      BLNK(1) = ' '
C
C     list attributes to table
      IF (KNT.GT.0 .OR. BEGEND.EQ.1) THEN
C       attributes (or dates) to list, get header text for Status window
        I   = 78
        SGRP= 21
        CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O               STBUF1)
        IF (LORT.EQ.2) THEN
C         tabling attributes, show number of available characters left
          LEFT= BUFLEN - PNT + 3
C         put number of available characters in text
          I= 3
          CALL INTCHR (LEFT,I,I1,
     O                 J,STBUF1(22))
          IF (J.LT.3) THEN
C           remove extra blanks
            IPOS= 22+ J
            DO 50 I= 1,3-J
              CALL CHRDEL (I78,IPOS,
     M                     STBUF1)
 50         CONTINUE
          END IF
        ELSE
C         listing attributes, omit available characters text
          I= 29
          CALL ZIPC (I,BLNK,STBUF1(19))
          STBUF1(19)= ':'
        END IF
C       put header in status buffer, dont display
        CALL ZSTADQ (I1,STBUFF,I0)
C
        IF (BEGEND.EQ.1) THEN
C         period of record being tabled, get text for buffer
          SGRP= 23
          I   = 78
          CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O                 STBUF1)
          IF (LORT.EQ.1) THEN
C           listing attributes, don't need size info
            IPOS= 6
            STBUF1(IPOS)= ','
            I= 4
            CALL ZIPC (I,BLNK,STBUF1(7))
          ELSE
C           tabling attributes, keep size info
            IPOS= 10
          END IF
          IF (KNT.EQ.0) THEN
C           nothing else in table, remove comma
            STBUF1(IPOS)= ' '
          END IF
C         account for space used by 'Period' text
          IPOS= IPOS+ 2
        ELSE
C         init status buffer
          STBUFF= ' '
          IPOS= 1
        END IF
        ILIN= 2
        IF (KNT.GT.0) THEN
C         put attributes into status buffer
          IF (SAIND(1).GT.0) THEN
C           list attributes by name
            DO 100 I= 1,KNT
C             put each attribute being tabled into status buffer
              IF (IPOS+11.GT.78) THEN
C               no room for this attribute, output this line to status buffer
                IF (I.EQ.KNT) THEN
C                 last attribute, put in status buffer and display it
                  J= 1
                ELSE
C                 more lines to come, put in status buffer, but dont display it
                  J= 0
                END IF
                CALL ZSTADQ (ILIN,STBUFF,J)
                ILIN= ILIN+ 1
                IPOS= 1
C               reinit status buffer
                STBUFF= ' '
              END IF
C             start with name
              CALL CHRCHR (I6,SANAM(1,I),STBUF1(IPOS))
              IF (LORT.EQ.1) THEN
C               listing attributes, no need for showing width
                J= -1
              ELSE
C               tabling attributes, show width
                CALL LENSAT (SATYP(I),SAIND(I),SALNG(I),
     O                       LSAWID)
                CALL INTCHR (LSAWID,I2,I0,
     O                       J,STBUF1(IPOS+7))
              END IF
              IF (I.LT.KNT) THEN
C               more to come, add comma
                STBUF1(IPOS+7+J)= ','
              END IF
              IPOS= IPOS+ 9+ J
 100        CONTINUE
          ELSE
C           using full set of attributes
            SGRP= 28
            I   = 78- IPOS
            CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O                   STBUF1(IPOS))
C           make sure line gets into status buffer
            IPOS= IPOS+ 1
          END IF
        END IF
C
        IF (IPOS.GT.1) THEN
C         last line not yet put into status buffer
          CALL ZSTADQ (ILIN,STBUFF,I1)
        END IF
      ELSE
C       no attributes to list, clear any old text in lines 2 through 4
        STBUFF= ' '
        DO 200 I= 2,4
          CALL ZSTADQ (I,STBUFF,I0)
 200    CONTINUE
C       get no attributes to table message
        I= 78
        SGRP= 22
        CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O               STBUF1)
        IF (LORT.EQ.2) THEN
C         tabling attributes, show number of available characters left
          LEFT= BUFLEN - PNT + 3
C         put number of available characters in text
          I= 3
          CALL INTCHR (LEFT,I,I1,
     O                 J,STBUF1(40))
          IF (J.LT.3) THEN
C           remove extra blanks
            IPOS= 40+ J
            DO 250 I= 1,3-J
              CALL CHRDEL (I78,IPOS,
     M                     STBUF1)
 250        CONTINUE
          END IF
        ELSE
C         listing attributes, omit available characters text
          I= 29
          CALL ZIPC (I,BLNK,STBUF1(37))
          STBUF1(37)= '.'
        END IF
C       put no attributes message in Status window
        CALL ZSTADQ (I1,STBUFF,I1)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LENSAT
     I                    (TYPE,INDEX,LENGTH,
     O                     WIDTH)
C
C     + + + PURPOSE + + +
C     This routine determines the output width to allow for a search
C     attribute.  A width of zero is returned for an invalid search
C     type.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TYPE,INDEX,LENGTH,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TYPE   - search attribute type
C              1 - integer
C              2 - real
C              3 - character
C     INDEX  - search attribute index number
C     LENGTH - word length or search attribute
C     WIDTH  - maximum number or spaces required to print value
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TYPE.EQ.1)THEN
C       integer attribute
        IF ((INDEX.GE.5 .AND. INDEX.LE.42) .OR.
     *       INDEX.GE.56) THEN
C         output width is <= 5
          WIDTH = 6
        ELSE IF (INDEX.EQ.50 .OR. INDEX.EQ.52 .OR.
     *           INDEX.EQ.53) THEN
C         attribute is a date
          WIDTH = 19
        ELSE
C         default attribute width
          WIDTH = 8
        END IF
      ELSE IF (TYPE.EQ.2) THEN
C       real attribute, default width
        WIDTH = 10
      ELSE IF (TYPE.EQ.3) THEN
C       character attribute, use actual width, minimum = 6
        WIDTH = LENGTH
        IF (WIDTH.LT.6) THEN
C         minimum width
          WIDTH = 6
        END IF
      ELSE
C       invalid attribute type
        WIDTH = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBAOUT
     I                    (WDMSFL,FUNIT,TORF,BUFLEN,BEGEND,MAXATR,
     I                     SLINES,FLINES,HEAD,KNT,DSNCNT,DSNBUF,
     I                     SAIND,SATYP,SALNG,SAWID,SAPNT,
     I                     SGFD,DPLA)
C
C     + + + PURPOSE + + +
C     Output in table format a list of given attributes
C     for the datasets found in the data-set number buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,FUNIT,TORF,BUFLEN,BEGEND,MAXATR,
     $            SLINES,FLINES,KNT,DSNCNT
      INTEGER     SAIND(MAXATR),SATYP(MAXATR),SALNG(MAXATR),
     $            SAWID(MAXATR),SAPNT(MAXATR),SGFD(MAXATR),DPLA(MAXATR),
     $            DSNBUF(DSNCNT)
      CHARACTER*1 HEAD(250,5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     FUNIT  - Fortran unit number of output table
C     TORF   - indicator flag for output destination
C              1 - terminal
C              2 - flat file
C              3 - both terminal and file
C     BUFLEN - width of output table
C     BEGEND - indicator flag for data begin and end dates
C              1 - output dates
C              2 - do not output dates
C     MAXATR - maxiumum number of attributes permitted
C     SLINES - number of lines per screen for output to terminal
C     FLINES - number of lines per page for output to file
C     HEAD   - output buffer containing the title and table headings
C     KNT    - number of attributes to be output to the table
C     DSNCNT - number of datasets in DSN buffer
C     DSNBUF - array of datasets in buffer
C     SAIND  - search attribute index numbers
C     SATYP  - search attribute types
C              1 - integer
C              2 - real
C              3 - character
C     SALNG  - search attribute length, in words
C     SAWID  - width allowed for search attribute in output table
C     SAPNT  - starting position pointer in output buffer for table
C     SGFD   - number of significant digits for real search attribute
C     DPLA   - number of decimal places for real search attributes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I0,I1,K,SAVALI(1),STRT(6),FNSH(6),
     1             RETC,SKOUNT,FKOUNT,JR,OL,GPFLG,DREC,IRET,ISCR
      REAL         SAVALR(1)
      CHARACTER*1  BLNK(1), SAVALC(80)
      CHARACTER*14 WNDNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL    PRTSTR, ZIPC, INTCHR, WTFNDT, DATCHR, WDBSGI, WDBSGR
      EXTERNAL    DECCHX, WDBSGC, CHRCHR, ZBLDWR, ZSTCMA, ZGTRET, ZWNSET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK,JR,GPFLG,I0,I1/' ',0,1,0,1/
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT( '1', // )
C
C     + + + END SPECIFICATIONS + + +
C
      SKOUNT= 0
      FKOUNT= 0
C
C     allow previous and interrupt commands
      I= 4
      CALL ZSTCMA (I,I1)
      I= 16
      CALL ZSTCMA (I,I1)
C
C     write out title and table headings
      IF (TORF.GT.1) THEN
C       to file, skip to top of page
        WRITE(FUNIT,2001)
        FKOUNT = FKOUNT + 3
        CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,1))
        CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,2))
        IF (BEGEND .EQ. 1) THEN
C         add BEGIN and END labels for period of record
          CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,4))
        END IF
        CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,3))
        FKOUNT = FKOUNT + 2 + BEGEND
      END IF
      IF (TORF.EQ.1 .OR. TORF.EQ.3) THEN
C       to terminal
        WNDNAM= 'Execute (DATE)'
        CALL ZWNSET (WNDNAM)
        CALL ZBLDWR (BUFLEN,HEAD(1,2),I1,-I1,I)
        IF (BEGEND .EQ. 1) THEN
C         add BEGIN and END labels for period of record
          CALL ZBLDWR (BUFLEN,HEAD(1,4),I0,-I1,I)
        END IF
        CALL ZBLDWR (BUFLEN,HEAD(1,3),I0,-I1,I)
        SKOUNT = SKOUNT + 2 + BEGEND
      END IF
C
      ISCR= 1
      IRET= 1
      I   = 0
 100  CONTINUE
C       put dataset number in output buffer
        I = I + 1
        CALL ZIPC (BUFLEN,BLNK,HEAD(1,5))
        J= 5
        CALL INTCHR (DSNBUF(I),J,JR,OL,HEAD(1,5))
        IF (BEGEND.EQ.1) THEN
C         try to get begin and end dates
          CALL WTFNDT (WDMSFL,DSNBUF(I),GPFLG,
     O                 DREC,STRT,FNSH,RETC )
          IF (RETC .EQ. 0) THEN
C           add dates to output buffer
            CALL DATCHR (STRT(1),STRT(2),STRT(3),OL,HEAD(8,5))
            CALL DATCHR (FNSH(1),FNSH(2),FNSH(3),OL,HEAD(19,5))
          END IF
        END IF
C
        IF (KNT.GT.0) THEN
C         put attributes into output buffer
          DO 150 K = 1, KNT
C           get search attribute values
            IF (SATYP(K).EQ.1) THEN
C             integer attribute
              CALL WDBSGI (WDMSFL,DSNBUF(I),SAIND(K),SALNG(K),
     O                     SAVALI,RETC)
              IF (RETC.EQ.0)
     I          CALL INTCHR (SAVALI(1),SAWID(K),JR,
     O                       OL,HEAD(SAPNT(K),5))
            ELSE IF (SATYP(K).EQ.2) THEN
C             real attribute
              CALL WDBSGR (WDMSFL,DSNBUF(I),SAIND(K),SALNG(K),
     O                     SAVALR,RETC )
              IF (RETC.EQ.0)
     I          CALL DECCHX (SAVALR(1),SAWID(K),SGFD(K),DPLA(K),
     O                       HEAD(SAPNT(K),5) )
            ELSE IF (SATYP(K).EQ.3) THEN
C             character attribute
              CALL WDBSGC (WDMSFL,DSNBUF(I),SAIND(K),SALNG(K),
     O                     SAVALC,RETC )
              IF (RETC.EQ.0)
     I          CALL CHRCHR (SALNG(K),SAVALC,HEAD(SAPNT(K),5))
            END IF
 150      CONTINUE
        END IF
        IF (TORF.GT.1) THEN
C         to file
          CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,5))
          FKOUNT = FKOUNT + 1
        END IF
        IF (TORF.EQ.1 .OR. TORF.EQ.3) THEN
C         to terminal
          CALL ZBLDWR (BUFLEN,HEAD(1,5),I0,-I1,K)
          SKOUNT = SKOUNT + 1
        END IF
        IF (SKOUNT.GE.SLINES .OR. FKOUNT.GE.FLINES .OR.
     1                    (I.EQ.DSNCNT .AND. TORF.NE.2)) THEN
C         start new screen/page OR at very end and need to hold screen
          IF ((TORF.EQ.1 .OR. TORF.EQ.3) .AND. FKOUNT.LT.FLINES) THEN
C           terminal output and we're not here because new page to file
C           hold screen for user
            CALL ZBLDWR (I0,BLNK,I0,I0,K)
C           get user exit command value
            CALL ZGTRET (IRET)
            SKOUNT = 0
          END IF
          IF (I.LT.DSNCNT .AND. TORF.GT.1 .AND. FKOUNT.GT.FLINES) THEN
C           more to come and we're here because new page on file needed
            WRITE(FUNIT,2001)
C           reset line number in file
            FKOUNT = 3
          END IF
          IF ((IRET.EQ.1 .AND. I.LT.DSNCNT) .OR.
     1        (IRET.EQ.2 .AND. ISCR.GT.1)) THEN
C           user wants to continue and theres more to do
C           OR go back and theres room to back up
C           write out heading for next screen
            IF (TORF.GT.1 .AND. FKOUNT.EQ.3) THEN
C             more to come, start of new page, write header to file
              CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,2))
              IF (BEGEND .EQ. 1) THEN
C               include dates header
                CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,4))
              END IF
              CALL PRTSTR (FUNIT,BUFLEN,HEAD(1,3))
              FKOUNT = FKOUNT + 2 + BEGEND
            END IF
            IF ((TORF.EQ.1 .OR. TORF.EQ.3) .AND. SKOUNT.EQ.0) THEN
C             to terminal, start of new screen
              CALL ZBLDWR (BUFLEN,HEAD(1,2),I1,-I1,K)
              IF (BEGEND .EQ. 1) THEN
C               extra line for dates
                CALL ZBLDWR (BUFLEN,HEAD(1,4),I0,-I1,K)
              END IF
              CALL ZBLDWR (BUFLEN,HEAD(1,3),I0,-I1,K)
              SKOUNT = SKOUNT + 2 + BEGEND
            END IF
            IF (IRET.EQ.1) THEN
C             save first dataset index for next screen
              ISCR= I+ 1
            ELSE IF (IRET.EQ.2) THEN
C             user wants to back up one screen
              K= I- ISCR+ 1
              I= ISCR- K- 1
              IF (I.LT.0) THEN
C               only back up to first dataset
                I= 0
              END IF
              ISCR= I+ 1
              IRET= 1
            END IF
          END IF
        END IF
      IF (I.LT.DSNCNT .AND. IRET.EQ.1) GO TO 100
C
C     turn off previous and interrupt commands
      I= 4
      CALL ZSTCMA (I,I0)
      I= 16
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBALIS
     I                   (MESSFL,SCLU,WDMSFL,DSNCNT,DSNBUF,
     I                    FULLFG,KNT,MAXATR,BEGEND,SAIND,
     I                    TORF,FUNIT,HEADR,SLINES,FLINES)
C
C     + + + PURPOSE + + +
C     Output attributes from selected data sets in single
C     column list format to the screen, a file, or both.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,WDMSFL,DSNCNT,DSNBUF(DSNCNT),FULLFG,KNT,
     $            MAXATR,BEGEND,SAIND(MAXATR),TORF,FUNIT,SLINES,FLINES
      CHARACTER*1 HEADR(78)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number for WDM file
C     DSNCNT - number of datasets in DSN buffer
C     DSNBUF - array of datasets in buffer
C     FULLFG - indicator to list full set of available attributes
C              0 - list only selected attributes
C              1 - list full set of available attributes
C     HEADR  - character string containing header for file output
C     KNT    - number of attributes to be listed
C     MAXATR - max number of attributes able to be listed
C     BEGEND - indicator flag for data begin and end dates
C              1 - output dates
C              2 - do not output dates
C     SAIND  - search attribute index numbers
C     TORF   - indicator flag for output destination
C              1 - terminal
C              2 - flat file
C              3 - both terminal and file
C     FUNIT  - Fortran unit number of output table
C     SLINES - number of lines per screen for output to terminal
C     FLINES - number of lines per page for output to file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,I50,SGRP,ILEN,SDAT(6),EDAT(6),DREC,INIT,
     $            RETCOD,IRET,SCFG,ISC,SCIND(20),IDS,SKOUNT,FKOUNT,
     $            LSACNT,IIND,IAP,SAMAX,PSA,PSIND,DSAIND
      CHARACTER*1 TBUFF(78),BLNK(1)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDGIVL, LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDGIVL, LENSTR, ZSTCMA, WDDSCK, WDSALV, ZGTRET, ZIPC
      EXTERNAL    PMXTXI, PMXTFI, ZBLDWR, PRTSTR, GETTXT, WTFNDT, DATLST
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A1,/,78A1,/)
 2001 FORMAT (A1,//)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I50= 50
      IAP= 10
      BLNK(1)= ' '
      FKOUNT = 0
C
C     allow previous and interrupt commands
      I= 4
      CALL ZSTCMA (I,I1)
      I= 16
      CALL ZSTCMA (I,I1)
C
      IF (TORF.EQ.1 .OR. TORF.EQ.3) THEN
C       output going to screen
        SCFG= 1
      ELSE
        SCFG= 0
      END IF
      INIT= 1
C
      IDS= 0
 50   CONTINUE
C       loop through selected data sets
        IDS= IDS+ 1
C       get directory record
        CALL WDDSCK (WDMSFL,DSNBUF(IDS),DREC,RETCOD)
C       get number of attributes available on data set
        PSA  = WDGIVL(WDMSFL,DREC,IAP)
        SAMAX= WDGIVL(WDMSFL,DREC,PSA)
        IF (SCFG.EQ.1 .AND. SAMAX.GT.SLINES-3 .AND.
     $      (KNT.GT.SLINES-3 .OR. FULLFG.EQ.1)) THEN
C         more attributes to output than will fit on one screen
C         keep track of screens output
          ISC= 1
          SCIND(ISC)= 1
        ELSE
C         don't need to keep track of screens
          ISC= -1
        END IF
C       get header
        SGRP= 2
        IF (SCFG.EQ.1) THEN
C         output header to screen
          CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,-I1,I1,DSNBUF(IDS))
          INIT  = 0
          SKOUNT= 2
        END IF
        IF (TORF.GT.1) THEN
C         output going to file
          IF (FKOUNT.EQ.0) THEN
C           1st page, include header at top of page
            WRITE (FUNIT,2000) CHAR(12),HEADR
          ELSE
C           just start new page
            WRITE (FUNIT,2001) CHAR(12)
          END IF
          CALL PMXTFI (MESSFL,FUNIT,SCLU,SGRP,I1,DSNBUF(IDS))
          FKOUNT= 5
        END IF
        IF (FULLFG.EQ.1) THEN
C         list all available attributes
          LSACNT= SAMAX
        ELSE
C         list only selected attributes
          LSACNT= KNT
        END IF
        IRET= 1
        IF (LSACNT.GT.0) THEN
C         write out attributes
          IIND= 0
 100      CONTINUE
C           loop through attributes
            IIND= IIND+ 1
            IF (FULLFG.EQ.1) THEN
C             get next available attribute index from data-set label
              PSIND = PSA+ 2*IIND
              DSAIND= WDGIVL(WDMSFL,DREC,PSIND)
            ELSE
C             get next attribute index from array
              DSAIND= SAIND(IIND)
            END IF
            CALL WDSALV (MESSFL,WDMSFL,DREC,DSAIND,
     O                   TBUFF,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             value for attribute found
              J   = 78
              ILEN= LENSTR(J,TBUFF)
              IF (SCFG.EQ.1) THEN
C               output buffer to screen
                CALL ZBLDWR (ILEN,TBUFF,INIT,-I1,J)
                INIT  = 0
                SKOUNT= SKOUNT+ 1
              END IF
              IF (TORF.GT.1) THEN
C               output buffer to file
                CALL PRTSTR (FUNIT,ILEN,TBUFF)
                FKOUNT= FKOUNT+ 1
              END IF
            END IF
            IF (SCFG.EQ.1 .AND. SKOUNT.GE.SLINES) THEN
C             hold screen for user to view
              CALL ZBLDWR (I0,BLNK,INIT,I0,J)
C             get user screen response value
              CALL ZGTRET (IRET)
              IF (IRET.EQ.1) THEN
C               on to next screen
                ISC= ISC+ 1
                SCIND(ISC)= IIND+ 1
              ELSE IF (IRET.EQ.2) THEN
C               user wants previous screen
                IF (ISC.LE.2) THEN
C                 on 1st or 2nd screen for this data set, exit attribute loop
                  IIND= LSACNT
                  IF (ISC.EQ.1) THEN
C                   1st screen, go back to start of previous data set
                    IDS= IDS- 2
                  ELSE
C                   2nd screen, restart this data set
                    IDS= IDS- 1
                  END IF
                ELSE
C                 go back to previous screen of attributes
                  ISC = ISC- 1
                  IIND= SCIND(ISC)- 1
                END IF
              ELSE IF (IRET.EQ.7) THEN
C               user interupting listing, out of both loops
                IIND= LSACNT
                IDS = DSNCNT
              END IF
              SKOUNT= 0
              INIT  = 1
            ELSE
C             continue listing attributes
              IRET= 1
            END IF
            IF (TORF.GT.1 .AND. FKOUNT.GE.FLINES) THEN
C             more to come and we're here because new page on file needed
              WRITE(FUNIT,2001) CHAR(12)
C             reset line number in file
              FKOUNT = 4
            END IF
          IF (IIND.LT.LSACNT) GO TO 100
        END IF
C
        IF (IRET.EQ.1) THEN
C         user continuing
          IF (BEGEND.EQ.1) THEN
C           try to write out starting and ending dates
            CALL WTFNDT (WDMSFL,DSNBUF(IDS),I1,
     O                   DREC,SDAT,EDAT,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             starting date
              CALL ZIPC (I50,BLNK,TBUFF)
              ILEN= 20
              SGRP= 3
              CALL GETTXT (MESSFL,SCLU,SGRP,ILEN,TBUFF)
              J   = 0
              CALL DATLST (SDAT,TBUFF(19),J,RETCOD)
              ILEN= 19 + J
              IF (SCFG.EQ.1) THEN
C               output buffer to screen
                CALL ZBLDWR (ILEN,TBUFF,INIT,-I1,RETCOD)
                INIT= 0
              END IF
              IF (TORF.GT.1) THEN
C               output buffer to file
                CALL PRTSTR (FUNIT,ILEN,TBUFF)
              END IF
C             ending date
              CALL ZIPC (I50,BLNK,TBUFF)
              ILEN= 20
              SGRP= 4
              CALL GETTXT (MESSFL,SCLU,SGRP,ILEN,TBUFF)
              CALL DATLST (EDAT,TBUFF(19),J,RETCOD)
              ILEN= 19 + J
              IF (SCFG.EQ.1) THEN
C               output buffer to screen
                CALL ZBLDWR (ILEN,TBUFF,I0,-I1,RETCOD)
              END IF
              IF (TORF.GT.1) THEN
C               output buffer to file
                CALL PRTSTR (FUNIT,ILEN,TBUFF)
              END IF
            ELSE IF (RETCOD.EQ.-6) THEN
C             no data present
              SGRP= 5
              ILEN= 78
              CALL GETTXT (MESSFL,SCLU,SGRP,
     M                     ILEN,
     O                     TBUFF)
              IF (SCFG.EQ.1) THEN
C               output to screen
                CALL ZBLDWR (ILEN,TBUFF,INIT,-I1,J)
                INIT= 0
              END IF
              IF (TORF.GT.1) THEN
C               output buffer to file
                CALL PRTSTR (FUNIT,ILEN,TBUFF)
              END IF
            ELSE IF (RETCOD.EQ.-82) THEN
C             no date information for this type data set
              SGRP= 6
              ILEN= 78
              CALL GETTXT (MESSFL,SCLU,SGRP,
     M                     ILEN,
     O                     TBUFF)
              IF (SCFG.EQ.1) THEN
C               output to screen
                CALL ZBLDWR (ILEN,TBUFF,INIT,-I1,J)
                INIT= 0
              END IF
              IF (TORF.GT.1) THEN
C               output buffer to file
                CALL PRTSTR (FUNIT,ILEN,TBUFF)
              END IF
            END IF
          END IF
          IF (SCFG.EQ.1) THEN
C           hold screen
            CALL ZBLDWR (I0,BLNK,I0,I0,J)
C           get user exit command
            CALL ZGTRET (IRET)
            IF (IRET.EQ.2) THEN
C             user wants previous from last screen of data set
              IF (ISC.GT.1) THEN
C               restart this data set
                IDS= IDS- 1
              ELSE
C               back up one data set
                IDS= IDS- 2
              END IF
            ELSE IF (IRET.EQ.7) THEN
C             user wants to interrupt listing
              IDS= DSNCNT
            END IF
          END IF
        END IF
      IF (IDS.LT.DSNCNT .AND. IDS.GE.0) GO TO 50
C
C     turn off previous and interrupt commands
      I= 4
      CALL ZSTCMA (I,I0)
      I= 16
      CALL ZSTCMA (I,I0)
C
      RETURN
      END

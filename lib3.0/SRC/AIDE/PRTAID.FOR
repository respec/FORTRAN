C
C
C
      SUBROUTINE   PRNTXT
     I                    (UMESFL,SCLU,SGRP)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAXLIN,SCNINI
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXT
C
      MAXLIN= 20
      SCNINI= 0
C
      CALL PMXTXT (UMESFL,SCLU,SGRP,MAXLIN,SCNINI)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTXT
     I                    (UMESFL,SCLU,SGRP,MAXLIN,SCNINI)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It prompts the user every MAXLIN lines to be sure its ok
C     to continue.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,MAXLIN,SCNINI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LINCNT
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXCNT
C
C     + + + END SPECIFICATIONS + + +
C
      CALL PMXCNT (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,
     O             LINCNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTXLC
     I                    (UMESFL,SCLU,SGRP,
     O                     LINCNT)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It also returns LINCNT, the number of lines printed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,LINCNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     LINCNT - count of lines printed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAXLIN,SCNINI
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXCNT
C
C     + + + END SPECIFICATIONS + + +
C
      MAXLIN= 20
      SCNINI= 0
C
      CALL PMXCNT (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,
     O             LINCNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXCNT
     I                    (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,
     O                     LINCNT)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It prompts the user every MAXLIN lines to be sure its ok
C     to continue.  It also returns the number of lines printed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,MAXLIN,SCNINI,LINCNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C     LINCNT - count of lines printed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IWRT
C
C     + + + EXTERNALS + + +
      EXTERNAL  PMXCNW
C
C     + + + END SPECIFICATIONS + + +
C
      IWRT= 0
C
      CALL PMXCNW (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,
     O             LINCNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXCNW
     I                    (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,
     O                     LINCNT)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It prompts the user every MAXLIN lines to be sure its ok
C     to continue.  It also returns the number of lines printed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,LINCNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C     IWRT   - 1 - write and go, 0 - write and wait for user acknowledge
C             -1 - dont write yet (EMIFE only)
C     LINCNT - count of lines printed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CONT,INITFG,DONFG,OLEN,LINIT,LWRT
      CHARACTER*1 TXT(78)
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZBLDWR, WMSGTT, ZTXINI
C
C     + + + END SPECIFICATIONS + + +
C
      LINCNT= 0
      DONFG = 0
C
      IF (SCNINI.GE.0) THEN
C       initialize the menu for EMIFE screen
        LINIT= 1
        CALL ZTXINI
      ELSE
C       dont initialize menu for EMIFE
        LINIT= 0
      END IF
C
      INITFG= 1
 10   CONTINUE
C       get next record of text
        OLEN= 78
        CALL WMSGTT (UMESFL,SCLU,SGRP,INITFG,
     M               OLEN,
     O               TXT,CONT)
        INITFG= 0
C
        LINCNT= LINCNT+ 1
C       build output screen (EMIFE style)
        IF (CONT.EQ.1) THEN
C         building screen
          LWRT= -1
        ELSE
C         screen complete
          LWRT= IWRT
        END IF
        CALL ZBLDWR (OLEN,TXT,LINIT,LWRT,
     O               DONFG)
        IF (DONFG.NE.0) THEN
C         completed screen, initialize next time through
          LINIT= 1
        ELSE
C         dont initialize next time through
          LINIT= 0
        END IF
C
      IF (DONFG.LE.1 .AND. CONT.EQ.1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRNTXI
     I                    (UMESFL,SCLU,SGRP,IVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal with
C     an integer value appended to the end of the first line.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     IVAL   - integer value to append at end of first line
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAXLIN,SCNINI,IWRT,INUM,IIVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXI
C
      MAXLIN  = 20
      SCNINI  = 0
      INUM    = 1
      IIVAL(1)= IVAL
      IWRT    = 0
C
      CALL PMXTXI (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,INUM,IIVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTXI
     I                  (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,INUM,IVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It fills in values from IVAL whenever delimeter is found.
C     It prompts the user every MAXLIN lines to be sure its ok
C     to continue.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,INUM,IVAL(INUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C     IWRT   - 1 - write and go, 0 - write and wait for user acknowledge
C             -1 - dont write yet (EMIFE only)
C     INUM   - number of integers to include in message
C     IVAL   - array containing integers to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,ONUM,OTYP(50),CLEN(1)
      REAL        RVAL(1)
      DOUBLE PRECISION DVAL(1)
      CHARACTER*1 CVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXM
C
C     + + + END SPECIFICATIONS + + +
C
      RVAL(1)= -999.
      DVAL(1)= -999.
      CLEN(1)= 1
      CVAL(1)= ' '
      ONUM   = INUM
      DO 10 I= 1,ONUM
        OTYP(I)= 1
 10   CONTINUE
C
      CALL PMXTXM (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,ONUM,OTYP,IWRT,
     I             IVAL,RVAL,DVAL,CLEN,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRNTXR
     I                    (UMESFL,SCLU,SGRP,RVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal with
C     an decimal value appended to the end of the first line.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP
      REAL      RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     RVAL   - array containing decimals to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAXLIN,SCNINI,RNUM,IWRT
      REAL      RRVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXR
C
      MAXLIN  = 20
      SCNINI  = 0
      RNUM    = 1
      RRVAL(1)= RVAL
      IWRT    = 0
C
      CALL PMXTXR (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,RNUM,RRVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTXR
     I                  (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,RNUM,RVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It fills in values from RVAL whenever delimeter is found.
C     It prompts the user every MAXLIN lines to be sure its ok
C     to continue.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,RNUM
      REAL      RVAL(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C     IWRT   - 1 - write and go, 0 - write and wait for user acknowledge
C             -1 - dont write yet (EMIFE only)
C     RNUM   - number of decimal numbers to include in message
C     RVAL   - array containing numbers to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,ONUM,OTYP(50),IVAL(1),CLEN(1)
      DOUBLE PRECISION DVAL(1)
      CHARACTER*1 CVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXM
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL(1)= -999
      DVAL(1)= -999.
      CLEN(1)= 1
      CVAL(1)= ' '
      ONUM   = RNUM
      DO 10 I= 1,ONUM
        OTYP(I)= 2
 10   CONTINUE
C
      CALL PMXTXM (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,ONUM,OTYP,IWRT,
     I             IVAL,RVAL,DVAL,CLEN,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRNTXD
     I                    (UMESFL,SCLU,SGRP,DVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal with
C     a double precision value appended to the end of the first line.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          UMESFL,SCLU,SGRP
      DOUBLE PRECISION DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     DVAL   - array containing decimals to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          MAXLIN,SCNINI,DNUM,IWRT
      DOUBLE PRECISION DDVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXD
C
      MAXLIN  = 20
      SCNINI  = 0
      DNUM    = 1
      DDVAL(1)= DVAL
      IWRT    = 0
C
      CALL PMXTXD (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,DNUM,DDVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTXD
     I                  (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,DNUM,DVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It fills in values from DVAL whenever delimeter is found.
C     It prompts the user every MAXLIN lines to be sure its ok
C     to continue.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,DNUM
      DOUBLE PRECISION DVAL(DNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C     IWRT   - 1 - write and go, 0 - write and wait for user acknowledge
C             -1 - dont write yet (EMIFE only)
C     DNUM   - number of double precision numbers to include in message
C     DVAL   - array containing numbers to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IVAL(1),CLEN(1),ONUM,OTYP(50)
      REAL        RVAL(1)
      CHARACTER*1 CVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXM
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL(1)= -999
      RVAL(1)= -999.
      CLEN(1)= 1
      CVAL(1)= ' '
      ONUM   = DNUM
      DO 10 I= 1,ONUM
        OTYP(I)= 3
 10   CONTINUE
C
      CALL PMXTXM (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,ONUM,OTYP,IWRT,
     I             IVAL,RVAL,DVAL,CLEN,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRNTXA
     I                    (UMESFL,SCLU,SGRP,CLEN,CVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal
C     with an ascii value (CVAL) of length LEN appended to the
C     end of the first line.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,CLEN
      CHARACTER*1 CVAL(CLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     CLEN   - length of character string
C     CVAL   - ascii value to append at end of first line
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MAXLIN,SCNINI,CNUM,IWRT,ICLEN(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXA
C
C     + + + END SPECIFICATIONS + + +
C
      MAXLIN= 20
      SCNINI= 0
      CNUM  = 1
      IWRT  = 0
      ICLEN(1)= CLEN
C
      CALL PMXTXA (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,CNUM,ICLEN,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTXA
     I            (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,CNUM,CLEN,CVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It fills in values from CVAL whenever a delimeter is found.
C     It prompts the user every MAXLIN lines to be sure its ok
C     to continue.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,MAXLIN,SCNINI,IWRT,CNUM,CLEN(CNUM)
      CHARACTER*1 CVAL(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing text to print
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C     IWRT   - 1 - write and go, 0 - write and wait for user acknowledge
C             -1 - dont write yet (EMIFE only)
C     CNUM   - number of character strings to include in message
C     CLEN   - array of lengths of character strings
C     CVAL   - array containing character strings to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,IVAL(1),ONUM,OTYP(50)
      REAL       RVAL(1)
      DOUBLE PRECISION DVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXM
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL(1)= -999
      RVAL(1)= -999.
      DVAL(1)= -999.
      ONUM   = CNUM
      DO 10 I= 1,ONUM
        OTYP(I)= 4
 10   CONTINUE
C
      CALL PMXTXM (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,ONUM,OTYP,IWRT,
     I             IVAL,RVAL,DVAL,CLEN,CVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTXM
     I                   (UMESFL,SCLU,SGRP,MAXLIN,SCNINI,ONUM,OTYP,IWRT,
     I                    IVAL,RVAL,DVAL,CLEN,CVAL)
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a terminal.
C     It fills in values from the IVAL, RVAL, DVAL and CVAL arrays
C     whenever a delimeter is found.  It prompts the user every
C     MAXLIN lines to be sure its ok to continue.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,MAXLIN,SCNINI,ONUM,OTYP(ONUM),
     1            IWRT,IVAL(*),CLEN(*)
      REAL        RVAL(*)
      DOUBLE PRECISION DVAL(*)
      CHARACTER*1 CVAL(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number for WDM file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     MAXLIN - maximum number of lines to print before more prompt
C     SCNINI - screen initialization flag (-1 - dont clear anything,
C                                           0 - clear in EMIFE mode only
C                                           1 - always clear)
C     ONUM   - number of variables to be added to the text
C     OTYP   - array of types for variables to be added to the text
C     IWRT   - 1 - write and go, 0 - write and wait for user acknowledge
C             -1 - dont write yet (EMIFE only)
C     IVAL   - array containing integers to include
C     RVAL   - array containing reals to include
C     DVAL   - array containing double precisions to include
C     CLEN   - array of lengths of character strings
C     CVAL   - array containing character strings to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I78,CONT,OPOS,OLEN,ICNT,RCNT,
     1            DCNT,CCNT,OCNT,DONFG,LWRT,LINIT,INITFG
      CHARACTER*1 DELIM(1),TXT(78)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, TXMULT, WMSGTT, ZBLDWR, ZTXINI
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
      I78   = 78
      OCNT  = 1
      OPOS  = 1
      DONFG = 0
      ICNT  = 1
      RCNT  = 1
      DCNT  = 1
      CCNT  = 1
      DELIM(1)= '&'
C
      IF (SCNINI.GE.0) THEN
C       initialize the menu for EMIFE screen
        LINIT= 1
        CALL ZTXINI
      ELSE
C       dont initialize menu for EMIFE
        LINIT= 0
      END IF
C
      INITFG= 1
 10   CONTINUE
C       get next record of text
        OLEN= I78
        CALL WMSGTT (UMESFL,SCLU,SGRP,INITFG,
     M               OLEN,
     O               TXT,CONT)
        INITFG= 0
C
C       look for delimeter within string to be printed
        CALL TXMULT (ONUM,OTYP,IVAL,RVAL,DVAL,CLEN,CVAL,DELIM,I78,
     M               OCNT,ICNT,RCNT,DCNT,CCNT,OPOS,TXT,CONT,
     O               DONFG)
C
C       print this part of the line
        I= LENSTR (I78,TXT)
C       build output screen
        IF (CONT.EQ.1) THEN
C         building screen
          LWRT= -1
        ELSE
C         screen complete
          LWRT= IWRT
        END IF
        IF (I.EQ.0) I= 1
        CALL ZBLDWR (I,TXT,LINIT,LWRT,
     O               DONFG)
C       dont initialize next time through
        LINIT= 0
C
      IF (CONT.EQ.1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXUPD
     I                   (MESSFL,SCLU,SGRP,INIT,ILEN,IVAL)
C
C     + + + PURPOSE + + +
C     display update of a screen generated by PMXTXI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,INIT,ILEN(2),IVAL(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     SGRP   - group number on message file
C     INIT   - flag indicating first write to screen
C     ILEN   - length of integer values
C     IVAL   - integer values to be displayed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I1,I80,IP1,IP2,LEN,JUST,IOFF,LIN
      CHARACTER*1  TBUFF(80),CHAND(1),BLNK
      CHARACTER*10 LBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LBUFF,LBUF1)
      CHARACTER*1  LBUF1(10)
C
C     + + + SAVES + + +
      SAVE         IP1,IP2
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND, ZLNTXT, LENSTR, ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL     STRFND, ZLNTXT, LENSTR, ZCMDON, ZIPC, GETTXT
      EXTERNAL     ZBLDWR, ZWRSCR, INTCHR, CHRINS
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      I80  = 80
      JUST = 0
      BLNK = ' '
      CHAND(1)= '&'
C
      IF (INIT.EQ.1) THEN
C       different question than last time, get new text
        CALL ZIPC (I80,BLNK,TBUFF)
        LEN= 80
        CALL GETTXT (MESSFL,SCLU,SGRP,LEN,TBUFF)
        IP1= STRFND(LEN,TBUFF,I1,CHAND)
        CALL INTCHR (IVAL(1),ILEN(1),JUST,
     O               I,LBUF1)
        TBUFF(IP1)= LBUF1(1)
        LEN= LEN+ ILEN(1)
        DO 10 I= 2,ILEN(1)
          CALL CHRINS (LEN,IP1+I-1,LBUF1(I),TBUFF)
 10     CONTINUE
        IP2= STRFND(LEN,TBUFF,I1,CHAND)
        CALL INTCHR (IVAL(2),ILEN(2),JUST,
     O               I,LBUF1)
        TBUFF(IP2)= LBUF1(1)
        LEN= LEN+ ILEN(2)
        DO 20 I= 2,ILEN(2)
          CALL CHRINS (LEN,IP2+I-1,LBUF1(I),TBUFF)
 20     CONTINUE
        I  = 78
        LEN= LENSTR (I,TBUFF)
        CALL ZBLDWR (LEN,TBUFF,I1,I1,J)
        IF (LEN.LT.78) THEN
C         offset integer positions for centering
          IOFF= (78-LEN)/2+ 1.51
          IP1 = IP1+ IOFF
          IP2 = IP2+ IOFF
        END IF
      ELSE
C       same message as last time, just update numbers
        LBUFF= ' '
        I= 20
        IF (ZCMDON(I).EQ.1) THEN
C         'Quiet' available, smaller data window
          LIN= 6
        ELSE
C         'Quiet' not available, larger data window
          LIN= 9
        END IF
        CALL INTCHR (IVAL(1),ILEN(1),JUST,
     O               I,LBUF1)
        LEN= ZLNTXT(LBUFF)
        CALL ZWRSCR (LBUFF(1:LEN),LIN,IP1)
        LBUFF= ' '
        CALL INTCHR (IVAL(2),ILEN(2),JUST,
     O               I,LBUF1)
        LEN= ZLNTXT(LBUFF)
        CALL ZWRSCR (LBUFF(1:LEN),LIN,IP2)
      END IF
C
      RETURN
      END

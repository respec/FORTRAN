C     qtchar.f 2.1 9/4/91
C
C
C
      SUBROUTINE   GETTXT
     I                   (UMESFL,SCLU,SGRP,
     M                    OLEN,
     O                    OBUFF)
C
C     + + + PURPOSE + + +
C     reads text from the message file into a buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,OLEN
      CHARACTER*1 OBUFF(OLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing string to read
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     OLEN   - length of string read from message file
C     OBUFF  - string read from message file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,INITFG
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSGTT, ZIPC
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK  = ' '
      INITFG= 1
C     init output buffer
      CALL ZIPC (OLEN,BLNK,OBUFF)
C     get the string from the message file
      CALL WMSGTT (UMESFL,SCLU,SGRP,INITFG,
     M             OLEN,
     O             OBUFF,I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QTSTR
     I                  (UMESFL,SCLU,SGRP,LEN,
     O                   STR)
C
C     + + + PURPOSE + + +
C     This routine, after prompting the user, reads text from the
C     terminal into the buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number for WDM file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     LEN    - length of string to read
C     STR    - string containing users response
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,RETCOD,IRET,SGLCHR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, ZEDT0M, ZLJUST, WMSGTP
C
C     + + + END SPECIFICATIONS + + +
C
      SGLCHR = 0
C
 20   CONTINUE
C       get needed info from WDM file
        CALL WMSGTP (UMESFL,SCLU,SGRP,
     O               I,RETCOD)
C       put current value of string in menu text
        I= SCOL(1)
        CALL CHRCHR (LEN,STR,ZMNTX1(I,FLIN(1)))
        CALL ZEDT0M (SGLCHR,
     O               IRET)
C       do again on 'Oops'
      IF (IRET.EQ.-1) GO TO 20
C     put value from menu text into STR
      I= SCOL(1)
      J= SCOL(1)+ FLEN(1)- 1
      CALL ZLJUST (ZMNTXT(FLIN(1))(I:J))
      CALL CHRCHR (LEN,ZMNTX1(I,FLIN(1)),STR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRSPIN
     I                   (INANS,ILANS,CTANS)
C
C     + + + PURPOSE + + +
C     routine to set responses in common for QRESP (menu) type question
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     INANS,ILANS
      CHARACTER*1 CTANS(ILANS,INANS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INANS - number of valid responses
C     ILANS - length of valid responses
C     CTANS - buffer of valid responses
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cqrsp.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     TLANS,LEN,POS
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK / ' ' /
C
C     + + + END SPECIFICATIONS + + +
C
C     full length of valid responses
      TLANS= INANS* ILANS
C
C     fill in common block values
      NANS= INANS
      LANS= ILANS
      CALL CHRCHR (TLANS,CTANS,TANS)
C     turn on init responses flag
      RSINIT= 1
C
C     blank out unused portion of common buffer (TANS)
      LEN= 480- TLANS
      POS= TLANS+ 1
      IF (LEN .GT. 0) CALL ZIPC (LEN,BLNK,TANS(POS))
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESP
     I                  (UMESFL,SCLU,SGRP,
     M                   RESP)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses in the message file are used to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are specific
C     character strings.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,RESP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing question
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     RESP   - number of users response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     STRLEN,JUST,DREC,DPOS,IPARM,ID,IX,LRESP
      CHARACTER*1 STR(63),BLNK
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESPS, ZIPC, WMSIDP, WDNXDV, WMSMNS
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK= ' '
C     find position of integer parameters id
      ID= 2
      CALL WMSIDP (UMESFL,SCLU,SGRP,ID,
     O             DREC,DPOS)
C     get integer parm word from wdm (next data value)
      CALL WDNXDV (UMESFL,
     M             DREC,DPOS,
     O             IPARM)
C     get length of string and def response from integer parm word
      CALL WMSMNS (IPARM,
     O             LRESP,STRLEN,IX,IX,IX)
      IF (RESP.LE.0 .OR. RESP.GT.100) THEN
C       no current value, use default
        RESP= LRESP
      END IF
C
      CALL ZIPC (STRLEN,BLNK,STR)
C     get response
      JUST= 1
      CALL QRESPS (UMESFL,SCLU,SGRP,STRLEN,JUST,
     M             STR,RESP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESPS
     I                   (UMESFL,SCLU,SGRP,STRLEN,JUST,
     M                    STR,STRNUM)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses in the message file are used to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are specific
C     character strings.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UMESFL,SCLU,SGRP,STRLEN,JUST,STRNUM
      CHARACTER*1 STR(STRLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing question
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     STRLEN - length of string response
C     JUST   - justification of string (0-right, 1-left)
C     STRNUM - number of users response
C     STR    - users response string
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cqrsp.inc'
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,CLEN,STRPOS,LEN,NUMB,WIDTH,RETCOD
      CHARACTER*63 ANSTR
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR, STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSTR, STRFND, CHRCHR, WMSGTM, ZSLMNN
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (63A1)
C
C     + + + END SPECIFICATIONS + + +
C
C
C     get parameters from WDM file
      CALL WMSGTM (MESSFL,SCLU,SGRP,
     O             NUMB,WIDTH,CLEN,RETCOD)
      IF (RETCOD.NE.0) WRITE (*,*) 'WDM problems, RETCOD',RETCOD
C
      IF (LENSTR(STRLEN,STR).GT.0) THEN
C       default response string already set, determine resp number
        I  = 480
        LEN= LENSTR(I,TANS)
        STRPOS= STRFND(LEN,TANS,STRLEN,STR)
        IF (STRPOS.GT.0) THEN
C         default matches a valid response
          STRNUM= STRPOS/LANS+ 1
        ELSE
C         no match found, set default to first response
          STRNUM= 1
          CALL CHRCHR (LANS,TANS,STR)
        END IF
      ELSE IF (STRNUM.GT.0.AND.STRNUM.LE.NANS) THEN
C       use the integer value of the string for default
        I= LANS* (STRNUM-1)+ 1
        CALL CHRCHR(LANS,TANS(I),STR)
      ELSE
C       still no valid response, set to one
        STRNUM= 1
      END IF
      DANS= STRNUM
C
      CALL ZSLMNN (NUMB,WIDTH,CLEN,
     M             STRNUM,
     O             ANSTR,RETCOD)
      IF (RETCOD.EQ.1) THEN
        READ (ANSTR,1000) (STR(I),I=1,LANS)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKANS
     I                   (ULEN,STR,BLNK,UMESFL,GPTR,HPTR,
     O                    RESP)
C
C     + + + PURPOSE + + +
C     This routine checks character answers supplied by the user.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ULEN,RESP,UMESFL,GPTR,HPTR(*)
      CHARACTER*1 BLNK,STR(ULEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ULEN   - length of users response
C     UMESFL - message file containing question
C     GPTR   - pointer to general help info
C     HPTR   - array of pointers to specific option help
C     BLNK   - character blank
C     STR    - users response string
C     RESP   - number of users response
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cqrsp.inc'
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIALBLES + + +
      INTEGER     TRESP,CRESP,I,J,I80,K,IANS,LSCLU,LSGRP,TLEN
      CHARACTER*1 HELP
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHKANQ, CHRCHR, LENSTR, PRNTXT, PRTANS, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA HELP/'?'/
C
C     + + + END SPECIFICATIONS + + +
C
      I80  = 80
      RESP = 0
      LSCLU= 1
C
      TLEN= LANS* NANS
      CALL CHKANQ (NANS,LANS,ULEN,TLEN,TANS,
     M             STR,
     O             TRESP,CRESP)
C
      IF (CRESP.NE.0) THEN
C       print conflicting response message
        CALL ZIPC(I80,BLNK,OBUFF)
C       fill the output buffer with the confliction values
        IANS = (TRESP- 1)* LANS+ 1
        CALL CHRCHR (LANS,TANS(IANS),OBUFF)
C
        K= LENSTR(LANS,OBUFF)+ 1
        OBUFF(K) = ','
        K= K+ 2
        IANS = (CRESP- 1)* LANS+ 1
        CALL CHRCHR (LANS,TANS(IANS),OBUFF(K))
C
        LSGRP= 5
        CALL PRNTXT(MESSFL,LSCLU,LSGRP)
C       CALL PRTSCR(I80,OBUFF)
      ELSE IF (TRESP.LE.0) THEN
C       no valid response entered
        IF (STR(1).EQ.HELP) THEN
C         user needs help
          I = - 1
          IF (GPTR.NE.0) THEN
C           show general help
            CALL PRNTXT(UMESFL,I,GPTR)
          END IF
          DO 10 J= 1,NANS
C           show specific help if it exists
            IF (HPTR(J).NE.0) THEN
              CALL PRNTXT(UMESFL,I,HPTR(J))
            END IF
 10       CONTINUE
C         print a list of valid responses.
          LSGRP= 4
          CALL PRNTXT(MESSFL,LSCLU,LSGRP)
C         print the valid values
          I= LANS* NANS
          CALL PRTANS (BLNK,I,LANS,NANS,TANS,
     O                 OBUFF,J)
        ELSE
C         print bad response message.
          LSGRP= 6
          CALL PRNTXT(MESSFL,LSCLU,LSGRP)
        END IF
      ELSE
C       good response
        RESP = TRESP
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKANQ
     I                   (NANS,LANS,ULEN,TLEN,TANS,
     M                    STR,
     O                    TRESP,CRESP)
C
C     + + + PURPOSE + + +
C     This routine checks character answers supplied by the user.
C     It also returns the number of a conflicting answer if it exists.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NANS,LANS,ULEN,TLEN,TRESP,CRESP
      CHARACTER*1 TANS(TLEN),STR(ULEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NANS   - number of answers to question
C     LANS   - length of answers to question
C     ULEN   - length of users answer
C     TLEN   - NANS*LANS
C     TANS   - allowable answers string
C     STR    - users answer string
C     TRESP  - number of users answer
C     CRESP  - number of conflict answer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     MATCH,IANS,I
      CHARACTER*1 BLNK
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QUPCAS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      TRESP= 1
      MATCH= 1
C
C     convert users response to upper case for check
      CALL QUPCAS (ULEN,
     M             STR)
C
  10  CONTINUE
        IANS = (TRESP- 1)* LANS + MATCH
        IF (STR(MATCH).EQ.TANS(IANS)) THEN
          MATCH = MATCH + 1
        ELSE
          TRESP = TRESP + 1
          MATCH = 1
        END IF
C
        IF (TRESP.GT.NANS) THEN
C         didn't find answer - get out of loop.
          TRESP = 0
          MATCH = LANS + 1
        END IF
      IF (MATCH.LE.ULEN) GO TO 10
C
C     check for conflicting response.
      CRESP= 0
      I    = IANS+ 1
      IF (I.LE.TLEN) THEN
        IF (ULEN.LT.LANS.AND.TRESP.NE.NANS.AND.TANS(I).NE.BLNK) THEN
          CRESP = TRESP + 1
          MATCH = 1
C
  20      CONTINUE
            IANS = (CRESP- 1)* LANS + MATCH
            IF (STR(MATCH).EQ.TANS(IANS)) THEN
              MATCH = MATCH + 1
            ELSE
              CRESP = CRESP + 1
              MATCH = 1
            END IF
C
            IF (CRESP.GT.NANS) THEN
C             no conflicting response - get out
              CRESP = 0
              MATCH = LANS + 1
            END IF
C
          IF (MATCH.LE.ULEN) GO TO 20
C
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTANS
     I                   (BLNK,TLEN,LANS,NANS,TANS,
     O                    OBUFF,LINCNT)
C
C     + + + PURPOSE + + +
C     prints the valid character responses for a particular question
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TLEN,LANS,NANS,LINCNT
      CHARACTER*1 TANS(TLEN),BLNK,OBUFF(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BLNK   - blank character
C     TLEN   - total length of character responses(NANS*LANS)
C     LANS   - length of answers to question
C     NANS   - number of answers to question
C     TANS   - values of allowable character responses
C     OBUFF  - buffer used to print responses
C     LINCNT - number of lines printed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,K,IANS,I80
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRCHR, LENSTR, ZIPC
C
C     + + + END SPECIFICATIONS + + +
C
      I  = 0
      I80= 80
      LINCNT= 0
 10   CONTINUE
C       clear output buffer
        CALL ZIPC (I80,BLNK,OBUFF)
        K= 2
C
 20     CONTINUE
          I= I+ 1
          IANS = (I- 1)* LANS+ 1
          CALL CHRCHR (LANS,TANS(IANS),OBUFF(K))
          K= LENSTR(K+LANS,OBUFF)+ 1
          IF (I.LT.NANS) THEN
            OBUFF(K)= ','
            K= K+ 2
          END IF
        IF (K+LANS.LE.77.AND.I.LT.NANS) GO TO 20
C       CALL PRTSCR (K,OBUFF)
        LINCNT= LINCNT+ 1
      IF (I.LT.NANS) GO TO 10
C
      RETURN
      END

C
C
C
      SUBROUTINE   INIKEY
C
C     + + + PURPOSE + + +
C     delete all uci keys
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_KEY_DEL (0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   KEYUCI
     I                   (NKWD,LKWD,PKWD,IKWD,IRPT,
     I                    KWDLIB,KWDDIM,KWDTYP,
     M                    ECNT,
     O                    KCNT)
C
C     + + + PURPOSE + + +
C     Look for keywords in a UCI file, save results with uci
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NKWD,LKWD,PKWD,IKWD,IRPT,KWDDIM(NKWD),KWDTYP(NKWD),
     $            ECNT,KCNT,RETCOD
      CHARACTER*1 KWDLIB(*)
C
C     + + + ARGUEMENT DEFINITIONS + + +
C     NKWD   - number of keywords to look for
C     LKWD   - length of keywords
C     PKWD   - offset to position of keyword on input
C     IKWD   - type of base keyword
C     IRPT   - which start at which copy of keyword
C     KWDLIB - keyword values
C     KWDDIM - max number of keyword occurences allowed
C     KWDTYP - type of each keyword
C     ECNT   - errors found count
C     KCNT   - keyword found count
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NREC,LPOS,RPOS,SCLU,SGRP,LOUT,LMOV,I,EREC,
     #              OPNREC,OPNKWD,KWDNO,ENDFLG,I4,I1,I0,
     #              UMSGFL,UMESSU
      LOGICAL       FOUND
      CHARACTER*132 UCIBUF
      CHARACTER*1   TXT1(20),COUT(12),CKEY(12),BLNK(1),CEND1(4)
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (CEND1,CEND),(TXT1,TXT)
      CHARACTER*4  CEND
      CHARACTER*20 TXT
C
C     + + + FUNCTIONS + + +
      INTEGER      CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSG,OMSTC,OMSTI
      EXTERNAL     GETSE,GETEND,ZIPC,CHKSTR,PUTKWD,CHRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (132A1)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (' FOUND ',A)
C
C     + + + END SPECIFICATIONS + + +
C
C     end keyword
      CALL GETEND(CEND)
C     output unit
      UMESSU = UCI_GET_MES()
C     message unit
      UMSGFL = UCI_GET_MSG()
C     no keywords found yet
      KCNT= 0
C
      I0= 0
      I1= 1
      I4= 4
C
      IF (IKWD .EQ. 0) THEN
C       start looking at first record
        NREC= -1
        EREC= 0
      ELSE
C       start looking where base is
        CALL GETSE(IKWD,IRPT,
     O             NREC,EREC)
      END IF
C
      LOUT  = 12
      CALL ZIPC (LOUT,BLNK,COUT)
      CALL ZIPC (LOUT,BLNK,CKEY)
      IF (LKWD .LT. LOUT) THEN
C       just update whats needed
        LMOV = LKWD
      ELSE
C       truncate
        LMOV = LOUT
      END IF
C
C     cluster for problem messages
      SCLU  = 210
C     no keyword open
      OPNKWD= 0
C     where to look for keyword in uci
      LPOS  = PKWD + 1
      RPOS  = PKWD + LKWD
C
 10   CONTINUE
C       get next non comment or blank record
        CALL UCI_GET(I0,
     M               NREC,
     O               FOUND,UCIBUF)
        READ(UCIBUF(LPOS:RPOS),1000) (TXT1(I),I=1,LKWD)
C       see if this a wanted keyword
        KWDNO= CHKSTR(LKWD,NKWD,TXT1,KWDLIB)
C
        IF (KWDNO .GT. 0) THEN
C         recognized, save the name
          I = (KWDNO-1)* LKWD + 1
          CALL CHRCHR(LMOV,KWDLIB(I),CKEY)
          IF (UMESSU .GT. 0) THEN
            IF (IKWD.LT.100 .OR. UCI_GET_DBG().GT.4) THEN
C             echo it
              WRITE (UMESSU,2010) UCIBUF(LPOS:RPOS)
            END IF
          END IF
          IF (OPNKWD .GT. 0) THEN
C           error - two data set headings found without an end between
            CALL OMSTC(LOUT,CKEY)
            I = (OPNKWD-1)* LKWD + 1
            CALL CHRCHR(LMOV,KWDLIB(I),COUT)
            CALL OMSTC(LOUT,COUT)
            SGRP= 2
            CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                 ECNT)
          END IF
C         ok - remember the key to this line
          OPNKWD= KWDNO
          OPNREC= NREC
        ELSE
C         unrecognized, is it end?
          ENDFLG = CHKSTR(I4,I1,TXT1,CEND1)
          IF (ENDFLG .EQ. 1) THEN
C           found a end delimiter - which is it?
            READ(UCIBUF(LPOS+I4:RPOS+I4),1000) (TXT1(I),I=1,LKWD)
            KWDNO= CHKSTR(LKWD,NKWD,TXT1,KWDLIB)
            IF (KWDNO .GT. 0) THEN
C             recognized
              IF (UMESSU .GT. 0) THEN
                IF (IKWD.LT.100 .OR. UCI_GET_DBG().GT.4) THEN
C                 echo it
                  WRITE (UMESSU,2010) UCIBUF(LPOS:RPOS+4)
                END IF
              END IF
C             does it match the heading?
              IF (OPNKWD .EQ. 0) THEN
C               there was no preceding keyword
                I = (KWDNO-1)* LKWD + 1
                CALL CHRCHR(LMOV,KWDLIB(I),COUT)
                CALL OMSTC(LOUT,COUT)
                SGRP= 5
                CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                     ECNT)
              ELSE IF (KWDNO .NE. OPNKWD) THEN
C               end doesn't match heading
                I = (KWDNO-1)* LKWD + 1
                CALL CHRCHR(LMOV,KWDLIB(I),COUT)
                CALL OMSTC(LOUT,COUT)
                CALL OMSTC(LOUT,CKEY)
                SGRP= 6
                CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                     ECNT)
              ELSE
C               complete
                CALL PUTKWD (KWDDIM(KWDNO),KWDTYP(KWDNO),
     I                       CKEY,OPNREC,NREC,
     O                       RETCOD)
                IF (RETCOD .NE. 0) THEN
C                 too many occurances
                  I = (KWDNO-1)* LKWD + 1
                  CALL CHRCHR(LMOV,KWDLIB(I),COUT)
                  CALL OMSTC(LOUT,COUT)
                  CALL OMSTI(KWDDIM(KWDNO))
                  SGRP= 3
                  CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                       ECNT)
                END IF
                OPNKWD= 0
                KCNT  = KCNT+ 1
              END IF
            END IF
          ELSE
C           line is neither a data set heading or delimiter
          END IF
        END IF
      IF (FOUND .AND. NREC.NE.EREC) GO TO 10
C
      IF (OPNKWD .GT. 0) THEN
C       end of block was encountered before the end keyword was found
        CALL OMSTC(LOUT,CKEY)
        SGRP= 7
        CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M             ECNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCIINP
     I                   (UCIFL,MSGFL,MESSU)
C
C     + + + PURPOSE + + +
C     Read a complete data set from the user's control input,
C     identify comments and blank lines, save it in memory
C
C     + + + MODULES + + +
      USE       UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UCIFL,MSGFL,MESSU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIFL  - fortran unit number of file containing hspf input
C     MSGFL  - unit number of message file containing info needed
C     MESSU  - unit number of output file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,CLEN
      CHARACTER*1  CEND1(4)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CEND1,CEND)
      CHARACTER*4  CEND
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT
C
C     + + + END SPECIFICATIONS + + +
C
C     end string
      SCLU  = 210
      SGRP  = 1
      CLEN  = 4
      CALL WMSGTT (MSGFL,SCLU,SGRP,SGRP,
     M             CLEN,
     O             CEND1,I)
C
C     uci file
      CALL UCI_NEW (MSGFL=MSGFL,MESSU=MESSU,CEND=CEND)
C
C     read file
      CALL UCI_REA (UCIFL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMKEY
     I                   (LREC)
C
C     + + + PURPOSE + + +
C     comment out a UCI line
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LREC   - record to comment
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      TYPE
C
C     + + + END SPECIFICATIONS + + +
C
      TYPE = -1
      CALL UCI_REP (LREC,TYP=TYPE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PREUCI
     M                   (LKEY)
C
C     + + + PURPOSE + + +
C     given a key, return the key of the previous uci line
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LKEY   - uci key
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL   FOUND
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_PRV(LKEY,FOUND)
C
      IF (.NOT. FOUND) THEN
        WRITE(99,*) 'PREV to record',LKEY,' not found'
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELUCI
     I                   (LDREC)
C
C     + + + PURPOSE + + +
C     delete a UCI line and assoc info
C
C     + + + MODULES + + +
      USE       UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LDREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LDREC  - record to delete
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL   FOUND
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_DEL (LDREC,FOUND)
C
      IF (.NOT.FOUND) THEN
        WRITE(99,*) 'Record ',LDREC,' not found to delete.'
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETUCI
     I                  (LTYPRC,
     M                   NREC,
     O                   UCIBUF)
C
C     + + + PURPOSE + + +
C     Retrieve a UCI line
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       LTYPRC,NREC
      CHARACTER*132 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LTYPRC - type of record: -2:blank line,
C                              -1:comment,
C                               0:unknown,
C                              >0:something useful
C     NREC   - record number to start looking after and record found
C     UCIBUF - UCI line
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL   FOUND
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_GET(LTYPRC,
     M             NREC,
     O             FOUND,UCIBUF)
C
      IF (.NOT. FOUND) THEN
        NREC= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTUCI
     I                   (UCIBUF,LTYPRC,LPRERC)
C
C     + + + PURPOSE + + +
C     Save a UCI line and assoc info
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       LTYPRC,LPRERC
      CHARACTER*132 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - UCI record
C     LTYPRC - type of record: -2:blank line,
C                              -1:comment,
C                               0:unknown,
C                              >0:something useful
C     LPRERC - record to write this one after
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_INS (LPRERC, UCIBUF, LTYPRC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   REPUCI
     I                   (LREC,UCIBUF,RTYP)
C
C     + + + PURPOSE + + +
C     replace a UCI line, no change to assoc info
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       LREC, RTYP
      CHARACTER*132 UCIBUF
C
      OPTIONAL      RTYP,UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LREC   - record to replace
C     UCIBUF - new record
C     RTYP   - type of record
C
C     + + + END SPECIFICATIONS + + +
C
      IF (.NOT. PRESENT(UCIBUF)) THEN
        CALL UCI_REP (LREC,TYP=RTYP)
      ELSE IF (.NOT. PRESENT(RTYP)) THEN
        CALL UCI_REP (LREC,BUFF=UCIBUF)
      ELSE
        CALL UCI_REP (LREC,UCIBUF,RTYP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTOLV
     I                   (OUTLEV)
C
C     + + + PURPOSE + + +
C     Save output level
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - output level
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_PUT_DBG(OUTLEV)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DUMPER
     I                   (KEYST,KEYND,MESSU)
C
C     + + + PURPOSE + + +
C     Dump a specified range of the user's control input to messu
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    KEYND,KEYST,MESSU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record numberending record number
C     KEYND  - ending record number
C     MESSU  - ftn unit no. to be used for printout of messages
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU, SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL     PMXTFT
C
C     + + + END SPECIFICATIONS + + +
C
C     dumping message
      SCLU= 210
      SGRP= 10
      CALL PMXTFT(UCI_GET_MSG(),MESSU,SCLU,SGRP)
C
      CALL UCI_PRT(FREC=-KEYST,LREC=KEYND,FMT=0,PUN=MESSU)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETEND
     O                   (CEND)
C
C     + + + PURPOSE + + +
C     get the end keyword
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*4 CEND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CEND    - end keyword
C
C     + + + END SPECIFICATIONS + + +
C
      CEND = UCI_GET_END()
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTKWD
     I                   (KWDDIM,KWDTYP,KNAM1,SREC,EREC,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     save info about where a keyword was found
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     KWDDIM,KWDTYP,SREC,EREC,RETCOD
      CHARACTER*1 KNAM1(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KWDDIM - max number of occurances allowed
C     KWDTYP - type of keyword
C     KNAM   - name of keyword
C     SREC   - start rec of block
C     EREC   - end rec of block
C     RETCOD - return code - 0:added ok, 1:too many
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*12  KNAME
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(12A1)
C
C     + + + END SPECIFCATIONS + + +
C
C     save name
      WRITE(KNAME,2000) KNAM1
C     record type
      CALL UCI_REP (SREC,TYP=KWDTYP)
C
      CALL UCI_KEY_PUT (KNAME,KWDDIM,KWDTYP,SREC,EREC,
     O                  RETCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELKWD
     I                   (KWDTYP)
C
C     + + + PURPOSE + + +
C     delete info about where a keyword was found because this
C     keyword no longer exists
C
C     + + + MODULES + + +
      USE UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     KWDTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KWDTYP - type of keyword
C
C     + + + END SPECIFCATIONS + + +
C
      CALL UCI_KEY_DEL(KWDTYP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DMPKEY
C
C     + + + PURPOSE + + +
C     dump keyword info known about uci
C
C     + + + MODULES + + +
      USE UCI
C
C     + + + END SPECIFICATIONS + + +
C
C     CALL UCI_KEY_PRT
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETKNM
     I                  (KTYP,KRPT,
     O                   KNAME)
C
C     + + + PURPOSE + + +
C     get the next keyword associated with a type
C
C     + + + MODULES + + +
      USE UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KTYP,KRPT
      CHARACTER*12 KNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KTYP    - type of keyword
C     KRPT    - repeat of keyword
C     KNAME   - associated name
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_KEY_GET(KTYP,KRPT,TEXT=KNAME)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETSE
     I                 (ITYPE,IRPT,
     O                  SREC,EREC)
C
C     + + + PURPOSE + + +
C     find start and end position in a uci for a type of key
C
C     + + + MODULES + + +
      USE UCI
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ITYPE,IRPT,SREC,EREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - type of key - 1:RUN, 2:GLOBAL, 100:OPN, etc.
C     IRPT   - which occurance of key
C     SREC   - starting record
C     EREC   - ending record
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_KEY_GET(ITYPE,IRPT,SREC=SREC,EREC=EREC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLOUCI
C
C     + + + PURPOSE + + +
C     close in memory uci file - free up space
C
C     + + + MODULES + + +
      USE UCI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL UCI_CLO
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETIND
     M                    (NREC,
     O                     INDENT)
C
C     + + + PURPOSE + + +
C     Retrieve the number of spaces to indent a UCI line.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NREC,INDENT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NREC   - record number
C     INDENT - number of spaces to indent line
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NREC .GT. 0) THEN
C       valid record - return indent
        INDENT= 0
        !INDENT= UCINDT(NREC)    not implemented in module yet
      ELSE
C       invalid record - no indent
        INDENT= 0
      END IF
C
      RETURN
      END

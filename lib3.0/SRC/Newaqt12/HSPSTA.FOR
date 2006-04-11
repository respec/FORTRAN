C
C
C
      SUBROUTINE   HSPSTA
     I                   (IOPT,NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO)
C
C     + + + PURPOSE + + +
C     routine to show run status for HSPF
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4   IOPT,NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output(not used in CUI version
C     NOPNS  - total number of options
C     LAST   - last time interval
C     COUNT  - number of current time interval
C     OPN    - number of current operation number
C     OMCODE - code number of current operation
C     OPTNO  - number for this operation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4   I2,I6,I78,J,SKIP,JUST,LEN,POS,SCOL,ROW,COL1,
     1            ALEN,APOS,IRET,I1
      REAL        COLWID
      CHARACTER*1 OPNAM(6,9),WBUFF1(33),OBUFF1(78),BK(1),CH1,CH2,CH3
C
C     + + + SAVES + + +
      SAVE        COLWID,SKIP,ALEN,APOS,ROW,WBUFF1
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (OBUFF1,OBUFF),(WBUFF1,WBUFF)
      CHARACTER*33  WBUFF
      CHARACTER*78  OBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR,ZIPC,CHRCHR,ZBLDWR,ZWRSCR,INTCHR,ZQUIET,
     #            ZWRVDR,XGTCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I2,I6,I78,JUST,SCOL/1,2,6,78,0,12/
      DATA WBUFF1/'O','P','x','x','x',' ','O','F','x','x','x',' ',' ',
     1           ' ','T','I','M','E',' ','P','A','D','x','x','x','x',
     2           ' ','O','F','x','x','x','x'/
      DATA OPNAM,BK(1)/'P','E','R','L','N','D','I','M','P','L','N','D',
     1                 'R','C','H','R','E','S','C','O','P','Y',' ',' ',
     2                 'P','L','T','G','E','N','D','I','S','P','L','Y',
     3                 'D','U','R','A','N','L','G','E','N','E','R',' ',
     4                 'M','U','T','S','I','N',' '/
C
C     + + + END SPECIFICATIONS + + +
C
C      WRITE(99,*) 'run stat:',COUNT,OPN
C
      CALL XGTCHR
     O            (CH1,CH2,CH3)
C
      IF (COUNT.EQ.1 .AND. OPN.EQ.1) THEN
C       determine how many lines to display
        CALL ZQUIET
C
        SKIP= (NOPNS- 1)/15+ 1
C        IF (NOPNS .GT. 60) THEN
C          SKIP= 5
C        ELSE IF (NOPNS .GT. 45) THEN
C          SKIP= 4
C        ELSE IF (NOPNS .GT. 30) THEN
C          SKIP= 3
C        ELSE IF (NOPNS .GT. 15) THEN
C          SKIP= 2
C        ELSE
C          SKIP= 1
C        END IF
C       determine column width
        COLWID= 66.0/FLOAT(LAST)
        ALEN= COLWID
C        WRITE (99,*) 'HSPSTA skip,last,colwid,alen',
C     #                SKIP,LAST,COLWID,ALEN
        APOS= 0
        ROW=  0
        IF (ALEN.EQ.0) ALEN= 1
C       clear screen
        CALL ZBLDWR (I1,BK(1),I1,I1,IRET)
      END IF
C
C     update where we are
      LEN= 3
      CALL INTCHR (OPN,LEN,JUST,J,WBUFF1(3))
      CALL INTCHR (NOPNS,LEN,JUST,J,WBUFF1(9))
      LEN= 4
      CALL INTCHR (COUNT,LEN,JUST,J,WBUFF1(23))
      CALL INTCHR (LAST,LEN,JUST,J,WBUFF1(30))
      CALL ZWRSCR (WBUFF,I2,I2)
C
      IF (MOD(OPN-1,SKIP).EQ.0.OR.OPN.EQ.1) THEN
C       make previous active section inactive
C        WRITE (99,*) 'HSPSTA count,opn',COUNT,OPN
        IF (COUNT.GT.1 .OR. OPN.GT.1) THEN
C         there was a previous active section to change
          CALL ZIPC (ALEN,CH1,OBUFF1(APOS))
C          WRITE (99,*) 'HSPSTA ch1,row,apos,alen',CH1,ROW,APOS,ALEN
C          WRITE (99,*) 'HSPSTA obuff',OBUFF
C          WRITE (99,*) 'HSPSTA obuff(apos:alen)',
C     #                  OBUFF(APOS:ALEN+APOS)
          CALL ZWRSCR (OBUFF(APOS:ALEN+APOS-1),ROW,APOS+1)
        END IF
C       display this line
        ROW = 3+ (OPN-1)/SKIP
        CALL ZIPC (I78,BK(1),OBUFF1)
C       put operation name in buffer
        CALL CHRCHR (I6,OPNAM(1,OMCODE),OBUFF1)
C       put operation number in buffer
        LEN = 4
        CALL INTCHR (OPTNO,LEN,JUST,
     O               J,OBUFF1(7))
C        WRITE (99,*) 'HSPSTA omcode,optno',OMCODE,OPTNO
C        WRITE (99,*) 'HSPSTA obuff',OBUFF
C       put completed portion in buffer
        COL1= (COUNT-1)* COLWID+ SCOL
        LEN = COL1- SCOL
        IF (LEN.GT.64) THEN
          LEN= 64
        END IF
        IF (LEN.GT.0) THEN
          CALL ZIPC (LEN,CH1,OBUFF1(SCOL))
        END IF
C        WRITE (99,*) 'HSPSTA colwid,count,scol,col1,len',
C     #                COLWID,COUNT,SCOL,COL1,LEN
C       write beginning of line and completed portion
        LEN = LENSTR(I78,OBUFF1)
        IF (LEN .LT. 12) THEN
          LEN= 12
        END IF
C        WRITE (99,*) 'HSPSTA row,i2,len',ROW,I2,LEN
C        WRITE (99,*) 'HSPSTA obuff',OBUFF
C        WRITE (99,*) 'HSPSTA obuff(1:len)',OBUFF(1:LEN)
        CALL ZWRSCR (OBUFF(1:LEN),ROW,I2)
        APOS= LEN+ 1
C       write active portion
        CALL ZIPC (ALEN,CH2,OBUFF1(APOS))
C        WRITE (99,*) 'HSPSTA ch2,row,apos,alen',CH2,ROW,APOS,ALEN
C        WRITE (99,*) 'HSPSTA obuff',OBUFF
C        WRITE (99,*) 'HSPSTA obuff(apos:alen)',
C     #                OBUFF(APOS:ALEN)
        CALL ZWRVDR (OBUFF(APOS:APOS+ALEN-1),ROW,APOS+1)
        POS = APOS+ ALEN
        LEN = I78- POS
C        WRITE (99,*) 'HSPSTA pos,len',POS,LEN
        IF (LEN.GT.0) THEN
          CALL ZIPC (LEN,CH3,OBUFF1(POS))
C         write remaining portion
C          WRITE (99,*) 'HSPSTA ch3,row,pos',CH3,ROW,POS
C          WRITE (99,*) 'HSPSTA obuff',OBUFF
C          WRITE (99,*) 'HSPSTA obuff(pos:78)',OBUFF(POS:78)
          CALL ZWRSCR (OBUFF(POS:78),ROW,POS+1)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESC
     I                   (IOPT,MESSFL,SCLU,SGRP,STRING)
C
C     + + + PURPOSE + + +
C     put message with character string to aide screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4     IOPT,MESSFL,SCLU,SGRP
      CHARACTER*64  STRING
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output(not used in CUI version
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C     STRING - character string to add to message
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    I1,I10,CLEN(1)
      CHARACTER*1  STRIN1(64)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CVARAR,PMXTXA
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I10 = 10
      CLEN(1)= 64
C
C     convert character string to array
      CALL CVARAR (CLEN(1),STRING,CLEN(1),STRIN1)
C     send message to aide screen
      CALL PMXTXA (MESSFL,SCLU,SGRP,I10,I1,I1,I1,CLEN,STRIN1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESI
     I                   (IOPT,MESSFL,SCLU,SGRP,I)
C
C     + + + PURPOSE + + +
C     put message with integer to aide screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4     IOPT,MESSFL,SCLU,SGRP,I
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output(not used in CUI version
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C     I      - integer value to add to message
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4   I1,I10,IVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    PMXTXI
C
C     + + + END SPECIFICATIONS + + +
C
      I1     = 1
      I10    = 10
      IVAL(1)= I
C
C     send message to aide screen
      CALL PMXTXI (MESSFL,SCLU,SGRP,I10,I1,I1,I1,IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMEST
     I                   (IOPT,MESSFL,SCLU,SGRP)
C
C     + + + PURPOSE + + +
C     put message (text only) to aide screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4     IOPT,MESSFL,SCLU,SGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output(not used in CUI version
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4   I1,I10,I
C
C     + + + EXTERNALS + + +
      EXTERNAL    PMXCNW
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I10 = 10
C
C     send message to aide screen
      CALL PMXCNW (MESSFL,SCLU,SGRP,I10,I1,I1,I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMES2
     I                   (IOPT,KTYP,OCCUR)
C
C     + + + PURPOSE + + +
C     put current operation or block to screen during interp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4     IOPT,KTYP,OCCUR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT    - position to output(not used in CUI version
C     KTYP    - type of keyword
C     OCCUR   - number of occurances
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    I,J
      CHARACTER*12 KNAME
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL    GETKNM,ZWRSCR,ZCMDON
C
C     + + + END SPECIFICATIONS + + +
C
C     get this keyword
      CALL GETKNM (KTYP,OCCUR,
     O             KNAME)
C
      I = 20
      IF (ZCMDON(I).EQ.0) THEN
C       quiet not on, no middle window
        I= 10
        J= 19
      ELSE
C       quiet on, middle window on
        I= 7
        J= 19
      END IF
C     write to screen
      CALL ZWRSCR (KNAME,I,J)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMES3
     I                   (IOPT,TABNAM)
C
C     + + + PURPOSE + + +
C     put current operation table name to screen during interp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT
      CHARACTER*12  TABNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT    - position to output(not used in CUI version
C     TABNAM  - table name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    I,J
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZWRSCR,ZCMDON
C
C     + + + END SPECIFICATIONS + + +
C
      I = 20
      IF (ZCMDON(I).EQ.0) THEN
C       quiet not on, no middle window
        I= 10
        J= 38
      ELSE
C       quiet on, middle window on
        I= 7
        J= 38
      END IF
C     write to screen
      CALL ZWRSCR (TABNAM,I,J)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESN
     I                   (IOPT,OPTNO)
C
C     + + + PURPOSE + + +
C     put current operation number to screen during interp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT,OPTNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   - position to output(not used in CUI version
C     OPTNO  - operation number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    I,J,K,I1,I5
      CHARACTER*1  CHRST1(5)
      CHARACTER*5  COPTNO
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZWRSCR,ZCMDON,INTCHR,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I = 20
      IF (ZCMDON(I).EQ.0) THEN
C       quiet not on, no middle window
        I= 10
        J= 32
      ELSE
C       quiet on, middle window on
        I= 7
        J= 32
      END IF
C
      I1 = 1
      I5 = 5
      IF (OPTNO.EQ.0) THEN
        COPTNO = '     '
      ELSE
        CALL INTCHR (OPTNO,I5,I1,K,CHRST1)
        CALL CARVAR (I5,CHRST1,I5,COPTNO)
      END IF
C     write to screen
      CALL ZWRSCR (COPTNO,I,J)
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION CKUSER ()
C
C     + + + PURPOSE + + +
C     ckeck user status - dummy for CUI version
C
C     + + + END SPECIFICATIONS + + +
C
      CKUSER = 0
C
      RETURN
      END
C
C
C
      SUBROUTINE SDELAY
     I                 (HUNSEC)
C
C     + + + PURPOSE + + +
C     delay specified amount of time - dummy for batch version
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     HUNSEC
C
C     + + + ARGUMENT DEFINTIONS + + +
C     HUNSEC - hundredths of a second to delay
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END

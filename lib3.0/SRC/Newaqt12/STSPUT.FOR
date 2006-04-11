C
C
C
      SUBROUTINE   STSPFA
     I                   (SREC,EREC,INTFG,UVQUAN,UVNAME,DESC1,
     I                    TMIN,TMAX,TDEF,MXUVQ,
     M                    NROW,DATFLG,CONFLG,AKEY,AVAL,
     M                    AMIN,AMAX,ADEF,AINT,
     M                    CAC,AUVQ,CDESC,ACOND)
C
C     + + + PURPOSE + + +
C     find use of this uvquan within the special actions block
C
C     + + + DUMMY ARGUMENTS + + +
C     SREC   - starting record of special actions block
C     EREC   - ending record of special actions block
C     INTFG  - integer value flag
C     UVQUAN - uvquan being searched for
C     UVNAME - uvname being searched for
C     NROW   - number of rows of uvquan records
C     DESC1  - description of uvquan
C     TMIN   - min value of this uvquan
C     TMAX   - max value of this uvquan
C     TDEF   - default value of this uvquan
C     MXUVQ  - max number of uvquan records to store
C     DATFLG - dated or undated special action flag
C     CONFLG - conditional special action flag
C     AKEY   - uci buffer keys for this record
C     AVAL   - value for this special action
C     AMIN   - min value for this special action
C     AMAX   - max value for this special action
C     ADEF   - default value for this special action
C     AINT   - integer value flag
C     CAC    - action code for this special action
C     AUVQ   - uvquan name
C     CDESC  - uvquan description
C     ACOND  - conditional string if applicable
C
C     + + + ARGUMENT DEFINITIONS + + +
      INTEGER       SREC,EREC,INTFG,NROW,MXUVQ,AKEY(MXUVQ),
     1              DATFLG(MXUVQ),CONFLG(MXUVQ),AINT(MXUVQ)
      REAL          TMIN,TMAX,TDEF,AVAL(MXUVQ),AMIN(MXUVQ),
     1              AMAX(MXUVQ),ADEF(MXUVQ)
      CHARACTER*1   CAC(3,MXUVQ)
      CHARACTER*6   UVQUAN,AUVQ(MXUVQ),UVNAME
      CHARACTER*64  DESC1,CDESC(MXUVQ),ACOND(MXUVQ)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,KEY,I,ELINE,
     2              ITEMP,STWORD,CONCNT
      REAL          RTEMP
      CHARACTER*64  CCOND(20),CTEMP
      CHARACTER*80  UCIBF
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT
C
C     + + + FUNCTIONS + + +
      INTEGER     CKNBLV
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETUCI,CKNBLV
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (60X,G10.5)
 1012 FORMAT (60X,I10)
 1020 FORMAT (20X,I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
      KEY = SREC
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
C     find first word in case line is a free-form conditional line
      I= 80
      STWORD= CKNBLV (I,UCIBF)
      IF (STWORD .LT. 1) THEN
C       set dummy stword on blank line
        STWORD= 1
      END IF
C     keep track of how many nested conditionals we're in
      CONCNT = 0
 20   CONTINUE
        IF (UCIBF(STWORD:STWORD+2) .EQ. 'IF ') THEN
C         opening of conditional
          CONCNT = CONCNT + 1
          ELINE  = STWORD+63
          IF (ELINE.GT.80) THEN
C           cant run past 80 characters
            ELINE = 80
          END IF
C         save text of conditional
          CCOND(CONCNT) = UCIBF(STWORD:ELINE)
        ELSE IF (UCIBF(STWORD:STWORD+5) .EQ. 'END IF') THEN
C         closing of conditional
          CONCNT = CONCNT - 1
        ELSE IF (UCIBF(STWORD:STWORD+6) .EQ. 'ELSE IF') THEN
C         else if term in conditional, save text
          ELINE  = STWORD+63
          IF (ELINE.GT.80) THEN
C           cant run past 80 characters
            ELINE = 80
          END IF
          CCOND(CONCNT) = UCIBF(STWORD:ELINE)
        ELSE IF (UCIBF(STWORD:STWORD+3) .EQ. 'ELSE') THEN
C         else term in conditional, save text of previous
          CTEMP = CCOND(CONCNT)
          CCOND(CONCNT) = '.NOT. ' // CTEMP
        ELSE IF ((UCIBF(3:8) .EQ. 'UVQUAN').OR.
     1           (UCIBF(3:8) .EQ. 'DISTRB').OR.
     1           (UCIBF(3:8) .EQ. 'UVNAME')) THEN
C         ignore this line
        ELSE IF (UVNAME.EQ.UCIBF(43:48)) THEN
C         action line, see if record contains a real value
          IF (INTFG.EQ.0) THEN
C           read real number
            READ (UCIBF,1010,ERR=89) RTEMP
          ELSE
C           read integer
            READ (UCIBF,1012,ERR=89) ITEMP
            RTEMP = FLOAT(ITEMP)
          END IF
          IF (NROW.LT.MXUVQ) THEN
C           save info in buffer
            NROW = NROW + 1
            AUVQ(NROW) = UVQUAN
            AKEY(NROW) = KEY
            AVAL(NROW) = RTEMP
            CDESC(NROW)= DESC1
            AMIN(NROW) = TMIN
            AMAX(NROW) = TMAX
            ADEF(NROW) = TDEF
            AINT(NROW) = INTFG
            IF (UCIBF(58:58).EQ.' ' .AND.
     1          UCIBF(59:59).EQ.' ') THEN
C             need to move up action code two places
              CAC(1,NROW) = UCIBF(60:60)
              CAC(2,NROW) = ' '
              CAC(3,NROW) = ' '
            ELSE IF (UCIBF(58:58).EQ.' ') THEN
C             need to move up action code one place
              CAC(1,NROW) = UCIBF(59:59)
              CAC(2,NROW) = UCIBF(60:60)
              CAC(3,NROW) = ' '
            ELSE
C             just store action codes
              CAC(1,NROW) = UCIBF(58:58)
              CAC(2,NROW) = UCIBF(59:59)
              CAC(3,NROW) = UCIBF(60:60)
            END IF
C           see if this special action is dated
            DATFLG(NROW)= 1
            READ (UCIBF,1020,ERR=93) ITEMP
            IF (ITEMP.GT.0) THEN
              DATFLG(NROW)= 2
            END IF
 93         CONTINUE
            IF (CONCNT.EQ.0) THEN
C             not in a conditional
              CONFLG(NROW)= 1
            ELSE
C             in a conditional
              CONFLG(NROW)= 2
              ACOND(NROW)= CCOND(CONCNT)
            END IF
          END IF
        END IF
 89     CONTINUE
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
C       find first word in case line is a free-form conditional line
        I= 80
        STWORD= CKNBLV (I,UCIBF)
        IF (STWORD .LT. 1) THEN
C         set dummy stword on blank line
          STWORD= 1
        END IF
      IF (KEY.NE.EREC) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE   STSPGR
     I                   (UVQFIL,MXGRP,MXROW,
     M                    IGROUP,GNUM,GNAME,GCONT,GDESC,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     read group information for special actions translator
C
C     + + + DUMMY ARGUMENTS + + +
C     UVQFIL - file containing uvquan information
C     MXGRP  - max number of uvquan groups
C     MXROW  - max number of elements in a uvquan group
C     IGROUP - number of uvquan groups found
C     GNAME  - name of each group
C     GDESC  - description of each group
C     GNUM   - number of uvquans in each group
C     GCONT  - array of uvquans in each group
C     RETCOD - return code, 1 if error reading file
C
C     + + + ARGUMENT DEFINITIONS + + +
      INTEGER       UVQFIL,IGROUP,MXGRP,MXROW,GNUM(MXGRP),RETCOD
      CHARACTER*6   GNAME(MXGRP),GCONT(MXROW,MXGRP)
      CHARACTER*64  GDESC(MXGRP)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        ILEN,I120
      CHARACTER*1    CSTR1(120)
      CHARACTER*120  CSTRIN
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR,CVARAR
C
C     + + + INPUT FORMATS + + +
 1005 FORMAT (A120)
 1015 FORMAT (A6,4X,A64)
 1025 FORMAT (A6)
C
C     + + + END SPECIFICATIONS + + +
C
      I120 = 120
      RETCOD = 0
C
 79   CONTINUE
        READ(UVQFIL,1005,ERR=91,END=99) CSTRIN
        CALL CVARAR (I120,CSTRIN,I120,CSTR1)
        ILEN = LENSTR(I120,CSTR1)
        IF (ILEN.GT.0) THEN
C         something on this line
          IF (IGROUP.LT.MXGRP) THEN
C           okay to add another group
            IGROUP = IGROUP + 1
            READ(CSTRIN,1015,ERR=91,END=99) GNAME(IGROUP),
     1                                      GDESC(IGROUP)
            GNUM(IGROUP) = 0
C
 77         CONTINUE
              READ(UVQFIL,1005,ERR=91,END=99) CSTRIN
              CALL CVARAR (I120,CSTRIN,I120,CSTR1)
              ILEN = LENSTR(I120,CSTR1)
              IF (ILEN.GT.0 .AND. GNUM(IGROUP).LT.MXROW) THEN
C               something on this line, read contents of this group
                GNUM(IGROUP) = GNUM(IGROUP) + 1
                READ(CSTRIN,1025,ERR=91,END=99)
     1                      GCONT(GNUM(IGROUP),IGROUP)
              END IF
            IF (ILEN.GT.0) GO TO 77
          END IF
        END IF
      GO TO 79
C
 91   CONTINUE
C       error in reading uvquan file
        RETCOD = 1
 99   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UVQ2WS
     I                   (UVQUAN,SREC,EREC,
     O                    INTFG,CWSLOC)
C
C     + + + PURPOSE + + +
C     given a uvquan name, find the corresponding global workspace variable
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       SREC,EREC,INTFG
      CHARACTER*6   UVQUAN
      CHARACTER*15  CWSLOC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UVQUAN - character string uvquan name
C     SREC   - starting record of spec act block
C     EREC   - ending record of spec act block
C     CWSLOC - character string global workspace location
C     INTFG  - integer value flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,KEY,IFOUND
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETUCI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IFOUND= 0
C
C     look for this name in uvquan records
      KEY = SREC
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
      INTFG = 0
 10   CONTINUE
        IF (UCIBF(3:8).EQ.'UVQUAN') THEN
C         found a uvquan record
          IF (UCIBF(10:15).EQ.UVQUAN) THEN
C           save workspace location
            CWSLOC= UCIBF(28:42)
            IFOUND= 1
            IF (UCIBF(43:43).EQ.'2' .OR. UCIBF(44:44).EQ.'2'
     1          .OR. UCIBF(45:45).EQ.'2') THEN
C             read as integer value
              INTFG = 1
            END IF
          END IF
        END IF
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
      IF (KEY.NE.EREC .AND. IFOUND.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WS2UVN
     I                   (CWSLOC,SREC,EREC,
     O                    UVNAME)
C
C     + + + PURPOSE + + +
C     given a global workspace variable, find the corresponding uvname
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       SREC,EREC
      CHARACTER*6   UVNAME
      CHARACTER*15  CWSLOC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UVNAME - character string uvname
C     SREC   - starting record of spec act block
C     EREC   - ending record of spec act block
C     CWSLOC - character string global workspace location
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,KEY,IFOUND
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETUCI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IFOUND= 0
C
C     look for this location in uvname records
      KEY = SREC
      CALL GETUCI (I0,
     M             KEY,
     O             UCIBF)
 10   CONTINUE
        IF (UCIBF(3:8).EQ.'UVNAME') THEN
C         found a uvname record
          IF (CWSLOC.EQ.UCIBF(21:35)) THEN
C           save uvname
            UVNAME= UCIBF(11:16)
            IFOUND= 1
          END IF
        END IF
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
      IF (KEY.NE.EREC .AND. IFOUND.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDATGT
     O                   (SDATE,EDATE)
C
C     + + + PURPOSE + + +
C     get dates from uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SDATE(5),EDATE(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATE  - starting date of simulation
C     EDATE  - ending date of simulation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,ITYP,KEYST,KEYND,KEY
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL  GETSE,GETUCI
C
C     + + + INPUT FORMATS + + +
 1030 FORMAT (10X,I8,4(1X,I2),5X,I8,4(1X,I2))
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
C     find table in uci (type 2 from hspf.seq, grp 22, col 3
      ITYP= 2
      CALL GETSE(ITYP,I1,
     O           KEYST,KEYND)
      IF (KEYST.NE.0) THEN
C       found global block
        KEY = KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
C       get dates and times
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        READ (UCIBF,1030) SDATE,EDATE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STSPGG
     I                   (UVQFIL,INITFG,
     O                    GNAME,GDESC,RETCOD)
C
C     + + + PURPOSE + + +
C     get group name and description for special actions translator
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       INITFG,UVQFIL,RETCOD
      CHARACTER*6   GNAME
      CHARACTER*64  GDESC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UVQFIL - file unit number for opened input uvquan file
C     RETCOD - return code  1-no spec acts, 2-end of file, 3-err reading file
C     INITFG - init flag 1-first time in
C     GNAME  - group name
C     GDESC  - group description
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I1,SREC,EREC,ID,ILEN,I120
      CHARACTER*1    CSTR1(120)
      CHARACTER*120  CSTRIN
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETSE,CVARAR,LENSTR
C
C     + + + INPUT FORMATS + + +
 1005 FORMAT (A120)
 1015 FORMAT (A6,4X,A64)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I120 = 120
C
      RETCOD = 0
      GNAME  = ' '
      GDESC  = ' '
      IF (INITFG.EQ.1) THEN
C       check to see if any special actions records exist
        ID = 9
        CALL GETSE (ID,I1,
     O              SREC,EREC)
      ELSE
        SREC = 1
      END IF
      IF (SREC.EQ.0) THEN
C       no special actions records exist in this uci file
        RETCOD = 1
      ELSE
C       have some special actions records
 10     CONTINUE
C         read next line of uvquan file
          READ(UVQFIL,1005,ERR=91,END=98) CSTRIN
          CALL CVARAR (I120,CSTRIN,I120,CSTR1)
          ILEN = LENSTR(I120,CSTR1)
        IF (ILEN.GT.0) GO TO 10
C       get here on a blank line
 20     CONTINUE
C         read next line of uvquan file
          READ(UVQFIL,1005,ERR=91,END=98) CSTRIN
          CALL CVARAR (I120,CSTRIN,I120,CSTR1)
          ILEN = LENSTR(I120,CSTR1)
        IF (ILEN.EQ.0) GO TO 20
C       get here on next non-blank line
        READ(CSTRIN,1015,ERR=91,END=98) GNAME,GDESC
      END IF
C
      GO TO 99
 91   CONTINUE
C       error reading file
        RETCOD = 3
        GO TO 99
 98   CONTINUE
C       end of file
        RETCOD = 2
 99   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   STSPGU
     I                   (UVQFIL,
     O                    UVQUAN,CGROUP,UVDESC,UVNAME,
     O                    UMIN,UMAX,UDEF,INTFG,RETCOD)
C
C     + + + PURPOSE + + +
C     get uvquan name and description for special actions translator
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       UVQFIL,RETCOD,INTFG
      REAL          UMIN,UMAX,UDEF
      CHARACTER*6   UVQUAN,UVNAME,CGROUP
      CHARACTER*64  UVDESC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UVQFIL - file unit number for opened input uvquan file
C     INTFG  - integer value flag 1-integer 0-real
C     RETCOD - return code  1-no spec acts, 2-end of file or blank,
C                           3-err reading file
C     UMIN   - min value for this uvquan
C     UMAX   - max value for this uvquan
C     UDEF   - default value for this uvquan
C     UVQUAN - uvquan name
C     CGROUP - group name
C     UVNAME - uvquan workspace variable name
C     UVDESC - description of uvquan
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I1,SREC,EREC,ID,ILEN,I120
      CHARACTER*1    CSTR1(120)
      CHARACTER*15   CWLOC
      CHARACTER*120  CSTRIN
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETSE,CVARAR,LENSTR
      EXTERNAL   UVQ2WS,WS2UVN
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A6,4X,A64,6X,3G10.5,A6)
 1005 FORMAT (A120)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I120 = 120
C
      RETCOD = 0
      UVQUAN = ' '
      UVDESC = ' '
      INTFG  = 0
C     check to see if any special actions records exist
      ID = 9
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.EQ.0) THEN
C       no special actions records exist in this uci file
        RETCOD = 1
      ELSE
C       have some special actions records
 10     CONTINUE
          UVNAME= ' '
C         read next line of uvquan file
          READ(UVQFIL,1005,ERR=91,END=98) CSTRIN
          CALL CVARAR (I120,CSTRIN,I120,CSTR1)
          ILEN = LENSTR(I120,CSTR1)
          IF (ILEN.GT.0) THEN
C           something on this line
            READ(CSTRIN,1000,ERR=91,END=98) UVQUAN,UVDESC,UMIN,UMAX,
     1                                      UDEF,CGROUP
            CWLOC = ' '
C           find global workspace location which corresponds to this uvquan
            CALL UVQ2WS (UVQUAN,SREC,EREC,
     O                   INTFG,CWLOC)
            IF (CWLOC.NE.'               ') THEN
C             found global workspace location, find corresponding uvname
              CALL WS2UVN (CWLOC,SREC,EREC,
     O                     UVNAME)
            END IF
          ELSE
C           blank line
            RETCOD = 2
          END IF
        IF (RETCOD.EQ.0 .AND. UVNAME.EQ.'      ') GO TO 10
      END IF
C
      GO TO 99
 91   CONTINUE
C       error reading file
        RETCOD = 3
        GO TO 99
 98   CONTINUE
C       end of file
        RETCOD = 2
 99   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   STSPFN
     I                   (UVNAME,INTFG,
     M                    AKEY,
     O                    IDAT,AVAL,CAC,ACOND)
C
C     + + + PURPOSE + + +
C     find next use of this uvquan within the special actions block
C
C     + + + DUMMY ARGUMENTS + + +
C     UVNAME - uvname being searched for
C     INTFG  - integer value flag
C     AKEY   - current uci key, -1 if at beginning of spec acts,
C              0 if not found
C     IDAT   - dates for this action, 0 if undated
C     AVAL   - value for this special action
C     CAC    - action code for this special action
C     ACOND  - conditional string if applicable
C
C     + + + ARGUMENT DEFINITIONS + + +
      INTEGER       SREC,EREC,INTFG,AKEY,IDAT(5)
      REAL          AVAL
      CHARACTER*1   CAC(3)
      CHARACTER*6   UVNAME
      CHARACTER*64  ACOND
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,I,ELINE,I1,ID,ITEMP,STWORD,KEY
      REAL          RTEMP
      CHARACTER*64  CTEMP
      CHARACTER*80  UCIBF
C
C     + + + SAVES + + +
      INTEGER       CONCNT
      CHARACTER*64  CCOND(20)
      SAVE          CONCNT,CCOND

C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT
C
C     + + + FUNCTIONS + + +
      INTEGER     CKNBLV
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETUCI,CKNBLV,GETSE
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (60X,G10.5)
 1012 FORMAT (60X,I10)
 1030 FORMAT (20X,I4,4(1X,I2))
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      ID  = 9
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (AKEY.EQ.-1) THEN
C       initializing
        KEY   = SREC
        CONCNT= 0
      ELSE
        KEY = AKEY
      END IF
      AKEY = 0
      IF (SREC.GT.0 .AND. KEY.NE.EREC) THEN
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
C       find first word in case line is a free-form conditional line
        I= 80
        STWORD= CKNBLV (I,UCIBF)
        IF (STWORD .LT. 1) THEN
C         set dummy stword on blank line
          STWORD= 1
        END IF
 20     CONTINUE
          IF (UCIBF(STWORD:STWORD+2) .EQ. 'IF ') THEN
C           opening of conditional
            CONCNT = CONCNT + 1
            ELINE  = STWORD+63
            IF (ELINE.GT.80) THEN
C             cant run past 80 characters
              ELINE = 80
            END IF
C           save text of conditional
            CCOND(CONCNT) = UCIBF(STWORD:ELINE)
          ELSE IF (UCIBF(STWORD:STWORD+5) .EQ. 'END IF') THEN
C           closing of conditional
            CONCNT = CONCNT - 1
          ELSE IF (UCIBF(STWORD:STWORD+6) .EQ. 'ELSE IF') THEN
C           else if term in conditional, save text
            ELINE  = STWORD+63
            IF (ELINE.GT.80) THEN
C             cant run past 80 characters
              ELINE = 80
            END IF
            CCOND(CONCNT) = UCIBF(STWORD:ELINE)
          ELSE IF (UCIBF(STWORD:STWORD+3) .EQ. 'ELSE') THEN
C           else term in conditional, save text of previous
            CTEMP = CCOND(CONCNT)
            CCOND(CONCNT) = '.NOT. ' // CTEMP
          ELSE IF ((UCIBF(3:8) .EQ. 'UVQUAN').OR.
     1             (UCIBF(3:8) .EQ. 'DISTRB').OR.
     1             (UCIBF(3:8) .EQ. 'UVNAME')) THEN
C           ignore this line
          ELSE IF (UVNAME.EQ.UCIBF(43:48)) THEN
C           action line, see if record contains a real value
            IF (INTFG.EQ.0) THEN
C             read real number
              READ (UCIBF,1010,ERR=89) RTEMP
            ELSE
C             read integer
              READ (UCIBF,1012,ERR=89) ITEMP
              RTEMP = FLOAT(ITEMP)
            END IF
C           return info
            AKEY = KEY
            AVAL = RTEMP
            IF (UCIBF(58:58).EQ.' ' .AND.
     1          UCIBF(59:59).EQ.' ') THEN
C             need to move up action code two places
              CAC(1) = UCIBF(60:60)
              CAC(2) = ' '
              CAC(3) = ' '
            ELSE IF (UCIBF(58:58).EQ.' ') THEN
C             need to move up action code one place
              CAC(1) = UCIBF(59:59)
              CAC(2) = UCIBF(60:60)
              CAC(3) = ' '
            ELSE
C             just store action codes
              CAC(1) = UCIBF(58:58)
              CAC(2) = UCIBF(59:59)
              CAC(3) = UCIBF(60:60)
            END IF
C           see if this special action is dated
            READ (UCIBF,1030,ERR=93) IDAT(1),IDAT(2),IDAT(3),
     1                               IDAT(4),IDAT(5)
            GO TO 94
 93         CONTINUE
              IDAT(1) = 0
              IDAT(2) = 0
              IDAT(3) = 0
              IDAT(4) = 0
              IDAT(5) = 0
 94         CONTINUE
            IF (CONCNT.EQ.0) THEN
C             not in a conditional
              ACOND  = ' '
            ELSE
C             in a conditional
              ACOND  = CCOND(CONCNT)
            END IF
          END IF
 89       CONTINUE
          IF (AKEY.EQ.0) THEN
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
C           find first word in case line is a free-form conditional line
            I= 80
            STWORD= CKNBLV (I,UCIBF)
            IF (STWORD .LT. 1) THEN
C             set dummy stword on blank line
              STWORD= 1
            END IF
          END IF
        IF (KEY.NE.EREC .AND. AKEY.EQ.0) GO TO 20
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STSPPN
     I                   (AKEY,AVAL,INTFG,
     I                    IDATES,AC)
C
C     + + + PURPOSE + + +
C     put uvquan name line to special actions block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       AKEY,INTFG,IDATES(5)
      REAL          AVAL
      CHARACTER*3   AC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AKEY   - uci buffer key for this record
C     AVAL   - value for this special action
C     INTFG  - integer value flag
C     AC     - action code for this special action
C     IDATES - dates for this special action
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I3
      CHARACTER*1  CAC(3)
      CHARACTER*80 UCIBF
C
C     + + + INTRINSICS + + +
      INTRINSIC    INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETUCI,REPUCI,PREUCI,CVARAR
C
C     + + + OUTPUT FORMATS + + +
 2020 FORMAT (G10.5)
 2022 FORMAT (I10)
 2030 FORMAT (I4,4(1X,I2))
 2040 FORMAT ('                ')
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I3 = 3
C
      CALL CVARAR (I3,AC,I3,CAC)
C
      CALL PREUCI (AKEY)
      CALL GETUCI (I0,
     M             AKEY,
     O             UCIBF)
      IF (CAC(2).EQ.' '.AND.CAC(3).EQ.' ') THEN
C       need to move up action code two places
        UCIBF(58:58) = ' '
        UCIBF(59:59) = ' '
        UCIBF(60:60) = CAC(1)
      ELSE IF (CAC(3).EQ.' ') THEN
C       need to move up action code one place
        UCIBF(58:58) = ' '
        UCIBF(59:59) = CAC(1)
        UCIBF(60:60) = CAC(2)
      ELSE
C       just store action codes
        UCIBF(58:58) = CAC(1)
        UCIBF(59:59) = CAC(2)
        UCIBF(60:60) = CAC(3)
      END IF
      IF (INTFG.EQ.0) THEN
C       real number
        WRITE (UCIBF(61:70),2020) AVAL
      ELSE
C       integer
        WRITE (UCIBF(61:70),2022) INT(AVAL)
      END IF
      IF (IDATES(1).EQ.0) THEN
C       no, undated special action
        WRITE (UCIBF(21:36),2040)
      ELSE
C       dated special action
        WRITE (UCIBF(21:36),2030) IDATES(1),IDATES(2),IDATES(3),
     1                            IDATES(4),IDATES(5)
      END IF
      CALL REPUCI (AKEY,UCIBF)
C
      RETURN
      END

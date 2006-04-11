C     qtfile.f 2.1 9/4/91
C
C
C
      SUBROUTINE   QFOPEN
     I                   (MFILE,SCLU,SGRP,
     O                    DAFL,RETCOD)
C
C     + + + PURPOSE + + +
C     This routine reads file specifications from the ANNIE message file
C     and opens the files.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MFILE,SCLU,SGRP,DAFL,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MFILE  - Fortran unit number of message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     DAFL   - Fortran unit number for the file opened
C     RETCOD - return code, 0 if file opened, else non-zero
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,PRTFLG
      CHARACTER*1 FLNEXT(64),BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC, QFOPFN
C
C     + + + END SPECIFICATIONS + + +
C
C     set print flag to try to fix error in QFOPFN
      PRTFLG= 2
C
C     initialize external file name to blank
      I     = 64
      BLNK  = ' '
      CALL ZIPC (I,BLNK,FLNEXT)
C
      CALL QFOPFN (MFILE,SCLU,SGRP,FLNEXT,PRTFLG,
     O             DAFL,RETCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QFOPFN
     I                   (MFILE,SCLU,SGRP,FLNEXT,PRTFLG,
     O                    DAFL,RETCOD)
C
C     + + + PURPOSE + + +
C     This routine reads file specifications from the ANNIE message file
C     and opens the files.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MFILE,SCLU,SGRP,PRTFLG,DAFL,RETCOD
      CHARACTER*1 FLNEXT(64)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MFILE  - Fortran unit number of message file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     FLNEXT - name of file to open (original QFOPEN initializes this
C              to blank, user may supply a name from an external routine)
C     PRTFLG - print flag, 0 - no print, calling routine prints message,
C                          1 - print message, dont try to fix error,
C                          2 - print message and try to fix error
C     DAFL   - Fortran unit number for the file opened
C     RETCOD - return code, 0 if file opened, else non-zero
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + +   LOCAL VARIABLES   + + +
      INTEGER      I, J, IRECL, FSCLU, LEN, IFLG, QFLG, L64, IRET, 
     $             REDO, RESP, LSGRP, FLNLEN, TRECL, PRVFLG, NAMLEN
      INTEGER*4    IOS
      INTEGER*4    I1, I64
      CHARACTER*64 FLNAME
      CHARACTER*11 FMTD
      CHARACTER*10 ACCD
      CHARACTER*7  STATD,STATF
      CHARACTER*1  FLN1(64), BLNK, WLDCHR(1)
      LOGICAL      GOTNAM
C
C     + + + SAVES + + +
      INTEGER      EFLG,IOERR(3)
      SAVE         EFLG,IOERR
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR, STRFND, ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, ZCMDON, PRNTXT, QRESP, QTSTR, GETFUN, PRNTXI
      EXTERNAL   CARVAR, CHRCHR, IOESET, ZIPC, ZMNSST
      EXTERNAL   RECSET, STRFND, WMSGTF, ZSTCMA, ZGTRET, ZMNWLD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA EFLG,L64,I1,I64/-999,64,1,64/
C
C     + + + END SPECIFICATIONS + + +
C
      FSCLU= 2
      REDO = 0
      BLNK = ' '
C
C     get unit number
      IFLG = 1
      CALL GETFUN (IFLG, DAFL)
C
      IF (DAFL .LT. 0) THEN
C       no more unit numbers available
        LSGRP = 10
        CALL PRNTXT (MFILE, FSCLU, LSGRP)
        DAFL = 0
        RETCOD = -1
      ELSE
        CALL WMSGTF (MFILE,SCLU,SGRP,
     O               QFLG,FLNAME,STATF,ACCD,FMTD,TRECL)
C
        STATD= STATF
C       adjust record length if necessary
        CALL RECSET (TRECL,
     O               IRECL)
C
C       copy file name from external routine to local file name
        FLNLEN= LENSTR(L64,FLNEXT)
        IF (FLNLEN.GT.0) THEN
          CALL CHRCHR (L64,FLNEXT,FLN1)
          CALL CARVAR (L64,FLN1,L64,FLNAME)
          NAMLEN = FLNLEN
        ELSE
C         case where name comes from message file
          CALL ZIPC (L64,BLNK,FLN1)
          NAMLEN = 0
        END IF
C
 10     CONTINUE
C         loop back here if need to try another file name
          RETCOD= 0
          IF (QFLG.EQ.1 .AND. REDO.NE.1 .AND. FLNLEN.EQ.0) THEN
C           write question and read response
            LEN= 64
            CALL ZIPC (LEN,BLNK,FLN1)
            I= 4
            IF (ZCMDON(I).EQ.0) THEN
C             make 'prev' available
              J= 1
              CALL ZSTCMA (I,J)
              PRVFLG= 0
            ELSE
C             'Prev' already available, set flag to not turn it off
              PRVFLG= 1
            END IF
            CALL QTSTR (MFILE,SCLU,SGRP,LEN,FLN1)
            NAMLEN = LENSTR (L64, FLN1)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user selected 'next', update file name
              CALL CARVAR (L64,FLN1,L64,FLNAME)
            ELSE
C             user selected 'prev', set unit number and RETCOD
              DAFL  = 0
              RETCOD= -1
            END IF
            IF (PRVFLG.EQ.0) THEN
C             'prev' unavailable at start, turn it off at end
              I= 4
              J= 0
              CALL ZSTCMA (I,J)
            END IF
          END IF
          REDO   = 0
C
          IF (RETCOD.EQ.0) THEN
            IF (NAMLEN .EQ. 0) THEN
C             file name is blanks
              RETCOD = -1
              IF (PRTFLG .NE. 0) THEN
C               write message
                LSGRP= 28
                CALL PRNTXT (MESSFL,FSCLU,LSGRP)
              END IF
              IF (PRTFLG .EQ. 2) THEN
C               do you want to use another file? (1=YES,2=NO)
                LSGRP= 14
                CALL QRESP (MESSFL,FSCLU,LSGRP,RESP)
                IF (RESP.EQ.1) REDO = 2
                STATD= STATF
              END IF
            ELSE
C             check name entered by user
C             wildcard matching capability added in
              GOTNAM = .TRUE.
              WLDCHR(1) = '*'
              IF (STRFND(I64,FLN1,I1,WLDCHR) .NE. 0) THEN
C               if (FLNAME .NE. 'ANNIE.LOG') then
C               asterik in filename means a wildcard was entered
                CALL ZMNWLD (FLN1,GOTNAM)
C               sooner or later, we need a regular file name, so loop back
                REDO = 2
C               but try to keep the menu from being cleared of all that hard work
                CALL ZMNSST
              END IF
C             try to open file
              IF (ACCD.NE.'SEQUENTIAL' .AND. GOTNAM) THEN
C               open for direct access files
                IF (STATD .EQ. 'SCRATCH') THEN
                  OPEN (UNIT=DAFL,ACCESS=ACCD,STATUS=STATD,
     $                  RECL=IRECL,FORM=FMTD,IOSTAT=IOS)
                ELSE
                  OPEN (UNIT=DAFL,FILE=FLNAME,ACCESS=ACCD,STATUS=STATD,
     $                  RECL=IRECL,FORM=FMTD,IOSTAT=IOS)
                END IF
              ELSE IF (GOTNAM) THEN
C               open for sequential files
                IF (STATD .EQ. 'SCRATCH') THEN
                  OPEN (UNIT=DAFL,ACCESS=ACCD,STATUS=STATD,
     $                  FORM=FMTD,IOSTAT=IOS)
                ELSE
                  OPEN (UNIT=DAFL,FILE=FLNAME,ACCESS=ACCD,STATUS=STATD,
     $                  FORM=FMTD,IOSTAT=IOS)
                END IF
              END IF
C
              IF (IOS .NE. 0) THEN
C               error occurred during attempt to open file
                RETCOD = IOS
                IF (EFLG .EQ. -999) THEN
C                 set error code numbers for type of machine
                  CALL IOESET (IOS,IOERR)
                  EFLG = 0
                END IF
C
C               if external file name passed in (FLNLEN>0)
C               don't do QRESPs due to possible recursion
                IF (IOS.EQ.IOERR(1) .AND. PRTFLG.GT.0) THEN
C                 file exists.
                  IF (PRTFLG.EQ.1) THEN
C                   just write message
                    LSGRP= 58
                    CALL PRNTXT (MESSFL,FSCLU,LSGRP)
                  ELSE
C                   Do you want to write over it? (1=y,2=n)
                    LSGRP = 17
                    CALL QRESP (MESSFL,FSCLU,LSGRP,RESP)
                    IF (RESP.NE.2) THEN
                      STATD = 'OLD    '
                      REDO  = 1
                    ELSE
C                     Do you want to use another file? (1=YES,2=NO)
                      LSGRP = 14
                      CALL QRESP (MESSFL,FSCLU,LSGRP,RESP)
                      IF (RESP.EQ.1) REDO = 2
                    END IF
                  END IF
C
                ELSE IF (IOS.EQ.IOERR(2) .AND. PRTFLG.GT.0) THEN
C                 File does not exist.
                  IF (PRTFLG.EQ.1) THEN
C                   just write message
                    LSGRP= 59
                    CALL PRNTXT (MESSFL,FSCLU,LSGRP)
                  ELSE
C                   Do you want to try again? (1=Y,2=N)
                    LSGRP = 15
                    CALL QRESP (MESSFL,FSCLU,LSGRP,RESP)
                    IF (RESP.EQ.1) REDO = 2
                  END IF
C
                ELSE IF (IOS.EQ.IOERR(3) .AND. PRTFLG.GT.0) THEN
C                 someone else using file.
                  LSGRP = 4
                  CALL PRNTXT (MESSFL,FSCLU,LSGRP)
                  IF (PRTFLG.EQ.2) THEN
C                   do you want to use another file? ( 1=YES,2=NO )
                    LSGRP= 14
                    CALL QRESP (MESSFL,FSCLU,LSGRP,RESP)
                    IF (RESP.EQ.1) REDO = 2
                    STATD= STATF
                  END IF
C
                ELSE IF (PRTFLG.GT.0) THEN
C                 no other message written so dump some info.
                  LSGRP = 51
                  CALL PRNTXI (MESSFL,FSCLU,LSGRP,IOS)
                END IF
C             end if (ios .ne. 0)
              END IF
C           end if (namlen .eq. 0)
            END IF
C         end if (retcod .eq. 0)
          END IF
        IF (REDO .NE. 0) GO TO 10
C
        IF (IOS.NE.0 .OR. NAMLEN.EQ.0) THEN
C         free up Fortran unit number not used
          IFLG = 2
          CALL GETFUN (IFLG, DAFL)
          DAFL = 0
        END IF
C
C     end if (dafl .lt. 0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZMNWLD
     M                   (PAT,PICKED)
C
C     + + + PURPOSE + + +
C     This routine puts the names of files matching the wildcard
C     specification "PAT" into the menu buffer ZMNTXT for display.
C
C     The variable PICKED is set to .TRUE. to indicate that a file
C     name has been picked from the display, and is being returned
C     in PAT.  PICKED is set to .FALSE. if no file was chosen from
C     those displayed, and in this case, PAT is undefined.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64  PAT
      LOGICAL       PICKED
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PAT    - Wildcard pattern to be matched
C     PICKED - Was file choosen from the list presented (True) or not (False)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2 ATTR, FTIME, FDATE, ERR, I, OLINES, POS
      INTEGER*4 FSIZE
      INTEGER*4 I78
      LOGICAL   MORE, ROOM
      INTEGER*4 LENGTH, REMAIN, FNSPAC, MAXLEN, FILECT
      INTEGER*2 TABCNT, TAB(5), ITAB
      INTEGER*2 LONGNO
C
C     TABCNT = the number of columns in the file name display
C     TAB    = an array of the starting positions of display columns
C     FILECT = the total number of files matched and displayed.
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL GT1ST, GTNXT, LENSTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TABCNT,TAB /5,4,19,34,49,64/
C 4 columns      DATA TABCNT,TAB /4,2,21,40,59/
C 3 columns      DATA TABCNT,TAB /3,2,27,54/
C
C     + + + END SPECIFICATIONS + + +
C
      I78    = 78
      ITAB   = 1
      POS    = TAB(ITAB)
      REMAIN = 80 - POS
      MAXLEN = 64
      ATTR   = 3
      PICKED = .FALSE.
      FILECT = 0
      LONGNO = 1
      FNSPAC = TAB(2) - TAB(1) - 1
C
      IF (ZWLDFL .EQ. 0) THEN
C     flag says ZWLDMN wasn't called (until now) since screen was cleared
        OLINES = 0
        ZWLDFL = 1
      ELSE
        OLINES = ZWLDLI
      END IF
      ZWLDLI = ZMNNLI
C     ZWLDLI+2 must be .le. the dimension of ZMNTXT initially for ROOM eq true
      ROOM = .TRUE.
      IF (ZWLDLI+2 .GT. MXSCBF) ROOM = .FALSE.
      IF (ROOM) THEN
        CALL GT1ST (PAT, ATTR, FTIME, FDATE, FSIZE, ERR)
        IF (ERR .NE. 0) THEN
C         error code is much more explicit under DOS
          ZWLDLI = ZWLDLI + 2
          ZMNTXT(ZWLDLI) = '   No matching files found!'
          ZMNLEN(ZWLDLI) = LENSTR (I78,ZMNTXT(ZWLDLI))
          MORE = .FALSE.
        ELSE
          FILECT = FILECT + 1
          ZWLDLI = ZWLDLI + 2
          LENGTH = LENSTR(MAXLEN,PAT)
          ZMNTXT(ZWLDLI)(POS:) = PAT
          IF (LENGTH .GE. FNSPAC) THEN
            LONGNO = 2
          END IF
          DO 30 I = ITAB,TABCNT
            IF (POS+LENGTH .GE. TAB(ITAB)) THEN
              ITAB = ITAB + 1
            ELSE
              GO TO 31
            END IF
 30       CONTINUE
 31       CONTINUE
          MORE = .TRUE.
        END IF
      END IF
C
C     ROOM tells if there is enough room in the ZMNTXT buffer to put another
C     file name, MORE tells whether there is another file name to insert.
C     Read as "while (room and more) do"
 100  IF (.NOT. ROOM .OR. .NOT. MORE) GO TO 199
        CALL GTNXT (PAT, ATTR, FTIME, FDATE, FSIZE, ERR)
        IF (ERR .NE. 0) THEN
          ZMNLEN(ZWLDLI) = LENSTR (I78,ZMNTXT(ZWLDLI))
          MORE = .FALSE.
        ELSE
C         we have another file name to display
          IF (ITAB .GT. TABCNT) THEN
            ZMNLEN(ZWLDLI) = LENSTR (I78,ZMNTXT(ZWLDLI))
            IF (ZWLDLI .LT. MXSCBF) THEN
              ZWLDLI = ZWLDLI + 1
              ITAB   = 1
              LONGNO = 1
            ELSE
              ROOM = .FALSE.
            END IF
          END IF
          POS = TAB(ITAB)
          REMAIN = 80 - POS
          LENGTH = LENSTR (MAXLEN,PAT)
          IF (LENGTH .GT. REMAIN) THEN
            ZMNLEN(ZWLDLI) = LENSTR (I78,ZMNTXT(ZWLDLI))
            IF (ZWLDLI .LT. MXSCBF) THEN
              ZWLDLI = ZWLDLI + 1
              ITAB   = 1
              LONGNO = 1
              POS    = TAB(ITAB)
              REMAIN = 80 - POS
            ELSE
              ROOM = .FALSE.
            END IF
          END IF
          IF (ROOM) THEN
            ZMNTXT(ZWLDLI)(POS:) = PAT
            FILECT = FILECT + 1
            IF (LENGTH .GE. FNSPAC) THEN
              LONGNO = LONGNO + 1
            END IF
            DO 40 I = ITAB,TABCNT
              IF (POS+LENGTH .GE. TAB(ITAB)) THEN
                ITAB = ITAB + 1
              ELSE
                GO TO 41
              END IF
 40         CONTINUE
 41         CONTINUE
          END IF
        END IF
C
      GO TO 100
C
 199  CONTINUE
C
C     this loop clears junk from later lines that are no longer in use
      IF (OLINES .GT. ZWLDLI) THEN
        DO 200 I= ZWLDLI+1, OLINES
          ZMNTXT(I)= ' '
          ZMNLEN(I)= 0
 200    CONTINUE
      END IF
C
C     we've got to lie about ZMNNLI for a second, so that the text from the
C     SEQ file is placed as before, and not after the previously placed text.
      ZMNNLI = 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   QFCLOS
     I                   (FIL,
     M                    DELFG)
C
C     + + + PURPOSE + + +
C     This routine closes a file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FIL,DELFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FIL    - Fortran unit number of file to be closed
C     DELFG  - 0 to keep the file, 1 to delete the file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    FLG
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETFUN
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DELFG.EQ.0) CLOSE (FIL,ERR = 10)
      IF (DELFG.EQ.1) CLOSE (FIL,STATUS = 'DELETE',ERR = 10)
      FLG = 2
      CALL GETFUN (FLG,FIL)
      GO TO 20
C
 10   CONTINUE
        DELFG = 2
C
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   QFSTAT
     I                   (FUNIT,
     O                    FNAME)
C
C     + + + PURPOSE + + +
C     routine to return a file name if the file is open
C     or blanks if it is not open
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FUNIT
      CHARACTER*1 FNAME(64)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FUNIT - unit number of file being inquired
C     FNAME - name of file being inquired
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I
      CHARACTER*64 FLNAME
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (64A1)
C
C     + + + END SPECIFICATIONS + + +
C
      FLNAME= ' '
      INQUIRE (UNIT=FUNIT,NAME=FLNAME,ERR=10)
 10   CONTINUE
      READ (FLNAME,1000) (FNAME(I),I=1,64)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETFUN
     I                   (OORC,
     M                    IUN)
C
C     + + + PURPOSE + + +
C     This routine gets a Fortran unit number that is not being used
C     or frees up the Fortran unit number for a file just closed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OORC, IUN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OORC   - 1= open file, 2= close file
C     IUN    - Fortran unit number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IND,ICNT,IFLG
C
C     + + + SAVES + + +
      INTEGER    IBEG,IDX(20)
      SAVE       IBEG,IDX
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IDX/20*0/,  IBEG/-999/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IBEG .EQ. -999) THEN
C       first time through, get starting unit number
        IND = 14
        CALL ANPRGT (IND,IBEG)
      END IF
C
      IF (OORC .EQ. 1) THEN
C       find next unit number for OPEN
        ICNT = 0
        IFLG = 0
 10     CONTINUE
          ICNT = ICNT + 1
          IF (IDX(ICNT) .EQ. 0) IFLG = 1
        IF (ICNT .LT. 20 .AND. IFLG .EQ. 0) GO TO 10
C
        IF (IFLG .EQ. 0) THEN
C         no more available, max reached
          IUN = -99
        ELSE
C         all ok
          IUN = IBEG + ICNT
          IDX(ICNT) = 1
        END IF
C
      ELSE IF (OORC .EQ. 2) THEN
C       free up unit number
        ICNT = IUN - IBEG
        IF (ICNT .LE. 20 .AND. ICNT .GE. 1) THEN
          IDX(ICNT) = 0
        END IF
C
      ELSE
C       bad code flag
        IUN = -99
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IOESET
     M                   (IOS,
     O                    IOERR)
C
C
C
C     + + + PURPOSE + + +
C     Sets I/O error values for type of machine being used.  If computer
C     type is PC, IOS converted as necessary for Lahey compiler.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IOS, IOERR(3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOS    - IOSTAT error from OPEN statement
C     IOERR  - (1)- opening new file but file exists
C              (2)- opening old file but it does not exist
C              (3)- file in use
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    PRMIND,CMPTYP
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT
C
C     + + + END SPECIFICATIONS + + +
C
C     get computer type
      PRMIND= 1
      CALL ANPRGT (PRMIND,CMPTYP)
C
      IF (CMPTYP.EQ.1) THEN
C       pc--Lahey compiler
        IOERR(1)= 70
        IOERR(2)= 71
        IOERR(3)= 52
C       convert Lahey IOSET 16-bit machine words to base 10
        IOS = MOD (IOS, 256)
      ELSE IF (CMPTYP.EQ.2) THEN
C       prime
        IOERR(1)= 18
        IOERR(2)= 15
        IOERR(3)= 5
      ELSE IF (CMPTYP.EQ.3) THEN
C       vax
        IOERR(1)= 30
        IOERR(2)= 29
C       no file sharing errors will occur on vax
        IOERR(3)= 0
      ELSE IF (CMPTYP.EQ.4) THEN
C       sun
        IOERR(1)= 117
        IOERR(2)= 118
C       no file sharing errors will occur on sun
        IOERR(3)= 0
      ELSE IF (CMPTYP.EQ.5) THEN
C       dg AViiON--Green Hills compiler
        IOERR(1)= 204
        IOERR(2)= 2
C       no file sharing errors will occur on dg
        IOERR(3)= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RECSET
     I                   (IRECL,
     O                    ORECL)
C
C     + + + PURPOSE + + +
C     determine the record length for an OPEN statement depending
C     on the computer type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRECL,ORECL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IRECL - input record length
C     ORECL - output record length
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    PRMIND,CMPTYP,RECTYP
      SAVE       CMPTYP,RECTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CMPTYP,RECTYP/0,0/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RECTYP.EQ.0) THEN
C       determine record type
        PRMIND= 15
        CALL ANPRGT (PRMIND,RECTYP)
      END IF
C
      IF (RECTYP.EQ.4) THEN
        IF (CMPTYP.EQ.0) THEN
C         determine computer type
          PRMIND= 1
          CALL ANPRGT (PRMIND,CMPTYP)
        END IF
      END IF
C     set RECTYP depending on computer type or RECTYP
      IF (CMPTYP.EQ.1 .OR. RECTYP.EQ.3) THEN
C       pc or byte
        ORECL= 2* IRECL
      ELSE IF (CMPTYP.EQ.2 .OR. RECTYP.EQ.2) THEN
C       prime or half word
        ORECL= IRECL
      ELSE IF (CMPTYP.EQ.3 .OR. RECTYP.EQ.1) THEN
C       vax or word
        ORECL= IRECL/2
      ELSE
        ORECL= IRECL
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKNAME
     I                   (PATH,FILNAM,RWFLG,
     O                    FUNIT)
C
C     + + + PURPOSE + + +
C     Check a file name for validity, and open the file if valid.
C     Returns 0 for the file unit number if the file is not valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      RWFLG,FUNIT
      CHARACTER*20 PATH
      CHARACTER*64 FILNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PATH   - screen name path
C     FILNAM - file name
C     RWFLG  - read/write flag, 1 - read, 2 - write
C     FUNIT  - file unit number, 0 if file name not valid
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,I2,IOERR,SCLU,SGRP,RESP,MSGFG
      CHARACTER*28 SCNAME
      LOGICAL      FLEXST
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, GETFUN, QRESP, PRNTXT, PRNTXI, ZWNSET, QFCLOS
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I2   = 2
      SCLU = 2
      MSGFG= 0
C
      I= ZLNTXT(PATH)
      SCNAME = PATH(1:I)//' Problem'
C
      IOERR = 0
      IF (FILNAM .EQ. ' ') THEN
C       file name is blanks
        CALL ZWNSET (SCNAME)
        SGRP = 28
        CALL PRNTXT (MESSFL,SCLU,SGRP)
        FUNIT= 0
        IOERR= 1
        MSGFG= 1
      END IF
C
      IF (IOERR .EQ. 0) THEN
C       check if file exists w/ same name as FILNAM
        INQUIRE (FILE=FILNAM, EXIST=FLEXST, IOSTAT=IOERR)
        IF (IOERR .NE. 0) THEN
C         error occurred during INQUIRE
          CALL ZWNSET (SCNAME)
          SGRP = 51
          CALL PRNTXI (MESSFL,SCLU,SGRP,IOERR)
          FUNIT= 0
          MSGFG= 1
        END IF
      END IF
      IF (IOERR .EQ. 0) THEN
C       ok so far, get unit number to work with
        CALL GETFUN (I1,FUNIT)
        IF (FLEXST) THEN
C         file exists w/ same name
          IF (RWFLG .EQ. 1) THEN
C           reading file, open as old
            OPEN (UNIT=FUNIT,FILE=FILNAM,STATUS='OLD',IOSTAT=IOERR)
          ELSE
C           writing to existing file, want to overwrite
            CALL ZWNSET (SCNAME)
            SGRP= 17
            CALL QRESP (MESSFL,SCLU,SGRP,RESP)
            IF (RESP .EQ. 1) THEN
C             user wants to overwrite
              OPEN (UNIT=FUNIT,FILE=FILNAM,IOSTAT=IOERR)
            ELSE
C             user does not want to overwrite, don't open file
              IOERR= 1
              MSGFG= 1
            END IF
          END IF
        ELSE
C         file does not exist
          IF (RWFLG .EQ. 1) THEN
C           trying to open a file for reading, but does not exist
            CALL ZWNSET (SCNAME)
            SGRP = 59
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            IOERR= 2
            MSGFG= 1
          ELSE
C           try to open file for writing
            OPEN (UNIT=FUNIT,FILE=FILNAM,IOSTAT=IOERR)
          END IF
        END IF
        IF (IOERR .NE. 0) THEN
C         problem opening file
          IF (MSGFG .EQ. 0) THEN
C           need a problem message
            CALL ZWNSET (SCNAME)
            SGRP = 51
            CALL PRNTXI (MESSFL,SCLU,SGRP,IOERR)
          END IF
          CALL GETFUN (I2,FUNIT)
          FUNIT= 0
        ELSE IF (RWFLG .EQ. 2) THEN
C         check for write access
          WRITE (FUNIT,*,ERR=10,IOSTAT=IOERR) I1
          BACKSPACE (FUNIT)
          GO TO 20
 10       CONTINUE
C           get here on write error
            CALL ZWNSET (SCNAME)
            SGRP = 27
            CALL PRNTXI (MESSFL,SCLU,SGRP,IOERR)
            CALL QFCLOS (FUNIT,I0)
            FUNIT= 0
 20       CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   QFDPRS
     I                   (ISTR,
     O                    WRKDIR,IFNAME)
C
C     + + + PURPOSE + + +
C     Parse the file name and/or directory from an input string.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*(*) ISTR,WRKDIR,IFNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTR   - input string being parsed
C     WRKDIR - working directory
C     IFNAME - file name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,ILEN,IPRM,CMPTYP
      CHARACTER DIRCHR
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT, ANPRGT
C
C     + + + END SPECIFICATIONS + + +
C
C     init file name and working directory
      IFNAME = ' '
      WRKDIR = ' '
C
C     determine directory indicator based on machine type
      IPRM = 1
      CALL ANPRGT (IPRM,CMPTYP)
      IF (CMPTYP.EQ.1) THEN
C       pc, use \
        DIRCHR= CHAR(92)
      ELSE IF (CMPTYP.EQ.2) THEN
C       prime, use >
        DIRCHR = CHAR(62)
      ELSE IF (CMPTYP.EQ.3) THEN
C       vax, use ]
        DIRCHR = CHAR(93)
      ELSE IF (CMPTYP.GE.4) THEN
C       unix, use /
        DIRCHR = CHAR(47)
      END IF
C
      ILEN = ZLNTXT(ISTR)
      IF (ILEN .GT. 0) THEN
        N = ILEN + 1
 20     CONTINUE        
          N = N - 1
        IF (ISTR(N:N) .NE. DIRCHR .AND. N .GT. 1) GOTO 20
        IF (ISTR(N:N) .EQ. DIRCHR) THEN
C         directory indicator found, set working directory and file name
          WRKDIR = ISTR(1:N)
          IFNAME = ISTR(N+1:)
        ELSE
C         no directory indicator found, just set file name
          IFNAME = ISTR(1:ILEN)
        END IF
      END IF
C
      RETURN
      END

C         Specify Interface to System Routine
C
C     INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]                        M
C    +(STR1[REFERENCE])                                                 M
C     CHARACTER*1 STR1                                                  M
C     END                                                               M
      SUBROUTINE PCDLIN
C
C           SUBROUTINE 'PCDLIN' CHECKS A LINE OF DATA FOR A
C           PREAD COMMAND.
C           THE SUBROUTINE DETERMINES WHICH COMMAND HAS BEEN
C           GIVEN THEN EXECUTES THE COMMAND.  FOR CLARITY THE
C           SUBROUTINE HAS BEEN DIVIDED INTO THE SECTIONS THAT
C           EXECUTE THE VARIOUS COMMANDS.
C
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PNAMES                                                           H
      INCLUDE 'pnames.h'                                                MLlg
CADD C.PMENU                                                            H
      INCLUDE 'pmenu.h'                                                 MLlg
CADD C.PERROR                                                           H
      INCLUDE 'perror.h'                                                MLlg
CADD C.PLINE                                                            H
      INCLUDE 'pline.h'                                                 MLlg
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX, LIFDON
      COMMON /DIE/ DIE
      COMMON /PLSET/ LPSETP
      COMMON /GTLINE/ LBOOT, LFTIME
      LOGICAL LBOOT, LFTIME, LPSETP, DIE
      CHARACTER CMODE*6
C
C   ***
      LOGICAL LEQ, LTMP, LOPEN
      CHARACTER*80 CMESS
C     CHARACTER*18 CCONT2                                               H
      CHARACTER*8 CMD, COPT, CTMP
      CHARACTER*3 CRESP(7),COFF
      CHARACTER*2 CPRINT, CSYMB*4
      CHARACTER*1 CTKEY,CH1,CSQUOT,CDQUOT
      INTEGER IEF(20)
C     INTEGER*2 SYSTEM                                                  M
C     INTEGER system                                                    lg
      EXTERNAL LCOPTS
      LOGICAL LFIRST,LEXIST,LREG, LCOPTS
C
      DATA LFIRST /.TRUE./
      DATA LIFDON /.TRUE./
C
C
      LCMD=.FALSE.
C
      CSQUOT = CHAR(39)
      CDQUOT = CHAR(34)
      COFF(1:3) = CHAR(27)//CHAR(33)//CHAR(56)
 2    FORMAT (A)
C   ***
C   ** IF THIS IS THE FIRST TIME IN THE SUBROUTINE
C   ** DETERMINE THE TYPE OF TERMINAL BEING USED
C   ** AND THE REGISTER.
C   ***
C---- IF (LFIRST) THEN
C      CALL GSTRRG('STM',CCONT2,ICONT2,ISTAT2)                          H
C      IF ( CCONT2(1:2) .EQ. '42' ) CCONT2(1:2) = '41'                  H
       LFIRST = .FALSE.
C---- END IF
C   ***
C ------ If LPAUZ is true we want to disgard the current line
C ------ By treating it like a pread comment and continuing
      IF ( LPAUZ ) THEN
      LPAUZ = .FALSE.
      CLINE = CSPL(1:1)//'* '
      ENDIF
C ------
C ------ CHECK FOR LTEACH LINE, THIS IS DONE WHEN !KBLINE IS USED TO
C ------ READ A LINE FROM THE KEYBOARD THAT SHOULD NOT BE PASSED TO
C ------ THE PROGRAM.  TEACH THE SYMBOL ON THE KBLINE COMMAND THE STRING
C
    3 CONTINUE
C
C ------
      IF( LTEACH ) THEN
      LTEACH = .FALSE.
      CMESS = CLINE
      CLINE = CSPL(1:1)//'TEACH '//CSYMB//' '//CMESS
      CLINSV = CLINE
      ENDIF
C ------
C   ***
C   ** SCAN FOR (!)
C   ***
C ------
C
    5 CONTINUE
C
C ------
      IF (CLINE(1:1).NE.CSPL(1:1)) GO TO 8000
C
      LCMD=.TRUE.
C   ***
C   ***
C   ** LOOK FOR FIRST NON-BLANK STARTING IN COL 2
C   ***
      ILINEP = (NINDX(CLINE(2:),' ') + 1)
C   ***
C   ***
C   ** BRANCH IF LINE BLANK
C   ***
      IF( (ILINEP-1) .LE. 0 ) GO TO 9000
C   ***
C   ***
C   ** CHECK FOR '/' OR '*' AS FIRST NONBLANK CHARACTER AFTER FIRST
C   ** POSITION.  IF THIS IS THE CASE, BRANCH OR RETURN RESPECTIVELY.
C   **            '/' ===>  DENOTES OPCOM COMMAND
C   **            '*' ===>  DENOTES A COMMENT LINE (=IGNORE)
C   ***
      IF (CLINE(ILINEP:ILINEP) .EQ. '/') GOTO 4000
      IF (CLINE(ILINEP:ILINEP) .EQ. '*') GO TO 8000
C   ***
C   ***
C   ** CHECK FOR '=' PRECEEDING COMMAND
C   ***
      LEQ=.FALSE.
      IF (CLINE(ILINEP:ILINEP) .EQ. '=') THEN
         LEQ=.TRUE.
         ILINEP=ILINEP+1
      END IF
      JMXNCH = LEN ( CLINE )
      IL = (JMXNCH-ILINEP+1)
      NF = -20
      CALL FINDLM(CLINE,ILINEP,IL,NF,IBF,ILF,IDT,IPT,ITBL)
      IF (NF.LE.0) GO TO 8000
      IF(ILF(1).LE.0) GO TO 8000
C   ***
C   ***
C   ** LOOK FOR  '-'
C   ***
      LMINUS=.FALSE.
      IF (CLINE(IBF(1):IBF(1)) .EQ. '-') THEN
       LMINUS=.TRUE.
       IBF(1)=IBF(1)+1
       ILF(1)=ILF(1)-1
      END IF
C   ***
      DO 10 I = 1, NF
   10 IEF(I) = IBF(I) + ILF(I) - 1
C   ***
C
C ------ Check for options on command, GTCOPT will blank option area
C
      CALL GTCOPT ( CLINE(IBF(1):IEF(1)), COPT, NCHRS )
      IEF(1) = IEF(1) - NCHRS
      ILF(1) = ILF(1) - NCHRS
C
C   ** CHECK IF VALID COMMAND
C   ***
      CMD = CLINE(IBF(1):IEF(1))
      NC = ILF(1)
      CALL UPCASE(CMD(1:NC))
      NMATCH=0
      DO 101 I = 1,NLIST
         IF ( CMD(1:NC) .EQ. CLIST(I)(1:NC) ) THEN
            IMATCH = I
            NMATCH = NMATCH + 1
         END IF
 101  CONTINUE
      IF (NMATCH .GT. 1 .AND. CMD(1:NC) .NE. 'ELSE' ) THEN
         WRITE (IDSP,*) 'ABBREVIATED COMMAND DOES NOT IDENTIFY'//
     +                      ' A UNIQUE COMMAND'
      ELSE
      NMATCH = 1
      ENDIF
C   ***
C   ***
C   ** BRANCH ON ERROR
C   ***
      IF (NMATCH .NE. 1) GO TO 9000
C   ***
C   ***
C   ** PROCESS COMMAND
C   ** NOTE:  THE '/' COMMAND BYPASSES THIS CODE AND GOES TO 4000
C   **        THE '*' COMMAND BYPASSES THIS CODE AND RETURNS
C   ***
      GO TO (      100, 200, 300, 400, 500, 600, 700, 800, 900,
     +       1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,
     +       2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,
     +       9000,3100,3200,3300,3400,3500,3600 ), IMATCH
C
C      ************************************************************
C      *   M E N U                                                *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE MENU        *
C      *   COMMAND.  FIRST THE NAME OF THE MENU IS DETERMINED     *
C      *   AND THEN THE SUBROUTINE PRDMEN IS CALLED (PREAD MENU). *
C      ************************************************************
C
C
  400 CONTINUE
C
C     CHECK TO SEE IF A VALID TABLET IS AVAILABLE
C
      IF (IMEN.EQ.-1) GO TO 8000
      IF (.NOT.LTABEX) THEN
      WRITE(IDSP,405)
  405 FORMAT(/' ERROR - MENU CAN NOT BE USED WITH INVALID TABLET'/
     . ' RETURN TO JCL AND SET THE  #GIN  REGISTER TO: '/
     . ' 4014  S13F T18F T20F T06I'/)
      GO TO 8000
      ENDIF
C
      IF (LEQ) THEN
       REWIND IMEN
 425   READ(IMEN,2,END=430) CLINE
       IF (CLINE(1:5).EQ.CLIST(4)(1:5)) THEN
        CALL CHRLNB(CLINE,NC)
        WRITE(IDSP,117) CLINE(1:NC+1)
       END IF
       GOTO 425
 430   GO TO 8000
      ELSE IF (LMINUS .OR. (NF .LE. 1)) THEN
         LMENU = .FALSE.
         IF (LMINUS) THEN
           GO TO 8000
         ELSE
           LMENU = .TRUE.
           GO TO 8000
         END IF
      ELSE
         LMENU = .FALSE.
         CNAMEN = (CLINE(IBF(2):(ILF(2)+IBF(2)-1)))
         IF (NF .GT. 2) THEN
           IF (CLINE(IBF(3):IBF(3)).EQ.'R')  LRGMEN = .TRUE.
           IF (CLINE(IBF(3):IBF(3)).EQ.'D')  LDPMEN = .TRUE.
         END IF
         IF (NF .GT. 3) THEN
           IF (CLINE(IBF(4):IBF(4)).EQ.'R')  LRGMEN = .TRUE.
           IF (CLINE(IBF(4):IBF(4)).EQ.'D')  LDPMEN=.TRUE.
         END IF
C        CALL PRDMEN(CNAMEN,ISTMEN)                                     H
         IF (DIE) GO TO 8000
         IF (ISTMEN .NE. 0) GOTO 9000
 431     CONTINUE
         IF (LMINUS) THEN
         ELSE
           LMENU = .TRUE.
           CALL CHRLNB(CNAMEN,NC)
           WRITE(IDSP,432) CNAMEN(1:NC+1)
 432       FORMAT(/1X,A,2X,'IS NOW ACTIVE  ')
           WRITE(IDSP,*) ' ENTER OPTION FROM MENU '
         END IF
         GO TO 8000
      END IF
      GO TO 8000
C
C      ************************************************************
C      *   R U N                                                  *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE RUN COMMAND.*
C      *   FIRST THE NAME OF THE MACRO TO BE RUN IS DETERMINED,   *
C      *   THEN THE SUBROUTINE PLDMAC IS CALLED (PRE-READ LOAD    *
C      *   MACROS).                                               *
C      ************************************************************
C
C
  100 CONTINUE
       GO TO 105
C
C   ***
C   ***
C   ** THESE ARE THE LINES THAT HANDLE THE
C   ** INTERRUPT CALL.
C   ***
 104  LMACRO = .FALSE.
      IMACP = IMXMCP
C   ***
C   ***
C   ** IF NO JUMP LOCATION, TRY A RETURN AND HOPE FOR THE BEST
C   ***
      IF (JUMP .EQ. 0) GO TO 8000
C   ***
C   ***
C   ** ENABLE CHANGE OF JUMP LOCATION   **  ON HARRIS ONLY
C   ***
C     CALL CNTRLX(-4)                                                   H
C     CALL CNTRLX(JUMP)                                                 H
C     JUMP = 0                                                          H
C     CALL CNTRLX(-2)                                                   H
C   ***
C   ***
C   ** THE COMMAND IS PROCESSED
C   ***
 105  CONTINUE
      IF (LEQ) THEN
C
C     Check that the macro file has been connected
      INQUIRE (UNIT=IMAC, OPENED=LOPEN)
      IF (.NOT.LOPEN) THEN
      WRITE (IDSP,110)
 110  FORMAT (/,' *** Error:  PREAD Macro File Not Connected ***',/)
      GO TO 8000
      ENDIF
C
        REWIND IMAC
        IF (NF .LE. 1) THEN
 115      READ(IMAC,2,END=8000) CLINE
          CALL CHRLNB(CLINE,NC)
           IF (NC.GT.1) CALL UPCASE (CLINE(1:NC))
          IF (CLINE(1:6).EQ.'MACRO ')WRITE(IDSP,117)CLINE(1:NC+1)
 117      FORMAT(1X,A)
          GOTO 115
        END IF
        CNAMAC = CLINE(IBF(2):(IBF(2)+ILF(2)-1))
        CALL UPCASE (CNAMAC)
  125   READ (IMAC,2,END=140) CLINE
        CALL UPCASE (CLINE(1:6))
        IF (CLINE(1:6) .NE. 'MACRO ') GOTO 125
        ISTART = (NINDX(CLINE(6:),' ')+5)
        IEND = (INDEX(CLINE(ISTART:),' ')+ISTART-1)
        CALL UPCASE (CLINE(ISTART:IEND))
        IF (CLINE(ISTART:IEND) .NE. CNAMAC) GOTO 125
        GOTO 135
 130    READ(IMAC,2,END=8000) CLINE
 135    CALL CHRLNB(CLINE,NC)
        WRITE(IDSP,117) CLINE(1:NC+1)
        CALL UPCASE (CLINE(1:9))
        IF (CLINE(1:9) .EQ. 'ENDMACRO ') GOTO 8000
        GOTO 130
 140    CONTINUE
        CALL CHRLNB(CNAMAC,NC)
        WRITE(IDSP,141) CNAMAC(1:NC+1)
 141    FORMAT(' MACRO ',A,'  NOT FOUND')
      GO TO 8000
      ELSE
        LMACRO = .TRUE.
        IF (LMINUS) LMACRO = .FALSE.
        IF (NF .GT. 1) THEN
          CNAMAC = CLINE(IBF(2):(IBF(2)+ILF(2)-1))
          CALL UPCASE (CNAMAC)
          CALL PLDMAC(CNAMAC,ISTMAC)
        END IF
      END IF
C   ***
C   ***
C   ** HANDLE INTERRUPT    ** ON HARRIS ONLY
C   ***
C     IF (.NOT. LMINUS) THEN                                            H
C      IF (JUMP .GT. 0) GO TO 8000                                      H
C      JUMP = -1                                                        H
C      CALL CNTRLX(JUMP)                                                H
C      CALL CNTRLX($104)                                                H
C      CALL CNTRLX(-3)                                                  H
C     ELSE                                                              H
C       IF (JUMP .EQ. 0) GO TO 8000                                     H
C       CALL CNTRLX(-4)                                                 H
C       CALL CNTRLX(JUMP)                                               H
C       JUMP = 0                                                        H
C     END IF                                                            H
      GO TO 8000
C
C      ************************************************************
C      *   E C H O                                                *
C      *                                                          *
C      *   THIS SECTION OF THE PROGRAM SIMPLY TURNS THE ECHO ON   *
C      *   OR OFF DEPENDING ON WHAT THE USER SPECIFIES.           *
C      ************************************************************
C
C
  300 CONTINUE
      LECHO=.TRUE.
      IF (LMINUS) LECHO=.FALSE.
      GO TO 8000
C
C      ************************************************************
C      *   L E A R N                                              *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE LEARN       *
C      *   COMMAND.  THROUGH LEARN THE USER CAN "REMEMBER" THE    *
C      *   LINES ENTERED WHILE LEARNING.  THESE LINES ARE STORED  *
C      *   IN A MACRO AT THE END OF THE MACRO FILE.               *
C      ************************************************************
C
C
 200  CONTINUE
      IF (IMAC.EQ.-1) GO TO 8000
      IF (LMINUS) THEN
        IF (.NOT. LLEARN) GOTO 9000
        LLEARN = .FALSE.
        CALL WIND(IMAC)
        WRITE(IMAC,260)
 260    FORMAT('ENDMACRO')
        GO TO 8000
      ELSEIF ( LLEARN ) THEN
        WRITE(IDSP,*)' CANNOT PERFORM A LEARN COMMAND WHILE A LEARN'//
     &                 ' IS IN PROGRESS'
        GO TO 8000
      ELSEIF (NF .LE. 1) THEN
        WRITE(IDSP,*) ' MUST SUPPLY MACRO NAME'
        GO TO 8000
      ELSEIF ( (NF .GT. 2) .OR. (ILF(2) .GT. 8) ) THEN
        WRITE(IDSP,*)' MACRO NAME MUST BE 1-8 ALPHANUMERIC CHARACTERS'
        GO TO 8000
      ELSEIF ( IDT(2) .EQ. 3 ) THEN
        WRITE(IDSP,*)' MACRO NAME MUST NOT BE ENCLOSED IN QUOTES'
        GO TO 8000
      ELSE
        IF (.NOT. LMACOP) THEN
C           INQUIRE(FILE=CMACFL,EXIST=LEXIST)                           H
C           IF (.NOT.LEXIST) THEN                                       H
C              CALL CCREAT(CMACFL,0,0,0,IERR)                           H
C              CALL CRETYP(CMACFL,'00000164,0,IERR)                     H
C              WRITE (IDSP,264) CMACFL                                  H
C264           FORMAT (' FILE GENERATED: ',A)                           H
C           ENDIF                                                       H
            OPEN (UNIT=IMAC,FILE=CMACFL)
            LMACOP = .TRUE.
        ENDIF
        LLEARN = .TRUE.
      ENDIF
 265  CONTINUE
      CNAMAC = '        '
      CNAMAC = CLINE( IBF(2) : (IBF(2)+ILF(2)-1) )
      CALL UPCASE (CNAMAC)
      J = 0
C   ***
C   ***
C   ** SEARCH FOR MACRO
C   ***
      REWIND ISCT
      REWIND IMAC
 212  READ(IMAC,2,END=270) C133
      CALL UPCASE (C133(1:6))
      IF (C133(1:6) .NE. 'MACRO ') GOTO 220
      ISTART = (NINDX(C133(6:),' ')+5)
      IEND = (INDEX(C133(ISTART:),' ') + ISTART-1)
      CALL UPCASE (C133(ISTART:IEND))
      IF (C133(ISTART:IEND) .NE. CNAMAC) GOTO 220
C   ***
C   ***
C   ** FOUND IT, SO SKIP IT
C   ***
      J = 1
 213  READ(IMAC,2,END=270) C133
      CALL UPCASE (C133(1:9))
      IF (C133(1:9) .NE. 'ENDMACRO ') GOTO 213
C   ***
C   ***
C   ** COPY REMAINDER
C   ***
 215  READ(IMAC,2,END = 270) C133
      CALL CHRLNB(C133,NC)
      WRITE(ISCT,2) C133(1:NC+1)
      GOTO 215
C   ***
C   ***
C   ** WRITE LINE
C   ***
 220  CONTINUE
      CALL CHRLNB(C133,NC)
      WRITE(ISCT,2) C133(1:NC+1)
      GOTO 212
C   ***
C   ***
C   ** CHECK IF NEED TO COPY BACK FROM ISCT TO IMAC
C   ***
 270  CONTINUE
      IF (J .EQ. 0) GOTO 290
      REWIND ISCT
      REWIND IMAC
 280  READ(ISCT,2,END=290) C133
      CALL CHRLNB(C133,NC)
      WRITE(IMAC,2) C133(1:NC+1)
      GOTO 280
C   ***
C   ***
C   ** HERE TO ADD NEW ONE AT END
C   ***
 290  CONTINUE
      CALL WIND(IMAC)
      WRITE(IMAC,210) CNAMAC
 210  FORMAT('MACRO ',A)
C
      GO TO 8000
C
C      ************************************************************
C      *   K E Y B O A R D                                        *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE KEYBOARD    *
C      *   COMMAND.  THE TABLET IS TURNED OFF IF THE USER WANTS   *
C      *   CONTROL VIA THE KEYBOARD.                              *
C      ************************************************************
C
C
  500 CONTINUE
      LMACRO=.FALSE.
C   ***
C   ***
C   ** TURN OFF TABLET
C   ***
      IF (LMENU) THEN
       CALL CHRWT(IDIG,COFF,3)
       LMENU=.FALSE.
      END IF
C
      GO TO 8000
C
C      ************************************************************
C      *   H A R D C O P Y                                        *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE HARDCOPY    *
C      *   COMMAND.  FIRST, THE TYPE OF THE TERMINAL BEING USED   *
C      *   IS DETERMINED, THEN THE WILL PRODUCE HARDCOPY DEPEND-  *
C      *   ING ON WHAT TYPE OF TERMINAL IS BEING USED.            *
C      ************************************************************
C
C
  600 CONTINUE
C   ***
C   ***
C   ** CHECK REGS FOR TERM TYPE
C   ***
C     IF(ISTAT2.NE.0) GO TO 630                                         H
C   ***
C   ***
C   ** CHECK FOR TEK 4014
C   ***
C     IF ((CCONT2(1:4) .EQ. '4014') .OR. (CCONT2(1:4) .EQ. 'TABG')) THENH
C      CPRINT(1:2) = CHAR(27)//CHAR(23)                                 H
C      CALL CHRWT (IDSP,CPRINT,2)                                       H
C      CALL WAITS(10.0)                                                 H
C      ELSE IF (CCONT2(1:2) .EQ. '41') THEN                             H
C       CALL CHRWT(IDSP,CHAR(27)//'KH1',4)                              H
C     END IF                                                            H
 630  GO TO 8000
C
C      ************************************************************
C      *   F U N C T I O N                                        *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE FUNCTION    *
C      *   COMMAND.  FIRST, THE FUNCTION KEY IS DETERMINED, AS    *
C      *   WELL AS ITS DEFINITION, THEN THE USER IS SHOWN THE     *
C      *   NEWLY DEFINED FUNCTION.                                *
C      ************************************************************
C
C
  700 CONTINUE
      IF (LMINUS) LFUN=.FALSE.
      IF(LMINUS) GO TO 8000
      IF(.NOT.LEQ) GO TO 705
      IF(NKEY.LE.0) GO TO 8000
      WRITE (IDSP,*) 'THE FUNCTION CHARACTER IS ',CSPL(4:4)
      DO 707 I=1,NKEY
        IF ( IKEY(I) .GT. 0 ) THEN
          C133 = (CFUNCT(I)(1:IKEY(I)))
        ELSE
        C133 = ' '
        ENDIF
        C133((IKEY(I)+1):) = CARROW
        CALL CHRLNB(C133,NC)
        IF(ICHAR(CKEY(I)).LT. 32) THEN
          CTKEY = CHAR((ICHAR(CKEY(I)))+64)
          WRITE(IDSP,709) CTKEY,C133(1:NC+1)
 709      FORMAT(' FUNCTION: CONTROL-',A,'  ===>',A)
        ELSE
          WRITE(IDSP,708) CKEY(I),C133(1:NC+1)
        ENDIF
  707 CONTINUE
  708 FORMAT(' FUNCTION: ',A,'  ===>',A)
      GO TO 8000
  705 LFUN=.TRUE.
      GO TO 8000
C
C      ************************************************************
C      *   J C L                                                  *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE JCL COMMAND.*
C      *   IF THE USER SPECIFIES THE JCL COMMAND, A MESSAGE IS    *
C      *   CREATED THAT SPECIFIES THAT THE USER HAS MADE A REQUEST*
C      *   TO PERFORM A JCL FUNCTION.                             *
C      ************************************************************
C
C
  800 CONTINUE
      IF (NF.EQ.1) THEN
C     CMESS = '*JOBCNTRL'                                               H
C     CMESS = 'COMMAND'//CHAR(0)                                        M
C     NMESS=9                                                           H
C     WRITE (*,*) ' Enter EXIT to return from Operating System'         M
      ELSE
      NMESS = IBF(NF)+ILF(NF)-IBF(2)
      CMESS = CLINE (IBF(2):IBF(2)+NMESS-1)
C     CMESS = CMESS(1:NMESS)//CHAR(0)                                   M
C     REWIND ISCT                                                       H
C     CALL XQTJCL(ISCT,CMESS,NMESS)                                     H
C     GO TO 8000                                                        H
      ENDIF
      GO TO 5000
C
C      ************************************************************
C      *   C O E D                                                *
C      *                                                          *
C      *   THIS SECTION OF THE PROGRAM HANDLES THE COED COMMAND.  *
C      *   SIMILAR TO JCL, A MESSAGE IS CREATED THAT SPECIFIES    *
C      *   THAT THE USER HAS MADE A REQUEST TO PERFORM A COED     *
C      *   FUNCTION.                                              *
C      ************************************************************
C
C
  900 CONTINUE
C     CMESS = '*COED'                                                   H
C     CMESS = 'COED'                                                    M
      IF(NF.LE.1) GO TO 9000
      IF(ILF(2).LE.0) GO TO 9000
      NMESS = ILF(NF)+IBF(NF)-IBF(2)
      CMESS(7:) = CLINE(IBF(2):IBF(2)+NMESS)
C     CMESS = CMESS(1:6+NMESS)//CHAR(0)                                 M
      NMESS=NMESS+6
      GO TO 5000
C
C      ************************************************************
C      *    L O G                                                 *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE LOG        *
C      *    COMMAND.  THE LOG IS SIMPLY TURNED ON OR OFF.         *
C      ************************************************************
C
C
 1000 CONTINUE
      IF (ILOG.EQ.-1) GO TO 8000
      INQUIRE (FILE=CLOGFL,OPENED=LLOG,EXIST = LEXIST)
      IF (.NOT.LLOG) THEN
C        IF (.NOT.LEXIST) THEN                                          H
C           CALL CCREAT(CLOGFL,0,0,0,IERR)                              H
C           CALL CRETYP(CLOGFL,'00000164,0,IERR)                        H
C           WRITE (IDSP,1001) CLOGFL                                    H
C1001       FORMAT (' FILE GENERATED: ',A)                              H
C        ENDIF                                                          H
         OPEN (UNIT=ILOG,FILE=CLOGFL)
      ENDIF
      LLOG = .NOT.LMINUS
      GO TO 8000
C
C      ************************************************************
C      *    T E A C H                                             *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE TEACH      *
C      *    COMMAND.  TEACH ALLOWS THE USER TO EITHER ADD OR      *
C      *    DELETE A FUNCTION FROM THE FUNCTION FILE.             *
C      ************************************************************
C
C
 1100 CONTINUE
      IL = (JMXNCH - ILINEP + 1)
      NF = -20
      LREG = .FALSE.
      CALL FINDLM(CLINSV,ILINEP,IL,NF,IBF,ILF,IDT,IPT,ITBL)
      IF (LMINUS .AND. (NF .GT. 2)) GOTO 9000
      IF (NF.LT.2) GO TO 9000
CX    IF (.NOT.LMINUS .AND. NF.EQ.2) THEN
C     MUST BE A TEACH THAT IS TO USE THE LAST KBLINE ENTRY
CX    IB = IBF(2) + ILF(2) + 1
CX    CALL CHRLNB ( CKBLIN, NB )
CX    IF (NB.LT.1) GO TO 9000
CX    CLINSV(IB:IB+NB-1) = CKBLIN(1:NB)
CX    GO TO 1100
CX    ENDIF
C     NOW CHECK IF THE 3RD FIELD IS A REG REFERENCE
C     IF (NF.EQ.3.AND.ILF(3).EQ.4.AND.IDT(3).LT.3) THEN                 H
C     IF (CLINSV(IBF(3):IBF(3)).EQ.'#') THEN                            H
C     CALL GSTRRG (CLINSV(IBF(3)+1:IBF(3)+3),                           H
C    1  CLINSV(IBF(3):IBF(3)+IFUNLN-1),NC,IST)                          H
C     IF (IST.NE.0) THEN                                                H
C ------ DEFINE AS ZERO LENGTH FUNCTION
C     NC = 0                                                            H
C     ENDIF                                                             H
C     CLINSV(IBF(3)+NC:) = ' '                                          H
C     GO TO 1100                                                        H
C     ENDIF                                                             H
C     ENDIF                                                             H
C
C     NOW CHECK IF THIRD FIELD IS A FORCED FUNCTION REF - ONE ONLY
C
C     IF(NF.EQ.3.AND.ILF(3).EQ.2.AND.IDT(3).LT.3) THEN                  H
C     IF(CLINSV(IBF(3):IBF(3)).EQ.CSPL(4:4)) THEN                       H
C     DO 1161 K=1,NKEY                                                  H
C     IF(CKEY(K).EQ.CLINSV(IBF(3)+1:IBF(3)+1)) THEN                     H
C     CLINSV(IBF(3):IBF(3)+IFUNLN-1)= CFUNCT(K)                         H
C     GO TO 1100                                                        H
C     ENDIF                                                             H
C1161 CONTINUE                                                          H
C     ENDIF                                                             H
C     ENDIF                                                             H
C
C     IF (ILF(2).EQ.4) THEN                                             H
C     IF (IDT(2).EQ.3) GO TO 9000                                       H
C     CHECK FOR REGISTER REFERENCE
C     IF (CLINSV(IBF(2):IBF(2)).NE.'#') GO TO 9000                      H
C     LREG = .TRUE.                                                     H
C     GO TO 1110                                                        H
C
C     ELSE IF (ILF(2).NE.1) THEN                                        H
C     GO TO 9000                                                        H
C     ENDIF                                                             H
C
      CH1 = CLINSV(IBF(2):IBF(2))
      IPOS = ISCAN( CSPL, 1, 4, CH1, 1, 1, IDUMMY )
      IF (IPOS .GT. 0) THEN
        WRITE(IDSP,1112) CSPL(IPOS:IPOS)
 1112   FORMAT(' THE ',A,' CHARACTER MAY NOT BE A DEFINED FUNCTION KEY')
        GOTO 9000
      ENDIF
      DO 1105 J=1,NKEY
        I = J
        IF (CKEY(I) .EQ. CH1) GO TO 1110
 1105 CONTINUE
      IF (LMINUS) GO TO 9000
      I = NKEY + 1
      IF (I.GT.KKEY) GO TO 9000
 1110 IF (LMINUS) GO TO 1118
      ISTART = NINDX(CLINSV(IBF(2)+ILF(2):),' ')
C ------ If zero, then only 2 fields, so do zero length function
      IF ( ISTART .EQ. 0 ) THEN
      ISTART = IBF(2) + ILF(2)
      ILSTCH = ISTART + 1
      ILNGTH = 0
      GO TO 1118
      ENDIF
C ------
      ISTART = ISTART + IBF(2) + ILF(2) - 1
      IF (ISTART.LE.0) GO TO 9000
      CALL CHRLNB(CLINSV,ILNGTH)
      ILNGTH = ILNGTH-ISTART+1
      ILSTCH = ISTART + ILNGTH - 1
      IF ((CLINSV(ISTART:ISTART) .EQ. CSQUOT)  .OR.
     &    (CLINSV(ISTART:ISTART) .EQ. CDQUOT)  .OR.
     &    (CLINSV(ILSTCH:ILSTCH) .EQ. CSQUOT)  .OR.
     &    (CLINSV(ILSTCH:ILSTCH) .EQ. CDQUOT)) THEN
          IF (CLINSV(ISTART:ISTART) .NE. CLINSV(ILSTCH:ILSTCH)) THEN
              WRITE(IDSP,*) ' MISMATCHED QUOTE ERROR'
              GOTO 9000
          ENDIF
          ISTART = ISTART + 1
          ILNGTH = ILNGTH - 2
      ENDIF
      IF ( ILNGTH .GT. IFUNLN         ) THEN
         WRITE(IDSP, 1113) IFUNLN, ILNGTH
 1113    FORMAT(' FUNCTION DEFINITION MUST NOT BE GREATER THAN', I3,
     &          ' CHARACTERS;  Length:',I4)
         GOTO 9000
      ENDIF
C
C
C ------
 1118 CONTINUE
C     IF( LREG ) THEN                                                   H
C     IF ( LMINUS ) THEN                                                H
C     REMOVE THIS REGISTER
C     CALL SSTRRG (CLINSV(IBF(2)+1:IBF(2)+3),CLINSV, 0, IST )           H
C     ELSE                                                              H
C     SET THIS REGISTER
C     CALL SSTRRG (CLINSV(IBF(2)+1:IBF(2)+3),CLINSV(ISTART:ILSTCH),     H
C    1  ILNGTH, IST)                                                    H
C     ENDIF                                                             H
C ------
C     ELSE                                                              H
      IF ( LMINUS ) THEN
      CALL PSETFN(CH1,CLINSV,-1)
      ELSE
      CALL PSETFN( CH1, CLINSV(ISTART:ILSTCH), ILNGTH )
      ENDIF
C ------
C     ENDIF                                                             H
      GO TO 8000
C   ***
C
C      ************************************************************
C      *    ?                                                     *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE '?'        *
C      *    COMMAND.  A LIST OF VALID PRE-READ COMMANDS ARE       *
C      *    DISPLAYED FOR THE USER.                               *
C      ************************************************************
C
C
 1200 CONTINUE
      IF (NF .LT. 2) THEN
         WRITE (IDSP,1201)
 1201    FORMAT (' VALID PRE-READ COMMANDS')
         WRITE (IDSP,1202) (CLIST(I),I=1,NLIST)
 1202    FORMAT (8(1X,A))
      ELSE
C
C     --- Use functions unit since it is not currently in use.
C
C        IHLP = IFUN                                                    H
C        IF (IHLP.NE.-1) THEN                                           H
C        OPEN (UNIT=IHLP,FILE=CHLPFL,STATUS='OLD',ERR=1220)             H
C        CALL MESSAG (IHLP, IDSP, CLINE(IBF(2):IBF(2)+ILF(2)-1))        H
C        CLOSE (UNIT=IHLP)                                              H
C        ENDIF                                                          H
      ENDIF
      GO TO 8000
C
 1220 WRITE (IDSP,*) 'HELP FILE, ',CHLPFL,', NOT AVAILABLE'
      GO TO 8000
C
C      ************************************************************
C      *    C H A I N                                             *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE CHAIN      *
C      *    COMMAND.  THE MESSAGE THAT SIGNIFIES THE CHAIN        *
C      *    COMMAND IS STORED IN A CHARACTER VARIABLE, THEN       *
C      *    CONTROL OF THE SUBROUTINE IS SENT TO THE AREA THAT    *
C      *    WILL RELINQUISH PRE-READ'S CONTROL TO WHAT (OR        *
C      *    WHERE) THE CHAIN COMMAND SPECIFIES.                   *
C      ************************************************************
C
C
 1300 CONTINUE
      IF (NF.LT.2) GO TO 9000
      CALL CHRLNB(CLINE,NMESS)
      NMESS = NMESS-IBF(2)+1
      CMESS = CLINE(IBF(2) : (IBF(2) + NMESS-1))
      CMESS = CMESS//CHAR(0)                                            MLlg
      GO TO 5000
C
C      ************************************************************
C      *    S T A T U S                                           *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE STATUS     *
C      *    COMMAND.  STATUS SIMPLY SHOWS THE USER THE STATUS OF  *
C      *    THE SIX PRE-READ INPUT AND FUNCTIONAL FILES (MACRO,   *
C      *    MENU,ECHO,FUNCTION,LEARN,& LOG).                      *
C      ************************************************************
C
C      16 AUG 88 Alaric Clinton - rewritten to show the macro
C        buffer size and the number of lines avialable
C
 1400 CONTINUE
C
        DO  1405 I=1,7
 1405     CRESP(I) = 'OFF'
C
        IF (LMACRO) CRESP(1) = 'ON'
        IF (LMENU)  CRESP(2) = 'ON'
        IF (LECHO)  CRESP(3) = 'ON'
        IF (LFUN)   CRESP(4) = 'ON'
        IF (LLEARN) CRESP(5) = 'ON'
        IF (LLOG)   CRESP(6) = 'ON'
        IF (LAUTO)  CRESP(7) = 'ON'
C
        WRITE (IDSP,'(1X,A,/)') 'FLAG STATUS'
C
C       WRITE (IDSP,'(1X,A11,A3,2(5X,A,1X,I3))') 'MACRO      ',CRESP(1),H
        WRITE (IDSP,'(1X,A11,A3,2(5X,A,1X,I2))') 'MACRO      ',CRESP(1),MLlg
     +    'buffer lines filled',IMXMCP-IMACP,'available',IMACP
        WRITE (IDSP,'(1X,A11,A3)')   'MENU       ',CRESP(2)
        WRITE (IDSP,'(1X,A11,A3)')   'ECHO       ',CRESP(3)
        WRITE (IDSP,'(1X,A11,A3)')   'FUNCTION   ',CRESP(4)
        WRITE (IDSP,'(1X,A11,A3)')   'LEARN      ',CRESP(5)
        WRITE (IDSP,'(1X,A11,A3)')   'LOG        ',CRESP(6)
        WRITE (IDSP,'(1X,A11,A3/)')  'AUTOSCREEN ',CRESP(7)
C
        WRITE (IDSP,*) 'THE COMMAND CHARACTER IS  ',CSPL(1:1)
        WRITE (IDSP,*) 'THE FUNCTION CHARACTER IS ',CSPL(4:4)
      GO TO 8000
C
C      ************************************************************
C      *    P A G E                                               *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE PAGE       *
C      *    COMMAND.  IF THE USER SPECIFIES THIS COMMAND, THE     *
C      *    PAGE WILL BE ADVANCED.  If arg 1 is GRAPHIC then      *
C      *    a graphics clear will be output  else if = PRINTER a  *
C      *    FF (form feed will be output.  If = TEKSEG all        *
C      *    segments will be made invisible.                      *
C      ************************************************************
C
C
 1500 CONTINUE
      IF (NF.EQ.1) THEN
      CALL VERASE
      ELSE IF (CLINE(IBF(2):IBF(2)).EQ.'G') THEN
      CALL VERASG
      ELSE IF (CLINE(IBF(2):IBF(2)).EQ.'P') THEN
         CALL CHRWT(IDSP,CHAR(12),1)
      ELSE IF (CLINE(IBF(2):IBF(2)).EQ.'T') THEN
C        CALL VSET ( 'TEK', '----' )                                    H
C        CALL SEGVIS(IDSP,-1,'OFF')                                     H
C        CALL CHRWT (IDSP, CHAR(27)//CHAR(12), 2)                       H
C        CALL VSET ( 'ANSI', '----' )                                   H
      ENDIF
 1530 GO TO 8000
C      ************************************************************
C      *    W A I T                                               *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE WAIT       *
C      *    COMMAND.  WHEN THE USER SPECIFIES THE WAIT COMMAND,   *
C      *    THE SUBROUTINE SIMPLY UTILIZES THE WAIT SUBROUTINE.   *
C      ************************************************************
C
C
 1600 CONTINUE
      ITIM=INTGR(CLINE,IBF(2),ILF(2),ISTX)
      CALL WAITS(FLOAT(ITIM))
      GO TO 8000
C
C      ************************************************************
C      *    P A U S E                                             *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE PAUSE      *
C      *    COMMAND.  PAUSE WAITS FOR INPUT FROM THE LFN THAT     *
C      *    IS ASSOCIATED WITH THE TERMINAL.  THE INPUT MUST      *
C      *    BE A RETURN IN ORDER FOR THE PROGRAM TO CONTINUE.     *
C      *    THE INPUT COULD BE FROM THE KEYBOARD (OR TABLET?).    *
C      ************************************************************
C
C
 1700 CONTINUE
C
C ------ Make PAUSE work the same way as KBLINE, except
C ------ Always trash what is entered at the keyboard
C ------ This will let PAUSE use timeout and other features
C
C     SET LKBLIN ON AND GO TO 8000
C
      LKBLIN = .TRUE.
      LTEACH = .FALSE.
      LPAUZ = .TRUE.
C ------
      LMOV = .TRUE.
      LPMT = .TRUE.
      IF ( LCOPTS ( COPT, 'H' ) ) THEN
      LPMT = .FALSE.
      LMOV = .FALSE.
      ENDIF
      IF ( LCOPTS ( COPT, 'P' ) ) LPMT = .TRUE.
C
      GO TO 8000
C
C      ************************************************************
C      *    /  AND  *                                             *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE EXISTS TO KEEP THE     *
C      *    LOGICAL STRUCTURE OF CLIST INTACT.  THE FOLLOWING     *
C      *    THREE LINES SHOULD NEVER BE EXECUTED SINCE BOTH THE   *
C      *    / AND THE * HAVE BEEN IDENTIFIED AND DEALT WITH AS    *
C      *    SPECIAL COMMAND CASES.  SINCE CLIST CONTAINS ALL OF   *
C      *    THE COMMANDS RECOGNIZED BY PREAD AND PRINTED OUT WITH *
C      *    THE ? COMMAND, THE COMMANDS / AND * WERE LEFT IN      *
C      *    CLIST.                                                *
C      *              '/'  ===>  DENOTES AN OPCOM COMMAND         *
C      *              '*'  ===>  DENOTES A COMMENT (=IGNORE)      *
C      ************************************************************
C
C
 1800 CONTINUE
 1900 CONTINUE
      GO TO 8000
C
C      ************************************************************
C      *    MACRO                                                 *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE 'MACRO'    *
C      *    COMMAND.  THIS COMMAND IS IDENTICAL IN FUNCTION TO    *
C      *    THE 'RUN' COMMAND AND WAS ADDED TO ENHANCE USER-      *
C      *    FRIENDLINESS.                                         *
C      ************************************************************
C
C
 2000 CONTINUE
      GO TO 100
C
C      ************************************************************
C      *   E D I T                                                *
C      *                                                          *
C      *   THIS SECTION OF THE SUBROUTINE HANDLES THE EDIT        *
C      *   COMMAND.  THROUGH EDIT THE USER CAN ADD A NEW MACRO,   *
C      *   CHANGE AN EXISTING MACRO, OR DELETE A MACRO.           *
C      ************************************************************
C
C
 2100 CONTINUE
      IF ( LLEARN ) THEN
        WRITE(IDSP,*)'CANNOT PERFORM AN EDIT COMMAND WHILE A LEARN'//
     &                  ' IS IN PROGRESS'
        GO TO 8000
      ELSEIF (LMINUS) THEN
       WRITE(IDSP,*)'Invalid PREAD command'
       LMINUS = .FALSE.
       GO TO 8000
      ELSEIF (NF .LE. 1) THEN
       WRITE(IDSP,*) 'MUST SUPPLY MACRO NAME'
       GO TO 8000
      ELSEIF ( (NF .GT. 2) .OR. (ILF(2) .GT. 8) ) THEN
       WRITE(IDSP,*)'MACRO NAME MUST BE 1-8 ALPHANUMERIC CHARACTERS'
       GO TO 8000
      ELSEIF ( IDT(2) .EQ. 3) THEN
       WRITE(IDSP,*)'MACRO NAME MUST NOT BE ENCLOSED IN QUOTES'
       GO TO 8000
      ENDIF
      CNAMAC = '        '
      CNAMAC = CLINE( IBF(2) : (IBF(2)+ILF(2)-1) )
      CALL UPCASE (CNAMAC)
C
      CALL PEDIT
      GO TO 8000
C
 2200 CONTINUE
C
C      ************************************************************
C      *    SCREEN                                                *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE 'SCREEN'   *
C      *    COMMAND.  THIS COMMAND IS BRINGS UP A SCREEN TYPE     *
C      *    SELECTION MODE FOR FEEDING ONE OR MORE LINES TO       *
C      *    A PROGRAM.                                            *
C      ************************************************************
C
      LVSCN=.FALSE.
      LISCN=.FALSE.
      IF(LCOPTS(COPT,'V')) LVSCN=.TRUE.
      IF(LCOPTS(COPT,'I')) LISCN=.TRUE.
      IF (ISCN.EQ.-1) GO TO 8000
      OPEN ( UNIT=ISCN, FILE=CSCNFL, STATUS='OLD', IOSTAT=IZ )
      IF ( IZ .NE. 0 ) THEN
 2202 WRITE ( IDSP, 2203 ) CSCNFL, IZ
 2203 FORMAT(/' PREAD Error - Screen file not valid: ',A,2X,I8/)
      GO TO 8000
      ENDIF
C
C-------
 2205 CONTINUE
      IF (LEQ) THEN
C
        REWIND ISCN
        IF (NF .LE. 1) THEN
 2215     READ(ISCN,2,END=2250) CLINE
          CALL CHRLNB(CLINE,NC)
          IF (CLINE(1:8).EQ.'#SCREEN ')WRITE(IDSP, 117)CLINE(1:NC+1)
          GOTO 2215
        END IF
        CNAMAC = CLINE(IBF(2):(IBF(2)+ILF(2)-1))
      CALL UPCASE (CNAMAC)
 2225   READ (ISCN,2,END=2240) CLINE
        IF (CLINE(1:8) .NE. '#SCREEN ') GOTO 2225
        ISTART = (NINDX(CLINE(8:),' ')+7)
        IEND = (INDEX(CLINE(ISTART:),' ')+ISTART-1)
        IF (CLINE(ISTART:IEND) .NE. CNAMAC) GOTO 2225
        GOTO 2235
 2230   READ(ISCN,2,END=2250) CLINE
 2235   CALL CHRLNB(CLINE,NC)
        WRITE(IDSP,117) CLINE(1:NC+1)
        IF (CLINE(1:10) .EQ. '#ENDSCREEN') GOTO 8000
        GOTO 2230
 2240   CONTINUE
        CALL CHRLNB(CNAMAC,NC)
        WRITE(IDSP,2241) CNAMAC(1:NC+1)
 2241   FORMAT(' Screen ',A,'  NOT found')
 2250   GO TO 8000
      ELSE
C-------
      IF(NF.NE.2) GO TO 9000
      LSCN=.TRUE.
      CALL PLDSCN(CLINE(IBF(2):IBF(2)+7))
      GO TO 8000
      ENDIF
C   ***
 2300 CONTINUE
C
C      ************************************************************
C      *    KBLINE                                                *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE 'KBLINE'   *
C      *    COMMAND.  THIS COMMAND ALLOWS THE ENTRY OF ONE LINE   *
C      *    ONLY OF INPUT FROM THE KEYBOARD WHILE A MACRO IS IN   *
C      *    EXECUTION. AFTER THE ENTRY THE MACRO CONTINUES.       *
C      ************************************************************
C
C
C     SET LKBLIN ON AND GO TO 8000
C
      LKBLIN = .TRUE.
      LTEACH = .FALSE.
      IF ( NF .EQ. 2 ) THEN
      IF ( ILF(2) .EQ. 1 .OR. ILF(2) .EQ. 4 ) THEN
      LTEACH = .TRUE.
      CSYMB = CLINE(IBF(2):IBF(2)+ILF(2)-1)
      ELSE
      LKBLIN = .FALSE.
      GO TO 9000
      ENDIF
      ENDIF
C
C     check if KB is requesting to use line input from
C     screen  "!KB.S"   prompt as input for teach
C     put last line from CKBLIN in CLINE and loop to top
C
      IF(LCOPTS (COPT,'S')) THEN
         CLINE=CKBLIN
         LKBLIN=.FALSE.
         GO TO 3
      ENDIF
C
C
      LMOV = .TRUE.
      LPMT = .TRUE.
      IF ( LCOPTS ( COPT, 'H' ) ) THEN
      LPMT = .FALSE.
      LMOV = .FALSE.
      ENDIF
      IF ( LCOPTS ( COPT, 'P' ) ) LPMT = .TRUE.
      GO TO 8000
C
C
 2400 CONTINUE
C
C      ************************************************************
C      *    PRINT                                                 *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE 'PRINT'    *
C      *    COMMAND.  THIS COMMAND ALLOWS A LINE TO BE DISPLAYED  *
C      *    TO THE USER MUCH LIKE A COMMENT. THIS DIFFERS FROM    *
C      *    COMMENT IN THAT IT WILL ALWAYS PRINT.                 *
C      ************************************************************
C
C
      IF ( LCOPTS ( COPT, 'C' ) ) THEN
C ------ Here for PRINT.C request
C ------ Format:  PRINT.C r, c, "Message to be printed"
C ------ Note: Must have exactly 4 fields on line ("'s are required)
C ------
C ------ If r, or c are unsigned, do absolute screen coordinate move
C ------ If signed, do relative move
C ------
      IF ( NF .NE. 4 ) GO TO 9000
      CTMP = CLINE(IBF(2):IEF(2))
      IF ( CTMP(1:1) .EQ. '+' ) THEN
      IBIAS = 2000
      ELSE IF ( CTMP(1:1) .EQ. '-' ) THEN
      IBIAS = 1000
      CTMP(1:1) = '0'
      ELSE
      IBIAS = 0
      ENDIF
      READ(CTMP,'(BN,I6)',ERR=9000) IROW
      IROW = IROW + IBIAS
      CTMP = CLINE (IBF(3):IEF(3))
      IF ( CTMP(1:1) .EQ. '+' ) THEN
      IBIAS = 2000
      ELSE IF ( CTMP(1:1) .EQ. '-' ) THEN
      IBIAS = 1000
      CTMP(1:1) = '0'
      ELSE
      IBIAS = 0
      ENDIF
      READ(CTMP,'(BN,I6)',ERR=9000) ICOL
      ICOL = ICOL + IBIAS
      CALL VMOVCR ( IROW, ICOL )
      IF ( ILF(4) .GT. 0 ) THEN
      CALL CHRWT ( IDSP, CLINE(IBF(4):IEF(4)), ILF(4) )
      ENDIF
      ELSE
      CALL CHRLNB(CLINE(1:80),N)
      IF (N.LE.IBF(1)+ILF(1)) N = IBF(1)+ILF(1)
      WRITE(IDSP,2410) CLINE(IBF(1)+ILF(1):N)
 2410 FORMAT (1X,A)
      ENDIF
C
      GO TO 8000
C
 2500 CONTINUE
 2600 CONTINUE
C
C      ************************************************************
C      *    DISPLAY, LIST                                         *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE 'DISPLAY'  *
C      *    AND 'LIST' COMMANDS.  THESE COMMANDS DISPLAY          *
C      *    PORTIONS OF A FILE.                                   *
C      ************************************************************
C
C
      CALL PDISP ('DISP', COPT ,LMINUS)
      GO TO 8000
C
 2700 CONTINUE
C
C      ************************************************************
C      *    FIND                                                  *
C      *                                                          *
C      *    THIS SECTION OF THE SUBROUTINE HANDLES THE 'FIND'     *
C      *    COMMAND.  THIS COMMAND SEARCHES A FILE FOR A STRING,  *
C      *    THEN DISPLAY A PORTION OF THE FILE WHERE THE STRING   *
C      *    IS FOUND.                                             *
C      ************************************************************
C
C
      CALL PDISP ('FIND', COPT, LMINUS)
      GO TO 8000
C
C
C
C     !IJOB jobfile, password
C
C
 2800 CONTINUE
C     IF (NF.LE.1) THEN                                                 H
C     WRITE (IDSP,*)'No Job-File Name follows IJOB command!'            H
C     GO TO 8000                                                        H
C     ENDIF                                                             H
C
C     JE = IBF(2) + ILF(2) - 1                                          H
C     IF (NF.EQ.2) THEN                                                 H
C     CALL CIJOBE (CLINE(IBF(2):JE), ' ', IERR)                         H
C     ELSE                                                              H
C     JE2 = IBF(3) + ILF(3) - 1                                         H
C     CALL CIJOBE (CLINE(IBF(2):JE), CLINE(IBF(3):JE2), IERR)           H
C     ENDIF                                                             H
C     IF (IERR.NE.0) THEN                                               H
C     WRITE (IDSP, 2710) IERR                                           H
C2710 FORMAT (/' Unable to submit job!',/' Harris Error Code:',I6,/)    H
C     ELSE                                                              H
C     WRITE (IDSP, 2720) CLINE(IBF(2):JE)                               H
C2720 FORMAT (' Job File ',A,' Submitted.')                             H
C     ENDIF                                                             H
      GO TO 8000
C
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     INPUT
C
C       This routine is intended to facilitate the addition of special
C     feature requests, notably SELECT5.  INPUT has two modes: OPTION
C     which is similar to chaining, FILE where a temporary file is
C     specified which is loaded into the MACRO BUFFER.
C
C     !INPUT OPTION=SELECT (<catalog unit>,<scratch macro file>)
C     !INPUT FILE=<scratch macro file>
C
 2900 IF (NF .GT. 2) THEN
        CMODE = CLINE (IBF(2): IBF(2)+ILF(2)-1)
        CALL UPCASE(CMODE)
C
C       * removed due to lack of use - necessary for SELECT 5 *
C
C       IF (INDEX ('OPTION', CMODE(1:ILF(2))) .NE. 0) THEN
C         CALL PINPUT (1,CLINE (IBF(3):))
C       ELSE IF (INDEX ('FILE', CMODE(1:ILF(2))) .NE. 0) THEN
C         CALL PINPUT (2,CLINE (IBF(3):IBF(3)+ILF(3)-1))
C       ELSE
          WRITE (IDSP, 2920) CMODE(1:ILF(2))
 2920 FORMAT (' INPUT option ',A,' not recognized.')
C       ENDIF
      ENDIF
      GO TO 8000
C
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     XX - Command not in use
C
 3000 CONTINUE
C
      GO TO 9000
C
C
C     SET Command to set various options
C
 3100 CONTINUE
C ------
C ------
C ------ Check for STATUS option
C ------
      IF ( CLINE(IBF(2):IEF(2)) .EQ. 'STATUS' ) THEN
C ------
      IF ( CLINE(IBF(4):IEF(4)) .EQ. 'ON' ) THEN
      LTMP = .TRUE.
      ELSE IF ( CLINE(IBF(4):IEF(4)) .EQ. 'OFF' ) THEN
      LTMP = .FALSE.
      ELSE
      GO TO 9000
      ENDIF
C
      CALL UPCASE (CLINE(IBF(3):IEF(3)))
      IF      ( CLINE(IBF(3):IEF(3)) .EQ. 'MACRO' ) THEN
      LMACRO = LTMP
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'MENU' ) THEN
      LMENU = LTMP
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'ECHO' ) THEN
      LECHO = LTMP
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'FUNCTION' ) THEN
      LFUN = LTMP
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'LEARN' ) THEN
      LLEARN = LTMP
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'LOG' ) THEN
      LLOG = LTMP
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'AUTOSCREEN' ) THEN
      LAUTO = LTMP
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'BREAK' ) THEN
      IF ( LTMP ) THEN
C     CALL BRKON                                                        H
      ELSE
C     CALL BRKOFF                                                       H
      ENDIF
      ELSE
      GO TO 9000
      ENDIF
C ------
C ------
C ------ Check for STTY option
C ------
C     ELSE IF ( CLINE(IBF(2):IEF(2)) .EQ. 'STTY' ) THEN                 H
C     IF ( NF .LT. 4 ) THEN                                             H
C     IBF(4) = IEF(3) + 1                                               H
C     IEF(4) = IBF(4)                                                   H
C     CLINE (IBF(4):IEF(4)) = CHAR ( 0 )                                H
C     ENDIF                                                             H
C     CALL STTY ( IKB, 'SET', CLINE(IBF(3):IEF(3)),                     H
C    *     CLINE(IBF(4):IEF(4)), IST )                                  H
C     IF ( IST .NE. 0 ) GO TO 9000                                      H
C ------
C ------ Check for TERMINAL option
C ------
C     ELSE IF ( CLINE(IBF(2):IEF(2)) .EQ. 'TERMINAL' ) THEN             H
C     IF ( CLINE(IBF(3):IEF(3)) .EQ. 'TYPE' ) THEN                      H
C     CALL SSTRRG ( 'STM', CLINE(IBF(4):IEF(4)), ILF(4), IST )          H
C ------ Now re-initialize ANSI characteristics of terminal
C     CALL VINIT ( -1 )                                                 H
C     ENDIF                                                             H
C ------
C ------ BAD REQUEST !!!!!!
      ELSE
      GO TO 9000
      ENDIF
C
      GO TO 8000
C
C
C
C
C
C     GET Command to get various options
C
 3200 CONTINUE
C ------
      CSYMB = CLINE(IBF(4):IEF(4))
C ------
C ------ Check for STATUS option
C ------
      IF ( CLINE(IBF(2):IEF(2)) .EQ. 'STATUS' ) THEN
C ------
        DO  3210 I=1,7
 3210     CRESP(I) = 'OFF'
C
        IF (LMACRO) CRESP(1) = 'ON'
        IF (LMENU)  CRESP(2) = 'ON'
        IF (LECHO)  CRESP(3) = 'ON'
        IF (LFUN)   CRESP(4) = 'ON'
        IF (LLEARN) CRESP(5) = 'ON'
        IF (LLOG)   CRESP(6) = 'ON'
        IF (LAUTO)  CRESP(7) = 'ON'
C
      CALL UPCASE (CLINE(IBF(3):IEF(3)))
      IF      ( CLINE(IBF(3):IEF(3)) .EQ. 'MACRO' ) THEN
      CLINE = CRESP(1)
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'MENU' ) THEN
      CLINE = CRESP(2)
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'ECHO' ) THEN
      CLINE = CRESP(3)
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'FUNCTION' ) THEN
      CLINE = CRESP(4)
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'LEARN' ) THEN
      CLINE = CRESP(5)
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'LOG' ) THEN
      CLINE = CRESP(6)
      ELSE IF ( CLINE(IBF(3):IEF(3)) .EQ. 'AUTOSCREEN' ) THEN
      CLINE = CRESP(7)
      ELSE
      GO TO 9000
      ENDIF
C ------
C ------ Check for STTY option
C ------
C     ELSE IF ( CLINE(IBF(2):IEF(2)) .EQ. 'STTY' ) THEN                 H
C     CALL STTY ( IKB, 'GET', CLINE(IBF(3):IEF(3)),                     H
C    *     CMESS, IST )                                                 H
C     IF ( IST .NE. 0 ) GO TO 9000                                      H
C     CLINE = CMESS                                                     H
C ------
C ------ ELSE BAD REQUEST 111111
      ELSE
      GO TO 9000
      ENDIF
C
      LTEACH = .TRUE.
      GO TO 3
C
C
C
C------ !IF
C
 3300 IFLEVL = IFLEVL + 1
      LIFDON = .FALSE.
C     WRITE(*,*) ' IFLEVL, LIFDON @ IF ', IFLEVL, LIFDON
C------ !ELSEIF
 3400 CONTINUE
      IF ( LIFDON ) GO TO 3650
      IF ( NF .NE. 5 ) GO TO 3650
      IF ( CLINE(IBF(5):IEF(5)) .NE. 'THEN' ) GO TO 3650
C------ Must be valid IF. or ELSEIF command
      IF ( CLINE(IBF(3):IEF(3)) .EQ. '.EQ.' ) THEN
C------ This is an EQ request
C------ Watch out for null length field!
      IF ( ILF(2) .EQ. 0 .AND. ILF(4) .EQ. 0 ) THEN
C------ Both null OK
      LIFDON = .TRUE.
      ELSEIF ( ILF(2) .EQ. 0 .OR. ILF(4) .EQ. 0 ) THEN
C------ Only one null
      GO TO 3450
      ELSEIF ( CLINE(IBF(2):IEF(2)) .EQ. CLINE(IBF(4):IEF(4)) ) THEN
C------ Yes found it let it process
      LIFDON = .TRUE.
C------ No didn't find it
      ELSE
C------ Look for an ELSEIF, or ELSE condition
      GO TO 3450
      ENDIF
C------
C
      ELSEIF ( CLINE(IBF(3):IEF(3)) .EQ. '.NE.' ) THEN
C------ This is an NE request
C------ Watch out for null length field!
      IF ( ILF(2) .EQ. 0 .AND. ILF(4) .EQ. 0 ) THEN
C------ Both null NG
      GO TO 3450
      ELSEIF ( ILF(2) .EQ. 0 .OR. ILF(4) .EQ. 0 ) THEN
C------ Only one null
      LIFDON = .TRUE.
      ELSEIF ( CLINE(IBF(2):IEF(2)) .NE. CLINE(IBF(4):IEF(4)) ) THEN
C------ Yes found it let it process
      LIFDON = .TRUE.
C------ No didn't find it
      ELSE
C------ Look for an ELSEIF, or ELSE condition
      GO TO 3450
      ENDIF
C------
      ELSE
C----- Come here when not an EQ or NE request, search for ENDIF
      GO TO 3650
      ENDIF
C------
      GO TO 8000
C
C------ Eat up lines looking for an ELSEIF or ELSE or ENDIF
C
 3450 CONTINUE
      IWASTE = 0
 3455 CONTINUE
C------
      IMACP = IMACP + 1
C     WRITE(*,*)' IMACP,IFLEVL,IWAS,LIFD-',IMACP,IFLEVL,IWASTE,LIFDON
C     WRITE(*,*) CLBUFM(IMACP)(1:30)
      IF ( IMACP .GT. IMXMCP ) THEN
C------ Macro buffer is EMPTY, should not ever happen, but it will!
      WRITE (IDSP, 3451 )
 3451 FORMAT(/' Macro line ELSEIF, ELSE, or ENDIF not found'/)
      IFLEVL = 0
      LIFDON = .TRUE.
      IMACP = IMXMCP
      GO TO 9000
      ELSE
      IF ( LIFDON ) GO TO 3650
C------ Look for !ELSEIF, !ELSE, !ENDIF or !IF so you can match IF-ENDIF
      IF ( CLBUFM(IMACP)(1:3) .EQ. CSPL(1:1)//'IF' ) THEN
      IWASTE = IWASTE + 1
      GO TO 3455
      ELSEIF ( CLBUFM(IMACP)(1:6) .EQ. CSPL(1:1)//'ENDIF' ) THEN
      IF ( IWASTE . GT. 0 ) THEN
      IWASTE = IWASTE - 1
      GO TO 3455
      ENDIF
      IMACP = IMACP - 1
      GO TO 8000
      ELSEIF ( CLBUFM(IMACP)(1:7) .EQ. CSPL(1:1)//'ELSEIF' ) THEN
      IF ( IWASTE .GT. 0 ) GO TO 3455
      IMACP = IMACP - 1
      GO TO 8000
      ELSEIF ( CLBUFM(IMACP)(1:5) .EQ. CSPL(1:1)//'ELSE' ) THEN
      IF ( IWASTE .GT. 0 ) GO TO 3455
      IMACP = IMACP - 1
      GO TO 8000
      ELSE
      GO TO 3455
      ENDIF
      ENDIF
C
C
C------ !ELSE
C
 3500 CONTINUE
      IF ( LIFDON ) GO TO 3650
      LIFDON = .TRUE.
      GO TO 8000
C------
C
C
C------ !ENDIF
C
 3600 CONTINUE
C     WRITE(*,*) ' IFLEVL, LIFDON @ ENDIF ', IFLEVL, LIFDON
      IFLEVL = IFLEVL - 1
      LIFDON = .TRUE.
C     WRITE(*,*) ' IFLEVL, LIFDON @ ENDIF ', IFLEVL, LIFDON
      IF ( IFLEVL .LT. 0 ) THEN
      IFLEVL = 0
      GO TO 9000
      ENDIF
C
      GO TO 8000
C
C----- Come here to eat macro lines searching for !ENDIF
C
 3650 CONTINUE
C
      IWASTE = 0
C
 3655 CONTINUE
C
      IMACP = IMACP + 1
C     WRITE(*,*)' IMACP,IFLEVL,IWAS,LIFD ',IMACP,IFLEVL,IWASTE,LIFDON
C     WRITE(*,*) CLBUFM(IMACP)(1:30)
C------
      IF ( IMACP .GT. IMXMCP ) THEN
C------ Macro buffer is EMPTY, should not ever happen, but it will!
      WRITE (IDSP, 3651 )
 3651 FORMAT(/' Macro line ENDIF not found'/)
      IFLEVL = 0
      LIFDON = .TRUE.
      IMACP = IMXMCP
      GO TO 9000
      ELSE
C------ Look for !ENDIF or !IF so you can pair up IF-ENDIF
C
      IF ( CLBUFM(IMACP)(1:3) .EQ. CSPL(1:1)//'IF' ) THEN
      IWASTE = IWASTE + 1
      GO TO 3655
      ELSEIF ( CLBUFM(IMACP)(1:6) .EQ. CSPL(1:1)//'ENDIF' ) THEN
      IF ( IWASTE .GT. 0 ) THEN
      IWASTE = IWASTE - 1
      GO TO 3655
      ENDIF
      IMACP = IMACP - 1
      GO TO 8000
      ELSE
      GO TO 3655
      ENDIF
      ENDIF
C
C
C
C   ***
C   ***
C   ** HERE FOR ALL OPCOM COMMANDS
C   ***
 4000 CONTINUE
C     CALL COPCOM( CLINE( (ILINEP+1):72 ), ISTOPC )                     H
      GO TO 8000
C   ***
C   ***
C   ** CHAIN AND RESET (OPEN) PFILES
C   ***
 5000 CONTINUE
C     WRITE (*,*) ' '                                                   Mlg
C     I = SYSTEM(CMESS(1:NMESS)//CHAR(0))                               M
C     CMESS(NMESS+1:NMESS+1) = CHAR(0)                                  lg
C     I = system (CMESS(1:NMESS+1))                                     lg
C     TURN OFF TABLET
C     IF (LMENU) CALL CHRWT (IKB, CTOFF, 3)                             H
CC    CALL CHAIN3(' ',1,CMESS,NMESS)
C     CALL CHAIN4(      CMESS      )                                    H
C     TURN ON TABLET
C     IF (LMENU) CALL CHRWT (IKB, CTON, 5)
C     CALL ROUTINE PITAB TO SET TABLET TYPE
C     CALL PITAB                                                        H
C     IF(CTRMTY.EQ.'ASY') THEN                                          H
C     CALL STTY(IDSP,'SET','PROMPT',CTPRM,IST)
C     ENDIF                                                             H
C     IF (I.NE.0) THEN                                                  M
C     WRITE (*,*) ' Unable to execute! '                                M
C     ELSE                                                              M
C     WRITE (*,*) ' Returned. '                                         M
C     ENDIF                                                             M
      GO TO 8000
C
 8000 CONTINUE
      RETURN
C   ***
C   ***
C   ** CHECK FOR REDFINING ESC CHAR
C   ***
 9000 CONTINUE
      WRITE(IDSP,9010)
 9010 FORMAT(' Invalid PREAD command')
      GO TO 8000
C
      END

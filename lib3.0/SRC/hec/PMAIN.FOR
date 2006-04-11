      SUBROUTINE PMAIN (ISCRT, CLIN1, CLIN2, IENTRY)
C
C        THIS IS THE MAIN ENTRY TO THE PREAD CAPABILITY.
C        TO USE THIS ROUTINE ALL READ STATEMENTS THAT ARE
C        TO BE AFFECTED MUST BE PRECEDED BY A CALL TO PREAD.
C        THE PARAMETER IN THE CALL MUST BE THE SAME UNIT
C        NUMBER AS APPEARS IN THE READ STATEMENT. THIS UNIT
C        SHOULD BE SET BY THE USER TO A VARIABLE CONTAINING
C        THE NUMBER OF THE NORMAL INPUT STREAM.  THIS VALUE
C        WILL BE ALTERED DURING THE FIRST CALL TO PREAD.
C
C        THE FOLLOWING FILES AND UNITS ARE THE DEFAULT:
C          ISCT=30    USED FOR UPDATING VARIOUS FILES AND MAY
C                     USED AS THE SCRATCH UNIT REQUIRED ABOVE.
C          IFUN=31   FUNCTION FILE   DEFAULT-GENFUN
C          IMAC=32   MACRO FILE      DEFAULT-GENMAC
C          IMEN=33   MENU FILE       DEFAULT-GENMEN
C          ILOG=34   LOG  FILE       DEFAULT-GENLOG
C
C
C          EXAMPLE:
C
C          -BEFORE USING PREAD-
C                      ......
C                      ......
C      READ(5,20)A,B,C
C                      ......
C                      ......
C
C          -AFTER ADDING PREAD-
C                      ......
C                      ......
C      INPUT=5
C      CALL ATTACH (INPUT, 'INPUT', STDIN,     ' ', CINFIL, ISTAT)
C      CALL ATTACH (IOUTP, 'OUTPUT, STDOUT,    ' ', COUFIL, ISTAT)
C
C      CALL PTTACH (30, 'SCRATCH', 'SCRATCH1', ' ', CSCRFL, ISTAT)
C      CALL PTTACH (31, 'FUNFILE', 'GENFUN',   ' ', CFUNFL, ISTAT)
C      CALL PTTACH (32, 'MACFILE', 'GENMAC',   ' ', CMACFL, ISTAT)
C      CALL PTTACH (33, 'MENFILE', 'GENMEN',   ' ', CMENFL, ISTAT)
C      CALL PTTACH (34, 'LOGFILE', 'GENLOG',   ' ', CLOGFL, ISTAT)
C      CALL ATTEND
C                      ......
C                      ......
C      CALL PREAD(INPUT)
C      READ(INPUT,20)A,B,C
C                      ......
C                      ......
C
C        NOTES:
C          -- AT THE END OF THE USER'S PROGRAM THE SUBROUTINE
C             'PEND' MUST BE CALLED.  PEND CLOSES ALL FILES
C             OPENED BY PREAD AND ALSO RESETS THE SYSTEM'S
C             PROMPT.
C          -- THE PROGRAMMER DOES NOT NEED TO OPEN THE FILES
C             ONCE THEY HAVE BEEN ASSIGNED USING THE ATTACH
C             CALL (ATTACH TAKES CARE OF OPENING THE FILES).
C          -- THE LAST CALL TO ATTACH SHOULD CONTAIN THE
C             STRING 'ENDATTACH' IN THE DEFAULT SETTING
C             POSITION.
C
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PERROR                                                           H
      INCLUDE 'perror.h'                                                MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
C
      COMMON /DIE/ DIE
      COMMON /PLSET/ LPSETP
      COMMON /GTLINE/ LBOOT, LFTIME
      LOGICAL LBOOT, LFTIME, LPSETP, DIE
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
C
      EXTERNAL PERR
C
C
      CHARACTER*20 CNAME, CREG*3
      CHARACTER CLIN1*(*), CLIN2*(*)
      LOGICAL LENTR2, LENTRC
C
      LOGICAL LFIRST,LPREAD,LEOF,LENTRY
      SAVE LFIRST
      DATA LFIRST,LPREAD/2*.TRUE./
C
C
C
      LENTRC = .FALSE.
      LENTRY = .FALSE.
      LENTR2 = .FALSE.
      CLINE = ' '
C
C     IF PATTACH WAS NEVER CALLED USE OLD FILE NUMBERS
C
      IF (.NOT.LPATT) THEN
      ISCT = 30
      CALL GETNAM (ISCT,CSCTFL,ISTAT)
      IFUN = 31
      CALL GETNAM (IFUN,CFUNFL,ISTAT)
      CLOSE (UNIT=IFUN)
      IMAC = 32
      CALL GETNAM (IMAC,CMACFL,ISTAT)
      LMACOP = .TRUE.
      IMEN = 33
      CALL GETNAM (IMEN,CMENFL,ISTAT)
      CLOSE (UNIT=IMEN)
      ILOG = 34
      CALL GETNAM (ILOG,CLOGFL,ISTAT)
      CLOSE (UNIT=ILOG)
      ISCN = 35
      CALL GETNAM (ISCN,CSCNFL,ISTAT)
      CLOSE (UNIT=ISCN)
      LPATT = .TRUE.
      ENDIF
C
C     Goto the proper locations for entry points
C
      IF (IENTRY.EQ.1) THEN
         LENTRY = .TRUE.
         CLINE = CLIN1
         GOTO 14
      ELSE IF (IENTRY.EQ.2) THEN
         LENTR2 = .TRUE.
         INCHR = LEN(CLIN1)
         CLINE = CLIN1
         GOTO 21
      ELSE IF (IENTRY.EQ.3) THEN
         LENTRC = .TRUE.
         IENTRY = LEN(CLIN1)
      ENDIF
C
      IF (ISCT.EQ.-1) THEN
C        WRITE (3,*) ' ERROR - PREAD NEEDS A SCRATCH UNIT'//            H
C    +  ' ATTACHED USING PTTACH'                                        H
         WRITE (6,*) ' ERROR - PREAD NEEDS A SCRATCH UNIT'//            MLlg
     +  ' ATTACHED USING PTTACH'                                        MLlg
         STOP
      ENDIF
      CALL CHRFIL( CTPRM(1:9),CHAR(0) )
      DIE = .FALSE.
C   ***
C   ***
C   ** CHECK FOR NEGATIVE ISCRT. IF YES CALL PLDFUN ONLY
C   ***
      IF (ISCRT.GE.0) GO TO 5
      CALL PLDFUN(ISTAT)
C     CREG='XXX'                                                        H
C     CALL GSTRRG ('SFN',CREG,NREG,ISTAT)                               H
C     IF(CREG(1:3).EQ.'OFF') THEN                                       H
C     LFUN = .FALSE.                                                    H
C     ELSE                                                              H
C     LFUN = .TRUE.                                                     H
C     ENDIF                                                             H
      RETURN
C   **********************
C   ***
C   ** CHECK IF FIRST TIME PREAD CALLED
C   ***
 5    IF(.NOT.LFIRST) GO TO 10
C     CALL PBLOCK                                                       H
C   ***
C   ***
C   ** CALL THE CONTIGENCY RETURN AND SAVE AND CHANGE SYSTEM PROMT
C   ** TO ALL NULLS
C   ***
C     CALL CRTN(PERR,1)                                                 H
C     May not need this call (is ctrmty used elsewhere?)  Bill Charley
C     CALL TRMTYP(IKB,CTRMTY)                                           H
C
C     CALL PITAB                                                        H
      IKB=ISCRT
      CALL VANSI(IDSP)                                                  MLlg
C     CALL VINIT (IDSP)                                                 H
      LFIRST=.FALSE.
C   ***
C   ***
C   ** CHECK IF THIS IS RUNNING FROM A FILE (IJ OR JS)
C   ***
      LKEYB = .FALSE.
      CALL GETNAM(IKB,CNAME,IFG)
      IF (CNAME(1:4).EQ.'CON ') LKEYB = .TRUE.                          ML
      IF (CNAME(1:5).EQ.'STDIN') LKEYB = .TRUE.                         L
C     IF (CNAME(1:5).EQ.'stdin') LKEYB = .TRUE.                         lg
C     IF (IFG.NE.0) LKEYB = .TRUE.                                      H
C   **********************
C   ***
C   ***
C   ** CHECK TO SEE IF USER IS ACCESSING PREAD'S CAPABILITIES
C   ***
   10 CONTINUE
      IF (.NOT.LPREAD) GO TO 800
      CALL PGTLIN (LEOF)
      IF (DIE) RETURN
      IF(LEOF) THEN
        CLINE(1:4) = 'EOF '
        GO TO 160
      ENDIF
C   ***
C   ** ENTRY PREAD1 FORCES PREAD TO ACT AS IF THE USER HAS TYPED
C   ** IN CLIN1
C   ***
   14 CONTINUE
C   ***
C   ** ENTRY PREAD2 EXPANDS FUNCTION CHARACTERS IN CLIN1,
C   ** THEN RETURNS EXPANDED LINE IN CLIN2
C   ***
   21 CONTINUE
C
C   ***
C   ***
C   ** SAVE LINE AND SET 'LINE TOO BIG' FLAG EQUAL TO FALSE.
C   ***
      CLINSV = CLINE
      LTOBIG = .FALSE.
      LOOP=0
  100 LOOP=LOOP+1
C   ***
C   ***
C   ** LIMIT SUBSD IN CASE OF RECURSION AND CHECK IF LINE HAS REACHED
C   ** THE POINT WHERE IT IS TOO BIG.
C   ***
      IF (LOOP.GT.50) GO TO 9000
      IF (LTOBIG) THEN
      WRITE(IDSP,110) 'WARNING: FUNCTION EXPANDED BEYOND BUFFER LENGTH'
      WRITE(IDSP,110) '         (LINE TRUNCATED TO 132 CHARACTERS)    '
  110 FORMAT(1X,A)
      GOTO 9000
      ENDIF
C   ***
C   ***
C   ** DO FUNCTION SUBSITIUTION ON LINE IF NECESSARY
C   ***
      CALL PEXLIN
      IF (DIE) RETURN
C   ***
C   ***
C   ** IF NO SUBS DONE LINE IS CLEAN SO GO ON
C   ***
      IF (LCLEAN) GO TO 9000
      GO TO 100
C   ***
C   ***
C   ** PASS LINE TO PROGRAM
C   ***
 9000 CONTINUE
C
      IF (LENTR2) THEN
         CLIN2 = CLINE
         RETURN
      ENDIF
C
      CALL CHRLNB(CLINE,NC)
      NC = NC+1
      IF (.NOT. LBECHO) GO TO 207
      IF (LECHO) WRITE(IDSP,*) CPROMT(1:NPROMT),CLINE(1:NC)             MLlg
      IF (LLOG) WRITE (ILOG,*) CPROMT(1:NPROMT),CLINE(1:NC)             MLlg
C     IF (LECHO) WRITE(IDSP,220) CPROMT(1:NPROMT)//CLINE(1:NC)          H
C     IF (LLOG) WRITE (ILOG,210) CPROMT(1:NPROMT)//CLINE(1:NC)          H
      GOTO 208
C     ECHO LINE UNLESS ECHO OFF, OR A PRINT COMMAND
 207  IF (LECHO.AND.(CLINE(1:3).NE.CSPL(1:1)//'PR'))                    MLlg
     *  WRITE(IDSP,*) CLINE(1:NC)                                       MLlg
      IF (LLOG) WRITE (ILOG,*) CLINE(1:NC)                              MLlg
C207  IF (LECHO.AND.(CLINE(1:3).NE.CSPL(1:1)//'PR'))                    H
C    *  WRITE(IDSP,220) CLINE(1:NC)                                     H
C     IF (LLOG) WRITE (ILOG,210) CLINE(1:NC)                            H
 208  CONTINUE
      LCMD = .FALSE.
      IF ((CLINE(1:1).EQ.CSPL(1:1)).OR.LPAUZ.OR.LTEACH) CALL PCDLIN
      IF (DIE) RETURN
      IF(.NOT.LLEARN) GO TO 150
      IF ( LCMD .AND. ((IMATCH .EQ. 2) .OR. (IMATCH .EQ. 21)) ) GOTO 150
      IF ( LCMD .AND. ((IMATCH .EQ. 1) .OR. (IMATCH .EQ. 20)) ) GOTO 299
      IF(LMACRO) GO TO 150
 299  CALL WIND(IMAC)
      CALL CHRLNB(CLINSV,NC)
      WRITE(IMAC,300) CLINSV(1:NC+1)
 300  FORMAT(A)
 150  IF (LENTRY .AND. LCMD) RETURN
      IF(LCMD) GO TO 10
C
 160  CONTINUE
      IF (LENTRC) THEN
      CALL CHRLNB(CLINE,NC)
      CLIN1(1:IENTRY) = ' '
      IF (NC.GT.IENTRY) THEN
      CLIN1(1:IENTRY) = CLINE(1:IENTRY)
      IENTRY = -2
      ELSE IF (NC.EQ.0) THEN
      IENTRY = 0
      ELSE IF (CLINE(1:4).EQ.'EOF ') THEN
      IENTRY = -1
      ELSE
      IENTRY = NC
      CLIN1(1:NC) = CLINE(1:NC)
      ENDIF
      ELSE
      ISCRT=ISCT
      REWIND ISCRT
      CALL CHRLNB(CLINE,NC)
      IF (CLINE(1:4) .NE. 'EOF ') WRITE(ISCRT,210) CLINE(1:NC+1)
      ENDFILE ISCRT
      REWIND ISCRT
      ENDIF
 220  FORMAT (1X,A)
 210  FORMAT (A)
      RETURN
C   ***
C   ***
C   ** USER NOT ACCESSING PREAD CAPABILITIES
C   ***
 800  CONTINUE
      IF (.NOT.LKEYB) READ(IKB,300)CLINE
      IF (LKEYB) THEN
        INCHR = 132
        CALL ANREAD(IKB,CPROMT,NPROMT,CLINE,INCHR)
        CLINE(INCHR+1:) = ' '
      END IF
      IF (CLINE(1:1) .NE. CSPL(1:1)) GO TO 160
C
      IF (CLINE(2:4) .EQ. 'JCL') GO TO 830
      WRITE(6,810)
 810  FORMAT(' THE PREAD MODE HAS BEEN SET IN AN INOPERATIVE STATUS!')
      GO TO 800
C
 830  CONTINUE
      LCMD = .FALSE.
      IF ((CLINE(1:1).EQ.CSPL(1:1)).OR.LPAUZ.OR.LTEACH) CALL PCDLIN
      IF (DIE) RETURN
      GO TO 800
C
      END
C     SUBROUTINE PBLOCK                                                 H
      BLOCK DATA PBLOCK                                                 PMLlg
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
CADD C.PLINE                                                            H
      INCLUDE 'pline.h'                                                 MLlg
C
      COMMON /DIE/ DIE
      COMMON /PLSET/ LPSETP
      COMMON /GTLINE/ LBOOT, LFTIME
      LOGICAL LBOOT, LFTIME, LPSETP, DIE
C
      COMMON /PNTYP/ NTYPAH, LTYPAH
      LOGICAL LTYPAH
C
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
C
        DATA LRUN,LMACRO,LMENU,LMINUS,LKBLIN /5*.FALSE./
        DATA LECHO,LLOG /.FALSE.,.FALSE./
      DATA LPAUZ, LTEACH /2*.FALSE./
        DATA LEXPAN,LFILE,LFUN,LCLEAN,LLEARN,LCMD,LSCN /7*.FALSE./
C       DATA IKB,IKBA /0,0/                                             H
        DATA IKB,IKBA /5,5/                                             MLlg
        DATA IIN,IDSP,IDSPA /5,6,6/                                     MLlg
C       DATA IIN,IDSP,IDSPA /0,3,3/                                     H
        DATA IPU /7/
        DATA ILP /6/
        DATA IDIG /3/
        DATA IPLOT /22/
        DATA IMACP /IMXMCP/
        DATA INWDS,INCHR/33,132/
        DATA IMXNCH /MAXCHR/
        DATA ISCT,IFUN,IMAC,IMEN,ILOG,ISCN,IHLP /7*-1/
C       DATA CHLPFL /'SYS*PRDMES  '/                                    H
        DATA CHLPFL /'PRDMES.HLP'/                                      MLlg
        DATA LMACOP /.FALSE./
       DATA LPATT /.FALSE./
        DATA LBECHO /.FALSE./
        DATA NPROMT /3/
        DATA CPROMT
     +  /'E>                                                          '/
        DATA JUMP /0/
        DATA CSPL /'! -^'/
        DATA LBOOT/.FALSE./,LFTIME/.TRUE./
        DATA LPSETP/.FALSE./
C     In the following list, the 'XX' are for any future commands
        DATA CLIST/'RUN','LEARN','ECHO','menu','KEYBOARD','hardcopy',   MLlg
C       DATA CLIST/'RUN','LEARN','ECHO','MENU','KEYBOARD','HARDCOPY',   H
     & 'FUNCTION','JCL','COED','LOG','TEACH','?','CHAIN','STATUS',
     & 'PAGE','WAIT','PAUSE','/','*','MACRO','EDIT','SCREEN','KBLINE',
     & 'PRINT','DISPLAY','LIST','FIND','IJOB','INPUT','XX','SET','GET',
     & 'IF','ELSEIF','ELSEDO','ENDIF'/
      DATA ILNCM/3,5,4,4,8,8,8,3,4,3,5,1,5,6,4,4,5,1,1,5,4,6,6,5,7,4,4,
     * 4,5,2,3,3,2,6,6,5/
        DATA NLIST/NCMD/
        DATA ILENG/8/
        DATA CARROW /'<==='/
        DATA KKEY/50/
        DATA CTRMTY /'???'/
      DATA LTYPAH /.FALSE./
      DATA LMOV /.TRUE./
      DATA LPMT /.TRUE./
      DATA LAUTO /.FALSE./
C
C     RETURN                                                            H
      END

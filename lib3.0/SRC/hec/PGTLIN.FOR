      SUBROUTINE PGTLIN (LEOF)
C
C        SUBROUTINE 'PGTLIN' GETS A LINE OF DATA THAT WILL
C        BECOME A LINE OF INPUT FOR THE USER'S NORMAL READ
C        STATEMENT.  THE LINE OF DATA CAN BE OBTAINED FROM
C        ONE OF THREE DIFFERENT SOURCES - A MACRO, MENU OR
C        SIMPLY FROM THE KEYBOARD.  IF THE END OF THE FILE
C        HAS BEEN REACHED, THE VARIABLE 'LEOF' IS SET TO
C        TRUE AND IS SENT BACK TO THE CALLING PROGRAM.
C
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PMENU                                                            H
      INCLUDE 'pmenu.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
CADD C.PCCDEF                                                           H
      INCLUDE 'pccdef.h'                                                MLlg
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
      COMMON /DIE/ DIE
      COMMON /PLSET/ LPSETP
      COMMON /GTLINE/ LBOOT, LFTIME
      LOGICAL LBOOT, LFTIME, LPSETP, DIE
C
C
      CHARACTER*40 CNAME, CREG*3
C
      LOGICAL LEOF, LRESP,LFIRST
      SAVE LFIRST
C
      DATA LFIRST /.TRUE./
C
C   ***
C   ***
C   ** ROUTINE TO GET NEW LINE FROM DESIRED DEVICE
C   ***
      IF (.NOT.LFIRST) GO TO 10
      IF (.NOT. LPSETP) CPROMT = CHAR(10)//'I>'
      LFIRST=.FALSE.
C   ***
C   ***
C   ** CHECK IF THIS IS RUNNING FROM A FILE (IJ OR JS)
C   ***
      LKEYB = .FALSE.
      CALL GETNAM (IKB,CNAME,IFG)
      IF (CNAME(1:4).EQ.'CON ') LKEYB = .TRUE.                          ML
      IF (CNAME(1:5).EQ.'STDIN') LKEYB = .TRUE.                         L
C     IF (CNAME(1:5).EQ.'stdin') LKEYB = .TRUE.                         lg
C     IF (IFG.NE.0) LKEYB = .TRUE.                                      H
C   ***
C   ***
C   ** LOAD FUNCTIONS FIRST
C   ***
      IF (LLOG) CALL WIND (ILOG)
      IF (.NOT. LMACOP) THEN
          IF (IMAC.EQ.-1) GOTO 4
          IF (CMACFL(1:2).EQ.'  ') GO TO 4
          OPEN (UNIT=IMAC,FILE=CMACFL,STATUS='OLD',IOSTAT=ISTAT,ERR=4)
          LMACOP = .TRUE.
      ENDIF
      REWIND IMAC
      READ (IMAC,1,END=4) CLINE
    1 FORMAT (A)
      IF (CLINE(1:3) .EQ. 'CC ') THEN
        I = NINDX( CLINE(4:132), ' ' ) + 3
        IF ((I-3) .GT. 0) CSPL(1:1) = CLINE(I:I)
        READ (IMAC,1,END = 4) CLINE
      END IF
      IF (CLINE(1:10) .EQ. 'BOOTSTRAP ') LBOOT = .TRUE.
 4    CALL PLDFUN(ISTAT)
C     CREG='XXX'                                                        H
C     CALL GSTRRG ('SFN',CREG,NREG,ISTAT)                               H
C     IF(CREG(1:3).EQ.'OFF') THEN                                       H
C     LFUN = .FALSE.                                                    H
C     ELSE                                                              H
C     LFUN = .TRUE.                                                     H
C     ENDIF                                                             H
      IF (LBOOT) THEN
        READ (IMAC,1,END = 10) CLINE
        RETURN
      END IF
 10   CONTINUE
      INCHR = 132
C
C     GIVE PRIORITY TO SCREENS OVER MACROS, SO THAT A MACRO
C     WILL HOLD FOR A SCREEN RESPONSE
C
      IF(LSCN) THEN
      CALL PSLSCN  (LRESP)
      IF(LRESP) THEN
C     CHECK IF GOT A RESPONSE FOR SCREEN, IF NOT GO TO MACRO BUFFER
C     THIS ALLOWS THE DISPLAY OF SCREENS WITHOUT A REPLY!
      GO TO 1000
      ENDIF
      ENDIF
C
C     GIVE PRIORITY TO "KBLINE" A ONE LINE RESPONSE FROM THE KEYBOARD
C     WHILE A MACRO IS RUNNING - THE MACRO CONTINUES AFTER THE RESPONSE
      IF(LKBLIN) THEN
      LKBLIN = .FALSE.
      GO TO 150
      ENDIF
C
      IF (IMACP .EQ. IMXMCP) THEN
        LMACRO=.FALSE.
        IF(JUMP.EQ.0) GO TO 100
C       CALL CNTRLX(-4)                                                 H
C       CALL CNTRLX(JUMP)                                               H
        JUMP=0
      ENDIF
C   ***
C   ***
C   ** GET MACRO LINE FROM BUFFER
C   ***
      IF (LMACRO) THEN
        CLINE = CLBUFM(IMACP+1)
        IMACP=IMACP+1
        GO TO 1000
      ENDIF
C   ***
C   ***
C   ** GET MENU RESPONSE
C   ***
 100  CONTINUE
C     IF (LMENU) THEN                                                   H
C     INCHR = 132                                                       H
C     CALL PANRED(IKB,CPROMT,NPROMT,CLINE,INCHR)                        H
C       IF (DIE) RETURN                                                 H
C       GO TO 1000                                                      H
C     ENDIF                                                             H
C   ***
C   ***
C   ** MUST BE FROM KEYBOARD
C
  150 CONTINUE
C   ***
      IF (.NOT.LKEYB) THEN
        READ(IKB,1,END=200) CLINE
      ELSE
        INCHR = 132
C ------ SET PROMPT VISIBILITY AND POSITION
        IF ( LMOV ) THEN
        NP = NPROMT
        IF ( .NOT. LPMT ) NP = 0
        ELSE
        NP = -NPROMT
        IF ( .NOT. LPMT ) NP = -99
        ENDIF
        LPMT = .TRUE.
        LMOV = .TRUE.
        CALL ANREAD(IKB,CPROMT,NP,CLINE,INCHR)
        IF (INCHR.LT.0) THEN
           IF (INCHR.EQ.-1) GO TO 200
           IF (INCHR.EQ.-2) INCHR = LEN(CLINE)
           IF (INCHR.EQ.-3) THEN
              CLINE = CSPL(1:1) // 'RUN #INACT'
              INCHR = 11
           ELSE IF (INCHR.EQ.-4) THEN
              CLINE = CSPL(1:1) // 'RUN #TIMEOUT'
              INCHR = 13
           ENDIF
        ENDIF
        IF (INCHR.LT.LEN(CLINE)) CLINE(INCHR+1:) = ' '
      ENDIF
      CKBLIN = CLINE
      GOTO 1000
 200  CONTINUE
C   ***
C   ***
C   ** SET EOF CONDITION  ($EOF)
C   ***
      LEOF=.TRUE.
      RETURN
C   ***
C   ***
C   ** GOT LINE, RETURN
C   ***
 1000 CONTINUE
      LEOF=.FALSE.
      RETURN
      END

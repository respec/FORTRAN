      SUBROUTINE PINQIR (CINSTR, CPRAM, NPRAM)
C
C        SUBROUTINE 'PINQIR' RECALLS THE VARIOUS FLAGS PREAD
C        USES.  THE NAME OF THE FLAG IS SENT, ALONG WITH
C        THE VALUE THE FLAG IS TO BE SET TO, AND THE NUMBER
C        OF CHARACTERS IN THE INSTRUCTION.
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
      COMMON /PLSET/LPSETP
C
      CHARACTER CINSTR*(*), CPRAM*(*)
      CHARACTER CSTR*10
      LOGICAL LPSETP
C
C
C
      IF (LEN(CINSTR).GE.3) THEN
      IF (CINSTR(1:4) .EQ. 'LOGF') GO TO 100
      IF (CINSTR(1:4) .EQ. 'LOGN') GO TO 200
      IF (CINSTR(1:4) .EQ. 'ECHO') GO TO 300
      IF (CINSTR(1:4) .EQ. 'PROM') GO TO 400
      IF (CINSTR(1:4) .EQ. 'BECH') GO TO 500
      IF (CINSTR(1:4) .EQ. 'TERM') GO TO 600
      IF (CINSTR(1:4) .EQ. 'FUNC') GO TO 700
      IF (CINSTR(1:4) .EQ. 'MACR' .OR. CINSTR(1:3).EQ.'RUN') GO TO 800
      IF (CINSTR(1:4) .EQ. 'MENU') GO TO 900
      IF (CINSTR(1:4) .EQ. 'LEAR') GO TO 1000
      IF (CINSTR(1:4) .EQ. 'SCRE') GO TO 1100
      IF (CINSTR(1:4) .EQ. 'KBLI') GO TO 1200
      IF (CINSTR(1:4) .EQ. 'CCOM') GO TO 1300
      IF (CINSTR(1:4) .EQ. 'BOOT') GO TO 1400
      ENDIF
C
C
 5    WRITE(IDSP,10) CINSTR
 10   FORMAT(' PSET - ILLEGAL PARAMETER ',A)
      RETURN
C
C
 100  IF (LLOG) GOTO 8000
      GOTO 9000
C
C
 200  CONTINUE
      NPRAM = ILOG
      RETURN
C
C
 300  CONTINUE
      IF (LECHO) GOTO 8000
      GOTO 9000
C
C
 400  CONTINUE
      NPRAM = NPROMT
      K = LEN(CPRAM)
      IF (NPRAM.LT.K) K = NPRAM
      CPRAM = CPROMT(1:K)
      RETURN
C
 500  CONTINUE
      IF (LBECHO) GOTO 8000
      GOTO 9000
C
C
 600  CONTINUE
      IF (CTRMTY.EQ. '???') THEN
C        CALL TRMTYP (0,CTRMTY)                                         H
      ENDIF
      NPRAM = 3
C
C
 700  CONTINUE
      IF (LFUN) GOTO 8000
      GOTO 9000
C
C
 800  CONTINUE
      IF (LMACRO .OR. LRUN) GOTO 8000
      GOTO 9000
C
C
 900  CONTINUE
      IF (LMENU) GOTO 8000
      GOTO 9000
C
C
 1000 CONTINUE
      IF (LLEARN) GOTO 8000
      GOTO 9000
C
C
 1100 CONTINUE
      IF (LSCN) GOTO 8000
      GOTO 9000
C
C
 1200 CONTINUE
      IF (LKBLIN) GOTO 8000
      GOTO 9000
C
C
C
 1300 CONTINUE
      CPRAM(1:1) = CSPL(1:1)
      RETURN
C
C
C     Check to see if the macro file begins with a bootstrap
 1400 CONTINUE
      IF (.NOT. LMACOP) THEN
      IF (IMAC.EQ.-1) GO TO 9000
      IF (CMACFL(1:2).EQ.'  ') GO TO 9000
      OPEN (UNIT=IMAC,FILE=CMACFL,STATUS='OLD',IOSTAT=ISTAT,ERR=9000)
      IF (ISTAT.NE.0) GO TO 9000
      LMACOP = .TRUE.
      ENDIF
      REWIND IMAC
      READ (IMAC,1410,END=9000) CSTR
 1410 FORMAT (A)
      IF (CSTR(1:9).EQ.'BOOTSTRAP') GO TO 8000
      GO TO 9000
C
C
 8000 CONTINUE
      CPRAM = 'ON'
      NPRAM = 2
      RETURN
C
C
 9000 CONTINUE
      CPRAM = 'OFF'
      NPRAM = 3
      RETURN
C
      END

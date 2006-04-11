      SUBROUTINE PSET (CINSTR, CPRAM, NPRAM)
C
C        SUBROUTINE 'PSET' SETS THE VARIOUS FLAGS THAT PREAD
C        USES.  THE NAME OF THE FLAG IS SENT, ALONG WITH
C        THE VALUE THE FLAG IS TO BE SET TO, AND THE NUMBER
C        OF CHARACTERS IN THE INSTRUCTION.  THE PROMT CAN
C        BE CHANGED BY SENDING "PROM" IN 'CINSTR', AND THE
C        NEW PROMT IN 'CPRAM'.
C
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
C
C
      COMMON /PLSET/LPSETP
C
      CHARACTER CINSTR*(*), CPRAM*(*)
      CHARACTER CNAME*40
      LOGICAL LPSETP,LEXIST
C
C
C
      IF (LEN(CINSTR).GE.4) THEN
      IF (CINSTR(1:4) .EQ. 'LOGF') GO TO 100
      IF (CINSTR(1:4) .EQ. 'LOGN') GO TO 200
      IF (CINSTR(1:4) .EQ. 'ECHO') GO TO 300
      IF (CINSTR(1:4) .EQ. 'PROM') GO TO 400
      IF (CINSTR(1:4) .EQ. 'BECH') GO TO 500
      IF (CINSTR(1:4) .EQ. 'INPU') GO TO 600
      IF (CINSTR(1:4) .EQ. 'UNIT') GO TO 600
      IF (CINSTR(1:4) .EQ. 'CCOM') GO TO 700
      ENDIF
C
C
 5    WRITE(IDSP,10)CINSTR,CPRAM,NPRAM
 10   FORMAT(' PSET - Illegal parameter ',A,2X,A,2X,I8)
      RETURN
C
 100  LLOG = .FALSE.
      IF (ILOG.EQ.-1) RETURN
      IF (CPRAM(1:2) .EQ. 'ON') THEN
         LLOG = .TRUE.
C        INQUIRE(FILE=CLOGFL,EXIST=LEXIST)                              H
C        IF (.NOT.LEXIST) THEN                                          H
C           CALL CCREAT(CLOGFL,0,0,0,IERR)                              H
C           CALL CRETYP(CLOGFL,'00000164,0,IERR)                        H
C           WRITE (IDSP,11) CLOGFL                                      H
C11         FORMAT (' FILE GENERATED: ',A)                              H
C        ENDIF                                                          H
         OPEN (UNIT=ILOG,FILE=CLOGFL)
         CALL WIND (ILOG)
      ENDIF
      RETURN
C
 200  ILOG = NPRAM
      RETURN
C
 300  LECHO = .FALSE.
      IF (CPRAM(1:2) .EQ. 'ON') LECHO = .TRUE.
      RETURN
C   ***
C   ***
C   ** IF THE USER SETS 'CINSTR' TO "PROM" THE PROMPT CAN BE
C   ** CHANGED.  THE NEW PROMPT IS STORED IN 'CPRAM' AND
C   ** CAN BE UP TO 60 CHARACTERS LONG.
C   ***
 400  CONTINUE
      IF (NPRAM.GT.0) THEN
      LPSETP = .TRUE.
      IF (NPRAM.GT.60) GO TO 5
      NPROMT = NPRAM
      CPROMT = CPRAM
      ELSE
      LPSETP = .FALSE.
      NPROMT = 0
      ENDIF
      RETURN
C
 500  CONTINUE
      LBECHO = .FALSE.
      IF (CPRAM(1:2) .EQ. 'ON') LBECHO = .TRUE.
      LECHO = .FALSE.
      IF (LBECHO) LECHO = .TRUE.
      RETURN
C
 600  CONTINUE
      IKB = NPRAM
      LKEYB = .FALSE.
      CALL GETNAM (IKB,CNAME,IFG)
      IF (CNAME(1:4).EQ.'CON ') LKEYB = .TRUE.                          ML
      IF (CNAME(1:5).EQ.'STDIN') LKEYB = .TRUE.                         lgL
C     IF (CNAME(1:5).EQ.'stdin') LKEYB = .TRUE.                         lg
C     IF (IFG.NE.0) LKEYB = .TRUE.                                      H
      RETURN
C
 700  CONTINUE
      CSPL(1:1) = CPRAM(1:1)
      RETURN
C
      END

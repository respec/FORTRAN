C     SUBROUTINE ZINQI6 ( IFLTAB, CFLG, CALPHA, INUMB)                  H
      SUBROUTINE ZINQIR ( IFLTAB, CFLG, CALPHA, INUMB)                  MLu
C
C
C     Inquire about settings in a DSS file or a record
C
C     Written by Bill Charley at HEC, 1988.
C
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*)                                               ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER*4 INUMB                                                   ML
      CHARACTER CFLG*(*), CALPHA*(*)
      CHARACTER CTEMP*128, CFLAG*4
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
      COMMON /WORDS/ IWORD(10)
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
C
C
C
      IF (LFIRST) THEN
      CALL ZINIT
      LFIRST = .FALSE.
      ENDIF
C
      CFLAG = CFLG
C
      IF (MLEVEL.GE.15) WRITE (MUNIT,20) CFLAG                          D
 20   FORMAT (T10,'-----DSS---Debug:  Enter ZINQIR;  Flag: -',A,'-')    D
C
C
C     UNIT: UNIT NUMBER OF DSS FILE
      IF (CFLAG.EQ.'UNIT') THEN
      INUMB = IFLTAB(KUNIT)
C
C     MLEVEL: MESSAGE LEVEL
      ELSE IF ((CFLAG.EQ.'MLEV').OR.(CFLAG.EQ.'MLVL')) THEN
      INUMB = MLEVEL
C
C     PREC: Data precision of last record read
      ELSE IF (CFLAG.EQ.'PREC') THEN
      IF (INFO(KIFLAG).EQ.NPFLAG) THEN
      INUMB = INFO(NPPWRD+KIPREC)
      IF ((INUMB.LT.0).OR.(INUMB.GT.8)) INUMB = 0
      ELSE
      INUMB = 0
      ENDIF
C
C     MUNIT: MESSAGE UNIT
      ELSE IF ((CFLAG.EQ.'MUNI').OR.(CFLAG.EQ.'MLFN')) THEN
      INUMB = MUNIT
C
C     NAME: NAME OF DSS FILE
      ELSE IF (CFLAG.EQ.'NAME') THEN
      IUNIT = IFLTAB(KUNIT)                                             HL
      CALL GETNAM ( IUNIT, CALPHA, INUMB)                               HL
C     CTEMP = ' '                                                       Mu
C     CALL HOL2CH (IFLTAB(KNAME), CTEMP, 16)                            Mlgms
C     CALL HOL2CH (IFLTAB(KNAME), CTEMP, 8)                             c
C     CALPHA = CTEMP                                                    Mu
C
C     RDATE: RECORD DATE
      ELSE IF (CFLAG.EQ.'RDAT') THEN
      CTEMP = ' '
      CALL HOLCHR (INFO(NPPWRD+KIDATE), 1, NDATEC, CTEMP, 1)
      CALPHA = CTEMP
C
C     RTIME: RECORD TIME
      ELSE IF (CFLAG.EQ.'RTIM') THEN
      CTEMP = ' '
      CALL HOLCHR (INFO(NPPWRD+KITIME), 1, NTIMEC, CTEMP, 1)
      CALPHA = CTEMP
C
C     PROG: PROGRAM
      ELSE IF (CFLAG.EQ.'PROG') THEN
      CTEMP = ' '
      CALL HOLCHR (INFO(NPPWRD+KIPROG), 1, NPROGC, CTEMP, 1)
      CALPHA = CTEMP
C
C     RVERS: RECORD VERSION
      ELSE IF (CFLAG.EQ.'RVER') THEN
      INUMB = INFO(NPPWRD+KIVER)
C
C
C     FLAGS: RECORD DATA FLAGS STORED
      ELSE IF ((CFLAG.EQ.'FLAG').OR.(CFLAG.EQ.'QUAL')) THEN
      INUMB = INFO(NPPWRD+KIQUAL)
C
C     FDATE: FILE DATE (CREATION DATE)
      ELSE IF (CFLAG.EQ.'FDAT') THEN
      CTEMP = ' '
      CALL HOLCHR (IFLTAB(KCREAT), 1, NDATEC, CTEMP, 1)
      CALPHA = CTEMP
C
C     FVERS: FILE VERSION
      ELSE IF (CFLAG.EQ.'FVER') THEN
      CTEMP = ' '
      CALL HOLCHR (IFLTAB(KVERS), 1, NVERSC, CTEMP, 1)
      CALPHA = CTEMP
C
C     TABLE:  Dynamic or Stable Hash Table
      ELSE IF (CFLAG.EQ.'TABL') THEN
      IF (IFLTAB(KTABLE).EQ.1) THEN
      CALPHA = 'DYNAMIC'
      ELSE
      CALPHA = 'STABLE'
      ENDIF
C
      ELSE IF (CFLAG.EQ.'HSIZ') THEN
      INUMB = IFLTAB(KHSIZE)
C
C     NREC: NUMBER OF RECORDS
      ELSE IF (CFLAG.EQ.'NREC') THEN
      INUMB = IFLTAB(KNRECS)
C
C     SIZE: FILE SIZE (IN KBYTES)
      ELSE IF (CFLAG.EQ.'SIZE') THEN
C     DETERMINE THE NUMBER OF WORDS IN THE FILE
      FILSIZ = REAL(IFLTAB(KFSIZE)) -1.
      FILSIZ = FILSIZ * (512./508.)                                     ML
C     GET THE NUMBER OF KILOBYTES
      FILSIZ = (FILSIZ/1000.) * REAL(IWORD(2))
      INUMB = FILSIZ
C
C     DEAD: PRECENTAGE DEAD SPACE
      ELSE IF (CFLAG.EQ.'DEAD') THEN
      DEAD = REAL(IFLTAB(KDEAD))
      FILSIZ = REAL(IFLTAB(KFSIZE)) -1.
      DEADS = (DEAD/FILSIZ) * 100.
      INUMB = DEADS
C
C     VERSION: DSS SOFTWARE VERSION
      ELSE IF (CFLAG.EQ.'VERS') THEN
      CALPHA = CVERS
C
C     TYPE:  DATA TYPE (E.G. PARIED DATA)
      ELSE IF (CFLAG.EQ.'TYPE') THEN
      IDTYPE = IFLTAB(KDTYPE)
      INUMB = IDTYPE
      IF (IDTYPE.EQ.100) THEN
      CALPHA = 'RTS'
      ELSE IF (IDTYPE.EQ.110) THEN
      CALPHA = 'ITS'
      ELSE IF (IDTYPE.EQ.200) THEN
      CALPHA = 'PD '
      ELSE IF (IDTYPE.EQ.300) THEN
      CALPHA = 'TXT'
      ELSE
      CALPHA = 'UND'
      ENDIF
C
C     80COL:  80 COLUMN OUTPUT ON OR OFF (VS. 132 COL)
      ELSE IF (CFLAG.EQ.'80CO') THEN
      IF (L80COL) THEN
      CALPHA = 'ON'
      ELSE
      CALPHA = 'OFF'
      ENDIF
C
C     TAG:  TAG OF LAST RECORD ACCESSED
      ELSE IF (CFLAG(1:3).EQ.'TAG') THEN
      CTEMP = ' '
      IF ((JPNBIN.GE.1).AND.(NPPWRD.GT.1)) THEN
      CALL HOLCHR (IPNBIN(JPNBIN+NPPWRD+KBTAG), 1, NTAGC, CTEMP, 1)
      ENDIF
      CALPHA = CTEMP
C
C     Read Only Access
      ELSE IF (CFLAG.EQ.'READ') THEN
      IF (IFLTAB(KREADO).EQ.1) THEN
      CALPHA = 'ON'
      ELSE
      CALPHA = 'OFF'
      ENDIF
C
C
      ELSE
      IF (MLEVEL.GE.4) WRITE (MUNIT,80) CFLG
 80   FORMAT(' ----ZINQIR - Unrecognized Inquire: ',A)
      INUMB = -1
      CALPHA = ' '
      ENDIF
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.15) THEN                                            D
      CALL CHRLNB (CALPHA, N)                                           D
      IF (N.EQ.0) N = 1                                                 D
      IF (MLEVEL.GE.15) WRITE (MUNIT,820) INUMB, CALPHA(1:N)            D
 820  FORMAT (T10,'-----DSS---Debug:  Exit ZINQIR',/,                   D
     * T10,'Number:',I8,',  Character: ',A)                             D
      ENDIF                                                             D
      RETURN
C
      END

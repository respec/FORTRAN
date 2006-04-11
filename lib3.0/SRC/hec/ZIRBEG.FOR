      SUBROUTINE ZIRBEG (IFLTAB, JUL, CE, IYR, IMON, IDAY, IBLOCK,
     * MINBLK, INCBLK)
C
C     Get the beginning of an irregular block for irregular
C     interval time series data
C
C     GIVEN THE STARTING TIME AND THE TIME INTL, DETERMINE
C     THE STANDARD START DATE AND BLOCK LENGTH
C
C     JUL  - JULIAN DATE SINCE 31DEC1899
C     CE - E PART OF PATHNAME
C
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*)                                               ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER*4 JUL                                                     ML
      CHARACTER CE*(*), CINTLS(5)*10
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
      DATA CINTLS / 'IR-DAY    ', 'IR-MONTH  ',
     * 'IR-YEAR   ', 'IR-DECADE ', 'IR-CENTURY'/
C
C
      I = JLIYMD (JUL, IYR, IMON, IDAY)
C
      DO 20 I=1,5
      IF (CE(1:6).EQ.CINTLS(I)(1:6)) THEN
      IBLOCK = I
      GO TO 40
      ENDIF
 20   CONTINUE
      IBLOCK = -1
      GO TO 800
C
 40   CONTINUE
      INCBLK = IFLTAB(KITSIN)
C
C     USE DAILY BLOCK
      IF (IBLOCK.EQ.1) THEN
      MINBLK = IFLTAB(KITSDA)
C
C     USE MONTHLY BLOCK - BACKUP TO FIRST DAY OF MONTH
      ELSE IF (IBLOCK.EQ.2) THEN
      IDAY = 1
      MINBLK = IFLTAB(KITSMO)
C
C     USE YEARLY BLOCK - BACKUP TO FIRST DAY OF YEAR
      ELSE IF (IBLOCK.EQ.3) THEN
      IDAY = 1
      IMON = 1
      MINBLK = IFLTAB(KITSYE)
C
C     USE DECADE BLOCK - BACKUP TO FIRST DAY OF DECADE
      ELSE IF (IBLOCK.EQ.4) THEN
      IDAY = 1
      IMON = 1
      IYR = (IYR/10)*10
      MINBLK = IFLTAB(KITSDE)
C
C     USE CENTURY BLOCK - BACKUP TO FIRST DAY OF CENTURY
      ELSE IF (IBLOCK.EQ.5) THEN
      IDAY = 1
      IMON = 1
      IYR = (IYR/100)*100
      MINBLK = IFLTAB(KITSCE)
C
      ENDIF
C
C
 800  CONTINUE
      RETURN
C
      END

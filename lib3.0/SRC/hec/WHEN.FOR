      SUBROUTINE WHEN (CDAT, CTIM)
C
C       This subroutine, when called, returns the current date and
C  time.  The format is shown in the following example:
C            Date: '05MAR85 '
C            Time: '07:35:10'
C
C       Output:  CDAT  (--- Date)
C                CTIM  (--- Time)
C
C
      CHARACTER CDAT*(*), CTIM*(*)
      CHARACTER CTEMP*10                                                MLlg
C     INTEGER IHOL(3)                                                   H
C
C---------------------------------------------------------------------
C  The following is harris machine dependent.
C---------------------------------------------------------------------
C
C     CALL DATE (IHOL)                                                  H
C
C     CALL HOLCHR ( IHOL, 1, 2, CDAT, 1)                                H
C     CALL HOLCHR ( IHOL, 4, 3, CDAT, 3)                                H
C     CALL HOLCHR ( IHOL, 8, 2, CDAT, 6)                                H
C
C     CALL TIME (IHOL)                                                  H
C
C     CALL HOLCHR ( IHOL, 2, 8, CTIM, 1)                                H
C
C     IF FIRST CHAR OF DATE OR TIME IS BLANK, MAKE IT A ZERO
C     IF (CDAT(1:1).EQ.' ') CDAT(1:1) = '0'                             H
C     IF (CTIM(1:1).EQ.' ') CTIM(1:1) = '0'                             H
C
C  These are dummy values for a non-harris or non-hp system.
C
C     CDAT(1:8) = '01JAN85 '                                            D
C     CTIM(1:8) = '12:34:56'                                            D
C
      CDAT = ' '                                                        MLlg
      CTIM = ' '                                                        MLlg
      CALL CTIME (CTIM)                                                 MLlg
      CALL CDATE (CTEMP)                                                MLlg
      CALL REMBLK (CTEMP(1:10), CDAT, ITMP)                             MLlg
C
      RETURN
      END

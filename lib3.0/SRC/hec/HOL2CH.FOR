      SUBROUTINE HOL2CH ( IHOL, CHR, NWORDS)
C
C     TRANSFER HOLLERITH IHOL TO CHARACTER CHR ON WORD BOUNDARIES
C     NUMBER OF WORDS TO TRANSFER = NWORDS
C
C     ***** A MACHINE DEPENDENT SUBROUTINE ******
C
C
      INTEGER IHOL(*), INT
      CHARACTER CHR*(*)
C     CHARACTER STR*(4)                                                 HMlg
C
C     EQUIVALENCE (INT,STR)                                             HMlg
C
      EXTERNAL BKDATW
      COMMON /WORDS/ IWORD(10)
C
C
C     This code for machines where you may
C     equivalence characters to integers.
C
C     IBEG = 1                                                          HMlg
      NCMW = IWORD(7)
      IF ((NCMW.LT.2).OR.(NCMW.GT.10)) THEN
C     WRITE ( 3, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             H
      WRITE ( 6, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             PLMAClg
      CALL ABORT
C     CALL BKDATW                                                       HMlg
      ENDIF
C
C     DO 10 I=1,NWORDS                                                  HMlg
C     INT = IHOL(I)                                                     HMlg
C     IEND = IBEG + NCMW - 1                                            HMlg
C     CHR(IBEG:IEND) = STR(1:NCMW)                                      HMlg
C     IBEG = IEND + 1                                                   HMlg
C10   CONTINUE                                                          HMlg
C
C
      KWORDS = NWORDS * IWORD(7)                                        L
      CALL HOLCHR (IHOL, 1, KWORDS, CHR, 1)                             L
C
C     THIS CODE FOR NO ALTERNATIVES
C     IHOL IS DECLARED *2
C     KWORDS = NWORDS * IWORD(7)/2
C     DO 10 I=1,KWORDS
C     IPOS = (I*2) - 1
C     CALL GETHOL (IHOL(I), 1, ICH)
C     CHR(IPOS:IPOS) = CHAR(ICH)
C     IPOS = IPOS + 1
C     CALL GETHOL (IHOL(I), 2, ICH)
C     CHR(IPOS:IPOS) = CHAR(ICH)
C10   CONTINUE
C
      RETURN
      END

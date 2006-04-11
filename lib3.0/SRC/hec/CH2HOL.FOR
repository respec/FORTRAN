      SUBROUTINE CH2HOL ( CHR, IHOL, NWORDS)
C
C     TRANSFER CHARACTER CHR TO HOLLERITH ON WORD BOUNDARIES
C     NUMBER OF WORDS TO TRANSFER = NWORDS
C
C     ***** A MACHINE DEPENDENT SUBROUTINE ******
C     NOTE: CHANGE THIS PARAMETER STATEMENT FOR THE CORRECT
C           NUMBER OF CHARACTERS PER MACHINE WORD
C
C
      INTEGER IHOL(*), INT
      CHARACTER CHR*(*)
      CHARACTER STR*4
C
C
C     EQUIVALENCE (INT,STR)                                             HMlg
C
      EXTERNAL BKDATW
      COMMON /WORDS/ IWORD(10)
C
C
C     THIS CODE FOR MACHINES WHERE YOU MAY
C     EQUIVALENCE CHARACTERS AND INTEGERS
C
      NCMW = IWORD(7)
      IF ((NCMW.LT.2).OR.(NCMW.GT.10)) THEN
      WRITE ( 6, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'
      CALL ABORT
C     CALL BKDATW                                                       HMlg
      ENDIF
C
C     IBEG = 1                                                          HMlg
C     DO 10 I=1,NWORDS                                                  HMlg
C     IEND = IBEG + NCMW - 1                                            HMlg
C     STR(1:NCMW) = CHR(IBEG:IEND)                                      HMlg
C     IHOL(I) = INT                                                     HMlg
C     IBEG = IEND + 1                                                   HMlg
C10   CONTINUE                                                          HMlg
C
C
      KWORDS = NWORDS * IWORD(7)                                        L
      CALL CHRHOL (CHR, 1, KWORDS, IHOL, 1)                             L
C
C     THIS CODE FOR NO ALTERNATIVES
C     IHOL IS DECLARED *2
C     KWORDS = NWORDS * IWORD(7) / 2
C     DO 10 I=1,KWORDS
C     GET EACH BYTE
C     IPOS = (I*2) - 1
C     CALL PUTHOL (IHOL(I), 1, ICHAR(CHR(IPOS:IPOS)))
C     IPOS = IPOS + 1
C     CALL PUTHOL (IHOL(I), 2, ICHAR(CHR(IPOS:IPOS)))
C10   CONTINUE
C
      RETURN
      END

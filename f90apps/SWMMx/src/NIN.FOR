      FUNCTION NIN(NNEED,KNEED)
C=======================================================================
C     THIS FUNCTION REPLACES ARRAY NIN FROM SWMM 3.3
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'HUGO.INC'
      CHARACTER KNEED*10
C=======================================================================
      IF(JCE.EQ.0) THEN
                   DO 100 KK = 1,NE
                   IF(NINNUM(KK).EQ.NNEED) THEN
                                           NIN = KK
                                           RETURN
                                           ENDIF
  100              CONTINUE
                   ELSE
                   DO 200 KK = 1,NE
                   IF(KINNUM(KK).EQ.KNEED) THEN
                                           NIN = KK
                                           RETURN
                                           ENDIF
  200              CONTINUE
                   ENDIF
      RETURN
      END

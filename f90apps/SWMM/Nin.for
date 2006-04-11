      FUNCTION NIN(NNEED,KNEED)
C	TRANSPORT BLOCK
C=======================================================================
C     THIS FUNCTION REPLACES ARRAY NIN FROM SWMM 3.3
C     RETURNS ELEMENT NUMBER OF NNEED OR KNEED
CIM   MODIFIED 4/99 to include check if it can't be found
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
c	Goes here if it didn't find it.
      WRITE(N6,6000) NNEED
	STOP 'ERROR - TRANSPORT ELEMENT NOT FOUND'
                   ELSE
                   DO 200 KK = 1,NE
                   IF(KINNUM(KK).EQ.KNEED) THEN
                                           NIN = KK
                                           RETURN
                                           ENDIF
  200              CONTINUE
C     Goes here if it didn't find it.
	WRITE(N6,6010) KNEED
	STOP 'ERROR - TRANSPORT ELEMENT NOT FOUND'
                   ENDIF
      RETURN
 6000 FORMAT(' ERROR *** ELEMENT ID ',I10,' COULD NOT BE FOUND IN',
     1' E1 ELEMENT DATA',/,
     1       '           CHECK INPUT DATA FILE')
 6010 FORMAT(' ERROR *** ELEMENT ID ',A10,' COULD NOT BE FOUND IN',
     1' E1 ELEMENT DATA',/,
     1       '           CHECK INPUT DATA FILE')
      END

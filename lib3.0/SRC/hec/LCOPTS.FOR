      LOGICAL FUNCTION LCOPTS ( COPTS, CHR )
C ------
C ------ Check if CHR is found in COPTS
C ------ Return TRUE or FALSE
C ------
      CHARACTER*(*) COPTS, CHR*1
C
      I = INDEX ( COPTS, CHR )
C
      IF ( I .EQ. 0 ) THEN
      LCOPTS = .FALSE.
      ELSE
      LCOPTS = .TRUE.
      ENDIF
C
      RETURN
      END

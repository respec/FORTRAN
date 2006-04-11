      SUBROUTINE GTCOPT ( CLINE, COPT, NCHRS )
C ------
C ------ Look for ".abc" options in the string CLINE
C ------ If you find them, move to COPT and blank area
C ------ Return # of options found in NCHRS
C ------
      CHARACTER*(*) CLINE, COPT
      I = INDEX ( CLINE, '.' )
C ------
      IF ( I .NE. 0 ) THEN
      N = LEN ( CLINE )
      COPT = CLINE (I+1:N)
      CLINE (I:N) = ' '
      NCHRS = N - I + 1
C ------
      ELSE
      NCHRS = 0
      COPT = ' '
      ENDIF
C ------
      RETURN
      END

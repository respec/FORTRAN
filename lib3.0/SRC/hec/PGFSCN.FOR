      SUBROUTINE PGFSCN ( CFIELD, CFMT, IVAL )
C ------
C ------ Check a field for valid reading with supplied format
C ------ If invalid, return neg ASCII code of right most character
C ------
      CHARACTER CFIELD*(*), CFMT*(*)
      READ ( CFIELD, FMT=CFMT, ERR=1000 ) IVAL
      RETURN
C ------ Error, set field to - ASCII code
 1000 I = LEN ( CFIELD )
      IVAL = - ICHAR ( CFIELD(I:I) )
      RETURN
      END

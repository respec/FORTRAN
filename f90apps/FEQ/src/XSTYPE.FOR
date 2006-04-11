C
C
C
      SUBROUTINE   XSTYPE
     I                   (STDOUT, VTYPE, TAB)
 
C     + + + PURPOSE + + +
C     Dump valid list of cross section table types.
 
C     + + + DUMMY ARGUMENTS + + +
      INTEGER STDOUT, TAB
      INTEGER VTYPE(*)
 
C     + + +DUMMY ARGUMENT DEFINITIONS + + +
C     STDOUT   - Fortran unit number for user output and messages
C     VTYPE  - list of valid cross section table types
C     TAB    - table number
 
C     + + + LOCAL VARIABLES + + +
      INTEGER I
 
C     + + + OUTPUT FORMATS + + +
 50   FORMAT('0*ERR:195* DATA DEFICIENCY IN TABLE#=',I5,' VALID TYPES:')
 52   FORMAT(11X,'TYPE=',I5)
C***********************************************************************
      WRITE(STDOUT,50) TAB
      DO 100 I=1,35
        IF(VTYPE(I).GT.0) THEN
          WRITE(STDOUT,52) I
        ENDIF
 100  CONTINUE
      RETURN
      END

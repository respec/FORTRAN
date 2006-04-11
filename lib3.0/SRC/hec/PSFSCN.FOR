      SUBROUTINE PSFSCN ( IVAL )
C ------
C ------ Check if ival is neg., if so replace value with
C ------ the integer value of the function character
C ------ represented by the absolute value of IVAL
C ------ What that means is if IVAL is -36, then replace
C ------ IVAL with what ever is defined as a '$' in the functions.
C ------
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
C ------
      CHARACTER CL*20, CF*7, C*1
C ------
CD    WRITE(3,*)' IVAL= ',IVAL
CD    CALL WAITS ( 1.0 )
      IF ( IVAL .LT. 0 ) THEN
      I = IABS ( IVAL )
      C = CHAR ( I )
C ------
      DO 10 J = 1, NKEY
C ------
      IF ( C .EQ. CKEY(J) ) THEN
      N = IKEY(J)
      WRITE ( CF, 5 ) N
    5 FORMAT( '(BN,I',I1,')' )
      CL = CFUNCT(J)(1:N)
      READ ( CL, FMT=CF, ERR=90 ) IVAL
CD    WRITE(3,*)'C,CL,IVAL ',C,CL,IVAL,N
      RETURN
      ENDIF
C ------
   10 CONTINUE
      ENDIF
C ------
   90 CONTINUE
CD    WRITE(3,*)'C,CL,IVAL ',C,CL,IVAL,N,' 90-',CF
      RETURN
      END

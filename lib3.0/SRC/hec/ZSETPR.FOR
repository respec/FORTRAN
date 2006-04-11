      SUBROUTINE ZSETPR (IFLTAB, CFLG, CSTR, INUMB)
C
C     Sets Items in the Permanent section of a DSS file
C     This routine is to be called only by DSSUTL and
C     internal DSS subroutines
C
C     Written by Bill Charley at HEC, January 1990.
C
C
C     INTEGER*6 IFLTAB(*), IADD                                         H
      INTEGER*4 IFLTAB(*), IADD                                         ML
      INTEGER*4 INUMB                                                   ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CFLAG*4, CFLG*(*), CSTR*(*)
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSTS                                                           H
      INCLUDE 'zdssts.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
C
      CFLAG = CFLG
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20) CFLAG                        D
 20   FORMAT (T2,'-----DSS---Debug:  Enter ZSETPR;  Flag: ',A)          D
C
C     Loc file and get the root section
      CALL ZMULTU (IFLTAB, .TRUE.)
C
C
      IF (CFLAG.EQ.'SEQN') THEN
      IFLTAB(KSEQNO) = INUMB
C
      ELSE
      GO TO 820
      ENDIF
C
C
 800  CONTINUE
C     Store new information
      IADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
C
 820  CONTINUE
C     Dump buffers and unlock file
      CALL ZMULTU (IFLTAB, .FALSE.)
C
      RETURN
      END

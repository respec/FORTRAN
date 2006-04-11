      SUBROUTINE ZORDPN (LSELCA, LERR)
C
C     Read pathnames from a catalog file, then call ZSELCA,
C     which will select pathname based on input parameters.
C
C     Written by Bill Charley at HEC, 1983.
C
      LOGICAL LMATCH, LSELCA, LEND, LERR
      CHARACTER CTAG*8, CDUM*1, CPATH*80
      INTEGER*4 IPOS, INUMB                                             ML
C
C
CADD C.ZDSSCA                                                           H
      INCLUDE 'zdssca.h'                                                MLu
C
CADD C.ZDSSCC                                                           H
      INCLUDE 'zdsscc.h'                                                MLu
C
CADD C.ZDSSCM                                                           H
      INCLUDE 'zdsscm.h'                                                MLu
C
      COMMON /ZDSSC1/ CPDATE, CPPROG
      CHARACTER CPDATE*7, CPPROG*6
C
C
C
      IF (LSORT) LTWCAT = .FALSE.
      IF (LTWCAT) THEN
      OPEN (UNIT=68,FILE='SCRATCHC.CAT',ACCESS='DIRECT',                MLu
     *RECL=80,IOSTAT=I)                                                 MLu
C     OPEN (UNIT=68, FILE='U1', ACCESS='DIRECT', RECL=80, IOSTAT=I)     H
      IF (I.NE.0) LTWCAT = .FALSE.
      ENDIF
C
      INUMB = 0
      IPOS = 0
      NTWPAT = 0
      CTAG = ' '
C
      NOPTHS = 0
      REWIND (UNIT=JNUNIT)
C     LOOP
 20   CONTINUE
      INUMB = INUMB + 1
      CALL ZRDPAT (JNUNIT, IPOS, INUMB, CTAG, CPATH, NPATH, LEND)
      IF (LEND) GO TO 800
      CALL ZSELCA (CPATH, NPATH, CTAG, CPPROG, CPDATE, CDUM, CDUM,
     * IDUM, IDUM, IDUM, INUMB, LSELCA, LMATCH, .FALSE., LERR)
      IF (LERR) GO TO 800
      IF (LMATCH) NOPTHS = IPOS
      GO TO 20
C
 800  CONTINUE
      IF (LTWCAT) CLOSE (UNIT=68, STATUS='DELETE')                      MLu
C     IF (LTWCAT) CLOSE (UNIT=68)                                       H
      LTWCAT = .FALSE.
      RETURN
      END

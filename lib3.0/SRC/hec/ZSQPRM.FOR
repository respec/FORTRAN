      SUBROUTINE ZSQPRM (IFTOLD, IFTNEW, LRETAG)
C
C     Copies DSS file attributes to a new DSS file for a squeeze
C     This routine is to be called only by DSSUTL and is
C     called only after the new file has been opened.
C     The new DSS file is assumed to be locked
C
C     Written by Bill Charley at HEC, January 1990.
C
C
C     INTEGER*6 IFTOLD(*), IFTNEW(*), IADD                              H
      INTEGER*4 IFTOLD(*), IFTNEW(*), IADD                              ML
C     INTEGER IFTOLD(*), IFTNEW(*)                                      u
      LOGICAL LRETAG
      CHARACTER CSCRAT*10
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
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20)                              D
 20   FORMAT (T8,'-----DSS---Debug:  Enter ZSQPRM')                     D
C
C
C     Copy information
C
C     Copy tag sequence number
      IF (.NOT.LRETAG) IFTNEW(KSEQNO) = IFTOLD(KNRECS)
C
C     Copy creation date
      CSCRAT = ' '
      CALL HOLCHR (IFTOLD(KCREAT), 1, NDATEC, CSCRAT, 1)
      CALL CHRHOL (CSCRAT, 1, NDATEC, IFTNEW(KCREAT), 1)
C
C     Copy tag information
      DO 60 I=1,16
      IFTNEW(KTAGS+I-1) = IFTOLD(KTAGS+I-1)
 60   CONTINUE
C
C     Copy irregular time-seires block size information
      IFTNEW(KITSIN) = IFTOLD(KITSIN)
      IFTNEW(KITSDA) = IFTOLD(KITSDA)
      IFTNEW(KITSMO) = IFTOLD(KITSMO)
      IFTNEW(KITSYE) = IFTOLD(KITSYE)
      IFTNEW(KITSDE) = IFTOLD(KITSDE)
      IFTNEW(KITSCE) = IFTOLD(KITSCE)
C
C
C     Copy data compression information
      IF (IFTOLD(KCOMPN).GT.0) THEN
      JSIZE = IFTOLD(KCOMPN)
      IF (JSIZE.LE.KLBUFF) THEN
      CALL ZGTREC(IFTOLD, ILBUFF, JSIZE, IFTOLD(KCOMPI), .FALSE.)
      IFTNEW(KCOMPI) = IFTNEW(KFSIZE)
      IFTNEW(KBSADD) = IFTNEW(KFSIZE)
      IFTNEW(KCOMPN) = JSIZE
      CALL ZPTREC(IFTNEW, ILBUFF, JSIZE, IFTNEW(KCOMPI), .FALSE.)
      IFTNEW(KFSIZE) = IFTNEW(KFSIZE) + IFTNEW(KCOMPN)
      ENDIF
      ENDIF
C
C     Zero out the tag-hash table address (so a new one will be made)
      IFTNEW(KTAGBK) = 0
C
C     Store new information
      IADD = 1
      CALL ZPTREC (IFTNEW, IFTNEW(KPERM), NPERM, IADD, .FALSE.)
C
      RETURN
      END

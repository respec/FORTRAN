      SUBROUTINE ZNWBIN (IFLTAB)
C
C     Set up a new pathname Bin.  If a new Bin Block is needed,
C     add it to the end of the file
C
C     Written by Bill Charley at HEC, 1988.
C
C
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*), IREC, NREC                                   ML
C     INTEGER IFLTAB(*)                                                 u
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSBZ                                                           H
      INCLUDE 'zdssbz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
C
      IF (MLEVEL.GE.14) WRITE (MUNIT,20) IFLTAB(KBNREM)                 D
 20   FORMAT (T6,'-----DSS---Debug:  Enter ZNWBIN',/,                   D
     * T12,'Bins Remaining:',I4)                                        D
C
C     Record that we are using a new bin
      IFLTAB(KBINS) = IFLTAB(KBINS) + 1
C
C     If we will be adding a new pathname block area, then
C     keep the path bins on record boundaries
C     This will waste a little space, but improve speed significantly
C     E.g., starting at words 1 or 225 or 449 or 673 on the Harris,
C     1 on the PC or other computer where the bin size is the same as
C     the record length.
      IF (IFLTAB(KBNREM).LE.0) THEN
      CALL ZPTREC (IFLTAB, IZERO, 1, IFLTAB(KFSIZE), .FALSE.)
      CALL ZGETRW (IFLTAB(KFSIZE), IREC, IWRD)
C     I = (IWRD-2) / INT(IFLTAB(KBNSIZ))                                Hc
C     JWRD = ((I + 1) * INT(IFLTAB(KBNSIZ))) + 1                        Hc
C     ISIZE = JWRD - IWRD                                               Hc
C     IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + ISIZE                           Hc
      IF (IWRD.NE.1) THEN                                               MLlgms
      NREC = IREC + 1                                                   MLlgms
      CALL ZGETAD (IFLTAB(KFSIZE), NREC, 1)                             MLlgms
      ENDIF                                                             MLlgms
      ENDIF
C
      IF (LINTAB) THEN
C     Are there any more pathname bins in this block?
      N = IFLTAB(KBNSIZ) - 1
      IF (IFLTAB(KBNREM).LE.0) THEN
C     Write a new pathname block area at the end of the file
C     First, update pointer to next path block
      IPNBIN(N) = IFLTAB(KFSIZE)
      ELSE
      IPNBIN(N) = IFLTAB(KANBIN)
      ENDIF
C
      I = IFLTAB(KBNSIZ)
      CALL ZPTREC (IFLTAB, IPNBIN, I, IPBADD, .TRUE.)
      IFLTAB(KBOVER) = IFLTAB(KBOVER) + 1
      ENDIF
C
      IF (IFLTAB(KBNREM).LE.0) THEN
C     Second, update last bin to point to new block for catalog
      N = IFLTAB(KBNSIZ)                                                MLu
C     N = INT(IFLTAB(KBNSIZ))                                           H
      CALL ZGTREC (IFLTAB, IPNBIN, N, IFLTAB(KANBIN), .TRUE.)
      IPNBIN(N) = IFLTAB(KFSIZE)
      CALL ZPTREC (IFLTAB, IPNBIN, N, IFLTAB(KANBIN), .TRUE.)
C
C     Set location of pathname block at end of file
      IFLTAB(KBNREM) = IFLTAB (KBNBLK) - 1
      IFLTAB(KANBIN) = IFLTAB(KFSIZE)
C     Store a new pathname block area (zero filled)
      ISIZE = -(IFLTAB(KBNBLK) * IFLTAB(KBNSIZ))
C     (This call keeps this block in memory - for speed only)
      CALL ZPTREC (IFLTAB, IZERO, 1, IFLTAB(KANBIN), .TRUE.)
      CALL ZPTREC (IFLTAB, IZERO, ISIZE, IFLTAB(KANBIN), .FALSE.)
      LSBUFF(JBUFF) = .TRUE.
C     Now update the file size
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + IABS(ISIZE)
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
C
      ELSE
      IFLTAB(KBNREM) = IFLTAB(KBNREM) - 1
      ENDIF
      JPNBIN = 1
      DO 40 I=1,IFLTAB(KBNSIZ)
      IPNBIN(I) = 0
 40   CONTINUE
      IPBADD = IFLTAB(KANBIN)
C     UPDATE THE LOCATION OF THE NEXT PATHNAME BIN
      IF (IFLTAB(KBNREM).GT.0) THEN
      IFLTAB(KANBIN) = IFLTAB(KANBIN) + IFLTAB(KBNSIZ)
      ENDIF
C
      IF (MLEVEL.GE.14) WRITE (MUNIT,100)                               D
 100  FORMAT (T6,'-----DSS---Debug:  Exit  ZNWBIN')                     D
C
      RETURN
      END

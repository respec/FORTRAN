      SUBROUTINE ZFSIZE (IFLTAB)
C
C     This subroutine sets the file size parameters based
C     on a ZSET call with the parameter 'SIZE', or uses
C     a default file size.
C
C     The parameters set include:
C        The Hash size (number of hashes possible)
C        The Bin size (how large a pathname bin is)
C        The number of bins per block (how many pathname
C        bins are grouped together)
C        The structure type:
C              Type 1:  With hash table for dynamic data bases
C              Type 2:  No hash table (hash directly to bins) for
C                       somewhat stable data bases
C
C
C   Size        Hash   Bin  N Bins  Target    Target         Default
C   Name        Size  Size  Block   Number    Range           Range
C   ----        ----  ----  -----   ------    -----           -----
C   Tiny           8    56    4       20       1-50            1-20
C   Extra Small   32    56    4       50       1-200          21-99
C   Small        128   112    8      200     100-1,000       100-499
C   Medimum      512   112   16    1,000     200-5,000       500-1,999
C   Large       1024   112   32    4,000   1,000-10,000    2,000-7,999
C   Extra Large 2048   112   32   10,000   2,000-20,000    8,000-14,999
C   Huge        4096   112   32   20,000   5,000-50,000   15,000-29,999
C   Extra Huge  8192   112   32   50,000     >20,000         >30,000
C
C
C     In the preceding table, the target number and target range are
C     the optimal number of records in the file, while the default
C     range is the size selected when a number of pathnames is
C     passed in a ZSET 'SIZE' call.
C
C     Written by Bill Charley at HEC, 1988.
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
C
C     Set file size parameters
      INTEGER JHASHS(8), IBSIZE(8), NBINBK(8), IRANGE(8)
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*)                                               ML
C     INTEGER IFLTAB(*)                                                 u
C
C     Note:  If IBSIZE is increased, change the dimension of IPNBIN
C     in the common ZDSSIZ also.
C     DATA JHASHS(1), IBSIZE(1), NBINBK(1), IRANGE(1)                   H
C    * /           8,        56,         4,        1/                   H
C     DATA JHASHS(2), IBSIZE(2), NBINBK(2), IRANGE(2)                   H
C    * /          32,        56,         4,       21/                   H
C     DATA JHASHS(3), IBSIZE(3), NBINBK(3), IRANGE(3)                   H
C    * /         128,       112,         8,      100/                   H
C     DATA JHASHS(4), IBSIZE(4), NBINBK(4), IRANGE(4)                   H
C    * /         512,       112,        16,      500/                   H
C     DATA JHASHS(5), IBSIZE(5), NBINBK(5), IRANGE(5)                   H
C    * /        1024,       112,        32,     2000/                   H
C     DATA JHASHS(6), IBSIZE(6), NBINBK(6), IRANGE(6)                   H
C    * /        2048,       112,        32,     8000/                   H
C     DATA JHASHS(7), IBSIZE(7), NBINBK(7), IRANGE(7)                   H
C    * /        4096,       112,        32,    15000/                   H
C     DATA JHASHS(8), IBSIZE(8), NBINBK(8), IRANGE(8)                   H
C    * /        8192,       112,        32,    30000/                   H
C
      DATA JHASHS(1), IBSIZE(1), NBINBK(1), IRANGE(1)                   MLlgms
     * /           8,        63,         4,        1/                   MLlgms
      DATA JHASHS(2), IBSIZE(2), NBINBK(2), IRANGE(2)                   MLlgms
     * /          32,        63,         4,       21/                   MLlgms
      DATA JHASHS(3), IBSIZE(3), NBINBK(3), IRANGE(3)                   MLlgms
     * /         128,       127,         8,      100/                   MLlgms
      DATA JHASHS(4), IBSIZE(4), NBINBK(4), IRANGE(4)                   MLlgms
     * /         512,       127,        16,      500/                   MLlgms
      DATA JHASHS(5), IBSIZE(5), NBINBK(5), IRANGE(5)                   MLlgms
     * /        1024,       127,        32,     2000/                   MLlgms
      DATA JHASHS(6), IBSIZE(6), NBINBK(6), IRANGE(6)                   MLlgms
     * /        2048,       127,        32,     8000/                   MLlgms
      DATA JHASHS(7), IBSIZE(7), NBINBK(7), IRANGE(7)                   MLlgms
     * /        4096,       127,        32,    15000/                   MLlgms
      DATA JHASHS(8), IBSIZE(8), NBINBK(8), IRANGE(8)                   MLlgms
     * /        8192,       127,        32,    30000/                   MLlgms
C
C
C     DATA JHASHS(1), IBSIZE(1), NBINBK(1), IRANGE(1)                   c
C    * /           8,        64,         4,        1/                   c
C     DATA JHASHS(2), IBSIZE(2), NBINBK(2), IRANGE(2)                   c
C    * /          32,        64,         4,       21/                   c
C     DATA JHASHS(3), IBSIZE(3), NBINBK(3), IRANGE(3)                   c
C    * /         128,       128,         8,      100/                   c
C     DATA JHASHS(4), IBSIZE(4), NBINBK(4), IRANGE(4)                   c
C    * /         512,       128,        16,      500/                   c
C     DATA JHASHS(5), IBSIZE(5), NBINBK(5), IRANGE(5)                   c
C    * /        1024,       128,        32,     2000/                   c
C     DATA JHASHS(6), IBSIZE(6), NBINBK(6), IRANGE(6)                   c
C    * /        2048,       256,        32,     8000/                   c
C     DATA JHASHS(7), IBSIZE(7), NBINBK(7), IRANGE(7)                   c
C    * /        4096,       256,        32,    15000/                   c
C     DATA JHASHS(8), IBSIZE(8), NBINBK(8), IRANGE(8)                   c
C    * /        8192,       256,        32,    30000/                   c
C
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) LSZSET, NSIZE, IHSIZE, CSIZE   D
 20   FORMAT (T8,'-----DSS---Debug:  Enter ZFSIZE',/,T12,               D
     * 'LSZSET, NSIZE, IHSIZE, CSIZE: ',L2,2I5,2X,A)                    D
C
C     If a ZSET 'SIZE' call has been made, determine proper parameter
      IF ((IHSIZE.GT.0).AND.(IHSIZE.LT.9)) THEN
C
      ELSE
C
      IF (LSZSET) THEN
      IHSIZE = 0
C
      IF (CSIZE(1:2).NE.'  ') THEN
      CALL UPCASE (CSIZE)
C
      I = INDEX(CSIZE,'TINY')
      IF (I.GT.0) THEN
      IHSIZE = 1
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'SMAL')
      IF (I.GT.0) THEN
      J = INDEX(CSIZE,'EX')
      IF (J.GT.0) THEN
      IHSIZE = 2
      ELSE
      IHSIZE = 3
      ENDIF
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'MED')
      IF (I.GT.0) THEN
      IHSIZE = 4
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'LARG')
      IF (I.GT.0) THEN
      J = INDEX(CSIZE,'EX')
      IF (J.EQ.0) THEN
      IHSIZE = 5
      ELSE
      IHSIZE = 6
      ENDIF
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'HUGE')
      IF (I.GT.0) THEN
      J = INDEX(CSIZE,'EX')
      IF (J.EQ.0) THEN
      IHSIZE = 7
      ELSE
      IHSIZE = 8
      ENDIF
      ENDIF
C
 100  CONTINUE
C
      ELSE
      DO 120 I=1,8
      IF (NSIZE.GE.IRANGE(I)) IHSIZE = I
 120  CONTINUE
      ENDIF
C
      IF (IHSIZE.EQ.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 140) NSIZE, CSIZE
 140  FORMAT (/,' -----DSS***  ZOPEN - Error:  Illegal File Size Set',/
     * ' Number of Records Set:',I7,'  Size Name Set: ',A,/,
     * ' --- The Default File Size Will Be Used ---',/)
      IHSIZE = 4
      ENDIF
C
      ELSE
      IHSIZE = 4
      ENDIF
      ENDIF
C
C
      IFLTAB(KHSIZE) = IHSIZE
      IFLTAB(KHASH)  = INT(JHASHS(IHSIZE))
      IFLTAB(KBNBLK) = INT(NBINBK(IHSIZE))
      IFLTAB(KBNSIZ) = INT(IBSIZE(IHSIZE))
C
      IF (LSTABL) THEN
      IFLTAB(KTABLE) = 2
      ELSE
      IFLTAB(KTABLE) = 1
      ENDIF
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,200) IHSIZE, IFLTAB(KTABLE)        D
 200  FORMAT (T8,'-----DSS---Debug:  Exit  ZFSIZE;  Size:',I3,          D
     * ',  Ktable:',I3)                                                 D
C
      RETURN
      END

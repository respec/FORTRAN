      SUBROUTINE ZWLOCK (IFLTAB, CACT, ISTAT)
C
C     Set the (opened) file in an exclusive lock or
C     write lock state.  In this state, no one else may
C     access the file.  The write lock is the exclusive lock
C     plus the permenant record and the hash table (if it fits)
C     remains in memory, and no records are specifically
C     flushed to the disk until the write lock is set to "OFF",
C     or the file is closed.  In the event of a crash or abort,
C     the file MUST be squeezed, because the pointer tables may
C     not have been updated.
C
C     CACT is a character string that describes the action to take.
C     The three possible actions are:
C        "ON"  - Locks the file and keeps pointers in memory.
C        "EXCLUSIVE" - Just locks the file (disk is updated after
C                      every write).
C        "OFF"  - Flushes pointers and buffers to disk, and unlock
C                 the file (applies to both "ON" and "EXCLUSIVE").
C
C
C     INTEGER*6 IFLTAB(*), NADD, IFSIZE, ISIZE                          H
C     INTEGER*4 IFLTAB(*), NADD, IFSIZE, ISIZE                          ML
      INTEGER IFLTAB(*)                                                 u
      INTEGER*4 IREC, JREC                                              ML
C
      CHARACTER CACT*(*)
C
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSBZ                                                           H
      INCLUDE 'zdssbz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
C     Be sure that we have no more than two records locked at this time
C     (from other file(s))
      NLOCK = 0
      DO 820 I=1,MXBUFF
      IF (LOCKBF(I)) NLOCK = NLOCK + 1
 820  CONTINUE
C
      IF (J.LE.2) THEN
C     Write to the file a flag indicating Write Lock on,
C     in case there is an abort, we will know the file size is unknown!!
      IFSIZE = IFLTAB(KFSIZE)
      IFLTAB(KFSIZE) = -1357
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
      CALL ZBDUMP (IFLTAB, 1)
      IFLTAB(KFSIZE) = IFSIZE
C     Now lock records 1 and 2
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
      JREC = 2
      CALL ZGETAD (NADD, JREC, 1)
      CALL ZGTREC (IFLTAB, INFO, 1, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
      IFLTAB(KWLOCK) = 1
      ENDIF
C
C     ENDIF                                                             thj
C     IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT), CPATH(1:NPATH), D
C    * NIHEAD, NCHEAD, NUHEAD, NDATA                                    D
C20   FORMAT (/,T10,'-----DSS---Debug: Enter ZNWRIT,  Unit:',I4,/,      D
C    * /,T12,'Pathname: ',A,/,T12,'NIHEAD:',I5,', NCHEAD:',I5,          D
C    * ', NUHEAD:',I5,', NDATA:',I5)                                    D
C
C     IF (MLEVEL.GE.12) WRITE (MUNIT,820)                               D
C820  FORMAT (T8,'-----DSS---Debug: EXIT ZNWRIT')                       D
      RETURN                                                            thj
      END                                                               thj

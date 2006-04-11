      SUBROUTINE POSFL (ILFN,ILINE,ISTAT)
C
      INTEGER  ILFN,ILINE,ISTAT,DUMMY
      CHARACTER C*1                                                     L
C
C     CALL GIOPLW (ILFN,'23,DUMMY,ILINE-1,ISTAT)                        H
C
      REWIND (ILFN)                                                     L
      DO 10 I=1, ILINE-1                                                L
      READ (ILFN,5,ERR=900,END=900,IOSTAT=ISTAT) C                      L
 5    FORMAT (A)                                                        L
 10   CONTINUE
      ISTAT = 0                                                         L
C
 900  CONTINUE                                                          L
      RETURN
      END

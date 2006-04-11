      SUBROUTINE ZBDUMP (IFLTAB, IBGBUF)
C
C     Dump all data in the buffers to disk,
C     then set the buffers as clear
C
C     Written by Bill Charley at HEC, 1989
C
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*), K                                            ML
C     INTEGER IFLTAB(*)                                                 u
C     CHARACTER CNAME*30                                                H
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSBZ                                                           H
      INCLUDE 'zdssbz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,20) IFLTAB(KUNIT),                 D
     * IFLTAB(KHANDL), IBGBUF, IFLTAB(KMXREC)                           D
 20   FORMAT (T5,'-----DSS---ZBDUMP:  Dump Buffers for unit:',I4,       D
     * ',  Handle:',I4,/,T10,'Min Buffer:',I4,',  Max Record:',I6)      D
      IF (MLEVEL.GE.12) THEN                                            D
      WRITE (MUNIT,21) JCREC                                            D
      WRITE (MUNIT,22) JBUNIT                                           D
      WRITE (MUNIT,23) LSBUFF                                           D
      WRITE (MUNIT,24) JWRITE                                           D
      WRITE (MUNIT,25) LOCKBF                                           D
      WRITE (MUNIT,26) JMXREC                                           D
 21   FORMAT (T16,'Current Record:',9I7)                                D
 22   FORMAT (T16,'Unit:          ',9I7)                                D
 23   FORMAT (T16,'Save Record:   ',9L7)                                D
 24   FORMAT (T16,'Write Flags:   ',9I7)                                D
 25   FORMAT (T16,'Lock Flags:    ',9L7)                                D
 26   FORMAT (T16,'Max File Rec:  ',9I7)                                D
      ENDIF                                                             D
C
      IF ((IBGBUF.LT.0).OR.(IBGBUF.GT.MXBUFF)) THEN
      WRITE (MUNIT,*)'ZBDUMP:  Illegal Arguments',IBGBUF
      MINBUF = 1
      ELSE
      MINBUF = IBGBUF
      IF (MINBUF.EQ.0) MINBUF = 1
      ENDIF
C
C
C     Write those buffers that are at the end of the file, in
C     forward order (e.g., need to write rec 9 before rec 10).
      DO 120 I=1,MXBUFF
      IF ((JCREC(I).GT.0).AND.(JWRITE(I).GT.0)) THEN
      IF (JCREC(I).GT.JMXREC(I)) THEN
C     Is there a write lock set for this buffer?
      IF (LOCKBF(I)) GO TO 120
      CALL ZWREC  (JBUNIT(I), JCREC(I), IBUFF(1,I), NBSIZE,
     * ISTAT, JSTAT)
C     Check for a write error
      IF (ISTAT.NE.0) GO TO 900
      JWRITE(I) = 0
      IF (I.GE.MINBUF) THEN
      LSBUFF(I) = .FALSE.
      IF (IFLTAB(KHANDL).EQ.JBUNIT(I)) THEN
      IF (JCREC(I).GT.IFLTAB(KMXREC)) IFLTAB(KMXREC) = JCREC(I)
      ENDIF
      JCREC(I) = -1
      ENDIF
      ENDIF
      ENDIF
 120  CONTINUE
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'After Statement 120'            D
C
C     Now that the end of the file has been written, store the
C     root (permanent) record (containing the file size)
      DO 140 I=1,MXBUFF
      IF ((JCREC(I).EQ.1).AND.(JWRITE(I).GT.0)) THEN
C     Is there a write lock set for this buffer?
      IF (LOCKBF(I)) GO TO 140
      CALL ZWREC  (JBUNIT(I), JCREC(I), IBUFF(1,I), NBSIZE,
     * ISTAT, JSTAT)
C     Check for a write error
      IF (ISTAT.NE.0) GO TO 900
      JWRITE(I) = 0
      IF (I.GE.MINBUF) THEN
      LSBUFF(I) = .FALSE.
      JCREC(I) = -1
      ENDIF
      ENDIF
 140  CONTINUE
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'After Statement 140'            D
C
C     Also, if one record directly follows another (9 and 10),
C     write them in order for speed, since we probably don't need
C     to worry about addresses.
      DO 160 I=1,MXBUFF-1
      IF ((JCREC(I).GT.0).AND.(JWRITE(I).GT.0)) THEN
      IF ((JCREC(I+1).GT.0).AND.(JWRITE(I+1).GT.0)) THEN
      K = JCREC(I) + 1
      IF ((JCREC(I+1).EQ.K).AND.(JBUNIT(I).EQ.JBUNIT(I+1))) THEN
      IF (K.GT.10) THEN
      CALL ZWREC  (JBUNIT(I), JCREC(I), IBUFF(1,I), NBSIZE,
     * ISTAT, JSTAT)
C     Check for a write error
      IF (ISTAT.NE.0) GO TO 900
      JWRITE(I) = 0
      IF (I.GE.MINBUF) THEN
      LSBUFF(I) = .FALSE.
      JCREC(I) = -1
      ENDIF
      ENDIF
      ENDIF
      ENDIF
      ENDIF
 160  CONTINUE
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'After Statement 160'            D
C
C     Now write all other buffers to disk, in reverse order
C     so that if a crash occurs during the write, no pointers
C     will be written pointing to a spot beyond the physical end
C     of the file
      DO 180 I=MXBUFF,1,-1
      IF (LOCKBF(I)) GO TO 180
      IF ((JCREC(I).GT.0).AND.(JWRITE(I).GT.0)) THEN
C     Is there a write lock set for this buffer?
      CALL ZWREC  (JBUNIT(I), JCREC(I), IBUFF(1,I), NBSIZE,
     * ISTAT, JSTAT)
C     Check for a write error
      IF (ISTAT.NE.0) GO TO 900
      JWRITE(I) = 0
      ENDIF
      IF (I.GE.MINBUF) THEN
      LSBUFF(I) = .FALSE.
      JCREC(I) = -1
      ENDIF
 180  CONTINUE
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,820)                               D
 820  FORMAT (T5,'-----DSS---ZBDUMP:  Exit')                            D
C
      RETURN
C
C     ERROR ON WRITE REQUEST
*  NEED TO DO SIMILAR THING ON PC (SEE DSSUTL) !!!!!!!!!!!!!!!
 900  CONTINUE
C     Test to see if the file execeeded disk space limits
C     IF ((ISTAT.EQ.7340032).AND.(JSTAT.EQ.8126464)) THEN               H
C     CALL ZINQIR (IFLTAB, 'NAME', CNAME, I)                            H
C     CALL ZERROR (IFLTAB, 41, 'ZBDUMP', JSTAT, IFLTAB(KFSIZE), CNAME,  H
C    * I, ' ', 1)                                                       H
C     ENDIF                                                             H
C
C     No - some other error
      WRITE (MUNIT, 910) IFLTAB(KUNIT), JCREC(I), ISTAT, JSTAT, JCREC
 910  FORMAT (///,' ********* DSS ********* ERROR ON WRITE REQUEST',
     * /,' ROUTINE ZBDUMP, UNIT, RECORD =',2I8,'  STATUS =',2I8,/,
     * '  Current Records: ',8I8)
C
      IFLTAB(1) = JCREC(I)
      CALL ZABORT (IFLTAB, 40, 'ZBDUMP', JSTAT, IFLTAB(1), ' ')
C
      END

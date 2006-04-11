      SUBROUTINE READNEXT
C=======================================================================
C     ROUTINE TO READ VALUES FROM PRECIPITATION FILE.
C     WRITTEN BY CHUCK MOORE, CDM, 8/93.
C     EDITED FOR STYLE BY WCH, 8/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'RDII.INC'
C=======================================================================
      JRDDAY(1) = JRDDAY(2)
      RDTIM(1)  = RDTIM(2)
      DO 10 J = 1,NTK
      DO 10 I = 1,3
   10 RDFLOW(J,I,1) = RDFLOW(J,I,2)
      READ(NRDII,IOSTAT=IOSTT,ERR=200,END=100) 
     1    JRDDAY(2),RDTIM(2),((IZERO(J,I,2),I=1,3),J=1,NTK)
      DO 20 J=1,NTK
      DO 20 I=1,3
C=======================================================================
C  IZERO = .TRUE. ==> NON-ZERO I/I EXCESS VALUE TO READ.
C  IZERO = .FALSE. => DON'T READ I/I VALUE.  RDFLOW = 0.0
C=======================================================================
      IF(IZERO(J,I,2)) THEN
           READ(NRDII,IOSTAT=IOSTT,ERR=200,END=100) RDFLOW(J,I,2)
           ELSE
           RDFLOW(J,I,2) = 0.0
           ENDIF
   20 CONTINUE
      RETURN
  100 JRDDAY(2) = 99999
      RETURN
  200 WRITE(N6,9000) JRDDAY(1),IOSTT
      WRITE(*,9000)  JRDDAY(1),IOSTT
      STOP
C=======================================================================
 9000 FORMAT (/,' ERROR while reading NSCRAT(8) at approx. Julian day',
     1 I8,/,' IOSTAT =',I6,' Run stopped from Subroutine READNEXT.')
      END

      PROGRAM LEN
      INTEGER I,LINE,LONG
      CHARACTER*64 FNAME
      CHARACTER*1 BUF1(80)
 1000 FORMAT (80A1)
      WRITE (*,*) ' FILE NAME'
      READ (*,*) FNAME
      OPEN (10,FNAME)
      LINE= 0
      LONG= 0
 10   CONTINUE
        READ (10,1000,END= 100) BUF1
        LINE= LINE+ 1
        DO 20 I= 73, 80
          IF (BUF1(I) .NE. ' ') THEN
            LONG= LONG+ 1
            WRITE (*,*) LINE,':',BUF1(I),':'
          END IF
 20     CONTINUE
      GO TO 10
 100  CONTINUE
      WRITE (*,*) 'LINES',LINE,' LONG',LONG
      END

      SUBROUTINE ZFVER (CFNAME, CVER, IVER)
C
C     Determine a DSS file's version number
C     CVER is returned with the 4 character version (6-FD)
C     IVER is returned with:
C        -3:  Not a DSS file
C        -2:  Unable to access file (but it exists)
C        -1:  File does not exist
C         4:  Version 4 file
C         5:  Version 5 file
C         6:  Version 6 file
C
C     INTEGER*6 IREC(5)                                                 H
      INTEGER*4 IREC(5)                                                 MLu
      CHARACTER CFNAME*(*), CVER*(*), CNAME*64
      LOGICAL LEXIST, LOPEN
C
C
      CVER = ' '
C
      CALL ZFNAME (CFNAME, CNAME, ILEN, LEXIST)
      IF (.NOT.LEXIST) GO TO 910
C
C     Try to find a unit to attach to the DSS file
      JUNIT = 80
      DO 20 I=1,10
      INQUIRE (UNIT=JUNIT, OPENED=LOPEN)
      IF (.NOT.LOPEN) GO TO 40
      JUNIT = JUNIT + 1
 20   CONTINUE
C
      JUNIT = 70
      INQUIRE (UNIT=JUNIT, OPENED=LOPEN)
      IF (.NOT.LOPEN) GO TO 40
      GO TO 920
C
 40   CONTINUE
C     CALL ASSIGS (JUNIT, CFNAME, IERR)                                 H
C     IF (IERR.NE.0) THEN                                               H
C     CLOSE (UNIT=JUNIT)                                                H
C     GO TO 920                                                         H
C     ENDIF                                                             H
C
C     OPEN (UNIT=JUNIT,ACCESS='DIRECT',RECL=2688,IOSTAT=ISTAT)          H
      OPEN (UNIT=JUNIT, FILE=CNAME(1:ILEN), ACCESS='DIRECT', RECL=20,   MLu
     * IOSTAT=ISTAT)                                                    MLu
      IF (ISTAT.NE.0) GO TO 920
      READ (UNIT=JUNIT, REC=1, END=930, ERR=930) IREC                   MLHlgms
C     READ (UNIT=JUNIT, REC=1, ERR=930) IREC                            c
      CLOSE (UNIT=JUNIT)
C
      CNAME = ' '
      CALL HOLCHR (IREC(5), 1, 4, CNAME, 1)
C
      IF (CNAME(2:2).NE.'-') GO TO 930
C
      CVER = CNAME
C
      IF (CNAME(1:1).EQ.'4') THEN
      IVER = 4
      ELSE IF (CNAME(1:1).EQ.'5') THEN
      IVER = 5
      ELSE IF (CNAME(1:1).EQ.'6') THEN
      IVER = 6
      ELSE IF (CNAME(1:1).EQ.'7') THEN
      IVER = 7
      ELSE
      GO TO 930
      ENDIF
C
C
 800  CONTINUE
      RETURN
C
 910  CONTINUE
      IVER = -1
      GO TO 800
C
 920  CONTINUE
      IVER = -2
      GO TO 800
C
 930  CONTINUE
      IVER = -3
      GO TO 800
C
      END

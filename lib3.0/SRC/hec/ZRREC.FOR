      SUBROUTINE ZRREC ( IUNIT, IREC, IARRAY, NWORDS, ISTAT, JSTAT)
C
C     Reads physical record from disk
C
C     Written by Bill Charley at HEC, 1984.
C
C     INTEGER*6 IARRAY(NWORDS)                                          H
C     INTEGER IARRAY(NWORDS)                                            u
      INTEGER*4 IARRAY(NWORDS)                                          ML
      INTEGER*4 IREC, IOFSET, IPOS                                      ML
C
C     CHARACTER C1*4, C2*4                                              sm
C     EQUIVALENCE (C1,I1), (C2,I2)                                      sm
C     COMMON /ZDSSSZ/ IARY2(128)                                        sm
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 20) IUNIT, IREC                   D
C     WRITE (MUNIT, 20) IUNIT, IREC
 20   FORMAT (T3,'+++++DSS+++Debug: Physical  Read;  Unit',I4,          D
     * '  Record',I8)                                                   D
C
C
C     Here for standard FORTRAN 77
      ISTAT = 0
      JSTAT = 0
      READ ( UNIT=IUNIT, REC=IREC, IOSTAT=ISTAT) IARRAY                 L
      IF (ISTAT.EQ.922) ISTAT = -1                                      L
C
C     MS-DOS and Unix Assembly I-O
C     IOFSET = (IREC-1) * 512                                           Mlgms
C     IOFSET = (IREC-1) * NRECL                                         c
C     ICOUNT = 0                                                        M
C40   CONTINUE                                                          M
C     CALL SEEKF (IUNIT, 0, IOFSET, IPOS, ISTAT)                        Mu
C     IF (ISTAT.EQ.5) GO TO 840                                         M
C     IF (ISTAT.NE.0) GO TO 800                                         Mu
C     CALL READF (IUNIT, IARRAY, 508, ISTAT, NTRANS)                    Mlg
C     CALL READF (IUNIT, IARY2,  508, ISTAT, NTRANS)                    sm
C     IF (ISTAT.EQ.5) GO TO 840                                         M
C     IF ((ISTAT.EQ.0).AND.(NTRANS.NE.508)) ISTAT = -1                  Mlgms
C     CALL READF (IUNIT, IARRAY, NRECL, ISTAT, NTRANS)                  c
C     IF ((ISTAT.EQ.0).AND.(NTRANS.NE.NRECL)) ISTAT = -1                c
C
C     Switch bytes for sun Sparc workstation
C     DO 40 I=1,NWORDS                                                  sm
C     I1 = IARY2(I)                                                     sm
C     C2(1:1) = C1(4:4)                                                 sm
C     C2(2:2) = C1(3:3)                                                 sm
C     C2(3:3) = C1(2:2)                                                 sm
C     C2(4:4) = C1(1:1)                                                 sm
C     IARRAY(I) = I2                                                    sm
C40   CONTINUE                                                          sm
C
C
C     Here for Harris assembly I-O (15 times faster than F77!)
C
C     Calculate the sector number
C     ISECT = (IREC) * NSECRC                                           H
C
C     Position to that sector
C     CALL GIOP (IUNIT,'23,IDUM1,ISECT,ISTAT)                           H
C     Check for an error
C     IF (ISTAT.NE.0) GO TO 920                                         H
C
C     Check the status of that position
C     CALL GIOP (IUNIT,0,IDUM1,IDUM2,JSTAT)                             H
C     ISTAT = JSTAT.AND.'34400000                                       H
C     IF (ISTAT.NE.0) GO TO 900                                         H
C
C     Request a read
C     NW = NWORDS * NWPW                                                H
C     CALL GIOP (IUNIT,'03,IARRAY,NW,ISTAT)                             H
C     IF (ISTAT.NE.0) GO TO 920                                         H
C
C     Check the status of the read (and wait until completed)
C     CALL GIOP (IUNIT,0,IDUM1,IDUM2,JSTAT)                             H
C     ISTAT = JSTAT.AND.'34400000                                       H
C     IF (ISTAT.NE.0) GO TO 900                                         H
C
 800  CONTINUE
      RETURN
C
C     Wait for file access on DOS (networked file)
C840  CONTINUE                                                          M
C     ICOUNT = ICOUNT + 1                                               M
C     IF (ICOUNT.GE.100) THEN                                           M
C     WRITE (MUNIT, 850)                                                M
C850  FORMAT (/,' ---DSS--- ERROR:  Unable to Access Locked DSS File',/)M
C     GO TO 800                                                         M
C     ENDIF                                                             M
C     I = MOD (ICOUNT, 25)                                              M
C     IF ((I.EQ.0).AND.(MLEVEL.GE.2)) WRITE (MUNIT, 860)                M
C860  FORMAT (' ---DSS--- Waiting for file access...')                  M
C     CALL WAITS (0.10)                                                 M
C     GO TO 40                                                          M
C
C     Error on Read or position
C900  CONTINUE                                                          H
C
C     KSTAT = ISTAT.AND.'10000000                                       H
C     IF (KSTAT.NE.0) ISTAT = -2                                        H
C
C920  CONTINUE                                                          H
C     RETURN                                                            H
C
      END

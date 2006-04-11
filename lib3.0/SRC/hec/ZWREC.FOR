      SUBROUTINE ZWREC ( IUNIT, IREC, IARRAY, NWORDS, ISTAT, JSTAT)
C
C     Write physical record to disk
C
C     Written by Bill Charley at HEC, 1984.
C
C     INTEGER*6 IARRAY(NWORDS)                                          H
C     INTEGER IARRAY(NWORDS)                                            u
C     INTEGER*4 IARRAY(NWORDS)                                          M
      INTEGER*4 IARRAY(128)                                             L
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
 20   FORMAT (T3,'=====DSS===Debug: Physical Write;  Unit',I4,          D
     * '  Record',I8)                                                   D
C
C
C     Here for standard FORTRAN 77
      ISTAT = 0
      JSTAT = 0
      WRITE ( UNIT=IUNIT, REC=IREC, IOSTAT=ISTAT) IARRAY                L
C
C     Switch bytes for sun Sparc workstation
C     DO 40 I=1,NWORDS                                                  sm
C     I1 = IARRAY(I)                                                    sm
C     C2(1:1) = C1(4:4)                                                 sm
C     C2(2:2) = C1(3:3)                                                 sm
C     C2(3:3) = C1(2:2)                                                 sm
C     C2(4:4) = C1(1:1)                                                 sm
C     IARY2(I) = I2                                                     sm
C40   CONTINUE                                                          sm
C
C
C     MS-DOS and Unix Assembly I-O
C     IOFSET = (IREC-1) * 512                                           Mlgms
C     IOFSET = (IREC-1) * NRECL                                         c
C     CALL SEEKF (IUNIT, 0, IOFSET, IPOS, ISTAT)                        Mu
C     IF (ISTAT.NE.0) GO TO 800                                         Mu
C     IF (IOFSET.LT.30000000) THEN                                      M
C     ISIZE = 508                                                       M
C     ELSE                                                              M
C     ISIZE = 512                                                       Mlgms
C     ENDIF                                                             M
C     CALL WRITF (IUNIT, IARRAY, ISIZE, ISTAT, NTRANS)                  Mlg
C     CALL WRITF (IUNIT, IARY2,  ISIZE, ISTAT, NTRANS)                  sm
C     IF ((ISTAT.EQ.0).AND.(NTRANS.NE.ISIZE)) ISTAT = -1                Mlgms
C     CALL WRITF (IUNIT, IARRAY, NRECL, ISTAT, NTRANS)                  c
C     IF ((ISTAT.EQ.0).AND.(NTRANS.NE.NRECL)) ISTAT = -1                c
C
C
C     Here for Harris assembly I-O (15 times faster than F77!)
C
C     Calculate the sector number
C     ISECT = (IREC) * NSECRC                                           H
C
C     Position to that sector
C     CALL GIOP (IUNIT,'23,IDUM1,ISECT,ISTAT)                           H
C     check for an error
C     IF (ISTAT.NE.0) GO TO 900                                         H
C
C     Check the status of that position
C     CALL GIOP (IUNIT,0,IDUM1,IDUM2,ISTAT)                             H
C     Don't Check for error here - Positioning beyond the
C     end of file is ok (as long as its not to far)
C
C     Request a write
C     NW = NWORDS * NWPW                                                H
C     CALL GIOP (IUNIT,'04,IARRAY,NW,ISTAT)                             H
C     IF (ISTAT.NE.0) GO TO 900                                         H
C
C     Check status of write (and wait unitl completed)
C     CALL GIOP (IUNIT,0,IDUM1,IDUM2,JSTAT)                             H
C     ISTAT = JSTAT.AND.'34400000                                       H
C     IF (ISTAT.NE.0) GO TO 900                                         H
C
 800  CONTINUE
      RETURN
C
C
C     Error on Write or position
C900  CONTINUE                                                          H
C     RETURN                                                            H
C
      END

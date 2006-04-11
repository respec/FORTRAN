      SUBROUTINE ZMULTU ( IFLTAB, LOCK)
C
C     ZMULTU takes care of all multiple user access
C     and loads in the permanent section of the file,
C     when called to lock the file for a write request.
C     It also dumps changed buffers to disk when and
C     unlock request is made.
C
C     This routine is always called at the beginning
C     and end of every change to the DSS file.
C
C     **** THIS ROUTINE IS SYSTEM DEPENDENT *****
C
C     LOCK IS A FLAG TO INDICATE IF THE OPERATION IS
C     TO BEGIN LOCK, OR HAS BEEN COMPLETED
C
C         LOCK = TRUE, BEGIN WRITE REQUEST
C         LOCK = FALSE, WRITE COMPLETE
C
C
C     IF IFLTAB(KMULT) = 1, THIS IS A MULTIPLE USER ACCESS FILE
C     IF IFLTAB(KMULT) = 2, THIS NOT IS A MULTIPLE USER ACCESS FILE
C
C     IF IFLTAB(KLOCK) = 1, The file is currently not locked
C     IF IFLTAB(KLOCK) = 2, The file is currently locked
C
C     Written by Art Pabst at HEC, 1984.
C
C
      LOGICAL LOCK
C     INTEGER*6 IFLTAB(*), IADD                                         H
      INTEGER*4 IFLTAB(*), IADD                                         ML
      INTEGER*4 IBYTE, ISIZE                                            ML
C     INTEGER IFLTAB(*)                                                 u
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IUNIT = IFLTAB(KUNIT)
      IF (MLEVEL.GE.10) WRITE (MUNIT, 20) IUNIT, IFLTAB(KMULT),         D
     * IFLTAB(KLOCK), LOCK                                              D
 20   FORMAT (T5,'----DSS--- DEBUG:  Enter ZMULTU;  Unit:',I4,/,        D
     * T12,'KMULT:',I3,',  KLOCK:',I3,'  LOCK: ',L1)                    D
C
C     CHECK THAT THIS IS A MULTIPLE USER ACCESS FILE
      IF (IFLTAB(KMULT).EQ.1) THEN
C
      IF (LOCK) THEN
C
C     If the file has already been locked, ignore this request
C     (this allows a smart program to lock and save buffers
C     before ZWRITE does)
      IF (IFLTAB(KLOCK).NE.2) THEN
C     CALL FLLKON ( IUNIT, 1, IERR)                                     H
C     CALL LOCKF (IFLTAB(KHANDL), .TRUE., IERR)                         u
C     IBYTE = 0                                                         M
C     ISIZE = IFLTAB(KFSIZE) * 4                                        M
C     ICOUNT = 0                                                        M
C30   CONTINUE                                                          M
C     CALL LOCKF (IFLTAB(KHANDL), 0, IBYTE, ISIZE, IERR)                M
C
C     Wait for file access on DOS (networked file)
C     IF (IERR.EQ.33) THEN                                              M
C     ICOUNT = ICOUNT + 1                                               M
C     IF (ICOUNT.GE.100) THEN                                           M
C     WRITE (MUNIT, 35)                                                 M
C35   FORMAT (/,' ---DSS--- ERROR:  Unable to Access Locked DSS File',/)M
C     GO TO 37                                                          M
C     ENDIF                                                             M
C     I = MOD (ICOUNT, 25)                                              M
C     IF ((I.EQ.0).AND.(MLEVEL.GE.2)) WRITE (MUNIT, 36)                 M
C36   FORMAT (' ---DSS--- Waiting for file access...')                  M
C     CALL WAITS (0.10)                                                 M
C     GO TO 30                                                          M
C     ENDIF                                                             M
C
      IFLTAB(KLOCK) = 2
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  File Locked'           D
C37   CONTINUE                                                          M
C
C     TEST FOR A PROPER LOCK (FLLKON WILL WAIT
C     IF THE FILE IS ALREADY LOCKED)
C     IF (IERR.EQ.1) THEN                                               H
C     WRITE ( MUNIT, 40) IUNIT                                          H
C40   FORMAT (//,' ***** DSS ERROR ******',/                            H
C    * ' SHARED ACCESS ATTEMPT ON FILE WITHOUT SHARED ASSIGN',/,        H
C    * ' EITHER A NON-SHARED ASSIGN WAS USED, OR THE SYSTEM HAS ',      H
C    * /,' NOT BE SET TO USE SHARED FILE CAPABILITY',/                  H
C    * ' UNIT =',I7,//)                                                 H
C     CALL ZABORT (IFLTAB, 200, 'ZMULTU', IUNIT, IFLTAB(KLOCK),         H
C    * 'No shared access')                                              H
C     ELSE IF (IERR.NE.0) THEN                                          H
C     IF (IERR.NE.0) THEN                                               Mu
C     WRITE ( MUNIT, 60) IUNIT, IERR                                    HMu
C60   FORMAT (//,' ***** DSS ERROR *****',/                             HMu
C    * ' UNABLE TO LOCK FILE FOR MULTIPLE USER ACCESS',/,               HMu
C    * ' UNIT = ',I4,'  ERROR CODE = ',I6)                              HMu
C     CALL ZABORT (IFLTAB, 210, 'ZMULTU', IUNIT, IFLTAB(KLOCK),         HMu
C    * 'Can not lock file')                                             HMu
C     ENDIF                                                             HMu
C
C     Read the permanent section of the file (Always!)
C     On Multiple users systems, be sure this is a
C     phyical read (not just a buffer in memory)!!
C     (ZBDUMP will clear all buffers)
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  ZMULTU:  Read Perm'    D
      CALL ZBDUMP (IFLTAB, 1)
      IADD = 1
      CALL ZGTREC (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .TRUE.)
C
      ENDIF
C
      ELSE
C
C     Unlock the file.
C     First dump all buffers to disk
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  ZMULTU:  Dump Buffs'   D
      CALL ZBDUMP (IFLTAB, 1)
C
      IF (IFLTAB(KLOCK).EQ.2) THEN
C     On Unix be sure buffers are written to disk (with sync)
C     CALL SYNCF(IFLTAB(KHANDL), IERR)                                  u
C
C     CALL FLLKOF ( IUNIT, IERR)                                        H
C     CALL LOCKF (IFLTAB(KHANDL), .FALSE., IERR)                        u
C     CALL LOCKF (IFLTAB(KHANDL), 1, IBYTE, ISIZE, IERR)                M
      IFLTAB(KLOCK) = 1
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  File Unlocked'         D
C     IF ((IERR.NE.0).AND.(MLEVEL.GE.1)) THEN                           HMu
C     WRITE ( MUNIT, 80) IUNIT, IERR                                    HMu
C80   FORMAT (' ---DSS - Caution:  Unable to unlock DSS file'           HMu
C    * ' for multiple user access.',/,'  Unit:',I4,'  Code:',I5)        HMu
C     ENDIF                                                             HMu
      ENDIF
      ENDIF
C
C
      ELSE
C
      IF (LOCK) THEN
      IF (IFLTAB(KLOCK).NE.2) THEN
      IFLTAB(KLOCK) = 2
C
C     Read the permanent section of the file (Always!)
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  ZMULTU:  Read Perm'    D
      IADD = 1
      CALL ZGTREC (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .TRUE.)
      ENDIF
C
      ELSE
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  ZMULTU:  Dump Buffs'   D
      CALL ZBDUMP (IFLTAB, 1)
      IFLTAB(KLOCK) = 1
      ENDIF
C
      ENDIF
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 120) IUNIT, IFLTAB(KMULT),        D
     * IFLTAB(KLOCK), LOCK                                              D
 120  FORMAT (T5,'----DSS--- DEBUG:  Exit ZMULTU;  Unit:',I4,/,         D
     * T12,'KMULT:',I3,',  KLOCK:',I3,'  LOCK: ',L1)                    D
C
      RETURN
      END

      SUBROUTINE ZPTREC ( IFLTAB, IARRAY, NARRAY, JADD, LSAVEB)
C
C     Write Logical Records to the DSS File
C
C     Vairable Definitions:
C         IARRAY: Data array to store
C         NARRAY: Number of (integer) words in data
C         NREC:   Beginning record number of where to
C                 store this data
C         NWORD:  Relative word address (in NREC) where
C                 this data starts
C         LSAVEB:  Flag indicating if this record should be saved
C     If NARRAY is:
C       - Less than zero write IABS(NARRAY) of first
C         value in IARRAY to disk (fill option)
C
C     Written by Bill Charley at HEC, 1985. Original version
C        written by Brent Cullimore, 1981.
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSBZ                                                           H
      INCLUDE 'zdssbz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C     INTEGER*6 IFLTAB(*), IARRAY(*), JADD, IADD, ISIZE, JSIZE, NADD    H
      INTEGER*4 IFLTAB(*), IARRAY(*), JADD, IADD, ISIZE, JSIZE, NADD    ML
C     INTEGER IFLTAB(*), IARRAY(*)                                      u
      INTEGER*4 NREC, IREC                                              ML
      LOGICAL LATEND, LFILL, LSAVEB, LEOFFL
      CHARACTER CNAME*64
      SAVE NREADO
C
      DATA NREADO /0/
C
C
      ISTAT = 0
      IUNIT = IFLTAB(KUNIT)
      IHANDL = IFLTAB(KHANDL)
      CALL ZGETRW (JADD, NREC, NWORD)
C
      IF (MLEVEL.GE.14) THEN                                            D
      WRITE (MUNIT,20) IHANDL, JADD, NREC, NWORD, NARRAY,               D
     * IFLTAB(KLOCK), JBUFF, IFLTAB(KMXREC), LSAVEB                     D
      WRITE (MUNIT,21) JCREC                                            D
      WRITE (MUNIT,22) JBUNIT                                           D
      WRITE (MUNIT,23) LSBUFF                                           D
      WRITE (MUNIT,24) JWRITE                                           D
      WRITE (MUNIT,25) LOCKBF                                           D
      WRITE (MUNIT,26) JMXREC                                           D
 20   FORMAT (T12,'----DSS---Debug:  Enter ZPTREC;  Unit:',I3,/,        D
     * T16,'Write Address:',I9,',  Rec:',I7,', Word:',I4,', Size:',I6,/,D
     * T16,'Lock:',I2,',  JBUFF:',I3,',  MXREC:',I8,',  Save Buff:',L2) D
 21   FORMAT (T16,'Current Record:',9I7)                                D
 22   FORMAT (T16,'Unit:          ',9I7)                                D
 23   FORMAT (T16,'Save Record:   ',9L7)                                D
 24   FORMAT (T16,'Write Flags:   ',9I7)                                D
 25   FORMAT (T16,'Lock Flags:    ',9L7)                                D
 26   FORMAT (T16,'Max File Rec:  ',9I7)                                D
      ENDIF                                                             D
C
C
C     Check KEY locations to insure that IFLTAB is not corrupt
C
      IF ((IFLTAB(KEY1).NE.NKEY).OR.(IFLTAB(KEY2).NE.NKEY).OR.
     * (IFLTAB(KEY3).NE.NKEY)) GO TO 980
C
      IF ((IUNIT.LE.0).OR.(IUNIT.GT.200)) THEN
      WRITE ( MUNIT, 40) IUNIT
 40   FORMAT (//,' ***** ILLEGAL UNIT NUMBER FOR DSS FILE:',I9)
      CALL ZABORT (IFLTAB, 20, 'ZPTREC', IUNIT, IFLTAB(1),
     * 'Illegal Unit')
      ENDIF
C
      IF (IFLTAB(KREADO).EQ.1) THEN
      WRITE (MUNIT,50) IUNIT
 50   FORMAT (T3,'-----DSS---ZPTREC;  Unit',I4,':  File has been',
     * ' Placed in a Read Access Only Mode')
      NREADO = NREADO + 1
      IF (NREADO.GE.40) THEN
      WRITE (MUNIT,*)' *** Excess Write with Read Only Errors ***'
      CALL ZABORT (IFLTAB, 130, 'ZPTREC', IUNIT, IFLTAB(1),
     * 'Excess Read Only Errors')
      ENDIF
      GO TO 800
      ENDIF
C
C     Error out if we have received a trashed address
      IF (JADD.LE.0) GO TO 910
C     IF (JADD-2000.GT.IFLTAB(KFSIZE)) GO TO 910
C
C     Initialize Pointers
      IREC = NREC
      IBEG = NWORD
      ISIZE = NARRAY
      IARRP = 0
C
C     Is this a request to write out an array of zeros?
      IF (NARRAY.LT.0) THEN
      LFILL = .TRUE.
C     For machines with less than INT*4, pass the size in the data
C     location (can be a large number) if narray is set to -1 as a flag
      IF (NARRAY.EQ.-1) THEN
      ISIZE = IARRAY(1)
      IF (ISIZE.LT.0) ISIZE = - ISIZE
      IARRAY(1) = 0
      ELSE
      ISIZE = IABS(NARRAY)
      ENDIF
      ELSE
      LFILL = .FALSE.
      ENDIF
C
      JSIZE = ISIZE
C
C
C     ** LOOP **
      LEOFFL = .FALSE.
 100  CONTINUE
C     Calcualte pointers for this record (equiv. to MIN function)
      ITEMP = NBSIZE - IBEG + 1
      IF (ITEMP.LT.ISIZE) THEN
      IEND = ITEMP + IBEG - 1
      ELSE
      IEND = ISIZE + IBEG - 1
      ENDIF
C
C     See if we are writing at the end of the file
      LATEND = .FALSE.
      CALL ZGETAD (IADD, IREC, IEND)
      IF (IADD.GE.IFLTAB(KBSADD)) LATEND = .TRUE.
C
C     Does this record need to be read first?
 110  CONTINUE
      JBUFF = 0
      DO 120 I=1,MXBUFF
      IF ((IREC.EQ.JCREC(I)).AND.(IHANDL.EQ.JBUNIT(I))) JBUFF = I
 120  CONTINUE
C
      IF (JBUFF.EQ.0) THEN
      DO 140 I=MXBUFF,1,-1
      IF (.NOT.LSBUFF(I)) JBUFF = I
 140  CONTINUE
C
C     If all buffers are in a save, use the lowest one that
C     is at the end of the file
      IF (JBUFF.EQ.0) THEN
      DO 160 I=MXBUFF,1,-1
      IF (JCREC(I).GT.IFLTAB(KMXREC)) JBUFF = I
 160  CONTINUE
      ENDIF
C
C     If still no buffers available, use the last one
      IF (JBUFF.EQ.0) JBUFF = MXBUFF
C
C     Does the current buffer need to be written first?
      IF (JWRITE(JBUFF).EQ.1) THEN
C     Yes - write the buffer
      IF (JCREC(JBUFF).LE.0) GO TO 950
C
C     Does a prior record have to be written first to keep the file
C     in order?  (We can't write record 23 if record 22 has not been
C     written yet.)
      IF (JCREC(JBUFF).GT.(JMXREC(JBUFF)+1)) THEN
      DO 170 I=1,MXBUFF
      IF ((JCREC(I).GT.JMXREC(JBUFF)).AND.(JWRITE(I).GT.0)) THEN
      CALL ZWREC (JBUNIT(I), JCREC(I), IBUFF(1,I), NBSIZE, ISTAT, JSTAT)
      IF (ISTAT.NE.0) GO TO 950
      JWRITE(I) = 0
      IF (IFLTAB(KHANDL).EQ.JBUNIT(I)) THEN
      IF (JCREC(I).GT.IFLTAB(KMXREC)) IFLTAB(KMXREC) = JCREC(I)
      ENDIF
      ENDIF
 170  CONTINUE
      ENDIF
C
C     Did we just write the buffer?
      IF (JWRITE(JBUFF).EQ.1) THEN
      CALL ZWREC (JBUNIT(JBUFF), JCREC(JBUFF), IBUFF(1,JBUFF), NBSIZE,
     * ISTAT, JSTAT)
      IF (ISTAT.NE.0) GO TO 950
      JWRITE(JBUFF) = 0
      IF (IFLTAB(KHANDL).EQ.JBUNIT(JBUFF)) THEN
      IF (JCREC(JBUFF).GT.IFLTAB(KMXREC)) IFLTAB(KMXREC) = JCREC(JBUFF)
      ENDIF
      ENDIF
      ENDIF
      ENDIF
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,180) IREC, JBUFF, LATEND           D
 180  FORMAT (T5,'----ZPTREC, Record:',I7,'  JBUFF:',I3,'  LATEND:',L2) D
C
C     Do we need to read the record from the file?
      IF ((IREC.NE.JCREC(JBUFF)).OR.(IHANDL.NE.JBUNIT(JBUFF))) THEN
      IF (.NOT.(LATEND.AND.(IBEG.EQ.1))) THEN
C     Don't read record if the entire record is to be re-written
      IF (.NOT.((IBEG.EQ.1).AND.(IEND.EQ.NBSIZE))) THEN
      CALL ZRREC  (IHANDL, IREC, IBUFF(1,JBUFF), NBSIZE,
     * ISTAT, JSTAT)
C     Check for an error on the Physical read
      IF (ISTAT.GT.0) GO TO 920
      ENDIF
      ELSE
C     This record is at the end of the file.
C     Should the last part of the array be zeroed?
      IF (IEND.LT.NBSIZE) THEN
      J = IEND + 1
C     Set the word after the very last one in the file
C     to the end of file flag.
      CALL ZGETAD (NADD, IREC, J)
      IF (NADD.GE.IFLTAB(KFSIZE)) THEN
      IBUFF(J,JBUFF) = JEOFFL
      J = J + 1
      ENDIF
C     "Zero out" the remainder of the buffer
      IF (J.LE.NBSIZE) THEN
      IF (MLEVEL.GE.14) WRITE (MUNIT,190) IREC, J, NBSIZE               D
 190  FORMAT (T5,'---ZPTREC;  Zeroing Rec:',I7,', From:',I4,', To:',I4) D
      DO 200 I=J,NBSIZE
      IBUFF(I,JBUFF) = 0
 200  CONTINUE
      ENDIF
      ENDIF
      ENDIF
      ENDIF
C
C
C     Update pointers
      JWRITE(JBUFF) = 1
      JCREC(JBUFF) = IREC
      JBUNIT(JBUFF) = IHANDL
      JMXREC(JBUFF) = IFLTAB(KMXREC)
      IF (LSAVEB) LSBUFF(JBUFF) = .TRUE.
C
C     If only an end of file flag was put in the beginning of a new
C     record after the last word in the file, exit.
      IF (LEOFFL) GO TO 800
C
C
C     Transfer the input data into the buffer array
      IF (LFILL) THEN
C     If we are writing a filled array (e.g., all zeros), set the array
      DO 220 I=IBEG,IEND
      IBUFF(I,JBUFF) = IARRAY(1)
 220  CONTINUE
      ELSE
      DO 240 I=IBEG,IEND
      IARRP = IARRP + 1
      IBUFF(I,JBUFF) = IARRAY(IARRP)
 240  CONTINUE
      ENDIF
C
C     Calculate next record pointers
      ISIZE = ISIZE - (IEND - IBEG + 1)
      IBEG = 1
C
C     Are we done?
      IF (ISIZE.LE.0) THEN
C     Make sure the actual ending location of the file is saved
      IADD = JADD + JSIZE
      IF (IADD.GE.IFLTAB(KBSADD)) THEN
C     Save the ending location
      IFLTAB(KBSADD) = IADD
C     Does the end of the file end on a record boundary (and we are
C     writting at the end of the file)?
      IF (IEND.EQ.NBSIZE) THEN
C     Yes.  Put an end of file flag at the beginning of the next
C     (new) record before leaving.
      IREC = IREC + 1
      IEND = 0
      LEOFFL = .TRUE.
      GO TO 110
      ENDIF
      ENDIF
C     ISIZE is less than zero, exit.
      GO TO 800
      ENDIF
C
      IREC = IREC + 1
      GO TO 100
C     ** END OF LOOP **
C
C
 800  CONTINUE
C     Write any Debug exit messages
      IF (MLEVEL.GE.14) THEN                                            D
      CALL ZGETRW (IFLTAB(KFSIZE), IREC, IWORD)                         D
      WRITE (MUNIT,810) IFLTAB(KFSIZE), IREC, IWORD                     D
      WRITE (MUNIT,21) JCREC                                            D
      WRITE (MUNIT,22) JBUNIT                                           D
      WRITE (MUNIT,24) JWRITE                                           D
 810  FORMAT (T12,'-----DSS---Debug:  Exit ZPTREC',/,                   D
     * T16,'File Size:',I9,' Words (',I7,' Records,',I5,' Words)')      D
      ENDIF                                                             D
C
      RETURN
C
C
C     Bad address
 910  CONTINUE
      WRITE (MUNIT, 911) JADD, IFLTAB(KFSIZE)
 911  FORMAT (///,' ********** DSS ******** ERROR DURING READ',/
     * ' ZPTREC, Invalid address:',I14,',  File Size:',I14,/)
      WRITE (MUNIT,922) IUNIT, IREC, NWORD, NARRAY
C
      JSTAT = IFLTAB(KFSIZE)
      CALL ZABORT (IFLTAB, 70, 'ZPTREC', JSTAT, JADD, 'Bad Address')
C
C
C     ERROR ON READ REQUEST
 920  CONTINUE
      WRITE (MUNIT, 921) JADD, IREC, ISTAT, JSTAT
 921  FORMAT (///,' ********* DSS ******** ERROR ON WRITE/READ REQUEST',
     * /,' ZPTREC, ADDRESS:',I9,'  RECORD:',I8,'  STATUS:',8I8)
C
      WRITE (MUNIT,922) IUNIT, IREC, NWORD, NARRAY
 922  FORMAT (' UNIT =',I8,' RECORD =',I8,'  WORD =',I8,'  SIZE:',I8)
C
      CALL ZABORT (IFLTAB, 30, 'ZPTREC', JSTAT, JADD, 'Error on Read')
C
C     ERROR ON WRITE REQUEST
 950  CONTINUE
C     Test to see if the file execeeded disk space limits
* ****DO SIMILAR ON PC !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     IF ((ISTAT.EQ.7340032).AND.(JSTAT.EQ.8126464)) THEN               H
C     CALL ZINQIR (IFLTAB, 'NAME', CNAME, I)                            H
C     CALL ZERROR (IFLTAB, 41, 'ZPTREC', JSTAT, IFLTAB(KFSIZE), CNAME,  H
C    * I, ' ', 1)                                                       H
C     ENDIF                                                             H
C
C     No - some other error
      WRITE (MUNIT, 960) IUNIT, IREC, ISTAT, JSTAT, JCREC
 960  FORMAT (///,' ********* DSS ********* ERROR ON WRITE REQUEST',
     * /,' ROUTINE ZPTREC, Unit and Record:',2I10,'  Status:',I8,/,
     * '  Record Buffer: ',8I9)
C
      CALL ZABORT (IFLTAB, 40, 'ZPTREC', JSTAT, JADD, 'Write Error')
C
 980  CONTINUE
      WRITE ( MUNIT, 990) IFLTAB(KUNIT), NKEY, IFLTAB(KEY1),
     * IFLTAB(KEY2), IFLTAB(KEY3)
 990  FORMAT (///,' ******** DSS: ERROR; IFLTAB HAS BECOME CORRUPT',
     * /,'  This is due to a program error (array overwritten)',/,
     * ' UNIT =',I4,'  NKEY =',I8,'  KEYS 1, 2, 3 =',3I8,/,
     * ' Note: All keys must equal NKEY',//)
C
      I = IFLTAB(KEY1)
      CALL ZABORT (IFLTAB, 50, 'ZPTREC', I, IFLTAB(KEY1), 'Corrupt Key')
C
C
      END

C
C
C
      SUBROUTINE   BINIT (
     O                    BINFRM)
C
C     + + + PURPOSE + + +
C     Set up any necessary binary file initialization, and determine
C     necessary file format type.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*30 BINFRM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BINFRM - format for binary file - may be "FORMATTED" or "UNFORMATTED"
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
C     + + + INCLUDES + + +
      INCLUDE 'chbin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      BINFRM= "FORMATTED"
C
      DO 10 I= 1, MAXHBN
        FDFLAG(I)= 0
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE BINHED (BINU,OPTYP,OPNO,SECT,VCOUNT,CLEN,CHEAD)
C
C     + + + PURPOSE + + +
C     Write a binary output header record for a section of an operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BINU,OPNO,VCOUNT,CLEN(VCOUNT)
      CHARACTER*8   OPTYP,SECT
      CHARACTER*(*) CHEAD(VCOUNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BINU   - fortran unit number of binary outout file
C     OPTYP  - operation type
C     OPNO   - operation ID number (LSID,RCHNO...)
C     SECT   - model section (e.g. PWATER, HYDR..)
C     VCOUNT - number of variables output
C     CLEN   - array of variable name lengths
C     CHEAD  - array of variable names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I4,I8,CPOS,RECLEN
      CHARACTER*32 RECFMT
C
C     + + + EQUIVALENCES + + +
      INTEGER     PI(2)
      CHARACTER*1 PC(8)
      EQUIVALENCE (PI,PC)
C
C     + + + EXTERNALS + + +
      EXTERNAL CVARAR,COPYC
C
C     + + + INCLUDES + + +
      INCLUDE 'chbin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
      I8= 8
C
C     first check if first record in file
      IF (FDFLAG(BINU) .EQ. 0) THEN
C       this is first header record - initialize file with hex FD (253)
        IBUF(1)= 253
        WRITE (BINU,"(A1,$)") BINBUF(1)
        FDFLAG(BINU)= 1
      END IF
C
C     create a record in the buffer
C
C     record leader material
C
C     leave room for record length to be filled in later
C     record type for header is zero
      IBUF(2)= 0
C     operation type
      CALL CVARAR(I8,OPTYP,I8,
     O            PC)
      IBUF(3)= PI(1)
      IBUF(4)= PI(2)
C     operation id number
      IBUF(5)= OPNO
C     module section name
      CALL CVARAR(I8,SECT,I8,
     O            PC)
      IBUF(6)= PI(1)
      IBUF(7)= PI(2)
C
C     variable namelengths and names
C
C     initial char position is 4 * last integer position written
      CPOS= 28
      DO 10 I= 1, VCOUNT
        PI(1)= CLEN(I)
        CALL COPYC (I4,PC(1),
     O              BINBUF(CPOS+1))
        CPOS= CPOS+4
        CALL CVARAR(CLEN(I),CHEAD(I),CLEN(I),
     O              BINBUF(CPOS+1))
        CPOS= CPOS+ CLEN(I)
 10   CONTINUE
C
C     add record length to beginning and dummy to end
      RECLEN= CPOS- 4
      CALL PAKLEN (RECLEN,
     M             CPOS)
C     write record to exact length to maintain suppression of newline
      WRITE (RECFMT,*) "(",CPOS,"A1,$)"
      WRITE (BINU,RECFMT) (BINBUF(I),I=1,CPOS)
C
C      WRITE (BINU) 0,OPTYP,OPNO,SECT,
C     $            (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,VCOUNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE BINREC (BINU,OPTYP,OPNO,SECT,UNITFG,LEV,DATE,VCOUNT,
     I                   VALUES)
C
C     + + + PURPOSE + + +
C     Write a binary output header record for a section of an operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BINU,OPNO,UNITFG,LEV,DATE(5),VCOUNT
      REAL          VALUES(VCOUNT)
      CHARACTER*(*) OPTYP,SECT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BINU   - fortran unit number of binary outout file
C     OPTYP  - operation type
C     OPNO   - operation ID number (LSID,RCHNO...)
C     SECT   - model section (e.g. PWATER, HYDR..)
C     UNITFG - unit system: 1-English, 2-Metric
C     LEV    - output level: 2-PIVL, 3-Daily, 4-Monthly, 5-Annual
C     DATE   - date and time of end of printout interval
C     VCOUNT - number of variables output
C     VALUES - array of variable values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I4,I8,CPOS,RPOS,RECLEN
      CHARACTER*32 RECFMT
C
C     + + + EQUIVALENCES + + +
      INTEGER     PI(2)
      CHARACTER*1 PC(8)
      EQUIVALENCE (PI,PC)
C
C     + + + EXTERNALS + + +
      EXTERNAL CVARAR,COPYC
C
C     + + + INCLUDES + + +
      INCLUDE 'chbin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
      I8= 8
C
C     create a record in the buffer
C
C     record leader material
C
C     leave room for record length to be filled in later
C     record type for data is 1
      IBUF(2)= 1
C
C     operation type
      CALL CVARAR(I8,OPTYP,I8,
     O            PC)
      IBUF(3)= PI(1)
      IBUF(4)= PI(2)
      IBUF(5)= OPNO
      CALL CVARAR(I8,SECT,I8,
     O            PC)
      IBUF(6)= PI(1)
      IBUF(7)= PI(2)
      IBUF(8)= UNITFG
      IBUF(9)= LEV
      DO 10 I= 1, 5
        IBUF(9+I)= DATE(I)
 10   CONTINUE
C
C     variable values
      RPOS= 14
C     write (99,*) "OPER ",OPTYP,OPNO,DATE
C     write (99,*) "  VCOUNT",VCOUNT
      DO 20 I= 1, VCOUNT
        RBUF(RPOS+I)= VALUES(I)
C     write (99,*) "    ",I,VALUES(I)
 20   CONTINUE
C     write (99,*) "  ",(RBUF(I),I=RPOS+1,RPOS+VCOUNT)
C     write (99,*) "  ",(BINBUF(I),I=1,(RPOS+VCOUNT)*4)
C
C     add record length to beginning and dummy to end
      RECLEN= (RPOS + VCOUNT - 1) * 4
      CPOS= RECLEN+ 4
      CALL PAKLEN (RECLEN,
     M             CPOS)
C     write record to exact length to maintain suppression of newline
      WRITE (RECFMT,*) "(",CPOS,"A1,$)"
      WRITE (BINU,RECFMT) (BINBUF(I),I=1,CPOS)
C
C      WRITE (BINU) 0,OPTYP,OPNO,SECT,
C     $            (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,VCOUNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE PAKLEN (LEN,
     M                   CPOS)
C
C     + + + PURPOSE + + +
C     record length is packed into a 4-byte field, with last two bits added
C     to specify the number of bytes beyond the first that must be read.  I have no idea
C     why extra bytes up to 4 always seem to be specified, even though less than 4
C     would work in many cases.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,CPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of record, including RECLEN but not BACKPTR
C     CPOS   - total length of buffer, including BACKPTR
C
C     + + + LOCAL VARIABLES + + +
      INTEGER FIELD,I
C
C     + + + EQUIVALENCES + + +
      INTEGER      PI(4)
      CHARACTER*1  PC(16)
      EQUIVALENCE (PI,PC)
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD,INT
C
C     + + + INCLUDES + + +
      INCLUDE 'chbin.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     create record length at beginning of buffer
C
C     first multiply by 4 to shift left by 2 bits
C     then add 3 to specify 3 more bytes
      FIELD= LEN * 4 + 3

C     then pack one byte at a time.
      PI(1)= MOD(FIELD,256)
      PI(2)= INT (FIELD / 256.0)
      PI(3)= INT (FIELD / 256.0**2.0)
      PI(4)= INT (FIELD / 256.0**3.0)
      BINBUF(1)= PC(1)
      BINBUF(2)= PC(5)
      BINBUF(3)= PC(9)
      BINBUF(4)= PC(13)
C
C     now create back-pointer at end of record
      FIELD= CPOS * 4 + 1
      IF (FIELD .GE. 256) THEN
C       two-byte back-pointer
        PI(1)= INT (FIELD / 256.0)
        PI(2)= MOD(FIELD,256)
        BINBUF(CPOS+1)= PC(1)
        BINBUF(CPOS+2)= PC(5)
        CPOS= CPOS+ 2
      ELSE
C       one-byte back-pointer
        PI(1)= FIELD- 1
        BINBUF(CPOS+1)= PC(1)
        CPOS= CPOS+ 1
      END IF
C
      RETURN
      END
C
C
C
      MODULE HSPF_BINARY_INPUT
C
C     + + + PURPOSE + + +
C     read from HSPF binary format file
C
C     + + + HISTORY + + +
C     2009/06/24 JLK - initial implementation 
C
C     + + + MODULE VARIABLES + + +
      CHARACTER*128  FILENAME
      INTEGER        FILEUNIT, Pos
      INTEGER        MXBBUF
      PARAMETER     (MXBBUF=10000)
C
      CHARACTER*1    BINBUF(MXBBUF*4)
      INTEGER        IBUF(MXBBUF)
      INTEGER*1      IBUF1(MXBBUF*4)
      INTEGER*2      IBUF2(MXBBUF*2)
      REAL           RBUF(MXBBUF)
      EQUIVALENCE (BINBUF,IBUF,RBUF,IBUF1,IBUF2)
C
      CONTAINS
C
C
C
      SUBROUTINE INIT
     I               (FUNIT,FNAME,
     O                RETCOD)
C     
C     + + + PURPOSE + + +
C     open and read headers from HSPF binary file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       FUNIT,RETCOD
      CHARACTER*(*) FNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FUNIT - fortran unit number  
C     FNAME - binary file name
C     RETCOD- return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER IOS, RecLen, I
      Logical First
C
C     + + + END SPECIFICATIONS + + +
C
      OPEN (UNIT=FUNIT,FILE=FNAME,STATUS='OLD',ACCESS='STREAM',
     $      FORM='FORMATTED',ACTION='READ',ERR=10,IOSTAT=IOS)
      GOTO 20
 10   CONTINUE
C       TODO: error handling code        
 20   CONTINUE      
      FILENAME= FNAME
      FILEUNIT= FUNIT
      Pos     = 1
C      
      READ(FILEUNIT,"(A1)") BINBUF(1)
      Pos     = Pos+ 1
C     INQUIRE(FILEUNIT,POS=I)
C     WRITE(*,*) IBUF1(1),IBUF2(1),IBUF(1),I,Pos
      IF (IBUF(1) .NE. 253) THEN
C       bad header
       
        RETCOD = 1
      ELSE
        First = .True.
        RecLen= 1
        DO WHILE (RecLen .GT. 0) 
          RecLen= FtnUnfSeqRecLen(FileUnit,First)
          WRITE(*,*) "Record,Len ", RecLen
        END DO
      END IF      
C      
      END SUBROUTINE
C
C
C
      Integer Function FtnUnfSeqRecLen(aFileUnit,aFirst) 
C
C     + + + DUMMY ARGUMENTS + + +
      Integer aFileUnit
      Logical aFirst
C
C     + + + LOCAL VARIABLES + + +          
      Integer      mLengthLast, c, h, lBytes, lRecordLength, I 
      Save         mLengthLast
C      
      If (aFirst) Then
          mLengthLast= 0
          aFirst     = .False.
          Pos        = 2
      Else
          c = 64
          Read(FILEUNIT,"(A1)",END=20,POS=Pos) BINBUF(1)
          Pos = Pos+ 1
          Do While (mLengthLast .ge. c)
              c = c * 256
              Read(FILEUNIT,"(A1)",END=20,POS=Pos) BINBUF(1)
              Pos = Pos+ 1
C             INQUIRE(FILEUNIT,POS=I)
C             WRITE(*,*) IBUF1(1),IBUF2(1),IBUF(1),I,Pos
          End Do
      End If
      Read(FILEUNIT,"(A1)",END=20,POS=Pos) BINBUF(1)
      Pos = Pos+ 1
C     INQUIRE(FILEUNIT,POS=I)
C     WRITE(*,*) IBUF1(1),IBUF2(1),IBUF(1),I,Pos
      lBytes = IBUF(1) .And. 3
        lRecordLength = IBUF(1) / 4
        c = 64
        h = lBytes + 1
        Do While (lBytes .gt. 0)
            Read(FILEUNIT,"(A1)",END=20,POS=Pos) BINBUF(1)
            Pos   = Pos+ 1
            lBytes= lBytes - 1
            lRecordLength = lRecordLength + IBUF(1) * c
            c = c * 256
        End Do
        mLengthLast    = lRecordLength + h
        Pos            = Pos+ lRecordLength
        FtnUnfSeqRecLen= lRecordLength
        
        Return 
 20     CONTINUE
          WRITE(*,*) 'EndOfFileAt ', Pos
          FtnUnfSeqRecLen= 0        
      End Function      
C      
      END MODULE HSPF_BINARY_INPUT      

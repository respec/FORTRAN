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
!
!
!
      MODULE HSPF_BINARY_INPUT
!
!     + + + PURPOSE + + +
!     read from HSPF binary format file
!
!     + + + HISTORY + + +
!     2009/06/24 JLK - initial implementation 
!
!     + + + MODULE VARIABLES + + +
      CHARACTER*128 mFileName
      INTEGER       mFileUnit 
      INTEGER       mMaxBinBuff,mMaxRecords
      PARAMETER    (mMaxBinBuff=10000,mMaxRecords=20000)
!     
      Integer       mRecordCount,mHeaderCount    
!
      CHARACTER*1   mBinBuffC(mMaxBinBuff*4)
      Character*4   mBinBuffC4(mMaxBinBuff)
      Character*8   mBinBuffC8(mMaxBinBuff/2)
      INTEGER       mBinBuffI(mMaxBinBuff)
      INTEGER*1     mBinBuffI1(mMaxBinBuff*4)
      INTEGER*2     mBinBuffI2(mMaxBinBuff*2)
      REAL          mBinBuffR(mMaxBinBuff)
      EQUIVALENCE  (mBinBuffC,mBinBuffC4,mBinBuffC8,
     $              mBinBuffI,mBinBuffR,mBinBuffI1,mBinBuffI2)
!
      type Header
        sequence
        character*8 OperationName, SectionName
        integer     OperationNumber
        character*8, allocatable, dimension(:) :: Constituents
      end type Header
      type(Header), allocatable, dimension(:) :: mHeaders 
!
      type Record
        sequence
        integer  StartPos, EndPos
      end type Record
      type(Record) mRecords(mMaxRecords)        
!
      CONTAINS
!
!
!
      SUBROUTINE Init
     I               (aFileUnit,aFileName,
     O                aReturnCode)
!     
!     + + + PURPOSE + + +
!     open and read headers from HSPF binary file
!
!     + + + DUMMY ARGUMENTS + + +
      INTEGER       aFileUnit,aReturnCode
      CHARACTER*(*) aFileName
!
!     + + + ARGUMENT DEFINITIONS + + +
!     aFileUnit  - fortran unit number  
!     aFileName  - binary file name
!     aReturnCode- return code
!
!     + + + LOCAL VARIABLES + + +
      integer lIOS, lRecLength, lPos, I,
     $        lRecordIndex, lHeaderIndex, lVariableIndex,
     $        lStartPos, lVariableNameLength
      logical lFirst
      character*8  :: lVariableName(256)
      type(Header) :: lHeader
!
!     + + + END SPECIFICATIONS + + +
!
      OPEN (UNIT=aFileUnit,FILE=aFileName,STATUS='OLD',ACCESS='STREAM',
     $      FORM='FORMATTED',ACTION='READ',ERR=10,IOSTAT=lIOS)
      GOTO 20
 10   CONTINUE
!       TODO: error handling code   
        aReturnCode = 1
        Return     
 20   CONTINUE      
      mFileName   = aFileName
      mFileUnit   = aFileUnit
      mRecordCount= 0
      mHeaderCount= 0
!      
      READ(mFileUnit,"(A1)") mBinBuffC(1)
      IF (mBinBuffI(1) .NE. 253) THEN
!       bad header
!       TODO: error handling code               
        aReturnCode= 2
      ELSE
        lFirst      = .True.
        lRecLength  = 1
        lPos        = 2
        DO WHILE (lRecLength .gt. 0) 
          Call NextRecord(lFirst, lPos, mHeaderCount, 
     O                    lRecLength, lStartPos)
          If (lRecLength .gt. 0) Then
            mRecordCount= mRecordCount + 1
            mRecords(mRecordCount)%StartPos= lStartPos
            mRecords(mRecordCount)%EndPos  = lStartPos + lRecLength- 1
            WRITE(*,*) "Record ", mRecordCount, lStartPos, lRecLength, 
     $                 lPos, mHeaderCount
          else
            write(*,*) "EOF ", mRecordCount
          End If
        END DO
        if (mHeaderCount .gt. 0) then
          allocate (mHeaders(mHeaderCount))
          lHeaderIndex = 0
          do lRecordIndex = 1, mRecordCount
             lPos = mRecords(lRecordIndex)%StartPos
             mBinBuffI(1)= 0 
             Read(mFileUnit,"(A1)",POS=lPos) mBinBuffC(1)
             if (mBinBuffI(1) .eq. 0) then
               Read(mFileUnit,"(A8)",POS=lPos+4)  lHeader%OperationName
               Read(mFileUnit,"(A4)",POS=lPos+12) mBinBuffC4(1)
               lHeader%OperationNumber = mBinBuffI(1)
               Read(mFileUnit,"(A8)",POS=lPos+16) lHeader%SectionName
               lVariableIndex= 0
               lPos = lPos+ 24
               do while (lPos .lt. mRecords(lRecordIndex)%EndPos)
                 Read(mFileUnit,"(A4)",POS=lPos) mBinBuffC4(1)
                 lVariableNameLength = mBinBuffI(1)
                 lPos= lPos+ 4
                 mBinBuffC8(1)= ""
                 Read(mFileUnit,"(64A1)",Pos=lPos) 
     $               (mBinBuffC(I),I=1,lVariableNameLength)
                 lVariableIndex = lVariableIndex + 1
                 lVariableName(lVariableIndex)= mBinBuffC8(1)
                 lPos= lPos+ lVariableNameLength
               end do
               lHeaderIndex= lHeaderIndex+ 1
               mHeaders(lHeaderIndex)= lHeader
               write(*,*) "Header ", lRecordIndex, lHeaderIndex, 
     $                    lVariableIndex, lHeader%OperationName, 
     $                    lHeader%OperationNumber, lHeader%SectionName
               if (lHeaderIndex .eq. mHeaderCount) then
!                have all headers, exit do loop               
                 exit  
               end if
             end if
          end do
        end if
        aReturnCode= 0
      END IF      
!      
      END SUBROUTINE
!
!
!
      Subroutine NextRecord
     M                     (aFirst, aPos, aHeaderCount,
     O                      aRecordLength, aStartPos) 
!
!     + + + DUMMY ARGUMENTS + + +
      Logical aFirst
      Integer aPos, aHeaderCount, aRecordLength, aStartPos
!
!     + + + LOCAL VARIABLES + + +          
      Integer lLengthLast, c, h, lBytes 
      Save    lLengthLast
!      
      If (aFirst) Then
        lLengthLast= 0
        aFirst     = .False.
      Else
        c   = 64
        Read(mFileUnit,"(A1)",END=20,POS=aPos) mBinBuffC(1)
        aPos= aPos+ 1
        Do While (lLengthLast .ge. c)
          c   = c * 256
          Read(mFileUnit,"(A1)",END=20,POS=aPos) mBinBuffC(1)
          aPos= aPos+ 1
        End Do
      End If
      Read(mFileUnit,"(A1)",END=20,POS=aPos) mBinBuffC(1)
      aPos  = aPos+ 1
      lBytes= mBinBuffI(1) .And. 3
      aRecordLength= mBinBuffI(1) / 4
      c     = 64
      h     = lBytes + 1
      do while (lBytes .gt. 0)
        read(mFileUnit,"(A1)",END=20,POS=aPos) mBinBuffC(1)
        aPos  = aPos+ 1
        lBytes= lBytes - 1
        aRecordLength = aRecordLength + mBinBuffI(1) * c
        c     = c * 256
      end do
      read(mFileUnit,"(A1)",END=20,POS=aPos) mBinBuffC(1)
      aStartPos = aPos
      if (mBinBuffI(1) .eq. 0) then
        aHeaderCount = aHeaderCount+ 1
      end if
      lLengthLast= aRecordLength + h
      aPos       = aPos+ aRecordLength
      Return 
 20   Continue
        aRecordLength= 0  
        aStartPos    = 0
        Return      
      End Subroutine    
!      
      END MODULE HSPF_BINARY_INPUT      

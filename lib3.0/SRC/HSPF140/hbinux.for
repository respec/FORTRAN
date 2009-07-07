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
      INTEGER       mMaxBinBuff,mMaxRecords
      PARAMETER    (mMaxBinBuff=2,mMaxRecords=100000)
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
        character*8  OperationName 
        integer      OperationNumber
        character*8  SectionName
        character*24 Id
        integer      DataRecordStart(5), DataRecordEnd(5)
        integer      DataRecordCount(5)
        integer      RecordIndex, ConstituentCount
        integer      UnitFlag, DateStart(6), DateEnd(6)
        integer, allocatable, dimension(:,:)   :: DataRecordStart500
        character*8, allocatable, dimension(:) :: Constituents
      end type Header
!
      type Record
        sequence
        integer  StartPos, EndPos, RecordType, NextRecord, PrevRecord
      end type Record
!
      type BData
        sequence
        type(Header), pointer             :: Header
        integer                           :: Level
        integer                           :: DateStart(6)
        real, allocatable, dimension(:,:) :: Values
      end type BData
!
      type BinaryFile
        sequence
        character*128 FileName
        integer       FileUnit, RecordCount, HeaderCount    
        type(Header), allocatable, dimension(:) :: Headers 
        type(Record) :: Records(mMaxRecords)    
        type(BData)  :: BData    
      end type BinaryFile
!     TODO: support multiple BinaryFiles      
      type(BinaryFile), Target :: mBinaryFiles(1)
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
      integer lIOS, lPos, I, J, K, lDataLevel,  
     $        lRecordIndex, lHeaderIndex, lVariableIndex,
     $        lStartPos, lVariableNameLength, lDPos, lDateIndex,
     $        lBinaryFileIndex, lNextRec
      logical lHeaderFound
      character*8               :: lVariableName(256)
      type(BinaryFile), Pointer :: lBinaryFile
      type(Header), Pointer     :: lHeader
      type(Header)              :: lHeaderTmp
!
!     + + + END SPECIFICATIONS + + +
!
      OPEN (UNIT=aFileUnit,FILE=aFileName,STATUS='OLD',ACCESS='STREAM',
     $      FORM='UNFORMATTED',ACTION='READ',ERR=10,IOSTAT=lIOS)
      GOTO 20
 10   CONTINUE
!       TODO: error handling code   
        aReturnCode = 1
        Return     
 20   CONTINUE      
!                    
!     TODO: support multiple binary files
      lBinaryFileIndex = 1
!
      mBinaryFiles(lBinaryFileIndex)%FileName   = aFileName
      mBinaryFiles(lBinaryFileIndex)%FileUnit   = aFileUnit
      mBinaryFiles(lBinaryFileIndex)%RecordCount= 0
      mBinaryFiles(lBinaryFileIndex)%HeaderCount= 0
!      
      READ(aFileUnit,POS=1) mBinBuffC(1)
      IF (mBinBuffI(1) .NE. 253) THEN
!       bad header
!       TODO: error handling code               
        aReturnCode= 2
      ELSE
        lPos       = 2
        do while (NextRecord(lBinaryFileIndex,lPos))
!         this reads through all records in the file        
        end do
        lBinaryFile => mBinaryFiles(lBinaryFileIndex)
        write(*,*) "EOF ", lBinaryFile%RecordCount, 
     $                     lBinaryFile%HeaderCount
        if (lBinaryFile%HeaderCount .gt. 0) then
          allocate (lBinaryFile%Headers(lBinaryFile%HeaderCount))
          lHeaderIndex = 0
          do lRecordIndex = 1, lBinaryFile%RecordCount
             lPos = lBinaryFile%Records(lRecordIndex)%StartPos
             read(aFileUnit,POS=lPos+4)  lHeaderTmp%OperationName
             read(aFileUnit,POS=lPos+12) mBinBuffC4(1)
             lHeaderTmp%OperationNumber = mBinBuffI(1)
             read(aFileUnit,POS=lPos+16) lHeaderTmp%SectionName
             write(lHeaderTmp%Id,'(A8,I4,A8)') lHeaderTmp%OperationName,
     $         lHeaderTmp%OperationNumber, lHeaderTmp%SectionName     
             mBinBuffI(1)= 0 
             read(aFileUnit,POS=lPos) mBinBuffC(1)
             if (mBinBuffI(1) .eq. 0) then
               lVariableIndex= 0
               lPos = lPos+ 24
               do while 
     $           (lPos .lt. lBinaryFile%Records(lRecordIndex)%EndPos)
                 Read(aFileUnit,POS=lPos) mBinBuffC4(1)
                 lVariableNameLength = mBinBuffI(1)
                 lPos= lPos+ 4
                 mBinBuffC8(1)= ""
                 Read(aFileUnit,Pos=lPos) 
     $               (mBinBuffC(I),I=1,lVariableNameLength)
                 lVariableIndex = lVariableIndex + 1
                 lVariableName(lVariableIndex)= mBinBuffC8(1)
                 lPos= lPos+ lVariableNameLength
               end do
               lHeaderIndex= lHeaderIndex+ 1
               lHeaderTmp%ConstituentCount= lVariableIndex 
               lHeaderTmp%DataRecordStart = 0
               lHeaderTmp%DataRecordEnd   = 0
               lHeaderTmp%DataRecordCount = 0
               lHeaderTmp%RecordIndex     = lRecordIndex
               if (lHeaderIndex .gt. 1) then
                 deallocate (lHeaderTmp%Constituents)
               end if
               allocate (lHeaderTmp%Constituents(lVariableIndex))
               do I = 1, lVariableIndex
                 lHeaderTmp%Constituents(I)= lVariableName(I)
               end do
!              this should copy the header, not point to it               
               lBinaryFile%Headers(lHeaderIndex)= lHeaderTmp
!               write(*,*) "Header ", lHeader%RecordIndex, lHeaderIndex, 
!     $                    lVariableIndex, lHeader%OperationName, 
!     $                    lHeader%OperationNumber, lHeader%SectionName
!               do I = 1, lVariableIndex
!                 Write(*,*) "  Variable ", I, 
!     $                      mHeaders(lHeaderIndex)%Constituents(I) 
!               end do
             elseif (mBinBuffI(1) .eq. 1) then
!              note: this is a data record, header is used for the id             
               lHeaderFound = .false.
               do I = 1, lHeaderIndex
                 if (lHeaderTmp%Id .eq. lBinaryFile%Headers(I)%Id) then
                   lHeaderFound= .true.
                   lHeader     => lBinaryFile%Headers(I)
                   read(aFileUnit,POS=lPos+28) lDataLevel 
                   if (lHeader%DataRecordCount(lDataLevel) .eq. 0) then
                     lHeader%DataRecordStart(lDataLevel)= lRecordIndex
                     lHeader%DataRecordEnd(lDataLevel)  = lRecordIndex
                     lHeader%DataRecordCount(lDataLevel)= 1
                     lBinaryFile%Records(lRecordIndex)%PrevRecord= 
     $                 lHeader%RecordIndex
                     lBinaryFile%Records(lRecordIndex)%NextRecord= 0
                     read(aFileUnit,POS=lPos+24) mBinBuffC4(1)
                     lHeader%UnitFlag = mBinBuffI(1)
                     lDateIndex= 0
                     do lDateIndex = 1,5
                       lDPos = lPos+ 28 + (lDateIndex * 4)
                       read(aFileUnit,POS=lDPos) mBinBuffC4(1)
                       lHeader%DateStart(lDateIndex) = mBinBuffI(1)
                     end do
!                     write(*,*) "  DateEndI1 ", lHeader%DateStart 
!                    adjust start date to beginning of interval
                     GOTO (110,120,130,140,150), lDataLevel
 110                 continue
                       lHeader%DateStart(5)= lHeader%DateStart(5) -1
                       goto 200                                          
 120                 continue
                       lHeader%DateStart(4)= lHeader%DateStart(4) -1
                       goto 200                                          
 130                 continue
                       lHeader%DateStart(3)= lHeader%DateStart(3) -1
                       goto 200                                          
 140                 continue
                       lHeader%DateStart(2)= lHeader%DateStart(2) -1
                       goto 200                                          
 150                 continue
                       lHeader%DateStart(1)= lHeader%DateStart(1) -1
                       goto 200                                          
 200                 continue
                     if (lHeader%DateStart(5) .lt. 0) then
                       lHeader%DateStart(5)= 60 + lHeader%DateStart(5)
                       lHeader%DateStart(4)= lHeader%DateStart(4) -1
                     end if
                     if (lHeader%DateStart(4) .lt. 0) then
                       lHeader%DateStart(4)= 24 + lHeader%DateStart(4)
                       lHeader%DateStart(3)= lHeader%DateStart(3) -1
                     end if
                     if (lHeader%DateStart(3) .le. 0) then
                       write(*,*) "NEED DAYMON HERE"
                       lHeader%DateStart(3)= 31 + lHeader%DateStart(3)
                       lHeader%DateStart(2)= lHeader%DateStart(2) -1
                     end if
                     if (lHeader%DateStart(2) .le. 0) then
                       lHeader%DateStart(2)= 12 + lHeader%DateStart(2)
                       lHeader%DateStart(1)= lHeader%DateStart(1) -1
                     end if                    
!                     write(*,*) "  DateStart ", lHeader%DateStart 
                   else
                     read(aFileUnit,POS=lPos+24) mBinBuffC4(1)
                     if (lHeader%UnitFlag .ne. mBinBuffI(1)) then
                       write(*,*) "BigProblemWithUnitFlagAt ", 
     $                            lRecordIndex,
     $                            lHeader%UnitFlag,mBinBuffI(1)            
                     end if
                     lDateIndex= 0
                     do lDateIndex = 1,5
                       lDPos = lPos+ 28 + (lDateIndex * 4)
                       read(aFileUnit,POS=lDPos) 
     $                       lHeader%DateEnd(lDateIndex)
                     end do
!                     write(*,*) "  DateEnd ", lHeader%DateEnd
                     lBinaryFile%Records(lRecordIndex)%PrevRecord= 
     $                 lHeader%DataRecordEnd(lDataLevel)
                     J = lHeader%DataRecordEnd(lDataLevel)
                     lBinaryFile%Records(J)%NextRecord = lRecordIndex
                     lBinaryFile%Records(lRecordIndex)%NextRecord= 0
                     lHeader%DataRecordEnd(lDataLevel)  = lRecordIndex
                     lHeader%DataRecordCount(lDataLevel)= 
     $                 lHeader%DataRecordCount(lDataLevel)+ 1
                   end if
!                   write(*,*) "Header", I, "Record", lRecordIndex
                   do J = 1, 5
!                      if (mHeaders(I)%DataRecordCount(J).gt.0) then
!                        write(*,*) "  Level",J,
!     $                     mHeaders(I)%DataRecordCount(J),
!     $                     mHeaders(I)%DataRecordStart(J),
!     $                     mHeaders(I)%DataRecordEnd(J)
!                      end if
                   end do
                   exit   
                 end if
               end do
               if (.not. lHeaderFound) then
                 write(*,*) "MissingHeaderForDataAt ", lRecordIndex
               end if
             else
               write(*,*) "BadRecordTypeAt ",lRecordIndex
             end if
          end do
        end if
!
        do I= 1, lBinaryFile%HeaderCount        
          lHeader=> lBinaryFile%Headers(I) 
          write(99,*) "Header", I, 
     $      lHeader%OperationName, 
     $      lHeader%OperationNumber,
     $      lHeader%SectionName,
     $      lHeader%ConstituentCount,
     $      lHeader%UnitFlag
          do J= 1, lHeader%ConstituentCount
            write(99,*) "  Cons",lHeader%Constituents(J)
          end do    
          do J= 1, 5
            if (lHeader%DataRecordCount(J).gt.0) then
              write(99,*) "  Level",J,
     $           lHeader%DataRecordCount(J),
     $           lHeader%DataRecordStart(J),
     $           lHeader%DataRecordEnd(J)
              K = lHeader%DataRecordCount(J) / 500
              if (.not. allocated(lHeader%DataRecordStart500)) then
                allocate(lHeader%DataRecordStart500(K,5))            
              end if
              if (I .gt. 0) then
                k = 0
                lNextRec = lHeader%DataRecordStart(J)
                do while (K .lt. lHeader%DataRecordCount(J))
                   k = k + 1
                   if (mod(k,500) .eq. 0) then
                     lHeader%DataRecordStart500(K/500,J) = lNextRec
                   end if
                   lNextRec = lBinaryFile%Records(lNextRec)%NextRecord
                end do
              end if
            end if
          end do
        end do
        aReturnCode= 0
      end if     
!      
      End Subroutine Init
!
!
!
      Logical Function NextRecord
     I                           (aBinaryFileIndex,
     M                            aPos)
!
!     + + + DUMMY ARGUMENTS + + +
      Integer aBinaryFileIndex,aPos 
!
!     + + + LOCAL VARIABLES + + +          
      Integer  c, h, i, lBytes, lRecordLength
      Integer, Save             :: lLengthLast
      Type(BinaryFile), Pointer :: lBinaryFile
!      
      lBinaryFile => mBinaryFiles(aBinaryFileIndex)
!     write(*,*) "NextRecord",aPos
!      
      If (lBinaryFile%RecordCount .eq. 0) Then
        lLengthLast= 0
      Else
        c   = 64
        Read(lBinaryFile%FileUnit,END=20,POS=aPos) mBinBuffC(1)
        aPos= aPos+ 1
        Do While (lLengthLast .ge. c)
          c   = c * 256
          Read(lBinaryFile%FileUnit,END=20,POS=aPos) mBinBuffC(1)
          aPos= aPos+ 1
        End Do
      End If
      Read(lBinaryFile%FileUnit,END=20,POS=aPos) mBinBuffC(1)
      aPos  = aPos+ 1
      lBytes= mod (mBinBuffI(1),4)
      lRecordLength= mBinBuffI(1) / 4
      c     = 64
      h     = lBytes + 1
      do while (lBytes .gt. 0)
        read(lBinaryFile%FileUnit,END=20,POS=aPos) mBinBuffC(1)
        aPos  = aPos+ 1
        lBytes= lBytes - 1
        lRecordLength= lRecordLength + mBinBuffI(1) * c
        c     = c * 256
      end do
      read(lBinaryFile%FileUnit,END=20,POS=aPos) mBinBuffC(1)
      if (mBinBuffI(1) .eq. 0) then
        lBinaryFile%HeaderCount = lBinaryFile%HeaderCount+ 1
      end if
      lBinaryFile%RecordCount= lBinaryFile%RecordCount + 1
      lBinaryFile%Records(lBinaryFile%RecordCount)%StartPos  = aPos
      lBinaryFile%Records(lBinaryFile%RecordCount)%EndPos    = 
     $  aPos + lRecordLength- 1
      lBinaryFile%Records(lBinaryFile%RecordCount)%RecordType= 
     $  mBinBuffI(1)
!      WRITE(*,*) "Record ", mRecordCount, aPos, lRecordLength, 
!     $            mBinBuffI(1),mHeaderCount
      lLengthLast= lRecordLength + h
      aPos       = aPos+ lRecordLength
      NextRecord = .true.
      Return 
 20   Continue
        NextRecord = .false.
        Return      
      End Function NextRecord
!
!
!
      Subroutine CheckMember 
     I                      (aBinaryFileIndex, aHeaderIndex,
     I                       aConstituent,
     O                       aConstituentIndex, aTimeCodeMin,
     O                       aDataStart, aDataEnd, aUnits, aKind)   
!
!     + + + DUMMY ARGUMENTS + + +
      Integer     aBinaryFileIndex, aHeaderIndex, aConstituentIndex,
     $            aTimeCodeMin, aDataStart(6), aDataEnd(6), 
     $            aUnits, aKind
      Character*6 aConstituent
!
!     + + + ARGUMENT DEFINITIONS + + +
!     aConstituentIndex - constituent index, negative if problem
!                     0 - no match
!                    -1 - bad binary file index 
!                    -2 - bad header index
!     aTimeCodeMin - minimum time code, 0 if no data
!     aDataStart   - starting date of data
!     aDataEnd     - ending date of data
!
!     + + + LOCAL VARIABLES + + +          
      Integer                   :: I
      Type(BinaryFile), Pointer :: lBinaryFile
      Type(Header), Pointer     :: lHeader
      Character*6               :: lConstituent
!      
!     + + + END SPECIFICATIONS + + +
!
      aTimeCodeMin= 0
      if (aBinaryFileIndex .eq. 1) then
        lBinaryFile => mBinaryFiles(aBinaryFileIndex)
        if (aHeaderIndex .ge. 1 .and. 
     $      aHeaderIndex .le. lBinaryFile%HeaderCount) then
            lHeader => lBinaryFile%Headers(aHeaderIndex)
            do I = 1, 5
              if (lHeader%DataRecordCount(I) .gt. 0) then
                aTimeCodeMin = I
                aDataStart   = lHeader%DateStart
                aDataEnd     = lHeader%DateEnd
                aUnits       = lHeader%UnitFlag
!               TODO: define kind                
                aKind        = 3
                exit
              end if
            end do
            aConstituentIndex= 0
            do I = 1, lHeader%ConstituentCount
              lConstituent= lHeader%Constituents(I)(3:8)
              if (aConstituent .eq. lConstituent) then
                aConstituentIndex= I
                exit  
              end if
            end do
        else
          aConstituentIndex= -2
        end if
      else
        aConstituentIndex= -1
      end if
!   
      Return     
      End Subroutine CheckMember
!  
!
!    
      Subroutine GetBData
     I                   (aBinaryFileIndex,
     I                    aOperationName,aOperationNumber,aSectionName,
     I                    aLevel,aConstituent,aStartDate,aNumValues,
     O                    aValues,aReturnCode)
!
!     + + + DUMMY ARGUMENTS + + +
      Character*8 aOperationName,aSectionName,aConstituent
      Integer     aOperationNumber,aLevel,aBinaryFileIndex,
     $            aStartDate(6),aNumValues 
      Real        aValues(*)
      Integer     aReturnCode
!
!     + + + ARGUMENT DEFINITIONS + + +
!     aReturnCode - return status of request
!                     0 - successful
!                    -1 - no header match     
!                    -2 - level out of range
!                    -3 - no data for level 
!                    -4 - no matching constituent
!      
!     + + + LOCAL VARIABLES + + +          
      Integer                   :: lIndex, lIndexCons, lIndexRecord
      Integer                   :: lPos, lDataIndex, lDataBaseIndex, lI1
      Integer                   :: lNumValues, lFinalRecord
      Type(BinaryFile), Pointer :: lBinaryFile
      Type(Header), Pointer     :: lHeader
      Logical                   :: lHaveData, lHaveHeader, lHaveCons
!      
!     + + + END SPECIFICATIONS + + +
!
      lBinaryFile => mBinaryFiles(aBinaryFileIndex)
      lI1 = 1
!      
      if (allocated(lBinaryFile%BData%Values)) then    
!       already have some data, is it the right stuff?
        lHeader => lBinaryFile%BData%Header
        if ((aOperationName .eq. lHeader%OperationName) .and.
     $      (aOperationNumber .eq. lHeader%OperationNumber) .and.
     $      (aSectionName .eq. lHeader%SectionName)) then
          lHaveHeader= .true.
          if (aLevel .eq. lBinaryFile%BData%Level) then
            call TimDif(lBinaryFile%BData%DateStart,aStartDate,
     I                  aLevel+1,lI1,
     O                  lIndex)
            lNumValues = Ubound(lBinaryFile%BData%Values,1)
            if (lIndex .eq. 0 .and. aNumValues .le. lNumValues) then
              lHaveData= .true.
            else
              deallocate (lBinaryFile%BData%Values)        
              lHaveData= .false.
            end if
!           TODO: check which time span we have          
          else
            deallocate (lBinaryFile%BData%Values)        
            lHaveData= .false.
          end if
        else
          deallocate (lBinaryFile%BData%Values)        
          lHaveData  = .false.
          lHaveHeader= .false.
        end if
      else
        lHaveData  = .false.
        lHaveHeader= .false.
      end if
!      
      aReturnCode = 0
      if (.not. lHaveHeader) then
        do lIndex = 1, lBinaryFile%HeaderCount
          lHeader => lBinaryFile%Headers(lIndex)
          if ((aOperationName .eq. lHeader%OperationName) .and.
     $        (aOperationNumber .eq. lHeader%OperationNumber) .and.
     $        (aSectionName .eq. lHeader%SectionName)) then
            lBinaryFile%BData%Header => lHeader
            lHaveHeader  = .true.
            exit
          end if
        end do
      end if
!      
      if (lHaveHeader) then
        if (.not. lHaveData) then
          if (aLevel .ge. 1 .and. aLevel .le. 5) then
            if (lHeader%DataRecordCount(aLevel) .gt. 0) then
!             figure out where to start reading 
              call TimDif(lHeader%DateStart,aStartDate,aLevel+1,lI1,
     O                    lDataBaseIndex)
              lNumValues = 
     $          lHeader%DataRecordCount(aLevel) - lDataBaseIndex
              if (lNumValues .gt. aNumValues) then
                lNumValues = aNumValues
              end if     
              lFinalRecord = lDataBaseIndex + lNumValues
              allocate (lBinaryFile%BData%Values(
     $                  lNumValues,lHeader%ConstituentCount))
              if (lDataBaseIndex .lt. 500) then
                lIndex = 0
                lIndexRecord = lHeader%DataRecordStart(aLevel)
              else
                lIndex = lDataBaseIndex/500
                lIndexRecord = 
     $            lHeader%DataRecordStart500(lIndex,aLevel) 
              end if
              lDataIndex = lIndex * 500
              do while (lDataIndex .lt. lDataBaseIndex)
                lDataIndex = lDataIndex + 1
                lIndexRecord = 
     $            lBinaryFile%Records(lIndexRecord)%NextRecord
              end do
!              
              lBinaryFile%BData%DateStart= aStartDate
              do lIndex = 1, lNumValues
                do lIndexCons = 1, lHeader%ConstituentCount
                  lPos= lBinaryFile%Records(lIndexRecord)%StartPos+ 
     $                  48+ (lIndexCons * 4)
                  read(lBinaryFile%FileUnit,POS=lPos) mBinBuffC4(1)
                  lBinaryFile%BData%Values(lIndex,lIndexCons) = 
     $                  mBinBuffR(1)
                end do
                lIndexRecord = 
     $                  lBinaryFile%Records(lIndexRecord)%NextRecord
              end do
              lBinaryFile%BData%Level = aLevel
              lHaveData  = .true.
            else
              aReturnCode= -3          
            end if
          else
            aReturnCode= -2
          end if
        end if
      else
        aReturnCode= -1
      end if
!      
      if (lHaveData) then
        lHaveCons = .false.
        do lIndexCons= 1, lHeader%ConstituentCount
          if (aConstituent .eq. lHeader%Constituents(lIndexCons)) then
            lHaveCons = .true.
            exit
          end if
        end do
        if (lHaveCons) then
          do lIndex= 1, aNumValues
            aValues(lIndex)= 
     $        lBinaryFile%BData%Values(lIndex,lIndexCons)
          end do 
        else
          aReturnCode = -4
        end if
      end if
!
      End Subroutine GetBData           
!      
!
!
      Subroutine GetBDataTest
!      
      integer lReturnCode, I, I1, lHbnIndex, lValueSize
      real    lValues(5000), lValuesTotal, lValuesTotalNow
      integer lStartDate(6),lNumValues, lNumValuesTotal
      integer lGetCount, lGetIndex
!         
      write(*,*) "GetBDataTest"
!      
      I1        = 1
      lHbnIndex = 1
      lValueSize= Ubound(lValues,1)
      
      do I = 2, 5
        lValues= 0.0
        lValuesTotal= 0.0
        lNumValuesTotal= 
     $    mBinaryFiles(lHbnIndex)%Headers(1)%DataRecordCount(I)
        lGetCount = 1+ lNumValuesTotal/lValueSize
        do lGetIndex = 1, lGetCount
          if (lGetIndex .lt. lGetCount) then
            lNumValues= lValueSize
          else
            lNumValues= lNumValuesTotal -((lGetCount-1) * lValueSize)
          end if
          if (lGetIndex .eq. 1) then
            lStartDate= mBinaryFiles(lHbnIndex)%Headers(1)%DateStart
          else
            call TimAdd (lStartDate,I+1,I1,lValueSize,
     O                   lStartDate)
          end if
          call GetBData
     I                 (lHbnIndex,"EXTMOD  ",66,"Met     ",
     I                  I,"I:PREC1 ",lStartDate,lNumValues,
     O                  lValues,lReturnCode)
          lValuesTotalNow = Sum(lValues(1:lNumValues))
          lValuesTotal = lValuesTotal + lValuesTotalNow
          write(*,*) lReturnCode, I, 
     $      lGetIndex, lValuesTotalNow, lValuesTotal
        end do
      end do
!
      End Subroutine GetBDataTest
!      
      END MODULE HSPF_BINARY_INPUT      
